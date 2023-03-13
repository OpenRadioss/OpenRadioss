Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  MULTI_EVOLVE_GLOBAL           source/multifluid/multi_evolve_global.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_EVOLVE_GLOBAL(TIMESTEP, NG, ELBUF_TAB, 
     .     IPARG, ITASK, IXS, IXQ, IXTG, MULTI_FVM, GRAVITY)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD      
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) :: TIMESTEP
      INTEGER, INTENT(IN) :: NG
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      INTEGER, INTENT(IN), TARGET :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      my_real, INTENT(IN) :: GRAVITY(4, *)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      INTEGER :: II, I, J, ITY, NEL, NFT, ISOLNOD
      INTEGER :: IPLA, NODEID, NVERTEX
      my_real :: RHO, ETOT, VEL2, VOL, SUMFLUX(5)
      my_real :: VII(5), GRAVII(3), PRES, VX, VY, VZ, TFEXTT, WEXT
      INTEGER :: NB_FACE, NB_NODE, NODE_LIST(8)
      INTEGER, DIMENSION(:, :), POINTER :: IX
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      GBUF    =>ELBUF_TAB(NG)%GBUF
      NEL     = IPARG(2, NG)
      NFT     = IPARG(3, NG)
      ITY     = IPARG(5, NG)
      ISOLNOD = IPARG(28, NG)
      NB_FACE = 6
      TFEXTT  = ZERO
      IX      =>IXS(1:NIXS, 1 + NFT : NEL + NFT)
      IF (ISOLNOD == 8) THEN
         NB_NODE = 8
         DO J = 1, NB_NODE
            NODE_LIST = J + 1
         ENDDO
      ELSE
         NB_NODE = 4
         NODE_LIST(1) = 2
         NODE_LIST(2) = 4
         NODE_LIST(3) = 7
         NODE_LIST(4) = 6
      ENDIF

      IF (MULTI_FVM%SYM /= 0) THEN
         IF (ITY == 2) THEN
C     QUADS
            NB_FACE = 4
            NB_NODE = 4
            NODE_LIST(1) = 2
            NODE_LIST(2) = 3
            NODE_LIST(3) = 4
            NODE_LIST(4) = 5

            IX => IXQ(1:NIXQ, 1 + NFT : NEL + NFT)
         ELSEIF (ITY == 7) THEN
C     TRIANGLES
            NB_FACE = 3
            NB_NODE = 3
            NODE_LIST(1) = 2
            NODE_LIST(2) = 3
            NODE_LIST(3) = 4
            IX => IXTG(1:NIXTG, 1 + NFT : NEL + NFT)
         ENDIF
      ENDIF

C     Update global quantities
      DO II = 1, NEL
         I = II + NFT
C     Velocity components
         VX = GBUF%MOM(II + 0 * NEL)
         VY = GBUF%MOM(II + 1 * NEL)
         VZ = GBUF%MOM(II + 2 * NEL)
!     Square norm of the velocity
         VEL2 = VX * VX + VY * VY + VZ * VZ
!     Conserved variable
         VII(1) = GBUF%RHO(II)
         VII(2) = VII(1) * VX
         VII(3) = VII(1) * VY
         VII(4) = VII(1) * VZ
         VII(5) = GBUF%EINT(II) + HALF * VII(1) * VEL2
         
! Volume 
         VOL = GBUF%VOL(II)
! Sum of fluxes
         IF (MULTI_FVM%SYM == 0 .AND. ISOLNOD /= 4) THEN
            SUMFLUX(1:5) = MULTI_FVM%FLUXES(1:5, 1, I) + MULTI_FVM%FLUXES(1:5, 2, I) + 
     .           MULTI_FVM%FLUXES(1:5, 3, I) + MULTI_FVM%FLUXES(1:5, 4, I) +
     .           MULTI_FVM%FLUXES(1:5, 5, I) + MULTI_FVM%FLUXES(1:5, 6, I)
         ELSEIF (ISOLNOD == 4) THEN
            SUMFLUX(1:5) = MULTI_FVM%FLUXES(1:5, 5, I) + MULTI_FVM%FLUXES(1:5, 6, I) + 
     .           MULTI_FVM%FLUXES(1:5, 2, I) + MULTI_FVM%FLUXES(1:5, 4, I)
         ELSE
C     TRIANGLES
            SUMFLUX(1:5) = MULTI_FVM%FLUXES(1:5, 1, I) + MULTI_FVM%FLUXES(1:5, 2, I) + 
     .           MULTI_FVM%FLUXES(1:5, 3, I)
            IF (ITY == 2) THEN
C     QUADS
               SUMFLUX(1:5) = SUMFLUX(1:5) + MULTI_FVM%FLUXES(1:5, 4, I)
            ENDIF
         ENDIF
! Time evolution
         VII(1:5) = VOL * VII(1:5) - TIMESTEP * SUMFLUX(1:5)
! 2D axi
         IF (MULTI_FVM%SYM == 1) THEN
            PRES = MULTI_FVM%PRES(I)
            VII(3) = VII(3) + TIMESTEP * GBUF%AREA(II) * PRES
         ENDIF
! Gravity
         GRAVII(1:3) = ZERO
         NVERTEX = 0
!     TODO(DC) :check the case of tetrahedra
         DO J = 1, NB_NODE
            NODEID = IX(NODE_LIST(J), II)
            IF(GRAVITY(4, NODEID) == ZERO) CYCLE
            NVERTEX = NVERTEX + 1
            GRAVII(1) = GRAVII(1) + GRAVITY(1, NODEID)
            GRAVII(2) = GRAVII(2) + GRAVITY(2, NODEID)
            GRAVII(3) = GRAVII(3) + GRAVITY(3, NODEID)
         ENDDO
         IF (NVERTEX > 0) THEN
            GRAVII(1:3) = GRAVII(1:3) / NVERTEX
         ENDIF
         VII(2:4) = VII(2:4) + TIMESTEP * GBUF%RHO(II) * VOL * GRAVII(1:3)
         WEXT     = TIMESTEP * GBUF%RHO(II) * VOL * (
     .              GRAVII(1) * MULTI_FVM%VEL(1, I) + 
     .              GRAVII(2) * MULTI_FVM%VEL(2, I) + 
     .              GRAVII(3) * MULTI_FVM%VEL(3, I))
         VII(5) = VII(5) + WEXT
         TFEXTT = TFEXTT + WEXT
! Mass is stored in RHO
         MULTI_FVM%RHO(I) = VII(1)
! Mass times velocity is stored in VEL
         MULTI_FVM%VEL(1, I) = VII(2)
         MULTI_FVM%VEL(2, I) = VII(3)
         MULTI_FVM%VEL(3, I) = VII(4)
! Total energy is stored in EINT
         MULTI_FVM%EINT(I) = VII(5)
      ENDDO

C-------------------------------------------
#include "atomic.inc"
       TFEXT=TFEXT+TFEXTT
#include "atomend.inc"
C-------------------------------------------

C-----------------------------------------------
C     E n d  o f   s u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE MULTI_EVOLVE_GLOBAL
