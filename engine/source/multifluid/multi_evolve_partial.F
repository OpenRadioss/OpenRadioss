Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MULTI_EVOLVE_PARTIAL          source/multifluid/multi_evolve_partial.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_EVOLVE_PARTIAL(TIMESTEP, NG, ELBUF_TAB, 
     .     IPARG, ITASK, IXS, IXQ, IXTG, MULTI_FVM, 
     .     PM, IPM, GRAVITY, CURRENT_TIME)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "task_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) :: TIMESTEP
      INTEGER, INTENT(IN) :: NG
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      my_real, INTENT(IN) :: PM(NPROPM, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: GRAVITY(4, *)
      my_real, INTENT(IN) :: CURRENT_TIME
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      INTEGER :: II, I, J, JJ, KFACE, KFACE2, NB_FACE
      INTEGER :: IPLA
      my_real :: RHO, ETOT, VEL2, VOL
      my_real :: VOLNEW(MVSIZ)
      my_real :: VOLFRAC, MASSFRAC, MASSFRACII, MASSFRACJJ, EINTII, EINTJJ
      my_real :: VOLFRACII, VOLFRACJJ
      my_real :: MASSFLUX, SS, SUMFLUX(3), TMP2, TMP3, TMP4, NORMAL_VEL
      my_real :: SR, SL, SSTAR, SURF, VII(3), VJJ(3), FII(3), FJJ(3)
      my_real :: FIISTAR(3), FJJSTAR(3), VIISTAR(3), VJJSTAR(3)
      my_real :: NORMALW, NX, NY, NZ
      my_real :: NORMAL_VELII, NORMAL_VELJJ
      my_real :: ALPHAII, ALPHAJJ, RHOII, RHOJJ, RHOEII, RHOEJJ, PII, PJJ
      my_real :: ALPHASTAR, RHOSTAR
      my_real :: PSTAR, ESTAR, PINF, GAM, GRAVII(3)
      INTEGER :: LOCAL_MATID, MATLAW, NVERTEX, NODEID, IMAT
      my_real :: SUM_NORMALVEL, PSHIFT
      LOGICAL :: COMPUTED
      TYPE(LBUF_PTR) :: LBUFS(MULTI_FVM%NBMAT)
      INTEGER :: ISOLNOD, NEL, ITY, NFT
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      
      GBUF => ELBUF_TAB(NG)%GBUF
      NEL = IPARG(2, NG)
      NFT = IPARG(3, NG)
      ITY = IPARG(5, NG)
      ISOLNOD = IPARG(28, NG)

      PSHIFT = MULTI_FVM%PRES_SHIFT

      IF (MULTI_FVM%NBMAT > 1) THEN
!DIR$ NOVECTOR
         DO IMAT = 1, MULTI_FVM%NBMAT
            LBUFS(IMAT)%LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
         ENDDO
      ENDIF
      DO IMAT = 1, MULTI_FVM%NBMAT

         IF (MULTI_FVM%SYM == 0) THEN
            NB_FACE = 6
            LOCAL_MATID = IPM(20 + IMAT, IXS(1, 1 + NFT))
         ELSEIF (ITY == 2) THEN
C     QUADS
            NB_FACE = 4
            LOCAL_MATID = IPM(20 + IMAT, IXQ(1, 1 + NFT))
         ELSEIF (ITY == 7) THEN
C     TRIANGLES
            NB_FACE = 3
            LOCAL_MATID = IPM(20 + IMAT, IXTG(1, 1 + NFT))
         ENDIF
         MATLAW = IPM(2, LOCAL_MATID)

C     UPDATE
         DO II = 1, NEL
            I = II + NFT
C     Fluxes sum
            IF (MULTI_FVM%SYM == 0 .AND. ISOLNOD /= 4) THEN
C     Volume flux
               SUMFLUX(1) = MULTI_FVM%SUBVOL_FLUXES(IMAT, 1, I) + MULTI_FVM%SUBVOL_FLUXES(IMAT, 2, I) + 
     .              MULTI_FVM%SUBVOL_FLUXES(IMAT, 3, I) + MULTI_FVM%SUBVOL_FLUXES(IMAT, 4, I) +
     .              MULTI_FVM%SUBVOL_FLUXES(IMAT, 5, I) + MULTI_FVM%SUBVOL_FLUXES(IMAT, 6, I)
C     Mass flux
               SUMFLUX(2) = MULTI_FVM%SUBMASS_FLUXES(IMAT, 1, I) + MULTI_FVM%SUBMASS_FLUXES(IMAT, 2, I) + 
     .              MULTI_FVM%SUBMASS_FLUXES(IMAT, 3, I) + MULTI_FVM%SUBMASS_FLUXES(IMAT, 4, I) +
     .              MULTI_FVM%SUBMASS_FLUXES(IMAT, 5, I) + MULTI_FVM%SUBMASS_FLUXES(IMAT, 6, I)
C     Energy flux
               SUMFLUX(3) = MULTI_FVM%SUBENER_FLUXES(IMAT, 1, I) + MULTI_FVM%SUBENER_FLUXES(IMAT, 2, I) + 
     .              MULTI_FVM%SUBENER_FLUXES(IMAT, 3, I) + MULTI_FVM%SUBENER_FLUXES(IMAT, 4, I) +
     .              MULTI_FVM%SUBENER_FLUXES(IMAT, 5, I) + MULTI_FVM%SUBENER_FLUXES(IMAT, 6, I)
            ELSEIF (ISOLNOD == 4) THEN
C     Volume flux
               SUMFLUX(1) = MULTI_FVM%SUBVOL_FLUXES(IMAT, 5, I) + MULTI_FVM%SUBVOL_FLUXES(IMAT, 6, I) + 
     .              MULTI_FVM%SUBVOL_FLUXES(IMAT, 2, I) + MULTI_FVM%SUBVOL_FLUXES(IMAT, 4, I)
C     Mass flux
               SUMFLUX(2) = MULTI_FVM%SUBMASS_FLUXES(IMAT, 5, I) + MULTI_FVM%SUBMASS_FLUXES(IMAT, 6, I) + 
     .              MULTI_FVM%SUBMASS_FLUXES(IMAT, 2, I) + MULTI_FVM%SUBMASS_FLUXES(IMAT, 4, I)
C     Energy flux
               SUMFLUX(3) = MULTI_FVM%SUBENER_FLUXES(IMAT, 5, I) + MULTI_FVM%SUBENER_FLUXES(IMAT, 6, I) + 
     .              MULTI_FVM%SUBENER_FLUXES(IMAT, 2, I) + MULTI_FVM%SUBENER_FLUXES(IMAT, 4, I)
            ELSE
C     TRIANGLES
C     Volume flux
               SUMFLUX(1) = MULTI_FVM%SUBVOL_FLUXES(IMAT, 1, I) + MULTI_FVM%SUBVOL_FLUXES(IMAT, 2, I) + 
     .              MULTI_FVM%SUBVOL_FLUXES(IMAT, 3, I)
C     Mass flux
               SUMFLUX(2) = MULTI_FVM%SUBMASS_FLUXES(IMAT, 1, I) + MULTI_FVM%SUBMASS_FLUXES(IMAT, 2, I) + 
     .              MULTI_FVM%SUBMASS_FLUXES(IMAT, 3, I)
C     Energy flux
               SUMFLUX(3) = MULTI_FVM%SUBENER_FLUXES(IMAT, 1, I) + MULTI_FVM%SUBENER_FLUXES(IMAT, 2, I) + 
     .              MULTI_FVM%SUBENER_FLUXES(IMAT, 3, I)
               IF (ITY == 2) THEN
C     QUADS
C     Volume flux
                  SUMFLUX(1) = SUMFLUX(1) + MULTI_FVM%SUBVOL_FLUXES(IMAT, 4, I)
C     Mass flux
                  SUMFLUX(2) = SUMFLUX(2) + MULTI_FVM%SUBMASS_FLUXES(IMAT, 4, I)
C     Energy flux
                  SUMFLUX(3) = SUMFLUX(3) + MULTI_FVM%SUBENER_FLUXES(IMAT, 4, I)
               ENDIF
            ENDIF         
            
            SUM_NORMALVEL = ZERO
            IF (MULTI_FVM%SYM == 0 .AND. ISOLNOD == 4) THEN
               KFACE = 5
               NORMAL_VEL = MULTI_FVM%FLUXES(6, KFACE, I)
               SUM_NORMALVEL = SUM_NORMALVEL + NORMAL_VEL
               KFACE = 6
               NORMAL_VEL = MULTI_FVM%FLUXES(6, KFACE, I)
               SUM_NORMALVEL = SUM_NORMALVEL + NORMAL_VEL
               KFACE = 2
               NORMAL_VEL = MULTI_FVM%FLUXES(6, KFACE, I)
               SUM_NORMALVEL = SUM_NORMALVEL + NORMAL_VEL
               KFACE = 4
               NORMAL_VEL = MULTI_FVM%FLUXES(6, KFACE, I)
               SUM_NORMALVEL = SUM_NORMALVEL + NORMAL_VEL
            ELSE
               DO KFACE = 1, NB_FACE
                  NORMAL_VEL = MULTI_FVM%FLUXES(6, KFACE, I)
                  SUM_NORMALVEL = SUM_NORMALVEL + NORMAL_VEL
               ENDDO
            ENDIF
            ALPHAII = MULTI_FVM%PHASE_ALPHA(IMAT, I)
            PII = MULTI_FVM%PHASE_PRES(IMAT, I)

C     Update massic fraction
            TMP2 = LBUFS(IMAT)%LBUF%VOL(II) * LBUFS(IMAT)%LBUF%RHO(II) - 
     .           TIMESTEP * SUMFLUX(2)
C     Udate energy
            TMP3 = LBUFS(IMAT)%LBUF%VOL(II) * LBUFS(IMAT)%LBUF%EINT(II) - 
     .           TIMESTEP * (SUMFLUX(3) + ALPHAII * (PII + PSHIFT) * SUM_NORMALVEL)
            

C     Update volume
            TMP4 = LBUFS(IMAT)%LBUF%VOL(II) - 
     .           TIMESTEP * (SUMFLUX(1) - ALPHAII * SUM_NORMALVEL)

C     Store volume in PHASE_ALPHA
            MULTI_FVM%PHASE_ALPHA(IMAT, I) = TMP4
C     Store mass density in PHASE_RHO
            MULTI_FVM%PHASE_RHO(IMAT, I) = TMP2
C     Store energy in PHASE_EINT
            MULTI_FVM%PHASE_EINT(IMAT, I) = TMP3
         ENDDO
      ENDDO


C-----------------------------------------------
C     E n d  o f   s u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE MULTI_EVOLVE_PARTIAL


