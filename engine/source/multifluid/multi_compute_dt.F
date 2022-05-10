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
Chd|  MULTI_COMPUTE_DT              source/multifluid/multi_compute_dt.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_COMPUTE_DT(DT2T, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .     PM, IPM, MULTI_FVM, WGRID, XGRID,
     .     NELTST, ITYPTST)
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
#include      "scr18_c.inc"         
#include      "units_c.inc"        
#include      "com08_c.inc"           
#include      "comlock.inc"        
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(OUT) :: DT2T
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      my_real, INTENT(IN) :: WGRID(*), XGRID(3, *)
      INTEGER, INTENT(OUT) :: ITYPTST, NELTST
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      INTEGER :: NG, NEL, II, JJ, KFACE, I, J, NB_FACE, NFT, ITY
      INTEGER :: IPLA
      INTEGER :: NBMAT, IMAT
      INTEGER :: NODE1, NODE2, NODE3, NODE4, 
     .     NODE5, NODE6, NODE7, NODE8
      my_real :: W1(3), W2(3), W3(3), W4(3), 
     .     W5(3), W6(3), W7(3), W8(3)
      my_real :: X1(3), X2(3), X3(3), X4(3), 
     .     X5(3), X6(3), X7(3), X8(3)
      my_real :: WFAC(1:3), SURF
      my_real :: LAMBDAII, LAMBDAF, NORMUII, NORMUJJ
      my_real :: FII(5), FJJ(5), NORMAL_VEL, NORMAL_VEL2, VII(5), VJJ(5), VEL2
      my_real :: DTEL(MVSIZ), NX, NY, NZ
      INTEGER :: ISOLNOD, MATLAW
      LOGICAL :: l_FOUND_LOWER

C     Time step
      DT2T = ZERO
      ITYPTST = 0
      NELTST = 0

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MATLAW = IPARG(1, NG)         
         IF (MATLAW .EQ. 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            ISOLNOD = IPARG(28, NG)
            GBUF => ELBUF_TAB(NG)%GBUF
C     DELTAX is to be kept
            GBUF%DELTAX(1:NEL) = ZERO
C     Number of faces in an element
            NB_FACE = 6 
            IF (ITY .EQ. 2) THEN
               NB_FACE = 4
            ELSEIF (ITY .EQ. 7) THEN
               NB_FACE = 3
            ENDIF
            
C     Flux computation
            DTEL(1:NEL) = ZERO
            DO II = 1, NEL
               I = II + NFT               
C     Face KFACE
               DO KFACE = 1, NB_FACE              
                  NX = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, I)
                  NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, I)
                  NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, I)
                  WFAC(1:3) = MULTI_FVM%FACE_DATA%WFAC(1:3, KFACE, I)
                  SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, I)
C     Time step
                  NORMAL_VEL2 = (MULTI_FVM%VEL(1, I) - WFAC(1)) * NX + 
     .                 (MULTI_FVM%VEL(2, I) - WFAC(2)) * NY + 
     .                 (MULTI_FVM%VEL(3, I) - WFAC(3)) * NZ
                  DTEL(II) = MAX(DTEL(II), 
     .                 SURF / GBUF%VOL(II) * (MULTI_FVM%SOUND_SPEED(I) + ABS(NORMAL_VEL2)) / DTFAC1(102))
                  GBUF%DELTAX(II) = MAX(GBUF%DELTAX(II), SURF / GBUF%VOL(II))
               ENDDO            !KFACE
               GBUF%DELTAX(II) = UN / GBUF%DELTAX(II)
            ENDDO ! II = 1, NEL
C----------------------
C     Globalize time step for this group
C----------------------
            l_FOUND_LOWER=.FALSE.
            DO II = 1, NEL
               IF(DTEL(II).GT.ZERO)GBUF%DT(II) = UN/DTEL(II)
               IF (DTEL(II) .GT. DT2T) THEN
                  l_FOUND_LOWER=.TRUE.
                  DT2T = DTEL(II)
                  ITYPTST = ITY
                  IF (MULTI_FVM%SYM .EQ. 0) THEN
                     NELTST = IXS(NIXS, II + NFT)
                  ELSE
                     IF (ITY .EQ. 2) THEN
C     QUADS
                        NELTST = IXQ(NIXQ, II + NFT)
                     ELSEIF (ITY .EQ. 7) THEN
C     TRIANGLES
                        NELTST = IXTG(NIXTG, II + NFT)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            
            !CHECK IF LOWER THAN DTMIN
            IF(l_FOUND_LOWER .AND. DT2T.NE.ZERO)THEN
              IF(UN/DT2T.LT.DTMIN1(102))THEN
                TSTOP = TT
#include "lockon.inc"
                WRITE(IOUT,*) ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR ALE/EULER CELL',NELTST
                WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR ALE/EULER CELL',NELTST
#include "lockoff.inc"          
              ENDIF  
            ENDIF           
            
         ENDIF
      ENDDO  ! NG = ITASK + 1, NGROUP, NTHREAD
C----------------------
C     Global time step
C----------------------
      IF (DT2T .GT. ZERO) THEN
         DT2T = UN / DT2T
      ELSE
         DT2T = EP30
      ENDIF
C----------------------
C     Boundary fluxes
C----------------------
      END SUBROUTINE MULTI_COMPUTE_DT
