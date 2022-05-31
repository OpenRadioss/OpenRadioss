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
Chd|  MULTI_FLUXOUT_EBCS            source/multifluid/multi_fluxout_ebcs.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_FLUXOUT_EBCS(MULTI_FVM, NELEM, ELEM_LIST, FACE_LIST, 
     .     IXS, IXQ, IXTG, XGRID, WGRID)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------  
! NIXS
#include      "param_c.inc"
! ISPMD
#include      "task_c.inc"
! ALE
#include      "com01_c.inc"
! NUMELS, NUMELQ, NUMELTG
#include      "com04_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: NELEM, ELEM_LIST(NELEM), FACE_LIST(NELEM)
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: XGRID(3, *), WGRID(3, *)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IELEM, ELEMID, NODE_ID
      INTEGER :: KFACE, NB_NOD_FOUND, KNOD, KK, JJ
      my_real :: X1(3), X2(3), X3(3), X4(3), P, NORMVEL, VX, VY, VZ, SSP, SURF, NX, NY, NZ
      my_real :: RHO, SSTAR, PSTAR, PFAC, SL, SR, WFAC(3), VII(5), VJJ(5), VEL2, NORMALW
      my_real :: FII(5), FJJ(5), VIISTAR(5), VJJSTAR(5), FIISTAR(5), FJJSTAR(5), PP(5)
      INTEGER :: NODE1, NODE2, IMAT
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      DO IELEM = 1, NELEM
         ELEMID = ELEM_LIST(IELEM)

C     Face of the element
         KFACE = FACE_LIST(IELEM)
         
         NX = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, ELEMID)
         NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, ELEMID)
         NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, ELEMID)
         SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, ELEMID)
         WFAC(1:3) = MULTI_FVM%FACE_DATA%WFAC(1:3, KFACE, ELEMID) 
         VX = MULTI_FVM%VEL(1, ELEMID)
         VY = MULTI_FVM%VEL(2, ELEMID)
         VZ = MULTI_FVM%VEL(3, ELEMID)
         NORMVEL = VX * NX + VY * NY + VZ * NZ
         NORMALW = WFAC(1) * NX + WFAC(2) * NY + WFAC(3) * NZ
         SSP = MULTI_FVM%SOUND_SPEED(ELEMID)
         P = MULTI_FVM%PRES(ELEMID)
         RHO = MULTI_FVM%RHO(ELEMID)

         VEL2 = VX * VX + VY * VY + VZ * VZ 

         VII(1) = RHO
         VII(2) = RHO * VX
         VII(3) = RHO * VY
         VII(4) = RHO * VZ
         VII(5) = MULTI_FVM%EINT(ELEMID) + HALF * RHO * VEL2

         FII(1) = VII(1) * NORMVEL
         FII(2) = VII(2) * NORMVEL + P * NX
         FII(3) = VII(3) * NORMVEL + P * NY
         FII(4) = VII(4) * NORMVEL + P * NZ
         FII(5) = (VII(5) + P) * NORMVEL
        
         MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FII(1:5) - NORMALW * VII(1:5)) * SURF
         MULTI_FVM%FLUXES(6, KFACE, ELEMID) = SURF * NORMVEL

         IF (MULTI_FVM%NBMAT .GT. 1) THEN
            MULTI_FVM%FLUXES(1, KFACE, ELEMID) = ZERO
            DO IMAT = 1, MULTI_FVM%NBMAT
               VII(1) = MULTI_FVM%PHASE_ALPHA(IMAT, ELEMID)
               VII(2) = MULTI_FVM%PHASE_ALPHA(IMAT, ELEMID) * 
     .              MULTI_FVM%PHASE_RHO(IMAT, ELEMID)
               VII(3) = MULTI_FVM%PHASE_ALPHA(IMAT, ELEMID) * 
     .              MULTI_FVM%PHASE_EINT(IMAT, ELEMID)
               FII(1:3) = VII(1:3) * NORMVEL
               MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE, ELEMID) = 
     .              (FII(1) - NORMALW * VII(1)) * SURF
               MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE, ELEMID) = 
     .              (FII(2) - NORMALW * VII(2)) * SURF
               MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE, ELEMID) = 
     .              (FII(3) - NORMALW * VII(3)) * SURF
               MULTI_FVM%FLUXES(1, KFACE, ELEMID) = MULTI_FVM%FLUXES(1, KFACE, ELEMID) +
     .              (FII(2) - NORMALW * VII(2)) * SURF
            ENDDO
         ENDIF
      ENDDO
C-----------------------------------------------
C     E n d   o f   s u b r o u t i n e 
C-----------------------------------------------
      END SUBROUTINE MULTI_FLUXOUT_EBCS
