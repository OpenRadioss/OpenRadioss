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
Chd|  MULTI_UPDATE_GLOBAL           source/multifluid/multi_update_global.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        MULTI_BILAN                   source/multifluid/multi_bilan.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_UPDATE_GLOBAL(NG, ELBUF_TAB, IPARG, ITASK, MULTI_FVM, 
     .     PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, VOLNEW, PRED)
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
      INTEGER, INTENT(IN) :: NG
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      my_real, INTENT(INOUT) :: PARTSAV(*), GRESAV(*)
      INTEGER, INTENT(IN) :: IPARTS(*), IGRTH(*), GRTH(*)
      my_real, INTENT(IN) :: VOLNEW(*)
      LOGICAL, INTENT(IN) :: PRED
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      INTEGER :: II, I
      my_real :: RHO, ETOT, VEL2, VOL, SUMFLUX(5)
      my_real :: VII(5)
      INTEGER :: NEL, NFT
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF
      NEL = IPARG(2, NG)
      NFT = IPARG(3, NG)

C     Update global quantities
      DO II = 1, NEL
         I = II + NFT
         VII(1) = MULTI_FVM%RHO(I)
         VII(2) = MULTI_FVM%VEL(1, I)
         VII(3) = MULTI_FVM%VEL(2, I)
         VII(4) = MULTI_FVM%VEL(3, I)
         VII(5) = MULTI_FVM%EINT(I)
         ! Divide vy new volume
         VII(1:5) = VII(1:5) / VOLNEW(II)
! RHO
         MULTI_FVM%RHO(I) = VII(1)
! Velocity
         MULTI_FVM%VEL(1, I) = VII(2) / VII(1)
         MULTI_FVM%VEL(2, I) = VII(3) / VII(1)
         MULTI_FVM%VEL(3, I) = VII(4) / VII(1)
! Square norm of the velocity
         VEL2 = MULTI_FVM%VEL(1, I)**2 + MULTI_FVM%VEL(2, I)**2 + MULTI_FVM%VEL(3, I)**2
! Internal energy
         MULTI_FVM%EINT(I) = VII(5) - HALF * VEL2 * VII(1)
      ENDDO
C     Balance for output
      IF (.NOT. PRED) THEN
         CALL MULTI_BILAN(PARTSAV, VOLNEW, MULTI_FVM, 
     .        NEL, NFT, IPARTS(1 + NFT), GRESAV, IGRTH, GRTH)
      ENDIF
C-----------------------------------------------
C     E n d  o f   s u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE MULTI_UPDATE_GLOBAL
