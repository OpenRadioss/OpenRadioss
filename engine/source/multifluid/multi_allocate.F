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
Chd|  MULTI_ALLOCATE                source/multifluid/multi_allocate.F
Chd|-- called by -----------
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_ALLOCATE(MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
!NSPMD
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "spmd_c.inc"
C----------------------------------------------
C     D u m m y   a r g u m e n t s
C----------------------------------------------
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IERR, LEN, NBMAT, IMAT
C----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C----------------------------------------------
C     Multifluid law
        IF (MULTI_FVM%IS_USED) THEN
C     Number of submaterials
           NBMAT = MULTI_FVM%NBMAT
           IF (N2D == 0) THEN
              LEN = NUMELS + NSVOIS
           ELSE
              LEN = NUMELQ + NQVOIS + NUMELTG + NTGVOIS              
           ENDIF
           MULTI_FVM%NELEM = LEN
C     Face datas
           ALLOCATE(MULTI_FVM%FACE_DATA%SURF(6, LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF 
           ALLOCATE(MULTI_FVM%FACE_DATA%NORMAL(3, 6, LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF 
           ALLOCATE(MULTI_FVM%FACE_DATA%WFAC(3, 6, LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF 
C     fluxes
           ALLOCATE(MULTI_FVM%FLUXES(6, 6, LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF     
C     submaterial fluxes
C     Volume fluxes
           IF (NBMAT > 1) THEN
              ALLOCATE(MULTI_FVM%SUBVOL_FLUXES(NBMAT, 6, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF 
              ALLOCATE(MULTI_FVM%SUBMASS_FLUXES(NBMAT, 6, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              ALLOCATE(MULTI_FVM%SUBENER_FLUXES(NBMAT, 6, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF  
              
              ALLOCATE(MULTI_FVM%PHASE_ALPHA(NBMAT, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF 
              ALLOCATE(MULTI_FVM%PHASE_PRES(NBMAT, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF 
              ALLOCATE(MULTI_FVM%PHASE_RHO(NBMAT, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              ALLOCATE(MULTI_FVM%PHASE_EINT(NBMAT, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
           ENDIF
C     Allocation of centroid velocities
           ALLOCATE(MULTI_FVM%VEL(3, LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF     
           MULTI_FVM%VEL(:, :) = ZERO
C     Allocation of soud speed
           ALLOCATE(MULTI_FVM%SOUND_SPEED(LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF         
           MULTI_FVM%SOUND_SPEED(:) = ZERO
C     Allocation of TBURN
           ALLOCATE(MULTI_FVM%TBURN(LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF           
           MULTI_FVM%TBURN(:) = ZERO  
C     Allocation of BFRAC
           ALLOCATE(MULTI_FVM%BFRAC(NBMAT, LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF           
           MULTI_FVM%BFRAC(:, :) = ZERO  
C     Allocation of temporary var 1
           ALLOCATE(MULTI_FVM%RHO(LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF        
           MULTI_FVM%RHO(:) = ZERO     
C     Allocation of temporary var 2
           ALLOCATE(MULTI_FVM%EINT(LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF       
           MULTI_FVM%EINT(:) = ZERO
C     Allocation of temporary var 3
           ALLOCATE(MULTI_FVM%PRES(LEN), STAT = IERR)
           IF (IERR /= 0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF       
           MULTI_FVM%PRES(:) = ZERO
C     MUSCL Variables
           IF (MULTI_FVM%MUSCL == 1) THEN
              ALLOCATE(MULTI_FVM%GRAD_RHO(3, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              ALLOCATE(MULTI_FVM%GRAD_U(3, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              ALLOCATE(MULTI_FVM%GRAD_V(3, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              ALLOCATE(MULTI_FVM%GRAD_W(3, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              ALLOCATE(MULTI_FVM%GRAD_PRES(3, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              IF (NBMAT > 1) THEN                 
                 ALLOCATE(MULTI_FVM%PHASE_GRAD_RHO(3, NBMAT, LEN), STAT = IERR)
                 IF (IERR /= 0) THEN
                    CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                    CALL ARRET(2)
                 ENDIF
                 ALLOCATE(MULTI_FVM%PHASE_GRAD_ALPHA(3, NBMAT, LEN), STAT = IERR)
                 IF (IERR /= 0) THEN
                    CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                    CALL ARRET(2)
                 ENDIF
                 ALLOCATE(MULTI_FVM%PHASE_GRAD_PRES(3, NBMAT, LEN), STAT = IERR)
                 IF (IERR /= 0) THEN
                    CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                    CALL ARRET(2)
                 ENDIF
              ENDIF
           ENDIF
             
           IF (MULTI_FVM%MUSCL == 2) THEN
C     Only volume fraction
              IF (NBMAT > 1) THEN                 
                 ALLOCATE(MULTI_FVM%PHASE_GRAD_ALPHA(3, NBMAT, LEN), STAT = IERR)
                 IF (IERR /= 0) THEN
                    CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                    CALL ARRET(2)
                 ENDIF
              ENDIF
           ENDIF
           IF (MULTI_FVM%MUSCL > 0) THEN
              ALLOCATE(MULTI_FVM%FACE_DATA%CENTROID(3, 6, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
              ALLOCATE(MULTI_FVM%ELEM_DATA%CENTROID(3, LEN), STAT = IERR)
              IF (IERR /= 0) THEN
                 CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                 CALL ARRET(2)
              ENDIF
           ENDIF
        ENDIF
C----------------------------------------------
C     E n d   o f   s u b r o u t i n e
C----------------------------------------------
      END SUBROUTINE MULTI_ALLOCATE
