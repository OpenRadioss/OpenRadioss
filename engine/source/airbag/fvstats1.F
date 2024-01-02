Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FVSTATS1                      source/airbag/fvstats1.F      
Chd|-- called by -----------
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        FVSTATS                       source/airbag/fvstats.F       
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE FVSTATS1(IOUT,MONVOL,INFO)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IOUT, MONVOL(*), INFO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, N, ITYP, ID, IFV
C
      K1 = 1
      IFV = 0
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP==6 .OR. ITYP==8) THEN
           IFV = IFV + 1
           ID=MONVOL(K1-1+1)
           ! INFO == 1 : FVSTATS1 called only by processor 0 after an
           ! update of MONVOL
           ! INFO == 0 : FVSTATS1 called by every processor
           IF((FVSPMD(IFV)%PMAIN-1 == ISPMD) .OR. (NSPMD == 1) .OR. INFO==1) THEN
             WRITE(IOUT,'(/1X,A,I10,A)')'** FINITE VOLUME',ID,' MERGE STATISTICS **'
             WRITE(IOUT,'(4X,A,I10)')
     .         'NUMBER OF REMAINING FINITE VOLUMES .....:',MONVOL(K1-1+49)
             WRITE(IOUT,'(4X,A,I10)') 
     .         'GLOBAL MERGE ...........................:',MONVOL(K1-1+70)
             WRITE(IOUT,'(4X,A,I10)')
     .         'NEIGHBORHOOD MERGE .....................:',MONVOL(K1-1+71)
             WRITE(IOUT,'(4X,A,I10)')
     .         'STABILITY MERGE ........................:',MONVOL(K1-1+72)
             WRITE(IOUT,'(4X,A,I10)')
     .         'TIME STEP MERGE ........................:',MONVOL(K1-1+73)
           ENDIF
         ENDIF
         K1=K1+NIMV
      ENDDO
C
      RETURN
      END

