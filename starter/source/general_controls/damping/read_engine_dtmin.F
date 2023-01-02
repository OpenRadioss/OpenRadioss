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
Chd|  READ_ENGINE_DTMIN             source/general_controls/damping/read_engine_dtmin.F
Chd|-- called by -----------
Chd|        DAMPDTNODA                    source/general_controls/damping/dampdtnoda.F
Chd|-- calls ---------------
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE READ_ENGINE_DTMIN(DTMIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real , INTENT(OUT) :: DTMIN 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IO_ERR1, IFUND
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
      my_real
     .   DFAC
C-----------------------------------------------
      IO_ERR1=0
      DTMIN = ZERO
C-----------------------------------------------   
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'

      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
      LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+9
      OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
C
      IF (IO_ERR1/=0) THEN
         FILNAM=ROOTNAM(1:ROOTLEN)//'D01'
         TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+3)
         LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+3
         OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
      ENDIF      

      IF (IO_ERR1==0) THEN
C
        IFUND = 0
        DO WHILE (IFUND==0)
          READ(71,'(A)',END=20) KEYA
C
          IF(KEYA(1:12)=='/DT/NODA/CST') THEN
            READ(71,*,ERR=20,END=20) DFAC,DTMIN
            IFUND = 1
          ENDIF
C
        END DO
C
 20     CONTINUE

        CLOSE(71)
      ENDIF
C------------------------------------------- 
      RETURN
      END
