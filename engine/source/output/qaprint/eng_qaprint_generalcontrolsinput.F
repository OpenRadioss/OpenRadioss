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
Chd|  ENG_QAPRINT_GENERALCONTROLSINPUTsource/output/qaprint/eng_qaprint_generalcontrolsinput.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ENG_QAPRINT_GENERALCONTROLSINPUT(IRUNN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "check.inc"
#include      "chara_c.inc"
#include      "titr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRUNN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C-----------------------------------------------
C     QA PRINT FOR RUN INPUT
C-----------------------------------------------
      IF (MYQAKEY('/RUN')) THEN
        CALL QAPRINT(ROOTNAM(1:ROOTLEN),IRUNN,0.0_8)

C       Le restart letter du run sert de nom de la variable dans le ref.extract , suivi de l'ID du run
        IF(LEN_TRIM(CHRUN0)/=0)THEN
          CALL QAPRINT(CHRUN0(1:LEN_TRIM(CHRUN0)),IRUNN,0.0_8)
        ELSE
          CALL QAPRINT('A_FAKE_RESTART_LETTER',IRUNN,0.0_8)
        END IF

        WRITE(VARNAME,'(A)') 'TSTOP'
        TEMP_DOUBLE = TSTOP
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
      ENDIF

C-----------------------------------------------
C     QA PRINT FOR VERS INPUT
C-----------------------------------------------
      IF (MYQAKEY('/VERS')) THEN
        WRITE(VARNAME,'(A)') 'INVERS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INVERS,0.0_8)
      ENDIF

C-----------------------------------------------
C     QA PRINT FOR RFILE INPUT
C-----------------------------------------------
      IF (MYQAKEY('/RFILE')) THEN
        WRITE(VARNAME,'(A)') 'MULTIREST'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),MULTIREST,0.0_8)

        WRITE(VARNAME,'(A)') 'NCRST'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NCRST,0.0_8)

        WRITE(VARNAME,'(A)') 'IRLTYP'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IRLTYP,0.0_8)

        WRITE(VARNAME,'(A)') 'IRETYP'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IRETYP,0.0_8)

        WRITE(VARNAME,'(A)') 'RF_CHECK'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),RF_CHECK,0.0_8)
      ENDIF

C-----------------------------------------------
C     QA PRINT FOR TFILE INPUT
C-----------------------------------------------
      IF (MYQAKEY('/TFILE')) THEN
        WRITE(VARNAME,'(A)') 'DTHIS'
        TEMP_DOUBLE = DTHIS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'ITFORM'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITFORM,0.0_8)

        DO I=1,9
          WRITE(VARNAME,'(A,I0)') 'DTHIS1_', I
          TEMP_DOUBLE = DTHIS1(I)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO
      ENDIF

C-----------------------------------------------
C     QA PRINT FOR STOP INPUT
C-----------------------------------------------
      IF (MYQAKEY('/STOP')) THEN
        WRITE(VARNAME,'(A)') 'NSTOPSENS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSTOPSENS,0.0_8)

        DO I=1,NSENSOR
          WRITE(VARNAME,'(A,I0)') 'LSTOPSENS_', I
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LSTOPSENS(I),0.0_8)
        ENDDO

        WRITE(VARNAME,'(A)') 'NSANIM'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSANIM,0.0_8)

        WRITE(VARNAME,'(A)') 'NSTH'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSTH,0.0_8)

        WRITE(VARNAME,'(A)') 'NSSTAT'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSSTAT,0.0_8)

        WRITE(VARNAME,'(A)') 'NSOUTP'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSOUTP,0.0_8)

        WRITE(VARNAME,'(A)') 'NSABF'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSABF,0.0_8)

        WRITE(VARNAME,'(A)') 'NSH3D'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSH3D,0.0_8)

        WRITE(VARNAME,'(A)') 'NTH'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NTH,0.0_8)

        WRITE(VARNAME,'(A)') 'NANIM'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NANIM,0.0_8)

        WRITE(VARNAME,'(A)') 'NERR_POSIT'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NERR_POSIT,0.0_8)

        WRITE(VARNAME,'(A)') 'DMTMXS'
        TEMP_DOUBLE = DMTMXS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DMNMXS'
        TEMP_DOUBLE = DMNMXS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DEMXS'
        TEMP_DOUBLE = DEMXS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DMTMXK'
        TEMP_DOUBLE = DMTMXK
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DMNMXK'
        TEMP_DOUBLE = DMNMXK
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DEMXK'
        TEMP_DOUBLE = DEMXK
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
      ENDIF


      RETURN
      END
