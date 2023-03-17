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
Chd|  ENG_QAPRINT_DTINPUT           source/output/qaprint/eng_qaprint_dtinput.F
Chd|-- called by -----------
Chd|        ENG_QAPRINT_DRIVER            source/output/qaprint/eng_qaprint_driver.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ENG_QAPRINT_DTINPUT()
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
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "scr17_c.inc"
#include      "scr_thermal_c.inc"
#include      "sms_c.inc"
#include      "cong2_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C=======================================================================
C-----------------------------------------------
C     QA PRINT FOR DT INPUTS
C-----------------------------------------------
      IF (MYQAKEY('/DT')) THEN

        WRITE(VARNAME,'(A)') 'NODADT'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NODADT,0.0_8)

        DO I=1,102 
C         VARNAME: variable name in ref.extract (without blanks)
          WRITE(VARNAME,'(A,I0)') 'IDTMIN_',I      ! IDTMIN(11) => 'IDTMIN_11'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDTMIN(I),0.0_8)

          WRITE(VARNAME,'(A,I0)') 'IDTGR_',I
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDTGR(I),0.0_8)

          WRITE(VARNAME,'(A,I0)') 'DTFAC_',I
          TEMP_DOUBLE = DTFAC1(I)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

          WRITE(VARNAME,'(A,I0)') 'DTMIN_',I
          TEMP_DOUBLE = DTMIN1(I)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO

        WRITE(VARNAME,'(A)') 'MIN_ASPECT'
        TEMP_DOUBLE = MIN_ASPECT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'MIN_DEFV'
        TEMP_DOUBLE = MIN_DEFV
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

C-----------------------------------------------
C     AMS
C-----------------------------------------------
        WRITE(VARNAME,'(A)') 'IDTMINS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDTMINS,0.0_8)

        WRITE(VARNAME,'(A)') 'DTMINS'
        TEMP_DOUBLE = DTMINS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DTFACS'
        TEMP_DOUBLE = DTFACS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'TOL_SMS'
        TEMP_DOUBLE = TOL_SMS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'NSMSPCG'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSMSPCG,0.0_8)

C-----------------------------------------------
C     /DTIX
C-----------------------------------------------
        WRITE(VARNAME,'(A)') 'DTIN'
        TEMP_DOUBLE = DTIN
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DTMX'
        TEMP_DOUBLE = DTMX
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

C-----------------------------------------------
C     Interface
C-----------------------------------------------
        WRITE(VARNAME,'(A)') 'IDTMINS_INT'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDTMINS_INT,0.0_8)

        WRITE(VARNAME,'(A)') 'DTMINS_INT'
        TEMP_DOUBLE = DTMINS_INT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DTFACS_INT'
        TEMP_DOUBLE = DTFACS_INT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

C-----------------------------------------------
C     Thermique
C-----------------------------------------------
        WRITE(VARNAME,'(A)') 'NODADT_THERM'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NODADT_THERM,0.0_8)

        WRITE(VARNAME,'(A)') 'IDT_THERM'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDT_THERM,0.0_8)

        WRITE(VARNAME,'(A)') 'DTFACTHERM'
        TEMP_DOUBLE = DTFACTHERM
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

C-----------------------------------------------

        WRITE(VARNAME,'(A)') 'IDT1SH'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDT1SH,0.0_8)

        WRITE(VARNAME,'(A)') 'IDT1SOL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDT1SOL,0.0_8)

        WRITE(VARNAME,'(A)') 'IDT1TET10'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDT1TET10,0.0_8)


      ENDIF

      RETURN
      END
