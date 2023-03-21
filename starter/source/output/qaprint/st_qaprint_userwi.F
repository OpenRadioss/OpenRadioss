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
Chd|  ST_QAPRINT_USERWI             source/output/qaprint/st_qaprint_userwi.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_USERWI(USER_WINDOWS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE USER_WINDOWS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr10_c.inc"
#include      "units_c.inc"
#include      "nchar_c.inc"
#include      "scry_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(USER_WINDOWS_),INTENT(IN)      :: USER_WINDOWS
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER I,J,ELEM_ID
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------


C------------------------------------
C         /USERWI
C------------------------------------

      OK_QA = MYQAKEY('/USERWI')  

      IF (OK_QA) THEN

        DO I=1,USER_WINDOWS%S_IUSER

            WRITE(VARNAME,'(A6,I1,A)') 'IUVAR(',I,')_'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),USER_WINDOWS%IUSER(I),0.0_8)

        ENDDO ! DO I=1,USER_WINDOWS%S_IUSER

        DO I=1,USER_WINDOWS%S_USER
            TEMP_DOUBLE = USER_WINDOWS%USER(I)
            WRITE(VARNAME,'(A5,I1,A)') 'UVAR(',I,')_'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
        ENDDO ! DO I=1,USER_WINDOWS%S_USER

        DO I=1,USER_WINDOWS%N_USERNODS
            WRITE(VARNAME,'(A6,I1,A)') 'USERNODS(',I,')_'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),USER_WINDOWS%USERNODS(I),0.0_8)
        ENDDO ! DO I=1,USER_WINDOWS%S_IUSER

      ENDIF ! IF (OK_QA) THEN

C------------------------------------
      RETURN
      END

