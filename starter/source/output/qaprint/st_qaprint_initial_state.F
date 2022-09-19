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
Chd|  ST_QAPRINT_INITIAL_STATE      source/output/qaprint/st_qaprint_initial_state.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_INITIAL_STATE(
     .                    NSIGSH   ,SIGSH    ,NSIGI   ,SIGSP    ,NSIGS   ,
     .                    SIGI     ,NSIGBEAM ,SIGBEAM ,NSIGTRUSS,SIGTRUSS,
     .                    NSIGRS   ,SIGRS    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
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
      INTEGER, INTENT(IN) :: NSIGSH,NSIGI,NSIGS,NSIGBEAM,NSIGTRUSS,NSIGRS
      my_real, INTENT(IN) :: SIGSH(MAX(1,NSIGSH),*),SIGSP(NSIGI,*),SIGI(NSIGS,*),
     .                       SIGBEAM(NSIGBEAM,*),SIGTRUSS(NSIGTRUSS,*),
     .                       SIGRS(NSIGRS,*)
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
C         /INIBRI
C------------------------------------

      OK_QA = MYQAKEY('/INIBRI')

      IF (OK_QA) THEN

        DO I=1,NUMSOL

         !!! ATTENTION no element ID storage in "SIGI" or "SIGSP" arrays
         !!! for /INIBRI, or /INIQUA
!
!!          ELEM_ID = NINT(SIGSP(1,I))
!!          WRITE(VARNAME,'(A)') 'ELEM_ID = '
!!          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ELEM_ID,0.0_8)
          ! print for : 'FILL','EPSP','ENER','DENS','STRESS'
          DO J=1,NSIGS
            TEMP_DOUBLE = SIGI(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
          ! print for all the rest, except above
          DO J=1,NSIGI
            TEMP_DOUBLE = SIGSP(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO

        ENDDO ! DO I=1,NUMSOL

      ENDIF ! IF (OK_QA) THEN


C------------------------------------
C         /INIHE3
C------------------------------------

      OK_QA = MYQAKEY('/INISHE')

      IF (OK_QA) THEN

        DO I=1,NUMSHEL

          ELEM_ID = NINT(SIGSH(1,I))
          WRITE(VARNAME,'(A)') 'ELEM_ID = '
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ELEM_ID,0.0_8)

          DO J=2,NSIGSH
            TEMP_DOUBLE = SIGSH(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO

        ENDDO ! DO I=1,NUMSHEL

      ENDIF ! IF (OK_QA) THEN


C------------------------------------
C         /INIHE3
C------------------------------------

      OK_QA = MYQAKEY('/INISH3')

      IF (OK_QA) THEN

        DO I=NUMSHEL+1,NUMSHEL+NUMSH3N

          ELEM_ID = NINT(SIGSH(1,I))
          WRITE(VARNAME,'(A)') 'ELEM_ID = '
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ELEM_ID,0.0_8)

          DO J=2,NSIGSH
            TEMP_DOUBLE = SIGSH(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO

        ENDDO ! DO I=1,NUMSHEL+1,NUMSHEL+NUMSH3N

      ENDIF ! IF (OK_QA) THEN


C------------------------------------
C         /INIQUA
C------------------------------------

      OK_QA = MYQAKEY('/INIQUA')

      IF (OK_QA) THEN

        DO I=1,NUMQUAD

         !!! ATTENTION no element storage in "SIGI" or "SIGSP" arrays
         !!! for /INIBRI, or /INIQUA
!
!!          ELEM_ID = NINT(SIGI(1,I))
!!          WRITE(VARNAME,'(A)') 'ELEM_ID = '
!!          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ELEM_ID,0.0_8)

          DO J=1,NSIGS
            TEMP_DOUBLE = SIGI(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO

        ENDDO ! DO I=1,NUMQUAD

      ENDIF ! IF (OK_QA) THEN


C------------------------------------
C         /INIBEAM
C------------------------------------

      OK_QA = MYQAKEY('/INIBEAM')

      IF (OK_QA) THEN

        DO I=1,NUMBEAM

          ELEM_ID = NINT(SIGBEAM(1,I))
          WRITE(VARNAME,'(A)') 'ELEM_ID = '
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ELEM_ID,0.0_8)

          DO J=2,NSIGBEAM
            TEMP_DOUBLE = SIGBEAM(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO

        ENDDO ! DO I=1,NUMBEAM

      ENDIF ! IF (OK_QA) THEN



C------------------------------------
C         /INITRUSS
C------------------------------------

      OK_QA = MYQAKEY('/INITRUSS')

      IF (OK_QA) THEN

        DO I=1,NUMTRUS

          ELEM_ID = NINT(SIGTRUSS(1,I))
          WRITE(VARNAME,'(A)') 'ELEM_ID = '
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ELEM_ID,0.0_8)

          DO J=2,NSIGTRUSS
            TEMP_DOUBLE = SIGTRUSS(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO

        ENDDO ! DO I=1,NUMTRUS

      ENDIF ! IF (OK_QA) THEN


C------------------------------------
C         /INISPRING
C------------------------------------

      OK_QA = MYQAKEY('/INISPRING')

      IF (OK_QA) THEN

        DO I=1,NUMSPRI

          ELEM_ID = NINT(SIGRS(1,I))
          WRITE(VARNAME,'(A)') 'ELEM_ID = '
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), ELEM_ID,0.0_8)

          DO J=2,NSIGRS
            TEMP_DOUBLE = SIGRS(J,I)
            WRITE(VARNAME,'(A)') 'VALUE = '
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO

        ENDDO ! DO I=1,NUMSPRI

      ENDIF ! IF (OK_QA) THEN
C------------------------------------
      RETURN
      END

