Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
C When compiled with -DDEBUG_RST
C calls to WRITE_I_C in other parts of the code will be replace by
C WRITE_I_C_DEBUG
C Same for WRITE_DB
C This allows to use the option -flush_rst_to_txt at the starter 

!||====================================================================
!||    write_i_c_debug        ../starter/source/output/tools/write_debug.F
!||--- calls      -----------------------------------------------------
!||    fretitl2               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE WRITE_I_C_DEBUG(A,N,SA)
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
! Do not replace IMPLICIT NONE by an include here
      IMPLICIT NONE
#include "debug_rst.inc"
      INTEGER :: A(N),N
      CHARACTER(len=*) ::  SA
      CHARACTER(LEN=NCHARTITLE) :: TITR
      INTEGER :: I,IT,J
      INTEGER :: NT
#if defined(_OPENMP)
       INTEGER OMP_GET_THREAD_NUM
       EXTERNAL OMP_GET_THREAD_NUM
       NT = OMP_GET_THREAD_NUM()
#else
       NT=1
#endif
      IF(FLUSH_RST_TO_TXT) THEN
        TITR(1:nchartitle) = " "
        IF(N==ceiling(dble(nchartitle)/3.0D0)) THEN
          CALL FRETITL2(TITR,A,ceiling(dble(nchartitle)/3.0D0))
          WRITE(777+NT,*) TRIM(SA),' '
          J = 1
          DO I = 1,N
               WRITE(777+NT,*) A(I),TITR(J:J+2)
               J = J + 3
          ENDDO
        ELSE
          WRITE(777+NT,*) TRIM(SA),' '
          DO I = 1,N
            WRITE(777+NT,*) A(I)
          ENDDO
        ENDIF
      ENDIF

      CALL WRITE_I_C(A,N)


      END SUBROUTINE

!||====================================================================
!||    write_db_debug   ../starter/source/output/tools/write_debug.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE WRITE_DB_DEBUG(A,N,SA)
! Do not replace IMPLICIT NONE by an include here
      IMPLICIT NONE
#include "debug_rst.inc"
      INTEGER :: N
      DOUBLE PRECISION :: A(N)
      CHARACTER(len=*) ::  SA
      INTEGER :: I,NT
#if defined(_OPENMP)
       INTEGER OMP_GET_THREAD_NUM
       EXTERNAL OMP_GET_THREAD_NUM
       NT = OMP_GET_THREAD_NUM()
#else
       NT=1  
#endif

  
      IF(FLUSH_RST_TO_TXT) THEN
        WRITE(777+NT,*) TRIM(SA),' '
        DO I = 1,N
          WRITE(777+NT,'(E10.4,2X,Z20)') A(I),A(I)
        ENDDO
      ENDIF
      CALL WRITE_DB(A,N)

      END SUBROUTINE



