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
!||====================================================================
!||    write_failparam        ../engine/source/output/restart/write_failparam.F
!||--- called by ------------------------------------------------------
!||    write_matparam         ../engine/source/output/restart/write_matparam.F
!||--- calls      -----------------------------------------------------
!||    write_c_c              ../common_source/tools/input_output/write_routtines.c
!||    write_db               ../common_source/tools/input_output/write_db.F
!||    write_i_c              ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    fail_param_mod         ../common_source/modules/mat_elem/fail_param_mod.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE WRITE_FAILPARAM(FAIL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE NAMES_AND_TITLES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(FAIL_PARAM_) ,INTENT(IN)    :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IAD,LENI,LENR,NUPARAM,NIPARAM,NUMTABL,NFUNC
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
      my_real ,DIMENSION(:) ,ALLOCATABLE :: RBUF
C=======================================================================
      LENI = 9
      ALLOCATE (IBUF(LENI))
c
      IBUF(1) = FAIL%IRUPT
      IBUF(2) = FAIL%FAIL_ID
      IBUF(3) = FAIL%NUPARAM
      IBUF(4) = FAIL%NIPARAM
      IBUF(5) = FAIL%NUVAR
      IBUF(6) = FAIL%NFUNC
      IBUF(7) = FAIL%NTABLE
      IBUF(8) = FAIL%NMOD
      IBUF(9) = FAIL%FAIL_IP
c
      CALL WRITE_I_C(IBUF,LENI)
      DEALLOCATE(IBUF)
c
      LENR = 1
      ALLOCATE (RBUF(LENR))
      RBUF(1) = FAIL%PTHK
      CALL WRITE_DB(RBUF ,LENR)
      DEALLOCATE(RBUF)
c
c     write law keyword and keywords of failure modes
c
      DO I=1,NCHARTITLE
        NAME(I) = ICHAR(FAIL%KEYWORD(I:I))
      END DO
      CALL WRITE_C_C(NAME,NCHARTITLE)
c           
      DO J=1,FAIL%NMOD
        DO I=1,NCHARTITLE
          NAME(I) = ICHAR(FAIL%MODE(J)(I:I))
        END DO
        CALL WRITE_C_C(NAME,NCHARTITLE)
      END DO
c
c     write parameter tables

      NUPARAM = FAIL%NUPARAM
      NIPARAM = FAIL%NIPARAM
      IF (NUPARAM > 0) THEN
        CALL WRITE_DB(FAIL%UPARAM ,NUPARAM)
      END IF      
      IF (NIPARAM > 0) THEN
        CALL WRITE_I_C(FAIL%IPARAM ,NIPARAM)
      END IF      
c      
c     write function adresses
c
      NFUNC = FAIL%NFUNC
      IF (NFUNC > 0) THEN
        CALL WRITE_I_C(FAIL%IFUNC,NFUNC)
      END IF
c      
c     write function tables
c
      NUMTABL  = FAIL%NTABLE
      IF (NUMTABL > 0) THEN
        CALL WRITE_I_C(FAIL%TABLE,NUMTABL)
      END IF
c-----------
      RETURN
      END
