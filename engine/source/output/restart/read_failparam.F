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
!||    read_failparam         ../engine/source/output/restart/read_failparam.F
!||--- called by ------------------------------------------------------
!||    read_matparam          ../engine/source/output/restart/read_matparam.F
!||--- calls      -----------------------------------------------------
!||    read_c_c               ../common_source/tools/input_output/write_routtines.c
!||    read_db                ../common_source/tools/input_output/read_db.F
!||    read_i_c               ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    fail_param_mod         ../common_source/modules/mat_elem/fail_param_mod.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE READ_FAILPARAM(FAIL)
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
      TYPE(FAIL_PARAM_) ,INTENT(OUT) :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IAD,NUPARAM,NIPARAM,NFUNC,NUMTABL,NMOD,LENI,LENR
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
      my_real ,DIMENSION(:) ,ALLOCATABLE :: RBUF
C=======================================================================
      LENI = 9
      ALLOCATE (IBUF(LENI))
      CALL READ_I_C(IBUF,LENI)
c
      FAIL%IRUPT   = IBUF(1)
      FAIL%FAIL_ID = IBUF(2)
      FAIL%NUPARAM = IBUF(3)
      FAIL%NIPARAM = IBUF(4)
      FAIL%NUVAR   = IBUF(5)
      FAIL%NFUNC   = IBUF(6)
      FAIL%NTABLE  = IBUF(7)
      FAIL%NMOD    = IBUF(8)
      FAIL%FAIL_IP = IBUF(9)
c
      DEALLOCATE(IBUF)
c
      LENR = 1
      ALLOCATE (RBUF(LENR))
      CALL READ_DB(RBUF,LENR)
      FAIL%PTHK = RBUF(1)
      DEALLOCATE(RBUF)
c
      NUPARAM = FAIL%NUPARAM
      NIPARAM = FAIL%NIPARAM
      NFUNC   = FAIL%NFUNC
      NUMTABL = FAIL%NTABLE
      NMOD    = FAIL%NMOD
      
c     read law keyword and keywords of failure modes

      CALL READ_C_C(NAME,NCHARTITLE)
      DO I=1,NCHARTITLE
        FAIL%KEYWORD(I:I) = CHAR(NAME(I))
      END DO
c           
      ALLOCATE (FAIL%MODE(NMOD))
      IF (NMOD > 0) THEN
        DO J = 1,NMOD
          CALL READ_C_C(NAME,NCHARTITLE)
          DO I = 1,NCHARTITLE
            FAIL%MODE(J)(I:I) = CHAR(NAME(I))
          END DO
        END DO
      END IF
c
c     read model parameters
c
      ALLOCATE (FAIL%UPARAM(NUPARAM))
      IF (NUPARAM > 0) THEN
        CALL READ_DB (FAIL%UPARAM ,NUPARAM)
      END IF
      ALLOCATE (FAIL%IPARAM(NIPARAM))
      IF (NIPARAM > 0) THEN
        CALL READ_I_C(FAIL%IPARAM ,NIPARAM)
      END IF
c      
c     read functions
c
      ALLOCATE (FAIL%IFUNC(NFUNC))
      IF (NFUNC > 0) THEN
        CALL READ_I_C(FAIL%IFUNC,NFUNC)
      END IF
c      
c     read function tables
c
      ALLOCATE (FAIL%TABLE(NUMTABL))
      IF (NUMTABL > 0) THEN
        CALL READ_I_C(FAIL%TABLE,NUMTABL)
      END IF
c-----------
      RETURN
      END
