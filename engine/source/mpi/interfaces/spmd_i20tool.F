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
      !||    spmd_fiadd20_pon   ../engine/source/mpi/interfaces/spmd_i20tool.F
      !||--- called by ------------------------------------------------------
      !||    spmd_i7fcom_pon    ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||--- calls      -----------------------------------------------------
      !||    getdpdaanc         ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||====================================================================
      SUBROUTINE SPMD_FIADD20_PON(
     1       NB, LEN, NSVSI, BUFR, DAANC6, NSV, ALPHAK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, NSVSI(*),NSV(*)
      my_real
     .        BUFR(LEN,*), ALPHAK(3,*)
      DOUBLE PRECISION DAANC6(3,6,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, INC ,ISIGN
      DOUBLE PRECISION DAANC6L(3,6) 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      DO I = 1, NB
        N = NSV(NSVSI(I))
        ISIGN = 1
        IF(ALPHAK(2,N) < ZERO .or. BUFR(1,I) < ZERO)ISIGN = -1
        ALPHAK(2,N) = ISIGN*MIN(ABS(ALPHAK(2,N)),ABS(BUFR(1,I)))
        CALL GETDPDAANC(DAANC6L,BUFR(2,I),IRESP,INC)
        DAANC6(1,1,N) = DAANC6(1,1,N)+DAANC6L(1,1)
        DAANC6(1,2,N) = DAANC6(1,2,N)+DAANC6L(1,2)
        DAANC6(1,3,N) = DAANC6(1,3,N)+DAANC6L(1,3)
        DAANC6(1,4,N) = DAANC6(1,4,N)+DAANC6L(1,4)
        DAANC6(1,5,N) = DAANC6(1,5,N)+DAANC6L(1,5)
        DAANC6(1,6,N) = DAANC6(1,6,N)+DAANC6L(1,6)
        DAANC6(2,1,N) = DAANC6(2,1,N)+DAANC6L(2,1)
        DAANC6(2,2,N) = DAANC6(2,2,N)+DAANC6L(2,2)
        DAANC6(2,3,N) = DAANC6(2,3,N)+DAANC6L(2,3)
        DAANC6(2,4,N) = DAANC6(2,4,N)+DAANC6L(2,4)
        DAANC6(2,5,N) = DAANC6(2,5,N)+DAANC6L(2,5)
        DAANC6(2,6,N) = DAANC6(2,6,N)+DAANC6L(2,6)
        DAANC6(3,1,N) = DAANC6(3,1,N)+DAANC6L(3,1)
        DAANC6(3,2,N) = DAANC6(3,2,N)+DAANC6L(3,2)
        DAANC6(3,3,N) = DAANC6(3,3,N)+DAANC6L(3,3)
        DAANC6(3,4,N) = DAANC6(3,4,N)+DAANC6L(3,4)
        DAANC6(3,5,N) = DAANC6(3,5,N)+DAANC6L(3,5)
        DAANC6(3,6,N) = DAANC6(3,6,N)+DAANC6L(3,6)
      END DO
C
      RETURN
      END
C
      !||====================================================================
      !||    spmd_fiadd20e_pon   ../engine/source/mpi/interfaces/spmd_i20tool.F
      !||--- called by ------------------------------------------------------
      !||    spmd_i7fcom_pon     ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||--- calls      -----------------------------------------------------
      !||    getdpdaanc          ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||====================================================================
      SUBROUTINE SPMD_FIADD20E_PON(
     1       NB, LEN, NSVSI, BUFR, DAANC6, IXLINS, ALPHAK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, NSVSI(*), IXLINS(2,*)
      my_real
     .        BUFR(LEN,*), ALPHAK(3,*)
      DOUBLE PRECISION  DAANC6(3,6,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NL1, NL2, INC ,ISIGN
      DOUBLE PRECISION DAANC6L(3,6) 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      DO I = 1, NB
        N = NSVSI(I)
        NL1 = IXLINS(1,N)
        NL2 = IXLINS(2,N)
        ISIGN = 1
        IF(ALPHAK(2,NL1) < ZERO .or. BUFR(1,I) < ZERO)ISIGN = -1
        ALPHAK(2,NL1) = ISIGN*MIN(ABS(ALPHAK(2,NL1)),ABS(BUFR(1,I)))          
        ISIGN = 1
        IF(ALPHAK(2,NL2) < ZERO .or. BUFR(2,I) < ZERO)ISIGN = -1
        ALPHAK(2,NL2) = ISIGN*MIN(ABS(ALPHAK(2,NL2)),ABS(BUFR(2,I)))          
        CALL GETDPDAANC(DAANC6L,BUFR(3,I),IRESP,INC)
        DAANC6(1,1,NL1) = DAANC6(1,1,NL1)+DAANC6L(1,1)
        DAANC6(1,2,NL1) = DAANC6(1,2,NL1)+DAANC6L(1,2)
        DAANC6(1,3,NL1) = DAANC6(1,3,NL1)+DAANC6L(1,3)
        DAANC6(1,4,NL1) = DAANC6(1,4,NL1)+DAANC6L(1,4)
        DAANC6(1,5,NL1) = DAANC6(1,5,NL1)+DAANC6L(1,5)
        DAANC6(1,6,NL1) = DAANC6(1,6,NL1)+DAANC6L(1,6)
        DAANC6(2,1,NL1) = DAANC6(2,1,NL1)+DAANC6L(2,1)
        DAANC6(2,2,NL1) = DAANC6(2,2,NL1)+DAANC6L(2,2)
        DAANC6(2,3,NL1) = DAANC6(2,3,NL1)+DAANC6L(2,3)
        DAANC6(2,4,NL1) = DAANC6(2,4,NL1)+DAANC6L(2,4)
        DAANC6(2,5,NL1) = DAANC6(2,5,NL1)+DAANC6L(2,5)
        DAANC6(2,6,NL1) = DAANC6(2,6,NL1)+DAANC6L(2,6)
        DAANC6(3,1,NL1) = DAANC6(3,1,NL1)+DAANC6L(3,1)
        DAANC6(3,2,NL1) = DAANC6(3,2,NL1)+DAANC6L(3,2)
        DAANC6(3,3,NL1) = DAANC6(3,3,NL1)+DAANC6L(3,3)
        DAANC6(3,4,NL1) = DAANC6(3,4,NL1)+DAANC6L(3,4)
        DAANC6(3,5,NL1) = DAANC6(3,5,NL1)+DAANC6L(3,5)
        DAANC6(3,6,NL1) = DAANC6(3,6,NL1)+DAANC6L(3,6)
        CALL GETDPDAANC(DAANC6L,BUFR(3+INC,I),IRESP,INC)
        DAANC6(1,1,NL2) = DAANC6(1,1,NL2)+DAANC6L(1,1)
        DAANC6(1,2,NL2) = DAANC6(1,2,NL2)+DAANC6L(1,2)
        DAANC6(1,3,NL2) = DAANC6(1,3,NL2)+DAANC6L(1,3)
        DAANC6(1,4,NL2) = DAANC6(1,4,NL2)+DAANC6L(1,4)
        DAANC6(1,5,NL2) = DAANC6(1,5,NL2)+DAANC6L(1,5)
        DAANC6(1,6,NL2) = DAANC6(1,6,NL2)+DAANC6L(1,6)
        DAANC6(2,1,NL2) = DAANC6(2,1,NL2)+DAANC6L(2,1)
        DAANC6(2,2,NL2) = DAANC6(2,2,NL2)+DAANC6L(2,2)
        DAANC6(2,3,NL2) = DAANC6(2,3,NL2)+DAANC6L(2,3)
        DAANC6(2,4,NL2) = DAANC6(2,4,NL2)+DAANC6L(2,4)
        DAANC6(2,5,NL2) = DAANC6(2,5,NL2)+DAANC6L(2,5)
        DAANC6(2,6,NL2) = DAANC6(2,6,NL2)+DAANC6L(2,6)
        DAANC6(3,1,NL2) = DAANC6(3,1,NL2)+DAANC6L(3,1)
        DAANC6(3,2,NL2) = DAANC6(3,2,NL2)+DAANC6L(3,2)
        DAANC6(3,3,NL2) = DAANC6(3,3,NL2)+DAANC6L(3,3)
        DAANC6(3,4,NL2) = DAANC6(3,4,NL2)+DAANC6L(3,4)
        DAANC6(3,5,NL2) = DAANC6(3,5,NL2)+DAANC6L(3,5)
        DAANC6(3,6,NL2) = DAANC6(3,6,NL2)+DAANC6L(3,6)
      END DO
C
      RETURN
      END
