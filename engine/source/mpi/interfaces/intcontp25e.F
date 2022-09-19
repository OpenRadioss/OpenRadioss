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
Chd|  INTCONTP25E                   source/mpi/interfaces/intcontp25e.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTCONTP25E(N,ISKY,NSNFI,ISIZENV,NSNFITOT,LEN)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "assert.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, LEN,
     .        NSNFI(*), ISKY(*), ISIZENV(2,*), NSNFITOT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG, P, I, LASTIG
      INTEGER LOCAL_ID
      INTEGER NSNFI_TOT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      NSNFI_TOT = 0
      DO P = 1,NSPMD
        NSNFI_TOT = NSNFI(P) + NSNFI_TOT
      ENDDO
      IF(N > 0) THEN
        ASSERT(NSNFI_TOT > 0)
        DO I = 1, N
          P = 1
          LOCAL_ID = ISKY(I)
          ASSERT(ISKY(I) > 0)
          ASSERT(ISKY(I) <= NSNFI_TOT)
          DO WHILE( NSNFI(P) < LOCAL_ID .AND. LOCAL_ID > 0)
            LOCAL_ID = LOCAL_ID - NSNFI(P)
            P = P + 1
          ENDDO
          ASSERT(P <= NSPMD)
          ASSERT(LOCAL_ID >= 0)
          IF(P <= NSPMD) THEN
            ISIZENV(1,P) = ISIZENV(1,P) + LEN
            ISIZENV(2,P) = ISIZENV(2,P) + 1
          ELSE
            ASSERT(.FALSE.)
          ENDIF
        ENDDO
      ENDIF

      DO P = 1, NSPMD
        NSNFITOT(P) = NSNFITOT(P) + NSNFI(P)
      END DO
C
      RETURN
      END

