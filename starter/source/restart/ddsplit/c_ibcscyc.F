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
!||    c_ibcscyc   ../starter/source/restart/ddsplit/c_ibcscyc.F
!||--- called by ------------------------------------------------------
!||    ddsplit     ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    nlocal      ../starter/source/spmd/node/ddtools.F
!||====================================================================
      SUBROUTINE C_IBCSCYC(IBCSCYC,LBCSCYC,PROC  ,NBCSCYC_L,LLCYC_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCSCYC(4,*),LBCSCYC(2,*),PROC ,NBCSCYC_L,LLCYC_L
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NSN, ILP, N,P,M,NS,PROC1,N1,N2,LLCYC_L0

C-----------------------------------------------
      NBCSCYC_L = 0
C
      PROC1 = PROC + 1
      LLCYC_L = 0
      DO I = 1, NBCSCYC
        NSN = IBCSCYC(3,I)
        K = IBCSCYC(1,I)
        LLCYC_L0 = LLCYC_L
        DO J = 1, NSN
         N1 = LBCSCYC(1,K+J)
         N2 = LBCSCYC(2,K+J)
         IF(NLOCAL(N1,PROC1)==1.AND.NLOCAL(N2,PROC1)==1) LLCYC_L = LLCYC_L + 1
        ENDDO
C
        IF (LLCYC_L>LLCYC_L0) NBCSCYC_L = NBCSCYC_L+1
      ENDDO
C      print *,'NBCSCYC_L,LLCYC_L,proc=',NBCSCYC_L,LLCYC_L,proc1
C --------------------------------
      RETURN
      END
