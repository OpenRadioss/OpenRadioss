Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C
Chd|====================================================================
Chd|  W_FRBE3                       source/restart/ddsplit/w_frbe3.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_FRBE3(IRBE3   ,LRBE3  ,PROC    ,LLRBE3_L,
     .                   FRBE3   ,FRBE3AD,LEN_IA  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LLRBE3_L, LEN_IA,
     .        IRBE3(NRBE3L,*), LRBE3(*)
      my_real
     .        FRBE3(6,*),FRBE3AD(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KL, NSNL, NSN, ILP, NN, M, N,IAD,IADS
      my_real
     .        FBTMP1(6,LLRBE3_L),FBTMP2(2*LLRBE3_L)
C
      NSNL = 0
      IADS=SLRBE3/2
      DO I = 1, NRBE3
       K = IRBE3(1,I)
       NSN = IRBE3(5,I)
       DO J = 1, NSN
          N = LRBE3(K+J)
          IF (NLOCAL(N,PROC+1)==1)THEN
            NSNL = NSNL + 1
            DO NN =1,6
             FBTMP1(NN,NSNL) = FRBE3(NN,K+J)
            ENDDO
            FBTMP2(NSNL) = FRBE3AD(K+J)
            FBTMP2(NSNL+LLRBE3_L) = FRBE3AD(K+J+IADS)
          ENDIF
       ENDDO
      ENDDO
C
      CALL WRITE_DB(FBTMP1,LLRBE3_L*6)
      CALL WRITE_DB(FBTMP2,LLRBE3_L*2)
      LEN_IA = LEN_IA + LLRBE3_L*8
C
      RETURN
      END
