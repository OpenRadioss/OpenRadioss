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
C
Chd|====================================================================
Chd|  W_IRBE2                       source/restart/ddsplit/w_irbe2.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_IRBE2(IRBE2   ,LRBE2  ,NODLOCAL ,PROC    ,
     .                   LLRBE2_L,ITABRBE2M,NBDDRBE2,NUMNOD_L,LEN_IA )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LLRBE2_L, LEN_IA,NBDDRBE2,NUMNOD_L,ITABRBE2M(*),
     .        IRBE2(NRBE2L,*), LRBE2(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KL, NSNL, NSN, ILP, NLL, NN, M, N,
     .        IBVTMP(NRBE2L,NRBE2), LBVTMP(LLRBE2_L),IAD,II

C-----------------------------------------------
C
      KL=0
      NSNL = 0
      II = 0
      DO I = 1, NRBE2
       K = IRBE2(1,I)
       M = IRBE2(3,I)
       NSN = IRBE2(5,I)
       IAD = NSNL
       IF (NSN > 0) II = II +1
       IF (NSN ==0) CYCLE
       DO J = 1, NSN
          N = LRBE2(K+J)
          IF (NLOCAL(N,PROC+1)==1)THEN
            NSNL = NSNL + 1
            LBVTMP(NSNL) = NODLOCAL(N)
          ENDIF
       ENDDO
       IF (NLOCAL(M,PROC+1)==1)THEN
        KL = KL + 1
        DO J = 1, NRBE2L
          IBVTMP(J,KL) = IRBE2(J,I)
        ENDDO
        IBVTMP(1,KL) = IAD
        IBVTMP(5,KL) = NSNL-IAD
        IBVTMP(3,KL) = NODLOCAL(M)
C SPMD issue global RBE2 Number
        IBVTMP(10,KL) = II
C----------IBVTMP(6,KL)-set in engine------------------------------------
       ENDIF
      ENDDO
C----------------
      CALL WRITE_I_C(IBVTMP,NRBE2L*KL)
      CALL WRITE_I_C(LBVTMP,LLRBE2_L)
      LEN_IA = LEN_IA + NRBE2L*KL + LLRBE2_L
C
      IF(NBDDRBE2>0) THEN
          ITABRBE2M(1:NUMNOD_L) = 0
          ILP=0
          DO K = 1, KL
            N = IBVTMP(3,K)
             IF(ITABRBE2M(N)==0) THEN
              ILP = ILP + 1
c              ITABRBE2M(N) = ILP
              ITABRBE2M(N) = N
            ENDIF
          END DO
      ENDIF
C
      RETURN
      END
