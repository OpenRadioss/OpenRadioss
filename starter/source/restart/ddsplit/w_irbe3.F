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
C
Chd|====================================================================
Chd|  W_IRBE3                       source/restart/ddsplit/w_irbe3.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_IRBE3(IRBE3   ,LRBE3  ,NODLOCAL ,PROC    ,
     .              LLRBE3_L,ITABRBE3M,NBDDRBE3M,NUMNOD_L,LEN_IA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LLRBE3_L, LEN_IA,ITABRBE3M(*),NBDDRBE3M,NUMNOD_L,
     .        IRBE3(NRBE3L,*), LRBE3(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KL, NSNL, NSN, ILP, NLL, NN, M, N,
     .        IBVTMP(NRBE3L,NRBE3), LBVTMP(2*LLRBE3_L),IAD
C
      KL=0
      NSNL = 0
      DO I = 1, NRBE3
       K = IRBE3(1,I)
       M = IRBE3(3,I)
       NSN = IRBE3(5,I)
       IAD = NSNL
       ILP = 0
       DO J = 1, NSN
          N = LRBE3(K+J)
          IF (NLOCAL(N,PROC+1)==1)THEN
            ILP = ILP + 1
            NSNL = NSNL + 1
            LBVTMP(NSNL) = NODLOCAL(N)
            LBVTMP(NSNL+LLRBE3_L) = LRBE3(K+J+SLRBE3/2)
          ENDIF
       ENDDO
       IF (NLOCAL(M,PROC+1)==1.OR.ILP>0)THEN
        KL = KL + 1
        DO J = 1, NRBE3L
          IBVTMP(J,KL) = IRBE3(J,I)
        ENDDO
        IBVTMP(1,KL) = IAD
        IBVTMP(5,KL) = NSNL-IAD
        IF (NLOCAL(M,PROC+1)==1.AND.IBVTMP(5,KL)>0)THEN
         IBVTMP(3,KL) = NODLOCAL(M)
        ELSE
         IBVTMP(3,KL) = 0
        ENDIF
C SPMD issue global RBE2 Number
        IBVTMP(10,KL) = I
       ENDIF
      ENDDO
C
      CALL WRITE_I_C(IBVTMP,NRBE3L*KL)
      CALL WRITE_I_C(LBVTMP,LLRBE3_L*2)
      LEN_IA = LEN_IA + NRBE3L*KL + LLRBE3_L*2
C
      IF(NBDDRBE3M>0) THEN
        DO I = 1, NUMNOD_L
          ITABRBE3M(I) = 0
        ENDDO
          ILP=0
          DO K = 1, NSNL
            N = LBVTMP(K)
             IF(ITABRBE3M(N)==0) THEN
              ILP = ILP + 1
              ITABRBE3M(N) = ILP
            ENDIF
          END DO
      ENDIF
C
      RETURN
      END
