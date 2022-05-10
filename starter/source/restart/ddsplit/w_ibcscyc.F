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
Chd|  W_IBCSCYC                     source/restart/ddsplit/w_ibcscyc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_IBCSCYC(IBCSCYC,LBCSCYC,NODLOCAL ,PROC    ,LLCYC_L,
     .                     NBCSCYC_L,LEN_IA )
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
      INTEGER IBCSCYC(4,*),LBCSCYC(2,*),NODLOCAL(*) ,PROC    ,LLCYC_L,
     .         NBCSCYC_L,LEN_IA
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KL, NSNL, NSN, ILP, NLL, NN, M, N,PROC1,
     .        IBVTMP(4,NBCSCYC_L), LBVTMP(2,LLCYC_L),IAD,N1,N2
C-----------------------------------------------
C
      PROC1 = PROC + 1
      KL=0
      NSNL = 0
      DO I = 1, NBCSCYC
       K = IBCSCYC(1,I)
       NSN = IBCSCYC(3,I)
       IAD = NSNL
       DO J = 1, NSN
          N1 = LBCSCYC(1,K+J)
          N2 = LBCSCYC(2,K+J)
          IF (NLOCAL(N1,PROC1)==1.AND.NLOCAL(N2,PROC1)==1)THEN
            NSNL = NSNL + 1
            LBVTMP(1,NSNL) = NODLOCAL(N1)
            LBVTMP(2,NSNL) = NODLOCAL(N2)
          ENDIF
       ENDDO
       IF (NSNL>IAD) THEN
        KL = KL + 1
        DO J = 1, 4
          IBVTMP(J,KL) = IBCSCYC(J,I)
        ENDDO
        IBVTMP(1,KL) = IAD
        IBVTMP(3,KL) = NSNL-IAD
       ENDIF
      ENDDO
C----------------
      CALL WRITE_I_C(IBVTMP,4*KL)
      CALL WRITE_I_C(LBVTMP,2*NSNL)
      LEN_IA = LEN_IA + 4*KL + 2*NSNL
      RETURN
      END
