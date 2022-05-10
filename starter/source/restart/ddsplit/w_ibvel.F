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
Chd|  W_IBVEL                       source/restart/ddsplit/w_ibvel.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_IBVEL(
     .  IBVEL,LBVEL,NODLOCAL,PROC,LLBVEL_L,LEN_IA)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LLBVEL_L, LEN_IA,
     .        IBVEL(NBVELP,*), LBVEL(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KL, NSNL, NSN, ILP, NLL, NN, M, N,
     .        IBVTMP(NBVELP,NIBVEL), LBVTMP(LLBVEL_L)
C
      K = 0
      KL = 0
      DO I = 1, NIBVEL
        NSN = IBVEL(3,I)
        DO J = 1, NBVELP
          IBVTMP(J,I) = IBVEL(J,I)
        ENDDO
        NSNL = 0
        DO J = 1, NSN
          N = LBVEL(K+J)
          IF (NLOCAL(N,PROC+1)==1)THEN
            NSNL = NSNL + 1
            LBVTMP(KL+NSNL) = NODLOCAL(N)
          ENDIF
        ENDDO
        IBVTMP(3,I) = NSNL
        M = IBVEL(4,I)
Coptimisation pour ne pas traiter les rb mous sans master et sans noeud slave
        IF(NLOCAL(M,PROC+1)==0.AND.NSNL.EQ.0)THEN
          IBVTMP(4,I) = -M
        ELSE
          IBVTMP(4,I) = NODLOCAL(M)
        ENDIF
        KL = KL+NSNL
        K = K + NSN
      ENDDO
C
      CALL WRITE_I_C(IBVTMP,NBVELP*NIBVEL)
      CALL WRITE_I_C(LBVTMP,LLBVEL_L)
      LEN_IA = LEN_IA + NBVELP*NIBVEL + LLBVEL_L
C
      RETURN
      END
