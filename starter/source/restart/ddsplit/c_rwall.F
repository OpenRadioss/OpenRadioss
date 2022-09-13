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
Chd|  C_RWALL                       source/restart/ddsplit/c_rwall.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_RWALL(NPRW    ,LPRW   ,RWL     ,
     2                   CEP    ,PROC    ,NSLAW_L ,NESLAW_L,
     3                   LWSAV_L,NSKYRW_L)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NSLAW_L, NESLAW_L, LWSAV_L,
     .          NSKYRW_L, 
     .          LPRW(*), NPRW(*), 
     .          CEP(*)
       my_real
     .          RWL(NRWLP,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSL, I, K, KK, N, NN, N1, N2, N3, N4, IE, NE, ITY, ILW,
     .        NSL_L, IFQ, MSR
C
      NSLAW_L = 0
      NSKYRW_L= 0
      NESLAW_L= 0
      LWSAV_L = 0
      K = 0
      DO N = 1, NRWALL
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        NSL=NPRW(N)
        MSR = NPRW(N3)
        ITY=NPRW(N4)
        NSL_L=0
        DO KK = 1, NSL
          NN = LPRW(K+KK)
          IF(NLOCAL(NN,PROC+1)==1)THEN
            NSL_L = NSL_L + 1
          ENDIF
        ENDDO
        NSLAW_L = NSLAW_L + NSL_L
C espace memoire P/ON
        IF(IPARI0/=0.AND.MSR/=0) NSKYRW_L = NSKYRW_L + NSL_L
        K = K + NSL
C
        IFQ = NINT(RWL(15,N))
        IF(IFQ>0) THEN
          LWSAV_L = LWSAV_L + NSL_L
        ENDIF
C
        IF(ITY==-1)THEN
          NE = NINT(RWL(8,N))
          IF(NE/=0) THEN
            DO KK = 1, NE
              IE = LPRW(K+KK)/10
C             IR = LPRW(K+KK)-10*IE
              IF(CEP(IE)==PROC)THEN
                NESLAW_L = NESLAW_L+1
              ENDIF
            ENDDO
            K = K + NE
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
