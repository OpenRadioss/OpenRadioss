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
Chd|  W_RWALL                       source/restart/ddsplit/w_rwall.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_RWALL(NPRW   ,LPRW     ,RWL   ,
     +                   CEP     ,CEL     ,NODLOCAL,PROC  ,
     +                   NSLAW_L,NESLAW_L ,NUMNOD_L,LEN_IA)
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
       INTEGER  PROC, LEN_IA, NSLAW_L, NESLAW_L,  NUMNOD_L,
     .          LPRW(*), NPRW(*), 
     .          CEP(*), CEL(*), NODLOCAL(*)
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
     .        ILP, NSL_L, NE_L, IR, MSR
      INTEGER, DIMENSION(:),ALLOCATABLE :: LPRW_L, NPRW_L
C
      IF(NRWALL>0) THEN
        ALLOCATE(NPRW_L(NNPRW*NRWALL))
        DO N = 1, NRWALL
          NPRW_L(N) = 0
          DO KK = 1, NNPRW-1
            NPRW_L(KK*NRWALL+N) = NPRW(KK*NRWALL+N)
          ENDDO
          N3 = 2*NRWALL+N
          MSR = NPRW(N3)
          IF(MSR/=0) THEN
            IF(NLOCAL(MSR,PROC+1)==1) THEN
              NPRW_L(N3) = NODLOCAL(MSR)
            ELSE
C noeud non present sur le proc, MSR a 0 pour eviter les comms.
              NPRW_L(N3) = 0
            ENDIF
          ENDIF
        ENDDO
C
        IF(NSLAW_L+NESLAW_L>0) THEN
          ALLOCATE(LPRW_L(NSLAW_L+NESLAW_L))
          K = 0
          ILP = 0
          DO N = 1, NRWALL
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            NSL=NPRW(N)
            ITY=NPRW(N4)
            NSL_L = 0
            DO KK = 1, NSL
              NN = LPRW(K+KK)
              IF(NLOCAL(NN,PROC+1)==1)THEN
                NSL_L = NSL_L + 1
                LPRW_L(ILP+NSL_L) = NODLOCAL(NN)
              ENDIF
            ENDDO
            ILP = ILP + NSL_L
            K = K + NSL
            NPRW_L(N) = NSL_L
            IF(ITY==-1)THEN
              NE = NINT(RWL(8,N))
              NE_L = 0
              IF(NE/=0) THEN
                DO KK = 1, NE
                  IE = LPRW(K+KK)/10
                  IR = LPRW(K+KK)-10*IE
                  IF(CEP(IE)==PROC)THEN
                    NE_L = NE_L+1
                    LPRW_L(ILP+NE_L) = CEL(IE)*10+IR
                  ENDIF
                ENDDO
                ILP = ILP + NE_L
                K = K + NE
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C
        CALL WRITE_I_C(NPRW_L,NNPRW*NRWALL)
        CALL WRITE_I_C(LPRW_L,NSLAW_L+NESLAW_L)
        LEN_IA = LEN_IA + NNPRW*NRWALL + NSLAW_L+NESLAW_L
        IF(NSLAW_L+NESLAW_L>0)DEALLOCATE(LPRW_L)
        DEALLOCATE(NPRW_L)
C
      ENDIF
C
      RETURN
      END
