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
Chd|  W_RWAR                        source/restart/ddsplit/w_rwar.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_RWAR(NPRW    ,LPRW ,RWL, RWSAV,
     +                  LWSAV_L ,CEP, PROC,LEN_AM)
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
       INTEGER  PROC, LEN_AM, LWSAV_L, LPRW(*), NPRW(*),
     .          CEP(*)
       my_real
     .          RWL(NRWLP,*), RWSAV(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KK, N, NN, IE, NE, ITY, IFQ, N4,
     .        ILP, NSL, NSL_L, KSAV, KSAV_L
      my_real    , DIMENSION(:),ALLOCATABLE :: RWSAV_L
      my_real    , DIMENSION(:,:),ALLOCATABLE :: RWL_L
C
      IF(NRWALL.GT.0) THEN
        ALLOCATE(RWL_L(NRWLP,NRWALL))
        IF(LWSAV_L.GT.0) ALLOCATE(RWSAV_L(3*LWSAV_L))
        K = 0
        KSAV = 0
        KSAV_L = 0
        DO N = 1, NRWALL
          DO J = 1, NRWLP
            RWL_L(J,N) = RWL(J,N)
          ENDDO
C
          NSL = NPRW(N)
C
          IFQ = NINT(RWL(15,N))
          IF(IFQ.GT.0) THEN
            NSL_L = 0
            DO KK = 1, NSL
              NN = LPRW(K+KK)
              IF(NLOCAL(NN,PROC+1)==1)THEN            
                RWSAV_L(KSAV_L+3*NSL_L+1) = RWSAV(KSAV+3*(KK-1)+1)
                RWSAV_L(KSAV_L+3*NSL_L+2) = RWSAV(KSAV+3*(KK-1)+2)
                RWSAV_L(KSAV_L+3*NSL_L+3) = RWSAV(KSAV+3*(KK-1)+3)
                NSL_L = NSL_L + 1
              ENDIF
            ENDDO
            KSAV = KSAV + 3*NSL
            KSAV_L = KSAV_L + 3*NSL_L
          ENDIF
C
          K = K + NSL
C
          N4 = N + 3*NRWALL
          IF(NPRW(N4).EQ.-1) THEN
C RW ALE TH
           NE = NINT(RWL(8,N))
           IF(NE.GT.0)THEN
            ILP = 0
            DO J = 1, NE
              IE = LPRW(K+J)/10
              IF(CEP(IE).EQ.PROC) THEN
                ILP = ILP + 1
              ENDIF
            ENDDO
            RWL_L(8,N) = ILP
            K = K + NE
           ENDIF
          ENDIF
        ENDDO
C
        CALL WRITE_DB(RWL_L,NRWLP*NRWALL)
        LEN_AM = LEN_AM + NRWLP*NRWALL
        DEALLOCATE(RWL_L)
        IF(LWSAV_L.GT.0) THEN
          CALL WRITE_DB(RWSAV_L,LWSAV_L*3)
          LEN_AM = LEN_AM + LWSAV_L*3
          DEALLOCATE(RWSAV_L)
        ENDIF
      ENDIF
C
      RETURN
      END
