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
C
!||====================================================================
!||    w_rwar     ../starter/source/restart/ddsplit/w_rwar.F
!||--- called by ------------------------------------------------------
!||    ddsplit    ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    nlocal     ../starter/source/spmd/node/ddtools.F
!||====================================================================
      SUBROUTINE W_RWAR(NPRW    ,LPRW ,RWL, RWSAV,
     +                  LWSAV_L ,LRW_PEN_L,NRW_PEN_L,RWSTIF_PEN,
     +                  SLN_PEN , CEP  ,PROC,LEN_AM)
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
       INTEGER  PROC, LEN_AM, LWSAV_L, LPRW(*), NPRW(*),
     .          CEP(*)
       INTEGER,  INTENT(IN   ) :: LRW_PEN_L, NRW_PEN_L, SLN_PEN
       my_real
     .          RWL(NRWLP,*), RWSAV(*)
      my_real, DIMENSION(SLN_PEN), INTENT(IN   )  :: RWSTIF_PEN
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J, K, KK, N, NN, IE, NE, IFQ, N4,
     .        ILP, NSL, NSL_L, KSAV, KSAV_L,N_P,IPEN,LSPEN
      my_real    , DIMENSION(:),ALLOCATABLE :: RWSAV_L
      my_real    , DIMENSION(:,:),ALLOCATABLE :: RWL_L
      my_real    , DIMENSION(:),ALLOCATABLE :: RWPEN1_L,RW_PEN_L
C
      IF(NRWALL>0) THEN
        ALLOCATE(RWL_L(NRWLP,NRWALL))
        IF(LWSAV_L>0) ALLOCATE(RWSAV_L(3*LWSAV_L))
        IF(LRW_PEN_L>0) THEN 
          ALLOCATE(RWPEN1_L(NRW_PEN_L))
          ALLOCATE(RW_PEN_L(4*LRW_PEN_L))
        ENDIF
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
          IF(IFQ>0) THEN
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
          IF(NPRW(N4)==-1) THEN
C RW ALE TH
           NE = NINT(RWL(8,N))
           IF(NE>0)THEN
            ILP = 0
            DO J = 1, NE
              IE = LPRW(K+J)/10
              IF(CEP(IE)==PROC) THEN
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
        IF(LWSAV_L>0) THEN
          CALL WRITE_DB(RWSAV_L,LWSAV_L*3)
          LEN_AM = LEN_AM + LWSAV_L*3
          DEALLOCATE(RWSAV_L)
        ENDIF
        IF(LRW_PEN_L>0) THEN
          RW_PEN_L = ZERO
          N_P = 0
          NSL_L = 0
          K = 0
          DO N = 1, NRWALL
            NSL = NPRW(N) 
            IPEN = NPRW(N+8*NRWALL)
            IF (IPEN>0) THEN
              DO J = 1, NSL
                 NN = LPRW(K+J)
                 IF(NLOCAL(NN,PROC+1)==1)THEN            
                   NSL_L = NSL_L + 1
                   RW_PEN_L(NSL_L) = RWSTIF_PEN(N_P+J)
                 ENDIF
              END DO 
              N_P = N_P + NSL
            END IF 
            K = K + NSL
          END DO 
          LSPEN = N_P
          RWPEN1_L = ZERO
          N_P = 0
          K = 0
          DO N = 1, NRWALL
            NSL = NPRW(N) 
            IPEN = NPRW(N+8*NRWALL)
            IF (IPEN>0) THEN
              NSL_L = 0
              DO J = 1, NSL
                 NN = LPRW(K+J)
                 IF(NLOCAL(NN,PROC+1)==1) NSL_L = NSL_L + 1
              END DO 
              IF (NSL_L>0) THEN
                N_P = N_P + 1
                RWPEN1_L(N_P) = RWSTIF_PEN(N_P+LSPEN) 
              END IF 
            END IF 
            K = K + NSL
          END DO 
          CALL WRITE_DB(RWPEN1_L,NRW_PEN_L)
          CALL WRITE_DB(RW_PEN_L,LRW_PEN_L*4)
          LEN_AM = LEN_AM + NRW_PEN_L + LRW_PEN_L*4
          DEALLOCATE(RW_PEN_L) 
          DEALLOCATE(RWPEN1_L) 
        END IF
      ENDIF
C
      RETURN
      END
