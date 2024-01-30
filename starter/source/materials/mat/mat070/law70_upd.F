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
Chd|====================================================================
Chd|  LAW70_UPD                     source/materials/mat/mat070/law70_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        LAW70_TABLE                   source/materials/mat/mat070/law70_table.F
Chd|        TABLE_SLOPE                   source/materials/tools/table_slope.F
Chd|        VW_SMOOTH                     source/tools/curve/vw_smooth.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAW70_UPD(MAT_PARAM,TITR     ,MAT_ID   ,NUPARAM  ,UPARAM   ,
     .                     NFUNC    ,IFUNC    ,NPC      ,PLD      ,IOUT     ,
     .                     NFUNCT   ,FUNC_ID  ,NPROPM   ,PM       )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD      
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: MAT_ID
      INTEGER ,INTENT(IN) :: NFUNC
      INTEGER ,INTENT(IN) :: NFUNCT
      INTEGER ,INTENT(IN) :: NUPARAM
      INTEGER ,INTENT(IN) :: IOUT
      INTEGER ,INTENT(IN) :: NPROPM
      INTEGER ,DIMENSION(NFUNC)  ,INTENT(IN) :: IFUNC
      INTEGER ,DIMENSION(NFUNCT) ,INTENT(IN) :: FUNC_ID
      INTEGER ,INTENT(IN)  :: NPC(*)
      my_real ,INTENT(IN)  :: PLD(*) 
      CHARACTER*nchartitle ,INTENT(IN):: TITR
      my_real ,DIMENSION(NPROPM)  ,INTENT(INOUT) :: PM
      my_real ,DIMENSION(NUPARAM) ,INTENT(INOUT) :: UPARAM
      TYPE (MATPARAM_STRUCT_)     ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,NDIM,NLOAD,NULOAD,NCURV,IPT,NPT,LMAX,FUNC_N,
     .           IC1,IC2,IZERO,IERROR,NTABLE,IS_ENCRYPTED,IFLAG0,IFLAG,ICHK
      INTEGER ,PARAMETER :: LOAD   = 1
      INTEGER ,PARAMETER :: UNLOAD = 2
      INTEGER ,PARAMETER :: NPTMAX = 100   ! max number of function input points
      my_real :: E0,EMAX,EPSMAX,STIFFMIN,STIFFMAX,STIFFINI,C1,G,NU,XMAX,
     .   S1,S2,T1,T2,X1,X2,Y1,Y2,DERI,YY,EPS0,EPST
      INTEGER ,DIMENSION(NFUNC) :: FUNC,PERM,LEN
      my_real ,DIMENSION(NFUNC) :: RATE,YFAC
      my_real ,DIMENSION(:)   ,ALLOCATABLE :: XF
      my_real ,DIMENSION(:,:) ,ALLOCATABLE :: XI,YI,YF
C=======================================================================
      NCURV  = INT(UPARAM(1))
      NLOAD  = INT(UPARAM(7))
      NULOAD = INT(UPARAM(8))
      E0     = UPARAM(2)
      EMAX   = UPARAM(2*NFUNC + 12)
      EPSMAX = UPARAM(4)
      NU     = UPARAM(6)
      IS_ENCRYPTED = NINT(UPARAM(2*NFUNC + 16))
      IF (NULOAD > 0) THEN
        NTABLE = 2
      ELSE
        NTABLE = 1
      END IF
      MAT_PARAM%NTABLE = NTABLE
      ALLOCATE (MAT_PARAM%TABLE(NTABLE))
      MAT_PARAM%TABLE(LOAD)%NOTABLE = LOAD
      IF (NTABLE == 2) MAT_PARAM%TABLE(UNLOAD)%NOTABLE = UNLOAD
c
c------------------------------------------------------      
c     Loading functions
c------------------------------------------------------      
      LMAX = 0
      DO I = 1,NLOAD
        FUNC_N = IFUNC(I)
        RATE(I) = UPARAM(I + 8)
        YFAC(I) = UPARAM(I + 8 + NFUNC)
        LEN(I)  = (NPC(FUNC_N+1) - NPC(FUNC_N)) / 2
        LMAX    = MAX(LMAX,LEN(I))
      END DO
      ALLOCATE (XI(LMAX,NLOAD))
      ALLOCATE (YI(LMAX,NLOAD))
c     build X,Y vectors from loading functions
c     skip negative function values (only positive part is taken into account and symmetrized)
c     check and enforce passing through (0,0)
      DO I = 1,NLOAD
        FUNC_N = IFUNC(I)
        IC1 = NPC(FUNC_N)
        IC2 = NPC(FUNC_N+1) - 2
        IPT = 0
        IZERO = 0
        S1 = PLD(IC1)
        DO J = IC1,IC2-2,2
          S1 = PLD(J)
          S2 = PLD(J+2)
          T1 = PLD(J+1) * YFAC(I)
          T2 = PLD(J+3) * YFAC(I)
          IF (S1 < ZERO .and. S2 < ZERO) CYCLE
          IF (J == IC1 .and. S1 > ZERO) THEN
            S1 = ZERO
            T1 = ZERO
            IZERO = 1
          ELSE IF (S1 <= ZERO .and. S2 > ZERO) THEN
            IF (T1 /= ZERO ) THEN
              S1 = ZERO
              T1 = ZERO
              IZERO = 1
            END IF             
          END IF             
          IPT = IPT + 1
          XI(IPT,I) = S1
          YI(IPT,I) = T1
        END DO
        IPT = IPT + 1
        XI(IPT,I) = S2
        YI(IPT,I) = T2
        LEN(I) = IPT
      END DO
c------------------------------------------------------      
c     interpolate if not passing through (0,0)
c------------------------------------------------------      
      DO I = 1,NLOAD
        DO J = 1,LEN(I)
          IF (XI(J,I) == ZERO .and. YI(J,I) /= ZERO) THEN
            YI(J,I) = ZERO
          ELSE IF (XI(J,I) == ZERO .and. XI(J,I) /= ZERO) THEN
            XI(J,I) = ZERO
          END IF
        END DO
      END DO
c     reduce number of points if necessary 
      DO I = 1,NLOAD
        IF (LEN(I) > NPTMAX) THEN
          CALL VW_SMOOTH(LEN(I),NPTMAX,XI(1:LEN(I),I),YI(1:LEN(I),I))
          LEN(I) = NPTMAX
        END IF
      END DO
c
c------------------------------------------------------      
c
      CALL LAW70_TABLE(
     .     MAT_PARAM%TABLE(LOAD)  ,NLOAD  ,LEN     ,LMAX   ,RATE  ,
     .     XI    ,YI     )      
      
      DEALLOCATE (YI)
      DEALLOCATE (XI)
c------------------------------------------------------      
c     Unloading functions -> tables
c------------------------------------------------------
      IF (NULOAD > 0) THEN
c       
        LMAX = 0
        DO I = 1,NULOAD
          K  = NLOAD + I
          FUNC(I) = IFUNC (K)
          RATE(I) = UPARAM(K + 8)
          YFAC(I) = UPARAM(K + 8 + NFUNC)
          LEN(I)  = (NPC(FUNC(I)+1) - NPC(FUNC(I))) / 2
          LMAX    = MAX(LMAX,LEN(I))
        END DO
        ALLOCATE (XI(LMAX,NULOAD))
        ALLOCATE (YI(LMAX,NULOAD))
c
        DO I = 1,NULOAD
          FUNC_N = FUNC(I)
          IC1 = NPC(FUNC_N)
          IC2 = NPC(FUNC_N+1) - 2
          IPT = 0
          IZERO = 0
          DO J = IC1,IC2-2,2
            S1 = PLD(J)
            S2 = PLD(J+2)
            T1 = PLD(J+1) * YFAC(I)
            T2 = PLD(J+3) * YFAC(I)
            IF (S1 < ZERO .and. S2 < ZERO) CYCLE
            IF (J == IC1 .and. S1 > ZERO) THEN
              S1 = ZERO
              T1 = ZERO
              IZERO = 1
            ELSE IF (S1 <= ZERO .and. S2 > ZERO) THEN
              IF (T1 /= ZERO ) THEN
                S1 = ZERO
                T1 = ZERO
                IZERO = 1
              END IF             
            END IF             
            IPT = IPT + 1
            XI(IPT,I) = S1
            YI(IPT,I) = T1
          END DO
          IPT = IPT + 1
          XI(IPT,I) = S2
          YI(IPT,I) = T2
          LEN(I) = IPT
        END DO
c------------------------------------------------------      
c     interpolate if not passing through (0,0)
c------------------------------------------------------      
        DO I = 1,NULOAD
          DO J = 1,LEN(I)
            IF (XI(J,I) == ZERO .and. YI(J,I) /= ZERO) THEN
              YI(J,I) = ZERO
            ELSE IF (XI(J,I) == ZERO .and. XI(J,I) /= ZERO) THEN
              XI(J,I) = ZERO
            END IF
          END DO
        END DO
c       reduce number of points if necessary 
        DO I = 1,NULOAD
          IF (LEN(I) > NPTMAX) THEN
            CALL VW_SMOOTH(LEN(I),NPTMAX,XI(1:LEN(I),I),YI(1:LEN(I),I))
            LEN(I) = NPTMAX
          END IF
        END DO
c--------------------------------------------------------
        CALL LAW70_TABLE(
     .       MAT_PARAM%TABLE(UNLOAD)  ,NULOAD  ,LEN     ,LMAX   ,RATE  ,
     .       XI    , YI     )      
      
        DEALLOCATE (YI)
        DEALLOCATE (XI)

      END IF
c--------------------------------------------------------
c     Automatic calculation of max function slope when Emax=0
c--------------------------------------------------------
      CALL TABLE_SLOPE(MAT_PARAM%TABLE(LOAD),STIFFINI,STIFFMIN,STIFFMAX,XMAX)
c
      IF (EMAX == ZERO) THEN
        EMAX = STIFFMAX
        UPARAM(3) = (EMAX - E0) / EPSMAX
        UPARAM(2*NFUNC + 12) = EMAX
c
        CALL ANCMSG(MSGID=1219, MSGTYPE=MSGINFO, ANMODE=ANINFO_BLIND_1,
     .              I1 = MAT_ID,
     .              C1 = TITR  ,
     .              R1 = EMAX  )
      END IF
c
      IFLAG  = 0
      IFLAG0 = 0
      IF (E0 < STIFFINI) THEN
        IF (EMAX < E0) EMAX = E0        
        IFLAG0 = 1
      END IF
c--------------------------------------------------------
c     Automatic modification of EPST and E0
c--------------------------------------------------------
      EPS0 = ONE
      EPST = ONE            
      X1   = MAT_PARAM%TABLE(LOAD)%X(1)%VALUES(1)
      NDIM = MAT_PARAM%TABLE(LOAD)%NDIM         
      NPT  = SIZE(MAT_PARAM%TABLE(LOAD)%X(1)%VALUES)
      DO K=1,NLOAD
        DO I = 1,NPT-1
          J  = I+1
          X2 = MAT_PARAM%TABLE(LOAD)%X(1)%VALUES(J)
          IF (NDIM == 1) THEN
            Y1 = MAT_PARAM%TABLE(LOAD)%Y1D(I)
            Y2 = MAT_PARAM%TABLE(LOAD)%Y1D(J)
          ELSE IF (NDIM == 2) THEN
            Y1 = MAT_PARAM%TABLE(LOAD)%Y2D(I,K)
            Y2 = MAT_PARAM%TABLE(LOAD)%Y2D(J,K)
          END IF
          DERI = (Y2 - Y1) / (X2 - X1)
          IF (DERI >= EMAX .and. X1 > ZERO) THEN
            EPS0 = MIN(EPS0, X1 )
            IFLAG = 1
            IF (X1 == EPS0) THEN
              EPST = MIN(EPST,ABS(EPS0 - Y1/EMAX))
            ENDIF
          ENDIF
          X1 = X2
         ENDDO
       ENDDO ! NLOAD
c
       IF (IFLAG == 1) THEN
           E0 = MIN(E0, EMAX)
           UPARAM(3) = (EMAX - E0) / EPST
           UPARAM(4) = EPS0
           CALL ANCMSG(MSGID=864, MSGTYPE=MSGINFO, ANMODE=ANINFO_BLIND_1,
     .                 I1=MAT_ID,
     .                 C1=TITR,
     .                 R1=EPS0)
       ENDIF
       IF (IFLAG0 == 1) THEN
           E0 = MIN(E0, EMAX)
           UPARAM(2) = E0
           UPARAM(3) = (EMAX - E0)/EPST
           CALL ANCMSG(MSGID=865, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                 I1=MAT_ID,
     .                 C1=TITR,
     .                 R1=E0)
       ENDIF
c------------------------------------
c     static function value at EPSMAX for engine
c------------------------------------
      K = 1
      DO WHILE (MAT_PARAM%TABLE(LOAD)%X(1)%VALUES(K) < EPSMAX .and. K < LEN(1)-1)
        K = K + 1
      END DO
      X1 = MAT_PARAM%TABLE(LOAD)%X(1)%VALUES(K-1)
      X2 = MAT_PARAM%TABLE(LOAD)%X(1)%VALUES(K)
      IF (MAT_PARAM%TABLE(LOAD)%NDIM == 1) THEN
        Y1 = MAT_PARAM%TABLE(LOAD)%Y1D(K-1)
        Y2 = MAT_PARAM%TABLE(LOAD)%Y1D(K)
      ELSE IF (MAT_PARAM%TABLE(LOAD)%NDIM == 2) THEN
        Y1 = MAT_PARAM%TABLE(LOAD)%Y2D(K-1,1)
        Y2 = MAT_PARAM%TABLE(LOAD)%Y2D(K,1)
      END IF
      DERI  = (Y2 - Y1) / (X2 - X1)
      UPARAM(2*NFUNC + 15) = Y1 + DERI * (EPSMAX - X1)
c------------------------------------
c     update stiffness values in PM
c------------------------------------
      G  = HALF *E0 / (ONE + NU)
      C1 = THIRD*E0 / (ONE - TWO*NU)
      UPARAM(5) = G
      PM(20) = E0
      PM(22) = G
      PM(24) = EMAX   ! E0/(ONE - NU**2)
      PM(32) = C1 
c------------------------------------
c     Output new tables definition
c------------------------------------
      IF (IS_ENCRYPTED == 0)THEN
        WRITE(IOUT,1000)     
        WRITE(IOUT,1001)
        NDIM = MAT_PARAM%TABLE(LOAD)%NDIM
        IF (NDIM == 1) THEN
          WRITE(IOUT,1101) FUNC_ID(IFUNC(1))
          DO J=1,SIZE(MAT_PARAM%TABLE(LOAD)%X(1)%VALUES)
            WRITE(IOUT,2000) MAT_PARAM%TABLE(LOAD)%X(1)%VALUES(J),
     .                       MAT_PARAM%TABLE(LOAD)%Y1D(J)
          END DO  
        ELSE
          DO I=1,NLOAD
            WRITE(IOUT,1102) FUNC_ID(IFUNC(I)),RATE(I)
            DO J=1,SIZE(MAT_PARAM%TABLE(LOAD)%X(1)%VALUES)
              WRITE(IOUT,2000) MAT_PARAM%TABLE(LOAD)%X(1)%VALUES(J),
     .                         MAT_PARAM%TABLE(LOAD)%Y2D(J,I)
            END DO  
          END DO  
        END IF
c
        IF (NULOAD == 1) THEN
          WRITE(IOUT,1002)
          WRITE(IOUT,1101) FUNC_ID(IFUNC(1+NLOAD))
          DO J=1,SIZE(MAT_PARAM%TABLE(UNLOAD)%X(1)%VALUES)
            WRITE(IOUT,2000) MAT_PARAM%TABLE(UNLOAD)%X(1)%VALUES(J),
     .                       MAT_PARAM%TABLE(UNLOAD)%Y1D(J)
          END DO  
        ELSE IF (NULOAD > 1) THEN
          WRITE(IOUT,1002)
          NDIM = MAT_PARAM%TABLE(UNLOAD)%NDIM
          IF (NDIM == 1) THEN
            WRITE(IOUT,1101) FUNC_ID(IFUNC(1+NLOAD))
            DO J=1,SIZE(MAT_PARAM%TABLE(UNLOAD)%X(1)%VALUES)
              WRITE(IOUT,2000) MAT_PARAM%TABLE(UNLOAD)%X(1)%VALUES(J),
     .                         MAT_PARAM%TABLE(UNLOAD)%Y1D(J)
            END DO  
          ELSE
            DO I=1,NULOAD
              WRITE(IOUT,1102) FUNC_ID(IFUNC(I+NLOAD)),RATE(I+NLOAD)
              DO J=1,SIZE(MAT_PARAM%TABLE(UNLOAD)%X(1)%VALUES)
                WRITE(IOUT,2000) MAT_PARAM%TABLE(UNLOAD)%X(1)%VALUES(J),
     .                           MAT_PARAM%TABLE(UNLOAD)%Y2D(J,I)
              END DO  
            END DO  
          END IF
        END IF
c
      END IF
c-----------
      RETURN
c-----------
 1000 FORMAT(/,'------------------------------------------',/,
     .         'MATERIAL LAW70 : UPDATE OF INPUT FUNCTIONS',/,
     .         '------------------------------------------',/)
 1001 FORMAT(5X,'LOADING :')
 1002 FORMAT(5X,'UNLOADING :')
 1101 FORMAT(5X,/,'YIELD STRESS FUNCTION=',I10,
     .       5X,/'      X,              Y')
 1102 FORMAT(5X,/,'YIELD STRESS FUNCTION=',I10,
     .       5X,'STRAIN RATE = ',1PG20.13,/,
     .       5X,'      X,              Y')
 2000 FORMAT(2G16.9)
c------------------------------------
      END
