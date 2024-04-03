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
Chd|  ALEW7                         source/ale/grid/alew7.F       
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        VALPVEC                       source/materials/mat/mat033/sigeps33.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE ALEW7(
     1   X       ,V       ,W       ,MS      , NALE,
     2   NODFT   ,NODLT   ,WEIGHT  ,NUMNOD  , DT1 ,
     3   SX      ,SV      ,SW      ,NSPMD   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Compute Grid velocities for /ALE/GRID/MASSFLOW
C
C  INPUT/OUTPUT
C     X,D,V are allocated to SX,SD,DV=3*(NUMNOD_L+NUMVVOIS_L)
C      in grid subroutine it may needed to access nodes which
C      are connected to a remote elem. They are sored in X(1:3,NUMNOD+1:)
C      Consequently X is defined here X(3,SX/3) instead of X(3,NUMNOD) as usually
C    WEIGHT is a tag (0 or 1) which ensure a unique contribution of a given nodal value when sum is calculated
C      over all SPMD domain. (otherwise nodes at common boundary with another domain will have multiple contributions)
C
C  A AVERAGED VALUE IS COMPUTED IN THE MASS FLOW : VMEAN(1:2)
C  THIS VALUE IS APPLIED TO THE GRID VELOCITY
C     SUM_MOM(1) : is Sum(m[i]*vx[i], i=1..numnod)
C     SUM_MOM(2) : is Sum(m[i]*vy[i], i=1..numnod)
C     SUM_MOM(3) : is Sum(m[i]*vz[i], i=1..numnod)
C     SUM_MOM(4) : is Sum(m[i]      , i=1..numnod)
C
C     VMEAN(1) = SUM_MOM(1) / SUM_MOM(4)
C     VMEAN(2) = SUM_MOM(1) / SUM_MOM(4)
C     VMEAN(3) = SUM_MOM(1) / SUM_MOM(4)
C
C     SMP :
C       SUM_MOM(1:4) are the cumulative from i=NODFT,NODLT
C       SUM_MOM_L(1:4) is then the cumulative result from i=1,NUMNOD
C
C     SPMD :
C       SUM_MOM_L(1:4) is calculated on each domain
C                      then exchanged
C                      finally each domain can deduce the complete cumulative result
C
C     PARITH/ON :
C        subroutine FOAT_TO_6_FLOAT is used for this purpose
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
! SPMD CASE : SX >= 3*NUMNOD    (SX = 3*(NUMNOD_L+NRCVVOIS_L))
! X(1:3,1:NUMNOD) : local nodes
!  (1:3, NUMNOD+1:) additional nodes (also on adjacent domains but connected to the boundary of the current domain)
!      idem with D(SD), and V(SV)
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NSPMD,NUMNOD,SX,SV,SW
      INTEGER, INTENT(IN) :: NALE(NUMNOD), NODFT, NODLT
      INTEGER, INTENT(IN) :: WEIGHT(NUMNOD)
      my_real, INTENT(INout) :: X(3,SX/3)
      my_real, INTENT(IN) :: V(3,SV/3), MS(NUMNOD)
      my_real, INTENT(INOUT) :: W(3,SW/3)
      my_real, INTENT(IN) :: DT1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II,JJ
      INTEGER :: IDEFORMATION,IROTATION
      my_real :: SUM_MS,MS_NODE, ksi
      my_real :: SUM_MOM(3),SUM_COG(3) !1:m*vx, 2:m*vy, 3:m*vz
      my_real :: SUM_M, SUM_VOL
      my_real :: SUM_ITM(6)
      my_real :: VMEAN(3),LD(6),LW(3),COG(3),ITM(6),LD_B(3,3),LD_Bp(3,3),LD_Bp_B(3,3),LD_TOT_B(3,3)
      my_real :: P_B_Bp(3,3)
      my_real :: EIGENVAL(3),EIGENVEC(3,3)
      my_real :: X_MIN_MAX(6),X_MIN_MAX_GRID(6)
      my_real :: R11,R12,R13,  R21,R22,R23,  R31,R32,R33 ! transition matrix
      my_real :: X_TRANS(3) ! vector after transition matrix
      my_real :: XX,YY,ZZ
      my_real :: LD_NORM
      my_real :: SCALE_DEF,SCALE_ROT
      my_real :: RATIO(3), LAMBDA(6), DW(3)
      my_real :: BETA(6),BETA0(6)
      my_real :: FAC(3)
      my_real :: MS_ELEM_MEAN
      LOGICAL :: lTENSOR, lDEF,lROT
      LOGICAL :: HAS_ALE_NODE, HAS_FLOW_NODE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !1.---user parameters
      !-----------------------------------
      lTENSOR = .FALSE.
      lDEF=.FALSE.
      lROT=.FALSE.
      SCALE_DEF = ALE%GRID%ALPHA !user parameter
      SCALE_ROT = ALE%GRID%GAMMA !user parameter
      IF(SCALE_DEF > EM20)lDEF=.TRUE. !enable mesh deformation
      IF(SCALE_ROT > EM20)lROT=.TRUE. !enalbe mesh rotation
      IF(lDEF .OR. lROT)lTENSOR=.TRUE.!request for mean tensor calculation

      !2.---AVERAGED DEFORMATION TENSOR
      !-----------------------------------
      !mean value is computed here
      !  cumulative mass value done in sfor3.F for 3d solid and qforc2.F for quads
      !  ALE%GRID%MASSFLOW_DATA%.. is a shared data strucutre for SMP  (omp single or lock on/off)
!$OMP SINGLE
      IF(lTENSOR)THEN
         IF(NSPMD > 1)THEN
           CALL SPMD_EXCH_MASSFLOW_DATA(ALE%GRID%MASSFLOW_DATA, NSPMD)
         ENDIF
         ALE%GRID%MASSFLOW_DATA%EP(1:9) = ALE%GRID%MASSFLOW_DATA%EP(1:9) / ALE%GRID%MASSFLOW_DATA%SUM_M
         !STRAIN RATE TENSOR
         ALE%GRID%MASSFLOW_DATA%LD(1) = ALE%GRID%MASSFLOW_DATA%EP(1) !xx
         ALE%GRID%MASSFLOW_DATA%LD(2) = ALE%GRID%MASSFLOW_DATA%EP(2) !yy
         ALE%GRID%MASSFLOW_DATA%LD(3) = ALE%GRID%MASSFLOW_DATA%EP(3) !zz
         ALE%GRID%MASSFLOW_DATA%LD(4) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(4)+ALE%GRID%MASSFLOW_DATA%EP(7)) !xy
         ALE%GRID%MASSFLOW_DATA%LD(5) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(5)+ALE%GRID%MASSFLOW_DATA%EP(8)) !xz
         ALE%GRID%MASSFLOW_DATA%LD(6) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(6)+ALE%GRID%MASSFLOW_DATA%EP(9)) !yz
         !SPIN TENSOR
         ALE%GRID%MASSFLOW_DATA%LW(1) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(4)-ALE%GRID%MASSFLOW_DATA%EP(7))
         ALE%GRID%MASSFLOW_DATA%LW(2) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(5)-ALE%GRID%MASSFLOW_DATA%EP(8))
         ALE%GRID%MASSFLOW_DATA%LW(3) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(6)-ALE%GRID%MASSFLOW_DATA%EP(9))
      ENDIF
!$OMP END SINGLE


      !4.---MEAN VELOCITY & CENTER OF MASS
      !-----------------------------------
      ALE%GRID%MASSFLOW_DATA%MOM_L(1:3) = ZERO
      ALE%GRID%MASSFLOW_DATA%COG_L(1:3) = ZERO
      ALE%GRID%MASSFLOW_DATA%SUM_M = ZERO !used for elem mass in sforc3, used below for nodal mass
      CALL MY_BARRIER
      SUM_MOM(1:3) = ZERO
      SUM_COG(1:3) = ZERO
      SUM_M = ZERO
      DO I = NODFT, NODLT
        IF(IABS(NALE(I)) == 1)THEN
          MS_NODE = MS(I)*WEIGHT(I) !WEIGHT(1:NUMNOD) ENSURE A SINGLE CONTRIBUTION FROM ALL DOMAIN
          SUM_MOM(1) = SUM_MOM(1) + MS_NODE*V(1,I)
          SUM_MOM(2) = SUM_MOM(2) + MS_NODE*V(2,I)
          SUM_MOM(3) = SUM_MOM(3) + MS_NODE*V(3,I)
          SUM_M = SUM_M + MS_NODE
          SUM_COG(1) = SUM_COG(1) + MS_NODE*X(1,I)
          SUM_COG(2) = SUM_COG(2) + MS_NODE*X(2,I)
          SUM_COG(3) = SUM_COG(3) + MS_NODE*X(3,I)
        ENDIF
      ENDDO
#include "lockon.inc"
      !assembly of global values for SMP case
      ALE%GRID%MASSFLOW_DATA%MOM_L(1) = ALE%GRID%MASSFLOW_DATA%MOM_L(1) + SUM_MOM(1)
      ALE%GRID%MASSFLOW_DATA%MOM_L(2) = ALE%GRID%MASSFLOW_DATA%MOM_L(2) + SUM_MOM(2)
      ALE%GRID%MASSFLOW_DATA%MOM_L(3) = ALE%GRID%MASSFLOW_DATA%MOM_L(3) + SUM_MOM(3)
      ALE%GRID%MASSFLOW_DATA%SUM_M = ALE%GRID%MASSFLOW_DATA%SUM_M + SUM_M
      ALE%GRID%MASSFLOW_DATA%COG_L(1) = ALE%GRID%MASSFLOW_DATA%COG_L(1) + SUM_COG(1)
      ALE%GRID%MASSFLOW_DATA%COG_L(2) = ALE%GRID%MASSFLOW_DATA%COG_L(2) + SUM_COG(2)
      ALE%GRID%MASSFLOW_DATA%COG_L(3) = ALE%GRID%MASSFLOW_DATA%COG_L(3) + SUM_COG(3)
#include "lockoff.inc"
      CALL MY_BARRIER
      !SPMD EXCHANGE 'MOM' and 'COG'
      IF(NSPMD > 1)THEN
         CALL SPMD_EXCH_MASSFLOW_DATA2(ALE%GRID%MASSFLOW_DATA, NSPMD)
      ENDIF
      VMEAN(1:3) = ZERO
      SUM_MS = ALE%GRID%MASSFLOW_DATA%SUM_M
      IF(SUM_MS > EM20)THEN
        VMEAN(1) = ALE%GRID%MASSFLOW_DATA%MOM_L(1)/SUM_MS
        VMEAN(2) = ALE%GRID%MASSFLOW_DATA%MOM_L(2)/SUM_MS
        VMEAN(3) = ALE%GRID%MASSFLOW_DATA%MOM_L(3)/SUM_MS
        COG(1) = ALE%GRID%MASSFLOW_DATA%COG_L(1)/SUM_MS
        COG(2) = ALE%GRID%MASSFLOW_DATA%COG_L(2)/SUM_MS
        COG(3) = ALE%GRID%MASSFLOW_DATA%COG_L(3)/SUM_MS
      END IF


      !5.---INERTIA TENSOR MATRIX (ITM)
      !-----------------------------------
      ALE%GRID%MASSFLOW_DATA%ITM_L(6) = ZERO
      CALL MY_BARRIER
      SUM_ITM(1:6) = ZERO
      DO I = NODFT, NODLT
        IF(IABS(NALE(I)) == 1)THEN
          XX = X(1,I)-COG(1)
          YY = X(2,I)-COG(2)
          ZZ = X(3,I)-COG(3)
          MS_NODE = MS(I)*WEIGHT(I) !WEIGHT(1:NUMNOD) ENSURE A SINGLE CONTRIBUTION FROM ALL DOMAIN
          SUM_ITM(1) = SUM_ITM(1) + MS_NODE*(YY*YY+ZZ*ZZ)
          SUM_ITM(2) = SUM_ITM(2) + MS_NODE*(XX*XX+ZZ*ZZ)
          SUM_ITM(3) = SUM_ITM(3) + MS_NODE*(XX*XX+YY*YY)
          SUM_ITM(4) = SUM_ITM(4) - MS_NODE*(XX*YY)
          SUM_ITM(5) = SUM_ITM(5) - MS_NODE*(YY*ZZ)
          SUM_ITM(6) = SUM_ITM(6) - MS_NODE*(XX*ZZ)
        ENDIF
      ENDDO
#include "lockon.inc"
      !assembly of global values for SMP case
      ALE%GRID%MASSFLOW_DATA%ITM_L(1) = ALE%GRID%MASSFLOW_DATA%ITM_L(1) + SUM_ITM(1)
      ALE%GRID%MASSFLOW_DATA%ITM_L(2) = ALE%GRID%MASSFLOW_DATA%ITM_L(2) + SUM_ITM(2)
      ALE%GRID%MASSFLOW_DATA%ITM_L(3) = ALE%GRID%MASSFLOW_DATA%ITM_L(3) + SUM_ITM(3)
      ALE%GRID%MASSFLOW_DATA%ITM_L(4) = ALE%GRID%MASSFLOW_DATA%ITM_L(4) + SUM_ITM(4)
      ALE%GRID%MASSFLOW_DATA%ITM_L(5) = ALE%GRID%MASSFLOW_DATA%ITM_L(5) + SUM_ITM(5)
      ALE%GRID%MASSFLOW_DATA%ITM_L(6) = ALE%GRID%MASSFLOW_DATA%ITM_L(6) + SUM_ITM(6)
#include "lockoff.inc"
      CALL MY_BARRIER
      !SPMD EXCHANGE 'ITM'
      IF(NSPMD > 1)THEN
         CALL SPMD_EXCH_MASSFLOW_DATA3(ALE%GRID%MASSFLOW_DATA, NSPMD)
      ENDIF
      SUM_MS = ALE%GRID%MASSFLOW_DATA%SUM_M
      IF(SUM_MS > EM20)THEN
        ITM(1) = ALE%GRID%MASSFLOW_DATA%ITM_L(1)/SUM_MS
        ITM(2) = ALE%GRID%MASSFLOW_DATA%ITM_L(2)/SUM_MS
        ITM(3) = ALE%GRID%MASSFLOW_DATA%ITM_L(3)/SUM_MS
        ITM(4) = ALE%GRID%MASSFLOW_DATA%ITM_L(4)/SUM_MS
        ITM(5) = ALE%GRID%MASSFLOW_DATA%ITM_L(5)/SUM_MS
        ITM(6) = ALE%GRID%MASSFLOW_DATA%ITM_L(6)/SUM_MS
      END IF


      !6.---EIGENVECTORS - INERTIA TENSOR
      !-----------------------------------
      CALL VALPVEC(ITM,EIGENVAL,EIGENVEC,1)
      !-----------------------------------
      IF(DT1 == ZERO)THEN
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,1)=EIGENVEC(1:3,1)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,2)=EIGENVEC(1:3,2)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,3)=EIGENVEC(1:3,3)
      ENDIF
      EIGENVEC(1:3,1) = ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,1)
      EIGENVEC(1:3,2) = ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,2)
      EIGENVEC(1:3,3) = ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,3)


      !7.---TRANSITION MATRIX (transposed)
      !-----------------------------------
      R11=ALE%GRID%MASSFLOW_DATA%EIGENVEC(1,1)
      R21=ALE%GRID%MASSFLOW_DATA%EIGENVEC(1,2)
      R31=ALE%GRID%MASSFLOW_DATA%EIGENVEC(1,3)

      R12=ALE%GRID%MASSFLOW_DATA%EIGENVEC(2,1)
      R22=ALE%GRID%MASSFLOW_DATA%EIGENVEC(2,2)
      R32=ALE%GRID%MASSFLOW_DATA%EIGENVEC(2,3)

      R13=ALE%GRID%MASSFLOW_DATA%EIGENVEC(3,1)
      R23=ALE%GRID%MASSFLOW_DATA%EIGENVEC(3,2)
      R33=ALE%GRID%MASSFLOW_DATA%EIGENVEC(3,3)


      !8.---MEAN MASS DENSITY
      !-----------------------------------
      CALL MY_BARRIER

      IF(DT1 == ZERO)THEN
        MS_ELEM_MEAN = EP20
        IF(ALE%GRID%MASSFLOW_DATA%NUM_ELEM_ALE > 0)THEN
          MS_ELEM_MEAN = ALE%GRID%MASSFLOW_DATA%SUM_M / (ONE*ALE%GRID%MASSFLOW_DATA%NUM_ELEM_ALE)
        ENDIF
        ALE%GRID%MASSFLOW_DATA%MS_ELEM_MEAN_0 = MS_ELEM_MEAN
      ENDIF
      MS_ELEM_MEAN = ALE%GRID%MASSFLOW_DATA%MS_ELEM_MEAN_0


      !9.---ENCOMPASSING BOX (FLOW & WHOLE GRID)
      !-----------------------------------
      ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(1:3) = EP20   !FLOW min 1,2,3
      ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(4:6) = -EP20  !FLOW max 1,2,3
      ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(1:3) = EP20 ! FULL GRID min 1,2,3
      ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(4:6) = -EP20 ! FULL GRID max 1,2,3
      CALL MY_BARRIER

      X_MIN_MAX(1:3) = EP20
      X_MIN_MAX(4:6) = -EP20
      X_MIN_MAX_GRID(1:3) = EP20
      X_MIN_MAX_GRID(4:6) = -EP20

      HAS_ALE_NODE = .FALSE.
      HAS_FLOW_NODE = .FALSE.

      DO I = NODFT, NODLT
        IF(IABS(NALE(I)) == 1)THEN
          HAS_ALE_NODE = .TRUE.
          ! apply transition matrix (local basis)
          X_TRANS(1) = R11*(X(1,I)-COG(1))+R12*(X(2,I)-COG(2))+R13*(X(3,I)-COG(3))
          X_TRANS(2) = R21*(X(1,I)-COG(1))+R22*(X(2,I)-COG(2))+R23*(X(3,I)-COG(3))
          X_TRANS(3) = R31*(X(1,I)-COG(1))+R32*(X(2,I)-COG(2))+R33*(X(3,I)-COG(3))
          !FULL GRID encompassing box (local basis)
          X_MIN_MAX_GRID(1) = MIN(X_MIN_MAX_GRID(1),X_TRANS(1))
          X_MIN_MAX_GRID(2) = MIN(X_MIN_MAX_GRID(2),X_TRANS(2))
          X_MIN_MAX_GRID(3) = MIN(X_MIN_MAX_GRID(3),X_TRANS(3))
          X_MIN_MAX_GRID(4) = MAX(X_MIN_MAX_GRID(4),X_TRANS(1))
          X_MIN_MAX_GRID(5) = MAX(X_MIN_MAX_GRID(5),X_TRANS(2))
          X_MIN_MAX_GRID(6) = MAX(X_MIN_MAX_GRID(6),X_TRANS(3))
          !MAIN FLOW encompassing box(local basis)
          MS_NODE = MS(I)*WEIGHT(I)
          RATIO(1) = MS_NODE / MS_ELEM_MEAN  !MS_ELEM_MEAN_0 ! with ratio : focus only on main flow  ! w/o ratio : all coordinates
          IF(RATIO(1) > ONE)THEN
            HAS_FLOW_NODE = .TRUE.
            X_MIN_MAX(1) = MIN(X_MIN_MAX(1),X_TRANS(1))
            X_MIN_MAX(2) = MIN(X_MIN_MAX(2),X_TRANS(2))
            X_MIN_MAX(3) = MIN(X_MIN_MAX(3),X_TRANS(3))
            X_MIN_MAX(4) = MAX(X_MIN_MAX(4),X_TRANS(1))
            X_MIN_MAX(5) = MAX(X_MIN_MAX(5),X_TRANS(2))
            X_MIN_MAX(6) = MAX(X_MIN_MAX(6),X_TRANS(3))
          ENDIF
        ENDIF
      ENDDO
      !gathering values for SMP
#include "lockon.inc"
      !assembly of global values for SMP case
      !---FLOW
      IF(HAS_FLOW_NODE)THEN
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(1) = MIN(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(1), X_MIN_MAX(1))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(2) = MIN(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(2), X_MIN_MAX(2))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(3) = MIN(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(3), X_MIN_MAX(3))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(4) = MAX(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(4), X_MIN_MAX(4))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(5) = MAX(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(5), X_MIN_MAX(5))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(6) = MAX(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(6), X_MIN_MAX(6))
      ENDIF
      !---FULL GRID
      IF(HAS_ALE_NODE)THEN
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(1) = MIN(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(1), X_MIN_MAX_GRID(1))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(2) = MIN(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(2), X_MIN_MAX_GRID(2))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(3) = MIN(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(3), X_MIN_MAX_GRID(3))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(4) = MAX(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(4), X_MIN_MAX_GRID(4))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(5) = MAX(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(5), X_MIN_MAX_GRID(5))
        ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(6) = MAX(ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(6), X_MIN_MAX_GRID(6))
      ENDIF
#include "lockoff.inc"
      CALL MY_BARRIER
      !gathering values for SPMD
      IF(NSPMD > 1)THEN
         CALL SPMD_EXCH_MASSFLOW_DATA4(ALE%GRID%MASSFLOW_DATA, NSPMD)
      ENDIF

      CALL MY_BARRIER


      !10. RATIO (to detect border proximity)
      !-----------------------------------
      !local axis 1 - BETA_Xmin,BETA_Xmax
      BETA(1)=ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(1)/ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(1)
      BETA(4)=ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(4)/ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(4)
      !local axis 2 - BETA_Ymin,BETA_Ymax
      BETA(2)=ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(2)/ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(2)
      BETA(5)=ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(5)/ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(5)
      !local axis 3 - BETA_Zmin,BETA_Zmax
      BETA(3)=ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(3)/ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(3)
      BETA(6)=ALE%GRID%MASSFLOW_DATA%X_MIN_MAX(6)/ALE%GRID%MASSFLOW_DATA%X_MIN_MAX_GRID(6)
      ! store initial value
      IF(DT1 == ZERO)THEN
        ALE%GRID%MASSFLOW_DATA%BETA0(1) = BETA(1)
        ALE%GRID%MASSFLOW_DATA%BETA0(2) = BETA(2)
        ALE%GRID%MASSFLOW_DATA%BETA0(3) = BETA(3)
        ALE%GRID%MASSFLOW_DATA%BETA0(4) = BETA(4)
        ALE%GRID%MASSFLOW_DATA%BETA0(5) = BETA(5)
        ALE%GRID%MASSFLOW_DATA%BETA0(6) = BETA(6)
      END IF
      CALL MY_BARRIER
      !retrieve initial ratio
      BETA0(1) = ALE%GRID%MASSFLOW_DATA%BETA0(1)
      BETA0(2) = ALE%GRID%MASSFLOW_DATA%BETA0(2)
      BETA0(3) = ALE%GRID%MASSFLOW_DATA%BETA0(3)
      BETA0(4) = ALE%GRID%MASSFLOW_DATA%BETA0(4)
      BETA0(5) = ALE%GRID%MASSFLOW_DATA%BETA0(5)
      BETA0(6) = ALE%GRID%MASSFLOW_DATA%BETA0(6)


      !11.---NORM OF STRAIN RATE TENSOR (global basis)
      !-----------------------------------
      CALL MY_BARRIER
      !local working array
      LD(1:6)=ALE%GRID%MASSFLOW_DATA%LD(1:6)
      LW(1:3)=ALE%GRID%MASSFLOW_DATA%LW(1:3)
      ALE%GRID%MASSFLOW_DATA%SUM_M = ZERO

      LD_B(1,1:3) = (/LD(1),LD(4),LD(5)/)
      LD_B(2,1:3) = (/LD(4),LD(2),LD(6)/)
      LD_B(3,1:3) = (/LD(5),LD(6),LD(3)/)

      LD_NORM = ZERO
      LD_NORM = LD_NORM + ABS(LD_B(1,1)) + ABS(LD_B(1,2)) + ABS(LD_B(1,3))
      LD_NORM = LD_NORM + ABS(LD_B(2,1)) + ABS(LD_B(2,2)) + ABS(LD_B(2,3))
      LD_NORM = LD_NORM + ABS(LD_B(3,1)) + ABS(LD_B(3,2)) + ABS(LD_B(3,3))
      LD_NORM = LD_NORM / NINE

      CALL MY_BARRIER
      IF( ALE%GRID%MASSFLOW_DATA%LD_NORM < LD_NORM)THEN
        ALE%GRID%MASSFLOW_DATA%LD_NORM = LD_NORM
      ENDIF
      LD_NORM = ALE%GRID%MASSFLOW_DATA%LD_NORM


      !12.---CORRECTION TERM : LD_TOTAL = LD_AVERAGE + LD_CORRECTION
      !-----------------------------------
      ! B  : global basis
      ! B' : local basis (Inertia Tensor Matrix)
      !transition matrix P_[B->B']
      P_B_Bp(1:3,1)=EIGENVEC(1:3,1)
      P_B_Bp(1:3,2)=EIGENVEC(1:3,2)
      P_B_Bp(1:3,3)=EIGENVEC(1:3,3)

      !factor margin in basis B'
      !   fac > 1 means that border is too near the flow
      FAC(1) = MAX(ONE, BETA(1)/BETA0(1), BETA(4)/BETA0(4))
      FAC(2) = MAX(ONE, BETA(2)/BETA0(2), BETA(5)/BETA0(5))
      FAC(3) = MAX(ONE, BETA(3)/BETA0(3), BETA(6)/BETA0(6))

      ! amplification with zero derivative at 1.000
      ksi = EP02
      FAC(1) = HALF*LD_NORM *(ONE + ksi*(FAC(1)-ONE)**2)
      FAC(2) = HALF*LD_NORM *(ONE + ksi*(FAC(2)-ONE)**2)
      FAC(3) = HALF*LD_NORM *(ONE + ksi*(FAC(3)-ONE)**2)

      ! correction term (strain rate tensor) in local basis B'
      LD_Bp(1,1:3) = (/ FAC(1) , ZERO   ,    ZERO /)
      LD_Bp(2,1:3) = (/ ZERO   , FAC(2) ,    ZERO /)
      LD_Bp(3,1:3) = (/ ZERO   , ZERO   ,    FAC(3) /)

      ! correction term (strain rate tensor) in in global basis B
      LD_Bp_B = matmul ( P_B_Bp, matmul(LD_Bp, transpose(P_B_Bp)) )


      !13.---TOTAL STRAIN RATE TENSOR
      !-----------------------------------
      !LD_B : averaged strain rate tensor in global basis
      !LD_Bp_B : correction term computed in local basis and then written in global basis
      !LD_TOT_B : total strain rate tensor in global basis
      LD_TOT_B = LD_B + LD_Bp_B
      !working array
      LD(1)=LD_TOT_B(1,1)
      LD(2)=LD_TOT_B(2,2)
      LD(3)=LD_TOT_B(3,3)
      LD(4)=LD_TOT_B(1,2)
      LD(5)=LD_TOT_B(1,3)
      LD(6)=LD_TOT_B(2,3)


      !14.---UPDATE GRID
      !-----------------------------------
      DO I = NODFT, NODLT
         IF(IABS(NALE(I)) == 1) THEN
            ! GRID TRANSLATION
            W(1,I)=VMEAN(1)
            W(2,I)=VMEAN(2)
            W(3,I)=VMEAN(3)

            ! coordinates relative to center of mass
            XX = (X(1,I)-COG(1))
            YY = (X(2,I)-COG(2))
            ZZ = (X(3,I)-COG(3))

            !GRID DEFORMATION
            IF(lDEF)THEN
              !increment in global basis (from strain rate tensor + correction term)
              DW(1) = SCALE_DEF*(LD(1)*XX+LD(4)*YY+LD(5)*ZZ)
              DW(2) = SCALE_DEF*(LD(4)*XX+LD(2)*YY+LD(6)*ZZ)
              DW(3) = SCALE_DEF*(LD(5)*XX+LD(6)*YY+LD(3)*ZZ)
              ! update grid velocities
              W(1,I) =W(1,I)+ DW(1)
              W(2,I) =W(2,I)+ DW(2)
              W(3,I) =W(3,I)+ DW(3)
            ENDIF

            !GRID ROTATION
            IF(lROT)THEN
              W(1,I) = W(1,I) + SCALE_ROT*(LW(2)*ZZ-LW(1)*YY)
              W(2,I) = W(2,I) + SCALE_ROT*(LW(1)*XX-LW(3)*ZZ)
              W(3,I) = W(3,I) + SCALE_ROT*(LW(3)*YY-LW(2)*XX)
            ENDIF

         ELSEIF(NALE(I) == 0)THEN
            ! lagrangian framework
            W(1,I)=V(1,I)
            W(2,I)=V(2,I)
            W(3,I)=V(3,I)
         ELSE
            ! eulerian framework
            W(1,I)=ZERO
            W(2,I)=ZERO
            W(3,I)=ZERO
         ENDIF
      ENDDO


      !14.---LOCAL AXIS UPDATE (rotation)
      !-----------------------------------
      IF(lROT)THEN
        !rotate local basis
        DO JJ=1,3
          XX = EIGENVEC(1,JJ)
          YY = EIGENVEC(2,JJ)
          ZZ = EIGENVEC(3,JJ)
          EIGENVEC(1,JJ) = XX + DT1*SCALE_ROT* (LW(2)*ZZ-LW(1)*YY)
          EIGENVEC(2,JJ) = YY + DT1*SCALE_ROT* (LW(1)*XX-LW(3)*ZZ)
          EIGENVEC(3,JJ) = ZZ + DT1*SCALE_ROT* (LW(3)*YY-LW(2)*XX)
        ENDDO
        ! save it in data structure (RESTART)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,1)=EIGENVEC(1:3,1)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,2)=EIGENVEC(1:3,2)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,3)=EIGENVEC(1:3,3)
      ENDIF

C-----------------------------------------------
      RETURN
      END SUBROUTINE ALEW7

