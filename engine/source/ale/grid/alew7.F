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
Chd|====================================================================
Chd|  ALEW7                         source/ale/grid/alew7.F       
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_MASSFLOW_DATA       source/ale/grid/spmd_exch_massflow_data.F
Chd|        SPMD_EXCH_MASSFLOW_DATA2      source/ale/grid/spmd_exch_massflow_data2.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE ALEW7(
     1   X       ,V       ,W       ,MS      , NALE,
     2   NODFT   ,NODLT   ,WEIGHT  ,NUMNOD  ,
     3   SX      ,SV      ,SW      ,NSPMD)
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
      my_real, INTENT(IN) :: X(3,SX/3)
      my_real, INTENT(IN) :: V(3,SV/3), MS(NUMNOD)
      my_real, INTENT(INOUT) :: W(3,SW/3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, LENCOM
      INTEGER :: LOC_PROC, ISIZE
      INTEGER :: IDEFORMATION,IROTATION
      my_real :: SUM_MS,MS_NODE
      my_real :: SUM_MOM(3),SUM_COG(3),SUM_M !1:m*vx, 2:m*vy, 3:m*vz, 4:m
      my_real :: VMEAN(3),LD(6),LW(3),COG(3)
      my_real :: XX,YY,ZZ
      my_real :: SCALE_DEF,SCALE_ROT
      LOGICAL :: lTENSOR, lDEF,lROT
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

      !2.---DEFORMATION TENSOR
      !-----------------------------------
!$OMP SINGLE
      IF(lTENSOR)THEN
         IF(NSPMD > 1)THEN
           CALL SPMD_EXCH_MASSFLOW_DATA(ALE%GRID%MASSFLOW_DATA, NSPMD)
         ENDIF
         ALE%GRID%MASSFLOW_DATA%EP(1:9) = ALE%GRID%MASSFLOW_DATA%EP(1:9) / ALE%GRID%MASSFLOW_DATA%SUM_M
         !STRAIN RATE TENSOR
         ALE%GRID%MASSFLOW_DATA%LD(1) = ALE%GRID%MASSFLOW_DATA%EP(1)
         ALE%GRID%MASSFLOW_DATA%LD(2) = ALE%GRID%MASSFLOW_DATA%EP(2)
         ALE%GRID%MASSFLOW_DATA%LD(3) = ALE%GRID%MASSFLOW_DATA%EP(3)
         ALE%GRID%MASSFLOW_DATA%LD(4) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(4)+ALE%GRID%MASSFLOW_DATA%EP(7))
         ALE%GRID%MASSFLOW_DATA%LD(5) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(5)+ALE%GRID%MASSFLOW_DATA%EP(8))
         ALE%GRID%MASSFLOW_DATA%LD(6) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(6)+ALE%GRID%MASSFLOW_DATA%EP(9))
         !SPIN TENSOR
         ALE%GRID%MASSFLOW_DATA%LW(1) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(4)-ALE%GRID%MASSFLOW_DATA%EP(7))
         ALE%GRID%MASSFLOW_DATA%LW(2) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(5)-ALE%GRID%MASSFLOW_DATA%EP(8))
         ALE%GRID%MASSFLOW_DATA%LW(3) = HALF*(ALE%GRID%MASSFLOW_DATA%EP(6)-ALE%GRID%MASSFLOW_DATA%EP(9))
      ENDIF
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif

      !3.---MEAN VELOCITY & CENTER OF MASS
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
      !SPMD EXCHANGE
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

      !4.---GRID VELOCITIES
      !-----------------------------------
      CALL MY_BARRIER
      !local working array
      LD(1:6)=ALE%GRID%MASSFLOW_DATA%LD(1:6)
      LW(1:3)=ALE%GRID%MASSFLOW_DATA%LW(1:3)
      ALE%GRID%MASSFLOW_DATA%SUM_M = ZERO

      DO I = NODFT, NODLT
         IF(IABS(NALE(I)) == 1) THEN

            !GRID TRANSLATION
            W(1,I)=VMEAN(1)
            W(2,I)=VMEAN(2)
            W(3,I)=VMEAN(3)

            ! DEFORMATION & ROTATION : coordinates relative to center of mass
            XX = (X(1,I)-COG(1))
            YY = (X(2,I)-COG(2))
            ZZ = (X(3,I)-COG(3))

            !GRID DEFORMATION
            IF(lDEF)THEN
              !W(1,I) = W(1,I)+LD(1)*XX
              !W(2,I) = W(2,I)+LD(2)*YY
              !W(3,I) = W(3,I)+LD(3)*ZZ
              W(1,I) =W(1,I)+ SCALE_DEF*(LD(1)*XX+LD(4)*YY+LD(5)*ZZ)
              W(2,I) =W(2,I)+ SCALE_DEF*(LD(4)*XX+LD(2)*YY+LD(6)*ZZ)
              W(3,I) =W(3,I)+ SCALE_DEF*(LD(5)*XX+LD(6)*YY+LD(3)*ZZ)
            ENDIF

            !GRID ROTATION
            IF(lROT)THEN
              W(1,I) = W(1,I) + SCALE_ROT*(LW(2)*ZZ-LW(1)*YY)
              W(2,I) = W(2,I) + SCALE_ROT*(LW(1)*XX-LW(3)*ZZ)
              W(3,I) = W(3,I) + SCALE_ROT*(LW(3)*XX-LW(2)*XX)
            ENDIF

         ELSEIF(NALE(I) == 0)THEN
            W(1,I)=V(1,I)
            W(2,I)=V(2,I)
            W(3,I)=V(3,I)
         ELSE
            W(1,I)=ZERO
            W(2,I)=ZERO
            W(3,I)=ZERO
         ENDIF
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE ALEW7

