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
      !||====================================================================
      !||    m5law   ../engine/source/materials/mat/mat005/m5law.F
      !||--- called by ------------------------------------------------------
      !||    mmain   ../engine/source/materials/mat_share/mmain.F90
      !||====================================================================
      SUBROUTINE M5LAW(PM   ,SIG   ,EINT  ,RHO    ,PSH   ,
     1                 P0   ,TBURN ,BFRAC ,VOLN   ,DELTAX,
     2                 MAT  ,NEL   ,SSP   ,DF     ,
     3                 ER1V ,ER2V  ,WDR1V ,WDR2V  ,W1    ,
     4                 RHO0 ,AMU   ,NUMMAT,TT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NUMMAT                !< size for PM array
      INTEGER,INTENT(IN)    :: MAT(*)                !< application : [1:NEL] -> mat_id
      INTEGER,INTENT(IN)    :: NEL                   !< number of element in the current group
      my_real,INTENT(IN)    :: TT                    !< current time
      my_real,INTENT(IN)    :: PM(NPROPM,NUMMAT)     !< material buffer (real)
      my_real,INTENT(INOUT) :: SIG(NEL,6)            !< stress tensor
      my_real,INTENT(IN)    :: EINT(NEL)             !< internal energy
      my_real,INTENT(IN)    :: RHO(NEL)              ! mass density
      my_real,INTENT(INOUT) :: PSH(*)                !< pressure shift
      my_real,INTENT(INOUT) :: P0(*)                 !< initial pressure
      my_real,INTENT(IN)    :: TBURN(MVSIZ)          !<time of burn (detonation time)
      my_real,INTENT(INOUT) :: BFRAC(MVSIZ)          !< burn fraction
      my_real,INTENT(IN)    :: VOLN(MVSIZ)           !< element volume
      my_real,INTENT(IN)    :: DELTAX(NEL)           !< mesh size
      my_real,INTENT(INOUT) :: SSP(NEL)              !< sound speed
      my_real,INTENT(INOUT) :: DF(*)                 !< V/V0 (or rho0/rho or 1/(1+mu))
      my_real,INTENT(IN)    :: AMU(*)                !< volumetric strain
      my_real,INTENT(INOUT) :: ER1V(NEL), ER2V(NEL), WDR1V(NEL), WDR2V(NEL), W1(NEL), RHO0(NEL)   !< working arrays to save computation time
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IBFRAC
      my_real A,B, R1, R2, W, VDET, PSH_PARAM, BULK, P0_PARAM, RHO0_PARAM !< eos parameters
      my_real TB                                                                   !< time of burn
      my_real BFRAC1, BFRAC2, BHE                                                  !< working arrays for burn fraction calculation
      my_real R1V(MVSIZ), R2V(MVSIZ), DR1V(MVSIZ)                                  !< temporary arrays
      my_real P(MVSIZ)                                                             !< pressure
C-----------------------------------------------
C   B o d y
C-----------------------------------------------

      ! User defined material properties
      MX         = MAT(1)
      RHO0_PARAM = PM( 1,MX)
      A          = PM(33,MX)
      B          = PM(34,MX)
      R1         = PM(35,MX)
      R2         = PM(36,MX)
      W          = PM(45,MX)
      VDET       = PM(38,MX)
      BHE        = PM(40,MX)
      PSH_PARAM  = PM(88,MX)
      P0_PARAM   = PM(31,MX)
      BULK       = PM(44,MX)
      IBFRAC     = NINT(PM(41,MX))

      ! Initialize the material properties used later from mjwl.F
      DO I=1,NEL
        RHO0(I) = RHO0_PARAM
        PSH(I) = PSH_PARAM
        P0(I) = P0_PARAM
        W1(I) = W
      ENDDO

      ! Relative Volume Calculation
      DO I=1,NEL
        DF(I) = RHO0(I)/RHO(I) ! DF = v = V/V0 = RHO0/RHO = 1/(MU+1)
      ENDDO

      ! Burn Fraction Calculation
      !   BFRAC1 : time control
      !   BFRAC2 : volumetric control
      DO I=1,NEL
        IF(BFRAC(I) < ONE) THEN
          TB=-TBURN(I)
          BFRAC1 = ZERO
          BFRAC2 = ZERO
          IF(IBFRAC /= 1 .AND. TT > TB) BFRAC1=VDET*(TT-TB)/THREE_HALF/DELTAX(I)   !time control
          IF(IBFRAC /= 2)BFRAC2=BHE*(ONE-DF(I))                                    !volumetric control
          BFRAC(I) = MAX(BFRAC1,BFRAC2)
          IF(BFRAC(I)<EM04) BFRAC(I)=ZERO
          IF(BFRAC(I)>ONE) THEN
            BFRAC(I) = ONE
          ENDIF
        ENDIF
      ENDDO

      ! working arrays to save CPU operations. ER1V ER2V, WDR1V, WDR2V used later from mjwl.F
      DO I=1,NEL
        R1V(I)   = A*W/(R1*DF(I))
        R2V(I)   = B*W/(R2*DF(I))
        WDR1V(I) = A-R1V(I)
        WDR2V(I) = B-R2V(I)
        DR1V(I)  = W*EINT(I)/MAX(EM20,VOLN(I))    !w*Eint/V = w*E/v  where v=V/V0
        ER1V(I)  = EXP(-R1*DF(I))
        ER2V(I)  = EXP(-R2*DF(I))
      ENDDO

      ! Pressure Calculation
      IF (BULK == ZERO) THEN
         ! Behavior of unreacted explosive is not provided
         DO I=1,NEL
            P(I) =  P0(I) + (WDR1V(I)*ER1V(I)+WDR2V(I)*ER2V(I)+DR1V(I))
         ENDDO
      ELSE
         ! Behavior of unreacted explosive is provided
         DO I=1,NEL
            P(I) = (ONE - BFRAC(I))*(P0(I)+BULK*AMU(I)) + BFRAC(I)*(WDR1V(I)*ER1V(I)+WDR2V(I)*ER2V(I)+DR1V(I))
         ENDDO
      ENDIF
      DO I=1,NEL
            P(I) = MAX(ZERO, P(I)) - PSH(I)  !PMIN = 0.0 (fluid)
      ENDDO

      ! Sound Speed Calculation
      DO I=1,NEL
        SSP(I) = A*ER1V(I)*( (-W/DF(I)/R1) + R1*DF(I) - W)
     .         + B*ER2V(I)*( (-W/DF(I)/R2) + R2*DF(I) - W)
     .         + DR1V(I)  +   (P(I) + PSH(I))*W
        SSP(I) = SSP(I) * DF(I)
      ENDDO
      IF (BULK == ZERO) THEN
         DO I=1,NEL
            SSP(I) = SQRT(ABS(SSP(I))/RHO0(I))
            SSP(I) = MAX(SSP(I),VDET*(ONE-BFRAC(I)))
         ENDDO
      ELSE
         DO I=1,NEL
            SSP(I) =  BFRAC(I) * (SSP(I) / RHO0(I)) + (ONE - BFRAC(I)) * (BULK / RHO0(I))
            SSP(I) = SQRT(ABS(SSP(I)))
         ENDDO
      ENDIF

      ! Return the updated stress tensor. FLUID => NO DEVIATOR STRESS
      DO I=1,NEL
        SIG(I,1) = ZERO
        SIG(I,2) = ZERO
        SIG(I,3) = ZERO
        SIG(I,4) = ZERO
        SIG(I,5) = ZERO
        SIG(I,6) = ZERO
      ENDDO

      RETURN
      END
