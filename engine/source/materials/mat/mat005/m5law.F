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
Chd|  M5LAW                         source/materials/mat/mat005/m5law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M5LAW(PM   ,SIG   ,EINT  ,RHO  ,PSH   ,
     1                 P0   ,TBURN ,BFRAC ,VOLN ,DELTAX,
     2                 MAT  ,NEL   ,SSP   ,DF   ,
     3                 ER1V, ER2V, WDR1V, WDR2V, W1    ,
     4                 RHO0, AMU)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NEL
      my_real
     . PM(NPROPM,*), SIG(NEL,6), EINT(*), RHO(*), 
     . PSH(*), P0(*),
     . TBURN(MVSIZ), BFRAC(MVSIZ), VOLN(MVSIZ),
     . DELTAX(*), SSP(*), DF(*), ER1V(*), ER2V(*), WDR1V(*), WDR2V(*), W1(*), RHO0(*), 
     . AMU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IBFRAC_1
      my_real
     .   R1(MVSIZ)  , R2(MVSIZ)  , BHE(MVSIZ),
     .   VDET(MVSIZ) , R1V(MVSIZ) , R2V(MVSIZ), 
     .   DR1V(MVSIZ),
     .   P(MVSIZ)    , B1(MVSIZ)   , B2(MVSIZ),
     .   TB, BFRAC1, BFRAC2, RHO0_1 , PSH_1,  B1_1 , B2_1 , R1_1,
     .   R2_1, W1_1, VDET_1, BHE_1, P0_1, BULK
C-----------------------------------------------
      MX         = MAT(1)
      RHO0_1     = PM( 1,MX)
      B1_1       = PM(33,MX)
      B2_1       = PM(34,MX)
      R1_1       = PM(35,MX)
      R2_1       = PM(36,MX)
      W1_1       = PM(45,MX)
      VDET_1     = PM(38,MX)
      BHE_1      = PM(40,MX)
      PSH_1      = PM(88,MX)
      P0_1       = PM(31,MX)
      BULK       = PM(44,MX)
      IBFRAC_1   = NINT(PM(41,MX))

      DO I=1,NEL
        RHO0(I)  = RHO0_1
        B1(I)    = B1_1
        B2(I)    = B2_1
        R1(I)    = R1_1
        R2(I)    = R2_1
        W1(I)    = W1_1
        VDET(I)  = VDET_1
        BHE(I)   = BHE_1
        PSH(I)   = PSH_1
        P0(I)    = P0_1
      ENDDO

      DO I=1,NEL
        DF(I)    = RHO0(I)/RHO(I) ! DF = v = V/V0 = 1/(MU+1)
      ENDDO

      DO I=1,NEL
        IF(BFRAC(I) < ONE) THEN
          TB=-TBURN(I)
          BFRAC1 = ZERO
            BFRAC2 = ZERO
          IF(IBFRAC_1/=1 .AND. TT>TB) BFRAC1=VDET(I)*(TT-TB)/THREE_HALF/DELTAX(I)   !time control
          IF(IBFRAC_1/=2)BFRAC2=BHE(I)*(ONE-DF(I))                                 !volumetric control
          BFRAC(I) = MAX(BFRAC1,BFRAC2)
          IF(BFRAC(I)<EM04) BFRAC(I)=ZERO
          IF(BFRAC(I)>ONE   ) THEN
            BFRAC(I) = ONE
          ENDIF
        ENDIF
      ENDDO

      DO I=1,NEL
        R1V(I)   = B1(I)*W1(I)/(R1(I)*DF(I))
        R2V(I)   = B2(I)*W1(I)/(R2(I)*DF(I))
        WDR1V(I) = B1(I)-R1V(I)
        WDR2V(I) = B2(I)-R2V(I)
        DR1V(I)  = W1(I)*EINT(I)/MAX(EM20,VOLN(I))    !w*Eint/V = w*E/v  where v=V/V0
        ER1V(I)  = EXP(-R1(I)*DF(I))
        ER2V(I)  = EXP(-R2(I)*DF(I))
      ENDDO

      IF (BULK == ZERO) THEN
         DO I=1,NEL
            P(I)     =  P0(I) - PSH(I) + (WDR1V(I)*ER1V(I)+WDR2V(I)*ER2V(I)+DR1V(I))
!!! Taking -PSH(I) as minimum value for pressure
            P(I)     = MAX(ZERO - PSH(I), P(I))
         ENDDO
      ELSE
         DO I=1,NEL
            P(I)     =  - PSH(I) + (ONE - BFRAC(I)) * (P0(I) + BULK * AMU(I)) + 
     .           BFRAC(I) * (WDR1V(I)*ER1V(I)+WDR2V(I)*ER2V(I)+DR1V(I))
!!! Taking -PSH(I) as minimum value for pressure
            P(I)     = MAX(ZERO - PSH(I), P(I))
         ENDDO
      ENDIF

      DO I=1,NEL
        SSP(I)   = B1_1*ER1V(I)*( (-W1_1/DF(I)/R1_1) + R1_1*DF(I) - W1_1)
     .           + B2_1*ER2V(I)*( (-W1_1/DF(I)/R2_1) + R2_1*DF(I) - W1_1)
     .           + DR1V(I)  +   (P(I) + PSH(I))*W1_1
        SSP(I)   = SSP(I) * DF(I)
      ENDDO

      IF (BULK == ZERO) THEN
         DO I=1,NEL
            SSP(I)   = SQRT(ABS(SSP(I))/RHO0(I))
            SSP(I)   = MAX(SSP(I),VDET(I)*(ONE-BFRAC(I)))
         ENDDO
      ELSE
         DO I=1,NEL
            SSP(I)   =  BFRAC(I) * (SSP(I) / RHO0(I)) + 
     .           (ONE - BFRAC(I)) * (BULK / RHO0(I))
            SSP(I)   = SQRT(ABS(SSP(I)))
         ENDDO
      ENDIF

      DO I=1,NEL
        !FLUID => NO DEVIATOR STRESS
        SIG(I,1) = ZERO
        SIG(I,2) = ZERO
        SIG(I,3) = ZERO
        SIG(I,4) = ZERO
        SIG(I,5) = ZERO
        SIG(I,6) = ZERO
      ENDDO

      RETURN
      END
