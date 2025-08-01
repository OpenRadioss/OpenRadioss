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
!||    gray20   ../engine/source/materials/mat/mat016/gray20.F
!||--- called by ------------------------------------------------------
!||    mmain    ../engine/source/materials/mat_share/mmain.F90
!||--- calls      -----------------------------------------------------
!||    gray21   ../engine/source/materials/mat/mat016/gray21.F
!||====================================================================
      SUBROUTINE GRAY20(
     1   PM,      EINT,    RHO,     TEMP,
     2   XIST,    QOLD,    VOLN,    MAT,
     3   DVOL,    POLD,    DF,      RHO0,
     4   P1,      P01,     P02,     E01,
     5   E02,     SSP,     DPDM,    NEL)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   PM(NPROPM,*), EINT(*), RHO(*), TEMP(*), XIST(*), QOLD(*), VOLN(*), 
     .   DVOL(*), POLD(*), DF(*), 
     .   RHO0(*), P1(*),   P01(*), P02(*), E01(*), E02(*), SSP(*), DPDM(*) 
      INTEGER MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, MX
      my_real
     .   EM0(MVSIZ),  EM1(MVSIZ),  EM2(MVSIZ), 
     .   ESPE(MVSIZ), ALP(MVSIZ),  PCR(MVSIZ), C(MVSIZ),
     .   GEAX(MVSIZ), G0AX(MVSIZ), TM(MVSIZ),  DELT(MVSIZ), RP3(MVSIZ), X(MVSIZ),   GP(MVSIZ), 
     .   DSP(MVSIZ),  XLAM(MVSIZ), S(MVSIZ),   PCC(MVSIZ),
     .   E0H(MVSIZ),  TM0(MVSIZ),  EGG(MVSIZ), XP(MVSIZ),   A(MVSIZ),   AM(MVSIZ),  GAM0(MVSIZ),
     .   GAME(MVSIZ), GAM0M(MVSIZ),E0(MVSIZ),  E00(MVSIZ),  TG(MVSIZ),  PMIN(MVSIZ),XIST0(MVSIZ), 
     .   P1A(MVSIZ),  DMU(MVSIZ),  XM, UNPM2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !-----------------------------
      !
      !     CALCUL DES VARIABLES GRAY
      !
      !-----------------------------

      UNPM2       = O88P9844
      XM          = NINEP38

      MX          = MAT(1)
      DO I=1,NEL
        DPDM(I)   = ONEP333*PM(22,MX)
        TM0(I)    = PM(29,MX)
        E00(I)    = PM(30,MX)
        C(I)      = PM(33,MX)
        S(I)      = PM(34,MX)
        PCC(I)    = PM(35,MX)
        GAM0(I)   = PM(36,MX)
        PMIN(I)   = PM(37,MX)
        A(I)      = PM(48,MX)
        GAM0M(I)  = PM(49,MX)
        AM(I)     = PM(50,MX)
        GAME(I)   = PM(51,MX)
        GP(I)     = PM(52,MX)
        DSP(I)    = PM(53,MX)
        E0H(I)    = PM(54,MX)
        RP3(I)    = PM(55,MX)
        ALP(I)    = PM(61,MX)
      ENDDO

      DO I=1,NEL
        DMU(I)    = -DVOL(I)/(VOLN(I)-DVOL(I))/DF(I)
        X(I)      = ONE-DF(I)
      ENDDO

      DO I=1,NEL
        E0(I)     = C(I)**2*X(I)**2*HALF/(ONE-S(I)*X(I))*(ONE+S(I)*X(I)/THREE+(S(I)-GAM0(I))*S(I)*X(I)**2*ONE_OVER_6) 
     .            + E00(I)*(ONE+GAM0(I)*X(I)) + E0H(I)
      ENDDO

      DO I=1,NEL
        XP(I)     = ZERO
        IF(X(I)>=ZERO) XP(I)=ONE
      ENDDO

      DO I=1,NEL
        TM(I)     = TM0(I)*((ONE-XP(I))*(ONE+TWO*(GAM0M(I)-FOUR_OVER_3)*X(I)+
     .             ((TWO*GAM0M(I)-FIVE_OVER_3)*(GAM0M(I)-FOUR_OVER_3)-AM(I))*X(I)**2)
     .     / (ONE-X(I))**2 + XP(I)*( ONE+(TWO*GAM0M(I)-TWO_THIRD)*X(I)+((GAM0M(I)-THIRD)*(TWO*GAM0M(I)+THIRD)-AM(I))*X(I)**2))
        TG(I)     = TM(I)*XM
      ENDDO

      DO I=1,NEL
        XLAM(I)   = TWO_THIRD - TWO*GAM0M(I)+TWO*AM(I)*X(I)
      ENDDO

      DO I=1,NEL
        DELT(I)   = DSP(I)*XLAM(I)**2*TM(I)**2
      ENDDO

      DO I=1,NEL
        EM1(I)    = E0(I)+RP3(I)*(TM(I)+DELT(I))+HALF*GP(I)*(TM(I)+DELT(I))**2
        EM2(I)    = EM1(I)+TM(I)*(DSP(I)-HALF*ALP(I)*(1.+(TM(I)+DELT(I))**2/TM(I)**2))
        EGG(I)    = E0(I)+RP3(I)*TG(I)+HALF*GP(I)*TG(I)**2+TM(I)*(DSP(I)-HALF*ALP(I)*UNPM2)
        G0AX(I)   = GAM0(I)-A(I)*X(I)
        GEAX(I)   =(GAME(I)-G0AX(I))*GP(I)
      ENDDO

      DO I=1,NEL
        XIST0(I)  = XIST(I)
        E01(I)    = EINT(I)-(POLD(I)+QOLD(I))*DVOL(I)*HALF
        E01(I)    = MAX(ZERO,E01(I))
        ESPE(I)   = E01(I)/(VOLN(I)*RHO(I))
        EM0(I)    = ESPE(I)-E0(I)
        P1A(I)    = C(I)**2*X(I)
     .            *(ONE-(ONE+HALF*GAM0(I))*X(I)+HALF*A(I)*X(I)**2)*RHO0(I)
     .            /((ONE-X(I))*(ONE-S(I)*X(I))**2)
        P1(I)     = PCC(I)+P1A(I)+G0AX(I)*(ESPE(I)-E0H(I))*RHO0(I)/(ONE-X(I))
      ENDDO
C-----------------------------
C     BRANCHEMENTS
C-----------------------------
      CALL GRAY21(
     1   PM,      RHO,     TEMP,    XIST,
     2   MAT,     RHO0,    DSP,     ALP,
     3   PCR,     P1,      EGG,     XIST0,
     4   XLAM,    EM0,     EM1,     EM2,
     5   ESPE,    GEAX,    G0AX,    TM,
     6   DELT,    RP3,     X,       GP,
     7   NEL)
C------------------------------------
C     PRESSION 1ERE ESTIMATION
C------------------------------------
      DO I=1,NEL
        P01(I)    = P1(I)+HALF*PCR(I)*RHO(I)
      ENDDO

      DO I=1,NEL
        IF(P01(I)<=PMIN(I))THEN
          P01(I)  = ZERO
          XIST(I) = FIVE
        ENDIF
      ENDDO

      DO I=1,NEL
        E02(I)    = EINT(I)-(P01(I)+QOLD(I))*DVOL(I)*HALF
        E02(I)    = MAX(ZERO,E02(I))
        ESPE(I)   = E02(I)/(VOLN(I)*RHO(I))
        EM0(I)    = ESPE(I)-E0(I)
        P1(I)     = PCC(I)+P1A(I)+G0AX(I)*(ESPE(I)-E0H(I))*RHO0(I)/(ONE-X(I))
      ENDDO
C-----------------------------
C     BRANCHEMENTS
C-----------------------------
      CALL GRAY21(
     1   PM,      RHO,     TEMP,    XIST,
     2   MAT,     RHO0,    DSP,     ALP,
     3   PCR,     P1,      EGG,     XIST0,
     4   XLAM,    EM0,     EM1,     EM2,
     5   ESPE,    GEAX,    G0AX,    TM,
     6   DELT,    RP3,     X,       GP,
     7   NEL)
C------------------------------------
C     PRESSION 2EME ESTIMATION
C------------------------------------
      DO I=1,NEL
        P02(I)    = P1(I)+HALF*PCR(I)*RHO(I)
      ENDDO

      DO I=1,NEL
        IF(P02(I)<=PMIN(I))THEN
          P02(I)  = ZERO
          XIST(I) = FIVE
        ENDIF
      ENDDO
C------------------------------------
C     VITESSE DU SON
C------------------------------------
      DO I=1,NEL
        IF(DMU(I)/=ZERO)THEN
          DPDM(I) = DPDM(I)+MAX(RHO0(I)*C(I)*C(I),ABS((P02(I)-POLD(I))/DMU(I)))
        ELSE
          DPDM(I) = DPDM(I)+RHO0(I)*C(I)*C(I)
        ENDIF
      ENDDO

      DO I=1,NEL
        SSP(I)    = SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO

      RETURN
      END
