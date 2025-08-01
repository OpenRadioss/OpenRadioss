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
!||    sesa20     ../engine/source/materials/mat/mat026/sesa20.F
!||--- called by ------------------------------------------------------
!||    mmain      ../engine/source/materials/mat_share/mmain.F90
!||--- calls      -----------------------------------------------------
!||    mintp_re   ../common_source/eos/mintp_re.F
!||    mintp_rt   ../common_source/eos/mintp_rt.F
!||====================================================================
      SUBROUTINE SESA20(
     1   PM,      EINT,    RHO,     TEMP,
     2   Z,       QOLD,    SESAME,  SOUND,
     3   XK,      VOLN,    MAT,     C,
     4   DV2,     POLD,    SSP,     RHO0,
     5   P01,     NEL)
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
     .   PM(NPROPM,*), EINT(*), RHO(*), TEMP(*), Z(*), QOLD(*),
     .   SESAME(*), SOUND(*), XK(*), VOLN(MVSIZ), C(*), DV2(*), POLD(*), SSP(*),
     .   RHO0(*), P01(*)
      INTEGER MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, NR, NT, IDR, IDT, IDP, IDE, IDQ
      my_real
     .   DPDM(MVSIZ), 
     .   ESPE(MVSIZ), E01(MVSIZ), 
     .   DTDE, DPDR, SPH, RL, TL, ZL, DXDR, ZZ, DELTAT, XLAMB, 
     .   STEFAN, OPAC, ROSSEL, XKR
      REAL*8 NE, NA, ATOM
C-----------------------------------------------
      DATA NA /6.0225E+23/
C---------------------------------
      DO I=1,NEL
        ZL = ZERO
        DXDR = ZERO
        MX = MAT(I)
        DPDM(I) = FOUR_OVER_3 * PM(22,MX)
        E01(I) =EINT(I)-(POLD(I)+QOLD(I))*DV2(I)
        E01(I) = MAX(ZERO,E01(I))
        ESPE(I)=E01(I)/MAX(EM30,VOLN(I)*RHO(I))
        NR  = NINT(PM(25,MX))
        NT  = NINT(PM(26,MX))
        IDR = NINT(PM(27,MX))
        IDT = IDR + NR
        IDP = IDT + NT
        IDE = IDP + NR * NT

        CALL MINTP_RE(SESAME(IDR),NR,SESAME(IDT),NT,SESAME(IDE),RHO(I),TEMP(I),ESPE(I),DTDE)
        DTDE = MAX (ZERO,DTDE)
        CALL MINTP_RT(SESAME(IDR),NR,SESAME(IDT),NT,SESAME(IDP),RHO(I),TEMP(I),P01(I),DPDR)

        DPDM(I)=DPDM(I)+RHO0(I)*DPDR
        SPH = RHO(I)/MAX(EM15,DTDE)
        !------------------------------------
        !     IONISATION
        !------------------------------------
        NR  = NINT(PM(28,MX))
        NT  = NINT(PM(29,MX))
        IDR = NINT(PM(30,MX))
        IDT = IDR + NR
        IDQ = IDT + NT
        RL = LOG10(RHO(I))
        TL = LOG10(TEMP(I))
        CALL MINTP_RT(SESAME(IDR),NR,SESAME(IDT),NT,SESAME(IDQ),RL,TL,ZL,DXDR)
        IF(ZL <= -20)THEN
          !machine epsilon
          Z(I)=EM20
        ELSEIF(ZL >= 20)THEN
          !machine infinity
          Z(I)=EP20
        ELSE
          Z(I)=EXP(ZL*LOG(TEN)) !TEN**ZL
        ENDIF  

        !(CONDUNCTIVITE MAX POUR CALCUL DE DT)
        ZZ = MAX(EM10,Z(I))
        DELTAT= THREEP44 * ZEP26 * LOG(ZZ) / ZZ
        IF(DELTAT > ZERO)THEN
          DELTAT= ONE / ( ONE +  DELTAT)
        ELSE
          DELTAT= ZERO
        ENDIF
        ATOM  = PM(37,MX)
        NE    = RHO(I)*NA*ZZ/ATOM
        XLAMB = PM(36,MX)*TEMP(I)**THREE_HALF/SQRT(NE)
        XLAMB = MAX(ONE,XLAMB)
        XLAMB = MAX(EM10, LOG(XLAMB))
        XK(I) = ZEP4*DELTAT*PM(35,MX) * TEMP(I)**TWOP5 / (ZZ*XLAMB)
        STEFAN = PM(51,MX)
        IF(STEFAN > ZERO.AND.TEMP(I) >= EP04)THEN
          NR     = NINT(PM(48,MX))
          NT     = NINT(PM(49,MX))
          IDR    = NINT(PM(50,MX))
          IDT = IDR + NR
          IDQ = IDT + NT
          RL = LOG10(RHO(I))
          TL = LOG10(TEMP(I))
          CALL MINTP_RT(SESAME(IDR),NR,SESAME(IDT),NT,SESAME(IDQ),RL,TL,OPAC,DXDR)
          OPAC = TEN**OPAC
          ROSSEL = ONE / ( RHO(I) * OPAC)
          XKR = SIXTEEN * STEFAN * TEMP(I)**3 * ROSSEL * THIRD
          XK(I) = XK(I) + XKR
        ENDIF
        XK(I) = MIN(XK(I),PM(51,MX))
      ENDDO
      
      !------------------------------------
      !     VITESSE DU SON
      !------------------------------------
      DO I=1,NEL
        SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
        SOUND(I) = SSP(I)
      ENDDO
C
      RETURN
      END
