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
Chd|  R2DEF3                        source/elements/spring/r2def3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        REPLA3                        source/elements/spring/repla3.F
Chd|====================================================================
      SUBROUTINE R2DEF3(
     1   SKEW,    GEO,     FX,      FY,
     2   FZ,      E,       DX,      DY,
     3   DZ,      NPF,     TF,      OFF,
     4   DPX,     DPY,     DPZ,     DPX2,
     5   DPY2,    DPZ2,    FXEP,    FYEP,
     6   FZEP,    X0,      Y0,      Z0,
     7   XMOM,    YMOM,    ZMOM,    RX,
     8   RY,      RZ,      RPX,     RPY,
     9   RPZ,     XMEP,    YMEP,    ZMEP,
     A   RPX2,    RPY2,    RPZ2,    ANIM,
     B   IPOSX,   IPOSY,   IPOSZ,   IPOSXX,
     C   IPOSYY,  IPOSZZ,  FR_WAVE, V,
     D   IGEO,    E6,      CRITNEW, NEL,
     E   X0_ERR,  X1DP,    X2DP,    YIELDX,
     F   YIELDY,  YIELDZ,  YIELDX2, YIELDY2,
     G   YIELDZ2, NGL,     XKR,     MGN,
     H   EXX,     EYX,     EZX,     EXY,
     I   EYY,     EZY,     EXZ,     EYZ,
     J   EZZ,     XCR,     RX1,     RY1,
     K   RZ1,     RX2,     RY2,     RZ2,
     L   XIN,     AK,      XM,      XKM,
     M   XCM,     NC1,     NC2,     NUVAR,
     N   UVAR,    DX0,     DY0,     DZ0,
     O   RX0,     RY0,     RZ0,     IEQUIL,
     P   SKEW_ID, NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER NPF(*), IGEO(NPROPGI,*),NEL,NGL(*),MGN(*),NC1(*),NC2(*),NUVAR,IEQUIL(*),SKEW_ID(*)
C     REAL
      my_real
     .   SKEW(LSKEW,*), GEO(NPROPG,*), FX(*), FY(*), FZ(*), E(*), DX(*),
     .   DY(*), DZ(*), TF(*), OFF(*), DPX(*), DPY(*), DPZ(*), FXEP(*),
     .   FYEP(*), FZEP(*), X0(*), Y0(*), Z0(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY(*), RZ(*), RPX(*), RPY(*), RPZ(*), XMEP(*),
     .   YMEP(*), ZMEP(*), DPX2(*), DPY2(*), DPZ2(*), RPX2(*), RPY2(*),
     .   RPZ2(*), ANIM(*),IPOSX(*),IPOSY(*),IPOSZ(*),IPOSXX(*),
     .   IPOSYY(*),IPOSZZ(*),FR_WAVE(*),V(3,*),
     .   CRITNEW(*),E6(NEL,6),X0_ERR(3,*),YIELDX(*),YIELDY(*) ,
     .   YIELDZ(*),YIELDX2(*),YIELDY2(*),YIELDZ2(*),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ),
     .   EXY(MVSIZ), EYY(MVSIZ), EZY(MVSIZ),
     .   EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ),
     .   XCR(MVSIZ),RX1(MVSIZ),RX2(MVSIZ),RY1(MVSIZ),
     .   RY2(MVSIZ),RZ1(MVSIZ),RZ2(MVSIZ),XIN(MVSIZ),
     .   AK(MVSIZ),XM(MVSIZ),XKM(MVSIZ),XCM(MVSIZ),XKR(MVSIZ),
     .   UVAR(NUVAR,*),DX0(*),DY0(*),DZ0(*),RX0(*),RY0(*),RZ0(*) 
      DOUBLE PRECISION X1DP(3,*),X2DP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC2(MVSIZ),
     .        IECROU(MVSIZ), IFUNC(MVSIZ), IFV(MVSIZ),
     .        INDX(MVSIZ),IFUNC3(MVSIZ),
     .        I,J,ISK, KK,NINDX,IFAIL(MVSIZ),IFAIL2(MVSIZ),ISRATE
C     REAL
      my_real
     .     XK(MVSIZ), YK(MVSIZ), ZK(MVSIZ),
     .     XC(MVSIZ), YC(MVSIZ), ZC(MVSIZ),
     .     XHR(MVSIZ),XH(MVSIZ),
     .     DXOLD(MVSIZ), DYOLD(MVSIZ), DZOLD(MVSIZ),DV(MVSIZ),
     .     EPLA(MVSIZ),XL0(MVSIZ),RSCALE(MVSIZ),
     .     B(MVSIZ), D(MVSIZ),DMN(MVSIZ),DMX(MVSIZ),CRIT(MVSIZ),
     .     X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ),LSCALE(MVSIZ),EE(MVSIZ),
     .     GF3(MVSIZ),HX(MVSIZ), HY(MVSIZ), HZ(MVSIZ),
     .     X0_INI(MVSIZ),Y0_INI(MVSIZ),Z0_INI(MVSIZ)
      my_real
     .     SX,SY,SZ,XX,YY,ZZ,XKA,YKA,ZKA,AA,BB,CC,X21PHI,Y21PHI,Z21PHI,
     .     ASRATE,DLIM,NOT_USED
      DOUBLE PRECISION X21DP(MVSIZ),Y21DP(MVSIZ),Z21DP(MVSIZ),
     .                 X0DP(MVSIZ),Y0DP(MVSIZ),Z0DP(MVSIZ)
C-----------------------------------------------
C
      NOT_USED = ZERO
C
      DO I=1,NEL
        EPLA(I)=ZERO
        XM(I)=GEO(1,MGN(I))
        XK(I)=GEO(3,MGN(I))
        XC(I)=GEO(4,MGN(I))
        YK(I)=GEO(10,MGN(I))
        YC(I)=GEO(11,MGN(I))
        ZK(I)=GEO(15,MGN(I))
        ZC(I)=GEO(16,MGN(I))
        IFAIL(I) =   NINT(GEO(79, MGN(I)))
        IFAIL2(I) =  NINT(GEO(95, MGN(I)))
        XKA=XK(I)*GEO(41,MGN(I))
        YKA=YK(I)*GEO(45,MGN(I))
        ZKA=ZK(I)*GEO(49,MGN(I))
        XKM(I)= MAX(XKA,YKA,ZKA)
        HX(I) = GEO(141,MGN(I))
        HY(I) = GEO(142,MGN(I))
        HZ(I) = GEO(143,MGN(I))
        XH(I)= MAX(HX(I),HY(I),HZ(I))
        XCM(I)= MAX(XC(I),YC(I),ZC(I))
        XCM(I)= XCM(I)+XH(I)

        ISK=SKEW_ID(I)
        EXX(I)=SKEW(1,ISK)
        EYX(I)=SKEW(2,ISK)
        EZX(I)=SKEW(3,ISK)
        EXY(I)=SKEW(4,ISK)
        EYY(I)=SKEW(5,ISK)
        EZY(I)=SKEW(6,ISK)
        EXZ(I)=SKEW(7,ISK)
        EYZ(I)=SKEW(8,ISK)
        EZZ(I)=SKEW(9,ISK)
        XL0(I)=ONE
        IEQUIL(I) = NINT(GEO(94,MGN(I)))
      ENDDO
C---------------------
C     TRANSLATIONS
C---------------------
      DO I=1,NEL
        DXOLD(I)=DX(I)
        DYOLD(I)=DY(I)
        DZOLD(I)=DZ(I)
      ENDDO
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          DXOLD(I)=DX0(I)
          DYOLD(I)=DY0(I)
          DZOLD(I)=DZ0(I)
        ENDDO
      ENDIF
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          X0_INI(I)=X0(I)
          Y0_INI(I)=Y0(I)
          Z0_INI(I)=Z0(I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        X21DP(I)= X2DP(1,I)-X1DP(1,I)
        Y21DP(I)= X2DP(2,I)-X1DP(2,I)
        Z21DP(I)= X2DP(3,I)-X1DP(3,I)
        X21(I)= X21DP(I)
        Y21(I)= Y21DP(I)
        Z21(I)= Z21DP(I)
      ENDDO
C
      IF (TT == ZERO) THEN
        DO I=1,NEL
          X0DP(I)= X21DP(I)*EXX(I)+Y21DP(I)*EYX(I)+Z21DP(I)*EZX(I)
          Y0DP(I)= X21DP(I)*EXY(I)+Y21DP(I)*EYY(I)+Z21DP(I)*EZY(I)
          Z0DP(I)= X21DP(I)*EXZ(I)+Y21DP(I)*EYZ(I)+Z21DP(I)*EZZ(I)
          X0(I)= X0DP(I)                 ! cast double to My_real
          Y0(I)= Y0DP(I)                 ! cast double to My_real
          Z0(I)= Z0DP(I)                 ! cast double to My_real
        ENDDO
!
        IF (INISPRI /= 0) THEN
!  condition nedeed for spring type 8, which are not concerned by /INISPRI,
!  and having initial length /= 0 
          DO I=1,NEL
            IF (X0_INI(I) == ZERO .and. DX0(I) == ZERO) X0_INI(I) = X0DP(I)
            IF (Y0_INI(I) == ZERO .and. DY0(I) == ZERO) Y0_INI(I) = Y0DP(I)
            IF (Z0_INI(I) == ZERO .and. DZ0(I) == ZERO) Z0_INI(I) = Z0DP(I)
          ENDDO
        ENDIF
!
      ENDIF ! IF (TT == ZERO)
C
      IF (SCODVER >= 101) THEN
        IF (TT == ZERO) THEN
          DO I=1,NEL
            X0_ERR(1,I)= X0DP(I)-X0(I)   ! difference between double and my_real
            X0_ERR(2,I)= Y0DP(I)-Y0(I)   ! difference between double and my_real
            X0_ERR(3,I)= Z0DP(I)-Z0(I)   ! difference between double and my_real
          ENDDO
        ENDIF
      ENDIF
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          X0(I)=X0_INI(I)
          Y0(I)=Y0_INI(I)
          Z0(I)=Z0_INI(I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        X0DP(I)= X0(I)                 ! cast My_real en double
        Y0DP(I)= Y0(I)                 ! cast My_real en double
        Z0DP(I)= Z0(I)                 ! cast My_real en double
      ENDDO
C
      IF (SCODVER >= 101) THEN
        DO I=1,NEL
          X0DP(I)= X0DP(I) + X0_ERR(1,I) ! AL_DP must be recomputed to be sure of absolute consistency between AL0_DP and AL_DP
          Y0DP(I)= Y0DP(I) + X0_ERR(2,I) ! AL_DP must be recomputed to be sure of absolute consistency between AL0_DP and AL_DP
          Z0DP(I)= Z0DP(I) + X0_ERR(3,I) ! AL_DP must be recomputed to be sure of absolute consistency between AL0_DP and AL_DP
        ENDDO
      ENDIF
C
      IF (ISMDISP > 0) THEN
        DO I=1,NEL
          IF (IEQUIL(I) == 1) THEN
            SX= HALF*(RX2(I)+RX1(I))
            SY= HALF*(RY2(I)+RY1(I))
            SZ= HALF*(RZ2(I)+RZ1(I))
            XX = Y21(I)*SZ - Z21(I)*SY
            YY = Z21(I)*SX - X21(I)*SZ
            ZZ = X21(I)*SY - Y21(I)*SX
            XX= (V(1,NC2(I)) - V(1,NC1(I)) + XX)*DT1
            YY= (V(2,NC2(I)) - V(2,NC1(I)) + YY)*DT1
            ZZ= (V(3,NC2(I)) - V(3,NC1(I)) + ZZ)*DT1
          ELSE
            XX= (V(1,NC2(I)) - V(1,NC1(I)))*DT1
            YY= (V(2,NC2(I)) - V(2,NC1(I)))*DT1
            ZZ= (V(3,NC2(I)) - V(3,NC1(I)))*DT1
          ENDIF
          DX(I) = DXOLD(I)+XX*EXX(I)+YY*EYX(I)+ZZ*EZX(I)
          DY(I) = DYOLD(I)+XX*EXY(I)+YY*EYY(I)+ZZ*EZY(I)
          DZ(I) = DZOLD(I)+XX*EXZ(I)+YY*EYZ(I)+ZZ*EZZ(I)
C
          CRIT(I) = ZERO
        ENDDO
      ELSE
        DO I=1,NEL
          IF (IEQUIL(I) == 1) THEN
            SX= HALF*(RX2(I)+RX1(I))
            SY= HALF*(RY2(I)+RY1(I))
            SZ= HALF*(RZ2(I)+RZ1(I))
            XX = Y21(I)*SZ - Z21(I)*SY
            YY = Z21(I)*SX - X21(I)*SZ
            ZZ = X21(I)*SY - Y21(I)*SX
            XX= (V(1,NC2(I)) - V(1,NC1(I)) + XX)*DT1
            YY= (V(2,NC2(I)) - V(2,NC1(I)) + YY)*DT1
            ZZ= (V(3,NC2(I)) - V(3,NC1(I)) + ZZ)*DT1
            DX(I)= DXOLD(I) + XX*EXX(I)+YY*EYX(I)+ZZ*EZX(I)
            DY(I)= DYOLD(I) + XX*EXY(I)+YY*EYY(I)+ZZ*EZY(I)
            DZ(I)= DZOLD(I) + XX*EXZ(I)+YY*EYZ(I)+ZZ*EZZ(I)
          ELSE
            DX(I)= X21DP(I)*EXX(I)+Y21DP(I)*EYX(I)+Z21DP(I)*EZX(I)-X0DP(I)
            DY(I)= X21DP(I)*EXY(I)+Y21DP(I)*EYY(I)+Z21DP(I)*EZY(I)-Y0DP(I)
            DZ(I)= X21DP(I)*EXZ(I)+Y21DP(I)*EYZ(I)+Z21DP(I)*EZZ(I)-Z0DP(I)
          ENDIF
          CRIT(I) = ZERO
        ENDDO
      ENDIF !(ISMDISP > 0) THEN
C-------------------------------
      NINDX = 0
      DO I=1,NEL
        IFUNC(I) = IGEO(101,MGN(I))
        IFV(I)   = IGEO(102,MGN(I))
        IFUNC2(I)= IGEO(103,MGN(I))
        IFUNC3(I)= IGEO(119,MGN(I))
        IECROU(I)= NINT(GEO(7,MGN(I)))
        AK(I)    = GEO(41,MGN(I))
        B(I)     = GEO(42,MGN(I))
        D(I)     = GEO(43,MGN(I))
        EE(I)    = GEO(40 ,MGN(I))
        GF3(I)   = GEO(132,MGN(I))
        RSCALE(I)= GEO(44,MGN(I))
        LSCALE(I)= GEO(39 ,MGN(I))
        DMN(I)   = GEO(65,MGN(I))
        DMX(I)   = GEO(66,MGN(I))
      ENDDO
      CALL REDEF3(
     1   FX,         XK,         DX,         FXEP,
     2   DXOLD,      DPX,        TF,         NPF,
     3   XC,         OFF,        E6(1,1),    DPX2,
     4   ANIM,       ANIM_FE(11),IPOSX,      FR_WAVE,
     5   XL0,        DMN,        DMX,        DV,
     6   RSCALE,     LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDX,     X0DP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       UVAR(1,1),
     A   NOT_USED,   NOT_USED,   NOT_USED,   NEL,
     B   NFT)
      DO I=1,NEL
        DLIM = ZERO
        IF (IFAIL2(I) == 0) THEN
          IF (DX(I) > ZERO) THEN
            DLIM = DX(I) / DMX(I)
          ELSE
            DLIM = DX(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 1) THEN
          IF (FX(I) > ZERO) THEN
             DLIM = FX(I) / DMX(I)
          ELSE
             DLIM = FX(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 2) THEN
          DLIM = MAX(ZERO, E6(I,1)) / DMX(I)
        ENDIF
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF( IFAIL(I) == 0 ) THEN
!        Uniaxial failure
           CRIT(I) = MAX(CRIT(I),DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
      DO I=1,NEL
        IFUNC(I) = IGEO(104,MGN(I))
        IFV(I)   = IGEO(105,MGN(I))
        IFUNC2(I)= IGEO(106,MGN(I))
        IFUNC3(I)= IGEO(120,MGN(I))
        IECROU(I)= NINT(GEO(14,MGN(I)))
        AK(I)    = GEO(45,MGN(I))
        B(I)     = GEO(46,MGN(I))
        D(I)     = GEO(47,MGN(I))
        DMN(I)   = GEO(67,MGN(I))
        DMX(I)   = GEO(68,MGN(I))
        EE(I)   =GEO(180,MGN(I))
        GF3(I)  =GEO(133,MGN(I))
        RSCALE(I)= GEO(48,MGN(I))
        LSCALE(I)= GEO(174,MGN(I))
      ENDDO
      KK = 1 + NUMELR * ANIM_FE(11)
      CALL REDEF3(
     1   FY,         YK,         DY,         FYEP,
     2   DYOLD,      DPY,        TF,         NPF,
     3   YC,         OFF,        E6(1,2),    DPY2,
     4   ANIM(KK),   ANIM_FE(12),IPOSY,      FR_WAVE,
     5   XL0,        DMN,        DMX,        DV,
     6   RSCALE,     LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDY,     Y0DP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       UVAR(2,1),
     A   NOT_USED,   NOT_USED,   NOT_USED,   NEL,
     B   NFT)
      DO I=1,NEL
        DLIM = ZERO
        IF (IFAIL2(I) == 0 ) THEN
          IF (DY(I) > ZERO) THEN
            DLIM = DY(I) / DMX(I)
          ELSE
            DLIM = DY(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 1) THEN
          IF (FY(I) > ZERO) THEN
            DLIM = FY(I) / DMX(I)
          ELSE
            DLIM = FY(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 2) THEN
          DLIM = MAX(ZERO, E6(I,2)) / DMX(I)
        ENDIF
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL(I) == 0) THEN
!        Uniaxial failure
           CRIT(I) = MAX(CRIT(I),DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
      DO I=1,NEL
        IFUNC(I) = IGEO(107,MGN(I))
        IFV(I)   = IGEO(108,MGN(I))
        IFUNC2(I)= IGEO(109,MGN(I))
        IFUNC3(I)= IGEO(121,MGN(I))
        IECROU(I)=NINT(GEO(18,MGN(I)))
        AK(I)   =GEO(49,MGN(I))
        B(I)    =GEO(50,MGN(I))
        D(I)    =GEO(51,MGN(I))
        EE(I)   =GEO(181,MGN(I))
        GF3(I)  =GEO(134,MGN(I))
        RSCALE(I)= GEO(52,MGN(I))
        LSCALE(I)=GEO(175,MGN(I))
        DMN(I)  =GEO(69,MGN(I))
        DMX(I)  =GEO(77,MGN(I))
      ENDDO
      KK = 1 + NUMELR * (ANIM_FE(11)+ANIM_FE(12))
      CALL REDEF3(
     1   FZ,         ZK,         DZ,         FZEP,
     2   DZOLD,      DPZ,        TF,         NPF,
     3   ZC,         OFF,        E6(1,3),    DPZ2,
     4   ANIM(KK),   ANIM_FE(13),IPOSZ,      FR_WAVE,
     5   XL0,        DMN,        DMX,        DV,
     6   RSCALE,     LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDZ,     Z0DP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       UVAR(3,1),
     A   NOT_USED,   NOT_USED,   NOT_USED,   NEL,
     B   NFT)
      DO I=1,NEL
        DLIM = ZERO
        IF (IFAIL2(I) == 0 ) THEN
          IF (DZ(I) > ZERO) THEN
            DLIM = DZ(I) / DMX(I)
          ELSE
            DLIM = DZ(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 1) THEN
          IF (FZ(I) > ZERO) THEN
            DLIM = FZ(I) / DMX(I)
          ELSE
            DLIM = FZ(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 2) THEN
          DLIM = MAX(ZERO, E6(I,3)) / DMX(I)
        ENDIF
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL(I) == 0) THEN
!        Uniaxial failure
           CRIT(I) = MAX(CRIT(I),DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
C---------------------
C     ROTATIONS
C---------------------
      DO I=1,NEL
        XIN(I)=GEO(9,MGN(I))
        XK(I)=GEO(19,MGN(I))
        XC(I)=GEO(20,MGN(I))
        YK(I)=GEO(23,MGN(I))
        YC(I)=GEO(24,MGN(I))
        ZK(I)=GEO(27,MGN(I))
        ZC(I)=GEO(28,MGN(I))
        HX(I) =  GEO(144,MGN(I))
        HY(I) =  GEO(145,MGN(I))
        HZ(I) =  GEO(146,MGN(I))

        XHR(I)= MAX(HX(I),HY(I),HZ(I))
        XKR(I)= MAX(XK(I)*GEO(53,MGN(I)),
     .              YK(I)*GEO(57,MGN(I)),
     .              ZK(I)*GEO(61,MGN(I)))
        XCR(I)= MAX(XC(I),YC(I),ZC(I)) + XHR(I)
      ENDDO
C
      DO I=1,NEL
        DXOLD(I)=RX(I)
        DYOLD(I)=RY(I)
        DZOLD(I)=RZ(I)
      ENDDO
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          DXOLD(I)=RX0(I)
          DYOLD(I)=RY0(I)
          DZOLD(I)=RZ0(I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        X21(I)= (RX2(I)-RX1(I))*DT1
        Y21(I)= (RY2(I)-RY1(I))*DT1
        Z21(I)= (RZ2(I)-RZ1(I))*DT1
        RX(I)= DXOLD(I)+X21(I)*EXX(I)+Y21(I)*EYX(I)+Z21(I)*EZX(I)
        RY(I)= DYOLD(I)+X21(I)*EXY(I)+Y21(I)*EYY(I)+Z21(I)*EZY(I)
        RZ(I)= DZOLD(I)+X21(I)*EXZ(I)+Y21(I)*EYZ(I)+Z21(I)*EZZ(I)
      ENDDO
C-------------------------------
      DO I=1,NEL
        IFUNC(I) = IGEO(110,MGN(I))
        IFV(I)   = IGEO(111,MGN(I))
        IFUNC2(I)= IGEO(112,MGN(I))
        IFUNC3(I)= IGEO(122,MGN(I))
        IECROU(I)=NINT(GEO(22,MGN(I)))
        AK(I)   =GEO(53,MGN(I))
        B(I)    =GEO(54,MGN(I))
        D(I)    =GEO(55,MGN(I))
        EE(I)   =GEO(182,MGN(I))
        GF3(I)  =GEO(135,MGN(I))
        RSCALE(I)= GEO(56,MGN(I))
        LSCALE(I)= GEO(176,MGN(I))
        DMN(I)  =GEO(71,MGN(I))
        DMX(I)  =GEO(72,MGN(I))
      ENDDO
      CALL REDEF3(
     1   XMOM,     XK,       RX,       XMEP,
     2   DXOLD,    RPX,      TF,       NPF,
     3   XC,       OFF,      E6(1,4),  RPX2,
     4   ANIM,     0,        IPOSXX,   FR_WAVE,
     5   XL0,      DMN,      DMX,      DV,
     6   RSCALE,   LSCALE,   EE,       GF3,
     7   IFUNC3,   YIELDX2,  X0DP,     AK,
     8   B,        D,        IECROU,   IFUNC,
     9   IFV,      IFUNC2,   EPLA,     UVAR(4,1),
     A   NOT_USED, NOT_USED, NOT_USED, NEL,
     B   NFT)
      DO I=1,NEL
        DLIM = ZERO
        IF (IFAIL2(I) == 0) THEN
          IF (RX(I) > ZERO) THEN
            DLIM = RX(I) / DMX(I)
          ELSE
            DLIM = RX(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 1) THEN
          IF (XMOM(I) > ZERO) THEN
            DLIM = XMOM(I) / DMX(I)
          ELSE
            DLIM = XMOM(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 2) THEN
          DLIM = MAX(ZERO, E6(I,4)) / DMX(I)
        ENDIF
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL(I) == 0) THEN
!        Uniaxial failure
           CRIT(I) = MAX(CRIT(I),DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
C-------------------------------------
      DO I=1,NEL
        IFUNC(I) = IGEO(113,MGN(I))
        IFV(I)   = IGEO(114,MGN(I))
        IFUNC2(I)= IGEO(115,MGN(I))
        IFUNC3(I)= IGEO(123,MGN(I))
        IECROU(I)=NINT(GEO(26,MGN(I)))
        AK(I)   =GEO(57,MGN(I))
        B(I)    =GEO(58,MGN(I))
        D(I)    =GEO(59,MGN(I))
        EE(I)= GEO(183,MGN(I))
        GF3(I)= GEO(136,MGN(I))
        RSCALE(I)= GEO(60,MGN(I))
        LSCALE(I)= GEO(177,MGN(I))
        DMN(I)  =GEO(73,MGN(I))
        DMX(I)  =GEO(74,MGN(I))
      ENDDO
      CALL REDEF3(
     1   YMOM,     YK,       RY,       YMEP,
     2   DYOLD,    RPY,      TF,       NPF,
     3   YC,       OFF,      E6(1,5),  RPY2,
     4   ANIM,     0,        IPOSYY,   FR_WAVE,
     5   XL0,      DMN,      DMX,      DV,
     6   RSCALE,   LSCALE,   EE,       GF3,
     7   IFUNC3,   YIELDY2,  Y0DP,     AK,
     8   B,        D,        IECROU,   IFUNC,
     9   IFV,      IFUNC2,   EPLA,     UVAR(5,1),
     A   NOT_USED, NOT_USED, NOT_USED, NEL,
     B   NFT)
      DO I=1,NEL
        DLIM = ZERO
        IF (IFAIL2(I) == 0) THEN
          IF (RY(I) > ZERO) THEN
            DLIM = RY(I) / DMX(I)
          ELSE
            DLIM = RY(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 1) THEN
          IF (YMOM(I) > ZERO) THEN
            DLIM = YMOM(I) / DMX(I)
          ELSE
            DLIM = YMOM(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 2) THEN
          DLIM = MAX(ZERO,E6(I,5)) / DMX(I)
        ENDIF
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL(I) == 0) THEN
!        Uniaxial failure
           CRIT(I) = MAX(CRIT(I),DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I) = CRIT(I)  +  DLIM**2
          ENDIF
        ENDIF
      ENDDO
C-----------------------------------
      DO I=1,NEL
        IFUNC(I) = IGEO(116,MGN(I))
        IFV(I)   = IGEO(117,MGN(I))
        IFUNC2(I)= IGEO(118,MGN(I))
        IFUNC3(I)= IGEO(124,MGN(I))
        IECROU(I)=NINT(GEO(30,MGN(I)))
        AK(I)   =GEO(61,MGN(I))
        B(I)    =GEO(62,MGN(I))
        D(I)    =GEO(63,MGN(I))
        EE(I)   =GEO(184,MGN(I))
        GF3(I)  =GEO(137,MGN(I))
        RSCALE(I)= GEO(64,MGN(I))
        LSCALE(I)= GEO(178,MGN(I))
        DMN(I)  =GEO(75,MGN(I))
        DMX(I)  =GEO(76,MGN(I))
      ENDDO
      CALL REDEF3(
     1   ZMOM,     ZK,       RZ,       ZMEP,
     2   DZOLD,    RPZ,      TF,       NPF,
     3   ZC,       OFF,      E6(1,6),  RPZ2,
     4   ANIM,     0,        IPOSZZ,   FR_WAVE,
     5   XL0,      DMN,      DMX,      DV,
     6   RSCALE,   LSCALE,   EE,       GF3,
     7   IFUNC3,   YIELDZ2,  Z0DP,     AK,
     8   B,        D,        IECROU,   IFUNC,
     9   IFV,      IFUNC2,   EPLA,     UVAR(6,1),
     A   NOT_USED, NOT_USED, NOT_USED, NEL,
     B   NFT)
      DO I=1,NEL
        DLIM = ZERO
        IF (IFAIL2(I) == 0) THEN
          IF (RZ(I) > ZERO) THEN
            DLIM = RZ(I) / DMX(I)
          ELSE
            DLIM = RZ(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 1) THEN
          IF (ZMOM(I) > ZERO) THEN
            DLIM = ZMOM(I) / DMX(I)
          ELSE
            DLIM = ZMOM(I) / DMN(I)
          ENDIF
        ELSEIF (IFAIL2(I) == 2) THEN
          DLIM = MAX(ZERO,E6(I,6)) / DMX(I)
        ENDIF
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL(I) == 0) THEN
!        Uniaxial failure
           CRIT(I) = MAX(CRIT(I),DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
C
C-------------------------------
C    COUPLED FAILURE
C-------------------------------
      DO I=1,NEL
        ISRATE = NINT(GEO(96, MGN(I)))
C---- smoothing factor alpha = 2PI*fc*dt/(2PI*fc*dt+1) ---
        ASRATE = (2*PI*GEO(97,MGN(I))*DT1)/(ONE+2*PI*GEO(97,MGN(I))*DT1)
        IF (ISRATE /= 0) THEN
          IF (CRITNEW(I) < ONE) THEN 
            CRIT(I) = MIN(CRIT(I),ONE+EM3)
            CRIT(I) = ASRATE*CRIT(I) + (ONE - ASRATE)*CRITNEW(I)
            CRITNEW(I) = MIN(CRIT(I),ONE)
          ELSE
            CRITNEW(I) = ONE
          ENDIF
        ELSE
          IF (CRITNEW(I) < ONE) THEN 
            CRITNEW(I) = MIN(CRIT(I),ONE)
          ELSE
            CRITNEW(I) = ONE
          ENDIF
        ENDIF
        IF (OFF(I) == ONE .AND. CRIT(I) >= ONE) THEN
          OFF(I)=ZERO
          NINDX = NINDX + 1
          INDX(NINDX) = I
          IDEL7NOK = 1
        ENDIF
      ENDDO
C
      DO J=1,NINDX
        I = INDX(J)
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(I)
        WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
      ENDDO
C-------------------------------
C     COUPLED PLASTICITY
C-------------------------------
      CALL REPLA3(
     1   XK,      RPX,     TF,      NPF,
     2   IECROU,  IFUNC,   IFV,     EPLA,
     3   NEL)
      CALL REPLA3(
     1   YK,      RPY,     TF,      NPF,
     2   IECROU,  IFUNC,   IFV,     EPLA,
     3   NEL)
      CALL REPLA3(
     1   ZK,      RPZ,     TF,      NPF,
     2   IECROU,  IFUNC,   IFV,     EPLA,
     3   NEL)
C
      DO I=1,NEL
        XK(I)=GEO(3,MGN(I))
        YK(I)=GEO(10,MGN(I))
        ZK(I)=GEO(15,MGN(I))
        E(I) = E6(I,1)+E6(I,2)+E6(I,3)+E6(I,4)+E6(I,5)+E6(I,6)
      ENDDO
C
      CALL REPLA3(
     1   XK,      DPX,     TF,      NPF,
     2   IECROU,  IFUNC,   IFV,     EPLA,
     3   NEL)
      CALL REPLA3(
     1   YK,      DPY,     TF,      NPF,
     2   IECROU,  IFUNC,   IFV,     EPLA,
     3   NEL)
      CALL REPLA3(
     1   ZK,      DPZ,     TF,      NPF,
     2   IECROU,  IFUNC,   IFV,     EPLA,
     3   NEL)
C---
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C---
      RETURN
      END
