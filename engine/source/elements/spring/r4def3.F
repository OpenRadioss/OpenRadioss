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
!||    r4def3             ../engine/source/elements/spring/r4def3.F
!||--- called by ------------------------------------------------------
!||    rforc3             ../engine/source/elements/spring/rforc3.F
!||--- calls      -----------------------------------------------------
!||    redef3             ../engine/source/elements/spring/redef3.F90
!||    repla3             ../engine/source/elements/spring/repla3.F
!||--- uses       -----------------------------------------------------
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||    redef3_mod         ../engine/source/elements/spring/redef3.F90
!||====================================================================
      SUBROUTINE R4DEF3(PYTHON,
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
     B   POSX,    POSY,    POSZ,    POSXX,
     C   POSYY,   POSZZ,   FR_WAVE, E6,
     D   NEL,     EXX2,    EYX2,    EZX2,
     E   EXY2,    EYY2,    EZY2,    EXZ2,
     F   EYZ2,    EZZ2,    AL2DP,   IGEO,
     G   CRIT_NEW,X0_ERR,  ALDP,    YIELDX,
     H   YIELDY,  YIELDZ,  YIELDX2, YIELDY2,
     I   YIELDZ2, NGL,     MGN,     EXX,
     J   EYX,     EZX,     EXY,     EYY,
     K   EZY,     EXZ,     EYZ,     EZZ,
     L   XCR,     RX1,     RY1,     RZ1,
     M   RX2,     RY2,     RZ2,     XIN,
     N   AK,      XM,      XKM,     XCM,
     O   XKR,     VX1,     VX2,     VY1,
     P   VY2,     VZ1,     VZ2,     NUVAR,
     Q   UVAR,    DX0,     DY0,     DZ0,
     R   RX0,     RY0,     RZ0,     FX0,
     S   FY0,     FZ0,     XMOM0,   YMOM0,
     T   ZMOM0,   NFT,     STF,     SANIN,
     U   IRESP,   SNPC)
         USE python_funct_mod
         USE REDEF3_MOD
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
      type(python_) :: PYTHON
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: STF !< Size of TF
      INTEGER, INTENT(IN) :: SANIN !< Size of ANIM
      INTEGER, INTENT(IN) :: IRESP !< Single Precision flag
      INTEGER, INTENT(IN) :: SNPC   !< Size of NPF
      INTEGER NPF(SNPC),IGEO(NPROPGI,*),NEL,NGL(*),MGN(*),NUVAR
C     REAL
      my_real
     .   SKEW(LSKEW,*), GEO(NPROPG,*), FX(*), FY(*), FZ(*), E(*), DX(*),
     .   DY(*), DZ(*), TF(STF), OFF(*), DPX(*), DPY(*), DPZ(*), FXEP(*),
     .   FYEP(*), FZEP(*), X0(*), Y0(*), Z0(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY(*), RZ(*), RPX(*), RPY(*), RPZ(*), XMEP(*),
     .   YMEP(*), ZMEP(*), DPX2(*), DPY2(*), DPZ2(*),RPX2(*), RPY2(*),
     .   RPZ2(*),ANIM(SANIN),POSX(*),POSY(*),POSZ(*),POSXX(*),
     .   POSYY(*),POSZZ(*),FR_WAVE(*),E6(NEL,6),
     .   EXX2(MVSIZ), EYX2(MVSIZ), EZX2(MVSIZ),
     .   EXY2(MVSIZ), EYY2(MVSIZ), EZY2(MVSIZ),
     .   EXZ2(MVSIZ), EYZ2(MVSIZ), EZZ2(MVSIZ),
     .   CRIT_NEW(*), X0_ERR(MVSIZ),YIELDX(*),YIELDY(*),
     .   YIELDZ(*),YIELDX2(*),YIELDY2(*),YIELDZ2(*),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ), EXY(MVSIZ),
     .   EYY(MVSIZ), EZY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   EZZ(MVSIZ), XCR(MVSIZ), RX1(MVSIZ), RX2(MVSIZ),
     .   RY1(MVSIZ), RY2(MVSIZ), RZ1(MVSIZ), RZ2(MVSIZ),
     .   XIN(MVSIZ),AK(MVSIZ),XM(MVSIZ),XKM(MVSIZ),XCM(MVSIZ),
     .   XKR(MVSIZ),VX1(MVSIZ),VX2(MVSIZ),VY1(MVSIZ),VY2(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ),UVAR(NUVAR,*),DX0(*),DY0(*),DZ0(*),
     .   RX0(*),RY0(*),RZ0(*),FX0(*),FY0(*),FZ0(*),XMOM0(*),YMOM0(*),ZMOM0(*)
            DOUBLE PRECISION ALDP(MVSIZ),AL2DP(MVSIZ)
      TARGET :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX(MVSIZ),
     .        IECROU(MVSIZ), IFUNC(MVSIZ), IFV(MVSIZ), IFUNC2(MVSIZ),
     .        I, ILENG, J, KK, IFAIL(MVSIZ),IFAIL2(MVSIZ),
     .        NINDX,ISRATE, IFUNC3(MVSIZ)
C     REAL
      my_real
     .     XK(MVSIZ), YK(MVSIZ),
     .     ZK(MVSIZ),XC(MVSIZ), YC(MVSIZ), ZC(MVSIZ),XH(MVSIZ),
     .     XHR(MVSIZ),DXOLD(MVSIZ), DYOLD(MVSIZ), DZOLD(MVSIZ),
     .     B(MVSIZ), D(MVSIZ), EPLA(MVSIZ),
     .     DV(MVSIZ),VRT(MVSIZ),VRR(MVSIZ),
     .     DMN(MVSIZ),DMX(MVSIZ),XL0(MVSIZ),CRIT(MVSIZ),
     .     XN(MVSIZ),FF(MVSIZ),LSCALE(MVSIZ),EE(MVSIZ),GF3(MVSIZ),
     .     HX(MVSIZ), HY(MVSIZ), HZ(MVSIZ)
      my_real
     .     AT,DT05,XKA,YKA,ZKA,CC,CN,XA,XB,DLIM,VFAIL,
     .     X21, Y21, Z21, EPXY, EPXZ,
     .     VX21, VY21, VZ21, RYAVP, RZAVP,EYZP,EXZP,
     .     RYAV, RZAV,DEN, C, CP, EXYP,
     .     X21PHI, Y21PHI, Z21PHI, VX21PHI, VY21PHI, VZ21PHI,
     .     RYAV1, RZAV1, RYAV1P, RZAV1P,ASRATE,NOT_USED,NOT_USED2(2)
      DOUBLE PRECISION X0DP(MVSIZ)
      my_real ,DIMENSION(:), POINTER :: XX_OLD
      TARGET :: NOT_USED2
C-----------------------------------------------
C
      NOT_USED = ZERO
      NOT_USED2 = ZERO
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
     
       XKR(I)= MAX(YKA,ZKA) * ALDP(I) * ALDP(I)
       XCR(I)= (MAX(YC(I),ZC(I)) + MAX(HY(I),HZ(I))) * ALDP(I) * ALDP(I)
       VRT(I) = GEO(101,MGN(I))
       VRR(I) = GEO(102,MGN(I))
       IFAIL(I) = NINT(GEO(79,MGN(I)))
       IFAIL2(I)= NINT(GEO(95,MGN(I)))
      ENDDO
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          XL0(I)= X0(I)
! if not initialized length
          IF (XL0(I) == ZERO) XL0(I) = ALDP(I)
        ENDDO
      ENDIF
C
      IF (TT == ZERO) THEN
        DO I=1,NEL
          X0(I)= ALDP(I)                   ! cast double to My_real
        ENDDO
      ENDIF
C
      IF (SCODVER >= 101) THEN
        IF (TT == ZERO) THEN
          DO I=1,NEL
            X0_ERR(I)= ALDP(I)-X0(I)         ! difference between double and my_real
          ENDDO
        ENDIF
      ENDIF
C
      IF ( INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          X0(I)= XL0(I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        X0DP(I)= X0(I)                     ! cast double to My_real
      ENDDO
C
      IF (SCODVER >= 101) THEN
        DO I=1,NEL
          X0DP(I)= X0(I) + X0_ERR(I)         ! difference between double and my_real
        ENDDO
      ENDIF
C---------------------
C     TRANSLATIONS
C---------------------
      DO I=1,NEL
        DXOLD(I)=DX(I)
        DYOLD(I)=DY(I)
        DZOLD(I)=DZ(I)
      ENDDO
!
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          DXOLD(I)=DX0(I)
          DYOLD(I)=DY0(I)
          DZOLD(I)=DZ0(I)
        ENDDO
      ENDIF
C
      DT05=HALF*DT1
      IF (ISMDISP > 0) THEN
       DO I=1,NEL
        VX21  = VX2(I)-VX1(I)
        VY21  = VY2(I)-VY1(I)
        VZ21  = VZ2(I)-VZ1(I)
        DX(I) = DXOLD(I)+(VX21*EXX(I)+VY21*EYX(I)+VZ21*EZX(I))*DT1
        DY(I) = DYOLD(I)+(VX21*EXY(I)+VY21*EYY(I)+VZ21*EZY(I))*DT1
        DZ(I) = DZOLD(I)+(VX21*EXZ(I)+VY21*EYZ(I)+VZ21*EZZ(I))*DT1
C
        X21  = (RX2(I)+RX1(I))
        Y21  = (RY2(I)+RY1(I))
        Z21  = (RZ2(I)+RZ1(I))
C
        RYAV1 = (X21*EXY2(I)+Y21*EYY2(I)+Z21*EZY2(I))
        RZAV1 = (X21*EXZ2(I)+Y21*EYZ2(I)+Z21*EZZ2(I))
C
        RYAV  = DT05 * RYAV1
        RZAV  = DT05 * RZAV1
C
        DY(I) = DY(I) - RZAV * AL2DP(I)
        DZ(I) = DZ(I) + RYAV * AL2DP(I)
C
        CRIT(I) = ZERO
       ENDDO
      ELSE
       DO I=1,NEL
         VX21  = VX2(I)-VX1(I)
         VY21  = VY2(I)-VY1(I)
         VZ21  = VZ2(I)-VZ1(I)
C
         EPXY = (VX21*EXY2(I)+VY21*EYY2(I)+VZ21*EZY2(I))*DT05
         EPXZ = (VX21*EXZ2(I)+VY21*EYZ2(I)+VZ21*EZZ2(I))*DT05
C
         X21  = (RX2(I)+RX1(I))
         Y21  = (RY2(I)+RY1(I))
         Z21  = (RZ2(I)+RZ1(I))
C
         RYAV1 = (X21*EXY2(I)+Y21*EYY2(I)+Z21*EZY2(I))
         RZAV1 = (X21*EXZ2(I)+Y21*EYZ2(I)+Z21*EZZ2(I))
C
         AT=EPXZ/MAX(AL2DP(I),EM30)
         AT=ATAN(AT)
         RYAV  = DT05 * (RYAV1) + TWO * AT
         AT=EPXY/MAX(AL2DP(I),EM30)
         AT=ATAN(AT)
         RZAV  = DT05 * (RZAV1) - TWO * AT
C
         DX(I) = ALDP(I) - X0DP(I)
         DY(I) = DYOLD(I) - RZAV * AL2DP(I)
         DZ(I) = DZOLD(I) + RYAV * AL2DP(I)
C
         CRIT(I) = ZERO
       ENDDO
      ENDIF !(ISMDISP > 0) THEN
C
      DO I=1,NEL
        ILENG=NINT(GEO(93,MGN(I)))
        IF (ILENG /= 0) THEN
          XL0(I)=X0DP(I)
        ELSE
          XL0(I)=ONE
        ENDIF
      ENDDO
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
        GF3(I)   = GEO(132 ,MGN(I))
        FF(I)    = GEO(44,MGN(I))
        LSCALE(I)= GEO(39 ,MGN(I))
        DMN(I)   = GEO(65,MGN(I))
        DMX(I)   = GEO(66,MGN(I))
      ENDDO
      IF (NUVAR > 0) THEN 
         XX_OLD => UVAR(1,1:NEL) 
      ELSE 
         XX_OLD => NOT_USED2
      ENDIF 
      CALL REDEF3(PYTHON,
     1   FX,         XK,         DX,         FXEP,
     2   DXOLD,      DPX,        TF,         NPF,
     3   XC,         OFF,        E6(1,1),    DPX2,
     4   ANIM,       ANIM_FE(11),POSX,       
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDX,     ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       XX_OLD,
     A   NEL,        NFT,        STF,        SANIN ,
     B   DT1,        IRESP,      IMPL_S,     IDYNA,
     C   SNPC)
C
      DLIM = ZERO
      DO I=1,NEL
        CC = GEO(103,MGN(I))
        CN = GEO(109,MGN(I))
        XA = GEO(115,MGN(I))
        XB = GEO(121,MGN(I))
        IF (OFF(I)  == ONE .AND. DMX(I) /= ZERO .AND. DMN(I)/= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (DX(I) > ZERO) THEN
              DLIM = DX(I) / (XL0(I)*DMX(I))
            ELSE
              DLIM = DX(I) / (XL0(I)*DMN(I)) 
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRT(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (DX(I) > ZERO) THEN
                DLIM = DX(I) / (XL0(I)*DMX(I) + VFAIL)
              ELSE
                DLIM = DX(I) / (XL0(I)*DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (FX(I) > ZERO) THEN
                DLIM = FX(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = FX(I) / (DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,1)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
!         Uniaxial failure
            CRIT(I) = MAX(CRIT(I),XA*DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I)= CRIT(I) + XA*DLIM**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IFUNC(I) = IGEO(104,MGN(I))
        IFV(I)   = IGEO(105,MGN(I))
        IFUNC2(I)= IGEO(106,MGN(I))
        IFUNC3(I)= IGEO(120,MGN(I))
        IECROU(I)=NINT(GEO(14,MGN(I)))
        AK(I)   =GEO(45,MGN(I))
        B(I)    =GEO(46,MGN(I))
        D(I)    =GEO(47,MGN(I))
        EE(I)   =GEO(180,MGN(I))
        GF3(I)  =GEO(133,MGN(I))
        FF(I)   =GEO(48,MGN(I))
        LSCALE(I)= GEO(174,MGN(I))
        DMN(I)  =GEO(67,MGN(I))
        DMX(I)  =GEO(68,MGN(I))
      ENDDO
C
      KK = 1 + NUMELR * ANIM_FE(11)
      IF (NUVAR > 0) XX_OLD => UVAR(2,1:NEL)
      CALL REDEF3(PYTHON,
     1   FY,         YK,         DY,         FYEP,
     2   DYOLD,      DPY,        TF,         NPF,
     3   YC,         OFF,        E6(1,2),    DPY2,
     4   ANIM(KK),   ANIM_FE(12),POSY,       
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDY,     ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       XX_OLD,
     A   NEL,        NFT,        STF,        SANIN,
     B   DT1,        IRESP,      IMPL_S,     IDYNA,
     C   SNPC)

      DO I=1,NEL
        CC  = GEO(104,MGN(I))
        CN  = GEO(110,MGN(I))
        XA  = GEO(116,MGN(I))
        XB  = GEO(122,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (DY(I) > ZERO) THEN
              DLIM = DY(I) / (XL0(I)*DMX(I))
            ELSE
              DLIM = DY(I) / (XL0(I)*DMN(I))
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRT(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (DY(I) > ZERO) THEN
                DLIM = DY(I) / (XL0(I)*DMX(I) + VFAIL)
              ELSE
                DLIM = DY(I) / (XL0(I)*DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (FY(I) > ZERO) THEN
                DLIM = FY(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = FY(I) / (DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,2)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
!         Uniaxial failure
            CRIT(I) = MAX(CRIT(I),XA*DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I)= CRIT(I) + XA*DLIM**XB
          ENDIF
        ENDIF
      ENDDO
C
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
        GF3(I)   =GEO(134,MGN(I))
        FF(I)   =GEO(52,MGN(I))
        LSCALE(I)=GEO(175,MGN(I))
        DMN(I)  =GEO(69,MGN(I))
        DMX(I)  =GEO(77,MGN(I))
      ENDDO
C
      KK = 1 + NUMELR * (ANIM_FE(11)+ANIM_FE(12))
      IF (NUVAR > 0) XX_OLD => UVAR(3,1:NEL)
      CALL REDEF3(PYTHON,
     1   FZ,         ZK,         DZ,         FZEP,
     2   DZOLD,      DPZ,        TF,         NPF,
     3   ZC,         OFF,        E6(1,3),    DPZ2,
     4   ANIM(KK),   ANIM_FE(13),POSZ,       
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDZ,     ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       XX_OLD,
     A   NEL,        NFT,        STF,        SANIN ,
     B   DT1,        IRESP,      IMPL_S,     IDYNA,
     C   SNPC)
C
      DO I=1,NEL
        CC  = GEO(105,MGN(I))
        CN  = GEO(111,MGN(I))
        XA  = GEO(117,MGN(I))
        XB  = GEO(123,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (DZ(I) > ZERO)THEN
              DLIM = DZ(I) / (XL0(I)*DMX(I))
            ELSE
              DLIM = DZ(I) / (XL0(I)*DMN(I))
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRT(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (DZ(I) > ZERO) THEN
                DLIM = DZ(I) / (XL0(I)*DMX(I) + VFAIL)
              ELSE
                DLIM = DZ(I) / (XL0(I)*DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (FZ(I) > ZERO) THEN
                DLIM = FZ(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = FZ(I) / (DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,3)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
!         Uniaxial failure
            CRIT(I) = MAX(CRIT(I),XA*DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I)= CRIT(I) + XA *DLIM**XB
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
     .              ZK(I)*GEO(61,MGN(I)))+XKR(I)
        XCR(I)= MAX(XC(I),YC(I),ZC(I)) + XHR(I) +XCR(I)+XH(I)
      ENDDO
C
      DO I=1,NEL
        DXOLD(I)=RX(I)
        DYOLD(I)=RY(I)
        DZOLD(I)=RZ(I)
      ENDDO
!
      IF ( INISPRI /= 0 .AND. TT == ZERO) THEN
        DO I=1,NEL
          DXOLD(I)=RX0(I)
          DYOLD(I)=RY0(I)
          DZOLD(I)=RZ0(I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        X21  = (RX2(I)-RX1(I))*DT1
        Y21  = (RY2(I)-RY1(I))*DT1
        Z21  = (RZ2(I)-RZ1(I))*DT1
        RX(I) = DXOLD(I) + X21*EXX2(I)+Y21*EYX2(I)+Z21*EZX2(I)
        RY(I) = DYOLD(I) + X21*EXY2(I)+Y21*EYY2(I)+Z21*EZY2(I)
        RZ(I) = DZOLD(I) + X21*EXZ2(I)+Y21*EYZ2(I)+Z21*EZZ2(I)
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
        FF(I)= GEO(56,MGN(I))
        LSCALE(I)= GEO(176,MGN(I))
        DMN(I)  =GEO(71,MGN(I))
        DMX(I)  =GEO(72,MGN(I))
      ENDDO
      IF (NUVAR > 0) XX_OLD => UVAR(4,1:NEL)
      CALL REDEF3(PYTHON,
     1   XMOM,       XK,         RX,         XMEP,
     2   DXOLD,      RPX,        TF,         NPF,
     3   XC,         OFF,        E6(1,4),    RPX2,
     4   ANIM,       0,          POSXX,      
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDX2,    ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       XX_OLD,
     A   NEL,        NFT,        STF,        SANIN,
     B   DT1,        IRESP,      IMPL_S,     IDYNA,
     C   SNPC)
C
      DO I=1,NEL
        CC  = GEO(106,MGN(I))
        CN  = GEO(112,MGN(I))
        XA  = GEO(118,MGN(I))
        XB  = GEO(124,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (RX(I) > ZERO) THEN
              DLIM = RX(I) / (XL0(I)*DMX(I))
            ELSE
              DLIM = RX(I) / (XL0(I)*DMN(I))
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (RX(I) > ZERO) THEN
                DLIM = RX(I) / (XL0(I)*DMX(I) + VFAIL)
              ELSE
                DLIM = RX(I) / (XL0(I)*DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 2) THEN
              IF(XMOM(I)>0.)THEN
                DLIM = XMOM(I)/(DMX(I) + VFAIL)
              ELSE
                DLIM = XMOM(I)/(DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,4)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
!         Uniaxial failure
            CRIT(I) = MAX(CRIT(I),XA*DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I)= CRIT(I) + XA *DLIM**XB
          ENDIF
        ENDIF
      ENDDO
C
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
        FF(I)= GEO(60,MGN(I))
        LSCALE(I)= GEO(177,MGN(I))
        DMN(I)  =GEO(73,MGN(I))
        DMX(I)  =GEO(74,MGN(I))
      ENDDO
      IF (NUVAR > 0) XX_OLD => UVAR(5,1:NEL)
      CALL REDEF3(PYTHON,
     1   YMOM,       YK,         RY,         YMEP,
     2   DYOLD,      RPY,        TF,         NPF,
     3   YC,         OFF,        E6(1,5),    RPY2,
     4   ANIM,       0,          POSYY,      
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDY2,    ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       XX_OLD,
     A   NEL,        NFT,        STF,        SANIN,
     B   DT1,        IRESP,      IMPL_S,     IDYNA,
     C   SNPC)
C
      DO I=1,NEL
        CC  = GEO(107,MGN(I))
        CN  = GEO(113,MGN(I))
        XA  = GEO(119,MGN(I))
        XB  = GEO(125,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (RY(I) > ZERO) THEN
              DLIM = RY(I) / (XL0(I)*DMX(I))
            ELSE
              DLIM = RY(I) / (XL0(I)*DMN(I))
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (RY(I) > ZERO) THEN
                DLIM = RY(I) / (XL0(I)*DMX(I) + VFAIL)
              ELSE
                DLIM = RY(I) / (XL0(I)*DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (YMOM(I) > ZERO)THEN
                DLIM = YMOM(I)/(DMX(I) + VFAIL)
              ELSE
                DLIM = YMOM(I)/(DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,5)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
!         Uniaxial failure
            CRIT(I) = MAX(CRIT(I),XA*DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I)= CRIT(I) + XA *DLIM**XB
          ENDIF
        ENDIF
      ENDDO
C
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
        FF(I)   =GEO(64,MGN(I))
        LSCALE(I)= GEO(178,MGN(I))
        DMN(I)  =GEO(75,MGN(I))
        DMX(I)  =GEO(76,MGN(I))
      ENDDO
      IF (NUVAR > 0) XX_OLD => UVAR(6,1:NEL)
      CALL REDEF3(PYTHON,
     1   ZMOM,       ZK,         RZ,         ZMEP,
     2   DZOLD,      RPZ,        TF,         NPF,
     3   ZC,         OFF,        E6(1,6),    RPZ2,
     4   ANIM,       0,          POSZZ,      
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDZ2,    ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       XX_OLD,
     A   NEL,        NFT,        STF,        SANIN,
     B   DT1,        IRESP,      IMPL_S,     IDYNA,
     C   SNPC)
C
      DO I=1,NEL
        CC  = GEO(108,MGN(I))
        CN  = GEO(114,MGN(I))
        XA  = GEO(120,MGN(I))
        XB  = GEO(126,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (RZ(I) > ZERO) THEN
              DLIM = RZ(I) / (XL0(I)*DMX(I))
            ELSE
              DLIM = RZ(I) / (XL0(I)*DMN(I))
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (RZ(I) > ZERO)THEN
                DLIM = RZ(I) / (XL0(I)*DMX(I) + VFAIL)
              ELSE
                DLIM = RZ(I) / (XL0(I)*DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (ZMOM(I) > ZERO)THEN
                DLIM = ZMOM(I)/(DMX(I) + VFAIL)
              ELSE
                DLIM = ZMOM(I)/(DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,6)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
!         Uniaxial failure
            CRIT(I) = MAX(CRIT(I),XA*DLIM)
          ELSE
!         Multiaxial failure
            CRIT(I)= CRIT(I) + XA *DLIM**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        E(I) = E6(I,1)+E6(I,2)+E6(I,3)+E6(I,4)+E6(I,5)+E6(I,6)
      ENDDO
C-------------------------------
C     COUPLED FAILURE
C-------------------------------
      DO I=1,NEL
        ISRATE = INT (GEO(127,MGN(I)))
C---- smoothing factor alpha = 2PI*fc*dt/(2PI*fc*dt+1) ---
        ASRATE = (2*PI*GEO(128,MGN(I))*DT1)/(ONE+2*PI*GEO(128,MGN(I))*DT1)
        IF (ISRATE /= 0) THEN
          IF (CRIT_NEW(I) < ONE) THEN 
            CRIT(I) = MIN(CRIT(I),ONE+EM3)
            CRIT(I) = ASRATE*CRIT(I) + (ONE - ASRATE)*CRIT_NEW(I)
            CRIT_NEW(I) = MIN(CRIT(I),ONE)
          ELSE
            CRIT_NEW(I) = ONE
          ENDIF
        ELSE
          IF (CRIT_NEW(I) < ONE) THEN 
            CRIT_NEW(I) = MIN(CRIT(I),ONE)
          ELSE
            CRIT_NEW(I) = ONE
          ENDIF
        ENDIF
        IF (OFF(I) == ONE) THEN
           IF (CRIT(I) >= ONE) THEN
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
          ENDIF
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
      DO I=1,NEL
        XM(I)=XM(I)*XL0(I)
        XKM(I)=XKM(I)/XL0(I)
        XCM(I)=XCM(I)/XL0(I)
        XIN(I)=XIN(I)*XL0(I)
        XKR(I)=XKR(I)/XL0(I)
        XCR(I)=XCR(I)/XL0(I)
      ENDDO
C---
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C---
      RETURN
      END
