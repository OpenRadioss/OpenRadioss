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
Chd|  R6DEF3                        source/elements/spring/r6def3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        REPLA3                        source/elements/spring/repla3.F
Chd|====================================================================
      SUBROUTINE R6DEF3(
     1                  SKEW   ,GEO    ,FX     ,FY     ,FZ      ,
     2                  E      ,DX     ,DY     ,DZ     ,NPF     ,
     3                  TF     ,OFF    ,DPX    ,DPY    ,DPZ     ,
     4                  DPX2   ,DPY2   ,DPZ2   ,FXEP   ,FYEP    ,
     5                  FZEP   ,X0     ,Y0     ,Z0     ,XMOM    ,
     6                  YMOM   ,ZMOM   ,RX     ,RY     ,RZ      ,
     7                  RPX    ,RPY    ,RPZ    ,XMEP   ,RMEP    ,
     8                  ZMEP   ,RPX2   ,RPY2   ,RPZ2   ,ANIM    ,
     9                  POSX   ,POSY   ,POSZ   ,POSXX  ,POSYY   ,
     A                  POSZZ  ,FR_WAVE,E6     ,NEL    ,AL2DP   ,
     C                  EXX2   ,EYX2   ,EZX2   ,EXY2   ,EYY2    ,
     L                  EZY2   ,EXZ2   ,EYZ2   ,EZZ2   ,IGEO    ,
     M                  X0_ERR ,ALDP   ,YIELDX ,YIELDY ,YIELDX2 ,
     N                  YIELDY2,NGL    ,MGN    ,XCR    ,RX1     ,
     O                  RY1    ,RZ1    ,RX2    ,RY2    ,RZ2     ,
     P                  XIN    ,AK     ,XM     ,XKM    ,XCM     ,
     Q                  XKR    ,VX1    ,VX2    ,VY1    ,VY2     ,
     R                  VZ1    ,VZ2    ,NUVAR  ,UVAR   ,DX0     ,
     S                  DY0    ,DZ0    ,RX0    ,RY0    ,RZ0     ,
     T                  FX0    ,FY0    ,FZ0    ,XMOM0  ,YMOM0   ,
     U                  ZMOM0  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*), IGEO(NPROPGI,*),NEL,NGL(*),MGN(*),NUVAR
C     REAL
      my_real
     .   SKEW(LSKEW,*), GEO(NPROPG,*), FX(*), FY(*), FZ(*), E(*), DX(*),
     .   DY(*), DZ(*), TF(*), OFF(*), DPX(*), DPY(*), DPZ(*), FXEP(*),
     .   FYEP(*), FZEP(*), X0(*), Y0(*), Z0(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY(*), RZ(*), RPX(*), RPY(*), RPZ(*), XMEP(*),
     .   RMEP(*), ZMEP(*), DPX2(*), DPY2(*), DPZ2(*),RPX2(*), RPY2(*),
     .   RPZ2(*),ANIM(*),POSX(*),POSY(*),POSZ(*),POSXX(*),
     .   POSYY(*),POSZZ(*),FR_WAVE(*),E6(NEL,6),
     .   EXX2(MVSIZ), EYX2(MVSIZ), EZX2(MVSIZ),
     .   EXY2(MVSIZ), EYY2(MVSIZ), EZY2(MVSIZ),
     .   EXZ2(MVSIZ), EYZ2(MVSIZ), EZZ2(MVSIZ),
     .   FR(MVSIZ), MR(MVSIZ), X0_ERR(MVSIZ),YIELDX(*),YIELDY(*),
     .   YIELDX2(*),YIELDY2(*),XCR(MVSIZ), RX1(MVSIZ), RX2(MVSIZ),
     .   RY1(MVSIZ), RY2(MVSIZ), RZ1(MVSIZ), RZ2(MVSIZ),XIN(MVSIZ),
     .   AK(MVSIZ),XM(MVSIZ),XKM(MVSIZ),XCM(MVSIZ),XKR(MVSIZ),
     .   VX1(MVSIZ),VX2(MVSIZ),VY1(MVSIZ),VY2(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ),UVAR(NUVAR,*),DX0(*),DY0(*),DZ0(*),
     .   RX0(*),RY0(*),RZ0(*),FX0(*),FY0(*),FZ0(*),XMOM0(*),YMOM0(*),ZMOM0(*)
      DOUBLE PRECISION ALDP(MVSIZ),AL2DP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX(MVSIZ),
     .        IECROU(MVSIZ), IFUNC(MVSIZ), IFV(MVSIZ), IFUNC2(MVSIZ),
     .        I, ILENG, J, KK, IFAIL(MVSIZ),IFAIL2(MVSIZ),
     .        NINDX,IFUNC3(MVSIZ)
C     REAL
      my_real
     .   XK(MVSIZ) , YK(MVSIZ),
     .   XC(MVSIZ) , YC(MVSIZ) ,XH(MVSIZ),XHR(MVSIZ),
     .   DXOLD(MVSIZ), DYOLD(MVSIZ), DZOLD(MVSIZ), DROLD(MVSIZ),
     .   B(MVSIZ), D(MVSIZ), EPLA(MVSIZ),
     .   DV(MVSIZ),VRT(MVSIZ),VRR(MVSIZ),FF(MVSIZ),EE(MVSIZ),
     .   DMN(MVSIZ),DMX(MVSIZ),XL0(MVSIZ),CRIT(MVSIZ),RMOM(MVSIZ),
     .   XN(MVSIZ) ,DR(MVSIZ),ST(MVSIZ),CT(MVSIZ),BID(MVSIZ),
     .   LSCALE(MVSIZ),GF3(MVSIZ),HX(MVSIZ), HY(MVSIZ), HZ(MVSIZ)
      my_real
     .   AT,C,CP,DT05,CC,CN,XA,XB,DLIM,VFAIL,XKA,YKA,X21,Y21,Z21,
     .   VX21,VY21,VZ21, EPXY,EPXZ,EYZP,EXZP,RYAV,RZAV,RYAV1,RZAV1,
     .   NOT_USED
      DOUBLE PRECISION X0DP(MVSIZ)
C-----------------------------------------------
C
      NOT_USED = ZERO
C
      DO I=LFT,LLT
        EPLA(I)=ZERO
        XM(I)=GEO(1,MGN(I))
        XK(I)=GEO(3,MGN(I))
        XC(I)=GEO(4,MGN(I))
        YK(I)=GEO(10,MGN(I))
        YC(I)=GEO(11,MGN(I))
        XKA  =GEO(41,MGN(I))*XK(I)
        YKA  =GEO(45,MGN(I))*YK(I)
        XKM(I)= MAX(XKA,YKA)
        HX(I) = GEO(141,MGN(I))
        HY(I) = GEO(142,MGN(I))

        XH(I)= MAX(HX(I),HY(I))

        XCM(I)= MAX(XC(I),YC(I))
        XCM(I)= XCM(I)+XH(I)

        XKR(I)= YKA*ALDP(I)*ALDP(I)
        XCR(I)= (YC(I)+ HY(I))*ALDP(I)* ALDP(I)
      ENDDO
C
      DO I=LFT,LLT
        VRT(I) = GEO(101,MGN(I))
        VRR(I) = GEO(102,MGN(I))
        IFAIL(I) = NINT(GEO(79,MGN(I)))
        IFAIL2(I)= NINT(GEO(95,MGN(I)))
      ENDDO
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          XL0(I)= X0(I)
! if not initialized length
          IF (XL0(I) == ZERO) XL0(I) = ALDP(I)
        ENDDO
      ENDIF
C
      IF (TT == ZERO)THEN
        DO I=LFT,LLT
          X0(I)= ALDP(I)                   ! cast double vers My_real
        ENDDO
      ENDIF
C
      IF (SCODVER >= 101) THEN
        IF (TT == ZERO)THEN
          DO I=LFT,LLT
            X0_ERR(I)= ALDP(I)-X0(I)         ! difference entre double et My_real
          ENDDO
        ENDIF
      ENDIF
C
      IF ( INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          X0(I)= XL0(I)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        X0DP(I)= X0(I)                     ! cast double vers My_real
      ENDDO
C
      IF (SCODVER >= 101) THEN
        DO I=LFT,LLT
          X0DP(I)= X0(I) + X0_ERR(I)         ! difference entre double et My_real
        ENDDO
      ENDIF
C---------------------
C     TRANSLATIONS
C---------------------
      DO I=LFT,LLT
        DXOLD(I)=DX(I)
        DYOLD(I)=DY(I)
        DZOLD(I)=DZ(I)
      ENDDO
!
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          DXOLD(I)=DX0(I)
          DYOLD(I)=DY0(I)
          DZOLD(I)=DZ0(I)
        ENDDO
      ENDIF
C
      DT05=HALF*DT1
C
      DO I=LFT,LLT
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
!!        DX(I) = ALDP(I) - X0(I)
        DX(I) = ALDP(I) - X0DP(I)
        DY(I) = DYOLD(I) - RZAV * AL2DP(I)
        DZ(I) = DZOLD(I) + RYAV * AL2DP(I)
        CRIT(I) = ZERO
      ENDDO
C
      DO I=LFT,LLT
        ILENG=NINT(GEO(93,MGN(I)))
        IF (ILENG /= 0) THEN
          XL0(I)=X0(I)
        ELSE
          XL0(I)=ONE
        ENDIF
      ENDDO
C
      NINDX = 0
C---- Traction / Compression
      DO I=LFT,LLT
        IECROU(I)= IGEO(101,MGN(I))
        IFUNC(I) = IGEO(102,MGN(I))
        IFUNC2(I)= IGEO(103,MGN(I))
        IFV(I)   = IGEO(104,MGN(I))
        IFUNC3(I)= IGEO(119,MGN(I))
        AK(I)    = GEO(41,MGN(I))
        B(I)     = GEO(42,MGN(I))
        D(I)     = GEO(43,MGN(I))
        EE(I)    = GEO(40 ,MGN(I))
        GF3(I)   = GEO(132,MGN(I))
        FF(I)    = GEO(44,MGN(I))
        LSCALE(I)= GEO(39 ,MGN(I))
        DMN(I)   = GEO(65,MGN(I))
        DMX(I)   = GEO(66,MGN(I))        
      ENDDO
C
      CALL REDEF3(FX      ,XK      ,DX      ,FXEP       ,DXOLD   ,
     .            DPX     ,TF      ,NPF     ,XC         ,OFF     ,
     .            E6(1,1) ,DPX2    ,ANIM    ,ANIM_E(11) ,POSX    ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDX  ,X0DP    ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA    ,
     .            UVAR(1,1),NOT_USED,NOT_USED,NOT_USED)
C
      DO I=LFT,LLT
        CC     = GEO(103,MGN(I))
        CN     = GEO(109,MGN(I))
        XA     = GEO(115,MGN(I))
        XB     = GEO(121,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
             IF(DX(I) > ZERO)THEN
              DLIM = DX(I) / DMX(I)
            ELSE
              DLIM = DX(I) / DMN(I)
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRT(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (DX(I) > ZERO) THEN
                DLIM = DX(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = DX(I) / (DMX(I) - VFAIL)
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
C---      Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I)=ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
C---      Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA * DLIM**XB
          ENDIF
        ENDIF
      ENDDO
C---- Cisaillement
      DO I=LFT,LLT
        IECROU(I)= IGEO(105,MGN(I))
        IFUNC(I) = IGEO(106,MGN(I))
        IFUNC2(I)= IGEO(107,MGN(I))
        IFV(I)   = IGEO(108,MGN(I))
        IFUNC3(I)= IGEO(120,MGN(I))
        AK(I)    = GEO(45,MGN(I))
        B(I)     = GEO(46,MGN(I))
        D(I)     = GEO(47,MGN(I))
        EE(I)    = GEO(180,MGN(I))
        GF3(I)   = GEO(133,MGN(I))
        FF(I)    = GEO(48,MGN(I))
        LSCALE(I)= GEO(174,MGN(I))
        DMN(I)   = GEO(67,MGN(I))
        DMX(I)   = GEO(68,MGN(I))
        DR(I)    = SQRT(DY(I)**2 + DZ(I)**2)
        DROLD(I) = SQRT(DYOLD(I)**2 + DZOLD(I)**2)
        ST(I)    = ZERO
        CT(I)    = ONE
        IF (DR(I) > ZERO) THEN
          ST(I) = DY(I) / DR(I)
          CT(I) = DZ(I) / DR(I)
        ENDIF
        FR(I) = SQRT(FY(I)**2 + FZ(I)**2)
      ENDDO
      KK = 1 + NUMELR * ANIM_E(11)
      CALL REDEF3(FR      ,YK      ,DR      ,FYEP       ,DROLD   ,
     .            DPY     ,TF      ,NPF     ,YC         ,OFF     ,
     .            E6(1,2) ,DPY2    ,ANIM(KK),ANIM_E(12) ,POSY    ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDY  ,X0DP    ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA    ,
     .            UVAR(2,1),NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
        CC  = GEO(104,MGN(I))
        CN  = GEO(110,MGN(I))
        XA  = GEO(116,MGN(I))
        XB  = GEO(122,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            DLIM = DR(I) / DMX(I)
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRT(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              DLIM = DR(I) / (DMX(I) + VFAIL)
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (FR(I) > ZERO) THEN
                DLIM = FR(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = FR(I) / (DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,2)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
C---      Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I)=ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
C---      Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA * DLIM**XB
          ENDIF
        ENDIF
C---
        FY(I) = FR(I)*ST(I)
        FZ(I) = FR(I)*CT(I)
      ENDDO
C--------------------------------------------------------------
C     ROTATIONS
C---------------------
      DO I=LFT,LLT
        XIN(I)= GEO(9,MGN(I))
        XK(I) = GEO(19,MGN(I))
        XC(I) = GEO(20,MGN(I))
        YK(I) = GEO(23,MGN(I))
        YC(I) = GEO(24,MGN(I))
        HX(I) = GEO(143,MGN(I))
        HY(I) = GEO(144,MGN(I))
        XHR(I)= MAX(HX(I),HY(I))

        XKR(I)= MAX(XK(I)*GEO(53,MGN(I)),YK(I)*GEO(57,MGN(I)))+XKR(I)
        XCR(I)= MAX(XC(I),YC(I))+XCR(I)+XHR(I)+XH(I)
      ENDDO
C
      DO I=LFT,LLT
        DXOLD(I)=RX(I)
        DYOLD(I)=RY(I)
        DZOLD(I)=RZ(I)
      ENDDO
C
      IF ( INISPRI /= 0 .AND. TT == ZERO) THEN
        DO I=LFT,LLT
          DXOLD(I)=RX0(I)
          DYOLD(I)=RY0(I)
          DZOLD(I)=RZ0(I)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        X21  = (RX2(I)-RX1(I))*DT1
        Y21  = (RY2(I)-RY1(I))*DT1
        Z21  = (RZ2(I)-RZ1(I))*DT1
        RX(I) = DXOLD(I) + X21*EXX2(I)+Y21*EYX2(I)+Z21*EZX2(I)
        RY(I) = DYOLD(I) + X21*EXY2(I)+Y21*EYY2(I)+Z21*EZY2(I)
        RZ(I) = DZOLD(I) + X21*EXZ2(I)+Y21*EYZ2(I)+Z21*EZZ2(I)
      ENDDO
C
C-----Torsion
C
      DO I=LFT,LLT
        IECROU(I)= IGEO(109,MGN(I))
        IFUNC(I) = IGEO(110,MGN(I))
        IFUNC2(I)= IGEO(111,MGN(I))
        IFUNC3(I)= IGEO(121,MGN(I))
        IFV(I)   = IGEO(112,MGN(I))
        AK(I)    = GEO(53,MGN(I))
        B(I)     = GEO(54,MGN(I))
        D(I)     = GEO(55,MGN(I))
        EE(I)    = GEO(182,MGN(I))
        GF3(I)   = GEO(135,MGN(I))
        FF(I)    = GEO(56,MGN(I))
        LSCALE(I)= GEO(176,MGN(I))
        DMN(I)   = GEO(71,MGN(I))
        DMX(I)   = GEO(72,MGN(I))
      ENDDO
C
      CALL REDEF3(XMOM    ,XK      ,RX      ,XMEP       ,DXOLD   ,
     .            RPX     ,TF      ,NPF     ,XC         ,OFF     ,
     .            E6(1,3) ,RPX2    ,ANIM    ,0          ,POSXX   ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDX2 ,X0DP    ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA    ,
     .            UVAR(4,1),NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
        CC  = GEO(105,MGN(I))
        CN  = GEO(111,MGN(I))
        XA  = GEO(117,MGN(I))
        XB  = GEO(123,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I)/=ZERO .AND. DMN(I)/=ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
             XA = ONE
            XB = TWO
            IF (RX(I) > ZERO)THEN
              DLIM = RX(I) / DMX(I)
            ELSE
              DLIM = RX(I) / DMN(I)
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF(RX(I) > ZERO)THEN
                DLIM = RX(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = RX(I) / (DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (XMOM(I) > ZERO) THEN
                DLIM = XMOM(I)/(DMX(I) + VFAIL)
              ELSE
                DLIM = XMOM(I)/(DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,3)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
C---      Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I)=ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
C---      Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA * DLIM**XB
          ENDIF
        ENDIF
      ENDDO
C
C---  Flexion radiale
C
      DO I=LFT,LLT
        IECROU(I)= IGEO(113,MGN(I))
        IFUNC(I) = IGEO(114,MGN(I))
        IFUNC2(I)= IGEO(115,MGN(I))
        IFUNC3(I)= IGEO(122,MGN(I))
        IFV(I)   = IGEO(116,MGN(I))
        AK(I)    = GEO(57,MGN(I))
        B(I)     = GEO(58,MGN(I))
        D(I)     = GEO(59,MGN(I))
        EE(I)    = GEO(183,MGN(I))
        GF3(I)   = GEO(136,MGN(I))
        FF(I)    = GEO(60,MGN(I))
        LSCALE(I)= GEO(177,MGN(I))
        DMN(I)   = GEO(73,MGN(I))
        DMX(I)   = GEO(74,MGN(I))
        DR(I)    = SQRT(RY(I)**2 + RZ(I)**2)
        DROLD(I) = SQRT(DYOLD(I)**2 + DZOLD(I)**2)
        ST(I)    = ZERO
        CT(I)    = ONE
        IF (DR(I) > ZERO) THEN
          ST(I) = RY(I) / DR(I)
          CT(I) = RZ(I) / DR(I)
        ENDIF
        RMOM(I) = SQRT(YMOM(I)**2 + ZMOM(I)**2)
      ENDDO
      CALL REDEF3(RMOM    ,YK      ,DR      ,RMEP       ,DROLD   ,
     .            RPY     ,TF      ,NPF     ,YC         ,OFF     ,
     .            E6(1,4) ,RPY2    ,ANIM    ,0          ,POSYY   ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDY2 ,X0DP    ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA    ,
     .            UVAR(5,1),NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
        CC  = GEO(106,MGN(I))
        CN  = GEO(112,MGN(I))
        XA  = GEO(118,MGN(I))
        XB  = GEO(124,MGN(I))
        IF (OFF(I) == ONE .AND. DMX(I)/=ZERO .AND. DMN(I)/=ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            DLIM = DR(I) / DMX(I)
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              DLIM = DR(I) / (DMX(I) + VFAIL)
            ELSEIF (IFAIL2(I) == 2) THEN
              IF (RMOM(I) > ZERO) THEN
                DLIM = RMOM(I)/(DMX(I) + VFAIL)
              ELSE
                DLIM = RMOM(I)/(DMN(I) - VFAIL)
              ENDIF
            ELSEIF (IFAIL2(I) == 3) THEN
              DLIM = MAX(ZERO,E6(I,4)) / (DMX(I) + VFAIL)
            ENDIF
          ENDIF
          IF (IFAIL(I) == 0) THEN
C---      Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I)=ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
C---      Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA * DLIM**XB
          ENDIF
        ENDIF
C
        YMOM(I) = RMOM(I)*ST(I)
        ZMOM(I) = RMOM(I)*CT(I)
      ENDDO
C-------------------------------
      DO I=LFT,LLT
        E(I) = E6(I,1)+E6(I,2)+E6(I,3)+E6(I,4)
      ENDDO
C-------------------------------
C     RUPTURE COUPLEE
C-------------------------------
      DO I=LFT,LLT
        IF (OFF(I) == ONE .AND. IFAIL(I) == 1) THEN
          IF (CRIT(I)/(XL0(I)*XL0(I)) > ONE) THEN
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
C     PLASTICITE COUPLEE
C-------------------------------
      CALL REPLA3(XK   ,RPX ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
      CALL REPLA3(YK   ,RPY ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
C
      DO I=LFT,LLT
        XK(I)=GEO(3,MGN(I))
        YK(I)=GEO(10,MGN(I))
      ENDDO
C
      CALL REPLA3(XK   ,DPX ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
      CALL REPLA3(YK   ,DPY ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
C
      DO I=LFT,LLT
        XM(I) =XM(I)*XL0(I)
        XKM(I)=XKM(I)/XL0(I)
        XCM(I)=XCM(I)/XL0(I)
        XIN(I)=XIN(I)*XL0(I)
        XKR(I)=XKR(I)/XL0(I)
        XCR(I)=XCR(I)/XL0(I)
      ENDDO
C-----------
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C-----------
      RETURN
      END
