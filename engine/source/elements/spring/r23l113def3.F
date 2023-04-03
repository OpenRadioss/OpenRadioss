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
Chd|  R23L113DEF3                   source/elements/spring/r23l113def3.F
Chd|-- called by -----------
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|-- calls ---------------
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        REPLA3                        source/elements/spring/repla3.F
Chd|====================================================================
      SUBROUTINE R23L113DEF3(
     1        SKEW    ,IPM    ,IGEO   ,MID     ,PID    ,
     2        GEO    ,UPARAM   ,FX     , FY    , FZ    ,
     3        E       ,DX     , DY     , DZ    , NPF   ,
     4        TF      ,OFF    ,DPX     ,DPY    ,DPZ   ,
     5        DPX2    ,DPY2   , DPZ2   ,FXEP   ,FYEP  ,
     6        FZEP    ,X0     ,Y0     , Z0     ,XMOM  ,
     7        YMOM    ,ZMOM   , RX     ,RY     ,RZ    ,
     8        RPX     ,RPY    ,RPZ    ,XMEP   ,YMEP   ,
     9        ZMEP    ,RPX2   , RPY2   ,RPZ2   ,ANIM  , 
     A        POSX    ,POSY   ,POSZ   ,POSXX  ,POSYY  ,
     B        POSZZ   ,FR_WAVE,E6     ,NEL    , EXX2  ,
     C        EYX2    ,EZX2   ,EXY2   ,EYY2   ,EZY2   , 
     D        EXZ2    ,EYZ2   , EZZ2   , AL2DP  , NGL , 
     E        CRIT_NEW,X0_ERR ,ALDP  ,YIELDX , YIELDY,
     F        YIELDZ  ,YIELDX2,YIELDY2,YIELDZ2,EXX    ,
     G        EYX    ,EZX   , EXY     ,EYY    ,  EZY  ,
     H        EXZ    , EYZ   ,EZZ     ,XCR    , RX1   ,
     I        RY1    ,RZ1   ,RX2      ,RY2    ,RZ2    ,
     K        XIN     ,AK    ,XM      ,XKM    , XCM   , 
     L        XKR     , VX1   ,VX2     ,VY1   ,VY2    ,
     I        VZ1     ,VZ2   ,NUVAR   ,UVAR   ,MASS   ,
     J        DX0    ,DY0    ,DZ0     ,RX0    ,RY0    ,
     K        RZ0    )
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
      INTEGER NPF(*),IGEO(NPROPGI,*),NEL,NGL(*),PID(*),MID(*),NUVAR,
     .     IPM(NPROPMI,*)
C     REAL
      my_real
     .   SKEW(LSKEW,*), GEO(NPROPG,*), FX(*), FY(*), FZ(*), E(*), DX(*),
     .   DY(*), DZ(*), TF(*), OFF(*), DPX(*), DPY(*), DPZ(*), FXEP(*),
     .   FYEP(*), FZEP(*), X0(*), Y0(*), Z0(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY(*), RZ(*), RPX(*), RPY(*), RPZ(*), XMEP(*),
     .   YMEP(*), ZMEP(*), DPX2(*), DPY2(*), DPZ2(*),RPX2(*), RPY2(*),
     .   RPZ2(*),ANIM(*),POSX(*),POSY(*),POSZ(*),POSXX(*),
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
     .   VZ1(MVSIZ),VZ2(MVSIZ),UVAR(NUVAR,*),UPARAM(*),MASS(*),
     .   DX0(*),DY0(*),DZ0(*),RX0(*),RY0(*),RZ0(*)
      TARGET :: UVAR
      DOUBLE PRECISION ALDP(MVSIZ),AL2DP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX(MVSIZ),
     .        IECROU(MVSIZ), IFUNC(MVSIZ), IFV(MVSIZ), IFUNC2(MVSIZ),
     .        I, ILENG, J, KK, IFAIL(MVSIZ),IFAIL2(MVSIZ),
     .        NINDX,ISRATE, IFUNC3(MVSIZ),I1,I2,I3,I4,I5,I6,I7,I8,
     .        I9,I10,I11,I12,I13,I14,IF1,IF2,IF3,IF4,IADBUF,NUPAR
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
     .     RYAV1, RZAV1, RYAV1P, RZAV1P,ASRATE,NOT_USED
      DOUBLE PRECISION X0DP(MVSIZ)
      my_real ,DIMENSION(:), POINTER :: COORD_OLD
C-----------------------------------------------
C
      NOT_USED = ZERO
C
      NUPAR = 4
      I1 = NUPAR
      I2 = I1 + 6 
      I3 = I2 + 6 
      I4 = I3 + 6 
      I5 = I4 + 6 
      I6 = I5 + 6 
      I7 = I6 + 6 
      I8 = I7 + 6 
      I9 = I8 + 6 
      I10 = I9 + 6 
      I11 = I10 + 6 
      I12 = I11 + 6 
      I13 = I12 + 6
      I14 = I13 + 6
      NUPAR = NUPAR +  84    
      DO I=1,NEL
       IADBUF= IPM(7,MID(I)) - 1
       EPLA(I)=ZERO
       XM(I)=MASS(I)
       XK(I)=UPARAM(IADBUF + I11 + 1)
       XC(I)=UPARAM(IADBUF + I12 + 1)
       YK(I)=UPARAM(IADBUF + I11 + 2)
       YC(I)=UPARAM(IADBUF + I12 + 2)
       ZK(I)=UPARAM(IADBUF + I11 + 3)
       ZC(I)=UPARAM(IADBUF + I12 + 3)
       XKA=XK(I)*UPARAM(IADBUF + I1 + 1)
       YKA=YK(I)*UPARAM(IADBUF + I1 + 2)
       ZKA=ZK(I)*UPARAM(IADBUF + I1 + 3)
       XKM(I)= MAX(XKA,YKA,ZKA)
       HX(I) = UPARAM(IADBUF + I14 + 1)
       HY(I) = UPARAM(IADBUF + I14 + 2)
       HZ(I) = UPARAM(IADBUF + I14 + 3)

       XH(I)= MAX(HX(I),HY(I),HZ(I))
       XCM(I)= MAX(XC(I),YC(I),ZC(I))
       XCM(I)= XCM(I)+XH(I)
     
       XKR(I)= MAX(YKA,ZKA) * ALDP(I) * ALDP(I)
       XCR(I)= (MAX(YC(I),ZC(I)) + MAX(HY(I),HZ(I))) * ALDP(I) * ALDP(I)
       VRT(I) = UPARAM(IADBUF + NUPAR + 1)
       VRR(I) = UPARAM(IADBUF + NUPAR + 2)
       IFAIL(I) = NINT(UPARAM(IADBUF + 1 ))
       IFAIL2(I)= NINT(UPARAM(IADBUF + 3 ))
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
          X0(I)= ALDP(I)                   ! cast double to my_real
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
        IADBUF = IPM(7,MID(I)) - 1
        ILENG = NINT(UPARAM(IADBUF + 2))
        IF (ILENG /= 0) THEN
          XL0(I)=X0DP(I)
        ELSE
          XL0(I)=ONE
        ENDIF
      ENDDO
C-------------------------------
      NINDX = 0
      IF1 = 0
      IF2 = 6
      IF3 = 12
      IF4 = 18
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 1,MID(I))
        IFV(I)   = IPM(10 + IF2 + 1,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 1,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 1,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 1))
        AK(I)    = UPARAM(IADBUF + I1 + 1)
        B(I)     = UPARAM(IADBUF + I2 + 1)
        D(I)     = UPARAM(IADBUF + I3 + 1)
        EE(I)    = UPARAM(IADBUF + I4 + 1)
        GF3(I)   = UPARAM(IADBUF + I5 + 1)
        FF(I)    = UPARAM(IADBUF + I6 + 1)
        LSCALE(I)= UPARAM(IADBUF + I7 + 1)
        DMN(I)   = UPARAM(IADBUF + I8 + 1)
        DMX(I)   = UPARAM(IADBUF + I9 + 1)
      ENDDO
      IF (NUVAR >=1) COORD_OLD => UVAR(1,1:NEL)
      CALL REDEF3(FX      ,XK      ,DX      ,FXEP       ,DXOLD   ,
     .            DPX     ,TF      ,NPF     ,XC         ,OFF     ,
     .            E6(1,1) ,DPX2    ,ANIM    ,ANIM_FE(11),POSX    ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDX  ,ALDP    ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA    ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
C
      DO I=1,NEL
        CC = UPARAM(IADBUF + NUPAR + 3)
        CN = UPARAM(IADBUF + NUPAR + 9)
        XA = UPARAM(IADBUF + NUPAR + 15)
        XB = UPARAM(IADBUF + NUPAR + 21)
        IF (OFF(I)  == ONE .AND. DMX(I) /= ZERO .AND. DMN(I)/= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (DX(I) > ZERO) THEN
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
                DLIM = DX(I) / (DMN(I) - VFAIL)
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
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 2,MID(I))
        IFV(I)   = IPM(10 + IF2 + 2,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 2,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 2,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 2))
        AK(I)    = UPARAM(IADBUF + I1 + 2)
        B(I)     = UPARAM(IADBUF + I2 + 2)
        D(I)     = UPARAM(IADBUF + I3 + 2)
        EE(I)    = UPARAM(IADBUF + I4 + 2)
        GF3(I)   = UPARAM(IADBUF + I5 + 2)
        FF(I)    = UPARAM(IADBUF + I6 + 2)
        LSCALE(I)= UPARAM(IADBUF + I7 + 2)
        DMN(I)   = UPARAM(IADBUF + I8 + 2)
        DMX(I)   = UPARAM(IADBUF + I9 + 2)
      ENDDO
C
      KK = 1 + NUMELR * ANIM_FE(11)
      IF (NUVAR >= 2) COORD_OLD => UVAR(2,1:NEL)
      CALL REDEF3(FY      ,YK     ,DY      ,FYEP       ,DYOLD   ,
     .            DPY     ,TF     ,NPF     ,YC         ,OFF     ,
     .            E6(1,2) ,DPY2   ,ANIM(KK),ANIM_FE(12),POSY    ,
     .            FR_WAVE ,XL0    ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDY  ,ALDP   ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC  ,IFV     ,IFUNC2     ,EPLA    ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        CC  = UPARAM(IADBUF + NUPAR + 4)
        CN  = UPARAM(IADBUF + NUPAR + 10)
        XA  = UPARAM(IADBUF + NUPAR + 16)
        XB  = UPARAM(IADBUF + NUPAR + 22)
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (DY(I) > ZERO) THEN
              DLIM = DY(I) / DMX(I)
            ELSE
              DLIM = DY(I) / DMN(I)
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRT(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (DY(I) > ZERO) THEN
                DLIM = DY(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = DY(I) / (DMN(I) - VFAIL)
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
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 3,MID(I))
        IFV(I)   = IPM(10 + IF2 + 3,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 3,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 3,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 3))
        AK(I)    = UPARAM(IADBUF + I1 + 3)
        B(I)     = UPARAM(IADBUF + I2 + 3)
        D(I)     = UPARAM(IADBUF + I3 + 3)
        EE(I)    = UPARAM(IADBUF + I4 + 3)
        GF3(I)   = UPARAM(IADBUF + I5 + 3)
        FF(I)    = UPARAM(IADBUF + I6 + 3)
        LSCALE(I)= UPARAM(IADBUF + I7 + 3)
        DMN(I)   = UPARAM(IADBUF + I8 + 3)
        DMX(I)   = UPARAM(IADBUF + I9 + 3)
      ENDDO
C
      KK = 1 + NUMELR * (ANIM_FE(11)+ANIM_FE(12))
      IF (NUVAR >= 3) COORD_OLD => UVAR(3,1:NEL)
      CALL REDEF3(FZ      ,ZK     ,DZ      ,FZEP       ,DZOLD   ,
     .            DPZ     ,TF     ,NPF     ,ZC         ,OFF     ,
     .            E6(1,3) ,DPZ2   ,ANIM(KK),ANIM_FE(13),POSZ    ,
     .            FR_WAVE ,XL0    ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDZ  ,ALDP   ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC  ,IFV     ,IFUNC2     ,EPLA    ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        CC  = UPARAM(IADBUF + NUPAR + 5)
        CN  = UPARAM(IADBUF + NUPAR + 11)
        XA  = UPARAM(IADBUF + NUPAR + 17)
        XB  = UPARAM(IADBUF + NUPAR + 23)
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (DZ(I) > ZERO)THEN
              DLIM = DZ(I) / DMX(I)
            ELSE
              DLIM = DZ(I) / DMN(I)
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRT(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (DZ(I) > ZERO) THEN
                DLIM = DZ(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = DZ(I) / (DMN(I) - VFAIL)
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
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C---------------------
C     ROTATIONS
C---------------------
      DO I=1,NEL
        IADBUF= IPM(7,MID(I)) - 1
        XIN(I)=  GEO(2,PID(I))
        XK(I) =  UPARAM(IADBUF + I11 + 4)
        XC(I) =  UPARAM(IADBUF + I12 + 4)
        YK(I) =  UPARAM(IADBUF + I11 + 5)
        YC(I) =  UPARAM(IADBUF + I12 + 5)
        ZK(I) =  UPARAM(IADBUF + I11 + 6)
        ZC(I) =  UPARAM(IADBUF + I12 + 6)
        HX(I) =  UPARAM(IADBUF + I14 + 4)
        HY(I) =  UPARAM(IADBUF + I14 + 5)
        HZ(I) =  UPARAM(IADBUF + I14 + 6)

        XHR(I)= MAX(HX(I),HY(I),HZ(I))

        XKR(I)= MAX(XK(I)*UPARAM(IADBUF + I1 + 4),
     .              YK(I)*UPARAM(IADBUF + I1 + 5),
     .              ZK(I)*UPARAM(IADBUF + I1 + 6))+XKR(I)
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
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 4,MID(I))
        IFV(I)   = IPM(10 + IF2 + 4,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 4,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 4,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 4))
        AK(I)    = UPARAM(IADBUF + I1 + 4)
        B(I)     = UPARAM(IADBUF + I2 + 4)
        D(I)     = UPARAM(IADBUF + I3 + 4)
        EE(I)    = UPARAM(IADBUF + I4 + 4)
        GF3(I)   = UPARAM(IADBUF + I5 + 4)
        FF(I)    = UPARAM(IADBUF + I6 + 4)
        LSCALE(I)= UPARAM(IADBUF + I7 + 4)
        DMN(I)   = UPARAM(IADBUF + I8 + 4)
        DMX(I)   = UPARAM(IADBUF + I9 + 4)
      ENDDO
      IF (NUVAR >= 4) COORD_OLD => UVAR(4,1:NEL)
      CALL REDEF3(XMOM    ,XK      ,RX      ,XMEP       ,DXOLD   ,
     .            RPX     ,TF      ,NPF     ,XC         ,OFF     ,
     .            E6(1,4) ,RPX2    ,ANIM    ,0          ,POSXX   ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDX2 ,ALDP    ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA    ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
C
      DO I=1,NEL
        IADBUF= IPM(7,MID(I)) - 1
        CC  = UPARAM(IADBUF + NUPAR + 6)
        CN  = UPARAM(IADBUF + NUPAR + 12)
        XA  = UPARAM(IADBUF + NUPAR + 18)
        XB  = UPARAM(IADBUF + NUPAR + 24)
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (RX(I) > ZERO) THEN
              DLIM = RX(I) / DMX(I)
            ELSE
              DLIM = RX(I) / DMN(I)
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (RX(I) > ZERO) THEN
                DLIM = RX(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = RX(I) / (DMN(I) - VFAIL)
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
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 5,MID(I))
        IFV(I)   = IPM(10 + IF2 + 5,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 5,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 5,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 5))
        AK(I)    = UPARAM(IADBUF + I1 + 5)
        B(I)     = UPARAM(IADBUF + I2 + 5)
        D(I)     = UPARAM(IADBUF + I3 + 5)
        EE(I)    = UPARAM(IADBUF + I4 + 5)
        GF3(I)   = UPARAM(IADBUF + I5 + 5)
        FF(I)    = UPARAM(IADBUF + I6 + 5)
        LSCALE(I)= UPARAM(IADBUF + I7 + 5)
        DMN(I)   = UPARAM(IADBUF + I8 + 5)
        DMX(I)   = UPARAM(IADBUF + I9 + 5)
      ENDDO
      IF (NUVAR >= 5) COORD_OLD => UVAR(5,1:NEL)
      CALL REDEF3(YMOM    ,YK      ,RY      ,YMEP       ,DYOLD   ,
     .            RPY     ,TF      ,NPF     ,YC         ,OFF     ,
     .            E6(1,5) ,RPY2    ,ANIM    ,0          ,POSYY   ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV      ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3  ,
     .            YIELDY2 ,ALDP    ,AK      ,B          ,D       ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA    ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
C
      DO I=1,NEL
        IADBUF= IPM(7,MID(I)) - 1
        CC  =  UPARAM(IADBUF + NUPAR + 7)
        CN  =  UPARAM(IADBUF + NUPAR + 13)
        XA  =  UPARAM(IADBUF + NUPAR + 19)
        XB  =  UPARAM(IADBUF + NUPAR + 25)
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (RY(I) > ZERO) THEN
              DLIM = RY(I) / DMX(I)
            ELSE
              DLIM = RY(I) / DMN(I)
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (RY(I) > ZERO) THEN
                DLIM = RY(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = RY(I) / (DMN(I) - VFAIL)
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
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 6,MID(I))
        IFV(I)   = IPM(10 + IF2 + 6,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 6,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 6,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 6))
        AK(I)    = UPARAM(IADBUF + I1 + 6)
        B(I)     = UPARAM(IADBUF + I2 + 6)
        D(I)     = UPARAM(IADBUF + I3 + 6)
        EE(I)    = UPARAM(IADBUF + I4 + 6)
        GF3(I)   = UPARAM(IADBUF + I5 + 6)
        FF(I)    = UPARAM(IADBUF + I6 + 6)
        LSCALE(I)= UPARAM(IADBUF + I7 + 6)
        DMN(I)   = UPARAM(IADBUF + I8 + 6)
        DMX(I)   = UPARAM(IADBUF + I9 + 6)
      ENDDO
      IF (NUVAR >= 6) COORD_OLD => UVAR(6,1:NEL)
      CALL REDEF3(ZMOM    ,ZK      ,RZ      ,ZMEP       ,DZOLD    ,
     .            RPZ     ,TF      ,NPF     ,ZC         ,OFF      ,
     .            E6(1,6) ,RPZ2    ,ANIM    ,0          ,POSZZ    ,
     .            FR_WAVE ,XL0     ,DMN     ,DMX        ,DV       ,
     .            FF      ,LSCALE  ,EE      ,GF3        ,IFUNC3   ,
     .            YIELDZ2 ,ALDP    ,AK      ,B          ,D        ,
     .            IECROU  ,IFUNC   ,IFV     ,IFUNC2     ,EPLA     ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
C
      DO I=1,NEL
        IADBUF= IPM(7,MID(I)) - 1
        CC  = UPARAM(IADBUF + NUPAR + 8)
        CN  = UPARAM(IADBUF + NUPAR + 14)
        XA  = UPARAM(IADBUF + NUPAR + 20)
        XB  = UPARAM(IADBUF + NUPAR + 26)
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (IFAIL2(I) == 0) THEN
            XA = ONE
            XB = TWO
            IF (RZ(I) > ZERO) THEN
              DLIM = RZ(I) / DMX(I)
            ELSE
              DLIM = RZ(I) / DMN(I)
            ENDIF
          ELSE
            VFAIL = CC * (ABS(DV(I)/VRR(I)))**CN
            IF (IFAIL2(I) == 1) THEN
              IF (RZ(I) > ZERO)THEN
                DLIM = RZ(I) / (DMX(I) + VFAIL)
              ELSE
                DLIM = RZ(I) / (DMN(I) - VFAIL)
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
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
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
        IADBUF   = IPM(7,MID(I)) - 1
        ISRATE = NINT(UPARAM(IADBUF + NUPAR + 27))
C---- smoothing factor alpha = 2PI*fc*dt/(2PI*fc*dt+1) ---
        ASRATE = (2*PI*UPARAM(IADBUF + NUPAR + 28)*DT1)/(ONE+2*PI*UPARAM(IADBUF + NUPAR + 28)*DT1)
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
          IADBUF= IPM(7,MID(I)) - 1
          XK(I)=UPARAM(IADBUF + I11 + 1)
          YK(I)=UPARAM(IADBUF + I11 + 2)
          ZK(I)=UPARAM(IADBUF + I11 + 3)
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
