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
Chd|  R23L108DEF3                   source/elements/spring/r23l108def3.F
Chd|-- called by -----------
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|-- calls ---------------
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        REPLA3                        source/elements/spring/repla3.F
Chd|====================================================================
      SUBROUTINE R23L108DEF3(
     .        IPM     ,IGEO   ,MID    ,PID    , UPARAM   ,   
     1        SKEW    ,GEO    ,FX     ,FY     ,FZ      ,
     2        E       ,DX     ,DY     ,DZ     ,NPF     ,
     3        TF      ,OFF    ,DPX    ,DPY    ,DPZ     ,
     4        DPX2    ,DPY2   ,DPZ2   ,FXEP   ,FYEP    ,
     5        FZEP    ,X0     ,Y0     ,Z0     ,XMOM    ,
     6        YMOM    ,ZMOM   ,RX     ,RY     ,RZ      ,
     7        RPX     ,RPY    ,RPZ    ,XMEP   ,YMEP    ,
     8        ZMEP    ,RPX2   ,RPY2   ,RPZ2   ,ANIM    ,
     9        IPOSX   ,IPOSY  ,IPOSZ  ,IPOSXX ,IPOSYY  ,
     A        IPOSZZ  ,FR_WAVE,V      ,E6      ,CRITNEW ,
     B        NEL    ,X0_ERR ,X1DP   ,X2DP    ,YIELDX  ,
     C        YIELDY ,YIELDZ ,YIELDX2,YIELDY2 ,YIELDZ2 ,
     D        NGL    ,XKR     ,EXX    ,EYX     ,EZX    ,
     E        EXY    ,EYY    ,EZY     ,EXZ     ,EYZ    ,
     F        EZZ    ,XCR    ,RX1     ,RY1     ,RZ1    ,
     G        RX2    ,RY2    ,RZ2     ,XIN    ,AK      ,
     H        XM     ,XKM    ,XCM     ,NC1    ,NC2     ,
     I        NUVAR  ,UVAR   ,MASS    ,DX0    , DY0    ,
     J        DZ0    ,RX0    ,RY0    , RZ0    ,IEQUIL  ,
     K        SKEW_ID)
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
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*), IGEO(NPROPGI,*),NEL,NGL(*),NC1(*),NC2(*),NUVAR,
     .   IPM(NPROPMI,*),MID(*),PID(*),IEQUIL(*),SKEW_ID(*)     
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
     .   UVAR(NUVAR,*),DX0(*),DY0(*),DZ0(*),RX0(*),RY0(*),RZ0(*),
     .   UPARAM(*),MASS(*)
      TARGET :: UVAR
      DOUBLE PRECISION X1DP(3,*),X2DP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC2(MVSIZ),
     .        IECROU(MVSIZ), IFUNC(MVSIZ), IFV(MVSIZ),
     .        INDX(MVSIZ),IFUNC3(MVSIZ),
     .        I,J,ISK, KK,NINDX,IFAIL(MVSIZ),IFAIL2(MVSIZ),ISRATE,
     .        I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,
     .        NUPAR,IADBUF,IF1,IF2,IF3,IF4
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
     .     ASRATE,DLIM,NOT_USED(2)
      DOUBLE PRECISION X21DP(MVSIZ),Y21DP(MVSIZ),Z21DP(MVSIZ),
     .                 X0DP(MVSIZ),Y0DP(MVSIZ),Z0DP(MVSIZ)
      my_real ,DIMENSION(:), POINTER :: COORD_OLD
      TARGET :: NOT_USED
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
      DO I=LFT,LLT
        IADBUF= IPM(7,MID(I)) - 1
        EPLA(I)=ZERO
        XM(I)=MASS(I)
        XK(I)=UPARAM(IADBUF + I11 + 1)
        XC(I)=UPARAM(IADBUF + I12 + 1)
        YK(I)=UPARAM(IADBUF + I11 + 2)
        YC(I)=UPARAM(IADBUF + I12 + 2)
        ZK(I)=UPARAM(IADBUF + I11 + 3)
        ZC(I)=UPARAM(IADBUF + I12 + 3)
        IFAIL(I) = NINT(UPARAM(IADBUF + 1 ))
        IFAIL2(I)= NINT(UPARAM(IADBUF + 3 ))
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
        IEQUIL(I) = UPARAM(IADBUF + 4)
      ENDDO
C---------------------
C     TRANSLATIONS
C---------------------
      DO I=LFT,LLT
        DXOLD(I)=DX(I)
        DYOLD(I)=DY(I)
        DZOLD(I)=DZ(I)
      ENDDO
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          DXOLD(I)=DX0(I)
          DYOLD(I)=DY0(I)
          DZOLD(I)=DZ0(I)
        ENDDO
      ENDIF
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          X0_INI(I)=X0(I)
          Y0_INI(I)=Y0(I)
          Z0_INI(I)=Z0(I)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        X21DP(I)= X2DP(1,I)-X1DP(1,I)
        Y21DP(I)= X2DP(2,I)-X1DP(2,I)
        Z21DP(I)= X2DP(3,I)-X1DP(3,I)
        X21(I)= X21DP(I)
        Y21(I)= Y21DP(I)
        Z21(I)= Z21DP(I)
      ENDDO
C
      IF (TT == ZERO) THEN
        DO I=LFT,LLT
          X0DP(I)= X21DP(I)*EXX(I)+Y21DP(I)*EYX(I)+Z21DP(I)*EZX(I)
          Y0DP(I)= X21DP(I)*EXY(I)+Y21DP(I)*EYY(I)+Z21DP(I)*EZY(I)
          Z0DP(I)= X21DP(I)*EXZ(I)+Y21DP(I)*EYZ(I)+Z21DP(I)*EZZ(I)
          X0(I)= X0DP(I)                 ! cast double vers My_real
          Y0(I)= Y0DP(I)                 ! cast double vers My_real
          Z0(I)= Z0DP(I)                 ! cast double vers My_real
        ENDDO
!
        IF (INISPRI /= 0) THEN
!  condition nedeed for spring type 8, which are not concerned by /INISPRI,
!  and having initial length /= 0 
          DO I=LFT,LLT
            IF (X0_INI(I) == ZERO .and. DX0(I) == ZERO) X0_INI(I) = X0DP(I)
            IF (Y0_INI(I) == ZERO .and. DY0(I) == ZERO) Y0_INI(I) = Y0DP(I)
            IF (Z0_INI(I) == ZERO .and. DZ0(I) == ZERO) Z0_INI(I) = Z0DP(I)
          ENDDO
        ENDIF
!
      ENDIF
C
      IF (SCODVER >= 101) THEN
        IF (TT == ZERO) THEN
          DO I=LFT,LLT
            X0_ERR(1,I)= X0DP(I)-X0(I)   ! difference entre double et My_real
            X0_ERR(2,I)= Y0DP(I)-Y0(I)   ! difference entre double et My_real
            X0_ERR(3,I)= Z0DP(I)-Z0(I)   ! difference entre double et My_real
          ENDDO
        ENDIF
      ENDIF
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          X0(I)=X0_INI(I)
          Y0(I)=Y0_INI(I)
          Z0(I)=Z0_INI(I)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        X0DP(I)= X0(I)                 ! cast My_real en double
        Y0DP(I)= Y0(I)                 ! cast My_real en double
        Z0DP(I)= Z0(I)                 ! cast My_real en double
      ENDDO
C
      IF (SCODVER >= 101) THEN
        DO I=LFT,LLT
          X0DP(I)= X0DP(I) + X0_ERR(1,I) ! AL_DP doit   tre recalcul   ainsi afin de garantir la coh  rence absolue entre AL0_DP et AL_DP
          Y0DP(I)= Y0DP(I) + X0_ERR(2,I) ! AL_DP doit   tre recalcul   ainsi afin de garantir la coh  rence absolue entre AL0_DP et AL_DP
          Z0DP(I)= Z0DP(I) + X0_ERR(3,I) ! AL_DP doit   tre recalcul   ainsi afin de garantir la coh  rence absolue entre AL0_DP et AL_DP
        ENDDO
      ENDIF
C
      IF (ISMDISP > 0) THEN
        DO I=LFT,LLT
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
        DO I=LFT,LLT
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
      IF1 = 0
      IF2 = 6
      IF3 = 12
      IF4 = 18
      DO I=LFT,LLT
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 1,MID(I))
        IFV(I)   = IPM(10 + IF2 + 1,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 1,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 1,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 1))
        AK(I)    =  UPARAM(IADBUF + I1 + 1)
        B(I)     =  UPARAM(IADBUF + I2 + 1)
        D(I)     =  UPARAM(IADBUF + I3 + 1)
        EE(I)    =  UPARAM(IADBUF + I4 + 1)
        GF3(I)   =  UPARAM(IADBUF + I5 + 1)
        RSCALE(I)=  UPARAM(IADBUF + I6 + 1)
        LSCALE(I)=  UPARAM(IADBUF + I7 + 1)
        DMN(I)   =  UPARAM(IADBUF + I8 + 1)
        DMX(I)   =  UPARAM(IADBUF + I9 + 1)
      ENDDO
      IF (NUVAR >= 1) THEN
         COORD_OLD => UVAR(1,1:NEL)
      ELSE
         COORD_OLD => NOT_USED
      ENDIF

      CALL REDEF3(FX      ,XK     ,DX    ,FXEP       ,DXOLD ,
     .            DPX     ,TF     ,NPF   ,XC         ,OFF   ,
     .            E6(1,1) ,DPX2   ,ANIM  ,ANIM_FE(11),IPOSX ,
     .            FR_WAVE ,XL0    ,DMN   ,DMX        ,DV    ,
     .            RSCALE  ,LSCALE ,EE    ,GF3        ,IFUNC3,
     .            YIELDX  ,X0DP   ,AK    ,B          ,D     ,
     .            IECROU  ,IFUNC  ,IFV   ,IFUNC2     ,EPLA  ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
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
           IF(DLIM > ONE) THEN
!        Rupture uniaxiale
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
           ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
      DO I=LFT,LLT
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
        RSCALE(I)= UPARAM(IADBUF + I6 + 2)
        LSCALE(I)= UPARAM(IADBUF + I7 + 2)
        DMN(I)   = UPARAM(IADBUF + I8 + 2)
        DMX(I)   = UPARAM(IADBUF + I9 + 2)
      ENDDO
      KK = 1 + NUMELR * ANIM_FE(11)

      IF (NUVAR >= 2) THEN 
         COORD_OLD => UVAR(2,1:NEL)
      ELSE
         COORD_OLD => NOT_USED
      ENDIF

      CALL REDEF3(FY      ,YK     ,DY      ,FYEP       ,DYOLD ,
     .            DPY     ,TF     ,NPF     ,YC         ,OFF   ,
     .            E6(1,2) ,DPY2   ,ANIM(KK),ANIM_FE(12),IPOSY ,
     .            FR_WAVE ,XL0    ,DMN     ,DMX        ,DV    ,
     .            RSCALE  ,LSCALE ,EE      ,GF3        ,IFUNC3,
     .            YIELDY  ,Y0DP   ,AK      ,B          ,D     ,
     .            IECROU  ,IFUNC  ,IFV     ,IFUNC2     ,EPLA  ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
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
           IF (DLIM > ONE) THEN
!         Rupture uniaxiale
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
           ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
      DO I=LFT,LLT
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
        RSCALE(I)= UPARAM(IADBUF + I6 + 3) 
        LSCALE(I)= UPARAM(IADBUF + I7 + 3) 
        DMN(I)   = UPARAM(IADBUF + I8 + 3) 
        DMX(I)   = UPARAM(IADBUF + I9 + 3) 
      ENDDO
      KK = 1 + NUMELR * (ANIM_FE(11)+ANIM_FE(12))
      IF (NUVAR >= 3) THEN
         COORD_OLD => UVAR(3,1:NEL)
      ELSE
         COORD_OLD => NOT_USED
      ENDIF
      CALL REDEF3(FZ      ,ZK     ,DZ      ,FZEP       ,DZOLD ,
     .            DPZ     ,TF     ,NPF     ,ZC         ,OFF   ,
     .            E6(1,3) ,DPZ2   ,ANIM(KK),ANIM_FE(13),IPOSZ ,
     .            FR_WAVE ,XL0    ,DMN     ,DMX        ,DV    ,
     .            RSCALE  ,LSCALE ,EE      ,GF3        ,IFUNC3,
     .            YIELDZ  ,Z0DP   ,AK      ,B          ,D     ,
     .            IECROU  ,IFUNC  ,IFV     ,IFUNC2     ,EPLA  ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
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
           IF (DLIM > ONE) THEN
!         Rupture uniaxiale
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
           ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
C---------------------
C     ROTATIONS
C---------------------
      DO I=LFT,LLT
        IADBUF= IPM(7,MID(I)) - 1
        XIN(I)= GEO(2,PID(I))
        XK(I) = UPARAM(IADBUF + I11 + 4)
        XC(I) = UPARAM(IADBUF + I12 + 4)
        YK(I) = UPARAM(IADBUF + I11 + 5)
        YC(I) = UPARAM(IADBUF + I12 + 5)
        ZK(I) = UPARAM(IADBUF + I11 + 6)
        ZC(I) = UPARAM(IADBUF + I12 + 6)
        HX(I) = UPARAM(IADBUF + I14 + 4)
        HY(I) = UPARAM(IADBUF + I14 + 5)
        HZ(I) = UPARAM(IADBUF + I14 + 6)

        XHR(I)= MAX(HX(I),HY(I),HZ(I))
        XKR(I)= MAX(XK(I)*UPARAM(IADBUF + I1 + 4),
     .              YK(I)*UPARAM(IADBUF + I1 + 5),
     .              ZK(I)*UPARAM(IADBUF + I1 + 6))
        XCR(I)= MAX(XC(I),YC(I),ZC(I)) + XHR(I)
      ENDDO
C
      DO I=LFT,LLT
        DXOLD(I)=RX(I)
        DYOLD(I)=RY(I)
        DZOLD(I)=RZ(I)
      ENDDO
C
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=LFT,LLT
          DXOLD(I)=RX0(I)
          DYOLD(I)=RY0(I)
          DZOLD(I)=RZ0(I)
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        X21(I)= (RX2(I)-RX1(I))*DT1
        Y21(I)= (RY2(I)-RY1(I))*DT1
        Z21(I)= (RZ2(I)-RZ1(I))*DT1
        RX(I)= DXOLD(I)+X21(I)*EXX(I)+Y21(I)*EYX(I)+Z21(I)*EZX(I)
        RY(I)= DYOLD(I)+X21(I)*EXY(I)+Y21(I)*EYY(I)+Z21(I)*EZY(I)
        RZ(I)= DZOLD(I)+X21(I)*EXZ(I)+Y21(I)*EYZ(I)+Z21(I)*EZZ(I)
      ENDDO
C-------------------------------
      DO I=LFT,LLT 
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 4,MID(I))
        IFV(I)   = IPM(10 + IF2 + 4,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 4,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 4,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 4))
        AK(I)    =UPARAM(IADBUF + I1 + 4)
        B(I)     =UPARAM(IADBUF + I2 + 4)
        D(I)     =UPARAM(IADBUF + I3 + 4)
        EE(I)    =UPARAM(IADBUF + I4 + 4)
        GF3(I)   =UPARAM(IADBUF + I5 + 4)
        RSCALE(I)=UPARAM(IADBUF + I6 + 4)
        LSCALE(I)=UPARAM(IADBUF + I7 + 4)
        DMN(I)   =UPARAM(IADBUF + I8 + 4)
        DMX(I)   =UPARAM(IADBUF + I9 + 4)
      ENDDO
      IF (NUVAR >= 4) THEN
        COORD_OLD => UVAR(4,1:NEL)
      ELSE
        COORD_OLD => NOT_USED
      ENDIF
      CALL REDEF3(XMOM    ,XK     ,RX    ,XMEP       ,DXOLD ,
     .            RPX     ,TF     ,NPF   ,XC         ,OFF   ,
     .            E6(1,4) ,RPX2   ,ANIM  ,0          ,IPOSXX,
     .            FR_WAVE ,XL0    ,DMN   ,DMX        ,DV    ,
     .            RSCALE  ,LSCALE ,EE    ,GF3        ,IFUNC3,
     .            YIELDX2 ,X0DP   ,AK    ,B          ,D     ,
     .            IECROU  ,IFUNC  ,IFV   ,IFUNC2     ,EPLA  ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
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
           IF (DLIM > ONE) THEN
!         Rupture uniaxiale
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
           ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
C-------------------------------------
      DO I=LFT,LLT
        IADBUF= IPM(7,MID(I)) - 1
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
        RSCALE(I)= UPARAM(IADBUF + I6 + 5) 
        LSCALE(I)= UPARAM(IADBUF + I7 + 5) 
        DMN(I)   = UPARAM(IADBUF + I8 + 5) 
        DMX(I)   = UPARAM(IADBUF + I9 + 5) 
      ENDDO
      IF (NUVAR >= 5) THEN
         COORD_OLD => UVAR(5,1:NEL)
      ELSE
        COORD_OLD => NOT_USED
      ENDIF

      CALL REDEF3(YMOM    ,YK     ,RY    ,YMEP       ,DYOLD ,
     .            RPY     ,TF     ,NPF   ,YC         ,OFF   ,
     .            E6(1,5) ,RPY2   ,ANIM  ,0          ,IPOSYY,
     .            FR_WAVE ,XL0    ,DMN   ,DMX        ,DV    ,
     .            RSCALE  ,LSCALE ,EE    ,GF3        ,IFUNC3,
     .            YIELDY2 ,Y0DP   ,AK    ,B          ,D     ,
     .            IECROU  ,IFUNC  ,IFV   ,IFUNC2     ,EPLA  ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
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
           IF (DLIM > ONE) THEN
!         Rupture uniaxiale
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
           ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I) = CRIT(I)  +  DLIM**2
          ENDIF
        ENDIF
      ENDDO
C-----------------------------------
      DO I=LFT,LLT
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
        RSCALE(I)= UPARAM(IADBUF + I6 + 6) 
        LSCALE(I)= UPARAM(IADBUF + I7 + 6) 
        DMN(I)   = UPARAM(IADBUF + I8 + 6) 
        DMX(I)   = UPARAM(IADBUF + I9 + 6) 
      ENDDO
      IF (NUVAR >= 6) THEN
          COORD_OLD => UVAR(6,1:NEL)
      ELSE
        COORD_OLD => NOT_USED
      ENDIF

      CALL REDEF3(ZMOM    ,ZK     ,RZ    ,ZMEP       ,DZOLD ,
     .            RPZ     ,TF     ,NPF   ,ZC         ,OFF   ,
     .            E6(1,6) ,RPZ2   ,ANIM  ,0          ,IPOSZZ,
     .            FR_WAVE ,XL0    ,DMN   ,DMX        ,DV    ,
     .            RSCALE  ,LSCALE ,EE    ,GF3        ,IFUNC3,
     .            YIELDZ2 ,Z0DP   ,AK    ,B          ,D     ,
     .            IECROU  ,IFUNC  ,IFV   ,IFUNC2     ,EPLA  ,
     .            COORD_OLD,NOT_USED,NOT_USED,NOT_USED)
      DO I=LFT,LLT
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
           IF (DLIM > ONE) THEN
!         Rupture uniaxiale
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
           ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I) = CRIT(I) + DLIM**2
          ENDIF
        ENDIF
      ENDDO
C
C-------------------------------
C     RUPTURE COUPLEE
C-------------------------------
      DO I=LFT,LLT
        IADBUF   = IPM(7,MID(I)) - 1
        ISRATE = NINT(UPARAM(IADBUF + NUPAR + 1))
C---- smoothing factor alpha = 2PI*fc*dt/(2PI*fc*dt+1) ---
        ASRATE = UPARAM(IADBUF + NUPAR + 2)
        ASRATE = (2*PI*ASRATE*DT1)/(ONE+2*PI*ASRATE*DT1)
        IF (ISRATE /= 0) THEN
         CRIT(I) = ASRATE*CRIT(I) + (ONE - ASRATE)*CRITNEW(I)
         CRITNEW(I) = CRIT(I)
        ENDIF
        IF (OFF(I) == ONE .AND. IFAIL(I) == 1 .AND. CRIT(I) > ONE) THEN
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
C     PLASTICITE COUPLEE
C-------------------------------
      CALL REPLA3(XK   ,RPX ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
      CALL REPLA3(YK   ,RPY ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
      CALL REPLA3(ZK   ,RPZ ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
C
      DO I=LFT,LLT
        IADBUF= IPM(7,MID(I)) - 1
        XK(I)=UPARAM(IADBUF + I11 + 1)
        YK(I)=UPARAM(IADBUF + I11 + 2)
        ZK(I)=UPARAM(IADBUF + I11 + 3)
        E(I) = E6(I,1)+E6(I,2)+E6(I,3)+E6(I,4)+E6(I,5)+E6(I,6)
      ENDDO
C
      CALL REPLA3(XK   ,DPX ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
      CALL REPLA3(YK   ,DPY ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
      CALL REPLA3(ZK   ,DPZ ,TF  ,NPF  ,IECROU,
     .            IFUNC,IFV ,EPLA)
C---
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C---
      RETURN
      END
