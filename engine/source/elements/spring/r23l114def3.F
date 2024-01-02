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
Chd|  R23L114DEF3                   source/elements/spring/r23l114def3.F
Chd|-- called by -----------
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|-- calls ---------------
Chd|        MATERIAL_FLOW                 source/tools/seatbelts/material_flow.F
Chd|        REDEF3                        source/elements/spring/redef3.F
Chd|        REDEF_SEATBELT                source/tools/seatbelts/redef_seatbelt.F
Chd|        REPLA3                        source/elements/spring/repla3.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE R23L114DEF3(
     1   SKEW,                 IPM,                  IGEO,                 MID,
     2   PID,                  GEO,                  UPARAM,               FX,
     3   FY,                   FZ,                   E,                    DX,
     4   DY,                   DZ,                   NPF,                  TF,
     5   OFF,                  DPX,                  DPY,                  DPZ,
     6   DPX2,                 DPY2,                 DPZ2,                 FXEP,
     7   FYEP,                 FZEP,                 X0,                   Y0,
     8   Z0,                   XMOM,                 YMOM,                 ZMOM,
     9   RX,                   RY,                   RZ,                   RPX,
     A   RPY,                  RPZ,                  XMEP,                 YMEP,
     B   ZMEP,                 RPX2,                 RPY2,                 RPZ2,
     C   ANIM,                 POSX,                 POSY,                 POSZ,
     D   POSXX,                POSYY,                POSZZ,                FR_WAVE,
     E   E6,                   NEL,                  EXX2,                 EYX2,
     F   EZX2,                 EXY2,                 EYY2,                 EZY2,
     G   EXZ2,                 EYZ2,                 EZZ2,                 AL2DP,
     H   NGL,                  CRIT_NEW,             X0_ERR,               ALDP,
     I   YIELDX,               YIELDY,               YIELDZ,               YIELDX2,
     J   YIELDY2,              YIELDZ2,              EXX,                  EYX,
     K   EZX,                  EXY,                  EYY,                  EZY,
     L   EXZ,                  EYZ,                  EZZ,                  XCR,
     M   RX1,                  RY1,                  RZ1,                  RX2,
     N   RY2,                  RZ2,                  XIN,                  AK,
     O   XM,                   XKM,                  XCM,                  XKR,
     P   VX1,                  VX2,                  VY1,                  VY2,
     Q   VZ1,                  VZ2,                  NUVAR,                UVAR,
     R   MASS,                 DX0,                  DY0,                  DZ0,
     S   RX0,                  RY0,                  RZ0,                  SLIPRING_STRAND,
     T   DFS,                  RING_SLIP,            X02,                  LMIN,
     U   SLIPRING_ID,          UPDATE_FLAG,          RETRACTOR_ID,         ADD_NODE1,
     V   ADD_NODE2,            NC1,                  NC2,                  NC3,
     W   X1DP,                 X2DP,                 X3DP,                 VX3,
     X   VY3,                  VZ3,                  FLAG_SLIPRING_UPDATE, FLAG_RETRACTOR_UPDATE,
     Y   SENSOR_TAB,           UINER,                FR_ID,                FRAM_FACTOR,
     Z   EPS_OLD,              FX_B2,                DPX_B2,               YIELDX_B2,
     1   XX_OLD_B2,            FXEP_B2,              POSX_B2,              EPS_OLD_B2,
     2   NFT      ,            NSENSOR               )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SEATBELT_MOD
      USE SENSOR_MOD
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
      INTEGER, INTENT(IN) :: NFT,NSENSOR
      INTEGER NPF(*),IGEO(NPROPGI,*),NEL,NGL(*),PID(*),MID(*),NUVAR,
     .     IPM(NPROPMI,*),NC1(*),NC2(*),NC3(*),ADD_NODE1(*),ADD_NODE2(*),
     .     SLIPRING_ID(*),UPDATE_FLAG(*),RETRACTOR_ID(*),SLIPRING_STRAND(*),
     .     FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE,FR_ID(*)
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
     .   DX0(*),DY0(*),DZ0(*),RX0(*),RY0(*),RZ0(*),LMIN(*),DFS(*),
     .   RING_SLIP(*),X02(*),VX3(MVSIZ),VY3(MVSIZ),VZ3(MVSIZ),
     .   UINER(*),FRAM_FACTOR(*)
      my_real ,INTENT(INOUT) :: EPS_OLD(MVSIZ),FX_B2(MVSIZ),DPX_B2(MVSIZ),YIELDX_B2(MVSIZ),
     .   XX_OLD_B2(MVSIZ),FXEP_B2(MVSIZ),POSX_B2(MVSIZ),EPS_OLD_B2(MVSIZ)
      TARGET :: UVAR
      DOUBLE PRECISION ALDP(MVSIZ),AL2DP(MVSIZ),X1DP(3,*),X2DP(3,*),X3DP(3,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX(MVSIZ),
     .        IECROU(MVSIZ), IFUNC(MVSIZ), IFV(MVSIZ), IFUNC2(MVSIZ),
     .        I,K,ILENG, J, KK, IFAIL(MVSIZ),IFAIL2(MVSIZ),ADHER(MVSIZ),
     .        NINDX,ISRATE, IFUNC3(MVSIZ),I1,I2,I3,I4,I5,I6,I7,I8,
     .        I9,I10,I11,I12,I13,I14,IF1,IF2,IF3,IF4,IADBUF,NUPAR,NN1,NN2,RET,
     .        FLAG_RETRACTOR_UPDATE_OLD,FLAG,INDEX1(MVSIZ),INDEX2(MVSIZ),COMPT,
     .        DIR,STRD,II,INDEX_SLIP(MVSIZ),COMPT2
C     REAL
      my_real
     .     XK(MVSIZ), YK(MVSIZ),
     .     ZK(MVSIZ),XC(MVSIZ), YC(MVSIZ), ZC(MVSIZ),XH(MVSIZ),
     .     XHR(MVSIZ),DXOLD(MVSIZ), DYOLD(MVSIZ), DZOLD(MVSIZ),
     .     B(MVSIZ), D(MVSIZ), EPLA(MVSIZ),
     .     DV(MVSIZ),VRT(MVSIZ),VRR(MVSIZ),
     .     DMN(MVSIZ),DMX(MVSIZ),XL0(MVSIZ),CRIT(MVSIZ),
     .     XN(MVSIZ),FF(MVSIZ),LSCALE(MVSIZ),EE(MVSIZ),GF3(MVSIZ),
     .     HX(MVSIZ), HY(MVSIZ), HZ(MVSIZ),FX_MAX(MVSIZ),E_OFFSET(MVSIZ),
     .     XK_COMP(MVSIZ),MX_MAX(MVSIZ),DFS_OLD(MVSIZ),NOT_USED,XL02(MVSIZ),
     .     XKP(MVSIZ),DFX(MVSIZ)
      my_real
     .     AT,DT05,XKA,YKA,ZKA,CC,CN,XA,XB,DLIM,VFAIL,
     .     X21, Y21, Z21, EPXY, EPXZ,
     .     VX21, VY21, VZ21, RYAVP, RZAVP,EYZP,EXZP,
     .     RYAV, RZAV,DEN, C, CP, EXYP,
     .     X21PHI, Y21PHI, Z21PHI, VX21PHI, VY21PHI, VZ21PHI,
     .     RYAV1, RZAV1, RYAV1P, RZAV1P,ASRATE,NORM,GAP,LMIN_B2(MVSIZ),
     .     EB(MVSIZ),DXOLDB(MVSIZ),XKP_B2(MVSIZ),DX_B2(MVSIZ),A1
      DOUBLE PRECISION X0DP(MVSIZ),X0DP_B2(MVSIZ),EX2DP(MVSIZ),EY2DP(MVSIZ),EZ2DP(MVSIZ),
     .                 ALDP_B2(MVSIZ)
      my_real ,DIMENSION(:), POINTER :: COORD_OLD
C-----------------------------------------------
C
      NOT_USED = ZERO
C
C-----------------------------------------------
C--   FRAM_FACTOR - used for 2D seatbelts --
C       = 1.0 -> 1D Seatbelt spring
C       = 1/(Nb_fram -1) ----> 2D Seatbelt spring inside belt
C       = 0.5/(Nb_fram -1) --> 2D Seatbelt spring on belt edge
C-----------------------------------------------
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
C
       INDEX1(I) = I
       IADBUF= IPM(7,MID(I)) - 1
       EPLA(I)=ZERO
       XM(I)=MASS(I)
C
       XK(I)=UPARAM(IADBUF + I11 + 1)
       YK(I)=UPARAM(IADBUF + I11 + 2)
       ZK(I)=UPARAM(IADBUF + I11 + 3)
C
       XC(I)=UPARAM(IADBUF + I12 + 1) * FRAM_FACTOR(I)
       YC(I)=UPARAM(IADBUF + I12 + 2) * FRAM_FACTOR(I)
       ZC(I)=UPARAM(IADBUF + I12 + 3) * FRAM_FACTOR(I)
C
       XK_COMP(I) = UPARAM(IADBUF + 117)*GEO(1,PID(I))
C
       XKA=XK(I)*UPARAM(IADBUF + I1 + 1)*FRAM_FACTOR(I)
       YKA=YK(I)*UPARAM(IADBUF + I1 + 2)*FRAM_FACTOR(I)
       ZKA=ZK(I)*UPARAM(IADBUF + I1 + 3)*FRAM_FACTOR(I)
       XKM(I)= MAX(XKA,YKA,ZKA,XK_COMP(I))
C
       HX(I) = UPARAM(IADBUF + I14 + 1)
       HY(I) = UPARAM(IADBUF + I14 + 2)
       HZ(I) = UPARAM(IADBUF + I14 + 3)
C
       XH(I)= MAX(HX(I),HY(I),HZ(I))
       XCM(I)= MAX(XC(I),YC(I),ZC(I))
       XCM(I)= XCM(I)+XH(I)
     
       XKR(I)= MAX(YKA,ZKA) * ALDP(I) * ALDP(I)
       XCR(I)= (MAX(YC(I),ZC(I)) + MAX(HY(I),HZ(I))) * ALDP(I) * ALDP(I)
       VRT(I) = UPARAM(IADBUF + NUPAR + 1)
       VRR(I) = UPARAM(IADBUF + NUPAR + 2)
       IFAIL(I) = NINT(UPARAM(IADBUF + 1 ))
       IFAIL2(I)= NINT(UPARAM(IADBUF + 3 ))
C
       E_OFFSET(I) = UPARAM(IADBUF + 118)
       IF (TT == ZERO) LMIN(I) = UPARAM(IADBUF + 119)
       FX_MAX(I) = UPARAM(IADBUF + 120)
       MX_MAX(I) = UPARAM(IADBUF + 121)
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
          X0(I)= ALDP(I)                   ! cast double vers My_real
        ENDDO
      ENDIF
C
      IF (SCODVER >= 101) THEN
        IF (TT == ZERO) THEN
          DO I=1,NEL
            X0_ERR(I)= ALDP(I)-X0(I)         ! difference entre double et My_real
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
        X0DP(I)= X0(I)                     ! cast double vers My_real
      ENDDO
C
      IF (SCODVER >= 101) THEN
        DO I=1,NEL
          X0DP(I)= X0(I) + X0_ERR(I)         ! difference entre double et My_real
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
          XL0(I)=MAX(X0DP(I),LMIN(I))
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
      COMPT = 0
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = IPM(10 + IF1 + 1,MID(I))
        IFV(I)   = IPM(10 + IF2 + 1,MID(I))
C      unloading curve activated only if cross point is passed
        IF (YIELDX(I) >  UPARAM(IADBUF + 125)) THEN
          IFUNC2(I)= IPM(10 + IF3 + 1,MID(I))
        ELSE
          IFUNC2(I)= IFUNC(I)
        ENDIF
        IFUNC3(I)= IPM(10 + IF4 + 1,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 1))
C
C--     parameters scaled fy fram_factor for springs of 2D seatblets
        AK(I)    = UPARAM(IADBUF + I1 + 1) * FRAM_FACTOR(I)
        B(I)     = UPARAM(IADBUF + I2 + 1)
        D(I)     = UPARAM(IADBUF + I3 + 1)
        EE(I)    = UPARAM(IADBUF + I4 + 1)
        GF3(I)   = UPARAM(IADBUF + I5 + 1)
        FF(I)    = UPARAM(IADBUF + I6 + 1)
        LSCALE(I)= UPARAM(IADBUF + I7 + 1)
        DMN(I)   = UPARAM(IADBUF + I8 + 1)
        DMX(I)   = UPARAM(IADBUF + I9 + 1)
C
        IF (UPDATE_FLAG(I) /= 0) THEN
          COMPT = COMPT + 1
          INDEX2(COMPT) = I
        ENDIF
      ENDDO
C
      IF (NSLIPRING + NRETRACTOR > 0) THEN
C
        IF (COMPT > 0) THEN
C
C---------------------
C       SLIPRING / RETRACTOR UPDATE  - step3
C---------------------
C
          DO II=1,COMPT
            I = INDEX2(II)
C
            IF (SLIPRING_ID(I) > 0) THEN
C--           slipring update step3
              J = SLIPRING_ID(I)
              K = FR_ID(I)
              IF (SLIPRING_STRAND(I) == 1) THEN
                LMIN(I) = UPARAM(IADBUF + 119)
                XL0(I)=MAX(X0DP(I),LMIN(I))
C--             Loading of internal variables for computation of brin 2 
                X02(I) = SLIPRING(J)%FRAM(K)%RESIDUAL_LENGTH(2)
                FX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR2(1)
                DPX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR2(2)
                YIELDX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR2(3)
                XX_OLD_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR2(4)
                FXEP_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR2(5)
                POSX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR2(6)
                EPS_OLD_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR2(7)
                IF (UPDATE_FLAG(I) <= 0) RING_SLIP(I) = X0(I)
              ELSEIF (SLIPRING_STRAND(I) == 2) THEN
                LMIN(I) = UPARAM(IADBUF + 119)
                XL0(I)=MAX(X0DP(I),LMIN(I))
C--             Loading of internal variables for computation of brin 2 
                X02(I) = SLIPRING(J)%FRAM(K)%RESIDUAL_LENGTH(1)
                FX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR1(1)
                DPX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR1(2)
                YIELDX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR1(3)
                XX_OLD_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR1(4)
                FXEP_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR1(5)
                POSX_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR1(6)
                EPS_OLD_B2(I) = SLIPRING(J)%FRAM(K)%INTVAR_STR1(7)
              ENDIF
C
            ELSEIF ((RETRACTOR_ID(I) > 0).AND.(UPDATE_FLAG(I) == -1)) THEN
C--           retractor update of mouth element step3
              OFF(I) = ONE
C
            ELSEIF ((RETRACTOR_ID(I) > 0).AND.(UPDATE_FLAG(I) == -2)) THEN
C--           retractir update of new entered element step3
              OFF(I) = ZERO
              UPDATE_FLAG(I) = 0
              X0(I) = ZERO
C
            ENDIF
C
          ENDDO
C
        ENDIF
C
        COMPT = 0
        COMPT2 = 0
        DO I=1,NEL
          ADHER(I) = 0
          IF (SLIPRING_STRAND(I) > 0) THEN
            COMPT = COMPT + 1
            COMPT2 = COMPT2 + 1
            INDEX2(COMPT) = I
            INDEX_SLIP(COMPT2) = I
            IF (SLIPRING(SLIPRING_ID(I))%NFRAM > 1) THEN
              YC(I) = 0.1*XC(I)
              ZC(I) = 0.1*XC(I)
              YK(I) = 0.01*XK(I)
              ZK(I) = 0.01*XK(I) 
            ELSE
              YC(I) = ZERO
              ZC(I) = ZERO             
            ENDIF
C-          No damping in slipring
            XC(I) = ZERO
            XCM(I)= MAX(XC(I),YC(I),ZC(I))+XH(I)
            XK_COMP(I) = XK(I)
            FX_MAX(I) = EP20
          ELSEIF (SLIPRING_STRAND(I) == -1) THEN
C-          No damping in retractor
            COMPT = COMPT + 1
            INDEX2(COMPT) = I
            XC(I) = ZERO
            YC(I) = ZERO
            ZC(I) = ZERO
            XCM(I)= MAX(XC(I),YC(I),ZC(I))+XH(I)
            XK_COMP(I) = XK(I)
            FX_MAX(I) = EP20
          ENDIF
        ENDDO
C
      ENDIF
C
      IF (NUVAR >=1) COORD_OLD => UVAR(1,1:NEL)
C
      FLAG = 1
      CALL REDEF_SEATBELT(
     1   FX,         XK,         DX,         FXEP,
     2   DXOLD,      DPX,        TF,         NPF,
     3   XC,         OFF,        E6(1,1),    ANIM,
     4   ANIM_FE(11),POSX(1),    XL0,        DMN,
     5   DMX,        LSCALE,     YIELDX,     AK,
     6   IECROU,     IFUNC,      IFUNC2,     COORD_OLD,
     7   FX_MAX,     XK_COMP,    NEL,        INDEX1,
     8   FLAG,       XKP,        EPS_OLD,    FRAM_FACTOR,
     9   NEL,        NFT)
C

C
      IF (NSLIPRING + NRETRACTOR > 0) THEN
C---------------------
C       MATERIAL FLOW COMPUTATION FOR SLIPRING/RETRACTOR
C---------------------
C
C-      Computation of length of 2nd strand
        DO J=1,COMPT2
          I = INDEX_SLIP(J)
          STRD = SLIPRING_STRAND(I)   
          DIR = SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%STRAND_DIRECTION(STRD)
C---      Depending on the orientation of the spring the 2nd strand is defined by IXR(2)/IXR(3) or IXR(1)/IXR(3)       
          IF (((STRD==1).AND.(DIR ==1)).OR.((STRD==2).AND.(DIR==-1))) THEN
            EX2DP(I)=X2DP(1,I)-X3DP(1,I)
            EY2DP(I)=X2DP(2,I)-X3DP(2,I)
            EZ2DP(I)=X2DP(3,I)-X3DP(3,I)
          ELSE
            EX2DP(I)=X3DP(1,I)-X1DP(1,I)
            EY2DP(I)=X3DP(2,I)-X1DP(2,I)
            EZ2DP(I)=X3DP(3,I)-X1DP(3,I)
          ENDIF
          NORM= MAX(EM15,SQRT(EX2DP(I)*EX2DP(I)+EY2DP(I)*EY2DP(I)+EZ2DP(I)*EZ2DP(I)))
          EX2DP(I)= EX2DP(I)/NORM
          EY2DP(I)= EY2DP(I)/NORM
          EZ2DP(I)= EZ2DP(I)/NORM
          ALDP_B2(I)= NORM
        ENDDO
C
        IF (TT == ZERO) THEN
          DO J=1,COMPT2
            I = INDEX_SLIP(J)
            X02(I)= ALDP_B2(I)
          ENDDO
        ENDIF
C
        DO J=1,COMPT2
          I = INDEX_SLIP(J)
          X0DP_B2(I)= X02(I)                     ! cast double vers My_real
          XL02(I)=MAX(X0DP_B2(I),LMIN(I))
          DX_B2(I) = ALDP_B2(I) - X0DP_B2(I)
C         for 2n strand - unloading curve activated only if cross point is passed
          IF (YIELDX_B2(I) >  UPARAM(IADBUF + 125)) THEN
            IFUNC2(I)= IPM(10 + IF3 + 1,MID(I))
          ELSE
            IFUNC2(I)= IFUNC(I)
          ENDIF
        ENDDO
C
C--     Computation of stress of 2nd strand for sliprings
        FLAG = 2
        EB(1:MVSIZ) = ZERO
        DXOLDB(1:MVSIZ) = ZERO
        CALL REDEF_SEATBELT(
     1   FX_B2,      XK,         DX_B2,      FXEP_B2,
     2   DXOLDB,     DPX_B2,     TF,         NPF,
     3   XC,         OFF,        EB,         ANIM,
     4   ANIM_FE(11),POSX_B2,    XL02,       DMN,
     5   DMX,        LSCALE,     YIELDX_B2,  AK,
     6   IECROU,     IFUNC,      IFUNC2,     XX_OLD_B2,
     7   FX_MAX,     XK_COMP,    COMPT2,     INDEX_SLIP,
     8   FLAG,       XKP_B2,     EPS_OLD_B2, FRAM_FACTOR,
     9   NEL,        NFT)
C
        DO J=1,COMPT2
          I = INDEX_SLIP(J)
          IF (ALDP(I) > ALDP_B2(I)) XKP_B2(I) = XKP(I)
          IF (ALDP_B2(I) > ALDP(I)) XKP(I) = XKP_B2(I)
        ENDDO

C--     Computation of material flow for slipring and retractor
        CALL MATERIAL_FLOW(DFS,DFS_OLD,ALDP,SLIPRING_STRAND,XKP,
     1                     OFF,X0,X02,LMIN,UPDATE_FLAG,
     2                     RING_SLIP,SLIPRING_ID,XL0,DX,DXOLD,
     3                     EXX,EYX,EZX,X1DP,X2DP,
     4                     X3DP,ADHER,NC1,NC2,NC3,
     5                     FLAG_SLIPRING_UPDATE,ADD_NODE1,ADD_NODE2,VX1,VY1,
     6                     VZ1,VX2,VY2,VZ2,VX3,
     7                     VY3,VZ3,XC,RETRACTOR_ID,FLAG_RETRACTOR_UPDATE,
     8                     SENSOR_TAB,X0DP,FR_ID,DFX,FX,FX_B2,
     9                     ALDP_B2,X0DP_B2,EX2DP,EY2DP,EZ2DP,XL02,
     A                     XKP_B2,COMPT,INDEX2,NSENSOR)
C
        DO I=1,NEL
          IF (SLIPRING_STRAND(I) > 0) THEN   
            FX(I) = FX(I) + DFX(I)
            E6(I,1) = E6(I,1) + (DX(I)-DXOLD(I))*DFX(I)*HALF
            DX(I) = ALDP(I)-X0DP(I)
            XKM(I) = MAX(XKM(I),FRAM_FACTOR(I)*XK(I)*(ONE + XL0(I)/XL02(I)))
            SLIPRING(SLIPRING_ID(I))%FRAM(FR_ID(I))%SLIP_FORCE(SLIPRING_STRAND(I)) = FX(I)
          ELSEIF (SLIPRING_STRAND(I) < 0) THEN 
            FX(I) = FX(I) + DFX(I)
            E6(I,1) = E6(I,1) + (DX(I)-DXOLD(I))*DFX(I)*HALF
            DX(I) = ALDP(I)-X0DP(I)
            RETRACTOR(RETRACTOR_ID(I))%RET_FORCE = FX(I)
          ENDIF
        END DO
C
      ENDIF
C
C---------------------
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
!         Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I) = ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = 0
        IFV(I)   = IPM(10 + IF2 + 2,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 2,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 2,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 2))
        IF (IECROU(I) > 0) IFUNC(I) = -1 
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
      CALL REDEF3(
     1   FY,         YK,         DY,         FYEP,
     2   DYOLD,      DPY,        TF,         NPF,
     3   YC,         OFF,        E6(1,2),    DPY2,
     4   ANIM(KK),   ANIM_FE(12),POSY,       FR_WAVE,
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDY,     ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       COORD_OLD,
     A   FX_MAX,     NOT_USED,   NOT_USED,   NEL,
     B   NFT)
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
!         Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I) = ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = 0
        IFV(I)   = IPM(10 + IF2 + 3,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 3,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 3,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 3))
        IF (IECROU(I) > 0) IFUNC(I) = -1 
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
      CALL REDEF3(
     1   FZ,         ZK,         DZ,         FZEP,
     2   DZOLD,      DPZ,        TF,         NPF,
     3   ZC,         OFF,        E6(1,3),    DPZ2,
     4   ANIM(KK),   ANIM_FE(13),POSZ,       FR_WAVE,
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELDZ,     ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       COORD_OLD,
     A   FX_MAX,     NOT_USED,   NOT_USED,   NEL,
     B   NFT)
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
!         Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I) = ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C---------------------
C     ROTATIONS
C---------------------
      DO I=1,NEL
        IADBUF= IPM(7,MID(I)) - 1
        XIN(I)=  UINER(I)
        XK(I) =  UPARAM(IADBUF + I11 + 4)
        XC(I) =  UPARAM(IADBUF + I12 + 4)
        YK(I) =  UPARAM(IADBUF + I11 + 5)
        YC(I) =  UPARAM(IADBUF + I12 + 5)
        ZK(I) =  UPARAM(IADBUF + I11 + 6)
        ZC(I) =  UPARAM(IADBUF + I12 + 6)
        HX(I) =  UPARAM(IADBUF + I14 + 4)
        HY(I) =  UPARAM(IADBUF + I14 + 5)
        HZ(I) =  UPARAM(IADBUF + I14 + 6)
C
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
        IFUNC(I) = 0
        IFV(I)   = IPM(10 + IF2 + 4,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 4,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 4,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 4))
        IF (IECROU(I) > 0) IFUNC(I) = -1 
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
      CALL REDEF3(
     1   XMOM,     XK,       RX,       XMEP,
     2   DXOLD,    RPX,      TF,       NPF,
     3   XC,       OFF,      E6(1,4),  RPX2,
     4   ANIM,     0,        POSXX,    FR_WAVE,
     5   XL0,      DMN,      DMX,      DV,
     6   FF,       LSCALE,   EE,       GF3,
     7   IFUNC3,   YIELDX2,  ALDP,     AK,
     8   B,        D,        IECROU,   IFUNC,
     9   IFV,      IFUNC2,   EPLA,     COORD_OLD,
     A   MX_MAX,   NOT_USED, NOT_USED, NEL,
     B   NFT)
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
!         Rupture uniaxiale
            IF ((XA*DLIM) > ONE) THEN
              OFF(I)= ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = 0
        IFV(I)   = IPM(10 + IF2 + 5,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 5,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 5,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 5))
        IF (IECROU(I) > 0) IFUNC(I) = -1 
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
      CALL REDEF3(
     1   YMOM,     YK,       RY,       YMEP,
     2   DYOLD,    RPY,      TF,       NPF,
     3   YC,       OFF,      E6(1,5),  RPY2,
     4   ANIM,     0,        POSYY,    FR_WAVE,
     5   XL0,      DMN,      DMX,      DV,
     6   FF,       LSCALE,   EE,       GF3,
     7   IFUNC3,   YIELDY2,  ALDP,     AK,
     8   B,        D,        IECROU,   IFUNC,
     9   IFV,      IFUNC2,   EPLA,     COORD_OLD,
     A   MX_MAX,   NOT_USED, NOT_USED, NEL,
     B   NFT)
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
!         Rupture uniaxiale
            IF ((XA*DLIM) > 1) THEN
              OFF(I) = ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        IFUNC(I) = 0
        IFV(I)   = IPM(10 + IF2 + 6,MID(I))
        IFUNC2(I)= IPM(10 + IF3 + 6,MID(I))
        IFUNC3(I)= IPM(10 + IF4 + 6,MID(I))
        IECROU(I)= NINT(UPARAM(IADBUF + I13 + 6))
        IF (IECROU(I) > 0) IFUNC(I) = -1 
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
      CALL REDEF3(
     1   ZMOM,     ZK,       RZ,       ZMEP,
     2   DZOLD,    RPZ,      TF,       NPF,
     3   ZC,       OFF,      E6(1,6),  RPZ2,
     4   ANIM,     0,        POSZZ,    FR_WAVE,
     5   XL0,      DMN,      DMX,      DV,
     6   FF,       LSCALE,   EE,       GF3,
     7   IFUNC3,   YIELDZ2,  ALDP,     AK,
     8   B,        D,        IECROU,   IFUNC,
     9   IFV,      IFUNC2,   EPLA,     COORD_OLD,
     A   MX_MAX,   NOT_USED, NOT_USED, NEL,
     B   NFT)
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
!         Rupture uniaxiale
            IF ((XA*DLIM) > 1) THEN
              OFF(I) = ZERO
              NINDX = NINDX + 1
              INDX(NINDX) = I
              IDEL7NOK = 1
            ENDIF
          ELSE
!         Rupture multiaxiale
            CRIT(I)= CRIT(I) + XA *(DLIM/XL0(I))**XB
          ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
        E(I) = E6(I,1)+E6(I,2)+E6(I,3)+E6(I,4)+E6(I,5)+E6(I,6)
      ENDDO
C-------------------------------
C     RUPTURE COUPLEE
C-------------------------------
      DO I=1,NEL
        IADBUF   = IPM(7,MID(I)) - 1
        ISRATE = NINT(UPARAM(IADBUF + NUPAR + 27))
C---- smoothing factor alpha = 2PI*fc*dt/(2PI*fc*dt+1) ---
        ASRATE = (2*PI*UPARAM(IADBUF + NUPAR + 28)*DT1)/(ONE+2*PI*UPARAM(IADBUF + NUPAR + 28)*DT1)
        IF (ISRATE /= 0) THEN
          CRIT(I) = ASRATE*CRIT(I) + (ONE - ASRATE)*CRIT_NEW(I)
          CRIT_NEW(I) = CRIT(I)
        ENDIF
        IF (OFF(I) == ONE .AND. IFAIL(I) == 1) THEN
           IF (CRIT(I) > ONE) THEN
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
C        XM(I)=XM(I)*XL0(I)
        XKM(I)=XKM(I)/XL0(I)
        XCM(I)=XCM(I)/XL0(I)
C        XIN(I)=XIN(I)*XL0(I)
        XKR(I)=XKR(I)/XL0(I)
        XCR(I)=XCR(I)/XL0(I)
      ENDDO
C---
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C---
      RETURN
      END
