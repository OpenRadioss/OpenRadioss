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
Chd|  M17LAW                        source/materials/mat/mat017/m17law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M17LAW(PM  ,OFF  ,SIG   ,EINT ,RHO,
     2                  RK  ,RE   ,VORTI ,WXX  ,WYY,
     3                  WZZ ,VOLN ,MAT   ,VIS  ,D1 ,
     4                  D2  ,D3   ,D4    ,D5   ,D6 ,
     5                  NEL ,ALOGEY, SSP, RHO0, TMU,
     6                  AMU, AMU2, PSH, PC, ESPE,
     7                  C1, C2, C3, C4, C5, C6, DF, DPDM)
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NEL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), RK(*), RE(*),
     .   VORTI(*),WXX(*),WYY(*),WZZ(*),VOLN(MVSIZ),VIS(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*), ALOGEY(*), SSP(*),
     .   RHO0(*), TMU(*), AMU(*), AMU2(*), PSH(*), PC(*), ESPE(*),
     .   C1(*), C2(*), C3(*), C4(*), C5(*), C6(*), DF(*), DPDM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real
     .   VIS2(MVSIZ), 
     .   DAV(MVSIZ),
     .   YP0, CMU, AX, E, A, XMU,
     .   XM, XK, XE, YPLUS, RK2T, FAC,
     .   RHO0_1, VIS_1, PC_1, C1_1, C2_1,
     .   C3_1, C4_1, C5_1, C6_1, PSH_1     
C-----------------------------------------------
C
      ! SAUVEGARDE VORTICITE SI ANIM
      IF((ANIM_E(10)==1 .OR. ANIM_SE(10)==1).AND. DT1/=0.)THEN
C      IF(ANIM_E(10)==1.AND. DT1/=0.)THEN
        FAC=FOUR/DT1
        IF(N2D==0)THEN
          DO 5 I=1,NEL
 5        VORTI(I)=FAC*SQRT(WXX(I)**2+WYY(I)**2+WZZ(I)**2)
        ELSE
          DO 6 I=1,NEL
 6        VORTI(I)=FAC*WZZ(I)
        ENDIF
      ENDIF
C
      MX     =MAT(1)
C
      RHO0_1=PM( 1,MX)
      VIS_1 =PM(24,MX)
      PC_1  =PM(37,MX)
      C1_1  =PM(31,MX)-PM(88,MX)
      C2_1  =PM(32,MX)
      C3_1  =PM(33,MX)
      C4_1  =PM(34,MX)
      C5_1  =PM(35,MX)
      C6_1  =PM(36,MX)
      PSH_1 =PM(88,MX)
C
      DO 10 I=1,NEL
      RHO0(I)=RHO0_1
      VIS(I) =VIS_1
      PC(I)  =PC_1
      C1(I)  =C1_1
      C2(I)  =C2_1
      C3(I)  =C3_1
      C4(I)  =C4_1
      C5(I)  =C5_1
      C6(I)  =C6_1
      PSH(I) =PSH_1
   10 CONTINUE
C
      MX     =MAT(1)
      IF(JPOR/=2)THEN
      DO 20 I=1,NEL
      YP0=PM(51,MX)
      CMU=PM(81,MX)
      AX =PM(47,MX)
      E  =PM(48,MX)
      A  =PM(49,MX)
      XMU      =RHO(I)*VIS(I)
      XM       =RHO(I)*VOLN(I)
      XK       =RK(I)/XM
      XE       = MAX(EM15,RE(I)/XM)
      YPLUS    =CMU*XK**2/(AX*XE*VIS(I))
      YPLUS    = MAX(YPLUS,YP0)
      ALOGEY(I)= A * LOG(E*YPLUS)
      VIS(I)   =XMU*AX*YPLUS/ALOGEY(I)
      TMU(I)   =VIS(I)-XMU
   20 CONTINUE
      ELSE
        DO 21 I=1,NEL
         VIS(I)=ZERO
         TMU(I)=ZERO 
   21   CONTINUE
      ENDIF
C
      DO 30 I=1,NEL
      VIS2(I)=TWO*VIS(I)
   30 DAV(I) =-(D1(I)+D2(I)+D3(I))*THIRD
C------------------------------
C     CONTRAINTES DEVIATORIQUES
C------------------------------
      DO 40 I=1,NEL
      SIG(I,1)=VIS2(I)*(D1(I)+DAV(I))
      SIG(I,2)=VIS2(I)*(D2(I)+DAV(I))
      SIG(I,3)=VIS2(I)*(D3(I)+DAV(I))
      SIG(I,4)=VIS(I) *D4(I)
      SIG(I,5)=VIS(I) *D5(I)
   40 SIG(I,6)=VIS(I) *D6(I)
C
      DO 50 I=1,NEL
      DF(I)  =RHO0(I)/RHO(I)
      AMU(I) =ONE/DF(I)-ONE
      AMU2(I)= MAX(ZERO,AMU(I))**2
   50 ESPE(I)=DF(I)*EINT(I)/VOLN(I)
C
      DO 60 I=1,NEL
      RK2T=TWO*RK(I)/(THREE*VOLN(I))
      DPDM(I) = DPDM(I)
     .     +(C5(I)+C6(I)*AMU(I))*DF(I)*DF(I)*RK2T + RK2T*DF(I)
   60 CONTINUE
C
      DO 70 I=1,NEL
   70 SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
C
      RETURN
      END
