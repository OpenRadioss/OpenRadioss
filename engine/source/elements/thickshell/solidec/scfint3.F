Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SCFINT3                       source/elements/thickshell/solidec/scfint3.F
Chd|-- called by -----------
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCFINT3(SIG,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   VOL,QVIS,
     .   PX1H1, PX1H2, PX2H1, PX2H2, 
     .   PX3H1, PX3H2, PX4H1, PX4H2, 
     .   RX0, RY0, SX0, SY0,
     .   EINT,RHO,Q,EPLAS,EPSD,
     .   EPSDM,SIGM,EINTM,EINTO,RHOM,QM,EPLASM,
     .   NU,ZI,WI,VOLG,MM,OFF,
     .   VOL0,VOL0G,G_PLA,G_EPSD,NEL)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "nsvis_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER G_PLA,G_EPSD,NEL
C     REAL
      my_real
     .   SIG(NEL,6),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX5(*), PX6(*), PX7(*), PX8(*),  
     .   PY5(*), PY6(*), PY7(*), PY8(*),  
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),  
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*),QVIS(*),
     .   PX1H1(*), PX1H2(*), PX2H1(*), PX2H2(*), 
     .   PX3H1(*), PX3H2(*), PX4H1(*), PX4H2(*), 
     .   RX0(*), RY0(*), SX0(*), SY0(*),
     .   EINT(*),RHO(*),Q(*),EPLASM(*),EPLAS(*),
     .   SIGM(NEL,6),EINTM(*),EINTO(*),RHOM(*),QM(*),EPSD(*),EPSDM(*),
     .   NU(*),ZI,WI,VOLG(*),MM(MVSIZ,2),OFF(*),
     +   VOL0(*),VOL0G(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),
     .   RX1(MVSIZ), RY1(MVSIZ), SX1(MVSIZ), SY1(MVSIZ),
     .   SS1,SS2,SS3,SVM0,FINT,FINT1,FINT2,
     .   XH1I,XH2I,YH1I,YH2I,NU1(MVSIZ),FAC(MVSIZ)
C-----------------------------------------------
      DO I=LFT,LLT
       S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*VOL(I)
       S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*VOL(I)
       S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*VOL(I)
       S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)
       S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)
       S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)
      ENDDO
      DO I=LFT,LLT
        FINT=S1(I)*PX1(I)+S4(I)*PY1(I)+S6(I)*PZ1(I)
        F11(I)=F11(I)-FINT
        F17(I)=F17(I)+FINT
        FINT=S2(I)*PY1(I)+S4(I)*PX1(I)+S5(I)*PZ1(I)
        F21(I)=F21(I)-FINT
        F27(I)=F27(I)+FINT
        FINT=S3(I)*PZ1(I)+S6(I)*PX1(I)+S5(I)*PY1(I)
        F31(I)=F31(I)-FINT
        F37(I)=F37(I)+FINT
C
        FINT=S1(I)*PX2(I)+S4(I)*PY2(I)+S6(I)*PZ2(I)
        F12(I)=F12(I)-FINT
        F18(I)=F18(I)+FINT
        FINT=S2(I)*PY2(I)+S4(I)*PX2(I)+S5(I)*PZ2(I)
        F22(I)=F22(I)-FINT
        F28(I)=F28(I)+FINT
        FINT=S3(I)*PZ2(I)+S6(I)*PX2(I)+S5(I)*PY2(I)
        F32(I)=F32(I)-FINT
        F38(I)=F38(I)+FINT
C
        FINT=S1(I)*PX3(I)+S4(I)*PY3(I)+S6(I)*PZ3(I)
        F13(I)=F13(I)-FINT
        F15(I)=F15(I)+FINT
        FINT=S2(I)*PY3(I)+S4(I)*PX3(I)+S5(I)*PZ3(I)
        F23(I)=F23(I)-FINT
        F25(I)=F25(I)+FINT
        FINT=S3(I)*PZ3(I)+S6(I)*PX3(I)+S5(I)*PY3(I)
        F33(I)=F33(I)-FINT
        F35(I)=F35(I)+FINT
C
        FINT=S1(I)*PX4(I)+S4(I)*PY4(I)+S6(I)*PZ4(I)
        F14(I)=F14(I)-FINT
        F16(I)=F16(I)+FINT
        FINT=S2(I)*PY4(I)+S4(I)*PX4(I)+S5(I)*PZ4(I)
        F24(I)=F24(I)-FINT
        F26(I)=F26(I)+FINT
        FINT=S3(I)*PZ4(I)+S6(I)*PX4(I)+S5(I)*PY4(I)
        F34(I)=F34(I)-FINT
        F36(I)=F36(I)+FINT
      ENDDO
Cqz +++ non constante part------------
      DO I=LFT,LLT
        RY1(I)= ZI*RY0(I)
        RX1(I)= ZI*RX0(I)
        SY1(I)= ZI*SY0(I)
        SX1(I)= ZI*SX0(I)
        NU1(I) = NU(I)/(ONE -NU(I))
      ENDDO
C---- asymetric part of gama------------
      DO I=LFT,LLT
Cqz ---- I =1--------------
        XH1I =-RY1(I)*PX1H1(I)
        XH2I = SY1(I)*PX1H2(I)
        YH1I = RX1(I)*PX1H1(I)
        YH2I =-SX1(I)*PX1H2(I)
        FINT=S1(I)*(XH1I+XH2I)-NU(I)*XH1I*S2(I)
        FINT=FINT-(NU1(I)*XH2I+NU(I)*XH1I)*S3(I)+S4(I)*(YH1I+YH2I)
        F11(I)=F11(I)+FINT
        F17(I)=F17(I)-FINT
        FINT=S2(I)*(YH1I+YH2I)-NU(I)*YH2I*S1(I)
        FINT=FINT-(NU(I)*YH2I+NU1(I)*YH1I)*S3(I)+S4(I)*(XH1I+XH2I)
        F21(I)=F21(I)+FINT
        F27(I)=F27(I)-FINT
        FINT= S6(I)*XH1I+S5(I)*YH2I
        F31(I)=F31(I)+FINT
        F37(I)=F37(I)-FINT
Cqz ---- I =2--------------
        XH1I =-RY1(I)*PX2H1(I)
        XH2I = SY1(I)*PX2H2(I)
        YH1I = RX1(I)*PX2H1(I)
        YH2I =-SX1(I)*PX2H2(I)
        FINT=S1(I)*(XH1I+XH2I)-NU(I)*XH1I*S2(I)
        FINT=FINT-(NU(I)*XH1I+NU1(I)*XH2I)*S3(I)+S4(I)*(YH1I+YH2I)
        F12(I)=F12(I)+FINT
        F18(I)=F18(I)-FINT
        FINT=S2(I)*(YH1I+YH2I)-NU(I)*YH2I*S1(I)
        FINT=FINT-(NU(I)*YH2I+NU1(I)*YH1I)*S3(I)+S4(I)*(XH1I+XH2I)
        F22(I)=F22(I)+FINT
        F28(I)=F28(I)-FINT
        FINT= S6(I)*XH1I+S5(I)*YH2I
        F32(I)=F32(I)+FINT
        F38(I)=F38(I)-FINT
Cqz ---- I =3--------------
        XH1I =-RY1(I)*PX3H1(I)
        XH2I = SY1(I)*PX3H2(I)
        YH1I = RX1(I)*PX3H1(I)
        YH2I =-SX1(I)*PX3H2(I)
        FINT=S1(I)*(XH1I+XH2I)-NU(I)*XH1I*S2(I)
        FINT=FINT-(NU(I)*XH1I+NU1(I)*XH2I)*S3(I)+S4(I)*(YH1I+YH2I)
        F13(I)=F13(I)+FINT
        F15(I)=F15(I)-FINT
        FINT=S2(I)*(YH1I+YH2I)-NU(I)*YH2I*S1(I)
        FINT=FINT-(NU(I)*YH2I+NU1(I)*YH1I)*S3(I)+S4(I)*(XH1I+XH2I)
        F23(I)=F23(I)+FINT
        F25(I)=F25(I)-FINT
        FINT= S6(I)*XH1I+S5(I)*YH2I
        F33(I)=F33(I)+FINT
        F35(I)=F35(I)-FINT
Cqz ---- I =4--------------
        XH1I =-RY1(I)*PX4H1(I)
        XH2I = SY1(I)*PX4H2(I)
        YH1I = RX1(I)*PX4H1(I)
        YH2I =-SX1(I)*PX4H2(I)
        FINT=S1(I)*(XH1I+XH2I)-NU(I)*XH1I*S2(I)
        FINT=FINT-(NU(I)*XH1I+NU1(I)*XH2I)*S3(I)+S4(I)*(YH1I+YH2I)
        F14(I)=F14(I)+FINT
        F16(I)=F16(I)-FINT
        FINT=S2(I)*(YH1I+YH2I)-NU(I)*YH2I*S1(I)
        FINT=FINT-(NU(I)*YH2I+NU1(I)*YH1I)*S3(I)+S4(I)*(XH1I+XH2I)
        F24(I)=F24(I)+FINT
        F26(I)=F26(I)-FINT
        FINT= S6(I)*XH1I+S5(I)*YH2I
        F34(I)=F34(I)+FINT
        F36(I)=F36(I)-FINT
      ENDDO
C---- symetric part of gama------------
C 1 1 -1 -1 -1 -1 1 1
C 1 -1 -1 1 -1 1 1 -1
      DO I=LFT,LLT
        XH1I =-ONE_OVER_8*RY1(I)
        XH2I = ONE_OVER_8*SY1(I)
        YH1I = ONE_OVER_8*RX1(I)
        YH2I =-ONE_OVER_8*SX1(I)	
Cqz ---- ---------------
        FINT1=(S1(I)-NU(I)*S2(I)-NU(I)*S3(I))*XH1I+S4(I)*YH1I
        FINT2=(S1(I)-NU1(I)*S3(I))*XH2I+S4(I)*YH2I
C
        F11(I)=F11(I)-FINT1-FINT2
        F12(I)=F12(I)-FINT1+FINT2
        F13(I)=F13(I)+FINT1+FINT2
        F14(I)=F14(I)+FINT1-FINT2
        F15(I)=F15(I)+FINT1+FINT2
        F16(I)=F16(I)+FINT1-FINT2
        F17(I)=F17(I)-FINT1-FINT2
        F18(I)=F18(I)-FINT1+FINT2
C
        FINT1=(S2(I)-NU1(I)*S3(I))*YH1I+S4(I)*XH1I
        FINT2=(S2(I)-NU(I)*S1(I)-NU(I)*S3(I))*YH2I+S4(I)*XH2I
C
        F21(I)=F21(I)-FINT1-FINT2
        F22(I)=F22(I)-FINT1+FINT2
        F23(I)=F23(I)+FINT1+FINT2
        F24(I)=F24(I)+FINT1-FINT2
        F25(I)=F25(I)+FINT1+FINT2
        F26(I)=F26(I)+FINT1-FINT2
        F27(I)=F27(I)-FINT1-FINT2
        F28(I)=F28(I)-FINT1+FINT2
C
        FINT1=S6(I)*XH1I
        FINT2=S5(I)*YH2I
C
        F31(I)=F31(I)-FINT1-FINT2
        F32(I)=F32(I)-FINT1+FINT2
        F33(I)=F33(I)+FINT1+FINT2
        F34(I)=F34(I)+FINT1-FINT2
        F35(I)=F35(I)+FINT1+FINT2
        F36(I)=F36(I)+FINT1-FINT2
        F37(I)=F37(I)-FINT1-FINT2
        F38(I)=F38(I)-FINT1+FINT2
      ENDDO
Cqz --- non constante part------------
C------------------------------------------------   -
Cqz - for hourglass yield critiron-----
      DO I=LFT,LLT
        SS1 =SIG(I,1)-SIG(I,2)
        SS2 =SIG(I,2)-SIG(I,3)
        SS3 =SIG(I,1)-SIG(I,3)
       SVM0 =(SS1*SS1+SS2*SS2+SS3*SS3)*HALF + THREE*(SIG(I,4)*SIG(I,4)
     .      + SIG(I,5)*SIG(I,5)+SIG(I,6)*SIG(I,6))
        MM(I,1) = MAX(SVM0,MM(I,1))
        MM(I,2) = MIN(SVM0,MM(I,2))
      ENDDO
C----------------------------------------------    /
Cqz - post-traitement-valeur moyenne au sens a'=(_/  a dv ) /v
      DO I=LFT,LLT
         FAC(I) = OFF(I)*VOL(I)/VOLG(I)
         SIGM(I,1) = SIGM(I,1) + FAC(I) * SIG(I,1)
         SIGM(I,2) = SIGM(I,2) + FAC(I) * SIG(I,2)
         SIGM(I,3) = SIGM(I,3) + FAC(I) * SIG(I,3)
         SIGM(I,4) = SIGM(I,4) + FAC(I) * SIG(I,4)
         SIGM(I,5) = SIGM(I,5) + FAC(I) * SIG(I,5)
         SIGM(I,6) = SIGM(I,6) + FAC(I) * SIG(I,6)
         RHOM(I)   = RHOM(I)   + FAC(I) * RHO(I)
         EINTM(I)  = EINTM(I)+OFF(I)*VOL0(I)/VOL0G(I)*(EINT(I)-EINTO(I))
         QM(I)     = QM(I)     + FAC(I) * Q(I)
      ENDDO
c
      IF (G_PLA > 0) THEN
        DO I=LFT,LLT
          EPLASM(I)  = EPLASM(I)  + FAC(I) * EPLAS(I)
        ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
        DO I=LFT,LLT
          EPSDM(I) = EPSDM(I) + FAC(I) * EPSD(I)
        ENDDO
      ENDIF
Cqz ---------------
      RETURN
      END
