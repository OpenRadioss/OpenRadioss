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
Chd|  S6CFINT3                      source/elements/thickshell/solide6c/s6cfint3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CFINT3(SIG,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX1H, PX2H, PX3H, 
     .   PY1H, PY2H, PY3H, 
     .   PZ1H, PZ2H, PZ3H, 
     .   JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .   B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,
     .   VOL,QVIS,
     .   EINT,RHO,Q,EPLA,EPSD,
     .   EPSDM,SIGM,EINTM,RHOM,QM,EPLASM,
     .   ZI,WI,VOLG,OFF,NU,
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
     .   PX1H(*), PX2H(*), PX3H(*),   
     .   PY1H(*), PY2H(*), PY3H(*),  
     .   PZ1H(*), PZ2H(*), PZ3H(*),   
     .   JI33(*),B1X(MVSIZ,2),B1Y(MVSIZ,2),B2X(MVSIZ,2),B2Y(MVSIZ,2),
     .   B1XH(MVSIZ,2),B1YH(MVSIZ,2),B2XH(MVSIZ,2),B2YH(MVSIZ,2),
     .   B1122(*),B1221(*),B2212(*),B1121(*),
     .   B1122H(*),B1221H(*),B2212H(*),B1121H(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   VOL(*),QVIS(*),
     .   EINT(*),RHO(*),Q(*),EPLASM(*),EPLA(*),
     .   SIGM(NEL,6),EINTM(*),RHOM(*),QM(*),EPSD(*),EPSDM(*),
     .   ZI,WI,VOLG(*),OFF(*),NU(*),VOL0(*),VOL0G(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),
     .   RX1(MVSIZ), RY1(MVSIZ), SX1(MVSIZ), SY1(MVSIZ),
     .   FINT,FINTX,FINTY,FINTZ,FAC(MVSIZ),FXC,FYC,FINSX,FINSY
      my_real 
     .  NU1,FINSZ
C=======================================================================
      DO I=LFT,LLT
       S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*VOL(I)
       S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*VOL(I)
       S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*VOL(I)
       S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)
       S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)
       S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)
      ENDDO
C                                                                     12
C  -------constant part---------
       DO I=LFT,LLT
        FINT=S1(I)*PX1(I)+S4(I)*PY1(I)
        FXC = JI33(I)*S6(I)
        FYC = JI33(I)*S5(I)
        FINTX=S1(I)*PX4(I)+S4(I)*PY4(I) + THREE*FXC
        FINSX=S6(I)*(B1X(I,1)-B1X(I,2))-S5(I)*(B2X(I,1)-B2X(I,2))-FXC
        F11(I)=F11(I)-FINT+FINTX+FINSX
        F14(I)=F14(I)-FINT-FINTX-FINSX
        FINT=S2(I)*PY1(I)+S4(I)*PX1(I)
        FINTY=S2(I)*PY4(I)+S4(I)*PX4(I) + THREE*FYC
        FINSY=S6(I)*(B1Y(I,1)-B1Y(I,2))-S5(I)*(B2Y(I,1)-B2Y(I,2))-FYC
        F21(I)=F21(I)-FINT+FINTY+FINSY
        F24(I)=F24(I)-FINT-FINTY-FINSY
        FINT=S3(I)*PZ1(I)+HALF*(S6(I)*PX1(I)+S5(I)*PY1(I))
        FINTZ=S3(I)*PZ4(I)
        F31(I)=F31(I)-FINT+FINTZ
        F34(I)=F34(I)-FINT-FINTZ
C
        FINT=S1(I)*PX2(I)+S4(I)*PY2(I)
        FINSX=S6(I)*(B1221(I)+B1X(I,2))-S5(I)*(B1121(I)+B2X(I,2))
        F12(I)=F12(I)-FINT+FINTX+FINSX
        F15(I)=F15(I)-FINT-FINTX-FINSX
        FINT=S2(I)*PY2(I)+S4(I)*PX2(I)
        FINSY=S6(I)*(B2212(I)+B1Y(I,2))-S5(I)*(B1122(I)+B2Y(I,2))
        F22(I)=F22(I)-FINT+FINTY+FINSY
        F25(I)=F25(I)-FINT-FINTY-FINSY
        FINT=S3(I)*PZ2(I)+(S6(I)*PX2(I)+S5(I)*PY2(I))*HALF
        F32(I)=F32(I)-FINT+FINTZ
        F35(I)=F35(I)-FINT-FINTZ
C
        FINT=S1(I)*PX3(I)+S4(I)*PY3(I)
        FINSX=-S6(I)*(B1122(I)+B1X(I,1))+S5(I)*(B1121(I)+B2X(I,1))
        F13(I)=F13(I)-FINT+FINTX+FINSX
        F16(I)=F16(I)-FINT-FINTX-FINSX
        FINT=S2(I)*PY3(I)+S4(I)*PX3(I)
        FINSY=-S6(I)*(B2212(I)+B1Y(I,1))+S5(I)*(B1221(I)+B2Y(I,1))
        F23(I)=F23(I)-FINT+FINTY+FINSY
        F26(I)=F26(I)-FINT-FINTY-FINSY
        FINT=S3(I)*PZ3(I)+(S6(I)*PX3(I)+S5(I)*PY3(I))*HALF
        F33(I)=F33(I)-FINT+FINTZ
        F36(I)=F36(I)-FINT-FINTZ
       ENDDO
C   --- non constante part------------
C                                                                     12
      DO I=LFT,LLT
       S1(I) = ZI*S1(I)
       S2(I) = ZI*S2(I)
       S3(I) = ZI*S3(I)
       S4(I) = ZI*S4(I)
       S5(I) = ZI*S5(I)
       S6(I) = ZI*S6(I)
      ENDDO
       DO I=LFT,LLT
        NU1 = NU(I)/(ONE - NU(I))
        FXC =S1(I)-(S2(I)+S3(I))*NU(I)
        FYC =S2(I)-(S1(I)+S3(I))*NU(I)
        FINSX = S1(I)*NU1
        FINSY = S2(I)*NU1
        FINSZ = S3(I)*NU1
        FINT=FXC*PX1H(I)+S4(I)*PY1H(I)
        FINTX=(S1(I)-FINSZ)*PX1(I)+S4(I)*PY1(I)
        FINTX=FINTX+S6(I)*(B1XH(I,1)-B1XH(I,2))
     .             -S5(I)*(B2XH(I,1)-B2XH(I,2))
        F11(I)=F11(I)-FINT+FINTX
        F14(I)=F14(I)-FINT-FINTX
        FINT=(S2(I)-FINSZ)*PY1H(I)+S4(I)*PX1H(I)
        FINTY=FYC*PY1(I)+S4(I)*PX1(I)
        FINTY=FINTY+S6(I)*(B1YH(I,1)-B1YH(I,2))
     .             -S5(I)*(B2YH(I,1)-B2YH(I,2))
        F21(I)=F21(I)-FINT+FINTY
        F24(I)=F24(I)-FINT-FINTY
        FINT=(S3(I)-FINSY)*PZ1H(I)+
     .       (S6(I)*PX1H(I)+S5(I)*PY1H(I))*HALF
        FINTZ=(S3(I)-FINSX)*PZ1(I)
        F31(I)=F31(I)-FINT+FINTZ
        F34(I)=F34(I)-FINT-FINTZ
C
        FINT=FXC*PX2H(I)+S4(I)*PY2H(I)
        FINTX=(S1(I)-FINSZ)*PX2(I)+S4(I)*PY2(I)
        FINTX=FINTX+
     .        S6(I)*(B1221H(I)+B1XH(I,2))-S5(I)*(B1121H(I)+B2XH(I,2))
        F12(I)=F12(I)-FINT+FINTX
        F15(I)=F15(I)-FINT-FINTX
        FINT=(S2(I)-FINSZ)*PY2H(I)+S4(I)*PX2H(I)
        FINTY=FYC*PY2(I)+S4(I)*PX2(I)
        FINTY=FINTY+
     .        S6(I)*(B2212H(I)+B1YH(I,2))-S5(I)*(B1122H(I)+B2YH(I,2))
        F22(I)=F22(I)-FINT+FINTY
        F25(I)=F25(I)-FINT-FINTY
        FINT=(S3(I)-FINSY)*PZ2H(I)+
     .       (S6(I)*PX2H(I)+S5(I)*PY2H(I))*HALF
        FINTZ=(S3(I)-FINSX)*PZ2(I)
        F32(I)=F32(I)-FINT+FINTZ
        F35(I)=F35(I)-FINT-FINTZ
C
        FINT=FXC*PX3H(I)+S4(I)*PY3H(I)
        FINTX=(S1(I)-FINSZ)*PX3(I)+S4(I)*PY3(I)
        FINTX=FINTX
     .        -S6(I)*(B1122H(I)+B1XH(I,1))+S5(I)*(B1121H(I)+B2XH(I,1))
        F13(I)=F13(I)-FINT+FINTX
        F16(I)=F16(I)-FINT-FINTX
        FINT=(S2(I)-FINSZ)*PY3H(I)+S4(I)*PX3H(I)
        FINTY=FYC*PY3(I)+S4(I)*PX3(I)
        FINTY=FINTY
     .        -S6(I)*(B2212H(I)+B1YH(I,1))+S5(I)*(B1221H(I)+B2YH(I,1))
        F23(I)=F23(I)-FINT+FINTY
        F26(I)=F26(I)-FINT-FINTY
        FINT=(S3(I)-FINSY)*PZ3H(I)+
     .       (S6(I)*PX3H(I)+S5(I)*PY3H(I))*HALF
        FINTZ=(S3(I)-FINSX)*PZ3(I)
        F33(I)=F33(I)-FINT+FINTZ
        F36(I)=F36(I)-FINT-FINTZ
       ENDDO
C----------------------------------------------    /
C   - post-traitement-valeur moyenne au sens a'=(_/  a dv ) /v
      DO I=LFT,LLT
         FAC(I) = OFF(I)*VOL(I)/VOLG(I)
         SIGM(I,1) = SIGM(I,1) + FAC(I) * SIG(I,1)
         SIGM(I,2) = SIGM(I,2) + FAC(I) * SIG(I,2)
         SIGM(I,3) = SIGM(I,3) + FAC(I) * SIG(I,3)
         SIGM(I,4) = SIGM(I,4) + FAC(I) * SIG(I,4)
         SIGM(I,5) = SIGM(I,5) + FAC(I) * SIG(I,5)
         SIGM(I,6) = SIGM(I,6) + FAC(I) * SIG(I,6)
         RHOM(I)   = RHOM(I)   + FAC(I) * RHO(I)
         EINTM(I)  = EINTM(I)  + EINT(I)*VOL0(I)/VOL0G(I)
         QM(I)     = QM(I)     + FAC(I) * Q(I)
      ENDDO
      IF (G_PLA > 0) THEN
        DO I=LFT,LLT
          EPLASM(I)  = EPLASM(I)  + FAC(I) * EPLA(I)
        ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
        DO I=LFT,LLT
          EPSDM(I) = EPSDM(I) + FAC(I) * EPSD(I)
        ENDDO
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S6CZERO3                      source/elements/thickshell/solide6c/s6cfint3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CZERO3(
     .   FX1, FY1, FZ1, FX2, FY2, FZ2, FX3, FY3, FZ3, FX4, FY4, FZ4, 
     .   FX5, FY5, FZ5, FX6, FY6, FZ6, 
     .   SIGM,EINTM,RHOM,QM,EPLASM,EPSDM,G_PLA,G_EPSD,NEL)
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
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  G_PLA,G_EPSD,NEL
C     REAL
      my_real
     .   FX1(*), FX2(*), FX3(*), FX4(*), FX5(*), FX6(*), 
     .   FY1(*), FY2(*), FY3(*), FY4(*), FY5(*), FY6(*),
     .   FZ1(*), FZ2(*), FZ3(*), FZ4(*), FZ5(*), FZ6(*), 
     .   EINTM(*),RHOM(*),QM(*),EPSDM(*),EPLASM(*),SIGM(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C=======================================================================
      DO I=LFT,LLT
        SIGM(I,1) = ZERO  
        SIGM(I,2) = ZERO  
        SIGM(I,3) = ZERO  
        SIGM(I,4) = ZERO  
        SIGM(I,5) = ZERO  
        SIGM(I,6) = ZERO  
        RHOM(I)   = ZERO  
        EINTM(I)  = ZERO  
        QM(I)     = ZERO	  
      ENDDO
      IF (G_EPSD > 0) THEN
        DO I=LFT,LLT
          EPSDM(I) = ZERO
        ENDDO
      ENDIF
      IF (G_PLA > 0) THEN
        DO I=LFT,LLT
          EPLASM(I)  = ZERO
        ENDDO
      ENDIF
      DO I=LFT,LLT
       FX1(I)=ZERO
       FY1(I)=ZERO
       FZ1(I)=ZERO
       FX2(I)=ZERO
       FY2(I)=ZERO
       FZ2(I)=ZERO
       FX3(I)=ZERO
       FY3(I)=ZERO
       FZ3(I)=ZERO
       FX4(I)=ZERO
       FY4(I)=ZERO
       FZ4(I)=ZERO
       FX5(I)=ZERO
       FY5(I)=ZERO
       FZ5(I)=ZERO
       FX6(I)=ZERO
       FY6(I)=ZERO
       FZ6(I)=ZERO      
      ENDDO
C-----------
      RETURN
      END SUBROUTINE S6CZERO3
