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
C new subroutine for simplified flow (euler euqation
C can be used also for Navier stocks equation
Chd|====================================================================
Chd|  FFINT3                        source/elements/solid/solide/ffint3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FFINT3(SIG,DFE,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   VOL,QVIS,
     .   N1X, N2X, N3X, N4X, N5X, N6X,
     .   N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .   N1Z, N2Z, N3Z, N4Z, N5Z, N6Z)
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
c#include      "vect04_c.2.inc"
#include      "com01_c.inc"
#include      "fluid_scr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   SIG(MVSIZ,6),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*),QVIS(*) ,DFE(MVSIZ,3)
      my_real
     .   N1X(*), N2X(*), N3X(*), N4X(*), N5X(*), N6X(*),
     .   N1Y(*), N2Y(*), N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .   N1Z(*), N2Z(*), N3Z(*), N4Z(*), N5Z(*), N6Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),    
     .   FINT,FINTD,P(MVSIZ)
C-----------------------------------------------
      IF(ICAA == 0)THEN
        DO I=LFT,LLT
c          S1(I)=(SIG(1,I)+SVIS(I,1)-QVIS(I))*VOL(I)
c         S2(I)=(SIG(2,I)+SVIS(I,2)-QVIS(I))*VOL(I)
c         S3(I)=(SIG(3,I)+SVIS(I,3)-QVIS(I))*VOL(I)
c         S4(I)=(SIG(4,I)+SVIS(I,4))*VOL(I)
c         S5(I)=(SIG(5,I)+SVIS(I,5))*VOL(I)
c         S6(I)=(SIG(6,I)+SVIS(I,6))*VOL(I)
         S1(I)=(SIG(I,1)-QVIS(I))*VOL(I)
         S2(I)=(SIG(I,2)-QVIS(I))*VOL(I)
         S3(I)=(SIG(I,3)-QVIS(I))*VOL(I)
         S4(I)=(SIG(I,4))*VOL(I)
         S5(I)=(SIG(I,5))*VOL(I)
         S6(I)=(SIG(I,6))*VOL(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
c         S1(I)=SIG(1,I)+SVIS(I,1)-QVIS(I)
c         S2(I)=SIG(2,I)+SVIS(I,2)-QVIS(I)
c         S3(I)=SIG(3,I)+SVIS(I,3)-QVIS(I)
         S1(I)=SIG(I,1)-QVIS(I)
         S2(I)=SIG(I,2)-QVIS(I)
         S3(I)=SIG(I,3)-QVIS(I)
         P(I)=(S1(I)+S2(I)+S3(I))/THREE
         S1(I)=(S1(I)-P(I))*VOL(I)
         S2(I)=(S2(I)-P(I))*VOL(I)
         S3(I)=(S3(I)-P(I))*VOL(I)
c         S4(I)=(SIG(4,I)+SVIS(I,4))*VOL(I)
c         S5(I)=(SIG(5,I)+SVIS(I,5))*VOL(I)
c         S6(I)=(SIG(6,I)+SVIS(I,6))*VOL(I)
         S4(I)=SIG(I,4)*VOL(I)
         S5(I)=SIG(I,5)*VOL(I)
         S6(I)=SIG(I,6)*VOL(I)
         P(I)=P(I)*0.125
         
        ENDDO
        DO I=LFT,LLT
          F11(I)=F11(I)-P(I)*(N1X(I)              +N4X(I)       +N6X(I))
          F21(I)=F21(I)-P(I)*(N1Y(I)              +N4Y(I)       +N6Y(I))
          F31(I)=F31(I)-P(I)*(N1Z(I)              +N4Z(I)       +N6Z(I))
          F12(I)=F12(I)-P(I)*(N1X(I)              +N4X(I)+N5X(I)       )
          F22(I)=F22(I)-P(I)*(N1Y(I)              +N4Y(I)+N5Y(I)       )
          F32(I)=F32(I)-P(I)*(N1Z(I)              +N4Z(I)+N5Z(I)       )
          F13(I)=F13(I)-P(I)*(N1X(I)+N2X(I)              +N5X(I)       )
          F23(I)=F23(I)-P(I)*(N1Y(I)+N2Y(I)              +N5Y(I)       )
          F33(I)=F33(I)-P(I)*(N1Z(I)+N2Z(I)              +N5Z(I)       )
          F14(I)=F14(I)-P(I)*(N1X(I)+N2X(I)                     +N6X(I))
          F24(I)=F24(I)-P(I)*(N1Y(I)+N2Y(I)                     +N6Y(I))
          F34(I)=F34(I)-P(I)*(N1Z(I)+N2Z(I)                     +N6Z(I))
          F15(I)=F15(I)-P(I)*(             +N3X(I)+N4X(I)       +N6X(I))
          F25(I)=F25(I)-P(I)*(             +N3Y(I)+N4Y(I)       +N6Y(I))
          F35(I)=F35(I)-P(I)*(             +N3Z(I)+N4Z(I)       +N6Z(I))
          F16(I)=F16(I)-P(I)*(             +N3X(I)+N4X(I)+N5X(I)       )
          F26(I)=F26(I)-P(I)*(             +N3Y(I)+N4Y(I)+N5Y(I)       )
          F36(I)=F36(I)-P(I)*(             +N3Z(I)+N4Z(I)+N5Z(I)       )
          F17(I)=F17(I)-P(I)*(      +N2X(I)+N3X(I)       +N5X(I)       )
          F27(I)=F27(I)-P(I)*(      +N2Y(I)+N3Y(I)       +N5Y(I)       )
          F37(I)=F37(I)-P(I)*(      +N2Z(I)+N3Z(I)       +N5Z(I)       )
          F18(I)=F18(I)-P(I)*(      +N2X(I)+N3X(I)              +N6X(I))
          F28(I)=F28(I)-P(I)*(      +N2Y(I)+N3Y(I)              +N6Y(I))
          F38(I)=F38(I)-P(I)*(      +N2Z(I)+N3Z(I)              +N6Z(I)) 
        ENDDO
      ENDIF
C      
       DO I=LFT,LLT
        FINT=S1(I)*PX1(I)+S4(I)*PY1(I)+S6(I)*PZ1(I)
        F11(I)=F11(I) - FINT  - DFE(I,1)     
        F17(I)=F17(I) + FINT - DFE(I,1)  
C              
        FINT=S2(I)*PY1(I)+S4(I)*PX1(I)+S5(I)*PZ1(I)
        F21(I)=F21(I)-FINT - DFE(I,2)
        F27(I)=F27(I)+FINT - DFE(I,2)
C       
        FINT=S3(I)*PZ1(I)+S6(I)*PX1(I)+S5(I)*PY1(I)
        F31(I)=F31(I) - FINT  - DFE(I,3) 
        F37(I)=F37(I) + FINT  - DFE(I,3)       
      
C
         FINT=S1(I)*PX2(I)+S4(I)*PY2(I)+S6(I)*PZ2(I) 
         F12(I)=F12(I) - FINT - DFE(I,1)
         F18(I)=F18(I) + FINT - DFE(I,1)                  
C
        FINT=S2(I)*PY2(I)+S4(I)*PX2(I)+S5(I)*PZ2(I)
        F22(I)=F22(I)-FINT - DFE(I,2)
        F28(I)=F28(I)+FINT - DFE(I,2)                   
C
        FINT=S3(I)*PZ2(I)+S6(I)*PX2(I)+S5(I)*PY2(I)
        F32(I)=F32(I) - FINT - DFE(I,3)
        F38(I)=F38(I) + FINT - DFE(I,3)    
C
        FINT=S1(I)*PX3(I)+S4(I)*PY3(I)+S6(I)*PZ3(I)
         F13(I)=F13(I)-FINT - DFE(I,1)
         F15(I)=F15(I)+FINT - DFE(I,1)
C 
        FINT=S2(I)*PY3(I)+S4(I)*PX3(I)+S5(I)*PZ3(I)
        F23(I)=F23(I)-FINT - DFE(I,2) 
        F25(I)=F25(I)+FINT - DFE(I,2)
cc
        FINT=S3(I)*PZ3(I)+S6(I)*PX3(I)+S5(I)*PY3(I)
        F33(I)=F33(I)-FINT  - DFE(I,3)
        F35(I)=F35(I)+FINT  - DFE(I,3)
C
        FINT=S1(I)*PX4(I)+S4(I)*PY4(I)+S6(I)*PZ4(I)
        F14(I)=F14(I)-FINT - DFE(I,1)
        F16(I)=F16(I)+FINT - DFE(I,1)
cc
        FINT=S2(I)*PY4(I)+S4(I)*PX4(I)+S5(I)*PZ4(I)
        F24(I)=F24(I)-FINT  - DFE(I,2)
        F26(I)=F26(I)+FINT  - DFE(I,2)
c
         FINT=S3(I)*PZ4(I)+S6(I)*PX4(I)+S5(I)*PY4(I)
         F34(I)=F34(I)-FINT  - DFE(I,3)
         F36(I)=F36(I)+FINT  - DFE(I,3)
       ENDDO     
C
      RETURN
      END SUBROUTINE FFINT3
     
