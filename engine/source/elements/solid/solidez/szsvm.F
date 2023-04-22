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
Chd|  SZSVM                         source/elements/solid/solidez/szsvm.F
Chd|-- called by -----------
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSVM(
     1   JR0,     JS0,     JT0,     FHOUR,
     2   SIGY,    SIG0,    NU,      SVM1,
     3   SVM2,    NEL,     IINT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IINT
      INTEGER NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIGY(*) ,SIG0(NEL,6),SVM1(*),SVM2(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,K,IKT
      my_real
     .   S1,S2,S3,SVM0,SR1,SR2,SR3,SR4,
     .   SS1,SS2,SS3,SS4,ST1,ST2,ST3,ST4,SVMR,SVMS,SVMT,
     .   SVM,SVMRST,COEF,COEF1, JR_1,JS_1,JT_1,NU1,NU2,
     .   RS,ST,RT,VT1(4),VT2(4),MAX1,MIN1,MAX2,MIN2,MAX0,MIN0    
C         coef= (3*8*0.75)^2 coef1 =1/0.75
          COEF= THREEHUNDRED24
          COEF1= ONEP333
C-----------------------------------------------
      IKT = IINT
      IF (IKT ==2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ELSEIF (IKT > 2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ENDIF
      DO I=1,NEL
       SVM1(I) = ZERO
       SVM2(I) = ZERO
       IF (SIGY(I)<ZEP9EP30) THEN
        S1 =SIG0(I,1)-SIG0(I,2)
        S2 =SIG0(I,2)-SIG0(I,3)
        S3 =SIG0(I,1)-SIG0(I,3)
        SVM0 = (S1*S1+S2*S2+S3*S3)*HALF + THREE*(SIG0(I,4)*SIG0(I,4)
     .                +SIG0(I,5)*SIG0(I,5)+SIG0(I,6)*SIG0(I,6))
        NU1 =ONE/(ONE - NU(I))
        NU2 =NU(I)*NU1
        JR_1 = ONE/MAX(EM20,JR0(I))
        JS_1 = ONE/MAX(EM20,JS0(I))
        JT_1 = ONE/MAX(EM20,JT0(I))
        SR1 =NU1*FHOUR(I,1,3)+NU2*FHOUR(I,3,1)
        SR2 =NU1*FHOUR(I,3,1)+NU2*FHOUR(I,1,3)
        SR3 =FHOUR(I,1,3)-FHOUR(I,3,1)
        SR4 =JT0(I)*JR_1*FHOUR(I,3,3)+JR0(I)*JT_1*FHOUR(I,1,1)
        SVMR = TWO*(SR1*SR1+SR2*SR2+SR3*SR3)+3*SR4*SR4
        SS1 =FHOUR(I,1,2)-FHOUR(I,2,1)
        SS2 =NU1*FHOUR(I,2,1)+NU2*FHOUR(I,1,2)
        SS3 =NU1*FHOUR(I,1,2)+NU2*FHOUR(I,2,1)
        SS4 =JS0(I)*JR_1*FHOUR(I,2,2)+JR0(I)*JS_1*FHOUR(I,1,1)
        SVMS = TWO*(SS1*SS1+SS2*SS2+SS3*SS3) + THREE*SS4*SS4
        ST1 =NU1*FHOUR(I,2,3)+NU2*FHOUR(I,3,2)
        ST2 =FHOUR(I,2,3)-FHOUR(I,3,2)
        ST3 =NU1*FHOUR(I,3,2)+NU2*FHOUR(I,2,3)
        ST4 =JS0(I)*JT_1*FHOUR(I,2,2)+JT0(I)*JS_1*FHOUR(I,3,3)
        SVMT = TWO*(ST1*ST1+ST2*ST2+ST3*ST3) + THREE*ST4*ST4
C
        RS =SR1*SS1
        RT =SR1*ST1
        ST =SS1*ST1
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =SR2*SS2
        RT =SR2*ST2
        ST =SS2*ST2
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX0+MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN0+MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =SR3*SS3
        RT =SR3*ST3
        ST =SS3*ST3
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX0+MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN0+MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        SVMRST = SVM0+COEF*(SVMR+SVMS+SVMT)
        SVM1(I) = SQRT(ABS(SVMRST+COEF*MAX0))
        SVM2(I) = SQRT(MAX(SVMRST+COEF*MIN0,ZERO))
        SVM2(I) = MIN(SVM2(I),SVM0)
       ENDIF
      ENDDO 
C
      RETURN
      END
