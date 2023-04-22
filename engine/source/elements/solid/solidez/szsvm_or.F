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
Chd|  SZSVM_OR                      source/elements/solid/solidez/szsvm_or.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3_or.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSVM_OR(
     1   JR0,     JS0,     JT0,     CC,
     2   CG,      G33,     FHOUR,   SIGY,
     3   SIG0,    NU,      SVM1,    SVM2,
     4   NEL,     IINT)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IINT
      INTEGER NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIGY(*) ,SIG0(NEL,6),SVM1(*),SVM2(*),NU,
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,K,IKT
      my_real
     .   S1,S2,S3,SVM0,SR1,SR2,SR3,SR4,
     .   SS1,SS2,SS3,SS4,ST1,ST2,ST3,ST4,SVMR,SVMS,SVMT,
     .   SVM,SVMRST,COEF,COEF1, JR_1,JS_1,JT_1,NU1,NU2,T1,T2,
     .   RS,ST,RT,VT1(4),VT2(4),MAX1,MIN1,MAX2,MIN2,MAX0,MIN0, 
     .   SR5,SR6,SS5,SS6, ST5,ST6
C-----------------------------------------------
C         coef= (3*8*0.75)^2 coef1 =1/0.75
          COEF= THREEHUNDRED24
          COEF1= ONEP333
      IKT = IINT
      IF (IKT ==2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ELSEIF (IKT > 2) THEN
        COEF1= COEF/(TEN*IKT)
        COEF= MAX(COEF1,EM01)
      ENDIF
        NU1 =ONE/(ONE - NU)
        NU2 =NU*NU1
      DO I=1,NEL
       SVM1(I) = ZERO
       SVM2(I) = ZERO
       IF (SIGY(I)<ZEP9EP30) THEN
        S1 =SIG0(I,1)-SIG0(I,2)
        S2 =SIG0(I,2)-SIG0(I,3)
        S3 =SIG0(I,1)-SIG0(I,3)
        SVM0 = (S1*S1+S2*S2+S3*S3)*HALF + THREE*(SIG0(I,4)*SIG0(I,4)
     .                +SIG0(I,5)*SIG0(I,5)+SIG0(I,6)*SIG0(I,6))
        JR_1 = ONE/MAX(EM20,JR0(I))
        JS_1 = ONE/MAX(EM20,JS0(I))
        JT_1 = ONE/MAX(EM20,JT0(I))
        J=2
        K=1
        T1 = NU2*(CC(I,J,2)-CC(I,K,2))
        T2 = JR0(I)*JT_1*FHOUR(I,1,1)+JT0(I)*JR_1*FHOUR(I,3,3)
        SR1 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,1)+
     .       (CG(I,J,2)-CG(I,K,2))*T2   
        J=3
        K=2
        T1 = NU2*(CC(I,J,2)-CC(I,K,2))
        SR2 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,1)+
     .       (CG(I,J,2)-CG(I,K,2))*T2   
        J=3
        K=1
        T1 = NU2*(CC(I,J,2)-CC(I,K,2))
        SR3 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,1)+
     .       (CG(I,J,2)-CG(I,K,2))*T2   
        J=1
        T1 = NU2*CG(I,2,J)
        SR4 =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,2)*T2
        J=2
        T1 = NU2*CG(I,2,J)
        SR5 =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,2)*T2
        J=3
        T1 = NU2*CG(I,2,J)
        SR6 =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,2)*T2
        SVMR =HALF*(SR1*SR1+SR2*SR2+SR3*SR3)+THREE*(SR4*SR4+SR5*SR5+SR6*SR6)
        J=2
        K=1
        T1 = NU2*(CC(I,J,3)-CC(I,K,3))
        T2 = JR0(I)*JS_1*FHOUR(I,1,1)+JS0(I)*JR_1*FHOUR(I,2,2)
        SS1 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,1)+
     .       (CG(I,J,1)-CG(I,K,1))*T2   
        J=3
        K=2
        T1 = NU2*(CC(I,J,3)-CC(I,K,3))
        SS2 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,1)+
     .       (CG(I,J,1)-CG(I,K,1))*T2   
        J=3
        K=1
        T1 = NU2*(CC(I,J,3)-CC(I,K,3))
        SS3 =(CC(I,J,1)-CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,1)+
     .       (CG(I,J,1)-CG(I,K,1))*T2   
        J=1
        T1 = NU2*CG(I,3,J)
        SS4 =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=2
        T1 = NU2*CG(I,3,J)
        SS5 =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=3
        T1 = NU2*CG(I,3,J)
        SS6 =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        SVMS =HALF*(SS1*SS1+SS2*SS2+SS3*SS3)+THREE*(SS4*SS4+SS5*SS5+SS6*SS6)
        J=2
        K=1
        T1 = NU2*(CC(I,J,1)-CC(I,K,1))
        T2 = JS0(I)*JT_1*FHOUR(I,2,2)+JT0(I)*JS_1*FHOUR(I,3,3)
        ST1 =(CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,2)+
     .       (CG(I,J,3)-CG(I,K,3))*T2   
        J=3
        K=2
        T1 = NU2*(CC(I,J,1)-CC(I,K,1))
        ST2 =(CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,2)+
     .       (CG(I,J,3)-CG(I,K,3))*T2   
        J=3
        K=1
        T1 = NU2*(CC(I,J,1)-CC(I,K,1))
        ST3 =(CC(I,J,2)-CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,J,3)-CC(I,K,3)-T1)*FHOUR(I,3,2)+
     .       (CG(I,J,3)-CG(I,K,3))*T2   
        J=1
        T1 = NU2*CG(I,1,J)
        ST4 =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,3)*T2
        J=2
        T1 = NU2*CG(I,1,J)
        ST5 =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,3)*T2
        J=3
        T1 = NU2*CG(I,1,J)
        ST6 =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,3)*T2
        SVMT =HALF*(ST1*ST1+ST2*ST2+ST3*ST3)+THREE*(ST4*ST4+ST5*ST5+ST6*ST6)
C
        RS =HALF*SR1*SS1
        RT =HALF*SR1*ST1
        ST =HALF*SS1*ST1
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =HALF*SR2*SS2
        RT =HALF*SR2*ST2
        ST =HALF*SS2*ST2
        VT2(1) = RS+RT+ST
        VT2(2) = RS-RT-ST
        VT2(3) = -RS+RT-ST
        VT2(4) = -RS-RT+ST
        MAX0 =MAX0+MAX(VT2(1),VT2(2),VT2(3),VT2(4))
        MIN0 =MIN0+MIN(VT2(1),VT2(2),VT2(3),VT2(4))
        RS =HALF*SR3*SS3
        RT =HALF*SR3*ST3
        ST =HALF*SS3*ST3
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
