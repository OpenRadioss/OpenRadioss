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
Chd|  SZSIGH_OR                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSIGH_OR(
     1   KSI,     ETA,     ZETA,    JR0,
     2   JS0,     JT0,     CC,      CG,
     3   G33,     FHOUR,   NU,      SIG,
     4   NEL)
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
      INTEGER NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,
     .   SIG(MVSIZ,6) ,KSI,ETA,ZETA,NU,
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,K
      my_real
     .   NU1,NU2,T1,T2,RS,ST,RT, 
     .   RXX,RYY,RZZ,RXY,RYZ,RXZ,SXX,SYY,SZZ,SXY,SYZ,SXZ,
     .   TXX,TYY,TZZ,TXY,TYZ,TXZ,QXX,QYY,QZZ,QXY,QYZ,QXZ,
     .   RS1,ST1,RT1,JR_1,JS_1,JT_1
C----------RXX,SXX,TXX:ETA,ZETA,KSI-------------------------------------
        NU1 =ONE/(ONE - NU)
        NU2 =NU*NU1
        RS = ETA*ZETA
        ST = ZETA*KSI
        RT = ETA*KSI
      DO I=1,NEL
        JR_1 = ONE/MAX(EM20,JR0(I))
        JS_1 = ONE/MAX(EM20,JS0(I))
        JT_1 = ONE/MAX(EM20,JT0(I))
        K=1
        T1 = NU2*CC(I,K,2)
        T2 = JR0(I)*JT_1*FHOUR(I,1,1)+JT0(I)*JR_1*FHOUR(I,3,3)
        RXX = (CC(I,K,1)-T1)*FHOUR(I,1,3)+     
     .        (CC(I,K,3)-T1)*FHOUR(I,3,1)+ CG(I,K,3)*T2
        K=2
        T1 = NU2*CC(I,K,2)
        RYY = (CC(I,K,1)-T1)*FHOUR(I,1,3)+     
     .        (CC(I,K,3)-T1)*FHOUR(I,3,1)+ CG(I,K,3)*T2
        K=3
        T1 = NU2*CC(I,K,2)
        RZZ = (CC(I,K,1)-T1)*FHOUR(I,1,3)+     
     .        (CC(I,K,3)-T1)*FHOUR(I,3,1)+ CG(I,K,3)*T2
        J=1
        T1 = NU2*CG(I,2,J)
        RXY =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,3)*T2
        J=2
        T1 = NU2*CG(I,2,J)
        RYZ =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,3)*T2
        J=3
        T1 = NU2*CG(I,2,J)
        RXZ =(CG(I,1,J)-T1)*FHOUR(I,1,3)+(CG(I,3,J)-T1)*FHOUR(I,3,1)+
     .        G33(I,J,3)*T2
        K=1
        T1 = NU2*CC(I,K,3)
        T2 = JR0(I)*JS_1*FHOUR(I,1,1)+JS0(I)*JR_1*FHOUR(I,2,2)
        SXX =(CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,K,2)-T1)*FHOUR(I,2,1)+ CG(I,K,1)*T2   
        K=2
        T1 = NU2*CC(I,K,3)
        SYY =(CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,K,2)-T1)*FHOUR(I,2,1)+ CG(I,K,1)*T2   
        K=3
        T1 = NU2*CC(I,K,3)
        SZZ =(CC(I,K,1)-T1)*FHOUR(I,1,2)+
     .       (CC(I,K,2)-T1)*FHOUR(I,2,1)+ CG(I,K,1)*T2   
        J=1
        T1 = NU2*CG(I,3,J)
        SXY =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=2
        T1 = NU2*CG(I,3,J)
        SYZ =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        J=3
        T1 = NU2*CG(I,3,J)
        SXZ =(CG(I,1,J)-T1)*FHOUR(I,1,2)+(CG(I,2,J)-T1)*FHOUR(I,2,1)+
     .        G33(I,J,1)*T2
        K=1
        T1 = NU2*CC(I,K,1)
        T2 = JS0(I)*JT_1*FHOUR(I,2,2)+JT0(I)*JS_1*FHOUR(I,3,3)
        TXX =(CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,K,3)-T1)*FHOUR(I,3,2)+ CG(I,K,2)*T2   
        K=2
        T1 = NU2*CC(I,K,1)
        TYY =(CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,K,3)-T1)*FHOUR(I,3,2)+ CG(I,K,2)*T2   
        K=3
        T1 = NU2*CC(I,K,1)
        TZZ =(CC(I,K,2)-T1)*FHOUR(I,2,3)+
     .       (CC(I,K,3)-T1)*FHOUR(I,3,2)+ CG(I,K,2)*T2   
        J=1
        T1 = NU2*CG(I,1,J)
        TXY =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,2)*T2
        J=2
        T1 = NU2*CG(I,1,J)
        TYZ =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,2)*T2
        J=3
        T1 = NU2*CG(I,1,J)
        TXZ =(CG(I,2,J)-T1)*FHOUR(I,2,3)+(CG(I,3,J)-T1)*FHOUR(I,3,2)+
     .        G33(I,J,2)*T2
        RS1 = FHOUR(I,1,4)*RS
        ST1 = FHOUR(I,2,4)*ST
        RT1 = FHOUR(I,3,4)*RT
        K = 1
        QXX =(CC(I,K,1)-NU*(CC(I,K,2)+CC(I,K,3)))*RS1+
     .       (CC(I,K,2)-NU*(CC(I,K,3)+CC(I,K,1)))*ST1+
     .       (CC(I,K,3)-NU*(CC(I,K,1)+CC(I,K,2)))*RT1     
        K = 2
        QYY =(CC(I,K,1)-NU*(CC(I,K,2)+CC(I,K,3)))*RS1+
     .       (CC(I,K,2)-NU*(CC(I,K,3)+CC(I,K,1)))*ST1+
     .       (CC(I,K,3)-NU*(CC(I,K,1)+CC(I,K,2)))*RT1     
        K = 3
        QZZ =(CC(I,K,1)-NU*(CC(I,K,2)+CC(I,K,3)))*RS1+
     .       (CC(I,K,2)-NU*(CC(I,K,3)+CC(I,K,1)))*ST1+
     .       (CC(I,K,3)-NU*(CC(I,K,1)+CC(I,K,2)))*RT1     
        J = 1
        QXY =(CG(I,1,J)-NU*(CG(I,1,J)+CG(I,1,J)))*RS1+
     .       (CG(I,2,J)-NU*(CG(I,2,J)+CG(I,2,J)))*ST1+
     .       (CG(I,3,J)-NU*(CG(I,3,J)+CG(I,3,J)))*RT1     
        J = 2
        QYZ =(CG(I,1,J)-NU*(CG(I,1,J)+CG(I,1,J)))*RS1+
     .       (CG(I,2,J)-NU*(CG(I,2,J)+CG(I,2,J)))*ST1+
     .       (CG(I,3,J)-NU*(CG(I,3,J)+CG(I,3,J)))*RT1     
        J = 3
        QXZ =(CG(I,1,J)-NU*(CG(I,1,J)+CG(I,1,J)))*RS1+
     .       (CG(I,2,J)-NU*(CG(I,2,J)+CG(I,2,J)))*ST1+
     .       (CG(I,3,J)-NU*(CG(I,3,J)+CG(I,3,J)))*RT1     
C     
        SIG(I,1) = RXX*ETA+SXX*ZETA+TXX*KSI+QXX
        SIG(I,2) = RYY*ETA+SYY*ZETA+TYY*KSI+QYY
        SIG(I,3) = RZZ*ETA+SZZ*ZETA+TZZ*KSI+QZZ
        SIG(I,4) = RXY*ETA+SXY*ZETA+TXY*KSI+QXY
        SIG(I,5) = RYZ*ETA+SYZ*ZETA+TYZ*KSI+QYZ
        SIG(I,6) = RXZ*ETA+SXZ*ZETA+TXZ*KSI+QXZ
      ENDDO 
C
      RETURN
      END
