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
Chd|  GFHOUR_OR                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GFHOUR_OR(LFT,LLT,
     .   FHOUR,JR0,JS0,JT0,FCL,
     .   HGX1, HGX2, HGX3, HGX4,
     .   HGY1, HGY2, HGY3, HGY4,
     .   HGZ1, HGZ2, HGZ3, HGZ4,
     .   H11 , H22 , H33 , 
     .   H12 , H13 , H23 , 
     .   JR_1,JS_1 , JT_1, NU ,NU2 ,
     .   CC  ,CG   ,G33  ,NFHOUR,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NEL
      my_real
     .   FHOUR(NEL,3,4),JR0(*),JS0(*),JT0(*) ,NU(*),NU2(*),
     .   H11(*), H22(*), H33(*),FCL(*),
     .   H12(*), H13(*), H23(*),
     .   HGX1(*), HGX2(*), HGX3(*), HGX4(*),
     .   HGY1(*), HGY2(*), HGY3(*), HGY4(*),
     .   HGZ1(*), HGZ2(*), HGZ3(*), HGZ4(*),
     .   JR_1(*),JS_1(*),JT_1(*),NFHOUR(MVSIZ,3,4)
      my_real
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,K
      my_real
     .   T13,T22,T31,T32,SXY,SYZ,SXZ,SYX,SZY,SZX,
     .   S1211,S1222,S1311,S1333,S2322,S2333,
     .   T1211,T1222,T1311,T1333,T2322,T2333,T12,T23
      my_real
     .   FHOURT(3,4),NUCC(MVSIZ,3,3),R1,R2,R3
C-----------------------------------------------
       DO J=1,3
       DO K=J,3
       DO I=LFT,LLT
        NUCC(I,J,K) = NU2(I)*CC(I,J,K)
        NUCC(I,K,J) = NUCC(I,J,K)
       ENDDO
       ENDDO
       ENDDO
       DO I=LFT,LLT
        FHOURT(1,1) = FHOUR(I,1,1)*JR0(I)+FCL(I)*HGX1(I)
        FHOURT(1,2) = FHOUR(I,1,2)*JR0(I)+FCL(I)*HGX2(I)
        FHOURT(1,3) = FHOUR(I,1,3)*JR0(I)+FCL(I)*HGX3(I)
        FHOURT(1,4) = FHOUR(I,1,4)*JR0(I)+FCL(I)*HGX4(I)
        FHOURT(2,1) = FHOUR(I,2,1)*JS0(I)+FCL(I)*HGY1(I)
        FHOURT(2,2) = FHOUR(I,2,2)*JS0(I)+FCL(I)*HGY2(I)
        FHOURT(2,3) = FHOUR(I,2,3)*JS0(I)+FCL(I)*HGY3(I)
        FHOURT(2,4) = FHOUR(I,2,4)*JS0(I)+FCL(I)*HGY4(I)
        FHOURT(3,1) = FHOUR(I,3,1)*JT0(I)+FCL(I)*HGZ1(I)
        FHOURT(3,2) = FHOUR(I,3,2)*JT0(I)+FCL(I)*HGZ2(I)
        FHOURT(3,3) = FHOUR(I,3,3)*JT0(I)+FCL(I)*HGZ3(I)
        FHOURT(3,4) = FHOUR(I,3,4)*JT0(I)+FCL(I)*HGZ4(I)
C-----
        T31 = NU2(I)*CG(I,3,1)
        T12 = NU2(I)*CG(I,1,2)
        T23 = NU2(I)*CG(I,2,3)
        SXY = (CG(I,1,1)-T31)*FHOURT(1,2)
        SYX = (CG(I,2,1)-T31)*FHOURT(2,1)
        SYZ = (CG(I,2,2)-T12)*FHOURT(2,3)
        SZY = (CG(I,3,2)-T12)*FHOURT(3,2)
        SXZ = (CG(I,1,3)-T23)*FHOURT(1,3)
        SZX = (CG(I,3,3)-T23)*FHOURT(3,1)
        S1211 = JR0(I)*JS_1(I)*FHOURT(1,1)
        S1222 = JS0(I)*JR_1(I)*FHOURT(2,2)
        S1311 = JR0(I)*JT_1(I)*FHOURT(1,1)
        S1333 = JT0(I)*JR_1(I)*FHOURT(3,3)
        S2322 = JS0(I)*JT_1(I)*FHOURT(2,2)
        S2333 = JT0(I)*JS_1(I)*FHOURT(3,3)
        T1211 = S1211*G33(I,1,1)
        T1222 = S1222*G33(I,1,1)
C-------verify w/ Radioss convention    
        T1311 = S1311*G33(I,3,3)
        T1333 = S1333*G33(I,3,3)
        T2322 = S2322*G33(I,2,2)
        T2333 = S2333*G33(I,2,2)
        NFHOUR(I,1,1) = H12(I)*(SXY+T1211)+H22(I)*(SYX+T1222)+
     .                  H13(I)*(SXZ+T1311)+H33(I)*(SZX+T1333)
        NFHOUR(I,2,2) = H11(I)*(SXY+T1211)+H12(I)*(SYX+T1222)+
     .                  H23(I)*(SYZ+T2322)+H33(I)*(SZY+T2333)
        NFHOUR(I,3,3) = H11(I)*(SXZ+T1311)+H13(I)*(SZX+T1333)+
     .                  H22(I)*(SYZ+T2322)+H23(I)*(SZY+T2333)
c       
        NFHOUR(I,1,2) = H11(I)*FHOURT(1,2)*(
     .   (CC(I,1,1)-NUCC(I,1,3)-NU2(I)*(CC(I,3,1)-NUCC(I,3,3))))+
     .                  H12(I)*FHOURT(2,1)*(
     .   (CC(I,1,2)-NUCC(I,1,3)-NU2(I)*(CC(I,3,2)-NUCC(I,3,3))))+
     .   (CG(I,1,1)-T31)*(H11(I)*S1211+H12(I)*S1222)
        NFHOUR(I,1,3) =H11(I)*FHOURT(1,3)*(
     .   (CC(I,1,1)-NUCC(I,1,2)-NU2(I)*(CC(I,2,1)-NUCC(I,2,2))))+
     .                  H13(I)*FHOURT(3,1)*(
     .   (CC(I,1,3)-NUCC(I,1,2)-NU2(I)*(CC(I,2,3)-NUCC(I,2,2))))+
     .   (CG(I,1,3)-T23)*(H11(I)*S1311+H13(I)*S1333)
        NFHOUR(I,1,4) = THIRD*H11(I)*FHOURT(1,4)*(
     .       (CC(I,1,1)-NU(I)*(TWO*(CC(I,1,2)+CC(I,1,3)-NU(I)*CC(I,2,3))-
     .                         NU(I)*(CC(I,2,2)+CC(I,3,3)))))
C     
        NFHOUR(I,2,1) = H12(I)*FHOURT(1,2)*(
     .   (CC(I,2,1)-NUCC(I,2,3)-NU2(I)*(CC(I,3,1)-NUCC(I,3,3))))+
     .                  H22(I)*FHOURT(2,1)*(
     .   (CC(I,2,2)-NUCC(I,2,3)-NU2(I)*(CC(I,3,2)-NUCC(I,3,3))))+
     .   (CG(I,2,1)-T31)*(H12(I)*S1211+H22(I)*S1222)
        NFHOUR(I,2,3) = H22(I)*FHOURT(2,3)*(
     .   (CC(I,2,2)-NUCC(I,2,1)-NU2(I)*(CC(I,1,2)-NUCC(I,1,1))))+
     .                  H23(I)*FHOURT(3,2)*(
     .   (CC(I,2,3)-NUCC(I,2,1)-NU2(I)*(CC(I,1,3)-NUCC(I,1,1))))+
     .   (CG(I,2,2)-T12)*(H22(I)*S2322+H23(I)*S2333)
        NFHOUR(I,2,4) = THIRD*H22(I)*FHOURT(2,4)*(
     .       (CC(I,2,2)-NU(I)*(TWO*(CC(I,1,2)+CC(I,2,3)-NU(I)*CC(I,1,3))-
     .                         NU(I)*(CC(I,1,1)+CC(I,3,3)))))
c
        NFHOUR(I,3,1) = H13(I)*FHOURT(1,3)*(
     .   (CC(I,3,1)-NUCC(I,3,2)-NU2(I)*(CC(I,2,1)-NUCC(I,2,2))))+
     .                  H33(I)*FHOURT(3,1)*(
     .   (CC(I,3,3)-NUCC(I,3,2)-NU2(I)*(CC(I,2,3)-NUCC(I,2,2))))+
     .   (CG(I,3,3)-T23)*(H13(I)*S1311+H33(I)*S1333)
        NFHOUR(I,3,2) = H23(I)*FHOURT(2,3)*(
     .   (CC(I,3,2)-NUCC(I,3,1)-NU2(I)*(CC(I,1,2)-NUCC(I,1,1))))+
     .                  H33(I)*FHOURT(3,2)*(
     .   (CC(I,3,3)-NUCC(I,3,1)-NU2(I)*(CC(I,1,3)-NUCC(I,1,1))))+
     .   (CG(I,3,2)-T12)*(H23(I)*S2322+H33(I)*S2333)
        NFHOUR(I,3,4) = THIRD*H33(I)*FHOURT(3,4)*(
     .       (CC(I,3,3)-NU(I)*(TWO*(CC(I,1,3)+CC(I,2,3)-NU(I)*CC(I,1,2))-
     .                         NU(I)*(CC(I,1,1)+CC(I,2,2)))))
       ENDDO
C
      RETURN
      END
