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
C   |============================================================
C   |  CBABE3                          /coqueba/cbabe3.F
C   |------------------------------------------------------------
C   |-- appelee par -----------
C   |         CBAKE3                         /coqueba/cbake3.F
C   |============================================================
Chd|====================================================================
Chd|  CBABE3                        source/elements/shell/coqueba/cbabe3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBABE3(JFT,JLT,NG,VCORE,AREA,CDET,VQN,VQ,VJFI,
     1                  VNRM,VASTN,HX,HY,VETA,VKSI,
     2                  BM,BMF,BF,BC,TC,BZZ,NPLAT,IPLAT,
     3                  ISROT,BRZ )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL  [B] AU POINT D'INTEGRATION Index changed*****EP-->I*****
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NPG,NG,JFT,JLT,NPLAT,IPLAT(*),ISROT
      PARAMETER (NPG = 4)
      MY_REAL 
     .   VCORE(MVSIZ,12),VQN(MVSIZ,9,4),VKSI(4,4),VETA(4,4),
     .   BM(MVSIZ,36),BMF(MVSIZ,36),BF(MVSIZ,24),BC(MVSIZ,40),HX(MVSIZ,4),HY(MVSIZ,4)
      MY_REAL 
     .   VNRM(MVSIZ,12),VASTN(MVSIZ,16),VJFI(MVSIZ,3,2,4),
     .   VQ(MVSIZ,3,3,4),AREA(*)
      MY_REAL 
     .   CDET(*),TC(MVSIZ,2,2),BZZ(MVSIZ,8),BRZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,NG1,K
      MY_REAL 
     .   VPG(2,NPG),THK(MVSIZ),DETJ,DET,
     .   TFN(3,2),BCX,BCY,V1(2),V2(2),C1,C2,BC1,BC2,VB1,
     .   VJF1(2,3),VJF(3,3),TBI(2,2),TBC(MVSIZ,2,2),V11(4)
      MY_REAL 
     .   A_1,C11,C12,C21,C22,CC,BETA1,KSI1,KSIY1,BETA2,KSI2,KSIY2,
     .   BMT(8),BCT(12),BXY(3),BYX(3)
      MY_REAL 
     .   PG1,PG
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
C-------VCORE(12):BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
C-------BM(1-4)->BxI,BM(5-8)->ByI
C------- (I=1:BMT(1)+BMT(3);I=2:BMT(2)+BMT(4);I=3:-BMT(1)+BMT(3);I=4:-BMT(2)+BMT(4))
        BMT(1)=VCORE(EP,1)+HX(EP,NG)*VCORE(EP,5)
        BMT(2)=VCORE(EP,2)+HX(EP,NG)*VCORE(EP,6)
        BMT(3)=HX(EP,NG)*FOURTH
        BMT(4)=-BMT(3)
        BM(I,1)=BMT(1)+BMT(3)
        BM(I,2)=BMT(2)+BMT(4)
        BM(I,3)=-BMT(1)+BMT(3)
        BM(I,4)=-BMT(2)+BMT(4)
        BMT(5)=VCORE(EP,3)+HY(EP,NG)*VCORE(EP,5)
        BMT(6)=VCORE(EP,4)+HY(EP,NG)*VCORE(EP,6)
        BMT(7)=HY(EP,NG)*FOURTH
        BMT(8)=-BMT(7)
        BM(I,5)=BMT(5)+BMT(7)
        BM(I,6)=BMT(6)+BMT(8)
        BM(I,7)=-BMT(5)+BMT(7)
        BM(I,8)=-BMT(6)+BMT(8)
       ENDDO 
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
        A_1 = ONE/MAX(CDET(EP),EM20)
        C11=(VCORE(EP,10)+VCORE(EP,12)*VPG(1,NG))*A_1
        C12=-(VCORE(EP,8)+VCORE(EP,12)*VPG(2,NG))*A_1
        C21=-(VCORE(EP,9)+VCORE(EP,11)*VPG(1,NG))*A_1
        C22=( VCORE(EP,7)+VCORE(EP,11)*VPG(2,NG))*A_1
        BZZ(I,1)=C11*VKSI(1,NG)+C12*VETA(1,NG)
        BZZ(I,2)=C21*VKSI(1,NG)+C22*VETA(1,NG)
        BZZ(I,3)=C11*VKSI(2,NG)+C12*VETA(2,NG)
        BZZ(I,4)=C21*VKSI(2,NG)+C22*VETA(2,NG)
        BZZ(I,5)=C11*VKSI(3,NG)+C12*VETA(3,NG)
        BZZ(I,6)=C21*VKSI(3,NG)+C22*VETA(3,NG)
        BZZ(I,7)=C11*VKSI(4,NG)+C12*VETA(4,NG)
        BZZ(I,8)=C21*VKSI(4,NG)+C22*VETA(4,NG)
        C11=FOURTH*C11
        C12=FOURTH*C12
        C21=FOURTH*C21
        C22=FOURTH*C22
C--------C.T-----
C-----PARTIE SYM---------
        BETA1=VCORE(EP,8)+VCORE(EP,12)*VPG(2,NG)
        KSI1=VCORE(EP,10)+VCORE(EP,12)*VPG(1,NG)
        BETA2=VCORE(EP,7)+VCORE(EP,11)*VPG(2,NG)
        KSI2=VCORE(EP,9)+VCORE(EP,11)*VPG(1,NG)
C-------NOEUD--- 3
        BCT(1)=C11*VPG(2,NG)+C12*VPG(1,NG)
        BCT(2)=C21*VPG(2,NG)+C22*VPG(1,NG)
        BCT(3)=-BETA1*C11-KSI1*C12
        BCT(4)=-BETA1*C21-KSI1*C22
        BCT(5)=C11*BETA2+C12*KSI2
        BCT(6)=C21*BETA2+C22*KSI2
C-------NOEUD--- 4
        BCT(7)=-BCT(1)
        BCT(8)=-BCT(2)
        BCT(9)=BCT(3)
        BCT(10)=BCT(4)
        BCT(11)=BCT(5)
        BCT(12)=BCT(6)
C
        BETA1=VCORE(EP,12)+VCORE(EP,8)*VPG(2,NG)
        KSI1=VCORE(EP,12)+VCORE(EP,10)*VPG(1,NG)
        BETA2=VCORE(EP,11)+VCORE(EP,7)*VPG(2,NG)
        KSI2=VCORE(EP,11)+VCORE(EP,9)*VPG(1,NG)
C-------NOEUD--- 1
        BC(I,1)=-C11-C12+BCT(1)
        BC(I,2)=-C21-C22+BCT(2)
        BC(I,3)= BETA1*C11+KSI1*C12+BCT(3)
        BC(I,4)= BETA1*C21+KSI1*C22+BCT(4)
        BC(I,5)= -BETA2*C11-KSI2*C12+BCT(5)
        BC(I,6)= -BETA2*C21-KSI2*C22+BCT(6)
C-------NOEUD--- 2
        BC(I,7)= C11-C12+BCT(7)
        BC(I,8)= C21-C22+BCT(8)
        BC(I,9)= BETA1*C11-KSI1*C12+BCT(9)
        BC(I,10)=BETA1*C21-KSI1*C22+BCT(10)
        BC(I,11)=-BETA2*C11+KSI2*C12+BCT(11)
        BC(I,12)=-BETA2*C21+KSI2*C22+BCT(12)
C-------NOEUD--- 3
        BC(I,13)=-BC(I,1)+BCT(1)+BCT(1)
        BC(I,14)=-BC(I,2)+BCT(2)+BCT(2)
        BC(I,15)=-BC(I,3)+BCT(3)+BCT(3)
        BC(I,16)=-BC(I,4)+BCT(4)+BCT(4)
        BC(I,17)=-BC(I,5)+BCT(5)+BCT(5)
        BC(I,18)=-BC(I,6)+BCT(6)+BCT(6)
C-------NOEUD--- 4
        BC(I,19)=-BC(I,7)+BCT(7)+BCT(7)
        BC(I,20)=-BC(I,8)+BCT(8)+BCT(8)
        BC(I,21)=-BC(I,9)+BCT(9)+BCT(9)
        BC(I,22)=-BC(I,10)+BCT(10)+BCT(10)
        BC(I,23)=-BC(I,11)+BCT(11)+BCT(11)
        BC(I,24)=-BC(I,12)+BCT(12)+BCT(12)
       ENDDO 
C
#include "vectorize.inc"
       DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
C---------------------------------------------------
C  CALCUL DE [FN] 
C---------------------------------------------------
          TFN(1,1)=VKSI(1,NG)*VQN(EP,7,1)+VKSI(2,NG)*VQN(EP,7,2)
     1            +VKSI(3,NG)*VQN(EP,7,3)+VKSI(4,NG)*VQN(EP,7,4)
          TFN(2,1)=VKSI(1,NG)*VQN(EP,8,1)+VKSI(2,NG)*VQN(EP,8,2)
     1            +VKSI(3,NG)*VQN(EP,8,3)+VKSI(4,NG)*VQN(EP,8,4)
          TFN(3,1)=VKSI(1,NG)*VQN(EP,9,1)+VKSI(2,NG)*VQN(EP,9,2)
     1            +VKSI(3,NG)*VQN(EP,9,3)+VKSI(4,NG)*VQN(EP,9,4)
          TFN(1,2)=VETA(1,NG)*VQN(EP,7,1)+VETA(2,NG)*VQN(EP,7,2)
     1            +VETA(3,NG)*VQN(EP,7,3)+VETA(4,NG)*VQN(EP,7,4)
          TFN(2,2)=VETA(1,NG)*VQN(EP,8,1)+VETA(2,NG)*VQN(EP,8,2)
     1            +VETA(3,NG)*VQN(EP,8,3)+VETA(4,NG)*VQN(EP,8,4)
          TFN(3,2)=VETA(1,NG)*VQN(EP,9,1)+VETA(2,NG)*VQN(EP,9,2)
     1            +VETA(3,NG)*VQN(EP,9,3)+VETA(4,NG)*VQN(EP,9,4)
C--------------------------------------------------------
C  CALCUL [BN]=[F]^-1 [FN]
C--------------------------------------------------------
          TBI(2,2)=VJFI(EP,1,1,NG)*TFN(1,1)+VJFI(EP,2,1,NG)*TFN(2,1)
     1           + VJFI(EP,3,1,NG)*TFN(3,1)
          TBI(2,1)=VJFI(EP,1,2,NG)*TFN(1,1)+VJFI(EP,2,2,NG)*TFN(2,1)
     1           + VJFI(EP,3,2,NG)*TFN(3,1)
          TBI(1,2)=VJFI(EP,1,1,NG)*TFN(1,2)+VJFI(EP,2,1,NG)*TFN(2,2)
     1           + VJFI(EP,3,1,NG)*TFN(3,2)
          TBI(1,1)=VJFI(EP,1,2,NG)*TFN(1,2)+VJFI(EP,2,2,NG)*TFN(2,2)
     1           + VJFI(EP,3,2,NG)*TFN(3,2)
C
        THK(EP) =-(TBI(1,1)+TBI(2,2))
        TBI(1,2)=-TBI(1,2)
        TBI(2,1)=-TBI(2,1)
C---------------------------
C  CALCUL DE TC=VJFI*VQ
C---------------------------
          TC(EP,1,1)=VJFI(EP,1,1,NG)*VQ(EP,1,1,NG)+VJFI(EP,2,1,NG)
     +             *VQ(EP,2,1,NG)+ VJFI(EP,3,1,NG)*VQ(EP,3,1,NG)
          TC(EP,2,1)=VJFI(EP,1,2,NG)*VQ(EP,1,1,NG)+VJFI(EP,2,2,NG)
     +             *VQ(EP,2,1,NG)+ VJFI(EP,3,2,NG)*VQ(EP,3,1,NG)
          TC(EP,1,2)=VJFI(EP,1,1,NG)*VQ(EP,1,2,NG)+VJFI(EP,2,1,NG)
     +             *VQ(EP,2,2,NG)+ VJFI(EP,3,1,NG)*VQ(EP,3,2,NG)
          TC(EP,2,2)=VJFI(EP,1,2,NG)*VQ(EP,1,2,NG)+VJFI(EP,2,2,NG)
     +             *VQ(EP,2,2,NG)+ VJFI(EP,3,2,NG)*VQ(EP,3,2,NG)
C------------------------------
C  CALCUL DE TBC=TBI*TC
C------------------------------
          TBC(EP,1,1)=TBI(1,1)*TC(EP,1,1)+TBI(1,2)*TC(EP,2,1)
          TBC(EP,2,1)=TBI(2,1)*TC(EP,1,1)+TBI(2,2)*TC(EP,2,1)
          TBC(EP,1,2)=TBI(1,1)*TC(EP,1,2)+TBI(1,2)*TC(EP,2,2)
          TBC(EP,2,2)=TBI(2,1)*TC(EP,1,2)+TBI(2,2)*TC(EP,2,2)
       ENDDO 
C
C--------------------------
C--------------------------
C     DEFORMATION IN-PLANE
C--------------------------
C      [BM](3,3*NPG),[BMF](3,3*NPG),[BF](3,2*NPG)
C--------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---------- NOEUDS J=1         II =0 KK = 0
#include "vectorize.inc"
       DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(1,NG)*TC(EP,1,1)+VETA(1,NG)*TC(EP,2,1)
         C2=VKSI(1,NG)*TC(EP,1,2)+VETA(1,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(1,NG)*TBC(EP,1,1)+VETA(1,NG)*TBC(EP,2,1)
         BC2=VKSI(1,NG)*TBC(EP,1,2)+VETA(1,NG)*TBC(EP,2,2)
C          I = 1; JJ = 3*KK =0
C---------CONTRIBUTION DU <T1> :EPS-X
         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BRZ(I,1,1)=-BXY(1)+BYX(1)
          BRZ(I,2,1)=-BXY(2)+BYX(2)
          BRZ(I,3,1)=-BXY(3)+BYX(3)
          BM(I,3)=BXY(1)+BYX(1)
          BM(I,6)=BXY(2)+BYX(2)
          BM(I,9)=BXY(3)+BYX(3)
         ELSE
          BM(I,9)=ZERO
         END IF
          BM(I,1)=VQ(EP,1,1,NG)*C1
          BMF(I,1)=THK(EP)*BM(I,1)+VQ(EP,1,1,NG)*BC1
C---------CONTRIBUTION DU <T2> :EPS-Y
          BM(I,2)=VQ(EP,1,2,NG)*C2
          BMF(I,2)=THK(EP)*BM(I,2)+VQ(EP,1,2,NG)*BC2
C---------CONTRIBUTION DU <T> :EPS-XY
          BMF(I,3)=THK(EP)*BM(I,3)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C          I = 2; JJ = 3*(KK+1)=3
C---------:EPS-X :EPS-Y :EPS-XY
          BM(I,4)=VQ(EP,2,1,NG)*C1
          BMF(I,4)=THK(EP)*BM(I,4)+VQ(EP,2,1,NG)*BC1
          BM(I,5)=VQ(EP,2,2,NG)*C2
          BMF(I,5)=THK(EP)*BM(I,5)+VQ(EP,2,2,NG)*BC2
          BMF(I,6)=THK(EP)*BM(I,6)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C          I = 3; JJ = 3*(KK+2)=6
C---------:EPS-X:EPS-Y:EPS-XY
          BM(I,7)=VQ(EP,3,1,NG)*C1
          BMF(I,7)=THK(EP)*BM(I,7)+VQ(EP,3,1,NG)*BC1
          BM(I,8)=VQ(EP,3,2,NG)*C2
          BMF(I,8)=THK(EP)*BM(I,8)+VQ(EP,3,2,NG)*BC2
          BMF(I,9)=THK(EP)*BM(I,9)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C---------- TERMES ASSOCIEES AU BETA ----------
C---------PRODUIT SCALAIRE <T(PG)>*<-T^2 T^1>----------
         V1(2)=VQ(EP,1,1,NG)*VQN(EP,1,1)+VQ(EP,2,1,NG)*VQN(EP,2,1)
     1           +VQ(EP,3,1,NG)*VQN(EP,3,1)
         V1(1)=-(VQ(EP,1,1,NG)*VQN(EP,4,1)+VQ(EP,2,1,NG)*VQN(EP,5,1)
     1           +VQ(EP,3,1,NG)*VQN(EP,6,1))
         V2(2)=VQ(EP,1,2,NG)*VQN(EP,1,1)+VQ(EP,2,2,NG)*VQN(EP,2,1)
     1           +VQ(EP,3,2,NG)*VQN(EP,3,1)
         V2(1)=-(VQ(EP,1,2,NG)*VQN(EP,4,1)+VQ(EP,2,2,NG)*VQN(EP,5,1)
     1           +VQ(EP,3,2,NG)*VQN(EP,6,1))
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II =0
            BF(I,1)=V1(1)*C1
            BF(I,2)=V2(1)*C2
            BF(I,3)=V1(1)*C2+V2(1)*C1
            BF(I,4)=V1(2)*C1
            BF(I,5)=V2(2)*C2
            BF(I,6)=V1(2)*C2+V2(2)*C1
       ENDDO 
C
C--------J=2---II=(J-1)*2 =2  KK = 3*(J-1) =3
#include "vectorize.inc"
       DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(2,NG)*TC(EP,1,1)+VETA(2,NG)*TC(EP,2,1)
         C2=VKSI(2,NG)*TC(EP,1,2)+VETA(2,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(2,NG)*TBC(EP,1,1)+VETA(2,NG)*TBC(EP,2,1)
         BC2=VKSI(2,NG)*TBC(EP,1,2)+VETA(2,NG)*TBC(EP,2,2)
C
C         I = 1; JJ = 3*KK =9
C---------:EPS-X:EPS-Y:EPS-XY
         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BRZ(I,1,2)=-BXY(1)+BYX(1)
          BRZ(I,2,2)=-BXY(2)+BYX(2)
          BRZ(I,3,2)=-BXY(3)+BYX(3)
          BM(I,12)=BXY(1)+BYX(1)
          BM(I,15)=BXY(2)+BYX(2)
          BM(I,18)=BXY(3)+BYX(3)
         ELSE
          BM(I,18)=ZERO
         END IF
          BM(I,10)=VQ(EP,1,1,NG)*C1
          BMF(I,10)=THK(EP)*BM(I,10)+VQ(EP,1,1,NG)*BC1
          BM(I,11)=VQ(EP,1,2,NG)*C2
          BMF(I,11)=THK(EP)*BM(I,11)+VQ(EP,1,2,NG)*BC2
          BMF(I,12)=THK(EP)*BM(I,12)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C
C         I = 2; JJ = 3*(KK+1)=12
C---------:EPS-X:EPS-Y:EPS-XY
          BM(I,13)=VQ(EP,2,1,NG)*C1
          BMF(I,13)=THK(EP)*BM(I,13)+VQ(EP,2,1,NG)*BC1
          BM(I,14)=VQ(EP,2,2,NG)*C2
          BMF(I,14)=THK(EP)*BM(I,14)+VQ(EP,2,2,NG)*BC2
          BMF(I,15)=THK(EP)*BM(I,15)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C
C         I = 3; JJ = 3*(KK+2)=15
C---------:EPS-X:EPS-Y:EPS-XY
          BM(I,16)=VQ(EP,3,1,NG)*C1
          BMF(I,16)=THK(EP)*BM(I,16)+VQ(EP,3,1,NG)*BC1
          BM(I,17)=VQ(EP,3,2,NG)*C2
          BMF(I,17)=THK(EP)*BM(I,17)+VQ(EP,3,2,NG)*BC2
          BMF(I,18)=THK(EP)*BM(I,18)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C
C---------- TERMES ASSOCIEES AU BETA ----------
C---------PRODUIT SCALAIRE <T(PG)>*<-T^2 T^1>----------
         V1(2)=VQ(EP,1,1,NG)*VQN(EP,1,2)+VQ(EP,2,1,NG)*VQN(EP,2,2)
     1           +VQ(EP,3,1,NG)*VQN(EP,3,2)
         V1(1)=-(VQ(EP,1,1,NG)*VQN(EP,4,2)+VQ(EP,2,1,NG)*VQN(EP,5,2)
     1           +VQ(EP,3,1,NG)*VQN(EP,6,2))
         V2(2)=VQ(EP,1,2,NG)*VQN(EP,1,2)+VQ(EP,2,2,NG)*VQN(EP,2,2)
     1           +VQ(EP,3,2,NG)*VQN(EP,3,2)
         V2(1)=-(VQ(EP,1,2,NG)*VQN(EP,4,2)+VQ(EP,2,2,NG)*VQN(EP,5,2)
     1           +VQ(EP,3,2,NG)*VQN(EP,6,2))
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II=6
            BF(I,7)=V1(1)*C1
            BF(I,8)=V2(1)*C2
            BF(I,9)=V1(1)*C2+V2(1)*C1
            BF(I,10)=V1(2)*C1
            BF(I,11)=V2(2)*C2
            BF(I,12)=V1(2)*C2+V2(2)*C1
       ENDDO 
C
C--------J=3---II=(J-1)*2 =4 KK = 3*(J-1)=6
#include "vectorize.inc"
       DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(3,NG)*TC(EP,1,1)+VETA(3,NG)*TC(EP,2,1)
         C2=VKSI(3,NG)*TC(EP,1,2)+VETA(3,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(3,NG)*TBC(EP,1,1)+VETA(3,NG)*TBC(EP,2,1)
         BC2=VKSI(3,NG)*TBC(EP,1,2)+VETA(3,NG)*TBC(EP,2,2)
C         I = 1;JJ = 3*KK=18
C---------:EPS-X:EPS-Y:EPS-XY
         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BRZ(I,1,3)=-BXY(1)+BYX(1)
          BRZ(I,2,3)=-BXY(2)+BYX(2)
          BRZ(I,3,3)=-BXY(3)+BYX(3)
          BM(I,21)=BXY(1)+BYX(1)
          BM(I,24)=BXY(2)+BYX(2)
          BM(I,27)=BXY(3)+BYX(3)
         ELSE
          BM(I,27)=ZERO
         END IF
          BM(I,19)=VQ(EP,1,1,NG)*C1
          BMF(I,19)=THK(EP)*BM(I,19)+VQ(EP,1,1,NG)*BC1
          BM(I,20)=VQ(EP,1,2,NG)*C2
          BMF(I,20)=THK(EP)*BM(I,20)+VQ(EP,1,2,NG)*BC2
          BMF(I,21)=THK(EP)*BM(I,21)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C         I = 2; JJ = 3*(KK+1)=21
          BM(I,22)=VQ(EP,2,1,NG)*C1
          BMF(I,22)=THK(EP)*BM(I,22)+VQ(EP,2,1,NG)*BC1
          BM(I,23)=VQ(EP,2,2,NG)*C2
          BMF(I,23)=THK(EP)*BM(I,23)+VQ(EP,2,2,NG)*BC2
          BMF(I,24)=THK(EP)*BM(I,24)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C         I = 3; JJ = 3*(KK+2)=24
          BM(I,25)=VQ(EP,3,1,NG)*C1
          BMF(I,25)=THK(EP)*BM(I,25)+VQ(EP,3,1,NG)*BC1
          BM(I,26)=VQ(EP,3,2,NG)*C2
          BMF(I,26)=THK(EP)*BM(I,26)+VQ(EP,3,2,NG)*BC2
          BMF(I,27)=THK(EP)*BM(I,27)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C
C---------DEF DE FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
C---------- TERMES ASSOCIEES AU BETA ----------
C---------PRODUIT SCALAIRE <T(PG)>*<-T^2 T^1>----------
         V1(2)=VQ(EP,1,1,NG)*VQN(EP,1,3)+VQ(EP,2,1,NG)*VQN(EP,2,3)
     1           +VQ(EP,3,1,NG)*VQN(EP,3,3)
         V1(1)=-(VQ(EP,1,1,NG)*VQN(EP,4,3)+VQ(EP,2,1,NG)*VQN(EP,5,3)
     1           +VQ(EP,3,1,NG)*VQN(EP,6,3))
         V2(2)=VQ(EP,1,2,NG)*VQN(EP,1,3)+VQ(EP,2,2,NG)*VQN(EP,2,3)
     1           +VQ(EP,3,2,NG)*VQN(EP,3,3)
         V2(1)=-(VQ(EP,1,2,NG)*VQN(EP,4,3)+VQ(EP,2,2,NG)*VQN(EP,5,3)
     1           +VQ(EP,3,2,NG)*VQN(EP,6,3))
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II =12
            BF(I,13)=V1(1)*C1
            BF(I,14)=V2(1)*C2
            BF(I,15)=V1(1)*C2+V2(1)*C1
            BF(I,16)=V1(2)*C1
            BF(I,17)=V2(2)*C2
            BF(I,18)=V1(2)*C2+V2(2)*C1
       ENDDO 
C
C--------J=4---II=(J-1)*2  =6 KK = 3*(J-1)=9
#include "vectorize.inc"
       DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(4,NG)*TC(EP,1,1)+VETA(4,NG)*TC(EP,2,1)
         C2=VKSI(4,NG)*TC(EP,1,2)+VETA(4,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(4,NG)*TBC(EP,1,1)+VETA(4,NG)*TBC(EP,2,1)
         BC2=VKSI(4,NG)*TBC(EP,1,2)+VETA(4,NG)*TBC(EP,2,2)
C         I = 1; JJ = 3*KK =27
          BM(I,28)=VQ(EP,1,1,NG)*C1
          BMF(I,28)=THK(EP)*BM(I,28)+VQ(EP,1,1,NG)*BC1
          BM(I,29)=VQ(EP,1,2,NG)*C2
          BMF(I,29)=THK(EP)*BM(I,29)+VQ(EP,1,2,NG)*BC2
C---------CONTRIBUTION DU <T> :EPS-XY
         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BRZ(I,1,4)=-BXY(1)+BYX(1)
          BRZ(I,2,4)=-BXY(2)+BYX(2)
          BRZ(I,3,4)=-BXY(3)+BYX(3)
          BM(I,30)=BXY(1)+BYX(1)
          BM(I,33)=BXY(2)+BYX(2)
          BM(I,36)=BXY(3)+BYX(3)
         ELSE
          BM(I,36)=ZERO
         END IF
          BMF(I,30)=THK(EP)*BM(I,30)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C         I = 2; JJ = 3*(KK+1)=30
          BM(I,31)=VQ(EP,2,1,NG)*C1
          BMF(I,31)=THK(EP)*BM(I,31)+VQ(EP,2,1,NG)*BC1
          BM(I,32)=VQ(EP,2,2,NG)*C2
          BMF(I,32)=THK(EP)*BM(I,32)+VQ(EP,2,2,NG)*BC2
          BMF(I,33)=THK(EP)*BM(I,33)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C         I = 3; JJ = 3*(KK+2)=33
          BM(I,34)=VQ(EP,3,1,NG)*C1
          BMF(I,34)=THK(EP)*BM(I,34)+VQ(EP,3,1,NG)*BC1
          BM(I,35)=VQ(EP,3,2,NG)*C2
          BMF(I,35)=THK(EP)*BM(I,35)+VQ(EP,3,2,NG)*BC2
          BMF(I,36)=THK(EP)*BM(I,36)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C
C---------- TERMES ASSOCIEES AU BETA ----------
C---------PRODUIT SCALAIRE <T(PG)>*<-T^2 T^1>----------
         V1(2)=VQ(EP,1,1,NG)*VQN(EP,1,4)+VQ(EP,2,1,NG)*VQN(EP,2,4)
     1           +VQ(EP,3,1,NG)*VQN(EP,3,4)
         V1(1)=-(VQ(EP,1,1,NG)*VQN(EP,4,4)+VQ(EP,2,1,NG)*VQN(EP,5,4)
     1           +VQ(EP,3,1,NG)*VQN(EP,6,4))
         V2(2)=VQ(EP,1,2,NG)*VQN(EP,1,4)+VQ(EP,2,2,NG)*VQN(EP,2,4)
     1           +VQ(EP,3,2,NG)*VQN(EP,3,4)
         V2(1)=-(VQ(EP,1,2,NG)*VQN(EP,4,4)+VQ(EP,2,2,NG)*VQN(EP,5,4)
     1           +VQ(EP,3,2,NG)*VQN(EP,6,4))
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II=18
            BF(I,19)=V1(1)*C1
            BF(I,20)=V2(1)*C2
            BF(I,21)=V1(1)*C2+V2(1)*C1
            BF(I,22)=V1(2)*C1
            BF(I,23)=V2(2)*C2
            BF(I,24)=V1(2)*C2+V2(2)*C1
C
       ENDDO 
C--------------------------
C     DEFORMATION OUT-PLANE (C.T)
C--------------------------
C      [BC](2,5*NPG),VDEF(4,5) 
C--------------------------
#include "vectorize.inc"
       DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
        V11(1)=VKSI(2,NG)
        V11(2)=VKSI(3,NG)
        V11(3)=VETA(4,NG)
        V11(4)=VETA(3,NG)
C--- J=1---------------------
C-----------POINTEURS POUR BC(II,J1~J2)
C         J1=(KA1(1,J)-1)*5=0
C         J2=(KA1(2,J)-1)*5=5
C         II=KIND(J)       =1
C         JJ = 3*(J-1)    =0
C--------TERMES <NAI>-------
          C1=V11(1)*VNRM(EP,1)
          BC(I,1)=-C1
          BC(I,11)=C1
C
          C1=V11(1)*VNRM(EP,2)
          BC(I,3)=-C1
          BC(I,13)=C1
C
          C1=V11(1)*VNRM(EP,3)
          BC(I,5)=-C1
          BC(I,15)=C1
C--------TERMES <ASI>-------
          BC(I,7)=V11(1)*VASTN(EP,1)
          BC(I,9)=V11(1)*VASTN(EP,2)
          BC(I,17)=V11(1)*VASTN(EP,3)
          BC(I,19)=V11(1)*VASTN(EP,4)
C--- J=2---------------------
C-----------POINTEURS POUR BC(II,J1~J2)
C         J1=(KA1(1,J)-1)*5 =15
C         J2=(KA1(2,J)-1)*5 =10
C         II=KIND(J)        =1
C         JJ = 3*(J-1)     =3
C--------TERMES <NAI>-------
          C1=V11(2)*VNRM(EP,4)
          BC(I,31)=-C1
          BC(I,21)=C1
C
          C1=V11(2)*VNRM(EP,5)
          BC(I,33)=-C1
          BC(I,23)=C1
C
          C1=V11(2)*VNRM(EP,6)
          BC(I,35)=-C1
          BC(I,25)=C1
C--------TERMES <ASI>-------
          BC(I,37)=V11(2)*VASTN(EP,5)
          BC(I,39)=V11(2)*VASTN(EP,6)
          BC(I,27)=V11(2)*VASTN(EP,7)
          BC(I,29)=V11(2)*VASTN(EP,8)
C--- J=3---------------------
C-----------POINTEURS POUR BC(II,J1~J2)
C         J1=(KA1(1,J)-1)*5 =0
C         J2=(KA1(2,J)-1)*5 =15
C         II=KIND(J)        =2
C         JJ = 3*(J-1)     =6
C--------TERMES <NAI>-------
          C1=V11(3)*VNRM(EP,7)
          BC(I,2)=-C1
          BC(I,32)=C1
C
          C1=V11(3)*VNRM(EP,8)
          BC(I,4)=-C1
          BC(I,34)=C1
C
          C1=V11(3)*VNRM(EP,9)
          BC(I,6)=-C1
          BC(I,36)=C1
C--------TERMES <ASI>-------
          BC(I,8)=V11(3)*VASTN(EP,9)
          BC(I,10)=V11(3)*VASTN(EP,10)
          BC(I,38)=V11(3)*VASTN(EP,11)
          BC(I,40)=V11(3)*VASTN(EP,12)
C--- J=4---------------------
C-----------POINTEURS POUR BC(II,J1~J2)
C         J1=(KA1(1,J)-1)*5 =5
C         J2=(KA1(2,J)-1)*5 =10
C         II=KIND(J)        =2
C         JJ = 3*(J-1)     =9
C--------TERMES <NAI>-------
          C1=V11(4)*VNRM(EP,10)
          BC(I,12)=-C1
          BC(I,22)=C1
C
          C1=V11(4)*VNRM(EP,11)
          BC(I,14)=-C1
          BC(I,24)=C1
C
          C1=V11(4)*VNRM(EP,12)
          BC(I,16)=-C1
          BC(I,26)=C1
C--------TERMES <ASI>-------
          BC(I,18)=V11(4)*VASTN(EP,13)
          BC(I,20)=V11(4)*VASTN(EP,14)
          BC(I,28)=V11(4)*VASTN(EP,15)
          BC(I,30)=V11(4)*VASTN(EP,16)
       ENDDO 
C--------pour rigidite en rzz-------
       IF (ISROT==0) THEN
#include "vectorize.inc"
       DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
        BZZ(I,1)=TC(EP,1,1)*VKSI(1,NG)+TC(EP,2,1)*VETA(1,NG)
        BZZ(I,2)=TC(EP,1,2)*VKSI(1,NG)+TC(EP,2,2)*VETA(1,NG)
        BZZ(I,3)=TC(EP,1,1)*VKSI(2,NG)+TC(EP,2,1)*VETA(2,NG)
        BZZ(I,4)=TC(EP,1,2)*VKSI(2,NG)+TC(EP,2,2)*VETA(2,NG)
        BZZ(I,5)=TC(EP,1,1)*VKSI(3,NG)+TC(EP,2,1)*VETA(3,NG)
        BZZ(I,6)=TC(EP,1,2)*VKSI(3,NG)+TC(EP,2,2)*VETA(3,NG)
        BZZ(I,7)=TC(EP,1,1)*VKSI(4,NG)+TC(EP,2,1)*VETA(4,NG)
        BZZ(I,8)=TC(EP,1,2)*VKSI(4,NG)+TC(EP,2,2)*VETA(4,NG)
       ENDDO 
       END IF !(ISROT==0) THEN
       RETURN
       END
Chd|====================================================================
Chd|  CBABEC3                       source/elements/shell/coqueba/cbabe3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBABEC3(JFT  ,JLT  ,X13  ,X24  ,Y13  ,Y24  ,
     1                                         BM, NPLAT, IPLAT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL 'membrane shear traitement' use only PARTIE CONSTANTE
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
       INTEGER JFT ,JLT,NPLAT,IPLAT(*)
       my_real 
     .     X13(*),X24(*),Y13(*),Y24(*),BM(MVSIZ,36)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J,EP
#include "vectorize.inc"
       DO EP=NPLAT+1,JLT 
        I=IPLAT(EP)
          BM(EP,3)=-X24(I)
          BM(EP,6)=Y24(I)
          BM(EP,12)=X13(I)
          BM(EP,15)=-Y13(I)
          BM(EP,21)=-BM(EP,3)
          BM(EP,24)=-BM(EP,6)
          BM(EP,30)=-BM(EP,12)
          BM(EP,33)=-BM(EP,15)
          BM(EP,9)=ZERO
          BM(EP,18)=ZERO
          BM(EP,27)=ZERO
          BM(EP,36)=ZERO
       ENDDO
       RETURN
       END
Chd|====================================================================
Chd|  CBABER3                       source/elements/shell/coqueba/cbabe3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBABER3(JFT ,JLT  ,BM0RZ,BMKRZ,BMERZ  ,
     2                     BMRZ ,BRZ  ,BM   ,NPLAT  ,IPLAT,
     3                     NG   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IPLAT(*),NPLAT,NG
      my_real
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .   BMRZ(MVSIZ,3,4),BM(MVSIZ,36),BRZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,EP,NPG
      PARAMETER (NPG = 4)
      my_real
     .   VPG(2,NPG),PG,PG1
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C---------------
C--------pay attention QEPH: Bi*A--- RLZ: VRZ/A-- QBAT:Bi--RLZ:VRZ
      DO J=1,4
        DO I=JFT,JLT
        BMRZ(I,1,J) = BM0RZ(I,1,J)+BMKRZ(I,1,J)*VPG(1,NG)+
     .                BMERZ(I,1,J)*VPG(2,NG)
        BMRZ(I,2,J) = BM0RZ(I,2,J)+BMKRZ(I,2,J)*VPG(1,NG)+
     .                BMERZ(I,2,J)*VPG(2,NG)
        BMRZ(I,3,J) = BM0RZ(I,3,J)+BMKRZ(I,3,J)*VPG(1,NG)+
     .                BMERZ(I,3,J)*VPG(2,NG)
C
        BRZ(I,4,J) = BM0RZ(I,4,J)+BMKRZ(I,4,J)*VPG(1,NG)+
     .               BMERZ(I,4,J)*VPG(2,NG)
       ENDDO
      ENDDO
C
      RETURN
      END
