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
Chd|  CBADEF                        source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADEF(JFT,JLT,NG,VCORE,AREA,CDET,VQN,VQ,VJFI,
     1                  VXYZ,RXYZ,VDEF,VNRM,VASTN,
     2                  HX,HY,VETA,VKSI,BM,BMF,BF,BC,TC,NPLAT,IPLAT,
     3                  ISROT,BRZ )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL DES DEFORMATIONS GENERALISEES AUX POINTS DE GAUSS ET [B]
C        ENTREES :  NEL,NG,VCORE,VQN,VXYZ,RXYZ,VNRM,VASM,VASTN
C        SORTIES : VDEF,BM (MEMBRANE),BMF(COUPLAGE MEM-FLEXION),BF (FLEXION),BC(C.T)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NPG,NG,JFT,JLT,NPLAT,IPLAT(*),ISROT
      PARAMETER (NPG = 4)
      MY_REAL 
     .   RXYZ(MVSIZ,8),VCORE(MVSIZ,12),VXYZ(MVSIZ,12),
     .   VQN(MVSIZ,9,4),VKSI(4,4),VETA(4,4),
     .   BM(MVSIZ,36),BMF(MVSIZ,36),BF(MVSIZ,24),BC(MVSIZ,40),HX(MVSIZ,4),HY(MVSIZ,4)
      MY_REAL 
     .   VNRM(MVSIZ,12),VASTN(MVSIZ,16),VJFI(MVSIZ,3,2,4),
     .   VQ(MVSIZ,3,3,4),VDEF(MVSIZ,8),DT1,AREA(*)
      MY_REAL 
     .   CDET(*),TC(MVSIZ,2,2),BRZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,IUN,NG1,K
      MY_REAL 
     .   VPG(2,NPG),PG1,PG,THK,DETJ,DET,
     .   TFN(3,2),BCX,BCY,BXY(3),BYX(3),
     .   V1(2),V2(2),RV1,RV2,
     .   C1,C2,VT1,VT2,BC1,BC2,VB1,
     .   VJF1(2,3),VJF(3,3),TBI(2,2),TBC(2,2),V11(4)
      MY_REAL 
     .   A_1,C11,C12,C21,C22,CC,BETA1,KSI1,KSIY1,BETA2,KSI2,KSIY2
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA IUN/1/
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
C-------VCORE(12):BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
C-------V13(I)->VXYZ(j,1,I),V24(I)->VXYZ(j,2,I),VHI(I)->VXYZ(j,3,I)------------
        BM(EP,1)=VCORE(EP,1)+HX(EP,NG)*VCORE(EP,5)
        BM(EP,2)=VCORE(EP,2)+HX(EP,NG)*VCORE(EP,6)
        BM(EP,3)=HX(EP,NG)*FOURTH
        BM(EP,4)=-BM(EP,3)
        BM(EP,5)=VCORE(EP,3)+HY(EP,NG)*VCORE(EP,5)
        BM(EP,6)=VCORE(EP,4)+HY(EP,NG)*VCORE(EP,6)
        BM(EP,7)=HY(EP,NG)*FOURTH
        BM(EP,8)=-BM(EP,7)
C
        VDEF(EP,1)=BM(EP,1)*VXYZ(EP,1)+BM(EP,2)*VXYZ(EP,4)
     1          +BM(EP,3)*VXYZ(EP,7)
        VDEF(EP,2)=BM(EP,5)*VXYZ(EP,2)+BM(EP,6)*VXYZ(EP,5)
     1          +BM(EP,7)*VXYZ(EP,8)
C--------FLEXION-----
        VDEF(EP,6)=BM(EP,1)*RXYZ(EP,2)+BM(EP,2)*RXYZ(EP,4)
     1           +BM(EP,3)*RXYZ(EP,6)
        VDEF(EP,7)=-(BM(EP,5)*RXYZ(EP,1)+BM(EP,6)*RXYZ(EP,3)
     1           +BM(EP,7)*RXYZ(EP,5))
        VDEF(EP,8)=-(BM(EP,1)*RXYZ(EP,1)+BM(EP,2)*RXYZ(EP,3)
     1            +BM(EP,3)*RXYZ(EP,5))
     2            +BM(EP,5)*RXYZ(EP,2)+BM(EP,6)*RXYZ(EP,4)
     3            +BM(EP,7)*RXYZ(EP,6)
C--------C.T-----
C-------VCORE(12):BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
       A_1 = 0.25/MAX(CDET(EP),EM20)
       C11=(VCORE(EP,10)+VCORE(EP,12)*VPG(1,NG))*A_1
       C12=-(VCORE(EP,8)+VCORE(EP,12)*VPG(2,NG))*A_1
       C21=-(VCORE(EP,9)+VCORE(EP,11)*VPG(1,NG))*A_1
       C22=( VCORE(EP,7)+VCORE(EP,11)*VPG(2,NG))*A_1
       BETA1=VCORE(EP,12)+VCORE(EP,8)*VPG(2,NG)
        KSI1=VCORE(EP,12)+VCORE(EP,10)*VPG(1,NG)
       BETA2=VCORE(EP,11)+VCORE(EP,7)*VPG(2,NG)
        KSI2=VCORE(EP,11)+VCORE(EP,9)*VPG(1,NG)
C-------NOEUD--- 1
       BC(EP,1)=-C11-C12
       BC(EP,2)=-C21-C22
       BC(EP,3)= BETA1*C11+KSI1*C12
       BC(EP,4)= BETA1*C21+KSI1*C22
       BC(EP,5)= -BETA2*C11-KSI2*C12
       BC(EP,6)= -BETA2*C21-KSI2*C22
C-------NOEUD--- 2
       BC(EP,7)= C11-C12
       BC(EP,8)= C21-C22
       BC(EP,9)= BETA1*C11-KSI1*C12
       BC(EP,10)=BETA1*C21-KSI1*C22
       BC(EP,11)=-BETA2*C11+KSI2*C12
       BC(EP,12)=-BETA2*C21+KSI2*C22
C
       BETA1=VCORE(EP,8)+VCORE(EP,12)*VPG(2,NG)
        KSI1=VCORE(EP,10)+VCORE(EP,12)*VPG(1,NG)
       BETA2=VCORE(EP,7)+VCORE(EP,11)*VPG(2,NG)
        KSI2=VCORE(EP,9)+VCORE(EP,11)*VPG(1,NG)
C-----PARTIE SYM---------
C-------NOEUD--- 1
       BC(EP,13)=C11*VPG(2,NG)+C12*VPG(1,NG)
       BC(EP,14)=C21*VPG(2,NG)+C22*VPG(1,NG)
       BC(EP,15)=-BETA1*C11-KSI1*C12
       BC(EP,16)=-BETA1*C21-KSI1*C22
       BC(EP,17)=C11*BETA2+C12*KSI2
       BC(EP,18)=C21*BETA2+C22*KSI2
C-------NOEUD--- 2
       BC(EP,19)=-BC(EP,13)
       BC(EP,20)=-BC(EP,14)
       BC(EP,21)=BC(EP,15)
       BC(EP,22)=BC(EP,16)
       BC(EP,23)=BC(EP,17)
       BC(EP,24)=BC(EP,18)
C
        VDEF(EP,4)=BC(EP,1)*VXYZ(EP,3)+BC(EP,7)*VXYZ(EP,6)
     1            +BC(EP,13)*VXYZ(EP,9)+BC(EP,3)*RXYZ(EP,1)
     2            +BC(EP,9)*RXYZ(EP,3)+BC(EP,15)*RXYZ(EP,7)
     3            +BC(EP,5)*RXYZ(EP,2)+BC(EP,11)*RXYZ(EP,4)
     4            +BC(EP,17)*RXYZ(EP,8)
        VDEF(EP,5)=BC(EP,2)*VXYZ(EP,3)+BC(EP,8)*VXYZ(EP,6)
     1            +BC(EP,14)*VXYZ(EP,9)+BC(EP,4)*RXYZ(EP,1)
     2            +BC(EP,10)*RXYZ(EP,3)+BC(EP,16)*RXYZ(EP,7)
     3            +BC(EP,6)*RXYZ(EP,2)+BC(EP,12)*RXYZ(EP,4)
     4            +BC(EP,18)*RXYZ(EP,8)
       ENDDO
       IF (ISROT>0) THEN
#include "vectorize.inc"
        DO I=JFT,NPLAT
         EP=IPLAT(I)
C----------no more constant shear-----------
         VDEF(EP,3)=BM(EP,1)*VXYZ(EP,2)+BM(EP,2)*VXYZ(EP,5)
     1             +BM(EP,3)*VXYZ(EP,8)
     1             +BM(EP,5)*VXYZ(EP,1)+BM(EP,6)*VXYZ(EP,4)
     1             +BM(EP,7)*VXYZ(EP,7)
        END DO
       END IF !(ISROT>0) THEN
C------------ELEMENT GAUCH-------------------
#include "vectorize.inc"
       DO 150 I=NPLAT+1,JLT 
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
        THK =-(TBI(1,1)+TBI(2,2))
C       THK(2)=TBI(1,1)*TBI(2,2)-TBI(1,2)*TBI(2,1)
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
          TBC(1,1)=TBI(1,1)*TC(EP,1,1)+TBI(1,2)*TC(EP,2,1)
          TBC(2,1)=TBI(2,1)*TC(EP,1,1)+TBI(2,2)*TC(EP,2,1)
          TBC(1,2)=TBI(1,1)*TC(EP,1,2)+TBI(1,2)*TC(EP,2,2)
          TBC(2,2)=TBI(2,1)*TC(EP,1,2)+TBI(2,2)*TC(EP,2,2)
C--------------------------
C     CONSTRUIRE LA MATRICE [B] ET DEFORMATION
C--------------------------
C--------------------------
C     DEFORMATION IN-PLANE
C--------------------------
C      [BM](3,3*NPG),[BMF](3,3*NPG),
C      [BF](3,2*NPG),VDEF(8), 
C--------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---------- BOUCLE SUR LES NOEUDS---
C--------J=1         II =0 KK = 0
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
         VT1=VQ(EP,1,1,NG)*VXYZ(EP,1)+VQ(EP,2,1,NG)*VXYZ(EP,2)
     1        +VQ(EP,3,1,NG)*VXYZ(EP,3)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
         VT2=VQ(EP,1,2,NG)*VXYZ(EP,1)+VQ(EP,2,2,NG)*VXYZ(EP,2)
     1        +VQ(EP,3,2,NG)*VXYZ(EP,3)
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(1,NG)*TC(EP,1,1)+VETA(1,NG)*TC(EP,2,1)
         C2=VKSI(1,NG)*TC(EP,1,2)+VETA(1,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(1,NG)*TBC(1,1)+VETA(1,NG)*TBC(2,1)
         BC2=VKSI(1,NG)*TBC(1,2)+VETA(1,NG)*TBC(2,2)
C          I = 1; JJ = 3*KK =0
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
          BM(EP,3)=BXY(1)+BYX(1)
          BM(EP,6)=BXY(2)+BYX(2)
          BM(EP,9)=BXY(3)+BYX(3)
          VDEF(EP,3)= C1*VT2+C2*VT1
         ELSE
          BM(EP,9)=ZERO
         END IF
C---------CONTRIBUTION DU <T1> :EPS-X
          BM(EP,1)=VQ(EP,1,1,NG)*C1
          BMF(EP,1)=THK*BM(EP,1)+VQ(EP,1,1,NG)*BC1
C---------CONTRIBUTION DU <T2> :EPS-Y
          BM(EP,2)=VQ(EP,1,2,NG)*C2
          BMF(EP,2)=THK*BM(EP,2)+VQ(EP,1,2,NG)*BC2
C---------CONTRIBUTION DU <T> :EPS-XY
          BMF(EP,3)=THK*BM(EP,3)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C          I = 2; JJ = 3*(KK+1)=3
C---------:EPS-X :EPS-Y :EPS-XY
          BM(EP,4)=VQ(EP,2,1,NG)*C1
          BMF(EP,4)=THK*BM(EP,4)+VQ(EP,2,1,NG)*BC1
          BM(EP,5)=VQ(EP,2,2,NG)*C2
          BMF(EP,5)=THK*BM(EP,5)+VQ(EP,2,2,NG)*BC2
          BMF(EP,6)=THK*BM(EP,6)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C          I = 3; JJ = 3*(KK+2)=6
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,7)=VQ(EP,3,1,NG)*C1
          BMF(EP,7)=THK*BM(EP,7)+VQ(EP,3,1,NG)*BC1
          BM(EP,8)=VQ(EP,3,2,NG)*C2
          BMF(EP,8)=THK*BM(EP,8)+VQ(EP,3,2,NG)*BC2
          BMF(EP,9)=THK*BM(EP,9)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C
C---------DEF MEMBRANE------
          VDEF(EP,1)= C1*VT1
          VDEF(EP,2)= C2*VT2
C
C---------FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
          VDEF(EP,6)= BC1*VT1
          VDEF(EP,7)= BC2*VT2
          VDEF(EP,8)= BC1*VT2+BC2*VT1
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
C----------RV1=<V1>*RXYZ AU CHAQUE NOEUD---------
          RV1=V1(1)*RXYZ(EP,1)+V1(2)*RXYZ(EP,2)
C----------RV2=<V2>*RXYZ AU CHAQUE NOEUD---------
          RV2=V2(1)*RXYZ(EP,1)+V2(2)*RXYZ(EP,2)
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II =0
            BF(EP,1)=V1(1)*C1
            BF(EP,2)=V2(1)*C2
            BF(EP,3)=V1(1)*C2+V2(1)*C1
            BF(EP,4)=V1(2)*C1
            BF(EP,5)=V2(2)*C2
            BF(EP,6)=V1(2)*C2+V2(2)*C1
C---------DEF DE FLEXION ASSOCIEE AUX THETA1,THETA2------
          VDEF(EP,6)= VDEF(EP,6)+C1*RV1
          VDEF(EP,7)= VDEF(EP,7)+C2*RV2
          VDEF(EP,8)= VDEF(EP,8)+C1*RV2+C2*RV1
C
C--------J=2---II=(J-1)*2 =2  KK = 3*(J-1) =3
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
         VT1=VQ(EP,1,1,NG)*VXYZ(EP,4)+VQ(EP,2,1,NG)*VXYZ(EP,5)
     1        +VQ(EP,3,1,NG)*VXYZ(EP,6)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
         VT2=VQ(EP,1,2,NG)*VXYZ(EP,4)+VQ(EP,2,2,NG)*VXYZ(EP,5)
     1        +VQ(EP,3,2,NG)*VXYZ(EP,6)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(2,NG)*TC(EP,1,1)+VETA(2,NG)*TC(EP,2,1)
         C2=VKSI(2,NG)*TC(EP,1,2)+VETA(2,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(2,NG)*TBC(1,1)+VETA(2,NG)*TBC(2,1)
         BC2=VKSI(2,NG)*TBC(1,2)+VETA(2,NG)*TBC(2,2)
C
C         I = 1; JJ = 3*KK =9
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
          BM(EP,12)=BXY(1)+BYX(1)
          BM(EP,15)=BXY(2)+BYX(2)
          BM(EP,18)=BXY(3)+BYX(3)
          VDEF(EP,3)= VDEF(EP,3)+ C1*VT2+C2*VT1
         ELSE
          BM(EP,18)=ZERO
         END IF 
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,10)=VQ(EP,1,1,NG)*C1
          BMF(EP,10)=THK*BM(EP,10)+VQ(EP,1,1,NG)*BC1
          BM(EP,11)=VQ(EP,1,2,NG)*C2
          BMF(EP,11)=THK*BM(EP,11)+VQ(EP,1,2,NG)*BC2
          BMF(EP,12)=THK*BM(EP,12)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C
C         I = 2; JJ = 3*(KK+1)=12
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,13)=VQ(EP,2,1,NG)*C1
          BMF(EP,13)=THK*BM(EP,13)+VQ(EP,2,1,NG)*BC1
          BM(EP,14)=VQ(EP,2,2,NG)*C2
          BMF(EP,14)=THK*BM(EP,14)+VQ(EP,2,2,NG)*BC2
          BMF(EP,15)=THK*BM(EP,15)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C
C         I = 3; JJ = 3*(KK+2)=15
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,16)=VQ(EP,3,1,NG)*C1
          BMF(EP,16)=THK*BM(EP,16)+VQ(EP,3,1,NG)*BC1
          BM(EP,17)=VQ(EP,3,2,NG)*C2
          BMF(EP,17)=THK*BM(EP,17)+VQ(EP,3,2,NG)*BC2
          BMF(EP,18)=THK*BM(EP,18)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C
C---------DEF MEMBRANE------
          VDEF(EP,1)= VDEF(EP,1)+ C1*VT1
          VDEF(EP,2)= VDEF(EP,2)+ C2*VT2
C---------DEF DE FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
          VDEF(EP,6)= VDEF(EP,6)+ BC1*VT1
          VDEF(EP,7)= VDEF(EP,7)+ BC2*VT2
          VDEF(EP,8)= VDEF(EP,8)+ BC1*VT2+BC2*VT1
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
C----------RV1=<V1>*RXYZ AU CHAQUE NOEUD---------
          RV1=V1(1)*RXYZ(EP,2+1)+V1(2)*RXYZ(EP,2+2)
C----------RV2=<V2>*RXYZ AU CHAQUE NOEUD---------
          RV2=V2(1)*RXYZ(EP,2+1)+V2(2)*RXYZ(EP,2+2)
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II=6
            BF(EP,7)=V1(1)*C1
            BF(EP,8)=V2(1)*C2
            BF(EP,9)=V1(1)*C2+V2(1)*C1
            BF(EP,10)=V1(2)*C1
            BF(EP,11)=V2(2)*C2
            BF(EP,12)=V1(2)*C2+V2(2)*C1
C---------DEF DE FLEXION ASSOCIEE AUX THETA1,THETA2------
          VDEF(EP,6)= VDEF(EP,6)+C1*RV1
          VDEF(EP,7)= VDEF(EP,7)+C2*RV2
          VDEF(EP,8)= VDEF(EP,8)+C1*RV2+C2*RV1
C
C--------J=3---II=(J-1)*2 =4 KK = 3*(J-1)=6
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
         VT1=VQ(EP,1,1,NG)*VXYZ(EP,7)+VQ(EP,2,1,NG)*VXYZ(EP,8)
     1        +VQ(EP,3,1,NG)*VXYZ(EP,9)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
         VT2=VQ(EP,1,2,NG)*VXYZ(EP,7)+VQ(EP,2,2,NG)*VXYZ(EP,8)
     1        +VQ(EP,3,2,NG)*VXYZ(EP,9)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(3,NG)*TC(EP,1,1)+VETA(3,NG)*TC(EP,2,1)
         C2=VKSI(3,NG)*TC(EP,1,2)+VETA(3,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(3,NG)*TBC(1,1)+VETA(3,NG)*TBC(2,1)
         BC2=VKSI(3,NG)*TBC(1,2)+VETA(3,NG)*TBC(2,2)
C         I = 1;JJ = 3*KK=18
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
          BM(EP,21)=BXY(1)+BYX(1)
          BM(EP,24)=BXY(2)+BYX(2)
          BM(EP,27)=BXY(3)+BYX(3)
          VDEF(EP,3)= VDEF(EP,3)+ C1*VT2+C2*VT1
         ELSE
          BM(EP,27)=ZERO
         END IF 
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,19)=VQ(EP,1,1,NG)*C1
          BMF(EP,19)=THK*BM(EP,19)+VQ(EP,1,1,NG)*BC1
          BM(EP,20)=VQ(EP,1,2,NG)*C2
          BMF(EP,20)=THK*BM(EP,20)+VQ(EP,1,2,NG)*BC2
          BMF(EP,21)=THK*BM(EP,21)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C
C         I = 2; JJ = 3*(KK+1)=21
          BM(EP,22)=VQ(EP,2,1,NG)*C1
          BMF(EP,22)=THK*BM(EP,22)+VQ(EP,2,1,NG)*BC1
          BM(EP,23)=VQ(EP,2,2,NG)*C2
          BMF(EP,23)=THK*BM(EP,23)+VQ(EP,2,2,NG)*BC2
          BMF(EP,24)=THK*BM(EP,24)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C
C         I = 3; JJ = 3*(KK+2)=24
          BM(EP,25)=VQ(EP,3,1,NG)*C1
          BMF(EP,25)=THK*BM(EP,25)+VQ(EP,3,1,NG)*BC1
          BM(EP,26)=VQ(EP,3,2,NG)*C2
          BMF(EP,26)=THK*BM(EP,26)+VQ(EP,3,2,NG)*BC2
          BMF(EP,27)=THK*BM(EP,27)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C
C---------DEF MEMBRANE------
          VDEF(EP,1)= VDEF(EP,1)+ C1*VT1
          VDEF(EP,2)= VDEF(EP,2)+ C2*VT2
C---------DEF DE FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
          VDEF(EP,6)= VDEF(EP,6)+ BC1*VT1
          VDEF(EP,7)= VDEF(EP,7)+ BC2*VT2
          VDEF(EP,8)= VDEF(EP,8)+ BC1*VT2+BC2*VT1
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
C----------RV1=<V1>*RXYZ AU CHAQUE NOEUD---------
          RV1=V1(1)*RXYZ(EP,4+1)+V1(2)*RXYZ(EP,4+2)
C----------RV2=<V2>*RXYZ AU CHAQUE NOEUD---------
          RV2=V2(1)*RXYZ(EP,4+1)+V2(2)*RXYZ(EP,4+2)
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II =12
            BF(EP,13)=V1(1)*C1
            BF(EP,14)=V2(1)*C2
            BF(EP,15)=V1(1)*C2+V2(1)*C1
            BF(EP,16)=V1(2)*C1
            BF(EP,17)=V2(2)*C2
            BF(EP,18)=V1(2)*C2+V2(2)*C1
C---------DEF DE FLEXION ASSOCIEE AUX THETA1,THETA2------
          VDEF(EP,6)= VDEF(EP,6)+C1*RV1
          VDEF(EP,7)= VDEF(EP,7)+C2*RV2
          VDEF(EP,8)= VDEF(EP,8)+C1*RV2+C2*RV1
C--------J=4---II=(J-1)*2  =6 KK = 3*(J-1)=9
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
         VT1=VQ(EP,1,1,NG)*VXYZ(EP,10)+VQ(EP,2,1,NG)*VXYZ(EP,11)
     1        +VQ(EP,3,1,NG)*VXYZ(EP,12)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
         VT2=VQ(EP,1,2,NG)*VXYZ(EP,10)+VQ(EP,2,2,NG)*VXYZ(EP,11)
     1        +VQ(EP,3,2,NG)*VXYZ(EP,12)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(4,NG)*TC(EP,1,1)+VETA(4,NG)*TC(EP,2,1)
         C2=VKSI(4,NG)*TC(EP,1,2)+VETA(4,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
         BC1=VKSI(4,NG)*TBC(1,1)+VETA(4,NG)*TBC(2,1)
         BC2=VKSI(4,NG)*TBC(1,2)+VETA(4,NG)*TBC(2,2)
C         I = 1; JJ = 3*KK =27
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
          BM(EP,30)=BXY(1)+BYX(1)
          BM(EP,33)=BXY(2)+BYX(2)
          BM(EP,36)=BXY(3)+BYX(3)
          VDEF(EP,3)= VDEF(EP,3)+ C1*VT2+C2*VT1
         ELSE
          BM(EP,36)=ZERO
         END IF 
          BM(EP,28)=VQ(EP,1,1,NG)*C1
          BMF(EP,28)=THK*BM(EP,28)+VQ(EP,1,1,NG)*BC1
          BM(EP,29)=VQ(EP,1,2,NG)*C2
          BMF(EP,29)=THK*BM(EP,29)+VQ(EP,1,2,NG)*BC2
C---------CONTRIBUTION DU <T> :EPS-XY
          BMF(EP,30)=THK*BM(EP,30)+VQ(EP,1,1,NG)*BC2+VQ(EP,1,2,NG)*BC1
C
C         I = 2; JJ = 3*(KK+1)=30
          BM(EP,31)=VQ(EP,2,1,NG)*C1
          BMF(EP,31)=THK*BM(EP,31)+VQ(EP,2,1,NG)*BC1
          BM(EP,32)=VQ(EP,2,2,NG)*C2
          BMF(EP,32)=THK*BM(EP,32)+VQ(EP,2,2,NG)*BC2
          BMF(EP,33)=THK*BM(EP,33)+VQ(EP,2,1,NG)*BC2+VQ(EP,2,2,NG)*BC1
C
C         I = 3; JJ = 3*(KK+2)=33
          BM(EP,34)=VQ(EP,3,1,NG)*C1
          BMF(EP,34)=THK*BM(EP,34)+VQ(EP,3,1,NG)*BC1
          BM(EP,35)=VQ(EP,3,2,NG)*C2
          BMF(EP,35)=THK*BM(EP,35)+VQ(EP,3,2,NG)*BC2
          BMF(EP,36)=THK*BM(EP,36)+VQ(EP,3,1,NG)*BC2+VQ(EP,3,2,NG)*BC1
C
C---------DEF MEMBRANE------
          VDEF(EP,1)= VDEF(EP,1)+ C1*VT1
          VDEF(EP,2)= VDEF(EP,2)+ C2*VT2
C---------DEF DE FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------SEULEMENT LA PARTIE DE [B01]---------
          VDEF(EP,6)= VDEF(EP,6)+ BC1*VT1
          VDEF(EP,7)= VDEF(EP,7)+ BC2*VT2
          VDEF(EP,8)= VDEF(EP,8)+ BC1*VT2+BC2*VT1
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
C----------RV1=<V1>*RXYZ AU CHAQUE NOEUD---------
          RV1=V1(1)*RXYZ(EP,6+1)+V1(2)*RXYZ(EP,6+2)
C----------RV2=<V2>*RXYZ AU CHAQUE NOEUD---------
          RV2=V2(1)*RXYZ(EP,6+1)+V2(2)*RXYZ(EP,6+2)
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II=18
            BF(EP,19)=V1(1)*C1
            BF(EP,20)=V2(1)*C2
            BF(EP,21)=V1(1)*C2+V2(1)*C1
            BF(EP,22)=V1(2)*C1
            BF(EP,23)=V2(2)*C2
            BF(EP,24)=V1(2)*C2+V2(2)*C1
C---------DEF DE FLEXION ASSOCIEE AUX THETA1,THETA2------
          VDEF(EP,6)= VDEF(EP,6)+THK*VDEF(EP,1)+C1*RV1
          VDEF(EP,7)= VDEF(EP,7)+THK*VDEF(EP,2)+C2*RV2
          VDEF(EP,8)= VDEF(EP,8)+THK*VDEF(EP,3)+C1*RV2+C2*RV1
C
C--------------------------
C     DEFORMATION OUT-PLANE (C.T)
C--------------------------
C      [BC](2,5*NPG),VDEF(4,5) 
C--------------------------
C
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
          BC(EP,1)=-C1
          BC(EP,11)=C1
C
          C1=V11(1)*VNRM(EP,2)
          BC(EP,3)=-C1
          BC(EP,13)=C1
C
          C1=V11(1)*VNRM(EP,3)
          BC(EP,5)=-C1
          BC(EP,15)=C1
C--------TERMES <ASI>-------
          BC(EP,7)=V11(1)*VASTN(EP,1)
          BC(EP,9)=V11(1)*VASTN(EP,2)
          BC(EP,17)=V11(1)*VASTN(EP,3)
          BC(EP,19)=V11(1)*VASTN(EP,4)
C--- J=2---------------------
C-----------POINTEURS POUR BC(II,J1~J2)
C         J1=(KA1(1,J)-1)*5 =15
C         J2=(KA1(2,J)-1)*5 =10
C         II=KIND(J)        =1
C         JJ = 3*(J-1)     =3
C--------TERMES <NAI>-------
          C1=V11(2)*VNRM(EP,4)
          BC(EP,31)=-C1
          BC(EP,21)=C1
C
          C1=V11(2)*VNRM(EP,5)
          BC(EP,33)=-C1
          BC(EP,23)=C1
C
          C1=V11(2)*VNRM(EP,6)
          BC(EP,35)=-C1
          BC(EP,25)=C1
C--------TERMES <ASI>-------
          BC(EP,37)=V11(2)*VASTN(EP,5)
          BC(EP,39)=V11(2)*VASTN(EP,6)
          BC(EP,27)=V11(2)*VASTN(EP,7)
          BC(EP,29)=V11(2)*VASTN(EP,8)
C--- J=3---------------------
C-----------POINTEURS POUR BC(II,J1~J2)
C         J1=(KA1(1,J)-1)*5 =0
C         J2=(KA1(2,J)-1)*5 =15
C         II=KIND(J)        =2
C         JJ = 3*(J-1)     =6
C--------TERMES <NAI>-------
          C1=V11(3)*VNRM(EP,7)
          BC(EP,2)=-C1
          BC(EP,32)=C1
C
          C1=V11(3)*VNRM(EP,8)
          BC(EP,4)=-C1
          BC(EP,34)=C1
C
          C1=V11(3)*VNRM(EP,9)
          BC(EP,6)=-C1
          BC(EP,36)=C1
C--------TERMES <ASI>-------
          BC(EP,8)=V11(3)*VASTN(EP,9)
          BC(EP,10)=V11(3)*VASTN(EP,10)
          BC(EP,38)=V11(3)*VASTN(EP,11)
          BC(EP,40)=V11(3)*VASTN(EP,12)
C--- J=4---------------------
C-----------POINTEURS POUR BC(II,J1~J2)
C         J1=(KA1(1,J)-1)*5 =5
C         J2=(KA1(2,J)-1)*5 =10
C         II=KIND(J)        =2
C         JJ = 3*(J-1)     =9
C--------TERMES <NAI>-------
          C1=V11(4)*VNRM(EP,10)
          BC(EP,12)=-C1
          BC(EP,22)=C1
C
          C1=V11(4)*VNRM(EP,11)
          BC(EP,14)=-C1
          BC(EP,24)=C1
C
          C1=V11(4)*VNRM(EP,12)
          BC(EP,16)=-C1
          BC(EP,26)=C1
C--------TERMES <ASI>-------
          BC(EP,18)=V11(4)*VASTN(EP,13)
          BC(EP,20)=V11(4)*VASTN(EP,14)
          BC(EP,28)=V11(4)*VASTN(EP,15)
          BC(EP,30)=V11(4)*VASTN(EP,16)
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C----------J=1--II=(J-1)*3 =0 JJ=(J-1)*2 =0 KK=10*(J-1)=0
         BCX=
     1            BC(EP,1)*VXYZ(EP,1)+BC(EP,3)*VXYZ(EP,2)
     2           +BC(EP,5)*VXYZ(EP,3)+BC(EP,7)*RXYZ(EP,1)
     3           +BC(EP,9)*RXYZ(EP,2)       
         BCY=
     1            BC(EP,2)*VXYZ(EP,1)+BC(EP,4)*VXYZ(EP,2)
     2           +BC(EP,6)*VXYZ(EP,3)+BC(EP,8)*RXYZ(EP,1)
     3           +BC(EP,10)*RXYZ(EP,2)       
C----------J=2 II=(J-1)*3 =3 JJ=(J-1)*2 =2 KK=10*(J-1) =10
         BCX=BCX
     1           +BC(EP,11)*VXYZ(EP,4)+BC(EP,13)*VXYZ(EP,5)
     2           +BC(EP,15)*VXYZ(EP,6)+BC(EP,17)*RXYZ(EP,3)
     3           +BC(EP,19)*RXYZ(EP,4)       
         BCY=BCY
     1           +BC(EP,12)*VXYZ(EP,4)+BC(EP,14)*VXYZ(EP,5)
     2           +BC(EP,16)*VXYZ(EP,6)+BC(EP,18)*RXYZ(EP,3)
     3           +BC(EP,20)*RXYZ(EP,4)       
C----------J=3--  II=(J-1)*3=6 JJ=(J-1)*2=4  KK=10*(J-1)=20
         BCX=BCX
     1           +BC(EP,21)*VXYZ(EP,7)+BC(EP,23)*VXYZ(EP,8)
     2           +BC(EP,25)*VXYZ(EP,9)+BC(EP,27)*RXYZ(EP,5)
     3           +BC(EP,29)*RXYZ(EP,6)       
         BCY=BCY
     1           +BC(EP,22)*VXYZ(EP,7)+BC(EP,24)*VXYZ(EP,8)
     2           +BC(EP,26)*VXYZ(EP,9)+BC(EP,28)*RXYZ(EP,5)
     3           +BC(EP,30)*RXYZ(EP,6)       
C----------J=4-- II=(J-1)*3 =9 JJ=(J-1)*2 =6 KK=10*(J-1)=30
         BCX=BCX
     1           +BC(EP,31)*VXYZ(EP,10)+BC(EP,33)*VXYZ(EP,11)
     2           +BC(EP,35)*VXYZ(EP,12)+BC(EP,37)*RXYZ(EP,7)
     3           +BC(EP,39)*RXYZ(EP,8)       
         BCY=BCY
     1           +BC(EP,32)*VXYZ(EP,10)+BC(EP,34)*VXYZ(EP,11)
     2           +BC(EP,36)*VXYZ(EP,12)+BC(EP,38)*RXYZ(EP,7)
     3           +BC(EP,40)*RXYZ(EP,8)
         VDEF(EP,4)=TC(EP,1,1)*BCX+TC(EP,2,1)*BCY
         VDEF(EP,5)=TC(EP,1,2)*BCX+TC(EP,2,2)*BCY
C      
 150   CONTINUE
       RETURN
       END
Chd|====================================================================
Chd|  CBADEFSH                      source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBADEFSH(JFT  ,JLT  ,X13  ,X24  ,Y13  ,Y24  ,
     1                                 BM, VDEF, VXYZ , NPLAT, IPLAT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL 'membrane shear traitement' use only PARTIE CONSTANTE
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
       INTEGER JFT ,JLT,NPLAT,IPLAT(*)
       my_real 
     .     X13(*)   ,X24(*)    ,Y13(*)     ,Y24(*)    ,
     .     BM(MVSIZ,36),VDEF(MVSIZ,8) ,VXYZ(MVSIZ,3,4)  
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J,EP
C
       DO EP=JFT,NPLAT 
        I=IPLAT(EP)
          VDEF(I,3)=Y24(I)*VXYZ(I,2,1)-Y13(I)*VXYZ(I,2,2)
     .             -X24(I)*VXYZ(I,1,1)+X13(I)*VXYZ(I,1,2)
       ENDDO
       DO EP=NPLAT+1,JLT 
        I=IPLAT(EP)
          BM(I,3)=-X24(I)
          BM(I,6)=Y24(I)
          BM(I,12)=X13(I)
          BM(I,15)=-Y13(I)
          BM(I,21)=-BM(I,3)
          BM(I,24)=-BM(I,6)
          BM(I,30)=-BM(I,12)
          BM(I,33)=-BM(I,15)
          VDEF(I,3)=Y24(I)*(VXYZ(I,2,1)-VXYZ(I,2,3))+
     1              Y13(I)*(-VXYZ(I,2,2)+VXYZ(I,2,4))
     2             -X24(I)*(VXYZ(I,1,1)-VXYZ(I,1,3))
     3             +X13(I)*(VXYZ(I,1,2)-VXYZ(I,1,4))
       ENDDO
       RETURN
       END
Chd|====================================================================
Chd|  CBADERIRZ                     source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|        CBADERIRZ0                    source/elements/shell/coqueba/cbadef.F
Chd|====================================================================
        SUBROUTINE CBADERIRZ(JFT  ,JLT  ,AREA ,X13 ,X24   ,
     2                       Y13  ,Y24  ,BM0RZ,BMKRZ,BMERZ,
     3                       VCORE,NPLAT,IPLAT,ISMSTR)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT,IPLAT(*),ISMSTR
      MY_REAL 
     .   VCORE(MVSIZ,3,4),X13(*),X24(*),Y13(*),Y24(*),
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .   AREA(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real 
     .   MX13(MVSIZ),MX23(MVSIZ),MX34(MVSIZ),MY13(MVSIZ),
     .   MY23(MVSIZ),MY34(MVSIZ),A1(MVSIZ),
     .   X13L(MVSIZ),X24L(MVSIZ),Y13L(MVSIZ),Y24L(MVSIZ)
C-----------------------------------------------
#include "vectorize.inc"
C-------VCORE(12):BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
      DO I=JFT,NPLAT
        EP =IPLAT(I)
        MX23(I) = VCORE(EP,1,3)
        MY23(I) = VCORE(EP,2,3)
        MX34(I) = VCORE(EP,3,3)
        MY34(I) = VCORE(EP,1,4)
        MX13(I) = VCORE(EP,2,4)
        MY13(I) = VCORE(EP,3,4)
        X13L(I) = X13(EP)
        X24L(I) = X24(EP)
        Y13L(I) = Y13(EP)
        Y24L(I) = Y24(EP)
        A1(I) = AREA(EP)
      END DO !I=JFT,NPLAT
C       
      DO I=NPLAT+1,JLT
       EP =IPLAT(I)
       MX13(I) = (VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
       MY13(I) = (VCORE(EP,2,1)+VCORE(EP,2,3))*HALF
       MX23(I) = (VCORE(EP,1,2)+VCORE(EP,1,3))*HALF
       MY23(I) = (VCORE(EP,2,2)+VCORE(EP,2,3))*HALF
       MX34(I) = (VCORE(EP,1,3)+VCORE(EP,1,4))*HALF
       MY34(I) = (VCORE(EP,2,3)+VCORE(EP,2,4))*HALF
       A1(I) = AREA(EP)
       X13L(I) = X13(EP)
       X24L(I) = X24(EP)
       Y13L(I) = Y13(EP)
       Y24L(I) = Y24(EP)
      END DO !I=NPLAT+1,JLT
C      
        CALL CBADERIRZ0(JFT ,JLT  ,A1  ,X13L  ,X24L   ,
     2                 Y13L  ,Y24L  ,MX13 ,MX23 ,MX34  ,
     3                 MY13 ,MY23 ,MY34 ,BM0RZ,BMKRZ ,
     4                 BMERZ,ISMSTR)
C
      RETURN
      END
Chd|====================================================================
Chd|  CBADEFRZ                      source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBADEFRZ(JFT ,JLT  ,AREA  ,RLZ   ,VDEF ,
     1                     VXYZ ,BM0RZ,BMKRZ,BMERZ  ,VRLZ ,
     2                     BMRZ ,BRZ  ,BM   ,NPLAT  ,IPLAT,
     3                     NG   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT,IPLAT(*),NG
      my_real 
     .   RLZ(MVSIZ,4),VXYZ(MVSIZ,3,4),
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .   BMRZ(MVSIZ,3,4),BM(MVSIZ,36)
      my_real 
     .   AREA(*),VDEF(MVSIZ,8),VRLZ(*),BRZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,EP,NPG
      PARAMETER (NPG = 4)
      my_real 
     .   AA4,BXV2,BYV1,ERZ,VPG(2,NPG),PG,PG1
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C--------pay attention QEPH: Bi*A--- RLZ: VRZ/A-- QBAT:Bi--RLZ:VRZ
      DO I=JFT,JLT
       DO J=1,4
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
#include "vectorize.inc"
      DO I=JFT,JLT
C---------------
C  MEMBRANE CONSTANT
C---------------
        EP=IPLAT(I)
       VDEF(EP,1)=VDEF(EP,1)+BMRZ(I,1,1)*RLZ(I,1)+BMRZ(I,1,2)*RLZ(I,2)
     1                      +BMRZ(I,1,3)*RLZ(I,3)+BMRZ(I,1,4)*RLZ(I,4)
       VDEF(EP,2)=VDEF(EP,2)+BMRZ(I,2,1)*RLZ(I,1)+BMRZ(I,2,2)*RLZ(I,2)
     1                      +BMRZ(I,2,3)*RLZ(I,3)+BMRZ(I,2,4)*RLZ(I,4)
       VDEF(EP,3)=VDEF(EP,3)+BMRZ(I,3,1)*RLZ(I,1)+BMRZ(I,3,2)*RLZ(I,2)
     1                      +BMRZ(I,3,3)*RLZ(I,3)+BMRZ(I,3,4)*RLZ(I,4)
      ENDDO
#include "vectorize.inc"
      DO I=JFT,NPLAT
        EP=IPLAT(I)
        BXV2= BM(EP,1)*VXYZ(EP,2,1)+BM(EP,2)*VXYZ(EP,2,2)
     1       +BM(EP,3)*VXYZ(EP,2,3)
        BYV1=BM(EP,5)*VXYZ(EP,1,1)+BM(EP,6)*VXYZ(EP,1,2)
     1       +BM(EP,7)*VXYZ(EP,1,3)
        VRLZ(I)=(BXV2-BYV1+
     1           BRZ(I,4,1)*RLZ(I,1)+BRZ(I,4,2)*RLZ(I,2)
     1          +BRZ(I,4,3)*RLZ(I,3)+BRZ(I,4,4)*RLZ(I,4))*HALF
      ENDDO
C      
#include "vectorize.inc"
      DO I=NPLAT+1,JLT 
       EP=IPLAT(I)
       ERZ=ZERO
       DO J=1,4
        ERZ=ERZ+BRZ(I,1,J)*VXYZ(EP,1,J)+
     1          BRZ(I,2,J)*VXYZ(EP,2,J)+BRZ(I,3,J)*VXYZ(EP,3,J)+
     1          BRZ(I,4,J)*RLZ(I,J)
       ENDDO
        VRLZ(I)=ERZ*HALF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CBADERIRZ0                    source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBADERIRZ                     source/elements/shell/coqueba/cbadef.F
Chd|-- calls ---------------
Chd|        CBPATCH                       source/elements/shell/coqueba/cbadef.F
Chd|====================================================================
        SUBROUTINE CBADERIRZ0(JFT ,JLT  ,AREA ,X13 ,X24   ,
     2                       Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3                       MY13 ,MY23 ,MY34 ,BM0RZ,BMKRZ,
     4                       BMERZ,ISMSTR )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISMSTR
      my_real
     .   X13(*),X24(*),Y13(*),Y24(*),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*)
      my_real
     .   AREA(*),
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   AA4,BXV2,BYV1,A05,NXY,NYX,A05K(4),A05E(4),
     .   XIJ(MVSIZ,4),YIJ(MVSIZ,4),XIL(MVSIZ,4),YIL(MVSIZ,4)
C--------(14,23,32,41)/4A----------X13:X13/A-----------------------------
      DO I=JFT,JLT
       A05=HALF/AREA(I)
       XIJ(I,1) = A05*(MX13(I)-MX34(I))
       XIJ(I,2) = HALF*X13(I)+A05*(MX23(I)-MX13(I))
       XIJ(I,3) = -XIJ(I,2)
       XIJ(I,4) = -XIJ(I,1)
       YIJ(I,1) = A05*(MY13(I)-MY34(I))
       YIJ(I,2) = HALF*Y13(I)+A05*(MY23(I)-MY13(I))
       YIJ(I,3) = -YIJ(I,2)
       YIJ(I,4) = -YIJ(I,1)
C--------(12,21,34,43)/4---------------------------------------
       XIL(I,1) = A05*(MX13(I)-MX23(I))
       XIL(I,2) = -XIL(I,1)
       XIL(I,3) = A05*(MX13(I)-MX34(I))-HALF*X13(I)
       XIL(I,4) = -XIL(I,3)
       YIL(I,1) = A05*(MY13(I)-MY23(I))
       YIL(I,2) = -YIL(I,1)
       YIL(I,3) = A05*(MY13(I)-MY34(I))-HALF*Y13(I)
       YIL(I,4) = -YIL(I,3)
      ENDDO
C
      DO I=JFT,JLT
C--------NxI,x ------
C-------- MY34YIJ KSI - MY23YIL ETA
       BM0RZ(I,1,1) = (-MY34(I)*YIJ(I,1)+MY23(I)*YIL(I,1))
       BM0RZ(I,1,2) = ( MY34(I)*YIJ(I,2)+MY23(I)*YIL(I,2))
       BM0RZ(I,1,3) = ( MY34(I)*YIJ(I,3)-MY23(I)*YIL(I,3))
       BM0RZ(I,1,4) = (-MY34(I)*YIJ(I,4)-MY23(I)*YIL(I,4))
C-------- MY13YIJ KSI - 2MY34YIL
       BMKRZ(I,1,1) = (-MY13(I)*YIJ(I,1)-TWO*MY34(I)*YIL(I,1))
       BMKRZ(I,1,2) = ( MY13(I)*YIJ(I,2)-TWO*MY34(I)*YIL(I,2))
       BMKRZ(I,1,3) = ( MY13(I)*YIJ(I,3)-TWO*MY34(I)*YIL(I,3))
       BMKRZ(I,1,4) = (-MY13(I)*YIJ(I,4)-TWO*MY34(I)*YIL(I,4))
C-------- -MY13YIL ETA + 2MY23YIJ
       BMERZ(I,1,1) = ( MY13(I)*YIL(I,1)+TWO*MY23(I)*YIJ(I,1))
       BMERZ(I,1,2) = ( MY13(I)*YIL(I,2)+TWO*MY23(I)*YIJ(I,2))
       BMERZ(I,1,3) = (-MY13(I)*YIL(I,3)+TWO*MY23(I)*YIJ(I,3))
       BMERZ(I,1,4) = (-MY13(I)*YIL(I,4)+TWO*MY23(I)*YIJ(I,4))
C -------NyI,y -------------
C-------- MX34XIJ KSI - MX23XIL ETA
       BM0RZ(I,2,1) = (-MX34(I)*XIJ(I,1)+MX23(I)*XIL(I,1))
       BM0RZ(I,2,2) = ( MX34(I)*XIJ(I,2)+MX23(I)*XIL(I,2))
       BM0RZ(I,2,3) = ( MX34(I)*XIJ(I,3)-MX23(I)*XIL(I,3))
       BM0RZ(I,2,4) = (-MX34(I)*XIJ(I,4)-MX23(I)*XIL(I,4))
C-------- MX13XIJ KSI - 2MX34XIL
       BMKRZ(I,2,1) = (-MX13(I)*XIJ(I,1)-TWO*MX34(I)*XIL(I,1))
       BMKRZ(I,2,2) = ( MX13(I)*XIJ(I,2)-TWO*MX34(I)*XIL(I,2))
       BMKRZ(I,2,3) = ( MX13(I)*XIJ(I,3)-TWO*MX34(I)*XIL(I,3))
       BMKRZ(I,2,4) = (-MX13(I)*XIJ(I,4)-TWO*MX34(I)*XIL(I,4))
C-------- -MX13XIL ETA + 2MX23XIJ
       BMERZ(I,2,1) = ( MX13(I)*XIL(I,1)+TWO*MX23(I)*XIJ(I,1))
       BMERZ(I,2,2) = ( MX13(I)*XIL(I,2)+TWO*MX23(I)*XIJ(I,2))
       BMERZ(I,2,3) = (-MX13(I)*XIL(I,3)+TWO*MX23(I)*XIJ(I,3))
       BMERZ(I,2,4) = (-MX13(I)*XIL(I,4)+TWO*MX23(I)*XIJ(I,4))
C--------NxI,y
C-------- -MX34YIJ KSI + MX23YIL ETA
       BM0RZ(I,3,1) = ( MX34(I)*YIJ(I,1)-MX23(I)*YIL(I,1))
       BM0RZ(I,3,2) = (-MX34(I)*YIJ(I,2)-MX23(I)*YIL(I,2))
       BM0RZ(I,3,3) = (-MX34(I)*YIJ(I,3)+MX23(I)*YIL(I,3))
       BM0RZ(I,3,4) = ( MX34(I)*YIJ(I,4)+MX23(I)*YIL(I,4))
C-------- -MX13YIJ KSI + 2MX34YIL
       BMKRZ(I,3,1) = ( MX13(I)*YIJ(I,1)+TWO*MX34(I)*YIL(I,1))
       BMKRZ(I,3,2) = (-MX13(I)*YIJ(I,2)+TWO*MX34(I)*YIL(I,2))
       BMKRZ(I,3,3) = (-MX13(I)*YIJ(I,3)+TWO*MX34(I)*YIL(I,3))
       BMKRZ(I,3,4) = ( MX13(I)*YIJ(I,4)+TWO*MX34(I)*YIL(I,4))
C-------- MX13YIL ETA - 2MX23YIJ
       BMERZ(I,3,1) = (-MX13(I)*YIL(I,1)-TWO*MX23(I)*YIJ(I,1))
       BMERZ(I,3,2) = (-MX13(I)*YIL(I,2)-TWO*MX23(I)*YIJ(I,2))
       BMERZ(I,3,3) = ( MX13(I)*YIL(I,3)-TWO*MX23(I)*YIJ(I,3))
       BMERZ(I,3,4) = ( MX13(I)*YIL(I,4)-TWO*MX23(I)*YIJ(I,4))
C--------NyI,x
C-------- -MY34XIJ KSI + MY23XIJ ETA
       BM0RZ(I,4,1) = ( MY34(I)*XIJ(I,1)-MY23(I)*XIL(I,1))
       BM0RZ(I,4,2) = (-MY34(I)*XIJ(I,2)-MY23(I)*XIL(I,2))
       BM0RZ(I,4,3) = (-MY34(I)*XIJ(I,3)+MY23(I)*XIL(I,3))
       BM0RZ(I,4,4) = ( MY34(I)*XIJ(I,4)+MY23(I)*XIL(I,4))
C-------- -MY13XIJ KSI + 2MY34XIL
       BMKRZ(I,4,1) = ( MY13(I)*XIJ(I,1)+TWO*MY34(I)*XIL(I,1))
       BMKRZ(I,4,2) = (-MY13(I)*XIJ(I,2)+TWO*MY34(I)*XIL(I,2))
       BMKRZ(I,4,3) = (-MY13(I)*XIJ(I,3)+TWO*MY34(I)*XIL(I,3))
       BMKRZ(I,4,4) = ( MY13(I)*XIJ(I,4)+TWO*MY34(I)*XIL(I,4))
C-------- MY13XIL ETA - 2MY23XIJ
       BMERZ(I,4,1) = (-MY13(I)*XIL(I,1)-TWO*MY23(I)*XIJ(I,1))
       BMERZ(I,4,2) = (-MY13(I)*XIL(I,2)-TWO*MY23(I)*XIJ(I,2))
       BMERZ(I,4,3) = ( MY13(I)*XIL(I,3)-TWO*MY23(I)*XIJ(I,3))
       BMERZ(I,4,4) = ( MY13(I)*XIL(I,4)-TWO*MY23(I)*XIJ(I,4))
      ENDDO
C--------correction to pass the patch test
      CALL CBPATCH(JFT ,JLT  ,BM0RZ,BMKRZ,BMERZ  ,AREA ,
     +             MX13 ,MX23,MX34  ,MY13 ,MY23 ,MY34)
C
        A05= HALF
       IF (ISMSTR==10) A05=ZERO
      DO I=JFT,JLT
C--------BM0RZ(I,3,1)->(NxI,y+NyI,x) -;BM0RZ(I,4,1)->(-NxI,y+NyI,x -2NI)-----
       A05K(1)= -A05
       A05K(2)= A05
       A05K(3)= A05
       A05K(4)= -A05
       A05E(1)= -A05
       A05E(2)= -A05
       A05E(3)= A05
       A05E(4)= A05
      DO J=1,4
       NXY=BM0RZ(I,3,J)
       NYX=BM0RZ(I,4,J)
       BM0RZ(I,3,J)=NXY+NYX
       BM0RZ(I,4,J)=-NXY+NYX-A05
       NXY=BMKRZ(I,3,J)
       NYX=BMKRZ(I,4,J)
       BMKRZ(I,3,J)=NXY+NYX
       BMKRZ(I,4,J)=-NXY+NYX-A05K(J)
       NXY=BMERZ(I,3,J)
       NYX=BMERZ(I,4,J)
       BMERZ(I,3,J)=NXY+NYX
       BMERZ(I,4,J)=-NXY+NYX-A05E(J)
      ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CBPATCH                       source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBADERIRZ0                    source/elements/shell/coqueba/cbadef.F
Chd|        CZDERIRZ                      source/elements/shell/coquez/czdef.F
Chd|        CZDERIRZT                     source/elements/shell/coquez/czdef.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBPATCH(JFT  ,JLT  ,BM0RZ,BMKRZ,BMERZ  ,AREA ,
     3                     MX13 ,MX23,MX34  ,MY13 ,MY23 ,MY34)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .   MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*),AREA(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,EP,NPG,NG,IAC
      PARAMETER (NPG = 4)
      my_real
     .   BM0RZM(MVSIZ,4,4),J1,J2,J0,JAC(MVSIZ,4),DET,JK_A,JE_A
      my_real
     .   VPG(2,NPG),PG,PG1
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C-------------Correction of BM0RZ to pass Patch test- IAC: analytical compute
      IAC = 1
	  IF (IAC>0) THEN
      DO I=JFT,JLT
	   JK_A=FOUR_OVER_3*(MX23(I)*MY13(I)-MX13(I)*MY23(I))/AREA(I)
	   JE_A=FOUR_OVER_3*(MX13(I)*MY34(I)-MX34(I)*MY13(I))/AREA(I)
C--------NxI,x ------
       BM0RZ(I,1,1) = -BMKRZ(I,1,1)*JK_A-BMERZ(I,1,1)*JE_A
       BM0RZ(I,1,2) = -BMKRZ(I,1,2)*JK_A-BMERZ(I,1,2)*JE_A
       BM0RZ(I,1,3) = -BMKRZ(I,1,3)*JK_A-BMERZ(I,1,3)*JE_A
       BM0RZ(I,1,4) = -BMKRZ(I,1,4)*JK_A-BMERZ(I,1,4)*JE_A
C -------NyI,y -------------
       BM0RZ(I,2,1) = -BMKRZ(I,2,1)*JK_A-BMERZ(I,2,1)*JE_A
       BM0RZ(I,2,2) = -BMKRZ(I,2,2)*JK_A-BMERZ(I,2,2)*JE_A
       BM0RZ(I,2,3) = -BMKRZ(I,2,3)*JK_A-BMERZ(I,2,3)*JE_A
       BM0RZ(I,2,4) = -BMKRZ(I,2,4)*JK_A-BMERZ(I,2,4)*JE_A
C--------NxI,y ------
       BM0RZ(I,3,1) = -BMKRZ(I,3,1)*JK_A-BMERZ(I,3,1)*JE_A
       BM0RZ(I,3,2) = -BMKRZ(I,3,2)*JK_A-BMERZ(I,3,2)*JE_A
       BM0RZ(I,3,3) = -BMKRZ(I,3,3)*JK_A-BMERZ(I,3,3)*JE_A
       BM0RZ(I,3,4) = -BMKRZ(I,3,4)*JK_A-BMERZ(I,3,4)*JE_A
C -------NyI,y -------------
       BM0RZ(I,4,1) = -BMKRZ(I,4,1)*JK_A-BMERZ(I,4,1)*JE_A
       BM0RZ(I,4,2) = -BMKRZ(I,4,2)*JK_A-BMERZ(I,4,2)*JE_A
       BM0RZ(I,4,3) = -BMKRZ(I,4,3)*JK_A-BMERZ(I,4,3)*JE_A
       BM0RZ(I,4,4) = -BMKRZ(I,4,4)*JK_A-BMERZ(I,4,4)*JE_A
      ENDDO
      RETURN
	  END IF !(ICN>0) THEN
C	  
      DO I=JFT,JLT
        J1=(MX23(I)*MY13(I)-MX13(I)*MY23(I))*PG
        J2=-(MX13(I)*MY34(I)-MX34(I)*MY13(I))*PG
        J0=AREA(I)*FOURTH
        JAC(I,1)=J0+J2-J1
        JAC(I,2)=J0+J2+J1
        JAC(I,3)=J0-J2+J1
        JAC(I,4)=J0-J2-J1
      ENDDO
C
      DO I=JFT,JLT
       DO J=1,4
        BM0RZM(I,1,J)=ZERO
        BM0RZM(I,2,J)=ZERO
        BM0RZM(I,3,J)=ZERO
        BM0RZM(I,4,J)=ZERO
       ENDDO
      ENDDO
C
      DO NG= 1,4
      DO I=JFT,JLT
	   DET=JAC(I,NG)/AREA(I)
       DO J=1,4
        BM0RZM(I,1,J) = BM0RZM(I,1,J)+ (BMKRZ(I,1,J)*VPG(1,NG)+
     .                  BMERZ(I,1,J)*VPG(2,NG))*DET
        BM0RZM(I,2,J) = BM0RZM(I,2,J)+(BMKRZ(I,2,J)*VPG(1,NG)+
     .                  BMERZ(I,2,J)*VPG(2,NG))*DET
        BM0RZM(I,3,J) = BM0RZM(I,3,J)+(BMKRZ(I,3,J)*VPG(1,NG)+
     .                  BMERZ(I,3,J)*VPG(2,NG))*DET
        BM0RZM(I,4,J) = BM0RZM(I,4,J)+(BMKRZ(I,4,J)*VPG(1,NG)+
     .                  BMERZ(I,4,J)*VPG(2,NG))*DET
C
       ENDDO
      ENDDO
      ENDDO
C	  
      DO I=JFT,JLT
       DO J=1,4
        BM0RZ(I,1,J) = -BM0RZM(I,1,J)
        BM0RZ(I,2,J) = -BM0RZM(I,2,J)
        BM0RZ(I,3,J) = -BM0RZM(I,3,J)
        BM0RZ(I,4,J) = -BM0RZM(I,4,J)
C
       ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CBADEF1                       source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADEF1(JFT,JLT,NG,VCORE,VXYZ,VDEF,
     1                  HX,HY,BM,NPLAT,IPLAT,ISROT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL DES DEFORMATIONS GENERALISEES AUX POINTS DE GAUSS ET [B] (MEMBRANE ONLY)
C        ENTREES :  NEL,NG,VCORE,VXYZ
C        SORTIES : VDEF,BM (MEMBRANE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NG,JFT,JLT,NPLAT,IPLAT(*),ISROT
      MY_REAL 
     .   VCORE(MVSIZ,12),VXYZ(MVSIZ,12),BM(MVSIZ,36),HX(MVSIZ,4),HY(MVSIZ,4)
      MY_REAL 
     .   VDEF(MVSIZ,8)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,IUN,NG1,K
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
C-------VCORE(12):BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
C-------V13(I)->VXYZ(j,1,I),V24(I)->VXYZ(j,2,I),VHI(I)->VXYZ(j,3,I)------------
        BM(EP,1)=VCORE(EP,1)+HX(EP,NG)*VCORE(EP,5)
        BM(EP,2)=VCORE(EP,2)+HX(EP,NG)*VCORE(EP,6)
        BM(EP,3)=HX(EP,NG)*FOURTH
        BM(EP,4)=-BM(EP,3)
        BM(EP,5)=VCORE(EP,3)+HY(EP,NG)*VCORE(EP,5)
        BM(EP,6)=VCORE(EP,4)+HY(EP,NG)*VCORE(EP,6)
        BM(EP,7)=HY(EP,NG)*FOURTH
        BM(EP,8)=-BM(EP,7)
C
        VDEF(EP,1)=BM(EP,1)*VXYZ(EP,1)+BM(EP,2)*VXYZ(EP,4)
     1          +BM(EP,3)*VXYZ(EP,7)
        VDEF(EP,2)=BM(EP,5)*VXYZ(EP,2)+BM(EP,6)*VXYZ(EP,5)
     1          +BM(EP,7)*VXYZ(EP,8)
        VDEF(EP,4)=ZERO
        VDEF(EP,5)=ZERO
        VDEF(EP,6)=ZERO
        VDEF(EP,7)=ZERO
        VDEF(EP,8)=ZERO
       ENDDO
       IF (ISROT>0) THEN
#include "vectorize.inc"
        DO I=JFT,NPLAT
         EP=IPLAT(I)
C----------no more constant shear-----------
         VDEF(EP,3)=BM(EP,1)*VXYZ(EP,2)+BM(EP,2)*VXYZ(EP,5)
     1             +BM(EP,3)*VXYZ(EP,8)
     1             +BM(EP,5)*VXYZ(EP,1)+BM(EP,6)*VXYZ(EP,4)
     1             +BM(EP,7)*VXYZ(EP,7)
        END DO
       END IF !(ISROT>0) THEN
C       
       RETURN
       END
Chd|====================================================================
Chd|  CBADEFT                       source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADEFT(JFT,JLT,VXYZ,RLZ,VDEF,
     2                   BM,NPLAT,IPLAT,ISROT,BMRZ )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT,IPLAT(*),ISROT
      MY_REAL 
     .   RLZ(MVSIZ,4),VXYZ(MVSIZ,3,4),
     .   BM(MVSIZ,9,4),VDEF(MVSIZ,8)
C     .   BM(9,4,*),BMF(9,4,*),BF(6,4,*),VDEF(8,*)
      MY_REAL 
     .   BMRZ(MVSIZ,4,4)
#include      "com01_c.inc"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---VDEF: 1:8 exx eyy,exy,eyx,byx,bxx,byy,bxy
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,IUN,NG1,K
C
      MY_REAL 
     .   A_1,C11,C12,C21,C22,CDEMI
C--------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
C-------BM(8):BXI(2),HX(1),not used,BYI(2),HY(1),not used
C-------V13(I)->VXYZ(j,1,I),V24(I)->VXYZ(j,2,I),VHI(I)->VXYZ(j,3,I)------------
C
        VDEF(EP,1)=BM(EP,1,1)*VXYZ(EP,1,1)+BM(EP,2,1)*VXYZ(EP,1,2)
     1            +BM(EP,3,1)*VXYZ(EP,1,3)
        VDEF(EP,2)=BM(EP,5,1)*VXYZ(EP,2,1)+BM(EP,6,1)*VXYZ(EP,2,2)
     1            +BM(EP,7,1)*VXYZ(EP,2,3)
C--------FLEXION-----
C        VDEF(EP,6)=BM(EP,1,1)*RXYZ(EP,2,1)+BM(EP,2,1)*RXYZ(EP,2,2)
C     1            +BM(EP,3,1)*RXYZ(EP,2,3)
C        VDEF(EP,7)=-(BM(EP,5,1)*RXYZ(EP,1,1)+BM(EP,6,1)*RXYZ(EP,1,2)
C     1            +BM(EP,7,1)*RXYZ(EP,1,3))
C        VDEF(EP,5)=-(BM(EP,1,1)*RXYZ(EP,1,1)+BM(EP,2,1)*RXYZ(EP,1,2)
C     1            +BM(EP,3,1)*RXYZ(EP,1,3))
C        VDEF(EP,8)=BM(EP,5,1)*RXYZ(EP,2,1)+BM(EP,6,1)*RXYZ(EP,2,2)
C     3            +BM(EP,7,1)*RXYZ(EP,2,3)
       ENDDO
c       constant shear is removed 
#include "vectorize.inc"
        DO I=JFT,NPLAT
         EP=IPLAT(I)
C---------eyx-----------
         VDEF(EP,4)=BM(EP,1,1)*VXYZ(EP,2,1)+BM(EP,2,1)*VXYZ(EP,2,2)
     1             +BM(EP,3,1)*VXYZ(EP,2,3)
C-------- exy---     
         VDEF(EP,3)=BM(EP,5,1)*VXYZ(EP,1,1)+BM(EP,6,1)*VXYZ(EP,1,2)
     1             +BM(EP,7,1)*VXYZ(EP,1,3)
        END DO
c       
C------------ELEMENT GAUCH-------------------
#include "vectorize.inc"
       DO EP=NPLAT+1,JLT 
        I=IPLAT(EP)
C          I = 1; JJ = 3*KK =0 terms VXYZ(3,j,I) shared
          CDEMI = HALF*(BM(I,9,1)*VXYZ(I,3,1)+BM(I,9,2)*VXYZ(I,3,2)+
     +                   BM(I,9,3)*VXYZ(I,3,3)+BM(I,9,4)*VXYZ(I,3,4))
          VDEF(I,4)=BM(I,6,1)*VXYZ(I,2,1)+BM(I,6,2)*VXYZ(I,2,2)+
     1              BM(I,6,3)*VXYZ(I,2,3)+BM(I,6,4)*VXYZ(I,2,4)+CDEMI
          VDEF(I,3)=BM(I,3,1)*VXYZ(I,1,1)+BM(I,3,2)*VXYZ(I,1,2)+
     1              BM(I,3,3)*VXYZ(I,1,3)+BM(I,3,4)*VXYZ(I,1,4)+CDEMI
C---------DEF MEMBRANE------
          VDEF(I,1)= 
     +BM(I,1,1)*VXYZ(I,1,1)+BM(I,4,1)*VXYZ(I,2,1)+BM(I,7,1)*VXYZ(I,3,1)+
     +BM(I,1,2)*VXYZ(I,1,2)+BM(I,4,2)*VXYZ(I,2,2)+BM(I,7,2)*VXYZ(I,3,2)+
     +BM(I,1,3)*VXYZ(I,1,3)+BM(I,4,3)*VXYZ(I,2,3)+BM(I,7,3)*VXYZ(I,3,3)+
     +BM(I,1,4)*VXYZ(I,1,4)+BM(I,4,4)*VXYZ(I,2,4)+BM(I,7,4)*VXYZ(I,3,4)
          VDEF(I,2)= 
     +BM(I,2,1)*VXYZ(I,1,1)+BM(I,5,1)*VXYZ(I,2,1)+BM(I,8,1)*VXYZ(I,3,1)+
     +BM(I,2,2)*VXYZ(I,1,2)+BM(I,5,2)*VXYZ(I,2,2)+BM(I,8,2)*VXYZ(I,3,2)+
     +BM(I,2,3)*VXYZ(I,1,3)+BM(I,5,3)*VXYZ(I,2,3)+BM(I,8,3)*VXYZ(I,3,3)+
     +BM(I,2,4)*VXYZ(I,1,4)+BM(I,5,4)*VXYZ(I,2,4)+BM(I,8,4)*VXYZ(I,3,4)
C
C---------FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
C          VDEF(I,6)= BMF(1,1,I)*VXYZ(1,1,I)+
C     +                BMF(4,1,I)*VXYZ(2,1,I)+BMF(7,1,I)*VXYZ(3,1,I)+
C     +                BMF(1,2,I)*VXYZ(1,2,I)+
C     +                BMF(4,2,I)*VXYZ(2,2,I)+BMF(7,2,I)*VXYZ(3,2,I)+
C     +                BMF(1,3,I)*VXYZ(1,3,I)+
C     +                BMF(4,3,I)*VXYZ(2,3,I)+BMF(7,3,I)*VXYZ(3,3,I)+
C     +                BMF(1,4,I)*VXYZ(1,4,I)+
C     +                BMF(4,4,I)*VXYZ(2,4,I)+BMF(7,4,I)*VXYZ(3,4,I)
C          VDEF(I,7)= BMF(2,1,I)*VXYZ(1,1,I)+
C     +                BMF(5,1,I)*VXYZ(2,1,I)+BMF(8,1,I)*VXYZ(3,1,I)+
C     +                BMF(2,2,I)*VXYZ(1,2,I)+
C     +                BMF(5,2,I)*VXYZ(2,2,I)+BMF(8,2,I)*VXYZ(3,2,I)+
C     +                BMF(2,3,I)*VXYZ(1,3,I)+
C     +                BMF(5,3,I)*VXYZ(2,3,I)+BMF(8,3,I)*VXYZ(3,3,I)+
C     +                BMF(2,4,I)*VXYZ(1,4,I)+
C     +                BMF(5,4,I)*VXYZ(2,4,I)+BMF(8,4,I)*VXYZ(3,4,I)
C          VXYZ(3,j,I) shared
C          CDEMI = HALF*(BMF(9,1,I)*VXYZ(3,1,I)+BMF(9,2,I)*VXYZ(3,2,I)+
C     +                   BMF(9,3,I)*VXYZ(3,3,I)+BMF(9,4,I)*VXYZ(3,4,I))
C          VDEF(I,8)= BMF(3,1,I)*VXYZ(1,1,I)+BMF(3,2,I)*VXYZ(1,2,I)+
C     +               BMF(3,3,I)*VXYZ(1,3,I)+BMF(3,4,I)*VXYZ(1,4,I)+CDEMI
C          VDEF(I,5)= BMF(6,1,I)*VXYZ(2,1,I)+BMF(6,2,I)*VXYZ(2,2,I)+
C     +               BMF(6,3,I)*VXYZ(2,3,I)+BMF(6,4,I)*VXYZ(2,4,I)+CDEMI
C---------- TERMES ASSOCIEES AU BETA ----------
C----------TERMES ASSOCIEES AUX BETA DE [B01]--
C           JJ = 3*II =0
C---------DEF DE FLEXION ASSOCIEE AUX THETA1,THETA2------
C          VDEF(I,6)= VDEF(I,6)+
C     +           BF(1,1,I)*RXYZ(1,1,I)+BF(4,1,I)*RXYZ(2,1,I)+
C     +           BF(1,2,I)*RXYZ(1,2,I)+BF(4,2,I)*RXYZ(2,2,I)+
C     +           BF(1,3,I)*RXYZ(1,3,I)+BF(4,3,I)*RXYZ(2,3,I)+
C     +           BF(1,4,I)*RXYZ(1,4,I)+BF(4,4,I)*RXYZ(2,4,I)
C          VDEF(I,7)= VDEF(I,7)+
C     +           BF(2,1,I)*RXYZ(1,1,I)+BF(5,1,I)*RXYZ(2,1,I)+
C     +           BF(2,2,I)*RXYZ(1,2,I)+BF(5,2,I)*RXYZ(2,2,I)+
C     +           BF(2,3,I)*RXYZ(1,3,I)+BF(5,3,I)*RXYZ(2,3,I)+
C     +           BF(2,4,I)*RXYZ(1,4,I)+BF(5,4,I)*RXYZ(2,4,I)
C          VDEF(I,8)= VDEF(I,5+
C     +           BF(6,1,I)*RXYZ(2,1,I)+BF(6,2,I)*RXYZ(2,2,I)+
C     +           BF(6,3,I)*RXYZ(2,3,I)+BF(6,4,I)*RXYZ(2,4,I)
C          VDEF(I,5)= VDEF(I,8+
C     +           BF(3,1,I)*RXYZ(1,1,I)+BF(3,2,I)*RXYZ(1,2,I)+
C     +           BF(3,3,I)*RXYZ(1,3,I)+BF(3,4,I)*RXYZ(1,4,I)
C      
      END DO
      IF (ISROT>0) THEN
#include "vectorize.inc"
       DO I=JFT,JLT
C---------------
C  MEMBRANE CONSTANT
C---------------
        EP=IPLAT(I)
       VDEF(EP,1)=VDEF(EP,1)+BMRZ(I,1,1)*RLZ(EP,1)+BMRZ(I,1,2)*
     1      RLZ(EP,2)+BMRZ(I,1,3)*RLZ(EP,3)+BMRZ(I,1,4)*RLZ(EP,4)
       VDEF(EP,2)=VDEF(EP,2)+BMRZ(I,2,1)*RLZ(EP,1)+BMRZ(I,2,2)*
     1      RLZ(EP,2)+BMRZ(I,2,3)*RLZ(EP,3)+BMRZ(I,2,4)*RLZ(EP,4)
       VDEF(EP,3)=VDEF(EP,3)+ 
     1                BMRZ(I,3,1)*RLZ(EP,1)+BMRZ(I,3,2)*RLZ(EP,2)
     2               +BMRZ(I,3,3)*RLZ(EP,3)+BMRZ(I,3,4)*RLZ(EP,4)
       VDEF(EP,4)=VDEF(EP,4)+ 
     1                BMRZ(I,4,1)*RLZ(EP,1)+BMRZ(I,4,2)*RLZ(EP,2)
     2               +BMRZ(I,4,3)*RLZ(EP,3)+BMRZ(I,4,4)*RLZ(EP,4)
       ENDDO
      END IF !(ISROT>0) THEN
C      
       RETURN
       END
Chd|====================================================================
Chd|  CBADEFT1                      source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADEFT1(JFT,JLT,NG,VCORE,VXYZ,VDEF,
     1                    HX,HY,BM,NPLAT,IPLAT,ISROT,
     2                    BMRZ,RXYZ,WXY )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NG,JFT,JLT,NPLAT,IPLAT(*),ISROT
      MY_REAL 
     .   VCORE(MVSIZ,12),VXYZ(MVSIZ,3,4),BM(MVSIZ,9,4),HX(MVSIZ,4),HY(MVSIZ,4)
      MY_REAL 
     .   VDEF(MVSIZ,8),RXYZ(MVSIZ,4),BMRZ(MVSIZ,4,4),WXY(*)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---VDEF: 1:8 exx eyy,exy,eyx,byx,bxx,byy,bxy
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,IUN,NG1,K
C
      MY_REAL 
     .   A_1,C11,C12,C21,C22,CDEMI
C--------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
       DO EP=JFT,JLT 
C--------FLEXION-----
        VDEF(EP,6)=ZERO
        VDEF(EP,7)=ZERO
        VDEF(EP,8)=ZERO
        VDEF(EP,5)=ZERO
        WXY(EP)=ZERO
       ENDDO
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
C-------BM(8):BXI(2),HX(1),not used,BYI(2),HY(1),not used
        BM(EP,1,1)=VCORE(EP,1)+HX(EP,NG)*VCORE(EP,5)
        BM(EP,2,1)=VCORE(EP,2)+HX(EP,NG)*VCORE(EP,6)
        BM(EP,3,1)=HX(EP,NG)*FOURTH
        BM(EP,4,1)=-BM(EP,3,1)
        BM(EP,5,1)=VCORE(EP,3)+HY(EP,NG)*VCORE(EP,5)
        BM(EP,6,1)=VCORE(EP,4)+HY(EP,NG)*VCORE(EP,6)
        BM(EP,7,1)=HY(EP,NG)*FOURTH
        BM(EP,8,1)=-BM(EP,7,1)
C-------V13(I)->VXYZ(j,1,I),V24(I)->VXYZ(j,2,I),VHI(I)->VXYZ(j,3,I)------------
C
        VDEF(EP,1)=BM(EP,1,1)*VXYZ(EP,1,1)+BM(EP,2,1)*VXYZ(EP,1,2)
     1            +BM(EP,3,1)*VXYZ(EP,1,3)
        VDEF(EP,2)=BM(EP,5,1)*VXYZ(EP,2,1)+BM(EP,6,1)*VXYZ(EP,2,2)
     1            +BM(EP,7,1)*VXYZ(EP,2,3)
C---------eyx-----------
         VDEF(EP,4)=BM(EP,1,1)*VXYZ(EP,2,1)+BM(EP,2,1)*VXYZ(EP,2,2)
     1             +BM(EP,3,1)*VXYZ(EP,2,3)
C-------- exy---     
         VDEF(EP,3)=BM(EP,5,1)*VXYZ(EP,1,1)+BM(EP,6,1)*VXYZ(EP,1,2)
     1             +BM(EP,7,1)*VXYZ(EP,1,3)
       ENDDO
      IF (ISROT>0) THEN
#include "vectorize.inc"
       DO I=JFT,NPLAT
C---------------
C  MEMBRANE CONSTANT
C---------------
       EP=IPLAT(I)
       VDEF(EP,1)=VDEF(EP,1)+BMRZ(I,1,1)*RXYZ(EP,1)+BMRZ(I,1,2)*
     1         RXYZ(EP,2)+BMRZ(I,1,3)*RXYZ(EP,3)+BMRZ(I,1,4)*RXYZ(EP,4)
       VDEF(EP,2)=VDEF(EP,2)+BMRZ(I,2,1)*RXYZ(EP,1)+BMRZ(I,2,2)*
     1         RXYZ(EP,2)+BMRZ(I,2,3)*RXYZ(EP,3)+BMRZ(I,2,4)*RXYZ(EP,4)
       VDEF(EP,3)=VDEF(EP,3)+ 
     1                BMRZ(I,3,1)*RXYZ(EP,1)+BMRZ(I,3,2)*RXYZ(EP,2)
     2               +BMRZ(I,3,3)*RXYZ(EP,3)+BMRZ(I,3,4)*RXYZ(EP,4)
       VDEF(EP,4)=VDEF(EP,4)+ 
     1                BMRZ(I,4,1)*RXYZ(EP,1)+BMRZ(I,4,2)*RXYZ(EP,2)
     2               +BMRZ(I,4,3)*RXYZ(EP,3)+BMRZ(I,4,4)*RXYZ(EP,4)
       ENDDO
      END IF !(ISROT>0) THEN
C      
       RETURN
       END
Chd|====================================================================
Chd|  CBADERIT1                     source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADERIT1(JFT,JLT,NG,VCORE,VQ,VJFI,
     2                  HX,HY,VETA,VKSI,BM,NPLAT,IPLAT,
     3                  ISROT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL DES DEFORMATIONS GENERALISEES AUX POINTS DE GAUSS ET [B]
C        ENTREES :  NEL,NG,VCORE,VQN,VXYZ,RXYZ,VNRM,VASM,VASTN
C        SORTIES : VDEF,BM (MEMBRANE),BMF(COUPLAGE MEM-FLEXION),BF (FLEXION),BC(C.T)
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
     .   VCORE(MVSIZ,12),VKSI(4,4),VETA(4,4),
     .   BM(MVSIZ,36),HX(MVSIZ,4),HY(MVSIZ,4)
      MY_REAL 
     .   VJFI(MVSIZ,3,2,4),VQ(MVSIZ,3,3,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,IUN,NG1,K
      MY_REAL 
     .   VPG(2,NPG),PG1,PG,THK,DETJ,DET,
     .   TFN(3,2),BCX,BCY,BXY(3),BYX(3),
     .   V1(2),V2(2),RV1,RV2,
     .   C1,C2,VT1,VT2,BC1,BC2,VB1,TC(MVSIZ,2,2),
     .   VJF1(2,3),VJF(3,3),TBI(2,2),TBC(2,2),V11(4)
      MY_REAL 
     .   A_1,C11,C12,C21,C22,CC,BETA1,KSI1,KSIY1,BETA2,KSI2,KSIY2
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA IUN/1/
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
C-------VCORE(12):BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
        BM(EP,1)=VCORE(EP,1)+HX(EP,NG)*VCORE(EP,5)
        BM(EP,2)=VCORE(EP,2)+HX(EP,NG)*VCORE(EP,6)
        BM(EP,3)=HX(EP,NG)*FOURTH
        BM(EP,4)=-BM(EP,3)
        BM(EP,5)=VCORE(EP,3)+HY(EP,NG)*VCORE(EP,5)
        BM(EP,6)=VCORE(EP,4)+HY(EP,NG)*VCORE(EP,6)
        BM(EP,7)=HY(EP,NG)*FOURTH
        BM(EP,8)=-BM(EP,7)
       ENDDO
C------------ELEMENT GAUCH-------------------
#include "vectorize.inc"
       DO 150 I=NPLAT+1,JLT 
        EP=IPLAT(I)
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
C--------------------------
C     CONSTRUIRE LA MATRICE [B] ET DEFORMATION
C--------------------------
C--------------------------
C     DEFORMATION IN-PLANE
C--------------------------
C      [BM](3,3*NPG),[BMF](3,3*NPG),
C--------------------------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(1,NG)*TC(EP,1,1)+VETA(1,NG)*TC(EP,2,1)
         C2=VKSI(1,NG)*TC(EP,1,2)+VETA(1,NG)*TC(EP,2,2)
C          I = 1; JJ = 3*KK =0
c         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BM(EP,3)=BXY(1)+BYX(1)
          BM(EP,6)=BXY(2)+BYX(2)
          BM(EP,9)=BXY(3)+BYX(3)
C---------CONTRIBUTION DU <T1> :EPS-X
          BM(EP,1)=VQ(EP,1,1,NG)*C1
C---------CONTRIBUTION DU <T2> :EPS-Y
          BM(EP,2)=VQ(EP,1,2,NG)*C2
C---------:EPS-X :EPS-Y :EPS-XY
          BM(EP,4)=VQ(EP,2,1,NG)*C1
          BM(EP,5)=VQ(EP,2,2,NG)*C2
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,7)=VQ(EP,3,1,NG)*C1
          BM(EP,8)=VQ(EP,3,2,NG)*C2
C
C--------J=2---II=(J-1)*2 =2  KK = 3*(J-1) =3
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(2,NG)*TC(EP,1,1)+VETA(2,NG)*TC(EP,2,1)
         C2=VKSI(2,NG)*TC(EP,1,2)+VETA(2,NG)*TC(EP,2,2)
C         I = 1; JJ = 3*KK =9
c         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BM(EP,12)=BXY(1)+BYX(1)
          BM(EP,15)=BXY(2)+BYX(2)
          BM(EP,18)=BXY(3)+BYX(3)
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,10)=VQ(EP,1,1,NG)*C1
          BM(EP,11)=VQ(EP,1,2,NG)*C2
C         I = 2; JJ = 3*(KK+1)=12
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,13)=VQ(EP,2,1,NG)*C1
          BM(EP,14)=VQ(EP,2,2,NG)*C2
C         I = 3; JJ = 3*(KK+2)=15
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,16)=VQ(EP,3,1,NG)*C1
          BM(EP,17)=VQ(EP,3,2,NG)*C2
C---------DEF DE FLEXION ASSOCIEE AUX THETA1,THETA2------
C--------J=3---II=(J-1)*2 =4 KK = 3*(J-1)=6
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(3,NG)*TC(EP,1,1)+VETA(3,NG)*TC(EP,2,1)
         C2=VKSI(3,NG)*TC(EP,1,2)+VETA(3,NG)*TC(EP,2,2)
c         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BM(EP,21)=BXY(1)+BYX(1)
          BM(EP,24)=BXY(2)+BYX(2)
          BM(EP,27)=BXY(3)+BYX(3)
C---------:EPS-X:EPS-Y:EPS-XY
          BM(EP,19)=VQ(EP,1,1,NG)*C1
          BM(EP,20)=VQ(EP,1,2,NG)*C2
C         I = 2; JJ = 3*(KK+1)=21
          BM(EP,22)=VQ(EP,2,1,NG)*C1
          BM(EP,23)=VQ(EP,2,2,NG)*C2
C         I = 3; JJ = 3*(KK+2)=24
          BM(EP,25)=VQ(EP,3,1,NG)*C1
          BM(EP,26)=VQ(EP,3,2,NG)*C2
C---------- TERMES ASSOCIEES AU BETA ----------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(4,NG)*TC(EP,1,1)+VETA(4,NG)*TC(EP,2,1)
         C2=VKSI(4,NG)*TC(EP,1,2)+VETA(4,NG)*TC(EP,2,2)
c         IF (ISROT>0) THEN
          BXY(1)=VQ(EP,1,1,NG)*C2
          BXY(2)=VQ(EP,2,1,NG)*C2
          BXY(3)=VQ(EP,3,1,NG)*C2
          BYX(1)=VQ(EP,1,2,NG)*C1
          BYX(2)=VQ(EP,2,2,NG)*C1
          BYX(3)=VQ(EP,3,2,NG)*C1
          BM(EP,30)=BXY(1)+BYX(1)
          BM(EP,33)=BXY(2)+BYX(2)
          BM(EP,36)=BXY(3)+BYX(3)
          BM(EP,28)=VQ(EP,1,1,NG)*C1
          BM(EP,29)=VQ(EP,1,2,NG)*C2
C         I = 2; JJ = 3*(KK+1)=30
          BM(EP,31)=VQ(EP,2,1,NG)*C1
          BM(EP,32)=VQ(EP,2,2,NG)*C2
C         I = 3; JJ = 3*(KK+2)=33
          BM(EP,34)=VQ(EP,3,1,NG)*C1
          BM(EP,35)=VQ(EP,3,2,NG)*C2
C
 150   CONTINUE
       RETURN
       END
Chd|====================================================================
Chd|  CBADERIRZT                    source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADERIRZT(JFT,JLT,NG,BM0RZ,BMKRZ,BMERZ,BMRZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL DES DEFORMATIONS GENERALISEES AUX POINTS DE GAUSS ET [B]
C        ENTREES :  JFT,JLT,NG,BM0RZ,BMKRZ,BMERZ
C        SORTIES : BMRZ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NG,JFT,JLT
      MY_REAL 
     .   BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .   BMRZ(MVSIZ,4,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER NPG,I,J
      PARAMETER (NPG = 4)
      MY_REAL 
     .   VPG(2,NPG),PG1,PG,NXYP,NYXN
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---+BMRZ(3,J,I): NXY ,BMRZ(4,J,I):NYX
      DO I=JFT,JLT
       DO J=1,4
        BMRZ(I,1,J) = BM0RZ(I,1,J)+BMKRZ(I,1,J)*VPG(1,NG)+
     .                BMERZ(I,1,J)*VPG(2,NG)
        BMRZ(I,2,J) = BM0RZ(I,2,J)+BMKRZ(I,2,J)*VPG(1,NG)+
     .                BMERZ(I,2,J)*VPG(2,NG)
        NXYP        = BM0RZ(I,3,J)+BMKRZ(I,3,J)*VPG(1,NG)+
     .                BMERZ(I,3,J)*VPG(2,NG)
        NYXN        = BM0RZ(I,4,J)+BMKRZ(I,4,J)*VPG(1,NG)+
     .                BMERZ(I,4,J)*VPG(2,NG)
        BMRZ(I,4,J) = HALF*(NXYP + NYXN)
        BMRZ(I,3,J) = HALF*(NXYP - NYXN)
       ENDDO
      ENDDO
       RETURN
       END
Chd|====================================================================
Chd|  CBADEFTW                      source/elements/shell/coqueba/cbadef.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADEFTW(JFT,JLT,VXYZ,RXYZ,
     2                    BM,BMF,BF,NPLAT,IPLAT,
     3                    WXY )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT,IPLAT(*)
      MY_REAL 
     .   RXYZ(MVSIZ,2,4),VXYZ(MVSIZ,3,4),
     .   BM(MVSIZ,9,4),BMF(MVSIZ,9,4),BF(MVSIZ,6,4),WXY(*)
#include      "com01_c.inc"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---VDEF: 1:8 exx eyy,exy,eyx,byx,bxx,byy,bxy
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,EP,K
C
      MY_REAL 
     .   A_1,C11,C12,C21,C22,CDEMI,KXY,KYX
C--------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
        KYX =-(BM(EP,1,1)*RXYZ(EP,1,1)+BM(EP,2,1)*RXYZ(EP,1,2)
     1            +BM(EP,3,1)*RXYZ(EP,1,3))
        KXY =BM(EP,5,1)*RXYZ(EP,2,1)+BM(EP,6,1)*RXYZ(EP,2,2)
     3            +BM(EP,7,1)*RXYZ(EP,2,3)
        WXY(EP) = KXY - KYX
       ENDDO
C------------ELEMENT GAUCH-------------------
#include "vectorize.inc"
      DO I=NPLAT+1,JLT 
        EP=IPLAT(I)
C          
        CDEMI =HALF*(BMF(EP,9,1)*VXYZ(EP,3,1)+BMF(I,9,2)*VXYZ(EP,3,2)+
     +                 BMF(EP,9,3)*VXYZ(EP,3,3)+BMF(I,9,4)*VXYZ(EP,3,4))
          KXY = BMF(EP,3,1)*VXYZ(EP,1,1)+BMF(EP,3,2)*VXYZ(EP,1,2)+
     +          BMF(EP,3,3)*VXYZ(EP,1,3)+BMF(EP,3,4)*VXYZ(EP,1,4)+CDEMI
          KYX = BMF(EP,6,1)*VXYZ(EP,2,1)+BMF(EP,6,2)*VXYZ(EP,2,2)+
     +          BMF(EP,6,3)*VXYZ(EP,2,3)+BMF(EP,6,4)*VXYZ(EP,2,4)+CDEMI
          KXY = KXY +
     +          BF(EP,6,1)*RXYZ(EP,2,1)+BF(EP,6,2)*RXYZ(EP,2,2)+
     +          BF(EP,6,3)*RXYZ(EP,2,3)+BF(EP,6,4)*RXYZ(EP,2,4)
          KYX = KYX +
     +          BF(EP,3,1)*RXYZ(EP,1,1)+BF(EP,3,2)*RXYZ(EP,1,2)+
     +          BF(EP,3,3)*RXYZ(EP,1,3)+BF(EP,3,4)*RXYZ(EP,1,4)
C      
        WXY(EP) = KXY - KYX
      END DO
C      
       RETURN
       END
      
