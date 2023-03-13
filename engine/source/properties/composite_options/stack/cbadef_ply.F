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
C|============================================================
C|  CBALYDEF                          /coqueba/cbalydef.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|         CBAFORC3                         /coqueba/cbaforc3.F
C|============================================================
C|---
Chd|====================================================================
Chd|  CBADEF_PLY                    source/properties/composite_options/stack/cbadef_ply.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADEF_PLY(JFT,JLT,NG, NPT,NPLAT,IPLAT,VQ,
     1                  VXYZ,VETA,VKSI,BM,BC,TC,DT1C,
     2                  EXX, EYY , EYZ, EZX)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C       CALCUL DES DEFORMATIONS GENERALISEES AUX POINTS DE GAUSS ET [B]
C       ENTREES :  NEL,NG,VQN,VXYZ,VASM,VASTN
C       SORTIES : VDEF,BM (MEMBRANE),BMF(COUPLAGE MEM-FLEXION),BF (FLEXION),BC(C.T)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NG,JFT,JLT,NPLAT,IPLAT(*),NPT
C      PARAMETER (NPG = 4)
      MY_REAL 
     .   VXYZ(MVSIZ,12,NPT),VKSI(4,4),VETA(4,4),
     .   BM(MVSIZ,36),BC(MVSIZ,40)
      MY_REAL 
     .   VQ(MVSIZ,3,3,4),DT1C(*),
     .   EXX(MVSIZ,NPT), EYY(MVSIZ,NPT) , EYZ(MVSIZ,NPT), 
     .   EZX(MVSIZ,NPT)
      MY_REAL 
     .   TC(MVSIZ,2,2)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER PT,PT00,PT0,I,J,EP,IUN,NG1,K,NPLAT0
      MY_REAL 
     .   THK,DETJ,DET,
     .   TFN(3,2),BCX,BCY,
     .   V1(2),V2(2),RV1,RV2,
     .   C1,C2,VT1,VT2,BC1,BC2,VB1,
     .   VJF1(2,3),VJF(3,3),TBI(2,2),TBC(2,2)
      MY_REAL 
     .   A_1,C11,C12,C21,C22,CC,BETA1,KSI1,KSIY1,BETA2,KSI2,KSIY2
c       PARAMETER (PG=.577350269189626)
c       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA IUN/1/
C      
        NPLAT0 = NPLAT
       DO J= 1, NPT
#include "vectorize.inc"
        DO I=JFT,NPLAT0 
          EP=IPLAT(I)
C
          EXX(EP,J) = 
     .        BM(EP,1)*VXYZ(EP,1,J) + BM(EP,2)*VXYZ(EP,4 , J)
     .                              + BM(EP,3)*VXYZ(EP,7 , J)
          EYY(EP,J)=  
     .        BM(EP,5)*VXYZ(EP,2,J) + BM(EP,6)*VXYZ(EP,5 , J)
     .                              + BM(EP,7)*VXYZ(EP,8 , J)
C
!!        EYZ(EP,J)= 
          EZX(EP,J)= 
     .        BC(EP,1)*VXYZ(EP,3,J) + BC(EP,7 )*VXYZ(EP,6 , J)
     .                              + BC(EP,13)*VXYZ(EP,9 , J)
!!        EZX(EP,J)= 
          EYZ(EP,J)= 
     .        BC(EP,2)*VXYZ(EP,3,J) + BC(EP,8 )*VXYZ(EP,6 , J)
     .                              + BC(EP,14)*VXYZ(EP,9 , J)
          EXX(EP,J) = EXX(EP,J)*DT1C(EP)
          EYY(EP,J) = EYY(EP,J)*DT1C(EP)
          EYZ(EP,J) = EYZ(EP,J)*DT1C(EP)
          EZX(EP,J) = EZX(EP,J)*DT1C(EP)
       ENDDO 
#include "vectorize.inc"
       DO 150 I=NPLAT0+1,JLT 
        EP=IPLAT(I)
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
         VT1=  
     .    VQ(EP,1,1,NG)*VXYZ(EP,1, J) + VQ(EP,2,1,NG)*VXYZ(EP,2, J)
     .                                + VQ(EP,3,1,NG)*VXYZ(EP,3, J)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
         VT2=
     .     VQ(EP,1,2,NG)*VXYZ(EP,1, J) +  VQ(EP,2,2,NG)*VXYZ(EP,2, J)
     .                                  + VQ(EP,3,2,NG)*VXYZ(EP,3, J)
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(1,NG)*TC(EP,1,1) + VETA(1,NG)*TC(EP,2,1)
         C2=VKSI(1,NG)*TC(EP,1,2) + VETA(1,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
c        BC1=VKSI(1,NG)*TBC(1,1)+VETA(1,NG)*TBC(2,1)
c        BC2=VKSI(1,NG)*TBC(1,2)+VETA(1,NG)*TBC(2,2)

C---------DEF MEMBRANE------
          EXX(EP ,J)= C1*VT1
          EYY(EP ,J)= C2*VT2
C
C---------FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
c         VDEF(6,EP)= BC1*VT1
c         VDEF(7,EP)= BC2*VT2
c         VDEF(8,EP)= BC1*VT2+BC2*VT1
C--------J=2---II=(J-1)*2 =2  KK = 3*(J-1) =3
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
          VT1 = VQ(EP,1,1,NG)*VXYZ(EP,4,J)+VQ(EP,2,1,NG)*VXYZ(EP,5,J)
     1                                    +VQ(EP,3,1,NG)*VXYZ(EP,6,J)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
          VT2 =  VQ(EP,1,2,NG)*VXYZ(EP,4,J)+VQ(EP,2,2,NG)*VXYZ(EP,5,J)
     1                                     +VQ(EP,3,2,NG)*VXYZ(EP,6,J)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(2,NG)*TC(EP,1,1)+VETA(2,NG)*TC(EP,2,1)
         C2=VKSI(2,NG)*TC(EP,1,2)+VETA(2,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
c        BC1=VKSI(2,NG)*TBC(1,1)+VETA(2,NG)*TBC(2,1)
c        BC2=VKSI(2,NG)*TBC(1,2)+VETA(2,NG)*TBC(2,2)
C
C         I = 1; JJ = 3*KK =9
C---------DEF MEMBRANE------
          EXX(EP,J) = EXX(EP,J) + C1*VT1
          EYY(EP,J) = EYY(EP,J) + C2*VT2
C---------DEF DE FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
c         VDEF(6,EP)= VDEF(6,EP)+ BC1*VT1
cc        VDEF(7,EP)= VDEF(7,EP)+ BC2*VT2
c         VDEF(8,EP)= VDEF(8,EP)+ BC1*VT2+BC2*VT1
C---------- TERMES ASSOCIEES AU BETA ----------
C
C--------J=3---II=(J-1)*2 =4 KK = 3*(J-1)=6
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
         VT1=
     .     VQ(EP,1,1,NG)*VXYZ(EP,7,J)   + VQ(EP,2,1,NG)*VXYZ(EP,8,J)
     1                                  + VQ(EP,3,1,NG)*VXYZ(EP,9,J)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
         VT2=
     .    VQ(EP,1,2,NG)*VXYZ(EP,7,J)  + VQ(EP,2,2,NG)*VXYZ(EP,8,J)
     1                                + VQ(EP,3,2,NG)*VXYZ(EP,9,J)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
         C1=VKSI(3,NG)*TC(EP,1,1)+VETA(3,NG)*TC(EP,2,1)
         C2=VKSI(3,NG)*TC(EP,1,2)+VETA(3,NG)*TC(EP,2,2)
C--------TERMES ASSOCIEES AUX U,V,W DE [B1]=<T>BCI-------
c        BC1=VKSI(3,NG)*TBC(1,1)+VETA(3,NG)*TBC(2,1)
c        BC2=VKSI(3,NG)*TBC(1,2)+VETA(3,NG)*TBC(2,2)
C         I = 1;JJ = 3*KK=18
C---------:EPS-X:EPS-Y:EPS-XY
C---------DEF MEMBRANE------
          EXX(EP,J) = EXX(EP,J) + C1*VT1
          EYY(EP,J) = EYY(EP,J) + C2*VT2
C
C---------DEF DE FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------[B01]---------
c         VDEF(6,EP)= VDEF(6,EP)+ BC1*VT1
c         VDEF(7,EP)= VDEF(7,EP)+ BC2*VT2
c         VDEF(8,EP)= VDEF(8,EP)+ BC1*VT2+BC2*VT1
C---------- TERMES ASSOCIEES AU BETA ----------
C---------PRODUIT SCALAIRE <T(PG)>*<-T^2 T^1>----------
C----------VT1=<T1>*VXYZ AU CHAQUE NOEUD---------
           VT1=
     .     VQ(EP,1,1,NG)*VXYZ(EP,10,J) + VQ(EP,2,1,NG)*VXYZ(EP,11,J)
     1                                 + VQ(EP,3,1,NG)*VXYZ(EP,12,J)
C----------VT2=<T2>*VXYZ AU CHAQUE NOEUD---------
           VT2=
     .     VQ(EP,1,2,NG)*VXYZ(EP,10,J) + VQ(EP,2,2,NG)*VXYZ(EP,11,J)
     1                                 + VQ(EP,3,2,NG)*VXYZ(EP,12,J)
C--------- [B0], [B01]---------
C--------TERMES DE [B0]=<T>CI-------
           C1=VKSI(4,NG)*TC(EP,1,1)+VETA(4,NG)*TC(EP,2,1)
           C2=VKSI(4,NG)*TC(EP,1,2)+VETA(4,NG)*TC(EP,2,2)
C
C---------DEF MEMBRANE------
            EXX(EP,J)= EXX(EP,J)+ C1*VT1
            EYY(EP,J)= EYY(EP,J)+ C2*VT2
C---------DEF DE FLEXION ASSOCIEE AUX U,V,W ([BMF]=2H[B0]+[B01])------
C----------SEULEMENT LA PARTIE DE [B01]---------
c         VDEF(6,EP)= VDEF(6,EP)+ BC1*VT1
c         VDEF(7,EP)= VDEF(7,EP)+ BC2*VT2
c         VDEF(8,EP)= VDEF(8,EP)+ BC1*VT2+BC2*VT1
C---------- TERMES ASSOCIEES AU BETA ----------
C---------PRODUIT SCALAIRE <T(PG)>*<-T^2 T^1>----------
C--------------------------
C     DEFORMATION OUT-PLANE (C.T)
C--------------------------
C      [BC](2,5*NPG),VDEF(4,5) 
C--------------------------
C
           BCX=  BC(EP,1)*VXYZ(EP,1,J) + BC(EP,3)*VXYZ(EP,2,J)
     1                                 + BC(EP,5)*VXYZ(EP,3,J) 
           BCY=  BC(EP,2)*VXYZ(EP,1,J) + BC(EP,4)*VXYZ(EP,2,J)
     1                                 + BC(EP,6)*VXYZ(EP,3,J)
C-------  ---J=2 II=(J-1)*3 =3 JJ=(J-1)*2 =2 KK=10*(J-1) =10
           BCX=BCX
     1          + BC(EP,11)*VXYZ(EP,4,J) + BC(EP,13)*VXYZ(EP,5,J)
     2          + BC(EP,15)*VXYZ(EP,6,J)      
           BCY=BCY
     1          + BC(EP,12)*VXYZ(EP,4,J) + BC(EP,14)*VXYZ(EP,5,J)
     2          + BC(EP,16)*VXYZ(EP,6,J)      
C-------  ---J=3--  II=(J-1)*3=6 JJ=(J-1)*2=4  KK=10*(J-1)=20
           BCX=BCX
     1          + BC(EP,21)*VXYZ(EP,7,J) + BC(EP,23)*VXYZ(EP,8,J)
     2          + BC(EP,25)*VXYZ(EP,9,J)     
           BCY=BCY
     1          +BC(EP,22)*VXYZ(EP,7,J)  + BC(EP,24)*VXYZ(EP,8,J)
     2          +BC(EP,26)*VXYZ(EP,9,J)      
C-------  ---J=4-- II=(J-1)*3 =9 JJ=(J-1)*2 =6 KK=10*(J-1)=30
           BCX=BCX
     1          +BC(EP,31)*VXYZ(EP,10,J) + BC(EP,33)*VXYZ(EP,11,J)
     2          +BC(EP,35)*VXYZ(EP,12,J)      
           BCY=BCY
     1          +BC(EP,32)*VXYZ(EP,10,J) + BC(EP,34)*VXYZ(EP,11,J)
     2          +BC(EP,36)*VXYZ(EP,12,J)
      
          EZX(EP,J)= TC(EP,1,1)*BCX + TC(EP,2,1)*BCY
          EYZ(EP,J)= TC(EP,1,2)*BCX + TC(EP,2,2)*BCY
C         
          EXX(EP,J) = EXX(EP,J)*DT1C(EP)
          EYY(EP,J) = EYY(EP,J)*DT1C(EP)
          EYZ(EP,J) = EYZ(EP,J)*DT1C(EP)
          EZX(EP,J) = EZX(EP,J)*DT1C(EP)
C
 150   CONTINUE
      ENDDO
       RETURN
       END
C
C ----nouvelle routine ---
Chd|====================================================================
Chd|  CBADEFSH_PLY                  source/properties/composite_options/stack/cbadef_ply.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBADEFSH_PLY(JFT,JLT,NPT ,NPLAT, IPLAT,X13 ,X24 ,
     .                        Y13 ,Y24, VXYZ,DT1C, EXY )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL 'membrane shear traitement' use only PARTIE CONSTANTE
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
       INTEGER JFT ,JLT,NPLAT,IPLAT(*),NPT
       my_real 
     .     X13(*)   ,X24(*)    ,Y13(*)     ,Y24(*)    ,
     .     EXY(MVSIZ,NPT) ,VXYZ(MVSIZ,12,NPT),DT1C(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J,EP,NPLAT0
C
!!       NPLAT0= JLT 
       NPLAT0= NPLAT
       DO J=1,NPT
        DO EP=JFT,NPLAT0
          I=IPLAT(EP)
          EXY(I,J)= Y24(I)*VXYZ(I,2,J) - Y13(I)*VXYZ(I,5,J)
     .             -X24(I)*VXYZ(I,1,J) + X13(I)*VXYZ(I,4,J)
          EXY(I,J) = EXY(I,J)*DT1C(I)
        ENDDO
        DO EP=NPLAT0+1,JLT 
         I=IPLAT(EP)
          EXY(I,J) =  Y24(I)*( VXYZ(I,2,J)  - VXYZ(I,8  ,J))
     1              + Y13(I)*(-VXYZ(I,5,J)  + VXYZ(I,11 ,J))
     2              - X24(I)*( VXYZ(I,1,J)  - VXYZ(I,7  ,J))
     3              + X13(I)*( VXYZ(I,4,J)  - VXYZ(I,10 ,J))
          EXY(I,J) = EXY(I,J)*DT1C(I)
        ENDDO
       ENDDO
       RETURN
       END

