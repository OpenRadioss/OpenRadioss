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
	SUBROUTINE CBAFORI(JFT  ,JLT   ,NG    ,CDET ,THK0,
     2                     TH12 ,FF0   ,MM0   ,NEL  ,BM  ,
     3                     BMF  ,BF    ,BC    ,TC   ,VF  ,
     4                     VM   ,NPLAT ,IPLAT ,VOL  ) 
C--------------------------------------------------------------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C	CALCUL DES FORCES INTERNES AUX NOEUDS 
C	ENTREES :  NEL,MNT,NPT,NP,NG,NIXC,IXC,HZ,OFF,CDET,EPAIS,BUFCON,VQN,BM,BMF,BF,BC
C	SORTIES : VF,VM
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NG,NPLAT,IPLAT(*),NEL 
      MY_REAL 
     .   FF0(NEL,5),MM0(NEL,3),CDET(*),VOL(*), 
     .   BM(MVSIZ,36),BMF(MVSIZ,36),BF(MVSIZ,24),BC(MVSIZ,40),
     .   THK0(*),VF(MVSIZ,3,4),VM(MVSIZ,2,4),TH12(*),
     .    C1,C2,CM1,CM2,CC1,CC2,BC1,BC3,BC5,
     .    TC(MVSIZ,2,2),BCX,BCY
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      MY_REAL 
     .   FF(MVSIZ,5),MM(MVSIZ,3) 
C--------------------------------------------------------------------------------------------------
C---due to bug of convention-----
       DO EP=JFT,JLT 
        FF(EP,1) = FF0(EP,1)
        FF(EP,2) = FF0(EP,2)
        FF(EP,3) = FF0(EP,3)
        FF(EP,5) = FF0(EP,4)
        FF(EP,4) = FF0(EP,5)

        MM(EP,1) = MM0(EP,1)
        MM(EP,2) = MM0(EP,2)
        MM(EP,3) = MM0(EP,3)
       END DO !  
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
            C2=VOL(EP)
          C1=TH12(EP)*CDET(EP)
          CM1=C1*(BM(EP,7)*MM(EP,2)+BM(EP,3)*MM(EP,3))
          CM2=C1*(BM(EP,3)*MM(EP,1)+BM(EP,7)*MM(EP,3))
          CC1=C2*(BC(EP,15)*FF(EP,4)+BC(EP,16)*FF(EP,5))
          CC2=C2*(BC(EP,17)*FF(EP,4)+BC(EP,18)*FF(EP,5))
          BC1=BC(EP,1)*FF(EP,4)
          BC3=BC(EP,3)*FF(EP,4)
          BC5=BC(EP,5)*FF(EP,4)
C----------NOEUD 1 ------------
        VF(EP,1,1)=VF(EP,1,1)+C2*(BM(EP,1)*FF(EP,1))
        VF(EP,2,1)=VF(EP,2,1)+C2*(BM(EP,5)*FF(EP,2))
        VF(EP,3,1)=VF(EP,3,1)+C2*(BC1+BC(EP,2)*FF(EP,5))
C
        VM(EP,1,1)=VM(EP,1,1)+C2*(BC3+BC(EP,4)*FF(EP,5))
     1           -C1*(BM(EP,5)*MM(EP,2)+BM(EP,1)*MM(EP,3))
        VM(EP,2,1)=VM(EP,2,1)+C2*(BC5+BC(EP,6)*FF(EP,5))+
     1            C1*(BM(EP,1)*MM(EP,1)+BM(EP,5)*MM(EP,3))
C
        VF(EP,1,3)=VF(EP,1,3)+C2*(BM(EP,3)*FF(EP,1))
        VF(EP,2,3)=VF(EP,2,3)+C2*(BM(EP,7)*FF(EP,2))
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
        VF(EP,3,3)=VF(EP,3,3)+C2*(BC(EP,13)*FF(EP,4)+BC(EP,14)*FF(EP,5))
C
        VM(EP,1,3)=VM(EP,1,3)+CC1-CM1
        VM(EP,2,3)=VM(EP,2,3)+CC2+CM2
C----------NOEUD 2 ------------
        VF(EP,1,2)=VF(EP,1,2)+C2*(BM(EP,2)*FF(EP,1))
        VF(EP,2,2)=VF(EP,2,2)+C2*(BM(EP,6)*FF(EP,2))
        VF(EP,3,2)=VF(EP,3,2)+C2*(BC(EP,7)*FF(EP,4)+BC(EP,8)*FF(EP,5))
        VM(EP,1,2)=VM(EP,1,2)+C2*(BC(EP,9)*FF(EP,4)+BC(EP,10)*FF(EP,5))
     1             -C1*(BM(EP,6)*MM(EP,2)+BM(EP,2)*MM(EP,3))
        VM(EP,2,2)=VM(EP,2,2)
     1            +C2*(BC(EP,11)*FF(EP,4)+BC(EP,12)*FF(EP,5))+
     1              C1*(BM(EP,2)*MM(EP,1)+BM(EP,6)*MM(EP,3))
C
        VF(EP,1,4)=-VF(EP,1,3)
        VF(EP,2,4)=-VF(EP,2,3)
        VF(EP,3,4)=-VF(EP,3,3)
C
        VM(EP,1,4)=VM(EP,1,4)+CC1+CM1
        VM(EP,2,4)=VM(EP,2,4)+CC2-CM2
C
       ENDDO  
#include "vectorize.inc"
       DO I=NPLAT+1,JLT
        EP=IPLAT(I)
          C2=VOL(EP)
          C1=TH12(EP)*CDET(EP)
          BCX=TC(EP,1,1)*FF(EP,4)+TC(EP,1,2)*FF(EP,5)
          BCY=TC(EP,2,1)*FF(EP,4)+TC(EP,2,2)*FF(EP,5)
C           J=1 K0=0 K1=0 K2=0
           VF(EP,1,1)=VF(EP,1,1) +C2*(BM(EP,1)*FF(EP,1)+
     1                               BM(EP,2)*FF(EP,2)+
     3                               BC(EP,1)*BCX+
     4                               BC(EP,2)*BCY )
     5                          +C1*(BMF(EP,1)*MM(EP,1)+
     6                               BMF(EP,2)*MM(EP,2)+
     7                               BMF(EP,3)*MM(EP,3))
C          I1=1 I2=4
           VM(EP,1,1)=VM(EP,1,1)+C2*(BC(EP,7)*BCX+
     1                              BC(EP,8)*BCY) 
     2                         +C1*(BF(EP,1)*MM(EP,1)+
     3                              BF(EP,2)*MM(EP,2)+
     4                              BF(EP,3)*MM(EP,3) )
C          I1=2  I2=2
           VF(EP,2,1)=VF(EP,2,1)+C2*(BM(EP,4)*FF(EP,1)+
     2                              BM(EP,5)*FF(EP,2)+
     4                              BC(EP,3)*BCX+
     5                              BC(EP,4)*BCY )
     6                         +C1*(BMF(EP,4)*MM(EP,1)+
     7                              BMF(EP,5)*MM(EP,2)+
     8                              BMF(EP,6)*MM(EP,3))
C          I1=2 I2=5
           VM(EP,2,1)=VM(EP,2,1)+C2*(BC(EP,9)*BCX+
     1                              BC(EP,10)*BCY) 
     2                         +C1*(BF(EP,4)*MM(EP,1)+
     3                              BF(EP,5)*MM(EP,2)+
     4                              BF(EP,6)*MM(EP,3) )
C          I1=3 I2=3
           VF(EP,3,1)=VF(EP,3,1)+C2*(BM(EP,7)*FF(EP,1)+
     2                              BM(EP,8)*FF(EP,2)+
     4                              BC(EP,5)*BCX+
     5                              BC(EP,6)*BCY )
     6                         +C1*(BMF(EP,7)*MM(EP,1)+
     7                              BMF(EP,8)*MM(EP,2)+
     8                              BMF(EP,9)*MM(EP,3))
C          J=2 K0=2 K1=3 K2=5 I1=4 I2=6
           VF(EP,1,2)=VF(EP,1,2)+C2*(BM(EP,10)*FF(EP,1)+
     2                              BM(EP,11)*FF(EP,2)+
     4                              BC(EP,11)*BCX+
     5                              BC(EP,12)*BCY )
     6                         +C1*(BMF(EP,10)*MM(EP,1)+
     7                              BMF(EP,11)*MM(EP,2)+
     8                              BMF(EP,12)*MM(EP,3))
C          I1=3  I2=9
           VM(EP,1,2)=VM(EP,1,2)+C2*(BC(EP,17)*BCX+
     1                              BC(EP,18)*BCY) 
     2                         +C1*(BF(EP,7)*MM(EP,1)+
     3                              BF(EP,8)*MM(EP,2)+
     4                              BF(EP,9)*MM(EP,3) )
C          I1=5 I2=7
           VF(EP,2,2)=VF(EP,2,2)+C2*(BM(EP,13)*FF(EP,1)+
     2                              BM(EP,14)*FF(EP,2)+
     4                              BC(EP,13)*BCX+
     5                              BC(EP,14)*BCY )
     6                         +C1*(BMF(EP,13)*MM(EP,1)+
     7                              BMF(EP,14)*MM(EP,2)+
     8                              BMF(EP,15)*MM(EP,3))
C          I1=4   I2=10
           VM(EP,2,2)=VM(EP,2,2)+C2*(BC(EP,19)*BCX+
     1                              BC(EP,20)*BCY) 
     2                         +C1*(BF(EP,10)*MM(EP,1)+
     3                              BF(EP,11)*MM(EP,2)+
     4                              BF(EP,12)*MM(EP,3) )
C          I1=6 I2=8
           VF(EP,3,2)=VF(EP,3,2)+C2*(BM(EP,16)*FF(EP,1)+
     2                              BM(EP,17)*FF(EP,2)+
     4                              BC(EP,15)*BCX+
     5                              BC(EP,16)*BCY )
     6                         +C1*(BMF(EP,16)*MM(EP,1)+
     7                              BMF(EP,17)*MM(EP,2)+
     8                              BMF(EP,18)*MM(EP,3))
C          J=3 K0=4  K1=6 K2=10 I1=7 I2=11
           VF(EP,1,3)=VF(EP,1,3)+C2*(BM(EP,19)*FF(EP,1)+
     2                              BM(EP,20)*FF(EP,2)+
     4                              BC(EP,21)*BCX+
     5                              BC(EP,22)*BCY )
     6                         +C1*(BMF(EP,19)*MM(EP,1)+
     7                              BMF(EP,20)*MM(EP,2)+
     8                              BMF(EP,21)*MM(EP,3))
C          I1=5  I2=14
           VM(EP,1,3)=VM(EP,1,3)+C2*(BC(EP,27)*BCX+
     1                              BC(EP,28)*BCY) 
     2                         +C1*(BF(EP,13)*MM(EP,1)+
     3                              BF(EP,14)*MM(EP,2)+
     4                              BF(EP,15)*MM(EP,3) )
C          I1=8  I2=12
           VF(EP,2,3)=VF(EP,2,3)+C2*(BM(EP,22)*FF(EP,1)+
     2                              BM(EP,23)*FF(EP,2)+
     4                              BC(EP,23)*BCX+
     5                              BC(EP,24)*BCY )
     6                         +C1*(BMF(EP,22)*MM(EP,1)+
     7                              BMF(EP,23)*MM(EP,2)+
     8                              BMF(EP,24)*MM(EP,3))
C          I1=6   I2=15
           VM(EP,2,3)=VM(EP,2,3)+C2*(BC(EP,29)*BCX+
     1                              BC(EP,30)*BCY) 
     2                         +C1*(BF(EP,16)*MM(EP,1)+
     3                              BF(EP,17)*MM(EP,2)+
     4                              BF(EP,18)*MM(EP,3) )
C          I1=9 I2=13
           VF(EP,3,3)=VF(EP,3,3)+C2*(BM(EP,25)*FF(EP,1)+
     2                              BM(EP,26)*FF(EP,2)+
     4                              BC(EP,25)*BCX+
     5                              BC(EP,26)*BCY )
     6                         +C1*(BMF(EP,25)*MM(EP,1)+
     7                              BMF(EP,26)*MM(EP,2)+
     8                              BMF(EP,27)*MM(EP,3))
C          J=4  K0=6 K1=9 K2=15 I=1 I1=10 I2=16
           VF(EP,1,4)=VF(EP,1,4)+C2*(BM(EP,28)*FF(EP,1)+
     2                              BM(EP,29)*FF(EP,2)+
     4                              BC(EP,31)*BCX+
     5                              BC(EP,32)*BCY )
     6                         +C1*(BMF(EP,28)*MM(EP,1)+
     7                              BMF(EP,29)*MM(EP,2)+
     8                              BMF(EP,30)*MM(EP,3))
C          I1=7  I2=19
           VM(EP,1,4)=VM(EP,1,4)+C2*(BC(EP,37)*BCX+
     1                              BC(EP,38)*BCY) 
     2                         +C1*(BF(EP,19)*MM(EP,1)+
     3                              BF(EP,20)*MM(EP,2)+
     4                              BF(EP,21)*MM(EP,3) )
C          I1=11  I2=17
           VF(EP,2,4)=VF(EP,2,4)+C2*(BM(EP,31)*FF(EP,1)+
     2                              BM(EP,32)*FF(EP,2)+
     4                              BC(EP,33)*BCX+
     5                              BC(EP,34)*BCY )
     6                         +C1*(BMF(EP,31)*MM(EP,1)+
     7                              BMF(EP,32)*MM(EP,2)+
     8                              BMF(EP,33)*MM(EP,3))
C          I1=8 I2=20
           VM(EP,2,4)=VM(EP,2,4)+C2*(BC(EP,39)*BCX +
     1                              BC(EP,40)*BCY ) 
     2                         +C1*(BF(EP,22)*MM(EP,1) +
     3                              BF(EP,23)*MM(EP,2) +
     4                              BF(EP,24)*MM(EP,3) )
C          I1=12 I2=18
           VF(EP,3,4)=VF(EP,3,4)+C2*(BM(EP,34)*FF(EP,1)+
     2                              BM(EP,35)*FF(EP,2)+
     4                              BC(EP,35)*BCX+
     5                              BC(EP,36)*BCY )
     6                         +C1*(BMF(EP,34)*MM(EP,1)+
     7                              BMF(EP,35)*MM(EP,2)+
     8                              BMF(EP,36)*MM(EP,3))
C
      ENDDO     
C    
      RETURN
      END
Chd|====================================================================
Chd|  CBAFORCT                      source/elements/shell/coqueba/cbafori.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBAFORCT(JFT   ,JLT  ,VOL   ,X13  ,X24  ,
     2                      Y13   ,Y24  ,VSTRE ,VF   ,NPLAT,
     3                      IPLAT ,OFF  ,NEL   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL 'membrane shear traitement' use only PARTIE CONSTANTE
C         VF(3,NODE),VM(2,NODE) : FORCES INTERNES GENERALISEES
C        ENTREES  : THK,X13,X24,Y13,Y24,
C        SORTIES  : VF,VM
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
       INTEGER JFT ,JLT,NPLAT,IPLAT(*),NEL
       my_real 
     .     VOL(*),   OFF(*), X13(*)   ,X24(*)    ,Y13(*)     ,
     .     VSTRE(NEL,5),VF(MVSIZ,3,4) ,Y24(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J,EP
        my_real SX1, SX2, SY1, SY2, THOFF 
C
#include "vectorize.inc"
       DO EP=JFT,NPLAT 
         I=IPLAT(EP)
         THOFF =VOL(I)*VSTRE(I,3)*OFF(I)
         SX1=-THOFF*X24(I)
         SY1= THOFF*Y24(I)
C
         SX2= THOFF*X13(I)
         SY2= -THOFF*Y13(I)
C-------------------------------------------------
C        PARTIE ANTISYM STOCKEE DANS NOEUDS 1,2
C        PARTIE SYM NOEUDS STOCKEE DANS NOEUDS 3,4
C-------------------------------------------------
         VF(I,1,1)=VF(I,1,1)+SX1
         VF(I,2,1)=VF(I,2,1)+SY1
C
         VF(I,1,2)=VF(I,1,2)+SX2
         VF(I,2,2)=VF(I,2,2)+SY2
       ENDDO  
#include "vectorize.inc"
       DO EP=NPLAT+1, JLT
         I=IPLAT(EP)
         THOFF =VOL(I)*VSTRE(I,3)*OFF(I)
         SX1=-THOFF*X24(I)
         SY1= THOFF*Y24(I)
         SX2= THOFF*X13(I)
         SY2= -THOFF*Y13(I)
         VF(I,1,1)=VF(I,1,1)+SX1
         VF(I,2,1)=VF(I,2,1)+SY1
C
         VF(I,1,2)=VF(I,1,2)+SX2
         VF(I,2,2)=VF(I,2,2)+SY2
C
         VF(I,1,3)=VF(I,1,3)-SX1
         VF(I,2,3)=VF(I,2,3)-SY1
C
         VF(I,1,4)=VF(I,1,4)-SX2
         VF(I,2,4)=VF(I,2,4)-SY2
       ENDDO
C
       RETURN
       END
Chd|====================================================================
Chd|  CBAFORRZ                      source/elements/shell/coqueba/cbafori.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBAFORRZ(JFT  ,JLT  ,VOL  ,FF   ,VSRZ ,
     2                      VF   ,VMZ  ,BM   ,BMRZ ,BRZ  ,
     3                      KRZ  ,VRLZ ,EINT ,OFF  ,DT1C ,
     4                      NPLAT,IPLAT,NG   ,NEL  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT ,JLT,NPLAT,IPLAT(*),NG,NEL
        my_real 
     .     VOL(*)  ,FF(NEL,5),VSRZ(NEL,5) ,VF(MVSIZ,3,4) ,VMZ(MVSIZ,4),
     .     BM(MVSIZ,36),BRZ(MVSIZ,4,4),BMRZ(MVSIZ,3,4),KRZ(*),VRLZ(*),
     .     EINT(NEL,2),OFF(*),DT1C
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J,EP
        my_real
     .     S1, C2, A05,ERZ,DEV,C3,FX3,FY3,CSRZ
C    -------- sig_rz added here----a verifier EINT(QEPH sans A)-----
#include "vectorize.inc"
       DO I=JFT,JLT
        EP =IPLAT(I)
        C2=VOL(EP)
        ERZ= VRLZ(I)*DT1C
        DEV = ERZ*C2*HALF*OFF(EP)
        EINT(EP,1) = EINT(EP,1)+ VSRZ(EP,NG)*DEV
        VSRZ(EP,NG)= VSRZ(EP,NG)+KRZ(EP)*ERZ*OFF(EP)
        EINT(EP,1) = EINT(EP,1)+ VSRZ(EP,NG)*DEV
        DO J=1,4
         VMZ(I,J)=VMZ(I,J)+(BMRZ(I,1,J)*FF(EP,1)+
     .             BMRZ(I,2,J)*FF(EP,2)+BMRZ(I,3,J)*FF(EP,3))*C2
        ENDDO
       ENDDO
C        
#include "vectorize.inc"
        DO I=JFT,JLT
         EP =IPLAT(I)
         C2=VOL(EP)*VSRZ(EP,NG)*HALF
         DO J=1,4
          VMZ(I,J)= VMZ(I,J)+BRZ(I,4,J)*C2
         ENDDO
        ENDDO
#include "vectorize.inc"
       DO I=JFT,NPLAT 
         EP=IPLAT(I)
         C2=VOL(EP)
         C3=C2*FF(EP,3)
         CSRZ=C2*VSRZ(EP,NG)*HALF
C-------------------------------------------------
C        PARTIE ANTISYM STOCKEE DANS NOEUDS 1,2
C        PARTIE SYM NOEUDS STOCKEE DANS NOEUDS 3,4
C-------------------------------------------------
C----------NOEUD 1 ------------
        VF(EP,1,1)=VF(EP,1,1)+(C3-CSRZ)*BM(EP,5)
        VF(EP,2,1)=VF(EP,2,1)+(C3+CSRZ)*BM(EP,1)
C
        FX3 = (C3-CSRZ)*BM(EP,7)
        FY3 = (C3+CSRZ)*BM(EP,3)
        VF(EP,1,3)=VF(EP,1,3)+FX3
        VF(EP,2,3)=VF(EP,2,3)+FY3
C----------NOEUD 2 ------------
        VF(EP,1,2)=VF(EP,1,2)+(C3-CSRZ)*BM(EP,6)
        VF(EP,2,2)=VF(EP,2,2)+(C3+CSRZ)*BM(EP,2)
C
        VF(EP,1,4)=VF(EP,1,4)-FX3
        VF(EP,2,4)=VF(EP,2,4)-FY3
       ENDDO  
#include "vectorize.inc"
       DO I=NPLAT+1,JLT
        EP=IPLAT(I)
          C2=VOL(EP)
          C3=C2*FF(EP,3)
          CSRZ=C2*VSRZ(EP,NG)*HALF
           VF(EP,1,1)=VF(EP,1,1)+C3*BM(EP,3)+CSRZ*BRZ(I,1,1)
           VF(EP,2,1)=VF(EP,2,1)+C3*BM(EP,6)+CSRZ*BRZ(I,2,1)
           VF(EP,3,1)=VF(EP,3,1)+C3*BM(EP,9)+CSRZ*BRZ(I,3,1)
           VF(EP,1,2)=VF(EP,1,2)+C3*BM(EP,12)+CSRZ*BRZ(I,1,2)
           VF(EP,2,2)=VF(EP,2,2)+C3*BM(EP,15)+CSRZ*BRZ(I,2,2)
           VF(EP,3,2)=VF(EP,3,2)+C3*BM(EP,18)+CSRZ*BRZ(I,3,2)
           VF(EP,1,3)=VF(EP,1,3)+C3*BM(EP,21)+CSRZ*BRZ(I,1,3)
           VF(EP,2,3)=VF(EP,2,3)+C3*BM(EP,24)+CSRZ*BRZ(I,2,3)
           VF(EP,3,3)=VF(EP,3,3)+C3*BM(EP,27)+CSRZ*BRZ(I,3,3)
           VF(EP,1,4)=VF(EP,1,4)+C3*BM(EP,30)+CSRZ*BRZ(I,1,4)
           VF(EP,2,4)=VF(EP,2,4)+C3*BM(EP,33)+CSRZ*BRZ(I,2,4)
           VF(EP,3,4)=VF(EP,3,4)+C3*BM(EP,36)+CSRZ*BRZ(I,3,4)
       ENDDO
C
       RETURN
       END
	SUBROUTINE CBAFORI1(JFT  ,JLT  ,FF  ,BM  ,VF,
     .                      NPLAT,IPLAT,VOL ,NEL ) 
C--------------------------------------------------------------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C	CALCUL DES FORCES INTERNES AUX NOEUDS (MEMBRANE ONLY)
C	ENTREES :  NEL,MNT,NPT,NP,NG,NIXC,IXC,HZ,OFF,CDET,EPAIS,BUFCON,VQN,BM,BMF,BF,BC
C	SORTIES : VF,VM
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT,IPLAT(*),NEL
      MY_REAL 
     .   FF(NEL,5),VOL(*),BM(MVSIZ,36),VF(MVSIZ,3,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      MY_REAL 
     .    C1,C2
C--------------------------------------------------------------------------------------------------
#include "vectorize.inc"
       DO I=JFT,NPLAT 
        EP=IPLAT(I)
        C2=VOL(EP)
C----------NOEUD 1 ------------
        VF(EP,1,1)=VF(EP,1,1)+C2*(BM(EP,1)*FF(EP,1))
        VF(EP,2,1)=VF(EP,2,1)+C2*(BM(EP,5)*FF(EP,2))
C
        VF(EP,1,3)=VF(EP,1,3)+C2*(BM(EP,3)*FF(EP,1))
        VF(EP,2,3)=VF(EP,2,3)+C2*(BM(EP,7)*FF(EP,2))
C----------NOEUD 2 ------------
        VF(EP,1,2)=VF(EP,1,2)+C2*(BM(EP,2)*FF(EP,1))
        VF(EP,2,2)=VF(EP,2,2)+C2*(BM(EP,6)*FF(EP,2))
C
        VF(EP,1,4)=-VF(EP,1,3)
        VF(EP,2,4)=-VF(EP,2,3)
C
       ENDDO  
      RETURN
      END
