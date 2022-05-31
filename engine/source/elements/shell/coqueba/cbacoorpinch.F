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
Chd|====================================================================
Chd|  CBACOORPINCH                  source/elements/shell/coqueba/cbacoorpinch.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CBACOORPINCH( 
     1                         TNPG    ,VPINCHXYZ ,VPINCH ,
     2                         VQ      ,VQN       ,IXC    ,JFT  ,JLT  ,
     3                         NPLAT   ,IPLAT     ,THK    ,DT1C,
     4                         FACP    ,LC        ,
     5                         VPINCHT1,VPINCHT2)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc" 
#include      "param_c.inc" 
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXC(NIXC,*), JFT, JLT, NPLAT, IPLAT(*)
      my_real 
     .   TNPG(MVSIZ,4,4), VPINCHXYZ(MVSIZ,4), VPINCH(3,*), 
     .   VQ(MVSIZ,3,3), VQN(MVSIZ,9,4), THK(*), DT1C(*),
     .   FACP(MVSIZ), LC(MVSIZ),
     .   VPINCHT1(MVSIZ,4),VPINCHT2(MVSIZ,4)   
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER NN(4), I, EP
      my_real 
     .   PG, PGPP, PGPM, PGMM, BETABETA(3,4), ELTHKINV, THKN(4), AVGTHK
      DATA   PG/.577350269189626/
C=======================================================================


C     shape function I evaluated at gauss point J = TNPG(I,J)
      PGPP = FOURTH*(ONE+PG)*(ONE+PG)
      PGPM = FOURTH*(ONE+PG)*(ONE-PG)
      PGMM = FOURTH*(ONE-PG)*(ONE-PG)
C
      DO I=JFT,JLT
        EP =IPLAT(I)

        TNPG(EP,1,1) = PGPP
        TNPG(EP,2,1) = PGPM
        TNPG(EP,3,1) = PGMM
        TNPG(EP,4,1) = PGPM

        TNPG(EP,1,2) = PGPM
        TNPG(EP,2,2) = PGPP
        TNPG(EP,3,2) = PGPM
        TNPG(EP,4,2) = PGMM

        TNPG(EP,1,3) = PGMM
        TNPG(EP,2,3) = PGPM
        TNPG(EP,3,3) = PGPP
        TNPG(EP,4,3) = PGPM

        TNPG(EP,1,4) = PGPM
        TNPG(EP,2,4) = PGMM
        TNPG(EP,3,4) = PGPM
        TNPG(EP,4,4) = PGPP
      ENDDO

C Transform VPINCH into VPINCHXYZ 
     
      DO I=JFT,JLT
        EP =IPLAT(I)   
        NN(1)=IXC(2,EP)
        NN(2)=IXC(3,EP)
        NN(3)=IXC(4,EP)
        NN(4)=IXC(5,EP)

        BETABETA(1,1) =VQ(EP,1,1)*VPINCH(1,NN(1))+VQ(EP,2,1)*VPINCH(2,NN(1))
     1              +VQ(EP,3,1)*VPINCH(3,NN(1))
        BETABETA(1,2) =VQ(EP,1,1)*VPINCH(1,NN(2))+VQ(EP,2,1)*VPINCH(2,NN(2))
     1              +VQ(EP,3,1)*VPINCH(3,NN(2))
        BETABETA(1,3) =VQ(EP,1,1)*VPINCH(1,NN(3))+VQ(EP,2,1)*VPINCH(2,NN(3))
     1              +VQ(EP,3,1)*VPINCH(3,NN(3))
        BETABETA(1,4) =VQ(EP,1,1)*VPINCH(1,NN(4))+VQ(EP,2,1)*VPINCH(2,NN(4))
     1              +VQ(EP,3,1)*VPINCH(3,NN(4))
        BETABETA(2,1) =VQ(EP,1,2)*VPINCH(1,NN(1))+VQ(EP,2,2)*VPINCH(2,NN(1))
     1              +VQ(EP,3,2)*VPINCH(3,NN(1))
        BETABETA(2,2) =VQ(EP,1,2)*VPINCH(1,NN(2))+VQ(EP,2,2)*VPINCH(2,NN(2))
     1              +VQ(EP,3,2)*VPINCH(3,NN(2))
        BETABETA(2,3) =VQ(EP,1,2)*VPINCH(1,NN(3))+VQ(EP,2,2)*VPINCH(2,NN(3))
     1              +VQ(EP,3,2)*VPINCH(3,NN(3))
        BETABETA(2,4) =VQ(EP,1,2)*VPINCH(1,NN(4))+VQ(EP,2,2)*VPINCH(2,NN(4))
     1              +VQ(EP,3,2)*VPINCH(3,NN(4))
        BETABETA(3,1) =VQ(EP,1,3)*VPINCH(1,NN(1))+VQ(EP,2,3)*VPINCH(2,NN(1))
     1              +VQ(EP,3,3)*VPINCH(3,NN(1))
        BETABETA(3,2) =VQ(EP,1,3)*VPINCH(1,NN(2))+VQ(EP,2,3)*VPINCH(2,NN(2))
     1              +VQ(EP,3,3)*VPINCH(3,NN(2))
        BETABETA(3,3) =VQ(EP,1,3)*VPINCH(1,NN(3))+VQ(EP,2,3)*VPINCH(2,NN(3))
     1              +VQ(EP,3,3)*VPINCH(3,NN(3))
        BETABETA(3,4) =VQ(EP,1,3)*VPINCH(1,NN(4))+VQ(EP,2,3)*VPINCH(2,NN(4))
     1              +VQ(EP,3,3)*VPINCH(3,NN(4))

C       projecting BETABETA on t1

        VPINCHT1(EP,1)=VQN(EP,1,1)*BETABETA(1,1)+
     +             VQN(EP,2,1)*BETABETA(2,1)+VQN(EP,3,1)*BETABETA(3,1)

        VPINCHT1(EP,2)=VQN(EP,1,2)*BETABETA(1,2)+
     +             VQN(EP,2,2)*BETABETA(2,2)+VQN(EP,3,2)*BETABETA(3,2)

        VPINCHT1(EP,3)=VQN(EP,1,3)*BETABETA(1,3)+
     +             VQN(EP,2,3)*BETABETA(2,3)+VQN(EP,3,3)*BETABETA(3,3)

        VPINCHT1(EP,4)=VQN(EP,1,4)*BETABETA(1,4)+
     +             VQN(EP,2,4)*BETABETA(2,4)+VQN(EP,3,4)*BETABETA(3,4)

C       projecting BETABETA on t2

        VPINCHT2(EP,1)=VQN(EP,4,1)*BETABETA(1,1)+
     +             VQN(EP,5,1)*BETABETA(2,1)+VQN(EP,6,1)*BETABETA(3,1)

        VPINCHT2(EP,2)=VQN(EP,4,2)*BETABETA(1,2)+
     +             VQN(EP,5,2)*BETABETA(2,2)+VQN(EP,6,2)*BETABETA(3,2)

        VPINCHT2(EP,3)=VQN(EP,4,3)*BETABETA(1,3)+
     +             VQN(EP,5,3)*BETABETA(2,3)+VQN(EP,6,3)*BETABETA(3,3)

        VPINCHT2(EP,4)=VQN(EP,4,4)*BETABETA(1,4)+
     +             VQN(EP,5,4)*BETABETA(2,4)+VQN(EP,6,4)*BETABETA(3,4)

C       projection of BETABETA onto nodal normal giving VPINCHXYZ 
         
        VPINCHXYZ(EP,1)=VQN(EP,7,1)*BETABETA(1,1)+
     +             VQN(EP,8,1)*BETABETA(2,1)+VQN(EP,9,1)*BETABETA(3,1)

        VPINCHXYZ(EP,2)=VQN(EP,7,2)*BETABETA(1,2)+
     +             VQN(EP,8,2)*BETABETA(2,2)+VQN(EP,9,2)*BETABETA(3,2)

        VPINCHXYZ(EP,3)=VQN(EP,7,3)*BETABETA(1,3)+
     +             VQN(EP,8,3)*BETABETA(2,3)+VQN(EP,9,3)*BETABETA(3,3)

        VPINCHXYZ(EP,4)=VQN(EP,7,4)*BETABETA(1,4)+
     +             VQN(EP,8,4)*BETABETA(2,4)+VQN(EP,9,4)*BETABETA(3,4)

C       calculate average thickness
        THKN(1) = THK(EP)*(ONE+TWO*VPINCHXYZ(EP,1)*DT1C(EP))
        THKN(2) = THK(EP)*(ONE+TWO*VPINCHXYZ(EP,2)*DT1C(EP))
        THKN(3) = THK(EP)*(ONE+TWO*VPINCHXYZ(EP,3)*DT1C(EP))          
        THKN(4) = THK(EP)*(ONE+TWO*VPINCHXYZ(EP,4)*DT1C(EP))
C    
        AVGTHK = FOURTH*(THKN(1) + THKN(2) + THKN(3) + THKN(4))
        ELTHKINV = TWO/AVGTHK

C       dividing by thickness (definition of beta for pinching)
        VPINCHXYZ(EP,1) = VPINCHXYZ(EP,1)*ELTHKINV
        VPINCHXYZ(EP,2) = VPINCHXYZ(EP,2)*ELTHKINV
        VPINCHXYZ(EP,3) = VPINCHXYZ(EP,3)*ELTHKINV
        VPINCHXYZ(EP,4) = VPINCHXYZ(EP,4)*ELTHKINV  

C       dividing by thickness (definition of beta for pinching) in t1
        VPINCHT1(EP,1) = VPINCHT1(EP,1)*ELTHKINV
        VPINCHT1(EP,2) = VPINCHT1(EP,2)*ELTHKINV
        VPINCHT1(EP,3) = VPINCHT1(EP,3)*ELTHKINV
        VPINCHT1(EP,4) = VPINCHT1(EP,4)*ELTHKINV 

C       dividing by thickness (definition of beta for pinching) in t2
        VPINCHT2(EP,1) = VPINCHT2(EP,1)*ELTHKINV
        VPINCHT2(EP,2) = VPINCHT2(EP,2)*ELTHKINV
        VPINCHT2(EP,3) = VPINCHT2(EP,3)*ELTHKINV
        VPINCHT2(EP,4) = VPINCHT2(EP,4)*ELTHKINV 

C       calculate scaling factor for stiffness 
C       to be used later for dynamic condensation STIFPINCH
        FACP(EP) = (LC(EP)/AVGTHK)**2

      ENDDO    
      RETURN
      END
