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
Chd|  CBADEFPINCH                   source/elements/shell/coqueba/cbadefpinch.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBADEFPINCH(
     1                       JFT  ,JLT  ,NG   ,VQG   ,VDEF  ,
     2                       VETA ,VKSI ,TC   ,NPLAT ,IPLAT ,
     3                       BCP  ,BP   ,VPINCHXYZ   ,VDEFPINCH ,TNPG  ,
     4                       DBETADXY   ,VPINCHT1    ,VPINCHT2 ,BPINCHDAMP)
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
      INTEGER NG,JFT,JLT,NPLAT,IPLAT(*)
      MY_REAL 
     .   VKSI(4,4),VETA(4,4),
     .   BCP(MVSIZ,8),BP(MVSIZ,4),
     .   VPINCHXYZ(MVSIZ,4),VDEF(MVSIZ,8),VDEFPINCH(MVSIZ,3),
     .   VQG(MVSIZ,3,3,4),TNPG(MVSIZ,4,4),TC(MVSIZ,2,2),
     .   DBETADXY(MVSIZ,3),VPINCHT1(MVSIZ,4),VPINCHT2(MVSIZ,4),
     .   BPINCHDAMP(MVSIZ,8)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,EP
      MY_REAL BCX,BCY,D1,D2,
     .        DN1DX,DN2DX,DN3DX,DN4DX,
     .        DN1DY,DN2DY,DN3DY,DN4DY
C---------------------------------
C
C----------------------------------
C     MATRIX [B] DUE TO PINCHING
C----------------------------------
#include "vectorize.inc"
       DO I=JFT,JLT 
         EP=IPLAT(I)
C----------------------------------
C     MATRIX [BCP] FOR TRNASVERSE SHEAR DUE TO PINCHING
C----------------------------------

C-------Node 1 contribution

         D1=VKSI(1,NG)*TC(EP,1,1)+VETA(1,NG)*TC(EP,2,1)
         D2=VKSI(1,NG)*TC(EP,1,2)+VETA(1,NG)*TC(EP,2,2)
C
C        dN1/dx, dN1/dy
C
         BCP(EP,1) = VQG(EP,1,1,NG)*D1
         BCP(EP,2) = VQG(EP,1,2,NG)*D2

C-------Node 2 contribution

         D1=VKSI(2,NG)*TC(EP,1,1)+VETA(2,NG)*TC(EP,2,1)
         D2=VKSI(2,NG)*TC(EP,1,2)+VETA(2,NG)*TC(EP,2,2)

C        dN2/dx, dN2/dy
         BCP(EP,3) = VQG(EP,1,1,NG)*D1
         BCP(EP,4) = VQG(EP,1,2,NG)*D2

C-------Node 3 contribution

         D1=VKSI(3,NG)*TC(EP,1,1)+VETA(3,NG)*TC(EP,2,1)
         D2=VKSI(3,NG)*TC(EP,1,2)+VETA(3,NG)*TC(EP,2,2)

C        dN3/dx, dN3/dy
         BCP(EP,5) = VQG(EP,1,1,NG)*D1
         BCP(EP,6) = VQG(EP,1,2,NG)*D2

C-------Node 4 contribution

         D1=VKSI(4,NG)*TC(EP,1,1)+VETA(4,NG)*TC(EP,2,1)
         D2=VKSI(4,NG)*TC(EP,1,2)+VETA(4,NG)*TC(EP,2,2)

C        dN4/dx, dN4/dy
         BCP(EP,7) = VQG(EP,1,1,NG)*D1
         BCP(EP,8) = VQG(EP,1,2,NG)*D2

C--------Node 1 contribution to BCX/BCY due to pinching
         BCX=      BCP(EP,1)*VPINCHXYZ(EP,1)       
         BCY=      BCP(EP,2)*VPINCHXYZ(EP,1)

C--------Node 2 contribution to BCX/BCY due to pinching
         BCX=BCX + BCP(EP,3)*VPINCHXYZ(EP,2)       
         BCY=BCY + BCP(EP,4)*VPINCHXYZ(EP,2)

C--------Node 3 contribution to BCX/BCY due to pinching
         BCX=BCX + BCP(EP,5)*VPINCHXYZ(EP,3)       
         BCY=BCY + BCP(EP,6)*VPINCHXYZ(EP,3)

C--------Node 4 contribution to BCX/BCY due to pinching
         BCX=BCX + BCP(EP,7)*VPINCHXYZ(EP,4)       
         BCY=BCY + BCP(EP,8)*VPINCHXYZ(EP,4)

C        calculate CT rate because of updates in BCX and BCY     

         VDEFPINCH(EP,1)= TC(EP,1,1)*BCX+TC(EP,2,1)*BCX 
         VDEFPINCH(EP,2)= TC(EP,1,2)*BCX+TC(EP,2,2)*BCY

C----------------------------------
C     MATRIX [BP] FOR PINCHING
C----------------------------------

C--------even though constant for all elements, 
C        if B_gp = Ni beta_i does not work then will need it here
C        like MITC4 calculation of CT at the edge centre.
         BP(EP,1) = TNPG(EP,1,NG)
         BP(EP,2) = TNPG(EP,2,NG)
         BP(EP,3) = TNPG(EP,3,NG)
         BP(EP,4) = TNPG(EP,4,NG)

         VDEFPINCH(EP,3) = BP(EP,1)*VPINCHXYZ(EP,1) 
     .                   + BP(EP,2)*VPINCHXYZ(EP,2)
     .                   + BP(EP,3)*VPINCHXYZ(EP,3)
     .                   + BP(EP,4)*VPINCHXYZ(EP,4)

C
C----------------------------------
C     CALCULATE DBeta_x/Dx, Dbeta_y/Dx, Dbeta_x/dy and Dbeta_y/dy at this Gp
C----------------------------------
C-------Calculating dN/dx and dN/dy at this gp
        DN1DX = VKSI(1,NG)*TC(EP,1,1)+VETA(1,NG)*TC(EP,2,1)
        DN1DY = VKSI(1,NG)*TC(EP,1,2)+VETA(1,NG)*TC(EP,2,2)
        DN2DX = VKSI(2,NG)*TC(EP,1,1)+VETA(2,NG)*TC(EP,2,1)
        DN2DY = VKSI(2,NG)*TC(EP,1,2)+VETA(2,NG)*TC(EP,2,2)
        DN3DX = VKSI(3,NG)*TC(EP,1,1)+VETA(3,NG)*TC(EP,2,1)
        DN3DY = VKSI(3,NG)*TC(EP,1,2)+VETA(3,NG)*TC(EP,2,2)
        DN4DX = VKSI(4,NG)*TC(EP,1,1)+VETA(4,NG)*TC(EP,2,1)
        DN4DY = VKSI(4,NG)*TC(EP,1,2)+VETA(4,NG)*TC(EP,2,2)
C
C       dbeta_x/dx at this gp
        DBETADXY(EP,1) =  DN1DX*VPINCHT1(EP,1)+DN2DX*VPINCHT1(EP,2)
     -                     +DN3DX*VPINCHT1(EP,3)+DN4DX*VPINCHT1(EP,4)
C       dbeta_y/dy at this gp
        DBETADXY(EP,2) =  DN1DY*VPINCHT2(EP,1)+DN2DY*VPINCHT2(EP,2)
     -                     +DN3DY*VPINCHT2(EP,3)+DN4DY*VPINCHT2(EP,4)
C       0.5*(dbeta_x/dy +dbeta_y/dx) at this gp
        DBETADXY(EP,3) =  HALF*(DN1DY*VPINCHT1(EP,1)+DN2DY*VPINCHT1(EP,2)
     -                     +DN3DY*VPINCHT1(EP,3)+DN4DY*VPINCHT1(EP,4)
     -                     +DN1DX*VPINCHT2(EP,1)+DN2DX*VPINCHT2(EP,2)
     -                     +DN3DX*VPINCHT2(EP,3)+DN4DX*VPINCHT2(EP,4))
C
        BPINCHDAMP(EP,1) = DN1DX
        BPINCHDAMP(EP,2) = DN1DY
        BPINCHDAMP(EP,3) = DN2DX
        BPINCHDAMP(EP,4) = DN2DY
        BPINCHDAMP(EP,5) = DN3DX
        BPINCHDAMP(EP,6) = DN3DY
        BPINCHDAMP(EP,7) = DN4DX
        BPINCHDAMP(EP,8) = DN4DY
C
       ENDDO
       RETURN
       END
