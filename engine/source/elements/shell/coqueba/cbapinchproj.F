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
Chd|  CBAPINCHPROJ                  source/elements/shell/coqueba/cbapinchproj.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE CBAPINCHPROJ(
     1            JFT    ,JLT    ,VQN    ,VQ     ,VFPINCH,
     2            NPLAT  ,IPLAT  ,FP     ,COREL  ,DI     ,THK0,
     3            VFPINCHDAMPX,VFPINCHDAMPY)
C-------------------------------------------------------------------------------
C   M o d u l e s
C-------------------------------------------------------------------------------
      USE PINCHTYPE_MOD
      USE ELBUFDEF_MOD
C-------------------------------------------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-------------------------------------------------------------------------------
C   D U M M Y   A R G U M E N T S
C-------------------------------------------------------------------------------
      INTEGER JFT,JLT,NPLAT,IPLAT(*)
      my_real 
     .   VQN(MVSIZ,9,4),VFPINCH(MVSIZ,4),VQ(MVSIZ,3,3),
     .   COREL(MVSIZ,3,4),DI(MVSIZ,6),FP(MVSIZ,3,4), THK0(*),
     .   VFPINCHDAMPX(MVSIZ,4),VFPINCHDAMPY(MVSIZ,4)
C-------------------------------------------------------------------------------
C   L O C A L   V A R I A B L E S
C-------------------------------------------------------------------------------
      INTEGER I, J, K,EP
      my_real 
     .     FPP(3,4), ELTHKINV
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
      DO EP=JFT,JLT
        K=IPLAT(EP)        
        ELTHKINV = ONE/THK0(EP)
C       FPP(1:3,node) = VFPINCH(node) * Normal(1:3,node)
C       Node 1 : FX,FY,FZ
        FPP(1,1)= VFPINCH(K,1)*VQN(K,7,1)*ELTHKINV
        FPP(2,1)= VFPINCH(K,1)*VQN(K,8,1)*ELTHKINV
        FPP(3,1)= VFPINCH(K,1)*VQN(K,9,1)*ELTHKINV
C       Node 2 : FX,FY,FZ
        FPP(1,2)= VFPINCH(K,2)*VQN(K,7,2)*ELTHKINV
        FPP(2,2)= VFPINCH(K,2)*VQN(K,8,2)*ELTHKINV
        FPP(3,2)= VFPINCH(K,2)*VQN(K,9,2)*ELTHKINV
C       Node 3 : FX,FY,FZ
        FPP(1,3)= VFPINCH(K,3)*VQN(K,7,3)*ELTHKINV
        FPP(2,3)= VFPINCH(K,3)*VQN(K,8,3)*ELTHKINV
        FPP(3,3)= VFPINCH(K,3)*VQN(K,9,3)*ELTHKINV
C       Node 1 : FX,FY,FZ
        FPP(1,4)= VFPINCH(K,4)*VQN(K,7,4)*ELTHKINV
        FPP(2,4)= VFPINCH(K,4)*VQN(K,8,4)*ELTHKINV
        FPP(3,4)= VFPINCH(K,4)*VQN(K,9,4)*ELTHKINV

C       damping contribution X
        FPP(1,1)= FPP(1,1)+VFPINCHDAMPX(K,1)*VQN(K,1,1)*ELTHKINV
        FPP(2,1)= FPP(2,1)+VFPINCHDAMPX(K,1)*VQN(K,2,1)*ELTHKINV
        FPP(3,1)= FPP(3,1)+VFPINCHDAMPX(K,1)*VQN(K,3,1)*ELTHKINV
C       Node 2 : FX,FY,FZ
        FPP(1,2)= FPP(1,2)+VFPINCHDAMPX(K,2)*VQN(K,1,2)*ELTHKINV
        FPP(2,2)= FPP(2,2)+VFPINCHDAMPX(K,2)*VQN(K,2,2)*ELTHKINV
        FPP(3,2)= FPP(3,2)+VFPINCHDAMPX(K,2)*VQN(K,3,2)*ELTHKINV
C       Node 3 : FX,FY,FZ
        FPP(1,3)= FPP(1,3)+VFPINCHDAMPX(K,3)*VQN(K,1,3)*ELTHKINV
        FPP(2,3)= FPP(2,3)+VFPINCHDAMPX(K,3)*VQN(K,2,3)*ELTHKINV
        FPP(3,3)= FPP(3,3)+VFPINCHDAMPX(K,3)*VQN(K,3,3)*ELTHKINV
C       Node 4 : FX,FY,FZ
        FPP(1,4)= FPP(1,4)+VFPINCHDAMPX(K,4)*VQN(K,1,4)*ELTHKINV
        FPP(2,4)= FPP(2,4)+VFPINCHDAMPX(K,4)*VQN(K,2,4)*ELTHKINV
        FPP(3,4)= FPP(3,4)+VFPINCHDAMPX(K,4)*VQN(K,3,4)*ELTHKINV

C       damping contribution Y
        FPP(1,1)= FPP(1,1)+VFPINCHDAMPY(K,1)*VQN(K,4,1)*ELTHKINV
        FPP(2,1)= FPP(2,1)+VFPINCHDAMPY(K,1)*VQN(K,5,1)*ELTHKINV
        FPP(3,1)= FPP(3,1)+VFPINCHDAMPY(K,1)*VQN(K,6,1)*ELTHKINV
C       Node 2 : FX,FY,FZ
        FPP(1,2)= FPP(1,2)+VFPINCHDAMPY(K,2)*VQN(K,4,2)*ELTHKINV
        FPP(2,2)= FPP(2,2)+VFPINCHDAMPY(K,2)*VQN(K,5,2)*ELTHKINV
        FPP(3,2)= FPP(3,2)+VFPINCHDAMPY(K,2)*VQN(K,6,2)*ELTHKINV
C       Node 3 : FX,FY,FZ
        FPP(1,3)= FPP(1,3)+VFPINCHDAMPY(K,3)*VQN(K,4,3)*ELTHKINV
        FPP(2,3)= FPP(2,3)+VFPINCHDAMPY(K,3)*VQN(K,5,3)*ELTHKINV
        FPP(3,3)= FPP(3,3)+VFPINCHDAMPY(K,3)*VQN(K,6,3)*ELTHKINV
C       Node 4 : FX,FY,FZ
        FPP(1,4)= FPP(1,4)+VFPINCHDAMPY(K,4)*VQN(K,4,4)*ELTHKINV
        FPP(2,4)= FPP(2,4)+VFPINCHDAMPY(K,4)*VQN(K,5,4)*ELTHKINV
        FPP(3,4)= FPP(3,4)+VFPINCHDAMPY(K,4)*VQN(K,6,4)*ELTHKINV
C
C       global projection
C       FP(1:3,node) (3x1) = VQ (3x3) * FPP(1:3,node) (3,1)
C       Node 1 : FX,FY,FZ
        FP(K,1,1)= VQ(K,1,1)*FPP(1,1)+VQ(K,1,2)*FPP(2,1)+VQ(K,1,3)*FPP(3,1)
        FP(K,2,1)= VQ(K,2,1)*FPP(1,1)+VQ(K,2,2)*FPP(2,1)+VQ(K,2,3)*FPP(3,1)
        FP(K,3,1)= VQ(K,3,1)*FPP(1,1)+VQ(K,3,2)*FPP(2,1)+VQ(K,3,3)*FPP(3,1)
C       Node 2 : FX,FY,FZ
        FP(K,1,2)= VQ(K,1,1)*FPP(1,2)+VQ(K,1,2)*FPP(2,2)+VQ(K,1,3)*FPP(3,2)
        FP(K,2,2)= VQ(K,2,1)*FPP(1,2)+VQ(K,2,2)*FPP(2,2)+VQ(K,2,3)*FPP(3,2)
        FP(K,3,2)= VQ(K,3,1)*FPP(1,2)+VQ(K,3,2)*FPP(2,2)+VQ(K,3,3)*FPP(3,2)
C       Node 3 : FX,FY,FZ
        FP(K,1,3)= VQ(K,1,1)*FPP(1,3)+VQ(K,1,2)*FPP(2,3)+VQ(K,1,3)*FPP(3,3)
        FP(K,2,3)= VQ(K,2,1)*FPP(1,3)+VQ(K,2,2)*FPP(2,3)+VQ(K,2,3)*FPP(3,3)
        FP(K,3,3)= VQ(K,3,1)*FPP(1,3)+VQ(K,3,2)*FPP(2,3)+VQ(K,3,3)*FPP(3,3)
C       Node 4 : FX,FY,FZ
        FP(K,1,4)= VQ(K,1,1)*FPP(1,4)+VQ(K,1,2)*FPP(2,4)+VQ(K,1,3)*FPP(3,4)
        FP(K,2,4)= VQ(K,2,1)*FPP(1,4)+VQ(K,2,2)*FPP(2,4)+VQ(K,2,3)*FPP(3,4)
        FP(K,3,4)= VQ(K,3,1)*FPP(1,4)+VQ(K,3,2)*FPP(2,4)+VQ(K,3,3)*FPP(3,4)
C
      ENDDO     
      RETURN
      END
