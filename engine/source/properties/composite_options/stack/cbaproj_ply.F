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
Csm|============================================================
Csm|  CBAPROJ_PLY                          /coqueba/cbaproj_ply.F
Csm|------------------------------------------------------------
Csm|-- appelee par -----------
Csm|         CBAFORC3                         /coqueba/cbaforc3.F
Csm|-- appelle ---------------
Csm|============================================================
Chd|====================================================================
Chd|  CBAPROJ_PLY                   source/properties/composite_options/stack/cbaproj_ply.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBAPROJ_PLY(
     1            JFT    ,JLT  ,NPT  , NPLAT  ,IPLAT, VQN    ,
     2            VQ     ,VF   , VFI , COREL ,DI ,
     6            F11    ,F12  ,F13  ,F14     ,F21   ,
     7            F22    ,F23  ,F24  ,F31     ,F32   ,
     8            F33    ,F34  ,OFF  )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        TRANSMET LES FORCES INTERNES LOCALES VF,VM ---> GLOBALES FIJ ,MIJ
C        ENTREES : 
C        SORTIES : FIJ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT   ,IPLAT(*),NPT
      my_real 
     .   VQN(MVSIZ,9,4),VF(MVSIZ,12,NPT),VQ(MVSIZ,3,3),
     .   COREL(MVSIZ,3,4),DI(MVSIZ,6)
      my_real
     .   F11(MVSIZ,NPT), F12(MVSIZ,NPT), F13(MVSIZ,NPT), 
     .   F14(MVSIZ,NPT), F21(MVSIZ,NPT), F22(MVSIZ,NPT),
     .   F23(MVSIZ,NPT), F24(MVSIZ,NPT), F31(MVSIZ,NPT), 
     .   F32(MVSIZ,NPT), F33(MVSIZ,NPT), F34(MVSIZ,NPT),
     .   VFI(MVSIZ,12,NPT) ,OFF(*)                     
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,EP,I_INF,I_SUP,IPLY,NPLAT0
      my_real 
     .     MM(3,4),FL(3,4),ML(2,4),C1,Z1,
     .     AR(3),AD(4),ALR(3),ALD(4),DBAD(3),
     .     F1, F2,F3,FAC,FAC1,FAC2,FAC3,FL1,FL2,
     .     FL3,E33,G,NU  
      my_real
     .   ML11(MVSIZ),ML12(MVSIZ),ML13(MVSIZ) ,ML14(MVSIZ),ML21(MVSIZ),         
     .   ML22(MVSIZ),ML23(MVSIZ),ML24(MVSIZ) ,ML31(MVSIZ),ML32(MVSIZ),         
     .   ML33(MVSIZ),ML34(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
!!        NPLAT0  = JLT
        NPLAT0 = NPLAT
       DO J=1,NPT
#include "vectorize.inc"
        DO EP=JFT,NPLAT0 
          K=IPLAT(EP)
C      I=1
          FL(1,1)=  VF(K,1,J)  +   VF(K,7,J)  
          FL(1,2)=  VF(K,4,J)  +   VF(K,10,J) 
          FL(1,3)= -VF(K,1,J)  +   VF(K,7,J)  
          FL(1,4)= -VF(K,4,J)  +   VF(K,10,J)         
C      I=2
          FL(2,1)=  VF(K,2,J)  +   VF(K,8,J)  
          FL(2,2)=  VF(K,5,J)  +   VF(K,11,J) 
          FL(2,3)= -VF(K,2,J)  +   VF(K,8,J)  
          FL(2,4)= -VF(K,5,J)  +   VF(K,11,J) 
C      I  =3
          FL(3,1)=  VF(K,3,J)  +   VF(K,9,J)  
          FL(3,2)=  VF(K,6,J)  +   VF(K,12,J) 
          FL(3,3)= -VF(K,3,J)  +   VF(K,9,J)
          FL(3,4)= -VF(K,6,J)  +   VF(K,12,J)
C---------------------------------------
C   TRANS LOCAL-->GLOBAL ET 5DDL-->6DDL
C---------------------------------------
C
C        J=1
C        I=1
         F11(K,J)= VQ(K,1,1)*FL(1,1)+VQ(K,1,2)*FL(2,1)+VQ(K,1,3)*FL(3,1)
C        I=2
         F21(K,J)= VQ(K,2,1)*FL(1,1)+VQ(K,2,2)*FL(2,1)+VQ(K,2,3)*FL(3,1)
C        I=3
         F31(K,J)= VQ(K,3,1)*FL(1,1)+VQ(K,3,2)*FL(2,1)+VQ(K,3,3)*FL(3,1)
C
C        J=2
C        I=1
         F12(K,J)= VQ(K,1,1)*FL(1,2)+VQ(K,1,2)*FL(2,2)+VQ(K,1,3)*FL(3,2)
C        I=2
         F22(K,J)= VQ(K,2,1)*FL(1,2)+VQ(K,2,2)*FL(2,2)+VQ(K,2,3)*FL(3,2)
C        I=3
         F32(K,J)= VQ(K,3,1)*FL(1,2)+VQ(K,3,2)*FL(2,2)+VQ(K,3,3)*FL(3,2)
C
C        J=3
C        I=1
         F13(K,J)= VQ(K,1,1)*FL(1,3)+VQ(K,1,2)*FL(2,3)+VQ(K,1,3)*FL(3,3)
C        I=2
         F23(K,J)= VQ(K,2,1)*FL(1,3)+VQ(K,2,2)*FL(2,3)+VQ(K,2,3)*FL(3,3)
C        I=3
         F33(K,J)= VQ(K,3,1)*FL(1,3)+VQ(K,3,2)*FL(2,3)+VQ(K,3,3)*FL(3,3)
C
C        J=4
C        I=1
         F14(K,J)= VQ(K,1,1)*FL(1,4)+VQ(K,1,2)*FL(2,4)+VQ(K,1,3)*FL(3,4)
C        I=2
         F24(K,J)= VQ(K,2,1)*FL(1,4)+VQ(K,2,2)*FL(2,4)+VQ(K,2,3)*FL(3,4)
C        I=3
         F34(K,J)= VQ(K,3,1)*FL(1,4)+VQ(K,3,2)*FL(2,4)+VQ(K,3,3)*FL(3,4)
C       
                
        ENDDO 
#include "vectorize.inc"
        DO EP=NPLAT0+1,JLT 
          K=IPLAT(EP)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
          Z1 = COREL(K,3,1)
          AR(1)= -Z1*(VF(K,2,J) - VF(K,5,J) + VF(K,8,J)-VF(K,11,J))
     1                          +  COREL(K,2,1)*VF(K,3,J)
     2                          +  COREL(K,2,2)*VF(K,6,J)
     3                          +  COREL(K,2,3)*VF(K,9,J)
     4                          +  COREL(K,2,4)*VF(K,12,J)
          AR(2)=  Z1*(VF(K,1,J)-VF(K,4,J)+VF(K,7,J)-VF(K,10,J))
     1                         - COREL(K,1,1)*VF(K,3,J)
     2                         - COREL(K,1,2)*VF(K,6,J)
     3                         - COREL(K,1,3)*VF(K,9,J)
     4                         - COREL(K,1,4)*VF(K,12,J)
          AR(3)=-COREL(K,2,1)*VF(K,1,J)+COREL(K,1,1)*VF(K,2,J)
     1          -COREL(K,2,2)*VF(K,4,J)+COREL(K,1,2)*VF(K,5,J)
     2          -COREL(K,2,3)*VF(K,7,J)+COREL(K,1,3)*VF(K,8,J)
     3          -COREL(K,2,4)*VF(K,10,J)+COREL(K,1,4)*VF(K,11,J)
C
          ALR(1) =DI(K,1)*AR(1)+DI(K,4)*AR(2)+DI(K,5)*AR(3)
          ALR(2) =DI(K,4)*AR(1)+DI(K,2)*AR(2)+DI(K,6)*AR(3)
          ALR(3) =DI(K,5)*AR(1)+DI(K,6)*AR(2)+DI(K,3)*AR(3)
C
          C1 =Z1*ALR(2)
          VF(K,1,J )= VF(K,1,J)  - C1+COREL(K,2,1)*ALR(3)
          VF(K,4,J )= VF(K,4,J)  + C1+COREL(K,2,2)*ALR(3)
          VF(K,7,J )= VF(K,7,J)  - C1+COREL(K,2,3)*ALR(3)
          VF(K,10,J)= VF(K,10,J) + C1+COREL(K,2,4)*ALR(3)
C
          C1 =Z1*ALR(1)
          VF(K,2,J)= VF(K,2,J) +C1-COREL(K,1,1)*ALR(3)
          VF(K,5,J)= VF(K,5,J) -C1-COREL(K,1,2)*ALR(3)
          VF(K,8,J)= VF(K,8,J) +C1-COREL(K,1,3)*ALR(3)
          VF(K,11,J)= VF(K,11,J)-C1-COREL(K,1,4)*ALR(3)
C
          VF(K,3,J) = VF(K,3,J) -COREL(K,2,1)*ALR(1)+COREL(K,1,1)*ALR(2)
          VF(K,6,J) = VF(K,6,J) -COREL(K,2,2)*ALR(1)+COREL(K,1,2)*ALR(2)
          VF(K,9,J) = VF(K,9,J) -COREL(K,2,3)*ALR(1)+COREL(K,1,3)*ALR(2)
          VF(K,12,J)= VF(K,12,J)-COREL(K,2,4)*ALR(1)+COREL(K,1,4)*ALR(2)

C        I=1
         F11(K,J)= VQ(K,1,1)*VF(K,1,J) + VQ(K,1,2)*VF(K,2,J)
     1                                 + VQ(K,1,3)*VF(K,3,J)
C        I=2
         F21(K,J)=  VQ(K,2,1)*VF(K,1,J)  + VQ(K,2,2)*VF(K,2,J)
     1                                   + VQ(K,2,3)*VF(K,3,J)
C        I=3  
         F31(K,J)= VQ(K,3,1)*VF(K,1,J)  + VQ(K,3,2)*VF(K,2,J)
     1                                  + VQ(K,3,3)*VF(K,3,J)
C
C        J=2
C        I=1
         F12(K,J)= VQ(K,1,1)*VF(K,4,J) + VQ(K,1,2)*VF(K,5,J)
     1                                 + VQ(K,1,3)*VF(K,6,J)
C        I=2
         F22(K,J)= VQ(K,2,1)*VF(K,4,J) + VQ(K,2,2)*VF(K,5,J)
     1                                 + VQ(K,2,3)*VF(K,6,J)
C        I=3
         F32(K,J)= VQ(K,3,1)*VF(K,4,J) + VQ(K,3,2)*VF(K,5,J)
     1                                 + VQ(K,3,3)*VF(K,6,J)
C
C        J=3
C        I=1
         F13(K,J)= VQ(K,1,1)*VF(K,7,J) + VQ(K,1,2)*VF(K,8,J)
     1                                 + VQ(K,1,3)*VF(K,9,J)
C        I=2
         F23(K,J)= VQ(K,2,1)*VF(K,7,J) + VQ(K,2,2)*VF(K,8,J)
     1                                 + VQ(K,2,3)*VF(K,9,J)
C        I=3
         F33(K,J)= VQ(K,3,1)*VF(K,7,J) + VQ(K,3,2)*VF(K,8,J)
     1                                 + VQ(K,3,3)*VF(K,9,J)
C
C        J=4
C        I=1
         F14(K,J)= VQ(K,1,1)*VF(K,10,J)+ VQ(K,1,2)*VF(K,11,J)
     1                                 + VQ(K,1,3)*VF(K,12,J)
C        I=2
         F24(K,J)= VQ(K,2,1)*VF(K,10,J)+ VQ(K,2,2)*VF(K,11,J)
     1                                 + VQ(K,2,3)*VF(K,12,J)
C        I=3
         F34(K,J)= VQ(K,3,1)*VF(K,10,J)+ VQ(K,3,2)*VF(K,11,J)
     1                                 + VQ(K,3,3)*VF(K,12,J)
       ENDDO
      ENDDO
C
C     Projection des forces d'interpli
       DO J = 1 , NPT 
#include "vectorize.inc"
        DO EP=JFT,JLT  
         K=IPLAT(EP)

         FL1 = VFI(K,1,J)
         FL2 = VFI(K,2,J)
         FL3 = VFI(K,3,J)
C transformation au repere globale
         F1= VQ(K,1,1)*FL1  + VQ(K,1,2)*FL2
     1                               + VQ(K,1,3)*FL3
         F2=  VQ(K,2,1)*FL1  + VQ(K,2,2)*FL2
     1                               + VQ(K,2,3)*FL3
         F3= VQ(K,3,1)*FL1  + VQ(K,3,2)*FL2
     1                               + VQ(K,3,3)*FL3  
CNode 1 - ply
         F11(K,J)= F11(K,J) + F1 
         F21(K,J)= F21(K,J) + F2
         F31(K,J)= F31(K,J) + F3
CNode 2 -ply 
         FL1 = VFI(K,4,J)
         FL2 = VFI(K,5,J)
         FL3 = VFI(K,6,J)
C transformation au repere globale
         F1= VQ(K,1,1)*FL1  + VQ(K,1,2)*FL2
     1                               + VQ(K,1,3)*FL3
         F2=  VQ(K,2,1)*FL1  + VQ(K,2,2)*FL2
     1                               + VQ(K,2,3)*FL3
         F3= VQ(K,3,1)*FL1  + VQ(K,3,2)*FL2
     1                               + VQ(K,3,3)*FL3  
         F12(K,J)= F12(K,J) + F1 
         F22(K,J)= F22(K,J) + F2
         F32(K,J)= F32(K,J) + F3
CC
         FL1 = VFI(K,7,J)
         FL2 = VFI(K,8,J)
         FL3 = VFI(K,9,J)
C transformation au repere globale
         F1= VQ(K,1,1)*FL1  + VQ(K,1,2)*FL2
     1                               + VQ(K,1,3)*FL3
         F2=  VQ(K,2,1)*FL1  + VQ(K,2,2)*FL2
     1                               + VQ(K,2,3)*FL3
         F3= VQ(K,3,1)*FL1  + VQ(K,3,2)*FL2
     1                               + VQ(K,3,3)*FL3  
CNode 3 - ply
         F13(K,J)= F13(K,J) + F1 
         F23(K,J)= F23(K,J) + F2
         F33(K,J)= F33(K,J) + F3
CNode 4 -ply 
         FL1 = VFI(K,10,J)
         FL2 = VFI(K,11,J)
         FL3 = VFI(K,12,J)
C transformation au repere globale
         F1= VQ(K,1,1)*FL1  + VQ(K,1,2)*FL2
     1                               + VQ(K,1,3)*FL3
         F2=  VQ(K,2,1)*FL1  + VQ(K,2,2)*FL2
     1                               + VQ(K,2,3)*FL3
         F3= VQ(K,3,1)*FL1  + VQ(K,3,2)*FL2
     1                               + VQ(K,3,3)*FL3  
         F14(K,J)= F14(K,J) + F1 
         F24(K,J)= F24(K,J) + F2
         F34(K,J)= F34(K,J) + F3
C
CNode 1 - ply
         F11(K,J)= F11(K,J)*OFF(K) 
         F21(K,J)= F21(K,J)*OFF(K)
         F31(K,J)= F31(K,J)*OFF(K)
CNode 2 - ply
         F12(K,J)= F12(K,J)*OFF(K)
         F22(K,J)= F22(K,J)*OFF(K)
         F32(K,J)= F32(K,J)*OFF(K)
CNode 3 - ply
         F13(K,J)= F13(K,J)*OFF(K) 
         F23(K,J)= F23(K,J)*OFF(K)
         F33(K,J)= F33(K,J)*OFF(K)
CNode 4 - ply
         F14(K,J)= F14(K,J)*OFF(K) 
         F24(K,J)= F24(K,J)*OFF(K)
         F34(K,J)= F34(K,J)*OFF(K)                           
         
        ENDDO 
       ENDDO 
      RETURN
      END
