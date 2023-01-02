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
Chd|  CBAPROJ                       source/elements/shell/coqueba/cbaproj.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBAPROJ(
     1            JFT    ,JLT    ,VQN    ,VQ     ,VF    ,
     2            VM     ,NPLAT   ,IPLAT   ,
     3            F11    ,F12    ,F13    ,F14    ,F21   ,
     4            F22    ,F23    ,F24    ,F31    ,F32   ,
     5            F33    ,F34    ,M11    ,M12    ,M13   ,
     6            M14    ,M21    ,M22    ,M23    ,M24   ,
     7            M31    ,M32    ,M33    ,M34    ,COREL ,
     8            DI     ,VMZ    ,ISROT  ,OFF   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        TRANSMET LES FORCES INTERNES LOCALES VF,VM ---> GLOBALES FIJ ,MIJ
C        ENTREES : 
C        SORTIES : FIJ,MIJ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT   ,IPLAT(*),ISROT
      my_real 
     .   VQN(MVSIZ,9,4),VF(MVSIZ,3,4),VM(MVSIZ,2,4),VQ(MVSIZ,3,3),
     .   COREL(MVSIZ,3,4),DI(MVSIZ,6),VMZ(MVSIZ,4)
      my_real
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),
     .   OFF(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,EP
      my_real 
     .     MM(3,4),FL(3,4),ML(2,4),C1,Z1,
     .     AR(3),AD(4),ALR(3),ALD(4),DBAD(3),MLZ(MVSIZ,3,4)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO EP=JFT,NPLAT 
        K=IPLAT(EP)
C      I=1
        FL(1,1)= VF(K,1,1)+VF(K,1,3)
        FL(1,2)= VF(K,1,2)+VF(K,1,4)
        FL(1,3)=-VF(K,1,1)+VF(K,1,3)
        FL(1,4)=-VF(K,1,2)+VF(K,1,4)
C      I=2
        FL(2,1)= VF(K,2,1)+VF(K,2,3)
        FL(2,2)= VF(K,2,2)+VF(K,2,4)
        FL(2,3)=-VF(K,2,1)+VF(K,2,3)
        FL(2,4)=-VF(K,2,2)+VF(K,2,4)
C      I=3
        FL(3,1)= VF(K,3,1)+VF(K,3,3)
        FL(3,2)= VF(K,3,2)+VF(K,3,4)
        FL(3,3)=-VF(K,3,1)+VF(K,3,3)
        FL(3,4)=-VF(K,3,2)+VF(K,3,4)
C
C      I=1
        ML(1,1)= VM(K,1,1)+VM(K,1,3)
        ML(1,2)= VM(K,1,2)+VM(K,1,4)
        ML(1,3)=-VM(K,1,1)+VM(K,1,3)
        ML(1,4)=-VM(K,1,2)+VM(K,1,4)
C      I=2
        ML(2,1)= VM(K,2,1)+VM(K,2,3)
        ML(2,2)= VM(K,2,2)+VM(K,2,4)
        ML(2,3)=-VM(K,2,1)+VM(K,2,3)
        ML(2,4)=-VM(K,2,2)+VM(K,2,4)
C---------------------------------------
C   TRANS LOCAL-->GLOBAL ET 5DDL-->6DDL
C---------------------------------------
C
C        J=1
C        I=1
         F11(K)= VQ(K,1,1)*FL(1,1)+VQ(K,1,2)*FL(2,1)+VQ(K,1,3)*FL(3,1)
         M11(K)= VQ(K,1,1)*ML(1,1)+VQ(K,1,2)*ML(2,1)
C        I=2
         F21(K)= VQ(K,2,1)*FL(1,1)+VQ(K,2,2)*FL(2,1)+VQ(K,2,3)*FL(3,1)
         M21(K)= VQ(K,2,1)*ML(1,1)+VQ(K,2,2)*ML(2,1)
C        I=3
         F31(K)= VQ(K,3,1)*FL(1,1)+VQ(K,3,2)*FL(2,1)+VQ(K,3,3)*FL(3,1)
         M31(K)= VQ(K,3,1)*ML(1,1)+VQ(K,3,2)*ML(2,1)
C
C        J=2
C        I=1
         F12(K)= VQ(K,1,1)*FL(1,2)+VQ(K,1,2)*FL(2,2)+VQ(K,1,3)*FL(3,2)
         M12(K)= VQ(K,1,1)*ML(1,2)+VQ(K,1,2)*ML(2,2)
C        I=2
         F22(K)= VQ(K,2,1)*FL(1,2)+VQ(K,2,2)*FL(2,2)+VQ(K,2,3)*FL(3,2)
         M22(K)= VQ(K,2,1)*ML(1,2)+VQ(K,2,2)*ML(2,2)
C        I=3
         F32(K)= VQ(K,3,1)*FL(1,2)+VQ(K,3,2)*FL(2,2)+VQ(K,3,3)*FL(3,2)
         M32(K)= VQ(K,3,1)*ML(1,2)+VQ(K,3,2)*ML(2,2)
C
C        J=3
C        I=1
         F13(K)= VQ(K,1,1)*FL(1,3)+VQ(K,1,2)*FL(2,3)+VQ(K,1,3)*FL(3,3)
         M13(K)= VQ(K,1,1)*ML(1,3)+VQ(K,1,2)*ML(2,3)
C        I=2
         F23(K)= VQ(K,2,1)*FL(1,3)+VQ(K,2,2)*FL(2,3)+VQ(K,2,3)*FL(3,3)
         M23(K)= VQ(K,2,1)*ML(1,3)+VQ(K,2,2)*ML(2,3)
C        I=3
         F33(K)= VQ(K,3,1)*FL(1,3)+VQ(K,3,2)*FL(2,3)+VQ(K,3,3)*FL(3,3)
         M33(K)= VQ(K,3,1)*ML(1,3)+VQ(K,3,2)*ML(2,3)
C
C        J=4
C        I=1
         F14(K)= VQ(K,1,1)*FL(1,4)+VQ(K,1,2)*FL(2,4)+VQ(K,1,3)*FL(3,4)
         M14(K)= VQ(K,1,1)*ML(1,4)+VQ(K,1,2)*ML(2,4)
C        I=2
         F24(K)= VQ(K,2,1)*FL(1,4)+VQ(K,2,2)*FL(2,4)+VQ(K,2,3)*FL(3,4)
         M24(K)= VQ(K,2,1)*ML(1,4)+VQ(K,2,2)*ML(2,4)
C        I=3
         F34(K)= VQ(K,3,1)*FL(1,4)+VQ(K,3,2)*FL(2,4)+VQ(K,3,3)*FL(3,4)
         M34(K)= VQ(K,3,1)*ML(1,4)+VQ(K,3,2)*ML(2,4)
C
       ENDDO
C--------------QBAT has not simplification w/ wi -> don't need drilling dof projection        
       IF (ISROT>0) THEN
#include "vectorize.inc"
        DO EP=JFT,NPLAT
         K =IPLAT(EP)
         M11(K)= M11(K)+ VQ(K,1,3)*VMZ(EP,1)
         M21(K)= M21(K)+ VQ(K,2,3)*VMZ(EP,1)
         M31(K)= M31(K)+ VQ(K,3,3)*VMZ(EP,1)
C
         M12(K)= M12(K)+ VQ(K,1,3)*VMZ(EP,2)
         M22(K)= M22(K)+ VQ(K,2,3)*VMZ(EP,2)
         M32(K)= M32(K)+ VQ(K,3,3)*VMZ(EP,2)
C
         M13(K)= M13(K)+ VQ(K,1,3)*VMZ(EP,3)
         M23(K)= M23(K)+ VQ(K,2,3)*VMZ(EP,3)
         M33(K)= M33(K)+ VQ(K,3,3)*VMZ(EP,3)
C
         M14(K)= M14(K)+ VQ(K,1,3)*VMZ(EP,4)
         M24(K)= M24(K)+ VQ(K,2,3)*VMZ(EP,4)
         M34(K)= M34(K)+ VQ(K,3,3)*VMZ(EP,4)
        ENDDO
        DO J=1,4
#include "vectorize.inc"
        DO EP=NPLAT+1,JLT 
         K=IPLAT(EP)
         MLZ(EP,1,J)= VQN(K,7,J)*VMZ(EP,J)
         MLZ(EP,2,J)= VQN(K,8,J)*VMZ(EP,J)
         MLZ(EP,3,J)= VQN(K,9,J)*VMZ(EP,J)
        ENDDO
        ENDDO
       ELSE
        DO J=1,4
        DO EP=NPLAT+1,JLT 
         MLZ(EP,1,J)= ZERO
         MLZ(EP,2,J)= ZERO
         MLZ(EP,3,J)= ZERO
        ENDDO
        ENDDO
       END IF !(ISROT>0) THEN
C       
       DO EP=NPLAT+1,JLT 
        K=IPLAT(EP)
         MM(1,1)= VQN(K,1,1)*VM(K,1,1)+VQN(K,4,1)*VM(K,2,1)+MLZ(EP,1,1)
         MM(2,1)= VQN(K,2,1)*VM(K,1,1)+VQN(K,5,1)*VM(K,2,1)+MLZ(EP,2,1)
         MM(3,1)= VQN(K,3,1)*VM(K,1,1)+VQN(K,6,1)*VM(K,2,1)+MLZ(EP,3,1)
C        J=2
         MM(1,2)= VQN(K,1,2)*VM(K,1,2)+VQN(K,4,2)*VM(K,2,2)+MLZ(EP,1,2)
         MM(2,2)= VQN(K,2,2)*VM(K,1,2)+VQN(K,5,2)*VM(K,2,2)+MLZ(EP,2,2)
         MM(3,2)= VQN(K,3,2)*VM(K,1,2)+VQN(K,6,2)*VM(K,2,2)+MLZ(EP,3,2)
C        J=3
         MM(1,3)= VQN(K,1,3)*VM(K,1,3)+VQN(K,4,3)*VM(K,2,3)+MLZ(EP,1,3)
         MM(2,3)= VQN(K,2,3)*VM(K,1,3)+VQN(K,5,3)*VM(K,2,3)+MLZ(EP,2,3)
         MM(3,3)= VQN(K,3,3)*VM(K,1,3)+VQN(K,6,3)*VM(K,2,3)+MLZ(EP,3,3)
C        J=4
         MM(1,4)= VQN(K,1,4)*VM(K,1,4)+VQN(K,4,4)*VM(K,2,4)+MLZ(EP,1,4)
         MM(2,4)= VQN(K,2,4)*VM(K,1,4)+VQN(K,5,4)*VM(K,2,4)+MLZ(EP,2,4)
         MM(3,4)= VQN(K,3,4)*VM(K,1,4)+VQN(K,6,4)*VM(K,2,4)+MLZ(EP,3,4)
C---------free rigid mode projection-----------       
        Z1 = COREL(K,3,1)
        AR(1)= -Z1*(VF(K,2,1)-VF(K,2,2)+VF(K,2,3)-VF(K,2,4))
     1         +COREL(K,2,1)*VF(K,3,1)+MM(1,1)
     2         +COREL(K,2,2)*VF(K,3,2)+MM(1,2)
     3         +COREL(K,2,3)*VF(K,3,3)+MM(1,3)
     4         +COREL(K,2,4)*VF(K,3,4)+MM(1,4)
        AR(2)=  Z1*(VF(K,1,1)-VF(K,1,2)+VF(K,1,3)-VF(K,1,4))
     1          -COREL(K,1,1)*VF(K,3,1)+MM(2,1)
     2          -COREL(K,1,2)*VF(K,3,2)+MM(2,2)
     3          -COREL(K,1,3)*VF(K,3,3)+MM(2,3)
     4          -COREL(K,1,4)*VF(K,3,4)+MM(2,4)
        AR(3)=-COREL(K,2,1)*VF(K,1,1)+COREL(K,1,1)*VF(K,2,1)+MM(3,1)
     1        -COREL(K,2,2)*VF(K,1,2)+COREL(K,1,2)*VF(K,2,2)+MM(3,2)
     2        -COREL(K,2,3)*VF(K,1,3)+COREL(K,1,3)*VF(K,2,3)+MM(3,3)
     3        -COREL(K,2,4)*VF(K,1,4)+COREL(K,1,4)*VF(K,2,4)+MM(3,4)
C
          ALR(1) =DI(K,1)*AR(1)+DI(K,4)*AR(2)+DI(K,5)*AR(3)
          ALR(2) =DI(K,4)*AR(1)+DI(K,2)*AR(2)+DI(K,6)*AR(3)
          ALR(3) =DI(K,5)*AR(1)+DI(K,6)*AR(2)+DI(K,3)*AR(3)
C
          C1 =Z1*ALR(2)
          VF(K,1,1)= VF(K,1,1)-C1+COREL(K,2,1)*ALR(3)
          VF(K,1,2)= VF(K,1,2)+C1+COREL(K,2,2)*ALR(3)
          VF(K,1,3)= VF(K,1,3)-C1+COREL(K,2,3)*ALR(3)
          VF(K,1,4)= VF(K,1,4)+C1+COREL(K,2,4)*ALR(3)
C
          C1 =Z1*ALR(1)
          VF(K,2,1)= VF(K,2,1)+C1-COREL(K,1,1)*ALR(3)
          VF(K,2,2)= VF(K,2,2)-C1-COREL(K,1,2)*ALR(3)
          VF(K,2,3)= VF(K,2,3)+C1-COREL(K,1,3)*ALR(3)
          VF(K,2,4)= VF(K,2,4)-C1-COREL(K,1,4)*ALR(3)
C
         DO J=1,4
          VF(K,3,J)= VF(K,3,J)-COREL(K,2,J)*ALR(1)+COREL(K,1,J)*ALR(2)
          MM(1,J)= MM(1,J)-ALR(1)
          MM(2,J)= MM(2,J)-ALR(2)
          MM(3,J)= MM(3,J)-ALR(3)
         ENDDO
C        J=1
C        I=1
         F11(K)= VQ(K,1,1)*VF(K,1,1)+VQ(K,1,2)*VF(K,2,1)
     1           +VQ(K,1,3)*VF(K,3,1)
         M11(K)= VQ(K,1,1)*MM(1,1)+VQ(K,1,2)*MM(2,1)+VQ(K,1,3)*MM(3,1)
C        I=2
         F21(K)= VQ(K,2,1)*VF(K,1,1)+VQ(K,2,2)*VF(K,2,1)
     1           +VQ(K,2,3)*VF(K,3,1)
         M21(K)= VQ(K,2,1)*MM(1,1)+VQ(K,2,2)*MM(2,1)+VQ(K,2,3)*MM(3,1)
C        I=3
         F31(K)= VQ(K,3,1)*VF(K,1,1)+VQ(K,3,2)*VF(K,2,1)
     1           +VQ(K,3,3)*VF(K,3,1)
         M31(K)= VQ(K,3,1)*MM(1,1)+VQ(K,3,2)*MM(2,1)+VQ(K,3,3)*MM(3,1)
C
C        J=2
C        I=1
         F12(K)= VQ(K,1,1)*VF(K,1,2)+VQ(K,1,2)*VF(K,2,2)
     1           +VQ(K,1,3)*VF(K,3,2)
         M12(K)= VQ(K,1,1)*MM(1,2)+VQ(K,1,2)*MM(2,2)+VQ(K,1,3)*MM(3,2)
C        I=2
         F22(K)= VQ(K,2,1)*VF(K,1,2)+VQ(K,2,2)*VF(K,2,2)
     1           +VQ(K,2,3)*VF(K,3,2)
         M22(K)= VQ(K,2,1)*MM(1,2)+VQ(K,2,2)*MM(2,2)+VQ(K,2,3)*MM(3,2)
C        I=3
         F32(K)= VQ(K,3,1)*VF(K,1,2)+VQ(K,3,2)*VF(K,2,2)
     1           +VQ(K,3,3)*VF(K,3,2)
         M32(K)= VQ(K,3,1)*MM(1,2)+VQ(K,3,2)*MM(2,2)+VQ(K,3,3)*MM(3,2)
C
C        J=3
C        I=1
         F13(K)= VQ(K,1,1)*VF(K,1,3)+VQ(K,1,2)*VF(K,2,3)
     1           +VQ(K,1,3)*VF(K,3,3)
         M13(K)= VQ(K,1,1)*MM(1,3)+VQ(K,1,2)*MM(2,3)+VQ(K,1,3)*MM(3,3)
C        I=2
         F23(K)= VQ(K,2,1)*VF(K,1,3)+VQ(K,2,2)*VF(K,2,3)
     1           +VQ(K,2,3)*VF(K,3,3)
         M23(K)= VQ(K,2,1)*MM(1,3)+VQ(K,2,2)*MM(2,3)+VQ(K,2,3)*MM(3,3)
C        I=3
         F33(K)= VQ(K,3,1)*VF(K,1,3)+VQ(K,3,2)*VF(K,2,3)
     1           +VQ(K,3,3)*VF(K,3,3)
         M33(K)= VQ(K,3,1)*MM(1,3)+VQ(K,3,2)*MM(2,3)+VQ(K,3,3)*MM(3,3)
C
C        J=4
C        I=1
         F14(K)= VQ(K,1,1)*VF(K,1,4)+VQ(K,1,2)*VF(K,2,4)
     1           +VQ(K,1,3)*VF(K,3,4)
         M14(K)= VQ(K,1,1)*MM(1,4)+VQ(K,1,2)*MM(2,4)+VQ(K,1,3)*MM(3,4)
C        I=2
         F24(K)= VQ(K,2,1)*VF(K,1,4)+VQ(K,2,2)*VF(K,2,4)
     1           +VQ(K,2,3)*VF(K,3,4)
         M24(K)= VQ(K,2,1)*MM(1,4)+VQ(K,2,2)*MM(2,4)+VQ(K,2,3)*MM(3,4)
C        I=3
         F34(K)= VQ(K,3,1)*VF(K,1,4)+VQ(K,3,2)*VF(K,2,4)
     1           +VQ(K,3,3)*VF(K,3,4)
         M34(K)= VQ(K,3,1)*MM(1,4)+VQ(K,3,2)*MM(2,4)+VQ(K,3,3)*MM(3,4)
C
       ENDDO
C
      DO EP=JFT,JLT
C      
         F11(EP)=F11(EP)*OFF(EP)
         F21(EP)=F21(EP)*OFF(EP)
         F31(EP)=F31(EP)*OFF(EP)
C
         F12(EP)=F12(EP)*OFF(EP)
         F22(EP)=F22(EP)*OFF(EP)
         F32(EP)=F32(EP)*OFF(EP)
C         
         F13(EP)= F13(EP)*OFF(EP)
         F23(EP)= F23(EP)*OFF(EP)
         F33(EP)= F33(EP)*OFF(EP)
C         
         F14(EP)= F14(EP)*OFF(EP)
         F24(EP)= F24(EP)*OFF(EP)
         F34(EP)= F34(EP)*OFF(EP)
         
         M11(EP)=M11(EP)*OFF(EP)
         M21(EP)=M21(EP)*OFF(EP)
         M31(EP)=M31(EP)*OFF(EP)
C
         M12(EP)=M12(EP)*OFF(EP)
         M22(EP)=M22(EP)*OFF(EP)
         M32(EP)=M32(EP)*OFF(EP)
C         
         M13(EP)= M13(EP)*OFF(EP)
         M23(EP)= M23(EP)*OFF(EP)
         M33(EP)= M33(EP)*OFF(EP)
C         
         M14(EP)= M14(EP)*OFF(EP)
         M24(EP)= M24(EP)*OFF(EP)
         M34(EP)= M34(EP)*OFF(EP) 

      ENDDO 
      
      RETURN
      END
