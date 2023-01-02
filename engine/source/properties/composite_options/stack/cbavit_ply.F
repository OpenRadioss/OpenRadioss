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
C   |============================================================
C   |  CBALYVIT                          /coqueba/cbalyvit.F
C   |------------------------------------------------------------
C   |-- appele -----------
C   |-- appelee par -----------
C   |         CBAFORC3                         /coqueba/cbaforc3.F
C   |============================================================
	SUBROUTINE CBAVIT_PLY(JFT,JLT,IXC,OFFG,OFF,NPLAT,IPLAT,NPT,
     1                        VCORE,DI,ZL,VQ , VXYZ,X13_T  ,X24_T ,
     2                        Y13_T,Y24_T,AREA,INOD,DEL_PLY,
     .                        VNI,ISTACK,VR)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE PLYXFEM_MOD
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C	transformation au repere local est absolue
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXC(NIXC,*),JFT,JLT,NNOD,NPLAT,IPLAT(*),NPT,
     .        ISTACK(MVSIZ,NPT),INOD(*)
      my_real
     .   OFFG(*),OFF(*)
      PARAMETER (NNOD = 4)
      my_real 
     .   VCORE(MVSIZ,3,NNOD),VXYZ(MVSIZ,3*NNOD,NPT),
     .   VQ(MVSIZ,3,3),ZL(*),DI(MVSIZ,6),
     .   Y24_T(*),X13_T(*),X24_T(*),Y13_T(*),AREA(*),
     .   VNI(4,4), DEL_PLY(MVSIZ,12,NPT), VR(3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER J,I,K,EP,NN(4), IPLY,IP,NPLAT0
      INTEGER L,M,J1,J2
      my_real
     .   PG,Z1,Z2,MX13,MY13,MX23,MY23,MX34,MY34,GAMA1,GAMA2, X21, 
     .   X34,Y21,Y34 ,Z21,Z34,L12,L34,X41,X32,Y41,Y32,Z41,Z32,XX1
     .   YY,XY,XZ1,YZ ,ZZ,Y24,X24,Y13,X13,COREL(3,4),XX1,YY,OFF_L,
     .   D1,D2,DT05,DT025,EXZ,EYZ,DDRY,V13X,V24X,VHIX,DDRZ1,DDRZ2,
     .   DDRZ,DDRX,A1,A2,D3
      my_real 
     .  VG13(3),VG24(3),VGHI(3),V13(MVSIZ,3), V24(MVSIZ,3),
     .  VHI(MVSIZ,3), AR(3),D(6),ALR(3),RR(3,NNOD),
     .  AREA_I(MVSIZ), DEL_IPLY(MVSIZ,3,NPT-1),DN_IPLY(MVSIZ,12,NPT-1),
     .  DN_PLY(MVSIZ,12,NPT)
      DATA   PG/.577350269189626/
C-----------------------------------------------
!!        NPLAT0 = JLT
        NPLAT0 = NPLAT
	A1 = ONE - PG
        A2 = ONE + PG
C (-PG, -PG)        
        VNI(1,1)= FOURTH*A2*A2
	VNI(2,1)= FOURTH*A1*A2
	VNI(3,1)= FOURTH*A1*A1
	VNI(4,1)= VNI(2,1)
C (xi = PG,eta=-PG)
        VNI(1,2)= VNI(2,1)
	VNI(2,2)= VNI(1,1)
	VNI(3,2)= VNI(2,1)
	VNI(4,2)= VNI(3,1)
C  xi = PG, eta = PG 
        VNI(1,3)= VNI(3,1)
	VNI(2,3)= VNI(2,1)
	VNI(3,3)= VNI(1,1)
	VNI(4,3)= VNI(2,1)
Cxi= -PG, eta= pG
        VNI(1,4)= VNI(2,1)
	VNI(2,4)= VNI(3,1)
	VNI(3,4)= VNI(2,1)
	VNI(4,4)= VNI(1,1)
C        
      DO EP=JFT,JLT
        AREA_I(EP)=ONE/MAX(EM20,AREA(EP))
      ENDDO
C
      DO J=1, NPT
       DO EP=JFT,JLT
         IP = ISTACK(EP,J)
         
          NN(1) = INOD(IXC(2,EP))
          NN(2) = INOD(IXC(3,EP))
          NN(3) = INOD(IXC(4,EP))
          NN(4) = INOD(IXC(5,EP)) 
C          
          VG13(1)=PLY(IP)%V(1,NN(1)) - PLY(IP)%V(1,NN(3))
          VG24(1)=PLY(IP)%V(1,NN(2)) - PLY(IP)%V(1,NN(4))
          VGHI(1)=PLY(IP)%V(1,NN(1)) - PLY(IP)%V(1,NN(2)) 
     .          + PLY(IP)%V(1,NN(3)) - PLY(IP)%V(1,NN(4))
C
          VG13(2)=PLY(IP)%V(2,NN(1)) - PLY(IP)%V(2,NN(3))
          VG24(2)=PLY(IP)%V(2,NN(2)) - PLY(IP)%V(2,NN(4))
          VGHI(2)=PLY(IP)%V(2,NN(1)) - PLY(IP)%V(2,NN(2)) 
     .           +PLY(IP)%V(2,NN(3)) - PLY(IP)%V(2,NN(4))
C
          VG13(3)= PLY(IP)%V(3,NN(1)) - PLY(IP)%V(3,NN(3))
          VG24(3)= PLY(IP)%V(3,NN(2)) - PLY(IP)%V(3,NN(4))
          VGHI(3)= PLY(IP)%V(3,NN(1)) - PLY(IP)%V(3,NN(2))
     .           + PLY(IP)%V(3,NN(3)) - PLY(IP)%V(3,NN(4))
C
          V13(EP,1) =(VQ(EP,1,1)*VG13(1)+VQ(EP,2,1)*VG13(2)
     1              +VQ(EP,3,1)*VG13(3))
          V24(EP,1)=(VQ(EP,1,1)*VG24(1)+VQ(EP,2,1)*VG24(2)
     1              +VQ(EP,3,1)*VG24(3))
          VHI(EP,1)=(VQ(EP,1,1)*VGHI(1)+VQ(EP,2,1)*VGHI(2)
     1              +VQ(EP,3,1)*VGHI(3))
          V13(EP,2)=(VQ(EP,1,2)*VG13(1)+VQ(EP,2,2)*VG13(2)
     1              +VQ(EP,3,2)*VG13(3))
          V24(EP,2)=(VQ(EP,1,2)*VG24(1)+VQ(EP,2,2)*VG24(2)
     1              +VQ(EP,3,2)*VG24(3))
          VHI(EP,2)=(VQ(EP,1,2)*VGHI(1)+VQ(EP,2,2)*VGHI(2)
     1              +VQ(EP,3,2)*VGHI(3))
          V13(EP,3)=(VQ(EP,1,3)*VG13(1)+VQ(EP,2,3)*VG13(2)
     1              +VQ(EP,3,3)*VG13(3))
          V24(EP,3)=(VQ(EP,1,3)*VG24(1)+VQ(EP,2,3)*VG24(2)
     1              +VQ(EP,3,3)*VG24(3))
          VHI(EP,3)=(VQ(EP,1,3)*VGHI(1)+VQ(EP,2,3)*VGHI(2)
     1              +VQ(EP,3,3)*VGHI(3))
     
         ENDDO 
C ---
       IF (ISHV51>51.AND.IMPL_S==0) THEN
          DT05 = HALF*DT1
          DT025 =FOURTH*DT1
          DO I=JFT,JLT
             EXZ =   Y24_T(I)*V13(I,3)-Y13_T(I)*V24(I,3)
             EYZ =  -X24_T(I)*V13(I,3)+X13_T(I)*V24(I,3)
             DDRY=DT05*EXZ*AREA_I(I)
             DDRX=DT05*EYZ*AREA_I(I)
             V13X = V13(I,1)
             V24X = V24(I,1)
             VHIX = VHI(I,1)
             DDRZ1=DT025*(V13(I,2)-V24(I,2))/(X13_T(I)-X24_T(I))
            IF (ABS(X13_T(I)-X24_T(I))<EM10) DDRZ1 = ZERO
             V13(I,1) = V13(I,1)-DDRY*V13(I,3)-DDRZ1*V13(I,2)
             V24(I,1) = V24(I,1)-DDRY*V24(I,3)-DDRZ1*V24(I,2)
             VHI(I,1) = VHI(I,1)-DDRY*VHI(I,3)-DDRZ1*VHI(I,2)
             DDRZ2=DT025*(V13X+V24X)/(Y13_T(I)+Y24_T(I))
            IF (ABS(Y13_T(I)+Y24_T(I))<EM10) DDRZ2 = ZERO
             V13(I,2) = V13(I,2)-DDRX*V13(I,3)-DDRZ2*V13X
             V24(I,2) = V24(I,2)-DDRX*V24(I,3)-DDRZ2*V24X
             VHI(I,2) = VHI(I,2)-DDRX*VHI(I,3)-DDRZ2*VHIX
           ENDDO
         ENDIF 
C ---
#include "vectorize.inc"
      DO I=JFT,NPLAT0                
        EP =IPLAT(I)               
        VXYZ(EP,1,J)=V13(EP,1)   
        VXYZ(EP,2,J)=V13(EP,2)   
        VXYZ(EP,3,J)=V13(EP,3)   
C
        VXYZ(EP,4,J)=V24(EP,1)   
        VXYZ(EP,5,J)=V24(EP,2)   
        VXYZ(EP,6,J)=V24(EP,3)   
C
        VXYZ(EP,7,J)=VHI(EP,1)   
        VXYZ(EP,8,J)=VHI(EP,2)   
        VXYZ(EP,9,J)=VHI(EP,3)           
C               
      ENDDO                        
#include "vectorize.inc"
      DO I=NPLAT0+1,JLT
       EP =IPLAT(I)
       Z1=ZL(EP)
       Z2=Z1*Z1
C

        X13 =(VCORE(EP,1,1)-VCORE(EP,1,3))*HALF
        X24 =(VCORE(EP,1,2)-VCORE(EP,1,4))*HALF
        Y13 =(VCORE(EP,2,1)-VCORE(EP,2,3))*HALF
        Y24 =(VCORE(EP,2,2)-VCORE(EP,2,4))*HALF
        MX13=(VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
        MY13=(VCORE(EP,2,1)+VCORE(EP,2,3))*HALF

C--------------------------
         AR(1)=-Z1*VHI(EP,2)+Y13*V13(EP,3)+Y24*V24(EP,3)+MY13*VHI(EP,3)
         AR(2)= Z1*VHI(EP,1)-X13*V13(EP,3)-X24*V24(EP,3)-MX13*VHI(EP,3)
         AR(3)= X13*V13(EP,2)+X24*V24(EP,2)+MX13*VHI(EP,2)
     1        -Y13*V13(EP,1)-Y24*V24(EP,1)-MY13*VHI(EP,1)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
         ALR(1) =DI(EP,1)*AR(1)+DI(EP,4)*AR(2)+DI(EP,5)*AR(3)
         ALR(2) =DI(EP,4)*AR(1)+DI(EP,2)*AR(2)+DI(EP,6)*AR(3)
         ALR(3) =DI(EP,5)*AR(1)+DI(EP,6)*AR(2)+DI(EP,3)*AR(3)
C
         V13(EP,1)= HALF*V13(EP,1)+ALR(3)*Y13
         V24(EP,1)= HALF*V24(EP,1)+ALR(3)*Y24
         VHI(EP,1)= FOURTH*VHI(EP,1)+(ALR(3)*MY13-Z1*ALR(2))
         V13(EP,2)= HALF*V13(EP,2)-ALR(3)*X13
         V24(EP,2)= HALF*V24(EP,2)-ALR(3)*X24
         VHI(EP,2)= FOURTH*VHI(EP,2)-(ALR(3)*MX13-Z1*ALR(1))
         V13(EP,3)= HALF*V13(EP,3)-(Y13*ALR(1)-X13*ALR(2))
         V24(EP,3)= HALF*V24(EP,3)-(Y24*ALR(1)-X24*ALR(2))
         VHI(EP,3)= FOURTH*VHI(EP,3)+(MX13*ALR(2)-MY13*ALR(1))
C
         VXYZ(EP,1 ,J) =  V13(EP,1) + VHI(EP,1)
         VXYZ(EP,4 ,J) =  V24(EP,1) - VHI(EP,1)
         VXYZ(EP,7 ,J) = -V13(EP,1) + VHI(EP,1)
         VXYZ(EP,10,J) = -V24(EP,1) - VHI(EP,1)
C
         VXYZ(EP,2 ,J) =  V13(EP,2) + VHI(EP,2)
         VXYZ(EP,5 ,J) =  V24(EP,2) - VHI(EP,2)
         VXYZ(EP,8 ,J) = -V13(EP,2) + VHI(EP,2)
         VXYZ(EP,11,J) = -V24(EP,2) - VHI(EP,2)
C
         VXYZ(EP,3 ,J) =  V13(EP,3) + VHI(EP,3)
         VXYZ(EP,6 ,J) =  V24(EP,3) - VHI(EP,3)
         VXYZ(EP,9 ,J) = -V13(EP,3) + VHI(EP,3)
         VXYZ(EP,12,J) = -V24(EP,3) - VHI(EP,3)
        ENDDO
      ENDDO
C----------------------------
C---------Facteur pour longueur caracteristique---
C----------------------------
      OFF_L = 0.
      DO EP=JFT,JLT
        OFF(EP) = MIN(ONE,ABS(OFFG(EP)))
        OFF_L  = MIN(OFF_L,OFFG(EP))
      ENDDO
      IF(OFF_L < ZERO)THEN
       DO J=1,NPT
        DO EP=JFT,JLT
         IF(OFFG(EP) < ZERO)THEN
          VXYZ(EP,1 ,J)= ZERO
          VXYZ(EP,2 ,J)= ZERO
          VXYZ(EP,3 ,J)= ZERO
          VXYZ(EP,4 ,J)= ZERO
          VXYZ(EP,5 ,J)= ZERO
          VXYZ(EP,6 ,J)= ZERO
          VXYZ(EP,7 ,J)= ZERO
          VXYZ(EP,8 ,J)= ZERO
          VXYZ(EP,9 ,J)= ZERO
          VXYZ(EP,10,J)= ZERO
          VXYZ(EP,11,J)= ZERO
          VXYZ(EP,12,J)= ZERO
         ENDIF
        ENDDO
       ENDDO  
      ENDIF
C ---
C  les deplacements nodaux dans le rep re local
      DO J=1, NPT
       DO EP=JFT,JLT
         IP = ISTACK(EP,J)
         NN(1) = INOD(IXC(2,EP))
         NN(2) = INOD(IXC(3,EP))
         NN(3) = INOD(IXC(4,EP))
         NN(4) = INOD(IXC(5,EP))             
C node 1 layer J
         D1 =  PLY(IP)%U(1,NN(1))                                      
         D2 =  PLY(IP)%U(2,NN(1))                                      
         D3 =  PLY(IP)%U(3,NN(1))                                      
         DN_PLY(EP,1,J)=VQ(EP,1,1)*D1 + VQ(EP,2,1)*D2 + VQ(EP,3,1)*D3   
         DN_PLY(EP,2,J)=VQ(EP,1,2)*D1 + VQ(EP,2,2)*D2 + VQ(EP,3,2)*D3          
         DN_PLY(EP,3,J)=VQ(EP,1,3)*D1 + VQ(EP,2,3)*D2 + VQ(EP,3,3)*D3
C  node 2 layer J           
         D1 =  PLY(IP)%U(1,NN(2))                                      
         D2 =  PLY(IP)%U(2,NN(2))                                      
         D3 =  PLY(IP)%U(3,NN(2))                                      
         DN_PLY(EP,4,J)=VQ(EP,1,1)*D1 + VQ(EP,2,1)*D2 + VQ(EP,3,1)*D3   
         DN_PLY(EP,5,J)=VQ(EP,1,2)*D1 + VQ(EP,2,2)*D2 + VQ(EP,3,2)*D3          
         DN_PLY(EP,6,J)=VQ(EP,1,3)*D1 + VQ(EP,2,3)*D2 + VQ(EP,3,3)*D3
C    node 3 layer J        
         D1 =  PLY(IP)%U(1,NN(3))                                      
         D2 =  PLY(IP)%U(2,NN(3))                                      
         D3 =  PLY(IP)%U(3,NN(3))                                      
         DN_PLY(EP,7,J)=VQ(EP,1,1)*D1 + VQ(EP,2,1)*D2 + VQ(EP,3,1)*D3   
         DN_PLY(EP,8,J)=VQ(EP,1,2)*D1 + VQ(EP,2,2)*D2 + VQ(EP,3,2)*D3          
         DN_PLY(EP,9,J)=VQ(EP,1,3)*D1 + VQ(EP,2,3)*D2 + VQ(EP,3,3)*D3
C  node 4 layer J           
         D1 =  PLY(IP)%U(1,NN(4))                                      
         D2 =  PLY(IP)%U(2,NN(4))                                      
         D3 =  PLY(IP)%U(3,NN(4))                                      
         DN_PLY(EP,10,J)=VQ(EP,1,1)*D1 + VQ(EP,2,1)*D2 + VQ(EP,3,1)*D3   
         DN_PLY(EP,11,J)=VQ(EP,1,2)*D1 + VQ(EP,2,2)*D2 + VQ(EP,3,2)*D3          
         DN_PLY(EP,12,J)=VQ(EP,1,3)*D1 + VQ(EP,2,3)*D2 + VQ(EP,3,3)*D3 
        ENDDO
       ENDDO  
C  glissement elementaire
      DO J=1, NPT
       DO EP=JFT,JLT
         DEL_PLY(EP,1,J) = (
     .       DN_PLY(EP,1,J)*VNI(1,1) + DN_PLY(EP,4,J)*VNI(2,1) +
     .       DN_PLY(EP,7,J)*VNI(3,1) + DN_PLY(EP,10,J)*VNI(4,1) )
         DEL_PLY(EP,2,J) = (
     .       DN_PLY(EP,2,J)*VNI(1,1) + DN_PLY(EP,5,J)*VNI(2,1) +
     .       DN_PLY(EP,8,J)*VNI(3,1) + DN_PLY(EP,11,J)*VNI(4,1) )       
         DEL_PLY(EP,3,J) = (
     .       DN_PLY(EP,3,J)*VNI(1,1) + DN_PLY(EP,6,J)*VNI(2,1) +
     .       DN_PLY(EP,9,J)*VNI(3,1) + DN_PLY(EP,12,J)*VNI(4,1) )
C 2nd gauss p
         DEL_PLY(EP,4,J) = (
     .       DN_PLY(EP,1,J)*VNI(1,2) + DN_PLY(EP,4,J)*VNI(2,2) +
     .       DN_PLY(EP,7,J)*VNI(3,2) + DN_PLY(EP,10,J)*VNI(4,2) )
         DEL_PLY(EP,5,J) = (
     .       DN_PLY(EP,2,J)*VNI(1,2) + DN_PLY(EP,5,J)*VNI(2,2) +
     .       DN_PLY(EP,8,J)*VNI(3,2) + DN_PLY(EP,11,J)*VNI(4,2) )       
         DEL_PLY(EP,6,J) = (
     .       DN_PLY(EP,3,J)*VNI(1,2) + DN_PLY(EP,6,J)*VNI(2,2) +
     .       DN_PLY(EP,9,J)*VNI(3,2) + DN_PLY(EP,12,J)*VNI(4,2) )
c 3rd      
         DEL_PLY(EP,7,J) = (
     .       DN_PLY(EP,1,J)*VNI(1,3) + DN_PLY(EP,4,J)*VNI(2,3) +
     .       DN_PLY(EP,7,J)*VNI(3,3) + DN_PLY(EP,10,J)*VNI(4,3) )
         DEL_PLY(EP,8,J) = (
     .       DN_PLY(EP,2,J)*VNI(1,3) + DN_PLY(EP,5,J)*VNI(2,3) +
     .       DN_PLY(EP,8,J)*VNI(3,3) + DN_PLY(EP,11,J)*VNI(4,3) )
         DEL_PLY(EP,9,J) = (
     .       DN_PLY(EP,3,J)*VNI(1,3) + DN_PLY(EP,6,J)*VNI(2,3) +
     .       DN_PLY(EP,9,J)*VNI(3,3) + DN_PLY(EP,12,J)*VNI(4,3) )
C 4th     
         DEL_PLY(EP,10,J) = (
     .       DN_PLY(EP,1,J)*VNI(1,4) + DN_PLY(EP,4,J)*VNI(2,4) +
     .       DN_PLY(EP,7,J)*VNI(3,4) + DN_PLY(EP,10,J)*VNI(4,4) )
         DEL_PLY(EP,11,J) = (
     .       DN_PLY(EP,2,J)*VNI(1,4) + DN_PLY(EP,5,J)*VNI(2,4) +
     .       DN_PLY(EP,8,J)*VNI(3,4) + DN_PLY(EP,11,J)*VNI(4,4) )
         DEL_PLY(EP,12,J) = (
     .       DN_PLY(EP,3,J)*VNI(1,4) + DN_PLY(EP,6,J)*VNI(2,4) +
     .       DN_PLY(EP,9,J)*VNI(3,4) + DN_PLY(EP,12,J)*VNI(4,4) )    
         ENDDO 
        ENDDO
C ---
	RETURN
	END
