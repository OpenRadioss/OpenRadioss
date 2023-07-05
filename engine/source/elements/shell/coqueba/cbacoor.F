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
Chd|  CBACOOR                       source/elements/shell/coqueba/cbacoor.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
       SUBROUTINE CBACOOR(ELBUF_STR,JFT,JLT,X,V,
     .            VR,IXC,PM,OFFG,LL,
     1            AREA,VXYZ, RXYZ,VCORE,JAC,HX,HY,VKSI,VETA,
     2            VQN,VQG,VQ,VJFI,VNRM,VASTN,NPLAT,IPLAT,
     3            X13_T  ,X24_T  ,Y13_T,Y24_T,OFF,DI,NLAY, 
     4            IREP  ,NPT   ,ISMSTR,NEL   ,ISROT ,
     5            SMSTR ,DIR_A ,DIR_B ,FACN  ,ZL1   ,
     6            R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,
     7            R31   ,R32   ,R33   ,INOD  ,RLZ   ,
     8            THK   ,IPLYCXFEM    ,UX1   ,UX2   ,UX3   ,
     9            UX4   ,UY1   ,UY2   ,UY3   ,UY4   ,
     A            VL1   ,VL2   ,VL3   ,VL4   ,XL2   ,
     B            XL3   ,XL4   ,YL2   ,YL3   ,YL4   ,XLCOR ,NPINCH)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NNOD,NPLAT,IREP,NPT,NLAY,ISMSTR,ISROT,IPLYCXFEM,NEL,NPINCH
      INTEGER IXC(NIXC,*),IPLAT(*),INOD(*)
      PARAMETER (NNOD = 4)
      my_real X(3,*), V(3,*), VR(3,*),PM(NPROPM,*),OFFG(*),OFF(*),
     .   VCORE(MVSIZ,3,NNOD),VXYZ(MVSIZ,3,NNOD),AREA(*),VJFI(MVSIZ,6,4),
     .   VQN(MVSIZ,9,NNOD),VQG(MVSIZ,9,NNOD),VQ(MVSIZ,3,3),RXYZ(MVSIZ,2,NNOD),
     .   LL(*),VASTN(MVSIZ,4,NNOD),VNRM(MVSIZ,3,NNOD),
     .   JAC(MVSIZ,4),HX(MVSIZ,4),HY(MVSIZ,4),VETA(4,4),VKSI(4,4),Y24_T(*),
     .   X13_T(*),X24_T(*),Y13_T(*),OFF_L, DI(MVSIZ,6),
     .   DIR_A(NEL,*),DIR_B(NEL,*),FACN(MVSIZ,2),
     .   ZL1(MVSIZ),R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),
     .   R33(MVSIZ),RLZ(MVSIZ,4),THK(MVSIZ),
     .   UX1(*),UX2(*),UX3(*),UX4(*),UY1(*),UY2(*),UY3(*),UY4(*),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),YL3(MVSIZ),YL4(MVSIZ),
     .   XLCOR(MVSIZ,2,3)
      DOUBLE PRECISION
     .  SMSTR(*)
C
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
c FUNCTION: premilitary compute for QBAT
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT            - element id limit
c  I   X, V, VR(3,NUMNOD) - coordinate,velocity, rotational velocity in global system
c  I   IXC(NIXC,*NEL)     - element connectivity of shell (and other data)
c  I   PM(NPROPM,MID)     - input Material data
c  I   OFFG(NEL),OFF(NEL) - element activation flag, local flag
c  O   LL(NEL),FACN(2,NEL)- characteristic length (for dt compute)
c  O   AREA(NEL),NTP      - Area, num. of integrating points in thickness
c  O   VCORE(MVSIZ,3,4)   - coordinates of 4 nodes in local system
c                           BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
c                           for plat element
c  O   VXYZ(3,4,NEL)      - nodal velocity (4 nodes) in local system
C                           V13->VXYZ(,1,),V24->VXYZ(,2,),VHI->VXYZ(,3,)------------
c                           for plat element
c  O   RXYZ(NEL,2,4)      - nodal rotational velocity (4 nodes) in local system 5 dof
C                           R13->RXYZ(,1,),R24->RXYZ(,2,),RHI->RXYZ(,3,),RTI->RXYZ(,4,)----
c                           for plat element
c  O  JAC,HX,HY(4,NEL)    - Jacobien,asymmetric part(hourglass) at 4 Gauss points
c  O   VKSI,VETA(4,4)     - derivation of shape functions------
c  O  VQ(NEL,9),VQN,VQG(NEL,9,4) - local system of element, nodal(4) and Gauss points(4)
c  O  VJFI,VNRM,VASTN     - terms used for assumed strains
c  O  NPLAT,IPLAT(NEL)    - num. of plat element and indice
c  O  Xij_T,Yij_T(NEL)    - [B] components used for in-plane shear assumed strain
c  IO XiS,YiS,ZiS(NEL)    - reference configurations used for small strain options
c  I  ISMSTR,IREP         - small strain and orthotrop flags 
c  O  DIR_A,DIR_B(2,NEL)  - actual orthotropic directions
c  O  ZL1(NEL)            - warped length
c  O  Rij(NEL)            - local system base
c  I  IDRIL               - drilling dof flag
c  O  RLZ(4,NEL)          - nodal Rz (rotational velocity) in local system 
c                          (used only Idril active )
c  I  INOD(NUMNOD)        - Node numbers for PLYXFEM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,II(6),J,K,L,M,I1,I2,I3,I4,EP,SPLAT,JJ
      INTEGER NN(4),JPLAT(MVSIZ)
      MY_REAL 
     .   XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
      MY_REAL
     .   C1,C2,L13(MVSIZ),L24(MVSIZ),
     .   AA,VV(3,NNOD),RR(3,NNOD)
      MY_REAL
     .    TOL,PG,J0,J1,J2,DETA,COREL(3,4),X1,Y1,S1
      MY_REAL
     .    X13,X24,Y13,MX13,MX23,MX34,MY13,Y13_2,Z1,Z2,GAMA1,GAMA2,
     .    X21,X34,Y21,Y34,Z21,Z34,X41,X32,Y41,Y32,Z_2,Z41,Z32,L12,L34,
     .    A_4,SL,SZ2,SZ,JMX13,JMY13,JMZ13,J2MYZ,LM(MVSIZ),Y24,Y24_2,
     .    MY23,MY34,SCAL,G1X1,G1X3,G1Y1,G1Y3,G2X1,G2X2,G2Y1,G2Y2,
     .   AR(3),AD(NNOD),BTB(6),XX1,YY,ZZ,XY,XZ1,YZ,D(6), 
     .   ALR(3),ALD(NNOD),DBAD(3),ABC,XXYZ2,ZZXY2,YYXZ2
      my_real 
     .   LXYZ0(3),DETA1(MVSIZ),RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),
     .   XX,AREA_I(MVSIZ),SSZ(MVSIZ)
C
      my_real 
     .   VG13(3),VG24(3),VGHI(3),V13(MVSIZ,3),V24(MVSIZ,3),VHI(MVSIZ,3),
     .   FACI,FAC1,FAC2,
     .   DT05,DT025,EXZ,EYZ,DDRX,DDRY,V13X,V24X,VHIX,DDRZ1,DDRZ2,DDRZ,
     .    VNX1, VNY1, VNZ1,VNX2,VNY2,VNZ2,VNX3,VNY3,VNZ3,VNX4,VNY4,VNZ4,
     .    NORM
        DATA   PG/.577350269189626/
C=======================================================================
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
C
      TOL=EM12
      IF (ISROT > 0  ) TOL=EM8
C
       VKSI(1,1)=-FOURTH*(ONE+PG)
       VKSI(2,1)=-VKSI(1,1)
       VKSI(3,1)= FOURTH*(ONE-PG)
       VKSI(4,1)=-VKSI(3,1)
       VETA(1,1)=-FOURTH*(ONE+PG)
       VETA(2,1)=-FOURTH*(ONE-PG)
       VETA(3,1)=-VETA(2,1)
       VETA(4,1)=-VETA(1,1)
       VKSI(1,2)= VKSI(1,1)
       VKSI(2,2)=-VKSI(1,2)
       VKSI(3,2)= VKSI(3,1)
       VKSI(4,2)=-VKSI(3,2)
       VETA(1,2)= VETA(2,1)
       VETA(2,2)= VETA(1,1)
       VETA(3,2)=-VETA(2,2)
       VETA(4,2)=-VETA(1,2)
       VKSI(1,3)=-VKSI(3,1)
       VKSI(2,3)=-VKSI(1,3)
       VKSI(3,3)=-VKSI(1,1)
       VKSI(4,3)=-VKSI(3,3)
       VETA(1,3)= VETA(1,2)
       VETA(2,3)= VETA(2,2)
       VETA(3,3)=-VETA(2,3)
       VETA(4,3)=-VETA(1,3)
       VKSI(1,4)= VKSI(1,3)
       VKSI(2,4)=-VKSI(1,4)
       VKSI(3,4)= VKSI(3,3)
       VKSI(4,4)=-VKSI(3,4)
       VETA(1,4)= VETA(1,1)
       VETA(2,4)= VETA(2,1)
       VETA(3,4)=-VETA(2,4)
       VETA(4,4)=-VETA(1,4)
C
      DO I=JFT,JLT
        RX(I)=X(1,IXC(3,I))+X(1,IXC(4,I))-X(1,IXC(2,I))-X(1,IXC(5,I))
        SX(I)=X(1,IXC(4,I))+X(1,IXC(5,I))-X(1,IXC(2,I))-X(1,IXC(3,I))
        RY(I)=X(2,IXC(3,I))+X(2,IXC(4,I))-X(2,IXC(2,I))-X(2,IXC(5,I))
        SY(I)=X(2,IXC(4,I))+X(2,IXC(5,I))-X(2,IXC(2,I))-X(2,IXC(3,I))
        RZ(I)=X(3,IXC(3,I))+X(3,IXC(4,I))-X(3,IXC(2,I))-X(3,IXC(5,I))
        SSZ(I)=X(3,IXC(4,I))+X(3,IXC(5,I))-X(3,IXC(2,I))-X(3,IXC(3,I))
      ENDDO 
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      K = 0
      CALL CLSKEW3(JFT,JLT,K,
     .   RX, RY, RZ, 
     .   SX, SY, SSZ, 
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,DETA1,OFFG )
      DO I=JFT,JLT
        AREA(I)=FOURTH*DETA1(I)
        AREA_I(I)=ONE/AREA(I)
        VQ(I,1,1)=R11(I)
        VQ(I,2,1)=R21(I)
        VQ(I,3,1)=R31(I)
        VQ(I,1,2)=R12(I)
        VQ(I,2,2)=R22(I)
        VQ(I,3,2)=R32(I)
        VQ(I,1,3)=R13(I)
        VQ(I,2,3)=R23(I)
        VQ(I,3,3)=R33(I)
      ENDDO 
c
      DO I=JFT,JLT

        J=IXC(2,I)
        K=IXC(3,I)
        L=IXC(4,I)
        M=IXC(5,I)

        LXYZ0(1)=FOURTH*(X(1,L)+X(1,M)+X(1,J)+X(1,K))
        LXYZ0(2)=FOURTH*(X(2,L)+X(2,M)+X(2,J)+X(2,K))
        LXYZ0(3)=FOURTH*(X(3,L)+X(3,M)+X(3,J)+X(3,K))

        XX1=X(1,K)-X(1,J)
        YY1=X(2,K)-X(2,J)
        ZZ1=X(3,K)-X(3,J)

        XL2(I)=R11(I)*XX1+R21(I)*YY1+R31(I)*ZZ1
        YL2(I)=R12(I)*XX1+R22(I)*YY1+R32(I)*ZZ1

        XX2=X(1,J)-LXYZ0(1)
        YY2=X(2,J)-LXYZ0(2)
        ZZ2=X(3,J)-LXYZ0(3)
        ZL1(I)=R13(I)*XX2+R23(I)*YY2+R33(I)*ZZ2

        XX3=X(1,L)-X(1,J)
        YY3=X(2,L)-X(2,J)
        ZZ3=X(3,L)-X(3,J)
        XL3(I)=R11(I)*XX3+R21(I)*YY3+R31(I)*ZZ3
        YL3(I)=R12(I)*XX3+R22(I)*YY3+R32(I)*ZZ3

        XX4=X(1,M)-X(1,J)
        YY4=X(2,M)-X(2,J)
        ZZ4=X(3,M)-X(3,J)
        XL4(I)=R11(I)*XX4+R21(I)*YY4+R31(I)*ZZ4
        YL4(I)=R12(I)*XX4+R22(I)*YY4+R32(I)*ZZ4
      ENDDO
C----------------------------
C     SMALL STRAIN
C----------------------------
      IF(ISMSTR==10)THEN
       DO I=JFT,JLT
        XLCOR(I,1,1)=XL2(I)
        XLCOR(I,2,1)=YL2(I)
        XLCOR(I,1,2)=XL3(I)
        XLCOR(I,2,2)=YL3(I)
        XLCOR(I,1,3)=XL4(I)
        XLCOR(I,2,3)=YL4(I)
       ENDDO
      ELSEIF(ISMSTR==11)THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I))==ONE)OFFG(I)=SIGN(TWO,OFFG(I))
          UX1(I) = ZERO
          UY1(I) = ZERO
          UX2(I) = ZERO
          UY2(I) = ZERO
          UX3(I) = ZERO
          UY3(I) = ZERO
          UX4(I) = ZERO
          UY4(I) = ZERO
          IF(ABS(OFFG(I))==TWO)THEN
            UX2(I) = XL2(I)-SMSTR(II(1)+I)
            UY2(I) = YL2(I)-SMSTR(II(2)+I)
            UX3(I) = XL3(I)-SMSTR(II(3)+I)
            UY3(I) = YL3(I)-SMSTR(II(4)+I)
            UX4(I) = XL4(I)-SMSTR(II(5)+I)
            UY4(I) = YL4(I)-SMSTR(II(6)+I)
            XL2(I) = SMSTR(II(1)+I)
            YL2(I) = SMSTR(II(2)+I)
            XL3(I) = SMSTR(II(3)+I)
            YL3(I) = SMSTR(II(4)+I)
            XL4(I) = SMSTR(II(5)+I)
            YL4(I) = SMSTR(II(6)+I)
            ZL1(I) = ZERO
            AREA(I)=HALF*((XL2(I)-XL4(I))*YL3(I)-XL3(I)*(YL2(I)-YL4(I)))
            AREA_I(I)=ONE/MAX(EM20,AREA(I))
          ELSE
            SMSTR(II(1)+I)=XL2(I)
            SMSTR(II(2)+I)=YL2(I)
            SMSTR(II(3)+I)=XL3(I)
            SMSTR(II(4)+I)=YL3(I)
            SMSTR(II(5)+I)=XL4(I)
            SMSTR(II(6)+I)=YL4(I)
          ENDIF
        ENDDO
      ELSEIF(ISMSTR==1.OR.ISMSTR==2)THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I))==TWO)THEN
            XL2(I)=SMSTR(II(1)+I)
            YL2(I)=SMSTR(II(2)+I)
            XL3(I)=SMSTR(II(3)+I)
            YL3(I)=SMSTR(II(4)+I)
            XL4(I)=SMSTR(II(5)+I)
            YL4(I)=SMSTR(II(6)+I)
            ZL1(I)=ZERO
            AREA(I)=HALF*((XL2(I)-XL4(I))*YL3(I)-XL3(I)*(YL2(I)-YL4(I)))
            AREA_I(I)=ONE/MAX(EM20,AREA(I))
          ELSE
            SMSTR(II(1)+I)=XL2(I)
            SMSTR(II(2)+I)=YL2(I)
            SMSTR(II(3)+I)=XL3(I)
            SMSTR(II(4)+I)=YL3(I)
            SMSTR(II(5)+I)=XL4(I)
            SMSTR(II(6)+I)=YL4(I)
         ENDIF
        ENDDO
      ENDIF
      IF(ISMSTR==1)THEN
        DO I=JFT,JLT
            IF(OFFG(I)==ONE)OFFG(I)=TWO
        ENDDO
      ENDIF
C----------------------------
C     ORTHOTROPY
C----------------------------
        CALL CORTDIR3(ELBUF_STR,DIR_A  ,DIR_B ,JFT    ,JLT    ,
     .                NLAY     ,IREP   ,RX    ,RY     ,RZ     , 
     .                SX       ,SY     ,SSZ   ,R11    ,R21    ,
     .                R31      ,R12    ,R22   ,R32    ,NEL    )
C----------------------------
      DO EP=JFT,JLT
        LXYZ0(1)=FOURTH*(XL2(EP)+XL3(EP)+XL4(EP))
        LXYZ0(2)=FOURTH*(YL2(EP)+YL3(EP)+YL4(EP))
        VCORE(EP,1,1)=-LXYZ0(1)
        VCORE(EP,1,2)=XL2(EP)-LXYZ0(1)
        VCORE(EP,1,3)=XL3(EP)-LXYZ0(1)
        VCORE(EP,1,4)=XL4(EP)-LXYZ0(1)
        VCORE(EP,2,1)=-LXYZ0(2)
        VCORE(EP,2,2)=YL2(EP)-LXYZ0(2)
        VCORE(EP,2,3)=YL3(EP)-LXYZ0(2)
        VCORE(EP,2,4)=YL4(EP)-LXYZ0(2)
C
        X13_T(EP) =(VCORE(EP,1,1)-VCORE(EP,1,3))*HALF
        X24_T(EP) =(VCORE(EP,1,2)-VCORE(EP,1,4))*HALF
        Y13_T(EP) =(VCORE(EP,2,1)-VCORE(EP,2,3))*HALF
        Y24_T(EP) =(VCORE(EP,2,2)-VCORE(EP,2,4))*HALF
        L13(EP)=X13_T(EP)*X13_T(EP)+Y13_T(EP)*Y13_T(EP)
        L24(EP)=X24_T(EP)*X24_T(EP)+Y24_T(EP)*Y24_T(EP)
        LL(EP)=MAX(L13(EP),L24(EP))
        C1 =VCORE(EP,1,2)*VCORE(EP,2,4)-VCORE(EP,2,2)*VCORE(EP,1,4)
        C2 =VCORE(EP,1,1)*VCORE(EP,2,3)-VCORE(EP,2,1)*VCORE(EP,1,3)
        LM(EP) =MAX(ABS(C1),ABS(C2))
      ENDDO 
c
      DO EP=JFT,JLT

        NN(1)=IXC(2,EP)
        NN(2)=IXC(3,EP)
        NN(3)=IXC(4,EP)
        NN(4)=IXC(5,EP)

        VL1(EP,1)=V(1,NN(1))
        VL1(EP,2)=V(2,NN(1))
        VL1(EP,3)=V(3,NN(1))
        VL2(EP,1)=V(1,NN(2))
        VL2(EP,2)=V(2,NN(2))
        VL2(EP,3)=V(3,NN(2))
        VL3(EP,1)=V(1,NN(3))
        VL3(EP,2)=V(2,NN(3))
        VL3(EP,3)=V(3,NN(3))
        VL4(EP,1)=V(1,NN(4))
        VL4(EP,2)=V(2,NN(4))
        VL4(EP,3)=V(3,NN(4))

        VG13(1)=VL1(EP,1)-VL3(EP,1)
        VG24(1)=VL2(EP,1)-VL4(EP,1)
        VGHI(1)=VL1(EP,1)-VL2(EP,1)+VL3(EP,1)-VL4(EP,1)
C
        VG13(2)=VL1(EP,2)-VL3(EP,2)
        VG24(2)=VL2(EP,2)-VL4(EP,2)
        VGHI(2)=VL1(EP,2)-VL2(EP,2)+VL3(EP,2)-VL4(EP,2)
C
        VG13(3)=VL1(EP,3)-VL3(EP,3)
        VG24(3)=VL2(EP,3)-VL4(EP,3)
        VGHI(3)=VL1(EP,3)-VL2(EP,3)+VL3(EP,3)-VL4(EP,3)
C
        V13(EP,1)=(VQ(EP,1,1)*VG13(1)+VQ(EP,2,1)*VG13(2)+VQ(EP,3,1)*VG13(3))
        V24(EP,1)=(VQ(EP,1,1)*VG24(1)+VQ(EP,2,1)*VG24(2)+VQ(EP,3,1)*VG24(3))
        VHI(EP,1)=(VQ(EP,1,1)*VGHI(1)+VQ(EP,2,1)*VGHI(2)+VQ(EP,3,1)*VGHI(3))
        V13(EP,2)=(VQ(EP,1,2)*VG13(1)+VQ(EP,2,2)*VG13(2)+VQ(EP,3,2)*VG13(3))
        V24(EP,2)=(VQ(EP,1,2)*VG24(1)+VQ(EP,2,2)*VG24(2)+VQ(EP,3,2)*VG24(3))
        VHI(EP,2)=(VQ(EP,1,2)*VGHI(1)+VQ(EP,2,2)*VGHI(2)+VQ(EP,3,2)*VGHI(3))
        V13(EP,3)=(VQ(EP,1,3)*VG13(1)+VQ(EP,2,3)*VG13(2)+VQ(EP,3,3)*VG13(3))
        V24(EP,3)=(VQ(EP,1,3)*VG24(1)+VQ(EP,2,3)*VG24(2)+VQ(EP,3,3)*VG24(3))
        VHI(EP,3)=(VQ(EP,1,3)*VGHI(1)+VQ(EP,2,3)*VGHI(2)+VQ(EP,3,3)*VGHI(3))
      ENDDO 
C
       IF (IMPL_S==0.OR.IMP_LR>0) THEN
        DT05 = HALF*DT1
        DT025 =FOURTH*DT1
        DO I=JFT,JLT
         EXZ = Y24_T(I)*V13(I,3)-Y13_T(I)*V24(I,3)
         EYZ = -X24_T(I)*V13(I,3)+X13_T(I)*V24(I,3)
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
      NPLAT=JFT-1
      SPLAT= 0
C [PM] in order to make all shells as non-flat shells for pinching
C [PM] flat shells algorithm not implemented for pinching
      DO EP=JFT,JLT
       Z2=ZL1(EP)*ZL1(EP)
       IF ((Z2<TOL*LL(EP).OR.NPT==1).AND.NPINCH==0) THEN
        NPLAT=NPLAT+1
        IPLAT(NPLAT)=EP
       ELSE
        SPLAT=SPLAT+1
        JPLAT(SPLAT)=EP
       ENDIF
      ENDDO 
      DO EP=NPLAT+1,JLT
        IPLAT(EP)=JPLAT(EP-NPLAT)
      ENDDO 
#include "vectorize.inc"
      DO I=JFT,NPLAT
        EP =IPLAT(I)
        X13 =X13_T(EP)
        X24 =X24_T(EP)
        Y13 =Y13_T(EP)
        Y24 =Y24_T(EP)
C
        MX13=(VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
        MY13=(VCORE(EP,2,1)+VCORE(EP,2,3))*HALF
        MX23=(VCORE(EP,1,2)+VCORE(EP,1,3))*HALF
        MX34=(VCORE(EP,1,3)+VCORE(EP,1,4))*HALF
        MY23=(VCORE(EP,2,2)+VCORE(EP,2,3))*HALF
        MY34=(VCORE(EP,2,3)+VCORE(EP,2,4))*HALF
        X13_T(EP) =X13*AREA_I(EP)
        X24_T(EP) =X24*AREA_I(EP)
        Y13_T(EP) =Y13*AREA_I(EP)
        Y24_T(EP) =Y24*AREA_I(EP)
C--------GAMA(2)
        GAMA1=-MX13*Y24+MY13*X24
        GAMA2= MX13*Y13-MY13*X13
        Z2=ZL1(EP)*ZL1(EP)
        NN(1)=IXC(2,EP)
        NN(2)=IXC(3,EP)
        NN(3)=IXC(4,EP)
        NN(4)=IXC(5,EP)
C-------TRANSMET 3DDL ROTATIONS ----
        RR(1,1) =VQ(EP,1,1)*VR(1,NN(1))+VQ(EP,2,1)*VR(2,NN(1))+VQ(EP,3,1)*VR(3,NN(1))
        RR(1,2) =VQ(EP,1,1)*VR(1,NN(2))+VQ(EP,2,1)*VR(2,NN(2))+VQ(EP,3,1)*VR(3,NN(2))
        RR(1,3) =VQ(EP,1,1)*VR(1,NN(3))+VQ(EP,2,1)*VR(2,NN(3))+VQ(EP,3,1)*VR(3,NN(3))
        RR(1,4) =VQ(EP,1,1)*VR(1,NN(4))+VQ(EP,2,1)*VR(2,NN(4))+VQ(EP,3,1)*VR(3,NN(4))
        RR(2,1) =VQ(EP,1,2)*VR(1,NN(1))+VQ(EP,2,2)*VR(2,NN(1))+VQ(EP,3,2)*VR(3,NN(1))
        RR(2,2) =VQ(EP,1,2)*VR(1,NN(2))+VQ(EP,2,2)*VR(2,NN(2))+VQ(EP,3,2)*VR(3,NN(2))
        RR(2,3) =VQ(EP,1,2)*VR(1,NN(3))+VQ(EP,2,2)*VR(2,NN(3))+VQ(EP,3,2)*VR(3,NN(3))
        RR(2,4) =VQ(EP,1,2)*VR(1,NN(4))+VQ(EP,2,2)*VR(2,NN(4))+VQ(EP,3,2)*VR(3,NN(4))
C-------V13(I)->VXYZ(I,1),V24(I)->VXYZ(I,2),VHI(I)->VXYZ(I,3)------------
        VXYZ(EP,1,1)=V13(EP,1)
        VXYZ(EP,2,1)=V13(EP,2)
        VXYZ(EP,3,1)=V13(EP,3)
C
        VXYZ(EP,1,2)=V24(EP,1)
        VXYZ(EP,2,2)=V24(EP,2)
        VXYZ(EP,3,2)=V24(EP,3)
C
        VXYZ(EP,1,3)=VHI(EP,1)
        VXYZ(EP,2,3)=VHI(EP,2)
        VXYZ(EP,3,3)=VHI(EP,3)
C-------R13(I)->RXYZ(I,1),R24(I)->RXYZ(I,2),RHI(I)->RXYZ(I,3),RTI(I)->RXYZ(I,4)------------
        RXYZ(EP,1,1)=RR(1,1)-RR(1,3)
        RXYZ(EP,2,1)=RR(2,1)-RR(2,3)
        RXYZ(EP,1,2)=RR(1,2)-RR(1,4)
        RXYZ(EP,2,2)=RR(2,2)-RR(2,4)
        RXYZ(EP,1,3)=RR(1,1)-RR(1,2)+RR(1,3)-RR(1,4)
        RXYZ(EP,2,3)=RR(2,1)-RR(2,2)+RR(2,3)-RR(2,4)
        RXYZ(EP,1,4)=RR(1,1)+RR(1,2)+RR(1,3)+RR(1,4)
        RXYZ(EP,2,4)=RR(2,1)+RR(2,2)+RR(2,3)+RR(2,4)
C-------BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
C-------SONT STOCKE DANS VCORE---
        VCORE(EP,1,1)=Y24_T(EP)
        VCORE(EP,2,1)=-Y13_T(EP)
        VCORE(EP,3,1)=-X24_T(EP)
        VCORE(EP,1,2)= X13_T(EP)
        VCORE(EP,2,2)=GAMA1*AREA_I(EP)
        VCORE(EP,3,2)=GAMA2*AREA_I(EP)
        VCORE(EP,1,3)=MX23
        VCORE(EP,2,3)=MY23
        VCORE(EP,3,3)=MX34
        VCORE(EP,1,4)=MY34
        VCORE(EP,2,4)=MX13
        VCORE(EP,3,4)=MY13
C
        J1=(MX23*MY13-MX13*MY23)*PG
        J2=-(MX13*MY34-MX34*MY13)*PG
        J0=AREA(EP)*FOURTH
C----   JAC(jacobiens aux points d'integration)peut etre negative------        
        JAC(EP,1)=ABS(J0+J2-J1)
        JAC(EP,2)=ABS(J0+J2+J1)
        JAC(EP,3)=ABS(J0-J2+J1)
        JAC(EP,4)=ABS(J0-J2-J1)
        J1=(MY23-MY34)*PG
        J2=-(MY23+MY34)*PG
        HX(EP,1)=J1/JAC(EP,1)
        HX(EP,2)=J2/JAC(EP,2)
        HX(EP,3)=-J1/JAC(EP,3)
        HX(EP,4)=-J2/JAC(EP,4)
        J1=(MX34-MX23)*PG
        J2=(MX34+MX23)*PG
        HY(EP,1)=J1/JAC(EP,1)
        HY(EP,2)=J2/JAC(EP,2)
        HY(EP,3)=-J1/JAC(EP,3)
        HY(EP,4)=-J2/JAC(EP,4)
C
      ENDDO
#include "vectorize.inc"
      DO I=NPLAT+1,JLT
        EP =IPLAT(I)
        Z1=ZL1(EP)
        Z2=Z1*Z1
        VCORE(EP,3,1)=Z1 
        VCORE(EP,3,2)=-Z1 
        VCORE(EP,3,3)=Z1 
        VCORE(EP,3,4)=-Z1 
        COREL(1,1)=VCORE(EP,1,1) 
        COREL(1,2)=VCORE(EP,1,2) 
        COREL(1,3)=VCORE(EP,1,3) 
        COREL(1,4)=VCORE(EP,1,4)
        COREL(2,1)=VCORE(EP,2,1) 
        COREL(2,2)=VCORE(EP,2,2) 
        COREL(2,3)=VCORE(EP,2,3)
        COREL(2,4)=VCORE(EP,2,4)
        X13 =X13_T(EP)
        X24 =X24_T(EP)
        Y13 =Y13_T(EP)
        Y24 =Y24_T(EP)
cc        
        MX13=(VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
        MY13=(VCORE(EP,2,1)+VCORE(EP,2,3))*HALF
        MX23=(VCORE(EP,1,2)+VCORE(EP,1,3))*HALF
        MY23=(VCORE(EP,2,2)+VCORE(EP,2,3))*HALF
        MX34=(VCORE(EP,1,3)+VCORE(EP,1,4))*HALF
        MY34=(VCORE(EP,2,3)+VCORE(EP,2,4))*HALF      
        X13_T(EP) =X13*AREA_I(EP)
        X24_T(EP) =X24*AREA_I(EP)
        Y13_T(EP) =Y13*AREA_I(EP)
        Y24_T(EP) =Y24*AREA_I(EP)
C--------GAMA(2)
        GAMA1=-MX13*Y24+MY13*X24
        GAMA2= MX13*Y13-MY13*X13
        NN(1)=IXC(2,EP)
        NN(2)=IXC(3,EP)
        NN(3)=IXC(4,EP)
        NN(4)=IXC(5,EP)
C--------------------------
C     BUILD [F], [Q], [NM], [A-S], [AS] AT NODES
C--------------------------
C----------------------------------------------------
C  CALCULATION OF [F] 
C----------------------------------------------------
C--------- 
C   2A1
C---------
        X21 =MX23-MX13
        X34 =(COREL(1,3)-COREL(1,4))*HALF
        Y21 =MY23-MY13
        Y34 =(COREL(2,3)-COREL(2,4))*HALF
        Z21 = -Z1
        Z34 = Z1
        L12 = SQRT(X21*X21+Y21*Y21+Z2)
        L34 = SQRT(X34*X34+Y34*Y34+Z2)
C--------- 
C   2A2
C---------
        X41 =MX34-MX13
        X32 =(COREL(1,3)-COREL(1,2))*HALF
        Y41 =MY34-MY13
        Y32 =(COREL(2,3)-COREL(2,2))*HALF
        Z41 = -Z1
        Z32 = Z1
C---------- 
C    CALCULATION OF [QN] N=1,4 
C----------
        A_4=AREA(EP)*FOURTH
C---------- N =1----------
        SL=ONE/MAX(L12,EM20)
        VQN(EP,1,1)=X21*SL
        VQN(EP,2,1)=Y21*SL
        VQN(EP,3,1)=Z21*SL
        SZ2=A_4-GAMA1
        SZ=Z2*L24(EP)
        SL=ONE/SQRT(SZ+SZ2*SZ2)
        VQN(EP,7,1)=-Z1*Y24
        VQN(EP,8,1)= Z1*X24
        VQN(EP,9,1)= SZ2*SL
C
        VQN(EP,7,3)=-VQN(EP,7,1)
        VQN(EP,8,3)=-VQN(EP,8,1)
        VQN(EP,7,1)= VQN(EP,7,1)*SL
        VQN(EP,8,1)= VQN(EP,8,1)*SL
C
        VQN(EP,4,1)= VQN(EP,8,1)*VQN(EP,3,1)-VQN(EP,9,1)*VQN(EP,2,1)
        VQN(EP,5,1)= VQN(EP,9,1)*VQN(EP,1,1)-VQN(EP,7,1)*VQN(EP,3,1)
        VQN(EP,6,1)= VQN(EP,7,1)*VQN(EP,2,1)-VQN(EP,8,1)*VQN(EP,1,1)
C---------- N =3----------
        SL=ONE/MAX(L34,EM20)
        VQN(EP,1,3)=X34*SL
        VQN(EP,2,3)=Y34*SL
        VQN(EP,3,3)=Z34*SL
        SZ2=A_4+GAMA1
        SL=ONE/SQRT(SZ+SZ2*SZ2)
        VQN(EP,7,3)= VQN(EP,7,3)*SL
        VQN(EP,8,3)= VQN(EP,8,3)*SL
        VQN(EP,9,3)= SZ2*SL
C
        VQN(EP,4,3)= VQN(EP,8,3)*VQN(EP,3,3)-VQN(EP,9,3)*VQN(EP,2,3)
        VQN(EP,5,3)= VQN(EP,9,3)*VQN(EP,1,3)-VQN(EP,7,3)*VQN(EP,3,3)
        VQN(EP,6,3)= VQN(EP,7,3)*VQN(EP,2,3)-VQN(EP,8,3)*VQN(EP,1,3)
C---------- N =2----------
        VQN(EP,1,2)=VQN(EP,1,1)
        VQN(EP,2,2)=VQN(EP,2,1)
        VQN(EP,3,2)=VQN(EP,3,1)
        SZ2=A_4+GAMA2
        SZ=Z2*L13(EP)
        SL=ONE/SQRT(SZ+SZ2*SZ2)
        VQN(EP,7,2)=-Z1*Y13
        VQN(EP,8,2)= Z1*X13
        VQN(EP,9,2)= SZ2*SL
        VQN(EP,7,4)=-VQN(EP,7,2)
        VQN(EP,8,4)=-VQN(EP,8,2)
        VQN(EP,7,2)= VQN(EP,7,2)*SL
        VQN(EP,8,2)= VQN(EP,8,2)*SL
C
        VQN(EP,4,2)= VQN(EP,8,2)*VQN(EP,3,2)-VQN(EP,9,2)*VQN(EP,2,2)
        VQN(EP,5,2)= VQN(EP,9,2)*VQN(EP,1,2)-VQN(EP,7,2)*VQN(EP,3,2)
        VQN(EP,6,2)= VQN(EP,7,2)*VQN(EP,2,2)-VQN(EP,8,2)*VQN(EP,1,2)
C---------- N =4----------
        VQN(EP,1,4)=VQN(EP,1,3)
        VQN(EP,2,4)=VQN(EP,2,3)
        VQN(EP,3,4)=VQN(EP,3,3)
        SZ2=A_4-GAMA2
        SL=ONE/SQRT(SZ+SZ2*SZ2)
        VQN(EP,7,4)= VQN(EP,7,4)*SL
        VQN(EP,8,4)= VQN(EP,8,4)*SL
        VQN(EP,9,4)= SZ2*SL
C
        VQN(EP,4,4)= VQN(EP,8,4)*VQN(EP,3,4)-VQN(EP,9,4)*VQN(EP,2,4)
        VQN(EP,5,4)= VQN(EP,9,4)*VQN(EP,1,4)-VQN(EP,7,4)*VQN(EP,3,4)
        VQN(EP,6,4)= VQN(EP,7,4)*VQN(EP,2,4)-VQN(EP,8,4)*VQN(EP,1,4)
C        
!!         IF(ANIM_PLY < 0 ) THEN
!!            I1 = INOD(NN(1)) 
!!            I2 = INOD(NN(2))
!!            I3 = INOD(NN(3)) 
!!           I4 = INOD(NN(4))
C            
!!            NORM =SQRT(VQN(EP,7,1)**2 + VQN(EP,8,1)**2 + VQN(EP,9,1)**2) 
!!            VN_NOD(1,I1)=VN_NOD(1,I1)+VQN(EP,7,1)/MAX(NORM, EM20)
!!            VN_NOD(2,I1)=VN_NOD(2,I1)+VQN(EP,8,1)/MAX(NORM, EM20)
!!            VN_NOD(3,I1)=VN_NOD(3,I1)+VQN(EP,9,1)/MAX(NORM, EM20)
C
!!           NORM =SQRT(VQN(EP,7,2)**2 + VQN(EP,8,2)**2 + VQN(EP,9,2)**2)
!!            VN_NOD(1,I2)=VN_NOD(1,I2)+VQN(EP,7,2)/MAX(NORM, EM20)
!!            VN_NOD(2,I2)=VN_NOD(2,I2)+VQN(EP,8,2)/MAX(NORM, EM20)
!!            VN_NOD(3,I2)=VN_NOD(3,I2)+VQN(EP,9,2)/MAX(NORM, EM20)
C
!!            NORM =SQRT(VQN(EP,7,3)**2 + VQN(EP,8,3)**2 + VQN(EP,9,3)**2)
!!            VN_NOD(1,I3)=VN_NOD(1,I3)+VQN(EP,7,3)/MAX(NORM, EM20)
!!            VN_NOD(2,I3)=VN_NOD(2,I3)+VQN(EP,8,3)/MAX(NORM, EM20)
!!            VN_NOD(3,I3)=VN_NOD(3,I3)+VQN(EP,9,3)/MAX(NORM, EM20) 
c
!!            NORM =SQRT(VQN(EP,7,4)**2 + VQN(EP,8,4)**2 + VQN(EP,9,4)**2)
!!            VN_NOD(1,I4)=VN_NOD(1,I4)+VQN(EP,7,4)/MAX(NORM, EM20)
!!            VN_NOD(2,I4)=VN_NOD(2,I4)+VQN(EP,8,4)/MAX(NORM, EM20)
!!            VN_NOD(3,I4)=VN_NOD(3,I4)+VQN(EP,9,4)/MAX(NORM, EM20)
C     
!!         ENDIF      
C--------------------------------------------------
C   COMPUTE AS N (MIDDLE OF EDGE)
C--------------------------------------------------
C         J=1 COTE
        VNRM(EP,1,1)=VQN(EP,7,1)+VQN(EP,7,2)
        VNRM(EP,2,1)=VQN(EP,8,1)+VQN(EP,8,2)
        VNRM(EP,3,1)=VQN(EP,9,1)+VQN(EP,9,2)
        C1=SQRT(VNRM(EP,1,1)*VNRM(EP,1,1)+VNRM(EP,2,1)*VNRM(EP,2,1)+VNRM(EP,3,1)*VNRM(EP,3,1))
        C1 = MAX(EM20,C1)
        VNRM(EP,1,1)=VNRM(EP,1,1)/C1
        VNRM(EP,2,1)=VNRM(EP,2,1)/C1
        VNRM(EP,3,1)=VNRM(EP,3,1)/C1
        VASTN(EP,1,1)=ZERO
        VASTN(EP,2,1)=L12
        VASTN(EP,3,1)=VASTN(EP,1,1)
        VASTN(EP,4,1)=VASTN(EP,2,1)
C         J=2 
        VNRM(EP,1,2)=VQN(EP,7,4)+VQN(EP,7,3)
        VNRM(EP,2,2)=VQN(EP,8,4)+VQN(EP,8,3)
        VNRM(EP,3,2)=VQN(EP,9,4)+VQN(EP,9,3)
        C1=SQRT(VNRM(EP,1,2)*VNRM(EP,1,2)+VNRM(EP,2,2)*VNRM(EP,2,2)+VNRM(EP,3,2)*VNRM(EP,3,2))
        C1 = MAX(EM20,C1)
        VNRM(EP,1,2)=VNRM(EP,1,2)/C1
        VNRM(EP,2,2)=VNRM(EP,2,2)/C1
        VNRM(EP,3,2)=VNRM(EP,3,2)/C1
        VASTN(EP,1,2)=ZERO
        VASTN(EP,2,2)=L34
        VASTN(EP,3,2)=VASTN(EP,1,2)
        VASTN(EP,4,2)=VASTN(EP,2,2)
C         J=3 
        VNRM(EP,1,3)=VQN(EP,7,1)+VQN(EP,7,4)
        VNRM(EP,2,3)=VQN(EP,8,1)+VQN(EP,8,4)
        VNRM(EP,3,3)=VQN(EP,9,1)+VQN(EP,9,4)
        C1=SQRT(VNRM(EP,1,3)*VNRM(EP,1,3)+VNRM(EP,2,3)*VNRM(EP,2,3)+VNRM(EP,3,3)*VNRM(EP,3,3))
        C1 = MAX(EM20,C1)
        VNRM(EP,1,3)=VNRM(EP,1,3)/C1
        VNRM(EP,2,3)=VNRM(EP,2,3)/C1
        VNRM(EP,3,3)=VNRM(EP,3,3)/C1
        VASTN(EP,1,3)=-(X41*VQN(EP,4,1)+Y41*VQN(EP,5,1)+Z41*VQN(EP,6,1))
        VASTN(EP,2,3)= X41*VQN(EP,1,1)+Y41*VQN(EP,2,1)+Z41*VQN(EP,3,1)
        VASTN(EP,3,3)=-(X41*VQN(EP,4,4)+Y41*VQN(EP,5,4)+Z41*VQN(EP,6,4))
        VASTN(EP,4,3)= X41*VQN(EP,1,4)+Y41*VQN(EP,2,4)+Z41*VQN(EP,3,4)
C         J=4 
        VNRM(EP,1,4)=VQN(EP,7,2)+VQN(EP,7,3)
        VNRM(EP,2,4)=VQN(EP,8,2)+VQN(EP,8,3)
        VNRM(EP,3,4)=VQN(EP,9,2)+VQN(EP,9,3)
        C1=SQRT(VNRM(EP,1,4)*VNRM(EP,1,4)+VNRM(EP,2,4)*VNRM(EP,2,4)+VNRM(EP,3,4)*VNRM(EP,3,4))
        C1 = MAX(EM20,C1)
        VNRM(EP,1,4)=VNRM(EP,1,4)/C1
        VNRM(EP,2,4)=VNRM(EP,2,4)/C1
        VNRM(EP,3,4)=VNRM(EP,3,4)/C1
        VASTN(EP,1,4)=-(X32*VQN(EP,4,2)+Y32*VQN(EP,5,2)+Z32*VQN(EP,6,2))
        VASTN(EP,2,4)= X32*VQN(EP,1,2)+Y32*VQN(EP,2,2)+Z32*VQN(EP,3,2)
        VASTN(EP,3,4)=-(X32*VQN(EP,4,3)+Y32*VQN(EP,5,3)+Z32*VQN(EP,6,3))
        VASTN(EP,4,4)= X32*VQN(EP,1,3)+Y32*VQN(EP,2,3)+Z32*VQN(EP,3,3)
C---------- 
C    CALCULATION OF [QG] NG=1,4 
C----------
        A_4=A_4/PG
        JMX13=MX13*PG
        JMY13=MY13*PG
        JMZ13=Z1*PG
        J2MYZ=JMZ13*JMZ13
C---------- NG =1----------
        SZ2=A_4-GAMA1
        SZ=Z2*L24(EP)
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,1)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,1)=-Z1*Y24
        VQG(EP,8,1)= Z1*X24
        VQG(EP,9,1)= SZ2*SL
        VQG(EP,7,3)=-VQG(EP,7,1)
        VQG(EP,8,3)=-VQG(EP,8,1)
        VQG(EP,7,1)= VQG(EP,7,1)*SL
        VQG(EP,8,1)= VQG(EP,8,1)*SL
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
        G1X1=MX23-JMX13
        G1Y1=MY23-JMY13
        C1 = SQRT(G1X1*G1X1 + G1Y1*G1Y1 +J2MYZ )
        G2X1=MX34-JMX13 
        G2Y1=MY34-JMY13
        C2 = SQRT(G2X1*G2X1 + G2Y1*G2Y1 +J2MYZ)
        VJFI(EP,1,1)=(G2Y1*VQG(EP,9,1)+JMZ13*VQG(EP,8,1))
        VJFI(EP,2,1)=(-JMZ13*VQG(EP,7,1)-G2X1*VQG(EP,9,1))
        VJFI(EP,3,1)=(G2X1*VQG(EP,8,1)-G2Y1*VQG(EP,7,1))
        VQG(EP,1,1)= G1X1*C2+VJFI(EP,1,1)*C1
        VQG(EP,2,1)= G1Y1*C2+VJFI(EP,2,1)*C1
        VQG(EP,3,1)= -JMZ13*C2+VJFI(EP,3,1)*C1
        SL=SL/PG
        VJFI(EP,1,1)=VJFI(EP,1,1)*SL
        VJFI(EP,2,1)=VJFI(EP,2,1)*SL
        VJFI(EP,3,1)=VJFI(EP,3,1)*SL
C
        VJFI(EP,4,1)=(-JMZ13*VQG(EP,8,1)-G1Y1*VQG(EP,9,1))*SL
        VJFI(EP,5,1)=(G1X1*VQG(EP,9,1)+JMZ13*VQG(EP,7,1))*SL
        VJFI(EP,6,1)=(G1Y1*VQG(EP,7,1)-G1X1*VQG(EP,8,1))*SL
C
        SL = SQRT(VQG(EP,1,1)*VQG(EP,1,1) + VQG(EP,2,1)*VQG(EP,2,1) + VQG(EP,3,1)*VQG(EP,3,1))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,1) = VQG(EP,1,1)*SL
        VQG(EP,2,1) = VQG(EP,2,1)*SL
        VQG(EP,3,1) = VQG(EP,3,1)*SL
C
        VQG(EP,4,1)= VQG(EP,8,1)*VQG(EP,3,1)-VQG(EP,9,1)*VQG(EP,2,1)
        VQG(EP,5,1)= VQG(EP,9,1)*VQG(EP,1,1)-VQG(EP,7,1)*VQG(EP,3,1)
        VQG(EP,6,1)= VQG(EP,7,1)*VQG(EP,2,1)-VQG(EP,8,1)*VQG(EP,1,1)
C---------- NG =3----------
        SZ2=A_4+GAMA1
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,3)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,3)= VQG(EP,7,3)*SL
        VQG(EP,8,3)= VQG(EP,8,3)*SL
        VQG(EP,9,3)= SZ2*SL
C
        G1X3=MX23+JMX13
        G1Y3=MY23+JMY13
        J1 = SQRT(G1X3*G1X3 + G1Y3*G1Y3 +J2MYZ )
C--------G2X3=G2X2------
        G2X2=MX34+JMX13 
        G2Y2=MY34+JMY13
        J2 = SQRT(G2X2*G2X2 + G2Y2*G2Y2 +J2MYZ)
        VJFI(EP,1,3)=(G2Y2*VQG(EP,9,3)-JMZ13*VQG(EP,8,3))
        VJFI(EP,2,3)=(JMZ13*VQG(EP,7,3)-G2X2*VQG(EP,9,3))
        VJFI(EP,3,3)=(G2X2*VQG(EP,8,3)-G2Y2*VQG(EP,7,3))
        VQG(EP,1,3)= G1X3*J2+VJFI(EP,1,3)*J1
        VQG(EP,2,3)= G1Y3*J2+VJFI(EP,2,3)*J1
        VQG(EP,3,3)= JMZ13*J2+VJFI(EP,3,3)*J1
        SL=SL/PG
        VJFI(EP,1,3)=VJFI(EP,1,3)*SL
        VJFI(EP,2,3)=VJFI(EP,2,3)*SL
        VJFI(EP,3,3)=VJFI(EP,3,3)*SL
C
        VJFI(EP,4,3)=(JMZ13*VQG(EP,8,3)-G1Y3*VQG(EP,9,3))*SL
        VJFI(EP,5,3)=(G1X3*VQG(EP,9,3)-JMZ13*VQG(EP,7,3))*SL
        VJFI(EP,6,3)=(G1Y3*VQG(EP,7,3)-G1X3*VQG(EP,8,3))*SL
C
        SL = SQRT(VQG(EP,1,3)*VQG(EP,1,3) + VQG(EP,2,3)*VQG(EP,2,3) + VQG(EP,3,3)*VQG(EP,3,3))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,3) = VQG(EP,1,3)*SL
        VQG(EP,2,3) = VQG(EP,2,3)*SL
        VQG(EP,3,3) = VQG(EP,3,3)*SL
C
        VQG(EP,4,3)= VQG(EP,8,3)*VQG(EP,3,3)-VQG(EP,9,3)*VQG(EP,2,3)
        VQG(EP,5,3)= VQG(EP,9,3)*VQG(EP,1,3)-VQG(EP,7,3)*VQG(EP,3,3)
        VQG(EP,6,3)= VQG(EP,7,3)*VQG(EP,2,3)-VQG(EP,8,3)*VQG(EP,1,3)
C---------- NG =2----------
        SZ2=A_4+GAMA2
        SZ=Z2*L13(EP)
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,2)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,2)=-Z1*Y13
        VQG(EP,8,2)= Z1*X13
        VQG(EP,9,2)= SZ2*SL
        VQG(EP,7,4)=-VQG(EP,7,2)
        VQG(EP,8,4)=-VQG(EP,8,2)
        VQG(EP,7,2)= VQG(EP,7,2)*SL
        VQG(EP,8,2)= VQG(EP,8,2)*SL
C
        VJFI(EP,1,2)=(G2Y2*VQG(EP,9,2)-JMZ13*VQG(EP,8,2))
        VJFI(EP,2,2)=(JMZ13*VQG(EP,7,2)-G2X2*VQG(EP,9,2))
        VJFI(EP,3,2)=(G2X2*VQG(EP,8,2)-G2Y2*VQG(EP,7,2))
        VQG(EP,1,2)= G1X1*J2+VJFI(EP,1,2)*C1
        VQG(EP,2,2)= G1Y1*J2+VJFI(EP,2,2)*C1
        VQG(EP,3,2)=-JMZ13*J2+VJFI(EP,3,2)*C1
        SL=SL/PG
        VJFI(EP,1,2)=VJFI(EP,1,2)*SL
        VJFI(EP,2,2)=VJFI(EP,2,2)*SL
        VJFI(EP,3,2)=VJFI(EP,3,2)*SL
C
        VJFI(EP,4,2)=(-JMZ13*VQG(EP,8,2)-G1Y1*VQG(EP,9,2))*SL
        VJFI(EP,5,2)=(G1X1*VQG(EP,9,2)+JMZ13*VQG(EP,7,2))*SL
        VJFI(EP,6,2)=(G1Y1*VQG(EP,7,2)-G1X1*VQG(EP,8,2))*SL
C
        SL = SQRT(VQG(EP,1,2)*VQG(EP,1,2) + VQG(EP,2,2)*VQG(EP,2,2) + VQG(EP,3,2)*VQG(EP,3,2))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,2) = VQG(EP,1,2)*SL
        VQG(EP,2,2) = VQG(EP,2,2)*SL
        VQG(EP,3,2) = VQG(EP,3,2)*SL
        VQG(EP,4,2)= VQG(EP,8,2)*VQG(EP,3,2)-VQG(EP,9,2)*VQG(EP,2,2)
        VQG(EP,5,2)= VQG(EP,9,2)*VQG(EP,1,2)-VQG(EP,7,2)*VQG(EP,3,2)
        VQG(EP,6,2)= VQG(EP,7,2)*VQG(EP,2,2)-VQG(EP,8,2)*VQG(EP,1,2)
C---------- NG =4----------
        SZ2=A_4-GAMA2
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,4)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,4)= VQG(EP,7,4)*SL
        VQG(EP,8,4)= VQG(EP,8,4)*SL
        VQG(EP,9,4)= SZ2*SL
C
        VJFI(EP,1,4)=(G2Y1*VQG(EP,9,4)+JMZ13*VQG(EP,8,4))
        VJFI(EP,2,4)=(-JMZ13*VQG(EP,7,4)-G2X1*VQG(EP,9,4))
        VJFI(EP,3,4)=(G2X1*VQG(EP,8,4)-G2Y1*VQG(EP,7,4))
        VQG(EP,1,4)= G1X3*C2+VJFI(EP,1,4)*J1
        VQG(EP,2,4)= G1Y3*C2+VJFI(EP,2,4)*J1
        VQG(EP,3,4)=JMZ13*C2+VJFI(EP,3,4)*J1
        SL=SL/PG
        VJFI(EP,1,4)=VJFI(EP,1,4)*SL
        VJFI(EP,2,4)=VJFI(EP,2,4)*SL
        VJFI(EP,3,4)=VJFI(EP,3,4)*SL
C
        VJFI(EP,4,4)=(JMZ13*VQG(EP,8,4)-G1Y3*VQG(EP,9,4))*SL
        VJFI(EP,5,4)=(G1X3*VQG(EP,9,4)-JMZ13*VQG(EP,7,4))*SL
        VJFI(EP,6,4)=(G1Y3*VQG(EP,7,4)-G1X3*VQG(EP,8,4))*SL
C
        SL = SQRT(VQG(EP,1,4)*VQG(EP,1,4) + VQG(EP,2,4)*VQG(EP,2,4) + VQG(EP,3,4)*VQG(EP,3,4))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,4) = VQG(EP,1,4)*SL
        VQG(EP,2,4) = VQG(EP,2,4)*SL
        VQG(EP,3,4) = VQG(EP,3,4)*SL
        VQG(EP,4,4)= VQG(EP,8,4)*VQG(EP,3,4)-VQG(EP,9,4)*VQG(EP,2,4)
        VQG(EP,5,4)= VQG(EP,9,4)*VQG(EP,1,4)-VQG(EP,7,4)*VQG(EP,3,4)
        VQG(EP,6,4)= VQG(EP,7,4)*VQG(EP,2,4)-VQG(EP,8,4)*VQG(EP,1,4)
        AREA(EP)=JAC(EP,1)+JAC(EP,2)+JAC(EP,3)+JAC(EP,4)
      ENDDO
C ---w/ drilling dof----------
      IF (ISROT>0) THEN
#include "vectorize.inc"
        DO I=JFT,NPLAT
         EP =IPLAT(I)
         NN(1)=IXC(2,EP)
         NN(2)=IXC(3,EP)
         NN(3)=IXC(4,EP)
         NN(4)=IXC(5,EP)
C-------APPLY 3DDL ROTATIONS ----
         RLZ(I,1) =(VQ(EP,1,3)*VR(1,NN(1))+VQ(EP,2,3)*VR(2,NN(1)) +VQ(EP,3,3)*VR(3,NN(1)))
         RLZ(I,2) =(VQ(EP,1,3)*VR(1,NN(2))+VQ(EP,2,3)*VR(2,NN(2)) +VQ(EP,3,3)*VR(3,NN(2)))
         RLZ(I,3) =(VQ(EP,1,3)*VR(1,NN(3))+VQ(EP,2,3)*VR(2,NN(3)) +VQ(EP,3,3)*VR(3,NN(3)))
         RLZ(I,4) =(VQ(EP,1,3)*VR(1,NN(4))+VQ(EP,2,3)*VR(2,NN(4)) +VQ(EP,3,3)*VR(3,NN(4)))
        ENDDO
      END IF !(ISROT>0) THEN
C  
       DO I=NPLAT+1,JLT
        EP =IPLAT(I)
        Z1=ZL1(EP)
        Z2=Z1*Z1
        COREL(1,1)=VCORE(EP,1,1) 
        COREL(1,2)=VCORE(EP,1,2) 
        COREL(1,3)=VCORE(EP,1,3) 
        COREL(1,4)=VCORE(EP,1,4)
        COREL(2,1)=VCORE(EP,2,1) 
        COREL(2,2)=VCORE(EP,2,2) 
        COREL(2,3)=VCORE(EP,2,3)
        COREL(2,4)=VCORE(EP,2,4)
        NN(1)=IXC(2,EP)
        NN(2)=IXC(3,EP)
        NN(3)=IXC(4,EP)
        NN(4)=IXC(5,EP)
        X13 =(VCORE(EP,1,1)-VCORE(EP,1,3))*HALF
        X24 =(VCORE(EP,1,2)-VCORE(EP,1,4))*HALF
        Y13 =(VCORE(EP,2,1)-VCORE(EP,2,3))*HALF
        Y24 =(VCORE(EP,2,2)-VCORE(EP,2,4))*HALF
        MX13=(VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
        MY13=(VCORE(EP,2,1)+VCORE(EP,2,3))*HALF
C--------------------------
C     APPLY 6DDL-->5DDL
C--------------------------
C-----RXYZ(2,NNOD)=[Q]^T RGXYZ(3,NNOD)-----
         RR(1,1) =VQ(EP,1,1)*VR(1,NN(1))+VQ(EP,2,1)*VR(2,NN(1))+VQ(EP,3,1)*VR(3,NN(1))
         RR(1,2) =VQ(EP,1,1)*VR(1,NN(2))+VQ(EP,2,1)*VR(2,NN(2))+VQ(EP,3,1)*VR(3,NN(2))
         RR(1,3) =VQ(EP,1,1)*VR(1,NN(3))+VQ(EP,2,1)*VR(2,NN(3))+VQ(EP,3,1)*VR(3,NN(3))
         RR(1,4) =VQ(EP,1,1)*VR(1,NN(4))+VQ(EP,2,1)*VR(2,NN(4))+VQ(EP,3,1)*VR(3,NN(4))
         RR(2,1) =VQ(EP,1,2)*VR(1,NN(1))+VQ(EP,2,2)*VR(2,NN(1))+VQ(EP,3,2)*VR(3,NN(1))
         RR(2,2) =VQ(EP,1,2)*VR(1,NN(2))+VQ(EP,2,2)*VR(2,NN(2))+VQ(EP,3,2)*VR(3,NN(2))
         RR(2,3) =VQ(EP,1,2)*VR(1,NN(3))+VQ(EP,2,2)*VR(2,NN(3))+VQ(EP,3,2)*VR(3,NN(3))
         RR(2,4) =VQ(EP,1,2)*VR(1,NN(4))+VQ(EP,2,2)*VR(2,NN(4))+VQ(EP,3,2)*VR(3,NN(4))
         RR(3,1) =VQ(EP,1,3)*VR(1,NN(1))+VQ(EP,2,3)*VR(2,NN(1))+VQ(EP,3,3)*VR(3,NN(1))
         RR(3,2) =VQ(EP,1,3)*VR(1,NN(2))+VQ(EP,2,3)*VR(2,NN(2))+VQ(EP,3,3)*VR(3,NN(2))
         RR(3,3) =VQ(EP,1,3)*VR(1,NN(3))+VQ(EP,2,3)*VR(2,NN(3))+VQ(EP,3,3)*VR(3,NN(3))
         RR(3,4) =VQ(EP,1,3)*VR(1,NN(4))+VQ(EP,2,3)*VR(2,NN(4))+VQ(EP,3,3)*VR(3,NN(4))
C-----------------------full projection------------------
        AR(1)=-Z1*VHI(EP,2)+Y13*V13(EP,3)+Y24*V24(EP,3)+MY13*VHI(EP,3)+RR(1,1)+RR(1,2)+RR(1,3)+RR(1,4)
        AR(2)= Z1*VHI(EP,1)-X13*V13(EP,3)-X24*V24(EP,3)-MX13*VHI(EP,3)+RR(2,1)+RR(2,2)+RR(2,3)+RR(2,4)
        AR(3)= X13*V13(EP,2)+X24*V24(EP,2)+MX13*VHI(EP,2)-Y13*V13(EP,1)-Y24*V24(EP,1)-MY13*VHI(EP,1)+RR(3,1)+RR(3,2)+RR(3,3)+RR(3,4)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
        XX1 = COREL(1,1)*COREL(1,1)+COREL(1,2)*COREL(1,2)+COREL(1,3)*COREL(1,3)+COREL(1,4)*COREL(1,4)
        YY = COREL(2,1)*COREL(2,1)+COREL(2,2)*COREL(2,2)+COREL(2,3)*COREL(2,3)+COREL(2,4)*COREL(2,4)
        XY = COREL(1,1)*COREL(2,1)+COREL(1,2)*COREL(2,2)+COREL(1,3)*COREL(2,3)+COREL(1,4)*COREL(2,4)
        XZ1 = (COREL(1,1)-COREL(1,2)+COREL(1,3)-COREL(1,4))*Z1
        YZ = (COREL(2,1)-COREL(2,2)+COREL(2,3)-COREL(2,4))*Z1
        ZZ = FOUR*Z2
         D(1)= YY+ZZ+4
         D(2)= XX1+ZZ+4
         D(3)= XX1+YY+4
         D(4)= -XY
         D(5)= -XZ1
         D(6)= -YZ
C       
         ABC = D(1)*D(2)*D(3)
         XXYZ2 = D(1)*D(6)*D(6)
         YYXZ2 = D(2)*D(5)*D(5)
         ZZXY2 = D(3)*D(4)*D(4)
         DETA = ABC+2*D(4)*D(5)*D(6)-XXYZ2-YYXZ2-ZZXY2
         IF (DETA<EM20) THEN
          DETA=ONE
         ELSE
          DETA=ONE/DETA
         ENDIF
         DI(EP,1) = (ABC-XXYZ2)*DETA/D(1)
         DI(EP,2) = (ABC-YYXZ2)*DETA/D(2)
         DI(EP,3) = (ABC-ZZXY2)*DETA/D(3)
         DI(EP,4) = (D(5)*D(6)-D(4)*D(3))*DETA
         DI(EP,5) = (D(6)*D(4)-D(5)*D(2))*DETA
         DI(EP,6) = (D(4)*D(5)-D(6)*D(1))*DETA
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
         ALR(1) =DI(EP,1)*AR(1)+DI(EP,4)*AR(2)+DI(EP,5)*AR(3)
         ALR(2) =DI(EP,4)*AR(1)+DI(EP,2)*AR(2)+DI(EP,6)*AR(3)
         ALR(3) =DI(EP,5)*AR(1)+DI(EP,6)*AR(2)+DI(EP,3)*AR(3)
         V13(EP,1)= HALF*V13(EP,1)+ALR(3)*Y13
         V24(EP,1)= HALF*V24(EP,1)+ALR(3)*Y24
         VHI(EP,1)= FOURTH*VHI(EP,1)+(ALR(3)*MY13-Z1*ALR(2))
         V13(EP,2)= HALF*V13(EP,2)-ALR(3)*X13
         V24(EP,2)= HALF*V24(EP,2)-ALR(3)*X24
         VHI(EP,2)= FOURTH*VHI(EP,2)-(ALR(3)*MX13-Z1*ALR(1))
         V13(EP,3)= HALF*V13(EP,3)-(Y13*ALR(1)-X13*ALR(2))
         V24(EP,3)= HALF*V24(EP,3)-(Y24*ALR(1)-X24*ALR(2))
         VHI(EP,3)= FOURTH*VHI(EP,3)+(MX13*ALR(2)-MY13*ALR(1))

         VXYZ(EP,1,1)= V13(EP,1)+VHI(EP,1)
         VXYZ(EP,1,2)= V24(EP,1)-VHI(EP,1)
         VXYZ(EP,1,3)= -V13(EP,1)+VHI(EP,1)
         VXYZ(EP,1,4)= -V24(EP,1)-VHI(EP,1)

         VXYZ(EP,2,1)= V13(EP,2)+VHI(EP,2)
         VXYZ(EP,2,2)= V24(EP,2)-VHI(EP,2)
         VXYZ(EP,2,3)= -V13(EP,2)+VHI(EP,2)
         VXYZ(EP,2,4)= -V24(EP,2)-VHI(EP,2)

         VXYZ(EP,3,1)= V13(EP,3)+VHI(EP,3)
         VXYZ(EP,3,2)= V24(EP,3)-VHI(EP,3)
         VXYZ(EP,3,3)= -V13(EP,3)+VHI(EP,3)
         VXYZ(EP,3,4)= -V24(EP,3)-VHI(EP,3)
C
         RR(1,1)= RR(1,1)-ALR(1)
         RR(1,2)= RR(1,2)-ALR(1)
         RR(1,3)= RR(1,3)-ALR(1)
         RR(1,4)= RR(1,4)-ALR(1)
C
         RR(2,1)= RR(2,1)-ALR(2)
         RR(2,2)= RR(2,2)-ALR(2)
         RR(2,3)= RR(2,3)-ALR(2)
         RR(2,4)= RR(2,4)-ALR(2)
C
         RR(3,1)= RR(3,1)-ALR(3)
         RR(3,2)= RR(3,2)-ALR(3)
         RR(3,3)= RR(3,3)-ALR(3)
         RR(3,4)= RR(3,4)-ALR(3)
         RXYZ(EP,1,1)=VQN(EP,1,1)*RR(1,1)+VQN(EP,2,1)*RR(2,1)+VQN(EP,3,1)*RR(3,1)
         RXYZ(EP,2,1)=VQN(EP,4,1)*RR(1,1)+VQN(EP,5,1)*RR(2,1)+VQN(EP,6,1)*RR(3,1)
         RXYZ(EP,1,2)=VQN(EP,1,2)*RR(1,2)+VQN(EP,2,2)*RR(2,2)+VQN(EP,3,2)*RR(3,2)
         RXYZ(EP,2,2)=VQN(EP,4,2)*RR(1,2)+VQN(EP,5,2)*RR(2,2)+VQN(EP,6,2)*RR(3,2)
         RXYZ(EP,1,3)=VQN(EP,1,3)*RR(1,3)+VQN(EP,2,3)*RR(2,3)+VQN(EP,3,3)*RR(3,3)
         RXYZ(EP,2,3)=VQN(EP,4,3)*RR(1,3)+VQN(EP,5,3)*RR(2,3)+VQN(EP,6,3)*RR(3,3)
         RXYZ(EP,1,4)=VQN(EP,1,4)*RR(1,4)+VQN(EP,2,4)*RR(2,4)+VQN(EP,3,4)*RR(3,4)
         RXYZ(EP,2,4)=VQN(EP,4,4)*RR(1,4)+VQN(EP,5,4)*RR(2,4)+VQN(EP,6,4)*RR(3,4)
C--------drilling dof  
        IF (ISROT>0) THEN
         RLZ(I,1)=(VQN(EP,7,1)*RR(1,1)+VQN(EP,8,1)*RR(2,1)+VQN(EP,9,1)*RR(3,1))
         RLZ(I,2)=(VQN(EP,7,2)*RR(1,2)+VQN(EP,8,2)*RR(2,2)+VQN(EP,9,2)*RR(3,2))
         RLZ(I,3)=(VQN(EP,7,3)*RR(1,3)+VQN(EP,8,3)*RR(2,3)+VQN(EP,9,3)*RR(3,3))
         RLZ(I,4)=(VQN(EP,7,4)*RR(1,4)+VQN(EP,8,4)*RR(2,4)+VQN(EP,9,4)*RR(3,4))
        END IF !(ISROT>0) THEN
       END DO 
C
C----------------------------
C---------Factor for characteristic length
C----------------------------
       DO I=JFT,JLT
        RX(I)=XL2(I)+XL3(I)-XL4(I)
        RY(I)=YL2(I)+YL3(I)-YL4(I)
        SX(I)=-XL2(I)+XL3(I)+XL4(I)
        SY(I)=-YL2(I)+YL3(I)+YL4(I)
        C1=SQRT(RX(I)*RX(I)+RY(I)*RY(I))
        C2=SQRT(SX(I)*SX(I)+SY(I)*SY(I))
        S1=FOURTH*(MAX(C1,C2)/MIN(C1,C2)-ONE)
        FAC1=MIN(HALF,S1)+ONE
        FAC2=FOUR*AREA(I)/(C1*C2)
        FAC2=3.413*MAX(ZERO,FAC2-0.7071)
        FAC2=0.78+0.22*FAC2*FAC2*FAC2
        FACI=TWO*FAC1*FAC2
C
        FACN(I,1)=SQRT(L24(I)/LL(I))
        FACN(I,2)=SQRT(L13(I)/LL(I))
        S1 = SQRT(FACI*(FOUR_OVER_3+LM(I)*AREA_I(I))*LL(I))
        S1 = MAX(S1,EM20)
        LL(I) = AREA(I)/S1
       ENDDO

      OFF_L = ZERO
      DO EP=JFT,JLT
C---------factor for characteristic length---
        OFF(EP) = MIN(ONE,ABS(OFFG(EP)))
        OFF_L  = MIN(OFF_L,OFFG(EP))
      ENDDO
C       
      IF(OFF_L<ZERO)THEN
        DO EP=JFT,JLT
         IF(OFFG(EP)<ZERO)THEN
          VXYZ(EP,1,1)= ZERO
          VXYZ(EP,2,1)= ZERO
          VXYZ(EP,3,1)= ZERO
          VXYZ(EP,1,2)= ZERO
          VXYZ(EP,2,2)= ZERO
          VXYZ(EP,3,2)= ZERO
          VXYZ(EP,1,3)= ZERO
          VXYZ(EP,2,3)= ZERO
          VXYZ(EP,3,3)= ZERO
          VXYZ(EP,1,4)= ZERO
          VXYZ(EP,2,4)= ZERO
          VXYZ(EP,3,4)= ZERO
C
          RXYZ(EP,1,1)= ZERO
          RXYZ(EP,2,1)= ZERO
          RXYZ(EP,1,2)= ZERO
          RXYZ(EP,2,2)= ZERO
          RXYZ(EP,1,3)= ZERO
          RXYZ(EP,2,3)= ZERO
          RXYZ(EP,1,4)= ZERO
          RXYZ(EP,2,4)= ZERO  
         ENDIF
        ENDDO
      ENDIF
         IF(ANIM_PLY > 0) THEN
#include "lockon.inc"
           DO EP=JFT,JLT
            I1 = INOD(IXC(2,EP)) 
            I2 = INOD(IXC(3,EP))
            I3 = INOD(IXC(4,EP)) 
            I4 = INOD(IXC(5,EP))
cc
            VN_NOD(1,I1) = VN_NOD(1,I1)+VQ(EP,1,3)
            VN_NOD(2,I1) = VN_NOD(2,I1)+VQ(EP,2,3)
            VN_NOD(3,I1) = VN_NOD(3,I1)+VQ(EP,3,3)
C            
            VN_NOD(1,I2) =VN_NOD(1,I2)+VQ(EP,1,3)
            VN_NOD(2,I2) =VN_NOD(2,I2)+VQ(EP,2,3)
            VN_NOD(3,I2) =VN_NOD(3,I2)+VQ(EP,3,3)
C
            VN_NOD(1,I3) =VN_NOD(1,I3)+VQ(EP,1,3)
            VN_NOD(2,I3) =VN_NOD(2,I3)+VQ(EP,2,3)
            VN_NOD(3,I3) =VN_NOD(3,I3)+VQ(EP,3,3)

C
            VN_NOD(1,I4) =VN_NOD(1,I4)+VQ(EP,1,3)
            VN_NOD(2,I4) =VN_NOD(2,I4)+VQ(EP,2,3)
            VN_NOD(3,I4) =VN_NOD(3,I4)+VQ(EP,3,3)
           ENDDO
#include "lockoff.inc"
         ENDIF  
C-------------              
        RETURN
        END
Chd|====================================================================
Chd|  CBACOORT                      source/elements/shell/coqueba/cbacoor.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
       SUBROUTINE CBACOORT(ELBUF_STR,JFT,JLT,X,V,
     .            VR,DR,IXC,PM,OFFG,AREA,
     1            VXYZ, RLZ,VCORE,JAC,HX,
     2            HY,VQ,VQG,VJFI,NPLAT,IPLAT,
     3            X13_T  ,X24_T  ,Y13_T,Y24_T,NPT   ,
     4            SMSTR , ISROT ,XLCOR,ZL  ,VQN,NEL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NNOD,NPLAT,NPT,ISROT,NEL
      INTEGER IXC(NIXC,*),IPLAT(*)
      PARAMETER (NNOD = 4)
      my_real
     .   X(3,*), V(3,*), VR(3,*),PM(NPROPM,*),OFFG(*),
     .   VCORE(MVSIZ,3,NNOD),VXYZ(MVSIZ,3,NNOD),AREA(*),VJFI(MVSIZ,6,4),
     .   VQ(MVSIZ,3,3),VQG(MVSIZ,9,NNOD),RLZ(MVSIZ,NNOD),
     .   JAC(MVSIZ,4),HX(MVSIZ,4),HY(MVSIZ,4),Y24_T(*),DR(3,*),
     .   X13_T(*),X24_T(*),Y13_T(*),XLCOR(MVSIZ,2,NNOD-1),ZL(*),VQN(MVSIZ,9,NNOD)
      DOUBLE PRECISION
     .  SMSTR(*)
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
C-----RLZ(NNOD,*)
C-----------------------------------------------
c FUNCTION: premilitary compute for QBAT and Ismstr=10
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT            - element id limit
c  I   X, V, VR(3,NUMNOD) - coordinate,velocity, rotational velocity in global system
c  I   IXC(NIXC,*NEL)     - element connectivity of shell (and other data)
c  I   PM(NPROPM,MID)     - input Material data
c  I   OFFG(NEL),OFF(NEL) - element activation flag, local flag
c  O   AREA(NEL),NTP      - Area, num. of integrating points in thickness
c  O   VCORE(MVSIZ,3,4)   - coordinates of 4 nodes in local system
c                           BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
c                           for plat element
c  O   VXYZ(3,4,NEL)      - nodal velocity (4 nodes) in local system
C                           V13->VXYZ(,1,),V24->VXYZ(,2,),VHI->VXYZ(,3,)------------
c                           for plat element
c  O   RXYZ(3,4,NEL)      - nodal rotational velocity (4 nodes) in local system 5 dof
C                           R13->RXYZ(,1,),R24->RXYZ(,2,),RHI->RXYZ(,3,),RTI->RXYZ(,4,)----
c                           for plat element
c  O  JAC,HX,HY(4,NEL)    - Jacobien,asymmetric part(hourglass) at 4 Gauss points
c  O  VQ(9,NEL),VQN,VQG(9,4,) - local system of element, nodal(4) and Gauss points(4)
c  O  Xij_T,Yij_T(NEL)    - [B] components used for in-plane shear assumed strain
c  IO XiS,YiS,ZiS(NEL)    - reference configurations used for small strain options
c  I  ISMSTR              - small strain  
c  O  ZL1(NEL)            - warped length
c  I  IDRIL               - drilling dof flag
c                          (used only Idril active )
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,II(9),J,K,L,M,I1,I2,I3,I4,EP,SPLAT,JJ
      INTEGER NN(4),JPLAT(MVSIZ),IFPROJ
      MY_REAL 
     .   XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
      MY_REAL
     .   C1,C2,L13(MVSIZ),L24(MVSIZ),
     .   AA,VV(3,NNOD),RR(3,NNOD),OFF_L
      MY_REAL
     .    TOL,PG,J0,J1,J2,DETA,COREL(3,4),X1,Y1,S1,LL(MVSIZ)
      MY_REAL
     .    X13,X24,Y13,MX13,MX23,MX34,MY13,Y13_2,Z1,Z2,GAMA1,GAMA2,
     .    X21,X34,Y21,Y34,Z21,Z34,X41,X32,Y41,Y32,Z_2,Z41,Z32,L12,L34,
     .    A_4,SL,SZ2,SZ,JMX13,JMY13,JMZ13,J2MYZ,LM(MVSIZ),Y24,Y24_2,
     .    MY23,MY34,SCAL,G1X1,G1X3,G1Y1,G1Y3,G2X1,G2X2,G2Y1,G2Y2,
     .   AR(3),AD(NNOD),BTB(6),XX1,YY,ZZ,XY,XZ1,YZ,D(6), 
     .   ALR(3),ALD(NNOD),DBAD(3),ABC,XXYZ2,ZZXY2,YYXZ2
      my_real 
     .   LXYZ0(3),DETA1(MVSIZ),
     .   XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),
     .   YL3(MVSIZ),YL4(MVSIZ),XX,AREA_I(MVSIZ),
     .   X0G2(MVSIZ),X0G3(MVSIZ),X0G4(MVSIZ),Y0G2(MVSIZ),
     .   Y0G3(MVSIZ),Y0G4(MVSIZ),Z0G2(MVSIZ),Z0G3(MVSIZ),Z0G4(MVSIZ)
C
      my_real 
     .   VG13(3),VG24(3),VGHI(3),V13(MVSIZ,3),V24(MVSIZ,3),VHI(MVSIZ,3),
     .   FACI,FAC1,FAC2,VR1_12,VR1_21,
     .   DT05,DT025,EXZ,EYZ,DDRX,DDRY,V13X,V24X,VHIX,DDRZ1,DDRZ2,DDRZ,
     .    VNX1, VNY1, VNZ1,VNX2,VNY2,VNZ2,VNX3,VNY3,VNZ3,VNX4,VNY4,VNZ4,
     .    NORM,DI(MVSIZ,6),ZL1(MVSIZ),SSZ(MVSIZ),
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),
     .   R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),DIRZ(MVSIZ,2),
     .   X0L2(MVSIZ),X0L3(MVSIZ),X0L4(MVSIZ),Y0L2(MVSIZ),
     .   Y0L3(MVSIZ),Y0L4(MVSIZ),NX,NY,NZ,DET,VQ0(MVSIZ,3,3),DET_1
C-------- [F] will contains only Rz ->2D 
      my_real
     .   AXYZ(MVSIZ,3,NNOD)
        DATA   PG/.577350269189626/
C=======================================================================
      DO I=1,9
        II(I) = NEL*(I-1)
      ENDDO
C
      TOL=EM12
      IF (ISROT > 0  ) TOL=EM8
       DO I=JFT,JLT
          IF(ABS(OFFG(I))==ONE)OFFG(I)=SIGN(TWO,OFFG(I))
          AXYZ(I,1:3,1:4)= ZERO
C            
           IF (ISROT > 0  ) THEN
            NN(1)=IXC(2,I)
            NN(2)=IXC(3,I)
            NN(3)=IXC(4,I)
            NN(4)=IXC(5,I)
            AXYZ(I,1,1) = DR(1,NN(1))
            AXYZ(I,2,1) = DR(2,NN(1))
            AXYZ(I,3,1) = DR(3,NN(1))
            AXYZ(I,1,2) = DR(1,NN(2))
            AXYZ(I,2,2) = DR(2,NN(2))
            AXYZ(I,3,2) = DR(3,NN(2))
            AXYZ(I,1,3) = DR(1,NN(3))
            AXYZ(I,2,3) = DR(2,NN(3))
            AXYZ(I,3,3) = DR(3,NN(3))
            AXYZ(I,1,4) = DR(1,NN(4))
            AXYZ(I,2,4) = DR(2,NN(4))
            AXYZ(I,3,4) = DR(3,NN(4))
           END IF !(ISROT > 0  ) THEN
            
            X0G2(I) = SMSTR(II(1)+I)
            Y0G2(I) = SMSTR(II(2)+I)
            Z0G2(I) = SMSTR(II(3)+I)
            X0G3(I) = SMSTR(II(4)+I)
            Y0G3(I) = SMSTR(II(5)+I)
            Z0G3(I) = SMSTR(II(6)+I)
            X0G4(I) = SMSTR(II(7)+I)
            Y0G4(I) = SMSTR(II(8)+I)
            Z0G4(I) = SMSTR(II(9)+I)
        ENDDO
C--- in [F](2,2) only Rz is inside, Rx,Ry have been excluded       
C-bases at T=0 : B0_g : x0g(3,nnod-1) saved in SMSTR, w/ origine at n1
C---         B0_l : x0l(2,nnod-1)+Z1,VCORE at only T=0, but can be calculated from x0g(3,nnod-1)
C-bases at T>0 : B_G : X(3,numnod), B_l : by rotate B0_l w/ Rz0 
C---------UL : xl - x0l' (rotated around Rz0) 
C--       normal in initial conf. to compute Z1  
      DO I=JFT,JLT
        RX(I)=X0G2(I)+X0G3(I)-X0G4(I)
        SX(I)=X0G3(I)+X0G4(I)-X0G2(I)
        RY(I)=Y0G2(I)+Y0G3(I)-Y0G4(I)
        SY(I)=Y0G3(I)+Y0G4(I)-Y0G2(I)
        RZ(I)=Z0G2(I)+Z0G3(I)-Z0G4(I)
        SSZ(I)=Z0G3(I)+Z0G4(I)-Z0G2(I)
      ENDDO 
C----------------------------
C     LOCAL SYSTEM VQ0
C----------------------------
      K = 0
      CALL CLSKEW3(JFT,JLT,K,
     .   RX, RY, RZ, 
     .   SX, SY, SSZ, 
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,DETA1,OFFG )
      DO I=JFT,JLT
        AREA(I)=FOURTH*DETA1(I)
        AREA_I(I)=ONE/AREA(I)
        VQ0(I,1,1)=R11(I)
        VQ0(I,2,1)=R21(I)
        VQ0(I,3,1)=R31(I)
        VQ0(I,1,2)=R12(I)
        VQ0(I,2,2)=R22(I)
        VQ0(I,3,2)=R32(I)
        VQ0(I,1,3)=R13(I)
        VQ0(I,2,3)=R23(I)
        VQ0(I,3,3)=R33(I)
      ENDDO 
      DO I=JFT,JLT
        LXYZ0(1)=FOURTH*(X0G2(I)+X0G3(I)+X0G4(I))
        LXYZ0(2)=FOURTH*(Y0G2(I)+Y0G3(I)+Y0G4(I))
        LXYZ0(3)=FOURTH*(Z0G2(I)+Z0G3(I)+Z0G4(I))
        ZL1(I)=-(VQ0(I,1,3)*LXYZ0(1)+VQ0(I,2,3)*LXYZ0(2)+VQ0(I,3,3)*LXYZ0(3))
      ENDDO 
C----------------------------
C     xl =R1^t x0l; R1^t=(VQ0^t*VQ)^t---extract Rz0 of R1
C----------------------------
      DO I=JFT,JLT
       VR1_12=VQ0(I,1,1)*VQ(I,1,2)+VQ0(I,2,1)*VQ(I,2,2)+VQ0(I,3,1)*VQ(I,3,2)
       VR1_21=VQ0(I,1,2)*VQ(I,1,1)+VQ0(I,2,2)*VQ(I,2,1)+VQ0(I,3,1)*VQ(I,3,2)
       DIRZ(I,2) = HALF*(VR1_12-VR1_21)
       NORM = ONE-DIRZ(I,2)*DIRZ(I,2)
       DIRZ(I,1) = SQRT(MAX(ZERO,NORM))
      ENDDO 
      DO I=JFT,JLT
        X0L2(I)=VQ0(I,1,1)*X0G2(I)+VQ0(I,2,1)*Y0G2(I)+VQ0(I,3,1)*Z0G2(I)
        Y0L2(I)=VQ0(I,1,2)*X0G2(I)+VQ0(I,2,2)*Y0G2(I)+VQ0(I,3,2)*Z0G2(I)
        X0L3(I)=VQ0(I,1,1)*X0G3(I)+VQ0(I,2,1)*Y0G3(I)+VQ0(I,3,1)*Z0G3(I)
        Y0L3(I)=VQ0(I,1,2)*X0G3(I)+VQ0(I,2,2)*Y0G3(I)+VQ0(I,3,2)*Z0G3(I)
        X0L4(I)=VQ0(I,1,1)*X0G4(I)+VQ0(I,2,1)*Y0G4(I)+VQ0(I,3,1)*Z0G4(I)
        Y0L4(I)=VQ0(I,1,2)*X0G4(I)+VQ0(I,2,2)*Y0G4(I)+VQ0(I,3,2)*Z0G4(I)
      ENDDO
C----------------------------
C     Rotate x0l of Rz0 
C----------------------------
      DO I=JFT,JLT
        XL2(I)= X0L2(I)*DIRZ(I,1)-Y0L2(I)*DIRZ(I,2)
        YL2(I)= X0L2(I)*DIRZ(I,2)+Y0L2(I)*DIRZ(I,1)
        XL3(I)= X0L3(I)*DIRZ(I,1)-Y0L3(I)*DIRZ(I,2)
        YL3(I)= X0L3(I)*DIRZ(I,2)+Y0L3(I)*DIRZ(I,1)
        XL4(I)= X0L4(I)*DIRZ(I,1)-Y0L4(I)*DIRZ(I,2)
        YL4(I)= X0L4(I)*DIRZ(I,2)+Y0L4(I)*DIRZ(I,1)
        X0L2(I)=XL2(I)
        Y0L2(I)=YL2(I)
        X0L3(I)=XL3(I)
        Y0L3(I)=YL3(I)
        X0L4(I)=XL4(I)
        Y0L4(I)=YL4(I)
      ENDDO
      DO I=JFT,JLT
        XL2(I)=XLCOR(I,1,1)
        YL2(I)=XLCOR(I,2,1)
        XL3(I)=XLCOR(I,1,2)
        YL3(I)=XLCOR(I,2,2)
        XL4(I)=XLCOR(I,1,3)
        YL4(I)=XLCOR(I,2,3)
      ENDDO
C-----------UL : xl - x0l' (rotated of rz0) 
      DO I=JFT,JLT
        V13(I,1)=-XL3(I)-(-X0L3(I))
        V24(I,1)=XL2(I)-XL4(I)-(X0L2(I)-X0L4(I))
        VHI(I,1)=-XL2(I)+XL3(I)-XL4(I)-(-X0L2(I)+X0L3(I)-X0L4(I))
        V13(I,2)=-YL3(I)-(-Y0L3(I))
        V24(I,2)=YL2(I)-YL4(I)-(Y0L2(I)-Y0L4(I))
        VHI(I,2)=-YL2(I)+YL3(I)-YL4(I)-(-Y0L2(I)+Y0L3(I)-Y0L4(I))
        V13(I,3)=ZERO
        V24(I,3)=ZERO
        VHI(I,3)=FOUR*(ZL(I)-ZL1(I))
      ENDDO 
C-------set up B_l by rotating B0_l (Rz0)      
      DO I=JFT,JLT
        XL2(I)=X0L2(I)
        YL2(I)=Y0L2(I)
        XL3(I)=X0L3(I)
        YL3(I)=Y0L3(I)
        XL4(I)=X0L4(I)
        YL4(I)=Y0L4(I)
      ENDDO
C----------------------------
      DO EP=JFT,JLT
        LXYZ0(1)=FOURTH*(XL2(EP)+XL3(EP)+XL4(EP))
        LXYZ0(2)=FOURTH*(YL2(EP)+YL3(EP)+YL4(EP))
        VCORE(EP,1,1)=-LXYZ0(1)
        VCORE(EP,1,2)=XL2(EP)-LXYZ0(1)
        VCORE(EP,1,3)=XL3(EP)-LXYZ0(1)
        VCORE(EP,1,4)=XL4(EP)-LXYZ0(1)
        VCORE(EP,2,1)=-LXYZ0(2)
        VCORE(EP,2,2)=YL2(EP)-LXYZ0(2)
        VCORE(EP,2,3)=YL3(EP)-LXYZ0(2)
        VCORE(EP,2,4)=YL4(EP)-LXYZ0(2)
C
        X13_T(EP) =(VCORE(EP,1,1)-VCORE(EP,1,3))*HALF
        X24_T(EP) =(VCORE(EP,1,2)-VCORE(EP,1,4))*HALF
        Y13_T(EP) =(VCORE(EP,2,1)-VCORE(EP,2,3))*HALF
        Y24_T(EP) =(VCORE(EP,2,2)-VCORE(EP,2,4))*HALF
        L13(EP)=X13_T(EP)*X13_T(EP)+Y13_T(EP)*Y13_T(EP)
        L24(EP)=X24_T(EP)*X24_T(EP)+Y24_T(EP)*Y24_T(EP)
        LL(EP)=MAX(L13(EP),L24(EP))
      ENDDO 
c
      NPLAT=JFT-1
      SPLAT= 0
      DO EP=JFT,JLT
       Z2=ZL1(EP)*ZL1(EP)
       IF (Z2<TOL*LL(EP).OR.NPT==1) THEN
        NPLAT=NPLAT+1
        IPLAT(NPLAT)=EP
       ELSE
        SPLAT=SPLAT+1
        JPLAT(SPLAT)=EP
       ENDIF
      ENDDO 
      DO EP=NPLAT+1,JLT
        IPLAT(EP)=JPLAT(EP-NPLAT)
      ENDDO 
#include "vectorize.inc"
      DO I=JFT,NPLAT
        EP =IPLAT(I)
        X13 =X13_T(EP)
        X24 =X24_T(EP)
        Y13 =Y13_T(EP)
        Y24 =Y24_T(EP)
C
        MX13=(VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
        MY13=(VCORE(EP,2,1)+VCORE(EP,2,3))*HALF
        MX23=(VCORE(EP,1,2)+VCORE(EP,1,3))*HALF
        MX34=(VCORE(EP,1,3)+VCORE(EP,1,4))*HALF
        MY23=(VCORE(EP,2,2)+VCORE(EP,2,3))*HALF
        MY34=(VCORE(EP,2,3)+VCORE(EP,2,4))*HALF
        X13_T(EP) =X13*AREA_I(EP)
        X24_T(EP) =X24*AREA_I(EP)
        Y13_T(EP) =Y13*AREA_I(EP)
        Y24_T(EP) =Y24*AREA_I(EP)
C--------GAMA(2)
        GAMA1=-MX13*Y24+MY13*X24
        GAMA2= MX13*Y13-MY13*X13
        Z2=ZL1(EP)*ZL1(EP)
CC-------V13(I)->VXYZ(I,1),V24(I)->VXYZ(I,2),VHI(I)->VXYZ(I,3)------------
         VXYZ(EP,1,1)=V13(EP,1)
         VXYZ(EP,2,1)=V13(EP,2)
         VXYZ(EP,3,1)=V13(EP,3)
C
         VXYZ(EP,1,2)=V24(EP,1)
         VXYZ(EP,2,2)=V24(EP,2)
         VXYZ(EP,3,2)=V24(EP,3)
C
         VXYZ(EP,1,3)=VHI(EP,1)
         VXYZ(EP,2,3)=VHI(EP,2)
         VXYZ(EP,3,3)=VHI(EP,3)
C-------BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
C-------SONT STOCKE DANS VCORE---
        VCORE(EP,1,1)=Y24_T(EP)
        VCORE(EP,2,1)=-Y13_T(EP)
        VCORE(EP,3,1)=-X24_T(EP)
        VCORE(EP,1,2)= X13_T(EP)
        VCORE(EP,2,2)=GAMA1*AREA_I(EP)
        VCORE(EP,3,2)=GAMA2*AREA_I(EP)
        VCORE(EP,1,3)=MX23
        VCORE(EP,2,3)=MY23
        VCORE(EP,3,3)=MX34
        VCORE(EP,1,4)=MY34
        VCORE(EP,2,4)=MX13
        VCORE(EP,3,4)=MY13
C
        J1=(MX23*MY13-MX13*MY23)*PG
        J2=-(MX13*MY34-MX34*MY13)*PG
        J0=AREA(EP)*FOURTH
C----         
        JAC(EP,1)=ABS(J0+J2-J1)
        JAC(EP,2)=ABS(J0+J2+J1)
        JAC(EP,3)=ABS(J0-J2+J1)
        JAC(EP,4)=ABS(J0-J2-J1)
        J1=(MY23-MY34)*PG
        J2=-(MY23+MY34)*PG
        HX(EP,1)=J1/JAC(EP,1)
        HX(EP,2)=J2/JAC(EP,2)
        HX(EP,3)=-J1/JAC(EP,3)
        HX(EP,4)=-J2/JAC(EP,4)
        J1=(MX34-MX23)*PG
        J2=(MX34+MX23)*PG
        HY(EP,1)=J1/JAC(EP,1)
        HY(EP,2)=J2/JAC(EP,2)
        HY(EP,3)=-J1/JAC(EP,3)
        HY(EP,4)=-J2/JAC(EP,4)
      ENDDO
C ---w/ drilling dof------drilling may not work -> check RLZj w/ Rz0
      IF (ISROT>0) THEN
#include "vectorize.inc"
        DO I=JFT,NPLAT
         EP =IPLAT(I)
C-------TRANSMET 3DDL ROTATIONS ----
         RLZ(EP,1) =VQ(EP,1,3)*AXYZ(EP,1,1)+VQ(EP,2,3)*AXYZ(EP,2,1)+VQ(EP,3,3)*AXYZ(EP,3,1)
         RLZ(EP,2) =VQ(EP,1,3)*AXYZ(EP,1,2)+VQ(EP,2,3)*AXYZ(EP,2,2)+VQ(EP,3,3)*AXYZ(EP,3,2)
         RLZ(EP,3) =VQ(EP,1,3)*AXYZ(EP,1,3)+VQ(EP,2,3)*AXYZ(EP,2,3)+VQ(EP,3,3)*AXYZ(EP,3,3)
         RLZ(EP,4) =VQ(EP,1,3)*AXYZ(EP,1,4)+VQ(EP,2,3)*AXYZ(EP,2,4)+VQ(EP,3,3)*AXYZ(EP,3,4)
        ENDDO
      END IF !(ISROT>0) THEN
C----------group warped elements , VJFI,VQN,VQG    
       IFPROJ = 0
#include "vectorize.inc"
      DO I=NPLAT+1,JLT
       EP =IPLAT(I)
       Z1=ZL1(EP)
       Z2=Z1*Z1
       VCORE(EP,3,1)=Z1 
       VCORE(EP,3,2)=-Z1 
       VCORE(EP,3,3)=Z1 
       VCORE(EP,3,4)=-Z1 
       COREL(1,1)=VCORE(EP,1,1) 
       COREL(1,2)=VCORE(EP,1,2) 
       COREL(1,3)=VCORE(EP,1,3) 
       COREL(1,4)=VCORE(EP,1,4)
       COREL(2,1)=VCORE(EP,2,1) 
       COREL(2,2)=VCORE(EP,2,2) 
       COREL(2,3)=VCORE(EP,2,3)
       COREL(2,4)=VCORE(EP,2,4)
       X13 =X13_T(EP)
       X24 =X24_T(EP)
       Y13 =Y13_T(EP)
       Y24 =Y24_T(EP)
cc       
       MX13=(VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
       MY13=(VCORE(EP,2,1)+VCORE(EP,2,3))*HALF
       MX23=(VCORE(EP,1,2)+VCORE(EP,1,3))*HALF
       MY23=(VCORE(EP,2,2)+VCORE(EP,2,3))*HALF
       MX34=(VCORE(EP,1,3)+VCORE(EP,1,4))*HALF
       MY34=(VCORE(EP,2,3)+VCORE(EP,2,4))*HALF       
       X13_T(EP) =X13*AREA_I(EP)
       X24_T(EP) =X24*AREA_I(EP)
       Y13_T(EP) =Y13*AREA_I(EP)
       Y24_T(EP) =Y24*AREA_I(EP)
C--------GAMA(2)
       GAMA1=-MX13*Y24+MY13*X24
       GAMA2= MX13*Y13-MY13*X13
C--------------------------
C     CONSTRUIRE [F], [Q], [NM], [A-S], [AS] AUX NOEUDS
C--------------------------
         X21 =MX23-MX13
         X34 =(COREL(1,3)-COREL(1,4))*HALF
         Y21 =MY23-MY13
         Y34 =(COREL(2,3)-COREL(2,4))*HALF
         Z21 = -Z1
         Z34 = Z1
         L12 = SQRT(X21*X21+Y21*Y21+Z2)
         L34 = SQRT(X34*X34+Y34*Y34+Z2)
C---------
         X41 =MX34-MX13
         X32 =(COREL(1,3)-COREL(1,2))*HALF
         Y41 =MY34-MY13
         Y32 =(COREL(2,3)-COREL(2,2))*HALF
         Z41 = -Z1
         Z32 = Z1
C----------
        A_4=AREA(EP)*FOURTH
C
C---------- 
C    CALCUL DE [QG] NG=1,4 
C----------
        A_4=A_4/PG
        JMX13=MX13*PG
        JMY13=MY13*PG
        JMZ13=Z1*PG
        J2MYZ=JMZ13*JMZ13
C---------- NG =1----------
        SZ2=A_4-GAMA1
        SZ=Z2*L24(EP)
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,1)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,1)=-Z1*Y24
        VQG(EP,8,1)= Z1*X24
        VQG(EP,9,1)= SZ2*SL
        VQG(EP,7,3)=-VQG(EP,7,1)
        VQG(EP,8,3)=-VQG(EP,8,1)
        VQG(EP,7,1)= VQG(EP,7,1)*SL
        VQG(EP,8,1)= VQG(EP,8,1)*SL
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
        G1X1=MX23-JMX13
        G1Y1=MY23-JMY13
        C1 = SQRT(G1X1*G1X1 + G1Y1*G1Y1 +J2MYZ )
        G2X1=MX34-JMX13 
        G2Y1=MY34-JMY13
        C2 = SQRT(G2X1*G2X1 + G2Y1*G2Y1 +J2MYZ)
        VJFI(EP,1,1)=(G2Y1*VQG(EP,9,1)+JMZ13*VQG(EP,8,1))
        VJFI(EP,2,1)=(-JMZ13*VQG(EP,7,1)-G2X1*VQG(EP,9,1))
        VJFI(EP,3,1)=(G2X1*VQG(EP,8,1)-G2Y1*VQG(EP,7,1))
        VQG(EP,1,1)= G1X1*C2+VJFI(EP,1,1)*C1
        VQG(EP,2,1)= G1Y1*C2+VJFI(EP,2,1)*C1
        VQG(EP,3,1)= -JMZ13*C2+VJFI(EP,3,1)*C1
        SL=SL/PG
        VJFI(EP,1,1)=VJFI(EP,1,1)*SL
        VJFI(EP,2,1)=VJFI(EP,2,1)*SL
        VJFI(EP,3,1)=VJFI(EP,3,1)*SL
C
        VJFI(EP,4,1)=(-JMZ13*VQG(EP,8,1)-G1Y1*VQG(EP,9,1))*SL
        VJFI(EP,5,1)=(G1X1*VQG(EP,9,1)+JMZ13*VQG(EP,7,1))*SL
        VJFI(EP,6,1)=(G1Y1*VQG(EP,7,1)-G1X1*VQG(EP,8,1))*SL
C
        SL = SQRT(VQG(EP,1,1)*VQG(EP,1,1) + VQG(EP,2,1)*VQG(EP,2,1) + VQG(EP,3,1)*VQG(EP,3,1))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,1) = VQG(EP,1,1)*SL
        VQG(EP,2,1) = VQG(EP,2,1)*SL
        VQG(EP,3,1) = VQG(EP,3,1)*SL
C
        VQG(EP,4,1)= VQG(EP,8,1)*VQG(EP,3,1)-VQG(EP,9,1)*VQG(EP,2,1)
        VQG(EP,5,1)= VQG(EP,9,1)*VQG(EP,1,1)-VQG(EP,7,1)*VQG(EP,3,1)
        VQG(EP,6,1)= VQG(EP,7,1)*VQG(EP,2,1)-VQG(EP,8,1)*VQG(EP,1,1)
C---------- NG =3----------
        SZ2=A_4+GAMA1
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,3)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,3)= VQG(EP,7,3)*SL
        VQG(EP,8,3)= VQG(EP,8,3)*SL
        VQG(EP,9,3)= SZ2*SL
C
        G1X3=MX23+JMX13
        G1Y3=MY23+JMY13
        J1 = SQRT(G1X3*G1X3 + G1Y3*G1Y3 +J2MYZ )
C--------G2X3=G2X2------
        G2X2=MX34+JMX13 
        G2Y2=MY34+JMY13
        J2 = SQRT(G2X2*G2X2 + G2Y2*G2Y2 +J2MYZ)
        VJFI(EP,1,3)=(G2Y2*VQG(EP,9,3)-JMZ13*VQG(EP,8,3))
        VJFI(EP,2,3)=(JMZ13*VQG(EP,7,3)-G2X2*VQG(EP,9,3))
        VJFI(EP,3,3)=(G2X2*VQG(EP,8,3)-G2Y2*VQG(EP,7,3))
        VQG(EP,1,3)= G1X3*J2+VJFI(EP,1,3)*J1
        VQG(EP,2,3)= G1Y3*J2+VJFI(EP,2,3)*J1
        VQG(EP,3,3)= JMZ13*J2+VJFI(EP,3,3)*J1
        SL=SL/PG
        VJFI(EP,1,3)=VJFI(EP,1,3)*SL
        VJFI(EP,2,3)=VJFI(EP,2,3)*SL
        VJFI(EP,3,3)=VJFI(EP,3,3)*SL
C
        VJFI(EP,4,3)=(JMZ13*VQG(EP,8,3)-G1Y3*VQG(EP,9,3))*SL
        VJFI(EP,5,3)=(G1X3*VQG(EP,9,3)-JMZ13*VQG(EP,7,3))*SL
        VJFI(EP,6,3)=(G1Y3*VQG(EP,7,3)-G1X3*VQG(EP,8,3))*SL
C
        SL = SQRT(VQG(EP,1,3)*VQG(EP,1,3) + VQG(EP,2,3)*VQG(EP,2,3) + VQG(EP,3,3)*VQG(EP,3,3))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,3) = VQG(EP,1,3)*SL
        VQG(EP,2,3) = VQG(EP,2,3)*SL
        VQG(EP,3,3) = VQG(EP,3,3)*SL
C
        VQG(EP,4,3)= VQG(EP,8,3)*VQG(EP,3,3)-VQG(EP,9,3)*VQG(EP,2,3)
        VQG(EP,5,3)= VQG(EP,9,3)*VQG(EP,1,3)-VQG(EP,7,3)*VQG(EP,3,3)
        VQG(EP,6,3)= VQG(EP,7,3)*VQG(EP,2,3)-VQG(EP,8,3)*VQG(EP,1,3)
C---------- NG =2----------
        SZ2=A_4+GAMA2
        SZ=Z2*L13(EP)
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,2)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,2)=-Z1*Y13
        VQG(EP,8,2)= Z1*X13
        VQG(EP,9,2)= SZ2*SL
        VQG(EP,7,4)=-VQG(EP,7,2)
        VQG(EP,8,4)=-VQG(EP,8,2)
        VQG(EP,7,2)= VQG(EP,7,2)*SL
        VQG(EP,8,2)= VQG(EP,8,2)*SL
C
        VJFI(EP,1,2)=(G2Y2*VQG(EP,9,2)-JMZ13*VQG(EP,8,2))
        VJFI(EP,2,2)=(JMZ13*VQG(EP,7,2)-G2X2*VQG(EP,9,2))
        VJFI(EP,3,2)=(G2X2*VQG(EP,8,2)-G2Y2*VQG(EP,7,2))
        VQG(EP,1,2)= G1X1*J2+VJFI(EP,1,2)*C1
        VQG(EP,2,2)= G1Y1*J2+VJFI(EP,2,2)*C1
        VQG(EP,3,2)=-JMZ13*J2+VJFI(EP,3,2)*C1
        SL=SL/PG
        VJFI(EP,1,2)=VJFI(EP,1,2)*SL
        VJFI(EP,2,2)=VJFI(EP,2,2)*SL
        VJFI(EP,3,2)=VJFI(EP,3,2)*SL
C
        VJFI(EP,4,2)=(-JMZ13*VQG(EP,8,2)-G1Y1*VQG(EP,9,2))*SL
        VJFI(EP,5,2)=(G1X1*VQG(EP,9,2)+JMZ13*VQG(EP,7,2))*SL
        VJFI(EP,6,2)=(G1Y1*VQG(EP,7,2)-G1X1*VQG(EP,8,2))*SL
C
        SL = SQRT(VQG(EP,1,2)*VQG(EP,1,2) + VQG(EP,2,2)*VQG(EP,2,2) + VQG(EP,3,2)*VQG(EP,3,2))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,2) = VQG(EP,1,2)*SL
        VQG(EP,2,2) = VQG(EP,2,2)*SL
        VQG(EP,3,2) = VQG(EP,3,2)*SL
        VQG(EP,4,2)= VQG(EP,8,2)*VQG(EP,3,2)-VQG(EP,9,2)*VQG(EP,2,2)
        VQG(EP,5,2)= VQG(EP,9,2)*VQG(EP,1,2)-VQG(EP,7,2)*VQG(EP,3,2)
        VQG(EP,6,2)= VQG(EP,7,2)*VQG(EP,2,2)-VQG(EP,8,2)*VQG(EP,1,2)
C---------- NG =4----------
        SZ2=A_4-GAMA2
        SL=SQRT(SZ+SZ2*SZ2)
        JAC(EP,4)=SL*PG
        SL=ONE/MAX(SL,EM20)
        VQG(EP,7,4)= VQG(EP,7,4)*SL
        VQG(EP,8,4)= VQG(EP,8,4)*SL
        VQG(EP,9,4)= SZ2*SL
C
        VJFI(EP,1,4)=(G2Y1*VQG(EP,9,4)+JMZ13*VQG(EP,8,4))
        VJFI(EP,2,4)=(-JMZ13*VQG(EP,7,4)-G2X1*VQG(EP,9,4))
        VJFI(EP,3,4)=(G2X1*VQG(EP,8,4)-G2Y1*VQG(EP,7,4))
        VQG(EP,1,4)= G1X3*C2+VJFI(EP,1,4)*J1
        VQG(EP,2,4)= G1Y3*C2+VJFI(EP,2,4)*J1
        VQG(EP,3,4)=JMZ13*C2+VJFI(EP,3,4)*J1
        SL=SL/PG
        VJFI(EP,1,4)=VJFI(EP,1,4)*SL
        VJFI(EP,2,4)=VJFI(EP,2,4)*SL
        VJFI(EP,3,4)=VJFI(EP,3,4)*SL
C
        VJFI(EP,4,4)=(JMZ13*VQG(EP,8,4)-G1Y3*VQG(EP,9,4))*SL
        VJFI(EP,5,4)=(G1X3*VQG(EP,9,4)-JMZ13*VQG(EP,7,4))*SL
        VJFI(EP,6,4)=(G1Y3*VQG(EP,7,4)-G1X3*VQG(EP,8,4))*SL
C
        SL = SQRT(VQG(EP,1,4)*VQG(EP,1,4) + VQG(EP,2,4)*VQG(EP,2,4) + VQG(EP,3,4)*VQG(EP,3,4))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,4) = VQG(EP,1,4)*SL
        VQG(EP,2,4) = VQG(EP,2,4)*SL
        VQG(EP,3,4) = VQG(EP,3,4)*SL
        VQG(EP,4,4)= VQG(EP,8,4)*VQG(EP,3,4)-VQG(EP,9,4)*VQG(EP,2,4)
        VQG(EP,5,4)= VQG(EP,9,4)*VQG(EP,1,4)-VQG(EP,7,4)*VQG(EP,3,4)
        VQG(EP,6,4)= VQG(EP,7,4)*VQG(EP,2,4)-VQG(EP,8,4)*VQG(EP,1,4)
        AREA(EP)=JAC(EP,1)+JAC(EP,2)+JAC(EP,3)+JAC(EP,4)
      ENDDO
C--------------------------
C     TRANSMET 6DDL-->5DDL, w/o projection
C--------------------------
       DO I=NPLAT+1,JLT
         EP =IPLAT(I)
         VXYZ(EP,1,1)= HALF*V13(EP,1)+FOURTH*VHI(EP,1)
         VXYZ(EP,1,2)= HALF*V24(EP,1)-FOURTH*VHI(EP,1)
         VXYZ(EP,1,3)= -HALF*V13(EP,1)+FOURTH*VHI(EP,1)
         VXYZ(EP,1,4)= -HALF*V24(EP,1)-FOURTH*VHI(EP,1)

         VXYZ(EP,2,1)= HALF*V13(EP,2)+FOURTH*VHI(EP,2)
         VXYZ(EP,2,2)= HALF*V24(EP,2)-FOURTH*VHI(EP,2)
         VXYZ(EP,2,3)= -HALF*V13(EP,2)+FOURTH*VHI(EP,2)
         VXYZ(EP,2,4)= -HALF*V24(EP,2)-FOURTH*VHI(EP,2)
C
         VXYZ(EP,3,1)= HALF*V13(EP,3)+FOURTH*VHI(EP,3)
         VXYZ(EP,3,2)= HALF*V24(EP,3)-FOURTH*VHI(EP,3)
         VXYZ(EP,3,3)= -HALF*V13(EP,3)+FOURTH*VHI(EP,3)
         VXYZ(EP,3,4)= -HALF*V24(EP,3)-FOURTH*VHI(EP,3)
C--------drilling dof  
         IF (ISROT>0) THEN
           RR(1,1) =VQ(EP,1,1)*AXYZ(EP,1,1)+VQ(EP,2,1)*AXYZ(EP,2,1)+VQ(EP,3,1)*AXYZ(EP,3,1)
           RR(1,2) =VQ(EP,1,1)*AXYZ(EP,1,2)+VQ(EP,2,1)*AXYZ(EP,2,2)+VQ(EP,3,1)*AXYZ(EP,3,2)
           RR(1,3) =VQ(EP,1,1)*AXYZ(EP,1,3)+VQ(EP,2,1)*AXYZ(EP,2,3)+VQ(EP,3,1)*AXYZ(EP,3,3)
           RR(1,4) =VQ(EP,1,1)*AXYZ(EP,1,4)+VQ(EP,2,1)*AXYZ(EP,2,4)+VQ(EP,3,1)*AXYZ(EP,3,4)
           RR(2,1) =VQ(EP,1,2)*AXYZ(EP,1,1)+VQ(EP,2,2)*AXYZ(EP,2,1)+VQ(EP,3,2)*AXYZ(EP,3,1)
           RR(2,2) =VQ(EP,1,2)*AXYZ(EP,1,2)+VQ(EP,2,2)*AXYZ(EP,2,2)+VQ(EP,3,2)*AXYZ(EP,3,2)
           RR(2,3) =VQ(EP,1,2)*AXYZ(EP,1,3)+VQ(EP,2,2)*AXYZ(EP,2,3)+VQ(EP,3,2)*AXYZ(EP,3,3)
           RR(2,4) =VQ(EP,1,2)*AXYZ(EP,1,4)+VQ(EP,2,2)*AXYZ(EP,2,4)+VQ(EP,3,2)*AXYZ(EP,3,4)
           RR(3,1) =VQ(EP,1,3)*AXYZ(EP,1,1)+VQ(EP,2,3)*AXYZ(EP,2,1)+VQ(EP,3,3)*AXYZ(EP,3,1)
           RR(3,2) =VQ(EP,1,3)*AXYZ(EP,1,2)+VQ(EP,2,3)*AXYZ(EP,2,2)+VQ(EP,3,3)*AXYZ(EP,3,2)
           RR(3,3) =VQ(EP,1,3)*AXYZ(EP,1,3)+VQ(EP,2,3)*AXYZ(EP,2,3)+VQ(EP,3,3)*AXYZ(EP,3,3)
           RR(3,4) =VQ(EP,1,3)*AXYZ(EP,1,4)+VQ(EP,2,3)*AXYZ(EP,2,4)+VQ(EP,3,3)*AXYZ(EP,3,4)
C----------not right takes the RLZ with cbacoor		 
           RLZ(EP,1)=(VQN(EP,7,1)*RR(1,1)+VQN(EP,8,1)*RR(2,1)+VQN(EP,9,1)*RR(3,1))
           RLZ(EP,2)=(VQN(EP,7,2)*RR(1,2)+VQN(EP,8,2)*RR(2,2)+VQN(EP,9,2)*RR(3,2))
           RLZ(EP,3)=(VQN(EP,7,3)*RR(1,3)+VQN(EP,8,3)*RR(2,3)+VQN(EP,9,3)*RR(3,3))
           RLZ(EP,4)=(VQN(EP,7,4)*RR(1,4)+VQN(EP,8,4)*RR(2,4)+VQN(EP,9,4)*RR(3,4))
         END IF !(ISROT>0) THEN
       END DO
C
      OFF_L = ZERO
      DO EP=JFT,JLT
        OFF_L  = MIN(OFF_L,OFFG(EP))
      ENDDO
C       
      IF(OFF_L<ZERO)THEN
        DO EP=JFT,JLT
         IF(OFFG(EP)<ZERO)THEN
          VXYZ(EP,1:3,1:4)= ZERO
          RLZ(EP,1:4)= ZERO
         ENDIF
        ENDDO
      ENDIF
C-------------              
        RETURN
        END
