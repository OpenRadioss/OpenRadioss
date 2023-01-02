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
	SUBROUTINE CBACOORK(JFT,JLT,X,IXC,PM,OFFG,
     1                     GEO,AREA,VCORE,JAC,HX,HY,
     2                     VQN,VQG,VQ,VJFI,VNRM,VASTN,NPLAT,IPLAT,
     3                     X13_T  ,X24_T  ,Y13_T,Y24_T, 
     4                     ELBUF_STR,NLAY, SMSTR,
     5                     IREP,NPT,ISMSTR,DIR_A ,DIR_B,
     6                     PID,MAT,NGL,OFF,ISROT ,NEL) 
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXC(NIXC,*),JFT,JLT,NNOD,NLAY,NPLAT,IPLAT(*),ISROT,NEL
      MY_REAL
     .   X(3,*), PM(NPROPM,*),GEO(NPROPG,*),OFFG(*)
      PARAMETER (NNOD = 4)
      MY_REAL 
     .   VCORE(MVSIZ,3,NNOD),AREA(*),VJFI(MVSIZ,6,4),
     .   VQN(MVSIZ,9,NNOD),VQG(MVSIZ,9,NNOD),VQ(MVSIZ,3,3),
     .   VASTN(MVSIZ,4,NNOD),VNRM(MVSIZ,3,NNOD),
     .   JAC(MVSIZ,4),HX(MVSIZ,4),HY(MVSIZ,4),Y24_T(*),
     .   DIR_A(NEL,*),DIR_B(NEL,*),OFF(*),
     .   X13_T(*),X24_T(*),Y13_T(*),OFF_L
      DOUBLE PRECISION
     .  SMSTR(*)
      INTEGER IREP,NPT,ISMSTR,PID(*),MAT(*),NGL(*)
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
c FUNCTION: premilitary compute for QBAT [K] build
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT            - element id limit
c  I   X                  - coordinate x,y,z in global system (basic)
c  I   IXC(NIXC,*NEL)     - element connectivity of shell (and other data)
c  I   PM(NPROPM,MID),GEO - input Material and geometric property data
c  I   OFFG(NEL),OFF(NEL) - element activation flag, local flag
c  O   AREA(NEL) ,NPT     - Area (element), num. of integrating points in thickness
c  O   VCORE(3,4,NEL)     - coordinates of 4 nodes in local system
c                           BX0(2),BY0(2),GAMA(2),MX23,MY23,MX34,MY34,MX13,MY13
c                           for plat element
c  O  JAC,HX,HY(4,NEL)    - Jacobien,asymmetric part(hourglass) at 4 Gauss points
c  O  VQ(9,NEL),VQN,VQG(9,4,NEL) - local system of element, nodal(4) and Gauss points(4)
c  O  VJFI,VNRM,VASTN     - terms used for assumed strains
c  O  NPLAT,IPLAT(NEL)    - num. of plat element and indice
c  O  Xij_T,Yij_T(NEL)    - [B] components used for in-plane shear assumed strain
c  IO XiS,YiS,ZiS(NEL)    - reference configurations used for small strain options
c  I  ISMSTR,IREP         - small strain and orthotrop flags 
c  O  DIR_A,DIR_B(2,NEL)  - orthotropic directions
c  I  ISROT               - drilling dof flag
c  O  PID,MAT,NGL(NEL)    - geometric property id, material id and users' element id
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER J,I,II(6),K,EP,SPLAT,JPLAT(MVSIZ),L,M,MAT_1
      MY_REAL
     .    J0,J1,J2,DETA,X1,Y1,S1,PG
      MY_REAL
     .    X13,X24,Y13,MX13,MX23,MX34,MY13,Y13_2,Z1,Z2,GAMA1,GAMA2,
     .    X21,X34,Y21,Y34,Z21,Z34,X41,X32,Y41,Y32,Z_2,Z41,Z32,L12,L34,
     .    A_4,SL,SZ2,SZ,JMX13,JMY13,JMZ13,J2MYZ,LM(MVSIZ),Y24,Y24_2,
     .    MY23,MY34,SCAL,G1X1,G1X3,G1Y1,G1Y3,G2X1,G2X2,G2Y1,G2Y2
      my_real 
     .   LXYZ0(3),DETA1(MVSIZ),RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),
     .   R33(MVSIZ),XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),
     .   YL3(MVSIZ),YL4(MVSIZ),ZL1(MVSIZ),AREA_I(MVSIZ),SSZ(MVSIZ),
     .   L13(MVSIZ),L24(MVSIZ),XX,YY,ZZ,C1,C2,LL(MVSIZ)
      MY_REAL 
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,TOL
        DATA   PG/.577350269189626/
C-----------------------------------------------
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
C
      TOL=EM12
      IF (ISROT > 0 ) TOL=EM8
      MAT_1 = IXC(1,JFT)
      DO I=JFT,JLT
        MAT(I) = MAT_1
        PID(I) = IXC(6,I)
        NGL(I) = IXC(7,I)
      ENDDO
C----------------------------
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
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      DO I=JFT,JLT
        J=IXC(2,I)
        K=IXC(3,I)
        L=IXC(4,I)
        M=IXC(5,I)
        LXYZ0(1)=FOURTH*(X(1,L)+X(1,M)+X(1,J)+X(1,K))
        LXYZ0(2)=FOURTH*(X(2,L)+X(2,M)+X(2,J)+X(2,K))
        LXYZ0(3)=FOURTH*(X(3,L)+X(3,M)+X(3,J)+X(3,K))
C
        XX1=X(1,K)-X(1,J)
        YY1=X(2,K)-X(2,J)
        ZZ1=X(3,K)-X(3,J)
C
        XL2(I)=R11(I)*XX1+R21(I)*YY1+R31(I)*ZZ1
        YL2(I)=R12(I)*XX1+R22(I)*YY1+R32(I)*ZZ1
C
        XX2=X(1,J)-LXYZ0(1)
        YY2=X(2,J)-LXYZ0(2)
        ZZ2=X(3,J)-LXYZ0(3)
        ZL1(I)=R13(I)*XX2+R23(I)*YY2+R33(I)*ZZ2
C
        XX3=X(1,L)-X(1,J)
        YY3=X(2,L)-X(2,J)
        ZZ3=X(3,L)-X(3,J)
        XL3(I)=R11(I)*XX3+R21(I)*YY3+R31(I)*ZZ3
        YL3(I)=R12(I)*XX3+R22(I)*YY3+R32(I)*ZZ3
C
        XX4=X(1,M)-X(1,J)
        YY4=X(2,M)-X(2,J)
        ZZ4=X(3,M)-X(3,J)
        XL4(I)=R11(I)*XX4+R21(I)*YY4+R31(I)*ZZ4
        YL4(I)=R12(I)*XX4+R22(I)*YY4+R32(I)*ZZ4
      ENDDO
C----------------------------
C     SMALL STRAIN
C----------------------------
      IF(ISMSTR==1.OR.ISMSTR==2)THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I))==TWO)THEN
            XL2(I)=SMSTR(II(1)+I)
            YL2(I)=SMSTR(II(2)+I)
            XL3(I)=SMSTR(II(3)+I)
            YL3(I)=SMSTR(II(4)+I)
            XL4(I)=SMSTR(II(5)+I)
            YL4(I)=SMSTR(II(6)+I)
            ZL1(I)=ZERO
            AREA(I)=HALF*
     .              ((XL2(I)-XL4(I))*YL3(I)-XL3(I)*(YL2(I)-YL4(I)))
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
C     ORTHOTROPY (plus tard)
C----------------------------
      IF (IREP > 0) THEN
       CALL CORTDIR3(ELBUF_STR,DIR_A,DIR_B ,JFT    ,JLT    ,
     .               NLAY   ,IREP   ,RX    ,RY     ,RZ     , 
     .               SX     ,SY     ,SSZ   ,R11    ,R21    ,
     .               R31    ,R12    ,R22   ,R32    ,NEL    )
      ENDIF 
c
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
        X13_T(EP) =(VCORE(EP,1,1)-VCORE(EP,1,3))*HALF
        X24_T(EP) =(VCORE(EP,1,2)-VCORE(EP,1,4))*HALF
        Y13_T(EP) =(VCORE(EP,2,1)-VCORE(EP,2,3))*HALF
        Y24_T(EP) =(VCORE(EP,2,2)-VCORE(EP,2,4))*HALF
        L13(EP)=X13_T(EP)*X13_T(EP)+Y13_T(EP)*Y13_T(EP)
        L24(EP)=X24_T(EP)*X24_T(EP)+Y24_T(EP)*Y24_T(EP)
        LL(EP)=MAX(L13(EP),L24(EP))
      ENDDO 
      IF (IMP_CHK > 0) THEN
       DO EP=JFT,JLT
        MX13=(VCORE(EP,1,1)+VCORE(EP,1,3))*HALF
        MY13=(VCORE(EP,2,1)+VCORE(EP,2,3))*HALF
        MX23=(VCORE(EP,1,2)+VCORE(EP,1,3))*HALF
        MX34=(VCORE(EP,1,3)+VCORE(EP,1,4))*HALF
        MY23=(VCORE(EP,2,2)+VCORE(EP,2,3))*HALF
        MY34=(VCORE(EP,2,3)+VCORE(EP,2,4))*HALF
        J1=(MX23*MY13-MX13*MY23)*PG
        J2=-(MX13*MY34-MX34*MY13)*PG
        J0=AREA(EP)*FOURTH
        JAC(EP,1)=J0+J2-J1
        JAC(EP,2)=J0+J2+J1
        JAC(EP,3)=J0-J2+J1
        JAC(EP,4)=J0-J2-J1
        IF(OFFG(EP)/=ZERO)THEN
         IF(JAC(EP,1)<=ZERO.OR.JAC(EP,2)<=ZERO.OR.
     .      JAC(EP,3)<=ZERO.OR.JAC(EP,4)<=ZERO)THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(EP)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
         ENDIF 
        ENDIF 
       ENDDO
      ENDIF 
C  -------vectoriser--un jour-faire index jft,nplat,jlt pour tous--
      NPLAT=JFT-1
      SPLAT= 0
      DO EP=JFT,JLT
       Z2=ZL1(EP)*ZL1(EP)
       IF (Z2<TOL*LL(EP)) THEN
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
        J1=(MX23*MY13-MX13*MY23)*PG
        J2=-(MX13*MY34-MX34*MY13)*PG
        J0=AREA(EP)*FOURTH
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
#include "vectorize.inc"
      DO I=NPLAT+1,JLT
       EP =IPLAT(I)
       Z1=ZL1(EP)
       Z2=Z1*Z1
       VCORE(EP,3,1)=Z1 
       VCORE(EP,3,2)=-Z1 
       VCORE(EP,3,3)=Z1 
       VCORE(EP,3,4)=-Z1 
       X13 =X13_T(EP)
       X24 =X24_T(EP)
       Y13 =Y13_T(EP)
       Y24 =Y24_T(EP)
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
C----------------------------------------------------
C  CALCUL DE [F] 
C----------------------------------------------------
C--------- 
C   2A1
C---------
         X21 =MX23-MX13
         X34 =(VCORE(EP,1,3)-VCORE(EP,1,4))*HALF
         Y21 =MY23-MY13
         Y34 =(VCORE(EP,2,3)-VCORE(EP,2,4))*HALF
         Z21 = -Z1
         Z34 = Z1
         L12 = SQRT(X21*X21+Y21*Y21+Z2)
         L34 = SQRT(X34*X34+Y34*Y34+Z2)
C--------- 
C   2A2
C---------
         X41 =MX34-MX13
         X32 =(VCORE(EP,1,3)-VCORE(EP,1,2))*HALF
         Y41 =MY34-MY13
         Y32 =(VCORE(EP,2,3)-VCORE(EP,2,2))*HALF
         Z41 = -Z1
         Z32 = Z1
C---------- 
C    CALCUL DE [QN] N=1,4 
C----------
        A_4=AREA(EP)*FOURTH
C
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
C--------------------------------------------------
C   CALCUL DE AS N AU MILIEU DES COTES
C--------------------------------------------------
C        J=1 COTE
	VNRM(EP,1,1)=VQN(EP,7,1)+VQN(EP,7,2)
	VNRM(EP,2,1)=VQN(EP,8,1)+VQN(EP,8,2)
	VNRM(EP,3,1)=VQN(EP,9,1)+VQN(EP,9,2)
	C1=SQRT(VNRM(EP,1,1)*VNRM(EP,1,1)+
     1      VNRM(EP,2,1)*VNRM(EP,2,1)+VNRM(EP,3,1)*VNRM(EP,3,1))
        C1 = MAX(EM20,C1)
	VNRM(EP,1,1)=VNRM(EP,1,1)/C1
	VNRM(EP,2,1)=VNRM(EP,2,1)/C1
	VNRM(EP,3,1)=VNRM(EP,3,1)/C1
	VASTN(EP,1,1)=ZERO
	VASTN(EP,2,1)=L12
	VASTN(EP,3,1)=VASTN(EP,1,1)
	VASTN(EP,4,1)=VASTN(EP,2,1)
C        J=2 
	VNRM(EP,1,2)=VQN(EP,7,4)+VQN(EP,7,3)
	VNRM(EP,2,2)=VQN(EP,8,4)+VQN(EP,8,3)
	VNRM(EP,3,2)=VQN(EP,9,4)+VQN(EP,9,3)
	C1=SQRT(VNRM(EP,1,2)*VNRM(EP,1,2)+
     1      VNRM(EP,2,2)*VNRM(EP,2,2)+VNRM(EP,3,2)*VNRM(EP,3,2))
        C1 = MAX(EM20,C1)
	VNRM(EP,1,2)=VNRM(EP,1,2)/C1
	VNRM(EP,2,2)=VNRM(EP,2,2)/C1
	VNRM(EP,3,2)=VNRM(EP,3,2)/C1
	VASTN(EP,1,2)=ZERO
	VASTN(EP,2,2)=L34
	VASTN(EP,3,2)=VASTN(EP,1,2)
	VASTN(EP,4,2)=VASTN(EP,2,2)
C        J=3 
	VNRM(EP,1,3)=VQN(EP,7,1)+VQN(EP,7,4)
	VNRM(EP,2,3)=VQN(EP,8,1)+VQN(EP,8,4)
	VNRM(EP,3,3)=VQN(EP,9,1)+VQN(EP,9,4)
	C1=SQRT(VNRM(EP,1,3)*VNRM(EP,1,3)+
     1      VNRM(EP,2,3)*VNRM(EP,2,3)+VNRM(EP,3,3)*VNRM(EP,3,3))
        C1 = MAX(EM20,C1)
	VNRM(EP,1,3)=VNRM(EP,1,3)/C1
	VNRM(EP,2,3)=VNRM(EP,2,3)/C1
	VNRM(EP,3,3)=VNRM(EP,3,3)/C1
	VASTN(EP,1,3)=-(X41*VQN(EP,4,1)+Y41*VQN(EP,5,1)+Z41*VQN(EP,6,1))
	VASTN(EP,2,3)= X41*VQN(EP,1,1)+Y41*VQN(EP,2,1)+Z41*VQN(EP,3,1)
	VASTN(EP,3,3)=-(X41*VQN(EP,4,4)+Y41*VQN(EP,5,4)+Z41*VQN(EP,6,4))
	VASTN(EP,4,3)= X41*VQN(EP,1,4)+Y41*VQN(EP,2,4)+Z41*VQN(EP,3,4)
C        J=4 
	VNRM(EP,1,4)=VQN(EP,7,2)+VQN(EP,7,3)
	VNRM(EP,2,4)=VQN(EP,8,2)+VQN(EP,8,3)
	VNRM(EP,3,4)=VQN(EP,9,2)+VQN(EP,9,3)
	C1=SQRT(VNRM(EP,1,4)*VNRM(EP,1,4)+
     1      VNRM(EP,2,4)*VNRM(EP,2,4)+VNRM(EP,3,4)*VNRM(EP,3,4))
        C1 = MAX(EM20,C1)
	VNRM(EP,1,4)=VNRM(EP,1,4)/C1
	VNRM(EP,2,4)=VNRM(EP,2,4)/C1
	VNRM(EP,3,4)=VNRM(EP,3,4)/C1
	VASTN(EP,1,4)=-(X32*VQN(EP,4,2)+Y32*VQN(EP,5,2)+Z32*VQN(EP,6,2))
	VASTN(EP,2,4)= X32*VQN(EP,1,2)+Y32*VQN(EP,2,2)+Z32*VQN(EP,3,2)
	VASTN(EP,3,4)=-(X32*VQN(EP,4,3)+Y32*VQN(EP,5,3)+Z32*VQN(EP,6,3))
	VASTN(EP,4,4)= X32*VQN(EP,1,3)+Y32*VQN(EP,2,3)+Z32*VQN(EP,3,3)
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
        SL = SQRT(VQG(EP,1,1)*VQG(EP,1,1) + VQG(EP,2,1)*VQG(EP,2,1)
     1            + VQG(EP,3,1)*VQG(EP,3,1))
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
        SL = SQRT(VQG(EP,1,3)*VQG(EP,1,3) + VQG(EP,2,3)*VQG(EP,2,3)
     1            + VQG(EP,3,3)*VQG(EP,3,3))
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
        SL = SQRT(VQG(EP,1,2)*VQG(EP,1,2) + VQG(EP,2,2)*VQG(EP,2,2)
     1            + VQG(EP,3,2)*VQG(EP,3,2))
        IF ( SL/=0.) SL = 1. / SL
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
        SL = SQRT(VQG(EP,1,4)*VQG(EP,1,4) + VQG(EP,2,4)*VQG(EP,2,4)
     1            + VQG(EP,3,4)*VQG(EP,3,4))
        IF ( SL/=ZERO) SL = ONE / SL
        VQG(EP,1,4) = VQG(EP,1,4)*SL
        VQG(EP,2,4) = VQG(EP,2,4)*SL
        VQG(EP,3,4) = VQG(EP,3,4)*SL
        VQG(EP,4,4)= VQG(EP,8,4)*VQG(EP,3,4)-VQG(EP,9,4)*VQG(EP,2,4)
        VQG(EP,5,4)= VQG(EP,9,4)*VQG(EP,1,4)-VQG(EP,7,4)*VQG(EP,3,4)
        VQG(EP,6,4)= VQG(EP,7,4)*VQG(EP,2,4)-VQG(EP,8,4)*VQG(EP,1,4)
       AREA(EP)=JAC(EP,1)+JAC(EP,2)+JAC(EP,3)+JAC(EP,4)
      ENDDO
C
      DO I=JFT,JLT
       OFF(I)=OFFG(I)
      ENDDO
C
	 RETURN
 2001 FORMAT(/' ZERO OR NEGATIVE SHELL SUB-AREA : ELEMENT NB:',
     .          I8/)
	 END
