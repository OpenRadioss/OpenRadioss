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
Chd|  S6RCOOR3                      source/elements/thickshell/solide6c/s6rcoor3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|        S6CORTHO3                     source/elements/thickshell/solide6c/s6cortho3.F
Chd|        S6PROJ3                       source/elements/thickshell/solide6c/s6rcoor3.F
Chd|        VRROTA3                       source/elements/thickshell/solide6c/vrrota3.F
Chd|====================================================================
      SUBROUTINE S6RCOOR3(
     .   X  ,IXS , V  , W  ,GAMA0,GAMA,
     .   X1 , X2 , X3 , X4 , X5 ,  X6   ,
     .   Y1 , Y2 , Y3 , Y4 , Y5 ,  Y6   ,
     .   Z1 , Z2 , Z3 , Z4 , Z5 ,  Z6   ,
     .   VX1, VX2, VX3, VX4, VX5, VX6   ,
     .   VY1, VY2, VY3, VY4, VY5, VY6   ,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6   ,
     .   VD2,VIS,OFFG,OFF,SAV,RHO,RHOO  ,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NGL,MXT,NGEO,
     .   IOUTPRT, VGAX, VGAY, VGAZ, VGA2,DI,
     .   NEL, XGAX, XGAY, XGAZ, XGXA2, XGYA2, XGZA2,
     .   XGXYA, XGYZA, XGZXA, IPARG,GAMA_R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),V(3,*),W(3,*), VIS(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*),
     .  VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*),
     .  VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*),
     .  VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*),
     .  VD2(*), OFFG(*), OFF(*),  RHO(*), RHOO(*),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) ,
     .   GAMA0(NEL,6),GAMA(MVSIZ,6),
     .   VGAX(*), VGAY(*), VGAZ(*), VGA2(*),DI(MVSIZ,6),
     .   XGAX(*), XGAY(*), XGAZ(*),
     .   XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ),
     .   XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),GAMA_R(NEL,6)
      DOUBLE PRECISION
     .  SAV(NEL,15)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*),
     .        NC5(*), NC6(*),  MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*),IOUTPRT,IPARG(*)
CMasParINCLUDE 'scoor3.intmap.inc'
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DT05
      my_real
     .   G11(MVSIZ),G12(MVSIZ),G13(MVSIZ),
     .   G21(MVSIZ),G22(MVSIZ),G23(MVSIZ),
     .   G31(MVSIZ),G32(MVSIZ),G33(MVSIZ),
     .   T11(MVSIZ),T12(MVSIZ),T13(MVSIZ),
     .   T21(MVSIZ),T22(MVSIZ),T23(MVSIZ),
     .   T31(MVSIZ),T32(MVSIZ),T33(MVSIZ)
      my_real
     .   XL,YL,ZL
      my_real
     .   XX,YY,ZZ,XY,XZ,YZ,RTR(6),ABC,XXYZ2,ZZXY2,YYXZ2,DETA
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L  = ZERO
C
      DO I=LFT,LLT
      VIS(I)=ZERO
      NGEO(I)=IXS(10,I)
      NGL(I)=IXS(11,I)
      MXT(I)=IXS(1,I)
      NC1(I)=IXS(2,I)
      NC2(I)=IXS(3,I)
      NC3(I)=IXS(4,I)
      NC4(I)=IXS(6,I)
      NC5(I)=IXS(7,I)
      NC6(I)=IXS(8,I)
      RHOO(I)=RHO(I)
      ENDDO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO I=LFT,LLT
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
        X5(I)=X(1,NC5(I))
        Y5(I)=X(2,NC5(I))
        Z5(I)=X(3,NC5(I))
        X6(I)=X(1,NC6(I))
        Y6(I)=X(2,NC6(I))
        Z6(I)=X(3,NC6(I))
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
C-----------
      DO I=LFT,LLT
       VX1(I)=V(1,NC1(I))
       VY1(I)=V(2,NC1(I))
       VZ1(I)=V(3,NC1(I))
       VX2(I)=V(1,NC2(I))
       VY2(I)=V(2,NC2(I))
       VZ2(I)=V(3,NC2(I))
       VX3(I)=V(1,NC3(I))
       VY3(I)=V(2,NC3(I))
       VZ3(I)=V(3,NC3(I))
       VX4(I)=V(1,NC4(I))
       VY4(I)=V(2,NC4(I))
       VZ4(I)=V(3,NC4(I))
       VX5(I)=V(1,NC5(I))
       VY5(I)=V(2,NC5(I))
       VZ5(I)=V(3,NC5(I))
       VX6(I)=V(1,NC6(I))
       VY6(I)=V(2,NC6(I))
       VZ6(I)=V(3,NC6(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO I=LFT,LLT
          IF(OFFG(I)<ZERO)THEN
            VX1(I)=ZERO
            VY1(I)=ZERO
            VZ1(I)=ZERO
            VX2(I)=ZERO
            VY2(I)=ZERO
            VZ2(I)=ZERO
            VX3(I)=ZERO
            VY3(I)=ZERO
            VZ3(I)=ZERO
            VX4(I)=ZERO
            VY4(I)=ZERO
            VZ4(I)=ZERO
            VX5(I)=ZERO
            VY5(I)=ZERO
            VZ5(I)=ZERO
            VX6(I)=ZERO
            VY6(I)=ZERO
            VZ6(I)=ZERO
          ENDIF
        ENDDO
      ENDIF
C-----------
C     Prepare les sorties par part.
C-----------
      IF(IOUTPRT/=0)THEN
       DO I=LFT,LLT
        VGAX(I)=VX1(I)+VX2(I)+VX3(I)+VX4(I)+VX5(I)+VX6(I)
        VGAY(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)+VY5(I)+VY6(I)
        VGAZ(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+VZ5(I)+VZ6(I)
        VGA2(I)=VX1(I)*VX1(I)+VX2(I)*VX2(I)+VX3(I)*VX3(I)+VX4(I)*VX4(I)
     1         +VX5(I)*VX5(I)+VX6(I)*VX6(I)
     2         +VY1(I)*VY1(I)+VY2(I)*VY2(I)+VY3(I)*VY3(I)+VY4(I)*VY4(I)
     3         +VY5(I)*VY5(I)+VY6(I)*VY6(I)
     4         +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)+VZ3(I)*VZ3(I)+VZ4(I)*VZ4(I)
     5         +VZ5(I)*VZ5(I)+VZ6(I)*VZ6(I)
       ENDDO
       IF(IPARG(80)==1) THEN
         DO I=LFT,LLT
          XGAX(I)=X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)
          XGAY(I)=Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)
          XGAZ(I)=Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)
          XGXA2(I)=X1(I)**2+X2(I)**2+X3(I)**2+X4(I)**2
     1            +X5(I)**2+X6(I)**2
          XGYA2(I)=Y1(I)**2+Y2(I)**2+Y3(I)**2+Y4(I)**2
     1            +Y5(I)**2+Y6(I)**2
          XGZA2(I)=Z1(I)**2+Z2(I)**2+Z3(I)**2+Z4(I)**2
     1            +Z5(I)**2+Z6(I)**2
          XGXYA(I)=X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)+X4(I)*Y4(I)
     1            +X5(I)*Y5(I)+X6(I)*Y6(I)
          XGYZA(I)=Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)+Y4(I)*Z4(I)
     1            +Y5(I)*Z5(I)+Y6(I)*Z6(I)
          XGZXA(I)=Z1(I)*X1(I)+Z2(I)*X2(I)+Z3(I)*X3(I)+Z4(I)*X4(I)
     1            +Z5(I)*X5(I)+Z6(I)*X6(I)
         ENDDO
       ENDIF
      ENDIF
C  -----------
      DO I=LFT,LLT
        XL=ONE_OVER_6*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I))
        YL=ONE_OVER_6*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I))
        ZL=ONE_OVER_6*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I))
        X1(I)=X1(I)-XL
        Y1(I)=Y1(I)-YL
        Z1(I)=Z1(I)-ZL
        X2(I)=X2(I)-XL
        Y2(I)=Y2(I)-YL
        Z2(I)=Z2(I)-ZL
        X3(I)=X3(I)-XL
        Y3(I)=Y3(I)-YL
        Z3(I)=Z3(I)-ZL
        X4(I)=X4(I)-XL
        Y4(I)=Y4(I)-YL
        Z4(I)=Z4(I)-ZL
        X5(I)=X5(I)-XL
        Y5(I)=Y5(I)-YL
        Z5(I)=Z5(I)-ZL
        X6(I)=X6(I)-XL
        Y6(I)=Y6(I)-YL
        Z6(I)=Z6(I)-ZL
      ENDDO
C-----------
C     REPERE CONVECTE 
C-----------
       CALL S6CORTHO3(
     .   X1, X2, X3, X4, X5, X6,
     .   Y1, Y2, Y3, Y4, Y5, Y6,
     .   Z1, Z2, Z3, Z4, Z5, Z6,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ )
C------stocker [R] in %GAMA_R--------------     
       GAMA_R(LFT:LLT,1) = R11(LFT:LLT)
       GAMA_R(LFT:LLT,2) = R21(LFT:LLT)
       GAMA_R(LFT:LLT,3) = R31(LFT:LLT)
       GAMA_R(LFT:LLT,4) = R12(LFT:LLT)
       GAMA_R(LFT:LLT,5) = R22(LFT:LLT)
       GAMA_R(LFT:LLT,6) = R32(LFT:LLT)
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
C   X=RX' <=> X'=t(R)X chgt de base.
      IF(ISMSTR<=4.AND.JLAG>0) THEN
       DO I=LFT,LLT
       IF(OFFG(I)>ONE)THEN
        X1(I)=SAV(I,1)
        Y1(I)=SAV(I,2)
        Z1(I)=SAV(I,3)
        X2(I)=SAV(I,4)
        Y2(I)=SAV(I,5)
        Z2(I)=SAV(I,6)
        X3(I)=SAV(I,7)
        Y3(I)=SAV(I,8)
        Z3(I)=SAV(I,9)
        X4(I)=SAV(I,10)
        Y4(I)=SAV(I,11)
        Z4(I)=SAV(I,12)
        X5(I)=SAV(I,13)
        Y5(I)=SAV(I,14)
        Z5(I)=SAV(I,15)
        X6(I)=ZERO
        Y6(I)=ZERO
        Z6(I)=ZERO
        OFF(I) = OFFG(I) -ONE
        XL=ONE_OVER_6*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I))
        YL=ONE_OVER_6*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I))
        ZL=ONE_OVER_6*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I))
        X1(I)=X1(I)-XL
        Y1(I)=Y1(I)-YL
        Z1(I)=Z1(I)-ZL
        X2(I)=X2(I)-XL
        Y2(I)=Y2(I)-YL
        Z2(I)=Z2(I)-ZL
        X3(I)=X3(I)-XL
        Y3(I)=Y3(I)-YL
        Z3(I)=Z3(I)-ZL
        X4(I)=X4(I)-XL
        Y4(I)=Y4(I)-YL
        Z4(I)=Z4(I)-ZL
        X5(I)=X5(I)-XL
        Y5(I)=Y5(I)-YL
        Z5(I)=Z5(I)-ZL
        X6(I)=X6(I)-XL
        Y6(I)=Y6(I)-YL
        Z6(I)=Z6(I)-ZL
       ELSE
        XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
        YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
        X1(I)=XL
        Y1(I)=YL
        Z1(I)=ZL
        XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
        YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
        X2(I)=XL
        Y2(I)=YL
        Z2(I)=ZL
        XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
        YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
        X3(I)=XL
        Y3(I)=YL
        Z3(I)=ZL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
C          ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=-Z1(I)
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
C          ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=-Z2(I)
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
C          ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=-Z3(I)
C       SAV dans rep. local.
C       SAV(I,1)=X1(I)-X6(I)
C       SAV(I,2)=Y1(I)-Y6(I)
C       SAV(I,3)=Z1(I)-Z6(I)
C       SAV(I,4)=X2(I)-X6(I)
C       SAV(I,5)=Y2(I)-Y6(I)
C       SAV(I,6)=Z2(I)-Z6(I)
C       SAV(I,7)=X3(I)-X6(I)
C       SAV(I,8)=Y3(I)-Y6(I)
C       SAV(I,9)=Z3(I)-Z6(I)
C       SAV(I,10)=X4(I)-X6(I)
C       SAV(I,11)=Y4(I)-Y6(I)
C       SAV(I,12)=Z4(I)-Z6(I)
C       SAV(I,13)=X5(I)-X6(I)
C       SAV(I,14)=Y5(I)-Y6(I)
C       SAV(I,15)=Z5(I)-Z6(I)
        OFF(I) = OFFG(I)
       ENDIF
       ENDDO
C
      ELSE
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   X1, Y1, Z1)
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   X2, Y2, Z2)
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   X3, Y3, Z3)
       DO I=LFT,LLT
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
C          ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=-Z1(I)
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
C          ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=-Z2(I)
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
C          ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=-Z3(I)
        OFF(I) = MIN(ONE,OFFG(I))
       ENDDO
C
       ENDIF
C-----------
C     PASSAGE DES VITESSES AU REPERE CONVECTE (OU ORTHOTROPE).
C-----------
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   VX1, VY1, VZ1)
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   VX2, VY2, VZ2)
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   VX3, VY3, VZ3)
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   VX4, VY4, VZ4)
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   VX5, VY5, VZ5)
      CALL VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   VX6, VY6, VZ6)
C-----------
      DO I=LFT,LLT
        VD2(I) = ZERO
      ENDDO
C-----PROJECTION----
      DO I=LFT,LLT
        XX = X1(I)*X1(I)+X2(I)*X2(I)+X3(I)*X3(I)
     1      +X4(I)*X4(I)+X5(I)*X5(I)+X6(I)*X6(I)
        YY = Y1(I)*Y1(I)+Y2(I)*Y2(I)+Y3(I)*Y3(I)
     1      +Y4(I)*Y4(I)+Y5(I)*Y5(I)+Y6(I)*Y6(I)
        XY = X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)
     1      +X4(I)*Y4(I)+X5(I)*Y5(I)+X6(I)*Y6(I)
        XZ = X1(I)*Z1(I)+X2(I)*Z2(I)+X3(I)*Z3(I)
     1      +X4(I)*Z4(I)+X5(I)*Z5(I)+X6(I)*Z6(I)
        YZ = Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)
     1      +Y4(I)*Z4(I)+Y5(I)*Z5(I)+Y6(I)*Z6(I)
        ZZ = Z1(I)*Z1(I)+Z2(I)*Z2(I)+Z3(I)*Z3(I)
     1      +Z4(I)*Z4(I)+Z5(I)*Z5(I)+Z6(I)*Z6(I)
        RTR(1)= YY+ZZ
        RTR(2)= XX+ZZ
        RTR(3)= XX+YY
        RTR(4)= -XY
        RTR(5)= -XZ
        RTR(6)= -YZ
C       
        ABC = RTR(1)*RTR(2)*RTR(3)
        XXYZ2 = RTR(1)*RTR(6)*RTR(6)
        YYXZ2 = RTR(2)*RTR(5)*RTR(5)
        ZZXY2 = RTR(3)*RTR(4)*RTR(4)
        DETA = ABC + TWO*RTR(4)*RTR(5)*RTR(6)-XXYZ2-YYXZ2-ZZXY2
        IF (DETA<EM20) THEN
          DETA=ONE
        ELSE
         DETA=ONE/DETA
        ENDIF
        DI(I,1) = (ABC-XXYZ2)*DETA/RTR(1)
        DI(I,2) = (ABC-YYXZ2)*DETA/RTR(2)
        DI(I,3) = (ABC-ZZXY2)*DETA/RTR(3)
        DI(I,4) = (RTR(5)*RTR(6)-RTR(4)*RTR(3))*DETA
        DI(I,5) = (RTR(6)*RTR(4)-RTR(5)*RTR(2))*DETA
        DI(I,6) = (RTR(4)*RTR(5)-RTR(6)*RTR(1))*DETA
      ENDDO
      CALL S6PROJ3(
     .   X1, X2, X3, X4, X5, X6,
     .   Y1, Y2, Y3, Y4, Y5, Y6,
     .   Z1, Z2, Z3, Z4, Z5, Z6,
     .   VX1, VX2, VX3, VX4, VX5, VX6,
     .   VY1, VY2, VY3, VY4, VY5, VY6,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6,
     .   DI)
      RETURN
      END
C  |---- nouvelle routine---
Chd|====================================================================
Chd|  S6PROJ3                       source/elements/thickshell/solide6c/s6rcoor3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S6RCOOR3                      source/elements/thickshell/solide6c/s6rcoor3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6PROJ3(
     .   X1, X2, X3, X4, X5, X6,
     .   Y1, Y2, Y3, Y4, Y5, Y6,
     .   Z1, Z2, Z3, Z4, Z5, Z6,
     .   VX1, VX2, VX3, VX4, VX5, VX6,
     .   VY1, VY2, VY3, VY4, VY5, VY6,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6,
     .   DI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*),
     .  VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*),
     .  VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*),
     .  VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*),
     .  DI(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   XVY,XVZ,YVX,YVZ,ZVX,ZVY,
     .   AR(3),ALR(3)
C-----------------------------------------------
      DO I=LFT,LLT
        XVY= X1(I)*VY1(I)+X2(I)*VY2(I)+X3(I)*VY3(I)
     .      +X4(I)*VY4(I)+X5(I)*VY5(I)+X6(I)*VY6(I)
        XVZ= X1(I)*VZ1(I)+X2(I)*VZ2(I)+X3(I)*VZ3(I)
     .      +X4(I)*VZ4(I)+X5(I)*VZ5(I)+X6(I)*VZ6(I)
        YVX= Y1(I)*VX1(I)+Y2(I)*VX2(I)+Y3(I)*VX3(I)
     .      +Y4(I)*VX4(I)+Y5(I)*VX5(I)+Y6(I)*VX6(I)
        YVZ= Y1(I)*VZ1(I)+Y2(I)*VZ2(I)+Y3(I)*VZ3(I)
     .      +Y4(I)*VZ4(I)+Y5(I)*VZ5(I)+Y6(I)*VZ6(I)
        ZVY= Z1(I)*VY1(I)+Z2(I)*VY2(I)+Z3(I)*VY3(I)
     .      +Z4(I)*VY4(I)+Z5(I)*VY5(I)+Z6(I)*VY6(I)
        ZVX= Z1(I)*VX1(I)+Z2(I)*VX2(I)+Z3(I)*VX3(I)
     .      +Z4(I)*VX4(I)+Z5(I)*VX5(I)+Z6(I)*VX6(I)
        AR(1)=-ZVY+YVZ
        AR(2)= ZVX-XVZ
        AR(3)=-YVX+XVY
        ALR(1) =DI(I,1)*AR(1)+DI(I,4)*AR(2)+DI(I,5)*AR(3)
        ALR(2) =DI(I,4)*AR(1)+DI(I,2)*AR(2)+DI(I,6)*AR(3)
        ALR(3) =DI(I,5)*AR(1)+DI(I,6)*AR(2)+DI(I,3)*AR(3)
C
        VX1(I) = VX1(I)-(Z1(I)*ALR(2)-Y1(I)*ALR(3))
        VY1(I) = VY1(I)-(X1(I)*ALR(3)-Z1(I)*ALR(1))
        VZ1(I) = VZ1(I)-(Y1(I)*ALR(1)-X1(I)*ALR(2))
        VX2(I) = VX2(I)-(Z2(I)*ALR(2)-Y2(I)*ALR(3))
        VY2(I) = VY2(I)-(X2(I)*ALR(3)-Z2(I)*ALR(1))
        VZ2(I) = VZ2(I)-(Y2(I)*ALR(1)-X2(I)*ALR(2))
        VX3(I) = VX3(I)-(Z3(I)*ALR(2)-Y3(I)*ALR(3))
        VY3(I) = VY3(I)-(X3(I)*ALR(3)-Z3(I)*ALR(1))
        VZ3(I) = VZ3(I)-(Y3(I)*ALR(1)-X3(I)*ALR(2))
        VX4(I) = VX4(I)-(Z4(I)*ALR(2)-Y4(I)*ALR(3))
        VY4(I) = VY4(I)-(X4(I)*ALR(3)-Z4(I)*ALR(1))
        VZ4(I) = VZ4(I)-(Y4(I)*ALR(1)-X4(I)*ALR(2))
        VX5(I) = VX5(I)-(Z5(I)*ALR(2)-Y5(I)*ALR(3))
        VY5(I) = VY5(I)-(X5(I)*ALR(3)-Z5(I)*ALR(1))
        VZ5(I) = VZ5(I)-(Y5(I)*ALR(1)-X5(I)*ALR(2))
        VX6(I) = VX6(I)-(Z6(I)*ALR(2)-Y6(I)*ALR(3))
        VY6(I) = VY6(I)-(X6(I)*ALR(3)-Z6(I)*ALR(1))
        VZ6(I) = VZ6(I)-(Y6(I)*ALR(1)-X6(I)*ALR(2))
C
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  S6SAV3                        source/elements/thickshell/solide6c/s6rcoor3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6SAV3(OFFG   ,SAV   ,
     .   XD1   ,XD2   ,XD3   ,XD4   ,XD5   ,XD6   ,
     .   YD1   ,YD2   ,YD3   ,YD4   ,YD5   ,YD6   ,
     .   ZD1   ,ZD2   ,ZD3   ,ZD4   ,ZD5   ,ZD6   ,
     .   NEL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*), SAV(NEL,15)

      my_real
     .   XD1(*), XD2(*), XD3(*), XD4(*), XD5(*), XD6(*),
     .   YD1(*), YD2(*), YD3(*), YD4(*), YD5(*), YD6(*),
     .   ZD1(*), ZD2(*), ZD3(*), ZD4(*), ZD5(*), ZD6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
        DO I=LFT,LLT
          IF(ABS(OFFG(I)) <= ONE )THEN
           SAV(I,1)=XD1(I)-XD6(I)
           SAV(I,2)=YD1(I)-YD6(I)
           SAV(I,3)=ZD1(I)-ZD6(I)
           SAV(I,4)=XD2(I)-XD6(I)
           SAV(I,5)=YD2(I)-YD6(I)
           SAV(I,6)=ZD2(I)-ZD6(I)
           SAV(I,7)=XD3(I)-XD6(I)
           SAV(I,8)=YD3(I)-YD6(I)
           SAV(I,9)=ZD3(I)-ZD6(I)
           SAV(I,10)=XD4(I)-XD6(I)
           SAV(I,11)=YD4(I)-YD6(I)
           SAV(I,12)=ZD4(I)-ZD6(I)
           SAV(I,13)=XD5(I)-XD6(I)
           SAV(I,14)=YD5(I)-YD6(I)
           SAV(I,15)=ZD5(I)-ZD6(I)
          ENDIF
        ENDDO
C-----------
      RETURN
      END

