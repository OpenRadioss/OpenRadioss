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
Chd|  CDK6COOR3                     source/elements/sh3n/coquedk6/cdk6coor3.F
Chd|-- called by -----------
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CDK6COOR3(ELBUF_STR,JFT,JLT,MAT,PID,
     .                    NGL,X,V,R,IXTG,OFFG,OFF,
     .                    R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .                    N4X,N4Y,N4Z,N5X,N5Y,N5Z,N6X,N6Y,N6Z,
     .                    XL2,YL2,XL3,YL3,XL4,YL4,ZL4,XL5,YL5,ZL5,
     .                    XL6,YL6,ZL6,
     .                    SMSTR,AREA,AREA2,
     .                    VLX,VLY,VLZ,VNZ4,VNZ5,VNZ6,ISMSTR,NLAY,IREP,
     .                    DIR_A ,DIR_B , IGEO  ,
     .                    IXTG1  ,NVS   ,IVS   ,AREA4 , AREA5 ,
     .                    AREA6  ,NEL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
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
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NLAY,ISMSTR,IREP,NEL
      INTEGER IXTG(NIXTG,*),MAT(*),PID(*),NGL(*),IXTG1(4,*),
     .        NVS,IVS(*),IGEO(NPROPGI,*)
C     REAL
      my_real
     .   X(3,*),V(3,*),R(3,*), OFFG(*), OFF(*),
     .   R11(*),R12(*),R13(*),R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),AREA(*),AREA2(*),
     .   N4X(*),N4Y(*),N4Z(*),N5X(*),N5Y(*),N5Z(*),N6X(*),N6Y(*),
     .   N6Z(*),AREA4(*),AREA5(*),AREA6(*),
     .   VLX(MVSIZ,2),VLY(MVSIZ,2),VLZ(MVSIZ,6),VNZ4(MVSIZ,3),VNZ5(MVSIZ,3),VNZ6(MVSIZ,3),
     .   XL2(*),XL3(*),YL2(*),YL3(*),XL4(*),XL5(*),YL4(*),YL5(*),
     .   XL6(*),YL6(*),ZL4(*),ZL5(*),ZL6(*),
     .   DIR_A(NEL,*),DIR_B(NEL,*)
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
      DOUBLE PRECISION
     .  SMSTR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC1, NC2, NC3, NC4(MVSIZ), NC5(MVSIZ), NC6(MVSIZ),
     .        I,II(13), J,I1, I2, I3, N ,SVS,JVS(MVSIZ),EP, MAT_1
C     REAL
      my_real
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), 
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ),
     .   VLX1(MVSIZ), VLX2(MVSIZ), VLX3(MVSIZ),
     .   VLY1(MVSIZ), VLY2(MVSIZ), VLY3(MVSIZ), 
     .   VX4(MVSIZ), VY4(MVSIZ), VZ4(MVSIZ),
     .   VX5(MVSIZ), VY5(MVSIZ), VZ5(MVSIZ),
     .   VX6(MVSIZ), VY6(MVSIZ), VZ6(MVSIZ),
     .   RX1(MVSIZ), RX2(MVSIZ), RX3(MVSIZ), RY1(MVSIZ),
     .   RY2(MVSIZ), RY3(MVSIZ), RZ1(MVSIZ), RZ2(MVSIZ),RZ3(MVSIZ),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ), 
     .   Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),Y5(MVSIZ),Y6(MVSIZ),
     .   Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ), 
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),
     .   VX0, VY0,VZ0,OFF_L,A,B,C ,DT05,EXZ,EYZ,VZ21,VZ31 ,     
     .   DDRX,DDRY,V21X,V31X,DDRZ1,DDRZ2
C=======================================================================
      DO I=1,13
        II(I) = NEL*(I-1)
      ENDDO
C
      MAT_1 = IXTG(1,JFT)
      DO I=JFT,JLT
       MAT(I)  = MAT_1
       NC1     = IXTG(2,I)
       NC2     = IXTG(3,I)
       NC3     = IXTG(4,I)
       PID(I)  = IXTG(5,I)
       NGL(I)  = IXTG(6,I)
C----------------------------
C     COORDINATES
C----------------------------
       X1(I)=X(1,NC1)
       Y1(I)=X(2,NC1)
       Z1(I)=X(3,NC1)
       X2(I)=X(1,NC2)
       Y2(I)=X(2,NC2)
       Z2(I)=X(3,NC2)
       X3(I)=X(1,NC3)
       Y3(I)=X(2,NC3)
       Z3(I)=X(3,NC3)
C----------------------------
C     VELOCITY
C----------------------------
       VX1(I)=V(1,NC1)
       VY1(I)=V(2,NC1)
       VZ1(I)=V(3,NC1)
       VX2(I)=V(1,NC2)
       VY2(I)=V(2,NC2)
       VZ2(I)=V(3,NC2)
       VX3(I)=V(1,NC3)
       VY3(I)=V(2,NC3)
       VZ3(I)=V(3,NC3)
      ENDDO 
C----------------------------
C     vectoriser elements avec ou sans voisins
C-----------------------------------------------
      NVS=JFT-1
      SVS= NVS
      DO I=JFT,JLT
        NC4(I)     = IXTG1(1,I)
        NC5(I)     = IXTG1(2,I)
        NC6(I)     = IXTG1(3,I)
        IF (NC4(I) > 0 .AND. NC5(I) > 0 .AND. NC6(I) > 0) THEN
          NVS=NVS+1
          IVS(NVS)=I
        ELSE
          SVS=SVS+1
          JVS(SVS)=I
        ENDIF
      ENDDO 
      DO I=NVS+1,JLT
        IVS(I)=JVS(I-NVS)
      ENDDO 
C----------Central Element----------
      DO I=JFT,JLT
       RX(I)=X2(I)-X1(I)
       RY(I)=Y2(I)-Y1(I)
       RZ(I)=Z2(I)-Z1(I)
       SX(I)=X3(I)-X1(I)
       SY(I)=Y3(I)-Y1(I)
       SZ(I)=Z3(I)-Z1(I)
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      I1 = 0
      CALL CLSKEW3(JFT,JLT,I1,
     .   RX, RY, RZ, 
     .   SX, SY, SZ, 
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,AREA2,OFFG )
C
      DO I=JFT,JLT
        XL2(I)=R11(I)*RX(I)+R21(I)*RY(I)+R31(I)*RZ(I)
        YL2(I)=R12(I)*RX(I)+R22(I)*RY(I)+R32(I)*RZ(I)
        XL3(I)=R11(I)*SX(I)+R21(I)*SY(I)+R31(I)*SZ(I)
        YL3(I)=R12(I)*SX(I)+R22(I)*SY(I)+R32(I)*SZ(I)
        AREA(I)=HALF*AREA2(I)
      ENDDO
C----------------------------
C     SMALL STRAIN
C----------------------------
      IF (ISMSTR == 1 .OR. ISMSTR == 2) THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I)) == TWO)THEN
            XL2(I)=SMSTR(II(1)+I)
            YL2(I)=SMSTR(II(2)+I)
            XL3(I)=SMSTR(II(3)+I)
            YL3(I)=SMSTR(II(4)+I)
            AREA2(I)=XL2(I)*YL3(I)-XL3(I)*YL2(I)
            AREA(I)=HALF*AREA2(I)
          ELSE
            SMSTR(II(1)+I)=XL2(I)
            SMSTR(II(2)+I)=YL2(I)
            SMSTR(II(3)+I)=XL3(I)
            SMSTR(II(4)+I)=YL3(I)
         ENDIF
        ENDDO
      ENDIF
      IF (ISMSTR == 1) THEN
        DO I=JFT,JLT
          IF (OFFG(I) == ONE) OFFG(I)=TWO
        ENDDO
      ENDIF
C----------Element edges----------
C--------with adjacent elems---------------------------------------
      DO EP=JFT,NVS
        I =IVS(EP)
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
      DO EP=JFT,NVS
        I =IVS(EP)
        X4(I)=X(1,NC4(I))-X1(I)
        Y4(I)=X(2,NC4(I))-Y1(I)
        Z4(I)=X(3,NC4(I))-Z1(I)
        X5(I)=X(1,NC5(I))-X2(I)
        Y5(I)=X(2,NC5(I))-Y2(I)
        Z5(I)=X(3,NC5(I))-Z2(I)
        X6(I)=X(1,NC6(I))-X3(I)
        Y6(I)=X(2,NC6(I))-Y3(I)
        Z6(I)=X(3,NC6(I))-Z3(I)
      ENDDO
C--------without adjacent elems---------------------------------------
      DO EP=NVS+1,JLT
        I =IVS(EP)
C--------without edge node 4 ---------------------------------------
       IF (NC4(I) <= 0) THEN
        VX4(I)=ZERO
        VY4(I)=ZERO
        VZ4(I)=ZERO
        X4(I)=SX(I)
        Y4(I)=SY(I)
        Z4(I)=SZ(I)
       ELSE
        VX4(I)=V(1,NC4(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
        X4(I)=X(1,NC4(I))-X1(I)
        Y4(I)=X(2,NC4(I))-Y1(I)
        Z4(I)=X(3,NC4(I))-Z1(I)
       ENDIF
       IF (NC5(I) <= 0) THEN
        VX5(I)=ZERO
        VY5(I)=ZERO
        VZ5(I)=ZERO
        X5(I)=X1(I)
        Y5(I)=Y1(I)
        Z5(I)=Z1(I)
       ELSE
        VX5(I)=V(1,NC5(I))
        VY5(I)=V(2,NC5(I))
        VZ5(I)=V(3,NC5(I))
        X5(I)=X(1,NC5(I))-X2(I)
        Y5(I)=X(2,NC5(I))-Y2(I)
        Z5(I)=X(3,NC5(I))-Z2(I)
       ENDIF
       IF (NC6(I) <= 0) THEN
        VX6(I)=ZERO
        VY6(I)=ZERO
        VZ6(I)=ZERO
        X6(I)=RX(I)
        Y6(I)=RY(I)
        Z6(I)=RZ(I)
       ELSE
        VX6(I)=V(1,NC6(I))
        VY6(I)=V(2,NC6(I))
        VZ6(I)=V(3,NC6(I))
        X6(I)=X(1,NC6(I))-X3(I)
        Y6(I)=X(2,NC6(I))-Y3(I)
        Z6(I)=X(3,NC6(I))-Z3(I)
       ENDIF
      ENDDO
C
      DO I=JFT,JLT
        XL4(I)=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL4(I)=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        ZL4(I)=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        XL5(I)=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL5(I)=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        ZL5(I)=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        XL6(I)=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL6(I)=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        ZL6(I)=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
      ENDDO
C----------------------------
C     SMALL STRAIN
C----------------------------
      IF (ISMSTR == 1 .OR. ISMSTR == 2) THEN
         DO I=JFT,JLT
          IF (ABS(OFFG(I)) == TWO) THEN
            XL4(I)=SMSTR(II(5)+I)
            YL4(I)=SMSTR(II(6)+I)
            ZL4(I)=SMSTR(II(7)+I)
            XL5(I)=SMSTR(II(8)+I)
            YL5(I)=SMSTR(II(9)+I)
            ZL5(I)=SMSTR(II(10)+I)
            XL6(I)=SMSTR(II(11)+I)
            YL6(I)=SMSTR(II(12)+I)
            ZL6(I)=SMSTR(II(13)+I)
          ELSE
            SMSTR(II(5)+I) =XL4(I)
            SMSTR(II(6)+I) =YL4(I)
            SMSTR(II(7)+I) =ZL4(I)
            SMSTR(II(8)+I) =XL5(I)
            SMSTR(II(9)+I) =YL5(I)
            SMSTR(II(10)+I)=ZL5(I)
            SMSTR(II(11)+I)=XL6(I)
            SMSTR(II(12)+I)=YL6(I)
            SMSTR(II(13)+I)=ZL6(I)
         ENDIF
        ENDDO
      ENDIF
C----------------------------
C     VELOCITY Global->Local
C----------------------------
      DO I=JFT,JLT
        VX0=R11(I)*VX1(I)+R21(I)*VY1(I)+R31(I)*VZ1(I)
        VY0=R12(I)*VX1(I)+R22(I)*VY1(I)+R32(I)*VZ1(I)
        VZ0=R13(I)*VX1(I)+R23(I)*VY1(I)+R33(I)*VZ1(I)
        VX1(I)=VX0
        VY1(I)=VY0
        VZ1(I)=VZ0
        VX0=R11(I)*VX2(I)+R21(I)*VY2(I)+R31(I)*VZ2(I)
        VY0=R12(I)*VX2(I)+R22(I)*VY2(I)+R32(I)*VZ2(I)
        VZ0=R13(I)*VX2(I)+R23(I)*VY2(I)+R33(I)*VZ2(I)
        VX2(I)=VX0
        VY2(I)=VY0
        VZ2(I)=VZ0
        VX0=R11(I)*VX3(I)+R21(I)*VY3(I)+R31(I)*VZ3(I)
        VY0=R12(I)*VX3(I)+R22(I)*VY3(I)+R32(I)*VZ3(I)
        VZ0=R13(I)*VX3(I)+R23(I)*VY3(I)+R33(I)*VZ3(I)
        VX3(I)=VX0
        VY3(I)=VY0
        VZ3(I)=VZ0
        VX0=R11(I)*VX4(I)+R21(I)*VY4(I)+R31(I)*VZ4(I)
        VY0=R12(I)*VX4(I)+R22(I)*VY4(I)+R32(I)*VZ4(I)
        VZ0=R13(I)*VX4(I)+R23(I)*VY4(I)+R33(I)*VZ4(I)
        VX4(I)=VX0
        VY4(I)=VY0
        VZ4(I)=VZ0
        VX0=R11(I)*VX5(I)+R21(I)*VY5(I)+R31(I)*VZ5(I)
        VY0=R12(I)*VX5(I)+R22(I)*VY5(I)+R32(I)*VZ5(I)
        VZ0=R13(I)*VX5(I)+R23(I)*VY5(I)+R33(I)*VZ5(I)
        VX5(I)=VX0
        VY5(I)=VY0
        VZ5(I)=VZ0
        VX0=R11(I)*VX6(I)+R21(I)*VY6(I)+R31(I)*VZ6(I)
        VY0=R12(I)*VX6(I)+R22(I)*VY6(I)+R32(I)*VZ6(I)
        VZ0=R13(I)*VX6(I)+R23(I)*VY6(I)+R33(I)*VZ6(I)
        VX6(I)=VX0
        VY6(I)=VY0
        VZ6(I)=VZ0
      ENDDO
C---------normal vector of adjacent elems---
      DO I=JFT,JLT
C---------node 4---
        A =-YL2(I)*ZL4(I)
        B = XL2(I)*ZL4(I)
        C = XL4(I)*YL2(I)-XL2(I)*YL4(I)
        AREA4(I)=ONE/SQRT(A*A+B*B+C*C)
        N4X(I)=A*AREA4(I)
        N4Y(I)=B*AREA4(I)
        N4Z(I)=C*AREA4(I)
C---------node 5---
        A =-(YL3(I)-YL2(I))*ZL5(I)
        B = (XL3(I)-XL2(I))*ZL5(I)
        C = XL5(I)*(YL3(I)-YL2(I))-(XL3(I)-XL2(I))*YL5(I)
        AREA5(I)=ONE/SQRT(A*A+B*B+C*C)
        N5X(I)=A*AREA5(I)
        N5Y(I)=B*AREA5(I)
        N5Z(I)=C*AREA5(I)
C---------node 6---
        A = YL3(I)*ZL6(I)
        B = -XL3(I)*ZL6(I)
        C = -XL6(I)*YL3(I)+XL3(I)*YL6(I)
        AREA6(I)=ONE/SQRT(A*A+B*B+C*C)
        N6X(I)=A*AREA6(I)
        N6Y(I)=B*AREA6(I)
        N6Z(I)=C*AREA6(I)
      ENDDO
      DO EP=NVS+1,JLT
       I =IVS(EP)
       IF (NC4(I) <= 0) N4Z(I)=ONE
       IF (NC5(I) <= 0) N5Z(I)=ONE
       IF (NC6(I) <= 0) N6Z(I)=ONE
      ENDDO
      DO I=JFT,JLT
        VLZ(I,4)=N4X(I)*VX4(I)+N4Y(I)*VY4(I)+N4Z(I)*VZ4(I)
        VLZ(I,5)=N5X(I)*VX5(I)+N5Y(I)*VY5(I)+N5Z(I)*VZ5(I)
        VLZ(I,6)=N6X(I)*VX6(I)+N6Y(I)*VY6(I)+N6Z(I)*VZ6(I)
C
        VNZ4(I,1)=N4X(I)*VX1(I)+N4Y(I)*VY1(I)+N4Z(I)*VZ1(I)
        VNZ4(I,2)=N4X(I)*VX2(I)+N4Y(I)*VY2(I)+N4Z(I)*VZ2(I)
        VNZ4(I,3)=N4X(I)*VX3(I)+N4Y(I)*VY3(I)+N4Z(I)*VZ3(I)
        VNZ5(I,1)=N5X(I)*VX1(I)+N5Y(I)*VY1(I)+N5Z(I)*VZ1(I)
        VNZ5(I,2)=N5X(I)*VX2(I)+N5Y(I)*VY2(I)+N5Z(I)*VZ2(I)
        VNZ5(I,3)=N5X(I)*VX3(I)+N5Y(I)*VY3(I)+N5Z(I)*VZ3(I)
        VNZ6(I,1)=N6X(I)*VX1(I)+N6Y(I)*VY1(I)+N6Z(I)*VZ1(I)
        VNZ6(I,2)=N6X(I)*VX2(I)+N6Y(I)*VY2(I)+N6Z(I)*VZ2(I)
        VNZ6(I,3)=N6X(I)*VX3(I)+N6Y(I)*VY3(I)+N6Z(I)*VZ3(I)
      ENDDO
      DO I=JFT,JLT
        VLX(I,1)=VX2(I)-VX1(I)
        VLX(I,2)=VX3(I)-VX1(I)
        VLY(I,1)=VY2(I)-VY1(I)
        VLY(I,2)=VY3(I)-VY1(I)
        VLZ(I,1)=VZ1(I)
        VLZ(I,2)=VZ2(I)
        VLZ(I,3)=VZ3(I)
      ENDDO
C----------------------------
C     ORTHOTROPY
C----------------------------
      IF (IREP > 0) THEN
        CALL CORTDIR3(ELBUF_STR,DIR_A,DIR_B ,JFT    ,JLT    ,
     .                NLAY   ,IREP   ,RX    ,RY     ,RZ     , 
     .                SX     ,SY     ,SZ    ,R11    ,R21    ,
     .                R31    ,R12    ,R22   ,R32    ,NEL    )
      ENDIF
C--------------------------
C-------Correction 2nd order rigid rotation due a V(t+dt/2),X(t+dt)----
C--------------------------
      DT05 = HALF*DT1                                  
      DO I=JFT,JLT                                     
        VZ21=VZ2(I)-VZ1(I)
        VZ31=VZ3(I)-VZ1(I)
        EXZ =  YL3(I)*VZ21-YL2(I)*VZ31
        EYZ = -XL3(I)*VZ21+XL2(I)*VZ31
        DDRY=DT05*EXZ/AREA2(I)
        DDRX=DT05*EYZ/AREA2(I)
        V21X = VLX(I,1)
        V31X = VLX(I,2)                         
        DDRZ1=DT05*VLY(I,1)/XL2(I)
        DDRZ2=DT05*V31X/YL3(I)
        VLX(I,1) = VLX(I,1)-DDRY*VZ21-DDRZ1*VLY(I,1)   
        VLX(I,2) = VLX(I,2)-DDRY*VZ31-DDRZ1*VLY(I,2)
        VLY(I,1) = VLY(I,1)-DDRX*VZ21-DDRZ2*V21X
        VLY(I,2) = VLY(I,2)-DDRX*VZ31-DDRZ2*V31X
      ENDDO                                            
C----------------------------
      OFF_L = ZERO
      DO I=JFT,JLT
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L < ZERO)THEN
        DO I=JFT,JLT
         IF(OFFG(I) < ZERO)THEN
          VLX(I,1) =ZERO
          VLX(I,2) =ZERO
          VLY(I,1) =ZERO
          VLY(I,2) =ZERO
          VLZ(I,1) =ZERO
          VLZ(I,2) =ZERO
          VLZ(I,3) =ZERO
          VLZ(I,4) =ZERO
          VLZ(I,5) =ZERO
          VLZ(I,6) =ZERO
          VNZ4(I,1)=ZERO
          VNZ4(I,2)=ZERO
          VNZ4(I,3)=ZERO
          VNZ5(I,1)=ZERO
          VNZ5(I,2)=ZERO
          VNZ5(I,3)=ZERO
          VNZ6(I,1)=ZERO
          VNZ6(I,2)=ZERO
          VNZ6(I,3)=ZERO
         ENDIF
        ENDDO
      ENDIF
C----------------------------
      RETURN
      END
