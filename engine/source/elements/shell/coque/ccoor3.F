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
Chd|  CCOOR3                        source/elements/shell/coque/ccoor3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCOOR3(JFT ,JLT ,X   ,IXC ,GEO ,OFFG  ,
     2                  OFF ,SIGY,PID,V,VR,VL1,VL2,VL3,VL4,
     3                  VRL1,VRL2,VRL3,VRL4,MAT,DT1C,THKE,THK0,NGL,
     4                  X1  ,X2  ,X3  ,X4  ,Y1  ,Y2    ,
     5                  Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4    )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, J
      INTEGER NGL(MVSIZ), IXC(NIXC,*),MAT(MVSIZ), PID(MVSIZ)
C     REAL
      my_real
     .   X(3,*), GEO(NPROPG,*), OFFG(*), OFF(*), SIGY(*),
     .   V(3,*),VR(3,*),VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3),DT1C(*),THKE(*),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), Y1(MVSIZ),
     .   Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Z1(MVSIZ), Z2(MVSIZ),
     .   Z3(MVSIZ), Z4(MVSIZ) ,THK0(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IXCTMP2,IXCTMP3,IXCTMP4,IXCTMP5
      INTEGER I, MAT_1
      my_real OFF_L      
C-----------------------------------------------
      MAT_1 = IXC(1,1)
      MAT(JFT:JLT)= MAT_1
c
      DO I=JFT,JLT      
        IXCTMP2=IXC(2,I)
        IXCTMP3=IXC(3,I)
        IXCTMP4=IXC(4,I)
        IXCTMP5=IXC(5,I)
        NGL(I)=IXC(7,I)
        PID(I)=IXC(6,I)
        THK0(I) = THKE(I)
C----------------------------
C     COORDONNEES
C----------------------------
        X1(I)=X(1,IXCTMP2)
        Y1(I)=X(2,IXCTMP2)
        Z1(I)=X(3,IXCTMP2)
        X2(I)=X(1,IXCTMP3)
        Y2(I)=X(2,IXCTMP3)
        Z2(I)=X(3,IXCTMP3)
        X3(I)=X(1,IXCTMP4)
        Y3(I)=X(2,IXCTMP4)
        Z3(I)=X(3,IXCTMP4)
        X4(I)=X(1,IXCTMP5)
        Y4(I)=X(2,IXCTMP5)
        Z4(I)=X(3,IXCTMP5)
        VL1(I,1)=V(1,IXCTMP2)
        VL1(I,2)=V(2,IXCTMP2)
        VL1(I,3)=V(3,IXCTMP2)
        VL2(I,1)=V(1,IXCTMP3)
        VL2(I,2)=V(2,IXCTMP3)
        VL2(I,3)=V(3,IXCTMP3)
        VL3(I,1)=V(1,IXCTMP4)
        VL3(I,2)=V(2,IXCTMP4)
        VL3(I,3)=V(3,IXCTMP4)
        VL4(I,1)=V(1,IXCTMP5)
        VL4(I,2)=V(2,IXCTMP5)
        VL4(I,3)=V(3,IXCTMP5)
        VRL1(I,1)=VR(1,IXCTMP2)
        VRL1(I,2)=VR(2,IXCTMP2)
        VRL1(I,3)=VR(3,IXCTMP2)
        VRL2(I,1)=VR(1,IXCTMP3)
        VRL2(I,2)=VR(2,IXCTMP3)
        VRL2(I,3)=VR(3,IXCTMP3)
        VRL3(I,1)=VR(1,IXCTMP4)
        VRL3(I,2)=VR(2,IXCTMP4)
        VRL3(I,3)=VR(3,IXCTMP4)
        VRL4(I,1)=VR(1,IXCTMP5)
        VRL4(I,2)=VR(2,IXCTMP5)
        VRL4(I,3)=VR(3,IXCTMP5)
      END DO
C
      OFF_L = ZERO
      DO I=JFT,JLT
        DT1C(I) = DT1
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
        SIGY(I) = EP30
      ENDDO
      IF(OFF_L<0.)THEN
        DO I=JFT,JLT
         IF(OFFG(I)<0.)THEN
           VL1(I,1)=ZERO
           VL1(I,2)=ZERO
           VL1(I,3)=ZERO
           VL2(I,1)=ZERO
           VL2(I,2)=ZERO
           VL2(I,3)=ZERO
           VL3(I,1)=ZERO
           VL3(I,2)=ZERO
           VL3(I,3)=ZERO
           VL4(I,1)=ZERO
           VL4(I,2)=ZERO
           VL4(I,3)=ZERO
           VRL1(I,1)=ZERO
           VRL1(I,2)=ZERO
           VRL1(I,3)=ZERO
           VRL2(I,1)=ZERO
           VRL2(I,2)=ZERO
           VRL2(I,3)=ZERO
           VRL3(I,1)=ZERO
           VRL3(I,2)=ZERO
           VRL3(I,3)=ZERO
           VRL4(I,1)=ZERO
           VRL4(I,2)=ZERO
           VRL4(I,3)=ZERO
         ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  CCOORT3                       source/elements/shell/coque/ccoor3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCOORT3(JFT     ,JLT   ,X     ,IXC    ,OFFG   ,
     1                    DR     ,PX1   ,PX2   ,PY1    ,PY2    ,
     2                    E1X    ,E1Y   ,E1Z   ,E2X    ,E2Y    ,
     3                    E2Z    ,E3X   ,E3Y   ,E3Z    ,AREA   , 
     4                    V13X   ,V24X  ,V13Y  ,V24Y   ,SMSTR  ,
     5                    NEL    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
      INTEGER IXC(NIXC,*)
      my_real
     .   X(3,*), OFFG(*), DR(3,*),
     .   E1X(*), E1Y(*), E1Z(*),
     .   E2X(*), E2Y(*), E2Z(*),E3X(*), E3Y(*), E3Z(*),
     .   SMSTR(*),AREA(*),PX1(*),PX2(*),PY1(*),PY2(*),
     .   V13X(*),V24X(*),V13Y(*),V24Y(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II(9),NN(4)
      my_real 
     .   X0G2(MVSIZ),X0G3(MVSIZ),X0G4(MVSIZ),Y0G2(MVSIZ),
     .   Y0G3(MVSIZ),Y0G4(MVSIZ),Z0G2(MVSIZ),Z0G3(MVSIZ),Z0G4(MVSIZ),
     .   OFF_L,VG13(3),VG24(3),
     .   XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),YL3(MVSIZ),
     .   YL4(MVSIZ),UXYZ(MVSIZ,3,4),A_I(MVSIZ)
C-----------------------------------------------
      DO I=1,9
        II(I) = NEL*(I-1)
      ENDDO
C
       DO I=JFT,JLT
          IF(ABS(OFFG(I))==ONE)OFFG(I)=SIGN(TWO,OFFG(I))
          UXYZ(I,1:3,1:4)= ZERO
            NN(1)=IXC(2,I)
            NN(2)=IXC(3,I)
            NN(3)=IXC(4,I)
            NN(4)=IXC(5,I)
            X0G2(I) = X(1,NN(2))-X(1,NN(1))
            Y0G2(I) = X(2,NN(2))-X(2,NN(1))
            Z0G2(I) = X(3,NN(2))-X(3,NN(1))
            X0G3(I) = X(1,NN(3))-X(1,NN(1))
            Y0G3(I) = X(2,NN(3))-X(2,NN(1))
            Z0G3(I) = X(3,NN(3))-X(3,NN(1))
            X0G4(I) = X(1,NN(4))-X(1,NN(1))
            Y0G4(I) = X(2,NN(4))-X(2,NN(1))
            Z0G4(I) = X(3,NN(4))-X(3,NN(1))
          IF(ABS(OFFG(I))==TWO)THEN
            UXYZ(I,1,2) = X0G2(I)-SMSTR(II(1)+I)
            UXYZ(I,2,2) = Y0G2(I)-SMSTR(II(2)+I)
            UXYZ(I,3,2) = Z0G2(I)-SMSTR(II(3)+I)
            UXYZ(I,1,3) = X0G3(I)-SMSTR(II(4)+I)
            UXYZ(I,2,3) = Y0G3(I)-SMSTR(II(5)+I)
            UXYZ(I,3,3) = Z0G3(I)-SMSTR(II(6)+I)
            UXYZ(I,1,4) = X0G4(I)-SMSTR(II(7)+I)
            UXYZ(I,2,4) = Y0G4(I)-SMSTR(II(8)+I)
            UXYZ(I,3,4) = Z0G4(I)-SMSTR(II(9)+I)
            
            X0G2(I) = SMSTR(II(1)+I)
            Y0G2(I) = SMSTR(II(2)+I)
            Z0G2(I) = SMSTR(II(3)+I)
            X0G3(I) = SMSTR(II(4)+I)
            Y0G3(I) = SMSTR(II(5)+I)
            Z0G3(I) = SMSTR(II(6)+I)
            X0G4(I) = SMSTR(II(7)+I)
            Y0G4(I) = SMSTR(II(8)+I)
            Z0G4(I) = SMSTR(II(9)+I)
C            
          ELSE
            SMSTR(II(1)+I)= X0G2(I)
            SMSTR(II(2)+I)= Y0G2(I)
            SMSTR(II(3)+I)= Z0G2(I)
            SMSTR(II(4)+I)= X0G3(I)
            SMSTR(II(5)+I)= Y0G3(I)
            SMSTR(II(6)+I)= Z0G3(I)
            SMSTR(II(7)+I)= X0G4(I)
            SMSTR(II(8)+I)= Y0G4(I)
            SMSTR(II(9)+I)= Z0G4(I)
          ENDIF
        ENDDO
C		
      DO I=JFT,JLT
        XL2(I)=E1X(I)*X0G2(I)+E1Y(I)*Y0G2(I)+E1Z(I)*Z0G2(I)
        XL3(I)=E1X(I)*X0G3(I)+E1Y(I)*Y0G3(I)+E1Z(I)*Z0G3(I)
        XL4(I)=E1X(I)*X0G4(I)+E1Y(I)*Y0G4(I)+E1Z(I)*Z0G4(I)
        YL2(I)=E2X(I)*X0G2(I)+E2Y(I)*Y0G2(I)+E2Z(I)*Z0G2(I)
        YL3(I)=E2X(I)*X0G3(I)+E2Y(I)*Y0G3(I)+E2Z(I)*Z0G3(I)
        YL4(I)=E2X(I)*X0G4(I)+E2Y(I)*Y0G4(I)+E2Z(I)*Z0G4(I)
      ENDDO
      DO I=JFT,JLT
       PX1(I)= HALF*(YL2(I)-YL4(I))
       PY1(I)= HALF*(XL4(I)-XL2(I))
       PX2(I)= HALF* YL3(I)
       PY2(I)=-HALF* XL3(I)     
       AREA(I)= MAX(TWO*(PY2(I)*PX1(I)-PY1(I)*PX2(I)),EM20)
       A_I(I) = ONE / AREA(I)
      ENDDO
C------  
      DO I=JFT,JLT
        VG13(1)=UXYZ(I,1,1)-UXYZ(I,1,3)
        VG24(1)=UXYZ(I,1,2)-UXYZ(I,1,4)
        VG13(2)=UXYZ(I,2,1)-UXYZ(I,2,3)
        VG24(2)=UXYZ(I,2,2)-UXYZ(I,2,4)
        VG13(3)=UXYZ(I,3,1)-UXYZ(I,3,3)
        VG24(3)=UXYZ(I,3,2)-UXYZ(I,3,4)
C
        V13X(I)=E1X(I)*VG13(1)+E1Y(I)*VG13(2)+E1Z(I)*VG13(3)
        V24X(I)=E1X(I)*VG24(1)+E1Y(I)*VG24(2)+E1Z(I)*VG24(3)
        V13Y(I)=E2X(I)*VG13(1)+E2Y(I)*VG13(2)+E2Z(I)*VG13(3)
        V24Y(I)=E2X(I)*VG24(1)+E2Y(I)*VG24(2)+E2Z(I)*VG24(3)
C	
       PX1(I)=A_I(I)*PX1(I) 
       PY1(I)=A_I(I)*PY1(I)
       PX2(I)=A_I(I)*PX2(I)
       PY2(I)=A_I(I)*PY2(I)
      ENDDO 
C
      OFF_L = ZERO
      DO I=JFT,JLT
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
      IF (OFF_L < ZERO) THEN
        DO I=JFT,JLT
         IF (OFFG(I) < ZERO) THEN
          V13X(I)=ZERO
          V24X(I)=ZERO
          V13Y(I)=ZERO
          V24Y(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
