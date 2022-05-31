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
Chd|  C3COOR3                       source/elements/sh3n/coque3n/c3coor3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3COOR3(JFT      ,JLT      ,X       ,IXTG     ,
     .                   OFFG     ,OFF      ,DT1C    ,
     .                   V        ,VR       ,VL1     ,VL2      ,VL3      ,
     .                   VRL1     ,VRL2     ,VRL3    ,SIGY     ,
     .                   X1       ,X2       ,X3      ,Y1       ,Y2       ,
     .                   Y3       ,Z1       ,Z2      ,Z3       ,XDP      )  
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      INTEGER IXTG(NIXTG,*)
      my_real
     .   OFFG(*), OFF(*),DT1C(*),X(3,*),
     .   V(3,*),VR(3,*),VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),SIGY(*)
!       SP issue :
      REAL(kind=8), DIMENSION(3,*), INTENT(in) :: XDP
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(out) ::X1,X2,X3
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(out) ::Y1,Y2,Y3
      REAL(kind=8), DIMENSION(MVSIZ), INTENT(out) ::Z1,Z2,Z3


C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NC1, NC2, NC3
      my_real OFF_L    
c=======================================================================
      IF(IRESP == 1)THEN
       DO I=JFT,JLT
        NC1     = IXTG(2,I) 
        NC2     = IXTG(3,I) 
        NC3     = IXTG(4,I) 
C----------------------------
C       COORDONNEES
C----------------------------
        X1(I)=XDP(1,NC1)
        Y1(I)=XDP(2,NC1)
        Z1(I)=XDP(3,NC1)
        X2(I)=XDP(1,NC2)
        Y2(I)=XDP(2,NC2)
        Z2(I)=XDP(3,NC2)
        X3(I)=XDP(1,NC3)
        Y3(I)=XDP(2,NC3)
        Z3(I)=XDP(3,NC3)
        VL1(I,1)=V(1,NC1)
        VL1(I,2)=V(2,NC1)
        VL1(I,3)=V(3,NC1)
        VL2(I,1)=V(1,NC2)
        VL2(I,2)=V(2,NC2)
        VL2(I,3)=V(3,NC2)
        VL3(I,1)=V(1,NC3)
        VL3(I,2)=V(2,NC3)
        VL3(I,3)=V(3,NC3)
        VRL1(I,1)=VR(1,NC1)
        VRL1(I,2)=VR(2,NC1)
        VRL1(I,3)=VR(3,NC1)
        VRL2(I,1)=VR(1,NC2)
        VRL2(I,2)=VR(2,NC2)
        VRL2(I,3)=VR(3,NC2)
        VRL3(I,1)=VR(1,NC3)
        VRL3(I,2)=VR(2,NC3)
        VRL3(I,3)=VR(3,NC3)
       ENDDO
      ELSE
       DO I=JFT,JLT
        NC1     = IXTG(2,I) 
        NC2     = IXTG(3,I) 
        NC3     = IXTG(4,I) 
C----------------------------
C       COORDONNEES
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
        VL1(I,1)=V(1,NC1)
        VL1(I,2)=V(2,NC1)
        VL1(I,3)=V(3,NC1)
        VL2(I,1)=V(1,NC2)
        VL2(I,2)=V(2,NC2)
        VL2(I,3)=V(3,NC2)
        VL3(I,1)=V(1,NC3)
        VL3(I,2)=V(2,NC3)
        VL3(I,3)=V(3,NC3)
        VRL1(I,1)=VR(1,NC1)
        VRL1(I,2)=VR(2,NC1)
        VRL1(I,3)=VR(3,NC1)
        VRL2(I,1)=VR(1,NC2)
        VRL2(I,2)=VR(2,NC2)
        VRL2(I,3)=VR(3,NC2)
        VRL3(I,1)=VR(1,NC3)
        VRL3(I,2)=VR(2,NC3)
        VRL3(I,3)=VR(3,NC3)
       ENDDO
      END IF!(IRESP == 1)THEN
C
      OFF_L = ZERO
      DO I=JFT,JLT
        DT1C(I) = DT1
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
        SIGY(I) = EP30
      ENDDO
      IF (OFF_L < ZERO) THEN
        DO I=JFT,JLT
         IF (OFFG(I) < ZERO) THEN
           VL1(I,1)=ZERO
           VL1(I,2)=ZERO
           VL1(I,3)=ZERO
           VL2(I,1)=ZERO
           VL2(I,2)=ZERO
           VL2(I,3)=ZERO
           VL3(I,1)=ZERO
           VL3(I,2)=ZERO
           VL3(I,3)=ZERO
           VRL1(I,1)=ZERO
           VRL1(I,2)=ZERO
           VRL1(I,3)=ZERO
           VRL2(I,1)=ZERO
           VRL2(I,2)=ZERO
           VRL2(I,3)=ZERO
           VRL3(I,1)=ZERO
           VRL3(I,2)=ZERO
           VRL3(I,3)=ZERO
         ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  C3COORT3                      source/elements/sh3n/coque3n/c3coor3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3COORT3(JFT    ,JLT    ,X     ,IXTG   ,OFFG   ,
     1                    DR     ,XL2    ,XL3   ,YL2    ,YL3    ,
     2                    E1X    ,E1Y    ,E1Z   ,E2X    ,E2Y    ,
     3                    E2Z    ,E3X    ,E3Y   ,E3Z    ,NEL    ,
     5                    V21X   ,V31X   ,V21Y  ,V31Y   ,RZ13   ,
     6                    RZ23   ,X2_T   ,X3_T  ,Y2_T   ,Y3_T   ,
     7                    AREA   ,SMSTR  ,ISROT )
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,ISROT,NEL
      INTEGER IXTG(NIXTG,*)
      my_real
     .   X(3,*), OFFG(*), DR(3,*),
     .   E1X(*), E1Y(*), E1Z(*),
     .   E2X(*), E2Y(*), E2Z(*),E3X(*), E3Y(*), E3Z(*),
     .   XL2(*),XL3(*),YL2(*),YL3(*),AREA(*),
     .   V21X(*),V31X(*),V21Y(*),V31Y(*),RZ13(*),RZ23(*),
     .   X2_T(*),X3_T(*),Y2_T(*),Y3_T(*)
      DOUBLE PRECISION 
     .   SMSTR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NC1, NC2, NC3,II(6),NN(3)
C     REAL ou REAL*8
      my_real 
     .   X0G2(MVSIZ),X0G3(MVSIZ),Y0G2(MVSIZ),Y0G3(MVSIZ),OFF_L,
     .   Z0G2(MVSIZ),Z0G3(MVSIZ),AXYZ(MVSIZ,3,3),
     .   E01X(MVSIZ), E01Y(MVSIZ), E01Z(MVSIZ),
     .   E02X(MVSIZ), E02Y(MVSIZ), E02Z(MVSIZ),E03X(MVSIZ), 
     .   E03Y(MVSIZ), E03Z(MVSIZ),X0L2(MVSIZ), X0L3(MVSIZ),
     .   Y0L2(MVSIZ),Y0L3(MVSIZ),SUM(MVSIZ),NORM,XL,YL,VR1_12,VR1_21,
     .   RLZ1,RLZ2,RLZ3,AREAI,X0G32,Y0G32,Z0G32,DIRZ(MVSIZ,2)
C-----------------------------------------------
c=======================================================================
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
C
       DO I=JFT,JLT
          IF(ABS(OFFG(I))==ONE)OFFG(I)=SIGN(TWO,OFFG(I))
          AXYZ(I,1:3,1:3)= ZERO
C            
           IF (ISROT > 0  ) THEN
            NN(1)     = IXTG(2,I) 
            NN(2)     = IXTG(3,I) 
            NN(3)     = IXTG(4,I) 
            AXYZ(I,1,1) = DR(1,NN(1))
            AXYZ(I,2,1) = DR(2,NN(1))
            AXYZ(I,3,1) = DR(3,NN(1))
            AXYZ(I,1,2) = DR(1,NN(2))
            AXYZ(I,2,2) = DR(2,NN(2))
            AXYZ(I,3,2) = DR(3,NN(2))
            AXYZ(I,1,3) = DR(1,NN(3))
            AXYZ(I,2,3) = DR(2,NN(3))
            AXYZ(I,3,3) = DR(3,NN(3))
           END IF !(ISROT > 0  ) THEN
            
            X0G2(I) = SMSTR(II(1)+I)
            Y0G2(I) = SMSTR(II(2)+I)
            Z0G2(I) = SMSTR(II(3)+I)
            X0G3(I) = SMSTR(II(4)+I)
            Y0G3(I) = SMSTR(II(5)+I)
            Z0G3(I) = SMSTR(II(6)+I)
        ENDDO
C--       normal in initial conf. 
      DO I=JFT,JLT
        E01X(I)= X0G2(I)
        E01Y(I)= Y0G2(I)
        E01Z(I)= Z0G2(I)
        SUM(I) = SQRT(E01X(I)*E01X(I)+E01Y(I)*E01Y(I)+E01Z(I)*E01Z(I))
        E01X(I)=E01X(I)/SUM(I)
        E01Y(I)=E01Y(I)/SUM(I)
        E01Z(I)=E01Z(I)/SUM(I)
      ENDDO
C
      DO I=JFT,JLT
        X0G32=X0G3(I)-X0G2(I)
        Y0G32=Y0G3(I)-Y0G2(I)
        Z0G32=Z0G3(I)-Z0G2(I)
        E03X(I)=Y0G3(I)*Z0G32-Z0G3(I)*Y0G32
        E03Y(I)=Z0G3(I)*X0G32-X0G3(I)*Z0G32
        E03Z(I)=X0G3(I)*Y0G32-Y0G3(I)*X0G32
        SUM(I) = SQRT(E03X(I)*E03X(I)+E03Y(I)*E03Y(I)+E03Z(I)*E03Z(I))
        E03X(I)=E03X(I)/SUM(I)
        E03Y(I)=E03Y(I)/SUM(I)
        E03Z(I)=E03Z(I)/SUM(I)
        AREA(I) = HALF * SUM(I)
      ENDDO
C
      DO I=JFT,JLT
        E02X(I)=E03Y(I)*E01Z(I)-E03Z(I)*E01Y(I)
        E02Y(I)=E03Z(I)*E01X(I)-E03X(I)*E01Z(I)
        E02Z(I)=E03X(I)*E01Y(I)-E03Y(I)*E01X(I)
        SUM(I) = SQRT(E02X(I)*E02X(I)+E02Y(I)*E02Y(I)+E02Z(I)*E02Z(I))
        E02X(I)=E02X(I)/SUM(I)
        E02Y(I)=E02Y(I)/SUM(I)
        E02Z(I)=E02Z(I)/SUM(I)
      ENDDO
C----------------------------
C     xl =VR1^t x0l; VR1^t=(VQ0^t*VQ)^t---extract Rzl of VR1
C----------------------------
      DO I=JFT,JLT
       VR1_12=E01X(I)*E2X(I)+E01Y(I)*E2Y(I)+E01Z(I)*E2Z(I)
       VR1_21=E02X(I)*E1X(I)+E02Y(I)*E1Y(I)+E02Z(I)*E1Z(I)
       DIRZ(I,2) = HALF*(VR1_12-VR1_21)
       NORM = ONE-DIRZ(I,2)*DIRZ(I,2)
       DIRZ(I,1) = SQRT(MAX(ZERO,NORM))
      ENDDO 
      DO I=JFT,JLT
        X0L2(I)=E01X(I)*X0G2(I)+E01Y(I)*Y0G2(I)+E01Z(I)*Z0G2(I)
        Y0L2(I)=E02X(I)*X0G2(I)+E02Y(I)*Y0G2(I)+E02Z(I)*Z0G2(I)
        X0L3(I)=E01X(I)*X0G3(I)+E01Y(I)*Y0G3(I)+E01Z(I)*Z0G3(I)
        Y0L3(I)=E02X(I)*X0G3(I)+E02Y(I)*Y0G3(I)+E02Z(I)*Z0G3(I)
      ENDDO
C----------------------------
C     Rotate x0l of Rz1 
C----------------------------
      DO I=JFT,JLT
        XL= X0L2(I)*DIRZ(I,1)-Y0L2(I)*DIRZ(I,2)
        YL= X0L2(I)*DIRZ(I,2)+Y0L2(I)*DIRZ(I,1)
        X0L2(I)=XL
        Y0L2(I)=YL
        XL= X0L3(I)*DIRZ(I,1)-Y0L3(I)*DIRZ(I,2)
        YL= X0L3(I)*DIRZ(I,2)+Y0L3(I)*DIRZ(I,1)
        X0L3(I)=XL
        Y0L3(I)=YL
      ENDDO
C------U21,U31 in actual local system	  
      DO I=JFT,JLT
        V21X(I)=XL2(I)-X0L2(I)
        V31X(I)=XL3(I)-X0L3(I)
        V21Y(I)=YL2(I)-Y0L2(I)
        V31Y(I)=YL3(I)-Y0L3(I)
      ENDDO 
      DO I=JFT,JLT
        X2_T(I) = X0L2(I)
        X3_T(I) = X0L3(I)
        Y2_T(I) = Y0L2(I)
        Y3_T(I) = Y0L3(I)
      ENDDO 
      IF (ISROT>0) THEN
C------RZ13,RZ23 in actual local system	  
       DO I=JFT,JLT
        AREAI=HALF/MAX(EM20,AREA(I))
        RLZ1 =E3X(I)*AXYZ(I,1,1)+E3Y(I)*AXYZ(I,2,1)+E3Z(I)*AXYZ(I,3,1)
        RLZ2 =E3X(I)*AXYZ(I,1,2)+E3Y(I)*AXYZ(I,2,2)+E3Z(I)*AXYZ(I,3,2)
        RLZ3 =E3X(I)*AXYZ(I,1,3)+E3Y(I)*AXYZ(I,2,3)+E3Z(I)*AXYZ(I,3,3)
        RZ13(I)=(RLZ1-RLZ3)*AREAI
        RZ23(I)=(RLZ2-RLZ3)*AREAI
       ENDDO 
      END IF !(ISROT>0) THEN
C
      OFF_L = ZERO
      DO I=JFT,JLT
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
      IF (OFF_L < ZERO) THEN
        DO I=JFT,JLT
         IF (OFFG(I) < ZERO) THEN
          V21X(I) = ZERO
          V31X(I) = ZERO
          V21Y(I) = ZERO
          V31Y(I) = ZERO
          RZ13(I) = ZERO
          RZ23(I) = ZERO
         ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
