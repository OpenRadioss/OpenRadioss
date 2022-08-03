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
Chd|  S4COOR3                       source/elements/solid/solide4/s4coor3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4COOR3(X,IXS,V,W,
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4, 
     .   Z1, Z2, Z3, Z4, VX1, VX2, VX3, VX4,
     .   VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4, 
     .   VDX1, VDX2, VDX3, VDX4, 
     .   VDY1, VDY2, VDY3, VDY4, 
     .   VDZ1, VDZ2, VDZ3, VDZ4, 
     .   VDX,VDY,VDZ,VD2,VIS,OFFG,OFF,SAV,RHO,RHOO,
     .   NC1,NC2,NC3,NC4,NGL,MXT,NGEO,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .   ZD1, ZD2, ZD3, ZD4, XDP,NEL)
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
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD), VIS(NEL),
     .   X1(NEL), X2(NEL), X3(NEL), X4(NEL), 
     .   Y1(NEL), Y2(NEL), Y3(NEL), Y4(NEL), 
     .   Z1(NEL), Z2(NEL), Z3(NEL), Z4(NEL), 
     .  VX1(NEL), VX2(NEL), VX3(NEL), VX4(NEL), 
     .  VY1(NEL), VY2(NEL), VY3(NEL), VY4(NEL), 
     .  VZ1(NEL), VZ2(NEL), VZ3(NEL), VZ4(NEL), 
     .  VDX1(NEL),VDX2(NEL),VDX3(NEL),VDX4(NEL),
     .  VDY1(NEL),VDY2(NEL),VDY3(NEL),VDY4(NEL),
     .  VDZ1(NEL),VDZ2(NEL),VDZ3(NEL),VDZ4(NEL),
     .  VDX(NEL), VDY(NEL), VDZ(NEL),VD2(NEL),OFFG(NEL),OFF(NEL),RHO(NEL),
     .   F11(NEL),F21(NEL),F31(NEL),F12(NEL),F22(NEL),F32(NEL),
     .   F13(NEL),F23(NEL),F33(NEL),F14(NEL),F24(NEL),F34(NEL),
     .  RHOO(NEL)
      INTEGER NC1(NEL), NC2(NEL), NC3(NEL), NC4(NEL),
     .        MXT(NEL), NGL(NEL),NGEO(NEL)
      INTEGER IXS(NIXS,*)
      
      DOUBLE PRECISION 
     .   XDP(3,NUMNOD), SAV(NEL,9),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ)
           
      my_real
     .   OFF_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,NEL
        VIS(I)=ZERO
        NGEO(I)=IXS(10,I)
        NGL(I)=IXS(11,I)
        MXT(I)=IXS(1,I)
        NC1(I)=IXS(2,I)
        NC2(I)=IXS(4,I)
        NC3(I)=IXS(7,I)
        NC4(I)=IXS(6,I)
        RHOO(I)=RHO(I)
      ENDDO
      DO I=1,NEL
        X1(I) =X(1,NC1(I))
        Y1(I) =X(2,NC1(I))
        Z1(I) =X(3,NC1(I))
        X2(I) =X(1,NC2(I))
        Y2(I) =X(2,NC2(I))
        Z2(I) =X(3,NC2(I))
        X3(I) =X(1,NC3(I))
        Y3(I) =X(2,NC3(I))
        Z3(I) =X(3,NC3(I))
        X4(I) =X(1,NC4(I))
        Y4(I) =X(2,NC4(I))
        Z4(I) =X(3,NC4(I))
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
      ENDDO

      OFF_L  = ZERO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      IF((ISMSTR<=4.AND.JLAG>0).OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN
C
        DO I=1,NEL
          IF(ABS(OFFG(I)).GT.ONE)THEN
            XD1(I)=SAV(I,1)
            YD1(I)=SAV(I,2)
            ZD1(I)=SAV(I,3)
            XD2(I)=SAV(I,4)
            YD2(I)=SAV(I,5)
            ZD2(I)=SAV(I,6)
            XD3(I)=SAV(I,7)
            YD3(I)=SAV(I,8)
            ZD3(I)=SAV(I,9)
            XD4(I)=ZERO
            YD4(I)=ZERO
            ZD4(I)=ZERO
            OFF(I) = ABS(OFFG(I))-ONE
            OFF_L  = MIN(OFF_L,OFFG(I))
          ELSE
           IF(IRESP==1)THEN
             XD1(I)=XDP(1,NC1(I))
             YD1(I)=XDP(2,NC1(I))
             ZD1(I)=XDP(3,NC1(I))
             XD2(I)=XDP(1,NC2(I))
             YD2(I)=XDP(2,NC2(I))
             ZD2(I)=XDP(3,NC2(I))
             XD3(I)=XDP(1,NC3(I))
             YD3(I)=XDP(2,NC3(I))
             ZD3(I)=XDP(3,NC3(I))
             XD4(I)=XDP(1,NC4(I))
             YD4(I)=XDP(2,NC4(I))
             ZD4(I)=XDP(3,NC4(I))
            ELSE
             XD1(I)=X1(I)
             YD1(I)=Y1(I)
             ZD1(I)=Z1(I)
             XD2(I)=X2(I)
             YD2(I)=Y2(I)
             ZD2(I)=Z2(I)
             XD3(I)=X3(I)
             YD3(I)=Y3(I)
             ZD3(I)=Z3(I)
             XD4(I)=X4(I)
             YD4(I)=Y4(I)
             ZD4(I)=Z4(I)
           ENDIF   
           OFF(I) = ABS(OFFG(I))
           OFF_L  = MIN(OFF_L,OFFG(I))
         ENDIF
       ENDDO
C
      ELSE
C
      IF(IRESP==1)THEN 
        DO I=1,NEL
          XD1(I)=XDP(1,NC1(I))  
          YD1(I)=XDP(2,NC1(I))  
          ZD1(I)=XDP(3,NC1(I))  
          XD2(I)=XDP(1,NC2(I))  
          YD2(I)=XDP(2,NC2(I))  
          ZD2(I)=XDP(3,NC2(I))  
          XD3(I)=XDP(1,NC3(I))  
          YD3(I)=XDP(2,NC3(I))  
          ZD3(I)=XDP(3,NC3(I))  
          XD4(I)=XDP(1,NC4(I))  
          YD4(I)=XDP(2,NC4(I))  
          ZD4(I)=XDP(3,NC4(I))  
          OFF(I) = ABS(OFFG(I))
          OFF_L  = MIN(OFF_L,OFFG(I))                 
        ENDDO
      ELSE  
        DO I=1,NEL
          XD1(I)=X1(I) 
          YD1(I)=Y1(I) 
          ZD1(I)=Z1(I) 
          XD2(I)=X2(I) 
          YD2(I)=Y2(I) 
          ZD2(I)=Z2(I) 
          XD3(I)=X3(I) 
          YD3(I)=Y3(I) 
          ZD3(I)=Z3(I) 
          XD4(I)=X4(I) 
          YD4(I)=Y4(I) 
          ZD4(I)=Z4(I)   
          OFF(I) = ABS(OFFG(I))
          OFF_L  = MIN(OFF_L,OFFG(I))     
        ENDDO      
      ENDIF     
C
      ENDIF
C
      IF(OFF_L.LT.ZERO)THEN
        DO I=1,NEL
          IF(OFFG(I).LT.ZERO)THEN
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
          ENDIF
        ENDDO
      ENDIF
C
      DO I=1,NEL
        VD2(I)=ZERO
        F11(I)=ZERO
        F21(I)=ZERO
        F31(I)=ZERO
        F12(I)=ZERO
        F22(I)=ZERO
        F32(I)=ZERO
        F13(I)=ZERO
        F23(I)=ZERO
        F33(I)=ZERO
        F14(I)=ZERO
        F24(I)=ZERO
        F34(I)=ZERO
      ENDDO
C
      IF (JLAG.NE.0)THEN
       RETURN
C
      ELSEIF(JALE.NE.0)THEN
        DO I=1,NEL
          VDX1(I)=VX1(I)-W(1,NC1(I))
          VDY1(I)=VY1(I)-W(2,NC1(I))
          VDZ1(I)=VZ1(I)-W(3,NC1(I))
          VDX2(I)=VX2(I)-W(1,NC2(I))
          VDY2(I)=VY2(I)-W(2,NC2(I))
          VDZ2(I)=VZ2(I)-W(3,NC2(I))
          VDX3(I)=VX3(I)-W(1,NC3(I))
          VDY3(I)=VY3(I)-W(2,NC3(I))
          VDZ3(I)=VZ3(I)-W(3,NC3(I))
          VDX4(I)=VX4(I)-W(1,NC4(I))
          VDY4(I)=VY4(I)-W(2,NC4(I))
          VDZ4(I)=VZ4(I)-W(3,NC4(I))
        ENDDO
      ELSEIF(JEUL.NE.0)THEN
        DO I=1,NEL
          VDX1(I)=VX1(I)
          VDY1(I)=VY1(I)
          VDZ1(I)=VZ1(I)
          VDX2(I)=VX2(I)
          VDY2(I)=VY2(I)
          VDZ2(I)=VZ2(I)
          VDX3(I)=VX3(I)
          VDY3(I)=VY3(I)
          VDZ3(I)=VZ3(I)
          VDX4(I)=VX4(I)
          VDY4(I)=VY4(I)
          VDZ4(I)=VZ4(I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        VDX(I)=FOURTH*(VDX1(I)+VDX2(I)+VDX3(I)+VDX4(I))
        VDY(I)=FOURTH*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I))
        VDZ(I)=FOURTH*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I))
        VD2(I)=NINE*(VDX(I)**2+VDY(I)**2+VDZ(I)**2)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S4SAV3                        source/elements/solid/solide4/s4coor3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4SAV3(OFFG,SAV,
     .   XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .   ZD1, ZD2, ZD3, ZD4,NEL)
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
      my_real
     .   OFFG(NEL)
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),SAV(NEL,9)
           
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C--------------------------------------------
      DO I=1,NEL
        IF(ABS(OFFG(I)) <= ONE) THEN
          SAV(I,1)=XD1(I)-XD4(I)
          SAV(I,2)=YD1(I)-YD4(I)
          SAV(I,3)=ZD1(I)-ZD4(I)
          SAV(I,4)=XD2(I)-XD4(I)
          SAV(I,5)=YD2(I)-YD4(I)
          SAV(I,6)=ZD2(I)-ZD4(I)
          SAV(I,7)=XD3(I)-XD4(I)
          SAV(I,8)=YD3(I)-YD4(I)
          SAV(I,9)=ZD3(I)-ZD4(I)
        ENDIF
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  S4RCOOR12                     source/elements/solid/solide4/s4coor3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        SMORTHO3                      source/elements/solid/solide4/s4coor3.F
Chd|====================================================================
      SUBROUTINE S4RCOOR12(OFF,NC1,NC2,NC3   ,NC4   ,X     ,XDP   ,D     ,
     .   R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,R31   ,R32   ,R33   ) 
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
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C     REAL
      my_real
     .   OFF(*),X(3,*),D(3,*),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      DOUBLE PRECISION 
     .   XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      
      DOUBLE PRECISION
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
    
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
C=======================================================================
C---for case of Ismstr=10-> Ismstr11 :
C----1) config is fixed at t= t_dtmin but will not be updated like Ismstr11
C----2) [H] = [B](t=0)*{u}, [B](t=0)<-JAC_I, {u}={d}; 
C----3) in case of JCVT=0, [H] and [D] are transformed to local system and
C-      return to global system for stress
C ---------local system compute----------     
      IF(IRESP==1)THEN 
        DO I=LFT,LLT
         IF (OFF(I) <= ONE ) CYCLE 
          X1(I)=XDP(1,NC1(I))  
          Y1(I)=XDP(2,NC1(I))  
          Z1(I)=XDP(3,NC1(I))  
          X2(I)=XDP(1,NC2(I))  
          Y2(I)=XDP(2,NC2(I))  
          Z2(I)=XDP(3,NC2(I))  
          X3(I)=XDP(1,NC3(I))  
          Y3(I)=XDP(2,NC3(I))  
          Z3(I)=XDP(3,NC3(I))  
          X4(I)=XDP(1,NC4(I))  
          Y4(I)=XDP(2,NC4(I))  
          Z4(I)=XDP(3,NC4(I))  
        ENDDO
      ELSE  
        DO I=LFT,LLT
         IF (OFF(I) <= ONE ) CYCLE 
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
        ENDDO      
      ENDIF     
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=LFT,LLT
       IF (OFF(I) <= ONE ) CYCLE 
       X43 = X4(I) - X3(I)
       Y43 = Y4(I) - Y3(I)
       Z43 = Z4(I) - Z3(I)
       X41 = X4(I) - X1(I)
       Y41 = Y4(I) - Y1(I)
       Z41 = Z4(I) - Z1(I)
       X42 = X4(I) - X2(I)
       Y42 = Y4(I) - Y2(I)
       Z42 = Z4(I) - Z2(I)
C
       RX(I) =  -X41
       RY(I) =  -Y41
       RZ(I) =  -Z41
       SX(I) =  -X42
       SY(I) =  -Y42
       SZ(I) =  -Z42
C
       TX(I) =  -X43
       TY(I) =  -Y43
       TZ(I) =  -Z43
      END DO 
      CALL SMORTHO3(OFF,
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33)
     
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
C       CALL S4RROTA3(OFF,
C     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
C     .   X0(I,1), X0(I,2), X0(I,3), X0(I,4), 
C     .   Y0(I,1), Y0(I,2), Y0(I,3), Y0(I,4), 
C     .   Z0(I,1), Z0(I,2), Z0(I,3), Z0(I,4))
C       CALL S4RROTA3(OFF,
C     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
C     .   VX0(I,1), VX0(I,2), VX0(I,3), VX0(I,4), 
C     .   VY0(I,1), VY0(I,2), VY0(I,3), VY0(I,4), 
C     .   VZ0(I,1), VZ0(I,2), VZ0(I,3), VZ0(I,4))
C    
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SMORTHO3                      source/elements/solid/solide4/s4coor3.F
Chd|-- called by -----------
Chd|        S10RCOOR12                    source/elements/solid/solide10/s10coor3.F
Chd|        S4RCOOR12                     source/elements/solid/solide4/s4coor3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMORTHO3(OFF,
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z )
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
C     REAL
      my_real, DIMENSION(MVSIZ), INTENT(IN)  ::
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,OFF
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NITER
C     REAL
      my_real
     .   aa,bb
      my_real, DIMENSION(MVSIZ) ::
     .   UX, UY, UZ, VX, VY, VZ, WX, WY, WZ
      DATA NITER/3/
C=======================================================================
c     norme r s t
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=LFT,LLT
       IF (OFF(I) <= ONE ) CYCLE 
       aa = sqrt(rx(I)*rx(I) + ry(I)*ry(I) + rz(I)*rz(I))
       if ( aa.ne.ZERO) aa = ONE / aa
       Ux(I) = rx(I) * aa
       Uy(I) = ry(I) * aa
       Uz(I) = rz(I) * aa
       aa = sqrt(sx(I)*sx(I) + sy(I)*sy(I) + sz(I)*sz(I))
       if ( aa.ne.ZERO) aa = ONE / aa
       Vx(I) = sx(I) * aa
       Vy(I) = sy(I) * aa
       Vz(I) = sz(I) * aa
       aa = sqrt(tx(I)*tx(I) + ty(I)*ty(I) + tz(I)*tz(I))
       if ( aa.ne.ZERO) aa = ONE / aa
       Wx(I) = tx(I) * aa
       Wy(I) = ty(I) * aa
       Wz(I) = tz(I) * aa
      END DO
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     iterations
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      N=0
111   CONTINUE
      N=N+1
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=LFT,LLT
       IF (OFF(I) <= ONE ) CYCLE 
       e1x(I) = Vy(I) * Wz(I) - Vz(I) * Wy(I) + Ux(I)
       e1y(I) = Vz(I) * Wx(I) - Vx(I) * Wz(I) + Uy(I)
       e1z(I) = Vx(I) * Wy(I) - Vy(I) * Wx(I) + Uz(I)
c
       e2x(I) = Wy(I) * Uz(I) - Wz(I) * Uy(I) + Vx(I)
       e2y(I) = Wz(I) * Ux(I) - Wx(I) * Uz(I) + Vy(I)
       e2z(I) = Wx(I) * Uy(I) - Wy(I) * Ux(I) + Vz(I)
c
       e3x(I) = Uy(I) * Vz(I) - Uz(I) * Vy(I) + Wx(I)
       e3y(I) = Uz(I) * Vx(I) - Ux(I) * Vz(I) + Wy(I)
       e3z(I) = Ux(I) * Vy(I) - Uy(I) * Vx(I) + Wz(I)
c
       bb = sqrt(e1x(I)*e1x(I) + e1y(I)*e1y(I) + e1z(I)*e1z(I))
       if ( bb.ne.ZERO) bb = ONE / bb
       Ux(I) = e1x(I) * bb
       Uy(I) = e1y(I) * bb
       Uz(I) = e1z(I) * bb
c
       bb = sqrt(e2x(I)*e2x(I) + e2y(I)*e2y(I) + e2z(I)*e2z(I))
       if ( bb.ne.ZERO) bb = ONE / bb
       Vx(I) = e2x(I) * bb
       Vy(I) = e2y(I) * bb
       Vz(I) = e2z(I) * bb
c
       bb = sqrt(e3x(I)*e3x(I) + e3y(I)*e3y(I) + e3z(I)*e3z(I))
       if ( bb.ne.ZERO) bb = ONE / bb
       Wx(I) = e3x(I) * bb
       Wy(I) = e3y(I) * bb
       Wz(I) = e3z(I) * bb
c
      END DO
      IF (N < NITER) GOTO 111
c     norme et orthogonalisation
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=LFT,LLT
      IF (OFF(I) <= ONE ) CYCLE 
      e1x(I) = Ux(I)
      e1y(I) = Uy(I)
      e1z(I) = Uz(I)
c
      e3x(I) = e1y(I) * Vz(I) - e1z(I) * Vy(I)
      e3y(I) = e1z(I) * Vx(I) - e1x(I) * Vz(I)
      e3z(I) = e1x(I) * Vy(I) - e1y(I) * Vx(I)
c
      aa = sqrt(e3x(I)*e3x(I) + e3y(I)*e3y(I) + e3z(I)*e3z(I))
      if ( aa.ne.ZERO) aa = ONE / aa
      e3x(I) = e3x(I) * aa
      e3y(I) = e3y(I) * aa
      e3z(I) = e3z(I) * aa
c
      e2x(I) = e3y(I) * e1z(I) - e3z(I) * e1y(I)
      e2y(I) = e3z(I) * e1x(I) - e3x(I) * e1z(I)
      e2z(I) = e3x(I) * e1y(I) - e3y(I) * e1x(I)
      END DO
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END SUBROUTINE SMORTHO3
Chd|====================================================================
Chd|  S4RROTA3                      source/elements/solid/solide4/s4coor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4RROTA3(OFF,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   X1, X2, X3, X4, 
     .   Y1, Y2, Y3, Y4, 
     .   Z1, Z2, Z3, Z4)
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
C     REAL
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),OFF(*),
     .   X1(*), X2(*), X3(*), X4(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*),   
     .   Z1(*), Z2(*), Z3(*), Z4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   X,Y,Z
C-----------------------------------------------
C   X=RX' <=> X'=t(R)X chgt de base.
C-  to do X'=RtX, just input Rt instead of R
      DO I=LFT,LLT
       IF (OFF(I) <= ONE) CYCLE
       X=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
       Y=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
       Z=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
       X1(I)=X
       Y1(I)=Y
       Z1(I)=Z
       X=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
       Y=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
       Z=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
       X2(I)=X
       Y2(I)=Y
       Z2(I)=Z
       X=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
       Y=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
       Z=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
       X3(I)=X
       Y3(I)=Y
       Z3(I)=Z
       X=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
       Y=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
       Z=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
       X4(I)=X
       Y4(I)=Y
       Z4(I)=Z
      END DO !I=LFT,LLT
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
Chd|====================================================================
Chd|  S4SAV12                       source/elements/solid/solide4/s4coor3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4SAV12(OFFG,OFFG0,SAV,
     .   XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .   ZD1, ZD2, ZD3, ZD4,NEL)
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
     .   OFFG(*),OFFG0(*)
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),SAV(NEL,9)
           
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
C
       DO I=LFT,LLT
       IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE)THEN
        SAV(I,1)=XD1(I)-XD4(I)
        SAV(I,2)=YD1(I)-YD4(I)
        SAV(I,3)=ZD1(I)-ZD4(I)
        SAV(I,4)=XD2(I)-XD4(I)
        SAV(I,5)=YD2(I)-YD4(I)
        SAV(I,6)=ZD2(I)-ZD4(I)
        SAV(I,7)=XD3(I)-XD4(I)
        SAV(I,8)=YD3(I)-YD4(I)
        SAV(I,9)=ZD3(I)-ZD4(I)
       ENDIF
       ENDDO
C
      RETURN
      END
