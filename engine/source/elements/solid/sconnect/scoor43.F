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
Chd|  SCOOR43                       source/elements/solid/sconnect/scoor43.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        SDET43                        source/elements/solid/sconnect/sdet43.F
Chd|====================================================================
      SUBROUTINE SCOOR43(OFFG   ,NEL  ,IOUTPRT    ,Q    ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,  
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,  
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,  
     .         VX1  ,VX2  ,VX3  ,VX4  ,VX5  ,VX6  ,VX7  ,VX8  ,   
     .         VY1  ,VY2  ,VY3  ,VY4  ,VY5  ,VY6  ,VY7  ,VY8  ,   
     .         VZ1  ,VZ2  ,VZ3  ,VZ4  ,VZ5  ,VZ6  ,VZ7  ,VZ8  ,   
     .         R1X  ,R2X  ,R3X  ,R4X  ,R5X  ,R6X  ,R7X  ,R8X  ,
     .         R1Y  ,R2Y  ,R3Y  ,R4Y  ,R5Y  ,R6Y  ,R7Y  ,R8Y  ,
     .         R1Z  ,R2Z  ,R3Z  ,R4Z  ,R5Z  ,R6Z  ,R7Z  ,R8Z  ,
     .         RXX  ,RYY  ,RZZ  ,VXLOC,VYLOC,VZLOC,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  , 
     .         AREAP,TIME ,DT   ,SOLID_ID,
     .         VGAX ,VGAY ,VGAZ ,VGA2 ,SYM, IPM, IMAT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,IMAT,IOUTPRT
      INTEGER IPM(NPROPMI,*),SOLID_ID(*)
C     REAL ou REAL*8
      my_real
     .   TIME,DT,OFFG(NEL),AREAP(MVSIZ,4),Q(NEL,9),
     .   RXX(NEL),RYY(NEL),RZZ(NEL),
     .   X1(NEL) ,X2(NEL),X3(NEL),X4(NEL),
     .   X5(NEL) ,X6(NEL),X7(NEL),X8(NEL),
     .   Y1(NEL) ,Y2(NEL),Y3(NEL),Y4(NEL),
     .   Y5(NEL) ,Y6(NEL),Y7(NEL),Y8(NEL),
     .   Z1(NEL) ,Z2(NEL),Z3(NEL),Z4(NEL),
     .   Z5(NEL) ,Z6(NEL),Z7(NEL),Z8(NEL),
     .   VX1(NEL), VX2(NEL), VX3(NEL), VX4(NEL),
     .   VX5(NEL), VX6(NEL), VX7(NEL), VX8(NEL),
     .   VY1(NEL), VY2(NEL), VY3(NEL), VY4(NEL),
     .   VY5(NEL), VY6(NEL), VY7(NEL), VY8(NEL),
     .   VZ1(NEL), VZ2(NEL), VZ3(NEL), VZ4(NEL),
     .   VZ5(NEL), VZ6(NEL), VZ7(NEL), VZ8(NEL),
     .   VXLOC(MVSIZ,8),VYLOC(MVSIZ,8),VZLOC(MVSIZ,8),
     .   R1X(NEL),R2X(NEL),R3X(NEL),R4X(NEL),
     .   R5X(NEL),R6X(NEL),R7X(NEL),R8X(NEL),
     .   R1Y(NEL),R2Y(NEL),R3Y(NEL),R4Y(NEL),
     .   R5Y(NEL),R6Y(NEL),R7Y(NEL),R8Y(NEL),
     .   R1Z(NEL),R2Z(NEL),R3Z(NEL),R4Z(NEL),
     .   R5Z(NEL),R6Z(NEL),R7Z(NEL),R8Z(NEL),
     .   VGAX(*), VGAY(*), VGAZ(*), VGA2(*),
     .   E1X(*),E2X(*),E3X(*),E1Y(*),E2Y(*),E3Y(*),E1Z(*),E2Z(*),E3Z(*),
     .   XLOC(NEL,8), YLOC(NEL,8), ZLOC(NEL,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IREP1,MTN
C     REAL ou REAL*8
      my_real
     .   OFF_L,CS,SN,KSI,NR,X0,Y0,Z0,VX0,VY0,VZ0,DTI,CC,COSIN
      my_real 
     .   P1X(NEL), P2X(NEL),  P3X(NEL),  P4X(NEL), 
     .   P1Y(NEL), P2Y(NEL),  P3Y(NEL),  P4Y(NEL), 
     .   P1Z(NEL), P2Z(NEL),  P3Z(NEL),  P4Z(NEL),
     .   RX(NEL),RY(NEL),RZ(NEL),SX(NEL),SY(NEL),SZ(NEL),
     .   AREA(NEL),DET(NEL),RV(3),MROT(9),NORME(NEL),SYM(NEL),
     .   a1x(NEL), a1y(NEL), a1z(NEL), b1x(NEL), b1y(NEL), b1z(NEL),
     .   a2x(NEL), a2y(NEL), a2z(NEL), b2x(NEL), b2y(NEL), b2z(NEL), 
     .   N1x(NEL), N1y(NEL), N1z(NEL), N2x(NEL), N2y(NEL), N2z(NEL)
C=======================================================================
      OFF_L = ZERO
      DTI = UN/MAX(DT,EM20)
      MTN = IPM(2,IMAT)   
C----------------------------
C     REPERE ISO
C----------------------------
      DO I=1,NEL 
        ! P milieu des aretes
        P1X(I)=(X1(I)+X5(I))*UNDEMI
        P1Y(I)=(Y1(I)+Y5(I))*UNDEMI
        P1Z(I)=(Z1(I)+Z5(I))*UNDEMI
        P2X(I)=(X2(I)+X6(I))*UNDEMI
        P2Y(I)=(Y2(I)+Y6(I))*UNDEMI
        P2Z(I)=(Z2(I)+Z6(I))*UNDEMI
        P3X(I)=(X3(I)+X7(I))*UNDEMI
        P3Y(I)=(Y3(I)+Y7(I))*UNDEMI
        P3Z(I)=(Z3(I)+Z7(I))*UNDEMI
        P4X(I)=(X4(I)+X8(I))*UNDEMI
        P4Y(I)=(Y4(I)+Y8(I))*UNDEMI
        P4Z(I)=(Z4(I)+Z8(I))*UNDEMI
        RX(I) = P2X(I) + P3X(I) - P1X(I) - P4X(I)
        RY(I) = P2Y(I) + P3Y(I) - P1Y(I) - P4Y(I)
        RZ(I) = P2Z(I) + P3Z(I) - P1Z(I) - P4Z(I)
        SX(I) = P3X(I) + P4X(I) - P1X(I) - P2X(I)
        SY(I) = P3Y(I) + P4Y(I) - P1Y(I) - P2Y(I)
        SZ(I) = P3Z(I) + P4Z(I) - P1Z(I) - P2Z(I)
      ENDDO
      IF (MTN==83) THEN
       DO I=1,NEL 
        
        ! N1 and N2 the normal to the surface 1234 and 5678
        a1X(I) = X6(I) + X7(I) - X5(I) - X8(I)
        a1Y(I) = Y6(I) + Y7(I) - Y5(I) - Y8(I)
        a1Z(I) = Z6(I) + Z7(I) - Z5(I) - Z8(I)
        b1X(I) = X7(I) + X8(I) - X6(I) - X5(I)
        b1Y(I) = Y7(I) + Y8(I) - Y6(I) - Y5(I)
        b1Z(I) = Z7(I) + Z8(I) - Z6(I) - Z5(I)
        ! A VECTORIEL b PUIS NORMALISER
        N1X(I) = a1Y(I) * b1Z(I) - a1Z(I) * b1Y(I) 
        N1Y(I) = a1Z(I) * b1X(I) - a1X(I) * b1Z(I) 
        N1Z(I) = a1X(I) * b1Y(I) - a1Y(I) * b1X(I) 
        NORME(I) = SQRT(N1X(I)*N1X(I) + N1Y(I)*N1Y(I) + N1Z(I)*N1Z(I))
        NORME(I)=MAX(EM20,NORME(I))
        CC = UN / NORME(I)
        N1X(I) = N1X(I) * CC 
        N1Y(I) = N1Y(I) * CC 
        N1Z(I) = N1Z(I) * CC 

        a2X(I) = X2(I) + X3(I) - X1(I) - X4(I)
        a2Y(I) = Y2(I) + Y3(I) - Y1(I) - Y4(I)
        a2Z(I) = Z2(I) + Z3(I) - Z1(I) - Z4(I)
        b2X(I) = X3(I) + X4(I) - X2(I) - X1(I)
        b2Y(I) = Y3(I) + Y4(I) - Y2(I) - Y1(I)
        b2Z(I) = Z3(I) + Z4(I) - Z2(I) - Z1(I)
        ! A VECTORIEL b PUIS NORMALISER
        N2X(I) = a2Y(I) * b2Z(I) - a2Z(I) * b2Y(I) 
        N2Y(I) = a2Z(I) * b2X(I) - a2X(I) * b2Z(I) 
        N2Z(I) = a2X(I) * b2Y(I) - a2Y(I) * b2X(I) 
        NORME(I) = SQRT(N2X(I)*N2X(I) + N2Y(I)*N2Y(I) + N2Z(I)*N2Z(I))
        NORME(I)=MAX(EM20,NORME(I))
        CC = UN / NORME(I)
        N2X(I) = N2X(I) * CC 
        N2Y(I) = N2Y(I) * CC 
        N2Z(I) = N2Z(I) * CC 
        ! TETA = ARCOS( N1.N2 ) CAR NORME =1 PAS BESOIN DE DIVISER
        COSIN =   N1X(I)*N2X(I) + N1Y(I)*N2Y(I) + N1Z(I)*N2Z(I)
        COSIN = MAX (-UN,MIN (UN, COSIN))
        SYM(I)= ACOS (COSIN)
       ENDDO
      ENDIF
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      IREP1 = 0
      CALL CLSKEW3(1,NEL ,IREP1,
     .             RX, RY, RZ,SX, SY, SZ, 
     .             E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,DET,OFFG)
      DO I=1,NEL
        AREA(I) = QUART*DET(I)
      ENDDO
C-----------
C     Prepare les sorties par part.
C-----------
      IF (IOUTPRT == 1) THEN
       DO I=1,NEL
         VGAX(I)=VX1(I)+VX2(I)+VX3(I)+VX4(I)+VX5(I)+VX6(I)+VX7(I)+VX8(I)
         VGAY(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)+VY5(I)+VY6(I)+VY7(I)+VY8(I)
         VGAZ(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+VZ5(I)+VZ6(I)+VZ7(I)+VZ8(I)
         VGA2(I)=VX1(I)*VX1(I)+VX2(I)*VX2(I)+VX3(I)*VX3(I)+VX4(I)*VX4(I)
     .          +VX5(I)*VX5(I)+VX6(I)*VX6(I)+VX7(I)*VX7(I)+VX8(I)*VX8(I)
     .          +VY1(I)*VY1(I)+VY2(I)*VY2(I)+VY3(I)*VY3(I)+VY4(I)*VY4(I)
     .          +VY5(I)*VY5(I)+VY6(I)*VY6(I)+VY7(I)*VY7(I)+VY8(I)*VY8(I)
     .          +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)+VZ3(I)*VZ3(I)+VZ4(I)*VZ4(I)
     .          +VZ5(I)*VZ5(I)+VZ6(I)*VZ6(I)+VZ7(I)*VZ7(I)+VZ8(I)*VZ8(I)
       ENDDO
      ENDIF
C-----------
C     PASSAGE AU REPERE CONVECTE
C-----------
      DO I=1,NEL                                                
        XLOC(I,1) = E1X(I)*X1(I)+E1Y(I)*Y1(I)+E1Z(I)*Z1(I)               
        YLOC(I,1) = E2X(I)*X1(I)+E2Y(I)*Y1(I)+E2Z(I)*Z1(I)               
        ZLOC(I,1) = E3X(I)*X1(I)+E3Y(I)*Y1(I)+E3Z(I)*Z1(I)               
        XLOC(I,2) = E1X(I)*X2(I)+E1Y(I)*Y2(I)+E1Z(I)*Z2(I)               
        YLOC(I,2) = E2X(I)*X2(I)+E2Y(I)*Y2(I)+E2Z(I)*Z2(I)               
        ZLOC(I,2) = E3X(I)*X2(I)+E3Y(I)*Y2(I)+E3Z(I)*Z2(I)               
        XLOC(I,3) = E1X(I)*X3(I)+E1Y(I)*Y3(I)+E1Z(I)*Z3(I)               
        YLOC(I,3) = E2X(I)*X3(I)+E2Y(I)*Y3(I)+E2Z(I)*Z3(I)               
        ZLOC(I,3) = E3X(I)*X3(I)+E3Y(I)*Y3(I)+E3Z(I)*Z3(I)               
        XLOC(I,4) = E1X(I)*X4(I)+E1Y(I)*Y4(I)+E1Z(I)*Z4(I)               
        YLOC(I,4) = E2X(I)*X4(I)+E2Y(I)*Y4(I)+E2Z(I)*Z4(I)               
        ZLOC(I,4) = E3X(I)*X4(I)+E3Y(I)*Y4(I)+E3Z(I)*Z4(I)               
        XLOC(I,5) = E1X(I)*X5(I)+E1Y(I)*Y5(I)+E1Z(I)*Z5(I)               
        YLOC(I,5) = E2X(I)*X5(I)+E2Y(I)*Y5(I)+E2Z(I)*Z5(I)               
        ZLOC(I,5) = E3X(I)*X5(I)+E3Y(I)*Y5(I)+E3Z(I)*Z5(I)               
        XLOC(I,6) = E1X(I)*X6(I)+E1Y(I)*Y6(I)+E1Z(I)*Z6(I)               
        YLOC(I,6) = E2X(I)*X6(I)+E2Y(I)*Y6(I)+E2Z(I)*Z6(I)               
        ZLOC(I,6) = E3X(I)*X6(I)+E3Y(I)*Y6(I)+E3Z(I)*Z6(I)               
        XLOC(I,7) = E1X(I)*X7(I)+E1Y(I)*Y7(I)+E1Z(I)*Z7(I)               
        YLOC(I,7) = E2X(I)*X7(I)+E2Y(I)*Y7(I)+E2Z(I)*Z7(I)               
        ZLOC(I,7) = E3X(I)*X7(I)+E3Y(I)*Y7(I)+E3Z(I)*Z7(I)               
        XLOC(I,8) = E1X(I)*X8(I)+E1Y(I)*Y8(I)+E1Z(I)*Z8(I)               
        YLOC(I,8) = E2X(I)*X8(I)+E2Y(I)*Y8(I)+E2Z(I)*Z8(I)               
        ZLOC(I,8) = E3X(I)*X8(I)+E3Y(I)*Y8(I)+E3Z(I)*Z8(I)               
C
        X0 = (XLOC(I,1)+XLOC(I,2)+XLOC(I,3)+XLOC(I,4)+
     .        XLOC(I,5)+XLOC(I,6)+XLOC(I,7)+XLOC(I,8))*UNHUIT
        Y0 = (YLOC(I,1)+YLOC(I,2)+YLOC(I,3)+YLOC(I,4)+
     .        YLOC(I,5)+YLOC(I,6)+YLOC(I,7)+YLOC(I,8))*UNHUIT
        Z0 = (ZLOC(I,1)+ZLOC(I,2)+ZLOC(I,3)+ZLOC(I,4)+ 
     .        ZLOC(I,5)+ZLOC(I,6)+ZLOC(I,7)+ZLOC(I,8))*UNHUIT
C
        XLOC(I,1) = XLOC(I,1) - X0
        XLOC(I,2) = XLOC(I,2) - X0
        XLOC(I,3) = XLOC(I,3) - X0
        XLOC(I,4) = XLOC(I,4) - X0
        XLOC(I,5) = XLOC(I,5) - X0
        XLOC(I,6) = XLOC(I,6) - X0
        XLOC(I,7) = XLOC(I,7) - X0
        XLOC(I,8) = XLOC(I,8) - X0
        YLOC(I,1) = YLOC(I,1) - Y0
        YLOC(I,2) = YLOC(I,2) - Y0
        YLOC(I,3) = YLOC(I,3) - Y0
        YLOC(I,4) = YLOC(I,4) - Y0
        YLOC(I,5) = YLOC(I,5) - Y0
        YLOC(I,6) = YLOC(I,6) - Y0
        YLOC(I,7) = YLOC(I,7) - Y0
        YLOC(I,8) = YLOC(I,8) - Y0
        ZLOC(I,1) = ZLOC(I,1) - Z0
        ZLOC(I,2) = ZLOC(I,2) - Z0
        ZLOC(I,3) = ZLOC(I,3) - Z0
        ZLOC(I,4) = ZLOC(I,4) - Z0
        ZLOC(I,5) = ZLOC(I,5) - Z0
        ZLOC(I,6) = ZLOC(I,6) - Z0
        ZLOC(I,7) = ZLOC(I,7) - Z0
        ZLOC(I,8) = ZLOC(I,8) - Z0
      ENDDO
C-----------
C     PASSAGE DES VITESSES AU REPERE CONVECTE
C-----------
      DO I=1,NEL   
        VXLOC(I,1) = E1X(I)*VX1(I)+E1Y(I)*VY1(I)+E1Z(I)*VZ1(I)               
        VYLOC(I,1) = E2X(I)*VX1(I)+E2Y(I)*VY1(I)+E2Z(I)*VZ1(I)               
        VZLOC(I,1) = E3X(I)*VX1(I)+E3Y(I)*VY1(I)+E3Z(I)*VZ1(I)               
        VXLOC(I,2) = E1X(I)*VX2(I)+E1Y(I)*VY2(I)+E1Z(I)*VZ2(I)               
        VYLOC(I,2) = E2X(I)*VX2(I)+E2Y(I)*VY2(I)+E2Z(I)*VZ2(I)               
        VZLOC(I,2) = E3X(I)*VX2(I)+E3Y(I)*VY2(I)+E3Z(I)*VZ2(I)               
        VXLOC(I,3) = E1X(I)*VX3(I)+E1Y(I)*VY3(I)+E1Z(I)*VZ3(I)               
        VYLOC(I,3) = E2X(I)*VX3(I)+E2Y(I)*VY3(I)+E2Z(I)*VZ3(I)               
        VZLOC(I,3) = E3X(I)*VX3(I)+E3Y(I)*VY3(I)+E3Z(I)*VZ3(I)               
        VXLOC(I,4) = E1X(I)*VX4(I)+E1Y(I)*VY4(I)+E1Z(I)*VZ4(I)               
        VYLOC(I,4) = E2X(I)*VX4(I)+E2Y(I)*VY4(I)+E2Z(I)*VZ4(I)               
        VZLOC(I,4) = E3X(I)*VX4(I)+E3Y(I)*VY4(I)+E3Z(I)*VZ4(I)               
        VXLOC(I,5) = E1X(I)*VX5(I)+E1Y(I)*VY5(I)+E1Z(I)*VZ5(I)               
        VYLOC(I,5) = E2X(I)*VX5(I)+E2Y(I)*VY5(I)+E2Z(I)*VZ5(I)               
        VZLOC(I,5) = E3X(I)*VX5(I)+E3Y(I)*VY5(I)+E3Z(I)*VZ5(I)               
        VXLOC(I,6) = E1X(I)*VX6(I)+E1Y(I)*VY6(I)+E1Z(I)*VZ6(I)               
        VYLOC(I,6) = E2X(I)*VX6(I)+E2Y(I)*VY6(I)+E2Z(I)*VZ6(I)               
        VZLOC(I,6) = E3X(I)*VX6(I)+E3Y(I)*VY6(I)+E3Z(I)*VZ6(I)               
        VXLOC(I,7) = E1X(I)*VX7(I)+E1Y(I)*VY7(I)+E1Z(I)*VZ7(I)               
        VYLOC(I,7) = E2X(I)*VX7(I)+E2Y(I)*VY7(I)+E2Z(I)*VZ7(I)               
        VZLOC(I,7) = E3X(I)*VX7(I)+E3Y(I)*VY7(I)+E3Z(I)*VZ7(I)               
        VXLOC(I,8) = E1X(I)*VX8(I)+E1Y(I)*VY8(I)+E1Z(I)*VZ8(I)               
        VYLOC(I,8) = E2X(I)*VX8(I)+E2Y(I)*VY8(I)+E2Z(I)*VZ8(I)               
        VZLOC(I,8) = E3X(I)*VX8(I)+E3Y(I)*VY8(I)+E3Z(I)*VZ8(I)               
        VX0 = (VXLOC(I,1)+VXLOC(I,2)+VXLOC(I,3)+VXLOC(I,4)+ 
     .         VXLOC(I,5)+VXLOC(I,6)+VXLOC(I,7)+VXLOC(I,8))*UNHUIT
        VY0 = (VYLOC(I,1)+VYLOC(I,2)+VYLOC(I,3)+VYLOC(I,4)+ 
     .         VYLOC(I,5)+VYLOC(I,6)+VYLOC(I,7)+VYLOC(I,8))*UNHUIT
        VZ0 = (VZLOC(I,1)+VZLOC(I,2)+VZLOC(I,3)+VZLOC(I,4)+ 
     .         VZLOC(I,5)+VZLOC(I,6)+VZLOC(I,7)+VZLOC(I,8))*UNHUIT
        VXLOC(I,1) = VXLOC(I,1) - VX0 
        VYLOC(I,1) = VYLOC(I,1) - VY0 
        VZLOC(I,1) = VZLOC(I,1) - VZ0 
        VXLOC(I,2) = VXLOC(I,2) - VX0 
        VYLOC(I,2) = VYLOC(I,2) - VY0 
        VZLOC(I,2) = VZLOC(I,2) - VZ0 
        VXLOC(I,3) = VXLOC(I,3) - VX0 
        VYLOC(I,3) = VYLOC(I,3) - VY0 
        VZLOC(I,3) = VZLOC(I,3) - VZ0 
        VXLOC(I,4) = VXLOC(I,4) - VX0 
        VYLOC(I,4) = VYLOC(I,4) - VY0 
        VZLOC(I,4) = VZLOC(I,4) - VZ0 
        VXLOC(I,5) = VXLOC(I,5) - VX0 
        VYLOC(I,5) = VYLOC(I,5) - VY0 
        VZLOC(I,5) = VZLOC(I,5) - VZ0 
        VXLOC(I,6) = VXLOC(I,6) - VX0 
        VYLOC(I,6) = VYLOC(I,6) - VY0 
        VZLOC(I,6) = VZLOC(I,6) - VZ0 
        VXLOC(I,7) = VXLOC(I,7) - VX0 
        VYLOC(I,7) = VYLOC(I,7) - VY0 
        VZLOC(I,7) = VZLOC(I,7) - VZ0 
        VXLOC(I,8) = VXLOC(I,8) - VX0 
        VYLOC(I,8) = VYLOC(I,8) - VY0 
        VZLOC(I,8) = VZLOC(I,8) - VZ0 
      ENDDO
C
      DO I=1,NEL                                                
        R1X(I) = XLOC(I,1)
        R2X(I) = XLOC(I,2)
        R3X(I) = XLOC(I,3)
        R4X(I) = XLOC(I,4)
        R5X(I) = XLOC(I,5)
        R6X(I) = XLOC(I,6)
        R7X(I) = XLOC(I,7)
        R8X(I) = XLOC(I,8)
        R1Y(I) = YLOC(I,1)
        R2Y(I) = YLOC(I,2)
        R3Y(I) = YLOC(I,3)
        R4Y(I) = YLOC(I,4)
        R5Y(I) = YLOC(I,5)
        R6Y(I) = YLOC(I,6)
        R7Y(I) = YLOC(I,7)
        R8Y(I) = YLOC(I,8)
        R1Z(I) = ZLOC(I,1)
        R2Z(I) = ZLOC(I,2)
        R3Z(I) = ZLOC(I,3)
        R4Z(I) = ZLOC(I,4)
        R5Z(I) = ZLOC(I,5)
        R6Z(I) = ZLOC(I,6)
        R7Z(I) = ZLOC(I,7)
        R8Z(I) = ZLOC(I,8)
C
        RXX(I) = R2X(I)+R3X(I)+R6X(I)+R7X(I)-R1X(I)-R4X(I)-R5X(I)-R8X(I)
        RYY(I) = R3Y(I)+R4Y(I)+R7Y(I)+R8Y(I)-R1Y(I)-R2Y(I)-R5Y(I)-R6Y(I)
        RZZ(I) = R5Z(I)+R6Z(I)+R7Z(I)+R8Z(I)-R1Z(I)-R2Z(I)-R3Z(I)-R4Z(I)
        RXX(I) = ABS(RXX(I))
        RYY(I) = ABS(RYY(I))
        RZZ(I) = ABS(RZZ(I))
      ENDDO
C-----------
      IF (TIME == ZERO) THEN
        DO I=1,NEL
          Q(I,1) = E1X(I)
          Q(I,2) = E1Y(I)
          Q(I,3) = E1Z(I)
          Q(I,4) = E2X(I)
          Q(I,5) = E2Y(I)
          Q(I,6) = E2Z(I)
          Q(I,7) = E3X(I)
          Q(I,8) = E3Y(I)
          Q(I,9) = E3Z(I)
        ENDDO
      ENDIF
C-----------
      DO I=1,NEL
C       Matrice rotation E(n) -> E(n+1) : MROT = E(n)T * E(n+1)               
        MROT(1) = Q(I,1)*E1X(I) +Q(I,2)*E1Y(I) +Q(I,3)*E1Z(I)   
        MROT(2) = Q(I,4)*E1X(I) +Q(I,5)*E1Y(I) +Q(I,6)*E1Z(I)   
        MROT(3) = Q(I,7)*E1X(I) +Q(I,8)*E1Y(I) +Q(I,9)*E1Z(I)   
        MROT(4) = Q(I,1)*E2X(I) +Q(I,2)*E2Y(I) +Q(I,3)*E2Z(I)   
        MROT(5) = Q(I,4)*E2X(I) +Q(I,5)*E2Y(I) +Q(I,6)*E2Z(I)   
        MROT(6) = Q(I,7)*E2X(I) +Q(I,8)*E2Y(I) +Q(I,9)*E2Z(I)   
        MROT(7) = Q(I,1)*E3X(I) +Q(I,2)*E3Y(I) +Q(I,3)*E3Z(I)   
        MROT(8) = Q(I,4)*E3X(I) +Q(I,5)*E3Y(I) +Q(I,6)*E3Z(I)   
        MROT(9) = Q(I,7)*E3X(I) +Q(I,8)*E3Y(I) +Q(I,9)*E3Z(I)   
C
        Q(I,1) = E1X(I)
        Q(I,2) = E1Y(I)
        Q(I,3) = E1Z(I)
        Q(I,4) = E2X(I)
        Q(I,5) = E2Y(I)
        Q(I,6) = E2Z(I)
        Q(I,7) = E3X(I)
        Q(I,8) = E3Y(I)
        Q(I,9) = E3Z(I)
C--------------------   
C       Vecteur de la  rotation instantanee
C
        CS = UNDEMI * (MROT(1)+MROT(5)+MROT(9)-UN)
        IF (CS >= UN) THEN
          RV(1) = (MROT(6) - MROT(8)) * UNDEMI
          RV(2) = (MROT(7) - MROT(3)) * UNDEMI
          RV(3) = (MROT(2) - MROT(4)) * UNDEMI
        ELSEIF (CS <= -UN) THEN
          RV(1) = PI*SQRT((MROT(1)+UN)*UNDEMI)
          RV(2) = PI*SQRT((MROT(5)+UN)*UNDEMI)
          RV(3) = PI*SQRT((MROT(9)+UN)*UNDEMI)
          IF (MROT(2) < ZERO .AND. MROT(6) < ZERO) THEN
            RV(2) = -RV(2)
          ELSEIF (MROT(6) < ZERO .AND. MROT(7) < ZERO) THEN
            RV(3) = -RV(3)
          ELSEIF (MROT(7) < ZERO .AND. MROT(2) < ZERO) THEN
            RV(1) = -RV(1)
          ELSEIF (MROT(2) < ZERO) THEN
            RV(3) = -RV(3)
          ELSEIF (MROT(6) < ZERO) THEN
            RV(1) = -RV(1)
          ELSEIF (MROT(7) < ZERO) THEN
            RV(2) = -RV(2)
          ENDIF
        ELSE
          KSI   = ACOS(CS)
          SN    = UNDEMI*KSI/SIN(KSI)
          RV(1) = (MROT(6) - MROT(8)) * SN
          RV(2) = (MROT(7) - MROT(3)) * SN
          RV(3) = (MROT(2) - MROT(4)) * SN
        ENDIF
C--------------------   
c       dVr = (RV x R ) / dt      
        VXLOC(I,1) = VXLOC(I,1) - (RV(2)*R1Z(I) - RV(3)*R1Y(I))*DTI 
        VYLOC(I,1) = VYLOC(I,1) - (RV(3)*R1X(I) - RV(1)*R1Z(I))*DTI 
        VZLOC(I,1) = VZLOC(I,1) - (RV(1)*R1Y(I) - RV(2)*R1X(I))*DTI 
        VXLOC(I,2) = VXLOC(I,2) - (RV(2)*R2Z(I) - RV(3)*R2Y(I))*DTI 
        VYLOC(I,2) = VYLOC(I,2) - (RV(3)*R2X(I) - RV(1)*R2Z(I))*DTI 
        VZLOC(I,2) = VZLOC(I,2) - (RV(1)*R2Y(I) - RV(2)*R2X(I))*DTI 
        VXLOC(I,3) = VXLOC(I,3) - (RV(2)*R3Z(I) - RV(3)*R3Y(I))*DTI 
        VYLOC(I,3) = VYLOC(I,3) - (RV(3)*R3X(I) - RV(1)*R3Z(I))*DTI 
        VZLOC(I,3) = VZLOC(I,3) - (RV(1)*R3Y(I) - RV(2)*R3X(I))*DTI 
        VXLOC(I,4) = VXLOC(I,4) - (RV(2)*R4Z(I) - RV(3)*R4Y(I))*DTI 
        VYLOC(I,4) = VYLOC(I,4) - (RV(3)*R4X(I) - RV(1)*R4Z(I))*DTI 
        VZLOC(I,4) = VZLOC(I,4) - (RV(1)*R4Y(I) - RV(2)*R4X(I))*DTI 
        VXLOC(I,5) = VXLOC(I,5) - (RV(2)*R5Z(I) - RV(3)*R5Y(I))*DTI 
        VYLOC(I,5) = VYLOC(I,5) - (RV(3)*R5X(I) - RV(1)*R5Z(I))*DTI 
        VZLOC(I,5) = VZLOC(I,5) - (RV(1)*R5Y(I) - RV(2)*R5X(I))*DTI 
        VXLOC(I,6) = VXLOC(I,6) - (RV(2)*R6Z(I) - RV(3)*R6Y(I))*DTI 
        VYLOC(I,6) = VYLOC(I,6) - (RV(3)*R6X(I) - RV(1)*R6Z(I))*DTI 
        VZLOC(I,6) = VZLOC(I,6) - (RV(1)*R6Y(I) - RV(2)*R6X(I))*DTI 
        VXLOC(I,7) = VXLOC(I,7) - (RV(2)*R7Z(I) - RV(3)*R7Y(I))*DTI 
        VYLOC(I,7) = VYLOC(I,7) - (RV(3)*R7X(I) - RV(1)*R7Z(I))*DTI 
        VZLOC(I,7) = VZLOC(I,7) - (RV(1)*R7Y(I) - RV(2)*R7X(I))*DTI 
        VXLOC(I,8) = VXLOC(I,8) - (RV(2)*R8Z(I) - RV(3)*R8Y(I))*DTI 
        VYLOC(I,8) = VYLOC(I,8) - (RV(3)*R8X(I) - RV(1)*R8Z(I))*DTI 
        VZLOC(I,8) = VZLOC(I,8) - (RV(1)*R8Y(I) - RV(2)*R8X(I))*DTI 
      ENDDO
C-----------
!      IF (OFF_L < ZERO)THEN
!        DO I=1,NEL
!          IF (OFFG(I) < ZERO)THEN
!            VXLOC(I,1)=ZERO
!            VYLOC(I,1)=ZERO
!            VZLOC(1,I)=ZERO
!            VXLOC(I,2)=ZERO
!            VYLOC(I,2)=ZERO
!            VZLOC(2,I)=ZERO
!            VXLOC(I,3)=ZERO
!            VYLOC(I,3)=ZERO
!            VZLOC(I,3)=ZERO
!            VXLOC(4,I)=ZERO
!            VYLOC(4,I)=ZERO
!            VZLOC(I,4)=ZERO
!            VXLOC(5,I)=ZERO
!            VYLOC(5,I)=ZERO
!            VZLOC(5,I)=ZERO
!            VXLOC(6,I)=ZERO
!            VYLOC(6,I)=ZERO
!            VZLOC(6,I)=ZERO
!            VXLOC(7,I)=ZERO
!            VYLOC(7,I)=ZERO
!            VZLOC(7,I)=ZERO
!            VXLOC(8,I)=ZERO
!            VYLOC(8,I)=ZERO
!            VZLOC(8,I)=ZERO
!          ENDIF
!        ENDDO
!      ENDIF
C-----------
      CALL SDET43(NEL  ,
     .         XLOC ,YLOC ,ZLOC ,AREA ,AREAP,OFFG ,SOLID_ID,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
C-----------
      RETURN
      END
