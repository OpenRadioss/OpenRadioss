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
Chd|  SCDEFC3                       source/elements/thickshell/solidec/scdefo3.F
Chd|-- called by -----------
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCDEFC3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .   WXX, WYY, WZZ, 
     .   DHXX, DHXY, DHYX, DHYY, DHZX, DHZY, DHZZ,DH4, DH5, DH6,
     .   PX1H1, PX1H2 , PX2H1, PX2H2, PX3H1, PX3H2 , PX4H1, PX4H2, 
     .   HGX1 , HGY2 , HGZ1 , HGZ2 ,
     .   RX0, RY0, SX0, SY0,NU,DDHV) 
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
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*), 
     .   DHXX(*), DHXY(*), 
     .   DHYX(*), DHYY(*), 
     .   DHZX(*), DHZY(*), DHZZ(*),DH4(*), DH5(*), DH6(*),
     .   PX1H1(*), PX1H2(*) , PX2H1(*), PX2H2(*), 
     .   PX3H1(*), PX3H2(*) , PX4H1(*), PX4H2(*), 
     .   HGX1(*),  HGY2(*)  , HGZ1(*), HGZ2(*),
     .   RX0(*), RY0(*), SX0(*), SY0(*),NU(*),DDHV(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C     REAL
      my_real
     .   VX17(MVSIZ), VY17(MVSIZ), VZ17(MVSIZ),
     .   VX28(MVSIZ), VY28(MVSIZ), VZ28(MVSIZ),
     .   VX35(MVSIZ), VY35(MVSIZ), VZ35(MVSIZ),
     .   VX46(MVSIZ), VY46(MVSIZ), VZ46(MVSIZ), DT1D2,DT1D1
      my_real 
     .   PXX2,PYY2,PZZ2,PXX2P,PYY2P,PZZ2P,
     .   HGY1(MVSIZ), HGX2(MVSIZ),
     .   H1VX,H1VY,H1VZ,H2VX,H2VY,H2VZ,NU1,
     .   X1VX,X2VX,Y1VY,Y2VY,Y2VY_NU,X1VX_NU
C=======================================================================
      DO I=LFT,LLT
       VX17(I)=VX1(I)-VX7(I)
       VX28(I)=VX2(I)-VX8(I)
       VX35(I)=VX3(I)-VX5(I)
       VX46(I)=VX4(I)-VX6(I)
       VY17(I)=VY1(I)-VY7(I)
       VY28(I)=VY2(I)-VY8(I)
       VY35(I)=VY3(I)-VY5(I)
       VY46(I)=VY4(I)-VY6(I)
       VZ17(I)=VZ1(I)-VZ7(I)
       VZ28(I)=VZ2(I)-VZ8(I)
       VZ35(I)=VZ3(I)-VZ5(I)
       VZ46(I)=VZ4(I)-VZ6(I)
      ENDDO
C                                                                     12
      DO I=LFT,LLT
        DXX(I)=PX1(I)*VX17(I)+PX2(I)*VX28(I)+
     .         PX3(I)*VX35(I)+PX4(I)*VX46(I)
        DYY(I)=PY1(I)*VY17(I)+PY2(I)*VY28(I)+
     .         PY3(I)*VY35(I)+PY4(I)*VY46(I)
        DZZ(I)=PZ1(I)*VZ17(I)+PZ2(I)*VZ28(I)+
     .         PZ3(I)*VZ35(I)+PZ4(I)*VZ46(I)
        DXY(I)=PY1(I)*VX17(I)+PY2(I)*VX28(I)+
     .         PY3(I)*VX35(I)+PY4(I)*VX46(I)
        DXZ(I)=PZ1(I)*VX17(I)+PZ2(I)*VX28(I)+
     .         PZ3(I)*VX35(I)+PZ4(I)*VX46(I)
        DYX(I)=PX1(I)*VY17(I)+PX2(I)*VY28(I)+
     .         PX3(I)*VY35(I)+PX4(I)*VY46(I)
        DYZ(I)=PZ1(I)*VY17(I)+PZ2(I)*VY28(I)+
     .         PZ3(I)*VY35(I)+PZ4(I)*VY46(I)
        DZX(I)=PX1(I)*VZ17(I)+PX2(I)*VZ28(I)+
     .         PX3(I)*VZ35(I)+PX4(I)*VZ46(I)
        DZY(I)=PY1(I)*VZ17(I)+PY2(I)*VZ28(I)+
     .         PY3(I)*VZ35(I)+PY4(I)*VZ46(I)
      ENDDO
C      
       DO I=LFT,LLT
        WXX(I)=ZERO
        WYY(I)=ZERO
        WZZ(I)=ZERO
       ENDDO
C------------------------------------------------
C 1 1 -1 -1 -1 -1 1 1
      DO I=LFT,LLT
        H1VX=VX1(I)+VX2(I)-VX3(I)-VX4(I)
     .         -VX5(I)-VX6(I)+VX7(I)+VX8(I)
        H1VY=VY1(I)+VY2(I)-VY3(I)-VY4(I)
     .         -VY5(I)-VY6(I)+VY7(I)+VY8(I)
        H1VZ=VZ1(I)+VZ2(I)-VZ3(I)-VZ4(I)
     .         -VZ5(I)-VZ6(I)+VZ7(I)+VZ8(I)
        HGX1(I)=ONE_OVER_8*H1VX-
     .         (PX1H1(I)*VX17(I)+PX2H1(I)*VX28(I)+
     .          PX3H1(I)*VX35(I)+PX4H1(I)*VX46(I))
        HGY1(I)=ONE_OVER_8*H1VY-	
     .         (PX1H1(I)*VY17(I)+PX2H1(I)*VY28(I)+
     .          PX3H1(I)*VY35(I)+PX4H1(I)*VY46(I))
        HGZ1(I)=ONE_OVER_8*H1VZ-
     .         (PX1H1(I)*VZ17(I)+PX2H1(I)*VZ28(I)+
     .          PX3H1(I)*VZ35(I)+PX4H1(I)*VZ46(I))
      ENDDO
C 1 -1 -1 1 -1 1 1 -1
      DO I=LFT,LLT
        H2VX=VX1(I)-VX2(I)-VX3(I)+VX4(I)
     .         -VX5(I)+VX6(I)+VX7(I)-VX8(I)
        H2VY=VY1(I)-VY2(I)-VY3(I)+VY4(I)
     .         -VY5(I)+VY6(I)+VY7(I)-VY8(I)
        H2VZ=VZ1(I)-VZ2(I)-VZ3(I)+VZ4(I)
     .         -VZ5(I)+VZ6(I)+VZ7(I)-VZ8(I)
        HGX2(I)=ONE_OVER_8*H2VX-
     .         (PX1H2(I)*VX17(I)+PX2H2(I)*VX28(I)+
     .          PX3H2(I)*VX35(I)+PX4H2(I)*VX46(I))
        HGY2(I)=ONE_OVER_8*H2VY-
     .         (PX1H2(I)*VY17(I)+PX2H2(I)*VY28(I)+
     .          PX3H2(I)*VY35(I)+PX4H2(I)*VY46(I))
        HGZ2(I)=ONE_OVER_8*H2VZ-
     .         (PX1H2(I)*VZ17(I)+PX2H2(I)*VZ28(I)+
     .          PX3H2(I)*VZ35(I)+PX4H2(I)*VZ46(I))
      ENDDO
      DO I=LFT,LLT
       NU1 = NU(I)/(ONE - NU(I))
       X1VX =-RY0(I)*HGX1(I)
       X2VX =SY0(I)*HGX2(I)
       Y1VY =RX0(I)*HGY1(I)
       Y2VY =-SX0(I)*HGY2(I)
       Y2VY_NU=-NU(I)*Y2VY
       X1VX_NU=-NU(I)*X1VX
       DHXX(I)= X1VX+X2VX+Y2VY_NU
       DHYY(I)= Y1VY+Y2VY+X1VX_NU
       DDHV(I)= X1VX_NU+Y2VY_NU
       DHZZ(I)= DDHV(I)-NU1*(X2VX+Y1VY) 
       DHXY(I)=  RX0(I)*HGX1(I)-SX0(I)*HGX2(I)
       DHYX(I)= -RY0(I)*HGY1(I)+SY0(I)*HGY2(I)
       DHZX(I)= -RY0(I)*HGZ1(I)
       DHZY(I)= -SX0(I)*HGZ2(I)
      ENDDO
C
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SCDEFO3                       source/elements/thickshell/solidec/scdefo3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCDEFO3( 
     .   DXX, DXY,DXZ, DYX, DYY,DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .   DCXX, DCXY,DCXZ, DCYX, DCYY,DCYZ,DCZX,DCZY, DCZZ,DC4, DC5,DC6,
     .   DHXX, DHXY,DHXZ, DHYX,DHYY,DHYZ, DHZX, DHZY, DHZZ,DH4,DH5,DH6,
     .   ZI,WI,VZL,VOL,VOLG,VOLO,DDHV,SIG,SIGZM,VOLM,USB,
     .   EINT,OFF,OFFG,DTI,OFFS,DVC,NEL,VOL0DP,VOLDP)
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
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   DXX(*), DXY(*), DXZ(*), DYX(*), DYY(*),DYZ(*), 
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   DCXX(*), DCXY(*),DCXZ(*), DCYX(*), DCYY(*),DCYZ(*), 
     .   DCZX(*), DCZY(*), DCZZ(*), DC4(*), DC5(*), DC6(*),
     .   DHXX(*), DHXY(*),DHXZ(*), DHYX(*), DHYY(*),DHYZ(*), 
     .   DHZX(*), DHZY(*), DHZZ(*), 
     .   DH4(*), DH5(*), DH6(*), DDHV(*),
     .   ZI,WI,VZL(*),VOL(*),VOLG(*),VOLO(*),
     .   SIG(NEL,6),SIGZM(*),VOLM(*),USB(*),EINT(*),OFF(*),OFFG(*),DTI,
     .   OFFS(*),DVC(*)
      DOUBLE PRECISION 
     .   VOL0DP(*),VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real
     .  DV,DVZ,DDV,TOL,DT1D2,DT1D1
C=======================================================================
      TOL=ONE-EM20
      DO I=LFT,LLT
        VOLDP(I)=HALF*WI*(VOLG(I)+VZL(I)*ZI)
        VOL(I)=VOLDP(I)
Cqz------due to the assumed strain terms----
        DV = DDHV(I)*DT1*ZI
Cqz------due to the sigzz traitement----
        DVZ = -(SIG(I,3)-SIGZM(I)/MAX(EM20,VOLM(I)))*USB(I)
        OFF(I)=OFFG(I)
        DDV = (DV+DVZ)*OFF(I)
        DVC(I) = DDV
        IF (VOL(I).LE.ZERO.OR.DDV.GT.TOL) THEN
         VOL(I)= EM20
         OFF(I)= ZERO
         DDV   = ZERO
        ELSEIF(OFF(I).EQ.ZERO.OR.OFFS(I).EQ.TWO.OR.ISMDISP.GT.0)THEN
         VOLDP(I)=MAX(EM20,VOLDP(I))
         VOL(I)=MAX(EM20,VOL(I))
         DDV   =ZERO
        ENDIF
C
        IF (IRESP==1) VOL0DP(I) = VOL0DP(I)*(ONE- DDV)
        VOLO(I) = VOLO(I)*(ONE- DDV)
        EINT(I) = EINT(I)/(ONE- DDV)
C	
        IF(ISMDISP.GT.0.OR.OFFS(I).EQ.TWO) DVC(I) = DVZ
      ENDDO
Cqz +++ -----partie non constante------
      DO I=LFT,LLT
        DXX(I)=DCXX(I)+ZI*DHXX(I)
        DYY(I)=DCYY(I)+ZI*DHYY(I)
        DZZ(I)=DCZZ(I)+ZI*DHZZ(I)
        DXY(I)=DCXY(I)+ZI*DHXY(I)
        DYX(I)=DCYX(I)+ZI*DHYX(I)
        DZX(I)=DCZX(I)+ZI*DHZX(I)
        DZY(I)=DCZY(I)+ZI*DHZY(I)
        DXZ(I)=DCXZ(I)+ZI*DHXZ(I)
        DYZ(I)=DCYZ(I)+ZI*DHYZ(I)
c        D4(I)=DC4(I)+ZI*DH4(I)
c        D5(I)=DC5(I)+ZI*DH5(I)
c        D6(I)=DC6(I)+ZI*DH6(I)
      ENDDO
C-----------
      DT1D1=DT1
      IF (ISMDISP.GT.0.AND.ISCAU.EQ.0) DT1D1=ZERO
      DT1D2=HALF*DT1D1
       DO I=LFT,LLT
        D4(I)   = DXY(I)+DYX(I)
     .           -DT1D1*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
        D5(I)   = DYZ(I)+DZY(I)
     .           -DT1D1*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
        D6(I)   = DXZ(I)+DZX(I)
     .           -DT1D1*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
        DXX(I)  = DXX(I)
     .           -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
        DYY(I)  = DYY(I)
     .           -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
        DZZ(I)  = DZZ(I)
     .           -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
       ENDDO
C       
      RETURN
      END
