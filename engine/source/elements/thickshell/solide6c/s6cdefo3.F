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
Chd|  S6CDEFC3                      source/elements/thickshell/solide6c/s6cdefo3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CDEFC3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, 
     .   VY1, VY2, VY3, VY4, VY5, VY6, 
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, 
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .   WXX, WYY, WZZ, 
     .   DHXX, DHXY, DHXZ,DHYX, DHYY, DHYZ,DHZX, DHZY, DHZZ,
     .   DH4, DH5, DH6,
     .   PX1H, PX2H, PX3H, 
     .   PY1H, PY2H, PY3H, 
     .   PZ1H, PZ2H, PZ3H,  
     .   JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .   B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .   DDHV,NU) 
Cqz ---
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
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), 
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), 
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), 
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*),
Cqz +++
     .   PX1H(*), PX2H(*), PX3H(*), 
     .   PY1H(*), PY2H(*), PY3H(*), 
     .   PZ1H(*), PZ2H(*), PZ3H(*), 
     .   JI33(*),B1X(MVSIZ,2),B1Y(MVSIZ,2),B2X(MVSIZ,2),B2Y(MVSIZ,2),
     .   B1XH(MVSIZ,2),B1YH(MVSIZ,2),B2XH(MVSIZ,2),B2YH(MVSIZ,2),
     .   B1122(*),B1221(*),B2212(*),B1121(*),
     .   B1122H(*),B1221H(*),B2212H(*),B1121H(*),
     .   DHXX(*), DHXY(*), DHXZ(*),  
     .   DHYX(*), DHYY(*), DHYZ(*), 
     .   DHZX(*), DHZY(*), DHZZ(*),DH4(*), DH5(*), DH6(*),
     .   DDHV(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C     REAL
      my_real
     .   VX14(MVSIZ), VY14(MVSIZ), VZ14(MVSIZ),
     .   VX25(MVSIZ), VY25(MVSIZ), VZ25(MVSIZ),
     .   VX36(MVSIZ), VY36(MVSIZ), VZ36(MVSIZ),
     .   VX14N(MVSIZ), VY14N(MVSIZ), VZ14N(MVSIZ),
     .   VX25N(MVSIZ), VY25N(MVSIZ), VZ25N(MVSIZ),
     .   VX36N(MVSIZ), VY36N(MVSIZ), VZ36N(MVSIZ),
     .   VX3614N(MVSIZ),VY3614N(MVSIZ),VX2514N(MVSIZ),VY2514N(MVSIZ),
     .   VXHI(MVSIZ), VYHI(MVSIZ), VZHI(MVSIZ), DT1D2, DT1D1
      my_real 
     .  NU1,PXSVX,PYSVY,PZSVZ,PXAVX,PYAVY,PZAVZ,TERMAD
C-----------------------------------------------
      DO I=LFT,LLT
       VX14(I)=VX1(I)+VX4(I)
       VX25(I)=VX2(I)+VX5(I)
       VX36(I)=VX3(I)+VX6(I)
       VXHI(I)=VX4(I)+VX5(I)+VX6(I)-VX1(I)-VX2(I)-VX3(I)
       VY14(I)=VY1(I)+VY4(I)
       VY25(I)=VY2(I)+VY5(I)
       VY36(I)=VY3(I)+VY6(I)
       VYHI(I)=VY4(I)+VY5(I)+VY6(I)-VY1(I)-VY2(I)-VY3(I)
       VZ14(I)=VZ1(I)+VZ4(I)
       VZ25(I)=VZ2(I)+VZ5(I)
       VZ36(I)=VZ3(I)+VZ6(I)
       VZHI(I)=VZ4(I)+VZ5(I)+VZ6(I)-VZ1(I)-VZ2(I)-VZ3(I)
      ENDDO
      DO I=LFT,LLT
       VX14N(I)=-VX1(I)+VX4(I)
       VX25N(I)=-VX2(I)+VX5(I)
       VX36N(I)=-VX3(I)+VX6(I)
       VY14N(I)=-VY1(I)+VY4(I)
       VY25N(I)=-VY2(I)+VY5(I)
       VY36N(I)=-VY3(I)+VY6(I)
       VZ14N(I)=-VZ1(I)+VZ4(I)
       VZ25N(I)=-VZ2(I)+VZ5(I)
       VZ36N(I)=-VZ3(I)+VZ6(I)
C
       VX3614N(I)=VX36N(I)-VX14N(I)
       VY3614N(I)=VY36N(I)-VY14N(I)
       VX2514N(I)=VX25N(I)-VX14N(I)
       VY2514N(I)=VY25N(I)-VY14N(I)
      ENDDO
C                                                                     12
Cqz ----------constant part--------------------------------------
      DO I=LFT,LLT
        DXX(I)=PX1(I)*VX14(I)+PX2(I)*VX25(I)+
     .         PX3(I)*VX36(I)+PX4(I)*VXHI(I)
        DYY(I)=PY1(I)*VY14(I)+PY2(I)*VY25(I)+
     .         PY3(I)*VY36(I)+PY4(I)*VYHI(I)
        DZZ(I)=PZ1(I)*VZ14(I)+PZ2(I)*VZ25(I)+
     .         PZ3(I)*VZ36(I)+PZ4(I)*VZHI(I)
        DXY(I)=PY1(I)*VX14(I)+PY2(I)*VX25(I)+
     .         PY3(I)*VX36(I)+PY4(I)*VXHI(I)
        DYX(I)=PX1(I)*VY14(I)+PX2(I)*VY25(I)+
     .         PX3(I)*VY36(I)+PX4(I)*VYHI(I)
Cqz --------shear traitement-------
        DXZ(I)=JI33(I)*TWO*VXHI(I)
     .         -B1122(I)*VX36N(I)+B1221(I)*VX25N(I)
     .         +B2212(I)*(VY25N(I)-VY36N(I))  
        DXZ(I)= DXZ(I)+JI33(I)*(VXHI(I)-VX14N(I))
     .         -B1X(I,1)*VX3614N(I)+B1X(I,2)*VX2514N(I)  
     .         -B1Y(I,1)*VY3614N(I)+B1Y(I,2)*VY2514N(I)  
        DYZ(I)=JI33(I)*TWO*VYHI(I)
     .         -B1122(I)*VY25N(I)+B1221(I)*VY36N(I)
     .         +B1121(I)*(VX36N(I)-VX25N(I))  
        DYZ(I)= DYZ(I)+ JI33(I)*(VYHI(I)-VY14N(I))
     .         +B2X(I,1)*VX3614N(I)-B2X(I,2)*VX2514N(I)
     .         +B2Y(I,1)*VY3614N(I)-B2Y(I,2)*VY2514N(I)  
C
        DZX(I)=HALF*(PX1(I)*VZ14(I)+PX2(I)*VZ25(I)+
     .         PX3(I)*VZ36(I))
        DZY(I)=HALF*(PY1(I)*VZ14(I)+PY2(I)*VZ25(I)+
     .         PY3(I)*VZ36(I))
      ENDDO
C
      DO I=LFT,LLT
        WXX(I)=ZERO
        WYY(I)=ZERO
        WZZ(I)=ZERO
      ENDDO
Cqz ----------non-constant part--------------------------------------
      DO I=LFT,LLT
        NU1 = NU(I)/(ONE - NU(I))
        PXAVX = PX1(I)*VX14N(I)+PX2(I)*VX25N(I)+PX3(I)*VX36N(I)
        PYAVY = PY1(I)*VY14N(I)+PY2(I)*VY25N(I)+PY3(I)*VY36N(I)
        PZAVZ = PZ1(I)*VZ14N(I)+PZ2(I)*VZ25N(I)+PZ3(I)*VZ36N(I)
        PXSVX = PX1H(I)*VX14(I)+PX2H(I)*VX25(I)+PX3H(I)*VX36(I)
        PYSVY = PY1H(I)*VY14(I)+PY2H(I)*VY25(I)+PY3H(I)*VY36(I)
        PZSVZ = PZ1H(I)*VZ14(I)+PZ2H(I)*VZ25(I)+PZ3H(I)*VZ36(I)
        TERMAD=-NU(I)*PYAVY-NU1*PZAVZ
        DDHV(I)= TERMAD
        DHXX(I)= PXSVX+PXAVX+TERMAD
        TERMAD=-NU(I)*PXSVX-NU1*PZSVZ
        DDHV(I)= DDHV(I)+TERMAD
        DHYY(I)= PYSVY+PYAVY+TERMAD
        TERMAD=-NU(I)*(PXSVX+PYAVY)-NU1*(PXAVX+PYSVY)
        DDHV(I)= DDHV(I)+TERMAD
        DHZZ(I)= PZSVZ+PZAVZ+TERMAD
        DHXY(I)=PY1(I)*VX14N(I)+PY2(I)*VX25N(I)+
     .          PY3(I)*VX36N(I)+
     .          PY1H(I)*VX14(I)+PY2H(I)*VX25(I)+
     .          PY3H(I)*VX36(I)
        DHYX(I)=PX1(I)*VY14N(I)+PX2(I)*VY25N(I)+
     .          PX3(I)*VY36N(I)+
     .          PX1H(I)*VY14(I)+PX2H(I)*VY25(I)+
     .          PX3H(I)*VY36(I)
Cqz --------shear traitement-------
        DHXZ(I)=-B1122H(I)*VX36N(I)+B1221H(I)*VX25N(I)
     .          +B2212H(I)*(VY25N(I)-VY36N(I))  
        DHXZ(I)= DHXZ(I)
     .         -B1XH(I,1)*VX3614N(I)+B1XH(I,2)*VX2514N(I)  
     .         -B1YH(I,1)*VY3614N(I)+B1YH(I,2)*VY2514N(I)  
        DHYZ(I)=-B1122H(I)*VY25N(I)+B1221H(I)*VY36N(I)
     .          +B1121H(I)*(VX36N(I)-VX25N(I))  
        DHYZ(I)= DHYZ(I) 
     .         +B2XH(I,1)*VX3614N(I)-B2XH(I,2)*VX2514N(I)
     .         +B2YH(I,1)*VY3614N(I)-B2YH(I,2)*VY2514N(I)  
        DHZX(I)=HALF*(PX1H(I)*VZ14(I)+PX2H(I)*VZ25(I)+
     .          PX3H(I)*VZ36(I))
        DHZY(I)=HALF*(PY1H(I)*VZ14(I)+PY2H(I)*VZ25(I)+
     .          PY3H(I)*VZ36(I))
      ENDDO
C
      RETURN
      END
