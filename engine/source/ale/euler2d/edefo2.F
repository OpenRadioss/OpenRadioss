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
Chd|  EDEFO2                        source/ale/euler2d/edefo2.F   
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EDEFO2(VOL,V,VEUL,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  WYZ,DYZ,DZY,EYY,EZZ,ETT,EYZ,EYT,EZT,
     .                  VOLN,AIRE,DELTAX,VDY,VDZ,VD2,
     .                  NC1,NC2,NC3,NC4)
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
C     REAL ou REAL*8
      my_real
     .   VOL(*), V(3,*), VEUL(10,*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .   PY1(*), PY2(*), PZ1(*), PZ2(*),
     .   WYZ(*), DYZ(*), DZY(*),
     .   EYY(*),EZZ(*), ETT(*), EYZ(*), EYT(*), EZT(*),
     .   VOLN(*),AIRE(*),DELTAX(*),VDY(*),VDZ(*),VD2(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
C     REAL ou REAL*8
      my_real
     .   VY13(MVSIZ), VY24(MVSIZ), VZ13(MVSIZ),VZ24(MVSIZ),YAVG(MVSIZ),
     .   DPY,DPZ
C-----------------------------------------------
      DO 10 I=LFT,LLT
      II=I+NFT
      PY1(I)=UNDEMI*(Z2(I)-Z4(I))
      PY2(I)=UNDEMI*(Z3(I)-Z1(I))
      PZ1(I)=UNDEMI*(Y4(I)-Y2(I))
      PZ2(I)=UNDEMI*(Y1(I)-Y3(I))      
      AIRE(I)=VEUL(1,II)
  10  DELTAX(I)=VEUL(2,II)
C
      IF(N2D.EQ.1) THEN
       DO 20 I=LFT,LLT
   20  VOLN(I)=VOL(I)
      ELSE
       DO 30 I=LFT,LLT
   30  VOLN(I)=AIRE(I)
      ENDIF
C
      DO 40 I=LFT,LLT
      VY1(I)=V(2,NC1(I))
      VZ1(I)=V(3,NC1(I))
      VY2(I)=V(2,NC2(I))
      VZ2(I)=V(3,NC2(I))
      VY3(I)=V(2,NC3(I))
      VZ3(I)=V(3,NC3(I))
      VY4(I)=V(2,NC4(I))
      VZ4(I)=V(3,NC4(I))
   40 CONTINUE
C
      IF(MTN.NE.11)THEN
       DO 50 I=LFT,LLT
        YAVG(I) =Y1(I)+Y2(I)+Y3(I)+Y4(I)
        VY13(I)=VY1(I)-VY3(I)
        VY24(I)=VY2(I)-VY4(I)
        VZ13(I)=VZ1(I)-VZ3(I)
        VZ24(I)=VZ2(I)-VZ4(I)
        VDY(I)=QUART*(VY1(I)+VY2(I)+VY3(I)+VY4(I))
        VDZ(I)=QUART*(VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I))
        VD2(I)=VDY(I)**2+VDZ(I)**2
   50  CONTINUE
C
       DO 60  I=LFT,LLT
        EYY(I)=(PY1(I)*VY13(I)+PY2(I)*VY24(I))/AIRE(I)
        EZZ(I)=(PZ1(I)*VZ13(I)+PZ2(I)*VZ24(I))/AIRE(I)
        ETT(I)=ZERO
        DZY(I)=(PY1(I)*VZ13(I)+PY2(I)*VZ24(I))/AIRE(I)
        DYZ(I)=(PZ1(I)*VY13(I)+PZ2(I)*VY24(I))/AIRE(I)
        EYT(I)=ZERO
        EZT(I)=ZERO
   60  CONTINUE
C
       IF(N2D.EQ.1) THEN
        DO 65 I=LFT,LLT
         ETT(I)=(VY1(I)+VY2(I)+VY3(I)+VY4(I))/YAVG(I)
   65   CONTINUE
       ENDIF
C
       DO 70 I=LFT,LLT
        EYZ(I)= DZY(I)+DYZ(I)
   70   WYZ(I)=UNDEMI*DT1*(DZY(I)-DYZ(I))
      ENDIF
C
      RETURN
      END
