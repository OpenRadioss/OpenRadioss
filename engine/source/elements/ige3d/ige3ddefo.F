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
Chd|  IGE3DDEFO                     source/elements/ige3d/ige3ddefo.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IGE3DDEFO(VX,VY,VZ,MATB,NCTRL,
     .                     WXX,WYY,WZZ,DXX,DYY,DZZ,DXY,DYX,
     .                     DYZ,DZY,DXZ,DZX,D4,D5,D6,BA,A)
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCTRL
C     REAL
      my_real
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*),
     .    D4(*),  D5(*),  D6(*),
     .   WXX(*), WYY(*), WZZ(*)
      my_real
     .   VX(NCTRL,MVSIZ),VY(NCTRL,MVSIZ),VZ(NCTRL,MVSIZ),
     .   BA(6,MVSIZ), A(3*NCTRL,MVSIZ),MATB(3*NCTRL,MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, INCTRL
C     REAL
      my_real
     .   DT1D2, DT1D, AAA
C=======================================================================

      DO I=LFT,LLT
        DXX(I)=ZERO
        DYY(I)=ZERO
        DZZ(I)=ZERO
        DXY(I)=ZERO
        DXZ(I)=ZERO
        DYX(I)=ZERO
        DYZ(I)=ZERO
        DZX(I)=ZERO
        DZY(I)=ZERO
        WXX(I)=ZERO
        WYY(I)=ZERO
        WZZ(I)=ZERO
      ENDDO
C                   NEL
C   [B] = [dNi/dx  dNk/dx  ..  ] \  3
C         [dNi/dy  dNk/dy  ..  ] |  *
C         [dNi/dz  dNk/dz  ..  ] |  N
C         [dNj/dx  dNl/dx  ..  ] |  C
C         [dNj/dy  dNl/dy  ..  ] |  T
C         [dNj/dz  dNl/dz  ..  ] |  R
C         [  ..      ..    ..  ] /  L

      DO I=LFT,LLT
        DO J=1,NCTRL
          INCTRL = (J-1)*3
          DXX(I)=DXX(I)+MATB(INCTRL+1,I)*VX(J,I)
          DYY(I)=DYY(I)+MATB(INCTRL+2,I)*VY(J,I)
          DZZ(I)=DZZ(I)+MATB(INCTRL+3,I)*VZ(J,I)
          DXY(I)=DXY(I)+MATB(INCTRL+2,I)*VX(J,I)
          DYX(I)=DYX(I)+MATB(INCTRL+1,I)*VY(J,I)
          DYZ(I)=DYZ(I)+MATB(INCTRL+3,I)*VY(J,I)
          DZY(I)=DZY(I)+MATB(INCTRL+2,I)*VZ(J,I)
          DZX(I)=DZX(I)+MATB(INCTRL+1,I)*VZ(J,I)
          DXZ(I)=DXZ(I)+MATB(INCTRL+3,I)*VX(J,I)
        ENDDO
      ENDDO

          DT1D2=HALF*DT1

      DO I=LFT,LLT
          DXX(I)  = DXX(I)
     .             -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
          DYY(I)  = DYY(I)
     .             -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
          DZZ(I)  = DZZ(I)
     .             -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
          AAA = DT1D2*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
          DXY(I)  = DXY(I) - AAA
          DYX(I)  = DYX(I) - AAA
          D4(I)   = DXY(I)+DYX(I)
          AAA = DT1D2*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
          DYZ(I)  = DYZ(I) - AAA
          DZY(I)  = DZY(I) - AAA
          D5(I)   = DYZ(I)+DZY(I)
          AAA = DT1D2*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
          DXZ(I)  = DXZ(I) - AAA
          DZX(I)  = DZX(I) - AAA
          D6(I)   = DXZ(I)+DZX(I)

          WXX(I)=DT1D2*(DZY(I)-DYZ(I))
          WYY(I)=DT1D2*(DXZ(I)-DZX(I))
          WZZ(I)=DT1D2*(DYX(I)-DXY(I))
      ENDDO



C-----------
      RETURN
      END
