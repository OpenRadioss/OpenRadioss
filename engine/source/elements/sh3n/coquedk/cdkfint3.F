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
	SUBROUTINE CDKFINT3(JFT,JLT,VOL,THK0,FOR,MOM,
     1                      PX2,PY2,PX3,PY3,
     2                      BZ1,BZ2,BZ3,
     3                      BRX1,BRX2,BRX3,BRY1,BRY2,BRY3,
     4                      F11,F12,F13,F21,F22,F23,F32,F33,
     5                      M11,M12,M13,M21,M22,M23,
     6                      NEL)
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NG,NEL
      MY_REAL 
     .   FOR(NEL,5),MOM(NEL,3), 
     .   PX2(*), PY2(*), PX3(*), PY3(*), 
     .   BRX1(MVSIZ,3),BRX2(MVSIZ,3),BRX3(MVSIZ,3),BRY1(MVSIZ,3),BRY2(MVSIZ,3),BRY3(MVSIZ,3),
     .   BZ1(MVSIZ,2),BZ2(MVSIZ,2),BZ3(MVSIZ,2),
     .   F11(*), F12(*), F13(*),
     .   F21(*), F22(*), F23(*), F32(*), F33(*),
     .   M11(*), M12(*), M13(*),
     .   M21(*), M22(*), M23(*),
     .   THK0(*),VOL(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I
      MY_REAL 
     .    C2,FX2,FX3,FY2,FY3
C--------------------------------------------------------------------------------------------------
      DO I=JFT,JLT
 	C2=VOL(I)
        FX2 =C2*(PX2(I)*FOR(I,1)+PY2(I)*FOR(I,3))
        FY2 =C2*(PY2(I)*FOR(I,2)+PX2(I)*FOR(I,3))
        FX3 =C2*(PX3(I)*FOR(I,1)+PY3(I)*FOR(I,3))
        FY3 =C2*(PY3(I)*FOR(I,2)+PX3(I)*FOR(I,3))
        F12(I)=F12(I)+FX2
        F22(I)=F22(I)+FY2
        F13(I)=F13(I)+FX3
        F23(I)=F23(I)+FY3
C        F11(I)=F11(I)-FX2-FX3
C        F21(I)=F21(I)-FY2-FY3
      ENDDO     
      DO I=JFT,JLT
 	C2=VOL(I)*THK0(I)
        F32(I)=F32(I)+C2*(BZ1(I,1)*MOM(I,1)+BZ2(I,1)*MOM(I,2)+
     .                    BZ3(I,1)*MOM(I,3))
        F33(I)=F33(I)+C2*(BZ1(I,2)*MOM(I,1)+BZ2(I,2)*MOM(I,2)+
     .                    BZ3(I,2)*MOM(I,3))
        M11(I)=M11(I)+C2*(BRX1(I,1)*MOM(I,1)+BRX2(I,1)*MOM(I,2)+
     .                    BRX3(I,1)*MOM(I,3))
        M21(I)=M21(I)+C2*(BRY1(I,1)*MOM(I,1)+BRY2(I,1)*MOM(I,2)+
     .                    BRY3(I,1)*MOM(I,3))
        M12(I)=M12(I)+C2*(BRX1(I,2)*MOM(I,1)+BRX2(I,2)*MOM(I,2)+
     .                    BRX3(I,2)*MOM(I,3))
        M22(I)=M22(I)+C2*(BRY1(I,2)*MOM(I,1)+BRY2(I,2)*MOM(I,2)+
     .                    BRY3(I,2)*MOM(I,3))
        M13(I)=M13(I)+C2*(BRX1(I,3)*MOM(I,1)+BRX2(I,3)*MOM(I,2)+
     .                    BRX3(I,3)*MOM(I,3))
        M23(I)=M23(I)+C2*(BRY1(I,3)*MOM(I,1)+BRY2(I,3)*MOM(I,2)+
     .                    BRY3(I,3)*MOM(I,3))
      ENDDO     
C    
      RETURN
      END
