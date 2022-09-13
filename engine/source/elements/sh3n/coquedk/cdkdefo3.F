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
Chd|  CDKDEFO3                      source/elements/sh3n/coquedk/cdkdefo3.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKDEFO3(JFT,JLT,VLX,VLY,PX2,PY2,PX3,PY3,
     1                    EXX,EYY,EXY,EXZ,EYZ,DT1,EPSDOT,NFT,
     2                    ISTRAIN,GSTR,VDEF,NEL)
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
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C  ds007  18/2/00   +1
      INTEGER JFT, JLT,NFT,ISTRAIN,NEL
C     REAL
      my_real
     .   PX2(*), PY2(*), PX3(*), PY3(*), 
     .   VLX(MVSIZ,2),VLY(MVSIZ,2),EXX(*),EYY(*),EXY(*),EXZ(*),EYZ(*),
     .   GSTR(NEL,8),EPSDOT(6,*),VDEF(MVSIZ,8),DT1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
C      my_real
C     .     DT1V4, TMP1, TMP2, TMP3, 
C     .     DT1V4B,TMP11,TMP22,VX10,VX20,VX30
C-----------------------------------------------
C
      DO I=JFT,JLT
       VDEF(I,1)=PX2(I)*VLX(I,1)+PX3(I)*VLX(I,2)
       VDEF(I,2)=PY2(I)*VLY(I,1)+PY3(I)*VLY(I,2)
C
       VDEF(I,3)=PX2(I)*VLY(I,1)+PX3(I)*VLY(I,2)+
     .        PY2(I)*VLX(I,1)+PY3(I)*VLX(I,2)
       EXZ(I)=ZERO
       EYZ(I)=ZERO
      ENDDO
C
      IF(IEPSDOT/=0)THEN
       DO I=JFT,JLT
        J = I + NFT
        EPSDOT(1,J)=VDEF(I,1)
        EPSDOT(2,J)=VDEF(I,2)
        EPSDOT(3,J)=VDEF(I,3)
       ENDDO
      ENDIF
C
      DO I=JFT,JLT
       EXX(I)=VDEF(I,1)*DT1
       EYY(I)=VDEF(I,2)*DT1
       EXY(I)=VDEF(I,3)*DT1
      ENDDO
C
      IF(ISTRAIN/=0)THEN
       DO I=JFT,JLT
        GSTR(I,1)=GSTR(I,1)+EXX(I)
        GSTR(I,2)=GSTR(I,2)+EYY(I)
        GSTR(I,3)=GSTR(I,3)+EXY(I)
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  CDKCURV3                      source/elements/sh3n/coquedk/cdkdefo3.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKCURV3(JFT,JLT,BZ1,BZ2,BZ3,BRX1,BRX2,BRX3,BRY1,
     1                   BRY2,BRY3,VLZ,RLX,RLY,KXX, KYY, KXY)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .    BZ1(MVSIZ,2),BZ2(MVSIZ,2),BZ3(MVSIZ,2), BRX1(MVSIZ,3),
     .    BRX2(MVSIZ,3),BRX3(MVSIZ,3),BRY1(MVSIZ,3),BRY2(MVSIZ,3),BRY3(MVSIZ,3),
     .    VLZ(MVSIZ,2),RLX(MVSIZ,3),RLY(MVSIZ,3),KXX(*),KYY(*),KXY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C
      DO I=JFT,JLT
       KXX(I)=BZ1(I,1)*VLZ(I,1)+BZ1(I,2)*VLZ(I,2)+
     .        BRX1(I,1)*RLX(I,1)+BRX1(I,2)*RLX(I,2)+BRX1(I,3)*RLX(I,3)+
     .        BRY1(I,1)*RLY(I,1)+BRY1(I,2)*RLY(I,2)+BRY1(I,3)*RLY(I,3)
C
       KYY(I)=BZ2(I,1)*VLZ(I,1)+BZ2(I,2)*VLZ(I,2)+
     .        BRX2(I,1)*RLX(I,1)+BRX2(I,2)*RLX(I,2)+BRX2(I,3)*RLX(I,3)+
     .        BRY2(I,1)*RLY(I,1)+BRY2(I,2)*RLY(I,2)+BRY2(I,3)*RLY(I,3)
C
       KXY(I)=BZ3(I,1)*VLZ(I,1)+BZ3(I,2)*VLZ(I,2)+
     .        BRX3(I,1)*RLX(I,1)+BRX3(I,2)*RLX(I,2)+BRX3(I,3)*RLX(I,3)+
     .        BRY3(I,1)*RLY(I,1)+BRY3(I,2)*RLY(I,2)+BRY3(I,3)*RLY(I,3)
      ENDDO
C
      RETURN
      END
