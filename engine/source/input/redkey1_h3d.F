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
Chd|  REDKEY1_H3D                   source/input/redkey1_h3d.F    
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REDKEY1_H3D(KEY,KEY0,NKEY,CARTE,IKEY,IR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NKEY, IKEY, IR, IOK
      CHARACTER CARTE*ncharline
      CHARACTER KEY(*)*ncharkey, KEY0(NKEY)*5
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKEYO, J, JK, I
      CHARACTER KEYO(9)*ncharkey
C-----------------------------------------------
      IR=0
C
      IKEYO=IKEY
      DO 5 J=1,9
      KEYO(J)=KEY(J)
   5  KEY(J)=' '
      JK=0
C
      DO 10 I=1,ncharline
      IF(CARTE(I:I)=='/')THEN
       JK=JK+1
       J=0
      ELSEIF(CARTE(I:I)/=' ')THEN
       J=J+1
       IF(J<=ncharkey)KEY(JK)(J:J)=CARTE(I:I)
      ENDIF
 10   CONTINUE
C
      DO 100 I=1,NKEY
      IKEY=I
 100  IF(KEY(1)(1:5)==KEY0(IKEY)(1:5)) GOTO 400
C
      IR=1
      IKEY=IKEYO
      DO 200 J=1,9
 200  KEY(J)=KEYO(J)
      RETURN
C
 400  CONTINUE
      IOK = 0
      J=0
C
      RETURN
      END
