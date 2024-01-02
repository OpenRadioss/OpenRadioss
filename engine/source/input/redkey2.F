Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  REDKEY2                       source/input/redkey2.F        
Chd|-- called by -----------
Chd|        MANCTR                        source/input/manctr.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REDKEY2(KEY2,LK2,KEY0,NKEY,CARTE,IKEY)
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
      INTEGER LK2, NKEY, IKEY
      CHARACTER KEY2*80, CARTE*ncharline 
      CHARACTER KEY0(NKEY)*5
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JK, I, J
      CHARACTER KEY1*80
C-----------------------------------------------
      KEY1=' '
      KEY2=' '
      JK=0
      DO 10 I=1,ncharline 
      IF(CARTE(I:I)=='/')THEN
       JK=JK+1
       J=0
      ELSEIF(CARTE(I:I)/=' '.AND.JK==1)THEN
       J=J+1
       KEY1(J:J)=CARTE(I:I)
      ELSEIF(CARTE(I:I)/=' '.AND.JK==2)THEN
       J=J+1
       LK2=J
       KEY2(J:J)=CARTE(I:I)
      ENDIF
 10   CONTINUE
C
      DO 100 IKEY=1,NKEY
 100  IF(KEY1==KEY0(IKEY))RETURN
      IKEY=0
C
      RETURN
      END
