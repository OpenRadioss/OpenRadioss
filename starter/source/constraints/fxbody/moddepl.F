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
Chd|  MODDEPL                       source/constraints/fxbody/moddepl.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MODDEPL(IBUF, MOD, MDEPL, IFILE, IRCM,
     .                   NSNI, NSN, AMOD )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), IFILE, IRCM, NSNI, NSN, AMOD
      my_real
     .        MOD(*), MDEPL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IAD, II
      my_real
     .        VV(3,NSN), VT(6)
C
      IF (IFILE==0) THEN
         IAD=0
         DO I=1,NSN
            VV(1,I)=MOD(IAD+1)
            VV(2,I)=MOD(IAD+2)
            VV(3,I)=MOD(IAD+3)
            IAD=IAD+6
         ENDDO
         AMOD=AMOD+6*NSN
      ELSEIF (IFILE==1) THEN
         IAD=0
         DO I=1,NSNI
            VV(1,I)=MOD(IAD+1)
            VV(2,I)=MOD(IAD+2)
            VV(3,I)=MOD(IAD+3)
            IAD=IAD+6
         ENDDO
         AMOD=AMOD+6*NSNI
         DO I=NSNI+1,NSN
            IRCM=IRCM+1
            READ(IFXM,REC=IRCM) (VT(II),II=1,6)
            VV(1,I)=VT(1)
            VV(2,I)=VT(2)
            VV(3,I)=VT(3)
         ENDDO   
      ENDIF
C
      DO I=1,NSN
         II=IBUF(I)
         MDEPL(1,II)=VV(1,I)
         MDEPL(2,II)=VV(2,I)
         MDEPL(3,II)=VV(3,I)
      ENDDO         
C
      RETURN
      END
      
