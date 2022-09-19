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
Chd|  CDKSTRA3                      source/elements/sh3n/coquedk/cdkstra3.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKSTRA3(JFT,JLT,GSTR,EXX, EYY, EXY,KXX, KYY, KXY,
     .                    EPSDOT,NFT,ISTRAIN,DT1,GSTRPG,NEL)
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
C  ds007  18/2/00  +1
      INTEGER JFT, JLT,NFT,ISTRAIN,NEL
C     REAL
      my_real
     .   GSTR(NEL,8), EXX(*), EYY(*), EXY(*),KXX(*), KYY(*), KXY(*),
     .   EPSDOT(6,*),DT1,GSTRPG(NEL,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   ONE_OVER_3
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      IF(IEPSDOT/=0)THEN
       DO I=JFT,JLT
        J = I + NFT
        EPSDOT(4,J)=KXX(I)
        EPSDOT(5,J)=KYY(I)
        EPSDOT(6,J)=KXY(I)
       ENDDO
      ENDIF
C
      DO I=JFT,JLT
       KXX(I)=KXX(I)*DT1
       KYY(I)=KYY(I)*DT1
       KXY(I)=KXY(I)*DT1
      ENDDO
C
      IF(ISTRAIN/=0)THEN
        DO I=JFT,JLT
         GSTR(I,6)=GSTR(I,6)+KXX(I)*THIRD
         GSTR(I,7)=GSTR(I,7)+KYY(I)*THIRD
         GSTR(I,8)=GSTR(I,8)+KXY(I)*THIRD	
C
         GSTRPG(I,1)=GSTRPG(I,1)+EXX(I)
         GSTRPG(I,2)=GSTRPG(I,2)+EYY(I)
         GSTRPG(I,3)=GSTRPG(I,3)+EXY(I)
         GSTRPG(I,6)=GSTRPG(I,6)+KXX(I)
         GSTRPG(I,7)=GSTRPG(I,7)+KYY(I)
         GSTRPG(I,8)=GSTRPG(I,8)+KXY(I)
        ENDDO
      ENDIF
C
      RETURN
      END
