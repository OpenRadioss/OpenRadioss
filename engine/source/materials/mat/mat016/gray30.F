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
Chd|  GRAY30                        source/materials/mat/mat016/gray30.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GRAY30(SIG  ,EINT ,QOLD ,VOL ,QNEW,
     2                  VNEW ,DVOL ,NEL  ,P01 ,P02, 
     3                  E01  ,E02  ,PNEW)
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
      INTEGER NEL
      my_real
     .   SIG(NEL,6), EINT(*), QOLD(*), VOL(*), QNEW(*), VNEW(*), DVOL(*),
     .   P01(*), P02(*), E01(*), E02(*), PNEW(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .    PE21(MVSIZ),
     .    E21, P21, DEDE, DQ, EI3
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      DO I=LFT,LLT
        E21       = E02(I)-E01(I)
        P21       = P02(I)-P01(I)
        IF(E21.EQ.ZERO)THEN
          PE21(I) = ZERO
        ELSE
          PE21(I) = P21/E21
        ENDIF
      ENDDO !next I

      DO I=LFT,LLT
        DEDE      = DVOL(I)*PE21(I)*HALF
        DQ        = DVOL(I)*(QNEW(I)-QOLD(I))*HALF
        QOLD(I)   = QNEW(I)
        EI3       =(E02(I)-DQ+E01(I)*DEDE)/(1.+DEDE)
        EI3       = MAX(ZERO,EI3)
        PNEW(I)   = P01(I)+(EI3-E01(I))*PE21(I)
        EINT(I)   = EINT(I)-DVOL(I)*(PNEW(I)+QNEW(I))*HALF
        EINT(I)   = EINT(I)/VOL(I)
      ENDDO !next I

      DO I=LFT,LLT
        SIG(I,1)  = SIG(I,1)-PNEW(I)
        SIG(I,2)  = SIG(I,2)-PNEW(I)
        SIG(I,3)  = SIG(I,3)-PNEW(I)
      ENDDO !next I

      RETURN
      END
