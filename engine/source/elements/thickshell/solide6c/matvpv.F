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
Chd|  MATVPV                        source/elements/thickshell/solide6c/matvpv.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MATVPV(JFT   ,JLT    ,P  ,VL ,ND )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ND
      my_real
     .   P(ND,ND,*),VL(ND,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .   V(ND)
C-----------------------------------------
        DO EP=JFT,JLT
         DO I=1,ND
          V(I)=ZERO
          DO L=1,ND
           V(I)=V(I)+P(I,L,EP)*VL(L,EP)
          ENDDO
         ENDDO
         DO I=1,ND
          VL(I,EP)=V(I)
         ENDDO
        ENDDO
      RETURN
      END
