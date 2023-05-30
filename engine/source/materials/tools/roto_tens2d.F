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
Chd|  ROTO_TENS2D                          
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTO_TENS2D(NEL,SIG,DIR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL
      my_real ,DIMENSION(NEL,2) ,INTENT(IN)    :: DIR
      my_real ,DIMENSION(NEL,3) ,INTENT(INOUT) :: SIG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      my_real :: D11,D22,D12,S1,S2,S3
C-----------------------------------------------
      DO I = 1,NEL
        D11 = DIR(I,1)**2
        D22 = DIR(I,2)**2
        D12 = DIR(I,1)*DIR(I,2)
        S1  = D11*SIG(I,1) + D22*SIG(I,2) + TWO*D12 * SIG(I,3)
        S2  = D22*SIG(I,1) + D11*SIG(I,2) - TWO*D12 * SIG(I,3)
        S3  = D12*(SIG(I,2)-SIG(I,1)) + (D11-D22) * SIG(I,3)
c        S4 =-DIR(I,2)*SIG(I,5)+DIR(I,1)*SIG(I,4)
c        S5 = DIR(I,1)*SIG(I,5)+DIR(I,2)*SIG(I,4)
        SIG(I,1)=S1
        SIG(I,2)=S2
        SIG(I,3)=S3
c        SIG(I,4)=S4
c        SIG(I,5)=S5
      ENDDO
c-----------
      RETURN
      END
