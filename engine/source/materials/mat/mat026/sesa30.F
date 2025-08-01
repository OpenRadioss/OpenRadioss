Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    sesa30   ../engine/source/materials/mat/mat026/sesa30.F
!||--- called by ------------------------------------------------------
!||    mmain    ../engine/source/materials/mat_share/mmain.F90
!||====================================================================
      SUBROUTINE SESA30(SIG  ,EINT ,QOLD ,VOL ,QNEW,
     2                  VNEW ,DV2  ,NEL, P01  )
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
      INTEGER NEL
      my_real
     .   SIG(NEL,6), EINT(*), QOLD(*), VOL(*) ,QNEW(*), VNEW(*), DV2(*), P01(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   P(MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
       P(I)=P01(I)
       EINT(I)=MAX(ZERO,EINT(I)-DV2(I)*(P(I)+QNEW(I)))
       EINT(I)=EINT(I)/MAX(EM30,VOL(I))
      ENDDO
C
      DO I=1,NEL
       SIG(I,1)=SIG(I,1)-P(I)
       SIG(I,2)=SIG(I,2)-P(I)
       SIG(I,3)=SIG(I,3)-P(I)
      ENDDO
C
      RETURN
      END
