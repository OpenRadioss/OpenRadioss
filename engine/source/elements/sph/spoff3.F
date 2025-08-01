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
!||    spoff3    ../engine/source/elements/sph/spoff3.F
!||--- called by ------------------------------------------------------
!||    spstres   ../engine/source/elements/sph/spstres.F
!||====================================================================
      SUBROUTINE SPOFF3(
     1   KXSP,    VD2,     VIS,     OFFG,
     2   OFF,     NC1,     NGL,     MXT,
     3   NGEO,    IPART,   IPARTSP, NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .  VIS(*),VD2(*),OFFG(*),OFF(*)
      INTEGER NC1(*), MXT(*), NGL(*), NGEO(*), KXSP(NISP,*),
     .        IPART(LIPART1,*),IPARTSP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IPRT
C-----------------------------------------------
      DO I=1,NEL
       VIS(I)=ZERO
       IPRT   =IPARTSP(I)
       NGEO(I)=IPART(2,IPRT)
       NGL(I) =KXSP(NISP,I)
       MXT(I) =IPART(1,IPRT)
       NC1(I) =KXSP(3,I)
      ENDDO
      DO I=1,NEL
       OFF(I) = MIN(ONE,ABS(OFFG(I)))
      ENDDO
      DO I=1,NEL
       VD2(I)=ZERO
      ENDDO
C-----------------------------------------------
      RETURN
      END
