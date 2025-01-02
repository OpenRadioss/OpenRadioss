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
C  _clean      SUBROUTINE INORI2(X,IRECT,ISG,NEL,NINT,NOINT)
      !||====================================================================
      !||    inori2   ../starter/source/interfaces/inter2d1/inori2.F
      !||--- called by ------------------------------------------------------
      !||    i1chk2   ../starter/source/interfaces/inter2d1/i1chk2.F
      !||    i3sti2   ../starter/source/interfaces/inter2d1/i3sti2.F
      !||    i9sti2   ../starter/source/interfaces/int09/i9sti2.F
      !||====================================================================
      SUBROUTINE INORI2(IRECT, ISG  , NEL, NINT,
     .                  NOINT, YM1  , YM2, ZM1 , ZM2 ,
     .                  YE   , ZE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C INIT INTERFACE : CHECK ORIENTATION
C     reverse segment connectivity if needed
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
      INTEGER ISG, NEL, NINT, NOINT
      INTEGER IRECT(4,*)
      my_real YM1, YM2, ZM1, ZM2,YE(4) ,ZE(4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IR
      my_real N2, N3, YM, ZM, DDS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      IF(NEL==0) RETURN ! NO ELEM CONNECTED TO CURRENT SEGMENT
      !segment vector p1->p2
      N2= ZM2-ZM1
      N3= YM1-YM2
      !elem centroid
      YM= FOURTH*(YE(1)+YE(2)+YE(3)+YE(4))
      ZM= FOURTH*(ZE(1)+ZE(2)+ZE(3)+ZE(4))
      !dot product
      DDS=N2*(YM-YM1)+ N3*(ZM-ZM1)
      IF(DDS >= ZERO)THEN
        !switch the segment connectivity to get outward normal
        IR=IRECT(1,ISG)
        IRECT(1,ISG)=IRECT(2,ISG)
        IRECT(2,ISG)=IR
        IF(NINT>0) WRITE (IOUT,1100) ISG, NOINT
        IF(NINT<0) WRITE (IOUT,1200) ISG, NOINT
      ENDIF
      RETURN
C-----------------------------------------------
 1100 FORMAT(' NODE CONNECTIVITY OF SECONDARY SEGMENT',I5,' OF INTERFACE',I10,' REVERSED')
 1200 FORMAT(' NODE CONNECTIVITY OF MAIN SEGMENT',I5,' OF INTERFACE',I10,' REVERSED')
      END
