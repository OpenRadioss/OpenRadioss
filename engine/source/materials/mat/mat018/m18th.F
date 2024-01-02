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
Chd|  M18TH                         source/materials/mat/mat018/m18th.F
Chd|-- called by -----------
Chd|        ATHERM                        source/ale/atherm.F           
Chd|-- calls ---------------
Chd|        INTERP                        source/tools/curve/interp.F   
Chd|====================================================================
      SUBROUTINE M18TH(
     1   T,       XK,      MAT,     PM,
     2   IPM,     TF,      NPF,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NPF(*), MAT(*),  IPM(NPROPMI,*)
C     REAL
      my_real
     .   T(*), XK(*), TF(*),PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IFUNC, NPOINT, MX
C     REAL
      my_real
     .   BID,TSCAL,KSCAL
C-----------------------------------------------
C=======================================================================
      MX = MAT(1)
      IFUNC = IPM(13,MX)
      KSCAL = PM(44,MX) 
      IF(IFUNC/=0)THEN
        DO 10 I=1,NEL
        NPOINT=(NPF(IFUNC+1)-NPF(IFUNC))/2
        TSCAL = T(I) / PM(42,MX)              
        CALL INTERP(TF(NPF(IFUNC)),TSCAL,NPOINT,XK(I),BID)
        XK(I) = XK(I) * KSCAL
   10 CONTINUE 
      ENDIF

      RETURN
      END
