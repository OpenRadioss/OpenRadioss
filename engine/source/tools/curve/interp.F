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
Chd|  INTERP                        source/tools/curve/interp.F   
Chd|-- called by -----------
Chd|        LASER2                        source/loads/laser/laser2.F   
Chd|        LASER3                        source/loads/laser/laser2.F   
Chd|        M18LAW                        source/materials/mat/mat018/m18law.F
Chd|        M18TH                         source/materials/mat/mat018/m18th.F
Chd|        REPLA3                        source/elements/spring/repla3.F
Chd|        RGWAL1                        source/ale/grid/rgwal1.F      
Chd|        TIMFUN                        source/tools/curve/timfun.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERP(TF,TT,NPOINT,F,TG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPOINT
      my_real TT, F, TG
      my_real TF(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M
      my_real DT1, DT2, D1, D2
C-----------------------------------------------
      IF (TT-TF(1,NPOINT)>ZERO) GO TO 50
      DO 20 I=2,NPOINT
      M=I
      IF (TT-TF(1,M)<=ZERO) GO TO 30
   20 CONTINUE
   50 F=ZERO
      TG=ZERO
      RETURN
   30 DT1=TT-TF(1,M-1)
      DT2=-TT+TF(1,M)
      D1=TF(1,M)-TF(1,M-1)
      D2=TF(2,M)-TF(2,M-1)
      TG=ZERO
      IF(D1/=ZERO)TG=D2/D1
      !TEST POUR PRECISION DU CALCUL
      IF(DT1<=DT2)THEN
       F =TF(2,M-1)+DT1*TG
      ELSE
       F =TF(2,M)-DT2*TG
      ENDIF
      RETURN
      END
