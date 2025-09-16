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
!||    mvoludt   ../engine/source/airbag/mvoludt.F
!||--- called by ------------------------------------------------------
!||    resol     ../engine/source/engine/resol.F
!||====================================================================
      SUBROUTINE MVOLUDT(MONVOL,VOLMON)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*)
      my_real
     .        VOLMON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITYP, K1, KK1
C     REAL
      my_real
     .   DTX, DTY, GAMA, P, V, S, ROT, AL
C-----------------------------------------------
C
      DTX=ONE
      DTY=ONE
      K1 = 1
      KK1 = 0
      DO I=1,NVOLU
       ITYP=MONVOL(K1+1)
C communication in resol just before mvoludt and sending to proc0 of volmon
C avoid difference in parallel when processor does not handle all airbags
C see formulas on min dty / max dtx
C separate branch for the main proc (proc having updated p,v,s)
       IF(ITYP/=1)THEN
        GAMA=VOLMON(KK1+1)
        AL=VOLMON(KK1+2)
        P=VOLMON(KK1+12)
        V=VOLMON(KK1+16)-VOLMON(KK1+5)
        S=VOLMON(KK1+18)
        ROT=VOLMON(KK1+21)
        DTY=MIN(SQRT(ONE+AL)-AL,DTY)
        IF(ROT==ZERO) THEN
          DTX=DTY
        ELSE
          DTX=MAX(SQRT(ONE+DT2**2*GAMA*P*S/V/ROT),DTX)
        ENDIF
       ENDIF
       K1  = K1  + NIMV
       KK1 = KK1 + NRVOLU
      ENDDO
C
      DT2=DT2*DTY/DTX
      RETURN
      END
