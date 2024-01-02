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
Chd|  DT5LAW                        source/materials/time_step/dt5law.F
Chd|-- called by -----------
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|-- calls ---------------
Chd|        DTEL                          source/materials/time_step/dtel.F
Chd|        DTSPH                         source/materials/time_step/dtsph.F
Chd|====================================================================
      SUBROUTINE DT5LAW(PM,GEO,PID,MAT, DELTAX, AIRE, VOL, DTX)
C
C     CALCUL DES DT ELEMENTAIRES POUR LA LOI 5
C
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*), DELTAX(*), AIRE(*), VOL(*), DTX(*)
      INTEGER PID(*),MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SSP(MVSIZ), RHO0(MVSIZ), VIS(MVSIZ)
C-----------------------------------------------
C=======================================================================
C
C   SSP(I) EST ICI EGAL QUND T=0 A LA VDET STOCKEE DANS PM
C
      DO I=LFT,LLT
         SSP(I)=PM(38,MAT(I))
         RHO0(I) =PM( 1,MAT(I))
         VIS(I) = ZERO
      END DO

      IF(JSPH==0)THEN
       CALL DTEL(SSP,PM,GEO,PID,MAT, RHO0, VIS, DELTAX, AIRE, VOL, DTX)
      ELSE
       CALL DTSPH(SSP,PM,GEO,PID,MAT, RHO0, VIS, DELTAX, VOL, DTX)
      ENDIF
C-----------
      RETURN
      END
