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
!||    dt1lawp   ../starter/source/elements/beam/dt1lawp.F
!||--- called by ------------------------------------------------------
!||    pinit3    ../starter/source/elements/beam/pinit3.F
!||====================================================================
      SUBROUTINE DT1LAWP(PM,GEO,MAT,MXG,DELTAX,DTX,IGTYP)
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
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), MXG(MVSIZ),IGTYP
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), DELTAX(MVSIZ), DTX(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, IG,INEW
C     REAL
      my_real
     .   RHO0, G, C1, SSP, C,
     .   IYY, IZZ, SL2I, SHF, DMP, 
     .   DSH, PHI, F1, F2, FAC, YOUNG,CST,FSH
C=======================================================================
        DO I=LFT,LLT
         MX   = MAT(I)
         RHO0 = PM( 1,MX)
         G    = PM(22,MX)
         C1   = PM(32,MX)
         SSP  = PM(27,MX)
         IG   = MXG(I)
         IYY  = GEO(2,IG)
         IZZ  = GEO(18,IG)
C
         SL2I = GEO(1,IG)*DELTAX(I)**2/ MAX(EM20,IZZ,IYY)
         CST = SIX_OVER_5*PM(20,MX)/G
         FSH = DELTAX(I)/(ONE_OVER_12*SL2I+CST)
         FSH = MAX(ONE,FSH)
C
         DMP =MAX(GEO(16,IG),GEO(17,IG))
         DMP =DMP*SQRT(TWO)
         FAC =SQRT(ONE+DMP*DMP)-DMP
C
         DTX(I) = FAC * DELTAX(I) / MAX(EM20,SSP)
         IF (IGTYP /= 18) DTX(I) = DTX(I)/SQRT(FSH)
        ENDDO
C-----------
      RETURN
      END
