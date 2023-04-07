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
Chd|  R2CUM3                        source/elements/spring/r2cum3.F
Chd|-- called by -----------
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R2CUM3(
     1   F,       FORX,    FORY,    FORZ,
     2   XM,      XMOM,    YMOM,    ZMOM,
     3   STI,     STIR,    STIFN,   STIFR,
     4   FX1,     FX2,     FY1,     FY2,
     5   FZ1,     FZ2,     MX1,     MX2,
     6   MY1,     MY2,     MZ1,     MZ2,
     7   GEO,     X1,      Y1,      Z1,
     8   X2,      Y2,      Z2,      IEQUIL,
     9   EXX,     EYX,     EZX,     EXY,
     A   EYY,     EZY,     EXZ,     EYZ,
     B   EZZ,     NC1,     NC2,     NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC1(*),NC2(*),IEQUIL(*)
C     REAL
      my_real
     .   F(3,*),FORX(*),FORY(*),FORZ(*),XM(3,*),XMOM(*),YMOM(*),
     .   ZMOM(*),STI(3,*),STIR(3,*),STIFN(*),STIFR(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .   GEO(NPROPG,*),X1(*),X2(*),Y1(*),Y2(*),
     .   Z1(*),Z2(*),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ), EXY(MVSIZ),
     .   EYY(MVSIZ), EZY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   MMX, MMY, MMZ, XX
C-----------------------------------------------
C-------------------------
C     FORCES
C-------------------------
      DO I=1,NEL
        FX2(I)=EXX(I)*FORX(I)+EXY(I)*FORY(I)+EXZ(I)*FORZ(I)
        FY2(I)=EYX(I)*FORX(I)+EYY(I)*FORY(I)+EYZ(I)*FORZ(I)
        FZ2(I)=EZX(I)*FORX(I)+EZY(I)*FORY(I)+EZZ(I)*FORZ(I)
        FX1(I) = -FX2(I)
        FY1(I) = -FY2(I)
        FZ1(I) = -FZ2(I)
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      DO I=1,NEL
        F(1,NC1(I))=F(1,NC1(I))-FX1(I)
        F(2,NC1(I))=F(2,NC1(I))-FY1(I)
        F(3,NC1(I))=F(3,NC1(I))-FZ1(I)
        STIFN(NC1(I))=STIFN(NC1(I))+STI(1,I)
        F(1,NC2(I))=F(1,NC2(I))-FX2(I)
        F(2,NC2(I))=F(2,NC2(I))-FY2(I)
        F(3,NC2(I))=F(3,NC2(I))-FZ2(I)
        STIFN(NC2(I))=STIFN(NC2(I))+STI(2,I)
      ENDDO
C-------------------------
C     MOMENTS
C-------------------------
      DO I=1,NEL
        MX2(I)=EXX(I)*XMOM(I)+EXY(I)*YMOM(I)+EXZ(I)*ZMOM(I)
        MY2(I)=EYX(I)*XMOM(I)+EYY(I)*YMOM(I)+EYZ(I)*ZMOM(I)
        MZ2(I)=EZX(I)*XMOM(I)+EZY(I)*YMOM(I)+EZZ(I)*ZMOM(I)
        MX1(I) = -MX2(I)
        MY1(I) = -MY2(I)
        MZ1(I) = -MZ2(I)
      ENDDO
C
      DO I=1,NEL
        IF (IEQUIL(I) == 1) THEN
          MMX = HALF*((Y2(I)-Y1(I))*FZ2(I) - (Z2(I)-Z1(I))*FY2(I))
          MMY = HALF*((Z2(I)-Z1(I))*FX2(I) - (X2(I)-X1(I))*FZ2(I))
          MMZ = HALF*((X2(I)-X1(I))*FY2(I) - (Y2(I)-Y1(I))*FX2(I)) 
          MX1(I) = MX1(I) - MMX
          MY1(I) = MY1(I) - MMY
          MZ1(I) = MZ1(I) - MMZ
          MX2(I) = MX2(I) - MMX
          MY2(I) = MY2(I) - MMY
          MZ2(I) = MZ2(I) - MMZ
          XX = (X2(I)-X1(I))*(X2(I)-X1(I)) 
     .       + (Y2(I)-Y1(I))*(Y2(I)-Y1(I))
     .       + (Z2(I)-Z1(I))*(Z2(I)-Z1(I))
          STIR(1,I) = STIR(1,I) + STI(2,I)*XX
          STIR(2,I) = STIR(2,I) + STI(1,I)*XX
        ENDIF
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      DO I=1,NEL
        XM(1,NC1(I))=XM(1,NC1(I))-MX1(I)
        XM(2,NC1(I))=XM(2,NC1(I))-MY1(I)
        XM(3,NC1(I))=XM(3,NC1(I))-MZ1(I)
        STIFR(NC1(I))=STIFR(NC1(I))+STIR(1,I)
        XM(1,NC2(I))=XM(1,NC2(I))-MX2(I)
        XM(2,NC2(I))=XM(2,NC2(I))-MY2(I)
        XM(3,NC2(I))=XM(3,NC2(I))-MZ2(I)
        STIFR(NC2(I))=STIFR(NC2(I))+STIR(2,I)
      ENDDO
C---
      RETURN
      END
