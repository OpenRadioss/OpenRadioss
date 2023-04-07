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
Chd|  R4CUM3                        source/elements/spring/r4cum3.F
Chd|-- called by -----------
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R4CUM3(
     1   F,       FORX,    FORY,    FORZ,
     2   XM,      XMOM,    YMOM,    ZMOM,
     3   STI,     STIR,    STIFN,   STIFR,
     4   FX1,     FX2,     FY1,     FY2,
     5   FZ1,     FZ2,     MX1,     MX2,
     6   MY1,     MY2,     MZ1,     MZ2,
     7   AL,      EXX,     EYX,     EZX,
     8   EXY,     EYY,     EZY,     EXZ,
     9   EYZ,     EZZ,     NC1,     NC2,
     A   NEL)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC1(*),NC2(*)
C     REAL
      my_real
     .   F(3,*), FORX(*), FORY(*), FORZ(*), XM(3,*), XMOM(*),
     .   YMOM(*), ZMOM(*), STI(3,*),STIR(3,*),STIFN(*),STIFR(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ),
     .   EXY(MVSIZ), EYY(MVSIZ), EZY(MVSIZ),
     .   EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ),AL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   F11(MVSIZ), F21(MVSIZ), F31(MVSIZ), 
     .   M11(MVSIZ), M21(MVSIZ), M31(MVSIZ),
     .   M12(MVSIZ), M22(MVSIZ), M32(MVSIZ),
     .   YMOM1, YMOM2, ZMOM1, ZMOM2, YMOM1P, ZMOM1P,
     .   YMOM2P,ZMOM2P
C-----------------------------------------------
      DO I=1,NEL
        F11(I)=EXX(I)*FORX(I)+EXY(I)*FORY(I)+EXZ(I)*FORZ(I)
        F21(I)=EYX(I)*FORX(I)+EYY(I)*FORY(I)+EYZ(I)*FORZ(I)
        F31(I)=EZX(I)*FORX(I)+EZY(I)*FORY(I)+EZZ(I)*FORZ(I)
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      DO I=1,NEL
        F(1,NC1(I))=F(1,NC1(I))+F11(I)
        F(2,NC1(I))=F(2,NC1(I))+F21(I)
        F(3,NC1(I))=F(3,NC1(I))+F31(I)
        FX1(I) = -F11(I)
        FY1(I) = -F21(I)
        FZ1(I) = -F31(I)
        STIFN(NC1(I))=STIFN(NC1(I))+STI(1,I)
C
        F(1,NC2(I))=F(1,NC2(I))-F11(I)
        F(2,NC2(I))=F(2,NC2(I))-F21(I)
        F(3,NC2(I))=F(3,NC2(I))-F31(I)
        FX2(I) = F11(I)
        FY2(I) = F21(I)
        FZ2(I) = F31(I)
        STIFN(NC2(I))=STIFN(NC2(I))+STI(2,I)
      ENDDO
C-------------------------
C     MOMENTS
C-------------------------
      DO I=1,NEL
        YMOM1 = YMOM(I) - HALF*AL(I)*FORZ(I)
        ZMOM1 = ZMOM(I) + HALF*AL(I)*FORY(I)
        M11(I)=EXX(I)*XMOM(I)+EXY(I)*YMOM1+EXZ(I)*ZMOM1
        M21(I)=EYX(I)*XMOM(I)+EYY(I)*YMOM1+EYZ(I)*ZMOM1
        M31(I)=EZX(I)*XMOM(I)+EZY(I)*YMOM1+EZZ(I)*ZMOM1
        YMOM2 = YMOM(I) + HALF*AL(I)*FORZ(I)
        ZMOM2 = ZMOM(I) - HALF*AL(I)*FORY(I)      
        M12(I)=EXX(I)*XMOM(I)+EXY(I)*YMOM2+EXZ(I)*ZMOM2
        M22(I)=EYX(I)*XMOM(I)+EYY(I)*YMOM2+EYZ(I)*ZMOM2
        M32(I)=EZX(I)*XMOM(I)+EZY(I)*YMOM2+EZZ(I)*ZMOM2
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      DO I=1,NEL
        XM(1,NC1(I))=XM(1,NC1(I))+M11(I)
        XM(2,NC1(I))=XM(2,NC1(I))+M21(I)
        XM(3,NC1(I))=XM(3,NC1(I))+M31(I)
        MX1(I) = -M11(I)
        MY1(I) = -M21(I)
        MZ1(I) = -M31(I)
        STIFR(NC1(I))=STIFR(NC1(I))+STIR(1,I)
C
        XM(1,NC2(I))=XM(1,NC2(I))-M12(I)
        XM(2,NC2(I))=XM(2,NC2(I))-M22(I)
        XM(3,NC2(I))=XM(3,NC2(I))-M32(I)
        MX2(I) = M12(I)
        MY2(I) = M22(I)
        MZ2(I) = M32(I)
        STIFR(NC2(I))=STIFR(NC2(I))+STIR(2,I)
      ENDDO
C---
      RETURN
      END
