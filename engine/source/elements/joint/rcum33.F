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
Chd|  RCUM33                        source/elements/joint/rcum33.F
Chd|-- called by -----------
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RCUM33(JFT   ,JLT    ,XL    ,NC1  ,NC2    ,
     .                  F     ,FORX   ,FORY  ,FORZ ,XM     ,
     .                  XMOM  ,YMOM   ,ZMOM  ,STI  ,STIR   ,
     .                  STIFN ,STIFR  ,FX1   ,FX2  ,FY1    ,
     .                  FY2   ,FZ1    ,FZ2   ,MX1  ,MX2    ,
     .                  MY1   ,MY2    ,MZ1   ,MZ2  ,NUVAR  ,
     .                  UVAR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NUVAR,NC1(*),NC2(*)
C     REAL
      my_real
     .   F(3,*), FORX(*), FORY(*), FORZ(*), XM(3,*), XMOM(*),
     .   YMOM(*), ZMOM(*), STI(3,*),STIR(3,*),STIFN(*),STIFR(*),
     .   FX1(*) ,FX2(*)  ,FY1(*),  FY2(*) ,FZ1(*)  ,
     .   FZ2(*), MX1(*) ,MX2(*)  ,MY1(*),  MY2(*) ,MZ1(*)  ,
     .   MZ2(*),UVAR(NUVAR,*)
      DOUBLE PRECISION XL(MVSIZ,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .     F11(MVSIZ), F21(MVSIZ), F31(MVSIZ), 
     .     M11(MVSIZ), M21(MVSIZ), M31(MVSIZ),
     .     M12(MVSIZ), M22(MVSIZ), M32(MVSIZ),
     .     XMOM1, XMOM2, YMOM1, YMOM2, ZMOM1, ZMOM2, M1,M2,M3,
     .     EX(9), XX
C=======================================================================
C-------------------------
C     FORCES
C-------------------------
      DO 20 I=JFT,JLT
        DO J=1,9
            EX(J) = UVAR(21+J,I)
        END DO
        F11(I)=EX(1)*FORX(I)+EX(4)*FORY(I)+EX(7)*FORZ(I)
        F21(I)=EX(2)*FORX(I)+EX(5)*FORY(I)+EX(8)*FORZ(I)
   20   F31(I)=EX(3)*FORX(I)+EX(6)*FORY(I)+EX(9)*FORZ(I)
   
C----  Assemble
      DO 100 I=JFT,JLT
        F(1,NC1(I))=F(1,NC1(I))+F11(I)
        F(2,NC1(I))=F(2,NC1(I))+F21(I)
        F(3,NC1(I))=F(3,NC1(I))+F31(I)
        FX1(I) = -F11(I)
        FY1(I) = -F21(I)
        FZ1(I) = -F31(I)
        STIFN(NC1(I))=STIFN(NC1(I))+TWO*STI(1,I)
        F(1,NC2(I))=F(1,NC2(I))-F11(I)
        F(2,NC2(I))=F(2,NC2(I))-F21(I)
        F(3,NC2(I))=F(3,NC2(I))-F31(I)
        FX2(I) = F11(I)
        FY2(I) = F21(I)
        FZ2(I) = F31(I)
        STIFN(NC2(I))=STIFN(NC2(I))+TWO*STI(2,I)
  100 CONTINUE
C
C-------------------------
C     MOMENTS
C-------------------------
C
      DO 120 I=JFT,JLT
C      
      DO J=1,9
          EX(J) = UVAR(21+J,I)
      END DO
      M1 = HALF*(XL(I,2)*FORZ(I)-XL(I,3)*FORY(I))
      M2 = HALF*(XL(I,3)*FORX(I)-XL(I,1)*FORZ(I))
      M3 = HALF*(XL(I,1)*FORY(I)-XL(I,2)*FORX(I))
      XMOM1 = XMOM(I) + M1
      YMOM1 = YMOM(I) + M2
      ZMOM1 = ZMOM(I) + M3
      M11(I)=EX(1)*XMOM1+EX(4)*YMOM1+EX(7)*ZMOM1
      M21(I)=EX(2)*XMOM1+EX(5)*YMOM1+EX(8)*ZMOM1
      M31(I)=EX(3)*XMOM1+EX(6)*YMOM1+EX(9)*ZMOM1
C
      XMOM2 = XMOM(I) - M1
      YMOM2 = YMOM(I) - M2
      ZMOM2 = ZMOM(I) - M3
      M12(I)=EX(1)*XMOM2+EX(4)*YMOM2+EX(7)*ZMOM2
      M22(I)=EX(2)*XMOM2+EX(5)*YMOM2+EX(8)*ZMOM2
      M32(I)=EX(3)*XMOM2+EX(6)*YMOM2+EX(9)*ZMOM2
C
      XX = XL(I,1)*XL(I,1)
     .   + XL(I,2)*XL(I,2)
     .   + XL(I,3)*XL(I,3)
      STIR(1,I) = STIR(1,I) + STI(2,I)*XX
      STIR(2,I) = STIR(2,I) + STI(1,I)*XX
 120  CONTINUE
 
C----   Assemble
      DO 140 I=JFT,JLT
        XM(1,NC1(I))=XM(1,NC1(I))+M11(I)
        XM(2,NC1(I))=XM(2,NC1(I))+M21(I)
        XM(3,NC1(I))=XM(3,NC1(I))+M31(I)
        MX1(I) = -M11(I)
        MY1(I) = -M21(I)
        MZ1(I) = -M31(I)
        STIFR(NC1(I))=STIFR(NC1(I))+STIR(1,I)
        XM(1,NC2(I))=XM(1,NC2(I))-M12(I)
        XM(2,NC2(I))=XM(2,NC2(I))-M22(I)
        XM(3,NC2(I))=XM(3,NC2(I))-M32(I)
        MX2(I) = M12(I)
        MY2(I) = M22(I)
        MZ2(I) = M32(I)
        STIFR(NC2(I))=STIFR(NC2(I))+STIR(2,I)
  140 CONTINUE
C-----------------------------------------------
      RETURN
      END
