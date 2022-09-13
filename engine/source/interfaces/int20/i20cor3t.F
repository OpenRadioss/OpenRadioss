Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I20COR3T                      source/interfaces/int20/i20cor3t.F
Chd|-- called by -----------
Chd|        I20STO                        source/interfaces/intsort/i20sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20COR3T(JLT     ,XA    ,IRECT ,NSV   ,CAND_E  ,
     1                    CAND_N  ,IGAP  ,GAP   ,X1    ,X2      ,
     2                    X3      ,X4    ,Y1    ,Y2    ,Y3      ,
     3                    Y4      ,Z1    ,Z2    ,Z3    ,Z4      ,
     4                    XI      ,YI    ,ZI    ,STIF  ,IX1     ,
     5                    IX2     ,IX3   ,IX4   ,NSN   ,GAP_S   ,
     6                    GAP_M   ,GAPV  ,GAPMAX,GAPMIN,CURV_MAX,
     7                    NIN     ,GAP_SH)

C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .    JLT,IDT, NOINT, NSN, IGAP, NIN
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real
     .   XA(3,*), GAPV(*), GAP_S(*), GAP_M(*),CURV_MAX(*),
     .   GAP, GAPMAX, GAPMIN,GAP_SH(*)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT
C-----------------------------------------------
C
      IF(IGAP==0)THEN
        DO I=1,JLT
          GAPV(I)=GAP
        END DO
      ELSE
        DO I=1,JLT
          J = CAND_N(I)
          IF(J<=NSN) THEN
            GAPV(I)=GAP_S(J)+GAP_M(CAND_E(I))
          ELSE
            IG = J-NSN
            GAPV(I)=XREM(12,IG)+GAP_M(CAND_E(I))
C           GAPV(I)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
          END IF
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=GAPV(I)+TWO*GAP_SH(CAND_E(I))
          GAPV(I)=MAX(GAPMIN,GAPV(I))
        END DO
      END IF
      DO I=1,JLT
        J = CAND_N(I)
        IF(J<=NSN) THEN
          IL = NSV(J)
          XI(I) = XA(1,IL)
          YI(I) = XA(2,IL)
          ZI(I) = XA(3,IL)
        ELSE
          IG = J-NSN
          XI(I) = XREM(1,IG)
          YI(I) = XREM(2,IG)
          ZI(I) = XREM(3,IG)
        ENDIF
C
        L  = CAND_E(I)
C
        IX1(I)=IRECT(1,L)
        X1(I)=XA(1,IX1(I))
        Y1(I)=XA(2,IX1(I))
        Z1(I)=XA(3,IX1(I))
C
        IX2(I)=IRECT(2,L)
        X2(I)=XA(1,IX2(I))
        Y2(I)=XA(2,IX2(I))
        Z2(I)=XA(3,IX2(I))
C
        IX3(I)=IRECT(3,L)
        X3(I)=XA(1,IX3(I))
        Y3(I)=XA(2,IX3(I))
        Z3(I)=XA(3,IX3(I))
C
        IX4(I)=IRECT(4,L)
        X4(I)=XA(1,IX4(I))
        Y4(I)=XA(2,IX4(I))
        Z4(I)=XA(3,IX4(I))
      ENDDO
C
      RETURN
      END

