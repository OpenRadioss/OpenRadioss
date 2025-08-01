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
!||    thermc        ../engine/source/materials/mat_share/thermc.F
!||--- called by ------------------------------------------------------
!||    cforc3        ../engine/source/elements/shell/coque/cforc3.F
!||    cforc3_crk    ../engine/source/elements/xfem/cforc3_crk.F
!||    czforc3       ../engine/source/elements/shell/coquez/czforc3.F
!||    czforc3_crk   ../engine/source/elements/xfem/czforc3_crk.F
!||====================================================================
      SUBROUTINE THERMC(JFT  ,JLT   ,PM   ,MAT   ,THK   ,IXC , 
     2                  PX1  ,PX2   ,PY1  ,PY2   ,AREA  ,DT1C ,
     3                  TEMPNC ,TEMPEL   ,DIE  ,FPHI    ,THEACCFACT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,MAT(*),IXC(NIXC,*)
      my_real ,INTENT(IN) :: THEACCFACT
      my_real
     .   AREA(*), PX1(*), PX2(*), PY1(*), PY2(*),
     .   TEMPNC(*), FPHI(MVSIZ,4), PM(NPROPM,*),DIE(*),
     .   DT1C(*), THK(*),TEMPEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,MX
      my_real :: CA,CB ,KC ,PHIX,PHIY,A
!=======================================================================
        MX  =MAT(JFT)
        CA = PM(75,MX)
        CB = PM(76,MX)
!
        DO I=JFT,JLT
C 
C - flux
C      
          KC = (CA + CB*TEMPEL(I))*DT1C(I)/MAX(EM20,AREA(I))*THEACCFACT
          PHIX =  TEMPNC(IXC(2,I))*PX1(I) + TEMPNC(IXC(3,I))*PX2(I)  -
     .            TEMPNC(IXC(4,I))*PX1(I) - TEMPNC(IXC(5,I))*PX2(I)
          
          PHIY =  TEMPNC(IXC(2,I))*PY1(I) + TEMPNC(IXC(3,I))*PY2(I)  -
     .            TEMPNC(IXC(4,I))*PY1(I) - TEMPNC(IXC(5,I))*PY2(I)
C     
          PHIX = KC*PHIX*THK(I)
          PHIY = KC*PHIY*THK(I)
C
C         nodal thermal force
C
          A = FOURTH * DIE(I)
          FPHI(I,1) = A - (PHIX*PX1(I) + PHIY*PY1(I))
          FPHI(I,2) = A - (PHIX*PX2(I) + PHIY*PY2(I))
          FPHI(I,3) = A + (PHIX*PX1(I) + PHIY*PY1(I))
          FPHI(I,4) = A + (PHIX*PX2(I) + PHIY*PY2(I))
       ENDDO
C 
      RETURN
      END
