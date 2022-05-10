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
Chd|  THERMC                        source/materials/mat_share/thermc.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THERMC(JFT  ,JLT   ,PM   ,MAT   ,THK   ,IXC , 
     2                  PX1  ,PX2   ,PY1  ,PY2   ,AREA   ,DT1C ,
     3                  TEMPNC ,TEL   ,DIE  ,FPHI)
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
C     REAL ou REAL*8
      my_real
     .   AREA(*), PX1(*), PX2(*), PY1(*), PY2(*),
     .   TEMPNC(*), FPHI(MVSIZ,4), PM(NPROPM,*),DIE(*),
     .   DT1C(*), THK(*),TEL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,MX
C     REAL ou REAL*8
      my_real
     .  CA,CB ,KC ,PHIX,PHIY,A
C --- calcul de la tempurature au CG 

        MX  =MAT(JFT)
        CA = PM(75,MX)
        CB = PM(76,MX)
        DO I=JFT,JLT
C 
C - flux
C      
          KC = (CA + CB*TEL(I))*DT1C(I)/MAX(EM20,AREA(I))
          PHIX =  TEMPNC(IXC(2,I))*PX1(I) + TEMPNC(IXC(3,I))*PX2(I)  -
     .            TEMPNC(IXC(4,I))*PX1(I) - TEMPNC(IXC(5,I))*PX2(I)
          
          PHIY =  TEMPNC(IXC(2,I))*PY1(I) + TEMPNC(IXC(3,I))*PY2(I)  -
     .            TEMPNC(IXC(4,I))*PY1(I) - TEMPNC(IXC(5,I))*PY2(I)
C     
          PHIX = KC*PHIX*THK(I)
          PHIY = KC*PHIY*THK(I)
C
C force thermique nodale
C
          A = QUART * DIE(I)
          FPHI(I,1) = A - (PHIX*PX1(I) + PHIY*PY1(I))
          FPHI(I,2) = A - (PHIX*PX2(I) + PHIY*PY2(I))
          FPHI(I,3) = A + (PHIX*PX1(I) + PHIY*PY1(I))
          FPHI(I,4) = A + (PHIX*PX2(I) + PHIY*PY2(I))
       ENDDO
C 
      RETURN
      END
