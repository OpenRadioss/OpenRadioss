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
Chd|  CBATHERM                      source/elements/shell/coqueba/cbatherm.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATHERM(JFT  ,JLT   ,PM   ,MAT   ,THK   ,IXC , 
     2                  BM   ,AREA   ,DT1C  ,TEMPNC ,TEL   ,DIE  ,
     3                  NPLAT, IPLAT ,FPHI)
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
      INTEGER JFT, JLT,NPLAT,
     .        MAT(*),IXC(NIXC,*),IPLAT(*)
C     REAL
      my_real
     .   AREA(*), 
     .   TEMPNC(*), FPHI(MVSIZ,4), PM(NPROPM,*),DIE(*),
     .   DT1C(*), THK(*),TEL(*),BM(MVSIZ,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,MX, EP,N1,N2,N3,N4
C     REAL
      my_real
     .  CA,CB ,KC ,PHIX,PHIY,A,TEMP1,TEMP2,TEMP3,TEMP4,
     .  TEMP13,TEMP24,TEMPH

C------------------------------------------

        MX  =MAT(JFT)
        CA = PM(75,MX)
        CB = PM(76,MX)


        DO EP=JFT,NPLAT 
          I=IPLAT(EP)

          KC = ( CA + CB*TEL(I))*DT1C(I)*THEACCFACT
C
          N1 = IXC(2,I)
          N2 = IXC(3,I)
          N3 = IXC(4,I)
          N4 = IXC(5,I)

          TEMP13 = TEMPNC(N1)-TEMPNC(N3)
          TEMP24 = TEMPNC(N2)-TEMPNC(N4)
          TEMPH  = TEMPNC(N1)-TEMPNC(N2)+TEMPNC(N3)-TEMPNC(N4)
C 
C - flux : K gradT = B * T
C   

          PHIX =  TEMP13*BM(I,1) + TEMP24*BM(I,2)+BM(I,3)*TEMPH
          
          PHIY =  TEMP13*BM(I,5) + TEMP24*BM(I,6)+BM(I,7)*TEMPH
C     
          PHIX = KC*PHIX*THK(I)*AREA(I)
          PHIY = KC*PHIY*THK(I)*AREA(I)
C
C Thermal nodal Force
C

          A = FOURTH *FOURTH * DIE(I)

          FPHI(I,1) = A - (PHIX*(BM(I,1)+BM(I,3)) + PHIY*(BM(I,5)+BM(I,7)))
          FPHI(I,2) = A - (PHIX*(BM(I,2)-BM(I,3)) + PHIY*(BM(I,6)-BM(I,7)))
          FPHI(I,3) = A - (PHIX*(BM(I,3)-BM(I,1)) + PHIY*(BM(I,7)-BM(I,5)))
          FPHI(I,4) = A + (PHIX*(BM(I,2)+BM(I,3)) + PHIY*(BM(I,6)+BM(I,7)))

        END DO

        DO EP=NPLAT+1,JLT 

          I=IPLAT(EP)

          KC = ( CA + CB*TEL(I))*DT1C(I)
C
          N1 = IXC(2,I)
          N2 = IXC(3,I)
          N3 = IXC(4,I)
          N4 = IXC(5,I)

          TEMP1 = TEMPNC(N1)
          TEMP2 = TEMPNC(N2)
          TEMP3 = TEMPNC(N3)
          TEMP4 = TEMPNC(N4)
C 
C - flux : K gradT = B * T
C
          PHIX =  (BM(I,1)+BM(I,4))*TEMP1 + (BM(I,10)+BM(I,13))*TEMP2
     .         +  (BM(I,19)+BM(I,22))*TEMP3 + (BM(I,28)+BM(I,31))*TEMP2  

          PHIY =  (BM(I,2)+BM(I,5))*TEMP1 + (BM(I,11)+BM(I,14))*TEMP2
     .         +  (BM(I,20)+BM(I,23))*TEMP3 + (BM(I,29)+BM(I,32))*TEMP2  

          PHIX = KC*PHIX*THK(I)*AREA(I)
          PHIY = KC*PHIY*THK(I)*AREA(I)    
C
C Thermal nodal Force
C          
          A = FOURTH *FOURTH * DIE(I)

          FPHI(I,1) = A - (PHIX*(BM(I,1)+BM(I,4))   + PHIY*(BM(I,2)+BM(I,5)))
          FPHI(I,2) = A - (PHIX*(BM(I,10)+BM(I,13)) + PHIY*(BM(I,11)+BM(I,14)))
          FPHI(I,3) = A - (PHIX*(BM(I,19)+BM(I,22)) + PHIY*(BM(I,20)+BM(I,23)))
          FPHI(I,4) = A - (PHIX*(BM(I,28)+BM(I,31))  + PHIY*(BM(I,29)+BM(I,32)))

        END DO
C 
      RETURN
      END
