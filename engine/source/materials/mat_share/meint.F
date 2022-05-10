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
Chd|  MEINT                         source/materials/mat_share/meint.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MEINT(OFF  ,SNEW ,QOLD ,EINT ,VNEW ,ESPE ,
     2                 SOLD1,SOLD2,SOLD3,SOLD4,SOLD5,SOLD6,
     3                 D1   ,D2   ,D3   ,D4   ,D5   ,D6   , 
     4                 PSH  ,DVOL ,DF   ,QNEW ,PNEW ,VIS  ,
     5                 TMU  ,EINC ,MLN  ,VOL_AVG ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MLN,NEL
      my_real
     .   OFF(*), SNEW(NEL,6), QOLD(*), EINT(*)
      my_real
     .   VNEW(*), ESPE(*), QNEW(*), PNEW(*), SOLD1(*), SOLD2(*),
     .   SOLD3(*), SOLD4(*), SOLD5(*), SOLD6(*), 
     .   DVOL(*), DF(*), D1(*), D2(*), D3(*), D4(*), D5(*),
     .   D6(*), PSH(*), EINC(*),
     .   VIS(*), TMU(*), VOL_AVG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   POLD(MVSIZ), VAVG, 
     .   E1, E2, E3, E4, E5, E6,
     .   DTA, TFEXTT   
C-----------------------------------------------
C
      DTA =UNDEMI*DT1
C
      IF(INCOMP.EQ.1.AND. JLAG.EQ.0)THEN
        DO I=LFT,LLT
          POLD(I)=(SOLD1(I)+SOLD2(I)+SOLD3(I))*THIRD
          EINT(I)=EINT(I)+(UNDEMI*DVOL(I)*(POLD(I)-PSH(I)))*OFF(I)
          ESPE(I)=EINT(I)*DF(I) / MAX(EM15,VNEW(I))
          QOLD(I)=QNEW(I)
        ENDDO
        RETURN
      ENDIF
C
      DO I=LFT,LLT
       POLD(I)=(SOLD1(I)+SOLD2(I)+SOLD3(I)) * THIRD
       SOLD1(I)=SOLD1(I)-POLD(I)
       SOLD2(I)=SOLD2(I)-POLD(I)
       SOLD3(I)=SOLD3(I)-POLD(I)
C
       E1=D1(I)*(SOLD1(I)+SNEW(I,1))
       E2=D2(I)*(SOLD2(I)+SNEW(I,2))
       E3=D3(I)*(SOLD3(I)+SNEW(I,3))
       E4=D4(I)*(SOLD4(I)+SNEW(I,4))
       E5=D5(I)*(SOLD5(I)+SNEW(I,5))
       E6=D6(I)*(SOLD6(I)+SNEW(I,6))
C
       EINC(I) = VOL_AVG(I) * (E1+E2+E3+E4+E5+E6) * DTA
      ENDDO
C
      IF(JTUR.EQ.0 .OR. JPOR.EQ.2)THEN
       DO I=LFT,LLT
         EINT(I)=EINT(I)+(EINC(I)+UNDEMI*DVOL(I)*(POLD(I)-PSH(I)-QOLD(I)-QNEW(I)))*OFF(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         EINT(I)=EINT(I)  +  (EINC(I)*(VIS(I)-TMU(I))/VIS(I)  +  UNDEMI*DVOL(I)*(POLD(I)-PSH(I)-QOLD(I)-QNEW(I)))*OFF(I)
       ENDDO
      ENDIF
C
      DO I=LFT,LLT
        QOLD(I)=QNEW(I)
        ESPE(I)=EINT(I)*DF(I) / MAX(EM15,VNEW(I))
      ENDDO
C      
      !NO NEED TO UPDATE PRESSURE CALLING EOSMAIN SUBROUTINE FOR THESE TWO MATERIAL LAWS SINCE PRESSURE IS NOT ENERGY DEPENDANT.
      IF(MLN==21)THEN
        TFEXTT = ZERO
        DO I=LFT,LLT
          EINT(I) = EINT(I) - UNDEMI*DVOL(I)*(PNEW(I)+PSH(I))  
          TFEXTT=TFEXTT-DVOL(I)*PSH(I)                                
        ENDDO
#include "atomic.inc"
        TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
        
      ENDIF      
C
      RETURN
      END
