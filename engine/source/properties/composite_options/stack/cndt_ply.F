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
Cqz|--nouvelle routine commune des elements coques---------
Chd|====================================================================
Chd|  CNDT_PLY                      source/properties/composite_options/stack/cndt_ply.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CNDT_PLY(JFT     ,JLT   ,NPT,OFF  ,ALDT    ,AREA ,THK,
     1                    THK_IPLY,A1    ,A1_IPLY,STI ,OFFI,VISCMX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NPT
C     REAL
      my_real
     .   OFF(*),STI(MVSIZ,*),
     .   ALDT(*), A1(MVSIZ,*), THK(MVSIZ,*),
     .   AREA(*),A1_IPLY(MVSIZ,*),THK_IPLY(MVSIZ,*),
     .   OFFI(MVSIZ,*),VISCMX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDXOF(MVSIZ),
     .        I, J, JINF,JSUP
C     REAL
      my_real
     .    DIVM ,FAC,VOL
C-----------------------------------------------
       DO I=JFT,JLT
         DIVM=ALDT(I)*VISCMX(I)
         DIVM= MAX(DIVM*DIVM, EM20)
         DO J=1,NPT
           VOL = AREA(I)*THK(I,J)
           STI(I,J) = HALF*VOL * A1(I,J)* OFF(I) / DIVM
         ENDDO
       ENDDO
C       
       DO I=JFT,JLT
         DIVM=MAX(ALDT(I)*ALDT(I),EM20)
         DO J=1,NPT - 1 
           JINF = J
           JSUP = J + 1
C    sti = vol*E/lc*lc
C    lc = (vol)**0.3333
C    here we take lc = hi 
C  la rigidit  est surestim , dans le cas ou hi<lc 
cc           FAC = OFF(I)*ONE_OVER_8*AREA(I)/THK_IPLY(I,J)
cc           FAC = OFFI(I,J)*AREA(I)/THK_IPLY(I,J)
           IF(OFF(I)/=ZERO)THEN
             FAC = AREA(I)*FOUR
             STI(I,JINF) = STI(I,JINF) + FAC * A1_IPLY(I,J)  
             STI(I,JSUP) = STI(I,JSUP) + FAC * A1_IPLY(I,J) 
           END IF
         ENDDO
       ENDDO       
C
      RETURN
      END
