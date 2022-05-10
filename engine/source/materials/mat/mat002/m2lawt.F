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
Chd|  M2LAWT                        source/materials/mat/mat002/m2lawt.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|        CVMGT                         source/system/machine.F       
Chd|====================================================================
      SUBROUTINE M2LAWT(PM   ,GEO   ,OFF  ,FOR  ,EINT,
     .                  AREA ,AL0   ,PLA  ,STI  ,MAT ,
     .                  MGM  ,NGL   ,EPS  ,AL   )
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
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),MGM(MVSIZ),NGL(MVSIZ)
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),OFF(*),FOR(*),EINT(*),
     .   AREA(*),AL0(*),PLA(*),STI(*),EPS(MVSIZ),AL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC(MVSIZ)
      INTEGER I,J
      my_real
     .   YM(MVSIZ),GAP(MVSIZ),
     .   CA(MVSIZ),CB(MVSIZ),CN(MVSIZ),YMAX(MVSIZ),
     .   EPMX(MVSIZ),YLD(MVSIZ),FF(MVSIZ),HH(MVSIZ),
     .   AA(MVSIZ),PR(MVSIZ),YMA
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real
     .   CVMGT
C-----------------------------------------------      
      DO I=LFT,LLT
        YM(I)  =PM(20,MAT(I))
        PR(I)  =PM(21,MAT(I))
        CA(I)  =PM(38,MAT(I))
        CB(I)  =PM(39,MAT(I))
        CN(I)  =PM(40,MAT(I))
        EPMX(I)=PM(41,MAT(I))
        YMAX(I)=PM(42,MAT(I))
        ICC(I) =NINT(PM(49,MAT(I)))
        GAP(I) =GEO(2,MGM(I))
      ENDDO
c
      DO I=LFT,LLT
        IF (GAP(I) > ZERO .AND. AL(I) <= (AL0(I)-GAP(I))) OFF(I)=UN
      ENDDO
c
      DO I=LFT,LLT
        EINT(I)=EINT(I)+FOR(I)*EPS(I)*AL(I)*DT1*UNDEMI
      ENDDO
c
      DO I=LFT,LLT
        AREA(I)=AREA(I)*(UN - DEUX*PR(I)*EPS(I)*DT1*OFF(I))
      ENDDO
c
      DO I=LFT,LLT
        YMA = YM(I)*AREA(I)
        FOR(I)=FOR(I)+YMA*EPS(I)*DT1
        STI(I)=YMA / AL(I)
      ENDDO
c
      DO I=LFT,LLT
        YLD(I)=CA(I)+CB(I)*PLA(I)**CN(I)
      ENDDO
C-----------------------
C     MODULE ECROUISSAGE
C-----------------------
      DO I=LFT,LLT
        IF (CN(I) == UN) THEN
          HH(I)= CB(I)
        ELSE
          IF (PLA(I) /= ZERO) THEN
            HH(I)= CB(I)*CN(I)/PLA(I)**(UN -CN(I))
          ELSE
            HH(I)=ZERO
          ENDIF
        ENDIF
      ENDDO
c
      DO I=LFT,LLT
        AA(I)  = (YM(I)+HH(I))*AREA(I)
        YLD(I) = MIN(YLD(I),YMAX(I))
        FF(I)  = ABS(FOR(I))-YLD(I)*AREA(I)
        FF(I)  = MAX(ZERO,FF(I))
      ENDDO
c
      DO I=LFT,LLT
        PLA(I)=PLA(I)+FF(I)/AA(I)
      ENDDO
c
      DO I=LFT,LLT
        FOR(I)=CVMGT(SIGN(YLD(I)*AREA(I),FOR(I)),FOR(I),FF(I) > ZERO)
      ENDDO
C--------------------------------
C     TEST DE RUPTURE DUCTILE
C-------------------------------
      DO I=LFT,LLT
        IF (OFF(I) < EM01) OFF(I)=ZERO
        IF (OFF(I) < UN) OFF(I)=OFF(I)*QCINQ
      ENDDO 
c
      DO I=LFT,LLT
        IF (OFF(I) < UN)      CYCLE
        IF (PLA(I) < EPMX(I)) CYCLE
        OFF(I)=OFF(I)*QCINQ
        IDEL7NOK = 1
      ENDDO
c
      DO I=LFT,LLT
        IF (OFF(I) /= QCINQ) CYCLE
#include "lockon.inc"
        WRITE(IOUT,1000)  NGL(I)
        WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
      ENDDO
c
      DO I=LFT,LLT
        STI(I)=STI(I)*OFF(I)
        FOR(I)= FOR(I)*OFF(I)  
      ENDDO
c
      DO I=LFT,LLT
        EINT(I)=EINT(I)+FOR(I)*EPS(I)*AL(I)*DT1*UNDEMI
      ENDDO
C
 1000 FORMAT(1X,'-- RUPTURE OF TRUSS ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF TRUSS ELEMENT :',I10,' AT TIME :',G11.4)
C-----------------------------------------------
      RETURN
      END
