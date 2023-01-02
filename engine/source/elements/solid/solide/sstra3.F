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
Chd|  SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SSTRA3(DXX   ,DYY   ,DZZ   ,D4    ,D5    ,
     .                   D6    ,STRAIN,WXX   ,WYY   ,WZZ   ,
     .                   OFF   ,NEL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real DXX(*), DYY(*), DZZ(*), D4(*), D5(*), D6(*), 
     .        STRAIN(NEL,6), WXX(*), WYY(*), WZZ(*),OFF(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real WXXF, WYYF, WZZF, Q1, Q2, Q3, SS1, SS2, SS3, 
     .        SS4, SS5, SS6, DTOFF(NEL)
C-      
      WXXF = ZERO
      WYYF = ZERO
      WZZF = ZERO
      Q1 = ZERO
      Q2= ZERO
      Q3=0
      DTOFF(1:NEL) = DT1*OFF(1:NEL)
      IF(JCVT>0)THEN 
        DO I=LFT,LLT
          STRAIN(I,1)=STRAIN(I,1)+DXX(I)*DTOFF(I)
          STRAIN(I,2)=STRAIN(I,2)+DYY(I)*DTOFF(I)
          STRAIN(I,3)=STRAIN(I,3)+DZZ(I)*DTOFF(I)
          STRAIN(I,4)=STRAIN(I,4)+D4(I)*DTOFF(I)
          STRAIN(I,5)=STRAIN(I,5)+D5(I)*DTOFF(I)
          STRAIN(I,6)=STRAIN(I,6)+D6(I)*DTOFF(I)
        ENDDO
      ELSE    
        DO I=LFT,LLT
C
           WXXF=WXX(I)*OFF(I)
           WYYF=WYY(I)*OFF(I)
           WZZF=WZZ(I)*OFF(I)
C
           Q1=STRAIN(I,4)*WZZF
           Q2=STRAIN(I,6)*WYYF
           Q3=STRAIN(I,5)*WXXF
           SS1=STRAIN(I,1)-Q1+Q2
           SS2=STRAIN(I,2)+Q1-Q3
           SS3=STRAIN(I,3)-Q2+Q3
           SS4=STRAIN(I,4)+2.*WZZF*(STRAIN(I,1)-STRAIN(I,2))+
     .         WYYF*STRAIN(I,5)-WXXF*STRAIN(I,6)
           SS5=STRAIN(I,5)+2.*WXXF*(STRAIN(I,2)-STRAIN(I,3))+
     .         WZZF*STRAIN(I,6)-WYYF*STRAIN(I,4)
           SS6=STRAIN(I,6)+2.*WYYF*(STRAIN(I,3)-STRAIN(I,1))+
     .         WXXF*STRAIN(I,4)-WZZF*STRAIN(I,5)
           STRAIN(I,1)= SS1 + DXX(I)*DTOFF(I)
           STRAIN(I,2)= SS2 + DYY(I)*DTOFF(I)
           STRAIN(I,3)= SS3 + DZZ(I)*DTOFF(I)
           STRAIN(I,4)= SS4 + D4(I)*DTOFF(I)
           STRAIN(I,5)= SS5 + D5(I)*DTOFF(I)
           STRAIN(I,6)= SS6 + D6(I)*DTOFF(I)
         ENDDO
       ENDIF
c------------
       RETURN      
       END SUBROUTINE SSTRA3
