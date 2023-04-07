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
Chd|  SZSTRAINGPS                   source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        GPSSTRAIN_SKIN                source/output/anim/generate/tensgpstrain.F
Chd|        STRN_TENSCOR3                 source/output/h3d/h3d_results/h3d_strn_tenscor3.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSTRAINGPS(
     1   STRAIN,  STR_PI,  STRHG,   NEL)
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
      INTEGER NEL
      my_real
     .   STRAIN(NEL,6) ,STR_PI(MVSIZ,6,8),STRHG(NEL,3,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,IR,IS,IT,IP
      my_real
     .   KSI,ETA,ZETA,COEF,F1,F2,F3,F1X,F2Y,F3Z
      my_real
     .  A_GAUSS(9,9)
      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
      
C-----------------------------------------------
      DO IR=1,2
       DO IS=1,2
        DO IT=1,2
         IP = IR + ( (IS-1) + (IT-1)*2 )*2
         KSI = A_GAUSS(IR,2)
         ETA = A_GAUSS(IS,2)
         ZETA = A_GAUSS(IT,2)
         F1 = ETA*ZETA
         F2 = KSI*ZETA
         F3 = ETA*KSI
C
         DO I=1,NEL
          STR_PI(I,1,IP)=STRAIN(I,1)+
     +                   STRHG(I,1,1)*KSI+STRHG(I,1,2)*ETA+STRHG(I,1,3)*ZETA+
     +                   STRHG(I,1,5)*F1+STRHG(I,2,6)*F2+STRHG(I,3,6)*F3
          STR_PI(I,2,IP)=STRAIN(I,2)+
     +                   STRHG(I,2,1)*KSI+STRHG(I,2,2)*ETA+STRHG(I,2,3)*ZETA+
     +                   STRHG(I,2,5)*F2+STRHG(I,1,6)*F1+STRHG(I,3,6)*F3
          STR_PI(I,3,IP)=STRAIN(I,3)+
     +                   STRHG(I,3,1)*KSI+STRHG(I,3,2)*ETA+STRHG(I,3,3)*ZETA+
     +                   STRHG(I,3,5)*F3+STRHG(I,1,6)*F1+STRHG(I,2,6)*F2
          STR_PI(I,4,IP)=STRAIN(I,4)+STRHG(I,1,4)*ZETA
          STR_PI(I,6,IP)=STRAIN(I,5)+STRHG(I,2,4)*ETA
          STR_PI(I,5,IP)=STRAIN(I,6)+STRHG(I,3,4)*KSI
         END DO
        ENDDO 
       ENDDO 
      ENDDO
C      
      RETURN
      END
