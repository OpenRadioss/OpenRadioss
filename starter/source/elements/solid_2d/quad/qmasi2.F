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
Chd|  QMASI2                        source/elements/solid_2d/quad/qmasi2.F
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QMASI2(PM,MAT,MS,VOL,MSQ,WMA,IPART,PARTSAV,
     .     IX1, IX2, IX3, IX4 ,X ,V)
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
      my_real
     .   PM(NPROPM,*), MS(*), VOL(*),WMA(*), MSQ(*), PARTSAV(20, *),
     .   X(3,*),V(3,*)
      INTEGER MAT(*),IPART(*), IX1(*), IX2(*), IX3(*), IX4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "com07_c.inc"
#include      "param_c.inc"
#include      "ale_param.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP ,I1 ,I2 ,I3 ,I4
      my_real
     .   YY ,ZZ ,YZ ,
     .   MASS(MVSIZ)
C-----------------------------------------------------------------------
      DO I=LFT,LLT
        MASS(I)=PM(89,MAT(I))*VOL(I)*FOURTH
      ENDDO

C calcul mass initiale en arithmetique // pour le spmd
        DO I=LFT,LLT
          MSQ(I)=MASS(I)
        ENDDO
	
      IF(JALE > 0 .AND. NWALE == 4)THEN
        DO I=LFT,LLT
          WMA(IX1(I))=WMA(IX1(I))+ONE
          WMA(IX2(I))=WMA(IX2(I))+ONE
          WMA(IX3(I))=WMA(IX3(I))+ONE
          WMA(IX4(I))=WMA(IX4(I))+ONE
        ENDDO
      ENDIF

      DO I=LFT,LLT
          IP = IPART(I)
          PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*MASS(I)
C
          I1 = IX1(I)
          I2 = IX2(I)
          I3 = IX3(I)
          I4 = IX4(I)
          YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .         +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4))
          YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .         +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4))
          ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .       +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4))
c
          PARTSAV(5,IP) =PARTSAV(5,IP)  + MASS(I) * (YY+ZZ)
          PARTSAV(6,IP) =PARTSAV(6,IP)  + MASS(I) * ZZ
          PARTSAV(7,IP) =PARTSAV(7,IP)  + MASS(I) * YY
          PARTSAV(9,IP) =PARTSAV(9,IP)  - MASS(I) * YZ
C
          PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*
     .       (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
          PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*
     .       (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
          PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .     (V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .     +V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .     +V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
C
      ENDDO

      RETURN

      END
