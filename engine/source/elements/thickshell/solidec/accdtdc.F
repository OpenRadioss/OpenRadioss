Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ACCDTDC                       source/elements/thickshell/solidec/accdtdc.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ACCDTDC(EFTSK ,ELTSK ,IENUNL ,ALPHA_DC, A       ,MS   ,ITAB )
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EFTSK ,ELTSK ,IENUNL(2,*), ITAB(*)
      my_real
     .   A(3,*) ,ALPHA_DC(*),MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N1,N2,inn
      my_real
     .     AM(3),AD(3),ALPHA_1,F1(3),F2(3),MSS_1
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !------------------------------------------------!
      !       MODIF  ACCELERATIONS (TRANSLATIONS) !
      !------------------------------------------------!
#include "vectorize.inc"
      DO I=EFTSK ,ELTSK
        N1= IENUNL(1,I)
        N2= IENUNL(2,I)
        IF (MAX(ALPHA_DC(N1),ALPHA_DC(N2))<=ONE) CYCLE
        IF (MAX(MS(N1),MS(N2))==ZERO) CYCLE
        IF (MS(N1)==ZERO) THEN
          F1(1:3) = A(1:3,N1)
        ELSE
          F1(1:3) = MS(N1)*A(1:3,N1)
        END IF
        IF (MS(N2)==ZERO) THEN
          F2(1:3) = A(1:3,N2)
        ELSE
          F2(1:3) = MS(N2)*A(1:3,N2)
        END IF
        MSS_1 = ONE/(MS(N1)+MS(N2))
        AM(1:3) = (F1(1:3)+F2(1:3))*MSS_1
        AD(1:3) = (F2(1:3)-F1(1:3))*MSS_1
        ALPHA_1 =ONE/MAX(ALPHA_DC(N1),ALPHA_DC(N2))
        A(1:3,N1) = AM(1:3) - AD(1:3)*ALPHA_1
        A(1:3,N2) = AM(1:3) + AD(1:3)*ALPHA_1
      END DO 
C      
      RETURN
      END
