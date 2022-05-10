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
C
Chd|====================================================================
Chd|  SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|        SLEN                          source/elements/solid/solide/slen.F
Chd|====================================================================
      SUBROUTINE SDLEN3(
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              DELTAX, VOLN)
C
C     CHARACTERISTIC LENGTH : DX = V / Smax
C
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
#include      "vect01_c.inc"
#include      "alesolver.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NFAC
      my_real
     .   VOLN(*), XIOFF(MVSIZ), AREAM(MVSIZ),
     .   ATEST(MVSIZ), AREA(6,MVSIZ),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*), Y1(*),
     .   Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*), Z1(*), Z2(*),
     .   Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*), DELTAX(*)
C
C---------------------------------------------------------------------
      DO I=LFT,LLT
        XIOFF(I) = UN
        AREAM(I) = ZERO
      ENDDO

      CALL SLEN(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,1, AREA, AREAM)
      CALL SLEN(X5,X6,X7,X8,Y5,Y6,Y7,Y8,Z5,Z6,Z7,Z8,2, AREA, AREAM)
      CALL SLEN(X1,X2,X6,X5,Y1,Y2,Y6,Y5,Z1,Z2,Z6,Z5,3, AREA, AREAM)
      CALL SLEN(X2,X3,X7,X6,Y2,Y3,Y7,Y6,Z2,Z3,Z7,Z6,4, AREA, AREAM)
      CALL SLEN(X3,X4,X8,X7,Y3,Y4,Y8,Y7,Z3,Z4,Z8,Z7,5, AREA, AREAM)
      CALL SLEN(X4,X1,X5,X8,Y4,Y1,Y5,Y8,Z4,Z1,Z5,Z8,6, AREA, AREAM)

      DO I=LFT,LLT
        ATEST(I) = EM4*AREAM(I)
      ENDDO
      
      DO I=LFT,LLT
        NFAC=0
        IF(AREA(1,I).LT.ATEST(I)) NFAC=NFAC+1
        IF(AREA(2,I).LT.ATEST(I)) NFAC=NFAC+1
        IF(AREA(3,I).LT.ATEST(I)) NFAC=NFAC+1
        IF(AREA(4,I).LT.ATEST(I)) NFAC=NFAC+1
        IF(AREA(5,I).LT.ATEST(I)) NFAC=NFAC+1
        IF(AREA(6,I).LT.ATEST(I)) NFAC=NFAC+1
        IF(NFAC.GE.3) XIOFF(I)=EP03
      ENDDO
      
      DO I=LFT,LLT
        DELTAX(I) = QUATRE*VOLN(I)*XIOFF(I)/SQRT(AREAM(I))
      ENDDO

      RETURN
      END
