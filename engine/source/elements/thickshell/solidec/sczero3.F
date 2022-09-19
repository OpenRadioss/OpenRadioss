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
Chd|  SCZERO3                       source/elements/thickshell/solidec/sczero3.F
Chd|-- called by -----------
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCZERO3(
     .   FX1, FY1, FZ1, FX2, FY2, FZ2, FX3, FY3, FZ3, FX4, FY4, FZ4,
     .   FX5, FY5, FZ5, FX6, FY6, FZ6, FX7, FY7, FZ7, FX8, FY8, FZ8,
     .   SIGM,EINTM,RHOM,QM,EPSPLA,EPSDM,G_PLA,G_EPSD,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  G_PLA,G_EPSD,NEL
C     REAL
      my_real
     .   FX1(*), FX2(*), FX3(*), FX4(*), FX5(*), FX6(*), FX7(*), FX8(*),
     .   FY1(*), FY2(*), FY3(*), FY4(*), FY5(*), FY6(*), FY7(*), FY8(*),
     .   FZ1(*), FZ2(*), FZ3(*), FZ4(*), FZ5(*), FZ6(*), FZ7(*), FZ8(*),
     .   EINTM(*),RHOM(*),QM(*),EPSDM(*),SIGM(NEL,6),EPSPLA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C=======================================================================
      DO I=LFT,LLT
         SIGM(I,1) = ZERO
         SIGM(I,2) = ZERO
         SIGM(I,3) = ZERO
         SIGM(I,4) = ZERO
         SIGM(I,5) = ZERO
         SIGM(I,6) = ZERO
         RHOM(I)   = ZERO
         EINTM(I)  = ZERO
         QM(I)     = ZERO	
      ENDDO
      IF (G_PLA > 0) THEN
       DO I=LFT,LLT
         EPSPLA(I) = ZERO
       ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
       DO I=LFT,LLT
         EPSDM(I) = ZERO
       ENDDO
      ENDIF
      DO I=LFT,LLT
       FX1(I)=ZERO
       FY1(I)=ZERO
       FZ1(I)=ZERO
       FX2(I)=ZERO
       FY2(I)=ZERO
       FZ2(I)=ZERO
       FX3(I)=ZERO
       FY3(I)=ZERO
       FZ3(I)=ZERO
       FX4(I)=ZERO
       FY4(I)=ZERO
       FZ4(I)=ZERO
       FX5(I)=ZERO
       FY5(I)=ZERO
       FZ5(I)=ZERO
       FX6(I)=ZERO
       FY6(I)=ZERO
       FZ6(I)=ZERO
       FX7(I)=ZERO
       FY7(I)=ZERO
       FZ7(I)=ZERO
       FX8(I)=ZERO
       FY8(I)=ZERO
       FZ8(I)=ZERO
      ENDDO
C-----------
      RETURN
      END
