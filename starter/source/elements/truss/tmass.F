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
Chd|  TMASS                         source/elements/truss/tmass.F 
Chd|-- called by -----------
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        TINIT3                        source/elements/truss/tinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TMASS(X      ,NC      ,GEO  ,PM   ,MS ,
     .     STIFN  ,PARTSAV ,V    ,IPART,MST,
     .     STIFINT,STT     ,AREA , MXT, NC1, NC2,
     .     X1, X2, Y1, Y2, Z1, Z2)
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
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
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC(NIXT,*),IPART(*), MXT(*), NC1(*), NC2(*)
      my_real
     .     X(3,*),GEO(NPROPG,*),PM(NPROPM,*),MS(*),STIFN(*),
     .     V(3,*),PARTSAV(20,*),MST(*),STIFINT(*),STT(*),AREA(*),
     .     X1(*), X2(*), Y1(*), Y2(*), Z1(*), Z2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,I1,I2,J,K
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX
      my_real
     .   AL(MVSIZ),
     .   RHO(MVSIZ),EMS(MVSIZ),
     .   E,STI(MVSIZ),DX,DY,DZ
C
C---------------------------------------------------------------------
      DO I=LFT,LLT
       RHO(I) =PM(89,MXT(I))
      ENDDO
C----------------------------------------------
C     MASSE ELEMENT /2
C----------------------------------------------
      DO I=LFT,LLT
       AL(I)=SQRT((X2(I)-X1(I))**2
     .           +(Y2(I)-Y1(I))**2
     .           +(Z2(I)-Z1(I))**2)
       EMS(I)=RHO(I)*AL(I)*AREA(I)*UNDEMI
      ENDDO
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
C     mass init parith/on spmd
      IF (IMACH /= 3) THEN
        DO I=LFT,LLT
          MS(NC1(I))=MS(NC1(I))+EMS(I)
          MS(NC2(I))=MS(NC2(I))+EMS(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          MST(I)=EMS(I)
        ENDDO
      ENDIF
C----------------------------------------------
C     INITIALISATION DES RIGIDITES NODALES
C     et RIGIDITES NODALES POUR INTERFACES
C----------------------------------------------
      DO I=LFT,LLT
       STI(I) = PM (20,MXT(I)) * AREA(I) / AL(I)
       STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
       STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
      ENDDO
      IF (I7STIFS /= 0) THEN
        IF (IMACH /= 3) THEN
          DO I=LFT,LLT
           STIFINT(NC1(I))=STIFINT(NC1(I))+STI(I)
           STIFINT(NC2(I))=STIFINT(NC2(I))+STI(I)
          ENDDO
        ELSE
          DO I=LFT,LLT
           STT(I)=STI(I)
          ENDDO
        ENDIF
      ENDIF
C
      DO I=LFT,LLT
        I1 = NC1(I)
        I2 = NC2(I)
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + DEUX*EMS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X(1,I1)+X(1,I2))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(X(2,I1)+X(2,I2))
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(X(3,I1)+X(3,I2))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2))
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2))
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2))
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2))
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2))
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2))
        PARTSAV(5,IP) =PARTSAV(5,IP) + EMS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + EMS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + EMS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
        PARTSAV(14,IP)=PARTSAV(14,IP) + UNDEMI * EMS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
      ENDDO
C
      RETURN
      END
