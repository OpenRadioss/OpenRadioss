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
Chd|  Q4FINTC2                      source/elements/solid_2d/quad4/q4fintc2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4FINTC2(PYC1,PYC2,PZC1,PZC2,AY,FAY,
     2                    FY1,FZ1,FY2,FZ2,
     3                    FY3,FZ3,FY4,FZ4,
     4                    AIRE,VOLU,SIGM,PP,ICP,NEL)
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
#include      "cong1_c.inc"
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "nsvis_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,NEL
C     REAL
      my_real
     .    PYC1(*),PYC2(*),PZC1(*),PZC2(*),AY(*),FAY(*),
     .    FY1(*),FZ1(*),FY2(*),FZ2(*),
     .    FY3(*),FZ3(*),FY4(*),FZ4(*),
     .    AIRE(*),VOLU(*),SIGM(NEL,6),PP(*)
C-----------------------------------------------
C FUNCTION: 
C ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
C TYPE NAME                FUNCTION
C  I   PYC1(*)~PZC2(*)    - SHAPE DERIVATIVES AT ELEMENT CENTER
C  O   FY1(*)~FZ4(*)      - INTERNAL FORCE IN LOCAL STORAGE
C  I   AIRE(*)            - ELEMENT AREA
C  I   VOLU(*)            - VOLUME/THICKNESS OR VOLUME/(2*PI)
C  I   SIGM(NEL,6)          - ELEMENT AVERAGE STRESS COMPONENTS
C  I   PP(*)              - ELEMENT AVERAGE PRESSURE
C  I   ICP                - FLAG FOR CONSTANT PRESURE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SV,SY,SZ,FAC(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(N2D==1 .AND. JHBE==17) THEN
        DO I=LFT,LLT
          FAC(I) = AIRE(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          FAC(I) = VOLU(I)
        ENDDO
      ENDIF
C
C     FOR CONSTANT SHEAR STRAIN
      DO I=LFT,LLT
        SV = SIGM(I,4)*FAC(I)
        SY = SV*PZC1(I)
        SZ = SV*PYC1(I)
        FY1(I) = FY1(I) - SY
        FZ1(I) = FZ1(I) - SZ
        FY3(I) = FY3(I) + SY
        FZ3(I) = FZ3(I) + SZ
        SY = SV*PZC2(I)
        SZ = SV*PYC2(I)
        FY2(I) = FY2(I) - SY
        FZ2(I) = FZ2(I) - SZ
        FY4(I) = FY4(I) + SY
        FZ4(I) = FZ4(I) + SZ
      ENDDO
C
C     FOR CONSTANT PRESSURE CASE
      IF(ICP==1) THEN
        DO I=LFT,LLT
          SV = PP(I)*FAC(I)
          SY = SV*PYC1(I)
          SZ = SV*PZC1(I)
          FY1(I) = FY1(I) - SY
          FZ1(I) = FZ1(I) - SZ
          FY3(I) = FY3(I) + SY
          FZ3(I) = FZ3(I) + SZ
          SY = SV*PYC2(I)
          SZ = SV*PZC2(I)
          FY2(I) = FY2(I) - SY
          FZ2(I) = FZ2(I) - SZ
          FY4(I) = FY4(I) + SY
          FZ4(I) = FZ4(I) + SZ
        ENDDO
      ENDIF
C
      RETURN
      END
	  
