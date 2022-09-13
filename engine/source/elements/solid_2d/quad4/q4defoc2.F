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
Chd|  Q4DEFOC2                      source/elements/solid_2d/quad4/q4defoc2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4DEFOC2(VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     2                    PYC1,PYC2,PZC1,PZC2,AIRE,
     3                    EYZ,EXX,DSV,ICP)
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .    VY1(*),VY2(*),VY3(*),VY4(*),
     .    VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .    PYC1(*),PYC2(*),PZC1(*),PZC2(*),AIRE(*),
     .    EYZ(*),EXX(*),DSV(*)
      INTEGER ICP
C-----------------------------------------------
c FUNCTION: 
c ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
c TYPE NAME                FUNCTION
c  I   VY1(*)~VZ4(*)      - NODAL VELOCITIES
c  I   PYC1(*)~PZC2(*)    - SHAPE DERIVATIVES AT CENTER
c  I   AY(*)              - Ni/r AT CENTER
c  I   AIRE(*)            - ELEMENT AREA
c  O   EYZ(*)             - RATE OF SHEAR STRAIN AT CENTER
c  O   EXX(*)             - RATE OF X- NORMAL STRAIN AT CENTER
c  O   DSV(*)             - VOLUME STRAIN RATE AT CENTER
c  I   ICP                - FLAG FOR CONSTANT PRESURE
c  I   R22(*)~R33(*)      - TRANSFORMATION MATRIX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .    VY13(MVSIZ),VY24(MVSIZ),VZ13(MVSIZ),VZ24(MVSIZ),
     .    DYZ(MVSIZ),DZY(MVSIZ),DYY(MVSIZ),DZZ(MVSIZ),
     .    VY,VZ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        VY13(I) = VY1(I)-VY3(I)
        VY24(I) = VY2(I)-VY4(I)
        VZ13(I) = VZ1(I)-VZ3(I)
        VZ24(I) = VZ2(I)-VZ4(I)
      ENDDO
C
C     SHEAR STRAIN RATE AND INTRINSIC ROTATION
      DO I=LFT,LLT
        IF(AIRE(I)>ZERO) THEN
          DZY(I) = PYC1(I)*VZ13(I)+PYC2(I)*VZ24(I)
          DYZ(I) = PZC1(I)*VY13(I)+PZC2(I)*VY24(I)
          DYY(I) = PYC1(I)*VY13(I)+PYC2(I)*VY24(I)
          DZZ(I) = PZC1(I)*VZ13(I)+PZC2(I)*VZ24(I)
        ELSE
          DZY(I) = ZERO
          DYZ(I) = ZERO
          DYY(I) = ZERO
          DZZ(I) = ZERO
        ENDIF
      ENDDO
      IF(JCVT==0) THEN
        DO I=LFT,LLT
c          WXX(I) = HALF*DT1*(DZY(I)-DYZ(I))
          EYZ(I) = DZY(I)+DYZ(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
c          WXX(I) = ZERO
          EYZ(I) = DZY(I)+DYZ(I)-DT1*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I))
        ENDDO
      ENDIF
C
C     X- NORMAL STRAIN RATE
      IF(N2D/=1) THEN
        DO I=LFT,LLT
          EXX(I) = ZERO
        ENDDO
      ENDIF
C
C     VOLUMETRIC STRAIN RATE
      IF(ICP>0) THEN
        DO I=LFT,LLT
          DSV(I) = DYY(I)+DZZ(I)+EXX(I)
        ENDDO
      ENDIF
C
      RETURN
      END
