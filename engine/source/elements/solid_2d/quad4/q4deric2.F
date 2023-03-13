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
Chd|  Q4DERIC2                      source/elements/solid_2d/quad4/q4deric2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4DERIC2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,  
     2                    Y12,Y34,Y13,Y24,Y14,Y23,
     3                    Z12,Z34,Z13,Z24,Z14,Z23,
     4                    PYC1,PYC2,PZC1,PZC2,AIRE,
     5                    VOLU,YAVG,RX , RY , RZ ,
     6                    SX ,SY , SZ )
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .    Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .    Y12(*),Y34(*),Y13(*),Y24(*),Y14(*),Y23(*),
     .    Z12(*),Z34(*),Z13(*),Z24(*),Z14(*),Z23(*),
     .    PYC1(*),PYC2(*),PZC1(*),PZC2(*),AIRE(*),VOLU(*),
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,YAVG(*)
C-----------------------------------------------
c FUNCTION: 
c ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
c TYPE NAME                FUNCTION
c  I   Y1(*)~Z4(*)        - NODAL COORDINATES
c  O   Y12(*)~Z23(*)      - DIFFERENCE: Yi-Yj, Zi-Zj
c  O   PYC1(*)~PZC2(*)    - SHAPE DERIVATIVES AT ELEMENT CENTER
c  I   AIRE(*)            - ELEMENT AREA
c  I/O VOL(*)            - ELEMENT VOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .    Y,Z,V0
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        Y12(I) = Y1(I) - Y2(I)
        Y34(I) = Y3(I) - Y4(I)
        Y13(I) = Y1(I) - Y3(I)
        Y24(I) = Y2(I) - Y4(I)
        Y14(I) = Y1(I) - Y4(I)
        Y23(I) = Y2(I) - Y3(I)
        Z12(I) = Z1(I) - Z2(I)
        Z34(I) = Z3(I) - Z4(I)
        Z13(I) = Z1(I) - Z3(I)
        Z24(I) = Z2(I) - Z4(I)
        Z14(I) = Z1(I) - Z4(I)
        Z23(I) = Z2(I) - Z3(I)
      END DO
C
      DO I=LFT,LLT
C       dNi/dY, dNi/dZ, A=4.*|J|
        PYC1(I) = HALF*(Z2(I)-Z4(I))/AIRE(I)
        PYC2(I) = HALF*(Z3(I)-Z1(I))/AIRE(I)
        PZC1(I) = HALF*(Y4(I)-Y2(I))/AIRE(I)
        PZC2(I) = HALF*(Y1(I)-Y3(I))/AIRE(I)
      ENDDO
C     COMPUTE VOLUME/(2*PI) FOR AXISYMMETRIC CASE 
      IF(N2D==1.AND.JHBE==17) THEN
          DO I=LFT,LLT
            VOLU(I) = FOURTH*YAVG(I)*AIRE(I)
          ENDDO
      ENDIF
      DO I=LFT,LLT
        RX(I) = ONE
        RY(I) = ZERO
        RZ(I) = ZERO
        SX(I) = ZERO
        SY(I) =HALF*(Y2(I)+Y3(I)-Y1(I)-Y4(I))
        SZ(I) =HALF*(Z2(I)+Z3(I)-Z1(I)-Z4(I))
      END DO
C
      RETURN
      END
	  