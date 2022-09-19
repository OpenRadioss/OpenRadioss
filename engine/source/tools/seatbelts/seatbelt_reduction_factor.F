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
Chd|  SEATBELT_REDUCTION_FACTOR     source/tools/seatbelts/seatbelt_reduction_factor.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SEATBELT_REDUCTION_FACTOR(ELBUF_STR,JFT,JLT,IXC,NEL,X,NFT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C----6------------------------------------------
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXC(NIXC,NUMELC),NEL,JFT,JLT,NFT
      my_real, INTENT(IN) :: X(3,NUMNOD)
      TYPE (ELBUF_STRUCT_), INTENT(INOUT), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2,N3,N4,POS,NEL_L,CORES(MVSIZ+1),FLAG
      my_real
     .        DIST,DISTB
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C---------------------------------------------------------
C
      GBUF => ELBUF_STR%GBUF
C
C-----------------------------------------------------------------------
C     Computation of logarthmic reduction factor of stress for seatbelts
C-----------------------------------------------------------------------
C
      NEL_L = 0
C
#include "vectorize.inc"
      DO I=JFT,JLT
        FLAG = MIN(1,ABS(GBUF%UPDATE(I)))
        NEL_L = NEL_L + FLAG
        CORES(1+NEL_L*FLAG) = I
      ENDDO
C
      DO K=1,NEL_L
C
        I = CORES(K+1)
        J = NFT + I
C
C--     Element will be decreased - scaling factor 1.0 -> 0.0
        IF (GBUF%UPDATE(I) > ZERO) THEN
          POS = GBUF%UPDATE(I)
          IF ((POS==2).OR.(POS==5)) THEN
            N1 = IXC(2,J)
            N2 = GBUF%ADD_NODE(I)
            N3 = IXC(5,J)
            N4 = GBUF%ADD_NODE(NEL*3+I)
          ELSE
            N1 = IXC(3,J)
            N2 = GBUF%ADD_NODE(NEL+I)
            N3 = IXC(4,J)
            N4 = GBUF%ADD_NODE(NEL*2+I)
          ENDIF
          DIST = SQRT((X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+(X(3,N1)-X(3,N2))**2)
          DISTB = SQRT(MAX(EM20,(X(1,N3)-X(1,N4))**2+(X(2,N3)-X(2,N4))**2+(X(3,N3)-X(3,N4))**2))
          DIST = MIN(DIST,DISTB)
          GBUF%INTVAR(I) = LOG(ONE + 1.72*DIST/GBUF%INTVAR(NEL+I))
          GBUF%INTVAR(I) = MAX(ZERO,GBUF%INTVAR(I))
          GBUF%INTVAR(I) = MIN(ONE,GBUF%INTVAR(I))
        ENDIF
C
C--     Element will be increased - scaling factor 0.0 -> 1.0
        IF (GBUF%UPDATE(I) < ZERO) THEN
          POS = -GBUF%UPDATE(I)
          IF ((POS==2).OR.(POS==5)) THEN
            N1 = IXC(2,J)
            N2 = GBUF%ADD_NODE(I)
            N3 = IXC(5,J)
            N4 = GBUF%ADD_NODE(NEL*3+I)
          ELSE
            N1 = IXC(3,J)
            N2 = GBUF%ADD_NODE(NEL+I)
            N3 = IXC(4,J)
            N4 = GBUF%ADD_NODE(NEL*2+I)
          ENDIF
          DIST = SQRT(MAX(EM20,(X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+(X(3,N1)-X(3,N2))**2))
          DISTB = SQRT(MAX(EM20,(X(1,N3)-X(1,N4))**2+(X(2,N3)-X(2,N4))**2+(X(3,N3)-X(3,N4))**2))
          DIST = MIN(DIST,DISTB)
          GBUF%INTVAR(I) = LOG(ONE + 1.72*DIST/GBUF%INTVAR(NEL+I))
          GBUF%INTVAR(I) = MAX(ZERO,GBUF%INTVAR(I))
          GBUF%INTVAR(I) = MIN(ONE,GBUF%INTVAR(I))
        ENDIF
C
      ENDDO
C
      RETURN
      END
