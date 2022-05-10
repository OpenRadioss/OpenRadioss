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
Chd|  Q4KEGA2                       source/elements/solid_2d/quad4/q4kega2.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|        Q4KEGAIJ2                     source/elements/solid_2d/quad4/q4kega2.F
Chd|====================================================================
      SUBROUTINE Q4KEGA2 (
     1              PY1,PY2,PZ1,PZ2,AY,
     2              K11,K12,K13,K14,K22,
     3              K23,K24,K33,K34,K44,
     4              SIG,AIR,OFF,NEL)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .    PY1(*), PY2(*), PZ1(*), PZ2(*), AY(*), 
     .    K11(2,2,*), K12(2,2,*), K13(2,2,*), K14(2,2,*), K22(2,2,*),
     .    K23(2,2,*), K24(2,2,*), K33(2,2,*), K34(2,2,*), K44(2,2,*),
     .    SIG(NEL,6), AIR(*), OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .    FYY(MVSIZ),FZZ(MVSIZ),FXX(MVSIZ),FYZ(MVSIZ),AYS(MVSIZ),
     .    GIJ(MVSIZ), HIJ(2,2,MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        FYY(I)=SIG(I,1)*AIR(I)
        FZZ(I)=SIG(I,2)*AIR(I)
        FYZ(I)=SIG(I,4)*AIR(I)
      ENDDO
      IF (N2D.EQ.1) THEN
        DO I=LFT,LLT
          FXX(I)=SIG(I,3)*AIR(I)
          AYS(I)=AY(I)*AY(I)
        ENDDO
      ENDIF
C-----K11,K13,K33
      CALL Q4KEGAIJ2(LFT, LLT, 
     .               PY1, PZ1, PY1, PZ1, AYS, GIJ, HIJ,  
     .               FYY, FZZ, FXX, FYZ)
      DO I=LFT,LLT
        DO J=1,2
          K11(J,J,I) = K11(J,J,I) + GIJ(I)
          K13(J,J,I) = K13(J,J,I) - GIJ(I)
          K33(J,J,I) = K33(J,J,I) + GIJ(I)
        ENDDO
        DO J=1,2
        DO K=1,2
          K11(K,J,I) = K11(K,J,I) - HIJ(K,J,I)
          K13(K,J,I) = K13(K,J,I) + HIJ(K,J,I)
          K33(K,J,I) = K33(K,J,I) - HIJ(K,J,I)
        ENDDO
        ENDDO
      ENDDO
C-----K22,K24,K44
      CALL Q4KEGAIJ2(LFT, LLT, 
     .               PY2, PZ2, PY2, PZ2, AYS, GIJ, HIJ,  
     .               FYY, FZZ, FXX, FYZ)
      DO I=LFT,LLT
        DO J=1,2
          K22(J,J,I) = K22(J,J,I) + GIJ(I)
          K24(J,J,I) = K24(J,J,I) - GIJ(I)
          K44(J,J,I) = K44(J,J,I) + GIJ(I)
        ENDDO
        DO J=1,2
        DO K=1,2
          K22(K,J,I) = K22(K,J,I) - HIJ(K,J,I)
          K24(K,J,I) = K24(K,J,I) + HIJ(K,J,I)
          K44(K,J,I) = K44(K,J,I) - HIJ(K,J,I)
        ENDDO
        ENDDO
      ENDDO
C-----K12,K14,K34,K23
      CALL Q4KEGAIJ2(LFT, LLT, 
     .               PY1, PZ1, PY2, PZ2, AYS, GIJ, HIJ,  
     .               FYY, FZZ, FXX, FYZ)
      DO I=LFT,LLT
        DO J=1,2
          K12(J,J,I) = K12(J,J,I) + GIJ(I)
          K14(J,J,I) = K14(J,J,I) - GIJ(I)
          K34(J,J,I) = K34(J,J,I) + GIJ(I)
          K23(J,J,I) = K23(J,J,I) - GIJ(I)
        ENDDO
        DO J=1,2
        DO K=1,2
          K12(K,J,I) = K12(K,J,I) - HIJ(K,J,I)
          K14(K,J,I) = K14(K,J,I) + HIJ(K,J,I)
          K34(K,J,I) = K34(K,J,I) - HIJ(K,J,I)
        ENDDO
        ENDDO
        K23(1,1,I) = K23(1,1,I) + HIJ(1,1,I)
        K23(2,2,I) = K23(2,2,I) + HIJ(2,2,I)
        K23(1,2,I) = K23(1,2,I) + HIJ(2,1,I)
        K23(2,1,I) = K23(2,1,I) + HIJ(1,2,I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  Q4KEGAIJ2                     source/elements/solid_2d/quad4/q4kega2.F
Chd|-- called by -----------
Chd|        Q4KEGA2                       source/elements/solid_2d/quad4/q4kega2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4KEGAIJ2 (LFT, LLT, 
     .                      PYI, PZI, PYJ, PZJ, AYS, GIJ, HIJ,
     .                      FYY, FZZ, FXX, FYZ)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
      my_real
     .   PYI(*), PZI(*), PYJ(*), PZJ(*), AYS(*), 
     .   FYY(*), FZZ(*), FXX(*), FYZ(*), GIJ(*), HIJ(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   AG, AH,
     .   BYIYJ,BZIZJ,BYIZJ,BZIYJ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      AG = 1. !0;1;1
      AH = 0. !0;0;2
C-----PART 1
      DO I=LFT,LLT
        GIJ(I) = FYY(I)*PYI(I)*PYJ(I) + FZZ(I)*PZI(I)*PZJ(I) + 
     .           FYZ(I)*(PYI(I)*PZJ(I)+PZI(I)*PYJ(I))
      ENDDO
      IF (N2D.EQ.1) THEN
        DO I=LFT,LLT
          GIJ(I) = GIJ(I) + FXX(I)*AYS(I)
        ENDDO
      ENDIF
      DO I=LFT,LLT
        GIJ(I) = AG*GIJ(I)
      ENDDO
C-----PART 2
      DO I=LFT,LLT
        BYIYJ = PYI(I)*PYJ(I)
        BZIZJ = PZI(I)*PZJ(I)
        BYIZJ = PYI(I)*PZJ(I)
        BZIYJ = PZI(I)*PYJ(I)
        HIJ(1,1,I) = FYY(I)*(BYIYJ+BZIZJ) + FZZ(I)*BZIZJ + 
     .               FYZ(I)*(BYIZJ+BZIYJ)
        HIJ(1,2,I) = (FYY(I)+FZZ(I))*BZIYJ + FYZ(I)*(BYIYJ+BZIZJ)
        HIJ(2,1,I) = (FYY(I)+FZZ(I))*BYIZJ + FYZ(I)*(BYIYJ+BZIZJ)
        HIJ(2,2,I) = FYY(I)*BYIYJ + FZZ(I)*(BYIYJ+BZIZJ) + 
     .               FYZ(I)*(BYIZJ+BZIYJ)
      ENDDO
      IF (N2D.EQ.1) THEN
        DO I=LFT,LLT
          HIJ(1,1,I) = HIJ(1,1,I) + FXX(I)*AYS(I)
        ENDDO
      ENDIF
      DO I=LFT,LLT
        DO J=1,2
        DO K=1,2
          HIJ(K,J,I) = AH*HIJ(K,J,I)
        ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
