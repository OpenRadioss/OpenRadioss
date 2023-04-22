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
Chd|  Q4KEGIJ2                      source/elements/solid_2d/quad4/q4kegij2.F
Chd|-- called by -----------
Chd|        Q4KEG2                        source/elements/solid_2d/quad4/q4keg2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4KEGIJ2 (LFT, LLT, 
     .                     PYI, PZI, PYJ, PZJ, AYS, KIJ, 
     .                     FYY, FZZ, FXX, FYZ)
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
     .   FYY(*), FZZ(*), FXX(*), FYZ(*), KIJ(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   GIJ(MVSIZ), HIJ(2,2,MVSIZ), AG, AH,
     .   BYIYJ,BZIZJ,BYIZJ,BZIYJ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      AG = 0. !0;1;1
      AH = 0. !0;0;2
C-----PART 1
      DO I=LFT,LLT
        GIJ(I) = FYY(I)*PYI(I)*PYJ(I) + FZZ(I)*PZI(I)*PZJ(I) + 
     .           FYZ(I)*(PYI(I)*PZJ(I)+PZI(I)*PYJ(I))
      ENDDO
      IF (N2D==1) THEN
        DO I=LFT,LLT
          GIJ(I) = GIJ(I) + FXX(I)*AYS(I)
        ENDDO
      ENDIF
      DO I=LFT,LLT
        DO J=1,2
          KIJ(J,J,I) = KIJ(J,J,I) + AG*GIJ(I)
        ENDDO
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
      IF (N2D==1) THEN
        DO I=LFT,LLT
          HIJ(1,1,I) = HIJ(1,1,I) + FXX(I)*AYS(I)
        ENDDO
      ENDIF
      DO I=LFT,LLT
        DO J=1,2
        DO K=1,2
          KIJ(K,J,I) = KIJ(K,J,I) - AH*HIJ(K,J,I)
        ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
