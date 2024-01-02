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
#include "my_real.inc"
Chd|====================================================================
Chd|  FINTER                        source/tools/curve/finter.F   
Chd|-- called by -----------
Chd|        CM58_REFSTA                   source/materials/mat/mat058/cm58_refsta.F
Chd|        FCT_FIBER_2                   source/materials/mat/mat098/lossfun_98.F
Chd|        INIMOM_FVM                    source/elements/solid/solide/inimom_fvm.F
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|        M77INIT                       source/materials/mat/mat077/m77init.F
Chd|        SIGEPS35                      source/materials/mat/mat035/sigeps35.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|        SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|        SIGEPS90                      source/materials/mat/mat090/sigeps90.F
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION FINTER(IFUNC,XX,NPF,TF,DERI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER IFUNC,NPF(*),I
      my_real TF(*),ABC,DERI,XX,DX1,DX2
C
      DX2 = TF(NPF(IFUNC)) - XX
C
C     constant function
      IF ((NPF(IFUNC+1)-NPF(IFUNC))==2) THEN
        FINTER = TF(NPF(IFUNC)+1)
        RETURN
      ELSE
        DO 100 I=NPF(IFUNC)+2,NPF(IFUNC+1)-2,2
        DX1 = -DX2
        DX2 = TF(I) - XX
        IF(DX2>=ZERO.OR.I==NPF(IFUNC+1)-2)THEN
          DERI = (TF(I+1) - TF(I-1)) / (TF(I) - TF(I-2))
          IF(DX1<=DX2)THEN
            FINTER = TF(I-1) + DX1 * DERI
          ELSE
            FINTER = TF(I+1) - DX2 * DERI
          ENDIF
          RETURN
        ENDIF
 100    CONTINUE
      END IF
C
      RETURN
      END
