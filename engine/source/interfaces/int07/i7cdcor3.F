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
Chd|  I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|-- called by -----------
Chd|        I10MAINF                      source/interfaces/int10/i10mainf.F
Chd|        I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|        IMP_I10MAINF                  source/interfaces/int10/i10ke3.F
Chd|        IMP_I7MAINF                   source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7CDCOR3(JLT,INDEX,CAND_E,CAND_N,CAND_E_N,
     .                    CAND_N_N)      
C============================================================================
C  cette routine est appelee par : I10MAINF(/int10/i10mainf.F)
C                                  I7MAINF(/int7/i7mainf.F)
C----------------------------------------------------------------------------
C  cette routine appelle : -
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, INDEX(*), CAND_E(*), CAND_N(*),
     .        CAND_E_N(*), CAND_N_N(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I 
C-----------------------------------------------
C
      !initialization of local buffer (1:MVSIZ)
      DO I=1,JLT
        CAND_E_N(I) = CAND_E(INDEX(I))
        CAND_N_N(I) = CAND_N(INDEX(I))
      ENDDO
C
      RETURN
      END
