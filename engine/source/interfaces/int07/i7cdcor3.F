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
      !||====================================================================
      !||    i7cdcor3         ../engine/source/interfaces/int07/i7cdcor3.F
      !||--- called by ------------------------------------------------------
      !||    i10mainf         ../engine/source/interfaces/int10/i10mainf.F
      !||    i18main_kine_i   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i20mainf         ../engine/source/interfaces/int20/i20mainf.F
      !||    i21mainf         ../engine/source/interfaces/int21/i21mainf.F
      !||    i23mainf         ../engine/source/interfaces/int23/i23mainf.F
      !||    i7main_lmult     ../engine/source/interfaces/int07/i7main_lmult.F
      !||    i7mainf          ../engine/source/interfaces/int07/i7mainf.F
      !||    imp_i10mainf     ../engine/source/interfaces/int10/i10ke3.F
      !||    imp_i7mainf      ../engine/source/interfaces/int07/i7ke3.F
      !||====================================================================
      SUBROUTINE I7CDCOR3(JLT,INDEX,CAND_E,CAND_N,CAND_E_N,
     .                    CAND_N_N)      
C============================================================================
C  cette routine est appelee par : I10MAINF(/int10/i10mainf.F)
C                                  I7MAINF(/int7/i7mainf.F)
C----------------------------------------------------------------------------
C  cette routine appelle : -
C============================================================================
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, INDEX(*), CAND_E(*), CAND_N(*),
     .        CAND_E_N(*), CAND_N_N(*)
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
