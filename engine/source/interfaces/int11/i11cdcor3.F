Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    i11cdcor3      ../engine/source/interfaces/int11/i11cdcor3.F
!||--- called by ------------------------------------------------------
!||    i11mainf       ../engine/source/interfaces/int11/i11mainf.F
!||    i20mainf       ../engine/source/interfaces/int20/i20mainf.F
!||    imp_i11mainf   ../engine/source/interfaces/int11/i11ke3.F
!||====================================================================
      SUBROUTINE I11CDCOR3(JLT,INDEX,CAND_M,CAND_S,CAND_M_N,
     .                    CAND_S_N)      
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, INDEX(*), CAND_M(*), CAND_S(*),
     .        CAND_M_N(*), CAND_S_N(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I 
C-----------------------------------------------
C
      DO I=1,JLT
        CAND_M_N(I) = CAND_M(INDEX(I))
        CAND_S_N(I) = CAND_S(INDEX(I))
      ENDDO
C
      RETURN
      END
