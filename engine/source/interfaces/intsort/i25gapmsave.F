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
C I25main_tri
!||====================================================================
!||    i25gapmsave   ../engine/source/interfaces/intsort/i25gapmsave.F
!||--- called by ------------------------------------------------------
!||    i25main_tri   ../engine/source/interfaces/intsort/i25main_tri.F
!||====================================================================
      SUBROUTINE I25GAPMSAVE(
     1       GAP_M     ,GAPMSAV  ,
     2       MSR       ,NRTM     ,ITASK)
C sauvegarde des GAPMSAV et calcul borne domaine
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK, NRTM, MSR(*)
      my_real
     .        GAP_M(*), GAPMSAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTMF,NRTML,I, J 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      NRTMF = 1 + ITASK * NRTM / NTHREAD
      NRTML = (ITASK+1) * NRTM / NTHREAD
C
      DO I=NRTMF, NRTML
        GAPMSAV(I) = GAP_M(I)
      ENDDO
C
      RETURN
      END
