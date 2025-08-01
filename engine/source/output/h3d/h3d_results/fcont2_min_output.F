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
!||    fcont2_min_output   ../engine/source/output/h3d/h3d_results/fcont2_min_output.F
!||--- called by ------------------------------------------------------
!||    sortie_main         ../engine/source/output/sortie_main.F
!||====================================================================
      SUBROUTINE FCONT2_MIN_OUTPUT(FCONT,FCONT_MIN,WEIGHT)
C-----------------------------s------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) ::  WEIGHT(NUMNOD)
      my_real , INTENT(INOUT) :: 
     .   FCONT(3,NUMNOD),FCONT_MIN(3,NUMNOD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      my_real  FOLD, FNEW, FMIN
      INTEGER I,K,N  
C-----------------------------------------------

      DO N=1,NUMNOD
         IF(WEIGHT(N) == 1) THEN
             FNEW = FCONT(1,N)**2 + FCONT(2,N)**2 + FCONT(3,N)**2
             FOLD = FCONT_MIN(1,N)**2 + FCONT_MIN(2,N)**2 +FCONT_MIN(3,N)**2
             FMIN = MIN(FNEW,FOLD)
             IF(FNEW < FOLD) THEN
                FCONT_MIN(1:3,N) = FCONT(1:3,N)   
             ENDIF
          ELSE
             FCONT(1:3,N) =  ZERO
          ENDIF
       ENDDO

 
       RETURN
       END 
