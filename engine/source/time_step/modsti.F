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
!||    modsti   ../engine/source/time_step/modsti.F
!||--- called by ------------------------------------------------------
!||    resol    ../engine/source/engine/resol.F
!||====================================================================
      SUBROUTINE MODSTI(NODFT,NODLT,STIFN,VISCN,MS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
      INTEGER NODFT,NODLT
      my_real 
     .  STIFN(*),VISCN(*),MS(*)
C----------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------
      INTEGER I,N
      my_real 
     .  C2,C21,AUX
C----------------------------------------
       DO I=NODFT,NODLT
        IF(MS(I)>ZERO)THEN
         C2      = FOUR*VISCN(I)*VISCN(I)
         C21     = EIGHT*MS(I)*STIFN(I)
         AUX     = SQRT(C2+C21)+2.*VISCN(I)
         STIFN(I)= ONE_OVER_8*AUX*AUX/MAX(MS(I),EM30)
C        STIFN(I)= STIFN(I)*AUX*AUX/MAX(C21,EM30)
         STIFN(I)= MAX(STIFN(I),TWO*VISCN(I)*VISCN(I)/MAX(MS(I),EM30))
         VISCN(I)= ZERO
        ENDIF
       ENDDO
C
      RETURN
      END
