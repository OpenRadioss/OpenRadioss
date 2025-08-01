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
!||    varcondec   ../engine/source/interfaces/interf/varcondec.F
!||--- called by ------------------------------------------------------
!||    aconve      ../engine/source/ale/aconve.F
!||    intti12a    ../engine/source/interfaces/interf/intti12.F
!||    seggetv     ../engine/source/interfaces/interf/seggetv.F
!||--- uses       -----------------------------------------------------
!||    ale_mod     ../common_source/modules/ale/ale_mod.F
!||====================================================================
      SUBROUTINE VARCONDEC(ICODV,VARCONV,CODTOT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODV(ALE%GLOBAL%LCONV),VARCONV,CODTOT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,PDIX,CODVR 
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      CODTOT=0
      PDIX=10**(ALE%GLOBAL%LCONV-1)
      CODVR=VARCONV
      DO I=ALE%GLOBAL%LCONV,1,-1
         ICODV(I)=CODVR/PDIX
         CODVR=CODVR-ICODV(I)*PDIX
         PDIX=PDIX/10
         IF(ICODV(I)/=0)CODTOT=CODTOT+1
      ENDDO
C-----------------------------------------------      
      RETURN
C-----------------------------------------------      
      END
      
              
      
