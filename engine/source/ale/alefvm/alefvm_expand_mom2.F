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
!||    alefvm_expand_mom2   ../engine/source/ale/alefvm/alefvm_expand_mom2.F
!||--- called by ------------------------------------------------------
!||    alefvm_scheme        ../engine/source/ale/alefvm/alefvm_scheme.F
!||--- uses       -----------------------------------------------------
!||    alefvm_mod           ../common_source/modules/ale/alefvm_mod.F
!||    element_mod          ../common_source/modules/elements/element_mod.F90
!||====================================================================
      SUBROUTINE ALEFVM_EXPAND_MOM2 ( IXS, MOM, NEL )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALEFVM_MOD
      use element_mod , only : nixs
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
#include      "param_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines expand cell momentum to 
C nodes for post-treatment purpose only
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*), NEL
      my_real :: MOM(NEL,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, K, ICF(4, 6)
      INTEGER :: IN(8)

      DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(ALEFVM_Param%IEnabled==0)    RETURN    
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      !-------------------------------------------------------------!
      ! EXPANDING CELL VALUE TO FACES AND NODES                     !
      !   SPECIFIC TREATMENT IS DONE TO HAVE WALL BCS WITH NODES    !
      !-------------------------------------------------------------!
      DO I=1,NEL
        II                = I + NFT
        IN(1)             = IXS(2,II)        
        IN(2)             = IXS(3,II)        
        IN(3)             = IXS(4,II)        
        IN(4)             = IXS(5,II)        
        IN(5)             = IXS(6,II)        
        IN(6)             = IXS(7,II)        
        IN(7)             = IXS(8,II)        
        IN(8)             = IXS(9,II)
#include "lockon.inc"         
        DO K=1,8
          ALEFVM_Buffer%VERTEX(1,IN(K)) = ALEFVM_Buffer%VERTEX(1,IN(K)) + MOM(I,1)/EIGHT 
          ALEFVM_Buffer%VERTEX(2,IN(K)) = ALEFVM_Buffer%VERTEX(2,IN(K)) + MOM(I,2)/EIGHT
          ALEFVM_Buffer%VERTEX(3,IN(K)) = ALEFVM_Buffer%VERTEX(3,IN(K)) + MOM(I,3)/EIGHT       
          ALEFVM_Buffer%VERTEX(4,IN(K)) = ALEFVM_Buffer%VERTEX(4,IN(K)) + ONE
        ENDDO
#include "lockoff.inc"
      ENDDO        
        
      RETURN
      END
