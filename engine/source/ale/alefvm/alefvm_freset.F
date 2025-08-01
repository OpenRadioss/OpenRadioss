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
!||    alefvm_freset    ../engine/source/ale/alefvm/alefvm_freset.F
!||--- called by ------------------------------------------------------
!||    sforc3           ../engine/source/elements/solid/solide/sforc3.F
!||--- uses       -----------------------------------------------------
!||    alefvm_mod       ../common_source/modules/ale/alefvm_mod.F
!||    i22bufbric_mod   ../common_source/modules/interfaces/cut-cell-search_mod.F
!||====================================================================
      SUBROUTINE ALEFVM_FRESET ( IAD22 )
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
      USE I22BUFBRIC_MOD
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
#include      "inter22.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines initializes cell forces (IALEFVM==1)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real :: IAD22(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IB, NIN
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(ALEFVM_Param%IEnabled==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      NIN = 1
      !-------------------------------------------------------------!
      ! FORCES INITIALIZATION                                       !
      !-------------------------------------------------------------!  
      IF(INT22==0)THEN    
        DO I=LFT,LLT
          II                         = I + NFT
          ALEFVM_Buffer%FCELL(1,II)  = ZERO
          ALEFVM_Buffer%FCELL(2,II)  = ZERO
          ALEFVM_Buffer%FCELL(3,II)  = ZERO      
        ENDDO!next I
      ELSE
        DO I=LFT,LLT
          II                        = I + NFT
          ALEFVM_Buffer%FCELL(1,II) = ZERO
          ALEFVM_Buffer%FCELL(2,II) = ZERO
          ALEFVM_Buffer%FCELL(3,II) = ZERO 
          IB                        = NINT(IAD22(I))
          IF(IB/=ZERO)THEN
            BRICK_LIST(NIN,IB)%FCELL(1:3) = ZERO
          ENDIF               
        ENDDO!next I             
      ENDIF
C-----------------------------------------------
      RETURN
      END
