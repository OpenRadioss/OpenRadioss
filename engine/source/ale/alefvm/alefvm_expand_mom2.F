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
Chd|  ALEFVM_EXPAND_MOM2            source/ale/alefvm/alefvm_expand_mom2.F
Chd|-- called by -----------
Chd|        ALEFVM_SCHEME                 source/ale/alefvm/alefvm_scheme.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_EXPAND_MOM2 (
     1                               X  , V   ,   IXS, NV46, 
     2                               MOM, VEUL, ITASK, NALE ,  NEL )
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
#include      "alefvm.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines expand cell momentum to 
C nodes for post-treatment purpose only
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*), NV46, ITASK,NALE(*),NEL
      my_real :: MOM(NEL,3), X(3,*), V(3,*), VEUL(LVEUL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IV, J, K, INOD, IBORDER(1:6), NF_BORDER, NF_INTERNAL, ICF(4,6)
      INTEGER :: IN(8) , NnodALE
      INTEGER :: NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),NC5(MVSIZ),NC6(MVSIZ),NC7(MVSIZ),NC8(MVSIZ)
      my_real :: Qf(3,6), Qmean(3), dq(3), qn, N(3,6), Norm, dq_add(3)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   Qnode(3,8,MVSIZ),Qface(3,6,MVSIZ)

      DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(IALEFVM==0)    RETURN    
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      !-------------------------------------------------------------!
      ! EXPANDING CELL VALUE TO FACES AND NODES                     !
      !   SPECIFIC TREATMENT IS DONE TO HAVE WALL BCS WITH NODES    !
      !-------------------------------------------------------------!
      DO I=LFT,LLT
        !!---DETECT BORDER FACES---!
        !IBORDER(1:6) = 0 !0=not a border face. 1:otherwise
        !DO J=1,NV46
        !  IV = IVOIS(J,II)
        !  IF(IV==0)IBORDER(J) = 1
        !ENDDO
        !NF_BORDER    = SUM(IBORDER(:))   !number of border faces
        !NF_INTERNAL  = NV46 - NF_BORDER  !number of internal face
        !IF(NF_INTERNAL==0)CYCLE
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
          VERTEX(1,IN(K)) = VERTEX(1,IN(K)) + MOM(I,1)/EIGHT 
          VERTEX(2,IN(K)) = VERTEX(2,IN(K)) + MOM(I,2)/EIGHT
          VERTEX(3,IN(K)) = VERTEX(3,IN(K)) + MOM(I,3)/EIGHT		   
          VERTEX(4,IN(K)) = VERTEX(4,IN(K)) + ONE
        ENDDO
#include "lockoff.inc"
      ENDDO        
        
      RETURN
      END
