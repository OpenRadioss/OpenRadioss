!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!Chd|====================================================================
!Chd|  brokmann_elem_renum          source/materials/fail/brokmann/brokmann_elem_renum.F
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|====================================================================
      !||====================================================================
      !||    brokmann_elem_renum_mod   ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                    ../starter/source/starter/lectur.F
      !||====================================================================
      module brokmann_elem_renum_mod
      contains
! ========================================================================================
! \brief renumber local element numbers in randomd element list after domain decomposition
! \details 

      !||====================================================================
      !||    brokmann_elem_renum       ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                    ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||    reorder_mod               ../starter/share/modules1/reorder_mod.F
      !||====================================================================
      subroutine brokmann_elem_renum(fail_brokmann,numelc,numeltg)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use brokmann_random_def_mod
      use constant_mod ,only : zero,one
      use reorder_mod
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      type (fail_brokmann_) ,intent(inout) :: fail_brokmann !< brokmann model structure
      integer ,intent(in)                  :: numelc        !< total number of 4n shell elements
      integer ,intent(in)                  :: numeltg       !< total number of 3n shell elements
!-----------------------------------------------
!     l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ifail,idebug
      integer :: iel,iel_old,nix,id,nelem
      integer ,dimension(:)   ,allocatable :: tag_elem
      integer ,dimension(:)   ,allocatable :: tag_id
      integer ,dimension(:)   ,allocatable :: tag_nix
      my_real ,dimension(:,:) ,allocatable :: tag_rand
!=======================================================================
      idebug = 0
!
      do ifail = 1,fail_brokmann%nfail
        nelem = fail_brokmann%brokmann(ifail)%nelem
        allocate(tag_elem(nelem))
        allocate(tag_nix(nelem))
        allocate(tag_id(nelem))
        allocate(tag_rand(nelem,6))
        tag_rand(:,:) = zero
        tag_elem(:) = 0
        tag_nix(:)  = 0
        tag_id(:)   = 0
        do i=1,nelem
          id  = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%id
          iel_old = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%elnum
          nix = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%nix
          if (nix == 4) then
            iel = permutation%shell(numelc + iel_old)
            tag_elem(i) = iel 
            tag_id(i)   = id
            tag_nix(i)  = nix
            tag_rand(i,1) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(1)
            tag_rand(i,2) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(2)
            tag_rand(i,3) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(3)
            tag_rand(i,4) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(4)
            tag_rand(i,5) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(5)
            tag_rand(i,6) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(6)
          else if (nix == 3) then
            iel = permutation%shell(numeltg + iel_old)
            tag_elem(i) = iel 
            tag_id(i)   = id
            tag_nix(i)  = nix
            tag_rand(i,1) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(1)
            tag_rand(i,2) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(2)
            tag_rand(i,3) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(3)
            tag_rand(i,4) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(4)
            tag_rand(i,5) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(5)
            tag_rand(i,6) = fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(6)
          end if
        end do
!        
        if (idebug == 1) then
          do i=1,nelem
            write(*,'(A,3I10,F18.6)') 'id,old_n,new_n=',tag_id(i),   &
                       fail_brokmann%brokmann(ifail)%brokmann_elem(i)%elnum,tag_elem(i),&
                       tag_rand(i,1)
          end do
        end if
!        
        do i=1,nelem
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%id     = tag_id(i)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%elnum  = tag_elem(i)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%nix    = tag_nix(i)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(1) = tag_rand(i,1)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(2) = tag_rand(i,2)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(3) = tag_rand(i,3)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(4) = tag_rand(i,4)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(5) = tag_rand(i,5)
          fail_brokmann%brokmann(ifail)%brokmann_elem(i)%random(6) = tag_rand(i,6)
        end do
        deallocate(tag_rand)
        deallocate(tag_id)
        deallocate(tag_nix)
        deallocate(tag_elem)
      end do
!-----------
      return
      end subroutine brokmann_elem_renum
!
      end module brokmann_elem_renum_mod
