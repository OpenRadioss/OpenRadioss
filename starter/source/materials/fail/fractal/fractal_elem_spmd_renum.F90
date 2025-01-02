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
!Chd|  fractal_elem_renum          source/materials/fail/fractal/fractal_elem_renum.F
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|====================================================================
      !||====================================================================
      !||    fractal_elem_renum_mod   ../starter/source/materials/fail/fractal/fractal_elem_spmd_renum.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                   ../starter/source/starter/lectur.F
      !||====================================================================
      module fractal_elem_renum_mod
      contains
! ========================================================================================
! \brief renumber local element numbers in damaged element list after domain decomposition
! \details 

      !||====================================================================
      !||    fractal_elem_renum    ../starter/source/materials/fail/fractal/fractal_elem_spmd_renum.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||    reorder_mod           ../starter/share/modules1/reorder_mod.F
      !||====================================================================
      subroutine fractal_elem_renum(fail_fractal,numelc,numeltg)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use random_walk_def_mod
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
      type (fail_fractal_) ,intent(inout) :: fail_fractal !< fractal model structure
      integer ,intent(in)                 :: numelc       !< total number of 4n shell elements
      integer ,intent(in)                 :: numeltg      !< total number of 3n shell elements
!-----------------------------------------------
!     l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ii,ifract
      integer :: iel,iel_old,nix,id,nelem
      integer ,dimension(:) ,allocatable :: tag_elem
      integer ,dimension(:) ,allocatable :: tag_id
      integer ,dimension(:) ,allocatable :: tag_nix
      my_real ,dimension(:) ,allocatable :: tag_dmg
!=======================================================================
      do ifract = 1,fail_fractal%nfail
        nelem = fail_fractal%fractal(ifract)%nelem
        allocate(tag_elem(nelem))
        allocate(tag_nix(nelem))
        allocate(tag_dmg(nelem))
        allocate(tag_id(nelem))
        tag_dmg(:)  = zero
        tag_elem(:) = 0
        tag_nix(:)  = 0
        tag_id(:)   = 0
        ii = 0
        do i=1,nelem
          if (fail_fractal%fractal(ifract)%random_walk(i)%damage > zero) then
            ii = ii + 1
            id  = fail_fractal%fractal(ifract)%random_walk(i)%id
            iel_old = fail_fractal%fractal(ifract)%random_walk(i)%elnum
            nix = fail_fractal%fractal(ifract)%random_walk(i)%nix
            if (nix == 4) then
              iel = permutation%shell(numelc + iel_old)
              tag_elem(ii) = iel 
              tag_id(ii)   = id
              tag_nix(ii)  = nix
              tag_dmg(ii)  = fail_fractal%fractal(ifract)%random_walk(i)%damage
            else if (nix == 3) then
              iel = permutation%shell(numeltg + iel_old)
              tag_elem(ii) = iel 
              tag_id(ii)   = id
              tag_nix(ii)  = nix
              tag_dmg(ii)  = fail_fractal%fractal(ifract)%random_walk(i)%damage
            end if
          end if
        end do
!        
        nelem = ii
        fail_fractal%fractal(ifract)%nelem = nelem
        do i=1,nelem
          fail_fractal%fractal(ifract)%random_walk(i)%damage = tag_dmg(i)
          fail_fractal%fractal(ifract)%random_walk(i)%id     = tag_id(i)
          fail_fractal%fractal(ifract)%random_walk(i)%elnum  = tag_elem(i)
          fail_fractal%fractal(ifract)%random_walk(i)%nix    = tag_nix(i)
        end do
        deallocate(tag_id)
        deallocate(tag_dmg)
        deallocate(tag_nix)
        deallocate(tag_elem)
      end do
!-----------
      return
      end subroutine fractal_elem_renum
!
      end module fractal_elem_renum_mod
