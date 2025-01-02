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
!|====================================================================
!|  brokmann_random          source/materials/fail/windshield_alter/brokmann_random.F
!|-- called by -----------
!|        updfail               ../starter/source/materials/updfail.F90
!|-- calls ---------------
!|====================================================================
      !||====================================================================
      !||    brokmann_random_mod   ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
      !||--- called by ------------------------------------------------------
      !||    updfail               ../starter/source/materials/updfail.F90
      !||====================================================================
      module brokmann_random_mod
      contains
! ========================================================================================
! \brief initializes random crack in /fail/alter following Ch.Brokmann extension
! \details
! ========================================================================================

      !||====================================================================
      !||    brokmann_random           ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
      !||--- called by ------------------------------------------------------
      !||    updfail                   ../starter/source/materials/updfail.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    stack_mod                 ../starter/share/modules1/stack_mod.F
      !||====================================================================
      subroutine brokmann_random(brokmann ,fail   ,                 &
                 nixc   ,ixc    ,nixtg  ,ixtg   ,numelc ,numeltg  , &
                 iworksh,stack  ,igeo   ,npropgi,numgeo )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use brokmann_random_def_mod
      use fail_param_mod
      use stack_mod
      use constant_mod ,only : zero,half,one,two,pi,em6,ep06
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!    D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer               ,intent(in)    :: numelc         !< total number of 4n shell elements
      integer               ,intent(in)    :: numeltg        !< total number of 3n shell elements
      integer               ,intent(in)    :: nixc           !< size of 4n shell connectivity table
      integer               ,intent(in)    :: nixtg          !< size of 3n shell connectivity table
      integer ,intent(in) :: numgeo                              !< total number of element properties
      integer ,intent(in) :: npropgi                             !< parameter size of numgeo
      integer ,dimension(npropgi,numgeo),intent(in) :: igeo      !< property parameter table
      integer ,dimension(nixc,numelc)   ,intent(in) :: ixc    !< 4n shell connectivity table
      integer ,dimension(nixtg,numeltg) ,intent(in) :: ixtg   !< 3n shell connectivity table
      integer ,dimension(3,numelc+numeltg),intent(in) :: iworksh !< 
      type (fail_param_)    ,intent(inout) :: fail           !< failure model data structure
      type (brokmann_)      ,intent(inout) :: brokmann       !< brokmann model structure
      type (stack_ply)                     :: stack              !< element stack structure
!-----------------------------------------------
!    L o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ipt,npt,isubstack,igtyp
      integer :: seed,i_seed
      integer :: nshell,nshell_3n,nshell_4n,nix
      integer :: imat,pid,pidlay,ippid
      my_real :: randp
      integer ,dimension(:) ,allocatable :: nixel,elmat
      integer ,dimension(:) ,allocatable :: a_seed
!=======================================================================
      seed     = fail%iparam(1)
      if (seed == 0) then
        call random_seed()
      else
        i_seed = 1
        call random_seed(size=i_seed)
        allocate(a_seed(1:i_seed))
        a_seed = seed
        call random_seed(put=a_seed)
        deallocate(a_seed)
      end if
!
      allocate (elmat(numelc+numeltg))
      allocate (nixel(numelc+numeltg))
      nixel(:)  = 0

      ! create list of shell elements with material law
      nshell = 0
      do i = 1,numelc
        pid   = ixc(nixc-1,i)
        igtyp = igeo(11,pid)
        if (igtyp == 11) then
          npt = igeo(4,pid)
          do ipt = 1,npt
            imat = igeo(100+ipt,pid)
            if (imat == brokmann%imat) then
              nshell = nshell + 1
              nixel(nshell) = 4
              elmat(nshell) = i
            end if
          end do
        else if (igtyp == 17 .or. igtyp == 51 .or. igtyp == 52) then
          npt       = iworksh(1,i)
          isubstack = iworksh(3,i)
          ippid     = 2
          do ipt = 1,npt
            pidlay = stack%igeo(ippid + ipt, isubstack)
            imat   = igeo(101,pidlay)
            if (imat == brokmann%imat) then
              nshell = nshell + 1
              nixel(nshell) = 4
              elmat(nshell) = i
            end if
          end do
        else      ! igtyp = 1
          imat = ixc(1,i)
          if (imat == brokmann%imat) then
            nshell = nshell + 1
            nixel(nshell) = 4
            elmat(nshell) = i
          end if
        end if
      end do
!
      nshell_4n = nshell
!
      ! complete list with 3n shell elements
      do i = 1,numeltg
        pid   = ixtg(nixtg-1,i)
        igtyp = igeo(11,pid)
        if (igtyp == 11) then
          npt    = igeo(4,pid)
          do ipt = 1,npt
            imat = igeo(100+ipt,pid)
            if (imat == brokmann%imat) then
              nshell = nshell + 1
              nixel(nshell) = 3
              elmat(nshell) = i
            end if
          end do
        else if (igtyp == 17 .or. igtyp == 51 .or. igtyp == 52) then
          npt       = iworksh(1,i)
          isubstack = iworksh(3,i)
          ippid     = 2
          do ipt = 1,npt
            pidlay = stack%igeo(ippid + ipt, isubstack)
            imat   = igeo(101,pidlay)
            if (imat == brokmann%imat) then
              nshell = nshell + 1
              nixel(nshell) = 3
              elmat(nshell) = i
            end if
          end do
        else      ! igtyp = 1
          imat = ixtg(1,i)
          if (imat == brokmann%imat) then
            nshell = nshell + 1
            nixel(nshell) = 3
            elmat(nshell) = i
          end if
        end if
      end do
      nshell_3n = nshell - nshell_4n

      ! initialize brokmann element structure

      brokmann%nelem = nshell
      allocate (brokmann%brokmann_elem(nshell))
      do i = 1,nshell
        brokmann%brokmann_elem(i)%elnum = elmat(i)
        nix = nixel(i)
        brokmann%brokmann_elem(i)%nix    = nix
        if (nix == 4) then
          brokmann%brokmann_elem(i)%id =ixc(nixc,i)
        else
          brokmann%brokmann_elem(i)%id =ixtg(nixtg,i-nshell_4n)
        end if
       !  generate and save random numbers by element
        call random_number(randp)
        brokmann%brokmann_elem(i)%random(1) = randp
        call random_number(randp)
        brokmann%brokmann_elem(i)%random(2) = randp
        call random_number(randp)
        brokmann%brokmann_elem(i)%random(3) = randp
        call random_number(randp)
        brokmann%brokmann_elem(i)%random(4) = randp
        call random_number(randp)
        brokmann%brokmann_elem(i)%random(5) = randp
        call random_number(randp)
        brokmann%brokmann_elem(i)%random(6) = randp
      end do
!
      deallocate(nixel)      
      deallocate(elmat)      
!-----------
      return
      end subroutine brokmann_random
!
      end module brokmann_random_mod
