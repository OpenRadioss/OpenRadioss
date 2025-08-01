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
!||====================================================================
!||    random_walk_dmg_mod   ../starter/source/materials/fail/fractal/random_walk_dmg.F90
!||--- called by ------------------------------------------------------
!||    updfail               ../starter/source/materials/updfail.F90
!||====================================================================
      module random_walk_dmg_mod
      contains
! ========================================================================================
! \brief fractal random walk algorithm for initializing damage in material law elements
!! \details

! ========================================================================================

!||====================================================================
!||    random_walk_dmg                ../starter/source/materials/fail/fractal/random_walk_dmg.F90
!||--- called by ------------------------------------------------------
!||    updfail                        ../starter/source/materials/updfail.F90
!||--- calls      -----------------------------------------------------
!||    fractal_element_neighbor       ../starter/source/materials/fail/fractal/fractal_element_neighbor.F90
!||    ngr2usrn                       ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    fractal_element_neighbor_mod   ../starter/source/materials/fail/fractal/fractal_element_neighbor.F90
!||    message_mod                    ../starter/share/message_module/message_mod.F
!||    stack_mod                      ../starter/share/modules1/stack_mod.F
!||====================================================================
        subroutine random_walk_dmg(fractal,fail ,                     &
          ngrshel,ngrsh3n,igrsh4n,igrsh3n,                   &
          nixc   ,ixc    ,nixtg  ,ixtg  ,numelc ,numeltg,    &
          iworksh,stack  ,igeo   ,npropgi,numgeo )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use fail_param_mod
          use message_mod
          use groupdef_mod
          use random_walk_def_mod
          use fractal_element_neighbor_mod
          use stack_mod
          use constant_mod ,only : zero,one
          use precision_mod, only : WP
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------


!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
          integer             ,intent(in)    :: numelc          !< total number of 4n shell elements
          integer             ,intent(in)    :: numeltg         !< total number of 3n shell elements
          integer             ,intent(in)    :: ngrshel         !< number of 4n shell element groups
          integer             ,intent(in)    :: ngrsh3n         !< number of 3n shell element groups
          integer             ,intent(in)    :: nixc            !< size of 4n shell connectivity table
          integer             ,intent(in)    :: nixtg           !< size of 3n shell connectivity table
          integer             ,intent(in)    :: numgeo          !< total number of element properties
          integer             ,intent(in)    :: npropgi         !< parameter size of numgeo
          integer ,dimension(npropgi,numgeo),intent(in) :: igeo !< property parameter table
          integer ,dimension(nixc,numelc)   ,intent(in) :: ixc  !< 4n shell connectivity table
          integer ,dimension(nixtg,numeltg) ,intent(in) :: ixtg !< 3n shell connectivity table
          integer ,dimension(3,numelc+numeltg),intent(in) :: iworksh !<
          type (group_)      ,intent(in) :: igrsh4n(ngrshel)    !< 4n shell group structure
          type (group_)      ,intent(in) :: igrsh3n(ngrsh3n)    !< 3n shell group structure
          type (fail_param_) ,intent(inout) :: fail             !< failure model data structure
          type (fractal_)    ,intent(inout) :: fractal          !< fractal model structure
          type (stack_ply)                  :: stack            !< element stack structure
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j,k,k_nxt,nix
          integer :: seed,i_seed
          integer :: n_rwalk,nstart,iwalk
          integer :: iel,count,stop_walk,prv,current,nxt
          integer :: curr_id,nxt_id,trgt_id
          integer :: igr4n_start,igr3n_start,igr4n_target,igr3n_target
          integer :: start_gsh4_id,start_gsh3_id,target_gsh4_id,target_gsh3_id,nb_sh3,nb_sh4
          integer ,dimension(6) :: nxtk4
          integer ,dimension(5) :: nxtk3
          integer ,dimension(:) ,allocatable :: start_group
          integer ,dimension(:) ,allocatable :: a_seed
          real(kind=WP) :: dmg,probability
          real(kind=WP) :: random_value
          logical :: random_start,random_target
          integer :: debug
          integer ,external :: ngr2usrn

          data nxtk4/2,3,4,1,2,3/
          data nxtk3/2,3,1,2,3/
!=======================================================================
          n_rwalk        = fail%iparam(1)
          seed           = fail%iparam(2)
          start_gsh4_id  = fail%iparam(3)
          start_gsh3_id  = fail%iparam(4)
          target_gsh4_id = fail%iparam(5)
          target_gsh3_id = fail%iparam(6)
          debug          = fail%iparam(7)
!
          dmg            = fail%uparam(1)
          probability    = fail%uparam(2)
!
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
!------------------------------------------------------------------------------------
!     create element - edge connectivity for all elements in material
!------------------------------------------------------------------------------------

          call fractal_element_neighbor(fractal,nixc  ,ixc  ,nixtg   ,ixtg,numelc,numeltg  ,  &
            iworksh,stack ,igeo ,npropgi ,numgeo )

!------------------------------------------------------------------------------------
!     build starting element list
!------------------------------------------------------------------------------------
          nb_sh4 = 0
          nb_sh3 = 0
          if (start_gsh4_id + start_gsh3_id == 0) then  ! empty starting element groups
            ! starting elements are chosen randomly for each random walker
            random_start = .true.
            nstart = fractal%nelem
            allocate (start_group(nstart))
            start_group(1:nstart) = fractal%random_walk(1:nstart)%elnum
          else
            random_start = .false.
            if (start_gsh4_id > 0) then
              igr4n_start = ngr2usrn(start_gsh4_id,igrsh4n,ngrshel,nb_sh4)
            end if
            if (start_gsh3_id > 0) then
              igr3n_start = ngr2usrn(start_gsh3_id,igrsh3n,ngrsh3n,nb_sh3)
            end if
!
            allocate (start_group(nb_sh3 + nb_sh4))
            count = 0
            do i=1,nb_sh4
              iel = igrsh4n(igr4n_start)%entity(i)
              do j = 1,fractal%nelem
                if (fractal%random_walk(j)%nix == 4) then
                  if (fractal%random_walk(j)%elnum == iel) then
                    count = count + 1
                    start_group(count) = iel
                  end if
                end if
              end do
            end do
!
            do i=1,nb_sh3
              iel = igrsh3n(igr3n_start)%entity(i)
              do j = 1,fractal%nelem
                if (fractal%random_walk(j)%nix == 3) then
                  if (fractal%random_walk(j)%elnum == iel) then
                    count = count + 1
                    start_group(count) = iel
                  end if
                end if
              end do
            end do
!
            nstart = count
!
          end if      ! random start
          !------------------------------
          if (debug == 1) then
            if (random_start .eqv. .false.) then
              print*,'starting element list'
              do i = 1,nstart
                iel = start_group(i)
                print*,'    ',fractal%random_walk(iel)%id
              end do
            else
              print*,'random starting element for each walker'
            end if
          end if
!------------------------------------------------------------------------------------
!     initialize damage in target element list
!------------------------------------------------------------------------------------
          nb_sh4 = 0
          nb_sh3 = 0
!
          if ((debug == 1)) then
            print*,' '
            print*,'target element list'
          end if
          if (target_gsh4_id + target_gsh3_id == 0) then  ! empty target element groups
            random_target = .true.
            call random_number(random_value)
            iel = ceiling(random_value * fractal%nelem)
            fractal%random_walk(iel)%damage = dmg
            if (debug==1) print*,'    ',fractal%random_walk(iel)%id
          else
            random_target = .false.
            igr4n_target = -HUGE(igr4n_target)
            if (target_gsh4_id > 0) then
              igr4n_target = ngr2usrn(target_gsh4_id,igrsh4n,ngrshel,nb_sh4)
            end if
            igr3n_target = -HUGE(igr3n_target)
            if (target_gsh3_id > 0) then
              igr3n_target = ngr2usrn(target_gsh3_id,igrsh3n,ngrsh3n,nb_sh3)
            end if
            !  initialize damage in target elements
            do i=1,nb_sh4
              iel = igrsh4n(igr4n_target)%entity(i)
              do j = 1,fractal%nelem
                if (fractal%random_walk(j)%nix == 4) then
                  if (fractal%random_walk(j)%elnum == iel) then
                    fractal%random_walk(j)%damage = dmg
                    if (debug==1) print*,'    ',fractal%random_walk(j)%id
                  end if
                end if
              end do
            end do
            do i=1,nb_sh3
              iel = igrsh3n(igr3n_target)%entity(i)
              do j = 1,fractal%nelem
                if (fractal%random_walk(j)%nix == 3) then
                  if (fractal%random_walk(j)%elnum == iel) then
                    fractal%random_walk(j)%damage = dmg
                    if (debug==1) print*,'    ',fractal%random_walk(j)%id
                  end if
                end if
              end do
            end do
          endif       ! random target
!------------------------------------------------------------------------------------
!     fractal random walks from starting elements to targets
!------------------------------------------------------------------------------------
          if (debug==1) then
            print*,' '
            print*,' begin random walk algorithm'
          end if
          !-------------------------------------
!
          do iwalk=1,n_rwalk
            stop_walk = 0
            call random_number(random_value)
            iel = ceiling(random_value * nstart)
            current = start_group(iel)
            ! skip already damaged elements => choose another starting point
            if (fractal%random_walk(current)%damage == zero) then
              if (debug==1) then
                print*,' '
                print*,' drunken walker number',iwalk,',start with',fractal%random_walk(current)%id
              end if
              !------------------------------------------------
              ! start random walk from current starting point
              !------------------------------------------------
              prv = 0
              do while (stop_walk == 0)
                call random_number(random_value)
                nix = fractal%random_walk(current)%nix
                k_nxt = ceiling(random_value * nix)
                nxt   = fractal%random_walk(current)%neighbor(k_nxt)
                ! check if any neighbor is damaged
                do k=1,nix
                  j = fractal%random_walk(current)%neighbor(k)
                  if (j > 0) then
                    if (fractal%random_walk(j)%damage > zero) then ! found damaged neighbor
                      call random_number(random_value)
                      if (random_value < probability) then         ! init damage in current element
                        fractal%random_walk(current)%damage = dmg
                        stop_walk = 1
                        if (debug==1) then
                          curr_id = fractal%random_walk(current)%id
                          trgt_id = fractal%random_walk(j)%id
                          print*,'     found target',trgt_id,',set damage to',curr_id
                        end if
                      end if
                    end if
                  end if
                end do
                ! continue if current element is not damaged
                if (stop_walk == 0) then
                  if (prv > 0 .and. nxt > 0) then          ! keep walking
                    do while (nxt == prv)                  ! check if not going back
                      call random_number(random_value)
                      k   = ceiling(random_value * nix)
                      nxt = fractal%random_walk(current)%neighbor(k)
                    end do
                  end if
                  if (nxt == 0) then          ! exit on border
                    stop_walk = 1
                    if (debug==1) print*,'     died on',fractal%random_walk(current)%id
                  else if (debug==1) then
                    curr_id = fractal%random_walk(current)%id
                    nxt_id  = fractal%random_walk(nxt)%id
                    print*,'   ',curr_id,' => ',nxt_id
                  end if
                  prv     = current
                  current = nxt
                end if
              end do
            end if
          end do
!
          if (allocated(start_group)) deallocate (start_group)
!-----------
          return
        end subroutine random_walk_dmg
!-----------
      end module random_walk_dmg_mod
