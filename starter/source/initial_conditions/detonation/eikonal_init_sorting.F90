      module eikonal_init_sorting_mod
      contains
        subroutine eikonal_init_sorting(neldet, numel, elem_list, uelem_list, idx_ng , idx_i, elem_list_bij, xel, vel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero, ep21
          use insertion_sort_mod , only : integer_insertion_sort_with_index
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: neldet,numel
          integer,intent(inout) :: elem_list(neldet)
          integer,intent(inout) :: uelem_list(neldet)
          integer,intent(inout) :: idx_ng(neldet)
          integer,intent(inout) :: idx_i(neldet)
          integer,intent(inout) :: elem_list_bij(1:numel)
          real(kind=WP),intent(inout) :: xel(3,neldet)
          real(kind=WP),intent(inout) :: vel(neldet)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer,allocatable,dimension(:) :: indx
          integer,allocatable,dimension(:) :: int_tmp_array
          real(kind=WP),allocatable,dimension(:) :: real_tmp_array
          integer :: kk
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(int_tmp_array(neldet))
          allocate(real_tmp_array(neldet))
          allocate(indx(neldet))

            indx(1:neldet) = [(kk, kk=1,neldet)]
            call integer_insertion_sort_with_index(uelem_list, indx, neldet)
            
            !sort other arrays with same order usin indx array
            int_tmp_array(:)  = elem_list(:)     ; elem_list(:)    = int_tmp_array(indx(:))
            int_tmp_array(:)  = idx_ng(:)        ; idx_ng(:)       = int_tmp_array(indx(:))
            int_tmp_array(:)  = idx_i(:)         ; idx_i(:)        = int_tmp_array(indx(:))
            real_tmp_array(:) = xel(1,:)         ; xel(1,:)        = real_tmp_array(indx(:))
            real_tmp_array(:) = xel(2,:)         ; xel(2,:)        = real_tmp_array(indx(:)) 
            real_tmp_array(:) = xel(3,:)         ; xel(3,:)        = real_tmp_array(indx(:)) 
            real_tmp_array(:) = vel(:)           ; vel(:)          = real_tmp_array(indx(:))

            ! sorting bijective array
            elem_list_bij(1:numel) = 0
            do kk=1,neldet
              elem_list_bij(elem_list(kk)) = kk
            end do

          deallocate(int_tmp_array)
          deallocate(real_tmp_array)
          deallocate(indx)

        end subroutine eikonal_init_sorting
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_init_sorting_mod


