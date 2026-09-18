!!\brief module of linked list of integers
module integer_LL_mod
  type integer_LL_
    integer(kind=8) :: val
    type(integer_LL_), pointer :: next
  end type integer_LL_    

  type ptr_to_integer_LL_
    type(integer_LL_), pointer :: ptr
  end type ptr_to_integer_LL_    

  contains
  subroutine integer_LL_new(list, val)
    implicit none

    type(integer_LL_), pointer :: list
    integer(kind=8), intent(in) :: val
    
    allocate(list)
    nullify(list%next)
    list%val =  val
  end subroutine integer_LL_new

  function integer_LL_insert_before(list, val) result(new_elem)
    implicit none

    type(integer_LL_), pointer :: list
    integer(kind=8), intent(in) :: val
    type(integer_LL_), pointer :: new_elem

    allocate(new_elem)
    new_elem%val = val
    new_elem%next => list
  end function integer_LL_insert_before

  subroutine integer_LL_insert_after(list, val)
    implicit none

    type(integer_LL_), pointer :: list
    integer(kind=8), intent(in) :: val

    type(integer_LL_), pointer :: new_elem
  
    if (.not. associated(list)) then 
      allocate(list)
      list%val = val
      nullify(list%next)
    else 
      allocate(new_elem)
      new_elem%val = val
      new_elem%next => list%next
      list%next => new_elem
    end if
  end subroutine integer_LL_insert_after

  subroutine integer_LL_insert_unique(list, val)
    implicit none

    type(integer_LL_), pointer :: list
    integer(kind=8), intent(in) :: val

    logical :: found
    type(integer_LL_), pointer :: curr_elem
  
    if (associated(list)) then 
      found = (list%val == val)
      curr_elem => list
      do while (associated(curr_elem%next) .and. .not. found)
        if (curr_elem%next%val == val) then
          found = .true.
        else
          curr_elem => curr_elem%next
        end if
      end do 

      if (.not. found) then
          call integer_LL_insert_after(curr_elem, val)
      end if
    else
      call integer_LL_new(list, val)
    end if
  end subroutine integer_LL_insert_unique

  function integer_LL_size(list) result(size)
    implicit none

    type(integer_LL_), pointer :: list
    integer(kind=8) :: size

    type(integer_LL_), pointer :: curr_elem
  
    size = 0
    curr_elem => list
    do while (associated(curr_elem))
      size = size + 1
      curr_elem => curr_elem%next
    end do 

  end function integer_LL_size

  function integer_LL_find_minimum(list) result(min_elem)
    implicit none

    type(integer_LL_), pointer :: list

    type(integer_LL_), pointer :: curr_elem
    type(integer_LL_), pointer :: min_elem
  
    min_elem => list
    curr_elem => list
    do while (associated(curr_elem))
      if (curr_elem%val < min_elem%val) then
        min_elem => curr_elem
      end if
      curr_elem => curr_elem%next
    end do 
  end function integer_LL_find_minimum

  subroutine integer_LL_copy(original, targ)
    implicit none
    type(integer_LL_), pointer :: original
    type(integer_LL_), pointer :: targ

    type(integer_LL_), pointer :: current_targ, current_original

    call integer_LL_destroy(targ)
    if (associated(original)) then
      allocate(targ)
      nullify(targ%next)
      targ%val = original%val
      current_original => original%next
      current_targ => targ
      do while ( associated(current_original) )
        call integer_LL_insert_after(current_targ, current_original%val)
        current_original => current_original%next
        current_targ => current_targ%next
      end do
      nullify(current_targ)
    else
      nullify(targ)
    end if
  end subroutine integer_LL_copy
  
  subroutine integer_LL_destroy( list )
    implicit none
    type(integer_LL_), pointer :: list
  
    type(integer_LL_), pointer  :: current
    type(integer_LL_), pointer  :: next

    if (associated(list)) then
      current => list
      do while ( associated(current%next))
        next => current%next
        deallocate( current )
        current => next
      enddo
      if ( associated(current)) then
        deallocate( current )
      endif
    end if
    nullify(list)
  end subroutine integer_LL_destroy

  subroutine integer_LL_delete_element( list, elem )
    implicit none

    type(integer_LL_), pointer :: list
    type(integer_LL_), pointer :: elem

    type(integer_LL_), pointer  :: current
    type(integer_LL_), pointer  :: prev

    if ( associated(list,elem) ) then
        list => elem%next
        deallocate( elem )
    else
        current => list
        prev    => list
        do while ( associated(current) )
            if ( associated(current,elem) ) then
                prev%next => current%next
                deallocate( current ) ! Is also "elem"
                exit
            endif
            prev    => current
            current => current%next
        enddo
    endif
  end subroutine integer_LL_delete_element  

  subroutine integer_LL_delete_value( list, val )
    implicit none 

    type(integer_LL_), pointer :: list
    integer(kind=8), intent(in)  :: val

    type(integer_LL_), pointer  :: current
    type(integer_LL_), pointer  :: prev

    if (associated(list)) then
      if ( list%val == val ) then
          current => list
          list => current%next
          deallocate( current )
      else
          current => list
          prev    => list
          do while ( associated(current) )
              if ( current%val == val ) then
                  prev%next => current%next
                  deallocate( current ) 
                  exit
              endif
              prev    => current
              current => current%next
          enddo
      endif
    endif
  end subroutine integer_LL_delete_value  

  function integer_LL_pop(list) result(val)
    implicit none

    type(integer_LL_), pointer :: list
    integer(kind=8) :: val

    type(integer_LL_), pointer :: curr_elem
  
    val = list%val
    curr_elem => list
    list => list%next
    deallocate(curr_elem)

  end function integer_LL_pop
end module integer_LL_mod