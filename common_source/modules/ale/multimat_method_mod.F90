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
      module multimat_method_mod

         use precision_mod , only : WP
       
         type multimat_method_list
           integer mmale
           integer surf_id
           integer surf_id2
           integer surf_id3
           integer surf_id4
           integer surf_id5
         end type
  
         type multimat_method_struct
           integer is_defined_mmale1  ! 0.or.1  "mmale=1: Staggered Upwind advection + von Neumann-Richtmyer artificial viscosity + diffuse interface"  <=> (law51)
           integer is_defined_mmale2  ! 0.or.1  "mmale=2: Collocated HLLC + diffuse interface"                                                          <=> MULTI_FVM%IS_USED (law151)
           integer is_defined_mmale3  ! 0.or.1  "mmale=3: Collocated HLLC + lagrangian interface tracking"                                              <=> (2d cutcell bimat)
           integer nb ! number of /ALE/SOLVER/MULTIMAT defined
           real(kind=WP) :: gamma(21) = 0.
           type(multimat_method_list), allocatable, dimension(:) :: list
             contains
               procedure :: destruct => multimat_method_destructor
               procedure :: construct => multimat_method_constructor
               procedure :: write_to_restart => multimat_method_write_to_restart
               procedure :: read_from_restart => multimat_method_read_from_restart
         end type

      contains
      
          
        !constructor
        subroutine multimat_method_constructor(this)
          implicit none
          class(multimat_method_struct) ,intent(inout) :: this
          if(allocated(this%list)) deallocate(this%list)
          allocate(this%list(this%nb))
        end subroutine multimat_method_constructor          
      
        !destructor
        subroutine multimat_method_destructor(this)
          implicit none
          class(multimat_method_struct) ,intent(inout) :: this
          if(allocated(this%list)) deallocate(this%list)
        end subroutine multimat_method_destructor     
      
        !write -> RST
        subroutine multimat_method_write_to_restart(this)
          implicit none
          class(multimat_method_struct) ,intent(inout) :: this
          integer :: nfix
          integer :: i
          integer :: iad
          integer,dimension(:), allocatable :: IBUF
          real(kind=WP) :: RBUF(21)
          NFIX = 4+6*this%nb
          ALLOCATE (IBUF(NFIX))
          IAD = 1
          IBUF(IAD) = this%is_defined_mmale1
          IAD = IAD+1
          IBUF(IAD) = this%is_defined_mmale2
          IAD = IAD+1
          IBUF(IAD) = this%is_defined_mmale3
          IAD = IAD+1
          IBUF(IAD) = this%nb
          IAD = IAD+1
          do i=1,this%nb
            IBUF(IAD+0) = this%list(i)%mmale
            IBUF(IAD+1) = this%list(i)%surf_id
            IBUF(IAD+2) = this%list(i)%surf_id2
            IBUF(IAD+3) = this%list(i)%surf_id3
            IBUF(IAD+4) = this%list(i)%surf_id4
            IBUF(IAD+5) = this%list(i)%surf_id5
            IAD=IAD+6
          enddo
          CALL WRITE_I_C(IBUF,NFIX)
          DEALLOCATE(IBUF)
          DEALLOCATE(this%list)
          RBUF(1:21) = this%gamma(1:21)
          CALL WRITE_DB(RBUF,21)
        end subroutine multimat_method_write_to_restart        
      
        !read <- RST
        subroutine multimat_method_read_from_restart(this)
          implicit none
          class(multimat_method_struct) ,intent(inout) :: this
          integer :: i
          integer :: iad
          integer :: ILEN4(4)
          integer,allocatable,dimension(:) :: ILEN
          real(kind=WP) :: RBUF(21)
          call read_i_c(ILEN4, 4)
          this%is_defined_mmale1 = ILEN4(1)
          this%is_defined_mmale2 = ILEN4(2)
          this%is_defined_mmale3 = ILEN4(3)
          this%nb = ILEN4(4)
          ALLOCATE(ILEN(6*this%nb))
          call read_i_c(ILEN, 6*this%nb)
          IAD = 1
          allocate(this%list(this%nb))
          do i=1,this%nb
            this%list(i)%mmale   = ILEN(IAD+0)
            this%list(i)%surf_id = ILEN(IAD+1)
            this%list(i)%surf_id2 = ILEN(IAD+2)
            this%list(i)%surf_id3 = ILEN(IAD+3)
            this%list(i)%surf_id4 = ILEN(IAD+4)
            this%list(i)%surf_id5 = ILEN(IAD+5)
            IAD = IAD + 6
          enddo
          deallocate(ILEN)
          CALL READ_DB(RBUF,21)
          this%gamma(1:21) = RBUF(1:21)
        end subroutine multimat_method_read_from_restart          


      end module multimat_method_mod
