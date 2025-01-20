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
      !||    spmd_xv_inter_type1_mod   ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
      !||--- called by ------------------------------------------------------
      !||    resol                     ../engine/source/engine/resol.F
      !||    resol_init                ../engine/source/engine/resol_init.F
      !||====================================================================
      module spmd_xv_inter_type1_mod

        INTEGER :: IS_PRESENT_INTER1  ! -1 : not yet defined
                                      !  0 : false
                                      !  1 : true
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      !||====================================================================
      !||    spmd_xv_inter_type1        ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
      !||--- called by ------------------------------------------------------
      !||    resol                      ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    alloc_my_real_2d_array     ../common_source/modules/array_mod.F
      !||    dealloc_my_real_2d_array   ../common_source/modules/array_mod.F
      !||    spmd_wait                  ../engine/source/mpi/spmd_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    array_mod                  ../common_source/modules/array_mod.F
      !||    constant_mod               ../common_source/modules/constant_mod.F
      !||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_xv_inter_type1(numnod,SFR_ELEM,ispmd,nspmd,iad_elem,fr_elem,a,v,ninter,ipari,npari)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use array_mod , only : array_type, alloc_my_real_2D_array, dealloc_my_real_2D_array
          use spmd_mod
          use constant_mod , only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent(in) :: nspmd
        integer, intent(in) :: ispmd
        integer,intent(in) :: numnod 
        integer,intent(in) :: SFR_ELEM      
        my_real, dimension(3,numnod), intent(inout) :: a,v
        INTEGER, DIMENSION(SFR_ELEM), INTENT(in) :: FR_ELEM    !< frontier node id 
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM  !< adress for frontier node
        INTEGER,INTENT(IN) :: NPARI, NINTER
        INTEGER,INTENT(IN) :: IPARI(NPARI,NINTER)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer, parameter :: my_tag = 18001
        integer :: my_request
        integer, dimension(nspmd) :: my_request_0
#ifdef MPI        
        integer, dimension(MPI_STATUS_SIZE) :: my_status
#endif
        type(array_type) :: rcv_buff
        type(array_type), dimension(nspmd) :: send_buff
        
        integer my_size, my_size_0(nspmd), array_size  !<
        integer k,j !< loop
        integer ityp !< interface type
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Pre-Condition
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
      if(is_present_inter1 == 0)return  !no /inter/type1 in input file
      !initialize 'is_present_inter1'
      if(is_present_inter1 == -1)then
        is_present_inter1 = 0
        do k=1,ninter
          ityp = ipari(7,k)
          if(ityp == 1)then
            is_present_inter1 = 1
            exit
          end if
        enddo
      end if
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        if(nspmd>1) then ! check the number of processor
          if(ispmd /= 0) then ! prepare the reception on secondary processor
         
            my_size = IAD_ELEM(1,2) - IAD_ELEM(1,1)
            if(my_size /= 0) then
              rcv_buff%size_my_real_array_2d(1) = 6
              rcv_buff%size_my_real_array_2d(2) = my_size
              call alloc_my_real_2D_array(rcv_buff) ! allocate the R buffer
              array_size = rcv_buff%size_my_real_array_2d(1) * rcv_buff%size_my_real_array_2d(2)
              call spmd_irecv(rcv_buff%my_real_array_2d(:,1),array_size,0,my_tag,my_request,SPMD_COMM_WORLD) ! post the R comm, sent by the processor 0
              call spmd_wait(my_request, my_status) ! wait the R comm, sent by the processor 0
              do k=1,my_size
                ! index pour noeuds frontieres appartement au interface /TYPE1
                a(1:3,FR_ELEM(IAD_ELEM(1,0+1)-1+k)) = rcv_buff%my_real_array_2d(1:3,k)
                v(1:3,FR_ELEM(IAD_ELEM(1,0+1)-1+k)) = rcv_buff%my_real_array_2d(4:6,k)
              enddo
              call dealloc_my_real_2D_array(rcv_buff) ! deallocate the R buffer
            endif

          else ! prepare the sending by main processor ("0")

            my_size_0(1) = 0
            do j=2,nspmd
              my_size_0(j) = IAD_ELEM(1,J+1) - IAD_ELEM(1,J)
              send_buff(j)%size_my_real_array_2d(1) = 6
              send_buff(j)%size_my_real_array_2d(2) = my_size_0(j)
              call alloc_my_real_2D_array(send_buff(j)) ! allocate the S buffer
              do k=1,my_size_0(j)
                ! index pour noeuds frontieres appartement au interface /TYPE1
                send_buff(j)%my_real_array_2d(1:3,k) = a(1:3, fr_elem(IAD_ELEM(1,J)+k-1)) ! save acceleration into the S buffer
                send_buff(j)%my_real_array_2d(4:6,k) = v(1:3, fr_elem(IAD_ELEM(1,J)+k-1)) ! save velocity into the S buffer
              enddo
              ! send the S buffer to processor "j"
              array_size = send_buff(j)%size_my_real_array_2d(1) * send_buff(j)%size_my_real_array_2d(2)
              if(my_size_0(j) /= 0) then
                call spmd_isend(send_buff(j)%my_real_array_2d(:,1),array_size,j-1,my_tag,my_request_0(j),SPMD_COMM_WORLD)
              endif
            enddo

            do j=2,nspmd
              if(my_size_0(j) /= 0) then
                call spmd_wait(my_request_0(j), my_status)  ! wait the S comm for the processor "j"
                call dealloc_my_real_2D_array(send_buff(j)) ! allocate the S buffer
              endif
            enddo

          endif
        endif
#endif
        return
        end subroutine spmd_xv_inter_type1
      end module spmd_xv_inter_type1_mod
