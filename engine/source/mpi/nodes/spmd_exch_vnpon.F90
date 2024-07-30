!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    spmd_exch_vnpon_mod   ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
      !||--- called by ------------------------------------------------------
      !||    inter_sh_offset_ini   ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
      !||    offset_nproj          ../engine/source/interfaces/shell_offset/offset_nproj.F90
      !||====================================================================
    module spmd_exch_vnpon_mod
        contains
!=======================================================================================================================
!!\brief This subroutine do nodal exchange vn6 in P/ON; ndim1=6*3,ndim2=numnod for vn6->nodal normal
!=======================================================================================================================
      !||====================================================================
      !||    spmd_exch_vnpon       ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
      !||--- called by ------------------------------------------------------
      !||    inter_sh_offset_ini   ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
      !||    offset_nproj          ../engine/source/interfaces/shell_offset/offset_nproj.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      subroutine spmd_exch_vnpon(ndim1,ndim2,vn6,iad_elem,fr_elem,nspmd,lenr )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                           :: nspmd            !< number of spmd domain
          integer, intent (in   )                           :: lenr             !< toal number of front node
          integer, intent (in   )                           :: ndim1            !< 1er dim of vn6
          integer, intent (in   )                           :: ndim2            !< 2nd dim of vn6
          integer, intent (in   ) ,dimension(2,nspmd+1)     :: iad_elem         !< index array for comm
          integer, intent (in   ) ,dimension(lenr)          :: fr_elem          !< front node arry
      double precision,intent (inout),dimension(ndim1,ndim2):: vn6              !< exchange nodal array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
      integer msgtyp,i,nod,loc_proc,ierror,msgoff,                     &
              siz,j,l,nb_nod,siz6,len,                                 &
              status(mpi_status_size),                                 &
              iad_send(nspmd+1),                                       &
              iad_recv(nspmd+1),                                       &
              req_r(nspmd),req_s(nspmd)
      data msgoff/231/

      double precision, dimension(:,:),allocatable :: rbuf, sbuf 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      allocate(rbuf(ndim1,lenr))
      allocate(sbuf(ndim1,lenr))

      siz6 = ndim1

      l = 1
      iad_recv(1)  = 1

      do i=1,nspmd
        len = iad_elem(1,i+1)-iad_elem(1,i)
        siz = siz6 *  len
        if(siz/=0)then
          msgtyp = msgoff
          call mpi_irecv(                               &
            rbuf(1,l),siz,mpi_double_precision,         &
            it_spmd(i),msgtyp,mpi_comm_world,           &
            req_r(i),ierror)
          l = l  + len
        endif
        iad_recv(i+1)  = l
      end do

      l  = 1
      iad_send(1)  = l

      do i=1,nspmd
        nb_nod = iad_elem(1,i+1)-iad_elem(1,i)
#include      "vectorize.inc"
        do j=iad_elem(1,i),iad_elem(1,i+1)-1
          nod = fr_elem(j)
          sbuf(1:ndim1, l)   =  vn6(1:ndim1,nod)
          l  = l  + 1
        end do
        iad_send(i+1)  = l
      enddo
!
!   echange messages
!
      do i=1,nspmd
! send
       nb_nod = iad_elem(1,i+1)-iad_elem(1,i)
       if(nb_nod>0)then
          msgtyp = msgoff 
          len = iad_elem(1,i+1)-iad_elem(1,i)

          siz = len *  siz6       
          l = iad_send(i)
          call mpi_isend(                               &
            sbuf(1,l),siz,mpi_double_precision,         &
            it_spmd(i),msgtyp,mpi_comm_world,           &
            req_s(i),ierror)
       endif
!
      enddo
!
! assemblage
!
      do i = 1, nspmd
        nb_nod = iad_elem(1,i+1)-iad_elem(1,i)
        if(nb_nod>0)then
          call mpi_wait(req_r(i),status,ierror)
          l  = iad_recv(i)

#include        "vectorize.inc"
          do j=iad_elem(1,i),iad_elem(1,i+1)-1
            nod = fr_elem(j)
            vn6(1:ndim1,nod) = vn6(1:ndim1,nod) + rbuf(1:ndim1,l)
            l  = l  + 1
          end do
        endif
      end do
!
!   wait for isend
!
      do i = 1, nspmd
        if(iad_elem(1,i+1)-iad_elem(1,i)>0)then
          call mpi_wait(req_s(i),status,ierror)
        endif
      enddo
!
#endif
      return
      end subroutine spmd_exch_vnpon
    end module spmd_exch_vnpon_mod
!    
