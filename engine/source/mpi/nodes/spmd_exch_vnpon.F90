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
!||    spmd_exch_vnpon_mod   ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
!||--- called by ------------------------------------------------------
!||    inter_sh_offset_ini   ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
!||    offset_nproj          ../engine/source/interfaces/shell_offset/offset_nproj.F90
!||====================================================================
      module spmd_exch_vnpon_mod
      implicit none
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
!||    spmd_wait             ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine spmd_exch_vnpon(ndim1,ndim2,vn6,iad_offset,fr_offset,nspmd,lenr )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use spmd_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                           :: nspmd            !< number of spmd domain
          integer, intent (in   )                           :: lenr             !< toal number of front node
          integer, intent (in   )                           :: ndim1            !< 1er dim of vn6
          integer, intent (in   )                           :: ndim2            !< 2nd dim of vn6
          integer, intent (in   ) ,dimension(2,nspmd+1)     :: iad_offset       !< index array for comm
          integer, intent (in   ) ,dimension(lenr)          :: fr_offset        !< front node array
          double precision,intent (inout),dimension(ndim1,ndim2):: vn6              !< exchange nodal array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: msgtyp,i,nod,                                   &
            siz,j,l,nb_nod,siz6,len,                                 &
            iad_send(nspmd+1),                                       &
            iad_recv(nspmd+1),                                       &
            req_r(nspmd),req_s(nspmd)
          integer, parameter :: msgoff = 231

          double precision, dimension(:,:),allocatable :: rbuf, sbuf
          integer :: it_spmd
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

#ifndef MPI
          return
#endif
          allocate(rbuf(ndim1,lenr))
          allocate(sbuf(ndim1,lenr))

          siz6 = ndim1

          l = 1
          iad_recv(1)  = 1

          do i=1,nspmd
            it_spmd = i - 1
            len = iad_offset(1,i+1)-iad_offset(1,i)
            siz = siz6 *  len
            if(siz/=0)then
              msgtyp = msgoff
              call spmd_irecv(                               &
                rbuf(1,l),siz,         &
                it_spmd,msgtyp,           &
                req_r(i))
              l = l  + len
            end if
            iad_recv(i+1)  = l
          end do

          l  = 1
          iad_send(1)  = l

          do i=1,nspmd
            nb_nod = iad_offset(1,i+1)-iad_offset(1,i)
            do j=iad_offset(1,i),iad_offset(1,i+1)-1
              nod = fr_offset(j)
              if(nod > 0) then
                sbuf(1:ndim1, l)   =  vn6(1:ndim1,nod)
              else
                sbuf(1:ndim1, l)   =  0.0D0
              end if
              l  = l  + 1
            end do
            iad_send(i+1)  = l
          end do
!
!   exchange messages
!
          do i=1,nspmd
            it_spmd = i - 1
! send
            nb_nod = iad_offset(1,i+1)-iad_offset(1,i)
            if(nb_nod>0)then
              msgtyp = msgoff
              len = iad_offset(1,i+1)-iad_offset(1,i)

              siz = len *  siz6
              l = iad_send(i)
              call spmd_isend(                               &
                sbuf(1,l),siz,         &
                it_spmd,msgtyp,           &
                req_s(i))
            end if
!
          end do
!
! assemblage
!
          do i = 1, nspmd
            nb_nod = iad_offset(1,i+1)-iad_offset(1,i)
            if(nb_nod>0)then
              call spmd_wait(req_r(i))
              l  = iad_recv(i)
              do j=iad_offset(1,i),iad_offset(1,i+1)-1
                nod = fr_offset(j)
                vn6(1:ndim1,nod) = vn6(1:ndim1,nod) + rbuf(1:ndim1,l)
                l  = l  + 1
              end do
            end if
          end do
!
!   wait for isend
!
          do i = 1, nspmd
            if(iad_offset(1,i+1)-iad_offset(1,i)>0)then
              call spmd_wait(req_s(i))
            end if
          end do
          return
        end subroutine spmd_exch_vnpon
      end module spmd_exch_vnpon_mod
