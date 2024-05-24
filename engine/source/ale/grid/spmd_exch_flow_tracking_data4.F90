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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief SPMD exchange necessary for option /ALE/GRID/MASS-WEIGHTED-VEL
!! \details  gathering  domain boundaries X_MIN_MAX (for main flow) and X_MIN_MAX_GRID (for ALE grid points)
!
      subroutine spmd_exch_flow_tracking_data4( domain_data, nspmd )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use spmd_mod
        use ale_mod , only : flow_tracking_data_
        use constant_mod , only: zero,ep20
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included file
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
#include "my_real.inc"
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(flow_tracking_data_),intent(inout)::domain_data !< intent(in) ale mass weighted velolcity data buffer for given domain
        integer,intent(in)::nspmd                       !< number of SPMD domains
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: msgtyp, msgoff, p, nbirecv
        integer :: req_sb(nspmd),irindexi(nspmd)
        integer :: loc_proc, isize
        my_real :: rbuf(12,nspmd)
        data msgoff/2205/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
        if(nspmd == 1)return
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!$OMP SINGLE
        loc_proc=ispmd+1
        rbuf(1:3,1:nspmd)=ep20
        rbuf(4:6,1:nspmd)=-ep20
        rbuf(7:9,1:nspmd)=ep20
        rbuf(10:12,1:nspmd)=-ep20
        rbuf(1:6,loc_proc) = domain_data%x_min_max(1:6)
        rbuf(7:12,loc_proc) = domain_data%x_min_max_grid(1:6)
        isize=12
        !-------------------------------------------!
        ! SENDING %X_MIN_MAX(1:6) & X_MIN_MAX(1:6)  !
        !-------------------------------------------!
        do p = 1, nspmd
          if(p /= loc_proc) then
            msgtyp = msgoff
            call spmd_isend(rbuf(1,loc_proc),isize,it_spmd(p),msgtyp,req_sb(p))
          endif
        enddo
        !-------------------------------------------!
        ! RECIEVING %X_MIN_MAX(1:6) & X_MIN_MAX(1:6)!
        !-------------------------------------------!
        nbirecv=0
        do p = 1, nspmd
          if(loc_proc /= p) then
            nbirecv=nbirecv+1
            irindexi(nbirecv)=p
            msgtyp = msgoff
            call spmd_recv(rbuf(1,p), isize, it_spmd(p), msgtyp)
          endif
        enddo
        !-------------------------------------------!
        !     MPI_WAITING                           !
        !-------------------------------------------!
        do p = 1, nspmd
          if(p /= loc_proc) then
            call spmd_wait(req_sb(p))
          endif
        enddo

        !-------------------------------------------!
        ! COMPUTE AVERAGE ON CURRENT DOMAIN         !
        !-------------------------------------------!
        domain_data%x_min_max(1:3)=ep20
        domain_data%x_min_max(4:6)=-ep20
        domain_data%x_min_max_grid(1:3)=ep20
        domain_data%x_min_max_grid(4:6)=-ep20

        do p=1,nspmd
          domain_data%x_min_max(1) = min(domain_data%x_min_max(1), rbuf(1,p))
          domain_data%x_min_max(2) = min(domain_data%x_min_max(2), rbuf(2,p))
          domain_data%x_min_max(3) = min(domain_data%x_min_max(3), rbuf(3,p))
          domain_data%x_min_max(4) = max(domain_data%x_min_max(4), rbuf(4,p))
          domain_data%x_min_max(5) = max(domain_data%x_min_max(5), rbuf(5,p))
          domain_data%x_min_max(6) = max(domain_data%x_min_max(6), rbuf(6,p))
          !
          domain_data%x_min_max_grid(1) = min(domain_data%x_min_max_grid(1), rbuf(7,p))
          domain_data%x_min_max_grid(2) = min(domain_data%x_min_max_grid(2), rbuf(8,p))
          domain_data%x_min_max_grid(3) = min(domain_data%x_min_max_grid(3), rbuf(9,p))
          domain_data%x_min_max_grid(4) = max(domain_data%x_min_max_grid(4), rbuf(10,p))
          domain_data%x_min_max_grid(5) = max(domain_data%x_min_max_grid(5), rbuf(11,p))
          domain_data%x_min_max_grid(6) = max(domain_data%x_min_max_grid(6), rbuf(12,p))
        enddo

!$OMP END SINGLE
!-----------------------------------------------
        return
      end subroutine spmd_exch_flow_tracking_data4
