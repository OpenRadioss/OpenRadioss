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
!! \brief SPMD exchange necessary for option /ALE/GRID/MASSFLOW
!!    gathering  SUM(mi.eps,i) : DOMAIN_DATA%EP(1:9)
!!    gathering  SUM(mi)       : DOMAIN_DATA%SUM_M
!!    then averaging eps tensor (L)
!!    deducde spherical tensor %LD and anti-symmetrical tensor %LW  (L=DL+LW)
!!    when finished, reset DOMAIN_DATA%SUM_M
!!      DOMAIN_DATA%EP(1:9) = DOMAIN_DATA%EP(1:9) / DOMAIN_DATA%SUM_M
!!      DOMAIN_DATA%SUM_M = ZERO !used for elem mass in sforc3, used below for nodal mass
!!      DOMAIN_DATA%LD(1) = ...
!!      DOMAIN_DATA%LW(3) = ...
!
      subroutine spmd_exch_massflow_data( domain_data, nspmd  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use ale_mod , only : massflow_data_
        use constant_mod , only: zero
        use spmd_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included file
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
#include "my_real.inc"
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(massflow_data_),intent(inout)::domain_data !< intent(in) ale massflow buffer for given domain
        integer,intent(in)::nspmd                       !< number of spmd domains
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
        rbuf(1:11,1:nspmd) = zero
        rbuf(1:9,loc_proc) = domain_data%ep(1:9)     !sum(mass*epsilon)
        rbuf(10,loc_proc) = domain_data%sum_m        !sum(mass)
        rbuf(11,loc_proc) = domain_data%sum_vol      !sum(volume)
        rbuf(12,loc_proc) = domain_data%num_elem_ale !sum(ale_elem)
        isize=12
        !-------------------------------------------!
        ! SENDING %EP(1:9)                          !
        !-------------------------------------------!
        do p = 1, nspmd
          if(p /= loc_proc) then
            msgtyp = msgoff
            call spmd_isend(rbuf(1,loc_proc),isize,it_spmd(p),msgtyp,req_sb(p))
          endif
        enddo
        !-------------------------------------------!
        ! RECIEVING %EP(1:9)                        !
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
        ! COMPUTE AVERAGE ON CurRENT DOMAIN         !
        !-------------------------------------------!
        domain_data%ep(1:9)=zero
        domain_data%sum_m=zero
        domain_data%sum_vol=zero
        domain_data%num_elem_ale=zero

        do p=1,nspmd
          domain_data%ep(1) = domain_data%ep(1) + rbuf(1,p)
          domain_data%ep(2) = domain_data%ep(2) + rbuf(2,p)
          domain_data%ep(3) = domain_data%ep(3) + rbuf(3,p)
          domain_data%ep(4) = domain_data%ep(4) + rbuf(4,p)
          domain_data%ep(5) = domain_data%ep(5) + rbuf(5,p)
          domain_data%ep(6) = domain_data%ep(6) + rbuf(6,p)
          domain_data%ep(7) = domain_data%ep(7) + rbuf(7,p)
          domain_data%ep(8) = domain_data%ep(8) + rbuf(8,p)
          domain_data%ep(9) = domain_data%ep(9) + rbuf(9,p)
          domain_data%sum_m = domain_data%sum_m + rbuf(10,p)
          domain_data%sum_vol = domain_data%sum_vol + rbuf(11,p)
          domain_data%num_elem_ale = domain_data%num_elem_ale + rbuf(12,p)
        enddo


!$OMP END SINGLE
! ----------------------------------------------------------------------------------------------------------------------
        return
      end subroutine spmd_exch_massflow_data
