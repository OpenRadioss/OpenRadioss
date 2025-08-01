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
! ======================================================================================================================
!||====================================================================
!||    spmd_exch_flow_tracking_data2_mod   ../engine/source/ale/grid/spmd_exch_flow_tracking_data2.F90
!||--- called by ------------------------------------------------------
!||    alew7                               ../engine/source/ale/grid/alew7.F
!||====================================================================
      module spmd_exch_flow_tracking_data2_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief SPMD exchange necessary for option /ALE/GRID/MASS-WEIGHTED-VEL
!! \details gathering  SUM(mi.vi,i) : DOMAIN_DATA%MOM_L(1:3)
!! gathering SUM(mi.xi,i) : DOMAIN_DATA%COG_L(1:3)
!! gathering SUM(mi)      : DOMAIN_DATA%SUM_M
!
!||====================================================================
!||    spmd_exch_flow_tracking_data2   ../engine/source/ale/grid/spmd_exch_flow_tracking_data2.F90
!||--- called by ------------------------------------------------------
!||    alew7                           ../engine/source/ale/grid/alew7.F
!||--- calls      -----------------------------------------------------
!||    spmd_wait                       ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    ale_mod                         ../common_source/modules/ale/ale_mod.F
!||    constant_mod                    ../common_source/modules/constant_mod.F
!||    precision_mod                   ../common_source/modules/precision_mod.F90
!||    spmd_mod                        ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine spmd_exch_flow_tracking_data2( domain_data, nspmd )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use spmd_mod
          use ale_mod , only : flow_tracking_data_
          use constant_mod , only: zero
          use precision_mod, only: WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included file
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(flow_tracking_data_),intent(inout)::domain_data !< intent(in) ale mass weighted velocity data buffer for given domain
          integer,intent(in)::nspmd                       !< number of spmd domains
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: msgtyp, msgoff, p, nbirecv
          integer :: req_sb(nspmd),irindexi(nspmd)
          integer :: loc_proc, isize
          real(kind=WP) :: rbuf(7,nspmd)
          data msgoff/2205/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
          if(nspmd == 1)return
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          loc_proc=ispmd+1
          rbuf(1:7,1:nspmd)=zero
          rbuf(1:3,loc_proc) = domain_data%mom_l(1:3)
          rbuf(4:6,loc_proc) = domain_data%cog_l(1:3)
          rbuf(7,loc_proc) = domain_data%sum_m
          isize=7
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
          ! RECEIVING %EP(1:9)                        !
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
          domain_data%mom_l(1:3)=zero
          domain_data%cog_l(1:3)=zero
          domain_data%sum_m=zero

          do p=1,nspmd
            domain_data%mom_l(1) = domain_data%mom_l(1) + rbuf(1,p)
            domain_data%mom_l(2) = domain_data%mom_l(2) + rbuf(2,p)
            domain_data%mom_l(3) = domain_data%mom_l(3) + rbuf(3,p)
            domain_data%cog_l(1) = domain_data%cog_l(1) + rbuf(4,p)
            domain_data%cog_l(2) = domain_data%cog_l(2) + rbuf(5,p)
            domain_data%cog_l(3) = domain_data%cog_l(3) + rbuf(6,p)
            domain_data%sum_m = domain_data%sum_m + rbuf(7,p)
          enddo


! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine spmd_exch_flow_tracking_data2
      end module spmd_exch_flow_tracking_data2_mod
