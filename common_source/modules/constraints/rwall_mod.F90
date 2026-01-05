!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!===================================================================================================

!||====================================================================
!||    rwall_mod         ../common_source/modules/constraints/rwall_mod.F90
!||--- called by ------------------------------------------------------
!||    radioss2          ../engine/source/engine/radioss2.F
!||    rdcomi            ../engine/source/output/restart/rdcomm.F
!||    rdresa            ../engine/source/output/restart/rdresa.F
!||    rdresb            ../engine/source/output/restart/rdresb.F
!||    read_rrwallpen    ../engine/source/output/restart/restart_rwallpen.F90
!||    resol             ../engine/source/engine/resol.F
!||    resol_head        ../engine/source/engine/resol_head.F
!||    restalloc         ../engine/source/output/restart/arralloc.F
!||    rgwal0_pen        ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||    write_rrwallpen   ../engine/source/output/restart/restart_rwallpen.F90
!||    wrrestp           ../engine/source/output/restart/wrrestp.F
!||--- uses       -----------------------------------------------------
!||    precision_mod     ../common_source/modules/precision_mod.F90
!||====================================================================
      module rwall_mod
        use precision_mod, only: WP
        implicit none
        private :: WP
        ! ----------------------------------------------------------------------------------------------------------------------
        ! T y p e s
        ! ----------------------------------------------------------------------------------------------------------------------

        type rwall_pen
          integer :: lrwmove                                             !< number of penalty moving rigid wall
          integer :: lnspen                                              !< number of toal sencondary nodes using penalty
          real(kind=WP) , dimension(:)   , allocatable ::  leng_m        !< leng_m(nrwall_pen), length of 2nd nodes to compute pen_ref
          real(kind=WP) , dimension(:)   , allocatable ::  stifm         !< stif(lrwmove), workarray
          real(kind=WP) , dimension(:)   , allocatable ::  stif          !< stif(lnspen)
          real(kind=WP) , dimension(:)   , allocatable ::  pen_old       !< penetration_old(lnspen),workarray
          real(kind=WP) , dimension(:,:) , allocatable ::  ft            !< tengent force(3,lnspen)
        end type rwall_pen

        type rwall_
          integer :: nrwall                                 !< Number of RWALL
          integer :: nrwlag                                 !< Number of RWALL with lagrange multipliers
          integer :: nnprw                                  !< 2nd dimension of nprw
          integer :: nrwlp                                  !< 1er dimension of rwbuf
          integer :: sz_lprw                                !< size of lprw
          integer :: sz_rwsav                               !< size of rwsav
          integer :: nrwall_pen                             !< Number of RWALL w/ penalty
!                    
          integer,dimension(:,:),allocatable   ::  nprw        !< nprw(nrwall,nprw)  IRWALL interger array
          integer,dimension(:),  allocatable   ::  lprw        !< nprw secondary node listes
          integer,dimension(:,:),allocatable   ::  fr_wall     !< front main node array of communication(nspmd+2,nrwall)
          real(kind=WP) ,dimension(:,:), allocatable ::  rwbuf !< RWALL Float variables (nrwlp,nrwall)
          real(kind=WP) ,dimension(:), allocatable   ::  rwsav !< RWALL Float variables (3*nsn_ifq)
          ! Buffers for RWALL penalty
          type (rwall_pen)                         :: pen    !< rwall_pen(nrwall_p)  RWALL penalty
        end type rwall_
!        NPRW(,1)  -> NSL
!        NPRW(,2)  -> ITIED
!        NPRW(,3)  -> MSR >0 -> moving wall, =0 -> fixed wall
!        NPRW(,4)  -> ITYP <0 thermo, 1=plane 2=cylinder 3=sphere 4=parallelogram
!        NPRW(,5)  -> ncont if there is impact
!        NPRW(,6)  -> ILAGM
!        NPRW(,7)  -> for sms
!        NPRW(,8)  -> nimpact (for sms)
!        NPRW(,9)  -> ipen ! new
!        RWL= RWBUF
!        RWL(1:3,) -> plane: normal, Axe of cylinder
!        RWL(4:6,) -> X(1:3,MSR) : position of M
!        RWL(7,)   -> DIAM diameter for cylinder,sphere
!        RWL(7:12,)-> position of parallelogram, parameters for thermo
!        RWL(13,)  -> FRIC
!        RWL(14,)  -> ALPHA for (IFQ)
!        RWL(15,)  -> IFQ
!        RWL(16,)  -> FACX for thermo
!        RWL(17,)  -> Fx
!        RWL(18,)  -> Fy
!        RWL(19,)  -> Fz
!        RWL(20,)  -> somme of secondary mass
!
!===================================================================================================
      contains

        !! \brief allocate rwall type
!===================================================================================================
!||====================================================================
!||    allocate_rwall   ../common_source/modules/constraints/rwall_mod.F90
!||--- called by ------------------------------------------------------
!||    restalloc        ../engine/source/output/restart/arralloc.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod     ../common_source/tools/memory/my_alloc.F90
!||====================================================================
        subroutine allocate_rwall( rwall,nspmd )
          use my_alloc_mod
          implicit none
          type(rwall_),INTENT(INOUT) :: rwall
          integer,INTENT(IN) :: nspmd

          ! General RWALL Buffer
          call my_alloc( rwall%nprw,rwall%nrwall,rwall%nnprw)
          rwall%nprw = 0
          call my_alloc( rwall%lprw,rwall%sz_lprw)
          call my_alloc( rwall%rwbuf,rwall%nrwlp,rwall%nrwall)
          rwall%rwbuf = 0
          call my_alloc( rwall%rwsav,rwall%sz_rwsav)
          rwall%rwsav = 0
          call my_alloc( rwall%fr_wall,nspmd+2,rwall%nrwall)
          rwall%fr_wall = 0
        end subroutine allocate_rwall
        !! \brief allocate rwall%pen type
!||====================================================================
!||    allocate_rwall_pen   ../common_source/modules/constraints/rwall_mod.F90
!||--- called by ------------------------------------------------------
!||    read_rrwallpen       ../engine/source/output/restart/restart_rwallpen.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod         ../common_source/tools/memory/my_alloc.F90
!||====================================================================
        subroutine allocate_rwall_pen(rwallpen,npen)
          use my_alloc_mod
          implicit none
          integer,INTENT(IN) :: npen
          type(rwall_pen),INTENT(INOUT) :: rwallpen

          call my_alloc( rwallpen%leng_m,npen)
          call my_alloc( rwallpen%stif,rwallpen%lnspen)
          call my_alloc( rwallpen%ft,3,rwallpen%lnspen)

        end subroutine allocate_rwall_pen
      end module rwall_mod

