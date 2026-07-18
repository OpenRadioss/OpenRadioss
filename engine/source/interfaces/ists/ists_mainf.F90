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
!||====================================================================
!||    ists_mainf_mod   ../engine/source/interfaces/ists/ists_mainf.F90
!||--- called by ------------------------------------------------------
!||    intfop2          ../engine/source/interfaces/interf/intfop2.F
!||====================================================================
      module ists_mainf_mod
        implicit none
      contains

!||====================================================================
!||    ists_mainf                        ../engine/source/interfaces/ists/ists_mainf.F90
!||--- called by ------------------------------------------------------
!||    intfop2                           ../engine/source/interfaces/interf/intfop2.F
!||--- calls      -----------------------------------------------------
!||    frictionparts_model_isot          ../engine/source/interfaces/int07/frictionparts_model.F
!||    ists_sts_bp_persist_set_ncycle    ../engine/source/interfaces/ists/ists_sts_bp_persist_mod.F90
!||    ists_sts_ensure_buffers           ../engine/source/interfaces/ists/ists_sts_capacity_mod.F90
!||    ists_sts_init_capacity            ../engine/source/interfaces/ists/ists_sts_capacity_mod.F90
!||    ists_sts_skip_tick                ../engine/source/interfaces/ists/ists_sts_skip_mod.F90
!||    ists_sts_skip_update              ../engine/source/interfaces/ists/ists_sts_skip_mod.F90
!||    ists_sts_try_grow_capacity        ../engine/source/interfaces/ists/ists_sts_capacity_mod.F90
!||    ists_sts_voxel_grid_get           ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    ists_sts_voxel_grid_is_ready      ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    my_barrier                        ../engine/source/system/machine.F
!||    q1np_contact_driver_int7          ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||    q1np_contact_init_grid_nodes      ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    startime                          ../engine/source/system/timer_mod.F90
!||    stoptime                          ../engine/source/system/timer_mod.F90
!||    sts_contact_stiffness             ../engine/source/interfaces/ists/ists_contact_stiffness.F90
!||    sts_contacts_assemble             ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||    sts_gp_cycle_begin                ../engine/source/interfaces/ists/ists_gp_state_mod.F90
!||    sts_gp_cycle_end                  ../engine/source/interfaces/ists/ists_gp_state_mod.F90
!||    sts_gp_state_init                 ../engine/source/interfaces/ists/ists_gp_state_mod.F90
!||    sts_int7_bucket_broad_phase       ../engine/source/interfaces/ists/ists_broad_phase_int7_bucket.F90
!||    sts_voxel_broad_phase             ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                      ../common_source/modules/constant_mod.F
!||    groupdef_mod                      ../common_source/modules/groupdef_mod.F
!||    h3d_mod                           ../engine/share/modules/h3d_mod.F
!||    intbuf_fric_mod                   ../common_source/modules/interfaces/intbuf_fric_mod.F90
!||    intbufdef_mod                     ../common_source/modules/interfaces/intbufdef_mod.F90
!||    ists_sts_bp_algo_mod              ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||    ists_sts_bp_persist_mod           ../engine/source/interfaces/ists/ists_sts_bp_persist_mod.F90
!||    ists_sts_capacity_mod             ../engine/source/interfaces/ists/ists_sts_capacity_mod.F90
!||    ists_sts_skip_mod                 ../engine/source/interfaces/ists/ists_sts_skip_mod.F90
!||    ists_sts_voxel_grid_mod           ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    precision_mod                     ../common_source/modules/precision_mod.F90
!||    q1np_contact_driver_mod           ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||    sts_broad_phase_int7_bucket_mod   ../engine/source/interfaces/ists/ists_broad_phase_int7_bucket.F90
!||    sts_broad_phase_voxel_mod         ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_contact_stiffness_mod         ../engine/source/interfaces/ists/ists_contact_stiffness.F90
!||    sts_gp_state_mod                  ../engine/source/interfaces/ists/ists_gp_state_mod.F90
!||    timer_mod                         ../engine/source/system/timer_mod.F90
!||====================================================================
      subroutine ists_mainf(timers, &
                       ipari          ,x              ,v              ,a              , &
                       stifn          ,fcont          ,nin            ,fsav           , &
                       jtask          ,nb_jlt_new     , &
                       temp           ,npc            ,tf             , &
                       intbuf_tab     ,ixs            , &
                       h3d_data       ,intbuf_fric_tab,igrsurf, &
                       ms             ,dt2t           ,neltst         ,ityptst        , &
                       iparit         ,lskyi          ,nfskyi         ,fskyi          , &
                       isky           ,nisky          ,snpc           ,stf            , &
                       ncycle         ,nsurf          ,ninter         ,ninterfric     , &
                       numnod         ,numels         ,econtv         ,econt          , &
                       debug          ,sdebug         ,imon           ,inconv         , &
                       anim_v         ,sanim_v        ,outp_v         , &
                       soutp_v        ,npari          ,nthvki         , &
                       tt             ,dt1            ,dt12           ,dtfac1_10)
!======================================================================
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      use timer_mod
      use ists_sts_capacity_mod
      use intbufdef_mod
      use h3d_mod
      use intbuf_fric_mod
      use groupdef_mod
      use sts_gp_state_mod
      use q1np_contact_driver_mod, only: q1np_contact_driver_int7, &
        q1np_contact_init_grid_nodes
      use sts_broad_phase_voxel_mod, only: sts_voxel_broad_phase
      use sts_broad_phase_int7_bucket_mod, only: &
        sts_int7_bucket_broad_phase
      use ists_sts_bp_algo_mod, only: sts_bp_algo, &
        sts_bp_algo_voxel, sts_bp_algo_int7_bucket
      use ists_sts_bp_persist_mod, only: ists_sts_bp_persist_set_ncycle
      use sts_contact_stiffness_mod, only: sts_contact_stiffness
      use ists_sts_skip_mod, only: ists_sts_skip_tick, ists_sts_skip_update
      use ists_sts_voxel_grid_mod, only: ists_sts_voxel_grid_is_ready, &
        ists_sts_voxel_grid_get, ists_sts_voxel_grid_get_tol_static, &
        ists_sts_voxel_grid_update_dynamic
      use ists_ass0_mod, only: ists_ass0, ists_ass_parith
      use constant_mod
      use precision_mod, only : WP
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   g l o b a l   p a r a m e t e r s
!-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "elements.inc"
!-----------------------------------------------
!   c o m m o n   b l o c k s
!-----------------------------------------------
#include      "comlock.inc"
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer, intent(in)    :: nin                            !< interface number (column index in ipari)
      integer, intent(in)    :: npari                          !< first dimension of ipari
      integer, intent(in)    :: nthvki                         !< size of the fsav array
      real(kind=WP), intent(in)    :: tt                             !< current time
      real(kind=WP), intent(in)    :: dt1                            !< current time step
      real(kind=WP), intent(in)    :: dt12                           !< time step used for force integration
      real(kind=WP), intent(in)    :: dtfac1_10                      !< time-step scale factor dtfac1(10)
      integer, intent(inout) :: ipari(npari,ninter)            !< interface parameter array
      integer, intent(in)    :: jtask                          !< current thread/task id
      integer, intent(inout) :: nb_jlt_new                     !< running count of new candidate pairs
      integer, intent(in)    :: snpc                           !< size of the npc array
      integer, intent(in)    :: npc(snpc)                      !< time-function pointer array
      integer, intent(in)    :: ixs(nixs,numels)               !< solid element connectivity
      integer, intent(inout) :: neltst                         !< element id controlling the time step
      integer, intent(inout) :: ityptst                        !< element type controlling the time step
      integer, intent(in)    :: iparit                         !< parith/on flag
      integer, intent(in)    :: lskyi                          !< size of the sky arrays
      integer, intent(in)    :: nfskyi                         !< number of sky force components
      integer, intent(inout) :: nisky                          !< current sky array index
      integer, intent(inout) :: isky(lskyi)                    !< sky node index array
      integer, intent(in)    :: stf                            !< size of the tf array
      integer, intent(in)    :: ncycle                         !< current cycle number
      integer, intent(in)    :: nsurf                          !< number of surfaces
      integer, intent(in)    :: ninter                         !< number of interfaces
      integer, intent(in)    :: ninterfric                     !< number of friction interfaces
      integer, intent(in)    :: numnod                         !< number of nodes
      integer, intent(in)    :: numels                         !< number of solid elements
      integer, intent(in)    :: sdebug                         !< size of the debug array
      integer, intent(in)    :: debug(sdebug)                  !< debug flags
      integer, intent(in)    :: imon                           !< timer monitoring flag
      integer, intent(in)    :: inconv                         !< contact energy output flag
      integer, intent(in)    :: sanim_v                        !< size of the anim_v array
      integer, intent(in)    :: anim_v(sanim_v)                !< animation output flags
      integer, intent(in)    :: soutp_v                        !< size of the outp_v array
      integer, intent(in)    :: outp_v(soutp_v)                !< output file flags
      real(kind=WP), intent(in)    :: x(3,numnod)                    !< nodal coordinates
      real(kind=WP), intent(in)    :: v(3,numnod)                    !< nodal velocities
      real(kind=WP), intent(inout) :: a(3,numnod)                    !< nodal accelerations / forces
      real(kind=WP), intent(inout) :: stifn(numnod)                  !< nodal stiffness
      real(kind=WP), intent(inout) :: fcont(3,numnod)                !< contact forces
      real(kind=WP), intent(in)    :: ms(numnod)                     !< nodal masses
      real(kind=WP), intent(inout) :: fsav(nthvki)                   !< interface saved quantities
      real(kind=WP), intent(inout) :: dt2t                           !< current time-step candidate
      real(kind=WP), intent(inout) :: fskyi(lskyi,nfskyi)            !< sky force array (parith/on)
      real(kind=WP), intent(in)    :: temp(numnod)                   !< nodal temperatures
      real(kind=WP), intent(in)    :: tf(stf)                        !< time functions
      real(kind=WP), intent(inout) :: econtv                         !< contact viscous energy
      real(kind=WP), intent(inout) :: econt                          !< contact energy
      type(timer_),              intent(inout) :: timers                             !< timer structure
      type(intbuf_struct_),      intent(inout) :: intbuf_tab                         !< interface buffer structure
      type(h3d_database),        intent(in)    :: h3d_data                           !< h3d output database
      type(surf_),               intent(in)    :: igrsurf(nsurf)                     !< surface group definitions
      type(intbuf_fric_struct_), target, intent(inout) :: intbuf_fric_tab(ninterfric) !< friction interface buffer
!-----------------------------------------------
!     n e w   v a r i a b l e s
!-----------------------------------------------
      real(kind=WP), dimension(:,:,:), allocatable :: cont_element        !contact element array
      real(kind=WP), dimension(:,:,:), allocatable :: load_arr            !load array
      real(kind=WP), dimension(:),     allocatable :: sts_stif            !STS pair stiffness
      integer, dimension(:,:),     allocatable :: cand_sec_seg_id   !secondary segment nodes
      integer, dimension(:,:),     allocatable :: cand_mst_seg_id   !primary segment nodes
      integer, dimension(:,:),     allocatable :: cand_sec_gp_mask   !active secondary Lobatto corners
      integer, dimension(:),       allocatable :: node_id_load      !node id array (8 nodes per element)
      integer, dimension(:),       allocatable :: sts_ifpen
      integer, save :: sts_wb_capacity = 0
      integer count !count for the number of contact elements found
      integer :: max_sts_size_actual !actual calculated sts array size needed
      integer option !option for the contact algorithm
      integer impact_glob !global impact flag, 0 if no impact, 1 if impact
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer, parameter :: ists_sts_ip_max = 2
      integer i, l, ifq, mfrot, igsti, ivis2, &
              jlt_tied, intth, iform, &
              intfric, nsetprts, npartfric, iorthfric
      integer ix1(mvsiz), ix2(mvsiz), ix3(mvsiz), ix4(mvsiz), &
              ipartfricsi(mvsiz), ipartfricmi(mvsiz)
      real(kind=WP) startt, fric, gap, stopt, visc, viscf, stiglo, gapmin, &
              kmin, kmax, gapmax, gap_for_sts, tint
!-----------------------------------------------
      real(kind=WP) h1(mvsiz), h2(mvsiz), h3(mvsiz), h4(mvsiz), &
              tempi(mvsiz)
      integer ifric

      real(kind=WP) xfiltr_fric,fric_coefs(mvsiz,10),viscffric(mvsiz), &
              fricc(mvsiz), xmu(mvsiz)
      integer, dimension(:) ,pointer  :: tabcoupleparts_fric
      integer, dimension(:) ,pointer  :: tabparts_fric
      integer, dimension(:) ,pointer  :: adparts_fric
      integer, dimension(:) ,pointer  :: ifricorth
      real(kind=WP), dimension(:) ,pointer  :: tabcoef_fric
      integer,target, dimension(1):: tabcoupleparts_fric_bid
      integer,target, dimension(1):: tabparts_fric_bid
      integer,target, dimension(1):: adparts_fric_bid
      integer,target, dimension(1):: ifricorth_bid
      real(kind=WP),target, dimension(1):: tabcoef_fric_bid
      integer :: nsn, nty, noint
      integer :: nrtm
!
      integer :: sts_flag
      integer :: sec_surf_id_sts
      integer :: mst_surf_id_sts
!     Secondary / master surfaces from IPARI(45/46); voxel broad-phase
!     populates candidate segment pairs up to MAX_STS_SIZE_ACTUAL.
      logical :: sts_bp_overflow                          ! broad-phase pair overflow flag
      logical :: sts_bp_retry                             ! retry broad phase after capacity grow
      logical :: sts_do_skip                              ! adaptive skip active this cycle
      integer :: sts_cap_limit
      integer :: sts_n_cell_radius_skip
      real(kind=WP) :: sts_d_min, sts_cell_size_skip
      real(kind=WP) :: sts_search_padding_skip, sts_pad_sq_skip
      logical, save :: int7_contact_mode_msg_done = .false. ! one-time flag for the contact-mode banner
      logical, save :: sts_voxel_overflow_warn_done = .false.
      real(kind=8) sts_econtt_pass, sts_econvt_pass
      real(kind=8) sts_fn_tot(3), sts_ft_tot(3)
!-----------------------------------------------
!   s o u r c e   l i n e s
!-----------------------------------------------
      nrtm = ipari(4,nin)
      nsn = ipari(5,nin)
      nty = ipari(7,nin)
      if(ipari(33,nin) == 1) return  !precondition
      noint = ipari(15,nin)
      mfrot = ipari(30,nin)
      ifq = ipari(31,nin)
      igsti = ipari(34,nin)
      ivis2 = ipari(14,nin)
! heat interface
      intth = ipari(47,nin)
      iform = ipari(48,nin)
      stiglo = -intbuf_tab%stfac(1)
      startt = intbuf_tab%variables(3)
      stopt = intbuf_tab%variables(11)
      if(startt > tt) return !precondition
      if(tt > stopt)  return !precondition
! surface to surface contact i(=1 linear, =2 nurbs q1np)
      sts_flag = ipari(105,nin)
!-----------------------------------------------
!     One-time diagnostic message: which contact branch applies (first cycle).
!-----------------------------------------------
      if (nty == 7 .and. jtask == 1 .and. .not. int7_contact_mode_msg_done) then
        if (abs(tt) <= 1.0e-20 .or. ncycle <= 0) then
          if (sts_flag == 2) then
            write(6,*) 'INT7 (NIN=', nin, '): contact mode = Q1NP (NURBS surface-to-surface)'
          else if (sts_flag == 1) then
            write(6,*) 'INT7 (NIN=', nin,'): contact mode = STS (surface-to-surface)'
            if (sts_bp_algo == sts_bp_algo_int7_bucket) then
              write(6,*) 'INT7 (NIN=', nin, &
                '): STS broad-phase = INT7 bucket (legacy, STS_REMAP)'
            else
              write(6,*) 'INT7 (NIN=', nin, &
                '): STS broad-phase = voxel'
            endif
          else
            write(6,*) 'INT7 (NIN=', nin, '): contact mode = NTS (node-to-surface)'
          endif
          int7_contact_mode_msg_done = .true.
        endif
      endif
!-----------------------------------------------
      fric = intbuf_tab%variables(1)
      visc = intbuf_tab%variables(14)
      gap = intbuf_tab%variables(2)
      gapmin = intbuf_tab%variables(13)
      viscf= intbuf_tab%variables(15)
      ifric = 0
      if(intth > 0) ifric =ipari(50,nin)
      gapmax = intbuf_tab%variables(16)
!     Scalar gap consistent with I7COR3:
      gap_for_sts = min(gapmax, max(gapmin, gap))
      kmin   = intbuf_tab%variables(17)
      kmax   = intbuf_tab%variables(18)
      tint   = intbuf_tab%variables(22)

!--- Corresponding Friction model
      intfric=ipari(72,nin)
      iorthfric = 0
      nsetprts = 0
      npartfric = 0
      xfiltr_fric = zero

      if(intfric /= 0) then
         tabcoupleparts_fric => intbuf_fric_tab(intfric)%tabcoupleparts_fric
         tabcoef_fric => intbuf_fric_tab(intfric)%tabcoef_fric
         tabparts_fric => intbuf_fric_tab(intfric)%tabparts_fric
         adparts_fric => intbuf_fric_tab(intfric)%adparts_fric
         xfiltr_fric = intbuf_fric_tab(intfric)%xfiltr_fric
         nsetprts = intbuf_fric_tab(intfric)%nsetprts
         npartfric = intbuf_fric_tab(intfric)%s_tabparts_fric
         iorthfric = intbuf_fric_tab(intfric)%iorthfric
         ifricorth => intbuf_fric_tab(intfric)%ifricorth
      else
         tabcoupleparts_fric => tabcoupleparts_fric_bid
         tabparts_fric => tabparts_fric_bid
         tabcoef_fric => tabcoef_fric_bid
         adparts_fric => adparts_fric_bid
         ifricorth => ifricorth_bid
         if (ifq/=0) xfiltr_fric = intbuf_tab%xfiltr(1)
      endif

!-----------------------------------------------
!     Type 7 STS (STS_FLAG == 1): voxel broad-phase via IPARI(45/46).
!-----------------------------------------------
      if (nty == 7 .and. sts_flag == 1) then
!
        impact_glob = 0
        mst_surf_id_sts = ipari(46,nin)
        sec_surf_id_sts = ipari(45,nin)
!
        if (jtask == 1) then
!          Set storage capacity for the candidate arrays. Start small,
!          grow on broad-phase overflow, and keep grown buffers alive.
          call ists_sts_init_capacity( &
              igrsurf(sec_surf_id_sts)%nseg, &
              igrsurf(mst_surf_id_sts)%nseg, &
              sts_wb_capacity, max_sts_size_actual, sts_cap_limit)

!          Adaptive skip applies to the voxel broad phase only. The
!          legacy INT7-bucket path always rebuilds candidates from the
!          INT7 sorting when it ran for this cycle.
          if (sts_bp_algo == sts_bp_algo_voxel) then
            call ists_sts_skip_tick(nin, sts_do_skip)
          else
            sts_do_skip = .false.
          endif

          if (.not. sts_do_skip) then
!           Broad phase populates CAND_*_SEG_ID and CONT_ELEMENT. If the
!           compact starting capacity overflows, grow and retry.
            if ((imon>0).and.(ncycle > 37290)) call startime(timers,35)
            sts_bp_retry = .true.
            do while (sts_bp_retry)
              sts_bp_retry = .false.
              call ists_sts_ensure_buffers(max_sts_size_actual, &
                  sts_wb_capacity, cont_element, load_arr, sts_stif, &
                  cand_sec_seg_id, cand_mst_seg_id, cand_sec_gp_mask, &
                  node_id_load, sts_ifpen)
              if (sts_bp_algo == sts_bp_algo_voxel) then
                cand_sec_gp_mask = 1
              endif

              call sts_gp_state_init(max_sts_size_actual, ists_sts_ip_max)

              if (sts_bp_algo == sts_bp_algo_int7_bucket) then
!               Legacy: reuse INT7 bucket sorting candidates (CAND_N/
!               CAND_E) and remap them to STS segment pairs.
                call ists_sts_bp_persist_set_ncycle(ncycle)
                call sts_int7_bucket_broad_phase( &
                    nin, intbuf_tab, igrsurf, nsurf, &
                    sec_surf_id_sts, mst_surf_id_sts, &
                    x, numnod, nsn, nrtm, max_sts_size_actual, &
                    cand_sec_seg_id, cand_mst_seg_id, cand_sec_gp_mask, &
                    cont_element, count, sts_bp_overflow, sts_d_min)
              else
                call sts_voxel_broad_phase( &
                    nin, igrsurf, nsurf, sec_surf_id_sts, mst_surf_id_sts, &
                    x, v, numnod, gap_for_sts, dt1, max_sts_size_actual, &
                    cand_sec_seg_id, cand_mst_seg_id, cont_element, &
                    count, sts_bp_overflow, sts_d_min)
              endif

              call ists_sts_try_grow_capacity(sts_bp_overflow, &
                  max_sts_size_actual, sts_cap_limit, sts_bp_retry)
            enddo
            if ((imon>0).and.(ncycle > 37290)) call stoptime(timers,35)

            if (sts_bp_overflow .and. .not. sts_voxel_overflow_warn_done) then
              if (sts_bp_algo == sts_bp_algo_int7_bucket) then
                write(*,'(A,I8,A,I10,A,I10)') &
                  ' WARNING INTERFACE ', noint, &
                  ': STS INT7-bucket pair overflow, kept ', count, &
                  ' / capacity ', max_sts_size_actual
              else
                write(*,'(A,I8,A,I10,A,I10)') &
                  ' WARNING INTERFACE ', noint, &
                  ': STS voxel pair overflow, kept ', count, &
                  ' / capacity ', max_sts_size_actual
              endif
              sts_voxel_overflow_warn_done = .true.
            endif

!           Friction model
!           STS uses interface-level friction coefficients on remapped
!           pairs; per-part pair ids are not available on this path.
            if (count > 0) then
              do i = 1, min(count, mvsiz)
                ipartfricsi(i) = 0
                ipartfricmi(i) = 0
              enddo
              if (iorthfric == 0) then
                jlt_tied = 0
                call frictionparts_model_isot( &
                       intfric,        min(count,mvsiz),  ipartfricsi, &
                       ipartfricmi,    adparts_fric, &
                       nsetprts,       tabcoupleparts_fric,npartfric, &
                       tabparts_fric,  tabcoef_fric, &
                       fric,           viscf,            intbuf_tab%fric_p, &
                       fric_coefs,     fricc, &
                       viscffric,      nty,              mfrot, &
                       iorthfric,      ifric, &
                       jlt_tied,       tint,             tempi, &
                       npc,            tf, &
                       temp,           h1,               h2, &
                       h3,             h4, &
                       ix1,            ix2,              ix3, &
                       ix4,            iform)
              endif

!             Calculate Contact Stiffness for STS pairs.
              call sts_contact_stiffness( &
                cand_mst_seg_id, cand_sec_seg_id, count, &
                max_sts_size_actual, &
                cand_sec_gp_mask, &
                intbuf_tab%irectm, intbuf_tab%stfm, nrtm, &
                intbuf_tab%nsv, intbuf_tab%stfns, nsn, numnod, &
                igsti, kmin, kmax, stiglo, sts_stif)

!             STS contact search, also used for friction history.
              option = 1
!             STS pair ids do not map to the legacy INT7 IFPEN capacity.
              sts_ifpen = 0
              ! Begin the cycle for the current Gauss/Lobatto point
              call sts_gp_cycle_begin()
              call sts_contacts_assemble(cont_element, count, option, &
                noint, ncycle, &
                cand_mst_seg_id, cand_sec_seg_id, cand_sec_gp_mask, &
                load_arr, node_id_load, l, impact_glob, sts_stif, &
                max_sts_size_actual, fricc, xmu, sts_ifpen, &
                gap_for_sts, v, ms, numnod, visc, ivis2, viscffric, &
                dt2t, neltst, ityptst, &
                sts_econtt_pass, sts_econvt_pass, &
                sts_fn_tot, sts_ft_tot, dt1, dtfac1_10)
              if (impact_glob /= 0) then
#include "lockon.inc"
                fsav(1) = fsav(1) - sts_fn_tot(1) * dt12
                fsav(2) = fsav(2) - sts_fn_tot(2) * dt12
                fsav(3) = fsav(3) - sts_fn_tot(3) * dt12
                fsav(4) = fsav(4) - sts_ft_tot(1) * dt12
                fsav(5) = fsav(5) - sts_ft_tot(2) * dt12
                fsav(6) = fsav(6) - sts_ft_tot(3) * dt12
                fsav(26) = fsav(26) + sts_econtt_pass
                fsav(27) = fsav(27) + sts_econvt_pass
#include "lockoff.inc"
              endif
              if (impact_glob /= 0 .and. inconv == 1) then
#include "lockon.inc"
                econt  = econt  + sts_econtt_pass
                econtv = econtv + sts_econvt_pass
#include "lockoff.inc"
              endif

!             Run Gauss fallback when the Lobatto pass found no active
!             contact.
              if (impact_glob == 0) then
                option = 0
                call sts_contacts_assemble(cont_element, count, option, &
                  noint, ncycle, &
                  cand_mst_seg_id, cand_sec_seg_id, cand_sec_gp_mask, &
                  load_arr, node_id_load, l, impact_glob, sts_stif, &
                  max_sts_size_actual, fricc, xmu, sts_ifpen, &
                  gap_for_sts, v, ms, numnod, visc, ivis2, viscffric, &
                  dt2t, neltst, ityptst, &
                  sts_econtt_pass, sts_econvt_pass, &
                  sts_fn_tot, sts_ft_tot, dt1, dtfac1_10)
                if (impact_glob /= 0) then
#include "lockon.inc"
                fsav(1) = fsav(1) - sts_fn_tot(1) * dt12
                fsav(2) = fsav(2) - sts_fn_tot(2) * dt12
                fsav(3) = fsav(3) - sts_fn_tot(3) * dt12
                fsav(4) = fsav(4) - sts_ft_tot(1) * dt12
                fsav(5) = fsav(5) - sts_ft_tot(2) * dt12
                fsav(6) = fsav(6) - sts_ft_tot(3) * dt12
                fsav(26) = fsav(26) + sts_econtt_pass
                fsav(27) = fsav(27) + sts_econvt_pass
#include "lockoff.inc"
              endif
              if (impact_glob /= 0 .and. inconv == 1) then
#include "lockon.inc"
                econt  = econt  + sts_econtt_pass
                econtv = econtv + sts_econvt_pass
#include "lockoff.inc"
                endif
              endif
              ! End the cycle for the current Gauss/Lobatto point
              call sts_gp_cycle_end()

              ! Scatter the per-pair load_arr / node_id_load into the force arrays.
              ! /PARITH/OFF writes A / STIFN directly (thread-local force block);
              ! /PARITH/ON scatters into the sky arrays so the STS contribution
              if (impact_glob /= 0) then
                ipari(29,nin) = 1
                if (debug(3) >= 1) nb_jlt_new = nb_jlt_new + l - 1
                if (iparit == 0) then
                  call ists_ass0(a, stifn, load_arr, node_id_load, l, &
                                 max_sts_size_actual, fcont, numnod, inconv, &
                                 anim_v(4), outp_v(4), h3d_data%n_vect_cont)
                else
                  call ists_ass_parith(fskyi, isky, nisky, lskyi, nfskyi, &
                                 load_arr, node_id_load, l, &
                                 max_sts_size_actual, fcont, numnod, inconv, &
                                 anim_v(4), outp_v(4), h3d_data%n_vect_cont)
                endif
              endif

            endif ! COUNT > 0

            if (ists_sts_voxel_grid_is_ready(nin)) then
              call ists_sts_voxel_grid_get(nin, sts_cell_size_skip, &
                  sts_search_padding_skip, sts_pad_sq_skip, &
                  sts_n_cell_radius_skip)
              call ists_sts_skip_update(nin, sts_d_min, &
                  sts_search_padding_skip, impact_glob /= 0)
            endif

          endif ! .NOT. STS_DO_SKIP
        endif ! JTASK == 1
        call my_barrier()
!
      elseif ((nty == 7).and.(sts_flag == 2)) then
!
        ! Q1NP_CONTACT_INIT_GRID_NODES:Returns IXS, NIXS, NUMELS for FCONT scatter in the force assembly.
        call q1np_contact_init_grid_nodes(ixs, nixs, numels)
        call my_barrier()
        if (jtask == 1) then
          call q1np_contact_driver_int7(ncycle, numnod, x, a, stifn, &
                                        gap, igsti, kmin, kmax, intbuf_tab%irectm, &
                                        intbuf_tab%nsv, intbuf_tab%stfns, nsn, &
                                        intbuf_tab%stfm, nrtm, fcont, &
                                        inconv == 1 .and. &
                                        anim_v(4)+outp_v(4)+h3d_data%n_vect_cont > 0, &
                                        impact_glob)
          if (impact_glob > 0) ipari(29,nin) = 1
        endif
        call my_barrier()
!
      endif
!
      return
      end subroutine ists_mainf

      end module ists_mainf_mod
