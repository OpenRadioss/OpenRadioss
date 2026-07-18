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
module q1np_generate_main_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Generate Q1NP elements from surface definitions (enriched HEX)
!! \details Elements are generated internally from two external /SURF IDs (100 and 200).
!!          This routine handles dual-surface, single-surface A-only, and single-surface B-only branches,
!!          allocates all Q1NP data tables, promotes control points to mesh nodes, and resizes
!!          all NUMNOD-dependent arrays accordingly.
!
  subroutine q1np_generate_main(iwcont, iwcin2, ikine1lag, dsdof, addcne,                      &
                                addcne_pxfem, fxbtag, itag, flagkin, xyzref,                   &
                                sx, sv, sd, sms, sin,                                          &
                                svr, sdr, s_nod2els, iparts, iroddl,                           &
                                iroddl0, isecut, impose_dr, nsurf, numnod,                     &
                                numels, iout, iisrot, idrot, sicode,                           &
                                siskew, snod2sp, ncharkey, hm_ninter, lsubmodel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
    use q1np_restart_mod
    use q1np_surf_grid_mod
    use q1np_promote_cp_mod
    use genq1np_mod
    use q1np_export_csv_mod
    use restmod
    use group_mod , only : igrsurf
    use groupdef_mod
    use front_mod , only : ifront, ientry2, sifront
    use nod2el_mod
    use message_mod
    use element_mod , only : nixs
    use constant_mod , only : zero
    use precision_mod , only : wp
    use submodel_mod, only: nsubmod, submodel_data
    use hm_option_read_mod
    use my_alloc_mod, only : my_alloc
    use my_dealloc_mod, only : my_dealloc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   INCLUDED FILES
! ----------------------------------------------------------------------------------------------------------------------
! No includes - common block variables passed as arguments
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, dimension(:), allocatable, intent(inout) :: iwcont         !< kinematic constraint work array
    integer, dimension(:), allocatable, intent(inout) :: iwcin2         !< kinematic constraint work array 2
    integer, dimension(:), allocatable, intent(inout) :: ikine1lag      !< Lagrange kinematic array
    integer, dimension(:), allocatable, intent(inout) :: dsdof          !< DOF array
    integer, dimension(:), allocatable, intent(inout) :: addcne         !< node-element connectivity pointer
    integer, dimension(:), allocatable, intent(inout) :: addcne_pxfem   !< XFEM node-element connectivity pointer
    integer, dimension(:), allocatable, intent(inout) :: fxbtag         !< FXB tag array
    integer, dimension(:), allocatable, intent(inout) :: itag           !< tag array
    integer, dimension(:), allocatable, intent(inout) :: flagkin        !< kinematic BC flag array
    real(kind=wp), dimension(:), allocatable, intent(inout) :: xyzref         !< reference coordinates
    integer,                            intent(inout) :: sx             !< size of X array
    integer,                            intent(inout) :: sv             !< size of V array
    integer,                            intent(inout) :: sd             !< size of D array
    integer,                            intent(inout) :: sms            !< size of MS array
    integer,                            intent(inout) :: sin            !< size of IN array
    integer,                            intent(inout) :: svr            !< size of VR array
    integer,                            intent(inout) :: sdr            !< size of DR array
    integer,                            intent(inout) :: s_nod2els      !< size of NOD2ELS array
    integer, dimension(:), pointer,      intent(in)    :: iparts         !< part pointer for solid elements
    integer,                            intent(in)    :: iroddl          !< rotational DOF flag
    integer,                            intent(in)    :: iroddl0         !< rotational DOF flag (initial)
    integer,                            intent(in)    :: isecut          !< section cut flag
    integer,                            intent(in)    :: impose_dr       !< imposed DR flag
    integer,                            intent(inout) :: nsurf           !< number of surfaces
    integer,                            intent(inout) :: numnod          !< number of nodes
    integer,                            intent(in)    :: numels          !< number of solid elements
    integer,                            intent(in)    :: iout            !< output file unit
    integer,                            intent(in)    :: iisrot          !< rotation flag
    integer,                            intent(in)    :: idrot           !< rotation direction flag
    integer,                            intent(inout) :: sicode          !< size of ICODE
    integer,                            intent(inout) :: siskew          !< size of ISKEW
    integer,                            intent(inout) :: snod2sp         !< size of NOD2SP
    integer,                            intent(in)    :: ncharkey        !< length of character keys
    integer,                            intent(in)    :: hm_ninter       !< number of interface cards
    type(submodel_data),                intent(in)    :: lsubmodel(nsubmod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
!    integer, parameter :: surf_id_a = 1000
!    integer, parameter :: surf_id_b = 2000
    integer, parameter :: p_test = 2
    integer, parameter :: q_test = 2

    integer :: i, iel, stat
    integer :: i_inter, ists, isu1, isu2, id_inter
    integer :: isurf_id_a, isurf_id_b
    integer :: surf_id_a, surf_id_b
    integer :: nseg_surf, nseg_surf_b
    integer :: nx_est, ny_est, nx_est_b, ny_est_b
    integer :: ncp_u_est, ncp_v_est, ncp_u_est_b, ncp_v_est_b
    integer :: nknot_u_est, nknot_v_est, nknot_u_est_b, nknot_v_est_b
    integer :: ierr_q1np_grid, ierr_q1np_grid_b
    integer :: maxq1npctrl
    integer :: sq1npctrl_shared_a, sq1npctrl_shared_b
    integer :: sq1npknot_l_a, sq1npknot_l_b
    integer :: siq1np_a, siq1np_b
    integer :: sq1npbulk_a, sq1npbulk_b
    integer :: cp_base_b, iq1np_ctrl_base_b, iq1np_bulk_base_b
    integer :: q1np_ktab_base_b
    integer :: numelq1np_a_out, numelq1np_b_out
    integer :: nx_knot_a, ny_knot_a, nx_knot_b, ny_knot_b
    integer :: numnod_est
    integer :: sx_old, sv_old, sd_old, sms_old, sin_old, svr_old, sdr_old
    integer :: sx_new, sv_new, sd_new, sms_new, sin_new, svr_new, sdr_new
    integer :: np_u_q1np, np_v_q1np, np_t_q1np
    integer :: size_old, gp_index, it, j
    integer :: max_user_id
    integer :: extra_per_el, s_nod2els_new
    integer :: sitabm1
    integer :: inter_flag_ists(hm_ninter),inter_surf(hm_ninter,2)

    integer, dimension(:), allocatable :: iwcont_tmp, iwcin2_tmp
    integer, dimension(:), allocatable :: ikine1lag_tmp, dsdof_new
    integer, dimension(:), allocatable :: icode_new, iskew_new
    integer, dimension(:), allocatable :: itab_new, itabm1_new
    integer, dimension(:), allocatable :: q1np_seg_i_tmp, q1np_seg_j_tmp
    integer, dimension(:,:), allocatable :: q1np_grid_node_tmp, q1np_grid_to_seg_tmp
    integer, dimension(:), allocatable :: iresize_tmp
    integer, dimension(:), allocatable :: nod2els_new

    real(kind=wp), allocatable :: x_save(:), v_save(:), d_save(:), ms_save(:)
    real(kind=wp), allocatable :: in_save(:), vr_save(:), dr_save(:), xyzref_save(:)

    logical :: is_ists2,is_available

    character(len=ncharkey) :: key

! ----------------------------------------------------------------------------------------------------------------------
!                                                   EXTERNAL FUNCTIONS
! ----------------------------------------------------------------------------------------------------------------------
    external constit
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

!     Reset Q1NP module counts/arrays (will be generated after surfaces are read).
      call reset_q1np_counts()
!
!     Preading of interfaces to detect ists flag
      is_ists2 = .false.
      inter_flag_ists(1:hm_ninter) = 0
      inter_surf(1:hm_ninter,1:2) = 0
      call hm_option_start('/INTER')
      do i_inter=1,hm_ninter
        call hm_option_read_key(lsubmodel,                    &
                                option_id = id_inter,         &
                                keyword2 = key)
        if (key(1:5)=='TYPE7') then
          call hm_get_intv('Ists', ists, is_available, lsubmodel)
          call hm_get_intv('secondaryentityids',isu1,is_available,lsubmodel)
          call hm_get_intv('mainentityids',isu2,is_available,lsubmodel)
          inter_flag_ists(i_inter) = ists
          inter_surf(i_inter,2) = isu1
          inter_surf(i_inter,1) = isu2
          if (ists == 2) then
            is_ists2 = .true.
          end if
        endif                          
      enddo  
!
!     Generation of q1np elements only if interface with ists=2 detected    
      if (.not. is_ists2) return
!
      do i_inter=1,hm_ninter   
!        
        if (inter_flag_ists(i_inter) == 2) then
!  
          surf_id_a = inter_surf(i_inter,1)    
          surf_id_b = inter_surf(i_inter,2)      
!    
          isurf_id_a = 0
          isurf_id_b = 0
          do i = 1, nsurf
            if (igrsurf(i)%id == surf_id_a) isurf_id_a = i
            if (igrsurf(i)%id == surf_id_b) isurf_id_b = i
          end do

!         Segment counts (used for dual / single-surface prototype branches).
          nseg_surf = 0
          nseg_surf_b = 0
          if (isurf_id_a > 0) nseg_surf = igrsurf(isurf_id_a)%nseg
          if (isurf_id_b > 0) nseg_surf_b = igrsurf(isurf_id_b)%nseg

!         Prototype Q1NP: dual surface (100+200), or single surface A or B only.
          if (isurf_id_a > 0 .and. isurf_id_b > 0 .and. nseg_surf > 0 .and. &
              nseg_surf_b > 0) then

!           Derive grid dimensions from surface topology (A).
            nx_est = 0
            ny_est = 0
            ierr_q1np_grid = 0
            call my_alloc(q1np_seg_i_tmp, nseg_surf, "Q1NP_SEG_I_TMP", stat=stat)
            if (stat == 0) &
              call my_alloc(q1np_seg_j_tmp, nseg_surf, "Q1NP_SEG_J_TMP", stat=stat)
            if (stat == 0) then
              call my_alloc(q1np_grid_to_seg_tmp, nseg_surf, nseg_surf, "Q1NP_GRID_TO_SEG_TMP", stat=stat)
            endif
            if (stat == 0) then
              call my_alloc(q1np_grid_node_tmp, nseg_surf*4, nseg_surf*4, "Q1NP_GRID_NODE_TMP", stat=stat)
            endif
            if (stat == 0) then
              call q1np_build_surf_grid(igrsurf(isurf_id_a), nseg_surf, &
                   nx_est, ny_est, q1np_seg_i_tmp, q1np_seg_j_tmp, &
                   q1np_grid_node_tmp, q1np_grid_to_seg_tmp, &
                   ierr_q1np_grid)
            else
              ierr_q1np_grid = 7
            endif
            call my_dealloc(q1np_seg_i_tmp)
            call my_dealloc(q1np_seg_j_tmp)
            call my_dealloc(q1np_grid_node_tmp)
            call my_dealloc(q1np_grid_to_seg_tmp)

            if (ierr_q1np_grid /= 0 .or. nx_est <= 0 .or. ny_est <= 0) then
!             Conservative fallback to avoid underallocation if the topology
!             pre-pass fails. GENQ1NP will still do the authoritative check.
              nx_est = nseg_surf
              ny_est = 1
              write(iout,'(A,I6,A,I6,A)') &
                ' Q1NP WARNING: pre-sizing fallback used for SURF_ID_A=', &
                isurf_id_a, ' (grid ierr=', ierr_q1np_grid, ').'
            endif

!           Derive grid dimensions from surface topology (B).
            nx_est_b = 0
            ny_est_b = 0
            ierr_q1np_grid_b = 0
            call my_alloc(q1np_seg_i_tmp, nseg_surf_b, "Q1NP_SEG_I_TMP", stat=stat)
            if (stat == 0) &
              call my_alloc(q1np_seg_j_tmp, nseg_surf_b, "Q1NP_SEG_J_TMP", stat=stat)
            if (stat == 0) then
              call my_alloc(q1np_grid_to_seg_tmp, nseg_surf_b, nseg_surf_b, "Q1NP_GRID_TO_SEG_TMP", stat=stat)
            endif
            if (stat == 0) then
              call my_alloc(q1np_grid_node_tmp, nseg_surf_b*4, nseg_surf_b*4, "Q1NP_GRID_NODE_TMP", stat=stat)
            endif
            if (stat == 0) then
              call q1np_build_surf_grid(igrsurf(isurf_id_b), nseg_surf_b, &
                   nx_est_b, ny_est_b, q1np_seg_i_tmp, q1np_seg_j_tmp, &
                   q1np_grid_node_tmp, q1np_grid_to_seg_tmp, &
                   ierr_q1np_grid_b)
            else
              ierr_q1np_grid_b = 7
            endif
            call my_dealloc(q1np_seg_i_tmp)
            call my_dealloc(q1np_seg_j_tmp)
            call my_dealloc(q1np_grid_node_tmp)
            call my_dealloc(q1np_grid_to_seg_tmp)

            if (ierr_q1np_grid_b /= 0 .or. nx_est_b <= 0 .or. ny_est_b <= 0) then
!             Conservative fallback to avoid underallocation if the topology
!             pre-pass fails. GENQ1NP will still do the authoritative check.
              nx_est_b = nseg_surf_b
              ny_est_b = 1
              write(iout,'(A,I6,A,I6,A)') &
                ' Q1NP WARNING: pre-sizing fallback used for SURF_ID_B=', &
                isurf_id_b, ' (grid ierr=', ierr_q1np_grid_b, ').'
            endif

!           Number of Q1Np elements (A+B) = sum of surface segments.
            numelq1np_g = nseg_surf + nseg_surf_b

!           Control points: ncp = ne + p (from Python algorithm).
            ncp_u_est = nx_est + p_test
            ncp_v_est = ny_est + q_test
            ncp_u_est_b = nx_est_b + p_test
            ncp_v_est_b = ny_est_b + q_test

            maxq1npctrl = (p_test+1)*(q_test+1)

!           Unique control points shared across each grid.
            sq1npctrl_shared_a = ncp_u_est * ncp_v_est
            sq1npctrl_shared_b = ncp_u_est_b * ncp_v_est_b
            sq1npctrl_shared_g = sq1npctrl_shared_a + sq1npctrl_shared_b

!           IQ1NP_TAB stores only per-element CP connectivity.
            sq1npctrl_l_g = numelq1np_g * maxq1npctrl
            siq1np_g = sq1npctrl_l_g

!           Separate storage for the 4 legacy bulk nodes per element.
            sq1npbulk_a = nbulkq1np * nseg_surf
            sq1npbulk_b = nbulkq1np * nseg_surf_b
            sq1npbulk_g = sq1npbulk_a + sq1npbulk_b

!           Weights: one per unique control point.
            sq1npweight_l_g = sq1npctrl_shared_g

!           Knot vectors: U and V concatenated (per knot set, then appended).
            nknot_u_est = nx_est + 2*p_test + 1
            nknot_v_est = ny_est + 2*q_test + 1
            nknot_u_est_b = nx_est_b + 2*p_test + 1
            nknot_v_est_b = ny_est_b + 2*q_test + 1

            sq1npknot_l_a = nknot_u_est + nknot_v_est
            sq1npknot_l_b = nknot_u_est_b + nknot_v_est_b
            sq1npknot_l_g = sq1npknot_l_a + sq1npknot_l_b

            skq1np_g = nkq1np * numelq1np_g

!           Bases for appending surface B results.
            siq1np_a = nseg_surf * maxq1npctrl
            siq1np_b = nseg_surf_b * maxq1npctrl

            cp_base_b = sq1npctrl_shared_a
            iq1np_ctrl_base_b = siq1np_a
            iq1np_bulk_base_b = sq1npbulk_a
            q1np_ktab_base_b = sq1npknot_l_a

            call my_alloc(kq1np_tab, nkq1np, numelq1np_g, "KQ1NP_TAB")
            call my_alloc(iq1np_tab, siq1np_g, "IQ1NP_TAB")
            call my_alloc(iq1np_bulk_tab, sq1npbulk_g, "IQ1NP_BULK_TAB")
            call my_alloc(q1np_wtab, sq1npweight_l_g, "Q1NP_WTAB")
            call my_alloc(q1np_ktab, sq1npknot_l_g, "Q1NP_KTAB")
            if(sq1npctrl_shared_g > 0) then
              call my_alloc(q1np_cptab, 3, sq1npctrl_shared_g, "Q1NP_CPTAB")
            endif

!           Initialize arrays to zero.
            kq1np_tab(:,:) = 0
            if(siq1np_g > 0) iq1np_tab = 0
            if(sq1npbulk_g > 0) iq1np_bulk_tab = 0
            if(sq1npweight_l_g > 0) q1np_wtab = zero
            if(sq1npknot_l_g > 0) q1np_ktab = zero
            if(allocated(q1np_cptab)) q1np_cptab = zero

!           Generate Q1NP elements from surface A (knot_set_id=1).
            call genq1np(igrsurf,ixs,x,iparts, &
                         nsurf,nixs,numnod,numels,iout, &
                         isurf_id_a, 1, &
                         kq1np_tab, iq1np_tab(1:siq1np_a), &
                         iq1np_bulk_tab(1:sq1npbulk_a), &
                         q1np_wtab(1:sq1npctrl_shared_a), &
                         q1np_ktab(1:sq1npknot_l_a), &
                         q1np_cptab(:,1:sq1npctrl_shared_a), &
                         sq1npctrl_shared_a, surf_id_a, numelq1np_a_out)

!           Generate Q1NP elements from surface B (knot_set_id=2).
            call genq1np(igrsurf,ixs,x,iparts, &
                         nsurf,nixs,numnod,numels,iout, &
                         isurf_id_b, 2, &
                         kq1np_tab, &
                         iq1np_tab(iq1np_ctrl_base_b+1:iq1np_ctrl_base_b+siq1np_b), &
                         iq1np_bulk_tab(iq1np_bulk_base_b+1:iq1np_bulk_base_b+sq1npbulk_b), &
                         q1np_wtab(cp_base_b+1:cp_base_b+sq1npctrl_shared_b), &
                         q1np_ktab(q1np_ktab_base_b+1:q1np_ktab_base_b+sq1npknot_l_b), &
                         q1np_cptab(:,cp_base_b+1:cp_base_b+sq1npctrl_shared_b), &
                         sq1npctrl_shared_b, surf_id_b, numelq1np_b_out, &
                         nseg_surf)

!           Shift control-point IDs in IQ1NP_TAB for appended surface B.
            do i = iq1np_ctrl_base_b+1, iq1np_ctrl_base_b+siq1np_b
              if (iq1np_tab(i) > 0) iq1np_tab(i) = iq1np_tab(i) + cp_base_b
            end do

!           Fix IQ1NP_TAB and IQ1NP_BULK_TAB offsets stored in KQ1NP_TAB for surface B.
            do iel = nseg_surf+1, numelq1np_g
              kq1np_tab(4,iel)  = kq1np_tab(4,iel)  + iq1np_ctrl_base_b
              kq1np_tab(14,iel) = kq1np_tab(14,iel) + iq1np_bulk_base_b
            end do

            if (q1np_export_csv) then
!             Export classical bricks with Q1NP replacements omitted.
!             Do it per surface to keep output files distinct.
              call q1np_export_hex8_csv(numels,nixs,ixs,x,numnod, &
                                        numelq1np_a_out, &
                                        kq1np_tab(:,1:numelq1np_a_out), nkq1np, &
                                        surf_id_a)

              call q1np_export_hex8_csv(numels,nixs,ixs,x,numnod, &
                                        numelq1np_b_out, &
                                        kq1np_tab(:,nseg_surf+1:nseg_surf+numelq1np_b_out), &
                                        nkq1np, surf_id_b)
            endif

            nx_knot_a = nx_est
            ny_knot_a = ny_est
            nx_knot_b = nx_est_b
            ny_knot_b = ny_est_b
            if (numelq1np_a_out > 0 .and. kq1np_tab(12,1) > 0 .and. &
                kq1np_tab(13,1) > 0) then
              nx_knot_a = kq1np_tab(12,1)
              ny_knot_a = kq1np_tab(13,1)
            endif
            if (numelq1np_b_out > 0 .and. nseg_surf+1 <= numelq1np_g .and. &
                kq1np_tab(12,nseg_surf+1) > 0 .and. &
                kq1np_tab(13,nseg_surf+1) > 0) then
              nx_knot_b = kq1np_tab(12,nseg_surf+1)
              ny_knot_b = kq1np_tab(13,nseg_surf+1)
            endif

!           Store Q1NP_KTAB pointer and grid dimensions in Q1NP_RESTART_MOD
!           (legacy single-grid view; full per-element hetero support is handled
!           in later refactors).
            q1np_ktab_g => q1np_ktab
            call set_q1np_knot_sets(2, (/nx_knot_a, nx_knot_b/), &
                                      (/ny_knot_a, ny_knot_b/), &
                                      (/1, sq1npknot_l_a+1/), (/sq1npknot_l_a, sq1npknot_l_b/))

            call set_q1np_counts(numelq1np_g,skq1np_g,siq1np_g, &
                                 sq1npbulk_g,sq1npctrl_shared_g, &
                                 sq1npctrl_l_g,sq1npweight_l_g, &
                                 sq1npknot_l_g)

!           SSURFTAG bounds checks use TABVINT_LEN_G for IQ1NP_TAB indexing.
!           In the multi-surface prototype, GENQ1NP is called on slices, so set
!           TABVINT_LEN_G explicitly to the total allocated IQ1NP_TAB length.
            call set_q1np_tabvint_len(siq1np_g)

          else if (isurf_id_a > 0 .and. nseg_surf > 0) then

!           Single surface A only (B missing or has no segments).
            nx_est = 0
            ny_est = 0
            ierr_q1np_grid = 0
            call my_alloc(q1np_seg_i_tmp, nseg_surf, "Q1NP_SEG_I_TMP", stat=stat)
            if (stat == 0) &
              call my_alloc(q1np_seg_j_tmp, nseg_surf, "Q1NP_SEG_J_TMP", stat=stat)
            if (stat == 0) then
              call my_alloc(q1np_grid_to_seg_tmp, nseg_surf, nseg_surf, "Q1NP_GRID_TO_SEG_TMP", stat=stat)
            endif
            if (stat == 0) then
              call my_alloc(q1np_grid_node_tmp, nseg_surf*4, nseg_surf*4, "Q1NP_GRID_NODE_TMP", stat=stat)
            endif
            if (stat == 0) then
              call q1np_build_surf_grid(igrsurf(isurf_id_a), nseg_surf, &
                   nx_est, ny_est, q1np_seg_i_tmp, q1np_seg_j_tmp, &
                   q1np_grid_node_tmp, q1np_grid_to_seg_tmp, &
                   ierr_q1np_grid)
            else
              ierr_q1np_grid = 7
            endif
            call my_dealloc(q1np_seg_i_tmp)
            call my_dealloc(q1np_seg_j_tmp)
            call my_dealloc(q1np_grid_node_tmp)
            call my_dealloc(q1np_grid_to_seg_tmp)

            if (ierr_q1np_grid /= 0 .or. nx_est <= 0 .or. ny_est <= 0) then
              nx_est = nseg_surf
              ny_est = 1
              write(iout,'(A,I6,A,I6,A)') &
                ' Q1NP WARNING: pre-sizing fallback used for SURF_ID_A=', &
                isurf_id_a, ' (grid ierr=', ierr_q1np_grid, ').'
            endif

            maxq1npctrl = (p_test+1)*(q_test+1)
            numelq1np_g = nseg_surf

            ncp_u_est = nx_est + p_test
            ncp_v_est = ny_est + q_test

            sq1npctrl_shared_g = ncp_u_est * ncp_v_est
            sq1npctrl_l_g = numelq1np_g * maxq1npctrl
            siq1np_g = sq1npctrl_l_g

            sq1npbulk_g = nbulkq1np * nseg_surf
            sq1npweight_l_g = sq1npctrl_shared_g

            nknot_u_est = nx_est + 2*p_test + 1
            nknot_v_est = ny_est + 2*q_test + 1
            sq1npknot_l_g = nknot_u_est + nknot_v_est

            skq1np_g = nkq1np * numelq1np_g

            call my_alloc(kq1np_tab, nkq1np, numelq1np_g, "KQ1NP_TAB")
            call my_alloc(iq1np_tab, siq1np_g, "IQ1NP_TAB")
            call my_alloc(iq1np_bulk_tab, sq1npbulk_g, "IQ1NP_BULK_TAB")
            call my_alloc(q1np_wtab, sq1npweight_l_g, "Q1NP_WTAB")
            call my_alloc(q1np_ktab, sq1npknot_l_g, "Q1NP_KTAB")
            if(sq1npctrl_shared_g > 0) then
              call my_alloc(q1np_cptab, 3, sq1npctrl_shared_g, "Q1NP_CPTAB")
            endif

            kq1np_tab(:,:) = 0
            if(siq1np_g > 0) iq1np_tab = 0
            if(sq1npbulk_g > 0) iq1np_bulk_tab = 0
            if(sq1npweight_l_g > 0) q1np_wtab = zero
            if(sq1npknot_l_g > 0) q1np_ktab = zero
            if(allocated(q1np_cptab)) q1np_cptab = zero

            call genq1np(igrsurf,ixs,x,iparts, &
                         nsurf,nixs,numnod,numels,iout, &
                         isurf_id_a, 1, &
                         kq1np_tab, iq1np_tab, &
                         iq1np_bulk_tab, &
                         q1np_wtab, &
                         q1np_ktab, &
                         q1np_cptab, &
                         sq1npctrl_shared_g, surf_id_a, numelq1np_a_out)

            if (q1np_export_csv) then
              call q1np_export_hex8_csv(numels,nixs,ixs,x,numnod, &
                                        numelq1np_a_out, &
                                        kq1np_tab(:,1:numelq1np_a_out), nkq1np, &
                                        surf_id_a)
            endif

            nx_knot_a = nx_est
            ny_knot_a = ny_est
            if (numelq1np_a_out > 0 .and. kq1np_tab(12,1) > 0 .and. &
                kq1np_tab(13,1) > 0) then
              nx_knot_a = kq1np_tab(12,1)
              ny_knot_a = kq1np_tab(13,1)
            endif

            q1np_ktab_g => q1np_ktab
            call set_q1np_knot_sets(1, (/nx_knot_a/), (/ny_knot_a/), (/1/), &
                                    (/sq1npknot_l_g/))

            call set_q1np_counts(numelq1np_g,skq1np_g,siq1np_g, &
                                 sq1npbulk_g,sq1npctrl_shared_g, &
                                 sq1npctrl_l_g,sq1npweight_l_g, &
                                 sq1npknot_l_g)

            call set_q1np_tabvint_len(siq1np_g)

          else if (isurf_id_b > 0 .and. nseg_surf_b > 0) then

!           Single surface B only (A missing or has no segments).
            nx_est_b = 0
            ny_est_b = 0
            ierr_q1np_grid_b = 0
            call my_alloc(q1np_seg_i_tmp, nseg_surf_b, "Q1NP_SEG_I_TMP", stat=stat)
            if (stat == 0) &
              call my_alloc(q1np_seg_j_tmp, nseg_surf_b, "Q1NP_SEG_J_TMP", stat=stat)
            if (stat == 0) then
              call my_alloc(q1np_grid_to_seg_tmp, nseg_surf_b, nseg_surf_b, "Q1NP_GRID_TO_SEG_TMP", stat=stat)
            endif
            if (stat == 0) then
              call my_alloc(q1np_grid_node_tmp, nseg_surf_b*4, nseg_surf_b*4, "Q1NP_GRID_NODE_TMP", stat=stat)
            endif
            if (stat == 0) then
              call q1np_build_surf_grid(igrsurf(isurf_id_b), nseg_surf_b, &
                   nx_est_b, ny_est_b, q1np_seg_i_tmp, q1np_seg_j_tmp, &
                   q1np_grid_node_tmp, q1np_grid_to_seg_tmp, &
                   ierr_q1np_grid_b)
            else
              ierr_q1np_grid_b = 7
            endif
            call my_dealloc(q1np_seg_i_tmp)
            call my_dealloc(q1np_seg_j_tmp)
            call my_dealloc(q1np_grid_node_tmp)
            call my_dealloc(q1np_grid_to_seg_tmp)

            if (ierr_q1np_grid_b /= 0 .or. nx_est_b <= 0 .or. ny_est_b <= 0) then
              nx_est_b = nseg_surf_b
              ny_est_b = 1
              write(iout,'(A,I6,A,I6,A)') &
                ' Q1NP WARNING: pre-sizing fallback used for SURF_ID_B=', &
                isurf_id_b, ' (grid ierr=', ierr_q1np_grid_b, ').'
            endif

            maxq1npctrl = (p_test+1)*(q_test+1)
            numelq1np_g = nseg_surf_b

            ncp_u_est_b = nx_est_b + p_test
            ncp_v_est_b = ny_est_b + q_test

            sq1npctrl_shared_g = ncp_u_est_b * ncp_v_est_b
            sq1npctrl_l_g = numelq1np_g * maxq1npctrl
            siq1np_g = sq1npctrl_l_g

            sq1npbulk_g = nbulkq1np * nseg_surf_b
            sq1npweight_l_g = sq1npctrl_shared_g

            nknot_u_est_b = nx_est_b + 2*p_test + 1
            nknot_v_est_b = ny_est_b + 2*q_test + 1
            sq1npknot_l_g = nknot_u_est_b + nknot_v_est_b

            skq1np_g = nkq1np * numelq1np_g
            call my_alloc(kq1np_tab, nkq1np, numelq1np_g, "KQ1NP_TAB")
            call my_alloc(iq1np_tab, siq1np_g, "IQ1NP_TAB")
            call my_alloc(iq1np_bulk_tab, sq1npbulk_g, "IQ1NP_BULK_TAB")
            call my_alloc(q1np_wtab, sq1npweight_l_g, "Q1NP_WTAB")
            call my_alloc(q1np_ktab, sq1npknot_l_g, "Q1NP_KTAB")
            if(sq1npctrl_shared_g > 0) then
              call my_alloc(q1np_cptab, 3, sq1npctrl_shared_g, "Q1NP_CPTAB")
            endif

            kq1np_tab(:,:) = 0
            if(siq1np_g > 0) iq1np_tab = 0
            if(sq1npbulk_g > 0) iq1np_bulk_tab = 0
            if(sq1npweight_l_g > 0) q1np_wtab = zero
            if(sq1npknot_l_g > 0) q1np_ktab = zero
            if(allocated(q1np_cptab)) q1np_cptab = zero

            call genq1np(igrsurf,ixs,x,iparts, &
                         nsurf,nixs,numnod,numels,iout, &
                         isurf_id_b, 1, &
                         kq1np_tab, iq1np_tab, &
                         iq1np_bulk_tab, &
                         q1np_wtab, &
                         q1np_ktab, &
                         q1np_cptab, &
                         sq1npctrl_shared_g, surf_id_b, numelq1np_b_out)

            if (q1np_export_csv) then
              call q1np_export_hex8_csv(numels,nixs,ixs,x,numnod, &
                                        numelq1np_b_out, &
                                        kq1np_tab(:,1:numelq1np_b_out), nkq1np, &
                                        surf_id_b)
            endif

            nx_knot_b = nx_est_b
            ny_knot_b = ny_est_b
            if (numelq1np_b_out > 0 .and. kq1np_tab(12,1) > 0 .and. &
                kq1np_tab(13,1) > 0) then
              nx_knot_b = kq1np_tab(12,1)
              ny_knot_b = kq1np_tab(13,1)
            endif

            q1np_ktab_g => q1np_ktab
            call set_q1np_knot_sets(1, (/nx_knot_b/), (/ny_knot_b/), (/1/), &
                                    (/sq1npknot_l_g/))

            call set_q1np_counts(numelq1np_g,skq1np_g,siq1np_g, &
                                 sq1npbulk_g,sq1npctrl_shared_g, &
                                 sq1npctrl_l_g,sq1npweight_l_g, &
                                 sq1npknot_l_g)

            call set_q1np_tabvint_len(siq1np_g)

          else
            write(iout,'(A,I6,A,I6,A,I6,A,I6)') &
              ' Q1NP: prototype /SURF IDs 100/200 not usable. ISURF_ID_A=', &
              isurf_id_a, ' ISURF_ID_B=', isurf_id_b, &
              ' NSEG_SURF=', nseg_surf, ' NSEG_SURF_B=', nseg_surf_b
            call set_q1np_counts(0,0,0,0,0,0,0,0)
          endif
!
        endif
!     
      enddo

      if (numelq1np_g > 0) then
!        
!         Store inverse connectivity of KQ1NP_TAB  
          call my_alloc(kq1np_tab_inv, numels, "KQ1NP_TAB_INV", stat=stat)
          if(stat /= 0) call ancmsg(msgid=268,anmode=anstop, &
                                    msgtype=msgerror, &
                                    c1='KQ1NP_TAB_INV')     
          kq1np_tab_inv(1:numels) = 0 
          do i=1, numelq1np_g
            kq1np_tab_inv(kq1np_tab(10,i)) = i
          end do

!         Define Gauss scheme for Q1Np elements (P_TEST/Q_TEST fixed in this prototype).
          np_u_q1np = p_test + 1
          np_v_q1np = q_test + 1
          np_t_q1np = 2
          call q1np_init_gauss_scheme_starter(np_u_q1np,np_v_q1np, &
                                             np_t_q1np)
          
!         Optional debug: print Gauss points in reference coordinates
!         Only when IDEBUG_Q1NP >= 2
          if (idebug_q1np >= 2) then
            write(*,'(A)') ' '
            print *, '=============================================='
            print *, 'NEW Q1NP-GAUSS-SCHEMA:'
            print *, '=============================================='
            write(*,'(A)') '   ---- Q1NP GAUSS QUADRATURE SCHEME ----'
            write(*,'(A, I3, A, I3, A, I3)') &
              '      U points: ', q1np_np_u_g, &
              '   |  V points: ', q1np_np_v_g, &
              '   |  T points: ', q1np_np_t_g
            write(*,'(A)')  '--------------------------------------------------------------------------'
            write(*,'(A)')  '  #  i_U  i_V  i_T     GP_U      GP_V      GP_T    GAUSS_WEIGHT'
            write(*,'(A)')  '--------------------------------------------------------------------------'
            gp_index = 0
            do it = 1, q1np_np_t_g
              do j = 1, q1np_np_v_g
                do i = 1, q1np_np_u_g
                  gp_index = gp_index + 1
!                 Gauss weight = product of 1D weights (Q1NP_WTAB is NURBS
!                 control-point weights, size SQ1NPCTRL_SHARED_G, not per GP)
                  write(*,'(I4,3I5,4F10.3)') &
                    gp_index, i, j, it, &
                    q1np_gp_u_g(i), q1np_gp_v_g(j), q1np_gp_t_g(it), &
                    q1np_gw_u_g(i) *q1np_gw_v_g(j)* q1np_gw_t_g(it)
                enddo
              enddo
            enddo
            write(*,'(A)')  '--------------------------------------------------------------------------'
          endif

          
!         Promote control points to real mesh nodes
          if (numelq1np_g > 0 .and. sq1npctrl_shared_g > 0) then
            numnod_old = numnod
            numnod_est = numnod + sq1npctrl_shared_g
              sx_old  = sx
              sv_old  = sv
              sd_old  = sd
              sms_old = sms
              sin_old = sin
              svr_old = svr
              sdr_old = sdr

              sx_new  = 3*numnod_est
              sv_new  = 3*numnod_est
              sd_new  = 5*numnod_est
              sms_new = numnod_est
              sin_new = numnod_est*max(iroddl,iroddl0)
              svr_new = 3*numnod_est*max(iroddl,iroddl0)
              if(isecut>0 .or. iisrot>0 .or. impose_dr>0 .or. idrot == 1) then
                sdr_new = 3*numnod_est*max(iroddl,iroddl0)
              else
                sdr_new = 0
              endif

!=======================================================================
!           Reallocate node arrays to accommodate new control point nodes
!=======================================================================
!           Step 1: Save old data to temporary arrays
!=======================================================================
              call my_alloc(x_save, sx_old, "X_SAVE", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='X_SAVE')
              x_save(1:sx_old) = x(1:sx_old)
              
              call my_alloc(v_save, sv_old, "V_SAVE", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='V_SAVE')
              v_save(1:sv_old) = v(1:sv_old)
              
              call my_alloc(ms_save, sms_old, "MS_SAVE", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='MS_SAVE')
              ms_save(1:sms_old) = ms(1:sms_old)
              
              if(sd_old > 0) then
                call my_alloc(d_save, sd_old, "D_SAVE", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='D_SAVE')
                d_save(1:sd_old) = d(1:sd_old)
              endif
              
              if(sin_old > 0) then
                call my_alloc(in_save, sin_old, "IN_SAVE", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='IN_SAVE')
                in_save(1:sin_old) = in(1:sin_old)
              endif
              
              if(svr_old > 0) then
                call my_alloc(vr_save, svr_old, "VR_SAVE", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='VR_SAVE')
                vr_save(1:svr_old) = vr(1:svr_old)
              endif
              
              if(sdr_old > 0) then
                call my_alloc(dr_save, sdr_old, "DR_SAVE", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='DR_SAVE')
                dr_save(1:sdr_old) = dr(1:sdr_old)
              endif
              
              call my_alloc(xyzref_save, sx_old, "XYZREF_SAVE", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='XYZREF_SAVE')
              xyzref_save(1:sx_old) = xyzref(1:sx_old)

!=======================================================================
!           Step 2: Reallocate arrays to new size
!=======================================================================
              call my_dealloc(x)
              call my_alloc(x, sx_new, "X", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='X')
              
              call my_dealloc(v)
              call my_alloc(v, sv_new, "V", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='V')
              
              if(sd_new > 0) then
                call my_dealloc(d)
                call my_alloc(d, sd_new, "D", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='D')
              endif
              
              call my_dealloc(ms)
              call my_alloc(ms, sms_new, "MS", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='MS')
              
              if(sin_new > 0) then
                call my_dealloc(in)
                call my_alloc(in, sin_new, "IN", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='IN')
              endif
              
              if(svr_new > 0) then
                call my_dealloc(vr)
                call my_alloc(vr, svr_new, "VR", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='VR')
              endif
              
              if(sdr_new > 0) then
                call my_dealloc(dr)
                call my_alloc(dr, sdr_new, "DR", stat=stat)
                if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                          msgtype=msgerror, c1='DR')
              endif
              
              call my_dealloc(xyzref)
              call my_alloc(xyzref, sx_new, "XYZREF", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='XYZREF')

!=======================================================================
!           Step 3: Copy old data back and initialize new indices to zero
!=======================================================================
              x(1:sx_old) = x_save(1:sx_old)
              x(sx_old+1:sx_new) = zero
              
              v(1:sv_old) = v_save(1:sv_old)
              v(sv_old+1:sv_new) = zero
              
              if(sd_new > 0) then
                if(sd_old > 0) then
                  d(1:sd_old) = d_save(1:sd_old)
                  d(sd_old+1:sd_new) = zero
                else
                  d(1:sd_new) = zero
                endif
              endif
              
              ms(1:sms_old) = ms_save(1:sms_old)
              ms(sms_old+1:sms_new) = zero
              
              if(sin_new > 0) then
                if(sin_old > 0) then
                  in(1:sin_old) = in_save(1:sin_old)
                  in(sin_old+1:sin_new) = zero
                else
                  in(1:sin_new) = zero
                endif
              endif
              
              if(svr_new > 0) then
                if(svr_old > 0) then
                  vr(1:svr_old) = vr_save(1:svr_old)
                  vr(svr_old+1:svr_new) = zero
                else
                  vr(1:svr_new) = zero
                endif
              endif
              
              if(sdr_new > 0) then
                if(sdr_old > 0) then
                  dr(1:sdr_old) = dr_save(1:sdr_old)
                  dr(sdr_old+1:sdr_new) = zero
                else
                  dr(1:sdr_new) = zero
                endif
              endif
              
              xyzref(1:sx_old) = xyzref_save(1:sx_old)
              xyzref(sx_old+1:sx_new) = zero

!=======================================================================
!           Step 4: Deallocate temporary save arrays
!=======================================================================
              call my_dealloc(x_save)
              call my_dealloc(v_save)
              call my_dealloc(ms_save)
              call my_dealloc(d_save)
              call my_dealloc(in_save)
              call my_dealloc(vr_save)
              call my_dealloc(dr_save)
              call my_dealloc(xyzref_save)

              sx  = sx_new
              sv  = sv_new
              sd  = sd_new
              sms = sms_new
              sin = sin_new
              svr = svr_new
              sdr = sdr_new

!=======================================================================
!           Step 5: Reallocate kinematic arrays after control point promotion
!=======================================================================
!           Reallocate kinematic arrays after control point promotion
              call my_alloc(iwcont_tmp, 5*numnod_est, "IWCONT_TMP", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='IWCONT_TMP')
              iwcont_tmp(1:5*numnod_old) = iwcont(1:5*numnod_old)
              iwcont_tmp(5*numnod_old+1:5*numnod_est) = 0
              call my_dealloc(iwcont)
              call move_alloc(iwcont_tmp, iwcont)
              
              call my_alloc(iwcin2_tmp, 2*numnod_est, "IWCIN2_TMP", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='IWCIN2_TMP')
              iwcin2_tmp(1:2*numnod_old) = iwcin2(1:2*numnod_old)
              iwcin2_tmp(2*numnod_old+1:2*numnod_est) = 0
              call my_dealloc(iwcin2)
              call move_alloc(iwcin2_tmp, iwcin2)
              
              call my_alloc(ikine1lag_tmp, 3*numnod_est, "IKINE1LAG_TMP", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='IKINE1LAG_TMP')
              ikine1lag_tmp(1:3*numnod_old) = ikine1lag(1:3*numnod_old)
              ikine1lag_tmp(3*numnod_old+1:3*numnod_est) = 0
              call my_dealloc(ikine1lag)
              call move_alloc(ikine1lag_tmp, ikine1lag)
!
            call q1np_promote_cp_to_nodes( &
                         numnod_old, numnod, &
                         x, v, d, ms, in, vr, dr, &
                         q1np_cptab, iq1np_tab, kq1np_tab, &
                         numelq1np_g, numnod_cp_added, &
                         sx, sv, sd, sms, sin, svr, sdr, iout)
            

!=======================================================================
!           Ensure DSDOF matches actual NUMNOD after promotion
!=======================================================================
            if (numnod_cp_added > 0) then
              if (allocated(dsdof)) then
                if (size(dsdof) < numnod) then
                  call my_alloc(dsdof_new, numnod, "DSDOF_NEW", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='DSDOF_NEW')
                  else
                  dsdof_new(1:size(dsdof)) = dsdof(1:size(dsdof))
                  dsdof_new(size(dsdof)+1:numnod) = 0
                  call my_dealloc(dsdof)
                    call move_alloc(dsdof_new, dsdof)
                  endif
                endif
              else
!                 DSDOF was not allocated, allocate it now
                call my_alloc(dsdof, numnod, "DSDOF", stat=stat)
                if(stat /= 0) then
                  call ancmsg(msgid=268,anmode=aninfo, &
                              msgtype=msgerror, c1='DSDOF')
                else
                dsdof(1:numnod) = 0
                endif
              endif
            endif

!-----------------------------------------------------------------------
!           ADDCNE / ADDCNE_PXFEM sized at first alloc (lectur ~1472);
!           DOMETIS uses ADDCNE as ADSKY(0:*) with loops 1:NUMNOD+1.
!           Reallocate if Q1NP promotion increased NUMNOD.
!-----------------------------------------------------------------------
            if (numnod > numnod_old) then
              call my_dealloc(addcne)
              call my_alloc(addcne, numnod+2, "ADDCNE", stat=stat, lower_bound=0)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                   msgtype=msgerror, c1='ADDCNE')
              addcne(0:numnod+1) = 0
              call my_dealloc(addcne_pxfem)
              call my_alloc(addcne_pxfem, numnod+2, "ADDCNE_PXFEM", stat=stat, lower_bound=0)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                   msgtype=msgerror, c1='ADDCNE_PXFEM')
              addcne_pxfem(0:numnod+1) = 0
            endif

!=======================================================================
!           Keep BC storage consistent with the promoted node count
!=======================================================================
            if (numnod_cp_added > 0) then
              sicode = numnod
              siskew = numnod

              if (allocated(icode)) then
                if (size(icode) < numnod) then
                  call my_alloc(icode_new, numnod, "ICODE_NEW", stat=stat)
                  if (stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='ICODE_NEW')
                  else
                    icode_new(1:size(icode)) = icode(1:size(icode))
                    icode_new(size(icode)+1:numnod) = 0
                    call my_dealloc(icode)
                    call move_alloc(icode_new, icode)
                  endif
                endif
              else
                call my_alloc(icode, numnod, "ICODE", stat=stat)
                if (stat /= 0) then
                  call ancmsg(msgid=268,anmode=aninfo, &
                              msgtype=msgerror, c1='ICODE')
                else
                  icode(1:numnod) = 0
                endif
              endif

              if (allocated(iskew)) then
                if (size(iskew) < numnod) then
                  call my_alloc(iskew_new, numnod, "ISKEW_NEW", stat=stat)
                  if (stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='ISKEW_NEW')
                  else
                    iskew_new(1:size(iskew)) = iskew(1:size(iskew))
                    iskew_new(size(iskew)+1:numnod) = 0
                    call my_dealloc(iskew)
                    call move_alloc(iskew_new, iskew)
                  endif
                endif
              else
                call my_alloc(iskew, numnod, "ISKEW", stat=stat)
                if (stat /= 0) then
                  call ancmsg(msgid=268,anmode=aninfo, &
                              msgtype=msgerror, c1='ISKEW')
                else
                  iskew(1:numnod) = 0
                endif
              endif
            endif
            
!=======================================================================
!           Step 6: Update ITAB and ITABM1 for promoted CP nodes
!=======================================================================
            if (numnod_cp_added > 0) then
!             Reallocate ITAB to include new nodes
              call my_alloc(itab_new, numnod, "ITAB_NEW", stat=stat)
              if(stat /= 0) then
                call ancmsg(msgid=268,anmode=aninfo, &
                            msgtype=msgerror, c1='ITAB_NEW')
              else
                  itab_new(1:numnod_old) = itab(1:numnod_old)
!               Assign user IDs to the promoted CP nodes
                max_user_id = maxval(itab(1:numnod_old))
              do i = numnod_old + 1, numnod
                itab_new(i) = max_user_id + (i - numnod_old)
              enddo
              
!               Replace ITAB with new array
                call my_dealloc(itab)
                  call my_alloc(itab, numnod, "ITAB", stat=stat)
                if(stat /= 0) then
                  call ancmsg(msgid=268,anmode=aninfo, &
                              msgtype=msgerror, c1='ITAB')
                  call my_dealloc(itab_new)
                else
                  itab(1:numnod) = itab_new(1:numnod)
                    call my_dealloc(itab_new)
                  
!=======================================================================
!                 Step 7: Rebuild ITABM1 - resize if needed, then rebuild
!=======================================================================
                  sitabm1 = 2*numnod
                  
                  if (.not. allocated(itabm1) .or. size(itabm1) < sitabm1) then
                    call my_dealloc(itabm1)
                    call my_alloc(itabm1, sitabm1, "ITABM1", stat=stat)
                    if(stat /= 0) then
                      call ancmsg(msgid=268,anmode=aninfo, &
                                  msgtype=msgerror, c1='ITABM1')
                    endif
                  endif
                  
!                 Rebuild ITABM1 mapping (only if allocation succeeded)
                  if (allocated(itabm1) .and. size(itabm1) >= sitabm1) then
                    call constit(itab, itabm1, numnod)
                  endif
                endif
              endif
            endif
                                
!=======================================================================
!           Step 8: Update NUMNOD0
!=======================================================================
            if (numnod_cp_added > 0) then
!-----------------------------------------------------------------------
!           NOD2ELS is sized from S_NOD2ELS; enlarge it for extra Q1NP
!           node–element associations if needed, preserving existing data.
!-----------------------------------------------------------------------
              if (allocated(nod2els)) then
                s_nod2els_new = s_nod2els
                extra_per_el = nbulkq1np + maxq1npctrl - 8
                if (extra_per_el < 0) extra_per_el = 0
                if (numelq1np_g > 0 .and. extra_per_el > 0) then
                  s_nod2els_new = s_nod2els_new + &
                                  extra_per_el*numelq1np_g
                endif
                if (s_nod2els_new > s_nod2els) then
                  call my_alloc(nod2els_new, s_nod2els_new, "NOD2ELS_NEW", stat=stat)
                  if (stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                 msgtype=msgerror, &
                                 c1='NOD2ELS_NEW')
                  else
                    nod2els_new(1:min(s_nod2els,size(nod2els_new))) = &
                                   nod2els(1:min(s_nod2els, &
                                                 size(nod2els_new)))
                    if (s_nod2els_new > s_nod2els) then
                      nod2els_new(s_nod2els+1:s_nod2els_new) = 0
                    endif
                    call my_dealloc(nod2els)
                    call move_alloc(nod2els_new,nod2els)
                    s_nod2els = s_nod2els_new
                  endif
                endif
              endif
!-----------------------------------------------------------------------
!             Reallocate IFRONT, IENTRY2, FLAGKIN for new NUMNOD and
!             reinitialize (required before DOMDEC1/DOMDEC2)
!-----------------------------------------------------------------------
              sifront = 2*numnod
              deallocate(ifront%ientry)
              allocate(ifront%ientry(numnod), stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='IFRONT%IENTRY')
              deallocate(ifront%p)
              allocate(ifront%p(2,sifront), stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='IFRONT%P')
              call my_dealloc(ientry2)
              call my_alloc(ientry2, numnod, "IENTRY2", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='IENTRY2')
              call my_dealloc(flagkin)
              call my_alloc(flagkin, numnod, "FLAGKIN", stat=stat)
              if(stat /= 0) call ancmsg(msgid=268,anmode=aninfo, &
                                        msgtype=msgerror, c1='FLAGKIN')
              call ini_ifront()
              ientry2(1:numnod) = -1
              flagkin(1:numnod) = 0
            endif

!=======================================================================
!             Step 9: Resize remaining NUMNOD-based arrays
!=======================================================================
            if (numnod_cp_added > 0) then
!              
              if (allocated(icode)) then
                size_old = size(icode)
                if (size_old < numnod) then
                  sicode = numnod
                  call my_alloc(iresize_tmp, numnod, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='ICODE_TMP')
                  else
                    iresize_tmp(1:size_old) = icode(1:size_old)
                    iresize_tmp(size_old+1:numnod) = 0
                    call move_alloc(iresize_tmp, icode)
                  endif
                endif
              endif

              if (allocated(iskew)) then
                size_old = size(iskew)
                if (size_old < numnod) then
                  siskew = numnod
                  call my_alloc(iresize_tmp, numnod, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='ISKEW_TMP')
                  else
                    iresize_tmp(1:size_old) = iskew(1:size_old)
                    iresize_tmp(size_old+1:numnod) = 0
                    call move_alloc(iresize_tmp, iskew)
                  endif
                endif
              endif

              if (allocated(addcne)) then
                size_old = size(addcne)
                if (size_old < numnod+2) then
                  call my_alloc(iresize_tmp, numnod+2, "IRESIZE_TMP", stat=stat, lower_bound=0)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='ADDCNE_TMP')
                  else
                    iresize_tmp(0:size_old-1) = addcne(0:size_old-1)
                    iresize_tmp(size_old:numnod+1) = 0
                    call move_alloc(iresize_tmp, addcne)
                  endif
                endif
              endif

              if (allocated(addcne_pxfem)) then
                size_old = size(addcne_pxfem)
                if (size_old < numnod+2) then
                  call my_alloc(iresize_tmp, numnod+2, "IRESIZE_TMP", stat=stat, lower_bound=0)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='ADDCNE_PXFEM_TMP')
                  else
                    iresize_tmp(0:size_old-1) = addcne_pxfem(0:size_old-1)
                    iresize_tmp(size_old:numnod+1) = 0
                    call move_alloc(iresize_tmp, addcne_pxfem)
                  endif
                endif
              endif

              if (allocated(fxbtag)) then
                size_old = size(fxbtag)
                if (size_old < numnod) then
                  call my_alloc(iresize_tmp, numnod, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='FXBTAG_TMP')
                  else
                    iresize_tmp(1:size_old) = fxbtag(1:size_old)
                    iresize_tmp(size_old+1:numnod) = 0
                    call move_alloc(iresize_tmp, fxbtag)
                  endif
                endif
              endif

              if (allocated(itag)) then
                size_old = size(itag)
                if (size_old < numnod) then
                  call my_alloc(iresize_tmp, numnod, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='ITAG_TMP')
                  else
                    iresize_tmp(1:size_old) = itag(1:size_old)
                    iresize_tmp(size_old+1:numnod) = 0
                    call move_alloc(iresize_tmp, itag)
                  endif
                endif
              endif

              if (allocated(nod2sp)) then
                size_old = size(nod2sp)
                if (size_old < numnod) then
                  snod2sp = numnod
                  call my_alloc(iresize_tmp, numnod, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='NOD2SP_TMP')
                  else
                    iresize_tmp(1:size_old) = nod2sp(1:size_old)
                    iresize_tmp(size_old+1:numnod) = 0
                    call move_alloc(iresize_tmp, nod2sp)
                  endif
                endif
              endif

              if (allocated(knod2els)) then
                size_old = size(knod2els)
                if (size_old < numnod+1) then
                  call my_alloc(iresize_tmp, numnod+1, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='KNOD2ELS_TMP')
                  else
                    iresize_tmp(1:size_old) = knod2els(1:size_old)
                    iresize_tmp(size_old+1:numnod+1) = 0
                    call move_alloc(iresize_tmp, knod2els)
                  endif
                endif
              endif

              if (allocated(knod2elc)) then
                size_old = size(knod2elc)
                if (size_old < numnod+1) then
                  call my_alloc(iresize_tmp, numnod+1, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='KNOD2ELC_TMP')
                  else
                    iresize_tmp(1:size_old) = knod2elc(1:size_old)
                    iresize_tmp(size_old+1:numnod+1) = 0
                    call move_alloc(iresize_tmp, knod2elc)
                  endif
                endif
              endif

              if (allocated(knod2eltg)) then
                size_old = size(knod2eltg)
                if (size_old < numnod+1) then
                  call my_alloc(iresize_tmp, numnod+1, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='KNOD2ELTG_TMP')
                  else
                    iresize_tmp(1:size_old) = knod2eltg(1:size_old)
                    iresize_tmp(size_old+1:numnod+1) = 0
                    call move_alloc(iresize_tmp, knod2eltg)
                  endif
                endif
              endif

              if (allocated(knod2el1d)) then
                size_old = size(knod2el1d)
                if (size_old < numnod+1) then
                  call my_alloc(iresize_tmp, numnod+1, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='KNOD2EL1D_TMP')
                  else
                    iresize_tmp(1:size_old) = knod2el1d(1:size_old)
                    iresize_tmp(size_old+1:numnod+1) = 0
                    call move_alloc(iresize_tmp, knod2el1d)
                  endif
                endif
              endif

              if (allocated(knod2elq)) then
                size_old = size(knod2elq)
                if (size_old < numnod+1) then
                  call my_alloc(iresize_tmp, numnod+1, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='KNOD2ELQ_TMP')
                  else
                    iresize_tmp(1:size_old) = knod2elq(1:size_old)
                    iresize_tmp(size_old+1:numnod+1) = 0
                    call move_alloc(iresize_tmp, knod2elq)
                  endif
                endif
              endif

              if (allocated(knod2elig3d)) then
                size_old = size(knod2elig3d)
                if (size_old < numnod+1) then
                  call my_alloc(iresize_tmp, numnod+1, "IRESIZE_TMP", stat=stat)
                  if(stat /= 0) then
                    call ancmsg(msgid=268,anmode=aninfo, &
                                msgtype=msgerror, c1='KNOD2ELIG3D_TMP')
                  else
                    iresize_tmp(1:size_old) = knod2elig3d(1:size_old)
                    iresize_tmp(size_old+1:numnod+1) = 0
                    call move_alloc(iresize_tmp, knod2elig3d)
                  endif
                endif
              endif
!              
            endif  
        else
          call set_q1np_counts(0,0,0,0,0,0,0,0)
        endif
      endif
! ----------------------------------------------------------------------------------------------------------------------
  end subroutine q1np_generate_main

end module q1np_generate_main_mod
