!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Chd|====================================================================
!Chd|  Q1NP_PROMOTE_CP_TO_NODES        source/elements/solid/solid_q1np/q1np_promote_cp.F90
!Chd|====================================================================
!=======================================================================
!   Promote Q1Np control points to real mesh nodes
!
!   This routine converts control points stored in Q1NP_CPTAB into
!   real mesh nodes by:
!   1. Assigning unique global node IDs starting from NUMNOD+1
!   2. Copying coordinates to X array
!   3. Initializing velocities and other nodal arrays
!   4. Updating IQ1NP_TAB to use the new node IDs
!   5. Tracking which HEX8 top nodes should have zero mass
!=======================================================================
      module q1np_promote_cp_mod
        use message_mod
        use q1np_restart_mod, only : SQ1NPCTRL_SHARED_G
        use precision_mod,  only : WP
        use constant_mod,   only : ZERO
        implicit none
      contains
!
        subroutine q1np_promote_cp_to_nodes( &
     &      numnod_in, numnod_out, &
     &      x, v, d, ms, in, vr, dr, &
     &      q1np_cptab, iq1np_tab, kq1np_tab, &
     &      numelq1np_in, numnod_cp_added, &
     &      sx, sv, sd, sms, sin, svr, sdr, iout)
!-----------------------------------------------------------------------
!     Modules
!-----------------------------------------------------------------------
          use my_alloc_mod,   only : my_alloc
          use my_dealloc_mod, only : my_dealloc
!-----------------------------------------------------------------------
!     Arguments
!-----------------------------------------------------------------------
          implicit none
          integer, intent(in)    :: numnod_in
          integer, intent(inout)   :: numnod_out
          integer, intent(in)    :: numelq1np_in
          integer, intent(inout)   :: numnod_cp_added
          integer, intent(inout) :: iq1np_tab(:)
          integer, intent(inout) :: kq1np_tab(:, :)
          real(kind=WP), intent(inout) :: x(:)
          real(kind=WP), intent(inout) :: v(:) 
          real(kind=WP), intent(inout) :: d(:)
          real(kind=WP), intent(inout) :: ms(:)
          real(kind=WP), intent(inout) :: in(:)
          real(kind=WP), intent(inout) :: vr(:)
          real(kind=WP), intent(inout) :: dr(:)
          real(kind=WP), intent(in)    :: q1np_cptab(:, :)
!     Explicit sizes of nodal arrays (for debug / bounds checking)
          integer, intent(in) :: sx, sv, sd, sms, sin, svr, sdr
          integer, intent(in) :: iout
!-----------------------------------------------------------------------
!     Local variables
!-----------------------------------------------------------------------
          integer, parameter :: IDEBUG_Q1NP = 0
          integer :: i, iel, cp_idx, cp_idx_old, new_node_id
          integer :: offset_ctrl, n_ctrl
          integer, allocatable :: cp_to_node_map(:)
          integer :: max_cp_id, max_new_node
          integer :: req_sx, req_sv, req_sd, req_sms, req_sin, req_svr, req_sdr
!=======================================================================
!   Step 1: Determine number of unique control points
!=======================================================================
          if (numelq1np_in == 0 .or. SQ1NPCTRL_SHARED_G == 0) then
            numnod_out      = numnod_in
            numnod_cp_added = 0
            return
          end if
          max_cp_id   = SQ1NPCTRL_SHARED_G
          max_new_node = numnod_in + max_cp_id
!=======================================================================
!   Debug bounds checks for nodal arrays
!=======================================================================
          req_sx  = 3*(max_new_node-1) + 3
          req_sv  = req_sx
          req_sd  = 5*max_new_node
          req_sms = max_new_node
          req_sin = max_new_node
          req_svr = 3*max_new_node
          req_sdr = 3*max_new_node
          if (req_sx > sx .or. req_sv > sv .or. &
     &        req_sd > sd .or. &
     &        (req_sms > sms .and. sms > 0) .or. &
     &        (req_sin > sin .and. sin > 0) .or. &
     &        (req_svr > svr .and. svr > 0) .or. &
     &        (req_sdr > sdr .and. sdr > 0)) then
            write(iout,'(A)') &
     &        ' Q1Np ERROR: Q1NP_PROMOTE_CP_TO_NODES array bounds check failed'
            write(iout,'(A,3I12)') &
     &        '  NUMNOD_IN, MAX_CP_ID, MAX_NEW_NODE =', &
     &        numnod_in, max_cp_id, max_new_node
            write(iout,'(A,7I12)') &
     &        '  Required sizes SX,SV,SD,SMS,SIN,SVR,SDR =', &
     &        req_sx, req_sv, req_sd, req_sms, req_sin, req_svr, req_sdr
            write(iout,'(A,7I12)') &
     &        '  Actual   sizes SX,SV,SD,SMS,SIN,SVR,SDR =', &
     &        sx, sv, sd, sms, sin, svr, sdr
            if (IDEBUG_Q1NP > 0) then
              write(*,'(A)') &
     &          ' Q1Np ERROR: Q1NP_PROMOTE_CP_TO_NODES array bounds check failed'
              write(*,'(A,3I12)') &
     &          '  NUMNOD_IN, MAX_CP_ID, MAX_NEW_NODE =', &
     &          numnod_in, max_cp_id, max_new_node
              write(*,'(A,7I12)') &
     &          '  Required sizes SX,SV,SD,SMS,SIN,SVR,SDR =', &
     &          req_sx, req_sv, req_sd, req_sms, req_sin, req_svr, req_sdr
              write(*,'(A,7I12)') &
     &          '  Actual   sizes SX,SV,SD,SMS,SIN,SVR,SDR =', &
     &          sx, sv, sd, sms, sin, svr, sdr
            end if
            call ancmsg(MSGID=364, MSGTYPE=MSGERROR, ANMODE=ANINFO, &
     &        C1='Q1NP control-point promotion array bounds')
            numnod_out      = numnod_in
            numnod_cp_added = 0
            return
          end if
          call my_alloc(cp_to_node_map, max_cp_id, "CP_TO_NODE_MAP")
          cp_to_node_map = 0
!=======================================================================
!   Step 2: Assign new node IDs to each control point
!=======================================================================
          numnod_cp_added = 0
          do cp_idx = 1, max_cp_id
            new_node_id = numnod_in + cp_idx
            cp_to_node_map(cp_idx) = new_node_id
            numnod_cp_added = numnod_cp_added + 1
          end do
!=======================================================================
!   Step 3: Copy control point coordinates to Node array X
!=======================================================================
          do cp_idx = 1, max_cp_id
            new_node_id = cp_to_node_map(cp_idx)
            x(3*(new_node_id-1)+1) = q1np_cptab(1, cp_idx)
            x(3*(new_node_id-1)+2) = q1np_cptab(2, cp_idx)
            x(3*(new_node_id-1)+3) = q1np_cptab(3, cp_idx)
!           Initialize kinematic arrays to zero (only if present)
            v(3*(new_node_id-1)+1) = ZERO
            v(3*(new_node_id-1)+2) = ZERO
            v(3*(new_node_id-1)+3) = ZERO
            d(3*(new_node_id-1)+1) = ZERO
            d(3*(new_node_id-1)+2) = ZERO
            d(3*(new_node_id-1)+3) = ZERO
            if (sms > 0) ms(new_node_id) = ZERO
            if (sin > 0) in(new_node_id) = ZERO
            if (svr > 0) then
              vr(3*(new_node_id-1)+1) = ZERO
              vr(3*(new_node_id-1)+2) = ZERO
              vr(3*(new_node_id-1)+3) = ZERO
            end if
            if (sdr > 0) then
              dr(3*(new_node_id-1)+1) = ZERO
              dr(3*(new_node_id-1)+2) = ZERO
              dr(3*(new_node_id-1)+3) = ZERO
            end if
          end do
!=======================================================================
!   Step 4: Update IQ1NP_TAB to use new node IDs
!=======================================================================
          do iel = 1, numelq1np_in
            n_ctrl      = kq1np_tab(3, iel)
            offset_ctrl = kq1np_tab(4, iel)
            do i = 1, n_ctrl
              cp_idx_old = iq1np_tab(offset_ctrl + i - 1)
              if (cp_idx_old > 0 .and. cp_idx_old <= max_cp_id) then
                iq1np_tab(offset_ctrl + i - 1) = cp_to_node_map(cp_idx_old)
              end if
            end do
          end do
!=======================================================================
!   Step 5: Update NUMNOD
!=======================================================================
          numnod_out = numnod_in + numnod_cp_added
!=======================================================================
!   Debug: print summary
!=======================================================================
          if (IDEBUG_Q1NP > 0) then
            write(iout,'(A,I10,A)') &
     &        'Q1NP DEBUG: Added ', numnod_cp_added, ' control point nodes'
            write(iout,'(A,I10)') &
     &        'Q1NP DEBUG: New NUMNOD = ', numnod_out
          end if
          call my_dealloc(cp_to_node_map)
          return
        end subroutine q1np_promote_cp_to_nodes
!
      end module q1np_promote_cp_mod

