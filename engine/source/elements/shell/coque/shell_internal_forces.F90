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
      module shell_internal_forces_mod
      contains

! ======================================================================================================================
!! \brief  Gather node data and launch GPU shell pipeline asynchronously
!! \details Gathers node positions/velocities from NODES into per-SU
!!          contiguous SoA buffers, then enqueues H2D + 3 kernels + D2H
!!          and (optionally) the min-dt reduction onto each SU's CUDA stream.
!!          Returns immediately — NO synchronization.  The caller must
!!          call gpu_shell_sync_scatter() later to collect results.
!!
!!          Intended usage in resol.F:
!!            CALL gpu_shell_launch_async(...)   ! enqueue GPU work
!!            CALL FORINTC(...)                  ! CPU shell forces  (hides GPU latency)
!!            CALL FORINT(...)                   ! CPU solid forces  (hides GPU latency)
!!            CALL gpu_shell_sync_scatter(...)   ! collect GPU results
! ======================================================================================================================
        subroutine gpu_shell_launch_async(NODES, SHELLS, dt1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only: WP
          use nodal_arrays_mod
          use shell_gpu_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: NODES    !< Global nodal arrays (positions, velocities)
          type(SHELLS_), intent(inout) :: SHELLS         !< GPU shell super-group data
          real(WP), intent(in) :: dt1                    !< Current cycle time step
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
          integer :: SU, NSU
          integer, save :: icall = 0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
          icall = icall + 1
          NSU = size(SHELLS%LAW2)

          ! One-time summary: print GPU super-group element counts and ISMSTR
          if (icall == 1) then
            do SU = 1, NSU
              write(6,"(A,I5,A,I6,A,I6,A,I5)") &
              &" [GPU-CFG] SU=", SU, &
              &"  NUMELC=", SHELLS%LAW2(SU)%numelc, &
              &"  NUMNOD=", SHELLS%LAW2(SU)%numnod, &
              &"  ISMSTR=", SHELLS%LAW2(SU)%ismstr
            end do
          end if

          ! ==================================================================
          ! STEP 1: Upload NODES%X/V/VR to device ONCE via global handle.
          !   Also zeros the global force accumulation arrays and records
          !   the upload_done event so that SU streams can wait on it.
          ! ==================================================================
          call shell_gpu_global_upload_nodes(SHELLS%global_handle, NODES%X, NODES%V, NODES%VR)

          ! ==================================================================
          ! STEP 2: Launch each SU's 3 kernels asynchronously.
          !   Each SU stream waits on the global upload_done event, runs
          !   K1→K2→K3, and records its kernels_done event.
          !   The SUs can potentially overlap on the GPU.
          ! ==================================================================
          do SU = 1, NSU
            if (SHELLS%LAW2(SU)%numelc > 0) then
              ! shell_gpu_full_step_async now waits on global upload_done
              ! and runs kernels only — no per-SU H2D or D2H.
              call shell_gpu_full_step_async(SHELLS%LAW2(SU)%handle, dt1, &
                NODES%X, NODES%V, NODES%VR, &
                SHELLS%raw_gpu_to_cpu_global)
            end if
          end do

          ! ==================================================================
          ! STEP 3: Make global stream wait for all SU kernels to complete,
          !   then start the single D2H of the global force arrays.
          ! ==================================================================
          do SU = 1, NSU
            if (SHELLS%LAW2(SU)%numelc > 0) then
              call shell_gpu_global_wait_su(SHELLS%global_handle, &
                SHELLS%LAW2(SU)%handle)
            end if
          end do
          call shell_gpu_global_download_forces(SHELLS%global_handle, &
            SHELLS%raw_gpu_to_cpu_global)

          ! ==================================================================
          ! STEP 4: START ASYNC MIN-DT REDUCTION PER SU
          !   Only when the CPU CDT3 would also reduce DT2T from element dt
          !   (i.e. NODADT=0 AND IDTMIN3/=0, mapped to compute_sti==2).
          ! ==================================================================
          if (SHELLS%reduce_elem_dt) then
            do SU = 1, NSU
              if (SHELLS%LAW2(SU)%numelc > 0) then
                call shell_gpu_min_dt(SHELLS%LAW2(SU)%handle, &
                  SHELLS%LAW2(SU)%dtfac, SHELLS%LAW2(SU)%dt_min_result)
              end if
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine gpu_shell_launch_async

! ======================================================================================================================
!! \brief  Synchronize GPU streams and scatter results into global nodal arrays
!! \details For each SU: blocks until the CUDA stream finishes, then
!!          scatters GPU-computed forces/moments/stiffness into NODES,
!!          collects min-dt results, and (if ipri>0) downloads internal
!!          energy for PARTSAV accumulation.
!!
!!          Must be called AFTER gpu_shell_launch_async() and after any
!!          CPU work that should overlap with GPU execution.
! ======================================================================================================================
        subroutine gpu_shell_sync_scatter(NODES, SHELLS, dt2t, partsav, npsav, npart, ipri)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only: WP
          use nodal_arrays_mod
          use shell_gpu_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: NODES    !< Global nodal arrays (forces accumulated here)
          type(SHELLS_), intent(inout) :: SHELLS         !< GPU shell super-group data
          integer, intent(in) :: npsav, npart
          real(WP), intent(inout) :: dt2t                !< Accumulator for next cycle time step
          real(WP), intent(inout) :: partsav(npsav,npart)
          integer, intent(in) :: ipri                    !< Print level for diagnostic
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
          integer :: SU, i, j, NSU, NN
          integer, save :: icall = 0
          real(WP) :: dt_gpu_min, eint_total
          integer :: part_id
          ! Diagnostic variables for controlling element
          integer  :: dt_min_su, dt_min_j, dt_min_ngl
          real(WP) :: dt_min_aldt, dt_min_ssp, dt_min_area
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
          icall = icall + 1
          dt_gpu_min = huge(1.0_WP)
          dt_min_su  = 0
          dt_min_j   = 0
          dt_min_aldt = 0.0_WP
          dt_min_ssp  = 0.0_WP
          dt_min_area = 0.0_WP

          NSU = size(SHELLS%LAW2)
          NN  = SHELLS%numnod_global

          ! ==================================================================
          ! STEP 1: Synchronize the global stream.
          !   The global stream's last operation was the D2H of the force
          !   arrays (enqueued by gpu_shell_launch_async after all SU
          !   kernels_done events).  This blocks until the download completes.
          ! ==================================================================
          call shell_gpu_global_synchronize(SHELLS%global_handle)

          ! Also synchronize each SU stream (for min-dt results)
          do SU = 1, NSU
            if (SHELLS%LAW2(SU)%numelc > 0) then
              call shell_gpu_synchronize(SHELLS%LAW2(SU)%handle)
            end if
          end do

          ! ==================================================================
          ! STEP 2: Scatter global force buffer into NODES%A / AR / stifn / stifr.
          !   The D2H buffer is [8*NUMNOD] = Fx|Fy|Fz|Mx|My|Mz|STIFN|STIFR
          !   indexed by global node ID (0-based in CUDA, 1-based in Fortran).
          !   We add directly: only shell-connected nodes have nonzero values,
          !   and the arrays were zeroed before kernel launch.
          ! ==================================================================
          do i = 1, NN
            NODES%A(1,i) = NODES%A(1,i) + SHELLS%raw_gpu_to_cpu_global(i)
            NODES%A(2,i) = NODES%A(2,i) + SHELLS%raw_gpu_to_cpu_global(NN + i)
            NODES%A(3,i) = NODES%A(3,i) + SHELLS%raw_gpu_to_cpu_global(2*NN + i)
            NODES%AR(1,i) = NODES%AR(1,i) + SHELLS%raw_gpu_to_cpu_global(3*NN + i)
            NODES%AR(2,i) = NODES%AR(2,i) + SHELLS%raw_gpu_to_cpu_global(4*NN + i)
            NODES%AR(3,i) = NODES%AR(3,i) + SHELLS%raw_gpu_to_cpu_global(5*NN + i)
            NODES%stifn(i) = NODES%stifn(i) + SHELLS%raw_gpu_to_cpu_global(6*NN + i)
            NODES%stifr(i) = NODES%stifr(i) + SHELLS%raw_gpu_to_cpu_global(7*NN + i)
          end do

          ! ==================================================================
          ! STEP 3: Collect min-dt from GPU reduction per SU
          ! ==================================================================
          do SU = 1, NSU
            if (SHELLS%LAW2(SU)%numelc > 0) then
              if (SHELLS%reduce_elem_dt) then
                if (SHELLS%LAW2(SU)%dt_min_result < dt_gpu_min) then
                  dt_gpu_min = SHELLS%LAW2(SU)%dt_min_result
                  dt_min_su  = SU
                  dt_min_j   = 0
                  dt_min_aldt = 0.0_WP
                  dt_min_ssp  = SHELLS%LAW2(SU)%ssp
                  dt_min_area = 0.0_WP
                end if
              end if
            end if
          end do

          ! Download internal energy from GPU and accumulate into PARTSAV
          if(ipri > 0) then
            do SU = 1, NSU
              call shell_gpu_download_energy(SHELLS%LAW2(SU)%handle, SHELLS%LAW2(SU)%eint)
              do j = 1, SHELLS%LAW2(SU)%numelc
                eint_total = SHELLS%LAW2(SU)%eint(j) &
                  + SHELLS%LAW2(SU)%eint(SHELLS%LAW2(SU)%numelc + j)
                part_id = SHELLS%LAW2(SU)%part_id(j)
                PARTSAV(1, part_id) = PARTSAV(1, part_id) + eint_total
              end do
            end do
          end if

          ! Reduce DT2T with the GPU element time step
          if (SHELLS%reduce_elem_dt .and. dt_gpu_min < dt2t) then
            if (icall-1 <= 5) then
              write(6,"(A,ES12.4,A,ES12.4)") &
              &" [GPU] dt_elem_min = ", dt_gpu_min, " reducing dt2t from ", dt2t
            endif
            dt2t = dt_gpu_min
          endif

          ! Periodic diagnostic: print controlling element details
          if (mod(icall-1, 100) == 0 .or. icall-1 <= 20) then
            dt_min_ngl = 0
            if (dt_min_su > 0 .and. dt_min_j > 0) then
              dt_min_ngl = SHELLS%LAW2(dt_min_su)%ngl(dt_min_j)
            end if
            write(6,"(A,I6,A,ES14.6,A,I10,A,I6,A,I8,A,ES14.6,A,ES14.6)") &
            &" [GPU-DT] CYC=", icall-1, &
            &"  dt_min=", dt_gpu_min, &
            &"  SU=", dt_min_su, &
            &"  elem=", dt_min_j, &
            &"  NGL=", dt_min_ngl, &
            &"  ALDT=", dt_min_aldt, &
            &"  SSP=", dt_min_ssp
          end if
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine gpu_shell_sync_scatter

! ======================================================================================================================
!! \brief  Legacy convenience wrapper — launch + sync in a single call
!! \details Kept for backward compatibility.  Equivalent to calling
!!          gpu_shell_launch_async() followed immediately by
!!          gpu_shell_sync_scatter(), with no CPU work in between.
! ======================================================================================================================
        subroutine gpu_shell_internal_forces(NODES, SHELLS, dt1, dt2t, partsav, npsav, npart, ipri)
          use precision_mod, only: WP
          use nodal_arrays_mod
          use shell_gpu_mod
          implicit none
          type(nodal_arrays_), intent(inout) :: NODES
          type(SHELLS_), intent(inout) :: SHELLS
          integer, intent(in) :: npsav, npart
          real(WP), intent(in) :: dt1
          real(WP), intent(inout) :: dt2t
          real(WP), intent(inout) :: partsav(npsav,npart)
          integer, intent(in) :: ipri

          call gpu_shell_launch_async(NODES, SHELLS, dt1)
          call gpu_shell_sync_scatter(NODES, SHELLS, dt2t, partsav, npsav, npart, ipri)

        end subroutine gpu_shell_internal_forces

        SUBROUTINE FORINTC_PREPARE_GPU( GPU, SHELLS, NUMELC,&
        &PM , NPROPM,       GEO, NPROPG,  &
        &NODES, MAT_ELEM  , NSECT, NSLIPRING, NEXMAD, &
        &IPARG, nparg     ,IXC, IPARTC,      &
        &IGROUC    ,NGROUC, NGROUP, NIXC    ,&
        &ELBUF_TAB, DTFAC1_3, &
        &NODADT_IN, IDT1SH_IN, IDTMINS_IN, IDTMIN3_IN, &
        &HVISC_IN, HELAS_IN, HVLIN_IN )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use constant_mod
          USE MAT_ELEM_MOD
          USE ELBUFDEF_MOD
          use shell_gpu_mod, only : SHELLS_,&
          &shell_gpu_data_create, shell_gpu_allocate,&
          &shell_gpu_set_mat_params, shell_gpu_set_hg_params,&
          &shell_gpu_set_compute_sti, shell_gpu_set_ihbe,&
          &shell_gpu_upload_constant, shell_gpu_upload_ip_state,&
          &shell_gpu_pin_host_memory,&
          &shell_gpu_global_create, shell_gpu_global_pin_host,&
          &shell_gpu_set_global
          use precision_mod, only : WP
          use nodal_arrays_mod
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          INTEGER , INTENT(IN) :: GPU
          TYPE(SHELLS_), INTENT(INOUT) :: SHELLS
          TYPE(nodal_arrays_), intent(inout) :: NODES
          integer, intent(in) :: NPROPM, NPROPG,nparg,NGROUP, NIXC, NUMELC, NSECT
          integer, intent(in) :: NSLIPRING
          real(wp), intent(in) :: PM(NPROPM,*), GEO(NPROPG,*)
          TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
          INTEGER, intent(in) :: IPARG(NPARG,*), IXC(NIXC,*)
          INTEGER, intent(in) :: IGROUC(*), NGROUC
          integer, intent(in) :: IPARTC(*)
          TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
          real(wp), intent(in) :: DTFAC1_3  !< Timestep scale factor DTFAC1(3)
          integer, intent(in) :: NODADT_IN   !< Nodal dt flag (from /SCR02/)
          integer, intent(in) :: IDT1SH_IN   !< Shell dt flag (from /SCR17/)
          integer, intent(in) :: IDTMINS_IN  !< SMS shell dt flag (from /SMS/)
          integer, intent(in) :: IDTMIN3_IN  !< Shell dt control IDTMIN(3)
          real(wp), intent(in) :: HVISC_IN    !< Viscous hourglass scaling (from /SCR06R/)
          real(wp), intent(in) :: HELAS_IN    !< Elastic hourglass scaling (from /SCR06R/)
          real(wp), intent(in) :: HVLIN_IN    !< Linear  hourglass scaling (from /SCR06R/)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          INTEGER :: I, J, NG, NVC, MLW, JFT, JLT, ISOLNOD, ITHK, IPLA,&
          &NF1, IPRI, OFFSET, NSG, NEL, KFTS, ISTRA,&
          &ICNOD, ISENS_ENERGY, IEXPAN, IG, ITG1, ITG2, ITG3,&
          &NLEVXF, ISEATBELT
          INTEGER :: ISH3N, ISHPLYXFEM, IXFEM
          INTEGER :: ACTIFXFEM, ISUBSTACK
          integer super_group_size


          integer :: IFAILURE, JCVT, IGTYP, ISORTH, ISORTHG, ISRAT, ISROT, JCLOSE, JSMS
          integer :: IINT, ISMSTR, JEUL, JTUR, JTHE, JLAG, IREP,ITY,JALE, JHBE
          integer :: NPT,JIVF, JMULT,JPOR, NEXMAD
          integer :: LFT,NFT, LLT, MTN
          integer :: NUMNOD


          integer :: law2_counter(NGROUP)
          integer :: nod_counter(NGROUP)
          logical, dimension(:), allocatable :: node_mask
          integer, dimension(:), allocatable :: glob_2_gpu, gpu_2_glob, node_local_id
          integer :: SUPER_GROUP_ID(NGROUP)
          integer :: PREVIOUS_SUPER_GROUP_ID
          integer :: IPARG_PREVIOUS(NPARG)
          integer :: SU,nod,imat,ipid
          integer :: elem_counter(NGROUP)
          integer :: IT, gpu_offset, IJ1, IJ2, IJ3, IJ4, IJ5
          integer :: glob_id
          integer :: i_compute_sti
!======================================================================|
          NUMNOD = NODES%numnod
          allocate(node_mask(NUMNOD))
          allocate(glob_2_gpu(NUMNOD))
          allocate(gpu_2_glob(NUMNOD))
          allocate(node_local_id(NUMNOD))
          IPARG_PREVIOUS = 0
          glob_2_gpu = 0
          gpu_2_glob = 0
          node_mask = .false.
          ITG2 = 2*NUMELC
          ITG3 = 1+NUMELC
          IPRI = 0
          ISENS_ENERGY = 0
          law2_counter = 0
          nod_counter = 0
          elem_counter = 0
          SUPER_GROUP_ID = 0
          PREVIOUS_SUPER_GROUP_ID = 0
          super_group_size = 0
          DO IG = 1, NGROUC
            NG = IGROUC(IG)
            IF (IPARG(1, NG) == 151) THEN !  Bypass law 151
              CYCLE
            ENDIF
!---------temporarily used to avoid pass KTBUF_STR everywhere
            IF(IPARG(8,NG)==1) CYCLE
            ITY   =IPARG(5,NG)
!            IF(ITY/=3.AND.ITY/=7)GOTO 250
            OFFSET  = 0
            MLW     = IPARG(1,NG)
! MLW= 0 ----> void
! MLW = 13 ----> rigid material
            IF (MLW == 0 .OR. MLW == 13) CYCLE
            NEL     = IPARG(2,NG)
            NFT     = IPARG(3,NG)
            NPT     = IPARG(6,NG)
            JALE    = IPARG(7,NG)
            ISMSTR  = IPARG(9,NG)
            NSG     = IPARG(10,NG)
            JEUL    = IPARG(11,NG)
            JTUR    = IPARG(12,NG)
            JTHE    = IPARG(13,NG)
            JLAG    = IPARG(14,NG)
            ISTRA   = IPARG(44,NG)
            NVC     = IPARG(19,NG)
            JMULT   = IPARG(20,NG)
            JHBE    = IPARG(23,NG)
            ISH3N   = IPARG(23,NG)
            JIVF    = IPARG(24,NG)
            JPOR    = IPARG(27,NG)
            ITHK    = IPARG(28,NG)
            ISOLNOD = IPARG(28,NG)
            IPLA    = IPARG(29,NG)
            ICNOD   = IPARG(11,NG)
            IREP    = IPARG(35,NG)
            IINT    = IPARG(36,NG)
            JCVT    = IPARG(37,NG)
            IGTYP   = IPARG(38,NG)
            ISORTH  = IPARG(42,NG)
            ISORTHG = ISORTH
            ISRAT   = IPARG(40,NG)
            ISROT   = IPARG(41,NG)
            IFAILURE= IPARG(43,NG)
            KFTS    = IPARG(30,NG)
            JCLOSE  = IPARG(33,NG)
            IEXPAN  = IPARG(49,NG)
            ISHPLYXFEM  = IPARG(50,NG)
            JSMS    = IPARG(52,NG)
            IXFEM   = IPARG(54,NG)
            NLEVXF  = IPARG(65,NG)
            ACTIFXFEM=IPARG(70,NG)
            ISUBSTACK=IPARG(71,NG)
            ISEATBELT=IPARG(91,NG)
            IF (ITY == 3&  ! 4-node shell
            &.AND. MLW == 2&  ! material law 2 (Johnson-Cook)
            &.AND. IPARG(8,NG) /= 1&  ! group active
            &.AND. NPT > 0&  ! through-thickness integration (not global)
            &.AND. JHBE < 11&  ! standard formulation (not BATOZ, not QEPH)
            &.AND. IGTYP < 29&  ! not user property
            &.AND. IXFEM == 0&  ! no XFEM
            &.AND. IFAILURE == 0&  ! no failure model (simplify for mockup)
            &.AND. NSECT == 0&  ! no section (simplify for mockup)
            &.AND. NEXMAD == 0&  ! no explicit mass scaling (simplify for mockup)
            &.AND. NSLIPRING == 0 .AND. ISEATBELT == 0&  ! no slipring/seatbelt (simplify for mockup)
            &.AND. GPU == 1&  ! GPU enabled
            &.AND. ISUBSTACK == 0 .AND. ISENS_ENERGY == 0) THEN
              ! ---------------------------------------------------------
              ! Super-group homogeneity check: only compare IPARG indices
              ! that actually affect GPU kernel behavior.
              !   IPARG(6)  = NPT    : integration point count (all kernels)
              !   IPARG(9)  = ISMSTR : small-strain flag (all kernels)
              !   IPARG(23) = JHBE   : hourglass formulation (kernel 3)
              !   IPARG(28) = ITHK   : thickness update flag (kernel 2)
              !   IPARG(29) = IPLA   : plasticity algorithm (kernel 2)
              ! The previous comparison IPARG(6:71) was buggy because many
              ! indices in that range are uninitialized or irrelevant to
              ! the GPU code path, causing spurious super-group splits.
              ! ---------------------------------------------------------
              IF(super_group_size < 10000000 .AND. &
              & IPARG(6,NG)  == IPARG_PREVIOUS(6)  .AND. &
              &  IPARG(9,NG)  == IPARG_PREVIOUS(9)  .AND. &
              &  IPARG(23,NG) == IPARG_PREVIOUS(23) .AND. &
              &  IPARG(28,NG) == IPARG_PREVIOUS(28) .AND. &
              &  IPARG(29,NG) == IPARG_PREVIOUS(29)) THEN
                SUPER_GROUP_ID(NG) = PREVIOUS_SUPER_GROUP_ID
                super_group_size = super_group_size + NEL
              ELSE
                super_group_size = NEL
                PREVIOUS_SUPER_GROUP_ID = PREVIOUS_SUPER_GROUP_ID + 1
                SUPER_GROUP_ID(NG) = PREVIOUS_SUPER_GROUP_ID
                node_mask = .false.
                ! Print which GPU-relevant IPARG values changed (diagnostic)
                write(700,"(A,I6,A,I6,A,I6)") " [GPU-GROUP] NG=", NG, "  NPT=",    IPARG(6,NG), "  previous=", IPARG_PREVIOUS(6)
                write(700,"(A,I6,A,I6,A,I6)") " [GPU-GROUP] NG=", NG, "  ISMSTR=", IPARG(9,NG), "  previous=", IPARG_PREVIOUS(9)
                write(700,"(A,I6,A,I6)") " [GPU-GROUP] NG=", NG, "  JHBE=",   IPARG(23,NG), "  previous=", IPARG_PREVIOUS(23)
                write(700,"(A,I6,A,I6,A,I6)") " [GPU-GROUP] NG=", NG, "  ITHK=",   IPARG(28,NG), "  previous=", IPARG_PREVIOUS(28)
                write(700,"(A,I6,A,I6,A,I6)") " [GPU-GROUP] NG=", NG, "  IPLA=",   IPARG(29,NG), "  previous=", IPARG_PREVIOUS(29)
              ENDIF
              IPARG_PREVIOUS = IPARG(:,NG)
              IF(.TRUE.) THEN
                LFT   = 1
                LLT   = MIN(128,NEL)
                MTN   = MLW
                JFT=LFT
                JLT=LLT
                NF1 = NFT+1
                SU = SUPER_GROUP_ID(NG)
                law2_counter(SU) = law2_counter(SU) + NEL
                do i=1,NEL
                  do j=1,4
                    if (node_mask(IXC(1+j,NFT+i)) .eqv. .false.) then
                      nod_counter(SU) = nod_counter(SU) + 1
                      node_mask(IXC(1+j,NFT+i)) = .true.
                    end if
                  end do
                end do
              ENDIF
!----6---------------------------------------------------------------7---------8
            ENDIF
          END DO




          ALLOCATE(SHELLS%LAW2(PREVIOUS_SUPER_GROUP_ID))

          ! Create global (shared) node/force handle — sized for full mesh NUMNOD
          SHELLS%numnod_global = NUMNOD
          SHELLS%global_handle = shell_gpu_global_create(NUMNOD)
          ! Allocate host-side D2H force buffer [8*NUMNOD] and pin it
          allocate(SHELLS%raw_gpu_to_cpu_global(8*NUMNOD))
          SHELLS%raw_gpu_to_cpu_global = 0.0d0
          call shell_gpu_global_pin_host(NODES%X, NODES%V, NODES%VR, &
            SHELLS%raw_gpu_to_cpu_global, NUMNOD)
          write(6,*) " [GPU-GLOBAL] Pinned NODES%X/V/VR and force buffer, NUMNOD=", NUMNOD

          PREVIOUS_SUPER_GROUP_ID = 0
          DO IG = 1, NGROUC
            NG = IGROUC(IG)
            NEL     = IPARG(2,NG)
            NFT     = IPARG(3,NG)
            NPT     = IPARG(6,NG)
            NF1   = NFT+1

            IF(SUPER_GROUP_ID(NG) == 0) CYCLE ! skip groups not selected for GPU
            IF(SUPER_GROUP_ID(NG) /= PREVIOUS_SUPER_GROUP_ID) THEN
              PREVIOUS_SUPER_GROUP_ID = SUPER_GROUP_ID(NG)
              SU = SUPER_GROUP_ID(NG)
              nod = nod_counter(SU)
              nod_counter(SU) = 0 ! reset for next super group
              SHELLS%LAW2(SU)%numelc = law2_counter(SU)
              SHELLS%LAW2(SU)%numnod = nod
              allocate(SHELLS%LAW2(SU)%gpu_2_glob(nod))
              SHELLS%LAW2(SU)%npt = IPARG(6,NG)
              SHELLS%LAW2(SU)%ismstr = IPARG(9,NG)
              SHELLS%LAW2(SU)%ithk = IPARG(28,NG)
              SHELLS%LAW2(SU)%ihbe = IPARG(23,NG)
              SHELLS%LAW2(SU)%handle = shell_gpu_data_create()
              write(6,*) "Allocating GPU data for SG ", SU, " with ", SHELLS%LAW2(SU)%numelc, " elements and ", nod, " nodes."
              call shell_gpu_allocate(&
              &shells%LAW2(SU)%handle,&
              &SHELLS%LAW2(SU)%numelc,&
              &SHELLS%LAW2(SU)%numnod,&
              &SHELLS%LAW2(SU)%npt,&
              &SHELLS%LAW2(SU)%ismstr,&
              &SHELLS%LAW2(SU)%ithk)
              ! Link per-SU handle to the global node/force arrays
              call shell_gpu_set_global(SHELLS%LAW2(SU)%handle, SHELLS%global_handle)
              ! Set compute_sti mode: match CPU cforc3→chsti3→cdt3 STI flow
              !   0 = no STI (NODADT=0 AND IDTMIN3=0: CDT3 returns early)
              !   1 = CPXPY3+CHSTI3 formula (NODADT/=0 OR IDT1SH=1 OR IDTMINS=2)
              !   2 = CDT3 formula (NODADT=0, IDTMIN3/=0, IDT1SH=0)
              if (NODADT_IN /= 0 .or. IDT1SH_IN == 1 .or. IDTMINS_IN == 2) then
                i_compute_sti = 1
              else if (IDTMIN3_IN /= 0) then
                i_compute_sti = 2
              else
                i_compute_sti = 0
              end if
              call shell_gpu_set_compute_sti(SHELLS%LAW2(SU)%handle, i_compute_sti)
              ! GPU element dt should reduce DT2T only when the CPU CDT3 would
              ! do the same.  In CDT3, DT2T is reduced only when:
              !   - CDT3 is called: ISMSTR/=3 AND (NODADT==0 OR IDTMIN3/=0)
              !   - CDT3 doesn't return early: NODADT==0 AND NOT (IDTMINS==2 AND JSMS/=0)
              ! For the GPU (no SMS/JSMS), this simplifies to:
              !   NODADT==0 AND IDTMIN3/=0  ↔  compute_sti==2
              SHELLS%reduce_elem_dt = (i_compute_sti == 2)
              ! Set IHBE for non-uniform GAMA in hourglass computation
              call shell_gpu_set_ihbe(SHELLS%LAW2(SU)%handle, SHELLS%LAW2(SU)%ihbe)
              write(6,*) "Completed allocation of GPU data for super-group ", SU
!
!             Retrieve material index and property index
!
              imat = ELBUF_TAB(NG)%bufly(1)%imat
              NF1  = IPARG(3,NG) + 1
              ipid = IXC(6,NF1)
!
!             Elastic properties  (cf. sigeps02c / mat_param)
!
              SHELLS%LAW2(SU)%e_young = MAT_ELEM%mat_param(imat)%young
              SHELLS%LAW2(SU)%nu      = MAT_ELEM%mat_param(imat)%nu
              SHELLS%LAW2(SU)%g_shear = MAT_ELEM%mat_param(imat)%shear
              SHELLS%LAW2(SU)%a11     = MAT_ELEM%mat_param(imat)%young&
              &/ (ONE - MAT_ELEM%mat_param(imat)%nu**2)
              SHELLS%LAW2(SU)%a12     = SHELLS%LAW2(SU)%a11&
              &* MAT_ELEM%mat_param(imat)%nu
!
!             Johnson-Cook / Zerilli-Armstrong hardening (uparam)
!
              SHELLS%LAW2(SU)%ca      = MAT_ELEM%mat_param(imat)%uparam(1)
              SHELLS%LAW2(SU)%cb      = MAT_ELEM%mat_param(imat)%uparam(2)
              SHELLS%LAW2(SU)%cn      = MAT_ELEM%mat_param(imat)%uparam(3)
              SHELLS%LAW2(SU)%epmx    = MAT_ELEM%mat_param(imat)%uparam(4)
              SHELLS%LAW2(SU)%ymax    = MAT_ELEM%mat_param(imat)%uparam(5)
              SHELLS%LAW2(SU)%cc      = MAT_ELEM%mat_param(imat)%uparam(6)
              SHELLS%LAW2(SU)%epdr    = MAT_ELEM%mat_param(imat)%uparam(7)
              SHELLS%LAW2(SU)%fisokin = MAT_ELEM%mat_param(imat)%uparam(8)
!
!             Thermal parameters
!
              SHELLS%LAW2(SU)%rhocp   = MAT_ELEM%mat_param(imat)%therm%rhocp
              SHELLS%LAW2(SU)%tref    = MAT_ELEM%mat_param(imat)%therm%tref
              SHELLS%LAW2(SU)%tmelt   = MAT_ELEM%mat_param(imat)%therm%tmelt
!
!             Formulation-dependent: m_exp / z3 / z4  (cf. sigeps02c)
!
              SHELLS%LAW2(SU)%iform   = MAT_ELEM%mat_param(imat)%iparam(1)
              IF (SHELLS%LAW2(SU)%iform == 1) THEN
!               Zerilli-Armstrong
                SHELLS%LAW2(SU)%z3    = MAT_ELEM%mat_param(imat)%uparam(10)
                SHELLS%LAW2(SU)%z4    = MAT_ELEM%mat_param(imat)%uparam(11)
                SHELLS%LAW2(SU)%m_exp = ONE
              ELSE
!               Johnson-Cook
                SHELLS%LAW2(SU)%z3    = ZERO
                SHELLS%LAW2(SU)%z4    = ZERO
                SHELLS%LAW2(SU)%m_exp = MAT_ELEM%mat_param(imat)%uparam(10)
              ENDIF
!
!             Integer flags (iparam)
!
              SHELLS%LAW2(SU)%icc     = MAT_ELEM%mat_param(imat)%iparam(2)
              SHELLS%LAW2(SU)%vp      = MAT_ELEM%mat_param(imat)%iparam(3)
!
!             Strain-rate filter coefficient (PM(9,imat) = 2*pi*fcut)
!
              SHELLS%LAW2(SU)%asrate  = PM(9,imat)
!
!             Density, sound speed, shear correction factor
!             Note: CPU ccoef3.F sets SHF=0 for NPT==1 (no bending HG
!             for single through-thickness integration point).
!
              SHELLS%LAW2(SU)%rho      = MAT_ELEM%mat_param(imat)%rho
              SHELLS%LAW2(SU)%ssp      = PM(27,imat)
              if (NPT == 1) then
                SHELLS%LAW2(SU)%shf_coef = 0.0_WP
              else
                SHELLS%LAW2(SU)%shf_coef = GEO(38,ipid)
              endif
!
!             Plasticity algorithm
!
              SHELLS%LAW2(SU)%ipla    = IPARG(29,NG)

!             hourglass parameters (from GEO property array)
              SHELLS%LAW2(SU)%h1   = GEO(13,ipid)
              SHELLS%LAW2(SU)%h2   = GEO(14,ipid)
              SHELLS%LAW2(SU)%h3   = GEO(15,ipid)
              SHELLS%LAW2(SU)%srh1 = GEO(18,ipid)
              SHELLS%LAW2(SU)%srh2 = GEO(19,ipid)
              SHELLS%LAW2(SU)%srh3 = GEO(20,ipid)
              SHELLS%LAW2(SU)%dtfac = DTFAC1_3

              write(6,*) "Setting GPU material parameters for super-group ", SU
              call shell_gpu_set_mat_params(&
              &SHELLS%LAW2(SU)%handle,&
              &SHELLS%LAW2(SU)%e_young,&
              &SHELLS%LAW2(SU)%nu,&
              &SHELLS%LAW2(SU)%g_shear,&
              &SHELLS%LAW2(SU)%a11,&
              &SHELLS%LAW2(SU)%a12,&
              &SHELLS%LAW2(SU)%ca,&
              &SHELLS%LAW2(SU)%cb,&
              &SHELLS%LAW2(SU)%cn,&
              &SHELLS%LAW2(SU)%cc,&
              &SHELLS%LAW2(SU)%epdr,&
              &SHELLS%LAW2(SU)%epmx,&
              &SHELLS%LAW2(SU)%ymax,&
              &SHELLS%LAW2(SU)%m_exp,&
              &SHELLS%LAW2(SU)%fisokin,&
              &SHELLS%LAW2(SU)%rhocp,&
              &SHELLS%LAW2(SU)%tref,&
              &SHELLS%LAW2(SU)%tmelt,&
              &SHELLS%LAW2(SU)%asrate,&
              &SHELLS%LAW2(SU)%rho,&
              &SHELLS%LAW2(SU)%ssp,&
              &SHELLS%LAW2(SU)%shf_coef,&
              &SHELLS%LAW2(SU)%ipla,&
              &SHELLS%LAW2(SU)%vp,&
              &SHELLS%LAW2(SU)%iform,&
              &SHELLS%LAW2(SU)%icc,&
              &SHELLS%LAW2(SU)%z3,&
              &SHELLS%LAW2(SU)%z4)
              write(6,*) "Completed setting GPU material parameters for super-group ", SU

              allocate(SHELLS%LAW2(SU)%n1(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%n2(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%n3(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%n4(SHELLS%LAW2(SU)%numelc))
              ! Per-element material arrays [NUMELC]
              allocate(SHELLS%LAW2(SU)%el_thk0(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_off(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_ssp(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_rho(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_ym(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_nu(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_a11(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_g_shear(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%el_shf(SHELLS%LAW2(SU)%numelc))
              ! Per-SU H2D/D2H buffers are no longer needed — node/force
              ! arrays now live on the global handle (ShellGPUGlobal).
              ! The old raw_cpu_to_gpu / raw_gpu_to_cpu / pointer aliases
              ! are kept but not allocated (they remain null).
              !IP state arrays [NPT*NUMELC]
              allocate(SHELLS%LAW2(SU)%sigxx(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%sigyy(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%sigxy(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%sigyz(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%sigzx(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%pla(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%epsd_ip(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%sigbakxx(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%sigbakyy(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%sigbakxy(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%tempel(NPT*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%eint(2*SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%part_id(SHELLS%LAW2(SU)%numelc))
              allocate(SHELLS%LAW2(SU)%ngl(SHELLS%LAW2(SU)%numelc))
              SHELLS%LAW2(SU)%eint = 0.0d0
              SHELLS%LAW2(SU)%sigxx = 0.0d0
              SHELLS%LAW2(SU)%sigyy = 0.0d0
              SHELLS%LAW2(SU)%sigxy = 0.0d0
              SHELLS%LAW2(SU)%sigyz = 0.0d0
              SHELLS%LAW2(SU)%sigzx = 0.0d0
              SHELLS%LAW2(SU)%pla = 0.0d0
              SHELLS%LAW2(SU)%epsd_ip = 0.0d0
              SHELLS%LAW2(SU)%sigbakxx = 0.0d0
              SHELLS%LAW2(SU)%sigbakyy = 0.0d0
              SHELLS%LAW2(SU)%sigbakxy = 0.0d0
              SHELLS%LAW2(SU)%tempel = 0.0d0
              elem_counter(SU) = 0
              node_mask = .false.
              node_local_id = 0
            endif
!
!           Copy per-IP state from ELBUF_TAB into flat GPU arrays
!           Layout in lbuf%sig: [sigxx(1:NEL), sigyy(1:NEL), sigxy(1:NEL), sigyz(1:NEL), sigzx(1:NEL)]
!           Layout in GPU arrays: (IT-1)*total_numelc + elem_counter + i
!
            NF1 = NFT + 1
            do IT = 1, NPT
              do i = 1, NEL
                gpu_offset = (IT-1)*SHELLS%LAW2(SU)%numelc&
                &+ elem_counter(SU) + i
!               Stress components from lbuf%sig (5*NEL flat array)
                IJ1 = 0
                IJ2 = NEL
                IJ3 = 2*NEL
                IJ4 = 3*NEL
                IJ5 = 4*NEL
                if (ELBUF_TAB(NG)%bufly(1)%l_sig > 0) then
                  SHELLS%LAW2(SU)%sigxx(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ1+i)
                  SHELLS%LAW2(SU)%sigyy(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ2+i)
                  SHELLS%LAW2(SU)%sigxy(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ3+i)
                  SHELLS%LAW2(SU)%sigyz(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ4+i)
                  SHELLS%LAW2(SU)%sigzx(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ5+i)
                endif
!               Plastic strain
                if (ELBUF_TAB(NG)%bufly(1)%l_pla > 0) then
                  SHELLS%LAW2(SU)%pla(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%pla(i)
                endif
!               Strain rate
                if (ELBUF_TAB(NG)%bufly(1)%l_epsd > 0) then
                  SHELLS%LAW2(SU)%epsd_ip(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%epsd(i)
                endif
!               Back-stress components from lbuf%sigb (same layout as sig)
                if (ELBUF_TAB(NG)%bufly(1)%l_sigb > 0) then
                  SHELLS%LAW2(SU)%sigbakxx(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sigb(IJ1+i)
                  SHELLS%LAW2(SU)%sigbakyy(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sigb(IJ2+i)
                  SHELLS%LAW2(SU)%sigbakxy(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sigb(IJ3+i)
                endif
!               Temperature
                if (ELBUF_TAB(NG)%bufly(1)%l_temp > 0) then
                  SHELLS%LAW2(SU)%tempel(gpu_offset) =&
                  &ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%temp(i)
                endif
              end do
            end do
!           Internal energy from global buffer: GBUF%EINT(1:2*NEL)
            do i = 1, NEL
              SHELLS%LAW2(SU)%part_id(elem_counter(SU)+i) = IPARTC(NFT+1)
              SHELLS%LAW2(SU)%ngl(elem_counter(SU)+i) = IXC(NIXC,NFT+i)
              SHELLS%LAW2(SU)%eint(elem_counter(SU)+i) =&
              &ELBUF_TAB(NG)%gbuf%eint(i)
              SHELLS%LAW2(SU)%eint(&
              &SHELLS%LAW2(SU)%numelc+elem_counter(SU)+i) =&
              &ELBUF_TAB(NG)%gbuf%eint(NEL+i)
            end do
!           Build node mapping
            !do i=1,NEL
            !  do j=1,4
            !    if (node_mask(IXC(1+j,NFT+i)) .eqv. .false.) then
            !      nod_counter(SU) = nod_counter(SU) + 1
            !      SHELLS%LAW2(SU)%gpu_2_glob(nod_counter(SU)) = IXC(1+j,NFT+i)
            !      node_mask(IXC(1+j,NFT+i)) = .true.
            !      node_local_id(IXC(1+j,NFT+i)) = nod_counter(SU)
            !    end if
            !  end do
            !end do
!           Fill connectivity (before advancing elem_counter)
!           Subtract 1 for C/CUDA 0-based indexing
            do i=1,NEL
              SHELLS%LAW2(SU)%n1(elem_counter(SU)+i) = IXC(2,NFT+i) - 1
              SHELLS%LAW2(SU)%n2(elem_counter(SU)+i) = IXC(3,NFT+i) - 1
              SHELLS%LAW2(SU)%n3(elem_counter(SU)+i) = IXC(4,NFT+i) - 1
              SHELLS%LAW2(SU)%n4(elem_counter(SU)+i) = IXC(5,NFT+i) - 1
            enddo
!           Fill per-element material arrays (uniform within super-group)
            do i = 1, NEL
              SHELLS%LAW2(SU)%el_thk0(elem_counter(SU)+i) =&
              &ELBUF_TAB(NG)%gbuf%thk(i)
              SHELLS%LAW2(SU)%el_off(elem_counter(SU)+i) =&
              &ELBUF_TAB(NG)%gbuf%off(i)
              SHELLS%LAW2(SU)%el_ssp(elem_counter(SU)+i) =&
              &SHELLS%LAW2(SU)%ssp
              SHELLS%LAW2(SU)%el_rho(elem_counter(SU)+i) =&
              &SHELLS%LAW2(SU)%rho
              SHELLS%LAW2(SU)%el_ym(elem_counter(SU)+i) =&
              &SHELLS%LAW2(SU)%e_young
              SHELLS%LAW2(SU)%el_nu(elem_counter(SU)+i) =&
              &SHELLS%LAW2(SU)%nu
              SHELLS%LAW2(SU)%el_a11(elem_counter(SU)+i) =&
              &SHELLS%LAW2(SU)%a11
              SHELLS%LAW2(SU)%el_g_shear(elem_counter(SU)+i) =&
              &SHELLS%LAW2(SU)%g_shear
              SHELLS%LAW2(SU)%el_shf(elem_counter(SU)+i) =&
              &SHELLS%LAW2(SU)%shf_coef
            end do
!           Advance element counter for this super-group
            elem_counter(SU) = elem_counter(SU) + NEL
            SHELLS%LAW2(SU)%numnod = numnod
            ! gather X and V for nodes in this super-group
!           do i=1,nod_counter(SU)
!             glob_id = SHELLS%LAW2(SU)%gpu_2_glob(i)
!             SHELLS%LAW2(SU)%xx(i) = NODES%X(1,glob_id)
!             SHELLS%LAW2(SU)%xy(i) = NODES%X(2,glob_id)
!             SHELLS%LAW2(SU)%xz(i) = NODES%X(3,glob_id)
!             SHELLS%LAW2(SU)%vx(i) = NODES%V(1,glob_id)
!             SHELLS%LAW2(SU)%vy(i) = NODES%V(2,glob_id)
!             SHELLS%LAW2(SU)%vz(i) = NODES%V(3,glob_id)

!             if(NODES%iroddl == 1) then
!               SHELLS%LAW2(SU)%vrx(i) = NODES%VR(1,glob_id)
!               SHELLS%LAW2(SU)%vry(i) = NODES%VR(2,glob_id)
!               SHELLS%LAW2(SU)%vrz(i) = NODES%VR(3,glob_id)
!             endif
!           end do


          ENDDO

!
!         Upload all constant data to the GPU for each super-group
!
          do SU = 1, size(SHELLS%LAW2)
            if (SHELLS%LAW2(SU)%numelc <= 0) cycle
!
!            Set hourglass parameters
!
            call shell_gpu_set_hg_params(&
            &SHELLS%LAW2(SU)%handle,&
            &SHELLS%LAW2(SU)%h1,&
            &SHELLS%LAW2(SU)%h2,&
            &SHELLS%LAW2(SU)%h3,&
            &SHELLS%LAW2(SU)%srh1,&
            &SHELLS%LAW2(SU)%srh2,&
            &SHELLS%LAW2(SU)%srh3,&
            &HVISC_IN, HELAS_IN, HVLIN_IN)
!
!            Upload connectivity, thickness, element activity, per-element scalars
!
!           write(6,*) "Uploading constant data to GPU for super-group ", SU
!           write(6,*) "n1,n2,n3,n4: ", SHELLS%LAW2(SU)%n1(1), SHELLS%LAW2(SU)%n2(1), SHELLS%LAW2(SU)%n3(1), SHELLS%LAW2(SU)%n4(1)
            call shell_gpu_upload_constant(&
            &SHELLS%LAW2(SU)%handle,&
            &SHELLS%LAW2(SU)%n1,&
            &SHELLS%LAW2(SU)%n2,&
            &SHELLS%LAW2(SU)%n3,&
            &SHELLS%LAW2(SU)%n4,&
            &SHELLS%LAW2(SU)%el_thk0,&
            &SHELLS%LAW2(SU)%el_off,&
            &SHELLS%LAW2(SU)%el_ssp,&
            &SHELLS%LAW2(SU)%el_rho,&
            &SHELLS%LAW2(SU)%el_ym,&
            &SHELLS%LAW2(SU)%el_nu,&
            &SHELLS%LAW2(SU)%el_a11,&
            &SHELLS%LAW2(SU)%el_g_shear,&
            &SHELLS%LAW2(SU)%el_shf)
!
!            Upload initial integration-point state
!
            write(6,*) "Uploading IP state to GPU for super-group ", SU
            call shell_gpu_upload_ip_state(&
            &SHELLS%LAW2(SU)%handle,&
            &SHELLS%LAW2(SU)%sigxx,&
            &SHELLS%LAW2(SU)%sigyy,&
            &SHELLS%LAW2(SU)%sigxy,&
            &SHELLS%LAW2(SU)%sigyz,&
            &SHELLS%LAW2(SU)%sigzx,&
            &SHELLS%LAW2(SU)%pla,&
            &SHELLS%LAW2(SU)%epsd_ip,&
            &SHELLS%LAW2(SU)%sigbakxx,&
            &SHELLS%LAW2(SU)%sigbakyy,&
            &SHELLS%LAW2(SU)%sigbakxy,&
            &SHELLS%LAW2(SU)%tempel)
            write(6,*) "Completed GPU upload for super-group ", SU
          end do

          if(allocated(node_mask)) deallocate(node_mask)
          if(allocated(node_local_id)) deallocate(node_local_id)
          if(allocated(glob_2_gpu)) deallocate(glob_2_gpu)
          if(allocated(gpu_2_glob)) deallocate(gpu_2_glob)

!
!----6---------------------------------------------------------------7---------8
          RETURN
        END
      end module shell_internal_forces_mod



