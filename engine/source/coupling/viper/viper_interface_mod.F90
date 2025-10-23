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
! These are the interface routines between Viper & Radioss
! All reordering of nodes & minimisation of timestep occurs in these routines rather than Viper's counterparts
! notes: ELBUFDEF_MOD includes a call to include task_c.inc, which is required for several of the included subroutines
!||====================================================================
!||    viper_mod       ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    freform         ../engine/source/input/freform.F
!||    resol           ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod    ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||    spmd_mod        ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      MODULE VIPER_MOD
        use ELBUFDEF_MOD
        use precision_mod, only : WP
        USE SPMD_MOD
        implicit none
#include "spmd.inc"

        logical :: ViperCoupling     ! set in engine/source/input/freform.F with the engine flag /VIPER/ON

        type :: viper_coupling_
          integer :: numon             ! number of 'alive' elements (i.e. not eroded/deleted/null)
          integer :: ivout             ! trigger for specific testing
          integer :: id! file id for printing time
          integer, dimension(:), allocatable :: ITABM1,IXEM1         ! nodal & elemental arrays for coupling re-indexing
          integer :: NUMELE,NUMEL_SC,NUMEL_SCG, ioffset_3shell,ioffset_4shell    ! elemental counts
          real(kind=wp):: TSTOP
        end type viper_coupling_


        logical   :: iverbose
        parameter (iverbose = .false.)

      CONTAINS
!||====================================================================
!||    viper_coupling_initialize                ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                                    ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    radiossviper_inittab                     ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_receivesendinitialnumbers   ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_receivesendinitialtimes     ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_sendinitialstatus           ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_sendmass                    ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod                         ../common_source/modules/connectivity.F90
!||    nodal_arrays_mod                         ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine viper_coupling_initialize(VIPER, NODES, ELEMENT,   NUMNOD,&
          NIXS, NUMELS, IXS, NIXC, NUMELC,NIXTG, NUMELTG,IXTG, &
          ISTDO, NELEML, NUMELQ, NUMELT, NUMELP, NUMELR, &
          DTMIN, TSTOP, DTANIM, TT, NPARG, NGROUP, IPARG, ELBUF_TAB, &
          TT_DOUBLE, TANIM)
          use nodal_arrays_mod, only: nodal_arrays_
          use connectivity_mod, only: connectivity_

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(viper_coupling_), intent(inout) :: VIPER
          type(nodal_arrays_), intent(in) :: NODES
          TYPE(connectivity_), INTENT(in) :: ELEMENT
          integer, intent(in) :: NUMNOD
          integer, intent(in) :: NIXS, NUMELS, NIXC, NUMELC, NIXTG, NUMELTG !< dimensions of connectivity arrays
          integer, intent(in) :: IXS(NIXS,NUMELS), IXTG(NIXTG,NUMELTG)
          integer, intent(in) :: ISTDO, NELEML, NUMELQ, NUMELT, NUMELP, NUMELR
          real(kind=WP), intent(inout) :: DTMIN, TSTOP, DTANIM, TT
          integer, intent(in) :: NPARG, NGROUP
          integer, intent(in) :: IPARG(NPARG,NGROUP)
          type(ELBUF_STRUCT_), dimension(NGROUP), intent(in) :: ELBUF_TAB
          real(kind=WP), intent(inout) :: TANIM
          double precision, intent(inout) :: TT_DOUBLE

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          WRITE(ISTDO,*) "Number of elements in Radioss:"
          WRITE(ISTDO,*) "  NIXS     = ",NIXS      ! This is the number of characteristics per element in the IXS array
          WRITE(ISTDO,*) "  NELEML   = ",NELEML    ! This is the total number of elements
          WRITE(ISTDO,*) "  NUMELS   = ",NUMELS    ! This is the number of SOLIDS
          !WRITE(ISTDO,*) '  NUMELS8  = ',NUMELS8   ! This is the number of 8-node solids
          !WRITE(ISTDO,*) '  NUMELS10 = ',NUMELS10  ! This is the number of 10-node tetrahedra
          !WRITE(ISTDO,*) '  NUMELS16 = ',NUMELS16  ! This is the number of 16-node solids
          !WRITE(ISTDO,*) '  NUMELS20 = ',NUMELS20  ! This is the number of 20-node solids
          WRITE(ISTDO,*) "  NUMELQ   = ",NUMELQ    ! This is the number of quads
          WRITE(ISTDO,*) "  NUMELC   = ",NUMELC    ! This is the number of 4-shells
          WRITE(ISTDO,*) "  NUMELT   = ",NUMELT    ! This is the number of trias (aka 3-shells)
          WRITE(ISTDO,*) "  NUMELP   = ",NUMELP    ! This is the number of beams / rebar
          WRITE(ISTDO,*) "  NUMELR   = ",NUMELR    ! This is the number of springs
          WRITE(ISTDO,*) "  NUMELTG  = ",NUMELTG   ! This is the number of 3-shells

!           Allocate the arays for inverse index lookup
          VIPER%NUMEL_SC       = NUMELS+NUMELC                                        ! The total number of elements used by Viper
          VIPER%NUMEL_SCG      = VIPER%NUMEL_SC+NUMELTG                                     ! The total number of elements used by Viper
          VIPER%NUMELE= VIPER%NUMEL_SCG                                            ! The total number of elements used by Viper
          VIPER%ioffset_4shell = NUMELS+NUMELQ                                        ! The (assumed) index offset for 4-shells (confirmed that NUMELS8,NUMELS10,NUMELS16,NUMELS20 are not in the array)
          VIPER%ioffset_3shell = VIPER%ioffset_4shell+NUMELC+NUMELT+NUMELP+NUMELR           ! The (assumed) index offset for 3-shells
          WRITE(ISTDO,"(a,I18)") "Radioss2Viper: the total number of elements used by Viper: ",VIPER%NUMELE
          ALLOCATE(VIPER%ITABM1(NUMNOD))
          ALLOCATE(VIPER%IXEM1(VIPER%NUMELE))

!           Initialize node ordering & send nodal masses to Viper
!           Based upon the ordering of the element numbers in the 0000.out & from our experimentation,
!           we are assuming the order of elements in ELBUF_TAB is the same as in 0000.out, therefore we are adding offsets as required
          viper%numon = VIPER%NUMELE
          viper%ivout = 0
          CALL RadiossViper_InitTab(NUMNOD, NODES%ITAB, VIPER%ITABM1, 1, 0)              ! nodes

          CALL RadiossViper_InitTab(NUMELS, IXS,VIPER%IXEM1(1:NUMELS), NIXS, 0)              ! solids

          CALL RadiossViper_InitTab(NUMELC, ELEMENT%SHELL%IXC, &
            VIPER%IXEM1(1+NUMELS:VIPER%NUMEL_SC), NIXC, viper%ioffset_4shell) ! 4-shells

          CALL RadiossViper_InitTab(NUMELTG, IXTG, &
            VIPER%IXEM1(1+VIPER%NUMEL_SC:VIPER%NUMEL_SCG), NIXTG, viper%ioffset_3shell) ! 3-shells
          CALL RadiossViper_ReceiveSendInitialTimes(DTMIN,TSTOP,DTANIM,TT)
          CALL RadiossViper_ReceiveSendInitialNumbers(TSTOP,NUMNOD,NUMELS,NUMELC,NUMELTG)
          IF (TSTOP > 0.) THEN
            CALL RadiossViper_SendMass(NUMNOD,NODES%MS,VIPER%ITABM1)                                                 ! required only for energy calculation checks
            CALL RadiossViper_SendInitialStatus(viper%numon,NELEML,VIPER%NUMELE,&
              NPARG,NGROUP,VIPER%IXEM1,IPARG,ELBUF_TAB)  ! required to tell Viper of void elements
            TT_DOUBLE = TT
            TANIM     = TT
            IF (TT > 0.) THEN
!                Viper is starting from a remap; need to set animation times correctly
              WRITE(ISTDO,"(a,F12.6,a)") &
                "Radioss2Viper: the simulation is starting at ", &
                TT, &
                " due to Viper-remapping"
              DO WHILE (TANIM < TT)
                TANIM = TANIM + DTANIM
              END DO
              TANIM = TANIM - DTANIM  ! this will enforce printing the initial dump
            END IF
          END IF
          VIPER%TSTOP= TSTOP-1.d-8    ! TSTOP can change to facilitate an early exit; use this to send a kill-command to Viper
          VIPER%id = 1701
          OPEN(unit=VIPER%id,file="TimeLog.txt")
        end subroutine viper_coupling_initialize


! ----------------------------------------------------------------------------------------------------------------------
! The itab array is the user nodal indices.  The idabm1 array is supposed to be the inverse array, but it does not work as required for Viper Coupling
! Creating a local of itabm1 that will work as required for Viper coupling
! Note that (e.g) itab is a 1D array, while IXS is an 11xN array that is re-arranged to be a 1D array with node ID in every ncol'th position
!||====================================================================
!||    radiossviper_inittab        ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    viper_coupling_initialize   ../engine/source/coupling/viper/viper_interface_mod.F90
!||====================================================================
        subroutine RadiossViper_InitTab(numnod,itab,itabm1,ncol,ioffset)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)  :: numnod,itab(numnod),ncol,ioffset
          integer, intent(out) :: itabm1(numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer, allocatable :: itabtmp(:),itab1D(:)
          integer :: i,j,idmax,idmin
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if (numnod == 0) return
          if (iverbose) print*, "Radioss2Viper: InitTab: entering with ncol = ",ncol," and N = ",numnod

          ! copy data to a 1D array to (hopefully) minimise cache misses
          allocate(itab1D(numnod))
          do i = 1,numnod
            itab1D(i) = itab(i*ncol)
          end do

          ! find the maximum index & add create a working array of that size
          idmax = 0
          idmin = 1e8
          do i = 1,numnod
            idmax = max(idmax,itab1D(i))
            idmin = min(idmin,itab1D(i))
          end do
          if (iverbose) print*, "Radioss2Viper: InitTab: IDs in the range ",idmin,idmax
          allocate(itabtmp(idmax))
          itabtmp = -1 ! initialise to illegal index

          ! place the shuffled index in the array entry corresponding to the user id
          do j = 1,numnod
            itabtmp(itab1D(j)) = j
          end do

          ! Reshuffle the array into itabm1 removing all the illegal -1's
          j = 1
          do i = 1,numnod
            do while (itabtmp(j)==-1)
              j = j + 1                     ! advance to skip over -1
            end do
            itabm1(i) = itabtmp(j) + ioffset ! fill in the correct entry using the correct offset value
            j = j + 1                        ! advance to next entry
          end do
          deallocate(itabtmp)
          deallocate(itab1D)

          print*, "Radioss2Viper: InitTab: exiting with ncol = ",ncol," and N = ",numnod

        end subroutine RadiossViper_InitTab
! ----------------------------------------------------------------------------------------------------------------------
! This will receive the initial time, final time and output frequency from Viper (note that Viper does not always start at time = 0)
! This will also receive the minimum timestep, select the largest value, and send the new value back to Viper
!||====================================================================
!||    radiossviper_receivesendinitialtimes   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    viper_coupling_initialize              ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_ReceiveSendInitialTimes(dt_min,t_max,dt_out,t_now)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), intent(inout) :: dt_min
          real(kind=WP), intent(out)   :: t_max,dt_out,t_now
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP)                :: dt_min_viper
          integer                :: ierror
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          t_now  = 0.
          t_max  = 0.
          dt_out = 0.
          if (iverbose) then
            print*, "Radioss2Viper: ReceiveSendInitialTimes: entering: t_now,t_max,dt_min,dt_out: ", &
              t_now, t_max, dt_min, dt_out
          end if
          call SPMD_RECV(dt_min_viper, 1, 1, 9931, MPI_COMM_WORLD)
          call SPMD_RECV(t_max,        1, 1, 9932, MPI_COMM_WORLD)
          call SPMD_RECV(dt_out,       1, 1, 9933, MPI_COMM_WORLD)
          call SPMD_RECV(t_now,        1, 1, 9934, MPI_COMM_WORLD)
          dt_min = max(dt_min,dt_min_viper)
          call SPMD_SEND(dt_min,       1,  1, 9935, MPI_COMM_WORLD)
          if (iverbose) then
            print*, "Radioss2Viper: ReceiveSendInitialTimes: exiting: t_now,t_max,dt_min,dt_out: ", &
              t_now, t_max, dt_min, dt_out
          end if

        end subroutine RadiossViper_ReceiveSendInitialTimes
! ----------------------------------------------------------------------------------------------------------------------
! This will send the number of nodes and elements to Viper; viper will compare the values and sent back a kill-command if
! there is a mismatch in numbers
!||====================================================================
!||    radiossviper_receivesendinitialnumbers   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    viper_coupling_initialize                ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_ReceiveSendInitialNumbers(t_max,numnodes,numsolids,num4shells,num3shells)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), intent(inout) :: t_max
          integer, intent(in)    :: numnodes,numsolids,num4shells,num3shells
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer                :: ikill,ierror
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if (iverbose) print*, "Radioss2Viper: Entering ReceiveSendInitialNumbers: ", t_max
          call SPMD_SEND(numnodes,   1,1, 9940)
          call SPMD_SEND(numsolids,  1,1, 9941)
          call SPMD_SEND(num4shells, 1,1, 9942)
          call SPMD_SEND(num3shells, 1,1, 9943)
          call SPMD_RECV(ikill,      1,1, 9944)
          if (ikill == 1) then
            print*, "Radioss2Viper: ReceiveSendInitialNumbers: ABORTING due to number mismatch"
            t_max = 0.
          end if
          if (iverbose) print*, "Radioss2Viper: Exiting ReceiveSendInitialNumbers: ", t_max

        end subroutine RadiossViper_ReceiveSendInitialNumbers
! ----------------------------------------------------------------------------------------------------------------------
! This will send nodal masses from to Viper for calculation of total energy; this needs to be done once
!||====================================================================
!||    radiossviper_sendmass       ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    viper_coupling_initialize   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_SendMass(numnod,MS,itabm1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: numnod,itabm1(numnod)
          real(kind=WP), intent(in) :: MS(numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer             :: i,ierror
          real(kind=WP), dimension(:), allocatable :: MSviper !(numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(MSviper(numnod))
          print*, "Radioss2Viper: SendMass: entering with numnod = ",numnod
!     make new arrays where the elements are in the correct order
          do i = 1,numnod     ! tests show tha this is slower if openmp-parallel
            MSviper(i) = MS(itabm1(i))
          end do
          call SPMD_SEND(MSviper,numnod,  1, 9930)
          if (iverbose) print*, "Radioss2Viper: SendMass: exiting"
          deallocate(MSviper)
        end subroutine RadiossViper_SendMass
! ----------------------------------------------------------------------------------------------------------------------
! This will send the initial erosion status to Viper; this is required to inform Viper of void elements that need to be excluded calculations
!||====================================================================
!||    radiossviper_sendinitialstatus   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    viper_coupling_initialize        ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_SendInitialStatus(n,numele,numele_viper,nparg,ngroup,ixem1,iparg,elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)    :: numele,numele_viper,nparg,ngroup
          integer, intent(in)    :: ixem1(numele_viper),iparg(nparg,ngroup)
          integer, intent(out)   :: n
          type(ELBUF_STRUCT_),dimension(ngroup), intent(in):: elbuf_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer                :: i,j,k,ierror
          integer                :: Eviper(numele_viper),Evipertmp(numele)
          logical                :: viper_element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          print*, "Radioss2Viper: SendInitialStatus: entering:", numele,numele_viper
!     make new erosion arrary in Viper's order & determine the number of eroded elements
!     first, we put them in a contigious array; we will sort and send only if the number of active elements has changed
          n = 0
          k = 0
          do i = 1,ngroup
            do j = 1,iparg(2,i)
              if (iparg(5,i)==1 .or. iparg(5,i)==3 .or. iparg(5,i)==7) then
                viper_element = .true.
              else
                viper_element = .false.
              end if
              k = k + 1
              if (k <= numele) then
                if (ELBUF_TAB(i)%GBUF%OFF(j) == 1 .and. ELBUF_TAB(i)%BUFLY(1)%ILAW > 0 .and. viper_element) then
                  n = n + 1
                  Evipertmp(k) = ELBUF_TAB(i)%GBUF%OFF(j)   ! pass element status (eroded or not)
                else
                  Evipertmp(k) = -1                         ! pass element status defined as void
                end if
              end if
            end do
          end do
          if (iverbose) print*, "Radioss2Viper: SendInitialStatus: filled primary array"
          do i = 1,numele_viper
            Eviper(i) = Evipertmp(ixem1(i))
          end do
          if (iverbose) print*, "Radioss2Viper: _SendInitialStatus: filled secondary array"
          call SPMD_SEND(Eviper, numele_viper, 1, 9950, MPI_COMM_WORLD)
          print*, "Radioss2Viper: SendInitialStatus: exiting", numele,numele_viper,n

        end subroutine RadiossViper_SendInitialStatus
! ----------------------------------------------------------------------------------------------------------------------
! This will send nodal positions to Viper
! This will also send element status (i.e., eroded or still active)
!||====================================================================
!||    radiossviper_sendxve   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                  ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_SendXVE( &
          numnod, numele, numele_viper, nparg, ngroup, &
          numonIO, ivoutIO, X, V, itabm1, ixem1, iparg, elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)    :: numnod,numele,numele_viper,nparg,ngroup
          integer, intent(in)    :: itabm1(numnod),ixem1(numele_viper),iparg(nparg,ngroup)
          integer, intent(inout) :: numonIO,ivoutIO
          real(kind=WP), intent(in)    :: X(3,numnod),V(3,numnod)
          type(ELBUF_STRUCT_),dimension(ngroup), intent(in) :: elbuf_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer                :: i,j,k,n,ierror
          integer                :: Eviper(numele_viper),Evipertmp(numele)
          real(kind=WP), dimension(:,:), allocatable :: Xviper,Vviper
          logical                :: viper_element,kill_element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(Xviper(3,numnod),Vviper(3,numnod))

          if (iverbose) print*, "Radioss2Viper: Entering SendXVE "
!     make temporary position & velocity arrays where the elements are in the correct order for Viper
          do i = 1,numnod     ! tests show that this is slower if openmp-parallel
            Xviper(1:3,i) = X(1:3,itabm1(i))
            Vviper(1:3,i) = V(1:3,itabm1(i))
          end do
          call SPMD_SEND(Xviper,  3*numnod,  1, 9902, MPI_COMM_WORLD)
          call SPMD_SEND(Vviper,  3*numnod,  1, 9903, MPI_COMM_WORLD)

          if (iverbose) print*, "Radioss2Viper: SendXVE: Sent position & velocity"
!     make new erosion arrary in Viper's order & determine the number of eroded elements
!     first, we put them in a contigious array; we will sort and send only if the number of active elements has changed
          n = 0
          k = 0
          do i = 1,ngroup
            do j = 1,iparg(2,i)
              if (iparg(5,i)==1 .or. iparg(5,i)==3 .or. iparg(5,i)==7) then
                viper_element = .true.
              else
                viper_element = .false.
              end if
              k = k + 1
!           Coupling tests where elements are manually eroded
!            if (ivoutIO==200 .and. k < 125 .and. .false.) then  ! Testing Chinook plate that is 2 FE thick
!            if (ivoutIO==200 .and. 0 < k .and. k < 101 .and. .false.) then  ! Testing Chinook plate that is 3 FE thick; removes central sheet
              if (ivoutIO==200 .and. .false.) then  ! Testing Chinook plate that is 3 FE thick; removes selected central elements
                kill_element = .false.
                if ( 1 <= k .and. k <=   6 ) kill_element = .true.
                if (15 <= k .and. k <=  16 ) kill_element = .true.
                if (30 <= k .and. k <=  35 ) kill_element = .true.
                if (47 <= k .and. k <=  48 ) kill_element = .true.
                if (61 <= k .and. k <=  66 ) kill_element = .true.
                if (92 <= k .and. k <= 100 ) kill_element = .true.
                if (k==24 .or. k==45 .or. k==69 .or. k==72 .or. k==74)  kill_element = .true.
                if (kill_element) then
                  viper_element = .false.
                  ELBUF_TAB(i)%GBUF%OFF(j) = 0
                end if
              end if
              if (k <= numele) then
                if (ELBUF_TAB(i)%GBUF%OFF(j) == 1 .and. ELBUF_TAB(i)%BUFLY(1)%ILAW > 0 .and. viper_element) then
                  n = n + 1
                  Evipertmp(k) = ELBUF_TAB(i)%GBUF%OFF(j)   ! pass active status
                else
                  Evipertmp(k) = -1                         ! failed, dead, null, eroded
                end if
              end if
            end do
          end do
          call SPMD_SEND(n, 1,  1, 9907, MPI_COMM_WORLD)
          print*, "Radioss2Viper: SendXVE: numnod, nElements_prev, nElements = : ",numnod,numonIO,n
          if (numonIO /= n) then
            do i = 1,numele_viper
              Eviper(i) = Evipertmp(ixem1(i))
            end do
            call SPMD_SEND(Eviper, numele_viper,  1, 9908, MPI_COMM_WORLD)
          end if
          numonIO = n
          ivoutIO = ivoutIO + 1
          deallocate(Xviper,Vviper)
        end subroutine RadiossViper_SendXVE
! ----------------------------------------------------------------------------------------------------------------------
! This will receive the FORCES on the nodes from Viper
! Despite the variable name being A, this is actually a force
! We add this to both A and to FEXT, where the latter is used only for output
!||====================================================================
!||    radiossviper_receiveaccelerations   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                               ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_ReceiveAccelerations(numnod,A,Fext,itabm1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)    :: numnod,itabm1(numnod)
          real(kind=WP), intent(inout) :: A(3,numnod),Fext(3,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), dimension(:,:), allocatable :: Aviper!(3,numnod)
          integer                :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(Aviper(3,numnod))
          if (iverbose) print*, "Radioss2Viper: ReceiveAccelerations: ", numnod
          call SPMD_RECV(Aviper, 3*numnod, 1, 9910, MPI_COMM_WORLD)
          do i = 1,numnod
            A(   1:3,itabm1(i)) = A(   1:3,itabm1(i)) + Aviper(1:3,i)
            Fext(1:3,itabm1(i)) = Fext(1:3,itabm1(i)) + Aviper(1:3,i)
          end do
          deallocate(Aviper)
        end subroutine RadiossViper_ReceiveAccelerations
! ----------------------------------------------------------------------------------------------------------------------
! This will pass Viper's timestep to OpenRadioss, compare the two, select the shortest;
! The shortest timestep will also be passed back to Viper
!||====================================================================
!||    radiossviper_receivesenddt   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                        ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_ReceiveSendDT(id_ViperCouplingIO,time,dt_rad)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)    :: id_ViperCouplingIO
          real(kind=WP), intent(in)    :: time
          real(kind=WP), intent(inout) :: dt_rad
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP)          :: dt_viper, dt_rad_in
          integer                :: ierror
          character(len=16)      :: dt_selected
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          dt_rad_in = dt_rad
          call SPMD_RECV(dt_viper, 1,  1, 9925, MPI_COMM_WORLD )
          dt_rad = min(dt_rad,dt_viper)
          call SPMD_SEND(dt_rad,   1,  1, 9926, MPI_COMM_WORLD)
          print*, "Radioss2Viper: ReceiveSendDT: exiting: dt_rad_in, dt_viper_in, dt_out: ", &
            dt_rad_in, dt_viper, dt_rad
          if (dt_rad_in < dt_viper) then
            dt_selected = "dt_Radioss"
          else
            dt_selected = "dt_Viper"
          end if
          write(id_ViperCouplingIO,"(3(a,Es13.6),3a,Es13.6)")&
          &"Time_Radioss=",time, &
            "   dt_Radioss=",dt_rad_in, &
            "   dt_Viper=",dt_viper, &
            "   dt_selected=",trim(dt_selected),"=",dt_rad

        end subroutine RadiossViper_ReceiveSendDT
! ----------------------------------------------------------------------------------------------------------------------
! This will send a kill-command to Viper if Radioss needs to end prematurely!
! There are commands that will modify tstop in Radioss to permit premature termination;
! We will compare the initial tstop with the current tstop to see if this change has been triggered
!||====================================================================
!||    radiossviper_sendkill   ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                   ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine RadiossViper_SendKill(mstop,tstop, tstop_viper)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), intent(in) :: tstop,tstop_viper
          integer, intent(in)             :: mstop
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer             :: ikill, ierror
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if (tstop < tstop_viper .or. mstop > 0) then
            print*, "Radioss2Viper: SendKill: ABORTING!  Sending kill-command to Viper!"
            ikill = 1
          else
            ikill = 0
          end if
          call SPMD_SEND(ikill,   1,  1, 9960, MPI_COMM_WORLD)

        end subroutine RadiossViper_SendKill
! ----------------------------------------------------------------------------------------------------------------------
      END MODULE VIPER_MOD
