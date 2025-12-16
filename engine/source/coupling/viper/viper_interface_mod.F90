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

        logical :: ViperCoupling                              ! set in engine/source/input/freform.F with the engine flag /VIPER/ON

        type :: viper_coupling_
          integer :: numon                                    ! number of 'alive' elements (i.e. not eroded/deleted/null)
          integer :: io_dt                                    ! file id for printing time
          integer, dimension(:), allocatable :: ITABM1,IXEM1  ! nodal & elemental arrays for coupling re-indexing
          integer :: NUMELEv                                  ! total number of elements Viper will use
          integer :: NUMELEr                                  ! total number of elements Radioss has;
          ! total has since been removed from main code, so storing here
          real(kind=wp):: TSTOP
          real(kind=wp):: DT_MIN
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
          ISTDO, NUMELQ, NUMELT, NUMELP, NUMELR, &
          TSTOP, DTANIM, TT, NPARG, NGROUP, IPARG, ELBUF_TAB, &
          TT_DOUBLE, TANIM, DTMIN)
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
          integer, intent(in) :: ISTDO, NUMELQ, NUMELT, NUMELP, NUMELR
          real(kind=WP), intent(inout) :: TSTOP, DTANIM, TT
          integer, intent(in) :: NPARG, NGROUP
          integer, intent(in) :: IPARG(NPARG,NGROUP)
          type(ELBUF_STRUCT_), dimension(NGROUP), intent(in) :: ELBUF_TAB
          real(kind=WP), intent(inout) :: TANIM, DTMIN
          double precision, intent(inout) :: TT_DOUBLE

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: iNUMELEr_TOTAL, iNUMELEv_TOTAL, iNUMEL_SC, iNUMEL_SCG, ioffset_3shell,ioffset_4shell    ! elemental counts
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Calculate & internally store the number of elements used by Radioss; before 9 Dec 2025, this value was
          ! calculated outside of this subroutine for use throughout the code, but is now no longer used
          iNUMELEr_TOTAL = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG
          VIPER%NUMELEr  = iNUMELEr_TOTAL
          WRITE(ISTDO,*) "Number of elements in Radioss:"
          WRITE(ISTDO,*) "  NIXS     = ",NIXS             ! This is the number of characteristics per element in the IXS array
          WRITE(ISTDO,*) "  NELEML   = ",iNUMELEr_TOTAL   ! This is the total number of elements
          WRITE(ISTDO,*) "  NUMELS   = ",NUMELS           ! This is the number of SOLIDS
          !WRITE(ISTDO,*)"  NUMELS8  = ",NUMELS8          ! This is the number of 8-node solids
          !WRITE(ISTDO,*)"  NUMELS10 = ",NUMELS10         ! This is the number of 10-node tetrahedra
          !WRITE(ISTDO,*)"  NUMELS16 = ",NUMELS16         ! This is the number of 16-node solids
          !WRITE(ISTDO,*)"  NUMELS20 = ",NUMELS20         ! This is the number of 20-node solids
          WRITE(ISTDO,*) "  NUMELQ   = ",NUMELQ           ! This is the number of quads
          WRITE(ISTDO,*) "  NUMELC   = ",NUMELC           ! This is the number of 4-shells
          WRITE(ISTDO,*) "  NUMELT   = ",NUMELT           ! This is the number of trias (aka 3-shells)
          WRITE(ISTDO,*) "  NUMELP   = ",NUMELP           ! This is the number of beams / rebar
          WRITE(ISTDO,*) "  NUMELR   = ",NUMELR           ! This is the number of springs
          WRITE(ISTDO,*) "  NUMELTG  = ",NUMELTG          ! This is the number of 3-shells

!         Allocate the arays for inverse index lookup
          iNUMEL_SC      = NUMELS+NUMELC                              ! The total number of solids + 4-shells
          iNUMEL_SCG     = iNUMEL_SC+NUMELTG                          ! The total number of solids + 4-shells + 3-shells
          iNUMELEv_TOTAL = iNUMEL_SCG                                 ! The total number of elements used by Viper
          ioffset_4shell = NUMELS+NUMELQ                              ! The (assumed) index offset for 4-shells
          ! (confirmed that NUMELS8,NUMELS10,NUMELS16,NUMELS20 are not in the array)
          ioffset_3shell = ioffset_4shell+NUMELC+NUMELT+NUMELP+NUMELR ! The (assumed) index offset for 3-shells
          WRITE(ISTDO,"(a,I18)") "Radioss2Viper: the total number of elements used by Viper: ",iNUMELEv_TOTAL
          ALLOCATE(VIPER%ITABM1(NUMNOD))
          ALLOCATE(VIPER%IXEM1(iNUMELEv_TOTAL))

!         Initialize node ordering & send nodal masses to Viper
!         Based upon the ordering of the element numbers in the 0000.out & from our experimentation,
!         we are assuming the order of elements in ELBUF_TAB is the same as in 0000.out, therefore we are adding offsets as required
          VIPER%NUMELEv = iNUMELEv_TOTAL
          VIPER%numon   = iNUMELEv_TOTAL
          CALL RadiossViper_InitTab(NUMNOD,  NODES%ITAB,        VIPER%ITABM1,                        1,     0)              ! nodes
          CALL RadiossViper_InitTab(NUMELS,  IXS,               VIPER%IXEM1(1          :NUMELS),     NIXS,  0)              ! solids
          CALL RadiossViper_InitTab(NUMELC,  ELEMENT%SHELL%IXC, VIPER%IXEM1(1+NUMELS   :iNUMEL_SC),  NIXC,  ioffset_4shell) ! 4-shells
          CALL RadiossViper_InitTab(NUMELTG, IXTG,              VIPER%IXEM1(1+iNUMEL_SC:iNUMEL_SCG), NIXTG, ioffset_3shell) ! 3-shells
          CALL RadiossViper_ReceiveSendInitialTimes(DTMIN,VIPER%DT_MIN,TSTOP,DTANIM,TT)
          CALL RadiossViper_ReceiveSendInitialNumbers(TSTOP,NUMNOD,NUMELS,NUMELC,NUMELTG)
          IF (TSTOP > 0.) THEN
            CALL RadiossViper_SendMass(NUMNOD,NODES%MS,VIPER%ITABM1)
            CALL RadiossViper_SendInitialStatus(VIPER%numon,iNUMELEr_TOTAL,iNUMELEv_TOTAL,&
              NPARG,NGROUP,VIPER%IXEM1,IPARG,ELBUF_TAB)  ! required to tell Viper of void elements
            TT_DOUBLE = TT
            TANIM     = 0.
            IF (TT > 0.) THEN
!             Viper is starting from a remap; need to set animation times correctly
              WRITE(ISTDO,"(a,F12.9,a)") &
                "Radioss2Viper: the simulation is starting at ", &
                TT, " due to Viper-remapping"
              DO WHILE (TANIM < TT)
                TANIM = TANIM + DTANIM
              END DO
              TANIM = TANIM - DTANIM  ! this will enforce printing the initial dump
            END IF
          END IF
          VIPER%TSTOP = TSTOP-1.d-8    ! TSTOP can change to facilitate an early exit; use this to send a kill-command to Viper
          VIPER%io_dt = 1701
          OPEN(unit=VIPER%io_dt,file="TimeLog.txt")
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

          ! allocate array & initialise to illegal index
          allocate(itabtmp(idmax))
          do i = 1,idmax
            itabtmp(i) = -1
          end do

          ! place the shuffled index in the array entry corresponding to the user id
          do j = 1,numnod
            itabtmp(itab1D(j)) = j
          end do

          ! Reshuffle the array into itabm1 removing all the illegal -1's
          j = 1
          do i = 1,numnod
            do while (itabtmp(j)==-1)
              j = j + 1                      ! advance to skip over -1
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
        subroutine RadiossViper_ReceiveSendInitialTimes(dt_min,dt_min_viper,t_max,dt_out,t_now)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), intent(inout) :: dt_min
          real(kind=WP), intent(out)   :: t_max,dt_out,t_now,dt_min_viper
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if (iverbose) print*, "Radioss2Viper: ReceiveSendInitialTimes: entering"
          call SPMD_RECV(dt_min_viper, 1, 1, 9931, MPI_COMM_WORLD)
          call SPMD_RECV(t_max,        1, 1, 9932, MPI_COMM_WORLD)
          call SPMD_RECV(dt_out,       1, 1, 9933, MPI_COMM_WORLD)
          call SPMD_RECV(t_now,        1, 1, 9934, MPI_COMM_WORLD)
          dt_min = max(dt_min,dt_min_viper)                            ! note: prior to this line, dt_min == dt_min_radioss
          dt_min_viper = dt_min                                        ! synchronise both dt_min's
          call SPMD_SEND(dt_min,       1, 1, 9935, MPI_COMM_WORLD)
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
          integer                :: ikill
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if (iverbose) print*, "Radioss2Viper: Entering ReceiveSendInitialNumbers: ", t_max
          call SPMD_SEND(numnodes,   1,1, 9940, MPI_COMM_WORLD)
          call SPMD_SEND(numsolids,  1,1, 9941, MPI_COMM_WORLD)
          call SPMD_SEND(num4shells, 1,1, 9942, MPI_COMM_WORLD)
          call SPMD_SEND(num3shells, 1,1, 9943, MPI_COMM_WORLD)
          call SPMD_RECV(ikill,      1,1, 9944, MPI_COMM_WORLD)
          if (ikill == 1) then
            print*, "Radioss2Viper: ReceiveSendInitialNumbers: ABORTING due to number mismatch"
            t_max = 0.
          end if
          if (iverbose) print*, "Radioss2Viper: Exiting ReceiveSendInitialNumbers: ", t_max

        end subroutine RadiossViper_ReceiveSendInitialNumbers
! ----------------------------------------------------------------------------------------------------------------------
! This will send nodal masses to Viper
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
          integer             :: i
          real(kind=WP), dimension(:), allocatable :: MSviper !(numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(MSviper(numnod))
          if (iverbose) print*, "Radioss2Viper: SendMass: entering with numnod = ",numnod
!         make new arrays where the elements are in the correct order
          do i = 1,numnod
            MSviper(i) = MS(itabm1(i))
          end do
          call SPMD_SEND(MSviper,numnod,  1, 9930, MPI_COMM_WORLD)
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
        subroutine RadiossViper_SendInitialStatus(n,numele_radioss,numele_viper,nparg,ngroup,ixem1,iparg,elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)    :: numele_radioss,numele_viper,nparg,ngroup
          integer, intent(in)    :: ixem1(numele_viper),iparg(nparg,ngroup)
          integer, intent(out)   :: n
          type(ELBUF_STRUCT_),dimension(ngroup), intent(in):: elbuf_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer                :: i,j,k
          integer                :: Eviper(numele_viper),Evipertmp(numele_radioss)
          logical                :: viper_element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          print*, "Radioss2Viper: SendInitialStatus: entering:", numele_radioss,numele_viper
!         make new erosion arrary in Viper's order & determine the number of eroded elements
!         first, we put them in a contigious array; we will sort and send only if the number of active elements has changed
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
              if (k <= numele_radioss) then
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
          print*, "Radioss2Viper: SendInitialStatus: exiting", numele_radioss,numele_viper,n

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
          numnod, numele_radioss, numele_viper, nparg, ngroup, &
          numonIO, X, V, itabm1, ixem1, iparg, elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)    :: numnod,numele_radioss,numele_viper,nparg,ngroup
          integer, intent(in)    :: itabm1(numnod),ixem1(numele_viper),iparg(nparg,ngroup)
          integer, intent(inout) :: numonIO
          real(kind=WP), intent(in)    :: X(3,numnod),V(3,numnod)
          type(ELBUF_STRUCT_),dimension(ngroup), intent(in) :: elbuf_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer                :: i,j,k,n
          integer                :: Eviper(numele_viper),Evipertmp(numele_radioss)
          real(kind=WP), dimension(:), allocatable :: Xviper,Vviper
          logical                :: viper_element,kill_element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(Xviper(3*numnod))
          allocate(Vviper(3*numnod))

          if (iverbose) print*, "Radioss2Viper: Entering SendXVE "
!         make temporary position & velocity arrays where the elements are in the correct order for Viper
          do i = 1,numnod
            Xviper(3*i-2) = X(1,itabm1(i))
            Xviper(3*i-1) = X(2,itabm1(i))
            Xviper(3*i  ) = X(3,itabm1(i))

            Vviper(3*i-2) = V(1,itabm1(i))
            Vviper(3*i-1) = V(2,itabm1(i))
            Vviper(3*i  ) = V(3,itabm1(i))
          end do
          call SPMD_SEND(Xviper,  3*numnod,  1, 9902, MPI_COMM_WORLD)
          call SPMD_SEND(Vviper,  3*numnod,  1, 9903, MPI_COMM_WORLD)

          if (iverbose) print*, "Radioss2Viper: SendXVE: Sent position & velocity"
!         make new erosion arrary in Viper's order & determine the number of eroded elements
!         first, we put them in a contigious array; we will sort and send only if the number of active elements has changed
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
              if (k <= numele_radioss) then
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
          deallocate(Xviper)
          deallocate(Vviper)
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
          real(kind=WP), dimension(:), allocatable :: Aviper!(3*numnod)
          integer                :: i,j
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(Aviper(3*numnod))
          if (iverbose) print*, "Radioss2Viper: ReceiveAccelerations: ", numnod
          call SPMD_RECV(Aviper, 3*numnod, 1, 9910, MPI_COMM_WORLD)
          do i = 1,numnod
            A(1,itabm1(i)) = A(1,itabm1(i)) + Aviper(3*i-2)
            A(2,itabm1(i)) = A(2,itabm1(i)) + Aviper(3*i-1)
            A(3,itabm1(i)) = A(3,itabm1(i)) + Aviper(3*i  )

            Fext(1,itabm1(i)) = Fext(1,itabm1(i)) + Aviper(3*i-2)
            Fext(2,itabm1(i)) = Fext(2,itabm1(i)) + Aviper(3*i-1)
            Fext(3,itabm1(i)) = Fext(3,itabm1(i)) + Aviper(3*i  )
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
        subroutine RadiossViper_ReceiveSendDT(id_iodt,time,dt_min,dt_rad,mstop,mrest)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)          :: id_iodt
          real(kind=WP), intent(in)    :: time,dt_min
          real(kind=WP), intent(inout) :: dt_rad
          integer, intent(inout)       :: mstop,mrest
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP)          :: dt_viper, dt_rad_in
          character(len=16)      :: dt_selected
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          dt_rad_in = dt_rad
          call SPMD_RECV(dt_viper, 1,  1, 9925, MPI_COMM_WORLD)
          dt_rad = min(dt_rad,dt_viper)
          if (dt_rad < dt_min) then
            mstop = 1
            mrest = 1
            print*, "Radioss2Viper: ReceiveSendDT: WARNING! dt_rad < dt_min: ", dt_rad, dt_min
          endif
          call SPMD_SEND(dt_rad,   1,  1, 9926, MPI_COMM_WORLD)

          if (dt_rad_in < dt_viper) then
            dt_selected = "dt_Radioss"
          else
            dt_selected = "dt_Viper"
          end if
          write(id_iodt,"(3(a,Es13.6),3a,Es13.6)")&
          &"Time_Radioss=",time, &
            "   dt_Radioss=",dt_rad_in, &
            "   dt_Viper=",dt_viper, &
            "   dt_selected=",trim(dt_selected),"=",dt_rad

          print*, "Radioss2Viper: ReceiveSendDT: exiting: dt_rad_in, dt_viper_in, dt_out: ", &
            dt_rad_in, dt_viper, dt_rad

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
          integer,       intent(in) :: mstop
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer             :: ikill
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          ikill = 0
          if (tstop < tstop_viper) then
            print*, "Radioss2Viper: SendKill: ABORTING!  Sending kill-command to Viper! (tstop < tstop_viper): ", tstop, tstop_viper
            ikill = 1
          endif
          if (mstop > 0) then
            print*, "Radioss2Viper: SendKill: ABORTING!  Sending kill-command to Viper! (mstop > 0): ", mstop
            ikill = 1
          endif
          call SPMD_SEND(ikill,   1,  1, 9960, MPI_COMM_WORLD)

        end subroutine RadiossViper_SendKill
! ----------------------------------------------------------------------------------------------------------------------
      END MODULE VIPER_MOD
