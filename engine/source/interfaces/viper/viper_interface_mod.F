C This are the interface routines between Viper & Radioss
C All reodrering of nodes & minimisation of timestep occurs in these routines rather than Viper's counterparts
C notes: ELBUFDEF_MOD includes a call to include task_c.inc, which is required for several of the included subroutines
      MODULE VIPER_MOD
      use ELBUFDEF_MOD
      implicit none
#ifdef MPI
#include  "mpif.h"
#endif
#include  "my_real.inc"

      integer numon                         ! number of 'alive' elements (i.e. not eroded/deleted/null)
      integer ivout                         ! trigger for specific testing
      integer id_ViperCoupling              ! file id for printing time

c     Toggle to use Viper Coupling
      logical ViperCoupling                 ! set in engine/source/input/freform.F with the engine flag /VIPER/ON

c     parameter for additional (developer) print statements
      logical   iverbose
      parameter (iverbose = .false.)

      CONTAINS

C--------------------------------------------------------
C The itab array is the user nodal indices.  The idabm1 array is supposed to be the inverse array, but it does not work as required for Viper Coupling
C Creating a local of itabm1 that will work as required for Viper coupling
C Note that (e.g) itab is a 1D array, while IXS is an 11xN array that is re-arranged to be a 1D array with node ID in every ncol'th position
      subroutine RadiossViper_InitTab(numnod,itab,itabm1,ncol,ioffset)
      integer, intent(in)  :: numnod,itab(*),ncol,ioffset
      integer, intent(out) :: itabm1(numnod)
      integer, allocatable :: itabtmp(:),itab1D(:)
      integer :: i,j,idmax,idmin

      if (numnod == 0) return
      if (iverbose) print*, 'Radioss2Viper: InitTab: entering with ncol = ',ncol,' and N = ',numnod

      ! copy data to a 1D array to (hopefully) minimise cache misses
      allocate(itab1D(numnod))
      do i = 1,numnod
         itab1D(i) = itab(i*ncol)
      enddo

      ! find the maximum index & add create a working array of that size
      idmax = 0
      idmin = 1e8
      do i = 1,numnod
         idmax = max(idmax,itab1D(i))
         idmin = min(idmin,itab1D(i))
      enddo
      if (iverbose) print*, 'Radioss2Viper: InitTab: IDs in the range ',idmin,idmax
      allocate(itabtmp(idmax))
      itabtmp = -1 ! initialise to illegal index

      ! place the shuffled index in the array entry corresponding to the user id
!$omp parallel default(none) 
!$omp+ shared(numnod,itab1D,itabtmp) 
!$omp+ private(j)
!$omp do schedule(runtime)
      do j = 1,numnod
         itabtmp(itab1D(j)) = j
      enddo
!$omp enddo
!$omp end parallel

      ! Reshuffle the array into itabm1 removing all the illegal -1's
      j = 1
      do i = 1,numnod
         do while (itabtmp(j)==-1)
            j = j + 1                     ! advance to skip over -1
         enddo
         itabm1(i) = itabtmp(j) + ioffset ! fill in the correct entry using the correct offset value
         j = j + 1                        ! advance to next entry
      enddo
      deallocate(itabtmp)
      deallocate(itab1D)

      print*, 'Radioss2Viper: InitTab: exiting with ncol = ',ncol,' and N = ',numnod

      end subroutine RadiossViper_InitTab
C--------------------------------------------------------
C This will receive the initial time, final time and output frequency from Viper (note that Viper does not always start at time = 0)
C This will also receive the minimum timestep, select the largest value, and send the new value back to Viper
      subroutine RadiossViper_ReceiveSendInitialTimes(dt_min,t_max,dt_out,t_now)
      my_real, intent(inout) :: dt_min
      my_real, intent(out)   :: t_max,dt_out,t_now
      my_real                :: dt_min_viper
      integer                :: ierror

#ifdef MPI
      if (iverbose) then
         print*, 'Radioss2Viper: ReceiveSendInitialTimes: entering: t_now,t_max,dt_min,dt_out: ', t_now,t_max,dt_min,dt_out
      endif
      call MPI_RECV(dt_min_viper, 1, MY_MPI_REAL, 1, 9931, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      call MPI_RECV(t_max,        1, MY_MPI_REAL, 1, 9932, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      call MPI_RECV(dt_out,       1, MY_MPI_REAL, 1, 9933, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      call MPI_RECV(t_now,        1, MY_MPI_REAL, 1, 9934, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      dt_min = max(dt_min,dt_min_viper)
      call MPI_SEND(dt_min,       1, MY_MPI_REAL, 1, 9935, MPI_COMM_WORLD, ierror)
      if (iverbose) then
         print*, 'Radioss2Viper: ReceiveSendInitialTimes: exiting: t_now,t_max,dt_min,dt_out: ', t_now,t_max,dt_min,dt_out
      endif
#else
      t_now  = 0.
      t_max  = 0.
      dt_out = 0.
#endif

      end subroutine RadiossViper_ReceiveSendInitialTimes
C--------------------------------------------------------
C This will send the number of nodes and elements to Viper; viper will compare the values and sent back a kill-command if 
C there is a mismatch in numbers
      subroutine RadiossViper_ReceiveSendInitialNumbers(t_max,numnodes,numsolids,num4shells,num3shells)
      my_real, intent(inout) :: t_max
      integer, intent(in)    :: numnodes,numsolids,num4shells,num3shells
      integer                :: ikill,ierror

#ifdef MPI
      if (iverbose) print*, 'Radioss2Viper: Entering ReceiveSendInitialNumbers: ', t_max
      call MPI_SEND(numnodes,   1, MPI_INT, 1, 9940, MPI_COMM_WORLD, ierror)
      call MPI_SEND(numsolids,  1, MPI_INT, 1, 9941, MPI_COMM_WORLD, ierror)
      call MPI_SEND(num4shells, 1, MPI_INT, 1, 9942, MPI_COMM_WORLD, ierror)
      call MPI_SEND(num3shells, 1, MPI_INT, 1, 9943, MPI_COMM_WORLD, ierror)
      call MPI_RECV(ikill,      1, MPI_INT, 1, 9944, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      if (ikill == 1) then
         print*, 'Radioss2Viper: ReceiveSendInitialNumbers: ABORTING due to number mismatch'
         t_max = 0.
      endif
      if (iverbose) print*, 'Radioss2Viper: Exiting ReceiveSendInitialNumbers: ', t_max
#endif

      end subroutine RadiossViper_ReceiveSendInitialNumbers
C--------------------------------------------------------
C This will send nodal masses from to Viper for calculation of total energy; this needs to be done once
      subroutine RadiossViper_SendMass(numnod,MS,itabm1)

      integer, intent(in) :: numnod,itabm1(numnod)
      my_real, intent(in) :: MS(numnod)
      integer             :: i,ierror
      my_real             :: MSviper(numnod)

#ifdef MPI
      print*, 'Radioss2Viper: SendMass: entering with numnod = ',numnod
c     make new arrays where the elements are in the correct order
      do i = 1,numnod     ! tests show tha this is slower if openmp-parallel
         MSviper(i) = MS(itabm1(i))
      enddo
      call MPI_SEND(MSviper,numnod, MY_MPI_REAL, 1, 9930, MPI_COMM_WORLD,ierror)
      if (iverbose) print*, 'Radioss2Viper: SendMass: exiting'
#endif

      end subroutine RadiossViper_SendMass
C--------------------------------------------------------
C This will send the initial erosion status to Viper; this is required to inform Viper of void elements that need to be excluded calculations
      subroutine RadiossViper_SendInitialStatus(n,numele,numele_viper,nparg,ngroup,ixem1,iparg,elbuf_tab)

      integer, intent(in)    :: numele,numele_viper,nparg,ngroup
      integer, intent(in)    :: ixem1(numele_viper),iparg(nparg,ngroup)
      integer, intent(out)   :: n
      integer                :: i,j,k,ierror
      integer                :: Eviper(numele_viper),Evipertmp(numele)
      logical                :: viper_element
      type(ELBUF_STRUCT_),dimension(ngroup), target :: elbuf_tab

#ifdef MPI
      print*, 'Radioss2Viper: SendInitialStatus: entering:', numele,numele_viper
c     make new erosion arrary in Viper's order & determine the number of eroded elements
c     first, we put them in a contigious array; we will sort and send only if the number of active elements has changed
      n = 0
      k = 0
      do i = 1,ngroup
         do j = 1,iparg(2,i)
            if (iparg(5,i)==1 .or. iparg(5,i)==3 .or. iparg(5,i)==7) then
               viper_element = .true.
            else
               viper_element = .false.
            endif
            k = k + 1
            if (k <= numele) then
               if (ELBUF_TAB(i)%GBUF%OFF(j) == 1 .and. ELBUF_TAB(i)%BUFLY(1)%ILAW > 0 .and. viper_element) then
                  n = n + 1
                  Evipertmp(k) = ELBUF_TAB(i)%GBUF%OFF(j)   ! pass element status (eroded or not)
               else
                  Evipertmp(k) = -1                         ! pass element status defined as void
               endif
            endif
         enddo
      enddo
      if (iverbose) print*, 'Radioss2Viper: SendInitialStatus: filled primary array'
      do i = 1,numele_viper
         Eviper(i) = Evipertmp(ixem1(i))
      enddo
      if (iverbose) print*, 'Radioss2Viper: _SendInitialStatus: filled secondary array'
      call MPI_SEND(Eviper, numele_viper, MPI_INT, 1, 9950, MPI_COMM_WORLD,ierror)
      print*, 'Radioss2Viper: SendInitialStatus: exiting', numele,numele_viper,n
#endif

      end subroutine RadiossViper_SendInitialStatus
C--------------------------------------------------------
C This will send nodal positions to Viper
C This will also send element status (i.e., eroded or still active)
      subroutine RadiossViper_SendXVE(numnod,numele,numele_viper,nparg,ngroup,numonIO,ivoutIO,X,V,itabm1,ixem1,iparg,elbuf_tab)

      integer, intent(in)    :: numnod,numele,numele_viper,nparg,ngroup
      integer, intent(in)    :: itabm1(numnod),ixem1(numele_viper),iparg(nparg,ngroup)
      integer, intent(inout) :: numonIO,ivoutIO
      my_real, intent(in)    :: X(3,numnod),V(3,numnod)
      integer                :: i,j,k,n,ierror
      integer                :: Eviper(numele_viper),Evipertmp(numele)
      my_real                :: Xviper(3,numnod),Vviper(3,numnod)
      logical                :: viper_element,kill_element
      type(ELBUF_STRUCT_),dimension(ngroup), target :: elbuf_tab
      character(len=32)      :: fileout

      if (iverbose) print*, 'Radioss2Viper: Entering SendXVE '
#ifdef MPI
c     make temporary position & velocity arrays where the elements are in the correct order for Viper
      do i = 1,numnod     ! tests show that this is slower if openmp-parallel
         Xviper(1:3,i) = X(1:3,itabm1(i))
         Vviper(1:3,i) = V(1:3,itabm1(i))
      enddo
      call MPI_SEND(Xviper,  3*numnod, MY_MPI_REAL, 1, 9902, MPI_COMM_WORLD,ierror)
      call MPI_SEND(Vviper,  3*numnod, MY_MPI_REAL, 1, 9903, MPI_COMM_WORLD,ierror)

      if (iverbose) print*, 'Radioss2Viper: SendXVE: Sent position & velocity'
c     make new erosion arrary in Viper's order & determine the number of eroded elements
c     first, we put them in a contigious array; we will sort and send only if the number of active elements has changed
      n = 0
      k = 0
      do i = 1,ngroup
         do j = 1,iparg(2,i)
            if (iparg(5,i)==1 .or. iparg(5,i)==3 .or. iparg(5,i)==7) then
               viper_element = .true.
            else
               viper_element = .false.
            endif
            k = k + 1
c           Coupling tests where elements are manually eroded
c            if (ivoutIO==200 .and. k < 125 .and. .false.) then  ! Testing Chinook plate that is 2 FE thick
c            if (ivoutIO==200 .and. 0 < k .and. k < 101 .and. .false.) then  ! Testing Chinook plate that is 3 FE thick; removes central sheet
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
               endif
            endif
            if (k <= numele) then
               if (ELBUF_TAB(i)%GBUF%OFF(j) == 1 .and. ELBUF_TAB(i)%BUFLY(1)%ILAW > 0 .and. viper_element) then
                  n = n + 1
                  Evipertmp(k) = ELBUF_TAB(i)%GBUF%OFF(j)   ! pass active status
               else
                  Evipertmp(k) = -1                         ! failed, dead, null, eroded
               endif
            endif
         enddo
      enddo
      call MPI_SEND(n, 1, MPI_INT, 1, 9907, MPI_COMM_WORLD,ierror)
      print*, 'Radioss2Viper: SendXVE: numnod, nElements_prev, nElements = : ',numnod,numonIO,n
      if (numonIO /= n) then
         do i = 1,numele_viper
            Eviper(i) = Evipertmp(ixem1(i))
         enddo
         call MPI_SEND(Eviper, numele_viper, MPI_INT, 1, 9908, MPI_COMM_WORLD,ierror)
      endif
      numonIO = n

      ivoutIO = ivoutIO + 1
#endif

      end subroutine RadiossViper_SendXVE
C--------------------------------------------------------
C This will receive the FORCES on the nodes from Viper
C Despite the variable name being A, this is actually a force
C We add this to both A and to FEXT, where the latter is used only for output
      subroutine RadiossViper_ReceiveAccelerations(numnod,A,Fext,itabm1)

      integer, intent(in)    :: numnod,itabm1(numnod)
      my_real, intent(inout) :: A(3,numnod),Fext(3,numnod)
      my_real                :: Aviper(3,numnod)
      integer                :: i,j,ierror
      integer                :: iA(numnod)

#ifdef MPI
      if (iverbose) print*, 'Radioss2Viper: ReceiveAccelerations: ', numnod
      call MPI_RECV(Aviper, 3*numnod,MY_MPI_REAL, 1, 9910, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      do i = 1,numnod
         A(   1:3,itabm1(i)) = A(   1:3,itabm1(i)) + Aviper(1:3,i)
         Fext(1:3,itabm1(i)) = Fext(1:3,itabm1(i)) + Aviper(1:3,i)
      enddo
#endif
      end subroutine RadiossViper_ReceiveAccelerations
C--------------------------------------------------------
C This will pass Viper's timestep to OpenRadioss, compare the two, select the shortest;
C The shortest timestep will also be passed back to Viper
      subroutine RadiossViper_ReceiveSendDT(id_ViperCouplingIO,time,dt_rad)

      integer, intent(in)    :: id_ViperCouplingIO
      my_real, intent(in)    :: time
      my_real, intent(inout) :: dt_rad
      my_real                :: dt_viper, dt_rad_in
      integer                :: ierror
      character(len=16)      :: dt_selected

      dt_rad_in = dt_rad
#ifdef MPI
      call MPI_RECV(dt_viper, 1, MY_MPI_REAL, 1, 9925, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      dt_rad = min(dt_rad,dt_viper)
      call MPI_SEND(dt_rad,   1, MY_MPI_REAL, 1, 9926, MPI_COMM_WORLD, ierror)
#endif
      print*, 'Radioss2Viper: ReceiveSendDT: exiting: dt_rad_in, dt_viper_in, dt_out: ', dt_rad_in,dt_viper,dt_rad
      if (dt_rad_in < dt_viper) then
         dt_selected = 'dt_Radioss'
      else
         dt_selected = 'dt_Viper'
      endif
      write(id_ViperCouplingIO,'(3(a,Es13.6),3a,Es13.6)')
     .    'Time_Radioss=',time,'   dt_Radioss=',dt_rad_in,'   dt_Viper=',dt_viper,'   dt_selected=',trim(dt_selected),'=',dt_rad

      end subroutine RadiossViper_ReceiveSendDT
C--------------------------------------------------------
C This will send a kill-command to Viper if Radioss needs to end prematurely!
C There are commands that will modify tstop in Radioss to permit premature termination;
C We will compare the initial tstop with the current tstop to see if this change has been triggered
      subroutine RadiossViper_SendKill(mstop,tstop, tstop_viper)

      my_real, intent(in) :: tstop,tstop_viper
      integer             :: mstop
      integer             :: ikill, ierror

#ifdef MPI
      if (tstop < tstop_viper .or. mstop > 0) then
         print*, 'Radioss2Viper: SendKill: ABORTING!  Sending kill-command to Viper!'
         ikill = 1
      else
         ikill = 0
      endif
      call MPI_SEND(ikill,   1, MPI_INT, 1, 9960, MPI_COMM_WORLD, ierror)
#endif

      end subroutine RadiossViper_SendKill
C--------------------------------------------------------
      END MODULE VIPER_MOD
