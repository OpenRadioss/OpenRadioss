Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  INTER_SORTING_MOD    share/modules/inter_sorting_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
        MODULE INTER_SORTING_MOD
#include      "my_real.inc"    
!   -------------------------    
            LOGICAL :: NEED_COMM_INACTI         ! flag : true if inacti comm is mandatory
            INTEGER :: NB_INTER_INACTI          ! number of interface with inacti > 0
            INTEGER :: NEED_TO_SORT             ! flag : 1 if 1 or several interfaces must be sorted
            INTEGER :: NB_INTER_7_INACTI        !   number of interface 7 with inacti /= 0
            INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_INTER_7_INACTI !   list of interface 7 with inacti /= 0

            INTEGER :: COMM_INACTI      ! sub-communicator for INACTI mpi comm
            INTEGER :: REQUEST_INACTI   ! request for inacti communication

            my_real, DIMENSION(6) :: BOX_LIMIT  ! min & max positions of the grid
            INTEGER, PARAMETER :: NB_BOX_COARSE_GRID=4
            INTEGER, PARAMETER :: NB_BOX_LIMIT=128
            INTEGER :: NB_CELL_X,NB_CELL_Y,NB_CELL_Z    !   number of cell of the global voxel

            INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_COARSE_GRID

            !   ---------------------
            TYPE buffer_comm_type
                !   buffer for communication of position/velocity
                INTEGER, DIMENSION(:), ALLOCATABLE :: IBUF
                my_real, DIMENSION(:), ALLOCATABLE :: RBUF
            END TYPE buffer_comm_type
            !   ---------------------
            TYPE sorting_comm_type
                INTEGER :: COMM              ! sub communicator for communication 
                !   -----------------------------
                !   COARSE CELL data
                INTEGER :: PROC_NUMBER              ! number of processor for the interface NIN
                INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_LIST ! list of processor for interface NIN
                INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_SIZE_COARSE_CELL    ! number of cell S
                INTEGER, DIMENSION(:), ALLOCATABLE :: RCV_SIZE_COARSE_CELL     ! number of cell R
                INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_DISPLS_COARSE_CELL   ! displacement for S
                INTEGER, DIMENSION(:), ALLOCATABLE :: RCV_DISPLS_COARSE_CELL    ! displacement for R
                INTEGER, DIMENSION(:), ALLOCATABLE :: COARSE_GRID               ! list of local cell colored by secondary nodes S (S --> R) : size = NB_BOX_COARSE_GRID**3 + 1
                INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: MAIN_COARSE_GRID          ! list of cell colored by main node
                INTEGER :: SIZE_GLOBAL_COARSE_CELL                              ! total number of cell 
                INTEGER, DIMENSION(:), ALLOCATABLE :: GLOBAL_COARSE_CELL        ! list of remote cell colored by secondary nodes R : size = nspmd x (NB_BOX_COARSE_GRID**3 + 1)

                LOGICAL, DIMENSION(:), ALLOCATABLE :: KEEP_PROC                 ! boolean, true if current proc and P proc need to communicate 
                !   -----------------------------
                !   FINE CELL data
                INTEGER, DIMENSION(2) :: SIZE_CELL_LIST ! 1 = total number of cell  ; 2 = always 0 for mpi communication
!                INTEGER, DIMENSION(:,:), ALLOCATABLE :: CELL_LIST ! list of local cell  (size = (total number of cell,3) )
                INTEGER, DIMENSION(:), ALLOCATABLE :: CELL_LIST ! list of local cell  (size = (total number of cell,3) )

                !   -----------------------------
                !   exchange of number of cell (fine grid) : mode = 1
                INTEGER, DIMENSION(:), ALLOCATABLE :: NB_CELL_PROC    ! number of cell on the other processor (size= number of proc on a given interface)
                INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_NB_CELL    ! number of sent data for fine grid comm (mode=1)
                INTEGER, DIMENSION(:), ALLOCATABLE :: RCV_NB_CELL     ! number of rcv data for fine grid comm (mode=1)
                INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_DISPLS_NB_CELL  ! displacement for send
                INTEGER, DIMENSION(:), ALLOCATABLE :: RCV_DISPLS_NB_CELL   ! displacement for recv

                !   -----------------------------
                !   exchange of cell (fine grid) : mode = 2
                INTEGER :: REQUEST_NB_CELL                              ! request for cell size exchange
                INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_SIZE_CELL    ! number of sent data for fine grid comm (mode=1)
                INTEGER, DIMENSION(:), ALLOCATABLE :: RCV_SIZE_CELL     ! number of rcv data for fine grid comm (mode=1)
                INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_DISPLS_CELL  ! displacement for send
                INTEGER, DIMENSION(:), ALLOCATABLE :: RCV_DISPLS_CELL   ! displacement for recv
                INTEGER, DIMENSION(:), ALLOCATABLE :: CELL              ! list of remote cells

                !   -----------------------------
                !   wait the comm "exchange of cell -- mode=2" + send/rcv of secondary nodes (fine grid) : mode = 3
                INTEGER :: RSIZ     ! size of real buffer
                INTEGER :: ISIZ     ! size of int buffer
                INTEGER, DIMENSION(:), ALLOCATABLE :: REQUEST_NB_S  ! request for S : number of secondary node comm
                INTEGER, DIMENSION(:), ALLOCATABLE :: REQUEST_NB_R  ! request for S : number of secondary node comm
                INTEGER, DIMENSION(:), ALLOCATABLE :: NB            ! number of secondary node for each processor
                INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_RCV     ! index of processor
                INTEGER :: NBSEND_NB ! number of send comm "exchange of secondary node number"
                INTEGER :: NBRECV_NB ! number of rcv comm "exchange of secondary node number"
                INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_NB ! index of send comm "exchange of secondary node number"
                INTEGER, DIMENSION(:), ALLOCATABLE :: RECV_NB ! index of rcv comm "exchange of secondary node number"

                INTEGER :: NB_REQUEST_CELL_SEND                         ! number of request S "exchange of cell"
                INTEGER :: NB_REQUEST_CELL_RCV                          ! number of request R "exchange of cell"
                INTEGER, DIMENSION(:), ALLOCATABLE :: REQUEST_CELL_SEND ! list of request S "exchange of cell"
                INTEGER, DIMENSION(:), ALLOCATABLE :: REQUEST_CELL_RCV  ! list of request R "exchange of cell"
                type(buffer_comm_type), DIMENSION(:), ALLOCATABLE :: DATA_PROC
                !   -----------------------------
                INTEGER :: NBIRECV  ! number of rcv comm
                INTEGER :: NSNR     ! total number of remote secondary nodes
                INTEGER, DIMENSION(:), ALLOCATABLE :: ISINDEXI  ! index of processor
                !   -----------------------------
                !   VOXEL of secondary nodes
                INTEGER, DIMENSION(:), ALLOCATABLE :: IIX,IIY,IIZ  ! index in the x/y/z direction
                INTEGER, DIMENSION(:), ALLOCATABLE :: LAST_NOD     ! adress of the last node in the VOXEL
                INTEGER, DIMENSION(:), ALLOCATABLE :: NEXT_NOD     ! adress of the next node in the VOXEL
                INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: VOXEL    ! voxel of secondary nodes
                !   -----------------------------
                !   Communication of secondary nodes :
                INTEGER, DIMENSION(:), ALLOCATABLE :: REQ_SD2       ! request for Send : XREM comm
                INTEGER, DIMENSION(:), ALLOCATABLE :: REQ_SD3       ! request for Send : IREM comm
                !   -----------------------------
            END TYPE sorting_comm_type
              
            LOGICAL :: COARSE_GRID
            INTEGER, DIMENSION(:), ALLOCATABLE :: NB_LOCAL_CELL
            LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: CELL_BOOL
!   -------------------------    
        END MODULE INTER_SORTING_MOD



