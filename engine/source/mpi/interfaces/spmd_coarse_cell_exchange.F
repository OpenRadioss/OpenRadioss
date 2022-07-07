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
Chd|  SPMD_COARSE_CELL_EXCHANGE     source/mpi/interfaces/spmd_coarse_cell_exchange.F
Chd|-- called by -----------
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|-- calls ---------------
Chd|        CHECK_COARSE_GRID             source/interfaces/generic/check_coarse_grid.F
Chd|        SPMD_IALLTOALLV_INT           source/mpi/generic/spmd_ialltoallv_int.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|====================================================================
        SUBROUTINE SPMD_COARSE_CELL_EXCHANGE(NB_INTER_SORTED,LIST_INTER_SORTED,IRECVFROM,ISENDTO,MODE,
     .              IPARI,SORT_COMM,NB_REQUEST_COARSE_CELL,ARRAY_REQUEST_COARSE_CELL,LIST_INTER_COARSE_CELL)
!$COMMENT
!       SPMD_COARSE_CELL_EXCHANGE description :
!       for large interfaces : communication of coarse cells with alltoll mpi comm 
!       and check if 2 procs need to echange data
!
!       SPMD_COARSE_CELL_EXCHANGE organization :
!           First part MODE=1 : alltoall comm --> exchange of coarse cell
!           Second part MODE=2 : wait & check if 2 processors for a given interface need to echange data
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_SORTING_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
        INTEGER, INTENT(in) :: MODE ! mode : 1 --> end/rcv / 2 --> wait + computation
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRECVFROM ! array for S and R : isendto = nsn ; IRECVFROM = nmn
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) ::  IPARI !   interface data
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
        INTEGER, INTENT(inout) :: NB_REQUEST_COARSE_CELL ! number of request
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(inout) :: ARRAY_REQUEST_COARSE_CELL ! array of request
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(inout) :: LIST_INTER_COARSE_CELL    ! list of interface
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI        
        INTEGER :: KK,NIN,I,J
        INTEGER :: P,P_LOC,LOCAL_RANK
        INTEGER :: MY_SIZE,OLD_POINTER
        INTEGER :: ADRESS,SHIFT_

        INTEGER IERROR1,STATUS(MPI_STATUS_SIZE),IERROR
        INTEGER :: SIZE_CELL_LIST,TOTAL_RCV_SIZE,TOTAL_SEND_SIZE
        INTEGER :: LOC_PROC,ID_PROC
        INTEGER :: COUNT_COMM_SIZE_CELL,ID_COMM
        INTEGER :: ITIED
!   ----------------------------------------
        LOC_PROC = ISPMD + 1
        !   -------------------------
        !   MODE=1 : alltoall comm --> exchange of coarse cell
        IF(MODE==1) THEN
          NB_REQUEST_COARSE_CELL = 0
          DO KK=1,NB_INTER_SORTED
            NIN = LIST_INTER_SORTED(KK)
            ARRAY_REQUEST_COARSE_CELL(KK) = MPI_REQUEST_NULL
            IF(SORT_COMM(NIN)%PROC_NUMBER>NSPMD/2) THEN
                IF(IRECVFROM(NIN,LOC_PROC).EQ.0.AND.ISENDTO(NIN,LOC_PROC).EQ.0) CYCLE

                IF(.NOT.ALLOCATED(SORT_COMM(NIN)%SEND_SIZE_COARSE_CELL)) THEN
                    MY_SIZE = SORT_COMM(NIN)%PROC_NUMBER
                    ALLOCATE(SORT_COMM(NIN)%SEND_SIZE_COARSE_CELL(MY_SIZE))
                ENDIF

                IF(.NOT.ALLOCATED(SORT_COMM(NIN)%RCV_SIZE_COARSE_CELL)) THEN
                    MY_SIZE = SORT_COMM(NIN)%PROC_NUMBER
                    ALLOCATE(SORT_COMM(NIN)%RCV_SIZE_COARSE_CELL(MY_SIZE))
                ENDIF

                IF(.NOT.ALLOCATED(SORT_COMM(NIN)%SEND_DISPLS_COARSE_CELL)) THEN
                    MY_SIZE = SORT_COMM(NIN)%PROC_NUMBER
                    ALLOCATE(SORT_COMM(NIN)%SEND_DISPLS_COARSE_CELL(MY_SIZE))
                ENDIF

                IF(.NOT.ALLOCATED(SORT_COMM(NIN)%RCV_DISPLS_COARSE_CELL)) THEN
                    MY_SIZE = SORT_COMM(NIN)%PROC_NUMBER
                    ALLOCATE(SORT_COMM(NIN)%RCV_DISPLS_COARSE_CELL(MY_SIZE))
                ENDIF

!isendto = nsn
!IRECVFROM = nmn
                TOTAL_RCV_SIZE = 0
                DO I=1,SORT_COMM(NIN)%PROC_NUMBER
                    ID_PROC = SORT_COMM(NIN)%PROC_LIST(I)
                    SORT_COMM(NIN)%SEND_SIZE_COARSE_CELL(I) = 0
                    IF(ISENDTO(NIN,LOC_PROC)>0.AND.IRECVFROM(NIN,ID_PROC)>0) THEN    !   nmn of proc ID_PROC >0
                        SORT_COMM(NIN)%SEND_SIZE_COARSE_CELL(I) = NB_BOX_COARSE_GRID**3 + 1
                    ENDIF
                    SORT_COMM(NIN)%SEND_DISPLS_COARSE_CELL(I) = 0

                    SORT_COMM(NIN)%RCV_SIZE_COARSE_CELL(I) = 0
                    IF(IRECVFROM(NIN,LOC_PROC)>0.AND.ISENDTO(NIN,ID_PROC)>0) THEN   !   nmn of current proc >0
                        SORT_COMM(NIN)%RCV_SIZE_COARSE_CELL(I) = NB_BOX_COARSE_GRID**3 + 1
                    ENDIF
                    SORT_COMM(NIN)%RCV_DISPLS_COARSE_CELL(I) = TOTAL_RCV_SIZE
                    IF(IRECVFROM(NIN,LOC_PROC)>0.AND.ISENDTO(NIN,ID_PROC)>0) THEN   !   nmn of current proc >0
                        TOTAL_RCV_SIZE = TOTAL_RCV_SIZE + NB_BOX_COARSE_GRID**3 + 1
                    ENDIF
                ENDDO

                IF(.NOT.ALLOCATED(SORT_COMM(NIN)%GLOBAL_COARSE_CELL ) )THEN                
                    ALLOCATE(SORT_COMM(NIN)%GLOBAL_COARSE_CELL(TOTAL_RCV_SIZE))
                ENDIF
                SORT_COMM(NIN)%SIZE_GLOBAL_COARSE_CELL = TOTAL_RCV_SIZE

                IF(ISENDTO(NIN,LOC_PROC)>0) TOTAL_SEND_SIZE = NB_BOX_COARSE_GRID**3 + 1
       
                NB_REQUEST_COARSE_CELL = NB_REQUEST_COARSE_CELL + 1
                LIST_INTER_COARSE_CELL(NB_REQUEST_COARSE_CELL) = NIN

                CALL SPMD_IALLTOALLV_INT(SORT_COMM(NIN)%COARSE_GRID,
     .            SORT_COMM(NIN)%GLOBAL_COARSE_CELL,SORT_COMM(NIN)%SEND_SIZE_COARSE_CELL,TOTAL_SEND_SIZE,
     .            SORT_COMM(NIN)%SEND_DISPLS_COARSE_CELL,
     .            TOTAL_RCV_SIZE,SORT_COMM(NIN)%RCV_SIZE_COARSE_CELL,
     .            SORT_COMM(NIN)%RCV_DISPLS_COARSE_CELL,ARRAY_REQUEST_COARSE_CELL(NB_REQUEST_COARSE_CELL),
     .            SORT_COMM(NIN)%COMM,SORT_COMM(NIN)%PROC_NUMBER)
            ENDIF               
          ENDDO
        ENDIF
        !   -------------------------
        !   MODE=2 : - wait the previous comm
        !            - check if curent proc and remote proc need to communicate
        IF(MODE==2) THEN
            DO KK=1,NB_REQUEST_COARSE_CELL
                CALL MPI_WAIT(ARRAY_REQUEST_COARSE_CELL(KK),STATUS,IERROR)  
                NIN = LIST_INTER_COARSE_CELL(KK)
                ITIED = IPARI(85,NIN)
                CALL CHECK_COARSE_GRID(NIN,SORT_COMM(NIN)%MAIN_COARSE_GRID,SORT_COMM,ITIED)

                DEALLOCATE( SORT_COMM(NIN)%GLOBAL_COARSE_CELL )
                DEALLOCATE( SORT_COMM(NIN)%COARSE_GRID )
            ENDDO 
            NB_REQUEST_COARSE_CELL = 0                   
        ENDIF
!   -------------------------

#endif
        END SUBROUTINE SPMD_COARSE_CELL_EXCHANGE
