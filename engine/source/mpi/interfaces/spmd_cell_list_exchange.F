Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    spmd_cell_list_exchange        ../engine/source/mpi/interfaces/spmd_cell_list_exchange.F
!||--- called by ------------------------------------------------------
!||    inter_prepare_sort             ../engine/source/interfaces/generic/inter_prepare_sort.F
!||--- calls      -----------------------------------------------------
!||    spmd_cell_size_exchange        ../engine/source/mpi/interfaces/spmd_cell_size_exchange.F
!||    spmd_cell_size_exchange_init   ../engine/source/mpi/interfaces/spmd_cell_size_exchange_init.F
!||    spmd_cell_size_post_rcv        ../engine/source/mpi/interfaces/spmd_cell_size_post_rcv.F
!||    spmd_waitany                   ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod                  ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter_sorting_mod              ../engine/share/modules/inter_sorting_mod.F
!||    inter_struct_mod               ../engine/share/modules/inter_struct_mod.F
!||    spmd_comm_world_mod            ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_mod                       ../engine/source/mpi/spmd_mod.F90
!||    tri7box                        ../engine/share/modules/tri7box.F
!||====================================================================
        SUBROUTINE SPMD_CELL_LIST_EXCHANGE(IRCVFROM,ISENDTO,MODE,WEIGHT,IAD_ELEM,
     .             FR_ELEM,X,V,MS,TEMP,
     .             KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .             NIN,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZE, GOT_PREVIEW,component)
!$COMMENT
!       SPMD_CELL_LIST_EXCHANGE description :
!       exchange of remote cell and preparation of comm "send/rcv the data of remote nodes"
!       SPMD_CELL_LIST_EXCHANGE organization :
!       * mode = 1 : send & rcv the number of cell & prepare the array for the next comm
!       * mode = 2 : - wait the previous comm "mode 1"
!                    - send & rcv the cell 
!       * mode = 3 : - wait the previous comm "send/rcv the cell"
!                    - send/rcv the number of secondary nodes   
!                    - prepare the S buffer of secondary nodes (x, v, temp ...)
!       * mode = 4 : - wait the S comm "send the cells"
!                    - deallocation              
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE INTER_SORTING_MOD
        USE INTER_STRUCT_MOD
        USE TRI7BOX
        use spmd_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NIN
        INTEGER, INTENT(in) :: MODE
        INTEGER, INTENT(in) :: NODNX_SMS_SIZ ! size of NODNX_SMS
        INTEGER, INTENT(in) :: TEMP_SIZE     ! size of TEMP
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRCVFROM
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) ::  IPARI
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: WEIGHT
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM ! connectivity array iad(P+1)-iad(P) = nb of frontier node
        INTEGER, DIMENSION(SFR_ELEM), INTENT(in) :: FR_ELEM ! frontier node ID
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X,V !   position & velocity
        my_real, DIMENSION(NUMNOD), INTENT(in) :: MS   !   mass 
        my_real, DIMENSION(TEMP_SIZE), INTENT(in) :: TEMP !   temperature
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB ! global node ID
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: KINET ! k energy 
        INTEGER, DIMENSION(NODNX_SMS_SIZ), INTENT(in) :: NODNX_SMS ! SMS array
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
        INTEGER, INTENT(IN) :: GOT_PREVIEW
        type(component_), dimension(ninter), intent(inout) :: component
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
#ifdef MPI        
        LOGICAL :: IS_EXCHANGE_NEEDED
        INTEGER :: KK,I,J,ijk
        INTEGER :: P,P_LOC,LOCAL_RANK,REMOTE_PROC
        INTEGER :: SIZE_,OLD_POINTER
        INTEGER :: ADRESS,SHIFT_
        INTEGER :: INDEX_S,INDEX_R
        INTEGER :: ITIED

        INTEGER IERROR1,STATUS(MPI_STATUS_SIZE),IERROR
        INTEGER :: TOTAL_RCV_SIZE,TOTAL_SEND_SIZE
        INTEGER :: LOC_PROC,ID_PROC
        INTEGER :: COUNT_COMM_SIZE_CELL,ID_COMM
        INTEGER :: DIPLS_
        INTEGER, DIMENSION(:), ALLOCATABLE :: ARRAY_REQUEST
        LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: ALREADY_SEND
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_ALREADY_SEND

        INTEGER :: MSGTYP,my_size
        INTEGER :: MSGOFF
        DATA MSGOFF/13000/
        integer, parameter :: my_tag = 13001
        logical :: my_condition
!   --------------------------------------------------------------------
        LOC_PROC = ISPMD + 1


        !   ----------------------------
        !   mode = 1
        !   send & rcv the number of cell & prepare the array for the next comm
        IF(MODE==1) THEN
            IF(IRCVFROM(NIN,LOC_PROC)==0.AND.ISENDTO(NIN,LOC_PROC)==0) RETURN !CYCLE
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%NB_CELL_PROC)) THEN
                SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
                ALLOCATE(SORT_COMM(NIN)%NB_CELL_PROC(SIZE_))
            ENDIF

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%SEND_NB_CELL)) THEN
                SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
                ALLOCATE(SORT_COMM(NIN)%SEND_NB_CELL(SIZE_))
            ENDIF

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%RCV_NB_CELL)) THEN
                SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
                ALLOCATE(SORT_COMM(NIN)%RCV_NB_CELL(SIZE_))
            ENDIF

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%SEND_DISPLS_NB_CELL)) THEN
                SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
                ALLOCATE(SORT_COMM(NIN)%SEND_DISPLS_NB_CELL(SIZE_))
            ENDIF

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%RCV_DISPLS_NB_CELL)) THEN
                SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
                ALLOCATE(SORT_COMM(NIN)%RCV_DISPLS_NB_CELL(SIZE_))
            ENDIF
            my_size = 0
            TOTAL_RCV_SIZE = 0
            DO I=1,SORT_COMM(NIN)%PROC_NUMBER
                ID_PROC = SORT_COMM(NIN)%PROC_LIST(I)
                SORT_COMM(NIN)%SEND_NB_CELL(I) = 0
                IF(ISENDTO(NIN,ID_PROC)>0) THEN    !   nsn of proc ID_PROC >0
                    SORT_COMM(NIN)%SEND_NB_CELL(I) = 1
                ENDIF
                SORT_COMM(NIN)%SEND_DISPLS_NB_CELL(I) = 0

                SORT_COMM(NIN)%RCV_NB_CELL(I) = 0
                IF(ISENDTO(NIN,LOC_PROC)>0) THEN   !   nsn of current proc >0
                    SORT_COMM(NIN)%RCV_NB_CELL(I) = 1
                ENDIF
                SORT_COMM(NIN)%RCV_DISPLS_NB_CELL(I) = TOTAL_RCV_SIZE
                IF(ISENDTO(NIN,LOC_PROC)>0) THEN   !   nsn of current proc >0
                    TOTAL_RCV_SIZE = TOTAL_RCV_SIZE + 1
                ENDIF
            ENDDO
            
            TOTAL_SEND_SIZE = 2

            if(.not.allocated(sort_comm(nin)%request_s)) then
              allocate(sort_comm(nin)%request_s(sort_comm(nin)%proc_number))
            endif
            if(.not.allocated(sort_comm(nin)%request_r)) then
              allocate(sort_comm(nin)%request_r(sort_comm(nin)%proc_number))
            endif
            if(.not.allocated(sort_comm(nin)%index_r)) then
              allocate(sort_comm(nin)%index_r(sort_comm(nin)%proc_number))
            endif 
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%SEND_SIZE_CELL)) THEN
              SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
              ALLOCATE(SORT_COMM(NIN)%SEND_SIZE_CELL(SIZE_))
            ENDIF

            sort_comm(nin)%request_s_nb = 0
            sort_comm(nin)%request_r_nb = 0                       
            my_size = 0            
            itied = ipari(85,nin)
            do i=1,sort_comm(nin)%proc_number
              id_proc = sort_comm(nin)%proc_list(i)
              sort_comm(nin)%nb_cell_proc(i) = 0
              sort_comm(nin)%send_size_cell(i) = 0
              my_condition = (itied/=0.and.id_proc-1/=ispmd).or.component(nin)%proc_comp(id_proc)%need_comm_r
              my_condition = my_condition.and.(isendto(nin,ispmd+1)>0)
              my_condition = my_condition.and.id_proc/=loc_proc
              if(my_condition) then
                sort_comm(nin)%request_r_nb = sort_comm(nin)%request_r_nb + 1
                sort_comm(nin)%index_r(sort_comm(nin)%request_r_nb) = i
                call spmd_irecv(sort_comm(nin)%nb_cell_proc(i),1,id_proc-1,my_tag, 
     .                       sort_comm(nin)%request_r(sort_comm(nin)%request_r_nb),SPMD_COMM_WORLD)    
              endif
              my_condition = (itied/=0.and.id_proc-1/=ispmd).or.component(nin)%proc_comp(id_proc)%need_comm_s
              my_condition = my_condition.and.(isendto(nin,id_proc)>0)
              my_condition = my_condition.and.id_proc/=loc_proc
              if(my_condition) then
                sort_comm(nin)%request_s_nb = sort_comm(nin)%request_s_nb + 1
                call spmd_isend(sort_comm(nin)%size_cell_list(1),1,id_proc-1,my_tag, 
     .                        sort_comm(nin)%request_s(sort_comm(nin)%request_s_nb),SPMD_COMM_WORLD)
                             my_size = my_size + 1
                sort_comm(nin)%send_size_cell(i) = sort_comm(nin)%size_cell_list(1)
              endif
            enddo

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%SEND_DISPLS_CELL)) THEN
                SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
                ALLOCATE(SORT_COMM(NIN)%SEND_DISPLS_CELL(SIZE_))
            ENDIF

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%RCV_DISPLS_CELL)) THEN
                SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
                ALLOCATE(SORT_COMM(NIN)%RCV_DISPLS_CELL(SIZE_))
            ENDIF
       ENDIF
        !   ----------------------------


        !   ----------------------------
        !   mode = 2
        !   wait the previous comm 
        !   send & rcv the cell
       IF(MODE==2) THEN
            !   -------------------------
            !   wait the previous comm
            SORT_COMM(NIN)%NB_REQUEST_CELL_SEND = 0
            SORT_COMM(NIN)%NB_REQUEST_CELL_RCV = 0
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%RCV_SIZE_CELL)) THEN
              SIZE_ = SORT_COMM(NIN)%PROC_NUMBER
              ALLOCATE(SORT_COMM(NIN)%RCV_SIZE_CELL(SIZE_))
            ENDIF
            SORT_COMM(NIN)%RCV_SIZE_CELL(:) = 0

            IF(IRCVFROM(NIN,LOC_PROC)==0.AND.ISENDTO(NIN,LOC_PROC)==0) RETURN

            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%REQUEST_CELL_SEND)) THEN
                ALLOCATE(SORT_COMM(NIN)%REQUEST_CELL_SEND(SORT_COMM(NIN)%PROC_NUMBER))
                SORT_COMM(NIN)%REQUEST_CELL_SEND(1:SORT_COMM(NIN)%PROC_NUMBER) = 0
            ENDIF
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%REQUEST_CELL_RCV)) THEN
                ALLOCATE(SORT_COMM(NIN)%REQUEST_CELL_RCV(SORT_COMM(NIN)%PROC_NUMBER))
                SORT_COMM(NIN)%REQUEST_CELL_RCV(1:SORT_COMM(NIN)%PROC_NUMBER) = 0
            ENDIF
            IF(.NOT.ALLOCATED(SORT_COMM(NIN)%INDEX_RCV)) THEN
                ALLOCATE(SORT_COMM(NIN)%INDEX_RCV(SORT_COMM(NIN)%PROC_NUMBER))
                SORT_COMM(NIN)%INDEX_RCV(1:SORT_COMM(NIN)%PROC_NUMBER) = 0
            ENDIF
            TOTAL_RCV_SIZE = 0
            do ijk=1,sort_comm(nin)%request_r_nb
              call spmd_waitany(sort_comm(nin)%request_r_nb,sort_comm(nin)%request_r,index_r,status)         
              i = sort_comm(nin)%index_r(index_r)
            enddo
            do i=1,sort_comm(nin)%proc_number
              id_proc = sort_comm(nin)%proc_list(i)

                SORT_COMM(NIN)%SEND_DISPLS_CELL(I) = 0

                SORT_COMM(NIN)%RCV_SIZE_CELL(I) = 0
                IF(ISENDTO(NIN,LOC_PROC)>0) THEN   !   nsn of current proc >0
                    SORT_COMM(NIN)%RCV_SIZE_CELL(I) = SORT_COMM(NIN)%NB_CELL_PROC(I)
                ENDIF
                SORT_COMM(NIN)%RCV_DISPLS_CELL(I) = TOTAL_RCV_SIZE
                IF(ISENDTO(NIN,LOC_PROC)>0) THEN   !   nsn of current proc >0
                    TOTAL_RCV_SIZE = TOTAL_RCV_SIZE + SORT_COMM(NIN)%NB_CELL_PROC(I)
                ENDIF
            ENDDO

            do ijk=1,sort_comm(nin)%request_s_nb
              call spmd_waitany(sort_comm(nin)%request_s_nb,sort_comm(nin)%request_s,index_r,status)
            enddo

            TOTAL_SEND_SIZE = SORT_COMM(NIN)%SIZE_CELL_LIST(1)

            IF(ALLOCATED(SORT_COMM(NIN)%CELL) ) DEALLOCATE(SORT_COMM(NIN)%CELL)
            ALLOCATE( SORT_COMM(NIN)%CELL(TOTAL_RCV_SIZE) )

            ITIED = IPARI(85,NIN)

            !   ----------------------
            !   rcv of remote cell 
            DO I=1,SORT_COMM(NIN)%PROC_NUMBER
                ID_PROC = SORT_COMM(NIN)%PROC_LIST(I)
                my_condition = (itied/=0.or.SORT_COMM(NIN)%RCV_SIZE_CELL(I)>0)
                my_condition = my_condition.and.(isendto(nin,ispmd+1)>0) 
                my_condition = my_condition.and.id_proc/=loc_proc                     
                IF(my_condition) THEN                  
                    MSGTYP = MSGOFF    
                    SORT_COMM(NIN)%NB_REQUEST_CELL_RCV = SORT_COMM(NIN)%NB_REQUEST_CELL_RCV + 1  
                    SORT_COMM(NIN)%INDEX_RCV(SORT_COMM(NIN)%NB_REQUEST_CELL_RCV) = I  
                    CALL MPI_IRECV(SORT_COMM(NIN)%CELL(SORT_COMM(NIN)%RCV_DISPLS_CELL(I) + 1), 
     .              SORT_COMM(NIN)%RCV_SIZE_CELL(I),MPI_INTEGER,IT_SPMD(ID_PROC),
     .              MSGTYP,SPMD_COMM_WORLD,SORT_COMM(NIN)%REQUEST_CELL_RCV(SORT_COMM(NIN)%NB_REQUEST_CELL_RCV),IERROR)
                ENDIF
            ENDDO
            !   ----------------------

            !   ----------------------
            !   send of cell 
            DO I=1,SORT_COMM(NIN)%PROC_NUMBER
                ID_PROC = SORT_COMM(NIN)%PROC_LIST(I)
                my_condition = (itied/=0.or.SORT_COMM(NIN)%SEND_SIZE_CELL(I)>0)
                my_condition = my_condition.and.(isendto(nin,id_proc)>0)
                my_condition = my_condition.and.id_proc/=loc_proc               
                IF(my_condition) THEN 
                    MSGTYP = MSGOFF   
                    SORT_COMM(NIN)%NB_REQUEST_CELL_SEND = 
     .                  SORT_COMM(NIN)%NB_REQUEST_CELL_SEND + 1  
                    CALL MPI_ISEND(SORT_COMM(NIN)%CELL_LIST, 
     .              SORT_COMM(NIN)%SEND_SIZE_CELL(I),MPI_INTEGER,IT_SPMD(ID_PROC),
     .              MSGTYP,SPMD_COMM_WORLD,SORT_COMM(NIN)%REQUEST_CELL_SEND(SORT_COMM(NIN)%NB_REQUEST_CELL_SEND),IERROR)
                ENDIF
            ENDDO
            !   ----------------------
       ENDIF
        !   ----------------------------

        !   ----------------------------
        !   mode = 3
        !   wait the previous comm "send/rcv the cell"
        !   send/rcv the number of secondary nodes
        !   prepare the S buffer of secondary nodes (x, v, temp ...)
        IF(MODE==3) THEN
            !   -------------------------
            !   wait the previous comm
            CALL SPMD_CELL_SIZE_EXCHANGE_INIT(IRCVFROM,ISENDTO,IPARI,NIN,INTER_STRUCT,SORT_COMM)  
            NSNFI(NIN)%P(1:NSPMD) = 0
            IF(IRCVFROM(NIN,LOC_PROC)==0.AND.ISENDTO(NIN,LOC_PROC)==0) RETURN

            ITIED = IPARI(85,NIN)
            IF(SORT_COMM(NIN)%SIZE_CELL_LIST(1)>0.OR.ITIED/=0) THEN !   need to prepare the reception only if there are still some NMN on the current proc
                CALL SPMD_CELL_SIZE_POST_RCV(IRCVFROM,ISENDTO,NIN,SORT_COMM,ITIED,component)  
            ENDIF
            ALLOCATE( ALREADY_SEND(NB_CELL_X,NB_CELL_Y,NB_CELL_Z) )
            IF(ISENDTO(NIN,LOC_PROC)/=0) ALREADY_SEND(1:NB_CELL_X,1:NB_CELL_Y,1:NB_CELL_Z) = .FALSE.
            ALLOCATE( INDEX_ALREADY_SEND(NB_CELL_X*NB_CELL_Y*NB_CELL_Z) )
            !   -------------------------
            if(GOT_PREVIEW == 1) THEN
            CALL MPI_WAITALL( SORT_COMM(NIN)%NB_REQUEST_CELL_RCV, SORT_COMM(NIN)%REQUEST_CELL_RCV,MPI_STATUSES_IGNORE,IERROR )
            ENDIF

            DO KK=1,SORT_COMM(NIN)%NB_REQUEST_CELL_RCV
                INDEX_R = KK 
                !   wait the first message

               if(GOT_PREVIEW == 0 ) then
               CALL MPI_WAITANY( SORT_COMM(NIN)%NB_REQUEST_CELL_RCV,
     .                            SORT_COMM(NIN)%REQUEST_CELL_RCV,INDEX_R,STATUS,IERROR )
               endif

                REMOTE_PROC = SORT_COMM(NIN)%INDEX_RCV(INDEX_R)
                IS_EXCHANGE_NEEDED = .TRUE.

                IF(SORT_COMM(NIN)%PROC_LIST(REMOTE_PROC)==ISPMD+1) IS_EXCHANGE_NEEDED = .FALSE.
                IF(IS_EXCHANGE_NEEDED.AND.SORT_COMM(NIN)%NB_CELL_PROC(REMOTE_PROC)==0) IS_EXCHANGE_NEEDED = .FALSE. !   proc without any main node cell
                IF(ITIED/=0.AND.IRCVFROM(NIN,REMOTE_PROC)/=0)  IS_EXCHANGE_NEEDED = .TRUE.   ! itied option : force the exchange    
                !   -------------------------
                IF(IS_EXCHANGE_NEEDED) THEN
                    !   ISEND the number of secondary nodes
                    !   prepare the S buffer of secondary nodes (x, v, temp ...)
                    CALL SPMD_CELL_SIZE_EXCHANGE(IRCVFROM,ISENDTO,WEIGHT,
     .                IAD_ELEM,FR_ELEM,X,V,MS,TEMP,KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,NIN,REMOTE_PROC,
     .                ALREADY_SEND,INDEX_ALREADY_SEND,SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZE)
                ENDIF
                !   ------------------------- 
            ENDDO
            !   -------------------------
            DEALLOCATE( ALREADY_SEND )
            DEALLOCATE( INDEX_ALREADY_SEND )
        ENDIF
        !   ----------------------------

        !   ----------------------------
        !   mode = 4
        !   wait the S comm "send the cells"
        !   and deallocation
        IF(MODE==4) THEN
            IF(IRCVFROM(NIN,LOC_PROC)==0.AND.ISENDTO(NIN,LOC_PROC)==0) RETURN
                CALL MPI_WAITALL( SORT_COMM(NIN)%NB_REQUEST_CELL_SEND,
     .                            SORT_COMM(NIN)%REQUEST_CELL_SEND,MPI_STATUSES_IGNORE,IERROR )
      
            IF(ALLOCATED(SORT_COMM(NIN)%SEND_SIZE_CELL)) DEALLOCATE(SORT_COMM(NIN)%SEND_SIZE_CELL)
            IF(ALLOCATED(SORT_COMM(NIN)%SEND_DISPLS_CELL)) DEALLOCATE(SORT_COMM(NIN)%SEND_DISPLS_CELL)
            IF(ALLOCATED(SORT_COMM(NIN)%RCV_SIZE_CELL)) DEALLOCATE(SORT_COMM(NIN)%RCV_SIZE_CELL)
            IF(ALLOCATED(SORT_COMM(NIN)%RCV_DISPLS_CELL)) DEALLOCATE(SORT_COMM(NIN)%RCV_DISPLS_CELL)
            IF(ALLOCATED(SORT_COMM(NIN)%CELL)) DEALLOCATE(SORT_COMM(NIN)%CELL)
        ENDIF
        !   ----------------------------

!   --------------------------------------------------------------------
#endif
        RETURN
        END SUBROUTINE SPMD_CELL_LIST_EXCHANGE
