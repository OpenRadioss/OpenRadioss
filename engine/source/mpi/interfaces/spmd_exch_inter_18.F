Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
        SUBROUTINE SPMD_EXCH_INTER_18(NINTER,NSPMD,NUMBER_INTER18,SXCELL,INTER18_LIST,
     .                 XCELL,MULTI_FVM,XCELL_REMOTE,INTBUF_TAB,ALE_CONNECTIVITY)
!$COMMENT
!       SPMD_EXCH_INTER_18 description : exchange of remote data (XCELL) between processor
!       
!       SPMD_EXCH_INTER_18 organization :           
!        * allocation of buffer + remote xcell array
!        * compute the size & adress for mpi comm
!        * post the rcv comm
!        * send the data
!        * wait the rcv comm + save the remote data
!        * wait the send comm
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD 
        USE ARRAY_MOD 
        USE TRI7BOX
        USE MULTI_FVM_MOD
        USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------    
        INTEGER, INTENT(in) :: NINTER !< number of interface
        INTEGER, INTENT(in) :: NSPMD !< number of mpi tasks
        INTEGER, INTENT(inout) :: NUMBER_INTER18 !< number of interface 18
        INTEGER, INTENT(in) :: SXCELL !< size of characteristic lenght array
        INTEGER, DIMENSION(NUMBER_INTER18), INTENT(inout) :: INTER18_LIST !< list of interface 18
        my_real, DIMENSION(3,SXCELL), INTENT(in) :: XCELL !< characteristic lenght
        TYPE(MULTI_FVM_STRUCT), INTENT(inout) :: MULTI_FVM
        TYPE(array_type), DIMENSION(NINTER), INTENT(inout) :: XCELL_REMOTE !< remote data structure for interface 18 
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    !< interface data 
        TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY !< ale connectivity structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
#ifdef MPI        
        INTEGER :: I,J,K,IJK,P,N
        INTEGER :: MY_SIZE
        INTEGER :: LOC_PROC
        INTEGER :: NIN,NODE_ID,ELEM_ID,NUMBER_REMOTE_NODE
        INTEGER :: BUFFER_SEND_SIZE,BUFFER_RCV_SIZE
        INTEGER :: SEND_SIZE,RCV_SIZE
        INTEGER :: LOCAL_ADRESS
        INTEGER, DIMENSION(NINTER) :: ADRESS_INTER
        INTEGER, DIMENSION(NSPMD+1) :: ADRESS_SEND,ADRESS_RCV

        INTEGER :: IAD1,IAD2
        my_real :: DL
        my_real, DIMENSION(:), ALLOCATABLE :: BUFFER_SEND,BUFFER_RCV

        INTEGER :: MSGTYP
        INTEGER :: ERROR_MPI
        INTEGER, DIMENSION(NSPMD) :: REQUEST_SEND,REQUEST_RCV
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS_MPI
        INTEGER :: MSGOFF
        DATA MSGOFF/13016/
!   --------------------------------------------------------------------
        LOC_PROC = ISPMD + 1

        ! ---------------------------
        ! allocation of remote array
        DO I=1,NUMBER_INTER18
          NIN = INTER18_LIST(I)
          MY_SIZE = 0 
          DO P=1,NSPMD
            MY_SIZE = MY_SIZE + NSNFI(NIN)%P(P)
          ENDDO
          IF(XCELL_REMOTE(NIN)%SIZE_MY_REAL_ARRAY_1D < MY_SIZE) THEN
            IF( ALLOCATED(XCELL_REMOTE(NIN)%MY_REAL_ARRAY_1D) ) CALL DEALLOC_MY_REAL_1D_ARRAY(XCELL_REMOTE(NIN))
            XCELL_REMOTE(NIN)%SIZE_MY_REAL_ARRAY_1D = MY_SIZE
            CALL ALLOC_MY_REAL_1D_ARRAY(XCELL_REMOTE(NIN))
          ENDIF
        ENDDO
        ! ---------------------------

        ! ---------------------------
        ! compute the size (send & rcv) and the adress in the buffer (send & rcv)
        BUFFER_SEND_SIZE = 0
        BUFFER_RCV_SIZE = 0

        ADRESS_SEND(1:NSPMD+1) = 0
        ADRESS_RCV(1:NSPMD+1) = 0

        DO P=1,NSPMD
          ADRESS_SEND(P) = BUFFER_SEND_SIZE + 1
          ADRESS_RCV(P) = BUFFER_RCV_SIZE + 1
          DO I=1,NUMBER_INTER18
            NIN = INTER18_LIST(I)
            BUFFER_SEND_SIZE = BUFFER_SEND_SIZE + NSNSI(NIN)%P(P)
            BUFFER_RCV_SIZE = BUFFER_RCV_SIZE + NSNFI(NIN)%P(P)
          ENDDO
        ENDDO

        ADRESS_SEND(NSPMD+1) = BUFFER_SEND_SIZE + 1
        ADRESS_RCV(NSPMD+1) = BUFFER_RCV_SIZE + 1
        ALLOCATE( BUFFER_SEND(BUFFER_SEND_SIZE) )
        ALLOCATE( BUFFER_RCV(BUFFER_RCV_SIZE) )
        ! ---------------------------

        ! ---------------------------
        ! rcv of buffer
        DO P=1,NSPMD
          RCV_SIZE = ADRESS_RCV(P+1)-ADRESS_RCV(P)
          IF(P/=LOC_PROC.AND.RCV_SIZE>0) THEN
            MSGTYP = MSGOFF           
            CALL MPI_IRECV( BUFFER_RCV(ADRESS_RCV(P)),RCV_SIZE,REAL,
     .                      IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQUEST_RCV(P),ERROR_MPI )
          ENDIF
        ENDDO
        ! ---------------------------

        ! ---------------------------
        ! initialize the buffer (send)
        IJK = 0
        ADRESS_INTER(1:NINTER) = 0
        DO P=1,NSPMD
          IF(P/=LOC_PROC) THEN
            DO I=1,NUMBER_INTER18
              NIN = INTER18_LIST(I)
              DO J =1,NSNSI(NIN)%P(P)
                  N = NSVSI(NIN)%P(ADRESS_INTER(NIN)+J)
                  NODE_ID = INTBUF_TAB(NIN)%NSV(N)
                  DL = ZERO
                  IF(.NOT.MULTI_FVM%IS_USED) THEN
                    IAD1 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODE_ID)                         
                    IAD2 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODE_ID + 1) - 1
                    DO K=IAD1,IAD2
                      ELEM_ID = ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(K)   
                      DL=MAX(DL, XCELL(1,ELEM_ID))   
                    ENDDO
                  ELSE
                    DL=XCELL(1,NODE_ID) 
                  ENDIF    
                  IJK = IJK + 1         
                  BUFFER_SEND(IJK) = DL   
              ENDDO
              ADRESS_INTER(NIN) = ADRESS_INTER(NIN) + NSNSI(NIN)%P(P)
            ENDDO
          ENDIF
        ENDDO
        ! ---------------------------

        ! ---------------------------
        ! send the buffer
        DO P=1,NSPMD
          SEND_SIZE = ADRESS_SEND(P+1)-ADRESS_SEND(P)
          IF(P/=LOC_PROC.AND.SEND_SIZE>0) THEN
            MSGTYP = MSGOFF           
            CALL MPI_ISEND( BUFFER_SEND(ADRESS_SEND(P)),SEND_SIZE,REAL,
     .                      IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQUEST_SEND(P),ERROR_MPI )
          ENDIF
        ENDDO
        ! ---------------------------

        ! ---------------------------
        ! wait the rcv comm 
        ! and save the remote data in the remote XCELL array
        ADRESS_INTER(1:NINTER) = 0
        DO P=1,NSPMD
          RCV_SIZE = ADRESS_RCV(P+1)-ADRESS_RCV(P)
          IF(P/=LOC_PROC.AND.RCV_SIZE>0) THEN
            LOCAL_ADRESS = 0
            MSGTYP = MSGOFF 
            CALL MPI_WAIT(REQUEST_RCV(P),STATUS_MPI,ERROR_MPI)
            DO I=1,NUMBER_INTER18
              NIN = INTER18_LIST(I)
              NUMBER_REMOTE_NODE = NSNFI(NIN)%P(P)
              IF(NUMBER_REMOTE_NODE>0) THEN
                DO J =1,NUMBER_REMOTE_NODE
                  XCELL_REMOTE(NIN)%MY_REAL_ARRAY_1D(ADRESS_INTER(NIN)+J) = BUFFER_RCV(LOCAL_ADRESS+ADRESS_RCV(P)+J)
                ENDDO
                ADRESS_INTER(NIN) = ADRESS_INTER(NIN) + NUMBER_REMOTE_NODE
                LOCAL_ADRESS = LOCAL_ADRESS + NUMBER_REMOTE_NODE
              ENDIF  
            ENDDO        
          ENDIF
        ENDDO
        ! ---------------------------

        ! ---------------------------
        ! wait the send comm 
        DO P=1,NSPMD
          SEND_SIZE = ADRESS_SEND(P+1)-ADRESS_SEND(P)
          IF(P/=LOC_PROC.AND.SEND_SIZE>0) THEN
            CALL MPI_WAIT(REQUEST_SEND(P),STATUS_MPI,ERROR_MPI)
          ENDIF
        ENDDO
        ! ---------------------------

        DEALLOCATE( BUFFER_SEND )
        DEALLOCATE( BUFFER_RCV )

!   --------------------------------------------------------------------
#endif
        RETURN
        END SUBROUTINE SPMD_EXCH_INTER_18
