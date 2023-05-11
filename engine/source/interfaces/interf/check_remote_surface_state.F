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
Chd|  CHECK_REMOTE_SURFACE_STATE    source/interfaces/interf/check_remote_surface_state.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ALLOC_2D_ARRAY                ../common_source/modules/array_mod.F
Chd|        DEALLOC_2D_ARRAY              ../common_source/modules/array_mod.F
Chd|        SURFACE_DEACTIVATION          source/interfaces/interf/surface_deactivation.F
Chd|        ARRAY_MOD                     ../common_source/modules/array_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE CHECK_REMOTE_SURFACE_STATE( ITASK,SURFARCE_NB,SURFACE_ID,SHIFT_INTERFACE,INTBUF_TAB,
     .                                  IPARI,IAD_ELEM,SHOOT_STRUCT )
!$COMMENT
!       CHECK_SURFACE_STATE description
!           deactivation of surface from an interface
!       CHECK_SURFACE_STATE organization
!$ENDCOMMENT
        USE INTBUFDEF_MOD
        USE SHOOTING_NODE_MOD
        USE ARRAY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"


#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK ! omp thread ID
        INTEGER, INTENT(in) :: SURFARCE_NB  ! number of local deactivated surface
        INTEGER, DIMENSION(SURFARCE_NB), INTENT(in) :: SURFACE_ID  ! id of surface that need to be deactivated
        INTEGER, DIMENSION(NINTER+1,2), INTENT(in) :: SHIFT_INTERFACE ! interface shift
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data 
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM ! index for frontier elements
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: DEACTIVATION
        INTEGER :: I,K,J,IJK,FIRST,LAST
        INTEGER :: NIN,ID_INTER,NUMBER_INTER,NRTM ! related to the surface : interface id, number of surface...
        INTEGER :: ITY,IDEL
        INTEGER :: N1,N2,N3,N4
        INTEGER :: NODE_ID
        INTEGER :: SHIFT
        INTEGER :: DICHOTOMIC_SEARCH_I_ASC  ! function
        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: S_BUFFER
        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: R_BUFFER
        
        INTEGER :: GLOCAL_SURFACE_ID ! global surface id
        INTEGER :: PROC_ID,REMOTE_PROC ! processor id and remote processor id
        INTEGER :: NB_PROC ! number of processor
        INTEGER :: FRONTIER_ELM ! number of frontier elements between 2 processors
        INTEGER, DIMENSION(NSPMD) :: NUMBER_REMOTE_SURF,NUMBER_REMOTE_SURF_R ! number of remote surface per proc
        LOGICAL, DIMENSION(NSPMD) :: ALREADY_DONE ! boolean to avoid to send 2 times the same surface
        
        INTEGER :: IERROR ! error for mpi commm
        INTEGER :: MSGTYP,MSGOFF1,MSGOFF2 ! mpi message id
        INTEGER :: RECV_NB,RECV_NB_2 ! number of received message
        INTEGER :: SIZE_R,SIZE_S ! size of mpi message
        INTEGER, DIMENSION(NSPMD) :: INDEX_R_PROC,INDEX_R_PROC_2 ! index of processor for rcv comm
        INTEGER, DIMENSION(NSPMD) :: REQUEST_SIZE_R,REQUEST_SIZE_R_2 ! array of request : rcv
        INTEGER, DIMENSION(NSPMD) :: REQUEST_SIZE_S,REQUEST_SIZE_S_2 ! array of request : send
#ifdef MPI
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS_MPI
        INTEGER, DIMENSION(MPI_STATUS_SIZE,NSPMD) :: ARRAY_STATUSES
#endif
        DATA MSGOFF1/13014/
        DATA MSGOFF2/13015/
C-----------------------------------------------
        FIRST = 1
        LAST = SURFARCE_NB 
        NUMBER_INTER = SHIFT_INTERFACE(NINTER+1,2)

        ALLOCATE( S_BUFFER(NSPMD), R_BUFFER(NSPMD) )
        S_BUFFER(1:NSPMD)%SIZE_INT_ARRAY_2D(1) = 2
        S_BUFFER(1:NSPMD)%SIZE_INT_ARRAY_2D(2) = SURFARCE_NB
        NUMBER_REMOTE_SURF(1:NSPMD) = 0

        ! --------------------------
        ! loop over the deactivated surface
        DO I=FIRST,LAST
            K = SURFACE_ID(I)  ! get the global surface id
            ID_INTER = DICHOTOMIC_SEARCH_I_ASC(K, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
            NIN = SHIFT_INTERFACE(ID_INTER,2)
            K = K - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface
            ITY = IPARI(7,NIN)
            IDEL = IPARI(17,NIN)
            NRTM = IPARI(4,NIN)
            ! *----*----*----*   1/2/3 surfaces need to deactivate the neighbouring deleted surface 
            ! | 1  |    |    |   the deleted surface must be deactivate 
            ! |    | 4  |    |   not sure about 4 & 5
            ! *----*----*----*
            ! |dele| 3  |    |
            ! |ted |    |    |
            ! *----*----*----*
            ! |    |  5 |    |
            ! | 2  |    |    |
            ! *----*----*----*
            
            IF(ITY==25) THEN 
                GLOCAL_SURFACE_ID = K
            ELSEIF(ITY==24) THEN
                GLOCAL_SURFACE_ID = INTBUF_TAB(NIN)%MSEGLO(K)
            ENDIF
            IF(ITY==24.OR.ITY==25) THEN
                CALL SURFACE_DEACTIVATION(ITY,NRTM,GLOCAL_SURFACE_ID,INTBUF_TAB(NIN)%MSEGLO,INTBUF_TAB(NIN)%MVOISIN)
            ENDIF

            IF(NSPMD>1) THEN            
            ! --------------
            ALREADY_DONE(1:NSPMD) = .FALSE.
            ALREADY_DONE(ISPMD+1) = .TRUE.
            DO J=1,4
                NODE_ID = INTBUF_TAB(NIN)%IRECTM((K-1)*4+J)
                NB_PROC = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID) ! get the number of processor of the node     
                IF(NB_PROC>1) THEN
                    SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID)
                    DO IJK=1,NB_PROC
                        REMOTE_PROC = SHOOT_STRUCT%M_NODE_PROC( SHIFT+IJK )
                        IF(.NOT.ALREADY_DONE(REMOTE_PROC) ) THEN
                            ALREADY_DONE(REMOTE_PROC) = .TRUE.
                            NUMBER_REMOTE_SURF(REMOTE_PROC) = NUMBER_REMOTE_SURF(REMOTE_PROC) + 1
                            IF(.NOT.ALLOCATED( S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D ) ) THEN
                                CALL ALLOC_2D_ARRAY(S_BUFFER(REMOTE_PROC))
                            ENDIF
                            IF(ITY==24) THEN
                                S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(1,NUMBER_REMOTE_SURF(REMOTE_PROC)) = INTBUF_TAB(NIN)%MSEGLO(K)
                            ELSEIF(ITY==25) THEN
                                S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(1,NUMBER_REMOTE_SURF(REMOTE_PROC)) = -INTBUF_TAB(NIN)%MSEGLO(K)
                            ENDIF
                            S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(2,NUMBER_REMOTE_SURF(REMOTE_PROC)) = NIN
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO
            ! --------------
            ENDIF
        ENDDO
        ! --------------------------

        IF(NSPMD>1) THEN
#ifdef MPI

        ! ----------------
        ! receive the data : "number of deleted surface of interface type 24 or 25"
        RECV_NB = 0        
        DO I=1,NSPMD
            FRONTIER_ELM = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
            IF(FRONTIER_ELM>0) THEN
                RECV_NB = RECV_NB + 1
                INDEX_R_PROC(RECV_NB) = I
                MSGTYP = MSGOFF1
                CALL MPI_IRECV( NUMBER_REMOTE_SURF_R(I),1,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQUEST_SIZE_R(RECV_NB),IERROR )
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        ! send the data : "number of deleted surface of interface type 24 or 25"
        DO I=1,NSPMD
            FRONTIER_ELM = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
            IF(FRONTIER_ELM>0) THEN
                MSGTYP = MSGOFF1
                CALL MPI_ISEND( NUMBER_REMOTE_SURF(I),1,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQUEST_SIZE_S(I),IERROR )
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        ! wait the R comm "number of deleted surface of interface type 24 or 25"
        IF(RECV_NB>0) CALL MPI_WAITALL(RECV_NB,REQUEST_SIZE_R,ARRAY_STATUSES,IERROR)

        ! ----------------
        ! receive the data : "list of deleted surface of interface type 24 or 25"
        RECV_NB_2 = 0
        DO I=1,RECV_NB
            PROC_ID = INDEX_R_PROC(I)
            IF(NUMBER_REMOTE_SURF_R(PROC_ID)>0) THEN
                RECV_NB_2 = RECV_NB_2 + 1
                INDEX_R_PROC_2(RECV_NB_2) = PROC_ID
                R_BUFFER(PROC_ID)%SIZE_INT_ARRAY_2D(1) = 2
                R_BUFFER(PROC_ID)%SIZE_INT_ARRAY_2D(2) = NUMBER_REMOTE_SURF_R(PROC_ID)
                CALL ALLOC_2D_ARRAY(R_BUFFER(PROC_ID)) 
                SIZE_R = R_BUFFER(PROC_ID)%SIZE_INT_ARRAY_2D(1) * R_BUFFER(PROC_ID)%SIZE_INT_ARRAY_2D(2)
                MSGTYP = MSGOFF2
                CALL MPI_IRECV(R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,1),SIZE_R,
     .                  MPI_INTEGER,IT_SPMD(PROC_ID),MSGTYP,
     .                  MPI_COMM_WORLD,REQUEST_SIZE_R_2(RECV_NB_2),IERROR )
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        ! send the data : "list of deleted surface of interface type 24 or 25"
        DO I=1,NSPMD
            IF(NUMBER_REMOTE_SURF(I)>0) THEN
                MSGTYP = MSGOFF2
                SIZE_S = NUMBER_REMOTE_SURF(I) * S_BUFFER(I)%SIZE_INT_ARRAY_2D(1)
                CALL MPI_ISEND( S_BUFFER(I)%INT_ARRAY_2D(1,1),SIZE_S,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQUEST_SIZE_S_2(I),IERROR )
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        DO I=1,RECV_NB_2
            CALL MPI_WAITANY(RECV_NB_2,REQUEST_SIZE_R_2,K,STATUS_MPI,IERROR)
            PROC_ID = INDEX_R_PROC_2(K)
            ! --------------
            DO J=1,NUMBER_REMOTE_SURF_R(PROC_ID)
                NIN = R_BUFFER(PROC_ID)%INT_ARRAY_2D(2,J) ! get the interface id
                ITY = IPARI(7,NIN)  ! get the type of interface
                IDEL = IPARI(17,NIN) ! get the kind of idel (1 or 2)
                NRTM = IPARI(4,NIN)     ! get the number of surfaces of the interface NIN
                ! --------------
                GLOCAL_SURFACE_ID = R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,J) ! get the global deleted surface id
                IF(ITY==24.OR.ITY==25) THEN
                    CALL SURFACE_DEACTIVATION(ITY,NRTM,GLOCAL_SURFACE_ID,INTBUF_TAB(NIN)%MSEGLO,INTBUF_TAB(NIN)%MVOISIN) 
                ENDIF
                ! --------------
            ENDDO
            CALL DEALLOC_2D_ARRAY(R_BUFFER(PROC_ID)) 
            ! --------------
        ENDDO
        ! ----------------

        ! ----------------
        ! wait the S comm : "number of deleted surface of interface type 24 or 25"
        DO I=1,NSPMD
            FRONTIER_ELM = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
            IF(FRONTIER_ELM>0) THEN
                CALL MPI_WAIT(REQUEST_SIZE_S(I),STATUS_MPI,IERROR)
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        ! wait the S comm : "list of deleted surface of interface type 24 or 25"
        DO I=1,NSPMD
            IF(NUMBER_REMOTE_SURF(I)>0) THEN
                CALL MPI_WAIT(REQUEST_SIZE_S_2(I),STATUS_MPI,IERROR)
                CALL DEALLOC_2D_ARRAY(S_BUFFER(I)) 
            ENDIF
        ENDDO
        ! ----------------
#endif
        ENDIF

        DEALLOCATE( S_BUFFER, R_BUFFER )

        ! --------------------------
        RETURN
        END SUBROUTINE CHECK_REMOTE_SURFACE_STATE
