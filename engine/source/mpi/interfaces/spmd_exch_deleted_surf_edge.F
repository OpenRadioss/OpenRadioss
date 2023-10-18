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
Chd|  SPMD_EXCH_DELETED_SURF_EDGE   source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ALLOC_1D_ARRAY                ../common_source/modules/array_mod.F
Chd|        DEALLOC_1D_ARRAY              ../common_source/modules/array_mod.F
Chd|        FIND_EDGE_FROM_REMOTE_PROC    source/interfaces/interf/find_edge_from_remote_proc.F
Chd|        FIND_SURFACE_FROM_REMOTE_PROC source/interfaces/interf/find_surface_from_remote_proc.F
Chd|        ARRAY_MOD                     ../common_source/modules/array_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE SPMD_EXCH_DELETED_SURF_EDGE( IAD_ELEM  ,ITABM1  ,SHOOT_STRUCT  ,INTBUF_TAB,  NEWFRONT  ,
     .                                          IPARI     ,GEO     ,IXS           ,IXC       ,  IXT       ,
     .                                          IXP       ,IXR     ,IXTG          ,ADDCNEL   ,  CNEL      ,
     .                                          TAG_NODE   ,TAG_ELEM,INTERFACES   ,CHECK_NEIGH_FLAG)
!$COMMENT
!       SPMD_EXCH_DELETED_SURF_EDGE description
!           exchange of edge/surface that need to be deactivated
!       SPMD_EXCH_DELETED_SURF_EDGE organization 
!           step 1 : exchange the number of edge and surface
!           step 2 : allocation of buffer
!           step 3 : exchange the list of edge and surface
!           step 4 : deactivate the edge/surface
!$ENDCOMMENT
        USE ARRAY_MOD
        USE SHOOTING_NODE_MOD
        USE INTBUFDEF_MOD 
        USE INTERFACES_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITABM1    ! array of global id
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo   
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data 
         INTEGER, DIMENSION(NINTER), INTENT(inout) :: NEWFRONT   ! array for sorting : 1 --> need to sort the interface NIN
        INTEGER, DIMENSION(NIXS,NUMELS),TARGET, INTENT(in) :: IXS   ! solid array
        INTEGER, DIMENSION(NIXC,NUMELC),TARGET, INTENT(in) :: IXC   ! shell array
        INTEGER, DIMENSION(NIXT,NUMELT),TARGET, INTENT(in) :: IXT! truss array
        INTEGER, DIMENSION(NIXP,NUMELP),TARGET, INTENT(in) :: IXP! beam array
        INTEGER, DIMENSION(NIXR,NUMELR),TARGET, INTENT(in) :: IXR! spring array
        INTEGER, DIMENSION(NIXTG,NUMELTG),TARGET, INTENT(in) :: IXTG! triangle array
        INTEGER, DIMENSION(0:NUMNOD+1), INTENT(in) :: ADDCNEL ! address for the CNEL array
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI
        my_real, DIMENSION(NPROPG,NUMGEO), INTENT(in) :: GEO
        INTEGER, DIMENSION(0:LCNEL), INTENT(in) :: CNEL ! connectivity node-->element
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: TAG_NODE
        INTEGER, DIMENSION(NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG), INTENT(inout) :: TAG_ELEM
        TYPE (INTERFACES_) ,INTENT(IN):: INTERFACES ! interfaces structure
        INTEGER, INTENT(inout) :: CHECK_NEIGH_FLAG ! Flag for detecting if internal is activated
#ifdef MPI
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K
        INTEGER :: MSGTYP,MSGOFF1,MSGOFF2
        INTEGER :: PROC_ID,SIZE_BUFFER_R
        INTEGER :: RECV_NB,RECV_SURF_NB
        INTEGER, DIMENSION(2,NSPMD) :: SURF_PER_PROC,REMOTE_SURF_PER_PROC,REMOTE_SURF_PER_PROC_2
        INTEGER, DIMENSION(NSPMD) :: INDEX_PROC,INDEX_BUFFER_R,INDEX_R_PROC,INDEX_R_PROC_2,INDEX_BUFFER_R_2
        INTEGER, DIMENSION(NSPMD) :: REQUEST_SIZE_R,REQUEST_SIZE_S
        INTEGER, DIMENSION(NSPMD) :: REQUEST_SURF_R,REQUEST_SURF_S

        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS_MPI
        INTEGER, DIMENSION(MPI_STATUS_SIZE,NSPMD) :: ARRAY_STATUSES
        INTEGER :: IERROR,FRONTIER_ELM,NB_SURFACE,ADDRESS,NB_EDGE

        INTEGER, DIMENSION(:), ALLOCATABLE :: BUFFER_R
        TYPE(array_type), DIMENSION(NSPMD) :: BUFFER_S

        DATA MSGOFF1/13010/
        DATA MSGOFF2/13011/
!       ----------------------------------------


        SURF_PER_PROC(1:2,1:NSPMD) = 0
        REMOTE_SURF_PER_PROC(1:2,1:NSPMD) = 0

        ! ----------------    
        ! count the number of surface (ie. 4 nodes) per processor
        DO I=1,SHOOT_STRUCT%SAVE_PROC_NB,5
            PROC_ID = SHOOT_STRUCT%SAVE_PROC(I)
            SURF_PER_PROC(1,PROC_ID) = SURF_PER_PROC(1,PROC_ID) + 1
        ENDDO
        ! ----------------    
        ! count the number of edge (ie. 2 nodes) per processor
        DO I=1,SHOOT_STRUCT%SAVE_PROC_NB_EDGE,3
            PROC_ID = SHOOT_STRUCT%SAVE_PROC_EDGE(I)
            SURF_PER_PROC(2,PROC_ID) = SURF_PER_PROC(2,PROC_ID) + 1
        ENDDO
        ! ----------------  
        ! allocate the S buffer
        INDEX_PROC(1:NSPMD) = 0
        DO I=1,NSPMD
            BUFFER_S(I)%SIZE_INT_ARRAY_1D = 4*SURF_PER_PROC(1,I) + 
     .          2 * SURF_PER_PROC(2,I)
            CALL ALLOC_1D_ARRAY(BUFFER_S(I))
        ENDDO
        ! ----------------
        ! initialize the S buffer
        ! surface initialization
        DO I=1,SHOOT_STRUCT%SAVE_PROC_NB,5
            PROC_ID = SHOOT_STRUCT%SAVE_PROC(I)
            DO J=1,4
                INDEX_PROC(PROC_ID) = INDEX_PROC(PROC_ID) + 1
                BUFFER_S(PROC_ID)%INT_ARRAY_1D( INDEX_PROC(PROC_ID) ) = SHOOT_STRUCT%SAVE_PROC(I+J)
            ENDDO
        ENDDO

        ! main edge initialization
        DO I=1,SHOOT_STRUCT%SAVE_PROC_NB_EDGE,3
            PROC_ID = SHOOT_STRUCT%SAVE_PROC_EDGE(I)
            DO J=1,2
                INDEX_PROC(PROC_ID) = INDEX_PROC(PROC_ID) + 1
                BUFFER_S(PROC_ID)%INT_ARRAY_1D( INDEX_PROC(PROC_ID) ) = SHOOT_STRUCT%SAVE_PROC_EDGE(I+J)
            ENDDO
        ENDDO
        ! ----------------

        ! ----------------
        ! receive the data : "number of 4 nodes defining a surface + number of 2 nodes def. an edge"
        RECV_NB = 0        
        DO I=1,NSPMD
            FRONTIER_ELM = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
            IF(FRONTIER_ELM>0) THEN
                RECV_NB = RECV_NB + 1
                INDEX_R_PROC(RECV_NB) = I
                MSGTYP = MSGOFF1
                CALL MPI_IRECV( REMOTE_SURF_PER_PROC(1,RECV_NB),2,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQUEST_SIZE_R(RECV_NB),IERROR )
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        ! send the data : "number of 4 nodes defining a surface + number of 2 nodes def. an edge"
        DO I=1,NSPMD
            FRONTIER_ELM = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
            IF(FRONTIER_ELM>0) THEN
                MSGTYP = MSGOFF1
                CALL MPI_ISEND( SURF_PER_PROC(1,I),2,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQUEST_SIZE_S(I),IERROR )
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        ! wait the R comm "number of 4 nodes defining a surface + number of 2 nodes def. an edge"
        IF(RECV_NB>0) CALL MPI_WAITALL(RECV_NB,REQUEST_SIZE_R,ARRAY_STATUSES,IERROR)
        ! allocation of R buffer "list of 4 nodes defining a surface"
        SIZE_BUFFER_R = 0
        INDEX_BUFFER_R(1:NSPMD) = 0
        INDEX_BUFFER_R(1) = 1
        DO I=1,RECV_NB
            IF(I>1) INDEX_BUFFER_R(I) = INDEX_BUFFER_R(I-1) + 4*REMOTE_SURF_PER_PROC(1,I-1) + 
     .           2 * REMOTE_SURF_PER_PROC(2,I-1)
            SIZE_BUFFER_R = SIZE_BUFFER_R + 4*REMOTE_SURF_PER_PROC(1,I) + 2*REMOTE_SURF_PER_PROC(2,I)
        ENDDO
        ALLOCATE( BUFFER_R( SIZE_BUFFER_R ) )
        ! ----------------

        ! ----------------
        ! receive the data : "list of 4 nodes defining a surface + 2 nodes def. an edge"
        RECV_SURF_NB = 0
        INDEX_BUFFER_R_2(1:NSPMD) = 0
        REMOTE_SURF_PER_PROC_2(1:2,1:NSPMD) = 0
        INDEX_R_PROC_2(1:NSPMD) = 0
        DO I=1,RECV_NB
            IF(REMOTE_SURF_PER_PROC(1,I)+REMOTE_SURF_PER_PROC(2,I)>0) THEN
                PROC_ID = INDEX_R_PROC(I)
                MSGTYP = MSGOFF2
                RECV_SURF_NB = RECV_SURF_NB + 1
                INDEX_R_PROC_2(RECV_SURF_NB) = INDEX_R_PROC(I)
                INDEX_BUFFER_R_2(RECV_SURF_NB) = INDEX_BUFFER_R(I)
                REMOTE_SURF_PER_PROC_2(1:2,RECV_SURF_NB) = REMOTE_SURF_PER_PROC(1:2,I)
                CALL MPI_IRECV( BUFFER_R(INDEX_BUFFER_R(I)),4*REMOTE_SURF_PER_PROC(1,I)+2*REMOTE_SURF_PER_PROC(2,I),
     .          MPI_INTEGER,IT_SPMD(PROC_ID),MSGTYP,
     .          MPI_COMM_WORLD,REQUEST_SURF_R(RECV_SURF_NB),IERROR )
            ENDIF
        ENDDO
        ! ----------------              


        ! ----------------
        ! send the data : "list of 4 nodes defining a surface + 2 nodes def. an edge"
        DO I=1,NSPMD
            IF(SURF_PER_PROC(1,I)+SURF_PER_PROC(2,I)>0) THEN
                MSGTYP = MSGOFF2
                CALL MPI_ISEND( BUFFER_S(I)%INT_ARRAY_1D,INDEX_PROC(I),MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQUEST_SURF_S(I),IERROR )
            ENDIF
        ENDDO 
        ! ----------------

        ! ----------------
        DO I=1,RECV_SURF_NB
            CALL MPI_WAITANY(RECV_SURF_NB,REQUEST_SURF_R,K,STATUS_MPI,IERROR)
            PROC_ID = INDEX_R_PROC_2(K)
            NB_SURFACE = REMOTE_SURF_PER_PROC_2(1,K)
            ADDRESS = INDEX_BUFFER_R_2(K)

            CALL FIND_SURFACE_FROM_REMOTE_PROC(SHOOT_STRUCT ,NB_SURFACE ,BUFFER_R(ADDRESS) ,INTBUF_TAB ,ITABM1 ,
     .                                         IPARI        ,GEO        ,IXS               ,IXC        ,IXT    ,
     .                                         IXP          ,IXR        ,IXTG              ,ADDCNEL    ,CNEL   ,
     .                                         TAG_NODE     ,TAG_ELEM   ,INTERFACES        ,CHECK_NEIGH_FLAG)
            NB_EDGE = REMOTE_SURF_PER_PROC_2(2,K)
            ADDRESS = INDEX_BUFFER_R_2(K)+4*NB_SURFACE  
            CALL FIND_EDGE_FROM_REMOTE_PROC( SHOOT_STRUCT,NB_EDGE,BUFFER_R(ADDRESS),INTBUF_TAB,ITABM1,
     .                                       NEWFRONT,IPARI,GEO,
     .                                       IXS,IXC,IXT,IXP,IXR,IXTG,
     .                                       ADDCNEL,CNEL,TAG_NODE,TAG_ELEM )
        ENDDO
        ! ----------------

        ! ----------------
        DO I=1,NSPMD
            FRONTIER_ELM = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
            IF(FRONTIER_ELM>0) THEN
                CALL MPI_WAIT(REQUEST_SIZE_S(I),STATUS_MPI,IERROR)
            ENDIF
        ENDDO

        DO I=1,NSPMD
            IF(SURF_PER_PROC(1,I)+SURF_PER_PROC(2,I)>0) THEN
                CALL MPI_WAIT(REQUEST_SURF_S(I),STATUS_MPI,IERROR)
            ENDIF
        ENDDO
        ! ----------------

        ! ----------------
        DO I=1,NSPMD
            CALL DEALLOC_1D_ARRAY(BUFFER_S(I))
        ENDDO
        ! ----------------
#endif
        RETURN
        END SUBROUTINE SPMD_EXCH_DELETED_SURF_EDGE


!       ----------------------------------------
