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
Chd|  INIT_NODAL_STATE              source/interfaces/interf/init_nodal_state.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ALLOC_1D_ARRAY                share/modules/array_mod.F     
Chd|        COUNT_NB_ELEM_EDGE            source/interfaces/interf/count_nb_elem_edge.F
Chd|        COUNT_REMOTE_NB_ELEM_EDGE     source/interfaces/interf/count_remote_nb_elem_edge.F
Chd|        DEALLOC_1D_ARRAY              share/modules/array_mod.F     
Chd|        MYQSORT_INT                   ../common_source/tools/sort/myqsort_int.F
Chd|        ARRAY_MOD                     share/modules/array_mod.F     
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE INIT_NODAL_STATE( IPARI,SHOOT_STRUCT,INTBUF_TAB,IAD_ELEM,FR_ELEM,
     .                               ITAB,ITABM1,GEO,ADDCNEL,CNEL,
     .                               IXS,IXC,IXT,IXP,IXR,IXTG,
     .                               SIZE_ADDCNEL,SIZE_CNEL )
!$COMMENT
!       INIT_NODAL_STATE description
!           initialization of structures for the new shooting node algo    
!           this algo is available only for /INTER/TYPE7 & 11 & 10 & 22       
!       INIT_NODAL_STATE organization
!           for each secondary node, save :
!           - the number of processor where the node is defined
!           - the list of processor where the node is defined
!           - the number of interface where the node is defined
!           - the list of interface where the node is defined
!
!           for each main node, save :
!           - the number of processor where the node is defined
!           - the list of processor where the node is defined
!           - the number of surface where the node is defined
!           - the list of surface where the node is defined
!$ENDCOMMENT

        USE INTBUFDEF_MOD  
        USE SHOOTING_NODE_MOD
        USE ARRAY_MOD
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
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: SIZE_ADDCNEL,SIZE_CNEL ! array size : cnel & addcnel
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM  ! adress for frontier node
        INTEGER, DIMENSION(SFR_ELEM), INTENT(in) :: FR_ELEM    ! frontier node id 
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB ! array to convert local id to global id
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITABM1    ! array of global id
        my_real, DIMENSION(NPROPG,NUMGEO), INTENT(in) :: GEO
        INTEGER, DIMENSION(0:SIZE_ADDCNEL), INTENT(in) :: ADDCNEL ! address for the CNEL array
        INTEGER, DIMENSION(0:SIZE_CNEL), INTENT(in) :: CNEL ! connectivity node --> element
        INTEGER, DIMENSION(NIXS,NUMELS),TARGET, INTENT(in) :: IXS   ! solid array
        INTEGER, DIMENSION(NIXC,NUMELC),TARGET, INTENT(in) :: IXC   ! shell array
        INTEGER, DIMENSION(NIXT,NUMELT),TARGET, INTENT(in) :: IXT! truss array
        INTEGER, DIMENSION(NIXP,NUMELP),TARGET, INTENT(in) :: IXP! beam array
        INTEGER, DIMENSION(NIXR,NUMELR),TARGET, INTENT(in) :: IXR! spring array
        INTEGER, DIMENSION(NIXTG,NUMELTG),TARGET, INTENT(in) :: IXTG! triangle array
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NIN,ITY,NSN,NMN,NRTM,NRTS,IDEL,IDELKEEP
        INTEGER :: I,J,K
        INTEGER :: NODE_ID,SHIFT,SHIFT_INTER,NEXT_INTER
        INTEGER :: TMP_,MY_ERROR,NB_PROC,NB_NODE_SURF,NB_SURF
        INTEGER :: NB_EDGE,NB_EDGE_2
        INTEGER :: N1,N2,N3,N4
        INTEGER, DIMENSION(:), ALLOCATABLE :: WORK_ARRAY,WORK_ARRAY_2,WORK_ARRAY_3
        INTEGER, DIMENSION(:), ALLOCATABLE :: SORT_ARRAY,PERM

        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: BUFFER_SECOND,BUFFER_MAIN
        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: R_BUFFER_SECOND,R_BUFFER_MAIN
#ifdef MPI
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATU
        INTEGER :: MSGTYP,MSGOFF1,IERROR
        DATA MSGOFF1/13013/
        INTEGER, DIMENSION(NSPMD) :: REQUEST_S,REQUEST_R
        INTEGER, DIMENSION(NSPMD) :: REQUEST_S2,REQUEST_R2
        INTEGER, DIMENSION(NSPMD) :: REQUEST_S3,REQUEST_R3
#endif
        INTEGER :: SIZ,OLD_SIZE
        INTEGER :: PROC_ID
        INTEGER :: NB_PROC_1,NB_PROC_2,NB_RESULT_INTERSECT
        INTEGER, DIMENSION(:), ALLOCATABLE :: INTERSECT_1,INTERSECT_2,RESULT_INTERSECT
        INTEGER, DIMENSION(2,NSPMD) :: S_BUFFER_2_INT,R_BUFFER_2_INT
        INTEGER, DIMENSION(NSPMD) :: SIZE_BUFFER_MAIN,SIZE_BUFFER_SECOND
        INTEGER, DIMENSION(NSPMD) :: R_SIZE_BUFFER_MAIN,R_SIZE_BUFFER_SECOND
C-----------------------------------------------

        ALLOCATE( BUFFER_SECOND(NSPMD) )
        ALLOCATE( BUFFER_MAIN(NSPMD) )
        ALLOCATE( R_BUFFER_SECOND(NSPMD) )
        ALLOCATE( R_BUFFER_MAIN(NSPMD) )

        BUFFER_SECOND(1:NSPMD)%SIZE_INT_ARRAY_1D = 0
        BUFFER_MAIN(1:NSPMD)%SIZE_INT_ARRAY_1D = 0
        R_BUFFER_SECOND(1:NSPMD)%SIZE_INT_ARRAY_1D = 0
        R_BUFFER_MAIN(1:NSPMD)%SIZE_INT_ARRAY_1D = 0
        ! --------------------------------
        ! SECONDARY NODES
        ! --------------------------------

        !   SHIFT_S_NODE(i) = index to INTER_SEC_NODE/SEC_NODE_ID for node_id = i
        !   SHIFT_S_NODE(i+1) - SHIFT_S_NODE(i) = number of interface per node
        ALLOCATE( SHOOT_STRUCT%SHIFT_S_NODE(NUMNOD+1) )
        SHOOT_STRUCT%SHIFT_S_NODE(1:NUMNOD+1) = 0
        DO NIN=1,NINTER
            ITY = IPARI(7,NIN)  ! interface id
            NSN = IPARI(5,NIN)  ! number of secondary nodes
            IDEL = IPARI(17,NIN)! idel option
            IDELKEEP = IPARI(61,NIN)
            IF((ITY==7.OR.ITY==10.OR.ITY==22).AND.IDEL>=1.AND.IDELKEEP/=1) THEN
                ! loop over the S nodes
                DO I=1,NSN
                    NODE_ID = INTBUF_TAB(NIN)%NSV(I)
                    ! need to check the node_id value : for typ24 + edge to edge,
                    ! a famous developer introduced NODE_ID greater than the number of node NUMNOD :(
                    IF(NODE_ID<=NUMNOD) SHOOT_STRUCT%SHIFT_S_NODE(NODE_ID+1) = SHOOT_STRUCT%SHIFT_S_NODE(NODE_ID+1) + 1
                ENDDO
            ENDIF
        ENDDO

        SHOOT_STRUCT%SIZE_SEC_NODE = 0
        DO I=1,NUMNOD
            SHOOT_STRUCT%SHIFT_S_NODE(I+1) = SHOOT_STRUCT%SHIFT_S_NODE(I+1) + SHOOT_STRUCT%SHIFT_S_NODE(I)
        ENDDO
        ! size of %INTER_SEC_NODE & %SEC_NODE_ID
        SHOOT_STRUCT%SIZE_SEC_NODE = SHOOT_STRUCT%SHIFT_S_NODE(NUMNOD+1)

        ! allocation
        ALLOCATE( SHOOT_STRUCT%INTER_SEC_NODE(SHOOT_STRUCT%SIZE_SEC_NODE) )
        ALLOCATE( SHOOT_STRUCT%SEC_NODE_ID(SHOOT_STRUCT%SIZE_SEC_NODE) )


        ALLOCATE( WORK_ARRAY(NUMNOD) )
        WORK_ARRAY(1:NUMNOD) = 0
        DO NIN=1,NINTER
            ITY = IPARI(7,NIN)
            NSN = IPARI(5,NIN)
            IDEL = IPARI(17,NIN)! idel option
            IDELKEEP = IPARI(61,NIN)
            IF((ITY==7.OR.ITY==10.OR.ITY==22).AND.IDEL>=1.AND.IDELKEEP/=1) THEN
                DO I=1,NSN
                    NODE_ID = INTBUF_TAB(NIN)%NSV(I)    ! node id
                    IF(NODE_ID<=NUMNOD) THEN
                        WORK_ARRAY(NODE_ID) = WORK_ARRAY(NODE_ID) + 1 
                        SHIFT = WORK_ARRAY(NODE_ID) + SHOOT_STRUCT%SHIFT_S_NODE(NODE_ID) ! index for %inter_sec_node
                        SHOOT_STRUCT%INTER_SEC_NODE( SHIFT ) = NIN  ! save the interface
                        SHOOT_STRUCT%SEC_NODE_ID( SHIFT ) = I       ! save the NSV id
                    ENDIF
                ENDDO
            ENDIF
        ENDDO

        DEALLOCATE( WORK_ARRAY )
        ! --------------------------------


        ! --------------------------------
        ! MAIN NODES
        ! --------------------------------
        ALLOCATE( SHOOT_STRUCT%SHIFT_M_NODE_PROC(NUMNOD+1) )
        SHOOT_STRUCT%SHIFT_M_NODE_PROC(2:NUMNOD+1) = 1
        SHOOT_STRUCT%SHIFT_M_NODE_PROC(1) = 0
        ! ---------------------
        ! compute the nodal index (main node)
        ! shift(i) gives the index of the i node
        ! shift(i+1)-shift(i) gives the number of surface of the i node
        DO I=1,NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NODE_ID = FR_ELEM(J)
                SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID+1) = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID+1) + 1
            ENDDO
        ENDDO
        DO I=1,NUMNOD
            SHOOT_STRUCT%SHIFT_M_NODE_PROC(I+1) = SHOOT_STRUCT%SHIFT_M_NODE_PROC(I+1) + SHOOT_STRUCT%SHIFT_M_NODE_PROC(I)
        ENDDO
        ! save the size 
        SHOOT_STRUCT%SIZE_M_NODE_PROC = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NUMNOD+1)
        ! ---------------------

        ! ---------------------
        ! compute the list of processor for each node
        ALLOCATE( SHOOT_STRUCT%M_NODE_PROC( SHOOT_STRUCT%SIZE_M_NODE_PROC ) )
        SHOOT_STRUCT%M_NODE_PROC(1:SHOOT_STRUCT%SIZE_M_NODE_PROC) = -1
        ALLOCATE( WORK_ARRAY(NUMNOD) )
        WORK_ARRAY(1:NUMNOD) = 0

        ! loop over the nodes to save the proc ID        
        DO I=1,NUMNOD
            WORK_ARRAY(I) = WORK_ARRAY(I) + 1
            SHIFT = WORK_ARRAY(I) + SHOOT_STRUCT%SHIFT_M_NODE_PROC(I) ! index for M_NODE_PROC
            SHOOT_STRUCT%M_NODE_PROC( SHIFT ) = ISPMD+1
        ENDDO
        ! loop over the frontier nodes to save the proc ID
        DO I=1,NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NODE_ID = FR_ELEM(J)
                WORK_ARRAY(NODE_ID) = WORK_ARRAY(NODE_ID) + 1
                SHIFT = WORK_ARRAY(NODE_ID) + SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID) ! index for M_NODE_PROC
                SHOOT_STRUCT%M_NODE_PROC( SHIFT ) = I     ! save the processor ID
            ENDDO
        ENDDO
        ! sort the list of processor and compute the max number of processor
        SHOOT_STRUCT%MAX_PROC_NB = 0
        DO I=1,NUMNOD
            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(I) ! index for M_NODE_PROC
            NB_PROC = SHOOT_STRUCT%SHIFT_M_NODE_PROC(I+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(I)
            SHOOT_STRUCT%MAX_PROC_NB = MAX(SHOOT_STRUCT%MAX_PROC_NB,NB_PROC)
            IF(NB_PROC>2) THEN
                ALLOCATE( SORT_ARRAY(NB_PROC),PERM(NB_PROC) )
                SORT_ARRAY(1:NB_PROC) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC )
                CALL MYQSORT_INT(NB_PROC, SORT_ARRAY, PERM, MY_ERROR)
                SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC ) = SORT_ARRAY(1:NB_PROC)
                DEALLOCATE( SORT_ARRAY,PERM )
            ELSEIF(NB_PROC==2) THEN
                IF(SHOOT_STRUCT%M_NODE_PROC(SHIFT+1)>SHOOT_STRUCT%M_NODE_PROC(SHIFT+2)) THEN
                    TMP_ = SHOOT_STRUCT%M_NODE_PROC(SHIFT+2)
                    SHOOT_STRUCT%M_NODE_PROC(SHIFT+2) = SHOOT_STRUCT%M_NODE_PROC(SHIFT+1)
                    SHOOT_STRUCT%M_NODE_PROC(SHIFT+1) = TMP_
                ENDIF
            ENDIF
        ENDDO
        ! ---------------------

        ! ---------------------
        ! compute the index for the surface/edge array
        ALLOCATE( SHOOT_STRUCT%SHIFT_M_NODE_SURF(NUMNOD+1) )
        SHOOT_STRUCT%SHIFT_M_NODE_SURF(1:NUMNOD+1) = 0

        ALLOCATE( SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NUMNOD+1) )
        SHOOT_STRUCT%SHIFT_M_NODE_EDGE(1:NUMNOD+1) = 0

        ALLOCATE( SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NUMNOD+1) )
        SHOOT_STRUCT%SHIFT_S_NODE_EDGE(1:NUMNOD+1) = 0

        DO NIN=1,NINTER
            ITY = IPARI(7,NIN)  ! interface id
            NMN = IPARI(6,NIN)  ! number of main nodes
            NRTM = IPARI(4,NIN) ! number of main surfaces/edges
            NRTS = IPARI(3,NIN) ! number of secondary edges
            IDEL = IPARI(17,NIN)! idel option
            ! -----------------------------
            IF((ITY==7.OR.ITY==10.OR.ITY==22).AND.IDEL>1) THEN
                ! loop over the M surface
                DO I=1,NRTM
                    N1 = INTBUF_TAB(NIN)%IRECTM((I-1)*4+1)
                    N2 = INTBUF_TAB(NIN)%IRECTM((I-1)*4+2)
                    N3 = INTBUF_TAB(NIN)%IRECTM((I-1)*4+3)
                    N4 = INTBUF_TAB(NIN)%IRECTM((I-1)*4+4)
                    SHOOT_STRUCT%SHIFT_M_NODE_SURF(N1+1) = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N1+1) + 1
                    SHOOT_STRUCT%SHIFT_M_NODE_SURF(N2+1) = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N2+1) + 1
                    SHOOT_STRUCT%SHIFT_M_NODE_SURF(N3+1) = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N3+1) + 1
                    IF(N3/=N4) SHOOT_STRUCT%SHIFT_M_NODE_SURF(N4+1) = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N4+1) + 1
                ENDDO
            ! -----------------------------
            ELSEIF(ITY == 11) THEN
                ! loop over the M edge
                IF(IDEL>=1) THEN
                    DO I=1,NRTM
                        N1 = INTBUF_TAB(NIN)%IRECTM((I-1)*2+1)
                        N2 = INTBUF_TAB(NIN)%IRECTM((I-1)*2+2)
                        SHOOT_STRUCT%SHIFT_M_NODE_EDGE(N1+1) = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(N1+1) + 1
                        SHOOT_STRUCT%SHIFT_M_NODE_EDGE(N2+1) = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(N2+1) + 1
                    ENDDO
                ENDIF
                ! loop over the S edge
                IF(IDEL>=1) THEN
                    DO I=1,NRTS
                        N1 = INTBUF_TAB(NIN)%IRECTS((I-1)*2+1)
                        N2 = INTBUF_TAB(NIN)%IRECTS((I-1)*2+2)
                        SHOOT_STRUCT%SHIFT_S_NODE_EDGE(N1+1) = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(N1+1) + 1
                        SHOOT_STRUCT%SHIFT_S_NODE_EDGE(N2+1) = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(N2+1) + 1
                    ENDDO
                ENDIF
            ENDIF
            ! -----------------------------
        ENDDO

        DO I=1,NUMNOD
            SHOOT_STRUCT%SHIFT_M_NODE_SURF(I+1) = SHOOT_STRUCT%SHIFT_M_NODE_SURF(I+1) + SHOOT_STRUCT%SHIFT_M_NODE_SURF(I)
            SHOOT_STRUCT%SHIFT_M_NODE_EDGE(I+1) = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(I+1) + SHOOT_STRUCT%SHIFT_M_NODE_EDGE(I)
            SHOOT_STRUCT%SHIFT_S_NODE_EDGE(I+1) = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(I+1) + SHOOT_STRUCT%SHIFT_S_NODE_EDGE(I)
        ENDDO
        SHOOT_STRUCT%SIZE_M_NODE_SURF = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NUMNOD+1)
        ALLOCATE( SHOOT_STRUCT%M_NODE_SURF( SHOOT_STRUCT%SIZE_M_NODE_SURF) )

        SHOOT_STRUCT%SIZE_M_NODE_EDGE = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NUMNOD+1)
        ALLOCATE( SHOOT_STRUCT%M_NODE_EDGE( SHOOT_STRUCT%SIZE_M_NODE_EDGE) )

        SHOOT_STRUCT%SIZE_S_NODE_EDGE = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NUMNOD+1)
        ALLOCATE( SHOOT_STRUCT%S_NODE_EDGE( SHOOT_STRUCT%SIZE_S_NODE_EDGE) )
        ! ---------------------
        ! compute the max number of surface/edge
        SHOOT_STRUCT%MAX_SURF_NB = 0
        SHOOT_STRUCT%MAX_EDGE_NB = 0
        DO I=1,NUMNOD
            NB_SURF = SHOOT_STRUCT%SHIFT_M_NODE_SURF(I+1) - SHOOT_STRUCT%SHIFT_M_NODE_SURF(I)
            SHOOT_STRUCT%MAX_SURF_NB = MAX(SHOOT_STRUCT%MAX_SURF_NB,NB_SURF)

            NB_EDGE = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(I+1) - SHOOT_STRUCT%SHIFT_M_NODE_EDGE(I)
            NB_EDGE_2 = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(I+1) - SHOOT_STRUCT%SHIFT_S_NODE_EDGE(I)
            NB_EDGE = MAX(NB_EDGE,NB_EDGE_2)
            SHOOT_STRUCT%MAX_EDGE_NB = MAX(SHOOT_STRUCT%MAX_EDGE_NB,NB_EDGE)
        ENDDO
        ! ---------------------
        ! save the surface/edge ID for each node
        WORK_ARRAY(1:NUMNOD) = 0
        ALLOCATE( WORK_ARRAY_2(NUMNOD) )
        WORK_ARRAY_2(1:NUMNOD) = 0
        ALLOCATE( WORK_ARRAY_3(NUMNOD) )
        WORK_ARRAY_3(1:NUMNOD) = 0
        ALLOCATE( SHOOT_STRUCT%SHIFT_INTERFACE(NINTER+1,2) )
        NEXT_INTER = 0
        SHIFT_INTER = 1
        DO NIN=1,NINTER
            ITY = IPARI(7,NIN)  ! interface id
            NMN = IPARI(6,NIN)  ! number of main nodes
            NRTM = IPARI(4,NIN) ! number of main surfaces/edges
            NRTS = IPARI(3,NIN) ! number of secondary edges
            IDEL = IPARI(17,NIN)! idel option
            ! ---------------------------
            IF((ITY==7.OR.ITY==10.OR.ITY==22).AND.IDEL>1) THEN
                ! loop over the M surface
                DO I=1,NRTM              
                    N3 = INTBUF_TAB(NIN)%IRECTM((I-1)*4+3)
                    N4 = INTBUF_TAB(NIN)%IRECTM((I-1)*4+4)
                    NB_NODE_SURF = 4
                    IF(N3==N4) NB_NODE_SURF = 3
                    DO J=1,NB_NODE_SURF
                        NODE_ID = INTBUF_TAB(NIN)%IRECTM((I-1)*4+J)
                        WORK_ARRAY(NODE_ID) = WORK_ARRAY(NODE_ID) + 1
                        SHIFT = WORK_ARRAY(NODE_ID) + SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID) ! index for M_NODE_SURF
                        SHOOT_STRUCT%M_NODE_SURF( SHIFT ) = SHIFT_INTER  - 1 + I     ! save the surface ID
                    ENDDO
                ENDDO
            ! ---------------------------
            ELSEIF(ITY==11) THEN
                ! loop over the M edge
                IF(IDEL>=1) THEN
                    DO I=1,NRTM
                        DO J=1,2              
                            NODE_ID = INTBUF_TAB(NIN)%IRECTM((I-1)*2+J)
                            WORK_ARRAY_2(NODE_ID) = WORK_ARRAY_2(NODE_ID) + 1
                            SHIFT = WORK_ARRAY_2(NODE_ID) + SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID) ! index for M_NODE_EDGE
                            SHOOT_STRUCT%M_NODE_EDGE( SHIFT ) = SHIFT_INTER  - 1 + I     ! save the edge ID
                        ENDDO
                    ENDDO
                ENDIF
                ! loop over the S edge
                IF(IDEL>=1) THEN
                    DO I=1,NRTS
                        DO J=1,2              
                            NODE_ID = INTBUF_TAB(NIN)%IRECTS((I-1)*2+J)
                            WORK_ARRAY_3(NODE_ID) = WORK_ARRAY_3(NODE_ID) + 1
                            SHIFT = WORK_ARRAY_3(NODE_ID) + SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID) ! index for S_NODE_EDGE
                            SHOOT_STRUCT%S_NODE_EDGE( SHIFT ) = SHIFT_INTER  - 1 + I     ! save the edge ID
                        ENDDO
                    ENDDO
                ENDIF
            ENDIF
            ! ---------------------------
            IF(NRTM+NRTS>0) THEN
                NEXT_INTER = NEXT_INTER + 1
                SHOOT_STRUCT%SHIFT_INTERFACE(NEXT_INTER,1) = SHIFT_INTER
                SHOOT_STRUCT%SHIFT_INTERFACE(NEXT_INTER,2) = NIN
            ENDIF
            SHIFT_INTER = SHIFT_INTER + NRTM + NRTS
        ENDDO
        SHOOT_STRUCT%SHIFT_INTERFACE(NEXT_INTER+1,1) = SHIFT_INTER + 1
        SHOOT_STRUCT%SHIFT_INTERFACE(NINTER+1,1) = SHIFT_INTER + 1
        SHOOT_STRUCT%SHIFT_INTERFACE(NINTER+1,2) = NEXT_INTER


        DEALLOCATE( WORK_ARRAY )

        ALLOCATE( INTERSECT_1(NSPMD) )
        ALLOCATE( INTERSECT_2(NSPMD) )
        ALLOCATE( RESULT_INTERSECT(NSPMD) )

        SIZE_BUFFER_MAIN(1:NSPMD) = 0
        SIZE_BUFFER_SECOND(1:NSPMD) = 0
        DO NIN=1,NINTER
            ITY = IPARI(7,NIN)  ! interface id
            NMN = IPARI(6,NIN)  ! number of main nodes
            NRTM = IPARI(4,NIN) ! number of main surfaces/edges
            NRTS = IPARI(3,NIN) ! number of secondary edges
            IDEL = IPARI(17,NIN)! idel option
            ! ---------------------------
            IF(ITY==11.AND.IDEL==1) THEN
                IF(.NOT.ALLOCATED(SHOOT_STRUCT%INTER)) ALLOCATE(SHOOT_STRUCT%INTER(NINTER))
                ALLOCATE( SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(NRTM) )
                SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(1:NRTM) = 0
                ALLOCATE( SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_S(NRTS) )
                SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_S(1:NRTS) = 0
            ENDIF

            IF(ITY==11.AND.IDEL==1.AND.NSPMD>1) THEN


                DO I=1,NSPMD
                    IF(.NOT.ALLOCATED(BUFFER_SECOND(I)%INT_ARRAY_1D)) THEN
                        BUFFER_SECOND(I)%SIZE_INT_ARRAY_1D = NUMNOD/4+1
                        CALL ALLOC_1D_ARRAY(BUFFER_SECOND(I))
                    ENDIF
                    IF(.NOT.ALLOCATED(BUFFER_MAIN(I)%INT_ARRAY_1D)) THEN
                        BUFFER_MAIN(I)%SIZE_INT_ARRAY_1D = NUMNOD/4+1
                        CALL ALLOC_1D_ARRAY(BUFFER_MAIN(I))
                    ENDIF
                ENDDO


                ! loop over the M edge
                DO I=1,NRTM
                    N1 = INTBUF_TAB(NIN)%IRECTM((I-1)*2+1)
                    N2 = INTBUF_TAB(NIN)%IRECTM((I-1)*2+2)
                    NB_PROC_1 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1) ! get the number of processor of the node     
                    NB_PROC_2 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2) ! get the number of processor of the node
                    IF(NB_PROC_1>1.AND.NB_PROC_2>1) THEN
                        SHIFT =  SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1)
                        INTERSECT_1(1:NB_PROC_1) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC_1 )
                        SHIFT =  SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2)
                        INTERSECT_2(1:NB_PROC_2) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC_2 )

                        NB_RESULT_INTERSECT = 0
                        CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_PROC_1,
     .                                                INTERSECT_2,NB_PROC_2,
     .                                                RESULT_INTERSECT,NB_RESULT_INTERSECT )

                        IF(NB_RESULT_INTERSECT>1) THEN
                            DO J=1,NB_RESULT_INTERSECT
                                PROC_ID = RESULT_INTERSECT(J)
                                IF(PROC_ID/=ISPMD+1) THEN
                                    IF(SIZE_BUFFER_MAIN(PROC_ID)+4>BUFFER_MAIN(PROC_ID)%SIZE_INT_ARRAY_1D) THEN 
                                        OLD_SIZE = BUFFER_MAIN(PROC_ID)%SIZE_INT_ARRAY_1D
                                        ALLOCATE( WORK_ARRAY(OLD_SIZE) )
                                        WORK_ARRAY(1:OLD_SIZE) = 
     .                                    BUFFER_MAIN(PROC_ID)%INT_ARRAY_1D(1:OLD_SIZE)
                                        CALL DEALLOC_1D_ARRAY(BUFFER_MAIN(PROC_ID))
                                        BUFFER_MAIN(PROC_ID)%SIZE_INT_ARRAY_1D = 4 * (OLD_SIZE + 4)
                                        CALL ALLOC_1D_ARRAY(BUFFER_MAIN(PROC_ID))
                                        BUFFER_MAIN(PROC_ID)%INT_ARRAY_1D(1:OLD_SIZE) = WORK_ARRAY(1:OLD_SIZE)
                                        DEALLOCATE( WORK_ARRAY )
                                    ENDIF

                                    SIZE_BUFFER_MAIN(PROC_ID) = SIZE_BUFFER_MAIN(PROC_ID) + 1
                                    BUFFER_MAIN(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_MAIN(PROC_ID) ) = NIN
                                    SIZE_BUFFER_MAIN(PROC_ID) = SIZE_BUFFER_MAIN(PROC_ID) + 1
                                    BUFFER_MAIN(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_MAIN(PROC_ID) ) = I
                                    SIZE_BUFFER_MAIN(PROC_ID) = SIZE_BUFFER_MAIN(PROC_ID) + 1
                                    BUFFER_MAIN(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_MAIN(PROC_ID) ) = ITAB(N1)
                                    SIZE_BUFFER_MAIN(PROC_ID) = SIZE_BUFFER_MAIN(PROC_ID) + 1
                                    BUFFER_MAIN(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_MAIN(PROC_ID) ) = ITAB(N2)
                                ENDIF
                            ENDDO
                        ENDIF
                    ENDIF
                ENDDO

                DO I=1,NRTS
                    N1 = INTBUF_TAB(NIN)%IRECTS((I-1)*2+1)
                    N2 = INTBUF_TAB(NIN)%IRECTS((I-1)*2+2)
                    NB_PROC_1 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1) ! get the number of processor of the node     
                    NB_PROC_2 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2) ! get the number of processor of the node
                    IF(NB_PROC_1>1.AND.NB_PROC_2>1) THEN
                        SHIFT =  SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1)
                        INTERSECT_1(1:NB_PROC_1) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC_1 )
                        SHIFT =  SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2)
                        INTERSECT_2(1:NB_PROC_2) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC_2 )

                        NB_RESULT_INTERSECT = 0
                        CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_PROC_1,
     .                                                INTERSECT_2,NB_PROC_2,
     .                                                RESULT_INTERSECT,NB_RESULT_INTERSECT )

                        IF(NB_RESULT_INTERSECT>1) THEN
                            DO J=1,NB_RESULT_INTERSECT
                                PROC_ID = RESULT_INTERSECT(J)
                                IF(PROC_ID/=ISPMD+1) THEN
                                    IF(SIZE_BUFFER_SECOND(PROC_ID)+4>BUFFER_SECOND(PROC_ID)%SIZE_INT_ARRAY_1D) THEN 
                                        OLD_SIZE = BUFFER_SECOND(PROC_ID)%SIZE_INT_ARRAY_1D
                                        ALLOCATE( WORK_ARRAY(OLD_SIZE) )
                                        WORK_ARRAY(1:OLD_SIZE) = 
     .                                    BUFFER_SECOND(PROC_ID)%INT_ARRAY_1D(1:OLD_SIZE)
                                        CALL DEALLOC_1D_ARRAY(BUFFER_SECOND(PROC_ID))
                                        BUFFER_SECOND(PROC_ID)%SIZE_INT_ARRAY_1D = 
     .                                    4 * (BUFFER_SECOND(PROC_ID)%SIZE_INT_ARRAY_1D + 4)
                                        CALL ALLOC_1D_ARRAY(BUFFER_SECOND(PROC_ID))
                                        BUFFER_SECOND(PROC_ID)%INT_ARRAY_1D(1:OLD_SIZE) = WORK_ARRAY(1:OLD_SIZE)
                                        DEALLOCATE( WORK_ARRAY )
                                    ENDIF
                                    SIZE_BUFFER_SECOND(PROC_ID) = SIZE_BUFFER_SECOND(PROC_ID) + 1
                                    BUFFER_SECOND(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_SECOND(PROC_ID) ) = NIN
                                    SIZE_BUFFER_SECOND(PROC_ID) = SIZE_BUFFER_SECOND(PROC_ID) + 1
                                    BUFFER_SECOND(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_SECOND(PROC_ID) ) = I
                                    SIZE_BUFFER_SECOND(PROC_ID) = SIZE_BUFFER_SECOND(PROC_ID) + 1
                                    BUFFER_SECOND(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_SECOND(PROC_ID) ) = ITAB(N1)
                                    SIZE_BUFFER_SECOND(PROC_ID) = SIZE_BUFFER_SECOND(PROC_ID) + 1
                                    BUFFER_SECOND(PROC_ID)%INT_ARRAY_1D( SIZE_BUFFER_SECOND(PROC_ID) ) = ITAB(N2)
                                ENDIF
                            ENDDO
                        ENDIF
                    ENDIF
                ENDDO
            ENDIF
            ! ---------------------------
        ENDDO
        ! ---------------------------
        IF(NSPMD>1) THEN
#ifdef MPI
            MSGTYP = MSGOFF1
            DO I=1,NSPMD
                R_SIZE_BUFFER_MAIN(I) = 0
                R_SIZE_BUFFER_SECOND(I) = 0
                SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
                IF(I/=ISPMD+1.AND.SIZ>0) THEN
                    S_BUFFER_2_INT(1,I) = SIZE_BUFFER_MAIN(I)
                    S_BUFFER_2_INT(2,I) = SIZE_BUFFER_SECOND(I)
                    CALL MPI_ISEND(S_BUFFER_2_INT(1,I),2,MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_S(I),IERROR)
                ENDIF
                IF(I/=ISPMD+1.AND.SIZ>0) THEN
                    CALL MPI_IRECV(R_BUFFER_2_INT(1,I),2,MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_R(I),IERROR)
           
                ENDIF
            ENDDO

            DO I=1,NSPMD
                SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
                IF(I/=ISPMD+1.AND.SIZ>0) THEN
                    CALL MPI_WAIT(REQUEST_S(I),STATU,IERROR)
                    CALL MPI_WAIT(REQUEST_R(I),STATU,IERROR)
                    R_SIZE_BUFFER_MAIN(I) = R_BUFFER_2_INT(1,I)
                    R_SIZE_BUFFER_SECOND(I) = R_BUFFER_2_INT(2,I)    
                ENDIF
            ENDDO
            DO I=1,NSPMD
                IF(R_SIZE_BUFFER_MAIN(I)>0) THEN
                    R_BUFFER_MAIN(I)%SIZE_INT_ARRAY_1D = R_SIZE_BUFFER_MAIN(I)
                    CALL ALLOC_1D_ARRAY(R_BUFFER_MAIN(I))    
                    CALL MPI_IRECV( R_BUFFER_MAIN(I)%INT_ARRAY_1D,R_BUFFER_MAIN(I)%SIZE_INT_ARRAY_1D,
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_R2(I),IERROR )         
                ENDIF
                IF(R_SIZE_BUFFER_SECOND(I)>0) THEN
                    R_BUFFER_SECOND(I)%SIZE_INT_ARRAY_1D = R_SIZE_BUFFER_SECOND(I)
                    CALL ALLOC_1D_ARRAY(R_BUFFER_SECOND(I))
                    CALL MPI_IRECV( R_BUFFER_SECOND(I)%INT_ARRAY_1D,R_BUFFER_SECOND(I)%SIZE_INT_ARRAY_1D,
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_R3(I),IERROR )                      
                ENDIF
                IF(SIZE_BUFFER_MAIN(I)>0) THEN
                    CALL MPI_ISEND( BUFFER_MAIN(I)%INT_ARRAY_1D,SIZE_BUFFER_MAIN(I),
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_S2(I),IERROR )         
                ENDIF
                IF(SIZE_BUFFER_SECOND(I)>0) THEN
                    CALL MPI_ISEND( BUFFER_SECOND(I)%INT_ARRAY_1D,SIZE_BUFFER_SECOND(I),
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_S3(I),IERROR )                      
                ENDIF
            ENDDO
            DO I=1,NSPMD
                SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
                IF(SIZE_BUFFER_MAIN(I)>0) CALL MPI_WAIT(REQUEST_S2(I),STATU,IERROR)
                IF(R_SIZE_BUFFER_MAIN(I)>0) CALL MPI_WAIT(REQUEST_R2(I),STATU,IERROR)
                IF(SIZE_BUFFER_SECOND(I)>0) CALL MPI_WAIT(REQUEST_S3(I),STATU,IERROR)
                IF(R_SIZE_BUFFER_SECOND(I)>0) CALL MPI_WAIT(REQUEST_R3(I),STATU,IERROR)
            ENDDO
            DO I=1,NSPMD
                IF(R_BUFFER_MAIN(I)%SIZE_INT_ARRAY_1D>0) THEN
                    CALL COUNT_REMOTE_NB_ELEM_EDGE( R_BUFFER_MAIN(I)%SIZE_INT_ARRAY_1D,R_BUFFER_MAIN(I)%INT_ARRAY_1D,
     .                                              GEO,IXS,IXC,IXT,IXP,IXR,IXTG,ADDCNEL,ITABM1,CNEL )
                    CALL MPI_ISEND( R_BUFFER_MAIN(I)%INT_ARRAY_1D,R_BUFFER_MAIN(I)%SIZE_INT_ARRAY_1D,
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_S2(I),IERROR )     
   
                ENDIF
                IF(R_BUFFER_SECOND(I)%SIZE_INT_ARRAY_1D>0) THEN
                    CALL COUNT_REMOTE_NB_ELEM_EDGE( R_BUFFER_SECOND(I)%SIZE_INT_ARRAY_1D,R_BUFFER_SECOND(I)%INT_ARRAY_1D,
     .                                              GEO,IXS,IXC,IXT,IXP,IXR,IXTG,ADDCNEL,ITABM1,CNEL )
                    CALL MPI_ISEND( R_BUFFER_SECOND(I)%INT_ARRAY_1D,R_BUFFER_SECOND(I)%SIZE_INT_ARRAY_1D,
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_S3(I),IERROR )
                ENDIF
                IF(SIZE_BUFFER_MAIN(I)>0) THEN
                    CALL MPI_IRECV( BUFFER_MAIN(I)%INT_ARRAY_1D,SIZE_BUFFER_MAIN(I),
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_R2(I),IERROR )  
                ENDIF
                IF(SIZE_BUFFER_SECOND(I)>0) THEN
                    CALL MPI_IRECV( BUFFER_SECOND(I)%INT_ARRAY_1D,SIZE_BUFFER_SECOND(I),
     .                             MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,REQUEST_R3(I),IERROR )  
                ENDIF
            ENDDO
            DO I=1,NSPMD
                SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
                IF(R_BUFFER_MAIN(I)%SIZE_INT_ARRAY_1D>0) THEN
                    CALL MPI_WAIT(REQUEST_S2(I),STATU,IERROR)
                    CALL DEALLOC_1D_ARRAY(R_BUFFER_MAIN(I))   
                ENDIF
                IF(SIZE_BUFFER_MAIN(I)>0) THEN
                    CALL MPI_WAIT(REQUEST_R2(I),STATU,IERROR)
                    CALL COUNT_NB_ELEM_EDGE( 1,SIZE_BUFFER_MAIN(I),BUFFER_MAIN(I)%INT_ARRAY_1D,SHOOT_STRUCT )
                    CALL DEALLOC_1D_ARRAY(BUFFER_MAIN(I))
                ENDIF
                IF(R_BUFFER_SECOND(I)%SIZE_INT_ARRAY_1D>0) THEN
                    CALL MPI_WAIT(REQUEST_S3(I),STATU,IERROR)
                    CALL DEALLOC_1D_ARRAY(R_BUFFER_SECOND(I))   
                ENDIF
                IF(SIZE_BUFFER_SECOND(I)>0) THEN
                    CALL MPI_WAIT(REQUEST_R3(I),STATU,IERROR)
                    CALL COUNT_NB_ELEM_EDGE( 2,SIZE_BUFFER_SECOND(I),BUFFER_SECOND(I)%INT_ARRAY_1D,SHOOT_STRUCT )
                    CALL DEALLOC_1D_ARRAY(BUFFER_SECOND(I))
                ENDIF
            ENDDO
#endif
        ENDIF
        ! ---------------------------

        DEALLOCATE( WORK_ARRAY_2 )
        DEALLOCATE( WORK_ARRAY_3 )

        DEALLOCATE( INTERSECT_1 )
        DEALLOCATE( INTERSECT_2 )
        DEALLOCATE( RESULT_INTERSECT )

        DEALLOCATE( BUFFER_SECOND )
        DEALLOCATE( BUFFER_MAIN )
        DEALLOCATE( R_BUFFER_SECOND )
        DEALLOCATE( R_BUFFER_MAIN )
        ! --------------------------------
        ! WORKING ARRAY
        ! --------------------------------
        ALLOCATE( SHOOT_STRUCT%GLOBAL_NB_ELEM_OFF(NTHREAD) )    ! number of deactivated element for each thread

        RETURN
        END SUBROUTINE INIT_NODAL_STATE
