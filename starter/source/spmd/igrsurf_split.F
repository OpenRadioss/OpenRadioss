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
Chd|  IGRSURF_SPLIT                 source/spmd/igrsurf_split.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
        SUBROUTINE IGRSURF_SPLIT(SCEP,CEP,T_MONVOL,IGRSURF,IGRSURF_PROC)
!$COMMENT
! IGSURF_SPLIT description
! IGSURF_SPLIT splits the global IGSURF array into local 
!              IGSURF_PROC arrays in order to save 
!              CPU time in ddsplit routine (avoid NSPMD 
!              treatments)
! 
! IGSURF_SPLIT organization :
! - 1rst step : count the number of element per surface 
!               on a given processor and allocate the 
!               IGRSURF_PROC structure
! - 2nd step : fill the structure
!$ENDCOMMENT
        
        USE GROUPDEF_MOD
        USE MONVOL_STRUCT_MOD
        USE ARRAY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: SCEP !< size of CEP array
        INTEGER, DIMENSION(SCEP), INTENT(IN) :: CEP !< connectivity element --> processor
        TYPE(SURF_), DIMENSION(NSURF), INTENT(INOUT) :: IGRSURF !< surface structure, size =NSURF
        TYPE(SURF_), DIMENSION(NSURF,NSPMD), INTENT(INOUT) :: IGRSURF_PROC !< surface structure per proc , size =NSURF,NSPMD
        TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL !< monitor volume structure, size =NVOLU

! -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
! CEP    : integer ; dimension=NUMNOD
!          CEP gives the id processor of an element

!             monitor volume array
! IGRSURF : SURF_ ; dimension=NSURF
!           global surface property array
!           %ELTYP --> type of element (shell, triangle...)
!           %ELEM  --> element id
!           %NSEG --> total element number
! IGRSURF_PROC : SURF_ ; dimension=NSURF*NSPMD
!           local surface property array (=IGRSURF for each proc)
!           %ELTYP --> type of element (shell, triangle...)
!           %ELEM  --> element id
!           %NSEG --> total element number
! -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NV,IS,NN,J,K,SHIFT
        INTEGER :: ITY,II,PROC,K1
        INTEGER :: OFFC,OFFTG,OFFS,OFFQ
        INTEGER, DIMENSION(NSPMD) :: JJ
        INTEGER :: NJ,NJ1,NJ2,NJ3
        INTEGER :: NODE_ID,NJET
        INTEGER :: I_AM_HERE
        LOGICAL, DIMENSION(NSURF) :: ALREADY_DONE
        LOGICAL, DIMENSION(:), ALLOCATABLE :: I_NEED_IT
        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: PROC_LIST_PER_NODE

        INTEGER :: UP_BOUND
        INTEGER :: INDEX_PROC,NUMBER_PROC
        INTEGER :: SIZE_MERGED_LIST,SIZE_PROC_LIST,SIZE_UNION_PROC_LIST 
        INTEGER, DIMENSION(:), ALLOCATABLE :: MERGED_LIST
        INTEGER, DIMENSION(NSPMD) :: UNION_PROC_LIST,PROC_LIST,NUMBER_APPEARANCE
        INTEGER :: ELEM_ID 
        INTEGER, DIMENSION(0:7) :: OFFSET ! offset array
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------

        ! --------------------------------------
        ! offset for the CEP array
        OFFS = 0 ! offset for solid
        OFFQ = NUMELS ! offset for quad
        OFFC = NUMELS+NUMELQ ! offset for shell   
        OFFTG = NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR ! offset for triangle
        OFFSET(0:7) = 0
        OFFSET(1) = OFFS
        OFFSET(2) = OFFQ
        OFFSET(3) = OFFC
        OFFSET(7) = OFFTG

        ALREADY_DONE(1:NSURF) = .FALSE.
        ! 1st step : count the number of element per proc and allocate the structure
        K1 = 1

        IGRSURF_PROC(1:NSURF,1:NSPMD)%NSEG = 0

        DO NV=1,NVOLU ! NVOLU = number of volume
            IS = T_MONVOL(NV)%EXT_SURFID ! id of the surface 
            NN = IGRSURF(IS)%NSEG   ! number of element per surface
            JJ(1:NSPMD) = 0         ! proc index
            IGRSURF_PROC(IS,1:NSPMD)%NSEG = 0
            ALREADY_DONE(IS) = .TRUE.
            DO J=1,NN
                ITY = IGRSURF(IS)%ELTYP(J)      ! type of the element 
                II  = IGRSURF(IS)%ELEM(J)       ! id of the element
                PROC = 0                        ! id of the proc /= 0 if ITY = 3 or 7
                IF(ITY==3) THEN
                    PROC = CEP(OFFC+II) + 1
                ELSEIF(ITY==7) THEN
                    PROC = CEP(OFFTG+II) + 1
                ENDIF
                IF(PROC>0) THEN
                    JJ(PROC) = JJ(PROC) + 1
                ENDIF
            ENDDO
            ! -----------------------------
            ! allocation : several MONVOL can refer to the same surface ID
            DO PROC=1,NSPMD
                IF(.NOT.ALLOCATED(IGRSURF_PROC(IS,PROC)%ELTYP).AND.JJ(PROC)>0) THEN
                    IGRSURF_PROC(IS,PROC)%NSEG = JJ(PROC)
                    ALLOCATE( IGRSURF_PROC(IS,PROC)%ELTYP( JJ(PROC) ) )
                    ALLOCATE( IGRSURF_PROC(IS,PROC)%ELEM( JJ(PROC) ) )
                    ALLOCATE( IGRSURF_PROC(IS,PROC)%LOCAL_SEG(  JJ(PROC) ) )
                ENDIF
                ! -----------------------------
                ! force the NJ1, NJ2, NJ3 nodes on the processor PROC
                IF(JJ(PROC)>0) THEN
                    NJET = T_MONVOL(NV)%NJET
                    DO NJ = 1, NJET
                        NJ1 = T_MONVOL(NV)%IBAGJET(5, NJ)
                        NJ2 = T_MONVOL(NV)%IBAGJET(6, NJ)
                        NJ3 = T_MONVOL(NV)%IBAGJET(7, NJ)
                        IF (NJ1 /= 0) CALL IFRONTPLUS(NJ1, PROC)
                        IF (NJ2 /= 0) CALL IFRONTPLUS(NJ2, PROC)
                        IF (NJ3 /= 0) CALL IFRONTPLUS(NJ3, PROC)
                    ENDDO

                    IF (T_MONVOL(NV)%NB_FILL_TRI > 0) THEN
                        DO J = 1, T_MONVOL(NV)%NB_FILL_TRI
                            I_AM_HERE = 0
                            NODE_ID = T_MONVOL(NV)%FILL_TRI(3 * (J - 1) + 1)
                            IF (NODE_ID > 0) THEN
                                CALL IFRONTPLUS(NODE_ID, PROC)
                                I_AM_HERE = I_AM_HERE + 1
                            ENDIF
                            NODE_ID = T_MONVOL(NV)%FILL_TRI(3 * (J - 1) + 2)
                            IF (NODE_ID > 0) THEN
                                CALL IFRONTPLUS(NODE_ID, PROC)
                                I_AM_HERE = I_AM_HERE + 1
                            ENDIF
                            NODE_ID = T_MONVOL(NV)%FILL_TRI(3 * (J - 1) + 3)
                            IF (NODE_ID > 0) THEN
                                CALL IFRONTPLUS(NODE_ID, PROC)
                                I_AM_HERE = I_AM_HERE + 1
                            ENDIF
                            IF( I_AM_HERE==3 ) THEN
                                T_MONVOL(NV)%NUMBER_TRI_PER_PROC(PROC) = 
     .                          T_MONVOL(NV)%NUMBER_TRI_PER_PROC(PROC) + 1
                            ENDIF
                        ENDDO
                    ENDIF
                    ! -----------------------------
                ENDIF
            ENDDO
                ! -----------------------------
                K1 = K1 + NIMV
        ENDDO
        ! --------------------------------------
        ! 2nd step : fill the structure
        K1 = 1
        DO NV=1,NVOLU                   ! NVOLU = number of volume
            IS = T_MONVOL(NV)%EXT_SURFID      ! id of the surface 
            NN = IGRSURF(IS)%NSEG   ! number of element per surface
            JJ(1:NSPMD) = 0                 ! proc index
            DO J=1,NN
                ITY = IGRSURF(IS)%ELTYP(J)      ! type of the element 
                II  = IGRSURF(IS)%ELEM(J)       ! id of the element
                PROC = 0                        ! id of the proc /= 0 if ITY = 3 or 7
                IF(ITY==3) THEN
                     PROC = CEP(OFFC+II) + 1
                ELSEIF(ITY==7) THEN
                     PROC = CEP(OFFTG+II) + 1
                ENDIF
                IF(PROC>0) THEN
                    JJ(PROC) = JJ(PROC) + 1
                    IGRSURF_PROC(IS,PROC)%ELTYP(JJ(PROC)) = ITY
                    IGRSURF_PROC(IS,PROC)%ELEM(JJ(PROC)) = II
                    IGRSURF_PROC(IS,PROC)%LOCAL_SEG( JJ(PROC) ) = J
                ENDIF
            ENDDO
            K1 = K1 + NIMV
        ENDDO
        ! --------------------------------------
        ALLOCATE( I_NEED_IT(NUMNOD) )
        ALLOCATE( PROC_LIST_PER_NODE(NUMNOD) )
        I_NEED_IT(1:NUMNOD) = .FALSE.
        DO IS=1,NSURF
            IF(.NOT.ALREADY_DONE(IS)) THEN
                NN = IGRSURF(IS)%NSEG ! number of segment
                ALLOCATE( IGRSURF(IS)%PROC(NN) )
                IGRSURF(IS)%PROC(1:NN) = 0
                IGRSURF_PROC(IS,1:NSPMD)%NSEG = 0
                ! -------------
                ! loop over the segment of the surface
                DO J=1,NN
                    ITY = IGRSURF(IS)%ELTYP(J)      ! type of the element 
                    II  = IGRSURF(IS)%ELEM(J)       ! id of the element                 
                    ! -------------
                    ! find on which processor the segment is defined
                    ! if ity/=0 (segment belongs to an element) --> the processor is given by CEP array
                    IF(ITY==0) THEN ! segment is not related to a element --> need to find the list of proc where the nodes are set
                        UP_BOUND = 4
                        IF(N2D/=0) UP_BOUND = 2
                        ! -------------
                        ! loop over the 4 nodes of the segment
                        DO K=1,UP_BOUND
                            NODE_ID = IGRSURF(IS)%NODES(J,K)
                            IF(NODE_ID/=0) THEN
                                IF(.NOT.I_NEED_IT(NODE_ID)) THEN ! if true, already computed
                                    I_NEED_IT(NODE_ID) = .TRUE.
                                    NUMBER_PROC = 0
                                    CALL PLIST_IFRONT(PROC_LIST,NODE_ID,NUMBER_PROC) ! compute the list of proc
                                    PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D = NUMBER_PROC
                                    CALL ALLOC_1D_ARRAY(PROC_LIST_PER_NODE(NODE_ID))
                                    PROC_LIST_PER_NODE(NODE_ID)%INT_ARRAY_1D(1:NUMBER_PROC) = PROC_LIST(1:NUMBER_PROC)
                                ENDIF
                            ENDIF
                        ENDDO
                        ! -------------
                    ENDIF
                    ! -------------
                ENDDO
                ! -------------
            ENDIF
        ENDDO
        ! --------------------------------------
        ! loop over the surface to find where the nodes ared defined
        ! if ity/=0 --> use the CEP array
        ! if ity=0 --> compute the union of processor list of the 4 nodes
        !              merge the processor lists in 1 list
        !              if there are several processor in the merged list, need to find the processor with the highest occurence
        DO IS=1,NSURF
            IF(.NOT.ALREADY_DONE(IS)) THEN
                 IGRSURF_PROC(IS,1:NSPMD)%NSEG = 0
                NN = IGRSURF(IS)%NSEG ! number of segment
                ! -------------
                ! loop over the segment of the surface
                DO J=1,NN
                    ITY = IGRSURF(IS)%ELTYP(J)      ! type of the element 
                    II  = IGRSURF(IS)%ELEM(J)       ! id of the element
                    PROC = 0                        
                    ! -------------
                    ! find on which processor the segment is defined
                    ! if ity/=0 (segment belongs to an element) --> the processor is given by CEP array
                    IF(ITY==1) THEN ! solid
                        PROC = CEP(II) + 1
                    ELSEIF(ITY==2) THEN ! quad
                        PROC = CEP(OFFS+II) + 1
                    ELSEIF(ITY==3) THEN ! shell
                        PROC = CEP(OFFC+II) + 1
                    ELSEIF(ITY==7) THEN ! triangle
                        PROC = CEP(OFFTG+II) + 1
                    ELSEIF(ITY==0) THEN ! segment is not related to a element
                        UP_BOUND = 4
                        IF(IGRSURF(IS)%NODES(J,3)==IGRSURF(IS)%NODES(J,4)) UP_BOUND = 3
                        IF(N2D/=0) UP_BOUND = 2
                        SIZE_MERGED_LIST = 0
                        ! ------------
                        ! compute the size of merged list
                        DO K=1,UP_BOUND
                            NODE_ID = IGRSURF(IS)%NODES(J,K)
                            IF(NODE_ID/=0) THEN
                                SIZE_MERGED_LIST = SIZE_MERGED_LIST + PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D
                            ENDIF
                        ENDDO
                        ! ------------
                        ALLOCATE( MERGED_LIST(SIZE_MERGED_LIST) )
                        MERGED_LIST(1:SIZE_MERGED_LIST) = -1
                        NODE_ID = IGRSURF(IS)%NODES(J,1)
                        SIZE_UNION_PROC_LIST = 0   
                        IF(NODE_ID/=0) THEN     
                            ! ------------
                            ! merge of processor list           
                            MERGED_LIST(1:PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D) = 
     .                       PROC_LIST_PER_NODE(NODE_ID)%INT_ARRAY_1D(1:PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D)
                            SHIFT = PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D

                            PROC_LIST(1:PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D) = 
     .                       PROC_LIST_PER_NODE(NODE_ID)%INT_ARRAY_1D(1:PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D)
                            SIZE_PROC_LIST = PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D
                            SIZE_UNION_PROC_LIST = 0
                            ! ------------
                            ! merge + union computation
                            DO K=2,UP_BOUND     
                                NODE_ID = IGRSURF(IS)%NODES(J,K)        

                                MERGED_LIST(SHIFT+1:SHIFT+PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D) = 
     .                             PROC_LIST_PER_NODE(NODE_ID)%INT_ARRAY_1D(1:PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D)
                                SHIFT = SHIFT + PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D

                                CALL UNION_2_SORTED_SETS(PROC_LIST, SIZE_PROC_LIST,
     .                             PROC_LIST_PER_NODE(NODE_ID)%INT_ARRAY_1D, PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D,
     .                             UNION_PROC_LIST, SIZE_UNION_PROC_LIST )
                                PROC_LIST(1:SIZE_UNION_PROC_LIST) = UNION_PROC_LIST(1:SIZE_UNION_PROC_LIST)
                                SIZE_PROC_LIST = SIZE_UNION_PROC_LIST
                            ENDDO
                            ! ------------
                        ENDIF

                        ! ------------
                        ! find the leading processor 
                        NUMBER_APPEARANCE(1:NSPMD) = 0
                        IF(SIZE_UNION_PROC_LIST>0) THEN
                            CALL COUNT_MEMBER_LIST( UNION_PROC_LIST, SIZE_UNION_PROC_LIST,
     .                                          MERGED_LIST, SIZE_MERGED_LIST,
     .                                          NUMBER_APPEARANCE, INDEX_PROC )
                            PROC = UNION_PROC_LIST(INDEX_PROC)
                            ! ------------
                            ! if all the nodes are not defined on PROC, need to add it on PROC
                            IF( NUMBER_APPEARANCE(INDEX_PROC)/=UP_BOUND ) THEN
                                DO K=1,UP_BOUND     
                                    NODE_ID = IGRSURF(IS)%NODES(J,K)   
                                    IF(NODE_ID/=0) THEN 
                                        CALL IFRONTPLUS(NODE_ID, PROC)
    
                                        NUMBER_PROC = 0
                                        CALL PLIST_IFRONT(PROC_LIST,NODE_ID,NUMBER_PROC)
                                        CALL DEALLOC_1D_ARRAY(PROC_LIST_PER_NODE(NODE_ID))
                                        PROC_LIST_PER_NODE(NODE_ID)%SIZE_INT_ARRAY_1D = NUMBER_PROC
                                        CALL ALLOC_1D_ARRAY(PROC_LIST_PER_NODE(NODE_ID))
                                        PROC_LIST_PER_NODE(NODE_ID)%INT_ARRAY_1D(1:NUMBER_PROC) = PROC_LIST(1:NUMBER_PROC)
                                    ENDIF
                                ENDDO
                            ENDIF
                            ! ------------
                        ENDIF
                        ! ------------
                        DEALLOCATE( MERGED_LIST )
                    ENDIF
                    ! -------------
                    ! save the processor for the segment J
                    IF(PROC>0) THEN
                        IGRSURF_PROC(IS,PROC)%NSEG = IGRSURF_PROC(IS,PROC)%NSEG + 1
                        IGRSURF(IS)%PROC(J) = PROC
                    ENDIF
                ENDDO
                ! -------------
            ENDIF
        ENDDO
        ! --------------------------------------
        ! loop over the surface to save the IGRSURF% data into IGRSURF_PROC% structure
        DO IS=1,NSURF
            IF(.NOT.ALREADY_DONE(IS)) THEN
                JJ(1:NSPMD) = 0
                DO J=1,NSPMD
                    ALLOCATE( IGRSURF_PROC(IS,J)%ELTYP( IGRSURF_PROC(IS,J)%NSEG ) )
                    ALLOCATE( IGRSURF_PROC(IS,J)%ELEM( IGRSURF_PROC(IS,J)%NSEG ) )
                    ALLOCATE( IGRSURF_PROC(IS,J)%LOCAL_SEG( IGRSURF_PROC(IS,J)%NSEG ) )
                ENDDO
                NN = IGRSURF(IS)%NSEG ! number of segment
                DO J=1,NN
                    PROC = IGRSURF(IS)%PROC(J)
                    IF(PROC/=0) THEN
                        JJ(PROC) = JJ(PROC) + 1
                        ITY = IGRSURF(IS)%ELTYP(J)
                        IGRSURF_PROC(IS,PROC)%ELTYP( JJ(PROC) ) = ITY ! type of segment
                        ELEM_ID = IGRSURF(IS)%ELEM(J) 
                        IGRSURF_PROC(IS,PROC)%ELEM( JJ(PROC) ) = ELEM_ID ! element id
                        IGRSURF_PROC(IS,PROC)%LOCAL_SEG( JJ(PROC) ) = J ! pointer from IGRSURF_PROC to IGRSURF
                    ENDIF
                ENDDO
            ENDIF
        ENDDO
        ! --------------------------------------
        DEALLOCATE( I_NEED_IT )
        DEALLOCATE( PROC_LIST_PER_NODE )

        RETURN
        END SUBROUTINE IGRSURF_SPLIT
