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
Chd|  SPLIT_EBCS                    source/boundary_conditions/ebcs/split_ebcs.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale/ale_ebcs_mod.F
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
        SUBROUTINE SPLIT_EBCS( PROC_ID,LOCAL_NEBCS,LIST_OTHER_EBCS,NUMEL,
     1                         CEP,IGRSURF,LOCAL_ELEMENT_ID,NODLOCAL,
     2                         EBCS_TAB,EBCS_TAB_LOC_2)
!$COMMENT
!       SPLIT_EBCS description
!       SPLIT_EBCS split the elements/node/data of the ebcs's surface on the different processors
!       
!       SPLIT_EBCS organization :
!       loop over the /EBCS options :
!           * loop over the elements of the surface
!               * if the element is on the current proc PROC_ID, 
!                 convert global data to local data 
!                 example : convert the global element ID to local element ID 
!                           (local = local to the PROC_ID processor)
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE FRONT_MOD
        USE UNITAB_MOD
        USE MESSAGE_MOD
        USE MULTI_FVM_MOD
        USE GROUPDEF_MOD
        USE RESTMOD
        USE TABLE_MOD
        USE SUBMODEL_MOD
        USE ALE_EBCS_MOD
        USE EBCS_MOD
        USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: LOCAL_NEBCS ! number of parallelized ebcs
        INTEGER, INTENT(IN) :: NUMEL ! number of element
        INTEGER, INTENT(IN) :: PROC_ID ! processor ID
        INTEGER, DIMENSION(LOCAL_NEBCS), INTENT(IN) :: LIST_OTHER_EBCS ! list of parallelized ebcs
        INTEGER, DIMENSION(NUMEL), INTENT(IN) :: CEP ! connectivity element/proc 
        INTEGER, DIMENSION(NUMEL), INTENT(IN) :: LOCAL_ELEMENT_ID ! list of local element id (local to the current proc)
        INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: NODLOCAL ! array : 1 if the node is on the current proc, used here for debug
        TYPE (SURF_), DIMENSION(NSURF), TARGET, INTENT(IN) :: IGRSURF ! surface structure
        TYPE(T_EBCS_TAB), INTENT(INOUT) :: EBCS_TAB,EBCS_TAB_LOC_2 ! ebcs structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,IJK
        INTEGER :: GLOBAL_INDEX,EBCS_ID,SURF_ID,NSEG
        INTEGER :: ELEM_ID,LOCAL_SEG
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: LIST_NODE,LIST_NODE_2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        
        GLOBAL_INDEX = 0
        ! ---------------------------
        ! loop over the ebcs
        DO I=1,LOCAL_NEBCS
            EBCS_ID = LIST_OTHER_EBCS(I)
            ! check if a surface is associated to the ebcs
            IF(EBCS_TAB_LOC_2%tab(I)%poly%surf_id>0) THEN
                SURF_ID = EBCS_TAB_LOC_2%tab(I)%poly%surf_id ! surface id
                NSEG = IGRSURF(SURF_ID)%NSEG ! number of surface
                ! --------------
                ! allocation of global element id array : its size is overestimated
                IF(.NOT.ALLOCATED(EBCS_TAB_LOC_2%tab(I)%poly%global_ielem)) THEN
                    ALLOCATE( EBCS_TAB_LOC_2%tab(I)%poly%global_ielem(NSEG) )
                ENDIF
                ! --------------
                ALLOCATE( LIST_NODE(NSEG,4) )
                LOCAL_SEG = 0 
                ! --------------
                ! loop over the element of the surface
                DO J = 1,NSEG
                    ELEM_ID = EBCS_TAB%tab(EBCS_ID)%poly%ielem(J)
                    ! --------------
                    ! check if the element is on the current PROC_ID processor
                    ! if true --> convert global data to local data
                    IF( CEP(ELEM_ID)+1 == PROC_ID) THEN
                        LOCAL_SEG = LOCAL_SEG + 1
                        GLOBAL_INDEX = GLOBAL_INDEX + 1
                        DO IJK = 1,4
                            LIST_NODE(LOCAL_SEG,IJK) = IGRSURF(SURF_ID)%NODES(J,IJK)
                        ENDDO
                        EBCS_TAB_LOC_2%tab(I)%poly%iseg(LOCAL_SEG) = SIGN(EBCS_TAB%tab(EBCS_ID)%poly%iseg(J),GLOBAL_INDEX)

                        EBCS_TAB_LOC_2%tab(I)%poly%ielem(LOCAL_SEG) = LOCAL_ELEMENT_ID(EBCS_TAB%tab(EBCS_ID)%poly%ielem(J))
                        EBCS_TAB_LOC_2%tab(I)%poly%itype(LOCAL_SEG) = EBCS_TAB%tab(EBCS_ID)%poly%itype(J)

                        EBCS_TAB_LOC_2%tab(I)%poly%global_ielem(LOCAL_SEG) = EBCS_TAB%tab(EBCS_ID)%poly%ielem(J)
                    ENDIF
                    ! --------------
                ENDDO
                ! --------------
                ALLOCATE( LIST_NODE_2(LOCAL_SEG,4) )
                LIST_NODE_2(1:LOCAL_SEG,1:4) = LIST_NODE(1:LOCAL_SEG,1:4)
                CALL EBCS_TAB_LOC_2%tab(I)%poly%set_nodes_elems(LOCAL_SEG, NUMNOD, LIST_NODE_2)
                DEALLOCATE( LIST_NODE,LIST_NODE_2 )
            ENDIF
        ENDDO
        ! ---------------------------
        RETURN

        END SUBROUTINE SPLIT_EBCS
