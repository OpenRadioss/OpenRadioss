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
Chd|  CHECK_EDGE_STATE              source/interfaces/interf/check_edge_state.F
Chd|-- called by -----------
Chd|        FIND_EDGE_FROM_REMOTE_PROC    source/interfaces/interf/find_edge_from_remote_proc.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CHECK_ACTIVE_ELEM_EDGE        source/interfaces/interf/check_active_elem_edge.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE CHECK_EDGE_STATE( ITASK,M_EDGE_NB,S_EDGE_NB,M_EDGE_ID,S_EDGE_ID,
     .                               SHIFT_INTERFACE,INTBUF_TAB,NEWFRONT,IPARI,GEO,
     .                               IXS,IXC,IXT,IXP,IXR,IXTG,
     .                               ADDCNEL,CNEL,TAG_NODE,TAG_ELEM,SHOOT_STRUCT )
!$COMMENT
!       CHECK_EDGE_STATE description
!           check the state of an edge (active or not)
!       CHECK_EDGE_STATE organization
!           loop over a list of edge :
!           -check if 1 or more element associated to the edge is/are active
!           - if there is no active element, the edge is deactivate
!$ENDCOMMENT
        USE INTBUFDEF_MOD 
        USE SHOOTING_NODE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK ! omp thread ID
        INTEGER, INTENT(in) :: M_EDGE_NB,S_EDGE_NB  ! number of local edge surface
        INTEGER, DIMENSION(M_EDGE_NB), INTENT(in) :: M_EDGE_ID  ! id of main edge that need to be deactivated
        INTEGER, DIMENSION(S_EDGE_NB), INTENT(in) :: S_EDGE_ID  ! id of secon. edge that need to be deactivated
        INTEGER, DIMENSION(NINTER), INTENT(inout) :: NEWFRONT   ! array for sorting : 1 --> need to sort the interface NIN
        INTEGER, DIMENSION(NINTER+1,2), INTENT(in) :: SHIFT_INTERFACE ! interface shift
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data 
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI

        INTEGER, DIMENSION(NIXS,NUMELS), INTENT(in) :: IXS   ! solid array
        INTEGER, DIMENSION(NIXC,NUMELC), INTENT(in) :: IXC   ! shell array
        INTEGER, DIMENSION(NIXT,NUMELT), INTENT(in) :: IXT! truss array
        INTEGER, DIMENSION(NIXP,NUMELP), INTENT(in) :: IXP! beam array
        INTEGER, DIMENSION(NIXR,NUMELR), INTENT(in) :: IXR! spring array
        INTEGER, DIMENSION(NIXTG,NUMELTG), INTENT(in) :: IXTG! triangle array
        INTEGER, DIMENSION(0:NUMNOD+1), INTENT(in) :: ADDCNEL ! address for the CNEL array
        my_real, DIMENSION(NPROPG,NUMGEO), INTENT(in) :: GEO
        INTEGER, DIMENSION(0:LCNEL), INTENT(in) :: CNEL ! connectivity node-->element
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: TAG_NODE
        INTEGER, DIMENSION(NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG), INTENT(inout) :: TAG_ELEM
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: DEACTIVATION
        INTEGER :: I,K,FIRST,LAST
        INTEGER :: NIN,ID_INTER,NUMBER_INTER
        INTEGER :: ITY,IDEL
        INTEGER :: N1,N2,N3,N4
        INTEGER :: NUMBER_NODE
        INTEGER :: DICHOTOMIC_SEARCH_I_ASC  ! function
C-----------------------------------------------
        IF(ITASK==-1) THEN
            FIRST = 1
            LAST = M_EDGE_NB
        ELSE
            FIRST = 1 + ITASK * (M_EDGE_NB / NTHREAD)
            LAST = (ITASK + 1) * (M_EDGE_NB / NTHREAD)
            IF((ITASK+1)==NTHREAD) LAST = M_EDGE_NB
        ENDIF
        NUMBER_INTER = SHIFT_INTERFACE(NINTER+1,2)
        ! --------------------------
        ! loop over the deactivated edge: main node
        DO I=FIRST,LAST
            K = M_EDGE_ID(I)  ! get the global edge id
            ID_INTER = DICHOTOMIC_SEARCH_I_ASC(K, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
            NIN = SHIFT_INTERFACE(ID_INTER,2)
            K = K - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface

            ITY = IPARI(7,NIN)
            IDEL = IPARI(17,NIN)

            IF(ITASK==-1) THEN
                IF(ITY==11.AND.IDEL==1) THEN
                    SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(K) = SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(K) - 1
                ENDIF
            ENDIF

            ! check if the edge is active, if yes --> deactivate it
            IF(INTBUF_TAB(NIN)%STFM(K)/=ZERO) THEN

                IF(ITY==11.AND.IDEL==1) THEN
                    N1 = INTBUF_TAB(NIN)%IRECTM((K-1)*2+1)
                    N2 = INTBUF_TAB(NIN)%IRECTM((K-1)*2+2)
                    N3 = 0
                    N4 = 0
                    NUMBER_NODE = 2
                    IF(SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(K)<1) THEN
                        CALL CHECK_ACTIVE_ELEM_EDGE( NUMBER_NODE, N1,N2,N3,N4,
     .                                               DEACTIVATION,GEO,IXS,IXC,
     .                                               IXT,IXP,IXR,IXTG,ADDCNEL,CNEL,
     .                                               TAG_NODE,TAG_ELEM )
                    ELSE
                        DEACTIVATION = .FALSE.
                    ENDIF
                ELSE
                    DEACTIVATION=.TRUE.
                ENDIF
                IF(DEACTIVATION) INTBUF_TAB(NIN)%STFM(K)  = ZERO
            ENDIF
        ENDDO
        ! --------------------------

        IF(ITASK==-1) THEN
            FIRST = 1
            LAST = S_EDGE_NB
        ELSE
            FIRST = 1 + ITASK * (S_EDGE_NB / NTHREAD)
            LAST = (ITASK + 1) * (S_EDGE_NB / NTHREAD)
            IF((ITASK+1)==NTHREAD) LAST = S_EDGE_NB
        ENDIF
        ! --------------------------
        ! loop over the deactivated edge : secondary node
        DO I=FIRST,LAST
            K = S_EDGE_ID(I)  ! get the global edge id
            ID_INTER = DICHOTOMIC_SEARCH_I_ASC(K, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
            NIN = SHIFT_INTERFACE(ID_INTER,2)
            K = K - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface

            ITY = IPARI(7,NIN)
            IDEL = IPARI(17,NIN)

            IF(ITASK==-1) THEN
                IF(ITY==11.AND.IDEL==1) THEN
                    SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_S(K) = SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_S(K) - 1
                ENDIF
            ENDIF

            ! check if the edge is active, if yes --> deactivate it
            IF(INTBUF_TAB(NIN)%STFS(K)/=ZERO) THEN
                IF(ITY==11.AND.IDEL==1) THEN
                    N1 = INTBUF_TAB(NIN)%IRECTS((K-1)*2+1)
                    N2 = INTBUF_TAB(NIN)%IRECTS((K-1)*2+2)
                    N3 = 0
                    N4 = 0
                    NUMBER_NODE = 2
                    IF(SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_S(K)<1) THEN
                        CALL CHECK_ACTIVE_ELEM_EDGE( NUMBER_NODE, N1,N2,N3,N4,
     .                                               DEACTIVATION,GEO,IXS,IXC,
     .                                               IXT,IXP,IXR,IXTG,ADDCNEL,CNEL,
     .                                               TAG_NODE,TAG_ELEM )
                    ELSE
                        DEACTIVATION = .FALSE.
                    ENDIF
                ELSE
                    DEACTIVATION=.TRUE.
                ENDIF

                IF(DEACTIVATION) THEN       
                    INTBUF_TAB(NIN)%STFS(K) = -ABS(INTBUF_TAB(NIN)%STFS(K))
                    NEWFRONT(NIN) = -1
                ENDIF
            ENDIF
        ENDDO
        ! --------------------------

        RETURN
        END SUBROUTINE CHECK_EDGE_STATE
