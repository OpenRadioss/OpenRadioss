Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CHECK_SURFACE_STATE           source/interfaces/interf/check_surface_state.F
Chd|-- called by -----------
Chd|        FIND_SURFACE_FROM_REMOTE_PROC source/interfaces/interf/find_surface_from_remote_proc.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CHECK_ACTIVE_ELEM_EDGE        source/interfaces/interf/check_active_elem_edge.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE CHECK_SURFACE_STATE( ITASK,SURFARCE_NB,SURFACE_ID,SHIFT_INTERFACE,INTBUF_TAB,
     .                                  IPARI,GEO,
     .                                  IXS,IXC,IXT,IXP,IXR,IXTG,
     .                                  ADDCNEL,CNEL,TAG_NODE,TAG_ELEM,SHOOT_STRUCT )
!$COMMENT
!       CHECK_SURFACE_STATE description
!           check the state of an surface (active or not)
!       CHECK_SURFACE_STATE organization
!           loop over a list of surface :
!           -check if 1 or more element associated to the surface is/are active
!           - if there is no active element, the surface is deactivate
!           - additional treatment for interface type 24 & 25 : save the list of deactivated surface
!             --> need to send them to remote proc for neighbouring surface deactivation
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
        INTEGER, INTENT(in) :: SURFARCE_NB  ! number of local deactivated surface
        INTEGER, DIMENSION(SURFARCE_NB), INTENT(in) :: SURFACE_ID  ! id of surface that need to be deactivated
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
        LOGICAL :: DEACTIVATION,ACTIVATION,TYPE_INTER
        INTEGER :: I,K,FIRST,LAST
        INTEGER :: NIN,ID_INTER,NUMBER_INTER
        INTEGER :: ITY,IDEL
        INTEGER :: N1,N2,N3,N4
        INTEGER :: NUMBER_NODE
        INTEGER :: DICHOTOMIC_SEARCH_I_ASC  ! function
        INTEGER :: MY_REAL_TASK_ID ! real task id : itask is used in a strange way here : -1 if there is no omp // (in the mpi comm)
        INTEGER, DIMENSION(:), ALLOCATABLE :: NUMBER_SAVED_SURFACE_TYP24_25 ! number of deactivated surface for interface type 24 or 25
        INTEGER, DIMENSION(SURFARCE_NB) :: SAVED_SURFACE_TYP24_25 ! list of deactivated surface for interface type 24 or 25
        SAVE NUMBER_SAVED_SURFACE_TYP24_25
        INTEGER :: OFFSET_TASK ! offset to point to the right place in the array SHOOT_STRUCT%REMOTE_SURF
        INTEGER :: LOW_BOUND,UP_BOUND ! bounds
        INTEGER :: TOTAL_NUMBER_SURFACE ! total number of deleted surface
        INTEGER, DIMENSION(:), ALLOCATABLE :: SAVED_SURFACE ! temporary array 
        INTEGER :: NB_CONNECTED_ELM ! number of element connected to a sefment/surface
        INTEGER :: TOTAL_NUMBER_NEW_SURFACE ! total number of new active surface/
        INTEGER, DIMENSION(:), ALLOCATABLE :: NUMBER_NEW_SURFACE ! number of new active surface
        SAVE NUMBER_NEW_SURFACE
        INTEGER, DIMENSION(SURFARCE_NB) :: NEW_SURFACE ! list of new surface
C-----------------------------------------------
        IF(ITASK==-1) THEN
          FIRST = 1
          LAST = SURFARCE_NB
          MY_REAL_TASK_ID = 1
        ELSE
          FIRST = 1 + ITASK * (SURFARCE_NB / NTHREAD)
          LAST = (ITASK + 1) * (SURFARCE_NB / NTHREAD)
          IF((ITASK+1)==NTHREAD) LAST = SURFARCE_NB
          MY_REAL_TASK_ID = ITASK+1
        ENDIF
        IF(MY_REAL_TASK_ID==1) THEN
          ALLOCATE( NUMBER_SAVED_SURFACE_TYP24_25(NTHREAD) )
          ALLOCATE( NUMBER_NEW_SURFACE(NTHREAD) )
        ENDIF
        IF(ITASK/=-1) CALL MY_BARRIER( )
        NUMBER_INTER = SHIFT_INTERFACE(NINTER+1,2)
        NUMBER_SAVED_SURFACE_TYP24_25(MY_REAL_TASK_ID) = 0
        NUMBER_NEW_SURFACE(MY_REAL_TASK_ID) = 0
        ! --------------------------
        ! loop over the deactivated surface
        DO I=FIRST,LAST
          K = SURFACE_ID(I)  ! get the global surface id
          ID_INTER = DICHOTOMIC_SEARCH_I_ASC(K, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
          NIN = SHIFT_INTERFACE(ID_INTER,2)
          K = K - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface
          ITY = IPARI(7,NIN)
          IDEL = IPARI(17,NIN)
          ACTIVATION =.FALSE.
          TYPE_INTER = (ITY==7.OR.ITY==10.OR.ITY==22.OR.ITY==24.OR.(IPARI(100,NIN)==0.AND.ITY==25))

          ! --------------
          IF(ITASK==-1) THEN
            IF((TYPE_INTER.AND.IDEL==1).OR.(IPARI(100,NIN)/=0.AND.ITY==25)) THEN
              SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(K) = SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(K) - 1
            ENDIF
          ENDIF
          ! --------------

          ! --------------
          IF(IPARI(100,NIN)/=0.AND.ITY==25) THEN
            SHOOT_STRUCT%INTER(NIN)%NB_ELM_M(K) = SHOOT_STRUCT%INTER(NIN)%NB_ELM_M(K) - 1
          ENDIF
          ! --------------

          ! --------------
          DEACTIVATION = .FALSE.
          IF((TYPE_INTER.AND.IDEL==1).OR.(IPARI(100,NIN)/=0.AND.ITY==25)) THEN
            N1 = INTBUF_TAB(NIN)%IRECTM((K-1)*4+1)
            N2 = INTBUF_TAB(NIN)%IRECTM((K-1)*4+2)
            N3 = INTBUF_TAB(NIN)%IRECTM((K-1)*4+3)
            N4 = INTBUF_TAB(NIN)%IRECTM((K-1)*4+4)
            NUMBER_NODE = 4
            IF(N3==N4) NUMBER_NODE = 3
            IF(SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(K)<1) THEN
              CALL CHECK_ACTIVE_ELEM_EDGE( NUMBER_NODE, N1,N2,N3,N4,
     .                                     DEACTIVATION,GEO,IXS,IXC,
     .                                     IXT,IXP,IXR,IXTG,ADDCNEL,CNEL,
     .                                     TAG_NODE,TAG_ELEM)
            ELSE
              DEACTIVATION = .FALSE.
            ENDIF
          ELSEIF(TYPE_INTER.AND.IDEL==2) THEN
              DEACTIVATION = .TRUE.
          ENDIF
          IF(.NOT.DEACTIVATION.AND.(IPARI(100,NIN)/=0.AND.ITY==25)) THEN
            NB_CONNECTED_ELM = SHOOT_STRUCT%INTER(NIN)%NB_ELM_M(K)
            ! ---------
            IF((NB_CONNECTED_ELM<1).AND.(INTBUF_TAB(NIN)%STFM(k)>ZERO)) THEN
              DEACTIVATION=.TRUE.
            ELSEIF(NB_CONNECTED_ELM==1.AND.(INTBUF_TAB(NIN)%STFM(k)<ZERO)) THEN
              ACTIVATION = .TRUE.
            ENDIF
            ! ---------
          ENDIF
          ! --------------

          ! --------------
          ! check if the surface is active, if yes --> deactivate it
          IF(DEACTIVATION) THEN
            INTBUF_TAB(NIN)%STFM(K)  = ZERO
            IF(ITY==24.OR.ITY==25) THEN
              NUMBER_SAVED_SURFACE_TYP24_25(MY_REAL_TASK_ID) = NUMBER_SAVED_SURFACE_TYP24_25(MY_REAL_TASK_ID) + 1
              SAVED_SURFACE_TYP24_25(NUMBER_SAVED_SURFACE_TYP24_25(MY_REAL_TASK_ID)) = SURFACE_ID(I)
            ENDIF
          ENDIF
          ! --------------

          ! --------------
          ! the surface was not active, need to active it 
          IF(ACTIVATION) THEN
            INTBUF_TAB(NIN)%STFM(K) = ABS(INTBUF_TAB(NIN)%STFM(K))
            NUMBER_NEW_SURFACE(MY_REAL_TASK_ID) = NUMBER_NEW_SURFACE(MY_REAL_TASK_ID) + 1
            NEW_SURFACE(NUMBER_NEW_SURFACE(MY_REAL_TASK_ID)) = SURFACE_ID(I)
          ENDIF
          ! --------------
        ENDDO
        ! --------------------------
       
        ! --------------------------
        ! Only for interface type 24 & 25 :
        ! need to save the surface id and send it to a remote processor
        ! the remote processor will also deactivated the surface for a neighbourhood question
        OFFSET_TASK = SHOOT_STRUCT%NUMBER_REMOTE_SURF
        IF(ITASK/=-1) THEN
          CALL MY_BARRIER()
          IF(ITASK>0) THEN
              DO I=1,ITASK
                OFFSET_TASK = OFFSET_TASK + NUMBER_SAVED_SURFACE_TYP24_25(I)
              ENDDO
          ENDIF
          TOTAL_NUMBER_SURFACE = 0
          DO I=1,NTHREAD
            TOTAL_NUMBER_SURFACE = TOTAL_NUMBER_SURFACE +  NUMBER_SAVED_SURFACE_TYP24_25(I)
          ENDDO
        ELSE
          TOTAL_NUMBER_SURFACE = NUMBER_SAVED_SURFACE_TYP24_25(MY_REAL_TASK_ID)        
        ENDIF
        ! --------------------------
        IF(TOTAL_NUMBER_SURFACE>0) THEN
          IF(MY_REAL_TASK_ID==1) THEN
            IF( TOTAL_NUMBER_SURFACE+SHOOT_STRUCT%NUMBER_REMOTE_SURF > SHOOT_STRUCT%SIZE_REMOTE_SURF) THEN
              ALLOCATE( SAVED_SURFACE(SHOOT_STRUCT%NUMBER_REMOTE_SURF) )
              SAVED_SURFACE(1:SHOOT_STRUCT%NUMBER_REMOTE_SURF) = SHOOT_STRUCT%REMOTE_SURF(1:SHOOT_STRUCT%NUMBER_REMOTE_SURF)
              DEALLOCATE( SHOOT_STRUCT%REMOTE_SURF )
              SHOOT_STRUCT%SIZE_REMOTE_SURF = TOTAL_NUMBER_SURFACE+SHOOT_STRUCT%NUMBER_REMOTE_SURF
              ALLOCATE( SHOOT_STRUCT%REMOTE_SURF(SHOOT_STRUCT%SIZE_REMOTE_SURF) )
              SHOOT_STRUCT%REMOTE_SURF(1:SHOOT_STRUCT%NUMBER_REMOTE_SURF) = SAVED_SURFACE(1:SHOOT_STRUCT%NUMBER_REMOTE_SURF)
              DEALLOCATE( SAVED_SURFACE )
            ENDIF
          ENDIF
          IF(ITASK/=-1) CALL MY_BARRIER()
          LOW_BOUND = 1
          UP_BOUND = NUMBER_SAVED_SURFACE_TYP24_25(MY_REAL_TASK_ID)
          SHOOT_STRUCT%REMOTE_SURF(LOW_BOUND+OFFSET_TASK:UP_BOUND+OFFSET_TASK) = SAVED_SURFACE_TYP24_25(LOW_BOUND:UP_BOUND)
          IF(ITASK/=-1) CALL MY_BARRIER()
          IF(MY_REAL_TASK_ID==1) SHOOT_STRUCT%NUMBER_REMOTE_SURF = SHOOT_STRUCT%NUMBER_REMOTE_SURF + TOTAL_NUMBER_SURFACE
        ENDIF
        ! --------------------------

        ! --------------------------
        ! Only for interface type 25 :
        ! need to save the new surface id to find the new neighbours
        OFFSET_TASK = SHOOT_STRUCT%NUMBER_NEW_SURF
        IF(ITASK/=-1) THEN
          CALL MY_BARRIER()
          IF(ITASK>0) THEN
            DO I=1,ITASK
              OFFSET_TASK = OFFSET_TASK + NUMBER_NEW_SURFACE(I)
            ENDDO
          ENDIF
          TOTAL_NUMBER_NEW_SURFACE = 0
          DO I=1,NTHREAD
            TOTAL_NUMBER_NEW_SURFACE = TOTAL_NUMBER_NEW_SURFACE +  NUMBER_NEW_SURFACE(I)
          ENDDO
        ELSE
          TOTAL_NUMBER_NEW_SURFACE = NUMBER_NEW_SURFACE(MY_REAL_TASK_ID)        
        ENDIF
        ! --------------------------
        IF(TOTAL_NUMBER_NEW_SURFACE>0) THEN
          IF(MY_REAL_TASK_ID==1) THEN
            IF( TOTAL_NUMBER_NEW_SURFACE+SHOOT_STRUCT%NUMBER_NEW_SURF > SHOOT_STRUCT%SIZE_NEW_SURF) THEN
              ALLOCATE( SAVED_SURFACE(SHOOT_STRUCT%NUMBER_NEW_SURF) )
              SAVED_SURFACE(1:SHOOT_STRUCT%NUMBER_NEW_SURF) = SHOOT_STRUCT%NEW_SURF(1:SHOOT_STRUCT%NUMBER_NEW_SURF)
              DEALLOCATE( SHOOT_STRUCT%NEW_SURF )
              SHOOT_STRUCT%SIZE_NEW_SURF = TOTAL_NUMBER_NEW_SURFACE+SHOOT_STRUCT%NUMBER_NEW_SURF
              ALLOCATE( SHOOT_STRUCT%NEW_SURF(SHOOT_STRUCT%SIZE_NEW_SURF) )
              SHOOT_STRUCT%NEW_SURF(1:SHOOT_STRUCT%NUMBER_NEW_SURF) = SAVED_SURFACE(1:SHOOT_STRUCT%NUMBER_NEW_SURF)
              DEALLOCATE( SAVED_SURFACE )
            ENDIF
          ENDIF
          IF(ITASK/=-1) CALL MY_BARRIER()
          LOW_BOUND = 1
          UP_BOUND = NUMBER_NEW_SURFACE(MY_REAL_TASK_ID)
          SHOOT_STRUCT%NEW_SURF(LOW_BOUND+OFFSET_TASK:UP_BOUND+OFFSET_TASK) = NEW_SURFACE(LOW_BOUND:UP_BOUND)
          IF(ITASK/=-1) CALL MY_BARRIER()
          IF(MY_REAL_TASK_ID==1) SHOOT_STRUCT%NUMBER_NEW_SURF = SHOOT_STRUCT%NUMBER_NEW_SURF + TOTAL_NUMBER_NEW_SURFACE
        ENDIF
        ! --------------------------


        IF(ITASK/=-1) CALL MY_BARRIER( )

        IF(MY_REAL_TASK_ID==1) THEN
          DEALLOCATE( NUMBER_SAVED_SURFACE_TYP24_25 )
          DEALLOCATE( NUMBER_NEW_SURFACE )
        ENDIF

        RETURN
        END SUBROUTINE CHECK_SURFACE_STATE
