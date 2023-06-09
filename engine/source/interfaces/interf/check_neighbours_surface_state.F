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
        SUBROUTINE CHECK_NEIGHBOURS_SURFACE_STATE( SURFARCE_NB  ,SURFACE_ID  ,SHIFT_INTERFACE ,INTBUF_TAB ,
     .                                             IPARI        ,IAD_ELEM    ,SHOOT_STRUCT    ,X          ,
     .                                             IXS          ,ITAB        ,ITABM1          ,NBINTC     ,
     .                                             INTLIST      ,NEWFRONT    ,SPMD_ARRAYS     ,NBDDEDGT   ,
     .                                             NBDDEDG_MAX )
!$COMMENT
!       CHECK_NEIGHBOURS_SURFACE_STATE description
!          Erosion of solid int25
!          After deactivation of surfaces from an interface :
!          Reorient new external segments 
!          rebuid  list of neigbhoors of each main segment MVOISIN
!          rebuid list of frontier edges for spmd comms 
!       CHECK_NEIGHBOURS_SURFACE_STATE organization
!$ENDCOMMENT
        USE INTBUFDEF_MOD
        USE SHOOTING_NODE_MOD
        USE ARRAY_MOD
        USE SPMD_ARRAYS_MOD
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: SURFARCE_NB  ! number of local deactivated surface
        INTEGER, DIMENSION(SURFARCE_NB), INTENT(in) :: SURFACE_ID  ! id of surface that need to be deactivated
        INTEGER, DIMENSION(NINTER+1,2), INTENT(in) :: SHIFT_INTERFACE ! interface shift
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data 
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM ! index for frontier elements
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo   
        INTEGER, INTENT(in) :: ITABM1(NUMNOD)    ! array of global id
        INTEGER, INTENT(in) :: ITAB(NUMNOD)    ! array of global id
        INTEGER, INTENT(in) :: IXS(NIXS,NUMELS) ! array of solid nodes
        INTEGER, INTENT(in) :: NBINTC ! number of inter
        INTEGER, INTENT(in) :: INTLIST(NBINTC) ! list of inter
        my_real, INTENT(in) :: X(3,NUMNOD)  ! coordinates of nodes
        INTEGER, DIMENSION(NINTER), INTENT(inout) :: NEWFRONT   ! array for sorting : 1 --> need to sort the interface NIN
        INTEGER, INTENT(inout) :: NBDDEDGT,NBDDEDG_MAX ! number of frontier edges
        TYPE(spmd_arrays_), INTENT(inout) :: SPMD_ARRAYS ! structure for interface spmd arrays

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,K,J,IJK,FIRST,LAST
        INTEGER :: NIN,ID_INTER,NRTM,IDEL25_SOLID ! related to the surface : interface id, number of surface...
        INTEGER :: ITY,IDEL,IOK
        INTEGER :: N1,N2,N3,N4,IEDG,NN ,NEL,IEL,IEL1,IEL2,IEL3,NN1,NN2,JJ,KK,N,NI
        INTEGER :: IY(4)
        INTEGER :: NODE_ID
        INTEGER :: SHIFT, NUMBER_INTER
        INTEGER :: DICHOTOMIC_SEARCH_I_ASC  ! function
        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: S_BUFFER
        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: R_BUFFER
        
        INTEGER :: GLOCAL_SURFACE_ID ! global surface id
        INTEGER :: PROC_ID,REMOTE_PROC,ADRESS_PROC ! processor id and remote processor id
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
        INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG,ITAGM,IDX

        INTEGER :: NB_RESULT_INTERSECT,NB_SURF1,NB_SURF2,NB_PROC1,NB_PROC2,NB_RESULT_INTERSECT_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT,INTERSECT_1,INTERSECT_2,RESULT_INTERSECT_2,INTERSECT_3,INTERSECT_4
        INTEGER :: EDGE_N1, EDGE_N2, ID, INEDG, ISURF, NTY, ISEARCH
        INTEGER :: NI25, LSHIFT, NBDDEDG,MVOISIN_CHANGE, P
        INTEGER :: ISOM(NSPMD)
        LOGICAL  FOUND        

        my_real  XS1 ,YS1 ,ZS1 ,XC ,YC ,ZC ,DDS, AREA, XX13, YY13, ZZ13, 
     .        XX24, YY24, ZZ24, NOR1, NOR2, NOR3
        my_real  XX1(4) ,XX2(4) ,XX3(4) ,XX4(4)        
#ifdef MPI
        INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS_MPI
        INTEGER, DIMENSION(MPI_STATUS_SIZE,NSPMD) :: ARRAY_STATUSES
#endif
        DATA MSGOFF1/13014/
        DATA MSGOFF2/13015/

        INTEGER, DIMENSION(:,:), ALLOCATABLE :: FR_SAV  !(2,NBDDEDG_MAX)
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX25  !(2*NBDDEDG_MAX)
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI25  !(3,NBDDEDG_MAX)
        INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_REM25 
        INTEGER, DIMENSION(:), ALLOCATABLE :: WORK  !(70000)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER SYSFUS2 
C-----------------------------------------------
        FIRST = 1
        LAST = SURFARCE_NB 
        NUMBER_INTER = SHIFT_INTERFACE(NINTER+1,2)

C-----------------------------------------------------------
C    Reorientation of segments internal becoming external
C---------------------------------------------------------
        ! --------------------------
        ! loop over the internal becoming external surfaces
        DO I=FIRST,LAST
            K = SURFACE_ID(I)  ! get the global surface id
            ID_INTER = DICHOTOMIC_SEARCH_I_ASC(K, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
            NIN = SHIFT_INTERFACE(ID_INTER,2)
            K = K - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface

 ! if new external surfaces : need to sort 

            NEWFRONT(NIN) = -1  ! force the sorting of the interface NIN for the next cycle

C-----------------------------------------------
C     ORIENTATION CHECKING 
C-----------------------------------------------

        ! Normal to the segment
            XS1=ZERO 
            YS1=ZERO 
            ZS1=ZERO
            DO  JJ=1,4
               NN=INTBUF_TAB(NIN)%IRECTM(4*(K-1)+JJ)
               IY(JJ)=NN
               XX1(JJ)=X(1,NN)
               XX2(JJ)=X(2,NN)
               XX3(JJ)=X(3,NN)
               XS1=XS1+FOURTH*X(1,NN)
               YS1=YS1+FOURTH*X(2,NN)
               ZS1=ZS1+FOURTH*X(3,NN)   
            ENDDO    
C
            XX13 =XX1(3)-XX1(1)
            YY13 =XX2(3)-XX2(1)
            ZZ13 =XX3(3)-XX3(1)
            XX24 =XX1(4)-XX1(2)
            YY24 =XX2(4)-XX2(2)
            ZZ24 =XX3(4)-XX3(2)
C
            NOR1=YY13*ZZ24-ZZ13*YY24
            NOR2=ZZ13*XX24-XX13*ZZ24
            NOR3=XX13*YY24-YY13*XX24
            AREA= MAX(EM20,SQRT(NOR1*NOR1+NOR2*NOR2+NOR3*NOR3))
            NOR1=NOR1/AREA
            NOR2=NOR2/AREA
            NOR3=NOR3/AREA

C
        ! Orientation of the element

            NEL = INTBUF_TAB(NIN)%IELEM_M(2*(K-1)+1)
            XC=0.
            YC=0.
            ZC=0.
            DO J=1,8
               KK=IXS(J+1,NEL)
               XC=XC+X(1,KK)
               YC=YC+X(2,KK)
               ZC=ZC+X(3,KK)
            ENDDO
            XC=XC*ONE_OVER_8
            YC=YC*ONE_OVER_8
            ZC=ZC*ONE_OVER_8

             DDS=NOR1*(XC-XS1)+NOR2*(YC-YS1)+NOR3*(ZC-ZS1)

             IF(DDS  >= ZERO) THEN
                IF(IY(3)==IY(4)) THEN
                    INTBUF_TAB(NIN)%IRECTM(4*(K-1)+1)=IY(2)
                    INTBUF_TAB(NIN)%IRECTM(4*(K-1)+2)=IY(1)
                ELSE
                   DO KK=1,4
                      INTBUF_TAB(NIN)%IRECTM(4*(K-1)+KK)=IY(4-KK+1)
                   ENDDO
                ENDIF
            ENDIF
        ENDDO
C-----------------------------------------------------------
C    Neighbours searching : Mvoisin updating
C---------------------------------------------------------

        ALLOCATE( S_BUFFER(NSPMD), R_BUFFER(NSPMD) )
        S_BUFFER(1:NSPMD)%SIZE_INT_ARRAY_2D(1) = 2
        DO I=1,NSPMD
            S_BUFFER(I)%SIZE_INT_ARRAY_2D(2) = 4*SHOOT_STRUCT%MAX_SURF_NB*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        ENDDO
        NUMBER_REMOTE_SURF(1:NSPMD) = 0

        MVOISIN_CHANGE = 0

        ALLOCATE( ITAG(NUMNOD)) 
        ITAG(1:NUMNOD) = 0

        IF(NSPMD > 1) THEN
            ALLOCATE( RESULT_INTERSECT_2( MAX(SHOOT_STRUCT%MAX_PROC_NB,1 ) ) )
            ALLOCATE( INTERSECT_3( SHOOT_STRUCT%MAX_PROC_NB ) )
            ALLOCATE( INTERSECT_4( SHOOT_STRUCT%MAX_PROC_NB ) )
        ENDIF


        DO NI =1,NBINTC
            NIN = INTLIST(NI)
            NTY=IPARI(7,NIN)
            IDEL25_SOLID = IPARI(100,NIN)
            NRTM = IPARI(4,NIN)
            IF(NTY==25.AND.IDEL25_SOLID > 0) THEN
               ALLOCATE( ITAGM(NRTM)) 
               ALLOCATE( IDX(NRTM)) 
               ITAGM(1:NRTM) = 0
               IDX(1:NRTM) = 0
               DO I=1,NRTM
                  IF(INTBUF_TAB(NIN)%IELEM_M(2*(I-1)+1) <= NUMELS.AND.INTBUF_TAB(NIN)%STFM(I) > ZERO) THEN
                    DO J=1,4
                        IF (INTBUF_TAB(NIN)%MVOISIN(4*(I-1)+J)==0) THEN
                          MVOISIN_CHANGE = 1 ! flag is ON if new Mvoisin is on another proc : need to update spmd arrays


                            IF(.NOT.(INTBUF_TAB(NIN)%IRECTM(4*(I-1)+4)==INTBUF_TAB(NIN)%IRECTM(4*(I-1)+3).AND.J==3))THEN
                                N1 = INTBUF_TAB(NIN)%IRECTM(4*(I-1)+J)
                                N2 = INTBUF_TAB(NIN)%IRECTM(4*(I-1)+MOD(J,4)+1) 
C
                                IEL = INTBUF_TAB(NIN)%IELEM_M(2*(I-1)+1)
                                IOK = 0
                                ISEARCH = 0
                                DO WHILE(IOK == 0.AND.IEL > 0)
                                  FOUND = .FALSE.
                                  K = SHOOT_STRUCT%SHIFT_M_ELEM_SURF(IEL)+1
                                  IEL3 = 0
                                  DO WHILE(.NOT.FOUND.AND.K <= SHOOT_STRUCT%SHIFT_M_ELEM_SURF(IEL+1))
                                    ISURF = SHOOT_STRUCT%M_ELEM_SURF (K)  ! get the global surface id
                                    ID_INTER = DICHOTOMIC_SEARCH_I_ASC(ISURF, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
                                    N = SHIFT_INTERFACE(ID_INTER,2)
                                    ISURF = ISURF - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface  
                                    IF(N==NIN.AND.ISURF /=I.AND.ITAGM(ISURF)==0) THEN    
                                       DO ID=1,4
                                         ITAG(INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+ID))  = 1      
                                       ENDDO
                                       IF(ITAG(N1)==1.AND.ITAG(N2)==1) THEN
                                         IF(INTBUF_TAB(NIN)%STFM(ISURF) > 0) THEN
                                           IOK =1
                                           DO ID=1,4
                                             IF(.NOT.(INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+4)==
     .                                           INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+3).AND.ID==3))THEN
                                                NN1= INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+ID)
                                                NN2= INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+MOD(ID,4)+1)
                                                IF((NN1==N1.AND.NN2==N2).OR.(NN1==N2.AND.NN2==N1)) THEN
                                                   IEDG = ID
                                                ENDIF  
                                              ENDIF 
                                           ENDDO
                                         ELSE
                                            IEL1 = INTBUF_TAB(NIN)%IELEM_M(2*(ISURF-1)+1)
                                            IEL2 = INTBUF_TAB(NIN)%IELEM_M(2*(ISURF-1)+2)
                                            IF(IEL1==IEL) THEN
                                               IEL3 = IEL2
                                            ELSEIF(IEL2==IEL) THEN
                                               IEL3 = IEL1
                                            ENDIF
                                         ENDIF
                                         FOUND = .TRUE.
                                         ISEARCH = ISEARCH + 1
                                         IDX(ISEARCH) = ISURF
                                         ITAGM(ISURF)  = 1
                                       ENDIF 
                                       DO ID=1,4
                                        ITAG(INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+ID))  = 0      
                                       ENDDO 
                                    ENDIF 
                                    K= K+1                                    
                                  ENDDO
                                  IF(IEL3 /=0) IEL = IEL3
                                ENDDO   
                                DO ID = 1,ISEARCH
                                    ITAGM(IDX(ID))  = 0
                                ENDDO
                                IF(IOK==0.AND.IEL > 0) write(*,*) 'problem finding mvoisin'
                                IF(IOK > 0) THEN
                                   INTBUF_TAB(NIN)%MVOISIN(4*(I-1)+J) = ISURF
                                   INTBUF_TAB(NIN)%MVOISIN(4*(ISURF-1)+IEDG) = I
                                   INTBUF_TAB(NIN)%EVOISIN(4*(I-1)+J) = IEDG
                                   INTBUF_TAB(NIN)%EVOISIN(4*(ISURF-1)+IEDG) = J       
                                   INTBUF_TAB(NIN)%PROC_MVOISIN(4*(I-1)+J) = ISPMD +1     
                                   INTBUF_TAB(NIN)%PROC_MVOISIN(4*(ISURF-1)+IEDG) = ISPMD +1     
                                ELSEIF(IEL <0) THEN ! Voisin remote : comm spmd  => Not possible 

                                    NB_PROC1 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1)
                                    NB_PROC2 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2) 

                                    IF(NB_PROC1 <= 1.OR. NB_PROC2 <= 1)   write(*,*) 'nrtm non remote iel <0'

                                    IF(NSPMD>1) THEN            
                                        ! --------------
                                        IF(NB_PROC1>1.AND.NB_PROC2>1) THEN                                            ! -----------------------         
                                            ! intersection of processor 
                                            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1)
                                            INTERSECT_3(1:NB_PROC1) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC1 )

                                            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N2)
                                            INTERSECT_4(1:NB_PROC2) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC2 )
                
                                            CALL INTERSECT_2_SORTED_SETS( INTERSECT_3,NB_PROC1,
     .                                                    INTERSECT_4,NB_PROC2,
     .                                                  RESULT_INTERSECT_2,NB_RESULT_INTERSECT_2 )
                                        ELSE
                                            write(*,*) 'nrtm non remote iel <0'
                                        ENDIF

                                        ALREADY_DONE(1:NSPMD) = .FALSE.
                                        ALREADY_DONE(ISPMD+1) = .TRUE.
                                        IF(NB_RESULT_INTERSECT_2>1) THEN
c                                            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(N1)
                                            DO IJK=1,NB_RESULT_INTERSECT_2
                                                REMOTE_PROC = RESULT_INTERSECT_2(IJK) !SHOOT_STRUCT%M_NODE_PROC( SHIFT+IJK )
                                                IF(.NOT.ALREADY_DONE(REMOTE_PROC) ) THEN
                                                    ALREADY_DONE(REMOTE_PROC) = .TRUE.
                                                    IF(.NOT.ALLOCATED( S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D ) ) THEN
                                                        CALL ALLOC_2D_ARRAY(S_BUFFER(REMOTE_PROC))
                                                    ENDIF
                                                    NUMBER_REMOTE_SURF(REMOTE_PROC) = NUMBER_REMOTE_SURF(REMOTE_PROC) + 1
                                                    ADRESS_PROC = 4*(NUMBER_REMOTE_SURF(REMOTE_PROC)-1) +1
                                                    S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(1,ADRESS_PROC) = -INTBUF_TAB(NIN)%MSEGLO(I)
                                                    S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(1,ADRESS_PROC+1) = ITAB(N1) 
                                                    S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(1,ADRESS_PROC+2) = ITAB(N2)   
                                                    S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(1,ADRESS_PROC+3) = J  
                                                    S_BUFFER(REMOTE_PROC)%INT_ARRAY_2D(2,ADRESS_PROC) = NIN
                                                ENDIF
                                                
                                            ENDDO
                                        ENDIF
                                    ENDIF
                                ENDIF
                            ENDIF
                        ENDIF
c
                    ENDDO
                  ENDIF

               ENDDO
               DEALLOCATE( ITAGM,IDX) 
            ENDIF
        ENDDO
c        IF(NCYCLE==463) PAUSE


        IF(NSPMD>1) THEN
        ! --------------------------
        ! working array : surface
           ALLOCATE( RESULT_INTERSECT( SHOOT_STRUCT%MAX_SURF_NB ) )
           ALLOCATE( INTERSECT_1( SHOOT_STRUCT%MAX_SURF_NB ) )
           ALLOCATE( INTERSECT_2( SHOOT_STRUCT%MAX_SURF_NB ) )

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
                R_BUFFER(PROC_ID)%SIZE_INT_ARRAY_2D(2) = 4*NUMBER_REMOTE_SURF_R(PROC_ID)
                CALL ALLOC_2D_ARRAY(R_BUFFER(PROC_ID)) 
                SIZE_R = R_BUFFER(PROC_ID)%SIZE_INT_ARRAY_2D(1) * R_BUFFER(PROC_ID)%SIZE_INT_ARRAY_2D(2)
                MSGTYP = MSGOFF2
                CALL MPI_IRECV(R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,1),SIZE_R,
     .                  MPI_INTEGER,IT_SPMD(PROC_ID),MSGTYP,
     .                  MPI_COMM_WORLD,REQUEST_SIZE_R_2(RECV_NB_2),IERROR )
             ENDIF

          ENDDO

        ! ----------------
        ! send the data : "list of deleted surface of interface type 24 or 25"
          DO I=1,NSPMD
            IF(NUMBER_REMOTE_SURF(I)>0) THEN
                MSGTYP = MSGOFF2
                SIZE_S = 4*NUMBER_REMOTE_SURF(I) * S_BUFFER(I)%SIZE_INT_ARRAY_2D(1)
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
                NIN = R_BUFFER(PROC_ID)%INT_ARRAY_2D(2,4*(J-1)+1) ! get the interface id
                ITY = IPARI(7,NIN)  ! get the type of interface
                IDEL = IPARI(17,NIN) ! get the kind of idel (1 or 2)
                NRTM = IPARI(4,NIN)     ! get the number of surfaces of the interface NIN
                ! --------------
                GLOCAL_SURFACE_ID = R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,4*(J-1)+1) ! get the global deleted surface id
                EDGE_N1 = R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,4*(J-1)+2) ! get the global node of common edge surface id
                EDGE_N2 = R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,4*(J-1)+3) ! get the global node of common edge surface id
                INEDG = R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,4*(J-1)+4) ! get the edge id of the surface
                
                N1 = SYSFUS2(EDGE_N1,ITABM1,NUMNOD)
                N2 = SYSFUS2(EDGE_N2,ITABM1,NUMNOD)

                NB_SURF1 = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N1+1) - SHOOT_STRUCT%SHIFT_M_NODE_SURF(N1)   ! get the number of surface of the node
                SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N1)
                INTERSECT_1(1:NB_SURF1) = SHOOT_STRUCT%M_NODE_SURF( SHIFT+1:SHIFT+NB_SURF1 )

                NB_SURF2 = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N2+1) - SHOOT_STRUCT%SHIFT_M_NODE_SURF(N2)   ! get the number of surface of the node
                SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(N2)
                INTERSECT_2(1:NB_SURF2) = SHOOT_STRUCT%M_NODE_SURF( SHIFT+1:SHIFT+NB_SURF2 )


                IF(NB_SURF1>0.AND. NB_SURF2>0) THEN
                   CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_SURF1,
     .                                           INTERSECT_2,NB_SURF2,
     .                                           RESULT_INTERSECT,NB_RESULT_INTERSECT )
                ELSE
                   NB_RESULT_INTERSECT = 0
                ENDIF

                DO K=1,NB_RESULT_INTERSECT
                    ISURF = RESULT_INTERSECT(K)
                    ID_INTER = DICHOTOMIC_SEARCH_I_ASC(ISURF, SHIFT_INTERFACE(1,1), NUMBER_INTER+1) ! find the interface of the surface
                    N = SHIFT_INTERFACE(ID_INTER,2)
                    ISURF = ISURF - SHIFT_INTERFACE(ID_INTER,1) + 1 ! get the surface id in the NIN interface

                    IF(NIN==N) THEN
                      IF(INTBUF_TAB(NIN)%STFM(ISURF) > ZERO ) THEN ! neighboor external surfaces
                        IEDG = 0
                        DO ID=1,4
                            IF(.NOT.(INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+3)==INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+4).AND.ID==3))THEN
                                NN1= INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+ID)
                                NN2= INTBUF_TAB(NIN)%IRECTM(4*(ISURF-1)+MOD(ID,4)+1)
                                IF((NN1==N1.AND.NN2==N2).OR.(NN1==N2.AND.NN2==N1)) THEN
                                   IEDG = ID
                                ENDIF  
                            ENDIF 
                        ENDDO                                                                        
                        INTBUF_TAB(NIN)%MVOISIN(4*(ISURF-1)+IEDG) = R_BUFFER(PROC_ID)%INT_ARRAY_2D(1,4*(J-1)+1)
                        INTBUF_TAB(NIN)%EVOISIN(4*(ISURF-1)+IEDG) = INEDG
                        INTBUF_TAB(NIN)%PROC_MVOISIN(4*(ISURF-1)+IEDG) = PROC_ID
                      ENDIF
                    ENDIF
                ENDDO
                
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
#else   
          N = ITABM1(1)    ! unused instruction : just for compilation purpose (itabm1 have to be used)    
#endif

           DEALLOCATE( RESULT_INTERSECT )
           DEALLOCATE( INTERSECT_1 )
           DEALLOCATE( INTERSECT_2 )
        ENDIF

        DEALLOCATE( S_BUFFER, R_BUFFER )
        ! --------------------------
C-----------------------------------------------------------
C    Neighbours searching : Updating Frontier edges arrays after Mvoisin updating
C---------------------------------------------------------

        IF(NSPMD > 1) THEN  
        !     Frontiers vs edges   
        ! First calculate frontier edges
            NI25=0
            LSHIFT=0
            NBDDEDG_MAX = 0
            NBDDEDGT    = 0
            DO NI =1,NBINTC
               NIN = INTLIST(NI)
               NTY=IPARI(7,NIN)
               NRTM = IPARI(4,NIN)
               IF(NTY==25) THEN                   
                  NBDDEDG = 0           
                  NI25=NI25+1           
                  DO I = 1, NRTM  
                    IF(INTBUF_TAB(NIN)%STFM(I) /= ZERO ) THEN ! neighboor external surfaces      
                    DO J = 1,4
                       K = INTBUF_TAB(NIN)%MVOISIN(4*(I-1)+J)
                       IF(K/=0)THEN
                         P = INTBUF_TAB(NIN)%PROC_MVOISIN(4*(I-1)+J)
                         IF(P /= ISPMD+1) THEN
                           NBDDEDG = NBDDEDG + 1
                         ENDIF
                      END IF
                    ENDDO
                   ENDIF
                  ENDDO
                  NBDDEDG_MAX = MAX(NBDDEDG_MAX,NBDDEDG)
                  NBDDEDGT    = NBDDEDGT+NBDDEDG
                ENDIF
            ENDDO
            !     Frontiers vs edges  : update FR_EDG array 
            IF(ALLOCATED(SPMD_ARRAYS%FR_EDG))  DEALLOCATE(SPMD_ARRAYS%FR_EDG)
            ALLOCATE(SPMD_ARRAYS%FR_EDG(2*NBDDEDGT))
            ALLOCATE(FR_SAV(2,NBDDEDG_MAX))
            ALLOCATE(PROC_REM25(NBDDEDG_MAX))
            ALLOCATE(ITRI25(5,NBDDEDG_MAX))
            ALLOCATE(INDEX25(2*NBDDEDG_MAX))
            ALLOCATE(WORK(70000))

            SPMD_ARRAYS%IAD_FREDG(1:NINTER25*(NSPMD+1))=0

            NI25=0
            LSHIFT=0
            DO NI =1,NBINTC
                NIN = INTLIST(NI)
                NTY=IPARI(7,NIN)
                NRTM = IPARI(4,NIN)
                IF(NTY==25) THEN                   
                    NBDDEDG = 0           
                    NI25=NI25+1           
                    DO I = 1, NRTM  
                      IF(INTBUF_TAB(NIN)%STFM(I) /= ZERO ) THEN ! neighboor external surfaces
                        DO J = 1,4
                          K = INTBUF_TAB(NIN)%MVOISIN(4*(I-1)+J)
                          IF(K/=0)THEN
                             P = INTBUF_TAB(NIN)%PROC_MVOISIN(4*(I-1)+J)
                             IF(P /= ISPMD+1) THEN
                               NBDDEDG = NBDDEDG + 1
                               FR_SAV(1,NBDDEDG) = I ! Sorting 
                               FR_SAV(2,NBDDEDG) = J
                               PROC_REM25(NBDDEDG) = P
                               ! Sorting of edges frontiers : unique order
                               ITRI25(1,NBDDEDG) =  P

                               N1=INTBUF_TAB(NI)%IRECTM(4*(I-1)+J)
                               N2=INTBUF_TAB(NI)%IRECTM(4*(I-1)+MOD(J,4)+1)

                               ITRI25(2,NBDDEDG) =  MIN(-K,INTBUF_TAB(NIN)%MSEGLO(I))
                               ITRI25(3,NBDDEDG) =  MAX(-K,INTBUF_TAB(NIN)%MSEGLO(I))
                               ITRI25(4,NBDDEDG) =  MIN(ITAB(N1),ITAB(N2))
                               ITRI25(5,NBDDEDG) =  MAX(ITAB(N1),ITAB(N2))
                             ENDIF
                          END IF
                        ENDDO
                      ENDIF
                    ENDDO
C
                    DO I = 1, NBDDEDG
                        INDEX25(I) = I
                    ENDDO
                    CALL MY_ORDERS(0,WORK,ITRI25,INDEX25,NBDDEDG,5)
                    DO I = 1, NBDDEDG
                        PROC_REM25(I)      = ITRI25(1,INDEX25(I))
                        SPMD_ARRAYS%FR_EDG(2*(LSHIFT+I-1)+1) = FR_SAV(1,INDEX25(I))
                        SPMD_ARRAYS%FR_EDG(2*(LSHIFT+I-1)+2) = FR_SAV(2,INDEX25(I))
                    ENDDO
C
                    DO P = 1, NSPMD
                       ISOM(P) = 0
                    ENDDO
                    DO I = 1, NBDDEDG
                       P =  PROC_REM25(I)
                       ISOM(P) = ISOM(P) + 1
                    ENDDO
                     SPMD_ARRAYS%IAD_FREDG((NSPMD+1)*(NI25-1)+1) = LSHIFT + 1

                    DO P = 1, NSPMD
                        SPMD_ARRAYS%IAD_FREDG((NSPMD+1)*(NI25-1)+P+1) = SPMD_ARRAYS%IAD_FREDG((NSPMD+1)*(NI25-1)+P) + ISOM(P)
                    ENDDO           
                    LSHIFT=LSHIFT+NBDDEDG
                ENDIF
            END DO
            DEALLOCATE(FR_SAV)
        ENDIF

        RETURN
        END SUBROUTINE CHECK_NEIGHBOURS_SURFACE_STATE
