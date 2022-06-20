Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  INTER_PREPARE_SORT                    source/interfaces/intsort/generic/inter_prepare_sort.F
Chd|-- called by -----------
Chd|        INTTRI                         source/interfaces/intsort/inttri.F         
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE INTER_PREPARE_SORT( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,ISENDTO,IRECVFROM,
     .                                 IPARI,IAD_ELEM,FR_ELEM,X,V,
     .                                 MS,TEMP,KINET,NODNX_SMS,ITAB,
     .                                 WEIGHT,INTBUF_TAB,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ )

!$COMMENT
!       INTER_PREPARE_SORT description
!       first step of the sort : creation of coarse & fine grids
!       exchange of cell (coarse & fine grid) between processor

!       INTER_PREPARE_SORT organization :
!       * computation of the bounds 
!       * exchange & globalization of the bounds
!       * creation of a common grid (coarse & fine)
!       * coloration of coarse grid
!       * exchange of coarse cells to limit the amount of comm. between proc
!       * coloration of the fine grid
!       * exchange the number of local cell (fine grid)
!       * create the voxel of secondary nodes
!       * prepare the comm of remote secondary nodes
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE INTER_STRUCT_MOD
        USE INTER_SORTING_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK    !   omp thread ID
        INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
        INTEGER, INTENT(in) :: NODNX_SMS_SIZ
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
        INTEGER, DIMENSION(NINTER+1,NSPMD), INTENT(in) :: ISENDTO,IRECVFROM ! array for S and R : isendto = nsn ; ircvfrom = nmn
        INTEGER,DIMENSION(NPARI,NINTER), INTENT(inout) :: IPARI
        my_real, DIMENSION(3*NUMNOD), INTENT(in) :: X   !   position
        my_real, DIMENSION(3*NUMNOD), INTENT(in) :: V   !   velocity
        my_real, DIMENSION(NUMNOD), INTENT(in) :: MS,TEMP   !   mass & temperature
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: WEIGHT ! weight : 1 if current proc computes the node
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB   ! global node ID
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: KINET  ! k energy & 
        INTEGER, DIMENSION(NODNX_SMS_SIZ), INTENT(in) :: NODNX_SMS ! SMS array
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM     ! connectivity array iad(P+1)-iad(P) = nb of frontier node on P
        INTEGER, DIMENSION(SFR_ELEM), INTENT(in) :: FR_ELEM          ! frontier node ID


        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB   ! interface data
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   struture for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: TYPE18
        INTEGER :: KK,N
        INTEGER :: INACTI,NTY
        INTEGER :: SIZE_X
        INTEGER :: NB_REQUEST_COARSE_CELL   ! number of request for coarse cell comm
        INTEGER, DIMENSION(NB_INTER_SORTED) :: ARRAY_REQUEST_COARSE_CELL ! array of request
        INTEGER, DIMENSION(NB_INTER_SORTED) :: LIST_INTER_COARSE_CELL ! list of interface
!   ----------------------------------------

        ! -----------------------------
        ! compute the min/max position 
        IF(NB_INTER_SORTED>0) THEN
            ! ---------------------
            ! compute the curv and the number of main nodes
            ! and the min/max position of main nodes
            DO KK=1,NB_INTER_SORTED
                N = LIST_INTER_SORTED(KK)
                NTY = IPARI(7,N)
                INACTI = IPARI(22,N)        
                TYPE18 = .FALSE.
                IF(NTY == 7 .AND. INACTI ==7)TYPE18=.TRUE.
                CALL INTER_COUNT_NODE_CURV( N,ITASK,IPARI,INTBUF_TAB,
     1                                      X,WEIGHT,INTER_STRUCT)
            ENDDO
            ! ---------------------
            ! compute the min/max position (all nodes)
            IF(NEED_TO_SORT>0) THEN
                BOX_LIMIT(1:3) = -EP30
                BOX_LIMIT(4:6) = EP30
                CALL MY_BARRIER( )
                CALL INTER_MINMAX_NODE( ITASK,X )
            ENDIF
            ! ---------------------
        ENDIF
        ! -----------------------------

        ! -----------------------------
        ! globalization of min/max position 
        IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
            IF(ITASK==0) THEN
                CALL SPMD_BOX_LIMIT_REDUCTION(NB_INTER_SORTED)
            ENDIF
            CALL MY_BARRIER()
        ENDIF
        ! -----------------------------

        ! -----------------------------
        ! allocation & initialization of coarse_grid
        IF(ITASK==0) THEN
            IF(.NOT.ALLOCATED(LOCAL_COARSE_GRID)) THEN
                ALLOCATE(LOCAL_COARSE_GRID(NB_BOX_COARSE_GRID**3+1))
            ENDIF            
            LOCAL_COARSE_GRID(1:NB_BOX_COARSE_GRID**3+1) = 0
        ENDIF
        CALL MY_BARRIER()
        ! -----------------------------

        ! -----------------------------
        ! color the coarse grid
        IF(NB_INTER_SORTED>0) THEN
            IF(ITASK==0) CALL INTER_BOX_CREATION( )
            CALL MY_BARRIER()
            CALL INTER_COLOR_COARSE_VOXEL(ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,IPARI,INTBUF_TAB,
     .                                    X,SORT_COMM)
        ENDIF
        ! -----------------------------

        ! -----------------------------
        ! exchange of coarse_grid : send/rcv part
        IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
            IF(ITASK==0) THEN
                CALL SPMD_COARSE_CELL_EXCHANGE(NB_INTER_SORTED,LIST_INTER_SORTED,IRECVFROM,ISENDTO,1,
     .                       IPARI,SORT_COMM,NB_REQUEST_COARSE_CELL,ARRAY_REQUEST_COARSE_CELL,LIST_INTER_COARSE_CELL)
            ENDIF
        ENDIF
        ! -----------------------------

        ! -----------------------------
        ! color the voxel (fine grid)
        IF(NB_INTER_SORTED>0) THEN
            CALL MY_BARRIER()
            CALL INTER_COLOR_VOXEL( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,IPARI,INTBUF_TAB,
     .                              X,INTER_STRUCT,SORT_COMM )
        ENDIF
        ! -----------------------------

        ! -----------------------------
        ! exchange of coarse_grid : waiting part + check if 2 processors need to communicate
        IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
            IF(ITASK==0) THEN
                CALL SPMD_COARSE_CELL_EXCHANGE(NB_INTER_SORTED,LIST_INTER_SORTED,IRECVFROM,ISENDTO,2,
     .                       IPARI,SORT_COMM,NB_REQUEST_COARSE_CELL,ARRAY_REQUEST_COARSE_CELL,LIST_INTER_COARSE_CELL)
            ENDIF
        ENDIF
        CALL MY_BARRIER()
        ! -----------------------------

        ! -----------------------------
        ! exchange of number of colored cells (fine grid) + creation of voxel of secondary nodes
        DO KK=1,NB_INTER_SORTED
            N = LIST_INTER_SORTED(KK)
            ! -----------------------------
            ! exchange of number of colored cells (fine grid)
            IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
                IF(ITASK==0) THEN
                    CALL SPMD_CELL_LIST_EXCHANGE(IRECVFROM,ISENDTO,1,WEIGHT,IAD_ELEM,
     .                 FR_ELEM,X,V,MS,TEMP,
     .                 KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ)   !   size of cell list
                ENDIF
                CALL MY_BARRIER()
            ENDIF
            ! -----------------------------

            ! -----------------------------
            ! creation of voxel of secondary nodes 
            ! wait comm R "exchange of number of colored cells" & send/rcv the colored cells
            IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
                IF(ITASK==0) THEN
                    CALL INTER_VOXEL_CREATION(ITASK,IPARI,INTBUF_TAB,X,N,SORT_COMM)! create the voxel of secondary nodes
                    CALL SPMD_CELL_LIST_EXCHANGE(IRECVFROM,ISENDTO,2,WEIGHT,IAD_ELEM,
     .                 FR_ELEM,X,V,MS,TEMP,
     .                 KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ) !   mpi wait size of cell list

                ENDIF
                CALL MY_BARRIER()
            ENDIF
            ! -----------------------------
 
            ! -----------------------------
            ! wait comm R "send/rcv the colored cells" 
            ! and compute the number of secondary nodes needed by remote proc 
            !             + comm of "number of secondary nodes needed by remote proc"
            !             + creation of the secondary node list 
            !             + initialize the S buffer of secondary node data (x & v)
            IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
                IF(ITASK==0) THEN
                    CALL SPMD_CELL_LIST_EXCHANGE(IRECVFROM,ISENDTO,3,WEIGHT,IAD_ELEM,
     .                 FR_ELEM,X,V,MS,TEMP,
     .                 KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ)

                ENDIF
                CALL MY_BARRIER()
            ENDIF
            ! -----------------------------
        ENDDO
        ! -----------------------------

        ! -----------------------------
        ! wait comm S "send/rcv the colored cells"
        ! wait comm S/R "number of secondary nodes needed by remote proc"
        DO KK=1,NB_INTER_SORTED
            N = LIST_INTER_SORTED(KK)
            IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
                IF(ITASK==0) THEN
                    ! wait comm S "send/rcv the colored cells"
                    CALL SPMD_CELL_LIST_EXCHANGE(IRECVFROM,ISENDTO,4,WEIGHT,IAD_ELEM,
     .                 FR_ELEM,X,V,MS,TEMP,
     .                 KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ)
                    ! wait "number of secondary nodes needed by remote proc"
                    CALL SPMD_WAIT_NB(IRECVFROM,ISENDTO,N,SORT_COMM)    
                ENDIF
                CALL MY_BARRIER()
            ENDIF
        ENDDO
        ! -----------------------------

        RETURN
        END SUBROUTINE INTER_PREPARE_SORT
