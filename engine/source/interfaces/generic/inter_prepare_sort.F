Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    inter_prepare_sort            ../engine/source/interfaces/generic/inter_prepare_sort.F
!||--- called by ------------------------------------------------------
!||    inttri                        ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    compute_voxel_dimensions      ../engine/source/interfaces/intsort/voxel_dimensions.F90
!||    fill_voxel_local_partial      ../engine/source/interfaces/intsort/fill_voxel.F90
!||    inter_box_creation            ../engine/source/interfaces/generic/inter_box_creation.F
!||    inter_color_coarse_voxel      ../engine/source/interfaces/generic/inter_color_coarse_voxel.F
!||    inter_color_voxel             ../engine/source/interfaces/generic/inter_color_voxel.F
!||    inter_component_bound         ../engine/source/interfaces/generic/inter_component_bound.F90
!||    inter_count_node_curv         ../engine/source/interfaces/generic/inter_count_node_curv.F
!||    inter_voxel_creation          ../engine/source/interfaces/generic/inter_voxel_creation.F
!||    my_barrier                    ../engine/source/system/machine.F
!||    spmd_box_limit_reduction      ../engine/source/mpi/interfaces/spmd_box_limit_reduction.F
!||    spmd_cell_list_exchange       ../engine/source/mpi/interfaces/spmd_cell_list_exchange.F
!||    spmd_exchange_component       ../engine/source/mpi/interfaces/spmd_exch_component.F90
!||    spmd_wait_nb                  ../engine/source/mpi/interfaces/spmd_wait_nb.F
!||--- uses       -----------------------------------------------------
!||    command_line_args_mod         ../engine/share/modules/command_line_args.F
!||    fill_voxel_mod                ../engine/source/interfaces/intsort/fill_voxel.F90
!||    intbufdef_mod                 ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter_component_bound_mod     ../engine/source/interfaces/generic/inter_component_bound.F90
!||    inter_sorting_mod             ../engine/share/modules/inter_sorting_mod.F
!||    inter_struct_mod              ../engine/share/modules/inter_struct_mod.F
!||    spmd_exchange_component_mod   ../engine/source/mpi/interfaces/spmd_exch_component.F90
!||    tri7box                       ../engine/share/modules/tri7box.F
!||    voxel_dimensions_mod          ../engine/source/interfaces/intsort/voxel_dimensions.F90
!||====================================================================
        SUBROUTINE INTER_PREPARE_SORT( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,ISENDTO,IRECVFROM,
     .                                 IPARI,IAD_ELEM,FR_ELEM,X,V,
     .                                 MS,TEMP,KINET,NODNX_SMS,ITAB,
     .                                 WEIGHT,INTBUF_TAB,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ,
     .                                 TEMP_SIZ, component )

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
        USE TRI7BOX
        USE FILL_VOXEL_MOD                                                  
        USE VOXEL_DIMENSIONS_MOD, ONLY : COMPUTE_VOXEL_DIMENSIONS
        USE COMMAND_LINE_ARGS_MOD, ONLY : GOT_PREVIEW
        use inter_component_bound_mod , only : inter_component_bound
        use spmd_exchange_component_mod , only : spmd_exchange_component      

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK    !   omp thread ID
        INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
        INTEGER, INTENT(in) :: NODNX_SMS_SIZ! size of NODNX_SMS
        INTEGER, INTENT(in) :: TEMP_SIZ     ! size of TEMP
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRECVFROM ! array for S and R : isendto = nsn ; ircvfrom = nmn
        INTEGER,DIMENSION(NPARI,NINTER), INTENT(inout) :: IPARI
        my_real, DIMENSION(3*NUMNOD), INTENT(in) :: X   !   position
        my_real, DIMENSION(3*NUMNOD), INTENT(in) :: V   !   velocity
        my_real, DIMENSION(NUMNOD), INTENT(in) :: MS    !   mass
        my_real, DIMENSION(TEMP_SIZ), INTENT(in) :: TEMP   !   temperature
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: WEIGHT ! weight : 1 if current proc computes the node
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB   ! global node ID
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: KINET  ! k energy & 
        INTEGER, DIMENSION(NODNX_SMS_SIZ), INTENT(in) :: NODNX_SMS ! SMS array
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM     ! connectivity array iad(P+1)-iad(P) = nb of frontier node on P
        INTEGER, DIMENSION(SFR_ELEM), INTENT(in) :: FR_ELEM          ! Frontier Node ID


        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB   ! interface data
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
        type(component_), dimension(ninter), intent(inout) :: component      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: TYPE18
        INTEGER :: KK,N
        INTEGER :: INACTI,NTY
        INTEGER :: NB_REQUEST_COARSE_CELL   ! number of request for coarse cell comm
        integer :: mode
        INTEGER, DIMENSION(NB_INTER_SORTED) :: ARRAY_REQUEST_COARSE_CELL ! array of request
        INTEGER, DIMENSION(NB_INTER_SORTED) :: LIST_INTER_COARSE_CELL ! list of interface
        integer :: nsn, nsnr, nrtm, nmn
        integer :: DUMMY(1)
        my_real :: tzinf,curv        
!   ----------------------------------------
        nsn = 0
        nsnr = 0
        nrtm = 0
        nmn = 0

        ! -----------------------------
        ! compute the min/max position 
        IF(NB_INTER_SORTED>0) THEN
          if(itask==0) then
            box_limit(1:3) = -ep30
            box_limit(4:6) = ep30
          endif
          call my_barrier( )

          ! ---------------------
          ! compute the curv and the number of main nodes
          ! the min/max position of main nodes
          ! and the min/max position of S nodes
          ! to create the bounding box "box_limit"
          DO KK=1,NB_INTER_SORTED
            N = LIST_INTER_SORTED(KK)
            NTY = IPARI(7,N)
            INACTI = IPARI(22,N)        
            TYPE18 = .FALSE.
            IF(NTY == 7 .AND. INACTI ==7)TYPE18=.TRUE.
            ! FILLS INTER_STRUCT%BOX_LIMIT_MAIN
            CALL INTER_COUNT_NODE_CURV( N,ITASK,IPARI,INTBUF_TAB,
     1                                      X,INTER_STRUCT)
            CALL MY_BARRIER()
            tzinf = intbuf_tab(n)%variables(tzinf_index)
            curv = inter_struct(n)%curv_max_max
            call inter_component_bound(numnod,tzinf,curv,x,component(n))
          ENDDO
          ! ---------------------
          CALL MY_BARRIER()
        ENDIF
        ! -----------------------------

        ! -----------------------------
        ! globalization of min/max position 
        IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
          IF(ITASK==0) THEN
            CALL SPMD_BOX_LIMIT_REDUCTION(NB_INTER_SORTED,box_limit)
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
          IF(ITASK==0) CALL INTER_BOX_CREATION(nb_cell_x,nb_cell_y,nb_cell_z,box_limit)
          CALL MY_BARRIER()
          CALL INTER_COLOR_COARSE_VOXEL(ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,IPARI,INTBUF_TAB,
     .                                  X,SORT_COMM)   
        ENDIF
        ! -----------------------------

        ! -----------------------------
        ! exchange of coarse_grid : send/rcv part
        IF(NSPMD>1.AND.NEED_TO_SORT>0)THEN
          IF(ITASK==0) THEN
            do kk=1,nb_inter_sorted
              n = list_inter_sorted(kk)
              call spmd_exchange_component(1,nspmd,component(n))
            enddo
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


        IF(NEED_TO_SORT>0)THEN
          IF(NSPMD > 1 .AND. ITASK == 0) THEN
        ! -----------------------------
            do kk=1,nb_inter_sorted
              n = list_inter_sorted(kk)
              call spmd_exchange_component(2,nspmd,component(n))
            enddo
        
            DO KK=1,NB_INTER_SORTED
              N = LIST_INTER_SORTED(KK)
              mode = 1
              ! -----------------------------
              ! exchange of number of colored cells (fine grid)
              CALL SPMD_CELL_LIST_EXCHANGE(IRECVFROM,ISENDTO,mode,WEIGHT,IAD_ELEM,
     .                 FR_ELEM,X,V,MS,TEMP,
     .                 KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZ, GOT_PREVIEW,component)   !   size of cell list
              ! -----------------------------
              if(GOT_PREVIEW == 1) THEN
                nrtm = IPARI(4,N)
                nsn = IPARI(5,N)
                nsnr = IPARI(24,N)
                nmn = IPARI(6,N)
                call compute_voxel_dimensions(nmn, nrtm, inter_struct(N))
              endif
              ! -----------------------------
              ! creation of voxel of secondary nodes 
              ! wait comm R "exchange of number of colored cells" & send/rcv the colored cells
              CALL INTER_VOXEL_CREATION(IPARI,INTBUF_TAB,X,N,SORT_COMM)! create the voxel of secondary nodes
              mode = 2
              CALL SPMD_CELL_LIST_EXCHANGE(IRECVFROM,ISENDTO,mode,WEIGHT,IAD_ELEM,
     .                 FR_ELEM,X,V,MS,TEMP,
     .                 KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZ, GOT_PREVIEW,component) !   mpi wait size of cell list

              ! -----------------------------
              if(GOT_PREVIEW == 1) THEN
                CALL FILL_VOXEL_LOCAL_PARTIAL(nsn,intbuf_tab(n)%nsv,nsnr,nrtm,numnod,x,intbuf_tab(N)%stfns,INTER_STRUCT(N),
     .                   sort_comm(n)%request_cell_rcv, sort_comm(n)%NB_REQUEST_CELL_RCV)
              endif            
              ! -----------------------------
              ! wait comm R "send/rcv the colored cells" 
              ! and compute the number of secondary nodes needed by remote proc 
              !             + comm of "number of secondary nodes needed by remote proc"
              !             + creation of the secondary node list 
              !             + initialize the S buffer of secondary node data (x & v)
              CALL SPMD_CELL_LIST_EXCHANGE(IRECVFROM,ISENDTO,3,WEIGHT,IAD_ELEM,
     .                 FR_ELEM,X,V,MS,TEMP,
     .                 KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZ, GOT_PREVIEW,component)   

            ENDDO
          ELSEIF (ITASK == 0) THEN
            DO KK=1,NB_INTER_SORTED
              N = LIST_INTER_SORTED(KK)
              if(GOT_PREVIEW == 1) THEN
                nrtm = IPARI(4,N) ;   nsn = IPARI(5,N) ; nsnr = IPARI(24,N) ; nmn = IPARI(6,N)
                call compute_voxel_dimensions(nmn, nrtm, inter_struct(N))
                CALL FILL_VOXEL_LOCAL_PARTIAL(nsn,intbuf_tab(n)%nsv,nsnr,nrtm,numnod,x,
     .                                           intbuf_tab(N)%stfns,INTER_STRUCT(N),DUMMY,0) 
              ENDIF
            ENDDO
          ENDIF
        ENDIF


          ! -----------------------------

          ! -----------------------------
          ! Fill the voxels with local secondary nodes 

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
     .                 N,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZ, GOT_PREVIEW,component)
                    ! wait "number of secondary nodes needed by remote proc"
              if(GOT_PREVIEW == 1) THEN
                nrtm = IPARI(4,N) ;   nsn = IPARI(5,N) ; nsnr = IPARI(24,N) ; nmn = IPARI(6,N)
                CALL FILL_VOXEL_LOCAL_PARTIAL(nsn,intbuf_tab(n)%nsv,nsnr,nrtm,numnod,x,intbuf_tab(N)%stfns,INTER_STRUCT(N),
     .                     SORT_COMM(N)%REQUEST_NB_R, SORT_COMM(N)%NBRECV_NB)
              endif

              CALL SPMD_WAIT_NB(IRECVFROM,ISENDTO,N,SORT_COMM)    
            ENDIF
            CALL MY_BARRIER()
          ENDIF
        ENDDO



        RETURN
        END SUBROUTINE INTER_PREPARE_SORT
