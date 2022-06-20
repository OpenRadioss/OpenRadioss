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
Chd|  INTER_COLOR_VOXEL                    source/interfaces/intsort/generic/inter_color_voxel.F
Chd|-- called by -----------
Chd|        INTER_PREPARE_SORT                    source/interfaces/intsort/generic/inter_prepare_sort.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE INTER_COLOR_VOXEL(ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,IPARI,INTBUF_TAB,
     .                               X,INTER_STRUCT,SORT_COMM)
!$COMMENT
!       INTER_COLOR_VOXEL description :
!       color the fine cell & coarse cell with main nodes
!
!       INTER_COLOR_VOXEL organization :
!           loop over the interface and call of INTER_CELL_COLOR
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE MULTI_FVM_MOD
        USE INTER_SORTING_MOD
        USE INTER_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "impl1_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK ! thread ID
        INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI    !   interface data
        TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB    ! interface data
        my_real, DIMENSION(3*NUMNOD), INTENT(in) :: X            !   position
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   struture for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: KK,I,J
        INTEGER :: NIN
        INTEGER :: IX,IY,IZ
        INTEGER :: SIZE_INDEX_CELL,TOTAL_NB_CELL
        INTEGER :: NSN,NMN,NRTM,NRTM_T
        INTEGER :: ADRESS,ESHIFT,SHIFT
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_CELL
        LOGICAL :: TYPE18
        INTEGER :: NTY,INACTI
        INTEGER :: VALUE
        my_real :: DGAPLOAD

        my_real :: TZINF,GAPMIN,GAPMAX,GAP,DRAD

!   ----------------------------------------
        ! --------------------
        ! allocation of global omp array
        IF(ITASK==0) THEN
            ALLOCATE(CELL_BOOL(NB_CELL_X,NB_CELL_Y,NB_CELL_Z))
            CELL_BOOL(1:NB_CELL_X,1:NB_CELL_Y,1:NB_CELL_Z) = .TRUE.
            ALLOCATE( NB_LOCAL_CELL(NTHREAD) ) 
        ENDIF
        CALL MY_BARRIER()
        ! --------------------
        ! allocation of local omp array
        SIZE_INDEX_CELL = NB_CELL_X*NB_CELL_Y*NB_CELL_Z
        ALLOCATE(INDEX_CELL(SIZE_INDEX_CELL))
        NB_LOCAL_CELL(ITASK+1) = 0

        ! --------------------
        ! loop over the interface
        DO KK=1,NB_INTER_SORTED
            NIN = LIST_INTER_SORTED(KK)
            NRTM = IPARI(4,NIN)
            NRTM_T = NRTM/NTHREAD
            ESHIFT = ITASK*NRTM_T
            ADRESS = 1 + ITASK*(IPARI(4,NIN)/NTHREAD)
            IF(ITASK.EQ.NTHREAD-1) NRTM_T= NRTM - ADRESS + 1
            NB_LOCAL_CELL(ITASK+1) = 0
            
            TZINF = INTBUF_TAB(NIN)%VARIABLES(8)
            GAP   =INTBUF_TAB(NIN)%VARIABLES(2)
            GAPMIN=INTBUF_TAB(NIN)%VARIABLES(13)
            GAPMAX=INTBUF_TAB(NIN)%VARIABLES(16)
            DRAD = ZERO
            IF(IPARI(7,NIN)==7) DRAD =INTBUF_TAB(NIN)%VARIABLES(32)
            DGAPLOAD = INTBUF_TAB(NIN)%VARIABLES(46)

            NTY = IPARI(7,NIN)
            INACTI = IPARI(22,NIN)
            TYPE18=.FALSE.
            IF(NTY==7 .AND. INACTI==7)TYPE18=.TRUE.

            IF(ITASK==0) THEN
                COARSE_GRID = .FALSE.
                IF(SORT_COMM(NIN)%PROC_NUMBER>NSPMD/2) THEN
                    COARSE_GRID = .TRUE.
                    IF(.NOT.ALLOCATED(SORT_COMM(NIN)%MAIN_COARSE_GRID) ) THEN
                        ALLOCATE(SORT_COMM(NIN)%MAIN_COARSE_GRID( 
     .                        NB_BOX_COARSE_GRID,NB_BOX_COARSE_GRID,NB_BOX_COARSE_GRID ) )
                   ENDIF
                   SORT_COMM(NIN)%MAIN_COARSE_GRID(:,:,:) = 0
               ENDIF
            ENDIF

            CALL MY_BARRIER()
            ! --------------------  
            ! cell coloration by omp threads  
            CALL INTER_CELL_COLOR( X,IPARI(21,NIN)   ,NRTM_T  ,INTBUF_TAB(NIN)%STFM(1+ESHIFT)   ,
     2                 TZINF,INTER_STRUCT(NIN)%CURV_MAX(ADRESS),
     3                 GAPMIN ,GAPMAX,INTBUF_TAB(NIN)%GAP_M(1+ESHIFT) ,
     4                 INTBUF_TAB(NIN)%IRECTM(1+4*ESHIFT),GAP,INTBUF_TAB(NIN)%VARIABLES(7),DRAD,
     5                 NB_LOCAL_CELL(ITASK+1),SIZE_INDEX_CELL,INDEX_CELL,
     6                 NIN,COARSE_GRID,SORT_COMM(NIN)%MAIN_COARSE_GRID,DGAPLOAD)

            CALL MY_BARRIER()
            ! --------------------   
            ! reduction of number of colored cells
            IF(ITASK==0) THEN
                TOTAL_NB_CELL = 0
                DO I=1,NTHREAD
                    TOTAL_NB_CELL = TOTAL_NB_CELL + NB_LOCAL_CELL(I)
                ENDDO
                SORT_COMM(NIN)%SIZE_CELL_LIST(1) = TOTAL_NB_CELL
                SORT_COMM(NIN)%SIZE_CELL_LIST(2) = 0
                ALLOCATE( SORT_COMM(NIN)%CELL_LIST(TOTAL_NB_CELL) )
            ENDIF
            ! --------------------   
            ! flush global array CELL_BOOL to true for the next interface
            DO I=1,NB_LOCAL_CELL(ITASK+1)

                VALUE = INDEX_CELL(I)
                IZ = ( VALUE - MOD(VALUE,1000000) ) / 1000000
                VALUE = VALUE - IZ * 1000000
                IY = ( VALUE - MOD(VALUE,1000) ) / 1000
                VALUE = VALUE - IY * 1000
                IX = VALUE
                CELL_BOOL(IX,IY,IZ) = .TRUE.
            ENDDO
            CALL MY_BARRIER()
            ! --------------------   

            ! --------------------   
            ! reduction of colored cells
            SHIFT = 0
            IF(ITASK>0) THEN
                DO J=1,ITASK
                    SHIFT = SHIFT + NB_LOCAL_CELL(J)
                ENDDO
            ENDIF
            SORT_COMM(NIN)%CELL_LIST(1+SHIFT:NB_LOCAL_CELL(ITASK+1)+SHIFT) = INDEX_CELL(1:NB_LOCAL_CELL(ITASK+1))

            CALL MY_BARRIER()
            ! --------------------   
        ENDDO

        CALL MY_BARRIER()
        ! --------------------
        ! deallocation
        IF(ITASK==0) THEN
            DEALLOCATE(CELL_BOOL)
            DEALLOCATE( NB_LOCAL_CELL ) 
        ENDIF
        DEALLOCATE(INDEX_CELL)
        ! --------------------
        RETURN
        END SUBROUTINE INTER_COLOR_VOXEL
