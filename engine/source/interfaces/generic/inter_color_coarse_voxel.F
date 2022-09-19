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
Chd|  INTER_COLOR_COARSE_VOXEL      source/interfaces/generic/inter_color_coarse_voxel.F
Chd|-- called by -----------
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|====================================================================
        SUBROUTINE INTER_COLOR_COARSE_VOXEL(ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,IPARI,INTBUF_TAB,
     .                                      X,SORT_COMM)
!$COMMENT
!       INTER_COLOR_COARSE_VOXEL description :
!       color the coarse grid with the secondary nodes only for large interfaces (= with more than NSPMD/2 processors)
!
!       INTER_COLOR_COARSE_VOXEL organization :
!           loop over the secondary nodes (omp //)
!           compute the index of the coarse grid with the node position
!           color the cell of the coarse grid
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
        USE INTER_SORTING_MOD
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
        my_real, DIMENSION(3,NUMNOD), INTENT(in), TARGET :: X            !   position
        TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB    ! interface data
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: KK,I,J
        INTEGER :: NIN,NEXT
        INTEGER :: IX,IY,IZ
        INTEGER :: NSN
        INTEGER :: FIRST,LAST
        INTEGER :: ADRESS,ESHIFT,SHIFT
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: INDEX_CELL
        my_real :: VALUE
        my_real :: XMINB,YMINB,ZMINB
        my_real :: XMAXB,YMAXB,ZMAXB
!   ----------------------------------------
        ! ------------------
        ! allocation of global omp array
        IF(ITASK==0) THEN
            ALLOCATE(CELL_BOOL(4,4,4))
            CELL_BOOL(1:4,1:4,1:4) = .FALSE.
        ENDIF
        CALL MY_BARRIER()
        ! ------------------
        ! loop over the interface
        DO KK=1,NB_INTER_SORTED
            NIN = LIST_INTER_SORTED(KK)
            NSN = IPARI(5,NIN)
            ! ------------------
            ! coarse grid is generated only for large interface (nb of processor > nspm/2)
            IF(SORT_COMM(NIN)%PROC_NUMBER>NSPMD/2) THEN
                FIRST = 1 + ITASK*NSN/NTHREAD
                LAST = (ITASK+1)*NSN/NTHREAD
                XMAXB = BOX_LIMIT(1)
                YMAXB = BOX_LIMIT(2)
                ZMAXB = BOX_LIMIT(3)
                XMINB = BOX_LIMIT(4)
                YMINB = BOX_LIMIT(5)
                ZMINB = BOX_LIMIT(6)
                IF(ITASK==0) CELL_BOOL(1:4,1:4,1:4) = .FALSE.
                CALL MY_BARRIER()
                ! ------------------
                !   loop over the secondary node to color the coarse grid
                DO I=FIRST,LAST
                    J=INTBUF_TAB(NIN)%NSV(I)
                    VALUE = NB_BOX_COARSE_GRID*(X(1,J)-XMINB)/(XMAXB-XMINB)
                    IX = MIN(INT(VALUE),NB_BOX_COARSE_GRID)
                    IX = MAX(IX,1)
                    VALUE = NB_BOX_COARSE_GRID*(X(2,J)-YMINB)/(YMAXB-YMINB)
                    IY = MIN(INT(VALUE),NB_BOX_COARSE_GRID)
                    IY = MAX(IY,1)
                    VALUE = NB_BOX_COARSE_GRID*(X(3,J)-ZMINB)/(ZMAXB-ZMINB)
                    IZ = MIN(INT(VALUE),NB_BOX_COARSE_GRID)
                    IZ = MAX(IZ,1)
                    CELL_BOOL(IX,IY,IZ) = .TRUE.
                ENDDO
                CALL MY_BARRIER()
                ! ------------------
                ! omp reduction
                IF(ITASK==0) THEN
                    IF(.NOT.ALLOCATED(SORT_COMM(NIN)%COARSE_GRID)) THEN
                        ALLOCATE(SORT_COMM(NIN)%COARSE_GRID(NB_BOX_COARSE_GRID**3 + 1))
                    ENDIF
                    SORT_COMM(NIN)%COARSE_GRID(1:NB_BOX_COARSE_GRID**3 + 1) = 0
                    NEXT = 0
                    DO IZ=1,4
                        DO IY=1,4
                            DO IX=1,4
                                IF(CELL_BOOL(IX,IY,IZ)) THEN
                                    NEXT = NEXT + 1
                                    SORT_COMM(NIN)%COARSE_GRID(NEXT) =
     .                IX+(IY-1)*NB_BOX_COARSE_GRID+(IZ-1)*NB_BOX_COARSE_GRID**2
                                ENDIF
                            ENDDO
                        ENDDO
                    ENDDO
                    SORT_COMM(NIN)%COARSE_GRID(NB_BOX_COARSE_GRID**3 + 1) = NEXT
                ENDIF
                CALL MY_BARRIER()
                ! ------------------
            ENDIF
            ! ------------------
        ENDDO
        ! ------------------

        CALL MY_BARRIER()
        IF(ITASK==0) THEN
            DEALLOCATE(CELL_BOOL)
        ENDIF
        !   ------------------
        RETURN
        END SUBROUTINE INTER_COLOR_COARSE_VOXEL
