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
Chd|  INTER_CELL_COLOR              source/interfaces/generic/inter_cell_color.F
Chd|-- called by -----------
Chd|        INTER_COLOR_VOXEL             source/interfaces/generic/inter_color_voxel.F
Chd|-- calls ---------------
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|====================================================================
        SUBROUTINE INTER_CELL_COLOR(X,IGAP   ,NRTM  ,STF   ,
     2              TZINF  ,CURV_MAX,
     3              GAPMIN ,GAPMAX,GAP_M ,
     4             IRECT  ,GAP     ,BGAPSMX,DRAD  ,NB_INDEX_CELL,SIZE_INDEX_CELL,INDEX_CELL,
     5             NIN,NEEDED,MAIN_COARSE_GRID,DGAPLOAD)
!$COMMENT
!       INTER_CELL_COLOR description :
!       color the fine cell & coarse cell with main nodes
!
!       INTER_CELL_COLOR organization :
!           loop over the active MAIN surface and :
!               * computation of fine grid index IX/IY/IZ
!               * color the fine grid IX/IY/IZ
!               * if the interface is a large interface : computation of coarse grid index and coloration of coarse grid
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_SORTING_MOD
        USE INTER_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        LOGICAL, INTENT(in) :: NEEDED
        INTEGER, INTENT(in) :: NIN
        INTEGER :: IGAP, NRTM, IRECT(4,NRTM)
        my_real :: X(3,NUMNOD),STF(NRTM), GAP_M(NRTM), BGAPSMX,DRAD
        my_real :: TZINF,GAPMIN,GAPMAX,GAP,CURV_MAX(NRTM)
        INTEGER, INTENT(inout) :: NB_INDEX_CELL
        INTEGER, INTENT(in) :: SIZE_INDEX_CELL
        INTEGER, DIMENSION(SIZE_INDEX_CELL),INTENT(inout) :: INDEX_CELL
        INTEGER, DIMENSION(NB_BOX_COARSE_GRID,NB_BOX_COARSE_GRID,NB_BOX_COARSE_GRID), INTENT(inout) :: MAIN_COARSE_GRID
        my_real , INTENT(IN) :: DGAPLOAD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NE,M1,M2,M3,M4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ
        my_real
     .        RATIO, AAA, MARGE,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
        INTEGER TMP
        INTEGER :: IX_COARSE,IY_COARSE,IZ_COARSE
        my_real :: VALUE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        LOC_PROC = ISPMD + 1
        MARGE = TZINF-MAX(GAP+DGAPLOAD,DRAD)

        NBX = NB_CELL_X
        NBY = NB_CELL_Y
        NBZ = NB_CELL_Z

        XMAXB = BOX_LIMIT(1)
        YMAXB = BOX_LIMIT(2)
        ZMAXB = BOX_LIMIT(3)
        XMINB = BOX_LIMIT(4)
        YMINB = BOX_LIMIT(5)
        ZMINB = BOX_LIMIT(6)
        ! ------------------------------
        ! loop over the main surface
        DO NE=1,NRTM
            !   skip the deleted surfaces
            IF(STF(NE) == ZERO)CYCLE

            IF(IGAP == 0)THEN
                AAA = TZINF+CURV_MAX(NE)
            ELSE
                AAA = MARGE+CURV_MAX(NE)+
     .           MAX(MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE)))+DGAPLOAD,DRAD)
            ENDIF
            !   nodes of the surface
            M1 = IRECT(1,NE)
            M2 = IRECT(2,NE)
            M3 = IRECT(3,NE)
            M4 = IRECT(4,NE)
            XX1=X(1,M1)
            XX2=X(1,M2)
            XX3=X(1,M3)
            XX4=X(1,M4)
            XMAXE=MAX(XX1,XX2,XX3,XX4)
            XMINE=MIN(XX1,XX2,XX3,XX4)

            YY1=X(2,M1)
            YY2=X(2,M2)
            YY3=X(2,M3)
            YY4=X(2,M4)
            YMAXE=MAX(YY1,YY2,YY3,YY4)
            YMINE=MIN(YY1,YY2,YY3,YY4)

            ZZ1=X(3,M1)
            ZZ2=X(3,M2)
            ZZ3=X(3,M3)
            ZZ4=X(3,M4)
            ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
            ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)

            !   cell index computation
            IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
            IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
            IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

            IX1=MAX(1,MIN(NBX,IX1))
            IY1=MAX(1,MIN(NBY,IY1))
            IZ1=MAX(1,MIN(NBZ,IZ1))

            IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
            IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
            IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

            IX2=MAX(1,MIN(NBX,IX2))
            IY2=MAX(1,MIN(NBY,IY2))
            IZ2=MAX(1,MIN(NBZ,IZ2))

            !   loop over the cell and cell coloration
            DO IZ = IZ1, IZ2             
                VALUE = 4.*float(IZ)/float(NB_CELL_Z)
                IZ_COARSE = MIN(INT(VALUE),NB_BOX_COARSE_GRID)
                IZ_COARSE = MAX(IZ_COARSE,1)
                DO IY = IY1, IY2
                    VALUE = 4.*float(IY)/float(NB_CELL_Y)
                    IY_COARSE = MIN(INT(VALUE),NB_BOX_COARSE_GRID)
                    IY_COARSE = MAX(IY_COARSE,1)
                    DO IX = IX1, IX2
                        VALUE = 4.*float(IX)/float(NB_CELL_X)
                        IX_COARSE = MIN(INT(VALUE),NB_BOX_COARSE_GRID)
                        IX_COARSE = MAX(IX_COARSE,1)
                        IF(CELL_BOOL(IX,IY,IZ)) THEN
                            NB_INDEX_CELL = NB_INDEX_CELL + 1
                            INDEX_CELL(NB_INDEX_CELL) = IX + 1000*IY +1000**2 * IZ
                            CELL_BOOL(IX,IY,IZ) = .FALSE.
                            IF(NEEDED) MAIN_COARSE_GRID(IX_COARSE,IY_COARSE,IZ_COARSE) = 1
                        ENDIF
                    END DO
                END DO
            END DO
        ENDDO
        ! ------------------------------
C
        RETURN
        END SUBROUTINE INTER_CELL_COLOR
