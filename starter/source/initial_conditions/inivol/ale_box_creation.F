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
Chd|  ALE_BOX_CREATION              source/initial_conditions/inivol/ale_box_creation.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE ALE_BOX_CREATION(NB_BOX_LIMIT,NB_CELL_X,NB_CELL_Y,NB_CELL_Z,
     .                              ALE_ELEMENT_NUMBER,ELEMENT_SIZE,MIN_MAX_POSITION,
     .                              LEADING_DIMENSION,SIZE_CELL)
!$COMMENT
!       ALE_BOX_CREATION description
!       creation of a grid with (NB_CELL_X,NB_CELL_Y,NB_CELL_Z) cells
!       
!       ALE_BOX_CREATION organization :
!       - a direction is choosen 
!       - the number of cell in this direction is computed
!       - then the number of cell in other directions are computed
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NB_BOX_LIMIT
        INTEGER, INTENT(INOUT) :: NB_CELL_X,NB_CELL_Y,NB_CELL_Z
        INTEGER, INTENT(IN) :: ALE_ELEMENT_NUMBER ! number of ale element with material 51 or 151
        my_real, INTENT(INOUT) :: ELEMENT_SIZE ! max element size
        my_real, DIMENSION(6), INTENT(INOUT) :: MIN_MAX_POSITION ! min/max position
        INTEGER, INTENT(INOUT) :: LEADING_DIMENSION
        my_real, DIMENSION(3), INTENT(INOUT) :: SIZE_CELL ! cell's size in x/y/z direction
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real :: DX_BOX,DY_BOX,DZ_BOX
        my_real :: RATIO,DIST_MAX,LOCAL_SIZE_CELL
C-----------------------------------------------
        ELEMENT_SIZE = SQRT(THREE) * ELEMENT_SIZE
        LEADING_DIMENSION = 0
        MIN_MAX_POSITION(1:3) = MIN_MAX_POSITION(1:3) - EM10
        MIN_MAX_POSITION(4:6) = MIN_MAX_POSITION(4:6) + EM10
        !   find the leading direction
        DX_BOX = ABS(MIN_MAX_POSITION(1) - MIN_MAX_POSITION(4))
        DY_BOX = ABS(MIN_MAX_POSITION(2) - MIN_MAX_POSITION(5))
        DZ_BOX = ABS(MIN_MAX_POSITION(3) - MIN_MAX_POSITION(6))
        
        IF(DX_BOX == 2*EP30)DX_BOX=ZERO ! y,z plane
        IF(DY_BOX == 2*EP30)DY_BOX=ZERO ! x,z plane
        IF(DZ_BOX == 2*EP30)DZ_BOX=ZERO ! x,y plane        
                
        DIST_MAX = MAX(DX_BOX,DY_BOX,DZ_BOX)
        IF(DIST_MAX==DX_BOX) LEADING_DIMENSION = 1
        IF(DIST_MAX==DY_BOX) LEADING_DIMENSION = 2
        IF(DIST_MAX==DZ_BOX) LEADING_DIMENSION = 3
        ! -----------------------
        ! compute the number of cell in the leading direction, 
        ! then compute the number of cell in the other direction
        RATIO = SQRT( ALE_ELEMENT_NUMBER / (DX_BOX*DY_BOX+DX_BOX*DZ_BOX+DY_BOX*DZ_BOX))
        IF(LEADING_DIMENSION==1) THEN
            NB_CELL_X = NINT( RATIO * DX_BOX )   
            NB_CELL_X = MIN(NB_CELL_X,NB_BOX_LIMIT)

            LOCAL_SIZE_CELL = DX_BOX / NB_CELL_X
            IF(LOCAL_SIZE_CELL<ELEMENT_SIZE) THEN 
                LOCAL_SIZE_CELL = ELEMENT_SIZE
                NB_CELL_X = NINT( DX_BOX / LOCAL_SIZE_CELL )             
                NB_CELL_X = MIN(NB_CELL_X,NB_BOX_LIMIT)
            ENDIF
            NB_CELL_Y = NINT( DY_BOX / LOCAL_SIZE_CELL )             
            NB_CELL_Y = MIN(NB_CELL_Y,NB_BOX_LIMIT)

            NB_CELL_Z = NINT( DZ_BOX / LOCAL_SIZE_CELL )
            NB_CELL_Z = MIN(NB_CELL_Z,NB_BOX_LIMIT)
        ELSEIF(LEADING_DIMENSION==2) THEN
            NB_CELL_Y = NINT( RATIO * DY_BOX )
            NB_CELL_Y = MIN(NB_CELL_Y,NB_BOX_LIMIT)

            LOCAL_SIZE_CELL = DY_BOX / NB_CELL_Y
            IF(LOCAL_SIZE_CELL<ELEMENT_SIZE) THEN 
                LOCAL_SIZE_CELL = ELEMENT_SIZE
                NB_CELL_Y = NINT( DY_BOX / LOCAL_SIZE_CELL )             
                NB_CELL_Y = MIN(NB_CELL_Y,NB_BOX_LIMIT)
            ENDIF
            NB_CELL_X = NINT( DX_BOX / LOCAL_SIZE_CELL )
            NB_CELL_X = MIN(NB_CELL_X,NB_BOX_LIMIT)

            NB_CELL_Z = NINT( DZ_BOX / LOCAL_SIZE_CELL )
            NB_CELL_Z = MIN(NB_CELL_Z,NB_BOX_LIMIT)
        ELSE !IF(LEADING_DIMENSION==3) THEN
            NB_CELL_Z = NINT( RATIO * DZ_BOX )
            NB_CELL_Z = MIN(NB_CELL_Z,NB_BOX_LIMIT)

            LOCAL_SIZE_CELL = DZ_BOX / NB_CELL_Z
            IF(LOCAL_SIZE_CELL<ELEMENT_SIZE) THEN 
                LOCAL_SIZE_CELL = ELEMENT_SIZE
                NB_CELL_Z = NINT( DZ_BOX / LOCAL_SIZE_CELL )             
                NB_CELL_Z = MIN(NB_CELL_Z,NB_BOX_LIMIT)
            ENDIF
            NB_CELL_Y = NINT( DY_BOX / LOCAL_SIZE_CELL )
            NB_CELL_Y = MIN(NB_CELL_Y,NB_BOX_LIMIT)

            NB_CELL_X = NINT( DX_BOX / LOCAL_SIZE_CELL )
            NB_CELL_X = MIN(NB_CELL_X,NB_BOX_LIMIT)
        ENDIF
        ! -----------------------

        ! -----------------------
        ! ensure that nb_cell_... is higher than 0 (2D case)
        NB_CELL_X = MAX(1,NB_CELL_X)
        NB_CELL_Y = MAX(1,NB_CELL_Y)
        NB_CELL_Z = MAX(1,NB_CELL_Z)

        SIZE_CELL(1) = DX_BOX / LOCAL_SIZE_CELL
        SIZE_CELL(2) = DY_BOX / LOCAL_SIZE_CELL
        SIZE_CELL(3) = DZ_BOX / LOCAL_SIZE_CELL
        ! -----------------------

        RETURN
        END SUBROUTINE ALE_BOX_CREATION
