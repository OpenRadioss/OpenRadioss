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
Chd|  INTER_BOX_CREATION            source/interfaces/generic/inter_box_creation.F
Chd|-- called by -----------
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|-- calls ---------------
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|====================================================================
        SUBROUTINE INTER_BOX_CREATION( )
!$COMMENT
!       INTER_BOX_CREATION description :
!       creation of the global box
!
!       INTER_BOX_CREATION organization :
!           * compute the size dx/dy/dz in each direction
!           * choose the largest size
!           * compute the cell's size
!           * compute the number of cell for each direction
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: KK
        INTEGER :: NIN
        INTEGER :: LEADING_DIMENSION
        my_real :: DX_BOX,DY_BOX,DZ_BOX
        my_real :: RATIO,SIZE_CELL,DIST_MAX
!   ----------------------------------------

        !   ------------------
        !   Create the box : 
        !   number of cell = [ tota_number_of_node / ( dx*dy + dx*dz+dy*dz ) ]**(1/2) * dx or dy or dz
        !   1 < number of cell < 128 
        
        !   find the leading direction
        DX_BOX = ABS(BOX_LIMIT(1) - BOX_LIMIT(4))
        DY_BOX = ABS(BOX_LIMIT(2) - BOX_LIMIT(5))
        DZ_BOX = ABS(BOX_LIMIT(3) - BOX_LIMIT(6))
        
        DIST_MAX = MAX(DX_BOX,DY_BOX,DZ_BOX)
        IF(DIST_MAX==DX_BOX) LEADING_DIMENSION = 1
        IF(DIST_MAX==DY_BOX) LEADING_DIMENSION = 2
        IF(DIST_MAX==DZ_BOX) LEADING_DIMENSION = 3
        
        !   compute the number of cell in the leading direction, then compute the number of cell in the other direction
        RATIO = SQRT( NUMNODG / (DX_BOX*DY_BOX+DX_BOX*DZ_BOX+DY_BOX*DZ_BOX))
        IF(LEADING_DIMENSION==1) THEN
            NB_CELL_X = NINT( RATIO * DX_BOX )
            NB_CELL_X = MAX(NB_CELL_X,NB_BOX_COARSE_GRID)     
            NB_CELL_X = NB_CELL_X - modulo(NB_CELL_X,4)           
            NB_CELL_X = MIN(NB_CELL_X,NB_BOX_LIMIT)
            NB_CELL_X = MAX(NB_CELL_X,NB_BOX_COARSE_GRID)

            SIZE_CELL = DX_BOX / NB_CELL_X

            NB_CELL_Y = NINT( DY_BOX / SIZE_CELL )
            NB_CELL_Y = MAX(NB_CELL_Y,NB_BOX_COARSE_GRID)
            NB_CELL_Y = NB_CELL_Y - modulo(NB_CELL_Y,4)                 
            NB_CELL_Y = MIN(NB_CELL_Y,NB_BOX_LIMIT)
            NB_CELL_Y = MAX(NB_CELL_Y,NB_BOX_COARSE_GRID)

            NB_CELL_Z = NINT( DZ_BOX / SIZE_CELL )
            NB_CELL_Z = MAX(NB_CELL_Z,NB_BOX_COARSE_GRID)
            NB_CELL_Z = NB_CELL_Z - modulo(NB_CELL_Z,4)
            NB_CELL_Z = MIN(NB_CELL_Z,NB_BOX_LIMIT)
            NB_CELL_Z = MAX(NB_CELL_Z,NB_BOX_COARSE_GRID)
        ELSEIF(LEADING_DIMENSION==2) THEN
            NB_CELL_Y = NINT( RATIO * DY_BOX )
            NB_CELL_Y = MAX(NB_CELL_Y,NB_BOX_COARSE_GRID)         
            NB_CELL_Y = NB_CELL_Y - modulo(NB_CELL_Y,4)   
            NB_CELL_Y = MIN(NB_CELL_Y,NB_BOX_LIMIT)
            NB_CELL_Y = MAX(NB_CELL_Y,NB_BOX_COARSE_GRID)

            SIZE_CELL = DY_BOX / NB_CELL_Y

            NB_CELL_X = NINT( DX_BOX / SIZE_CELL )
            NB_CELL_X = MAX(NB_CELL_X,NB_BOX_COARSE_GRID)
            NB_CELL_X = NB_CELL_X - modulo(NB_CELL_X,4)    
            NB_CELL_X = MIN(NB_CELL_X,NB_BOX_LIMIT)
            NB_CELL_X = MAX(NB_CELL_X,NB_BOX_COARSE_GRID)

            NB_CELL_Z = NINT( DZ_BOX / SIZE_CELL )
            NB_CELL_Z = MAX(NB_CELL_Z,NB_BOX_COARSE_GRID)
            NB_CELL_Z = NB_CELL_Z - modulo(NB_CELL_Z,4)
            NB_CELL_Z = MIN(NB_CELL_Z,NB_BOX_LIMIT)
            NB_CELL_Z = MAX(NB_CELL_Z,NB_BOX_COARSE_GRID)
        ELSEIF(LEADING_DIMENSION==3) THEN
            NB_CELL_Z = NINT( RATIO * DZ_BOX )
            NB_CELL_Z = MAX(NB_CELL_Z,NB_BOX_COARSE_GRID)
            NB_CELL_Z = NB_CELL_Z - modulo(NB_CELL_Z,4)  
            NB_CELL_Z = MIN(NB_CELL_Z,NB_BOX_LIMIT)
            NB_CELL_Z = MAX(NB_CELL_Z,NB_BOX_COARSE_GRID)

            SIZE_CELL = DZ_BOX / NB_CELL_Z

            NB_CELL_Y = NINT( DY_BOX / SIZE_CELL )
            NB_CELL_Y = MAX(NB_CELL_Y,NB_BOX_COARSE_GRID)
            NB_CELL_Y = NB_CELL_Y - modulo(NB_CELL_Y,4)  
            NB_CELL_Y = MIN(NB_CELL_Y,NB_BOX_LIMIT)
            NB_CELL_Y = MAX(NB_CELL_Y,NB_BOX_COARSE_GRID)

            NB_CELL_X = NINT( DX_BOX / SIZE_CELL )
            NB_CELL_X = MAX(NB_CELL_X,NB_BOX_COARSE_GRID)
            NB_CELL_X = NB_CELL_X - modulo(NB_CELL_X,4)  
            NB_CELL_X = MIN(NB_CELL_X,NB_BOX_LIMIT)
            NB_CELL_X = MAX(NB_CELL_X,NB_BOX_COARSE_GRID)
        ENDIF
        !   ------------------

        RETURN
        END SUBROUTINE INTER_BOX_CREATION
