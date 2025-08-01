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
!||    surf_mod                      ../starter/share/modules1/surf_mod.F
!||--- called by ------------------------------------------------------
!||    create_surface_from_element   ../starter/source/model/sets/create_surface_from_element.F
!||    deallocate_surf_elm           ../starter/source/groups/init_surf_elm.F
!||    fill_igr                      ../starter/source/model/sets/fill_igr.F
!||    hm_read_lines                 ../starter/source/groups/hm_read_lines.F
!||    hm_read_sensors               ../starter/source/tools/sensor/hm_read_sensors.F
!||    hm_read_surf                  ../starter/source/groups/hm_read_surf.F
!||    init_surf_elm                 ../starter/source/groups/init_surf_elm.F
!||    ssurftag                      ../starter/source/groups/ssurftag.F
!||    surface_type                  ../starter/source/model/sets/surface_type.F90
!||    surftag                       ../starter/source/groups/surftag.F
!||====================================================================
      MODULE SURF_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
        INTEGER, PARAMETER :: EXT_SURF = 1 !< definition of /EXT surface
        INTEGER, PARAMETER :: ALL_SURF = 2 !< definition of /ALL surface
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
        TYPE PART_TYPE
!--------------------------------------------------------------------
!       NSOL : number of solid (/= solid10) element per part
!       NSOL10 : number of solid10 element per part
!       NSHELL : number of shell element per part
!       NTRI : number of shell3n element per part
!       NTRUSS : number of truss element per part
!       NBEAM : number of beam element per part
!       NSPRING : number of spring element per part
!       SOL_PART : ID of each solid (/=solid10) element in the current part
!       SOL10_PART : ID of each solid10 element in the current part
!       SHELL_PART : ID of each shell element in the current part
!       TRI_PART : ID of each shell3n element in the current part
!       TRUSS_PART : ID of each truss element in the current part
!       BEAM_PART : ID of each beam element in the current part
!       SPRING_PART : ID of each spring element in the current part
!--------------------------------------------------------------------
                INTEGER :: NSOL,NSOL10,NSHELL,NTRI
                INTEGER :: NTRUSS,NBEAM,NSPRING
                INTEGER, DIMENSION(:), ALLOCATABLE :: SOL_PART
                INTEGER, DIMENSION(:), ALLOCATABLE :: SOL10_PART
                INTEGER, DIMENSION(:), ALLOCATABLE :: SHELL_PART
                INTEGER, DIMENSION(:), ALLOCATABLE :: TRI_PART
                INTEGER, DIMENSION(:), ALLOCATABLE :: TRUSS_PART
                INTEGER, DIMENSION(:), ALLOCATABLE :: BEAM_PART
                INTEGER, DIMENSION(:), ALLOCATABLE :: SPRING_PART

        END TYPE PART_TYPE
C
      END MODULE SURF_MOD
