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
!||    mapping_option_mod     ../starter/share/modules1/dichotomy_mod.F
!||--- called by ------------------------------------------------------
!||    create_map_tables      ../starter/source/model/sets/map_tables.F
!||    hm_read_inistate_d00   ../starter/source/elements/initia/hm_read_inistate_d00.F
!||    hm_read_lines          ../starter/source/groups/hm_read_lines.F
!||    hm_read_surf           ../starter/source/groups/hm_read_surf.F
!||    hm_read_thgrou         ../starter/source/output/th/hm_read_thgrou.F
!||    hm_set                 ../starter/source/model/sets/hm_set.F
!||    lec_inistate           ../starter/source/elements/initia/lec_inistate.F
!||    lectur                 ../starter/source/starter/lectur.F
!||    sort_set               ../starter/source/model/sets/sort_sets.F
!||====================================================================
      MODULE MAPPING_OPTION_MOD
!$COMMENT
C-----------------------------------------------
C   Module DESCRIPTION :
C   ===================
C   Structure which hosts all dochotomy Mappting tables.
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET_LIST      List of sorted SETs
C===========================================================================================

!$ENDCOMMENT
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE MAPPING_STRUCT_
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: IPARTM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISETM
        INTEGER                              :: NSET_GENERAL
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISETCOLM
        INTEGER                              :: NSET_COLLECT
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISOLM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: IQUADM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISH4NM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISH3NM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRIAM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRUSSM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: IBEAMM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISPRINGM
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISUBSM  ! subsets
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISUBMM  ! submodels
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: IELMURM  ! Renault element
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISPHM  ! SPH
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: IRIVETM ! Rivet
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: IRBODYM ! Rbody

      END TYPE MAPPING_STRUCT_
c
c--------------------------
      END MODULE MAPPING_OPTION_MOD
