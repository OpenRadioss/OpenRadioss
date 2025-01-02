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
      !||    inverted_group_mod         ../starter/share/modules1/inverted_group_mod.F
      !||--- called by ------------------------------------------------------
      !||    compute_connect_partelm    ../starter/source/model/sets/compute_connect_partelm.F
      !||    create_element_from_part   ../starter/source/model/sets/create_element_from_part.F
      !||    hm_set                     ../starter/source/model/sets/hm_set.F
      !||    inverted_group_alloc       ../starter/source/model/sets/inverted_group_alloc.F
      !||    inverted_group_dealloc     ../starter/source/model/sets/inverted_group_dealloc.F
      !||    inverted_group_init        ../starter/source/model/sets/inverted_group_init.F
      !||    lectur                     ../starter/source/starter/lectur.F
      !||====================================================================
      MODULE INVERTED_GROUP_MOD
!$COMMENT

!$ENDCOMMENT
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE INVERTGROUP_STRUCT_
        LOGICAL :: IS_USED
        LOGICAL :: ALREADY_DONE
        LOGICAL :: ALREADY_ALLOC
        INTEGER :: SIZE_INDPART

        INTEGER :: SIZE_S
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTS
        INTEGER, DIMENSION(:), POINTER :: PARTS

        INTEGER :: SIZE_Q
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTQ
        INTEGER, DIMENSION(:), POINTER :: PARTQ

        INTEGER :: SIZE_C
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTC
        INTEGER, DIMENSION(:), POINTER :: PARTC

        INTEGER :: SIZE_T
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTT
        INTEGER, DIMENSION(:), POINTER :: PARTT

        INTEGER :: SIZE_P
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTP
        INTEGER, DIMENSION(:), POINTER :: PARTP

        INTEGER :: SIZE_TG
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTTG
        INTEGER, DIMENSION(:), POINTER :: PARTTG

        INTEGER :: SIZE_TRIA
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTTRIA
        INTEGER, DIMENSION(:), POINTER :: PARTTRIA

        INTEGER :: SIZE_R
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTR
        INTEGER, DIMENSION(:), POINTER :: PARTR

        INTEGER :: SIZE_SPH
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDPARTSPH
        INTEGER, DIMENSION(:), POINTER :: PARTSPH
      END TYPE INVERTGROUP_STRUCT_
c
c--------------------------
      END MODULE INVERTED_GROUP_MOD
