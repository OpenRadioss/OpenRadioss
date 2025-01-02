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
      !||    plyxfem_mod          ../starter/share/modules1/plyxfem_mod.F
      !||--- called by ------------------------------------------------------
      !||    get_ply_nod          ../starter/source/spmd/spmd_anim_ply_init.F
      !||    get_ply_nodid        ../starter/source/spmd/spmd_anim_ply_init.F
      !||    spmd_anim_ply_init   ../starter/source/spmd/spmd_anim_ply_init.F
      !||    w_anim_ply           ../starter/source/restart/ddsplit/w_anim_ply.F
      !||====================================================================
      MODULE PLYXFEM_MOD

C---------------------------------------------
C Structures needed for animation writing
C---------------------------------------------

C PLY  Nodes structures for animations
      TYPE PLYNODS
         INTEGER  PLYNUMNODS                                 ! Number of nodes for each ply
         INTEGER, DIMENSION (:), ALLOCATABLE :: NODES        ! Nod ID for each ply
         INTEGER, DIMENSION (:), ALLOCATABLE :: PLYNODID     ! New Nod ID for Animation writing
      END TYPE PLYNODS

      TYPE (PLYNODS), DIMENSION(:), ALLOCATABLE :: PLYNOD

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: PLYIADNOD

C PLY SHELLS
      TYPE PLYSHELLS
         INTEGER  PLYNUMSHELL                                ! Number of shell elements for each ply
         INTEGER, DIMENSION (:), ALLOCATABLE :: SHID         ! Shell ID for each ply
         INTEGER, DIMENSION (:), ALLOCATABLE :: SHELLIPT     ! Ply Integration point for the ply in each shell
         INTEGER, DIMENSION (:), ALLOCATABLE :: SHELLID      ! Shell ID for global renumbering
      END TYPE PLYSHELLS

      TYPE (PLYSHELLS), DIMENSION(:), ALLOCATABLE :: PLYSHELL

C NPLYPART & IND_PLY are global values shared by all processors
      INTEGER NPLYPART
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_PLY
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDPID_PLY

      END MODULE PLYXFEM_MOD

