Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    plyxfem_mod            ../engine/share/modules/plyxfem_mod.F
      !||--- called by ------------------------------------------------------
      !||    anioffc_ply            ../engine/source/output/anim/generate/anioffc_ply.F
      !||    asspar4                ../engine/source/assembly/asspar4.F
      !||    cbacoor                ../engine/source/elements/shell/coqueba/cbacoor.F
      !||    cbavit_ply             ../engine/source/properties/composite_options/stack/cbavit_ply.F
      !||    cupdt_ply              ../engine/source/properties/composite_options/stack/cupdt_ply.F
      !||    delnumbc_ply           ../engine/source/output/anim/generate/delnumbc_ply.F
      !||    dfuncc_ply             ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    genani                 ../engine/source/output/anim/generate/genani.F
      !||    genh3d                 ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    get_ply_elem           ../engine/source/output/anim/generate/anin_ply_init.F
      !||    get_ply_nod            ../engine/source/output/anim/generate/anin_ply_init.F
      !||    get_ply_nodid          ../engine/source/output/anim/generate/anin_ply_init.F
      !||    i24ass0                ../engine/source/interfaces/int24/i24for3.F
      !||    i24ass2                ../engine/source/interfaces/int24/i24for3.F
      !||    i24for3                ../engine/source/interfaces/int24/i24for3.F
      !||    i24gap_pxfem           ../engine/source/interfaces/int24/i24gap_pxfem.F
      !||    i24iply_pxfem          ../engine/source/interfaces/int24/i24iply_pxfem.F
      !||    i25for3                ../engine/source/interfaces/int25/i25for3.F
      !||    parsor_ply             ../engine/source/output/anim/generate/parsor_ply.F
      !||    ply_accele             ../engine/source/assembly/ply_accele.F
      !||    ply_bcs1v              ../engine/source/constraints/general/bcs/ply_bcs.F
      !||    ply_vitesse            ../engine/source/assembly/ply_vitesse.F
      !||    plyxfem_ravuply        ../engine/source/output/restart/rdresb.F
      !||    plyxfem_rrestanim      ../engine/source/output/restart/rdresb.F
      !||    plyxfem_wravuply       ../engine/source/output/restart/wrrest.F
      !||    plyxfem_wrestanim      ../engine/source/output/restart/wrrest.F
      !||    reallocate_i_skyline   ../engine/source/system/reallocate_skyline.F
      !||    resol                  ../engine/source/engine/resol.F
      !||    resol_init             ../engine/source/engine/resol_init.F
      !||    spmd_anim_ply_init     ../engine/source/mpi/anim/spmd_anim_ply_init.F
      !||    spmd_anim_ply_velvec   ../engine/source/mpi/anim/spmd_anim_ply_velvec.F
      !||    spmd_anim_ply_xyznod   ../engine/source/mpi/anim/spmd_anim_ply_xyznod.F
      !||    spmd_anim_ply_xyznor   ../engine/source/mpi/anim/spmd_anim_ply_xyznor.F
      !||    spmd_exch2_a_pon       ../engine/source/mpi/forces/spmd_exch2_a_pon.F
      !||    spmd_exchi_a_pon       ../engine/source/mpi/forces/spmd_exchi_a_pon.F
      !||    spmd_iget_partn_ply    ../engine/source/mpi/anim/spmd_iget_partn_ply.F
      !||    tensorc_ply            ../engine/source/output/anim/generate/tensorc_ply.F
      !||    thnod                  ../engine/source/output/th/thnod.F
      !||    thnod_count            ../engine/source/output/th/thnod_count.F
      !||    velvec                 ../engine/source/output/anim/generate/velvec.F
      !||    xyznod_ply             ../engine/source/output/anim/generate/xyznod_ply.F
      !||    xyznor_ply             ../engine/source/output/anim/generate/xyznor_ply.F
      !||====================================================================
      MODULE PLYXFEM_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C------------------------
C tableau de travail 
C------------------------
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
CC         my_real,
CC     .            DIMENSION(:,:,:),ALLOCATABLE :: PLYA,PLYV,PLYU,PLYFSKY
C-------------------------------
C Donnees briques intersectantes
C-------------------------------
        TYPE PLY_DATA
           my_real,
     .            DIMENSION(:,:), POINTER :: A,V,U,FSKY,FSKYI,F
           integer, DIMENSION(:),POINTER :: ITAG
        END TYPE PLY_DATA
C        
        TYPE(PLY_DATA), DIMENSION(:), ALLOCATABLE :: PLY,PLYSKY 
        TYPE(PLY_DATA), ALLOCATABLE :: PLYSKYI 
        my_real,
     .            DIMENSION(:,:), ALLOCATABLE :: VN_NOD

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
         INTEGER, DIMENSION (:), ALLOCATABLE :: SHID         ! internal Shell ID for each ply
         INTEGER, DIMENSION (:), ALLOCATABLE :: SHELLIPT     ! Ply Integration point for the ply in each shell
         INTEGER, DIMENSION (:), ALLOCATABLE :: SHELLID      ! Shell ID for global renumbering
      END TYPE PLYSHELLS

      TYPE (PLYSHELLS), DIMENSION(:), ALLOCATABLE :: PLYSHELL

C NPLYPART & IND_PLY are global values shared by all processors
      INTEGER NPLYPART
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_PLY,PLYSIZG,IDPID_PLY
C-------------------------------
C Donnees briques intersectantes
C-------------------------------
cc        my_real,
cc    .            DIMENSION(:,:), POINTER :: FSKY
cc        END TYPE PLYSKY_DATA
cc        TYPE(PLYSKY_DATA), DIMENSION(:), ALLOCATABLE :: PLYSKY
cc        INTEGER,
cc     .            DIMENSION(:), ALLOCATABLE :: IADC_PXFEM, IADSKY_PFXEM
      END MODULE PLYXFEM_MOD
     
