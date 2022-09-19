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
Chd|  PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|-- called by -----------
Chd|        ANIOFFC_PLY                   source/output/anim/generate/anioffc_ply.F
Chd|        ASSPAR4                       source/assembly/asspar4.F     
Chd|        CBACOOR                       source/elements/shell/coqueba/cbacoor.F
Chd|        CBAVIT_PLY                    source/properties/composite_options/stack/cbavit_ply.F
Chd|        CUPDT_PLY                     source/properties/composite_options/stack/cupdt_ply.F
Chd|        DELNUMBC_PLY                  source/output/anim/generate/delnumbc_ply.F
Chd|        DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        I24ASS0                       source/interfaces/int24/i24for3.F
Chd|        I24ASS2                       source/interfaces/int24/i24for3.F
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|        I24GAP_PXFEM                  source/interfaces/int24/i24gap_pxfem.F
Chd|        I24IPLY_PXFEM                 source/interfaces/int24/i24iply_pxfem.F
Chd|        I25FOR3                       source/interfaces/int25/i25for3.F
Chd|        PARSOR_PLY                    source/output/anim/generate/parsor_ply.F
Chd|        PLYXFEM_RAVUPLY               source/output/restart/rdresb.F
Chd|        PLYXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|        PLYXFEM_WRAVUPLY              source/output/restart/wrrest.F
Chd|        PLYXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|        PLY_ACCELE                    source/assembly/ply_accele.F  
Chd|        PLY_BCS1V                     source/constraints/general/bcs/ply_bcs.F
Chd|        PLY_VITESSE                   source/assembly/ply_vitesse.F 
Chd|        REALLOCATE_I_SKYLINE          source/system/reallocate_skyline.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        SPMD_ANIM_PLY_INIT            source/mpi/anim/spmd_anim_ply_init.F
Chd|        SPMD_ANIM_PLY_VELVEC          source/mpi/anim/spmd_anim_ply_velvec.F
Chd|        SPMD_ANIM_PLY_XYZNOD          source/mpi/anim/spmd_anim_ply_xyznod.F
Chd|        SPMD_ANIM_PLY_XYZNOR          source/mpi/anim/spmd_anim_ply_xyznor.F
Chd|        SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|        SPMD_EXCHI_A_PON              source/mpi/forces/spmd_exchi_a_pon.F
Chd|        SPMD_IGET_PARTN_PLY           source/mpi/anim/spmd_iget_partn_ply.F
Chd|        TENSORC_PLY                   source/output/anim/generate/tensorc_ply.F
Chd|        THNOD                         source/output/th/thnod.F      
Chd|        THNOD_COUNT                   source/output/th/thnod_count.F
Chd|        VELVEC                        source/output/anim/generate/velvec.F
Chd|        XYZNOD_PLY                    source/output/anim/generate/xyznod_ply.F
Chd|        XYZNOR_PLY                    source/output/anim/generate/xyznor_ply.F
Chd|        GET_PLY_ELEM                  source/output/anim/generate/anin_ply_init.F
Chd|        GET_PLY_NOD                   source/output/anim/generate/anin_ply_init.F
Chd|        GET_PLY_NODID                 source/output/anim/generate/anin_ply_init.F
Chd|-- calls ---------------
Chd|====================================================================
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

