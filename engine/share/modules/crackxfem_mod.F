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
Chd|  CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|-- called by -----------
Chd|        ACTIV_XFEM                    source/elements/xfem/xfeoff.F 
Chd|        ANIM_XFE_INIT                 source/output/anim/generate/anim_crk_init.F
Chd|        ANIOFFC_CRK                   source/output/anim/generate/anioffc_crk.F
Chd|        ASSPAR_CRK                    source/elements/xfem/asspar_crk.F
Chd|        C3COOR3_CRK                   source/elements/xfem/c3coor3_crk.F
Chd|        C3COOR3_CRK2                  source/elements/xfem/c3coor3_crk.F
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        C3UPDT3_CRK                   source/elements/xfem/xfemfsky.F
Chd|        CCOOR3Z_CRK                   source/elements/xfem/ccoor3z_crk.F
Chd|        CCOOR3_CRK                    source/elements/xfem/ccoor3_crk.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CRKLAYER3N_ADV                source/elements/xfem/crklayer3n_adv.F
Chd|        CRKLAYER3N_INI                source/elements/xfem/crklayer3n_ini.F
Chd|        CRKLAYER4N_ADV                source/elements/xfem/crklayer4n_adv.F
Chd|        CRKLAYER4N_INI                source/elements/xfem/crklayer4n_ini.F
Chd|        CRKLEN3N_ADV                  source/elements/xfem/crklen3n_adv.F
Chd|        CRKLEN4N_ADV                  source/elements/xfem/crklen4n_adv.F
Chd|        CRKOFFC                       source/elements/xfem/precrklay.F
Chd|        CRKOFFTG                      source/elements/xfem/precrklay.F
Chd|        CRKXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|        CRK_ACCELE                    source/elements/xfem/accele_crk.F
Chd|        CRK_COORD_INI                 source/elements/xfem/inixfem.F
Chd|        CRK_TAGXP3                    source/elements/xfem/upxfem_tagxp.F
Chd|        CRK_TAGXP4                    source/elements/xfem/upxfem_tagxp.F
Chd|        CRK_VITESSE                   source/elements/xfem/crk_vitesse.F
Chd|        CRK_VITESSE2                  source/elements/xfem/crk_vitesse2.F
Chd|        CRK_ZERO_ACCEL                source/elements/xfem/crk_vitesse.F
Chd|        CRK_ZERO_FSKY                 source/elements/xfem/crk_vitesse.F
Chd|        CUPDT3_CRK                    source/elements/xfem/xfemfsky.F
Chd|        CUPDTN3_CRK                   source/elements/xfem/xfemfsky.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        DELNUMBC_CRK                  source/output/anim/generate/delnumbc_crk.F
Chd|        DFUNCC_CRK                    source/output/anim/generate/dfuncc_crk.F
Chd|        EIG                           stub/eig.F                    
Chd|        EIG1                          stub/eig1.F                   
Chd|        EIGCOND                       stub/eigcond.F                
Chd|        EIGP                          stub/eigp.F                   
Chd|        ENRICHC_INI                   source/elements/xfem/enrichc_ini.F
Chd|        ENRICHTG_INI                  source/elements/xfem/enrichtg_ini.F
Chd|        FAIL_JOHNSON_C                source/materials/fail/johnson_cook/fail_johnson_c.F
Chd|        FAIL_JOHNSON_XFEM             source/materials/fail/johnson_cook/fail_johnson_xfem.F
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|        PARSOR_CRK                    source/output/anim/generate/parsor_crk.F
Chd|        PRECRKLAY                     source/elements/xfem/precrklay.F
Chd|        PRECRKLAYTG                   source/elements/xfem/precrklay.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        SIGROTA_XFE                   source/output/anim/generate/sigrota_xfe.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        SPMD_CRK_ADV                  source/elements/xfem/xfemfsky.F
Chd|        SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|        SPMD_EXCH_CRKAVX              source/mpi/elements/spmd_xfem.F
Chd|        SPMD_EXCH_CRKVEL              source/mpi/elements/spmd_xfem.F
Chd|        SPMD_EXCH_IEDGE               source/mpi/elements/spmd_xfem.F
Chd|        SPMD_EXCH_NODENR              source/mpi/elements/spmd_xfem.F
Chd|        SPMD_EXCH_REDGE               source/mpi/elements/spmd_xfem.F
Chd|        SPMD_EXCH_TAGXP               source/mpi/elements/spmd_xfem.F
Chd|        SPMD_GATHERITAB_CRK           source/mpi/anim/spmd_gatheritab_crk.F
Chd|        SPMD_WRT_CRK_XYZNOD           source/mpi/anim/spmd_wrt_crk_xyznod.F
Chd|        SPMD_WRT_CRK_XYZNOR           source/mpi/anim/spmd_wrt_crk_xyznor.F
Chd|        TENSORC_CRK                   source/output/anim/generate/tensorc_crk.F
Chd|        UPENRIC1_N3                   source/elements/xfem/upxfem1.F
Chd|        UPENRIC1_N4                   source/elements/xfem/upxfem1.F
Chd|        UPENRIC2_N3                   source/elements/xfem/upxfem1.F
Chd|        UPENRIC2_N4                   source/elements/xfem/upxfem1.F
Chd|        UPENRIC3_N3                   source/elements/xfem/upenric3_nx.F
Chd|        UPENRIC3_N4                   source/elements/xfem/upenric3_nx.F
Chd|        UPENRIC_LAST                  source/elements/xfem/upxfem2.F
Chd|        UPENRITG_LAST                 source/elements/xfem/upxfem2.F
Chd|        UPENR_CRK                     source/elements/xfem/upenr_crk.F
Chd|        UPOFFC                        source/elements/xfem/upoffc.F 
Chd|        UPOFFTG                       source/elements/xfem/upofftg.F
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|        UPXVIT_C1                     source/elements/xfem/crk_vitesse2.F
Chd|        UPXVIT_C2                     source/elements/xfem/crk_vitesse2.F
Chd|        UPXVIT_T1                     source/elements/xfem/crk_vitesse2.F
Chd|        UPXVIT_T2                     source/elements/xfem/crk_vitesse2.F
Chd|        XFECONNEC3N                   source/output/anim/generate/xfeconnec3n.F
Chd|        XFECONNEC4N                   source/output/anim/generate/xfeconnec4n.F
Chd|        XFECUT                        source/output/anim/generate/xfecut.F
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|        XYZNOD_CRK                    source/output/anim/generate/xyznod_crk.F
Chd|        XYZNOD_CRK0                   source/output/anim/generate/xyznod_crk.F
Chd|        XYZNOR_CRK                    source/output/anim/generate/xyznor_crk.F
Chd|        XFEM2VARS_MOD                 share/modules/xfem2vars_mod.F 
Chd|-- calls ---------------
Chd|====================================================================
      MODULE CRACKXFEM_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C---------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C=======================================================================
C     Structures needed for cracking of layered shell process
C-----------------------------------------------

      TYPE XFEM_PHANTOM_
           INTEGER, DIMENSION(:,:)  ,ALLOCATABLE :: ITRI      
           INTEGER, DIMENSION(:,:,:),ALLOCATABLE :: TAGXP ! velocity link flag
           INTEGER, DIMENSION(:)    ,ALLOCATABLE :: ELCUT ! flag (global Id) of cut xfem element     
           INTEGER, DIMENSION(:)    ,ALLOCATABLE :: IFI   ! sign within partitioned superposed element     
      END TYPE XFEM_PHANTOM_

        TYPE(XFEM_PHANTOM_), DIMENSION(:), ALLOCATABLE :: XFEM_PHANTOM    ! (NXLAYMAX)
C-----------------------------------------------
      TYPE XFEM_LVSET_
           INTEGER, DIMENSION(:,:) ,ALLOCATABLE :: ENR0       ! phantom enrichment
           my_real, DIMENSION(:)   ,ALLOCATABLE :: AREA       ! phantom area ratio
      END TYPE XFEM_LVSET_

        TYPE(XFEM_LVSET_), DIMENSION(:), ALLOCATABLE :: CRKLVSET   ! NLEVMAX
C-------------------------------
      TYPE XFEM_SKY_                                 
         my_real,  DIMENSION(:,:), ALLOCATABLE :: FSKY  
      END TYPE XFEM_SKY_                             

c-------------------------------
      TYPE XFEM_AVX_
         my_real, DIMENSION(:,:), ALLOCATABLE :: X   
         my_real, DIMENSION(:,:), ALLOCATABLE :: U   
         my_real, DIMENSION(:,:), ALLOCATABLE :: V   
         my_real, DIMENSION(:,:), ALLOCATABLE :: A   
         my_real, DIMENSION(:,:), ALLOCATABLE :: AR  
         my_real, DIMENSION(:,:), ALLOCATABLE :: VR
         my_real, DIMENSION(:,:), ALLOCATABLE :: XX        ! temp coord for anim (crack positions)
      END TYPE XFEM_AVX_

        TYPE(XFEM_AVX_), DIMENSION(:), ALLOCATABLE :: CRKAVX   ! NLEVMAX
c---------------------------------------------
      TYPE XFEM_EDGE_
         INTEGER, DIMENSION(:)  , ALLOCATABLE :: LAYCUT    ! cut xfem element flag => move to XFEM_PHANTOM%ELCUT
         INTEGER, DIMENSION(:)  , ALLOCATABLE :: EDGEICRK  ! Id fissure sur un edge  
         INTEGER, DIMENSION(:,:) ,ALLOCATABLE :: IEDGEC    ! local cut edge N within element 
         INTEGER, DIMENSION(:,:) ,ALLOCATABLE :: IEDGETG   ! local cut edge N within element 
         INTEGER, DIMENSION(:,:), ALLOCATABLE :: EDGEIFI   ! signe lvset sur un edge (+/- ICRK)  
         INTEGER, DIMENSION(:,:), ALLOCATABLE :: EDGEENR   ! edge enrichment
         INTEGER, DIMENSION(:,:), ALLOCATABLE :: EDGETIP   ! flag d'edge interne/ext  
         INTEGER, DIMENSION(:)   ,ALLOCATABLE :: IBORDEDGE ! flag of border edge 
         INTEGER, DIMENSION(:)   ,ALLOCATABLE :: ICUTEDGE  ! flag of cut edge in element      
         my_real, DIMENSION(:)   ,ALLOCATABLE :: RATIO     ! ratio of cut edge   
      END TYPE XFEM_EDGE_
c
c---------------------------------------------
c
      TYPE XFEM_SHELL_
         INTEGER  CRKNUMSHELL                                ! Number of shell elements for each level
         INTEGER  CRKNUMSH3
         INTEGER  CRKNUMSH4
         INTEGER, DIMENSION(:)  , ALLOCATABLE :: CRKSHELLID ! Shell ID for global renumbering
         INTEGER, DIMENSION(:)  , ALLOCATABLE :: ELTYPE     ! local xfem element type (0=SH4N, 1=SH3N)
c
         INTEGER, DIMENSION(:,:), ALLOCATABLE :: XNODEL     ! local  phantom node num per ilev = old XFENODES
      END TYPE XFEM_SHELL_
c
      TYPE (XFEM_SHELL_), DIMENSION(:), ALLOCATABLE :: CRKSHELL    ! NLEVMAX

!!! new
!      TYPE XFEM_SHELL_
!         INTEGER  CRKNUMSHELL                                ! Number of shell elements for each level
!         INTEGER  CRKNUMSH3
!         INTEGER  CRKNUMSH4
!         INTEGER, DIMENSION(:)  , ALLOCATABLE :: ELTYPE     ! local xfem element type (0=SH4N, 1=SH3N)
!c
!         TYPE(XFEM_PHANTOM_), DIMENSION(:), ALLOCATABLE :: XPHANTOM    ! (NXLAYMAX)
!         INTEGER, DIMENSION(:,:)  , ALLOCATABLE :: CRKSHELLID ! Shell ID for global renumbering
!c
!         INTEGER, DIMENSION(:,:), ALLOCATABLE :: XNODEL     ! local  phantom node num per ilev = old XFENODES
!      END TYPE XFEM_SHELL_
C
C---------------------------------------------
C Structures needed for animation writing
C---------------------------------------------
C
      TYPE XFEM_NODES_
         INTEGER  CRKNUMNODS                                ! Number of nodes for each level (activ layer)
         INTEGER, DIMENSION(:), ALLOCATABLE :: NODES        ! Nod ID for each level
         INTEGER, DIMENSION(:), ALLOCATABLE :: CRKNODID     ! New Nod ID for Animation writing
         INTEGER, DIMENSION(:), ALLOCATABLE :: XFENODES     ! Nod ID for each level ( nodes connected to KNOD2ELC )
         INTEGER, DIMENSION(:), ALLOCATABLE :: XFECRKNODID  ! New Nod ID for Animation writing ( nodes connected to KNOD2ELC )
         INTEGER, DIMENSION(:), ALLOCATABLE :: NOD2IAD      ! for ANIM only 
      END TYPE XFEM_NODES_

      TYPE (XFEM_NODES_), DIMENSION(:), ALLOCATABLE :: CRKNOD    ! NLEVMAX
C-----------------------------
      END MODULE CRACKXFEM_MOD
