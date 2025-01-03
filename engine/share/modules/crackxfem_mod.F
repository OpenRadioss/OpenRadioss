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
      !||    crackxfem_mod         ../engine/share/modules/crackxfem_mod.F
      !||--- called by ------------------------------------------------------
      !||    activ_xfem            ../engine/source/elements/xfem/activ_xfem.F
      !||    anim_xfe_init         ../engine/source/output/anim/generate/anim_crk_init.F
      !||    anioffc_crk           ../engine/source/output/anim/generate/anioffc_crk.F
      !||    asspar_crk            ../engine/source/elements/xfem/asspar_crk.F
      !||    c3coor3_crk           ../engine/source/elements/xfem/c3coor3_crk.F
      !||    c3coor3_crk2          ../engine/source/elements/xfem/c3coor3_crk.F
      !||    c3forc3               ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk           ../engine/source/elements/xfem/c3forc3_crk.F
      !||    c3updt3_crk           ../engine/source/elements/xfem/xfemfsky.F
      !||    ccoor3_crk            ../engine/source/elements/xfem/ccoor3_crk.F
      !||    ccoor3z_crk           ../engine/source/elements/xfem/ccoor3z_crk.F
      !||    cforc3                ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk            ../engine/source/elements/xfem/cforc3_crk.F
      !||    crk_accele            ../engine/source/elements/xfem/accele_crk.F
      !||    crk_coord_ini         ../engine/source/elements/xfem/crk_coord_ini.F
      !||    crk_tagxp3            ../engine/source/elements/xfem/crk_tagxp3.F
      !||    crk_tagxp4            ../engine/source/elements/xfem/crk_tagxp4.F
      !||    crk_vitesse           ../engine/source/elements/xfem/crk_vitesse.F
      !||    crk_vitesse2          ../engine/source/elements/xfem/crk_vitesse2.F
      !||    crk_zero_accel        ../engine/source/elements/xfem/crk_vitesse.F
      !||    crk_zero_fsky         ../engine/source/elements/xfem/crk_vitesse.F
      !||    crklayer3n_adv        ../engine/source/elements/xfem/crklayer3n_adv.F
      !||    crklayer3n_ini        ../engine/source/elements/xfem/crklayer3n_ini.F
      !||    crklayer4n_adv        ../engine/source/elements/xfem/crklayer4n_adv.F
      !||    crklayer4n_ini        ../engine/source/elements/xfem/crklayer4n_ini.F
      !||    crklen3n_adv          ../engine/source/elements/xfem/crklen3n_adv.F
      !||    crklen4n_adv          ../engine/source/elements/xfem/crklen4n_adv.F
      !||    crkoffc               ../engine/source/elements/xfem/precrklay.F
      !||    crkofftg              ../engine/source/elements/xfem/precrklay.F
      !||    crkxfem_wrestanim     ../engine/source/output/restart/wrrest.F
      !||    cupdt3_crk            ../engine/source/elements/xfem/xfemfsky.F
      !||    cupdtn3_crk           ../engine/source/elements/xfem/xfemfsky.F
      !||    czforc3               ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk           ../engine/source/elements/xfem/czforc3_crk.F
      !||    delnumbc_crk          ../engine/source/output/anim/generate/delnumbc_crk.F
      !||    dfuncc_crk            ../engine/source/output/anim/generate/dfuncc_crk.F
      !||    eig                   ../engine/stub/eig.F
      !||    eig1                  ../engine/stub/eig1.F
      !||    eigcond               ../engine/stub/eigcond.F
      !||    eigp                  ../engine/stub/eigp.F
      !||    enrichc_ini           ../engine/source/elements/xfem/enrichc_ini.F
      !||    enrichtg_ini          ../engine/source/elements/xfem/enrichtg_ini.F
      !||    fail_johnson_c        ../engine/source/materials/fail/johnson_cook/fail_johnson_c.F
      !||    fail_johnson_xfem     ../engine/source/materials/fail/johnson_cook/fail_johnson_xfem.F
      !||    forintc               ../engine/source/elements/forintc.F
      !||    genani                ../engine/source/output/anim/generate/genani.F
      !||    genh3d                ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    imp_buck              ../engine/source/implicit/imp_buck.F
      !||    inixfem               ../engine/source/elements/xfem/inixfem.F
      !||    parsor_crk            ../engine/source/output/anim/generate/parsor_crk.F
      !||    precrklay             ../engine/source/elements/xfem/precrklay.F
      !||    precrklaytg           ../engine/source/elements/xfem/precrklay.F
      !||    resol                 ../engine/source/engine/resol.F
      !||    resol_init            ../engine/source/engine/resol_init.F
      !||    sigrota_xfe           ../engine/source/output/anim/generate/sigrota_xfe.F
      !||    sortie_main           ../engine/source/output/sortie_main.F
      !||    spmd_crk_adv          ../engine/source/elements/xfem/xfemfsky.F
      !||    spmd_exch2_a_pon      ../engine/source/mpi/forces/spmd_exch2_a_pon.F
      !||    spmd_exch_crkavx      ../engine/source/mpi/elements/spmd_xfem.F
      !||    spmd_exch_crkvel      ../engine/source/mpi/elements/spmd_xfem.F
      !||    spmd_exch_iedge       ../engine/source/mpi/elements/spmd_xfem.F
      !||    spmd_exch_nodenr      ../engine/source/mpi/elements/spmd_xfem.F
      !||    spmd_exch_redge       ../engine/source/mpi/elements/spmd_xfem.F
      !||    spmd_exch_tagxp       ../engine/source/mpi/elements/spmd_xfem.F
      !||    spmd_gatheritab_crk   ../engine/source/mpi/anim/spmd_gatheritab_crk.F
      !||    spmd_wrt_crk_xyznod   ../engine/source/mpi/anim/spmd_wrt_crk_xyznod.F
      !||    spmd_wrt_crk_xyznor   ../engine/source/mpi/anim/spmd_wrt_crk_xyznor.F
      !||    tensorc_crk           ../engine/source/output/anim/generate/tensorc_crk.F
      !||    upenr_crk             ../engine/source/elements/xfem/upenr_crk.F
      !||    upenric1_n3           ../engine/source/elements/xfem/upenric1_n3.F
      !||    upenric1_n4           ../engine/source/elements/xfem/upenric1_n4.F
      !||    upenric2_n3           ../engine/source/elements/xfem/upenric2_n3.F
      !||    upenric2_n4           ../engine/source/elements/xfem/upenric2_n4.F
      !||    upenric3_n3           ../engine/source/elements/xfem/upenric3_nx.F
      !||    upenric3_n4           ../engine/source/elements/xfem/upenric3_nx.F
      !||    upenric_last          ../engine/source/elements/xfem/upenric_last.F
      !||    upenritg_last         ../engine/source/elements/xfem/upenritg_last.F
      !||    upoffc                ../engine/source/elements/xfem/upoffc.F
      !||    upofftg               ../engine/source/elements/xfem/upofftg.F
      !||    upxfem1               ../engine/source/elements/xfem/upxfem1.F
      !||    upxfem2               ../engine/source/elements/xfem/upxfem2.F
      !||    upxfem_tagxp          ../engine/source/elements/xfem/upxfem_tagxp.F
      !||    upxvit_c1             ../engine/source/elements/xfem/crk_vitesse2.F
      !||    upxvit_c2             ../engine/source/elements/xfem/crk_vitesse2.F
      !||    upxvit_t1             ../engine/source/elements/xfem/crk_vitesse2.F
      !||    upxvit_t2             ../engine/source/elements/xfem/crk_vitesse2.F
      !||    xfeconnec3n           ../engine/source/output/anim/generate/xfeconnec3n.F
      !||    xfeconnec4n           ../engine/source/output/anim/generate/xfeconnec4n.F
      !||    xfecut                ../engine/source/output/anim/generate/xfecut.F
      !||    xfem2vars_mod         ../engine/share/modules/xfem2vars_mod.F
      !||    xfeoff                ../engine/source/elements/xfem/xfeoff.F
      !||    xyznod_crk            ../engine/source/output/anim/generate/xyznod_crk.F
      !||    xyznod_crk0           ../engine/source/output/anim/generate/xyznod_crk.F
      !||    xyznor_crk            ../engine/source/output/anim/generate/xyznor_crk.F
      !||====================================================================
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
