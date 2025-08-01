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
!||    fvbag_mod                     ../engine/share/modules/fvbag_mod.F
!||--- called by ------------------------------------------------------
!||    alevec                        ../engine/source/output/anim/generate/monvol_anim.F90
!||    alevflu                       ../engine/source/output/anim/generate/monvol_anim.F90
!||    anim_nodal_contour_fvmbags    ../engine/source/output/anim/generate/anim_nodal_contour_fvmbags.F
!||    anim_nodal_vector_fvmbags     ../engine/source/output/anim/generate/anim_nodal_vector_fvmbags.F
!||    animbale                      ../engine/source/output/anim/generate/monvol_anim.F90
!||    freform                       ../engine/source/input/freform.F
!||    fv_switch_crit                ../engine/source/airbag/fvdim.F
!||    fv_up_switch                  ../engine/source/airbag/fv_up_switch.F
!||    fvbag0                        ../engine/source/airbag/fvbag0.F
!||    fvbag1                        ../engine/source/airbag/fvbag1.F
!||    fvbag2                        ../engine/source/airbag/fvbag2.F
!||    fvbric                        ../engine/source/airbag/fvbric.F
!||    fvcopy                        ../engine/source/airbag/fvcopy.F
!||    fvdeal                        ../engine/source/airbag/fvdeal.F
!||    fvdim                         ../engine/source/airbag/fvdim.F
!||    fvmesh0                       ../engine/source/airbag/fvmesh0.F
!||    fvmesh1                       ../engine/source/airbag/fvmesh.F
!||    fvrezone0                     ../engine/source/airbag/fvrezone.F
!||    fvrezone1                     ../engine/source/airbag/fvrezone.F
!||    fvrrest                       ../engine/source/output/restart/rdresb.F
!||    fvstats                       ../engine/source/airbag/fvstats.F
!||    fvstats1                      ../engine/source/airbag/fvstats1.F
!||    fvupd0                        ../engine/source/airbag/fvupd.F
!||    fvupd1                        ../engine/source/airbag/fvupd.F
!||    fvwrest                       ../engine/source/output/restart/wrrest.F
!||    genani                        ../engine/source/output/anim/generate/genani.F
!||    genh3d                        ../engine/source/output/h3d/h3d_results/genh3d.F
!||    h3d_create_fvmbag_centroids   ../engine/source/output/h3d/h3d_build_fortran/h3d_create_fvmbag_centroids.F
!||    h3d_nodal_scalar              ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
!||    h3d_nodal_vector              ../engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
!||    h3d_update_fvmbag_centroids   ../engine/source/output/h3d/h3d_build_fortran/h3d_update_fvmbag_centroids.F
!||    lecfvbag                      ../engine/source/input/lecfvbag.F
!||    lectur                        ../engine/source/input/lectur.F
!||    nodald                        ../engine/source/output/anim/generate/nodald.F
!||    nodaldt                       ../engine/source/output/anim/generate/nodaldt.F
!||    nodalp                        ../engine/source/output/anim/generate/nodalp.F
!||    nodalssp                      ../engine/source/output/anim/generate/nodalssp.F
!||    nodalt                        ../engine/source/output/anim/generate/nodalt.F
!||    nodalvol                      ../engine/source/output/anim/generate/nodalvol.F
!||    resol                         ../engine/source/engine/resol.F
!||    sortie_main                   ../engine/source/output/sortie_main.F
!||    spmd_exch_fvstats             ../engine/source/mpi/airbags/spmd_exch_fvstats.F
!||    spmd_fvb_adim                 ../engine/source/mpi/anim/spmd_fvb_adim.F
!||    spmd_fvb_aelf                 ../engine/source/mpi/anim/spmd_fvb_aelf.F
!||    spmd_fvb_amon                 ../engine/source/mpi/anim/spmd_fvb_amon.F
!||    spmd_fvb_anod                 ../engine/source/mpi/anim/spmd_fvb_anod.F
!||    spmd_fvb_anum                 ../engine/source/mpi/anim/spmd_fvb_anum.F
!||    spmd_fvb_aoff                 ../engine/source/mpi/anim/spmd_fvb_aoff.F
!||    spmd_fvb_apar                 ../engine/source/mpi/anim/spmd_fvb_apar.F
!||    spmd_fvb_asub1                ../engine/source/mpi/anim/spmd_fvb_asub1.F
!||    spmd_fvb_asub2                ../engine/source/mpi/anim/spmd_fvb_asub2.F
!||    spmd_fvb_atit                 ../engine/source/mpi/anim/spmd_fvb_atit.F
!||    spmd_fvb_atr                  ../engine/source/mpi/anim/spmd_fvb_atr.F
!||    spmd_fvb_avec                 ../engine/source/mpi/anim/spmd_fvb_avec.F
!||    spmd_fvb_comm_pattern         ../engine/source/mpi/airbags/spmd_fvb_comm_pattern.F
!||    spmd_fvb_gath                 ../engine/source/mpi/airbags/spmd_fvb_gath.F
!||    spmd_fvb_gath_begin           ../engine/source/mpi/airbags/spmd_fvb.F
!||    spmd_fvb_gath_end             ../engine/source/mpi/airbags/spmd_fvb.F
!||    spmd_fvb_igath                ../engine/source/mpi/airbags/spmd_fvb_igath.F
!||    spmd_fvb_scat_num_noda        ../engine/source/mpi/anim/spmd_fvb_scat_num_noda.F
!||    spmd_fvb_switch               ../engine/source/mpi/airbags/spmd_fvb_switch.F
!||====================================================================
      MODULE FVBAG_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE T_ELEM_TO_TRI
        INTEGER :: MINUS_SIZE, PLUS_SIZE
        INTEGER, DIMENSION(:), ALLOCATABLE :: MINUS, PLUS
      END TYPE T_ELEM_TO_TRI
                 
      TYPE FVBAG_DATA
        !-------------------------------
        ! Courant number (DT scale factor) & Time Step options 
        !-------------------------------
        my_real CFL_COEF, DTMIN
        my_real LAMBDA       ! /DT/FVMBAG param 3
        my_real DTOLD        ! FVMBAG time step from previous cycle
        INTEGER L_TYPE       ! FVMBAG characteristic length option     
        INTEGER ID_DT_OPTION ! /DT/FVMBAG/[ID_DT_OPTION]
        !------------------------------------------------
        ! Dispersion of pressure around mean pressure
        !------------------------------------------------
        my_real PDISP_OLD, PDISP
        !-------------------------------
        ! Donnees briques intersectantes
        !-------------------------------
        INTEGER, DIMENSION(:,:), POINTER :: BRIC, TBRIC 
        my_real, DIMENSION(:,:), POINTER :: XB
        my_real, DIMENSION(:,:,:), POINTER :: SFAC
        !-----------
        ! Dimensions
        !-----------
        INTEGER NNS, NNTR, LENP, NPOLY, LENH, NPOLH
        !----------------
        ! Anim polyhedres
        !----------------
        INTEGER NPOLH_ANIM, NNS_ANIM, ID
        INTEGER, DIMENSION(:), POINTER :: IFVPOLY_ANIM, IFVTADR_ANIM,
     .                                    IFVPOLH_ANIM, IFVPADR_ANIM,
     .                                    REDIR_ANIM
        INTEGER, DIMENSION(:,:), POINTER :: IFVTRI_ANIM
        my_real, DIMENSION(:,:), POINTER :: NOD_ANIM
        !------------------------
        ! Polygones et polyhedres
        !------------------------
        INTEGER, DIMENSION(:), POINTER :: IFVPOLY, IFVTADR,
     .                                    IFVPOLH, IFVPADR, IDPOLH,
     .                                    IBPOLH
        INTEGER, DIMENSION(:,:), POINTER :: IFVNOD, IFVTRI
        my_real, DIMENSION(:,:), POINTER :: RFVNOD
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: TRI_TO_ELEM
        TYPE(T_ELEM_TO_TRI), DIMENSION(:), ALLOCATABLE :: ELEM_TO_TRI
        !------------------------
        ! Variables volumes finis
        !------------------------
        my_real, DIMENSION(:), POINTER :: MPOLH,   EPOLH,   PPOLH,
     .                                    RPOLH,   GPOLH,   TPOLH,
     .                                    CPAPOLH, CPBPOLH, CPCPOLH, 
     .                                    CPDPOLH, CPEPOLH, CPFPOLH,
     .                                    RMWPOLH, VPOLH_INI,
     .                                    SSPPOLH !SSPPOLH : output purpose only (sound speed in polyhedra)          
        my_real, DIMENSION(:,:), POINTER :: QPOLH     
        my_real, DIMENSION(:,:), POINTER :: CENTROID_POLH !output purpose only (centroids positions)
        
        !---------------------------
        ! Variables stabilite
        !---------------------------
        my_real DLH
        my_real, DIMENSION(:), POINTER :: DTPOLH
      END TYPE FVBAG_DATA

      TYPE FVBAG_SPMD
          INTEGER NN_L, NNA_L, NNSA, NNSA_L, NSA, NELSA, PMAIN, NNI_L,MPI_COMM, RANK, NSPMD,NNA_L_GLOB
          INTEGER, DIMENSION(:,:), POINTER :: IBUF_L, IBUFA_L, IBUFSA_L,IXSA, ELEMSA,ITAB
          ! buffers
          my_real,DIMENSION(:,:), POINTER :: GGG,GGA,AAA
          my_real,DIMENSION(:), POINTER :: RBUF ! buffer for fvupd
          INTEGER, DIMENSION(:), POINTER :: IBUF !buffer for fvupd
          INTEGER, DIMENSION(:), POINTER :: REQ,IADI,IADR
          my_real,DIMENSION(:,:), POINTER :: XXX,VVV,WAX,WAV 
      END TYPE FVBAG_SPMD

      !-------------------------------------
      ! TEMP BUFFER (ENGINE FILE PARAMETERS)
      !-------------------------------------
      !Reading Engine file option /DT/FVMBAG and store it (FREEFORM subroutine)
      ! Then use it when airbag buffer are allocated (restart file is read after FREEFORM subroutine)
      TYPE FVMBAG_INPUT_OPTIONS_
        INTEGER ID_BAG        ! airbag identifier
        INTEGER L_TYPE        ! characteristic length option
        INTEGER ID_DT_OPTION  !/DT/FVMBAG/[ID_DT_OPTION]
        my_real CFL_COEF
        my_real DTMIN        
        my_real LAMBDA        ! 3rd parameter of /DT/FVMBAG option
      END TYPE FVMBAG_INPUT_OPTIONS_
            
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s   D e f i n i t i o n
C-----------------------------------------------
      INTEGER NFVBAG
      INTEGER AIRBAGS_TOTAL_FVM_IN_H3D ! number of polyhedra/centroids at first cycle (upper bound for following cycle due to merging) Visualization purpose only.
      
      TYPE(FVBAG_DATA), DIMENSION(:), ALLOCATABLE :: FVDATA, FVDATA_OLD
      TYPE(FVBAG_SPMD), DIMENSION(:), ALLOCATABLE :: FVSPMD
C-----------------------------------------------      
      TYPE(FVMBAG_INPUT_OPTIONS_),DIMENSION(:), ALLOCATABLE :: FVMBAG_INPUT_OPTIONS
      INTEGER NUM_OPT_DT_FVMBAG   !number of option /DT/FVMBAG/[0-9] in Engine file
      INTEGER NUM_OPT_DT_FVMBAG_0 !number of option /DT/FVMBAG/0 in Engine file
      INTEGER NUM_OPT_DT_FVMBAG_1 !number of option /DT/FVMBAG/1 in Engine file
      INTEGER NUM_OPT_DT_FVMBAG_2 !number of option /DT/FVMBAG/2 in Engine file
      INTEGER NUM_OPT_DT_FVMBAG_3 !number of option /DT/FVMBAG/3 in Engine file      
C-----------------------------------------------      

      END MODULE FVBAG_MOD
