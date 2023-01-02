Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|-- called by -----------
Chd|        ALEVEC                        source/output/anim/generate/genani.F
Chd|        ALEVFLU                       source/output/anim/generate/genani.F
Chd|        ANIMBALE                      source/output/anim/generate/genani.F
Chd|        FREFORM                       source/input/freform.F        
Chd|        FVBAG0                        source/airbag/fvbag0.F        
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FVBAG2                        source/airbag/fvbag2.F        
Chd|        FVBRIC                        source/airbag/fvbric.F        
Chd|        FVCOPY                        source/airbag/fvcopy.F        
Chd|        FVDEAL                        source/airbag/fvdeal.F        
Chd|        FVDIM                         source/airbag/fvdim.F         
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVREZONE0                     source/airbag/fvrezone.F      
Chd|        FVREZONE1                     source/airbag/fvrezone.F      
Chd|        FVRREST                       source/output/restart/rdresb.F
Chd|        FVSTATS                       source/airbag/fvstats.F       
Chd|        FVSTATS1                      source/airbag/fvstats1.F      
Chd|        FVUPD0                        source/airbag/fvupd.F         
Chd|        FVUPD1                        source/airbag/fvupd.F         
Chd|        FVWREST                       source/output/restart/wrrest.F
Chd|        FV_SWITCH_CRIT                source/airbag/fvdim.F         
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        LECFVBAG                      source/input/lecfvbag.F       
Chd|        LECTUR                        source/input/lectur.F         
Chd|        RESOL                         source/engine/resol.F         
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        SPMD_EXCH_FVSTATS             source/mpi/airbags/spmd_exch_fvstats.F
Chd|        SPMD_FVB_ADIM                 source/mpi/anim/spmd_fvb_adim.F
Chd|        SPMD_FVB_AELF                 source/mpi/anim/spmd_fvb_aelf.F
Chd|        SPMD_FVB_AMON                 source/mpi/anim/spmd_fvb_amon.F
Chd|        SPMD_FVB_ANOD                 source/mpi/anim/spmd_fvb_anod.F
Chd|        SPMD_FVB_ANUM                 source/mpi/anim/spmd_fvb_anum.F
Chd|        SPMD_FVB_AOFF                 source/mpi/anim/spmd_fvb_aoff.F
Chd|        SPMD_FVB_APAR                 source/mpi/anim/spmd_fvb_apar.F
Chd|        SPMD_FVB_ASUB1                source/mpi/anim/spmd_fvb_asub1.F
Chd|        SPMD_FVB_ASUB2                source/mpi/anim/spmd_fvb_asub2.F
Chd|        SPMD_FVB_ATIT                 source/mpi/anim/spmd_fvb_atit.F
Chd|        SPMD_FVB_ATR                  source/mpi/anim/spmd_fvb_atr.F
Chd|        SPMD_FVB_AVEC                 source/mpi/anim/spmd_fvb_avec.F
Chd|        SPMD_FVB_COMM_PATTERN         source/mpi/airbags/spmd_fvb_comm_pattern.F
Chd|        SPMD_FVB_GATH                 source/mpi/airbags/spmd_fvb_gath.F
Chd|        SPMD_FVB_GATH_BEGIN           source/mpi/airbags/spmd_fvb.F 
Chd|        SPMD_FVB_GATH_END             source/mpi/airbags/spmd_fvb.F 
Chd|        SPMD_FVB_IGATH                source/mpi/airbags/spmd_fvb_igath.F
Chd|        SPMD_FVB_SCAT_NUM_NODA        source/mpi/anim/spmd_fvb_scat_num_noda.F
Chd|        SPMD_FVB_SWITCH               source/mpi/airbags/spmd_fvb_switch.F
Chd|-- calls ---------------
Chd|====================================================================
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
     .                                    RMWPOLH, VPOLH_INI
        my_real, DIMENSION(:,:), POINTER :: QPOLH
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
