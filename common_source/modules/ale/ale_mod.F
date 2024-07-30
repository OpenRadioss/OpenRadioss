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
C-----------------------------------------------
      !||====================================================================
      !||    ale_mod                         ../common_source/modules/ale/ale_mod.F
      !||--- called by ------------------------------------------------------
      !||    a4momt3                         ../engine/source/elements/solid/solide4/a4momt3.F
      !||    aconve                          ../engine/source/ale/aconve.F
      !||    ale51_antidiff2                 ../engine/source/ale/ale51/ale51_antidiff2.F
      !||    ale51_antidiff3                 ../engine/source/ale/ale51/ale51_antidiff3.F
      !||    ale51_antidiff3_int22           ../engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
      !||    ale_connectivity_init           ../common_source/modules/ale/ale_connectivity_mod.F
      !||    ale_euler_init                  ../starter/source/materials/ale/ale_euler_init.F
      !||    alemain                         ../engine/source/ale/alemain.F
      !||    alesub1                         ../engine/source/ale/subcycling/alesub1.F
      !||    alesub2                         ../engine/source/ale/subcycling/alesub2.F
      !||    alethe                          ../engine/source/ale/alethe.F
      !||    alew                            ../engine/source/ale/grid/alew.F
      !||    alew1                           ../engine/source/ale/grid/alew1.F
      !||    alew2                           ../engine/source/ale/grid/alew2.F
      !||    alew4                           ../engine/source/ale/grid/alew4.F
      !||    alew5                           ../engine/source/ale/grid/alew5.F
      !||    alew7                           ../engine/source/ale/grid/alew7.F
      !||    alewdx                          ../engine/source/ale/grid/alewdx.F
      !||    amomt2                          ../engine/source/ale/ale2d/amomt2.F
      !||    amomt3                          ../engine/source/elements/solid/solide/amomt3.F
      !||    amomtn3                         ../engine/source/elements/solid/solide/amomtn3.F
      !||    asspar2                         ../engine/source/assembly/asspar2.F
      !||    asspar4                         ../engine/source/assembly/asspar4.F
      !||    contrl                          ../starter/source/starter/contrl.F
      !||    ddsplit                         ../starter/source/restart/ddsplit/ddsplit.F
      !||    definter                        ../starter/source/interfaces/interf1/definter.F
      !||    dtnoda                          ../engine/source/time_step/dtnoda.F
      !||    ebcs10                          ../engine/source/boundary_conditions/ebcs/ebcs10.F
      !||    ebcs_vol2seg                    ../engine/source/boundary_conditions/ebcs/ebcs_vol2seg.F
      !||    emomt2                          ../engine/source/ale/euler2d/emomt2.F
      !||    emomt3b                         ../engine/source/elements/solid/solide/emomt3b.F
      !||    epxle3                          ../engine/source/elements/solid/solide/epxle3.F
      !||    fdlen3                          ../engine/source/elements/solid/solide/fdlen3.F
      !||    ffint3                          ../engine/source/elements/solid/solide/ffint3.F
      !||    fhvis3                          ../engine/source/elements/solid/solide/fhvis3.F
      !||    fmqviscb                        ../engine/source/materials/mat_share/fmqviscb.F
      !||    forint                          ../engine/source/elements/forint.F
      !||    freform                         ../engine/source/input/freform.F
      !||    freupwind                       ../engine/source/input/freupwind.F
      !||    freupwm                         ../engine/source/input/freupwm.F
      !||    frho3                           ../engine/source/elements/solid/solide/frho3.F
      !||    gravit_fvm_fem                  ../engine/source/loads/general/grav/gravit_fvm_fem.F
      !||    hm_read_ale_grid                ../starter/source/general_controls/ale_grid/hm_read_ale_grid.F
      !||    hm_read_caa                     ../starter/source/general_controls/computation/hm_read_caa.F
      !||    hm_read_inter_type02            ../starter/source/interfaces/int02/hm_read_inter_type02.F
      !||    hm_read_mat11                   ../starter/source/materials/mat/mat011/hm_read_mat11.F
      !||    hm_read_mat11_k_eps             ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
      !||    hm_read_mat51                   ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_part                    ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_prop14                  ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop14f                 ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop15                  ../starter/source/properties/solid/hm_read_prop15.F
      !||    hm_read_properties              ../starter/source/properties/hm_read_properties.F
      !||    i12m2s                          ../engine/source/interfaces/interf/i12m2s.F
      !||    i12nod3                         ../engine/source/interfaces/interf/i12nod3.F
      !||    i12s2m                          ../engine/source/interfaces/interf/i12s2m.F
      !||    ig3dmass3                       ../starter/source/elements/ige3d/ig3dmass3.F
      !||    ingrbric_nodes                  ../starter/source/interfaces/interf1/ingrbric_nodes.F
      !||    iniresa                         ../engine/source/output/restart/rdresa.F
      !||    intti12a                        ../engine/source/interfaces/interf/intti12.F
      !||    lecint                          ../starter/source/interfaces/interf1/lecint.F
      !||    lectur                          ../engine/source/input/lectur.F
      !||    m46law                          ../engine/source/materials/mat/mat046/m46law.F
      !||    manctr                          ../engine/source/input/manctr.F
      !||    meint                           ../engine/source/materials/mat_share/meint.F
      !||    mmain                           ../engine/source/materials/mat_share/mmain.F90
      !||    mqviscb                         ../engine/source/materials/mat_share/mqviscb.F
      !||    qdlen2                          ../engine/source/elements/solid_2d/quad/qdlen2.F
      !||    qforc2                          ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    qhvis2                          ../engine/source/elements/solid_2d/quad/qhvis2.F
      !||    qmasi2                          ../starter/source/elements/solid_2d/quad/qmasi2.F
      !||    qmasi2b                         ../starter/source/elements/solid_2d/quad/qmasi2b.F
      !||    r2r_split                       ../starter/source/coupling/rad2rad/r2r_split.F
      !||    r_bufbric_22                    ../engine/source/interfaces/int22/r_bufbric_22.F
      !||    radioss2                        ../engine/source/engine/radioss2.F
      !||    rdcomi                          ../engine/source/output/restart/rdcomm.F
      !||    rdcomr                          ../engine/source/output/restart/rdcomm.F
      !||    rdresb                          ../engine/source/output/restart/rdresb.F
      !||    read_ale_grid                   ../engine/source/output/restart/read_ale_grid.F90
      !||    resol                           ../engine/source/engine/resol.F
      !||    restalloc                       ../engine/source/output/restart/arralloc.F
      !||    s4forc3                         ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s4mass3                         ../starter/source/elements/solid/solide4/s4mass3.F
      !||    sdlen3                          ../engine/source/elements/solid/solide/sdlen3.F
      !||    seggetv                         ../engine/source/interfaces/interf/seggetv.F
      !||    segvar_create                   ../engine/share/modules/segvar_mod.F
      !||    sfint3                          ../engine/source/elements/solid/solide/sfint3.F
      !||    sforc3                          ../engine/source/elements/solid/solide/sforc3.F
      !||    shvis3                          ../engine/source/elements/solid/solide/shvis3.F
      !||    sigeps46                        ../engine/source/materials/mat/mat046/sigeps46.F
      !||    smass3                          ../starter/source/elements/solid/solide/smass3.F
      !||    smass3b                         ../starter/source/elements/solid/solid8p/smass3b.F
      !||    spmd_exch_flow_tracking_data    ../engine/source/ale/grid/spmd_exch_flow_tracking_data.F90
      !||    spmd_exch_flow_tracking_data2   ../engine/source/ale/grid/spmd_exch_flow_tracking_data2.F90
      !||    spmd_exch_flow_tracking_data3   ../engine/source/ale/grid/spmd_exch_flow_tracking_data3.F90
      !||    spmd_exch_flow_tracking_data4   ../engine/source/ale/grid/spmd_exch_flow_tracking_data4.F90
      !||    spmd_sd_xv                      ../engine/source/mpi/nodes/spmd_sd_xv.F
      !||    spmd_segcom                     ../engine/source/mpi/fluid/spmd_cfd.F
      !||    srho3                           ../engine/source/elements/solid/solide/srho3.F
      !||    st_qaprint_ale_options_driver   ../starter/source/output/qaprint/st_qaprint_ale_options_driver.F
      !||    st_qaprint_general_controls     ../starter/source/output/qaprint/st_qaprint_general_controls.F
      !||    starter0                        ../starter/source/starter/starter0.F
      !||    sz_print                        ../engine/source/output/restart/arralloc.F
      !||    szforc3                         ../engine/source/elements/solid/solidez/szforc3.F
      !||    upwind                          ../engine/source/elements/solid/solide/upwind.F
      !||    upwind4                         ../engine/source/elements/solid/solide4/upwind4.F
      !||    upwind_v                        ../engine/source/elements/solid/solide/upwind_v.F
      !||    varcondec                       ../engine/source/interfaces/interf/varcondec.F
      !||    wrcomi                          ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                         ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrcomr                          ../engine/source/output/restart/wrcomm.F
      !||    write_ale_grid                  ../common_source/output/restart/write_ale_grid.F90
      !||    wrrestp                         ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE ALE_MOD
      
        IMPLICIT NONE
#include      "my_real.inc"

        ! specific buffer for grid formulation 7 (/ALE/GRID/FLOW-TRACKING)
        TYPE FLOW_TRACKING_DATA_
          my_real :: EP(9) ! L=GRAD V
          my_real :: LD(6) ! D = 1/2*( L + t(L) ) !strain rate
          my_real :: LW(3) ! W = 1/2*( L - t(L) ) !rotation
          my_real :: ITM_L(6) ! inertia tensor matrix (Ixx,Iyy,Izz,-Ixy,-Iyz,-Ixz)
          my_real :: X_min_max(6) ! Xmin,Ymin,Zmin,Xmax,Ymax,Zmax
          my_real :: X_min_max_GRID(6)
          my_real :: BETA0(6)
          my_real :: COG_L(3) ! mass * X accumulation
          my_real :: MOM_L(3) ! mass * VEL accumulation
          my_real :: SUM_M    ! mass accumulation
          my_real :: SUM_VOL   ! volume accumulation
          my_real :: MS_ELEM_MEAN_0
          my_real :: EIGENVEC(3,3)
          my_real :: LD_NORM
          INTEGER :: NUM_ELEM_ALE
          INTEGER :: NUM_NODE_ALE_L
        END TYPE FLOW_TRACKING_DATA_

        ! GRID FORMULATION PARAMETERS
        TYPE ALE_GRID_
          my_real ALPHA
          my_real GAMMA
          my_real VGX
          my_real VGY
          my_real VGZ
          my_real VGY0
          my_real VGZ0          
          INTEGER NWALE_ENGINE       !        read from Engine input file with /ALE/GRID/...
          INTEGER NWALE_RST          !        read from RESTART FILE
          INTEGER NWALE              !        effective value used in numerical scheme
          TYPE(FLOW_TRACKING_DATA_) FLOW_TRACKING_DATA
        END TYPE ALE_GRID_

        ! UPWIND PARAMETERS
        TYPE ALE_UPWIND_
          my_real UPWMG
          my_real UPWOG           
          my_real UPWSM
          my_real CUPWM
          my_real UPWMG2
          my_real UPWOG2
          my_real UPWSM2
          INTEGER I_SUPG_ON_OFF
          INTEGER UPWM
          INTEGER UPW_UPDATE
        END TYPE ALE_UPWIND_

        !GLOBAL PARAMETERS
        TYPE ALE_GLOBAL_
          INTEGER NALENOVS
          INTEGER ICAA
          INTEGER INCOMP
          INTEGER ISFINT
          INTEGER I_DT_NODA_ALE_ON
          INTEGER I_DT_NODA_ALE_ON_KEY
          INTEGER IDT_ALE
          LOGICAL IS_BOUNDARY_MATERIAL
          INTEGER LCONV
          INTEGER CODV(10)
          INTEGER NVCONV
          INTEGER SNALE
          INTEGER SIELVS
        END TYPE ALE_GLOBAL_

        !ALE SUBCCLING (obsolete)
        TYPE ALE_SUB_
          INTEGER IALESUB
          INTEGER IFSUB
          INTEGER IFSUBM
          INTEGER NODSUBDT
          my_real DT1SAVE
          my_real DTFSUB
          my_real DTMSUB
        END TYPE ALE_SUB_

        ! GENERAL DATA STRUCTURE USED IN SOURCE CODE
        TYPE ALE_
          TYPE(ALE_GRID_)   :: GRID
          TYPE(ALE_UPWIND_) :: UPWIND
          TYPE(ALE_GLOBAL_) :: GLOBAL
          TYPE(ALE_SUB_)    :: SUB
          contains
            procedure, pass :: init         
        END TYPE ALE_
        
C-----------------------------------------------        
        TYPE(ALE_) :: ALE
C-----------------------------------------------                
        
        CONTAINS        
      !||====================================================================
      !||    init   ../common_source/modules/ale/ale_mod.F
      !||====================================================================
          subroutine init(this)
            !initialization is done : - during Starter before reading Starter input file
            !                         - during Engine before reading Restart file and Engine input file.
            implicit none
#include "constant.inc"            
            class(ALE_), intent(inout) :: this
            !---ALE%GRID
            this%GRID%ALPHA = ZERO
            this%GRID%GAMMA = ZERO
            this%GRID%VGX = ZERO
            this%GRID%VGY = ZERO
            this%GRID%VGZ = ZERO
            this%GRID%VGY0 = ZERO
            this%GRID%VGZ0 = ZERO            
            this%GRID%NWALE_ENGINE = -1
            this%GRID%NWALE_RST = 0
            this%GRID%NWALE = 0
            this%GRID%flow_tracking_data%EP(1:9) = ZERO
            this%GRID%flow_tracking_data%LD(1:6) = ZERO
            this%GRID%flow_tracking_data%LW(1:3) = ZERO
            this%GRID%flow_tracking_data%MOM_L(1:3) = ZERO
            this%GRID%flow_tracking_data%COG_L(1:3) = ZERO
            this%GRID%flow_tracking_data%SUM_M = ZERO
            !---ALE%UPWIND
            this%UPWIND%UPWMG = ZERO
            this%UPWIND%UPWOG = ZERO           
            this%UPWIND%UPWSM = ZERO
            this%UPWIND%CUPWM = ZERO
            this%UPWIND%UPWMG2 = ZERO
            this%UPWIND%UPWOG2 = ZERO
            this%UPWIND%UPWSM2 = ZERO
            this%UPWIND%I_SUPG_ON_OFF = 0
            this%UPWIND%UPWM = 0
            this%UPWIND%UPW_UPDATE = 0
            !---ALE%GLOBAL
            this%GLOBAL%NALENOVS = 0
            this%GLOBAL%ICAA = 0
            this%GLOBAL%INCOMP = 0
            this%GLOBAL%ISFINT = 3
            this%GLOBAL%I_DT_NODA_ALE_ON = 0
            this%GLOBAL%I_DT_NODA_ALE_ON_KEY = 0
            this%GLOBAL%IDT_ALE = 0
            this%GLOBAL%IS_BOUNDARY_MATERIAL = .FALSE.
            this%GLOBAL%LCONV = 10
            this%GLOBAL%CODV(1:10) = 0
            this%GLOBAL%NVCONV = 0
            this%GLOBAL%SNALE = 0
            this%GLOBAL%SIELVS = 0
            !---ALE%SUB
            this%SUB%IALESUB = 0
            this%SUB%IFSUB = 0
            this%SUB%IFSUBM = 0
            this%SUB%NODSUBDT = 0
            this%SUB%DT1SAVE = ZERO
            this%SUB%DTFSUB = ZERO
            this%SUB%DTMSUB = ZERO                                                              
          end subroutine    
               
      END MODULE ALE_MOD

