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
C-----------------------------------------------
Chd|====================================================================
Chd|  ALE_MOD                       modules/ale/ale_mod.F         
Chd|-- called by -----------
Chd|        ALE_CONNECTIVITY_INIT         common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_EULER_INIT                starter/source/materials/ale/ale_euler_init.F
Chd|        CONTRL                        starter/source/starter/contrl.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        DEFINTER                      starter/source/interfaces/interf1/definter.F
Chd|        HM_READ_ALE_GRID              starter/source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|        HM_READ_CAA                   starter/source/general_controls/computation/hm_read_caa.F
Chd|        HM_READ_INTER_TYPE02          starter/source/interfaces/int02/hm_read_inter_type02.F
Chd|        HM_READ_MAT11                 starter/source/materials/mat/mat011/hm_read_mat11.F
Chd|        HM_READ_MAT11_K_EPS           starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
Chd|        HM_READ_MAT51                 starter/source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_PART                  starter/source/model/assembling/hm_read_part.F
Chd|        HM_READ_PROP14                starter/source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP14F               starter/source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP15                starter/source/properties/solid/hm_read_prop15.F
Chd|        HM_READ_PROPERTIES            starter/source/properties/hm_read_properties.F
Chd|        IG3DMASS3                     starter/source/elements/ige3d/ig3dmass3.F
Chd|        INGRBRIC_NODES                starter/source/interfaces/interf1/ingrbric_nodes.F
Chd|        LECINT                        starter/source/interfaces/interf1/lecint.F
Chd|        LECSTACK_PLY                  starter/source/properties/composite_options/stack/lecstack_ply.F
Chd|        QDLEN2                        starter/source/elements/solid_2d/quad/qdlen2.F
Chd|        QMASI2                        starter/source/elements/solid_2d/quad/qmasi2.F
Chd|        QMASI2B                       starter/source/elements/solid_2d/quad/qmasi2b.F
Chd|        R2R_SPLIT                     starter/source/coupling/rad2rad/r2r_split.F
Chd|        S4MASS3                       starter/source/elements/solid/solide4/s4mass3.F
Chd|        SMASS3                        starter/source/elements/solid/solide/smass3.F
Chd|        SMASS3B                       starter/source/elements/solid/solid8p/smass3b.F
Chd|        STARTER0                      starter/source/starter/starter0.F
Chd|        ST_QAPRINT_ALE_OPTIONS_DRIVER starter/source/output/qaprint/st_qaprint_ale_options_driver.F
Chd|        ST_QAPRINT_GENERAL_CONTROLS   starter/source/output/qaprint/st_qaprint_general_controls.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        WRCOMR                        starter/source/restart/ddsplit/wrcomm.F
Chd|        A4MOMT3                       engine/source/elements/solid/solide4/a4momt3.F
Chd|        ACONVE                        engine/source/ale/aconve.F    
Chd|        ALE51_ANTIDIFF2               engine/source/ale/ale51/ale51_antidiff2.F
Chd|        ALE51_ANTIDIFF3               engine/source/ale/ale51/ale51_antidiff3.F
Chd|        ALE51_ANTIDIFF3_INT22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        ALESUB1                       engine/source/ale/subcycling/alesub1.F
Chd|        ALESUB2                       engine/source/ale/subcycling/alesub2.F
Chd|        ALETHE                        engine/source/ale/alethe.F    
Chd|        ALEW                          engine/source/ale/grid/alew.F 
Chd|        ALEW1                         engine/source/ale/grid/alew1.F
Chd|        ALEW2                         engine/source/ale/grid/alew2.F
Chd|        ALEW4                         engine/source/ale/grid/alew4.F
Chd|        ALEW5                         engine/source/ale/grid/alew5.F
Chd|        ALEWDX                        engine/source/ale/grid/alewdx.F
Chd|        AMOMT2                        engine/source/ale/ale2d/amomt2.F
Chd|        AMOMT3                        engine/source/elements/solid/solide/amomt3.F
Chd|        AMOMTN3                       engine/source/elements/solid/solide/amomtn3.F
Chd|        ASSPAR2                       engine/source/assembly/asspar2.F
Chd|        ASSPAR4                       engine/source/assembly/asspar4.F
Chd|        DTNODA                        engine/source/time_step/dtnoda.F
Chd|        EBCS_VOL2SEG                  engine/source/boundary_conditions/ebcs/ebcs_vol2seg.F
Chd|        EMOMT2                        engine/source/ale/euler2d/emomt2.F
Chd|        EMOMT3B                       engine/source/elements/solid/solide/emomt3b.F
Chd|        EPXLE3                        engine/source/elements/solid/solide/epxle3.F
Chd|        FDLEN3                        engine/source/elements/solid/solide/fdlen3.F
Chd|        FFINT3                        engine/source/elements/solid/solide/ffint3.F
Chd|        FHVIS3                        engine/source/elements/solid/solide/fhvis3.F
Chd|        FMQVISCB                      engine/source/materials/mat_share/mqviscb.F
Chd|        FORINT                        engine/source/elements/forint.F
Chd|        FREFORM                       engine/source/input/freform.F 
Chd|        FREUPWIND                     engine/source/input/freupwind.F
Chd|        FREUPWM                       engine/source/input/freupwm.F 
Chd|        FRHO3                         engine/source/elements/solid/solide/frho3.F
Chd|        GRAVIT                        engine/source/loads/general/grav/gravit.F
Chd|        GRAVIT_FVM_FEM                engine/source/loads/general/grav/gravit_fvm_fem.F
Chd|        I12M2S                        engine/source/interfaces/interf/i12m2s.F
Chd|        I12NOD3                       engine/source/interfaces/interf/i12nod3.F
Chd|        I12S2M                        engine/source/interfaces/interf/i12s2m.F
Chd|        INIRESA                       engine/source/output/restart/rdresa.F
Chd|        INTTI12A                      engine/source/interfaces/interf/intti12.F
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        M46LAW                        engine/source/materials/mat/mat046/m46law.F
Chd|        MANCTR                        engine/source/input/manctr.F  
Chd|        MEINT                         engine/source/materials/mat_share/meint.F
Chd|        MMAIN                         engine/source/materials/mat_share/mmain.F
Chd|        MQVISCB                       engine/source/materials/mat_share/mqviscb.F
Chd|        QDLEN2                        engine/source/elements/solid_2d/quad/qdlen2.F
Chd|        QHVIS2                        engine/source/elements/solid_2d/quad/qhvis2.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDCOMR                        engine/source/output/restart/rdcomm.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        R_BUFBRIC_22                  engine/source/interfaces/int22/r_bufbric_22.F
Chd|        S4FORC3                       engine/source/elements/solid/solide4/s4forc3.F
Chd|        SDLEN3                        engine/source/elements/solid/solide/sdlen3.F
Chd|        SEGGETV                       engine/source/interfaces/interf/seggetv.F
Chd|        SEGVAR_CREATE                 engine/share/modules/segvar_mod.F
Chd|        SFINT3                        engine/source/elements/solid/solide/sfint3.F
Chd|        SFORC3                        engine/source/elements/solid/solide/sforc3.F
Chd|        SHVIS3                        engine/source/elements/solid/solide/shvis3.F
Chd|        SIGEPS46                      engine/source/materials/mat/mat046/sigeps46.F
Chd|        SPMD_SD_XV                    engine/source/mpi/nodes/spmd_sd_xv.F
Chd|        SPMD_SEGCOM                   engine/source/mpi/fluid/spmd_cfd.F
Chd|        SRHO3                         engine/source/elements/solid/solide/srho3.F
Chd|        SZFORC3                       engine/source/elements/solid/solidez/szforc3.F
Chd|        SZ_PRINT                      engine/source/output/restart/arralloc.F
Chd|        UPWIND                        engine/source/elements/solid/solide/upwind.F
Chd|        UPWIND4                       engine/source/elements/solid/solide4/upwind4.F
Chd|        UPWIND_V                      engine/source/elements/solid/solide/upwind.F
Chd|        VARCONDEC                     engine/source/interfaces/interf/varcondec.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRCOMR                        engine/source/output/restart/wrcomm.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ALE_MOD
      
        IMPLICIT NONE
#include      "my_real.inc"

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
        END TYPE ALE_GRID_
        
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
        
        TYPE ALE_SUB_
          INTEGER IALESUB
          INTEGER IFSUB
          INTEGER IFSUBM
          INTEGER NODSUBDT
          my_real DT1SAVE
          my_real DTFSUB
          my_real DTMSUB
        END TYPE ALE_SUB_
                
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
            this%GRID%NWALE_ENGINE = 0
            this%GRID%NWALE_RST = 0
            this%GRID%NWALE = 0   
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

