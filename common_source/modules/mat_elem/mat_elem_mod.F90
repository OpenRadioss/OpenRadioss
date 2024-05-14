!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  MAT_ELEM_MOD                  modules/mat_elem/mat_elem_mod.F
!hd|-- called by -----------
!hd|        C3INIT3                       starter/source/elements/sh3n/coque3n/c3init3.F
!hd|        CHECK_PTHICKFAIL              starter/source/materials/fail/check_pthickfail.F
!hd|        CONTRL                        starter/source/starter/contrl.F
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        ELBUF_INI                     starter/source/elements/elbuf_init/elbuf_ini.F
!hd|        FAIL_WINDSHIELD_INIT          starter/source/materials/fail/windshield_alter/fail_windshield_init.F
!hd|        HM_READ_PART                  starter/source/model/assembling/hm_read_part.F
!hd|        HM_READ_PERTURB               starter/source/general_controls/computation/hm_read_perturb.F
!hd|        HM_READ_PERTURB_FAIL          starter/source/general_controls/computation/hm_read_perturb_fail.F
!hd|        INI_MAT_ELEM                  starter/source/materials/mat/ini_mat_elem.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        MMAIN                         starter/source/materials/mat_share/mmain.F
!hd|        READ_MATERIAL_MODELS          starter/source/materials/read_material_models.F
!hd|        S4REFSTA3                     starter/source/elements/solid/solide4/s4refsta3.F
!hd|        SREFSTA3                      starter/source/elements/solid/solide/srefsta3.F
!hd|        STARTER0                      starter/source/starter/starter0.F
!hd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
!hd|        ST_QAPRINT_MATERIALS          starter/source/output/qaprint/st_qaprint_materials.F
!hd|        WRITE_MATPARAM                starter/source/materials/mat/write_matparam.F
!hd|        ALEMAIN                       engine/source/ale/alemain.F
!hd|        BFORC2                        engine/source/ale/bimat/bforc2.F
!hd|        C3FORC3                       engine/source/elements/sh3n/coque3n/c3forc3.F
!hd|        C3FORC3_CRK                   engine/source/elements/xfem/c3forc3_crk.F
!hd|        CBAFORC3                      engine/source/elements/shell/coqueba/cbaforc3.F
!hd|        CDK6FORC3                     engine/source/elements/sh3n/coquedk6/cdk6forc3.F
!hd|        CDKFORC3                      engine/source/elements/sh3n/coquedk/cdkforc3.F
!hd|        CFORC3                        engine/source/elements/shell/coque/cforc3.F
!hd|        CFORC3_CRK                    engine/source/elements/xfem/cforc3_crk.F
!hd|        CMAIN3                        engine/source/materials/mat_share/cmain3.F
!hd|        CZFORC3                       engine/source/elements/shell/coquez/czforc3.F
!hd|        CZFORC3_CRK                   engine/source/elements/xfem/czforc3_crk.F
!hd|        DELAMINATION                  engine/source/properties/composite_options/stack/delamination.F
!hd|        DFUNCS                        engine/source/output/anim/generate/dfunc6.F
!hd|        ELBUF_INI                     engine/source/elements/elbuf/elbuf_ini.F
!hd|        FAIL_BEAM18                   engine/source/elements/beam/fail_beam18.F
!hd|        FAIL_BEAM3                    engine/source/elements/beam/fail_beam3.F
!hd|        FAIL_SETOFF_C                 engine/source/materials/fail/fail_setoff_c.F
!hd|        FAIL_SETOFF_NPG_C             engine/source/materials/fail/fail_setoff_npg_c.F
!hd|        FAIL_SETOFF_WIND_FRWAVE       engine/source/materials/fail/fail_setoff_wind_frwave.F
!hd|        FORINT                        engine/source/elements/forint.F
!hd|        FORINTC                       engine/source/elements/forintc.F
!hd|        FORINTP                       engine/source/elements/forintp.F
!hd|        GENSTAT                       engine/source/output/sta/genstat.F
!hd|        H3D_SKIN_SCALAR               engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
!hd|        H3D_SOLID_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
!hd|        H3D_SOL_SKIN_SCALAR           engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!hd|        H3D_SOL_SKIN_SCALAR1          engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
!hd|        IG3DUFORC3                    engine/source/elements/ige3d/ig3duforc3.F
!hd|        LECSTAT                       engine/source/input/lecstat.F
!hd|        LECTUR                        engine/source/input/lectur.F
!hd|        MAIN_BEAM18                   engine/source/elements/beam/main_beam18.F
!hd|        MAIN_BEAM3                    engine/source/elements/beam/main_beam3.F
!hd|        MMAIN                         engine/source/materials/mat_share/mmain.F
!hd|        MMAIN8                        engine/source/materials/mat_share/mmain8.F
!hd|        MULAW                         engine/source/materials/mat_share/mulaw.F
!hd|        MULAW8                        engine/source/materials/mat_share/mulaw8.F
!hd|        MULAWC                        engine/source/materials/mat_share/mulawc.F
!hd|        PFORC3                        engine/source/elements/beam/pforc3.F
!hd|        Q4FORC2                       engine/source/elements/solid_2d/quad4/q4forc2.F
!hd|        QFORC2                        engine/source/elements/solid_2d/quad/qforc2.F
!hd|        RADIOSS2                      engine/source/engine/radioss2.F
!hd|        RDCOMI                        engine/source/output/restart/rdcomm.F
!hd|        RDRESA                        engine/source/output/restart/rdresa.F
!hd|        RDRESB                        engine/source/output/restart/rdresb.F
!hd|        READ_ELGROUP_PARAM            engine/source/output/restart/read_elgroup_param.F
!hd|        READ_MATPARAM                 engine/source/output/restart/read_matparam.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESOL_HEAD                    engine/source/engine/resol_head.F
!hd|        S10FORC3                      engine/source/elements/solid/solide10/s10forc3.F
!hd|        S16FORC3                      engine/source/elements/thickshell/solide16/s16forc3.F
!hd|        S20FORC3                      engine/source/elements/solid/solide20/s20forc3.F
!hd|        S4FORC3                       engine/source/elements/solid/solide4/s4forc3.F
!hd|        S6CFORC3                      engine/source/elements/thickshell/solide6c/s6cforc3.F
!hd|        S8CFORC3                      engine/source/elements/thickshell/solide8c/s8cforc3.F
!hd|        S8EFORC3                      engine/source/elements/solid/solide8e/s8eforc3.F
!hd|        S8FORC3                       engine/source/elements/solid/solide8/s8forc3.F
!hd|        S8SFORC3                      engine/source/elements/solid/solide8s/s8sforc3.F
!hd|        S8ZFORC3                      engine/source/elements/solid/solide8z/s8zforc3.F
!hd|        SCFORC3                       engine/source/elements/thickshell/solidec/scforc3.F
!hd|        SFORC3                        engine/source/elements/solid/solide/sforc3.F
!hd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
!hd|        SPSTRES                       engine/source/elements/sph/spstres.F
!hd|        STAT_C_FAIL                   engine/source/output/sta/stat_c_fail.F
!hd|        STAT_S_FAIL                   engine/source/output/sta/stat_s_fail.F
!hd|        SUFORC3                       engine/source/user_interface/suforc3.F
!hd|        SUSER43                       engine/source/elements/solid/sconnect/suser43.F
!hd|        SZFORC3                       engine/source/elements/solid/solidez/szforc3.F
!hd|        USERMAT_SHELL                 engine/source/materials/mat_share/usermat_shell.F
!hd|        USERMAT_SOLID                 engine/source/materials/mat_share/usermat_solid.F
!hd|        WRITE_MATPARAM                engine/source/output/restart/write_matparam.F
!hd|        WRRESTP                       engine/source/output/restart/wrrestp.F
!hd|-- calls ---------------
!hd|        ELBUFDEF_MOD                  modules/mat_elem/elbufdef_mod.F
!hd|        GROUP_PARAM_MOD               modules/mat_elem/group_param_mod.F
!hd|        MATPARAM_DEF_MOD              modules/mat_elem/matparam_def_mod.F
!hd|        PROP_PARAM_MOD                modules/mat_elem/prop_param_mod.F
!hd|====================================================================
      MODULE MAT_ELEM_MOD
        USE ELBUFDEF_MOD
        USE MATPARAM_DEF_MOD
        USE PROP_PARAM_MOD
        USE GROUP_PARAM_MOD

        TYPE MAT_ELEM_
          INTEGER :: NGROUP       ! Number of element groups
          INTEGER :: NUMMAT       ! Number of /MAT  cards
!           INTEGER :: NUMFAIL      ! Number of /FAIL cards
          INTEGER :: NUMGEO       ! Number of /PROP cards
          INTEGER :: NUMSUBSTACK  ! Number of SUBSTACK created from /PROP/TYPE17, TYPE51 or TYPE52 => NS_STACK (see stackgroup.F)
          INTEGER :: NUMSTACK     ! Number of /STACK  used with /PCOMPP
          INTEGER :: NUMPLY       ! Number of /PLY    used with /PCOMPP

          TYPE (ELBUF_STRUCT_)    ,DIMENSION(:)   ,ALLOCATABLE :: ELBUF          ! NGROUP
          TYPE (ELBUF_STRUCT_)    ,DIMENSION(:,:) ,ALLOCATABLE :: XFEM_TAB
          TYPE (GROUP_PARAM_)     ,DIMENSION(:)   ,ALLOCATABLE :: GROUP_PARAM    ! NGROUP

          TYPE (MATPARAM_STRUCT_) ,DIMENSION(:)   ,POINTER     :: MAT_PARAM      ! NUMMAT

          TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_PARAM     ! NUMGEO
          TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_STACK     ! NUMSTACK
          TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_PLY       ! NUMPLY
          TYPE (PROP_PARAM_)      ,DIMENSION(:)   ,ALLOCATABLE :: PROP_SUBSTACK  ! NUMSUBSTACK

        END TYPE MAT_ELEM_

      END MODULE MAT_ELEM_MOD
