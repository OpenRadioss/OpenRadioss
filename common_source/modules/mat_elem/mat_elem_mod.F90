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
!Chd|====================================================================
!Chd|  mat_elem_mod                  modules/mat_elem/mat_elem_mod.f
!Chd|-- called by -----------
!Chd|        c3init3                       starter/source/elements/sh3n/coque3n/c3init3.f
!Chd|        check_pthickfail              starter/source/materials/fail/check_pthickfail.f
!Chd|        contrl                        starter/source/starter/contrl.f
!Chd|        ddsplit                       starter/source/restart/ddsplit/ddsplit.f
!Chd|        elbuf_ini                     starter/source/elements/elbuf_init/elbuf_ini.f
!Chd|        fail_windshield_init          starter/source/materials/fail/windshield_alter/fail_windshield_init.f
!Chd|        hm_read_part                  starter/source/model/assembling/hm_read_part.f
!Chd|        hm_read_perturb               starter/source/general_controls/computation/hm_read_perturb.f
!Chd|        hm_read_perturb_fail          starter/source/general_controls/computation/hm_read_perturb_fail.f
!Chd|        ini_mat_elem                  starter/source/materials/mat/ini_mat_elem.f
!Chd|        lectur                        starter/source/starter/lectur.f
!Chd|        mmain                         starter/source/materials/mat_share/mmain.f
!Chd|        read_material_models          starter/source/materials/read_material_models.f
!Chd|        s4refsta3                     starter/source/elements/solid/solide4/s4refsta3.f
!Chd|        srefsta3                      starter/source/elements/solid/solide/srefsta3.f
!Chd|        starter0                      starter/source/starter/starter0.f
!Chd|        st_qaprint_driver             starter/source/output/qaprint/st_qaprint_driver.f
!Chd|        st_qaprint_materials          starter/source/output/qaprint/st_qaprint_materials.f
!Chd|        write_matparam                starter/source/materials/mat/write_matparam.f
!Chd|        alemain                       engine/source/ale/alemain.f   
!Chd|        bforc2                        engine/source/ale/bimat/bforc2.f
!Chd|        c3forc3                       engine/source/elements/sh3n/coque3n/c3forc3.f
!Chd|        c3forc3_crk                   engine/source/elements/xfem/c3forc3_crk.f
!Chd|        cbaforc3                      engine/source/elements/shell/coqueba/cbaforc3.f
!Chd|        cdk6forc3                     engine/source/elements/sh3n/coquedk6/cdk6forc3.f
!Chd|        cdkforc3                      engine/source/elements/sh3n/coquedk/cdkforc3.f
!Chd|        cforc3                        engine/source/elements/shell/coque/cforc3.f
!Chd|        cforc3_crk                    engine/source/elements/xfem/cforc3_crk.f
!Chd|        cmain3                        engine/source/materials/mat_share/cmain3.f
!Chd|        czforc3                       engine/source/elements/shell/coquez/czforc3.f
!Chd|        czforc3_crk                   engine/source/elements/xfem/czforc3_crk.f
!Chd|        delamination                  engine/source/properties/composite_options/stack/delamination.f
!Chd|        dfuncs                        engine/source/output/anim/generate/dfunc6.f
!Chd|        elbuf_ini                     engine/source/elements/elbuf/elbuf_ini.f
!Chd|        fail_beam18                   engine/source/elements/beam/fail_beam18.f
!Chd|        fail_beam3                    engine/source/elements/beam/fail_beam3.f
!Chd|        fail_setoff_c                 engine/source/materials/fail/fail_setoff_c.f
!Chd|        fail_setoff_npg_c             engine/source/materials/fail/fail_setoff_npg_c.f
!Chd|        fail_setoff_wind_frwave       engine/source/materials/fail/fail_setoff_wind_frwave.f
!Chd|        forint                        engine/source/elements/forint.f
!Chd|        forintc                       engine/source/elements/forintc.f
!Chd|        forintp                       engine/source/elements/forintp.f
!Chd|        genstat                       engine/source/output/sta/genstat.f
!Chd|        h3d_skin_scalar               engine/source/output/h3d/h3d_results/h3d_skin_scalar.f
!Chd|        h3d_solid_scalar_1            engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.f
!Chd|        h3d_sol_skin_scalar           engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.f
!Chd|        h3d_sol_skin_scalar1          engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.f
!Chd|        ig3duforc3                    engine/source/elements/ige3d/ig3duforc3.f
!Chd|        lecstat                       engine/source/input/lecstat.f 
!Chd|        lectur                        engine/source/input/lectur.f  
!Chd|        main_beam18                   engine/source/elements/beam/main_beam18.f
!Chd|        main_beam3                    engine/source/elements/beam/main_beam3.f
!Chd|        mmain                         engine/source/materials/mat_share/mmain.f
!Chd|        mmain8                        engine/source/materials/mat_share/mmain8.f
!Chd|        mulaw                         engine/source/materials/mat_share/mulaw.f
!Chd|        mulaw8                        engine/source/materials/mat_share/mulaw8.f
!Chd|        mulawc                        engine/source/materials/mat_share/mulawc.f
!Chd|        pforc3                        engine/source/elements/beam/pforc3.f
!Chd|        q4forc2                       engine/source/elements/solid_2d/quad4/q4forc2.f
!Chd|        qforc2                        engine/source/elements/solid_2d/quad/qforc2.f
!Chd|        radioss2                      engine/source/engine/radioss2.f
!Chd|        rdcomi                        engine/source/output/restart/rdcomm.f
!Chd|        rdresa                        engine/source/output/restart/rdresa.f
!Chd|        rdresb                        engine/source/output/restart/rdresb.f
!Chd|        read_elgroup_param            engine/source/output/restart/read_elgroup_param.f
!Chd|        read_matparam                 engine/source/output/restart/read_matparam.f
!Chd|        resol                         engine/source/engine/resol.f  
!Chd|        resol_head                    engine/source/engine/resol_head.f
!Chd|        s10forc3                      engine/source/elements/solid/solide10/s10forc3.f
!Chd|        s16forc3                      engine/source/elements/thickshell/solide16/s16forc3.f
!Chd|        s20forc3                      engine/source/elements/solid/solide20/s20forc3.f
!Chd|        s4forc3                       engine/source/elements/solid/solide4/s4forc3.f
!Chd|        s6cforc3                      engine/source/elements/thickshell/solide6c/s6cforc3.f
!Chd|        s8cforc3                      engine/source/elements/thickshell/solide8c/s8cforc3.f
!Chd|        s8eforc3                      engine/source/elements/solid/solide8e/s8eforc3.f
!Chd|        s8forc3                       engine/source/elements/solid/solide8/s8forc3.f
!Chd|        s8sforc3                      engine/source/elements/solid/solide8s/s8sforc3.f
!Chd|        s8zforc3                      engine/source/elements/solid/solide8z/s8zforc3.f
!Chd|        scforc3                       engine/source/elements/thickshell/solidec/scforc3.f
!Chd|        sforc3                        engine/source/elements/solid/solide/sforc3.f
!Chd|        sortie_main                   engine/source/output/sortie_main.f
!Chd|        spstres                       engine/source/elements/sph/spstres.f
!Chd|        stat_c_fail                   engine/source/output/sta/stat_c_fail.f
!Chd|        stat_s_fail                   engine/source/output/sta/stat_s_fail.f
!Chd|        suforc3                       engine/source/user_interface/suforc3.f
!Chd|        suser43                       engine/source/elements/solid/sconnect/suser43.f
!Chd|        szforc3                       engine/source/elements/solid/solidez/szforc3.f
!Chd|        usermat_shell                 engine/source/materials/mat_share/usermat_shell.f
!Chd|        usermat_solid                 engine/source/materials/mat_share/usermat_solid.f
!Chd|        write_matparam                engine/source/output/restart/write_matparam.f
!Chd|        wrrestp                       engine/source/output/restart/wrrestp.f
!Chd|-- calls ---------------
!Chd|        elbufdef_mod                  modules/mat_elem/elbufdef_mod.f
!Chd|        group_param_mod               modules/mat_elem/group_param_mod.f
!Chd|        matparam_def_mod              modules/mat_elem/matparam_def_mod.f
!Chd|        prop_param_mod                modules/mat_elem/prop_param_mod.f
!Chd|====================================================================

      module mat_elem_mod
      
! ======================================================================================================================
!! \brief module to host the top level material, property and element datatype
!! \details 

      use elbufdef_mod
      use matparam_def_mod
      use prop_param_mod
      use group_param_mod

! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------

        type mat_elem_
           integer :: ngroup       !< number of element groups
           integer :: nummat       !< number of /mat  cards
           integer :: numgeo       !< number of /prop cards
           integer :: numsubstack  !< number of substack created from /prop/type17, type51 or type52 => ns_stack (see stackgroup.F)
           integer :: numstack     !< number of /stack  used with /pcompp
           integer :: numply       !< number of /ply    used with /pcompp

           type (elbuf_struct_)    ,dimension(:)   ,allocatable :: elbuf          !< global element group buffer structure
           type (elbuf_struct_)    ,dimension(:,:) ,allocatable :: xfem_tab       !< element buffer for xfem elements      
           type (group_param_)     ,dimension(:)   ,allocatable :: group_param    !< common element group data
        
           type (matparam_struct_) ,dimension(:)   ,pointer     :: mat_param      !< material model data structure
        
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_param     !< element property data structure
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_stack     !< element stack data
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_ply       !< element ply data
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_substack  !< element substack data      
     
        end type mat_elem_

      end module mat_elem_mod
