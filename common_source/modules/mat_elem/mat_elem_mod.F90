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
!Chd|  mat_elem_mod                  modules/mat_elem/mat_elem_mod.F90
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|        elbufdef_mod                  modules/mat_elem/elbufdef_mod.F90
!Chd|        group_param_mod               modules/mat_elem/group_param_mod.F90
!Chd|        matparam_def_mod              modules/mat_elem/matparam_def_mod.F90
!Chd|        prop_param_mod                modules/mat_elem/prop_param_mod.F90
!Chd|====================================================================

      !||====================================================================
      !||    mat_elem_mod              ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||--- called by ------------------------------------------------------
      !||    alemain                   ../engine/source/ale/alemain.F
      !||    bforc2                    ../engine/source/ale/bimat/bforc2.F
      !||    c3epsini                  ../starter/source/elements/sh3n/coque3n/c3epsini.F
      !||    c3forc3                   ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk               ../engine/source/elements/xfem/c3forc3_crk.F
      !||    c3init3                   ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    cbaforc3                  ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cbainit3                  ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cdk6forc3                 ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkepsini                 ../starter/source/elements/sh3n/coquedk/cdkepsini.F
      !||    cdkforc3                  ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cdkinit3                  ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cepsini                   ../starter/source/elements/shell/coque/cepsini.F
      !||    cforc3                    ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk                ../engine/source/elements/xfem/cforc3_crk.F
      !||    check_pthickfail          ../starter/source/materials/fail/check_pthickfail.F
      !||    cmain3                    ../engine/source/materials/mat_share/cmain3.F
      !||    cmaini3                   ../starter/source/elements/sh3n/coquedk/cmaini3.F
      !||    cmlawi                    ../starter/source/elements/shell/coque/cepsini.F
      !||    cncoefort                 ../engine/source/elements/sh3n/coquedk/cncoef3.F
      !||    cnepsini                  ../starter/source/elements/shell/coqueba/cnepsini.F
      !||    contrl                    ../starter/source/starter/contrl.F
      !||    czforc3                   ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk               ../engine/source/elements/xfem/czforc3_crk.F
      !||    ddsplit                   ../starter/source/restart/ddsplit/ddsplit.F
      !||    delamination              ../engine/source/properties/composite_options/stack/delamination.F
      !||    dfuncs                    ../engine/source/output/anim/generate/dfunc6.F
      !||    elbuf_ini                 ../engine/source/elements/elbuf/elbuf_ini.F
      !||    fail_beam18               ../engine/source/elements/beam/fail_beam18.F
      !||    fail_beam3                ../engine/source/elements/beam/fail_beam3.F
      !||    fail_setoff_c             ../engine/source/materials/fail/fail_setoff_c.F
      !||    fail_setoff_npg_c         ../engine/source/materials/fail/fail_setoff_npg_c.F
      !||    fail_setoff_wind_frwave   ../engine/source/materials/fail/fail_setoff_wind_frwave.F
      !||    forint                    ../engine/source/elements/forint.F
      !||    forintc                   ../engine/source/elements/forintc.F
      !||    forintp                   ../engine/source/elements/forintp.F
      !||    fractal_dmg_init          ../starter/source/materials/fail/fractal/fractal_dmg_init.F90
      !||    genstat                   ../engine/source/output/sta/genstat.F
      !||    h3d_skin_scalar           ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_sol_skin_scalar       ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
      !||    h3d_sol_skin_scalar1      ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
      !||    h3d_solid_scalar_1        ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    hm_read_part              ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_perturb           ../starter/source/general_controls/computation/hm_read_perturb.F
      !||    hm_read_perturb_fail      ../starter/source/general_controls/computation/hm_read_perturb_fail.F
      !||    ig3duforc3                ../engine/source/elements/ige3d/ig3duforc3.F
      !||    ini_mat_elem              ../starter/source/materials/mat/ini_mat_elem.F
      !||    lecstat                   ../engine/source/input/lecstat.F
      !||    lectur                    ../engine/source/input/lectur.F
      !||    main_beam18               ../engine/source/elements/beam/main_beam18.F
      !||    main_beam3                ../engine/source/elements/beam/main_beam3.F
      !||    mmain                     ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8                    ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw                     ../engine/source/materials/mat_share/mulaw.F90
      !||    pforc3                    ../engine/source/elements/beam/pforc3.F
      !||    q4forc2                   ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2                    ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    radioss2                  ../engine/source/engine/radioss2.F
      !||    rdcomi                    ../engine/source/output/restart/rdcomm.F
      !||    rdresa                    ../engine/source/output/restart/rdresa.F
      !||    rdresb                    ../engine/source/output/restart/rdresb.F
      !||    read_elgroup_param        ../engine/source/output/restart/read_elgroup_param.F
      !||    read_material_models      ../starter/source/materials/read_material_models.F
      !||    read_matparam             ../engine/source/output/restart/read_matparam.F
      !||    resol                     ../engine/source/engine/resol.F
      !||    resol_head                ../engine/source/engine/resol_head.F
      !||    s10forc3                  ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16forc3                  ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3                  ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4forc3                   ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s4refsta3                 ../starter/source/elements/solid/solide4/s4refsta3.F
      !||    s6cforc3                  ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cforc3                  ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3                  ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3                   ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3                  ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3                  ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scforc3                   ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sforc3                    ../engine/source/elements/solid/solide/sforc3.F
      !||    sortie_main               ../engine/source/output/sortie_main.F
      !||    spstres                   ../engine/source/elements/sph/spstres.F
      !||    srefsta3                  ../starter/source/elements/solid/solide/srefsta3.F
      !||    st_qaprint_driver         ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_materials      ../starter/source/output/qaprint/st_qaprint_materials.F
      !||    starter0                  ../starter/source/starter/starter0.F
      !||    stat_c_fail               ../engine/source/output/sta/stat_c_fail.F
      !||    stat_s_fail               ../engine/source/output/sta/stat_s_fail.F
      !||    suforc3                   ../engine/source/user_interface/suforc3.F
      !||    suser43                   ../engine/source/elements/solid/sconnect/suser43.F
      !||    szforc3                   ../engine/source/elements/solid/solidez/szforc3.F
      !||    usermat_shell             ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid             ../engine/source/materials/mat_share/usermat_solid.F
      !||    write_matparam            ../engine/source/output/restart/write_matparam.F
      !||    wrrestp                   ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod              ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    group_param_mod           ../common_source/modules/mat_elem/group_param_mod.F90
      !||    matparam_def_mod          ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    prop_param_mod            ../common_source/modules/mat_elem/prop_param_mod.F90
      !||====================================================================
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
