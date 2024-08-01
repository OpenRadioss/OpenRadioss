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
      !||====================================================================
      !||    names_and_titles_mod           ../common_source/modules/names_and_titles_mod.F
      !||--- called by ------------------------------------------------------
      !||    alloc_surf_str                 ../engine/source/groups/alloc_surf_str.F
      !||    ancmsg                         ../engine/source/output/message/message.F
      !||    anim_dcod_key_0                ../engine/source/output/anim/reader/anim_dcod_key_0.F
      !||    build_msg                      ../common_source/tools/memory/my_alloc.F
      !||    contrl                         ../starter/source/starter/contrl.F
      !||    create_ellipse_clause          ../starter/source/model/sets/create_ellipse_clause.F
      !||    create_h3d_1d_scalar           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
      !||    create_h3d_1d_tensor           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
      !||    create_h3d_1d_torsor           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
      !||    create_h3d_1d_vector           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
      !||    create_h3d_arg_keyword         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
      !||    create_h3d_input               ../engine/source/output/h3d/h3d_build_fortran/create_h3d_input.F
      !||    create_h3d_nodal_scalar        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
      !||    create_h3d_nodal_tensor        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
      !||    create_h3d_nodal_vector        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
      !||    create_h3d_parts               ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts.F
      !||    create_h3d_parts_all           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
      !||    create_h3d_quad_scalar         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
      !||    create_h3d_quad_tensor         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
      !||    create_h3d_quad_vector         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
      !||    create_h3d_shell_scalar        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
      !||    create_h3d_shell_tensor        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
      !||    create_h3d_shell_vector        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
      !||    create_h3d_skin_scalar         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
      !||    create_h3d_skin_tensor         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
      !||    create_h3d_skin_vector         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
      !||    create_h3d_solid_scalar        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
      !||    create_h3d_solid_tensor        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
      !||    create_h3d_solid_vector        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
      !||    create_h3d_sph_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
      !||    create_h3d_sph_tensor          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
      !||    create_plane_clause            ../starter/source/model/sets/create_plane_clause.F90
      !||    ddsplit                        ../starter/source/restart/ddsplit/ddsplit.F
      !||    desout                         ../starter/source/output/outp/desout.F
      !||    fail_param_mod                 ../common_source/modules/mat_elem/fail_param_mod.F90
      !||    fraleonoff                     ../engine/source/input/fraleonoff.F
      !||    fralnk                         ../engine/source/input/fralnk.F
      !||    fralnk2                        ../engine/source/input/fralnk.F
      !||    freabf                         ../engine/source/input/freabf.F
      !||    freanim                        ../engine/source/output/anim/reader/freanim.F
      !||    frecpl                         ../engine/source/input/frecpl.F
      !||    fredec2i                       ../engine/source/output/message/fredec2i.F
      !||    fredec3                        ../engine/source/output/message/fredec3.F
      !||    fredec_8key_i                  ../engine/source/input/fredec_8key_i.F
      !||    fredynain                      ../engine/source/input/fredynain.F
      !||    freform                        ../engine/source/input/freform.F
      !||    freimpl                        ../engine/source/input/freimpl.F
      !||    freiniv                        ../engine/source/input/freiniv.F
      !||    frelnk                         ../engine/source/input/frelnk.F
      !||    frenois                        ../engine/source/input/frenois.F
      !||    freoutp                        ../engine/source/input/freoutp.F
      !||    frestat                        ../engine/source/input/frestat.F
      !||    freupwm                        ../engine/source/input/freupwm.F
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    genh3d                         ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    groupdef_mod                   ../common_source/modules/groupdef_mod.F
      !||    h3d_gene_keyword               ../engine/source/output/h3d/input_list/h3d_gene_keyword.F
      !||    h3d_nodal_scalar               ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
      !||    h3d_nodal_tensor               ../engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
      !||    h3d_nodal_vector               ../engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
      !||    h3d_oned_scalar                ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F
      !||    h3d_oned_tensor                ../engine/source/output/h3d/h3d_results/h3d_oned_tensor.F
      !||    h3d_oned_torsor                ../engine/source/output/h3d/h3d_results/h3d_oned_torsor.F
      !||    h3d_oned_vector                ../engine/source/output/h3d/h3d_results/h3d_oned_vector.F
      !||    h3d_pre_skin_scalar            ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_quad_scalar                ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_quad_scalar_1              ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    h3d_quad_tensor                ../engine/source/output/h3d/h3d_results/h3d_quad_tensor.F
      !||    h3d_quad_vector                ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
      !||    h3d_read                       ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    h3d_shell_scalar               ../engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
      !||    h3d_shell_scalar_1             ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_tensor               ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
      !||    h3d_shell_vector               ../engine/source/output/h3d/h3d_results/h3d_shell_vector.F
      !||    h3d_shell_vector_1             ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
      !||    h3d_skin_scalar                ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_tensor                ../engine/source/output/h3d/h3d_results/h3d_skin_tensor.F
      !||    h3d_skin_vector                ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    h3d_sol_skin_scalar            ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
      !||    h3d_sol_skin_scalar1           ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
      !||    h3d_sol_skin_tensor            ../engine/source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
      !||    h3d_solid_scalar               ../engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
      !||    h3d_solid_scalar_1             ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    h3d_solid_tensor               ../engine/source/output/h3d/h3d_results/h3d_solid_tensor.F
      !||    h3d_solid_tensor_1             ../engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
      !||    h3d_solid_vector               ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
      !||    h3d_sph_scalar                 ../engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
      !||    h3d_sph_tensor                 ../engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
      !||    hist1                          ../engine/source/output/th/hist1.F
      !||    hist13                         ../engine/source/output/th/hist13.F
      !||    hm_pre_read_preload_axial      ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_read_funct_python           ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    hm_read_preload_axial          ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    ini_msg                        ../engine/source/output/message/ini_msg.F
      !||    lech3d                         ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lecnoise                       ../engine/source/general_controls/computation/lecnoise.F
      !||    lectur                         ../engine/source/input/lectur.F
      !||    manctr                         ../engine/source/input/manctr.F
      !||    matparam_def_mod               ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    message_mod2                   ../engine/share/message_module/message_mod2.F
      !||    nolib_usermat99                ../engine/source/user_interface/nolib_usermat99.F
      !||    nvar                           ../engine/source/input/nvar.F
      !||    optiondef_mod                  ../common_source/modules/optiondef_mod.F
      !||    pblast_parameters__air_burst   ../common_source/modules/loads/pblast_mod.F
      !||    plot_curve                     ../common_source/sortie/plot_curve.F
      !||    prop_param_mod                 ../common_source/modules/mat_elem/prop_param_mod.F90
      !||    radioss2                       ../engine/source/engine/radioss2.F
      !||    rdresa                         ../engine/source/output/restart/rdresa.F
      !||    read_failparam                 ../engine/source/output/restart/read_failparam.F
      !||    read_matparam                  ../engine/source/output/restart/read_matparam.F
      !||    read_msgfile                   ../engine/source/output/message/read_msgfile.F
      !||    read_sensors                   ../engine/source/output/restart/read_sensors.F
      !||    read_viscparam                 ../engine/source/output/restart/read_viscparam.F
      !||    redkey0                        ../engine/source/input/redkey0.F
      !||    redkey1                        ../engine/source/input/redkey1.F
      !||    redkey1_h3d                    ../engine/source/input/redkey1_h3d.F
      !||    redkey2                        ../engine/source/input/redkey2.F
      !||    report                         ../engine/source/output/report/report.F
      !||    resol                          ../engine/source/engine/resol.F
      !||    resol_head                     ../engine/source/engine/resol_head.F
      !||    section_init                   ../engine/source/tools/sect/section_init.F
      !||    sensor_mod                     ../engine/share/modules/sensor_mod.F
      !||    setdef_mod                     ../common_source/modules/setdef_mod.F
      !||    starter0                       ../starter/source/starter/starter0.F
      !||    stock_msg                      ../engine/source/output/message/stock_msg.F
      !||    tmax_ipart                     ../engine/source/output/tmax_ipart.F
      !||    usermat_shell                  ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid                  ../engine/source/materials/mat_share/usermat_solid.F
      !||    visc_param_mod                 ../common_source/modules/mat_elem/visc_param_mod.F90
      !||    w_gr_entity                    ../engine/source/output/restart/w_gr_entity.F
      !||    w_line_str                     ../engine/source/output/restart/w_line_str.F
      !||    w_subset_str                   ../engine/source/output/restart/w_subset_str.F
      !||    w_surf_str                     ../engine/source/output/restart/w_surf_str.F
      !||    wciusc2                        ../engine/source/input/wciusc2.F
      !||    write_failparam                ../engine/source/output/restart/write_failparam.F
      !||    write_matparam                 ../engine/source/output/restart/write_matparam.F
      !||    write_sensors                  ../engine/source/output/restart/write_sensors.F
      !||    write_viscparam                ../engine/source/output/restart/write_viscparam.F
      !||    wriusc2                        ../engine/source/input/wriusc2.F
      !||    wrrestp                        ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE NAMES_AND_TITLES_MOD
C-----------------------------------------------
            ! Global parameters for strings
            PARAMETER (NCHARKEY      = 20)                      ! Max lengts of KEY input
            PARAMETER (NCHARLINE     = 500)                     ! Line read & write for Starter. Includes & titles can have up to 500 chars
            PARAMETER (NCHARLINE100  = 100)                     ! Line Read & write for Engine. Must be kept at size 100
            PARAMETER (NCHARTITLE    = 500)                     ! Titles / and path
            PARAMETER (LINE120       = 120)                     ! Line of size 120 / exists in some parts of code
            PARAMETER (NCHARFIELD    = 20)                      ! lengts of a field for Float input storage / 20

            ! Parameters for NAMES_AND_TITLES type
            PARAMETER (LTITLE        = 100)
            PARAMETER (LROOTNAME     = 80)
            PARAMETER (LCHRUN        = 4)
            PARAMETER (LCHRUN0       = 2)
            PARAMETER (LPATH         = 2048)
            PARAMETER (LVERS         = 32)
            PARAMETER (LIMVW_REFPATH = 100)

C-----------------------------------------------
            TYPE NAMES_AND_TITLES_
                CHARACTER(LEN=LTITLE)        :: TITLE           ! Title of Model from /TITLE option.
                CHARACTER(LEN=LROOTNAME)     :: ROOTNAME        ! ROOTNAME : Name of Input Deck
                CHARACTER(LEN=LCHRUN)        :: CHRUN           ! Run Number : 0 in starter [1-N] in Engine
                CHARACTER(LEN=LCHRUN0)       :: CHRUN0          ! Run Number0 
                CHARACTER(LEN=LVERS)         :: VERS            ! Radioss version number
                CHARACTER(LEN=LIMVW_REFPATH) :: IMVW_REFPATH    ! Refsta Directory
                INTEGER                      :: ROOTNAME_LENGTH ! LEN of ROOTNAME as in input
            END TYPE NAMES_AND_TITLES_


      CONTAINS
      !||====================================================================
      !||    names_and_title_init   ../common_source/modules/names_and_titles_mod.F
      !||--- called by ------------------------------------------------------
      !||    radioss2               ../engine/source/engine/radioss2.F
      !||    starter0               ../starter/source/starter/starter0.F
      !||====================================================================
        SUBROUTINE NAMES_AND_TITLE_INIT(NAMES_AND_TITLES)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          TYPE(NAMES_AND_TITLES_),INTENT(OUT) :: NAMES_AND_TITLES
C-----------------------------------------------
          NAMES_AND_TITLES%TITLE=' '
          NAMES_AND_TITLES%ROOTNAME=' '
          NAMES_AND_TITLES%ROOTNAME_LENGTH=0
          NAMES_AND_TITLES%CHRUN=' '
          NAMES_AND_TITLES%CHRUN0=' '
          NAMES_AND_TITLES%VERS=' '
        END SUBROUTINE NAMES_AND_TITLE_INIT

      END MODULE NAMES_AND_TITLES_MOD

      
