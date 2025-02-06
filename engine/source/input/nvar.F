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
      !||    nvar                      ../engine/source/input/nvar.F
      !||--- called by ------------------------------------------------------
      !||    a22conv3                  ../engine/source/ale/alefvm/cut_cells/a22conv3.F
      !||    aconv2                    ../engine/source/ale/ale2d/aconv2.F
      !||    aconv3                    ../engine/source/ale/ale3d/aconv3.F
      !||    aconve                    ../engine/source/ale/aconve.F
      !||    aleconve                  ../engine/source/ale/porous/aleconv.F
      !||    aleflow                   ../engine/source/ale/porous/aleflow.F
      !||    alefvm_init               ../engine/source/ale/alefvm/alefvm_init.F
      !||    alethe                    ../engine/source/ale/alethe.F
      !||    alloc_elbuf_imp           ../engine/source/elements/elbuf/alloc_elbuf_imp.F
      !||    allocbuf_auto             ../engine/source/elements/elbuf/allocbuf_auto.F
      !||    arezon                    ../engine/source/ale/arezon.F
      !||    copy_elbuf_1              ../engine/source/elements/elbuf/copy_elbuf_1.F
      !||    create_h3d_1d_scalar      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
      !||    create_h3d_1d_tensor      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
      !||    create_h3d_1d_torsor      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
      !||    create_h3d_1d_vector      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
      !||    create_h3d_input          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_input.F
      !||    create_h3d_nodal_scalar   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
      !||    create_h3d_nodal_tensor   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
      !||    create_h3d_nodal_vector   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
      !||    create_h3d_parts          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts.F
      !||    create_h3d_parts_all      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
      !||    create_h3d_quad_scalar    ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
      !||    create_h3d_quad_tensor    ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
      !||    create_h3d_quad_vector    ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
      !||    create_h3d_shell_scalar   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
      !||    create_h3d_shell_tensor   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
      !||    create_h3d_shell_vector   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
      !||    create_h3d_skin_scalar    ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
      !||    create_h3d_skin_tensor    ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
      !||    create_h3d_skin_vector    ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
      !||    create_h3d_solid_scalar   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
      !||    create_h3d_solid_tensor   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
      !||    create_h3d_solid_vector   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
      !||    create_h3d_sph_scalar     ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
      !||    create_h3d_sph_tensor     ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
      !||    delamination              ../engine/source/properties/composite_options/stack/delamination.F
      !||    dfuncc                    ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncc_ply                ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    fail_beam18               ../engine/source/elements/beam/fail_beam18.F
      !||    fail_beam3                ../engine/source/elements/beam/fail_beam3.F
      !||    fail_energy_b             ../engine/source/materials/fail/energy/fail_energy_b.F
      !||    fail_energy_ib            ../engine/source/materials/fail/energy/fail_energy_ib.F
      !||    fail_visual_b             ../engine/source/materials/fail/visual/fail_visual_b.F90
      !||    fail_visual_ib            ../engine/source/materials/fail/visual/fail_visual_ib.F90
      !||    fralnk                    ../engine/source/input/fralnk.F
      !||    fralnk2                   ../engine/source/input/fralnk.F
      !||    freanim                   ../engine/source/output/anim/reader/freanim.F
      !||    fredynain                 ../engine/source/input/fredynain.F
      !||    freform                   ../engine/source/input/freform.F
      !||    freimpl                   ../engine/source/input/freimpl.F
      !||    freiniv                   ../engine/source/input/freiniv.F
      !||    frelnk                    ../engine/source/input/frelnk.F
      !||    frenois                   ../engine/source/input/frenois.F
      !||    freoutp                   ../engine/source/input/freoutp.F
      !||    frestat                   ../engine/source/input/frestat.F
      !||    get_u_mnu                 ../engine/source/user_interface/upidmid.F
      !||    get_u_pnu                 ../engine/source/user_interface/upidmid.F
      !||    h3d_fld_tsh               ../engine/source/output/h3d/h3d_results/h3d_fld_tsh.F
      !||    h3d_read                  ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    hist1                     ../engine/source/output/th/hist1.F
      !||    hist2                     ../engine/source/output/th/hist2.F
      !||    iniresa                   ../engine/source/output/restart/rdresa.F
      !||    init_reac_nod             ../engine/source/output/th/init_reac_nod.F
      !||    init_th                   ../engine/source/output/th/init_th.F
      !||    init_th_group             ../engine/source/output/th/init_th_group.F
      !||    ktbuf_ini                 ../engine/source/implicit/imp_init.F
      !||    mmain                     ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8                    ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw                     ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8                    ../engine/source/materials/mat_share/mulaw8.F90
      !||    mulawc                    ../engine/source/materials/mat_share/mulawc.F90
      !||    outp_c_s                  ../engine/source/output/sty/outp_c_s.F
      !||    read_sensors              ../engine/source/output/restart/read_sensors.F
      !||    sensor_hic                ../engine/source/tools/sensor/sensor_hic.F
      !||    spmd_fxb_for_pon          ../engine/source/mpi/kinematic_conditions/spmd_fxb_for_pon.F
      !||    stat_c_auxf               ../engine/source/output/sta/stat_c_auxf.F
      !||    stat_c_fail               ../engine/source/output/sta/stat_c_fail.F
      !||    stat_p_aux                ../engine/source/output/sta/stat_p_aux.F
      !||    stat_s_fail               ../engine/source/output/sta/stat_s_fail.F
      !||    stat_size_c               ../engine/source/output/sta/stat_size.F
      !||    thcluster                 ../engine/source/output/th/thcluster.F
      !||    thcoq                     ../engine/source/output/th/thcoq.F
      !||    thcoq_count               ../engine/source/output/th/thcoq_count.F
      !||    thfxmod                   ../engine/source/output/th/thfxmod.F
      !||    thnod                     ../engine/source/output/th/thnod.F
      !||    thnod_count               ../engine/source/output/th/thnod_count.F
      !||    thnst                     ../engine/source/output/th/thnst.F
      !||    thnst_count               ../engine/source/output/th/thnst_count.F
      !||    thpout                    ../engine/source/output/th/thpout.F
      !||    thpout_count              ../engine/source/output/th/thpout_count.F
      !||    thquad                    ../engine/source/output/th/thquad.F
      !||    thquad_count              ../engine/source/output/th/thquad_count.F
      !||    thres                     ../engine/source/output/th/thres.F
      !||    thres_count               ../engine/source/output/th/thres_count.F
      !||    thrnur                    ../engine/source/output/th/thrnur.F
      !||    thsol                     ../engine/source/output/th/thsol.F
      !||    thsol_count               ../engine/source/output/th/thsol_count.F
      !||    thsph                     ../engine/source/output/th/thsph.F
      !||    thsph_count               ../engine/source/output/th/thsph_count.F
      !||    thtrus                    ../engine/source/output/th/thtrus.F
      !||    thtrus_count              ../engine/source/output/th/thtrus_count.F
      !||    upidmid_dum               ../engine/source/user_interface/upidmid.F
      !||    usermat_shell             ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid             ../engine/source/materials/mat_share/usermat_solid.F
      !||    w_elbuf_str               ../engine/source/elements/elbuf/w_elbuf_str.F
      !||    wciusc2                   ../engine/source/input/wciusc2.F
      !||    write_sensors             ../engine/source/output/restart/write_sensors.F
      !||    write_th                  ../engine/source/output/th/write_th.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod      ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      INTEGER FUNCTION NVAR(TEXT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE100):: TEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, J
      CHARACTER TEXT1*101
C-----------------------------------------------
      TEXT1(1:1)=' '
      TEXT1(2:101)=TEXT
      N=0
      DO I=2,101
        J=I-1
        IF(TEXT1(I:I) /= ' ' .AND. TEXT1(J:J) == ' ') THEN
          N=N+1
        ENDIF
      ENDDO
      NVAR=N
      RETURN
      END
