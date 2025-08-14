!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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

!||====================================================================
!||    output_mod                         ../common_source/modules/output/output_mod.F90
!||--- called by ------------------------------------------------------
!||    alemain                            ../engine/source/ale/alemain.F
!||    anim_dcod_key_0                    ../engine/source/output/anim/reader/anim_dcod_key_0.F
!||    arret                              ../engine/source/system/arret.F
!||    bforc2                             ../engine/source/ale/bimat/bforc2.F
!||    cforc3                             ../engine/source/elements/shell/coque/cforc3.F
!||    checksum_write_starter_restart     ../starter/source/output/checksum/checksum_option.F90
!||    contrl                             ../starter/source/starter/contrl.F
!||    ddsplit                            ../starter/source/restart/ddsplit/ddsplit.F
!||    ecrit                              ../engine/source/output/ecrit.F
!||    eng_qaprint_driver                 ../engine/source/output/qaprint/eng_qaprint_driver.F
!||    eng_qaprint_generalcontrolsinput   ../engine/source/output/qaprint/eng_qaprint_generalcontrolsinput.F
!||    execargcheck                       ../engine/source/engine/execargcheck.F
!||    f_anend                            ../starter/source/output/analyse/analyse_arret.F
!||    forint                             ../engine/source/elements/forint.F
!||    forintc                            ../engine/source/elements/forintc.F
!||    forintp                            ../engine/source/elements/forintp.F
!||    freform                            ../engine/source/input/freform.F
!||    frestat                            ../engine/source/input/frestat.F
!||    genani                             ../engine/source/output/anim/generate/genani.F
!||    genoutp                            ../engine/source/output/sty/genoutp.F
!||    genstat                            ../engine/source/output/sta/genstat.F
!||    h3d_list_nodal_scalar              ../engine/source/output/h3d/input_list/h3d_list_noda_scalar.F
!||    h3d_nodal_scalar                   ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
!||    hist2                              ../engine/source/output/th/hist2.F
!||    hm_read_checksum                   ../starter/source/output/checksum/checksum_option.F90
!||    hm_read_prethgrou                  ../starter/source/output/th/hm_read_prethgrou.F
!||    i14frt                             ../engine/source/interfaces/int14/i14frt.F
!||    i14wfs                             ../engine/source/interfaces/int14/i14wfs.F
!||    i15ass                             ../engine/source/interfaces/int15/i15ass.F
!||    imp_buck                           ../engine/source/implicit/imp_buck.F
!||    lech3d                             ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
!||    lecinp                             ../engine/source/input/lecinp.F
!||    lectur                             ../engine/source/input/lectur.F
!||    meint                              ../engine/source/materials/mat_share/meint.F
!||    monvol0                            ../engine/source/airbag/monvol0.F
!||    mulaw                              ../engine/source/materials/mat_share/mulaw.F90
!||    pblast_1                           ../engine/source/loads/pblast/pblast_1.F
!||    pblast_2                           ../engine/source/loads/pblast/pblast_2.F
!||    pblast_3                           ../engine/source/loads/pblast/pblast_3.F
!||    printime                           ../engine/source/system/timer.F
!||    r2r_input_init                     ../engine/source/coupling/rad2rad/r2r_input_init.F
!||    radioss2                           ../engine/source/engine/radioss2.F
!||    rdcomi                             ../engine/source/output/restart/rdcomm.F
!||    rdcomr                             ../engine/source/output/restart/rdcomm.F
!||    rdresb                             ../engine/source/output/restart/rdresb.F
!||    report                             ../engine/source/output/report/report.F
!||    resol                              ../engine/source/engine/resol.F
!||    resol_head                         ../engine/source/engine/resol_head.F
!||    resol_init                         ../engine/source/engine/resol_init.F
!||    sigeps41                           ../engine/source/materials/mat/mat041/sigeps41.F
!||    sms_encin_2                        ../engine/source/ams/sms_encin_2.F
!||    sortie_main                        ../engine/source/output/sortie_main.F
!||    spmd_exch_output_noda_pext         ../engine/source/mpi/output/spmd_exch_output_noda_pext.F
!||    spstres                            ../engine/source/elements/sph/spstres.F
!||    st_checksum_file_print             ../starter/source/output/checksum/checksum_option.F90
!||    st_qaprint_driver                  ../starter/source/output/qaprint/st_qaprint_driver.F
!||    st_qaprint_time_histories          ../starter/source/output/qaprint/st_qaprint_time_histories.F
!||    starter0                           ../starter/source/starter/starter0.F
!||    stat_size_c                        ../engine/source/output/sta/stat_size.F
!||    stop_sensor                        ../engine/source/tools/sensor/stop_sensor.F
!||    szforc3                            ../engine/source/elements/solid/solidez/szforc3.F
!||    th_time_output                     ../engine/source/output/th/th_time_output.F
!||    thnod                              ../engine/source/output/th/thnod.F
!||    volp_lfluid                        ../engine/source/airbag/volp_lfluid.F
!||    wrcomi                             ../engine/source/output/restart/wrcomm.F
!||    wrcomr                             ../engine/source/output/restart/wrcomm.F
!||    wrrestp                            ../engine/source/output/restart/wrrestp.F
!||--- uses       -----------------------------------------------------
!||    checksum_output_option_mod         ../common_source/modules/output/checksum_mod.F90
!||    precision_mod                      ../common_source/modules/precision_mod.F90
!||    state_file_mod                     ../common_source/modules/output/state_file_mod.F90
!||    time_history_mod                   ../common_source/modules/output/time_history_mod.F
!||====================================================================
      module output_mod
        ! *.out , TH, STA, but no /ANIM/ or /H3D/ here
        use time_history_mod
        use state_file_mod
        use checksum_output_option_mod
        use precision_mod, only : WP

        type output_
          type (th_) :: th
          type (state_) :: state
          type (checksum_option_) :: checksum !< checksum option from Starter
          character(len=2048) :: out_filename !< *.out file name  
        end type output_

        ! /H3D/NODA/PEXT and /ANIM/NODA/PEXT and /TH/NODE(PEXT)
        real(kind=WP), dimension(:), allocatable :: NODA_SURF, NODA_PEXT       !domain array
        real(kind=WP), dimension(:), allocatable :: NODA_SURF_G, NODA_PEXT_G   !global array (proc 0) for animation files
        integer :: H3D_HAS_NODA_PEXT
        integer :: ANIM_HAS_NODA_PEXT

        type(output_),pointer :: output_ptr      ! pointer to output structure (need for arret)


        contains

!||====================================================================
!||    output_allocate_noda_pext   ../common_source/modules/output/output_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                       ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                ../common_source/modules/constant_mod.F
!||    th_mod                      ../engine/share/modules/th_mod.F
!||====================================================================
          SUBROUTINE OUTPUT_ALLOCATE_NODA_PEXT(NUMNOD, NUMNODG)
              use th_mod , only : th_has_noda_pext
              use constant_mod , only : zero
              !use output_mod , only : anim_has_noda_pext, h3d_has_noda_pext
              !use output_mod , only : NODA_SURF,NODA_PEXT
              !use output_mod , only : NODA_SURF_G,NODA_PEXT_G
              implicit none
              INTEGER,INTENT(IN) :: NUMNOD  ! number of nodes per domain
              INTEGER,INTENT(IN) :: NUMNODG ! number of nodes (proc 0 only)
              IF(TH_HAS_NODA_PEXT > 0 .OR. ANIM_HAS_NODA_PEXT > 0 .OR. H3D_HAS_NODA_PEXT > 0) THEN
                ALLOCATE(NODA_SURF(NUMNOD))
                ALLOCATE(NODA_PEXT(NUMNOD))
                NODA_SURF(1:NUMNOD)=ZERO
                NODA_PEXT(1:NUMNOD)=ZERO
             END IF
              IF(ANIM_HAS_NODA_PEXT > 0) THEN
                ALLOCATE(NODA_SURF_G(NUMNODG))
                ALLOCATE(NODA_PEXT_G(NUMNODG))
                NODA_SURF_G(1:NUMNODG)=ZERO
                NODA_PEXT_G(1:NUMNODG)=ZERO
             END IF
          END SUBROUTINE OUTPUT_ALLOCATE_NODA_PEXT

!||====================================================================
!||    output_deallocate_noda_pext   ../common_source/modules/output/output_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                         ../engine/source/engine/resol.F
!||====================================================================
          SUBROUTINE OUTPUT_DEALLOCATE_NODA_PEXT
            IF(ALLOCATED(NODA_SURF_G)) DEALLOCATE(NODA_SURF_G)
            IF(ALLOCATED(NODA_PEXT_G)) DEALLOCATE(NODA_PEXT_G)
            IF(ALLOCATED(NODA_SURF))DEALLOCATE(NODA_SURF)
            IF(ALLOCATED(NODA_PEXT))DEALLOCATE(NODA_PEXT)
          END SUBROUTINE OUTPUT_DEALLOCATE_NODA_PEXT

      end module output_mod
