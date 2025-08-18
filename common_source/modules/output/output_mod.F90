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
!||    i14cmp                             ../engine/source/interfaces/int14/i14cmp.F
!||    i14frt                             ../engine/source/interfaces/int14/i14frt.F
!||    i14wfs                             ../engine/source/interfaces/int14/i14wfs.F
!||    i15ass                             ../engine/source/interfaces/int15/i15ass.F
!||    i15cmp                             ../engine/source/interfaces/int15/i15cmp.F
!||    ig3duforc3                         ../engine/source/elements/ige3d/ig3duforc3.F
!||    imp_buck                           ../engine/source/implicit/imp_buck.F
!||    intfop1                            ../engine/source/interfaces/interf/intfop1.F
!||    lech3d                             ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
!||    lecinp                             ../engine/source/input/lecinp.F
!||    lectur                             ../engine/source/input/lectur.F
!||    meint                              ../engine/source/materials/mat_share/meint.F
!||    mmain                              ../engine/source/materials/mat_share/mmain.F90
!||    mmain8                             ../engine/source/materials/mat_share/mmain8.F
!||    monvol0                            ../engine/source/airbag/monvol0.F
!||    mulaw                              ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8                             ../engine/source/materials/mat_share/mulaw8.F90
!||    pblast_1                           ../engine/source/loads/pblast/pblast_1.F
!||    pblast_2                           ../engine/source/loads/pblast/pblast_2.F
!||    pblast_3                           ../engine/source/loads/pblast/pblast_3.F
!||    printime                           ../engine/source/system/timer.F
!||    q4forc2                            ../engine/source/elements/solid_2d/quad4/q4forc2.F
!||    qforc2                             ../engine/source/elements/solid_2d/quad/qforc2.F
!||    r2r_input_init                     ../engine/source/coupling/rad2rad/r2r_input_init.F
!||    radioss2                           ../engine/source/engine/radioss2.F
!||    rdcomi                             ../engine/source/output/restart/rdcomm.F
!||    rdcomr                             ../engine/source/output/restart/rdcomm.F
!||    rdresb                             ../engine/source/output/restart/rdresb.F
!||    resol                              ../engine/source/engine/resol.F
!||    resol_head                         ../engine/source/engine/resol_head.F
!||    resol_init                         ../engine/source/engine/resol_init.F
!||    s10forc3                           ../engine/source/elements/solid/solide10/s10forc3.F
!||    s16forc3                           ../engine/source/elements/thickshell/solide16/s16forc3.F
!||    s20forc3                           ../engine/source/elements/solid/solide20/s20forc3.F
!||    s4forc3                            ../engine/source/elements/solid/solide4/s4forc3.F
!||    s6cforc3                           ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||    s8cforc3                           ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    s8eforc3                           ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8forc3                            ../engine/source/elements/solid/solide8/s8forc3.F
!||    s8sforc3                           ../engine/source/elements/solid/solide8s/s8sforc3.F
!||    s8zforc3                           ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    scforc3                            ../engine/source/elements/thickshell/solidec/scforc3.F
!||    sforc3                             ../engine/source/elements/solid/solide/sforc3.F
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
        use ALEANIM_MOD, only : FANI_CELL_

      implicit none
        type working_arrays_
          ! /H3D/NODA/PEXT and /ANIM/NODA/PEXT and /TH/NODE(PEXT)
          real(kind=WP), dimension(:), allocatable :: NODA_SURF, NODA_PEXT       !domain array
          real(kind=WP), dimension(:), allocatable :: NODA_SURF_G, NODA_PEXT_G   !global array (proc 0) for animation files
          integer :: H3D_HAS_NODA_PEXT = 0 ! should be in H3D_MOD ?
          integer :: ANIM_HAS_NODA_PEXT = 0

          ! Friction (both for /H3D/ and /ANIM/)
          integer :: S_EFRICINT
          integer :: S_EFRICINTG
          integer :: S_EFRIC
          integer :: S_EFRICG
          integer :: NINEFRIC_STAMP
          integer :: NINEFRIC
          ! number of interfaces defined for output fricional energy
          real(kind=WP), dimension(:,:), allocatable :: EFRIC
          real(kind=WP), dimension(:,:), allocatable :: EFRIC_STAMP
          real(kind=WP), dimension(:), allocatable :: EFRICG
          real(kind=WP), dimension(:), allocatable :: EFRICG_STAMP
          TYPE(FANI_CELL_) :: FANI_CELL
          integer :: S_VECT_CONT 
          integer :: S_VECT_FINT 
          integer :: S_VECT_FEXT 
          integer :: S_VECT_PCONT
          integer :: S_VECT_PCONT_2
          integer :: S_VECT_CONT2
          integer :: S_VECT_PCONT2
          integer :: S_VECT_PCONT2_2
          integer :: S_FOPT

          real(kind=wp), dimension(:,:), allocatable :: VECT_CONT 
          real(kind=wp), dimension(:,:), allocatable :: VECT_FINT 
          real(kind=wp), dimension(:,:), allocatable :: VECT_FEXT 
          real(kind=wp), dimension(:,:), allocatable :: VECT_PCONT
          real(kind=wp), dimension(:,:), allocatable :: VECT_PCONT_2
          real(kind=wp), dimension(:,:), allocatable :: VECT_CONT2
          real(kind=wp), dimension(:,:), allocatable :: VECT_PCONT2
          real(kind=wp), dimension(:,:), allocatable :: VECT_PCONT2_2
          real(kind=wp), dimension(:,:), allocatable :: FOPT

          integer :: S_SCAL_DT                                                  
          integer :: S_SCAL_DMAS                                                    
          integer :: S_SCAL_DINER                                                      
          integer :: S_SCAL_DAMA2                                                         
          integer :: S_SCAL_SPRING !

          real(kind=wp), dimension(:), allocatable :: SCAL_DT                                                  
          real(kind=wp), dimension(:), allocatable :: SCAL_DMAS                                                    
          real(kind=wp), dimension(:), allocatable :: SCAL_DINER                                                      
          real(kind=wp), dimension(:), allocatable :: SCAL_DAMA2                                                         
          real(kind=wp), dimension(:), allocatable :: SCAL_SPRING ! 
 
        end type working_arrays_

        type output_
          type (th_) :: th
          type (state_) :: state
          type (checksum_option_) :: checksum !< checksum option from Starter
          type(working_arrays_) :: data !< working arrays for output
          character(len=2048) :: out_filename !< *.out file name  
          integer :: nb_anim_frame
          integer :: DTANIM_FCT_ID

          real(kind=WP) :: TANIM            !start time
          real(kind=WP) :: TANIM0           !start time
          real(kind=WP) :: DTANIM           !time frequency
          real(kind=WP) :: DTANIM0          !time frequency
          real(kind=WP) :: TANIM_STOP       !stop time
          real(kind=WP) :: TANIM_STOP0      !stop time
          real(kind=WP) :: TANIMSENS
        end type output_


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
          SUBROUTINE OUTPUT_ALLOCATE_NODA_PEXT(OUTPUT,NUMNOD, NUMNODG)
              use th_mod , only : th_has_noda_pext
              use constant_mod , only : zero
              !use output_mod , only : anim_has_noda_pext, h3d_has_noda_pext
              !use output_mod , only : NODA_SURF,NODA_PEXT
              !use output_mod , only : NODA_SURF_G,NODA_PEXT_G
              implicit none
              TYPE(working_arrays_) :: OUTPUT
              INTEGER,INTENT(IN) :: NUMNOD  ! number of nodes per domain
              INTEGER,INTENT(IN) :: NUMNODG ! number of nodes (proc 0 only)
              IF(TH_HAS_NODA_PEXT > 0 .OR. OUTPUT%ANIM_HAS_NODA_PEXT > 0 .OR. OUTPUT%H3D_HAS_NODA_PEXT > 0) THEN
                ALLOCATE(OUTPUT%NODA_SURF(NUMNOD))
                ALLOCATE(OUTPUT%NODA_PEXT(NUMNOD))
                OUTPUT%NODA_SURF(1:NUMNOD)=ZERO
                OUTPUT%NODA_PEXT(1:NUMNOD)=ZERO
             END IF
              IF(OUTPUT%ANIM_HAS_NODA_PEXT > 0) THEN
                ALLOCATE(OUTPUT%NODA_SURF_G(NUMNODG))
                ALLOCATE(OUTPUT%NODA_PEXT_G(NUMNODG))
                OUTPUT%NODA_SURF_G(1:NUMNODG)=ZERO
                OUTPUT%NODA_PEXT_G(1:NUMNODG)=ZERO
             END IF
          END SUBROUTINE OUTPUT_ALLOCATE_NODA_PEXT

!||====================================================================
!||    output_deallocate_noda_pext   ../common_source/modules/output/output_mod.F90
!||--- called by ------------------------------------------------------
!||    resol                         ../engine/source/engine/resol.F
!||====================================================================
          SUBROUTINE OUTPUT_DEALLOCATE_NODA_PEXT(OUTPUT)
            implicit none
            TYPE(working_arrays_), intent(inout) :: OUTPUT
            IF(ALLOCATED(OUTPUT%NODA_SURF_G)) DEALLOCATE(OUTPUT%NODA_SURF_G)
            IF(ALLOCATED(OUTPUT%NODA_PEXT_G)) DEALLOCATE(OUTPUT%NODA_PEXT_G)
            IF(ALLOCATED(OUTPUT%NODA_SURF))   DEALLOCATE(OUTPUT%NODA_SURF)
            IF(ALLOCATED(OUTPUT%NODA_PEXT))   DEALLOCATE(OUTPUT%NODA_PEXT)
          END SUBROUTINE OUTPUT_DEALLOCATE_NODA_PEXT

          SUBROUTINE ALLOCATE_OUTPUT_DATA(OUTPUT,NUMNOD)
          use extend_array_mod
          implicit none
          TYPE(output_) :: OUTPUT
          INTEGER,INTENT(IN) :: NUMNOD
          INTEGER :: current_size1, current_size2, new_size2, new_size1
          INTEGER :: ierr
          
          ! Handle VECT_CONT
          if (allocated(OUTPUT%DATA%VECT_CONT)) then
            current_size1 = size(OUTPUT%DATA%VECT_CONT, 1)
            current_size2 = size(OUTPUT%DATA%VECT_CONT, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_CONT
          call extend_array(OUTPUT%DATA%VECT_CONT, current_size1, current_size2,3, new_size2)
          ! Handle VECT_FINT
          if (allocated(OUTPUT%DATA%VECT_FINT)) then
            current_size1 = size(OUTPUT%DATA%VECT_FINT, 1)
            current_size2 = size(OUTPUT%DATA%VECT_FINT, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_FINT
          call extend_array(OUTPUT%DATA%VECT_FINT, current_size1, current_size2, 3, new_size2)
          
          ! Handle VECT_FEXT
          if (allocated(OUTPUT%DATA%VECT_FEXT)) then
            current_size1 = size(OUTPUT%DATA%VECT_FEXT, 1)
            current_size2 = size(OUTPUT%DATA%VECT_FEXT, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_FEXT
          call extend_array(OUTPUT%DATA%VECT_FEXT, current_size1, current_size2, 3, new_size2)
          
          ! Handle VECT_PCONT
          if (allocated(OUTPUT%DATA%VECT_PCONT)) then
            current_size1 = size(OUTPUT%DATA%VECT_PCONT, 1)
            current_size2 = size(OUTPUT%DATA%VECT_PCONT, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_PCONT
          call extend_array(OUTPUT%DATA%VECT_PCONT, current_size1, current_size2, 3, new_size2)
          ! Handle VECT_PCONT_2
          if (allocated(OUTPUT%DATA%VECT_PCONT_2)) then
            current_size1 = size(OUTPUT%DATA%VECT_PCONT_2, 1)
            current_size2 = size(OUTPUT%DATA%VECT_PCONT_2, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_PCONT_2
          call extend_array(OUTPUT%DATA%VECT_PCONT_2, current_size1, current_size2, 3, new_size2)
 
          
          ! Handle VECT_CONT2
          if (allocated(OUTPUT%DATA%VECT_CONT2)) then
            current_size1 = size(OUTPUT%DATA%VECT_CONT2, 1)
            current_size2 = size(OUTPUT%DATA%VECT_CONT2, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_CONT2
          call extend_array(OUTPUT%DATA%VECT_CONT2, current_size1, current_size2, 3, new_size2)
          
          ! Handle VECT_PCONT2
          if (allocated(OUTPUT%DATA%VECT_PCONT2)) then
            current_size1 = size(OUTPUT%DATA%VECT_PCONT2, 1)
            current_size2 = size(OUTPUT%DATA%VECT_PCONT2, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_PCONT2
          call extend_array(OUTPUT%DATA%VECT_PCONT2, current_size1, current_size2,3, new_size2)

          ! Handle VECT_PCONT2_2
          if (allocated(OUTPUT%DATA%VECT_PCONT2_2)) then
            current_size1 = size(OUTPUT%DATA%VECT_PCONT2_2, 1)
            current_size2 = size(OUTPUT%DATA%VECT_PCONT2_2, 2)
          else
            current_size1 = 0
            current_size2 = 0
          end if
          new_size2 = NUMNOD * OUTPUT%DATA%S_VECT_PCONT2_2
          call extend_array(OUTPUT%DATA%VECT_PCONT2_2, current_size1, current_size2,3, new_size2)



           ! Handle OUTPUT%DATA%S_SCAL_DT                                                  
          if(allocated(OUTPUT%DATA%SCAL_DT)) then
            current_size1 = size(OUTPUT%DATA%SCAL_DT, 1)
          else
            current_size1 = 0
          end if  
          new_size1 = NUMNOD * OUTPUT%DATA%S_SCAL_DT
          call extend_array(OUTPUT%DATA%SCAL_DT, current_size1,  new_size1)

         ! Handle OUTPUT%DATA%S_SCAL_DMAS                                                    
          if(allocated(OUTPUT%DATA%SCAL_DMAS)) then
            current_size1 = size(OUTPUT%DATA%SCAL_DMAS, 1)
          else
            current_size1 = 0
          end if
          new_size1 = NUMNOD * OUTPUT%DATA%S_SCAL_DMAS
          call extend_array(OUTPUT%DATA%SCAL_DMAS, current_size1, new_size1)  

          ! Handle OUTPUT%DATA%S_SCAL_DINER                                                      
          if(allocated(OUTPUT%DATA%SCAL_DINER)) then
            current_size1 = size(OUTPUT%DATA%SCAL_DINER, 1)
          else
            current_size1 = 0
          end if
          new_size1 = NUMNOD * OUTPUT%DATA%S_SCAL_DINER
          call extend_array(OUTPUT%DATA%SCAL_DINER, current_size1, new_size1) 

           !Handle OUTPUT%DATA%S_SCAL_DAMA2                                                         
          if(allocated(OUTPUT%DATA%SCAL_DAMA2)) then
            current_size1 = size(OUTPUT%DATA%SCAL_DAMA2, 1)
          else
            current_size1 = 0
          end if
          new_size1 = NUMNOD * OUTPUT%DATA%S_SCAL_DAMA2
          call extend_array(OUTPUT%DATA%SCAL_DAMA2, current_size1, new_size1) 
           !Handle OUTPUT%DATA%S_SCAL_SPRING                                                         
          if(allocated(OUTPUT%DATA%SCAL_SPRING)) then
            current_size1 = size(OUTPUT%DATA%SCAL_SPRING, 1)
          else
            current_size1 = 0
          end if
          new_size1 = NUMNOD * OUTPUT%DATA%S_SCAL_SPRING
          call extend_array(OUTPUT%DATA%SCAL_SPRING, current_size1, new_size1) 
        END SUBROUTINE ALLOCATE_OUTPUT_DATA
      end module output_mod
