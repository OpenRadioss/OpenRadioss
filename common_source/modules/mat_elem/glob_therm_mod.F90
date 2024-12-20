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
! ======================================================================================================================

      !||====================================================================
      !||    glob_therm_mod           ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||--- called by ------------------------------------------------------
      !||    alemain                  ../engine/source/ale/alemain.F
      !||    alethe                   ../engine/source/ale/alethe.F
      !||    assadd2                  ../engine/source/assembly/assadd2.F
      !||    asspar2                  ../engine/source/assembly/asspar2.F
      !||    asspar4                  ../engine/source/assembly/asspar4.F
      !||    asspari2                 ../engine/source/assembly/asspar4.F
      !||    bforc2                   ../engine/source/ale/bimat/bforc2.F
      !||    c3forc3                  ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk              ../engine/source/elements/xfem/c3forc3_crk.F
      !||    c3init3                  ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    cbaforc3                 ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cbainit3                 ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cdk6forc3                ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3                 ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cdkinit3                 ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cforc3                   ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk               ../engine/source/elements/xfem/cforc3_crk.F
      !||    cinit3                   ../starter/source/elements/shell/coque/cinit3.F
      !||    contrl                   ../starter/source/starter/contrl.F
      !||    convec                   ../engine/source/constraints/thermic/convec.F
      !||    convecoff                ../engine/source/constraints/thermic/convecoff.F
      !||    convrrest                ../engine/source/output/restart/rdresb.F
      !||    convwrest                ../engine/source/output/restart/wrrest.F
      !||    czforc3                  ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk              ../engine/source/elements/xfem/czforc3_crk.F
      !||    ddsplit                  ../starter/source/restart/ddsplit/ddsplit.F
      !||    desacti                  ../engine/source/elements/desacti.F
      !||    dttherm                  ../engine/source/time_step/dttherm.F90
      !||    eig                      ../engine/stub/eig.F
      !||    eng_qaprint_driver       ../engine/source/output/qaprint/eng_qaprint_driver.F
      !||    eng_qaprint_dtinput      ../engine/source/output/qaprint/eng_qaprint_dtinput.F
      !||    fixflux                  ../engine/source/constraints/thermic/fixflux.F
      !||    fixtemp                  ../engine/source/constraints/thermic/fixtemp.F
      !||    forint                   ../engine/source/elements/forint.F
      !||    forintc                  ../engine/source/elements/forintc.F
      !||    forintp                  ../engine/source/elements/forintp.F
      !||    freform                  ../engine/source/input/freform.F
      !||    frethermal               ../engine/source/constraints/thermic/frethermal.F
      !||    fsdcod                   ../starter/source/system/fsdcod.F
      !||    fxfluxrrest              ../engine/source/constraints/thermic/fxfluxrrest.F
      !||    fxfluxwrest              ../engine/source/constraints/thermic/fxfluxwrest.F
      !||    fxtemprrest              ../engine/source/output/restart/rdresb.F
      !||    fxtempwrest              ../engine/source/output/restart/wrrest.F
      !||    genani                   ../engine/source/output/anim/generate/genani.F
      !||    genh3d                   ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    glob_therm_init          ../starter/source/constraints/thermic/glob_therm_init.F90
      !||    hist2                    ../engine/source/output/th/hist2.F
      !||    hm_read_part             ../starter/source/model/assembling/hm_read_part.F
      !||    i10main_tri              ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i20main_tri              ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i7main_tri               ../engine/source/interfaces/intsort/i7main_tri.F
      !||    ig3duforc3               ../engine/source/elements/ige3d/ig3duforc3.F
      !||    imp_buck                 ../engine/source/implicit/imp_buck.F
      !||    imp_inttd0               ../engine/source/implicit/imp_int_k.F
      !||    imp_solv                 ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                ../engine/source/implicit/imp_int_k.F
      !||    iniresa                  ../engine/source/output/restart/rdresa.F
      !||    inirig_mat               ../starter/source/elements/initia/inirig_mat.F
      !||    inisoldist               ../starter/source/initial_conditions/inivol/inisoldist.F
      !||    init_inivol              ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    initia                   ../starter/source/elements/initia/initia.F
      !||    inivoid                  ../starter/source/elements/initia/inivoid.F
      !||    inter_sort               ../engine/source/interfaces/generic/inter_sort.F
      !||    intti1                   ../engine/source/interfaces/interf/intti1.F
      !||    inttri                   ../engine/source/interfaces/intsort/inttri.F
      !||    lecinp                   ../engine/source/input/lecinp.F
      !||    lectur                   ../engine/source/input/lectur.F
      !||    m1law                    ../engine/source/materials/mat/mat001/m1law.F
      !||    m1lawi                   ../engine/source/materials/mat/mat001/m1lawi.F
      !||    m1lawtot                 ../engine/source/materials/mat/mat001/m1lawtot.F
      !||    m22law                   ../engine/source/materials/mat/mat022/m22law.F
      !||    m24law                   ../engine/source/materials/mat/mat024/m24law.F
      !||    m2law                    ../engine/source/materials/mat/mat002/m2law.F
      !||    m46law                   ../engine/source/materials/mat/mat046/m46law.F
      !||    mmain                    ../engine/source/materials/mat_share/mmain.F90
      !||    mqviscb                  ../engine/source/materials/mat_share/mqviscb.F
      !||    mulaw                    ../engine/source/materials/mat_share/mulaw.F90
      !||    multifluid_init3         ../starter/source/multifluid/multifluid_init3.F
      !||    pinit3                   ../starter/source/elements/beam/pinit3.F
      !||    q4forc2                  ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2                   ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    radiarrest               ../engine/source/output/restart/rdresb.F
      !||    radiation                ../engine/source/constraints/thermic/radiation.F
      !||    radiatoff                ../engine/source/constraints/thermic/radiatoff.F
      !||    radiawrest               ../engine/source/output/restart/wrrest.F
      !||    radioss2                 ../engine/source/engine/radioss2.F
      !||    rdcomi                   ../engine/source/output/restart/rdcomm.F
      !||    rdcomr                   ../engine/source/output/restart/rdcomm.F
      !||    rdresa                   ../engine/source/output/restart/rdresa.F
      !||    rdresb                   ../engine/source/output/restart/rdresb.F
      !||    resol                    ../engine/source/engine/resol.F
      !||    resol_head               ../engine/source/engine/resol_head.F
      !||    resol_init               ../engine/source/engine/resol_init.F
      !||    restalloc                ../engine/source/output/restart/arralloc.F
      !||    s10forc3                 ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s10init3                 ../starter/source/elements/solid/solide10/s10init3.F
      !||    s16forc3                 ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s16init3                 ../starter/source/elements/thickshell/solide16/s16init3.F
      !||    s20forc3                 ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s20init3                 ../starter/source/elements/solid/solide20/s20init3.F
      !||    s4forc3                  ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cforc3                 ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s6cinit3                 ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    s8cforc3                 ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8cinit3                 ../starter/source/elements/thickshell/solide8c/s8cinit3.F
      !||    s8eforc3                 ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3                  ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3                 ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3                 ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    s8zinit3                 ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    sboltlaw                 ../engine/source/elements/solid/solide/sboltlaw.F
      !||    scforc3                  ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    scinit3                  ../starter/source/elements/thickshell/solidec/scinit3.F
      !||    sforc3                   ../engine/source/elements/solid/solide/sforc3.F
      !||    sinit3                   ../starter/source/elements/solid/solide/sinit3.F
      !||    sortie_main              ../engine/source/output/sortie_main.F
      !||    spinit3                  ../starter/source/elements/sph/spinit3.F
      !||    spmd_exch2_a_pon         ../engine/source/mpi/forces/spmd_exch2_a_pon.F
      !||    spmd_exch_a              ../engine/source/mpi/forces/spmd_exch_a.F
      !||    spmd_exch_a_ams_poff     ../engine/source/mpi/forces/spmd_exch_a_ams_poff.F
      !||    spmd_i7fcom_pon          ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spstres                  ../engine/source/elements/sph/spstres.F
      !||    st_qaprint_constraints   ../starter/source/output/qaprint/st_qaprint_constraints.F
      !||    st_qaprint_driver        ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_loads         ../starter/source/output/qaprint/st_qaprint_loads.F
      !||    starter0                 ../starter/source/starter/starter0.F
      !||    suinit3                  ../starter/source/elements/elbuf_init/suinit3.F
      !||    szforc3                  ../engine/source/elements/solid/solidez/szforc3.F
      !||    thermbilan               ../engine/source/constraints/thermic/thermbilan.F
      !||    usermat_solid            ../engine/source/materials/mat_share/usermat_solid.F
      !||    w_pon                    ../starter/source/restart/ddsplit/w_pon.F
      !||    wrcomi                   ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                  ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrcomr                   ../engine/source/output/restart/wrcomm.F
      !||    wrrestp                  ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      module glob_therm_mod

! ======================================================================================================================
!! \brief module to define data structure for global thermal model parameters and flags
!! \details 

!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"
!
!=======================================================================      
      
      type glob_therm_

        integer     :: itherm_fe     !< thermal option flag for lagrangial analysis   
        integer     :: itherm        !< thermal option flag for ale/eulerian analysis 
        integer     :: intheat       !< thermal option in interfaces
!
        integer     :: nimtemp       !< number of /imptemp   input cards
        integer     :: nintemp       !< number of /initemp   input cards
        integer     :: nimpflux      !< number of /impflux   input cards
        integer     :: nconvec       !< number of /convec    input cards
        integer     :: nradia        !< number of /radiation input cards
!
        integer     :: nfxtemp       !< number of nodes with imposed temperature
        integer     :: nfxflux       !< number of entities with imposed thermal flux
        integer     :: numconv       !< number of segments subject to convection
        integer     :: numradia      !< number of segments subject to radiation

        integer     :: nift          !< size of IBFT table for imposed nodal temperature
        integer     :: nitflux       !< size of IB table for imposed thermal flux IB(NITFLUX,*)
        integer     :: niconv        !< size of convection data table : IB 
        integer     :: niradia       !< size of IBCR table for imposed radiation
        integer     :: lfacther      !< 
!
        integer     :: nodadt_therm  !< nodal thermal time step flag
        integer     :: idt_therm     !< thermal time step flag
!
        my_real     :: dt_therm      !< thermal time step value
        my_real     :: theaccfact    !< thermal model acceleration factor
        my_real     :: dtfactherm    !< thermal time step reduction factor
!
        my_real     :: heat_meca     !< cumulated mechanical heat flux
        my_real     :: heat_conv     !< cumulated convection heat flux
        my_real     :: heat_radia    !< cumulated radiation heat flux
        my_real     :: heat_fflux    !< cumulated fixed heat flux      
        my_real     :: heat_stored   !< cumulated total heat flux
!
!         my_real, dimension(:), allocatable :: FTHE
!         my_real, dimension(:), allocatable :: FTHESKYI
!         my_real, dimension(:), allocatable :: FTHESKY
!         my_real, dimension(:), allocatable :: TEMP_FTHESKYI
!         my_real, dimension(:), allocatable :: CONDN
!         my_real, dimension(:), allocatable :: CONDNSKY 
!         my_real, dimension(:), allocatable :: CONDNSKYI
!         my_real, dimension(:), allocatable :: TEMP_CONDNSKYI

      end type glob_therm_   
!
!------------------------------
      end module glob_therm_mod
