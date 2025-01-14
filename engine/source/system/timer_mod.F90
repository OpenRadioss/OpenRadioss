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
      !||    timer_mod                  ../engine/source/system/timer_mod.F90
      !||--- called by ------------------------------------------------------
      !||    add_elapsed_time_mon_off   ../engine/source/system/timer.F
      !||    alemain                    ../engine/source/ale/alemain.F
      !||    alewdx                     ../engine/source/ale/grid/alewdx.F
      !||    bforc2                     ../engine/source/ale/bimat/bforc2.F
      !||    c3forc3                    ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk                ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3                   ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cdk6forc3                  ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3                   ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3                     ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk                 ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3                     ../engine/source/materials/mat_share/cmain3.F
      !||    czforc3                    ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk                ../engine/source/elements/xfem/czforc3_crk.F
      !||    ecrit                      ../engine/source/output/ecrit.F
      !||    elapstime                  ../engine/source/system/timer.F
      !||    forint                     ../engine/source/elements/forint.F
      !||    forintc                    ../engine/source/elements/forintc.F
      !||    forintp                    ../engine/source/elements/forintp.F
      !||    genh3d                     ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    i10main_tri                ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i11main_tri                ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11mainf                   ../engine/source/interfaces/int11/i11mainf.F
      !||    i17main_tri                ../engine/source/interfaces/int17/i17main_pena.F
      !||    i20main_tri                ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                   ../engine/source/interfaces/int20/i20mainf.F
      !||    i21main_opt_tri            ../engine/source/interfaces/intsort/i21main_opt_tri.F
      !||    i21main_tri                ../engine/source/interfaces/intsort/i21main_tri.F
      !||    i21mainf                   ../engine/source/interfaces/int21/i21mainf.F
      !||    i21optcd                   ../engine/source/interfaces/int21/i21optcd.F
      !||    i22main_tri                ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                   ../engine/source/interfaces/int22/i22mainf.F
      !||    i23main_tri                ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                   ../engine/source/interfaces/int23/i23mainf.F
      !||    i24main_tri                ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf                   ../engine/source/interfaces/int24/i24main.F
      !||    i25main_free               ../engine/source/interfaces/intsort/i25main_free.F
      !||    i25main_tri                ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                   ../engine/source/interfaces/int25/i25mainf.F
      !||    i7main_tri                 ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                    ../engine/source/interfaces/int07/i7mainf.F
      !||    ig3duforc3                 ../engine/source/elements/ige3d/ig3duforc3.F
      !||    imp_chkm                   ../engine/source/implicit/imp_solv.F
      !||    imp_inttd0                 ../engine/source/implicit/imp_int_k.F
      !||    imp_solv                   ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                  ../engine/source/implicit/imp_int_k.F
      !||    intcrit                    ../engine/source/interfaces/intsort/intcrit.F
      !||    inter_sort                 ../engine/source/interfaces/generic/inter_sort.F
      !||    inter_sort_07              ../engine/source/interfaces/int07/inter_sort_07.F
      !||    intfop2                    ../engine/source/interfaces/interf/intfop2.F
      !||    inttri                     ../engine/source/interfaces/intsort/inttri.F
      !||    mmain                      ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8                     ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw                      ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8                     ../engine/source/materials/mat_share/mulaw8.F90
      !||    mulawc                     ../engine/source/materials/mat_share/mulawc.F90
      !||    multi_muscl_gradients      ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_timeevolution        ../engine/source/multifluid/multi_timeevolution.F
      !||    printime                   ../engine/source/system/timer.F
      !||    q4forc2                    ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2                     ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    radioss2                   ../engine/source/engine/radioss2.F
      !||    rbyfor                     ../engine/source/constraints/general/rbody/rbyfor.F
      !||    resol                      ../engine/source/engine/resol.F
      !||    resol_head                 ../engine/source/engine/resol_head.F
      !||    rmatforp                   ../engine/source/materials/mat/mat013/rmatforp.F
      !||    s10forc3                   ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16forc3                   ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3                   ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4forc3                    ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cforc3                   ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cforc3                   ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3                   ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3                    ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3                   ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3                   ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scforc3                    ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sforc3                     ../engine/source/elements/solid/solide/sforc3.F
      !||    sms_check                  ../engine/source/ams/sms_fsa_inv.F
      !||    sms_encin_2                ../engine/source/ams/sms_encin_2.F
      !||    sms_inist                  ../engine/source/ams/sms_proj.F
      !||    sms_inix                   ../engine/source/ams/sms_proj.F
      !||    sms_mass_scale_2           ../engine/source/ams/sms_mass_scale_2.F
      !||    sms_mav_lt                 ../engine/source/ams/sms_pcg.F
      !||    sms_mav_lt2                ../engine/source/ams/sms_pcg.F
      !||    sms_pcg                    ../engine/source/ams/sms_pcg.F
      !||    sms_pro_p                  ../engine/source/ams/sms_proj.F
      !||    sortie_main                ../engine/source/output/sortie_main.F
      !||    sphprep                    ../engine/source/elements/sph/sphprep.F
      !||    sphtri0                    ../engine/source/elements/sph/sphtri0.F
      !||    spmd_cell_exchange         ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    spstres                    ../engine/source/elements/sph/spstres.F
      !||    suforc3                    ../engine/source/user_interface/suforc3.F
      !||    suser43                    ../engine/source/elements/solid/sconnect/suser43.F
      !||    szforc3                    ../engine/source/elements/solid/solidez/szforc3.F
      !||    usermat_shell              ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid              ../engine/source/materials/mat_share/usermat_solid.F
      !||====================================================================
      module timer_mod
        implicit none
        integer, parameter :: max_nb_timer= 150
        integer, parameter :: TIMER_RESOL     =     1
        integer, parameter :: TIMER_CONTSORT  =     2
        integer, parameter :: TIMER_ELEMENT   =     3
        integer, parameter :: TIMER_KIN       =     4
        integer, parameter :: TIMER_INTEG     =     5
        integer, parameter :: TIMER_P0        =     6
        integer, parameter :: TIMER_IO        =     7
        integer, parameter :: TIMER_CONTFOR   =     8
        integer, parameter :: TIMER_ASM       =     9
        integer, parameter :: TIMER_EXFOR     =     10
        integer, parameter :: TIMER_EXRBYF    =     11
        integer, parameter :: TIMER_EXRBYV    =     12
        integer, parameter :: TIMER_EXSPMDV   =     13
        integer, parameter :: TIMER_MADYMO    =     14
        integer, parameter :: TIMER_AMS       =     39
        integer, parameter :: TIMER_TMP1      =    150
        integer, parameter :: TIMER_TMP2      =    149
        integer, parameter :: TIMER_ALEMAIN   =     110
        integer, parameter :: TIMER_MULTIFVM  =     111
        integer, parameter :: TIMER_IFSUB0    =     112
        integer, parameter :: TIMER_MUSCL     =     113
        integer, parameter :: TIMER_SPMDCFD   =     114
        integer, parameter :: TIMER_FRIC      =     108
        integer, parameter :: TIMER_LIBH3D    =     120
        integer, parameter :: TIMER_SPMDH3D   =     121
        integer, parameter :: TIMER_GENH3D    =     122
        integer, parameter :: TIMER_GENH3D1   =      123
        integer, parameter :: TIMER_GENH3D2   =      124
        integer, parameter :: TIMER_SKEW      =      125
        integer, parameter :: TIMER_FVMBAG    =      126
        integer, parameter :: TIMER_FVMBAG1   =      127
        integer, parameter :: TIMER_MONVOL    =      128
        integer, parameter :: TIMER_T25SLIDING=      129
        integer, parameter :: TIMER_T25NORM   =      106
        integer, parameter :: TIMER_T25STFE   =      130
        integer, parameter :: TIMER_T25VOX0   =      131
        integer, parameter :: TIMER_T25VOX0E2E=      132
        integer, parameter :: TIMER_T25VOX    =      133
        integer, parameter :: TIMER_T25RNUM   =      134
        integer, parameter :: TIMER_T25RNUME  =      135
        integer, parameter :: TIMER_T25BUC    =      136
        integer, parameter :: TIMER_T25BUCE2E =      137
        integer, parameter :: TIMER_T25TRCE   =      138
        type timer_
          real, dimension(:,:), allocatable :: timer
          real, dimension(:), allocatable :: cputime
          real, dimension(:), allocatable :: systime
          real, dimension(:), allocatable :: realtime
          integer, dimension(:), allocatable :: clockini
          double precision :: omp_starting_time
          double precision, dimension(:), allocatable :: omp_initime
          integer :: clock0
          double precision :: elapsed
        end type timer_
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!
      !||====================================================================
      !||    initime        ../engine/source/system/timer_mod.F90
      !||--- called by ------------------------------------------------------
      !||    radioss2       ../engine/source/engine/radioss2.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine initime(t)
!     initialisation des timers
!     timer                       signification
!
!     1                           resol
!     2                           interfaces (total ou tri fop) [tri type 7, ALE]
!     3                           elements
!     4                           cond. cin.
!     5                           integration
!     6                           non parallel sur p0
!     7                           IO
!     8                           interfaces (forces fop) [i7opcd,i7dst3,i7for3,shooting nodes]
!     9                           assemblage forces
!    10                           echange forces spmd
!    11                           echange rigid bodies forces
!    12                           echange rigid bodies vitesses
!    13                           echange vitesse spmd
!    14                           -----
!********* TIMERS 15 A 25 ACTIVE sur /DEBUG/CAND + /MON/ON ***************
!    15                           interface i7buce_crit
!    16                           interface icomcrit (comm critere de tris)
!    17                           interface i7main_tri (tri)
!    18                           interface spmd_ifront     (maj frontieres)
!    19                           interface i7optcd
!    20                           interface force i7for3
!    21                           interface envoi force
!    22                           interface reception force
!    23                           interface envoi vitesse
!    24                           interface reception vitesse
!    25                           interface envoi XV tri (TRI7BOX)
!    26                           interface maj cand tri (no comm, included in i7maindb)
!    27                           interface tri mmx (comm only, included in spmd_i7crit)
!    28                           interface tied type 2
!    29                           interface shooting nodes
!    30                           interface i7buce pure (sans comm)
!    31                           [K] setup
!    32                           [M] setup
!    33                           implicit solver
!    34                           total implicit
!    35                           time spent in Material routines
!    36                           adaptive meshing : criteria
!    37                           adaptive meshing : kinematic conditions forces
!    38                           adaptive meshing : kinematic conditions velocities
!    39                           selective mass scaling
!********* TIMERS 40 A 60 ACTIVE sur /DEBUG/CAND + /MON/ON ***************
!    40                           cond cine rbodies + sensors + accelerometres
!    41                           cond cine concentrated load forces
!    42                           cond cine boundary conditions
!    43                           cond cine rlinks + rivets + cjoints + rwalls
!    44                           cond cine fixvel + fixtemp
!    45                           cond cine RBE3
!    46                           cond cine gravity
!    47                           task0 manctr
!    48                           sph (total)
!    49                           task0 geo+uwin+tfum+desacti
!    50                           task0 airbag
!    51                           task0 forints
!    52                           task0 damping
!    53                           task0 DT2
!    54                           task0 R2R
!    55                           -----
!    56                           synchro avant critere tri
!    57                           synchro apres critere tri
!    58                           synchro fin de tri
!    59                           synchro apres element+interf forces
!    60                           synchro fin cycle si cc a envoyer
!********* TIMERS 61 A 70 ACTIVE sur /MON/FULL ***************
!    61                           AMS PCG  PCG       :: PCG sauf produits matrice-vecteur W=MV
!    62                           AMS PCG  PARIT F   :: calcul des normes et produits scalaires P/ON (sum_6_float)
!    63                           AMS PCG  COMM R    :: comm. pour calcul des normes et produiys scalaires
!    64                           AMS PCG  COMP M.V  :: calcul W=MV hors communications
!    65                           AMS PCG  COMM VFI  :: COMM. ECHANGE VFI cf contacts
!    66                           IMP PCG
!    67                           IMP PCG
!    68                           IMP PCG
!    69                           IMP PCG
!    70                           AMS EIGENVECTORS
!******** 71 A 74 ACTIVE sur /MON/FULL ***********************
!    71                           AMS EIGENVECTORS
!    72                           AMS EIGENVECTORS
!    73                           AMS EIGENVECTORS
!    74                           AMS PCG  COMP MV/E :: calcul W=MV wrt matrice elementaire uniquement - inclus dans time(64)
!    75                           AMS BUILD MATRIX
!    76                           synchro fin de cycle comm shooting
!
!    75 - 86                      utilise, mais non decrit ..
!
!    80                           AMS PCG COMM M.V   :: COMM. pour ASSEMBLAGE de W=MV
!
!    87                           sph preparation (sorting, ...)
!    88                           sph interactions (forintp)
!    89                           sph others
!******** 90 A 95 SPH************************************
!    90                           SPH/SORT1 including 91
!    91                           SPH/COMM.SORT1
!    94                           SPH/SORT0 including 92
!    92                           SPH/COMM.SORT0
!    93                           SPH/COMM.FORCES
!    95                           SPH/LOAD INBALANCE SORTING
!******** 96 A 100 LIBRES ************************************
!******** 106 /INTER/TYPE25 Specific  ************************
!   106                           /INTER/TYPE25 Normals computation
!   107                           Libre
!   108                           Libre
!   109                           Libre
!********* 110-119  ALE ***************************
!********* 120-122  H3D ANIM***********************
!********  149-150  temporary *********************
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(timer_), intent(inout) :: T
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
#ifdef _OPENMP
          real(kind=8) omp_get_wtime
          external omp_get_wtime
#endif
          integer j
! ----------------------------------------------------------------------------------------------------------------------
          allocate(T%timer(4,max_nb_timer))
          allocate(T%cputime(max_nb_timer))
          allocate(T%systime(max_nb_timer))
          allocate(T%realtime(max_nb_timer))
          allocate(T%omp_initime(max_nb_timer))
          allocate(T%clockini(max_nb_timer))
          do j = 1, max_nb_timer
            t%cputime(j) = 0
            t%systime(j) = 0
            t%realtime(j) = 0
            t%omp_initime(j) = 0
            t%clockini(j) = 0
          enddo
          call system_clock(t%clock0)
          if(t%clock0 < 0 )  call system_clock(t%clock0)   !in case of failure
          t%elapsed = 0

#ifdef _OPENMP
          t%omp_starting_time = omp_get_wtime( )
#endif
          RETURN
        END
! ======================================================================================================================
      !||====================================================================
      !||    startime                ../engine/source/system/timer_mod.F90
      !||--- called by ------------------------------------------------------
      !||    alemain                 ../engine/source/ale/alemain.F
      !||    alewdx                  ../engine/source/ale/grid/alewdx.F
      !||    c3forc3                 ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk             ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3                ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cforc3                  ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk              ../engine/source/elements/xfem/cforc3_crk.F
      !||    czforc3                 ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk             ../engine/source/elements/xfem/czforc3_crk.F
      !||    forintp                 ../engine/source/elements/forintp.F
      !||    genh3d                  ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    i10main_tri             ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i11main_tri             ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11mainf                ../engine/source/interfaces/int11/i11mainf.F
      !||    i17main_tri             ../engine/source/interfaces/int17/i17main_pena.F
      !||    i20main_tri             ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                ../engine/source/interfaces/int20/i20mainf.F
      !||    i21main_tri             ../engine/source/interfaces/intsort/i21main_tri.F
      !||    i21mainf                ../engine/source/interfaces/int21/i21mainf.F
      !||    i21optcd                ../engine/source/interfaces/int21/i21optcd.F
      !||    i22main_tri             ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                ../engine/source/interfaces/int22/i22mainf.F
      !||    i23main_tri             ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                ../engine/source/interfaces/int23/i23mainf.F
      !||    i24main_tri             ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf                ../engine/source/interfaces/int24/i24main.F
      !||    i25main_free            ../engine/source/interfaces/intsort/i25main_free.F
      !||    i25main_tri             ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                ../engine/source/interfaces/int25/i25mainf.F
      !||    i7main_tri              ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                 ../engine/source/interfaces/int07/i7mainf.F
      !||    ig3duforc3              ../engine/source/elements/ige3d/ig3duforc3.F
      !||    imp_chkm                ../engine/source/implicit/imp_solv.F
      !||    imp_pcgh                ../engine/source/implicit/imp_pcg.F
      !||    imp_solv                ../engine/source/implicit/imp_solv.F
      !||    intcrit                 ../engine/source/interfaces/intsort/intcrit.F
      !||    inter_sort_07           ../engine/source/interfaces/int07/inter_sort_07.F
      !||    inttri                  ../engine/source/interfaces/intsort/inttri.F
      !||    mmain                   ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8                  ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8                  ../engine/source/materials/mat_share/mulaw8.F90
      !||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
      !||    multi_muscl_gradients   ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_timeevolution     ../engine/source/multifluid/multi_timeevolution.F
      !||    rbyfor                  ../engine/source/constraints/general/rbody/rbyfor.F
      !||    resol                   ../engine/source/engine/resol.F
      !||    rmatforp                ../engine/source/materials/mat/mat013/rmatforp.F
      !||    s10forc3                ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s4forc3                 ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s8cforc3                ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3                ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3                 ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3                ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3                ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    sforc3                  ../engine/source/elements/solid/solide/sforc3.F
      !||    sms_check               ../engine/source/ams/sms_fsa_inv.F
      !||    sms_inist               ../engine/source/ams/sms_proj.F
      !||    sms_inix                ../engine/source/ams/sms_proj.F
      !||    sms_mass_scale_2        ../engine/source/ams/sms_mass_scale_2.F
      !||    sms_mav_lt              ../engine/source/ams/sms_pcg.F
      !||    sms_mav_lt2             ../engine/source/ams/sms_pcg.F
      !||    sms_pcg                 ../engine/source/ams/sms_pcg.F
      !||    sms_pro_p               ../engine/source/ams/sms_proj.F
      !||    sphprep                 ../engine/source/elements/sph/sphprep.F
      !||    sphtri0                 ../engine/source/elements/sph/sphtri0.F
      !||    spmd_cell_exchange      ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    suser43                 ../engine/source/elements/solid/sconnect/suser43.F
      !||    szforc3                 ../engine/source/elements/solid/solidez/szforc3.F
      !||    usermat_shell           ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid           ../engine/source/materials/mat_share/usermat_solid.F
      !||--- calls      -----------------------------------------------------
      !||    my_etime                ../engine/source/system/machine.F
      !||====================================================================
        subroutine startime(t,event)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: event
          type(timer_) :: t
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
#ifdef _OPENMP
          real(kind=8) OMP_GET_WTIME
          external OMP_GET_WTIME
#endif
          call my_etime(t%timer(1,event))
#ifdef _OPENMP
          t%omp_initime(event) = omp_get_wtime( )
#else
          call system_clock(t%clockini(event))
          if(t%clockini(event)< 0 ) call system_clock(t%clockini(event))
#endif
          return
        end

! ======================================================================================================================
      !||====================================================================
      !||    stoptime                ../engine/source/system/timer_mod.F90
      !||--- called by ------------------------------------------------------
      !||    alemain                 ../engine/source/ale/alemain.F
      !||    alewdx                  ../engine/source/ale/grid/alewdx.F
      !||    c3forc3                 ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk             ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3                ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cforc3                  ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk              ../engine/source/elements/xfem/cforc3_crk.F
      !||    czforc3                 ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk             ../engine/source/elements/xfem/czforc3_crk.F
      !||    forintp                 ../engine/source/elements/forintp.F
      !||    genh3d                  ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    i10main_tri             ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i11main_tri             ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11mainf                ../engine/source/interfaces/int11/i11mainf.F
      !||    i17main_tri             ../engine/source/interfaces/int17/i17main_pena.F
      !||    i20main_tri             ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                ../engine/source/interfaces/int20/i20mainf.F
      !||    i21main_tri             ../engine/source/interfaces/intsort/i21main_tri.F
      !||    i21mainf                ../engine/source/interfaces/int21/i21mainf.F
      !||    i22main_tri             ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                ../engine/source/interfaces/int22/i22mainf.F
      !||    i23main_tri             ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                ../engine/source/interfaces/int23/i23mainf.F
      !||    i24main_tri             ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf                ../engine/source/interfaces/int24/i24main.F
      !||    i25main_free            ../engine/source/interfaces/intsort/i25main_free.F
      !||    i25main_tri             ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                ../engine/source/interfaces/int25/i25mainf.F
      !||    i7main_tri              ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                 ../engine/source/interfaces/int07/i7mainf.F
      !||    ig3duforc3              ../engine/source/elements/ige3d/ig3duforc3.F
      !||    imp_chkm                ../engine/source/implicit/imp_solv.F
      !||    imp_pcgh                ../engine/source/implicit/imp_pcg.F
      !||    imp_solv                ../engine/source/implicit/imp_solv.F
      !||    intcrit                 ../engine/source/interfaces/intsort/intcrit.F
      !||    inter_sort_07           ../engine/source/interfaces/int07/inter_sort_07.F
      !||    inttri                  ../engine/source/interfaces/intsort/inttri.F
      !||    mmain                   ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8                  ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8                  ../engine/source/materials/mat_share/mulaw8.F90
      !||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
      !||    multi_muscl_gradients   ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_timeevolution     ../engine/source/multifluid/multi_timeevolution.F
      !||    rbyfor                  ../engine/source/constraints/general/rbody/rbyfor.F
      !||    resol                   ../engine/source/engine/resol.F
      !||    rmatforp                ../engine/source/materials/mat/mat013/rmatforp.F
      !||    s10forc3                ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s4forc3                 ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s8cforc3                ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3                ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3                 ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3                ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3                ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    sforc3                  ../engine/source/elements/solid/solide/sforc3.F
      !||    sms_check               ../engine/source/ams/sms_fsa_inv.F
      !||    sms_inist               ../engine/source/ams/sms_proj.F
      !||    sms_inix                ../engine/source/ams/sms_proj.F
      !||    sms_mass_scale_2        ../engine/source/ams/sms_mass_scale_2.F
      !||    sms_mav_lt              ../engine/source/ams/sms_pcg.F
      !||    sms_mav_lt2             ../engine/source/ams/sms_pcg.F
      !||    sms_pcg                 ../engine/source/ams/sms_pcg.F
      !||    sms_pro_p               ../engine/source/ams/sms_proj.F
      !||    sphprep                 ../engine/source/elements/sph/sphprep.F
      !||    sphtri0                 ../engine/source/elements/sph/sphtri0.F
      !||    spmd_cell_exchange      ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    suser43                 ../engine/source/elements/solid/sconnect/suser43.F
      !||    szforc3                 ../engine/source/elements/solid/solidez/szforc3.F
      !||    usermat_shell           ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid           ../engine/source/materials/mat_share/usermat_solid.F
      !||--- calls      -----------------------------------------------------
      !||    my_etime                ../engine/source/system/machine.F
      !||====================================================================
        subroutine stoptime(t,event)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(timer_) :: t
          integer event
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer clock1, clockrate, nbmax
          double precision  secs
          real(kind=8) :: omp_ending_time
#ifdef _OPENMP
          real(kind=8) omp_get_wtime
          external omp_get_wtime
#endif
! ----------------------------------------------------------------------------------------------------------------------
          call my_etime(t%timer(3,event))
          t%cputime(event) = t%cputime(event) + t%timer(3,event)-t%timer(1,event)
          t%systime(event) = t%systime(event) + t%timer(4,event)-t%timer(2,event)
#ifdef _OPENMP
          omp_ending_time = omp_get_wtime( )
          secs = omp_ending_time - t%omp_initime(event)
#else
          call system_clock(count=clock1, count_rate=clockrate,count_max=nbmax)
          if(clock1 < 0 ) then
            ! retry in case of failure
            call system_clock(count=clock1, count_rate=clockrate,count_max=nbmax)
          endif

          secs = clock1-t%clockini(event)
          if(secs<0) secs = secs + nbmax
          secs = secs/clockrate
#endif
          t%realtime(event)=t%realtime(event)+secs
          return
        end


      end module timer_mod

