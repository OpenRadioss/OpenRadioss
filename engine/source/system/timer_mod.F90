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

