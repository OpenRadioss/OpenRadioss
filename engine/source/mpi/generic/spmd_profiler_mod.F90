!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    spmd_profiler_mod           spmd_profiler_mod.F90
!||
!||    Fortran interface to the C++ SPMD profiler (spmd_profiler.cpp).
!||
!||    Profiling is controlled at runtime via spmd_profiling_enabled.
!||    Call spmd_profiler_init(rank) to enable, or set
!||    spmd_profiling_enabled = .true. directly.
!||    All subroutines are no-ops when profiling is disabled (~1 ns overhead).
!||
!||    Typical use (before MPI_Finalize):
!||
!||      use spmd_mod
!||      call spmd_profiler_init(rank)  ! enables profiling
!||      ...
!||      call spmd_profiler_flush()     ! writes spmd_timeline_rank_NNNNN.spmd
!||      call MPI_Finalize(ierr)
!||====================================================================
      module spmd_profiler_mod
        use, intrinsic :: iso_c_binding
        implicit none




        !> Runtime profiling flag — set to .true. to enable profiling
        logical, public, save :: spmd_profiling_enabled = .false.

        ! Profiling section tags for use with spmd_profile_begin/end
        integer, parameter, public :: PROF_CONTSORT = -3002
        integer, parameter, public :: PROF_ELEMENT  = -3003
        integer, parameter, public :: PROF_KIN      = -3004
        integer, parameter, public :: PROF_INTEG    = -3005
        integer, parameter, public :: PROF_P0       = -3006
        integer, parameter, public :: PROF_IO       = -3007
        integer, parameter, public :: PROF_CONTFOR  = -3008
        integer, parameter, public :: PROF_ASM      = -3009
        integer, parameter, public :: PROF_EXFOR    = -3010
        integer, parameter, public :: PROF_CONTACT  = -3011
        integer, parameter, public :: PROF_OUTPUT   = -3012
        integer, parameter, public :: PROF_RESTART  = -3013
        integer, parameter, public :: PROF_CONTPOST = -3014
        integer, parameter, public :: PROF_RBODY    = -3015
        integer, parameter, public :: PROF_DTSTEP   = -3016
        integer, parameter, public :: PROF_ACCEL    = -3017
        integer, parameter, public :: PROF_INIT     = -3018
        integer, parameter, public :: PROF_ALEFOR   = -3019
        integer, parameter, public :: PROF_PREELEM  = -3020
        integer, parameter, public :: PROF_SPH_NLOC = -3021
        integer, parameter, public :: PROF_VELUPD   = -3022
        integer, parameter, public :: PROF_POSTOUT  = -3023
        integer, parameter, public :: PROF_ACC      = -3024
        integer, parameter, public :: PROF_ALE      = -3025
        integer, parameter, public :: PROF_ANIM     = -3026
        integer, parameter, public :: PROF_BCS      = -3027
        integer, parameter, public :: PROF_KINCOND  = -3028
        integer, parameter, public :: PROF_RBY      = -3029
        integer, parameter, public :: PROF_SENSOR   = -3030
        integer, parameter, public :: PROF_SPH      = -3031
        integer, parameter, public :: PROF_ACCELE   = -3032
        integer, parameter, public :: PROF_ALEMAIN  = -3033
        integer, parameter, public :: PROF_CONV_RAD = -3034
        integer, parameter, public :: PROF_DEPLA    = -3035
        integer, parameter, public :: PROF_DTNODA   = -3036
        integer, parameter, public :: PROF_ERR_THK  = -3037
        integer, parameter, public :: PROF_FIXVEL   = -3038
        integer, parameter, public :: PROF_FORANI1  = -3039
        integer, parameter, public :: PROF_FORANI3  = -3040
        integer, parameter, public :: PROF_FORINT   = -3041
        integer, parameter, public :: PROF_FORINTC  = -3042
        integer, parameter, public :: PROF_FVBAG    = -3043
        integer, parameter, public :: PROF_GRAVIT   = -3044
        integer, parameter, public :: PROF_I7XVCOM2 = -3045
        integer, parameter, public :: PROF_INTFO1   = -3046
        integer, parameter, public :: PROF_INTFO2   = -3047
        integer, parameter, public :: PROF_INTTRI   = -3048
        integer, parameter, public :: PROF_LOADP    = -3049
        integer, parameter, public :: PROF_OTHER_KIN = -3050
        integer, parameter, public :: PROF_PFLUID   = -3051
        integer, parameter, public :: PROF_RBYCOR   = -3052
        integer, parameter, public :: PROF_REAC     = -3053
        integer, parameter, public :: PROF_REACTION = -3054
        integer, parameter, public :: PROF_RESOL_INIT = -3055
        integer, parameter, public :: PROF_RIGIDWALL = -3056
        integer, parameter, public :: PROF_RLINK    = -3057
        integer, parameter, public :: PROF_RWALL    = -3058
        integer, parameter, public :: PROF_SECTION_IO = -3059
        integer, parameter, public :: PROF_SHOOTING_NODE = -3060
        integer, parameter, public :: PROF_SPH_GAUGE = -3061
        integer, parameter, public :: PROF_SPLISSV  = -3062
        integer, parameter, public :: PROF_SPONFV   = -3063
        integer, parameter, public :: PROF_TAGOFF3N = -3064
        integer, parameter, public :: PROF_TIED     = -3065
        integer, parameter, public :: PROF_VEL      = -3066
        integer, parameter, public :: PROF_BCSCYCL  = -3067
        integer, parameter, public :: PROF_CENTRIF  = -3068
        integer, parameter, public :: PROF_DAMPING  = -3069
        integer, parameter, public :: PROF_I18MAIN_KINE_2 = -3070
        integer, parameter, public :: PROF_INIT_FORCES = -3071
        integer, parameter, public :: PROF_STATIC   = -3072
        integer, parameter, public :: PROF_TAGNCONT = -3073

        !> C++ back-end — declared private so callers use the Fortran wrappers.
        private :: spmd_profiler_init_c, spmd_profiler_flush_c
        private :: spmd_profiler_section_begin_c, spmd_profiler_section_end_c

        interface
          subroutine spmd_profiler_init_c(rank) &
            bind(c, name="spmd_profiler_init")
            import :: c_int
            integer(c_int), intent(in) :: rank
          end subroutine spmd_profiler_init_c

          subroutine spmd_profiler_flush_c() &
            bind(c, name="spmd_profiler_flush")
          end subroutine spmd_profiler_flush_c

          subroutine spmd_profiler_section_begin_c(tag, name, name_len) &
            bind(c, name="spmd_profiler_section_begin")
            import :: c_int, c_char
            integer(c_int), intent(in) :: tag
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), intent(in) :: name_len
          end subroutine spmd_profiler_section_begin_c

          subroutine spmd_profiler_section_end_c(tag) &
            bind(c, name="spmd_profiler_section_end")
            import :: c_int
            integer(c_int), intent(in) :: tag
          end subroutine spmd_profiler_section_end_c
        end interface

      contains

! ======================================================================================================================
!! \brief Initialise the profiler, set the rank, and enable profiling.
        subroutine spmd_profiler_init(rank)
          implicit none
          integer, intent(in) :: rank
          integer(c_int) :: rank_c

          rank_c = int(rank, c_int)
          call spmd_profiler_init_c(rank_c)
          spmd_profiling_enabled = .true.
        end subroutine spmd_profiler_init

! ======================================================================================================================
!! \brief Write the collected timeline and clear the in-memory buffer.
!!        Must be called BEFORE MPI_Finalize.
        subroutine spmd_profiler_flush()
          implicit none

          if (.not. spmd_profiling_enabled) return
          call spmd_profiler_flush_c()
        end subroutine spmd_profiler_flush

! ======================================================================================================================
!! \brief Begin a named user section for profiling.
!! \details If a section is already active, it is auto-closed first.
!!          User sections are suspended by MPI calls (spmd_in/spmd_out) and
!!          automatically resumed after the MPI call completes.
!!          Use tags <= -3000 to avoid collision with MPI tags.

        subroutine spmd_profile_begin(tag, name)
          implicit none
          integer, intent(in) :: tag
          character(len=*), intent(in), optional :: name
          integer(c_int) :: tag_c, name_len_c
          character(kind=c_char), dimension(65) :: name_c
          character(len=32) :: local_name
          integer :: i, n

          if (.not. spmd_profiling_enabled) return
          tag_c = int(tag, c_int)

          if (present(name)) then
            local_name = name
          else
            select case (tag)
             case (PROF_CONTSORT); local_name = "CONTSORT"
             case (PROF_ELEMENT);  local_name = "ELEMENT"
             case (PROF_KIN);      local_name = "KIN"
             case (PROF_INTEG);    local_name = "INTEG"
             case (PROF_P0);       local_name = "P0"
             case (PROF_IO);       local_name = "IO"
             case (PROF_CONTFOR);  local_name = "CONTFOR"
             case (PROF_ASM);      local_name = "ASM"
             case (PROF_EXFOR);    local_name = "EXFOR"
             case (PROF_CONTACT);  local_name = "CONTACT"
             case (PROF_OUTPUT);   local_name = "OUTPUT"
             case (PROF_RESTART);  local_name = "RESTART"
             case (PROF_CONTPOST); local_name = "CONTPOST"
             case (PROF_RBODY);    local_name = "RBODY"
             case (PROF_DTSTEP);   local_name = "DTSTEP"
             case (PROF_ACCEL);    local_name = "ACCEL"
             case (PROF_INIT);     local_name = "INIT"
             case (PROF_ALEFOR);   local_name = "ALEFOR"
             case (PROF_PREELEM);  local_name = "PREELEM"
             case (PROF_SPH_NLOC); local_name = "SPH_NLOC"
             case (PROF_VELUPD);   local_name = "VELUPD"
             case (PROF_POSTOUT);  local_name = "POSTOUT"
             case (PROF_ACC);      local_name = "ACC"
             case (PROF_ALE);      local_name = "ALE"
             case (PROF_ANIM);     local_name = "ANIM"
             case (PROF_BCS);      local_name = "BCS"
             case (PROF_KINCOND);  local_name = "KINCOND"
             case (PROF_RBY);      local_name = "RBY"
             case (PROF_SENSOR);   local_name = "SENSOR"
             case (PROF_SPH);      local_name = "SPH"
             case (PROF_ACCELE);   local_name = "ACCELE"
             case (PROF_ALEMAIN);  local_name = "ALEMAIN"
             case (PROF_CONV_RAD); local_name = "CONV_RAD"
             case (PROF_DEPLA);    local_name = "DEPLA"
             case (PROF_DTNODA);   local_name = "DTNODA"
             case (PROF_ERR_THK);  local_name = "ERR_THK"
             case (PROF_FIXVEL);   local_name = "FIXVEL"
             case (PROF_FORANI1);  local_name = "FORANI1"
             case (PROF_FORANI3);  local_name = "FORANI3"
             case (PROF_FORINT);   local_name = "FORINT"
             case (PROF_FORINTC);  local_name = "FORINTC"
             case (PROF_FVBAG);    local_name = "FVBAG"
             case (PROF_GRAVIT);   local_name = "GRAVIT"
             case (PROF_I7XVCOM2); local_name = "I7XVCOM2"
             case (PROF_INTFO1);   local_name = "INTFO1"
             case (PROF_INTFO2);   local_name = "INTFO2"
             case (PROF_INTTRI);   local_name = "INTTRI"
             case (PROF_LOADP);    local_name = "LOADP"
             case (PROF_OTHER_KIN); local_name = "OTHER_KIN"
             case (PROF_PFLUID);   local_name = "PFLUID"
             case (PROF_RBYCOR);   local_name = "RBYCOR"
             case (PROF_REAC);     local_name = "REAC"
             case (PROF_REACTION); local_name = "REACTION"
             case (PROF_RESOL_INIT); local_name = "RESOL_INIT"
             case (PROF_RIGIDWALL); local_name = "RIGIDWALL"
             case (PROF_RLINK);    local_name = "RLINK"
             case (PROF_RWALL);    local_name = "RWALL"
             case (PROF_SECTION_IO); local_name = "SECTION_IO"
             case (PROF_SHOOTING_NODE); local_name = "SHOOTING_NODE"
             case (PROF_SPH_GAUGE); local_name = "SPH_GAUGE"
             case (PROF_SPLISSV);  local_name = "SPLISSV"
             case (PROF_SPONFV);   local_name = "SPONFV"
             case (PROF_TAGOFF3N); local_name = "TAGOFF3N"
             case (PROF_TIED);     local_name = "TIED"
             case (PROF_VEL);      local_name = "VEL"
             case (PROF_BCSCYCL);  local_name = "BCSCYCL"
             case (PROF_CENTRIF);  local_name = "CENTRIF"
             case (PROF_DAMPING);  local_name = "DAMPING"
             case (PROF_I18MAIN_KINE_2); local_name = "I18MAIN_KINE_2"
             case (PROF_INIT_FORCES); local_name = "INIT_FORCES"
             case (PROF_STATIC);   local_name = "STATIC"
             case (PROF_TAGNCONT); local_name = "TAGNCONT"
             case default; local_name = " "
            end select
          end if

          n = min(len_trim(local_name), 64)
          if (n > 0) then
            do i = 1, n
              name_c(i) = local_name(i:i)
            end do
            name_c(n+1) = c_null_char
            name_len_c = int(n, c_int)
          else
            name_c(1) = c_null_char
            name_len_c = 0_c_int
          end if
          call spmd_profiler_section_begin_c(tag_c, name_c, name_len_c)
        end subroutine spmd_profile_begin

! ======================================================================================================================
!! \brief End the active user section.
!! \details Emits the final segment. No-op if no section is active.
        subroutine spmd_profile_end(tag)
          implicit none
          integer, intent(in) :: tag
          integer(c_int) :: tag_c

          if (.not. spmd_profiling_enabled) return
          tag_c = int(tag, c_int)
          call spmd_profiler_section_end_c(tag_c)
        end subroutine spmd_profile_end

      end module spmd_profiler_mod
