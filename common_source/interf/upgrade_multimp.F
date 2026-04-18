Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    upgrade_multimp   ../common_source/interf/upgrade_multimp.F
!||--- called by ------------------------------------------------------
!||    i10main_tri       ../engine/source/interfaces/intsort/i10main_tri.F
!||    i11main_tri       ../engine/source/interfaces/intsort/i11main_tri.F
!||    i20main_tri       ../engine/source/interfaces/intsort/i20main_tri.F
!||    i21main_tri       ../engine/source/interfaces/intsort/i21main_tri.F
!||    i22main_tri       ../engine/source/interfaces/intsort/i22main_tri.F
!||    i23main_tri       ../engine/source/interfaces/intsort/i23main_tri.F
!||    i24main_tri       ../engine/source/interfaces/intsort/i24main_tri.F
!||    i25main_tri       ../engine/source/interfaces/intsort/i25main_tri.F
!||    i25trivox1        ../starter/source/interfaces/inter3d1/i25trivox1.F
!||    i7main_tri        ../engine/source/interfaces/intsort/i7main_tri.F
!||    i7trivox1         ../starter/source/interfaces/inter3d1/i7trivox1.F
!||    inintr            ../starter/source/interfaces/interf1/inintr.F
!||    inintr_thkvar     ../starter/source/interfaces/interf1/inintr_thkvar.F
!||    inter_sort_07     ../engine/source/interfaces/int07/inter_sort_07.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod     ../common_source/modules/interfaces/intbufdef_mod.F90
!||    restmod           ../engine/share/modules/restart_mod.F
!||====================================================================
      subroutine upgrade_multimp(ni, multimp_parameter, intbuf_tab)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use restmod
        use intbufdef_mod
!-------------------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)    :: ni, multimp_parameter
        type(intbuf_struct_), intent(inout) :: intbuf_tab
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: ity, ifq, inacti, mfrot, ncont, itied, nconte
        integer :: old_size, multimp
        integer :: old_size2
        integer :: old_size_opt, new_size_opt
        integer :: iedge1
        integer,  dimension(:), allocatable :: old_tab_i
        my_real,  dimension(:), allocatable :: old_tab_r
!======================================================================
        multimp  = ipari(npari*(ni-1)+23)
        ncont    = ipari(npari*(ni-1)+18)
        old_size = multimp * ncont

        !use for type20
        nconte    = ncont
        old_size2 = multimp * nconte
!  Set the new MULTIMP parameter for the given Interface
        ipari(npari*(ni-1)+23) = multimp_parameter
!  Set Global parameters
        ity    =     ipari(npari*(ni-1)+7)
        inacti = abs(ipari(npari*(ni-1)+22))
        mfrot  = ipari(npari*(ni-1)+30)
        ifq    =     ipari(npari*(ni-1)+31)
        iedge1 =     ipari(npari*(ni-1)+58)
        itied  =     ipari(npari*(ni-1)+85)

!======================================================================
        if (ity == 7) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          if (ifq /= 0) then
            !-----------!
            ! IFPEN
            !-----------!
            call move_alloc(intbuf_tab%ifpen, old_tab_i)
            intbuf_tab%s_ifpen = multimp_parameter * ncont
            allocate(intbuf_tab%ifpen(intbuf_tab%s_ifpen))
            intbuf_tab%ifpen = 0
            intbuf_tab%ifpen(1:old_size) = old_tab_i
            deallocate(old_tab_i)

            !-----------!
            ! FTSAVX
            !-----------!
            call move_alloc(intbuf_tab%ftsavx, old_tab_r)
            intbuf_tab%s_ftsavx = multimp_parameter * ncont
            allocate(intbuf_tab%ftsavx(intbuf_tab%s_ftsavx))
            intbuf_tab%ftsavx = 0
            intbuf_tab%ftsavx(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! FTSAVY
            !-----------!
            call move_alloc(intbuf_tab%ftsavy, old_tab_r)
            intbuf_tab%s_ftsavy = multimp_parameter * ncont
            allocate(intbuf_tab%ftsavy(intbuf_tab%s_ftsavy))
            intbuf_tab%ftsavy = 0
            intbuf_tab%ftsavy(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! FTSAVZ
            !-----------!
            call move_alloc(intbuf_tab%ftsavz, old_tab_r)
            intbuf_tab%s_ftsavz = multimp_parameter * ncont
            allocate(intbuf_tab%ftsavz(intbuf_tab%s_ftsavz))
            intbuf_tab%ftsavz = 0
            intbuf_tab%ftsavz(1:old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

          if (itied /= 0) then
            !-----------!
            ! CAND_F
            !-----------!
            call move_alloc(intbuf_tab%cand_f, old_tab_r)
            intbuf_tab%s_cand_f = 8 * multimp_parameter * ncont
            allocate(intbuf_tab%cand_f(intbuf_tab%s_cand_f))
            intbuf_tab%cand_f = 0
            intbuf_tab%cand_f(1:8*old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

          if (inacti == 5 .or. inacti == 6 .or. inacti == 7) then
            !-----------!
            ! CAND_P
            !-----------!
            call move_alloc(intbuf_tab%cand_p, old_tab_r)
            intbuf_tab%s_cand_p = multimp_parameter * ncont
            allocate(intbuf_tab%cand_p(intbuf_tab%s_cand_p))
            intbuf_tab%cand_p = 0
            intbuf_tab%cand_p(1:old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

!======================================================================
        elseif (ity == 10) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_F
          !-----------!
          call move_alloc(intbuf_tab%cand_f, old_tab_r)
          intbuf_tab%s_cand_f = 6 * multimp_parameter * ncont
          allocate(intbuf_tab%cand_f(intbuf_tab%s_cand_f))
          intbuf_tab%cand_f = 0
          intbuf_tab%cand_f(1:6*old_size) = old_tab_r
          deallocate(old_tab_r)

!======================================================================
        elseif (ity == 11) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CHAIN  (size is 2*MULTIMP*NCONT)
          !-----------!
          call move_alloc(intbuf_tab%chain, old_tab_i)
          intbuf_tab%s_chain = 2 * multimp_parameter * ncont
          allocate(intbuf_tab%chain(intbuf_tab%s_chain))
          intbuf_tab%chain = 0
          intbuf_tab%chain(1:2*old_size) = old_tab_i
          deallocate(old_tab_i)

          if (mfrot == 2) then
            !-----------!
            ! IFPEN
            !-----------!
            call move_alloc(intbuf_tab%ifpen, old_tab_i)
            intbuf_tab%s_ifpen = multimp_parameter * ncont
            allocate(intbuf_tab%ifpen(intbuf_tab%s_ifpen))
            intbuf_tab%ifpen = 0
            intbuf_tab%ifpen(1:old_size) = old_tab_i
            deallocate(old_tab_i)

            !-----------!
            ! FTSAVX
            !-----------!
            call move_alloc(intbuf_tab%ftsavx, old_tab_r)
            intbuf_tab%s_ftsavx = multimp_parameter * ncont
            allocate(intbuf_tab%ftsavx(intbuf_tab%s_ftsavx))
            intbuf_tab%ftsavx = 0
            intbuf_tab%ftsavx(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! FTSAVY
            !-----------!
            call move_alloc(intbuf_tab%ftsavy, old_tab_r)
            intbuf_tab%s_ftsavy = multimp_parameter * ncont
            allocate(intbuf_tab%ftsavy(intbuf_tab%s_ftsavy))
            intbuf_tab%ftsavy = 0
            intbuf_tab%ftsavy(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! FTSAVZ
            !-----------!
            call move_alloc(intbuf_tab%ftsavz, old_tab_r)
            intbuf_tab%s_ftsavz = multimp_parameter * ncont
            allocate(intbuf_tab%ftsavz(intbuf_tab%s_ftsavz))
            intbuf_tab%ftsavz = 0
            intbuf_tab%ftsavz(1:old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

!======================================================================
        elseif (ity == 16) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

!======================================================================
        elseif (ity == 17) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

!======================================================================
        elseif (ity == 20) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! LCAND_N
          !-----------!
          call move_alloc(intbuf_tab%lcand_n, old_tab_i)
          intbuf_tab%s_lcand_n = multimp_parameter * nconte
          allocate(intbuf_tab%lcand_n(intbuf_tab%s_lcand_n))
          intbuf_tab%lcand_n = 0
          intbuf_tab%lcand_n(1:old_size2) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! LCAND_S
          !-----------!
          call move_alloc(intbuf_tab%lcand_s, old_tab_i)
          intbuf_tab%s_lcand_s = multimp_parameter * nconte
          allocate(intbuf_tab%lcand_s(intbuf_tab%s_lcand_s))
          intbuf_tab%lcand_s = 0
          intbuf_tab%lcand_s(1:old_size2) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CHAIN20  (size is 2*MULTIMP*NCONTE)
          !-----------!
          call move_alloc(intbuf_tab%chain20, old_tab_i)
          intbuf_tab%s_chain20 = 2 * multimp_parameter * nconte
          allocate(intbuf_tab%chain20(intbuf_tab%s_chain20))
          intbuf_tab%chain20 = 0
          intbuf_tab%chain20(1:2*old_size2) = old_tab_i
          deallocate(old_tab_i)

          if (inacti == 5 .or. inacti == 6 .or. inacti == 7) then
            !-----------!
            ! CAND_P
            !-----------!
            call move_alloc(intbuf_tab%cand_p, old_tab_r)
            intbuf_tab%s_cand_p = multimp_parameter * ncont
            allocate(intbuf_tab%cand_p(intbuf_tab%s_cand_p))
            intbuf_tab%cand_p = 0
            intbuf_tab%cand_p(1:old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

          if (ifq /= 0) then
            !-----------!
            ! IFPEN
            !-----------!
            call move_alloc(intbuf_tab%ifpen, old_tab_i)
            intbuf_tab%s_ifpen = multimp_parameter * ncont
            allocate(intbuf_tab%ifpen(intbuf_tab%s_ifpen))
            intbuf_tab%ifpen = 0
            intbuf_tab%ifpen(1:old_size) = old_tab_i
            deallocate(old_tab_i)

            !-----------!
            ! CAND_FX
            !-----------!
            call move_alloc(intbuf_tab%cand_fx, old_tab_r)
            intbuf_tab%s_cand_fx = multimp_parameter * ncont
            allocate(intbuf_tab%cand_fx(intbuf_tab%s_cand_fx))
            intbuf_tab%cand_fx = 0
            intbuf_tab%cand_fx(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! CAND_FY
            !-----------!
            call move_alloc(intbuf_tab%cand_fy, old_tab_r)
            intbuf_tab%s_cand_fy = multimp_parameter * ncont
            allocate(intbuf_tab%cand_fy(intbuf_tab%s_cand_fy))
            intbuf_tab%cand_fy = 0
            intbuf_tab%cand_fy(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! CAND_FZ
            !-----------!
            call move_alloc(intbuf_tab%cand_fz, old_tab_r)
            intbuf_tab%s_cand_fz = multimp_parameter * ncont
            allocate(intbuf_tab%cand_fz(intbuf_tab%s_cand_fz))
            intbuf_tab%cand_fz = 0
            intbuf_tab%cand_fz(1:old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

!======================================================================
        elseif (ity == 21) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

!======================================================================
        elseif (ity == 22) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          if (ifq /= 0) then
            !-----------!
            ! IFPEN
            !-----------!
            call move_alloc(intbuf_tab%ifpen, old_tab_i)
            intbuf_tab%s_ifpen = multimp_parameter * ncont
            allocate(intbuf_tab%ifpen(intbuf_tab%s_ifpen))
            intbuf_tab%ifpen = 0
            intbuf_tab%ifpen(1:old_size) = old_tab_i
            deallocate(old_tab_i)
          end if

!======================================================================
        elseif (ity == 23) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! IFPEN
          !-----------!
          call move_alloc(intbuf_tab%ifpen, old_tab_i)
          intbuf_tab%s_ifpen = multimp_parameter * ncont
          allocate(intbuf_tab%ifpen(intbuf_tab%s_ifpen))
          intbuf_tab%ifpen = 0
          intbuf_tab%ifpen(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_P
          !-----------!
          call move_alloc(intbuf_tab%cand_p, old_tab_r)
          intbuf_tab%s_cand_p = multimp_parameter * ncont
          allocate(intbuf_tab%cand_p(intbuf_tab%s_cand_p))
          intbuf_tab%cand_p = 0
          intbuf_tab%cand_p(1:old_size) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! FTSAVX
          !-----------!
          call move_alloc(intbuf_tab%ftsavx, old_tab_r)
          intbuf_tab%s_ftsavx = multimp_parameter * ncont
          allocate(intbuf_tab%ftsavx(intbuf_tab%s_ftsavx))
          intbuf_tab%ftsavx = 0
          intbuf_tab%ftsavx(1:old_size) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! FTSAVY
          !-----------!
          call move_alloc(intbuf_tab%ftsavy, old_tab_r)
          intbuf_tab%s_ftsavy = multimp_parameter * ncont
          allocate(intbuf_tab%ftsavy(intbuf_tab%s_ftsavy))
          intbuf_tab%ftsavy = 0
          intbuf_tab%ftsavy(1:old_size) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! FTSAVZ
          !-----------!
          call move_alloc(intbuf_tab%ftsavz, old_tab_r)
          intbuf_tab%s_ftsavz = multimp_parameter * ncont
          allocate(intbuf_tab%ftsavz(intbuf_tab%s_ftsavz))
          intbuf_tab%ftsavz = 0
          intbuf_tab%ftsavz(1:old_size) = old_tab_r
          deallocate(old_tab_r)

!======================================================================
        elseif (ity == 24) then
!======================================================================

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          if (iedge1 > 0) then
            !-----------!
            ! CAND_T
            !-----------!
            call move_alloc(intbuf_tab%cand_t, old_tab_i)
            intbuf_tab%s_cand_t = multimp_parameter * ncont
            allocate(intbuf_tab%cand_t(intbuf_tab%s_cand_t))
            intbuf_tab%cand_t = 0
            intbuf_tab%cand_t(1:old_size) = old_tab_i
            deallocate(old_tab_i)
          end if

!======================================================================
        elseif (ity == 25) then
!======================================================================

          old_size_opt = intbuf_tab%s_cand_opt_n
          new_size_opt = max(multimp_parameter * ncont, old_size_opt)

          !-----------!
          ! CAND_E
          !-----------!
          call move_alloc(intbuf_tab%cand_e, old_tab_i)
          intbuf_tab%s_cand_e = multimp_parameter * ncont
          allocate(intbuf_tab%cand_e(intbuf_tab%s_cand_e))
          intbuf_tab%cand_e = 0
          intbuf_tab%cand_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_N
          !-----------!
          call move_alloc(intbuf_tab%cand_n, old_tab_i)
          intbuf_tab%s_cand_n = multimp_parameter * ncont
          allocate(intbuf_tab%cand_n(intbuf_tab%s_cand_n))
          intbuf_tab%cand_n = 0
          intbuf_tab%cand_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! FARM
          !-----------!
          call move_alloc(intbuf_tab%farm, old_tab_i)
          intbuf_tab%s_farm = 4 * new_size_opt
          allocate(intbuf_tab%farm(intbuf_tab%s_farm))
          intbuf_tab%farm(1:4*old_size_opt) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_OPT_N
          !-----------!
          call move_alloc(intbuf_tab%cand_opt_n, old_tab_i)
          intbuf_tab%s_cand_opt_n = new_size_opt
          allocate(intbuf_tab%cand_opt_n(intbuf_tab%s_cand_opt_n))
          intbuf_tab%cand_opt_n = 0
          intbuf_tab%cand_opt_n(1:old_size_opt) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_OPT_E
          !-----------!
          call move_alloc(intbuf_tab%cand_opt_e, old_tab_i)
          intbuf_tab%s_cand_opt_e = new_size_opt
          allocate(intbuf_tab%cand_opt_e(intbuf_tab%s_cand_opt_e))
          intbuf_tab%cand_opt_e = 0
          intbuf_tab%cand_opt_e(1:old_size_opt) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! PENM
          !-----------!
          call move_alloc(intbuf_tab%penm, old_tab_r)
          intbuf_tab%s_penm = 4 * new_size_opt
          allocate(intbuf_tab%penm(intbuf_tab%s_penm))
          intbuf_tab%penm = zero
          intbuf_tab%penm(1:4*old_size_opt) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! DISTM
          !-----------!
          call move_alloc(intbuf_tab%distm, old_tab_r)
          intbuf_tab%s_distm = new_size_opt
          allocate(intbuf_tab%distm(intbuf_tab%s_distm))
          intbuf_tab%distm(1:old_size_opt) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! LBM
          !-----------!
          call move_alloc(intbuf_tab%lbm, old_tab_r)
          intbuf_tab%s_lbm = 4 * new_size_opt
          allocate(intbuf_tab%lbm(intbuf_tab%s_lbm))
          intbuf_tab%lbm(1:4*old_size_opt) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! LCM
          !-----------!
          call move_alloc(intbuf_tab%lcm, old_tab_r)
          intbuf_tab%s_lcm = 4 * new_size_opt
          allocate(intbuf_tab%lcm(intbuf_tab%s_lcm))
          intbuf_tab%lcm(1:4*old_size_opt) = old_tab_r
          deallocate(old_tab_r)

        end if !end all interfaces type

      end subroutine upgrade_multimp

!||====================================================================
!||    upgrade_cand_opt   ../common_source/interf/upgrade_multimp.F
!||--- called by ------------------------------------------------------
!||    i25main_opt_tri    ../engine/source/interfaces/intsort/i25main_opt_tri.F
!||    i25main_slid       ../engine/source/interfaces/int25/i25main_slid.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod      ../common_source/modules/interfaces/intbufdef_mod.F90
!||    restmod            ../engine/share/modules/restart_mod.F
!||====================================================================
      subroutine upgrade_cand_opt(ni, k_stok, intbuf_tab)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use restmod
        use intbufdef_mod
!-------------------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)    :: ni, k_stok
        type(intbuf_struct_), intent(inout) :: intbuf_tab
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: ity, ifq, inacti, mfrot, nsn, nrtm
        integer :: old_size, new_size

        integer,  dimension(:), allocatable :: old_tab_i
        my_real,  dimension(:), allocatable :: old_tab_r
!======================================================================

        old_size = intbuf_tab%s_cand_opt_n
        new_size = intbuf_tab%i_stok(2) + k_stok

!  Set Global parameters
        ity    =     ipari(npari*(ni-1)+7)
        inacti = abs(ipari(npari*(ni-1)+22))
        mfrot  = ipari(npari*(ni-1)+30)
        ifq    =     ipari(npari*(ni-1)+31)
        nsn    =     ipari(npari*(ni-1)+5)
        nrtm   =     ipari(npari*(ni-1)+4)

!======================================================================
        if (ity == 25) then
!======================================================================

          !-----------!
          ! FARM
          !-----------!
          call move_alloc(intbuf_tab%farm, old_tab_i)
          intbuf_tab%s_farm = 4 * new_size
          allocate(intbuf_tab%farm(intbuf_tab%s_farm))
          intbuf_tab%farm = 0
          intbuf_tab%farm(1:4*old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_OPT_N
          !-----------!
          call move_alloc(intbuf_tab%cand_opt_n, old_tab_i)
          intbuf_tab%s_cand_opt_n = new_size
          allocate(intbuf_tab%cand_opt_n(intbuf_tab%s_cand_opt_n))
          intbuf_tab%cand_opt_n = 0
          intbuf_tab%cand_opt_n(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CAND_OPT_E
          !-----------!
          call move_alloc(intbuf_tab%cand_opt_e, old_tab_i)
          intbuf_tab%s_cand_opt_e = new_size
          allocate(intbuf_tab%cand_opt_e(intbuf_tab%s_cand_opt_e))
          intbuf_tab%cand_opt_e = 0
          intbuf_tab%cand_opt_e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! PENM
          !-----------!
          call move_alloc(intbuf_tab%penm, old_tab_r)
          intbuf_tab%s_penm = 4 * new_size
          allocate(intbuf_tab%penm(intbuf_tab%s_penm))
          intbuf_tab%penm = zero
          intbuf_tab%penm(1:4*old_size) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! DISTM
          !-----------!
          call move_alloc(intbuf_tab%distm, old_tab_r)
          intbuf_tab%s_distm = new_size
          allocate(intbuf_tab%distm(intbuf_tab%s_distm))
          intbuf_tab%distm = zero
          intbuf_tab%distm(1:old_size) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! LBM
          !-----------!
          call move_alloc(intbuf_tab%lbm, old_tab_r)
          intbuf_tab%s_lbm = 4 * new_size
          allocate(intbuf_tab%lbm(intbuf_tab%s_lbm))
          intbuf_tab%lbm = zero
          intbuf_tab%lbm(1:4*old_size) = old_tab_r
          deallocate(old_tab_r)

          !-----------!
          ! LCM
          !-----------!
          call move_alloc(intbuf_tab%lcm, old_tab_r)
          intbuf_tab%s_lcm = 4 * new_size
          allocate(intbuf_tab%lcm(intbuf_tab%s_lcm))
          intbuf_tab%lcm = zero
          intbuf_tab%lcm(1:4*old_size) = old_tab_r
          deallocate(old_tab_r)

        end if !end all interfaces type

      end subroutine upgrade_cand_opt

!||====================================================================
!||    upgrade_lcand_edg   ../common_source/interf/upgrade_multimp.F
!||--- called by ------------------------------------------------------
!||    i25main_tri         ../engine/source/interfaces/intsort/i25main_tri.F
!||    inint3              ../starter/source/interfaces/inter3d1/inint3.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod       ../common_source/modules/interfaces/intbufdef_mod.F90
!||    restmod             ../engine/share/modules/restart_mod.F
!||====================================================================
      subroutine upgrade_lcand_edg(ni, multimp_parameter, intbuf_tab)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use restmod
        use intbufdef_mod
!-------------------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)    :: ni, multimp_parameter
        type(intbuf_struct_), intent(inout) :: intbuf_tab
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: ity, inacti, nconte
        integer :: old_size, new_size, multimp, ifq

        integer,  dimension(:), allocatable :: old_tab_i
        my_real,  dimension(:), allocatable :: old_tab_r
!======================================================================

        multimp  = ipari(npari*(ni-1)+87)
        nconte   = ipari(npari*(ni-1)+88)
        old_size = multimp * nconte
        new_size = multimp_parameter * nconte

!  Set the new MULTIMP parameter for the given Interface
        ipari(npari*(ni-1)+87) = multimp_parameter
!  Set Global parameters
        ity    =     ipari(npari*(ni-1)+7)
        inacti = abs(ipari(npari*(ni-1)+22))
        ifq    =     ipari(npari*(ni-1)+31)

!======================================================================
        if (ity == 25) then

          !-----------!
          ! CANDM_E2E
          !-----------!
          call move_alloc(intbuf_tab%candm_e2e, old_tab_i)
          intbuf_tab%s_candm_e2e = multimp_parameter * nconte
          allocate(intbuf_tab%candm_e2e(intbuf_tab%s_candm_e2e))
          intbuf_tab%candm_e2e = 0
          intbuf_tab%candm_e2e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CANDS_E2E
          !-----------!
          call move_alloc(intbuf_tab%cands_e2e, old_tab_i)
          intbuf_tab%s_cands_e2e = multimp_parameter * nconte
          allocate(intbuf_tab%cands_e2e(intbuf_tab%s_cands_e2e))
          intbuf_tab%cands_e2e = 0
          intbuf_tab%cands_e2e(1:old_size) = old_tab_i
          deallocate(old_tab_i)

!       IF(INACTI == 5) THEN
          !-----------!
          ! CAND_P
          !-----------!
          call move_alloc(intbuf_tab%cand_p, old_tab_r)
          intbuf_tab%s_cand_p = multimp_parameter * nconte
          allocate(intbuf_tab%cand_p(intbuf_tab%s_cand_p))
          intbuf_tab%cand_p = 0
          intbuf_tab%cand_p(1:old_size) = old_tab_r
          deallocate(old_tab_r)
!       ENDIF

          if (ifq /= 0) then
            !-----------!
            ! IFPEN_E2E
            !-----------!
            call move_alloc(intbuf_tab%ifpen_e, old_tab_i)
            intbuf_tab%s_ifpen_e = multimp_parameter * nconte
            allocate(intbuf_tab%ifpen_e(intbuf_tab%s_ifpen_e))
            intbuf_tab%ifpen_e = 0
            intbuf_tab%ifpen_e(1:old_size) = old_tab_i
            deallocate(old_tab_i)

            !-----------!
            ! FTSAVX_E
            !-----------!
            call move_alloc(intbuf_tab%ftsavx_e, old_tab_r)
            intbuf_tab%s_ftsavx_e = multimp_parameter * nconte
            allocate(intbuf_tab%ftsavx_e(intbuf_tab%s_ftsavx_e))
            intbuf_tab%ftsavx_e = 0
            intbuf_tab%ftsavx_e(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! FTSAVY_E
            !-----------!
            call move_alloc(intbuf_tab%ftsavy_e, old_tab_r)
            intbuf_tab%s_ftsavy_e = multimp_parameter * nconte
            allocate(intbuf_tab%ftsavy_e(intbuf_tab%s_ftsavy_e))
            intbuf_tab%ftsavy_e = 0
            intbuf_tab%ftsavy_e(1:old_size) = old_tab_r
            deallocate(old_tab_r)

            !-----------!
            ! FTSAVZ_E
            !-----------!
            call move_alloc(intbuf_tab%ftsavz_e, old_tab_r)
            intbuf_tab%s_ftsavz_e = multimp_parameter * nconte
            allocate(intbuf_tab%ftsavz_e(intbuf_tab%s_ftsavz_e))
            intbuf_tab%ftsavz_e = 0
            intbuf_tab%ftsavz_e(1:old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

            !-----------!
            ! PENE_OLD_E
            !-----------!

          call move_alloc(intbuf_tab%pene_old_e, old_tab_r)
          intbuf_tab%s_pene_old_e = multimp_parameter * nconte
          allocate(intbuf_tab%pene_old_e(intbuf_tab%s_pene_old_e))
          intbuf_tab%pene_old_e = 0
          intbuf_tab%pene_old_e(1:old_size) = old_tab_r
          deallocate(old_tab_r)

            !-----------!
            ! STIF_OLD_E
            !-----------!
          call move_alloc(intbuf_tab%stif_old_e, old_tab_r)
          intbuf_tab%s_stif_old_e = multimp_parameter * nconte
          allocate(intbuf_tab%stif_old_e(intbuf_tab%s_pene_old_e))
          intbuf_tab%stif_old_e = 0
          intbuf_tab%stif_old_e(1:old_size) = old_tab_r
          deallocate(old_tab_r)

        end if !end all interfaces type

      end subroutine upgrade_lcand_edg

!||====================================================================
!||    upgrade_lcand_e2s   ../common_source/interf/upgrade_multimp.F
!||--- called by ------------------------------------------------------
!||    i25main_tri         ../engine/source/interfaces/intsort/i25main_tri.F
!||    inint3              ../starter/source/interfaces/inter3d1/inint3.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod       ../common_source/modules/interfaces/intbufdef_mod.F90
!||    restmod             ../engine/share/modules/restart_mod.F
!||====================================================================
      subroutine upgrade_lcand_e2s(ni, multimp_parameter, intbuf_tab)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use restmod
        use intbufdef_mod
!-------------------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
#include      "implicit_f.inc"
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in)    :: ni, multimp_parameter
        type(intbuf_struct_), intent(inout) :: intbuf_tab
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: ity, inacti, nconte
        integer :: old_size, new_size, multimp, ifq

        integer,  dimension(:), allocatable :: old_tab_i
        my_real,  dimension(:), allocatable :: old_tab_r
!======================================================================

        multimp  = ipari(npari*(ni-1)+89)
        nconte   = ipari(npari*(ni-1)+88)
        old_size = multimp * nconte
        new_size = multimp_parameter * nconte

!  Set the new MULTIMP parameter for the given Interface
        ipari(npari*(ni-1)+89) = multimp_parameter
!  Set Global parameters
        ity    =     ipari(npari*(ni-1)+7)
        inacti = abs(ipari(npari*(ni-1)+22))
        ifq    =     ipari(npari*(ni-1)+31)

!======================================================================
        if (ity == 25) then

          !-----------!
          ! CANDM_E2S
          !-----------!
          call move_alloc(intbuf_tab%candm_e2s, old_tab_i)
          intbuf_tab%s_candm_e2s = multimp_parameter * nconte
          allocate(intbuf_tab%candm_e2s(intbuf_tab%s_candm_e2s))
          intbuf_tab%candm_e2s = 0
          intbuf_tab%candm_e2s(1:old_size) = old_tab_i
          deallocate(old_tab_i)

          !-----------!
          ! CANDS_E2S
          !-----------!
          call move_alloc(intbuf_tab%cands_e2s, old_tab_i)
          intbuf_tab%s_cands_e2s = multimp_parameter * nconte
          allocate(intbuf_tab%cands_e2s(intbuf_tab%s_cands_e2s))
          intbuf_tab%cands_e2s = 0
          intbuf_tab%cands_e2s(1:old_size) = old_tab_i
          deallocate(old_tab_i)

!       IF(INACTI == 5) THEN
          !-----------!
          ! CAND_PS
          !-----------!
          call move_alloc(intbuf_tab%cand_ps, old_tab_r)
          intbuf_tab%s_cand_ps = 4 * multimp_parameter * nconte
          allocate(intbuf_tab%cand_ps(intbuf_tab%s_cand_ps))
          intbuf_tab%cand_ps = 0
          intbuf_tab%cand_ps(1:4*old_size) = old_tab_r
          deallocate(old_tab_r)
!       ENDIF

          if (ifq /= 0) then
            !-----------!
            ! IFPEN_E2S
            !-----------!
            call move_alloc(intbuf_tab%ifpen_e2s, old_tab_i)
            intbuf_tab%s_ifpen_e2s = multimp_parameter * nconte
            allocate(intbuf_tab%ifpen_e2s(intbuf_tab%s_ifpen_e2s))
            intbuf_tab%ifpen_e2s = 0
            intbuf_tab%ifpen_e2s(1:old_size) = old_tab_i
            deallocate(old_tab_i)

            !-----------!
            ! FTSAVX_E2S
            !-----------!

            call move_alloc(intbuf_tab%ftsavx_e2s, old_tab_r)
            intbuf_tab%s_ftsavx_e2s = 4 * multimp_parameter * nconte
            allocate(intbuf_tab%ftsavx_e2s(intbuf_tab%s_ftsavx_e2s))
            intbuf_tab%ftsavx_e2s = 0
            intbuf_tab%ftsavx_e2s(1:old_size) = old_tab_r
            deallocate(old_tab_r)


            !-----------!
            ! FTSAVY_E2S
            !-----------!

            call move_alloc(intbuf_tab%ftsavy_e2s, old_tab_r)
            intbuf_tab%s_ftsavy_e2s = 4 * multimp_parameter * nconte
            allocate(intbuf_tab%ftsavy_e2s(intbuf_tab%s_ftsavy_e2s))
            intbuf_tab%ftsavy_e2s = 0
            intbuf_tab%ftsavy_e2s(1:old_size) = old_tab_r
            deallocate(old_tab_r)


            !-----------!
            ! FTSAVZ_E2S
            !-----------!

            call move_alloc(intbuf_tab%ftsavz_e2s, old_tab_r)
            intbuf_tab%s_ftsavz_e2s = 4 * multimp_parameter * nconte
            allocate(intbuf_tab%ftsavz_e2s(intbuf_tab%s_ftsavz_e2s))
            intbuf_tab%ftsavz_e2s = 0
            intbuf_tab%ftsavz_e2s(1:old_size) = old_tab_r
            deallocate(old_tab_r)
          end if

            !-------------!
            ! PENE_OLD_E2S
            !-------------!
          call move_alloc(intbuf_tab%pene_old_e2s, old_tab_r)
          intbuf_tab%s_pene_old_e2s = 4 * multimp_parameter * nconte
          allocate(intbuf_tab%pene_old_e2s(intbuf_tab%s_pene_old_e2s))
          intbuf_tab%pene_old_e2s = 0
          intbuf_tab%pene_old_e2s(1:4*old_size) = old_tab_r
          deallocate(old_tab_r)

            !-------------!
            ! STIF_OLD_E2S
            !-------------!
          call move_alloc(intbuf_tab%stif_old_e2s, old_tab_r)
          intbuf_tab%s_stif_old_e2s = 4 * multimp_parameter * nconte
          allocate(intbuf_tab%stif_old_e2s(intbuf_tab%s_stif_old_e2s))
          intbuf_tab%stif_old_e2s = 0
          intbuf_tab%stif_old_e2s(1:4*old_size) = old_tab_r
          deallocate(old_tab_r)


        end if !end all interfaces type

      end subroutine upgrade_lcand_e2s
