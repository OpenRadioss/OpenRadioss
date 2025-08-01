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
!||    updfail_mod   ../starter/source/materials/updfail.F90
!||--- called by ------------------------------------------------------
!||    lectur        ../starter/source/starter/lectur.F
!||====================================================================
      module updfail_mod
        implicit none
      contains

! ========================================================================================
! \brief initialization of parameters in failure models
! \details replacement of function and table Ids by system numbers in matparam%fail structure

! ========================================================================================
!
!||====================================================================
!||    updfail                   ../starter/source/materials/updfail.F90
!||--- called by ------------------------------------------------------
!||    lectur                    ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                    ../starter/source/output/message/message.F
!||    brokmann_random           ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
!||    fail_fun2sys              ../starter/source/materials/tools/fail_fun2sys.F
!||    mattab_usr2sys            ../starter/source/materials/tools/mattab_usr2sys.F
!||    random_walk_dmg           ../starter/source/materials/fail/fractal/random_walk_dmg.F90
!||--- uses       -----------------------------------------------------
!||    brokmann_random_mod       ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
!||    message_mod               ../starter/share/message_module/message_mod.F
!||    random_walk_dmg_mod       ../starter/source/materials/fail/fractal/random_walk_dmg.F90
!||    stack_mod                 ../starter/share/modules1/stack_mod.F
!||    table_mod                 ../starter/share/modules1/table_mod.F
!||====================================================================
        subroutine updfail(mat_param ,nummat ,nfunct ,ntable ,func_id ,table ,      &
          fail_fractal,ngrshel   ,ngrsh3n,igrsh4n,igrsh3n,                 &
          nixc   ,ixc    ,nixtg  ,ixtg   ,numelc ,numeltg ,                &
          iworksh,stack,igeo,npropgi,numgeo,fail_brokmann)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod
          use table_mod
          use stack_mod
          use message_mod
          use groupdef_mod
          use random_walk_def_mod
          use random_walk_dmg_mod
          use brokmann_random_def_mod
          use brokmann_random_mod
          use constant_mod, only : zero
! ---------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
          integer ,intent(in) :: nummat                              !< total number of material models
          integer ,intent(in) :: numgeo                              !< total number of element properties
          integer ,intent(in) :: nfunct                              !< total number of system functions
          integer ,intent(in) :: ntable                              !< total number of system function tables
          integer ,intent(in) :: ngrshel                             !< number of 4n shell element groups
          integer ,intent(in) :: ngrsh3n                             !< number of 3n shell element groups
          integer ,intent(in) :: numelc                              !< number of system 4n shell elements
          integer ,intent(in) :: numeltg                             !< number of system 3n shell elements
          integer ,intent(in) :: nixc                                !< size of 4n shell connectivity table
          integer ,intent(in) :: nixtg                               !< size of 3n shell connectivity table
          integer ,intent(in) :: npropgi                             !< parameter size of numgeo
          integer ,dimension(npropgi,numgeo),intent(in) :: igeo      !< property parameter table
          integer ,dimension(nixc,numelc)   ,intent(in) :: ixc       !< 4n shell connectivity table
          integer ,dimension(nixtg,numeltg) ,intent(in) :: ixtg      !< 3n shell connectivity table
          integer ,dimension(nfunct)        ,intent(in) :: func_id   !< input function ids
          type(ttable), dimension(ntable)   ,intent(in) :: table     !< input function tables
          type(matparam_struct_) ,dimension(nummat)     :: mat_param !< material law parameter structure
          type (group_)    ,dimension(ngrshel) :: igrsh4n            !< 4n shell group structure
          type (group_)    ,dimension(ngrsh3n) :: igrsh3n            !< 3n shell group structure
          type (fail_fractal_)  ,intent(inout) :: fail_fractal       !< fractal model structure
          type (fail_brokmann_) ,intent(inout) :: fail_brokmann      !< brokmann model structure
          type (stack_ply)                     :: stack              !< element stack structure
          integer ,dimension(3,numelc+numeltg),intent(in) :: iworksh !<
!-----------------------------------------------
!   L o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: imat,ir,nfail,ifail,flag_brokmann,nfail_fractal,nfail_brokmann
!=======================================================================
          nfail_fractal  = 0
          nfail_brokmann = 0
!
          do imat=1,nummat
            nfail  = mat_param(imat)%nfail
            if (nfail > 0) then
              do ir = 1,nfail
!
                ! function(s) user ids into internal ids
                if (mat_param(imat)%fail(ir)%nfunc > 0) then
                  call fail_fun2sys(mat_param(imat)%fail(ir) ,                  &
                    mat_param(imat)%title    ,                   &
                    mat_param(imat)%mat_id   ,                   &
                    nfunct  ,func_id  )
                end if
!
                ! table(s) user ids into internal ids
                if (mat_param(imat)%fail(ir)%ntable > 0) then
                  call mattab_usr2sys(mat_param(imat)%title    ,                 &
                    mat_param(imat)%mat_id,                                      &
                    ntable ,table ,                                              &
                    mat_param(imat)%fail(ir)%ntable ,                            &
                    mat_param(imat)%fail(ir)%table  )
                end if
!
                ! /fail/tab2 temperature dependency check
                if (mat_param(imat)%fail(ir)%irupt == 41) then
                  ! check if temperature dependency is defined twice: in epsf table and fct_TEMP
                  if ((table(mat_param(imat)%fail(ir)%table(1))%ndim == 3) .and.            &
                    (mat_param(imat)%fail(ir)%ifunc(4) > 0)) then
                    call ancmsg(msgid=3055, msgtype=msgwarning, anmode=aninfo_blind,        &
                      i1=mat_param(imat)%mat_id,                                            &
                      c1=mat_param(imat)%title)
                  endif
                  ! check if temperature dependency is defined twice: in inst table and fct_TEMP
                  if (mat_param(imat)%fail(ir)%table(2) > 0) then
                    if ((table(mat_param(imat)%fail(ir)%table(2))%ndim == 3) .and.          &
                      (mat_param(imat)%fail(ir)%ifunc(4) > 0)) then
                      call ancmsg(msgid=3056, msgtype=msgwarning, anmode=aninfo_blind,      &
                        i1=mat_param(imat)%mat_id,                                          &
                        c1=mat_param(imat)%title)
                    endif
                  endif
                  ! check if strain rate dependency tables are in logarithmic scale
                  if (nint(mat_param(imat)%fail(ir)%uparam(9)) == 1) then
                    if (mat_param(imat)%fail(ir)%table(3) > 0) then
                      if (table(mat_param(imat)%fail(ir)%table(3))%ndim == 2) then
                        if (table(mat_param(imat)%fail(ir)%table(3))%x(2)%values(1) < zero) then
                          mat_param(imat)%fail(ir)%uparam(21) = 1
                        endif
                      endif
                    endif
                  endif
                  if (mat_param(imat)%fail(ir)%ifunc(2) > 0) then
                    if (table(mat_param(imat)%fail(ir)%ifunc(2))%x(1)%values(1) < zero) then
                      mat_param(imat)%fail(ir)%uparam(22) = 1
                    endif
                  endif
                endif
                ! count number of /fail/fractal_dmg models
                if (mat_param(imat)%fail(ir)%irupt == 12) then
                  nfail_fractal = nfail_fractal + 1
                end if
                ! count number of /fail/alter + Brokmann models
                if (mat_param(imat)%fail(ir)%irupt == 28) then
                  flag_brokmann = nint(mat_param(imat)%fail(ir)%uparam(22))
                  if (flag_brokmann == 1) nfail_brokmann = nfail_brokmann + 1
                end if
!
              enddo
            endif
          enddo
!------------------------------------------------
!     fractal damage model initialization
!------------------------------------------------
          fail_fractal%nfail = nfail_fractal
          allocate(fail_fractal%fractal(nfail_fractal))

          if (nfail_fractal > 0) then
            ifail = 0
            do imat=1,nummat
              nfail  = mat_param(imat)%nfail
              if (nfail > 0) then
                do ir = 1,nfail
                  if (mat_param(imat)%fail(ir)%irupt == 12) then
                    ifail = ifail + 1
                    fail_fractal%fractal(ifail)%imat = imat
                    call random_walk_dmg(fail_fractal%fractal(ifail),mat_param(imat)%fail(ir),  &
                      ngrshel,ngrsh3n,igrsh4n,igrsh3n,                                          &
                      nixc   ,ixc    ,nixtg  ,ixtg  ,numelc ,numeltg,                           &
                      iworksh,stack  ,igeo   ,npropgi,numgeo )
                  end if
                enddo
              endif
            enddo
          endif
!------------------------------------------------
!     /fail/alter with Brokmann random crack initialization
!------------------------------------------------
          fail_brokmann%nfail = nfail_brokmann
          allocate(fail_brokmann%brokmann(nfail_brokmann))
          if (nfail_brokmann > 0) then
            ifail = 0
            do imat=1,nummat
              nfail  = mat_param(imat)%nfail
              if (nfail > 0) then
                do ir = 1,nfail
                  if (mat_param(imat)%fail(ir)%irupt == 28) then
                    flag_brokmann = nint(mat_param(imat)%fail(ir)%uparam(22))
                    if (flag_brokmann ==1) then
                      ifail = ifail + 1
                      fail_brokmann%brokmann(ifail)%imat = imat
                      call brokmann_random(                                   &
                        fail_brokmann%brokmann(ifail),                     &
                        mat_param(imat)%fail(ir),                          &
                        nixc   ,ixc    ,nixtg  ,ixtg  ,numelc ,numeltg ,   &
                        iworksh,stack  ,igeo   ,npropgi,numgeo )
                    end if
                  end if
                end do
              end if
            end do
          end if
!-----------
          return
        end subroutine updfail
!-----------
      end module updfail_mod
