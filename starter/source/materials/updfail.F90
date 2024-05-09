!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
!Chd|====================================================================
!Chd|  updfail                       source/materials/updfail.F
!Chd|-- called by -----------
!Chd|        lectur                        source/starter/lectur.F
!Chd|-- calls ---------------
!Chd|====================================================================
      module updfail_mod
      contains

! ========================================================================================
! \brief initialization of parameters in failure models
! \details replacement of function and table Ids by system numbers in matparam%fail structure

! ========================================================================================
!
        subroutine updfail(mat_param ,nummat ,nfunct ,ntable ,func_id ,table ,    &
          fail_fractal,ngrshel   ,ngrsh3n,igrsh4n,igrsh3n,       &
          nixc   ,ixc    ,nixtg  ,ixtg   ,numelc ,numeltg )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod
          use table_mod
          use message_mod
          use groupdef_mod
          use random_walk_def_mod
          use random_walk_dmg_mod
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
          integer ,intent(in) :: nummat                              !< total number of material models
          integer ,intent(in) :: nfunct                              !< total number of system functions
          integer ,intent(in) :: ntable                              !< total number of system function tables
          integer ,intent(in) :: ngrshel                             !< number of 4n shell element groups
          integer ,intent(in) :: ngrsh3n                             !< number of 3n shell element groups
          integer ,intent(in) :: numelc                              !< number of system 4n shell elements
          integer ,intent(in) :: numeltg                             !< number of system 3n shell elements
          integer ,intent(in) :: nixc                                !< size of 4n shell connectivity table
          integer ,intent(in) :: nixtg                               !< size of 3n shell connectivity table
          integer ,dimension(nixc,numelc)   ,intent(in) :: ixc       !< 4n shell connectivity table
          integer ,dimension(nixtg,numeltg) ,intent(in) :: ixtg      !< 3n shell connectivity table
          integer ,dimension(nfunct)        ,intent(in) :: func_id   !< input function ids
          type(ttable), dimension(ntable)   ,intent(in) :: table     !< input function tables
          type(matparam_struct_) ,dimension(nummat)     :: mat_param !< material law parameter structure
          type (group_)  ,dimension(ngrshel)  :: igrsh4n             !< 4n shell group structure
          type (group_)  ,dimension(ngrsh3n)  :: igrsh3n             !< 3n shell group structure
          type (fail_fractal_) ,intent(inout) :: fail_fractal        !< fractal model structure
!-----------------------------------------------
!   L o c a l   v a r i a b l e s
!-----------------------------------------------
          integer imat,mat_id,ir,nfail,ifrac,nfail_fractal
!=======================================================================
          nfail_fractal = 0
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
                    mat_param(imat)%mat_id,                    &
                    ntable ,table ,                            &
                    mat_param(imat)%fail(ir)%ntable ,          &
                    mat_param(imat)%fail(ir)%table  )
                end if
!
                ! /fail/tab2 temperature dependency check
                if (mat_param(imat)%fail(ir)%irupt == 41) then
                  ! check if temperature dependency is defined twice: in epsf table and fct_TEMP
                  if ((table(mat_param(imat)%fail(ir)%table(1))%ndim == 3) .and.                     &
                    (mat_param(imat)%fail(ir)%ifunc(4) > 0)) then
                    call ancmsg(msgid=3055,                                                           &
                      msgtype=msgwarning,                                                   &
                      anmode=aninfo_blind,                                                  &
                      i1=mat_param(imat)%mat_id,                                            &
                      c1=mat_param(imat)%title)
                  endif
                  ! check if temperature dependency is defined twice: in inst table and fct_TEMP
                  if (mat_param(imat)%fail(ir)%table(2) > 0) then
                    if ((table(mat_param(imat)%fail(ir)%table(2))%ndim == 3) .and.                    &
                      (mat_param(imat)%fail(ir)%ifunc(4) > 0)) then
                      call ancmsg(msgid=3056,                                                         &
                        msgtype=msgwarning,                                                 &
                        anmode=aninfo_blind,                                                &
                        i1=mat_param(imat)%mat_id,                                          &
                        c1=mat_param(imat)%title)
                    endif
                  endif
                endif
!           count number of /fail/fractal_dmg models
                if (mat_param(imat)%fail(ir)%irupt == 12) then
                  nfail_fractal = nfail_fractal + 1
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
            ifrac = 0
            do imat=1,nummat
              nfail  = mat_param(imat)%nfail
              if (nfail > 0) then
                mat_id = mat_param(imat)%mat_id
                do ir = 1,nfail
                  if (mat_param(imat)%fail(ir)%irupt == 12) then
                    ifrac = ifrac + 1
                    call random_walk_dmg(fail_fractal%fractal(ifrac),mat_param(imat)%fail(ir),      &
                      ngrshel,ngrsh3n,igrsh4n,igrsh3n,                                            &
                      imat   ,nixc   ,ixc    ,nixtg  ,ixtg  ,numelc ,numeltg)
                  end if
                enddo
              endif
            enddo
          endif
!-----------
          return
        end
      end module updfail_mod
