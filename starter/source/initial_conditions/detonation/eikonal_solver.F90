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
      !||    eikonal_solver_mod   ../starter/source/initial_conditions/detonation/eikonal_solver.F90
      !||--- called by ------------------------------------------------------
      !||    initia               ../starter/source/elements/initia/initia.F
      !||====================================================================
      module eikonal_solver_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Solver for the Eikonal equation (arrival time depending on medium velocity)
!! \details a numerical scheme is used to solve the Eikonal equation on the relevant domain.
!! \details Option available for Detonators for which flag I_SHADOW is available.
      !||====================================================================
      !||    eikonal_solver                     ../starter/source/initial_conditions/detonation/eikonal_solver.F90
      !||--- called by ------------------------------------------------------
      !||    initia                             ../starter/source/elements/initia/initia.F
      !||--- calls      -----------------------------------------------------
      !||    eikonal_fast_marching_method       ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- uses       -----------------------------------------------------
      !||    detonators_mod                     ../starter/share/modules1/detonators_mod.F
      !||    eikonal_fast_marching_method_mod   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||====================================================================
        subroutine eikonal_solver(ixq      , nixq     , numelq  , &
                                  ixs      , nixs     , numels  , &
                                  ixtg     , nixtg    , numeltg , &
                                  x        , numnod   , title55 , &
                                  elbuf_tab, ngroup   , nparg   , &
                                  nod2eltg , knod2eltg, &
                                  nod2elq  , knod2elq , &
                                  nod2els  , knod2els , &
                                  iparg    , ale_connectivity, npropm, nummat, pm, n2d, detonators,&
                                  npropmi  , ipm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use eikonal_fast_marching_method_mod , only : eikonal_fast_marching_method
          use elbufdef_mod, only : elbuf_struct_
          use ale_connectivity_mod , only : t_ale_connectivity
          use constant_mod , only : zero
          use detonators_mod , only : detonators_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: ngroup  !< number of groups
          integer,intent(in) :: numnod  !< number of nodes
          integer,intent(in) :: nixq    !< array size ixq
          integer,intent(in) :: numelq  !< number of quad elems (solid)
          integer,intent(in) :: nixs    !< array size ixs
          integer,intent(in) :: numeltg !< number of tria elems (solid)
          integer,intent(in) :: nixtg   !< array size ixtg
          integer,intent(in) :: numels  !< number of hexa elems (solid)
          integer,intent(in),target :: ixq(nixq,numelq) !< quad connectivities
          integer,intent(in),target :: ixtg(nixtg,numeltg) !< quad connectivities
          integer,intent(in),target :: ixs(nixs,numels) !< quad connectivities
          integer,intent(in) :: nparg !< array size
          integer,intent(in) :: iparg(nparg,ngroup)
          my_real,intent(in) :: x(3,numnod) !< node coordinates
          integer,intent(in) :: npropm, nummat
          my_real,intent(in) :: pm(npropm,nummat)
          type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
          type (t_ale_connectivity), intent(inout) :: ale_connectivity
          integer,intent(in) :: n2d
          type (detonators_struct_),intent(in) :: detonators
          integer,intent(in) :: nod2eltg(3*numeltg)
          integer,intent(in) :: nod2elq(4*numelq)
          integer,intent(in) :: nod2els(8*numels)
          integer,intent(in) :: knod2eltg(numnod+1)
          integer,intent(in) :: knod2elq(numnod+1)
          integer,intent(in) :: knod2els(numnod+1)
          integer,intent(in) :: npropmi
          integer,intent(in) :: ipm(npropmi, nummat)
          character*89,intent(in) :: title55
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: I_shadow_flag
          integer :: idet, mat_det
          logical :: is_printed_title55
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Precondition
! ----------------------------------------------------------------------------------------------------------------------
          if( detonators%n_det == 0 )return              !NO DETONATORS => RETURN
          if( numels + numelq + numeltg*n2d == 0)return   !NO SOLID ELEMS => RETURN
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_printed_title55 = .false.

          do idet = 1, detonators%n_det_point

            I_shadow_flag = detonators%point(idet)%shadow
            If(I_shadow_flag == 0)cycle
            Mat_det = detonators%point(idet)%mat
            if(.not. is_printed_title55)then
              write(istdo, '(a)') title55
              is_printed_title55 =.false.
            end if

            ! FAST MARCHING METHOD
            if(n2d == 0) then
              call eikonal_fast_marching_method(&
                                     ixs,nixs,numels,x,numnod, &
                                     elbuf_tab,ngroup,nparg,iparg,ale_connectivity,npropm,nummat,pm,&
                                     detonators, idet, 6, nod2els, knod2els, npropmi, ipm)

            elseif(numelq > 0)then
              call eikonal_fast_marching_method(&
                                     ixq,nixq,numelq,x,numnod, &
                                     elbuf_tab,ngroup,nparg,iparg,ale_connectivity,npropm,nummat,pm,&
                                     detonators, idet, 4, nod2elq, knod2elq, npropmi, ipm)

            elseif(numeltg > 0)then
              call eikonal_fast_marching_method(&
                                     ixtg,nixtg,numeltg,x,numnod, &
                                     elbuf_tab,ngroup,nparg,iparg,ale_connectivity,npropm,nummat,pm,&
                                     detonators, idet, 3, nod2eltg, knod2eltg, npropmi, ipm)
            end if

          end do

      end subroutine eikonal_solver
! ----------------------------------------------------------------------------------------------------------------------

      end module eikonal_solver_mod
