!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    hm_read_yield_criterion_vonmises_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_vonmises.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion                ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||====================================================================
      module hm_read_yield_criterion_vonmises_mod
        implicit none
! \brief Read von Mises yield criterion input data for /MAT/LAW131
! \details Read the von Mises isotropic yield criterion parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_yield_criterion_vonmises   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_vonmises.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion            ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                 ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                       ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_yield_criterion_vonmises(                           &
          icrit    ,nupar_crit,iout     ,is_encrypted)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(inout) :: icrit                 !< yield criterion type
          integer,                 intent(inout) :: nupar_crit            !< number of yield criterion parameters
          integer,                 intent(in)    :: iout                  !< output unit
          logical,                 intent(in)    :: is_encrypted          !< encryption flag
!===============================================================================
!        
          !===================================================================
          !< Von Mises yield criterion
          !===================================================================
          !< Yield criterion type
          icrit = 1
          !< Number of parameters
          nupar_crit = 0
          !< Printing yield criterion parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) 
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"VON MISES YIELD CRITERION                              ",/,      &
          5X,"-------------------------------------------------------",/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_yield_criterion_vonmises
      end module hm_read_yield_criterion_vonmises_mod