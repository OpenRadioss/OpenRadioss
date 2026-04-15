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
!||    hm_read_yield_criterion_barlat1989_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat1989.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion                  ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||====================================================================
      module hm_read_yield_criterion_barlat1989_mod
        implicit none
! \brief Read Barlat 1989 yield criterion input data for /MAT/LAW131
! \details Read the Barlat 1989 anisotropic yield criterion parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_yield_criterion_barlat1989   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat1989.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion              ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||--- calls      -----------------------------------------------------
!||    calculp2                             ../starter/source/materials/mat/mat057/calculp2.F90
!||    hm_get_float_array_index             ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||--- uses       -----------------------------------------------------
!||    calculp2_mod                         ../starter/source/materials/mat/mat057/calculp2.F90
!||    hm_option_read_mod                   ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                         ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_yield_criterion_barlat1989(                         &
          ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab   ,     &
          lsubmodel,iout     ,is_encrypted)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use calculp2_mod
          use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< material key
          integer,                 intent(inout) :: icrit                 !< yield criterion type
          integer,                 intent(inout) :: nupar_crit            !< number of yield criterion parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_crit         !< yield criterion parameters
          logical,                 intent(in)    :: is_available          !< availability flag
          type(unit_type_),        intent(in)    :: unitab                !< units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< submodel data structure
          integer,                 intent(in)    :: iout                  !< output unit
          logical,                 intent(in)    :: is_encrypted          !< encryption flag
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: r00,r45,r90,m,r,h
!===============================================================================
!  
          !===================================================================
          !< Barlat (1989) yield criterion
          !===================================================================
          call hm_get_float_array_index("CRIT_BARL89_R00",r00,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BARL89_R45",r45,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BARL89_R90",r90,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BARL89_M"  ,m  ,ikey,is_available,lsubmodel,unitab)
          !< Yield criterion type
          icrit = 4
          !< Compute Barlat 89 coefficients
          if (r00 == zero) r00 = one
          if (r45 == zero) r45 = one
          if (r90 == zero) r90 = one
          if (m   == zero) m   = six
          r = r00/(one + r00)
          h = r90/(one + r90)
          !< Number of parameters
          nupar_crit = 5
          !< Save yield criterion parameters
          upar_crit(1) = two*sqrt(r*h)    !< c
          upar_crit(2) = two-upar_crit(1) !< a
          upar_crit(3) = sqrt(r/h)        !< h
          upar_crit(4) = one              !< p
          call calculp2(upar_crit(2),upar_crit(1),upar_crit(3),upar_crit(4),m,r45)
          upar_crit(5) = m
          !< Printing yield criterion parameters
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) r00,r45,r90,m,upar_crit(2),upar_crit(1),          &
              upar_crit(3),upar_crit(4)
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"BARLAT (1989) YIELD CRITERION                          ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"LANKFORD COEFFICIENT R00 . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"LANKFORD COEFFICIENT R45 . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"LANKFORD COEFFICIENT R90 . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"BARLAT YIELD EXPONENT M. . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"BARLAT COEFFICIENT A . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"BARLAT COEFFICIENT C . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"BARLAT COEFFICIENT H . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"BARLAT COEFFICIENT P . . . . . . . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_yield_criterion_barlat1989
      end module hm_read_yield_criterion_barlat1989_mod