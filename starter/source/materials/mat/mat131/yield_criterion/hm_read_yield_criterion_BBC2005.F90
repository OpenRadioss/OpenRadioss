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
!||    hm_read_yield_criterion_bbc2005_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_BBC2005.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion               ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||====================================================================
      module hm_read_yield_criterion_bbc2005_mod
        implicit none
! \brief Read BBC2005 yield criterion input data for /MAT/LAW131
! \details Read the BBC2005 anisotropic yield criterion parameters
!          for /MAT/LAW131.
      contains
      
!||====================================================================
!||    hm_read_yield_criterion_bbc2005   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_BBC2005.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion           ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||--- calls      -----------------------------------------------------
!||    bbc2005_calcul_coeffs             ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||    hm_get_float_array_index          ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_int_array_index            ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||--- uses       -----------------------------------------------------
!||    bbc2005_calcul_coeffs_mod         ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||    hm_option_read_mod                ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_yield_criterion_bbc2005(                        &
         ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab   ,  &
         lsubmodel,iout     ,is_encrypted)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use bbc2005_calcul_coeffs_mod
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
          real(kind=8)  :: y0, y45, y90, r0, r45, r90, yb, rb, k_in
          integer       :: k_val
          real(kind=WP) :: a, b, l_coeff, m_coeff, n_coeff, p, q, r 
!===============================================================================

          !===================================================================
          !< BBC (2005) yield criterion mapping from CFG
          !===================================================================
          call hm_get_float_array_index("CRIT_BBC2005_Y0" ,y0 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BBC2005_Y45",y45,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BBC2005_Y90",y90,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BBC2005_R0" ,r0 ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BBC2005_R45",r45,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BBC2005_R90",r90,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BBC2005_YB" ,yb ,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("CRIT_BBC2005_RB" ,rb ,ikey,is_available,lsubmodel,unitab)
          call hm_get_int_array_index("CRIT_BBC2005_K"  ,k_val,ikey,is_available,lsubmodel)
          !< Yield criterion type ID (Assigned an arbitrary ID: 6, ensure it does not conflict)
          icrit = 6

          !< Cast lattice exponent K to integer (defaulting to 3 for BCC if not provided)
          if (k_val <= 0) k_val = 3 

          !< Compute BBC2005 math coefficients using Newton-Raphson 
  
          call bbc2005_calcul_coeffs(y0, y45, y90, r0, r45, r90, yb, rb, k_val, &
                                     a, b, l_coeff, m_coeff, n_coeff, p, q, r)  

          !< Number of parameters packed into upar_crit array
          nupar_crit = 9

          !< Save the 9 underlying mathematical yield criterion parameters to engine array
          upar_crit(1) = a
          upar_crit(2) = b
          upar_crit(3) = l_coeff
          upar_crit(4) = m_coeff
          upar_crit(5) = n_coeff
          upar_crit(6) = p
          upar_crit(7) = q
          upar_crit(8) = r
          upar_crit(9) = real(k_val)

          !< Printing yield criterion parameters to the Starter OUT file
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) y0, y45, y90, r0, r45, r90, yb, rb, k_val, &
                             a, b, l_coeff, m_coeff, n_coeff, p, q, r
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"BBC (2005) YIELD CRITERION                             ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"YIELD STRESS (Y0)  . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"YIELD STRESS (Y45) . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"YIELD STRESS (Y90) . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"LANKFORD COEFFICIENT (R00)  . . . . .  . . . . . . . =",1PG20.13/  &
          5X,"LANKFORD COEFFICIENT (R45) . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"LANKFORD COEFFICIENT (R90) . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"BIAXIAL YIELD STRESS (YB)  . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"BIAXIAL LANKFORD COEFF (RB)  . . . . . . . . . . . . =",1PG20.13/  &
          5X,"LATTICE EXPONENT (K) . . . . . . . . . . . . . . . . =",I20/       &
          5X,"--- CALCULATED MATHEMATICAL COEFFICIENTS ---       ",/         &
          5X,"PARAMETER (a)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"PARAMETER (b)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"PARAMETER (L)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"PARAMETER (M)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"PARAMETER (N)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"PARAMETER (P)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"PARAMETER (Q)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/  &
          5X,"PARAMETER (R)  . . . . . . . . . . . . . . . . . . . =",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_yield_criterion_bbc2005


      end module hm_read_yield_criterion_bbc2005_mod