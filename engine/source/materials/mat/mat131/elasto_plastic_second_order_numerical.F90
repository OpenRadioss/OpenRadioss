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
!||    elasto_plastic_second_order_numerical_mod   ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress                    ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||====================================================================
      module elasto_plastic_second_order_numerical_mod
! \brief Compute second-order numerical approximation for /MAT/LAW131
! \details Compute second-order numerical derivatives of the yield function
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    elasto_plastic_second_order_numerical   ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress                ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||--- calls      -----------------------------------------------------
!||    yield_criterion_barlat1989              ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat1989.F90
!||    yield_criterion_barlat2000              ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat2000.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                            ../common_source/modules/constant_mod.F
!||    matparam_def_mod                        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                           ../common_source/modules/precision_mod.F90
!||    yield_criterion_barlat1989_mod          ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat1989.F90
!||    yield_criterion_barlat2000_mod          ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat2000.F90
!||====================================================================
      subroutine elasto_plastic_second_order_numerical(                        &
        matparam ,nel      ,eltype   ,icrit    ,                               &
        signxx   , signyy  ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,           &
        N        ,offset   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use precision_mod, only : WP
        use constant_mod
        use yield_criterion_barlat1989_mod
        use yield_criterion_barlat2000_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),            intent(in)  :: matparam !< Material parameters data structure
        integer,                           intent(in)  :: nel      !< Number of elements in the group
        integer,                           intent(in)  :: eltype   !< Element type (1 for solids, 2 for shells)
        integer,                           intent(in)  :: icrit    !< Yield criterion type
        real(kind=WP), dimension(nel),     intent(in)  :: signxx   !< Stress tensor component xx 
        real(kind=WP), dimension(nel),     intent(in)  :: signyy   !< Stress tensor component yy
        real(kind=WP), dimension(nel),     intent(in)  :: signzz   !< Stress tensor component zz
        real(kind=WP), dimension(nel),     intent(in)  :: signxy   !< Stress tensor component xy
        real(kind=WP), dimension(nel),     intent(in)  :: signyz   !< Stress tensor component yz
        real(kind=WP), dimension(nel),     intent(in)  :: signzx   !< Stress tensor component zx
        real(kind=WP), dimension(nel),     intent(in)  :: normxx   !< Yield criterion gradient component xx
        real(kind=WP), dimension(nel),     intent(in)  :: normyy   !< Yield criterion gradient component yy
        real(kind=WP), dimension(nel),     intent(in)  :: normzz   !< Yield criterion gradient component zz
        real(kind=WP), dimension(nel),     intent(in)  :: normxy   !< Yield criterion gradient component xy
        real(kind=WP), dimension(nel),     intent(in)  :: normyz   !< Yield criterion gradient component yz
        real(kind=WP), dimension(nel),     intent(in)  :: normzx   !< Yield criterion gradient component zx
        real(kind=WP), dimension(nel,6,6), intent(out) :: N        !< Second-order derivative of the yield criterion (6x6 matrix)
        integer,                           intent(in)  :: offset   !< Offset for the yield criterion parameters in the material parameters data structure
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: j, jj, njdir
        integer, dimension(6) :: jlist
        real(kind=WP) :: h
        real(kind=WP), dimension(nel) :: norm, seq_p, inv_h
        real(kind=WP), dimension(nel) :: sxx_p,syy_p,szz_p,sxy_p,syz_p,szx_p
        real(kind=WP), dimension(nel) :: nxx_p,nyy_p,nzz_p,nxy_p,nyz_p,nzx_p
!===============================================================================
!
        !< Initialisation of the output array
        N(1:nel,1:6,1:6) = zero
!
        !< Computation of the perturbation step size
        h = epsilon(one)**(0.25_WP)
        ! -> For solid elements 
        if (eltype == 1) then
          norm(1:nel) = signxx(1:nel)**2 + signyy(1:nel)**2                    &
                      + signzz(1:nel)**2 + two*signxy(1:nel)**2                &
                      + two*signyz(1:nel)**2 + two*signzx(1:nel)**2
        ! -> For shell elements
        else
          norm(1:nel) = signxx(1:nel)**2 + signyy(1:nel)**2                    &
                      + two*signxy(1:nel)**2
        endif
        norm(1:nel) = sqrt(norm(1:nel))
        norm(1:nel) = max(norm(1:nel),one)
!
        !< List of the perturbed directions : 
        ! -> 6 for solids
        if (eltype == 1) then
          njdir = 6
          jlist = [1, 2, 3, 4, 5, 6]
        ! -> 3 for shells
        else
          njdir = 3
          jlist = [1, 2, 4, 0, 0, 0] 
        endif
!
        !< Loop over the perturbed directions
        do jj = 1, njdir
          j = jlist(jj)
!
          !< Perturbed stress states : +h in the j-th direction
          sxx_p(1:nel) = signxx(1:nel)
          syy_p(1:nel) = signyy(1:nel)
          szz_p(1:nel) = signzz(1:nel)
          sxy_p(1:nel) = signxy(1:nel)
          syz_p(1:nel) = signyz(1:nel)
          szx_p(1:nel) = signzx(1:nel)
!
          select case (j)
            case (1)
              sxx_p(1:nel) = signxx(1:nel) + h*norm(1:nel)
            case (2)
              syy_p(1:nel) = signyy(1:nel) + h*norm(1:nel)
            case (3)
              szz_p(1:nel) = signzz(1:nel) + h*norm(1:nel)
            case (4)
              sxy_p(1:nel) = signxy(1:nel) + h*norm(1:nel)
            case (5)
              syz_p(1:nel) = signyz(1:nel) + h*norm(1:nel)
            case (6)
              szx_p(1:nel) = signzx(1:nel) + h*norm(1:nel)
          end select
!
          !< Evaluation of the yield criterion and its gradient for the 
          ! perturbed stress states
          select case(icrit)
            !-------------------------------------------------------------------
            !< Barlat 89 yield criterion
            !-------------------------------------------------------------------
            case(4)
              call yield_criterion_barlat1989(                                 &          
                matparam ,nel      ,seq_p    ,sxx_p    ,syy_p    ,sxy_p    ,   &
                nxx_p    ,nyy_p    ,nzz_p    ,nxy_p    ,nyz_p    ,nzx_p    ,   &
                offset   ) 
            !-------------------------------------------------------------------
            !< Barlat 2000 yield criterion
            !-------------------------------------------------------------------
            case(5)
              call yield_criterion_barlat2000(                                 &          
                matparam ,nel      ,seq_p    ,sxx_p    ,syy_p    ,sxy_p    ,   &
                nxx_p    ,nyy_p    ,nzz_p    ,nxy_p    ,nyz_p    ,nzx_p    ,   &
                offset   ) 
          end select
!
          !< Inverse of h*norm for finite differences
          inv_h(1:nel) = one / (h*norm(1:nel))
!
          !< Forward finite differences : N(:,i,j) = (norm_i(sigma+h) - norm_i(sigma)) / h
          N(1:nel,1,j) = (nxx_p(1:nel) - normxx(1:nel))*inv_h(1:nel)
          N(1:nel,2,j) = (nyy_p(1:nel) - normyy(1:nel))*inv_h(1:nel)
          N(1:nel,4,j) = (nxy_p(1:nel) - normxy(1:nel))*inv_h(1:nel)
          if (eltype == 1) then
            N(1:nel,3,j) = (nzz_p(1:nel) - normzz(1:nel))*inv_h(1:nel)
            N(1:nel,5,j) = (nyz_p(1:nel) - normyz(1:nel))*inv_h(1:nel)
            N(1:nel,6,j) = (nzx_p(1:nel) - normzx(1:nel))*inv_h(1:nel)
          endif
!
        enddo
!
      end subroutine elasto_plastic_second_order_numerical
      end module elasto_plastic_second_order_numerical_mod
