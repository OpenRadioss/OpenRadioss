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
!||    yield_criterion_hershey_mod   ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hershey.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress      ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||====================================================================
      module yield_criterion_hershey_mod
! \brief Compute Hershey yield criterion for /MAT/LAW131
! \details Compute the equivalent stress and its first and second-order
!          derivatives using the Hershey isotropic yield criterion
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    yield_criterion_hershey        ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hershey.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress       ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||--- calls      -----------------------------------------------------
!||    valpvec_v                      ../engine/source/materials/mat/mat033/sigeps33.F
!||    valpvecdp_v                    ../engine/source/materials/mat/mat033/sigeps33.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                   ../common_source/modules/constant_mod.F
!||    matparam_def_mod               ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod                      ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod                  ../common_source/modules/precision_mod.F90
!||    yield_criterion_vonmises_mod   ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_vonmises.F90
!||====================================================================
      subroutine yield_criterion_hershey(                                      &
          matparam ,nel      ,seq      ,iresp    ,eltype   ,                   &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,         &
          N        ,second_order,offset)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use mvsiz_mod
        use precision_mod, only : WP
        use yield_criterion_vonmises_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
        integer,                       intent(in)    :: nel      !< Number of elements in the group
        real(kind=WP), dimension(nel), intent(inout) :: seq      !< Equivalent stress
        integer,                       intent(in)    :: iresp    !< Precision flag
        integer,                       intent(in)    :: eltype   !< Element type
        real(kind=WP), dimension(nel), intent(in)    :: signxx   !< Current stress xx
        real(kind=WP), dimension(nel), intent(in)    :: signyy   !< Current stress yy
        real(kind=WP), dimension(nel), intent(in)    :: signzz   !< Current stress zz
        real(kind=WP), dimension(nel), intent(in)    :: signxy   !< Current stress xy
        real(kind=WP), dimension(nel), intent(in)    :: signyz   !< Current stress yz
        real(kind=WP), dimension(nel), intent(in)    :: signzx   !< Current stress zx
        real(kind=WP), dimension(nel), intent(inout) :: normxx   !< 1st derivative of equivalent stress wrt stress xx
        real(kind=WP), dimension(nel), intent(inout) :: normyy   !< 1st derivative of equivalent stress wrt stress yy
        real(kind=WP), dimension(nel), intent(inout) :: normzz   !< 1st derivative of equivalent stress wrt stress zz
        real(kind=WP), dimension(nel), intent(inout) :: normxy   !< 1st derivative of equivalent stress wrt stress xy
        real(kind=WP), dimension(nel), intent(inout) :: normyz   !< 1st derivative of equivalent stress wrt stress yz
        real(kind=WP), dimension(nel), intent(inout) :: normzx   !< 1st derivative of equivalent stress wrt stress zx
        real(kind=WP), dimension(nel,6,6), intent(inout) :: N    !< 2nd derivative of equivalent stress
        logical,                       intent(in)    :: second_order !< Flag for computing second order derivatives
        integer,                       intent(in)    :: offset   !< Offset in the material parameters array for yield criterion parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,j
        real(kind=WP), dimension(mvsiz,6) :: strs
        real(kind=WP), dimension(mvsiz,3) :: pr_strs
        real(kind=WP), dimension(mvsiz,3,3) :: dir
        real(kind=WP) :: dsigeq_dsig1,dsigeq_dsig2,dsigeq_dsig3
        real(kind=WP) :: dsig1_dsig(6),dsig2_dsig(6),dsig3_dsig(6)
        real(kind=WP) :: dsig1_dsigxx,dsig1_dsigyy,dsig1_dsigxy
        real(kind=WP) :: dsig2_dsigxx,dsig2_dsigyy,dsig2_dsigxy
        real(kind=WP), dimension(nel) :: norm,invnorm,center,rootv,ab12,ab1,ab2
        real(kind=WP) :: nexp,nexp_m1,one_m_nexp,inv_nexp,nexp_m2,half_one_m_nexp
        real(kind=WP) :: loc_seq, loc_ab12, loc_ab1, loc_ab2
        real(kind=WP) :: loc_s1, loc_s2, loc_s3, loc_s4, loc_rootv
        real(kind=WP) :: loc_norm, loc_half_seq_pow
        real(kind=WP) :: diff_ratio, two_s4_ratio
        real(kind=WP) :: row1_xx, row1_yy, row1_xy
        real(kind=WP) :: row2_xx, row2_yy, row2_xy
        real(kind=WP) :: ab12_n1, ab1_n1, ab2_n1
        real(kind=WP) :: ab12_n2, ab1_n2, ab2_n2
        real(kind=WP) :: d2sig12, d2sig2sig1, d2sig22
        real(kind=WP) :: fac2,inv_seq,inv_norm_i
        real(kind=WP) :: d12, d23, d31
        real(kind=WP) :: ad12, ad23, ad31
        real(kind=WP) :: sd12, sd23, sd31
        real(kind=WP) :: ad12_n1, ad23_n1, ad31_n1
        real(kind=WP) :: ad12_n2, ad23_n2, ad31_n2
        real(kind=WP) :: half_seq_pow
        real(kind=WP) :: row1(6), row2(6), row3(6)
        real(kind=WP) :: d11, d12e, d13e, d21, d22e, d23e, d31e, d32e, d33e
        real(kind=WP) :: H11, H12, H13, H22, H23, H33
!===============================================================================
!
        !=======================================================================
        !< - Hershey yield criterion and its derivatives
        !=======================================================================
        !< Hershey exponent
        nexp = matparam%uparam(offset + 1)
        !< Compute constant
        nexp_m1        = nexp - one
        one_m_nexp     = one - nexp
        inv_nexp       = one / nexp
        nexp_m2         = nexp - two
        half_one_m_nexp = half * one_m_nexp
        !< Reset derivatives of eq. stress
        normxx(1:nel) = zero
        normyy(1:nel) = zero
        normzz(1:nel) = zero
        normxy(1:nel) = zero
        normyz(1:nel) = zero
        normzx(1:nel) = zero
        if (second_order) N(1:nel,1:6,1:6) = zero
!
        !< Solid element
        if (eltype == 1) then 
        norm(1:nel) = (signxx(1:nel)*signxx(1:nel) +                           &
                       signyy(1:nel)*signyy(1:nel) +                           &
                       signzz(1:nel)*signzz(1:nel) -                           &
                       signxx(1:nel)*signyy(1:nel) -                           &
                       signyy(1:nel)*signzz(1:nel) -                           &
                       signzz(1:nel)*signxx(1:nel)) * third +                  &
                       signxy(1:nel)*signxy(1:nel) +                           &
                       signyz(1:nel)*signyz(1:nel) +                           &
                       signzx(1:nel)*signzx(1:nel)
          norm(1:nel) = max(sqrt(three*norm(1:nel)),one)
          invnorm(1:nel) = one/norm(1:nel)
          strs(1:nel,1) = signxx(1:nel)*invnorm(1:nel)
          strs(1:nel,2) = signyy(1:nel)*invnorm(1:nel)
          strs(1:nel,3) = signzz(1:nel)*invnorm(1:nel)
          strs(1:nel,4) = signxy(1:nel)*invnorm(1:nel)
          strs(1:nel,5) = signyz(1:nel)*invnorm(1:nel)
          strs(1:nel,6) = signzx(1:nel)*invnorm(1:nel)
          !< Compute principal strains and directions
          if (iresp == 1) then
            call valpvecdp_v(strs ,pr_strs ,dir ,nel)
          else
            call valpvec_v(strs ,pr_strs ,dir ,nel)
          endif
          !< Equivalent stress
          do i = 1,nel
            seq(i) = half * ((abs(pr_strs(i,1) - pr_strs(i,2)))**nexp +        &
                             (abs(pr_strs(i,2) - pr_strs(i,3)))**nexp +        &
                             (abs(pr_strs(i,3) - pr_strs(i,1)))**nexp )
          enddo
          where (seq(1:nel) > zero)
            seq(1:nel) = seq(1:nel)**inv_nexp
          elsewhere
            seq(1:nel) = zero
          end where
          do i = 1, nel
            if (seq(i) > zero) then 
              loc_seq   = seq(i)
              loc_norm  = norm(i)
              inv_norm_i = one / loc_norm
              inv_seq   = one / loc_seq
              loc_s1 = pr_strs(i,1)
              loc_s2 = pr_strs(i,2)
              loc_s3 = pr_strs(i,3)
              d12 = loc_s1 - loc_s2
              d23 = loc_s2 - loc_s3
              d31 = loc_s3 - loc_s1
              ad12 = abs(d12)
              sd12 = sign(one, d12)
              ad23 = abs(d23)
              sd23 = sign(one, d23)
              ad31 = abs(d31)
              sd31 = sign(one, d31)
              ad12_n1 = ad12**nexp_m1
              ad23_n1 = ad23**nexp_m1
              ad31_n1 = ad31**nexp_m1
              half_seq_pow = half * loc_seq**(one_m_nexp)
              dsigeq_dsig1 = half_seq_pow * ( ad12_n1*sd12 - ad31_n1*sd31)
              dsigeq_dsig2 = half_seq_pow * ( ad23_n1*sd23 - ad12_n1*sd12)
              dsigeq_dsig3 = half_seq_pow * ( ad31_n1*sd31 - ad23_n1*sd23)
              d11  = dir(i,1,1)
              d12e = dir(i,1,2)
              d13e = dir(i,1,3)
              d21  = dir(i,2,1)
              d22e = dir(i,2,2)
              d23e = dir(i,2,3)
              d31e = dir(i,3,1)
              d32e = dir(i,3,2)
              d33e = dir(i,3,3)
              dsig1_dsig(1) =     d11*d11  
              dsig2_dsig(1) =     d12e*d12e
              dsig3_dsig(1) =     d13e*d13e
              dsig1_dsig(2) =     d21*d21  
              dsig2_dsig(2) =     d22e*d22e
              dsig3_dsig(2) =     d23e*d23e
              dsig1_dsig(3) =     d31e*d31e
              dsig2_dsig(3) =     d32e*d32e
              dsig3_dsig(3) =     d33e*d33e
              dsig1_dsig(4) = two*d11*d21  
              dsig2_dsig(4) = two*d12e*d22e
              dsig3_dsig(4) = two*d13e*d23e
              dsig1_dsig(5) = two*d21*d31e 
              dsig2_dsig(5) = two*d22e*d32e
              dsig3_dsig(5) = two*d23e*d33e
              dsig1_dsig(6) = two*d11*d31e 
              dsig2_dsig(6) = two*d12e*d32e
              dsig3_dsig(6) = two*d13e*d33e
              !< Assembly of the derivative of the eq. stress w.r.t. stress tensor
              normxx(i) = dsigeq_dsig1*dsig1_dsig(1) +                         &
                          dsigeq_dsig2*dsig2_dsig(1) +                         &
                          dsigeq_dsig3*dsig3_dsig(1)
              normyy(i) = dsigeq_dsig1*dsig1_dsig(2) +                         &
                          dsigeq_dsig2*dsig2_dsig(2) +                         &
                          dsigeq_dsig3*dsig3_dsig(2)     
              normzz(i) = dsigeq_dsig1*dsig1_dsig(3) +                         &
                          dsigeq_dsig2*dsig2_dsig(3) +                         &
                          dsigeq_dsig3*dsig3_dsig(3)      
              normxy(i) = dsigeq_dsig1*dsig1_dsig(4) +                         &
                          dsigeq_dsig2*dsig2_dsig(4) +                         &
                          dsigeq_dsig3*dsig3_dsig(4)          
              normyz(i) = dsigeq_dsig1*dsig1_dsig(5) +                         &
                          dsigeq_dsig2*dsig2_dsig(5) +                         &
                          dsigeq_dsig3*dsig3_dsig(5)          
              normzx(i) = dsigeq_dsig1*dsig1_dsig(6) +                         &
                          dsigeq_dsig2*dsig2_dsig(6) +                         &
                          dsigeq_dsig3*dsig3_dsig(6)  
              !< Second order derivative of eq. stress
              if (second_order) then 
                ad12_n2 = ad12**nexp_m2
                ad23_n2 = ad23**nexp_m2
                ad31_n2 = ad31**nexp_m2
                fac2 = half_one_m_nexp * loc_seq**one_m_nexp
                H11 = fac2*(two*dsigeq_dsig1*dsigeq_dsig1*inv_seq -            &
                                                ad12_n2 - ad31_n2)
                H22 = fac2*(two*dsigeq_dsig2*dsigeq_dsig2*inv_seq -            &
                                                ad23_n2 - ad12_n2)
                H33 = fac2*(two*dsigeq_dsig3*dsigeq_dsig3*inv_seq -            &
                                                ad31_n2 - ad23_n2)
                H12 = fac2*(two*dsigeq_dsig1*dsigeq_dsig2*inv_seq + ad12_n2)
                H13 = fac2*(two*dsigeq_dsig1*dsigeq_dsig3*inv_seq + ad31_n2)
                H23 = fac2*(two*dsigeq_dsig2*dsigeq_dsig3*inv_seq + ad23_n2)
                row1(1:6) = H11*dsig1_dsig(1:6) + H12*dsig2_dsig(1:6) +        &
                                                  H13*dsig3_dsig(1:6)
                row2(1:6) = H12*dsig1_dsig(1:6) + H22*dsig2_dsig(1:6) +        &
                                                  H23*dsig3_dsig(1:6)
                row3(1:6) = H13*dsig1_dsig(1:6) + H23*dsig2_dsig(1:6) +        &
                                                  H33*dsig3_dsig(1:6)
                do j = 1, 6
                  N(i,1:6,j) = (row1(1:6)*dsig1_dsig(j) +                      &
                                row2(1:6)*dsig2_dsig(j) +                      &
                                row3(1:6)*dsig3_dsig(j)) * inv_norm_i
                enddo
              endif
            endif
          enddo
          !< Remove normalization of stress tensor and its derivative
          seq(1:nel) = seq(1:nel)*norm(1:nel)
        !< Shell element
        elseif (eltype == 2) then 
          !< Normalization of the stress tensor
          norm(1:nel) = signxx(1:nel)*signxx(1:nel) +                          &
                        signyy(1:nel)*signyy(1:nel) -                          &
                        signxx(1:nel)*signyy(1:nel) +                          &
                        three*(signxy(1:nel)*signxy(1:nel))
          norm(1:nel) = max(sqrt(norm(1:nel)),one)
          invnorm(1:nel) = one/norm(1:nel)
          strs(1:nel,1)  = signxx(1:nel)*invnorm(1:nel)
          strs(1:nel,2)  = signyy(1:nel)*invnorm(1:nel)
          strs(1:nel,4)  = signxy(1:nel)*invnorm(1:nel)
          !< Principal stresses under plane stress condition
          center(1:nel) = strs(1:nel,1) + strs(1:nel,2)
          ab12(1:nel)   = strs(1:nel,1) - strs(1:nel,2)
          rootv(1:nel)  = sqrt(ab12(1:nel)*ab12(1:nel) +                       &
                          four*(strs(1:nel,4)*strs(1:nel,4)))     
          pr_strs(1:nel,1) = half*(center(1:nel) + rootv(1:nel))
          pr_strs(1:nel,2) = half*(center(1:nel) - rootv(1:nel))
          rootv(1:nel) = max(rootv(1:nel),em20)
          ab12(1:nel) = rootv(1:nel) 
          ab1(1:nel)  = abs(-pr_strs(1:nel,1))
          ab2(1:nel)  = abs(pr_strs(1:nel,2))
          !< Equivalent stress
          seq(1:nel) = half*((ab12(1:nel))**nexp + (ab2(1:nel))**nexp +        &
                                                   (ab1(1:nel))**nexp)
          where (seq(1:nel) > zero)
            seq(1:nel) = seq(1:nel)**inv_nexp
          elsewhere
            seq(1:nel) = zero
          end where
          !< Derivatives of eq. stress
          do i = 1,nel
            if (seq(i) > zero) then
              !< First order derivative of eq. stress
              loc_seq   = seq(i)
              loc_ab12  = ab12(i)
              loc_ab1   = ab1(i)
              loc_ab2   = ab2(i)
              loc_s1    = pr_strs(i,1)
              loc_s2    = pr_strs(i,2)
              loc_s4    = strs(i,4)
              loc_rootv = rootv(i)
              loc_norm  = norm(i)
              ab12_n1 = loc_ab12**(nexp_m1)
              ab1_n1  = loc_ab1 **(nexp_m1)
              ab2_n1  = loc_ab2 **(nexp_m1)
              loc_half_seq_pow = half * loc_seq**(one - nexp)
              dsigeq_dsig1 = loc_half_seq_pow * (                              &
                   ab12_n1 * sign(one, loc_s1 - loc_s2) -                      &
                   ab1_n1  * sign(one, -loc_s1))
              dsigeq_dsig2 = loc_half_seq_pow * (                              &
                   ab2_n1  * sign(one,  loc_s2) -                              &
                   ab12_n1 * sign(one,  loc_s1 - loc_s2))
          
              diff_ratio    = (strs(i,1) - strs(i,2)) / loc_rootv
              two_s4_ratio  = two * loc_s4 / loc_rootv
              dsig1_dsigxx =  half * (one + diff_ratio)
              dsig1_dsigyy =  half * (one - diff_ratio)
              dsig1_dsigxy =  two_s4_ratio
              dsig2_dsigxx =  half * (one - diff_ratio)   
              dsig2_dsigyy =  half * (one + diff_ratio) 
              dsig2_dsigxy = -two_s4_ratio
              normxx(i) = dsigeq_dsig1*dsig1_dsigxx + dsigeq_dsig2*dsig2_dsigxx
              normyy(i) = dsigeq_dsig1*dsig1_dsigyy + dsigeq_dsig2*dsig2_dsigyy
              normzz(i) = -normxx(i) - normyy(i)
              normxy(i) = dsigeq_dsig1*dsig1_dsigxy + dsigeq_dsig2*dsig2_dsigxy
              normyz(i) = zero
              normzx(i) = zero
              !< Second order derivative of eq. stress  
              if (second_order) then
                ab12_n2 = loc_ab12**(nexp - two)
                ab1_n2  = loc_ab1 **(nexp - two)
                ab2_n2  = loc_ab2 **(nexp - two)
                fac2 = loc_half_seq_pow * one_m_nexp
                inv_seq = one / loc_seq
                d2sig12    = fac2 * (two*(dsigeq_dsig1*dsigeq_dsig1)*inv_seq   &
                                 - ab12_n2 - ab1_n2)
                d2sig2sig1 = fac2 * (two*(dsigeq_dsig2*dsigeq_dsig1)*inv_seq   &
                                 + ab12_n2)      
                d2sig22    = fac2 * (two*(dsigeq_dsig2*dsigeq_dsig2)*inv_seq   &
                                 - ab2_n2 - ab12_n2)
                row1_xx = d2sig12    * dsig1_dsigxx + d2sig2sig1 * dsig2_dsigxx
                row1_yy = d2sig12    * dsig1_dsigyy + d2sig2sig1 * dsig2_dsigyy
                row1_xy = d2sig12    * dsig1_dsigxy + d2sig2sig1 * dsig2_dsigxy
                row2_xx = d2sig2sig1 * dsig1_dsigxx + d2sig22    * dsig2_dsigxx
                row2_yy = d2sig2sig1 * dsig1_dsigyy + d2sig22    * dsig2_dsigyy
                row2_xy = d2sig2sig1 * dsig1_dsigxy + d2sig22    * dsig2_dsigxy
                inv_norm_i = one / loc_norm
                N(i,1,1) = (row1_xx*dsig1_dsigxx + row2_xx*dsig2_dsigxx) * inv_norm_i
                N(i,1,2) = (row1_xx*dsig1_dsigyy + row2_xx*dsig2_dsigyy) * inv_norm_i
                N(i,1,4) = (row1_xx*dsig1_dsigxy + row2_xx*dsig2_dsigxy) * inv_norm_i
                N(i,2,1) = (row1_yy*dsig1_dsigxx + row2_yy*dsig2_dsigxx) * inv_norm_i
                N(i,2,2) = (row1_yy*dsig1_dsigyy + row2_yy*dsig2_dsigyy) * inv_norm_i
                N(i,2,4) = (row1_yy*dsig1_dsigxy + row2_yy*dsig2_dsigxy) * inv_norm_i
                N(i,4,1) = (row1_xy*dsig1_dsigxx + row2_xy*dsig2_dsigxx) * inv_norm_i
                N(i,4,2) = (row1_xy*dsig1_dsigyy + row2_xy*dsig2_dsigyy) * inv_norm_i
                N(i,4,4) = (row1_xy*dsig1_dsigxy + row2_xy*dsig2_dsigxy) * inv_norm_i
              endif
            endif
          enddo
          !< Remove normalization of stress tensor and its derivative
          seq(1:nel) = seq(1:nel)*norm(1:nel)
        endif  
!
      end subroutine yield_criterion_hershey
      end module yield_criterion_hershey_mod
