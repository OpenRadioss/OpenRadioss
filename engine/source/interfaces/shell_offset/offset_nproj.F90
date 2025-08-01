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
!||    offset_nproj_mod   ../engine/source/interfaces/shell_offset/offset_nproj.F90
!||--- called by ------------------------------------------------------
!||    resol              ../engine/source/engine/resol.F
!||====================================================================
      module offset_nproj_mod
      contains
!=======================================================================================================================
!!\brief This subroutine do nodal offset projection for shell
!=======================================================================================================================
!||====================================================================
!||    offset_nproj          ../engine/source/interfaces/shell_offset/offset_nproj.F90
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    foat_to_6_float       ../engine/source/system/parit.F
!||    spmd_exch_n           ../engine/source/mpi/generic/spmd_exch_n.F
!||    spmd_exch_vnpon       ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    inter_sh_offset_mod   ../engine/source/modules/interfaces/sh_offset_mod.F90
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    spmd_exch_vnpon_mod   ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
!||====================================================================
        subroutine offset_nproj(nspmd,numnod,xyz,sh_offset_tab,iparit)
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,em20
          use inter_sh_offset_mod , only:sh_offset_
          use spmd_exch_vnpon_mod , only:spmd_exch_vnpon
          use precision_mod, only:WP
          implicit none
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
!
          integer, intent(in   )                      :: numnod          !< number of node
          integer, intent(in   )                      :: nspmd           !< number domains
          integer, intent(in   )                      :: iparit          !< flag P/ON
          real(kind=WP), intent(inout),dimension(3,numnod)  :: xyz             !< node coordinates
          type (sh_offset_)                           :: sh_offset_tab   !< offset struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,n,nnod,lenr,n_l,ndim1,ndim2,ng
          real(kind=WP)  :: shelloff,r(3),s(3),t(3),xv(3,4),norm2
          double precision  :: dx(3) ,t6(6,3)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          sh_offset_tab%norm_n = zero
          if (iparit >0) then
            sh_offset_tab%norm_n6 = zero
            do i=1,sh_offset_tab%nsh_oset
              do k = 1,4
                n = sh_offset_tab%ix_offset(k,i)
                xv(1:3,k) = xyz(1:3,n)
              end do
              if (sh_offset_tab%ix_offset(4,i)/=sh_offset_tab%ix_offset(3,i)) then
                nnod = 4
                do j=1,3
                  r(j) = xv(j,2)+xv(j,3)-xv(j,1)-xv(j,4)
                  s(j) = xv(j,3)+xv(j,4)-xv(j,1)-xv(j,2)
                end do
              else
                nnod = 3
                do j=1,3
                  r(j) = xv(j,3)-xv(j,2)
                  s(j) = xv(j,1)-xv(j,2)
                end do
              end if
              t(1) = r(2) * s(3) - r(3) * s(2)
              t(2) = r(3) * s(1) - r(1) * s(3)
              t(3) = r(1) * s(2) - r(2) * s(1)
              norm2 = t(1)*t(1)+t(2)*t(2)+t(3)*t(3)
              t(1:3) = t(1:3)/sqrt(max(em20,norm2))
              call foat_to_6_float(1  ,3  ,t ,t6 )
              do k = 1,nnod
                n = sh_offset_tab%ix_offset(k,i)
                n_l = sh_offset_tab%intag(n)
                if (n_l==0) cycle
                sh_offset_tab%norm_n6(1:6,1,n_l) = sh_offset_tab%norm_n6(1:6,1,n_l) + t6(1:6,1)
                sh_offset_tab%norm_n6(1:6,2,n_l) = sh_offset_tab%norm_n6(1:6,2,n_l) + t6(1:6,2)
                sh_offset_tab%norm_n6(1:6,3,n_l) = sh_offset_tab%norm_n6(1:6,3,n_l) + t6(1:6,3)
              end do
            enddo
            if (nspmd>1) then
              lenr = sh_offset_tab%iad_offset(1,nspmd+1)-sh_offset_tab%iad_offset(1,1)
              ndim1 = 3*6
              ndim2 = sh_offset_tab%nnsh_oset
              call spmd_exch_vnpon(ndim1,ndim2,sh_offset_tab%norm_n6,sh_offset_tab%iad_offset,    &
                sh_offset_tab%fr_offset,nspmd,lenr)
            end if
            do n = 1, sh_offset_tab%nnsh_oset
              do k = 1, 6
                sh_offset_tab%norm_n(1,n) = sh_offset_tab%norm_n(1,n)+ sh_offset_tab%norm_n6(k,1,n)
                sh_offset_tab%norm_n(2,n) = sh_offset_tab%norm_n(2,n)+ sh_offset_tab%norm_n6(k,2,n)
                sh_offset_tab%norm_n(3,n) = sh_offset_tab%norm_n(3,n)+ sh_offset_tab%norm_n6(k,3,n)
              end do
            end do
          else
            do i=1,sh_offset_tab%nsh_oset
              do k = 1,4
                n = sh_offset_tab%ix_offset(k,i)
                xv(1:3,k) = xyz(1:3,n)
              end do
              if (sh_offset_tab%ix_offset(4,i)/=sh_offset_tab%ix_offset(3,i)) then
                nnod = 4
                do j=1,3
                  r(j) = xv(j,2)+xv(j,3)-xv(j,1)-xv(j,4)
                  s(j) = xv(j,3)+xv(j,4)-xv(j,1)-xv(j,2)
                end do
              else
                nnod = 3
                do j=1,3
                  r(j) = xv(j,3)-xv(j,2)
                  s(j) = xv(j,1)-xv(j,2)
                end do
              end if
              t(1) = r(2) * s(3) - r(3) * s(2)
              t(2) = r(3) * s(1) - r(1) * s(3)
              t(3) = r(1) * s(2) - r(2) * s(1)
              norm2 = t(1)*t(1)+t(2)*t(2)+t(3)*t(3)
              t(1:3) = t(1:3)/sqrt(max(em20,norm2))
              do k = 1,nnod
                n = sh_offset_tab%ix_offset(k,i)
                n_l = sh_offset_tab%intag(n)
                if (n_l==0) cycle
                sh_offset_tab%norm_n(1:3,n_l) = sh_offset_tab%norm_n(1:3,n_l) + t(1:3)
              end do
            enddo
!
            if (nspmd>1) then
              lenr = sh_offset_tab%iad_offset(1,nspmd+1)-sh_offset_tab%iad_offset(1,1)
              call spmd_exch_n(sh_offset_tab%norm_n,sh_offset_tab%iad_offset,sh_offset_tab%fr_offset,  &
                lenr)
            end if
!
          end if !(iparit >0) then
          do n = 1, sh_offset_tab%nnsh_oset
            norm2 = sh_offset_tab%norm_n(1,n)*sh_offset_tab%norm_n(1,n)+                   &
              sh_offset_tab%norm_n(2,n)*sh_offset_tab%norm_n(2,n)+                   &
              sh_offset_tab%norm_n(3,n)*sh_offset_tab%norm_n(3,n)
            sh_offset_tab%norm_n(1:3,n) = sh_offset_tab%norm_n(1:3,n)/sqrt(max(em20,norm2))
            dx(1:3) = sh_offset_tab%norm_n(1:3,n)*sh_offset_tab%offset_n(n)
            ng = sh_offset_tab%indexg(n)
            xyz(1:3,ng) = xyz(1:3,ng) + dx(1:3)
          end do
!
        end subroutine offset_nproj
!
      end module offset_nproj_mod
