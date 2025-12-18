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
      module ale51_apply_remote_flux_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief
!! \details
        subroutine ale51_apply_remote_flux(n_entity,nv46,trimat,s_flux,s_flux_vois,flux_mat,flux_vois_mat,ale_connect)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod, only : ep20
          use ale_connectivity_mod , only : t_ale_connectivity
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: n_entity !< number of solid (3d) or quad (2d)
          integer, intent(in) :: trimat !< number of sub-materials
          integer, intent(in) :: s_flux !< first dimension of flux array
          integer, intent(in) :: s_flux_vois !< first dimension of flux_vois array
          integer, intent(in) :: nv46 !< number of facets per element (4 for 2d, 6 for 3d)
          real(kind=WP), dimension(s_flux,nv46,trimat), intent(inout) :: flux_mat !< flux to be updated
          real(kind=WP), dimension(s_flux_vois,nv46,trimat), intent(in) :: flux_vois_mat !< flux received from neighbor processors
          type(t_ale_connectivity), intent(in) :: ale_connect
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ii,k,kk,iv
          integer :: my_address,itrimat
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! --------
          !$omp do schedule(guided)
          do i=1,ale_connect%ee_connect%remote_neighb_elem
            ii = ale_connect%ee_connect%index_elem_w_neigh(i) ! get the local element connected to a remote neighbor
            my_address = ale_connect%ee_connect%iad_connect(ii) ! get the starting address in the connectivity array
            ! --------
            ! loop over the facets of the element "i"
            do k=1,nv46 
              iv = ale_connect%ee_connect%connected(my_address+k-1) ! get the id of the neighbor element
              if(iv>n_entity) then ! check if the neighbor element is a remote element
                kk = ale_connect%ee_connect%id_facet_neighbor_elem(nv46*(i-1)+k) ! get the remote facet id
                if(kk/=0) then
                  do itrimat=1,trimat
                    if(flux_vois_mat(iv,kk,itrimat)/=-ep20) then
                      !write(*,*) ispmd_debug,ii,k," VeRiF 1",iv,kk,i,flux_mat(ii,k,itrimat),-flux_vois_mat(iv,kk,itrimat)
                      flux_mat(ii,k,itrimat) = -flux_vois_mat(iv,kk,itrimat) ! apply the flux
                    endif
                  enddo
                endif
              endif
            enddo          
            ! --------            
          enddo
          !$omp end do          
          ! --------          

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine ale51_apply_remote_flux
      end module ale51_apply_remote_flux_mod
