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
      module ale51_finish_int22_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief 
!! \details 
        subroutine ale51_finish_int22(itask,nthread,numels,nparg,ngroup, &
                               npropm,nummat,iparg,ixs,pm,elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod , only : WP
          use element_mod , only : nixs
          use constant_mod         
          use i22tri_mod
          use i22bufbric_mod
          use elbufdef_mod
          use multimat_param_mod , only : m51_n0phas,m51_nvphas
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
          integer, intent(in) :: itask !< omp task id
          integer, intent(in) :: nthread !< number of omp threads
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: nparg !< first dimension of iparg array
          integer, intent(in) :: ngroup !< number of element group
          integer, intent(in) :: npropm !< first dimension of pm array
          integer, intent(in) :: nummat !< total number of material
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< element group data
          integer, dimension(nixs,numels), intent(in) :: ixs !< element to node connectivity array
          real(kind=WP), dimension(npropm,nummat), intent(inout) :: pm !< flux array
          type(elbuf_struct_), dimension(ngroup), target :: elbuf_tab !< element buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nin,j,ib
          integer :: nbf,nbl
          integer :: mlw
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        !Restore Volume Fluxes
        nin = 1
        nbf = 1+itask*nb/nthread
        nbl = (itask+1)*nb/nthread
        nbl = MIN(nbl,nb)
        do ib=nbf,nbl
          mlw = brick_list(nin,ib)%mlw 
          if(mlw /= 51)cycle
          do j=1,6
            brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_UpwFLUX(1) = brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_FLUX(1)
            brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_UpwFLUX(2) = brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_FLUX(2)
            brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_UpwFLUX(3) = brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_FLUX(3)          
            brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_UpwFLUX(4) = brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_FLUX(4)
            brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_UpwFLUX(5) = brick_list(nin,ib)%POLY(1:9)%FACE(j)%Adjacent_FLUX(5)
          enddo !next J
        enddo 

      ! --------------------------------
      ! restore fluxes
      ! --------------------------------        
      call ale51_upwind3_int22(pm,ixs,0,0,iparg,elbuf_tab,itask )      

      call my_barrier
      return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine ale51_finish_int22
      end module ale51_finish_int22_mod
