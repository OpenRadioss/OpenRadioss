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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief This subroutine is setting nodal sound speed requested by Engine keyword /ANIM/NODA/SSP & /H3D/NODA/SSP
!! \details nodal value for fvmbag centroids is only available with H3D (automatic centroid generation)
      subroutine anim_nodal_ssp_elems( wa4, swa4, iparg, elbuf_tab, ix, nix, numel, is_written_node, &
                 multi_fvm, nparg, ngroup, numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------      
      use initbuf_mod
      use elbufdef_mod
      use multi_fvm_mod , only : multi_fvm_struct
      use constant_mod , only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nparg, ngroup, numnod,swa4                          !< size for array definitions
      integer, intent(in) :: iparg(nparg,ngroup)                                !< data buffer for group of elems
      integer,intent(in) :: ix(nix,numel),nix,numel                             !< data buffer for elem definition
      integer, intent(inout) :: is_written_node(numnod)                         !< tag specific to H3D output
      real,intent(inout) :: wa4(swa4)                                           !< working array
      type (elbuf_struct_),intent(in), dimension(ngroup), target :: elbuf_tab   !< data structure for elem buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer :: ng,i,j,jj, mlw, nnod, nel, nft, ityp
      type(g_bufel_)  ,pointer :: gbuf
      type(l_bufel_)  ,pointer :: lbuf
      my_real, allocatable, dimension(:) :: sum_weight
      my_real :: weight, ssp
      type (multi_fvm_struct), intent(in) :: multi_fvm
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
         allocate(sum_weight(numnod))
         sum_weight = 0
         nnod = nix-3   !8-node brick or 4-node quad

         !---------------------------------------------------------!         
         !         EXPAND SSP PRESSURE TO NODES                    !
         !---------------------------------------------------------!                                
           do ng = 1, ngroup
             mlw   =iparg(1,ng)
             nel   =iparg(2,ng)
             nft   =iparg(3,ng)
             ityp  =iparg(5,ng)
             if(ityp/=1 .and. ityp/=2)cycle
             gbuf => elbuf_tab(ng)%gbuf

             if(mlw /=151)then
               lbuf => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
               if(elbuf_tab(ng)%bufly(1)%l_ssp > 0)then    !this may not be allocated (example : /mat/void)
                do i=1,nel
                  ssp = lbuf%ssp(i)
                  weight = gbuf%vol(i)
                  do j=2,nnod+1
                    jj=ix(j,nft+i)
                    is_written_node(jj)=1
                    wa4(jj)=wa4(jj)+weight*ssp
                    sum_weight(jj) = sum_weight(jj) + weight !cumulated volume
                  enddo
                enddo!next i
               end if
             else
               do i=1,nel
                 ssp = multi_fvm%sound_speed(i+nft)
                 weight = gbuf%vol(i)
                 do j=2,nnod+1
                   jj=ix(j,nft+i)
                   is_written_node(jj)=1
                   wa4(jj)=wa4(jj)+weight*ssp
                   sum_weight(jj) = sum_weight(jj) + weight !cumulated volume
                 enddo
               enddo!next i
             endif
           enddo
         
!-----------------------------------------------
         !divinding by sum of weights to get finally weighting factors
         do i=1,numnod
           if(sum_weight(i)/=zero)then
             wa4(i)=wa4(i)/sum_weight(i)
           endif
         enddo

         deallocate(sum_weight)
!-----------------------------------------------
      
      return
      end subroutine anim_nodal_ssp_elems
