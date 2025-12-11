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
!||    aflut_int22_mod   ../engine/source/ale/ale51/afluxt_int22.F90
!||--- called by ------------------------------------------------------
!||    alethe            ../engine/source/ale/alethe.F
!||====================================================================
      module afluxt_int22_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief 
!! \details 
!||====================================================================
!||    aflut_int22             ../engine/source/ale/ale51/afluxt_int22.F90
!||--- called by ------------------------------------------------------
!||    alethe                  ../engine/source/ale/alethe.F
!||--- calls      -----------------------------------------------------
!||    ale51_antidiff3_int22   ../engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
!||    ale51_upwind3_int22     ../engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
!||    my_barrier              ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    elbufdef_mod            ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod             ../common_source/modules/elements/element_mod.F90
!||    i22bufbric_mod          ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22tri_mod              ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    multimat_param_mod      ../common_source/modules/multimat_param_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine afluxt_int22(itask,nthread,numels,nparg,ngroup,s_flux,s_alpha_mat,nv46, &
                               itrimat,trimat,npropm,nummat,iparg,ixs,pm,alpha_mat,flux_mat,elbuf_tab, &
                               nb_int22,int22,i22len,brick_list)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod , only : WP
          use element_mod , only : nixs
          use constant_mod         
          use i22tri_mod , only : brick_entity
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
          integer, intent(in) :: s_flux !< first dimension of flux array      
          integer, intent(in) :: s_alpha_mat !< first dimension of alpha array
          integer, intent(in) :: nv46 !< second dimension of flux & qmv array
          integer, intent(in) :: itrimat !< current sub-material id
          integer, intent(in) :: trimat !< number of materials
          integer, intent(in) :: npropm !< first dimension of pm array
          integer, intent(in) :: nummat !< total number of material
          integer, intent(in) :: nb_int22 !< number of candidates for the interface 22
          integer, intent(in) :: int22 !< number of /TYPE22 interfaces
          integer, intent(in) :: i22len !< buffer size for intersected bricks          
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< element group data
          integer, dimension(nixs,numels), intent(in) :: ixs !< element to node connectivity array
          real(kind=WP), dimension(npropm,nummat), intent(inout) :: pm !< flux array
          real(kind=WP), dimension(s_alpha_mat,trimat), intent(inout) :: alpha_mat !< alpha array      
          real(kind=WP), dimension(s_flux,nv46,trimat), intent(inout) :: flux_mat !< flux array
          type(elbuf_struct_), dimension(ngroup), target :: elbuf_tab !< element buffer
          type(brick_entity), dimension(int22,i22len), intent(inout) :: brick_list !< interface 22 data structure          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nin,ie,icell,j,ie_m,ibm,j1,j2,ibv,ng,ib
          integer :: nbf,nbl,tnb,idloc,mtn,llt_,k0,k1,ipos
          integer :: mcell,ncell
          real(kind=WP) :: vfrac
          type(buf_mat_), pointer :: mbuf
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      !idem for cut cells (inter22) (obsolete)
      nin = 1
      nbf = 1+itask*nb_int22/nthread
      nbl = (itask+1)*nb_int22/nthread
      nbl = min(nbl,nb_int22)
      tnb = nbl-nbf+1
      do ib=nbf,nbl
        ncell =  brick_list(nin,ib)%nbcut 
        mcell =  brick_list(nin,ib)%mainid       
        icell =  0  
        ie    =  brick_list(nin,ib)%id
        do while (icell <= ncell) ! loop on polyhedron {1:ncell} u {9}
          icell = icell +1
          if (icell>ncell .and. ncell /= 0)icell=9 
          !get_main_data
          j     = brick_list(nin,ib)%poly(icell)%whereismain(1)
          if(j==0)then
            ie_m = ie
            ibm  = ib
          elseif(j <= nv46)then
            ie_m = brick_list(nin,ib)%adjacent_brick(j,1)
            ibm  = brick_list(nin,ib)%adjacent_brick(j,4)
          else
            j1   = j/10
            j2   = mod(j,10)
            ibv  = brick_list(nin,ib )%adjacent_brick(j1,4)
            ibm  = brick_list(nin,ibv)%adjacent_brick(j2,4)
            ie_m = brick_list(nin,ibv)%adjacent_brick(j2,1)
          endif
          ng    = brick_list(nin,ibm)%ng
          idloc = brick_list(nin,ibm)%idloc
          mtn = iparg(1,ng)
          if(mtn == 51)then
            mbuf  => elbuf_tab(ng)%bufly(1)%mat(1,1,1)           
            llt_  = iparg(2,ng)
            !===restore direct fluxes====!
            brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_upwflux_mat(1,itrimat)= &
                        brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_flux(1)   
            brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_upwflux_mat(2,itrimat)= &
                        brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_flux(2)   
            brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_upwflux_mat(3,itrimat)= &
                        brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_flux(3)   
            brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_upwflux_mat(4,itrimat)= &
                        brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_flux(4)   
            brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_upwflux_mat(5,itrimat)= &
                        brick_list(nin,ib)%poly(icell)%face(1:6)%adjacent_flux(5)                          
            !===get vfrac================!
            ipos = 1                                                                                               
            k0 = ((m51_n0phas + (itrimat-1)*m51_nvphas )+ipos-1)             ! example : ipos=1 => vfrac  {uvar(i,add)=uvar(k+i)}  
            k1 = k0 * llt_                                                                                       
            vfrac = mbuf%var(k1+idloc)                                                                              
            vfrac = max(zero,min(one,vfrac))                                                                         
            brick_list(nin,ib)%poly(icell)%vfracm(itrimat)= vfrac
          endif
        enddo!next icell
      enddo!next ib

      call my_barrier

      ! --------------------------------
      ! submatrial volume fluxes update : law51
      ! --------------------------------
      call ale51_antidiff3_int22(flux_mat(:,:,itrimat),itrimat,ixs, &
                                 nv46,elbuf_tab,itask,alpha_mat(:,itrimat),s_flux,nb_int22,int22,i22len,brick_list)

      call my_barrier

      ! --------------------------------
      ! updating volume fluxes & upwind
      ! --------------------------------        
      call ale51_upwind3_int22(pm,ixs,itrimat,1,iparg,elbuf_tab,itask,nb_int22,int22,i22len,brick_list)

      call my_barrier
      return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine afluxt_int22
      end module afluxt_int22_mod
