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
!||    init_ale_mod   ../engine/source/ale/init_ale.F90
!||--- called by ------------------------------------------------------
!||    resol          ../engine/source/engine/resol.F
!||====================================================================
      module init_ale_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initialization of some ALE data
!! \details
!||====================================================================
!||    init_ale                          ../engine/source/ale/init_ale.F90
!||--- called by ------------------------------------------------------
!||    resol                             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    init_ale_boundary_condition       ../engine/source/ale/init_ale_boundary_condition.F90
!||    init_ale_spmd                     ../engine/source/ale/init_ale_spmd.F90
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod              ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                           ../common_source/modules/ale/ale_mod.F
!||    element_mod                       ../common_source/modules/elements/element_mod.F90
!||    init_ale_boundary_condition_mod   ../engine/source/ale/init_ale_boundary_condition.F90
!||    init_ale_spmd_mod                 ../engine/source/ale/init_ale_spmd.F90
!||====================================================================
        subroutine init_ale(global_active_ale_element,n2d,numels,numelq,numeltg,nmult, &
                            iale,ieuler,trimat,itherm,numnod, &
                            nspmd,nsvois,nqvois,ntgvois,nparg,ngroup,s_lesdvois,s_lercvois, &
                            nesdvois,nercvois,lesdvois,lercvois,itab, &
                            itabm1,ixs,ixq,iparg,ale,ale_connect,elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ale_mod , only : ale_
          use ale_connectivity_mod , only : t_ale_connectivity
          use element_mod , only : nixs, nixq
          use init_ale_spmd_mod , only : init_ale_spmd
          use init_ale_boundary_condition_mod , only : init_ale_boundary_condition
          use init_ale_arezon_spmd_mod , only : init_ale_arezon_spmd
          use elbufdef_mod
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
          logical, intent(in) :: global_active_ale_element !< global flag for ALE element activation
          integer, intent(in) :: n2d !< 0: 3D, 1: 2D
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: numelq !< number of quad elements
          integer, intent(in) :: numeltg !< number of triangle elements
          integer, intent(in) :: nmult !< number of ALE materials (2d case)
          integer, intent(in) :: iale !< ALE activated flag
          integer, intent(in) :: ieuler !< Eulerian activated flag
          integer, intent(in) :: trimat !< number of sub-materials
          integer, intent(in) :: itherm !< thermal activated flag
          integer, intent(in) :: numnod !< Number of nodes
          integer, intent(in) :: nspmd !< Number of processors
          integer, intent(in) :: nsvois !< number of frontier solid elements
          integer, intent(in) :: nqvois !< number of frontier quad elements
          integer, intent(in) :: ntgvois !< number of frontier triangle elements
          integer, intent(in) :: nparg !< first dimension of iparg array
          integer, intent(in) :: ngroup !< number of element group
          integer, intent(in) :: s_lesdvois !< size of lesdvois array
          integer, intent(in) :: s_lercvois !< size of lercvois array  
          integer, dimension(nspmd+1), intent(in) :: nesdvois !< number of frontier elements (send)          
          integer, dimension(nspmd+1), intent(in) :: nercvois !< number of frontier elements (rcv)
          integer, dimension(s_lesdvois), intent(in) :: lesdvois !< frontier element ids (send)
          integer, dimension(s_lercvois), intent(in) :: lercvois !< frontier element ids (rcv)
          integer, dimension(numnod), intent(in) :: itab !< local node ID to global node ID mapping
          integer, dimension(2*numnod), intent(in) :: itabm1 !< local node ID to user node ID mapping
          integer, dimension(nixs,numels), intent(in) :: ixs !< Solid element connectivity
          integer, dimension(nixq,numelq), intent(in) :: ixq !< Quad element connectivity
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< group element data    
          type(ale_), intent(inout) :: ale !< ALE data structure                  
          type(t_ale_connectivity), intent(inout) :: ale_connect !< ALE data structure for connectivity
          type(elbuf_struct_), dimension(ngroup), intent(in) :: elbuf_tab !< element buffer structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------        
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if(n2d==0) then
            ale%global%nv46 = 6
          else
            ale%global%nv46 = 4
          end if
          ale%global%s_flux = 0
          ale%global%s_qmv = 0
          if(iale+ieuler+itherm/=0.and.global_active_ale_element) then
            if(n2d==0) then
              ale%global%s_flux = numels+numelq
            else
              ale%global%s_flux = max(1,nmult)*numelq
            end if

            ale%global%s_qmv = 1
            if(trimat>0) ale%global%s_qmv = min(1,trimat)*(numels+numelq)

            if(numels+numelq>0) then
              call init_ale_spmd(ale%global%nv46,n2d,numels,numelq,numnod, &
                                 nspmd,nsvois,nqvois,s_lesdvois,s_lercvois,nesdvois,nercvois, &
                                 lesdvois,lercvois,itab,itabm1,ixs,ixq,ale_connect )
            end if

            call init_ale_boundary_condition(ale%global%nv46,nparg,ngroup,iparg,ale_connect)
            call init_ale_arezon_spmd(n2d,numels,numelq,numeltg,nsvois,nqvois,ntgvois,trimat,nmult,ngroup,nparg, &
                                      nspmd,iparg,elbuf_tab)
          end if


          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_ale
      end module init_ale_mod
