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
!||    get_element_group_mod   ../starter/source/elements/get_element_group.F90
!||--- called by ------------------------------------------------------
!||    lectur                  ../starter/source/starter/lectur.F
!||====================================================================
      module get_element_group_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Creation of homogeneous super groups of elements based on the element properties, material and part ids, etc...
!! \details
!||====================================================================
!||    get_element_group          ../starter/source/elements/get_element_group.F90
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    create_element_group       ../starter/source/elements/create_element_group.F90
!||    get_sort_key_shell         ../starter/source/elements/shell/get_sort_key_shell.F90
!||    get_sort_key_shell3n       ../starter/source/elements/sh3n/get_sort_key_shell3n.F90
!||    get_sort_key_solid         ../starter/source/elements/solid/get_sort_key_solid.F90
!||--- uses       -----------------------------------------------------
!||    create_element_group_mod   ../starter/source/elements/create_element_group.F90
!||    elm_group_mod              ../starter/source/modules/elm_group_mod.F90
!||    get_sort_key_shell3n_mod   ../starter/source/elements/sh3n/get_sort_key_shell3n.F90
!||    get_sort_key_shell_mod     ../starter/source/elements/shell/get_sort_key_shell.F90
!||    get_sort_key_solid_mod     ../starter/source/elements/solid/get_sort_key_solid.F90
!||    stack_mod                  ../starter/share/modules1/stack_mod.F
!||====================================================================
        subroutine get_element_group(numels,numelc,numeltg,nb_key, &
                                        nummat,numgeo,npart,npropgi, &
                                        npropmi,npropm,npropg,lipart1, &
                                        isms,idtgrs,npreload,icrack3d, &
                                        sh4tree_dim1,sh4tree_dim2,sh3tree_dim1,sh3tree_dim2, &
                                        ltitr,iwarnhb,trimat,nadmesh,iddlevel, &
                                        iparts,ipartc,iparttg,ipart, &
                                        isoloff,isheoff,ish3noff,isolnod,icnod, &
                                        iflag_bpreload,damp_range_part,tagprt_sms, &
                                        iworksh,ixs,ixc,ixtg, &
                                        igeo,ipm,sh4tree,sh3tree, &
                                        pm,geo,stack,mat_param,elm_group)


! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use element_mod, only : nixs,nixc,nixtg
          use matparam_def_mod , only : matparam_struct_
          use elm_group_mod , only : elm_group_
          use create_element_group_mod, only : create_element_group
          use get_sort_key_solid_mod, only : get_sort_key_solid
          use get_sort_key_shell_mod, only : get_sort_key_shell
          use get_sort_key_shell3n_mod, only : get_sort_key_shell3n
          use stack_mod
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
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: numelc !< number of shell elements
          integer, intent(in) :: numeltg !< number of shell3n elements
          integer, intent(in) :: nb_key !< number of keys for sorting
          integer, intent(in) :: nummat !< total number of materials in the model
          integer, intent(in) :: numgeo !< total number of properties in the model
          integer, intent(in) :: npart !< total number of parts in the model
    
          integer, intent(in) :: npropgi !< number of integer property parameters
          integer, intent(in) :: npropmi !< number of integer material parameters
          integer, intent(in) :: npropm !< number of real material parameters
          integer, intent(in) :: npropg !< number of real property parameters
          integer, intent(in) :: lipart1 !< leading dimension of the ipart array

          integer, intent(in) :: isms !< global flag for sms in the model
          integer, intent(in) :: idtgrs !< flag for using part tags for sms
          integer, intent(in) :: npreload !< number of preload steps in the model
          integer, intent(inout) :: icrack3d !< global flag for 3D cracking

          integer, intent(in) :: sh4tree_dim1 !< first dimension of the sh4tree array for shells (adaptive meshing)
          integer, intent(in) :: sh4tree_dim2 !< second dimension of the sh4tree array for shells (adaptive meshing)
          integer, intent(in) :: sh3tree_dim1 !< first dimension of the sh3tree array for shell3ns (adaptive meshing)
          integer, intent(in) :: sh3tree_dim2 !< second dimension of the sh3tree array for shell3ns (adaptive meshing)

          integer, intent(in) :: ltitr !< title size ???
          integer, intent(inout) :: iwarnhb !< number of Warning mesages
          integer, intent(inout) :: trimat !< number of sub-materials for the law 151
          integer, intent(in) :: nadmesh !< flag for adaptive meshing 

          integer, intent(in) :: iddlevel !< flag for the domain decomposition: 0 --> first domain decomposition without the interfaces
                                          !<                                    1 --> second domain decomposition with the interfaces
                                          !< if there are some interfaces, need to save the data to the appropiate locations (i instead of index(i))

          integer, dimension(numels), intent(in) :: iparts !< array of part ids for each element
          integer, dimension(numelc), intent(in) :: ipartc !< array of part ids for each element
          integer, dimension(numeltg), intent(in) :: iparttg !< array of part ids for each element      

          integer, dimension(numels), intent(in) :: isoloff  !< deactivated solid flag
          integer, dimension(numelc), intent(in) :: isheoff  !< deactivated shell flag
          integer, dimension(numeltg), intent(in) :: ish3noff  !< deactivated shell3n flag

          integer, dimension(numels), intent(in) :: isolnod  !< array of element types (hexa8, hexa20, etc...)
          integer, dimension(numeltg), intent(in) :: icnod !< ???
          integer, dimension(numels), intent(in) :: iflag_bpreload !< array of flags for elements with preload conditions

          integer, dimension(npart), intent(in) :: damp_range_part !< array of damping range for each part
          integer, dimension(isms*npart), intent(in) :: tagprt_sms !< array of sms tags for each element's part


    

          integer, dimension(lipart1,npart), intent(in) :: ipart !< array of part parameters
          integer, dimension(3,numelc+numeltg), intent(in) :: iworksh !< ply array for shell element              
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid connectivity array
          integer, dimension(3,numelc), intent(in) :: ixc !< shell connectivity array
          integer, dimension(nixtg,numeltg), intent(in) :: ixtg !< shell3n connectivity array          
          integer, dimension(npropgi,numgeo), intent(inout) :: igeo !< property parameters array
          integer, dimension(npropmi,nummat), intent(in) :: ipm          
          
          integer, dimension(sh4tree_dim1,sh4tree_dim2), intent(in) :: sh4tree !< tree array for shells (adaptive meshing)          
          integer, dimension(sh3tree_dim1,sh3tree_dim2), intent(in) :: sh3tree !< tree array for shell3ns (adaptive meshing)

          real(kind=WP), dimension(npropg,numgeo), intent(in) :: geo !< property parameters array
          real(kind=WP), dimension(npropm,nummat), intent(in) :: pm !< material parameters array

          type(stack_ply), intent(in) :: stack !< stack data structure for multilayer shells
          type(matparam_struct_), dimension(nummat), intent(in) :: mat_param !< array of material parameters structures
          type(elm_group_), intent(out) :: elm_group !< element group data structure to store the group id and the number of element in the group for each element  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: numels8a
          integer, dimension(:,:), allocatable :: sort_key ! array to store the sorting keys for each element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          if(allocated(elm_group%solid)) deallocate(elm_group%solid)
          allocate(elm_group%solid(numels,2))
          elm_group%solid(:,:) = 0
          numels8a = 0
          if(numels>0) then
            allocate(sort_key(nb_key,numels))
            sort_key(1:nb_key,1:numels) = 0

            call get_sort_key_solid(numels,nb_key,nummat,numgeo, &
                                    npart,npropgi, &
                                    npropmi,npropm,npropg, &
                                    isms,idtgrs,npreload,trimat,numels8a, &
                                    iparts,isoloff,isolnod,iflag_bpreload,damp_range_part,tagprt_sms, &
                                    sort_key,ixs,igeo, &
                                    ipm,pm,geo, &
                                    mat_param)

            call create_element_group(numels,nb_key,elm_group%solid,sort_key,iddlevel)

            deallocate(sort_key)
          end if

          if(allocated(elm_group%shell)) deallocate(elm_group%shell)
          allocate(elm_group%shell(numelc,2))
          elm_group%shell(:,:) = 0
          if(numelc>0) then
            allocate(sort_key(nb_key,numelc))
            sort_key(1:nb_key,1:numelc) = 0

            call get_sort_key_shell(numelc,numeltg,nb_key,nummat,numgeo, &
                                      npart,nadmesh,lipart1,npropgi, &
                                      npropmi,npropm,npropg,icrack3d, &
                                      isms,idtgrs,sh4tree_dim1,sh4tree_dim2, &
                                      ipartc,isheoff,damp_range_part,tagprt_sms, &
                                      ipart,iworksh,sort_key,ixc,igeo, &
                                      ipm,sh4tree,pm,geo, &
                                      stack,mat_param)

            call create_element_group(numelc,nb_key,elm_group%shell,sort_key,iddlevel)

            deallocate(sort_key)
          end if

          if(allocated(elm_group%shell3n)) deallocate(elm_group%shell3n)
          allocate(elm_group%shell3n(numeltg,2))

          elm_group%shell3n(:,:) = 0
          if(numeltg>0) then
            allocate(sort_key(nb_key,numeltg))
            sort_key(1:nb_key,1:numeltg) = 0

            call get_sort_key_shell3n(numelc,numeltg,nb_key,nummat,numgeo, &
                                      npart,nadmesh,lipart1,npropgi, &
                                      npropmi,npropm,npropg,icrack3d, &
                                      isms,idtgrs,sh3tree_dim1,sh3tree_dim2,ltitr,trimat,iwarnhb, &
                                      iparttg,ish3noff,damp_range_part,icnod,tagprt_sms, &
                                      ipart,iworksh,sort_key,ixtg,igeo, &
                                      ipm,sh3tree,pm,geo, &
                                      stack,mat_param)

            call create_element_group(numeltg,nb_key,elm_group%shell3n,sort_key,iddlevel)

            deallocate(sort_key)
          end if                 
          


          return

        end subroutine get_element_group
      end module get_element_group_mod
