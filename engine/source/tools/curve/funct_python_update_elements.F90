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
      !||====================================================================
      !||    funct_python_update_elements_mod   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||--- called by ------------------------------------------------------
      !||    resol                              ../engine/source/engine/resol.F
      !||====================================================================
      module funct_python_update_elements_mod
      contains
!! \brief initialize the python elemental variables found in the python function
      !||====================================================================
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||--- called by ------------------------------------------------------
      !||    resol                          ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    h3d_quad_scalar_1              ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    h3d_shell_scalar_1             ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_solid_scalar_1             ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    python_element_sync            ../engine/source/mpi/python_spmd_mod.F90
      !||    schlieren_buffer_gathering     ../engine/source/output/anim/generate/schlieren_buffer_gathering.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod           ../common_source/modules/ale/ale_connectivity_mod.F
      !||    aleanim_mod                    ../engine/share/modules/aleanim_mod.F
      !||    elbufdef_mod                   ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    h3d_quad_scalar_1_mod          ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    matparam_def_mod               ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    names_and_titles_mod           ../common_source/modules/names_and_titles_mod.F
      !||    python_funct_mod               ../common_source/modules/python_mod.F90
      !||    python_spmd_mod                ../engine/source/mpi/python_spmd_mod.F90
      !||    stack_mod                      ../engine/share/modules/stack_mod.F
      !||====================================================================
        subroutine funct_python_update_elements(py, ispmd, &
        &   n2d,  ngroup,  nixc, nixtg, nixs, nixq, &
        &   numgeo, numelc, numeltg, numels,numelq, nummat, numnod, &
        &   nparg, npropg, npropm, npropmi, npropgi, &
        &   snercvois, snesdvois, slercvois, slesdvois, &
        &   sthke, seani, npart,&
        &   elbuf_tab   ,iparg       ,geo        , &
        &   ixc         ,ixtg,     ixs, ixq, pm          ,bufmat     , &
        &   ehour       , &
        &   ipm         ,igeo      ,thke      ,err_thk_sh4 ,err_thk_sh3, &
        &   x         ,v         ,w           ,ale_connect, &
        &   nercvois  ,nesdvois  ,lercvois    ,lesdvois, &
        &   n0phas, nvphas,stack       ,          &
        &   ipartc, iparts, iparttg, ipartq, &
        &   d           , multi_fvm , &
        &   mat_param, fani_cell    ,itherm)

! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use python_funct_mod, only : python_
          use python_spmd_mod, only : python_element_sync
          use elbufdef_mod, only : ELBUF_STRUCT_
          use stack_mod, only : STACK_PLY
          use multi_fvm_mod, only : MULTI_FVM_STRUCT
          use ale_connectivity_mod, only : T_ALE_CONNECTIVITY
          use names_and_titles_mod, only: ncharline100
          use matparam_def_mod, only: MATPARAM_STRUCT_
          use aleanim_mod  , only : fani_cell_
          use h3d_quad_scalar_1_mod, only : h3d_quad_scalar_1

! ----------------------------------------------------------------------------------------------------------------------
!                                                     implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     include
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),intent(inout) :: py !< the Fortran structure that holds the python functions
          integer, intent(in) :: ispmd !< current MPI rank
          integer, intent(in) :: n2d !< is 2d model
          integer, intent(in) :: ngroup !< number of element groups
          integer, intent(in) :: nixc !< shell connectivity size
          integer, intent(in) :: nixtg !< triangle connectivity size
          integer, intent(in) :: nixs !< solid connectivity size
          integer, intent(in) :: nixq !< quad connectivity size
          integer, intent(in) :: numgeo !< size of geo array
          integer, intent(in) :: numelc !< number of shell elements
          integer, intent(in) :: numeltg !< number of triangle elements
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: numelq !< number of quad elements
          integer, intent(in) :: nummat !< number of materials
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: nparg !< size of iparg array
          integer, intent(in) :: npropg !< size of geo array
          integer, intent(in) :: npropm !< size of pm array
          integer, intent(in) :: npropmi !< size of ipm array
          integer, intent(in) :: npropgi !< size of igeo array
          integer, intent(in) :: snercvois !< size of nercvois array
          integer, intent(in) :: snesdvois !< size of nesdvois array
          integer, intent(in) :: slercvois !< size of lercvois array
          integer, intent(in) :: slesdvois !< size of lesdvois array
          integer, intent(in) :: sthke !< size of thke array
          integer, intent(in) :: seani !< size of ehour array
          integer, intent(in) :: npart !< number of partitions
          integer, intent(in) :: n0phas !< law 51 phases
          integer, intent(in) :: nvphas !< law 51 phases
          integer, intent(in) :: itherm
          my_real, intent(inout) :: x(3,numnod) !< node coordinates
          my_real, intent(inout) :: v(3,numnod) !< node velocities
          my_real, intent(inout) :: w(3,numnod) !< node momenta?
          my_real, intent(inout) :: d(3,numnod) !< node displacements
          my_real, intent(inout) :: thke(sthke) !< thickness of shell elements ?
          my_real, intent(inout) :: ehour(seani) !< working array ?
          my_real, intent(inout) :: geo(npropg,numgeo) !< property array
          my_real, intent(inout) :: pm(npropm,nummat) !< property array
          my_real, intent(inout) :: err_thk_sh4(numelc) !< ?
          my_real, intent(inout) :: err_thk_sh3(numeltg) !< ?
          integer, intent(inout) :: iparg(nparg,ngroup) !< element group properties
          integer, intent(inout) :: ixc(nixc,numelc) !< shell connectivity array
          integer, intent(inout) :: ixtg(nixtg,numeltg) !< triangle connectivity array
          integer, intent(inout) :: ixs(nixs,numels) !< solid connectivity array
          integer, intent(inout) :: ixq(nixq,numelq) !< quad connectivity array
          integer, intent(inout) :: ipm(npropmi,nummat) !< property array
          integer, intent(inout) :: igeo(npropgi,numgeo) !< property array
          integer, intent(inout) :: ipartc(numelc) !< part id of shell elements
          integer, intent(inout) :: iparts(numels) !< part id of solid elements
          integer, intent(inout) :: iparttg(numeltg) !< part id of triangle elements
          integer, intent(inout) :: ipartq(numelq) !< part id of quad elements
          integer, intent(inout) :: nercvois(snercvois) !< for Schlieren option
          integer, intent(inout) :: nesdvois(snesdvois) !< for Schlieren option
          integer, intent(inout) :: lercvois(slercvois) !< for Schlieren option
          integer, intent(inout) :: lesdvois(slesdvois) !< for Schlieren option
          type(ELBUF_STRUCT_), dimension(ngroup), intent(inout), target :: elbuf_tab !< element buffer structure
          type(STACK_PLY), intent(inout) :: stack !< tack structure
          type(MULTI_FVM_STRUCT), intent(in) :: multi_fvm !< finite volume structure
          my_real, intent(in) :: bufmat(*) !< buffer material ?
          type(T_ALE_CONNECTIVITY), intent(in) :: ale_connect !< ALE connectivity structure
          type(MATPARAM_STRUCT_), dimension(nummat), intent(in) :: mat_param !< material parameters
          type(FANI_CELL_), intent(in) :: fani_cell !< ALE cell animation structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer ii,j,ng,i
          my_real, dimension(:), allocatable :: scalar_results
          integer, dimension(:), allocatable :: id_elem
          integer, dimension(:), allocatable :: ity_elem
          integer, dimension(:), allocatable :: is_written
          integer, dimension(:), allocatable :: h3d_part
          integer, dimension(:), allocatable :: mds_matid
          integer :: idmds
          integer :: imdsvar
          integer :: ipt_input
          integer :: ply_input
          integer :: layer_input
          integer :: iuvar_input
          integer :: id
          integer :: mode
          integer :: ity
          character(len=ncharline100) :: keyword
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(py%elements%global%n > 0) then
            !-----------------------------------------------------------!
            ! stubs for h3d keywords not available from Python functions
            !-----------------------------------------------------------!
            idmds = 0
            imdsvar = 0
            allocate(mds_matid(0))
            ipt_input = -2
            layer_input = -2
            ply_input = -2
            iuvar_input = -2
            id = -1
            mode = -1
            allocate(h3d_part(npart))
            allocate(scalar_results(MVSIZ))
            allocate(id_elem(MVSIZ))
            allocate(ity_elem(MVSIZ))
            allocate(is_written(MVSIZ))
            is_written(1:MVSIZ) = 0
            !-------------------------------------------------------!
            !     LOOP OVER ELEM GROUPS & OUTPUT DATA               !
            !-------------------------------------------------------!
            scalar_results(1:MVSIZ) = -HUGE(scalar_results(1))
            do ii = 1, py%elements%local%n
              ng = py%elements%local%group_id(ii)
              j = py%elements%local%local_id(ii)
              ity = iparg(5,ng)
              do i = 1,ncharline100
                keyword(i:i) = py%elements%local%keyword(ii)%h3d(i:i)
                if(ichar(keyword(i:i)) == 0) keyword(i:i) = ' '
              enddo
              if(ity == 3 .or. ity == 7 ) then
                ! shell or triangle
                !-------------------------------------------------------!
                !     SCHLIEREN INITIALIZATION (IF DEFINED)             !
                !       DENSITY FOR ALL TRIA ARE STORED IN WA_L         !
                !-------------------------------------------------------!
                ! /TRIA are 2d solid elements (new entity type derived from SH3N buffer, it is currently managed from h3d_shell_* subroutines. It will change in the future.
                if(keyword == 'SCHLIEREN' .and. n2d > 0)then
                  call schlieren_buffer_gathering(nercvois ,nesdvois ,lercvois ,lesdvois, iparg, elbuf_tab, multi_fvm,itherm)
                endif

                call h3d_shell_scalar_1(.true.,                                                     &
                &       elbuf_tab   ,scalar_results,iparg       ,geo        ,                &
                &       ixc       ,ixtg      ,pm          ,bufmat     ,                    &
                &       ehour       ,                                                      &
                &       ipm         ,igeo      ,thke      ,err_thk_sh4 ,err_thk_sh3,       &
                &       x         ,v         ,w           ,ale_connect      ,              &
                &       stack       ,id_elem   ,ity_elem  ,                                &
                &       is_written,ipartc,iparttg   ,layer_input ,ipt_input  ,       &
                &       ply_input   ,iuvar_input,h3d_part  ,keyword    ,                   &
                &       d           ,ng         ,multi_fvm,idmds       ,imdsvar    ,       &
                &       mds_matid   ,id         ,mode     ,mat_param   )
              elseif (ity == 1) then ! solid
                call h3d_solid_scalar_1(.true.,                                              &
                &         elbuf_tab       ,scalar_results ,iparg       ,                      &
                &         ixs          ,pm          ,bufmat      ,                            &
                &         ehour        ,                                                      &
                &         ipm             ,                                                   &
                &         x            ,v         ,w           ,ale_connect,                  &
                &         id_elem      ,ity_elem  ,iparts      ,layer_input ,                 &
                &         -1 ,-1 ,-1,iuvar_input ,h3d_part   , &
                &         is_written,0 ,keyword   ,fani_cell   ,             &
                &         multi_fvm       , ng        ,idmds       ,imdsvar     ,             &
                &         id           ,mat_param ,mode     )

              elseif (ity == 2) then ! quad
                call h3d_quad_scalar_1(.true.,ng,                                               &
                &       n0phas        ,nvphas        ,ngroup, n2d, numelq, nummat, numnod, nparg, npropm, npropmi, ispmd,&
                &       elbuf_tab     ,scalar_results, MVSIZ, iparg       ,                             &
                &       ixq           ,NIXQ          ,pm          ,                                           &
                &       ehour         ,                                                          &
                &       ipm           ,                                                        &
                &       x             ,v             ,w           ,ale_connect      ,                &
                &       id_elem       ,                                                          &
                &       is_written    ,ipartq        ,layer_input , npart,                               &
                &       iuvar_input   ,h3d_part      ,keyword     ,                                   &
                &       bufmat        ,multi_fvm     ,                                              &
                &       id            ,mat_param)

              endif
              py%elements%local%values(ii) = scalar_results(j)
              !write(6,*) "keyword",keyword(1:10),"value",scalar_results(j)

            enddo

            deallocate(scalar_results)
            deallocate(id_elem)
            deallocate(ity_elem)
            deallocate(is_written)

            call python_element_sync(py%elements)



            deallocate(h3d_part)
          endif
          return
        end subroutine
      end module



