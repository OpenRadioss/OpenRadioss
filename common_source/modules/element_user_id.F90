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
      !||    user_id_mod                ../common_source/modules/element_user_id.F90
      !||--- called by ------------------------------------------------------
      !||    fredec0                    ../starter/source/starter/freform.F
      !||    fredec5                    ../starter/source/starter/freform.F
      !||    fredec_2key_4id            ../starter/source/starter/freform.F
      !||    fredec_2key_4id_t          ../starter/source/starter/freform.F
      !||    fredec_2key_id_or_key_id   ../starter/source/starter/freform.F
      !||    fredec_key_3id_t           ../starter/source/starter/freform.F
      !||    hm_prelce16s               ../starter/source/elements/reader/hm_read_solid.F
      !||    hm_preread_node            ../starter/source/elements/reader/hm_preread_node.F
      !||    hm_read_beam               ../starter/source/elements/reader/hm_read_beam.F
      !||    hm_read_node               ../starter/source/elements/reader/hm_read_node.F
      !||    hm_read_quad               ../starter/source/elements/reader/hm_read_quad.F
      !||    hm_read_rivet              ../starter/source/elements/reader/hm_read_rivet.F
      !||    hm_read_sh3n               ../starter/source/elements/reader/hm_read_sh3n.F
      !||    hm_read_shell              ../starter/source/elements/reader/hm_read_shell.F
      !||    hm_read_sphcel             ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_spring             ../starter/source/elements/reader/hm_read_spring.F
      !||    hm_read_submodel           ../starter/source/model/assembling/hm_read_submodel.F
      !||    hm_read_thgrou             ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_tria               ../starter/source/elements/reader/hm_read_tria.F
      !||    hm_read_truss              ../starter/source/elements/reader/hm_read_truss.F
      !||    hm_read_unit               ../starter/source/general_controls/computation/hm_read_unit.F
      !||    hm_thgrki_vent             ../starter/source/output/th/hm_thgrki_vent.F
      !||    identson3                  ../starter/source/model/remesh/build_admesh.F
      !||    identson4                  ../starter/source/model/remesh/build_admesh.F
      !||    nbadigemesh                ../starter/source/elements/ige3d/nbadigemesh.F
      !||    nbadmesh                   ../starter/source/model/remesh/nbadmesh.F
      !||    python_register            ../engine/source/tools/curve/python_register.F90
      !||====================================================================
      module user_id_mod

        type id_limits_
          integer :: global = 1100000000                !< maximum identifier
          integer :: admesh = 899999999                 !< related to /ADMESH option
          integer :: admesh_ft_node_auto = 1000000000   !< related to /ADMESH option
          integer :: admesh_lt_node_auto = 1000000000   !< related to /ADMESH option
          integer :: th = 1000000000                    !< related to /MONVOL option
          integer :: unit = 1000000000                  !< related to /UNIT option
        end type id_limits_

        type(id_limits_) :: id_limit

      contains
!! \brief Returns the user id and group id of all the elements
      !||====================================================================
      !||    element_user_id   ../common_source/modules/element_user_id.F90
      !||--- called by ------------------------------------------------------
      !||    python_register   ../engine/source/tools/curve/python_register.F90
      !||====================================================================
        subroutine element_user_id(user_id, group_id, local_id, nelem, &
          ixs, nixs, numels, &
          ixc, nixc, numelc, &
          ixp, nixp, numelp, &
          ixt, nixt, numelt, &
          ixq, nixq, numelq, &
          ixtg, nixtg, numeltg, &
          ixr, nixr, numelr, &
          iparg, ngroup, nparg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nelem               !< total number of elements
          integer, intent(inout) :: user_id(nelem)   !< user id of the elements
          integer, intent(inout) :: local_id(nelem)  !< local id of the elements
          integer, intent(inout) :: group_id(nelem)  !< group id of the elements
          integer, intent(in) :: nixs                !< number of integers in the solid data structure
          integer, intent(in) :: numels              !< number of solids
          integer, intent(in) :: ixs(nixs,numels)    !< solid data structure
          integer, intent(in) :: nixc                !< number of integers in the shell data structure
          integer, intent(in) :: numelc              !< number of shells
          integer, intent(in) :: ixc(nixc,numelc)    !< shell data structure
          integer, intent(in) :: nixp                !< number of integers in the beam data structure
          integer, intent(in) :: numelp              !< number of beams
          integer, intent(in) :: ixp(nixp,numelp)    !< beam data structure
          integer, intent(in) :: nixt                !< number of integers in the truss data structure
          integer, intent(in) :: numelt              !< number of trusses
          integer, intent(in) :: ixt(nixt,numelt)    !< truss data structure
          integer, intent(in) :: nixtg               !< number of integers in the triangle data structure
          integer, intent(in) :: numeltg             !< number of triangles
          integer, intent(in) :: ixtg(nixtg,numeltg) !< triangle data structure
          integer, intent(in) :: nixr                !< number of integers in the spring data structure
          integer, intent(in) :: numelr              !< number of springs
          integer, intent(in) :: ixr(nixr,numelr)    !< spring data structure
          integer, intent(in) :: nixq                !< number of integers in the quad data structure
          integer, intent(in) :: numelq              !< number of quads
          integer, intent(in) :: ixq(nixq,numelq)    !< quad data structure
          integer, intent(in) :: ngroup              !< number of groups
          integer, intent(in) :: nparg               !< number of integers in the group data structure
          integer, intent(in) :: iparg(nparg,ngroup) !< group data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ng
          integer :: i
          integer :: nel
          integer :: ity
          integer :: j
          integer :: mlw
          integer :: nft
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          j = 0
          do ng = 1, ngroup
            mlw     = iparg(1,ng)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            ity     = iparg(5,ng)
            if(mlw /= 13) then
              do  i=1,nel
                j = j + 1
                group_id(j) = ng
                local_id(j) = i
                if(ity == 1) then ! solid
                  user_id(j) = ixs(nixs,nft+i)
                elseif(ity == 2) then ! quad (2d)
                  user_id(j) = ixq(nixq,nft+i)
                elseif (ity == 3) then ! shell
                  user_id(j) = ixc(nixc,nft+i)
                elseif (ity == 4) then ! truss
                  user_id(j) = ixt(nixt,nft+i)
                elseif (ity == 5) then ! beam
                  user_id(j) = ixp(nixp,nft+i)
                elseif (ity == 6) then ! spring
                  user_id(j) = ixr(nixr,nft+i)
                elseif (ity == 7) then ! triangle
                  user_id(j) = ixtg(nixtg,nft+i)
                endif
              enddo
            endif
          enddo



        end subroutine



      end module user_id_mod
