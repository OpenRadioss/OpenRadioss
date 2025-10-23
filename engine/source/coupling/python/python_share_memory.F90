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
!||    python_share_memory_mod   ../engine/source/coupling/python/python_share_memory.F90
!||--- called by ------------------------------------------------------
!||    resol                     ../engine/source/engine/resol.F
!||====================================================================
      module python_share_memory_mod
        implicit none
      contains
!||====================================================================
!||    python_share_memory     ../engine/source/coupling/python/python_share_memory.F90
!||--- called by ------------------------------------------------------
!||    resol                   ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    python_expose_doubles   ../common_source/modules/python_mod.F90
!||    python_expose_ints      ../common_source/modules/python_mod.F90
!||--- uses       -----------------------------------------------------
!||    nodal_arrays_mod        ../common_source/modules/nodal_arrays.F90
!||    python_funct_mod        ../common_source/modules/python_mod.F90
!||====================================================================
        subroutine python_share_memory(py, nodes, numnod,&
        & ixs, nixs, numels, &
        & ixc, nixc, numelc, &
        & ixp, nixp, numelp, &
        & ixt, nixt, numelt, &
        & ixq, nixq, numelq, &
        & ixtg, nixtg, numeltg, &
        & ixr, nixr, numelr, &
        & iparg, ngroup, nparg )
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use, intrinsic :: iso_c_binding, only : c_char, c_null_char, c_loc
          use nodal_arrays_mod
          use python_funct_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),intent(inout) :: py          !< the Fortran structure that holds the python function
          integer,      intent(in) :: numnod         !< the global number of nodes
          type(nodal_arrays_), target, intent(in) :: nodes  !< the nodal arrays
          integer, intent(in) :: nixs                !< number of integers in the solid data structure
          integer, intent(in) :: numels              !< number of solids
          integer, target, intent(in) :: ixs(nixs,numels)    !< solid data structure
          integer, intent(in) :: nixc                !< number of integers in the shell data structure
          integer, intent(in) :: numelc              !< number of shells
          integer, target, intent(in) :: ixc(nixc,numelc)    !< shell data structure
          integer, intent(in) :: nixp                !< number of integers in the beam data structure
          integer, intent(in) :: numelp              !< number of beams
          integer, target, intent(in) :: ixp(nixp,numelp)    !< beam data structure
          integer, intent(in) :: nixt                !< number of integers in the truss data structure
          integer, intent(in) :: numelt              !< number of trusses
          integer, target, intent(in) :: ixt(nixt,numelt)    !< truss data structure
          integer, intent(in) :: nixtg               !< number of integers in the triangle data structure
          integer, intent(in) :: numeltg             !< number of triangles
          integer, target, intent(in) :: ixtg(nixtg,numeltg) !< triangle data structure
          integer, intent(in) :: nixr                !< number of integers in the spring data structure
          integer, intent(in) :: numelr              !< number of springs
          integer, target, intent(in) :: ixr(nixr,numelr)    !< spring data structure
          integer, intent(in) :: nixq                !< number of integers in the quad data structure
          integer, intent(in) :: numelq              !< number of quads
          integer, target, intent(in) :: ixq(nixq,numelq)    !< quad data structure
          integer, intent(in) :: ngroup              !< number of groups
          integer, intent(in) :: nparg               !< number of integers in the group data structure
          integer, target, intent(in) :: iparg(nparg,ngroup) !< group data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          py%context = python_create_context()
          if (numnod > 0) then
            if (allocated(nodes%ITAB))    call python_expose_ints(py, "ITAB", 4, c_loc(nodes%ITAB),     size(nodes%ITAB))
            if (allocated(nodes%ITABM1))  call python_expose_ints(py, "ITABM1", 6, c_loc(nodes%ITABM1),   size(nodes%ITABM1))
            if (allocated(nodes%IKINE))   call python_expose_ints(py, "IKINE", 5, c_loc(nodes%IKINE),    size(nodes%IKINE))
            if (allocated(nodes%MAIN_PROC)) then
              call python_expose_ints(py, "MAIN_PROC", 9, c_loc(nodes%MAIN_PROC),size(nodes%MAIN_PROC))
            end if
            if (allocated(nodes%WEIGHT)) then
              call python_expose_ints(py, "WEIGHT", 6, c_loc(nodes%WEIGHT),   size(nodes%WEIGHT))
            end if
            if (allocated(nodes%A))       call python_expose_doubles(py, "A", 1, c_loc(nodes%A),     3*numnod)
            if (allocated(nodes%AR))      call python_expose_doubles(py, "AR", 2, c_loc(nodes%AR),    3*numnod)
            if (allocated(nodes%V))       call python_expose_doubles(py, "V", 1, c_loc(nodes%V),     3*numnod)
            if (allocated(nodes%X))       call python_expose_doubles(py, "X", 1, c_loc(nodes%X),     3*numnod)
            if (allocated(nodes%D))       call python_expose_doubles(py, "D", 1, c_loc(nodes%D),     3*numnod)
            if (allocated(nodes%VR))      call python_expose_doubles(py, "VR", 2, c_loc(nodes%VR),    3*numnod)
            if (allocated(nodes%DR))      call python_expose_doubles(py, "DR", 2, c_loc(nodes%DR),    3*numnod)
            if (allocated(nodes%MS))      call python_expose_doubles(py, "MS", 2, c_loc(nodes%MS),    numnod)
            if (allocated(nodes%IN))      call python_expose_doubles(py, "IN", 2, c_loc(nodes%IN),    size(nodes%IN))
            if (allocated(nodes%STIFN))   call python_expose_doubles(py, "STIFN", 5, c_loc(nodes%STIFN), size(nodes%STIFN))
            if (allocated(nodes%MS0))     call python_expose_doubles(py, "MS0", 3, c_loc(nodes%MS0),   size(nodes%MS0))
            if (allocated(nodes%IN0))     call python_expose_doubles(py, "IN0", 3, c_loc(nodes%IN0),   size(nodes%IN0))
            if (allocated(nodes%MCP))     call python_expose_doubles(py, "MCP", 3, c_loc(nodes%MCP),   size(nodes%MCP))
            if (allocated(nodes%VISCN))   call python_expose_doubles(py, "VISCN", 5, c_loc(nodes%VISCN), size(nodes%VISCN))
            if (allocated(nodes%TEMP))    call python_expose_doubles(py, "TEMP", 4, c_loc(nodes%TEMP),  size(nodes%TEMP))
          end if

          if(numelc > 0)  call python_expose_ints(py, "IXC", 3, c_loc(ixc), numelc*nixc)
          if(numels > 0)  call python_expose_ints(py, "IXS", 3, c_loc(ixs), numels*nixs)
          if(numelp > 0)  call python_expose_ints(py, "IXP", 3, c_loc(ixp), numelp*nixp)
          if(numelt > 0)  call python_expose_ints(py, "IXT", 3, c_loc(ixt), numelt*nixt)
          if(numeltg > 0) call python_expose_ints(py, "IXTG", 4, c_loc(ixtg), numeltg*nixtg)
          if(numelr > 0)  call python_expose_ints(py, "IXR", 3, c_loc(ixr), numelr*nixr)
          if(numelq > 0)  call python_expose_ints(py, "IXQ", 3, c_loc(ixq), numelq*nixq)
          if(ngroup > 0)  call python_expose_ints(py, "IPARG", 5, c_loc(iparg), ngroup*nparg)

        end subroutine python_share_memory

      end module python_share_memory_mod
