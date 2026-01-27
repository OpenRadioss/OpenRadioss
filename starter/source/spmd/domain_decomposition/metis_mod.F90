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
!||    metis_mod       ../starter/source/spmd/domain_decomposition/metis_mod.F90
!||--- called by ------------------------------------------------------
!||    dometis         ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    spdometis       ../starter/source/spmd/domain_decomposition/grid2mat.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      module metis_mod
        implicit none

        interface   

          function Wrap_METIS_PartGraphKway(nelem, ncond, xadj, adjncy, iwd, nnode, ubvec, options, nec, cep) &
              bind(C, name="wrap_metis_partgraphkway") result(ierr)
            use iso_c_binding, only : c_int, c_float
            integer(c_int) :: ierr
            integer(c_int) :: nelem
            integer(c_int) :: ncond
            integer(c_int), dimension(*) :: xadj
            integer(c_int), dimension(*) :: adjncy
            integer(c_int), dimension(*) :: iwd
            integer(c_int) :: nnode
            real(c_float), dimension(*) :: ubvec
            integer(c_int), dimension(*) :: options
            integer(c_int) :: nec
            integer(c_int), dimension(*) :: cep
          end function Wrap_METIS_PartGraphKway

          function Wrap_METIS_PartGraphRecursive(nelem, ncond, xadj, adjncy, iwd, nnode, ubvec, options, nec, cep) &
              bind(C, name="wrap_metis_partgraphrecursive") result(ierr)
            use iso_c_binding, only : c_int, c_float
            integer(c_int) :: ierr
            integer(c_int) :: nelem
            integer(c_int) :: ncond
            integer(c_int), dimension(*) :: xadj
            integer(c_int), dimension(*) :: adjncy
            integer(c_int), dimension(*) :: iwd
            integer(c_int) :: nnode
            real(c_float), dimension(*) :: ubvec
            integer(c_int), dimension(*) :: options
            integer(c_int) :: nec
            integer(c_int), dimension(*) :: cep
          end function Wrap_METIS_PartGraphRecursive

          function Wrap_METIS_part(nelem, ncond, xadj, adjncy, iwd, nnode, ubvec, options, nec, cep, part, coords) &
              bind(C, name="wrap_metis_partgraphkway_part") result(ierr)
            use iso_c_binding, only : c_int, c_float
            integer(c_int) :: ierr
            integer(c_int) :: nelem
            integer(c_int) :: ncond
            integer(c_int), dimension(*) :: xadj
            integer(c_int), dimension(*) :: adjncy
            integer(c_int), dimension(*) :: iwd
            integer(c_int) :: nnode
            real(c_float), dimension(*) :: ubvec
            integer(c_int), dimension(*) :: options
            integer(c_int) :: nec
            integer(c_int), dimension(*) :: cep
            integer(c_int), dimension(*) :: part
            real(c_float), dimension(3,*) :: coords
          end function Wrap_METIS_part

          function Wrap_METIS_PartGraphRecursive_part(nelem, ncond, xadj, adjncy, iwd, nnode, ubvec, options, nec, cep,part) &
              bind(C, name="wrap_metis_partgraphrecursive_part") result(ierr)
            use iso_c_binding, only : c_int, c_float
            integer(c_int) :: ierr
            integer(c_int) :: nelem
            integer(c_int) :: ncond
            integer(c_int), dimension(*) :: xadj
            integer(c_int), dimension(*) :: adjncy
            integer(c_int), dimension(*) :: iwd
            integer(c_int) :: nnode
            real(c_float), dimension(*) :: ubvec
            integer(c_int), dimension(*) :: options
            integer(c_int) :: nec
            integer(c_int), dimension(*) :: cep
            integer(c_int), dimension(*) :: part
          end function Wrap_METIS_PartGraphRecursive_part



        end interface
      contains


      end module