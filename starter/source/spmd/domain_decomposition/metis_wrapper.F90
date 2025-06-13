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




     module metis_wrapper_mod
        implicit none

!  int cpp_wrap_metis_setdefaultoptions(int *OPTIONS)
        interface
              function wrap_metis_setdefaultoptions(OPTIONS) bind(c, name='cpp_wrap_metis_setdefaultoptions')
                  use iso_c_binding
                  integer(c_int), intent(inout) :: OPTIONS(40)
                  integer(c_int) :: cpp_wrap_metis_setdefaultoptions
              end function wrap_metis_setdefaultoptions 
!  int cpp_wrap_metis_partgraphkway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
!                                   int *IWD, int *NNODE,
!                                   float *UBVEC, int *OPTIONS, int *NEC, int *CEP)
! 
              function wrap_metis_partgraphkway(NELEM, NCOND, XADJ, ADJNCY, IWD, NNODE, UBVEC, OPTIONS, NEC, CEP,X) &
                  bind(c, name='cpp_wrap_metis_partgraphkway')
                  use iso_c_binding
                  integer(c_int), intent(in) :: NELEM, NCOND, XADJ(*), ADJNCY(*), IWD(*), NNODE
                  real(c_float), intent(in) :: UBVEC(*), X(3,*)
                  integer(c_int), intent(inout) :: OPTIONS(40)
                  integer(c_int), intent(out) :: NEC, CEP(*)
                  integer(c_int) :: wrap_metis_partgraphkway
              end function wrap_metis_partgraphkway 
!  int cpp_wrap_metis_partgraphrecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
!                                        int *IWD, int *NNODE,
!                                        float *UBVEC, int *OPTIONS, int *NEC, int *CEP)
! 
              function wrap_metis_partgraphrecursive(NELEM, NCOND, XADJ, ADJNCY, IWD, NNODE, UBVEC, OPTIONS, NEC, CEP,X) &
                  bind(c, name='cpp_wrap_metis_partgraphrecursive')
                  use iso_c_binding
                  integer(c_int), intent(in) :: NELEM, NCOND, XADJ(*), ADJNCY(*), IWD(*), NNODE
                  real(c_float), intent(in) :: UBVEC(*),X(3,*)
                  integer(c_int), intent(inout) :: OPTIONS(40)
                  integer(c_int), intent(out) :: NEC, CEP(*)
                  integer(c_int) :: wrap_metis_partgraphrecursive
              end function wrap_metis_partgraphrecursive
        end interface

     end module metis_wrapper_mod