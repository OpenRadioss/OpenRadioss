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
      module int8_mod
!-----------------------------------------------------------------------

        type buft8
          integer ::  nbmain
          integer ::  nbsecnd_tot
          integer, dimension(:) , pointer ::  nbsecnd !table of nbsecnd per main
          integer, dimension(:) , pointer ::  main_uid
          integer, dimension(:) , pointer ::  main_id
          integer, dimension(:) , pointer ::  secnd_uid
          integer, dimension(:) , pointer ::  secnd_id
        end type buft8
        type front8
          !local number of the main node :
          integer ::  numloc
          !user id of the main node :
          integer ::  uid
          ! number of time the interface main has to be
          ! sent and received
          integer  ::  nbcom !
          !list (of size nbcom) of processors that share the main
          integer, dimension(:) , pointer ::  proclist
          !pointer to the structure buffer
          integer, dimension(:) , pointer ::  buf_index
        end type front8

        type int8_struct_
          integer :: ni
          integer :: s_comm
          integer :: is_activated
          type(buft8), dimension(:) , pointer :: buffer
          type(front8), dimension(:) , pointer :: spmd_comm_pattern
        end type int8_struct_

      end module int8_mod
