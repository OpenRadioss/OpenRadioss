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
!||    int8_mod           ../common_source/modules/interfaces/int8_mod.F90
!||--- called by ------------------------------------------------------
!||    ddsplit            ../starter/source/restart/ddsplit/ddsplit.F
!||    i8loc3             ../engine/source/interfaces/inter3d/i8loc3.F
!||    i8msr3             ../engine/source/interfaces/inter3d/i8msr3.F
!||    intbuf_ini_t8      ../common_source/interf/intbuf_ini.F
!||    intbufdef_mod      ../common_source/modules/interfaces/intbufdef_mod.F90
!||    intfop8            ../engine/source/interfaces/interf/intfop8.F
!||    intvo8             ../engine/source/interfaces/inter3d/intvo8.F
!||    lectur             ../engine/source/input/lectur.F
!||    prepare_split_i8   ../starter/source/restart/ddsplit/inter_tools.F
!||    set_front8         ../starter/source/spmd/node/ddtools.F
!||    split_interfaces   ../starter/source/restart/ddsplit/split_interfaces.F
!||    spmd_i8_commslv    ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_iloc       ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_index      ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_irtl       ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_updbuf     ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    w_type8            ../starter/source/restart/ddsplit/split_interfaces.F
!||    write_intbuf_t8    ../engine/source/output/restart/write_intbuf.F
!||    wrrestp            ../engine/source/output/restart/wrrestp.F
!||====================================================================
      module int8_mod
      implicit none
!-----------------------------------------------------------------------

        type buft8
          integer ::  nbmain
          integer ::  nbsecnd_tot
          integer, dimension(:) , pointer ::  nbsecnd => null() !table of nbsecnd per main
          integer, dimension(:) , pointer ::  main_uid => null()
          integer, dimension(:) , pointer ::  main_id => null()
          integer, dimension(:) , pointer ::  secnd_uid => null()
          integer, dimension(:) , pointer ::  secnd_id => null()
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
          integer, dimension(:) , pointer ::  proclist => null()
          !pointer to the structure buffer
          integer, dimension(:) , pointer ::  buf_index => null()
        end type front8

        type int8_struct_
          integer :: ni
          integer :: s_comm
          integer :: is_activated
          type(buft8), dimension(:) , pointer :: buffer => null()
          type(front8), dimension(:) , pointer :: spmd_comm_pattern => null()
        end type int8_struct_

      end module int8_mod
