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
!hd|====================================================================
!hd|  int8_mod                      modules/interfaces/int8_mod.f
!hd|-- called by -----------
!hd|        intbuf_ini_t8                 common_source/interf/intbuf_ini.f
!hd|        intbufdef_mod                 common_source/modules/intbufdef_mod.f
!hd|        ddsplit                       starter/source/restart/ddsplit/ddsplit.f
!hd|        lectur                        starter/source/starter/lectur.f
!hd|        prepare_split_i8              starter/source/restart/ddsplit/inter_tools.f
!hd|        set_front8                    starter/source/spmd/node/ddtools.f
!hd|        split_interfaces              starter/source/restart/ddsplit/split_interfaces.f
!hd|        w_type8                       starter/source/restart/ddsplit/split_interfaces.f
!hd|        i8loc3                        engine/source/interfaces/inter3d/i8loc3.f
!hd|        i8msr3                        engine/source/interfaces/inter3d/i8msr3.f
!hd|        intfop8                       engine/source/interfaces/interf/intfop8.f
!hd|        intvo8                        engine/source/interfaces/inter3d/intvo8.f
!hd|        spmd_i8_commslv               engine/source/mpi/interfaces/spmd_i8tool.f
!hd|        spmd_i8_iloc                  engine/source/mpi/interfaces/spmd_i8tool.f
!hd|        spmd_i8_index                 engine/source/mpi/interfaces/spmd_i8tool.f
!hd|        spmd_i8_irtl                  engine/source/mpi/interfaces/spmd_i8tool.f
!hd|        spmd_i8_updbuf                engine/source/mpi/interfaces/spmd_i8tool.f
!hd|        write_intbuf_t8               engine/source/output/restart/write_intbuf.f
!hd|        wrrestp                       engine/source/output/restart/wrrestp.f
!hd|        intbuf_mod                    engine/share/modules/restart_mod.f
!hd|-- calls ---------------
!hd|====================================================================
module int8_mod
!-----------------------------------------------------------------------
#include "my_real.inc"

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
   end type

end module
