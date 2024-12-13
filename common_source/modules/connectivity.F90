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
      !||    connectivity_mod   ../common_source/modules/connectivity.F90
      !||--- called by ------------------------------------------------------
      !||    radioss2           ../engine/source/engine/radioss2.F
      !||    rdresb             ../engine/source/output/restart/rdresb.F
      !||    resol              ../engine/source/engine/resol.F
      !||    resol_head         ../engine/source/engine/resol_head.F
      !||    restalloc          ../engine/source/output/restart/arralloc.F
      !||    wrrestp            ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      module connectivity_mod
!       INTEGER, PARAMETER :: NIXS = 11
!       INTEGER, PARAMETER :: NIXC = 7
!       INTEGER, PARAMETER :: NIXQ = 7
!       INTEGER, PARAMETER :: NIXT = 5
!       INTEGER, PARAMETER :: NIXP = 6
!       INTEGER, PARAMETER :: NIXR = 6
!       INTEGER, PARAMETER :: NIXTG = 6
!       INTEGER, PARAMETER :: NIXUR = 6
        type shell_
          ! old storage of shells
          integer, dimension(:,:), allocatable :: ixc !< ixc(1,i) : Material ID of the i-th shell element
                                                      !< ixc(2:5,i) :  nodes of the i-th shell element
                                                      !< ixc(6,i) :  PID of the i-th shell element 
                                                      !< ixc(7,i) :  user id of the shell element
         ! new storage of shells
          integer, dimension(:,:), allocatable :: nodes !< nodes(1:4,i) :  nodes of the i-th shell element
          integer, dimension(:), allocatable :: pid !< pid(i) :  PID of the i-th shell element
          integer, dimension(:), allocatable :: matid !< matid(i) :  Material ID of the i-th shell element
          integer, dimension(:), allocatable :: user_id !< user_id(i) :  user id of the shell element
        end type shell_
        type solid_
          ! old storage of solids
          integer, dimension(:,:), allocatable :: ixs !< ixs(1,i) : Material ID of the i-th solid element
                                                      !< ixs(2:9,i) :  nodes of the i-th solid element
                                                      !< ixs(10,i) :  PID of the i-th solid element 
                                                      !< ixs(11,i) :  user id of the solid element
          !new storage of solids
          integer, dimension(:,:), allocatable :: nodes !< nodes(1:8,i) :  nodes of the i-th solid element
          integer, dimension(:), allocatable :: pid !< pid(i) :  PID of the i-th solid element
          integer, dimension(:), allocatable :: matid !< matid(i) :  Material ID of the i-th solid element
          integer, dimension(:), allocatable :: user_id !< user_id(i) :  user id of the solid element
        end type solid_

        type connectivity_
          type(shell_) :: shell
          type(solid_) :: solid
        end type connectivity_ 
      end module
