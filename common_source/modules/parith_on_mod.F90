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
      !||    parith_on_mod          ../common_source/modules/parith_on_mod.F90
      !||--- called by ------------------------------------------------------
      !||    connectivity_mod       ../common_source/modules/connectivity.F90
      !||    reallocate_i_skyline   ../engine/source/system/reallocate_skyline.F
      !||    resol_init             ../engine/source/engine/resol_init.F
      !||    update_pon_shells      ../engine/source/engine/node_spliting/update_pon.F90
      !||====================================================================
      module parith_on_mod
#include "my_real.inc"

        type element_pon_ 
            integer :: SFSKY !< second dimension of FSKY 
            integer :: SADSKY !< size of ADSKY (numnod + 1 ?)
            integer :: MAX_SFSKY  !< max size of FSKY (allocated)
            my_real, dimension(:,:), allocatable :: FSKY !< 8xSFSKY array of the skyline Forces
            my_real, dimension(:), allocatable :: FSKYM !< mass (solid only?) 
            my_real, dimension(:), allocatable :: FTHESKY !<        
            my_real, dimension(:), allocatable :: CONDNSKY !< 
            my_real, dimension(:), allocatable :: FSKYD !< sph ?
            integer, dimension(:), allocatable :: ADSKY !< pointers to FSKY
             !spmd:
            integer, dimension(:), allocatable :: IADRCP !< reception of forces
            integer, dimension(:), allocatable :: IRECVP !< reception of forces
            integer, dimension(:), allocatable :: IADSDP !< send forces
            integer, dimension(:), allocatable :: ISENDP !< send forces


            integer, dimension(:,:), allocatable :: IADS !< 1 ; 8xNUMELS  solid indexes to FSKY
            integer, dimension(:,:), allocatable :: IADS10 !< 6* NUMELS10 
            integer, dimension(:,:), allocatable :: IADS20 ! 12*NUMELS20
            integer, dimension(:,:), allocatable :: IADS16 ! 8*NUMELS16
            integer, dimension(:,:), allocatable :: IADQ !<i87b ; quad i87b 
            integer, dimension(:,:), allocatable :: IADC !<i87C shell (4 nodes) indexes to FSKY
            integer, dimension(:,:), allocatable :: IAD_TRUSS !< I87D 2xNUMELT
            integer, dimension(:,:), allocatable :: IAD_BEAM !< I87E 2xNUMELP
            integer, dimension(:,:), allocatable :: IAD_SPRING !<F 3xNUMELR
            integer, dimension(:,:), allocatable :: IAD_TG !<G 3xNUMELTG
            integer, dimension(:,:), allocatable :: IAD_TG6 !<H 3xNUMELTG6
            integer, dimension(:,:), allocatable :: IAD_MV !I 4xNSKYMV0
            integer, dimension(:,:), allocatable :: IAD_CONLD !<J 4xNCONLD
            integer, dimension(:,:), allocatable :: IAD_CONV !<K 4x glob_therm%NCONV
            integer, dimension(:,:), allocatable :: IAD_RADIA !<L 4x glob_therm%Numrada
            integer, dimension(:), allocatable :: IAD_LOADP !<M SLLOADP
            integer, dimension(:,:), allocatable :: IAD_FXFLUX !<N 4x glob_therm%nfxflux
        end type element_pon_
    

        type interface_pon_
          my_real, dimension(:,:), allocatable :: FSKYI
          integer, dimension(:), allocatable :: ISKY    
          integer, dimension(:), allocatable :: ADSKYI
        end type

        contains 
      end module parith_on_mod
