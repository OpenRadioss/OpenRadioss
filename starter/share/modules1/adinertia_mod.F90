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
!||    adinertia_mod          ../starter/share/modules1/adinertia_mod.F90
!||--- called by ------------------------------------------------------
!||    contrl                 ../starter/source/starter/contrl.F
!||    hm_read_adinertia      ../starter/source/tools/adinertia/hm_read_adinertia.F90
!||    lectur                 ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      module adinertia_mod
        use precision_mod, only: WP
        use names_and_titles_mod, only:nchartitle
        implicit none
        private WP

        integer :: nbaddiner
        !--------------
        type node_
          real(kind=WP) :: inertia ! inertia value for a node
          real(kind=WP) :: mass    ! mass value for a node
        END TYPE node_
        type adiner_

          character(len=nchartitle) :: title
          integer :: id
          integer :: nbnod
          integer :: type

          integer, dimension(:) ,allocatable :: nodeid ! node ids list of /adinertia
          type (node_) , dimension(:) ,allocatable :: node  ! node to store inertia/mass values for /adinertia

        end type adiner_
        type (adiner_)  , dimension(:) ,allocatable :: adinertia

      end module adinertia_mod