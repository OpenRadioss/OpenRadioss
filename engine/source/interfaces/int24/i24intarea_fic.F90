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
      !||    i24intarea_fic_mod   ../engine/source/interfaces/int24/i24intarea_fic.F90
      !||--- called by ------------------------------------------------------
      !||    i24_save_sub         ../engine/source/interfaces/int24/i24_save_sub.F
      !||    i24for3              ../engine/source/interfaces/int24/i24for3.F
      !||====================================================================
      module i24intarea_fic_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief get the area of fictif node based on edge nodes
      !||====================================================================
      !||    i24intarea_fic   ../engine/source/interfaces/int24/i24intarea_fic.F90
      !||--- called by ------------------------------------------------------
      !||    i24_save_sub     ../engine/source/interfaces/int24/i24_save_sub.F
      !||    i24for3          ../engine/source/interfaces/int24/i24for3.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod     ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine i24intarea_fic(irtse, nsne, is2se, is2pt, ns, nrtse, numnod, arean, arean_fic)

! --------------------------------------------------------------------------------------------------
!                                                   Modules
! --------------------------------------------------------------------------------------------------
          use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none

#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: ns         ! ficitf node
          integer,                                   intent(in) :: nsne       ! number of fictif node
          integer,                                   intent(in) :: nrtse      ! number of fictif segments
          integer,                                   intent(in) :: numnod     ! number of nodes
          integer,                                   intent(in) :: is2se(2,nsne )
          integer,                                   intent(in) :: is2pt(nsne )
          integer,                                   intent(in) :: irtse(5,nrtse )
          my_real,                                   intent(in) :: arean(numnod) ! nodal areas
          my_real,                                   intent(inout) :: arean_fic  ! nodal area of fictif node

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ie,ie1,ie2,ied,ns1,ns2,ip,is1,is2
          integer :: ik1(4),ik2(4) 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
         ik1 = (/1, 2, 3, 4/)
         ik2 = (/2, 3, 4, 1/)
         if (ns >= 0) THEN
            ik1 = (/1, 2, 3, 4/)
            ik2 = (/2, 3, 4, 1/)
            ip = is2pt(ns)
            ie1 = is2se(1, ns)
            ie2 = is2se(2, ns)
            if (ie1 /= 0) then
               ie = ie1
               ied = irtse(5, ie)
               ns1 = ik1(ied)
               ns2 = ik2(ied)
             else if (ie2 /= 0) then
               ie = ie2
               ied = irtse(5, ie)
               ns1 = ik2(ied)
               ns2 = ik1(ied)
             else
                print *, 'probleme EDGES,IE1,IE2=', ns, ie1, ie2
             end if
             is1 = irtse(ns1, ie)
             is2 = irtse(ns2, ie)

             arean_fic = half*(arean(is1)+arean(is2))
           endif
       return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine i24intarea_fic
      end module i24intarea_fic_mod

