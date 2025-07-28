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
      !||    get_edge_fic_node_mod   ../starter/source/output/subinterface/get_edge_fic_node.F90
      !||--- called by ------------------------------------------------------
      !||    inintsub_25             ../starter/source/output/subinterface/inintsub_25.F
      !||====================================================================
      module get_edge_fic_node_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief get the edge to which the ficitf node blongs
      !||====================================================================
      !||    get_edge_fic_node   ../starter/source/output/subinterface/get_edge_fic_node.F90
      !||--- called by ------------------------------------------------------
      !||    inintsub_25         ../starter/source/output/subinterface/inintsub_25.F
      !||====================================================================
        subroutine get_edge_fic_node(irtse   ,nsne    ,is2se  ,is2pt   ,ns       , nrtse ,is1 , is2)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: ns         ! ficitf node
          integer,                                   intent(in) :: nsne       ! number of fictif node
          integer,                                   intent(in) :: nrtse      ! number of fictif segments
          integer,                                   intent(in) :: is2se(2,nsne )
          integer,                                   intent(in) :: is2pt(nsne )
          integer,                                   intent(in) :: irtse(5,nrtse )
          integer,                                   intent(inout) :: is1,is2 ! nodes of the edge

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ie,ie1,ie2,ied,ns1,ns2,ip
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
           endif
           return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_edge_fic_node
      end module get_edge_fic_node_mod

