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
!||    compute_contact_force_guide_mod   ../engine/source/tools/seatbelts/compute_contact_force_guide.F90
!||--- called by ------------------------------------------------------
!||    guided_cable_force_mod            ../engine/source/tools/seatbelts/guided_cable_force.F90
!||====================================================================
      module compute_contact_force_guide_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This suborutine compute contact force
!=======================================================================================================================
!
!||====================================================================
!||    compute_contact_force_guide   ../engine/source/tools/seatbelts/compute_contact_force_guide.F90
!||--- called by ------------------------------------------------------
!||    guided_cable_force_mod        ../engine/source/tools/seatbelts/guided_cable_force.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine compute_contact_force_guide(alpha,anchor_node,node1,node2,seg_vec,numnod,x,v,stiff,visc,           &
                                               fric,dt1,forc,forc_norm,forc_t,cont_type)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,                         only : zero
          use precision_mod,                        only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(4),                              intent(in)  :: cont_type                  !< type of contact response: "ADHERENCE" or "SLIDING"
          real(kind=WP),                             intent(in)  :: alpha                      !< position along the segment
          integer,                                   intent(in)  :: anchor_node                !< anchor node for the contact
          integer,                                   intent(in)  :: node1                      !< first node of the segment
          integer,                                   intent(in)  :: node2                      !< second node of the segment
          integer,                                   intent(in)  :: numnod                     !< number of nodes
          real(kind=WP),                             intent(in)  :: seg_vec(3)                 !< segment vector normalized
          real(kind=WP),                             intent(in)  :: x(3,numnod)                !< nodal positions
          real(kind=WP),                             intent(in)  :: v(3,numnod)                !< nodal velocities
          real(kind=WP),                             intent(in)  :: stiff                      !< contact stiffness
          real(kind=WP),                             intent(in)  :: visc                       !< contact viscosity
          real(kind=WP),                             intent(in)  :: fric                       !< friction coefficient
          real(kind=WP),                             intent(in)  :: dt1                        !< time step
          real(kind=WP),                             intent(out) :: forc(3)                    !< total contact force
          real(kind=WP),                             intent(out) :: forc_norm                  !< norm of the contact force
          real(kind=WP),                             intent(out) :: forc_t                     !< tangential contact force        
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP) :: xproj(3), vproj(3), vproj_axialn,force_ad_old(3),force_star(3),force_star_axial
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
!         position and velocity of the projection point
          xproj(1:3) = x(1:3,node1) + alpha * (x(1:3,node2) - x(1:3,node1))
          vproj(1:3) = v(1:3,node1) + alpha * (v(1:3,node2) - v(1:3,node1))-v(1:3,anchor_node)
!         velocity projection along the segment
          vproj_axialn = (vproj(1)*seg_vec(1) + vproj(2)*seg_vec(2) + vproj(3)*seg_vec(3))            
!
          if (cont_type == "ADHE") then
!           adherence - computation of tangential force
             force_ad_old(1:3) = forc(1:3)
             force_star(1:3) = force_ad_old(1:3) - stiff*dt1*(vproj(1:3)-v(1:3,anchor_node))         
             force_star_axial = (force_star(1)*seg_vec(1) + force_star(2)*seg_vec(2) + force_star(3)*seg_vec(3))
             forc_t = force_star_axial
          else  
!           sliding with friction - computation of the total contact force                 
            forc(1:3) = -stiff * (xproj(1:3)-x(1:3,anchor_node)) -visc*(vproj(1:3)-vproj_axialn*seg_vec(1:3))
            forc_norm = forc(1)**2 + forc(2)**2 + forc(3)**2      
            if (forc_norm > zero) then
              forc_norm = sqrt(forc_norm)
            else
              forc_norm = zero
            end if   
!           sliding - computation of friction axial force 
            if (abs(vproj_axialn) >  zero) then
              forc_t = -fric * forc_norm * vproj_axialn / abs(vproj_axialn)
            else
              forc_t = zero      
            end if
          endif        
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine compute_contact_force_guide
      end module compute_contact_force_guide_mod
