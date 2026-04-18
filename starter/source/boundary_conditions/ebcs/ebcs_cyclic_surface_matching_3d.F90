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
!||    ebcs_cyclic_surface_matching_3d_mod   ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_3d.F90
!||--- called by ------------------------------------------------------
!||    ebcs_cyclic_surface_matching          ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching.F90
!||====================================================================
      module ebcs_cyclic_surface_matching_3d_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief check surface 1 (nseg) and surface 2 (nseg)
!! \details storing data in linear arrays %elem_list and %node_lide with same order 1:nseg and nseg+1 : nseg+nseg
!! \details number of segment already match (checked with Reader subroutine)
!||====================================================================
!||    ebcs_cyclic_surface_matching_3d   ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_3d.F90
!||--- called by ------------------------------------------------------
!||    ebcs_cyclic_surface_matching      ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                            ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod                       ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine ebcs_cyclic_surface_matching_3d(ebcs_cyclic, ebcs, numnod, X)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use ebcs_mod , only : t_ebcs_cyclic, t_ebcs
          use groupdef_mod , only : surf_
          use constant_mod, only : em06, half, zero, one, em03
          use names_and_titles_mod , only : nchartitle
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(t_ebcs_cyclic), intent(in) :: ebcs_cyclic   !< ebcs data structure (specific data structure)
          type(t_ebcs), target, intent(inout) :: ebcs      !< common data structure
          integer,intent(in) :: numnod
          real(kind=WP),intent(in) :: X(3,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: lVALID
          integer :: surf_iid(1:2)
          integer :: nod(6)
          integer :: ebcs_uid
          real(kind=WP) :: v12(3),v12_(3)
          real(kind=WP) :: v13(3),v13_(3)
          real(kind=WP) :: d12,d12_,d13,d13_ !< vector length
          real(kind=WP) :: costh, dotp, theta, theta_
          real(kind=WP) :: tol
          real(kind=WP) :: cost, sint
          character(len=nchartitle) :: title
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          title = ebcs%title
          ebcs_uid = ebcs%ebcs_id

          surf_iid(1) = ebcs%surf_id
          surf_iid(2) = ebcs%surf_id2

          nod(1:6) = ebcs_cyclic%node_id(1:6)


          !--- TESTING USER NODES
          lVALID = .TRUE.

          ! N1->N2
          v12(1)  = X(1,nod(2))-X(1,nod(1))
          v12(2)  = X(2,nod(2))-X(2,nod(1))
          v12(3)  = X(3,nod(2))-X(3,nod(1))
          ! N1'->N2'
          v12_(1) = X(1,nod(5))-X(1,nod(4))
          v12_(2) = X(2,nod(5))-X(2,nod(4))
          v12_(3) = X(3,nod(5))-X(3,nod(4))

          !tol
          d12 = sqrt(v12(1)*v12(1) + v12(2)*v12(2) + v12(3)*v12(3))
          d12_ = sqrt(v12_(1)*v12_(1) + v12_(2)*v12_(2) + v12_(3)*v12_(3))
          tol = em06*half*(d12+d12_)

          if(abs(d12-d12_) > tol )then
            lVALID = .FALSE.
            !ERROR MESSAGE : user nodes N1N2 and N1'N2' do not match on both surfaces
            call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="NODES DO NOT MATCH ON SURFACE : LENGTHES ARE DIFFERENT")
            return
          endif

          if(d12 < tol .or. d12_ < tol )then
            lVALID = .FALSE.
            !ERROR MESSAGE : N1N2 leads to null vector
            call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="NODES DEFINE A NULL VECTOR")
            return
          endif

          ! N1->N3
          v13(1)  = X(1,nod(3))-X(1,nod(1))
          v13(2)  = X(2,nod(3))-X(2,nod(1))
          v13(3)  = X(3,nod(3))-X(3,nod(1))
          ! N1'->N3'
          v13_(1) = X(1,nod(6))-X(1,nod(4))
          v13_(2) = X(2,nod(6))-X(2,nod(4))
          v13_(3) = X(3,nod(6))-X(3,nod(4))
          ! lenghtes
          d13 = sqrt(v13(1)*v13(1) + v13(2)*v13(2) + v13(3)*v13(3))
          d13_ = sqrt(v13_(1)*v13_(1) + v13_(2)*v13_(2) + v13_(3)*v13_(3))
          if(abs(d13-d13_) > tol )then
            lVALID = .FALSE.
            !ERROR MESSAGE : user nodes N1N3 and N1'N3' do not match on both surfaces
             call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="THIRD NODES DO NOT MATCH ON SURFACE : N1N3 LENGTHES ARE DIFFERENT")
            return
          end if
          if(d13 < tol)then
            lVALID = .FALSE.
            !ERROR MESSAGE : N1N3 leads to null vector
             call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="THIRD NODES DEFINE A NULL VECTOR ON SURFACE 1")
            return
          endif
          if(d13_ < tol )then
            lVALID = .FALSE.
            !ERROR MESSAGE : N1N3 leads to null vector
             call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="THIRD NODES DEFINE A NULL VECTOR ON SURFACE 2")
            return
          endif
          !CHECK ANGLE
          ! N1-N2 / N1-N3
          dotp = v12(1)*v13(1) + v12(2)*v13(2) + v12(3)*v13(3)
          costh = dotp / (d12*d13)
          costh = min(costh,one)
          costh = max(-one, costh)
          theta = acos(costh)
          ! N1'-N2' / N1'-N3'
          dotp = v12_(1)*v13_(1) + v12_(2)*v13_(2) + v12_(3)*v13_(3)
          costh = dotp / (d12_*d13_)
          costh = min(costh,one)
          costh = max(-one, costh)
          theta_ = acos(costh)
          if(abs(theta-theta_) > tol)then
            lVALID = .FALSE.
            !ERROR MESSAGE : angles does not match
             call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="NODES DO NOT MATCH : THEY ARE DEFINING DIFFERENT ANGLES ON BOTH SURFACES")
            return
          end if
          if(abs(theta) < em03)then
            lVALID = .FALSE.
             call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="THE THREE NODES ARE DEFINING A NULL ANLGE ON SURFACE 1")
            return
          end if
          if(abs(theta_) < em03)then
            lVALID = .FALSE.
             call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
             C2="THE THREE NODES ARE DEFINING A NULL ANLGE ON SURFACE 2")
            return
          end if

          if(.NOT.lVALID) return

! ----------------------------------------------------------------------------------------------------------------------
          ! --- FROM HERE NODES WERE TESTED AND DO MATCH
          cost = zero
          sint = zero




! ----------------------------------------------------------------------------------------------------------------------
        end subroutine ebcs_cyclic_surface_matching_3d
      end module ebcs_cyclic_surface_matching_3d_mod
