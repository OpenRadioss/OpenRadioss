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
!||    iface                             ../starter/source/ale/ale3d/iface.F
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
          use constant_mod, only : em02, half, zero, one
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
          integer :: nface
          real(kind=WP) :: TT(3)    !< translation vector
          real(kind=WP) :: v12(3), v13(3), v12_(3), v13_(3)
          real(kind=WP) :: d12, d13, d12_, d13_ !< vector lengths
          real(kind=WP) :: normal1(3), normal2(3) !< normals to check non-colinearity
          real(kind=WP) :: R(3,3) !< rotation matrix
          real(kind=WP) :: tol
          real(kind=WP) :: det, cross_norm
          character(len=nchartitle) :: title
          real(kind=WP) :: imgX, imgY, imgZ
          integer,dimension(:),allocatable :: NODLIST1, NODLIST2, NODLIST3
          integer,dimension(:),allocatable :: ITAG_ELEM, IFACE, IELEM, ISEG
          integer,dimension(:,:),allocatable :: ELEM_LIST
          integer :: inod1, inod2, inod3, inod4
          logical :: lFOUND
          integer :: i, j, k
          integer :: nnod, nnod_per_face
          real(kind=WP) :: u1(3), u2(3), u3(3) !< orthonormal basis for surface 1
          real(kind=WP) :: v1(3), v2(3), v3(3) !< orthonormal basis for surface 2
          real(kind=WP) :: norm_tmp
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

          ! N1->N2 (surface 1)
          v12(1) = X(1,nod(2)) - X(1,nod(1))
          v12(2) = X(2,nod(2)) - X(2,nod(1))
          v12(3) = X(3,nod(2)) - X(3,nod(1))

          ! N1->N3 (surface 1)
          v13(1) = X(1,nod(3)) - X(1,nod(1))
          v13(2) = X(2,nod(3)) - X(2,nod(1))
          v13(3) = X(3,nod(3)) - X(3,nod(1))

          ! N1'->N2' (surface 2)
          v12_(1) = X(1,nod(5)) - X(1,nod(4))
          v12_(2) = X(2,nod(5)) - X(2,nod(4))
          v12_(3) = X(3,nod(5)) - X(3,nod(4))

          ! N1'->N3' (surface 2)
          v13_(1) = X(1,nod(6)) - X(1,nod(4))
          v13_(2) = X(2,nod(6)) - X(2,nod(4))
          v13_(3) = X(3,nod(6)) - X(3,nod(4))

          ! Compute lengths
          d12 = sqrt(v12(1)*v12(1) + v12(2)*v12(2) + v12(3)*v12(3))
          d13 = sqrt(v13(1)*v13(1) + v13(2)*v13(2) + v13(3)*v13(3))
          d12_ = sqrt(v12_(1)*v12_(1) + v12_(2)*v12_(2) + v12_(3)*v12_(3))
          d13_ = sqrt(v13_(1)*v13_(1) + v13_(2)*v13_(2) + v13_(3)*v13_(3))

          ! Tolerance
          tol = em02 * half * (d12 + d12_ + d13 + d13_) * half

          ! Check matching lengths
          if(abs(d12 - d12_) > tol) then
            lVALID = .FALSE.
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES DO NOT MATCH ON SURFACE : N1-N2 AND N1'-N2' LENGTHS ARE DIFFERENT")
            return
          endif

          if(abs(d13 - d13_) > tol) then
            lVALID = .FALSE.
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES DO NOT MATCH ON SURFACE : N1-N3 AND N1'-N3' LENGTHS ARE DIFFERENT")
            return
          endif

          ! Check non-null vectors
          if(d12 < tol .or. d13 < tol) then
            lVALID = .FALSE.
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES N1, N2, N3 DEFINE NULL VECTORS")
            return
          endif

          if(d12_ < tol .or. d13_ < tol) then
            lVALID = .FALSE.
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES N1', N2', N3' DEFINE NULL VECTORS")
            return
          endif

          ! Check non-colinearity: verify that v12 and v13 are not parallel (surface 1)
          normal1(1) = v12(2)*v13(3) - v12(3)*v13(2)
          normal1(2) = v12(3)*v13(1) - v12(1)*v13(3)
          normal1(3) = v12(1)*v13(2) - v12(2)*v13(1)
          cross_norm = sqrt(normal1(1)**2 + normal1(2)**2 + normal1(3)**2)

          if(cross_norm < tol) then
            lVALID = .FALSE.
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES N1, N2, N3 ARE COLINEAR")
            return
          endif

          ! Check non-colinearity for surface 2
          normal2(1) = v12_(2)*v13_(3) - v12_(3)*v13_(2)
          normal2(2) = v12_(3)*v13_(1) - v12_(1)*v13_(3)
          normal2(3) = v12_(1)*v13_(2) - v12_(2)*v13_(1)
          cross_norm = sqrt(normal2(1)**2 + normal2(2)**2 + normal2(3)**2)

          if(cross_norm < tol) then
            lVALID = .FALSE.
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES N1', N2', N3' ARE COLINEAR")
            return
          endif

          if(.NOT.lVALID) return

! ----------------------------------------------------------------------------------------------------------------------
          !NODES WERE CHECKED - NOW BUILD ROTATION MATRIX
! ----------------------------------------------------------------------------------------------------------------------

          ! Build orthonormal basis for surface 1
          ! u1 = v12 / |v12|
          u1(1:3) = v12(1:3) / d12

          ! u3 = normal = (v12 x v13) / |v12 x v13|
          norm_tmp = sqrt(normal1(1)**2 + normal1(2)**2 + normal1(3)**2)
          u3(1:3) = normal1(1:3) / norm_tmp

          ! u2 = u3 x u1
          u2(1) = u3(2)*u1(3) - u3(3)*u1(2)
          u2(2) = u3(3)*u1(1) - u3(1)*u1(3)
          u2(3) = u3(1)*u1(2) - u3(2)*u1(1)

          ! Build orthonormal basis for surface 2
          ! v1 = v12_ / |v12_|
          v1(1:3) = v12_(1:3) / d12_

          ! v3 = normal = (v12_ x v13_) / |v12_ x v13_|
          norm_tmp = sqrt(normal2(1)**2 + normal2(2)**2 + normal2(3)**2)
          v3(1:3) = normal2(1:3) / norm_tmp

          ! v2 = v3 x v1
          v2(1) = v3(2)*v1(3) - v3(3)*v1(2)
          v2(2) = v3(3)*v1(1) - v3(1)*v1(3)
          v2(3) = v3(1)*v1(2) - v3(2)*v1(1)

          ! Rotation matrix R such that: X' = R*X + T
          ! R = [v1 v2 v3] * [u1 u2 u3]^T
          do i = 1, 3
            do j = 1, 3
              R(i,j) = v1(i)*u1(j) + v2(i)*u2(j) + v3(i)*u3(j)
            enddo
          enddo

          ! Translation vector: T = N1' - R*N1
          TT(1) = X(1,nod(4)) - (R(1,1)*X(1,nod(1)) + R(1,2)*X(2,nod(1)) + R(1,3)*X(3,nod(1)))
          TT(2) = X(2,nod(4)) - (R(2,1)*X(1,nod(1)) + R(2,2)*X(2,nod(1)) + R(2,3)*X(3,nod(1)))
          TT(3) = X(3,nod(4)) - (R(3,1)*X(1,nod(1)) + R(3,2)*X(2,nod(1)) + R(3,3)*X(3,nod(1)))

          ! Verifying isometry by checking N2' and N3'
          imgX = R(1,1)*X(1,nod(2)) + R(1,2)*X(2,nod(2)) + R(1,3)*X(3,nod(2)) + TT(1)
          imgY = R(2,1)*X(1,nod(2)) + R(2,2)*X(2,nod(2)) + R(2,3)*X(3,nod(2)) + TT(2)
          imgZ = R(3,1)*X(1,nod(2)) + R(3,2)*X(2,nod(2)) + R(3,3)*X(3,nod(2)) + TT(3)

          if(abs(imgX - X(1,nod(5))) > tol .or. &
             abs(imgY - X(2,nod(5))) > tol .or. &
             abs(imgZ - X(3,nod(5))) > tol) then
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="ISOMETRY VERIFICATION FAILED FOR N2'")
            return
          endif

          imgX = R(1,1)*X(1,nod(3)) + R(1,2)*X(2,nod(3)) + R(1,3)*X(3,nod(3)) + TT(1)
          imgY = R(2,1)*X(1,nod(3)) + R(2,2)*X(2,nod(3)) + R(2,3)*X(3,nod(3)) + TT(2)
          imgZ = R(3,1)*X(1,nod(3)) + R(3,2)*X(2,nod(3)) + R(3,3)*X(3,nod(3)) + TT(3)

          if(abs(imgX - X(1,nod(6))) > tol .or. &
             abs(imgY - X(2,nod(6))) > tol .or. &
             abs(imgZ - X(3,nod(6))) > tol) then
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="ISOMETRY VERIFICATION FAILED FOR N3'")
            return
          endif

! ----------------------------------------------------------------------------------------------------------------------
          ! MATCH NODES BETWEEN SURFACES
! ----------------------------------------------------------------------------------------------------------------------

          NFACE = ebcs%nb_elem  !nface1=nface2 (checked by Reader subroutine), allocated to 2*nface
          NNOD = ebcs%nb_node   !nnod1=nnod2 (checked by Reader subroutine), allocated to 2*nnod

          allocate(NODLIST1(NNOD))
          allocate(NODLIST2(NNOD))
          allocate(NODLIST3(NNOD))
          NODLIST1(1:NNOD) = ebcs%node_list(1:NNOD)
          NODLIST2(1:NNOD) = ebcs%node_list(NNOD+1:NNOD+NNOD)
          NODLIST3(1:NNOD) = ebcs%node_list(NNOD+1:NNOD+NNOD) !backup

          ! ASSOCIATE NODES
          ! NODLIST1(i) : index j on surface 2 that corresponds to node i on surface 1
          ! NODLIST2(j) : -i to mark that node j on surface 2 has been matched
          DO I = 1, NNOD
            INOD1 = NODLIST1(I)
            lFOUND = .FALSE.

            ! Apply isometry to node INOD1
            imgX = R(1,1)*X(1,INOD1) + R(1,2)*X(2,INOD1) + R(1,3)*X(3,INOD1) + TT(1)
            imgY = R(2,1)*X(1,INOD1) + R(2,2)*X(2,INOD1) + R(2,3)*X(3,INOD1) + TT(2)
            imgZ = R(3,1)*X(1,INOD1) + R(3,2)*X(2,INOD1) + R(3,3)*X(3,INOD1) + TT(3)

            DO J = 1, NNOD
              INOD2 = NODLIST2(J)
              if(INOD2 > 0) then
                if(abs(imgX - X(1,INOD2)) < tol .and. &
                   abs(imgY - X(2,INOD2)) < tol .and. &
                   abs(imgZ - X(3,INOD2)) < tol) then
                  NODLIST1(I) = J
                  NODLIST2(J) = -I
                  lFOUND = .TRUE.
                  exit
                endif
              endif
            END DO

            ! NODE I on surface 1 does not match any node on surface 2
            if(.NOT. lFOUND) then
              call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
               C2="SURFACE NODES DO NOT MATCH. CHECK IF N1, N2, N3 BELONG TO EBCS SURFACE.")
              exit
            endif
          END DO

          if(.NOT. lFOUND) return

          ! Renumber list of nodes
          DO I = 1, NNOD
            ebcs%node_list(NNOD+I) = NODLIST3(NODLIST1(I))
          END DO

          ! Renumber face nodes (assuming max 4 nodes per face, adjust if needed)
          nnod_per_face = size(ebcs%elem_list, 1)
          DO I = 1, NFACE
            do k = 1, nnod_per_face
              inod1 = ebcs%elem_list(k, NFACE+I)
              if(inod1 > 0) then
                ebcs%elem_list(k, NFACE+I) = -NODLIST2(NODLIST1(INOD1))
              endif
            enddo
          END DO

          ! Ordering faces to match surface 1
          allocate(ITAG_ELEM(NFACE))
          allocate(IFACE(NFACE))
          allocate(ISEG(NFACE))
          allocate(IELEM(NFACE))
          allocate(ELEM_LIST(nnod_per_face, NFACE))

          ITAG_ELEM(1:NFACE) = 0
          ELEM_LIST(1:nnod_per_face, 1:NFACE) = ebcs%elem_list(1:nnod_per_face, NFACE+1:NFACE+NFACE)
          IFACE(1:NFACE) = ebcs%iface(NFACE+1:NFACE+NFACE)
          ISEG(1:NFACE) = ebcs%iseg(NFACE+1:NFACE+NFACE)
          IELEM(1:NFACE) = ebcs%ielem(NFACE+1:NFACE+NFACE)

          ! Match faces based on their nodes
          DO I = 1, NFACE
            DO J = 1, NFACE
              if(ITAG_ELEM(J) == 0) then
                ! Check if all nodes of face I match all nodes of face J
                lFOUND = .TRUE.
                do k = 1, nnod_per_face
                  inod1 = ebcs%elem_list(k, I)
                  if(inod1 > 0) then
                    ! Check if inod1 is in ELEM_LIST(:,J)
                    lVALID = .FALSE.
                    do inod2 = 1, nnod_per_face
                      if(inod1 == ELEM_LIST(inod2, J)) then
                        lVALID = .TRUE.
                        exit
                      endif
                    enddo
                    if(.NOT. lVALID) then
                      lFOUND = .FALSE.
                      exit
                    endif
                  endif
                enddo

                if(lFOUND) then
                  ITAG_ELEM(J) = 1
                  ebcs%elem_list(1:nnod_per_face, NFACE+I) = ELEM_LIST(1:nnod_per_face, J)
                  ebcs%iface(NFACE+I) = IFACE(J)
                  ebcs%iseg(NFACE+I) = ISEG(J)
                  ebcs%ielem(NFACE+I) = IELEM(J)
                  exit
                endif
              endif
            END DO
          END DO

          deallocate(NODLIST1, NODLIST2, NODLIST3)
          deallocate(ITAG_ELEM, IFACE, IELEM, ELEM_LIST, ISEG)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine ebcs_cyclic_surface_matching_3d
      end module ebcs_cyclic_surface_matching_3d_mod