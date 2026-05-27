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
!! \brief check surface 1 (nseg) and surface 2 (nseg) for 3D isometry
!! \details storing data in linear arrays %elem_list and %node_list with same order 1:nseg and nseg+1 : nseg+nseg
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
          use constant_mod, only : em10, em02, em03, half
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
          integer,intent(in) :: numnod                     !< Total number of nodes in the model
          real(kind=WP),intent(in) :: X(3,numnod)          !< Coordinates of all nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: lFOUND
          integer :: nod(6)           !< N1, N2, N3 for surface 1; N1', N2', N3' for surface 2
          integer :: ebcs_uid
          real(kind=WP) :: v12(3),v12_(3)
          real(kind=WP) :: v13(3),v13_(3)
          real(kind=WP) :: d12,d12_,d13,d13_ !< vector length
          real(kind=WP) :: tol
          real(kind=WP) :: cross_norm
          character(len=nchartitle) :: title
          real(kind=WP) :: imgX, imgY, imgZ                 !< Transformed coordinates
          integer,dimension(:),allocatable :: NODLIST1_idx   !< Stores index J of surface 2 node for node I of surface 1
          integer,dimension(:),allocatable :: NODLIST2_used  !< Marks if surface 2 node J has been used (-I if used by node I)
          integer,dimension(:),allocatable :: NODLIST3_backup!< Backup of initial surface 2 node IDs
          integer,dimension(:),allocatable :: ITAG_ELEM      !< Marks if surface 2 segment J has been used
          integer,dimension(:),allocatable :: IFACE_backup, IELEM_backup, ISEG_backup !< Backups for segment data
          integer,dimension(:,:),allocatable :: ELEM_LIST_backup                    !< Backup for segment node IDs
          integer :: inod1_s2, inod2_s2, inod3_s2, inod4_s2 !< Node IDs for surface 2 segments
          integer :: i, j                                    !< Loop counters
          integer :: nnod                                   !< Number of nodes in each surface
          real(kind=WP) :: u1(3), u2(3), u3(3)              !< Orthonormal basis for surface 1
          real(kind=WP) :: v1(3), v2(3), v3(3)              !< Orthonormal basis for surface 2
          real(kind=WP) :: norm_tmp                         !< Temporary norm
          logical :: all_nodes_found                        !< Flag for overall node matching status
          integer :: s2_node_id1, s2_node_id2, s2_node_id3, s2_node_id4 !< Transformed node IDs for S1 segment
          real(kind=WP) :: transformed_coords(3) !< Temporary array for transformed coordinates
          real(kind=WP) :: current_s1_node_coords(3) !< Temporary array for current S1 node coordinates
          integer :: node_id_s1, node_id_s2,node_id_s3,node_id_s4 !< Node IDs from ebcs%elem_list
          real(kind=wp) :: normal1(3), normal2(3) !< Normals for surface 1 and surface 2
          real(kind=wp) :: R(3,3)
          real(kind=wp) :: tt(3)
          real(kind=wp) :: abs_tol
          integer :: nseg

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          title = ebcs%title
          ebcs_uid = ebcs%ebcs_id

          ! nod(1) = N1, nod(2) = N2, nod(3) = N3 for surface 1
          ! nod(4) = N1', nod(5) = N2', nod(6) = N3' for surface 2
          nod(1:6) = ebcs_cyclic%node_id(1:6)

          !--- TESTING USER NODES

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
          d12 = sqrt(v12(1)**2 + v12(2)**2 + v12(3)**2)
          d13 = sqrt(v13(1)**2 + v13(2)**2 + v13(3)**2)
          d12_ = sqrt(v12_(1)**2 + v12_(2)**2 + v12_(3)**2)
          d13_ = sqrt(v13_(1)**2 + v13_(2)**2 + v13_(3)**2)

          ! Tolerance (corrected logic for robustness)
          abs_tol = em10 ! A small absolute tolerance (e.g., 1.0E-10)
          ! The relative tolerance is based on the average length of the reference vectors
          tol = MAX(abs_tol, em02 * half * (d12 + d12_ + d13 + d13_) * half)

          ! --- Check for null or too short reference vectors ---
          if (d12 < abs_tol .or. d13 < abs_tol .or. d12_ < abs_tol .or. d13_ < abs_tol) then
              call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title),&
                C2="NODES DEFINE A NULL OR TOO SHORT REFERENCE VECTOR. Check N1,N2,N3 and N1',N2',N3'.")
              return
          end if

          ! Check matching lengths (N1-N2 vs N1'-N2')
          if(abs(d12 - d12_) > tol) then
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="N1-N2 AND N1'-N2' LENGTHS ARE DIFFERENT (not an isometry).")
            return
          endif

          ! Check matching lengths (N1-N3 vs N1'-N3')
          if(abs(d13 - d13_) > tol) then
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="N1-N3 AND N1'-N3' LENGTHS ARE DIFFERENT (not an isometry).")
            return
          endif

          ! Check non-colinearity for surface 1 (v12 and v13)
          normal1(1) = v12(2)*v13(3) - v12(3)*v13(2)
          normal1(2) = v12(3)*v13(1) - v12(1)*v13(3)
          normal1(3) = v12(1)*v13(2) - v12(2)*v13(1)
          cross_norm = sqrt(normal1(1)**2 + normal1(2)**2 + normal1(3)**2)

          if(cross_norm < em03 * d12 * d13) then ! Relative sine threshold: sin(theta) < em03
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES N1, N2, N3 ARE COLINEAR (cannot define a plane).")
            return
          endif

          ! Check non-colinearity for surface 2 (v12' and v13')
          normal2(1) = v12_(2)*v13_(3) - v12_(3)*v13_(2)
          normal2(2) = v12_(3)*v13_(1) - v12_(1)*v13_(3)
          normal2(3) = v12_(1)*v13_(2) - v12_(2)*v13_(1)
          cross_norm = sqrt(normal2(1)**2 + normal2(2)**2 + normal2(3)**2)

          if(cross_norm < em03 * d12_ * d13_) then ! Relative sine threshold: sin(theta) < em03
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="NODES N1', N2', N3' ARE COLINEAR (cannot define a plane).")
            return
          endif


! ----------------------------------------------------------------------------------------------------------------------
          ! NODES WERE CHECKED - NOW BUILD ROTATION MATRIX AND TRANSLATION VECTOR
! ----------------------------------------------------------------------------------------------------------------------

          ! Build orthonormal basis for surface 1 (u1, u2, u3)
          ! u1 = v12 / |v12|
          u1(1:3) = v12(1:3) / d12

          ! u3 = normal = (v12 x v13) / |v12 x v13|
          norm_tmp = sqrt(normal1(1)**2 + normal1(2)**2 + normal1(3)**2)
          u3(1:3) = normal1(1:3) / norm_tmp

          ! u2 = u3 x u1 (ensures right-handed system)
          u2(1) = u3(2)*u1(3) - u3(3)*u1(2)
          u2(2) = u3(3)*u1(1) - u3(1)*u1(3)
          u2(3) = u3(1)*u1(2) - u3(2)*u1(1)

          ! Build orthonormal basis for surface 2 (v1, v2, v3)
          ! v1 = v12_ / |v12_|
          v1(1:3) = v12_(1:3) / d12_

          ! v3 = normal = (v12_ x v13_) / |v12_ x v13_|
          norm_tmp = sqrt(normal2(1)**2 + normal2(2)**2 + normal2(3)**2)
          v3(1:3) = normal2(1:3) / norm_tmp

          ! v2 = v3 x v1 (ensures right-handed system)
          v2(1) = v3(2)*v1(3) - v3(3)*v1(2)
          v2(2) = v3(3)*v1(1) - v3(1)*v1(3)
          v2(3) = v3(1)*v1(2) - v3(2)*v1(1)

          ! Rotation matrix R such that: X' = R*X + T
          ! R transforms coordinates from the S1 basis to the S2 basis.
          ! R = [v1 v2 v3] * [u1 u2 u3]^T
          ! R(i,j) = v1(i)*u1(j) + v2(i)*u2(j) + v3(i)*u3(j)
          do i = 1, 3
            do j = 1, 3
              R(i,j) = v1(i)*u1(j) + v2(i)*u2(j) + v3(i)*u3(j)
            enddo
          enddo

          ! Translation vector: T = P_N1' - R*P_N1
          ! Where P_N1 is the position vector of nod(1)
          TT(1) = X(1,nod(4)) - (R(1,1)*X(1,nod(1)) + R(1,2)*X(2,nod(1)) + R(1,3)*X(3,nod(1)))
          TT(2) = X(2,nod(4)) - (R(2,1)*X(1,nod(1)) + R(2,2)*X(2,nod(1)) + R(2,3)*X(3,nod(1)))
          TT(3) = X(3,nod(4)) - (R(3,1)*X(1,nod(1)) + R(3,2)*X(2,nod(1)) + R(3,3)*X(3,nod(1)))

          ! Verifying isometry by checking N2' and N3' (transformed N2 and N3 should match N2' and N3')
          ! Transformed N2
          imgX = R(1,1)*X(1,nod(2)) + R(1,2)*X(2,nod(2)) + R(1,3)*X(3,nod(2)) + TT(1)
          imgY = R(2,1)*X(1,nod(2)) + R(2,2)*X(2,nod(2)) + R(2,3)*X(3,nod(2)) + TT(2)
          imgZ = R(3,1)*X(1,nod(2)) + R(3,2)*X(2,nod(2)) + R(3,3)*X(3,nod(2)) + TT(3)

          if(abs(imgX - X(1,nod(5))) > tol .or. &
             abs(imgY - X(2,nod(5))) > tol .or. &
             abs(imgZ - X(3,nod(5))) > tol) then
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="ISOMETRY VERIFICATION FAILED: Transformed N2 does not match N2'.")
            return
          endif

          ! Transformed N3
          imgX = R(1,1)*X(1,nod(3)) + R(1,2)*X(2,nod(3)) + R(1,3)*X(3,nod(3)) + TT(1)
          imgY = R(2,1)*X(1,nod(3)) + R(2,2)*X(2,nod(3)) + R(2,3)*X(3,nod(3)) + TT(2)
          imgZ = R(3,1)*X(1,nod(3)) + R(3,2)*X(2,nod(3)) + R(3,3)*X(3,nod(3)) + TT(3)

          if(abs(imgX - X(1,nod(6))) > tol .or. &
             abs(imgY - X(2,nod(6))) > tol .or. &
             abs(imgZ - X(3,nod(6))) > tol) then
            call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
             C2="ISOMETRY VERIFICATION FAILED: Transformed N3 does not match N3'.")
            return
          endif

! ----------------------------------------------------------------------------------------------------------------------
          ! MATCH NODES BETWEEN SURFACES
! ----------------------------------------------------------------------------------------------------------------------

          nseg = ebcs%nb_elem  ! nseg1=nseg2 (checked by Reader subroutine), allocated to 2*nseg
          nnod = ebcs%nb_node   ! nnod1=nnod2 (checked by Reader subroutine), allocated to 2*nnod

          allocate(NODLIST1_idx(nnod))     ! Stores index J of surface 2 node for node I of surface 1
          allocate(NODLIST2_used(nnod))    ! Marks if surface 2 node J has been used (-I if used by node I)
          allocate(NODLIST3_backup(nnod))  ! Backup of initial surface 2 node IDs

          ! Initialize NODLIST2_used with the original node IDs of surface 2
          ! This allows us to mark them as used by setting to a negative value.
          NODLIST2_used(1:nnod) = ebcs%node_list(nnod+1:nnod+nnod)
          NODLIST3_backup(1:nnod) = ebcs%node_list(nnod+1:nnod+nnod) ! Backup for final renumbering


          all_nodes_found = .TRUE. ! Flag to track if all nodes from surface 1 found a match

          DO I = 1, nnod ! Loop through each node of surface 1
            node_id_s1 = ebcs%node_list(I) ! Get the actual node ID from surface 1 list

            ! Get coordinates of current node from surface 1
            current_s1_node_coords(1) = X(1, node_id_s1)
            current_s1_node_coords(2) = X(2, node_id_s1)
            current_s1_node_coords(3) = X(3, node_id_s1)

            ! Apply isometry (Rotation + Translation) to the current node from surface 1
            transformed_coords(1) = R(1,1)*current_s1_node_coords(1) + R(1,2)*current_s1_node_coords(2) +&
             R(1,3)*current_s1_node_coords(3) + TT(1)
            transformed_coords(2) = R(2,1)*current_s1_node_coords(1) + R(2,2)*current_s1_node_coords(2) +&
             R(2,3)*current_s1_node_coords(3) + TT(2)
            transformed_coords(3) = R(3,1)*current_s1_node_coords(1) + R(3,2)*current_s1_node_coords(2) +&
             R(3,3)*current_s1_node_coords(3) + TT(3)

            lFOUND = .FALSE. ! Flag for current node I

            DO J = 1, nnod ! Loop through each node of surface 2 to find a match
              node_id_s2 = NODLIST2_used(J) ! Get the actual node ID from surface 2 list

              if(node_id_s2 > 0) then ! If this node from surface 2 has not been used yet (positive ID)
                ! Check if the transformed node from surface 1 matches this node from surface 2
                if(abs(transformed_coords(1) - X(1,node_id_s2)) < tol .and. &
                   abs(transformed_coords(2) - X(2,node_id_s2)) < tol .and. &
                   abs(transformed_coords(3) - X(3,node_id_s2)) < tol) then

                  NODLIST1_idx(I) = J       ! Store the index J (in NODLIST2_used) of the matching S2 node
                  NODLIST2_used(J) = -I     ! Mark this S2 node as used by S1 node I (by making it negative)
                  lFOUND = .TRUE.
                  exit ! Found a match for current S1 node, move to next S1 node
                endif
              endif
            END DO

            ! If no match was found for the current S1 node
            if(.NOT. lFOUND) then
              call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
               C2="SURFACE NODES DO NOT MATCH. Node from Surface 1 could not find a corresponding node on Surface 2.")
              all_nodes_found = .FALSE. ! Set global flag to indicate failure
              exit ! Exit the main loop over S1 nodes
            endif
          END DO

          if(.NOT. all_nodes_found) then
            ! Deallocate node-related temporary arrays if allocated before exiting
            if (allocated(NODLIST1_idx)) deallocate(NODLIST1_idx)
            if (allocated(NODLIST2_used)) deallocate(NODLIST2_used)
            if (allocated(NODLIST3_backup)) deallocate(NODLIST3_backup)
            return
          end if ! Return if any node failed to find a match

          ! --- Renumber list of nodes for surface 2 ---
          ! ebcs%node_list(NNOD+I) should contain the ID of the node on surface 2 that corresponds to ebcs%node_list(I)
          DO I = 1, nnod
            ! NODLIST1_idx(I) contains the index J in NODLIST2_used (and NODLIST3_backup)
            ! NODLIST3_backup(J) contains the original node ID of that matching node on surface 2
            ebcs%node_list(NNOD+I) = NODLIST3_backup(NODLIST1_idx(I))
          END DO

! ----------------------------------------------------------------------------------------------------------------------
          ! RENUMBER SEGMENT NODES AND ORDER SEGMENTS
! ----------------------------------------------------------------------------------------------------------------------

          ! Allocate backup arrays for surface 2 segments
          allocate(ITAG_ELEM(nseg))
          allocate(IFACE_backup(nseg))
          allocate(ISEG_backup(nseg))
          allocate(IELEM_backup(nseg))
          allocate(ELEM_LIST_backup(4, nseg)) ! Assuming 4 nodes per face for 3D, adjust if needed

          ITAG_ELEM(1:nseg) = 0 ! Initialize all segments of surface 2 as not yet matched

          ! Backup the original data for surface 2 segments
          ELEM_LIST_backup(1:4,1:nseg) = ebcs%elem_list(1:4,nseg+1:nseg+nseg)
          IFACE_backup(1:nseg) = ebcs%iface(nseg+1:nseg+nseg)
          ISEG_backup(1:nseg) = ebcs%iseg(nseg+1:nseg+nseg)
          IELEM_backup(1:nseg) = ebcs%ielem(nseg+1:nseg+nseg)

          ! --- Loop to match and reorder segments ---
          DO I = 1, nseg ! Loop through each segment of surface 1
            ! Get node IDs of current segment nodes from surface 1. Result in [1:nnod]
            node_id_s1 = ebcs%elem_list(1,I)
            node_id_s2 = ebcs%elem_list(2,I)
            node_id_s3 = ebcs%elem_list(3,I)
            node_id_s4 = ebcs%elem_list(4,I)

            ! Get the corresponding node IDs on surface 2
            s2_node_id1 = ebcs%node_list(NNOD + node_id_s1)
            s2_node_id2 = ebcs%node_list(NNOD + node_id_s2)
            s2_node_id3 = ebcs%node_list(NNOD + node_id_s3)
            s2_node_id4 = ebcs%node_list(NNOD + node_id_s4)

            lFOUND = .FALSE. ! Flag for current segment I

            DO J = 1, nseg ! Loop through each segment of surface 2 (from backup)
              if(ITAG_ELEM(J) == 0) then ! If this segment from surface 2 has not been matched yet
                ! Get node IDs of current segment from surface 2 backup
                inod1_s2 = NODLIST3_backup(ELEM_LIST_backup(1,J))
                inod2_s2 = NODLIST3_backup(ELEM_LIST_backup(2,J))
                inod3_s2 = NODLIST3_backup(ELEM_LIST_backup(3,J))
                inod4_s2 = NODLIST3_backup(ELEM_LIST_backup(4,J))

                ! Check if the set of node IDs from transformed S1 segment matches the set of node IDs from S2 segment
                if( ( (s2_node_id1 == inod1_s2 .or. s2_node_id1 == inod2_s2 .or. s2_node_id1 == inod3_s2 .or. &
                       s2_node_id1 == inod4_s2) .and. &
                      (s2_node_id2 == inod1_s2 .or. s2_node_id2 == inod2_s2 .or. s2_node_id2 == inod3_s2 .or. &
                       s2_node_id2 == inod4_s2) .and. &
                      (s2_node_id3 == inod1_s2 .or. s2_node_id3 == inod2_s2 .or. s2_node_id3 == inod3_s2 .or. &
                       s2_node_id3 == inod4_s2) .and. &
                      (s2_node_id4 == inod1_s2 .or. s2_node_id4 == inod2_s2 .or. s2_node_id4 == inod3_s2 .or. &
                      s2_node_id4 == inod4_s2) ) ) then

                    ITAG_ELEM(J) = 1 ! Mark this surface 2 segment as used
                    ! Copy the segment data from surface 2 backup to the reordered position
                    ebcs%elem_list(1:4,nseg+I) = ELEM_LIST_backup(1:4,J)
                    ebcs%iface(nseg+I) = IFACE_backup(J)
                    ebcs%iseg(nseg+I) = ISEG_backup(J)
                    ebcs%ielem(nseg+I) = IELEM_backup(J)
                    lFOUND = .TRUE.
                    exit ! Found a match for current S1 segment, move to next S1 segment
                end if
              end if
            END DO

            ! If no match was found for the current S1 segment
            if(.NOT. lFOUND) then
              call ancmsg(msgid=1602, msgtype=msgerror, anmode=aninfo, i1=ebcs_uid, c1=trim(title), &
               C2="SURFACE SEGMENTS DO NOT MATCH. Segment from Surface 1 could not find a corresponding segment on Surface 2.")
              all_nodes_found = .FALSE. ! Re-use this flag to indicate overall failure (segment matching)
              exit ! Exit the main loop over S1 segments
            endif
          END DO

          if (allocated(NODLIST1_idx)) deallocate(NODLIST1_idx)
          if (allocated(NODLIST2_used)) deallocate(NODLIST2_used)
          if (allocated(NODLIST3_backup)) deallocate(NODLIST3_backup)
          if (allocated(ITAG_ELEM)) deallocate(ITAG_ELEM)
          if (allocated(IFACE_backup)) deallocate(IFACE_backup)
          if (allocated(IELEM_backup)) deallocate(IELEM_backup)
          if (allocated(ELEM_LIST_backup)) deallocate(ELEM_LIST_backup)
          if (allocated(ISEG_backup)) deallocate(ISEG_backup)

        end subroutine ebcs_cyclic_surface_matching_3d
      end module ebcs_cyclic_surface_matching_3d_mod