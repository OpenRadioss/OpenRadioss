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
!||    inter1_check_ale_lag_sides_mod   ../starter/source/interfaces/int01/inter1_check_ale_lag_sides.F90
!||--- called by ------------------------------------------------------
!||    lecint                           ../starter/source/interfaces/interf1/lecint.F
!||====================================================================
      module inter1_check_ale_lag_sides_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief check ALE and lagrangian surfaces
!! \details ALE must be based on ALE nodes, lagrangian surface must be based on lagrangian nodes.
!! \details it may happen that user defines EULER framework instead of ALE one, this is also checked.
!||====================================================================
!||    inter1_check_ale_lag_sides   ../starter/source/interfaces/int01/inter1_check_ale_lag_sides.F90
!||--- called by ------------------------------------------------------
!||    lecint                       ../starter/source/interfaces/interf1/lecint.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                       ../starter/source/output/message/message.F
!||    normalize_shape              ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||    sort_shape                   ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||--- uses       -----------------------------------------------------
!||    inter1_seg_utils_mod         ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||    message_mod                  ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine inter1_check_ale_lag_sides(n2d, surf_uid1,surf_uid2, inter_uid, title,  &
          numnod, itab, nseg1, nseg2, irect1, irect2, nale, iddlevel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use names_and_titles_mod , only : nchartitle
          use message_mod
          use inter1_seg_utils_mod , only : sort_shape, normalize_shape
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                    intent(in) :: n2d                !< analysis flag 0:3d  1:2d_axi  2:2d_plane
          integer,                    intent(in) :: inter_uid          !< interface user id
          integer,                    intent(in) :: surf_uid1          !< surface user id
          integer,                    intent(in) :: surf_uid2          !< surface user id
          character(len=nchartitle),  intent(in) :: title              !< interface title
          integer,                    intent(in) :: numnod             !< number of nodes in input file
          integer,                    intent(in) :: itab(numnod)       !< user identifiers for nodes
          integer,                    intent(inout) :: nseg1           !< number of segments list 1
          integer,                    intent(inout) :: nseg2           !< number of segments list 1
          integer,                    intent(inout) :: irect1(4,nseg1) !< internal identifiers for each segment
          integer,                    intent(inout) :: irect2(4,nseg2) !< internal identifiers for each segment
          integer,                    intent(in) :: nale(numnod)       !< tag to detect ale/lag nodes  (0:lagrange, 1:ale, 2:euler)
          integer,                    intent(in) :: iddlevel           !< pass number : 0 or 1
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii,jj
          integer :: iseg               !< segment loop
          integer :: node_iid(4)        !< internal node identifiers
          integer :: node_uid(4)        !< user node identifiers
          integer :: nale_sum           !< calculated sum
          integer :: npt                !< number of points composing a segment (3d:4, 2d:2)
          CHARACTER(len=10),parameter :: chain1 = "ALE       "
          CHARACTER(len=10),parameter :: chain2 = "LAGRANGIAN"
          character(len=10) :: expected_type   !< error message management
          character(len=10) :: unexpected_type !< error message management
          logical :: is_valid                  !< segment has unexpected ALE node or unexpected LAG nodes
          logical :: is_reversed
          logical :: is_euler_detected         !< .true. if EULER framework defined instead of ALE one.
          logical :: found_duplicate
          integer, allocatable,dimension(:,:) :: norm1,norm2 !< normalized segments (same order N1 N2 N3 N4)
          integer, allocatable,dimension(:,:) :: key1,key2 !< sorted segments
          integer :: valid_nseg1, valid_nseg2
          integer :: nduplicate
! ----------------------------------------------------------------------------------------------------------------------
!                                                   P r e - C o n d i t i o n s
! ----------------------------------------------------------------------------------------------------------------------
          IF(numnod <= 0) return  !no node in input file
          if(nseg1 <= 0) return  !empty ALE surface
          if(nseg2 <= 0) return  !empty LAG surface
          if(iddlevel == 0) return !single check is enough (before domain decomposition)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          npt=4
          if(n2d > 0) npt=2

          !remove duplicated segments
          allocate(norm1(4, nseg1), norm2(4, nseg2))
          allocate(key1(4, nseg1), key2(4, nseg2))
          do ii = 1, nseg1
            call sort_shape(IRECT1(1,ii), key1(1,ii))
            call normalize_shape(IRECT1(1,ii), norm1(1,ii))
          end do
          do ii = 1, nseg2
            call sort_shape(IRECT2(1,ii), key2(1,ii))
            call normalize_shape(IRECT2(1,ii), norm2(1,ii))
          end do

          ! compare and resize original arrays IRECT1 and IRECT2
          ! IRECT1 is ALE side
          ! IRECT2 is LAG side
          valid_nseg1 = 0
          nduplicate = 0
          do ii = 1, nseg1
            found_duplicate = .false.
            do jj = 1, nseg2
              if (all(key1(:,ii) == key2(:,jj))) then
                if (all(norm1(:,ii) == norm2(:,jj))) then
                  found_duplicate = .true.
                  exit
                end if
              end if
            end do
            if (found_duplicate) then
              !print '(4I10)', norm1(:,ii)
              nduplicate = nduplicate + 1
            else
              valid_nseg1 = valid_nseg1 + 1
              IRECT1(:,valid_nseg1) = IRECT1(:,ii)
            end if
          end do

          ! if the number of segments has been reduced, we need to update nseg1 and warn the user there were merged segments
          if(nseg1 /= valid_nseg1)then
            nseg1 = valid_nseg1
            call ancmsg(msgid=136,msgtype=msgwarning,anmode=aninfo, i1=inter_uid,c1=title, i2=nduplicate)
          end if

          valid_nseg2 = 0
          do ii = 1, nseg2
            found_duplicate = .false.
            do jj = 1, nseg1
              if (all(key2(:,ii) == key1(:,jj))) then
                if (all(norm2(:,ii) == norm1(:,jj))) then
                  found_duplicate = .true.
                  exit
                end if
              end if
            end do
            if (found_duplicate) then
              !print '(4I10)', norm2(:,ii)
            else
              valid_nseg2 = valid_nseg2 + 1
              IRECT2(:,valid_nseg2) = IRECT2(:,ii)
            end if
          end do
          nseg2 = valid_nseg2

          ! checking an ALE surface
          expected_type = chain1
          unexpected_type = chain2
          is_valid = .true.
          is_reversed=.false.
          is_euler_detected = .false.
          do iseg=1,nseg1
            ! internal node identifiers
            node_iid(1:4) = irect1(1:4,iseg)
            ! user node identifiers
            node_uid(1:4) = 0 ; node_uid(1:npt) =  itab(irect1(1:npt,iseg))
            ! nale_sum = 0+0+0+0 when checking a segment on LAGRANGIAN surface
            ! nale_sum = 1+1+1+1 when checking a segment ALE surface
            ! nale_sum = 2+2+2+2 when EULER surface provided instead of ALE surface
            nale_sum=sum(abs(nale(node_iid(1:npt))))   !warning : law151 has negative nale to distinguish collocated scheme from staggered scheme
            if(nale_sum /= npt)then
              is_valid = .false.
              if(nale_sum == 2*npt)then
                ! found EULER instead of ALE when checking ALE side
                call ancmsg(msgid=945,msgtype=msgerror,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid1, &
                  i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
                  c2="ALE")
                return
              end if
              if(nale_sum == 0)then
                !     found full ALE segment when checking LAG surface
                ! or  found full LAG segment when checking ALE surface
                call ancmsg(msgid=946,msgtype=msgerror,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid1, &
                  i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
                  c2=expected_type,c3=unexpected_type,c4=expected_type)
              end if
              exit
            end if
          end do

          if(.not. is_valid .and. .not.is_reversed)then
            ! or  found LAG node when checking ALE surface
            call ancmsg(msgid=4,msgtype=msgwarning,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid1, &
              i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
              c2=expected_type,c3=expected_type)
          end if


          !checking LAGRANGIAN SURFACE
          ! request for checking a Lagrangian surface
          expected_type = chain2
          unexpected_type = chain1
          is_valid = .true.
          is_reversed=.false.
          is_euler_detected = .false.
          do iseg=1,nseg2
            ! internal node identifiers
            node_iid(1:4) = irect2(1:4,iseg)
            ! user node identifiers
            node_uid(1:4) = 0 ; node_uid(1:npt) =  itab(irect2(1:npt,iseg))
            ! nale_sum = 0+0+0+0 when checking a segment on LAGRANGIAN surface
            ! nale_sum = 1+1+1+1 when checking a segment ALE surface
            ! nale_sum = 2+2+2+2 when EULER surface provided instead of ALE surface
            nale_sum=sum(abs(nale(node_iid(1:npt))))   !warning : law151 has negative nale to distinguish collocated scheme from staggered scheme
            if(nale_sum == npt)then
              !     found full ALE segment when checking LAG surface
              call ancmsg(msgid=946,msgtype=msgerror,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid1, &
                i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
                c2=expected_type,c3=unexpected_type,c4=expected_type)
            end if
            if(nale_sum == 2*npt)then
              ! found EULER instead of LAG when checking LAG side
              call ancmsg(msgid=945,msgtype=msgerror,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid1, &
                i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
                c2="LAGRANGIAN")
              return
            end if
            if(nale_sum /= 0)then
              is_valid = .false.
              exit
            end if
          end do

          if(.not. is_valid .and. .not.is_reversed)then
            !     found ALE node when checking LAG surface
            call ancmsg(msgid=4,msgtype=msgwarning,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid2, &
              i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
              c2=expected_type,c3=expected_type)
          end if

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine inter1_check_ale_lag_sides
      end module inter1_check_ale_lag_sides_mod
