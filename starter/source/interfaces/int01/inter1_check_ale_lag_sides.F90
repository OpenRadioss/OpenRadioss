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
      !||--- uses       -----------------------------------------------------
      !||    message_mod                  ../starter/share/message_module/message_mod.F
      !||====================================================================
        subroutine inter1_check_ale_lag_sides(n2d, surf_uid, inter_uid, title,  &
                                              numnod, itab, nseg, irect, nale, action_key, iddlevel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use names_and_titles_mod , only : nchartitle
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: n2d           !< analysis flag 0:3d  1:2d_axi  2:2d_plane
          integer,                                   intent(in) :: inter_uid     !< interface user id
          integer,                                   intent(in) :: surf_uid      !< surface user id
          character(len=nchartitle),                 intent(in) :: title         !< interface title
          integer,                                   intent(in) :: numnod        !< number of nodes in input file
          integer,                                   intent(in) :: itab(numnod)  !< user identifiers for nodes
          integer,                                   intent(in) :: nseg          !< number of segments
          integer,                                   intent(in) :: irect(4,nseg) !< internal identifiers for each segment
          integer,                                   intent(in) :: nale(numnod)  !< tag to detect ale/lag nodes  (0:lagrange, 1:ale, 2:euler)
          character(len=3),                          intent(in) :: action_key    !< key determining if we check 'ale' or 'lag' surface
          integer,                                   intent(in) :: iddlevel      !< pass number : 0 or 1
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: iseg               !< segment loop
          integer :: node_iid(4)        !< intenal node identifiers
          integer :: node_uid(4)        !< user node identifiers
          integer :: nale_expected      !< when checking 'ALE' side nale_expected is 1, and 0 for 'LAG' side
          integer :: nale_sum_expected  !< sum of nale values for each node composing the segment
          integer :: nale_sum           !< calculated sum
          integer :: npt                !< number of points composing a segment (3d:4, 2d:2)
          CHARACTER(len=10),parameter :: chain1 = 'ALE       '
          CHARACTER(len=10),parameter :: chain2 = 'LAGRANGIAN'
          character(len=10) :: expected_type   !< error message management
          character(len=10) :: unexpected_type !< error message management
          logical :: is_valid                  !< segment has unexpected ALE node or unexpected LAG nodes
          logical :: is_euler_detected         !< .true. if EULER framework defined instead of ALE one.
! ----------------------------------------------------------------------------------------------------------------------
!                                                   P r e - C o n d i t i o n s
! ----------------------------------------------------------------------------------------------------------------------
        IF(numnod <= 0)RETURN  !no node in input file
        if(nseg <= 0)RETURN  !empty surface
        IF(ACTION_KEY /= 'ALE' .AND. action_key /= 'LAG')return  ! action key not expected
        if(iddlevel ==1) return !single check is enough (before domain decomposition)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        is_valid = .true.
        is_euler_detected = .false.
        npt=4
        if(n2d > 0) npt=2

        select case (action_key)
          case('LAG')
            ! request for checking a Lagrangian surface
            nale_expected = 0
            nale_sum_expected = 0
            expected_type = chain2
            unexpected_type = chain1
          case('ALE')
            ! request for checking an ALE surface
            nale_expected = 1
            nale_sum_expected = npt
            expected_type = chain1
            unexpected_type = chain2
        end select

        do iseg=1,nseg
          ! internal node identifiers
          node_iid(1:4) = irect(1:4,iseg)
          ! user node identifiers
          node_uid(1:4) = 0 ; node_uid(1:npt) =  itab(irect(1:npt,iseg))
          ! nale_sum = 0+0+0+0 when checking a segment on LAGRANGIAN surface
          ! nale_sum = 1+1+1+1 when checking a segment ALE surface
          ! nale_sum = 2+2+2+2 when EULER surface provided instead of ALE surface
          nale_sum=sum(iabs(nale(node_iid(1:npt))))   !warning : law151 has negative nale to distinguish collocated scheme from staggered scheme
          if(nale_sum /= nale_sum_expected)then
            is_valid = .false.
            if(nale_expected == 1 .and. nale_sum == 2*nale_sum_expected)is_euler_detected=.true.  !if you expect ALE but find EULER
            exit
          endif
        enddo

        if(is_euler_detected .and. action_key=='ALE')then
           ! found EULER instead of ALE when checking ALE side
           call ancmsg(msgid=945,msgtype=msgerror,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid, &
                       i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
                       c2='ALE')
           return
        endif

        if(.not. is_valid)then
           !     found ALE node when checking LAG surface
           ! or  found LAG node when checking ALE surface
           call ancmsg(msgid=4,msgtype=msgerror,anmode=aninfo, i1=inter_uid,c1=title, i2=1, i3=surf_uid, &
                       i4=node_uid(1),i5=node_uid(2),i6=node_uid(3),i7=node_uid(4), &
                       c2=expected_type,c3=expected_type,c4=unexpected_type)
        endif

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine inter1_check_ale_lag_sides
