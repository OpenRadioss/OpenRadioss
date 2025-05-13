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
      !||    polygon_clipping_mod      ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||--- called by ------------------------------------------------------
      !||    init_inivol_2d_polygons   ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||--- uses       -----------------------------------------------------
      !||    polygon_mod               ../common_source/tools/clipping/polygon_mod.F90
      !||====================================================================
      module polygon_clipping_mod
        use polygon_mod
        implicit none
#include  "my_real.inc"

          !pointer to a list of data structure below
          type pointer_to_point_
            integer id_edge  ! 1 or 2
            integer id_point ! from 1 to n where n= #endpoints + #intersection_points    
          end type pointer_to_point_

          !data structure for Weiler Atherton algorithm
          type points_on_edge_
            my_real, allocatable, dimension(:) :: alpha !position on edge 0.0:first endpoint, ]0.,1.[:intersection point, 1.0:second edge endpoint
            type(polygon_point_), allocatable, dimension(:) :: coor ! coordinates in global frame
            integer,dimension(:),allocatable :: point_id !identifier for intersection point (same id for Clipped polygon and Clipping polygon)
            integer :: numpoints !number of points on the edge : min is 2 for both edge endpoints
            integer, allocatable, dimension(:) :: iorient
            integer, allocatable, dimension(:) :: num_inter_pt
            type(pointer_to_point_),allocatable,dimension(:) :: ptr
           end type points_on_edge_

      contains


! ======================================================================================================================
!                                                   FUNCTION
! ======================================================================================================================
  function intersectPt(P1, P2, Q1, Q2, tol, alpha, beta) result(intersection)
!! \brief Compute intersection point [P1,P2[ and {Q1,Q2[
!! \details On [P1 P2[ : position is alpha in \[0,1[ and On [Q1 Q2[ : position is beta in \[0,1[
   use constant_mod , only : zero, one, ep20
    implicit none
#include "my_real.inc"
    type(polygon_point_), intent(in) :: P1, P2, Q1, Q2
    my_real, intent(inout) :: alpha, beta
    my_real, intent(in) :: tol
    type(polygon_point_) :: intersection
    my_real :: denom, numer_a, numer_b

    intersection%y = ep20  ! Initialize with NaN (or use other convention)
    intersection%z = ep20
    alpha = -one
    beta = -one

    denom = (Q2%z - Q1%z) * (P2%y - P1%y) - (Q2%y - Q1%y) * (P2%z - P1%z)

    if (abs(denom) < tol) then
      return  ! Segments are parallel or coincident
    end if

    numer_a = (Q2%y - Q1%y) * (P1%z - Q1%z) - (Q2%z - Q1%z) * (P1%y - Q1%y)

    numer_b = (P2%y - P1%y) * (P1%z - Q1%z) - (P2%z - P1%z) * (P1%y - Q1%y)

    alpha = numer_a / denom
    beta = numer_b / denom

    if (alpha >= zero .and. alpha <= one .and. beta >= zero .and. beta <= one) then
      intersection%y = P1%y + alpha * (P2%y - P1%y)
      intersection%z = P1%z + alpha * (P2%z - P1%z)
    end if

  end function intersectPt

! ======================================================================================================================
!                                                   FUNCTION
! ======================================================================================================================
  function GetEdgeFromPointId (List_Edge, point_id, list_size, out_point_pos) result(out_edge_pos)
    !output : ii : edge number in the list
    !output : jj : point number on this list
    implicit none
    integer,intent(in) :: list_size
    integer,intent(in) :: point_id
    integer,intent(inout) :: out_point_pos
    integer out_edge_pos
    type(points_on_edge_), dimension(list_size) :: List_Edge
    integer ii, jj
    logical is_found
    is_found = .false.
    out_edge_pos = -HUGE(out_edge_pos)
    do ii=1,list_size
      do jj=2,List_Edge(ii)%numpoints-1
        if(List_Edge(ii)%point_id(jj) == point_id)then
          is_found = .true.
          exit
        end if
      end do
      if(is_found)then
        out_edge_pos = ii
        out_point_pos = jj
        exit
      end if
    end do
  end function GetEdgeFromPointId


! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
      !||====================================================================
      !||    nextpoint                  ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||--- called by ------------------------------------------------------
      !||    clipping_weiler_atherton   ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||====================================================================
    subroutine NextPoint ( currentPoint, icur_list, list1, size1, list2, size2)
      implicit none
      type(pointer_to_point_), intent(inout) :: currentPoint
      integer,intent(inout) :: icur_list !< current list : 1 or 2
      integer,intent(in) :: size1
      integer,intent(in) :: size2
      type(points_on_edge_), target, dimension(size1) :: list1
      type(points_on_edge_), target, dimension(size2) :: list2
      integer ii , jj !< edge local id
      integer kk !< point local id
      integer iorient !< flag for entering (1) or leaving point (-1) or summit (0)
      integer :: num_pt_on_edge
      integer :: size_ !size1 or size2 depending on icur_list=1|2
      !!!!type(points_on_edge_), dimension(:), pointer :: list

      ii = currentPoint%id_edge
      kk = currentPoint%id_point
      num_pt_on_edge = 2
      size_ = 0
      iorient = -1
      if(icur_list == 1)then
        size_ = size1
        iorient = list1(ii)%iorient(kk)
      elseif(icur_list == 2)then
        size_ = size2
        iorient = list2(ii)%iorient(kk)
      endif

      !if we had to remain on current list

      if(iorient /=-1)then  ! if not a leaving point, remain on current list
        if(icur_list==1)num_pt_on_edge = 2 + list1(ii)%num_inter_pt(kk)
        if(icur_list==2)num_pt_on_edge = 2 + list2(ii)%num_inter_pt(kk)
        if(kk < num_pt_on_edge)then
          !next point on current edge
          kk = kk + 1
        elseif(ii < size_)then
          !first point on next edge
          ii = ii + 1
          kk = 1
        else
          !rewind to first point of first edge
          ii = 1
          kk = 1
        end if
        jj = ii

      else
        !leaving point : move to the other list
        if(icur_list == 1)then
          jj = list1(ii)%ptr(kk)%id_edge;
          kk = list1(ii)%ptr(kk)%id_point;
        else
          jj = list2(ii)%ptr(kk)%id_edge;
          kk = list2(ii)%ptr(kk)%id_point;
        end if

        icur_list = 3 - icur_list  ! switch : 1 becomes 2 and 2 becomes 1

        if(icur_list == 1)then
          size_ = size1
          num_pt_on_edge = 2 + list1(jj)%num_inter_pt(kk) ! then increment to next point (do not stay on same point)
        elseif(icur_list == 2)then
          size_ = size2
          num_pt_on_edge = 2 + list2(jj)%num_inter_pt(kk) ! then increment to next point (do not stay on same point)
        end if

        if(kk < num_pt_on_edge)then
          !next point on current edge
          kk = kk + 1
          ! last point is first point on next edge
          if(kk == num_pt_on_edge)then
            kk = 1
            jj = jj +1
            if (jj > size_)jj=1
          end if
        elseif(ii < size_)then
          !first point on next edge
          jj = jj + 1
          kk = 1
        else
          !rewind to first point of first edge
          jj = 1
          kk = 1
        end if

       endif

      currentPoint%id_edge = jj
      currentPoint%id_point = kk

    end subroutine NextPoint


! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
! --- POLYGONAL CLIPPING WITH WEILER-ATHERTON ALGORITHM
! pre-condition : no self intersection
! details : this subroutine is used for 2D /INIVOL option. Clipped polygon is user polygon. Clipping polygon is elem from user mesh
      !||====================================================================
      !||    clipping_weiler_atherton         ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||--- called by ------------------------------------------------------
      !||    init_inivol_2d_polygons          ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||--- calls      -----------------------------------------------------
      !||    integer_array_reindex            ../common_source/tools/sort/array_reindex.F90
      !||    nextpoint                        ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||    points_array_reindex             ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||    polygon_addpoint                 ../common_source/tools/clipping/polygon_mod.F90
      !||    polygon_create                   ../common_source/tools/clipping/polygon_mod.F90
      !||    real_insertion_sort_with_index   ../common_source/tools/sort/insertion_sort.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                     ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine Clipping_Weiler_Atherton(ClippedPolygon, ClippingPolygon, result_list, iStatus)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero, em10, em06, one, ep20
          use insertion_sort_mod , only : real_insertion_sort_with_index
          use array_reindex_mod , only : integer_array_reindex
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_), intent(in) :: ClippedPolygon      !< Clipped Polygon
          type(polygon_), intent(in) :: ClippingPolygon     !< Clipping Polygon
          type(polygon_list_), intent(out) :: result_list   !< potentially more than 1 polygon (in case of non convex polygons)
          integer,intent(inout) :: iStatus                  !< return code for algorithm status
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: num_points_1, num_points_2
          integer :: num_edges_1, num_edges_2
          integer :: num_polygons
          integer :: ii, jj, kk, ll, kk_bis
          integer :: numPtmax
          integer :: numEnteringPoints1 ! Number of Entering points on list 1 (Clipped Polygon)
          integer :: numLeavingPoints1,numLeavingPoints2 !! Number of Leaving points on list 1 (Clipped) and 2 (Clipping)
          integer :: icur_1 !< cursor position for loop on Clipped Polygon
          integer :: icur_list !< current cursor position (list 1 or 2)
          integer,allocatable,dimension(:) :: icur_2  !< cursor position for loop on Clipping Polygon
          integer,allocatable,dimension(:) :: index   !< bijection array for sorting algorithm
          integer ierr !< error code to check if something went wrong
          integer :: point_id !< to identify common intersection points on both Clipping and Clipped polygons
          integer :: ipoly !< current polygon (result is a list)
          integer :: starting_point_gid !< starting point must be saved to stop algorithm when polygon is closed
          integer :: current_point_gid !< global identifier to make relation on identical intersections on both lists (same intersection points on Clipped and Clipping polygons)
          integer :: total_int_pt !< total number of intersection points
          integer :: counter_entering_point
          integer :: total_number_poly

          my_real :: tol   !< tolerance
          my_real :: Ny,Nz !< normal vector (used to defined entering or leaving intersection point)
          my_real :: Vy,Vz !< temporary array
          my_real :: alpha !< alpha \in ]0.,1.[ is position on edge1 (clipped polygon)
          my_real :: beta  !< beta \in ]0.,1.[ is position on edge2 (clipping polygon)
          my_real :: dotproduct
          logical :: finished
          logical :: vertice_on_edge

          type(polygon_point_) ::  p1,p2, q1,q2, tmpPoint                  !< temporary points
          type(points_on_edge_), dimension(:), allocatable :: list_edges_1 !clipped polygon (elem mesh)
          type(points_on_edge_), dimension(:), allocatable :: list_edges_2 !clipping polygon (user mesh)
          type(pointer_to_point_) :: starting_point
          type(pointer_to_point_) :: current_point
          type(pointer_to_point_), dimension(:), allocatable :: pointer_entering_points_1  !data structure used to mark all entering points on list 1
          type(pointer_to_point_), dimension(:), allocatable :: pointer_leaving_points_1   !data structure used to mark all leaving points on list 1
          type(pointer_to_point_), dimension(:), allocatable :: pointer_leaving_points_2   !data structure used to mark all leaving points on list 2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          num_polygons = 0
          num_points_1 = ClippedPolygon%numpoint
          num_points_2 = ClippingPolygon%numpoint
          num_edges_1 = num_points_1 - 1
          num_edges_2 = num_points_2 - 1
          numPtmax = num_points_1*num_points_2
          point_id = 0
          iStatus = 0
          kk_bis = 0

          ! init. list 1 (Clipped polygon)
          allocate(list_edges_1(num_edges_1))
          do ii=1,num_edges_1
            allocate(list_edges_1(ii)%alpha(2+num_edges_2)) ; list_edges_1(ii)%alpha(:) = zero
            allocate(list_edges_1(ii)%coor(2+num_edges_2))
            allocate(list_edges_1(ii)%point_id(2+num_edges_2)) ; list_edges_1(ii)%point_id(:) = 0
            allocate(list_edges_1(ii)%iorient(2+num_edges_2)) ; list_edges_1(ii)%iorient(:) = 0
            allocate(list_edges_1(ii)%num_inter_pt(2+num_edges_2)) ; list_edges_1(ii)%num_inter_pt(:) = 0
            allocate(list_edges_1(ii)%ptr(2+num_edges_2)) ;
              list_edges_1(ii)%ptr(:)%id_edge=0;
              list_edges_1(ii)%ptr(:)%id_point=0;
              list_edges_1(ii)%numpoints = 0
          end do

          ! init. list 2 (Clipping polygon)
          allocate(list_edges_2(num_edges_2))
          do jj=1,num_edges_2
            allocate(list_edges_2(jj)%alpha(2+num_edges_1)) ; list_edges_2(jj)%alpha(:) = zero
            allocate(list_edges_2(jj)%coor(2+num_edges_1))
            allocate(list_edges_2(jj)%point_id(2+num_edges_1)) ; list_edges_2(jj)%point_id(:) = 0
            allocate(list_edges_2(jj)%iorient(2+num_edges_1)) ; list_edges_2(jj)%iorient(:) = 0
            allocate(list_edges_2(jj)%num_inter_pt(2+num_edges_1)) ; list_edges_2(jj)%num_inter_pt(:) = 0
            allocate(list_edges_2(jj)%ptr(2+num_edges_1)) ;
              list_edges_2(jj)%ptr(:)%id_edge=0;
              list_edges_2(jj)%ptr(:)%id_point=0;
              list_edges_2(jj)%numpoints = 0
          end do

          allocate(icur_2(num_edges_2)) ! cursor for each edge of current elem

          !define CLipping polygon
          do jj=1,num_edges_2
            icur_2(jj) = 1
            list_edges_2(jj)%alpha(1) = zero
            list_edges_2(jj)%coor(1)%y = ClippingPolygon%point(jj)%y
            list_edges_2(jj)%coor(1)%z = ClippingPolygon%point(jj)%z
          end do

          total_int_pt = 0
          vertice_on_edge = .false.

          do ii=1,num_edges_1
            !Clipped polygon
            p1%y = ClippedPolygon%point(ii)%y
            p1%z = ClippedPolygon%point(ii)%z
            p2%y = ClippedPolygon%point(ii+1)%y
            p2%z = ClippedPolygon%point(ii+1)%z
            tol = EM06*ClippedPolygon%diag

            !ADDING FIRST ENDPOINT ON list_edges_1 (Clipped)
            icur_1 = 1
            list_edges_1(ii)%alpha(1)=zero ;
            list_edges_1(ii)%coor(1)%y = p1%y;
            list_edges_1(ii)%coor(1)%z = p1%z;

            do jj=1,num_edges_2
              !Clipping polygon
              q1%y = ClippingPolygon%point(jj)%y
              q1%z = ClippingPolygon%point(jj)%z
              q2%y = ClippingPolygon%point(jj+1)%y
              q2%z = ClippingPolygon%point(jj+1)%z

              ! pre-criterion : if both edge has boxes without intersection then skip
              if( max(p1%y,p2%y)+tol < min(q1%y,q2%y) ) cycle
              if( max(q1%y,q2%y)+tol < min(p1%y,p2%y) ) cycle
              if( max(p1%z,p2%z)+tol < min(q1%z,q2%z) ) cycle
              if( max(q1%z,q2%z)+tol < min(p1%z,p2%z) ) cycle
              ! criterion : alpha in ]0,1[ in case of intersection
              tmpPoint = intersectPt(p1,p2,q1,q2,em06, alpha, beta) ! [p1,p2] & [q1,q2]
              if(tmpPoint%y == ep20 .or. tmpPoint%z == ep20)then
                cycle
              end if

              if(alpha < em10 .or. beta < em10 .or. alpha > one-em10 .or. beta > one-em10)then
                 vertice_on_edge = .true.
                 exit
              end if

              total_int_pt = total_int_pt + 1 !numbering to affect a global identifier and make relation between the two lists

              !ADDING INTERSECTION POINT ON list_edges_1
              icur_1 = icur_1 + 1 ; list_edges_1(ii)%alpha(icur_1) = alpha ;  !alpha \in ]0,1[ : position on edge
              !storing intersection point
              list_edges_1(ii)%coor(icur_1)%y = tmpPoint%y
              list_edges_1(ii)%coor(icur_1)%z = tmpPoint%z
              list_edges_1(ii)%point_id(icur_1) = total_int_pt

              !ADDING INTERSECTION POINT ON list_edges_2
              icur_2(jj) = icur_2(jj) + 1 ; list_edges_2(jj)%alpha(icur_2(jj))=beta ;
              list_edges_2(jj)%coor(icur_2(jj))%y = tmpPoint%y;
              list_edges_2(jj)%coor(icur_2(jj))%z = tmpPoint%z;
              list_edges_2(jj)%point_id(icur_2(jj)) = total_int_pt;

              !entering or leaving point ? (p1p2 point of view : along p1p2)
              !normal vector is vector q1q2 rotation +90°
              Ny = -(q2%z - q1%z)
              Nz = +(q2%y - q1%y)
              !vector V is p1-> p2
              Vy = p2%y - p1%y
              Vz = p2%z - p1%z
              dotproduct = Ny * Vy + Nz * Vz
              list_edges_1(ii)%num_inter_pt  = list_edges_1(ii)%num_inter_pt + 1
              if (dotproduct > zero)then
                ! "entering point";
                list_edges_1(ii)%iorient(icur_1) = +1
              else
                ! "leaving point";
                list_edges_1(ii)%iorient(icur_1) = -1
              end if

              !entering or leaving point ?  (from q1q2 point of view : along q1q2)
              !normal vector is vector p1p2 rotation +90°
              Ny = -(p2%z - p1%z)
              Nz = +(p2%y - p1%y)
              !vector V is q1-> q2
              Vy = q2%y - q1%y
              Vz = q2%z - q1%z
              dotproduct = Ny * Vy + Nz * Vz
              list_edges_2(jj)%num_inter_pt  = list_edges_2(jj)%num_inter_pt + 1
              if (dotproduct > zero)then
                ! "entering point";
                list_edges_2(jj)%iorient((icur_2(jj))) = +1
              else
                ! "leaving point";
                list_edges_2(jj)%iorient((icur_2(jj))) = -1
              end if

            end do

            if(vertice_on_edge)exit
            ! ADDING LAST ENDPOINT ON list_edges_1 (Clipped)
            icur_1 = icur_1 + 1 ;
            list_edges_1(ii)%alpha(icur_1) = one ;  !last point = endpoints
            list_edges_1(ii)%coor(icur_1)%y = p2%y;
            list_edges_1(ii)%coor(icur_1)%z = p2%z;
            ! NUMBER OF POINT ON list_edges_1
            list_edges_1(ii)%numpoints = icur_1;   !numpoints = 2 endpoints + intersection points
          end do

          if(vertice_on_edge)then
            iStatus=-1
            return
          end if

          ! ADDING LAST ENDPOINT ON list_edges_2 (Clipping)
          do jj=1,num_edges_2
            icur_2(jj) = icur_2(jj) + 1 ;
            list_edges_2(jj)%alpha(icur_2(jj)) = one ;  !last point = endpoint
            list_edges_2(jj)%coor((icur_2(jj)))%y = ClippingPolygon%point(jj+1)%y;
            list_edges_2(jj)%coor((icur_2(jj)))%z = ClippingPolygon%point(jj+1)%z;
            ! NUMBER OF POINT ON list_edges_1
            list_edges_2(jj)%numpoints = icur_2(jj);   !numpoints = 2 endpoints + intersection points
          end do

          !sorting intersection points on list_edges_1 (Clipped)
          !  several intersection points might be found on a given edge, they must be ordered
          allocate(index(num_edges_2))
          do ii=1,num_edges_1
            if (list_edges_1(ii)%numpoints > 3) then   !at least 2 points : segment endpoints ;
              call real_insertion_sort_with_index(list_edges_1(ii)%alpha, index, list_edges_1(ii)%numpoints)
              !reindex coordinates
              call points_array_reindex(list_edges_1(ii)%coor, index, list_edges_1(ii)%numpoints)
              call integer_array_reindex(list_edges_1(ii)%iorient, index, list_edges_1(ii)%numpoints)
              call integer_array_reindex(list_edges_1(ii)%point_id, index, list_edges_1(ii)%numpoints)
            end if
          end do
          deallocate(index)

          !sorting intersection points on list_edges_1 (CLipping)
          !  several intersection points might be found on a given edge, they must be ordered
          allocate(index(num_edges_2))
          do jj=1,num_edges_2
            if(list_edges_2(jj)%numpoints > 3)then   !at least 2 points : segment endpoints ;
              call real_insertion_sort_with_index(list_edges_2(jj)%alpha, index, list_edges_2(jj)%numpoints)
              !reindex coordinates
              call points_array_reindex(list_edges_2(jj)%coor, index, list_edges_2(jj)%numpoints)
              call integer_array_reindex(list_edges_2(jj)%iorient, index, list_edges_2(jj)%numpoints)
              call integer_array_reindex(list_edges_2(jj)%point_id, index, list_edges_2(jj)%numpoints)
            end if
          end do
          deallocate(index)


          ! mark all entring point on list 1
          allocate(pointer_entering_points_1(numPtmax))
          numEnteringPoints1 = 0
          do ii=1,num_edges_1
            !find first intersection points which is entering
            if (list_edges_1(ii)%numpoints == 2) cycle ! numpoints = 2 => 2 endpoints only
            do kk=2,list_edges_1(ii)%numpoints-1 !num_inter_pt : intersection points without endpoints
              if(list_edges_1(ii)%iorient(kk) /= 1)cycle !first point is first vertice (first endpoint) : not an intersection point
              numEnteringPoints1 = numEnteringPoints1 + 1
              pointer_entering_points_1(numEnteringPoints1)%id_edge = ii
              pointer_entering_points_1(numEnteringPoints1)%id_point = kk
            end do
          end do

          ! mark all leaving point on list 1
          allocate(pointer_leaving_points_1(numPtmax))
          numLeavingPoints1 = 0
          do ii=1,num_edges_1
            !find first intersection points which is leaving
            if (list_edges_1(ii)%numpoints == 2) cycle ! numpoints = 2 => 2 endpoints only
            do kk=2,list_edges_1(ii)%numpoints-1 !num_inter_pt : intersection points without endpoints
              if(list_edges_1(ii)%iorient(kk) /= -1)cycle !first point is first vertice (first endpoint) : not an intersection point
              numLeavingPoints1 = numLeavingPoints1 + 1
              pointer_leaving_points_1(numLeavingPoints1)%id_edge = ii
              pointer_leaving_points_1(numLeavingPoints1)%id_point = kk
            end do
          end do

          ! mark all leaving point on list 2
          allocate(pointer_leaving_points_2(numPtmax))
          numLeavingPoints2 = 0
          do jj=1,num_edges_2
            !find first intersection points which is leaving
            if (list_edges_2(jj)%numpoints == 2) cycle ! numpoints = 2 => 2 endpoints only
            do kk=2,list_edges_2(jj)%numpoints-1 !num_inter_pt : intersection points without endpoints
              if(list_edges_2(jj)%iorient(kk) /= -1)cycle !first point is first vertice (first endpoint) : not an intersection point
              numLeavingPoints2 = numLeavingPoints2 + 1
              pointer_leaving_points_2(numLeavingPoints2)%id_edge = jj
              pointer_leaving_points_2(numLeavingPoints2)%id_point = kk
            end do
          end do

          !Set pointer from list 1 (entering points) to list 2 (leaving points)
          do ll=1,numEnteringPoints1
            ii = pointer_entering_points_1(ll)%id_edge
            kk = pointer_entering_points_1(ll)%id_point
            point_id = list_edges_1(ii)%point_id(kk)
            jj = GetEdgeFromPointId (list_edges_2, point_id, num_edges_2, kk_bis)
            list_edges_1(ii)%ptr(kk)%id_edge = jj
            list_edges_1(ii)%ptr(kk)%id_point = kk_bis
          end do

          !Set pointer from list 1 (leaving points) to list 2 (entering points)
          do ll=1,numLeavingPoints1
            ii = pointer_leaving_points_1(ll)%id_edge
            kk = pointer_leaving_points_1(ll)%id_point
            point_id = list_edges_1(ii)%point_id(kk)
            jj = GetEdgeFromPointId (list_edges_2, point_id, num_edges_2, kk_bis)
            list_edges_1(ii)%ptr(kk)%id_edge = jj
            list_edges_1(ii)%ptr(kk)%id_point = kk_bis
          end do

          !Set pointer from list 2 (leaving points) to list 1 (entering points)
          do ll=1,numLeavingPoints2
            jj = pointer_leaving_points_2(ll)%id_edge
            kk = pointer_leaving_points_2(ll)%id_point
            point_id = list_edges_2(jj)%point_id(kk)
            ii = GetEdgeFromPointId (list_edges_1, point_id, num_edges_1, kk_bis)
            list_edges_2(jj)%ptr(kk)%id_edge = ii
            list_edges_2(jj)%ptr(kk)%id_point = kk_bis
          end do


          !--- RESULT
          !---   BUILDING THE POLYGONS
          !---     or the list of polygons (non convex case)
          !---     The size of the result list is numEnteringPoints1

          allocate(result_list%polygon(numEnteringPoints1))
          counter_entering_point = 1
          total_number_poly = 0
          do ipoly = 1, numEnteringPoints1
            finished = .false.
            call polygon_create(result_list%polygon(ipoly), numPtmax)
            ii = pointer_entering_points_1(ipoly)%id_edge
            kk = pointer_entering_points_1(ipoly)%id_point
            icur_list = 1
            !
            starting_point%id_edge = ii
            starting_point%id_point = kk
            !
            current_point%id_edge = ii
            current_point%id_point = kk
            !
            starting_point_gid = list_edges_1(ii)%point_id(kk)
            current_point_gid = 0
            ! add starting point
            ierr = polygon_addpoint(result_list%polygon(ipoly), list_edges_1(ii)%coor(kk))   !  starting point
            do while(.not.finished .and. ierr == 0 )
              call  NextPoint ( current_Point, icur_list, list_edges_1 , num_edges_1, list_edges_2 , num_edges_2)
               ii = current_point%id_edge
               kk = current_point%id_point
                 if(icur_list == 1)current_point_gid = list_edges_1(ii)%point_id(kk)
                 if(icur_list == 2)current_point_gid = list_edges_2(ii)%point_id(kk)
                 if (current_point_gid == starting_point_gid)then
                     finished = .true.
                 else
                   if(icur_list == 1)then
                     if(list_edges_1(ii)%iorient(kk)==1)then
                       counter_entering_point=counter_entering_point+1
                     end if
                   elseif(icur_list == 2)then
                     if(list_edges_2(ii)%iorient(kk)==-1)then
                       counter_entering_point=counter_entering_point+1
                     end if
                   end if
                 end if
                 if(icur_list == 1) ierr=polygon_addpoint(result_list%polygon(ipoly), list_edges_1(ii)%coor(kk))
                 if(icur_list == 2) ierr=polygon_addpoint(result_list%polygon(ipoly), list_edges_2(ii)%coor(kk))
                 !  go on with next point
            enddo

            !write( *,*) "  poly id:",ipoly
            !do ii =1, result_list%polygon(ipoly)%numpoint
            !  !HM TCL SCRIPT TO CHECK ELEM ON SCREEN
            !  write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,result_list%polygon(ipoly)%point(ii)%y , &
            !                                                   result_list%polygon(ipoly)%point(ii)%z
            !end do

            if (counter_entering_point == numEnteringPoints1) then
                !all entering points (from elem point of view) were used.
                !no more polygon to build
                total_number_poly = ipoly
                exit
            end if

          end do

          result_list%num_polygons = total_number_poly

          do ii=1,num_edges_1
            deallocate(list_edges_1(ii)%alpha)
            deallocate(list_edges_1(ii)%coor)
            deallocate(list_edges_1(ii)%iorient)
          end do
          deallocate(list_edges_1)

          do jj=1,num_edges_2
            deallocate(list_edges_2(jj)%alpha)
            deallocate(list_edges_2(jj)%coor)
            deallocate(list_edges_2(jj)%iorient)
          end do
          deallocate(list_edges_2)

          deallocate(icur_2)
          deallocate(pointer_entering_points_1)
          deallocate(pointer_leaving_points_1)
          deallocate(pointer_leaving_points_2)

        end subroutine





! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
      !||====================================================================
      !||    polygon_setclockwise      ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||--- called by ------------------------------------------------------
      !||    init_inivol_2d_polygons   ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod              ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine polygon_SetClockWise( Polyg )
          use constant_mod , only : zero, half
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_), intent(inout) :: Polyg
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_point_),allocatable,dimension(:) :: point
          my_real :: total
          integer :: i,n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          n = Polyg%numpoint
          allocate(point(n))
          point(1:n)%y = Polyg%point(1:n)%y
          point(1:n)%z = Polyg%point(1:n)%z
          total = ZERO
          do i=1,n-1
            total = total + (point(i+1)%y-point(i)%y)*(point(i+1)%z+point(i)%z)
          enddo
          polyg%area = half * abs(total)
          !set counter clockwise
          if (total > zero) then
            do i=1,n
              polyg%point(i)%y = point(n-i+1)%y
              polyg%point(i)%z = point(n-i+1)%z
            enddo
          endif
          deallocate(point)
      end subroutine polygon_SetClockWise




! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
      !||====================================================================
      !||    polygon_is_point_inside   ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod              ../common_source/modules/constant_mod.F
      !||====================================================================
        function polygon_is_point_inside( Polyg, pt ) result(is_inside)
          use constant_mod , only : zero, em20, em10, half, one
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_), intent(in) :: Polyg !< polygon
          type(polygon_point_), intent(in) :: pt !< point to test
          logical is_inside !< output result true or false
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer iedg
          integer num_edges
          integer npt !< number of polygon points
          type(polygon_point_) :: P1,P2
          integer :: num_inter_pt !depending if number is odd or even we can status about point location (inside/outside)
          my_real dy,dz
          my_real lambda
          my_real tol
          logical cond1, cond2  !< conditional test to determine the final result
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          npt = Polyg%numpoint
          num_edges = npt - 1
          num_inter_pt = 0
          tol = em10
          is_inside = .false.
          do iedg=1,num_edges
            P1%y = Polyg%point(iedg)%y
            P1%z = Polyg%point(iedg)%z
            P2%y = Polyg%point(iedg+1)%y
            P2%z = Polyg%point(iedg+1)%z
            dy = P2%y-P1%y
            dz = P2%z-P1%z

            cond2=.false.
            cond1 = abs(dz) > em20
            if (cond1) then
              lambda = (pt%z-P1%z)/dz
              cond2 = ((P1%y-pt%y) + lambda*dy) > em20
              if(lambda<zero .or. lambda > one)cond2=.false.
            end if
            if (cond1 .and. cond2)then
              num_inter_pt = num_inter_pt + 1
            end if
          end do

          if (mod(num_inter_pt, 2) == 0)then
            is_inside = .false.
          else
            is_inside = .true.
          end if

      end function polygon_is_point_inside


! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief   This subroutine is reindexing a array of POINTS using index(1:n) array
!! \details      Example array = (/ P1 P2 P3 P4/)
!! \details              index = (/4 3 2 1/)
!! \details      result will be  (/ P4 P3 P2 P1 /)
      !||====================================================================
      !||    points_array_reindex       ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||--- called by ------------------------------------------------------
      !||    clipping_weiler_atherton   ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||====================================================================
      subroutine points_array_reindex(array, index, n)
        implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent(in) :: n
        type(polygon_point_), intent(inout) :: array(n)
        integer, intent(inout) :: index(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: ii
        type(polygon_point_) :: temp_array(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        do ii=1,n
          temp_array(ii)%y=array(ii)%y
          temp_array(ii)%z=array(ii)%z
        end do
        do ii = 1, n
          array(ii)%y = temp_array(index(ii))%y
          array(ii)%z = temp_array(index(ii))%z
        end do
      end subroutine points_array_reindex




    end module polygon_clipping_mod
