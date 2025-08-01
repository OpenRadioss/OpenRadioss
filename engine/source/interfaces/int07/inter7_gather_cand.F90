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
!||    inter7_gather_cand_mod   ../engine/source/interfaces/int07/inter7_gather_cand.F90
!||--- called by ------------------------------------------------------
!||    inter7_filter_cand       ../engine/source/interfaces/intsort/inter7_filter_cand.F90
!||====================================================================
      MODULE INTER7_GATHER_CAND_MOD
      CONTAINS
!||====================================================================
!||    inter7_gather_cand   ../engine/source/interfaces/int07/inter7_gather_cand.F90
!||--- called by ------------------------------------------------------
!||    inter7_filter_cand   ../engine/source/interfaces/intsort/inter7_filter_cand.F90
!||--- uses       -----------------------------------------------------
!||    collision_mod        ../engine/source/interfaces/intsort/collision_mod.F
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine inter7_gather_cand(jlt     ,x     ,irect ,nsv   ,cand_e ,&
        &cand_n  ,igap  ,gap   ,x1    ,x2     ,&
        &x3      ,x4    ,y1    ,y2    ,y3     ,&
        &y4      ,z1    ,z2    ,z3    ,z4     ,&
        &xi      ,yi    ,zi    ,&
        &nsn     ,gap_s  , ix1, ix2, ix3, ix4,&
        &gap_m   ,gapv  ,gapmax,gapmin,curv_max,&
        &ityp    ,gap_s_l,gap_m_l,&
        &drad    ,dgapload, nsnr,&
        &s_xrem, xrem, nrtm,mulnsn,numnod)
          USE COLLISION_MOD ,  ONLY : GROUP_SIZE
          USE PRECISION_MOD, ONLY : WP
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in), value :: jlt !< number of secondary nodes to be checked
          integer, intent(in), value :: nsn !< number of secondary nodes
          integer, intent(in), value :: ityp !< contact interface type
          integer, intent(in), value :: nrtm !< number of main segments
          integer, intent(in), value :: mulnsn !< number of collision candidates
          integer, intent(in), value :: numnod  !< number of nodes

          integer, intent(in) :: irect(4,nrtm) !< irect(1:4,i) contains the node id of the i-th main segment
          integer, intent(in) :: nsv(nsn)     !< nsv(i) contains the id of the i-th secondary node
          integer, intent(inout) :: cand_e(mulnsn)  !< cand_e(i) contains the id of the main segment of the i-th pair of collision candidates
          integer, intent(inout) :: cand_n(mulnsn)  !< cand_n(i) contains the id of the secondary node of the i-th pair of collision candidates
          integer, intent(in), value :: igap       !< flag for gap formulation

          real(kind=WP), intent(in) :: x(3,numnod)     !< x(1:3,i) contains the coordinates of the i-th node
          real(kind=WP), intent(inout) :: gapv(nsn) !< gap per secondary node, may be variable depending on the gap formulation
          real(kind=WP), intent(in) :: gap_s(nsn)   !< gap of the secondary nodes
          real(kind=WP), intent(in) :: gap_m(nrtm) !< gap of the main segments
          real(kind=WP), intent(in) :: curv_max(nrtm) !< maximum curvature of the main segments
          real(kind=WP), intent(in), value :: gap !< initial gap
          real(kind=WP), intent(in), value :: gapmax !< maximum gap
          real(kind=WP), intent(in), value :: gapmin !< minimum gap

          real(kind=WP), intent(in), value :: dgapload !< ??
          real(kind=WP), intent(in), value :: drad !< radiation gap

          real(kind=WP), intent(inout) :: x1(GROUP_SIZE)    !<x coordinate of the first node of the main segment
          real(kind=WP), intent(inout) :: x2(GROUP_SIZE)    !<x coordinate of the second node of the main segment
          real(kind=WP), intent(inout) :: x3(GROUP_SIZE)    !<x coordinate of the third node of the main segment
          real(kind=WP), intent(inout) :: x4(GROUP_SIZE)    !<x coordinate of the fourth node of the main segment
          real(kind=WP), intent(inout) :: y1(GROUP_SIZE)    !<y coordinate of the first node of the main segment
          real(kind=WP), intent(inout) :: y2(GROUP_SIZE)    !<y coordinate of the second node of the main segment
          real(kind=WP), intent(inout) :: y3(GROUP_SIZE)    !<y coordinate of the third node of the main segment
          real(kind=WP), intent(inout) :: y4(GROUP_SIZE)    !<y coordinate of the fourth node of the main segment
          real(kind=WP), intent(inout) :: z1(GROUP_SIZE)    !<z coordinate of the first node of the main segment
          real(kind=WP), intent(inout) :: z2(GROUP_SIZE)    !<z coordinate of the second node of the main segment
          real(kind=WP), intent(inout) :: z3(GROUP_SIZE)    !<z coordinate of the third node of the main segment
          real(kind=WP), intent(inout) :: z4(GROUP_SIZE)    !<z coordinate of the fourth node of the main segment
          real(kind=WP), intent(inout) :: xi(GROUP_SIZE) !<x coordinate of the i-th secondary node
          real(kind=WP), intent(inout) :: yi(GROUP_SIZE) !<y coordinate of the i-th secondary node
          real(kind=WP), intent(inout) :: zi(GROUP_SIZE) !<z coordinate of the i-th secondary node
          real(kind=WP), intent(in) :: gap_s_l(nsn) !< ???
          real(kind=WP), intent(in) :: gap_m_l(nrtm) !< ???
          integer, intent(in), value :: s_xrem              !< size of xrem
          integer, intent(in), value :: nsnr                !< number of remote (spmd) secondary nodes
          real(kind=WP), intent(in) :: xrem(s_xrem, nsnr)  !< Remote (spmd) secondary data (coordinates etc.)
          integer,intent(inout) :: ix1(GROUP_SIZE) !< list of first node of the main segments
          integer,intent(inout) :: ix2(GROUP_SIZE) !< list of second node of the main segments
          integer,intent(inout) :: ix3(GROUP_SIZE) !< list of third node of the main segments
          integer,intent(inout) :: ix4(GROUP_SIZE) !< list of fourth node of the main segments

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i
          integer :: j
          integer :: l
          integer :: ig
          integer :: iadd
!-----------------------------------------------
          if(igap==0)then
            do i=1,jlt
              gapv(i)=max(gap+dgapload,drad)
            end do
          elseif(igap == 3)then
            iadd = 9
            do i=1,jlt
              j = cand_n(i)
              if(j<=nsn) then
                gapv(i)=gap_s(j)+gap_m(cand_e(i))
                gapv(i)=min(gap_s_l(j)+gap_m_l(cand_e(i)),gapv(i))
              else
                ig = j-nsn
                gapv(i)=xrem(9,ig)+gap_m(cand_e(i))
                gapv(i)=min(xrem(10,ig)+gap_m_l(cand_e(i)),gapv(i))
              end if
              gapv(i)=min(gapv(i),gapmax)
              gapv(i)=max(gapmin,gapv(i))
              gapv(i)=max(drad,gapv(i)+dgapload)
            end do
          else
            do i=1,jlt
              j = cand_n(i)
              if(j<=nsn) then
                gapv(i)=gap_s(j)+gap_m(cand_e(i))
              else
                ig = j-nsn
                gapv(i)=xrem(9,ig)+gap_m(cand_e(i))
              end if
              gapv(i)=min(gapv(i),gapmax)
              gapv(i)=max(gapmin,gapv(i))
              gapv(i)=max(drad,gapv(i)+dgapload)
            end do
          end if
          do i=1,jlt
            j = cand_n(i)
            if(j<=nsn) then
              ig = nsv(j)
              xi(i) = x(1,ig)
              yi(i) = x(2,ig)
              zi(i) = x(3,ig)
            else
              ig = j-nsn
              xi(i) = xrem(1,ig)
              yi(i) = xrem(2,ig)
              zi(i) = xrem(3,ig)
            endif
!
            l  = cand_e(i)
!
            ix1(i)=irect(1,l)
            x1(i)=x(1,ix1(i))
            y1(i)=x(2,ix1(i))
            z1(i)=x(3,ix1(i))
!
            ix2(i)=irect(2,l)
            x2(i)=x(1,ix2(i))
            y2(i)=x(2,ix2(i))
            z2(i)=x(3,ix2(i))
!
            ix3(i)=irect(3,l)
            x3(i)=x(1,ix3(i))
            y3(i)=x(2,ix3(i))
            z3(i)=x(3,ix3(i))
!
            ix4(i)=irect(4,l)
            x4(i)=x(1,ix4(i))
            y4(i)=x(2,ix4(i))
            z4(i)=x(3,ix4(i))
          enddo
          if(ityp == 7)then
            do i=1,jlt
              gapv(i) = gapv(i) + curv_max(cand_e(i))
            end do
          endif
!
          return
        end subroutine inter7_gather_cand
      end module INTER7_GATHER_CAND_MOD

