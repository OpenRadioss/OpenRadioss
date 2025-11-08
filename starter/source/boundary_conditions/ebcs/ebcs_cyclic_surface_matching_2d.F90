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
!||    ebcs_cyclic_surface_matching_2d_mod   ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_2d.F90
!||--- called by ------------------------------------------------------
!||    ebcs_cyclic_surface_matching          ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching.F90
!||====================================================================
      module ebcs_cyclic_surface_matching_2d_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief check surface 1 (nseg) and surface 2 (nseg)
!! \details storing data in linear arrays %elem_list and %node_lide with same order 1:nseg and nseg+1 : nseg+nseg
!! \details number of segment already match (checked with Reader subroutine)
!||====================================================================
!||    ebcs_cyclic_surface_matching_2d   ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_2d.F90
!||--- called by ------------------------------------------------------
!||    ebcs_cyclic_surface_matching      ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                            ../starter/source/output/message/message.F
!||    iface                             ../starter/source/ale/ale3d/iface.F
!||--- uses       -----------------------------------------------------
!||    message_mod                       ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine ebcs_cyclic_surface_matching_2d(ebcs_cyclic, ebcs, numnod, X)
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
          integer :: nseg
          real(kind=WP) :: TT(3)    !< normal and tangent vector
          real(kind=WP) :: v12(3),v12_(3)
          real(kind=WP) :: d12,d12_ !< vector length
          real(kind=WP) :: R(2,2) !< roation matrix
          real(kind=WP) :: tol
          real(kind=WP) :: cost, sint
          character(len=nchartitle) :: title
          real(kind=WP) :: imgY,imgZ
          integer,dimension(:),allocatable :: NODLIST1, NODLIST2, NODLIST3
          integer,dimension(:),allocatable :: ITAG_ELEM,IFACE,IELEM,ISEG
          integer,dimension(:,:),allocatable :: ELEM_LIST
          integer :: inod1,inod2,inod3,inod4
          logical :: lFOUND
          integer :: i , j
          integer :: nnod
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
          v12(2)  = X(2,nod(2))-X(2,nod(1))
          v12(3)  = X(3,nod(2))-X(3,nod(1))
          ! N1'->N2'
          v12_(2) = X(2,nod(5))-X(2,nod(4))
          v12_(3) = X(3,nod(5))-X(3,nod(4))

          !tol
          d12 = sqrt(v12(2)*v12(2) + v12(3)*v12(3))
          d12_ = sqrt(v12_(2)*v12_(2) + v12_(3)*v12_(3))
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

          if(.NOT.lVALID) return


! ----------------------------------------------------------------------------------------------------------------------
          !NODES WERE CHECKED
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------


          ! --- FROM HERE NODES WERE TESTED AND DO MATCH
          cost = v12(2)*v12_(2) + v12(3)*v12_(3)
          sint = v12(2)*v12_(3) - v12(3)*v12_(2)
          cost = cost / (d12*d12_)
          sint = sint / (d12*d12_)
          R(1,1) = cost ; R(1,2) = -sint
          R(2,1) = sint ; R(2,2) = cost

          ! tt = N1'-R.N1
          tt(2) = X(2,nod(4)) - cost*X(2,nod(1)) + sint*X(3,nod(1))
          tt(3) = X(3,nod(4)) - sint*X(2,nod(1)) - cost*X(3,nod(1))

          NSEG = ebcs%nb_elem  !nseg1=nseg2 (checked by Reader subroutine), allocated to 2*nseg
          NNOD = ebcs%nb_node  !nnod1=nnod2 (checked by Reader subroutine), allocated to 2*nnod

          allocate(NODLIST1(NNOD))
          allocate(NODLIST2(NNOD))
          allocate(NODLIST3(NNOD))
          NODLIST1(1:NNOD) = ebcs%node_list(1:NNOD)
          NODLIST2(1:NNOD) = ebcs%node_list(NNOD+1:NNOD+NNOD)
          NODLIST3(1:NNOD) = ebcs%node_list(NNOD+1:NNOD+NNOD) !backup

          ! ASSOCIATE NODES
          !  NODLIST1 : corresponding index on %node_list2
          ! -NODLIST2 : corresponding index on %node_list1
          DO I=1,NNOD
            INOD1 = NODLIST1(I)
            DO J=1,NNOD
              INOD2 = NODLIST2(J)
              lFOUND=.FALSE.
              if(inod2 > 0)then
                imgY = X(2,INOD1) + tt(2)
                imgZ = X(3,INOD1) + tt(3)
                if(abs(imgY-X(2,INOD2)) < tol .and. abs(imgZ-X(3,INOD2)) < tol)then
                  NODLIST1(I) = J
                  NODLIST2(J) = - I
                  lFOUND=.TRUE.
                  exit
                endif
              end if
            END DO
             ! NODE I on surface 1 do not match any node on surface 2
             if(.NOT.lFOUND)then
               !error surface soes not match
                 call ancmsg(msgid=1602,msgtype=msgerror,anmode=aninfo,i1=ebcs_uid,c1=trim(title), &
                 C2="SURFACE NODES DO NOT MATCH")
                 exit
            end if
          END DO

          if(.NOT. lFOUND)return

          !renumber list of nodes
          DO I=1,NNOD
            ebcs%node_list(nnod+i) = NODLIST3(NODLIST1(I))
          END DO

          !renumber segment nodes
          DO I=1,NSEG
              inod1 = ebcs%elem_list(1,nseg+i)
              inod2 = ebcs%elem_list(2,nseg+i)
              ebcs%elem_list(1,nseg+i) = -NODLIST2(NODLIST1(INOD1))
              ebcs%elem_list(2,nseg+i) = -NODLIST2(NODLIST1(INOD2))
          END DO

          !ordering segments
          allocate(ITAG_ELEM(NSEG))
          allocate(IFACE(NSEG))
          allocate(ISEG(NSEG))
          allocate(IELEM(NSEG))
          allocate(ELEM_LIST(2,NSEG))
          ITAG_ELEM(1:NSEG)=0
          ELEM_LIST(1,1:NSEG) = ebcs%elem_list(1,nseg+1:nseg+nseg)
          ELEM_LIST(2,1:NSEG) = ebcs%elem_list(2,nseg+1:nseg+nseg)
          IFACE(1:NSEG) = ebcs%iface(nseg+1:nseg+nseg)
          ISEG(1:NSEG) = ebcs%iseg(nseg+1:nseg+nseg)
          IELEM(1:NSEG) = ebcs%ielem(nseg+1:nseg+nseg)
          DO I=1,NSEG
            inod1 = ebcs%elem_list(1,i)
            inod2 = ebcs%elem_list(2,i)
            DO J=1,NSEG
              inod3 = ELEM_LIST(1,j)
              inod4 = ELEM_LIST(2,j)
              if(itag_elem(j) == 0)then
                if(inod1==inod3 .OR. inod1==inod4)then
                    if(inod2==inod3 .OR. inod2==inod4)then
                        ITAG_ELEM(J)=1
                        ebcs%elem_list(1:2,nseg+i) = ELEM_LIST(1:2,J)
                        ebcs%iface(nseg+i) = IFACE(J)
                        ebcs%iseg(nseg+i) = ISEG(J)
                        ebcs%ielem(nseg+i) = IELEM(J)
                    end if
                end if
              end if
            END DO
          END DO

          deallocate(nodlist1,nodlist2,nodlist3)
          deallocate(ITAG_ELEM,IFACE,IELEM,ELEM_LIST,ISEG)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine ebcs_cyclic_surface_matching_2d
      end module ebcs_cyclic_surface_matching_2d_mod
