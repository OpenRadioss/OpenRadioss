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
      !||    get_list_remnode_mod   ../starter/source/interfaces/inter3d1/get_list_remnode.F90
      !||--- called by ------------------------------------------------------
      !||    i7remnode              ../starter/source/interfaces/inter3d1/i7remnode.F
      !||====================================================================
      module get_list_remnode_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      !||====================================================================
      !||    get_list_remnode   ../starter/source/interfaces/inter3d1/get_list_remnode.F90
      !||--- called by ------------------------------------------------------
      !||    i7remnode          ../starter/source/interfaces/inter3d1/i7remnode.F
      !||--- calls      -----------------------------------------------------
      !||    upgrade_remnode    ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine get_list_remnode(nrtm,igap ,numnod,npari,irect,kremnode, &
                                    knod2seg,nod2seg,tagsecnd,   &
                                    ipari,gapmin,gapmax,gap,drad,         &
                                    gaps_mx,gaps_l_mx,  &
                                    minseg,dgapload,x,gap_m,              &
                                    gap_m_l,gapsecnd,gap_s_l_tmp,    &
                                    intbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
!  [ the module names in use must be in uppercase for now, it will change latter]
!  [ only is mandatory, note the space before the ,]
          use intbufdef_mod , only : intbuf_struct_
          use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nrtm !< number of segment
          integer, intent(in) :: igap !< gap value
          integer, intent(in) :: numnod !< number of node    
          integer, intent(in) :: npari !< first dim of ipari
          integer, dimension(4,nrtm), intent(in) ::  irect !< list of node for each segment
          integer, dimension(nrtm+1), intent(inout) ::  kremnode !< index for remnode array
          integer, dimension(numnod+1), intent(in) ::  knod2seg !< number of segment connected to the same node
          integer, dimension(4*nrtm), intent(in) ::  nod2seg !< list of segment connected to the same node
          integer, dimension(numnod), intent(in) :: tagsecnd !< 0 if the node is not a S type2 node
          integer, dimension(npari), intent(in) :: ipari !< interface data
          my_real , intent(in) :: gapmin !< 
          my_real , intent(in) :: gapmax !< 
          my_real , intent(in) :: gap !< 
          my_real , intent(in) :: drad !< 
          my_real , intent(in) :: gaps_mx !< 
          my_real , intent(in) :: gaps_l_mx !< 
          my_real , intent(in) :: minseg !< 
          my_real , intent(in) :: dgapload !< 
          my_real, dimension(3,numnod), intent(in) :: x !< position
          my_real, dimension(nrtm), intent(in) :: gap_m !< gap of M node
          my_real, dimension(nrtm), intent(in) :: gap_m_l !< gap of M node
          my_real, dimension(numnod), intent(in) :: gapsecnd !<  gap of S node
          my_real, dimension(numnod), intent(in) :: gap_s_l_tmp !<  gap of S node
          type(intbuf_struct_), intent(inout) :: intbuf_tab !< interface data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      
          integer i,j,k,seg,iseg,level,cpt,nbseg,seg1,l,cpt1,cpt_total
          integer :: cptoper
          integer :: jmax,kmax,nty
          integer :: my_size,my_new_size,local_remnode_size
          integer :: node_id,node_id_seg1,my_seg_id,my_seg_number
          integer :: my_local_address,my_address
          integer, dimension(:), allocatable :: tmp_array,local_remnode
          integer, dimension(:,:), allocatable :: local_kremnode
          integer, dimension(:), allocatable :: listseg,listsegtmp,listsegtotal,itagseg
          integer, dimension(:), allocatable :: id_nod,noddel,nod2expand,tagnod
          my_real, dimension(:), allocatable :: dist1,gapv
          my_real :: mindist,dmax
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
          my_seg_number = 0
          allocate(noddel(numnod),nod2expand(numnod) )
          allocate(listseg(nrtm),listsegtmp(nrtm),listsegtotal(nrtm))
          allocate( id_nod(numnod) )
          allocate( tagnod(numnod) )
          allocate( dist1(numnod) )
          allocate( gapv(numnod) )
          allocate(itagseg(nrtm) )
          id_nod(1:numnod) = 0
          nod2expand(1:numnod) = 0
          tagnod(1:numnod) = 0
          dist1(1:numnod) = ep30
          itagseg(1:nrtm) = 0
          gapv(1:numnod) = zero

          local_remnode_size = 4*nrtm
          allocate( local_remnode(local_remnode_size) )
          allocate( local_kremnode(nrtm+1,2) )
          local_kremnode(1,1:2) = 0
!$omp do schedule(guided)
          do i=1,nrtm
            my_seg_number = my_seg_number + 1
            ! ---------------
            ! define the max distance
            dmax = zero
            if(igap==0)then
              dmax  = sqrt(two) * max(gap+dgapload,drad)
            elseif(igap==1 .or. igap==2) then
              dmax  = sqrt(two) * max(gap_m(i)+gaps_mx+dgapload,drad)
            elseif(igap==3) then
              dmax  = sqrt(two) * max(min(gap_m(i)+gaps_mx,gap_m_l(i)+gaps_l_mx)+dgapload,drad)
            end if
            ! ---------------


            cptoper = 0
            level   = 1
            seg     = i
            itagseg(seg) = level
            nbseg     =1
            mindist   = zero
            jmax      = 4
            cpt_total = 0

            if((irect(jmax,seg) == 0) .or. irect(3,seg) == irect(4,seg) ) jmax = 3
            do j=1,jmax
              node_id = irect(j,seg)
              tagnod(node_id) = 1
              dist1(node_id) = zero
            enddo
            listseg(1)=seg

            ! ----------------------------------
            do while( (mindist + minseg) <= dmax .and. nbseg /= 0)
              level   = level + 1
              mindist = ep30
              cpt     = 0
              do iseg=1,nbseg
                seg  = listseg(iseg) ! list of segment
                jmax = 4
                if((irect(jmax,seg) == 0) .or. irect(3,seg) == irect(4,seg) ) jmax = 3 ! number of node
                tagnod(irect(1:jmax,seg))=2 ! tag the node of the segment "seg"
                
                do j=1,jmax
                  node_id = irect(j,seg)
                  if(nod2expand(node_id)/=0) cycle ! this node was already done
     
                  nod2expand(node_id)=1 ! tag the node 
                  do k=knod2seg(node_id)+1,knod2seg(node_id+1) ! loop over the segment connected to the node irect(j,seg)
                    seg1 = nod2seg(k) ! get the segment id

                    kmax = 4
                    if((irect(kmax,seg1) == 0) .or. irect(3,seg1) == irect(4,seg1) ) kmax = 3 ! number of node of the segment "seg1"

                    if(itagseg(seg1) == 0 .or. itagseg(seg1) == level) then ! the segment was not already treated
                      if(itagseg(seg1) == 0)then ! add the segment "seg1" to the list of segment
                        cpt = cpt + 1 ! number of new segment to be checked
                        listsegtmp(cpt)=seg1 ! list of new segment to be checked
                      endif
                      itagseg(seg1)=level ! tag the segment seg1
     
                      do l=1,kmax ! loop over the node of the segment "seg1"
                        node_id_seg1 = irect(l,seg1)             
                        if(tagsecnd(node_id_seg1)== 0 .or.tagnod(node_id_seg1) == 2)cycle   ! the node irect(l,seg1) is also a node of the segment "seg"
                      ! compute the distance between the node irect(l,seg1) & irect(j,seg)
                        dist1(node_id_seg1)=min(dist1(node_id_seg1),dist1(node_id)+  &
                             sqrt((x(1,node_id_seg1) - x(1,node_id))**2 +             &
                                  (x(2,node_id_seg1) - x(2,node_id))**2 +             &
                                  (x(3,node_id_seg1) - x(3,node_id))**2 ))
                        mindist=min(mindist,dist1(node_id_seg1))
                        if(tagnod(node_id_seg1) == 0) then ! node "irect(l,seg1)" was not already treated
                          cptoper = cptoper + 1
                          tagnod(node_id_seg1) = 1 ! tag the node "irect(l,seg1)"
                          id_nod(cptoper)=node_id_seg1 ! save the node id
                        endif 

                        if(igap==1 .or. igap==2)then
                          gapv(irect(l,seg1))=gapsecnd(irect(l,seg1))+gap_m(i)
                          gapv(irect(l,seg1))=min(gapmax,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(gapmin,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(drad,gapv(irect(l,seg1))+dgapload)
                        elseif(igap==3)then
                          gapv(irect(l,seg1))=gapsecnd(irect(l,seg1))+gap_m(i)
                          gapv(irect(l,seg1))= min(gap_s_l_tmp(irect(l,seg1))+gap_m_l(i),gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=min(gapmax,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(gapmin,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(drad,gapv(irect(l,seg1))+dgapload)
                        end if

                      enddo
                    endif
                     
                  enddo 
       
                enddo

                tagnod(irect(1:4,seg))=1 ! tag the node of the segment "seg" to 1 (instead of 2)

              enddo

              do iseg=1,nbseg
                seg  = listseg(iseg)
                jmax = 4
                if((irect(jmax,seg) == 0) .or. irect(3,seg) == irect(4,seg) ) jmax = 3
                do j=1,jmax
                  node_id = irect(j,seg)
                  nod2expand(node_id)=0
                end do
              end do

              nbseg     = cpt
              if(nbseg ==0)exit
              do j=1,cpt
                listseg(j)    =listsegtmp(j)
                listsegtmp(j) = 0
                listsegtotal(j+cpt_total) = listseg(j)
              enddo
              cpt_total = cpt_total + cpt

            enddo
            ! ----------------------------------


            if (level == 1) then ! no node to remove for the segment "i"
              kremnode(i+1) = 0
              local_kremnode(my_seg_number+1,1) = local_kremnode(my_seg_number,1)
              local_kremnode(my_seg_number,2) = i
            else  
              dist1(irect(1,i)) = ep30
              dist1(irect(2,i)) = ep30
              dist1(irect(3,i)) = ep30
              dist1(irect(4,i)) = ep30

              cpt1 = 0 
              if(igap==0)then
                do l=1,cptoper 
                  if(dist1(id_nod(l)) <= dmax)then ! check if the distance to the node id_nod(l) is < to dmax
                    cpt1 = cpt1 + 1 
                    noddel(cpt1) = id_nod(l) ! save the node
                  endif
                enddo
              else
                do l=1,cptoper 
                  if(dist1(id_nod(l)) <= sqrt(two)*gapv(id_nod(l)))then
                    cpt1 = cpt1 + 1
                    noddel(cpt1) = id_nod(l)
                  endif
                enddo
              end if

              kremnode(i+1) = cpt1
              local_kremnode(my_seg_number+1,1) = local_kremnode(my_seg_number,1) + cpt1
              local_kremnode(my_seg_number,2) = i
              if(cpt1+local_kremnode(my_seg_number+1,1)>local_remnode_size) then
                my_new_size = local_remnode_size + max( local_remnode_size/10, 10*cpt1)
                allocate( tmp_array(my_new_size) )
                tmp_array(1:local_remnode_size) = local_remnode(1:local_remnode_size)
                deallocate( local_remnode )
                call move_alloc( tmp_array,local_remnode )
                local_remnode_size = my_new_size
              endif
              do l=1,cpt1
                local_remnode(local_kremnode(my_seg_number,1)+l) = noddel(l) ! save the forbidden node in remnode
              enddo
              do l=1,cptoper
                dist1(id_nod(l)) = ep30
              enddo
              do l=1,cpt_total
                tagnod(irect(1:4, listsegtotal(l))) = 0
                itagseg(listsegtotal(l)) = 0
                listsegtotal(l)          = 0
              enddo
              tagnod(irect(1:4,i)) = 0
              itagseg(i)           = 0
            endif
          enddo
!$omp end do

!$omp barrier

!$omp single
          do i=1,nrtm
            kremnode(i+1) = kremnode(i+1) + kremnode(i) 
          enddo
          my_size = intbuf_tab%s_remnode
          nty = ipari(7)
          if(kremnode(nrtm+1)>my_size) call upgrade_remnode( ipari,kremnode(nrtm+1),intbuf_tab,nty )
!$omp end single

          do i=1,my_seg_number
            my_local_address = local_kremnode(i,1)
            my_size = local_kremnode(i+1,1) - local_kremnode(i,1)
            my_seg_id = local_kremnode(i,2)
            my_address = kremnode(my_seg_id)
            do j=1,my_size
              intbuf_tab%remnode(my_address+j) = local_remnode(my_local_address+j)
            enddo
          enddo


          deallocate(noddel,nod2expand )
          deallocate(listseg,listsegtmp,listsegtotal)
          deallocate( local_remnode )
          deallocate( local_kremnode )
          deallocate( itagseg )
          deallocate( gapv )



          return
        end subroutine get_list_remnode
      end module get_list_remnode_mod
