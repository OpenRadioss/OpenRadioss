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
!||    hm_read_bcs_wall_mod   ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||====================================================================
      module hm_read_guided_cable_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Reader subroutine for option /GUIDED_CABLE
!! \details
!||====================================================================
!||    hm_read_guided_cable       ../starter/source/tools/seatbelts/hm_read_guided_cable.F90
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    nodgrnr5               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_guided_cable(lsubmodel, igrnod , ngrnod, igrpart, ngrpart,   &
                                        npart    , unitab , numelr, ipartr , ixr    ,   &
                                        nixr     , numelp , ipartp, ixp    , nixp   ,   &
                                        numelt   , ipartt , ixt   , nixt   , nspmd   ,   &
                                        knod2el1d,nod2el1d,snod2el1d,numnod, x      ,   &
                                        numels   ,numelq  ,numelc)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use submodel_mod , only : submodel_data, nsubmod
          use groupdef_mod , only : group_
          use message_mod , only : ancmsg, msgwarning, msgerror, aninfo_blind_1
          use hm_option_read_mod , only : hm_option_read_key
          use constant_mod , only : zero, em20, ep20, one
          use names_and_titles_mod , only : nchartitle
          use precision_mod, only : WP
          use seatbelt_mod, only : nguided_cable,guide
          use unitab_mod, only : unit_type_
          use dist_node_segment_mod
          use find_prev_next_nodes_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ngrnod                                    !< number of groups of nodes
          integer, intent(in) :: ngrpart                                   !< number of groups of parts
          integer, intent(in) :: npart                                     !< number of parts
          integer, intent(in) :: numnod                                    !< number of nodes
          integer, intent(in) :: numelr                                    !< number of spring elements
          integer, intent(in) :: numelp                                    !< number of beam elements
          integer, intent(in) :: numelt                                    !< number of truss elements
          integer, intent(in) :: numels                                    !< number of solid elements
          integer, intent(in) :: numelq                                    !< number of quad elements
          integer, intent(in) :: numelc                                    !< number of shell elements
          integer, intent(in) :: nspmd                                     !< number of spmd domains
          integer, intent(in) :: nixr                                      !< number of connectivity per spring element
          integer, intent(in) :: nixp                                      !< number of connectivity per beam element
          integer, intent(in) :: nixt                                      !< number of connectivity per truss element
          integer, intent(in) :: snod2el1d                                 !< size of nod2el1d array
          integer, intent(in) :: ipartp(numelp)                            !< beam element connectivity
          integer, intent(in) :: ipartr(numelr)                            !< spring element connectivity
          integer, intent(in) :: ipartt(numelt)                            !< truss element connectivity
          integer, intent(in) :: ixr(nixr,numelr)                          !< spring element connectivity
          integer, intent(in) :: ixp(nixp,numelp)                          !< beam element connectivity
          integer, intent(in) :: ixt(nixt,numelt)                          !< truss element connectivity
          integer, intent(in) :: knod2el1d(numnod+1)                       !< mapping from node to 1D element
          integer, intent(in) :: nod2el1d(snod2el1d)                       !< mapping from node to 1D element     
          real(kind=wp), dimension(:), intent(in) :: x(3,numnod)           !< coordinates of nodes
          type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< submodel data for Reader subroutines
          type (group_), dimension(ngrnod), target :: igrnod               !< data for group of nodes
          type (group_), dimension(ngrpart),target :: igrpart              !< data for group of parts
          type (unit_type_)   :: unitab                                    !< unit number for restart file
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=wp) :: stfac, fric, dist
          integer :: id, uid
          integer :: grnod_id_u, grpart_id_u,grnod_id, grpart_id
          integer :: istiff
          integer :: i, j, k, l, ig
          integer :: ncont,size_part_to_guide
          integer :: node1,node2,anchor_node
          integer :: nb_guide_group,igroup,group_nb_elem
          integer :: guide_nb_elem(nguided_cable),guide_igroup(nguided_cable)
          integer :: nodes_found(3),nodes_next_found(2),active_segment
          integer :: compt,compt_group,igroup_min
          integer :: compt_node_guide,inod
          integer :: part_to_guide_group(npart)
          integer :: nb_common_nodes,ig1,ig2
          real(kind=wp) :: dist1,dist2
          integer, dimension(:), pointer :: ingr2usr
          integer, dimension(:), allocatable :: npart_to_guide,add_part_to_guide,cpt_part_to_guide
          integer, dimension(:), allocatable :: part_to_guide    
          integer, dimension(:), allocatable :: cc_elem 
          integer, dimension(:), allocatable :: tagnod_guide   
          integer, dimension(:), allocatable :: listnod_guide              
          integer, dimension(:,:), allocatable :: common_nodes          
          character(len=nchartitle) :: titr
          character(len=40) :: mess
          logical :: is_available
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External Functions
! ----------------------------------------------------------------------------------------------------------------------
          integer,external :: ngr2usr
! ----------------------------------------------------------------------------------------------------------------------
          data mess/"GUIDED CABLE DEFINITION                  "/         
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          call hm_option_start('/GUIDED_CABLE')
          write(iout, 1000)
!          
          allocate(guide(nguided_cable))
          guide(1:nguided_cable)%id = 0
          guide(1:nguided_cable)%node_set = 0
          guide(1:nguided_cable)%part_set = 0
          guide(1:nguided_cable)%istiff = 0
          guide(1:nguided_cable)%stfac = zero
          guide(1:nguided_cable)%fric = zero
!          
          allocate(npart_to_guide(npart))
          npart_to_guide(1:npart) = 0
          size_part_to_guide = 0
          
!-------------------------------------------------------------------------------------------          
!         Reading of input
!-------------------------------------------------------------------------------------------           
          do i = 1, nguided_cable
!           
            call hm_option_read_key(lsubmodel, option_titr=titr, option_id=id, unit_id=uid)
            call hm_get_intv("grnod_id", grnod_id_u, is_available, lsubmodel)
            call hm_get_intv("grpart_id", grpart_id_u, is_available, lsubmodel)
            call hm_get_intv("istiff", istiff, is_available, lsubmodel)
            call hm_get_floatv("stfac", stfac, is_available, lsubmodel,unitab)
            call hm_get_floatv("fric", fric, is_available, lsubmodel,unitab)
            if (stfac == zero) stfac = one
            if (istiff == 0) istiff = 1
!
!---------  Check set node Id-----------------------------
            ingr2usr => igrnod(1:ngrnod)%id
            grnod_id=ngr2usr(grnod_id_u,INGR2USR,ngrnod)
!
!---------  Check set part Id-----------------------------          
            if (grpart_id_u /= 0) then
              grpart_id = 0
              do j = 1, ngrpart
                if (igrpart(j)%id == grpart_id_u) then
                  grpart_id = j
                  exit
                end if
              end do
              if (grpart_id == 0) then
                call ancmsg(msgid=3124, msgtype=msgerror, anmode=aninfo_blind_1, i1=id, c1=trim(titr), i2=grpart_id_u)
              end if
            end if
!           
            write(iout, 1100) id,trim(titr),grnod_id_u,grpart_id_u,istiff,stfac,fric
            guide(i)%id = id
            guide(i)%node_set = grnod_id
            guide(i)%part_set = grpart_id
            guide(i)%istiff = istiff
            guide(i)%stfac = stfac
            guide(i)%fric = fric
            guide(i)%eltype = 0
            guide(i)%n_remote_proc = 0
            guide(i)%proc = 1
!
            ncont = igrnod(grnod_id)%nentity
            allocate(guide(i)%cont(ncont))
            guide(i)%ncont = ncont
!
            guide(i)%cont(1:ncont)%update = 0
            guide(i)%cont(1:ncont)%active_segment = 0 
            guide(i)%cont(1:ncont)%anchor_node = 0
            guide(i)%cont(1:ncont)%node(1) = 0
            guide(i)%cont(1:ncont)%node(2) = 0
            guide(i)%cont(1:ncont)%node_next(1) = 0
            guide(i)%cont(1:ncont)%node_next(2) = 0
            guide(i)%cont(1:ncont)%node_iremote(1) = 0
            guide(i)%cont(1:ncont)%node_iremote(2) = 0
            guide(i)%cont(1:ncont)%node_iremote(3) = 0
            guide(i)%cont(1:ncont)%node_iremote(4) = 0
            guide(i)%cont(1:ncont)%dist = ep20
            guide(i)%cont(1:ncont)%forc(1) = zero
            guide(i)%cont(1:ncont)%forc(2) = zero
            guide(i)%cont(1:ncont)%forc(3) = zero
            guide(i)%cont(1:ncont)%forc_ad(1) = zero
            guide(i)%cont(1:ncont)%forc_ad(2) = zero
            guide(i)%cont(1:ncont)%forc_ad(3) = zero                       
!
!           Loop on each contact node         
            if (grnod_id > 0) then
              do j=1, ncont
                guide(i)%cont(j)%anchor_node = igrnod(grnod_id)%entity(j)
              end do
            end if

!           Loop on connected parts - couting of guides per part  
            if (grpart_id > 0) then
              do j=1, igrpart(grpart_id)%nentity
                l = igrpart(grpart_id)%entity(j)
                npart_to_guide(l) = npart_to_guide(l) + 1
                size_part_to_guide = size_part_to_guide + 1
              end do
            end if
!              
          end do
!          
!-------------------------------------------------------------------------------------------          
!         Contruction of mapping part -> guide 
!-------------------------------------------------------------------------------------------         
          allocate(add_part_to_guide(npart+1))
          add_part_to_guide(1:npart+1) = 0
          add_part_to_guide(1) = 1
          do i = 1, npart
            add_part_to_guide(i+1) = add_part_to_guide(i) + npart_to_guide(i)
          end do
!
          allocate(part_to_guide(size_part_to_guide))
          allocate(cpt_part_to_guide(npart))
          part_to_guide(1:size_part_to_guide) = 0
          cpt_part_to_guide(1:npart) = add_part_to_guide(1:npart)
          do i = 1, nguided_cable
            grpart_id = guide(i)%part_set
            if (grpart_id > 0) then
              do j=1, igrpart(grpart_id)%nentity
                l = igrpart(grpart_id)%entity(j)
                part_to_guide(cpt_part_to_guide(l)) = i
                cpt_part_to_guide(l) = cpt_part_to_guide(l) + 1
              end do
            endif  
          enddo  
!
!-------------------------------------------------------------------------------------------           
!         Loop on 1D elements - initial projection on segment to determine initial segment connection
!                             - couting of number of elements per guide          
!-------------------------------------------------------------------------------------------            
!
          guide_nb_elem(1:nguided_cable) = 0       
          allocate(tagnod_guide(numnod))
          allocate(listnod_guide(numnod))
          allocate(common_nodes(nguided_cable,nguided_cable))
          tagnod_guide(1:numnod) = 0
          listnod_guide(1:numnod) = 0
          common_nodes(1:nguided_cable,1:nguided_cable) = 0
          compt_node_guide = 0
          nb_common_nodes = 0
!
!         Spring elements                             
          do i=1,numelr
            if (npart_to_guide(ipartr(i)) > 0) then
              node1 = ixr(2,i)
              node2 = ixr(3,i)
              do j=add_part_to_guide(ipartr(i)),add_part_to_guide(ipartr(i)+1)-1
                ig = part_to_guide(j)
                guide(ig)%eltype = 6 ! spring element
                guide_nb_elem(ig) = guide_nb_elem(ig) + 1  
                do k = 1,guide(ig)%ncont 
                  anchor_node = guide(ig)%cont(k)%anchor_node
                  call dist_node_segment(node1,node2,anchor_node,numnod,x,dist)
                  if (dist < guide(ig)%cont(k)%dist) then
                    guide(ig)%cont(k)%dist = dist
                    guide(ig)%cont(k)%node(1) = ixr(2,i)
                    guide(ig)%cont(k)%node(2) = ixr(3,i)
                  endif  
                enddo 
                do k=1,2
                  if (tagnod_guide(ixr(k+1,i)) == 0) then
                    tagnod_guide(ixr(k+1,i)) = ig
                    compt_node_guide = compt_node_guide + 1
                    listnod_guide(compt_node_guide) = ixr(k+1,i)
                  elseif (tagnod_guide(ixr(k+1,i)) /= ig) then
                    nb_common_nodes = nb_common_nodes + 1
                    common_nodes(ig, tagnod_guide(ixr(k+1,i))) = 1
                    common_nodes(tagnod_guide(ixr(k+1,i)), ig) = 1
!                   common node is attached to guide with smallest id                    
                    if (ig < tagnod_guide(ixr(k+1,i))) tagnod_guide(ixr(k+1,i)) = ig
                  endif
                enddo
              enddo
            endif  
          enddo
!
!         Beam elements    
          do i=1,numelp
            if (npart_to_guide(ipartp(i)) > 0) then
              node1 = ixp(2,i)
              node2 = ixp(3,i)
              do j=add_part_to_guide(ipartp(i)),add_part_to_guide(ipartp(i)+1)-1
                ig = part_to_guide(j)
                guide(ig)%eltype = 5 ! beam element
                guide_nb_elem(ig) = guide_nb_elem(ig) + 1
                do k = 1,guide(ig)%ncont 
                  anchor_node = guide(ig)%cont(k)%anchor_node
                  call dist_node_segment(node1,node2,anchor_node,numnod,x,dist)
                  if (dist < guide(ig)%cont(k)%dist) then
                    guide(ig)%cont(k)%dist = dist
                    guide(ig)%cont(k)%node(1) = ixp(2,i)
                    guide(ig)%cont(k)%node(2) = ixp(3,i)
                  endif  
                enddo
                do k=1,2
                  if (tagnod_guide(ixp(k+1,i)) == 0) then
                    tagnod_guide(ixp(k+1,i)) = ig
                    compt_node_guide = compt_node_guide + 1
                    listnod_guide(compt_node_guide) = ixp(k+1,i)
                  elseif (tagnod_guide(ixp(k+1,i)) /= ig) then
                    nb_common_nodes = nb_common_nodes + 1        
                    common_nodes(ig, tagnod_guide(ixp(k+1,i))) = 1
                    common_nodes(tagnod_guide(ixp(k+1,i)), ig) = 1
!                   common node is attached to guide with smallest id   
                    if (ig < tagnod_guide(ixp(k+1,i))) tagnod_guide(ixp(k+1,i)) = ig
                  endif
                enddo  
              enddo
            endif  
          enddo       
!
!         Truss elements    
          do i=1,numelt
            if (npart_to_guide(ipartt(i)) > 0) then
              node1 = ixt(2,i)
              node2 = ixt(3,i)
              do j=add_part_to_guide(ipartt(i)),add_part_to_guide(ipartt(i)+1)-1
                ig = part_to_guide(j)
                guide(ig)%eltype = 4 ! truss element
                guide_nb_elem(ig) = guide_nb_elem(ig) + 1
                do k = 1,guide(ig)%ncont 
                  anchor_node = guide(ig)%cont(k)%anchor_node
                  call dist_node_segment(node1,node2,anchor_node,numnod,x,dist)
                  if (dist < guide(ig)%cont(k)%dist) then
                    guide(ig)%cont(k)%dist = dist
                    guide(ig)%cont(k)%node(1) = ixt(2,i)
                    guide(ig)%cont(k)%node(2) = ixt(3,i)
                  endif  
                enddo
                do k=1,2
                  if (tagnod_guide(ixt(k+1,i)) == 0) then
                    tagnod_guide(ixt(k+1,i)) = ig
                    compt_node_guide = compt_node_guide + 1
                    listnod_guide(compt_node_guide) = ixt(k+1,i)
                  elseif (tagnod_guide(ixt(k+1,i)) /= ig) then
                    nb_common_nodes = nb_common_nodes + 1        
                    common_nodes(ig, tagnod_guide(ixt(k+1,i))) = 1
                    common_nodes(tagnod_guide(ixt(k+1,i)), ig) = 1
!                   common node is attached to guide with smallest id
                    if (ig < tagnod_guide(ixt(k+1,i))) tagnod_guide(ixt(k+1,i)) = ig
                  endif
                enddo  
              enddo
            endif  
          enddo
!                                          
!-------------------------------------------------------------------------------------------           
!         Determination of the second segment and of nodes before and after segments
!-------------------------------------------------------------------------------------------           
!
          do ig = 1, nguided_cable
            do i = 1, guide(ig)%ncont
              node1 = guide(ig)%cont(i)%node(1)
              node2 = guide(ig)%cont(i)%node(2)
              anchor_node = guide(ig)%cont(i)%anchor_node
              dist1 = ep20
              dist2 = ep20
              if (guide(ig)%eltype ==4) then
!               find previous and next nodes connected to segment (node1-node2) - truss elements
                call find_prev_next_nodes(ig,node1,node2,anchor_node,numnod,0,                             &
                                          snod2el1d,knod2el1d,nod2el1d,numelt,nixt,                        &
                                          ixt,ipartt,npart,size_part_to_guide,add_part_to_guide,           &
                                          part_to_guide,x,nodes_found,nodes_next_found,active_segment)          
              elseif (guide(ig)%eltype ==5) then
!               find previous and next nodes connected to segment (node1-node2) - beam elements
                call find_prev_next_nodes(ig,node1,node2,anchor_node,numnod,numelt,                        &
                                          snod2el1d,knod2el1d,nod2el1d,numelp,nixp,                        &
                                          ixp,ipartp,npart,size_part_to_guide,add_part_to_guide,           &
                                          part_to_guide,x,nodes_found,nodes_next_found,active_segment)                                          
              elseif (guide(ig)%eltype ==6) then
!               find previous and next nodes connected to segment (node1-node2) - spring elements
                call find_prev_next_nodes(ig,node1,node2,anchor_node,numnod,numelp+numelt,                 &
                                          snod2el1d,knod2el1d,nod2el1d,numelr,nixr,                        &
                                          ixr,ipartr,npart,size_part_to_guide,add_part_to_guide,           &
                                          part_to_guide,x,nodes_found,nodes_next_found,active_segment)                                                            
              endif                                         
!             storage of found nodes              
              guide(ig)%cont(i)%node(1:3) = nodes_found(1:3) 
              guide(ig)%cont(i)%node_next(1:2) = nodes_next_found(1:2)       
              guide(ig)%cont(i)%active_segment = active_segment
            end do
          end do    
!          
!-------------------------------------------------------------------------------------------          
!         Contruction of groups of guides for domain decomposition
!         if guide connected to same part  they are put in same group         
!-------------------------------------------------------------------------------------------
!          
          if (nspmd > 1) then
!
            guide_igroup(1:nguided_cable) = 0     
            compt_group = 0  
!          
            do i = 1, npart
              if (npart_to_guide(i) > 0) then
                compt =0
                igroup_min = nguided_cable + 1
                do j=add_part_to_guide(i),add_part_to_guide(i+1)-1
                  ig = part_to_guide(j)
                  if (guide_igroup(ig) > 0) then
                    compt = compt + 1
                    igroup_min = min(igroup_min,guide_igroup(ig))
                  endif  
                end do        
                if (compt == 0) then
!                 New guide group - all guides affected to new group                   
                  compt_group = compt_group + 1
                  do j=add_part_to_guide(i),add_part_to_guide(i+1)-1
                    ig = part_to_guide(j)
                    guide_igroup(ig) = compt_group
                  end do
                else
!                 Existing guide groups - assignation to this group  
                  do j=add_part_to_guide(i),add_part_to_guide(i+1)-1
                    ig = part_to_guide(j)
                    if (guide_igroup(ig) /= igroup_min) then
!                     group of guide is merged in igroup_min
                      do k = 1, nguided_cable
                        if (guide_igroup(k) == guide_igroup(ig)) then
                          guide_igroup(k) = igroup_min
                        end if
                      end do
                    endif
                  end do
                endif  
              end if
            end do   
!
!           guides with common nodes are put in the same group
            if (nb_common_nodes > 0) then
              do i = 1, nguided_cable
                do j = i+1, nguided_cable
                  if (common_nodes(i,j) == 1) then
                    common_nodes(i,j) = 0
                    common_nodes(j,i) = 0      
                    if (guide_igroup(i) /= guide_igroup(j)) then
                      ig1 = i
                      ig2 = j
                      if (guide_igroup(j) < guide_igroup(i)) then
                        ig1 = j
                        ig2 = i
                      end if
!                     merge in the grou pwith smallest id
                      do k = 1, nguided_cable
                        if (guide_igroup(k) == guide_igroup(ig2)) then
                          guide_igroup(k) = guide_igroup(ig1)
                        end if
                      end do
                    end if
                  end if
                end do  
              end do  
            endif       
!          
!           Update of number of guide group after merge of groupd
            nb_guide_group = 0
            do i = 1, nguided_cable
              nb_guide_group = max(nb_guide_group,guide_igroup(i))
            end do   
!
            part_to_guide_group(1:npart) = 0
            do i = 1, npart
              if (npart_to_guide(i) > 0) then
!               group of guide is the group of the first guide connected to the part              
                ig = part_to_guide(add_part_to_guide(i)) 
                part_to_guide_group(i) = guide_igroup(ig)
              endif
            enddo    
!
          endif              
!          
!-------------------------------------------------------------------------------------------          
!         Tag of elments for domain decomposition per guide group     
!------------------------------------------------------------------------------------------- 
!
          if (nspmd > 1) then          
!          
            do igroup = 1, nb_guide_group
!  
              group_nb_elem = 0          
!             counting of number of elements in the guide group       
              do ig = 1, nguided_cable
                if (guide_igroup(ig) == igroup) then
                  group_nb_elem = group_nb_elem + guide_nb_elem(ig)
                end if
              end do
!
              allocate(cc_elem(group_nb_elem))            
              cc_elem(1:group_nb_elem) = 0
              compt = 0
!  
!             tag of number of elements in the guide group   
!               
!             Beam elements
              do i=1,numelp
                if (part_to_guide_group(ipartp(i)) == igroup) then
                  compt = compt + 1
                  cc_elem(compt) = numels+numelq+numelc+i
                end if
              end do     
!             Truss elements
              do i=1,numelt
                if (part_to_guide_group(ipartt(i)) == igroup) then
                  compt = compt + 1
                  cc_elem(compt) = numels+numelq+numelc+numelp+i
                end if
              end do            
!             Spring elements
              do i=1,numelr
                if (part_to_guide_group(ipartr(i)) == igroup) then
                  compt = compt + 1
                  cc_elem(compt) = numels+numelq+numelc+numelp+numelt+i
                end if
              end do      
!   
              call c_prevent_decomposition(compt,cc_elem)         
              deallocate(cc_elem)
!            
            enddo       
!
          endif
!-------------------------------------------------------------------------------------------          
!         Construction of node list for spmd communications    
!------------------------------------------------------------------------------------------- 
!
          guide(1:nguided_cable)%nb_nodes = 0 
          if (nspmd > 1) then          
!         
!           First step counting of nodes per guide (anchor + cable nodes)            
            do i = 1, compt_node_guide
              inod = listnod_guide(i)  
              do j = 1, nguided_cable
                if (tagnod_guide(inod) == j) then
                  guide(j)%nb_nodes = guide(j)%nb_nodes + 1
                end if
              end do           
            enddo
!
            do j = 1, nguided_cable
              guide(j)%nb_nodes = guide(j)%nb_nodes + guide(j)%ncont
              allocate(guide(j)%nodes(guide(j)%nb_nodes))
            end do            
!
!           Second step - filling of list of nodes per guide             
            guide(1:nguided_cable)%nb_nodes = 0
!           Anchor nodes
             do j = 1, nguided_cable
              do k = 1, guide(j)%ncont
                guide(j)%nb_nodes = guide(j)%nb_nodes + 1
                guide(j)%nodes(guide(j)%nb_nodes) = guide(j)%cont(k)%anchor_node
              end do
            end do            
!           Cable nodes            
            do i = 1, compt_node_guide
              inod = listnod_guide(i)  
              do j = 1, nguided_cable
                if (tagnod_guide(inod) == j) then
                  guide(j)%nb_nodes = guide(j)%nb_nodes + 1
                  guide(j)%nodes(guide(j)%nb_nodes) = inod
                end if
              end do           
            enddo           
!
          endif                          
!          
!------------------------------------------------------------------------------------------- 
!          
!-------------------------------------------------------------------------------------------                        
!
          deallocate(npart_to_guide)
          deallocate(add_part_to_guide)
          deallocate(cpt_part_to_guide)
          deallocate(part_to_guide)    
          deallocate(tagnod_guide)  
          deallocate(common_nodes) 
          deallocate(listnod_guide)          
!
! ----------------------------------------------------------------------------------------------------------------------
1000      format(/                                                                     &
               '      GUIDED_CABLE DEFINITIONS '/                                      &
               '      ---------------------- ')   

1100      format(/5X,'GUIDED_CABLE ID',I10,1X,A                                        &
                 /5X,'SET OF NODES  . . . . . . . . . . . . . .',I10                   &
                 /5X,'SET OF PARTS  . . . . . . . . . . . . . .',I10                   &
                 /5X,'CONTACT STIFFNESS TYPE  . . . . . . . . .',I10                   &
                 /5X,'STIFFNESS SCALE FACTOR  . . . . . . . . .',1PG14.4               &
                 /5X,'FRICTION COEFFICIENT  . . . . . . . . . .',1PG14.4)

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_read_guided_cable
      end module hm_read_guided_cable_mod