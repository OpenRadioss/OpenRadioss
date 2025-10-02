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
!||    i2trivox_mod   ../starter/source/interfaces/inter3d1/i2trivox.F90
!||--- called by ------------------------------------------------------
!||    i2buc1         ../starter/source/interfaces/inter3d1/i2buc1.F
!||====================================================================
      module i2trivox_mod       
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    i2trivox        ../starter/source/interfaces/inter3d1/i2trivox.F90
!||--- called by ------------------------------------------------------
!||    i2buc1          ../starter/source/interfaces/inter3d1/i2buc1.F
!||--- calls      -----------------------------------------------------
!||    i2cor3          ../starter/source/interfaces/inter3d1/i2cor3.F
!||    i2dst3          ../starter/source/interfaces/inter3d1/i2dst3.F
!||    i2dst3_27       ../starter/source/interfaces/inter3d1/i2dst3_27.F
!||--- uses       -----------------------------------------------------
!||    stack_mod       ../starter/share/modules1/stack_mod.F
!||====================================================================
        subroutine i2trivox(nvsiz,numnod,numels,numels10, &
                            numels16,numels20,numelc,numeltg, &
                            nint,noint, &
                            ixs,ixs10,ixs16,ixs20,ixc,ixtg, &
                            iworksh,nsn,nrtm, &
                            ilev,npropgi,npropg,numgeo,npropm,nummat,npart,ignore,cell_nb,nsv,irtl,ipartc,iparttg,& 
                            knod2els,knod2elc,knod2eltg,nod2els,nod2elc,nod2eltg,irect, &
                            igeo,dsearch,bound,tzinf,segment_data, &
                            dmin,thk,thk_part,x,geo,st,pm,stack)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod , only : WP
          use stack_mod , only : stack_ply
          use element_mod , only :nixc,nixtg,nixs,nixs10,nixs16,nixs20
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nvsiz !< size of the prov arrays
          integer, intent(in) :: numnod !< number of all the nodes
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: numels10 !< number of 10-node tetrahedra elements
          integer, intent(in) :: numels16 !< number of solid16 elements
          integer, intent(in) :: numels20 !< number of 20-node hexahedra elements
          integer, intent(in) :: numelc !< number of shell elements
          integer, intent(in) :: numeltg !< number of triangle (shell3n) elements

          integer, intent(in) :: nsn !< number of secondary nodes
          integer, intent(in) :: nrtm !< number of main segments
          integer, intent(in) :: ilev !< level of accuracy for the projection (default is 0, 27 is more accurate)
          integer, intent(in) :: npropgi !< number of integer properties per geometry (integer data)
          integer, intent(in) :: npropg !< number of properties per geometry (real data)
          integer, intent(in) :: numgeo !< number of geometries
          integer, intent(in) :: npropm !< first dimension of pm array
          integer, intent(in) :: nummat !< number of material
          integer, intent(in) :: npart !< number of parts
          integer, intent(in) :: ignore !< interface type2 option
          integer, intent(in) :: nint !< local interface id
          integer, intent(in) :: noint !< global interface id
                 

          integer, dimension(3), intent(in) :: cell_nb !< number of cells in each direction
          integer, dimension(nsn), intent(in) :: nsv !< secondary node ids
          integer, dimension(nsn), intent(inout) :: irtl !< index of the main segment associated to each secondary node
          integer, dimension(numelc), intent(in) :: ipartc !< part id of each shell element
          integer, dimension(numeltg), intent(in) :: iparttg !< part id of each triangle (shell3n) element


          INTEGER KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*)


          integer, dimension(4,nrtm), intent(in) :: irect !< node ids of the main segments
          integer, dimension(npropgi,numgeo), intent(in) :: igeo !< geometry properties (integer data)
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid connectivity
          integer, dimension(nixs10,numels10), intent(in) :: ixs10 !< solid connectivity for 10-node tetrahedra
          integer, dimension(nixs16,numels16), intent(in) :: ixs16 !< solid connectivity for solid16 elements
          integer, dimension(nixs20,numels20), intent(in) :: ixs20 !< solid connectivity for 20-node hexahedra
          integer, dimension(nixc,numelc), intent(in) :: ixc !< shell connectivity
          integer, dimension(nixtg,numeltg), intent(in) :: ixtg !< triangle (shell3n) connectivity
          integer, dimension(3,numelc+numeltg), intent(in) :: iworksh

          real(kind=WP), intent(in) :: dsearch !< search distance for the projection
          real(kind=WP), dimension(6), intent(in) :: bound !< bounding box of the
          real(kind=WP), intent(in) :: tzinf !< influence zone thickness
          real(kind=WP), dimension(nrtm,2), intent(in) :: segment_data !< length & gap of each main segment 
          real(kind=WP), dimension(nsn), intent(in) :: dmin !< minimum distance to the main segments
          real(kind=WP), dimension(numelc+numeltg), intent(in) :: thk !< thickness of each element
          real(kind=WP), dimension(npart), intent(in) :: thk_part !< thickness of each part
          real(kind=WP), dimension(3,numnod), intent(in) :: x !< coordinates of all the nodes   
          real(kind=WP), dimension(npropg,numgeo), intent(in) :: geo
          real(kind=WP), dimension(2,nsn), intent(inout) :: st !< curvilinear abscissa and distance to the segment for each secondary node
          real(kind=WP), dimension(npropm,nummat), intent(in) :: pm !< property / material data

          type(stack_ply), intent(inout) :: stack !< stack data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: j_stok
          integer :: total_cell_nb
          integer :: jx,jy,jz
          integer :: i,j,k,first,last
          integer :: iflag
          integer :: my_cell_id,my_address
          integer :: node_id,nodes_id(4),node_index
          integer, dimension(3) :: ix,ix_min,ix_max
          real(kind=WP), dimension(3,4) :: x_nodes
          real(kind=WP), dimension(3) :: x_node,x_min,x_max
          integer, dimension(:), allocatable :: cell_id,s_node_nb,cell_pointer,s_bucket

          integer, dimension(mvsiz) :: prov_e,prov_n,nsvg
          integer, dimension(mvsiz) :: ix1,ix2,ix3,ix4
          real(kind=WP), dimension(mvsiz) :: x1,x2,x3,x4
          real(kind=WP), dimension(mvsiz) :: y1,y2,y3,y4
          real(kind=WP), dimension(mvsiz) :: z1,z2,z3,z4
          real(kind=WP), dimension(mvsiz) :: xi,yi,zi
          real(kind=WP), dimension(mvsiz) :: x0,y0,z0
          real(kind=WP), dimension(mvsiz) :: nx1,ny1,nz1
          real(kind=WP), dimension(mvsiz) :: nx2,ny2,nz2
          real(kind=WP), dimension(mvsiz) :: nx3,ny3,nz3
          real(kind=WP), dimension(mvsiz) :: nx4,ny4,nz4
          real(kind=WP), dimension(mvsiz) :: p1,p2,p3,p4
          real(kind=WP), dimension(mvsiz) :: lb1,lb2,lb3,lb4
          real(kind=WP), dimension(mvsiz) :: lc1,lc2,lc3,lc4
          real(kind=WP), dimension(mvsiz) :: s,t,gapv
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          total_cell_nb = cell_nb(1)*cell_nb(2)*cell_nb(3)
          allocate(cell_id(nsn)) ! cell id of each S node
          allocate(s_node_nb(total_cell_nb)) ! number of S nodes in each cell
          cell_id(:) = 0 ! initialize the array
          s_node_nb(:) = 0 ! initialize the array
          do i=1,nsn ! loop over the secondary node of the interface
            node_id = nsv(i) ! get the node id
            x_node(1:3) = x(1:3,node_id) ! get the node coordinates
            ix(1:3) = 0
            ! check if the S node is inside the domain
            if (x_node(1)>=bound(1).and.x_node(2)>=bound(2).and.x_node(3)>=bound(3).and. &
              x_node(1)<=bound(4).and.x_node(2)<=bound(5).and.x_node(3)<=bound(6)) then
              ix(1:3) = int(cell_nb(1:3)*(x_node(1:3)-bound(1:3))/(bound(4:6)-bound(1:3)))+1 ! get the cell index
              ix(1:3) = max(1,ix(1:3)) ! ensure the index is at least 1
              ix(1:3) = min(cell_nb(1:3),ix(1:3)) ! ensure the index is at most the number of cells
              cell_id(i) = ix(1) + (ix(2)-1)*cell_nb(1) + (ix(3)-1)*cell_nb(1)*cell_nb(2) ! compute a unique cell id
              s_node_nb(cell_id(i)) = s_node_nb(cell_id(i)) + 1 ! count the number of S nodes in this cell             
            endif
          enddo

          allocate(cell_pointer(total_cell_nb+1)) ! pointer to the first node of each cell in the s_bucket array
          cell_pointer(1) = 0
          do i=2,total_cell_nb+1
            cell_pointer(i) = cell_pointer(i-1) + s_node_nb(i-1)
          enddo
          s_node_nb(:) = 0 ! reuse this array to count the number of nodes in each cell
          allocate(s_bucket(nsn))
          do i=1,nsn ! loop over the secondary node of the interface
            my_cell_id = cell_id(i) ! get the cell id of the current node
            if(my_cell_id>0) then
              s_node_nb(my_cell_id) = s_node_nb(my_cell_id) + 1 ! count the number of nodes already stored in this cell
              my_address = cell_pointer(my_cell_id) + s_node_nb(my_cell_id) ! compute the address in the s_bucket array
              s_bucket(my_address) = i ! store the node in the s_bucket array
            endif
          enddo

          j_stok = 0
          do i=1,nrtm ! loop over the main segments
            nodes_id(1:4) = irect(1:4,i) ! get the node ids of the segment
            x_nodes(1:3,1:4) = x(1:3,nodes_id(1:4)) ! get the coordinates of the nodes
            x_min(1:3) = huge(x_min(1:3))
            x_max(1:3) = -huge(x_max(1:3))
            do j=1,4 ! loop over the nodes of the segment to find the minimum and maximum coordinates
              x_min(1:3) = min(x_min(1:3),x_nodes(1:3,j))
              x_max(1:3) = max(x_max(1:3),x_nodes(1:3,j))
            enddo
            ! add the influence zone
            x_min(1:3) = x_min(1:3) - segment_data(i,2)
            x_max(1:3) = x_max(1:3) + segment_data(i,2)
            ix_min(1:3) = int(cell_nb(1:3)*(x_min(1:3)-bound(1:3))/(bound(4:6)-bound(1:3)))+1
            ix_max(1:3) = int(cell_nb(1:3)*(x_max(1:3)-bound(1:3))/(bound(4:6)-bound(1:3)))+1
   
            ix_min(1:3) = max(1,ix_min(1:3))
            ix_min(1:3) = min(cell_nb(1:3),ix_min(1:3))
            ix_max(1:3) = max(1,ix_max(1:3))
            ix_max(1:3) = min(cell_nb(1:3),ix_max(1:3))

            do jx=ix_min(1),ix_max(1)
              do jy=ix_min(2),ix_max(2)
                do jz=ix_min(3),ix_max(3)
                  my_cell_id = jx + (jy-1)*cell_nb(1) + (jz-1)*cell_nb(1)*cell_nb(2)

                  if(s_node_nb(my_cell_id)>0) then
                    my_address = cell_pointer(my_cell_id)
                    do k=1,s_node_nb(my_cell_id)                    
                      node_index = s_bucket(k+my_address)
                      node_id = nsv(node_index)
                      ! skip if the S node is one of the 4 nodes of the segment
                      if(node_id==nodes_id(1).or.node_id==nodes_id(2).or.node_id==nodes_id(3).or.node_id==nodes_id(4)) cycle
                      x_node(1:3) = x(1:3,node_id)
                      if(x_node(1)<x_min(1).or.x_node(1)>x_max(1)) cycle
                      if(x_node(2)<x_min(2).or.x_node(2)>x_max(2)) cycle
                      if(x_node(3)<x_min(3).or.x_node(3)>x_max(3)) cycle
                      ! here the S node is in the bounding box of the segment + influence zone
                      j_stok = j_stok + 1
                      prov_n(j_stok) = node_index
                      prov_e(j_stok) = i                        
                      if(j_stok==nvsiz) then
                        first = 1                        
                        last = nvsiz
                        j_stok = 0
                        iflag = 0
                      call i2cor3(x     ,irect ,nsv   ,prov_e  ,prov_n, &
                             gapv  ,0       ,tzinf,first,last, &
                             nint    ,ixc   ,  &
                             ixtg  ,thk_part,ipartc,geo   , noint, &
                             ixs   ,ixs10 ,pm    ,thk     ,knod2els, &
                             knod2elc,knod2eltg,nod2els,nod2elc,nod2eltg, &
                             ignore,ixs16 ,ixs20 ,iparttg,igeo,dsearch , &
                             stack%pm , iworksh ,ix1     ,ix2   , &
                             ix3    ,ix4    ,nsvg,x1      ,x2    , &
                             x3     ,x4     ,y1  ,y2      ,y3    , &
                             y4     ,z1     ,z2  ,z3      ,z4    , &
                             xi     ,yi     ,zi  ,iflag )
                        if (ilev == 27) then
                          call i2dst3_27(first,last, &
                              gapv,prov_e ,prov_n,tzinf,irtl,st,dmin, &
                              ignore,irect, &
                              ix3, &
                              ix4,x1 ,x2 ,x3 ,x4 , &
                              y1 ,y2 ,y3 ,y4 ,z1 , &
                              z2 ,z3 ,z4 ,xi ,yi , &
                              zi ,x0 ,y0 ,z0 ,nx1, &
                              ny1,nz1,nx2,ny2,nz2, &
                              nx3,ny3,nz3,nx4,ny4, &
                              nz4,p1 ,p2 ,p3 ,p4 , &
                              lb1,lb2,lb3,lb4,lc1, &
                              lc2,lc3,lc4,s  ,t  )
                        else
                        call i2dst3(first,last, &
                             gapv,prov_e ,prov_n,tzinf,irtl,st,dmin, &
                             ignore, &
                             ix3, &
                             ix4,x1 ,x2 ,x3 ,x4 , &
                             y1 ,y2 ,y3 ,y4 ,z1 , &
                             z2 ,z3 ,z4 ,xi ,yi , &
                             zi ,x0 ,y0 ,z0 ,nx1, &
                             ny1,nz1,nx2,ny2,nz2, &
                             nx3,ny3,nz3,nx4,ny4, &
                             nz4,p1 ,p2 ,p3 ,p4 , &
                             lb1,lb2,lb3,lb4,lc1, &
                             lc2,lc3,lc4,s,t)
                        endif
                      endif
                    enddo
                  endif
                enddo
              enddo
            enddo
          enddo
          if(j_stok/=0) then
            first = 1
            last = j_stok
            j_stok = 0
            iflag = 0
            call i2cor3(x     ,irect ,nsv   ,prov_e  ,prov_n, &
                        gapv  ,0       ,tzinf,first,last, &
                        nint    ,ixc   ,  &
                        ixtg  ,thk_part,ipartc,geo   , noint, &
                        ixs   ,ixs10 ,pm    ,thk     ,knod2els, &
                        knod2elc,knod2eltg,nod2els,nod2elc,nod2eltg, &
                        ignore,ixs16 ,ixs20 ,iparttg,igeo,dsearch , &
                        stack%pm , iworksh ,ix1     ,ix2   , &
                        ix3    ,ix4    ,nsvg,x1      ,x2    , &
                        x3     ,x4     ,y1  ,y2      ,y3    , &
                        y4     ,z1     ,z2  ,z3      ,z4    , &
                        xi     ,yi     ,zi  ,iflag )
            if (ilev == 27) then
              call i2dst3_27(first,last, &
                             gapv,prov_e ,prov_n,tzinf,irtl,st,dmin, &
                             ignore,irect, &
                             ix3, &
                             ix4,x1 ,x2 ,x3 ,x4 , &
                             y1 ,y2 ,y3 ,y4 ,z1 , &
                             z2 ,z3 ,z4 ,xi ,yi , &
                             zi ,x0 ,y0 ,z0 ,nx1, &
                             ny1,nz1,nx2,ny2,nz2, &
                             nx3,ny3,nz3,nx4,ny4, &
                             nz4,p1 ,p2 ,p3 ,p4 , &
                             lb1,lb2,lb3,lb4,lc1, &
                             lc2,lc3,lc4,s  ,t  )
            else
              call i2dst3(first,last, &
                          gapv,prov_e ,prov_n,tzinf,irtl,st,dmin, &
                          ignore, &
                          ix3, &
                          ix4,x1 ,x2 ,x3 ,x4 , &
                          y1 ,y2 ,y3 ,y4 ,z1 , &
                          z2 ,z3 ,z4 ,xi ,yi , &
                          zi ,x0 ,y0 ,z0 ,nx1, &
                          ny1,nz1,nx2,ny2,nz2, &
                          nx3,ny3,nz3,nx4,ny4, &
                          nz4,p1 ,p2 ,p3 ,p4 , &
                          lb1,lb2,lb3,lb4,lc1, &
                          lc2,lc3,lc4,s,t)
            endif
          endif
          deallocate( cell_pointer )
          deallocate( cell_id )
          deallocate( s_bucket )
          deallocate( s_node_nb )

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine i2trivox
      end module i2trivox_mod
