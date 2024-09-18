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
!! \brief This subroutine is computing initial volume fraction with polygon clipping
!! \details the user polygon is provided with /SUF/SEG option and used to clip elem polygons (quads or trias)
!! \details algo first searches for mesh nodes inside/outside the surface, the clipping is used if at least one but not all nodes are inside the user polygon
!! \details STEP 1 :elem is inside user polygon : if and only all its nodes are inside it
!! \details STEP 2 :elem is outside suer polygon : if their encompassing boxes are not intersecting
!! \details STEP 3 : for all other case compute clipping and decide (no clip : outside,   clip area > 0.0 : cut)
      !||====================================================================
      !||    init_inivol_2d_polygons    ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||--- called by ------------------------------------------------------
      !||    init_inivol                ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    inivol_def_mod             ../starter/share/modules1/inivol_mod.F
      !||====================================================================
      subroutine init_inivol_2D_polygons( &
                                i_inivol  ,      idc,           mat_param,            &
                                NUM_INIVOL,   inivol,               nsurf,   igrsurf, &
                                nparg     ,   ngroup,               iparg,    numnod, &
                                numeltg   ,    nixtg,                ixtg,            &
                                numelq    ,     nixq,                 ixq,            &
                                x         , nbsubmat,    kvol_2d_polygons,    nummat, &
                                sipart    ,    ipart,                                 &
                                i15b      ,    i15h ,                itab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod , only : zero, em10, em06, em02, fourth, third, one, ep9, ep20
      use array_mod , only : array_type, alloc_1d_array, dealloc_1d_array, dealloc_3d_array
      use inivol_def_mod , only : inivol_struct_
      use groupdef_mod , only : surf_
      use elbufdef_mod , only : elbuf_struct_, buf_mat_
      use multi_fvm_mod , only : MULTI_FVM_STRUCT
      use polygon_clipping_mod
      use matparam_def_mod, only : matparam_struct_
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
      integer,intent(in) :: idc                                                !< inivol container
      integer,intent(in) :: i_inivol                                           !< inivol identifier
      integer,intent(in) :: nsurf, num_inivol, nbsubmat, sipart, nummat        !< array sizes
      integer,intent(in) :: nixtg,nixq,numeltg,numelq, numnod, nparg, ngroup   !< array sizes
      integer,intent(in) :: ixtg(nixtg,numeltg), ixq(nixq,numelq)              !< elems node-connectivity
      integer,intent(in) :: iparg(nparg,ngroup)                                !< buffer for elem groups
      integer,intent(in) :: i15b,i15h                                          !< indexes for ipart array
      integer,intent(in) :: ipart(sipart)                                      !< buffer for parts
      my_real, intent(in) :: x(3,numnod)                                       !< node coordinates
      my_real,intent(inout) :: kvol_2d_polygons(nbsubmat,numelq+numeltg)       !< 2d volume fractions (for polygon clipping)
      type (inivol_struct_), dimension(NUM_INIVOL), intent(inout) :: inivol    !< inivol data structure
      type (surf_), dimension(nsurf), intent(in) :: igrsurf                    !< surface buffer
      integer,intent(in) :: itab(numnod)                                       !< user identifier for nodes
      type(matparam_struct_) ,dimension(nummat) ,intent(in) :: mat_param       !< modern buffer for material laws
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real :: vfrac                                                           !< user volume fraction for /INIVOL option (current container)
      my_real ratio                                                              !< ratio of area immersed inside the surface
      my_real XYZ(6)                                                             !<box size xmin ymin zmin, xmax ymax zmax (box encompassing the user polygon)
      my_real xyz_elem(6)                                                        !<box size for current elem
      my_real :: coor_node(2:3)                                                  !< temporary point
      my_real DLy, DLz                                                           !< element of length for margin estimation
      my_real :: sumvf                                                           !< sum of volume fractions
      my_real :: vf_to_substract
      my_real :: vfrac0(nbsubmat)                                                !< volume fraction (initial def from material law)
      my_real :: tol

      integer :: iadbuf                                                          !< index for buffer bufmat
      integer nsegsurf                                                           !< number of segments for a given 2d surface
      integer ng,nel,mtn,imat,icumu,I15_,nft,ity,isolnod,invol,iad,part_id,idp   !< local variables
      integer iseg                                                               !< loop over segments
      integer ii                                                                 !< various loops
      integer :: idsurf,ireversed,isubmat                                        !< user parameter for /INIVOL option (current container)
      integer :: inod1, inod2                                                    !< node identifier of current segment
      integer, allocatable, dimension (:) :: list_quad, list_tria                !<list of elements to retain
      integer icur_q, icur_t                                                     !< cursor for array indexes (quad and tria)
      integer :: iel                                                             !< elem loop for the current group
      integer :: ielg                                                            !< global elem id
      integer :: inod                                                            !< local node 1:4 (quad) or 1:3 (tria)
      integer, allocatable, dimension(:) :: itag_n                               !< tag to mark relevant nodes
      integer :: node_id(1:4)                                                    !< mesh elem connectivity
      integer ipoly                                                              !< curent polygon (loop)
      integer :: isubmat_to_substract
      integer :: mid                                                             !< material internal identifier
      integer :: iter
      integer :: iStatus                                                         !< return code from CLipping Algorithm
      integer :: prod_tag !< product of tag for point of elem mesh               !prod > 0 => elem indise the polygon
                                                                                 !sum = 0 => elem outside the polygon
      logical :: is_quad, is_tria, is_inside
      logical :: is_reversed
      logical :: debug

      type(polygon_) :: user_polygon, elem_polygon
      type(polygon_list_) :: result_list_polygon
      type(polygon_point_) :: point
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! /INIVOL parameters
          idsurf    = inivol(i_inivol)%container(idc)%surf_id
          isubmat   = inivol(i_inivol)%container(idc)%submat_id
          ireversed = inivol(i_inivol)%container(idc)%ireversed
          vfrac     = inivol(i_inivol)%container(idc)%vfrac
          vfrac     = vfrac/ep9
          icumu     = inivol(i_inivol)%container(idc)%icumu
          nsegsurf  = igrsurf(idsurf)%nseg
          part_id   = inivol(i_inivol)%part_id
          is_reversed = .false. ; if(ireversed == 1)is_reversed = .true.

          ! debug output
          debug=.false.

          !---  polygon box (box used in a pre criterion to select relevant nodes with low CPU cost)
          xyz(1:3) = ep20
          xyz(4:6) = -ep20
          call polygon_create( user_polygon, nsegsurf+1)
          user_polygon%numpoint = nsegsurf + 1
          user_polygon%area = zero
          do iseg = 1,nsegsurf
            inod1 = igrsurf(idsurf)%nodes(iseg, 1)
            inod2 = igrsurf(idsurf)%nodes(iseg, 2)
            user_polygon%point(iseg)%y = x(2,inod1)
            user_polygon%point(iseg)%z = x(3,inod1)
            xyz(2) = min (xyz(2), x(2,inod1))
            xyz(2) = min (xyz(2), x(2,inod2))
            xyz(3) = min (xyz(3), x(3,inod1))
            xyz(3) = min (xyz(3), x(3,inod2))
            xyz(5) = max (xyz(5), x(2,inod1))
            xyz(5) = max (xyz(5), x(2,inod2))
            xyz(6) = max (xyz(6), x(3,inod1))
            xyz(6) = max (xyz(6), x(3,inod2))
          end do
          !last node
          user_polygon%point(nsegsurf+1)%y = user_polygon%point(1)%y
          user_polygon%point(nsegsurf+1)%z = user_polygon%point(1)%z
          !margin Y-dir
          DLy = xyz(5)-xyz(2)
          xyz(2) = xyz(2) - max(em10,em02*DLy)
          xyz(5) = xyz(5) + max(em10,em02*DLy)
          !margin Z-dir
          DLz = xyz(6)-xyz(3) !Z-dir
          xyz(3) = xyz(3) - max(em10,em02*DLz)
          xyz(6) = xyz(6) + max(em10,em02*DLz)
          !
          user_polygon%diag = max(DLy,Dlz) !reference length used to normalize tolerance value

          !tolerance shift (gain cpu time avoiding degenerated case : vertice on any edge)
          tol = em06*user_polygon%diag
          do iseg=1,nsegsurf+1
            user_polygon%point(iseg)%y = user_polygon%point(iseg)%y + tol
            user_polygon%point(iseg)%z = user_polygon%point(iseg)%z + tol
          end do

          !---  test elem nodes inside the box (PRE-CRITERION USIN BOX ABOVE)
          !---      loop over related elems, use their nodes : tag set to 1 if inside the box
          if(numnod > 0) then ; allocate(itag_n(numnod)); itag_n(1:numnod) = 0; end if
          do ng=1,ngroup
            mtn     = iparg(1,ng)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            iad     = iparg(4,ng)
            ity     = iparg(5,ng)
            isolnod = iparg(28,ng)
            invol   = iparg(53,ng)
            if (mtn /= 51 .and. mtn /= 151) cycle
            is_quad = .false.
            is_tria = .false.
            if(ity == 7)then
              imat = ixtg(1,1+nft)
              i15_=i15h
              is_tria = .true.
            elseif(ity == 2)then
              imat = ixq(1,1+nft)
              i15_=i15b
              is_quad = .true.
            else
              i15_ = 0
              cycle
            endif
            i15_ = i15_ -1
            ! list elem inside the box. Skip the other (outside the polygon)
            if(is_quad)then
              do iel = 1, nel
                ielg = iel + nft
                idp = ipart(i15_+ielg)
                if(idp == part_id)then
                  do inod=2,5
                    coor_node(2) = x(2,ixq(inod,ielg))
                    coor_node(3) = x(3,ixq(inod,ielg))
                    if(coor_node(2) > xyz(2) .and. coor_node(2) < xyz(5))then
                      if(coor_node(3) > xyz(3) .and. coor_node(3) < xyz(6))then
                        itag_n(ixq(inod,ielg)) = 1
                      end if
                    end if
                  end do
                end if
              end do
            elseif(is_tria)then
              do iel = 1, nel
                ielg = iel + nft
                idp = ipart(i15_+ielg)
                if(idp == part_id)then
                  do inod = 2,4
                    coor_node(2) = x(2,ixtg(inod,ielg))
                    coor_node(3) = x(3,ixtg(inod,ielg))
                    if(coor_node(2) > xyz(2) .and. coor_node(2) < xyz(5))then
                      if(coor_node(3) > xyz(3) .and. coor_node(3) < xyz(6))then
                        itag_n(ixtg(inod,ielg)) = 1
                      end if
                    end if
                  enddo
                end if
              end do
            end if
          enddo ! next element group ng

          ! --- EXACT CRITERION : RETAIN ONLY NODES INSIDE  (computation done only if node is inside the box, which means if node is retaines by pre-criterion)
          do ii=1,numnod
            if(itag_n(ii)==1)then
              point%y=x(2,ii)
              point%z=x(3,ii)
              is_inside = polygon_is_point_inside (user_polygon, point)
              if(.not.is_inside)itag_n(ii)=0
            end if
          end do

          if(debug)then
            print *, "number of nodes inside the user polygon : ",sum(itag_n)
            do ii=1,numnod
              if(itag_n(ii)>0)print *, "   *createmark nodes 1", itab(ii)," ;*nodemarkaddtempmark 1"  !HM TCL SCRIPT TO MARK RETAINED NODES ON SCREEN
            end do
          end if

          ! --- LOOP OVER ELEM AND STATUS USING RETAINED NODES
          if(numelq > 0)then  ; allocate (list_quad(numelq))  ; list_quad(:)=0 ; end if
          if(numeltg > 0)then ; allocate (list_tria(numeltg)) ; list_tria(:)=0 ; end if
          icur_q = 0
          icur_t = 0
          do ng=1,ngroup
            mtn     = iparg(1,ng)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            iad     = iparg(4,ng)
            ity     = iparg(5,ng)
            mid     = iparg(18,ng)
            isolnod = iparg(28,ng)
            invol   = iparg(53,ng)
            if (mtn /= 51 .and. mtn /= 151) cycle
            is_quad = .false.
            is_tria = .false.
            if(ity == 7)then
              imat = ixtg(1,1+nft)
              i15_=i15h
              is_tria = .true.
            elseif(ity == 2)then
              imat = ixq(1,1+nft)
              i15_=i15b
              is_quad = .true.
            else
              i15_ = 0
              cycle
            endif
            i15_ = i15_ -1

            !volume fraction as defined by user material law
            vfrac0(1:nbsubmat) = MAT_PARAM(MID)%MULTIMAT%VFRAC(1:NBSUBMAT)

            ! list elem inside the box. Skip the other (outside the polygon)
            if(is_quad)then
              do iel = 1, nel
                ielg = iel + nft
                idp = ipart(i15_+ielg)
                if(idp == part_id)then
                  node_id(1) =  ixq(2,ielg)
                  node_id(2) =  ixq(3,ielg)
                  node_id(3) =  ixq(4,ielg)
                  node_id(4) =  ixq(5,ielg)
                  prod_tag = itag_n(node_id(1))*itag_n(node_id(2))*itag_n(node_id(3))*itag_n(node_id(4))
                  if(prod_tag > 0)then
                    !elem is inside (may be considered as outside if is_reversed is true)
                    if(icumu == 0)kvol_2d_polygons(isubmat,ielg) = zero
                    ratio = one
                    if(is_reversed) ratio=zero
                    kvol_2d_polygons(isubmat,ielg) = kvol_2d_polygons(isubmat,ielg) + ratio*vfrac !100% inside
                    ! if added volume ratio makes that sum is > 1, then substract from previous filling
                    if(icumu == -1)then
                      sumvf = sum(kvol_2d_polygons(1:nbsubmat,ielg))
                      if (sumvf > one)then
                        if(idc == 1)then
                          ! substract from existing submat (default one)
                          ! pre-condition : sum (vf)= 1.0
                          isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                          vf_to_substract = sumvf-one
                          kvol_2d_polygons(isubmat_to_substract,ielg) = &
                            kvol_2d_polygons(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                        elseif(idc > 1)then
                         ! substract from previous step
                         isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                         vf_to_substract = sumvf-one
                         vf_to_substract = min(vf_to_substract, kvol_2d_polygons(isubmat_to_substract,ielg))
                         kvol_2d_polygons(isubmat_to_substract,ielg) = kvol_2d_polygons(isubmat_to_substract,ielg)-vf_to_substract
                        end if
                      end if
                    end if
                    cycle ! no volume fraction to fill
                  else
                    ! clipping required to calculate ratio inside the polygon
                    icur_q = icur_q +1
                    list_quad(icur_q) = ielg
                  end if
                end if
              end do
            elseif(is_tria)then
              do iel = 1, nel
                ielg = iel + nft
                idp = ipart(i15_+ielg)
                if(idp == part_id)then
                  node_id(1) =  ixq(2,ielg)
                  node_id(2) =  ixq(3,ielg)
                  node_id(3) =  ixq(4,ielg)
                  prod_tag = itag_n(node_id(1))*itag_n(node_id(2))*itag_n(node_id(3))
                  if(prod_tag > 0)then
                    !elem is inside  (may be considered as outside if is_reversed is true)
                    ratio = one
                    if(is_reversed) ratio=zero
                    if(icumu == 0)kvol_2d_polygons(isubmat,ielg) = zero
                    kvol_2d_polygons(isubmat,ielg) = kvol_2d_polygons(isubmat,ielg) + ratio*vfrac !100% inside
                    ! if added volume ratio makes that sum is > 1, then substract from previous filling
                    if(icumu == -1)then
                      sumvf = sum(kvol_2d_polygons(1:nbsubmat,ielg))
                      if (sumvf > one)then
                        if(idc == 1)then
                          ! substract from existing submat (default one)
                          ! pre-condition : sum (vf)= 1.0
                          isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                          vf_to_substract = sumvf-one
                          kvol_2d_polygons(isubmat_to_substract,ielg) = &
                            kvol_2d_polygons(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                        elseif(idc > 1)then
                         ! substract from previous step
                         isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                         vf_to_substract = sumvf-one
                         vf_to_substract = min(vf_to_substract, kvol_2d_polygons(isubmat_to_substract,ielg))
                         kvol_2d_polygons(isubmat_to_substract,ielg) = kvol_2d_polygons(isubmat_to_substract,ielg)-vf_to_substract
                        end if
                      end if
                    end if
                    cycle ! no volume fraction to fill
                  else
                    ! clipping required to calculate ratio inside the polygon
                    icur_q = icur_q +1
                    list_quad(icur_q) = ielg
                  end if
                end if
              end do
            end if
          enddo ! next element group ng

          ! ---RATIO WITH POLYGONAL CLIPPING
          !      list_quad and list_tria are listing elems which required polygonal clipping since
          !      they are partially cut by user polygon
          call polygon_SetClockWise( user_polygon )
            if(debug)then
             write (*,*)"  --> user polygon  "
             do ii = 1, user_polygon%numpoint
               write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,user_polygon%point(ii)%y ,user_polygon%point(ii)%z  !HM TCL SCRIPT TO CHECK USER POLYGON ON SCREEN
             end do
               write (*,*)"   "
            end if
          !
          ! --- QUAD CLIPPING
          call polygon_create(elem_polygon, 5)
          do iel = 1, icur_q
            ielg = list_quad(iel)
            !current quad
            elem_polygon%point(1)%y = x(2,ixq(2,ielg));    elem_polygon%point(1)%z = x(3,ixq(2,ielg))
            elem_polygon%point(2)%y = x(2,ixq(3,ielg));    elem_polygon%point(2)%z = x(3,ixq(3,ielg))
            elem_polygon%point(3)%y = x(2,ixq(4,ielg));    elem_polygon%point(3)%z = x(3,ixq(4,ielg))
            elem_polygon%point(4)%y = x(2,ixq(5,ielg));    elem_polygon%point(4)%z = x(3,ixq(5,ielg))
            elem_polygon%point(5)%y = x(2,ixq(2,ielg));    elem_polygon%point(5)%z = x(3,ixq(2,ielg))
            elem_polygon%numpoint = 5
            ! already oriented Y->Z
            call polygon_SetClockWise( elem_polygon )  !cen be removed if we already computed the area, then just set %area=...
             if(debug)then
              write (*,*)"  current elem  "
              !HM TCL SCRIPT TO CHECK ELEM ON SCREEN
              write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,elem_polygon%point(1)%y ,elem_polygon%point(1)%z
              write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,elem_polygon%point(2)%y ,elem_polygon%point(2)%z
              write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,elem_polygon%point(3)%y ,elem_polygon%point(3)%z
              write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,elem_polygon%point(4)%y ,elem_polygon%point(4)%z
             end if
            !diagonal : max L1 norm
            xyz_elem(2) =                  elem_polygon%point(1)%y   ; xyz_elem(3) =                  elem_polygon%point(1)%z   ;
            xyz_elem(2) = min(xyz_elem(2), elem_polygon%point(2)%y ) ; xyz_elem(3) = min(xyz_elem(3), elem_polygon%point(2)%z ) ;
            xyz_elem(2) = min(xyz_elem(2), elem_polygon%point(3)%y ) ; xyz_elem(3) = min(xyz_elem(3), elem_polygon%point(3)%z ) ;
            xyz_elem(2) = min(xyz_elem(2), elem_polygon%point(4)%y ) ; xyz_elem(3) = min(xyz_elem(3), elem_polygon%point(4)%z ) ;
            xyz_elem(5) =                  elem_polygon%point(1)%y   ; xyz_elem(6) =                  elem_polygon%point(1)%z   ;
            xyz_elem(5) = max(xyz_elem(5), elem_polygon%point(2)%y ) ; xyz_elem(6) = max(xyz_elem(6), elem_polygon%point(2)%z ) ;
            xyz_elem(5) = max(xyz_elem(5), elem_polygon%point(3)%y ) ; xyz_elem(6) = max(xyz_elem(6), elem_polygon%point(3)%z ) ;
            xyz_elem(5) = max(xyz_elem(5), elem_polygon%point(4)%y ) ; xyz_elem(6) = max(xyz_elem(6), elem_polygon%point(4)%z ) ;
            DLy = xyz_elem(5)-xyz_elem(2) !Y-dir
            DLz = xyz_elem(6)-xyz_elem(3) !Z-dir
            elem_polygon%diag = max(DLy,Dlz)       !strictly positive by construction

            ! clipping between user polygon and current quad
            iter=0
            iStatus = -1
             tol = em06*elem_polygon%diag
            do while (iStatus /= 0 .and. iter < 10)
              !tolerance (to avoid vertice on any edge and avoid loops)
              tol=2*tol
              elem_polygon%point(1:5)%y = elem_polygon%point(1:5)%y - tol
              elem_polygon%point(1:5)%z = elem_polygon%point(1:5)%z - tol
              !clipping algorithm
              call Clipping_Weiler_Atherton( elem_polygon, user_polygon, result_list_polygon, iStatus)
              iter=iter+1
            end do

            if (result_list_polygon%num_polygons > 0)then
              ! clipped area > 0 => elem is partially inside the user polygon
              if(icumu == 0)kvol_2d_polygons(isubmat,ielg) = zero
              do ipoly=1,result_list_polygon%num_polygons
                call polygon_SetClockWise( result_list_polygon%polygon(ipoly) )
                ratio = result_list_polygon%polygon(ipoly)%area / elem_polygon%area !partially inside
                if(is_reversed)ratio = one - ratio
                kvol_2d_polygons(isubmat,ielg) = kvol_2d_polygons(isubmat,ielg) + vfrac * ratio
                ! if added volume ratio makes that sum is > 1, then substract from previous filling
                if(icumu == -1)then
                  sumvf = sum(kvol_2d_polygons(1:nbsubmat,ielg))
                  if (sumvf > one)then
                    if(idc == 1)then
                      ! substract from existing submat (default one)
                      ! pre-condition : sum (vf)= 1.0
                      isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                      vf_to_substract = sumvf-one
                      kvol_2d_polygons(isubmat_to_substract,ielg) = &
                        kvol_2d_polygons(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                    elseif(idc > 1)then
                     ! substract from previous step
                     isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                     vf_to_substract = sumvf-one
                     vf_to_substract = min(vf_to_substract, kvol_2d_polygons(isubmat_to_substract,ielg))
                     kvol_2d_polygons(isubmat_to_substract,ielg) = kvol_2d_polygons(isubmat_to_substract,ielg)-vf_to_substract
                    end if
                  end if
                end if
                if(debug)print *, "isubmat,quad partially inside",isubmat,ixq(7,ielg),kvol_2d_polygons(isubmat,ielg)
              enddo
            else
              !no clipped area => elem is outside the user polygon
              !elem is outside  (may be considered as inside if is_reversed is true)
              ratio = zero
              if(is_reversed) ratio=one
              if(icumu == 0)kvol_2d_polygons(isubmat,ielg) = zero
              kvol_2d_polygons(isubmat,ielg) = kvol_2d_polygons(isubmat,ielg) + ratio*vfrac !0% inside
              ! if added volume ratio makes that sum is > 1, then substract from previous filling
              if(icumu == -1)then
                sumvf = sum(kvol_2d_polygons(1:nbsubmat,ielg))
                if (sumvf > one)then
                  if(idc == 1)then
                    ! substract from existing submat (default one)
                    ! pre-condition : sum (vf)= 1.0
                    isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                    vf_to_substract = sumvf-one
                    kvol_2d_polygons(isubmat_to_substract,ielg) = &
                      kvol_2d_polygons(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                  elseif(idc > 1)then
                   ! substract from previous step
                   isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                   vf_to_substract = sumvf-one
                   vf_to_substract = min(vf_to_substract, kvol_2d_polygons(isubmat_to_substract,ielg))
                   kvol_2d_polygons(isubmat_to_substract,ielg) = kvol_2d_polygons(isubmat_to_substract,ielg)-vf_to_substract
                  end if
                end if
              end if
            end if
          enddo
          call polygon_destroy(elem_polygon)

          ! --- TRIA CLIPPING
          call polygon_create(elem_polygon, 4)
          do iel = 1, icur_t
            ielg = list_tria(iel)
            !current quad
            elem_polygon%point(1)%y = x(2,ixtg(2,ielg));    elem_polygon%point(1)%z = x(3,ixtg(2,ielg))
            elem_polygon%point(2)%y = x(2,ixtg(3,ielg));    elem_polygon%point(2)%z = x(3,ixtg(3,ielg))
            elem_polygon%point(3)%y = x(2,ixtg(4,ielg));    elem_polygon%point(3)%z = x(3,ixtg(4,ielg))
            elem_polygon%point(4)%y = x(2,ixtg(2,ielg));    elem_polygon%point(4)%z = x(3,ixtg(2,ielg))
            elem_polygon%numpoint = 4
            ! already oriented Y->Z
            call polygon_SetClockWise( elem_polygon )  !cen be removed if we already computed the area, then just set %area=...
             if(debug)then
              write (*,*)"  current elem  "
              !HM TCL SCRIPT TO CHECK ELEM ON SCREEN
              write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,elem_polygon%point(1)%y ,elem_polygon%point(1)%z
              write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,elem_polygon%point(2)%y ,elem_polygon%point(2)%z
              write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,elem_polygon%point(3)%y ,elem_polygon%point(3)%z
             end if
            !diagonal : max L1 norm
            xyz_elem(2) =                  elem_polygon%point(1)%y   ; xyz_elem(3) =                  elem_polygon%point(1)%z   ;
            xyz_elem(2) = min(xyz_elem(2), elem_polygon%point(2)%y ) ; xyz_elem(3) = min(xyz_elem(3), elem_polygon%point(2)%z ) ;
            xyz_elem(2) = min(xyz_elem(2), elem_polygon%point(3)%y ) ; xyz_elem(3) = min(xyz_elem(3), elem_polygon%point(3)%z ) ;
            xyz_elem(5) =                  elem_polygon%point(1)%y   ; xyz_elem(6) =                  elem_polygon%point(1)%z   ;
            xyz_elem(5) = max(xyz_elem(5), elem_polygon%point(2)%y ) ; xyz_elem(6) = max(xyz_elem(6), elem_polygon%point(2)%z ) ;
            xyz_elem(5) = max(xyz_elem(5), elem_polygon%point(3)%y ) ; xyz_elem(6) = max(xyz_elem(6), elem_polygon%point(3)%z ) ;
            DLy = xyz_elem(5)-xyz_elem(2) !Y-dir
            DLz = xyz_elem(6)-xyz_elem(3) !Z-dir
            elem_polygon%diag = max(DLy,Dlz)       !strictly positive by construction

            ! clipping between user polygon and current quad
            iter=0
            iStatus = -1
             tol = em06*elem_polygon%diag
            do while (iStatus /= 0 .and. iter < 10)
              !tolerance (to avoid vertice on any edge and avoid loops)
              tol=2*tol
              elem_polygon%point(1:4)%y = elem_polygon%point(1:4)%y - tol
              elem_polygon%point(1:4)%z = elem_polygon%point(1:4)%z - tol
              !clipping algorithm
              call Clipping_Weiler_Atherton( elem_polygon, user_polygon, result_list_polygon, iStatus)
              iter=iter+1
            end do

            if (result_list_polygon%num_polygons > 0)then
              ! clipped area > 0 => elem is partially inside the user polygon
              if(icumu == 0)kvol_2d_polygons(isubmat,ielg) = zero
              do ipoly=1,result_list_polygon%num_polygons
                call polygon_SetClockWise( result_list_polygon%polygon(ipoly) )
                ratio = result_list_polygon%polygon(ipoly)%area / elem_polygon%area !partially inside
                if(is_reversed)ratio = one - ratio
                kvol_2d_polygons(isubmat,ielg) = kvol_2d_polygons(isubmat,ielg) + vfrac * ratio
                if(debug)print *, "isubmat,quad partially inside",isubmat,ixq(7,ielg),kvol_2d_polygons(isubmat,ielg)
              enddo
              ! if added volume ratio makes that sum is > 1, then substract from previous filling
              if(icumu == -1)then
                sumvf = sum(kvol_2d_polygons(1:nbsubmat,ielg))
                if (sumvf > one)then
                  if(idc == 1)then
                    ! substract from existing submat (default one)
                    ! pre-condition : sum (vf)= 1.0
                    isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                    vf_to_substract = sumvf-one
                    kvol_2d_polygons(isubmat_to_substract,ielg) = &
                      kvol_2d_polygons(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                  elseif(idc > 1)then
                   ! substract from previous step
                   isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                   vf_to_substract = sumvf-one
                   vf_to_substract = min(vf_to_substract, kvol_2d_polygons(isubmat_to_substract,ielg))
                   kvol_2d_polygons(isubmat_to_substract,ielg) = kvol_2d_polygons(isubmat_to_substract,ielg)-vf_to_substract
                  end if
                end if
              end if
            else
              !no clipped area => elem is outside the user polygon
              !elem is outside  (may be considered as inside if is_reversed is true)
              ratio = zero
              if(is_reversed) ratio=one
              if(icumu == 0)kvol_2d_polygons(isubmat,ielg) = zero
              kvol_2d_polygons(isubmat,ielg) = kvol_2d_polygons(isubmat,ielg) + ratio*vfrac !0% inside
              ! if added volume ratio makes that sum is > 1, then substract from previous filling
              if(icumu == -1)then
                sumvf = sum(kvol_2d_polygons(1:nbsubmat,ielg))
                if (sumvf > one)then
                  if(idc == 1)then
                    ! substract from existing submat (default one)
                    ! pre-condition : sum (vf)= 1.0
                    isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                    vf_to_substract = sumvf-one
                    kvol_2d_polygons(isubmat_to_substract,ielg) = &
                      kvol_2d_polygons(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                  elseif(idc > 1)then
                   ! substract from previous step
                   isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                   vf_to_substract = sumvf-one
                   vf_to_substract = min(vf_to_substract, kvol_2d_polygons(isubmat_to_substract,ielg))
                   kvol_2d_polygons(isubmat_to_substract,ielg) = kvol_2d_polygons(isubmat_to_substract,ielg)-vf_to_substract
                  end if
                end if
              end if
            end if
          enddo
          call polygon_destroy(elem_polygon)


     ! --- deallocate
      if(allocated(itag_n))deallocate(itag_n)
      if(allocated(list_quad))deallocate(list_quad)
      if(allocated(list_tria))deallocate(list_tria)
      call polygon_destroy(user_polygon)
      call polygon_destroy(elem_polygon)

    end subroutine init_inivol_2D_polygons



