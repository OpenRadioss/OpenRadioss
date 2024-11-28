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
                                i_inivol  ,      idc,           mat_param, GLOBAL_xyz, &
                                NUM_INIVOL,   inivol,               nsurf,    igrsurf, &
                                nparg     ,   ngroup,               iparg,     numnod, &
                                numeltg   ,    nixtg,                ixtg,     igrnod, &
                                numelq    ,     nixq,                 ixq,     ngrnod, &
                                x         , nbsubmat,                kvol,     nummat, &
                                sipart    ,    ipart,               bufsf,     sbufsf, &
                                i15b      ,    i15h ,                itab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod , only : zero, em20, em10, em06, em02, fourth, third, half, one, two, pi, ep9, ep10, ep20
      use array_mod , only : array_type, alloc_1d_array, dealloc_1d_array, dealloc_3d_array
      use inivol_def_mod , only : inivol_struct_
      use groupdef_mod , only : surf_, group_
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
      integer,intent(in) :: sbufsf                                             !< buffer size for surfaces
      integer,intent(in) :: ngrnod                                             !< array size igrnod
      my_real, intent(in) :: x(3,numnod)                                       !< node coordinates
      my_real,intent(inout) :: kvol(nbsubmat,numelq+numeltg)                   !< volume fractions (for polygon clipping)
      my_real,intent(in) :: bufsf(sbufsf)                                      !< buffer for surfaces
      my_real,intent(in) :: GLOBAL_xyz(6)                                      !< global min,max (/surf/plane)
      type (inivol_struct_), dimension(NUM_INIVOL), intent(inout) :: inivol    !< inivol data structure
      type (surf_), dimension(nsurf), intent(in) :: igrsurf                    !< surface buffer
      integer,intent(in) :: itab(numnod)                                       !< user identifier for nodes
      type(matparam_struct_) ,dimension(nummat) ,intent(in) :: mat_param       !< modern buffer for material laws
      type (group_)  , dimension(ngrnod)  :: igrnod                            !< data structure for groups of nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real :: vfrac                                                           !< user volume fraction for /INIVOL option (current container)
      my_real ratio                                                              !< ratio of area immersed inside the surface
      my_real XYZ(6)                                                             !<box size xmin ymin zmin, xmax ymax zmax (box encompassing the user polygon)
      my_real xyz_elem(6)                                                        !<box size for current elem
      my_real :: coor_node(2:3)                                                  !< temporary point
      my_real DL, DLy, DLz                                                       !< element of length for margin estimation
      my_real :: sumvf                                                           !< sum of volume fractions
      my_real :: vf_to_substract
      my_real :: vfrac0(nbsubmat)                                                !< volume fraction (initial def from material law)
      my_real :: tol
      my_real :: YP1,ZP1,YP2,ZP2                                                 !< planar surface definition
      my_real :: normal(3), tangent(3)                                           !< normal and tangent to the planar surface
      my_real :: bb,cc,nn,yg,zg                                                  !< superellipse surface definition
      my_real skw(9)                                                             !< skew data for superell transformation
      my_real :: tmp(3)                                                          !< temporary array
      my_real :: theta

      integer :: iadbuf                                                          !< index for buffer bufmat
      integer nsegsurf                                                           !< number of segments for a given 2d surface
      integer npoints                                                            !< number of points for ordered list of nodes
      integer ng,nel,mtn,imat,icumu,I15_,nft,ity,isolnod,invol,iad,part_id,idp   !< local variables
      integer iseg                                                               !< loop over segments
      integer ii,ipt                                                             !< various loops
      integer :: idsurf,idgrnod,ireversed,isubmat                                !< user parameter for /INIVOL option (current container)
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
      integer :: sum_tag                                                         !sum = 0 => elem outside the polygon
      integer :: iad0                                                            !< index for buffer bufsf
      integer :: npt_superellipse                                                !< number of points for superellipse

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
          idgrnod   =  inivol(i_inivol)%container(idc)%grnod_id  !2d : ordered list of nodes
          isubmat   = inivol(i_inivol)%container(idc)%submat_id
          ireversed = inivol(i_inivol)%container(idc)%ireversed
          vfrac     = inivol(i_inivol)%container(idc)%vfrac
          vfrac     = vfrac/ep9
          icumu     = inivol(i_inivol)%container(idc)%icumu
          nsegsurf  = 0
          npoints   = 0
          if(idsurf > 0) nsegsurf = igrsurf(idsurf)%nseg
          if(idgrnod > 0) npoints = igrnod(idgrnod)%nentity
          part_id   = inivol(i_inivol)%part_id
          is_reversed = .false. ; if(ireversed == 1)is_reversed = .true.

          ! debug output
          debug=.false.

          !---  polygon box (box used in a pre criterion to select relevant nodes with low CPU cost)
          xyz(1:3) = ep20
          xyz(4:6) = -ep20
          ! SURF_TYPE = 0         : SEGMENTS
          ! SURF_TYPE = 100       : SUPER-ELLIPSOIDE MADYMO.
          ! SURF_TYPE = 101       : SUPER-ELLIPSOIDE RADIOSS.
          ! SURF_TYPE = 200       : INFINITE PLANE
          if(idsurf > 0)then
            if (igrsurf(idsurf)%type == 0 ) then
             ! POLYGON DEFINED WITH USER SEGMENTS
             !building polygon
             call polygon_create( user_polygon, nsegsurf+1+1)   !+1 in case of automatic closure
             user_polygon%numpoint = nsegsurf + 1
             user_polygon%area = zero
             do iseg = 1,nsegsurf
               inod1 = igrsurf(idsurf)%nodes(iseg, 1)
               inod2 = igrsurf(idsurf)%nodes(iseg, 2)
               user_polygon%point(iseg)%y = x(2,inod1)
               user_polygon%point(iseg)%z = x(3,inod1)
               xyz(2) = min (xyz(2), x(2,inod1))
               xyz(3) = min (xyz(3), x(3,inod1))
               xyz(5) = max (xyz(5), x(2,inod1))
               xyz(6) = max (xyz(6), x(3,inod1))
             end do
             !automatic closure (if needed)
             if( igrsurf(idsurf)%nodes(1, 1) /= igrsurf(idsurf)%nodes(nsegsurf, 2) )then
               user_polygon%point(nsegsurf+1)%y = x(2,1)
               user_polygon%point(nsegsurf+1)%z = x(3,1)
               xyz(2) = min (xyz(2), x(2,inod1))
               xyz(3) = min (xyz(3), x(3,inod1))
               xyz(5) = max (xyz(5), x(2,inod1))
               xyz(6) = max (xyz(6), x(3,inod1))
             end if

            elseif(igrsurf(idsurf)%type == 200)then
              iad0 = igrsurf(idsurf)%iad_bufr
              !xp1 = bufsf(iad0+1)
              yp1 = bufsf(iad0+2)
              zp1 = bufsf(iad0+3)
              !xp2 = bufsf(iad0+4)
              yp2 = bufsf(iad0+5)
              zp2 = bufsf(iad0+6)
              !tolerance (in order to avoid degenerated case)
              DL=one !do not use vectos in plane definition, n=(1,0,0) and n=(1e20,0,0) would define same plane but provides unreliable tolerances
              DL=-two*em06*DL
              yp1 = yp1 + DL
              zp1 = zp1 + DL
              !normal vector
              normal(1)=0
              normal(2)=yp2-yp1
              normal(3)=zp2-zp1
              !normalized normal
              DL = max(em20,sqrt(normal(2)*normal(2)+normal(3)*normal(3)))
              normal(2)=normal(2)/DL
              normal(3)=normal(3)/DL
              !tangent vector
              tangent(1)=0
              tangent(2)=normal(3)
              tangent(3)=-normal(2)
              !building corresponding polygon (box)
              call polygon_create( user_polygon, 4+1)
              user_polygon%numpoint = 4 + 1
              user_polygon%area = zero
              DL=(one+fourth)*max(abs(GLOBAL_xyz(5)-GLOBAL_xyz(2)),abs(GLOBAL_xyz(6)-GLOBAL_xyz(3)))
              user_polygon%point(1)%y = YP1 + half*DL*tangent(2)
              user_polygon%point(1)%z = ZP1 + half*DL*tangent(3)
              user_polygon%point(2)%y = user_polygon%point(1)%y + DL*normal(2)
              user_polygon%point(2)%z = user_polygon%point(1)%z + DL*normal(3)
              user_polygon%point(3)%y = user_polygon%point(2)%y - DL*tangent(2)
              user_polygon%point(3)%z = user_polygon%point(2)%z - DL*tangent(3)
              user_polygon%point(4)%y = user_polygon%point(3)%y - DL*normal(2)
              user_polygon%point(4)%z = user_polygon%point(3)%z - DL*normal(3)
              nsegsurf = 4
              xyz(2) = min(user_polygon%point(1)%y,user_polygon%point(2)%y,user_polygon%point(3)%y,user_polygon%point(4)%y)
              xyz(3) = min(user_polygon%point(1)%z,user_polygon%point(2)%z,user_polygon%point(3)%z,user_polygon%point(4)%z)
              xyz(5) = max(user_polygon%point(1)%y,user_polygon%point(2)%y,user_polygon%point(3)%y,user_polygon%point(4)%y)
              xyz(6) = max(user_polygon%point(1)%z,user_polygon%point(2)%z,user_polygon%point(3)%z,user_polygon%point(4)%z)
              if(debug)then
                print *, "box (/surf/plane)"
                write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,user_polygon%point(1)%y ,user_polygon%point(1)%z
                write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,user_polygon%point(2)%y ,user_polygon%point(2)%z
                write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,user_polygon%point(3)%y ,user_polygon%point(3)%z
                write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,user_polygon%point(4)%y ,user_polygon%point(4)%z
              endif

            elseif(igrsurf(idsurf)%type == 101)then
              iad0 = igrsurf(idsurf)%iad_bufr
              !aa = bufsf(iad0+1)
              bb = bufsf(iad0+2)
              cc = bufsf(iad0+3)
              !xg = bufsf(iad0+4)
              yg = bufsf(iad0+5)
              zg = bufsf(iad0+6)
              bb=bb*(one-em10)
              cc=cc*(one-em10)
              !skw(1)=bufsf(iad0+7)
              skw(2)=bufsf(iad0+8)
              skw(3)=bufsf(iad0+9)
              !skw(4)=bufsf(iad0+10)
              skw(5)=bufsf(iad0+11)
              skw(6)=bufsf(iad0+12)
              !skw(7)=bufsf(iad0+13)
              skw(8)=bufsf(iad0+14)
              skw(9)=bufsf(iad0+15)
              nn=bufsf(iad0+36)

              !tolerance
              DL=two*max(bb,cc)
              DL=em06*DL
              yp1 = yp1 + DL
              zp1 = zp1 + DL

              !super-ellipse discretization (in order to avoid degenerated case)
              npt_superellipse = 256
              call polygon_create( user_polygon, npt_superellipse+1)
              user_polygon%numpoint = npt_superellipse + 1
              user_polygon%area = zero
              if(debug)write(*,*)"building super-ellipse"
              tmp(1) = two*pi/npt_superellipse
              do ii=1,npt_superellipse
                theta = tmp(1)*ii
                tmp(2) = bb * sign(one,cos(theta)) * (abs(cos(theta)))**(two/nn)
                tmp(3) = cc * sign(one,sin(theta)) * (abs(sin(theta)))**(two/nn)
                user_polygon%point(ii)%y = yg + skw(5)*tmp(2) + skw(6)*tmp(3)
                user_polygon%point(ii)%z = zg + skw(8)*tmp(2) + skw(9)*tmp(3)
                xyz(2) = min (xyz(2), user_polygon%point(ii)%y)
                xyz(3) = min (xyz(3), user_polygon%point(ii)%z)
                xyz(5) = max (xyz(5), user_polygon%point(ii)%y)
                xyz(6) = max (xyz(6), user_polygon%point(ii)%z)
                if(debug)write (*,FMT='(A,3F45.35)') "  *createnode ",0.0,user_polygon%point(ii)%y ,user_polygon%point(ii)%z
              end do
              nsegsurf = npt_superellipse

            endif ! igrsurf(idsurf)%type

          elseif(idgrnod > 0)then
             ! ORDERED LIST OF NODES (2d only, pre-condition only verified by Reader)
             !building polygon
             call polygon_create( user_polygon, npoints+1)   !+1 in case of automatic closure
             user_polygon%numpoint = npoints+1
             user_polygon%area = zero
             nsegsurf = npoints
             do ipt = 1, igrnod(idgrnod)%nentity
               inod1 = igrnod(idgrnod)%entity(ipt)
               user_polygon%point(ipt)%y = x(2,inod1)
               user_polygon%point(ipt)%z = x(3,inod1)
               xyz(2) = min (xyz(2), x(2,inod1))
               xyz(3) = min (xyz(3), x(3,inod1))
               xyz(5) = max (xyz(5), x(2,inod1))
               xyz(6) = max (xyz(6), x(3,inod1))
             end do
          end if ! idsurf>0 or idgrnod>0

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
                  sum_tag = itag_n(node_id(1))+itag_n(node_id(2))+itag_n(node_id(3))+itag_n(node_id(4))
                  if(sum_tag == 4 .or. sum_tag == 0)then
                    !sum_tag == 4 : elem is inside  (may be considered as outside if is_reversed is true)
                    !sum_tag == 0 : elem is outside (may be considered as inside if is_reversed is true)
                    ! reversed option (Iopt)
                    if(is_reversed .and. sum_tag == 4)then
                       cycle
                    elseif(.not.is_reversed .and. sum_tag == 0)then
                       cycle
                    else
                    end if
                     ratio = one
                     if(icumu == 0)kvol(isubmat,ielg) = zero
                     kvol(isubmat,ielg) = kvol(isubmat,ielg) + ratio*vfrac !100% inside
                    ! if added volume ratio makes that sum is > 1, then substract from previous filling
                    if(icumu == -1)then
                      sumvf = sum(kvol(1:nbsubmat,ielg))
                      if (sumvf > one)then
                        if(idc == 1)then
                          ! substract from existing submat (default one)
                          ! pre-condition : sum (vf)= 1.0
                          isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                          vf_to_substract = sumvf-one
                          kvol(isubmat_to_substract,ielg) = &
                            kvol(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                        elseif(idc > 1)then
                         ! substract from previous step
                         isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                         vf_to_substract = sumvf-one
                         vf_to_substract = min(vf_to_substract, kvol(isubmat_to_substract,ielg))
                         kvol(isubmat_to_substract,ielg) = kvol(isubmat_to_substract,ielg)-vf_to_substract
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
                  sum_tag = itag_n(node_id(1))+itag_n(node_id(2))+itag_n(node_id(3))
                  if(sum_tag == 3 .or. sum_tag == 0)then
                    !sum_tag == 3 : elem is inside  (may be considered as outside if is_reversed is true)
                    !sum_tag == 0 : elem is outside (may be considered as inside if is_reversed is true)
                    ! reversed option (Iopt)
                    if(is_reversed .and. prod_tag > 0)then
                       cycle
                    elseif(.not.is_reversed .and. sum_tag == 0)then
                       cycle
                    end if
                    ratio = one
                    if(icumu == 0)kvol(isubmat,ielg) = zero
                    kvol(isubmat,ielg) = kvol(isubmat,ielg) + ratio*vfrac !100% inside
                    ! if added volume ratio makes that sum is > 1, then substract from previous filling
                    if(icumu == -1)then
                      sumvf = sum(kvol(1:nbsubmat,ielg))
                      if (sumvf > one)then
                        if(idc == 1)then
                          ! substract from existing submat (default one)
                          ! pre-condition : sum (vf)= 1.0
                          isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                          vf_to_substract = sumvf-one
                          kvol(isubmat_to_substract,ielg) = &
                            kvol(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                        elseif(idc > 1)then
                         ! substract from previous step
                         isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                         vf_to_substract = sumvf-one
                         vf_to_substract = min(vf_to_substract, kvol(isubmat_to_substract,ielg))
                         kvol(isubmat_to_substract,ielg) = kvol(isubmat_to_substract,ielg)-vf_to_substract
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
              if(icumu == 0)kvol(isubmat,ielg) = zero
              do ipoly=1,result_list_polygon%num_polygons
                call polygon_SetClockWise( result_list_polygon%polygon(ipoly) )
                ratio = result_list_polygon%polygon(ipoly)%area / elem_polygon%area !partially inside
                if(is_reversed)ratio = one - ratio
                kvol(isubmat,ielg) = kvol(isubmat,ielg) + vfrac * ratio
                ! if added volume ratio makes that sum is > 1, then substract from previous filling
                if(icumu == -1)then
                  sumvf = sum(kvol(1:nbsubmat,ielg))
                  if (sumvf > one)then
                    if(idc == 1)then
                      ! substract from existing submat (default one)
                      ! pre-condition : sum (vf)= 1.0
                      isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                      vf_to_substract = sumvf-one
                      kvol(isubmat_to_substract,ielg) = &
                        kvol(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                    elseif(idc > 1)then
                     ! substract from previous step
                     isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                     vf_to_substract = sumvf-one
                     vf_to_substract = min(vf_to_substract, kvol(isubmat_to_substract,ielg))
                     kvol(isubmat_to_substract,ielg) = kvol(isubmat_to_substract,ielg)-vf_to_substract
                    end if
                  end if
                end if
                if(debug)print *, "isubmat,quad partially inside",isubmat,ixq(7,ielg),kvol(isubmat,ielg)
              enddo
            else
               ! degenerated case. Check if elem is fully outside/inside by testing centroid.
              point%y=fourth*sum(elem_polygon%point(1:4)%y)
              point%z=fourth*sum(elem_polygon%point(1:4)%z)
              is_inside = polygon_is_point_inside (user_polygon, point)
              if(is_inside)then
                !centroid inside user polygon
                !quad is fully inside but clipping algorithm had at least one node on user polygon vertice
              else
                !centroid outside user polygon
                !quad is fully outside but clipping algorithm had at least one node on user polygon vertice
              end if
              if(is_inside.and.is_reversed) then
                ratio=one
              elseif(.not.is_inside .and. is_reversed)then
                ratio=one
              else
                cycle !nothing to do
              end if
              if(icumu == 0)kvol(isubmat,ielg) = zero
              kvol(isubmat,ielg) = kvol(isubmat,ielg) + ratio*vfrac !0% inside
              ! if added volume ratio makes that sum is > 1, then substract from previous filling
              if(icumu == -1)then
                sumvf = sum(kvol(1:nbsubmat,ielg))
                if (sumvf > one)then
                  if(idc == 1)then
                    ! substract from existing submat (default one)
                    ! pre-condition : sum (vf)= 1.0
                    isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                    vf_to_substract = sumvf-one
                    kvol(isubmat_to_substract,ielg) = &
                      kvol(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                  elseif(idc > 1)then
                   ! substract from previous step
                   isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                   vf_to_substract = sumvf-one
                   vf_to_substract = min(vf_to_substract, kvol(isubmat_to_substract,ielg))
                   kvol(isubmat_to_substract,ielg) = kvol(isubmat_to_substract,ielg)-vf_to_substract
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
              if(icumu == 0)kvol(isubmat,ielg) = zero
              do ipoly=1,result_list_polygon%num_polygons
                call polygon_SetClockWise( result_list_polygon%polygon(ipoly) )
                ratio = result_list_polygon%polygon(ipoly)%area / elem_polygon%area !partially inside
                if(is_reversed)ratio = one - ratio
                kvol(isubmat,ielg) = kvol(isubmat,ielg) + vfrac * ratio
                if(debug)print *, "isubmat,quad partially inside",isubmat,ixq(7,ielg),kvol(isubmat,ielg)
              enddo
              ! if added volume ratio makes that sum is > 1, then substract from previous filling
              if(icumu == -1)then
                sumvf = sum(kvol(1:nbsubmat,ielg))
                if (sumvf > one)then
                  if(idc == 1)then
                    ! substract from existing submat (default one)
                    ! pre-condition : sum (vf)= 1.0
                    isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                    vf_to_substract = sumvf-one
                    kvol(isubmat_to_substract,ielg) = &
                      kvol(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                  elseif(idc > 1)then
                   ! substract from previous step
                   isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                   vf_to_substract = sumvf-one
                   vf_to_substract = min(vf_to_substract, kvol(isubmat_to_substract,ielg))
                   kvol(isubmat_to_substract,ielg) = kvol(isubmat_to_substract,ielg)-vf_to_substract
                  end if
                end if
              end if
            else
               ! degenerated case. Check if elem is fully outside/inside by testing centroid.
              point%y=third*sum(elem_polygon%point(1:4)%y)
              point%z=third*sum(elem_polygon%point(1:4)%z)
              is_inside = polygon_is_point_inside (user_polygon, point)
              if(is_inside)then
                !centroid inside user polygon
                !quad is fully inside but clipping algorithm had at least one node on user polygon vertice
              else
                !centroid outside user polygon
                !quad is fully outside but clipping algorithm had at least one node on user polygon vertice
              end if
              if(is_inside.and.is_reversed) then
                ratio=one
              elseif(.not.is_inside .and. is_reversed)then
                ratio=one
              else
                cycle !nothing to do
              end if
              if(icumu == 0)kvol(isubmat,ielg) = zero
              kvol(isubmat,ielg) = kvol(isubmat,ielg) + ratio*vfrac !0% inside
              ! if added volume ratio makes that sum is > 1, then substract from previous filling
              if(icumu == -1)then
                sumvf = sum(kvol(1:nbsubmat,ielg))
                if (sumvf > one)then
                  if(idc == 1)then
                    ! substract from existing submat (default one)
                    ! pre-condition : sum (vf)= 1.0
                    isubmat_to_substract = max(1,maxloc(vfrac0(1:nbsubmat),1))
                    vf_to_substract = sumvf-one
                    kvol(isubmat_to_substract,ielg) = &
                      kvol(isubmat_to_substract,ielg) - vf_to_substract * vfrac0(isubmat_to_substract)
                  elseif(idc > 1)then
                   ! substract from previous step
                   isubmat_to_substract = inivol(i_inivol)%container(idc-1)%submat_id
                   vf_to_substract = sumvf-one
                   vf_to_substract = min(vf_to_substract, kvol(isubmat_to_substract,ielg))
                   kvol(isubmat_to_substract,ielg) = kvol(isubmat_to_substract,ielg)-vf_to_substract
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



