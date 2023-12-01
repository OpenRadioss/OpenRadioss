Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      MODULE INTER7_CANDIDATE_PAIRS_MOD
      CONTAINS

!! \brief get the list of candidates for all segment

        SUBROUTINE INTER7_CANDIDATE_PAIRS(
     &                                    nsn          ,
     &                                    oldnum       ,
     &                                    nsnr         ,
     &                                    isznsnr      ,
     &                                    i_mem        ,
     &                                    irect        ,
     &                                    x            ,
     &                                    stf          ,
     &                                    stfn         ,
     &                                    xyzm         ,
     &                                    nsv          ,
     &                                    ii_stok      ,
     &                                    cand_n       ,
     &                                    eshift       ,
     &                                    cand_e       ,
     &                                    mulnsn       ,
     &                                    tzinf        ,
     &                                    gap_s_l      ,
     &                                    gap_m_l      ,
     &                                    voxel        ,
     &                                    nbx          ,
     &                                    nby          ,
     &                                    nbz          ,
     &                                    inacti       ,
     &                                    ifq          ,
     &                                    cand_a       ,
     &                                    cand_p       ,
     &                                    ifpen        ,
     &                                    nrtm         ,
     &                                    nsnrold      ,
     &                                    igap         ,
     &                                    gap          ,
     &                                    gap_s        ,
     &                                    gap_m        ,
     &                                    gapmin       ,
     &                                    gapmax       ,
     &                                    marge        ,
     &                                    curv_max     ,
     &                                    itask        ,
     &                                    bgapsmx      ,
     &                                    s_kremnod    ,
     &                                    kremnod      ,
     &                                    s_remnod     ,
     &                                    remnod       ,
     &                                    flagremnode  ,
     &                                    drad         ,
     &                                    itied        ,
     &                                    cand_f       ,
     &                                    dgapload     ,
     &                                    s_cand_a     ,
     &                                    total_nb_nrtm,
     &                                    numnod       ,
     &                                    xrem         ,
     &                                    s_xrem       ,
     &                                    irem         ,
     &                                    s_irem       ,
     &                                    next_nod      )
          USE COLLISION_MOD , ONLY : GROUP_SIZE
          USE INTER7_FILTER_CAND_MOD
          USE CONSTANT_MOD
C-----------------------------------------------
          implicit none
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          integer, intent(inout) :: i_mem !< error code when not enough memory
          integer, intent(in), value :: eshift !< openmp shift for main segments
          integer, intent(in), value :: nsn !< number of secondary nodes
          integer, intent(in), value :: nsnr !< current number of remote secondary nodes
          integer, intent(in), value :: nsnrold !< old number of remote secondary nodes
          integer, intent(in), value :: isznsnr !< size of oldnum
          integer, intent(in), value :: nrtm !< number of considered segment
          integer, intent(in), value :: total_nb_nrtm !< total number of segments
          integer, intent(in), value :: itask !< id of the current task
          integer, intent(in), value :: nbx !< number of voxels in x
          integer, intent(in), value :: nby !< number of voxels in y
          integer, intent(in), value :: nbz !< number of voxels in z
          integer, intent(in), value :: inacti !< inactivation of initial penetrations
          integer, intent(in), value :: ifq !< friction model ?
          integer, intent(in), value :: igap !< gap model ?
          integer, intent(in), value :: flagremnode !< flag for removed nodes?
          integer, intent(in), value :: itied !< tied contact ?
          integer, intent(in), value :: numnod !< total number of nodes of the model
          integer, intent(in), value :: s_xrem !< size of xrem
          integer, intent(in), value :: s_irem !< size of xrem
          integer, intent(in), value :: s_cand_a !< size of cand_a
          integer, intent(in), value :: s_kremnod !< 2 * nrtm + 1 if option is used
          integer, intent(in), value :: s_remnod !< size of remnod
          integer, intent(in), value :: mulnsn !< maximum numbrer of candidates (size of cand_n)
          integer, intent(inout) :: ii_stok !< number of candidates found

          integer, intent(in) :: nsv(nsn) !< global secondary node numbers
          integer, intent(in) :: oldnum(isznsnr) !< renumbering ?
          integer, intent(in) :: kremnod(s_kremnod) !< list of removed nodes
          integer, intent(in) :: remnod(s_remnod) !< list of removed nodes
          integer, intent(in) :: irect(4,nrtm) !< node id (from 1 to NUMNOD) for each segment (1:nrtm)

          integer, intent(inout) :: cand_n(mulnsn) !< list of candidates (secondary)
          integer, intent(inout) :: cand_e(mulnsn) !< list of candidates (main)
          integer, intent(inout) :: ifpen(mulnsn) !< something related to friction (???)
          integer, intent(inout) :: cand_a(s_cand_a) !< (???)
          integer, intent(inout) :: irem(s_irem,nsnr) !< remote (spmd) integer data
          integer, intent(inout) :: voxel(nbx+2,nby+2,nbz+2) !< contain the first node of each voxel
          integer, intent(inout) :: next_nod(nsn+nsnr) !< next node in the same voxel

          my_real, intent(in), value :: gap !< gap (???)
          my_real, intent(in), value :: gapmin !< minimum gap
          my_real, intent(in), value :: gapmax !< maximum gap
          my_real, intent(in), value :: bgapsmx!< overestimation of gap_s
          my_real, intent(in), value :: marge !< margin
          my_real, intent(in), value :: tzinf !< some kind of length for "zone of influence" ?
          my_real, intent(in), value :: drad !< radiation distance (thermal analysis)
          my_real, intent(in), value :: dgapload !< gap load (???)
          my_real, intent(in) :: x(3,numnod) !< coordinates of nodes all
          my_real, intent(in) :: gap_s(nsn) !< gap for secondary nodes
          my_real, intent(in) :: gap_m(nrtm) !< gap for main nodes
          my_real, intent(in) :: gap_s_l(nsn) !< gap for secondary nodes (???)
          my_real, intent(in) :: gap_m_l(nrtm) !< gap for main nodes (???)
          my_real, intent(in) :: curv_max(nrtm) !< maximum curvature
          my_real, intent(in) :: xyzm(12) !< bounding box

          my_real, intent(inout) :: cand_p(mulnsn) !< penetration (???)
          my_real, intent(inout) :: cand_f(mulnsn) !< related to tied contact, cand force (???)
          my_real, intent(in) :: stf(nrtm) !< stiffness of segments (quadrangles or triangles)
          my_real, intent(inout) :: stfn(nsn) !< stiffness secondary nodes
          my_real, intent(inout) :: xrem(s_xrem,nsnr) !< remote (spmd) real data
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          integer i,j,
     .            nn,ne,k,l,j_stok,jj,
     .            delnod,m
          integer, dimension(:), allocatable :: tagremnode
          my_real
     .       xs,ys,zs,sx,sy,sz,s2,
     .       xmin, xmax,ymin, ymax,zmin, zmax,
     .       xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     .       d1x,d1y,d1z,d2x,d2y,d2z,dd1,dd2,d2,a2
          integer, dimension(:), allocatable::  last_nod
          integer  ix,iy,iz,m1,m2,m3,m4,
     .             ix1,iy1,iz1,ix2,iy2,iz2
          integer :: iix,iiy,iiz
          my_real
     .       xminb,yminb,zminb,xmaxb,ymaxb,zmaxb,
     .       xmine,ymine,zmine,xmaxe,ymaxe,zmaxe,aaa
          integer first,last
          integer :: nb_voxel_on !< number of remote nodes in the bounding box
          integer, dimension(:), allocatable :: list_nb_voxel_on
          integer, dimension(GROUP_SIZE) :: prov_n, prov_e !< temporary list of candidates


C-----------------------------------------------

          if(itask == 0)then
            allocate(list_nb_voxel_on((nbx+2)*(nby+2)*(nbz+2)))
            nb_voxel_on = 0
          end if

!$OMP BARRIER

! The global bounding box contains all the nodes
! Some nodes may by higly distant from the impact zone
! The domain is subdivided in cells (voxel)
! All the cells have the sime size, except the first and the last one in each direction

! bounding box of the model
          xmin = xyzm(1)
          ymin = xyzm(2)
          zmin = xyzm(3)
          xmax = xyzm(4)
          ymax = xyzm(5)
          zmax = xyzm(6)

! reduced bounding box of the model
! The reduced bounding box corresponds to voxel(2:nbx+1,2:nby+1,2:nbz+1), it contains cells of the same size

          xminb = xyzm(7)
          yminb = xyzm(8)
          zminb = xyzm(9)
          xmaxb = xyzm(10)
          ymaxb = xyzm(11)
          zmaxb = xyzm(12)

C=======================================================================
C 1   Add local nodes to the cells
C=======================================================================
          if(itask==0.and.total_nb_nrtm>0)then
            allocate(last_nod(nsn+nsnr))
            do i=1,nsn
              iix=0
              iiy=0
              iiz=0
              if(stfn(i) == zero)cycle
              j=nsv(i)
              if(x(1,j) < xmin)  cycle
              if(x(1,j) > xmax)  cycle
              if(x(2,j) < ymin)  cycle
              if(x(2,j) > ymax)  cycle
              if(x(3,j) < zmin)  cycle
              if(x(3,j) > zmax)  cycle
              iix=int(nbx*(x(1,j)-xminb)/(xmaxb-xminb))
              iiy=int(nby*(x(2,j)-yminb)/(ymaxb-yminb))
              iiz=int(nbz*(x(3,j)-zminb)/(zmaxb-zminb))
              iix=max(1,2+min(nbx,iix))
              iiy=max(1,2+min(nby,iiy))
              iiz=max(1,2+min(nbz,iiz))
              first = voxel(iix,iiy,iiz)
              if(first == 0)then
                !empty cell
                nb_voxel_on = nb_voxel_on + 1
                ! 1d version of 3d potition, indexes starts at 1
                list_nb_voxel_on(nb_voxel_on) = (iix-1)*(nby+2)*(nbz+2)+(iiy-1)*(nbz+2)+(iiz-1)
                voxel(iix,iiy,iiz) = i ! first
                next_nod(i)                 = 0 ! last one
                last_nod(i)                 = 0 ! no last
              elseif(last_nod(first) == 0)then
                !cell containing one node
                !add as next node
                next_nod(first) = i ! next
                last_nod(first) = i ! last
                next_nod(i)     = 0 ! last one
              else
                !
                !jump to the last node of the cell
                last = last_nod(first) ! last node in this voxel
                next_nod(last)  = i ! next
                last_nod(first) = i ! last
                next_nod(i)     = 0 ! last one
              endif
            enddo

C=======================================================================
C 2   Add remote (spmd) nodes to the cells
C=======================================================================
            do j = 1, nsnr

              if(xrem(1,j) < xmin)  cycle
              if(xrem(1,j) > xmax)  cycle
              if(xrem(2,j) < ymin)  cycle
              if(xrem(2,j) > ymax)  cycle
              if(xrem(3,j) < zmin)  cycle
              if(xrem(3,j) > zmax)  cycle

              iix=int(nbx*(xrem(1,j)-xminb)/(xmaxb-xminb))
              iiy=int(nby*(xrem(2,j)-yminb)/(ymaxb-yminb))
              iiz=int(nbz*(xrem(3,j)-zminb)/(zmaxb-zminb))
              iix=max(1,2+min(nbx,iix))
              iiy=max(1,2+min(nby,iiy))
              iiz=max(1,2+min(nbz,iiz))

              first = voxel(iix,iiy,iiz)

              if(first == 0)then
                ! empty cell
                nb_voxel_on = nb_voxel_on + 1
                ! 1d version of 3d potition
                list_nb_voxel_on( nb_voxel_on ) = (iix-1) * (nby+2) * (nbz+2) + (iiy-1) * (nbz+2) + (iiz-1)

                voxel(iix,iiy,iiz) = nsn+j ! first
                next_nod(nsn+j)     = 0 ! last one
                last_nod(nsn+j)     = 0 ! no last
              elseif(last_nod(first) == 0)then
                ! cell containing one node, add it as next node
                next_nod(first) = nsn+j  ! next
                last_nod(first) = nsn+j  ! last
                next_nod(nsn+j)  = 0     ! last one
              else
                ! , jump to the last node of the cell
                last = last_nod(first)  ! last node in this voxel
                next_nod(last)  = nsn+j ! next
                last_nod(first) = nsn+j ! last
                next_nod(nsn+j)     = 0 ! last one
              endif
            enddo
            deallocate(last_nod)
          end if !itask == 0
!$OMP BARRIER


C=======================================================================
C 3   FACE RECOVERY AND ENUMERATION OF CANDIDATE COUPLES
C=======================================================================
          j_stok = 0
          if(flagremnode == 2) then
            allocate(tagremnode(numnod))
            do i=1,numnod
              tagremnode(i) = 0
            enddo
          endif
!$OMP BARRIER
!$OMP DO SCHEDULE(DYNAMIC)
          do ne=1,nrtm
            if(stf(ne) == zero)cycle ! the segment is deleted/eroded
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                ! the segment ne cannot be in contact with the node remnod(i)
                ! typically, remnod(i) contains nodes of neighboring elements
                tagremnode(remnod(i)) = 1
              enddo
            endif
            if(igap == 0)then
              aaa = tzinf+curv_max(ne)
            else
              aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,bgapsmx+gap_m(ne)))+dgapload,drad)
            endif

            m1 = irect(1,ne)
            m2 = irect(2,ne)
            m3 = irect(3,ne)
            m4 = irect(4,ne)

            xx1=x(1,m1)
            xx2=x(1,m2)
            xx3=x(1,m3)
            xx4=x(1,m4)
            xmaxe=max(xx1,xx2,xx3,xx4)
            xmine=min(xx1,xx2,xx3,xx4)

            yy1=x(2,m1)
            yy2=x(2,m2)
            yy3=x(2,m3)
            yy4=x(2,m4)
            ymaxe=max(yy1,yy2,yy3,yy4)
            ymine=min(yy1,yy2,yy3,yy4)

            zz1=x(3,m1)
            zz2=x(3,m2)
            zz3=x(3,m3)
            zz4=x(3,m4)
            zmaxe=max(zz1,zz2,zz3,zz4)
            zmine=min(zz1,zz2,zz3,zz4)

            ! surface (to trim candidate list)
            sx = (yy3-yy1)*(zz4-zz2) - (zz3-zz1)*(yy4-yy2)
            sy = (zz3-zz1)*(xx4-xx2) - (xx3-xx1)*(zz4-zz2)
            sz = (xx3-xx1)*(yy4-yy2) - (yy3-yy1)*(xx4-xx2)
            s2 = sx*sx + sy*sy + sz*sz

            !find voxels containing the bounding box of the segment
            if(nbx>1) then
              ix1=int(nbx*(xmine-aaa-xminb)/(xmaxb-xminb))
              ix2=int(nbx*(xmaxe+aaa-xminb)/(xmaxb-xminb))
            else
              ix1=-2
              ix2=1
            endif

            if(nby>1) then
              iy1=int(nby*(ymine-aaa-yminb)/(ymaxb-yminb))
              iy2=int(nby*(ymaxe+aaa-yminb)/(ymaxb-yminb))
            else
              iy1=-2
              iy2=1
            endif

            if(nbz>1) then
              iz1=int(nbz*(zmine-aaa-zminb)/(zmaxb-zminb))
              iz2=int(nbz*(zmaxe+aaa-zminb)/(zmaxb-zminb))
            else
              iz1=-2
              iz2=1
            endif

            ix1=max(1,2+min(nbx,ix1))
            iy1=max(1,2+min(nby,iy1))
            iz1=max(1,2+min(nbz,iz1))

            ix2=max(1,2+min(nbx,ix2))
            iy2=max(1,2+min(nby,iy2))
            iz2=max(1,2+min(nbz,iz2))

            do iz = iz1,iz2
              do iy = iy1,iy2
                do ix = ix1,ix2
                  if(i_mem==2) cycle
                  jj = voxel(ix,iy,iz)
                  do while(jj /= 0)

                    if(jj<=nsn)then
                      ! local node
                      nn=nsv(jj)

                      if(nn == m1)goto 200
                      if(nn == m2)goto 200
                      if(nn == m3)goto 200
                      if(nn == m4)goto 200

                      if(flagremnode == 2) then
                        if( tagremnode(nsv(jj)) == 1) goto 200
                      endif
                      xs = x(1,nn)
                      ys = x(2,nn)
                      zs = x(3,nn)
                      if(igap /= 0)then
                        aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,gap_s(jj)+gap_m(ne)))+dgapload,drad)
                      endif
                    else
                      ! remote (SPMD) node: data are stored in irem/xrem (communicated earlier)
                      j=jj-nsn
                      delnod = 0
                      if(flagremnode == 2) then
                        k = kremnod(2*(ne-1)+2) + 1
                        l = kremnod(2*(ne-1)+3)
                        do m=k,l
                          if(remnod(m) == -irem(2,j) ) then
                            delnod = delnod + 1
                            exit
                          endif
                        enddo
                        if(delnod /= 0)goto 200
                      endif

                      xs = xrem(1,j)
                      ys = xrem(2,j)
                      zs = xrem(3,j)
                      if(igap /= 0)then
                        aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,xrem(9,j)+gap_m(ne)))+dgapload,drad)
                      endif
                    endif

                    if(xs<=xmine-aaa)goto 200
                    if(xs>=xmaxe+aaa)goto 200
                    if(ys<=ymine-aaa)goto 200
                    if(ys>=ymaxe+aaa)goto 200
                    if(zs<=zmine-aaa)goto 200
                    if(zs>=zmaxe+aaa)goto 200

                    ! underestimation of the distance**2 to eliminate candidates

                    d1x = xs - xx1
                    d1y = ys - yy1
                    d1z = zs - zz1
                    d2x = xs - xx2
                    d2y = ys - yy2
                    d2z = zs - zz2
                    dd1 = d1x*sx+d1y*sy+d1z*sz
                    dd2 = d2x*sx+d2y*sy+d2z*sz
                    if(dd1*dd2 > zero)then
                      d2 = min(dd1*dd1,dd2*dd2)
                      a2 = aaa*aaa*s2
                      if(d2 > a2)goto 200
                    endif

                    j_stok = j_stok + 1
                    prov_n(j_stok) = jj
                    prov_e(j_stok) = ne

                    if(j_stok == GROUP_SIZE) then 
                       ! filter prov_n, prov_e and append to cand_n, cand_e
                       if(i_mem == 0) call inter7_filter_cand(
     1                   j_stok,irect  ,x     ,nsv   ,ii_stok,
     2                   cand_n,cand_e ,mulnsn,marge  ,
     3                   i_mem ,prov_n ,prov_e,eshift,inacti ,
     4                   ifq   ,cand_a ,cand_p,ifpen ,nsn    ,
     5                   oldnum,nsnrold,igap  ,gap   ,gap_s  ,
     6                   gap_m ,gapmin ,gapmax,curv_max,
     7                   gap_s_l,gap_m_l,drad,itied    ,
     8                   cand_f ,dgapload,
     .                   nsnr,
     .                   xrem ,s_xrem)
                         j_stok = 0
                    endif
 
  200               continue
                    jj = next_nod(jj)
c                   if(i_mem==2) jj = 0
                  enddo ! while(jj /= 0)
                enddo ! x
              enddo  ! y
            enddo   ! z
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                tagremnode(remnod(i)) = 0
              enddo
            endif
          enddo
!$OMP END DO
          if(j_stok > 0 .and. i_mem == 0) call inter7_filter_cand(
     1                   j_stok,irect  ,x     ,nsv   ,ii_stok,
     2                   cand_n,cand_e ,mulnsn,marge  ,
     3                   i_mem ,prov_n ,prov_e,eshift,inacti ,
     4                   ifq   ,cand_a ,cand_p,ifpen ,nsn    ,
     5                   oldnum,nsnrold,igap  ,gap   ,gap_s  ,
     6                   gap_m ,gapmin ,gapmax,curv_max,
     7                   gap_s_l,gap_m_l,drad,itied    ,
     8                   cand_f ,dgapload,
     .                   nsnr,
     .                   xrem ,s_xrem)

C=======================================================================
C 5   VOXEL RESET
C=======================================================================
!$OMP BARRIER
          if(total_nb_nrtm>0 .and. itask == 0) then
            do jj = 1, nb_voxel_on
              j = list_nb_voxel_on(jj)
              k = j
              iiz =  mod(k,nbz+2) + 1
              ! iz [1,nbz+2]
              k = (k-iiz+1)/(nbz+2)
              iiy =  mod(k,nby+2) + 1
              k = (k-iiy+1)/(nby+2)
              iix = mod(k,nbx+2) + 1
              voxel(iix,iiy,iiz) = 0
            enddo
          endif
!$OMP BARRIER
C=======================================================================
C 7   DEALLOCATE
C=======================================================================
          if(flagremnode == 2) then
            if(allocated(tagremnode)) deallocate(tagremnode)
          endif
          if(itask == 0) deallocate(list_nb_voxel_on)

C#ifndef NO_SERIALIZE
C        if(nsn > 13602 .and. nrtm > 1800) then
C        call INTER7_SERIALIZE(      "t10m.dat", 
C     &                                    nsn          ,
C     &                                    oldnum       ,
C     &                                    nsnr         ,
C     &                                    isznsnr      ,
C     &                                    irect        ,
C     &                                    x            ,
C     &                                    stf          ,
C     &                                    stfn         ,
C     &                                    xyzm         ,
C     &                                    nsv          ,
C     &                                    ii_stok      ,
C     &                                    cand_n       ,
C     &                                    cand_e       ,
C     &                                    mulnsn       ,
C     &                                    tzinf        ,
C     &                                    gap_s_l      ,
C     &                                    gap_m_l      ,
C     &                                    nbx          ,
C     &                                    nby          ,
C     &                                    nbz          ,
C     &                                    inacti       ,
C     &                                    ifq          ,
C     &                                    cand_a       ,
C     &                                    nrtm         ,
C     &                                    nsnrold      ,
C     &                                    igap         ,
C     &                                    gap          ,
C     &                                    gap_s        ,
C     &                                    gap_m        ,
C     &                                    gapmin       ,
C     &                                    gapmax       ,
C     &                                    marge        ,
C     &                                    curv_max     ,
C     &                                    bgapsmx      ,
C     &                                    s_kremnod    ,
C     &                                    kremnod      ,
C     &                                    s_remnod     ,
C     &                                    remnod       ,
C     &                                    flagremnode  ,
C     &                                    drad         ,
C     &                                    itied        ,
C     &                                    dgapload     ,
C     &                                    s_cand_a     ,
C     &                                    total_nb_nrtm,
C     &                                    numnod       )
C        stop              
C        endif
C#endif

          return
        end

!! \brief write the data to a file
        SUBROUTINE INTER7_SERIALIZE(      filename     , 
     &                                    nsn          ,
     &                                    oldnum       ,
     &                                    nsnr         ,
     &                                    isznsnr      ,
     &                                    irect        ,
     &                                    x            ,
     &                                    stf          ,
     &                                    stfn         ,
     &                                    xyzm         ,
     &                                    nsv          ,
     &                                    ii_stok      ,
     &                                    cand_n       ,
     &                                    cand_e       ,
     &                                    mulnsn       ,
     &                                    tzinf        ,
     &                                    gap_s_l      ,
     &                                    gap_m_l      ,
     &                                    nbx          ,
     &                                    nby          ,
     &                                    nbz          ,
     &                                    inacti       ,
     &                                    ifq          ,
     &                                    cand_a       ,
     &                                    nrtm         ,
     &                                    nsnrold      ,
     &                                    igap         ,
     &                                    gap          ,
     &                                    gap_s        ,
     &                                    gap_m        ,
     &                                    gapmin       ,
     &                                    gapmax       ,
     &                                    marge        ,
     &                                    curv_max     ,
     &                                    bgapsmx      ,
     &                                    s_kremnod    ,
     &                                    kremnod      ,
     &                                    s_remnod     ,
     &                                    remnod       ,
     &                                    flagremnode  ,
     &                                    drad         ,
     &                                    itied        ,
     &                                    dgapload     ,
     &                                    s_cand_a     ,
     &                                    total_nb_nrtm,
     &                                    numnod       )
C-----------------------------------------------
          implicit none
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          character(len =*), intent(in) :: filename

          integer, intent(in), value :: nsn !< number of secondary nodes
          integer, intent(in), value :: nsnr !< current number of remote secondary nodes
          integer, intent(in), value :: nsnrold !< old number of remote secondary nodes
          integer, intent(in), value :: isznsnr !< size of oldnum
          integer, intent(in), value :: nrtm !< number of considered segment
          integer, intent(in), value :: total_nb_nrtm !< total number of segments
          integer, intent(in), value :: inacti !< inactivation of initial penetrations
          integer, intent(in), value :: ifq !< friction model ?
          integer, intent(in), value :: igap !< gap model ?
          integer, intent(in), value :: flagremnode !< flag for removed nodes?
          integer, intent(in), value :: itied !< tied contact ?
          integer, intent(in), value :: numnod !< total number of nodes of the model
          integer, intent(in), value :: s_cand_a !< size of cand_a
          integer, intent(in), value :: s_kremnod ! 2 * nrtm + 1 if option is used
          integer, intent(in), value :: s_remnod !< size of remnod
          integer, intent(in), value :: mulnsn !< maximum numbrer of candidates (size of cand_n)
          integer, intent(in), value :: nbx !< number of voxels in x
          integer, intent(in), value :: nby !< number of voxels in y
          integer, intent(in), value :: nbz !< number of voxels in z

          integer, intent(in) :: ii_stok !< number of candidates found

          integer, intent(in) :: nsv(nsn) !< global secondary node numbers
          integer, intent(in) :: oldnum(isznsnr) !< renumbering ?
          integer, intent(in) :: kremnod(s_kremnod) !< list of removed nodes
          integer, intent(in) :: remnod(s_remnod) !< list of removed nodes
          integer, intent(in) :: irect(4,nrtm) !< node id (from 1 to NUMNOD) for each segment (1:nrtm)
          integer, intent(in) :: cand_a(s_cand_a) !< (???)

          integer, intent(in) :: cand_n(mulnsn) !< list of candidates (secondary)
          integer, intent(in) :: cand_e(mulnsn) !< list of candidates (main)

          my_real, intent(in), value :: gap !< gap (???)
          my_real, intent(in), value :: gapmin !< minimum gap
          my_real, intent(in), value :: gapmax !< maximum gap
          my_real, intent(in), value :: bgapsmx!< overestimation of gap_s
          my_real, intent(in), value :: marge !< margin
          my_real, intent(in), value :: tzinf !< some kind of length for "zone of influence" ?
          my_real, intent(in), value :: drad !< radiation distance (thermal analysis)
          my_real, intent(in), value :: dgapload !< gap load (???)
          my_real, intent(in) :: x(3,numnod) !< coordinates of nodes all
          my_real, intent(in) :: gap_s(nsn) !< gap for secondary nodes
          my_real, intent(in) :: gap_m(nrtm) !< gap for main nodes
          my_real, intent(in) :: gap_s_l(nsn) !< gap for secondary nodes (???)
          my_real, intent(in) :: gap_m_l(nrtm) !< gap for main nodes (???)
          my_real, intent(in) :: curv_max(nrtm) !< maximum curvature
          my_real, intent(in) :: xyzm(12) !< bounding box
          my_real, intent(in) :: stf(nrtm) !< stiffness of segments (quadrangles or triangles)
          my_real, intent(in) :: stfn(nsn) !< stiffness secondary nodes
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          integer :: i, j, iostat, unitNum
          integer :: pos

C-----------------------------------------------
          open(NEWUNIT=unitNum, FILE=filename, FORM='UNFORMATTED', STATUS='REPLACE', IOSTAT=iostat)
          if (iostat /= 0) then
            write(6,*) "error opening file: ", filename
            return
          end if

          ! Relevant input
          write(unitNum) nsn !< number of secondary nodes
          write(unitNum) nsnr !< current number of remote secondary nodes
          write(unitNum) nsnrold !< old number of remote secondary nodes
          write(unitNum) isznsnr !< size of oldnum
          write(unitNum) nrtm !< number of considered segment
          write(unitNum) total_nb_nrtm !< total number of segments
          write(unitNum) inacti !< inactivation of initial penetrations
          write(unitNum) ifq !< friction model ?
          write(unitNum) igap !< gap model ?
          write(unitNum) flagremnode !< flag for removed nodes?
          write(unitNum) itied !< tied contact ?
          write(unitNum) numnod !< total number of nodes of the model
          write(unitNum) s_cand_a !< size of cand_a
          write(unitNum) s_kremnod !< 2 * nrtm + 1 if option is used
          write(unitNum) s_remnod !< size of remnod
          write(unitNum) mulnsn !< maximum numbrer of candidates (size of cand_n)
          write(unitNum) nbx !< number of voxels in x
          write(unitNum) nby !< number of voxels in y
          write(unitNum) nbz !< number of voxels in z


          write(unitNum) nsv(1:nsn) !< global secondary node numbers
          write(unitNum) oldnum(1:isznsnr) !< renumbering ?
          write(unitNum) kremnod(1:s_kremnod) !< list of removed nodes
          write(unitNum) remnod(1:s_remnod) !< list of removed nodes

          do i = 1, nrtm
            write(unitNum) (irect(j, i), j = 1, 4)
          enddo
          write(unitNum) cand_a(1:s_cand_a) !< (???)
          write(unitNum) gap !< gap (???)
          write(unitNum) gapmin !< minimum gap
          write(unitNum) gapmax !< maximum gap
          write(unitNum) bgapsmx!< overestimation of gap_s
          write(unitNum) marge !< margin
          write(unitNum) tzinf !< some kind of length for "zone of influence" ?
          write(unitNum) drad !< radiation distance (thermal analysis)
          write(unitNum) dgapload !< gap load (???)
          do i = 1, numnod
            write(unitnum) (x(j, i), j = 1, 3)
          end do
          write(unitNum) gap_s(1:nsn) !< gap for secondary nodes
          write(unitNum) gap_m(1:nrtm) !< gap for main nodes
          write(unitNum) gap_s_l(1:nsn) !< gap for secondary nodes (???)
          write(unitNum) gap_m_l(1:nrtm) !< gap for main nodes (???)

          write(unitNum) curv_max(1:nrtm) !< maximum curvature
          write(unitNum) xyzm(1:12) !< bounding box
          write(unitNum) stf(1:nrtm) !< stiffness of segments (quadrangles or triangles)
          write(unitNum) stfn(1:nsn) !< stiffness secondary nodes

          !relevant output
          !mumnsn is the maximum number of candidates, ii_stok is the number of candidates found
          write(unitNum) ii_stok !< number of candidates found
          write(unitNum) cand_n(1:ii_stok) !< list of candidates (secondary)
          write(unitNum) cand_e(1:ii_stok) !< list of candidates (main)
          write(6,*)  "nsn          ",nsn                             
          write(6,*)  "nsnr         ",nsnr                                            
          write(6,*)  "nsnrold      ",nsnrold                                        
          write(6,*)  "isznsnr      ",isznsnr                  
          write(6,*)  "nrtm         ",nrtm                                
          write(6,*)  "total_nb_nrtm",total_nb_nrtm                            
          write(6,*)  "inacti       ",inacti                                        
          write(6,*)  "ifq          ",ifq                    
          write(6,*)  "igap         ",igap               
          write(6,*)  "flagremnode  ",flagremnode                           
          write(6,*)  "itied        ",itied                  
          write(6,*)  "numnod       ",numnod                                      
          write(6,*)  "s_cand_a     ",s_cand_a                  
          write(6,*)  "s_kremnod    ",s_kremnod                                  
          write(6,*)  "s_remnod     ",s_remnod                  
          write(6,*)  "mulnsn       ",mulnsn                                                  
          write(6,*)  "nbx          ",nbx                         
          write(6,*)  "nby          ",nby                         
          write(6,*)  "nbz          ",nbz                         
          write(6,*)  "ii_stok      ",ii_stok                         

          call flush(unitNum)
          close(unitNum)
        end subroutine
!! \brief write the data to a file
        SUBROUTINE INTER7_DESERIALIZE(    filename     , 
     &                                    nsn          ,
     &                                    oldnum       ,
     &                                    nsnr         ,
     &                                    isznsnr      ,
     &                                    irect        ,
     &                                    x            ,
     &                                    stf          ,
     &                                    stfn         ,
     &                                    xyzm         ,
     &                                    nsv          ,
     &                                    ii_stok      ,
     &                                    cand_n       ,
     &                                    cand_e       ,
     &                                    mulnsn       ,
     &                                    tzinf        ,
     &                                    gap_s_l      ,
     &                                    gap_m_l      ,
     &                                    nbx          ,
     &                                    nby          ,
     &                                    nbz          ,
     &                                    inacti       ,
     &                                    ifq          ,
     &                                    cand_a       ,
     &                                    nrtm         ,
     &                                    nsnrold      ,
     &                                    igap         ,
     &                                    gap          ,
     &                                    gap_s        ,
     &                                    gap_m        ,
     &                                    gapmin       ,
     &                                    gapmax       ,
     &                                    marge        ,
     &                                    curv_max     ,
     &                                    bgapsmx      ,
     &                                    s_kremnod    ,
     &                                    kremnod      ,
     &                                    s_remnod     ,
     &                                    remnod       ,
     &                                    flagremnode  ,
     &                                    drad         ,
     &                                    itied        ,
     &                                    dgapload     ,
     &                                    s_cand_a     ,
     &                                    total_nb_nrtm,
     &                                    numnod       )
C-----------------------------------------------
          implicit none
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          character(len =*), intent(in) :: filename

          integer, intent(out) :: nsn !< number of secondary nodes
          integer, intent(out) :: nsnr !< current number of remote secondary nodes
          integer, intent(out) :: nsnrold !< old number of remote secondary nodes
          integer, intent(out) :: isznsnr !< size of oldnum
          integer, intent(out) :: nrtm !< number of considered segment
          integer, intent(out) :: total_nb_nrtm !< total number of segments
          integer, intent(out) :: inacti !< inactivation of initial penetrations
          integer, intent(out) :: ifq !< friction model ?
          integer, intent(out) :: igap !< gap model ?
          integer, intent(out) :: flagremnode !< flag for removed nodes?
          integer, intent(out) :: itied !< tied contact ?
          integer, intent(out) :: numnod !< total number of nodes of the model
          integer, intent(out) :: s_cand_a !< size of cand_a
          integer, intent(out) :: s_kremnod ! 2 * nrtm + 1 if option is used
          integer, intent(out) :: s_remnod !< size of remnod
          integer, intent(out) :: mulnsn !< maximum numbrer of candidates (size of cand_n)
          integer, intent(out) :: nbx !< number of voxels in x
          integer, intent(out) :: nby !< number of voxels in y
          integer, intent(out) :: nbz !< number of voxels in z

          integer, intent(out) :: ii_stok !< number of candidates found

          integer, intent(out), dimension(:), allocatable :: nsv !< global secondary node numbers
          integer, intent(out), dimension(:), allocatable :: oldnum !< renumbering ?
          integer, intent(out), dimension(:), allocatable :: kremnod !< list of removed nodes
          integer, intent(out), dimension(:), allocatable :: remnod !< list of removed nodes
          integer, intent(out), dimension(:,:), allocatable :: irect !< node id (from 1 to NUMNOD) for each segment (1:nrtm)
          integer, intent(out), dimension(:), allocatable :: cand_a !< (???)

          integer, intent(out), dimension(:), allocatable :: cand_n !< list of candidates (secondary)
          integer, intent(out), dimension(:), allocatable :: cand_e !< list of candidates (main)

          my_real, intent(out) :: gap !< gap (???)
          my_real, intent(out) :: gapmin !< minimum gap
          my_real, intent(out) :: gapmax !< maximum gap
          my_real, intent(out) :: bgapsmx!< overestimation of gap_s
          my_real, intent(out) :: marge !< margin
          my_real, intent(out) :: tzinf !< some kind of length for "zone of influence" ?
          my_real, intent(out) :: drad !< radiation distance (thermal analysis)
          my_real, intent(out) :: dgapload !< gap load (???)

          my_real, intent(out), dimension(:,:), allocatable :: x !< coordinates of nodes all
          my_real, intent(out), dimension(:), allocatable :: gap_s !< gap for secondary nodes
          my_real, intent(out), dimension(:), allocatable :: gap_m !< gap for main nodes
          my_real, intent(out), dimension(:), allocatable :: gap_s_l !< gap for secondary nodes (???)
          my_real, intent(out), dimension(:), allocatable :: gap_m_l !< gap for main nodes (???)
          my_real, intent(out), dimension(:), allocatable :: curv_max !< maximum curvature
          my_real, intent(out), dimension(:), allocatable :: stf !< stiffness of segments (quadrangles or triangles)
          my_real, intent(out), dimension(:), allocatable :: stfn !< stiffness secondary nodes
          my_real, intent(out) :: xyzm(12) !< bounding box

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          integer :: i, j, iostat, unitNum
          integer :: pos
C-----------------------------------------------
          open(NEWUNIT=unitNum, FILE=filename, FORM='UNFORMATTED', STATUS='OLD', IOSTAT=iostat)
          if (iostat /= 0) then
            write(6,*) "error opening file: ", filename
            return
          end if

          ! Relevant input
          read(unitNum) nsn !< number of secondary nodes
          read(unitNum) nsnr !< current number of remote secondary nodes
          read(unitNum) nsnrold !< old number of remote secondary nodes
          read(unitNum) isznsnr !< size of oldnum
          read(unitNum) nrtm !< number of considered segment
          read(unitNum) total_nb_nrtm !< total number of segments
          read(unitNum) inacti !< inactivation of initial penetrations
          read(unitNum) ifq !< friction model ?
          read(unitNum) igap !< gap model ?
          read(unitNum) flagremnode !< flag for removed nodes?
          read(unitNum) itied !< tied contact ?
          read(unitNum) numnod !< total number of nodes of the model
          read(unitNum) s_cand_a !< size of cand_a
          read(unitNum) s_kremnod !< 2 * nrtm + 1 if option is used
          read(unitNum) s_remnod !< size of remnod
          read(unitNum) mulnsn !< maximum numbrer of candidates (size of cand_n)
          read(unitNum) nbx !< number of voxels in x
          read(unitNum) nby !< number of voxels in y
          read(unitNum) nbz !< number of voxels in z

          write(6,*)  "nsn          ",nsn                             
          write(6,*)  "nsnr         ",nsnr                                            
          write(6,*)  "nsnrold      ",nsnrold                                        
          write(6,*)  "isznsnr      ",isznsnr                  
          write(6,*)  "nrtm         ",nrtm                                
          write(6,*)  "total_nb_nrtm",total_nb_nrtm                            
          write(6,*)  "inacti       ",inacti                                        
          write(6,*)  "ifq          ",ifq                    
          write(6,*)  "igap         ",igap               
          write(6,*)  "flagremnode  ",flagremnode                           
          write(6,*)  "itied        ",itied                  
          write(6,*)  "numnod       ",numnod                                      
          write(6,*)  "s_cand_a     ",s_cand_a                  
          write(6,*)  "s_kremnod    ",s_kremnod                                  
          write(6,*)  "s_remnod     ",s_remnod                  
          write(6,*)  "mulnsn       ",mulnsn                                                  
          write(6,*)  "nbx          ",nbx                         
          write(6,*)  "nby          ",nby                         
          write(6,*)  "nbz          ",nbz                         


          allocate(nsv(nsn))
          allocate(oldnum(isznsnr))
          allocate(kremnod(s_kremnod))
          allocate(remnod(s_remnod))
          allocate(irect(4, nrtm))
          allocate(cand_a(s_cand_a))
          allocate(x(3, numnod))
          allocate(gap_s(nsn))
          allocate(gap_m(nrtm))
          allocate(gap_s_l(nsn))
          allocate(gap_m_l(nrtm))
          allocate(curv_max(nrtm))
          allocate(stf(nrtm))
          allocate(stfn(nsn))

          read(unitNum) nsv(1:nsn) !< global secondary node numbers
          read(unitNum) oldnum(1:isznsnr) !< renumbering ?
          read(unitNum) kremnod(1:s_kremnod) !< list of removed nodes
          read(unitNum) remnod(1:s_remnod) !< list of removed nodes
          do i = 1, nrtm
            read(unitNum) (irect(j, i), j = 1, 4)
          enddo
          read(unitNum) cand_a(1:s_cand_a) !< (???)

          read(unitNum) gap !< gap (???)
          read(unitNum) gapmin !< minimum gap
          read(unitNum) gapmax !< maximum gap
          read(unitNum) bgapsmx!< overestimation of gap_s
          read(unitNum) marge !< margin
          read(unitNum) tzinf !< some kind of length for "zone of influence" ?
          read(unitNum) drad !< radiation distance (thermal analysis)
          read(unitNum) dgapload !< gap load (???)

          do i = 1, numnod
            read(unitnum) (x(j, i), j = 1, 3)
          end do
          read(unitNum) gap_s(1:nsn) !< gap for secondary nodes
          read(unitNum) gap_m(1:nrtm) !< gap for main nodes
          read(unitNum) gap_s_l(1:nsn) !< gap for secondary nodes (???)
          read(unitNum) gap_m_l(1:nrtm) !< gap for main nodes (???)

          read(unitNum) curv_max(1:nrtm) !< maximum curvature
          read(unitNum) xyzm(1:12) !< bounding box
          read(unitNum) stf(1:nrtm) !< stiffness of segments (quadrangles or triangles)
          read(unitNum) stfn(1:nsn) !< stiffness secondary nodes

          !relevant output
          !mumnsn is the maximum number of candidates, ii_stok is the number of candidates found
          read(unitNum) ii_stok !< number of candidates found
          allocate(cand_n(mulnsn))
          allocate(cand_e(mulnsn))
          cand_n = 0
          cand_e = 0
          read(unitNum) cand_n(1:ii_stok) !< list of candidates (secondary)
          read(unitNum) cand_e(1:ii_stok) !< list of candidates (main)
          close(unitNum)
          write(6,*)  "ii_stok_ref   ",ii_stok                         
        end subroutine


        !!\brief compare the couple cand_n(i) cand_e(i) with the couple cand_n_ref/cand_e_ref
        !!\details check if there exist i and j such as cand_n(i)=cand_n_ref(j) and cand_e(i)=cand_e_ref(j)
        subroutine test_candidates(filename)
          use iso_c_binding , only : c_int
          implicit none
          interface
          subroutine compare_cand(cand_n, cand_e, ii_stok, cand_n_ref, cand_e_ref, ii_stok_ref) bind(C, name="compare_cand")
              import :: c_int
              integer(c_int), intent(in) :: cand_n(*), cand_e(*), cand_n_ref(*), cand_e_ref(*)
              integer(c_int), intent(in), value :: ii_stok_ref, ii_stok
          end subroutine compare_cand
          end interface
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          character(len =*), intent(in) :: filename
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          integer :: nsn !< number of secondary nodes
          integer :: nsnr !< current number of remote secondary nodes
          integer :: nsnrold !< old number of remote secondary nodes
          integer :: isznsnr !< size of oldnum
          integer :: nrtm !< number of considered segment
          integer :: total_nb_nrtm !< total number of segments
          integer :: inacti !< inactivation of initial penetrations
          integer :: ifq !< friction model ?
          integer :: igap !< gap model ?
          integer :: flagremnode !< flag for removed nodes?
          integer :: itied !< tied contact ?
          integer :: numnod !< total number of nodes of the model
          integer :: s_cand_a !< size of cand_a
          integer :: s_kremnod ! 2 * nrtm + 1 if option is used
          integer :: s_remnod !< size of remnod
          integer :: mulnsn !< maximum numbrer of candidates (size of cand_n)
          integer :: nbx !< number of voxels in x
          integer :: nby !< number of voxels in y
          integer :: nbz !< number of voxels in z
          integer :: ii_stok, ii_stok_ref !< number of candidates found
          integer, dimension(:), allocatable :: nsv !< global secondary node numbers
          integer, dimension(:), allocatable :: oldnum !< renumbering ?
          integer, dimension(:), allocatable :: kremnod !< list of removed nodes
          integer, dimension(:), allocatable :: remnod !< list of removed nodes
          integer, dimension(:,:), allocatable :: irect !< node id (from 1 to NUMNOD) for each segment (1:nrtm)
          integer, dimension(:), allocatable :: cand_a !< (???)
          integer, dimension(:), allocatable :: cand_n, cand_n_ref !< list of candidates (secondary)
          integer, dimension(:), allocatable :: cand_e, cand_e_ref !< list of candidates (main)
          my_real :: gap !< gap (???)
          my_real :: gapmin !< minimum gap
          my_real :: gapmax !< maximum gap
          my_real :: bgapsmx!< overestimation of gap_s
          my_real :: marge !< margin
          my_real :: tzinf !< some kind of length for "zone of influence" ?
          my_real :: drad !< radiation distance (thermal analysis)
          my_real :: dgapload !< gap load (???)
          my_real, dimension(:,:), allocatable :: x !< coordinates of nodes all
          my_real, dimension(:), allocatable :: gap_s !< gap for secondary nodes
          my_real, dimension(:), allocatable :: gap_m !< gap for main nodes
          my_real, dimension(:), allocatable :: gap_s_l !< gap for secondary nodes (???)
          my_real, dimension(:), allocatable :: gap_m_l !< gap for main nodes (???)
          my_real, dimension(:), allocatable :: curv_max !< maximum curvature
          my_real, dimension(:), allocatable :: stf !< stiffness of segments (quadrangles or triangles)
          my_real, dimension(:), allocatable :: stfn !< stiffness secondary nodes
          my_real, dimension(:), allocatable :: cand_f,cand_p

          integer OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
          external OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS

          my_real ::  xyzm(12) !< bounding box
          integer :: voxel(8000000)
          integer, dimension(:), allocatable :: next_nod,ifpen
          integer :: eshift,i_mem,i, itask, s_irem, s_xrem 
          my_real, dimension(:,:), allocatable :: xrem
          integer, dimension(:,:), allocatable :: irem
          double precision :: start_time, end_time, elapsed_time
          double precision OMP_GET_WTIME
          external OMP_GET_WTIME
          i_mem = 0
          eshift = 0
          ii_stok_ref = 0


          call INTER7_DESERIALIZE(        filename     , 
     &                                    nsn          ,
     &                                    oldnum       ,
     &                                    nsnr         ,
     &                                    isznsnr      ,
     &                                    irect        ,
     &                                    x            ,
     &                                    stf          ,
     &                                    stfn         ,
     &                                    xyzm         ,
     &                                    nsv          ,
     &                                    ii_stok_ref  ,
     &                                    cand_n_ref   ,
     &                                    cand_e_ref   ,
     &                                    mulnsn       ,
     &                                    tzinf        ,
     &                                    gap_s_l      ,
     &                                    gap_m_l      ,
     &                                    nbx          ,
     &                                    nby          ,
     &                                    nbz          ,
     &                                    inacti       ,
     &                                    ifq          ,
     &                                    cand_a       ,
     &                                    nrtm         ,
     &                                    nsnrold      ,
     &                                    igap         ,
     &                                    gap          ,
     &                                    gap_s        ,
     &                                    gap_m        ,
     &                                    gapmin       ,
     &                                    gapmax       ,
     &                                    marge        ,
     &                                    curv_max     ,
     &                                    bgapsmx      ,
     &                                    s_kremnod    ,
     &                                    kremnod      ,
     &                                    s_remnod     ,
     &                                    remnod       ,
     &                                    flagremnode  ,
     &                                    drad         ,
     &                                    itied        ,
     &                                    dgapload     ,
     &                                    s_cand_a     ,
     &                                    total_nb_nrtm,
     &                                    numnod       )

        allocate(cand_n(mulnsn))
        allocate(cand_e(mulnsn))
        allocate(cand_f(mulnsn))
        allocate(cand_p(mulnsn))
        allocate(ifpen(mulnsn))
        s_xrem = 1 
        s_irem = 1
        allocate(xrem(s_xrem, nsnr))
        allocate(irem(s_irem, nsnr))
        ifpen = 0
        cand_n = 0
        cand_e = 0
        cand_f = 0
        cand_p = 0
        ii_stok = 0
        allocate(next_nod(nsn+nsnr))
        start_time = OMP_GET_WTIME()
!$OMP PARALLEL PRIVATE(i,itask)
!$OMP SINGLE
        do i=1,(nbx+2)*(nby+2)*(nbz+2)
          voxel(i)=0
        enddo
!$OMP END SINGLE
         ITASK = OMP_GET_THREAD_NUM() 


        call INTER7_CANDIDATE_PAIRS(
     &                                    nsn          ,
     &                                    oldnum       ,
     &                                    nsnr         ,
     &                                    isznsnr      ,
     &                                    i_mem        ,
     &                                    irect        ,
     &                                    x            ,
     &                                    stf          ,
     &                                    stfn         ,
     &                                    xyzm         ,
     &                                    nsv          ,
     &                                    ii_stok      ,
     &                                    cand_n       ,
     &                                    eshift       ,
     &                                    cand_e       ,
     &                                    mulnsn       ,
     &                                    tzinf        ,
     &                                    gap_s_l      ,
     &                                    gap_m_l      ,
     &                                    voxel        ,
     &                                    nbx          ,
     &                                    nby          ,
     &                                    nbz          ,
     &                                    inacti       ,
     &                                    ifq          ,
     &                                    cand_a       ,
     &                                    cand_p       ,
     &                                    ifpen        ,
     &                                    nrtm         ,
     &                                    nsnrold      ,
     &                                    igap         ,
     &                                    gap          ,
     &                                    gap_s        ,
     &                                    gap_m        ,
     &                                    gapmin       ,
     &                                    gapmax       ,
     &                                    marge        ,
     &                                    curv_max     ,
     &                                    itask        ,
     &                                    bgapsmx      ,
     &                                    s_kremnod    ,
     &                                    kremnod      ,
     &                                    s_remnod     ,
     &                                    remnod       ,
     &                                    flagremnode  ,
     &                                    drad         ,
     &                                    itied        ,
     &                                    cand_f       ,
     &                                    dgapload     ,
     &                                    s_cand_a     ,
     &                                    total_nb_nrtm,
     &                                    numnod       ,
     &                                    xrem         ,
     &                                    s_xrem       ,
     &                                    irem         ,
     &                                    s_irem       ,
     &                                    next_nod      )

!$OMP END PARALLEL
          end_time = OMP_GET_WTIME()

          write(6,*) "Elapsed time =", end_time - start_time 

          call compare_cand(cand_n, cand_e, ii_stok, cand_n_ref, cand_e_ref, ii_stok_ref)

        end subroutine


      END MODULE



