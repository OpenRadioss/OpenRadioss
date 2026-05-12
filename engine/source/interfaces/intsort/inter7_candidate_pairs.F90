!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    inter7_candidate_pairs_mod   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
!||--- called by ------------------------------------------------------
!||    inter7_collision_detection   ../engine/source/interfaces/intsort/inter7_collision_detection.F90
!||    main                         ../engine/unit_test/unit_test1.F
!||====================================================================/
      MODULE INTER7_CANDIDATE_PAIRS_MOD
      implicit none
      CONTAINS

!! \brief Search for candidate pairs using ONLY LOCAL nodes (voxel contains local nodes only)
!! \details Called before MPI_WAITANY to overlap computation with communication.
!!          Uses the local voxel (indices 1..nsn). No remote node processing.
        SUBROUTINE INTER7_CANDIDATE_PAIRS_LOCAL(multimp_param ,&
        &                                    nsn          ,&
        &                                    i_mem        ,&
        &                                    irect        ,&
        &                                    x            ,&
        &                                    stf          ,&
        &                                    xyzm         ,&
        &                                    nsv          ,&
        &                                    ii_stok      ,&
        &                                    cand_n       ,&
        &                                    eshift       ,&
        &                                    cand_e       ,&
        &                                    mulnsn       ,&
        &                                    tzinf        ,&
        &                                    gap_s_l      ,&
        &                                    gap_m_l      ,&
        &                                    voxel        ,&
        &                                    nbx          ,&
        &                                    nby          ,&
        &                                    nbz          ,&
        &                                    inacti       ,&
        &                                    ifq          ,&
        &                                    cand_a       ,&
        &                                    cand_p       ,&
        &                                    ifpen        ,&
        &                                    nrtm         ,&
        &                                    igap         ,&
        &                                    gap          ,&
        &                                    gap_s        ,&
        &                                    gap_m        ,&
        &                                    gapmin       ,&
        &                                    gapmax       ,&
        &                                    marge        ,&
        &                                    curv_max     ,&
        &                                    bgapsmx      ,&
        &                                    s_kremnod    ,&
        &                                    kremnod      ,&
        &                                    s_remnod     ,&
        &                                    remnod       ,&
        &                                    flagremnode  ,&
        &                                    drad         ,&
        &                                    itied        ,&
        &                                    cand_f       ,&
        &                                    dgapload     ,&
        &                                    s_cand_a     ,&
        &                                    numnod       ,&
        &                                    next_nod     ,&
        &                                    inv_nsv       )
          USE PRECISION_MOD, ONLY : WP
          USE COLLISION_MOD , ONLY : GROUP_SIZE
          USE INTER7_FILTER_CAND_MOD
          USE CONSTANT_MOD
          USE EXTEND_ARRAY_MOD, ONLY : extend_array
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(inout) :: multimp_param 
          integer, intent(inout) :: i_mem
          integer, intent(in), value :: eshift
          integer, intent(in), value :: nsn
          integer, intent(in), value :: nrtm
          integer, intent(in), value :: nbx
          integer, intent(in), value :: nby
          integer, intent(in), value :: nbz
          integer, intent(in), value :: inacti
          integer, intent(in), value :: ifq
          integer, intent(in), value :: igap
          integer, intent(in), value :: flagremnode
          integer, intent(in), value :: itied
          integer, intent(in), value :: numnod
          integer, intent(in), value :: s_cand_a
          integer, intent(in), value :: s_kremnod
          integer, intent(in), value :: s_remnod
          integer, intent(inout) :: mulnsn
          integer, intent(inout) :: ii_stok

          integer, intent(in) :: nsv(nsn)
          integer, intent(in) :: kremnod(s_kremnod)
          integer, intent(in) :: remnod(s_remnod)
          integer, intent(in) :: irect(4,nrtm)

          integer, intent(inout), allocatable :: cand_n(:)
          integer, intent(inout), allocatable :: cand_e(:)
          integer, intent(inout), allocatable :: ifpen(:)
          integer, intent(inout) :: cand_a(s_cand_a)
          integer, intent(inout) :: voxel((nbx+2)*(nby+2)*(nbz+2))
          integer, intent(inout) :: next_nod(nsn)
          integer, intent(in) :: inv_nsv(numnod)

          real(kind=WP), intent(in), value :: gap
          real(kind=WP), intent(in), value :: gapmin
          real(kind=WP), intent(in), value :: gapmax
          real(kind=WP), intent(in), value :: bgapsmx
          real(kind=WP), intent(in), value :: marge
          real(kind=WP), intent(in), value :: tzinf
          real(kind=WP), intent(in), value :: drad
          real(kind=WP), intent(in), value :: dgapload
          real(kind=WP), intent(in) :: x(3,numnod)
          real(kind=WP), intent(in) :: gap_s(nsn)
          real(kind=WP), intent(in) :: gap_m(nrtm)
          real(kind=WP), intent(in) :: gap_s_l(nsn)
          real(kind=WP), intent(in) :: gap_m_l(nrtm)
          real(kind=WP), intent(in) :: curv_max(nrtm)
          real(kind=WP), intent(in) :: xyzm(12)

          real(kind=WP), intent(inout), allocatable :: cand_p(:)
          real(kind=WP), intent(inout), allocatable :: cand_f(:)
          real(kind=WP), intent(in) :: stf(nrtm)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j, nn, ne, k, l, j_stok, jj, m
          integer, dimension(:), allocatable :: tagremnode
          real(kind=WP) :: xs, ys, zs, sx, sy, sz, s2
          real(kind=WP) :: xmin, xmax, ymin, ymax, zmin, zmax
          real(kind=WP) :: xx1, xx2, xx3, xx4, yy1, yy2, yy3, yy4, zz1, zz2, zz3, zz4
          real(kind=WP) :: d1x, d1y, d1z, d2x, d2y, d2z, dd1, dd2, d2, a2
          real(kind=WP) :: aaa

          integer :: ix, iy, iz, m1, m2, m3, m4, ix1, iy1, iz1, ix2, iy2, iz2
          integer :: jj_m1, jj_m2, jj_m3, jj_m4

          real(kind=WP) :: xminb, yminb, zminb, xmaxb, ymaxb, zmaxb, xmine, ymine, zmine, xmaxe, ymaxe, zmaxe

          integer, dimension(GROUP_SIZE) :: prov_n, prov_e
          integer :: cellid
          integer :: old_mulnsn !< old mulnsn value before reallocation
          ! Dummy variables for inter7_filter_cand compatibility (no remote nodes)
          integer :: nsnr_dummy, nsnrold_dummy, isznsnr_dummy
          integer, dimension(1) :: oldnum_dummy
          real(kind=WP), dimension(1,1) :: xrem_dummy

          nsnr_dummy = 0
          nsnrold_dummy = 0
          isznsnr_dummy = 1
          !write(*,*) "Number of local nodes to process: ", nsn, ii_stok



          xmin = xyzm(1)
          ymin = xyzm(2)
          zmin = xyzm(3)
          xmax = xyzm(4)
          ymax = xyzm(5)
          zmax = xyzm(6)
          xminb = xyzm(7)
          yminb = xyzm(8)
          zminb = xyzm(9)
          xmaxb = xyzm(10)
          ymaxb = xyzm(11)
          zmaxb = xyzm(12)

          j_stok = 0
          if(flagremnode == 2) then
            allocate(tagremnode(numnod))
            do i=1,numnod
              tagremnode(i) = 0
            end do
          end if
          do ne=1,nrtm
            if(stf(ne) == zero)cycle
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                tagremnode(remnod(i)) = 1
              end do
            end if
            if(igap == 0)then
              aaa = tzinf+curv_max(ne)
            else
              aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,bgapsmx+gap_m(ne)))+dgapload,drad)
            end if

            m1 = irect(1,ne)
            m2 = irect(2,ne)
            m3 = irect(3,ne)
            m4 = irect(4,ne)

            jj_m1 = inv_nsv(m1)
            jj_m2 = inv_nsv(m2)
            jj_m3 = inv_nsv(m3)
            jj_m4 = inv_nsv(m4)

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

            sx = (yy3-yy1)*(zz4-zz2) - (zz3-zz1)*(yy4-yy2)
            sy = (zz3-zz1)*(xx4-xx2) - (xx3-xx1)*(zz4-zz2)
            sz = (xx3-xx1)*(yy4-yy2) - (yy3-yy1)*(xx4-xx2)
            s2 = sx*sx + sy*sy + sz*sz

            if(nbx>1) then
              ix1=int(nbx*(xmine-aaa-xminb)/(xmaxb-xminb))
              ix2=int(nbx*(xmaxe+aaa-xminb)/(xmaxb-xminb))
            else
              ix1=-2
              ix2=1
            end if

            if(nby>1) then
              iy1=int(nby*(ymine-aaa-yminb)/(ymaxb-yminb))
              iy2=int(nby*(ymaxe+aaa-yminb)/(ymaxb-yminb))
            else
              iy1=-2
              iy2=1
            end if

            if(nbz>1) then
              iz1=int(nbz*(zmine-aaa-zminb)/(zmaxb-zminb))
              iz2=int(nbz*(zmaxe+aaa-zminb)/(zmaxb-zminb))
            else
              iz1=-2
              iz2=1
            end if

            ix1=max(1,2+min(nbx,ix1))
            iy1=max(1,2+min(nby,iy1))
            iz1=max(1,2+min(nbz,iz1))

            ix2=max(1,2+min(nbx,ix2))
            iy2=max(1,2+min(nby,iy2))
            iz2=max(1,2+min(nbz,iz2))

            do iz = iz1,iz2
              do iy = iy1,iy2
                do ix = ix1,ix2
                  cellid = (iz-1)*(nbx+2)*(nby+2)+(iy-1)*(nbx+2)+ix
                  jj = voxel(cellid)
                  do while(jj > 0)
                    ! Self-node rejection
                    if(jj == jj_m1) goto 300
                    if(jj == jj_m2) goto 300
                    if(jj == jj_m3) goto 300
                    if(jj == jj_m4) goto 300

                    ! Local node only
                    nn=nsv(jj)

                    if(flagremnode == 2) then
                      if( tagremnode(nn) == 1) goto 300
                    end if
                    xs = x(1,nn)
                    ys = x(2,nn)
                    zs = x(3,nn)
                    if(igap /= 0)then
                      aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,gap_s(jj)+gap_m(ne)))+dgapload,drad)
                    end if

                    if(xs<=xmine-aaa)goto 300
                    if(xs>=xmaxe+aaa)goto 300
                    if(ys<=ymine-aaa)goto 300
                    if(ys>=ymaxe+aaa)goto 300
                    if(zs<=zmine-aaa)goto 300
                    if(zs>=zmaxe+aaa)goto 300

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
                      if(d2 > a2)goto 300
                    end if

                    j_stok = j_stok + 1
                    prov_n(j_stok) = jj
                    prov_e(j_stok) = ne

                    if(j_stok == GROUP_SIZE) then
                      if(j_stok + ii_stok > mulnsn) then
                        old_mulnsn = mulnsn
                        multimp_param = multimp_param +4
                        mulnsn = mulnsn * multimp_param
                        ! Reallocate candidate arrays to the new size
                        call extend_array(cand_n, old_mulnsn, mulnsn)
                        call extend_array(cand_e, old_mulnsn, mulnsn)
                        if (ifq /= 0) call extend_array(ifpen, old_mulnsn, mulnsn)
                        if (inacti == 5 .or. inacti == 6 .or. inacti == 7) call extend_array(cand_p, old_mulnsn, mulnsn)
                        if (itied /= 0) call extend_array(cand_f, 8*old_mulnsn, 8*mulnsn)
                      end if
                      call inter7_filter_cand(&
                      &                   j_stok,irect  ,x     ,nsv   ,ii_stok,&
                      &                   cand_n,cand_e ,mulnsn,marge  ,&
                      &                   prov_n ,prov_e,eshift,inacti ,&
                      &                   ifq   ,cand_a ,cand_p,ifpen ,nsn    ,&
                      &                   oldnum_dummy,nsnrold_dummy,igap  ,gap   ,gap_s  ,&
                      &                   gap_m ,gapmin ,gapmax,curv_max,&
                      &                   gap_s_l,gap_m_l,drad,itied    ,&
                      &                   cand_f ,dgapload, numnod,&
                      &                   nsnr_dummy, nrtm, isznsnr_dummy,&
                      &                   xrem_dummy ,1)
                      j_stok = 0
                    end if

300                 continue
                    jj = next_nod(jj)
                  end do
                end do
              end do
            end do
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                tagremnode(remnod(i)) = 0
              end do
            end if
          end do
          if(j_stok > 0) then 
            if(j_stok + ii_stok > mulnsn) then
              old_mulnsn = mulnsn
              multimp_param = multimp_param +4
              mulnsn = mulnsn * multimp_param
              ! Reallocate candidate arrays to the new size
              call extend_array(cand_n, old_mulnsn, mulnsn)
              call extend_array(cand_e, old_mulnsn, mulnsn)
              if (ifq /= 0) call extend_array(ifpen, old_mulnsn, mulnsn)
              if (inacti == 5 .or. inacti == 6 .or. inacti == 7) call extend_array(cand_p, old_mulnsn, mulnsn)
              if (itied /= 0) call extend_array(cand_f, 8*old_mulnsn, 8*mulnsn)
            end if
            call inter7_filter_cand(&
          &                   j_stok,irect  ,x     ,nsv   ,ii_stok,&
          &                   cand_n,cand_e ,mulnsn,marge  ,&
          &                   prov_n ,prov_e,eshift,inacti ,&
          &                   ifq   ,cand_a ,cand_p,ifpen ,nsn    ,&
          &                   oldnum_dummy,nsnrold_dummy,igap  ,gap   ,gap_s  ,&
          &                   gap_m ,gapmin ,gapmax,curv_max,&
          &                   gap_s_l,gap_m_l,drad,itied    ,&
          &                   cand_f ,dgapload, numnod,&
          &                   nsnr_dummy, nrtm, isznsnr_dummy,&
          &                   xrem_dummy ,1)
        endif

          if(flagremnode == 2) then
            if(allocated(tagremnode)) deallocate(tagremnode)
          end if
          if(i_mem == 2) then
            !write(*,*) "Memory mode 2: Not enough memory (local)",mulnsn
          endif
          !write(*,*) "Number of candidate pairs found in local search: ", ii_stok
          return
        end subroutine INTER7_CANDIDATE_PAIRS_LOCAL

!! \brief Search for candidate pairs using ONLY REMOTE nodes (voxel_remote contains remote nodes only)
!! \details Called after MPI receives are complete. Walks voxel_remote (1-based indexing).
!!          Maps jj -> nsn+jj for downstream cand_n compatibility.
        SUBROUTINE INTER7_CANDIDATE_PAIRS_REMOTE(multimp_param, &
        &                                    nsn          ,&
        &                                    oldnum       ,&
        &                                    nsnr         ,&
        &                                    isznsnr      ,&
        &                                    i_mem        ,&
        &                                    irect        ,&
        &                                    x            ,&
        &                                    stf          ,&
        &                                    xyzm         ,&
        &                                    nsv          ,&
        &                                    ii_stok      ,&
        &                                    cand_n       ,&
        &                                    eshift       ,&
        &                                    cand_e       ,&
        &                                    mulnsn       ,&
        &                                    tzinf        ,&
        &                                    gap_s_l      ,&
        &                                    gap_m_l      ,&
        &                                    voxel_remote ,&
        &                                    nbx          ,&
        &                                    nby          ,&
        &                                    nbz          ,&
        &                                    inacti       ,&
        &                                    ifq          ,&
        &                                    cand_a       ,&
        &                                    cand_p       ,&
        &                                    ifpen        ,&
        &                                    nrtm         ,&
        &                                    nsnrold      ,&
        &                                    igap         ,&
        &                                    gap          ,&
        &                                    gap_s        ,&
        &                                    gap_m        ,&
        &                                    gapmin       ,&
        &                                    gapmax       ,&
        &                                    marge        ,&
        &                                    curv_max     ,&
        &                                    bgapsmx      ,&
        &                                    s_kremnod    ,&
        &                                    kremnod      ,&
        &                                    s_remnod     ,&
        &                                    remnod       ,&
        &                                    flagremnode  ,&
        &                                    drad         ,&
        &                                    itied        ,&
        &                                    cand_f       ,&
        &                                    dgapload     ,&
        &                                    s_cand_a     ,&
        &                                    numnod       ,&
        &                                    xrem         ,&
        &                                    s_xrem       ,&
        &                                    irem         ,&
        &                                    s_irem       ,&
        &                                    next_nod_remote )
          USE PRECISION_MOD, ONLY : WP
          USE COLLISION_MOD , ONLY : GROUP_SIZE
          USE INTER7_FILTER_CAND_MOD
          USE EXTEND_ARRAY_MOD, ONLY : extend_array
          USE CONSTANT_MOD
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(inout) :: multimp_param
          integer, intent(inout) :: i_mem
          integer, intent(in), value :: eshift
          integer, intent(in), value :: nsn
          integer, intent(in), value :: nsnr
          integer, intent(in), value :: nsnrold
          integer, intent(in), value :: isznsnr
          integer, intent(in), value :: nrtm
          integer, intent(in), value :: nbx
          integer, intent(in), value :: nby
          integer, intent(in), value :: nbz
          integer, intent(in), value :: inacti
          integer, intent(in), value :: ifq
          integer, intent(in), value :: igap
          integer, intent(in), value :: flagremnode
          integer, intent(in), value :: itied
          integer, intent(in), value :: numnod
          integer, intent(in), value :: s_xrem
          integer, intent(in), value :: s_irem
          integer, intent(in), value :: s_cand_a
          integer, intent(in), value :: s_kremnod
          integer, intent(in), value :: s_remnod
          integer, intent(inout) :: mulnsn
          integer, intent(inout) :: ii_stok

          integer, intent(in) :: nsv(nsn)
          integer, intent(in) :: oldnum(isznsnr)
          integer, intent(in) :: kremnod(s_kremnod)
          integer, intent(in) :: remnod(s_remnod)
          integer, intent(in) :: irect(4,nrtm)

          integer, intent(inout), allocatable :: cand_n(:)
          integer, intent(inout), allocatable :: cand_e(:)
          integer, intent(inout), allocatable :: ifpen(:)
          integer, intent(inout) :: cand_a(s_cand_a)
          integer, intent(inout) :: voxel_remote((nbx+2)*(nby+2)*(nbz+2))
          integer, intent(inout) :: next_nod_remote(nsnr)
          integer, intent(inout) :: irem(s_irem,nsnr)

          real(kind=WP), intent(in), value :: gap
          real(kind=WP), intent(in), value :: gapmin
          real(kind=WP), intent(in), value :: gapmax
          real(kind=WP), intent(in), value :: bgapsmx
          real(kind=WP), intent(in), value :: marge
          real(kind=WP), intent(in), value :: tzinf
          real(kind=WP), intent(in), value :: drad
          real(kind=WP), intent(in), value :: dgapload
          real(kind=WP), intent(in) :: x(3,numnod)
          real(kind=WP), intent(in) :: gap_s(nsn)
          real(kind=WP), intent(in) :: gap_m(nrtm)
          real(kind=WP), intent(in) :: gap_s_l(nsn)
          real(kind=WP), intent(in) :: gap_m_l(nrtm)
          real(kind=WP), intent(in) :: curv_max(nrtm)
          real(kind=WP), intent(in) :: xyzm(12)

          real(kind=WP), intent(inout), allocatable :: cand_p(:)
          real(kind=WP), intent(inout), allocatable :: cand_f(:)
          real(kind=WP), intent(in) :: stf(nrtm)
          real(kind=WP), intent(inout) :: xrem(s_xrem,nsnr)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j, ne, k, l, j_stok, jj, delnod, m
          integer, dimension(:), allocatable :: tagremnode
          real(kind=WP) :: xs, ys, zs, sx, sy, sz, s2
          real(kind=WP) :: xmin, xmax, ymin, ymax, zmin, zmax
          real(kind=WP) :: xx1, xx2, xx3, xx4, yy1, yy2, yy3, yy4, zz1, zz2, zz3, zz4
          real(kind=WP) :: d1x, d1y, d1z, d2x, d2y, d2z, dd1, dd2, d2, a2
          real(kind=WP) :: aaa

          integer :: ix, iy, iz, m1, m2, m3, m4, ix1, iy1, iz1, ix2, iy2, iz2

          real(kind=WP) :: xminb, yminb, zminb, xmaxb, ymaxb, zmaxb, xmine, ymine, zmine, xmaxe, ymaxe, zmaxe

          integer, dimension(GROUP_SIZE) :: prov_n, prov_e
          integer :: cellid
          integer :: old_mulnsn !< old mulnsn value before reallocation

          !write(*,*) "Number of remote nodes to process: ", nsnr, ii_stok

          if(nsnr == 0) return

!$OMP BARRIER

          xmin = xyzm(1)
          ymin = xyzm(2)
          zmin = xyzm(3)
          xmax = xyzm(4)
          ymax = xyzm(5)
          zmax = xyzm(6)
          xminb = xyzm(7)
          yminb = xyzm(8)
          zminb = xyzm(9)
          xmaxb = xyzm(10)
          ymaxb = xyzm(11)
          zmaxb = xyzm(12)

          j_stok = 0
          if(flagremnode == 2) then
            allocate(tagremnode(numnod))
            do i=1,numnod
              tagremnode(i) = 0
            end do
          end if
!$OMP BARRIER
!$OMP DO SCHEDULE(DYNAMIC)
          do ne=1,nrtm
            if(stf(ne) == zero)cycle
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                tagremnode(remnod(i)) = 1
              end do
            end if
            if(igap == 0)then
              aaa = tzinf+curv_max(ne)
            else
              aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,bgapsmx+gap_m(ne)))+dgapload,drad)
            end if

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

            sx = (yy3-yy1)*(zz4-zz2) - (zz3-zz1)*(yy4-yy2)
            sy = (zz3-zz1)*(xx4-xx2) - (xx3-xx1)*(zz4-zz2)
            sz = (xx3-xx1)*(yy4-yy2) - (yy3-yy1)*(xx4-xx2)
            s2 = sx*sx + sy*sy + sz*sz

            if(nbx>1) then
              ix1=int(nbx*(xmine-aaa-xminb)/(xmaxb-xminb))
              ix2=int(nbx*(xmaxe+aaa-xminb)/(xmaxb-xminb))
            else
              ix1=-2
              ix2=1
            end if

            if(nby>1) then
              iy1=int(nby*(ymine-aaa-yminb)/(ymaxb-yminb))
              iy2=int(nby*(ymaxe+aaa-yminb)/(ymaxb-yminb))
            else
              iy1=-2
              iy2=1
            end if

            if(nbz>1) then
              iz1=int(nbz*(zmine-aaa-zminb)/(zmaxb-zminb))
              iz2=int(nbz*(zmaxe+aaa-zminb)/(zmaxb-zminb))
            else
              iz1=-2
              iz2=1
            end if

            ix1=max(1,2+min(nbx,ix1))
            iy1=max(1,2+min(nby,iy1))
            iz1=max(1,2+min(nbz,iz1))

            ix2=max(1,2+min(nbx,ix2))
            iy2=max(1,2+min(nby,iy2))
            iz2=max(1,2+min(nbz,iz2))

            do iz = iz1,iz2
              do iy = iy1,iy2
                do ix = ix1,ix2
                  cellid = (iz-1)*(nbx+2)*(nby+2)+(iy-1)*(nbx+2)+ix
                  jj = voxel_remote(cellid)
                  do while(jj > 0)
                    ! jj is 1-based remote index; actual remote data is at xrem(:,jj)/irem(:,jj)
                    j = jj

                    delnod = 0
                    if(flagremnode == 2) then
                      k = kremnod(2*(ne-1)+2) + 1
                      l = kremnod(2*(ne-1)+3)
                      do m=k,l
                        if(remnod(m) == -irem(2,j) ) then
                          delnod = delnod + 1
                          exit
                        end if
                      end do
                      if(delnod /= 0)goto 400
                    end if

                    xs = xrem(1,j)
                    ys = xrem(2,j)
                    zs = xrem(3,j)
                    if(igap /= 0)then
                      aaa = marge+curv_max(ne)+max(min(gapmax,max(gapmin,xrem(9,j)+gap_m(ne)))+dgapload,drad)
                    end if

                    if(xs<=xmine-aaa)goto 400
                    if(xs>=xmaxe+aaa)goto 400
                    if(ys<=ymine-aaa)goto 400
                    if(ys>=ymaxe+aaa)goto 400
                    if(zs<=zmine-aaa)goto 400
                    if(zs>=zmaxe+aaa)goto 400

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
                      if(d2 > a2)goto 400
                    end if

                    j_stok = j_stok + 1
                    ! Store nsn+jj so cand_n is compatible with downstream (remote nodes > nsn)
                    prov_n(j_stok) = nsn + jj
                    prov_e(j_stok) = ne

                    if(j_stok == GROUP_SIZE) then
                      if(j_stok + ii_stok > mulnsn) then
                        old_mulnsn = mulnsn
                        multimp_param = multimp_param +4
                        mulnsn = mulnsn * multimp_param
                        ! Reallocate candidate arrays to the new size
                        call extend_array(cand_n, old_mulnsn, mulnsn)
                        call extend_array(cand_e, old_mulnsn, mulnsn)
                        if (ifq /= 0) call extend_array(ifpen, old_mulnsn, mulnsn)
                        if (inacti == 5 .or. inacti == 6 .or. inacti == 7) call extend_array(cand_p, old_mulnsn, mulnsn)
                        if (itied /= 0) call extend_array(cand_f, 8*old_mulnsn, 8*mulnsn)
                      end if
                      call inter7_filter_cand(&
                      &                   j_stok,irect  ,x     ,nsv   ,ii_stok,&
                      &                   cand_n,cand_e ,mulnsn,marge  ,&
                      &                   prov_n ,prov_e,eshift,inacti ,&
                      &                   ifq   ,cand_a ,cand_p,ifpen ,nsn    ,&
                      &                   oldnum,nsnrold,igap  ,gap   ,gap_s  ,&
                      &                   gap_m ,gapmin ,gapmax,curv_max,&
                      &                   gap_s_l,gap_m_l,drad,itied    ,&
                      &                   cand_f ,dgapload, numnod,&
                      &                   nsnr, nrtm, isznsnr,&
                      &                   xrem ,s_xrem)
                      j_stok = 0
                    end if

400                 continue
                    jj = next_nod_remote(jj)
                  end do
                end do
              end do
            end do
            if(flagremnode == 2) then
              k = kremnod(2*(ne-1)+1)+1
              l = kremnod(2*(ne-1)+2)
              do i=k,l
                tagremnode(remnod(i)) = 0
              end do
            end if
          end do
!$OMP END DO
          if(j_stok > 0) then 
            if(j_stok + ii_stok > mulnsn) then
              old_mulnsn = mulnsn
              multimp_param = multimp_param +4
              mulnsn = mulnsn * multimp_param
              ! Reallocate candidate arrays to the new size
              call extend_array(cand_n, old_mulnsn, mulnsn)
              call extend_array(cand_e, old_mulnsn, mulnsn)
              if (ifq /= 0) call extend_array(ifpen, old_mulnsn, mulnsn)
              if (inacti == 5 .or. inacti == 6 .or. inacti == 7) call extend_array(cand_p, old_mulnsn, mulnsn)
              if (itied /= 0) call extend_array(cand_f, 8*old_mulnsn, 8*mulnsn)
            end if
             call inter7_filter_cand(&
          &                   j_stok,irect  ,x     ,nsv   ,ii_stok,&
          &                   cand_n,cand_e ,mulnsn,marge  ,&
          &                   prov_n ,prov_e,eshift,inacti ,&
          &                   ifq   ,cand_a ,cand_p,ifpen ,nsn    ,&
          &                   oldnum,nsnrold,igap  ,gap   ,gap_s  ,&
          &                   gap_m ,gapmin ,gapmax,curv_max,&
          &                   gap_s_l,gap_m_l,drad,itied    ,&
          &                   cand_f ,dgapload, numnod,&
          &                   nsnr, nrtm, isznsnr,&
          &                   xrem ,s_xrem)
          endif

!$OMP BARRIER
          if(flagremnode == 2) then
            if(allocated(tagremnode)) deallocate(tagremnode)
          end if
          if(i_mem == 2) then
             ! In memory mode 2, we only search for candidates but do not store them (no filtering).
             ! This is used to overlap communication with computation. Candidate pairs will be searched again after MPI_WAITANY.
          !    write(*,*) "Memory mode 2: Not enough memory (remote)",mulnsn
          endif
         ! write(*,*) "Number of candidate pairs found in remote search: ", ii_stok

          return
        end subroutine INTER7_CANDIDATE_PAIRS_REMOTE

!! \brief write the data to a file

      end module INTER7_CANDIDATE_PAIRS_MOD



