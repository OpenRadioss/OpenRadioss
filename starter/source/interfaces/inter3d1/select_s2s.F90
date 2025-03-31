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
      !||    select_s2s_mod   ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||--- called by ------------------------------------------------------
      !||    i2_surfi         ../starter/source/interfaces/inter3d1/i2_surfi.F90
      !||    i2_surfi_dim     ../starter/source/interfaces/inter3d1/i2_surfi_dim.F90
      !||====================================================================
      module select_s2s_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief this subroutine doing preparation to remove useless surf of the interface type2 w/ input surf/surf
      !||====================================================================
      !||    select_s2s     ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||--- called by ------------------------------------------------------
      !||    i2_surfi       ../starter/source/interfaces/inter3d1/i2_surfi.F90
      !||    i2_surfi_dim   ../starter/source/interfaces/inter3d1/i2_surfi_dim.F90
      !||--- calls      -----------------------------------------------------
      !||    norma4n        ../starter/source/interfaces/inter3d1/norma1.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine select_s2s(nsu1,nsu2,nodes1,nodes2,itag1,itag2,x,numnod,dsearch)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,half,third,fourth,ep20,em01
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
          integer,                                intent(in   ) :: nsu1                !< the size of surf1
          integer,                                intent(in   ) :: nsu2                !< the size of surf2
          integer,   dimension(nsu1,4),           intent(in   ) :: nodes1              !< seg1 nodes
          integer,   dimension(nsu2,4),           intent(in   ) :: nodes2              !< seg2 nodes
          integer,   dimension(nsu1),             intent(inout) :: itag1               !< tag array for surf1
          integer,   dimension(nsu2),             intent(inout) :: itag2               !< tag array for surf2
          integer,                                intent(in   ) :: numnod              !< the size of x
          my_real,                                intent(in   ) :: dsearch             !< search distance
          my_real,    dimension(3,numnod),        intent(in   ) :: x                   !< coordinates of the nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,l,k,m,n,ns,nj(4),ii,ifound,iwork(4),nj1(4),ier,jmin,i_old,n_buck,ihuge
          integer :: n_dir(3),ix,iy,iz,nsu_1,nsu_2,ichange,jj,ndiv,ndiv_min,nb_seg1(3),nb_seg2(3)
          my_real :: area1(nsu1),area2(nsu2),xs1(3,nsu1),xs2(3,nsu2),n1(3,nsu1),n2(3,nsu2)
          my_real :: ds,ds2,dsn,dmin,xj(3,4),marge1(nsu1),marge2(nsu2),angle,angle_min
          my_real :: marge,xmin(3),xmax(3),xmin2(3),xmax2(3),tol_d,xmin_i(3),xmax_i(3)
          my_real :: sz_g(3),sz_max,sz_min,marge_1,marge_2,xming(3),xmaxg(3),marge_g,ll,marge_max
          integer,  dimension(:), allocatable   :: ind_1,ind_2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      ds = dsearch
      angle_min = half  
      xmin(1:3) = ep20
      xmax(1:3) = -ep20
      marge_1 = zero !mean value 
      marge_max = zero
      n_buck = 100
!  1er surf
      do i=1,nsu1  
        nj(1:4) = nodes1(i,1:4)
        call norma4n(n1(1,i),n1(2,i),n1(3,i),area1(i),nj,x)
        xj(1:3,1:4) = x(1:3,nj(1:4))
        if (nj(3)==nj(4)) then 
          xmin(1:3) =min(xmin(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3))
          xmax(1:3) =max(xmax(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3))
          xs1(1:3,i) = third*(xj(1:3,1)+xj(1:3,2)+xj(1:3,3))
        else
          xmin(1:3) =min(xmin(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3),xj(1:3,4))
          xmax(1:3) =max(xmax(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3),xj(1:3,4))
          xs1(1:3,i) = fourth*(xj(1:3,1)+xj(1:3,2)+xj(1:3,3)+xj(1:3,4))
        end if
        ll = sqrt(area1(i))
        marge1(i) = max(ds,0.2*ll)
        marge_1 = marge_1 + ll
        marge_max = max(marge_max,ll)
      end do
      if (nsu1>0) marge_1 = marge_1/nsu1
      marge_g = marge_max
      nb_seg1(1:3) = (xmax(1:3)-xmin(1:3))/(n_buck*marge_1)
!  2nd 
      xmin2(1:3) = ep20
      xmax2(1:3) = -ep20
      marge_2 = zero
      marge_max = zero  
      do i=1,nsu2 
        nj(1:4) = nodes2(i,1:4)
        call norma4n(n2(1,i),n2(2,i),n2(3,i),area2(i),nj,x)
        xj(1:3,1:4) = x(1:3,nj(1:4))
        if (nj(3)==nj(4)) then 
          xmin2(1:3) =min(xmin2(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3))
          xmax2(1:3) =max(xmax2(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3))
          xs2(1:3,i) = third*(xj(1:3,1)+xj(1:3,2)+xj(1:3,3))
        else
          xmin2(1:3) =min(xmin2(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3),xj(1:3,4))
          xmax2(1:3) =max(xmax2(1:3),xj(1:3,1),xj(1:3,2),xj(1:3,3),xj(1:3,4))
          xs2(1:3,i) = fourth*(xj(1:3,1)+xj(1:3,2)+xj(1:3,3)+xj(1:3,4))
        end if
        ll = sqrt(area2(i))
        marge2(i) = max(ds,0.2*ll)
        marge_2 = marge_2 + ll
        marge_max = max(marge_max,ll)
      end do
      if (nsu2>0) marge_2 = marge_2/nsu2
      nb_seg2(1:3) = (xmax2(1:3)-xmin2(1:3))/(n_buck*marge_2)
      marge_g = half*(marge_g+marge_max)
      marge_g = max(ds,marge_g)
      xming(1:3) = min(xmin(1:3),xmin2(1:3))
      xmaxg(1:3) = max(xmax(1:3),xmax2(1:3))
      n_dir(1:3) = (nb_seg1(1:3)+nb_seg2(1:3))/2
      ihuge = 0
      k = 0
      do j=1,3
        if (n_dir(j)==0) cycle
        k= k+1
      end do
      ndiv_min = 1
      select case (k)
        case (1)
          if ((nsu1+nsu2)>2000000) ihuge = 1
        case (2)
          if ((nsu1+nsu2)>50000) ihuge = 1
        case (3)
          ihuge = 1
      end select
      allocate(ind_1(nsu1))
      allocate(ind_2(nsu2))
      if (ihuge > 0) then !have to do bucket sorting
        sz_g(1:3) = xmaxg(1:3)-xming(1:3)
        sz_max=max(sz_g(1),sz_g(2),sz_g(3))
        sz_min=min(sz_g(1),sz_g(2),sz_g(3))
        n_dir(1:3) = max(ndiv_min,n_dir(1:3))
        if ((nsu1+nsu2)>1000000) then
          if (max(n_dir(1),n_dir(2),n_dir(3))>10) then 
            do j=1,3
              if (sz_g(j)==sz_min) then
                n_dir(j) = n_dir(j)+1
              else 
                n_dir(j) = n_dir(j)/2 + n_dir(j)
              end if
            end do
          end if
        end if
        sz_g(1:3) = sz_g(1:3)/n_dir(1:3)
      else 
        n_dir(1:3) = 1
        sz_g(1:3) = zero
        marge_g = zero
      end if
!       buckset in 3 directions
      do ix=1,n_dir(1)
        do iy=1,n_dir(2)
          do iz=1,n_dir(3)
            xmin_i(1) = xming(1) + (ix-1)*sz_g(1) -marge_g
            xmin_i(2) = xming(2) + (iy-1)*sz_g(2) -marge_g
            xmin_i(3) = xming(3) + (iz-1)*sz_g(3) -marge_g
            xmax_i(1) = xmaxg(1) + (ix-n_dir(1))*sz_g(1) +marge_g
            xmax_i(2) = xmaxg(2) + (iy-n_dir(2))*sz_g(2) +marge_g
            xmax_i(3) = xmaxg(3) + (iz-n_dir(3))*sz_g(3) +marge_g
            nsu_1 = 0
            do i=1,nsu1  
              if(xs1(1,i) < xmin_i(1) .or. xs1(1,i) > xmax_i(1)) cycle
              if(xs1(2,i) < xmin_i(2) .or. xs1(2,i) > xmax_i(2)) cycle
              if(xs1(3,i) < xmin_i(3) .or. xs1(3,i) > xmax_i(3)) cycle
              nsu_1 = nsu_1 + 1
              ind_1(nsu_1) = i
            end do
            nsu_2 = 0
            do i=1,nsu2  
              if(xs2(1,i) < xmin_i(1) .or. xs2(1,i) > xmax_i(1)) cycle
              if(xs2(2,i) < xmin_i(2) .or. xs2(2,i) > xmax_i(2)) cycle
              if(xs2(3,i) < xmin_i(3) .or. xs2(3,i) > xmax_i(3)) cycle
              nsu_2 = nsu_2 + 1
              ind_2(nsu_2) = i
            end do
            if (nsu_1 > 0 .and. nsu_2 > 0) then
! check double : isu2 -> sub isu1
               do j=1,nsu_2 ! if surf2 has the same seg in surf1
                 i = ind_2(j) 
                 if (itag2(i)==0) cycle
                 nj(1:4) = nodes2(i,1:4)
                 call MYQSORT_INT(4, nj, iwork, ier)
                 do jj = 1,nsu_1
                   ii = ind_1(jj) 
                   nj1(1:4) = nodes1(ii,1:4)
                   jmin=min(nj1(1),nj1(2),nj1(3),nj1(4))
                   if(jmin/=nj(1)) cycle
                   call MYQSORT_INT(4, nj1, iwork, ier)
                   if(nj1(2)/=nj(2).or.nj1(3)/=nj(3).or.nj1(4)/=nj(4)) cycle
                   itag2(i) = 0  
                 end do
               end do
!  tag usful surf1 by 2nd nodes found, remove 2nd surf by distance and angle
               do k=1,nsu_1
                 i = ind_1(k)
                 if (itag1(i)==0) cycle
                 nj(1:4) = nodes1(i,1:4)
                 xj(1:3,1:4) = x(1:3,nj(1:4))
                 xmin(1:3) = xj(1:3,1)
                 xmax(1:3) = xj(1:3,1)
                 do j=2,4 
                    xmin(1:3) = min(xmin(1:3),xj(1:3,j))
                    xmax(1:3) = max(xmax(1:3),xj(1:3,j))
                 end do
                 ifound = 0
                 dmin = marge1(i)
                 do jj = 1,nsu_2
                   ii = ind_2(jj) 
                   if (itag2(ii)==0) cycle
                   marge = max(marge1(i),marge2(ii))
                   xmin2(1:3) = xmin(1:3)-marge
                   xmax2(1:3) = xmax(1:3)+marge
                   if(xs2(1,ii) < xmin2(1) .or. xs2(1,ii) > xmax2(1)) cycle
                   if(xs2(2,ii) < xmin2(2) .or. xs2(2,ii) > xmax2(2)) cycle
                   if(xs2(3,ii) < xmin2(3) .or. xs2(3,ii) > xmax2(3)) cycle
                   angle = n1(1,i)*n2(1,ii)+n1(2,i)*n2(2,ii)+n1(3,i)*n2(3,ii)
                   if (abs(angle) < angle_min) cycle
                   dsn = abs((xs1(1,i)-xs2(1,ii))*n1(1,i)+(xs1(2,i)-xs2(2,ii))*n1(2,i)+(xs1(3,i)-xs2(3,ii))*n1(3,i))
!                   ds2 = (xs1(1,i)-xs2(1,ii))**2+(xs1(2,i)-xs2(2,ii))**2+(xs1(3,i)-xs2(3,ii))**2
                   tol_d=0.1*marge2(ii)
                   if (dsn < dmin) then 
                     i_old = ifound
                     ifound=ii 
                     if (i_old>0.and. (dsn+tol_d) < dmin) itag2(i_old) = 0
                     dmin = dsn
                   elseif (dsn > dmin+tol_d .and. ifound>0) then
                     itag2(ii) = 0 
                   endif
                   if (ifound==0) ifound=ii
                 end do
                 if (ifound==0) itag1(i) = ifound
               end do
!  tag usful surf2 by nodes in surf1 found, remove surf1 by distance and angle
               do k=1,nsu_2
                 i = ind_2(k)
                 if (itag2(i)==0) cycle
                 nj(1:4) = nodes2(i,1:4)
                 xj(1:3,1:4) = x(1:3,nj(1:4))
                 xmin(1:3) = xj(1:3,1)
                 xmax(1:3) = xj(1:3,1)
                 do j=2,4 
                    xmin(1:3) = min(xmin(1:3),xj(1:3,j))
                    xmax(1:3) = max(xmax(1:3),xj(1:3,j))
                 end do
                 ifound=0
                 dmin = marge2(i)  
                 do jj = 1,nsu_1
                   ii = ind_1(jj)
                   if (itag1(ii)==0) cycle
                   marge = max(marge1(ii),marge2(i))
                   xmin2(1:3) = xmin(1:3)-marge
                   xmax2(1:3) = xmax(1:3)+marge
                   if(xs1(1,ii) < xmin2(1) .or. xs1(1,ii) > xmax2(1)) cycle
                   if(xs1(2,ii) < xmin2(2) .or. xs1(2,ii) > xmax2(2)) cycle
                   if(xs1(3,ii) < xmin2(3) .or. xs1(3,ii) > xmax2(3)) cycle
                   angle = n1(1,ii)*n2(1,i)+n1(2,ii)*n2(2,i)+n1(3,ii)*n2(3,i)
                   if (abs(angle) < angle_min) cycle
                   dsn =abs((xs1(1,ii)-xs2(1,i))*n2(1,i)+(xs1(2,ii)-xs2(2,i))*n2(2,i)+(xs1(3,ii)-xs2(3,i))*n2(3,i))
                   tol_d=0.1*marge1(ii)
                   if (dsn < dmin) then 
                     i_old = ifound
                     ifound=ii 
                     if (i_old>0.and.(dsn+tol_d) < dmin) itag1(i_old) = 0
                     dmin = dsn
                   elseif (dsn > dmin+tol_d .and. ifound>0) then
                     itag1(ii) = 0 
                   endif
                   if (ifound==0) ifound=ii
                 end do
                 if (ifound==0) itag2(i) = ifound
               end do
            elseif (nsu_1 > 0) then ! nsu_2 = 0
              do ii=1,nsu_1
                i = ind_1(ii)
                itag1(i) = 0
              end do
            elseif (nsu_2 > 0) then ! nsu_1 = 0
              do ii=1,nsu_2
                i = ind_2(ii)
                itag2(i) = 0
              end do
            end if
          end do
        end do
      end do
      deallocate(ind_1)
      deallocate(ind_2)
        end subroutine select_s2s
      end module select_s2s_mod







