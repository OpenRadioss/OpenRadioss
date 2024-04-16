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
!chd|====================================================================
!chd|  LAW50_UPD                     source/materials/mat/mat050/law50_upd.F
!chd|-- called by -----------
!chd|        UPDMAT                        source/materials/updmat.F     
!chd|-- calls ---------------
!chd|====================================================================

      module law50_upd_mod
      contains

! ======================================================================================================================
! \brief replace input functions in orthotropic directions by 2D tables depending on &
!        strain and strain rate
!! \details 

! ======================================================================================================================

      subroutine law50_upd(mat_param,nuparam,uparam,nfunc,ifunc,snpc,stf,npc,pld)

!----------------------------------------------- 
!     M o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use law50_table_mod
! ----------------------------------------------------------------------------------------------------------------------

      implicit none

! ----------------------------------------------------------------------------------------------------------------------
!     included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!     D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in) :: nfunc                             !< number of material law functions
      integer ,intent(in) :: nuparam                           !< size of uparam table
      integer ,intent(in) :: snpc                              !< size function table adresses
      integer ,intent(in) :: stf                               !< size of function table values
      integer ,dimension(nfunc)   ,intent(in)    :: ifunc      !< material law functions
      integer ,dimension(snpc)    ,intent(in)    :: npc        !< function table adresses
      my_real ,dimension(stf)     ,intent(in)    :: pld        !< function table values
      my_real ,dimension(nuparam) ,intent(inout) :: uparam     !< material law parameter table
      type (matparam_struct_)     ,intent(inout) :: mat_param  !< material law parameter module
!-----------------------------------------------
!     L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,ipt,ncurv,lmax,func_n
      integer :: ic1,ic2,ntable
      my_real :: s1,t1
      integer ,dimension(nfunc) :: len
      my_real ,dimension(nfunc) :: rate,yfac
      my_real ,dimension(:,:) ,allocatable :: xi,yi
      integer :: n11,n22,n33,n12,n23,n31
!=======================================================================
      ntable = 6
      mat_param%ntable = ntable
      allocate (mat_param%table(ntable))
!
      ! number of functions by direction   
      n11 = nint(uparam(45))
      n22 = nint(uparam(46))
      n33 = nint(uparam(47))
      n12 = nint(uparam(48))
      n23 = nint(uparam(49))
      n31 = nint(uparam(50))
!------------------------------------------------------      
!     create X,Y vectors for all curves of each table before unifying abscissas
!--------------------------------------------------------
      !  direction 11 - table(1)
!--------------------------------------------------------
      
      lmax  = 0
      ncurv = n11
      do i = 1,ncurv
        func_n  = ifunc(i)
        rate(i) = uparam(i + 14)
        yfac(i) = uparam(i + 51)
        len(i)  = (npc(func_n+1) - npc(func_n)) / 2
        lmax    = max(lmax,len(i))
      end do
!
      allocate (xi(lmax,ncurv))
      allocate (yi(lmax,ncurv))
!
      do i = 1,ncurv
        func_n = ifunc(i)
        ic1 = npc(func_n)
        ic2 = npc(func_n+1) - 2
        s1  = pld(ic1)
        ipt = 0
        do j = ic1,ic2,2
          ipt = ipt + 1
          s1 = pld(j)
          t1 = pld(j+1) * yfac(i)
          xi(ipt,i) = s1
          yi(ipt,i) = t1
        end do
        len(i) = ipt
      end do
!
      call law50_table(mat_param%table(1),ncurv,len,lmax,rate,xi,yi)      
      
      deallocate (yi)
      deallocate (xi)
!--------------------------------------------------------
      !  direction 22 - table(2)
!--------------------------------------------------------
      
      lmax  = 0
      ncurv = n22
      do i = 1,ncurv
        func_n  = ifunc(i+5)
        rate(i) = uparam(i + 19)
        yfac(i) = uparam(i + 56)
        len(i)  = (npc(func_n+1) - npc(func_n)) / 2
        lmax    = max(lmax,len(i))
      end do
!
      allocate (xi(lmax,ncurv))
      allocate (yi(lmax,ncurv))
!
      do i = 1,ncurv
        func_n = ifunc(i+5)
        ic1 = npc(func_n)
        ic2 = npc(func_n+1) - 2
        s1  = pld(ic1)
        ipt = 0
        do j = ic1,ic2,2
          ipt = ipt + 1
          s1 = pld(j)
          t1 = pld(j+1) * yfac(i)
          xi(ipt,i) = s1
          yi(ipt,i) = t1
        end do
        len(i) = ipt
      end do
!
      call law50_table(mat_param%table(2),ncurv,len,lmax,rate,xi,yi)      
      
      deallocate (yi)
      deallocate (xi)
!--------------------------------------------------------
      !  direction 33 - table(3)
!--------------------------------------------------------
      
      lmax  = 0
      ncurv = n33
      do i = 1,ncurv
        func_n  = ifunc(i+10)
        rate(i) = uparam(i + 24)
        yfac(i) = uparam(i + 61)
        len(i)  = (npc(func_n+1) - npc(func_n)) / 2
        lmax    = max(lmax,len(i))
      end do
!
      allocate (xi(lmax,ncurv))
      allocate (yi(lmax,ncurv))
!
      do i = 1,ncurv
        func_n = ifunc(i+10)
        ic1 = npc(func_n)
        ic2 = npc(func_n+1) - 2
        s1  = pld(ic1)
        ipt = 0
        do j = ic1,ic2,2
          ipt = ipt + 1
          s1 = pld(j)
          t1 = pld(j+1) * yfac(i)
          xi(ipt,i) = s1
          yi(ipt,i) = t1
        end do
        len(i) = ipt
      end do
!
      call law50_table(mat_param%table(3),ncurv,len,lmax,rate,xi,yi)      
      
      deallocate (yi)
      deallocate (xi)
!--------------------------------------------------------
      !  direction 12 - table(4)
!--------------------------------------------------------
      
      lmax  = 0
      ncurv = n12
      do i = 1,ncurv
        func_n  = ifunc(i+15)
        rate(i) = uparam(i + 29)
        yfac(i) = uparam(i + 66)
        len(i)  = (npc(func_n+1) - npc(func_n)) / 2
        lmax    = max(lmax,len(i))
      end do
!
      allocate (xi(lmax,ncurv))
      allocate (yi(lmax,ncurv))
!
      do i = 1,ncurv
        func_n = ifunc(i+15)
        ic1 = npc(func_n)
        ic2 = npc(func_n+1) - 2
        s1  = pld(ic1)
        ipt = 0
        do j = ic1,ic2,2
          ipt = ipt + 1
          s1 = pld(j)
          t1 = pld(j+1) * yfac(i)
          xi(ipt,i) = s1
          yi(ipt,i) = t1
        end do
        len(i) = ipt
      end do
!
      call law50_table(mat_param%table(4),ncurv,len,lmax,rate,xi,yi)      
      
      deallocate (yi)
      deallocate (xi)
!--------------------------------------------------------
      !  direction 23 - table(5)
!--------------------------------------------------------
      
      lmax  = 0
      ncurv = n23
      do i = 1,ncurv
        func_n  = ifunc(i+20)
        rate(i) = uparam(i + 34)
        yfac(i) = uparam(i + 71)
        len(i)  = (npc(func_n+1) - npc(func_n)) / 2
        lmax    = max(lmax,len(i))
      end do
!
      allocate (xi(lmax,ncurv))
      allocate (yi(lmax,ncurv))
!
      do i = 1,ncurv
        func_n = ifunc(i+20)
        ic1 = npc(func_n)
        ic2 = npc(func_n+1) - 2
        s1  = pld(ic1)
        ipt = 0
        do j = ic1,ic2,2
          ipt = ipt + 1
          s1 = pld(j)
          t1 = pld(j+1) * yfac(i)
          xi(ipt,i) = s1
          yi(ipt,i) = t1
        end do
        len(i) = ipt
      end do
!
      call law50_table(mat_param%table(5),ncurv,len,lmax,rate,xi,yi)      
      
      deallocate (yi)
      deallocate (xi)
!--------------------------------------------------------
      !  direction 31 - table(6)
!--------------------------------------------------------
      
      lmax  = 0
      ncurv = n31
      do i = 1,ncurv
        func_n  = ifunc(i+25)
        rate(i) = uparam(i + 39)
        yfac(i) = uparam(i + 76)
        len(i)  = (npc(func_n+1) - npc(func_n)) / 2
        lmax    = max(lmax,len(i))
      end do
!
      allocate (xi(lmax,ncurv))
      allocate (yi(lmax,ncurv))
!
      do i = 1,ncurv
        func_n = ifunc(i+25)
        ic1 = npc(func_n)
        ic2 = npc(func_n+1) - 2
        s1  = pld(ic1)
        ipt = 0
        do j = ic1,ic2,2
          ipt = ipt + 1
          s1 = pld(j)
          t1 = pld(j+1) * yfac(i)
          xi(ipt,i) = s1
          yi(ipt,i) = t1
        end do
        len(i) = ipt
      end do
!
      call law50_table(mat_param%table(6),ncurv,len,lmax,rate,xi,yi)      
      
      deallocate (yi)
      deallocate (xi)
!-----------
      return
      end subroutine law50_upd
      end module law50_upd_mod
