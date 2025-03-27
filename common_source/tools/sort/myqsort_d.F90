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
! ----------------------------------------------------------------------------------------------------------------------
      !||====================================================================
      !||    myqsort_d_mod      ../common_source/tools/sort/myqsort_d.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_table2_1   ../starter/source/tools/curve/hm_read_table2_1.F
      !||====================================================================
      module myqsort_d_mod
      
      contains
!

      !||====================================================================
      !||    myqsort_d          ../common_source/tools/sort/myqsort_d.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_table2_1   ../starter/source/tools/curve/hm_read_table2_1.F
      !||====================================================================
      subroutine myqsort_d(n, a, perm, error)
!-----------------------------------------------
!!< brief      q u i c k s o r t algorithm using double precision input vector
!< sedgewick algorithm from "implementing quicksort programs"
!<     a: data
!<     n: len
!<     perm: permutations
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
       implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer n,error,perm(n)
      double precision a(n)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: stacklen
      integer :: treshold
      integer :: done
! the max stacklen <= 1 + 2 x log2 (n+1)/(treshold + 2)
      parameter( stacklen = 128 , treshold   =  9 )
!
      integer :: i 
      integer :: iplus1
      integer :: j
      integer :: jminus1
      integer :: k 
      integer :: left
      integer :: llen
      integer :: right 
      integer :: rlen
      integer :: top  
      integer :: stack(stacklen)
      double precision :: rk, rv
!===============================================================================
      error = 0
!
      if  (n < 1)  then
        error = -1
        return
      endif

      if  (n == 1)  then
         perm(1)=1
         return
      endif

      do  i = 1, n
         perm(i) = i
      enddo
!
      top = 1
      left = 1
      right = n
      if (n <= treshold) then
        done = 1
      else
        done = 0
      endif

!     quicksort                                                              
!
       do while (done /= 1)
         rk = a((left+right)/2)
         a((left+right)/2) = a(left)
         a(left) = rk
!
         k = perm((left+right)/2)
         perm((left+right)/2) = perm(left)
         perm(left) = k

         if( a(left+1) > a(right) ) then
           rk = a(left+1)
           a(left+1) = a(right)
           a(right) = rk
           k = perm(left+1)
           perm(left+1) = perm(right)
           perm(right) = k
         endif
         if( a(left) > a(right) ) then
           rk = a(left)
           a(left) = a(right)
           a(right) = rk
           k = perm(left)
           perm(left) = perm(right)
           perm(right) = k
         endif
         if( a(left+1) >  a(left) ) then
           rk = a(left+1)
           a(left+1) = a(left)
           a(left) = rk
           k = perm(left+1)
           perm(left+1) = perm(left)
           perm(left) = k
         endif

         rv = a(left)
         i = left+1
         j = right

         do while(j >= i)
           i  = i + 1
           do while(a(i) <  rv) 
             i = i +1
           enddo
           j = j - 1
           do while(a(j) > rv)
             j = j - 1  
           enddo
           if (j >= i) then 
             rk = a(i)
             a(i) = a(j)
             a(j) = rk
             k = perm(i)
             perm(i) = perm(j)
             perm(j) = k
           endif
         enddo
!
         rk = a(left)
         a(left) = a(j)
         a(j) = rk
!
         k = perm(left)
         perm(left) = perm(j)
         perm(j) = k
!
         llen = j-left
         rlen = right - i + 1

         if(max(llen, rlen) <= treshold ) then
             if  (top == 1) then
               done = 1
             else
               top = top - 2
               left = stack(top)
               right = stack(top+1)
             endif
         else if(min(llen, rlen) <=  treshold) then 
             if( llen > rlen ) then 
               right = j - 1
             else
               left = i
             endif
         else
           if( llen > rlen ) then 
              stack(top) = left
              stack(top+1) = j-1
              left = i
            else
              stack(top) = i
              stack(top+1) = right
              right = j-1
            endif
            top = top + 2
         endif
       end do
!
!     insertion sort 
!
      i = n - 1
      iplus1 = n
      do while (i > 0) 
        if( a(i) > a(iplus1) ) then 
          rk = a(i)
          k  = perm(i)
          j = iplus1
          jminus1 = i
          do while(a(j) <  rk)
            a(jminus1) = a(j) 
            perm(jminus1) = perm(j)
            jminus1 = j
            j = j + 1
            if  ( j > n )  exit
          enddo
          a(jminus1) = rk
          perm(jminus1) = k
        endif
!
        iplus1 = i
        i = i - 1
      enddo
!-----------
      return
      end subroutine

      end module myqsort_d_mod
