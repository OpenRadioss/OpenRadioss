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
      !||    calculp2_mod    ../starter/source/materials/mat/mat057/calculp2.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat57   ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat78   ../starter/source/materials/mat/mat078/hm_read_mat78.F
      !||====================================================================
     module calculp2_mod
      contains
      !||====================================================================
      !||    calculp2        ../starter/source/materials/mat/mat057/calculp2.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat57   ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat78   ../starter/source/materials/mat/mat078/hm_read_mat78.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      subroutine calculp2(a    ,c    ,h    ,p    ,m    ,r45  )  
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------   
      use constant_mod                    
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
      implicit none 
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      my_real, intent(in)    :: a   !< Barlat 89 a parameter
      my_real, intent(in)    :: c   !< Barlat 89 c parameter
      my_real, intent(in)    :: h   !< Barlat 89 h parameter
      my_real, intent(in)    :: m   !< Barlat 89 m exponent
      my_real, intent(in)    :: r45 !< Lankford coefficient in 45 deg. direction
      my_real, intent(inout) :: p   !< Barlat 89 p parameter
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer i
      my_real gama,alpha,beta,f,df,c1,c2,c3,c4,c5,pp,m2,                       &
        aba2,abb2,abg1,ca,cb,ca1,cb1
      integer, parameter :: nmax = 10
!--------------------------------------------------------------
      m2 = m-two
      c1 = two**m*c
      c2 = fourth*(1-h)
      c2 = c2*c2
      c3 = fourth*(1+h)
      c4 = half*(1+h)
      pp = p
      do i = 1,nmax
       gama  = sqrt(c2 + fourth*pp*pp)
       alpha = c3-gama
       beta  = c3+gama
       c5    = two*c2/gama
       aba2  = abs(alpha)**m2
       abb2  = abs(beta)**m2
       ca    = aba2*(c4-c5)*(one+r45)
       cb    = abb2*(c4+c5)*(one+r45)
       ca1   = aba2*alpha
       cb1   = abb2*beta
       abg1  = c1*gama**(m-one)
       f     = a*(alpha*(ca1-ca) + beta*(cb1-cb)) + gama*abg1
       df    = a*((m-one)*(ca-cb)-(ca1-cb1)*(m+(one+r45)*c5/gama)) + m*abg1
       df    = half*df*pp/gama
       pp    = pp - f/df
      enddo
      p = pp
!
      end subroutine calculp2
     end module calculp2_mod







