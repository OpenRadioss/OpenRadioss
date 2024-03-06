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
      module damping_vref_rby_stiff_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes nodal stiffness on main nodes of RBODY for /DAMP/VREL
!=======================================================================================================================
!
        subroutine damping_vref_rby_stiff(numnod,nnpby,nrbykin,nrbykin_l,npby,                       &
                                          rby6_c,ms,in,stifn,stifr,size_rby6_c,irbkin_l)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: one,two,em20
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
          integer,                                   intent(in) :: numnod                      !< number of nodes         
          integer,                                   intent(in) :: nnpby                       !< first dimension of array NPBY
          integer,                                   intent(in) :: nrbykin                     !< number of rigid bodies
          integer,                                   intent(in) :: nrbykin_l                   !< number of rigid bodies on this proc         
          integer,                                   intent(in) :: npby(nnpby,nrbykin)         !< main structure for rigid bodies
          integer,                                   intent(in) :: size_rby6_c                 !< dimension of array rby6c
          integer,                                   intent(in) :: irbkin_l(nrbykin)           !< local global id of rigid_body 
          my_real,                                   intent(in) :: ms(numnod)                  !< nodal mass
          my_real,                                   intent(in) :: in(numnod)                  !< nodal inertia      
          my_real,                                intent(inout) :: stifn(numnod)               !< nodal stiffness
          my_real,                                intent(inout) :: stifr(numnod)               !< nodal rotational stiffness                                  
          double precision,                          intent(in) :: rby6_c(2,6,size_rby6_c)         !< working array for rigid body damping assembly  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nd,m,n
          my_real :: c_tot,cr_tot,dd,fac
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------        
!         
          do n=1,nrbykin_l

            nd = irbkin_l(n)
            if(npby(7,nd)>0) THEN

              m = npby(1,nd)

              c_tot = rby6_c(1,1,nd)+rby6_c(1,2,nd)+rby6_c(1,3,nd)+rby6_c(1,4,nd)+rby6_c(1,5,nd)+rby6_c(1,6,nd)
              cr_tot= rby6_c(2,1,nd)+rby6_c(2,2,nd)+rby6_c(2,3,nd)+rby6_c(2,4,nd)+rby6_c(2,5,nd)+rby6_c(2,6,nd)

              dd = c_tot/sqrt(two*stifn(m)*ms(m))
              fac = sqrt(one + dd*dd) - dd
              stifn(m) = stifn(m) / fac**2
!
              dd = cr_tot/sqrt(two*stifr(m)*in(m))
              fac = sqrt(one + dd*dd) - dd
              stifr(m) = stifr(m) / fac**2
!              
            endif  
!
          enddo          
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_vref_rby_stiff
      end module damping_vref_rby_stiff_mod