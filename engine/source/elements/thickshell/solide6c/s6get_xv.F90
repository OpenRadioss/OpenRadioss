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
      !||====================================================================
      !||    s6get_xv_mod   ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
      !||--- called by ------------------------------------------------------
      !||    s6cforc3       ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||====================================================================
      module s6get_xv_mod
      contains
! ======================================================================================================================
! \brief get x,v in global(basic) coordinate system for penta6 element
! ======================================================================================================================
      !||====================================================================
      !||    s6get_xv   ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
      !||--- called by ------------------------------------------------------
      !||    s6cforc3   ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||====================================================================
        subroutine s6get_xv(                                               &
                            nc1,      nc2,      nc3,                       &                       
                            nc4,      nc5,      nc6,                       &
                             x1,       x2,       x3,                       &      
                             x4,       x5,       x6,                       &      
                             y1,       y2,       y3,                       &      
                             y4,       y5,       y6,                       &      
                             z1,       z2,       z3,                       &      
                             z4,       z5,       z6,                       &      
                            vx1,      vx2,      vx3,                       &      
                            vx4,      vx5,      vx6,                       &      
                            vy1,      vy2,      vy3,                       &      
                            vy4,      vy5,      vy6,                       &      
                            vz1,      vz2,      vz3,                       &      
                            vz4,      vz5,      vz6,                       &      
                              x,      xdp,        v,                       &
                         numnod,   ismstr,     nel )
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                              :: nel             !< number of elements
          integer, intent(in)                              :: numnod          !< number of node
          integer, intent(in)                              :: ismstr          !< small strain falg
          integer, dimension(mvsiz), intent(in   )         ::             &            
                                    nc1,nc2,nc3,nc4,nc5,nc6                   !< connectivity
          my_real, dimension(3,numnod),intent(in   )       :: x               !< coordinate array 
          my_real, dimension(3,numnod),intent(in   )       :: v               !< velocity 
          double precision, dimension(3,numnod),intent(in) :: xdp             !< coordinate array in DP
          my_real, dimension(mvsiz), intent(inout)         ::             &        
                                       x1,x2,x3,x4,x5,x6,                 &
                                       y1,y2,y3,y4,y5,y6,                 &
                                       z1,z2,z3,z4,z5,z6,                 &
                                 vx1,vx2,vx3,vx4,vx5,vx6,                 &
                                 vy1,vy2,vy3,vy4,vy5,vy6,                 &
                                 vz1,vz2,vz3,vz4,vz5,vz6                      !< nodal coordinate array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
       if (ismstr == 1.or.ismstr >= 11) then
#ifdef MYREAL8 
         do i=1,nel
           x1(i)=x(1,nc1(i))
           y1(i)=x(2,nc1(i))
           z1(i)=x(3,nc1(i))
           x2(i)=x(1,nc2(i))
           y2(i)=x(2,nc2(i))
           z2(i)=x(3,nc2(i))
           x3(i)=x(1,nc3(i))
           y3(i)=x(2,nc3(i))
           z3(i)=x(3,nc3(i))
           x4(i)=x(1,nc4(i))
           y4(i)=x(2,nc4(i))
           z4(i)=x(3,nc4(i))
           x5(i)=x(1,nc5(i))
           y5(i)=x(2,nc5(i))
           z5(i)=x(3,nc5(i))
           x6(i)=x(1,nc6(i))
           y6(i)=x(2,nc6(i))
           z6(i)=x(3,nc6(i))
         enddo
#else
         do i=1,nel
           x1(i)=xdp(1,nc1(i))
           y1(i)=xdp(2,nc1(i))
           z1(i)=xdp(3,nc1(i))
           x2(i)=xdp(1,nc2(i))
           y2(i)=xdp(2,nc2(i))
           z2(i)=xdp(3,nc2(i))
           x3(i)=xdp(1,nc3(i))
           y3(i)=xdp(2,nc3(i))
           z3(i)=xdp(3,nc3(i))
           x4(i)=xdp(1,nc4(i))
           y4(i)=xdp(2,nc4(i))
           z4(i)=xdp(3,nc4(i))
           x5(i)=xdp(1,nc5(i))
           y5(i)=xdp(2,nc5(i))
           z5(i)=xdp(3,nc5(i))
           x6(i)=xdp(1,nc6(i))
           y6(i)=xdp(2,nc6(i))
           z6(i)=xdp(3,nc6(i))
         enddo
#endif
       end if! (ismstr == 1.or.ismstr >= 11) 
         do i=1,nel
           vx1(i)=v(1,nc1(i))
           vy1(i)=v(2,nc1(i))
           vz1(i)=v(3,nc1(i))
           vx2(i)=v(1,nc2(i))
           vy2(i)=v(2,nc2(i))
           vz2(i)=v(3,nc2(i))
           vx3(i)=v(1,nc3(i))
           vy3(i)=v(2,nc3(i))
           vz3(i)=v(3,nc3(i))
           vx4(i)=v(1,nc4(i))
           vy4(i)=v(2,nc4(i))
           vz4(i)=v(3,nc4(i))
           vx5(i)=v(1,nc5(i))
           vy5(i)=v(2,nc5(i))
           vz5(i)=v(3,nc5(i))
           vx6(i)=v(1,nc6(i))
           vy6(i)=v(2,nc6(i))
           vz6(i)=v(3,nc6(i))
         enddo
!         
        end subroutine s6get_xv
!-------------------
      end module s6get_xv_mod
