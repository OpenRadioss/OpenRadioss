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
!||    sfor_visn6_mod   ../engine/source/elements/thickshell/solide6c/sfor_visn6.F90
!||--- called by ------------------------------------------------------
!||    s6for_distor     ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||====================================================================
      module sfor_visn6_mod
      contains
! ======================================================================================================================
! \brief damping force calculation of distortion control for penta6 elements
! ======================================================================================================================
!||====================================================================
!||    sfor_visn6      ../engine/source/elements/thickshell/solide6c/sfor_visn6.F90
!||--- called by ------------------------------------------------------
!||    s6for_distor    ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine sfor_visn6(                                                 &
                                 vc,      fld,    tol_v,                       &      
                                vx1,      vx2,      vx3,                       &      
                                vx4,      vx5,      vx6,                       &      
                                vy1,      vy2,      vy3,                       &      
                                vy4,      vy5,      vy6,                       &      
                                vz1,      vz2,      vz3,                       &      
                                vz4,      vz5,      vz6,                       &      
                             for_t1,   for_t2,   for_t3,                       &      
                             for_t4,   for_t5,   for_t6,                       &      
                                sti,     ifc1,       mu,                       &
                              ifctl ,e_distor,      dt1,                       &
                               nel )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,          only : zero,one,two,em20,four
      use precision_mod, only : WP
      use mvsiz_mod, only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer, intent (in)                                  :: nel           !< number of elements
      integer, intent (out)                                 :: ifctl         !< if at least one element reached criterion 
      integer, dimension(mvsiz),intent (inout)              :: ifc1          !< if element reached velocity criterion
      real(kind=WP), dimension(mvsiz), intent (in)                :: fld           !< undimensional damping array
      real(kind=WP), dimension(mvsiz), intent (inout)             :: sti           !< nodal stifness
      real(kind=WP), dimension(mvsiz,3), intent (in)              :: vc            !< average velocity
      real(kind=WP), intent (in)                                  :: tol_v         !< tolerance
      real(kind=WP), intent (in)                                  :: mu            !< damping coefficient
      real(kind=WP), intent (in)                                  :: dt1           !< time step
      real(kind=WP), dimension(nel),   intent(inout)              :: e_distor      ! distortion energy
      real(kind=WP), dimension(mvsiz), intent (in)                ::             &       
                            vx1,      vx2,      vx3,                       &    
                            vx4,      vx5,      vx6,                       &          
                            vy1,      vy2,      vy3,                       &          
                            vy4,      vy5,      vy6,                       &          
                            vz1,      vz2,      vz3,                       &          
                            vz4,      vz5,      vz6                          !< nodal velocity     
      real(kind=WP), dimension(mvsiz,3), intent (inout)           ::             &       
                             for_t1, for_t2, for_t3,                       &  
                             for_t4, for_t5, for_t6                          !< nodal internal force (viscous)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i,j,IFCONT
!                                                                   
      real(kind=WP) :: fx,fy,fz,fac,vnj(6),vl,tol_v2,v2max,vc2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
        tol_v2 = tol_v*tol_v
        ifctl = 0
        ifcont = 1
         do i=1,nel
           vc2 = vc(i,1)*vc(i,1)+vc(i,2)*vc(i,2)+vc(i,3)*vc(i,3)
           if (vc2 <em20.or.sti(i)==zero) cycle
           vl = tol_v2*vc2
           vnj(1) = vx1(i)*vx1(i) + vy1(i)*vy1(i) + vz1(i)*vz1(i)
           vnj(2) = vx2(i)*vx2(i) + vy2(i)*vy2(i) + vz2(i)*vz2(i)
           vnj(3) = vx3(i)*vx3(i) + vy3(i)*vy3(i) + vz3(i)*vz3(i)
           vnj(4) = vx4(i)*vx4(i) + vy4(i)*vy4(i) + vz4(i)*vz4(i)
           vnj(5) = vx5(i)*vx5(i) + vy5(i)*vy5(i) + vz5(i)*vz5(i)
           vnj(6) = vx6(i)*vx6(i) + vy6(i)*vy6(i) + vz6(i)*vz6(i)
           v2max = max(vnj(1),vnj(2),vnj(3),vnj(4),vnj(5),vnj(6))
           if (v2max > vl) ifc1(i) = 1
           if (ifc1(i) > 0)   ifctl=1
           if (v2max > four*vc2) ifcont=1
         end do
!
       if (ifctl==1) then
         fac = one + two*mu
         do i=1,nel
           if (ifc1(i)==0) cycle
           for_t1(i,1) = for_t1(i,1) - fld(i)*(vx1(i)-vc(i,1))
           for_t1(i,2) = for_t1(i,2) - fld(i)*(vy1(i)-vc(i,2))
           for_t1(i,3) = for_t1(i,3) - fld(i)*(vz1(i)-vc(i,3))
           for_t2(i,1) = for_t2(i,1) - fld(i)*(vx2(i)-vc(i,1))
           for_t2(i,2) = for_t2(i,2) - fld(i)*(vy2(i)-vc(i,2))
           for_t2(i,3) = for_t2(i,3) - fld(i)*(vz2(i)-vc(i,3))
           for_t3(i,1) = for_t3(i,1) - fld(i)*(vx3(i)-vc(i,1))
           for_t3(i,2) = for_t3(i,2) - fld(i)*(vy3(i)-vc(i,2))
           for_t3(i,3) = for_t3(i,3) - fld(i)*(vz3(i)-vc(i,3))
           for_t4(i,1) = for_t4(i,1) - fld(i)*(vx4(i)-vc(i,1))
           for_t4(i,2) = for_t4(i,2) - fld(i)*(vy4(i)-vc(i,2))
           for_t4(i,3) = for_t4(i,3) - fld(i)*(vz4(i)-vc(i,3))
           for_t5(i,1) = for_t5(i,1) - fld(i)*(vx5(i)-vc(i,1))
           for_t5(i,2) = for_t5(i,2) - fld(i)*(vy5(i)-vc(i,2))
           for_t5(i,3) = for_t5(i,3) - fld(i)*(vz5(i)-vc(i,3))
           for_t6(i,1) = for_t6(i,1) - fld(i)*(vx6(i)-vc(i,1))
           for_t6(i,2) = for_t6(i,2) - fld(i)*(vy6(i)-vc(i,2))
           for_t6(i,3) = for_t6(i,3) - fld(i)*(vz6(i)-vc(i,3))
           sti(i)      = fac*sti(i)
           e_distor(i)=e_distor(i) -dt1*(for_t1(i,1)*(vx1(i)-vc(i,1))+     &
                                         for_t1(i,2)*(vy1(i)-vc(i,2))+     &
                                         for_t1(i,3)*(vz1(i)-vc(i,3))+     &
                                         for_t2(i,1)*(vx2(i)-vc(i,1))+     &
                                         for_t2(i,2)*(vy2(i)-vc(i,2))+     &
                                         for_t2(i,3)*(vz2(i)-vc(i,3))+     &
                                         for_t3(i,1)*(vx3(i)-vc(i,1))+     &
                                         for_t3(i,2)*(vy3(i)-vc(i,2))+     &
                                         for_t3(i,3)*(vz3(i)-vc(i,3))+     &
                                         for_t4(i,1)*(vx4(i)-vc(i,1))+     &
                                         for_t4(i,2)*(vy4(i)-vc(i,2))+     &
                                         for_t4(i,3)*(vz4(i)-vc(i,3))+     &
                                         for_t5(i,1)*(vx5(i)-vc(i,1))+     &
                                         for_t5(i,2)*(vy5(i)-vc(i,2))+     &
                                         for_t5(i,3)*(vz5(i)-vc(i,3))+     &
                                         for_t6(i,1)*(vx6(i)-vc(i,1))+     &
                                         for_t6(i,2)*(vy6(i)-vc(i,2))+     &
                                         for_t6(i,3)*(vz6(i)-vc(i,3)))
         enddo
       end if 
        ifctl = ifcont  ! used for self-contact compute less strict : 2 times
!         
        end subroutine sfor_visn6
!-------------------
      end module sfor_visn6_mod
