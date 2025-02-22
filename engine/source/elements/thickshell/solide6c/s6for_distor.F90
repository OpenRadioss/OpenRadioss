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
      !||    s6for_distor_mod   ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
      !||--- called by ------------------------------------------------------
      !||    s6cforc3           ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||====================================================================
      module s6for_distor_mod
      contains
! ======================================================================================================================
! \brief distortion control for penta6 element
! ======================================================================================================================
      !||====================================================================
      !||    s6for_distor       ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
      !||--- called by ------------------------------------------------------
      !||    s6cforc3           ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||--- calls      -----------------------------------------------------
      !||    sfor_3n2s3         ../engine/source/elements/solid/solide/sfor_4n2s4.F90
      !||    sfor_n2s4          ../engine/source/elements/solid/solide/sfor_n2s4.F
      !||    sfor_n2stria       ../engine/source/elements/solid/solide4/sfor_n2stria.F
      !||    sfor_visn6         ../engine/source/elements/thickshell/solide6c/sfor_visn6.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    sfor_nsn2seg_mod   ../engine/source/elements/solid/solide/sfor_4n2s4.F90
      !||    sfor_visn6_mod     ../engine/source/elements/thickshell/solide6c/sfor_visn6.F90
      !||====================================================================
        subroutine s6for_distor(                                              &
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
                               f11,      f12,      f13,                       &     
                               f14,      f15,      f16,                       &     
                               f21,      f22,      f23,                       &     
                               f24,      f25,      f26,                       &     
                               f31,      f32,      f33,                       &     
                               f34,      f35,      f36,                       &     
                               sti,    sti_c,      fld,                       &    
                               mu ,      ll ,    istab,                       &
                             fqmax, e_distor,      dt1,                       &
                              nel )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,          only : zero,zep2,two,five,ten,em20,one_over_6,em02
      use sfor_visn6_mod,        only : sfor_visn6
      use sfor_nsn2seg_mod,      only : sfor_3n2s3
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
          integer, intent(in)                                          :: nel       !< number of elements
          integer, dimension(mvsiz), intent(in)                        :: istab     !< if already high stress (buckling)
          my_real, dimension(mvsiz), intent(in)                        :: sti_c     !< initial nodal stiffness 
          my_real, intent(in)                                          :: mu        !< damping coefficient
          my_real, intent(in)                                          :: fqmax     !< quadratic stiffness limite of self-contact
          my_real, intent(in)                                          :: dt1       !< time step
          my_real, dimension(mvsiz), intent(in)                        :: fld       !< damping undimensional array
          my_real, dimension(mvsiz), intent(in)                        :: ll        !< characteristic length
          my_real, dimension(mvsiz), intent(inout)                     :: sti       !< nodal stiffness to be updated
          my_real, dimension(nel),   intent(inout)                     :: e_distor  ! distortion energy
          my_real, dimension(mvsiz), intent(in   )                     ::        &        
                                       x1,x2,x3,x4,x5,x6,                        &        
                                       y1,y2,y3,y4,y5,y6,                        &                              
                                       z1,z2,z3,z4,z5,z6                            !< nodal coordinate array
          my_real, dimension(mvsiz), intent(in   )                     ::        &        
                                 vx1,vx2,vx3,vx4,vx5,vx6,                        &
                                 vy1,vy2,vy3,vy4,vy5,vy6,                        &
                                 vz1,vz2,vz3,vz4,vz5,vz6                            !< nodal coordinate array
          my_real, dimension(mvsiz), intent(inout)                     ::        &        
                                 f11,      f12,      f13,                        &  
                                 f14,      f15,      f16,                        &     
                                 f21,      f22,      f23,                        &     
                                 f24,      f25,      f26,                        &     
                                 f31,      f32,      f33,                        &     
                                 f34,      f35,      f36                            !< nodal internal force
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real :: xc(mvsiz),yc(mvsiz),zc(mvsiz),stif(mvsiz),                      &
                 vc(mvsiz,3),forc_n(mvsiz,3),for_t1(mvsiz,3),                    &
                 for_t2(mvsiz,3),for_t3(mvsiz,3),for_t4(mvsiz,3),                &
                 for_t5(mvsiz,3),for_t6(mvsiz,3),                                &
                 fcx,fcy,fcz,fac,gap_max,gap_min,                                &
                 penmin(mvsiz),penref(mvsiz),marge(mvsiz),                       &
                 tol_t,tol_c,tol_v
      integer i,j,nctl,ifctl,ifc1(mvsiz)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
         tol_c= zep2
         tol_v = ten
         do i=1,nel
           vc(i,1) = one_over_6*(vx1(i)+vx2(i)+vx3(i)+vx4(i)+                    &
                                               vx5(i)+vx6(i))                    
           vc(i,2) = one_over_6*(vy1(i)+vy2(i)+vy3(i)+vy4(i)+                    &
                                               vy5(i)+vy6(i))                    
           vc(i,3) = one_over_6*(vz1(i)+vz2(i)+vz3(i)+vz4(i)+                    &
                                               vz5(i)+vz6(i))
           stif(i) = sti_c(i)
           ifc1(i) = istab(i)
          enddo
!         
        nctl = 0
        forc_n = zero
        for_t1 = zero
        for_t2 = zero
        for_t3 = zero
        for_t4 = zero
        for_t5 = zero
        for_t6 = zero
       call sfor_visn6(                                                         &
                  vc,      fld,    tol_v,                                       &      
                 vx1,      vx2,       x3,                                       &      
                 vx4,      vx5,       x6,                                       &      
                 vy1,      vy2,       y3,                                       &      
                 vy4,      vy5,       y6,                                       &      
                 vz1,      vz2,       z3,                                       &      
                 vz4,      vz5,       z6,                                       &      
              for_t1,   for_t2,   for_t3,                                       &      
              for_t4,   for_t5,   for_t6,                                       &      
                 sti,     ifc1,       mu,                                       &
               ifctl ,e_distor,      dt1,                                       &
                nel )
       if (ifctl >0) then
         nctl = nctl + ifctl
!---- element center
         do i=1,nel
           xc(i) = one_over_6*(x1(i)+x2(i)+x3(i)+x4(i)+x5(i)+x6(i))
           yc(i) = one_over_6*(y1(i)+y2(i)+y3(i)+y4(i)+y5(i)+y6(i))
           zc(i) = one_over_6*(z1(i)+z2(i)+z3(i)+z4(i)+z5(i)+z6(i))
         enddo
!    ifc1 is used for contact        
         gap_min = tol_c*em02  !percentage
         gap_max = five*gap_min
         penmin(1:nel) = gap_min*ll(1:nel)
         penref(1:nel) = gap_max*ll(1:nel)
         marge(1:nel) = two*gap_max*ll(1:nel)
!---- seg 1 : 1,3,2 (normal should be towards outside)
         call sfor_n2stria(xc,      yc,     zc,                                &
                           x1,      x3,     x2,                                &
                           y1,      y3,     y2,                                &
                           z1,      z3,     z2,                                &
                       vc(1,1), vc(1,2),vc(1,3),                               &
                          vx1,     vx3,    vx2,                                &
                          vy1,     vy3,    vy2,                                &
                          vz1,     vz3,    vz2,                                &
                       for_t1,  for_t3, for_t2,                                &
                        forc_n,  sti_c,   stif,                                &
                        fqmax , penmin, penref,                                &
                          ll  ,  ifctl,   nel ,                                &
                        e_distor, dt1 )                                        
         nctl = nctl + ifctl                                                   
!---- seg 2 : 4,5,6 (normal will be towards outside)                         
         call sfor_n2stria(xc,      yc,     zc,                                &
                           x4,      x5,     x6,                                &
                           y4,      y5,     y6,                                &
                           z4,      z5,     z6,                                &
                      vc(1,1), vc(1,2),vc(1,3),                                &
                          vx4,     vx5,    vx6,                                &
                          vy4,     vy5,    vy6,                                &
                          vz4,     vz5,    vz6,                                &
                        for_t4, for_t5, for_t6,                                &
                        forc_n,  sti_c,   stif,                                &
                        fqmax , penmin, penref,                                &
                          ll  ,  ifctl,   nel ,                                &
                        e_distor, dt1 )
         nctl = nctl + ifctl
!---- seg 3 : 1,2,5,4 (normal will be towards outside)
         call sfor_n2s4(  xc,      yc,     zc,   stif,                         & 
                          x1,      x2,     x5,     x4,                         &
                          y1,      y2,     y5,     y4,                         &
                          z1,      z2,     z5,     z4,                         &
                         vx1,     vx2,    vx5,    vx4,                         &
                         vy1,     vy2,    vy5,    vy4,                         &
                         vz1,     vz2,    vz5,    vz4,                         &
                       for_t1, for_t2, for_t5, for_t4,                         &
                       forc_n,    ll ,  ifctl,  ifc1 ,                         &
                       penmin, penref,  marge,  fqmax,                         &
                        sti_c,   nel ,    vc ,e_distor,                        &
                          dt1)                                                 
         nctl = nctl + ifctl                                                   
!---- seg 4 : 2,3,6,5                                                          
         call sfor_n2s4(   xc,      yc,     zc,   stif,                        &  
                           x2,      x3,     x6,     x5,                        &
                           y2,      y3,     y6,     y5,                        &
                           z2,      z3,     z6,     z5,                        &
                          vx2,     vx3,    vx6,    vx5,                        &
                          vy2,     vy3,    vy6,    vy5,                        &
                          vz2,     vz3,    vz6,    vz5,                        &
                       for_t2,  for_t3, for_t6, for_t5,                        &
                       forc_n,     ll ,  ifctl,  ifc1 ,                        &
                       penmin,  penref,  marge,  fqmax,                        &
                        sti_c,    nel ,    vc ,e_distor,                       &
                          dt1)
         nctl = nctl + ifctl
!---- seg 5 : 4,6,3,1
         call sfor_n2s4(   xc,      yc,     zc,    stif,                       &   
                           x4,      x6,     x3,      x1,                       &
                           y4,      y6,     y3,      y1,                       &
                           z4,      z6,     z3,      z1,                       &
                          vx4,     vx6,    vx3,     vx1,                       &
                          vy4,     vy6,    vy3,     vy1,                       &
                          vz4,     vz6,    vz3,     vz1,                       &
                       for_t4,  for_t6, for_t3,  for_t1,                       &
                       forc_n,     ll ,  ifctl,   ifc1 ,                       &
                       penmin,  penref,  marge,   fqmax,                       &
                        sti_c,    nel ,    vc ,e_distor,                       &
                          dt1)
         nctl = nctl + ifctl
!----add : 4,6,5 as 2nd to seg 1,3,2 
              call sfor_3n2s3(                                                 &
                             x4,     x6,    x5,                                &
                             y4,     y6,    y5,                                &
                             z4,     z6,    z5,                                &
                            vx4,    vx6,   vx5,                                &
                            vy4,    vy6,   vy5,                                &
                            vz4,    vz6,   vz5,                                &
                         for_t4, for_t6,for_t5,                                &
                             x1,     x3,    x2,                                &
                             y1,     y3,    y2,                                &
                             z1,     z3,    z2,                                &
                            vx1,    vx3,   vx2,                                &
                            vy1,    vy3,   vy2,                                &
                            vz1,    vz3,   vz2,                                &
                         for_t1, for_t3,for_t2,                                &
                           stif,    ll , fqmax,                                &
                         penmin, penref, marge,                                &
                          sti_c,e_distor,  dt1,                                &
                          nctl ,   ifc1 , nel )
!---- force assemblage and sti update (dt)
          do i=1,nel
             fcx = one_over_6*forc_n(i,1)
             fcy = one_over_6*forc_n(i,2)
             fcz = one_over_6*forc_n(i,3)
             f11(i)=f11(i) + for_t1(i,1) + fcx
             f21(i)=f21(i) + for_t1(i,2) + fcy
             f31(i)=f31(i) + for_t1(i,3) + fcz
             f12(i)=f12(i) + for_t2(i,1) + fcx
             f22(i)=f22(i) + for_t2(i,2) + fcy
             f32(i)=f32(i) + for_t2(i,3) + fcz
             f13(i)=f13(i) + for_t3(i,1) + fcx
             f23(i)=f23(i) + for_t3(i,2) + fcy
             f33(i)=f33(i) + for_t3(i,3) + fcz
             f14(i)=f14(i) + for_t4(i,1) + fcx
             f24(i)=f24(i) + for_t4(i,2) + fcy
             f34(i)=f34(i) + for_t4(i,3) + fcz
             f15(i)=f15(i) + for_t5(i,1) + fcx
             f25(i)=f25(i) + for_t5(i,2) + fcy
             f35(i)=f35(i) + for_t5(i,3) + fcz
             f16(i)=f16(i) + for_t6(i,1) + fcx
             f26(i)=f26(i) + for_t6(i,2) + fcy
             f36(i)=f36(i) + for_t6(i,3) + fcz
!             
             sti(i) = max(sti(i),stif(i))  ! elem dt will be looked later
          end do 
        endif !(ifctl >0) then
      
!         
        end subroutine s6for_distor
!-------------------
      end module s6for_distor_mod
