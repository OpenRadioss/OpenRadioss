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
      !||    sfor_ns2s4_mod   ../engine/source/elements/solid/solide/sfor_ns2s4.F90
      !||--- called by ------------------------------------------------------
      !||    sfor_4n2s4       ../engine/source/elements/solid/solide/sfor_4n2s4.F90
      !||====================================================================
      module sfor_ns2s4_mod
      contains
! ======================================================================================================================
! \brief self-contact for node to quad (optimized)
! ======================================================================================================================
      !||====================================================================
      !||    sfor_ns2s4     ../engine/source/elements/solid/solide/sfor_ns2s4.F90
      !||--- called by ------------------------------------------------------
      !||    sfor_4n2s4     ../engine/source/elements/solid/solide/sfor_4n2s4.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod   ../common_source/modules/constant_mod.F
      !||====================================================================
         subroutine sfor_ns2s4(                                                 &
                              xi,      yi,     zi, itgsub,                      &
                             vxi,     vyi,    vzi,  for_i,                      &
                              x1,      x2,     x3,     x4,                      &
                              y1,      y2,     y3,     y4,                      &
                              z1,      z2,     z3,     z4,                      &
                             vx1,     vx2,    vx3,    vx4,                      &
                             vy1,     vy2,    vy3,    vy4,                      &
                             vz1,     vz2,    vz3,    vz4,                      &
                          for_t1,  for_t2, for_t3, for_t4,                      &
                            stif,   stif0,  ifctl,  ifc1 ,                      &
                          penmin,  penref,  fqmax,e_distor,                     &
                             dt1,   nel )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,          only : zero,one,em20,ep02,three
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
          integer, dimension(mvsiz), intent(in)                        :: ifc1      !< first criterion to check self-contact
          integer, dimension(mvsiz), intent(in)                        :: itgsub    !< sub tria value
          integer, intent (inout)                                      :: ifctl     !< number of self-contact pairs
          my_real, dimension(mvsiz), intent(in)                        :: stif0     !< initial nodal stiffness 
          my_real, dimension(mvsiz), intent(in)                        :: penmin    !< minimum penetration 
          my_real, dimension(mvsiz), intent(in)                        :: penref    !< reference penetration for quadratic stiffness 
          my_real, intent(in)                                          :: fqmax     !< quadratic stiffness limite of self-contact
          my_real, intent(in)                                          :: dt1       !< time step
          my_real, dimension(mvsiz), intent(inout)                     :: stif      !< nodal stiffness to be updated
          my_real, dimension(nel),   intent(inout)                     :: e_distor  ! distortion energy
          my_real, dimension(mvsiz), intent(in   )                     ::       &        
                              xi,      yi,     zi,                              &
                             vxi,     vyi,    vzi,                              &                               
                              x1,      x2,     x3,     x4,                      &
                              y1,      y2,     y3,     y4,                      &
                              z1,      z2,     z3,     z4,                      &
                             vx1,     vx2,    vx3,    vx4,                      &
                             vy1,     vy2,    vy3,    vy4,                      &
                             vz1,     vz2,    vz3,    vz4                           !< main quad segment nodal x,v array
          my_real, dimension(mvsiz,3), intent (inout) ::                        &
                           for_t1, for_t2, for_t3, for_t4                           !< main quad segment force array
          my_real, dimension(mvsiz,3), intent (inout) :: for_i                      !< 2nd node force array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,j,ifctl1
!                                                                    
      my_real :: rx,ry,rz,sx,sy,sz,nx(mvsiz),ny(mvsiz),nz(mvsiz),               &
                 pene(mvsiz),xa(mvsiz),ya(mvsiz),za(mvsiz),hj(mvsiz,4),         &
                 xb(mvsiz),yb(mvsiz),zb(mvsiz),fn(mvsiz),                       &
                 xc(mvsiz),yc(mvsiz),zc(mvsiz),                                 &
                 la(mvsiz),lb(mvsiz),lc(mvsiz),fkt,                             &
                 x42,y42, z42, x31, y31, z31,fx,fy,fz,                          &
                 sax,say,saz,sbx,sby,sbz,scx,scy,scz,                           &
                 trx,try,trz,tsx,tsy,tsz,ttx,tty,ttz,                           &
                 tr2,ts2,tt2,aaa,bbb,vr,vs,vt,nnx,nny,nnz,                      &
                 xab,xbc,xca,yab,ybc,yca,zab,zbc,zca,                           &
                 xia,  xib,  xic, yia,  yib,  yic,                              &
                 zia,  zib,  zic, norm,s2,fac,                                  &
                 f_q,f_c,kts,zerom,tx,ty,tz,pendr,lj,                           &
                 dx,dy,dz,dn,area,dmin
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! 4-------3
! |       |
! |       |
! 1-------2
         ifctl = 0
         do i=1,nel
           if(ifc1(i) == zero ) cycle
           select case (itgsub(i)) ! 1: 1-2-4, 2:2-3-1, 3:3-4-2, 4:4-1-3, 34:1-2-3, 14:2-3-4, 23:4-1-2, 12:3-4-1
               case (1)        !ns near 1
                 xb(i) = x1(i)   
                 yb(i) = y1(i)    
                 zb(i) = z1(i)   
                 xc(i) = x2(i)   
                 yc(i) = y2(i)    
                 zc(i) = z2(i)   
                 xa(i) = x4(i)   
                 ya(i) = y4(i)    
                 za(i) = z4(i) 
               case (2)        !ns near 2
                 xb(i) = x2(i)   
                 yb(i) = y2(i)    
                 zb(i) = z2(i)   
                 xc(i) = x3(i)   
                 yc(i) = y3(i)    
                 zc(i) = z3(i)   
                 xa(i) = x1(i)   
                 ya(i) = y1(i)    
                 za(i) = z1(i) 
               case (3)        !ns near 3
                 xb(i) = x3(i)   
                 yb(i) = y3(i)    
                 zb(i) = z3(i)   
                 xc(i) = x4(i)   
                 yc(i) = y4(i)    
                 zc(i) = z4(i)   
                 xa(i) = x2(i)   
                 ya(i) = y2(i)    
                 za(i) = z2(i) 
               case (4)        !ns near 4
                 xb(i) = x4(i)   
                 yb(i) = y4(i)    
                 zb(i) = z4(i)   
                 xc(i) = x1(i)   
                 yc(i) = y1(i)    
                 zc(i) = z1(i)   
                 xa(i) = x3(i)   
                 ya(i) = y3(i)    
                 za(i) = z3(i) 
               case (34)        !n3=n4
                 xb(i) = x1(i)   
                 yb(i) = y1(i)    
                 zb(i) = z1(i)   
                 xc(i) = x2(i)   
                 yc(i) = y2(i)    
                 zc(i) = z2(i)   
                 xa(i) = x3(i)   
                 ya(i) = y3(i)    
                 za(i) = z3(i) 
               case (14)         !n1=n4
                 xb(i) = x2(i)   
                 yb(i) = y2(i)    
                 zb(i) = z2(i)   
                 xc(i) = x3(i)   
                 yc(i) = y3(i)    
                 zc(i) = z3(i)   
                 xa(i) = x4(i)   
                 ya(i) = y4(i)    
                 za(i) = z4(i) 
               case (23)       !n2=n3
                 xb(i) = x4(i)   
                 yb(i) = y4(i)    
                 zb(i) = z4(i)   
                 xc(i) = x1(i)   
                 yc(i) = y1(i)    
                 zc(i) = z1(i)   
                 xa(i) = x2(i)   
                 ya(i) = y2(i)    
                 za(i) = z2(i) 
               case (12)       !n1=n2
                 xb(i) = x3(i)   
                 yb(i) = y3(i)    
                 zb(i) = z3(i)   
                 xc(i) = x4(i)   
                 yc(i) = y4(i)    
                 zc(i) = z4(i)   
                 xa(i) = x1(i)   
                 ya(i) = y1(i)    
                 za(i) = z1(i) 
             end select
         enddo
!
         pene(1:nel)= zero
         do i=1,nel
           if(ifc1(i) == zero ) cycle
           rx =xb(i)-xa(i)
           ry =yb(i)-ya(i)
           rz =zb(i)-za(i)
           sx =xc(i)-xa(i)
           sy =yc(i)-ya(i)
           sz =zc(i)-za(i)
           nx(i)=ry*sz - rz*sy
           ny(i)=rz*sx - rx*sz
           nz(i)=rx*sy - ry*sx
           area = sqrt(nx(i)*nx(i)+ny(i)*ny(i)+nz(i)*nz(i))
           norm=one/max(em20,area)
           nx(i)=nx(i)*norm
           ny(i)=ny(i)*norm
           nz(i)=nz(i)*norm
           bbb = (xb(i)-xi(i))*nx(i) +(yb(i)-yi(i))*ny(i) +                         &
                 (zb(i)-zi(i))*nz(i) -penmin(i)
           pene(i) = max(zero,-bbb)  
         enddo
         do i=1,nel
           if(pene(i) == zero) cycle
           xab = xb(i)-xa(i)
           yab = yb(i)-ya(i)
           zab = zb(i)-za(i)
           xbc = xc(i)-xb(i)
           ybc = yc(i)-yb(i)
           zbc = zc(i)-zb(i)
           xca = xa(i)-xc(i)
           yca = ya(i)-yc(i)
           zca = za(i)-zc(i)
           
           xia = xa(i)-xi(i)
           yia = ya(i)-yi(i)
           zia = za(i)-zi(i)
           xib = xb(i)-xi(i)
           yib = yb(i)-yi(i)
           zib = zb(i)-zi(i)
           xic = xc(i)-xi(i)
           yic = yc(i)-yi(i)
           zic = zc(i)-zi(i)
           sx = - yab*zca + zab*yca
           sy = - zab*xca + xab*zca
           sz = - xab*yca + yab*xca
           s2 = sx*sx+sy*sy+sz*sz
           sax = yib*zic - zib*yic
           say = zib*xic - xib*zic
           saz = xib*yic - yib*xic
           la(i) = (sx*sax+sy*say+sz*saz)/s2
           sbx = yic*zia - zic*yia
           sby = zic*xia - xic*zia
           sbz = xic*yia - yic*xia
           lb(i) = (sx*sbx+sy*sby+sz*sbz)/s2
           lc(i) = one - la(i) - lb(i)
           lj = min(la(i),lb(i),lc(i))
           if(la(i)<zero)then
             if(lb(i)<zero)then
               la(i) = zero
               lb(i) = zero
               lc(i) = one
             elseif(lc(i)<zero)then
               lc(i) = zero
               la(i) = zero
               lb(i) = one
             else
               la(i) = zero
               aaa = lb(i) + lc(i)
               lb(i) = lb(i)/aaa
               lc(i) = lc(i)/aaa
             endif
           elseif(lb(i)<zero)then
             if(lc(i)<zero)then
               lb(i) = zero
               lc(i) = zero
               la(i) = one
             else
               lb(i) = zero
               aaa = lc(i) + la(i)
               lc(i) = lc(i)/aaa
               la(i) = la(i)/aaa
             endif
           elseif(lc(i)<zero)then
               lc(i) = zero
               aaa = la(i) + lb(i)
               la(i) = la(i)/aaa
               lb(i) = lb(i)/aaa
           endif
         enddo
         do i=1,nel
           if(pene(i) == zero) cycle
           select case (itgsub(i)) ! 1: 1-2-4, 2:2-3-1, 3:3-4-2, 4:4-1-3, 34:1-2-3, 14:2-3-4, 23:4-1-2, 12:3-4-1
               case (1)        !ns near 1
                 hj(i,1) = lb(i)
                 hj(i,2) = lc(i) 
                 hj(i,4) = la(i)
                 hj(i,3) = zero 
               case (2)        !ns near 2
                 hj(i,2) = lb(i)
                 hj(i,3) = lc(i) 
                 hj(i,1) = la(i)
                 hj(i,4) = zero 
               case (3)        !ns near 3
                 hj(i,3) = lb(i)
                 hj(i,4) = lc(i) 
                 hj(i,1) = la(i)
                 hj(i,2) = zero 
               case (4)        !ns near 4
                 hj(i,4) = lb(i)
                 hj(i,1) = lc(i) 
                 hj(i,3) = la(i)
                 hj(i,2) = zero 
               case (34)        !n3=n4
                 hj(i,1) = lb(i)
                 hj(i,2) = lc(i) 
                 hj(i,3) = la(i)
                 hj(i,4) = zero 
               case (14)         !n1=n4
                 hj(i,2) = lb(i)
                 hj(i,3) = lc(i) 
                 hj(i,4) = la(i)
                 hj(i,1) = zero 
               case (23)       !n2=n3
                 hj(i,4) = lb(i)
                 hj(i,1) = lc(i) 
                 hj(i,2) = la(i)
                 hj(i,3) = zero 
               case (12)       !n1=n2
                 hj(i,3) = lb(i)
                 hj(i,4) = lc(i) 
                 hj(i,1) = la(i)
                 hj(i,2) = zero 
             end select
         enddo
         f_q = ep02
         do i=1,nel
           if(pene(i) == zero) cycle
           pendr  = (pene(i)/penref(i))**2
           fac = min(fqmax,f_q*pendr)
           fn(i) = (fac+one)*stif0(i)*pene(i)
           fkt = one+three*fac
           stif(i) =max(stif(i),fkt*stif0(i))
         enddo
         do i=1,nel
           if(pene(i) == zero) cycle
           dx = vxi(i) - hj(i,1)*vx1(i) - hj(i,2)*vx2(i)                  &
                       - hj(i,3)*vx3(i) - hj(i,4)*vx4(i)
           dy = vyi(i) - hj(i,1)*vy1(i) - hj(i,2)*vy2(i)                  &
                       - hj(i,3)*vy3(i) - hj(i,4)*vy4(i)
           dz = vzi(i) - hj(i,1)*vz1(i) - hj(i,2)*vz2(i)                  &
                       - hj(i,3)*vz3(i) - hj(i,4)*vz4(i)
           dn = (nx(i)*dx + ny(i)*dy + nz(i)*dz)*dt1
           e_distor(i) = e_distor(i) - fn(i)*dn
         enddo
        do i=1,nel
          if (pene(i) ==zero) cycle
          fx = nx(i)*fn(i)
          fy = ny(i)*fn(i) 
          fz = nz(i)*fn(i)
          for_i(i,1) = for_i(i,1) - fx
          for_i(i,2) = for_i(i,2) - fy
          for_i(i,3) = for_i(i,3) - fz
          for_t1(i,1) = for_t1(i,1) + fx*hj(i,1)
          for_t1(i,2) = for_t1(i,2) + fy*hj(i,1)
          for_t1(i,3) = for_t1(i,3) + fz*hj(i,1)
          for_t2(i,1) = for_t2(i,1) + fx*hj(i,2)
          for_t2(i,2) = for_t2(i,2) + fy*hj(i,2)
          for_t2(i,3) = for_t2(i,3) + fz*hj(i,2)
          for_t3(i,1) = for_t3(i,1) + fx*hj(i,3)
          for_t3(i,2) = for_t3(i,2) + fy*hj(i,3)
          for_t3(i,3) = for_t3(i,3) + fz*hj(i,3)
          for_t4(i,1) = for_t4(i,1) + fx*hj(i,4)
          for_t4(i,2) = for_t4(i,2) + fy*hj(i,4)
          for_t4(i,3) = for_t4(i,3) + fz*hj(i,4)
          ifctl =1
        enddo
       end subroutine sfor_ns2s4
!-------------------
      end module sfor_ns2s4_mod
 