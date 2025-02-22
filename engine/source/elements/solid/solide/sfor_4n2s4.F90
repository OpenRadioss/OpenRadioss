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
      !||    sfor_nsn2seg_mod   ../engine/source/elements/solid/solide/sfor_4n2s4.F90
      !||--- called by ------------------------------------------------------
      !||    s6for_distor       ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
      !||    s8for_distor       ../engine/source/elements/solid/solide/s8for_distor.F
      !||====================================================================
      module sfor_nsn2seg_mod
      contains
! ======================================================================================================================
! \brief self-contact for 4-node to quad 
! ======================================================================================================================
      !||====================================================================
      !||    sfor_4n2s4       ../engine/source/elements/solid/solide/sfor_4n2s4.F90
      !||--- called by ------------------------------------------------------
      !||    s8for_distor     ../engine/source/elements/solid/solide/s8for_distor.F
      !||--- calls      -----------------------------------------------------
      !||    sfor_ns2s4       ../engine/source/elements/solid/solide/sfor_ns2s4.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod     ../common_source/modules/constant_mod.F
      !||    sfor_ns2s4_mod   ../engine/source/elements/solid/solide/sfor_ns2s4.F90
      !||====================================================================
         subroutine sfor_4n2s4(                                                 &
                             xn1,     xn2,    xn3,    xn4,                      &
                             yn1,     yn2,    yn3,    yn4,                      &
                             zn1,     zn2,    zn3,    zn4,                      &
                            vnx1,    vnx2,   vnx3,   vnx4,                      &
                            vny1,    vny2,   vny3,   vny4,                      &
                            vnz1,    vnz2,   vnz3,   vnz4,                      &
                          for_n1,  for_n2, for_n3, for_n4,                      &
                              x1,      x2,     x3,     x4,                      &
                              y1,      y2,     y3,     y4,                      &
                              z1,      z2,     z3,     z4,                      &
                             vx1,     vx2,    vx3,    vx4,                      &
                             vy1,     vy2,    vy3,    vy4,                      &
                             vz1,     vz2,    vz3,    vz4,                      &
                          for_t1,  for_t2, for_t3, for_t4,                      &
                            stif,     ll ,  nctl,  ifc1 ,                       &
                           penmin, penref,  marge,  fqmax,                      &
                           stif0,e_distor,    dt1,   nel )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,          only : zero,one,em20
      use sfor_ns2s4_mod,        only : sfor_ns2s4
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
          integer, intent (inout)                                      :: nctl      !< number of self-contact pairs
          my_real, dimension(mvsiz), intent(in)                        :: stif0     !< initial nodal stiffness 
          my_real, dimension(mvsiz), intent(in)                        :: penmin    !< minimum penetration 
          my_real, dimension(mvsiz), intent(in)                        :: penref    !< reference penetration for quadratic stiffness 
          my_real, dimension(mvsiz), intent(in)                        :: marge     !< sorting marge 
          my_real, intent(in)                                          :: fqmax     !< quadratic stiffness limite of self-contact
          my_real, intent(in)                                          :: dt1       !< time step
          my_real, dimension(mvsiz), intent(in)                        :: ll        !< characteristic length
          my_real, dimension(mvsiz), intent(inout)                     :: stif      !< nodal stiffness to be updated
          my_real, dimension(nel),   intent(inout)                     :: e_distor  ! distortion energy
          my_real, dimension(mvsiz), intent(in   )                     ::       &        
                             xn1,     xn2,    xn3,    xn4,                      &
                             yn1,     yn2,    yn3,    yn4,                      &
                             zn1,     zn2,    zn3,    zn4,                      &
                            vnx1,    vnx2,   vnx3,   vnx4,                      &
                            vny1,    vny2,   vny3,   vny4,                      &
                            vnz1,    vnz2,   vnz3,   vnz4                            !< 2nd nodal x,v array
          my_real, dimension(mvsiz,3), intent (inout) ::                        &
                           for_n1, for_n2, for_n3, for_n4                            !< 2nd nodal force array
          my_real, dimension(mvsiz), intent(in   )                     ::       &        
                              x1,      x2,     x3,     x4,                      &
                              y1,      y2,     y3,     y4,                      &
                              z1,      z2,     z3,     z4,                      &
                             vx1,     vx2,    vx3,    vx4,                      &
                             vy1,     vy2,    vy3,    vy4,                      &
                             vz1,     vz2,    vz3,    vz4                           !< main quad segment nodal x,v array
          my_real, dimension(mvsiz,3), intent (inout) ::                        &
                           for_t1, for_t2, for_t3, for_t4                           !< main quad segment force array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,j,ifctl,ifctl1,ifc2(mvsiz),ifde_s(mvsiz),itgsub(mvsiz)
!                                                                    
      my_real :: rx,ry,rz,sx,sy,sz,nx(mvsiz),ny(mvsiz),nz(mvsiz),dx,dy,dz,dn,norm,area,dmin
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! if degenerated for seg nodes,line case : -1;
         itgsub(1:nel) = 0
! 4-------3
! |       |
! |       |
! 1-------2
         do i=1,nel
           if (ifc1(i)==0.or.ll(i)<em20) then 
             itgsub(i)=-1
             cycle
           end if
           dx =x4(i)-x3(i)
           dy =y4(i)-y3(i)
           dz =z4(i)-z3(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) then
             itgsub(i)=34
             dx =x2(i)-x1(i)
             dy =y2(i)-y1(i)
             dz =z2(i)-z1(i)
             dmin = abs(dx)+abs(dy)+abs(dz)
             if (dmin==zero) itgsub(i)=-1
             cycle
           end if
           dx =x2(i)-x1(i)
           dy =y2(i)-y1(i)
           dz =z2(i)-z1(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) then
             itgsub(i)=12
             cycle
           end if
           dx =x4(i)-x1(i)
           dy =y4(i)-y1(i)
           dz =z4(i)-z1(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) then
             itgsub(i)=14
             dx =x3(i)-x2(i)
             dy =y3(i)-y2(i)
             dz =z3(i)-z2(i)
             dmin = abs(dx)+abs(dy)+abs(dz)
             if (dmin==zero) itgsub(i)=-1
             cycle
           end if
           dx =x3(i)-x2(i)
           dy =y3(i)-y2(i)
           dz =z3(i)-z2(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) itgsub(i)=23
         enddo
!------general info of seg, and sorting by projected distance
        do i=1,nel
           if (itgsub(i)==-1) cycle
           rx =x2(i)+x3(i)-x1(i)-x4(i)
           ry =y2(i)+y3(i)-y1(i)-y4(i)
           rz =z2(i)+z3(i)-z1(i)-z4(i)
           sx =x3(i)+x4(i)-x1(i)-x2(i)
           sy =y3(i)+y4(i)-y1(i)-y2(i)
           sz =z3(i)+z4(i)-z1(i)-z2(i)
           nx(i)=ry*sz - rz*sy
           ny(i)=rz*sx - rx*sz
           nz(i)=rx*sy - ry*sx
           area = sqrt(nx(i)*nx(i)+ny(i)*ny(i)+nz(i)*nz(i))
           norm=one/max(em20,area)
           nx(i)=nx(i)*norm
           ny(i)=ny(i)*norm
           nz(i)=nz(i)*norm
        enddo
! if degenerated for 2nd nodes,not possible have 1=3 or 2=4
        ifde_s(1:nel) = 0
        do i=1,nel
           if (itgsub(i)==-1) cycle
           dx =xn4(i)-xn3(i)
           dy =yn4(i)-yn3(i)
           dz =zn4(i)-zn3(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) then 
              ifde_s(i)=4
              dx =xn2(i)-xn1(i)
              dy =yn2(i)-yn1(i)
              dz =zn2(i)-zn1(i)
              dmin = abs(dx)+abs(dy)+abs(dz)
              if (dmin==zero) ifde_s(i)=-2
              cycle
           end if
           dx =xn2(i)-xn1(i)
           dy =yn2(i)-yn1(i)
           dz =zn2(i)-zn1(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) then
             ifde_s(i)=2
             cycle
           end if
           dx =xn4(i)-xn1(i)
           dy =yn4(i)-yn1(i)
           dz =zn4(i)-zn1(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) then
             ifde_s(i)=4
             dx =xn3(i)-xn2(i)
             dy =yn3(i)-yn2(i)
             dz =zn3(i)-zn2(i)
             dmin = abs(dx)+abs(dy)+abs(dz)
             if (dmin==zero) ifde_s(i)=-3
             cycle
           end if
           dx =xn3(i)-xn2(i)
           dy =yn3(i)-yn2(i)
           dz =zn3(i)-zn2(i)
           dmin = abs(dx)+abs(dy)+abs(dz)
           if (dmin==zero) ifde_s(i)=3
        enddo
!----ns=n1
        ifc2(1:nel) = 0
        ifctl1 = 0
        do i=1,nel
          if (itgsub(i)==-1) cycle
          dx = xn1(i)-x1(i)
          dy = yn1(i)-y1(i)
          dz = zn1(i)-z1(i)
          dmin = abs(dx)+abs(dy)+abs(dz)
          if (dmin==zero) cycle
          dn = abs(nx(i)*dx+ny(i)*dy+nz(i)*dz)
          if (dn<marge(i).and.stif0(i)>zero) then 
            ifc2(i)=1
            if (itgsub(i)==0) itgsub(i)=1
            ifctl1=1
          endif
        enddo
       if (ifctl1==1) then
         call sfor_ns2s4(                                                 &
                       xn1,     yn1,    zn1, itgsub,                      &
                      vnx1,    vny1,   vnz1, for_n1,                      &
                        x1,      x2,     x3,     x4,                      &
                        y1,      y2,     y3,     y4,                      &
                        z1,      z2,     z3,     z4,                      &
                       vx1,     vx2,    vx3,    vx4,                      &
                       vy1,     vy2,    vy3,    vy4,                      &
                       vz1,     vz2,    vz3,    vz4,                      &
                    for_t1,  for_t2, for_t3, for_t4,                      &
                      stif,   stif0,  ifctl,  ifc2 ,                      &
                    penmin,  penref,  fqmax,e_distor,                     &
                       dt1,   nel )
         nctl = nctl + ifctl                                                   
         do i=1,nel
          if (itgsub(i)==1) itgsub(i)=0
         enddo
       end if ! (ifctl1==1) then
!----ns=n2
        ifc2(1:nel) = 0
        ifctl1 = 0
        do i=1,nel
          if (itgsub(i)==-1.or.iabs(ifde_s(i))==2) cycle
          dx = xn2(i)-xn1(i)
          dy = yn2(i)-yn1(i)
          dz = zn2(i)-zn1(i)
          dmin = abs(dx)+abs(dy)+abs(dz)
          if (dmin==zero) cycle
          dx = xn2(i)-x2(i)
          dy = yn2(i)-y2(i)
          dz = zn2(i)-z2(i)
          dmin = abs(dx)+abs(dy)+abs(dz)
          if (dmin==zero) cycle
          dn = abs(nx(i)*dx+ny(i)*dy+nz(i)*dz)
          if (dn<marge(i).and.stif0(i)>zero) then 
            ifc2(i)=1
            if (itgsub(i)==0) itgsub(i)=2
            ifctl1=1
          endif
        enddo
       if (ifctl1==1) then
         call sfor_ns2s4(                                                 &
                       xn2,     yn2,    zn2, itgsub,                      &
                      vnx2,    vny2,   vnz2, for_n2,                      &
                        x1,      x2,     x3,     x4,                      &
                        y1,      y2,     y3,     y4,                      &
                        z1,      z2,     z3,     z4,                      &
                       vx1,     vx2,    vx3,    vx4,                      &
                       vy1,     vy2,    vy3,    vy4,                      &
                       vz1,     vz2,    vz3,    vz4,                      &
                    for_t1,  for_t2, for_t3, for_t4,                      &
                      stif,   stif0,  ifctl,  ifc2 ,                      &
                    penmin,  penref,  fqmax,e_distor,                     &
                       dt1,   nel )
         nctl = nctl + ifctl                                                   
         do i=1,nel
          if (itgsub(i)==2) itgsub(i)=0
         enddo
       end if ! (ifctl1==1) then
!----ns=n3
        ifc2(1:nel) = 0
        ifctl1 = 0
        do i=1,nel
          if (itgsub(i)==-1.or.iabs(ifde_s(i))==3) cycle
          dx = xn3(i)-x3(i)
          dy = yn3(i)-y3(i)
          dz = zn3(i)-z3(i)
          dmin = abs(dx)+abs(dy)+abs(dz)
          if (dmin==zero) cycle
          dn = abs(nx(i)*dx+ny(i)*dy+nz(i)*dz)
          if (dn<marge(i).and.stif0(i)>zero) then 
            ifc2(i)=1
            if (itgsub(i)==0) itgsub(i)=3
            ifctl1=1
          endif
        enddo
       if (ifctl1==1) then
         call sfor_ns2s4(                                                 &
                       xn3,     yn3,    zn3, itgsub,                      &
                      vnx3,    vny3,   vnz3, for_n3,                      &
                        x1,      x2,     x3,     x4,                      &
                        y1,      y2,     y3,     y4,                      &
                        z1,      z2,     z3,     z4,                      &
                       vx1,     vx2,    vx3,    vx4,                      &
                       vy1,     vy2,    vy3,    vy4,                      &
                       vz1,     vz2,    vz3,    vz4,                      &
                    for_t1,  for_t2, for_t3, for_t4,                      &
                      stif,   stif0,  ifctl,  ifc2 ,                      &
                    penmin,  penref,  fqmax,e_distor,                     &
                       dt1,   nel )
         nctl = nctl + ifctl                                                   
         do i=1,nel
          if (itgsub(i)==3) itgsub(i)=0
         enddo
       end if ! (ifctl1==1) then
!----ns=n4
        ifc2(1:nel) = 0
        ifctl1 = 0
        do i=1,nel
          if (itgsub(i)==-1.or.iabs(ifde_s(i))==4) cycle
          dx = xn4(i)-x4(i)
          dy = yn4(i)-y4(i)
          dz = zn4(i)-z4(i)
          dmin = abs(dx)+abs(dy)+abs(dz)
          if (dmin==zero) cycle
          dn = abs(nx(i)*dx+ny(i)*dy+nz(i)*dz)
          if (dn<marge(i).and.stif0(i)>zero) then 
            ifc2(i)=1
            if (itgsub(i)==0) itgsub(i)=4
            ifctl1=1
          endif
        enddo
       if (ifctl1==1) then
         call sfor_ns2s4(                                                 &
                       xn4,     yn4,    zn4, itgsub,                      &
                      vnx4,    vny4,   vnz4, for_n4,                      &
                        x1,      x2,     x3,     x4,                      &
                        y1,      y2,     y3,     y4,                      &
                        z1,      z2,     z3,     z4,                      &
                       vx1,     vx2,    vx3,    vx4,                      &
                       vy1,     vy2,    vy3,    vy4,                      &
                       vz1,     vz2,    vz3,    vz4,                      &
                    for_t1,  for_t2, for_t3, for_t4,                      &
                      stif,   stif0,  ifctl,  ifc2 ,                      &
                    penmin,  penref,  fqmax,e_distor,                     &
                       dt1,   nel )
         nctl = nctl + ifctl                                                   
       end if ! (ifctl1==1) then
!
!-----------
        end subroutine sfor_4n2s4
! ======================================================================================================================
! \brief self-contact for 3-node to tria 
! ======================================================================================================================
      !||====================================================================
      !||    sfor_3n2s3     ../engine/source/elements/solid/solide/sfor_4n2s4.F90
      !||--- called by ------------------------------------------------------
      !||    s6for_distor   ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
      !||--- calls      -----------------------------------------------------
      !||    sfor_n2s3      ../engine/source/elements/solid/solide4/sfor_n2s3.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod   ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine sfor_3n2s3(                                                  &
                             xn1,     xn2,    xn3,                              &
                             yn1,     yn2,    yn3,                              &
                             zn1,     zn2,    zn3,                              &
                            vnx1,    vnx2,   vnx3,                              &
                            vny1,    vny2,   vny3,                              &
                            vnz1,    vnz2,   vnz3,                              &
                          for_n1,  for_n2, for_n3,                              &
                              x1,      x2,     x3,                              &
                              y1,      y2,     y3,                              &
                              z1,      z2,     z3,                              &
                             vx1,     vx2,    vx3,                              &
                             vy1,     vy2,    vy3,                              &
                             vz1,     vz2,    vz3,                              &
                          for_t1,  for_t2, for_t3,                              &
                            stif,     ll ,  fqmax,                              &
                           penmin, penref,  marge,                              &
                           stif0,e_distor,    dt1,                              &
                           nctl ,   ifc1 ,   nel )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,          only : zero,one,em20
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
          integer, intent (inout)                                      :: nctl      !< number of self-contact pairs
          my_real, dimension(mvsiz), intent(in)                        :: stif0     !< initial nodal stiffness 
          my_real, dimension(mvsiz), intent(in)                        :: penmin    !< minimum penetration 
          my_real, dimension(mvsiz), intent(in)                        :: penref    !< reference penetration for quadratic stiffness 
          my_real, dimension(mvsiz), intent(in)                        :: marge     !< sorting marge 
          my_real, intent(in)                                          :: fqmax     !< quadratic stiffness limite of self-contact
          my_real, intent(in)                                          :: dt1       !< time step
          my_real, dimension(mvsiz), intent(in)                        :: ll        !< characteristic length
          my_real, dimension(mvsiz), intent(inout)                     :: stif      !< nodal stiffness to be updated
          my_real, dimension(nel),   intent(inout)                     :: e_distor  ! distortion energy
          my_real, dimension(mvsiz), intent(in   )                     ::       &        
                             xn1,     xn2,    xn3,                              &
                             yn1,     yn2,    yn3,                              &
                             zn1,     zn2,    zn3,                              &
                            vnx1,    vnx2,   vnx3,                              &
                            vny1,    vny2,   vny3,                              &
                            vnz1,    vnz2,   vnz3                                   !< 2nd nodal x,v array
          my_real, dimension(mvsiz,3), intent (inout) ::                        &
                           for_n1, for_n2, for_n3                                   !< 2nd nodal force array
          my_real, dimension(mvsiz), intent(in   )                     ::       &        
                              x1,      x2,     x3,                              &
                              y1,      y2,     y3,                              &
                              z1,      z2,     z3,                              &
                             vx1,     vx2,    vx3,                              &
                             vy1,     vy2,    vy3,                              &
                             vz1,     vz2,    vz3                                   !< main quad segment nodal x,v array
          my_real, dimension(mvsiz,3), intent (inout) ::                        &
                           for_t1, for_t2, for_t3                                   !< main quad segment force array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,j,ifctl1,ifctl,ifc2(mvsiz)
!                                                                    
      my_real :: rx,ry,rz,sx,sy,sz,nx(mvsiz),ny(mvsiz),nz(mvsiz),dx,dy,dz,dn,norm,area,dmin,fkt(mvsiz)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!------general info of seg, and sorting by projected distance
        do i=1,nel
           if (ifc1(i)==0) cycle
           rx =x2(i)-x1(i)
           ry =y2(i)-y1(i)
           rz =z2(i)-z1(i)
           sx =x3(i)-x1(i)
           sy =y3(i)-y1(i)
           sz =z3(i)-z1(i)
           nx(i)=ry*sz - rz*sy
           ny(i)=rz*sx - rx*sz
           nz(i)=rx*sy - ry*sx
           area = sqrt(nx(i)*nx(i)+ny(i)*ny(i)+nz(i)*nz(i))
           norm=one/max(em20,area)
           nx(i)=nx(i)*norm
           ny(i)=ny(i)*norm
           nz(i)=nz(i)*norm
           fkt(i)= one
        enddo
!----ns=n1
        ifc2(1:nel) = 0
        ifctl1 = 0
        ifctl = 0
        do i=1,nel
          if (ifc1(i)==0) cycle
          dx = xn1(i)-x1(i)
          dy = yn1(i)-y1(i)
          dz = zn1(i)-z1(i)
          dn = abs(nx(i)*dx+ny(i)*dy+nz(i)*dz)
          if (dn<marge(i).and.stif0(i)>zero) then 
            ifc2(i)=1
            ifctl1=1
          endif
        enddo
       if (ifctl1==1) then
         call sfor_n2s3(xn1,     yn1,    zn1,  for_n1,                        &
                         x1,      y1,     z1,  for_t1,                        &
                         x2,      y2,     z2,  for_t2,                        &
                         x3,      y3,     z3,  for_t3,                        &
                      stif0,    ifc2,  ifctl,  penmin,                        &
                     penref,     fkt,     ll,  fqmax ,                        &
                        vx1,     vx2,    vx3,  vnx1  ,                        &
                        vy1,     vy2,    vy3,  vny1  ,                        &
                        vz1,     vz2,    vz3,  vnz1  ,                        &
                        nel,e_distor,   dt1)                                  
         nctl = nctl + ifctl                                                   
       end if ! (ifctl1==1) then
!----ns=n2
        ifc2(1:nel) = 0
        ifctl1 = 0
        ifctl = 0
        do i=1,nel
          if (ifc1(i)==0) cycle
          dx = xn2(i)-x2(i)
          dy = yn2(i)-y2(i)
          dz = zn2(i)-z2(i)
          dn = abs(nx(i)*dx+ny(i)*dy+nz(i)*dz)
          if (dn<marge(i).and.stif0(i)>zero) then 
            ifc2(i)=1
            ifctl1=1
          endif
        enddo
       if (ifctl1==1) then
         call sfor_n2s3(xn2,     yn2,    zn2,  for_n2,                        &
                         x1,      y1,     z1,  for_t1,                        &
                         x2,      y2,     z2,  for_t2,                        &
                         x3,      y3,     z3,  for_t3,                        &
                      stif0,    ifc2,  ifctl,  penmin,                        &
                     penref,     fkt,     ll,  fqmax ,                        &
                        vx1,     vx2,    vx3,  vnx2  ,                        &
                        vy1,     vy2,    vy3,  vny2  ,                        &
                        vz1,     vz2,    vz3,  vnz2  ,                        &
                        nel,e_distor,   dt1)                                  
         nctl = nctl + ifctl                                                   
       end if ! (ifctl1==1) then
!----ns=n3
        ifc2(1:nel) = 0
        ifctl1 = 0
        ifctl = 0
        do i=1,nel
          if (ifc1(i)==0) cycle
          dx = xn3(i)-x3(i)
          dy = yn3(i)-y3(i)
          dz = zn3(i)-z3(i)
          dn = abs(nx(i)*dx+ny(i)*dy+nz(i)*dz)
          if (dn<marge(i).and.stif0(i)>zero) then 
            ifc2(i)=1
            ifctl1=1
          endif
        enddo
       if (ifctl1==1) then
         call sfor_n2s3(xn3,     yn3,    zn3,  for_n3,                        &
                         x1,      y1,     z1,  for_t1,                        &
                         x2,      y2,     z2,  for_t2,                        &
                         x3,      y3,     z3,  for_t3,                        &
                      stif0,    ifc2,  ifctl,  penmin,                        &
                     penref,     fkt,     ll,  fqmax ,                        &
                        vx1,     vx2,    vx3,  vnx3  ,                        &
                        vy1,     vy2,    vy3,  vny3  ,                        &
                        vz1,     vz2,    vz3,  vnz3  ,                        &
                        nel,e_distor,   dt1)                                  
         nctl = nctl + ifctl                                                   
       end if ! (ifctl1==1) then
!
        do i=1,nel
         if (ifc1(i)==0) cycle
         if ( fkt(i) > one) then
            stif(i) = max(stif(i),fkt(i)*stif0(i))
         end if
        enddo
!-----------
       end subroutine sfor_3n2s3
!-------------------
      end module sfor_nsn2seg_mod
 