!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    ush_force3_mod   ../engine/source/user_interface/ushforce3.F90
!||--- called by ------------------------------------------------------
!||    forintc          ../engine/source/elements/forintc.F
!||====================================================================
      module ush_force3_mod

      implicit none

      contains
!=======================================================================================================================
!!\brief This subroutine computes internal forces&moments of user shell property
!=======================================================================================================================
!||====================================================================
!||    ush_force3          ../engine/source/user_interface/ushforce3.F90
!||--- called by ------------------------------------------------------
!||    forintc             ../engine/source/elements/forintc.F
!||--- calls      -----------------------------------------------------
!||    ush_output          ../engine/source/user_interface/ushforce3.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod        ../common_source/modules/constant_mod.F
!||    elbufdef_mod        ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||    prop_param_mod      ../common_source/modules/mat_elem/prop_param_mod.F90
!||    sensor_mod          ../common_source/modules/sensor_mod.F90
!||====================================================================
        subroutine ush_force3(elbuf_tab,                                         &
                   numnod,  nparg,   npart,   nummat,    npsav,                  &
                    iparg,   nixx,     ixx,   ipartx,     nnod,                  &
                    itask,   ipri,userl_avail,   pm ,      nel,                  &
                        x,      v,      vr,        a,       ar,                  &
                        d,  stifn,   stifr,     thke,     iadx,                  &
                     fsky,partsav,     dt1,     dt2t,    time ,                  &
                      fcx,    fcy,     fcz,      mcx,      mcy,                  &
                      mcz,sensors,  iparit,     lsky,   nodadt)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use constant_mod,           only: zero,half,one,two,ep20,three,ten,one_over_12
          use prop_param_mod ,       only : n_var_pm
          use precision_mod,          only: WP
          use sensor_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include      "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                               intent (in   )     :: numnod           !< number node
          integer,                               intent (in   )     :: nummat           !< number material
          integer,                               intent (in   )     :: nparg            !< 1er dim of iparg
          integer,                               intent (in   )     :: npart            !< number of ipart
          integer,                               intent (in   )     :: nel              !< number of elements in this group
          integer,                               intent (in   )     :: nnod             !< number of nodes per element
          integer,                               intent (in   )     :: itask            !< thread id
          integer,                               intent (in   )     :: lsky             !< dimension of fskyv
          integer,                               intent (in   )     :: npsav            !< 1er dimension of partsav
          integer,                               intent (in   )     :: ipri             !< output flag
          integer,                               intent (in   )     :: iparit           !< P/ON flag
          integer,                               intent (in   )     :: nodadt           !< nodal time step flag
          integer,                               intent (in   )     :: userl_avail      !< user prop flag
          integer,                               intent (in   )     :: nixx             !< 1er dimension of ixx
          integer, dimension(nel),               intent (in   )     :: ipartx           !< element part id
          integer, dimension(nparg),             intent (in   )     :: iparg            !< element group data
          integer, dimension(nixx,nel),          intent (in   )     :: ixx              !< x element connectivity
          integer, dimension(nnod,nel),          intent (in   )     :: iadx             !< index for fskyv
          real(kind=WP),dimension(nel) ,         intent (in   )     :: thke             !< element initial thickness
          real(kind=WP),dimension(numnod) ,      intent (inout)     :: stifn            !< nodal translational stiffness
          real(kind=WP),dimension(numnod) ,      intent (inout)     :: stifr            !< nodal rotational stiffness
!          real(kind=WP),dimension(numnod) ,      intent (in   )     :: ms               !< nodal mass
!          real(kind=WP),dimension(numnod) ,      intent (in   )     :: in               !< nodal inertia
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: x                !< coordinates of nodes
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: v                !< velocity of nodes
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: d                !< displacement of nodes
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: vr               !< rotational velocity of nodes
          real(kind=WP),dimension(3,numnod),     intent (inout)     :: a                !< acceleration of nodes
          real(kind=WP),dimension(3,numnod),     intent (inout)     :: ar               !< rotational acceleration of nodes
          real(kind=WP),dimension(n_var_pm,nummat),intent(in   )    :: pm               !< material parameters
          real(kind=WP),dimension(npsav,npart),  intent(inout)      :: partsav          !< data to be saved for each part
          real(kind=WP),dimension(mvsiz,nnod),   intent(inout)      :: fcx              !< internal force x
          real(kind=WP),dimension(mvsiz,nnod),   intent(inout)      :: fcy              !< internal force y
          real(kind=WP),dimension(mvsiz,nnod),   intent(inout)      :: fcz              !< internal force z
          real(kind=WP),dimension(mvsiz,nnod),   intent(inout)      :: mcx              !< internal moment x
          real(kind=WP),dimension(mvsiz,nnod),   intent(inout)      :: mcy              !< internal moment y
          real(kind=WP),dimension(mvsiz,nnod),   intent(inout)      :: mcz              !< internal moment z
          real(kind=WP),dimension(8,lsky),       intent(inout)      :: fsky             !< data for Parith/on
          real(kind=WP),                         intent(in   )      :: dt1              !< time step at n-1
          real(kind=WP),                         intent(inout)      :: dt2t             !< smallest time step
          real(kind=WP),                         intent(in   )      :: time             !< time 
          type (elbuf_struct_),  target                             :: elbuf_tab        !< el_buf struct_
          type (sensors_)     ,                   intent(inout)     :: sensors          !< sensors structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n,imid,mtn,pid,ity,nf1,ip,igtyp,nuvar
          integer, dimension(6)  :: ii
          integer, dimension(nel)  :: iprop,sid,imat
          integer, dimension(nel,4)  :: ncj
          real(kind=WP),dimension(mvsiz,8) :: xx,yy,zz,ux,uy,uz,vx,vy,vz,vrx,vry,vrz,fx,fy,fz,mx,my,mz
          real(kind=WP),dimension(mvsiz)   :: area,mass,dtx,iner,sti,stir,viscm,viscr,masm,inm,rx,ry,rz,sx,sy,sz
          real(kind=WP),dimension(nel)     :: eint_loc,vol_loc,off_loc,rho_loc,thk,fr_w_e
          real(kind=WP),dimension(6,nel)   :: sig_loc
          real(kind=WP) :: lx,ly,lz,sxx,syy,szz,sxy,szx,syz,svx,svy,svz,sv2,dt2,visn,visr,ex,ey,ez,a2,off_l,dum,rho0
          type(g_bufel_) , pointer :: gbuf     
          real(kind=WP), dimension(:), pointer :: uvar     

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
      gbuf  => elbuf_tab%gbuf

!      nuvar = elbuf_tab%bufly(1)%nvar_mat         !doesn't work for shell
!      uvar => elbuf_tab%bufly(1)%mat(1,1,1)%var      
      nuvar = elbuf_tab%gbuf%g_nuvar 
      uvar => elbuf_tab%gbuf%var      
      ity = iparg(5)
      igtyp = iparg(38)

!
      do i=1,6
        ii(i) = nel*(i-1)
      enddo
!
!-----------------------------------------------
        xx=zero
        yy=zero
        zz=zero
        vx=zero
        vy=zero
        vz=zero
        vrx=zero
        vry=zero
        vrz=zero
        sig_loc = zero
        fr_w_e=zero
      do j=1,nnod
        do i=1,nel
          ncj(i,j)=ixx(j+1,i)
          xx(i,j)=x(1,ncj(i,j))
          yy(i,j)=x(2,ncj(i,j))
          zz(i,j)=x(3,ncj(i,j))
          ux(i,j)=d(1,ncj(i,j))
          uy(i,j)=d(2,ncj(i,j))
          uz(i,j)=d(3,ncj(i,j))
          vx(i,j)=v(1,ncj(i,j))
          vy(i,j)=v(2,ncj(i,j))
          vz(i,j)=v(3,ncj(i,j))
          vrx(i,j)=vr(1,ncj(i,j))
          vry(i,j)=vr(2,ncj(i,j))
          vrz(i,j)=vr(3,ncj(i,j))
        enddo
      enddo
!-----------------------------------------------
      do i=1,nel
        iprop(i)=ixx(nixx-1,i)
        sid(i)  =ixx(nixx,i)
        imat(i) =ixx(1,i)
      enddo
      rho0 = pm(1,imat(1)) ! initial density, may not correct for multi-mat
!----------------------------
!     internal forces
!----------------------------
       do i=1,nel
         eint_loc(i)  = gbuf%eint(i)
         vol_loc(i)   = gbuf%vol(i)
         off_loc(i)   = gbuf%off(i)
         rho_loc(i)   = rho0
       enddo
      if (userl_avail>0) then
          call eng_userlib_suser(igtyp,                                    &
       nel    ,nuvar        ,iprop  ,imat  ,sid  ,time   ,dt1    ,         &
       eint_loc,vol_loc,uvar,fr_w_e,off_loc,rho_loc,sig_loc      ,         &
       xx(1,1),xx(1,2),xx(1,3),xx(1,4),xx(1,5),xx(1,6),xx(1,7),xx(1,8),    &
       yy(1,1),yy(1,2),yy(1,3),yy(1,4),yy(1,5),yy(1,6),yy(1,7),yy(1,8),    &
       zz(1,1),zz(1,2),zz(1,3),zz(1,4),zz(1,5),zz(1,6),zz(1,7),zz(1,8),    &
       ux(1,1),ux(1,2),ux(1,3),ux(1,4),ux(1,5),ux(1,6),ux(1,7),ux(1,8),    &
       uy(1,1),uy(1,2),uy(1,3),uy(1,4),uy(1,5),uy(1,6),uy(1,7),uy(1,8),    &
       uz(1,1),uz(1,2),uz(1,3),uz(1,4),uz(1,5),uz(1,6),uz(1,7),uz(1,8),    &
       vx(1,1),vx(1,2),vx(1,3),vx(1,4),vx(1,5),vx(1,6),vx(1,7),vx(1,8),    &
       vy(1,1),vy(1,2),vy(1,3),vy(1,4),vy(1,5),vy(1,6),vy(1,7),vy(1,8),    &
       vz(1,1),vz(1,2),vz(1,3),vz(1,4),vz(1,5),vz(1,6),vz(1,7),vz(1,8),    &
       vrx(1,1),vrx(1,2),vrx(1,3),vrx(1,4),                                &
                                   vrx(1,5),vrx(1,6),vrx(1,7),vrx(1,8),    &
       vry(1,1),vry(1,2),vry(1,3),vry(1,4),                                &
                                   vry(1,5),vry(1,6),vry(1,7),vry(1,8),    &
       vrz(1,1),vrz(1,2),vrz(1,3),vrz(1,4),                                &
                                   vrz(1,5),vrz(1,6),vrz(1,7),vrz(1,8),    &
       fx(1,1),fx(1,2),fx(1,3),fx(1,4),fx(1,5),fx(1,6),fx(1,7),fx(1,8),    &
       fy(1,1),fy(1,2),fy(1,3),fy(1,4),fy(1,5),fy(1,6),fy(1,7),fy(1,8),    &
       fz(1,1),fz(1,2),fz(1,3),fz(1,4),fz(1,5),fz(1,6),fz(1,7),fz(1,8),    &
       mx(1,1),mx(1,2),mx(1,3),mx(1,4),mx(1,5),mx(1,6),mx(1,7),mx(1,8),    &
       my(1,1),my(1,2),my(1,3),my(1,4),my(1,5),my(1,6),my(1,7),my(1,8),    &
       mz(1,1),mz(1,2),mz(1,3),mz(1,4),mz(1,5),mz(1,6),mz(1,7),mz(1,8),    &
       sti    ,stir   ,viscm  ,viscr) ! add mass_el(replacing rho_loc?) iner_el,thk_new,dt_elem  
       do i=1,nel
         gbuf%eint(i) = eint_loc(i)
         gbuf%vol(i) = vol_loc(i)
         if (off_loc(i) < one) gbuf%off(i) = off_loc(i)
         gbuf%rho(i) = rho_loc(i)
       enddo
       do j=1,nnod
          do i=1,nel
            fcx(i,j) = - fx(i,j)
            fcy(i,j) = - fy(i,j)
            fcz(i,j) = - fz(i,j)
            mcx(i,j) = - mx(i,j)
            mcy(i,j) = - my(i,j)
            mcz(i,j) = - mz(i,j)
          enddo
       enddo
! compute area
      if (nnod==4) then
        do i=1,nel
          rx(i)=x(1,ncj(i,2))+x(1,ncj(i,3))-x(1,ncj(i,1))-x(1,ncj(i,4))
          sx(i)=x(1,ncj(i,3))+x(1,ncj(i,4))-x(1,ncj(i,1))-x(1,ncj(i,2))
          ry(i)=x(2,ncj(i,2))+x(2,ncj(i,3))-x(2,ncj(i,1))-x(2,ncj(i,4))
          sy(i)=x(2,ncj(i,3))+x(2,ncj(i,4))-x(2,ncj(i,1))-x(2,ncj(i,2))
          rz(i)=x(3,ncj(i,2))+x(3,ncj(i,3))-x(3,ncj(i,1))-x(3,ncj(i,4))
          sz(i)=x(3,ncj(i,3))+x(3,ncj(i,4))-x(3,ncj(i,1))-x(3,ncj(i,2))
        enddo 
      else
        do i=1,nel
          rx(i)=x(1,ncj(i,2))-x(1,ncj(i,1))
          sx(i)=x(1,ncj(i,3))-x(1,ncj(i,1))
          ry(i)=x(2,ncj(i,2))-x(2,ncj(i,1))
          sy(i)=x(2,ncj(i,3))-x(2,ncj(i,1))
          rz(i)=x(3,ncj(i,2))-x(3,ncj(i,1))
          sz(i)=x(3,ncj(i,3))-x(3,ncj(i,1))
        enddo 
      end if !(nnod==4) then
      do i=1,nel
        ex = ry(i) * sz(i) - rz(i) * sy(i) 
        ey = rz(i) * sx(i) - rx(i) * sz(i) 
        ez = rx(i) * sy(i) - ry(i) * sx(i) 
        a2 = sqrt(ex*ex + ey*ey + ez*ez)
        area(i)=half*a2
        thk(i)=gbuf%vol(i)/area(i) ! have to recompute
        dum = thk(i)/thke(i)
      end do
!
      off_l = zero
      do i=1,nel
        off_l = min(off_l,gbuf%off(i))
      enddo
      if(off_l<zero)then
        do j=1,nnod
          do i=1,nel
            if(gbuf%off(i)<zero)then
              fx(i,j) =zero
              fy(i,j) =zero
              fz(i,j) =zero
              mx(i,j) =zero
              my(i,j) =zero
              mz(i,j) =zero
              stifn(ncj(i,j))=zero
              stifr(ncj(i,j))=zero
            end if
          enddo
        enddo
      end if
!----------------------------
      if (iparit == 0) then
        do j=1,nnod
          do i=1,nel
            a(1,ncj(i,j)) = a(1,ncj(i,j)) - fx(i,j)
            a(2,ncj(i,j)) = a(2,ncj(i,j)) - fy(i,j)
            a(3,ncj(i,j)) = a(3,ncj(i,j)) - fz(i,j)
            ar(1,ncj(i,j)) = ar(1,ncj(i,j)) - mx(i,j)
            ar(2,ncj(i,j)) = ar(2,ncj(i,j)) - my(i,j)
            ar(3,ncj(i,j)) = ar(3,ncj(i,j)) - mz(i,j)
            stifn(ncj(i,j))=stifn(ncj(i,j))+sti(i)
            stifr(ncj(i,j))=stifr(ncj(i,j))+stir(i)
          enddo
        enddo
      else
!-----------------only ivector=0
          do j=1,nnod
            do i=1,nel
              fsky(1,iadx(j,i))=-fx(i,j)
              fsky(2,iadx(j,i))=-fy(i,j)
              fsky(3,iadx(j,i))=-fz(i,j)
              fsky(4,iadx(j,i))=-mx(i,j)
              fsky(5,iadx(j,i))=-my(i,j)
              fsky(6,iadx(j,i))=-mz(i,j)
              fsky(7,iadx(j,i))=sti(i)
              fsky(8,iadx(j,i))=stir(i)
            enddo
          enddo 
      endif
!------elementary time step calculation have to be computed here
      if (nodadt==0) then
        do i=1,nel
          if(gbuf%off(i)<=zero) cycle
          mass(i)=rho_loc(i)*gbuf%vol(i)/nnod
          iner(i)=mass(i)*(thk(i)*thk(i)+area(i))*one_over_12
          visn = sqrt(one + viscm(i)*viscm(i))-viscm(i)
          visr = sqrt(one + viscr(i)*viscr(i))-viscr(i)
          dt2=min(visn*mass(i)/sti(i),visr*iner(i)/stir(i))
          dtx(i) = sqrt(two*dt2)
          dt2t = min(dtx(i),dt2t)
        enddo
      endif
! output partsav... for user
      if (ipri==1) then
        ip = ipartx(1)
        call ush_output(                                                         &
                   numnod,    nnod,  itask,    npsav,      nel,                  &
                      ncj,  ipartx,gbuf%off,eint_loc,       thk,                 &
                     area,    mass,       x,        v,       vr,                 &
                  partsav(1,ip),sensors)
      end if !(ipri==1) then
    endif ! if (userl_avail)
      end subroutine ush_force3
!=======================================================================================================================
!!\brief This subroutine computes output data for user shell property
!=======================================================================================================================
!||====================================================================
!||    ush_output            ../engine/source/user_interface/ushforce3.F90
!||--- called by ------------------------------------------------------
!||    ush_force3            ../engine/source/user_interface/ushforce3.F90
!||--- calls      -----------------------------------------------------
!||    sensor_energy_bilan   ../engine/source/tools/sensor/sensor_energy_bilan.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    sensor_mod            ../common_source/modules/sensor_mod.F90
!||====================================================================
        subroutine ush_output(                                                   &
                   numnod,    nnod,  itask,    npsav,      nel,                  &
                      ncj,  ipartx,    off,     eint,      thk,                  &
                     area,    xmas,      x,        v,       vr,                  &
                  partsav, sensors)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,           only: zero,one,half,one_over_12
          use precision_mod,          only: WP
          use sensor_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                               intent (in   )     :: numnod           !< number node
          integer,                               intent (in   )     :: nel              !< number of elements in this group
          integer,                               intent (in   )     :: nnod             !< number of nodes per element
          integer,                               intent (in   )     :: itask            !< thread id
          integer,                               intent (in   )     :: npsav            !< 1er dimension of partsav
          integer, dimension(nel),               intent (in   )     :: ipartx           !< element part id
          integer, dimension(nel,nnod),          intent (in   )     :: ncj              !< element node id
          real(kind=WP),dimension(nel) ,         intent (in   )     :: off              !< element activation value
          real(kind=WP),dimension(nel) ,         intent (in   )     :: eint             !< element internal energy
          real(kind=WP),dimension(nel) ,         intent (in   )     :: area             !< element area
          real(kind=WP),dimension(nel) ,         intent (in   )     :: thk              !< element thickness
          real(kind=WP),dimension(nel) ,         intent (in   )     :: xmas             !< element mass
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: x                !< coordinates of nodes
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: v                !< velocity of nodes
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: vr               !< rotational velocity of nodes
          real(kind=WP),dimension(npsav),        intent (inout)     :: partsav          !< data to be saved for each part
          type (sensors_),                       intent (inout)     :: sensors          !< sensors structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          real(kind=WP),dimension(nel)   :: ei,ek,xm,ym,zm,vxa,vya,vza,thk2,xmas25,xcg,ycg,zcg,xxm,yym,zzm,rei,rek
          real(kind=WP),dimension(nel)   :: ixx,iyy,izz,ixy,iyz,izx
          real(kind=WP) :: svx,svy,svz,sv2,lx,ly,lz,sxx,syy,szz,inel,in25,xx,yy,zz
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------
      thk2(1:nel)=thk(1:nel)*thk(1:nel)
      do i=1,nel
        svx = zero
        svy = zero
        svz = zero
        sv2 = zero
        do j=1,nnod
           svx = svx + v(1,ncj(i,j))
           svy = svy + v(2,ncj(i,j))
           svz = svz + v(3,ncj(i,j))
           sv2 = sv2 + v(1,ncj(i,j))*v(1,ncj(i,j)) +       &
                       v(2,ncj(i,j))*v(2,ncj(i,j)) +       &
                       v(3,ncj(i,j))*v(3,ncj(i,j))
        end do
        ei(i)= eint(i) 
        ek(i)= xmas(i)*sv2/nnod
        xmas25(i)= xmas(i)/nnod
        xm(i)= xmas25(i)*svx
        ym(i)= xmas25(i)*svy
        zm(i)= xmas25(i)*svz
        vxa(i)=svx/nnod
        vya(i)=svy/nnod
        vza(i)=svz/nnod
      enddo
!      
      do i=1,nel
        lx = zero 
        ly = zero
        lz = zero
        sxx = zero 
        syy = zero
        szz = zero
        do j=1,nnod
           lx = lx + x(1,ncj(i,j))
           ly = ly + x(2,ncj(i,j)) 
           lz = lz + x(3,ncj(i,j)) 
           sxx = sxx + x(1,ncj(i,j))*x(1,ncj(i,j))
           syy = syy + x(2,ncj(i,j))*x(2,ncj(i,j))
           szz = szz + x(3,ncj(i,j))*x(3,ncj(i,j))
        end do
        svx = zero
        svy = zero
        svz = zero
        sv2 = zero
        do j=1,nnod
           svx = svx + vr(1,ncj(i,j))
           svy = svy + vr(2,ncj(i,j))
           svz = svz + vr(3,ncj(i,j))
           sv2 = sv2 + vr(1,ncj(i,j))*vr(1,ncj(i,j)) +       &
                       vr(2,ncj(i,j))*vr(2,ncj(i,j)) +       &
                       vr(3,ncj(i,j))*vr(3,ncj(i,j))
        end do
         xcg(i)= xmas25(i)*lx
         ycg(i)= xmas25(i)*ly
         zcg(i)= xmas25(i)*lz
!
         inel = xmas(i)*(thk2(i)+area(i))*one_over_12
         in25 = inel/nnod
         xx = lx/nnod
         yy = ly/nnod
         zz = lz/nnod
         ixy(i) = -xcg(i)*yy
         iyz(i) = -ycg(i)*zz
         izx(i) = -zcg(i)*xx
         xx = xcg(i)*xx
         yy = ycg(i)*yy
         zz = zcg(i)*zz
         ixx(i)= inel + yy + zz
         iyy(i)= inel + zz + xx
         izz(i)= inel + xx + yy
         xxm(i)= vza(i)*ycg(i)-vya(i)*zcg(i)+in25*svx
         yym(i)= vxa(i)*zcg(i)-vza(i)*xcg(i)+in25*svy
         zzm(i)= vya(i)*xcg(i)-vxa(i)*ycg(i)+in25*svz
         rei(i)= eint(i)
         rek(i)= in25*sv2*half
        enddo
      do i=1,nel
         if(off(i)/=zero)then
           partsav(1)=partsav(1) + ei(i)
           partsav(2)=partsav(2) + ek(i)
           partsav(3)=partsav(3) + xm(i)
           partsav(4)=partsav(4) + ym(i)
           partsav(5)=partsav(5) + zm(i)
           partsav(6)=partsav(6) + xmas(i)
         endif
          partsav(9) =partsav(9)  + xcg(i)
          partsav(10)=partsav(10) + ycg(i)
          partsav(11)=partsav(11) + zcg(i)
          partsav(12)=partsav(12) + xxm(i)
          partsav(13)=partsav(13) + yym(i)
          partsav(14)=partsav(14) + zzm(i)
          partsav(15)=partsav(15) + ixx(i)
          partsav(16)=partsav(16) + iyy(i)
          partsav(17)=partsav(17) + izz(i)
          partsav(18)=partsav(18) + ixy(i)
          partsav(19)=partsav(19) + iyz(i)
          partsav(20)=partsav(20) + izx(i)
          partsav(21)=partsav(21) + rei(i)
          partsav(22)=partsav(22) + rek(i)
          if (off(i)==zero) partsav(25) = partsav(25) + one
      enddo
      call sensor_energy_bilan(1,nel,ei,ek,off,ipartx,itask,sensors)      
!---
      end subroutine ush_output
!
      end module ush_force3_mod
