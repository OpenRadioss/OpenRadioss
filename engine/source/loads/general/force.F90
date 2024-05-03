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
      module force_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief force : computation for loads
        subroutine force (                                             &
        & nibcld     ,ib         ,lfaccld    ,fac       ,snpc       ,&
        & npc        ,stf        ,tf         ,a         ,v          ,&
        & x          ,lskew      ,numskw     ,skew      ,ar         ,&
        & vr         ,nsensor    ,sensor_tab ,tfexc     ,iadc       ,&
        & lsky       ,fsky       ,fext       ,h3d_data  ,cptreac    ,&
        & fthreac    ,nodreac    ,th_surf    ,fsavsurf  ,nseg_loadp ,&
        & dpl0cld    ,vel0cld    ,d          ,dr        ,nconld     ,&
        & numnod     ,nsurf      ,nfunct     ,anim_v    ,outp_v     ,&
        & iparit     ,tt         ,dt1        ,n2d       ,tfext      ,&
        & impl_s     ,python )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod ,only : one_over_6,zero,one,half,one_over_8
          use python_funct_mod
          use h3d_mod
          use pinchtype_mod
          use sensor_mod
          use th_surf_mod , only : th_surf_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          integer get_u_numsens    !< External function / User subroutines
          integer get_u_sens_fpar  !< External function / User subroutines
          integer get_u_sens_ipar  !< External function / User subroutines
          integer get_u_sens_value !< External function / User subroutines
          integer set_u_sens_value !< External function / User subroutines
          external get_u_numsens
          external get_u_sens_fpar
          external get_u_sens_ipar
          external get_u_sens_value
          external set_u_sens_value
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          ! Input
          integer, intent(in) :: numnod                        !< number of nodes in model
          integer, intent(in) :: numskw                        !< number of skew in model
          integer, intent(in) :: nfunct                        !< number of tabulated functions in model
          integer, intent(in) :: nsurf                         !< number of surfaces in model
          integer, intent(in) :: stf                           !< size of tabulated functions array
          integer, intent(in) :: nconld                        !< number of condition loads in model
          integer, intent(in) :: n2d                           !< 2d resolution flag
          integer, intent(in) :: impl_s                        !< implicit flag
          integer, intent(in) :: iparit                        !< parallel arithmetic flag
          integer, intent(in) :: lsky                          !< size of skyline array (/parith/on)
          integer, intent(in) :: lskew                         !< size of skew array
          integer, intent(in) :: nibcld                        !< loads: number of variables per load
          integer, intent(in) :: lfaccld                       !< loads: number of variables per fac
          integer, intent(in) :: nsensor                       !< number of sensors in model
          integer, intent(in) :: cptreac                       !< size of fthreac
          integer, intent(in) :: snpc                          !< size of npc array
          integer, intent(in) :: npc(snpc)                     !< NPC option
          integer, intent(in) :: nodreac(numnod)               !< Node ID for REAC option
          integer, intent(in) :: ib(nibcld,nconld)             !< Concentrated loads buffer
          my_real, intent(in) :: fac(lfaccld,nconld)           !< array for concentrated loads
          my_real, intent(in) :: skew(lskew,numskw+1)          !< Skew array
          my_real, intent(in) :: tf(stf)                       !< Tabulated function array
          my_real, intent(in) :: dpl0cld(6,nconld)             !< condition loads displacements
          my_real, intent(in) :: vel0cld(6,nconld)             !< condition loads velocity
          integer, intent(in) :: iadc(4,nconld)                !< condition loads nodes
          my_real, intent(in) :: x(3,numnod)                   !< node position
          my_real, intent(in) :: v(3,numnod)                   !< node velocity
          my_real, intent(in) :: vr(3,numnod)                  !< node rotational velocity
          my_real, intent(in) :: d(3,numnod)                   !< displacement
          my_real, intent(in) :: dr(3,numnod)                  !< rotational displacement
          integer, intent(in) :: anim_v(80000)                 !< animation vector options
          integer, intent(in) :: outp_v(140)                   !< outp vector options
          my_real, intent(in) :: tt                            !< current time
          my_real, intent(in) :: dt1                           !< current time step
          type (sensor_str_) ,dimension(nsensor),intent(in) :: sensor_tab !< Sensors type
          type(python_), intent(in), optional :: python        !< Python functions structures
          type(h3d_database), intent(in) :: h3d_data           !< H3D output structure
          type (th_surf_) , intent(in) :: th_surf              !< Time history / surface
          ! Inout / output
          my_real, intent(inout) :: fsky(8,lsky)               !< skyline vector : Strore forces for PARITH/ON
          my_real, intent(inout) :: a(3,numnod)                !< node accelerations
          my_real, intent(inout) :: ar(3,numnod)               !< node rotational accelerations
          my_real, intent(inout) :: fext(3,numnod)             !< nodal external work
          double precision, intent(inout) :: tfext             !< external work - Care : double precision
          my_real, intent(inout) :: tfexc                      !< external work
          my_real, intent(inout) :: fsavsurf(5,nsurf)          !< save forces for surfaces
          my_real, intent(inout) :: fthreac(6,cptreac)         !< TH forces
          integer, intent(inout) :: nseg_loadp(nsurf)          !< surface  array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer nl, n1, isk, n2, n3, n4, n5, k1, k2, k3, isens,k,&
          &iad,n_old,ianim,ifun,idir,&
          &ismooth,idel,ksurf,ns,nsegpl
          integer :: up_bound
          integer :: fid ! id of the python function
          my_real nx, ny, nz, axi, aa, a0, vv, fx, fy, fz, ax, dydx, ts,&
          &sixth,tfextt,x_old, f1, f2,xsens,fcx,fcy,area,&
          &disp,disp_old,vel,vel_old
          my_real finter, finter_smooth
          external finter,finter_smooth
! ----------------------------------------------------------------------------------------------------------------------
          sixth  = one_over_6
          tfexc  = zero
          tfextt = zero
          n_old  = 0
          x_old  = zero
          disp_old = zero
          vel_old  = zero
          vel = zero
          disp = zero
          nsegpl = 0
          ianim  = anim_v(5)+outp_v(5)+h3d_data%n_vect_fint+&
          &anim_v(6)+outp_v(6)+h3d_data%n_vect_fext
!
          if(iparit==0) then
! code spmd parith/off ou smp ne pas oublier le code p/on !
            ! en parith/on pour le spmd il faut calculer sur chaque proc
            ! en parith/off pour le spmd il ne faut calculer qu'une fois
            ! noeud servant sur ce proc
            do nl=1,nconld-nploadpinch
              n1      = ib(1,nl)
              n2      = ib(2,nl)
              n3      = ib(3,nl)
              n4      = ib(4,nl)
              n5      = ib(5,nl)
              idel    = ib(8,nl)
              ifun    = ib(9,nl)
              fcy     = fac(1,nl)
              fcx     = fac(2,nl)
!---------------------------------------
              if (ifun > 1) then
                isk = ib(2,nl)/10
                idir  = ib(2,nl)-10*isk
                if (idir<=3) then
                  disp = d(idir,n1)
                  vel  = v(idir,n1)
                elseif (idir<=6) then
                  disp = dr(idir-3,n1)
                  vel  = vr(idir-3,n1)
                endif
              endif
!----------------------------------------
!
              isens   = 0
              xsens   = one
              do k=1,nsensor
                if(ib(6,nl) == sensor_tab(k)%sens_id) isens=k
              enddo
              if(isens==0)then
                ts=tt
              else
                ts = tt-sensor_tab(isens)%tstart
                if(ts < zero) cycle
              endif
!
              if(n4==-1)then
!----------------
!       force concentree
!----------------
                !!--------------------
                if(ifun == 1)then
                  !! time dependent abscisa
                  if(n_old/=n3.or.x_old/=ts) then
                    ismooth = 0
                    if (n3 > 0) ismooth = npc(2*nfunct+n3+1)
                    if (ismooth == 0) then
                      f1 = finter(n3,(ts-dt1)*fcx,npc,tf,dydx) !! current cycle
                      f2 = finter(n3,ts*fcx,npc,tf,dydx)       !! previous cycle
                    else if (ismooth > 0) then
                      f1 = finter_smooth(n3,(ts-dt1)*fcx,npc,tf,dydx)
                      f2 = finter_smooth(n3,ts*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,(ts-dt1)*fcx,f1)
                        call python_call_funct1d(python, fid,ts*fcx,f2)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n3
                    x_old = ts
                  endif
                else if(ifun == 2)then
                  !! displacement dependent abscisa
                  !!disp_old  !! previous cycle displacement
                  !!disp      !! current cycle displacement
                  disp_old = dpl0cld(idir,nl)
                  if(n_old/=n3.or.x_old/=disp) then
                    ismooth = 0
                    if (n3 > 0) ismooth = npc(2*nfunct+n3+1)
                    if (ismooth == 0) then
                      f1 = finter(n3,disp_old*fcx,npc,tf,dydx)
                      f2 = finter(n3,disp*fcx,npc,tf,dydx)
                    else if (ismooth > 0) then
                      f1 = finter_smooth(n3,disp_old*fcx,npc,tf,dydx)
                      f2 = finter_smooth(n3,disp*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,disp_old*fcx,f1)
                        call python_call_funct1d(python, fid,disp*fcx,f2)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n3
                    x_old = disp
                  endif
                else if(ifun == 3)then
                  !! velocity dependent abscisa
                  !!vel_old   !! previous cycle velocity
                  !!vel       !! current cycle velocity
                  vel_old  = vel0cld(idir,nl)
                  if(n_old/=n3.or.x_old/=vel) then
                    ismooth = 0
                    if (n3 > 0) ismooth = npc(2*nfunct+n3+1)
                    if (ismooth == 0) then
                      f1 = finter(n3,vel_old*fcx,npc,tf,dydx)
                      f2 = finter(n3,vel*fcx,npc,tf,dydx)
                    else if (ismooth > 0) then
                      f1 = finter_smooth(n3,vel_old*fcx,npc,tf,dydx)
                      f2 = finter_smooth(n3,vel*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,vel_old*fcx,f1)
                        call python_call_funct1d(python, fid,vel*fcx,f2)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n3
                    x_old = vel
                  endif
                endif
                !!--------------------
                a0  = fcy*f1
                aa  = fcy*f2
                isk = ib(2,nl)/10
                n2  = ib(2,nl)-10*isk
                if(n2<=3)then
                  if(n2d/=1)then
                    axi=one
                  else
                    axi=x(2,n1)
                  endif
                  if(isk<=1)then
                    a(n2,n1)=a(n2,n1)+aa
!
                    if (cptreac > 0)  then
                      if(nodreac(n1)/=0) then
                        fthreac(n2,nodreac(n1)) = fthreac(n2,nodreac(n1)) + aa*dt1
                      endif
                    endif
!
                    if(ianim >0 .and.impl_s==0) then
                      fext(n2,n1)=fext(n2,n1)+aa
                    endif
                    tfexc = tfexc+half*(a0+aa)*v(n2,n1)*axi
                  else
                    k1=3*n2-2
                    k2=3*n2-1
                    k3=3*n2
                    vv = skew(k1,isk)*v(1,n1)+skew(k2,isk)*v(2,n1)+skew(k3,isk)*v(3,n1)
                    a(1,n1)=a(1,n1)+skew(k1,isk)*aa
                    a(2,n1)=a(2,n1)+skew(k2,isk)*aa
                    a(3,n1)=a(3,n1)+skew(k3,isk)*aa
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n1) = fext(1,n1)+skew(k1,isk)*aa
                      fext(2,n1) = fext(2,n1)+skew(k2,isk)*aa
                      fext(3,n1) = fext(3,n1)+skew(k3,isk)*aa
                    endif
!
!
                    if (cptreac > 0 ) then
                      if(nodreac(n1)/=0) then
                        fthreac(1,nodreac(n1)) = fthreac(1,nodreac(n1)) + skew(k1,isk)*aa*dt1
                        fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + skew(k2,isk)*aa*dt1
                        fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + skew(k3,isk)*aa*dt1
                      endif
                    endif!
!
                  endif
                elseif(n2<=6)then
                  n2 = n2 - 3
                  if(isk<=1)then
                    ar(n2,n1)=ar(n2,n1)+aa
                    tfexc=tfexc+half*(a0+aa)*vr(n2,n1)
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(n2+3,nodreac(n1)) = fthreac(n2+3,nodreac(n1)) + aa*dt1
                      endif
                    endif!
!
                  else
                    k1       = 3*n2-2
                    k2       = 3*n2-1
                    k3       = 3*n2
                    vv       = skew(k1,isk)*vr(1,n1)+skew(k2,isk)*vr(2,n1)+skew(k3,isk)*vr(3,n1)
                    ar(1,n1) = ar(1,n1)+skew(k1,isk)*aa
                    ar(2,n1) = ar(2,n1)+skew(k2,isk)*aa
                    ar(3,n1) = ar(3,n1)+skew(k3,isk)*aa
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(4,nodreac(n1)) = fthreac(4,nodreac(n1)) + skew(k1,isk)*aa*dt1
                        fthreac(5,nodreac(n1)) = fthreac(5,nodreac(n1)) + skew(k2,isk)*aa*dt1
                        fthreac(6,nodreac(n1)) = fthreac(6,nodreac(n1)) + skew(k3,isk)*aa*dt1
                      endif
                    endif
!
                    tfexc    = tfexc+half*(a0+aa)*vv
                  endif
                endif
!
              else
!----------------
!       pression
!----------------

                if(idel > 0 ) cycle  ! segment deleted
!
                if(ifun == 1)then
                  !! time dependent abscisa
                  if(n_old/=n5.or.x_old/=ts) then
                    ismooth = 0
                    if (n5 > 0) ismooth = npc(2*nfunct+n5+1)
                    if (ismooth == 0) then
                      f1 = finter(n5,ts*fcx,npc,tf,dydx)
                    else if(ismooth > 0) then
                      f1 = finter_smooth(n5,ts*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,ts*fcx,f1)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n5
                    x_old = ts
                  endif
                elseif(ifun == 2)then
                  !! displacement dependent abscisa
                  !!disp      !! current cycle displacement
                  if(n_old/=n5.or.x_old/=disp) then
                    ismooth = 0
                    if (n5 > 0) ismooth = npc(2*nfunct+n5+1)
                    if (ismooth == 0) then
                      f1 = finter(n5,disp*fcx,npc,tf,dydx)
                    else if(ismooth > 0) then
                      f1 = finter_smooth(n5,disp*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,disp*fcx,f1)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n5
                    x_old = disp
                  endif
                elseif(ifun == 3)then
                  !! velocity dependent abscisa
                  !!vel       !! current cycle velocity
                  if(n_old/=n5.or.x_old/=vel) then
                    ismooth = 0
                    if (n5 > 0) ismooth = npc(2*nfunct+n5+1)
                    if (ismooth == 0) then
                      f1 = finter(n5,vel*fcx,npc,tf,dydx)
                    else if(ismooth > 0) then
                      f1 = finter_smooth(n5,vel*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,vel*fcx,f1)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n5
                    x_old = vel
                  endif
                endif
!
                aa = fcy*f1*xsens
!
                if(n2d==0)then
!        analyse 3d
                  if(n4/=0)then
                    nx = (x(2,n3)-x(2,n1))*(x(3,n4)-x(3,n2)) - (x(3,n3)-x(3,n1))*(x(2,n4)-x(2,n2))
                    ny = (x(3,n3)-x(3,n1))*(x(1,n4)-x(1,n2)) - (x(1,n3)-x(1,n1))*(x(3,n4)-x(3,n2))
                    nz = (x(1,n3)-x(1,n1))*(x(2,n4)-x(2,n2)) - (x(2,n3)-x(2,n1))*(x(1,n4)-x(1,n2))
                    fx = aa*nx*one_over_8
                    fy = aa*ny*one_over_8
                    fz = aa*nz*one_over_8
!
                    a(1,n1)=a(1,n1)+fx
                    a(2,n1)=a(2,n1)+fy
                    a(3,n1)=a(3,n1)+fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n1) = fext(1,n1)+fx
                      fext(2,n1) = fext(2,n1)+fy
                      fext(3,n1) = fext(3,n1)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(1,nodreac(n1)) = fthreac(1,nodreac(n1)) + fx*dt1
                        fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + fy*dt1
                        fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + fz*dt1
                      endif
                    endif
!
                    if(th_surf%pload_flag >0 ) then
                      nsegpl = nsegpl + 1
                      area = half*sqrt(nx*nx+ny*ny+nz*nz)
                      do ns=th_surf%pload_ksegs(nsegpl) +1,th_surf%pload_ksegs(nsegpl+1)
                        ksurf = th_surf%pload_segs(ns)
                        nseg_loadp(ksurf) = nseg_loadp(ksurf) + 1
                        fsavsurf(4,ksurf)= fsavsurf(4,ksurf) + aa
                        fsavsurf(5,ksurf)= fsavsurf(5,ksurf) + area
                      enddo
                    endif
!
                    a(1,n2)=a(1,n2)+fx
                    a(2,n2)=a(2,n2)+fy
                    a(3,n2)=a(3,n2)+fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n2) = fext(1,n2)+fx
                      fext(2,n2) = fext(2,n2)+fy
                      fext(3,n2) = fext(3,n2)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n2)/=0) then
                        fthreac(1,nodreac(n2)) = fthreac(1,nodreac(n2)) + fx*dt1
                        fthreac(2,nodreac(n2)) = fthreac(2,nodreac(n2)) + fy*dt1
                        fthreac(3,nodreac(n2)) = fthreac(3,nodreac(n2)) + fz*dt1
                      endif
                    endif
!
!
                    a(1,n3)=a(1,n3)+fx
                    a(2,n3)=a(2,n3)+fy
                    a(3,n3)=a(3,n3)+fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n3) = fext(1,n3)+fx
                      fext(2,n3) = fext(2,n3)+fy
                      fext(3,n3) = fext(3,n3)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n3)/=0) then
                        fthreac(1,nodreac(n3)) = fthreac(1,nodreac(n3)) + fx*dt1
                        fthreac(2,nodreac(n3)) = fthreac(2,nodreac(n3)) + fy*dt1
                        fthreac(3,nodreac(n3)) = fthreac(3,nodreac(n3)) + fz*dt1
                      endif
                    endif!
!
!
                    a(1,n4)=a(1,n4)+fx
                    a(2,n4)=a(2,n4)+fy
                    a(3,n4)=a(3,n4)+fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n4) = fext(1,n4)+fx
                      fext(2,n4) = fext(2,n4)+fy
                      fext(3,n4) = fext(3,n4)+fz
                    endif
!
                    if (cptreac > 0) then
                      if (nodreac(n4)/=0) then
                        fthreac(1,nodreac(n4)) = fthreac(1,nodreac(n4)) + fx*dt1
                        fthreac(2,nodreac(n4)) = fthreac(2,nodreac(n4)) + fy*dt1
                        fthreac(3,nodreac(n4)) = fthreac(3,nodreac(n4)) + fz*dt1
                      endif
                    endif
!
!
                    tfextt=tfextt+dt1*(fx*(v(1,n1)+v(1,n2)+v(1,n3)+v(1,n4))&
                    &+fy*(v(2,n1)+v(2,n2)+v(2,n3)+v(2,n4))&
                    &+fz*(v(3,n1)+v(3,n2)+v(3,n3)+v(3,n4)))
!
                  else
                    ! true triangles.
                    nx = (x(2,n3)-x(2,n1))*(x(3,n3)-x(3,n2)) - (x(3,n3)-x(3,n1))*(x(2,n3)-x(2,n2))
                    ny = (x(3,n3)-x(3,n1))*(x(1,n3)-x(1,n2)) - (x(1,n3)-x(1,n1))*(x(3,n3)-x(3,n2))
                    nz = (x(1,n3)-x(1,n1))*(x(2,n3)-x(2,n2)) - (x(2,n3)-x(2,n1))*(x(1,n3)-x(1,n2))
                    fx = aa*nx*sixth
                    fy = aa*ny*sixth
                    fz = aa*nz*sixth
!
                    a(1,n1)=a(1,n1)+fx
                    a(2,n1)=a(2,n1)+fy
                    a(3,n1)=a(3,n1)+fz
                    if(ianim >0  .and.impl_s==0) then
                      fext(1,n1) = fext(1,n1)+fx
                      fext(2,n1) = fext(2,n1)+fy
                      fext(3,n1) = fext(3,n1)+fz
                    endif
!
                    if (cptreac > 0) then
                      if (nodreac(n1)/=0) then
                        fthreac(1,nodreac(n1)) = fthreac(1,nodreac(n1)) + fx*dt1
                        fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + fy*dt1
                        fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + fz*dt1
                      endif
                    endif
!
                    if(th_surf%pload_flag >0 ) then
                      nsegpl = nsegpl + 1
                      area = half*sqrt(nx*nx+ny*ny+nz*nz)
                      do ns=th_surf%pload_ksegs(nsegpl) +1,th_surf%pload_ksegs(nsegpl+1)
                        ksurf = th_surf%pload_segs(ns)
                        nseg_loadp(ksurf) = nseg_loadp(ksurf) + 1
                        fsavsurf(4,ksurf)= fsavsurf(4,ksurf) + aa
                        fsavsurf(5,ksurf)= fsavsurf(5,ksurf) + area
                      enddo
                    endif
!
                    a(1,n2) = a(1,n2)+fx
                    a(2,n2) = a(2,n2)+fy
                    a(3,n2) = a(3,n2)+fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n2) = fext(1,n2)+fx
                      fext(2,n2) = fext(2,n2)+fy
                      fext(3,n2) = fext(3,n2)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n2)/=0) then
                        fthreac(1,nodreac(n2)) = fthreac(1,nodreac(n2)) + fx*dt1
                        fthreac(2,nodreac(n2)) = fthreac(2,nodreac(n2)) + fy*dt1
                        fthreac(3,nodreac(n2)) = fthreac(3,nodreac(n2)) + fz*dt1
                      endif
                    endif
!
!
                    a(1,n3) = a(1,n3)+fx
                    a(2,n3) = a(2,n3)+fy
                    a(3,n3) = a(3,n3)+fz
                    if(ianim >0  .and.impl_s==0) then
                      fext(1,n3) = fext(1,n3)+fx
                      fext(2,n3) = fext(2,n3)+fy
                      fext(3,n3) = fext(3,n3)+fz
                    endif
!
                    if (cptreac > 0 ) then
                      if (nodreac(n3)/=0) then
                        fthreac(1,nodreac(n3)) = fthreac(1,nodreac(n3)) + fx*dt1
                        fthreac(2,nodreac(n3)) = fthreac(2,nodreac(n3)) + fy*dt1
                        fthreac(3,nodreac(n3)) = fthreac(3,nodreac(n3)) + fz*dt1
                      endif
                    endif
!
!
                    tfextt=tfextt+dt1*(fx*(v(1,n1)+v(1,n2)+v(1,n3)) + fy*(v(2,n1)+v(2,n2)+v(2,n3)) + fz*(v(3,n1)+v(3,n2)+v(3,n3)))

                  endif
                else
!        analyse 2d
                  ny      = -x(3,n2)+x(3,n1)
                  nz      =  x(2,n2)-x(2,n1)
                  fy      = aa*ny*half
                  fz      = aa*nz*half
!
                  a(2,n1) = a(2,n1)+fy
                  a(3,n1) = a(3,n1)+fz
!
                  a(2,n2) = a(2,n2)+fy
                  a(3,n2) = a(3,n2)+fz
!
                  if(ianim >0 .and.impl_s==0) then
                    fext(2,n1) = fext(2,n1)+fy
                    fext(3,n1) = fext(3,n1)+fz
!
                    fext(2,n2) = fext(2,n2)+fy
                    fext(3,n2) = fext(3,n2)+fz
                  endif
!
                  if (cptreac > 0) then
                    if(nodreac(n1)/=0) then
                      fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + fy*dt1
                      fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + fz*dt1
                    endif
                    if (nodreac(n2)/=0) then
                      fthreac(2,nodreac(n2)) = fthreac(2,nodreac(n2)) + fy*dt1
                      fthreac(3,nodreac(n2)) = fthreac(3,nodreac(n2)) + fz*dt1
                    endif
                  endif
!
                  if(th_surf%pload_flag >0 ) then
                    nsegpl = nsegpl + 1
                    area = half*sqrt(nx*nx+ny*ny+nz*nz)
                    do ns=th_surf%pload_ksegs(nsegpl) +1,th_surf%pload_ksegs(nsegpl+1)
                      ksurf = th_surf%pload_segs(ns)
                      nseg_loadp(ksurf) = nseg_loadp(ksurf) + 1
                      fsavsurf(4,ksurf)= fsavsurf(4,ksurf) + aa
                      fsavsurf(5,ksurf)= fsavsurf(5,ksurf) + area
                    enddo
                  endif
!
                  if(n2d==1)then
                    ax     = half*(x(2,n1)+x(2,n2))
                  else
                    ax     = one
                  endif
                  tfextt  = tfextt+dt1*(fy*(v(2,n1)+v(2,n2))+fz*(v(3,n1)+v(3,n2)))*ax

                endif
              endif
            enddo
!
!$OMP atomic
            tfext = tfext + tfextt
!$OMP end atomic
!
          else
!-------------------------
! code spmd parith/on
!-------------------------
            do nl=1,nconld
              n1  = ib(1,nl)
              n2  = ib(2,nl)
              n3  = ib(3,nl)
              n4  = ib(4,nl)
              n5  = ib(5,nl)
              idel = ib(8,nl)
              ifun = ib(9,nl)
              fcy = fac(1,nl)
              fcx = fac(2,nl)
              up_bound = 4
              ! -------------
              ! flush sky array to 0.
              if(n4==-1) then
                up_bound = 1
              else
                if(n2d==0)then
                  !  3d
                  if(n4/=0)then
                    up_bound = 4
                  else
                    up_bound = 3
                  endif
                else
                  ! 2d
                  up_bound = 2
                endif
              endif
              fsky(1:8,iadc(1:up_bound,nl))=zero
              ! -------------
!---------------------------------------
              if (ifun > 1) then
                isk = ib(2,nl)/10
                idir  = ib(2,nl)-10*isk
                if (idir<=3) then
                  disp = d(idir,n1)
                  vel  = v(idir,n1)
                elseif (idir<=6) then
                  disp = dr(idir-3,n1)
                  vel  = vr(idir-3,n1)
                endif
              endif
!----------------------------------------
              isens=0
              do k=1,nsensor
                if(ib(6,nl) == sensor_tab(k)%sens_id) isens=k
              enddo
              if(isens==0)then
                ts=tt
              else
                ts = tt - sensor_tab(isens)%tstart
                if(ts<zero) cycle
              endif
!
              if(n4==-1)then
!-------------------------
!        force concentree
!-------------------------
!!--------------------
                if(ifun == 1)then
                  !! time dependent abscisa
                  if(n_old/=n3.or.x_old/=ts) then
                    ismooth = 0
                    if (n3 > 0) ismooth = npc(2*nfunct+n3+1)
                    if (ismooth == 0) then
                      f1 = finter(n3,(ts-dt1)*fcx,npc,tf,dydx) !! current cycle
                      f2 = finter(n3,ts*fcx,npc,tf,dydx)       !! previous cycle
                    else if (ismooth > 0) then
                      f1 = finter_smooth(n3,(ts-dt1)*fcx,npc,tf,dydx)
                      f2 = finter_smooth(n3,ts*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,(ts-dt1)*fcx,f1)
                        call python_call_funct1d(python, fid,ts*fcx,f2)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n3
                    x_old = ts
                  endif
                else if(ifun == 2)then
                  !! displacement dependent abscisa
                  !!disp_old  !! previous cycle displacement
                  !!disp      !! current cycle displacement
                  disp_old = dpl0cld(idir,nl)
                  if(n_old/=n3.or.x_old/=disp) then
                    ismooth = 0
                    if (n3 > 0) ismooth = npc(2*nfunct+n3+1)
                    if (ismooth == 0) then
                      f1 = finter(n3,disp_old*fcx,npc,tf,dydx)
                      f2 = finter(n3,disp*fcx,npc,tf,dydx)
                    else if (ismooth > 0) then
                      f1 = finter_smooth(n3,disp_old*fcx,npc,tf,dydx)
                      f2 = finter_smooth(n3,disp*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,disp_old*fcx,f1)
                        call python_call_funct1d(python, fid,disp*fcx,f2)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n3
                    x_old = disp
                  endif
                else if(ifun == 3)then
                  !! velocity dependent abscisa
                  !!vel_old   !! previous cycle velocity
                  !!vel       !! current cycle velocity
                  vel_old  = vel0cld(idir,nl)
                  if(n_old/=n3.or.x_old/=vel) then
                    ismooth = 0
                    if (n3 > 0) ismooth = npc(2*nfunct+n3+1)
                    if (ismooth == 0) then
                      f1 = finter(n3,vel_old*fcx,npc,tf,dydx)
                      f2 = finter(n3,vel*fcx,npc,tf,dydx)
                    else if (ismooth > 0) then
                      f1 = finter_smooth(n3,vel_old*fcx,npc,tf,dydx)
                      f2 = finter_smooth(n3,vel*fcx,npc,tf,dydx)
                    else
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,vel_old*fcx,f1)
                        call python_call_funct1d(python, fid,vel*fcx,f2)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n3
                    x_old = vel
                  endif
                endif
!!--------------------
                a0  = fcy*f1
                aa  = fcy*f2
                isk = ib(2,nl)/10
                n2  = ib(2,nl)-10*isk
                if(n2<=3)then
                  if(n2d/=1)then
                    axi=one
                  else
                    axi=x(2,n1)
                  endif
                  if(isk<=1)then
                    fsky(n2,iadc(1,nl)) = aa
                    if(ianim >0 .and.impl_s==0) then
                      fext(n2,n1)=fext(n2,n1)+aa
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(n2,nodreac(n1)) = fthreac(n2,nodreac(n1)) + aa*dt1
                      endif
                    endif
!
                    tfexc = tfexc+half*(a0+aa)*v(n2,n1)*axi
                  else
                    k1  = 3*n2-2
                    k2  = 3*n2-1
                    k3  = 3*n2
                    vv  = skew(k1,isk)*v(1,n1)+skew(k2,isk)*v(2,n1)+ skew(k3,isk)*v(3,n1)
                    iad = iadc(1,nl)
                    fsky(1,iad)  = skew(k1,isk)*aa
                    fsky(2,iad)  = skew(k2,isk)*aa
                    fsky(3,iad)  = skew(k3,isk)*aa
                    if(ianim >0  .and.impl_s==0) then
                      fext(1,n1) = fext(1,n1)+skew(k1,isk)*aa
                      fext(2,n1) = fext(2,n1)+skew(k2,isk)*aa
                      fext(3,n1) = fext(3,n1)+skew(k3,isk)*aa
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(1,nodreac(n1)) = fthreac(1,nodreac(n1)) + skew(k1,isk)*aa*dt1
                        fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + skew(k2,isk)*aa*dt1
                        fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + skew(k3,isk)*aa*dt1
                      endif
                    endif
!
                    tfexc = tfexc+half*(a0+aa)*vv*axi
                  endif
                elseif(n2<=6)then
                  n2 = n2 - 3
                  if(isk<=1)then
                    fsky(3+n2,iadc(1,nl)) = aa
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(n2+3,nodreac(n1)) = fthreac(n2+3,nodreac(n1)) + aa*dt1
                      endif
                    endif
!
                    tfexc=tfexc+half*(a0+aa)*vr(n2,n1)
                  else
                    k1  = 3*n2-2
                    k2  = 3*n2-1
                    k3  = 3*n2
                    vv  = skew(k1,isk)*vr(1,n1)+skew(k2,isk)*vr(2,n1)+ skew(k3,isk)*vr(3,n1)
                    iad = iadc(1,nl)
                    fsky(4,iad) = skew(k1,isk)*aa
                    fsky(5,iad) = skew(k2,isk)*aa
                    fsky(6,iad) = skew(k3,isk)*aa
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(4,nodreac(n1)) = fthreac(4,nodreac(n1)) + skew(k1,isk)*aa*dt1
                        fthreac(5,nodreac(n1)) = fthreac(5,nodreac(n1)) + skew(k2,isk)*aa*dt1
                        fthreac(6,nodreac(n1)) = fthreac(6,nodreac(n1)) + skew(k3,isk)*aa*dt1
                      endif
                    endif
!
                    tfexc=tfexc+half*(a0+aa)*vv
                  endif
                endif
!
              else
!--------------------
!         pression
!--------------------

                if(idel > 0 ) cycle  ! segment deleted
!
                if(ifun == 1)then
                  !! time dependent abscisa
                  if(n_old/=n5.or.x_old/=ts) then
                    ismooth = 0
                    if (n5 > 0) ismooth = npc(2*nfunct+n5+1)
                    if (ismooth == 0) then
                      f1 = finter(n5,ts*fcx,npc,tf,dydx)
                    elseif(ismooth > 0) then
                      f1 = finter_smooth(n5,ts*fcx,npc,tf,dydx)
                    elseif(ismooth < 0) then
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,ts*fcx,f1)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n5
                    x_old = ts
                  endif
                elseif(ifun == 2)then
                  !! displacement dependent abscisa
                  !!disp      !! current cycle displacement
                  if(n_old/=n5.or.x_old/=disp) then
                    ismooth = 0
                    if (n5 > 0) ismooth = npc(2*nfunct+n5+1)
                    if (ismooth == 0) then
                      f1 = finter(n5,disp*fcx,npc,tf,dydx)
                    elseif(ismooth > 0) then
                      f1 = finter_smooth(n5,disp*fcx,npc,tf,dydx)
                    elseif(ismooth < 0) then
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,disp*fcx,f1)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n5
                    x_old = disp
                  endif
                elseif(ifun == 3)then
                  !! velocity dependent abscisa
                  !!vel       !! current cycle velocity
                  if(n_old/=n5.or.x_old/=vel) then
                    ismooth = 0
                    if (n5 > 0) ismooth = npc(2*nfunct+n5+1)
                    if (ismooth == 0) then
                      f1 = finter(n5,vel*fcx,npc,tf,dydx)
                    elseif(ismooth > 0) then
                      f1 = finter_smooth(n5,vel*fcx,npc,tf,dydx)
                    elseif(ismooth < 0) then
                      if(present(python)) then
                        fid = -ismooth ! the id the python function is saved in the position of ismooth in the npc array
                        call python_call_funct1d(python, fid,vel*fcx,f1)
                      endif
                    endif ! if (ismooth == 0)
                    n_old = n5
                    x_old = vel
                  endif
                endif
!
                aa = fcy*f1
                if(n2d==0)then
                  !  3d
                  if(n4/=0)then
                    nx = (x(2,n3)-x(2,n1))*(x(3,n4)-x(3,n2)) - (x(3,n3)-x(3,n1))*(x(2,n4)-x(2,n2))
                    ny = (x(3,n3)-x(3,n1))*(x(1,n4)-x(1,n2)) - (x(1,n3)-x(1,n1))*(x(3,n4)-x(3,n2))
                    nz = (x(1,n3)-x(1,n1))*(x(2,n4)-x(2,n2)) - (x(2,n3)-x(2,n1))*(x(1,n4)-x(1,n2))
!
                    fx = aa*nx*one_over_8
                    fy = aa*ny*one_over_8
                    fz = aa*nz*one_over_8
!
                    iad = iadc(1,nl)
                    fsky(1,iad) = fx
                    fsky(2,iad) = fy
                    fsky(3,iad) = fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n1) = fext(1,n1)+fx
                      fext(2,n1) = fext(2,n1)+fy
                      fext(3,n1) = fext(3,n1)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(1,nodreac(n1)) = fthreac(1,nodreac(n1)) + fx*dt1
                        fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + fy*dt1
                        fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + fz*dt1
                      endif
                    endif
!
                    if(th_surf%pload_flag >0 ) then
                      nsegpl = nsegpl + 1
                      area = half*sqrt(nx*nx+ny*ny+nz*nz)
                      do ns=th_surf%pload_ksegs(nsegpl) +1,th_surf%pload_ksegs(nsegpl+1)
                        ksurf = th_surf%pload_segs(ns)
                        nseg_loadp(ksurf) = nseg_loadp(ksurf) + 1
                        fsavsurf(4,ksurf)= fsavsurf(4,ksurf) + aa
                        fsavsurf(5,ksurf)= fsavsurf(5,ksurf) + area
                      enddo
                    endif
!
                    iad = iadc(2,nl)
                    fsky(1,iad) = fx
                    fsky(2,iad) = fy
                    fsky(3,iad) = fz
                    if(ianim >0  .and.impl_s==0) then
                      fext(1,n2) = fext(1,n2)+fx
                      fext(2,n2) = fext(2,n2)+fy
                      fext(3,n2) = fext(3,n2)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n2)/=0) then
                        fthreac(1,nodreac(n2)) = fthreac(1,nodreac(n2)) + fx*dt1
                        fthreac(2,nodreac(n2)) = fthreac(2,nodreac(n2)) + fy*dt1
                        fthreac(3,nodreac(n2)) = fthreac(3,nodreac(n2)) + fz*dt1
                      endif
                    endif
!
!
                    iad = iadc(3,nl)
                    fsky(1,iad) = fx
                    fsky(2,iad) = fy
                    fsky(3,iad) = fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n3) = fext(1,n3)+fx
                      fext(2,n3) = fext(2,n3)+fy
                      fext(3,n3) = fext(3,n3)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n3)/=0) then
                        fthreac(1,nodreac(n3)) = fthreac(1,nodreac(n3)) + fx*dt1
                        fthreac(2,nodreac(n3)) = fthreac(2,nodreac(n3)) + fy*dt1
                        fthreac(3,nodreac(n3)) = fthreac(3,nodreac(n3)) + fz*dt1
                      endif
                    endif
!
!
                    iad = iadc(4,nl)
                    fsky(1,iad) = fx
                    fsky(2,iad) = fy
                    fsky(3,iad) = fz
                    if(ianim >0  .and.impl_s==0) then
                      fext(1,n4) = fext(1,n4)+fx
                      fext(2,n4) = fext(2,n4)+fy
                      fext(3,n4) = fext(3,n4)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n4)/=0) then
                        fthreac(1,nodreac(n4)) = fthreac(1,nodreac(n4)) + fx*dt1
                        fthreac(2,nodreac(n4)) = fthreac(2,nodreac(n4)) + fy*dt1
                        fthreac(3,nodreac(n4)) = fthreac(3,nodreac(n4)) + fz*dt1
                      endif
                    endif
!
!
                    tfextt=tfextt+dt1*(fx*(v(1,n1)+v(1,n2)+v(1,n3)+v(1,n4))&
                    &+fy*(v(2,n1)+v(2,n2)+v(2,n3)+v(2,n4))&
                    &+fz*(v(3,n1)+v(3,n2)+v(3,n3)+v(3,n4)))

                  else
                    ! true triangles.
                    nx  = (x(2,n3)-x(2,n1))*(x(3,n3)-x(3,n2)) - (x(3,n3)-x(3,n1))*(x(2,n3)-x(2,n2))
                    ny  = (x(3,n3)-x(3,n1))*(x(1,n3)-x(1,n2)) - (x(1,n3)-x(1,n1))*(x(3,n3)-x(3,n2))
                    nz  = (x(1,n3)-x(1,n1))*(x(2,n3)-x(2,n2)) - (x(2,n3)-x(2,n1))*(x(1,n3)-x(1,n2))
                    fx  = aa*nx*sixth
                    fy  = aa*ny*sixth
                    fz  = aa*nz*sixth
!
                    iad = iadc(1,nl)
                    fsky(1,iad) = fx
                    fsky(2,iad) = fy
                    fsky(3,iad) = fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n1) = fext(1,n1)+fx
                      fext(2,n1) = fext(2,n1)+fy
                      fext(3,n1) = fext(3,n1)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n1)/=0) then
                        fthreac(1,nodreac(n1)) = fthreac(1,nodreac(n1)) + fx*dt1
                        fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + fy*dt1
                        fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + fz*dt1
                      endif
                    endif
!
                    if(th_surf%pload_flag >0 ) then
                      nsegpl = nsegpl + 1
                      area = half*sqrt(nx*nx+ny*ny+nz*nz)
                      do ns=th_surf%pload_ksegs(nsegpl) +1,th_surf%pload_ksegs(nsegpl+1)
                        ksurf = th_surf%pload_segs(ns)
                        nseg_loadp(ksurf) = nseg_loadp(ksurf) + 1
                        fsavsurf(4,ksurf)= fsavsurf(4,ksurf) + aa
                        fsavsurf(5,ksurf)= fsavsurf(5,ksurf) + area
                      enddo
                    endif
!
                    iad = iadc(2,nl)
                    fsky(1,iad) = fx
                    fsky(2,iad) = fy
                    fsky(3,iad) = fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n2) = fext(1,n2)+fx
                      fext(2,n2) = fext(2,n2)+fy
                      fext(3,n2) = fext(3,n2)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n2)/=0) then
                        fthreac(1,nodreac(n2)) = fthreac(1,nodreac(n2)) + fx*dt1
                        fthreac(2,nodreac(n2)) = fthreac(2,nodreac(n2)) + fy*dt1
                        fthreac(3,nodreac(n2)) = fthreac(3,nodreac(n2)) + fz*dt1
                      endif
                    endif
!
!
                    iad = iadc(3,nl)
                    fsky(1,iad) = fx
                    fsky(2,iad) = fy
                    fsky(3,iad) = fz
                    if(ianim >0 .and.impl_s==0) then
                      fext(1,n3) = fext(1,n3)+fx
                      fext(2,n3) = fext(2,n3)+fy
                      fext(3,n3) = fext(3,n3)+fz
                    endif
!
                    if (cptreac > 0) then
                      if(nodreac(n3)/=0) then
                        fthreac(1,nodreac(n3)) = fthreac(1,nodreac(n3)) + fx*dt1
                        fthreac(2,nodreac(n3)) = fthreac(2,nodreac(n3)) + fy*dt1
                        fthreac(3,nodreac(n3)) = fthreac(3,nodreac(n3)) + fz*dt1
                      endif
                    endif
!
!
                    tfextt=tfextt+dt1*(fx*(v(1,n1)+v(1,n2)+v(1,n3)) + fy*(v(2,n1)+v(2,n2)+v(2,n3)) + fz*(v(3,n1)+v(3,n2)+v(3,n3)))
                  endif
                else
!        analyse 2d
                  ny  = -x(3,n2)+x(3,n1)
                  nz  =  x(2,n2)-x(2,n1)
!
                  fy  = aa*ny*half
                  fz  = aa*nz*half
!
                  iad = iadc(1,nl)
                  fsky(2,iad) = fy
                  fsky(3,iad) = fz
!
                  iad = iadc(2,nl)
                  fsky(2,iad) = fy
                  fsky(3,iad) = fz
!
                  if(ianim >0 .and.impl_s==0) then
                    fext(2,n1) = fext(2,n1)+fy
                    fext(3,n1) = fext(3,n1)+fz
!
                    fext(2,n2) = fext(2,n2)+fy
                    fext(3,n2) = fext(3,n2)+fz
                  endif
!
                  if (cptreac > 0) then
                    if(nodreac(n1)/=0) then
                      fthreac(2,nodreac(n1)) = fthreac(2,nodreac(n1)) + fy*dt1
                      fthreac(3,nodreac(n1)) = fthreac(3,nodreac(n1)) + fz*dt1
                    endif
                    if (nodreac(n2)/=0) then
                      fthreac(2,nodreac(n2)) = fthreac(2,nodreac(n2)) + fy*dt1
                      fthreac(3,nodreac(n2)) = fthreac(3,nodreac(n2)) + fz*dt1
                    endif
                  endif
!
                  if(th_surf%pload_flag >0 ) then
                    nsegpl = nsegpl + 1
                    area = half*sqrt(nx*nx+ny*ny+nz*nz)
                    do ns=th_surf%pload_ksegs(nsegpl) +1,th_surf%pload_ksegs(nsegpl+1)
                      ksurf = th_surf%pload_segs(ns)
                      nseg_loadp(ksurf) = nseg_loadp(ksurf) + 1
                      fsavsurf(4,ksurf)= fsavsurf(4,ksurf) + aa
                      fsavsurf(5,ksurf)= fsavsurf(5,ksurf) + area
                    enddo
                  endif
!
                  if(n2d==1)then
                    ax=half*(x(2,n1)+x(2,n2))
                  else
                    ax=one
                  endif
                  tfextt=tfextt+dt1*(fy*(v(2,n1)+v(2,n2))+fz*(v(3,n1)+v(3,n2)))*ax
                endif
              endif
            enddo
!
            tfext = tfext + tfextt
!
          endif
          return
        end subroutine force
      end module force_mod

