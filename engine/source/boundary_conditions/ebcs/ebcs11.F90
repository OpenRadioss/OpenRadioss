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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
      !||====================================================================
      !||    ebcs11                ../engine/source/boundary_conditions/ebcs/ebcs11.F90
      !||--- called by ------------------------------------------------------
      !||    ebcs_main             ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||--- calls      -----------------------------------------------------
      !||    finter                ../engine/source/tools/curve/finter.F
      !||    finter_smooth         ../engine/source/tools/curve/finter_smooth.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||    ebcs_mod              ../common_source/modules/boundary_conditions/ebcs_mod.F90
      !||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    multi_fvm_mod         ../common_source/modules/ale/multi_fvm_mod.F90
      !||    python_funct_mod      ../common_source/modules/python_mod.F90
      !||    segvar_mod            ../engine/share/modules/segvar_mod.F
      !||    sensor_mod            ../common_source/modules/sensor_mod.F90
      !||    th_surf_mod           ../common_source/modules/interfaces/th_surf_mod.F
      !||====================================================================
      subroutine ebcs11(nseg,iseg,segvar, &
                        v,w,x, &
                        liste,nod,irect,ielem,iface, &
                        la,ms,stifn,ebcs,iparg,elbuf_tab,ixq,ixs,ixtg, &
                        fsavsurf,time,iparit,dt1, &
                        numels, numelq, numeltg, numnod, nparg, ngroup, nixs, nixq, nixtg, nsurf, iale, n2d, &
                        nfunct, npc, tf ,snpc, stf, python, &
                        nsensor, sensor_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
      use ebcs_mod
      use elbufdef_mod
      use multi_fvm_mod
      use segvar_mod
      use th_surf_mod , only : th_surf_num_channel
      use constant_mod , only : zero, em06, one, third, fourth, half, two, three, four, em20
      use sensor_mod, only : sensor_str_
      use python_funct_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   include files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: n2d !< 2d/3d flag
      integer,intent(in) :: iale !< ale flag
      integer,intent(in) :: numeltg, numels, numelq, numnod, nparg, ngroup, nixs, nixq, nixtg, nsurf !< array sizes
      my_real, intent(in) :: dt1 !< time step
      my_real, intent(in) :: time !< simulation time
      integer,intent(in) :: iparit !< /parith/on flag
      my_real, intent(inout) :: fsavsurf(th_surf_num_channel,nsurf)
      integer,intent(in) :: nseg,nod,iseg(nseg),liste(nod),irect(4,nseg),ielem(nseg),iface(nseg)
      integer,intent(in) :: ixq(nixq,numelq),ixs(nixs,numels),ixtg(nixtg,numeltg)
      integer,intent(in) :: nfunct    !< number of user functions
      integer,intent(in) :: snpc      !< size of npc array
      integer,intent(in) :: npc(snpc) !< function working array
      integer,intent(in) :: stf       !< array size of array tf
      integer,intent(in) :: tf(stf)   !< function working array
      my_real,intent(inout) :: ms(numnod) !< nodal mass
      my_real,intent(inout) :: v(3,numnod),w(3,numnod),x(3,numnod) !< mat. velocity, grid velocity, coordinates
       my_real,intent(inout) :: la(3,nod),stifn(numnod)
      type(t_ebcs_propergol), intent(inout) :: ebcs !< ebcs data structure
      integer :: iparg(nparg,ngroup) !< data for all group of elems
      type(elbuf_struct_), target, dimension(ngroup) :: elbuf_tab !< element buffer
      type(t_segvar),intent(inout) :: segvar !< ghost cell data
      integer,intent(in) :: nsensor !< number of sensor(s) in input file
      type (sensor_str_) ,dimension(nsensor) ,intent(in) :: sensor_tab  !< data stucture for sensors
      type (python_), intent(in) :: python !< may be needed for user functions
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      type(g_bufel_), pointer :: gbuf
      type(l_bufel_)  ,pointer :: lbuf
      integer ii,is,kk,kseg,nn(4),nng(4),num,kty,klt,mft,ngrp,iloc,ivoi,idx(6),ix(4)
      integer icf_2d(2,4), icf_3d(4,6), jj, isubmat, ipos, mtn
      integer isolnod
      my_real npt
      my_real orient,rho,vol,volg,mass,mass_face
      my_real x13,y13,z13,x24,y24,z24
      my_real v0(3,nod)
      my_real xn, yn, zn, vold, vnew, padj, eadj, tadj
      my_real pp,ee,tt,ssp,vel_front,fac1,fac2
      my_real param_a, param_n, param_q, surf,phase_alpha(21),phase_rho(21), phase_eint(21)
      my_real face_force
      my_real :: param_rho0s  !< initial propergol density
      my_real :: dmass_g     !< mass increment (burnt propergol)
      my_real :: dvol_s      !< burnt volume
      my_real :: tmp(3)
      my_real :: fscaleX, fscaleY, gscaleX, gscaleY, hscaleX, hscaleY
      my_real :: tstart
      my_real :: ff, gg, hh
      my_real :: dydx

      logical bfound
      type(buf_mat_)  ,pointer :: mbuf
      integer :: param_surf_id !< ebcs surface identifier
      integer :: nbsubmat
      integer :: sensor_iid, sensor_uid, ffunc_id, gfunc_id, hfunc_id
      integer :: ismooth_f, ismooth_g, ismooth_h

      data icf_2d  /1,2,2,3,3,4,4,1/
      data icf_3d  /1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/

      INTEGER N0PHAS
      PARAMETER (N0PHAS = 04)

      INTEGER NVPHAS
      PARAMETER (NVPHAS = 23)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   External Functions
! ----------------------------------------------------------------------------------------------------------------------
      my_real, external :: FINTER,FINTER_SMOOTH
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------

      !--- user parameters
      param_a = ebcs%a
      param_n = ebcs%n
      param_q = ebcs%q
      param_rho0s = ebcs%rho0s
      param_surf_id = ebcs%surf_id
      sensor_uid = ebcs%sensor_id !user id
      isubmat =  ebcs%submat_id
      ffunc_id = ebcs%ffunc_id
      gfunc_id = ebcs%gfunc_id
      hfunc_id = ebcs%hfunc_id
      fscaleX = ebcs%fscaleX
      fscaleY = ebcs%fscaleY
      gscaleX = ebcs%gscaleX
      gscaleY = ebcs%gscaleY
      hscaleX = ebcs%hscaleX
      hscaleY = ebcs%hscaleY

      phase_alpha(:) = zero
      phase_eint(:) = zero
      phase_rho(:) = zero

      !sensor
      sensor_iid = 0
      do kk=1,nsensor
        if(sensor_uid == sensor_tab(kk)%sens_id) sensor_iid=kk !internal id
      enddo
      if(sensor_iid == 0)then
        tstart=time
      else
        tstart = time - sensor_tab(sensor_iid)%tstart
        if(tstart < zero)return
      endif

      !function : smooth option (interpolation)
      ismooth_f = 0
      ismooth_g = 0
      ismooth_h = 0
      if (ffunc_id > 0) ismooth_f = npc(2*nfunct+ffunc_id+1)
      if (gfunc_id > 0) ismooth_g = npc(2*nfunct+gfunc_id+1)
      if (hfunc_id > 0) ismooth_h = npc(2*nfunct+hfunc_id+1)

      !function f
      if (ffunc_id > 0) then
        if (ismooth_f == 0) then
          ff = fscaley*finter(ffunc_id,tstart*fscalex,npc,tf,dydx)
        else if (ismooth_f > 0) then
          ff = fscalex*finter_smooth(ffunc_id,tstart*fscaley,npc,tf,dydx)
        else
          ismooth_f = -ismooth_f ! the id the python function is saved in the position of ismooth in the npc array
          call python_call_funct1d(python, ismooth_f,tstart*fscalex, ff)
          ff = fscaley * ff
        endif
      else
        ff = fscaley
      endif


      !--- nodal velocities at face nodes
      do ii=1,nod
        num=liste(ii)
        v0(1,ii)=v(1,num)
        v0(2,ii)=v(2,num)
        v0(3,ii)=v(3,num)
      enddo
      if(iale == 1)then
        do ii=1,nod
          num=liste(ii)
          v0(1,ii)=v0(1,ii)-w(1,num)
          v0(2,ii)=v0(1,ii)-w(2,num)
          v0(3,ii)=v0(1,ii)-w(3,num)
        enddo
      endif

      !--- reset working array for internal forces
      do ii=1,nod
        num=liste(ii)
        la(1,ii)=zero
        la(2,ii)=zero
        la(3,ii)=zero
      enddo

      do is=1,nseg
        kseg=abs(iseg(is))
        orient=float(iseg(is)/kseg)
        !---outward normal
        if(n2d == 0)then
          jj = iface(is)
          ix(1)=ixs(icf_3d(1,jj)+1,ielem(is))
          ix(2)=ixs(icf_3d(2,jj)+1,ielem(is))
          ix(3)=ixs(icf_3d(3,jj)+1,ielem(is))
          ix(4)=ixs(icf_3d(4,jj)+1,ielem(is))
          x13=x(1,ix(3))-x(1,ix(1))
          y13=x(2,ix(3))-x(2,ix(1))
          z13=x(3,ix(3))-x(3,ix(1))
          x24=x(1,ix(4))-x(1,ix(2))
          y24=x(2,ix(4))-x(2,ix(2))
          z24=x(3,ix(4))-x(3,ix(2))
          xn=y13*z24-z13*y24
          yn=z13*x24-x13*z24
          zn=x13*y24-y13*x24
          fac2=one/sqrt(xn*xn+yn*yn+zn*zn)
          xn = xn*fac2
          yn = yn*fac2
          zn = zn*fac2
          surf = half/fac2
          if(ix(4) == ix(3))then ; npt=THREE;fac1=third; else; npt=FOUR;fac1=fourth; endif
        else
          fac1=half
          npt=two
          jj = iface(is)
          if(numeltg > 0)then
            ix(1)  = ixtg(icf_2d(1,jj)+1,ielem(is))
            ix(2)  = ixtg(icf_2d(2,jj)+1,ielem(is))
          else
            ix(1)  = ixq(icf_2d(1,jj)+1,ielem(is))
            ix(2)  = ixq(icf_2d(2,jj)+1,ielem(is))
          endif
          xn = zero
          yn = -(-x(3,ix(2))+x(3,ix(1)))
          zn =  (-x(2,ix(2))+x(2,ix(1)))
          fac2 = one/sqrt(yn*yn+zn*zn)
          yn=yn*fac2
          zn=zn*fac2
          surf = one/fac2
        endif
        tmp(1:3) = zero
        nn(1)=irect(1,is)
        nn(2)=irect(2,is)
        nn(3)=irect(3,is)
        nn(4)=irect(4,is)
        nng(1:4)=liste(nn(1:4))
        do kk=1,INT(npt)
          tmp(1) = tmp(1) + v0(nn(kk),kk)
          tmp(2) = tmp(2) + v0(nn(kk),kk)
          tmp(3) = tmp(3) + v0(nn(kk),kk)
        enddo
        vnew = fac1 * (tmp(1)*xn + tmp(2)*yn + tmp(3)*zn)
        !-- adjacent state
        bfound = .false.
        ivoi = ielem(is)
        do ngrp=1,ngroup
          kty = iparg(5,ngrp)
          klt = iparg(2,ngrp)
          mft = iparg(3,ngrp)
          isolnod = iparg(28,ngrp)
          if(n2d == 0)then
            if(kty /= 1)cycle
          else
            if(kty == 2)then
              isolnod=4
            elseif(kty == 7)then
              isolnod=3
            else
              cycle
            endif
          endif
           if (ivoi <= klt+mft)then
             bfound = .true.
             exit
           endif
        enddo
        if(.not.bfound)cycle !next i
        gbuf => elbuf_tab(ngrp)%gbuf
        lbuf => elbuf_tab(ngrp)%bufly(1)%lbuf(1,1,1)
        mtn = iparg(1,ngrp)
        !adjacent pressure
        iloc = ivoi-mft-1
        do kk=1,6
          idx(kk) = klt*(kk-1)
        enddo
        padj = -third*(gbuf%sig(idx(1)+iloc+1) + gbuf%sig(idx(2)+iloc+1) + gbuf%sig(idx(3)+iloc+1))
        eadj = gbuf%eint(iloc+1)
        tadj= gbuf%temp(iloc+1)
        !density
        rho = gbuf%rho(iloc+1)
        vol = gbuf%vol(iloc+1)
        volg=vol ! global volume (for multimaterial case)
        mass = vol * rho
        !adjacent sound speed
        ssp = lbuf%ssp(iloc+1)
        !volume fracions and submat state
        if(mtn == 51)then
          mbuf => elbuf_tab(ngrp)%bufly(1)%mat(1,1,1)
          nbsubmat = 4
          do isubmat=1,nbsubmat
            !volume fraction
            ipos = 1
            kk = (n0phas + (isubmat-1)*nvphas +ipos-1) * klt  +  iloc+1
            phase_alpha(isubmat) = mbuf%var(kk)
            !mass density
            ipos = 9
            kk = (n0phas + (isubmat-1)*nvphas +ipos-1) * klt  +  iloc+1
            phase_rho(isubmat) = mbuf%var(kk)
            !energy density
            ipos = 8
            kk = (n0phas + (isubmat-1)*nvphas +ipos-1) * klt  +  iloc+1
            phase_eint(isubmat) = mbuf%var(kk)
          enddo
          isubmat = ebcs%submat_id
          vol = volg * phase_alpha(isubmat) ! burnt gas is supposed to be submat #isubmat
          if(vol == zero)vol = em06*volg !if burnt gas not present at initial time (we must trigger burnt gas present for sigeps51 subroutine)
          mass = vol * phase_rho(isubmat)
        endif
        !-- formulation
        !burnt propergol volume
        pp = padj
        ee = eadj
        tt = tadj
         !adjacent elem
        if(pp <= zero)then
          vel_front = zero
          dvol_s = zero
          dmass_g = zero
        else

          !function g
          if (gfunc_id > 0) then
            if (ismooth_g == 0) then
              gg = gscaley*finter(gfunc_id,tt*gscalex,npc,tf,dydx)
            else if (ismooth_g > 0) then
              gg = gscalex*finter_smooth(gfunc_id,tt*gscaley,npc,tf,dydx)
            else
              ismooth_g = -ismooth_g ! the id the python function is saved in the position of ismooth in the npc array
              call python_call_funct1d(python, ismooth_g, tt*gscalex, gg)
              gg = gscaley * gg
            endif
          else
            gg = gscaley
          endif

          !function h
          if (hfunc_id > 0) then
            if (ismooth_h == 0) then
              hh = hscaley*finter(hfunc_id,pp*hscalex,npc,tf,dydx)
            else if (ismooth_h > 0) then
              hh = hscalex*finter_smooth(hfunc_id,pp*hscaley,npc,tf,dydx)
            else
              ismooth_h = -ismooth_h ! the id the python function is saved in the position of ismooth in the npc array
              call python_call_funct1d(python, ismooth_h, pp*hscalex, hh)
              hh = hscaley * hh
            endif
          else
            hh = hscaley
          endif

          !ff : f(t), default = 1.0
          !gg : g(T), default = 1.0
          !hh : h(P), default = 1.0
          vel_front = ff*gg*hh*param_a*exp(param_n*log(pp))   !extended Vieille's law
          dvol_s = surf*vel_front*dt1
          dmass_g = dvol_s * param_rho0s

        endif

        vold = ebcs%vold(is)
        ebcs%vold(is) = vel_front
        if(time == zero) then
            vold = zero
        endif


        ! ghost cell update ( upwind/aconve() )
        if(mtn == 51 )then
          ! in multimaterial, burnt gas may be not present at initial time.
          isubmat = ebcs%submat_id
          mbuf => elbuf_tab(ngrp)%bufly(1)%mat(1,1,1)
          !init volume fraction to non zero (submaterial presence triggered inside the cell)
          ipos = 1
          kk = (n0phas + (isubmat-1)*nvphas +ipos-1) * klt  +  iloc+1
          mbuf%var(kk) = vol/volg  !initialize to non zero for sigeps51 subroutine
          segvar%phase_alpha(isubmat,kseg) = mbuf%var(kk)

          !volume (must be initialize for conveective fluxes, otherwise mass en energy is rho*0 and rho.e.0)
          ipos = 11
          kk = (n0phas + (isubmat-1)*nvphas +ipos-1) * klt  +  iloc+1
          mbuf%var(kk) = vol
          !no related entry in segvar

          !mass density
          ipos = 9
          kk = (n0phas + (isubmat-1)*nvphas +ipos-1) * klt  +  iloc+1
          mbuf%var(kk) = (mass + dmass_g)/vol
          segvar%phase_rho(isubmat,kseg)=mbuf%var(kk)

          !energy density
          ipos = 8
          kk = (n0phas + (isubmat-1)*nvphas +ipos-1) * klt  +  iloc+1
          mbuf%var(kk) = ( mbuf%var(kk)*vol + param_q*param_rho0s*dvol_s)/vol
          segvar%phase_eint(isubmat,kseg)=mbuf%var(kk)

          gbuf%eint(iloc+1) = phase_eint(1)*phase_alpha(1) + &
                              phase_eint(2)*phase_alpha(2) + &
                              phase_eint(3)*phase_alpha(3) + &
                              phase_eint(4)*phase_alpha(4)
                              
          gbuf%rho(iloc+1)  = phase_rho(1)*phase_alpha(1) + &
                              phase_rho(2)*phase_alpha(2) + &
                              phase_rho(3)*phase_alpha(3) + &
                              phase_rho(4)*phase_alpha(4)

          segvar%rho(kseg)=gbuf%rho(iloc+1)  !param_rho0s
          segvar%eint(kseg)=gbuf%eint(iloc+1) !param_q*param_rho0s

        else
          gbuf%rho(iloc+1) = (mass + dmass_g)/vol
          gbuf%eint(iloc+1) = (gbuf%eint(iloc+1)*vol + param_q*param_rho0s*dvol_s)/vol
          segvar%rho(kseg)=gbuf%rho(iloc+1)  !param_rho0s
          segvar%eint(kseg)=gbuf%eint(iloc+1) !param_q*param_rho0s
        endif

        !-- expand pressure to face nodes
        face_force = pp*surf                                        !pp for equilibrium
        !mass = mass + dmass_g
        mass_face = mass*(npt*one)/(isolnod*one)
        !vold = zero
        if(dt1 > zero)face_force = face_force + (vel_front) * mass_face / dt1   !pp= pp+ dp to input corresponding propergol
        !expand pressure loading to segment nodes
        !face_force = pp*surf
        do kk=1,INT(npt)
          la(1,nn(kk)) = la(1,nn(kk)) - fac1* (face_force) * xn
          la(2,nn(kk)) = la(2,nn(kk)) - fac1* (face_force) * yn
          la(3,nn(kk)) = la(3,nn(kk)) - fac1* (face_force) * zn
          if(dt1 > zero)then
            if (ms(nng(kk)) > em20)then
              stifn(nng(kk))=stifn(nng(kk))+(two*(surf*rho*ssp)**2)/ms(nng(kk))
            endif
          end if
        enddo
         !/th/surf (massflow, velocity, pressure)
         fsavsurf(2,param_surf_id) = fsavsurf(2,param_surf_id) + rho*surf*vel_front     !rho.s.u = dm/dt
         fsavsurf(3,param_surf_id) = fsavsurf(3,param_surf_id) + surf*vel_front         !s.u
         fsavsurf(4,param_surf_id) = fsavsurf(4,param_surf_id) + surf*padj              !s.p
         fsavsurf(6,param_surf_id) = fsavsurf(6,param_surf_id) + rho*surf*vel_front*dt1 ! m<-m+dm (cumulative)
         ! -------------
         ! for parith/on option : update forces in fsky array (specific assembly)
         if(iparit > 0) then
          ! do not update reaction force as coupling with inter1 is expected : vn_fluid = vn_wall
          ! do kk=1,npt
          !   adress = elem_adress(kk,is) ! adress of fsky array for element is and node kk
          !   fsky(1,adress) = -face_force*xn*fac1
          !   fsky(2,adress) = -face_force*yn*fac1
          !   fsky(3,adress) = -face_force*zn*fac1
          !   fsky(4:8,adress) = zero
          ! enddo
         endif
         ! -------------
      enddo

      ! numerical staggered scheme : vitesse.F ;  v[n+1] = v[n] + int(acc, t=t[n],t[n+1] )   ;   acc = F/m
      !   to impose v[n+1] = Vel_Front
      !           -->  v[n] is reset in velocity subroutine for nodes related to /EBCS/PROPERGOL

      ! -------------
      ! for parith/off option : update directly the acceleration array a() : no specific assembly
      if(iparit == 0) then
        ! do not update reaction force as coupling with inter1 is expected : vn_fluid = vn_wall
        !do ii=1,nod
          !num=liste(ii)
          !a(1,num)=a(1,num)+la(1,ii)
          !a(2,num)=a(2,num)+la(2,ii)
          !a(3,num)=a(3,num)+la(3,ii)
        !enddo
      endif

      ! -------------


      return
      end
