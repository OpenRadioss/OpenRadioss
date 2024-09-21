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
      !||    inivel_start_mod   ../engine/source/loads/general/inivel/inivel_start.F90
      !||--- called by ------------------------------------------------------
      !||    resol              ../engine/source/engine/resol.F
      !||====================================================================
      module inivel_start_mod
!        
       contains
  !! \brief apply inivel w/ T_start 
      !||====================================================================
      !||    inivel_start    ../engine/source/loads/general/inivel/inivel_start.F90
      !||--- called by ------------------------------------------------------
      !||    resol           ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg          ../engine/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod    ../common_source/modules/constant_mod.F
      !||    elbufdef_mod    ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    groupdef_mod    ../common_source/modules/groupdef_mod.F
      !||    inivel_mod      ../common_source/modules/inivel_mod.F90
      !||    message_mod     ../engine/share/message_module/message_mod.F
      !||    multi_fvm_mod   ../common_source/modules/ale/multi_fvm_mod.F
      !||    sensor_mod      ../engine/share/modules/sensor_mod.F
      !||====================================================================
        subroutine inivel_start(                                              &
                       ngrnod,  ngrbric,    ngrquad,       ngrsh3n,           &
                       igrnod,  igrbric,    igrquad,       igrsh3n,           &
                       numskw,    lskew,    numfram,       sensors,           &
                      xframe,      skew,          x,             v,           &
                          vr,    numnod,      vflow,         wflow,           &
                           w, multi_fvm,       iale,       ialelag,           &
                       time ,    iroddl,   ninivelt,      inivel_t,           &
                       nparg,    ngroup,       lens,         iparg,           &
                  elbuf_tab,         ms,         in,        weight,           &
                    nxframe,      t_kin)  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use inivel_mod 
      use message_mod
      use groupdef_mod
      use sensor_mod
      USE multi_fvm_mod
      use elbufdef_mod 
      use constant_mod,          only : zero,half
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
      integer , intent(inout)                          :: ninivelt  !< dimension of inivel_t
      integer , intent(in   )                          :: numskw    !< number of skew
      integer , intent(in   )                          :: numfram   !< number of frame
      integer , intent(in   )                          :: lskew     !< 1er dimension of skew
      integer , intent(in   )                          :: nxframe   !< 1er dimension of frame
      integer , intent(in   )                          :: numnod    !< number of node
      integer , intent(in   )                          :: nparg     !< 1er dimension of iparg
      integer , intent(in   )                          :: ngroup    !< number of element group
      integer , intent(in   )                          :: iroddl    !< rotational dof flag
      integer , intent(in   )                          :: iale      !< ale flag
      integer , intent(in   )                          :: ialelag   !< alelag flag
      integer , intent(in   )                          :: ngrnod    !< number node group
      integer , intent(in   )                          :: ngrbric   !< number solid element group
      integer , intent(in   )                          :: ngrquad   !< number quad element group
      integer , intent(in   )                          :: ngrsh3n   !< number tria element group
      integer , intent(in   )                          :: lens      !< dimesion of work array itagvel
      integer , intent(in   ) ,dimension(numnod)       :: weight    !< nodal mass weight array (spmd)
      integer , dimension(nparg,ngroup), intent(in   ) :: iparg     !< element group data array
      type(inivel_), dimension(ninivelt),intent(inout) :: inivel_t  !< inivel_struc 
      type (group_)  , dimension(ngrnod)               :: igrnod    !< node group array
      type (group_)  , dimension(ngrbric)              :: igrbric   !< solid element group array
      type (group_)  , dimension(ngrquad)              :: igrquad   !< quad element group array
      type (group_)  , dimension(ngrsh3n)              :: igrsh3n   !< tria element group array
      type (sensors_) ,intent(in  )                    :: sensors   !< sensor structure
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT)            :: multi_fvm !< multi_fvm structure
      type(elbuf_struct_), target, dimension(ngroup)   :: elbuf_tab !< elemnt buffer data
      my_real, intent(in) ,dimension(lskew,numskw+1)   :: skew      !< local skew data
      my_real, intent(in) ,dimension(nxframe,numfram+1):: xframe    !< frame data
      my_real, intent(in) ,dimension(numnod)           :: ms        !< nodal mass
      my_real, intent(in) ,dimension(numnod)           :: in        !< nodal inertia
      my_real, intent(in) ,dimension(3,numnod)         :: x         !< coordinnate array
      my_real, intent(inout) ,dimension(3,numnod)      :: v         !< velocity
      my_real, intent(inout) ,dimension(3,numnod)      :: vr        !< rotational velocity
      my_real, intent(inout) ,dimension(3,numnod)      :: vflow     !< velocity for int22
      my_real, intent(inout) ,dimension(3,numnod)      :: wflow     !< velocity for int22 (ale)
      my_real, intent(inout) ,dimension(3,numnod)      :: w         !< velocity for ALE
      my_real, intent(in   )                           :: time      !< time
      my_real, intent(inout)                           :: t_kin     !< kinematic energy of inivel
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: i,j,id,n,ng,itype,igr,nosys,sens_id,iremain,iupdate
      integer  :: igrs,igbric,igqd,igtria,isk,ifra,idir,ifm,k1,k2,k3
      integer  :: mtn,nel,nft,ii
      integer , dimension(:) , allocatable :: itagvel
      my_real  :: tstart,tstart_s,tstart1,vx,vy,vz,vl(3), nixj(6),vlt(3),mas
      my_real :: vl1, vl2, vl3,vra, ox, oy, oz,vr1,vr2,vr3,vrl1,vrl2,vrl3
      type(g_bufel_), pointer :: gbuf
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!  if need update in case of FVM
       iupdate = 0 
       t_kin = zero
       do n =1,ninivelt 
         itype = inivel_t(n)%itype
         if (itype /=5 ) cycle 
         sens_id = inivel_t(n)%fvm%sensor_id 
         tstart = inivel_t(n)%fvm%tstart 
         tstart_s = zero 
         if (sens_id>0) tstart_s = sensors%sensor_tab(sens_id)%tstart 
         if (tstart==zero) then 
           tstart1 = tstart_s 
         else if (sens_id==0) then
           tstart1 = tstart 
         else
           tstart1 = min(tstart,tstart_s) 
         end if 
         if (tstart1<=time) iupdate = 1 
       end do 
       if (iupdate>0) then 
         allocate(itagvel(lens))
         itagvel = 0
       end if 
       iremain = 0
       do n =1,ninivelt 
         itype = inivel_t(n)%itype
         if (itype <0) cycle ! applied already
         select case (itype)
           case(0,1,2,3)
               sens_id = inivel_t(n)%general%sensor_id 
               tstart = inivel_t(n)%general%tstart 
           case(4)
               sens_id = inivel_t(n)%axis%sensor_id 
               tstart = inivel_t(n)%axis%tstart 
           case(5)
               sens_id = inivel_t(n)%fvm%sensor_id 
               tstart = inivel_t(n)%fvm%tstart 
         end select 
         tstart_s = zero 
         if (sens_id>0) tstart_s = sensors%sensor_tab(sens_id)%tstart 
         if (tstart==zero) then 
           tstart1 = tstart_s 
         else if (sens_id==0) then
           tstart1 = tstart 
         else
           tstart1 = min(tstart,tstart_s) 
         end if 
         if (tstart1<=time) then 
           id = inivel_t(n)%id
           if (tstart1==tstart_s) then 
             call ancmsg(msgid=308,anmode=aninfo,i1=id,c1='SENSOR ON')
           else 
             call ancmsg(msgid=308,anmode=aninfo,i1=id,c1='T_START')
           end if
           select case (itype)
             case(0,1,2,3)
               isk = inivel_t(n)%general%skew_id 
               vx = inivel_t(n)%general%vx 
               vy = inivel_t(n)%general%vy 
               vz = inivel_t(n)%general%vz 
               if (isk > 0) then
                  vl(1) = skew(1,isk)*vx+skew(4,isk)*vy+skew(7,isk)*vz
                  vl(2) = skew(2,isk)*vx+skew(5,isk)*vy+skew(8,isk)*vz
                  vl(3) = skew(3,isk)*vx+skew(6,isk)*vy+skew(9,isk)*vz
               else 
                  vl(1) = vx
                  vl(2) = vy
                  vl(3) = vz
               end if
               igrs = inivel_t(n)%general%grnd_id 
             case(4)
               idir = inivel_t(n)%axis%dir
               ifra = inivel_t(n)%axis%frame_id 
               vx = inivel_t(n)%axis%vx 
               vy = inivel_t(n)%axis%vy 
               vz = inivel_t(n)%axis%vz 
               vra = inivel_t(n)%axis%vr 
               if (ifra > 0) then
                 j = ifra
                 vl(1) = xframe(1,j)*vx+xframe(4,j)*vy+xframe(7,j)*vz
                 vl(2) = xframe(2,j)*vx+xframe(5,j)*vy+xframe(8,j)*vz
                 vl(3) = xframe(3,j)*vx+xframe(6,j)*vy+xframe(9,j)*vz
               else 
                 vl(1) = vx
                 vl(2) = vy
                 vl(3) = vz
               end if
               igrs = inivel_t(n)%axis%grnd_id 
             case(5)
               vl(1) = inivel_t(n)%fvm%vx 
               vl(2) = inivel_t(n)%fvm%vy 
               vl(3) = inivel_t(n)%fvm%vz 
               igbric = inivel_t(n)%fvm%grbric_id
               igqd   = inivel_t(n)%fvm%grqd_id
               igtria = inivel_t(n)%fvm%grtria_id
           end select 
           select case (itype)
             case(0)
                do j=1,igrnod(igrs)%nentity
                  nosys=igrnod(igrs)%entity(j)
                    v(1:3,nosys)=vl(1:3)
                    if(ialelag > 0) then
                       vflow(1:3,nosys) = vl(1:3)
                       wflow(1:3,nosys) = vl(1:3)
                    endif 
                    mas=ms(nosys)*weight(nosys)
                    t_kin=t_kin+( vl(1)*vl(1)+vl(2)*vl(2)+vl(3)*vl(3))*half*mas
                end do
             case(1)
                do j=1,igrnod(igrs)%nentity
                  nosys=igrnod(igrs)%entity(j)
                    if(iroddl > 0) then
                       vr(1:3,nosys) = vl(1:3)
                       mas=in(nosys)*weight(nosys)
                       t_kin=t_kin+( vl(1)*vl(1)+vl(2)*vl(2)+vl(3)*vl(3))*half*mas
                    endif 
                end do
             case(2)
                do j=1,igrnod(igrs)%nentity
                  nosys=igrnod(igrs)%entity(j)
                    v(1:3,nosys)=vl(1:3)
                    if(ialelag > 0) then
                       vflow(1:3,nosys) = vl(1:3)
                       wflow(1:3,nosys) = vl(1:3)
                    endif 
                    if (iale == 1) then
                       w(1:3,nosys)=vl(1:3)
                    endif
                    mas=ms(nosys)*weight(nosys)
                    t_kin=t_kin+( vl(1)*vl(1)+vl(2)*vl(2)+vl(3)*vl(3))*half*mas
                end do
             case(3)
                do j=1,igrnod(igrs)%nentity
                  nosys=igrnod(igrs)%entity(j)
                    w(1:3,nosys)=vl(1:3)
                    if(ialelag > 0) then
                       vflow(1:3,nosys) = vl(1:3)
                       wflow(1:3,nosys) = vl(1:3)
                    endif 
                    mas=ms(nosys)*weight(nosys)
                    t_kin=t_kin+( vl(1)*vl(1)+vl(2)*vl(2)+vl(3)*vl(3))*half*mas
                end do
             case(4)
! no treatment needed for the case with main node of RBODY 
               do j=1,igrnod(igrs)%nentity
                 nosys=igrnod(igrs)%entity(j)
                 nixj(1:6) = zero
                 if (ifra > 0) then
                    k1=3*idir-2
                    k2=3*idir-1
                    k3=3*idir
                    ifm = ifra
                    ox  = xframe(10,ifm)
                    oy  = xframe(11,ifm)
                    oz  = xframe(12,ifm)
                    nixj(1)=xframe(k1,ifm)*(x(2,nosys)-oy)
                    nixj(2)=xframe(k2,ifm)*(x(1,nosys)-ox)
                    nixj(3)=xframe(k2,ifm)*(x(3,nosys)-oz)
                    nixj(4)=xframe(k3,ifm)*(x(2,nosys)-oy)
                    nixj(5)=xframe(k3,ifm)*(x(1,nosys)-ox)
                    nixj(6)=xframe(k1,ifm)*(x(3,nosys)-oz)
                    if (iroddl>0) then
                       vr(1,nosys)= vra*xframe(k1,ifm)
                       vr(2,nosys)= vra*xframe(k2,ifm)
                       vr(3,nosys)= vra*xframe(k3,ifm)
                       vlt(1:3) = vr(1:3,nosys)
                       mas=in(nosys)*weight(nosys)
                       t_kin=t_kin+(vlt(1)*vlt(1)+vlt(2)*vlt(2)+vlt(3)*vlt(3))*half*mas
                    end if
                 else
                    if(idir==1) then
                       nixj(1)=x(2,nosys)
                       nixj(6)=x(3,nosys)
                    elseif(idir==2) then
                       nixj(2)=x(1,nosys)
                       nixj(3)=x(3,nosys)
                    elseif(idir==3) then
                       nixj(4)=x(2,nosys)
                       nixj(5)=x(1,nosys)
                    endif
                    if (iroddl>0) then
                       vr(1:3,nosys)= zero !vra*xframe(k1,ifm)
                       if (idir>0) vr(idir,nosys)= vra
                       mas=in(nosys)*weight(nosys)
                       t_kin=t_kin+vra*vra*half*mas
                    end if
                 endif
                 v(1,nosys)= vl(1)+vra*(nixj(3)-nixj(4))
                 v(2,nosys)= vl(2)+vra*(nixj(5)-nixj(6))
                 v(3,nosys)= vl(3)+vra*(nixj(1)-nixj(2))
                 if(ialelag > 0) then
                    vflow(1:3,nosys) = v(1:3,nosys)
                    wflow(1:3,nosys) = v(1:3,nosys)
                 end if 
                 vlt(1:3) = v(1:3,nosys)
                 mas=ms(nosys)*weight(nosys)
                t_kin=t_kin+(vlt(1)*vlt(1)+vlt(2)*vlt(2)+vlt(3)*vlt(3))*half*mas
!--               
               end do 
             case(5)
               if (igbric > 0) then
                 igrs = igbric
                 do j=1,igrbric(igrs)%nentity
                   nosys=igrbric(igrs)%entity(j)
                   multi_fvm%vel(1:3, nosys) = vl(1:3)
                   itagvel(nosys) = 1
                 end do 
               end if
               if (igqd > 0) then
                 igrs = igqd
                 do j=1,igrquad(igrs)%nentity
                   nosys=igrquad(igrs)%entity(j)
                   multi_fvm%vel(1:3, nosys) = vl(1:3)
                   itagvel(nosys) = 1
                 end do 
               end if
               if (igtria > 0) then
                 igrs = igtria
                 do j=1,igrsh3n(igrs)%nentity
                   nosys=igrsh3n(igrs)%entity(j)
                   multi_fvm%vel(1:3, nosys) = vl(1:3)
                   itagvel(nosys) = 1
                 end do 
               end if
           end select 
           inivel_t(n)%itype = -1
         else 
           iremain = 1
         end if
       end do 
       if (iremain==0) ninivelt = 0 
!       
       if (iupdate>0) then 
         do ng =1, ngroup
           mtn = iparg(1, ng)
           if (mtn == 151) then
            nel = iparg(2, ng)
            nft = iparg(3, ng)
            gbuf => elbuf_tab(ng)%gbuf
!     as done at T=0
            do ii = 1, nel
               i = ii + nft 
               if (itagvel(i)==1) then
                 gbuf%mom(ii + 0 * nel) = multi_fvm%vel(1, i)
                 gbuf%mom(ii + 1 * nel) = multi_fvm%vel(2, i)
                 gbuf%mom(ii + 2 * nel) = multi_fvm%vel(3, i)
                 mas = gbuf%vol(ii) * multi_fvm%rho(i)
                 vlt(1:3) = multi_fvm%vel(1:3, i)
                 t_kin=t_kin+(vlt(1)*vlt(1)+vlt(2)*vlt(2)+vlt(3)*vlt(3))*half*mas
               end if
            end do
           end if !(mtn == 151) then
         end do
         deallocate(itagvel)
       end if 

! 1000   FORMAT(3X,'BY SENSOR ON, ACTIVATING INIVEL OF ID =',I10)
! 2000   FORMAT(3X,'ACTIVATING INIVEL BY T_START OF ID =',I10)
       end subroutine inivel_start
      end module inivel_start_mod
