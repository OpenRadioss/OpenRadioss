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
!=======================================================================================================================
!||====================================================================
!||    ush_init_mod   ../starter/source/elements/elbuf_init/ush_init.F90
!||--- called by ------------------------------------------------------
!||    initia         ../starter/source/elements/initia/initia.F
!||====================================================================
      module ush_init_mod

      use, intrinsic :: iso_c_binding
      implicit none
      interface
        subroutine  st_userlib_cini(   igtyp,                                       &
                   nel,        nnod,      nuvar,    iprop,    imat    , sid,      &
                   eint,       vol,       var,      off,      rho,      sig_loc,  &
                   xx,         yy,        zz,       vx,       vy,       vz,       &
                   vrx,        vry,       vrz,      mas,      inn,                & 
                   sti,        stir,      viscm,    viscr,    return_code ) bind (C, name="st_userlib_cini")
                    use, intrinsic :: iso_c_binding
                    use precision_mod,          only: wp
                    integer(c_int) :: igtyp
                    integer(c_int) :: nel
                    integer(c_int) :: nnod
                    integer(c_int) :: nuvar
                    integer(c_int), dimension(nel) :: iprop
                    integer(c_int), dimension(nel) :: imat
                    integer(c_int), dimension(nel) :: sid
                    real(kind=wp), dimension(nel) :: eint
                    real(kind=wp), dimension(nel) :: vol
                    real(kind=wp), dimension(nel) :: var
                    real(kind=wp), dimension(nel) :: off
                    real(kind=wp), dimension(nel) :: rho
                    real(kind=wp), dimension(6,nel) :: sig_loc
                    real(kind=wp), dimension(nel,nnod) :: xx
                    real(kind=wp), dimension(nel,nnod) :: yy
                    real(kind=wp), dimension(nel,nnod) :: zz
                    real(kind=wp), dimension(nel,nnod) :: vx
                    real(kind=wp), dimension(nel,nnod) :: vy
                    real(kind=wp), dimension(nel,nnod) :: vz
                    real(kind=wp), dimension(nel,nnod) :: vrx
                    real(kind=wp), dimension(nel,nnod) :: vry
                    real(kind=wp), dimension(nel,nnod) :: vrz
                    real(kind=wp), dimension(nel,nnod) :: mas
                    real(kind=wp), dimension(nel,nnod) :: inn
                    real(kind=wp), dimension(nel) :: sti
                    real(kind=wp), dimension(nel) :: stir
                    real(kind=wp), dimension(nel) :: viscm
                    real(kind=wp), dimension(nel) :: viscr
                    integer(c_int) :: return_code

        end subroutine st_userlib_cini
      end interface

      contains
!=======================================================================================================================
!!\brief This subroutine does the initialization of user shell property
!=======================================================================================================================
!||====================================================================
!||    ush_init             ../starter/source/elements/elbuf_init/ush_init.F90
!||--- called by ------------------------------------------------------
!||    initia               ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine ush_init(elbuf_tab,                                     &
                            numnod,  npart,       nummat,  nel,  pm,       &
                            iparg,   numelx,      nixx,    ixx,   ipartx,  &
                            i7stifs, userl_avail, nshnod,                  &
                            x,       v,           vr,      stifn, stifr,   &
                            dtelem,  thke,        partsav, msx,   inx ,    &
                            stx)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use constant_mod,           only: zero,half,one,two,fourth,hundred,three_half,three,ten,ep20
          use prop_param_mod ,       only : n_var_iparg,n_var_pm
          use precision_mod,          only: WP
          use my_alloc_mod
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include      "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                               intent (in   )     :: userl_avail      !< if user prop available
          integer,                               intent (in   )     :: i7stifs          !< stiffness flag for contact
          integer,                               intent (in   )     :: numnod           !< number node
          integer,                               intent (in   )     :: nummat           !< number material
          integer,                               intent (in   )     :: npart            !< number of ipart
          integer,                               intent (in   )     :: nel              !< number of elements in this group
          integer,                               intent (in   )     :: numelx           !< number x element
          integer,                               intent (in   )     :: nixx             !< 1er dimension of ixx
          integer, dimension(numelx),            intent (in   )     :: ipartx           !< element part id
          integer, dimension(numnod),            intent (inout)     :: nshnod           !< number shell element connected to node
          integer, dimension(n_var_iparg),       intent (in   )     :: iparg            !< element group data
          integer, dimension(nixx,numelx),       intent (in   )     :: ixx              !< x element connectivity
          real(kind=WP),dimension(numelx) ,      intent (inout)     :: dtelem           !< element time step
          real(kind=WP),dimension(numelx) ,      intent (inout)     :: thke             !< element thickness
          real(kind=WP), dimension(numelx),      intent (inout)     :: msx              !< element nodal mass
          real(kind=WP), dimension(numelx),      intent (inout)     :: inx              !< element nodal inertia
          real(kind=WP),dimension(nel)    ,      intent (inout)     :: stx              !< element Young*thickness
          real(kind=WP),dimension(numnod) ,      intent (inout)     :: stifn            !< nodal translational stiffness
          real(kind=WP),dimension(numnod) ,      intent (inout)     :: stifr            !< nodal rotational stiffness
          real(kind=WP),dimension(3,numnod),     intent (in   )     :: x                !< coordinates of nodes
          real(kind=WP),dimension(3,numnod),     intent (inout)     :: v                !< velocity of nodes
          real(kind=WP),dimension(3,numnod),     intent (inout)     :: vr               !< rotational velocity of nodes
          real(kind=WP),dimension(n_var_pm,nummat),intent(in   )    :: pm               !< material parameters
          real(kind=WP),dimension(20,npart),     intent(inout)      :: partsav          !< data to be saved for each part
          type (elbuf_struct_),  target                             :: elbuf_tab        !< el_buf struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,imid,ity,nft,nf1,nnod,ip,nuvar,igtyp
          integer, dimension(6)  :: ii
          integer, dimension(nel)  :: iprop,sid,imat
          integer, dimension(nel,4)  :: ncj
          integer :: return_code                        ! Return code from user subroutine / If routine could be called or not
          real(kind=WP),dimension(:,:),allocatable :: mas,inn,xx,yy,zz,vx,vy,vz,vrx,vry,vrz
          real(kind=WP),dimension(mvsiz)   :: area,mass,dtx,iner,sti,stir,viscm,viscr,masm,inm,rx,ry,rz,sx,sy,sz
          real(kind=WP) :: sig_loc(6,nel),lx,ly,lz,sxx,syy,szz,sxy,szx,syz,svx,svy,svz,sv2,dt2,visn,visr,ex,ey,ez,a2
          type(g_bufel_) ,pointer :: gbuf
          character(len=100) :: option
          character(len=10) :: char_pid
          integer :: size
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          dtx(1:mvsiz) = ep20
          gbuf => elbuf_tab%gbuf
!
          ity = iparg(5)
          nft = iparg(3)
          igtyp = iparg(38)
          if (ity==3) then
             nnod = 4
          else
             nnod = 3
          end if

          call my_alloc(xx,nel,nnod)
          call my_alloc(yy,nel,nnod)
          call my_alloc(zz,nel,nnod)
          call my_alloc(vx,nel,nnod)
          call my_alloc(vy,nel,nnod)
          call my_alloc(vz,nel,nnod)
          call my_alloc(vrx,nel,nnod)
          call my_alloc(vry,nel,nnod)
          call my_alloc(vrz,nel,nnod)
          call my_alloc(mas,nel,nnod)
          call my_alloc(inn,nel,nnod)
!
          nf1=nft+1
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

          do j=1,nnod
            do i=1,nel
              ncj(i,j)=ixx(j+1,i+nft)
              xx(i,j)=x(1,ncj(i,j))
              yy(i,j)=x(2,ncj(i,j))
              zz(i,j)=x(3,ncj(i,j))
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
            iprop(i)=ixx(nixx-1,i+nft)
            sid(i)  =ixx(nixx,i+nft)
            imat(i) =ixx(1,i+nft)
            gbuf%rho(i) =pm(1,imat(i)) !if rho isn't initialized mass will be zero in user subroutine
          enddo
          nuvar = elbuf_tab%gbuf%g_nuvar
!----------------------------------------
!     initialisation user: volume, masses et inerties, 
!----------------------------------------
          if (userl_avail==1)then
             return_code = 0
             call st_userlib_cini( igtyp,                                         &
                   nel,        nnod,      nuvar,    iprop,    imat    , sid,      &
                   gbuf%eint,  gbuf%vol,  gbuf%var, gbuf%off, gbuf%rho, sig_loc,  &
                   xx,         yy,        zz,       vx,       vy,       vz,       &
                   vrx,        vry,       vrz,      mas,      inn,                & 
                   sti,        stir,      viscm,    viscr,    return_code )

             if (return_code == 0)then
                       write(char_pid,'(i2)') igtyp
                       option='/PROP/'//trim(char_pid)//' Shells initialization: cinit'//char_pid
                       call ancmsg(MSGID=1130,msgtype=MSGERROR,c1=trim(OPTION),anmode=ANINFO)
                       call arret(2)
             endif

             do i=1,nel
               gbuf%sig(ii(1:6)+i) = sig_loc(1:6,i)
             enddo
!  
             masm =zero
             inm  =zero
             do j=1,nnod
               do i=1,nel
                 v(1,ncj(i,j))  = vx(i,j)
                 v(2,ncj(i,j))  = vy(i,j)
                 v(3,ncj(i,j))  = vz(i,j)
                 vr(1,ncj(i,j)) = vrx(i,j)
                 vr(2,ncj(i,j)) = vry(i,j)
                 vr(3,ncj(i,j)) = vrz(i,j)
                 masm(i) = masm(i) + mas(i,j)
                 inm (i) = inm (i) + inn(i,j)
               enddo
             enddo
!----------------------------------------
!     initialization of thke from volume  
!----------------------------------------
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
               thke(i)=gbuf%vol(i)/area(i)
             end do
!----------------------------------------
!     initialization of masses and inertias
!----------------------------------------
! fill msc,inc
             do i=1,nel  
                msx(i+nft)=masm(i)/nnod
                inx(i+nft)=inm(i)/nnod
                mass(i) = msx(i+nft)
                iner (i) = inx(i+nft)
             end do

             do i=1,nel      
               ip=ipartx(i+nft)
               partsav(1,ip)=partsav(1,ip) + masm(i)
               lx = zero 
               ly = zero
               lz = zero
               sxx = zero 
               syy = zero
               szz = zero
               sxy = zero
               szx = zero  
               syz = zero
               svx = zero
               svy = zero
               svz = zero
               sv2 = zero

               do j=1,nnod
                  lx = lx + x(1,ncj(i,j))
                  ly = ly + x(2,ncj(i,j)) 
                  lz = lz + x(3,ncj(i,j)) 
                  sxx = sxx + x(1,ncj(i,j))*x(1,ncj(i,j))
                  syy = syy + x(2,ncj(i,j))*x(2,ncj(i,j))
                  szz = szz + x(3,ncj(i,j))*x(3,ncj(i,j))
                  sxy = sxy + x(1,ncj(i,j))*x(2,ncj(i,j))
                  szx = szx + x(3,ncj(i,j))*x(1,ncj(i,j))
                  syz = syz + x(2,ncj(i,j))*x(3,ncj(i,j))
                  svx = svx + v(1,ncj(i,j))
                  svy = svy + v(2,ncj(i,j))
                  svz = svz + v(3,ncj(i,j))
                  sv2 = sv2 + v(1,ncj(i,j))*v(1,ncj(i,j)) +       &
                              v(2,ncj(i,j))*v(2,ncj(i,j)) +       &
                              v(3,ncj(i,j))*v(3,ncj(i,j))
               end do

               partsav(2,ip)=partsav(2,ip) + mass(i)*lx
               partsav(3,ip)=partsav(3,ip) + mass(i)*ly
               partsav(4,ip)=partsav(4,ip) + mass(i)*lz
               partsav(5,ip) =partsav(5,ip)  + mass(i) * (syy+szz)
               partsav(6,ip) =partsav(6,ip)  + mass(i) * (szz+sxx)
               partsav(7,ip) =partsav(7,ip)  + mass(i) * (sxx+syy)
               partsav(8,ip) =partsav(8,ip)  - mass(i) * sxy
               partsav(9,ip) =partsav(9,ip)  - mass(i) * syz
               partsav(10,ip)=partsav(10,ip) - mass(i) * szx
!
               partsav(11,ip)=partsav(11,ip) + mass(i)*svx
               partsav(12,ip)=partsav(12,ip) + mass(i)*svy
               partsav(13,ip)=partsav(13,ip) + mass(i)*svz
               partsav(14,ip)=partsav(14,ip) + half * mass(i) *sv2
             enddo
!------------------------------------------
!     assembly of nodal volumes and nodal modules
!     (for interface rigidities)
!------------------------------------------
             if(i7stifs/=0)then
                  do i=1,nel
                    imid=imat(i)
                    stx(i)=pm(20,imid)*thke(i)  !pm(20) should be max throught thickness
                    nshnod(ncj(i,1:nnod))=nshnod(ncj(i,1:nnod))+1
                  enddo
             endif
!------------------------------------------
!    calculation of elementary dt: based on nodal one (element)
!------------------------------------------
             do i=1,nel
               visn = sqrt(one + viscm(i)*viscm(i))-viscm(i)
               visr = sqrt(one + viscr(i)*viscr(i))-viscr(i)
               dt2=min(visn*mass(i)/sti(i),visr*iner(i)/stir(i))
               dtx(i) = sqrt(two*dt2)
               dtelem(nft+i)=dtx(i)
             enddo

             do j=1,nnod
                do i=1,nel
                  stifn(ncj(i,j))=stifn(ncj(i,j))+sti(i)
                  stifr(ncj(i,j))=stifr(ncj(i,j))+stir(i)
                enddo
             enddo

          else
              if (return_code == 0)then
                   write(char_pid,'(i2)') igtyp
                   option='/PROP/'//trim(char_pid)//' Shells initialization: cinit'//char_pid
                   call ancmsg(MSGID=1130,msgtype=MSGERROR,c1=trim(OPTION),anmode=ANINFO)
                   call arret(2)
              endif
          endif ! if (userl_avail==1)then
!
          deallocate(mas)
          deallocate(inn)
          deallocate(xx)
          deallocate(yy)
          deallocate(zz)
          deallocate(vx)
          deallocate(vy)
          deallocate(vz)
          deallocate(vrx)
          deallocate(vry)
          deallocate(vrz)

        end subroutine ush_init
!
      end module ush_init_mod
