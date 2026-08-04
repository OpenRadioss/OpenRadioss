!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    tetra_for_trac_mod   ../engine/source/elements/solid/solide10/tetra_for_trac.F90
!||--- called by ------------------------------------------------------
!||    s10for_distor        ../engine/source/elements/solid/solide10/s10for_distor.F
!||====================================================================
      module tetra_for_trac_mod
      implicit none
      contains
! ======================================================================================================================
! \brief distortion control on extra-traction of tet elements
! ======================================================================================================================
!||====================================================================
!||    tetra_for_trac   ../engine/source/elements/solid/solide10/tetra_for_trac.F90
!||--- called by ------------------------------------------------------
!||    s10for_distor    ../engine/source/elements/solid/solide10/s10for_distor.F
!||--- uses       -----------------------------------------------------
!||    constant_mod     ../common_source/modules/constant_mod.F
!||    mvsiz_mod        ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine tetra_for_trac(                                 &
                   nel,    sti,    sti_c,                          &
                   xx ,    yy ,     zz ,                           &
                   xx0,     yy0,     zz0,                          &
                   vx ,     vy ,     vz ,                          &
                   xc ,     yc ,     zc ,                          &
                   xc0,     yc0,     zc0,                          &
                for_t1,  for_t2,  for_t3,                          &
                for_t4,  tol_t,   ifce ,                           &
                ifctl ,   e_distor, vc,dt1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,one,three,fourth,twenty,third,ten
          use precision_mod, only : WP
          use mvsiz_mod , only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                    :: nel             !< number of elements
          integer, intent (out)                                  :: ifctl           ! if control actived
          integer, dimension(mvsiz), intent(inout)               :: ifce            ! flag to do the control
          real(kind=WP), intent(in)                              :: tol_t           !< strain tolerance
          real(kind=WP), intent(in)                              :: dt1             !< time step
          real(kind=WP), dimension(mvsiz), intent(in   )         :: sti_c           !< control stiffness
          real(kind=WP), dimension(mvsiz), intent(inout)         :: sti             !< stiffness for time step
          double precision, dimension(mvsiz,4), intent(in)       :: xx0, yy0, zz0   !< coordinates in initial configuration
          double precision, dimension(mvsiz,4), intent(in)       :: xx, yy, zz      !< coordinates in current configuration
          real(kind=WP), dimension(mvsiz,4), intent(in)          :: vx, vy, vz      !< velocities in current configuration
          real(kind=WP), dimension(nel), intent(in)              :: xc, yc, zc      !< element center coordinates in current configuration
          real(kind=WP), dimension(nel), intent(in)              :: xc0, yc0, zc0   !< element center coordinates in initial configuration
          real(kind=WP), dimension(nel,3), intent(in)            :: vc              !< element center velocity
          real(kind=WP), dimension(nel), intent(inout)           :: e_distor        !< distortion control energy
          real(kind=WP), dimension(mvsiz,3), intent(inout)       ::          &
                               for_t1, for_t2, for_t3,for_t4                        !< distortion control forces
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,jj,ifc1(nel),jmaxi(nel),j2,jmax,iff
          real(kind=WP), dimension(nel) :: stif        
          real(kind=WP), dimension(nel,4) :: xl,yl,zl,xl0,yl0,zl0        
          real(kind=WP), dimension(nel)   :: fn,ll,l0        
          real(kind=WP) :: fact,norm,alpha,fac,nx,ny,nz,fx,fy,fz,f_t,kts,fnj,ddn,l3(3),v3(3),tol_t2,fac_max,k0
          real(kind=WP) :: dx,dy,dz,dmax,l0min,tol1,e_e,e_e0,lam2max,l2(4),l02(4),lam2(4),lam2m,tol_d,tol_dm
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
!   4 /\   
!    /  \  \
!   /    \   \3
!  /      \  /
!1/________\/2     12,13,14,23,24,34
!----1er sorting IFCE(I) 
         ifctl = 0
         stif(1:nel) = sti_c(1:nel)
! algo: 4 edge nodes respect to element center
!     only the case to do the largest tension strain is extra (much higher than others)
! one spring per element
! first sorting : 4 nodes in local coordinates respect to element center
         tol1=third*tol_t
         ifc1=0
        do i=1,nel
          if (ifce(i)==0) cycle
          do j=1,4
            xl0(i,j)= xx0(i,j)-xc0(i)
            yl0(i,j)= yy0(i,j)-yc0(i)
            zl0(i,j)= zz0(i,j)-zc0(i)
            xl(i,j)= xx(i,j)-xc(i)
            yl(i,j)= yy(i,j)-yc(i)
            zl(i,j)= zz(i,j)-zc(i)
            dx =xl(i,j)-xl0(i,j)
            dy =yl(i,j)-yl0(i,j)
            dz =zl(i,j)-zl0(i,j)
            dmax=max(dx,dy,dz)
            l0min=max(abs(xl0(i,j)),abs(yl0(i,j)),abs(zl0(i,j)))
            if (dmax>tol1*l0min) ifc1(i)=1
          end do
        end do
! 2nd sorting : largest tension and extra
         iff = 0
         tol_t2 = (one+tol_t)*(one+tol_t)
         tol_d = 1.1*1.1
         tol_dm = 1.25*1.25
         do i=1,nel
           if (ifc1(i)==0) cycle
           l02(1:4)=xl0(i,1:4)*xl0(i,1:4)+yl0(i,1:4)*yl0(i,1:4)+zl0(i,1:4)*zl0(i,1:4)
           l2(1:4)=xl(i,1:4)*xl(i,1:4)+yl(i,1:4)*yl(i,1:4)+zl(i,1:4)*zl(i,1:4)
           lam2(1:4) = l2(1:4)/l02(1:4)
           lam2max=zero
           lam2m=zero
           jmax=0
           do j = 1,4
             lam2m=lam2m+lam2(j)
             if (lam2(j)>lam2max) then
                lam2max=lam2(j)
                jmax=j
             end if
           end do
           if (lam2max<tol_t2) then 
             ifc1(i)=0
             cycle 
           end if
! look at average of reste and 2nd largest one
           lam2m=third*(lam2m-lam2max)
           j2=0
           lam2max=zero
           do j = 1,4
             if (j==jmax) cycle
             if (lam2(j)>lam2max) then
                lam2max=lam2(j)
                j2=j
             end if
           end do
           if (lam2(jmax)<tol_d*lam2(j2).or.lam2(jmax)<tol_dm*lam2m ) then 
             ifc1(i)=0
             cycle 
           end if
           iff = 1
           ll(i)=sqrt(l2(jmax))
           l0(i)=sqrt(l02(jmax))
           jmaxi(i)=jmax
         end do
        if (iff >0) then
         fn = zero
         f_t = three !quadratic
         alpha = one
!         alpha = fourth
         e_e0 = tol_t
         fac_max = ten
         do i=1,nel
            if (ifc1(i)==0) cycle
            e_e = (ll(i)-l0(i))/l0(i) -e_e0  ! relative strain
            fac = min(fac_max,alpha*e_e*e_e)
            k0 = sti_c(i)
            kts = (fac+one)*k0
            fn(i) = kts*e_e*l0(i)
            fact = f_t*fac+one
            stif(i) = max(stif(i),fact*k0) 
         end do
         do i=1,nel
           if (fn(i)==zero) cycle
           j= jmaxi(i)
           v3(1)=vc(i,1)-vx(i,j)
           v3(2)=vc(i,2)-vy(i,j)
           v3(3)=vc(i,3)-vz(i,j)
           l3(1)=-xl(i,j)
           l3(2)=-yl(i,j)
           l3(3)=-zl(i,j)
           norm = one/sqrt(l3(1)*l3(1)+l3(2)*l3(2)+l3(3)*l3(3))
           fnj = fn(i)
           nx = l3(1)*norm
           ny = l3(2)*norm
           nz = l3(3)*norm
           ddn = dt1*(v3(1)*nx+v3(2)*ny+v3(3)*nz)
           e_distor(i) = e_distor(i) + fnj*ddn
           fx = fnj*nx
           fy = fnj*ny
           fz = fnj*nz
           select case (j)
            case (1)
              for_t1(i,1) = for_t1(i,1) + fx
              for_t1(i,2) = for_t1(i,2) + fy
              for_t1(i,3) = for_t1(i,3) + fz
              fnj = third*fn(i)
              fx = fnj*nx
              fy = fnj*ny
              fz = fnj*nz
              for_t2(i,1) = for_t2(i,1) - fx
              for_t2(i,2) = for_t2(i,2) - fy
              for_t2(i,3) = for_t2(i,3) - fz
              for_t3(i,1) = for_t3(i,1) - fx
              for_t3(i,2) = for_t3(i,2) - fy
              for_t3(i,3) = for_t3(i,3) - fz
              for_t4(i,1) = for_t4(i,1) - fx
              for_t4(i,2) = for_t4(i,2) - fy
              for_t4(i,3) = for_t4(i,3) - fz
            case (2)
              for_t2(i,1) = for_t2(i,1) + fx
              for_t2(i,2) = for_t2(i,2) + fy
              for_t2(i,3) = for_t2(i,3) + fz
              fnj = third*fn(i)
              fx = fnj*nx
              fy = fnj*ny
              fz = fnj*nz
              for_t1(i,1) = for_t1(i,1) - fx
              for_t1(i,2) = for_t1(i,2) - fy
              for_t1(i,3) = for_t1(i,3) - fz
              for_t3(i,1) = for_t3(i,1) - fx
              for_t3(i,2) = for_t3(i,2) - fy
              for_t3(i,3) = for_t3(i,3) - fz
              for_t4(i,1) = for_t4(i,1) - fx
              for_t4(i,2) = for_t4(i,2) - fy
              for_t4(i,3) = for_t4(i,3) - fz
            case (3)
              for_t3(i,1) = for_t3(i,1) + fx
              for_t3(i,2) = for_t3(i,2) + fy
              for_t3(i,3) = for_t3(i,3) + fz
              fnj = third*fn(i)
              fx = fnj*nx
              fy = fnj*ny
              fz = fnj*nz
              for_t2(i,1) = for_t2(i,1) - fx
              for_t2(i,2) = for_t2(i,2) - fy
              for_t2(i,3) = for_t2(i,3) - fz
              for_t1(i,1) = for_t1(i,1) - fx
              for_t1(i,2) = for_t1(i,2) - fy
              for_t1(i,3) = for_t1(i,3) - fz
              for_t4(i,1) = for_t4(i,1) - fx
              for_t4(i,2) = for_t4(i,2) - fy
              for_t4(i,3) = for_t4(i,3) - fz
            case (4)
              for_t4(i,1) = for_t4(i,1) + fx
              for_t4(i,2) = for_t4(i,2) + fy
              for_t4(i,3) = for_t4(i,3) + fz
              fnj = third*fn(i)
              fx = fnj*nx
              fy = fnj*ny
              fz = fnj*nz
              for_t2(i,1) = for_t2(i,1) - fx
              for_t2(i,2) = for_t2(i,2) - fy
              for_t2(i,3) = for_t2(i,3) - fz
              for_t3(i,1) = for_t3(i,1) - fx
              for_t3(i,2) = for_t3(i,2) - fy
              for_t3(i,3) = for_t3(i,3) - fz
              for_t1(i,1) = for_t1(i,1) - fx
              for_t1(i,2) = for_t1(i,2) - fy
              for_t1(i,3) = for_t1(i,3) - fz
           end select
         end do
         do i=1,nel
           if (ifc1(i)==0) cycle
           if (stif(i)<=sti_c(i)) cycle
           sti(i) = sti(i)+stif(i)
           ifctl=ifctl+1
         enddo
        end if !(iff >0) then
!----------------------------
        end subroutine tetra_for_trac
!-------------------
      end module tetra_for_trac_mod
