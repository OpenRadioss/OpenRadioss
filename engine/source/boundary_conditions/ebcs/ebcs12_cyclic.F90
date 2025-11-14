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
!||    ebcs12_cyclic_mod   ../engine/source/boundary_conditions/ebcs/ebcs12_cyclic.F90
!||--- called by ------------------------------------------------------
!||    ebcs_main           ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||====================================================================
      module ebcs12_cyclic_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!||====================================================================
!||    ebcs12_cyclic         ../engine/source/boundary_conditions/ebcs/ebcs12_cyclic.F90
!||--- called by ------------------------------------------------------
!||    ebcs_main             ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||--- calls      -----------------------------------------------------
!||    arret                 ../engine/source/system/arret.F
!||    ebcs_get_group_info   ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    ebcs8_inlet_mod       ../engine/source/boundary_conditions/ebcs/ebcs8_inlet.F90
!||    ebcs_mod              ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    multimat_param_mod    ../common_source/modules/multimat_param_mod.F90
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    segvar_mod            ../engine/share/modules/segvar_mod.F
!||    th_surf_mod           ../common_source/modules/interfaces/th_surf_mod.F
!||====================================================================
        subroutine ebcs12_cyclic(nseg,iseg,segvar, &
          x, v, w, a, &
          liste,nod,irect,ielem,iface, &
          la,ms,stifn,ebcs,iparg,elbuf_tab,ixq,ixs,ixtg, &
          fsavsurf,iparit,dt1, &
          numels, numelq, numeltg, numnod, nparg, ngroup, nixs, nixq, nixtg, nsurf, n2d, &
          ncycle,iale,elem_adress,lsky,fsky)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use ebcs_mod
          use elbufdef_mod
          use segvar_mod
          use th_surf_mod , only : th_surf_num_channel
          use constant_mod , only : zero, em06, one, third, fourth, half, two, three, four, em20
          use precision_mod, only : WP
          use multimat_param_mod , only : m51_n0phas, m51_nvphas
          use EBCS8_INLET_MOD , only : EBCS_GET_GROUP_INFO
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   include files
! ----------------------------------------------------------------------------------------------------------------------
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: n2d !< 2d/3d flag
          integer,intent(in) :: numeltg, numels, numelq, numnod, nparg, ngroup, nixs, nixq, nixtg, nsurf !< array sizes
          real(kind=WP), intent(in) :: dt1 !< time step
          integer,intent(in) :: iparit !< /parith/on flag
          real(kind=WP), intent(inout) :: fsavsurf(th_surf_num_channel,nsurf)
          integer,intent(in) :: nseg,nod,iseg(nseg),liste(nod),irect(4,nseg),ielem(nseg),iface(nseg)
          integer,intent(in) :: ixq(nixq,numelq),ixs(nixs,numels),ixtg(nixtg,numeltg)
          real(kind=WP),intent(inout) :: ms(numnod) !< nodal mass
          real(kind=WP),intent(inout) :: x(3,numnod),v(3,numnod),w(3,numnod),a(3,numnod) !< coordinates, mat. velocity, grid velocity, accelerations
          real(kind=WP),intent(inout) :: la(3,nod),stifn(numnod)
          type(t_ebcs_cyclic), intent(inout) :: ebcs !< ebcs data structure
          integer :: iparg(nparg,ngroup) !< data for all group of elems
          type(elbuf_struct_), target, dimension(ngroup) :: elbuf_tab !< element buffer
          type(t_segvar),intent(inout) :: segvar !< ghost cell data
          integer,intent(in) :: ncycle !< solver cycle
          integer,intent(in) :: iale  !< ale flag : 1 if grid is updated
          integer, dimension(4,nseg), intent(in) :: elem_adress ! adress for fsky array (only used with parith/on)
          integer,intent(in) :: lsky
          real(kind=WP), dimension(8,lsky), intent(inout) :: fsky ! acceleration array for parith/on option
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          type(g_bufel_), pointer :: gbuf1,gbuf2
          type(l_bufel_)  ,pointer :: lbuf1,lbuf2
          type(buf_mat_)  ,pointer :: mbuf1,mbuf2
          integer :: ii,is,kk,num,ix(4)
          integer :: icf_2d(2,4), icf_3d(4,6), jj, isubmat, ipos, mtn1,mtn2
          integer :: ng1,ng2
          integer :: iloc1,iloc2
          integer :: klt1,klt2
          integer :: nn1(4),nn2(4)
          integer :: nng1(4),nng2(4)
          integer :: kseg1,kseg2
          integer :: adress
          real(kind=WP) :: x13,y13,z13, x24,y24,z24
          real(kind=WP) :: phase_rho1(21), phase_eint1(21), phase_alpha1(21), phase_temp1(21)
          real(kind=WP) :: phase_rho2(21), phase_eint2(21), phase_alpha2(21), phase_temp2(21)
          real(kind=WP) :: e1,e2,t1,t2,p1,p2,rho1,rho2,vol1,vol2,volg1,volg2,ssp1,ssp2
          real(kind=WP) :: rho_,ssp_,ms_ !< mean value (surface1 & surface2)
          real(kind=WP) :: v1(3), v2(3), v_(3) !< velocity on surf1, surf2 and mean value
          real(kind=WP) :: xn,yn,zn !< normal vector
          real(kind=WP) :: fac1,fac2
          real(kind=WP) :: vf,wf ! face velocity
          real(kind=WP) :: npt, orient, surf
          logical :: lMULTIMAT
          integer :: surf_iid1, surf_iid2 !< ebcs surface identifiers
          integer :: nbsubmat

          data icf_2d  /1,2,2,3,3,4,4,1/
          data icf_3d  /1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/


! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------

          IF(NCYCLE == 0) CALL EBCS_GET_GROUP_INFO(NSEG, EBCS%IELEM, EBCS%NG, EBCS%ILOC,  NGROUP, NPARG, IPARG, N2D)
          rho_ = zero
          wf = zero
          vf = zero
          surf_iid1 = ebcs%surf_id
          surf_iid2 = ebcs%surf_id2

          !velocity safety check (to avoid digits difference and bifurcation)
          DO ii=1,nod/2
            v1(1:3) = V(1:3,EBCS%NODE_LIST(ii))
            v2(1:3) = V(1:3,EBCS%NODE_LIST(nod/2+ii))
            v_(1:3) = half*(v1(1:3)+v2(1:3))
            V(1:3,EBCS%NODE_LIST(ii)) = v_(1:3)
            V(1:3,EBCS%NODE_LIST(nod/2+ii)) = v_(1:3)
          END DO

          !--- reset working array for internal forces
          do ii=1,nod
            num=liste(ii)
            la(1,ii)=zero
            la(2,ii)=zero
            la(3,ii)=zero
          enddo

          do is=1,nseg/2
            kseg1=abs(iseg(is))
            kseg2=abs(iseg(nseg/2+is))
            orient=float(iseg(is)/kseg1)
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
              if(ix(4) == ix(3))then
                npt=THREE
                fac1=third

                vf =   v(1,ix(1))*xn + v(1,ix(2))*xn + v(1,ix(3))*xn &
                     + v(2,ix(1))*yn + v(2,ix(2))*yn + v(2,ix(3))*yn &
                     + v(3,ix(1))*zn + v(3,ix(2))*zn + v(3,ix(3))*zn
                wf = zero
                if (iale == 1)then
                wf =   w(1,ix(1))*xn + w(1,ix(2))*xn + w(1,ix(3))*xn &
                     + w(2,ix(1))*yn + w(2,ix(2))*yn + w(2,ix(3))*yn &
                     + w(3,ix(1))*zn + w(3,ix(2))*zn + w(3,ix(3))*zn
                end if
                vf=(vf-wf)*fac1

              else
                npt=FOUR
                fac1=fourth

                vf =   v(1,ix(1))*xn + v(1,ix(2))*xn + v(1,ix(3))*xn + v(1,ix(4))*xn  &
                     + v(2,ix(1))*yn + v(2,ix(2))*yn + v(2,ix(3))*yn + v(2,ix(4))*yn  &
                     + v(3,ix(1))*zn + v(3,ix(2))*zn + v(3,ix(3))*zn + v(3,ix(4))*zn
                wf = zero
                if (iale == 1)then
                wf =   w(1,ix(1))*xn + w(1,ix(2))*xn + w(1,ix(3))*xn + w(1,ix(4))*xn &
                     + w(2,ix(1))*yn + w(2,ix(2))*yn + w(2,ix(3))*yn + w(2,ix(4))*yn &
                     + w(3,ix(1))*zn + w(3,ix(2))*zn + w(3,ix(3))*zn + w(3,ix(4))*zn
                end if
                vf=(vf-wf)*fac1

              endif





            else !n2d > 0

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
              if(n2d == 1)then
                yn = yn * (x(2,ix(1))+x(2,ix(2))) * half
                zn = zn * (x(2,ix(1))+x(2,ix(2))) * half
              endif
              fac2 = one/sqrt(yn*yn+zn*zn)
              yn=yn*fac2
              zn=zn*fac2
              surf = one/fac2

                vf =   v(2,ix(1))*yn + v(2,ix(2))*yn  &
                     + v(3,ix(1))*zn + v(3,ix(2))*zn
                wf = zero
                if (iale == 1)then
                wf =   w(2,ix(1))*yn + w(2,ix(2))*yn  &
                     + w(3,ix(1))*zn + w(3,ix(2))*zn
                end if
                vf=(vf-wf)*fac1

            endif

            nn1(1:4)=irect(1:4,is)
            nn2(1:4)=nod/2+irect(1:4,is+nseg/2)
            nng1(1:4)=liste(nn1(1:4))
            nng2(1:4)=liste(nn2(1:4))

            !-- adjacent state
            ng1 = EBCS%ng(is)
            ng2 = EBCS%ng(nseg/2+is)

            iloc1 = EBCS%iloc(is)
            iloc2 = EBCS%iloc(nseg/2+is)

            gbuf1 => elbuf_tab(ng1)%gbuf
            gbuf2 => elbuf_tab(ng2)%gbuf

            lbuf1 => elbuf_tab(ng1)%bufly(1)%lbuf(1,1,1)
            lbuf2 => elbuf_tab(ng2)%bufly(1)%lbuf(1,1,1)

            klt1 = iparg(2,ng1)
            klt2 = iparg(2,ng2)

            mtn1 = iparg(1,ng1)
            mtn2 = iparg(1,ng2)
            if(mtn1 /= mtn2)then
                print *, "**ERROR /EBCS/CYCLIC : MATERIAL LAWS ON SURFACES 1 & 2 DO NOT MATCH"
                CALL ARRET(2)
            end if
            lMULTIMAT=.FALSE.
            if(mtn1 == 51)lMULTIMAT=.TRUE.

            !adjacent pressure
            p1 = -third*(gbuf1%sig(iloc1) + gbuf1%sig(klt1+iloc1) + gbuf1%sig(2*klt1+iloc1))
            p2 = -third*(gbuf2%sig(iloc2) + gbuf2%sig(klt2+iloc2) + gbuf2%sig(2*klt2+iloc2))

            e1 = gbuf1%eint(iloc1)
            e2 = gbuf2%eint(iloc2)

            t1 = gbuf1%temp(iloc1)
            t2 = gbuf2%temp(iloc2)

            !adjecent density
            rho1 = gbuf1%rho(iloc1)
            rho2 = gbuf2%rho(iloc2)

            vol1 = gbuf1%vol(iloc1)
            vol2 = gbuf2%vol(iloc2)

            volg1=vol1 ! global volume (for multimaterial case)
            volg2=vol2 ! global volume (for multimaterial case)

            ssp1 = lbuf1%ssp(iloc1)
            ssp2 = lbuf2%ssp(iloc2)
            
            !volume fracions and submat state
            if(lMULTIMAT)then
              nbsubmat = 4
              mbuf1 => elbuf_tab(ng1)%bufly(1)%mat(1,1,1)
              mbuf2 => elbuf_tab(ng2)%bufly(1)%mat(1,1,1)

              ! data on surface 1
              do isubmat=1,nbsubmat
                !volume fraction
                ipos = 1 ;  kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt1  +  iloc1
                phase_alpha1(isubmat) = mbuf1%var(kk)
                !mass density
                ipos = 9 ;  kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt1  +  iloc1
                phase_rho1(isubmat) = mbuf1%var(kk)
                !energy density
                ipos = 8 ;  kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt1  +  iloc1
                phase_eint1(isubmat) = mbuf1%var(kk)
                !energy density
                ipos = 16 ; kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt1  +  iloc1
                phase_temp1(isubmat) = mbuf1%var(kk)
              enddo

              ! data on surface 2
              do isubmat=1,nbsubmat
                !volume fraction
                ipos = 1 ;  kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt2  +  iloc2
                phase_alpha2(isubmat) = mbuf2%var(kk)
                !mass density
                ipos = 9 ;  kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt2  +  iloc2
                phase_rho2(isubmat) = mbuf2%var(kk)
                !energy density
                ipos = 8 ;  kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt2  +  iloc2
                phase_eint2(isubmat) = mbuf2%var(kk)
                !energy density
                ipos = 16 ; kk = (m51_n0phas + (isubmat-1)*m51_nvphas +ipos-1) * klt2  +  iloc2
                phase_temp2(isubmat) = mbuf2%var(kk)
              enddo

              segvar%phase_alpha(1:4,kseg1) = phase_alpha2(1:4)
              segvar%phase_rho(1:4,kseg1) = phase_rho2(1:4)
              segvar%phase_eint(1:4,kseg1) = phase_eint2(1:4)

              segvar%phase_alpha(1:4,kseg2) = phase_alpha1(1:4)
              segvar%phase_rho(1:4,kseg2) = phase_rho1(1:4)
              segvar%phase_eint(1:4,kseg2) = phase_eint1(1:4)

              e1 = phase_eint1(1)*phase_alpha1(1) + &
                   phase_eint1(2)*phase_alpha1(2) + &
                   phase_eint1(3)*phase_alpha1(3) + &
                   phase_eint1(4)*phase_alpha1(4)

              rho1= phase_rho1(1)*phase_alpha1(1) + &
                    phase_rho1(2)*phase_alpha1(2) + &
                    phase_rho1(3)*phase_alpha1(3) + &
                    phase_rho1(4)*phase_alpha1(4)

              e2 = phase_eint2(1)*phase_alpha2(1) + &
                   phase_eint2(2)*phase_alpha2(2) + &
                   phase_eint2(3)*phase_alpha2(3) + &
                   phase_eint2(4)*phase_alpha2(4)

              rho2= phase_rho2(1)*phase_alpha2(1) + &
                    phase_rho2(2)*phase_alpha2(2) + &
                    phase_rho2(3)*phase_alpha2(3) + &
                    phase_rho2(4)*phase_alpha2(4)

            endif

            segvar%rho(kseg1) = rho2
            segvar%eint(kseg1) = e2

            segvar%rho(kseg2) = rho1
            segvar%eint(kseg2) = e1

            !-- expand pressure to face nodes

            !expand pressure loading to segment nodes
            do kk=1,INT(npt)

              !surface 1
              la(1,nn1(kk)) = la(1,nn1(kk)) - fac1* (p2*surf) * xn
              la(2,nn1(kk)) = la(2,nn1(kk)) - fac1* (p2*surf) * yn
              la(3,nn1(kk)) = la(3,nn1(kk)) - fac1* (p2*surf) * zn

              !surface 2 (opposite force)
              la(1,nn2(kk)) = la(1,nn2(kk)) + fac1* (p1*surf) * xn
              la(2,nn2(kk)) = la(2,nn2(kk)) + fac1* (p1*surf) * yn
              la(3,nn2(kk)) = la(3,nn2(kk)) + fac1* (p1*surf) * zn

              if(dt1 > zero)then
                if (ms(nng1(kk)) > em20)then
                  rho_ = half*(rho1+rho2)
                  ssp_ = half*(ssp1+ssp2)
                  ms_ = half * (ms(nng1(kk)) + ms(nng2(kk)))
                  stifn(nng1(kk))=stifn(nng1(kk))+(two*(surf*rho_*ssp_)**2)/ms_
                  stifn(nng2(kk))=stifn(nng2(kk))+(two*(surf*rho_*ssp_)**2)/ms_
                endif
              end if
            enddo
            !/th/surf (massflow, velocity, pressure)
            fsavsurf(2,surf_iid1) = fsavsurf(2,surf_iid1) + rho_*surf*vf     !rho.s.u = dm/dt
            fsavsurf(3,surf_iid1) = fsavsurf(3,surf_iid1) + surf*vf         !s.u
            fsavsurf(4,surf_iid1) = fsavsurf(4,surf_iid1) + surf*p2                !s.p
            fsavsurf(6,surf_iid1) = fsavsurf(6,surf_iid1) + rho_*surf*vf*dt1 ! m<-m+dm (cumulative)

            fsavsurf(2,surf_iid2) = fsavsurf(2,surf_iid2) + rho_*surf*vf     !rho.s.u = dm/dt
            fsavsurf(3,surf_iid2) = fsavsurf(3,surf_iid2) + surf*vf         !s.u
            fsavsurf(4,surf_iid2) = fsavsurf(4,surf_iid2) + surf*p1                !s.p
            fsavsurf(6,surf_iid2) = fsavsurf(6,surf_iid2) + rho_*surf*vf*dt1 ! m<-m+dm (cumulative)

            ! -------------
            ! NON PARALLEL EBCS ARE NOT YET COMPATIBLE WITH PARITH/ON
            ! SEE RDRESB.F and W_PON.F for elem_adress allocation
            ! for parith/on option : update forces in fsky array (specific assembly)
            if(iparit == -1 ) then !unplug
               do kk=1,int(npt)
                 adress = elem_adress(kk,is) ! adress of fsky array for element is and node kk
                 fsky(1,adress) = - fac1* (p2*surf) * xn
                 fsky(2,adress) = - fac1* (p2*surf) * yn
                 fsky(3,adress) = - fac1* (p2*surf) * zn
                 fsky(4:8,adress) = zero
                 adress = elem_adress(kk,nseg/2+is) ! surface 2 (opposite normal)
                 fsky(1,adress) = + fac1* (p1*surf) * xn
                 fsky(2,adress) = + fac1* (p1*surf) * yn
                 fsky(3,adress) = + fac1* (p1*surf) * zn
                 fsky(4:8,adress) = zero
               enddo
            endif
            ! -------------
          enddo !next is

          ! -------------
          ! for parith/off option : update directly the acceleration array a() : no specific assembly
          if(iparit >= 0) then !pluged for both parith/on & parith/off
            do ii=1,nod
              num=liste(ii)
              a(1,num)=a(1,num)+la(1,ii)
              a(2,num)=a(2,num)+la(2,ii)
              a(3,num)=a(3,num)+la(3,ii)
            enddo
          endif

          ! -------------


        return
        end subroutine ebcs12_cyclic

      end module ebcs12_cyclic_mod
