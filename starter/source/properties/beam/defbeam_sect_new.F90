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
      module defbeam_sect_new_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine compute new predefined sections for integrated beams (position and weight) Isect 10-31
!=======================================================================================================================
!
        subroutine defbeam_sect_new(geo,npropg,isect,intr,intr_max,nip,area,l,nb_dim)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod ,only : half,one,zero,two,three,fourth,pi
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
          integer,                                   intent(in) :: isect                       !< section type
          integer,                                   intent(in) :: intr                        !< order of integration
          integer,                                intent(inout) :: intr_max                    !< max allowed order of integration
          integer,                                intent(inout) :: nip                         !< number of integration points
          integer,                                intent(inout) :: nb_dim                      !< number of needed dimensions
          integer,                                   intent(in) :: npropg                      !< size of array geo
          my_real,                                intent(inout) :: geo(npropg)                 !< main array for properties
          my_real,                                   intent(in) :: l(6)                        !< section dimensions
          my_real,                                intent(inout) :: area                        !< section area
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer i,j,ip,nc,ns,ipy,ipz,ipa,jj
          my_real ai,yi,zi,wi,r1,r2,r3,r4,d2, d3, d4
          my_real area1_i,area2_i,area3_i,area4_i,dy1,dy2,dz1,dz2,y1_0,z1_0
          my_real y2_0,z2_0,fac,fac2,fac3,fac4,dl,dh,l_sup,l_inf
          my_real r_0,r_sup,r_inf,dr,phi_0,phi,dphi
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          ipy    = 200
          ipz    = 300
          ipa    = 400
!
! ----------------------------------------------------------------------------------------------------------------------
          select case (isect)
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
           case (10) ! i-shape section (l(1)-l(2)-l(3)-l(4))
!                     -----
!                       |
!                     -----
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 3*(2*intr+3)
            intr_max = 15
            area = l(1)*l(3)-(l(1)-l(4))*(l(3)-two*l(2))
            fac = one/(2*intr+3)
            area1_i = l(1)*l(2)*fac
            area2_i = l(4)*(l(3)-two*l(2))*fac
            dy1 = l(1)*fac
            y1_0 = -half*l(1)+half*dy1
            dz2 = -(l(3)-two*l(2))*fac
            z2_0 = half*l(3)-l(2)+half*dz2
            ip = 0
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=zero
              geo(ipz+ip)=z2_0+(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (11) ! channel section (l(1)-l(2)-l(3)-l(4))
!                      ----
!                      |
!                      ----
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 3*(intr+3)
            intr_max = 30
            area = l(1)*l(3)-(l(1)-l(4))*(l(3)-two*l(2))
            fac = one/(intr+3)
            area1_i = l(1)*l(2)*fac
            area2_i = l(4)*(l(3)-two*l(2))*fac
            dy1 = l(1)*fac
            y1_0 = -half*l(1)+half*dy1
            dz2 = -(l(3)-two*l(2))*fac
            z2_0 = half*l(3)-l(2)+half*dz2
            ip = 0
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=-half*(l(1)-l(4))
              geo(ipz+ip)=z2_0+(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (12) ! l-shape section (l(1)-l(2)-l(3)-l(4))
!                      |
!                      |
!                      ----
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 2*(intr+2)+1
            intr_max = 47
            area = l(1)*l(3)-(l(1)-l(4))*(l(3)-l(2))
            fac = one/(intr+2)
            area1_i = l(4)*(l(3)-l(2))*fac
            area2_i = l(2)*(l(1)-l(4))*fac
            dz1 = -(l(3)-l(2))*fac
            z1_0 = half*l(3)+half*dz1
            dy2 = (l(1)-l(4))*fac
            y2_0 = -half*l(1)+l(4)+half*dy2
            ip = 0
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=half*(-l(1)+l(4))
              geo(ipz+ip)=z1_0 +(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
            ip = ip+1
            geo(ipy+ip)=half*(-l(1)+l(4))
            geo(ipz+ip)=half*(-l(3)+l(2))
            geo(ipa+ip)=l(2)*l(4)
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=y2_0 +(i-1)*dy2
              geo(ipz+ip)=half*(-l(3)+l(2))
              geo(ipa+ip)=area2_i
            enddo
! -----------------------------------------------------------------------------------------------------------------------
           case (13) ! t-shape section (l(1)-l(2)-l(3)-l(4))
!                    -----
!                      |
!                      |
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 4*(intr+2)+1
            intr_max = 22
            area = l(1)*l(3)-(l(1)-l(4))*(l(3)-l(2))
            fac = one/(intr+2)
            fac2 = one/(2*(intr+2))
            area1_i = half*(l(1)-l(4))*l(2)*fac
            area2_i = l(4)*(l(3)-l(2))*fac2
            dy1 = half*(l(1)-l(4))*fac
            y1_0 = -half*l(1)+half*dy1
            dz2 = -(l(3)-l(2))*fac2
            z2_0 = half*l(3)-l(2)+half*dz2
            ip = 0
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            ip = ip+1
            geo(ipy+ip)=zero
            geo(ipz+ip)=half*(l(3)-l(2))
            geo(ipa+ip)=l(2)*l(4)
            y1_0 = half*l(4)+half*dy1
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*(intr+2)
              ip = ip+1
              geo(ipy+ip)=zero
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (14) ! box-shape section (l(1)-l(2)-l(3)-l(4))
!                   -------
!                   ||   ||
!                   -------
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 2*(intr+3)+2*(intr+1)
            intr_max = 23
            area = l(1)*l(3)-(l(1)-two*l(4))*(l(3)-two*l(2))
            fac = one/(intr+3)
            fac2 = one/(intr+1)
            area1_i = l(1)*l(2)*fac
            area2_i = l(4)*(l(3)-two*l(2))*fac2
            dy1 = l(1)*fac
            dz2 = -(l(3)-two*l(2))*fac2
            ip = 0
            y1_0 = -half*l(1)+half*dy1
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            z2_0 = half*l(3)-l(2)+half*dz2
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=-half*(l(1)-l(4))
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=half*(l(1)-l(4))
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (15) ! z-shape section (l(1)-l(2)-l(3)-l(4))
!                   ----
!                      |
!                      ----
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 3*(intr+3)
            intr_max = 30
            area = l(1)*l(3)-(l(1)-l(4))*(l(3)-l(2))
            fac = one/(intr+3)
            area1_i = half*(l(1)+l(4))*l(2)*fac
            area2_i = l(4)*(l(3)-two*l(2))*fac
            dy1 = half*(l(1)+l(4))*fac
            dz2 = -(l(3)-two*l(2))*fac
            ip = 0
            y1_0 = -half*l(1)+half*dy1
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=-y1_0-(i-1)*dy1
              geo(ipz+ip)=-half*(l(3)-l(2))
              geo(ipa+ip)=area1_i
            enddo
            z2_0 = half*l(3)-l(2)+half*dz2
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=zero
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (16) ! trapezoidal section (l(1)-l(2)-l(3))
!                      ---
!                     /   \
!                     -----
! ----------------------------------------------------------------------------------------------------------------------
            nip = (intr+3)*(intr+3)
            nb_dim = 3
            intr_max = 7
            area = half*(l(1)+l(2))*l(3)
            fac = one/(intr+3)
            ip = 0
            dl = (l(1)-l(2))*fac
            dh = -l(3)*fac
            z1_0 = half*l(3)+half*dh
            do j=1,intr+3
              l_sup = l(2)+(j-1)*dl
              l_inf = l(2)+j*dl
              area1_i = fac*half*(l_sup+l_inf)*abs(dh)
              dy1 = half*(l_sup+l_inf)*fac
              y1_0 = -half*half*(l_sup+l_inf)+half*dy1
              do i = 1,intr+3
                ip = ip+1
                geo(ipy+ip)=y1_0 +(i-1)*dy1
                geo(ipz+ip)=z1_0 +(j-1)*dh
                geo(ipa+ip)=area1_i
              enddo
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (17) ! circal section (l(1))
!                     /  \
!                     \  /
! ----------------------------------------------------------------------------------------------------------------------
            nip = (intr+3)*(4*intr+12)
            nb_dim = 1
            intr_max = 2
            area = pi*l(1)**2
            fac = one/(intr+3)
            fac2 = one/(4*intr+12)
            ip = 0
            dr = l(1)*fac
            dphi = pi*two*fac2
            do j=1,intr+3
              r_sup = j*dr
              r_inf = (j-1)*dr
              area1_i = pi*(r_sup**2-r_inf**2)*fac2
              phi_0 = half*dphi
              do i = 1,4*intr+12
                ip = ip+1
                phi = phi_0 + (i-1)*dphi
                geo(ipy+ip)=half*(r_sup+r_inf)*cos(phi)
                geo(ipz+ip)=half*(r_sup+r_inf)*sin(phi)
                geo(ipa+ip)=area1_i
              enddo
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (18) ! tubular section (l(1)-l(2))
!                     /  \
!                     \  /
! ----------------------------------------------------------------------------------------------------------------------
            nip = (intr+3)*(4*intr+12)
            nb_dim = 2
            intr_max = 2
            area = pi*(l(1)**2-l(2)**2)
            fac = one/(intr+3)
            fac2 = one/(4*intr+12)
            ip = 0
            dr = (l(1)-l(2))*fac
            dphi = pi*two*fac2
            do j=1,intr+3
              r_sup = l(2)+j*dr
              r_inf = l(2)+(j-1)*dr
              area1_i = pi*(r_sup**2-r_inf**2)*fac2
              phi_0 = half*dphi
              do i = 1,4*intr+12
                ip = ip+1
                phi = phi_0 + (i-1)*dphi
                geo(ipy+ip)=half*(r_sup+r_inf)*cos(phi)
                geo(ipz+ip)=half*(r_sup+r_inf)*sin(phi)
                geo(ipa+ip)=area1_i
              enddo
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (19) ! i-shape section (l(1)-l(2)-l(3)-l(4)-l(5)-l(6))
!                     -----
!                       |
!                      ---
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 6
            nip = 3*(2*intr+3)
            intr_max = 15
            area = l(3)*l(6)+l(2)*l(5)+l(4)*(l(1)-l(5)-l(6))
            fac = one/(2*intr+3)
            ip = 0
            area1_i = l(3)*l(6)*fac
            dy1 = l(3)*fac
            y1_0 = -half*l(3)+half*dy1
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*(l(1)-l(6))
              geo(ipa+ip)=area1_i
            enddo
            area1_i = l(2)*l(5)*fac
            dy1 = l(2)*fac
            y1_0 = -half*l(2)+half*dy1
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*(l(1)-l(5))
              geo(ipa+ip)=area1_i
            enddo
            area1_i = l(4)*(l(1)-l(5)-l(6))*fac
            dz1 = -(l(1)-l(5)-l(6))*fac
            z1_0 = half*(l(1)-l(5)-l(6))+half*dz1
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=zero
              geo(ipz+ip)=z1_0+(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (20) ! rectangular section (l(1)-l(2))
!                     -----
!                     |   |
!                     -----
! ----------------------------------------------------------------------------------------------------------------------
            nip = (intr+3)*(intr+3)
            nb_dim = 2
            intr_max = 7
            area = l(1)*l(2)
            fac = one/(intr+3)
            ip = 0
            dy1 = l(1)*fac
            dz1 = l(2)*fac
            y1_0 =-half*l(1)+half*dy1
            z1_0 =-half*l(2)+half*dz1
            area1_i = fac*l(1)*fac*l(2)
            do j=1,intr+3
              do i = 1,intr+3
                ip = ip+1
                geo(ipy+ip)=y1_0 +(i-1)*dy1
                geo(ipz+ip)=z1_0 +(j-1)*dz1
                geo(ipa+ip)=area1_i
              enddo
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (21) ! cross-shape section (l(1)-l(2)-l(3)-l(4))
!                       |
!                      ---
!                       |
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 2*(2*intr+4)+4*(intr+1)
            intr_max = 8
            area = l(2)*l(3)+l(4)*l(1)
            ip = 0
            fac = one/(2*intr+4)
            area1_i = half*l(3)*l(2)*fac
            dz1 = -l(3)*fac
            z1_0 = half*l(3)+half*dz1
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=-half*half*l(2)
              geo(ipz+ip)=z1_0 +(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=half*half*l(2)
              geo(ipz+ip)=z1_0 +(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
            fac = one/(intr+1)
            area1_i = half*l(4)*half*l(1)*fac
            dy1 = half*l(1)*fac
            y1_0 = -half*(l(1)+l(2))+half*dy1
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*l(3)-l(4)-half*half*l(4)
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*l(3)-two*l(4)+half*half*l(4)
              geo(ipa+ip)=area1_i
            enddo
            y1_0 = half*(l(1)+l(2))-half*dy1
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=y1_0 -(i-1)*dy1
              geo(ipz+ip)=half*l(3)-l(4)-half*half*l(4)
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=y1_0 -(i-1)*dy1
              geo(ipz+ip)=half*l(3)-two*l(4)+half*half*l(4)
              geo(ipa+ip)=area1_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (22) ! h-shape section (l(1)-l(2)-l(3)-l(4))
!                     |    |
!                     |----|
!                     |    |
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 6*intr+13
            intr_max = 14
            area = l(3)*l(2)+l(4)*l(1)
            fac = one/(2*intr+5)
            fac2 = one/(2*intr+3)
            area1_i = half*l(3)*l(2)*fac
            area2_i = l(4)*l(1)*fac2
            dz1 = -l(3)*fac
            z1_0 = half*l(3)+half*dz1
            dy2 = l(1)*fac2
            y2_0 = -half*l(1)+half*dy2
            ip = 0
            do i = 1,2*intr+5
              ip = ip+1
              geo(ipy+ip)=-half*(l(1)+half*l(2))
              geo(ipz+ip)=z1_0 +(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+5
              ip = ip+1
              geo(ipy+ip)=half*(l(1)+half*l(2))
              geo(ipz+ip)=z1_0 +(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=y2_0+(i-1)*dy2
              geo(ipz+ip)=zero
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (23) ! t section (l(1)-l(2)-l(3)-l(4))
!                      |
!                      |---
!                      |
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 4*(2*intr+4)
            intr_max = 8
            area = l(1)*l(3)+l(2)*l(4)
            fac = one/(2*intr+4)
            area1_i = half*l(1)*l(3)*fac
            area2_i = half*l(4)*l(2)*fac
            dz1 = -l(1)*fac
            z1_0 = half*l(1)+half*dz1
            dy2 = l(2)*fac
            y2_0 = -half*(l(3)+l(2))+half*dy2
            ip = 0
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=half*(l(2)-l(3)+half*l(3))
              geo(ipz+ip)=z1_0 +(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=half*(l(2)+l(3)-half*l(3))
              geo(ipz+ip)=z1_0 +(i-1)*dz1
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=y2_0+(i-1)*dy2
              geo(ipz+ip)=half*half*l(4)
              geo(ipa+ip)=area2_i
            enddo
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=y2_0+(i-1)*dy2
              geo(ipz+ip)=-half*half*l(4)
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (24) ! i section (l(1)-l(2)-l(3)-l(4))
!                      ------
!                        ||
!                      ------
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 2*(2*intr+4)+ 2*(2*intr+3)
            intr_max = 10
            area = l(2)*l(3)+(l(1)+l(2))*(l(4)-l(3))
            fac = one/(2*intr+4)
            fac2 = one/(2*intr+3)
            area1_i = half*(l(1)+l(2))*(l(4)-l(3))*fac
            area2_i = half*l(3)*l(2)*fac2
            dy1 = (l(1)+l(2))*fac
            y1_0 = -half*(l(1)+l(2))+half*dy1
            dz2 = -l(3)*fac2
            z2_0 = half*l(3)+half*dz2
            ip = 0
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*l(4)-half*half*(l(4)-l(3))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(4)+half*half*(l(4)-l(3))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=-half*half*l(2)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=half*half*l(2)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (25) ! Channel section (l(1)-l(2)-l(3)-l(4))
!                      |----
!                      ||
!                      |----
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 4*(intr+3)
            intr_max = 22
            area = l(2)*l(4)+l(1)*(l(4)-l(3))
            fac = one/(intr+3)
            area1_i = half*(l(1)+l(2))*(l(4)-l(3))*fac
            area2_i = half*l(3)*l(2)*fac
            dy1 = (l(1)+l(2))*fac
            y1_0 = -half*(l(1)+l(2))+half*dy1
            dz2 = -l(3)*fac
            z2_0 = half*l(3)+half*dz2
            ip = 0
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*l(4)-half*half*(l(4)-l(3))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(4)+half*half*(l(4)-l(3))
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=-half*(l(1)+l(2)) +half*half*l(2)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=-half*(l(1)+l(2))+three*half*half*l(2)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (26) ! u section (l(1)-l(2)-l(3)-l(4))
!                       |   |
!                       |   |
!                       -----
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 6*intr+7
            intr_max = 15
            area = two*l(1)*l(3)+l(2)*(l(4)-two*l(1))
            fac = one/(2*intr+3)
            fac2 = one/(2*intr+2)
            area1_i = l(2)*l(4)*fac
            area2_i = l(1)*(l(3)-l(2))*fac2
            dy1 = l(4)*fac
            y1_0 = -half*l(4)+half*dy1
            dz2 = -(l(3)-l(2))*fac2
            z2_0 = half*l(3)+half*dz2
            ip = 0
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(3)+half*l(2)
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+2
              ip = ip+1
              geo(ipy+ip)=-half*l(4)+half*l(1)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
            do i = 1,2*intr+2
              ip = ip+1
              geo(ipy+ip)=half*l(4)-half*l(1)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (27) ! T section (l(1)-l(2)-l(3)-l(4))
!                        ||
!                      ------
!                      ------
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 8*intr+12
            intr_max = 11
            area = l(1)*l(3)+l(4)*(l(2)-l(3))
            fac = one/(2*intr+4)
            fac2 = one/(2*intr+2)
            area1_i = half*l(1)*l(3)*fac
            area2_i = half*l(4)*(l(2)-l(3))*fac2
            dy1 = l(1)*fac
            y1_0 = -half*l(1)+half*dy1
            dz2 = -(l(2)-l(3))*fac2
            z2_0 = half*l(2)+half*dz2
            ip = 0
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(2)+three*half*half*l(3)
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(2)+half*half*l(3)
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,2*intr+2
              ip = ip+1
              geo(ipy+ip)=-half*half*l(4)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i
            enddo
            do i = 1,2*intr+2
              ip = ip+1
              geo(ipy+ip)=half*half*l(4)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area2_i

            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (28) ! box section (l(1)-l(2)-l(3)-l(4)-l(5)-l(6))
!                      ------
!                      |  | |
!                      ------
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 6
            nip = 4*intr+8
            intr_max = 23
            area = l(2)*(l(5)+l(6))+(l(3)+l(4))*(l(1)-l(5)-l(6))
            fac = one/(intr+3)
            fac2 = one/(intr+1)
            area1_i = l(1)*l(3)*fac
            area2_i = l(1)*l(4)*fac
            area3_i = l(6)*(l(2)-l(3)-l(4))*fac2
            area4_i = l(5)*(l(2)-l(3)-l(4))*fac2
            dy1 = l(1)*fac
            y1_0 = -half*l(1)+half*dy1
            dz2 = -(l(2)-l(3)-l(4))*fac2
            z2_0 = half*l(2)-l(3)+half*dz2
            ip = 0
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*l(2)-half*l(3)
              geo(ipa+ip)=area1_i
            enddo
            do i = 1,intr+3
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(2)+half*l(4)
              geo(ipa+ip)=area2_i
            enddo
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=-half*l(1)+half*l(6)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area3_i
            enddo
            do i = 1,intr+1
              ip = ip+1
              geo(ipy+ip)=half*l(1)-half*l(5)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area4_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (29) ! hexagon section (l(1)-l(2)-l(3))
!                      ---
!                     /   \
!                     \   /
!                      ---
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 3
            nip = 2*(intr+3)*(intr+3)
            intr_max = 4
            area = (l(2)-l(1))*l(3)
            fac = one/(intr+3)
            ip = 0
            dl = two*l(1)*fac
            dh = half*l(3)*fac
            z1_0 = half*l(3)-half*dh
            do j=1,intr+3
              l_sup = l(2)-two*l(1)+(j-1)*dl
              l_inf = l(2)-two*l(1)+j*dl
              area1_i = fac*half*(l_sup+l_inf)*dh
              dy1 = half*(l_sup+l_inf)*fac
              y1_0 = -half*half*(l_sup+l_inf)+half*dy1
              do i = 1,intr+3
                ip = ip+1
                geo(ipy+ip)=y1_0 +(i-1)*dy1
                geo(ipz+ip)=z1_0 -(j-1)*dh
                geo(ipa+ip)=area1_i
              enddo
            enddo
            z1_0 = -half*l(3)+half*dh
            do j=1,intr+3
              l_sup = l(2)-two*l(1)+(j-1)*dl
              l_inf = l(2)-two*l(1)+j*dl
              area1_i = fac*half*(l_sup+l_inf)*dh
              dy1 = half*(l_sup+l_inf)*fac
              y1_0 = -half*half*(l_sup+l_inf)+half*dy1
              do i = 1,intr+3
                ip = ip+1
                geo(ipy+ip)=y1_0 +(i-1)*dy1
                geo(ipz+ip)=z1_0 +(j-1)*dh
                geo(ipa+ip)=area1_i
              enddo
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (30) ! hat section (l(1)-l(2)-l(3)-l(4))
!                       ----
!                       |  |
!                     ---  ---
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 4
            nip = 8*intr+14
            intr_max = 8
            area = (two*l(4)+l(3))*l(2)+two*(l(1)-l(2))*l(2)
            fac = one/(2*intr+4)
            fac2 = one/(intr+2)
            fac3 = one/(2*intr+3)
            area1_i = l(2)*l(3)*fac
            area2_i = l(2)*l(4)*fac2
            area3_i = l(2)*(l(1)-l(2))*fac3
            ip = 0
            dy1 = l(3)*fac
            y1_0 = -half*l(3)+half*dy1
            do i = 1,2*intr+4
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*l(1)-half*l(2)
              geo(ipa+ip)=area1_i
            enddo
            dy1 = l(4)*fac2
            y1_0 = -half*(l(3)+two*l(4))+half*dy1
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(1)+half*l(2)
              geo(ipa+ip)=area2_i
            enddo
            dy1 = -l(4)*fac2
            y1_0 = half*(l(3)+two*l(4))+half*dy1
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(1)+half*l(2)
              geo(ipa+ip)=area2_i
            enddo
            dz2 = -(l(1)-l(2))*fac3
            z2_0 = half*l(1)-l(2)+half*dz2
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=-half*l(3)+half*l(2)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area3_i
            enddo
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=half*l(3)-half*l(2)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area3_i
            enddo
! ----------------------------------------------------------------------------------------------------------------------
           case (31) ! hat section (l(1)-l(2)-l(3)-l(4)-l(5)-l(6))
!                       ----
!                       |  |
!                      --  --
!                    ----------
! ----------------------------------------------------------------------------------------------------------------------
            nb_dim = 6
            nip = 14*intr+22
            intr_max = 5
            area = (two*l(6)+l(3))*l(4)+two*(l(2)-l(4)-l(5))*l(4)+l(1)*l(5)
            fac = one/(2*intr+5)
            fac2 = one/(intr+2)
            fac3 = one/(2*intr+3)
            fac4 = one/(6*intr+7)
            area1_i = l(4)*l(3)*fac
            area2_i = l(4)*l(6)*fac2
            area3_i = l(4)*(l(2)-l(4)-l(5))*fac3
            area4_i = l(5)*l(1)*fac4
            ip = 0
            dy1 = l(3)*fac
            y1_0 = -half*l(3)+half*dy1
            do i = 1,2*intr+5
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=half*l(2)-half*l(4)
              geo(ipa+ip)=area1_i
            enddo
            dy1 = l(6)*fac2
            y1_0 = -half*(l(3)+two*l(6))+half*dy1
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(2)+l(5)+half*l(4)
              geo(ipa+ip)=area2_i
            enddo
            dy1 = -l(6)*fac2
            y1_0 = half*(l(3)+two*l(6))+half*dy1
            do i = 1,intr+2
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(2)+l(5)+half*l(4)
              geo(ipa+ip)=area2_i
            enddo
            dz2 = -(l(2)-l(4)-l(5))*fac3
            z2_0 = half*l(2)-l(4)+half*dz2
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=-half*l(3)+half*l(4)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area3_i
            enddo
            do i = 1,2*intr+3
              ip = ip+1
              geo(ipy+ip)=half*l(3)-half*l(4)
              geo(ipz+ip)=z2_0 +(i-1)*dz2
              geo(ipa+ip)=area3_i
            enddo
            dy1 = l(1)*fac4
            y1_0 = -half*l(1)+half*dy1
            do i = 1,6*intr+7
              ip = ip+1
              geo(ipy+ip)=y1_0 +(i-1)*dy1
              geo(ipz+ip)=-half*l(2)+half*l(5)
              geo(ipa+ip)=area4_i
            enddo
           case default
          end select
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine defbeam_sect_new
      end module defbeam_sect_new_mod
