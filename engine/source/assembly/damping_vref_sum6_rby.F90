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
      module damping_vref_sum6_rby_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes damping forces for /DAMP/VREL with RBODY and make assembly on main node
!=======================================================================================================================
!
        subroutine damping_vref_sum6_rby(nsn,igr,id_rby,isk,im,                  &
          igrnod,ngrnod,v,vr,a,                   &
          x,ms,dt1,numnod,tagslv_rby,             &
          nrbykin,rby6,rby6_c,weight,lskew,       &
          numskw,skew,damp_a,dim,damp,            &
          dw,damp_a2,iparit,size_rby6_c)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use GROUPDEF_MOD , only: GROUP_
          use constant_mod , only: pi,one,zero,two,half,em20,four
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
          type(GROUP_),                              intent(in) :: igrnod(ngrnod)              !< group od nodes structure
          integer,                                   intent(in) :: nsn                         !< number of nodes in /DAMP/VREL
          integer,                                   intent(in) :: igr                         !< node group id
          integer,                                   intent(in) :: isk                         !< skew id
          integer,                                   intent(in) :: im                          !< ib of rigid body main node
          integer,                                   intent(in) :: id_rby                      !< id of rigid body
          integer,                                   intent(in) :: ngrnod                      !< number of groups of nodes
          integer,                                   intent(in) :: numnod                      !< number of nodes
          integer,                                   intent(in) :: tagslv_rby(numnod)          !< tag of slaves nodes of rigid bodies
          integer,                                   intent(in) :: iparit                      !< PARITH/ON flag
          integer,                                   intent(in) :: nrbykin                     !< number of rigid bodies
          integer,                                   intent(in) :: weight(numnod)              !< weight
          integer,                                   intent(in) :: lskew                       !< first dimension of array skew
          integer,                                   intent(in) :: numskw                      !< number of skews
          integer,                                   intent(in) :: dim                         !< first dimension of array damp
          integer,                                   intent(in) :: size_rby6_c                 !< dimension of array rby6c
          my_real,                                   intent(in) :: damp_a(3)                   !< damping coefficient in 3 directions
          my_real,                                   intent(in) :: v(3,numnod)                 !< nodal velocity
          my_real,                                   intent(in) :: vr(3,numnod)                !< nodal rotational velocity
          my_real,                                intent(inout) :: a(3,numnod)                 !< nodal force
          my_real,                                   intent(in) :: x(3,numnod)                 !< node position
          my_real,                                   intent(in) :: ms(numnod)                  !< nodal mass
          my_real,                                   intent(in) :: skew(lskew,numskw)          !< main structure for skews
          my_real,                                intent(inout) :: damp(dim,numnod)            !< damping force at previous time step
          double precision,                       intent(inout) :: dw                          !< increment of external forces work
          my_real,                                   intent(in) :: dt1                         !< time step
          my_real,                                   intent(in) :: damp_a2(3)                  !< quadratic damping coefficient in 3 directions
          double precision,                       intent(inout) :: rby6(8,6,nrbykin)           !< working array for rigid body assembly
          double precision,                       intent(inout) :: rby6_c(2,6,size_rby6_c)         !< working array for rigid body damping assembly
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,n
          my_real :: v_refmx,v_refmy,v_refmz,fdamp_x,fdamp_y,fdamp_z
          my_real :: fdamp_x_old,fdamp_y_old,fdamp_z_old
          my_real :: dvskw_x,dvskw_y,dvskw_z,fskw_x,fskw_y,fskw_z,dv_x,dv_y,dv_z
          my_real :: dist2,dt2n,fac,bb,stif_damp,stifr_damp,cc
          my_real :: f1(nsn),f2(nsn),f3(nsn),f4(nsn),f5(nsn),f6(nsn),f7(nsn),f8(nsn),f9(nsn),f10(nsn)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          f1(1:nsn) =  zero
          f2(1:nsn) =  zero
          f3(1:nsn) =  zero
          f4(1:nsn) =  zero
          f5(1:nsn) =  zero
          f6(1:nsn) =  zero
          f7(1:nsn) =  zero
          f8(1:nsn) =  zero
          f9(1:nsn) =  zero
          f10(1:nsn) =  zero

!-------- computation of nodal stiffness -----
#include "vectorize.inc"
          do n=1,nsn
            i=igrnod(igr)%entity(n)
            if (tagslv_rby(i)==0) then
              cc = ms(i)*max(damp_a(1),damp_a(2),damp_a(3))
              dist2 =(x(1,i)-x(1,im))**2+(x(2,i)-x(2,im))**2+(x(3,i)-x(3,im))**2
              f1(n) = cc*weight(i)
              f2(n) = cc*dist2*weight(i)
              f9(n) = four*cc*cc*weight(i)/ms(i)
              f10(n) = four*cc*cc*dist2*weight(i)/ms(i)
            endif
          enddo
!
          if (isk == 1) then
!-------- computation of forces in global skew -----
#include "vectorize.inc"
            do n=1,nsn
              i=igrnod(igr)%entity(n)
              if (tagslv_rby(i)==0) then
                v_refmx = v(1,im)+vr(2,im)*(x(3,i)-x(3,im))-vr(3,im)*(x(2,i)-x(2,im))
                v_refmy = v(2,im)+vr(3,im)*(x(1,i)-x(1,im))-vr(1,im)*(x(3,i)-x(3,im))
                v_refmz = v(3,im)+vr(1,im)*(x(2,i)-x(2,im))-vr(2,im)*(x(1,i)-x(1,im))
                dv_x = v(1,i)- v_refmx
                dv_y = v(2,i)- v_refmy
                dv_z = v(3,i)- v_refmz
!
                fdamp_x = -ms(i)*dv_x*(damp_a(1) + damp_a2(1)*dv_x)
                fdamp_y = -ms(i)*dv_y*(damp_a(2) + damp_a2(2)*dv_y)
                fdamp_z = -ms(i)*dv_z*(damp_a(3) + damp_a2(3)*dv_z)
                fdamp_x_old = damp(1,i)
                fdamp_y_old = damp(2,i)
                fdamp_z_old = damp(3,i)
                damp(1,i) = fdamp_x
                damp(2,i) = fdamp_y
                damp(3,i) = fdamp_z
!
                a(1,i) = a(1,i)+fdamp_x
                a(2,i) = a(2,i)+fdamp_y
                a(3,i) = a(3,i)+fdamp_z
                dw = dw + dt1*half*((fdamp_x+fdamp_x_old)*(v(1,i)-v_refmx)       &
                  +(fdamp_y+fdamp_y_old)*(v(2,i)-v_refmy)               &
                  +(fdamp_z+fdamp_z_old)*(v(3,i)-v_refmz))*weight(i)
!
                f3(n) = -fdamp_x*weight(i)
                f4(n) = -fdamp_y*weight(i)
                f5(n) = -fdamp_z*weight(i)
                f6(n) = (fdamp_y*(x(3,i)-x(3,im))-fdamp_z*(x(2,i)-x(2,im)))*weight(i)
                f7(n) = (fdamp_z*(x(1,i)-x(1,im))-fdamp_x*(x(3,i)-x(3,im)))*weight(i)
                f8(n) = (fdamp_x*(x(2,i)-x(2,im))-fdamp_y*(x(1,i)-x(1,im)))*weight(i)
              endif
            enddo
          else
!-------- computation of forces in local skew -----
#include "vectorize.inc"
            do n=1,nsn
              i=igrnod(igr)%entity(n)
              if (tagslv_rby(i)==0) then
                v_refmx = v(1,im)+vr(2,im)*(x(3,i)-x(3,im))-vr(3,im)*(x(2,i)-x(2,im))
                v_refmy = v(2,im)+vr(3,im)*(x(1,i)-x(1,im))-vr(1,im)*(x(3,i)-x(3,im))
                v_refmz = v(3,im)+vr(1,im)*(x(2,i)-x(2,im))-vr(2,im)*(x(1,i)-x(1,im))
!
                dvskw_x = skew(1,isk)*(v(1,i)-v_refmx)+skew(2,isk)*(v(2,i)-v_refmy)+skew(3,isk)*(v(3,i)-v_refmz)
                dvskw_y = skew(4,isk)*(v(1,i)-v_refmx)+skew(5,isk)*(v(2,i)-v_refmy)+skew(6,isk)*(v(3,i)-v_refmz)
                dvskw_z = skew(7,isk)*(v(1,i)-v_refmx)+skew(8,isk)*(v(2,i)-v_refmy)+skew(9,isk)*(v(3,i)-v_refmz)
                fskw_x = -ms(i)*dvskw_x*(damp_a(1) + damp_a2(1)*dvskw_x)
                fskw_y = -ms(i)*dvskw_y*(damp_a(2) + damp_a2(2)*dvskw_y)
                fskw_z = -ms(i)*dvskw_z*(damp_a(3) + damp_a2(3)*dvskw_z)
!
                fdamp_x = skew(1,isk)*fskw_x+skew(4,isk)*fskw_y+skew(7,isk)*fskw_z
                fdamp_y = skew(2,isk)*fskw_x+skew(5,isk)*fskw_y+skew(8,isk)*fskw_z
                fdamp_z = skew(3,isk)*fskw_x+skew(6,isk)*fskw_y+skew(9,isk)*fskw_z
                fdamp_x_old = damp(1,i)
                fdamp_y_old = damp(2,i)
                fdamp_z_old = damp(3,i)
                damp(1,i) = fdamp_x
                damp(2,i) = fdamp_y
                damp(3,i) = fdamp_z
!
                a(1,i) = a(1,i)+fdamp_x
                a(2,i) = a(2,i)+fdamp_y
                a(3,i) = a(3,i)+fdamp_z
                dw = dw + dt1*half*((fdamp_x+fdamp_x_old)*(v(1,i)-v_refmx)       &
                  +(fdamp_y+fdamp_y_old)*(v(2,i)-v_refmy)               &
                  +(fdamp_z+fdamp_z_old)*(v(3,i)-v_refmz))*weight(i)
!
                f3(n) = -fdamp_x*weight(i)
                f4(n) = -fdamp_y*weight(i)
                f5(n) = -fdamp_z*weight(i)
                f6(n) = (fdamp_y*(x(3,i)-x(3,im))-fdamp_z*(x(2,i)-x(2,im)))*weight(i)
                f7(n) = (fdamp_z*(x(1,i)-x(1,im))-fdamp_x*(x(3,i)-x(3,im)))*weight(i)
                f8(n) = (fdamp_x*(x(2,i)-x(2,im))-fdamp_y*(x(1,i)-x(1,im)))*weight(i)
              endif
            enddo
          endif

          if (iparit > 0) then
!--------   PARITH/ON assembly -----
            call SUM_6_FLOAT(1,nsn,f1,rby6_c(1,1,id_rby),2)
            call SUM_6_FLOAT(1,nsn,f2,rby6_c(2,1,id_rby),2)
            call SUM_6_FLOAT(1,nsn,f3,rby6(3,1,id_rby),8)
            call SUM_6_FLOAT(1,nsn,f4,rby6(4,1,id_rby),8)
            call SUM_6_FLOAT(1,nsn,f5,rby6(5,1,id_rby),8)
            call SUM_6_FLOAT(1,nsn,f6,rby6(6,1,id_rby),8)
            call SUM_6_FLOAT(1,nsn,f7,rby6(7,1,id_rby),8)
            call SUM_6_FLOAT(1,nsn,f8,rby6(8,1,id_rby),8)
            call SUM_6_FLOAT(1,nsn,f9,rby6(1,1,id_rby),8)
            call SUM_6_FLOAT(1,nsn,f10,rby6(2,1,id_rby),8)
          else
!--------   PARITH/OFF assembly -----
#include "vectorize.inc"
            do n=1,nsn
              rby6_c(1,1,id_rby) = rby6_c(1,1,id_rby) + f1(n)
              rby6_c(2,1,id_rby) = rby6_c(2,1,id_rby) + f2(n)
              rby6(3,1,id_rby) = rby6(3,1,id_rby) + f3(n)
              rby6(4,1,id_rby) = rby6(4,1,id_rby) + f4(n)
              rby6(5,1,id_rby) = rby6(5,1,id_rby) + f5(n)
              rby6(6,1,id_rby) = rby6(6,1,id_rby) + f6(n)
              rby6(7,1,id_rby) = rby6(7,1,id_rby) + f7(n)
              rby6(8,1,id_rby) = rby6(8,1,id_rby) + f8(n)
              rby6(1,1,id_rby) = rby6(1,1,id_rby) + f9(n)
              rby6(2,1,id_rby) = rby6(2,1,id_rby) + f10(n)
            enddo
          endif
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_vref_sum6_rby
      end module damping_vref_sum6_rby_mod
