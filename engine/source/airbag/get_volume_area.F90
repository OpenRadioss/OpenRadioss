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
      !||    get_volume_area_mod   ../engine/source/airbag/get_volume_area.F90
      !||--- called by ------------------------------------------------------
      !||    monvol0               ../engine/source/airbag/monvol0.F
      !||====================================================================
      module get_volume_area_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine computes the volume & area of monitored volumes
!! \details * local computation of volume & area for each monitored volume
!!          * global mpi comm (1 comm for all moniotred volumes)
!!          * reduction of volume & area 
      !||====================================================================
      !||    get_volume_area       ../engine/source/airbag/get_volume_area.F90
      !||--- called by ------------------------------------------------------
      !||    monvol0               ../engine/source/airbag/monvol0.F
      !||--- calls      -----------------------------------------------------
      !||    omp_get_num_threads   ../engine/source/engine/openmp_stub.F90
      !||    omp_get_thread_num    ../engine/source/engine/openmp_stub.F90
      !||    spmd_exch_fr6         ../engine/source/mpi/kinematic_conditions/spmd_exch_fr6.F
      !||    sum_6_float           ../engine/source/system/parit.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||    groupdef_mod          ../common_source/modules/groupdef_mod.F
      !||    monvol_struct_mod     ../engine/share/modules/monvol_struct_mod.F
      !||====================================================================
        subroutine get_volume_area(ispmd,nspmd,numelc,numeltg, &
                       nvolu,nsurf,intbag,sporo,&
                       numnod,sicontact,nimv,nrvolu,           &
                       monvol,rvolu,vol,x,                     & 
                       normal,icontact,poro,fr_mv,  &
                       frontier_global_mv,t_monvoln,igrsurf )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use monvol_struct_mod , only : monvol_struct_
          use constant_mod
          use groupdef_mod , only : surf_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ispmd !< mpi task id
          integer, intent(in) :: nspmd !< total number of mpi tasks
          integer, intent(in) :: numelc !< number of shell
          integer, intent(in) :: numeltg !< number of 3n shell
          integer, intent(in) :: nvolu !< number of monitored volume
          integer, intent(in) :: nsurf !< number of surface
          integer, intent(in) :: intbag !<
          integer, intent(in) :: numnod !< number of node
          integer, intent(in) :: nimv !< first dim of monvol
          integer, intent(in) :: nrvolu !< first dim of rvolu
          integer, intent(in) :: sicontact !< dimension of icontact array
          integer, intent(in) :: sporo !< dimension of poro array
          integer, dimension(sicontact), intent(in) :: icontact
          integer, dimension(nspmd+2,nvolu), intent(in) :: fr_mv !< mpi frontier per monitored volume
          integer, dimension(nspmd+2), intent(in) :: frontier_global_mv !< global mpi frontier 
          integer, dimension(nvolu*nimv), intent(in) :: monvol !< monitored volume data
          my_real, dimension(3,numnod), intent(in) :: x !< position array
          my_real, dimension(nvolu*nrvolu), intent(inout) :: rvolu !< monitored volume data (real)
          my_real, dimension(sporo), intent(inout) :: poro !<
          my_real, dimension(nvolu), intent(inout) :: vol !< volume of airbag
          my_real, dimension(3,numnod+numelc+numeltg), intent(inout) :: normal !< normal of node
          type(monvol_struct_), dimension(nvolu), intent(inout) :: t_monvoln !< monvol structure
          type(surf_), dimension(nsurf), intent(in) :: igrsurf !< surface structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: computation_needed
          integer :: segment_number,surf_id,ityp
          integer :: number_entity
          integer :: monvol_address,rvolu_address
          integer :: i,ijk,ii,is,k,nod1,nod2,nod3,nod4
          my_real :: area,xx,yy,zz,x13,y13,z13,x24,y24,z24
          my_real :: nx, ny, nz
          my_real, dimension(:), allocatable :: f1, f2
          double precision, dimension(2,6,nvolu) :: frmv6
          double precision, dimension(2,6) :: frmv6_l
          integer :: first,last,nthread,itask
          integer, external :: omp_get_thread_num,omp_get_num_threads
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
          frmv6(1:2,1:6,1:nvolu) = zero
!$omp parallel private(ii,nod1,nod2,nod3,nod4,xx,yy,zz,x13,y13,z13,x24,y24,z24,computation_needed,ityp) &
!$omp& private(surf_id,segment_number,monvol_address,rvolu_address,nx,ny,nz,number_entity,itask,first,last,frmv6_l)
          itask = omp_get_thread_num()
          nthread = omp_get_num_threads()
          monvol_address = 1
          rvolu_address = 1
          do ijk=1,nvolu
            ityp = monvol(monvol_address+1) ! get the type of the airbag
            surf_id = monvol(monvol_address+3) ! get the id of the surface
            segment_number = igrsurf(surf_id)%nseg ! get the number of segment of the surface "surf_id"
            computation_needed = .true.
            if(ityp==6.or.ityp==8) computation_needed = .false.
            if(fr_mv(ispmd+1,ijk)==0.and.fr_mv(nspmd+2,ijk)/=ispmd+1) computation_needed = .false.

            if(computation_needed) then
!$omp single
              allocate(f1(segment_number + t_monvoln(ijk)%nb_fill_tri), f2(segment_number + t_monvoln(ijk)%nb_fill_tri))
!$omp end single
              if(intbag==0)then
!$omp do schedule(guided)
                do i=1,segment_number
                  ii=igrsurf(surf_id)%elem(i)
                  if(igrsurf(surf_id)%eltyp(i)==7)then
                    ii=ii+numelc
                  elseif(igrsurf(surf_id)%eltyp(i)/=3)then
                    ii=i+numelc+numeltg
                  endif
                  nod1 = igrsurf(surf_id)%nodes(i,1)
                  nod2 = igrsurf(surf_id)%nodes(i,2)
                  nod3 = igrsurf(surf_id)%nodes(i,3)
                  nod4 = igrsurf(surf_id)%nodes(i,4)
                  xx=half*(x(1,nod1)+x(1,nod2))
                  yy=half*(x(2,nod1)+x(2,nod2))
                  zz=half*(x(3,nod1)+x(3,nod2)) 
                  x13=x(1,nod3)-x(1,nod1)
                  y13=x(2,nod3)-x(2,nod1)
                  z13=x(3,nod3)-x(3,nod1)
                  x24=x(1,nod4)-x(1,nod2)
                  y24=x(2,nod4)-x(2,nod2)
                  z24=x(3,nod4)-x(3,nod2)
                  t_monvoln(ijk)%normal(1,i)=half*(y13*z24-y24*z13)
                  t_monvoln(ijk)%normal(2,i)=half*(z13*x24-z24*x13)
                  t_monvoln(ijk)%normal(3,i)=half*(x13*y24-x24*y13)
                  normal(1,ii) = t_monvoln(ijk)%normal(1,i)
                  normal(2,ii) = t_monvoln(ijk)%normal(2,i)
                  normal(3,ii) = t_monvoln(ijk)%normal(3,i)
                  f1(i) = sqrt( t_monvoln(ijk)%normal(1,i)**2+t_monvoln(ijk)%normal(2,i)**2+t_monvoln(ijk)%normal(3,i)**2 )
                  f2(i) = third*( t_monvoln(ijk)%normal(1,i)*xx+t_monvoln(ijk)%normal(2,i)*yy+t_monvoln(ijk)%normal(3,i)*zz )
                enddo
!$omp end do
              else
!$omp do schedule(guided)
                do i=1,segment_number
                  ii=igrsurf(surf_id)%elem(i)
                  nod1 = igrsurf(surf_id)%nodes(i,1)
                  nod2 = igrsurf(surf_id)%nodes(i,2)
                  nod3 = igrsurf(surf_id)%nodes(i,3)
                  nod4 = igrsurf(surf_id)%nodes(i,4)
                  if(igrsurf(surf_id)%eltyp(i)==3)then
                    poro(ii)=zero
                    if(icontact(nod1)/=0)poro(ii)=poro(ii)+fourth
                    if(icontact(nod2)/=0)poro(ii)=poro(ii)+fourth
                    if(icontact(nod3)/=0)poro(ii)=poro(ii)+fourth
                    if(icontact(nod4)/=0)poro(ii)=poro(ii)+fourth
                  elseif(igrsurf(surf_id)%eltyp(i)==7)then
                    ii=ii+numelc
                    poro(ii)=zero
                    if(icontact(nod1)/=0)poro(ii)=poro(ii)+third
                    if(icontact(nod2)/=0)poro(ii)=poro(ii)+third
                    if(icontact(nod3)/=0)poro(ii)=poro(ii)+third
                  else
                    ii=i+numelc+numeltg
                    poro(ii)=zero
                    if(icontact(nod1)/=0)poro(ii)=poro(ii)+fourth
                    if(icontact(nod2)/=0)poro(ii)=poro(ii)+fourth
                    if(icontact(nod3)/=0)poro(ii)=poro(ii)+fourth
                    if(icontact(nod4)/=0)poro(ii)=poro(ii)+fourth    
                  endif
                  xx=half*(x(1,nod1)+x(1,nod2))
                  yy=half*(x(2,nod1)+x(2,nod2))
                  zz=half*(x(3,nod1)+x(3,nod2))
                  x13=x(1,nod3)-x(1,nod1)
                  y13=x(2,nod3)-x(2,nod1)
                  z13=x(3,nod3)-x(3,nod1)
                  x24=x(1,nod4)-x(1,nod2)
                  y24=x(2,nod4)-x(2,nod2)
                  z24=x(3,nod4)-x(3,nod2)
                  t_monvoln(ijk)%normal(1,i)=half*(y13*z24-y24*z13)
                  t_monvoln(ijk)%normal(2,i)=half*(z13*x24-z24*x13)
                  t_monvoln(ijk)%normal(3,i)=half*(x13*y24-x24*y13)
                  normal(1,ii) = t_monvoln(ijk)%normal(1,i)
                  normal(2,ii) = t_monvoln(ijk)%normal(2,i)
                  normal(3,ii) = t_monvoln(ijk)%normal(3,i)
                  f1(i) = sqrt( t_monvoln(ijk)%normal(1,i)**2+t_monvoln(ijk)%normal(2,i)**2+t_monvoln(ijk)%normal(3,i)**2 )
                  f2(i) = third*( t_monvoln(ijk)%normal(1,i)*xx+t_monvoln(ijk)%normal(2,i)*yy+t_monvoln(ijk)%normal(3,i)*zz )
                enddo
!$omp end do
              endif
              number_entity = segment_number
              if (ispmd + 1 == fr_mv(nspmd+2,ijk)) then
                number_entity = number_entity + t_monvoln(ijk)%nb_fill_tri
!$omp do schedule(guided)
                do i = 1, t_monvoln(ijk)%nb_fill_tri
                  nod1 = t_monvoln(ijk)%fill_tri(3 * (i - 1) + 1)
                  nod2 = t_monvoln(ijk)%fill_tri(3 * (i - 1) + 2)
                  nod3 = t_monvoln(ijk)%fill_tri(3 * (i - 1) + 3)
                  nod4 = nod3
                  xx=half*(x(1,nod1)+x(1,nod2))
                  yy=half*(x(2,nod1)+x(2,nod2))
                  zz=half*(x(3,nod1)+x(3,nod2)) 
                  x13=x(1,nod3)-x(1,nod1)
                  y13=x(2,nod3)-x(2,nod1)
                  z13=x(3,nod3)-x(3,nod1)
                  x24=x(1,nod4)-x(1,nod2)
                  y24=x(2,nod4)-x(2,nod2)
                  z24=x(3,nod4)-x(3,nod2)
                  nx=half*(y13*z24-y24*z13)
                  ny=half*(z13*x24-z24*x13)
                  nz=half*(x13*y24-x24*y13)
                  f1(segment_number + i) = sqrt( nx**2+ny**2+nz**2 )
                  f2(segment_number + i) = third*( nx*xx+ny*yy+nz*zz )
                enddo
!$omp end do
              endif
              first = 1 + number_entity * itask / nthread 
              last = number_entity * (itask+1) / nthread 
              frmv6_l(1:2,1:6) = zero
              call sum_6_float(first, last, f1, frmv6_l(1,1), 2)
              call sum_6_float(first, last, f2, frmv6_l(2,1), 2)
!$omp critical
              do i=1,6
                frmv6(1,i,ijk) = frmv6(1,i,ijk) + frmv6_l(1,i)
                frmv6(2,i,ijk) = frmv6(2,i,ijk) + frmv6_l(2,i)
              enddo
!$omp end critical

!$omp barrier

!$omp single
              deallocate( f1,f2 )
!$omp end single
            endif
            monvol_address = monvol_address + nimv
            rvolu_address = rvolu_address + nrvolu

          enddo
!$omp end parallel

          if(nspmd>1) then
            call spmd_exch_fr6(frontier_global_mv,frmv6,2*6*nvolu)
          endif
          monvol_address = 1
          rvolu_address = 1
          do ijk=1,nvolu
            ityp = monvol(monvol_address+1) ! get the type of the airbag
            computation_needed = .true.
            if(ityp==6.or.ityp==8) computation_needed = .false.
            if(fr_mv(ispmd+1,ijk)==0.and.fr_mv(nspmd+2,ijk)/=ispmd+1) computation_needed = .false.

            if(computation_needed) then
              area = frmv6(1,1,ijk)+frmv6(1,2,ijk)+frmv6(1,3,ijk)+ &
                     frmv6(1,4,ijk)+frmv6(1,5,ijk)+frmv6(1,6,ijk)
              vol(ijk)= frmv6(2,1,ijk)+frmv6(2,2,ijk)+frmv6(2,3,ijk)+ &
                     frmv6(2,4,ijk)+frmv6(2,5,ijk)+frmv6(2,6,ijk)
              rvolu(rvolu_address-1+18) = area
            endif
            monvol_address = monvol_address + nimv
            rvolu_address = rvolu_address + nrvolu
          enddo

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_volume_area
      end module get_volume_area_mod
