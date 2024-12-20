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
MODULE PBLAST_MOD

#include "my_real.inc"

   ! --- data structure for blast parameters (each segment)
   ! ------------------------------------------------------------
   type pblast_struct_
      integer siz
      logical is_reset
      my_real :: dtmin
      my_real, allocatable,dimension(:)    :: pres                                      !< pressure output(workarray)
      my_real, allocatable,dimension(:)    :: cos_theta                                 !< angle on structural face
      my_real, allocatable,dimension(:)    :: p_inci,p_refl,ta,t0,decay_inci,decay_refl !< friedlander parameters
      my_real, dimension(:)  , allocatable :: fx,fy,fz,npt                              !< working arrays (forces)
      integer, dimension(:,:), allocatable :: n                                         !< working array (normal vectors)
      integer, allocatable,dimension(:)    :: tagmsg
   end type pblast_struct_

   type friedlander_params_
      my_real :: p_inci     !< incident pressure (positive phase)
      my_real :: p_inci_    !< incident pressure (positive phase)
      my_real :: p_refl     !< reflected pressure (negative phase)
      my_real :: p_refl_    !< reflected pressure (negative phase)
      my_real :: i_inci     !< incident impulse (positive phase)
      my_real :: i_inci_    !< incident impulse (negative phase)
      my_real :: i_refl     !< reflected impulse (positive phase)
      my_real :: i_refl_    !< reflected impulse (negative phase)
      my_real :: t_a        !< arrival time
      my_real :: dt_0       !< duiration of positive phase
      my_real :: dt_0_      !< duration of negative phase
      my_real :: decay_inci !< decay parameter for incident wave
      my_real :: decay_refl !< decay parameter for reflected wave
   end type friedlander_params_


   ! --- hardcoded tables {g,cm,µs,mbar}
   ! ------------------------------------------------------------
   type pblast_data_
      !-----------------------------------------------------------!
      !            free air burst                                 !
      ! positive parameters                                       !
      ! negative parameter (digitized & extrapolated plot)        !
      !-----------------------------------------------------------!
      my_real :: rw3(256),ta(256),t0(256),t0_(256)
      my_real :: pr(256),pr_(256),irefl(256),irefl_(256)
      my_real :: pso(256),pso_(256),iso(256),iso_(256)
      !-----------------------------------------------------------!
      !            air burst (ground reflecion)                   !
      !-----------------------------------------------------------!
      my_real pra(10,256)   !< mach front pressure
      my_real sri(10,256)   !< scale of reflected impulse
      my_real shtp(10,256)  !< scaled height of triple point
      my_real shtp_abscissa(256)
      my_real angle_pra(10) !< x-bound pra
      my_real angle_sri(10) !< x-bound sri
      my_real shdc(2,10),dshdc    !< x-bounds scaled height distance from charge
      my_real curve_val_2_9(10), curve_val_2_10(10), curve_val_2_13(10) !< data from curve legend
      my_real delta_angle   !< 85 deg - 0deg / (256 points - 1)
      !-----------------------------------------------------------!
      !            surface burst (hemispherical+ground)           !
      !-----------------------------------------------------------!
      my_real :: rw3_surf(256),ir_surf(256),iso_surf(256),pr_surf(256),pso_surf(256),t0_surf(256),ta_surf(256)
   end type pblast_data_

   ! --- data structure for pblast time step
   ! ------------------------------------------------------------
   type pblast_dt_
      integer idt
      my_real ta_inf
      my_real dt
   end type pblast_dt_

   type pblast_
      ! ------------------------------------------------------------
      integer nloadp_b
      type(pblast_struct_),allocatable,dimension(:) :: pblast_tab
      type(pblast_data_) :: pblast_data
      type(pblast_dt_) :: pblast_dt
      ! ------------------------------------------------------------
   end type pblast_

contains

   subroutine pblast_load(pblast)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type(pblast_),intent(inout) :: pblast
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: siz,ierr1,i
      my_real,dimension(:),allocatable :: rtmp
      my_real :: tmp(1)
      integer iad,kk
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------
!   p r e - c o n d i t i o n
!-----------------------------------------------
      if(pblast%nloadp_b<=0)return
!-----------------------------------------------
      allocate (pblast%pblast_tab(pblast%nloadp_b),stat=ierr1);if (ierr1/=0)  call pblast_alloc_error()
      call read_db(tmp,1)
      pblast%pblast_dt%ta_inf = tmp(1)
      !--------------------------------------
      !     reading number of local segments
      !--------------------------------------
      do i=1,pblast%nloadp_b
         call read_i_c(pblast%pblast_tab(i)%siz,1)
         pblast%pblast_tab(i)%is_reset = .false.
      enddo
      !--------------------------------------
      !     reading minimum time step
      !--------------------------------------
      do i=1,pblast%nloadp_b
         call read_db(tmp,1)
         pblast%pblast_tab(i)%dtmin = tmp(1)
      enddo
      !--------------------------------------
      !     reading real buffer (local segments only)
      !--------------------------------------
      do i=1,pblast%nloadp_b
         siz = pblast%pblast_tab(i)%siz
         allocate ( pblast%pblast_tab(i)%cos_theta(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%p_inci(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%p_refl(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%ta(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%t0(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%decay_inci(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%decay_refl(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%fx(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%fy(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%fz(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%n(4,siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%npt(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate ( pblast%pblast_tab(i)%tagmsg(siz),stat=ierr1); if (ierr1/=0) call pblast_alloc_error()
         allocate(rtmp(8*siz))
         call read_db(rtmp,8*siz)
         iad = 0
         do kk=1,siz
            pblast%pblast_tab(i)%cos_theta(kk) = rtmp(iad+1)
            pblast%pblast_tab(i)%p_inci(kk) = rtmp(iad+2)
            pblast%pblast_tab(i)%p_refl(kk) = rtmp(iad+3)
            pblast%pblast_tab(i)%ta(kk) = rtmp(iad+4)
            pblast%pblast_tab(i)%t0(kk) = rtmp(iad+5)
            pblast%pblast_tab(i)%decay_inci(kk) = rtmp(iad+6)
            pblast%pblast_tab(i)%decay_refl(kk) = rtmp(iad+7)
            pblast%pblast_tab(i)%tagmsg(kk) = nint(rtmp(iad+8))
            iad = iad+8
         enddo
         deallocate(rtmp)
      enddo
!-----------------------------------------------
      return
   end

   ! ======================================================================================================================
   !! \brief : Write Error message in case of memory allocation error
   !! \details
   ! ======================================================================================================================
   subroutine pblast_alloc_error()
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use file_descriptor_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      write(iout,*)' ** ERROR IN MEMORY ALLOCATION'
      write(istdo,*)' ** ERROR IN MEMORY ALLOCATION'
      call arret(2)
   end

   subroutine pblast_deallocate(pblast)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type(pblast_),intent(inout) :: pblast
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------
!   p r e - c o n d i t i o n
!-----------------------------------------------
      if(pblast%nloadp_b<=0)return
!-----------------------------------------------
!   s o u r c e   c o d e
!-----------------------------------------------
      do ii=1,pblast%nloadp_b
         if(allocated( pblast%pblast_tab(ii)%cos_theta ))  deallocate ( pblast%pblast_tab(ii)%cos_theta )
         if(allocated( pblast%pblast_tab(ii)%p_inci ))     deallocate ( pblast%pblast_tab(ii)%p_inci )
         if(allocated( pblast%pblast_tab(ii)%p_refl ))     deallocate ( pblast%pblast_tab(ii)%p_refl )
         if(allocated( pblast%pblast_tab(ii)%ta ))         deallocate ( pblast%pblast_tab(ii)%ta )
         if(allocated( pblast%pblast_tab(ii)%t0 ))         deallocate ( pblast%pblast_tab(ii)%t0 )
         if(allocated( pblast%pblast_tab(ii)%decay_inci )) deallocate ( pblast%pblast_tab(ii)%decay_inci )
         if(allocated( pblast%pblast_tab(ii)%decay_refl )) deallocate ( pblast%pblast_tab(ii)%decay_refl )
         if(allocated( pblast%pblast_tab(ii)%decay_refl )) deallocate ( pblast%pblast_tab(ii)%decay_refl )
         if(allocated( pblast%pblast_tab(ii)%fx ))         deallocate ( pblast%pblast_tab(ii)%fx )
         if(allocated( pblast%pblast_tab(ii)%fy ))         deallocate ( pblast%pblast_tab(ii)%fy )
         if(allocated( pblast%pblast_tab(ii)%fz ))         deallocate ( pblast%pblast_tab(ii)%fz )
         if(allocated( pblast%pblast_tab(ii)%n ))          deallocate ( pblast%pblast_tab(ii)%n )
         if(allocated( pblast%pblast_tab(ii)%npt ))        deallocate ( pblast%pblast_tab(ii)%npt )
      enddo

      if(allocated(pblast%pblast_tab))deallocate(pblast%pblast_tab)
!-----------------------------------------------
      return
   end


   subroutine pblast_write_starter(pblast,glob_therm,proc,cep,scep,&
   &                               numelc,numeltg,numels,          &
   &                               numelq,numelt,numelp,           &
   &                               numelr,numelx,nconld)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
      use glob_therm_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type (pblast_)      ,intent(in) :: pblast
      type (glob_therm_)  ,intent(in) :: glob_therm
      integer,intent(in)::scep        !< size of cep
      integer,intent(in)::numelc      !< number of shell elements
      integer,intent(in)::numeltg     !< number of triangle elements
      integer,intent(in)::numels      !< number of solid elements
      integer,intent(in)::numelq      !< number of quad elements
      integer,intent(in)::numelt      !< number of truss elements
      integer,intent(in)::numelp      !< number of beam elements
      integer,intent(in)::numelr      !< number of spring elements
      integer,intent(in)::numelx      !< number of multispring elements
      integer,intent(in)::nconld      !< number of condition loads
      integer,intent(in)::proc,cep(scep)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: siz,i,nseg_loc,iad,off,jj
      integer, dimension(:),allocatable :: nsegl
      my_real, dimension(:),allocatable :: rtmp
      my_real :: tmp(1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------
!   p r e - c o n d i t i o n
!-----------------------------------------------
      if(pblast%nloadp_b <= 0)return
!-----------------------------------------------
!   s o u r c e   c o d e
!-----------------------------------------------
      tmp(1) = pblast%pblast_dt%ta_inf
      call write_db(tmp,1)
      !--------------------------------------
      off = numelc+numeltg+numels+numelq+numelt+numelp+numelr+numelx+nconld&
      &+ glob_therm%numconv + glob_therm%numradia + glob_therm%nfxflux
      allocate(nsegl(pblast%nloadp_b))
      !--------------------------------------
      !     writing local size (number of local segments)
      !--------------------------------------
      do i=1,pblast%nloadp_b
         nseg_loc=0
         do jj=1,pblast%pblast_tab(i)%siz
            if(cep(jj+off)==proc)then  !< check if segment is on local domain
               nseg_loc=nseg_loc+1
            endif
         enddo
         nsegl(i)=nseg_loc
         call write_i_c(nseg_loc,1)
         off = off + pblast%pblast_tab(i)%siz
      enddo

      !--------------------------------------
      !     writing minimum time step
      !--------------------------------------
      do i=1,pblast%nloadp_b
         tmp(1) = pblast%pblast_tab(i)%dtmin
         call write_db(tmp,1)
      enddo

      !--------------------------------------
      !     writing real buffer (for local segments only)
      !--------------------------------------
      off = numelc+numeltg+numels+numelq+numelt+numelp+numelr+numelx+nconld&
      &+ glob_therm%numconv + glob_therm%numradia + glob_therm%nfxflux
      do i=1,pblast%nloadp_b
         siz = nsegl(i)
         allocate(rtmp(8*nsegl(i)))
         iad=0
         do jj=1,pblast%pblast_tab(i)%siz
            if(cep(jj+off)==proc)then !< write buffer if segment is on local domain
               rtmp(iad+1) = pblast%pblast_tab(i)%cos_theta(jj)
               rtmp(iad+2) = pblast%pblast_tab(i)%p_inci(jj)
               rtmp(iad+3) = pblast%pblast_tab(i)%p_refl(jj)
               rtmp(iad+4) = pblast%pblast_tab(i)%ta(jj)
               rtmp(iad+5) = pblast%pblast_tab(i)%t0(jj)
               rtmp(iad+6) = pblast%pblast_tab(i)%decay_inci(jj)
               rtmp(iad+7) = pblast%pblast_tab(i)%decay_refl(jj)
               rtmp(iad+8) = pblast%pblast_tab(i)%tagmsg(jj)
               iad = iad + 8
            endif
         enddo
         call write_db(rtmp,8*nsegl(i))
         deallocate(rtmp)
         off = off + pblast%pblast_tab(i)%siz
      enddo
      deallocate(nsegl)
!-----------------------------------------------
      return
   end


   subroutine pblast_write_engine(pblast)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
type (pblast_) :: pblast
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: siz,i,iad,jj
      my_real, dimension(:),allocatable :: rtmp
      my_real :: tmp(1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------
!   p r e - c o n d i t i o n
!-----------------------------------------------
      if(pblast%nloadp_b<=0)return
!-----------------------------------------------
!   s o u r c e   c o d e
!-----------------------------------------------
      tmp(1)=pblast%pblast_dt%ta_inf
      call write_db(tmp,1)
      !--------------------------------------
      !     writing local size (number of local segments)
      !--------------------------------------
      do i=1,pblast%nloadp_b
         call write_i_c(pblast%pblast_tab(i)%siz,1)
      enddo
      !--------------------------------------
      !     writing minimum time step
      !--------------------------------------
      do i=1,pblast%nloadp_b
         tmp(1) = pblast%pblast_tab(i)%dtmin
         call write_db(tmp,1)
      enddo
      !--------------------------------------
      !     writing real buffer (for local segments only)
      !--------------------------------------
      do i=1,pblast%nloadp_b
         siz = pblast%pblast_tab(i)%siz
         iad=0
         allocate(rtmp(8*siz))
         do jj=1,pblast%pblast_tab(i)%siz
            rtmp(iad+1) = pblast%pblast_tab(i)%cos_theta(jj)
            rtmp(iad+2) = pblast%pblast_tab(i)%p_inci(jj)
            rtmp(iad+3) = pblast%pblast_tab(i)%p_refl(jj)
            rtmp(iad+4) = pblast%pblast_tab(i)%ta(jj)
            rtmp(iad+5) = pblast%pblast_tab(i)%t0(jj)
            rtmp(iad+6) = pblast%pblast_tab(i)%decay_inci(jj)
            rtmp(iad+7) = pblast%pblast_tab(i)%decay_refl(jj)
            rtmp(iad+8) = pblast%pblast_tab(i)%tagmsg(jj)
            iad = iad + 8
         enddo
         call write_db(rtmp,8*siz)
         deallocate(rtmp)
      enddo
!-----------------------------------------------
      return
   end

   subroutine pblast_init_tables(pblast_data)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type(pblast_data_),intent(inout) :: pblast_data
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      !curve limit shdc
      !ft/lb^1/3
      pblast_data%shdc(1,:)=(/2.,2. ,2. ,4. ,4. ,6. ,6. ,7. ,9. ,11. /)
      pblast_data%shdc(2,:)=(/9.,11.,14.,15.,18.,18.,18.,18.,18.,18. /)
      !cm/g^1/3
      pblast_data%shdc(1,:) = pblast_data%shdc(1,:) * 3.966977216838d00
      pblast_data%shdc(2,:) = pblast_data%shdc(2,:) * 3.966977216838d00
      !delta_sdhc
      pblast_data%dshdc = (71.5d0 - 7.9d0) / 255.d0

      !angle - upper bound (domain of definition)
      pblast_data%angle_pra =(/85.00, 85.00, 85.00, 85.00, 85.00, 83.10, 81.90, 80.70, 79.50, 78.30 /)
      pblast_data%angle_sri =(/85.00, 85.00, 85.00, 85.00, 85.00, 85.00, 85.00, 83.95, 82.90, 79.85 /)

      pblast_data%delta_angle = 85.d00/255.d00

      !curve value (legend multiple plot)
      ! 1.00 ft/lb**1/3 =  3,966977216838196139019  cm/g**1/3
      !
      pblast_data%curve_val_2_9  = (/0.3, 0.6, 0.8, 1.9, 3.0, 5.3, 7.2, 8.9, 11.9, 14.3/) ! ft/lb**1/3
      ! keeping unit ft/lb**1/3
      !
      pblast_data%curve_val_2_10  = (/0.3, 0.5, 0.8, 1.9, 3.0, 5.3, 7.2, 8.9, 11.9, 14.3/) ! ft/lb**1/3
      ! keeping unit ft/lb**1/3
      !
      pblast_data%curve_val_2_13 = (/1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0, 6.0, 7.0 /)  ! ft/lb**1/3
      ! keeping unit ft/lb**1/3

! figure 2_7 scaled radius r/w**(1/3): g,cm,mus,mbar
      pblast_data%rw3 = (/&
      &5.0000000000e-01, 5.1328038846e-01, 5.2691351436e-01, 5.4090874667e-01, 5.5527570323e-01,&
      &5.7002425731e-01, 5.8516454445e-01, 6.0070696938e-01, 6.1666221319e-01, 6.3304124067e-01,&
      &6.4985530785e-01, 6.6711596971e-01, 6.8483508817e-01, 7.0302484017e-01, 7.2169772613e-01,&
      &7.4086657843e-01, 7.6054457035e-01, 7.8074522503e-01, 8.0148242478e-01, 8.2277042068e-01,&
      &8.4462384228e-01, 8.6705770774e-01, 8.9008743409e-01, 9.1372884787e-01, 9.3799819597e-01,&
      &9.6291215681e-01, 9.8848785180e-01, 1.0147428571e+00, 1.0416952158e+00, 1.0693634500e+00,&
      &1.0977665741e+00, 1.1269241072e+00, 1.1568560870e+00, 1.1875830835e+00, 1.2191262128e+00,&
      &1.2515071522e+00, 1.2847481545e+00, 1.3188720636e+00, 1.3539023303e+00, 1.3898630281e+00,&
      &1.4267788699e+00, 1.4646752252e+00, 1.5035781371e+00, 1.5435143406e+00, 1.5845112807e+00,&
      &1.6265971313e+00, 1.6698008149e+00, 1.7141520218e+00, 1.7596812313e+00, 1.8064197319e+00,&
      &1.8543996435e+00, 1.9036539387e+00, 1.9542164663e+00, 2.0061219739e+00, 2.0594061322e+00,&
      &2.1141055590e+00, 2.1702578452e+00, 2.2279015797e+00, 2.2870763766e+00, 2.3478229020e+00,&
      &2.4101829024e+00, 2.4741992328e+00, 2.5399158867e+00, 2.6073780259e+00, 2.6766320120e+00,&
      &2.7477254378e+00, 2.8207071602e+00, 2.8956273339e+00, 2.9725374455e+00, 3.0514903495e+00,&
      &3.1325403040e+00, 3.2157430082e+00, 3.3011556409e+00, 3.3888368994e+00, 3.4788470403e+00,&
      &3.5712479205e+00, 3.6661030399e+00, 3.7634775849e+00, 3.8634384735e+00, 3.9660544010e+00,&
      &4.0713958872e+00, 4.1795353251e+00, 4.2905470305e+00, 4.4045072931e+00, 4.5214944287e+00,&
      &4.6415888336e+00, 4.7648730392e+00, 4.8914317691e+00, 5.0213519971e+00, 5.1547230074e+00,&
      &5.2916364553e+00, 5.4321864307e+00, 5.5764695227e+00, 5.7245848857e+00, 5.8766343078e+00,&
      &6.0327222808e+00, 6.1929560715e+00, 6.3574457962e+00, 6.5263044958e+00, 6.6996482137e+00,&
      &6.8775960753e+00, 7.0602703704e+00, 7.2477966368e+00, 7.4403037464e+00, 7.6379239945e+00,&
      &7.8407931899e+00, 8.0490507487e+00, 8.2628397901e+00, 8.4823072345e+00, 8.7076039047e+00,&
      &8.9388846296e+00, 9.1763083502e+00, 9.4200382292e+00, 9.6702417633e+00, 9.9270908975e+00,&
      &1.0190762144e+01, 1.0461436704e+01, 1.0739300591e+01, 1.1024544758e+01, 1.1317365232e+01,&
      &1.1617963246e+01, 1.1926545376e+01, 1.2243323687e+01, 1.2568515876e+01, 1.2902345423e+01,&
      &1.3245041741e+01, 1.3596840340e+01, 1.3957982983e+01, 1.4328717856e+01, 1.4709299734e+01,&
      &1.5099990163e+01, 1.5501057633e+01, 1.5912777767e+01, 1.6335433508e+01, 1.6769315313e+01,&
      &1.7214721356e+01, 1.7671957730e+01, 1.8141338657e+01, 1.8623186706e+01, 1.9117833014e+01,&
      &1.9625617512e+01, 2.0146889161e+01, 2.0682006189e+01, 2.1231336342e+01, 2.1795257130e+01,&
      &2.2374156093e+01, 2.2968431062e+01, 2.3578490436e+01, 2.4204753460e+01, 2.4847650517e+01,&
      &2.5507623420e+01, 2.6185125715e+01, 2.6880622998e+01, 2.7594593229e+01, 2.8327527064e+01,&
      &2.9079928191e+01, 2.9852313677e+01, 3.0645214321e+01, 3.1459175023e+01, 3.2294755153e+01,&
      &3.3152528940e+01, 3.4033085866e+01, 3.4937031067e+01, 3.5864985756e+01, 3.6817587642e+01,&
      &3.7795491374e+01, 3.8799368989e+01, 3.9829910374e+01, 4.0887823738e+01, 4.1973836103e+01,&
      &4.3088693801e+01, 4.4233162985e+01, 4.5408030159e+01, 4.6614102719e+01, 4.7852209503e+01,&
      &4.9123201365e+01, 5.0427951758e+01, 5.1767357335e+01, 5.3142338565e+01, 5.4553840365e+01,&
      &5.6002832749e+01, 5.7490311497e+01, 5.9017298836e+01, 6.0584844145e+01, 6.2194024675e+01,&
      &6.3845946291e+01, 6.5541744228e+01, 6.7282583875e+01, 6.9069661576e+01, 7.0904205450e+01,&
      &7.2787476233e+01, 7.4720768153e+01, 7.6705409807e+01, 7.8742765086e+01, 8.0834234103e+01,&
      &8.2981254163e+01, 8.5185300744e+01, 8.7447888514e+01, 8.9770572373e+01, 9.2154948521e+01,&
      &9.4602655551e+01, 9.7115375581e+01, 9.9694835408e+01, 1.0234280769e+02, 1.0506111218e+02,&
      &1.0785161694e+02, 1.1071623968e+02, 1.1365694902e+02, 1.1667576589e+02, 1.1977476488e+02,&
      &1.2295607569e+02, 1.2622188459e+02, 1.2957443591e+02, 1.3301603360e+02, 1.3654904279e+02,&
      &1.4017589146e+02, 1.4389907204e+02, 1.4772114319e+02, 1.5164473152e+02, 1.5567253341e+02,&
      &1.5980731684e+02, 1.6405192334e+02, 1.6840926987e+02, 1.7288235092e+02, 1.7747424048e+02,&
      &1.8218809419e+02, 1.8702715152e+02, 1.9199473797e+02, 1.9709426738e+02, 2.0232924424e+02,&
      &2.0770326617e+02, 2.1322002628e+02, 2.1888331584e+02, 2.2469702676e+02, 2.3066515437e+02,&
      &2.3679180008e+02, 2.4308117425e+02, 2.4953759910e+02, 2.5616551160e+02, 2.6296946661e+02,&
      &2.6995413995e+02, 2.7712433164e+02, 2.8448496919e+02, 2.9204111100e+02, 2.9979794980e+02,&
      &3.0776081627e+02, 3.1593518265e+02, 3.2432666656e+02, 3.3294103480e+02, 3.4178420736e+02,&
      &3.5086226145e+02, 3.6018143570e+02, 3.6974813447e+02, 3.7956893219e+02, 3.8965057792e+02,&
      &4.0000000000e+02 /)

! figure 2_7 reflected wave impulse+ iso/w**(1/3): g,cm,mus,mbar
      pblast_data%irefl = (/&
      &0.91477183325000e-01,0.86528105463000e-01,0.81747669692000e-01,0.77205331351000e-01,0.72935216422000e-01,&
      &0.68926595545000e-01,0.65155762569000e-01,0.61605258202000e-01,0.58261234933000e-01,0.55116706434000e-01,&
      &0.52157117945000e-01,0.49369331703000e-01,0.46741478929000e-01,0.44263568374000e-01,0.41930623267000e-01,&
      &0.39731830505000e-01,0.37657444298000e-01,0.35699132791000e-01,0.33851653363000e-01,0.32109313204000e-01,&
      &0.30464733929000e-01,0.28911037617000e-01,0.27442220877000e-01,0.26055591267000e-01,0.24745926467000e-01,&
      &0.23507837896000e-01,0.22336564827000e-01,0.21228358039000e-01,0.20180850481000e-01,0.19190166866000e-01,&
      &0.18252395395000e-01,0.17363870389000e-01,0.16522622026000e-01,0.15726471856000e-01,0.14972351984000e-01,&
      &0.14257484905000e-01,0.13579422203000e-01,0.12937063711000e-01,0.12328269341000e-01,0.11750748790000e-01,&
      &0.11202564039000e-01,0.10682146354000e-01,0.10188623131000e-01,0.97202356938000e-02,0.92753582055000e-02,&
      &0.88525638710000e-02,0.84509032845000e-02,0.80695029142000e-02,0.77070701287000e-02,0.73624185028000e-02,&
      &0.70344549922000e-02,0.67227004552000e-02,0.64262872814000e-02,0.61442475472000e-02,0.58757220402000e-02,&
      &0.56199717285000e-02,0.53766361687000e-02,0.51449962923000e-02,0.49243243434000e-02,0.47139549115000e-02,&
      &0.45134509991000e-02,0.43224879107000e-02,0.41404759841000e-02,0.39668781856000e-02,0.38012056753000e-02,&
      &0.36432102343000e-02,0.34925506699000e-02,0.33487844380000e-02,0.32115002499000e-02,0.30803557268000e-02,&
      &0.29552081086000e-02,0.28357278684000e-02,0.27215840879000e-02,0.26124725669000e-02,0.25081665190000e-02,&
      &0.24085309612000e-02,0.23133031208000e-02,0.22222273036000e-02,0.21350631673000e-02,0.20516950046000e-02,&
      &0.19719770076000e-02,0.18957066819000e-02,0.18226860568000e-02,0.17527390761000e-02,0.16858001545000e-02,&
      &0.16217109600000e-02,0.15603253764000e-02,0.15015001694000e-02,0.14451090088000e-02,0.13911059787000e-02,&
      &0.13393468863000e-02,0.12897155507000e-02,0.12421068214000e-02,0.11964389898000e-02,0.11526715448000e-02,&
      &0.11106781805000e-02,0.10703673778000e-02,0.10316771577000e-02,0.99453576735000e-03,0.95889895413000e-03,&
      &0.92468249398000e-03,0.89179906180000e-03,0.86020541073000e-03,0.82987226763000e-03,0.80073567496000e-03,&
      &0.77272962540000e-03,0.74579616069000e-03,0.71990590329000e-03,0.69502433763000e-03,0.67109939485000e-03,&
      &0.64808398507000e-03,0.62593220794000e-03,0.60462671348000e-03,0.58413377118000e-03,0.56441143937000e-03,&
      &0.54542065033000e-03,0.52713048355000e-03,0.50953045974000e-03,0.49258537809000e-03,0.47626157447000e-03,&
      &0.46053043534000e-03,0.44537292901000e-03,0.43077433104000e-03,0.41670697706000e-03,0.40314587519000e-03,&
      &0.39006680562000e-03,0.37745930199000e-03,0.36530735226000e-03,0.35358743596000e-03,0.34227907268000e-03,&
      &0.33136628075000e-03,0.32084129884000e-03,0.31068766312000e-03,0.30088822396000e-03,0.29142548201000e-03,&
      &0.28228755028000e-03,0.27347061941000e-03,0.26495786558000e-03,0.25673470912000e-03,0.24878922399000e-03,&
      &0.24111428351000e-03,0.23370287648000e-03,0.22654151724000e-03,0.21961847348000e-03,0.21292508147000e-03,&
      &0.20645708441000e-03,0.20020638229000e-03,0.19416211963000e-03,0.18831479925000e-03,0.18265936067000e-03,&
      &0.17719088371000e-03,0.17190210439000e-03,0.16678516845000e-03,0.16183144276000e-03,0.15703806677000e-03,&
      &0.15240117586000e-03,0.14791306588000e-03,0.14356707831000e-03,0.13935802150000e-03,0.13528423179000e-03,&
      &0.13134036713000e-03,0.12752069558000e-03,0.12382002895000e-03,0.12023410966000e-03,0.11676130720000e-03,&
      &0.11339756917000e-03,0.11013764987000e-03,0.10697688806000e-03,0.10391437390000e-03,0.10094556255000e-03,&
      &0.98068358183000e-04,0.95279599934000e-04,0.92573594690000e-04,0.89950121569000e-04,0.87406910992000e-04,&
      &0.84940493105000e-04,0.82547225901000e-04,0.80224621294000e-04,0.77972443231000e-04,0.75788083272000e-04,&
      &0.73668342465000e-04,0.71610254402000e-04,0.69612413228000e-04,0.67674342382000e-04,0.65793533180000e-04,&
      &0.63967278510000e-04,0.62193421725000e-04,0.60471483923000e-04,0.58800087464000e-04,0.57177049718000e-04,&
      &0.55600341813000e-04,0.54068453463000e-04,0.52581047503000e-04,0.51136603454000e-04,0.49733229944000e-04,&
      &0.48369218952000e-04,0.47043767977000e-04,0.45756505151000e-04,0.44505724980000e-04,0.43289943080000e-04,&
      &0.42107933061000e-04,0.40959170307000e-04,0.39842895212000e-04,0.38757845993000e-04,0.37702798580000e-04,&
      &0.36676716010000e-04,0.35679438718000e-04,0.34709920654000e-04,0.33767116420000e-04,0.32850064668000e-04,&
      &0.31958033910000e-04,0.31090838598000e-04,0.30247501537000e-04,0.29427112400000e-04,0.28628832617000e-04,&
      &0.27852311224000e-04,0.27097213515000e-04,0.26362662771000e-04,0.25647830119000e-04,0.24952090005000e-04,&
      &0.24275308771000e-04,0.23616970640000e-04,0.22976350094000e-04,0.22352777764000e-04,0.21745846777000e-04,&
      &0.21155312341000e-04,0.20580726510000e-04,0.20021530889000e-04,0.19477040110000e-04,0.18947044201000e-04,&
      &0.18431310051000e-04,0.17929389646000e-04,0.17440728285000e-04,0.16964812211000e-04,0.16501751323000e-04,&
      &0.16051022536000e-04,0.15612215032000e-04,0.15184994100000e-04,0.14768927008000e-04,0.14363929653000e-04,&
      &0.13969800213000e-04,0.13586039280000e-04,0.13212178561000e-04,0.12848270901000e-04,0.12494058036000e-04,&
      &0.12149198944000e-04,0.11813404195000e-04,0.11486314565000e-04,0.11167834915000e-04,0.10857925150000e-04,&
      &0.10556151611000e-04,0.10262176638000e-04,0.99759564290000e-05,0.96980220473000e-05,0.94237557535000e-05,&
      &0.91680439661000e-05      /)

! figure 2_7 incident wave impulse+ iso/w**(1/3): g,cm,mus,mbar
      pblast_data%iso = (/&
      &0.28466039898000e-02,0.27397298250000e-02,0.26338772965000e-02,0.25288980910000e-02,0.24248936980000e-02,&
      &0.23222763541000e-02,0.22214340331000e-02,0.21226308810000e-02,0.20261452592000e-02,0.19322212643000e-02,&
      &0.18410269480000e-02,0.17527133903000e-02,0.16673922226000e-02,0.15851131499000e-02,0.15059951850000e-02,&
      &0.14300587858000e-02,0.13572839922000e-02,0.12876344916000e-02,0.12211101901000e-02,0.11577054502000e-02,&
      &0.10973307551000e-02,0.10398863562000e-02,0.98528819025000e-03,0.93350919887000e-03,0.88445450058000e-03,&
      &0.83800313524000e-03,0.79403575086000e-03,0.75245707228000e-03,0.71321435536000e-03,0.67619227893000e-03,&
      &0.64126683013000e-03,0.60831822030000e-03,0.57728075599000e-03,0.54807788464000e-03,0.52059908550000e-03,&
      &0.49473861506000e-03,0.47039793139000e-03,0.44753393608000e-03,0.42606143337000e-03,0.40588790285000e-03,&
      &0.38693011966000e-03,0.36911854765000e-03,0.35241580877000e-03,0.33674804567000e-03,0.32204351916000e-03,&
      &0.30823873948000e-03,0.29529252747000e-03,0.28316512433000e-03,0.27179967600000e-03,0.26114477571000e-03,&
      &0.25115311933000e-03,0.24180135261000e-03,0.23305230307000e-03,0.22486442639000e-03,0.21720130452000e-03,&
      &0.21003236967000e-03,0.20334034203000e-03,0.19709646346000e-03,0.19127081195000e-03,0.18583628115000e-03,&
      &0.18077629388000e-03,0.17607716576000e-03,0.17171578832000e-03,0.16767267681000e-03,0.16393044341000e-03,&
      &0.16047923079000e-03,0.15730747273000e-03,0.15440049690000e-03,0.15174415500000e-03,0.14932716084000e-03,&
      &0.14714615502000e-03,0.14519071915000e-03,0.14345191926000e-03,0.14192160984000e-03,0.14059324235000e-03,&
      &0.13946512744000e-03,0.13853130999000e-03,0.13778625842000e-03,0.13722537079000e-03,0.13684781214000e-03,&
      &0.13665189262000e-03,0.13663547070000e-03,0.13679660198000e-03,0.13713369067000e-03,0.13764893428000e-03,&
      &0.13834229578000e-03,0.13921420467000e-03,0.14026592491000e-03,0.14149969858000e-03,0.14291915497000e-03,&
      &0.14452661212000e-03,0.14632459502000e-03,0.14831666810000e-03,0.15050869521000e-03,0.15290565349000e-03,&
      &0.15551274351000e-03,0.15833502224000e-03,0.16137777987000e-03,0.16464909344000e-03,0.16815675742000e-03,&
      &0.17190725494000e-03,0.17590811881000e-03,0.18016865205000e-03,0.18471311408000e-03,0.18952764202000e-03,&
      &0.19427088842000e-03,0.19540138575000e-03,0.19428473966000e-03,0.19257673146000e-03,0.19049547803000e-03,&
      &0.18802517403000e-03,0.18524655829000e-03,0.18220205068000e-03,0.17894154175000e-03,0.17551666935000e-03,&
      &0.17197687958000e-03,0.16834409901000e-03,0.16465136299000e-03,0.16092916235000e-03,0.15720273970000e-03,&
      &0.15349180711000e-03,0.14981045766000e-03,0.14617288892000e-03,0.14259161220000e-03,0.13907498156000e-03,&
      &0.13562969158000e-03,0.13226184209000e-03,0.12897549631000e-03,0.12577273811000e-03,0.12265436319000e-03,&
      &0.11962033635000e-03,0.11667263158000e-03,0.11380955825000e-03,0.11102926411000e-03,0.10832986376000e-03,&
      &0.10570918286000e-03,0.10316621083000e-03,0.10069819928000e-03,0.98301612783000e-04,0.95972823710000e-04,&
      &0.93711003180000e-04,0.91514411357000e-04,0.89378765474000e-04,0.87300727110000e-04,0.85278349250000e-04,&
      &0.83310301013000e-04,0.81394034974000e-04,0.79526672679000e-04,0.77705645759000e-04,0.75929254172000e-04,&
      &0.74196520318000e-04,0.72505230598000e-04,0.70853345878000e-04,0.69238933275000e-04,0.67661145280000e-04,&
      &0.66119133296000e-04,0.64611134135000e-04,0.63135583752000e-04,0.61691337173000e-04,0.60278272171000e-04,&
      &0.58895347274000e-04,0.57541307995000e-04,0.56215222235000e-04,0.54916530090000e-04,0.53645046853000e-04,&
      &0.52400085098000e-04,0.51180607493000e-04,0.49985895318000e-04,0.48816342381000e-04,0.47671048174000e-04,&
      &0.46549677377000e-04,0.45451895155000e-04,0.44376721648000e-04,0.43324332896000e-04,0.42294636612000e-04,&
      &0.41286982585000e-04,0.40300656744000e-04,0.39335432110000e-04,0.38391537127000e-04,0.37468620617000e-04,&
      &0.36566011264000e-04,0.35683072463000e-04,0.34819747406000e-04,0.33976178890000e-04,0.33151886949000e-04,&
      &0.32346215204000e-04,0.31558651325000e-04,0.30789326978000e-04,0.30037917683000e-04,0.29303942395000e-04,&
      &0.28586935462000e-04,0.27886474673000e-04,0.27202657122000e-04,0.26535082210000e-04,0.25883146235000e-04,&
      &0.25246366538000e-04,0.24624629421000e-04,0.24017856951000e-04,0.23425558741000e-04,0.22847218145000e-04,&
      &0.22282376362000e-04,0.21730989395000e-04,0.21192924243000e-04,0.20667644355000e-04,0.20154676614000e-04,&
      &0.19653762155000e-04,0.19164870338000e-04,0.18687622252000e-04,0.18221649146000e-04,0.17766619383000e-04,&
      &0.17322283932000e-04,0.16888666847000e-04,0.16465333988000e-04,0.16051917951000e-04,0.15648177640000e-04,&
      &0.15254038599000e-04,0.14869356116000e-04,0.14493784878000e-04,0.14127060826000e-04,0.13768965767000e-04,&
      &0.13419433620000e-04,0.13078344922000e-04,0.12745457526000e-04,0.12420471127000e-04,0.12103230081000e-04,&
      &0.11793720741000e-04,0.11491778610000e-04,0.11197179768000e-04,0.10909647141000e-04,0.10629102621000e-04,&
      &0.10355562677000e-04,0.10088813549000e-04,0.98286236160000e-05,0.95748177218000e-05,0.93273726935000e-05,&
      &0.90860968492000e-05,0.88508475841000e-05,0.86214503431000e-05,0.83976852932000e-05,0.81795748738000e-05,&
      &0.79669170309000e-05,0.77594784954000e-05,0.75570544146000e-05,0.73595405651000e-05,0.71668320455000e-05,&
      &0.69787041434000e-05,0.67949249968000e-05,0.66152571905000e-05,0.64396144493000e-05,0.62678111840000e-05,&
      &0.60995819868000e-05,0.59346691447000e-05,0.57728461957000e-05,0.56143300399000e-05,0.54564870079000e-05,&
      &0.53075442983000e-05      /)

! figure 2_7 reflected pressure pr/w**(1/3) : g,cm,mus,mbar
      pblast_data%pr = (/&
      &0.65377435351000e-02,0.63990109044000e-02,0.62628593274000e-02,0.61318373118000e-02,0.60069744773000e-02,&
      &0.58874419106000e-02,0.57720063815000e-02,0.56598285372000e-02,0.55502891498000e-02,0.54430179634000e-02,&
      &0.53374523647000e-02,0.52331692183000e-02,0.51298477959000e-02,0.50272372394000e-02,0.49252487652000e-02,&
      &0.48236758917000e-02,0.47223991104000e-02,0.46213765299000e-02,0.45205687955000e-02,0.44199860124000e-02,&
      &0.43196539355000e-02,0.42195980297000e-02,0.41198604169000e-02,0.40205336327000e-02,0.39216839927000e-02,&
      &0.38233874686000e-02,0.37257315411000e-02,0.36288013903000e-02,0.35326923797000e-02,0.34374962444000e-02,&
      &0.33432899648000e-02,0.32501457844000e-02,0.31581565103000e-02,0.30673824532000e-02,0.29778994713000e-02,&
      &0.28897723702000e-02,0.28030323927000e-02,0.27177518123000e-02,0.26339773340000e-02,0.25517337104000e-02,&
      &0.24710416267000e-02,0.23919269767000e-02,0.23144327330000e-02,0.22385646701000e-02,0.21643252052000e-02,&
      &0.20917127673000e-02,0.20207402597000e-02,0.19514177991000e-02,0.18837263230000e-02,0.18176499083000e-02,&
      &0.17531683153000e-02,0.16902987570000e-02,0.16290207421000e-02,0.15693053114000e-02,0.15111278924000e-02,&
      &0.14544621900000e-02,0.13993155885000e-02,0.13456660178000e-02,0.12934792732000e-02,0.12427132752000e-02,&
      &0.11933523606000e-02,0.11454112094000e-02,0.10988477314000e-02,0.10536222858000e-02,0.10097106014000e-02,&
      &0.96711064351000e-03,0.92580724326000e-03,0.88576804891000e-03,0.84696030596000e-03,0.80935818804000e-03,&
      &0.77296898165000e-03,0.73777102759000e-03,0.70373088322000e-03,0.67081816273000e-03,0.63902115274000e-03,&
      &0.60833778220000e-03,0.57873897418000e-03,0.55019854491000e-03,0.52268982309000e-03,0.49620471686000e-03,&
      &0.47073468027000e-03,0.44625430041000e-03,0.42273194518000e-03,0.40014094677000e-03,0.37848595372000e-03,&
      &0.35773847546000e-03,0.33787370352000e-03,0.31886685537000e-03,0.30069230737000e-03,0.28335098477000e-03,&
      &0.26680918260000e-03,0.25104017155000e-03,0.23601987374000e-03,0.22172784035000e-03,0.20815413235000e-03,&
      &0.19526773040000e-03,0.18304096636000e-03,0.17145013988000e-03,0.16047989300000e-03,0.15011071392000e-03,&
      &0.14031437562000e-03,0.13106161721000e-03,0.12233188864000e-03,0.11411551229000e-03,0.10638605302000e-03,&
      &0.99116778030000e-04,0.92283249760000e-04,0.85869632208000e-04,0.79861182437000e-04,0.74234212836000e-04,&
      &0.68965771369000e-04,0.64034256747000e-04,0.59429417523000e-04,0.55133973089000e-04,0.51127437367000e-04,&
      &0.47390620316000e-04,0.43907577670000e-04,0.40670099942000e-04,0.37661052483000e-04,0.34863853234000e-04,&
      &0.32263766337000e-04,0.29850095438000e-04,0.27614024814000e-04,0.25542037617000e-04,0.23621617815000e-04,&
      &0.21841292403000e-04,0.20194680554000e-04,0.18673096675000e-04,0.17266207208000e-04,0.15964797855000e-04,&
      &0.14761188241000e-04,0.13650890879000e-04,0.12626493104000e-04,0.11680712474000e-04,0.10806965294000e-04,&
      &0.10000383733000e-04,0.92574427702000e-05,0.85725128177000e-05,0.79405296676000e-05,0.73570133955000e-05,&
      &0.68192096478000e-05,0.63239171549000e-05,0.58672467226000e-05,0.54457911219000e-05,0.50566905854000e-05,&
      &0.46982682655000e-05,0.43679472874000e-05,0.40631434947000e-05,0.37815907378000e-05,0.35215911762000e-05,&
      &0.32819233309000e-05,0.30607240063000e-05,0.28563176191000e-05,0.26672139808000e-05,0.24924758086000e-05,&
      &0.23311239612000e-05,0.21818971509000e-05,0.20436986391000e-05,0.19156100711000e-05,0.17971071694000e-05,&
      &0.16873867168000e-05,0.15856489354000e-05,0.14911927316000e-05,0.14034593792000e-05,0.13220973018000e-05,&
      &0.12465433921000e-05,0.11762603751000e-05,0.11107977619000e-05,0.10498897627000e-05,0.99320044794000e-06,&
      &0.94036517373000e-06,0.89106692978000e-06,0.84499590486000e-06,0.80199249321000e-06,0.76183205029000e-06,&
      &0.72425863313000e-06,0.68904777793000e-06,0.65603246710000e-06,0.62511033028000e-06,0.59610856566000e-06,&
      &0.56886115698000e-06,0.54322125418000e-06,0.51909114992000e-06,0.49639379716000e-06,0.47500880947000e-06,&
      &0.45482439006000e-06,0.43574571861000e-06,0.41772446520000e-06,0.40068973001000e-06,0.38455891370000e-06,&
      &0.36926061439000e-06,0.35473776657000e-06,0.34095784266000e-06,0.32786541744000e-06,0.31540620546000e-06,&
      &0.30353198967000e-06,0.29220957237000e-06,0.28141494231000e-06,0.27110708707000e-06,0.26124967189000e-06,&
      &0.25181112004000e-06,0.24277388613000e-06,0.23411634742000e-06,0.22581036156000e-06,0.21783173675000e-06,&
      &0.21016136722000e-06,0.20278978158000e-06,0.19569836461000e-06,0.18886795929000e-06,0.18228305244000e-06,&
      &0.17593339560000e-06,0.16981122948000e-06,0.16390297747000e-06,0.15819716050000e-06,0.15268330115000e-06,&
      &0.14735612014000e-06,0.14220926598000e-06,0.13723284571000e-06,0.13241936419000e-06,0.12776295870000e-06,&
      &0.12326110840000e-06,0.11890767766000e-06,0.11469663363000e-06,0.11062284593000e-06,0.10668238217000e-06,&
      &0.10287408158000e-06,0.99193354174000e-07,0.95635333161000e-07,0.92195812154000e-07,0.88873697645000e-07,&
      &0.85666831387000e-07,0.82571337841000e-07,0.79583710229000e-07,0.76700516106000e-07,0.73921417036000e-07,&
      &0.71243389691000e-07,0.68663156655000e-07,0.66177626830000e-07,0.63783964410000e-07,0.61481772454000e-07,&
      &0.59267914610000e-07,0.57138867458000e-07,0.55091405014000e-07,0.53123891331000e-07,0.51234686396000e-07,&
      &0.49420531748000e-07,0.47678154560000e-07,0.46004252276000e-07,0.44397636561000e-07,0.42855822174000e-07,&
      &0.41375294118000e-07,0.39952762924000e-07,0.38585465842000e-07,0.37276312963000e-07,0.35991594474000e-07,&
      &0.34838519952000e-07      /)

! figure 2_7 incident pressure pso/w**(1/3) : g,cm,mus,mbar
      pblast_data%pso = (/&
      &0.50457104491000e-03,0.49812560374000e-03,0.49148671257000e-03,0.48465143737000e-03,0.47761065595000e-03,&
      &0.47039912890000e-03,0.46303445186000e-03,0.45555007580000e-03,0.44796704479000e-03,0.44030410791000e-03,&
      &0.43258099777000e-03,0.42481696902000e-03,0.41702907706000e-03,0.40922899202000e-03,0.40143238986000e-03,&
      &0.39365130028000e-03,0.38589545130000e-03,0.37817520316000e-03,0.37049844301000e-03,0.36287292151000e-03,&
      &0.35530559037000e-03,0.34780192381000e-03,0.34036618554000e-03,0.33300467768000e-03,0.32572053712000e-03,&
      &0.31851555521000e-03,0.31139228850000e-03,0.30435352566000e-03,0.29740192245000e-03,0.29053895374000e-03,&
      &0.28376432476000e-03,0.27707741296000e-03,0.27048104883000e-03,0.26397594975000e-03,0.25756107493000e-03,&
      &0.25123581593000e-03,0.24499964019000e-03,0.23885489980000e-03,0.23279968968000e-03,0.22683369374000e-03,&
      &0.22095613950000e-03,0.21516482554000e-03,0.20946331425000e-03,0.20385003914000e-03,0.19832261263000e-03,&
      &0.19288063237000e-03,0.18752564538000e-03,0.18225674452000e-03,0.17707385712000e-03,0.17197608065000e-03,&
      &0.16696132626000e-03,0.16203258654000e-03,0.15718902219000e-03,0.15242982138000e-03,0.14775461128000e-03,&
      &0.14316269941000e-03,0.13865608744000e-03,0.13423432802000e-03,0.12989669841000e-03,0.12564266116000e-03,&
      &0.12147274133000e-03,0.11738859133000e-03,0.11338862673000e-03,0.10947297120000e-03,0.10564192251000e-03,&
      &0.10189498932000e-03,0.98233709853000e-04,0.94657417237000e-04,0.91164913832000e-04,0.87756191965000e-04,&
      &0.84432715892000e-04,0.81194063785000e-04,0.78039139297000e-04,0.74967207008000e-04,0.71978550111000e-04,&
      &0.69073553125000e-04,0.66251113418000e-04,0.63510396449000e-04,0.60850156598000e-04,0.58270486102000e-04,&
      &0.55771581842000e-04,0.53352291842000e-04,0.51010750535000e-04,0.48745387622000e-04,0.46557202751000e-04,&
      &0.44444387400000e-04,0.42405662968000e-04,0.40439570891000e-04,0.38544251883000e-04,0.36720419783000e-04,&
      &0.34965614406000e-04,0.33278038213000e-04,0.31656194111000e-04,0.30098690202000e-04,0.28605097008000e-04,&
      &0.27173098788000e-04,0.25800742951000e-04,0.24486386216000e-04,0.23228930073000e-04,0.22027116371000e-04,&
      &0.20878843607000e-04,0.19781817072000e-04,0.18734543445000e-04,0.17736464926000e-04,0.16785671578000e-04,&
      &0.15879897260000e-04,0.15017155163000e-04,0.14196593519000e-04,0.13416940486000e-04,0.12676224971000e-04,&
      &0.11972703960000e-04,0.11304609833000e-04,0.10671159240000e-04,0.10070889959000e-04,0.95021561164000e-05,&
      &0.89633360657000e-05,0.84529682771000e-05,0.79704961444000e-05,0.75144019548000e-05,0.70831631200000e-05,&
      &0.66754349453000e-05,0.62903037053000e-05,0.59269858684000e-05,0.55842060575000e-05,0.52607595064000e-05,&
      &0.49554851042000e-05,0.46678115073000e-05,0.43969013764000e-05,0.41416770546000e-05,0.39011633021000e-05,&
      &0.36745438697000e-05,0.34613673129000e-05,0.32608459988000e-05,0.30721512383000e-05,0.28944980800000e-05,&
      &0.27273389254000e-05,0.25702958494000e-05,0.24226794168000e-05,0.22838495467000e-05,0.21532273813000e-05,&
      &0.20304847822000e-05,0.19152228662000e-05,0.18069007514000e-05,0.17050422426000e-05,0.16092514877000e-05,&
      &0.15193068052000e-05,0.14348320251000e-05,0.13554370206000e-05,0.12807677664000e-05,0.12105583384000e-05,&
      &0.11446364049000e-05,0.10826980801000e-05,0.10244519988000e-05,0.96963983210000e-06,0.91811877663000e-06,&
      &0.86970602080000e-06,0.82417363879000e-06,0.78132272636000e-06,0.74097191385000e-06,0.70303075623000e-06,&
      &0.66733659643000e-06,0.63372699660000e-06,0.60205860882000e-06,0.57221234826000e-06,0.54411769185000e-06,&
      &0.51765212379000e-06,0.49269396563000e-06,0.46913763912000e-06,0.44692458060000e-06,0.42597697625000e-06,&
      &0.40620703520000e-06,0.38753540975000e-06,0.36988158657000e-06,0.35320841310000e-06,0.33745844270000e-06,&
      &0.32256401338000e-06,0.30846257740000e-06,0.29510664589000e-06,0.28247219019000e-06,0.27050974738000e-06,&
      &0.25916994460000e-06,0.24840879029000e-06,0.23819760759000e-06,0.22851515729000e-06,0.21932389777000e-06,&
      &0.21058764411000e-06,0.20227580577000e-06,0.19437438366000e-06,0.18685995870000e-06,0.17970512824000e-06,&
      &0.17288548234000e-06,0.16638082938000e-06,0.16018165714000e-06,0.15426793753000e-06,0.14861956188000e-06,&
      &0.14321909823000e-06,0.13805467748000e-06,0.13311747872000e-06,0.12839233531000e-06,0.12386531316000e-06,&
      &0.11952339208000e-06,0.11535956237000e-06,0.11136583151000e-06,0.10753105060000e-06,0.10384468514000e-06,&
      &0.10029830856000e-06,0.96888325226000e-07,0.93606819583000e-07,0.90445407282000e-07,0.87397022639000e-07,&
      &0.84456898173000e-07,0.81622092954000e-07,0.78885672228000e-07,0.76241669090000e-07,0.73685657985000e-07,&
      &0.71215609179000e-07,0.68827277657000e-07,0.66516250661000e-07,0.64278979765000e-07,0.62111778332000e-07,&
      &0.60013786042000e-07,0.57981886855000e-07,0.56012845418000e-07,0.54103951549000e-07,0.52253194361000e-07,&
      &0.50459850895000e-07,0.48721752181000e-07,0.47036640908000e-07,0.45402397684000e-07,0.43818297429000e-07,&
      &0.42283597696000e-07,0.40796617858000e-07,0.39355797742000e-07,0.37959791581000e-07,0.36608560155000e-07,&
      &0.35301062538000e-07,0.34036159612000e-07,0.32812768355000e-07,0.31629978248000e-07,0.30487923583000e-07,&
      &0.29385716371000e-07,0.28322362795000e-07,0.27296919175000e-07,0.26309099090000e-07,0.25358811516000e-07,&
      &0.24445211578000e-07,0.23567465183000e-07,0.22724820351000e-07,0.21917375323000e-07,0.21144634254000e-07,&
      &0.20405786438000e-07,0.19700173870000e-07,0.19027321245000e-07,0.18389463417000e-07,0.17772017546000e-07,&
      &0.17222643810000e-07      /)

! figure 2_7 characteristic time t0/w**(1/3) : g,cm,mus,mbar
      pblast_data%t0 = (/&
      &0.26915250134000e+02,0.26937738651000e+02,0.26955155348000e+02,0.26967184234000e+02,0.26973501300000e+02,&
      &0.26973775158000e+02,0.26967667809000e+02,0.26954835552000e+02,0.26934930064000e+02,0.26907599651000e+02,&
      &0.26872490706000e+02,0.26829249386000e+02,0.26777523537000e+02,0.26716964895000e+02,0.26647231576000e+02,&
      &0.26567990916000e+02,0.26478922667000e+02,0.26379722602000e+02,0.26270106563000e+02,0.26149815009000e+02,&
      &0.26018618091000e+02,0.25876321336000e+02,0.25722771968000e+02,0.25557865963000e+02,0.25381555872000e+02,&
      &0.25193859522000e+02,0.24994869651000e+02,0.24784764582000e+02,0.24563820036000e+02,0.24332422176000e+02,&
      &0.24091082017000e+02,0.23840451314000e+02,0.23581340083000e+02,0.23314735882000e+02,0.23041825043000e+02,&
      &0.22764016014000e+02,0.22482965008000e+02,0.22200604184000e+02,0.21919172566000e+02,0.21641249973000e+02,&
      &0.21369794216000e+02,0.21108181861000e+02,0.20860252884000e+02,0.20628743999000e+02,0.20413157286000e+02,&
      &0.20212309291000e+02,0.20024336197000e+02,0.19847290276000e+02,0.19679454537000e+02,0.19519176754000e+02,&
      &0.19365282465000e+02,0.19216948203000e+02,0.19073633472000e+02,0.18935591050000e+02,0.18803255529000e+02,&
      &0.18677294868000e+02,0.18559078435000e+02,0.18450073080000e+02,0.18351891285000e+02,0.18266576472000e+02,&
      &0.18196306879000e+02,0.18143033567000e+02,0.18109041325000e+02,0.18096647534000e+02,0.18107942496000e+02,&
      &0.18145146556000e+02,0.18210292079000e+02,0.18305420579000e+02,0.18432743037000e+02,0.18594228081000e+02,&
      &0.18791764704000e+02,0.19027348994000e+02,0.19303073835000e+02,0.19621275870000e+02,0.19984016372000e+02,&
      &0.20393840703000e+02,0.20853683831000e+02,0.21366553379000e+02,0.21936129958000e+02,0.22566787309000e+02,&
      &0.23263373819000e+02,0.24031745471000e+02,0.24879061500000e+02,0.25814037437000e+02,0.26846887246000e+02,&
      &0.27989119689000e+02,0.29255581850000e+02,0.30662970218000e+02,0.32232677333000e+02,0.33988388116000e+02,&
      &0.35958648524000e+02,0.38178254388000e+02,0.40687197015000e+02,0.43531045698000e+02,0.46763227291000e+02,&
      &0.50446419378000e+02,0.54649328633000e+02,0.59446576109000e+02,0.64920133698000e+02,0.71150228181000e+02,&
      &0.78210090804000e+02,0.86155760192000e+02,0.95010901878000e+02,0.10473432123000e+03,0.11525163025000e+03,&
      &0.12612796434000e+03,0.13710148132000e+03,0.14742186245000e+03,0.15610990887000e+03,0.16178976538000e+03,&
      &0.16622218744000e+03,0.17074693027000e+03,0.17408278815000e+03,0.17681780010000e+03,0.17897974667000e+03,&
      &0.18060817422000e+03,0.18170296329000e+03,0.18225667274000e+03,0.18227104392000e+03,0.18177527409000e+03,&
      &0.18082758241000e+03,0.17950891813000e+03,0.17791873770000e+03,0.17619273840000e+03,0.17440901935000e+03,&
      &0.17268800579000e+03,0.17112868375000e+03,0.16979722522000e+03,0.16874623601000e+03,0.16800792928000e+03,&
      &0.16759899122000e+03,0.16751816046000e+03,0.16775750913000e+03,0.16830108402000e+03,0.16913828254000e+03,&
      &0.17026827385000e+03,0.17170703641000e+03,0.17349544767000e+03,0.17569861984000e+03,0.17840776472000e+03,&
      &0.18173744266000e+03,0.18580433968000e+03,0.19070357957000e+03,0.19645689712000e+03,0.20299226909000e+03,&
      &0.20987480124000e+03,0.21668068560000e+03,0.22304270781000e+03,0.22902190332000e+03,0.23472274795000e+03,&
      &0.24013657512000e+03,0.24528533487000e+03,0.25018623192000e+03,0.25485898942000e+03,0.25932350634000e+03,&
      &0.26359678465000e+03,0.26769934871000e+03,0.27164956051000e+03,0.27546157460000e+03,0.27915340942000e+03,&
      &0.28273997311000e+03,0.28623641743000e+03,0.28965464294000e+03,0.29300738944000e+03,0.29630578731000e+03,&
      &0.29956080364000e+03,0.30278072054000e+03,0.30597201423000e+03,0.30914388703000e+03,0.31230266921000e+03,&
      &0.31545143065000e+03,0.31859620454000e+03,0.32174111264000e+03,0.32488794860000e+03,0.32804139477000e+03,&
      &0.33120142924000e+03,0.33437183903000e+03,0.33755151380000e+03,0.34074060323000e+03,0.34394219485000e+03,&
      &0.34715483089000e+03,0.35037735637000e+03,0.35361099361000e+03,0.35685424622000e+03,0.36010678425000e+03,&
      &0.36336862619000e+03,0.36663765860000e+03,0.36991401035000e+03,0.37319692761000e+03,0.37648468061000e+03,&
      &0.37977705602000e+03,0.38307381660000e+03,0.38637404100000e+03,0.38967690190000e+03,0.39298291777000e+03,&
      &0.39629246912000e+03,0.39960529383000e+03,0.40292079236000e+03,0.40623916582000e+03,0.40956234064000e+03,&
      &0.41288974290000e+03,0.41622320469000e+03,0.41956337910000e+03,0.42291058065000e+03,0.42626472923000e+03,&
      &0.42963005446000e+03,0.43300650571000e+03,0.43639519497000e+03,0.43979641288000e+03,0.44321294826000e+03,&
      &0.44664720926000e+03,0.45009826593000e+03,0.45356808427000e+03,0.45705864429000e+03,0.46057047693000e+03,&
      &0.46410412238000e+03,0.46766057348000e+03,0.47124182940000e+03,0.47484551617000e+03,0.47847376746000e+03,&
      &0.48212635132000e+03,0.48580133643000e+03,0.48949986903000e+03,0.49322196284000e+03,0.49696459838000e+03,&
      &0.50072655207000e+03,0.50450721105000e+03,0.50830391482000e+03,0.51211498032000e+03,0.51593821534000e+03,&
      &0.51976999183000e+03,0.52360870801000e+03,0.52745114026000e+03,0.53129399577000e+03,0.53513379008000e+03,&
      &0.53896962928000e+03,0.54279811220000e+03,0.54661571662000e+03,0.55042143113000e+03,0.55421453694000e+03,&
      &0.55799252222000e+03,0.56175574713000e+03,0.56550666293000e+03,0.56924422361000e+03,0.57297476594000e+03,&
      &0.57670231948000e+03,0.58043297630000e+03,0.58417388026000e+03,0.58793828018000e+03,0.59174009996000e+03,&
      &0.59559288524000e+03,0.59951690526000e+03,0.60354147138000e+03,0.60768403414000e+03,0.61199123165000e+03,&
      &0.61640408737000e+03      /)

! figure 2_7 arrival time ta/w**(1/3) : g,cm,mus,mbar
      pblast_data%ta = (/&
      &0.73009651097000e+00,0.74717753694000e+00,0.76511807958000e+00,0.78400414463000e+00,0.80389279569000e+00,&
      &0.82479524802000e+00,0.84672839561000e+00,0.86971684946000e+00,0.89377634767000e+00,0.91892962351000e+00,&
      &0.94520760261000e+00,0.97263023267000e+00,0.10012214637000e+01,0.10310214226000e+01,0.10620494769000e+01,&
      &0.10943436606000e+01,0.11279387694000e+01,0.11628640886000e+01,0.11991676947000e+01,0.12368873773000e+01,&
      &0.12760630715000e+01,0.13167361672000e+01,0.13589538184000e+01,0.14027780850000e+01,0.14482543355000e+01,&
      &0.14954326793000e+01,0.15443706330000e+01,0.15951263482000e+01,0.16477738891000e+01,0.17023739396000e+01,&
      &0.17590051456000e+01,0.18177484991000e+01,0.18786613194000e+01,0.19418438840000e+01,0.20073752359000e+01,&
      &0.20753530047000e+01,0.21458937747000e+01,0.22190711201000e+01,0.22949992715000e+01,0.23738074590000e+01,&
      &0.24556234927000e+01,0.25405737041000e+01,0.26287751925000e+01,0.27203797770000e+01,0.28155504077000e+01,&
      &0.29144443245000e+01,0.30172283057000e+01,0.31240789764000e+01,0.32351873339000e+01,0.33507595175000e+01,&
      &0.34710103714000e+01,0.35961408728000e+01,0.37264064570000e+01,0.38620506579000e+01,0.40033262602000e+01,&
      &0.41505327545000e+01,0.43039487743000e+01,0.44638748899000e+01,0.46306441909000e+01,0.48046227212000e+01,&
      &0.49861726894000e+01,0.51756529237000e+01,0.53734725000000e+01,0.55800760690000e+01,0.57959300633000e+01,&
      &0.60215005851000e+01,0.62572739950000e+01,0.65038027905000e+01,0.67616673503000e+01,0.70314580046000e+01,&
      &0.73137952445000e+01,0.76093356951000e+01,0.79187904316000e+01,0.82429249045000e+01,0.85825158626000e+01,&
      &0.89383427577000e+01,0.93113250677000e+01,0.97023884461000e+01,0.10112507708000e+02,0.10542761735000e+02,&
      &0.10994127571000e+02,0.11467719650000e+02,0.11964858578000e+02,0.12486886468000e+02,0.13034968716000e+02,&
      &0.13610686105000e+02,0.14215425511000e+02,0.14850727172000e+02,0.15518552639000e+02,0.16220156221000e+02,&
      &0.16957788793000e+02,0.17733412288000e+02,0.18548771290000e+02,0.19406490297000e+02,0.20308373671000e+02,&
      &0.21257066517000e+02,0.22255176019000e+02,0.23305042602000e+02,0.24409905760000e+02,0.25572409326000e+02,&
      &0.26795668781000e+02,0.28083187680000e+02,0.29438212978000e+02,0.30864206702000e+02,0.32364915055000e+02,&
      &0.33944578617000e+02,0.35607414786000e+02,0.37357283220000e+02,0.39199048061000e+02,0.41137530831000e+02,&
      &0.43177571438000e+02,0.45324565785000e+02,0.47583898723000e+02,0.49961290086000e+02,0.52462757520000e+02,&
      &0.55094643443000e+02,0.57863487154000e+02,0.60775541229000e+02,0.63838336941000e+02,0.67059529121000e+02,&
      &0.70446555596000e+02,0.74007388050000e+02,0.77750429442000e+02,0.81684120722000e+02,0.85817404746000e+02,&
      &0.90159907620000e+02,0.94721001609000e+02,0.99510575064000e+02,0.10453927454000e+03,0.10981776002000e+03,&
      &0.11535677699000e+03,0.12116817880000e+03,0.12726376545000e+03,0.13365530806000e+03,0.14035540575000e+03,&
      &0.14737724472000e+03,0.15473245506000e+03,0.16243651907000e+03,0.17050364207000e+03,0.17894585506000e+03,&
      &0.18777849854000e+03,0.19701809844000e+03,0.20667993939000e+03,0.21677760073000e+03,0.22732660376000e+03,&
      &0.23834582605000e+03,0.24985017510000e+03,0.26185717424000e+03,0.27438399632000e+03,0.28744511374000e+03,&
      &0.30106192408000e+03,0.31525152967000e+03,0.33002991223000e+03,0.34541553061000e+03,0.36142816405000e+03,&
      &0.37808602883000e+03,0.39540821833000e+03,0.41341316463000e+03,0.43211797791000e+03,0.45154293106000e+03,&
      &0.47170889994000e+03,0.49263513839000e+03,0.51433853963000e+03,0.53683771752000e+03,0.56015658423000e+03,&
      &0.58431164843000e+03,0.60932531953000e+03,0.63521601276000e+03,0.66199395240000e+03,0.68969683251000e+03,&
      &0.71833661730000e+03,0.74792461304000e+03,0.77848898663000e+03,0.81005351429000e+03,0.84263364011000e+03,&
      &0.87624816701000e+03,0.91092275354000e+03,0.94667870423000e+03,0.98353278545000e+03,0.10215042446000e+04,&
      &0.10606178601000e+04,0.11009002691000e+04,0.11423770428000e+04,0.11850627500000e+04,0.12289778517000e+04,&
      &0.12741556347000e+04,0.13206231623000e+04,0.13684011257000e+04,0.14175139188000e+04,0.14679939475000e+04,&
      &0.15198766960000e+04,0.15731875160000e+04,0.16279407481000e+04,0.16841894782000e+04,0.17419728747000e+04,&
      &0.18013129740000e+04,0.18622489342000e+04,0.19248299910000e+04,0.19890815163000e+04,0.20550516637000e+04,&
      &0.21228023646000e+04,0.21923622369000e+04,0.22637863858000e+04,0.23371413280000e+04,0.24124735637000e+04,&
      &0.24898259884000e+04,0.25692701891000e+04,0.26508802640000e+04,0.27347192684000e+04,0.28208490815000e+04,&
      &0.29093520035000e+04,0.30003025432000e+04,0.30937806724000e+04,0.31898694792000e+04,0.32886541731000e+04,&
      &0.33902192999000e+04,0.34946683301000e+04,0.36021130153000e+04,0.37126518630000e+04,0.38263768266000e+04,&
      &0.39433810277000e+04,0.40638036111000e+04,0.41877675750000e+04,0.43153687858000e+04,0.44467331977000e+04,&
      &0.45819987512000e+04,0.47212816031000e+04,0.48647030793000e+04,0.50124052207000e+04,0.51645180288000e+04,&
      &0.53211679204000e+04,0.54824810963000e+04,0.56485908675000e+04,0.58196243884000e+04,0.59957012358000e+04,&
      &0.61769496085000e+04,0.63634753921000e+04,0.65553777709000e+04,0.67527563135000e+04,0.69557047590000e+04,&
      &0.71643082033000e+04,0.73785857416000e+04,0.75986175623000e+04,0.78243947379000e+04,0.80559524142000e+04,&
      &0.82932503186000e+04,0.85362075939000e+04,0.87847098407000e+04,0.90386790783000e+04,0.92979545908000e+04,&
      &0.95623075120000e+04,0.98314489319000e+04,0.10105020633000e+05,0.10383194195000e+05,0.10663316073000e+05,&
      &0.10949912549000e+05      /)

! figure 2_7 scaled negative reflected impulse ir- : g,cm,mus,mbar
      pblast_data%irefl_ = (&
      &/4.8971553528e+02, 4.8984536052e+02, 4.8991542491e+02, 4.8989847810e+02, 4.8982527409e+02,&
      &4.8973472552e+02, 4.8967287663e+02, 4.8969376748e+02, 4.8986038778e+02, 4.9015567223e+02,&
      &4.9049540118e+02, 4.9086455461e+02, 4.9124532849e+02, 4.9161666828e+02, 4.9197507913e+02,&
      &4.9235110072e+02, 4.9274071772e+02, 4.9313750077e+02, 4.9353385482e+02, 4.9392087909e+02,&
      &4.9428821259e+02, 4.9462386380e+02, 4.9491621831e+02, 4.9516584991e+02, 4.9537445025e+02,&
      &4.9554382658e+02, 4.9567649134e+02, 4.9577576085e+02, 4.9584586487e+02, 4.9589206800e+02,&
      &4.9592080423e+02, 4.9593982583e+02, 4.9595836792e+02, 4.9598128140e+02, 4.9598515021e+02,&
      &4.9597261059e+02, 4.9595269494e+02, 4.9593612175e+02, 4.9593550097e+02, 4.9596556076e+02,&
      &4.9605391322e+02, 4.9621174282e+02, 4.9641901880e+02, 4.9665152286e+02, 4.9688049046e+02,&
      &4.9707205654e+02, 4.9718664363e+02, 4.9718802112e+02, 4.9713737576e+02, 4.9705542398e+02,&
      &4.9694183240e+02, 4.9679648093e+02, 4.9661949782e+02, 4.9641129881e+02, 4.9617263069e+02,&
      &4.9590506218e+02, 4.9562032256e+02, 4.9532239367e+02, 4.9501025068e+02, 4.9468277204e+02,&
      &4.9433873025e+02, 4.9397206762e+02, 4.9354960562e+02, 4.9308384843e+02, 4.9260150412e+02,&
      &4.9213416125e+02, 4.9171887893e+02, 4.9142813573e+02, 4.9129005593e+02, 4.9125285792e+02,&
      &4.9125424474e+02, 4.9122038399e+02, 4.9106509457e+02, 4.9080828094e+02, 4.9050102958e+02,&
      &4.9013257921e+02, 4.8969074476e+02, 4.8916088508e+02, 4.8849555747e+02, 4.8767166932e+02,&
      &4.8668618866e+02, 4.8553687907e+02, 4.8422258553e+02, 4.8274407114e+02, 4.8108022805e+02,&
      &4.7919943857e+02, 4.7706671873e+02, 4.7471592876e+02, 4.7219325982e+02, 4.6951190435e+02,&
      &4.6670088405e+02, 4.6372832692e+02, 4.6047612938e+02, 4.5678671686e+02, 4.5245941162e+02,&
      &4.4781531450e+02, 4.4334240995e+02, 4.3900976812e+02, 4.3458509127e+02, 4.3012664109e+02,&
      &4.2569975136e+02, 4.2122926255e+02, 4.1664014640e+02, 4.1191339193e+02, 4.0701265443e+02,&
      &4.0195493276e+02, 3.9680638785e+02, 3.9163674053e+02, 3.8642517067e+02, 3.8111531426e+02,&
      &3.7564722594e+02, 3.6992644330e+02, 3.6404993447e+02, 3.5827898028e+02, 3.5266693962e+02,&
      &3.4705528790e+02, 3.4126688980e+02, 3.3526886200e+02, 3.2911319257e+02, 3.2285732744e+02,&
      &3.1664598217e+02, 3.1040379659e+02, 3.0412739265e+02, 2.9784771001e+02, 2.9151786550e+02,&
      &2.8531417111e+02, 2.7948862007e+02, 2.7399461500e+02, 2.6721222724e+02, 2.6051073506e+02,&
      &2.5479461722e+02, 2.4922014866e+02, 2.4361229640e+02, 2.3798350999e+02, 2.3267308372e+02,&
      &2.2742938043e+02, 2.2182895737e+02, 2.1592725395e+02, 2.1030539233e+02, 2.0541234187e+02,&
      &2.0090536302e+02, 1.9634334724e+02, 1.9165983733e+02, 1.8685707910e+02, 1.8204217171e+02,&
      &1.7750123968e+02, 1.7340633852e+02, 1.6945061441e+02, 1.6538330635e+02, 1.6126472675e+02,&
      &1.5713477109e+02, 1.5303598183e+02, 1.4898307927e+02, 1.4508182167e+02, 1.4149566581e+02,&
      &1.3806661567e+02, 1.3464389765e+02, 1.3120502252e+02, 1.2772121108e+02, 1.2420376265e+02,&
      &1.2071569752e+02, 1.1744130943e+02, 1.1452314032e+02, 1.1182347057e+02, 1.0920642937e+02,&
      &1.0653350359e+02, 1.0379691466e+02, 1.0100292852e+02, 9.8234642150e+01, 9.5720653447e+01,&
      &9.3369352953e+01, 9.1076671409e+01, 8.8939695460e+01, 8.6967566560e+01, 8.4963987015e+01,&
      &8.2918433577e+01, 8.0831454321e+01, 7.8740125651e+01, 7.6812725737e+01, 7.5042660111e+01,&
      &7.3365725976e+01, 7.1715826987e+01, 7.0086235305e+01, 6.8473960489e+01, 6.6875098278e+01,&
      &6.5271118503e+01, 6.3652053642e+01, 6.2059886544e+01, 6.0545044303e+01, 5.9148686431e+01,&
      &5.7819074307e+01, 5.6533042688e+01, 5.5273062753e+01, 5.4006469374e+01, 5.2719547677e+01,&
      &5.1457725490e+01, 5.0275774623e+01, 4.9177032854e+01, 4.8103878976e+01, 4.7049672514e+01,&
      &4.6006661672e+01, 4.4964631668e+01, 4.3905677094e+01, 4.2881466217e+01, 4.1944745684e+01,&
      &4.1042302766e+01, 4.0158665493e+01, 3.9275601992e+01, 3.8389281627e+01, 3.7509516304e+01,&
      &3.6642181357e+01, 3.5796735328e+01, 3.4984877248e+01, 3.4201252487e+01, 3.3438884607e+01,&
      &3.2696254450e+01, 3.1970579778e+01, 3.1246434226e+01, 3.0527103749e+01, 2.9825730459e+01,&
      &2.9145355850e+01, 2.8476588892e+01, 2.7832104201e+01, 2.7202958907e+01, 2.6583694720e+01,&
      &2.5962588820e+01, 2.5377208597e+01, 2.4829256797e+01, 2.4278814568e+01, 2.3717335620e+01,&
      &2.3139989928e+01, 2.2546939401e+01, 2.1959572851e+01, 2.1501794512e+01, 2.1093330311e+01,&
      &2.0647770119e+01, 2.0201350447e+01, 1.9766560745e+01, 1.9333222618e+01, 1.8901110248e+01,&
      &1.8471828162e+01, 1.8047184805e+01, 1.7629213285e+01, 1.7220194066e+01, 1.6822525044e+01,&
      &1.6435183244e+01, 1.6053499654e+01, 1.5669677484e+01, 1.5274064913e+01, 1.4896990620e+01,&
      &1.4572867054e+01, 1.4277596203e+01, 1.3991721986e+01, 1.3700471017e+01, 1.3401103936e+01,&
      &1.3099532364e+01/)

! scaled negative incoming impulse is- : g,cm,mus,mbar
      pblast_data%iso_ = (&
      &/4.8939313950e+02, 4.8960281742e+02, 4.8977560515e+02, 4.8974334113e+02, 4.8957242831e+02,&
      &4.8938996121e+02, 4.8934593127e+02, 4.8958232707e+02, 4.9006248153e+02, 4.9071841925e+02,&
      &4.9147411702e+02, 4.9223909653e+02, 4.9290665585e+02, 4.9335191684e+02, 4.9350577837e+02,&
      &4.9361252987e+02, 4.9371217512e+02, 4.9381106449e+02, 4.9391670873e+02, 4.9403791966e+02,&
      &4.9418336792e+02, 4.9434724878e+02, 4.9452394176e+02, 4.9470871205e+02, 4.9489592629e+02,&
      &4.9507894290e+02, 4.9524999120e+02, 4.9540003783e+02, 4.9552206519e+02, 4.9563642006e+02,&
      &4.9574940032e+02, 4.9586215417e+02, 4.9597605330e+02, 4.9609272069e+02, 4.9621816770e+02,&
      &4.9637083137e+02, 4.9653496185e+02, 4.9668630153e+02, 4.9679637524e+02, 4.9683286904e+02,&
      &4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02,&
      &4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02,&
      &4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02, 4.9683286904e+02, 4.9680468215e+02,&
      &4.9670514612e+02, 4.9652107546e+02, 4.9623779589e+02, 4.9585290660e+02, 4.9539901150e+02,&
      &4.9489401419e+02, 4.9435808626e+02, 4.9379379340e+02, 4.9313751533e+02, 4.9244156060e+02,&
      &4.9178531598e+02, 4.9127660803e+02, 4.9113403351e+02, 4.9125202014e+02, 4.9140781190e+02,&
      &4.9140372556e+02, 4.9151241634e+02, 4.9152695202e+02, 4.9104741518e+02, 4.8963405972e+02,&
      &4.8756961642e+02, 4.8515324479e+02, 4.8248434689e+02, 4.7968211027e+02, 4.7688484562e+02,&
      &4.7407933373e+02, 4.7107849882e+02, 4.6719769618e+02, 4.6128135835e+02, 4.5603969460e+02,&
      &4.5153384640e+02, 4.4691102127e+02, 4.4217163892e+02, 4.3732090011e+02, 4.3233054208e+02,&
      &4.2703665759e+02, 4.2085380815e+02, 4.1437575855e+02, 4.0835682885e+02, 4.0235970569e+02,&
      &3.9619648767e+02, 3.8982065121e+02, 3.8323873293e+02, 3.7650764524e+02, 3.6977773214e+02,&
      &3.6315907574e+02, 3.5666388966e+02, 3.5017118835e+02, 3.4361084366e+02, 3.3692559394e+02,&
      &3.3011087090e+02, 3.2318205250e+02, 3.1630935529e+02, 3.0970735936e+02, 3.0319466920e+02,&
      &2.9625760498e+02, 2.8877767115e+02, 2.8192537551e+02, 2.7544024815e+02, 2.6879005432e+02,&
      &2.6167936044e+02, 2.5499628942e+02, 2.4902944755e+02, 2.4295634731e+02, 2.3665389580e+02,&
      &2.3047697816e+02, 2.2508950687e+02, 2.1954015087e+02, 2.1366923373e+02, 2.0788293034e+02,&
      &2.0256637550e+02, 1.9597472380e+02, 1.8934645089e+02, 1.8378181955e+02, 1.7845308191e+02,&
      &1.7314686610e+02, 1.6772714578e+02, 1.6212931665e+02, 1.5687919485e+02, 1.5238863632e+02,&
      &1.4795381735e+02, 1.4336803604e+02, 1.3886490956e+02, 1.3466289538e+02, 1.3066045373e+02,&
      &1.2668153493e+02, 1.2269928297e+02, 1.1894342569e+02, 1.1566025005e+02, 1.1259073561e+02,&
      &1.0959939561e+02, 1.0665402324e+02, 1.0368837176e+02, 1.0068011101e+02, 9.7673458856e+01,&
      &9.4742940296e+01, 9.2043258307e+01, 8.9526052997e+01, 8.7087941250e+01, 8.4712413762e+01,&
      &8.2321170377e+01, 8.0027909719e+01, 7.7881211902e+01, 7.5679288869e+01, 7.3416714087e+01,&
      &7.1092154838e+01, 6.8825647838e+01, 6.6843251543e+01, 6.5122702909e+01, 6.3466997053e+01,&
      &6.1794259542e+01, 6.0101662598e+01, 5.8427301617e+01, 5.6816413772e+01, 5.5295817173e+01,&
      &5.3846390748e+01, 5.2427378096e+01, 5.1014613801e+01, 4.9643426522e+01, 4.8354115695e+01,&
      &4.7136151110e+01, 4.5954752610e+01, 4.4789421588e+01, 4.3665714752e+01, 4.2588008758e+01,&
      &4.1519751481e+01, 4.0461706135e+01, 3.9407882820e+01, 3.8366177589e+01, 3.7385374751e+01,&
      &3.6436113747e+01, 3.5493215219e+01, 3.4542626497e+01, 3.3594656822e+01, 3.2669814088e+01,&
      &3.1786185510e+01, 3.0940319775e+01, 3.0124664146e+01, 2.9328257633e+01, 2.8549857680e+01,&
      &2.7797006147e+01, 2.7074601184e+01, 2.6361275968e+01, 2.5660366104e+01, 2.4983714066e+01,&
      &2.4338521289e+01, 2.3703779672e+01, 2.3076582715e+01, 2.2457268223e+01, 2.1839554314e+01,&
      &2.1220081779e+01, 2.0608487698e+01, 2.0016047844e+01, 1.9453757842e+01, 1.8920362657e+01,&
      &1.8416238535e+01, 1.7942679891e+01, 1.7489056381e+01, 1.7038728066e+01, 1.6598558482e+01,&
      &1.6172323002e+01, 1.5748346042e+01, 1.5334620280e+01, 1.4937960261e+01, 1.4547109944e+01,&
      &1.4165878917e+01, 1.3800826775e+01, 1.3448091713e+01, 1.3101942157e+01, 1.2760346625e+01,&
      &1.2422196184e+01, 1.2091836626e+01, 1.1774140818e+01, 1.1480104039e+01, 1.1196471523e+01,&
      &1.0883313251e+01, 1.0613190268e+01, 1.0379889281e+01, 1.0136472192e+01, 9.8774049835e+00,&
      &9.6123457377e+00, 9.3524520785e+00, 9.1108022561e+00, 8.9013927747e+00, 8.7163328517e+00,&
      &8.5369317208e+00, 8.3496056828e+00, 8.1568666007e+00, 7.9604848213e+00, 7.7623773616e+00,&
      &7.5688135236e+00, 7.3895629060e+00, 7.2205409289e+00, 7.0561845007e+00, 6.9028867677e+00,&
      &6.7581420768e+00, 6.6022790568e+00, 6.4443994775e+00, 6.3063671904e+00, 6.2137713354e+00,&
      &6.1917416444e+00/)

! scaled negative reflected pressure pr- : g,cm,mus,mbar
      pblast_data%pr_  = (&
      &/1.0155727559e+00, 1.0155187210e+00, 1.0158481988e+00, 1.0166345335e+00, 1.0176176085e+00,&
      &1.0187503089e+00, 1.0199882290e+00, 1.0212781998e+00, 1.0225572076e+00, 1.0237512003e+00,&
      &1.0248703642e+00, 1.0259987417e+00, 1.0271143291e+00, 1.0281905959e+00, 1.0291957396e+00,&
      &1.0299567900e+00, 1.0304070290e+00, 1.0306164951e+00, 1.0306700246e+00, 1.0306691132e+00,&
      &1.0307339739e+00, 1.0310058096e+00, 1.0316493215e+00, 1.0328279682e+00, 1.0344921427e+00,&
      &1.0364656389e+00, 1.0385376817e+00, 1.0404590397e+00, 1.0419373742e+00, 1.0427621454e+00,&
      &1.0434626605e+00, 1.0441465927e+00, 1.0448128409e+00, 1.0454603580e+00, 1.0460881677e+00,&
      &1.0466953837e+00, 1.0472812307e+00, 1.0478463675e+00, 1.0484652771e+00, 1.0491643421e+00,&
      &1.0499158677e+00, 1.0506868108e+00, 1.0514381239e+00, 1.0521240306e+00, 1.0526912264e+00,&
      &1.0530779974e+00, 1.0532132483e+00, 1.0530584768e+00, 1.0528383087e+00, 1.0526004121e+00,&
      &1.0523460944e+00, 1.0520770132e+00, 1.0517952215e+00, 1.0515032187e+00, 1.0512040062e+00,&
      &1.0509011493e+00, 1.0505988458e+00, 1.0502996235e+00, 1.0499940942e+00, 1.0496805726e+00,&
      &1.0493590616e+00, 1.0490295938e+00, 1.0486922354e+00, 1.0483470907e+00, 1.0479943068e+00,&
      &1.0476516289e+00, 1.0474972075e+00, 1.0475321385e+00, 1.0476891623e+00, 1.0478878526e+00,&
      &1.0480329914e+00, 1.0480127753e+00, 1.0476904987e+00, 1.0466577191e+00, 1.0450530820e+00,&
      &1.0433842373e+00, 1.0422379463e+00, 1.0416746692e+00, 1.0413806299e+00, 1.0411035509e+00,&
      &1.0405456730e+00, 1.0393731077e+00, 1.0376934601e+00, 1.0358490800e+00, 1.0341468006e+00,&
      &1.0325643851e+00, 1.0309844502e+00, 1.0294092196e+00, 1.0278410590e+00, 1.0262824816e+00,&
      &1.0247361523e+00, 1.0232407953e+00, 1.0220839197e+00, 1.0212768521e+00, 1.0207449489e+00,&
      &1.0203975844e+00, 1.0201261334e+00, 1.0198017405e+00, 1.0192728568e+00, 1.0183625201e+00,&
      &1.0168653548e+00, 1.0145942904e+00, 1.0116441140e+00, 1.0080931492e+00, 1.0040176275e+00,&
      &9.9951169649e-01, 9.9468971847e-01, 9.8965260974e-01, 9.8432191408e-01, 9.7862550628e-01,&
      &9.7249089678e-01, 9.6583574573e-01, 9.5856677487e-01, 9.5057857124e-01, 9.4165884081e-01,&
      &9.3150864777e-01, 9.1986808547e-01, 9.0644263508e-01, 8.9092484377e-01, 8.7420026703e-01,&
      &8.5684166297e-01, 8.3882373398e-01, 8.2012295938e-01, 8.0081685898e-01, 7.8082955335e-01,&
      &7.5967726327e-01, 7.3704557930e-01, 7.1254308982e-01, 6.8593258353e-01, 6.6036436356e-01,&
      &6.3591362868e-01, 6.1165553975e-01, 5.8725387176e-01, 5.6263939736e-01, 5.3833377943e-01,&
      &5.1507232623e-01, 4.9469297929e-01, 4.7689784063e-01, 4.5886509125e-01, 4.4034640651e-01,&
      &4.2157954970e-01, 4.0498632448e-01, 3.9105916257e-01, 3.7667571971e-01, 3.6284677248e-01,&
      &3.5037689768e-01, 3.3853706202e-01, 3.2680393351e-01, 3.1562728782e-01, 3.0550904870e-01,&
      &2.9612729758e-01, 2.8727320876e-01, 2.7847205327e-01, 2.6981413277e-01, 2.6159657437e-01,&
      &2.5375252189e-01, 2.4621558696e-01, 2.3889016235e-01, 2.3190625607e-01, 2.2509156047e-01,&
      &2.1839940153e-01, 2.1177455625e-01, 2.0518597288e-01, 1.9928581125e-01, 1.9403922321e-01,&
      &1.8869970373e-01, 1.8322235193e-01, 1.7764875976e-01, 1.7250271620e-01, 1.6800836035e-01,&
      &1.6373620405e-01, 1.5946809127e-01, 1.5503415105e-01, 1.5117508571e-01, 1.4769153430e-01,&
      &1.4415715721e-01, 1.4034253137e-01, 1.3652022535e-01, 1.3329985353e-01, 1.3026466701e-01,&
      &1.2706280485e-01, 1.2378065180e-01, 1.2055404217e-01, 1.1780279885e-01, 1.1526425918e-01,&
      &1.1255321476e-01, 1.0979550543e-01, 1.0726019545e-01, 1.0499139230e-01, 1.0273883662e-01,&
      &1.0033211993e-01, 9.7765965229e-02, 9.5186365358e-02, 9.2789226136e-02, 9.0694166201e-02,&
      &8.8698991204e-02, 8.6612005676e-02, 8.4532427933e-02, 8.2625297823e-02, 8.0755459738e-02,&
      &7.8861467045e-02, 7.6924800615e-02, 7.4950462652e-02, 7.3028671360e-02, 7.1308769998e-02,&
      &6.9639182683e-02, 6.8021096295e-02, 6.6534122544e-02, 6.5124209273e-02, 6.3719361849e-02,&
      &6.2299400177e-02, 6.0853673125e-02, 5.9391484663e-02, 5.7924317619e-02, 5.6506160654e-02,&
      &5.5193903084e-02, 5.4012442470e-02, 5.2928347382e-02, 5.1872718146e-02, 5.0797130947e-02,&
      &4.9713754806e-02, 4.8654030584e-02, 4.7644860640e-02, 4.6669635576e-02, 4.5721442454e-02,&
      &4.4794866964e-02, 4.3883199769e-02, 4.2978267228e-02, 4.2070243992e-02, 4.1147445670e-02,&
      &4.0196017281e-02, 3.9212310805e-02, 3.8213562096e-02, 3.7221736374e-02, 3.6262544230e-02,&
      &3.5365883950e-02, 3.4566329109e-02, 3.3881624397e-02, 3.3275929580e-02, 3.2713296678e-02,&
      &3.2151173529e-02, 3.1539287880e-02, 3.0820886729e-02, 2.9998678286e-02, 2.9116549776e-02,&
      &2.8218229132e-02, 2.7355320086e-02, 2.6582994256e-02, 2.5893922685e-02, 2.5260662190e-02,&
      &2.4656723130e-02, 2.4049408549e-02, 2.3363032972e-02, 2.2681727127e-02, 2.2186712538e-02,&
      &2.2063223338e-02/)

! scaled negative incoming pressure ps- : g,cm,mus,mbar
      pblast_data%pso_ = (&
      &/1.0178812760e+00, 1.0181762067e+00, 1.0185082250e+00, 1.0188806968e+00, 1.0192675211e+00,&
      &1.0196721252e+00, 1.0201004632e+00, 1.0205594001e+00, 1.0210965031e+00, 1.0218974869e+00,&
      &1.0229233702e+00, 1.0240888527e+00, 1.0252922055e+00, 1.0264132554e+00, 1.0273111582e+00,&
      &1.0279034646e+00, 1.0282595070e+00, 1.0284400531e+00, 1.0285181836e+00, 1.0285810435e+00,&
      &1.0287315702e+00, 1.0290904015e+00, 1.0297924056e+00, 1.0308349052e+00, 1.0321269031e+00,&
      &1.0335781670e+00, 1.0350810101e+00, 1.0365081474e+00, 1.0377103275e+00, 1.0385245014e+00,&
      &1.0391748730e+00, 1.0398218473e+00, 1.0404632529e+00, 1.0410966667e+00, 1.0417193894e+00,&
      &1.0423284192e+00, 1.0429204232e+00, 1.0434917056e+00, 1.0440381740e+00, 1.0445553017e+00,&
      &1.0450380874e+00, 1.0455016664e+00, 1.0459722626e+00, 1.0464486579e+00, 1.0469291829e+00,&
      &1.0474118960e+00, 1.0478945529e+00, 1.0483745724e+00, 1.0488489989e+00, 1.0493144621e+00,&
      &1.0497689038e+00, 1.0502175603e+00, 1.0506494653e+00, 1.0510486272e+00, 1.0513965360e+00,&
      &1.0516718741e+00, 1.0518501955e+00, 1.0519035752e+00, 1.0518000777e+00, 1.0513994743e+00,&
      &1.0506492578e+00, 1.0496343042e+00, 1.0484570034e+00, 1.0472394472e+00, 1.0461258475e+00,&
      &1.0452852061e+00, 1.0448084822e+00, 1.0444107094e+00, 1.0440305592e+00, 1.0436325529e+00,&
      &1.0431749520e+00, 1.0426202959e+00, 1.0420359345e+00, 1.0414343787e+00, 1.0408023430e+00,&
      &1.0401243491e+00, 1.0393824667e+00, 1.0385560289e+00, 1.0376446192e+00, 1.0366793501e+00,&
      &1.0356571434e+00, 1.0345735899e+00, 1.0334239009e+00, 1.0322028752e+00, 1.0309048627e+00,&
      &1.0295237322e+00, 1.0280730076e+00, 1.0265705165e+00, 1.0250188541e+00, 1.0234213194e+00,&
      &1.0217820047e+00, 1.0201079503e+00, 1.0184181988e+00, 1.0167063465e+00, 1.0149583263e+00,&
      &1.0131576384e+00, 1.0112850566e+00, 1.0092789026e+00, 1.0065162892e+00, 1.0027978394e+00,&
      &9.9814276544e-01, 9.9258163830e-01, 9.8615802551e-01, 9.7891617097e-01, 9.7090807201e-01,&
      &9.6225424541e-01, 9.5317601419e-01, 9.4400301993e-01, 9.3398757148e-01, 9.2208189369e-01,&
      &9.0796607894e-01, 8.9271802955e-01, 8.7709700813e-01, 8.6177887486e-01, 8.4629507875e-01,&
      &8.2972878146e-01, 8.1170217340e-01, 7.9282142481e-01, 7.7359754253e-01, 7.5427589969e-01,&
      &7.3236375555e-01, 7.0504999311e-01, 6.7658966796e-01, 6.4617175508e-01, 6.1407341386e-01,&
      &5.8052531801e-01, 5.4871787207e-01, 5.1890657075e-01, 4.9056101248e-01, 4.6635915180e-01,&
      &4.3844862360e-01, 4.1236243104e-01, 3.9122460653e-01, 3.7231689131e-01, 3.5136995055e-01,&
      &3.3106054449e-01, 3.1448443278e-01, 3.0072294660e-01, 2.8626408684e-01, 2.7082420635e-01,&
      &2.5834037290e-01, 2.4741767401e-01, 2.3687730432e-01, 2.2770408475e-01, 2.1939245017e-01,&
      &2.1138623300e-01, 2.0373218563e-01, 1.9637520468e-01, 1.8954431567e-01, 1.8303194772e-01,&
      &1.7668783174e-01, 1.7035816916e-01, 1.6428211967e-01, 1.5879380786e-01, 1.5376659456e-01,&
      &1.4896755384e-01, 1.4419841261e-01, 1.3961324348e-01, 1.3515459951e-01, 1.3073002977e-01,&
      &1.2636237028e-01, 1.2255579271e-01, 1.1936155098e-01, 1.1596861792e-01, 1.1224184053e-01,&
      &1.0866001588e-01, 1.0539286462e-01, 1.0228727606e-01, 9.9261952088e-02, 9.6380631608e-02,&
      &9.3598405826e-02, 9.0992199831e-02, 8.8858073432e-02, 8.6760304588e-02, 8.4604069538e-02,&
      &8.2386881910e-02, 8.0107151657e-02, 7.7764921684e-02, 7.5436018173e-02, 7.3386122502e-02,&
      &7.1484085476e-02, 6.9522754875e-02, 6.7323963616e-02, 6.5459042700e-02, 6.3877558086e-02,&
      &6.2247536723e-02, 6.0565216740e-02, 5.8833548104e-02, 5.6962500489e-02, 5.5456727643e-02,&
      &5.4336581992e-02, 5.2772786049e-02, 5.1205697221e-02, 4.9632736185e-02, 4.8066023135e-02,&
      &4.6528494718e-02, 4.5046730181e-02, 4.3629488952e-02, 4.2253263644e-02, 4.0919940231e-02,&
      &3.9631955351e-02, 3.8391594590e-02, 3.7200950990e-02, 3.6061878149e-02, 3.4975937335e-02,&
      &3.3944337984e-02, 3.2967870889e-02, 3.2046833325e-02, 3.1180945267e-02, 3.0369255795e-02,&
      &2.9610038699e-02, 2.8900474505e-02, 2.8204988390e-02, 2.7501748339e-02, 2.6794889508e-02,&
      &2.6089226389e-02, 2.5390331012e-02, 2.4704619043e-02, 2.4039444513e-02, 2.3403203991e-02,&
      &2.2805451097e-02, 2.2257022311e-02, 2.1753602841e-02, 2.1240635674e-02, 2.0716548429e-02,&
      &2.0186677947e-02, 1.9657299915e-02, 1.9135740239e-02, 1.8630497831e-02, 1.8151379891e-02,&
      &1.7700415379e-02, 1.7235721202e-02, 1.6763301671e-02, 1.6300873019e-02, 1.5869178003e-02,&
      &1.5492344224e-02, 1.5198279178e-02, 1.4967385142e-02, 1.4731072182e-02, 1.4488165989e-02,&
      &1.4238482608e-02, 1.3981832955e-02, 1.3718022665e-02, 1.3446851955e-02, 1.3168115466e-02,&
      &1.2881602109e-02, 1.2587094906e-02, 1.2283059396e-02, 1.1961360483e-02, 1.1629914071e-02,&
      &1.1301859651e-02, 1.0992620045e-02, 1.0720173214e-02, 1.0505351988e-02, 1.0372174408e-02,&
      &1.0342135940e-02/)

! scaled negative impulse duration t0- : g,cm,mus,mbar
      pblast_data%t0_  = (&
      &/8.2367135310e+03, 8.2384805202e+03, 8.2403894189e+03, 8.2424287459e+03, 8.2447616018e+03,&
      &8.2474380154e+03, 8.2502280131e+03, 8.2528596276e+03, 8.2550236477e+03, 8.2569036097e+03,&
      &8.2587030723e+03, 8.2604136232e+03, 8.2620263835e+03, 8.2635319911e+03, 8.2649205853e+03,&
      &8.2661817907e+03, 8.2673128044e+03, 8.2684290993e+03, 8.2695725923e+03, 8.2707447096e+03,&
      &8.2719470311e+03, 8.2731813086e+03, 8.2744494843e+03, 8.2757537125e+03, 8.2770963823e+03,&
      &8.2784801437e+03, 8.2799080746e+03, 8.2813836799e+03, 8.2829084451e+03, 8.2844835088e+03,&
      &8.2861099693e+03, 8.2877888740e+03, 8.2895212084e+03, 8.2913078828e+03, 8.2935613684e+03,&
      &8.2970045893e+03, 8.3016931926e+03, 8.3076689158e+03, 8.3149700618e+03, 8.3236307133e+03,&
      &8.3336798488e+03, 8.3453039926e+03, 8.3601559296e+03, 8.3777817470e+03, 8.3968818102e+03,&
      &8.4159123197e+03, 8.4330555484e+03, 8.4468432013e+03, 8.4602095072e+03, 8.4738970884e+03,&
      &8.4879409846e+03, 8.5023811845e+03, 8.5172632026e+03, 8.5326387165e+03, 8.5485511946e+03,&
      &8.5649295315e+03, 8.5817650822e+03, 8.5990718611e+03, 8.6168644518e+03, 8.6351595118e+03,&
      &8.6540047405e+03, 8.6734170515e+03, 8.6933942607e+03, 8.7139313536e+03, 8.7350200991e+03,&
      &8.7566486218e+03, 8.7787773857e+03, 8.8009395154e+03, 8.8230247864e+03, 8.8451348588e+03,&
      &8.8673924209e+03, 8.8899438647e+03, 8.9129622461e+03, 8.9366382999e+03, 8.9608729764e+03,&
      &8.9855875934e+03, 9.0107937772e+03, 9.0365041827e+03, 9.0627326302e+03, 9.0894942598e+03,&
      &9.1168057021e+03, 9.1445640103e+03, 9.1726311196e+03, 9.2010144585e+03, 9.2297242121e+03,&
      &9.2587733171e+03, 9.2881778840e+03, 9.3179576680e+03, 9.3481184679e+03, 9.3784525628e+03,&
      &9.4088638697e+03, 9.4393111513e+03, 9.4697495755e+03, 9.5001304776e+03, 9.5304011081e+03,&
      &9.5605043664e+03, 9.5903785165e+03, 9.6200309360e+03, 9.6498307948e+03, 9.6798263541e+03,&
      &9.7100021851e+03, 9.7403425445e+03, 9.7708314838e+03, 9.8014529782e+03, 9.8321910772e+03,&
      &9.8630300788e+03, 9.8939547318e+03, 9.9249919549e+03, 9.9563455766e+03, 9.9879892913e+03,&
      &1.0019848018e+04, 1.0051835565e+04, 1.0083853437e+04, 1.0115789529e+04, 1.0147516729e+04,&
      &1.0179446489e+04, 1.0211853610e+04, 1.0244377709e+04, 1.0276597058e+04, 1.0308021377e+04,&
      &1.0338083887e+04, 1.0366132567e+04, 1.0391420588e+04, 1.0415013771e+04, 1.0438594463e+04,&
      &1.0462352972e+04, 1.0486515378e+04, 1.0511347947e+04, 1.0537162006e+04, 1.0564320494e+04,&
      &1.0593356370e+04, 1.0624164760e+04, 1.0656217732e+04, 1.0688885577e+04, 1.0721424372e+04,&
      &1.0752962240e+04, 1.0782346023e+04, 1.0808160443e+04, 1.0830652836e+04, 1.0850401106e+04,&
      &1.0868114614e+04, 1.0884651033e+04, 1.0901034989e+04, 1.0918405671e+04, 1.0936525555e+04,&
      &1.0955024874e+04, 1.0973824423e+04, 1.0992830597e+04, 1.1011933680e+04, 1.1031005975e+04,&
      &1.1049919864e+04, 1.1068797240e+04, 1.1087627140e+04, 1.1106313717e+04, 1.1124747767e+04,&
      &1.1142805283e+04, 1.1160345889e+04, 1.1177207543e+04, 1.1192698977e+04, 1.1205988157e+04,&
      &1.1216424112e+04, 1.1223266447e+04, 1.1225865677e+04, 1.1227008648e+04, 1.1227877160e+04,&
      &1.1228438350e+04, 1.1228656337e+04, 1.1228491957e+04, 1.1227902479e+04, 1.1226841301e+04,&
      &1.1225258312e+04, 1.1223181944e+04, 1.1220739600e+04, 1.1218073654e+04, 1.1215353720e+04,&
      &1.1212779983e+04, 1.1210586882e+04, 1.1209047174e+04, 1.1208166039e+04, 1.1207473019e+04,&
      &1.1206967260e+04, 1.1206665675e+04, 1.1206586313e+04, 1.1206748422e+04, 1.1207172526e+04,&
      &1.1207880503e+04, 1.1208895844e+04, 1.1210332847e+04, 1.1212214146e+04, 1.1214436649e+04,&
      &1.1216875944e+04, 1.1219383639e+04, 1.1221784419e+04, 1.1223872797e+04, 1.1225409531e+04,&
      &1.1226193508e+04, 1.1226312134e+04, 1.1225951903e+04, 1.1225336986e+04, 1.1224734040e+04,&
      &1.1224457400e+04, 1.1224874819e+04, 1.1226274545e+04, 1.1228087631e+04, 1.1230270123e+04,&
      &1.1232937189e+04, 1.1236220614e+04, 1.1240270668e+04, 1.1245170018e+04, 1.1250605705e+04,&
      &1.1256422577e+04, 1.1262475206e+04, 1.1268590735e+04, 1.1274565526e+04, 1.1280360609e+04,&
      &1.1286224261e+04, 1.1292119085e+04, 1.1297991188e+04, 1.1303778009e+04, 1.1309406804e+04,&
      &1.1314525903e+04, 1.1318915543e+04, 1.1322637187e+04, 1.1325769929e+04, 1.1328047073e+04,&
      &1.1329465220e+04, 1.1330817452e+04, 1.1333086739e+04, 1.1338187089e+04, 1.1342572918e+04,&
      &1.1342796326e+04, 1.1341864373e+04, 1.1340382835e+04, 1.1338936739e+04, 1.1338215615e+04,&
      &1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04,&
      &1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04,&
      &1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04,&
      &1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04, 1.1338212354e+04,&
      &1.1338212354e+04/)


! surface burst - fig 2-15 - scaled radius r/w**(1/3): g,cm,mus,mbar
      pblast_data%rw3_surf = (/&
      &0.50000000000000e+00,0.51328038846000e+00,0.52691351436000e+00,0.54090874667000e+00,0.55527570323000e+00,&
      &0.57002425731000e+00,0.58516454445000e+00,0.60070696938000e+00,0.61666221319000e+00,0.63304124067000e+00,&
      &0.64985530785000e+00,0.66711596971000e+00,0.68483508817000e+00,0.70302484017000e+00,0.72169772613000e+00,&
      &0.74086657843000e+00,0.76054457035000e+00,0.78074522503000e+00,0.80148242478000e+00,0.82277042068000e+00,&
      &0.84462384228000e+00,0.86705770774000e+00,0.89008743409000e+00,0.91372884787000e+00,0.93799819597000e+00,&
      &0.96291215681000e+00,0.98848785180000e+00,0.10147428571000e+01,0.10416952158000e+01,0.10693634500000e+01,&
      &0.10977665741000e+01,0.11269241072000e+01,0.11568560870000e+01,0.11875830835000e+01,0.12191262128000e+01,&
      &0.12515071522000e+01,0.12847481545000e+01,0.13188720636000e+01,0.13539023303000e+01,0.13898630281000e+01,&
      &0.14267788699000e+01,0.14646752252000e+01,0.15035781371000e+01,0.15435143406000e+01,0.15845112807000e+01,&
      &0.16265971313000e+01,0.16698008149000e+01,0.17141520218000e+01,0.17596812313000e+01,0.18064197319000e+01,&
      &0.18543996435000e+01,0.19036539387000e+01,0.19542164663000e+01,0.20061219739000e+01,0.20594061322000e+01,&
      &0.21141055590000e+01,0.21702578452000e+01,0.22279015797000e+01,0.22870763766000e+01,0.23478229020000e+01,&
      &0.24101829024000e+01,0.24741992328000e+01,0.25399158867000e+01,0.26073780259000e+01,0.26766320120000e+01,&
      &0.27477254378000e+01,0.28207071602000e+01,0.28956273339000e+01,0.29725374455000e+01,0.30514903495000e+01,&
      &0.31325403040000e+01,0.32157430082000e+01,0.33011556409000e+01,0.33888368994000e+01,0.34788470403000e+01,&
      &0.35712479205000e+01,0.36661030399000e+01,0.37634775849000e+01,0.38634384735000e+01,0.39660544010000e+01,&
      &0.40713958872000e+01,0.41795353251000e+01,0.42905470305000e+01,0.44045072931000e+01,0.45214944287000e+01,&
      &0.46415888336000e+01,0.47648730392000e+01,0.48914317691000e+01,0.50213519971000e+01,0.51547230074000e+01,&
      &0.52916364553000e+01,0.54321864307000e+01,0.55764695227000e+01,0.57245848857000e+01,0.58766343078000e+01,&
      &0.60327222808000e+01,0.61929560715000e+01,0.63574457962000e+01,0.65263044958000e+01,0.66996482137000e+01,&
      &0.68775960753000e+01,0.70602703704000e+01,0.72477966368000e+01,0.74403037464000e+01,0.76379239945000e+01,&
      &0.78407931899000e+01,0.80490507487000e+01,0.82628397901000e+01,0.84823072345000e+01,0.87076039047000e+01,&
      &0.89388846296000e+01,0.91763083502000e+01,0.94200382292000e+01,0.96702417633000e+01,0.99270908975000e+01,&
      &0.10190762144000e+02,0.10461436704000e+02,0.10739300591000e+02,0.11024544758000e+02,0.11317365232000e+02,&
      &0.11617963246000e+02,0.11926545376000e+02,0.12243323687000e+02,0.12568515876000e+02,0.12902345423000e+02,&
      &0.13245041741000e+02,0.13596840340000e+02,0.13957982983000e+02,0.14328717856000e+02,0.14709299734000e+02,&
      &0.15099990163000e+02,0.15501057633000e+02,0.15912777767000e+02,0.16335433508000e+02,0.16769315313000e+02,&
      &0.17214721356000e+02,0.17671957730000e+02,0.18141338657000e+02,0.18623186706000e+02,0.19117833014000e+02,&
      &0.19625617512000e+02,0.20146889161000e+02,0.20682006189000e+02,0.21231336342000e+02,0.21795257130000e+02,&
      &0.22374156093000e+02,0.22968431062000e+02,0.23578490436000e+02,0.24204753460000e+02,0.24847650517000e+02,&
      &0.25507623420000e+02,0.26185125715000e+02,0.26880622998000e+02,0.27594593229000e+02,0.28327527064000e+02,&
      &0.29079928191000e+02,0.29852313677000e+02,0.30645214321000e+02,0.31459175023000e+02,0.32294755153000e+02,&
      &0.33152528940000e+02,0.34033085866000e+02,0.34937031067000e+02,0.35864985756000e+02,0.36817587642000e+02,&
      &0.37795491374000e+02,0.38799368989000e+02,0.39829910374000e+02,0.40887823738000e+02,0.41973836103000e+02,&
      &0.43088693801000e+02,0.44233162985000e+02,0.45408030159000e+02,0.46614102719000e+02,0.47852209503000e+02,&
      &0.49123201365000e+02,0.50427951758000e+02,0.51767357335000e+02,0.53142338565000e+02,0.54553840365000e+02,&
      &0.56002832749000e+02,0.57490311497000e+02,0.59017298836000e+02,0.60584844145000e+02,0.62194024675000e+02,&
      &0.63845946291000e+02,0.65541744228000e+02,0.67282583875000e+02,0.69069661576000e+02,0.70904205450000e+02,&
      &0.72787476233000e+02,0.74720768153000e+02,0.76705409807000e+02,0.78742765086000e+02,0.80834234103000e+02,&
      &0.82981254163000e+02,0.85185300744000e+02,0.87447888514000e+02,0.89770572373000e+02,0.92154948521000e+02,&
      &0.94602655551000e+02,0.97115375581000e+02,0.99694835408000e+02,0.10234280769000e+03,0.10506111218000e+03,&
      &0.10785161694000e+03,0.11071623968000e+03,0.11365694902000e+03,0.11667576589000e+03,0.11977476488000e+03,&
      &0.12295607569000e+03,0.12622188459000e+03,0.12957443591000e+03,0.13301603360000e+03,0.13654904279000e+03,&
      &0.14017589146000e+03,0.14389907204000e+03,0.14772114319000e+03,0.15164473152000e+03,0.15567253341000e+03,&
      &0.15980731684000e+03,0.16405192334000e+03,0.16840926987000e+03,0.17288235092000e+03,0.17747424048000e+03,&
      &0.18218809419000e+03,0.18702715152000e+03,0.19199473797000e+03,0.19709426738000e+03,0.20232924424000e+03,&
      &0.20770326617000e+03,0.21322002628000e+03,0.21888331584000e+03,0.22469702676000e+03,0.23066515437000e+03,&
      &0.23679180008000e+03,0.24308117425000e+03,0.24953759910000e+03,0.25616551160000e+03,0.26296946661000e+03,&
      &0.26995413995000e+03,0.27712433164000e+03,0.28448496919000e+03,0.29204111100000e+03,0.29979794980000e+03,&
      &0.30776081627000e+03,0.31593518265000e+03,0.32432666656000e+03,0.33294103480000e+03,0.34178420736000e+03,&
      &0.35086226145000e+03,0.36018143570000e+03,0.36974813447000e+03,0.37956893219000e+03,0.38965057792000e+03,&
      &0.40000000000000e+03      /)

! surface burst - fig 2-15 - reflected wave impulse+ iso/w**(1/3): g,cm,mus,mbar
      pblast_data%ir_surf = (/&
      &0.14620100371000e+00,0.14144121241000e+00,0.13650614490000e+00,0.13141457452000e+00,0.12618916640000e+00,&
      &0.12085693786000e+00,0.11544976464000e+00,0.11000493693000e+00,0.10456576994000e+00,0.99182273823000e-01,&
      &0.93911888417000e-01,0.88820288631000e-01,0.83982195045000e-01,0.79428806359000e-01,0.75144411207000e-01,&
      &0.71110834997000e-01,0.67311375052000e-01,0.63729542237000e-01,0.60351715722000e-01,0.57169680161000e-01,&
      &0.54170174489000e-01,0.51341303266000e-01,0.48672038571000e-01,0.46151349949000e-01,0.43771940748000e-01,&
      &0.41527321525000e-01,0.39408045019000e-01,0.37405674736000e-01,0.35512954386000e-01,0.33723365988000e-01,&
      &0.32032621339000e-01,0.30434629996000e-01,0.28923368761000e-01,0.27493299129000e-01,0.26139412888000e-01,&
      &0.24858315062000e-01,0.23646221257000e-01,0.22498566377000e-01,0.21411414703000e-01,0.20381137242000e-01,&
      &0.19404513640000e-01,0.18479528668000e-01,0.17602940494000e-01,0.16771611415000e-01,0.15982866405000e-01,&
      &0.15234320323000e-01,0.14524253448000e-01,0.13850707663000e-01,0.13211346061000e-01,0.12604155999000e-01,&
      &0.12027295601000e-01,0.11479104880000e-01,0.10958614952000e-01,0.10464103292000e-01,0.99940033542000e-02,&
      &0.95469067101000e-02,0.91214902814000e-02,0.87170434513000e-02,0.83324111561000e-02,0.79664261423000e-02,&
      &0.76180171611000e-02,0.72861783441000e-02,0.69702152924000e-02,0.66694387321000e-02,0.63829815638000e-02,&
      &0.61100293821000e-02,0.58498121717000e-02,0.56016903322000e-02,0.53652698434000e-02,0.51398842345000e-02,&
      &0.49249278347000e-02,0.47198282716000e-02,0.45240305100000e-02,0.43372265549000e-02,0.41589891176000e-02,&
      &0.39888434011000e-02,0.38263597124000e-02,0.36711139971000e-02,0.35227646621000e-02,0.33811234796000e-02,&
      &0.32458040917000e-02,0.31164251217000e-02,0.29927007017000e-02,0.28743779706000e-02,0.27612588905000e-02,&
      &0.26530778716000e-02,0.25495923460000e-02,0.24505544789000e-02,0.23557203871000e-02,0.22649464640000e-02,&
      &0.21780819374000e-02,0.20949106507000e-02,0.20152476850000e-02,0.19389134048000e-02,0.18657427168000e-02,&
      &0.17956698707000e-02,0.17285209509000e-02,0.16641436141000e-02,0.16024140567000e-02,0.15432000108000e-02,&
      &0.14864154922000e-02,0.14319576503000e-02,0.13797146685000e-02,0.13295814851000e-02,0.12814500841000e-02,&
      &0.12352350904000e-02,0.11908842256000e-02,0.11483052551000e-02,0.11074148810000e-02,0.10681270442000e-02,&
      &0.10303618027000e-02,0.99409309382000e-03,0.95924334517000e-03,0.92574673494000e-03,0.89354534402000e-03,&
      &0.86257375599000e-03,0.83278604465000e-03,0.80415045665000e-03,0.77660640050000e-03,0.75010609575000e-03,&
      &0.72460315109000e-03,0.70004794851000e-03,0.67642152008000e-03,0.65368165354000e-03,0.63178921574000e-03,&
      &0.61070700622000e-03,0.59039466326000e-03,0.57082555406000e-03,0.55198202878000e-03,0.53382927400000e-03,&
      &0.51633375916000e-03,0.49946724746000e-03,0.48320479972000e-03,0.46753194894000e-03,0.45242184690000e-03,&
      &0.43785155144000e-03,0.42379802868000e-03,0.41023604485000e-03,0.39715208693000e-03,0.38453256398000e-03,&
      &0.37235617913000e-03,0.36060339883000e-03,0.34925593007000e-03,0.33829768083000e-03,0.32772153338000e-03,&
      &0.31751044694000e-03,0.30764915483000e-03,0.29812251525000e-03,0.28891537773000e-03,0.28002187211000e-03,&
      &0.27143145403000e-03,0.26313035973000e-03,0.25510576816000e-03,0.24734677007000e-03,0.23984539435000e-03,&
      &0.23259462628000e-03,0.22558405695000e-03,0.21880424466000e-03,0.21224555611000e-03,0.20589899017000e-03,&
      &0.19976066401000e-03,0.19382209810000e-03,0.18807623196000e-03,0.18251542352000e-03,0.17713096910000e-03,&
      &0.17191847381000e-03,0.16687444889000e-03,0.16199162903000e-03,0.15726276464000e-03,0.15268213816000e-03,&
      &0.14824489718000e-03,0.14394733068000e-03,0.13978499620000e-03,0.13575301495000e-03,0.13184569875000e-03,&
      &0.12805801237000e-03,0.12438772806000e-03,0.12083091065000e-03,0.11738397908000e-03,0.11404202152000e-03,&
      &0.11080052494000e-03,0.10765772158000e-03,0.10461028278000e-03,0.10165617732000e-03,0.98791283032000e-04,&
      &0.96011057197000e-04,0.93313146418000e-04,0.90696164013000e-04,0.88157944364000e-04,0.85695185309000e-04,&
      &0.83304946477000e-04,0.80984625567000e-04,0.78732078781000e-04,0.76546388785000e-04,0.74424962790000e-04,&
      &0.72365336862000e-04,0.70365173656000e-04,0.68422332055000e-04,0.66536252030000e-04,0.64704924781000e-04,&
      &0.62926426449000e-04,0.61198750380000e-04,0.59519896820000e-04,0.57888995475000e-04,0.56305089143000e-04,&
      &0.54766237348000e-04,0.53270881855000e-04,0.51817626238000e-04,0.50404998081000e-04,0.49032356052000e-04,&
      &0.47698466835000e-04,0.46402055974000e-04,0.45141741573000e-04,0.43916142939000e-04,0.42724599578000e-04,&
      &0.41566469147000e-04,0.40440530100000e-04,0.39345681230000e-04,0.38280822249000e-04,0.37244931743000e-04,&
      &0.36237727877000e-04,0.35258426116000e-04,0.34305880403000e-04,0.33379152168000e-04,0.32477567016000e-04,&
      &0.31600547717000e-04,0.30747536516000e-04,0.29917690903000e-04,0.29110273076000e-04,0.28324545203000e-04,&
      &0.27559833640000e-04,0.26815881211000e-04,0.26092139745000e-04,0.25387793825000e-04,0.24702173039000e-04,&
      &0.24034860466000e-04,0.23385404390000e-04,0.22753425698000e-04,0.22138406266000e-04,0.21539677031000e-04,&
      &0.20956678388000e-04,0.20389191065000e-04,0.19836935538000e-04,0.19299352397000e-04,0.18776003125000e-04,&
      &0.18266448822000e-04,0.17770231127000e-04,0.17287241552000e-04,0.16817116123000e-04,0.16359371442000e-04,&
      &0.15913655702000e-04,0.15479636026000e-04,0.15056958466000e-04,0.14646395363000e-04,0.14242187735000e-04,&
      &0.13862594070000e-04      /)

! surface burst - fig 2-15 - incident wave impulse+ iso/w**(1/3): g,cm,mus,mbar
      pblast_data%iso_surf = (/&
      &0.45259316942000e-02,0.43840173716000e-02,0.42424991037000e-02,0.41015308242000e-02,0.39612716642000e-02,&
      &0.38218856092000e-02,0.36835410767000e-02,0.35464104049000e-02,0.34106692427000e-02,0.32764958279000e-02,&
      &0.31440701438000e-02,0.30135729368000e-02,0.28851847432000e-02,0.27591920026000e-02,0.26359238204000e-02,&
      &0.25157212129000e-02,0.23988421767000e-02,0.22854594214000e-02,0.21757206140000e-02,0.20698013261000e-02,&
      &0.19677625040000e-02,0.18696612685000e-02,0.17755191273000e-02,0.16852938995000e-02,0.15989920260000e-02,&
      &0.15166207878000e-02,0.14380897184000e-02,0.13632929819000e-02,0.12921305520000e-02,0.12245014749000e-02,&
      &0.11603535956000e-02,0.10995589003000e-02,0.10419869852000e-02,0.98750009132000e-03,0.93595251649000e-03,&
      &0.88726108920000e-03,0.84131369640000e-03,0.79796651750000e-03,0.75708787318000e-03,0.71854672783000e-03,&
      &0.68222594307000e-03,0.64805388517000e-03,0.61590559896000e-03,0.58565810105000e-03,0.55720082631000e-03,&
      &0.53043171545000e-03,0.50528224705000e-03,0.48166577577000e-03,0.45948317116000e-03,0.43864498898000e-03,&
      &0.41906940397000e-03,0.40068880646000e-03,0.38345112671000e-03,0.36728154475000e-03,0.35211051976000e-03,&
      &0.33787513651000e-03,0.32451939076000e-03,0.31200644021000e-03,0.30028639312000e-03,0.28930717702000e-03,&
      &0.27902179937000e-03,0.26938809574000e-03,0.26037462775000e-03,0.25195150946000e-03,0.24408316457000e-03,&
      &0.23673461667000e-03,0.22987393737000e-03,0.22347686488000e-03,0.21752649064000e-03,0.21199668748000e-03,&
      &0.20686450996000e-03,0.20210826180000e-03,0.19770724399000e-03,0.19365049829000e-03,0.18992449098000e-03,&
      &0.18651190139000e-03,0.18339839255000e-03,0.18057145715000e-03,0.17802015565000e-03,0.17573920831000e-03,&
      &0.17371858621000e-03,0.17194832787000e-03,0.17042102715000e-03,0.16913105517000e-03,0.16807481948000e-03,&
      &0.16724750378000e-03,0.16664503425000e-03,0.16626421276000e-03,0.16610201743000e-03,0.16615753618000e-03,&
      &0.16643077548000e-03,0.16692209054000e-03,0.16763114157000e-03,0.16855780291000e-03,0.16970464130000e-03,&
      &0.17107584259000e-03,0.17267312849000e-03,0.17450125261000e-03,0.17656441134000e-03,0.17886595068000e-03,&
      &0.18141384385000e-03,0.18421475531000e-03,0.18727433578000e-03,0.19060096817000e-03,0.19420358679000e-03,&
      &0.19808999158000e-03,0.20227217100000e-03,0.20675917941000e-03,0.21156247193000e-03,0.21669474302000e-03,&
      &0.22216736549000e-03,0.22801317471000e-03,0.23443444791000e-03,0.23687244285000e-03,0.23273229481000e-03,&
      &0.23097259472000e-03,0.22870156905000e-03,0.22600887748000e-03,0.22295374751000e-03,0.21959560881000e-03,&
      &0.21598630785000e-03,0.21217100923000e-03,0.20818241113000e-03,0.20407260147000e-03,0.19986356226000e-03,&
      &0.19559165877000e-03,0.19128685329000e-03,0.18696994282000e-03,0.18266001600000e-03,0.17837683984000e-03,&
      &0.17413592781000e-03,0.16995005932000e-03,0.16583016197000e-03,0.16178316724000e-03,0.15781704439000e-03,&
      &0.15393783793000e-03,0.15014855812000e-03,0.14645114588000e-03,0.14284859156000e-03,0.13934222389000e-03,&
      &0.13593193392000e-03,0.13261689573000e-03,0.12939488945000e-03,0.12626382804000e-03,0.12322487444000e-03,&
      &0.12027475330000e-03,0.11740993146000e-03,0.11462833526000e-03,0.11192707761000e-03,0.10930321066000e-03,&
      &0.10675469682000e-03,0.10427824240000e-03,0.10187085057000e-03,0.99529423088000e-04,0.97250794558000e-04,&
      &0.95033317188000e-04,0.92875266761000e-04,0.90772710144000e-04,0.88722154992000e-04,0.86722347377000e-04,&
      &0.84771656382000e-04,0.82867788785000e-04,0.81008477752000e-04,0.79191697858000e-04,0.77415397782000e-04,&
      &0.75678216055000e-04,0.73979114884000e-04,0.72316470396000e-04,0.70688651921000e-04,0.69094171034000e-04,&
      &0.67531886574000e-04,0.66001191387000e-04,0.64501049398000e-04,0.63030877097000e-04,0.61589485454000e-04,&
      &0.60175497621000e-04,0.58788972045000e-04,0.57429481020000e-04,0.56096497211000e-04,0.54789113574000e-04,&
      &0.53506580288000e-04,0.52248866884000e-04,0.51015839252000e-04,0.49807236191000e-04,0.48622371589000e-04,&
      &0.47460642321000e-04,0.46321950088000e-04,0.45206509641000e-04,0.44113976042000e-04,0.43043941328000e-04,&
      &0.41996110390000e-04,0.40970138077000e-04,0.39965882859000e-04,0.38983414527000e-04,0.38022420879000e-04,&
      &0.37082630421000e-04,0.36163593701000e-04,0.35264913225000e-04,0.34386992834000e-04,0.33529272720000e-04,&
      &0.32691457662000e-04,0.31873283598000e-04,0.31074221366000e-04,0.30294151881000e-04,0.29533098204000e-04,&
      &0.28790533792000e-04,0.28065972595000e-04,0.27359114422000e-04,0.26669636230000e-04,0.25997378246000e-04,&
      &0.25341946001000e-04,0.24702984553000e-04,0.24080003149000e-04,0.23472473806000e-04,0.22880299969000e-04,&
      &0.22303253487000e-04,0.21740791035000e-04,0.21192480480000e-04,0.20657891025000e-04,0.20136570395000e-04,&
      &0.19628515219000e-04,0.19133280321000e-04,0.18650335261000e-04,0.18179243038000e-04,0.17719668199000e-04,&
      &0.17271533397000e-04,0.16834367611000e-04,0.16407787573000e-04,0.15991527214000e-04,0.15585183020000e-04,&
      &0.15188423527000e-04,0.14801110860000e-04,0.14422991441000e-04,0.14053702054000e-04,0.13692898011000e-04,&
      &0.13340358168000e-04,0.12995942320000e-04,0.12659448828000e-04,0.12330656169000e-04,0.12009365507000e-04,&
      &0.11695346466000e-04,0.11388426742000e-04,0.11088490570000e-04,0.10795547250000e-04,0.10509394130000e-04,&
      &0.10229761929000e-04,0.99566618937000e-05,0.96901227325000e-05,0.94300952617000e-05,0.91764468617000e-05,&
      &0.89290872150000e-05,0.86880179246000e-05,0.84533043913000e-05,0.82253887694000e-05,0.80016335871000e-05,&
      &0.77910186478000e-05      /)

! surface burst - fig 2-15 - reflected pressure pr/w**(1/3) : g,cm,mus,mbar
      pblast_data%pr_surf = (/&
      &0.10247858483000e-01,0.99942560801000e-02,0.97377651477000e-02,0.94789610283000e-02,0.92185058512000e-02,&
      &0.89571576652000e-02,0.86957804253000e-02,0.84353549081000e-02,0.81769906394000e-02,0.79219389215000e-02,&
      &0.76716070577000e-02,0.74275738783000e-02,0.71916052999000e-02,0.69646812536000e-02,0.67468721703000e-02,&
      &0.65380624601000e-02,0.63379106259000e-02,0.61459029796000e-02,0.59615417669000e-02,0.57845237645000e-02,&
      &0.56142731786000e-02,0.54502261136000e-02,0.52918394107000e-02,0.51385615794000e-02,0.49900059094000e-02,&
      &0.48458497450000e-02,0.47056241826000e-02,0.45689359544000e-02,0.44354741706000e-02,0.43049649643000e-02,&
      &0.41772482975000e-02,0.40521182678000e-02,0.39293992169000e-02,0.38089468070000e-02,0.36906541758000e-02,&
      &0.35744878189000e-02,0.34604084088000e-02,0.33483731379000e-02,0.32383792203000e-02,0.31304299761000e-02,&
      &0.30245319542000e-02,0.29207571078000e-02,0.28191312954000e-02,0.27196738232000e-02,0.26224183634000e-02,&
      &0.25274072588000e-02,0.24347036043000e-02,0.23443513330000e-02,0.22563611791000e-02,0.21707525095000e-02,&
      &0.20875440487000e-02,0.20067443071000e-02,0.19283894071000e-02,0.18524641416000e-02,0.17789501177000e-02,&
      &0.17078309760000e-02,0.16390783503000e-02,0.15726802354000e-02,0.15086150212000e-02,0.14468289409000e-02,&
      &0.13872640484000e-02,0.13298694052000e-02,0.12746033899000e-02,0.12214200140000e-02,0.11702590384000e-02,&
      &0.11210583538000e-02,0.10737379836000e-02,0.10282275018000e-02,0.98449894151000e-03,0.94248585888000e-03,&
      &0.90211271854000e-03,0.86330967807000e-03,0.82600960521000e-03,0.79016305492000e-03,0.75572672559000e-03,&
      &0.72263600706000e-03,0.69082140047000e-03,0.66022285741000e-03,0.63079763726000e-03,0.60251709343000e-03,&
      &0.57532346615000e-03,0.54915930117000e-03,0.52398259447000e-03,0.49975746597000e-03,0.47645604602000e-03,&
      &0.45403871257000e-03,0.43247096087000e-03,0.41171716467000e-03,0.39174124736000e-03,0.37252494096000e-03,&
      &0.35404955954000e-03,0.33628483732000e-03,0.31920563834000e-03,0.30278744155000e-03,0.28700796688000e-03,&
      &0.27186244461000e-03,0.25732910057000e-03,0.24338572510000e-03,0.23001756747000e-03,0.21720838703000e-03,&
      &0.20494367191000e-03,0.19321556859000e-03,0.18200767503000e-03,0.17130304767000e-03,0.16108685480000e-03,&
      &0.15134676300000e-03,0.14207428034000e-03,0.13325698887000e-03,0.12487892074000e-03,0.11692335180000e-03,&
      &0.10937686445000e-03,0.10223241192000e-03,0.95476653976000e-04,0.89095080893000e-04,0.83071829264000e-04,&
      &0.77390777554000e-04,0.72041182264000e-04,0.67014504174000e-04,0.62294260845000e-04,0.57865964658000e-04,&
      &0.53715476413000e-04,0.49827985958000e-04,0.46195609374000e-04,0.42805118570000e-04,0.39642994435000e-04,&
      &0.36695980274000e-04,0.33950677605000e-04,0.31397151302000e-04,0.29026969675000e-04,0.26827892306000e-04,&
      &0.24788269638000e-04,0.22897396531000e-04,0.21145607591000e-04,0.19526275555000e-04,0.18030210415000e-04,&
      &0.16648276951000e-04,0.15371944021000e-04,0.14193234135000e-04,0.13106337233000e-04,0.12105499272000e-04,&
      &0.11183667786000e-04,0.10334441724000e-04,0.95519786081000e-05,0.88312230634000e-05,0.81686855708000e-05,&
      &0.75594913042000e-05,0.69990320379000e-05,0.64831844994000e-05,0.60082283105000e-05,0.55715556295000e-05,&
      &0.51702236841000e-05,0.48010928959000e-05,0.44613157284000e-05,0.41483337141000e-05,0.38600990775000e-05,&
      &0.35949839029000e-05,0.33509099381000e-05,0.31259845589000e-05,0.29185056263000e-05,0.27269820476000e-05,&
      &0.25504106158000e-05,0.23875350253000e-05,0.22371132556000e-05,0.20980420510000e-05,0.19693235991000e-05,&
      &0.18501918223000e-05,0.17399990506000e-05,0.16379376848000e-05,0.15432758502000e-05,0.14553678640000e-05,&
      &0.13736638416000e-05,0.12977791037000e-05,0.12272291174000e-05,0.11615628264000e-05,0.11003464922000e-05,&
      &0.10431906572000e-05,0.98984792974000e-06,0.94004707313000e-06,0.89348982127000e-06,0.84989337471000e-06,&
      &0.80901191952000e-06,0.77065855500000e-06,0.73467312801000e-06,0.70087635164000e-06,0.66908456448000e-06,&
      &0.63912974741000e-06,0.61087868284000e-06,0.58424078801000e-06,0.55910164975000e-06,0.53534199822000e-06,&
      &0.51285815516000e-06,0.49155575101000e-06,0.47135981267000e-06,0.45221501316000e-06,0.43404352873000e-06,&
      &0.41677602521000e-06,0.40034842808000e-06,0.38470398098000e-06,0.36981272586000e-06,0.35562534856000e-06,&
      &0.34209596646000e-06,0.32918166792000e-06,0.31684208433000e-06,0.30505187475000e-06,0.29378745240000e-06,&
      &0.28301434741000e-06,0.27270380483000e-06,0.26283038782000e-06,0.25336976073000e-06,0.24430773072000e-06,&
      &0.23562400463000e-06,0.22729800388000e-06,0.21930945984000e-06,0.21164010517000e-06,0.20427914583000e-06,&
      &0.19721384061000e-06,0.19042769598000e-06,0.18390643987000e-06,0.17763628683000e-06,0.17160470919000e-06,&
      &0.16580470034000e-06,0.16022473358000e-06,0.15485182870000e-06,0.14967459970000e-06,0.14468363462000e-06,&
      &0.13987217484000e-06,0.13523070978000e-06,0.13075005656000e-06,0.12642121347000e-06,0.12223462135000e-06,&
      &0.11818345478000e-06,0.11426300988000e-06,0.11046551177000e-06,0.10678311501000e-06,0.10320883599000e-06,&
      &0.99736956007000e-07,0.96364540564000e-07,0.93085350382000e-07,0.89895373702000e-07,0.86790026724000e-07,&
      &0.83764611986000e-07,0.80818708975000e-07,0.77949690035000e-07,0.75155466294000e-07,0.72434961489000e-07,&
      &0.69787231343000e-07,0.67212432377000e-07,0.64712405330000e-07,0.62288253926000e-07,0.59941816899000e-07,&
      &0.57675641566000e-07,0.55492871208000e-07,0.53397890929000e-07,0.51398213416000e-07,0.49481175473000e-07,&
      &0.47709698815000e-07      /)

! surface burst - fig 2-15 - incident pressure pso/w**(1/3) : g,cm,mus,mbar
      pblast_data%pso_surf = (/&
      &0.69878901860000e-03,0.68682880765000e-03,0.67464370120000e-03,0.66225300193000e-03,0.64967954523000e-03,&
      &0.63695009127000e-03,0.62409575481000e-03,0.61115247589000e-03,0.59816153507000e-03,0.58517011740000e-03,&
      &0.57223192919000e-03,0.55940787242000e-03,0.54676672476000e-03,0.53434245542000e-03,0.52212773981000e-03,&
      &0.51011561977000e-03,0.49829753867000e-03,0.48666130290000e-03,0.47519835105000e-03,0.46390794069000e-03,&
      &0.45277965155000e-03,0.44180827449000e-03,0.43098915139000e-03,0.42031470303000e-03,0.40978431042000e-03,&
      &0.39939959892000e-03,0.38915738856000e-03,0.37905549035000e-03,0.36909437121000e-03,0.35927517343000e-03,&
      &0.34960163913000e-03,0.34007560232000e-03,0.33069950853000e-03,0.32147552010000e-03,0.31240527950000e-03,&
      &0.30349312749000e-03,0.29474277781000e-03,0.28615641290000e-03,0.27773702945000e-03,0.26948629609000e-03,&
      &0.26140515522000e-03,0.25350001131000e-03,0.24577087358000e-03,0.23821745733000e-03,0.23084052018000e-03,&
      &0.22364006397000e-03,0.21661888572000e-03,0.20977627965000e-03,0.20311036150000e-03,0.19661989806000e-03,&
      &0.19030304761000e-03,0.18415850539000e-03,0.17818503004000e-03,0.17238040708000e-03,0.16674132389000e-03,&
      &0.16126344818000e-03,0.15594373756000e-03,0.15078145239000e-03,0.14577239482000e-03,0.14091196922000e-03,&
      &0.13619655072000e-03,0.13162160793000e-03,0.12718325445000e-03,0.12287969738000e-03,0.11870672723000e-03,&
      &0.11465937893000e-03,0.11073255693000e-03,0.10692235450000e-03,0.10322820360000e-03,0.99644745386000e-04,&
      &0.96168074756000e-04,0.92794531113000e-04,0.89519576753000e-04,0.86342224737000e-04,0.83258693728000e-04,&
      &0.80265353674000e-04,0.77359702364000e-04,0.74538200651000e-04,0.71797558024000e-04,0.69137934070000e-04,&
      &0.66555825132000e-04,0.64047660280000e-04,0.61611659408000e-04,0.59246231269000e-04,0.56950075509000e-04,&
      &0.54720992675000e-04,0.52557432328000e-04,0.50457528595000e-04,0.48419139382000e-04,0.46441694334000e-04,&
      &0.44524504945000e-04,0.42665767790000e-04,0.40864156342000e-04,0.39118384133000e-04,0.37427131385000e-04,&
      &0.35790570470000e-04,0.34207424430000e-04,0.32676304139000e-04,0.31196403117000e-04,0.29766768326000e-04,&
      &0.28386569279000e-04,0.27055459500000e-04,0.25772355101000e-04,0.24536115017000e-04,0.23345721921000e-04,&
      &0.22200353651000e-04,0.21099585247000e-04,0.20042538961000e-04,0.19028073529000e-04,0.18054893019000e-04,&
      &0.17121960256000e-04,0.16229021506000e-04,0.15374943819000e-04,0.14558648149000e-04,0.13778925864000e-04,&
      &0.13034464464000e-04,0.12324524602000e-04,0.11648421016000e-04,0.11004845900000e-04,0.10392638058000e-04,&
      &0.98106202839000e-05,0.92575557154000e-05,0.87328119650000e-05,0.82353463870000e-05,0.77640350438000e-05,&
      &0.73176302125000e-05,0.68949133481000e-05,0.64951369162000e-05,0.61175332999000e-05,0.57609708566000e-05,&
      &0.54243590752000e-05,0.51066752060000e-05,0.48070031132000e-05,0.45247384043000e-05,0.42589574477000e-05,&
      &0.40087512774000e-05,0.37732376216000e-05,0.35515487388000e-05,0.33430965398000e-05,0.31472827070000e-05,&
      &0.29633208085000e-05,0.27904794225000e-05,0.26280678881000e-05,0.24754831484000e-05,0.23323486607000e-05,&
      &0.21980526300000e-05,0.20720179413000e-05,0.19537115054000e-05,0.18426255015000e-05,0.17384315726000e-05,&
      &0.16407364395000e-05,0.15490896670000e-05,0.14630827932000e-05,0.13823391397000e-05,0.13065435410000e-05,&
      &0.12354616910000e-05,0.11687705710000e-05,0.11061608254000e-05,0.10473467587000e-05,0.99207700625000e-06,&
      &0.94018557660000e-06,0.89146183989000e-06,0.84566686930000e-06,0.80260151651000e-06,0.76208655011000e-06,&
      &0.72395885345000e-06,0.68810689151000e-06,0.65436898647000e-06,0.62259093011000e-06,0.59263650043000e-06,&
      &0.56438798160000e-06,0.53776424554000e-06,0.51265721283000e-06,0.48896815422000e-06,0.46659531121000e-06,&
      &0.44544220804000e-06,0.42545330071000e-06,0.40656261921000e-06,0.38869633304000e-06,0.37178176080000e-06,&
      &0.35575352575000e-06,0.34056293993000e-06,0.32616967770000e-06,0.31252457707000e-06,0.29957484325000e-06,&
      &0.28727242631000e-06,0.27557958782000e-06,0.26447102825000e-06,0.25391295435000e-06,0.24386785083000e-06,&
      &0.23430389778000e-06,0.22519182918000e-06,0.21650707582000e-06,0.20823352360000e-06,0.20034441599000e-06,&
      &0.19281606829000e-06,0.18562680488000e-06,0.17875653180000e-06,0.17219533609000e-06,0.16592595927000e-06,&
      &0.15993059328000e-06,0.15419314172000e-06,0.14869900982000e-06,0.14343808664000e-06,0.13840093929000e-06,&
      &0.13357441467000e-06,0.12894682898000e-06,0.12450722590000e-06,0.12024562810000e-06,0.11615608584000e-06,&
      &0.11222994356000e-06,0.10845889365000e-06,0.10483369285000e-06,0.10134575513000e-06,0.97991196931000e-07,&
      &0.94763907986000e-07,0.91656745348000e-07,0.88663185528000e-07,0.85777029577000e-07,0.82993204081000e-07,&
      &0.80307931783000e-07,0.77716313075000e-07,0.75213144136000e-07,0.72792870560000e-07,0.70451071952000e-07,&
      &0.68186144503000e-07,0.65993424655000e-07,0.63868797380000e-07,0.61808909223000e-07,0.59810001033000e-07,&
      &0.57869259516000e-07,0.55984852474000e-07,0.54153853068000e-07,0.52373360194000e-07,0.50640843421000e-07,&
      &0.48954261058000e-07,0.47312489007000e-07,0.45713761419000e-07,0.44156507075000e-07,0.42639335571000e-07,&
      &0.41160969559000e-07,0.39720946288000e-07,0.38318992835000e-07,0.36954579865000e-07,0.35627333159000e-07,&
      &0.34337123943000e-07,0.33084272603000e-07,0.31869579306000e-07,0.30693550319000e-07,0.29556917800000e-07,&
      &0.28460554740000e-07,0.27405474912000e-07,0.26393270827000e-07,0.25427389928000e-07,0.24498628791000e-07,&
      &0.23644141466000e-07      /)

! surface burst - fig 2-15 - characteristic time t0/w**(1/3) : g,cm,mus,mbar
      pblast_data%t0_surf = (/&
      &0.36768976143000e+01,0.48224540840000e+01,0.59664703043000e+01,0.71075979284000e+01,0.82444162323000e+01,&
      &0.93754309912000e+01,0.10499073595000e+02,0.11613700441000e+02,0.12717592653000e+02,0.13808956168000e+02,&
      &0.14885922247000e+02,0.15946548474000e+02,0.16988820301000e+02,0.18010653215000e+02,0.19009895611000e+02,&
      &0.19984332450000e+02,0.20931689808000e+02,0.21849640417000e+02,0.22735810316000e+02,0.23587786746000e+02,&
      &0.24403127425000e+02,0.25179371359000e+02,0.25914051358000e+02,0.26604708461000e+02,0.27248908440000e+02,&
      &0.27844260647000e+02,0.28388439417000e+02,0.28879208322000e+02,0.29314447550000e+02,0.29692184740000e+02,&
      &0.30010629629000e+02,0.30268212882000e+02,0.30463629536000e+02,0.30595887517000e+02,0.30664361716000e+02,&
      &0.30668854186000e+02,0.30609661050000e+02,0.30487646762000e+02,0.30304326437000e+02,0.30061957028000e+02,&
      &0.29763638169000e+02,0.29413423621000e+02,0.29016444315000e+02,0.28579044070000e+02,0.28108929176000e+02,&
      &0.27615333132000e+02,0.27109197934000e+02,0.26603373448000e+02,0.26112836516000e+02,0.25654931611000e+02,&
      &0.25247563298000e+02,0.24891133726000e+02,0.24576489545000e+02,0.24297477331000e+02,0.24048685172000e+02,&
      &0.23825671823000e+02,0.23624578901000e+02,0.23442201393000e+02,0.23276073972000e+02,0.23124106260000e+02,&
      &0.22984649740000e+02,0.22856789153000e+02,0.22739847085000e+02,0.22633585754000e+02,0.22537869251000e+02,&
      &0.22453266803000e+02,0.22380318564000e+02,0.22320168123000e+02,0.22273867757000e+02,0.22242811571000e+02,&
      &0.22228815721000e+02,0.22233884030000e+02,0.22259958022000e+02,0.22309311567000e+02,0.22384603163000e+02,&
      &0.22488573002000e+02,0.22624211532000e+02,0.22794831642000e+02,0.23003890899000e+02,0.23255296254000e+02,&
      &0.23553295362000e+02,0.23902382484000e+02,0.24307740386000e+02,0.24774837436000e+02,0.25309821447000e+02,&
      &0.25919717480000e+02,0.26612045728000e+02,0.27395214993000e+02,0.28278547458000e+02,0.29272943081000e+02,&
      &0.30389942765000e+02,0.31643173134000e+02,0.33047629954000e+02,0.34619897575000e+02,0.36378861860000e+02,&
      &0.38346400841000e+02,0.40545760298000e+02,0.43004558553000e+02,0.45751949147000e+02,0.48822470485000e+02,&
      &0.52253463920000e+02,0.56086469901000e+02,0.60367704986000e+02,0.65148281966000e+02,0.70484126419000e+02,&
      &0.76436529539000e+02,0.83071020104000e+02,0.90456895088000e+02,0.98666577414000e+02,0.10777655923000e+03,&
      &0.11786517305000e+03,0.12900529408000e+03,0.14127808506000e+03,0.15473970396000e+03,0.16897425976000e+03,&
      &0.18073825850000e+03,0.19153613121000e+03,0.19931622313000e+03,0.20527490852000e+03,0.20991431134000e+03,&
      &0.21360765861000e+03,0.21655517289000e+03,0.21883997756000e+03,0.22048085535000e+03,0.22146633621000e+03,&
      &0.22179347685000e+03,0.22147923699000e+03,0.22057878228000e+03,0.21917582816000e+03,0.21738593669000e+03,&
      &0.21533269041000e+03,0.21319091062000e+03,0.21104291514000e+03,0.20903998728000e+03,0.20727659755000e+03,&
      &0.20582818576000e+03,0.20474744101000e+03,0.20406047880000e+03,0.20377548916000e+03,0.20388488786000e+03,&
      &0.20437125529000e+03,0.20521976221000e+03,0.20642370556000e+03,0.20799109768000e+03,0.20995671089000e+03,&
      &0.21238669277000e+03,0.21537577221000e+03,0.21905100461000e+03,0.22355580129000e+03,0.22902244708000e+03,&
      &0.23553083390000e+03,0.24305750893000e+03,0.25134035723000e+03,0.25956977269000e+03,0.26864330830000e+03,&
      &0.27555007360000e+03,0.28186154131000e+03,0.28807878631000e+03,0.29412586524000e+03,0.30001007423000e+03,&
      &0.30573368476000e+03,0.31130059605000e+03,0.31671491523000e+03,0.32198133087000e+03,0.32710483893000e+03,&
      &0.33209180395000e+03,0.33694783803000e+03,0.34167926558000e+03,0.34629322789000e+03,0.35079665274000e+03,&
      &0.35519657226000e+03,0.35949845392000e+03,0.36371238369000e+03,0.36784507824000e+03,0.37190246137000e+03,&
      &0.37589151195000e+03,0.37982165151000e+03,0.38369595251000e+03,0.38752557789000e+03,0.39131357226000e+03,&
      &0.39506674797000e+03,0.39879071915000e+03,0.40249296802000e+03,0.40617938619000e+03,0.40985300705000e+03,&
      &0.41351928566000e+03,0.41718568314000e+03,0.42085435586000e+03,0.42452854303000e+03,0.42821302977000e+03,&
      &0.43191215433000e+03,0.43562820998000e+03,0.43936508528000e+03,0.44312532180000e+03,0.44691005465000e+03,&
      &0.45072215828000e+03,0.45456340287000e+03,0.45843510906000e+03,0.46233852428000e+03,0.46627515929000e+03,&
      &0.47024485269000e+03,0.47424817174000e+03,0.47828430189000e+03,0.48235543061000e+03,0.48645963756000e+03,&
      &0.49059765998000e+03,0.49476607216000e+03,0.49896722571000e+03,0.50319601366000e+03,0.50745389224000e+03,&
      &0.51174028091000e+03,0.51605037638000e+03,0.52038321766000e+03,0.52473824316000e+03,0.52911136342000e+03,&
      &0.53350158779000e+03,0.53790612930000e+03,0.54232318737000e+03,0.54675039360000e+03,0.55118472627000e+03,&
      &0.55562323696000e+03,0.56006489202000e+03,0.56450785445000e+03,0.56894792999000e+03,0.57338436420000e+03,&
      &0.57781602875000e+03,0.58223974846000e+03,0.58665460841000e+03,0.59105885704000e+03,0.59545141366000e+03,&
      &0.59983287445000e+03,0.60420219885000e+03,0.60856064830000e+03,0.61290572798000e+03,0.61724131319000e+03,&
      &0.62156703580000e+03,0.62588582562000e+03,0.63019939232000e+03,0.63451265094000e+03,0.63882799339000e+03,&
      &0.64315113154000e+03,0.64748522304000e+03,0.65183942773000e+03,0.65621781893000e+03,0.66063040905000e+03,&
      &0.66508547970000e+03,0.66959352309000e+03,0.67416367417000e+03,0.67881002342000e+03,0.68354551986000e+03,&
      &0.68838190211000e+03,0.69334106061000e+03,0.69843475298000e+03,0.70368407830000e+03,0.70911299625000e+03,&
      &0.71469273734000e+03      /)



! surface burst - fig 2-15 - arrival time ta/w**(1/3) : g,cm,mus,mbar
      pblast_data%ta_surf = (/&
      &0.84868904252000e+00,0.86381654354000e+00,0.87974759865000e+00,0.89649661948000e+00,0.91407569092000e+00,&
      &0.93249417227000e+00,0.95175825413000e+00,0.97187046648000e+00,0.99282913363000e+00,0.10146277706000e+01,&
      &0.10372544158000e+01,0.10606908934000e+01,0.10849120725000e+01,0.11099385280000e+01,0.11358437715000e+01,&
      &0.11627002940000e+01,0.11905800087000e+01,0.12195626838000e+01,0.12497203081000e+01,0.12811143075000e+01,&
      &0.13138295464000e+01,0.13479362715000e+01,0.13835030999000e+01,0.14206171095000e+01,0.14593552241000e+01,&
      &0.14997820367000e+01,0.15419914515000e+01,0.15860618633000e+01,0.16320635347000e+01,0.16800968002000e+01,&
      &0.17302401102000e+01,0.17825939239000e+01,0.18372369137000e+01,0.18942615930000e+01,0.19537832838000e+01,&
      &0.20158832687000e+01,0.20806819613000e+01,0.21482894052000e+01,0.22187980977000e+01,0.22923261781000e+01,&
      &0.23690157658000e+01,0.24489722076000e+01,0.25323212209000e+01,0.26192062362000e+01,0.27097743881000e+01,&
      &0.28041654532000e+01,0.29025136737000e+01,0.30049859550000e+01,0.31117633214000e+01,0.32230097764000e+01,&
      &0.33388966366000e+01,0.34596274926000e+01,0.35853870302000e+01,0.37163792899000e+01,0.38528388951000e+01,&
      &0.39949886224000e+01,0.41430464462000e+01,0.42972935483000e+01,0.44579771246000e+01,0.46253716866000e+01,&
      &0.47997796254000e+01,0.49815092040000e+01,0.51708877097000e+01,0.53682589896000e+01,0.55739672667000e+01,&
      &0.57883948413000e+01,0.60119631601000e+01,0.62450916366000e+01,0.64882112926000e+01,0.67418187678000e+01,&
      &0.70063874201000e+01,0.72824519349000e+01,0.75706187162000e+01,0.78714028804000e+01,0.81854660893000e+01,&
      &0.85134768821000e+01,0.88561064305000e+01,0.92141108425000e+01,0.95882932208000e+01,0.99793859624000e+01,&
      &0.10388297476000e+02,0.10816083599000e+02,0.11263665021000e+02,0.11731925254000e+02,0.12222026276000e+02,&
      &0.12735218769000e+02,0.13272654738000e+02,0.13835553265000e+02,0.14425402619000e+02,0.15043687169000e+02,&
      &0.15691666460000e+02,0.16371094065000e+02,0.17083809600000e+02,0.17831526337000e+02,0.18616145851000e+02,&
      &0.19439607228000e+02,0.20303986693000e+02,0.21211771285000e+02,0.22165210462000e+02,0.23166525566000e+02,&
      &0.24218696336000e+02,0.25324345417000e+02,0.26486355135000e+02,0.27707858176000e+02,0.28992131671000e+02,&
      &0.30342581737000e+02,0.31762891543000e+02,0.33256745977000e+02,0.34828012083000e+02,0.36481155848000e+02,&
      &0.38220741508000e+02,0.40050816221000e+02,0.41976593537000e+02,0.44003128508000e+02,0.46135838528000e+02,&
      &0.48380613359000e+02,0.50742982354000e+02,0.53228804658000e+02,0.55845249350000e+02,0.58598836025000e+02,&
      &0.61496371563000e+02,0.64545964983000e+02,0.67754510332000e+02,0.71130638216000e+02,0.74682475649000e+02,&
      &0.78418528435000e+02,0.82348581682000e+02,0.86482115512000e+02,0.90828205031000e+02,0.95397511160000e+02,&
      &0.10020104845000e+03,0.10524943548000e+03,0.11055397868000e+03,0.11612668007000e+03,0.12198009348000e+03,&
      &0.12812661290000e+03,0.13457914521000e+03,0.14135110897000e+03,0.14845558817000e+03,0.15590818740000e+03,&
      &0.16372341779000e+03,0.17191521754000e+03,0.18049944579000e+03,0.18949277938000e+03,0.19891028349000e+03,&
      &0.20876931217000e+03,0.21908527962000e+03,0.22987571873000e+03,0.24116017075000e+03,0.25295357052000e+03,&
      &0.26527688360000e+03,0.27814727254000e+03,0.29158197116000e+03,0.30560106881000e+03,0.32022348657000e+03,&
      &0.33546863389000e+03,0.35135645597000e+03,0.36790696888000e+03,0.38513920154000e+03,0.40307242110000e+03,&
      &0.42172813307000e+03,0.44112713161000e+03,0.46128925824000e+03,0.48223470245000e+03,0.50398391847000e+03,&
      &0.52655753494000e+03,0.54997704517000e+03,0.57426210836000e+03,0.59943524010000e+03,0.62551767548000e+03,&
      &0.65252879392000e+03,0.68049469698000e+03,0.70943869900000e+03,0.73937063650000e+03,0.77031576319000e+03,&
      &0.80230706821000e+03,0.83535628418000e+03,0.86949293029000e+03,0.90473327225000e+03,0.94110482597000e+03,&
      &0.97863527856000e+03,0.10173389416000e+04,0.10572513628000e+04,0.10983874751000e+04,0.11407799228000e+04,&
      &0.11844658264000e+04,0.12294631017000e+04,0.12757936351000e+04,0.13234894612000e+04,0.13725934654000e+04,&
      &0.14231336010000e+04,0.14751397082000e+04,0.15286583002000e+04,0.15836982377000e+04,0.16403200805000e+04,&
      &0.16985645363000e+04,0.17584587711000e+04,0.18200558772000e+04,0.18833921901000e+04,0.19485185682000e+04,&
      &0.20154843926000e+04,0.20843402403000e+04,0.21551409379000e+04,0.22279397371000e+04,0.23027834943000e+04,&
      &0.23797518223000e+04,0.24588979241000e+04,0.25402712839000e+04,0.26239597777000e+04,0.27100169482000e+04,&
      &0.27985209088000e+04,0.28895487687000e+04,0.29831699295000e+04,0.30794619617000e+04,0.31785104780000e+04,&
      &0.32803956435000e+04,0.33852007217000e+04,0.34930040817000e+04,0.36039015484000e+04,0.37180030902000e+04,&
      &0.38353727773000e+04,0.39561061591000e+04,0.40803150292000e+04,0.42080874131000e+04,0.43395035529000e+04,&
      &0.44746689224000e+04,0.46137044553000e+04,0.47566882655000e+04,0.49037082623000e+04,0.50548739805000e+04,&
      &0.52102761945000e+04,0.53700322421000e+04,0.55342209290000e+04,0.57029484907000e+04,0.58762997358000e+04,&
      &0.60543490988000e+04,0.62372252637000e+04,0.64250208953000e+04,0.66177950137000e+04,0.68156480439000e+04,&
      &0.70186850608000e+04,0.72269706935000e+04,0.74406108697000e+04,0.76597025687000e+04,0.78843397371000e+04,&
      &0.81146124342000e+04,0.83506172356000e+04,0.85924732999000e+04,0.88403035889000e+04,0.90942346946000e+04,&
      &0.93544102532000e+04,0.96210015789000e+04,0.98941933557000e+04,0.10174149696000e+05,0.10461334312000e+05,&
      &0.10755199332000e+05      /)

!   figure 2_9 - pra(curve_1, angle=0..85.0) mbar
      pblast_data%pra(1,1:256)= (/&
      &0.27980418453000e-02,0.28232083307000e-02,0.28467431325000e-02,0.28686681324000e-02,0.28890084644000e-02,&
      &0.29077789976000e-02,0.29250091522000e-02,0.29407244626000e-02,0.29549616545000e-02,0.29677631019000e-02,&
      &0.29791608576000e-02,0.29891920391000e-02,0.29978988404000e-02,0.30053303349000e-02,0.30115397243000e-02,&
      &0.30165614227000e-02,0.30204386179000e-02,0.30232296734000e-02,0.30249891667000e-02,0.30257569024000e-02,&
      &0.30255793158000e-02,0.30245057529000e-02,0.30225927869000e-02,0.30198951809000e-02,0.30164399545000e-02,&
      &0.30122816645000e-02,0.30074747414000e-02,0.30020631853000e-02,0.29960883443000e-02,0.29895845796000e-02,&
      &0.29825929480000e-02,0.29751669582000e-02,0.29673521811000e-02,0.29591676468000e-02,0.29506547238000e-02,&
      &0.29418489213000e-02,0.29327838942000e-02,0.29234954411000e-02,0.29140074778000e-02,0.29043449878000e-02,&
      &0.28945462408000e-02,0.28846364239000e-02,0.28746204171000e-02,0.28645399616000e-02,0.28544120145000e-02,&
      &0.28442523244000e-02,0.28340748587000e-02,0.28239089811000e-02,0.28137551091000e-02,0.28036271714000e-02,&
      &0.27935495251000e-02,0.27835244043000e-02,0.27735567995000e-02,0.27636630620000e-02,0.27538514667000e-02,&
      &0.27441180645000e-02,0.27344732546000e-02,0.27249214951000e-02,0.27154596879000e-02,0.27060879441000e-02,&
      &0.26968112914000e-02,0.26876306543000e-02,0.26785343152000e-02,0.26695228588000e-02,0.26606001716000e-02,&
      &0.26517524224000e-02,0.26429712207000e-02,0.26342587094000e-02,0.26256034179000e-02,0.26169877534000e-02,&
      &0.26084208154000e-02,0.25998827211000e-02,0.25913595900000e-02,0.25828450363000e-02,0.25743270439000e-02,&
      &0.25657960085000e-02,0.25572345927000e-02,0.25486300045000e-02,0.25399698135000e-02,0.25312424337000e-02,&
      &0.25224292324000e-02,0.25135173582000e-02,0.25044935412000e-02,0.24953343583000e-02,0.24860325619000e-02,&
      &0.24765747097000e-02,0.24669395038000e-02,0.24571083832000e-02,0.24470705369000e-02,0.24368107490000e-02,&
      &0.24263139625000e-02,0.24155626544000e-02,0.24045397549000e-02,0.23932311756000e-02,0.23816220800000e-02,&
      &0.23697005028000e-02,0.23574512983000e-02,0.23448570472000e-02,0.23319127751000e-02,0.23186035308000e-02,&
      &0.23049107841000e-02,0.22908307163000e-02,0.22763534945000e-02,0.22614640364000e-02,0.22461541478000e-02,&
      &0.22304198153000e-02,0.22142517311000e-02,0.21976469323000e-02,0.21805991323000e-02,0.21631009039000e-02,&
      &0.21451508613000e-02,0.21267497336000e-02,0.21078959826000e-02,0.20885915684000e-02,0.20688378724000e-02,&
      &0.20486303431000e-02,0.20279796267000e-02,0.20068929003000e-02,0.19853715201000e-02,0.19634237800000e-02,&
      &0.19410621331000e-02,0.19182985832000e-02,0.18951374745000e-02,0.18715903258000e-02,0.18476761507000e-02,&
      &0.18234101088000e-02,0.17988031723000e-02,0.17738718788000e-02,0.17486390162000e-02,0.17231156048000e-02,&
      &0.16973255970000e-02,0.16712883699000e-02,0.16450236596000e-02,0.16185545046000e-02,0.15918974641000e-02,&
      &0.15650757682000e-02,0.15381168614000e-02,0.15110457123000e-02,0.14838773602000e-02,0.14566357029000e-02,&
      &0.14293475101000e-02,0.14020418711000e-02,0.13747393614000e-02,0.13474547632000e-02,0.13202145502000e-02,&
      &0.12930511911000e-02,0.12659802633000e-02,0.12390240277000e-02,0.12122041799000e-02,0.11855423001000e-02,&
      &0.11590597445000e-02,0.11327771334000e-02,0.11067110772000e-02,0.10808837232000e-02,0.10553132022000e-02,&
      &0.10300149078000e-02,0.10050024238000e-02,0.98029238470000e-03,0.95590269532000e-03,0.93184465271000e-03,&
      &0.90812764518000e-03,0.88476717218000e-03,0.86177332831000e-03,0.83915458606000e-03,0.81692118164000e-03,&
      &0.79507802821000e-03,0.77363294871000e-03,0.75259363392000e-03,0.73196344843000e-03,0.71174489333000e-03,&
      &0.69194345749000e-03,0.67255984788000e-03,0.65359591747000e-03,0.63505326854000e-03,0.61692887675000e-03,&
      &0.59922614798000e-03,0.58193913206000e-03,0.56506477037000e-03,0.54860494577000e-03,0.53255318294000e-03,&
      &0.51690459550000e-03,0.50165600329000e-03,0.48680106791000e-03,0.47233141137000e-03,0.45824514145000e-03,&
      &0.44453669924000e-03,0.43119547180000e-03,0.41821242127000e-03,0.40558503033000e-03,0.39330359109000e-03,&
      &0.38135837651000e-03,0.36974201386000e-03,0.35844657259000e-03,0.34746340715000e-03,0.33678421505000e-03,&
      &0.32639919715000e-03,0.31629789705000e-03,0.30647514713000e-03,0.29691966418000e-03,0.28762208559000e-03,&
      &0.27857439691000e-03,0.26976676403000e-03,0.26119146466000e-03,0.25284083635000e-03,0.24470537708000e-03,&
      &0.23677382569000e-03,0.22903862329000e-03,0.22149581989000e-03,0.21413471428000e-03,0.20694539587000e-03,&
      &0.19992285356000e-03,0.19306056708000e-03,0.18635054248000e-03,0.17978494989000e-03,0.17335734319000e-03,&
      &0.16706311933000e-03,0.16089647494000e-03,0.15485150368000e-03,0.14892334784000e-03,0.14310731269000e-03,&
      &0.13739959846000e-03,0.13179751738000e-03,0.12629669605000e-03,0.12089384489000e-03,0.11558894376000e-03,&
      &0.11037962951000e-03,0.10526454318000e-03,0.10024327891000e-03,0.95315896304000e-04,0.90483101561000e-04,&
      &0.85746586301000e-04,0.81107921044000e-04,0.76568990514000e-04,0.72132916038000e-04,0.67803344590000e-04,&
      &0.63582702296000e-04,0.59475192341000e-04,0.55486736225000e-04,0.51619377708000e-04,0.47879402788000e-04,&
      &0.44271180895000e-04,0.40798799369000e-04,0.37468401225000e-04,0.34282460311000e-04,0.31245217765000e-04,&
      &0.28360702721000e-04,0.25631929858000e-04,0.23060769729000e-04,0.20648433777000e-04,0.18396246387000e-04,&
      &0.16303943856000e-04,0.14369423584000e-04,0.12591467215000e-04,0.10969362838000e-04,0.94880376934000e-05,&
      &0.81601897766000e-05      /)

      pblast_data%pra(2,1:256)= (/&
      &0.12514189348000e-02,0.12502386832000e-02,0.12490372748000e-02,0.12478137293000e-02,0.12465672698000e-02,&
      &0.12452981989000e-02,0.12440057852000e-02,0.12426913926000e-02,0.12413537844000e-02,0.12399907684000e-02,&
      &0.12386032380000e-02,0.12371945244000e-02,0.12357592733000e-02,0.12342956360000e-02,0.12328085066000e-02,&
      &0.12312944372000e-02,0.12297530731000e-02,0.12281842925000e-02,0.12265868322000e-02,0.12249594077000e-02,&
      &0.12233020574000e-02,0.12216153865000e-02,0.12198970054000e-02,0.12181445503000e-02,0.12163602193000e-02,&
      &0.12145413200000e-02,0.12126854868000e-02,0.12107934312000e-02,0.12088652272000e-02,0.12068985860000e-02,&
      &0.12048913573000e-02,0.12028435946000e-02,0.12007540260000e-02,0.11986184956000e-02,0.11964406364000e-02,&
      &0.11942187926000e-02,0.11919474737000e-02,0.11896260770000e-02,0.11872549180000e-02,0.11848329500000e-02,&
      &0.11823587600000e-02,0.11798297032000e-02,0.11772422336000e-02,0.11745988479000e-02,0.11718957669000e-02,&
      &0.11691297658000e-02,0.11663026779000e-02,0.11634115339000e-02,0.11604553978000e-02,0.11574317609000e-02,&
      &0.11543372533000e-02,0.11511708701000e-02,0.11479328923000e-02,0.11446226701000e-02,0.11412357425000e-02,&
      &0.11377693082000e-02,0.11342272569000e-02,0.11306023985000e-02,0.11268931191000e-02,0.11231026683000e-02,&
      &0.11192275813000e-02,0.11152625672000e-02,0.11112096704000e-02,0.11070693759000e-02,0.11028361474000e-02,&
      &0.10985091156000e-02,0.10940873748000e-02,0.10895705008000e-02,0.10849564953000e-02,0.10802433645000e-02,&
      &0.10754338961000e-02,0.10705227760000e-02,0.10655082840000e-02,0.10603935853000e-02,0.10551761557000e-02,&
      &0.10498531300000e-02,0.10444248216000e-02,0.10388909396000e-02,0.10332491860000e-02,0.10275026269000e-02,&
      &0.10216500045000e-02,0.10156886210000e-02,0.10096185146000e-02,0.10034405541000e-02,0.99715503090000e-03,&
      &0.99076052654000e-03,0.98425807340000e-03,0.97764969275000e-03,0.97093326831000e-03,0.96410739983000e-03,&
      &0.95717635556000e-03,0.95014234676000e-03,0.94300180396000e-03,0.93575491756000e-03,0.92840490825000e-03,&
      &0.92095426302000e-03,0.91340287382000e-03,0.90575148574000e-03,0.89800063823000e-03,0.89015355413000e-03,&
      &0.88221276412000e-03,0.87417834397000e-03,0.86605182496000e-03,0.85783661356000e-03,0.84953398286000e-03,&
      &0.84114499596000e-03,0.83267339229000e-03,0.82412179872000e-03,0.81549153223000e-03,0.80678526558000e-03,&
      &0.79800695769000e-03,0.78915861963000e-03,0.78024264906000e-03,0.77126161338000e-03,0.76221933258000e-03,&
      &0.75311953154000e-03,0.74396324310000e-03,0.73475507281000e-03,0.72549923609000e-03,0.71619761996000e-03,&
      &0.70685471196000e-03,0.69747191018000e-03,0.68805381951000e-03,0.67860523852000e-03,0.66912657930000e-03,&
      &0.65962543811000e-03,0.65010417783000e-03,0.64056404524000e-03,0.63101021385000e-03,0.62144779892000e-03,&
      &0.61187788001000e-03,0.60230663953000e-03,0.59273550571000e-03,0.58317006006000e-03,0.57361304949000e-03,&
      &0.56406749558000e-03,0.55453706854000e-03,0.54502737870000e-03,0.53554087161000e-03,0.52607986709000e-03,&
      &0.51664833325000e-03,0.50725097893000e-03,0.49788990505000e-03,0.48856875553000e-03,0.47929148987000e-03,&
      &0.47006086772000e-03,0.46087985237000e-03,0.45175169967000e-03,0.44267891132000e-03,0.43366496498000e-03,&
      &0.42471345659000e-03,0.41582658101000e-03,0.40700657672000e-03,0.39825600145000e-03,0.38957819282000e-03,&
      &0.38097612128000e-03,0.37245065131000e-03,0.36400460314000e-03,0.35564088238000e-03,0.34736127580000e-03,&
      &0.33916747917000e-03,0.33106171283000e-03,0.32304565550000e-03,0.31512116238000e-03,0.30729042975000e-03,&
      &0.29955458703000e-03,0.29191490207000e-03,0.28437282185000e-03,0.27692994550000e-03,0.26958763965000e-03,&
      &0.26234650585000e-03,0.25520784462000e-03,0.24817299538000e-03,0.24124270736000e-03,0.23441734844000e-03,&
      &0.22769827685000e-03,0.22108540083000e-03,0.21457944015000e-03,0.20818199612000e-03,0.20189241077000e-03,&
      &0.19571102416000e-03,0.18963864890000e-03,0.18367547473000e-03,0.17782114643000e-03,0.17207616658000e-03,&
      &0.16644051518000e-03,0.16091406566000e-03,0.15549692360000e-03,0.15018898887000e-03,0.14498996633000e-03,&
      &0.13989947154000e-03,0.13491743407000e-03,0.13004375180000e-03,0.12527771097000e-03,0.12061873968000e-03,&
      &0.11606668906000e-03,0.11162118987000e-03,0.10728160992000e-03,0.10304724222000e-03,0.98917566115000e-04,&
      &0.94891894924000e-04,0.90969390267000e-04,0.87150137498000e-04,0.83432869969000e-04,0.79816475889000e-04,&
      &0.76300479458000e-04,0.72884099335000e-04,0.69566644552000e-04,0.66347056028000e-04,0.63224264517000e-04,&
      &0.60197382368000e-04,0.57265679553000e-04,0.54428075774000e-04,0.51683416383000e-04,0.49030548535000e-04,&
      &0.46468468971000e-04,0.43996238901000e-04,0.41612548194000e-04,0.39316079157000e-04,0.37105567728000e-04,&
      &0.34979919984000e-04,0.32937950948000e-04,0.30978103722000e-04,0.29098803925000e-04,0.27298924050000e-04,&
      &0.25576997610000e-04,0.23931567935000e-04,0.22360987196000e-04,0.20863368620000e-04,0.19437575420000e-04,&
      &0.18081952596000e-04,0.16794672704000e-04,0.15573849922000e-04,0.14417721958000e-04,0.13324861204000e-04,&
      &0.12293340991000e-04,0.11321104222000e-04,0.10406086418000e-04,0.95467825780000e-05,0.87412954105000e-05,&
      &0.79875110178000e-05,0.72833001397000e-05,0.66266548093000e-05,0.60160379288000e-05,0.54492835034000e-05,&
      &0.49242430571000e-05,0.44388018659000e-05,0.39912403055000e-05,0.35797577134000e-05,0.32022507611000e-05,&
      &0.28566389363000e-05,0.25410038208000e-05,0.22538845883000e-05,0.19941251767000e-05,0.17559051001000e-05,&
      &0.15446427355000e-05      /)

      pblast_data%pra(3,1:256)= (/&
      &0.78322749910000e-03,0.78122802881000e-03,0.77925118404000e-03,0.77729733776000e-03,0.77536323245000e-03,&
      &0.77344447225000e-03,0.77153939642000e-03,0.76964497731000e-03,0.76775945180000e-03,0.76587878374000e-03,&
      &0.76399991332000e-03,0.76212042302000e-03,0.76023843426000e-03,0.75835018866000e-03,0.75645637461000e-03,&
      &0.75455062818000e-03,0.75263166392000e-03,0.75069807064000e-03,0.74874578707000e-03,0.74677264475000e-03,&
      &0.74477791764000e-03,0.74275923926000e-03,0.74071336724000e-03,0.73863834637000e-03,0.73653395178000e-03,&
      &0.73439413532000e-03,0.73221898552000e-03,0.73000962520000e-03,0.72776032586000e-03,0.72546886595000e-03,&
      &0.72313526972000e-03,0.72075825752000e-03,0.71833459917000e-03,0.71586401454000e-03,0.71334566131000e-03,&
      &0.71077679979000e-03,0.70815481604000e-03,0.70547946169000e-03,0.70275105217000e-03,0.69996816413000e-03,&
      &0.69712910485000e-03,0.69423223182000e-03,0.69127551602000e-03,0.68826031241000e-03,0.68518511990000e-03,&
      &0.68204848890000e-03,0.67885118119000e-03,0.67559204161000e-03,0.67227015528000e-03,0.66888538639000e-03,&
      &0.66543742815000e-03,0.66192543575000e-03,0.65835072860000e-03,0.65471234302000e-03,0.65100954484000e-03,&
      &0.64724330267000e-03,0.64341297417000e-03,0.63952058049000e-03,0.63556536946000e-03,0.63154637242000e-03,&
      &0.62746566085000e-03,0.62332366495000e-03,0.61911966626000e-03,0.61485528494000e-03,0.61053251959000e-03,&
      &0.60614969820000e-03,0.60170942482000e-03,0.59721243052000e-03,0.59265889827000e-03,0.58805101220000e-03,&
      &0.58338984155000e-03,0.57867550243000e-03,0.57390926263000e-03,0.56909344695000e-03,0.56422887418000e-03,&
      &0.55931714191000e-03,0.55436030923000e-03,0.54935929742000e-03,0.54431494778000e-03,0.53922953024000e-03,&
      &0.53410548824000e-03,0.52894356401000e-03,0.52374503418000e-03,0.51851251707000e-03,0.51324787156000e-03,&
      &0.50795260230000e-03,0.50262835143000e-03,0.49727729166000e-03,0.49190099958000e-03,0.48650119870000e-03,&
      &0.48108027304000e-03,0.47564040858000e-03,0.47018291805000e-03,0.46470943901000e-03,0.45922258597000e-03,&
      &0.45372423614000e-03,0.44821607887000e-03,0.44270002879000e-03,0.43717811768000e-03,0.43165157165000e-03,&
      &0.42612325285000e-03,0.42059503865000e-03,0.41506833046000e-03,0.40954502192000e-03,0.40402697559000e-03,&
      &0.39851590482000e-03,0.39301355232000e-03,0.38752177337000e-03,0.38204213631000e-03,0.37657645862000e-03,&
      &0.37112690748000e-03,0.36569429161000e-03,0.36028031923000e-03,0.35488680091000e-03,0.34951525684000e-03,&
      &0.34416693055000e-03,0.33884346574000e-03,0.33354624504000e-03,0.32827654125000e-03,0.32303585568000e-03,&
      &0.31782488630000e-03,0.31264534045000e-03,0.30749844719000e-03,0.30238503314000e-03,0.29730634651000e-03,&
      &0.29226359391000e-03,0.28725756487000e-03,0.28228919742000e-03,0.27735973752000e-03,0.27246929371000e-03,&
      &0.26761940473000e-03,0.26281092805000e-03,0.25804420246000e-03,0.25332031656000e-03,0.24863937416000e-03,&
      &0.24400196622000e-03,0.23940914261000e-03,0.23486151671000e-03,0.23035903239000e-03,0.22590231754000e-03,&
      &0.22149183010000e-03,0.21712800505000e-03,0.21281143701000e-03,0.20854200270000e-03,0.20431961704000e-03,&
      &0.20014500681000e-03,0.19601876090000e-03,0.19194035936000e-03,0.18791033218000e-03,0.18392870116000e-03,&
      &0.17999523557000e-03,0.17611022357000e-03,0.17227373577000e-03,0.16848580168000e-03,0.16474630812000e-03,&
      &0.16105515966000e-03,0.15741235634000e-03,0.15381777182000e-03,0.15027127467000e-03,0.14677277295000e-03,&
      &0.14332215148000e-03,0.13991935268000e-03,0.13656403658000e-03,0.13325599424000e-03,0.12999522176000e-03,&
      &0.12678160401000e-03,0.12361457585000e-03,0.12049397988000e-03,0.11741973741000e-03,0.11439147977000e-03,&
      &0.11140930410000e-03,0.10847269858000e-03,0.10558138558000e-03,0.10273526017000e-03,0.99933900544000e-04,&
      &0.97177260343000e-04,0.94465068835000e-04,0.91796885826000e-04,0.89172487304000e-04,0.86591970104000e-04,&
      &0.84054857626000e-04,0.81560908988000e-04,0.79109991558000e-04,0.76701689552000e-04,0.74335980411000e-04,&
      &0.72012662881000e-04,0.69731468199000e-04,0.67492173926000e-04,0.65294569297000e-04,0.63138557854000e-04,&
      &0.61023941400000e-04,0.58950471605000e-04,0.56918019775000e-04,0.54926477745000e-04,0.52975612698000e-04,&
      &0.51065275299000e-04,0.49195374517000e-04,0.47365746928000e-04,0.45576247751000e-04,0.43826710311000e-04,&
      &0.42116971962000e-04,0.40446902832000e-04,0.38816435424000e-04,0.37225421609000e-04,0.35673694043000e-04,&
      &0.34161088038000e-04,0.32687454525000e-04,0.31252693965000e-04,0.29856617232000e-04,0.28499010414000e-04,&
      &0.27179703289000e-04,0.25898546905000e-04,0.24655317310000e-04,0.23449801773000e-04,0.22281763779000e-04,&
      &0.21150948252000e-04,0.20057133270000e-04,0.18999982446000e-04,0.17979176400000e-04,0.16994401211000e-04,&
      &0.16045340827000e-04,0.15131635205000e-04,0.14252833325000e-04,0.13408472775000e-04,0.12598177987000e-04,&
      &0.11821501517000e-04,0.11077901617000e-04,0.10366821159000e-04,0.96876476777000e-05,0.90399085919000e-05,&
      &0.84229861986000e-05,0.78362104246000e-05,0.72788604209000e-05,0.67502275466000e-05,0.62497298983000e-05,&
      &0.57765975789000e-05,0.53299983377000e-05,0.49090630091000e-05,0.45131447603000e-05,0.41414527036000e-05,&
      &0.37930808178000e-05,0.34671023392000e-05,0.31626496555000e-05,0.28790488351000e-05,0.26153584954000e-05,&
      &0.23706216638000e-05,0.21438943810000e-05,0.19344278810000e-05,0.17414204433000e-05,0.15639217042000e-05,&
      &0.14010025793000e-05,0.12518142441000e-05,0.11156982313000e-05,0.99213607723000e-06,0.87850835533000e-06,&
      &0.77726008432000e-06      /)

      pblast_data%pra(4,1:256)= (/&
      &0.10592157787000e-03,0.10598960341000e-03,0.10604568366000e-03,0.10608989686000e-03,0.10612227007000e-03,&
      &0.10614275249000e-03,0.10615138642000e-03,0.10614821313000e-03,0.10613324937000e-03,0.10610648024000e-03,&
      &0.10606790748000e-03,0.10601762440000e-03,0.10595557319000e-03,0.10588182606000e-03,0.10579651067000e-03,&
      &0.10569946046000e-03,0.10559078806000e-03,0.10547065619000e-03,0.10533904793000e-03,0.10519592304000e-03,&
      &0.10504148485000e-03,0.10487574933000e-03,0.10469865968000e-03,0.10451030207000e-03,0.10431075675000e-03,&
      &0.10410016913000e-03,0.10387863439000e-03,0.10364612528000e-03,0.10340266329000e-03,0.10314841315000e-03,&
      &0.10288350452000e-03,0.10260790483000e-03,0.10232167377000e-03,0.10202502773000e-03,0.10171806508000e-03,&
      &0.10140071287000e-03,0.10107313231000e-03,0.10073560899000e-03,0.10038798070000e-03,0.10003038846000e-03,&
      &0.99662995721000e-04,0.99285911807000e-04,0.98899297130000e-04,0.98503233789000e-04,0.98097696948000e-04,&
      &0.97682849326000e-04,0.97258943900000e-04,0.96825923703000e-04,0.96384006782000e-04,0.95933323438000e-04,&
      &0.95473952250000e-04,0.95006082614000e-04,0.94529674007000e-04,0.94044941097000e-04,0.93552111427000e-04,&
      &0.93051240484000e-04,0.92542406116000e-04,0.92025768497000e-04,0.91501505962000e-04,0.90969722258000e-04,&
      &0.90430507806000e-04,0.89884135391000e-04,0.89330590316000e-04,0.88770017293000e-04,0.88202710115000e-04,&
      &0.87628616392000e-04,0.87047890377000e-04,0.86460857611000e-04,0.85867643108000e-04,0.85268211436000e-04,&
      &0.84662756725000e-04,0.84051491943000e-04,0.83434517099000e-04,0.82811973803000e-04,0.82184106712000e-04,&
      &0.81550926113000e-04,0.80912654874000e-04,0.80269494162000e-04,0.79621476451000e-04,0.78968743744000e-04,&
      &0.78311516262000e-04,0.77649963815000e-04,0.76984225578000e-04,0.76314463101000e-04,0.75640733229000e-04,&
      &0.74963232581000e-04,0.74282153864000e-04,0.73597624138000e-04,0.72909831215000e-04,0.72218893004000e-04,&
      &0.71524892559000e-04,0.70828037816000e-04,0.70128554890000e-04,0.69426508718000e-04,0.68722033525000e-04,&
      &0.68015332825000e-04,0.67306577463000e-04,0.66595851044000e-04,0.65883290601000e-04,0.65169107908000e-04,&
      &0.64453477792000e-04,0.63736503722000e-04,0.63018242248000e-04,0.62299003199000e-04,0.61578897676000e-04,&
      &0.60857984213000e-04,0.60136497466000e-04,0.59414544351000e-04,0.58692254056000e-04,0.57969851365000e-04,&
      &0.57247444092000e-04,0.56525118150000e-04,0.55803021911000e-04,0.55081383807000e-04,0.54360283587000e-04,&
      &0.53639850131000e-04,0.52920243058000e-04,0.52201625717000e-04,0.51484141938000e-04,0.50767934067000e-04,&
      &0.50053043379000e-04,0.49339677582000e-04,0.48627970156000e-04,0.47918032003000e-04,0.47210135700000e-04,&
      &0.46504221791000e-04,0.45800442908000e-04,0.45099020364000e-04,0.44400015292000e-04,0.43703608225000e-04,&
      &0.43009894663000e-04,0.42318966812000e-04,0.41630994305000e-04,0.40946116746000e-04,0.40264441527000e-04,&
      &0.39586042440000e-04,0.38911062139000e-04,0.38239682091000e-04,0.37571932603000e-04,0.36907945583000e-04,&
      &0.36247842079000e-04,0.35591749955000e-04,0.34939812836000e-04,0.34292080056000e-04,0.33648640398000e-04,&
      &0.33009630482000e-04,0.32375186270000e-04,0.31745387444000e-04,0.31120296477000e-04,0.30500051180000e-04,&
      &0.29884780618000e-04,0.29274530076000e-04,0.28669394743000e-04,0.28069479572000e-04,0.27474888202000e-04,&
      &0.26885719830000e-04,0.26302042725000e-04,0.25723910059000e-04,0.25151449527000e-04,0.24584768730000e-04,&
      &0.24023899309000e-04,0.23468887584000e-04,0.22919872604000e-04,0.22376931376000e-04,0.21840090308000e-04,&
      &0.21309440156000e-04,0.20785066865000e-04,0.20267048225000e-04,0.19755416332000e-04,0.19250221772000e-04,&
      &0.18751566859000e-04,0.18259481726000e-04,0.17774012987000e-04,0.17295242846000e-04,0.16823205359000e-04,&
      &0.16357963632000e-04,0.15899551073000e-04,0.15448002373000e-04,0.15003380670000e-04,0.14565722379000e-04,&
      &0.14135053974000e-04,0.13711412778000e-04,0.13294840433000e-04,0.12885356853000e-04,0.12482988637000e-04,&
      &0.12087766647000e-04,0.11699706338000e-04,0.11318823107000e-04,0.10945146431000e-04,0.10578666663000e-04,&
      &0.10219404643000e-04,0.98673831211000e-05,0.95225851196000e-05,0.91850130920000e-05,0.88546687822000e-05,&
      &0.85315497651000e-05,0.82156449381000e-05,0.79069441934000e-05,0.76054208163000e-05,0.73110666105000e-05,&
      &0.70238681034000e-05,0.67437858134000e-05,0.64707965413000e-05,0.62048644899000e-05,0.59459588392000e-05,&
      &0.56940373213000e-05,0.54490497479000e-05,0.52109659923000e-05,0.49797296056000e-05,0.47552793737000e-05,&
      &0.45375620374000e-05,0.43265201560000e-05,0.41220876650000e-05,0.39242001199000e-05,0.37327778682000e-05,&
      &0.35477425091000e-05,0.33690353837000e-05,0.31965621932000e-05,0.30302291530000e-05,0.28699497880000e-05,&
      &0.27156398282000e-05,0.25672117193000e-05,0.24245550353000e-05,0.22875534072000e-05,0.21561182417000e-05,&
      &0.20301496648000e-05,0.19095314888000e-05,0.17941430674000e-05,0.16838560859000e-05,0.15785796979000e-05,&
      &0.14781927368000e-05,0.13825632567000e-05,0.12915558198000e-05,0.12050449946000e-05,0.11229308735000e-05,&
      &0.10450754977000e-05,0.97133569968000e-06,0.90157064624000e-06,0.83567498427000e-06,0.77351921706000e-06,&
      &0.71496007938000e-06,0.65985534971000e-06,0.60807243384000e-06,0.55950833088000e-06,0.51402248727000e-06,&
      &0.47147359618000e-06,0.43172351651000e-06,0.39466435950000e-06,0.36018020627000e-06,0.32813561143000e-06,&
      &0.29839856410000e-06,0.27084834173000e-06,0.24539208907000e-06,0.22195836243000e-06,0.20017179380000e-06,&
      &0.18038574429000e-06      /)

      pblast_data%pra(5,1:256)= (/&
      &0.31127269655000e-04,0.31121684042000e-04,0.31106361563000e-04,0.31081404513000e-04,0.31047027770000e-04,&
      &0.31003423888000e-04,0.30950748387000e-04,0.30889203649000e-04,0.30818985318000e-04,0.30740321222000e-04,&
      &0.30653438444000e-04,0.30558473945000e-04,0.30455686352000e-04,0.30345317393000e-04,0.30227582490000e-04,&
      &0.30102720064000e-04,0.29970877910000e-04,0.29832340400000e-04,0.29687405213000e-04,0.29536227697000e-04,&
      &0.29379009547000e-04,0.29216009237000e-04,0.29047478438000e-04,0.28873656817000e-04,0.28694723750000e-04,&
      &0.28510909867000e-04,0.28322451957000e-04,0.28129595257000e-04,0.27932571116000e-04,0.27731506377000e-04,&
      &0.27526663405000e-04,0.27318277569000e-04,0.27106550739000e-04,0.26891678524000e-04,0.26673839767000e-04,&
      &0.26453221872000e-04,0.26230047261000e-04,0.26004531434000e-04,0.25776796182000e-04,0.25547023532000e-04,&
      &0.25315429838000e-04,0.25082196744000e-04,0.24847432799000e-04,0.24611340715000e-04,0.24374068056000e-04,&
      &0.24135749045000e-04,0.23896561521000e-04,0.23656644534000e-04,0.23416114554000e-04,0.23175095144000e-04,&
      &0.22933756981000e-04,0.22692222328000e-04,0.22450580413000e-04,0.22208967391000e-04,0.21967496768000e-04,&
      &0.21726268643000e-04,0.21485379713000e-04,0.21244928298000e-04,0.21005027716000e-04,0.20765776661000e-04,&
      &0.20527231103000e-04,0.20289483824000e-04,0.20052616010000e-04,0.19816696030000e-04,0.19581809323000e-04,&
      &0.19348013274000e-04,0.19115369449000e-04,0.18883934003000e-04,0.18653776774000e-04,0.18424965320000e-04,&
      &0.18197517579000e-04,0.17971480413000e-04,0.17746926063000e-04,0.17523900571000e-04,0.17302403834000e-04,&
      &0.17082496662000e-04,0.16864219664000e-04,0.16647579792000e-04,0.16432607981000e-04,0.16219376002000e-04,&
      &0.16007856240000e-04,0.15798065107000e-04,0.15590065245000e-04,0.15383835967000e-04,0.15179410884000e-04,&
      &0.14976796036000e-04,0.14776007096000e-04,0.14577070378000e-04,0.14379952655000e-04,0.14184695143000e-04,&
      &0.13991301465000e-04,0.13799746254000e-04,0.13610052261000e-04,0.13422233827000e-04,0.13236276516000e-04,&
      &0.13052167055000e-04,0.12869915232000e-04,0.12689523105000e-04,0.12510966406000e-04,0.12334253638000e-04,&
      &0.12159391237000e-04,0.11986347042000e-04,0.11815118986000e-04,0.11645699569000e-04,0.11478085054000e-04,&
      &0.11312262946000e-04,0.11148207109000e-04,0.10985926140000e-04,0.10825405705000e-04,0.10666619179000e-04,&
      &0.10509561856000e-04,0.10354221686000e-04,0.10200585783000e-04,0.10048637243000e-04,0.98983586460000e-05,&
      &0.97497413340000e-05,0.96027618516000e-05,0.94574146045000e-05,0.93136814747000e-05,0.91715297559000e-05,&
      &0.90309652055000e-05,0.88919700422000e-05,0.87545225466000e-05,0.86186049845000e-05,0.84841988743000e-05,&
      &0.83512951304000e-05,0.82198692182000e-05,0.80899115419000e-05,0.79614167733000e-05,0.78343434148000e-05,&
      &0.77086882984000e-05,0.75844354901000e-05,0.74615635139000e-05,0.73400769101000e-05,0.72199261920000e-05,&
      &0.71011102898000e-05,0.69836290137000e-05,0.68674456809000e-05,0.67525521498000e-05,0.66389345159000e-05,&
      &0.65265802539000e-05,0.64154698544000e-05,0.63055833738000e-05,0.61969204637000e-05,0.60894573320000e-05,&
      &0.59831780589000e-05,0.58780756877000e-05,0.57741255162000e-05,0.56713265606000e-05,0.55696629473000e-05,&
      &0.54691143737000e-05,0.53696754843000e-05,0.52713280004000e-05,0.51740631809000e-05,0.50778709974000e-05,&
      &0.49827343385000e-05,0.48886434318000e-05,0.47955953648000e-05,0.47035707999000e-05,0.46125547824000e-05,&
      &0.45225470474000e-05,0.44335347384000e-05,0.43455062671000e-05,0.42584550504000e-05,0.41723710845000e-05,&
      &0.40872412520000e-05,0.40030622972000e-05,0.39198283942000e-05,0.38375282857000e-05,0.37561529059000e-05,&
      &0.36757005436000e-05,0.35961618179000e-05,0.35175271319000e-05,0.34397926102000e-05,0.33629560524000e-05,&
      &0.32870112142000e-05,0.32119500265000e-05,0.31377706048000e-05,0.30644714041000e-05,0.29920440378000e-05,&
      &0.29204863985000e-05,0.28497976957000e-05,0.27799733944000e-05,0.27110094164000e-05,0.26429071995000e-05,&
      &0.25756642129000e-05,0.25092785401000e-05,0.24437507303000e-05,0.23790791550000e-05,0.23152635786000e-05,&
      &0.22523057569000e-05,0.21902053084000e-05,0.21289608510000e-05,0.20685769230000e-05,0.20090550779000e-05,&
      &0.19503956977000e-05,0.18926012316000e-05,0.18356738418000e-05,0.17796146448000e-05,0.17244292159000e-05,&
      &0.16701211082000e-05,0.16166902970000e-05,0.15641416684000e-05,0.15124798996000e-05,0.14617071862000e-05,&
      &0.14118275989000e-05,0.13628475284000e-05,0.13147691479000e-05,0.12675940785000e-05,0.12213288116000e-05,&
      &0.11759803321000e-05,0.11315475885000e-05,0.10880365230000e-05,0.10454516104000e-05,0.10037948364000e-05,&
      &0.96307019169000e-06,0.92328205936000e-06,0.88443137539000e-06,0.84652110687000e-06,0.80955614992000e-06,&
      &0.77353503846000e-06,0.73845986126000e-06,0.70433119766000e-06,0.67114914369000e-06,0.63891501848000e-06,&
      &0.60762665808000e-06,0.57728385434000e-06,0.54788375087000e-06,0.51942136933000e-06,0.49189612335000e-06,&
      &0.46530246885000e-06,0.43963478448000e-06,0.41488713983000e-06,0.39105175805000e-06,0.36812246644000e-06,&
      &0.34609047849000e-06,0.32494516754000e-06,0.30467448757000e-06,0.28526910921000e-06,0.26671741731000e-06,&
      &0.24900473840000e-06,0.23211541116000e-06,0.21603426400000e-06,0.20074845274000e-06,0.18623973518000e-06,&
      &0.17248905965000e-06,0.15947639389000e-06,0.14718473427000e-06,0.13559615949000e-06,0.12468804419000e-06,&
      &0.11443692842000e-06,0.10482052830000e-06,0.95821073210000e-07,0.87426092938000e-07,0.79555164891000e-07,&
      &0.72283836469000e-07      /)

      pblast_data%pra(6,1:256)= (/&
      &0.50849785793000e-05,0.50957213193000e-05,0.51059282777000e-05,0.51156003983000e-05,0.51247447309000e-05,&
      &0.51333687617000e-05,0.51414745454000e-05,0.51490723729000e-05,0.51561672007000e-05,0.51627650459000e-05,&
      &0.51688735188000e-05,0.51744927274000e-05,0.51796349977000e-05,0.51843032150000e-05,0.51885026139000e-05,&
      &0.51922435447000e-05,0.51955259168000e-05,0.51983566743000e-05,0.52007446426000e-05,0.52026928883000e-05,&
      &0.52042052079000e-05,0.52052917088000e-05,0.52059558002000e-05,0.52061995643000e-05,0.52060288307000e-05,&
      &0.52054517230000e-05,0.52044734246000e-05,0.52030944077000e-05,0.52013197960000e-05,0.51991566949000e-05,&
      &0.51966077016000e-05,0.51936736054000e-05,0.51903631805000e-05,0.51866816635000e-05,0.51826292006000e-05,&
      &0.51782085160000e-05,0.51734233732000e-05,0.51682788894000e-05,0.51627797369000e-05,0.51569253297000e-05,&
      &0.51507165529000e-05,0.51441621499000e-05,0.51372611692000e-05,0.51300134188000e-05,0.51224249179000e-05,&
      &0.51144991721000e-05,0.51062318605000e-05,0.50976297891000e-05,0.50886953312000e-05,0.50794267603000e-05,&
      &0.50698265968000e-05,0.50598958445000e-05,0.50496380939000e-05,0.50390512322000e-05,0.50281350877000e-05,&
      &0.50168947201000e-05,0.50053289306000e-05,0.49934390622000e-05,0.49812240801000e-05,0.49686838236000e-05,&
      &0.49558206328000e-05,0.49426339234000e-05,0.49291244184000e-05,0.49152929134000e-05,0.49011385004000e-05,&
      &0.48866597205000e-05,0.48718556333000e-05,0.48567299436000e-05,0.48412826614000e-05,0.48255097379000e-05,&
      &0.48094108585000e-05,0.47929880103000e-05,0.47762417553000e-05,0.47591699353000e-05,0.47417724788000e-05,&
      &0.47240511488000e-05,0.47060009947000e-05,0.46876265231000e-05,0.46689250557000e-05,0.46498929512000e-05,&
      &0.46305361377000e-05,0.46108518570000e-05,0.45908392528000e-05,0.45704985297000e-05,0.45498295634000e-05,&
      &0.45288319763000e-05,0.45075055504000e-05,0.44858514851000e-05,0.44638689276000e-05,0.44415574796000e-05,&
      &0.44189193843000e-05,0.43959546970000e-05,0.43726600914000e-05,0.43490392289000e-05,0.43250952951000e-05,&
      &0.43008233837000e-05,0.42762279506000e-05,0.42513103801000e-05,0.42260703422000e-05,0.42005092405000e-05,&
      &0.41746289089000e-05,0.41484304350000e-05,0.41219136759000e-05,0.40950827417000e-05,0.40679417671000e-05,&
      &0.40404895929000e-05,0.40127274273000e-05,0.39846576636000e-05,0.39562852848000e-05,0.39276137444000e-05,&
      &0.38986432685000e-05,0.38693776213000e-05,0.38398205010000e-05,0.38099751266000e-05,0.37798447729000e-05,&
      &0.37494321364000e-05,0.37187432959000e-05,0.36877808031000e-05,0.36565480604000e-05,0.36250513223000e-05,&
      &0.35932938863000e-05,0.35612805014000e-05,0.35290173588000e-05,0.34965087319000e-05,0.34637592600000e-05,&
      &0.34307753372000e-05,0.33975634128000e-05,0.33641263325000e-05,0.33304707592000e-05,0.32966064555000e-05,&
      &0.32625370553000e-05,0.32282671031000e-05,0.31938037122000e-05,0.31591565201000e-05,0.31243328893000e-05,&
      &0.30893356820000e-05,0.30541737626000e-05,0.30188547171000e-05,0.29833862010000e-05,0.29477773650000e-05,&
      &0.29120343191000e-05,0.28761632731000e-05,0.28401734931000e-05,0.28040746063000e-05,0.27678747251000e-05,&
      &0.27315805173000e-05,0.26952013631000e-05,0.26587452846000e-05,0.26222217261000e-05,0.25856409128000e-05,&
      &0.25490088706000e-05,0.25123335356000e-05,0.24756264466000e-05,0.24388962450000e-05,0.24021522267000e-05,&
      &0.23654055106000e-05,0.23286630288000e-05,0.22919329817000e-05,0.22552276147000e-05,0.22185551287000e-05,&
      &0.21819240298000e-05,0.21453439661000e-05,0.21088261029000e-05,0.20723803342000e-05,0.20360147007000e-05,&
      &0.19997377660000e-05,0.19635619170000e-05,0.19274954738000e-05,0.18915470496000e-05,0.18557300380000e-05,&
      &0.18200502202000e-05,0.17845176710000e-05,0.17491423884000e-05,0.17139342435000e-05,0.16789038854000e-05,&
      &0.16440602702000e-05,0.16094104724000e-05,0.15749648348000e-05,0.15407354589000e-05,0.15067303046000e-05,&
      &0.14729571596000e-05,0.14394253065000e-05,0.14061449275000e-05,0.13731257085000e-05,0.13403758484000e-05,&
      &0.13079033525000e-05,0.12757163219000e-05,0.12438245037000e-05,0.12122384879000e-05,0.11809660202000e-05,&
      &0.11500123173000e-05,0.11193874189000e-05,0.10890999223000e-05,0.10591573898000e-05,0.10295688578000e-05,&
      &0.10003394008000e-05,0.97147693451000e-06,0.94298964266000e-06,0.91488532095000e-06,0.88717102237000e-06,&
      &0.85985222760000e-06,0.83293452438000e-06,0.80642633692000e-06,0.78033523814000e-06,0.75466426754000e-06,&
      &0.72942011958000e-06,0.70460729365000e-06,0.68023192739000e-06,0.65630036997000e-06,0.63281490766000e-06,&
      &0.60978157868000e-06,0.58720333096000e-06,0.56508417536000e-06,0.54342935056000e-06,0.52224054119000e-06,&
      &0.50152051914000e-06,0.48127264660000e-06,0.46149890891000e-06,0.44220072812000e-06,0.42338006079000e-06,&
      &0.40503792793000e-06,0.38717533678000e-06,0.36979232075000e-06,0.35288868188000e-06,0.33646461791000e-06,&
      &0.32051895766000e-06,0.30505044551000e-06,0.29005766290000e-06,0.27553829406000e-06,0.26148980868000e-06,&
      &0.24790967607000e-06,0.23479450339000e-06,0.22214030826000e-06,0.20994232697000e-06,0.19819605919000e-06,&
      &0.18689752272000e-06,0.17604076776000e-06,0.16561934110000e-06,0.15562670967000e-06,0.14605669032000e-06,&
      &0.13690282510000e-06,0.12815701425000e-06,0.11981091112000e-06,0.11185602172000e-06,0.10428493238000e-06,&
      &0.97089009301000e-07,0.90258690862000e-07,0.83784853117000e-07,0.77655574368000e-07,0.71845428191000e-07,&
      &0.66522407530000e-07,0.61891683642000e-07,0.58158389975000e-07,0.55527659975000e-07,0.54204627088000e-07,&
      &0.54394424761000e-07      /)

      pblast_data%pra(7,1:256)= (/&
      &0.22185448516000e-05,0.22341932096000e-05,0.22491466619000e-05,0.22634062912000e-05,0.22769747165000e-05,&
      &0.22898564331000e-05,0.23020573712000e-05,0.23135827277000e-05,0.23244395264000e-05,0.23346349674000e-05,&
      &0.23441785894000e-05,0.23530812025000e-05,0.23613490920000e-05,0.23689938497000e-05,0.23760289779000e-05,&
      &0.23824662281000e-05,0.23883173806000e-05,0.23935944489000e-05,0.23983097735000e-05,0.24024782479000e-05,&
      &0.24061146112000e-05,0.24092330539000e-05,0.24118470638000e-05,0.24139705236000e-05,0.24156195717000e-05,&
      &0.24168095622000e-05,0.24175551031000e-05,0.24178705359000e-05,0.24177712658000e-05,0.24172712178000e-05,&
      &0.24163866138000e-05,0.24151335096000e-05,0.24135250811000e-05,0.24115753802000e-05,0.24092989903000e-05,&
      &0.24067109414000e-05,0.24038247360000e-05,0.24006544556000e-05,0.23972148521000e-05,0.23935159333000e-05,&
      &0.23895714164000e-05,0.23853948730000e-05,0.23809985380000e-05,0.23763947065000e-05,0.23715929373000e-05,&
      &0.23666054861000e-05,0.23614432747000e-05,0.23561168160000e-05,0.23506366619000e-05,0.23450101474000e-05,&
      &0.23392461123000e-05,0.23333560860000e-05,0.23273490588000e-05,0.23212307527000e-05,0.23150080696000e-05,&
      &0.23086890918000e-05,0.23022806871000e-05,0.22957896330000e-05,0.22892224478000e-05,0.22825830098000e-05,&
      &0.22758759635000e-05,0.22691077897000e-05,0.22622827433000e-05,0.22554034648000e-05,0.22484736232000e-05,&
      &0.22414979416000e-05,0.22344777120000e-05,0.22274152385000e-05,0.22203141251000e-05,0.22131755967000e-05,&
      &0.22060007196000e-05,0.21987899791000e-05,0.21915452866000e-05,0.21842669330000e-05,0.21769535405000e-05,&
      &0.21696069135000e-05,0.21622252443000e-05,0.21548088369000e-05,0.21473566292000e-05,0.21398659486000e-05,&
      &0.21323383082000e-05,0.21247677940000e-05,0.21171547114000e-05,0.21094984846000e-05,0.21017937790000e-05,&
      &0.20940392336000e-05,0.20862322427000e-05,0.20783692050000e-05,0.20704472480000e-05,0.20624634923000e-05,&
      &0.20544139012000e-05,0.20462953262000e-05,0.20381046632000e-05,0.20298373996000e-05,0.20214895433000e-05,&
      &0.20130580379000e-05,0.20045378309000e-05,0.19959254328000e-05,0.19872164244000e-05,0.19784064618000e-05,&
      &0.19694917103000e-05,0.19604679169000e-05,0.19513319857000e-05,0.19420791097000e-05,0.19327044377000e-05,&
      &0.19232039868000e-05,0.19135738316000e-05,0.19038103181000e-05,0.18939099031000e-05,0.18838690235000e-05,&
      &0.18736837045000e-05,0.18633502580000e-05,0.18528656063000e-05,0.18422259680000e-05,0.18314289380000e-05,&
      &0.18204718016000e-05,0.18093510288000e-05,0.17980653163000e-05,0.17866101110000e-05,0.17749849830000e-05,&
      &0.17631885697000e-05,0.17512172243000e-05,0.17390713833000e-05,0.17267504255000e-05,0.17142511985000e-05,&
      &0.17015724143000e-05,0.16887161973000e-05,0.16756832496000e-05,0.16624709712000e-05,0.16490791175000e-05,&
      &0.16355110235000e-05,0.16217682264000e-05,0.16078498928000e-05,0.15937574061000e-05,0.15794940942000e-05,&
      &0.15650611459000e-05,0.15504609258000e-05,0.15356980275000e-05,0.15207738053000e-05,0.15056913854000e-05,&
      &0.14904543777000e-05,0.14750670718000e-05,0.14595342658000e-05,0.14438599991000e-05,0.14280483693000e-05,&
      &0.14121033952000e-05,0.13960311253000e-05,0.13798379431000e-05,0.13635282795000e-05,0.13471064412000e-05,&
      &0.13305806794000e-05,0.13139561321000e-05,0.12972384152000e-05,0.12804360079000e-05,0.12635532127000e-05,&
      &0.12465974766000e-05,0.12295759733000e-05,0.12124958904000e-05,0.11953642449000e-05,0.11781856775000e-05,&
      &0.11609693350000e-05,0.11437219027000e-05,0.11264511525000e-05,0.11091642127000e-05,0.10918651305000e-05,&
      &0.10745670273000e-05,0.10572713494000e-05,0.10399850515000e-05,0.10227211832000e-05,0.10054825630000e-05,&
      &0.98827473324000e-06,0.97110621752000e-06,0.95398377627000e-06,0.93691271576000e-06,0.91990073711000e-06,&
      &0.90295161939000e-06,0.88607436069000e-06,0.86927361547000e-06,0.85255377817000e-06,0.83592336250000e-06,&
      &0.81938594946000e-06,0.80294436939000e-06,0.78660718035000e-06,0.77037925004000e-06,0.75426160154000e-06,&
      &0.73826112487000e-06,0.72238274002000e-06,0.70662896719000e-06,0.69100306043000e-06,0.67550971657000e-06,&
      &0.66015178783000e-06,0.64493358388000e-06,0.62985589363000e-06,0.61492193599000e-06,0.60013618827000e-06,&
      &0.58549991982000e-06,0.57101463441000e-06,0.55668290515000e-06,0.54250667578000e-06,0.52848788638000e-06,&
      &0.51462857153000e-06,0.50092933801000e-06,0.48739205716000e-06,0.47401782272000e-06,0.46080713577000e-06,&
      &0.44776154851000e-06,0.43488289491000e-06,0.42217109336000e-06,0.40962675661000e-06,0.39725158454000e-06,&
      &0.38504616156000e-06,0.37301014211000e-06,0.36114550007000e-06,0.34945216378000e-06,0.33793180112000e-06,&
      &0.32658544536000e-06,0.31541254951000e-06,0.30441547037000e-06,0.29359324387000e-06,0.28294780279000e-06,&
      &0.27248139370000e-06,0.26219428883000e-06,0.25208803504000e-06,0.24216423216000e-06,0.23242327510000e-06,&
      &0.22286716106000e-06,0.21349827038000e-06,0.20431693153000e-06,0.19532709268000e-06,0.18652885537000e-06,&
      &0.17792479068000e-06,0.16951818496000e-06,0.16130918843000e-06,0.15330139005000e-06,0.14549722748000e-06,&
      &0.13789797153000e-06,0.13050622440000e-06,0.12332530752000e-06,0.11635599447000e-06,0.10960158807000e-06,&
      &0.10306385207000e-06,0.96743746474000e-07,0.90643666534000e-07,0.84765732967000e-07,0.79109760779000e-07,&
      &0.73679053634000e-07,0.68468066185000e-07,0.63465375177000e-07,0.58659729215000e-07,0.54039876900000e-07,&
      &0.49594566834000e-07,0.45312547621000e-07,0.41182567863000e-07,0.37193376163000e-07,0.33333721123000e-07,&
      &0.29592351345000e-07      /)

      pblast_data%pra(8,1:256)= (/&
      &0.13678645364000e-05,0.13702539681000e-05,0.13726418277000e-05,0.13750245543000e-05,0.13773939486000e-05,&
      &0.13797434946000e-05,0.13820671485000e-05,0.13843600359000e-05,0.13866157337000e-05,0.13888290615000e-05,&
      &0.13909951327000e-05,0.13931073811000e-05,0.13951603795000e-05,0.13971524328000e-05,0.13990759857000e-05,&
      &0.14009272780000e-05,0.14027035575000e-05,0.14043977226000e-05,0.14060079484000e-05,0.14075303867000e-05,&
      &0.14089612851000e-05,0.14102971670000e-05,0.14115343184000e-05,0.14126694307000e-05,0.14137006773000e-05,&
      &0.14146254032000e-05,0.14154407234000e-05,0.14161438203000e-05,0.14167328005000e-05,0.14172067396000e-05,&
      &0.14175632626000e-05,0.14177988938000e-05,0.14179138582000e-05,0.14179083728000e-05,0.14177792739000e-05,&
      &0.14175254914000e-05,0.14171463741000e-05,0.14166419493000e-05,0.14160122565000e-05,0.14152563777000e-05,&
      &0.14143732242000e-05,0.14133637905000e-05,0.14122281294000e-05,0.14109663934000e-05,0.14095790285000e-05,&
      &0.14080665132000e-05,0.14064296414000e-05,0.14046690080000e-05,0.14027858405000e-05,0.14007810018000e-05,&
      &0.13986555134000e-05,0.13964110812000e-05,0.13940485687000e-05,0.13915694932000e-05,0.13889753554000e-05,&
      &0.13862680986000e-05,0.13834494837000e-05,0.13805205109000e-05,0.13774837250000e-05,0.13743417850000e-05,&
      &0.13710950147000e-05,0.13677457381000e-05,0.13642979961000e-05,0.13607516000000e-05,0.13571096918000e-05,&
      &0.13533749506000e-05,0.13495484505000e-05,0.13456326791000e-05,0.13416312516000e-05,0.13375439260000e-05,&
      &0.13333745624000e-05,0.13291263647000e-05,0.13248003781000e-05,0.13203986951000e-05,0.13159245284000e-05,&
      &0.13113787608000e-05,0.13067649218000e-05,0.13020842858000e-05,0.12973386404000e-05,0.12925322231000e-05,&
      &0.12876654673000e-05,0.12827399792000e-05,0.12777588121000e-05,0.12727239359000e-05,0.12676363564000e-05,&
      &0.12624981811000e-05,0.12573117758000e-05,0.12520778095000e-05,0.12467980478000e-05,0.12414749846000e-05,&
      &0.12361098294000e-05,0.12307034950000e-05,0.12252581946000e-05,0.12197744543000e-05,0.12142532304000e-05,&
      &0.12086961282000e-05,0.12031037396000e-05,0.11974770766000e-05,0.11918176280000e-05,0.11861256082000e-05,&
      &0.11804011439000e-05,0.11746453035000e-05,0.11688587226000e-05,0.11630418150000e-05,0.11571946692000e-05,&
      &0.11513172425000e-05,0.11454094914000e-05,0.11394715006000e-05,0.11335036893000e-05,0.11275059067000e-05,&
      &0.11214770624000e-05,0.11154172154000e-05,0.11093265105000e-05,0.11032034403000e-05,0.10970479352000e-05,&
      &0.10908590824000e-05,0.10846350014000e-05,0.10783759280000e-05,0.10720822286000e-05,0.10657507017000e-05,&
      &0.10593800014000e-05,0.10529706280000e-05,0.10465208636000e-05,0.10400278652000e-05,0.10334914883000e-05,&
      &0.10269112017000e-05,0.10202842200000e-05,0.10136088345000e-05,0.10068836197000e-05,0.10001067583000e-05,&
      &0.99327771857000e-06,0.98639375830000e-06,0.97945310323000e-06,0.97245401522000e-06,0.96539458331000e-06,&
      &0.95827325206000e-06,0.95108818488000e-06,0.94383794631000e-06,0.93652083792000e-06,0.92913433854000e-06,&
      &0.92167643481000e-06,0.91414618127000e-06,0.90654168721000e-06,0.89886149176000e-06,0.89110347635000e-06,&
      &0.88326628669000e-06,0.87534908719000e-06,0.86734973116000e-06,0.85926806321000e-06,0.85110115633000e-06,&
      &0.84284911929000e-06,0.83451153951000e-06,0.82608494445000e-06,0.81757149475000e-06,0.80896963599000e-06,&
      &0.80027812586000e-06,0.79149740547000e-06,0.78262771308000e-06,0.77366851677000e-06,0.76462040938000e-06,&
      &0.75548301364000e-06,0.74625756073000e-06,0.73694538628000e-06,0.72754668535000e-06,0.71806259316000e-06,&
      &0.70849577311000e-06,0.69884650257000e-06,0.68911649295000e-06,0.67930911757000e-06,0.66942639922000e-06,&
      &0.65947136409000e-06,0.64944608195000e-06,0.63935359621000e-06,0.62919754036000e-06,0.61898167630000e-06,&
      &0.60871011943000e-06,0.59838530167000e-06,0.58801316869000e-06,0.57759835168000e-06,0.56714424523000e-06,&
      &0.55665636028000e-06,0.54614063637000e-06,0.53560176165000e-06,0.52504473917000e-06,0.51447592727000e-06,&
      &0.50390264557000e-06,0.49332905262000e-06,0.48276137961000e-06,0.47220641919000e-06,0.46167084089000e-06,&
      &0.45116070110000e-06,0.44068428986000e-06,0.43024647059000e-06,0.41985401349000e-06,0.40951444371000e-06,&
      &0.39923448623000e-06,0.38902056858000e-06,0.37887993093000e-06,0.36881966765000e-06,0.35884508022000e-06,&
      &0.34896310725000e-06,0.33918078772000e-06,0.32950413012000e-06,0.31994012824000e-06,0.31049321304000e-06,&
      &0.30117001562000e-06,0.29197607144000e-06,0.28291665775000e-06,0.27399790824000e-06,0.26522359604000e-06,&
      &0.25659845008000e-06,0.24812699328000e-06,0.23981353319000e-06,0.23166216239000e-06,0.22367617288000e-06,&
      &0.21585829223000e-06,0.20821141666000e-06,0.20073815787000e-06,0.19344113397000e-06,0.18632230795000e-06,&
      &0.17938242785000e-06,0.17262305919000e-06,0.16604493830000e-06,0.15964906354000e-06,0.15343556488000e-06,&
      &0.14740355336000e-06,0.14155394267000e-06,0.13588547059000e-06,0.13039656413000e-06,0.12508655680000e-06,&
      &0.11995405597000e-06,0.11499645847000e-06,0.11021253475000e-06,0.10559991443000e-06,0.10115592986000e-06,&
      &0.96878184190000e-07,0.92763747643000e-07,0.88809188065000e-07,0.85012101613000e-07,0.81368984911000e-07,&
      &0.77876875587000e-07,0.74528695313000e-07,0.71324890009000e-07,0.68328792850000e-07,0.65609887699000e-07,&
      &0.63237658420000e-07,0.61281588878000e-07,0.59811162936000e-07,0.58895864458000e-07,0.58605177308000e-07,&
      &0.59008585349000e-07,0.60175572447000e-07,0.62175622465000e-07,0.65078219266000e-07,0.68952846714000e-07,&
      &0.73868988674000e-07      /)

      pblast_data%pra(9,1:256)= (/&
      &0.75873455368000e-06,0.75869138572000e-06,0.75862552394000e-06,0.75853770477000e-06,0.75842861735000e-06,&
      &0.75829857203000e-06,0.75814797374000e-06,0.75797718537000e-06,0.75778743959000e-06,0.75757947019000e-06,&
      &0.75735324765000e-06,0.75710881009000e-06,0.75684682747000e-06,0.75656811362000e-06,0.75627245376000e-06,&
      &0.75596070107000e-06,0.75563317678000e-06,0.75528993331000e-06,0.75493139689000e-06,0.75455746880000e-06,&
      &0.75416866236000e-06,0.75376480350000e-06,0.75334663661000e-06,0.75291463921000e-06,0.75246800774000e-06,&
      &0.75200647042000e-06,0.75153091716000e-06,0.75104150789000e-06,0.75053825519000e-06,0.75002086016000e-06,&
      &0.74948907207000e-06,0.74894344156000e-06,0.74838333663000e-06,0.74780936910000e-06,0.74722083645000e-06,&
      &0.74661772552000e-06,0.74600065012000e-06,0.74536800469000e-06,0.74472112926000e-06,0.74405864957000e-06,&
      &0.74338079231000e-06,0.74268788531000e-06,0.74197906814000e-06,0.74125500086000e-06,0.74051363184000e-06,&
      &0.73975572164000e-06,0.73898113602000e-06,0.73818909435000e-06,0.73737976113000e-06,0.73655260458000e-06,&
      &0.73570726459000e-06,0.73484349202000e-06,0.73396026551000e-06,0.73305755001000e-06,0.73213549444000e-06,&
      &0.73119260290000e-06,0.73022971776000e-06,0.72924557956000e-06,0.72823976657000e-06,0.72721212016000e-06,&
      &0.72616164143000e-06,0.72508807207000e-06,0.72399173059000e-06,0.72287077341000e-06,0.72172488147000e-06,&
      &0.72055468181000e-06,0.71935913251000e-06,0.71813702826000e-06,0.71688863941000e-06,0.71561315635000e-06,&
      &0.71431005109000e-06,0.71297873840000e-06,0.71161839818000e-06,0.71022874487000e-06,0.70880935018000e-06,&
      &0.70736005641000e-06,0.70587958178000e-06,0.70436728819000e-06,0.70282321856000e-06,0.70124635710000e-06,&
      &0.69963652581000e-06,0.69799269635000e-06,0.69631502992000e-06,0.69460247495000e-06,0.69285370225000e-06,&
      &0.69106950594000e-06,0.68924903891000e-06,0.68739137894000e-06,0.68549625159000e-06,0.68356309889000e-06,&
      &0.68159118866000e-06,0.67958040650000e-06,0.67752996887000e-06,0.67543923836000e-06,0.67330826798000e-06,&
      &0.67113630203000e-06,0.66892242932000e-06,0.66666680199000e-06,0.66436868502000e-06,0.66202773713000e-06,&
      &0.65964345606000e-06,0.65721564773000e-06,0.65474396258000e-06,0.65222754545000e-06,0.64966617580000e-06,&
      &0.64705974985000e-06,0.64440779128000e-06,0.64170997192000e-06,0.63896610757000e-06,0.63617574882000e-06,&
      &0.63333879868000e-06,0.63045474282000e-06,0.62752342631000e-06,0.62454502284000e-06,0.62151910434000e-06,&
      &0.61844505428000e-06,0.61532333110000e-06,0.61215331487000e-06,0.60893512399000e-06,0.60566897360000e-06,&
      &0.60235459745000e-06,0.59899185244000e-06,0.59558063882000e-06,0.59212095046000e-06,0.58861317013000e-06,&
      &0.58505708210000e-06,0.58145267888000e-06,0.57780054430000e-06,0.57410019359000e-06,0.57035222656000e-06,&
      &0.56655655514000e-06,0.56271353968000e-06,0.55882366525000e-06,0.55488680487000e-06,0.55090347820000e-06,&
      &0.54687397664000e-06,0.54279845247000e-06,0.53867739568000e-06,0.53451144562000e-06,0.53030099834000e-06,&
      &0.52604648096000e-06,0.52174821962000e-06,0.51740688202000e-06,0.51302294624000e-06,0.50859703480000e-06,&
      &0.50413002061000e-06,0.49962221404000e-06,0.49507438607000e-06,0.49048735602000e-06,0.48586188066000e-06,&
      &0.48119866758000e-06,0.47649850963000e-06,0.47176228095000e-06,0.46699094446000e-06,0.46218526075000e-06,&
      &0.45734584978000e-06,0.45247393396000e-06,0.44757088104000e-06,0.44263730428000e-06,0.43767434576000e-06,&
      &0.43268288122000e-06,0.42766404386000e-06,0.42261911060000e-06,0.41754925655000e-06,0.41245577851000e-06,&
      &0.40733948899000e-06,0.40220169446000e-06,0.39704387920000e-06,0.39186726421000e-06,0.38667310022000e-06,&
      &0.38146281909000e-06,0.37623744999000e-06,0.37099851394000e-06,0.36574760220000e-06,0.36048595752000e-06,&
      &0.35521499897000e-06,0.34993643340000e-06,0.34465153118000e-06,0.33936148162000e-06,0.33406818763000e-06,&
      &0.32877347139000e-06,0.32347810706000e-06,0.31818406799000e-06,0.31289284219000e-06,0.30760604286000e-06,&
      &0.30232522377000e-06,0.29705212273000e-06,0.29178828163000e-06,0.28653512983000e-06,0.28129440890000e-06,&
      &0.27606774968000e-06,0.27085669415000e-06,0.26566312528000e-06,0.26048878191000e-06,0.25533481946000e-06,&
      &0.25020290649000e-06,0.24509496512000e-06,0.24001248212000e-06,0.23495703374000e-06,0.22993054956000e-06,&
      &0.22493448226000e-06,0.21997000231000e-06,0.21503908353000e-06,0.21014328064000e-06,0.20528401730000e-06,&
      &0.20046309235000e-06,0.19568189826000e-06,0.19094181397000e-06,0.18624439771000e-06,0.18159106520000e-06,&
      &0.17698325444000e-06,0.17242253833000e-06,0.16791031143000e-06,0.16344750380000e-06,0.15903582659000e-06,&
      &0.15467649442000e-06,0.15037060185000e-06,0.14611963165000e-06,0.14192470864000e-06,0.13778684033000e-06,&
      &0.13370714529000e-06,0.12968670139000e-06,0.12572660460000e-06,0.12182787849000e-06,0.11799136933000e-06,&
      &0.11421790847000e-06,0.11050842065000e-06,0.10686356889000e-06,0.10328412722000e-06,0.99770930364000e-07,&
      &0.96324467155000e-07,0.92945303987000e-07,0.89633952922000e-07,0.86390874669000e-07,0.83216519771000e-07,&
      &0.80111257686000e-07,0.77075338348000e-07,0.74108859057000e-07,0.71212734303000e-07,0.68384247240000e-07,&
      &0.65619580762000e-07,0.62914964767000e-07,0.60266629151000e-07,0.57670803813000e-07,0.55123718651000e-07,&
      &0.52621603562000e-07,0.50160688445000e-07,0.47737203196000e-07,0.45347377713000e-07,0.42987441895000e-07,&
      &0.40653625639000e-07,0.38342158843000e-07,0.36049271404000e-07,0.33771193221000e-07,0.31504154191000e-07,&
      &0.29244384211000e-07      /)

      pblast_data%pra(10,1:256)= (/&
      &0.52869405787000e-06,0.52856529495000e-06,0.52839838351000e-06,0.52819427726000e-06,0.52795402166000e-06,&
      &0.52767847728000e-06,0.52736872636000e-06,0.52702576315000e-06,0.52665049672000e-06,0.52624361376000e-06,&
      &0.52580622876000e-06,0.52533922381000e-06,0.52484339590000e-06,0.52431978458000e-06,0.52376901260000e-06,&
      &0.52319191301000e-06,0.52258940799000e-06,0.52196216019000e-06,0.52131103720000e-06,0.52063686600000e-06,&
      &0.51994012462000e-06,0.51922179788000e-06,0.51848243550000e-06,0.51772265394000e-06,0.51694303252000e-06,&
      &0.51614430581000e-06,0.51532723397000e-06,0.51449215755000e-06,0.51363967422000e-06,0.51277045295000e-06,&
      &0.51188488598000e-06,0.51098347209000e-06,0.51006678319000e-06,0.50913516892000e-06,0.50818912097000e-06,&
      &0.50722903268000e-06,0.50625520739000e-06,0.50526812291000e-06,0.50426813657000e-06,0.50325546117000e-06,&
      &0.50223054202000e-06,0.50119369349000e-06,0.50014486760000e-06,0.49908453963000e-06,0.49801297955000e-06,&
      &0.49693019008000e-06,0.49583658351000e-06,0.49473228695000e-06,0.49361730105000e-06,0.49249188869000e-06,&
      &0.49135618239000e-06,0.49021009551000e-06,0.48905379060000e-06,0.48788756649000e-06,0.48671119270000e-06,&
      &0.48552475515000e-06,0.48432822169000e-06,0.48312168456000e-06,0.48190515596000e-06,0.48067840621000e-06,&
      &0.47944163399000e-06,0.47819464278000e-06,0.47693739084000e-06,0.47566991330000e-06,0.47439192821000e-06,&
      &0.47310324756000e-06,0.47180409811000e-06,0.47049419818000e-06,0.46917329451000e-06,0.46784132641000e-06,&
      &0.46649820580000e-06,0.46514375397000e-06,0.46377775430000e-06,0.46239980760000e-06,0.46101000900000e-06,&
      &0.45960813800000e-06,0.45819402493000e-06,0.45676746851000e-06,0.45532807654000e-06,0.45387564926000e-06,&
      &0.45241018520000e-06,0.45093142475000e-06,0.44943901708000e-06,0.44793274322000e-06,0.44641245033000e-06,&
      &0.44487789530000e-06,0.44332882859000e-06,0.44176513026000e-06,0.44018645433000e-06,0.43859255505000e-06,&
      &0.43698327735000e-06,0.43535839651000e-06,0.43371769236000e-06,0.43206087988000e-06,0.43038777580000e-06,&
      &0.42869824055000e-06,0.42699203163000e-06,0.42526892590000e-06,0.42352873658000e-06,0.42177115654000e-06,&
      &0.41999619291000e-06,0.41820361561000e-06,0.41639318344000e-06,0.41456479440000e-06,0.41271826111000e-06,&
      &0.41085333831000e-06,0.40897006239000e-06,0.40706836408000e-06,0.40514789381000e-06,0.40320856072000e-06,&
      &0.40125041779000e-06,0.39927327531000e-06,0.39727702344000e-06,0.39526165034000e-06,0.39322698473000e-06,&
      &0.39117308637000e-06,0.38909994937000e-06,0.38700743984000e-06,0.38489558223000e-06,0.38276426463000e-06,&
      &0.38061363707000e-06,0.37844367250000e-06,0.37625440944000e-06,0.37404589197000e-06,0.37181807473000e-06,&
      &0.36957107364000e-06,0.36730495600000e-06,0.36501981641000e-06,0.36271590795000e-06,0.36039314898000e-06,&
      &0.35805155370000e-06,0.35569157973000e-06,0.35331325971000e-06,0.35091645285000e-06,0.34850162899000e-06,&
      &0.34606899933000e-06,0.34361851639000e-06,0.34115053652000e-06,0.33866542194000e-06,0.33616318135000e-06,&
      &0.33364388677000e-06,0.33110801925000e-06,0.32855589192000e-06,0.32598757750000e-06,0.32340341072000e-06,&
      &0.32080376479000e-06,0.31818877163000e-06,0.31555872705000e-06,0.31291402141000e-06,0.31025493170000e-06,&
      &0.30758173691000e-06,0.30489483844000e-06,0.30219454557000e-06,0.29948109064000e-06,0.29675484815000e-06,&
      &0.29401612704000e-06,0.29126532207000e-06,0.28850293875000e-06,0.28572917215000e-06,0.28294431722000e-06,&
      &0.28014886447000e-06,0.27734318513000e-06,0.27452752972000e-06,0.27170233491000e-06,0.26886810373000e-06,&
      &0.26602502334000e-06,0.26317345664000e-06,0.26031381100000e-06,0.25744658453000e-06,0.25457217434000e-06,&
      &0.25169086290000e-06,0.24880304113000e-06,0.24590902922000e-06,0.24300934957000e-06,0.24010426612000e-06,&
      &0.23719417167000e-06,0.23427956179000e-06,0.23136069553000e-06,0.22843792642000e-06,0.22551171435000e-06,&
      &0.22258250121000e-06,0.21965056103000e-06,0.21671628310000e-06,0.21378013449000e-06,0.21084241979000e-06,&
      &0.20790349247000e-06,0.20496370366000e-06,0.20202354554000e-06,0.19908334469000e-06,0.19614352551000e-06,&
      &0.19320443191000e-06,0.19026644286000e-06,0.18733004955000e-06,0.18439548061000e-06,0.18146310935000e-06,&
      &0.17853353881000e-06,0.17560706701000e-06,0.17268403578000e-06,0.16976499656000e-06,0.16685024671000e-06,&
      &0.16394022863000e-06,0.16103542130000e-06,0.15813621333000e-06,0.15524315063000e-06,0.15235665844000e-06,&
      &0.14947705647000e-06,0.14660496819000e-06,0.14374093487000e-06,0.14088536737000e-06,0.13803880234000e-06,&
      &0.13520174293000e-06,0.13237486511000e-06,0.12955848916000e-06,0.12675337152000e-06,0.12396012363000e-06,&
      &0.12117914651000e-06,0.11841132309000e-06,0.11565730872000e-06,0.11291747441000e-06,0.11019267354000e-06,&
      &0.10748368374000e-06,0.10479115453000e-06,0.10211585805000e-06,0.99458467978000e-07,0.96819931484000e-07,&
      &0.94200895740000e-07,0.91602277692000e-07,0.89024837762000e-07,0.86469407303000e-07,0.83937187964000e-07,&
      &0.81428808098000e-07,0.78945134661000e-07,0.76487214634000e-07,0.74055575275000e-07,0.71653293687000e-07,&
      &0.69276664406000e-07,0.66911693149000e-07,0.64545539010000e-07,0.62165361083000e-07,0.59758318464000e-07,&
      &0.57311570248000e-07,0.54812275530000e-07,0.52247593405000e-07,0.49604682967000e-07,0.46870703312000e-07,&
      &0.44032813534000e-07,0.41078172729000e-07,0.37993939992000e-07,0.34767274417000e-07,0.31385335100000e-07,&
      &0.27835281135000e-07,0.24104271618000e-07,0.20179465643000e-07,0.16048022306000e-07,0.11697100701000e-07,&
      &0.71138599232000e-08      /)

!figure 2-10
      pblast_data%sri(1,1:256) = (/&
      &0.15478697982000e-01,0.15434457411000e-01,0.15393660061000e-01,0.15356150737000e-01,0.15321660321000e-01,&
      &0.15289997977000e-01,0.15261012912000e-01,0.15234483398000e-01,0.15210230319000e-01,0.15188072840000e-01,&
      &0.15167821064000e-01,0.15149333974000e-01,0.15132460337000e-01,0.15117010268000e-01,0.15102793403000e-01,&
      &0.15089707645000e-01,0.15077608417000e-01,0.15066317295000e-01,0.15055674345000e-01,0.15045556221000e-01,&
      &0.15035866817000e-01,0.15026430251000e-01,0.15017073598000e-01,0.15007748777000e-01,0.14998295340000e-01,&
      &0.14988604733000e-01,0.14978522366000e-01,0.14967945171000e-01,0.14956764064000e-01,0.14944898899000e-01,&
      &0.14932190464000e-01,0.14918534475000e-01,0.14903874938000e-01,0.14888089960000e-01,0.14871075245000e-01,&
      &0.14852734479000e-01,0.14832995063000e-01,0.14811772717000e-01,0.14788986834000e-01,0.14764535436000e-01,&
      &0.14738363245000e-01,0.14710411908000e-01,0.14680588621000e-01,0.14648848949000e-01,0.14615122501000e-01,&
      &0.14579348301000e-01,0.14541486980000e-01,0.14501494858000e-01,0.14459323591000e-01,0.14414915720000e-01,&
      &0.14368243291000e-01,0.14319307600000e-01,0.14268038515000e-01,0.14214418995000e-01,0.14158449590000e-01,&
      &0.14100102633000e-01,0.14039382551000e-01,0.13976269632000e-01,0.13910750685000e-01,0.13842837228000e-01,&
      &0.13772543189000e-01,0.13699870999000e-01,0.13624823038000e-01,0.13547418773000e-01,0.13467690793000e-01,&
      &0.13385661450000e-01,0.13301337729000e-01,0.13214767117000e-01,0.13125988861000e-01,0.13035015681000e-01,&
      &0.12941923848000e-01,0.12846731375000e-01,0.12749477524000e-01,0.12650241382000e-01,0.12549053818000e-01,&
      &0.12445971485000e-01,0.12341041671000e-01,0.12234326806000e-01,0.12125906557000e-01,0.12015829555000e-01,&
      &0.11904158913000e-01,0.11790960790000e-01,0.11676307200000e-01,0.11560256782000e-01,0.11442890954000e-01,&
      &0.11324271944000e-01,0.11204467957000e-01,0.11083570161000e-01,0.10961644582000e-01,0.10838732739000e-01,&
      &0.10714922604000e-01,0.10590327393000e-01,0.10465010704000e-01,0.10338991751000e-01,0.10212368863000e-01,&
      &0.10085243631000e-01,0.99576771256000e-02,0.98297175697000e-02,0.97014497820000e-02,0.95729341843000e-02,&
      &0.94442532506000e-02,0.93154799396000e-02,0.91866789873000e-02,0.90578922943000e-02,0.89291841187000e-02,&
      &0.88006622685000e-02,0.86723480834000e-02,0.85442995087000e-02,0.84165896971000e-02,0.82892786041000e-02,&
      &0.81624050885000e-02,0.80360325711000e-02,0.79102057229000e-02,0.77849760310000e-02,0.76604077257000e-02,&
      &0.75365188559000e-02,0.74133650427000e-02,0.72909903964000e-02,0.71694331635000e-02,0.70487420214000e-02,&
      &0.69289355663000e-02,0.68100586353000e-02,0.66921424971000e-02,0.65752136937000e-02,0.64593129740000e-02,&
      &0.63444527434000e-02,0.62306642899000e-02,0.61179804447000e-02,0.60064170078000e-02,0.58959914732000e-02,&
      &0.57867230795000e-02,0.56786331944000e-02,0.55717378991000e-02,0.54660445261000e-02,0.53615677681000e-02,&
      &0.52583263380000e-02,0.51563266154000e-02,0.50555722162000e-02,0.49560774974000e-02,0.48578498137000e-02,&
      &0.47608849064000e-02,0.46651867285000e-02,0.45707683361000e-02,0.44776257311000e-02,0.43857588914000e-02,&
      &0.42951690135000e-02,0.42058539744000e-02,0.41178116185000e-02,0.40310383934000e-02,0.39455316902000e-02,&
      &0.38612880575000e-02,0.37783013287000e-02,0.36965668741000e-02,0.36160737156000e-02,0.35368190468000e-02,&
      &0.34587996991000e-02,0.33820022995000e-02,0.33064181767000e-02,0.32320403214000e-02,0.31588618423000e-02,&
      &0.30868726587000e-02,0.30160614967000e-02,0.29464182420000e-02,0.28779325195000e-02,0.28105947464000e-02,&
      &0.27444004246000e-02,0.26793328474000e-02,0.26153819238000e-02,0.25525374897000e-02,0.24907839061000e-02,&
      &0.24301157137000e-02,0.23705241293000e-02,0.23119970253000e-02,0.22545198670000e-02,0.21980818360000e-02,&
      &0.21426752267000e-02,0.20882902220000e-02,0.20349140908000e-02,0.19825334926000e-02,0.19311392698000e-02,&
      &0.18807219046000e-02,0.18312718128000e-02,0.17827774113000e-02,0.17352280502000e-02,0.16886148411000e-02,&
      &0.16429264524000e-02,0.15981518677000e-02,0.15542809616000e-02,0.15113068306000e-02,0.14692196784000e-02,&
      &0.14280073234000e-02,0.13876604720000e-02,0.13481720392000e-02,0.13095271061000e-02,0.12717229624000e-02,&
      &0.12347487719000e-02,0.11985879786000e-02,0.11632416778000e-02,0.11286974818000e-02,0.10949430900000e-02,&
      &0.10619703684000e-02,0.10297710026000e-02,0.99833803929000e-03,0.96766059461000e-03,0.93772964753000e-03,&
      &0.90853556015000e-03,0.88006760479000e-03,0.85231981807000e-03,0.82528430111000e-03,0.79895006647000e-03,&
      &0.77330583812000e-03,0.74834175591000e-03,0.72405273730000e-03,0.70042799189000e-03,0.67745437118000e-03,&
      &0.65512737586000e-03,0.63343622952000e-03,0.61237152509000e-03,0.59192304241000e-03,0.57207827566000e-03,&
      &0.55282864329000e-03,0.53416967386000e-03,0.51608848261000e-03,0.49856944379000e-03,0.48160642812000e-03,&
      &0.46519244730000e-03,0.44931612642000e-03,0.43396547906000e-03,0.41913022212000e-03,0.40480295256000e-03,&
      &0.39097042812000e-03,0.37762317593000e-03,0.36475321352000e-03,0.35234909550000e-03,0.34040054371000e-03,&
      &0.32889793909000e-03,0.31783166194000e-03,0.30719050263000e-03,0.29696757684000e-03,0.28715079131000e-03,&
      &0.27773196230000e-03,0.26870186580000e-03,0.26004816235000e-03,0.25176697511000e-03,0.24384646013000e-03,&
      &0.23627776810000e-03,0.22905405101000e-03,0.22216577349000e-03,0.21560734517000e-03,0.20937036390000e-03,&
      &0.20344611094000e-03,0.19782830661000e-03,0.19251188143000e-03,0.18749358600000e-03,0.18275301775000e-03,&
      &0.17831039180000e-03      /)

      pblast_data%sri(2,1:256) = (/&
      &0.62770705928000e-02,0.62593544593000e-02,0.62414788023000e-02,0.62234442332000e-02,0.62052377659000e-02,&
      &0.61868420035000e-02,0.61682596466000e-02,0.61494726100000e-02,0.61304722210000e-02,0.61112546318000e-02,&
      &0.60918028261000e-02,0.60721107229000e-02,0.60521736419000e-02,0.60319852154000e-02,0.60115347792000e-02,&
      &0.59908137422000e-02,0.59698211621000e-02,0.59485420727000e-02,0.59269694471000e-02,0.59051135275000e-02,&
      &0.58829598923000e-02,0.58604944046000e-02,0.58377181363000e-02,0.58146311636000e-02,0.57912177401000e-02,&
      &0.57674915796000e-02,0.57434402243000e-02,0.57190465453000e-02,0.56943214119000e-02,0.56692685276000e-02,&
      &0.56438682379000e-02,0.56181229273000e-02,0.55920445026000e-02,0.55656115724000e-02,0.55388398465000e-02,&
      &0.55117183697000e-02,0.54842404543000e-02,0.54564225084000e-02,0.54282548362000e-02,0.53997324072000e-02,&
      &0.53708628293000e-02,0.53416497697000e-02,0.53120845855000e-02,0.52821698912000e-02,0.52519126997000e-02,&
      &0.52213164561000e-02,0.51903794998000e-02,0.51590992671000e-02,0.51274859334000e-02,0.50955387449000e-02,&
      &0.50632565532000e-02,0.50306523450000e-02,0.49977246482000e-02,0.49644776832000e-02,0.49309171969000e-02,&
      &0.48970462339000e-02,0.48628670790000e-02,0.48283894213000e-02,0.47936147998000e-02,0.47585473985000e-02,&
      &0.47231999547000e-02,0.46875690744000e-02,0.46516651944000e-02,0.46154976074000e-02,0.45790690692000e-02,&
      &0.45423861768000e-02,0.45054592499000e-02,0.44682926756000e-02,0.44308929993000e-02,0.43932723330000e-02,&
      &0.43554337720000e-02,0.43173867180000e-02,0.42791386521000e-02,0.42406965204000e-02,0.42020687870000e-02,&
      &0.41632701832000e-02,0.41243067912000e-02,0.40851819685000e-02,0.40459075920000e-02,0.40064967174000e-02,&
      &0.39669527415000e-02,0.39272857644000e-02,0.38875108577000e-02,0.38476338449000e-02,0.38076588556000e-02,&
      &0.37676015860000e-02,0.37274739769000e-02,0.36872821434000e-02,0.36470345965000e-02,0.36067461317000e-02,&
      &0.35664237091000e-02,0.35260753481000e-02,0.34857107024000e-02,0.34453465485000e-02,0.34049868328000e-02,&
      &0.33646461954000e-02,0.33243278285000e-02,0.32840467740000e-02,0.32438106321000e-02,0.32036302562000e-02,&
      &0.31635198636000e-02,0.31234846980000e-02,0.30835334258000e-02,0.30436745170000e-02,0.30039210476000e-02,&
      &0.29642865175000e-02,0.29247723421000e-02,0.28853911750000e-02,0.28461545941000e-02,0.28070689084000e-02,&
      &0.27681423579000e-02,0.27293858537000e-02,0.26908068372000e-02,0.26524133026000e-02,0.26142176623000e-02,&
      &0.25762275706000e-02,0.25384476102000e-02,0.25008845441000e-02,0.24635502006000e-02,0.24264542117000e-02,&
      &0.23895998394000e-02,0.23529899483000e-02,0.23166376318000e-02,0.22805555303000e-02,0.22447422410000e-02,&
      &0.22092017224000e-02,0.21739454574000e-02,0.21389807580000e-02,0.21043090276000e-02,0.20699395316000e-02,&
      &0.20358760274000e-02,0.20021217095000e-02,0.19686837314000e-02,0.19355679704000e-02,0.19027766682000e-02,&
      &0.18703136407000e-02,0.18381843386000e-02,0.18063934313000e-02,0.17749432010000e-02,0.17438378299000e-02,&
      &0.17130792290000e-02,0.16826701332000e-02,0.16526152628000e-02,0.16229138141000e-02,0.15935708473000e-02,&
      &0.15645883333000e-02,0.15359648065000e-02,0.15077039384000e-02,0.14798075325000e-02,0.14522774279000e-02,&
      &0.14251129779000e-02,0.13983127821000e-02,0.13718802136000e-02,0.13458150371000e-02,0.13201164786000e-02,&
      &0.12947852683000e-02,0.12698208340000e-02,0.12452220168000e-02,0.12209893836000e-02,0.11971213141000e-02,&
      &0.11736143864000e-02,0.11504704988000e-02,0.11276862084000e-02,0.11052602530000e-02,0.10831932375000e-02,&
      &0.10614822761000e-02,0.10401221836000e-02,0.10191127691000e-02,0.99845336120000e-03,0.97813880791000e-03,&
      &0.95816816529000e-03,0.93853846135000e-03,0.91924644692000e-03,0.90028862663000e-03,0.88166169394000e-03,&
      &0.86336388234000e-03,0.84539187195000e-03,0.82774123388000e-03,0.81040820275000e-03,0.79339082737000e-03,&
      &0.77668438588000e-03,0.76028497181000e-03,0.74418912354000e-03,0.72839256782000e-03,0.71289231778000e-03,&
      &0.69768451509000e-03,0.68276448074000e-03,0.66812732602000e-03,0.65376984404000e-03,0.63968863863000e-03,&
      &0.62587936834000e-03,0.61233668175000e-03,0.59905612352000e-03,0.58603502898000e-03,0.57326889435000e-03,&
      &0.56075282587000e-03,0.54848221336000e-03,0.53645352537000e-03,0.52466223664000e-03,0.51310452097000e-03,&
      &0.50177582401000e-03,0.49067084866000e-03,0.47978664497000e-03,0.46911932295000e-03,0.45866397792000e-03,&
      &0.44841545578000e-03,0.43837047412000e-03,0.42852537377000e-03,0.41887589485000e-03,0.40941756056000e-03,&
      &0.40014597474000e-03,0.39105747837000e-03,0.38214885370000e-03,0.37341560944000e-03,0.36485286791000e-03,&
      &0.35645796872000e-03,0.34822718523000e-03,0.34015635493000e-03,0.33224155401000e-03,0.32447895268000e-03,&
      &0.31686577620000e-03,0.30939874273000e-03,0.30207349182000e-03,0.29488618263000e-03,0.28783453141000e-03,&
      &0.28091495475000e-03,0.27412397261000e-03,0.26745864238000e-03,0.26091555188000e-03,0.25449191225000e-03,&
      &0.24818503211000e-03,0.24199189806000e-03,0.23590918280000e-03,0.22993416061000e-03,0.22406495247000e-03,&
      &0.21829868061000e-03,0.21263261530000e-03,0.20706427363000e-03,0.20159158352000e-03,0.19621230229000e-03,&
      &0.19092427025000e-03,0.18572515481000e-03,0.18061276199000e-03,0.17558624901000e-03,0.17064302591000e-03,&
      &0.16578118849000e-03,0.16099951206000e-03,0.15629622950000e-03,0.15167039011000e-03,0.14712042539000e-03,&
      &0.14264467854000e-03,0.13824218318000e-03,0.13391239893000e-03,0.12965489727000e-03,0.12546567201000e-03,&
      &0.12134852623000e-03      /)

      pblast_data%sri(3,1:256) = (/&
      &0.27890752167000e-02,0.27873667001000e-02,0.27850627013000e-02,0.27821679896000e-02,0.27786976087000e-02,&
      &0.27746656848000e-02,0.27700776402000e-02,0.27649480510000e-02,0.27592857071000e-02,0.27531028015000e-02,&
      &0.27464178931000e-02,0.27392339959000e-02,0.27315642812000e-02,0.27234229102000e-02,0.27148211772000e-02,&
      &0.27057715637000e-02,0.26962879353000e-02,0.26863784838000e-02,0.26760544453000e-02,0.26653318823000e-02,&
      &0.26542194992000e-02,0.26427298548000e-02,0.26308751213000e-02,0.26186674089000e-02,0.26061162948000e-02,&
      &0.25932353943000e-02,0.25800352741000e-02,0.25665260018000e-02,0.25527204591000e-02,0.25386292135000e-02,&
      &0.25242623152000e-02,0.25096296910000e-02,0.24947438769000e-02,0.24796142821000e-02,0.24642534066000e-02,&
      &0.24486686651000e-02,0.24328696952000e-02,0.24168701338000e-02,0.24006757071000e-02,0.23842957783000e-02,&
      &0.23677415537000e-02,0.23510239650000e-02,0.23341506181000e-02,0.23171275648000e-02,0.22999638783000e-02,&
      &0.22826711486000e-02,0.22652587231000e-02,0.22477307204000e-02,0.22300956013000e-02,0.22123614986000e-02,&
      &0.21945367064000e-02,0.21766293340000e-02,0.21586454456000e-02,0.21405929416000e-02,0.21224798731000e-02,&
      &0.21043128362000e-02,0.20860953481000e-02,0.20678342503000e-02,0.20495384014000e-02,0.20312144668000e-02,&
      &0.20128669947000e-02,0.19945005334000e-02,0.19761209445000e-02,0.19577369436000e-02,0.19393514731000e-02,&
      &0.19209700390000e-02,0.19025953970000e-02,0.18842338313000e-02,0.18658936646000e-02,0.18475786746000e-02,&
      &0.18292899231000e-02,0.18110316196000e-02,0.17928106540000e-02,0.17746323988000e-02,0.17564990104000e-02,&
      &0.17384142884000e-02,0.17203803977000e-02,0.17024018353000e-02,0.16844851712000e-02,0.16666309665000e-02,&
      &0.16488428518000e-02,0.16311239129000e-02,0.16134773374000e-02,0.15959075347000e-02,0.15784149718000e-02,&
      &0.15610048142000e-02,0.15436802387000e-02,0.15264404161000e-02,0.15092888166000e-02,0.14922295642000e-02,&
      &0.14752647697000e-02,0.14583957097000e-02,0.14416251276000e-02,0.14249553317000e-02,0.14083865214000e-02,&
      &0.13919218890000e-02,0.13755658209000e-02,0.13593163783000e-02,0.13431759399000e-02,0.13271456671000e-02,&
      &0.13112273616000e-02,0.12954247593000e-02,0.12797379870000e-02,0.12641670515000e-02,0.12487131283000e-02,&
      &0.12333784524000e-02,0.12181643009000e-02,0.12030720640000e-02,0.11881012333000e-02,0.11732530647000e-02,&
      &0.11585304567000e-02,0.11439325584000e-02,0.11294588810000e-02,0.11151117040000e-02,0.11008927513000e-02,&
      &0.10868003963000e-02,0.10728353819000e-02,0.10589990692000e-02,0.10452918273000e-02,0.10317135212000e-02,&
      &0.10182653379000e-02,0.10049466899000e-02,0.99175804707000e-03,0.97870006090000e-03,0.96577144776000e-03,&
      &0.95297301190000e-03,0.94030591514000e-03,0.92776971984000e-03,0.91536338932000e-03,0.90308710435000e-03,&
      &0.89094109209000e-03,0.87892501422000e-03,0.86703848354000e-03,0.85528167083000e-03,0.84365405464000e-03,&
      &0.83215476783000e-03,0.82078398245000e-03,0.80954179275000e-03,0.79842663013000e-03,0.78743904692000e-03,&
      &0.77657791084000e-03,0.76584199101000e-03,0.75523205608000e-03,0.74474691459000e-03,0.73438603842000e-03,&
      &0.72414831139000e-03,0.71403258388000e-03,0.70403931861000e-03,0.69416756143000e-03,0.68441586346000e-03,&
      &0.67478334790000e-03,0.66526947344000e-03,0.65587310280000e-03,0.64659386598000e-03,0.63743078371000e-03,&
      &0.62838193466000e-03,0.61944617556000e-03,0.61062374663000e-03,0.60191348429000e-03,0.59331345475000e-03,&
      &0.58482277304000e-03,0.57644021056000e-03,0.56816506357000e-03,0.55999604149000e-03,0.55193148800000e-03,&
      &0.54397072883000e-03,0.53611263913000e-03,0.52835566550000e-03,0.52069816867000e-03,0.51313892114000e-03,&
      &0.50567757955000e-03,0.49831224837000e-03,0.49104123155000e-03,0.48386342612000e-03,0.47677747398000e-03,&
      &0.46978235207000e-03,0.46287604561000e-03,0.45605719024000e-03,0.44932487068000e-03,0.44267741243000e-03,&
      &0.43611353186000e-03,0.42963154476000e-03,0.42322978627000e-03,0.41690720746000e-03,0.41066283244000e-03,&
      &0.40449432888000e-03,0.39839993691000e-03,0.39237871944000e-03,0.38642937845000e-03,0.38055038197000e-03,&
      &0.37473982846000e-03,0.36899616751000e-03,0.36331824949000e-03,0.35770475448000e-03,0.35215394234000e-03,&
      &0.34666415596000e-03,0.34123388778000e-03,0.33586205994000e-03,0.33054671592000e-03,0.32528664764000e-03,&
      &0.32008047725000e-03,0.31492619027000e-03,0.30982308192000e-03,0.30476941128000e-03,0.29976377743000e-03,&
      &0.29480483713000e-03,0.28989093164000e-03,0.28502116791000e-03,0.28019360292000e-03,0.27540698511000e-03,&
      &0.27066073235000e-03,0.26595314593000e-03,0.26128284462000e-03,0.25664865006000e-03,0.25204940314000e-03,&
      &0.24748419088000e-03,0.24295203190000e-03,0.23845136095000e-03,0.23398111483000e-03,0.22954069309000e-03,&
      &0.22512935774000e-03,0.22074597318000e-03,0.21638952604000e-03,0.21205929262000e-03,0.20775464620000e-03,&
      &0.20347515459000e-03,0.19922008834000e-03,0.19498885776000e-03,0.19078106301000e-03,0.18659622890000e-03,&
      &0.18243440885000e-03,0.17829533906000e-03,0.17417858315000e-03,0.17008419932000e-03,0.16601237194000e-03,&
      &0.16196311059000e-03,0.15793655703000e-03,0.15393323548000e-03,0.14995360783000e-03,0.14599799877000e-03,&
      &0.14206689548000e-03,0.13816121425000e-03,0.13428195465000e-03,0.13042971184000e-03,0.12660538766000e-03,&
      &0.12281027076000e-03,0.11904580028000e-03,0.11531294842000e-03,0.11161300214000e-03,0.10794767979000e-03,&
      &0.10431865457000e-03,0.10072748748000e-03,0.97175529256000e-04,0.93663531679000e-04,0.90200062083000e-04,&
      &0.86775413623000e-04      /)

      pblast_data%sri(4,1:256) = (/&
      &0.80441405932000e-03,0.80335096600000e-03,0.80221820404000e-03,0.80101546694000e-03,0.79974357576000e-03,&
      &0.79840389682000e-03,0.79699636976000e-03,0.79552189841000e-03,0.79398135157000e-03,0.79237549721000e-03,&
      &0.79070524001000e-03,0.78897105763000e-03,0.78717392532000e-03,0.78531495173000e-03,0.78339502379000e-03,&
      &0.78141485174000e-03,0.77937483603000e-03,0.77727669045000e-03,0.77512169006000e-03,0.77290980962000e-03,&
      &0.77064266205000e-03,0.76832092758000e-03,0.76594541635000e-03,0.76351775885000e-03,0.76103891909000e-03,&
      &0.75850982924000e-03,0.75593099785000e-03,0.75330370921000e-03,0.75062966205000e-03,0.74790908624000e-03,&
      &0.74514356159000e-03,0.74233412083000e-03,0.73948161494000e-03,0.73658772666000e-03,0.73365236297000e-03,&
      &0.73067720771000e-03,0.72766405365000e-03,0.72461325408000e-03,0.72152543380000e-03,0.71840235381000e-03,&
      &0.71524525408000e-03,0.71205487152000e-03,0.70883235652000e-03,0.70557868702000e-03,0.70229469189000e-03,&
      &0.69898154835000e-03,0.69564071278000e-03,0.69227294591000e-03,0.68887937266000e-03,0.68546079838000e-03,&
      &0.68201830233000e-03,0.67855318908000e-03,0.67506648539000e-03,0.67155866472000e-03,0.66803076844000e-03,&
      &0.66448435833000e-03,0.66091996460000e-03,0.65733871688000e-03,0.65374129507000e-03,0.65012872386000e-03,&
      &0.64650239031000e-03,0.64286267444000e-03,0.63921065391000e-03,0.63554736423000e-03,0.63187355620000e-03,&
      &0.62818982357000e-03,0.62449753530000e-03,0.62079741462000e-03,0.61709004615000e-03,0.61337648576000e-03,&
      &0.60965775786000e-03,0.60593416555000e-03,0.60220638896000e-03,0.59847567342000e-03,0.59474284099000e-03,&
      &0.59100797129000e-03,0.58727241388000e-03,0.58353667989000e-03,0.57980170288000e-03,0.57606772984000e-03,&
      &0.57233549160000e-03,0.56860599195000e-03,0.56487983440000e-03,0.56115748660000e-03,0.55743945551000e-03,&
      &0.55372650730000e-03,0.55001944434000e-03,0.54631872263000e-03,0.54262437688000e-03,0.53893734376000e-03,&
      &0.53525848257000e-03,0.53158805982000e-03,0.52792650897000e-03,0.52427424749000e-03,0.52063170767000e-03,&
      &0.51699959270000e-03,0.51337855977000e-03,0.50976859244000e-03,0.50617009938000e-03,0.50258353927000e-03,&
      &0.49900946298000e-03,0.49544840487000e-03,0.49190030357000e-03,0.48836557497000e-03,0.48484471470000e-03,&
      &0.48133799698000e-03,0.47784577085000e-03,0.47436830697000e-03,0.47090570833000e-03,0.46745817164000e-03,&
      &0.46402608570000e-03,0.46061003293000e-03,0.45720973130000e-03,0.45382522956000e-03,0.45045717786000e-03,&
      &0.44710555962000e-03,0.44377039311000e-03,0.44045204363000e-03,0.43715064307000e-03,0.43386598220000e-03,&
      &0.43059844284000e-03,0.42734787467000e-03,0.42411432537000e-03,0.42089804529000e-03,0.41769889780000e-03,&
      &0.41451699849000e-03,0.41135239358000e-03,0.40820492469000e-03,0.40507448587000e-03,0.40196124515000e-03,&
      &0.39886497591000e-03,0.39578573405000e-03,0.39272347396000e-03,0.38967770312000e-03,0.38664870403000e-03,&
      &0.38363620056000e-03,0.38063990606000e-03,0.37765982229000e-03,0.37469556157000e-03,0.37174728471000e-03,&
      &0.36881449616000e-03,0.36589675083000e-03,0.36299430107000e-03,0.36010659415000e-03,0.35723343567000e-03,&
      &0.35437449630000e-03,0.35152925188000e-03,0.34869757661000e-03,0.34587938557000e-03,0.34307410252000e-03,&
      &0.34028112202000e-03,0.33750024311000e-03,0.33473113793000e-03,0.33197335834000e-03,0.32922644161000e-03,&
      &0.32648991361000e-03,0.32376338553000e-03,0.32104633873000e-03,0.31833833111000e-03,0.31563880383000e-03,&
      &0.31294713342000e-03,0.31026312613000e-03,0.30758609504000e-03,0.30491529863000e-03,0.30225022909000e-03,&
      &0.29959052812000e-03,0.29693533096000e-03,0.29428427342000e-03,0.29163679604000e-03,0.28899197170000e-03,&
      &0.28634902800000e-03,0.28370830473000e-03,0.28106804749000e-03,0.27842782293000e-03,0.27578735336000e-03,&
      &0.27314594164000e-03,0.27050267055000e-03,0.26785697671000e-03,0.26520813946000e-03,0.26255553569000e-03,&
      &0.25989833502000e-03,0.25723599901000e-03,0.25456792162000e-03,0.25189311876000e-03,0.24921128946000e-03,&
      &0.24652162497000e-03,0.24382327242000e-03,0.24111567048000e-03,0.23839826640000e-03,0.23567052408000e-03,&
      &0.23293159466000e-03,0.23018089088000e-03,0.22741820808000e-03,0.22464252862000e-03,0.22185348147000e-03,&
      &0.21905062619000e-03,0.21623319901000e-03,0.21340117525000e-03,0.21055385455000e-03,0.20769096630000e-03,&
      &0.20481217780000e-03,0.20191682777000e-03,0.19900519911000e-03,0.19607675634000e-03,0.19313127225000e-03,&
      &0.19016891333000e-03,0.18718938183000e-03,0.18419271939000e-03,0.18117895315000e-03,0.17814831197000e-03,&
      &0.17510113316000e-03,0.17203743528000e-03,0.16895756071000e-03,0.16586193042000e-03,0.16275111719000e-03,&
      &0.15962580409000e-03,0.15648648567000e-03,0.15333400926000e-03,0.15016921789000e-03,0.14699299164000e-03,&
      &0.14380617186000e-03,0.14061010330000e-03,0.13740598100000e-03,0.13419512666000e-03,0.13097899811000e-03,&
      &0.12775869434000e-03,0.12453640703000e-03,0.12131359157000e-03,0.11809177231000e-03,0.11487318956000e-03,&
      &0.11165955996000e-03,0.10845294694000e-03,0.10525577068000e-03,0.10207027578000e-03,0.98898251203000e-04,&
      &0.95742271106000e-04,0.92604961678000e-04,0.89488946430000e-04,0.86396464530000e-04,0.83329883906000e-04,&
      &0.80292099679000e-04,0.77286009835000e-04,0.74314099521000e-04,0.71378685298000e-04,0.68482564855000e-04,&
      &0.65628565825000e-04,0.62819566454000e-04,0.60057673706000e-04,0.57345305684000e-04,0.54685153353000e-04,&
      &0.52079780844000e-04,0.49531547658000e-04,0.47042137400000e-04,0.44612512376000e-04,0.42251285028000e-04,&
      &0.39948943854000e-04      /)

      pblast_data%sri(5,1:256) = (/&
      &0.44519588514000e-03,0.44440892437000e-03,0.44359865789000e-03,0.44276577357000e-03,0.44191124326000e-03,&
      &0.44103569211000e-03,0.44013994760000e-03,0.43922457321000e-03,0.43829044690000e-03,0.43733839445000e-03,&
      &0.43636875512000e-03,0.43538214597000e-03,0.43437948130000e-03,0.43336132231000e-03,0.43232804514000e-03,&
      &0.43128013124000e-03,0.43021850049000e-03,0.42914352473000e-03,0.42805561149000e-03,0.42695557691000e-03,&
      &0.42584331444000e-03,0.42471963081000e-03,0.42358502463000e-03,0.42243954837000e-03,0.42128363414000e-03,&
      &0.42011812651000e-03,0.41894284587000e-03,0.41775799582000e-03,0.41656461364000e-03,0.41536248142000e-03,&
      &0.41415186221000e-03,0.41293290587000e-03,0.41170588439000e-03,0.41047140199000e-03,0.40922937139000e-03,&
      &0.40797969694000e-03,0.40672279184000e-03,0.40545901207000e-03,0.40418804360000e-03,0.40291031404000e-03,&
      &0.40162586577000e-03,0.40033452174000e-03,0.39903664490000e-03,0.39773224805000e-03,0.39642118701000e-03,&
      &0.39510363393000e-03,0.39377976592000e-03,0.39244904979000e-03,0.39111191950000e-03,0.38976833432000e-03,&
      &0.38841798076000e-03,0.38706119225000e-03,0.38569749355000e-03,0.38432713594000e-03,0.38295015672000e-03,&
      &0.38156608625000e-03,0.38017510238000e-03,0.37877709987000e-03,0.37737187721000e-03,0.37595937096000e-03,&
      &0.37453959678000e-03,0.37311240905000e-03,0.37167756064000e-03,0.37023499220000e-03,0.36878470309000e-03,&
      &0.36732653437000e-03,0.36586014135000e-03,0.36438554080000e-03,0.36290278611000e-03,0.36141157212000e-03,&
      &0.35991172186000e-03,0.35840331122000e-03,0.35688623766000e-03,0.35536016042000e-03,0.35382499843000e-03,&
      &0.35228088961000e-03,0.35072753011000e-03,0.34916476532000e-03,0.34759279661000e-03,0.34601114811000e-03,&
      &0.34441995171000e-03,0.34281916270000e-03,0.34120854651000e-03,0.33958826325000e-03,0.33795832455000e-03,&
      &0.33631829158000e-03,0.33466812850000e-03,0.33300825484000e-03,0.33133869313000e-03,0.32965888253000e-03,&
      &0.32796895779000e-03,0.32626933379000e-03,0.32455966746000e-03,0.32284005130000e-03,0.32111069390000e-03,&
      &0.31937142838000e-03,0.31762217517000e-03,0.31586354011000e-03,0.31409539979000e-03,0.31231755349000e-03,&
      &0.31053023308000e-03,0.30873371155000e-03,0.30692807063000e-03,0.30511346324000e-03,0.30328999242000e-03,&
      &0.30145776314000e-03,0.29961688736000e-03,0.29776784077000e-03,0.29591072278000e-03,0.29404557064000e-03,&
      &0.29217277194000e-03,0.29029249915000e-03,0.28840484688000e-03,0.28651022463000e-03,0.28460903915000e-03,&
      &0.28270125867000e-03,0.28078716080000e-03,0.27886729237000e-03,0.27694182504000e-03,0.27501080447000e-03,&
      &0.27307490876000e-03,0.27113429695000e-03,0.26918920939000e-03,0.26724011102000e-03,0.26528720444000e-03,&
      &0.26333098095000e-03,0.26137159442000e-03,0.25940941954000e-03,0.25744500928000e-03,0.25547842017000e-03,&
      &0.25351009135000e-03,0.25154047346000e-03,0.24956986956000e-03,0.24759851186000e-03,0.24562700344000e-03,&
      &0.24365551891000e-03,0.24168419880000e-03,0.23971363968000e-03,0.23774426954000e-03,0.23577634324000e-03,&
      &0.23380996733000e-03,0.23184557125000e-03,0.22988370844000e-03,0.22792439554000e-03,0.22596812933000e-03,&
      &0.22401525386000e-03,0.22206580984000e-03,0.22012028201000e-03,0.21817882543000e-03,0.21624169468000e-03,&
      &0.21430921700000e-03,0.21238156932000e-03,0.21045905148000e-03,0.20854181104000e-03,0.20663006621000e-03,&
      &0.20472406708000e-03,0.20282376365000e-03,0.20092945963000e-03,0.19904143475000e-03,0.19715969942000e-03,&
      &0.19528425311000e-03,0.19341519452000e-03,0.19155278000000e-03,0.18969700197000e-03,0.18784777764000e-03,&
      &0.18600530220000e-03,0.18416941242000e-03,0.18234007609000e-03,0.18051738790000e-03,0.17870129498000e-03,&
      &0.17689167035000e-03,0.17508842353000e-03,0.17329148054000e-03,0.17150062162000e-03,0.16971558315000e-03,&
      &0.16793639663000e-03,0.16616283704000e-03,0.16439465087000e-03,0.16263164590000e-03,0.16087350197000e-03,&
      &0.15911995600000e-03,0.15737074115000e-03,0.15562546664000e-03,0.15388374892000e-03,0.15214547807000e-03,&
      &0.15041007475000e-03,0.14867713715000e-03,0.14694638731000e-03,0.14521741950000e-03,0.14348968687000e-03,&
      &0.14176256401000e-03,0.14003566253000e-03,0.13830882988000e-03,0.13658136923000e-03,0.13485269942000e-03,&
      &0.13312233308000e-03,0.13138964299000e-03,0.12965428911000e-03,0.12791569362000e-03,0.12617323851000e-03,&
      &0.12442640280000e-03,0.12267465837000e-03,0.12091756908000e-03,0.11915450022000e-03,0.11738496501000e-03,&
      &0.11560845954000e-03,0.11382434968000e-03,0.11203242657000e-03,0.11023216533000e-03,0.10842298611000e-03,&
      &0.10660461196000e-03,0.10477674643000e-03,0.10293886108000e-03,0.10109069130000e-03,0.99232131964000e-04,&
      &0.97362896012000e-04,0.95482975593000e-04,0.93592070970000e-04,0.91690141537000e-04,0.89777494814000e-04,&
      &0.87854152000000e-04,0.85920159730000e-04,0.83975815042000e-04,0.82021632926000e-04,0.80058049965000e-04,&
      &0.78085426219000e-04,0.76104424552000e-04,0.74115928086000e-04,0.72120881169000e-04,0.70119988116000e-04,&
      &0.68114326104000e-04,0.66105095160000e-04,0.64093955609000e-04,0.62081793676000e-04,0.60070633602000e-04,&
      &0.58061463284000e-04,0.56056463342000e-04,0.54057715062000e-04,0.52066753767000e-04,0.50085581247000e-04,&
      &0.48116507674000e-04,0.46161941590000e-04,0.44224291423000e-04,0.42305458503000e-04,0.40407859761000e-04,&
      &0.38534270002000e-04,0.36687585087000e-04,0.34869671227000e-04,0.33083055041000e-04,0.31330443816000e-04,&
      &0.29614451413000e-04,0.27937667929000e-04,0.26301803628000e-04,0.24707651489000e-04,0.23165465029000e-04,&
      &0.21663709048000e-04      /)

      pblast_data%sri(6,1:256) = (/&
      &0.22167698836000e-03,0.22158620351000e-03,0.22148004090000e-03,0.22135920461000e-03,0.22122453132000e-03,&
      &0.22107657553000e-03,0.22091609197000e-03,0.22074370872000e-03,0.22056013172000e-03,0.22036599907000e-03,&
      &0.22016194722000e-03,0.21994853738000e-03,0.21972642119000e-03,0.21949609525000e-03,0.21925811239000e-03,&
      &0.21901309996000e-03,0.21876142033000e-03,0.21850351437000e-03,0.21824006079000e-03,0.21797143263000e-03,&
      &0.21769786185000e-03,0.21741979859000e-03,0.21713764413000e-03,0.21685159548000e-03,0.21656205742000e-03,&
      &0.21626947252000e-03,0.21597385757000e-03,0.21567559223000e-03,0.21537481748000e-03,0.21507168600000e-03,&
      &0.21476642979000e-03,0.21445900066000e-03,0.21414977469000e-03,0.21383892596000e-03,0.21352626026000e-03,&
      &0.21321178813000e-03,0.21289594512000e-03,0.21257830378000e-03,0.21225899713000e-03,0.21193820958000e-03,&
      &0.21161558024000e-03,0.21129117837000e-03,0.21096503641000e-03,0.21063683815000e-03,0.21030647738000e-03,&
      &0.20997413080000e-03,0.20963939549000e-03,0.20930216843000e-03,0.20896236665000e-03,0.20861967205000e-03,&
      &0.20827377087000e-03,0.20792470500000e-03,0.20757225760000e-03,0.20721601508000e-03,0.20685576053000e-03,&
      &0.20649141939000e-03,0.20612261128000e-03,0.20574909045000e-03,0.20537062159000e-03,0.20498686043000e-03,&
      &0.20459749141000e-03,0.20420241760000e-03,0.20380127554000e-03,0.20339371611000e-03,0.20297945607000e-03,&
      &0.20255816210000e-03,0.20212959778000e-03,0.20169357937000e-03,0.20124968906000e-03,0.20079757565000e-03,&
      &0.20033715263000e-03,0.19986799682000e-03,0.19938983808000e-03,0.19890245260000e-03,0.19840549224000e-03,&
      &0.19789864815000e-03,0.19738188232000e-03,0.19685479608000e-03,0.19631715710000e-03,0.19576877279000e-03,&
      &0.19520923785000e-03,0.19463861592000e-03,0.19405661503000e-03,0.19346288832000e-03,0.19285736316000e-03,&
      &0.19223999750000e-03,0.19161052499000e-03,0.19096870041000e-03,0.19031440021000e-03,0.18964754903000e-03,&
      &0.18896805972000e-03,0.18827579744000e-03,0.18757091761000e-03,0.18685311954000e-03,0.18612213763000e-03,&
      &0.18537834247000e-03,0.18462153832000e-03,0.18385166423000e-03,0.18306877461000e-03,0.18227292811000e-03,&
      &0.18146415823000e-03,0.18064250416000e-03,0.17980778585000e-03,0.17896029268000e-03,0.17810033177000e-03,&
      &0.17722765146000e-03,0.17634240100000e-03,0.17544496412000e-03,0.17453538606000e-03,0.17361366894000e-03,&
      &0.17268022245000e-03,0.17173492548000e-03,0.17077811269000e-03,0.16981008179000e-03,0.16883097431000e-03,&
      &0.16784115492000e-03,0.16684053570000e-03,0.16582957820000e-03,0.16480858772000e-03,0.16377765968000e-03,&
      &0.16273727086000e-03,0.16168745953000e-03,0.16062861562000e-03,0.15956111945000e-03,0.15848518382000e-03,&
      &0.15740113433000e-03,0.15630912704000e-03,0.15520957338000e-03,0.15410287828000e-03,0.15298928458000e-03,&
      &0.15186903352000e-03,0.15074246864000e-03,0.14960998454000e-03,0.14847183571000e-03,0.14732828474000e-03,&
      &0.14617966101000e-03,0.14502639320000e-03,0.14386859721000e-03,0.14270659313000e-03,0.14154087037000e-03,&
      &0.14037150869000e-03,0.13919878824000e-03,0.13802315003000e-03,0.13684489422000e-03,0.13566416435000e-03,&
      &0.13448109641000e-03,0.13329609503000e-03,0.13210937419000e-03,0.13092120159000e-03,0.12973189322000e-03,&
      &0.12854154715000e-03,0.12735021307000e-03,0.12615830130000e-03,0.12496594446000e-03,0.12377323226000e-03,&
      &0.12258045128000e-03,0.12138766975000e-03,0.12019497589000e-03,0.11900285465000e-03,0.11781109066000e-03,&
      &0.11661970546000e-03,0.11542889979000e-03,0.11423888826000e-03,0.11304970270000e-03,0.11186137776000e-03,&
      &0.11067390131000e-03,0.10948733215000e-03,0.10830165316000e-03,0.10711695228000e-03,0.10593335222000e-03,&
      &0.10475057982000e-03,0.10356883186000e-03,0.10238799312000e-03,0.10120797181000e-03,0.10002894186000e-03,&
      &0.98850609805000e-04,0.97672924688000e-04,0.96495995504000e-04,0.95319661920000e-04,0.94143698905000e-04,&
      &0.92968121869000e-04,0.91793006705000e-04,0.90618029172000e-04,0.89443075868000e-04,0.88268108773000e-04,&
      &0.87093046226000e-04,0.85917783010000e-04,0.84742237062000e-04,0.83566277986000e-04,0.82389608199000e-04,&
      &0.81212260685000e-04,0.80034144739000e-04,0.78855171329000e-04,0.77675142572000e-04,0.76493835472000e-04,&
      &0.75311644613000e-04,0.74128210733000e-04,0.72943294293000e-04,0.71757090264000e-04,0.70569415918000e-04,&
      &0.69380329822000e-04,0.68189680149000e-04,0.66997635103000e-04,0.65804245612000e-04,0.64609181335000e-04,&
      &0.63412841826000e-04,0.62215334615000e-04,0.61016610488000e-04,0.59816694864000e-04,0.58615723341000e-04,&
      &0.57414361341000e-04,0.56212047726000e-04,0.55009489628000e-04,0.53806954567000e-04,0.52604694008000e-04,&
      &0.51402656473000e-04,0.50201373461000e-04,0.49001573060000e-04,0.47803040546000e-04,0.46606585936000e-04,&
      &0.45412786591000e-04,0.44221960302000e-04,0.43034497309000e-04,0.41851203609000e-04,0.40672540850000e-04,&
      &0.39498817374000e-04,0.38331028867000e-04,0.37170162963000e-04,0.36016546490000e-04,0.34870649096000e-04,&
      &0.33733490675000e-04,0.32605857744000e-04,0.31488481535000e-04,0.30382309542000e-04,0.29287834088000e-04,&
      &0.28206021398000e-04,0.27137709111000e-04,0.26083616979000e-04,0.25044693292000e-04,0.24021738716000e-04,&
      &0.23015334243000e-04,0.22026720446000e-04,0.21056493168000e-04,0.20104992605000e-04,0.19173700512000e-04,&
      &0.18262826602000e-04,0.17372941714000e-04,0.16505025238000e-04,0.15659823271000e-04,0.14838520312000e-04,&
      &0.14031629474000e-04,0.13230498925000e-04,0.12426484918000e-04,0.11610943709000e-04,0.10775231551000e-04,&
      &0.99107046987000e-05      /)

      pblast_data%sri(7,1:256) = (/&
      &0.15372630763000e-03,0.15387384048000e-03,0.15401465070000e-03,0.15414878718000e-03,0.15427643961000e-03,&
      &0.15439768328000e-03,0.15451244248000e-03,0.15462086963000e-03,0.15472308393000e-03,0.15481917460000e-03,&
      &0.15490912582000e-03,0.15499289580000e-03,0.15507057157000e-03,0.15514234494000e-03,0.15520820301000e-03,&
      &0.15526813954000e-03,0.15532226473000e-03,0.15537048796000e-03,0.15541292028000e-03,0.15544958592000e-03,&
      &0.15548051809000e-03,0.15550572612000e-03,0.15552507968000e-03,0.15553884420000e-03,0.15554689769000e-03,&
      &0.15554923225000e-03,0.15554587921000e-03,0.15553660583000e-03,0.15552180225000e-03,0.15550135199000e-03,&
      &0.15547502854000e-03,0.15544291785000e-03,0.15540510814000e-03,0.15536148623000e-03,0.15531194423000e-03,&
      &0.15525663244000e-03,0.15519546513000e-03,0.15512819858000e-03,0.15505507430000e-03,0.15497592402000e-03,&
      &0.15489069214000e-03,0.15479938673000e-03,0.15470194569000e-03,0.15459834025000e-03,0.15448829115000e-03,&
      &0.15437201432000e-03,0.15424948122000e-03,0.15412040326000e-03,0.15398485920000e-03,0.15384281600000e-03,&
      &0.15369408644000e-03,0.15353867432000e-03,0.15337658807000e-03,0.15320763734000e-03,0.15303182052000e-03,&
      &0.15284901317000e-03,0.15265921264000e-03,0.15246236797000e-03,0.15225829551000e-03,0.15204692313000e-03,&
      &0.15182826966000e-03,0.15160223785000e-03,0.15136879739000e-03,0.15112790531000e-03,0.15087933789000e-03,&
      &0.15062313316000e-03,0.15035920625000e-03,0.15008747907000e-03,0.14980788978000e-03,0.14952033750000e-03,&
      &0.14922486598000e-03,0.14892131725000e-03,0.14860961830000e-03,0.14828981099000e-03,0.14796179573000e-03,&
      &0.14762531544000e-03,0.14728072177000e-03,0.14692779771000e-03,0.14656629311000e-03,0.14619639945000e-03,&
      &0.14581795851000e-03,0.14543098629000e-03,0.14503545758000e-03,0.14463128834000e-03,0.14421847599000e-03,&
      &0.14379708387000e-03,0.14336691519000e-03,0.14292806287000e-03,0.14248052264000e-03,0.14202419070000e-03,&
      &0.14155915834000e-03,0.14108549913000e-03,0.14060303702000e-03,0.14011177727000e-03,0.13961182572000e-03,&
      &0.13910316588000e-03,0.13858592528000e-03,0.13805999827000e-03,0.13752535186000e-03,0.13698210204000e-03,&
      &0.13643034546000e-03,0.13587012716000e-03,0.13530128689000e-03,0.13472400774000e-03,0.13413835893000e-03,&
      &0.13354432042000e-03,0.13294213574000e-03,0.13233161066000e-03,0.13171299387000e-03,0.13108638373000e-03,&
      &0.13045170310000e-03,0.12980910343000e-03,0.12915865580000e-03,0.12850045447000e-03,0.12783461532000e-03,&
      &0.12716120185000e-03,0.12648027551000e-03,0.12579198280000e-03,0.12509641780000e-03,0.12439367893000e-03,&
      &0.12368385769000e-03,0.12296707125000e-03,0.12224345681000e-03,0.12151312728000e-03,0.12077606120000e-03,&
      &0.12003251422000e-03,0.11928270976000e-03,0.11852653252000e-03,0.11776418481000e-03,0.11699585916000e-03,&
      &0.11622161672000e-03,0.11544151811000e-03,0.11465574624000e-03,0.11386448986000e-03,0.11306777067000e-03,&
      &0.11226577743000e-03,0.11145861685000e-03,0.11064629711000e-03,0.10982910856000e-03,0.10900699066000e-03,&
      &0.10818015306000e-03,0.10734879030000e-03,0.10651290433000e-03,0.10567260359000e-03,0.10482804481000e-03,&
      &0.10397925639000e-03,0.10312640139000e-03,0.10226963144000e-03,0.10140890953000e-03,0.10054446497000e-03,&
      &0.99676314567000e-04,0.98804491176000e-04,0.97929116916000e-04,0.97050290389000e-04,0.96168184900000e-04,&
      &0.95282658047000e-04,0.94393814174000e-04,0.93501863952000e-04,0.92606790486000e-04,0.91708584843000e-04,&
      &0.90807361667000e-04,0.89903134361000e-04,0.88995917590000e-04,0.88085800738000e-04,0.87172813342000e-04,&
      &0.86256930531000e-04,0.85338186266000e-04,0.84416605812000e-04,0.83492194875000e-04,0.82564979588000e-04,&
      &0.81634925794000e-04,0.80702077555000e-04,0.79766403351000e-04,0.78827874835000e-04,0.77886531709000e-04,&
      &0.76942307559000e-04,0.75995252198000e-04,0.75045341405000e-04,0.74092481589000e-04,0.73136671388000e-04,&
      &0.72177975434000e-04,0.71216300111000e-04,0.70251625446000e-04,0.69283932823000e-04,0.68313188158000e-04,&
      &0.67339430684000e-04,0.66362593473000e-04,0.65382707493000e-04,0.64399719245000e-04,0.63413602140000e-04,&
      &0.62424457758000e-04,0.61432187859000e-04,0.60436875647000e-04,0.59438534836000e-04,0.58437162913000e-04,&
      &0.57432855844000e-04,0.56425632234000e-04,0.55415560749000e-04,0.54402714501000e-04,0.53387188490000e-04,&
      &0.52369109722000e-04,0.51348588527000e-04,0.50325781126000e-04,0.49300814438000e-04,0.48273865616000e-04,&
      &0.47245177679000e-04,0.46214967453000e-04,0.45183464487000e-04,0.44150901905000e-04,0.43117574303000e-04,&
      &0.42083915042000e-04,0.41050142827000e-04,0.40016702036000e-04,0.38983902355000e-04,0.37952214165000e-04,&
      &0.36922129327000e-04,0.35894141354000e-04,0.34868701305000e-04,0.33846343177000e-04,0.32827653855000e-04,&
      &0.31813258650000e-04,0.30803833322000e-04,0.29799963464000e-04,0.28802290575000e-04,0.27811516515000e-04,&
      &0.26828416538000e-04,0.25853828135000e-04,0.24888461884000e-04,0.23932980882000e-04,0.22988242972000e-04,&
      &0.22055110254000e-04,0.21134432792000e-04,0.20227067257000e-04,0.19333707974000e-04,0.18455202579000e-04,&
      &0.17592413320000e-04,0.16746202461000e-04,0.15917407879000e-04,0.15106666557000e-04,0.14314772288000e-04,&
      &0.13542486275000e-04,0.12790517818000e-04,0.12059565974000e-04,0.11350163053000e-04,0.10662458156000e-04,&
      &0.99992035761000e-05,0.93549543184000e-05,0.87210500603000e-05,0.80892432570000e-05,0.74512863637000e-05,&
      &0.67989318356000e-05,0.61239321279000e-05,0.54180396959000e-05,0.46730069947000e-05,0.38805864795000e-05,&
      &0.30325306055000e-05      /)

      pblast_data%sri(8,1:256) = (/&
      &0.12065146179000e-03,0.12068430490000e-03,0.12071946123000e-03,0.12075631997000e-03,0.12079446569000e-03,&
      &0.12083351239000e-03,0.12087297904000e-03,0.12091238107000e-03,0.12095135275000e-03,0.12098938737000e-03,&
      &0.12102622767000e-03,0.12106141904000e-03,0.12109464343000e-03,0.12112553717000e-03,0.12115371722000e-03,&
      &0.12117884146000e-03,0.12120065798000e-03,0.12121887183000e-03,0.12123302259000e-03,0.12124298809000e-03,&
      &0.12124851109000e-03,0.12124927088000e-03,0.12124503833000e-03,0.12123555288000e-03,0.12122043761000e-03,&
      &0.12119972528000e-03,0.12117317818000e-03,0.12114043628000e-03,0.12110142107000e-03,0.12105597990000e-03,&
      &0.12100385229000e-03,0.12094492547000e-03,0.12087902435000e-03,0.12080599020000e-03,0.12072578526000e-03,&
      &0.12063817927000e-03,0.12054317541000e-03,0.12044050811000e-03,0.12033007073000e-03,0.12021192903000e-03,&
      &0.12008587936000e-03,0.11995190823000e-03,0.11980991358000e-03,0.11965979765000e-03,0.11950157766000e-03,&
      &0.11933520461000e-03,0.11916044435000e-03,0.11897743570000e-03,0.11878612418000e-03,0.11858639400000e-03,&
      &0.11837835528000e-03,0.11816207369000e-03,0.11793721732000e-03,0.11770400604000e-03,0.11746246443000e-03,&
      &0.11721244515000e-03,0.11695402782000e-03,0.11668731413000e-03,0.11641225069000e-03,0.11612877089000e-03,&
      &0.11583704050000e-03,0.11553711529000e-03,0.11522886734000e-03,0.11491244557000e-03,0.11458782428000e-03,&
      &0.11425508067000e-03,0.11391432380000e-03,0.11356556238000e-03,0.11320880208000e-03,0.11284413540000e-03,&
      &0.11247169373000e-03,0.11209148759000e-03,0.11170355077000e-03,0.11130800183000e-03,0.11090487226000e-03,&
      &0.11049424913000e-03,0.11007627173000e-03,0.10965094786000e-03,0.10921832484000e-03,0.10877857903000e-03,&
      &0.10833172622000e-03,0.10787786572000e-03,0.10741708384000e-03,0.10694955297000e-03,0.10647535531000e-03,&
      &0.10599441477000e-03,0.10550702687000e-03,0.10501322381000e-03,0.10451302020000e-03,0.10400657965000e-03,&
      &0.10349405183000e-03,0.10297551446000e-03,0.10245106832000e-03,0.10192079887000e-03,0.10138486207000e-03,&
      &0.10084337830000e-03,0.10029638685000e-03,0.99744044118000e-04,0.99186438263000e-04,0.98623731419000e-04,&
      &0.98056023832000e-04,0.97483369499000e-04,0.96905934845000e-04,0.96323862463000e-04,0.95737249254000e-04,&
      &0.95146250407000e-04,0.94550972018000e-04,0.93951472176000e-04,0.93347965006000e-04,0.92740534074000e-04,&
      &0.92129346034000e-04,0.91514480087000e-04,0.90896023841000e-04,0.90274201771000e-04,0.89649050364000e-04,&
      &0.89020733196000e-04,0.88389407245000e-04,0.87755186413000e-04,0.87118200579000e-04,0.86478576134000e-04,&
      &0.85836426833000e-04,0.85191865532000e-04,0.84545048011000e-04,0.83896109739000e-04,0.83245149274000e-04,&
      &0.82592319192000e-04,0.81937712594000e-04,0.81281470539000e-04,0.80623708969000e-04,0.79964564830000e-04,&
      &0.79304227614000e-04,0.78642683550000e-04,0.77980118090000e-04,0.77316685870000e-04,0.76652447305000e-04,&
      &0.75987521529000e-04,0.75322073379000e-04,0.74656157094000e-04,0.73989865419000e-04,0.73323347934000e-04,&
      &0.72656744914000e-04,0.71990129150000e-04,0.71323571725000e-04,0.70657177717000e-04,0.69991054506000e-04,&
      &0.69325274687000e-04,0.68659986158000e-04,0.67995222461000e-04,0.67331050001000e-04,0.66667579330000e-04,&
      &0.66004877667000e-04,0.65343012472000e-04,0.64682102004000e-04,0.64022149990000e-04,0.63363238128000e-04,&
      &0.62705434446000e-04,0.62048781840000e-04,0.61393325592000e-04,0.60739105142000e-04,0.60086147618000e-04,&
      &0.59434531293000e-04,0.58784304814000e-04,0.58135437273000e-04,0.57487935867000e-04,0.56841898333000e-04,&
      &0.56197268197000e-04,0.55554101735000e-04,0.54912374572000e-04,0.54272056765000e-04,0.53633235661000e-04,&
      &0.52995796794000e-04,0.52359821737000e-04,0.51725249521000e-04,0.51092006687000e-04,0.50460126184000e-04,&
      &0.49829564307000e-04,0.49200269237000e-04,0.48572230464000e-04,0.47945361009000e-04,0.47319619863000e-04,&
      &0.46695009846000e-04,0.46071372619000e-04,0.45448752880000e-04,0.44827048589000e-04,0.44206175745000e-04,&
      &0.43586073688000e-04,0.42966615446000e-04,0.42347851111000e-04,0.41729658091000e-04,0.41111856399000e-04,&
      &0.40494478424000e-04,0.39877485913000e-04,0.39260633756000e-04,0.38644022790000e-04,0.38027463792000e-04,&
      &0.37410906903000e-04,0.36794319579000e-04,0.36177530825000e-04,0.35560592822000e-04,0.34943356377000e-04,&
      &0.34325766017000e-04,0.33707841003000e-04,0.33089470831000e-04,0.32470590377000e-04,0.31851230272000e-04,&
      &0.31231291066000e-04,0.30610778141000e-04,0.29989762576000e-04,0.29368212776000e-04,0.28746120965000e-04,&
      &0.28123484830000e-04,0.27500399326000e-04,0.26876895597000e-04,0.26253079684000e-04,0.25629104909000e-04,&
      &0.25004973315000e-04,0.24380866783000e-04,0.23756875674000e-04,0.23133253454000e-04,0.22510170129000e-04,&
      &0.21887815569000e-04,0.21266436502000e-04,0.20646332639000e-04,0.20027750533000e-04,0.19410961723000e-04,&
      &0.18796374337000e-04,0.18184333154000e-04,0.17575211125000e-04,0.16969309405000e-04,0.16367125777000e-04,&
      &0.15769281195000e-04,0.15176106524000e-04,0.14588013225000e-04,0.14005556675000e-04,0.13429346383000e-04,&
      &0.12859913105000e-04,0.12297788662000e-04,0.11743564049000e-04,0.11197718102000e-04,0.10660912631000e-04,&
      &0.10133580164000e-04,0.96171283890000e-05,0.91110483697000e-05,0.86075351265000e-05,0.81006197457000e-05,&
      &0.75843333137000e-05,0.70527069166000e-05,0.64997716409000e-05,0.59195585727000e-05,0.53060987985000e-05,&
      &0.46534234045000e-05,0.39555634770000e-05,0.32065501024000e-05,0.24004143668000e-05,0.15311873566000e-05,&
      &0.59290015822000e-06      /)

      pblast_data%sri(9,1:256) = (/&
      &0.86501963000000e-04,0.86314296894000e-04,0.86139184523000e-04,0.85975961709000e-04,0.85823750146000e-04,&
      &0.85681844717000e-04,0.85549502216000e-04,0.85426087935000e-04,0.85310976957000e-04,0.85203491937000e-04,&
      &0.85103022576000e-04,0.85008955555000e-04,0.84920679458000e-04,0.84837693305000e-04,0.84759448613000e-04,&
      &0.84685424777000e-04,0.84615099371000e-04,0.84547978717000e-04,0.84483583020000e-04,0.84421434027000e-04,&
      &0.84361181795000e-04,0.84302379017000e-04,0.84244582313000e-04,0.84187362319000e-04,0.84130403562000e-04,&
      &0.84073269153000e-04,0.84015720573000e-04,0.83957395242000e-04,0.83897907960000e-04,0.83836974647000e-04,&
      &0.83774317140000e-04,0.83709633207000e-04,0.83642707896000e-04,0.83573243133000e-04,0.83501015203000e-04,&
      &0.83425811467000e-04,0.83347370526000e-04,0.83265526583000e-04,0.83180100074000e-04,0.83090871039000e-04,&
      &0.82997709849000e-04,0.82900452777000e-04,0.82798909781000e-04,0.82693030484000e-04,0.82582646996000e-04,&
      &0.82467658862000e-04,0.82347967876000e-04,0.82223472809000e-04,0.82094100837000e-04,0.81959780298000e-04,&
      &0.81820469706000e-04,0.81676061983000e-04,0.81526573780000e-04,0.81371985848000e-04,0.81212194284000e-04,&
      &0.81047214736000e-04,0.80877070848000e-04,0.80701742230000e-04,0.80521198885000e-04,0.80335466376000e-04,&
      &0.80144594640000e-04,0.79948602606000e-04,0.79747493740000e-04,0.79541315230000e-04,0.79330100493000e-04,&
      &0.79113895371000e-04,0.78892769833000e-04,0.78666771718000e-04,0.78435939855000e-04,0.78200344685000e-04,&
      &0.77960038363000e-04,0.77715109479000e-04,0.77465650480000e-04,0.77211699408000e-04,0.76953339676000e-04,&
      &0.76690637409000e-04,0.76423696628000e-04,0.76152598242000e-04,0.75877397464000e-04,0.75598188124000e-04,&
      &0.75315062918000e-04,0.75028090226000e-04,0.74737346728000e-04,0.74442946665000e-04,0.74144967067000e-04,&
      &0.73843447504000e-04,0.73538505524000e-04,0.73230216507000e-04,0.72918658906000e-04,0.72603907411000e-04,&
      &0.72286032794000e-04,0.71965116908000e-04,0.71641233193000e-04,0.71314427914000e-04,0.70984807101000e-04,&
      &0.70652439271000e-04,0.70317345405000e-04,0.69979597825000e-04,0.69639274038000e-04,0.69296433392000e-04,&
      &0.68951111940000e-04,0.68603353385000e-04,0.68253216881000e-04,0.67900752652000e-04,0.67545998019000e-04,&
      &0.67188980583000e-04,0.66829753944000e-04,0.66468320157000e-04,0.66104711711000e-04,0.65739015965000e-04,&
      &0.65371188622000e-04,0.65001280955000e-04,0.64629291480000e-04,0.64255243281000e-04,0.63879170409000e-04,&
      &0.63501051277000e-04,0.63120904041000e-04,0.62738713236000e-04,0.62354490490000e-04,0.61968243984000e-04,&
      &0.61579981402000e-04,0.61189703196000e-04,0.60797340833000e-04,0.60402921968000e-04,0.60006459510000e-04,&
      &0.59607888248000e-04,0.59207206192000e-04,0.58804458315000e-04,0.58399535328000e-04,0.57992473130000e-04,&
      &0.57583228392000e-04,0.57171774584000e-04,0.56758117774000e-04,0.56342216125000e-04,0.55924057240000e-04,&
      &0.55503591647000e-04,0.55080785326000e-04,0.54655675781000e-04,0.54228224819000e-04,0.53798335416000e-04,&
      &0.53366061433000e-04,0.52931406091000e-04,0.52494261565000e-04,0.52054632218000e-04,0.51612565485000e-04,&
      &0.51167991890000e-04,0.50720896692000e-04,0.50271326244000e-04,0.49819207205000e-04,0.49364533706000e-04,&
      &0.48907347358000e-04,0.48447622904000e-04,0.47985395713000e-04,0.47520651155000e-04,0.47053369972000e-04,&
      &0.46583569887000e-04,0.46111310976000e-04,0.45636625823000e-04,0.45159492401000e-04,0.44679982935000e-04,&
      &0.44198112608000e-04,0.43713910204000e-04,0.43227437523000e-04,0.42738751516000e-04,0.42247922117000e-04,&
      &0.41754951112000e-04,0.41259962921000e-04,0.40763009730000e-04,0.40264143675000e-04,0.39763495010000e-04,&
      &0.39261119126000e-04,0.38757147429000e-04,0.38251656437000e-04,0.37744694470000e-04,0.37236434137000e-04,&
      &0.36727035840000e-04,0.36216518912000e-04,0.35705076934000e-04,0.35192793353000e-04,0.34679837392000e-04,&
      &0.34166375727000e-04,0.33652523073000e-04,0.33138409157000e-04,0.32624225382000e-04,0.32110113323000e-04,&
      &0.31596215942000e-04,0.31082732432000e-04,0.30569880465000e-04,0.30057808839000e-04,0.29546573386000e-04,&
      &0.29036440907000e-04,0.28527671115000e-04,0.28020356595000e-04,0.27514688250000e-04,0.27010960844000e-04,&
      &0.26509199544000e-04,0.26009639796000e-04,0.25512573393000e-04,0.25018150975000e-04,0.24526491178000e-04,&
      &0.24037807229000e-04,0.23552308523000e-04,0.23070230088000e-04,0.22591690648000e-04,0.22116870063000e-04,&
      &0.21646016346000e-04,0.21179235910000e-04,0.20716770791000e-04,0.20258717820000e-04,0.19805258132000e-04,&
      &0.19356592076000e-04,0.18912856751000e-04,0.18474259066000e-04,0.18040898839000e-04,0.17612892118000e-04,&
      &0.17190405814000e-04,0.16773586357000e-04,0.16362537075000e-04,0.15957388024000e-04,0.15558235431000e-04,&
      &0.15165216992000e-04,0.14778405704000e-04,0.14397868080000e-04,0.14023707103000e-04,0.13656001940000e-04,&
      &0.13294874856000e-04,0.12940335410000e-04,0.12592392979000e-04,0.12251155867000e-04,0.11916690837000e-04,&
      &0.11588961695000e-04,0.11267993698000e-04,0.10953855977000e-04,0.10646565160000e-04,0.10346050191000e-04,&
      &0.10052329153000e-04,0.97654825498000e-05,0.94853685910000e-05,0.92118757523000e-05,0.89458660192000e-05,&
      &0.86884637647000e-05,0.84407794183000e-05,0.82039234099000e-05,0.79790061691000e-05,0.77671381255000e-05,&
      &0.75694297088000e-05,0.73869913486000e-05,0.72209334747000e-05,0.70723665167000e-05,0.69424009042000e-05,&
      &0.68321470670000e-05,0.67427154347000e-05,0.66752164370000e-05,0.66307605034000e-05,0.66104580638000e-05,&
      &0.66154195478000e-05      /)

      pblast_data%sri(10,1:256) = (/&
      &0.69426863634000e-04,0.69397372741000e-04,0.69374514801000e-04,0.69357829638000e-04,0.69346709792000e-04,&
      &0.69340659610000e-04,0.69339172373000e-04,0.69341748727000e-04,0.69347938850000e-04,0.69357290867000e-04,&
      &0.69369350298000e-04,0.69383651080000e-04,0.69399766583000e-04,0.69417277023000e-04,0.69435761068000e-04,&
      &0.69454913669000e-04,0.69474195205000e-04,0.69493384588000e-04,0.69512054024000e-04,0.69529819080000e-04,&
      &0.69546350634000e-04,0.69561354430000e-04,0.69574490830000e-04,0.69585460966000e-04,0.69593957919000e-04,&
      &0.69599681363000e-04,0.69602370865000e-04,0.69601719641000e-04,0.69597535803000e-04,0.69589570744000e-04,&
      &0.69577558432000e-04,0.69561286442000e-04,0.69540556160000e-04,0.69515137352000e-04,0.69484877912000e-04,&
      &0.69449614699000e-04,0.69409151179000e-04,0.69363345433000e-04,0.69312068340000e-04,0.69255151989000e-04,&
      &0.69192483849000e-04,0.69123982212000e-04,0.69049534218000e-04,0.68969040032000e-04,0.68882432565000e-04,&
      &0.68789654569000e-04,0.68690643326000e-04,0.68585340549000e-04,0.68473737721000e-04,0.68355775038000e-04,&
      &0.68231456283000e-04,0.68100780372000e-04,0.67963715237000e-04,0.67820320400000e-04,0.67670586296000e-04,&
      &0.67514536627000e-04,0.67352221540000e-04,0.67183711329000e-04,0.67009032101000e-04,0.66828208076000e-04,&
      &0.66641373687000e-04,0.66448559523000e-04,0.66249882763000e-04,0.66045418137000e-04,0.65835236852000e-04,&
      &0.65619468934000e-04,0.65398212938000e-04,0.65171580542000e-04,0.64939650426000e-04,0.64702579873000e-04,&
      &0.64460499440000e-04,0.64213512193000e-04,0.63961773453000e-04,0.63705400294000e-04,0.63444547736000e-04,&
      &0.63179296749000e-04,0.62909834784000e-04,0.62636332872000e-04,0.62358887518000e-04,0.62077672988000e-04,&
      &0.61792830287000e-04,0.61504472427000e-04,0.61212788071000e-04,0.60917910712000e-04,0.60619976398000e-04,&
      &0.60319176751000e-04,0.60015607052000e-04,0.59709421613000e-04,0.59400798340000e-04,0.59089855216000e-04,&
      &0.58776716476000e-04,0.58461539143000e-04,0.58144457753000e-04,0.57825601306000e-04,0.57505113301000e-04,&
      &0.57183106954000e-04,0.56859699392000e-04,0.56535041443000e-04,0.56209267854000e-04,0.55882467790000e-04,&
      &0.55554726457000e-04,0.55226180175000e-04,0.54896946595000e-04,0.54567107134000e-04,0.54236766310000e-04,&
      &0.53906036837000e-04,0.53574973921000e-04,0.53243685189000e-04,0.52912226604000e-04,0.52580711976000e-04,&
      &0.52249189780000e-04,0.51917741064000e-04,0.51586412468000e-04,0.51255291645000e-04,0.50924412882000e-04,&
      &0.50593798978000e-04,0.50263532267000e-04,0.49933671313000e-04,0.49604210171000e-04,0.49275216067000e-04,&
      &0.48946671515000e-04,0.48618631575000e-04,0.48291128306000e-04,0.47964157306000e-04,0.47637746155000e-04,&
      &0.47311893979000e-04,0.46986595156000e-04,0.46661813900000e-04,0.46337607045000e-04,0.46013964801000e-04,&
      &0.45690827141000e-04,0.45368211757000e-04,0.45046078318000e-04,0.44724412675000e-04,0.44403194116000e-04,&
      &0.44082383867000e-04,0.43761926630000e-04,0.43441795614000e-04,0.43121984866000e-04,0.42802410699000e-04,&
      &0.42483045577000e-04,0.42163852560000e-04,0.41844769851000e-04,0.41525723644000e-04,0.41206662505000e-04,&
      &0.40887547698000e-04,0.40568311500000e-04,0.40248878108000e-04,0.39929214261000e-04,0.39609253874000e-04,&
      &0.39288885983000e-04,0.38968083671000e-04,0.38646749672000e-04,0.38324869057000e-04,0.38002338820000e-04,&
      &0.37679060971000e-04,0.37355005665000e-04,0.37030086619000e-04,0.36704229322000e-04,0.36377416695000e-04,&
      &0.36049513066000e-04,0.35720467917000e-04,0.35390254945000e-04,0.35058775020000e-04,0.34725987115000e-04,&
      &0.34391789249000e-04,0.34056182552000e-04,0.33719082104000e-04,0.33380414687000e-04,0.33040147616000e-04,&
      &0.32698222617000e-04,0.32354638720000e-04,0.32009320799000e-04,0.31662229568000e-04,0.31313349572000e-04,&
      &0.30962648965000e-04,0.30610099961000e-04,0.30255694594000e-04,0.29899399906000e-04,0.29541239005000e-04,&
      &0.29181159882000e-04,0.28819198850000e-04,0.28455380837000e-04,0.28089661787000e-04,0.27722128964000e-04,&
      &0.27352780447000e-04,0.26981659760000e-04,0.26608766714000e-04,0.26234207925000e-04,0.25858050866000e-04,&
      &0.25480237523000e-04,0.25100926689000e-04,0.24720210307000e-04,0.24338105369000e-04,0.23954757981000e-04,&
      &0.23570238860000e-04,0.23184587783000e-04,0.22798045073000e-04,0.22410649982000e-04,0.22022537970000e-04,&
      &0.21633810728000e-04,0.21244627263000e-04,0.20855141731000e-04,0.20465487035000e-04,0.20075810024000e-04,&
      &0.19686270449000e-04,0.19297054291000e-04,0.18908314550000e-04,0.18520194972000e-04,0.18132924070000e-04,&
      &0.17746675915000e-04,0.17361633188000e-04,0.16977941007000e-04,0.16595812184000e-04,0.16215475710000e-04,&
      &0.15837092321000e-04,0.15460851978000e-04,0.15086973336000e-04,0.14715628300000e-04,0.14346997331000e-04,&
      &0.13981316093000e-04,0.13618774920000e-04,0.13259527616000e-04,0.12903814270000e-04,0.12551784901000e-04,&
      &0.12203613042000e-04,0.11859518877000e-04,0.11519635624000e-04,0.11184121428000e-04,0.10853219436000e-04,&
      &0.10526990949000e-04,0.10205627071000e-04,0.98892641701000e-05,0.95779913956000e-05,0.92722375589000e-05,&
      &0.89714367214000e-05,0.86734894536000e-05,0.83764305008000e-05,0.80782946081000e-05,0.77771165207000e-05,&
      &0.74709309835000e-05,0.71577727420000e-05,0.68356765410000e-05,0.65026771259000e-05,0.61568092416000e-05,&
      &0.57961076335000e-05,0.54186070466000e-05,0.50223422260000e-05,0.46053479169000e-05,0.41656588645000e-05,&
      &0.37013098139000e-05,0.32103355102000e-05,0.26907706985000e-05,0.21406501240000e-05,0.15580085319000e-05,&
      &0.94088066731000e-06      /)



! figure 2_13 scale height of trople point versus scaled horizontal distance  cm/g**1/3

      pblast_data%shtp_abscissa(1:256) = (/&
      &7.9000000000e+00, 8.1494117647e+00, 8.3988235294e+00, 8.6482352941e+00, 8.8976470588e+00,&
      &9.1470588235e+00, 9.3964705882e+00, 9.6458823529e+00, 9.8952941176e+00, 1.0144705882e+01,&
      &1.0394117647e+01, 1.0643529412e+01, 1.0892941176e+01, 1.1142352941e+01, 1.1391764706e+01,&
      &1.1641176471e+01, 1.1890588235e+01, 1.2140000000e+01, 1.2389411765e+01, 1.2638823529e+01,&
      &1.2888235294e+01, 1.3137647059e+01, 1.3387058824e+01, 1.3636470588e+01, 1.3885882353e+01,&
      &1.4135294118e+01, 1.4384705882e+01, 1.4634117647e+01, 1.4883529412e+01, 1.5132941176e+01,&
      &1.5382352941e+01, 1.5631764706e+01, 1.5881176471e+01, 1.6130588235e+01, 1.6380000000e+01,&
      &1.6629411765e+01, 1.6878823529e+01, 1.7128235294e+01, 1.7377647059e+01, 1.7627058824e+01,&
      &1.7876470588e+01, 1.8125882353e+01, 1.8375294118e+01, 1.8624705882e+01, 1.8874117647e+01,&
      &1.9123529412e+01, 1.9372941176e+01, 1.9622352941e+01, 1.9871764706e+01, 2.0121176471e+01,&
      &2.0370588235e+01, 2.0620000000e+01, 2.0869411765e+01, 2.1118823529e+01, 2.1368235294e+01,&
      &2.1617647059e+01, 2.1867058824e+01, 2.2116470588e+01, 2.2365882353e+01, 2.2615294118e+01,&
      &2.2864705882e+01, 2.3114117647e+01, 2.3363529412e+01, 2.3612941176e+01, 2.3862352941e+01,&
      &2.4111764706e+01, 2.4361176471e+01, 2.4610588235e+01, 2.4860000000e+01, 2.5109411765e+01,&
      &2.5358823529e+01, 2.5608235294e+01, 2.5857647059e+01, 2.6107058824e+01, 2.6356470588e+01,&
      &2.6605882353e+01, 2.6855294118e+01, 2.7104705882e+01, 2.7354117647e+01, 2.7603529412e+01,&
      &2.7852941176e+01, 2.8102352941e+01, 2.8351764706e+01, 2.8601176471e+01, 2.8850588235e+01,&
      &2.9100000000e+01, 2.9349411765e+01, 2.9598823529e+01, 2.9848235294e+01, 3.0097647059e+01,&
      &3.0347058824e+01, 3.0596470588e+01, 3.0845882353e+01, 3.1095294118e+01, 3.1344705882e+01,&
      &3.1594117647e+01, 3.1843529412e+01, 3.2092941176e+01, 3.2342352941e+01, 3.2591764706e+01,&
      &3.2841176471e+01, 3.3090588235e+01, 3.3340000000e+01, 3.3589411765e+01, 3.3838823529e+01,&
      &3.4088235294e+01, 3.4337647059e+01, 3.4587058824e+01, 3.4836470588e+01, 3.5085882353e+01,&
      &3.5335294118e+01, 3.5584705882e+01, 3.5834117647e+01, 3.6083529412e+01, 3.6332941176e+01,&
      &3.6582352941e+01, 3.6831764706e+01, 3.7081176471e+01, 3.7330588235e+01, 3.7580000000e+01,&
      &3.7829411765e+01, 3.8078823529e+01, 3.8328235294e+01, 3.8577647059e+01, 3.8827058824e+01,&
      &3.9076470588e+01, 3.9325882353e+01, 3.9575294118e+01, 3.9824705882e+01, 4.0074117647e+01,&
      &4.0323529412e+01, 4.0572941176e+01, 4.0822352941e+01, 4.1071764706e+01, 4.1321176471e+01,&
      &4.1570588235e+01, 4.1820000000e+01, 4.2069411765e+01, 4.2318823529e+01, 4.2568235294e+01,&
      &4.2817647059e+01, 4.3067058824e+01, 4.3316470588e+01, 4.3565882353e+01, 4.3815294118e+01,&
      &4.4064705882e+01, 4.4314117647e+01, 4.4563529412e+01, 4.4812941176e+01, 4.5062352941e+01,&
      &4.5311764706e+01, 4.5561176471e+01, 4.5810588235e+01, 4.6060000000e+01, 4.6309411765e+01,&
      &4.6558823529e+01, 4.6808235294e+01, 4.7057647059e+01, 4.7307058824e+01, 4.7556470588e+01,&
      &4.7805882353e+01, 4.8055294118e+01, 4.8304705882e+01, 4.8554117647e+01, 4.8803529412e+01,&
      &4.9052941176e+01, 4.9302352941e+01, 4.9551764706e+01, 4.9801176471e+01, 5.0050588235e+01,&
      &5.0300000000e+01, 5.0549411765e+01, 5.0798823529e+01, 5.1048235294e+01, 5.1297647059e+01,&
      &5.1547058824e+01, 5.1796470588e+01, 5.2045882353e+01, 5.2295294118e+01, 5.2544705882e+01,&
      &5.2794117647e+01, 5.3043529412e+01, 5.3292941176e+01, 5.3542352941e+01, 5.3791764706e+01,&
      &5.4041176471e+01, 5.4290588235e+01, 5.4540000000e+01, 5.4789411765e+01, 5.5038823529e+01,&
      &5.5288235294e+01, 5.5537647059e+01, 5.5787058824e+01, 5.6036470588e+01, 5.6285882353e+01,&
      &5.6535294118e+01, 5.6784705882e+01, 5.7034117647e+01, 5.7283529412e+01, 5.7532941176e+01,&
      &5.7782352941e+01, 5.8031764706e+01, 5.8281176471e+01, 5.8530588235e+01, 5.8780000000e+01,&
      &5.9029411765e+01, 5.9278823529e+01, 5.9528235294e+01, 5.9777647059e+01, 6.0027058824e+01,&
      &6.0276470588e+01, 6.0525882353e+01, 6.0775294118e+01, 6.1024705882e+01, 6.1274117647e+01,&
      &6.1523529412e+01, 6.1772941176e+01, 6.2022352941e+01, 6.2271764706e+01, 6.2521176471e+01,&
      &6.2770588235e+01, 6.3020000000e+01, 6.3269411765e+01, 6.3518823529e+01, 6.3768235294e+01,&
      &6.4017647059e+01, 6.4267058824e+01, 6.4516470588e+01, 6.4765882353e+01, 6.5015294118e+01,&
      &6.5264705882e+01, 6.5514117647e+01, 6.5763529412e+01, 6.6012941176e+01, 6.6262352941e+01,&
      &6.6511764706e+01, 6.6761176471e+01, 6.7010588235e+01, 6.7260000000e+01, 6.7509411765e+01,&
      &6.7758823529e+01, 6.8008235294e+01, 6.8257647059e+01, 6.8507058824e+01, 6.8756470588e+01,&
      &6.9005882353e+01, 6.9255294118e+01, 6.9504705882e+01, 6.9754117647e+01, 7.0003529412e+01,&
      &7.0252941176e+01, 7.0502352941e+01, 7.0751764706e+01, 7.1001176471e+01, 7.1250588235e+01,&
      &7.1500000000e+01 /)

      pblast_data%shtp(1,1:256) = (/&
      &1.5436157639e+00, 1.6457950233e+00, 1.7518039443e+00, 1.8616826666e+00, 1.9754696672e+00,&
      &2.0932024046e+00, 2.2149182204e+00, 2.3406528447e+00, 2.4704424897e+00, 2.6043197745e+00,&
      &2.7423192054e+00, 2.8844717032e+00, 3.0308104054e+00, 3.1813645279e+00, 3.3361634421e+00,&
      &3.4952355981e+00, 3.6586098108e+00, 3.8263117214e+00, 3.9983660568e+00, 4.1748014743e+00,&
      &4.3556372703e+00, 4.5408969300e+00, 4.7306042738e+00, 4.9247806442e+00, 5.1234407076e+00,&
      &5.3266137920e+00, 5.5343057192e+00, 5.7465449507e+00, 5.9633394341e+00, 6.1847107281e+00,&
      &6.4106701823e+00, 6.6412291284e+00, 6.8764032229e+00, 7.1162029908e+00, 7.3606383209e+00,&
      &7.6097205120e+00, 7.8634560805e+00, 8.1218586596e+00, 8.3849253783e+00, 8.6526682818e+00,&
      &8.9250932521e+00, 9.2022009362e+00, 9.4839988440e+00, 9.7704810212e+00, 1.0061657659e+01,&
      &1.0357523292e+01, 1.0658080438e+01, 1.0963322231e+01, 1.1273255693e+01, 1.1587869656e+01,&
      &1.1907158116e+01, 1.2231121089e+01, 1.2559748580e+01, 1.2893039221e+01, 1.3230976532e+01,&
      &1.3573556130e+01, 1.3920764370e+01, 1.4272597198e+01, 1.4629036309e+01, 1.4990070953e+01,&
      &1.5355682077e+01, 1.5725868095e+01, 1.6100597557e+01, 1.6479863960e+01, 1.6863649582e+01,&
      &1.7251925836e+01, 1.7644683533e+01, 1.8041899339e+01, 1.8443549406e+01, 1.8849614961e+01,&
      &1.9260071920e+01, 1.9674890649e+01, 2.0094053132e+01, 2.0517525783e+01, 2.0945292064e+01,&
      &2.1377315390e+01, 2.1813566967e+01, 2.2254022670e+01, 2.2698648070e+01, 2.3147408020e+01,&
      &2.3600270440e+01, 2.4057212418e+01, 2.4518190036e+01, 2.4983164178e+01, 2.5452106361e+01,&
      &2.5924972757e+01, 2.6401730337e+01, 2.6882335671e+01, 2.7366753018e+01, 2.7854932674e+01,&
      &2.8346841797e+01, 2.8842435599e+01, 2.9341664227e+01, 2.9844485581e+01, 3.0350856495e+01,&
      &3.0860727100e+01, 3.1374055126e+01, 3.1890779694e+01, 3.2410865111e+01, 3.2934244518e+01,&
      &3.3460887747e+01, 3.3990730483e+01, 3.4523703217e+01, 3.5059778801e+01, 3.5598889561e+01,&
      &3.6140981098e+01, 3.6685995483e+01, 3.7233864281e+01, 3.7784551658e+01, 3.8337980692e+01,&
      &3.8894087393e+01, 3.9452812899e+01, 4.0014348363e+01, 4.0580358453e+01, 4.1152784098e+01,&
      &4.1733566229e+01, 4.2324645776e+01, 4.2927963670e+01, 4.3545460840e+01, 4.4179078218e+01,&
      &4.4830756733e+01, 4.5502437317e+01, 4.6196060898e+01, 4.6913568409e+01, 4.7656900779e+01,&
      &4.8427998938e+01, 4.9228803816e+01, 5.0061256346e+01, 5.0927297455e+01, 5.1828868076e+01,&
      &5.2767909138e+01, 5.3746361572e+01, 5.4766166308e+01, 5.5829264276e+01, 5.6937596407e+01,&
      &5.8093103631e+01, 5.9297726879e+01, 6.0553407081e+01, 6.1862085167e+01, 6.3225702068e+01,&
      &6.4646198713e+01, 6.6125516035e+01, 6.7665594962e+01, 6.9268376425e+01, 7.0935801354e+01,&
      &7.2669810681e+01, 7.4472345335e+01, 7.6345346246e+01, 7.8290754346e+01, 8.0310510563e+01,&
      &8.2406555830e+01, 8.4580831076e+01, 8.6835277231e+01, 8.9171835226e+01, 9.1592445991e+01,&
      &9.4099050457e+01, 9.6693589554e+01, 9.9378004212e+01, 1.0215423536e+02, 1.0502422393e+02,&
      &1.0798991086e+02, 1.1105323707e+02, 1.1421614349e+02, 1.1748057105e+02, 1.2084846069e+02,&
      &1.2432175333e+02, 1.2790238991e+02, 1.3159231135e+02, 1.3539345859e+02, 1.3930777255e+02,&
      &1.4333719417e+02, 1.4748366438e+02, 1.5174912410e+02, 1.5613551427e+02, 1.6064477582e+02,&
      &1.6527884967e+02, 1.7003967677e+02, 1.7492919803e+02, 1.7994935439e+02, 1.8510208678e+02,&
      &1.9038933614e+02, 1.9581304338e+02, 2.0137514944e+02, 2.0707759525e+02, 2.1292232175e+02,&
      &2.1891126985e+02, 2.2504638050e+02, 2.3132959462e+02, 2.3776285315e+02, 2.4434809701e+02,&
      &2.5108726713e+02, 2.5798230444e+02, 2.6503514988e+02, 2.7224774437e+02, 2.7962202885e+02,&
      &2.8715994425e+02, 2.9486343149e+02, 3.0273443150e+02, 3.1077488522e+02, 3.1898673358e+02,&
      &3.2737191751e+02, 3.3593237794e+02, 3.4467005579e+02, 3.5358689200e+02, 3.6268482750e+02,&
      &3.7196580322e+02, 3.8143176009e+02, 3.9108463903e+02, 4.0092638099e+02, 4.1095892689e+02,&
      &4.2118421766e+02, 4.3160419423e+02, 4.4222079754e+02, 4.5303596850e+02, 4.6405164806e+02,&
      &4.7526977714e+02, 4.8669229667e+02, 4.9832114759e+02, 5.1015827082e+02, 5.2220560730e+02,&
      &5.3446509795e+02, 5.4693868371e+02, 5.5962830550e+02, 5.7253590425e+02, 5.8566342091e+02,&
      &5.9901279639e+02, 6.1258597162e+02, 6.2638488755e+02, 6.4041148509e+02, 6.5466770518e+02,&
      &6.6915548875e+02, 6.8387677672e+02, 6.9883351004e+02, 7.1402762962e+02, 7.2946107641e+02,&
      &7.4513579132e+02, 7.6105371530e+02, 7.7721678927e+02, 7.9362695416e+02, 8.1028615090e+02,&
      &8.2719632042e+02, 8.4435940365e+02, 8.6177734153e+02, 8.7945207498e+02, 8.9738554494e+02,&
      &9.1557969233e+02, 9.3403645808e+02, 9.5275778313e+02, 9.7174560840e+02, 9.9100187483e+02,&
      &1.0105285233e+03, 1.0303274949e+03, 1.0504007303e+03, 1.0707501707e+03, 1.0913777569e+03,&
      &1.1122854298e+03 /)

      pblast_data%shtp(2,1:256) = (/&
      &4.8733623144e-01, 5.9423154180e-01, 6.9921777160e-01, 8.0247803139e-01, 9.0419765901e-01,&
      &1.0045705419e+00, 1.1037627330e+00, 1.2019697554e+00, 1.2993575522e+00, 1.3960977025e+00,&
      &1.4923697186e+00, 1.5883267346e+00, 1.6841468329e+00, 1.7799807530e+00, 1.8759874940e+00,&
      &1.9723294489e+00, 2.0691481854e+00, 2.1666019235e+00, 2.2648330451e+00, 2.3639795614e+00,&
      &2.4641926738e+00, 2.5655996201e+00, 2.6683391086e+00, 2.7725438671e+00, 2.8783350546e+00,&
      &2.9858481942e+00, 3.0951973887e+00, 3.2065063056e+00, 3.3198900571e+00, 3.4354595108e+00,&
      &3.5533303905e+00, 3.6736067068e+00, 3.7963919105e+00, 3.9217915262e+00, 4.0499006085e+00,&
      &4.1808163426e+00, 4.3146325145e+00, 4.4514343693e+00, 4.5913168405e+00, 4.7343547834e+00,&
      &4.8806316806e+00, 5.0302301926e+00, 5.1832164848e+00, 5.3396719034e+00, 5.4996612053e+00,&
      &5.6632473440e+00, 5.8304948554e+00, 6.0014647862e+00, 6.1762183931e+00, 6.3548019739e+00,&
      &6.5372750936e+00, 6.7236741915e+00, 6.9140555038e+00, 7.1084569320e+00, 7.3069173728e+00,&
      &7.5094758438e+00, 7.7161582792e+00, 7.9269997459e+00, 8.1420326599e+00, 8.3612722175e+00,&
      &8.5847479800e+00, 8.8124728864e+00, 9.0444643773e+00, 9.2807340228e+00, 9.5212895250e+00,&
      &9.7661440892e+00, 1.0015297681e+01, 1.0268753247e+01, 1.0526504065e+01, 1.0788542938e+01,&
      &1.1054870366e+01, 1.1325465241e+01, 1.1600324024e+01, 1.1879416476e+01, 1.2162737258e+01,&
      &1.2450249507e+01, 1.2741940182e+01, 1.3037764986e+01, 1.3337712013e+01, 1.3641721569e+01,&
      &1.3949783951e+01, 1.4261827848e+01, 1.4577832878e+01, 1.4897743576e+01, 1.5221512469e+01,&
      &1.5549079211e+01, 1.5880402891e+01, 1.6215409158e+01, 1.6554043975e+01, 1.6896242975e+01,&
      &1.7241937017e+01, 1.7591052550e+01, 1.7943526108e+01, 1.8299271442e+01, 1.8658209852e+01,&
      &1.9020260724e+01, 1.9385348306e+01, 1.9753362453e+01, 2.0124230571e+01, 2.0497856739e+01,&
      &2.0874116765e+01, 2.1252973970e+01, 2.1634234729e+01, 2.2017884150e+01, 2.2403744325e+01,&
      &2.2791739352e+01, 2.3181745973e+01, 2.3573642240e+01, 2.3967321951e+01, 2.4362623414e+01,&
      &2.4759454872e+01, 2.5157691841e+01, 2.5557171845e+01, 2.5957770452e+01, 2.6359357618e+01,&
      &2.6761783450e+01, 2.7164903253e+01, 2.7568575930e+01, 2.7972636630e+01, 2.8376957856e+01,&
      &2.8781347907e+01, 2.9185678985e+01, 2.9589769984e+01, 2.9993460794e+01, 3.0396585716e+01,&
      &3.0798960605e+01, 3.1200437616e+01, 3.1600802341e+01, 3.1999903610e+01, 3.2397555785e+01,&
      &3.2793557835e+01, 3.3187723260e+01, 3.3579884807e+01, 3.3969803594e+01, 3.4357327267e+01,&
      &3.4742220875e+01, 3.5124286360e+01, 3.5503328884e+01, 3.5879120822e+01, 3.6251475735e+01,&
      &3.6620155209e+01, 3.6984948923e+01, 3.7345642631e+01, 3.7701910755e+01, 3.8054886505e+01,&
      &3.8407424568e+01, 3.8762348160e+01, 3.9122480501e+01, 3.9490644809e+01, 3.9869664302e+01,&
      &4.0262362199e+01, 4.0671561718e+01, 4.1100086078e+01, 4.1550758496e+01, 4.2026402192e+01,&
      &4.2529840384e+01, 4.3063896291e+01, 4.3631393130e+01, 4.4235154120e+01, 4.4878002480e+01,&
      &4.5562761428e+01, 4.6292254182e+01, 4.7069303961e+01, 4.7896733984e+01, 4.8777367468e+01,&
      &4.9714027632e+01, 5.0709537695e+01, 5.1766720875e+01, 5.2888400391e+01, 5.4077399460e+01,&
      &5.5336541302e+01, 5.6668649135e+01, 5.8076546176e+01, 5.9563055645e+01, 6.1131000761e+01,&
      &6.2783204741e+01, 6.4522490803e+01, 6.6351682167e+01, 6.8273602051e+01, 7.0291073673e+01,&
      &7.2406920251e+01, 7.4623965005e+01, 7.6945031152e+01, 7.9372941911e+01, 8.1910520500e+01,&
      &8.4560590138e+01, 8.7325974043e+01, 9.0209495434e+01, 9.3213977529e+01, 9.6342243546e+01,&
      &9.9597116704e+01, 1.0298142022e+02, 1.0649797732e+02, 1.1014961121e+02, 1.1393914511e+02,&
      &1.1786940225e+02, 1.2194320584e+02, 1.2616337910e+02, 1.3053274525e+02, 1.3505412751e+02,&
      &1.3973034909e+02, 1.4456423321e+02, 1.4955860310e+02, 1.5471628197e+02, 1.6004009303e+02,&
      &1.6553285952e+02, 1.7119740464e+02, 1.7703655161e+02, 1.8305312365e+02, 1.8924994399e+02,&
      &1.9562983584e+02, 2.0219562241e+02, 2.0895012693e+02, 2.1589617262e+02, 2.2303658269e+02,&
      &2.3037418036e+02, 2.3791178885e+02, 2.4565223137e+02, 2.5359833116e+02, 2.6175291142e+02,&
      &2.7011879537e+02, 2.7869880624e+02, 2.8749576724e+02, 2.9651250159e+02, 3.0575183250e+02,&
      &3.1521658320e+02, 3.2490957690e+02, 3.3483363683e+02, 3.4499158620e+02, 3.5538624823e+02,&
      &3.6602044613e+02, 3.7689700313e+02, 3.8801874245e+02, 3.9938848730e+02, 4.1100906090e+02,&
      &4.2288328648e+02, 4.3501398724e+02, 4.4740398641e+02, 4.6005610720e+02, 4.7297317284e+02,&
      &4.8615800654e+02, 4.9961343152e+02, 5.1334227100e+02, 5.2734734820e+02, 5.4163148633e+02,&
      &5.5619750862e+02, 5.7104823828e+02, 5.8618649853e+02, 6.0161511259e+02, 6.1733690368e+02,&
      &6.3335469502e+02, 6.4967130982e+02, 6.6628957131e+02, 6.8321230270e+02, 7.0044232720e+02,&
      &7.1798246805e+02 /)

      pblast_data%shtp(3,1:256) = (/&
      &4.1742688050e-01, 4.5390385122e-01, 4.9161750894e-01, 5.3057148908e-01, 5.7077164754e-01,&
      &6.1222298892e-01, 6.5493039667e-01, 6.9889851769e-01, 7.4413309534e-01, 7.9063850691e-01,&
      &8.3842008582e-01, 8.8748319780e-01, 9.3783211040e-01, 9.8947222399e-01, 1.0424085918e+00,&
      &1.0966465760e+00, 1.1521905714e+00, 1.2090463548e+00, 1.2672185312e+00, 1.3267122679e+00,&
      &1.3875328536e+00, 1.4496853507e+00, 1.5131744198e+00, 1.5780056045e+00, 1.6441842112e+00,&
      &1.7117141522e+00, 1.7806020069e+00, 1.8508520278e+00, 1.9224695598e+00, 1.9954599416e+00,&
      &2.0698278604e+00, 2.1455793270e+00, 2.2227180179e+00, 2.3012502445e+00, 2.3811817504e+00,&
      &2.4625163564e+00, 2.5452590606e+00, 2.6294161145e+00, 2.7149921340e+00, 2.8019916187e+00,&
      &2.8904214600e+00, 2.9802868144e+00, 3.0715907579e+00, 3.1643394323e+00, 3.2585388050e+00,&
      &3.3541937374e+00, 3.4513083658e+00, 3.5498906217e+00, 3.6499436025e+00, 3.7514721230e+00,&
      &3.8544816073e+00, 3.9589796329e+00, 4.0649698299e+00, 4.1724554043e+00, 4.2814427447e+00,&
      &4.3919406559e+00, 4.5039519743e+00, 4.6174782048e+00, 4.7325300794e+00, 4.8491133546e+00,&
      &4.9672280692e+00, 5.0868816924e+00, 5.2080816378e+00, 5.3308318900e+00, 5.4551362979e+00,&
      &5.5810032188e+00, 5.7084353936e+00, 5.8374383442e+00, 5.9680189351e+00, 6.1001804972e+00,&
      &6.2339320394e+00, 6.3692751681e+00, 6.5062126436e+00, 6.6447578748e+00, 6.7849117738e+00,&
      &6.9266784267e+00, 7.0700631430e+00, 7.2150778751e+00, 7.3617188298e+00, 7.5099953203e+00,&
      &7.6599122813e+00, 7.8114781320e+00, 7.9646973479e+00, 8.1195717864e+00, 8.2761066043e+00,&
      &8.4343139617e+00, 8.5941939040e+00, 8.7557503177e+00, 8.9189953592e+00, 9.0839274489e+00,&
      &9.2505533322e+00, 9.4188821532e+00, 9.5889174103e+00, 9.7606639646e+00, 9.9341251692e+00,&
      &1.0109314542e+01, 1.0286229601e+01, 1.0464873209e+01, 1.0645263034e+01, 1.0827394981e+01,&
      &1.1011274602e+01, 1.1196914472e+01, 1.1384312695e+01, 1.1573473656e+01, 1.1764413330e+01,&
      &1.1957127647e+01, 1.2151625302e+01, 1.2347914042e+01, 1.2545994273e+01, 1.2745876847e+01,&
      &1.2947559261e+01, 1.3151058339e+01, 1.3356371154e+01, 1.3563505434e+01, 1.3772468700e+01,&
      &1.3983267468e+01, 1.4195898949e+01, 1.4410376471e+01, 1.4626709197e+01, 1.4844891442e+01,&
      &1.5064939872e+01, 1.5286851562e+01, 1.5510635014e+01, 1.5736298700e+01, 1.5963842995e+01,&
      &1.6193278151e+01, 1.6424609581e+01, 1.6657837594e+01, 1.6892975918e+01, 1.7130023295e+01,&
      &1.7368990652e+01, 1.7609879516e+01, 1.7852699402e+01, 1.8097449394e+01, 1.8344141118e+01,&
      &1.8592780510e+01, 1.8843369235e+01, 1.9095913428e+01, 1.9350426381e+01, 1.9606902641e+01,&
      &1.9865354482e+01, 2.0125785417e+01, 2.0388207782e+01, 2.0652614504e+01, 2.0919016787e+01,&
      &2.1187428484e+01, 2.1457845681e+01, 2.1730277253e+01, 2.2004727970e+01, 2.2281206890e+01,&
      &2.2559716047e+01, 2.2840260640e+01, 2.3122854210e+01, 2.3407491110e+01, 2.3694181514e+01,&
      &2.3982935981e+01, 2.4273758410e+01, 2.4566650691e+01, 2.4861615124e+01, 2.5158671497e+01,&
      &2.5457814873e+01, 2.5759051129e+01, 2.6062392489e+01, 2.6367837749e+01, 2.6675393848e+01,&
      &2.6985070518e+01, 2.7296873191e+01, 2.7610801590e+01, 2.7926868521e+01, 2.8245082954e+01,&
      &2.8565440332e+01, 2.8887947408e+01, 2.9212619571e+01, 2.9539452209e+01, 2.9868458144e+01,&
      &3.0199645638e+01, 3.0533011857e+01, 3.0868568914e+01, 3.1206320295e+01, 3.1546273042e+01,&
      &3.1888431050e+01, 3.2232802084e+01, 3.2579390955e+01, 3.2928208616e+01, 3.3279250695e+01,&
      &3.3632527396e+01, 3.3988050951e+01, 3.4345825832e+01, 3.4705845984e+01, 3.5068136468e+01,&
      &3.5432688539e+01, 3.5799507330e+01, 3.6168694598e+01, 3.6540360337e+01, 3.6914614506e+01,&
      &3.7291567064e+01, 3.7671327969e+01, 3.8054007180e+01, 3.8439714657e+01, 3.8828560357e+01,&
      &3.9220654240e+01, 3.9616106264e+01, 4.0015026389e+01, 4.0417524573e+01, 4.0823710774e+01,&
      &4.1233694952e+01, 4.1647587065e+01, 4.2065497073e+01, 4.2487534933e+01, 4.2913810605e+01,&
      &4.3344434048e+01, 4.3779515220e+01, 4.4219164079e+01, 4.4663490586e+01, 4.5112604699e+01,&
      &4.5566616375e+01, 4.6025635575e+01, 4.6489772257e+01, 4.6959136380e+01, 4.7433837902e+01,&
      &4.7913986783e+01, 4.8399692981e+01, 4.8891066455e+01, 4.9388217163e+01, 4.9891255065e+01,&
      &5.0400290119e+01, 5.0915432285e+01, 5.1436791520e+01, 5.1964477784e+01, 5.2498601035e+01,&
      &5.3039271232e+01, 5.3586598335e+01, 5.4140692301e+01, 5.4701663089e+01, 5.5269620659e+01,&
      &5.5844674970e+01, 5.6426935979e+01, 5.7016513646e+01, 5.7613517929e+01, 5.8218058788e+01,&
      &5.8830246180e+01, 5.9450190066e+01, 6.0078000403e+01, 6.0713787151e+01, 6.1357660267e+01,&
      &6.2009729712e+01, 6.2670105444e+01, 6.3338897421e+01, 6.4016215602e+01, 6.4702169947e+01,&
      &6.5396870414e+01, 6.6100426961e+01, 6.6812949548e+01, 6.7534548133e+01, 6.8265332675e+01,&
      &6.9005413133e+01 /)

      pblast_data%shtp(4,1:256) = (/&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-02, 6.9596407474e-02, 1.5625491062e-01, 2.4280354519e-01,&
      &3.2914417122e-01, 4.1517864878e-01, 5.0080883794e-01, 5.8593659876e-01, 6.7046379128e-01,&
      &7.5429227558e-01, 8.3732391172e-01, 9.1946055975e-01, 1.0006058744e+00, 1.0807433457e+00,&
      &1.1599437897e+00, 1.2382735728e+00, 1.3157942227e+00, 1.3925717659e+00, 1.4686662239e+00,&
      &1.5441439144e+00, 1.6190678664e+00, 1.6934975702e+00, 1.7674970042e+00, 1.8411194724e+00,&
      &1.9144286701e+00, 1.9874813868e+00, 2.0603342028e+00, 2.1330492109e+00, 2.2056763731e+00,&
      &2.2782807931e+00, 2.3509052272e+00, 2.4236111356e+00, 2.4964474971e+00, 2.5694701917e+00,&
      &2.6427293768e+00, 2.7162807960e+00, 2.7901704949e+00, 2.8644514852e+00, 2.9391675563e+00,&
      &3.0143712485e+00, 3.0901072737e+00, 3.1664269906e+00, 3.2433707351e+00, 3.3209884428e+00,&
      &3.3993268071e+00, 3.4784243666e+00, 3.5583268087e+00, 3.6390739449e+00, 3.7207106963e+00,&
      &3.8032792924e+00, 3.8868173342e+00, 3.9713660405e+00, 4.0569641117e+00, 4.1436489743e+00,&
      &4.2314620693e+00, 4.3204298974e+00, 4.4106017753e+00, 4.5020006672e+00, 4.5946723193e+00,&
      &4.6886449792e+00, 4.7839489552e+00, 4.8806244785e+00, 4.9786938953e+00, 5.0781937595e+00,&
      &5.1791587502e+00, 5.2816073181e+00, 5.3855797297e+00, 5.4910945314e+00, 5.5981859962e+00,&
      &5.7068772280e+00, 5.8171959595e+00, 5.9291655916e+00, 6.0428116961e+00, 6.1581580447e+00,&
      &6.2752288107e+00, 6.3940408069e+00, 6.5146233479e+00, 6.6369925220e+00, 6.7611704824e+00,&
      &6.8871757504e+00, 7.0150313764e+00, 7.1447464710e+00, 7.2763409908e+00, 7.4098369750e+00,&
      &7.5452510867e+00, 7.6825879362e+00, 7.8218692180e+00, 7.9631096347e+00, 8.1063259396e+00,&
      &8.2515131660e+00, 8.3987027540e+00, 8.5478949568e+00, 8.6990950443e+00, 8.8523259331e+00,&
      &9.0075923097e+00, 9.1648946712e+00, 9.3242437932e+00, 9.4856427232e+00, 9.6491127857e+00,&
      &9.8146421112e+00, 9.9822406740e+00, 1.0151915236e+01, 1.0323663484e+01, 1.0497483946e+01,&
      &1.0673388637e+01, 1.0851367795e+01, 1.1031434070e+01, 1.1213573657e+01, 1.1397794698e+01,&
      &1.1584093838e+01, 1.1772458741e+01, 1.1962896741e+01, 1.2155396956e+01, 1.2349945377e+01,&
      &1.2546556162e+01, 1.2745217791e+01, 1.2945913972e+01, 1.3148646430e+01, 1.3353404131e+01,&
      &1.3560167858e+01, 1.3768934004e+01, 1.3979695222e+01, 1.4192439179e+01, 1.4407148257e+01,&
      &1.4623811341e+01, 1.4842422136e+01, 1.5062949652e+01, 1.5285397640e+01, 1.5509743721e+01,&
      &1.5735960111e+01, 1.5964044807e+01, 1.6193961061e+01, 1.6425722056e+01, 1.6659276192e+01,&
      &1.6894616300e+01, 1.7131729166e+01, 1.7370571625e+01, 1.7611128269e+01, 1.7853394238e+01,&
      &1.8097329875e+01, 1.8342920285e+01, 1.8590123090e+01, 1.8838931782e+01, 1.9089304721e+01,&
      &1.9341225306e+01, 1.9594657768e+01, 1.9849570039e+01, 2.0105940128e+01, 2.0363736979e+01,&
      &2.0622928791e+01, 2.0883497871e+01, 2.1145377056e+01, 2.1408558099e+01, 2.1673005547e+01,&
      &2.1938679206e+01, 2.2205539730e+01, 2.2473554557e+01, 2.2742700176e+01, 2.3012926838e+01,&
      &2.3284183757e+01, 2.3556460205e+01, 2.3829693453e+01, 2.4103857010e+01, 2.4378884965e+01,&
      &2.4654786849e+01, 2.4931459120e+01, 2.5208900440e+01, 2.5487047253e+01, 2.5765850221e+01,&
      &2.6045288350e+01, 2.6325296685e+01, 2.6605830968e+01, 2.6886857510e+01, 2.7168304566e+01,&
      &2.7450132621e+01, 2.7732281531e+01, 2.8014723438e+01, 2.8297400020e+01, 2.8580241904e+01,&
      &2.8863229984e+01, 2.9146274383e+01, 2.9429324761e+01, 2.9712366210e+01, 2.9995303559e+01,&
      &3.0278100330e+01, 3.0560667360e+01, 3.0843011657e+01, 3.1126372239e+01, 3.1412679313e+01,&
      &3.1703863367e+01, 3.2001854887e+01, 3.2308584361e+01, 3.2625982275e+01, 3.2955979116e+01,&
      &3.3300505371e+01, 3.3661491527e+01, 3.4040868069e+01, 3.4440565486e+01, 3.4862514264e+01,&
      &3.5308644890e+01, 3.5780887850e+01, 3.6281173632e+01, 3.6811432722e+01, 3.7373595607e+01,&
      &3.7969592774e+01, 3.8601354710e+01, 3.9270811901e+01, 3.9979894834e+01, 4.0730533996e+01,&
      &4.1524659874e+01, 4.2364202955e+01, 4.3251093725e+01, 4.4187262672e+01, 4.5174640282e+01,&
      &4.6215157041e+01, 4.7310743438e+01, 4.8463329957e+01, 4.9674847088e+01, 5.0947225315e+01,&
      &5.2282395126e+01, 5.3682287008e+01, 5.5148831447e+01, 5.6683958931e+01, 5.8289599946e+01,&
      &5.9967684979e+01, 6.1720144517e+01, 6.3548909046e+01, 6.5455909054e+01, 6.7443075027e+01,&
      &6.9512337452e+01, 7.1665626816e+01, 7.3904873605e+01, 7.6232008307e+01, 7.8648961408e+01,&
      &8.1157663395e+01 /)

      pblast_data%shtp(5,1:256) = (/&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-02,&
      &0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-02, 2.8146041705e-03,&
      &2.5753449025e-02, 4.9506945149e-02, 7.4078069361e-02, 9.9469798478e-02, 1.2568510932e-01,&
      &1.5272697869e-01, 1.8059838342e-01, 2.0930230033e-01, 2.3884170622e-01, 2.6921957793e-01,&
      &3.0043889225e-01, 3.3250262602e-01, 3.6541375604e-01, 3.9917525914e-01, 4.3379011213e-01,&
      &4.6926129183e-01, 5.0559177506e-01, 5.4278453863e-01, 5.8084255936e-01, 6.1976687593e-01,&
      &6.5955565970e-01, 7.0020681733e-01, 7.4171843771e-01, 7.8408831678e-01, 8.2731419076e-01,&
      &8.7139433402e-01, 9.1632682261e-01, 9.6210907344e-01, 1.0087398410e+00, 1.0562175099e+00,&
      &1.1045398706e+00, 1.1537046095e+00, 1.2037104677e+00, 1.2545560994e+00, 1.3062389440e+00,&
      &1.3587573769e+00, 1.4121106798e+00, 1.4662954498e+00, 1.5213115069e+00, 1.5771563935e+00,&
      &1.6338294726e+00, 1.6913289224e+00, 1.7496531464e+00, 1.8088002204e+00, 1.8687683333e+00,&
      &1.9295573988e+00, 1.9911645735e+00, 2.0535903015e+00, 2.1168318903e+00, 2.1808882340e+00,&
      &2.2457582486e+00, 2.3114402979e+00, 2.3779332270e+00, 2.4452364017e+00, 2.5133486928e+00,&
      &2.5822683254e+00, 2.6519942984e+00, 2.7225254674e+00, 2.7938609289e+00, 2.8659998855e+00,&
      &2.9389414054e+00, 3.0126838415e+00, 3.0872266237e+00, 3.1625692751e+00, 3.2387097636e+00,&
      &3.3156483860e+00, 3.3933838647e+00, 3.4719157126e+00, 3.5512427843e+00, 3.6313644468e+00,&
      &3.7122796568e+00, 3.7939877372e+00, 3.8764889330e+00, 3.9597820255e+00, 4.0438651515e+00,&
      &4.1287393375e+00, 4.2144057506e+00, 4.3008624899e+00, 4.3881042454e+00, 4.4761361606e+00,&
      &4.5649579123e+00, 4.6545654633e+00, 4.7449618385e+00, 4.8361437994e+00, 4.9281149280e+00,&
      &5.0208720305e+00, 5.1144140040e+00, 5.2087452997e+00, 5.3038613950e+00, 5.3997605742e+00,&
      &5.4964462719e+00, 5.5939175603e+00, 5.6921745785e+00, 5.7912173108e+00, 5.8910460548e+00,&
      &5.9916574347e+00, 6.0930528844e+00, 6.1952378076e+00, 6.2982080069e+00, 6.4019609620e+00,&
      &6.5065004646e+00, 6.6118255667e+00, 6.7179363692e+00, 6.8248326970e+00, 6.9325187474e+00,&
      &7.0409867098e+00, 7.1502455170e+00, 7.2602902278e+00, 7.3711213286e+00, 7.4827435063e+00,&
      &7.5951516214e+00, 7.7083526472e+00, 7.8223421781e+00, 7.9371166638e+00, 8.0526862726e+00,&
      &8.1690456460e+00, 8.2861984969e+00, 8.4041432651e+00, 8.5228786526e+00, 8.6424094914e+00,&
      &8.7627319987e+00, 8.8838503936e+00, 9.0057673762e+00, 9.1284795318e+00, 9.2519893231e+00,&
      &9.3762978762e+00, 9.5014051019e+00, 9.6273091461e+00, 9.7540158176e+00, 9.8815295277e+00,&
      &1.0009840639e+01, 1.0138957048e+01, 1.0268882096e+01, 1.0399608250e+01, 1.0531143063e+01,&
      &1.0663488622e+01, 1.0796639600e+01, 1.0930603479e+01, 1.1065383424e+01, 1.1200970288e+01,&
      &1.1337372441e+01, 1.1474592448e+01, 1.1612624854e+01, 1.1751484607e+01, 1.1891158987e+01,&
      &1.2031651879e+01, 1.2172971367e+01, 1.2315111238e+01, 1.2458080081e+01, 1.2601878842e+01,&
      &1.2746504536e+01, 1.2891955971e+01, 1.3038243681e+01, 1.3185362347e+01, 1.3333316594e+01,&
      &1.3482109590e+01, 1.3631740745e+01, 1.3782214889e+01, 1.3933528219e+01, 1.4085684778e+01,&
      &1.4238687056e+01, 1.4392537595e+01, 1.4547240845e+01, 1.4702792734e+01, 1.4859199500e+01,&
      &1.5016462781e+01, 1.5174579503e+01, 1.5333559728e+01, 1.5493401315e+01, 1.5654104558e+01,&
      &1.5815674811e+01, 1.5978117527e+01, 1.6141425830e+01, 1.6305603248e+01, 1.6470657761e+01,&
      &1.6636591558e+01, 1.6803405189e+01, 1.6971096887e+01, 1.7139676305e+01, 1.7309143387e+01,&
      &1.7479493818e+01, 1.7650733747e+01, 1.7822873791e+01, 1.7995909556e+01, 1.8169841642e+01,&
      &1.8344676912e+01, 1.8520412619e+01, 1.8697058492e+01, 1.8874609718e+01, 1.9053076264e+01,&
      &1.9232455687e+01, 1.9412750882e+01, 1.9593967938e+01, 1.9776105883e+01, 1.9959168976e+01,&
      &2.0143164061e+01, 2.0328087183e+01, 2.0513945268e+01, 2.0700741361e+01, 2.0888474020e+01,&
      &2.1077151527e+01, 2.1266777043e+01, 2.1457351457e+01, 2.1648875359e+01, 2.1841352853e+01,&
      &2.2034794727e+01, 2.2229196086e+01, 2.2424559644e+01, 2.2620896793e+01, 2.2818197773e+01,&
      &2.3016468817e+01, 2.3215729543e+01, 2.3415971289e+01, 2.3617186874e+01, 2.3819394285e+01,&
      &2.4022593485e+01, 2.4226791113e+01, 2.4431982979e+01, 2.4638176682e+01, 2.4845379023e+01,&
      &2.5053585273e+01, 2.5262800589e+01, 2.5473036376e+01, 2.5684291890e+01, 2.5896567905e+01,&
      &2.6109870812e+01, 2.6324210107e+01, 2.6539579828e+01, 2.6755982313e+01, 2.6973425871e+01,&
      &2.7191922132e+01, 2.7411472081e+01, 2.7632069017e+01, 2.7853720311e+01, 2.8076437334e+01,&
      &2.8300222157e+01, 2.8525071798e+01, 2.8750988381e+01, 2.8977983128e+01, 2.9206086939e+01,&
      &2.9435212426e+01 /)

      pblast_data%shtp(6,1:256) = (/&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-04, 0.00000000000-02,&
      &5.7890220823e-02, 8.7454689996e-02, 1.1715760555e-01, 1.4701390519e-01, 1.7703852660e-01,&
      &2.0724640748e-01, 2.3765248552e-01, 2.6827169843e-01, 2.9911898388e-01, 3.3020927959e-01,&
      &3.6155752324e-01, 3.9317865253e-01, 4.2508760516e-01, 4.5729931881e-01, 4.8982873119e-01,&
      &5.2269077998e-01, 5.5590040289e-01, 5.8947253762e-01, 6.2342212184e-01, 6.5776409326e-01,&
      &6.9251338958e-01, 7.2768494849e-01, 7.6329370768e-01, 7.9935460485e-01, 8.3588257770e-01,&
      &8.7289256391e-01, 9.1039950119e-01, 9.4841832723e-01, 9.8696397972e-01, 1.0260513964e+00,&
      &1.0656955149e+00, 1.1059112729e+00, 1.1467136081e+00, 1.1881174583e+00, 1.2301377611e+00,&
      &1.2727894543e+00, 1.3160874754e+00, 1.3600467623e+00, 1.4046822525e+00, 1.4500088839e+00,&
      &1.4960408639e+00, 1.5427777971e+00, 1.5902098361e+00, 1.6383291704e+00, 1.6871280195e+00,&
      &1.7365997279e+00, 1.7867370402e+00, 1.8375340470e+00, 1.8889814587e+00, 1.9410750485e+00,&
      &1.9938048050e+00, 2.0471690267e+00, 2.1011569387e+00, 2.1557659300e+00, 2.2109874707e+00,&
      &2.2668178003e+00, 2.3232493226e+00, 2.3802794086e+00, 2.4378998503e+00, 2.4961081857e+00,&
      &2.5548976038e+00, 2.6142626689e+00, 2.6741995573e+00, 2.7347066060e+00, 2.7957758454e+00,&
      &2.8574035434e+00, 2.9195868175e+00, 2.9823222552e+00, 3.0456046024e+00, 3.1094310949e+00,&
      &3.1737997668e+00, 3.2387039855e+00, 3.3041445562e+00, 3.3701170029e+00, 3.4366170827e+00,&
      &3.5036443025e+00, 3.5711956631e+00, 3.6392693239e+00, 3.7078642949e+00, 3.7769762287e+00,&
      &3.8466039002e+00, 3.9167463443e+00, 3.9874003924e+00, 4.0585727324e+00, 4.1302488915e+00,&
      &4.2024354396e+00, 4.2751349490e+00, 4.3483409722e+00, 4.4220544251e+00, 4.4962789750e+00,&
      &4.5710083744e+00, 4.6462473205e+00, 4.7219955969e+00, 4.7982501575e+00, 4.8750105627e+00,&
      &4.9522836480e+00, 5.0300668983e+00, 5.1083614213e+00, 5.1871703787e+00, 5.2664875705e+00,&
      &5.3463250059e+00, 5.4266767709e+00, 5.5075491020e+00, 5.5889436326e+00, 5.6708557467e+00,&
      &5.7532982235e+00, 5.8362662765e+00, 5.9197621276e+00, 6.0037964150e+00, 6.0883613074e+00,&
      &6.1734677633e+00, 6.2591157282e+00, 6.3453055364e+00, 6.4320515553e+00, 6.5193499443e+00,&
      &6.6071971710e+00, 6.6956090585e+00, 6.7845880473e+00, 6.8741364812e+00, 6.9642583942e+00,&
      &7.0549597848e+00, 7.1462432297e+00, 7.2381149926e+00, 7.3305812603e+00, 7.4236462812e+00,&
      &7.5173142319e+00, 7.6115938457e+00, 7.7064900252e+00, 7.8020073176e+00, 7.8981541476e+00,&
      &7.9949314640e+00, 8.0923525920e+00, 8.1904206728e+00, 8.2891428594e+00, 8.3885303301e+00,&
      &8.4885813359e+00, 8.5893072423e+00, 8.6907194819e+00, 8.7928226221e+00, 8.8956244630e+00,&
      &8.9991319895e+00, 9.1033490211e+00, 9.2082950997e+00, 9.3139711237e+00, 9.4203798841e+00,&
      &9.5275462693e+00, 9.6354639163e+00, 9.7441482945e+00, 9.8536138451e+00, 9.9638549469e+00,&
      &1.0074896576e+01, 1.0186740623e+01, 1.0299399776e+01, 1.0412878789e+01, 1.0527192596e+01,&
      &1.0642356469e+01, 1.0758371112e+01, 1.0875252371e+01, 1.0993009959e+01, 1.1111652740e+01,&
      &1.1231193822e+01, 1.1351645870e+01, 1.1473018721e+01, 1.1595320721e+01, 1.1718575513e+01,&
      &1.1842781447e+01, 1.1967952480e+01, 1.2094109031e+01, 1.2221254820e+01, 1.2349408472e+01,&
      &1.2478583407e+01, 1.2608786821e+01, 1.2740036250e+01, 1.2872341430e+01, 1.3005719701e+01,&
      &1.3140180676e+01, 1.3275741790e+01, 1.3412412710e+01, 1.3550210404e+01, 1.3689145786e+01,&
      &1.3829239528e+01, 1.3970501747e+01, 1.4112948645e+01, 1.4256590774e+01, 1.4401447114e+01,&
      &1.4547537143e+01, 1.4694870767e+01, 1.4843459981e+01, 1.4993329351e+01, 1.5144485190e+01,&
      &1.5296945599e+01, 1.5450736000e+01, 1.5605859992e+01, 1.5762344770e+01, 1.5920200702e+01,&
      &1.6079442291e+01, 1.6240087586e+01, 1.6402163160e+01, 1.6565683049e+01, 1.6730640574e+01,&
      &1.6897088508e+01, 1.7065031425e+01, 1.7234474813e+01, 1.7405455157e+01, 1.7577985117e+01,&
      &1.7752077096e+01, 1.7927753270e+01, 1.8105034572e+01, 1.8283933035e+01, 1.8464469562e+01,&
      &1.8646675493e+01, 1.8830548910e+01, 1.9016130768e+01, 1.9203433076e+01, 1.9392452794e+01,&
      &1.9583252754e+01, 1.9775822956e+01, 1.9970183776e+01, 2.0166375157e+01, 2.0364402896e+01,&
      &2.0564291109e+01, 2.0766063252e+01, 2.0969737113e+01, 2.1175327555e+01, 2.1382869468e+01,&
      &2.1592374608e+01, 2.1803865531e+01, 2.2017374702e+01, 2.2232912787e+01, 2.2450523317e+01,&
      &2.2670109302e+01 /)

      pblast_data%shtp(7,1:256) = (/&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-02,&
      &0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-02,&
      &0.00000000000e-02, 0.00000000000e-02, 0.00000000000e-03, 4.7129429080e-03, 1.6749192600e-02,&
      &2.8936352702e-02, 4.1285951576e-02, 5.3809517580e-02, 6.6518579076e-02, 7.9424664423e-02,&
      &9.2539301982e-02, 1.0587402011e-01, 1.1944034717e-01, 1.3324981153e-01, 1.4731394153e-01,&
      &1.6164426555e-01, 1.7625231194e-01, 1.9114960906e-01, 2.0634768527e-01, 2.2185806894e-01,&
      &2.3769228842e-01, 2.5386187207e-01, 2.7037834825e-01, 2.8725324533e-01, 3.0449809166e-01,&
      &3.2212441560e-01, 3.4014374551e-01, 3.5856760976e-01, 3.7740753670e-01, 3.9667505470e-01,&
      &4.1638169211e-01, 4.3653897729e-01, 4.5715843860e-01, 4.7825160441e-01, 4.9983000308e-01,&
      &5.2190516295e-01, 5.4448861240e-01, 5.6759187979e-01, 5.9122649347e-01, 6.1540398180e-01,&
      &6.4013587315e-01, 6.6543369588e-01, 6.9130897833e-01, 7.1777324889e-01, 7.4483803589e-01,&
      &7.7251486772e-01, 8.0081527271e-01, 8.2975077925e-01, 8.5933291567e-01, 8.8957321036e-01,&
      &9.2048264878e-01, 9.5206078923e-01, 9.8430057513e-01, 1.0171973443e+00, 1.0507440458e+00,&
      &1.0849360979e+00, 1.1197670901e+00, 1.1552313432e+00, 1.1913230327e+00, 1.2280369893e+00,&
      &1.2653665993e+00, 1.3033055756e+00, 1.3418507703e+00, 1.3809947020e+00, 1.4207321811e+00,&
      &1.4610575868e+00, 1.5019663915e+00, 1.5434510986e+00, 1.5855083358e+00, 1.6281313936e+00,&
      &1.6713149577e+00, 1.7150542564e+00, 1.7593442894e+00, 1.8041788910e+00, 1.8495532951e+00,&
      &1.8954610092e+00, 1.9418987080e+00, 1.9888601698e+00, 2.0363399206e+00, 2.0843342991e+00,&
      &2.1328375195e+00, 2.1818426941e+00, 2.2313477552e+00, 2.2813464682e+00, 2.3318332502e+00,&
      &2.3828036569e+00, 2.4342522538e+00, 2.4861755978e+00, 2.5385670580e+00, 2.5914241445e+00,&
      &2.6447391041e+00, 2.6985105727e+00, 2.7527296556e+00, 2.8073958279e+00, 2.8625008123e+00,&
      &2.9180425462e+00, 2.9740165931e+00, 3.0304166043e+00, 3.0872371499e+00, 3.1444774794e+00,&
      &3.2021294845e+00, 3.2601915129e+00, 3.3186572542e+00, 3.3775224346e+00, 3.4367847521e+00,&
      &3.4964361624e+00, 3.5564755862e+00, 3.6168973198e+00, 3.6776974504e+00, 3.7388714846e+00,&
      &3.8004161645e+00, 3.8623255748e+00, 3.9245973695e+00, 3.9872271214e+00, 4.0502080725e+00,&
      &4.1135420954e+00, 4.1772173556e+00, 4.2412408100e+00, 4.3055946413e+00, 4.3702824514e+00,&
      &4.4353041668e+00, 4.5006476588e+00, 4.5663156672e+00, 4.6323043497e+00, 4.6986056501e+00,&
      &4.7652144513e+00, 4.8321328237e+00, 4.8993544970e+00, 4.9668756967e+00, 5.0346945424e+00,&
      &5.1028027205e+00, 5.1712001165e+00, 5.2398829409e+00, 5.3088508201e+00, 5.3780964668e+00,&
      &5.4476134902e+00, 5.5174066467e+00, 5.5874634090e+00, 5.6577874225e+00, 5.7283738879e+00,&
      &5.7992149075e+00, 5.8703149207e+00, 5.9416675175e+00, 6.0132651367e+00, 6.0851094492e+00,&
      &6.1571970621e+00, 6.2295206826e+00, 6.3020826400e+00, 6.3748779764e+00, 6.4479014932e+00,&
      &6.5211506691e+00, 6.5946277718e+00, 6.6683252230e+00, 6.7422410294e+00, 6.8163705299e+00,&
      &6.8907147857e+00, 6.9652681123e+00, 7.0400282056e+00, 7.1149920599e+00, 7.1901538536e+00,&
      &7.2655227284e+00, 7.3410777448e+00, 7.4168348694e+00, 7.4927770152e+00, 7.5689098893e+00,&
      &7.6452301182e+00, 7.7217278043e+00, 7.7984114327e+00, 7.8752746094e+00, 7.9523128082e+00,&
      &8.0295234598e+00, 8.1069056702e+00, 8.1844524454e+00, 8.2621677105e+00, 8.3400531700e+00,&
      &8.4180949834e+00, 8.4962979126e+00, 8.5746596195e+00, 8.6531762428e+00, 8.7318449107e+00,&
      &8.8106641493e+00, 8.8896346905e+00, 8.9687521912e+00, 9.0480166010e+00, 9.1274192527e+00,&
      &9.2069680838e+00, 9.2866508083e+00, 9.3664762891e+00, 9.4464380602e+00, 9.5265292374e+00,&
      &9.6067553501e+00, 9.6871124646e+00, 9.7675953081e+00, 9.8482067627e+00, 9.9289431630e+00,&
      &1.0009802211e+01, 1.0090784673e+01, 1.0171887495e+01, 1.0253109510e+01, 1.0334446285e+01,&
      &1.0415900855e+01, 1.0497471723e+01, 1.0579149573e+01, 1.0660942826e+01, 1.0742845135e+01,&
      &1.0824859820e+01, 1.0906974004e+01, 1.0989203822e+01, 1.1071533304e+01, 1.1153962582e+01,&
      &1.1236503514e+01, 1.1319142954e+01, 1.1401880490e+01, 1.1484715997e+01, 1.1567656089e+01,&
      &1.1650687129e+01, 1.1733818014e+01, 1.1817040991e+01, 1.1900355906e+01, 1.1983765952e+01,&
      &1.2067272916e+01, 1.2150864315e+01, 1.2234552824e+01, 1.2318328342e+01, 1.2402190627e+01,&
      &1.2486148512e+01, 1.2570184476e+01, 1.2654311948e+01, 1.2738528208e+01, 1.2822826588e+01,&
      &1.2907206489e+01, 1.2991676715e+01, 1.3076232758e+01, 1.3160863279e+01, 1.3245588343e+01,&
      &1.3330389201e+01, 1.3415269364e+01, 1.3500237221e+01, 1.3585289405e+01, 1.3670413282e+01,&
      &1.3755645347e+01 /)

      pblast_data%shtp(8,1:256) = (/&
      &6.6857414410e-01, 6.3362647456e-01, 5.9996602392e-01, 5.6757773715e-01, 5.3644655922e-01,&
      &5.0655743511e-01, 4.7789530979e-01, 4.5044512823e-01, 4.2419183541e-01, 3.9912037631e-01,&
      &3.7521569589e-01, 3.5246273914e-01, 3.3084645102e-01, 3.1035177651e-01, 2.9096366059e-01,&
      &2.7266704823e-01, 2.5544688440e-01, 2.3928811408e-01, 2.2417568224e-01, 2.1009453385e-01,&
      &1.9702961390e-01, 1.8496586735e-01, 1.7388823918e-01, 1.6378167436e-01, 1.5463111788e-01,&
      &1.4642151469e-01, 1.3913780978e-01, 1.3276494812e-01, 1.2728787468e-01, 1.2269153444e-01,&
      &1.1896087238e-01, 1.1608083346e-01, 1.1403636266e-01, 1.1281240496e-01, 1.1239390533e-01,&
      &1.1276580875e-01, 1.1391306018e-01, 1.1582060460e-01, 1.1847338700e-01, 1.2185635233e-01,&
      &1.2595444558e-01, 1.3075261172e-01, 1.3623579572e-01, 1.4238894257e-01, 1.4919699722e-01,&
      &1.5664490466e-01, 1.6471760987e-01, 1.7340005780e-01, 1.8267719345e-01, 1.9253396178e-01,&
      &2.0295530778e-01, 2.1392617640e-01, 2.2543151263e-01, 2.3745626144e-01, 2.4998536780e-01,&
      &2.6300377670e-01, 2.7649643310e-01, 2.9044828198e-01, 3.0484426831e-01, 3.1966933706e-01,&
      &3.3490843322e-01, 3.5054650175e-01, 3.6656848763e-01, 3.8295933584e-01, 3.9970399134e-01,&
      &4.1678739911e-01, 4.3419450414e-01, 4.5191025138e-01, 4.6991958581e-01, 4.8820745242e-01,&
      &5.0675879617e-01, 5.2555856204e-01, 5.4459169500e-01, 5.6384314002e-01, 5.8329784209e-01,&
      &6.0294074617e-01, 6.2275679724e-01, 6.4273094027e-01, 6.6284812024e-01, 6.8309328212e-01,&
      &7.0345137089e-01, 7.2390919432e-01, 7.4446962766e-01, 7.6514459905e-01, 7.8594230691e-01,&
      &8.0687258762e-01, 8.2794585018e-01, 8.4917083770e-01, 8.7055528050e-01, 8.9211194679e-01,&
      &9.1384537251e-01, 9.3576599016e-01, 9.5788231503e-01, 9.8020184214e-01, 1.0027350197e+00,&
      &1.0254874946e+00, 1.0484685427e+00, 1.0716861771e+00, 1.0951452804e+00, 1.1188543784e+00,&
      &1.1428232966e+00, 1.1670555319e+00, 1.1915606154e+00, 1.2163429188e+00, 1.2414126015e+00,&
      &1.2667731785e+00, 1.2924289167e+00, 1.3183903694e+00, 1.3446608852e+00, 1.3712477957e+00,&
      &1.3981563871e+00, 1.4253913738e+00, 1.4529577128e+00, 1.4808618200e+00, 1.5091071209e+00,&
      &1.5377010994e+00, 1.5666442926e+00, 1.5959461174e+00, 1.6256081421e+00, 1.6556342316e+00,&
      &1.6860290326e+00, 1.7167962023e+00, 1.7479422800e+00, 1.7794647795e+00, 1.8113720546e+00,&
      &1.8436677210e+00, 1.8763525225e+00, 1.9094285706e+00, 1.9429019900e+00, 1.9767734056e+00,&
      &2.0110443625e+00, 2.0457182311e+00, 2.0807966319e+00, 2.1162835625e+00, 2.1521793686e+00,&
      &2.1884851921e+00, 2.2252031845e+00, 2.2623352316e+00, 2.2998804590e+00, 2.3378418317e+00,&
      &2.3762199212e+00, 2.4150171255e+00, 2.4542309185e+00, 2.4938614358e+00, 2.5339116631e+00,&
      &2.5743800215e+00, 2.6152669044e+00, 2.6565722192e+00, 2.6982965745e+00, 2.7404383909e+00,&
      &2.7829965492e+00, 2.8259671182e+00, 2.8693561571e+00, 2.9131582283e+00, 2.9573740283e+00,&
      &3.0019977815e+00, 3.0470327005e+00, 3.0924735966e+00, 3.1383211676e+00, 3.1845695946e+00,&
      &3.2312216163e+00, 3.2782705498e+00, 3.3257167944e+00, 3.3735537483e+00, 3.4217812152e+00,&
      &3.4703973337e+00, 3.5193999113e+00, 3.5687790556e+00, 3.6185369667e+00, 3.6686679089e+00,&
      &3.7191676636e+00, 3.7700329204e+00, 3.8212576741e+00, 3.8728442608e+00, 3.9247784893e+00,&
      &3.9770599609e+00, 4.0296850779e+00, 4.0826509389e+00, 4.1359478497e+00, 4.1895732493e+00,&
      &4.2435178829e+00, 4.2977836701e+00, 4.3523568309e+00, 4.4072342127e+00, 4.4624077309e+00,&
      &4.5178811019e+00, 4.5736352181e+00, 4.6296705022e+00, 4.6859785320e+00, 4.7425530452e+00,&
      &4.7993820830e+00, 4.8564669877e+00, 4.9137938363e+00, 4.9713580896e+00, 5.0291539907e+00,&
      &5.0871696841e+00, 5.1453958003e+00, 5.2038293725e+00, 5.2624604468e+00, 5.3212810296e+00,&
      &5.3802785569e+00, 5.4394460547e+00, 5.4987774256e+00, 5.5582622548e+00, 5.6178909842e+00,&
      &5.6776522632e+00, 5.7375438111e+00, 5.7975426729e+00, 5.8576480913e+00, 5.9178523245e+00,&
      &5.9781450558e+00, 6.0385038068e+00, 6.0989241029e+00, 6.1594077559e+00, 6.2199261933e+00,&
      &6.2804830412e+00, 6.3410577382e+00, 6.4016335712e+00, 6.4622207555e+00, 6.5227871011e+00,&
      &6.5833248556e+00, 6.6438253823e+00, 6.7042818024e+00, 6.7646699139e+00, 6.8249847347e+00,&
      &6.8852107733e+00, 6.9453423091e+00, 7.0053628702e+00, 7.0652488433e+00, 7.1249998583e+00,&
      &7.1846060796e+00, 7.2440336548e+00, 7.3032822784e+00, 7.3623464114e+00, 7.4212037279e+00,&
      &7.4798340597e+00, 7.5382228954e+00, 7.5963720457e+00, 7.6542513395e+00, 7.7118502283e+00,&
      &7.7691605036e+00, 7.8261502768e+00, 7.8828224288e+00, 7.9391535788e+00, 7.9951250378e+00,&
      &8.0507272934e+00, 8.1059472519e+00, 8.1607540878e+00, 8.2151435761e+00, 8.2690963509e+00,&
      &8.3226006384e+00, 8.3756308800e+00, 8.4281734150e+00, 8.4802177051e+00, 8.5317267797e+00,&
      &8.5827809751e+00 /)

      pblast_data%shtp(9,1:256) = (/&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-02, 0.00000000000e-02,&
      &0.00000000000e-02, 2.5403585557e-03, 3.2518432381e-02, 6.2501672243e-02, 9.2475450017e-02,&
      &1.2242513758e-01, 1.5233610680e-01, 1.8219372956e-01, 2.1198337773e-01, 2.4169042319e-01,&
      &2.7130023782e-01, 3.0079819348e-01, 3.3016966205e-01, 3.5940001542e-01, 3.8847462544e-01,&
      &4.1737886401e-01, 4.4609810299e-01, 4.7461771426e-01, 5.0292763233e-01, 5.3103699358e-01,&
      &5.5895678392e-01, 5.8669477003e-01, 6.1426536903e-01, 6.4167845800e-01, 6.6894215771e-01,&
      &6.9606898432e-01, 7.2306673652e-01, 7.4995066962e-01, 7.7672411208e-01, 8.0339924065e-01,&
      &8.2998636857e-01, 8.5649601759e-01, 8.8293261231e-01, 9.0930982242e-01, 9.3563504356e-01,&
      &9.6191538777e-01, 9.8816253176e-01, 1.0143835442e+00, 1.0405854875e+00, 1.0667788642e+00,&
      &1.0929699694e+00, 1.1191671915e+00, 1.1453803749e+00, 1.1716113426e+00, 1.1978719614e+00,&
      &1.2241668281e+00, 1.2505058799e+00, 1.2768949605e+00, 1.3033418140e+00, 1.3298490040e+00,&
      &1.3564291962e+00, 1.3830835445e+00, 1.4098175499e+00, 1.4366433968e+00, 1.4635603474e+00,&
      &1.4905757110e+00, 1.5177015138e+00, 1.5449314044e+00, 1.5722817287e+00, 1.5997527312e+00,&
      &1.6273491427e+00, 1.6550705262e+00, 1.6829360495e+00, 1.7109338839e+00, 1.7390763561e+00,&
      &1.7673709646e+00, 1.7958182092e+00, 1.8244168213e+00, 1.8531791500e+00, 1.8821050837e+00,&
      &1.9111945484e+00, 1.9404542951e+00, 1.9698880426e+00, 1.9995009951e+00, 2.0292893478e+00,&
      &2.0592595683e+00, 2.0894138514e+00, 2.1197513499e+00, 2.1502832500e+00, 2.1810029755e+00,&
      &2.2119109915e+00, 2.2430174385e+00, 2.2743192664e+00, 2.3058204642e+00, 2.3375169565e+00,&
      &2.3694101386e+00, 2.4015107883e+00, 2.4338073766e+00, 2.4663118890e+00, 2.4990138123e+00,&
      &2.5319210763e+00, 2.5650290823e+00, 2.5983415577e+00, 2.6318604447e+00, 2.6655785811e+00,&
      &2.6994966938e+00, 2.7336195166e+00, 2.7679452791e+00, 2.8024676433e+00, 2.8371936924e+00,&
      &2.8721122649e+00, 2.9072330756e+00, 2.9425449551e+00, 2.9780537936e+00, 3.0137517793e+00,&
      &3.0496453110e+00, 3.0857209173e+00, 3.1219773988e+00, 3.1584230942e+00, 3.1950505990e+00,&
      &3.2318529869e+00, 3.2688332450e+00, 3.3059842055e+00, 3.3433108468e+00, 3.3807982105e+00,&
      &3.4184474222e+00, 3.4562548460e+00, 3.4942222865e+00, 3.5323335319e+00, 3.5705988462e+00,&
      &3.6089998582e+00, 3.6475416714e+00, 3.6862182901e+00, 3.7250288037e+00, 3.7639563271e+00,&
      &3.8030106652e+00, 3.8421713821e+00, 3.8814469048e+00, 3.9208210789e+00, 3.9602959483e+00,&
      &3.9998667215e+00, 4.0395242107e+00, 4.0792470222e+00, 4.1190656268e+00, 4.1589324812e+00,&
      &4.1988686938e+00, 4.2388645068e+00, 4.2788981992e+00, 4.3189860419e+00, 4.3590974383e+00,&
      &4.3992456289e+00, 4.4393973966e+00, 4.4795725050e+00, 4.5197516236e+00, 4.5599175137e+00,&
      &4.6000826977e+00, 4.6402260096e+00, 4.6803294261e+00, 4.7204117683e+00, 4.7604373575e+00,&
      &4.8004164158e+00, 4.8403242183e+00, 4.8801611991e+00, 4.9199191503e+00, 4.9595806587e+00,&
      &4.9991482166e+00, 5.0386079073e+00, 5.0779353643e+00, 5.1171459226e+00, 5.1562034902e+00,&
      &5.1951834143e+00 /)

      pblast_data%shtp(10,1:256) = (/&
      &0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01,&
      &0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01,&
      &0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01,&
      &0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01,&
      &0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+01,&
      &0.00000000000e+01, 0.00000000000e+01, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00, 0.00000000000e+00,&
      &0.00000000000e+00, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-01,&
      &0.00000000000e-01, 0.00000000000e-01, 0.00000000000e-02, 1.0661128012e-02, 7.3492189064e-02,&
      &1.3352715047e-01, 1.9082468523e-01, 2.4544346634e-01, 2.9744216681e-01, 3.4687945964e-01,&
      &3.9381401783e-01, 4.3830451437e-01, 4.8040962228e-01, 5.2018801454e-01, 5.5769836417e-01,&
      &5.9299934416e-01, 6.2614962751e-01, 6.5720788723e-01, 6.8623279632e-01, 7.1328302777e-01,&
      &7.3841725459e-01, 7.6169414978e-01, 7.8317238633e-01, 8.0291063726e-01, 8.2096757556e-01,&
      &8.3740187423e-01, 8.5227220627e-01, 8.6563724469e-01, 8.7755566249e-01, 8.8808613266e-01,&
      &8.9728732820e-01, 9.0521792213e-01, 9.1193658743e-01, 9.1750199712e-01, 9.2197282418e-01,&
      &9.2540774163e-01, 9.2786542246e-01, 9.2940453967e-01, 9.3008376627e-01, 9.2996177526e-01,&
      &9.2909723963e-01, 9.2754883239e-01, 9.2537522653e-01, 9.2263509507e-01, 9.1938711100e-01,&
      &9.1568994732e-01, 9.1160227703e-01, 9.0718277314e-01, 9.0249010864e-01, 8.9758295653e-01,&
      &8.9251998982e-01, 8.8735988151e-01, 8.8216130460e-01, 8.7698293208e-01, 8.7188343697e-01,&
      &8.6692149226e-01, 8.6215577094e-01, 8.5764494604e-01, 8.5344769053e-01, 8.4962267743e-01,&
      &8.4622857974e-01, 8.4332407045e-01, 8.4096782257e-01, 8.3921850910e-01, 8.3813444140e-01,&
      &8.3773489176e-01, 8.3801479578e-01, 8.3896153577e-01, 8.4056950969e-01, 8.4282738304e-01,&
      &8.4573642127e-01, 8.4927872924e-01, 8.5344883031e-01, 8.5824062181e-01, 8.6364396090e-01,&
      &8.6965056740e-01, 8.7625442967e-01, 8.8344666199e-01, 8.9121683615e-01, 8.9955688243e-01,&
      &9.0846086169e-01, 9.1792197538e-01, 9.2792399443e-01, 9.3846612796e-01, 9.4953808248e-01,&
      &9.6112522478e-01, 9.7322649347e-01, 9.8583036628e-01, 9.9892887197e-01, 1.0125136801e+00,&
      &1.0265734270e+00, 1.0411007734e+00, 1.0560904808e+00, 1.0715276202e+00, 1.0874045370e+00,&
      &1.1037157875e+00, 1.1204496562e+00, 1.1375974672e+00, 1.1551488881e+00, 1.1730996581e+00,&
      &1.1914362837e+00, 1.2101519860e+00, 1.2292335652e+00, 1.2486734506e+00, 1.2684671183e+00,&
      &1.2886007280e+00, 1.3090630335e+00, 1.3298518867e+00, 1.3509469086e+00, 1.3723484979e+00,&
      &1.3940456048e+00, 1.4160248976e+00, 1.4382773728e+00, 1.4607975522e+00, 1.4835686402e+00,&
      &1.5065880102e+00, 1.5298388252e+00, 1.5533197585e+00, 1.5770166093e+00, 1.6009192803e+00,&
      &1.6250211538e+00, 1.6493031720e+00, 1.6737644409e+00, 1.6983939117e+00, 1.7231754370e+00,&
      &1.7481104458e+00, 1.7731756382e+00, 1.7983687758e+00, 1.8236791004e+00, 1.8490928224e+00,&
      &1.8746028372e+00, 1.9001984209e+00, 1.9258704773e+00, 1.9516015706e+00, 1.9773910487e+00,&
      &2.0032237675e+00, 2.0290905063e+00, 2.0549766693e+00, 2.0808782068e+00, 2.1067795391e+00,&
      &2.1326698558e+00, 2.1585435210e+00, 2.1843846537e+00, 2.2101882886e+00, 2.2359306483e+00,&
      &2.2616184105e+00, 2.2872307443e+00, 2.3127578426e+00, 2.3381865489e+00, 2.3635147714e+00,&
      &2.3887211957e+00, 2.4137981876e+00, 2.4387372756e+00, 2.4635238438e+00, 2.4881534045e+00,&
      &2.5126003759e+00, 2.5368733908e+00, 2.5609438856e+00, 2.5848097671e+00, 2.6084571768e+00,&
      &2.6318757754e+00, 2.6550497004e+00, 2.6779715943e+00, 2.7006335707e+00, 2.7230154368e+00,&
      &2.7451122458e+00, 2.7669105238e+00, 2.7883996265e+00, 2.8095647015e+00, 2.8303924184e+00,&
      &2.8508834734e+00, 2.8710097719e+00, 2.8907682112e+00, 2.9101505815e+00, 2.9291332741e+00,&
      &2.9477030357e+00 /)


   end subroutine



!! \ this subroutine is returning blast characteristic parameters {p_inci,p_refl,ta,t0,decay_inci,decay_refl}
!! \ to build a blast time history. parameters are determined from free air model
   subroutine pblast_parameters__free_air( pblast,z, w13, tdet,          &
   &                                       fac_p_bb, fac_i_bb, fac_t_bb, &
   &                                       is_decay_to_be_computed,      &
   &                                       output_params )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
      use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type (pblast_),intent(inout) :: pblast
      logical,intent(in) :: is_decay_to_be_computed
      my_real,intent(in) :: z,w13,tdet
      my_real,intent(in) :: fac_p_bb, fac_i_bb, fac_t_bb
      type(friedlander_params_), intent(out) :: output_params
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: phi_i,niter,iter
      my_real :: bound1,bound2,phi_db,lambda,logres,tol,zeta,res,tmp2,tmp3,kk,funct,diff
      my_real :: p_inci,p_refl,i_inci,i_refl,t_a,dt_0,decay_inci,decay_refl
      my_real :: cst_255_div_ln_z1_on_zn,log10_,z1_
      data cst_255_div_ln_z1_on_zn/-38.147316611455952998/
      data log10_ /2.30258509299405000000/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------
!   p r e - c o n d i t i o n
!-----------------------------------------------
!      z>0.5 .and. z<400.
!        otherwise z set to 0.5 as lower bound and 400. as upperbound
!-----------------------------------------------
!   s o u r c e   c o d e
!-----------------------------------------------
      !finding index for ufc table (figure 2-7) using bijection.
      z1_ = half
      phi_db = zero
      phi_i = 1
      lambda = zero
      if(z>0.5 .and. z<400.)then
         phi_db = log(z1_/z)*cst_255_div_ln_z1_on_zn
         phi_i  = 1 + int(phi_db)
         bound1 = pblast%pblast_data%rw3(phi_i)
         bound2 = pblast%pblast_data%rw3(phi_i+1)
         lambda = (z-bound1) / (bound2-bound1)
      elseif(z <= 0.5)then
         lambda = zero
         phi_i  = 1
      elseif(z > 400.)then
         lambda = one
         phi_i  = 255
      endif

      !=== spherical charge in free field ===!

      !incident upper pressure (ufc table from figure 2-7)
      bound1 = pblast%pblast_data%pso(phi_i)
      bound2 = pblast%pblast_data%pso(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      p_inci = exp(logres*log10_)
      !incident lower pressure (ufc table from figure 2-7)
      !bound1 = pblast_data%pso_(phi_i)
      !bound2 = pblast_data%pso_(phi_i+1)
      !logres = log10(bound1) + lambda*log10(bound2/bound1)
      !p_inci_ = exp(logres*log10_)

      !incident upper impulse (ufc table from figure 2-7)
      bound1 = pblast%pblast_data%iso(phi_i)
      bound2 = pblast%pblast_data%iso(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      i_inci = exp(logres*log10_)
      !incident lower impulse (ufc table from figure 2-7)
      !bound1 = pblast_data%iso_(phi_i)
      !bound2 = pblast_data%iso_(phi_i+1)
      !logres = log10(bound1) + lambda*log10(bound2/bound1)
      !i_inci_ = exp(logres*log10_)

      !reflected upper pressure (ufc table from figure 2-7)
      bound1 = pblast%pblast_data%pr(phi_i)
      bound2 = pblast%pblast_data%pr(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      p_refl = exp(logres*log10_)
      !reflected lower pressure (ufc table from figure 2-7)
      !bound1 = pblast_data%pr_(phi_i)
      !bound2 = pblast_data%pr_(phi_i+1)
      !logres = log10(bound1) + lambda*log10(bound2/bound1)
      !p_refl_ = exp(logres*log10_)
      !reflected upper impulse (ufc table from figure 2-7)

      bound1 = pblast%pblast_data%irefl(phi_i)
      bound2 = pblast%pblast_data%irefl(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      i_refl = exp(logres*log10_)
      !reflected lower impulse (ufc table from figure 2-7)
      !bound1 = pblast_data%irefl_(phi_i)
      !bound2 = pblast_data%irefl_(phi_i+1)
      !logres = log10(bound1) + lambda*log10(bound2/bound1)
      !i_refl_ = exp(logres*log10_)

      !first time for which p=p0 after t_arrival (ufc table from figure 2-7)
      bound1 = pblast%pblast_data%t0(phi_i)
      bound2 = pblast%pblast_data%t0(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      dt_0 = exp(logres*log10_)
      !second time for which p=p0 after t_arrival (ufc table from figure 2-7)
      !bound1 = pblast_data%t0_(phi_i)
      !bound2 = pblast_data%t0_(phi_i+1)
      !logres = log10(bound1) + lambda*log10(bound2/bound1)
      !dt_0_ = exp(logres*log10_)

      !time arrival (ufc table from figure 2-7)
      bound1 = pblast%pblast_data%ta(phi_i)
      bound2 = pblast%pblast_data%ta(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      t_a = exp(logres*log10_)

      !switch from normalized values. (pressure are not scaled by w13 in tables)
      i_inci  = i_inci * w13
      !i_inci_ = i_inci_* w13
      i_refl  = i_refl * w13
      !i_refl_ = i_refl_* w13
      if(dt_0 /= ep20)dt_0 = dt_0 * w13
      !if(dt_0_ /= ep20)dt_0_ = dt_0_ * w13
      if(t_a /= ep20)t_a = t_a * w13

      !---decay  ('b' parameter in modified friedlander model)
      !    iterative solver : it can be solved in same unit system as hardcoded tables {g,cm,µs} since b has no dimension
      if(.not. is_decay_to_be_computed)then
         !-friedlander
         decay_inci = one
         decay_refl = one

      else
         !solve decay (b):    i_inci = p_inci*dt_0/b*(one-(1-exp(-b))/b)
         !     g: b-> i_inci - p_inci*dt_0/b*(one-(1-exp(-b))/b)
         ! find b such as g(b)=0
         ! newton iterations
         niter=20
         tol=em06
         iter=0
         zeta=one
         res=ep20
         tmp2= p_inci*dt_0*exp(-zeta)/zeta/zeta
         !--initialize first iteration
         kk=p_inci*dt_0
         funct = half*kk -i_inci !-one_over_6*kk*zeta
         !--iterative solving
         do while (iter<=niter .and. res>tol)
            iter=iter+1
            if(abs(zeta) < em06)then
               !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
               diff = kk*(-one_over_6 + one_over_12*zeta)
               zeta = zeta - funct/diff
               funct = half*kk-one_over_6*kk*zeta - i_inci
            else
               diff = zeta*tmp2*exp(zeta) - (funct+i_inci)*(one + two/zeta)
               zeta = zeta - funct/diff
               tmp2= p_inci*dt_0*exp(-zeta)/zeta/zeta
               tmp3 = exp(zeta)*(zeta-one)+one
               funct = tmp2 * tmp3 -i_inci
            endif
            res = abs(funct)   !g(x_new)
         enddo
         decay_inci=max(zero,zeta)   ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)

         iter=0
         zeta=one
         res=ep20
         tmp2= p_refl*dt_0*exp(-zeta)/zeta/zeta
         !--initialize first iteration
         kk=p_refl*dt_0
         funct = half*kk -i_refl !-one_over_6*kk*zeta
         !--iterative solving
         do while (iter<=niter .and. res>tol)
            iter=iter+1
            if(abs(zeta) < em06)then
               !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
               diff = kk*(-one_over_6 + one_over_12*zeta)
               zeta = zeta - funct/diff
               funct = half*kk-one_over_6*kk*zeta - i_refl
            else
               diff = zeta*tmp2*exp(zeta) - (funct+i_refl)*(one + two/zeta)
               zeta = zeta - funct/diff
               tmp2= p_refl*dt_0*exp(-zeta)/zeta/zeta
               tmp3 = exp(zeta)*(zeta-one)+one
               funct = tmp2 * tmp3 -i_refl
            endif
            res = abs(funct)   !g(x_new)
         enddo
         decay_refl=max(zero,zeta)   ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)
      endif

      !conversion units !
      !g,cm,mus,bar -> working unit system
      p_inci  =  p_inci / fac_p_bb
      i_inci  =  i_inci / fac_i_bb
      p_refl  =  p_refl / fac_p_bb
      i_refl  =  i_refl / fac_i_bb
      !p_inci_ =  p_inci_ / fac_p_bb
      !i_inci_ =  i_inci_ / fac_i_bb
      !p_refl_ =  p_refl_ / fac_p_bb
      !i_refl_ =  i_refl_ / fac_i_bb
      if(dt_0 /= ep20)dt_0    =  dt_0    / fac_t_bb
      !if(dt_0_ /= ep20)dt_0_   =  dt_0_   / fac_t_bb
      if(t_a /= ep20)then
         t_a     =  t_a     / fac_t_bb
         t_a    = t_a + tdet
      endif

      output_params%p_inci = p_inci
      output_params%i_inci = i_inci
      output_params%p_refl = p_refl
      output_params%i_refl = i_refl
      output_params%dt_0 = dt_0
      output_params%t_a = t_a
      output_params%decay_inci = decay_inci
      output_params%decay_refl = decay_refl

   end subroutine pblast_parameters__free_air


!! \ this subroutine is returning blast characteristic parameters {p_inci,p_refl,ta,t0,decay_inci,decay_refl}
!! \ to build a blast time history. parameters are determined from surface burst model
   subroutine pblast_parameters__surface_burst(  pblast,z, w13, tdet,         &
   &                                            fac_p_bb, fac_i_bb, fac_t_bb, &
   &                                            is_decay_to_be_computed,      &
   &                                            output_params)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
      use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type(pblast_), intent(inout) :: pblast
      logical,intent(in) :: is_decay_to_be_computed
      my_real,intent(in) :: z,w13,tdet
      my_real,intent(in) :: fac_p_bb, fac_i_bb, fac_t_bb
      type(friedlander_params_), intent(out) :: output_params
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: phi_i,niter,iter
      my_real :: bound1,bound2,phi_db,lambda,logres,tol,zeta,res,tmp2,tmp3,kk,funct,diff
      my_real :: p_inci,p_refl,i_inci,i_refl,t_a,dt_0,decay_inci,decay_refl
      my_real :: cst_255_div_ln_z1_on_zn,log10_,z1_
      data cst_255_div_ln_z1_on_zn/-38.147316611455952998/
      data log10_ /2.30258509299405000000/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------
!   p r e - c o n d i t i o n
!-----------------------------------------------
!      z>0.5 .and. z<400.
!        otherwise z set to 0.5 as lower bound and 400. as upperbound
!-----------------------------------------------
!   s o u r c e   c o d e
!-----------------------------------------------
      !finding index for ufc table (figure 2-7) using bijection.
      phi_i = 1
      lambda = zero
      z1_ = half
      !finding index for ufc table (figure 2-15) using bijection.
      if(z > 0.5 .and. z < 400.) then
         phi_db = log(z1_/z)*cst_255_div_ln_z1_on_zn
         phi_i  = 1 + int(phi_db)
         bound1 = pblast%pblast_data%rw3_surf(phi_i)
         bound2 = pblast%pblast_data%rw3_surf(phi_i+1)
         lambda = (z-bound1) / (bound2-bound1)
      elseif(z <= 0.5)then
         lambda = zero
         phi_i  = 1
      elseif(z > 400.)then
         lambda = one
         phi_i  = 255
      endif


      !=== hemispherical charge with ground reflection ===!

      !incident upper pressure (ufc table from figure 2-15)
      bound1 = pblast%pblast_data%pso_surf(phi_i)
      bound2 = pblast%pblast_data%pso_surf(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      p_inci = exp(logres*log10_)

      !incident upper impulse (ufc table from figure 2-15)
      bound1 = pblast%pblast_data%iso_surf(phi_i)
      bound2 = pblast%pblast_data%iso_surf(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      i_inci = exp(logres*log10_)

      !reflected upper pressure (ufc table from figure 2-15)
      bound1 = pblast%pblast_data%pr_surf(phi_i)
      bound2 = pblast%pblast_data%pr_surf(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      p_refl = exp(logres*log10_)
      !reflected upper impulse (ufc table from figure 2-15)
      bound1 = pblast%pblast_data%ir_surf(phi_i)
      bound2 = pblast%pblast_data%ir_surf(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      i_refl = exp(logres*log10_)

      !first time for which p=p0 after t_arrival (ufc table from figure 2-15)
      bound1 = pblast%pblast_data%t0_surf(phi_i)
      bound2 = pblast%pblast_data%t0_surf(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      dt_0 = exp(logres*log10_)

      !time arrival (ufc table from figure 2-15)
      bound1 = pblast%pblast_data%ta_surf(phi_i)
      bound2 = pblast%pblast_data%ta_surf(phi_i+1)
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      t_a = exp(logres*log10_)

      !switch from normalized values.      ( pressure are not scaled by w13 in tables )
      i_inci  = i_inci * w13
      i_refl  = i_refl * w13
      dt_0    = dt_0   * w13
      t_a     = t_a    * w13

      !---decay  ('b' parameter in modified friedlander model)
      !    iterative solver : it can be solved in same unit system as hardcoded tables {g,cm,µs} since b has no dimension
      if(.not. is_decay_to_be_computed)then
         !-friedlander
         decay_inci = one
         decay_refl = one

      else
         !solve decay (b):    i_inci = p_inci*dt_0/b*(one-(1-exp(-b))/b)
         !     g: b-> i_inci - p_inci*dt_0/b*(one-(1-exp(-b))/b)
         ! find b such as g(b)=0
         ! newton iterations
         niter=20
         tol=em06
         iter=0
         zeta=one
         res=ep20
         tmp2= p_inci*dt_0*exp(-zeta)/zeta/zeta
         !--initialize first iteration
         kk=p_inci*dt_0
         funct = half*kk -i_inci !-one_over_6*kk*zeta
         !--iterative solving
         do while (iter<=niter .and. res>tol)
            iter=iter+1
            if(abs(zeta) < em06)then
               !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
               diff = kk*(-one_over_6 + one_over_12*zeta)
               zeta = zeta - funct/diff
               funct = half*kk-one_over_6*kk*zeta - i_inci
            else
               diff = zeta*tmp2*exp(zeta) - (funct+i_inci)*(one + two/zeta)
               zeta = zeta - funct/diff
               tmp2= p_inci*dt_0*exp(-zeta)/zeta/zeta
               tmp3 = exp(zeta)*(zeta-one)+one
               funct = tmp2 * tmp3 -i_inci
            endif
            res = abs(funct)   !g(x_new)
         enddo
         decay_inci=max(zero,zeta)    ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)

         iter=0
         zeta=one
         res=ep20
         tmp2= p_refl*dt_0*exp(-zeta)/zeta/zeta
         !--initialize first iteration
         kk=p_refl*dt_0
         funct = half*kk -i_refl !-one_over_6*kk*zeta
         !--iterative solving
         do while (iter<=niter .and. res>tol)
            iter=iter+1
            if(abs(zeta) < em06)then
               !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
               diff = kk*(-one_over_6 + one_over_12*zeta)
               zeta = zeta - funct/diff
               funct = half*kk-one_over_6*kk*zeta - i_refl
            else
               diff = zeta*tmp2*exp(zeta) - (funct+i_refl)*(one + two/zeta)
               zeta = zeta - funct/diff
               tmp2= p_refl*dt_0*exp(-zeta)/zeta/zeta
               tmp3 = exp(zeta)*(zeta-one)+one
               funct = tmp2 * tmp3 -i_refl
            endif
            res = abs(funct)   !g(x_new)
         enddo
         decay_refl=max(zero,zeta)    ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)

      endif! is_decay_to_be_computed


      !conversion units !
      !g,cm,mus,bar -> working unit system
      p_inci = p_inci / fac_p_bb
      i_inci = i_inci / fac_i_bb
      p_refl = p_refl / fac_p_bb
      i_refl = i_refl / fac_i_bb
      if(dt_0 /= ep20)dt_0    =  dt_0    / fac_t_bb
      if(t_a /= ep20)then
         t_a     =  t_a     / fac_t_bb
         t_a    = t_a + tdet
      endif

      output_params%p_inci = p_inci
      output_params%i_inci = i_inci
      output_params%p_refl = p_refl
      output_params%i_refl = i_refl
      output_params%dt_0 = dt_0
      output_params%t_a = t_a
      output_params%decay_inci = decay_inci
      output_params%decay_refl = decay_refl

   end subroutine pblast_parameters__surface_burst


!! \ this subroutine is returning blast characteristic parameters {p_inci,p_refl,ta,t0,decay_inci,decay_refl}
!! \to build a blast time history. parameters are determined from surface burst model
   subroutine pblast_parameters__air_burst( pblast,z_struct, zc, zg, angle_g, w13, tdet, &
   &                                        fac_p_bb, fac_i_bb, fac_t_bb,                &
   &                                        is_decay_to_be_computed,                     &
   &                                        id,label,is_output_enabled,                  &
   &                                        output_params )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
      use names_and_titles_mod, only: ncharline
      use constant_mod
      use file_descriptor_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
type(pblast_) :: pblast
      logical,intent(in) :: is_decay_to_be_computed, is_output_enabled
      integer,intent(in) :: id
      my_real,intent(in) :: w13,tdet,angle_g,z_struct,zc
      my_real,intent(inout) :: zg
      my_real,intent(in) :: fac_p_bb, fac_i_bb, fac_t_bb
      character*4,intent(in)::label  !'ebcs' or 'load'
      type(friedlander_params_), intent(out) :: output_params
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: niter,iter
      my_real :: bound1,bound2,lambda,logres,tol,zeta,res,tmp2,tmp3,kk,funct,diff
      my_real :: p_inci,p_refl,i_inci,i_refl,t_a,dt_0,decay_inci,decay_refl
      my_real :: cst_255_div_ln_z1_on_zn,log10_,z1_
      my_real tmp(3),alpha_angle,alpha_zc,alpha_zg,htp,ira,ira_refl,pra,pra_refl,shtp
      my_real fac_unit ! convert scaled distance from cm/g^1/3 to ft/lb^1/3
      logical calculate
      integer curve_id1,curve_id2,idx1,idx2,idx1_angle,idx2_angle,idx_zg1,idx_zg2,itmp
      data cst_255_div_ln_z1_on_zn/-38.147316611455952998/
      data log10_ /2.30258509299405000000/
      data fac_unit/3.966977216838196139019/
      character(len=ncharline) :: msgout
      integer,external :: dichotomic_search_r_desc,dichotomic_search_r_asc

!-----------------------------------------------
!   p r e - c o n d i t i o n
!-----------------------------------------------
!      z>0.5 .and. z<400.
!        otherwise z set to 0.5 as lower bound and 400. as upperbound
!-----------------------------------------------
!   s o u r c e   c o d e
!-----------------------------------------------
      !finding index for ufc table (figure 2-7) using bijection.
      z1_ = half
      tmp(1)=angle_g/pblast%pblast_data%delta_angle
      itmp=int(tmp(1))
      idx1_angle = 1+itmp   ! surjection angle -> idx1
      idx2_angle = min(idx1_angle+1,256)
      alpha_angle = (angle_g-itmp*pblast%pblast_data%delta_angle)/pblast%pblast_data%delta_angle  !interpolation factor between angle(idx1_angle) & angle(idx2_angle)


      ! -----------(zc,zc) => shtp (fig 2-13) ------------ !
      !  -- scaled height of triple point --
      ! curve_ids             1    2    3    4    5    6    7    8    9   10
      ! 10 curves for zc : {1.0; 1.5; 2.0; 2.5; 3.0; 3.5; 4.0; 5.0; 6.0; 7.0}    ft/lb**1/3
      !                    <--------curve_id=[2x-1]---------><----[x+3]----->
      !
      !--------------------------------------------------------------------------------------------
      ! --- 1. two retained curves & interpolation factor alpha_zc
      ! --------------------------
      curve_id1 = 0
      alpha_zc = zero
      if(zc < 4)then
         itmp = int( two*(zc)-one )
         curve_id1 =  max(1,itmp)
         curve_id2 = curve_id1+1
         if(itmp < 1)then
            !message out of bounds. curve 1 will be used. no extrapolation
            curve_id2=curve_id1 !=1
            alpha_zc = zero
         else
            alpha_zc = (zc - pblast%pblast_data%curve_val_2_13(curve_id1))
            alpha_zc = alpha_zc/  ( pblast%pblast_data%curve_val_2_13(curve_id2) - pblast%pblast_data%curve_val_2_13(curve_id1) )
         endif
      else
         itmp = int( zc+three )
         curve_id1 = int( min(10,itmp) )
         curve_id2 = curve_id1+1
         if(curve_id1 == 10)then
            !message out of bounds. curve 10 will be used. no extrapolation
            curve_id2=curve_id1 !=10
            alpha_zc = zero
         else
            alpha_zc = (zc - pblast%pblast_data%curve_val_2_13(curve_id1))
            alpha_zc = alpha_zc / ( pblast%pblast_data%curve_val_2_13(curve_id2) - pblast%pblast_data%curve_val_2_13(curve_id1) )
         endif
      endif

      ! --------------------------
      ! --- 2. abscissa (zg) index to read the plot and interpolate (alpha_zg)  :  idx_zg1, idx_zg2
      ! --------------------------
      !idx_zg1 is index in [1,256] for abscissa %shtp(curve_id,1:256)
      ! pblast_data%shdc(1:2,curve_id) are the shdc bounds for a given curve_id
      idx_zg1 = min(max(1,int((zg-7.9)/pblast%pblast_data%dshdc)+1),256)  !curve on pliot 2-13 are starting from 2 ft/lb**0.333, hardcoded table start from  7.9 cm/g**0.333
      idx_zg2 = max(1,min(256,idx_zg1+1))
      alpha_zg = zero
      if(zg < pblast%pblast_data%shdc(1,curve_id2))then
         zg =  pblast%pblast_data%shdc(1,curve_id2)
         msgout = ''
         write(msgout, FMT='(A,I0,A,E10.4,A,I0,A,E10.4,A)')&
         &' ** WARNING : /LOAD/PBLAST id=',ID,' SCALED HORIZONTAL DISTANCE ',ZG,&
         &' IS BELOW LOWER BOUND, FIGURE 2-13, CURVE=',&
         &curve_id2,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
         write(msgout(16:19),FMT='(A4)')label
         write(iout,FMT='(A)')msgout
         write(istdo,FMT='(A)')msgout
      endif
      if(zg > pblast%pblast_data%shdc(2,curve_id1))then
         zg =  pblast%pblast_data%shdc(2,curve_id1)
         if(is_output_enabled)then
            msgout = ''
            write(msgout,FMT='(A,I0,A,E10.4,A,I0,A,E10.4,A)')&
            &' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HORIZONTAL DISTANCE ', ZG ,&
            &' IS ABOVE UPPER BOUND, FIGURE 2-13, CURVE=',&
            &curve_id1,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
            write(msgout(16:19),fmt='(A4)')label
            write(iout,fmt='(A)')msgout
            write(istdo,fmt='(A)')msgout
         endif
      endif
      alpha_zg = (zg-pblast%pblast_data%shtp_abscissa(idx_zg1))/pblast%pblast_data%dshdc !abscissa interpolation

      ! --------------------------
      ! --- 3. read and interpolate ordinate : shtp
      ! --------------------------
      !scaled height of triple point shtp
      !tmp(1) : angle interpolation on curve_id1 figure 2_13
      tmp(1)=pblast%pblast_data%shtp(curve_id1,idx_zg1)
      tmp(2)=pblast%pblast_data%shtp(curve_id1,idx_zg2)
      tmp(1) = (one-alpha_zg)*tmp(1) + alpha_zg*tmp(2)
      !tmp(2) : interpolation on curve_id2 figure 2_13
      tmp(2)=pblast%pblast_data%shtp(curve_id2,idx_zg1)
      tmp(3)=pblast%pblast_data%shtp(curve_id2,idx_zg2)
      tmp(2) = (one-alpha_zg)*tmp(2) + alpha_zg*tmp(3)
      !interpolate now with scaled height of charge
      shtp = (one-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)
      htp = shtp*w13


      ! --------------------------
      ! --- 4. compare shtp with target height
      ! --------------------------
      !check if triple point is above the target centroid
      ! print warning otherwise and load with the same wave as below (may be updated later but in practise triple point is above the structure)
      !
      if(z_struct > shtp .and. is_output_enabled)then
         msgout = ''
         write(msgout, FMT='(A,I0,A,A,E10.4,A,E10.4,A)')&
         &' ** WARNING : /LOAD/PBLAST id=',ID,' TARGET FACE IS ABOVE THE TRIPLE POINT:',&
         &"Zface = ",Z_struct," > ",SHTP," = SHTP (ft/fb**1/3)"
         write(msgout(16:19),fmt='(A4)')label
         write(iout,fmt='(A)')msgout
         write(istdo,fmt='(A)')msgout
      endif


      ! -----------(alpha,zc) => pra (fig 2-9) ------------ !
      ! -- reflected pressure pra --
      !
      !--------------------------------------------------------------------------------------------
      ! --- 1. two retained curves & interpolation factor alpha_zc
      ! --------------------------

      ! pra =
      ! here curves are plot from zc=0.3 to zc=14.3 : need to calculate alpha_zc again since 10 curves are for different zc values : 0.3 "0.5" 0.8 etc....
      !   zc in ft/lb**0.33  (10 plots on fig 2-9 depending on zc)
      idx1 = max(1,dichotomic_search_r_asc(zc, pblast%pblast_data%curve_val_2_9, 10))
      calculate=.false.
      if(idx1 == 1)then
         if(zc <= pblast%pblast_data%curve_val_2_9(1))then
            alpha_zc = zero
            idx1 = 1
            idx2 = 1
            if(is_output_enabled)then
               msgout = ''
               write(msgout,FMT='(A,I0,A,I0,A,E10.4,A)')&
               &' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-9, CURVE=',1,&
               &' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
               write(msgout(16:19),FMT='(A4)')label
               write(iout,fmt='(A)')msgout
               write(istdo,fmt='(A)')msgout
            endif
         else
            idx1 = 1
            idx2 = 2
            calculate=.true.
         endif
      elseif(idx1 >= 10)then
         alpha_zc = zero
         idx1 = 10
         idx2 = 10
         if(is_output_enabled)then
            msgout = ''
            write(msgout,FMT='(A,I0,A,I0,A,E10.4)')&
            &' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-9, CURVE=',&
            &10,' HC=',ZC/FAC_UNIT
            write(msgout(16:19),FMT='(A4)')label
            write(iout,fmt='(A)')msgout
            write(istdo,fmt='(A)')msgout
         endif
      else
         idx2=idx1+1
         calculate=.true.
      endif
      if(calculate)then
         alpha_zc=(zc-pblast%pblast_data%curve_val_2_9(idx1))&
         &/ (pblast%pblast_data%curve_val_2_9(idx2)-pblast%pblast_data%curve_val_2_9(idx1)) !interpolation between 2 retained plots (out of 10 from fig 2-9, each one for a given zc value)
      endif

      curve_id1=idx1
      curve_id2=idx2
      !tmp(1) : angle interpolation on curve_id1 figure 2_9
      tmp(1)=pblast%pblast_data%pra(curve_id1,idx1_angle)
      tmp(2)=pblast%pblast_data%pra(curve_id1,idx2_angle)
      tmp(1)=(one-alpha_angle)*log10(tmp(1))+alpha_angle*log10(tmp(2))
      !tmp(2) : interpolation on curve_id2 figure 2_9
      tmp(2)=pblast%pblast_data%pra(curve_id2,idx1_angle)
      tmp(3)=pblast%pblast_data%pra(curve_id2,idx2_angle)
      tmp(2)=(one-alpha_angle)*log10(tmp(2))+alpha_angle*log10(tmp(3))
      !interpolate now with scaled height of charge
      pra = (one-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)
      pra = exp(pra*log10_)

      ! -----------(alpha,zc) => pra (fig 2-10) ------------ !
      ! -- scaled reflected impulse --
      !
      !--------------------------------------------------------------------------------------------
      ! --- 1. two retained curves & interpolation factor alpha_zc
      ! --------------------------

      idx1 = max(1,dichotomic_search_r_asc(zc, pblast%pblast_data%curve_val_2_10, 10))
      calculate=.false.
      if(idx1 == 1)then
         if(zc <= pblast%pblast_data%curve_val_2_10(1))then
            alpha_zc = zero
            idx1 = 1
            idx2 = 1
            if(is_output_enabled)then
               msgout = ''
               write(msgout,FMT='(A,I0,A,I0,A,E10.4,A)')&
               &'   ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-10, CURVE=',1,&
               &' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
               write(msgout(16:19),FMT='(A4)')label
               write(iout,FMT='(A)')msgout
               write(istdo,FMT='(A)')msgout
            endif
         else
            idx1 = 1
            idx2 = 2
            calculate=.true.
         endif
      elseif(idx1 >= 10)then
         alpha_zc = zero
         idx1 = 10
         idx2 = 10
         if(is_output_enabled)then
            msgout = ''
            write(msgout,FMT='(A,I0,A,I0,A,E10.4)')&
            &' ** WARNING : /LOAD/PBLAST id=',ID,'  SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-10, CURVE=',&
            &10,' HC=',ZC/FAC_UNIT
            WRITE(MSGOUT(16:19),FMT='(A4)')label
            write(iout,FMT='(A)')msgout
            write(istdo,FMT='(A)')msgout
         endif
      else
         idx2=idx1+1
         calculate=.true.
      endif

      if(calculate)then
         alpha_zc=(zc-pblast%pblast_data%curve_val_2_10(idx1))&
         &/ (pblast%pblast_data%curve_val_2_10(idx2)-pblast%pblast_data%curve_val_2_10(idx1)) !interpolation between 2 retained plots (out of 10 from fig 2-9, each one for a given zc value)
      endif

      curve_id1=idx1
      curve_id2=idx2

      ! --------------------------
      ! --- 2. interpolate sri on each curve
      ! --------------------------
      !tmp(1) : angle interpolation on curve_id1 figure 2_10
      tmp(1)=pblast%pblast_data%sri(curve_id1,idx1_angle)
      tmp(2)=pblast%pblast_data%sri(curve_id1,idx2_angle)
      tmp(1)=(one-alpha_angle)*log10(tmp(1))+alpha_angle*log10(tmp(2))
      !tmp(2) : interpolation on curve_id2 figure 2_10
      tmp(2)=pblast%pblast_data%sri(curve_id2,idx1_angle)
      tmp(3)=pblast%pblast_data%sri(curve_id2,idx2_angle)
      tmp(2)=(one-alpha_angle)*log10(tmp(2))+alpha_angle*log10(tmp(3))
      ! --------------------------
      ! --- 3. interpolate sri between 2 curves
      ! --------------------------
      !interpolate now with scaled height of charge
      ira = (one-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)
      ira = exp(ira*log10_)

      ! use pra as pso on figure 2-7 ; determine corresponding scaled distance ; read corresponding values pr, pso-, ta/w**1/3
      !
      !get pra
      ! searching in monotonic function : idx1 such as     pblast_data%pra(idx1) <= pra <  pblast_data%pra(idx1+1)
      idx1 = max(1,dichotomic_search_r_desc(pra, pblast%pblast_data%pso, 256))
      idx2 = min(idx1+1,256)
      bound1=log10(pblast%pblast_data%pso(idx1))
      bound2=log10(pblast%pblast_data%pso(idx2))
      lambda = (log10(pra)-bound1) / (bound2-bound1)
      !!deduce z (may be useful for debug)
      !---bound1 = log10(pblast_data%rw3(idx1))
      !---bound2 = log10(pblast_data%rw3(idx2))
      !---logres =  (one-lambda)*bound1+lambda*bound2
      !---z = exp(logres*log10_)
      !deduce pra_refl (=pr(z) where z=z(pra) )
      bound1 = pblast%pblast_data%pr(idx1)
      bound2 = pblast%pblast_data%pr(min(256,idx2))
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      pra_refl = exp(logres*log10_)
      !deduce ta
      bound1 = pblast%pblast_data%ta(idx1)
      bound2 = pblast%pblast_data%ta(min(256,idx2))
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      t_a = exp(logres*log10_)

      ! use ira as is on figure 2-7  ; determine corresponding scaled distance ; read corresponding values ir,ir-, t0/w**1/3, t0-/w**1/3
      !
      ! searching in monotonic function : idx1 such as     pblast_data%pra(idx1) <= pra <  pblast_data%pra(idx1+1)
      idx1 = max(1,dichotomic_search_r_desc(ira, pblast%pblast_data%iso, 256))
      idx2 = min(idx1+1,256)
      bound1=log10(pblast%pblast_data%iso(idx1))
      bound2=log10(pblast%pblast_data%iso(idx2))
      lambda = (log10(ira)-bound1) / (bound2-bound1)
      !deduce z (may be useful for debug)
      !---bound1 = log10(pblast_data%rw3(idx1))
      !---bound2 = log10(pblast_data%rw3(idx2))
      !---logres =  (one-lambda)*bound1+lambda*bound2
      !---z = exp(logres*log10_)
      !deduce ira_refl (=pr(z) where z=z(pra) )
      bound1 = pblast%pblast_data%irefl(idx1)
      bound2 = pblast%pblast_data%irefl(min(256,idx2))
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      ira_refl = exp(logres*log10_)
      !deduce t0
      bound1 = pblast%pblast_data%t0(idx1)
      bound2 = pblast%pblast_data%t0(min(256,idx2))
      logres = log10(bound1) + lambda*log10(bound2/bound1)
      dt_0 = exp(logres*log10_)

      p_inci = pra
      p_refl = pra_refl
      i_inci = ira
      i_refl = ira_refl

      !switch from normalized values.      ( pressure are not scaled by w13 in tables )
      i_inci  = i_inci * w13
      i_refl  = i_refl * w13
      if(dt_0 /= ep20)dt_0    = dt_0   * w13
      if(t_a /= ep20)t_a     = t_a    * w13

      !---decay  ('b' parameter in modified friedlander model)
      !    iterative solver : it can be solved in same unit system as hardcoded tables {g,cm,µs} since b has no dimension
      if(.not. is_decay_to_be_computed)then
         !-friedlander
         decay_inci = one
         decay_refl = one

      else
         !solve decay (b):    i_inci = p_inci*dt_0/b*(one-(1-exp(-b))/b)
         !     g: b-> i_inci - p_inci*dt_0/b*(one-(1-exp(-b))/b)
         ! find b such as g(b)=0
         ! newton iterations
         niter=20
         tol=em06
         iter=0
         zeta=one
         res=ep20
         tmp2= p_inci*dt_0*exp(-zeta)/zeta/zeta
         !--initialize first iteration
         kk=p_inci*dt_0
         funct = half*kk -i_inci !-one_over_6*kk*zeta
         !--iterative solving
         do while (iter<=niter .and. res>tol)
            iter=iter+1
            if(abs(zeta) < em06)then
               !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
               diff = kk*(-one_over_6 + one_over_12*zeta)
               zeta = zeta - funct/diff
               funct = half*kk-one_over_6*kk*zeta - i_inci
            else
               diff = zeta*tmp2*exp(zeta) - (funct+i_inci)*(one + two/zeta)
               zeta = zeta - funct/diff
               tmp2= p_inci*dt_0*exp(-zeta)/zeta/zeta
               tmp3 = exp(zeta)*(zeta-one)+one
               funct = tmp2 * tmp3 -i_inci
            endif
            res = abs(funct)   !g(x_new)
         enddo
         decay_inci=max(zero,zeta)    ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)

         iter=0
         zeta=one
         res=ep20
         tmp2= p_refl*dt_0*exp(-zeta)/zeta/zeta
         !--initialize first iteration
         kk=p_refl*dt_0
         funct = half*kk -i_refl !-one_over_6*kk*zeta
         !--iterative solving
         do while (iter<=niter .and. res>tol)
            iter=iter+1
            if(abs(zeta) < em06)then
               !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
               diff = kk*(-one_over_6 + one_over_12*zeta)
               zeta = zeta - funct/diff
               funct = half*kk-one_over_6*kk*zeta - i_refl
            else
               diff = zeta*tmp2*exp(zeta) - (funct+i_refl)*(one + two/zeta)
               zeta = zeta - funct/diff
               tmp2= p_refl*dt_0*exp(-zeta)/zeta/zeta
               tmp3 = exp(zeta)*(zeta-one)+one
               funct = tmp2 * tmp3 -i_refl
            endif
            res = abs(funct)   !g(x_new)
         enddo
         decay_refl=max(zero,zeta)    ! lower value may have few change on positive impulse but large change to negative impulse (potential unphysical solution)

      endif! is_decay_to_be_computed


      !conversion units !
      !g,cm,mus,bar -> working unit system
      p_inci = p_inci / fac_p_bb
      i_inci = i_inci / fac_i_bb
      p_refl = p_refl / fac_p_bb
      i_refl = i_refl / fac_i_bb
      if(dt_0 /= ep20)dt_0    =  dt_0    / fac_t_bb
      if(t_a /= ep20)then
         t_a =  t_a     / fac_t_bb
         t_a = t_a + tdet
      endif

      output_params%p_inci = p_inci
      output_params%i_inci = i_inci
      output_params%p_refl = p_refl
      output_params%i_refl = i_refl
      output_params%dt_0 = dt_0
      output_params%t_a = t_a
      output_params%decay_inci = decay_inci
      output_params%decay_refl = decay_refl

   end subroutine pblast_parameters__air_burst
end module pblast_mod
