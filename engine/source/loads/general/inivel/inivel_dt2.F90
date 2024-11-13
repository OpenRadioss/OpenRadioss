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
      !||    inivel_dt2_mod   ../engine/source/loads/general/inivel/inivel_dt2.F90
      !||--- called by ------------------------------------------------------
      !||    resol            ../engine/source/engine/resol.F
      !||====================================================================
      module inivel_dt2_mod
!        
       contains
  !! \brief time step change due to inivel w/ T_start
      !||====================================================================
      !||    inivel_dt2     ../engine/source/loads/general/inivel/inivel_dt2.F90
      !||--- called by ------------------------------------------------------
      !||    resol          ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    spmd_max_i     ../engine/source/mpi/implicit/imp_spmd.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod   ../common_source/modules/constant_mod.F
      !||    inivel_mod     ../common_source/modules/inivel_mod.F90
      !||    sensor_mod     ../engine/share/modules/sensor_mod.F
      !||====================================================================
        subroutine inivel_dt2(ninivelt,inivel_t,sensors,time , dt2 ,nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use inivel_mod 
      use sensor_mod
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
      integer , intent(in   )                          :: nspmd     !< number of domain
      integer , intent(inout)                          :: ninivelt  !< dimension of inivel_t
      my_real, intent(in   )                           :: time      !< time
      my_real, intent(inout)                           :: dt2       !< time step
      type (sensors_) ,intent(in  )                    :: sensors
      type(inivel_), dimension(ninivelt),intent(in   ) :: inivel_t  !< inivel_struc 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: i,j,id,n,itype,iactiv,sens_id
      my_real  :: tstart,tstart_s,tstart1,time1
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!  if activation due to T_start or sensor
       iactiv = 0
       time1 = time +half*dt2
       tstart = -HUGE(tstart)
       sens_id = -HUGE(sens_id)
       do n =1,ninivelt 
         itype = inivel_t(n)%itype
         if (itype < 0 ) cycle 
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
         if (tstart1<=time1) iactiv = 1 
       end do 
       if (nspmd>1) call spmd_max_i(iactiv)
!       
       if (iactiv>0) dt2 = half*dt2
!       
       end subroutine inivel_dt2
      end module inivel_dt2_mod
