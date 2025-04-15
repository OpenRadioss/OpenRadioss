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
      !||    eikonal_init_mixture_vel_mod   ../starter/source/initial_conditions/detonation/eikonal_ini_mixture_vel.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||====================================================================
      module eikonal_init_mixture_vel_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
      !||====================================================================
      !||    eikonal_init_mixture_vel         ../starter/source/initial_conditions/detonation/eikonal_ini_mixture_vel.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method     ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- calls      -----------------------------------------------------
      !||    material_is_high_explosive       ../starter/source/materials/material_is_high_explosive.F90
      !||--- uses       -----------------------------------------------------
      !||    material_is_high_explosive_mod   ../starter/source/materials/material_is_high_explosive.F90
      !||====================================================================
        subroutine eikonal_init_mixture_vel(lgth,vel,idx_ng,idx_i,multimat_id,mid, &
                                            ngroup,nummat,npropm,pm,npropmi,ipm,elbuf_tab,&
                                            nparg,iparg)
!! \brief Initialization of detonation velocity in case of mixture (multimaterial law)
!! \details laws 51 and 151
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod, only : elbuf_struct_
          use constant_mod , only : zero, one
          use multimat_param_mod , only : m51_n0phas, m51_nvphas
          use material_is_high_explosive_mod , only : material_is_high_explosive
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
          integer,intent(in) :: lgth,npropm,npropmi,nummat,ngroup !< array sizes
          integer,intent(in) :: multimat_id          !< tag for law 51 or 151
          integer,intent(in) :: mid                  !<internal material identifier
          my_real,intent(inout) :: vel(lgth)         !< velocity to initialize
          integer,intent(in) :: idx_ng(lgth)         !< group numbers
          integer,intent(in) :: idx_i(lgth)          !< local identifers in groups
          integer,intent(in) :: ipm(npropmi,nummat)  !< material buffer (integer)
          my_real,intent(in) :: pm(npropm,nummat)    !< material buffer (real)
          type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
          integer,intent(in) :: nparg
          integer,intent(in) :: iparg(nparg,ngroup) !< group parameters
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii
          integer :: ng, i
          integer :: nbmat
          integer :: submat_id, isubmat
          integer :: iu(4)
          integer :: nel
          integer :: submat_mlw(21)
          integer :: submat_mid(21)
          my_real :: vfrac(21)
          my_real :: detvel(21)
          my_real :: mix_detvel
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

           if(multimat_id == 151)then
             ! --- MATERIAL LAW 151 ---!
             do ii=1,lgth
               ng = idx_ng(ii)
               i = idx_i(ii)
               nbmat = ipm(20,mid)

               !harmonic mean of inverse velocities
               do isubmat = 1, nbmat
                 submat_id = ipm(20 + isubmat, mid)
                 submat_mlw(isubmat) = ipm(2,submat_id)
                 vfrac(isubmat) = elbuf_tab(ng)%bufly(isubmat)%lbuf(1,1,1)%vol(i) / elbuf_tab(ng)%gbuf%vol(i)
                 detvel(isubmat) = zero
                 if(material_is_high_explosive(submat_mlw(isubmat)))then
                   detvel(isubmat) = pm(38, submat_id)
                 end if
               end do
               mix_detvel = zero
               do isubmat = 1, nbmat
                 if(detvel(isubmat) > zero)then
                   mix_detvel = mix_detvel + vfrac(isubmat)/detvel(isubmat)
                 end if
               end do
               if(mix_detvel > zero) mix_detvel = one/mix_detvel
               vel(ii) = mix_detvel
             enddo

           elseif (multimat_id == 51)then
             ! --- MATERIAL LAW 51 ---!
             isubmat = 1 ; iu(1)=m51_n0phas+(isubmat-1)*m51_nvphas
             isubmat = 2 ; iu(2)=m51_n0phas+(isubmat-1)*m51_nvphas
             isubmat = 3 ; iu(3)=m51_n0phas+(isubmat-1)*m51_nvphas
             isubmat = 4 ; iu(4)=m51_n0phas+(isubmat-1)*m51_nvphas
             submat_mid(1) = ipm(20+1,mid)
             submat_mid(2) = ipm(20+2,mid)
             submat_mid(3) = ipm(20+3,mid)
             submat_mid(4) = ipm(20+4,mid)
             submat_mlw(1) = ipm(2,submat_mid(1))
             submat_mlw(2) = ipm(2,submat_mid(2))
             submat_mlw(3) = ipm(2,submat_mid(3))
             submat_mlw(4) = ipm(2,submat_mid(4))
             do ii=1,lgth
               ng = idx_ng(ii)
               i = idx_i(ii)
               nel = iparg(2,ng)
               vfrac(1) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(i+iu(1)*nel)
               vfrac(2) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(i+iu(2)*nel)
               vfrac(3) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(i+iu(3)*nel)
               vfrac(4) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(i+iu(4)*nel)
               detvel(1:4) = zero
               if(material_is_high_explosive(submat_mlw(1)))then
                 detvel(1) = pm(38, submat_mid(1))
               end if
               if(material_is_high_explosive(submat_mlw(2)))then
                 detvel(2) = pm(38, submat_mid(2))
               end if
               if(material_is_high_explosive(submat_mlw(3)))then
                 detvel(3) = pm(38, submat_mid(3))
               end if
               if(material_is_high_explosive(submat_mlw(4)))then
                 detvel(4) = pm(38, submat_mid(4))
               end if

               !harmonic mean of inverse velocities
               mix_detvel = zero
               do isubmat = 1,4
                 if(detvel(isubmat) > zero)then
                   mix_detvel = mix_detvel + vfrac(isubmat)/detvel(isubmat)
                 end if
               end do
               if(mix_detvel > zero) mix_detvel = one/mix_detvel
               vel(ii) = mix_detvel

             enddo
           endif
           
        end subroutine eikonal_init_mixture_vel
! ----------------------------------------------------------------------------------------------------------------------
        end module eikonal_init_mixture_vel_mod
