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
!Copyright>    !ommercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    visc_plas_mod   ../engine/source/materials/visc/visc_plas.F90
      !||--- called by ------------------------------------------------------
      !||    viscmain        ../engine/source/materials/visc/viscmain.F
      !||====================================================================
      module visc_plas_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes damping forces for /DAMP/FREQ_RANGE for solid elements
!=======================================================================================================================
!
      !||====================================================================
      !||    visc_plas        ../engine/source/materials/visc/visc_plas.F90
      !||--- called by ------------------------------------------------------
      !||    viscmain         ../engine/source/materials/visc/viscmain.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod     ../common_source/modules/constant_mod.F
      !||    visc_param_mod   ../common_source/modules/mat_elem/visc_param_mod.F90
      !||====================================================================
        subroutine visc_plas(visc,nel     , rho, soundsp     , &
                                       depspxx  ,depspyy  ,depspzz  ,depspxy  ,depspyz  ,depspzx  , &
                                       sigvoxx  ,sigvoyy  ,sigvozz  ,sigvoxy  ,sigvoyz  ,sigvozx  , &
                                       sigvxx   ,sigvyy  ,sigvzz  ,sigvxy  ,sigvyz  ,sigvzx  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: four_over_3, half, two, three,third
          USE visc_param_mod, ONLY: visc_param_
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
          integer,                                   intent(in)    :: nel                      !< number of elements
          my_real, dimension(nel) ,                  intent(in)    :: depspxx                   !< incremetal strain xx
          my_real, dimension(nel) ,                  intent(in)    :: depspyy                   !< incremenatal strain yy
          my_real, dimension(nel) ,                  intent(in)    :: depspzz                   !< strain zz
          my_real, dimension(nel) ,                  intent(in)    :: depspxy                   !< strain xy
          my_real, dimension(nel) ,                  intent(in)    :: depspyz                   !< strain yz
          my_real, dimension(nel) ,                  intent(in)    :: depspzx                   !< strain zx
          my_real, dimension(nel) ,                  intent(in) :: sigvoxx                      !< old damp stress xx
          my_real, dimension(nel) ,                  intent(in) :: sigvoyy                      !< old damp stress yy
          my_real, dimension(nel) ,                  intent(in) :: sigvozz                      !< old damp stress zz
          my_real, dimension(nel) ,                  intent(in) :: sigvoxy                      !< old damp stress xy
          my_real, dimension(nel) ,                  intent(in) :: sigvoyz                      !< old damp stress yz
          my_real, dimension(nel) ,                  intent(in) :: sigvozx                      !< old damp stress zx 
          my_real, dimension(nel) ,                  intent(out) :: sigvxx                      !< damp stress xx
          my_real, dimension(nel) ,                  intent(out) :: sigvyy                      !< damp stress yy
          my_real, dimension(nel) ,                  intent(out) :: sigvzz                      !< damp stress zz
          my_real, dimension(nel) ,                  intent(out) :: sigvxy                      !< damp stress xy
          my_real, dimension(nel) ,                  intent(out) :: sigvyz                      !< damp stress yz
          my_real, dimension(nel) ,                  intent(out) :: sigvzx                      !< damp stress zx   
          my_real, dimension(nel) ,                  intent(inout) :: soundsp                   !< sound speed   
          my_real, dimension(nel) ,                  intent(in) :: rho                          !< density
          type(visc_param_) ,                         intent(in) :: VISC 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          my_real :: g,sigy, depxx, depyy, depzz, dav, sigvmises, scale,twog,trace
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          g    =   VISC%UPARAM(1)   
          sigy =   VISC%UPARAM(2)  
          twog =  two*g            
          do i=1,nel                                                     
!           spheric part 
            trace = -(depspxx(i) + depspyy(i) + depspzz(i))
            dav = third*trace                 
!           deviatoric part                                           
            depxx = depspxx(i) + dav                                  
            depyy = depspyy(i) + dav                                   
            depzz = depspzz(i) + dav
!           ! we shoud check if deviatoric strain are used ? must be check with lsdyna result
            sigvxx(i) = sigvoxx(i) + twog*depxx
            sigvyy(i) = sigvoyy(i) + twog*depyy
            sigvzz(i) = sigvozz(i) + twog*depzz
            sigvxy(i) = sigvoxy(i) + g*depspxy(i)
            sigvyz(i) = sigvoyz(i) + g*depspyz(i)
            sigvzx(i) = sigvozx(i) + g*depspzx(i)
            ! update of sound speed
             soundsp(i) = sqrt(soundsp(i)**2 + (four_over_3*g )/rho(i)) 
          enddo
!           
          do i=1,nel
             sigvmises = three*(half*(sigvxx(i)**2  + sigvyy(i)**2 + sigvzz(i)**2)      & 
                             + sigvxy(i)**2 + sigvyz(i)**2 + sigvzx(i)**2 )
             sigvmises = sqrt(sigvmises)
             if( sigvmises > sigy ) then
                scale = sigy/sigvmises
                sigvxx(i) = scale*sigvxx(i)
                sigvyy(i) = scale*sigvyy(i)
                sigvzz(i) = scale*sigvzz(i)
                sigvxy(i) = scale*sigvxy(i)
                sigvyz(i) = scale*sigvyz(i)
                sigvzx(i) = scale*sigvzx(i)
            endif
          enddo
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine visc_plas
      end module visc_plas_mod
