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
      !||    damping_range_solid_mod   ../engine/source/general_controls/damping/damping_range_solid.F90
      !||--- called by ------------------------------------------------------
      !||    viscmain                  ../engine/source/materials/visc/viscmain.F
      !||====================================================================
      module damping_range_solid_mod
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
      !||    damping_range_solid   ../engine/source/general_controls/damping/damping_range_solid.F90
      !||--- called by ------------------------------------------------------
      !||    viscmain              ../engine/source/materials/visc/viscmain.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||====================================================================
        subroutine damping_range_solid(damp_buf,nel     ,nvarvis ,uvarvis ,nvar_damp,et     , &
                                       epspxx  ,epspyy  ,epspzz  ,epspxy  ,epspyz  ,epspzx  , &
                                       sv1     ,sv2     ,sv3     ,sv4     ,sv5     ,sv6     , &
                                       timestep,rho     ,soundsp ,young   ,shear_modulus)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: one,zero, third, four_over_3, half, two
          use elbufdef_mod , only: buf_damp_range_
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
          integer,                                   intent(in)    :: nvarvis                  !< number of variables in buf_visc
          integer,                                   intent(in)    :: nvar_damp                !< number of variables in buf_visc used for damping        
          my_real, dimension(nel,nvarvis) ,          intent(inout) :: uvarvis                  !< buffer for viscous variables
          my_real, dimension(nel) ,                  intent(in)    :: et                       !< tangent young modulus coefficient
          my_real,                                   intent(in)    :: timestep                 !< time step
          my_real,                                   intent(in)    :: young                    !< young modulus
          my_real,                                   intent(in)    :: shear_modulus            !< shear modulus
          my_real, dimension(nel) ,                  intent(in)    :: epspxx                   !< strain xx
          my_real, dimension(nel) ,                  intent(in)    :: epspyy                   !< strain yy
          my_real, dimension(nel) ,                  intent(in)    :: epspzz                   !< strain zz
          my_real, dimension(nel) ,                  intent(in)    :: epspxy                   !< strain xy
          my_real, dimension(nel) ,                  intent(in)    :: epspyz                   !< strain yz
          my_real, dimension(nel) ,                  intent(in)    :: epspzx                   !< strain zx
          my_real, dimension(nel) ,                  intent(inout) :: sv1                      !< damp stress xx
          my_real, dimension(nel) ,                  intent(inout) :: sv2                      !< damp stress yy
          my_real, dimension(nel) ,                  intent(inout) :: sv3                      !< damp stress zz
          my_real, dimension(nel) ,                  intent(inout) :: sv4                      !< damp stress xy
          my_real, dimension(nel) ,                  intent(inout) :: sv5                      !< damp stress yz
          my_real, dimension(nel) ,                  intent(inout) :: sv6                      !< damp stress zx   
          my_real, dimension(nel) ,                  intent(in)    :: rho                      !< density
          my_real, dimension(nel) ,                  intent(inout) :: soundsp                  !< sound speed    
          type(buf_damp_range_)   ,                  intent(in)    :: damp_buf                 !< damping frequency range buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, ii, offset
          my_real :: g, rbulk,h0(6),h(6),hp0,hp,dav
          my_real, dimension(nel) :: p, epxx, epyy, epzz, trace
          my_real, dimension(3)   :: aa, bb, gv, beta, aak, bbk, betak, kv
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          g  = zero
          rbulk = zero                                         
    
          do j=1,3              
        !   tangent young modulus is used - bb multiplied by tangent coefficient for each element                        
            gv(j)    = damp_buf%alpha(j)*shear_modulus         
            beta(j)  = one/damp_buf%tau(j)
            kv(j)    = damp_buf%alpha(j)*young            
            betak(j) = one/damp_buf%tau(j)
            g = g + gv(j)   
            rbulk = rbulk + kv(j)
            aa(j) = exp(-beta(j)*timestep)
            bb(j) = two*timestep*gv(j)*exp(-half*beta(j)*timestep)         
            aak(j) = exp(-betak(j)*timestep)
            bbk(j) = timestep*kv(j)*exp(-half*betak(j)*timestep)
          enddo 
!          
          do i=1,nel                                                     
!           spheric part 
            trace(i) = -(epspxx(i) + epspyy(i) + epspzz(i))
            dav = third*trace(i)
            p(i)   = zero 
                                                 
!           deviatoric part                                           
            epxx(i) = epspxx(i) + dav                                  
            epyy(i) = epspyy(i) + dav                                   
            epzz(i) = epspzz(i) + dav
!           
            sv1(i) = zero
            sv2(i) = zero
            sv3(i) = zero
            sv4(i) = zero
            sv5(i) = zero
            sv6(i) = zero
          enddo
!           
          do j= 1,3 
            offset = nvarvis - nvar_damp
            ii = offset + 7*(j-1)
            do i=1,nel                                                                    
              h0(1) = uvarvis(i,ii + 1)                         
              h0(2) = uvarvis(i,ii + 2)                         
              h0(3) = uvarvis(i,ii + 3)                         
              h0(4) = uvarvis(i,ii + 4)                         
              h0(5) = uvarvis(i,ii + 5)                         
              h0(6) = uvarvis(i,ii + 6)                          
              hp0   = uvarvis(i,ii + 7)                     
!
              h(1) = aa(j)*h0(1) + bb(j)*epxx(i)*et(i)                     
              h(2) = aa(j)*h0(2) + bb(j)*epyy(i)*et(i)                     
              h(3) = aa(j)*h0(3) + bb(j)*epzz(i)*et(i)                     
              h(4) = aa(j)*h0(4) + half*bb(j)*epspxy(i)*et(i)         
              h(5) = aa(j)*h0(5) + half*bb(j)*epspyz(i)*et(i)         
              h(6) = aa(j)*h0(6) + half*bb(j)*epspzx(i)*et(i)                   
              hp = aak(j)*hp0 + bbk(j)*trace(i)*et(i)
!
              uvarvis(i,ii + 1) = h(1)                        
              uvarvis(i,ii + 2) = h(2)                        
              uvarvis(i,ii + 3) = h(3)                        
              uvarvis(i,ii + 4) = h(4)                        
              uvarvis(i,ii + 5) = h(5)                        
              uvarvis(i,ii + 6) = h(6)                          
              uvarvis(i,ii + 7) = hp                      
!
              sv1(i) = sv1(i) + h(1)                                    
              sv2(i) = sv2(i) + h(2)                                    
              sv3(i) = sv3(i) + h(3)                                    
              sv4(i) = sv4(i) + h(4)                                    
              sv5(i) = sv5(i) + h(5)                                    
              sv6(i) = sv6(i) + h(6)
              p(i)   = p(i)   + hp       
!                                                             
            enddo
          enddo                                                     
!
          do i=1,nel
            sv1(i) = sv1(i) - p(i)                                       
            sv2(i) = sv2(i) - p(i)                                       
            sv3(i) = sv3(i) - p(i)                                                                                                     
            soundsp(i) = sqrt(soundsp(i)**2 + (four_over_3*g + rbulk)*et(i)/rho(i)) 
          enddo                                                    
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_range_solid
      end module damping_range_solid_mod
