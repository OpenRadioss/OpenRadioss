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
      !||    damping_range_shell_mod   ../engine/source/general_controls/damping/damping_range_shell.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc                    ../engine/source/materials/mat_share/mulawc.F90
      !||    mulawglc                  ../engine/source/materials/mat_share/mulawglc.F
      !||====================================================================
      module damping_range_shell_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes damping forces for /DAMP/FREQ_RANGE for shells - simlar as prony_modelc
!=======================================================================================================================
!
      !||====================================================================
      !||    damping_range_shell   ../engine/source/general_controls/damping/damping_range_shell.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc                ../engine/source/materials/mat_share/mulawc.F90
      !||    mulawglc              ../engine/source/materials/mat_share/mulawglc.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||====================================================================
        subroutine damping_range_shell(damp_buf,nel     ,nuvarv  ,nvar_damp,timestep ,           &
                                       rho0    ,soundsp ,young   ,shear_mod,                     &
                                       epspxx  ,epspyy  ,epspxy  ,epspyz  ,epspzx    ,           &
                                       sigvxx  ,sigvyy  ,sigvxy  ,sigvyz  ,sigvzx    ,           &
                                       uvarv   ,off     ,etse    ,flag_incr)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: one,zero, third, four_over_3, half, two,em20,three,two_third
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
          integer,                                   intent(in)    :: nuvarv                   !< number of variables in buf_visc
          integer,                                   intent(in)    :: nvar_damp                !< number of variables in buf_visc used for damping        
          integer,                                   intent(in)    :: flag_incr                !< flag for viscous stress increment
          my_real, dimension(nel,nuvarv) ,           intent(inout) :: uvarv                    !< buffer for viscous variables
          my_real,                                   intent(in)    :: timestep                 !< time step
          my_real,                                   intent(in)    :: young                    !< young modulus
          my_real,                                   intent(in)    :: shear_mod                !< shear modulus
          my_real, dimension(nel) ,                  intent(in)    :: epspxx                   !< strain xx
          my_real, dimension(nel) ,                  intent(in)    :: epspyy                   !< strain yy
          my_real, dimension(nel) ,                  intent(in)    :: epspxy                   !< strain xy
          my_real, dimension(nel) ,                  intent(in)    :: epspyz                   !< strain yz
          my_real, dimension(nel) ,                  intent(in)    :: epspzx                   !< strain zx
          my_real, dimension(nel) ,                  intent(inout) :: sigvxx                   !< damp stress xx
          my_real, dimension(nel) ,                  intent(inout) :: sigvyy                   !< damp stress yy
          my_real, dimension(nel) ,                  intent(inout) :: sigvxy                   !< damp stress zz
          my_real, dimension(nel) ,                  intent(inout) :: sigvyz                   !< damp stress xy
          my_real, dimension(nel) ,                  intent(inout) :: sigvzx                   !< damp stress yz 
          my_real, dimension(nel) ,                  intent(in)    :: rho0                     !< density
          my_real, dimension(nel) ,                  intent(inout) :: soundsp                  !< sound speed    
          my_real, dimension(nel) ,                  intent(in)    :: off                      !< off flag
          my_real, dimension(nel) ,                  intent(in)    :: etse                     !< tangent young modulus coefficient
          type(buf_damp_range_)   ,                  intent(in)    :: damp_buf                 !< damping frequency range buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, ii, offset
          my_real :: g, rbulk,h0(6),h(6,3),hp0,dav,fac,epxx, epyy, epzz,p,p_old
          my_real, dimension(nel) :: a1,a2,a3,epspzz,et
          my_real, dimension(3)   :: aa, bb, gv, kv, beta, aak, bbk, betak, hp
          my_real, dimension(6)   :: s,s_old
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          g = zero
          rbulk = zero
          offset = nuvarv - nvar_damp 
!       
          do j=1,3              
!           tangent young modulus is used - bb multiplied by tangent coefficient for each element                        
            gv(j)    = damp_buf%alpha(j)*shear_mod         
            beta(j)  = one/damp_buf%tau(j)
            kv(j)    = damp_buf%alpha(j)*young             
            betak(j) = one/damp_buf%tau(j)
            g = g + gv(j)   
            rbulk = rbulk + kv(j)
            aa(j) =  exp(-beta(j)*timestep) 
            bb(j) =  two*timestep*gv(j)*exp(-half*beta(j)*timestep)      
            aak(j) = exp(-betak(j)*timestep)
            bbk(j) = timestep*kv(j)*exp(-half*betak(j)*timestep)
          enddo    
!          
          if (flag_incr==0) then
!           call from mulawc            
            et(1:nel) = etse(1:nel)
          else
!           call from mulawglc                
            et(1:nel) = one + etse(1:nel)            
          endif
!          
          do i=1,nel
            a1(i) = zero
            a2(i) = zero
            a3(i) = zero
            epspzz(i) = zero        
            do j=1,3
              ii = offset + 7*(j-1) 
!             paramaters for computaion of epszz_dot (sigzz=0)
              h0(3) = uvarv(i,ii + 3)
              hp0   = uvarv(i,ii + 7)
              a1(i)  = a1(i) +  aa(j)*h0(3) -  aak(j)*hp0
              a2(i)  = a2(i) +  bb(j)*et(i)
              a3(i)  = a3(i) +  bbk(j)*et(i)
            enddo
          enddo  
!
!         compute epszz_dot  sig33= 0
!            
          do i=1,nel
            fac = one/max(em20,two_third*a2(i) + a3(i))
            epspzz(i) = -a1(i) + (third*a2(i)-a3(i))*(epspxx(i) + epspyy(i))
            epspzz(i) = fac*epspzz(i)
          enddo
!                
          do i=1,nel 
!           spherique part
            dav = third*(epspxx(i) + epspyy(i) + epspzz(i))
!           deviatorique part           
            epxx = epspxx(i) - dav
            epyy = epspyy(i) - dav
            epzz = epspzz(i) - dav          
!  
            do j= 1,3    
              ii = offset + 7*(j-1)     
              h0(1) = uvarv(i,ii + 1)
              h0(2) = uvarv(i,ii + 2)
              h0(3) = uvarv(i,ii + 3)
              h0(4) = uvarv(i,ii + 4)
              h0(5) = uvarv(i,ii + 5)
              h0(6) = uvarv(i,ii + 6)
              hp0   = uvarv(i,ii + 7)
!                    
              h(1,j) =    aa(j)*h0(1) + bb(j)*epxx*et(i)
              h(2,j) =    aa(j)*h0(2) + bb(j)*epyy*et(i)     
              h(3,j) =    aa(j)*h0(3) + bb(j)*epzz*et(i)
              h(4,j) =    aa(j)*h0(4) + half*bb(j)*epspxy(i)*et(i)       
              h(5,j) =    aa(j)*h0(5) + half*bb(j)*epspyz(i)*et(i)
              h(6,j) =    aa(j)*h0(6) + half*bb(j)*epspzx(i)*et(i)
              hp(j)  =    aak(j)*hp0  + bbk(j)*(-three*dav)*et(i)
!
              uvarv(i,ii + 1) = h(1,j)
              uvarv(i,ii + 2) = h(2,j)
              uvarv(i,ii + 3) = h(3,j)
              uvarv(i,ii + 4) = h(4,j)
              uvarv(i,ii + 5) = h(5,j)
              uvarv(i,ii + 6) = h(6,j)              
              uvarv(i,ii + 7) = hp(j)         
            enddo 
!     
!        comppute stress
!
            s(1:6) = zero
            p = zero
          
            do  j= 1,3
              s(1) = s(1) + h(1,j)
              s(2) = s(2) + h(2,j)
!c            s(3) = s(3) + h(3,j)
              s(4) = s(4) + h(4,j)
              s(5) = s(5) + h(5,j)
              s(6) = s(6) + h(6,j)
              p    = p    + hp(j)
            enddo
!            
            if (flag_incr == 0) then
!              viscous stress increment in computed (mulawc)               
              sigvxx(i) = sigvxx(i)+(s(1)- p)*off(i)
              sigvyy(i) = sigvyy(i)+(s(2)- p)*off(i)
              sigvxy(i) = sigvxy(i)+s(4)*off(i)
              sigvyz(i) = sigvyz(i)+s(5)*off(i)
              sigvzx(i) = sigvzx(i)+s(6)*off(i)          
            else
!             viscous stress increment in computed - incremental formulation(mulawglc)
              s_old(1) =  uvarv(i,offset+22)
              s_old(2) =  uvarv(i,offset+23)
!              s_old(3) =  uvarv(i,offset+24)
              s_old(4) =  uvarv(i,offset+25) 
              s_old(5) =  uvarv(i,offset+26)
              s_old(6) =  uvarv(i,offset+27)
              p_old    =  uvarv(i,offset+28)
!
              uvarv(i,offset+22) = s(1)
              uvarv(i,offset+23) = s(2)
!              uvarv(i,offset+24) = s(3)
              uvarv(i,offset+25) = s(4)
              uvarv(i,offset+26) = s(5)
              uvarv(i,offset+27) = s(6)
              uvarv(i,offset+28) = p
!              
              sigvxx(i) = sigvxx(i)+(s(1)-s_old(1)-p+p_old)*off(i)
              sigvyy(i) = sigvyy(i)+(s(2)-s_old(2)-p+p_old)*off(i)
              sigvxy(i) = sigvxy(i)+(s(4)-s_old(4))*off(i)
              sigvyz(i) = sigvyz(i)+(s(5)-s_old(5))*off(i)
              sigvzx(i) = sigvzx(i)+(s(6)-s_old(6))*off(i)                                
            endif    
!            
            soundsp(i) = sqrt(soundsp(i)**2 + (g + rbulk)*et(i)/rho0(i))
!            
          enddo                                                  
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_range_shell
      end module damping_range_shell_mod