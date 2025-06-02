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
      !||    damping_range_shell_mom_mod   ../engine/source/general_controls/damping/damping_range_shell_mom.F90
      !||--- called by ------------------------------------------------------
      !||    mulawglc                      ../engine/source/materials/mat_share/mulawglc.F
      !||====================================================================
      module damping_range_shell_mom_mod
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
      !||    damping_range_shell_mom   ../engine/source/general_controls/damping/damping_range_shell_mom.F90
      !||--- called by ------------------------------------------------------
      !||    mulawglc                  ../engine/source/materials/mat_share/mulawglc.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod              ../common_source/modules/constant_mod.F
      !||    elbufdef_mod              ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    precision_mod             ../common_source/modules/precision_mod.F90
      !||====================================================================
        subroutine damping_range_shell_mom(damp_buf,nel      ,nuvarv  ,timestep ,dtinv   ,       &
                                           young   ,shear_mod,depbxx  ,depbyy   ,depbxy   ,       &
                                           momnxx  ,momnyy   ,momnxy  ,thk0     ,uvarv    ,       &
                                           off     ,etse)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: one,zero, third, four_over_3, half, two,em20,three,two_third,one_over_12
          use elbufdef_mod , only: buf_damp_range_
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in)    :: nel     !< number of elements
          integer,                                   intent(in)    :: nuvarv  !< number of variables in buf_visc
          real(kind=WP),                             intent(in)    :: dtinv       
          real(kind=WP), dimension(nel,nuvarv) ,           intent(inout) :: uvarv                    !< buffer for viscous variables
          real(kind=WP),                                   intent(in)    :: timestep                 !< time step
          real(kind=WP),                                   intent(in)    :: young                    !< young modulus
          real(kind=WP),                                   intent(in)    :: shear_mod                !< shear modulus
          real(kind=WP), dimension(nel) ,                  intent(in)    :: depbxx                   !< strain xx
          real(kind=WP), dimension(nel) ,                  intent(in)    :: depbyy                   !< strain yy
          real(kind=WP), dimension(nel) ,                  intent(in)    :: depbxy                   !< strain xy          
          real(kind=WP), dimension(nel) ,                  intent(inout) :: momnxx                   !< damp stress xx
          real(kind=WP), dimension(nel) ,                  intent(inout) :: momnyy                   !< damp stress yy
          real(kind=WP), dimension(nel) ,                  intent(inout) :: momnxy                   !< damp stress xy
          real(kind=WP), dimension(nel) ,                  intent(in)    :: thk0                     !< density
          real(kind=WP), dimension(nel) ,                  intent(in)    :: off                      !< off flag
          real(kind=WP), dimension(nel) ,                  intent(in)    :: etse                     !< tangent young modulus coefficient
          type(buf_damp_range_)   ,                  intent(in)    :: damp_buf                 !< damping frequency range buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, ii, offset, offset2
          real(kind=WP) :: h0(3),h(3,3),hp0,dav,fac,epbxx,epbyy,epbxy,p,p_old,thk08,epspbxx,epspbyy
          real(kind=WP), dimension(nel) :: a2,a3,fac_nu,et
          real(kind=WP), dimension(3)   :: aa, bb, gv, kv, beta, aak, bbk, betak, hp
          real(kind=WP), dimension(3)   :: s,s_old
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          offset = nuvarv - 16
          offset2 = nuvarv - 4  
!       
          do j=1,3              
!           tangent young modulus is used - bb multiplied by tangent coefficient for each element    
            et(1:nel) = one + etse(1:nel)                    
            gv(j)    = damp_buf%alpha(j)*shear_mod         
            beta(j)  = one/damp_buf%tau(j)
            kv(j)    = damp_buf%alpha(j)*young             
            betak(j) = one/damp_buf%tau(j)
            aa(j) =  exp(-beta(j)*timestep) 
            bb(j) =  two*timestep*gv(j)*exp(-half*beta(j)*timestep)      
            aak(j) = exp(-betak(j)*timestep)
            bbk(j) = timestep*kv(j)*exp(-half*betak(j)*timestep)
          enddo      
!          
          do i=1,nel
!           epspzz(i) = -a1(i) + (third*a2(i)-a3(i))*(epspxx(i) + epspyy(i))
            a2(i) = zero
            a3(i) = zero     
            do j=1,3
!             paramaters for computaion of epszz_dot (sigzz=0)
              a2(i)  = a2(i) +  bb(j)*et(i)
              a3(i)  = a3(i) +  bbk(j)*et(i)
            enddo  
            fac = one/max(em20,two_third*a2(i) + a3(i))
            fac_nu(i) = fac*(third*a2(i)-a3(i))
          enddo
!                
          do i=1,nel 
            epspbxx = depbxx(i)*dtinv
            epspbyy = depbyy(i)*dtinv      
            dav =  ((one+fac_nu(i))/three)*(epspbxx + epspbyy)
            epbxx = ((two-fac_nu(i))/three)*epspbxx - ((one+fac_nu(i))/three)*epspbyy
            epbyy = ((two-fac_nu(i))/three)*epspbyy - ((one+fac_nu(i))/three)*epspbxx
            epbxy = depbxy(i)*dtinv   
!  
            do j= 1,3    
              ii = offset + 4*(j-1)     
              h0(1) = uvarv(i,ii + 1)
              h0(2) = uvarv(i,ii + 2)
              h0(3) = uvarv(i,ii + 3)
              hp0   = uvarv(i,ii + 4)
!                    
              h(1,j) =    aa(j)*h0(1) + bb(j)*epbxx*et(i)
              h(2,j) =    aa(j)*h0(2) + bb(j)*epbyy*et(i)
              h(3,j) =    aa(j)*h0(3) + half*bb(j)*epbxy*et(i)
              hp(j)  =    aak(j)*hp0  + bbk(j)*(-three*dav)*et(i)
!
              uvarv(i,ii + 1) = h(1,j)
              uvarv(i,ii + 2) = h(2,j)
              uvarv(i,ii + 3) = h(3,j)           
              uvarv(i,ii + 4) = hp(j)         
            enddo 
!     
!        compute moment
!
            s(1:3) = zero
            p = zero
          
            do  j= 1,3
              s(1) = s(1) + h(1,j)
              s(2) = s(2) + h(2,j)
              s(3) = s(3) + h(3,j)
              p    = p    + hp(j)
            enddo
!            
!           viscous stress increment in computed - incremental formulation(mulawglc)
            s_old(1) =  uvarv(i,offset2+1)
            s_old(2) =  uvarv(i,offset2+2)
            s_old(3) =  uvarv(i,offset2+3)
            p_old    =  uvarv(i,offset2+4)
!
            uvarv(i,offset2+1) = s(1)
            uvarv(i,offset2+2) = s(2)
            uvarv(i,offset2+3) = s(3)
            uvarv(i,offset2+4) = p
!
            thk08 = thk0(i)*one_over_12         
            momnxx(i) = momnxx(i)+thk08*(s(1)-s_old(1)-p+p_old)*off(i)
            momnyy(i) = momnyy(i)+thk08*(s(2)-s_old(2)-p+p_old)*off(i)
            momnxy(i) = momnxy(i)+thk08*(s(3)-s_old(3))*off(i)
!            
          enddo                                                  
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_range_shell_mom
      end module damping_range_shell_mom_mod
