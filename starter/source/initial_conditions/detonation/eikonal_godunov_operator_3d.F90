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
      !||    eikonal_godunov_operator_3d_mod   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_3d.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_compute_adjacent          ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
      !||====================================================================
      module eikonal_godunov_operator_3d_mod
      contains

! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Godunov operator for fast marching method in 2D using gradient reconstruction (unstructured mesh)
!! \details Finite differences cannot be used since adjacent points are not necessarily aligned.
      !||====================================================================
      !||    eikonal_godunov_operator_3d   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_3d.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_compute_adjacent      ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine eikonal_Godunov_Operator_3d(xel, tt, xel_adj, tt_adj, n_adj, Velocity, Velocity_adj)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero, one, ep21, two, four, em06
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
          integer, intent(in) :: n_adj ! number of adjacent points
          my_real, intent(in) :: Xel(3) ! centroids coordinates
          my_real, intent(in) :: Xel_adj(3, n_adj) ! centroids coordinates of adjacent points
          my_real, intent(inout) :: tt ! arrival time
          my_real, intent(in) :: tt_adj(n_adj) ! arrival time of adjacent points
          my_real, intent(in) :: Velocity ! velocity on current point
          my_real, intent(in) :: Velocity_adj(n_adj) ! velocity on adjacent points
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: k,l,m
          my_real :: dx, dy, dz, dist
          my_real :: tt_candidate
          my_real :: a, b, c
          my_real :: s
          my_real :: delta
          my_real :: A1,A2,B1,B2,C1,C2,AA,BB,CC,DENOM
          my_real :: max_abc
          my_real :: D2
          my_real :: u(3), norm_u, v(3), norm_v, xel2d(3,3), k_proj, l_proj_u, l_proj_v
          
          my_real :: x10,x20,x30, y10,y20,y30, z10,z20,z30
          my_real :: invA(3,3)
          
          my_real :: T1,T2,T3
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Initialize gradients
          a = min(tt_adj(1),tt_adj(3))
          b = min(tt_adj(2),tt_adj(4))
          c = min(tt_adj(5),tt_adj(6))
          max_abc = max(max(a,b),c)
          if (a == ep21 .and. b == ep21 .and. c == ep21) return
          if (a == ep21 .and. b == ep21)then
            m=5
            if(c == tt_adj(6))m=6  !supposing x axis fior adj elems 2,4
            dx = (xel(1)-xel_adj(1,m))
            dy = (xel(2)-xel_adj(2,m))
            dz = (xel(3)-xel_adj(3,m))
            dist = sqrt(dx*dx+dy*dy+dz*dz)
            s = max(Velocity, Velocity_adj(m))
            s = one / s
            tt_candidate = c+s*dist
            tt = min(tt, tt_candidate)
          elseif (a == ep21 .and. c ==ep21)then
            l=2
            if(b == tt_adj(4))l=4  !supposing x axis fior adj elems 2,4
            dx = (xel(1)-xel_adj(1,l))
            dy = (xel(2)-xel_adj(2,l))
            dz = (xel(3)-xel_adj(3,l))
            dist = sqrt(dx*dx+dy*dy+dz*dz)
            s = max(Velocity, Velocity_adj(l))
            s = one / s
            tt_candidate = b+s*dist
            tt = min(tt, tt_candidate)
          elseif (b == ep21 .and. c == ep21) then
            k=1
            if(a==tt_adj(3))k=3  !supposing x axis fior adj elems 1,3
            dx = (xel(1)-xel_adj(1,k))
            dy = (xel(2)-xel_adj(2,k))
            dz = (xel(3)-xel_adj(3,k))
            dist = sqrt(dx*dx+dy*dy+dz*dz)
            s = max(Velocity, Velocity_adj(k))
            s = one / s
            tt_candidate = a+s*dist
            tt = min(tt, tt_candidate)
          elseif (max_abc == ep21) then  !at least one 'direction' ignored (two directions)
            if(a == ep21)then
              k=5
              if(c == tt_adj(6))k=6
              l=2
              if(b == tt_adj(4))l=4
            elseif(b==ep21)then
              k=1
              if(a == tt_adj(3))k=3
              l=5
              if(c == tt_adj(6))l=6
            elseif(c==ep21)then
              k=1
              if(a == tt_adj(3))k=3
              l=2
              if(b == tt_adj(4))l=4
            end if

            s = max(Velocity, Velocity_adj(k))
            s = max(s, Velocity_adj(l))
            s = one / s

            !P : node to treat (xel2d(1:3,1))
            !K,L : ajdacent points. xel2d(1:3,2) and xel2d(1:3,3)
            
            ! u is PK vector
            u = xel_adj(:,k) - xel
            norm_u = sqrt(sum(u**2))  ! Norme de u
            u = u / norm_u  ! Normalisation de u

            !v is orthogonal to u (one plane generated by PK, PL)
            v = xel_adj(:,l) - xel
            v = v - dot_product(u, v) * u  ! Retirer la composante dans la direction de u
            norm_v = sqrt(sum(v**2))  ! Norme de v
            v = v / norm_v  ! Normalisation de v

            ! Projection de P sur le plan 2D -> P devient l'origine (0, 0)
            xel2d(1:3,1) = [zero, zero, zero]

            ! K-Projection on 2D plane
            k_proj = dot_product(u, xel_adj(:,k) - xel)  ! Projection of K on u
            xel2d(1:3,2) = [k_proj, zero, zero]  ! Coordinates of K on u

            ! L-Projection on 2D plane
            l_proj_u = dot_product(u, xel_adj(:,l) - xel)  ! Projection of L on u
            l_proj_v = dot_product(v, xel_adj(:,l) - xel)  ! Projection of L on v
            xel2d(1:3,3) = [l_proj_u, l_proj_v, zero]  ! Coordinates of L in (u,v) plane

            ! Calculate coefficients for quadratic equation
            A1 = (xel2d(2,2) - xel2d(2,3))
            A2 = (xel2d(1,3) - xel2d(1,2))
            B1 = two * ( (xel2d(2,3) - zero)*tt_adj(k) - (xel2d(2,2) - zero)*tt_adj(l) ) * A1
            B2 = two * ( (xel2d(1,2) - zero)*tt_adj(l) - (xel2d(1,3) - zero)*tt_adj(k) ) * A2
            C1 = ( (xel2d(2,3) - zero)*tt_adj(k) - (xel2d(2,2) - zero)*tt_adj(l) )
            C2 = ( (xel2d(1,2) - zero)*tt_adj(l) - (xel2d(1,3) - zero)*tt_adj(k) )

            DENOM = (xel2d(1,2) - zero)*(xel2d(2,3) - zero) - (xel2d(1,3) - zero)*(xel2d(2,2) - zero)
            DENOM = DENOM*DENOM

            A1 = A1*A1
            A2 = A2*A2
            C1 = C1*C1
            C2 = C2*C2

            AA = (A1+A2)/DENOM
            BB = (B1+B2)/DENOM
            CC = (C1+C2)/DENOM - s*s

            delta = BB*BB-FOUR*AA*CC

            if(delta >= zero)then
              tt_candidate = (-BB + sqrt(delta)) / two / AA
            else
              tt_candidate = (-BB + ZERO) / TWO / AA
            end if
            tt = min(tt, tt_candidate)




          elseif (max_abc < ep21) then  !three directions
            s = maxval(Velocity_adj)
            s = max(s, Velocity)
            s = one / s
            k=1
            if(a == tt_adj(3))k=3
            l=2
            if(b == tt_adj(4))l=4
            m=5
            if(c == tt_adj(6))m=6
            
            x10 = xel_adj(1,K)-xel(1)
            x20 = xel_adj(1,L)-xel(1)
            x30 = xel_adj(1,M)-xel(1)
            
            y10 = xel_adj(2,K)-xel(2)
            y20 = xel_adj(2,L)-xel(2)
            y30 = xel_adj(2,M)-xel(2)

            z10 = xel_adj(3,K)-xel(3)
            z20 = xel_adj(3,L)-xel(3)
            z30 = xel_adj(3,M)-xel(3)
            
            T1 = tt_adj(k)
            T2 = tt_adj(l)
            T3 = tt_adj(m)
            
            DENOM =  x10*y20*z30 - x10*z20*y30 - x20*y10*z30 + x20*z10*y30 + x30*y10*z20 - x30*z10*y20

            invA(1,1:3) = [ y20*z30-z20*y30, -y10*z30+z10*y30,  y10*z20-z10*y20]
            invA(2,1:3) = [-x20*z30+z20*x30,  x10*z30-x30*z10, -x10*z20+x20*z10]
            invA(3,1:3) = [ x20*y30-y20*x30, -x10*y30+x30*y10,  x10*y20-x20*y10]
            
            D2 = DENOM*DENOM
            
            AA = (-invA(2,1) - invA(2,2) - invA(2,3))**2 &
               + (-invA(1,1) - invA(1,2) - invA(1,3))**2 &
               + (-invA(3,1) - invA(3,2) - invA(3,3))**2
               
            BB = two*(invA(1,1)*T1 + invA(1,2)*T2 + invA(1,3)*T3) * (-invA(1,1)- invA(1,2) - invA(1,3)) &
               + two*(invA(2,1)*T1 + invA(2,2)*T2 + invA(2,3)*T3) * (-invA(2,1) -invA(2,2) - invA(2,3)) &
               + two*(invA(3,1)*T1 + invA(3,2)*T2 + invA(3,3)*T3) * (-invA(3,1)- invA(3,2) - invA(3,3))
           
            CC = (invA(1,1)*T1 + invA(1,2)*T2 + invA(1,3)*T3)**2 &
               + (invA(3,1)*T1 + invA(3,2)*T2 + invA(3,3)*T3)**2 &
               + (invA(2,1)*T1 + invA(2,2)*T2 + invA(2,3)*T3)**2 &
               - s*s*D2
            
            delta = BB*BB-FOUR*AA*CC

            if(delta >= zero)then
              tt_candidate = (-BB + sqrt(delta)) / two / AA
            else
              tt_candidate = (-BB + ZERO) / TWO / AA
            end if
            tt = min(tt, tt_candidate)

          end if

        end subroutine eikonal_godunov_operator_3d
! ----------------------------------------------------------------------------------------------------------------------

      end module eikonal_godunov_operator_3d_mod
