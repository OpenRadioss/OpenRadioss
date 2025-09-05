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
      module shell_offset_wm_ini_mod
      contains
! ======================================================================================================================
! \brief initialization wm with offset
! ======================================================================================================================
        subroutine shell_offset_wm_ini(ipt  ,npt   ,zoffset  ,wmi)     
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,one,half,third
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                    :: ipt             !< integration number
          integer, intent(in)                                    :: npt             !< number of integration points
          real(kind=WP),                  intent(in)             :: zoffset         !< offset position
          real(kind=WP),                  intent(  out)          :: wmi             !< weights for moment of ipt
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          real(kind=WP) :: z01(11,11),fac,lobatto,pos
! ======================================================================================================================
!
      DATA  z01/                                                      &
       0.       ,0.       ,0.       ,0.       ,0.       ,             &  !1
       0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,   &  !1
       -.5      ,0.5      ,0.       ,0.       ,0.       ,             &  !2
       0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,   &  !2
       -.5      ,0.       ,0.5      ,0.       ,0.       ,             &  !3
       0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,   &  !3
       -.5      ,-.1666667,0.1666667,0.5      ,0.       ,             &  !4
       0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,   &  !4
       -.5      ,-.25     ,0.       ,0.25     ,0.5      ,             &  !5
       0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,   &  !5
       -.5      ,-.3      ,-.1      ,0.1      ,0.3      ,             &  !6
       0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,   &  !6
       -.5      ,-.3333333,-.1666667,0.0      ,0.1666667,             &  !7
       0.3333333,0.5      ,0.       ,0.       ,0.       ,0.       ,   &  !7
       -.5      ,-.3571429,-.2142857,-.0714286,0.0714286,             &  !8
       0.2142857,0.3571429,0.5      ,0.       ,0.       ,0.       ,   &  !8
       -.5      ,-.375    ,-.25     ,-.125    ,0.0      ,             &  !9
       0.125    ,0.25     ,0.375    ,0.5      ,0.       ,0.       ,   &  !9
       -.5      ,-.3888889,-.2777778,-.1666667,-.0555555,             &  !A
       0.0555555,0.1666667,0.2777778,0.3888889,0.5      ,0.       ,   &  !A
       -.5      ,-.4      ,-.3      ,-.2      ,-.1      ,             &  !B
       0.       ,0.1      ,0.2      ,0.3      ,0.4      ,0.5      /      !B
!                 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!    WM1i = Z01i / (n-1)    if |Z01i| /= 0.5
!    WM1i =  1/2(n-1) [1/2 - 1/3(n-1)]    if  Z01i =  0.5
        wmi = zero
        if (npt>1) then
          fac =one/(npt-1)
          if (abs(z01(ipt,npt))==half) then
            lobatto = (half-third*fac)*fac
          else
            lobatto = fac
          end if
          pos = z01(ipt,npt) + zoffset
          wmi = pos*lobatto
        end if
!        
        end subroutine shell_offset_wm_ini
! ----------------------------------------------------------------------------------------------------------------------
      end module shell_offset_wm_ini_mod
