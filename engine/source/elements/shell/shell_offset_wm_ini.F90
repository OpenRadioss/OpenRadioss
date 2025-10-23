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
!||    shell_offset_wm_ini_mod   ../engine/source/elements/shell/shell_offset_wm_ini.F90
!||--- called by ------------------------------------------------------
!||    mulawc                    ../engine/source/materials/mat_share/mulawc.F90
!||====================================================================
      module shell_offset_wm_ini_mod
      contains
! ======================================================================================================================
! \brief initialization wm with offset
! ======================================================================================================================
!||====================================================================
!||    shell_offset_wm_ini   ../engine/source/elements/shell/shell_offset_wm_ini.F90
!||--- called by ------------------------------------------------------
!||    mulawc                ../engine/source/materials/mat_share/mulawc.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine shell_offset_wm_ini(ipt  ,npt   ,zoffset  ,wmi)     
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,one,half,third,one_over_6,fourth,one_over_8,zep1,zep2,zep3,zep4,zep0555555,    &
                                   zep0714286,zep1666667,zep2142857,zep2777778,zep3571429,zep3888889,zep375
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
          real(kind=WP) :: fac,lobatto,pos
! ======================================================================================================================
!
      real(kind=WP), parameter :: z01(11,11) = reshape((/                         &
       zero       ,zero       ,zero       ,zero       ,zero       ,               &  !1
       zero       ,zero       ,zero       ,zero       ,zero       ,zero       ,   &  !1
       -half      ,half       ,zero       ,zero       ,zero       ,               &  !2
       zero       ,zero       ,zero       ,zero       ,zero       ,zero       ,   &  !2
       -half      ,zero       ,half       ,zero       ,zero       ,               &  !3
       zero       ,zero       ,zero       ,zero       ,zero       ,zero       ,   &  !3
       -half      ,-one_over_6,one_over_6 ,half       ,zero       ,               &  !4
       zero       ,zero       ,zero       ,zero       ,zero       ,zero       ,   &  !4
       -half      ,-fourth    ,zero       ,fourth     ,half       ,               &  !5
       zero       ,zero       ,zero       ,zero       ,zero       ,zero       ,   &  !5
       -half      ,-zep3      ,-zep1      ,zep1      ,zep3        ,               &  !6
       half       ,zero       ,zero       ,zero      ,zero        ,zero       ,   &  !6
       -half      ,-third     ,-one_over_6,zero      ,one_over_6  ,               &  !7
       third      ,half       ,zero       ,zero      ,zero        ,zero       ,   &  !7
       -half      ,-zep3571429,-zep2142857,-zep0714286,zep0714286 ,               &  !8
       zep2142857 ,zep3571429 ,half       ,zero      ,zero        ,zero       ,   &  !8
       -half      ,-zep375    ,-fourth    ,-one_over_8,zero       ,               &  !9
       one_over_8 ,fourth     ,zep375     ,half      ,zero       ,zero        ,   &  !9
       -half      ,-zep3888889,-zep2777778,-zep1666667,-zep0555555,               &  !A
       zep0555555 ,zep1666667 ,zep2777778 ,zep3888889 ,half      ,zero        ,   &  !A
       -half      ,-zep4      ,-zep3      ,-zep2     ,-zep1      ,                &  !B
       zero       ,zep1       ,zep2       ,zep3      ,zep4       ,half        /), &  !B
       (/11,11/))    
!
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
