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
!||    finter_mixed_mod       ../engine/source/tools/finter_mixed.F90
!||--- called by ------------------------------------------------------
!||    airbaga                ../engine/source/airbag/airbag1.F
!||    airbaga1               ../engine/source/airbag/airbaga1.F
!||    cfield_imp             ../engine/source/loads/general/load_centri/cfield_imp.F
!||    fv_up_switch           ../engine/source/airbag/fv_up_switch.F
!||    fvbag1                 ../engine/source/airbag/fvbag1.F
!||    fvinjt6                ../engine/source/airbag/fvinjt6.F
!||    fvinjt8                ../engine/source/airbag/fvinjt8.F
!||    fxbodfp2               ../engine/source/constraints/fxbody/fxbodfp.F
!||    fxgrvcor               ../engine/source/constraints/fxbody/fxgrvcor.F
!||    get_preload_axial      ../engine/source/elements/spring/preload_axial.F90
!||    gravit_imp             ../engine/source/loads/general/grav/gravit_imp.F
!||    sms_gravit             ../engine/source/ams/sms_gravit.F
!||    volp_lfluid            ../engine/source/airbag/volp_lfluid.F
!||    volpfv                 ../engine/source/airbag/volpfv.F
!||    volpre                 ../engine/source/airbag/volpres.F
!||    volprep                ../engine/source/airbag/volpresp.F
!||--- calls      -----------------------------------------------------
!||    finter                 ../engine/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||    precision_mod          ../common_source/modules/precision_mod.F90
!||    python_funct_mod       ../common_source/modules/python_mod.F90
!||====================================================================
      module finter_mixed_mod
      use precision_mod, only : WP
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief  interpolate a table of values, or evaluate a python function
        real(kind=WP) function finter_mixed(python,nfunct,ifunc,x,npc,tf,dydx) result(y)
!                                  FINTER(IPT,TS*SCALT,NPC,TF,DYDX)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use python_funct_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_) :: python
          integer :: nfunct
          real(kind=WP) :: x
          real(kind=WP) :: tf(2,*)
          real(kind=WP), optional :: dydx
          integer, intent(in) :: ifunc
          integer :: npc(*)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ismooth
          real(kind=WP), external :: FINTER
          real(kind=WP) :: unused_dxdy

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            ismooth = 0
            if (ifunc > 0) ismooth = npc(2*nfunct+ifunc+1)
            if(ismooth < 0) then 
              call python_call_funct1d(python, -ismooth,x, y) 
              if(present(dydx)) then
              call python_deriv_funct1D(python, -ismooth,x, dydx) 
              endif
            else
                if(present(dydx)) then
                  y = FINTER(ifunc, x, npc, tf, dydx)
                else
                  y = FINTER(ifunc, x, npc, tf, unused_dxdy)
                endif
            endif
          return
        end function finter_mixed
      end module finter_mixed_mod
