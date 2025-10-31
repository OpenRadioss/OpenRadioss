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
!||    preload_axial_mod   ../engine/source/elements/spring/preload_axial.F90
!||--- called by ------------------------------------------------------
!||    forint              ../engine/source/elements/forint.F
!||    pforc3              ../engine/source/elements/beam/pforc3.F
!||    r23law113           ../engine/source/elements/spring/r23law113.F
!||    rforc3              ../engine/source/elements/spring/rforc3.F
!||    tforc3              ../engine/source/elements/truss/tforc3.F
!||====================================================================
      module preload_axial_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine get info of /PRELOD/AXIAL
!=======================================================================================================================
!||====================================================================
!||    get_preload_axial   ../engine/source/elements/spring/preload_axial.F90
!||--- called by ------------------------------------------------------
!||    forint              ../engine/source/elements/forint.F
!||--- uses       -----------------------------------------------------
!||    constant_mod        ../common_source/modules/constant_mod.F
!||    finter_mixed_mod    ../engine/source/tools/finter_mixed.F90
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||    python_funct_mod    ../common_source/modules/python_mod.F90
!||    sensor_mod          ../common_source/modules/sensor_mod.F90
!||====================================================================
        subroutine get_preload_axial(python, nfunct,                 &
          fun_id    ,    sens_id,          npc,            snpc,     &
          tf        ,        stf,      sensors,            time,     &
          preload1  ,      stf_f)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,two_third
          use sensor_mod
          use python_funct_mod, only : python_
          use finter_mixed_mod, only : finter_mixed
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                         :: nfunct       !< number of functions
          type(python_) ,intent(inout)                     :: python       !< python module
          integer, intent (in   )                         :: fun_id       !< function id
          integer, intent (in   )                         :: sens_id      !< sensor id
          integer, intent (in   )                         :: snpc,stf     !< array dimension
          integer, intent (in   ) ,dimension(snpc)        :: npc          !< index pointer of function
          type (sensors_) ,intent(in)                     :: sensors      !< sensor module
          real(kind=WP), intent (in  )  ,dimension(stf)         :: tf           !< (x,y) of function
          real(kind=WP), intent (in  )                          :: time         !< time
          real(kind=WP), intent (inout)                         :: preload1     !< y-value of preload function
          real(kind=WP), intent (inout)                         :: stf_f        !< stiffness restoring factor
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP) :: t_start,t_stop,t_shift,tt,t_stif
! ----------------------------------------------------------------------------------------------------------------------
!           e x t e r n a l   f u n c t i o n s
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          t_start = tf(npc(fun_id))
          t_stop  = tf(npc(fun_id+1)-2)
          t_shift = zero
          preload1 = zero
          stf_f = zero
          if (sens_id>0) then
            t_shift = sensors%sensor_tab(sens_id)%tstart
          end if
          tt = time-t_shift
          if (tt>=t_start.and.tt<t_stop) then
            preload1= finter_mixed(python,nfunct,fun_id,tt,npc,tf)
            t_stif = t_start + (t_stop-t_start)*two_third
            stf_f = zero
          end if
!---
        end subroutine get_preload_axial
!=======================================================================================================================
!!\brief This subroutine compute axial force of 1D-element using /PRELOD/AXIAL
!=======================================================================================================================
!||====================================================================
!||    preload_axial   ../engine/source/elements/spring/preload_axial.F90
!||--- called by ------------------------------------------------------
!||    pforc3          ../engine/source/elements/beam/pforc3.F
!||    r23law113       ../engine/source/elements/spring/r23law113.F
!||    rforc3          ../engine/source/elements/spring/rforc3.F
!||    tforc3          ../engine/source/elements/truss/tforc3.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||    sensor_mod      ../common_source/modules/sensor_mod.F90
!||====================================================================
        subroutine preload_axial(nel,preload1,bpreload,v12,stf_f,f1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero
          use sensor_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                       :: nel          !< element number
          real(kind=WP), intent (in   ) , dimension(nel,2)    :: bpreload     !< preload parameters
          real(kind=WP), intent (in   )                       :: preload1     !< preload function value
          real(kind=WP), intent (in   ) , dimension(nel)      :: v12          !< axial velocity
          real(kind=WP), intent (inout) , dimension(nel)      :: f1           !< axial preload force
          real(kind=WP), intent (in   )                       :: stf_f        !< stiffness restoring factor
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          real(kind=WP) :: y_scal,damp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          do i=1,nel
            y_scal = bpreload(i,1)
            damp   = bpreload(i,2)
            f1(i)  = stf_f*f1(i) + y_scal*preload1 + damp*v12(i)
          end do
!---

        end subroutine preload_axial
!-------------------------------------------------
      end module preload_axial_mod
