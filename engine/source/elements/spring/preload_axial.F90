!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      module preload_axial_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine get info of /PRELOD/AXIAL 
!=======================================================================================================================
        subroutine get_preload_axial(                                         &
                   fun_id    ,    sens_id,          npc,            snpc,     &
                   tf        ,        stf,      sensors,            time,     &
                   preload1  ,      stf_f)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use constant_mod, only : zero,two_third
        use sensor_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent (in   )                         :: fun_id       !< function id
        integer, intent (in   )                         :: sens_id      !< sensor id
        integer, intent (in   )                         :: snpc,stf     !< array dimension
        integer, intent (in   ) ,dimension(snpc)        :: npc          !< index pointer of function
        type (sensors_) ,intent(in)                     :: sensors      !< sensor mudule
        my_real, intent (in  )  ,dimension(stf)         :: tf           !< (x,y) of function
        my_real, intent (in  )                          :: time         !< time
        my_real, intent (inout)                         :: preload1     !< y-value of preload function
        my_real, intent (inout)                         :: stf_f        !< stiffness restoring factor
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i,j,nld,isens
        my_real t_start,t_stop,t_shift,deri,tt,t_stif
! ----------------------------------------------------------------------------------------------------------------------
!           e x t e r n a l   f u n c t i o n s
! ----------------------------------------------------------------------------------------------------------------------                   
        my_real finter
        external finter
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
          preload1= finter(fun_id,tt,npc,tf,deri)
          t_stif = t_start + (t_stop-t_start)*two_third
          stf_f = zero
        end if
!---
        end subroutine get_preload_axial
!=======================================================================================================================
!!\brief This subroutine compute axial force of 1D-element using /PRELOD/AXIAL 
!=======================================================================================================================
        subroutine preload_axial(nel,preload1,bpreload,v12,stf_f,f1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use constant_mod, only : zero
        use sensor_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent (in   )                       :: nel          !< element number
        my_real, intent (in   ) , dimension(nel,2)    :: bpreload     !< preload parameters
        my_real, intent (in   )                       :: preload1     !< preload function value
        my_real, intent (in   ) , dimension(nel)      :: v12          !< axial velocity
        my_real, intent (inout) , dimension(nel)      :: f1           !< axial preload force 
        my_real, intent (in   )                       :: stf_f        !< stiffness restoring factor
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i
        my_real y_scal,damp
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
