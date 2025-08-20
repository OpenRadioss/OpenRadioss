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
      module preload_solid_ini_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine get info of /PRELOAD using FCT_ID
!=======================================================================================================================
        subroutine preload_solid_ini(python    , nfunct    ,         &
          fun_id    ,    sens_id,          npc,            snpc,     &
          tf        ,        stf,      sensors,            time,     &
          bpreld    ,    nbpreld,          nel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,one
          use sensor_mod
          use python_funct_mod, only : python_
          use finter_mixed_mod, only : finter_mixed
          use precision_mod,    only : WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                         :: nfunct             !< number of functions
          type(python_) ,intent(inout)                    :: python             !< python module
          integer, intent (in   )                         :: fun_id             !< function id
          integer, intent (in   )                         :: sens_id            !< sensor id
          integer, intent (in   )                         :: snpc,stf           !< array dimension
          integer, intent (in   )                         :: nel,nbpreld        !< array dimension of bpreld
          integer, intent (in   ) ,dimension(snpc)        :: npc                !< index pointer of function
          type (sensors_) ,intent(in)                     :: sensors            !< sensor module
          real(kind=WP), intent (in  )  ,dimension(stf)         :: tf           !< (x,y) of function
          real(kind=WP), intent (in  )                          :: time         !< time
          real(kind=WP), intent (inout) ,dimension(nel,nbpreld) :: bpreld     !< y-value of preload function
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          real(kind=WP) :: t_start,t_stop,t_shift,tt,sfac
! ----------------------------------------------------------------------------------------------------------------------
!           e x t e r n a l   f u n c t i o n s
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          sfac = one
          t_shift = zero
          if (sens_id>0) then
            t_shift = sensors%sensor_tab(sens_id)%tstart
          end if
          if (fun_id>0) then
            t_start = tf(npc(fun_id)) 
            t_stop  = tf(npc(fun_id+1)-2) 
            tt = time-t_shift
            if (tt>=t_start.and.tt<t_stop) then
              sfac = finter_mixed(python,nfunct,fun_id,tt,npc,tf)
            end if
          end if
          bpreld(1:nel,10) = sfac
          if (fun_id>0) then
            do i=1,nel
              if (bpreld(i,3) == zero) then
                bpreld(i,1) = t_start
                bpreld(i,2) = t_stop
              end if
            end do
          end if !(funs_id>0) then
!--- t_shift has to be traited in boltst.F due to the huge value before sensor activation
        end subroutine preload_solid_ini
!-------------------------------------------------
      end module preload_solid_ini_mod
