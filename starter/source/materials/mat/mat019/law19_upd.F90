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
!chd|====================================================================
!chd|  law19_upd                     source/materials/mat/mat019/law19_upd.F90
!chd|-- called by -----------
!chd|        updmat                        source/materials/updmat.F
!chd|-- calls ---------------
!chd|====================================================================

      !||====================================================================
      !||    law19_upd_mod   ../starter/source/materials/mat/mat019/law19_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat          ../starter/source/materials/updmat.F
      !||====================================================================
      module law19_upd_mod
      contains

! ======================================================================================================================
! \brief replace user sensor_Id by internal number in material law parameter table
!! \details

! ======================================================================================================================

      !||====================================================================
      !||    law19_upd          ../starter/source/materials/mat/mat019/law19_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat             ../starter/source/materials/updmat.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg             ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod        ../starter/share/message_module/message_mod.F
      !||    sensor_mod         ../starter/share/modules1/sensor_mod.F
      !||====================================================================
      subroutine law19_upd(mat_param,sensors)

!-----------------------------------------------
!     M o d u l e s
!-----------------------------------------------
      use matparam_def_mod
      use sensor_mod
      use message_mod   
! ----------------------------------------------------------------------------------------------------------------------

          implicit none

! ----------------------------------------------------------------------------------------------------------------------
!     included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!     D u m m y   A r g u m e n t s
!-----------------------------------------------
      type (matparam_struct_) ,intent(inout) :: mat_param  !< material law parameter module
      type (sensors_)         ,intent(in)    :: sensors    !< sensor structure
!-----------------------------------------------
!     L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,isens,sens_id
!=======================================================================
      sens_id = mat_param%iparam(1)
      isens   = 0
      if (sens_id > 0) then
        do i=1,sensors%nsensor
          if (sens_id == sensors%sensor_tab(i)%sens_id) then
            isens = i
            exit
          end if
        enddo
        if (isens == 0)                                               &
          call ancmsg(msgid=1240,anmode=aninfo,msgtype=msgwarning,    &
                i1=mat_param%mat_id,c1=mat_param%title,i2=isens) 
      end if
      mat_param%iparam(1) = isens
!-----------
      return
      end subroutine law19_upd
      end module law19_upd_mod
