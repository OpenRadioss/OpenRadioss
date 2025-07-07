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
      !||    law129_upd_mod   ../starter/source/materials/mat/mat129/law129_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat          ../starter/source/materials/updmat.F
      !||====================================================================
      module law129_upd_mod
      contains
      ! ======================================================================================================================
      ! \brief Updating material parameters of /MAT/law129
      ! \details converting sensor_id to internal sensor number
      ! ======================================================================================================================
      !||====================================================================
      !||    law129_upd                ../starter/source/materials/mat/mat129/law129_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat                   ../starter/source/materials/updmat.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      subroutine law129_upd(mat_param,sensors)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use matparam_def_mod
        use sensor_mod
        use message_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      type(matparam_struct_) ,intent(inout) :: mat_param !< Material parameters data structure
      type(sensors_)         ,intent(in)    :: sensors   !< sensor structure
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,sens_id,isens
!=======================================================================
      isens   = 0
      sens_id = mat_param%iparam(2)
      if (sens_id > 0) then
        do i=1,sensors%nsensor
          if (sens_id == sensors%sensor_tab(i)%sens_id) then
            isens = i
            exit
          end if
        end do
        if (isens == 0) then
          call ancmsg(MSGID=1240,ANMODE=ANINFO,MSGTYPE=MSGWARNING,    &   
                      I1=mat_param%mat_id,C1=mat_param%title,I2=sens_id) 
        end if
      end if

      mat_param%iparam(2) = isens
!------------------------------------------------------------------------------------
      end subroutine law129_upd
      end module law129_upd_mod
