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
      module mat51_associate_eos_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief
!! \details
        subroutine mat51_associate_eos(mat_param,nummat,parent_mid)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
              use mat_elem_mod , only : matparam_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: nummat
          integer,intent(in) :: parent_mid
          type (matparam_struct_) ,dimension(nummat) ,intent(inout), target :: mat_param
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nb,i,submat_mid
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          nb = mat_param(parent_mid)%multimat%nb
          ! MODERN FORMAT
          if(mat_param(parent_mid)%multimat%old_data_format == 0) then
            if (nb > 0) then
              allocate(mat_param(parent_mid)%multimat%pEOS(nb))
              do i=1,nb
                submat_mid = mat_param(parent_mid)%multimat%mid(i)
                if(submat_mid > 0) then
                  mat_param(parent_mid)%multimat%pEOS(i)%eos => mat_param(submat_mid)%eos
                end if
              end do
            end if
          ! OLD FORMAT (EMBEDDED EOS PARAMETERS)
          else
              allocate(mat_param(parent_mid)%multimat%pEOS(4))
              do i=1,4
                mat_param(parent_mid)%multimat%pEOS(i)%eos => mat_param(parent_mid)%multimat%EOS(i)
              end do
          end if
        end subroutine mat51_associate_eos
      end module mat51_associate_eos_mod
