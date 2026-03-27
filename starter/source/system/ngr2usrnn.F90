!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    ngr2usrnn_mod          ../starter/source/system/ngr2usrnn.F90
!||--- called by ------------------------------------------------------
!||    hm_read_inter_type25   ../starter/source/interfaces/int25/hm_read_inter_type25.F
!||====================================================================
      module ngr2usrnn_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!||====================================================================
!||    ngr2usrnn              ../starter/source/system/ngr2usrnn.F90
!||--- called by ------------------------------------------------------
!||    hm_read_inter_type25   ../starter/source/interfaces/int25/hm_read_inter_type25.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
        integer function ngr2usrnn(iu,igrnod,ngrnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
          use groupdef_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: iu 
          integer,intent(in) :: ngrnod 
          integer,intent(in) :: igrnod(ngrnod) 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
         ngr2usrnn = 0
         if (iu /= 0) then
            i = 1
            do while (i <= ngrnod .and. ngr2usrnn == 0)
              if (iu == igrnod(i)) then
                  ngr2usrnn = i
              endif
              i = i + 1
            enddo
          endif

         return

       end function ngr2usrnn

      end module ngr2usrnn_mod
