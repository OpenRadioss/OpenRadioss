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

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief check boundary condition applied on nodes related to interface type 1
!! \details
      !||====================================================================
      !||    i9bcs_check   ../starter/source/interfaces/int09/i9bcs_check.F90
      !||--- called by ------------------------------------------------------
      !||    inint2        ../starter/source/interfaces/inter2d1/inint2.F
      !||    inint3        ../starter/source/interfaces/inter3d1/inint3.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
        subroutine i9bcs_check(icode, sicode, nsn , nsv, siloc, iloc)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                  intent(in) :: siloc, sicode    !< the size of array ICODE
          integer,                  intent(in) :: icode(sicode)           !< bcs codes for nodes
          integer,                  intent(in) :: iloc(siloc)             !< working array for interface type9
          integer,                  intent(in) :: nsn                     !< number of secnd nodes
          integer,                  intent(in) :: nsv(nsn)                !< list of secnd nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii      !< loop
          integer :: num_bcs !< number of boundary conditions
          integer :: jbc(3)  !< working array
          integer :: icodt   !< current code (translation)
          integer :: lcod    !< code in [1,7] which is a 3-bit-integer
          integer :: inod    !< current node
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

        do ii=1,nsn

          if(iloc(ii) >= 1)then

            inod=nsv(ii)
            lcod=icode(inod)/512
            num_bcs = 2

            if(lcod /= 0)then
              ! --- this node has a bc(s) defined
              jbc(1:3) = 0  ! 3-bit-integer representing XYZ tags :  lcod = sum( jbc(i)*2**(i-1) , i=1..3 )
              jbc(3) = IAND(lcod, 1)    !1st bit
              jbc(2) = IAND(lcod, 2)    !2nd bit
              jbc(1) = IAND(lcod, 4)    !3rd bit

              num_bcs=2
              if(jbc(1) /= 0) num_bcs = num_bcs+1
              if(jbc(2) /= 0) num_bcs = num_bcs+1
              if(jbc(3) /= 0)then
               if(num_bcs == 4)then
                 ! bcs check also in engine, since engine options /bcs, /bcsr may update nodal bcs
                 !call ancmsg(msgid=3065, anmode = aninfo, msgtype = msgerror, i1=interf_uid, i2=nty, i3=itab(inod), c1=title)
                 exit
               else
                 num_bcs=num_bcs+1
               endif
              endif

            elseif(lcod == 0)then
              !no bc defined on this node

            endif !lcod

          endif !(iloc(ii) >= 1)

        enddo  !next ii

        end subroutine i9bcs_check
