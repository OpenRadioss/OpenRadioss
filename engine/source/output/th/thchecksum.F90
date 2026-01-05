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
!||    thsechecksum_mod   ../engine/source/output/th/thchecksum.F90
!||--- called by ------------------------------------------------------
!||    hist2              ../engine/source/output/th/hist2.F
!||====================================================================
      module thsechecksum_mod

      implicit none

      contains

!! \brief /CHECKSUM/START Option : write deck checksum digests to TH++
!||====================================================================
!||    thsechecksum    ../engine/source/output/th/thchecksum.F90
!||--- called by ------------------------------------------------------
!||    hist2           ../engine/source/output/th/hist2.F
!||--- calls      -----------------------------------------------------
!||    wrtdes          ../engine/source/output/th/wrtdes.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine thsechecksum(                                  &
        &                         j1 ,j2    ,l1      ,l2  ,ithbuf,  &
        &                         wa ,iform ,sithbuf ,swa ,ispmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod, only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer ,intent(in) :: ispmd
          integer ,intent(in) :: swa
          integer ,intent(in) :: sithbuf
          integer ,intent(in) :: iform
          integer ,intent(in) :: j1
          integer ,intent(in) :: j2
          integer ,intent(in) :: l1
          integer ,intent(in) :: l2
          integer ,dimension(sithbuf) ,intent(in)  :: ithbuf
          real(kind=WP) ,dimension(swa)     ,intent(out) :: wa
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: j
          integer :: k
          integer :: l
          integer :: ii
          integer,dimension(2)::iwa
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!     smpd : gather already done, write only by proc 0
! ----------------------------------------------------------------------------------------------------------------------
          if (ispmd == 0) then
!
            iwa=0
            ii = 0
            do j = j1,j2
              i = ithbuf(j)
              do l=l1,l2
                k = ithbuf(l)
                ii= ii+1
                wa(ii) = zero
              end do
            end do
            if (ii > 0) call wrtdes(wa,iwa,ii,iform,1)
          end if
!-----------
          return
        end subroutine thsechecksum

      end module thsechecksum_mod
