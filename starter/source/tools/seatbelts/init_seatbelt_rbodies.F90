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
      module init_seatbelt_rbodies_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \routine to search for rbodies that are referenced by slipring and fill slipring data_structure with
        subroutine init_seatbelt_rbodies(nnpby,nrbody,npby,slrbody,lpby,sicode,icode,nslipring)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod, only: ancmsg
          use message_mod, only: aninfo_blind_1
          use message_mod, only: msgerror
          use seatbelt_mod, only: slipring
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
          integer,                                    intent(in) :: nnpby   !< size of first dimension of the npby array
          integer,                                    intent(in) :: nrbody  !< size of second dimension of the npby array (number of rbodies)
          integer,                                    intent(in) :: npby(nnpby,nrbody)      !< rbodies data array
          integer,                                    intent(in) :: slrbody       !< size of lpby array 
          integer,                                    intent(in) :: lpby(slrbody) !< array of rbody nodes
          integer,                                    intent(in) :: sicode !< size of icode array
          integer,                                    intent(in) :: icode(sicode) !< bcs code of nodes
          integer,                                    intent(in) :: nslipring !< slipring number
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,l,n 
          integer :: idrb ! rigid body index
          integer :: nfound_rby ! number of rigid bodies found
          integer :: nfound_bcs ! nimber of bcs found
          integer :: ic ! bcs code
          integer :: ic1 ! bcs code in dir1
          integer :: ic2 ! bcs code in dir1
          integer :: bcs_x 
          integer :: bcs_y 
          integer :: bcs_z 
          integer :: nod ! frame anchor node
          integer :: nsl ! rigid body secondary node number
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
        do i=1,nslipring
          if (slipring(i)%nfram > 1) then
            idrb = 0
            nfound_rby = 0
            nfound_bcs = 0
            do j=1,slipring(i)%nfram
              nod = slipring(i)%fram(j)%anchor_node
!---          check of rbodies ---
              l = 0
              do n=1,nrbody
                nsl=npby(2,n)
                do k=1,nsl              
                  if ((lpby(k+l)==nod).and.((idrb==0).or.(idrb == n))) then
                    nfound_rby  = nfound_rby  + 1
                    idrb = n
                  endif
                enddo
                l = l+nsl
              enddo
!---          check of bcs ---
              ic = icode(nod)
              ic1=ic/512
              ic2=(ic-512*ic1)/64
              bcs_x = ic1/4
              bcs_y = (ic1-4*bcs_x)/2
              bcs_z = ic1-4*bcs_x-2*bcs_y
              if (bcs_x*bcs_y*bcs_z > 0) nfound_bcs = nfound_bcs + 1
            enddo
            if (nfound_rby ==slipring(i)%nfram) then
              slipring(i)%rbody = idrb
            elseif (nfound_bcs /= slipring(i)%nfram) then
              call ancmsg(msgid=2081,              &
                          msgtype=msgerror,        &
                          anmode=aninfo_blind_1,   &
                          i1=slipring(i)%id) 
            endif
          endif
        enddo
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_seatbelt_rbodies
      end module init_seatbelt_rbodies_mod
