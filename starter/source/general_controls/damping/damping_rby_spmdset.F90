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
!||    damping_rby_spmdset_mod   ../starter/source/general_controls/damping/damping_rby_spmdset.F90
!||--- called by ------------------------------------------------------
!||    lectur                    ../starter/source/starter/lectur.F
!||====================================================================
      module damping_rby_spmdset_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine stick main node of ridid body on procs where damping nodes are present
!=======================================================================================================================
!
!||====================================================================
!||    damping_rby_spmdset   ../starter/source/general_controls/damping/damping_rby_spmdset.F90
!||--- called by ------------------------------------------------------
!||    lectur                ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                ../starter/source/output/message/message.F
!||    spmdset               ../starter/source/constraints/general/rbody/spmdset.F
!||--- uses       -----------------------------------------------------
!||    message_mod           ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine damping_rby_spmdset(igrnod,ngrnod,ndamp,nrdamp,dampr,nnpby,nrbody,npby,nrbmerge)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use GROUPDEF_MOD , only: GROUP_
          USE MESSAGE_MOD
          USE PRECISION_MOD, ONLY : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(GROUP_),                              intent(in) :: igrnod(ngrnod)              !< group od nodes structure
          integer,                                   intent(in) :: ngrnod                      !< number of groups of nodes
          integer,                                   intent(in) :: ndamp                       !< number of /DAMP
          integer,                                   intent(in) :: nrdamp                      !< first dimension of array DAMP
          integer,                                   intent(in) :: nnpby                       !< first dimension of array NPBY
          integer,                                   intent(in) :: nrbody                      !< number of rigid bodies
          integer,                                   intent(in) :: npby(nnpby,nrbody)          !< main structure for rigid bodies
          integer,                                   intent(in) :: nrbmerge                    !< nb or rigid body merge
          real(kind=WP),                                intent(inout) :: dampr(nrdamp,ndamp)         !< main structure for option /DAMP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nd,igr,id_rby,id_rby_user,j,id_damp
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
!         Rbody id replaced by id or main rigid body in case of rigid body merge
!
          do nd=1,ndamp
            id_damp = nint(dampr(1,nd))
            id_rby_user = nint(dampr(25,nd))
            if (id_rby_user > 0) then
              id_rby = 0
              do j=1,nrbody
                if (id_rby_user == npby(6,j)) then
                  if (nrbmerge == 0) then
                    id_rby = j
                  else
                    if (npby(12,j)==0) then
                      !         main rbody
                      id_rby = j
                    else
                      !         secondary rbody - switch to main
                      id_rby = npby(13,j)
                    endif
                  endif
                endif
              enddo
              if (id_rby == 0) then!  rbody not found
                call ancmsg(msgid=3048,                   &
                  msgtype=msgerror,             &
                  anmode=aninfo,                &
                  i1=id_damp,                   &
                  i2=id_rby_user)
              endif
              dampr(25,nd) = id_rby
            endif
          enddo
!
!         Stick main node of rigid body on proc in damped nodes are present
!
          do nd=1,ndamp
            id_rby = nint(dampr(25,nd))
            if (id_rby > 0) then
              igr   = nint(dampr(2,nd))
              call spmdset(id_rby,npby,nnpby,igrnod(igr)%entity,igrnod(igr)%nentity,0)
            endif
          enddo
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_rby_spmdset
      end module damping_rby_spmdset_mod
