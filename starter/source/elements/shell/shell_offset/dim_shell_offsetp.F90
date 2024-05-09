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
      module dim_shell_offsetp_mod

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine do the dimensioning of shell offset projection
!=======================================================================================================================
        subroutine dim_shell_offsetp(                                          &
          ngroup,    nparg,      iparg,        npropg,            &
          numgeo,      geo,     numelc,          nixc,            &
          ixc,  numeltg,      nixtg,          ixtg,            &
          numnod,    intag,    nsh_oset   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,half
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                         :: ngroup           !< number of elem group
          integer, intent (in   )                         :: nparg            !< 1er dim of iparg
          integer, intent (in   )                         :: npropg           !< 1er dim of geo
          integer, intent (in   )                         :: numgeo           !< number of prop
          integer, intent (in   )                         :: numelc           !< number shell 4n element
          integer, intent (in   )                         :: nixc             !< 1er dim of ixc
          integer, intent (in   )                         :: numeltg          !< number shell 3n element
          integer, intent (in   )                         :: nixtg            !< 1er dim of ixtg
          integer, intent (in   )                         :: numnod           !< number node
          integer, intent (in   ) ,dimension(nparg,ngroup):: iparg            !< elem group array
          integer, intent (in   ) ,dimension(nixc,numelc) :: ixc              !< shell 4n connectivity
          integer, intent (in   ),dimension(nixtg,numeltg):: ixtg             !< shell 3n connectivity
          integer, intent (inout) ,dimension(numnod)      :: intag            !< itag working array
          integer, intent (inout)                         :: nsh_oset         !< number offset shell for projection
          my_real, intent (in   ),dimension(npropg,numgeo):: geo              !< property array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer i,j,n,nel,nft,nn,ie,igtyp,nf1,ity,nnode,pid,nshel,ng
          my_real shelloff
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! 1er pass to fill intag
          intag = 0
          do  ng=1,ngroup
            ity=iparg(5,ng)
            igtyp  = iparg(38,ng)
            if (igtyp == 0.or.(ity /= 3 .and. ity /= 7) ) cycle
            nel=iparg(2,ng)
            nft=iparg(3,ng)
            pid =iparg(62,ng)
            shelloff = zero
            select case(igtyp)
             case (11)
              shelloff = geo(199,pid)
             case (17,51,52)
              shelloff = half+geo(199,pid)
            end select
            if (ity == 3)then
              nnode =4
              do i=1,nel
                ie = nft + i
                do j=1,nnode
                  n = ixc(j+1,ie)
                  if (shelloff/=zero) intag(n)=1
                enddo
              end do
            elseif (ity == 7)then
              nnode =3
              do i=1,nel
                ie = nft + i
                do j=1,nnode
                  n = ixtg(j+1,ie)
                  if (shelloff/=zero) intag(n)=1
                enddo
              end do
            end if
          end do
! 2nd pass for dim w/ connected 0 offset shell
          nshel=0
          do  ng=1,ngroup
            ity=iparg(5,ng)
            igtyp  = iparg(38,ng)
            if (igtyp == 0.or.(ity /= 3 .and. ity /= 7) ) cycle
            nel=iparg(2,ng)
            nft=iparg(3,ng)
            if (ity == 3)then
              nnode =4
              do i=1,nel
                ie = nft + i
                nn=0
                do j=1,nnode
                  n = ixc(j+1,ie)
                  nn = nn + intag(n)
                enddo
                if (nn>0) nshel = nshel + 1
              end do
            elseif (ity == 7)then
              nnode =3
              do i=1,nel
                ie = nft + i
                nn=0
                do j=1,nnode
                  n = ixtg(j+1,ie)
                  nn = nn + intag(n)
                enddo
                if (nn>0) nshel = nshel + 1
              end do
            end if
          end do
          nsh_oset = nshel
!-----------
        end subroutine dim_shell_offsetp
      end module dim_shell_offsetp_mod
