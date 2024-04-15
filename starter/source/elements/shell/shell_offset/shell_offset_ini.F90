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
      module shell_offset_ini_mod

        contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine do the initialization of shell offset treatment  
!=======================================================================================================================
        subroutine shell_offset_ini(                                          &                                    
                       ngroup,    nparg,      iparg,        npropg,           &
                       numgeo,      geo,     numelc,       numeltg,           &
                      npropgi,     igeo,     itagsh,     elbuf_tab,           &
                      defaults_shell)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use constant_mod, only : zero,half
          use defaults_mod, only : shell_defaults_
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent (in   )                          :: ngroup           !< number of elem group
        integer, intent (in   )                          :: nparg            !< 1er dim of iparg
        integer, intent (in   )                          :: npropg           !< 1er dim of geo
        integer, intent (in   )                          :: numgeo           !< number of prop
        integer, intent (in   )                          :: npropgi          !< 1er dim of igeo
        integer, intent (in   )                          :: numelc           !< number shell 4n element
        integer, intent (in   )                          :: numeltg          !< number shell 3n element
        integer, intent (in   ) ,dimension(nparg,ngroup) :: iparg            !< elem group array
        integer, intent (inout),dimension(npropgi,numgeo):: igeo             !< property array
        my_real, intent (inout),dimension(npropg,numgeo) :: geo              !< property array
        integer, intent (in   ),dimension(numelc+numeltg):: itagsh           !< shell w/ offset
        type (elbuf_struct_),  target, dimension(ngroup) :: elbuf_tab        !< el_buf struct_
        type (shell_defaults_),intent(in)                :: defaults_shell   !< Default values for Shell : /DEF_SHELL option
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i,j,n,nel,nft,nn,ie,igtyp,nf1,ity,nnode,pid,ng
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------  
      if (defaults_shell%ioffset==1) then
        nn = 0
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
              if (itagsh(ie)>0) elbuf_tab(ng)%gbuf%sh_ioffset(i)=1
              if (itagsh(ie)>0) nn = nn+1
            end do
          elseif (ity == 7)then
            nnode =3
            do i=1,nel
              ie = nft +numelc+ i
              if (itagsh(ie)>0) elbuf_tab(ng)%gbuf%sh_ioffset(i)=1
              if (itagsh(ie)>0) nn = nn+1
            end do
          end if
        end do
      elseif (defaults_shell%ioffset>=3) then
!---change offset to zero after projection
        do  ng=1,ngroup
          ity=iparg(5,ng)
          igtyp  = iparg(38,ng)
          if (igtyp == 0.or.(ity /= 3 .and. ity /= 7) ) cycle
          pid =iparg(62,ng) 
          select case(igtyp)
            case (17,51,52)
              geo(198,pid) = geo(199,pid)    ! old value saved in 198
              if (geo(199,pid)/=-half) geo(199,pid) = -half
              if (igeo(99,pid) == 2) igeo(99,pid) = -2  ! avoid case Ipos=2
          end select
        end do
      end if
!-----------
      end subroutine shell_offset_ini
!    
      end module shell_offset_ini_mod
