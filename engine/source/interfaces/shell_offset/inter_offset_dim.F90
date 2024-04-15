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
    module inter_sh_offset_dim_mod
        contains
  ! ======================================================================================================================
  !                                                   PROCEDURES
  ! ======================================================================================================================
  !
  !=======================================================================================================================
  !!\brief This subroutine get number of shell w/ offset treatment
  !=======================================================================================================================
          subroutine inter_sh_offset_dim(                                         &                                    
                         ngroup,    nparg,      iparg,     elbuf_tab,            &
                         nsh_oset)
  ! ----------------------------------------------------------------------------------------------------------------------
  !                                                   Modules
  ! ----------------------------------------------------------------------------------------------------------------------
            use elbufdef_mod
            use inter_sh_offset_mod , only:sh_offset_
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
          integer, intent (in   ) ,dimension(nparg,ngroup):: iparg            !< elem group array
          integer, intent (inout)                         :: nsh_oset         !< number offset shell 
          type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab        !< el_buf struct_
  ! ----------------------------------------------------------------------------------------------------------------------
  !                                                   Local variables
  ! ----------------------------------------------------------------------------------------------------------------------
          integer i,j,n,nel,nft,nn,ie,igtyp,ity,nshel,ng
          type(g_bufel_)     , pointer :: gbuf
  !
  ! ----------------------------------------------------------------------------------------------------------------------
  !                                                   Body
  ! ----------------------------------------------------------------------------------------------------------------------
        nshel = 0  
        do  ng=1,ngroup
          ity=iparg(5,ng)
          igtyp  = iparg(38,ng)
          gbuf  => elbuf_tab(ng)%gbuf                                  
          if (igtyp == 0.or.(ity /= 3 .and. ity /= 7).or.gbuf%G_SH_IOFFSET==0 ) cycle
          nel=iparg(2,ng)
          do i=1,nel
            if (gbuf%sh_ioffset(i)>0) nshel= nshel + 1
          end do
        end do
        nsh_oset = nshel
  !-----------
        end subroutine inter_sh_offset_dim
    end module inter_sh_offset_dim_mod