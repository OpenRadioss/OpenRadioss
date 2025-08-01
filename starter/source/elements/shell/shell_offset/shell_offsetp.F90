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
!||    shell_offsetp_mod   ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||--- called by ------------------------------------------------------
!||    lectur              ../starter/source/starter/lectur.F
!||====================================================================
      module shell_offsetp_mod

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine do the shell offset treatment w/ projection for composite shell
!=======================================================================================================================
!||====================================================================
!||    shell_offsetp             ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||--- called by ------------------------------------------------------
!||    lectur                    ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    dim_shell_offsetp         ../starter/source/elements/shell/shell_offset/dim_shell_offsetp.F90
!||    sh_offset_jonct_chk       ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
!||    sh_offset_nproj           ../starter/source/elements/shell/shell_offset/shell_offset_nproj.F90
!||    sh_offset_setn            ../starter/source/elements/shell/shell_offset/sh_offset_setn.F90
!||--- uses       -----------------------------------------------------
!||    defaults_mod              ../starter/source/modules/defaults_mod.F90
!||    dim_shell_offsetp_mod     ../starter/source/elements/shell/shell_offset/dim_shell_offsetp.F90
!||    sh_offset_jonct_chk_mod   ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
!||    sh_offset_nproj_mod       ../starter/source/elements/shell/shell_offset/shell_offset_nproj.F90
!||    sh_offset_setn_mod        ../starter/source/elements/shell/shell_offset/sh_offset_setn.F90
!||====================================================================
        subroutine shell_offsetp(                                              &
          ngroup,    nparg,      iparg,        npropg,            &
          numgeo,      geo,     numelc,          nixc,            &
          ixc,     numeltg,      nixtg,          ixtg,            &
          numnod,        x,        thk,        itagsh,            &
          defaults_shell)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero,half
          use defaults_mod, only : shell_defaults_
          use sh_offset_nproj_mod, only:sh_offset_nproj
          use sh_offset_setn_mod, only:sh_offset_setn
          use sh_offset_jonct_chk_mod, only:sh_offset_jonct_chk
          use dim_shell_offsetp_mod, only: dim_shell_offsetp
          use precision_mod, only: WP
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                          :: ngroup           !< number of elem group
          integer, intent (in   )                          :: nparg            !< 1er dim of iparg
          integer, intent (in   )                          :: npropg           !< 1er dim of geo
          integer, intent (in   )                          :: numgeo           !< number of prop
          integer, intent (in   )                          :: numelc           !< number shell 4n element
          integer, intent (in   )                          :: nixc             !< 1er dim of ixc
          integer, intent (in   )                          :: numeltg          !< number shell 3n element
          integer, intent (in   )                          :: nixtg            !< 1er dim of ixtg
          integer, intent (in   )                          :: numnod           !< number node
          integer, intent (in   ) ,dimension(nparg,ngroup) :: iparg            !< elem group array
          integer, intent (in   ) ,dimension(nixc,numelc)  :: ixc              !< shell 4n connectivity
          integer, intent (in   ),dimension(nixtg,numeltg) :: ixtg             !< shell 3n connectivity
          real(kind=WP), intent (in   ),dimension(npropg,numgeo) :: geo              !< property array
          real(kind=WP), intent (in  ),dimension(numelc+numeltg) :: thk              !< shell thickness
          real(kind=WP), intent (inout),dimension(3,numnod)      :: x                !< node coordinates
          integer, intent (inout),dimension(numelc+numeltg):: itagsh           !< shell w/ offset
          type(shell_defaults_), intent(inout)             :: defaults_shell   !< /DEF_SHELL variables
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n,nel,nft,nn,ie,igtyp,ity,nnode,pid,ng,nshel,nneoset_g,ix(4)
          integer, dimension(:), allocatable   :: intag,idnneoset
          integer, dimension(:,:), allocatable :: ixnneoset
          real(kind=WP), dimension(:), allocatable   :: shoset_n,sh_oset,thk_g
          real(kind=WP) :: shelloff
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(intag(numnod))
!
          call dim_shell_offsetp(                                              &
            ngroup,    nparg,      iparg,        npropg,                   &
            numgeo,      geo,     numelc,          nixc,                   &
            ixc,  numeltg,      nixtg,          ixtg,                   &
            numnod,    intag,      nshel)
!
          nneoset_g = nshel
          allocate(idnneoset(nneoset_g))
          allocate(ixnneoset(4,nneoset_g))
          allocate(shoset_n(numnod))
          allocate(sh_oset(nneoset_g))
          allocate(thk_g(nneoset_g))
          shoset_n = zero
          nshel = 0
          do  ng=1,ngroup
            ity=iparg(5,ng)
            igtyp  = iparg(38,ng)
            if (igtyp == 0.or.(ity /= 3 .and. ity /= 7) ) cycle
            nel=iparg(2,ng)
            nft=iparg(3,ng)
            pid =iparg(62,ng)
            shelloff = zero
            select case(igtyp)
             case (1,9,10,11,16)
              shelloff = geo(199,pid)    ! updated already in hm_read_prop11
             case (17,51,52)
              shelloff = half+geo(199,pid)   ! respect to the bottom
            end select
            if (ity == 3)then
              nnode =4
              do i=1,nel
                ie = nft + i
                nn=0
                do j=1,nnode
                  n = ixc(j+1,ie)
                  nn = nn + intag(n)
                  ix(j) = n
                enddo
                if (nn>0) then
                  nshel = nshel + 1
                  idnneoset(nshel) = ie
                  ixnneoset(1:nnode,nshel) = ix(1:nnode)
                  sh_oset(nshel) = shelloff
                  if (thk(ie)>zero) then
                    thk_g(nshel) = thk(ie)
                  else
                    thk_g(nshel) = geo(1,pid)
                  end if
                end if
              end do
            elseif (ity == 7)then
              nnode =3
              do i=1,nel
                ie = nft + i
                nn=0
                do j=1,nnode
                  n = ixtg(j+1,ie)
                  nn = nn + intag(n)
                  ix(j) = n
                enddo
                if (nn>0) then
                  nshel = nshel + 1
                  idnneoset(nshel) = ie + numelc  ! same than thke
                  ixnneoset(1:nnode,nshel) = ix(1:nnode)
                  sh_oset(nshel) = shelloff
                  if (thk(ie+numelc)>zero) then
                    thk_g(nshel) = thk(ie+numelc)
                  else
                    thk_g(nshel) = geo(1,pid)
                  end if
                end if
                ixnneoset(4,nshel) = ix(nnode)
              end do
            end if
          end do
!--- check jnuctions- tag elements not used in nodal normal,thk compute
          call sh_offset_jonct_chk(nshel    ,ixnneoset   ,x     ,numnod ,          &
            idnneoset ,sh_oset )
! reduce again dim nshel after junction check
          nshel = 0
          do i = 1,nneoset_g
            ie = idnneoset(i)
            if (ie>0) then
              nshel = nshel + 1
              idnneoset(nshel) = idnneoset(i)
              ixnneoset(1:4,nshel) = ixnneoset(1:4,i)
              sh_oset(nshel) = sh_oset(i)*thk_g(i)
            end if
          end do
          nneoset_g = nshel
! compute nodal thk
          call sh_offset_setn(nshel,numnod,ixnneoset,sh_oset,shoset_n,intag)
! compute nodal normal & do protection
          call sh_offset_nproj(nshel,ixnneoset,numnod,x  ,shoset_n,intag)
          if (defaults_shell%ioffset==1) then
            do i = 1,nshel
              ie = idnneoset(i)
              itagsh(ie) = 1
            end do
          end if
!
          deallocate(intag)
          deallocate(idnneoset)
          deallocate(ixnneoset)
          deallocate(shoset_n)
          deallocate(sh_oset)
          deallocate(thk_g)
!-----------
        end subroutine shell_offsetp
      end module shell_offsetp_mod
