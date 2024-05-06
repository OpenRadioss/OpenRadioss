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
      module inter_sh_offset_ini_mod
      contains
!=======================================================================================================================
!!\brief This subroutine do the initialization for offset treatment
!=======================================================================================================================
        subroutine inter_sh_offset_ini(                                         &
          ngroup,    nparg,      iparg,        npropg,            &
          numgeo,      geo,     numelc,          nixc,            &
          ixc,  numeltg,      nixtg,          ixtg,            &
          numnod,    nspmd,   iad_elem,       fr_elem,            &
          sfr_elem,     thke,  elbuf_tab,      sh_offset_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use constant_mod, only : zero,half
          use inter_sh_offset_mod , only:sh_offset_
          use inter_sh_offset_dim_mod , only:inter_sh_offset_dim
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
          integer, intent (in   )                          :: numelc           !< number shell 4n element
          integer, intent (in   )                          :: nixc             !< 1er dim of ixc
          integer, intent (in   )                          :: numeltg          !< number shell 3n element
          integer, intent (in   )                          :: nixtg            !< 1er dim of ixtg
          integer, intent (in   )                          :: numnod           !< number node
          integer, intent (in   )                          :: nspmd            !< number of domains
          integer, intent (in   )                          :: sfr_elem         !< number of comm nodes
          integer, intent (in   ) ,dimension(nparg,ngroup) :: iparg            !< elem group array
          integer, intent (in   ) ,dimension(2,nspmd+1)    :: iad_elem         !< index array for comm
          integer, intent (in   ) ,dimension(sfr_elem)     :: fr_elem          !< comm node arry
          integer, intent (in   ) ,dimension(nixc,numelc)  :: ixc              !< shell 4n connectivity
          integer, intent (in   ),dimension(nixtg,numeltg) :: ixtg             !< shell 3n connectivity
          my_real, intent (in   ),dimension(numelc+numeltg):: thke             !< initial thikness
          my_real, intent (in   ),dimension(npropg,numgeo) :: geo              !< property array
          type (elbuf_struct_), target, dimension(ngroup)  :: elbuf_tab        !< el_buf struct_
          type (sh_offset_)                                :: sh_offset_tab         !< offset struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer i,j,k,n,nel,nft,nn,ie,ii,igtyp,nf1,ity,nnode,pid,nshel,ng,stat,lenr,nsh_oset
          integer ibid(1)
          my_real shelloff
          my_real, dimension(:)  ,  allocatable :: thkoset
          type(g_bufel_)     , pointer :: gbuf
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call inter_sh_offset_dim(                                                &
            ngroup,    nparg,      iparg,     elbuf_tab,            &
            nsh_oset)

          sh_offset_tab%nsh_oset = nsh_oset
          if (nsh_oset >0) then
            allocate(sh_offset_tab%ix_offset(4,nsh_oset),STAT=stat)
            allocate(sh_offset_tab%offset_n(numnod),STAT=stat)
            allocate(sh_offset_tab%norm_n(3,numnod),STAT=stat)
            allocate(thkoset(nsh_oset),STAT=stat)
            sh_offset_tab%offset_n = zero
            thkoset = zero
            nshel=0
            do  ng=1,ngroup
              ity=iparg(5,ng)
              igtyp  = iparg(38,ng)
              gbuf  => elbuf_tab(ng)%gbuf
              if (igtyp == 0.or.(ity /= 3 .and. ity /= 7).or.gbuf%G_SH_IOFFSET==0 ) cycle
              pid =iparg(62,ng)
              shelloff = zero
              select case(igtyp)
               case (11)
                shelloff = geo(199,pid)
               case (17,51,52)
                shelloff = half + geo(199,pid)
              end select
              nel=iparg(2,ng)
              nft=iparg(3,ng)
              if (ity == 3)then
                nnode =4
                do i=1,nel
                  ie = nft + i
                  if (gbuf%sh_ioffset(i)>0) then
                    nshel= nshel + 1
                    sh_offset_tab%ix_offset(1:nnode,nshel) =ixc(2:nnode+1,ie)
                    thkoset(nshel)=shelloff*thke(ie)
                  end if
                end do
              elseif (ity == 7)then
                nnode =3
                do i=1,nel
                  ie = nft + i
                  if (gbuf%sh_ioffset(i)>0) then
                    nshel= nshel + 1
                    sh_offset_tab%ix_offset(1:nnode,nshel) = ixtg(2:nnode+1,ie)
                    thkoset(nshel) = shelloff*thke(ie+numelc)
                    sh_offset_tab%ix_offset(4,nshel) = sh_offset_tab%ix_offset(3,nshel)
                  end if
                end do
              end if
            end do
            allocate(sh_offset_tab%intag(numnod),STAT=stat)
! initialize comm
            if (nspmd>1) then
              sh_offset_tab%intag = 0
              do i = 1, nshel
                do k = 1,4
                  n = sh_offset_tab%ix_offset(k,i)
                  sh_offset_tab%intag(n) = 1
                end do
              end do
              lenr = iad_elem(1,nspmd+1)-iad_elem(1,1)
              call spmd_exch_nodareai(sh_offset_tab%intag,iad_elem,fr_elem,lenr,ibid)
              nn = 0
              do i = 1, nspmd
                do j=iad_elem(1,i),iad_elem(1,i+1)-1
                  n = fr_elem(j)
                  if (sh_offset_tab%intag(n)>0) nn = nn + 1
                enddo
              enddo
              allocate(sh_offset_tab%iad_offset(2,nspmd+1),STAT=stat) ! dim (2,*) to use existing spmd_exch routines
              sh_offset_tab%iad_offset= 0
              allocate(sh_offset_tab%fr_offset(nn),STAT=stat)
              sh_offset_tab%iad_offset(1,1) = 1
              k = 0
              do i = 1, nspmd
                do j=iad_elem(1,i),iad_elem(1,i+1)-1
                  n = fr_elem(j)
                  if (sh_offset_tab%intag(n)>0) then
                    k = k + 1
                    sh_offset_tab%fr_offset(k) = n
                  end if
                enddo
                sh_offset_tab%iad_offset(1,i+1) = k+1
              enddo
            end if !(nspmd>1)
!  compute offset_n
            sh_offset_tab%intag = 0
            do i = 1, nshel
!------each node
              if (sh_offset_tab%ix_offset(4,i)/=sh_offset_tab%ix_offset(3,i)) then
                nnode = 4
              else
                nnode = 3
              end if
!
              do k = 1,nnode
                n = sh_offset_tab%ix_offset(k,i)
                sh_offset_tab%intag(n) = sh_offset_tab%intag(n) + 1
                sh_offset_tab%offset_n(n) = sh_offset_tab%offset_n(n) + thkoset(i)
              end do
            end do
            if (nspmd>1) then
              lenr = sh_offset_tab%iad_offset(1,nspmd+1)-sh_offset_tab%iad_offset(1,1)
              call spmd_exch_nodareai(sh_offset_tab%intag,sh_offset_tab%iad_offset,           &
                sh_offset_tab%fr_offset,lenr,ibid)
              call spmd_exch_nodarea(sh_offset_tab%offset_n,sh_offset_tab%iad_offset,         &
                sh_offset_tab%fr_offset,lenr,ibid)
            end if

            do n = 1, numnod
              if (sh_offset_tab%intag(n)==0) cycle
              sh_offset_tab%offset_n(n) = sh_offset_tab%offset_n(n)/sh_offset_tab%intag(n)
              if (sh_offset_tab%offset_n(n)==zero) sh_offset_tab%intag(n)=0
            end do
            deallocate(thkoset)
          end if !(nsh_oset>0) then
        end subroutine inter_sh_offset_ini
      end module inter_sh_offset_ini_mod
