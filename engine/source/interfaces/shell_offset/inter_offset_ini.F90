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
      !||    inter_sh_offset_ini_mod   ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
      !||--- called by ------------------------------------------------------
      !||    resol_init                ../engine/source/engine/resol_init.F
      !||====================================================================
      module inter_sh_offset_ini_mod
      contains
!=======================================================================================================================
!!\brief This subroutine do the initialization for offset treatment
!=======================================================================================================================
      !||====================================================================
      !||    inter_sh_offset_ini       ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
      !||--- called by ------------------------------------------------------
      !||    resol_init                ../engine/source/engine/resol_init.F
      !||--- calls      -----------------------------------------------------
      !||    foat_to_6_float           ../engine/source/system/parit.F
      !||    inter_sh_offset_dim       ../engine/source/interfaces/shell_offset/inter_offset_dim.F90
      !||    spmd_exch_nodarea         ../engine/source/mpi/anim/spmd_exch_nodarea.F
      !||    spmd_exch_nodareai        ../engine/source/mpi/anim/spmd_exch_nodareai.F
      !||    spmd_exch_vnpon           ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod              ../common_source/modules/constant_mod.F
      !||    elbufdef_mod              ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    inter_sh_offset_dim_mod   ../engine/source/interfaces/shell_offset/inter_offset_dim.F90
      !||    inter_sh_offset_mod       ../engine/source/modules/interfaces/sh_offset_mod.F90
      !||    spmd_exch_vnpon_mod       ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
      !||====================================================================
        subroutine inter_sh_offset_ini(                                        &                                    
                       ngroup,    nparg,      iparg,        npropg,            &
                       numgeo,      geo,     numelc,          nixc,            &
                          ixc,  numeltg,      nixtg,          ixtg,            &
                       numnod,    nspmd,   iad_elem,       fr_elem,            &
                     sfr_elem,     thke,  elbuf_tab, sh_offset_tab,            &
                       iparit)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use constant_mod,             only: zero,half
          use inter_sh_offset_mod ,     only: sh_offset_
          use inter_sh_offset_dim_mod , only: inter_sh_offset_dim
          use spmd_exch_vnpon_mod ,     only: spmd_exch_vnpon
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
          integer, intent (in   )                          :: iparit           !< flag P/ON
          integer, intent (in   ) ,dimension(nparg,ngroup) :: iparg            !< elem group array
          integer, intent (in   ) ,dimension(2,nspmd+1)    :: iad_elem         !< index array for comm
          integer, intent (in   ) ,dimension(sfr_elem)     :: fr_elem          !< comm node array
          integer, intent (in   ) ,dimension(nixc,numelc)  :: ixc              !< shell 4n connectivity
          integer, intent (in   ),dimension(nixtg,numeltg) :: ixtg             !< shell 3n connectivity
          my_real, intent (in   ),dimension(numelc+numeltg):: thke             !< initial thickness
          my_real, intent (in   ),dimension(npropg,numgeo) :: geo              !< property array
          type (elbuf_struct_), target, dimension(ngroup)  :: elbuf_tab        !< el_buf struct_
          type (sh_offset_)                                :: sh_offset_tab    !< offset struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i,j,k,n,nel,nft,nn,ie,ii,igtyp,nf1,ity,nnode,pid,nshel,ng,stat,lenr,nsh_oset,nnoset
        integer ibid(1),ndim1,ndim2,nsh_oset_g,nfr
        my_real shelloff
        my_real, dimension(:)  ,  allocatable :: thkoset,thkoset_n    
        double precision, dimension(:,:),  allocatable :: thkoset6,thkoset_n6    
        type(g_bufel_)     , pointer :: gbuf
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call inter_sh_offset_dim(                                                &
            ngroup,    nparg,      iparg,     elbuf_tab,            &
            nsh_oset)

          sh_offset_tab%nsh_oset = nsh_oset
          nshel=0
          if (nsh_oset >0) then
            allocate(sh_offset_tab%ix_offset(4,nsh_oset),STAT=stat)
            allocate(sh_offset_tab%offset_n(numnod),STAT=stat)
            allocate(sh_offset_tab%norm_n(3,numnod),STAT=stat)
            allocate(thkoset(nsh_oset),STAT=stat)
            sh_offset_tab%offset_n = zero
            thkoset = zero
            do  ng=1,ngroup
              ity=iparg(5,ng)
              igtyp  = iparg(38,ng)
              gbuf  => elbuf_tab(ng)%gbuf
              if (igtyp == 0.or.(ity /= 3 .and. ity /= 7).or.gbuf%G_SH_IOFFSET==0 ) cycle
              pid =iparg(62,ng)
              shelloff = zero
              select case(igtyp)
               case (1,9,10,11,16)
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
          else
            allocate(sh_offset_tab%ix_offset(4,0) )
            allocate(sh_offset_tab%offset_n(0) )
            allocate(sh_offset_tab%norm_n(3,0) )
            allocate(thkoset(0) ) 
          end if !(nsh_oset>0) then
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
          allocate(thkoset_n(numnod),STAT=stat)
          thkoset_n = zero
          if (iparit >0) then !P/ON
            allocate(thkoset6(6,nshel),STAT=stat)
            allocate(thkoset_n6(6,numnod),STAT=stat)
            thkoset6 = zero
            thkoset_n6 = zero
            call foat_to_6_float(1  ,nshel  ,thkoset ,thkoset6 )
            do i = 1, nshel
              if (sh_offset_tab%ix_offset(4,i)/=sh_offset_tab%ix_offset(3,i)) then
                nnode = 4
              else
                nnode = 3
              end if
!
              do k = 1,nnode
                n = sh_offset_tab%ix_offset(k,i)
                sh_offset_tab%intag(n) = sh_offset_tab%intag(n) + 1
                thkoset_n6(1:6,n) = thkoset_n6(1:6,n) + thkoset6(1:6,i)
              end do
            end do
            if (nspmd>1) then
              lenr = sh_offset_tab%iad_offset(1,nspmd+1)-sh_offset_tab%iad_offset(1,1)
              ndim1 = 6
              ndim2 = numnod
              call spmd_exch_nodareai(sh_offset_tab%intag,sh_offset_tab%iad_offset,   &
                                    sh_offset_tab%fr_offset,lenr,ibid)  
              call spmd_exch_vnpon(ndim1,ndim2,thkoset_n6,sh_offset_tab%iad_offset,   &
                                    sh_offset_tab%fr_offset,nspmd,lenr)
            end if

            do n = 1, numnod
              if (sh_offset_tab%intag(n)==0) cycle
              do k=1,6
                thkoset_n(n) = thkoset_n(n) + thkoset_n6(k,n)
              enddo
            end do
            deallocate(thkoset6)
            deallocate(thkoset_n6)
          else
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
                thkoset_n(n) = thkoset_n(n) + thkoset(i)
              end do
            end do
            if (nspmd>1) then
              lenr = sh_offset_tab%iad_offset(1,nspmd+1)-sh_offset_tab%iad_offset(1,1)
              call spmd_exch_nodareai(sh_offset_tab%intag,sh_offset_tab%iad_offset,     &
                                    sh_offset_tab%fr_offset,lenr,ibid)  
              call spmd_exch_nodarea(thkoset_n,sh_offset_tab%iad_offset,   &
                                    sh_offset_tab%fr_offset,lenr,ibid)
            end if
          
          end if !(iparit >0) then 
          do n = 1, numnod
            if (sh_offset_tab%intag(n)==0) cycle
            thkoset_n(n) = thkoset_n(n)/sh_offset_tab%intag(n)
            if (thkoset_n(n)==zero) sh_offset_tab%intag(n)=0
          end do
          if (nsh_oset >0) deallocate(thkoset)
! reducing nodal dim
          nnoset=0
          do n = 1, numnod
            if (sh_offset_tab%intag(n)>0) nnoset = nnoset + 1
          end do
          sh_offset_tab%nnsh_oset = nnoset
          allocate(sh_offset_tab%indexg(nnoset),STAT=stat)
          allocate(sh_offset_tab%offset_n(nnoset),STAT=stat)
          allocate(sh_offset_tab%norm_n(3,nnoset),STAT=stat)
          if (iparit >0) allocate(sh_offset_tab%norm_n6(6,3,nnoset),STAT=stat)
          nnoset=0
          do n = 1, numnod
            if (sh_offset_tab%intag(n)>0) then 
              nnoset = nnoset + 1
              sh_offset_tab%indexg(nnoset) = n
              sh_offset_tab%intag(n) = nnoset
              sh_offset_tab%offset_n(nnoset) = thkoset_n(n)
            end if
          end do
!  update sh_offset_tab%iad_offset, sh_offset_tab%fr_offset         
          if (nspmd>1) then
              nfr = 0
              do i = 1, nspmd
                do j=iad_elem(1,i),iad_elem(1,i+1)-1
                  n = fr_elem(j)
                  if (sh_offset_tab%intag(n)>0) nfr = nfr + 1
                enddo
              enddo
              deallocate(sh_offset_tab%fr_offset)
              allocate(sh_offset_tab%fr_offset(nfr),STAT=stat)
              sh_offset_tab%iad_offset(1,1) = 1
              k = 0
              do i = 1, nspmd
                do j=iad_elem(1,i),iad_elem(1,i+1)-1
                  n = fr_elem(j)
                  ii = sh_offset_tab%intag(n)
                  if (ii>0) then
                    k = k + 1
                    sh_offset_tab%fr_offset(k) = ii
                  end if
                enddo
                sh_offset_tab%iad_offset(1,i+1) = k+1
              enddo
            end if !(nspmd>1)
          deallocate(thkoset_n)
        end subroutine inter_sh_offset_ini
      end module inter_sh_offset_ini_mod
