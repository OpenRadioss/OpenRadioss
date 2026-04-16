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
!||    init_rwall_penalty_mod   ../starter/source/constraints/general/rwall/init_rwall_penalty.F90
!||--- called by ------------------------------------------------------
!||    initia                   ../starter/source/elements/initia/initia.F
!||====================================================================
      module init_rwall_penalty_mod

      implicit none

      contains
!=======================================================================================================================
!!\brief This subroutine performs the initialization of stiffness for Rwall penalty
!=======================================================================================================================
!||====================================================================
!||    init_rwall_penalty   ../starter/source/constraints/general/rwall/init_rwall_penalty.F90
!||--- called by ------------------------------------------------------
!||    initia               ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine init_rwall_penalty(elbuf_tab,                                 &
                   numnod,  nparg,  ngroup,  iparg,   nummat,                    &
                   nrwall,  nnprw,    nprw,   lprw,    slprw,                    &
                   numelc,numeltg,  numels,numels8, numels10,                    &
                 numels16,numels20,    ixc,   ixtg,      ixs,                    &
                    ixs10,   ixs16,  ixs20,   ixt ,      ixp,                    &
                      ixr,  numelt, numelp, numelr,    stifn,                    &
                mat_param, sln_pen,stif_pen,nrbykin,  nnpby ,                    &
                   npby  ,  slpby , lpby  ,  nrbe2,    irbe2,                    &
                   nrbe2l,  slrbe2,lrbe2)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use constant_mod,           only: zero,half,one,two,fourth,hundred,three_half,three,ten,em12,zep025
          use element_mod,            only: nixc, nixtg, nixs,nixp,nixr,nixt
          use matparam_def_mod,       only: matparam_struct_
          use precision_mod,          only: WP
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include      "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                          :: numnod           !< number node
          integer, intent (in   )                          :: nummat           !< number material
          integer, intent (in   )                          :: nparg            !< first dimension of iparg
          integer, intent (in   )                          :: nrwall           !< number rwall
          integer, intent (in   )                          :: nnprw            !< 2nd dim of nprw
          integer, intent (in   )                          :: slprw            !< dim of lprw
          integer, intent (in   )                          :: sln_pen          !< dim of stif_pen
          integer, intent (in   )                          :: ngroup           !< number of rwall groups
          integer, intent (in   )                          :: numelc           !< number shell 4n element
          integer, intent (in   )                          :: numeltg          !< number sh3n element
          integer, intent (in   )                          :: numels           !< number solid element
          integer, intent (in   )                          :: numelt           !< number truss element
          integer, intent (in   )                          :: numelp           !< number beam element
          integer, intent (in   )                          :: numelr           !< number spring element
          integer, intent (in   )                          :: numels8          !< number solid (n<=8) element
          integer, intent (in   )                          :: numels10         !< number tet10 element
          integer, intent (in   )                          :: numels16         !< number s16 element
          integer, intent (in   )                          :: numels20         !< number s20 element
          integer, intent (in   ) ,dimension(nparg,ngroup) :: iparg            !< element group data
          integer, intent (in   ) ,dimension(nnprw*nrwall) :: nprw             !< rwall array
          integer, intent (in   ) ,dimension(slprw)        :: lprw             !< rwall 2nd node list
          integer, intent (in   ) ,dimension(nixc,numelc)  :: ixc              !< sh4n node-connectivity
          integer, intent (in   ) ,dimension(nixtg,numeltg):: ixtg             !< sh3n node-connectivity
          integer, intent (in   ) ,dimension(nixs,numels)  :: ixs              !< solid connectivity
          integer, intent (in   ) ,dimension(6,numels10)   :: ixs10            !< tet10 connectivity supp
          integer, intent (in   ) ,dimension(8,numels16)   :: ixs16            !< s16 connectivity supp
          integer, intent (in   ) ,dimension(12,numels20)  :: ixs20            !< s20 connectivity supp
          integer, intent (in   ) ,dimension(nixt,numelt)  :: ixt              !< truss connectivity
          integer, intent (in   ) ,dimension(nixp,numelp)  :: ixp              !< beam connectivity
          integer, intent (in   ) ,dimension(nixr,numelr)  :: ixr              !< spring connectivity
          integer, intent(in)                              :: nrbykin          !< number of rbody
          integer, intent(in)                              :: nnpby            !< first dimension of npby
          integer, intent(in)                              :: slpby            !< dimension of lpby
          integer, dimension(nnpby,nrbykin),intent(in   )  :: npby             !< rbody data
          integer, dimension(slpby),        intent(in   )  :: lpby             !< rbody secondary node data
          integer, intent(in)                              :: nrbe2            !< number of rbe2
          integer, intent(in)                              :: nrbe2l           !< first dimension of irbe2
          integer, intent(in)                              :: slrbe2           !< dimension of lrbe2
          integer, dimension(nrbe2l,nrbe2),intent(in   )   :: irbe2            !< rbe2 data
          integer, dimension(slrbe2),     intent(in   )    :: lrbe2            !< rbe2 secondary node data
          real(kind=WP), intent (in   ) ,dimension(numnod) :: stifn            !< nodal stiffness for interface
          type (elbuf_struct_),  target,dimension(ngroup)  :: elbuf_tab        !< el_buf struct_
          real(kind=WP), intent (inout) ,dimension(sln_pen):: stif_pen         !< penalty Rwall stiffness +leng_m
          type(matparam_struct_),dimension(nummat),intent(in) :: mat_param     !< material parameters
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,n,ii,mid,mtn,pid,iad,ity,nn1,nsl,ns,isolnod,ipen,nel,k,n_p,ng,nft,lspen,iconst,r_id,nsn,ns1,m
          integer, dimension(20,mvsiz)  :: nc
          integer, dimension(:)  ,        allocatable :: imnt,itag
          real(kind=WP), dimension(:)  ,  allocatable :: noda_l
          real(kind=WP) :: sfac,v,l_e
          logical :: is_found
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call my_alloc(imnt,numnod)
          call my_alloc(itag,numnod)
          call my_alloc(noda_l,numnod)
          noda_l = zero
          imnt = 0
          itag = 0
          do ng=1,ngroup
            mtn     = iparg(1,ng)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            ity     = iparg(5,ng)
            isolnod = iparg(28,ng)
            if (ity == 1) then 
                mid   = ixs(1,1+nft)
                if (mat_param(mid)%compressibility == 2) mtn = 42 !same than law42
                do i = 1, nel
                  ii = nft + i
                  do j = 1, 8
                    n = ixs(1+j,ii)
                    if (n>0) imnt(n) = mid
                  end do
                end do
              select case (isolnod)
                case (10)
                 do i = 1, nel
                   ii = nft + i -numels8
                   do j = 1, 6
                     n = ixs10(j,ii)
                     if (n>0) imnt(n) = mid
                   end do
                 end do
                case (16)
                 do i = 1, nel
                   ii = nft + i -numels8 -numels10 -numels20
                   do j = 1, 8
                     n = ixs16(j,ii)
                     if (n>0) imnt(n) = mid
                   end do
                 end do
                case (20)
                 do i = 1, nel
                   ii = nft + i -numels8 -numels10
                   do j = 1, 12
                     n = ixs20(j,ii)
                     if (n>0) imnt(n) = mid
                   end do
                 end do
              end select
            elseif (ity == 3) then
                mid   = ixc(1,1+nft)
                do i = 1, nel
                  ii = nft + i
                  do j = 1, 4
                    n = ixc(1+j,ii)
                    if (n>0) imnt(n) = -mid
                  end do
                end do
            elseif (ity == 7) then
                mid   = ixtg(1,1+nft)
                do i = 1, nel
                  ii = nft + i
                  do j = 1, 3
                    n = ixtg(1+j,ii)
                    if (n>0) imnt(n) = -mid
                  end do
                end do
            end if
          end do !ng=1,ngroup
!
        k = 1
        n_p = 0
        do n = 1, nrwall
          nsl = nprw(n)
          ipen = nprw(n+8*nrwall)
          if (ipen > 0) then 
            do j=1,nsl
              ns = lprw(k+j-1)
              mid     = iabs(imnt(ns))
              mtn     = mat_param(mid)%ilaw
              select case (mtn)
                case (42,69)
                  sfac = two/hundred
                case (70,90)
                  sfac = two*mat_param(mid)%young0/mat_param(mid)%young
                case (28,50,68)
                  sfac = three_half
                case (130)
                  sfac = fourth*mat_param(mid)%young0/mat_param(mid)%young
                case default
                  sfac = one
              end select
              if (imnt(ns)<0) sfac = half !shell
              if (imnt(ns)<0.and.mat_param(mid)%compressibility ==3) sfac = zep025 
              n_p = n_p + 1
              stif_pen(n_p) = sfac*stifn(ns)
            end do
          end if
          k = k + nsl
        end do 
        lspen = n_p
! compute leng_m : thickness for shell
          imnt = 0 ! used to tag number of connection nodes
          do ng=1,ngroup
            mtn     = iparg(1,ng)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            ity     = iparg(5,ng)
            isolnod = iparg(28,ng)
            if (ity == 1) then 
              select case (isolnod)
                case (8)
                 do i = 1, nel
                  ii = i + nft
                    do j = 1,8
                      nc(j,i) = ixs(j+1,ii)
                    enddo
                 end do
                case (4)
                 do i = 1, nel
                   ii = i + nft
                    nc(1,i)=ixs(2,ii)
                    nc(2,i)=ixs(4,ii)
                    nc(3,i)=ixs(7,ii)
                    nc(4,i)=ixs(6,ii)
                 end do
                case (6)
                  do i = 1, nel
                    ii = i + nft
                    nc(1:3,i)=ixs(2:4,ii)
                    nc(4:6,i)=ixs(6:8,ii)
                  end do
                case (10)
                  do i = 1, nel
                    ii = i + nft
                    nc(1,i)=ixs(2,ii)
                    nc(2,i)=ixs(4,ii)
                    nc(3,i)=ixs(7,ii)
                    nc(4,i)=ixs(6,ii)
                    nn1 = ii - numels8
                    do j=1,6
                      nc(j+4,i) = ixs10(j,nn1)
                    enddo
                  end do
                case (16)
                  do i = 1, nel
                    ii = i + nft
                    do j = 1,8
                      nc(j,i) = ixs(j+1,ii)
                    enddo
                    nn1 = ii - (numels8+numels10+numels20)
                    do j=1,8
                      nc(j+8,i) = ixs16(j,nn1)
                    enddo
                  end do
                case (20)
                  do i = 1, nel
                    ii = i + nft
                    do j = 1,8
                      nc(j,i) = ixs(j+1,ii)
                    enddo
                    nn1 = ii - (numels8+numels10)
                    do j=1,12
                      nc(j+8,i) = ixs20(j,nn1)
                    enddo
                   end do
              end select
                do i = 1, nel
                   ii = nft + i
                   v = elbuf_tab(ng)%gbuf%vol(i)
                   l_e = v**(one/three)
                   do j = 1, isolnod
                     n = nc(j,i)
                     if (n>0) then 
                       imnt(n) = imnt(n) + 1 
                       noda_l(n) = noda_l(n) + l_e
                     end if
                   end do
                end do
            elseif (ity == 3) then
                do i = 1, nel
                  l_e = elbuf_tab(ng)%gbuf%thk(i)
                  ii = nft + i
                  do j = 1, 4
                    n = ixc(1+j,ii)
                     if (n>0) then 
                       imnt(n) = imnt(n) + 1 
                       noda_l(n) = noda_l(n) + l_e
                     end if
                  end do
                end do
            elseif (ity == 7) then
                do i = 1, nel
                  l_e = elbuf_tab(ng)%gbuf%thk(i)
                  ii = nft + i
                  do j = 1, 3
                    n = ixtg(1+j,ii)
                     if (n>0) then 
                       imnt(n) = imnt(n) + 1 
                       noda_l(n) = noda_l(n) + l_e
                     end if
                  end do
                end do
            elseif (ity == 4 ) then ! truss element
                do i = 1, nel
                  l_e = elbuf_tab(ng)%gbuf%length(i)
                  ii = nft + i
                  do j = 1, 2
                    n = ixt(1+j,ii)
                     if (n>0) then 
                       imnt(n) = imnt(n) + 1 
                       noda_l(n) = noda_l(n) + l_e
                     end if
                  end do
                end do
            elseif (ity == 5 ) then ! beam element
                do i = 1, nel
                  l_e = elbuf_tab(ng)%gbuf%length(i)
                  ii = nft + i
                  do j = 1, 2
                    n = ixp(1+j,ii)
                     if (n>0) then 
                       imnt(n) = imnt(n) + 1 
                       noda_l(n) = noda_l(n) + l_e
                     end if
                  end do
                end do
            elseif (ity == 6 ) then ! spring element
                do i = 1, nel
                  l_e = elbuf_tab(ng)%gbuf%length(i)
                  ii = nft + i
                  do j = 1, 2
                    n = ixr(1+j,ii)
                     if (n>0) then 
                       imnt(n) = imnt(n) + 1 
                       noda_l(n) = noda_l(n) + l_e
                     end if
                  end do
                end do
             end if
          end do !ng=1,ngroup
        do n = 1, numnod 
          if (imnt(n) > 0) then 
            noda_l(n) = noda_l(n) / imnt(n)
          end if
        end do
        k = 1
        n_p = 0
        do n = 1, nrwall
          nsl = nprw(n)
          ipen = nprw(n+8*nrwall)
          if (ipen > 0) then 
            l_e = zero
            iconst =0
            do j=1,nsl
              ns = lprw(k+j-1)
              l_e = l_e + noda_l(ns)
              mid     = iabs(imnt(ns))
              mtn     = mat_param(mid)%ilaw
              if (mtn==130.or.mat_param(mid)%compressibility ==3) iconst =1
            end do
            n_p = n_p + 1
            stif_pen(lspen+n_p) = l_e / nsl
            if (iconst==1) stif_pen(lspen+n_p) = -l_e / nsl !negative length will not use nonlinear stif
          end if
          k = k + nsl
        end do 
! look at if main node of RBODY or RBE2 are secondary node of RWALL
        is_found = .false.
        do i=1,nrbykin
            m = npby(1,i)
            if (itag(m)==0.and.stifn(m)<em12) then 
              itag(m) = i
              is_found = .true.
            end if
        end do 
      if (is_found) then 
        k = 1
        n_p = 0
        do n = 1, nrwall
          nsl = nprw(n)
          ipen = nprw(n+8*nrwall)
          if (ipen > 0) then 
            do j=1,nsl
              ns = lprw(k+j-1)
              n_p = n_p + 1
              if (itag(ns)>0) then 
                r_id = itag(ns)
                iad = npby(11,r_id)
                nsn = npby(2,r_id)
                do ii = 1,nsn
                  ns1 = lpby(iad+ii)
                  stif_pen(n_p) = stif_pen(n_p) + stifn(ns1) 
                end do 
              end if
            end do
          end if
          k = k + nsl
        end do 
      end if !(is_found) then 
! rbe2        
        itag =0  
        is_found = .false.
        do i=1,nrbe2
            m = irbe2(3,i)
            if (itag(m)==0.and.stifn(m)<em12) then
              itag(m) = i
              is_found = .true.
            end if
        end do 
      if (is_found) then 
        k = 1
        n_p = 0
        do n = 1, nrwall
          nsl = nprw(n)
          ipen = nprw(n+8*nrwall)
          if (ipen > 0) then 
            do j=1,nsl
              ns = lprw(k+j-1)
              n_p = n_p + 1
              if (itag(ns)>0) then 
                r_id = itag(ns)
                iad = irbe2(1,r_id)
                nsn = irbe2(5,r_id)
                do ii = 1,nsn
                  ns1 = lrbe2(iad+ii)
                  stif_pen(n_p) = stif_pen(n_p) + stifn(ns1) 
                end do 
              end if
            end do
          end if
          k = k + nsl
        end do 
      end if !(is_found) then 
!          
          deallocate(imnt)
          deallocate(itag)
          deallocate(noda_l)
!
        end subroutine init_rwall_penalty
!
      end module init_rwall_penalty_mod
