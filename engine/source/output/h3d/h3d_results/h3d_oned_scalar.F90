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
      !||    h3d_oned_scalar_mod   ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F90
      !||--- called by ------------------------------------------------------
      !||    genh3d                ../engine/source/output/h3d/h3d_results/genh3d.F
      !||====================================================================
      module h3d_oned_scalar_mod
      contains
!! \brief fill the scalar values for the 1D elements
      !||====================================================================
      !||    h3d_oned_scalar        ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F90
      !||--- called by ------------------------------------------------------
      !||    genh3d                 ../engine/source/output/h3d/h3d_results/genh3d.F
      !||--- calls      -----------------------------------------------------
      !||    h3d_write_scalar       ../engine/source/output/h3d/h3d_results/h3d_write_scalar.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod           ../common_source/modules/constant_mod.F
      !||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
        subroutine h3d_oned_scalar(  elbuf_tab      , iparg    , geo   , ixt     ,            &
        &  ixp            , ixr      , pm    , anim    ,            &
        &  oned_scalar    , id_elem , ity_elem ,                    &
        &  is_written_oned, ipartt  , ipartp   , ipartr, h3d_part,  &
        &  keyword        , x       , d        ,ipt,                &
        &  numelp, numelt , numelr  ,nixt     ,nixp,                &
        &  nixr ,ngroup   ,anim_fe  ,mx_ani   ,nparg,               &
        &  npropm, npropg , nummat  ,numgeo   ,numnod,              &
        &  sz_anin,numelpg,numelrg,numeltrg,npart                   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use elbufdef_mod
          use names_and_titles_mod, only: ncharline100
! ----------------------------------------------------------------------------------------------------------------------
!                                                     implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     include
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          ! Input arguments
          ! ----------------------------------------
          ! Integer arrays
          integer, intent(in) :: numelp
          integer, intent(in) :: numelt
          integer, intent(in) :: numelr
          integer, intent(in) :: numelpg
          integer, intent(in) :: numeltrg
          integer, intent(in) :: numelrg
          integer, intent(in) :: nixt
          integer, intent(in) :: nixp
          integer, intent(in) :: nixr
          integer, intent(in) :: ngroup
          integer, intent(in) :: mx_ani
          integer, intent(in) :: nparg
          integer, intent(in) :: npropm
          integer, intent(in) :: npropg
          integer, intent(in) :: nummat
          integer, intent(in) :: numgeo
          integer, intent(in) :: numnod
          integer, intent(in) :: sz_anin
          integer, intent(in) :: ipt
          integer, intent(in) :: npart
          integer, dimension(mx_ani), intent(in) :: anim_fe
          integer, dimension(nixt,numelt), intent(in) :: ixt
          integer, dimension(nixp,numelp), intent(in) :: ixp
          integer, dimension(nixr,numelr), intent(in) :: ixr
          integer, dimension(nparg,ngroup),intent(in) :: iparg
          integer, dimension(numelt),intent(in)       :: ipartt
          integer, dimension(numelp),intent(in)       :: ipartp
          integer, dimension(numelr),intent(in)       :: ipartr
          integer, dimension(npart),intent(in)        :: h3d_part
          ! Float arrays
          my_real, intent(in) :: anim(sz_anin)
          my_real, intent(in) :: geo(npropg,numgeo)
          my_real, intent(in) :: pm(npropm,nummat)
          my_real, intent(in) :: x(3,numnod)
          my_real, intent(in) :: d(3,numnod)

          ! Type arrays
          type (elbuf_struct_), dimension(ngroup), target :: elbuf_tab

          ! Characters
          character(len=ncharline100) :: keyword

          ! Output arguments
          ! ----------------------------------------
          ! INTEGER arrays
          integer, dimension(numelpg+numelrg+numeltrg),intent(inout) :: ity_elem
          integer, dimension(numelpg+numelrg+numeltrg),intent(inout) :: id_elem
          integer, dimension(numelpg+numelrg+numeltrg),intent(inout) :: is_written_oned

          ! Float Arrays
          my_real, intent(inout) :: oned_scalar(numelt+numelp+numelr)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Local variables
! ----------------------------------------------------------------------------------------------------------------------
          MY_REAL :: MASS(MVSIZ)
          MY_REAL :: VALUE(MVSIZ)
          MY_REAL :: OFF
          MY_REAL :: A1,   B1, B2,    B3
          MY_REAL :: F1,   M1, M2,    M3
          MY_REAL :: YEQ,  XM, DAMMX, VOL, DAMINI
          MY_REAL :: FOR,  AREA, FEQ, EPLAS
          MY_REAL :: RHO0, A0, XX1, YY1, ZZ1
          MY_REAL :: AL0,EFRAC

          INTEGER I,  NG, NEL, NFT,  ITY,  NPT
          INTEGER N,   MLW, IGTYP,IFAIL, IPID
          INTEGER NN1,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10
          INTEGER OFFSET,K,KK,ILAYER,IR,IS,JJ(6),IOK_PART(MVSIZ)
          INTEGER IS_WRITTEN_VALUE(MVSIZ),N1, N2
!-----------------------------------------------
!
          nn1 = 1
          nn3 = 1
          nn4 = nn3
          nn5 = nn4
          nn6 = nn5
          nn7 = nn6 + numelt
          nn8 = nn7 + numelp
          nn9 = nn8 + numelr
          nn10= nn9
!
          do ng=1,ngroup
            mlw   =iparg(1,ng)
            nel   =iparg(2,ng)
            ity   =iparg(5,ng)
            igtyp =iparg(38,ng)
            ifail =iparg(43,ng)
!---
            nft   =iparg(3,ng)
            do i=1,6
              jj(i) = nel*(i-1)
            enddo

            do i=1,nel
              value(i) = zero
              is_written_value(i) = 0
            enddo
!
            if (ity == 4) offset = 0
            if (ity == 5) offset = numelt
            if (ity == 6) offset = numelt+numelp
!
            do  i=1,nel
              if (ity == 4) then
                id_elem(offset+nft+i) = ixt(nixt,nft+i)
                ity_elem(offset+nft+i) = 4
                if( h3d_part(ipartt(nft+i)) == 1) iok_part(i) = 1
              elseif (ity == 5) then
                id_elem(offset+nft+i) = ixp(nixp,nft+i)
                ity_elem(offset+nft+i) = 5
                if( h3d_part(ipartp(nft+i)) == 1) iok_part(i) = 1
              elseif (ity == 6) then
                id_elem(offset+nft+i) = ixr(nixr,nft+i)
                ity_elem(offset+nft+i) = 6
                if( h3d_part(ipartr(nft+i)) == 1) iok_part(i) = 1
              endif
            enddo

            if(ity==4 .or. ity==5 .or. ity==6)then
              do i=1,nel
                oned_scalar(offset+nft+i) = zero   ! default = zero in all cases !
              enddo
            endif
!-----------------------------------------------
! MASS COMPUTATION
!-----------------------------------------------
         if (keyword == 'MASS' .OR. keyword == 'ENER') then
!-----------------------------------------------
!       truss
!-----------------------------------------------
              if (ity == 4) then
                do i=1,nel
                  n = i + nft
                  rho0 = pm(1,ixt(1,n))
                  a0 = geo(1,ixt(4,n))
                  n1 = ixt(2,n)
                  n2 = ixt(3,n)
                  xx1 = x(1,n2)-d(1,n2)-x(1,n1)+d(1,n1)
                  yy1 = x(2,n2)-d(2,n2)-x(2,n1)+d(2,n1)
                  zz1 = x(3,n2)-d(3,n2)-x(3,n1)+d(3,n1)
                  al0  = sqrt(xx1*xx1 + yy1*yy1 + zz1*zz1)
                  mass(i) = rho0*al0*a0
                enddo
!-----------------------------------------------
!       poutres
!-----------------------------------------------
              elseif (ity == 5) then
                do i=1,nel
                  n = i + nft
                  rho0 = pm(1,ixp(1,n))
                  a0 = geo(1,ixp(5,n))
                  n1 = ixp(2,n)
                  n2 = ixp(3,n)
                  xx1 = x(1,n2)-d(1,n2)-x(1,n1)+d(1,n1)
                  yy1 = x(2,n2)-d(2,n2)-x(2,n1)+d(2,n1)
                  zz1 = x(3,n2)-d(3,n2)-x(3,n1)+d(3,n1)
                  al0  = sqrt(xx1*xx1 + yy1*yy1 + zz1*zz1)
                  mass(i) = rho0*al0*a0
                enddo
!-----------------------------------------------
!       ressorts
!-----------------------------------------------
              elseif (ity == 6) then
                if(mlw==3)then
                  do i=1,nel
                    n = i + nft
                    mass(i) = half*geo(1,ixr(1,n))
                  enddo
                elseif (mlw == 5) then
                  do i=1,nel
                    n = i + nft
                    mass(i) = elbuf_tab(ng)%gbuf%mass(i)
                  enddo
                else
                  do i=1,nel
                    n = i + nft
                    mass(i) = geo(1,ixr(1,n))
                  enddo
                endif ! if(mlw)
              endif ! if (ity)
            endif
!-----------------------------------------------
!       truss
!-----------------------------------------------
            if(ity==4)then
!--------------------------------------------------
              if (keyword == 'MASS') then
!--------------------------------------------------
                do i=1,nel
                  value(i) = mass(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'EPSP')then
!--------------------------------------------------
                if(mlw/=1)then
                  do  i=1,nel
                    off = elbuf_tab(ng)%gbuf%off(i)
                    value(i) = elbuf_tab(ng)%gbuf%pla(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'ENER')then
!--------------------------------------------------
                do i=1,nel
                  value(i) =elbuf_tab(ng)%gbuf%eint(i)/max(em30,mass(i))
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'EINT')then
!--------------------------------------------------
                do i=1,nel
                  value(i) =elbuf_tab(ng)%gbuf%eint(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'VONM')then
!--------------------------------------------------
                do i=1,nel
                  for = elbuf_tab(ng)%gbuf%for(i)
                  area = elbuf_tab(ng)%gbuf%area(i)
                  feq = for*for
                  value(i) = sqrt(feq)/area
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SIGX')then
!--------------------------------------------------
                do i=1,nel
                  value(i)=(elbuf_tab(ng)%gbuf%for(i))/(elbuf_tab(ng)%gbuf%area(i))
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'DT')then
!--------------------------------------------------
                if(elbuf_tab(ng)%gbuf%g_dt>0)then
                  do i=1,nel
                    value(i) = elbuf_tab(ng)%gbuf%dt(i)
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif (keyword == 'AMS'.and.(elbuf_tab(ng)%gbuf%g_isms>0)) then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%isms(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'OFF')then
!--------------------------------------------------
                do i=1,nel
                  if (elbuf_tab(ng)%gbuf%g_off > 0) then
                    if(elbuf_tab(ng)%gbuf%off(i) > one) then
                      value(i) = elbuf_tab(ng)%gbuf%off(i) - one
                    elseif((elbuf_tab(ng)%gbuf%off(i) >= zero .and. elbuf_tab(ng)%gbuf%off(i) <= one)) then
                      value(i) = elbuf_tab(ng)%gbuf%off(i)
                    else
                      value(i) = -one
                    endif
                  endif
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'GROUP')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = ng
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'INTERNAL.ID')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = i+nft
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'LOCAL.ID')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = i
                  is_written_value(i) = 1
                enddo
              endif
!-----------------------------------------------
!       poutres
!-----------------------------------------------
            elseif(ity==5)then
!--------------------------------------------------
              if (keyword == 'MASS') then
!--------------------------------------------------
                do i=1,nel
                  value(i) = mass(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif (keyword == 'EPSP') then
!--------------------------------------------------
                if (mlw /= 1) then
                  if (igtyp == 18) then
!   ilayer=null,   npt=null
                    ilayer=1
                    ir = 1
                    is = 1
                    npt  = iparg(6,ng)
                    if (ipt == -1 .and. elbuf_tab(ng)%gbuf%g_pla > 0) then
                      do  i=1,nel
                        eplas = zero
                        do k = 1,npt
                          eplas = eplas + elbuf_tab(ng)%bufly(ilayer)%lbuf(ir,is,k)%pla(i)
                        enddo
                        value(i) = eplas/npt
                        is_written_value(i) = 1
                      enddo
!   ilayer=null,   npt=ipt
                    elseif ( ipt <= npt .and. ipt > 0 .and. elbuf_tab(ng)%gbuf%g_pla > 0) then
                      do  i=1,nel
                        value(i) = elbuf_tab(ng)%bufly(ilayer)%lbuf(ir,is,ipt)%pla(i)
                        is_written_value(i) = 1
                      enddo
                    endif ! if (ipt == -1 .and. elbuf_tab(ng)%gbuf%g_pla > 0)
                  else ! (igtyp == 3)
                    do  i=1,nel
                      off = elbuf_tab(ng)%gbuf%off(i)
                      value(i) = elbuf_tab(ng)%gbuf%pla(i)
                      is_written_value(i) = 1
                    enddo
                  endif ! if (igtyp == 18)
                endif ! if (mlw /= 1)
!--------------------------------------------------
              elseif(keyword == 'ENER')then
!--------------------------------------------------
                do i=1,nel
!a mass a recalculer !!        value(i) = (elbuf_tab(ng)%gbuf%eint(i) + elbuf_tab(ng)%gbuf%eint(i)) / max(em30,mass(nft+i))
                  value(i) = (elbuf_tab(ng)%gbuf%eint(i) + elbuf_tab(ng)%gbuf%eint(i))/ max(em30,mass(i))
                  is_written_value(i) = 1

                enddo
!--------------------------------------------------
              elseif(keyword == 'EINTV')then
!--------------------------------------------------
                do i=1,nel
                  n  = i + nft
                  ipid = ixp(5,n)
                  vol = geo(1,ipid)*elbuf_tab(ng)%gbuf%length(i)
                  value(i) = (elbuf_tab(ng)%gbuf%eint(i)+elbuf_tab(ng)%gbuf%eint(i+nel))/max(vol,em20)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'VONM')then
!--------------------------------------------------
                do i=1,nel
                  n = i + nft
                  a1 = geo(1,ixp(5,n))
                  b1 = geo(2,ixp(5,n))
                  b2 = geo(18,ixp(5,n))
                  b3 = geo(4,ixp(5,n))
                  f1 = elbuf_tab(ng)%gbuf%for(jj(1)+i)
                  m1 = elbuf_tab(ng)%gbuf%mom(jj(1) + i)
                  m2 = elbuf_tab(ng)%gbuf%mom(jj(2) + i)
                  m3 = elbuf_tab(ng)%gbuf%mom(jj(3) + i)
                  yeq= f1*f1 + three* a1 *&
                  &( m1*m1 / max(b3,em30)&
                  &+ m2*m2 / max(b1,em30)&
                  &+ m3*m3 / max(b2,em30) )
                  value(i) = sqrt(yeq)/a1
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SIGX')then
!--------------------------------------------------
                do i=1,nel
                  n = i + nft
                  value(i) = elbuf_tab(ng)%gbuf%for(jj(1)+i) / geo(1,ixp(5,n))
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SIGXY')then
!--------------------------------------------------
                do i=1,nel
                  n = i + nft
                  value(i) = elbuf_tab(ng)%gbuf%for(jj(2)+i) / geo(1,ixp(5,n))
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'SIGZX')then
!--------------------------------------------------
                do i=1,nel
                  n = i + nft
                  value(i) = elbuf_tab(ng)%gbuf%for(jj(3)+i) / geo(1,ixp(5,n))
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'DT')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%dt(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif (keyword == 'AMS'.and.(elbuf_tab(ng)%gbuf%g_isms>0)) then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%isms(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'OFF')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%off(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'FRACTION/MARTENSITE')then
!--------------------------------------------------
                if (igtyp == 18) then
!   ilayer=null,   npt=null
                  if (mlw == 71) then
                    ilayer=1
                    ir = 1
                    is = 1
                    npt  = iparg(6,ng)
                    if (ipt == -1 ) then ! no npt= in engine file
                      do  i=1,nel
                        efrac = zero
                        do k = 1,npt
                          efrac = efrac + elbuf_tab(ng)%bufly(ilayer)%lbuf(ir,is,k)%frac(i)
                        enddo
                        value(i) = efrac/npt
                        is_written_value(i) = 1
                      enddo
!   ilayer=null,   npt=ipt
                    elseif ( ipt <= npt .and. ipt > 0 ) then
                      do  i=1,nel
                        value(i) =  elbuf_tab(ng)%bufly(ilayer)%lbuf(ir,is,ipt)%frac(i)
                        is_written_value(i) = 1
                      enddo
                    endif ! if (ipt == -1 .and. elbuf_tab(ng)%gbuf%g_pla > 0)
                  endif !(mlw /= 71)
                endif !(igtyp == 18)
!--------------------------------------------------
              elseif(keyword == 'TEPS')then
!--------------------------------------------------
                if (igtyp == 18) then
!   ilayer=null,   npt=null
                  if (mlw == 71) then
                    ilayer=1
                    ir = 1
                    is = 1
                    npt  = iparg(6,ng)
                    if (ipt == -1 ) then ! no npt= in engine file
                      do  i=1,nel
                        efrac = zero
                        do k = 1,npt
                          efrac = efrac + elbuf_tab(ng)%bufly(ilayer)%lbuf(ir,is,k)%pla(i)
                        enddo
                        value(i) = efrac/npt
                        is_written_value(i) = 1
                      enddo
!   ilayer=null,   npt=ipt
                    elseif ( ipt <= npt .and. ipt > 0 ) then

                      do  i=1,nel
                        value(i) = elbuf_tab(ng)%bufly(ilayer)%lbuf(ir,is,ipt)%pla(i)
                        is_written_value(i) = 1
                      enddo
                    endif ! if (ipt == -1 .and. elbuf_tab(ng)%gbuf%g_pla > 0)
                  endif !(mlw /= 71)
                endif !(igtyp == 18)

!--------------------------------------------------
              elseif(keyword == 'FRACTION/MARTENSITE/TMAX')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%maxfrac(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'GROUP')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = ng
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'INTERNAL.ID')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = i+nft
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'LOCAL.ID')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = i
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'EPSD')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%epsd(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif (keyword == 'DAMINI' .and. ifail > 0) then
!--------------------------------------------------
                if (igtyp == 3) then 
                  do i=1,nel
                    if (elbuf_tab(ng)%gbuf%g_dmgscl > 0) then 
                      value(i) = elbuf_tab(ng)%gbuf%fail(1)%damini(i)
                      is_written_value(i) = 1
                    endif
                  enddo
                else if (igtyp == 18) then
                    do i=1,nel
                      damini  = zero
                      do k = 1,elbuf_tab(ng)%bufly(1)%nptt
                        if (elbuf_tab(ng)%bufly(1)%fail(1,1,k)%floc(1)%lf_damini > 0) then
                          damini = max(damini,elbuf_tab(ng)%bufly(1)%fail(1,1,k)%floc(1)%damini(i))
                        endif
                      enddo
                      value(i) = damini
                      is_written_value(i) = 1
                    enddo
                endif
!--------------------------------------------------
              elseif (keyword == 'DAMA' .and. ifail > 0) then
!--------------------------------------------------
                if (igtyp == 3) then
                  do i=1,nel
                    value(i) = elbuf_tab(ng)%gbuf%fail(1)%dammx(i)
                    is_written_value(i) = 1
                  enddo
                else if (igtyp == 18) then
                  do i=1,nel
                    dammx  = zero
                    do k = 1,elbuf_tab(ng)%bufly(1)%nptt
                      dammx = max(dammx ,elbuf_tab(ng)%bufly(1)%fail(1,1,k)%floc(1)%dammx(i))
                    enddo
                    value(i) = dammx
                    is_written_value(i) = 1
                  enddo
                endif
!
              endif
!-----------------------------------------------
!       ressorts
!-----------------------------------------------
            elseif(ity==6)then
!--------------------------------------------------
              if (keyword == 'MASS') then
!--------------------------------------------------
                do i=1,nel
                  value(i) = mass(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'ENER')then
!--------------------------------------------------
                if (mlw==1) then
                  xm = one/geo(1,ixr(1,1+nft))
                  do  i=1,nel
!             xm cannot be zero (was checked in starter).
                    value(i) = elbuf_tab(ng)%gbuf%eint(i)*xm
                    is_written_value(i) = 1
                  enddo
                elseif (mlw==2) then
                  xm = one/geo(1,ixr(1,1+nft))
                  do  i=1,nel
!             xm cannot be zero (was checked in starter).
                    value(i) = elbuf_tab(ng)%gbuf%eint(i)*xm
                    is_written_value(i) = 1
                  enddo
                elseif (mlw==3) then
                  xm = one/geo(1,ixr(1,1+nft))
                  do  i=1,nel
!             xm cannot be zero (was checked in starter).
                    value(i) = elbuf_tab(ng)%gbuf%eint(i)*xm
                    is_written_value(i) = 1
                  enddo
                elseif (mlw==4) then
                  xm = one/geo(1,ixr(1,1+nft))
                  do  i=1,nel
!             xm cannot be zero (was checked in starter).
                    value(i) = elbuf_tab(ng)%gbuf%eint(i)*xm
                    is_written_value(i) = 1
                  enddo
                elseif (mlw==5) then
!           user springs.
                  do  i=1,nel
                    value(i) = elbuf_tab(ng)%gbuf%eint(i)/max(em30,elbuf_tab(ng)%gbuf%mass(i))
                    is_written_value(i) = 1
                  enddo
!           spring axi
                elseif (mlw==6) then
                  xm = one/geo(1,ixr(1,1+nft))
                  do  i=1,nel
!             xm cannot be zero (was checked in starter).
                    value(i) = elbuf_tab(ng)%gbuf%eint(i)*xm
                    is_written_value(i) = 1
                  enddo
                elseif (mlw==7) then
                  xm = one/geo(1,ixr(1,1+nft))
                  do  i=1,nel
                    value(i) = elbuf_tab(ng)%gbuf%eint(i)*xm
                    is_written_value(i) = 1
                  enddo
                endif
!--------------------------------------------------
              elseif(keyword == 'DAM1')then
!--------------------------------------------------
                do  i=1,nel
                  value(i) = anim(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'DAM2')then
!--------------------------------------------------
                kk = numelr * anim_fe(11)
                do  i=1,nel
                  value(i) = anim(i+kk)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'DAM3')then
!--------------------------------------------------
                kk = numelr * (anim_fe(11)+anim_fe(12))
                do  i=1,nel
                  value = anim(i+kk)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'DT' .and. elbuf_tab(ng)%gbuf%g_dt/=0)then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%dt(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif (keyword == 'AMS'.and.(elbuf_tab(ng)%gbuf%g_isms>0)) then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%isms(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'OFF')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = elbuf_tab(ng)%gbuf%off(i)
                  is_written_value(i) = 1
                enddo
!--------------------------------------------------
              elseif(keyword == 'GROUP')then
!--------------------------------------------------
                do i=1,nel
                  value(i) = ng
                  is_written_value(i) = 1
                enddo
              endif
!--------------------------------------------------
            elseif(keyword == 'INTERNAL.ID')then
!--------------------------------------------------
              do i=1,nel
                value(i) = i+nft
                is_written_value(i) = 1
              enddo
!--------------------------------------------------
            elseif(keyword == 'LOCAL.ID')then
!--------------------------------------------------
              do i=1,nel
                value(i) = i
                is_written_value(i) = 1
              enddo
            endif

            if (ity == 4 .or. ity == 5 .or. ity == 6)&
            &call h3d_write_scalar(iok_part,is_written_oned,oned_scalar,nel,offset,nft,&
            &value,is_written_value)
          enddo ! do ng=1,ngroup


          return
        end

      end module h3d_oned_scalar_mod
