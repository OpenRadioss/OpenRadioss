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
!Chd|====================================================================
!Chd|  fractal_dmg_init          source/materials/fail/fractal/fractal_dmg_init.F
!Chd|-- called by -----------
!Chd|        c3init3                       source/elements/sh3n/coque3n/c3init3.F
!Chd|        cbainit3                      source/elements/shell/coqueba/cbainit3.F
!Chd|        cbainit3                        source/elements/shell/coque/cbainit3.F
!Chd|-- calls ---------------
!Chd|====================================================================
      module fractal_dmg_init_mod
      contains
! ========================================================================================
! \brief initialize local element buffer variable dammx in shell elements calculated by /fail/fractal_dmg
! \details damage calculated in /fail/fractal_dmg is copied to all failure models referring to the same material

! ========================================================================================

      subroutine fractal_dmg_init(elbuf_str,mat_param,fail_fractal,nummat,nshell,nel,nft,ngl,ity)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use mat_elem_mod    
      use groupdef_mod
      use random_walk_def_mod
      use constant_mod ,only : zero,one
! ---------------------------------------------------------------------------------------------
      implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in)  :: nshell                                         !< total number of shell elements
      integer ,intent(in)  :: nummat                                         !< number material models
      integer ,intent(in)  :: nel                                            !< number shells in element group
      integer ,intent(in)  :: nft                                            !< element group offset
      integer ,intent(in)  :: ity                                            !< property type
      integer ,dimension(nel) ,intent(in)  :: ngl                            !< element Id table
      type (elbuf_struct_)                       ,intent(inout) :: elbuf_str !< element buffer structure
      type (matparam_struct_) ,dimension(nummat) ,intent(inout) :: mat_param !< material parameter structure
      type (fail_fractal_) :: fail_fractal                                   !< fractal model structure
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ii,jj,iel,nlay,ilay,ir,is,it,nptr,npts,nptt,nix
      integer :: nelem_dmg,nlay_dmg
      integer :: imat,mat_fail
      integer :: ifract,ifail,nfail,l_dmx
      integer :: debug
      integer ,dimension(:) ,allocatable :: lay_dmg
      my_real :: dmg
      integer :: tagsh(nshell)
      integer :: elem_dmg(nel)
!=======================================================================
      debug = 0
      if (ity == 3) then
        nix = 4
      else if (ity == 7) then                    
        nix = 3
      end if
!
      do ifract = 1,fail_fractal%nfail
        mat_fail = fail_fractal%fractal(ifract)%imat
        do ifail = 1,mat_param(mat_fail)%nfail
          if (mat_param(mat_fail)%fail(ifail)%irupt == 12) then
            dmg = mat_param(mat_fail)%fail(ifail)%uparam(1)
            debug = mat_param(mat_fail)%fail(ifail)%iparam(7)
          end if
        end do
        nlay = elbuf_str%nlay
        allocate(lay_dmg(nlay))
        lay_dmg(:) = 0
        nlay_dmg  = 0
        do ilay = 1,nlay  
          imat  = elbuf_str%bufly(ilay)%imat
          if (imat == mat_fail) then
            nlay_dmg = nlay_dmg + 1
            lay_dmg(nlay_dmg) = ilay
          end if
        end do
        if (nlay_dmg > 0) then
          tagsh(:)    = 0
          elem_dmg(:) = 0
          nelem_dmg   = 0
          do i=1,fail_fractal%fractal(ifract)%nelem                        
            iel = fail_fractal%fractal(ifract)%random_walk(i)%id
            if (fail_fractal%fractal(ifract)%random_walk(iel)%nix  == nix .and.   &
                fail_fractal%fractal(ifract)%random_walk(iel)%damage > zero) tagsh(iel) = 1             
          end do
          do i=1,nel     
            if (tagsh(i+nft) == 1) then
              nelem_dmg = nelem_dmg + 1
              elem_dmg(nelem_dmg) = i
            end if
          end do
        end if
        !------------------------------------------
        if (debug == 1 .and. nelem_dmg > 0) then
          print*,'  '
          print*,'damaged element list '
          do ii=1,nelem_dmg  
            i = elem_dmg(ii) 
            if (debug == 1) print*,'    damaged elem ',ngl(i)
          end do
        end if
        !------------------------------------------
        do ii = 1,nlay_dmg
          ilay = lay_dmg(ii)
          nfail = elbuf_str%bufly(ilay)%nfail
          nptr  = elbuf_str%nptr
          npts  = elbuf_str%npts
          nptt  = elbuf_str%bufly(ilay)%nptt

          do ifail = 1,nfail
            l_dmx = elbuf_str%bufly(ilay)%fail(1,1,1)%floc(ifail)%lf_dam
            if (l_dmx > 0) then ! dammx variable is allocated and used in local failure model
              do jj = 1,nelem_dmg
                iel = elem_dmg(jj)
                do ir=1,nptr
                  do is=1,npts
                    do it=1,nptt
                      elbuf_str%bufly(ilay)%fail(ir,is,it)%floc(ifail)%dam(iel)   = dmg
                      elbuf_str%bufly(ilay)%fail(ir,is,it)%floc(ifail)%dammx(iel) = dmg
                    end do      !  it=1,nptt
                  end do        !  it=1,npts
                end do          !  it=1,nptr
              end do          !  nelem_dmg
            end if
          end do            !   ifail = 1,nfail
        end do              !  il=1,nlay   
        if (allocated(lay_dmg)) deallocate(lay_dmg)
      end do          !  loop over fractal models
!
      if (debug == 1) print*,' '
!-----------
      return
      end subroutine fractal_dmg_init
      end module fractal_dmg_init_mod
