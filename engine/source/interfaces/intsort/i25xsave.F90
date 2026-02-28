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
!||    i25xsave_mod   ../engine/source/interfaces/intsort/i25xsave_mod.F90
!||--- called by ------------------------------------------------------
!||    i25main_tri   ../engine/source/interfaces/intsort/i25main_tri.F
!||====================================================================
        module i25xsave_mod
            implicit none
           contains
!! \brief computes variables for sorting criteria, xsav, xmin
!||====================================================================
!||    inter7_penetration   ../engine/source/interfaces/intsort/inter7_penetration.F90
!||--- called by ------------------------------------------------------
!||    i25main_tri   ../engine/source/interfaces/intsort/i25main_tri.F
!||--- uses       -----------------------------------------------------
!||    collision_mod        ../engine/source/interfaces/intsort/collision_mod.F
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================

        subroutine i25xsave( x, nsv, msr, nsn, nmn, &
                    itask, xsav, xmin, ymin, zmin, &
                    xmax, ymax, zmax, c_max, curv_max, &
                    icurv, irect, nrtm_t, &
                    sx, sy, sz, sx2, sy2, sz2, &
                    nmn_l, nmn_1d, msr_1d ,nrtm,numnod,inconv,&
                    nthread)


! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          USE PRECISION_MOD, ONLY : WP
          USE CONSTANT_MOD
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------

          integer, intent(in)  :: nsn, nmn, itask, icurv, nrtm_t
          integer, intent(in)  :: nmn_1d
          integer, intent(in)  :: numnod,nrtm,inconv,nthread
          integer, intent(in)  :: nsv(nsn), msr(nmn)
          integer, intent(in)  :: msr_1d(nmn_1d)
          integer, intent(in)  :: irect(4, nrtm)
          real(kind=WP),  intent(in)    :: x(3, numnod)
          real(kind=WP), intent(inout) :: xsav(3, min(numnod,nsn+nmn+nmn_1d) )
          real(kind=WP), intent(out)   :: xmin, ymin, zmin
          real(kind=WP), intent(out)   :: xmax, ymax, zmax
          real(kind=WP), intent(out)   :: c_max
          real(kind=WP), intent(out)   :: sx, sy, sz
          real(kind=WP), intent(out)   :: sx2, sy2, sz2
          real(kind=WP), intent(out)   :: curv_max(nrtm_t)
          integer, intent(out)         :: nmn_l

!-------------------------
! local variables
!-------------------------

          integer :: nsnf, nsnl
          integer :: nmnf, nmnl
          integer :: nmn_1df, nmn_1dl
          integer :: i, j, ii
          integer :: inconv_l
          real(kind=WP):: xxx, yyy, zzz

!-------------------------
! thread partition
!-------------------------

          nsnf    = 1 + itask*nsn / nthread
          nsnl    =     (itask+1)*nsn / nthread
          nmnf    = 1 + itask*nmn / nthread
          nmnl    =     (itask+1)*nmn / nthread
          nmn_1df = 1 + itask*nmn_1d / nthread
          nmn_1dl =     (itask+1)*nmn_1d / nthread

!-------------------------
! initialization
!-------------------------

          xmin =  ep30
          ymin =  ep30
          zmin =  ep30
          xmax = -ep30
          ymax = -ep30
          zmax = -ep30

          sx  = zero
          sy  = zero
          sz  = zero
          sx2 = zero
          sy2 = zero
          sz2 = zero

          nmn_l   = 0
          inconv_l = inconv

!===========================================================
! main loops
!===========================================================

          if (nsn + nmn + nmn_1d < numnod ) then

            if (inconv_l == 1) then
               do i = nsnf, nsnl
                  j = nsv(i)
                  if (j > 0 .and. j <= numnod ) then
                    xsav(1:3, i) = x(1:3, j)
                  end if
               end do
            end if

            do i = nmnf, nmnl
               ii = i + nsn
               j  = msr(i)

               if (j > 0) then

                 xmin = min(xmin, x(1, j))
                 ymin = min(ymin, x(2, j))
                 zmin = min(zmin, x(3, j))

                 xmax = max(xmax, x(1, j))
                 ymax = max(ymax, x(2, j))
                 zmax = max(zmax, x(3, j))

                 if (inconv_l == 1) then
                    xsav(1:3, ii) = x(1:3, j)
                 end if
   
                 sx  = sx  + x(1, j)
                 sy  = sy  + x(2, j)
                 sz  = sz  + x(3, j)

                 sx2 = sx2 + x(1, j)**2
                 sy2 = sy2 + x(2, j)**2
                 sz2 = sz2 + x(3, j)**2

                 nmn_l = nmn_l + 1
               end if
            end do

            do i = nmn_1df, nmn_1dl
               ii = i + nsn + nmn
               j  = msr_1d(i)

               if (j > 0) then

                  xmin = min(xmin, x(1, j))
                  ymin = min(ymin, x(2, j))
                  zmin = min(zmin, x(3, j))

                  xmax = max(xmax, x(1, j))
                  ymax = max(ymax, x(2, j))
                  zmax = max(zmax, x(3, j))

                  if (inconv_l == 1) then
                      xsav(1:3, ii) = x(1:3, j)
                  end if

                  sx  = sx  + x(1, j)
                  sy  = sy  + x(2, j)
                  sz  = sz  + x(3, j)

                  sx2 = sx2 + x(1, j)**2
                  sy2 = sy2 + x(2, j)**2
                  sz2 = sz2 + x(3, j)**2
               end if
            end do

          else

            if (inconv_l == 1) then
              do i = nsnf, nsnl
                j = nsv(i)
                if (j > 0 .and. j <= numnod ) then
                   xsav(1:3, j) = x(1:3, j)
                end if
              end do
            end if

            do i = nmnf, nmnl
              j = msr(i)

              if (j > 0) then

                xmin = min(xmin, x(1, j))
                ymin = min(ymin, x(2, j))
                zmin = min(zmin, x(3, j))

                xmax = max(xmax, x(1, j))
                ymax = max(ymax, x(2, j))
                zmax = max(zmax, x(3, j))

                if (inconv_l == 1) then
                    xsav(1:3, j) = x(1:3, j)
                end if

                sx  = sx  + x(1, j)
                sy  = sy  + x(2, j)
                sz  = sz  + x(3, j)

                sx2 = sx2 + x(1, j)**2
                sy2 = sy2 + x(2, j)**2
                sz2 = sz2 + x(3, j)**2

                nmn_l = nmn_l + 1
              end if
            end do

            do i = nmn_1df, nmn_1dl
              j = msr_1d(i)

              if (j > 0) then

                xmin = min(xmin, x(1, j))
                ymin = min(ymin, x(2, j))
                zmin = min(zmin, x(3, j))

                xmax = max(xmax, x(1, j))
                ymax = max(ymax, x(2, j))
                zmax = max(zmax, x(3, j))

                if (inconv_l == 1) then
                    xsav(1:3, j) = x(1:3, j)
                end if

                sx  = sx  + x(1, j)
                sy  = sy  + x(2, j)
                sz  = sz  + x(3, j)

                sx2 = sx2 + x(1, j)**2
                sy2 = sy2 + x(2, j)**2
                sz2 = sz2 + x(3, j)**2
              end if
            end do

          end if

!===========================================================
! curvature computation
!===========================================================

          c_max = zero

          if (icurv /= 0) then
            do i=1,nrtm_t
              xxx=max(x(1,irect(1,i)),x(1,irect(2,i)),     &
                  x(1,irect(3,i)),x(1,irect(4,i)))         &
                  -min(x(1,irect(1,i)),x(1,irect(2,i)),    &
                  x(1,irect(3,i)),x(1,irect(4,i)))
             yyy=max(x(2,irect(1,i)),x(2,irect(2,i)),      &
                  x(2,irect(3,i)),x(2,irect(4,i)))         &
                  -min(x(2,irect(1,i)),x(2,irect(2,i)),    &
                  x(2,irect(3,i)),x(2,irect(4,i)))
             zzz=max(x(3,irect(1,i)),x(3,irect(2,i)),      &
                  x(3,irect(3,i)),x(3,irect(4,i)))         &
                  -min(x(3,irect(1,i)),x(3,irect(2,i)),    &
                  x(3,irect(3,i)),x(3,irect(4,i)))
             curv_max(i) = half * max(xxx,yyy,zzz)
             c_max = max(c_max,curv_max(i))
            enddo
          else
                curv_max = zero
          end if

          return

        end subroutine i25xsave
      end module i25xsave_mod

