!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
! ----------------------------------------------------------------------------------------------------------------------
!
!||====================================================================
!||    extract_table_plas_mod   ../starter/source/materials/tools/extract_table_plas_mod.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat123             ../starter/source/materials/mat/mat123/hm_read_mat123.F90
!||====================================================================
      module extract_table_plas_mod
        implicit none
      contains

!! \brief  make a private copy of input function table to material table stored in mat_param
!! \detail one to one copy of a single input table
!||====================================================================
!||    extract_table_plas      ../starter/source/materials/tools/extract_table_plastic.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat123  ../starter/source/materials/mat/mat163/hm_read_mat123.F90
!||--- uses       -----------------------------------------------------
!||    message_mod      ../starter/share/message_module/message_mod.F
!||====================================================================
        Subroutine extract_table_plas(mat_table, mat_table_plas, e, yld0, npt_plas )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------       
          use table4d_mod
          use names_and_titles_mod , only : nchartitle
          use constant_mod         , only : zero
          use precision_mod        , only : WP
          use MY_ALLOC_MOD, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   included files
! ----------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP)                         ,intent(inout)    :: yld0   !<  initial yld
          real(kind=WP)                         ,intent(in)      :: e      ! initial rigidity
          type(table_4d_)                 ,intent(in) :: mat_table !<  material table structure
          type(table_4d_)                 ,intent(inout) :: mat_table_plas !< target material table structure

          integer                         ,intent(inout)   :: npt_plas      !< number of point in the plastic curve
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER ndim, i,npt,len2,j
      real(kind=WP) ::  x1,y1,epsp
      real(kind=WP) , allocatable, dimension(:) :: xp, yld
!=======================================================================
      ! COMPUTE INITIAL, MIN and MAX SLOPE OF A FUNCTION TABLE
!=======================================================================
        ndim = mat_table%ndim
        npt  = size(mat_table%x(1)%values)
!
        mat_table_plas%notable = mat_table%notable
        mat_table_plas%ndim = 1 ! only  one dim is considered for exactrat plastic curve
        allocate(mat_table_plas%x(1))
        if(ndim == 1) then
           allocate(xp(npt), yld(npt)) 
           yld = zero
           xp = zero
           npt_plas = 0 
           do i = 1,npt
              x1  = mat_table%x(1)%values(i)
              y1  = mat_table%y1d(i)
              epsp = x1 - y1/e
              if(epsp >0) then 
                  npt_plas = npt_plas  + 1
                  xp(npt_plas) = epsp
                  if(npt_plas == 1 .and. epsp /= zero) then
                      xp(npt_plas)=zero
                      yld0 = y1
                  endif
                 yld(npt_plas) = y1
             endif
           enddo
           if(npt_plas >= 2 ) then
               call my_alloc(mat_table_plas%x(1)%values,npt_plas, "mat_table_plas%x(1)%values")
               call my_alloc(mat_table_plas%y1d,npt_plas,"mat_table_plas%y1d")
               mat_table_plas%x(1)%values(1:npt_plas) = xp(1:npt_plas)
               mat_table_plas%y1d(1:npt_plas) = yld(1:npt_plas)
           endif
           deallocate(xp,yld)
         elseif(ndim == 2) then
             len2 = size(mat_table%x(2)%values)
             allocate(xp(npt), yld(npt)) 
             yld = zero
             xp = zero 
             npt_plas = 0
             do i = 1,npt
                  x1 = mat_table%x(1)%values(i)
                 ! with out strain rate effect. we take only the first curve
                    y1 = mat_table%y2d(i,1)
                    epsp = x1 - y1/e 
                    if (epsp > zero) then
                      npt_plas = npt_plas + 1
                      xp(npt_plas) = epsp
                      if(npt_plas == 1 .and. epsp > zero) then
                        xp(npt_plas) = zero 
                        yld0 = y1
                      endif 
                      yld(npt_plas ) = y1
                    endif
            enddo
            if(npt_plas >= 2 ) then
               call my_alloc(mat_table_plas%x(1)%values,npt_plas, "mat_table_plas%x(1)%values")
               call my_alloc(mat_table_plas%y1d,npt_plas,"mat_table_plas%y1d")
               mat_table_plas%x(1)%values(1:npt_plas) = xp(1:npt_plas)
               mat_table_plas%y1d(1:npt_plas) = yld(1:npt_plas) 
           endif
           deallocate(xp,yld)
         endif ! ndim
         if(yld0 == zero .or. npt_plas < 2 ) then
                 mat_table_plas%notable = 0
                 npt_plas = 0 
         endif
!------------------------------
         return
        end subroutine extract_table_plas
      end module extract_table_plas_mod