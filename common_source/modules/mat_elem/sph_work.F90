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
!||    sph_work_mod     ../common_source/modules/mat_elem/sph_work.F90
!||--- called by ------------------------------------------------------
!||    forintp          ../engine/source/elements/forintp.F
!||    resol            ../engine/source/engine/resol.F
!||    sph_crit_voxel   ../engine/source/elements/sph/sph_crit_voxel.F90
!||    sphprep          ../engine/source/elements/sph/sphprep.F
!||    splissv          ../engine/source/elements/sph/splissv.F
!||    sponfv           ../engine/source/elements/sph/sponfv.F
!||--- uses       -----------------------------------------------------
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||====================================================================
      module sph_work_mod
        use precision_mod, only : WP
!=======================================================================================
!! \brief  module to define type for buffers use in SPHPREP and SPHINT
!! \details

      implicit none

        type sph_work_voxel_
          integer, dimension(:,:,:), allocatable :: nnod         ! sphprep
          real(kind=WP), dimension(:,:,:), allocatable :: dxmin        ! sphprep
          real(kind=WP), dimension(:,:,:), allocatable :: dymin        ! sphprep
          real(kind=WP), dimension(:,:,:), allocatable :: dzmin        ! sphprep
          real(kind=WP), dimension(:,:,:), allocatable :: dxmax        ! sphprep
          real(kind=WP), dimension(:,:,:), allocatable :: dymax        ! sphprep
          real(kind=WP), dimension(:,:,:), allocatable :: dzmax        ! sphprep
        end type sph_work_voxel_

        type sph_work_
          integer, dimension(:), allocatable :: wreduce            ! sphprep
          integer :: voxel_nb                                      ! sphprep
          type(sph_work_voxel_) :: voxel                           ! sphprep
          !
          integer, dimension(:), allocatable ::  itag              ! splissv
          double precision, dimension(:,:,:), allocatable :: as6   ! splissv
          double precision, dimension(:,:,:), allocatable ::  a6   ! splissv
          real(kind=WP), dimension(:,:), allocatable :: as               ! splissv
          real(kind=WP), dimension(:,:), allocatable :: asphr            ! splissv
          !
          real(kind=WP), dimension(:), allocatable :: wt                 ! forintp
          real(kind=WP), dimension(:), allocatable :: wgradt             ! forintp
          real(kind=WP), dimension(:), allocatable :: wlaplt             ! forintp
          real(kind=WP), dimension(:), allocatable :: lambda             ! forintp
          real(kind=WP), dimension(:), allocatable :: wgradtsm           ! forintp
          real(kind=WP), dimension(:), allocatable :: wtr                ! forintp
          real(kind=WP), dimension(:), allocatable :: lambdr             ! forintp
          real(kind=WP), dimension(:), allocatable :: wasigsm            ! forintp
          real(kind=WP), dimension(:,:), allocatable :: war              ! forintp
          real(kind=WP), dimension(:,:), allocatable :: wgr              ! forintp
          real(kind=WP), dimension(:,:), allocatable :: war2             ! forintp
          real(kind=WP), dimension(:,:), allocatable :: stab             ! forintp
        end type sph_work_

      contains
! ======================================================================================================================
!                                                   init_sph_work
! ======================================================================================================================

!||====================================================================
!||    allocate_sph_work   ../common_source/modules/mat_elem/sph_work.F90
!||--- called by ------------------------------------------------------
!||    resol               ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod        ../common_source/tools/memory/my_alloc.F90
!||====================================================================
        subroutine allocate_sph_work(sph_work,                                &
        &                              numsph,size_wreduce,                   &
        &                              flag_sol_to_sph, size_itag,            &
        &                              size_as6, size_a6, size_as,            &
        &                              numsphg)
!=======================================================================================
!! \brief  subroutine to allocate the buffers used in SPHPREP and SPHINT
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   g l o b a l   p a r a m e t e r s
!-----------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: size_wreduce
          integer,intent(in) :: numsph
          integer,intent(in) :: numsphg
          integer,intent(in) :: flag_sol_to_sph
          integer,intent(in) :: size_itag
          integer,intent(in) :: size_as6
          integer,intent(in) :: size_a6
          integer,intent(in) :: size_as
          type(sph_work_) :: sph_work
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nbk
! ---------------------------------------------------------------------------------------------------------------------
!
          if (numsph > 0) call my_alloc(sph_work%wreduce,size_wreduce)
          if (flag_sol_to_sph > 0) then
            call my_alloc(sph_work%itag,size_itag)
            call my_alloc(sph_work%a6,6,3,size_a6)
            call my_alloc(sph_work%as,3,8*size_as)
            call my_alloc(sph_work%as6,6,3,8*size_as6)
          end if
          if (numsphg > 0) then
            sph_work%voxel_nb = 15
            nbk = sph_work%voxel_nb
            call my_alloc(sph_work%voxel%nnod,nbk,nbk,nbk)
            call my_alloc(sph_work%voxel%dxmin,nbk,nbk,nbk)
            call my_alloc(sph_work%voxel%dymin,nbk,nbk,nbk)
            call my_alloc(sph_work%voxel%dzmin,nbk,nbk,nbk)
            call my_alloc(sph_work%voxel%dxmax,nbk,nbk,nbk)
            call my_alloc(sph_work%voxel%dymax,nbk,nbk,nbk)
            call my_alloc(sph_work%voxel%dzmax,nbk,nbk,nbk)
          end if

        end subroutine allocate_sph_work

      end module sph_work_mod



