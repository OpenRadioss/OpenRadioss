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
! ======================================================================================================================
!                                                   MODULE
! ======================================================================================================================
!! \brief Initialization of mass and reference volume for Q1NP enriched solid elements.
!! \details Reconstructs the scalar element volume from per-Gauss-point Q1NP reference
!!          volumes, then computes the lumped mass contribution of the Q1NP enriched
!!          elements that replace standard HEX8 elements.
!||====================================================================
!||    q1np_init_mod            ../starter/source/elements/solid/solid_q1np/q1np_init.F90
!||--- called by ------------------------------------------------------
!||    initia                   ../starter/source/elements/initia/initia.F
!||--- uses       -----------------------------------------------------
!||    q1np_init_lbuf_vol_mod   ../starter/source/elements/solid/solid_q1np/q1np_init_lbuf_vol.F90
!||    q1np_mass3_mod           ../starter/source/elements/solid/solid_q1np/q1np_mass3.F90
!||====================================================================
      module q1np_init_mod
        use q1np_restart_mod
        use q1np_init_lbuf_vol_mod, only : q1np_init_lbuf_gp_vol
        use q1np_mass3_mod, only : q1np_mass3
        use elbufdef_mod
        use my_alloc_mod, only : my_alloc
        use my_dealloc_mod, only : my_dealloc
        use precision_mod, only : WP
        use constant_mod, only : ZERO
        implicit none
      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Compute mass and reference volume of Q1NP enriched solid elements.
!||====================================================================
!||    q1np_init               ../starter/source/elements/solid/solid_q1np/q1np_init.F90
!||--- called by ------------------------------------------------------
!||    initia                  ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    q1np_init_lbuf_gp_vol   ../starter/source/elements/solid/solid_q1np/q1np_init_lbuf_vol.F90
!||    q1np_mass3              ../starter/source/elements/solid/solid_q1np/q1np_mass3.F90
!||====================================================================
        subroutine q1np_init(iparg, elbuf_tab, x, v, ms, mssa, partsav, pm, &
     &                       numnod, numels, npart, npropm, nummat, ngroup, nparg, &
     &                       sfill, irest_mselt)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
          integer,             intent(in)    :: numnod                    !< Number of nodes
          integer,             intent(in)    :: numels                    !< Number of solid elements
          integer,             intent(in)    :: npart                     !< Number of parts
          integer,             intent(in)    :: npropm                    !< First dimension of material property array
          integer,             intent(in)    :: nummat                    !< Number of materials
          integer,             intent(in)    :: ngroup                    !< Number of element groups
          integer,             intent(in)    :: nparg                     !< First dimension of iparg
          integer,             intent(in)    :: sfill                     !< Size of the fill array
          integer,             intent(in)    :: irest_mselt               !< Element mass restart flag
          integer,             intent(in)    :: iparg(nparg,ngroup)       !< Element group parameters
          real(kind=WP),       intent(in)    :: pm(npropm,nummat)         !< Material property array
          type(ELBUF_STRUCT_), target, intent(inout) :: elbuf_tab(ngroup) !< Element buffer per group
          real(kind=WP),       intent(inout) :: x(3,numnod)               !< Nodal coordinates
          real(kind=WP),       intent(inout) :: v(3,numnod)               !< Nodal velocities
          real(kind=WP),       intent(inout) :: ms(numnod)                !< Nodal masses
          real(kind=WP),       intent(inout) :: mssa(numels)              !< Element masses (restart)
          real(kind=WP),       intent(inout) :: partsav(20,npart)         !< Part mass/inertia accumulators
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ng, nel, nft, i, ity, iel
          integer :: iel_hex8, iel_local, iel_q1np
          integer :: nptr_q1np, npts_q1np, nptt_q1np, iu_q1np, iv_q1np, it_q1np
          real(kind=WP) :: vol_sum_q1np
          real(kind=WP), allocatable :: rho_hex8(:), fill_hex8(:)
          type(G_BUFEL_), pointer :: gbuf
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
!          
          ! Initialize per-Gauss-point Q1NP reference volumes in ELBUF/LBUF
          call q1np_init_lbuf_gp_vol(iparg, elbuf_tab, x, numnod, numels,        &
                                    kq1np_tab, iq1np_tab, iq1np_bulk_tab)

          ! Reconstruct scalar GBUF%VOL from per-Gauss Q1NP volumes.
          if (associated(q1np_ktab_g) .and. (q1np_nknot_sets_g > 0 .or.          &
              (q1np_nx_g > 0 .and. q1np_ny_g > 0))) then
            do iel_q1np = 1, numelq1np_g
              iel_hex8 = kq1np_tab(10, iel_q1np)
              if (iel_hex8 <= 0 .or. iel_hex8 > numels) cycle

              do ng = 1, ngroup
                if (iparg(5,ng) /= 1) cycle
                nel = iparg(2,ng)
                nft = iparg(3,ng)
                if (nel <= 0) cycle
                if (iel_hex8 < nft+1 .or. iel_hex8 > nft+nel) cycle

                iel_local = iel_hex8 - nft
                gbuf => elbuf_tab(ng)%gbuf

                nptr_q1np = iparg(56,ng)
                npts_q1np = iparg(57,ng)
                nptt_q1np = iparg(58,ng)

                vol_sum_q1np = ZERO
                do it_q1np = 1, nptt_q1np
                  do iu_q1np = 1, nptr_q1np
                    do iv_q1np = 1, npts_q1np
                      vol_sum_q1np = vol_sum_q1np + elbuf_tab(ng)%bufly(1)%lbuf(    &
                       iu_q1np, iv_q1np, it_q1np)%vol(iel_local)
                    end do
                  end do
                end do

                gbuf%vol(iel_local) = vol_sum_q1np
                exit
              end do
            end do
          end if

          ! Allocate temporary arrays for Q1Np mass calculation
          call my_alloc(rho_hex8, numels, "RHO_HEX8")
          call my_alloc(fill_hex8, numels, "FILL_HEX8")

          ! 1.Step: Initialize arrays to zero
          rho_hex8(1:numels) = ZERO
          fill_hex8(1:numels) = ZERO

          ! 2.Step: Extract RHO and FILL from GBUF for all HEX8 elements
          do ng = 1, ngroup
            ity = iparg(5,ng)
            if (ity == 1) then
              nel = iparg(2,ng)
              nft = iparg(3,ng)
              gbuf => elbuf_tab(ng)%gbuf
              if (associated(gbuf%rho)) then
                do i = 1, nel
                  iel = nft + i
                  if (iel <= numels) rho_hex8(iel) = gbuf%rho(i)
                end do
              end if
              if (associated(gbuf%fill)) then
                do i = 1, nel
                  iel = nft + i
                  if (iel <= numels) fill_hex8(iel) = gbuf%fill(i)
                end do
              end if
            end if
          end do

          ! 3.Step: Call Q1Np mass calculation routine
          call q1np_mass3(rho_hex8, ms, mssa, partsav, x, v,                                   &
                          fill_hex8, iparg, elbuf_tab, kq1np_tab, iq1np_tab, iq1np_bulk_tab,   &
                          numelq1np_g, npropm, nummat, pm,                                     &
                          numels, numnod, npart, q1np_ktab_g, sfill, irest_mselt)

          ! 4.Step: Deallocate temporary arrays
          call my_dealloc(rho_hex8)
          call my_dealloc(fill_hex8)

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine q1np_init

      end module q1np_init_mod
