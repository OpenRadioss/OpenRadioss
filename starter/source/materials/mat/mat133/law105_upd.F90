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
!||    law105_upd_mod   ../starter/source/materials/mat/mat105/law105_upd.F90
!||--- called by ------------------------------------------------------
!||    updmat           ../starter/source/materials/updmat.F
!||====================================================================
      module law105_upd_mod
        implicit none
      contains
!! \brief check & update of material law 105
!||====================================================================
!||    law105_upd              ../starter/source/materials/mat/mat105/law105_upd.F90
!||--- called by ------------------------------------------------------
!||    updmat                  ../starter/source/materials/updmat.F
!||====================================================================
        subroutine law105_upd(  matparam , pm , npropm, mat_uid , nfunc, ifunc, npc   ,pld, fac_shear   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : one, two, zero, three
          use matparam_def_mod
          use precision_mod, only : WP
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(matparam_struct_), target :: matparam
          integer, intent(in) :: npropm
          integer, intent(in) :: mat_uid
          real(kind=wp), dimension(npropm), intent(inout) :: pm
          integer ,intent(in) :: nfunc
          integer, intent(in) :: npc(*)
          real(kind=wp), intent(in) :: pld(*)
          integer, intent(in), dimension(nfunc):: ifunc
          real(kind=wp),intent(in) :: fac_shear
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ieos
          real(kind=WP) :: shear_max, young_max, nu, bulk_max, rho_tmd, rho0
          real(kind=WP) :: mu_max
          real(kind=WP) :: slope
          logical :: is_compaction
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External
! ----------------------------------------------------------------------------------------------------------------------
          integer,external :: FINTER
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          rho0 = matparam%rho0
          rho_tmd = rho0
          ieos = matparam%ieos
          is_compaction = .false.
          if(ieos == 13)then!      /EOS/COMPACTION
            mu_max = matparam%eos%uparam(1)
            rho_tmd = rho0*(mu_max + one)
            is_compaction = .true.
          else if(ieos == 20)then ! /EOS/COMPACTION2
            mu_max = matparam%eos%uparam(4)
            rho_tmd = rho0*(mu_max + one)
            is_compaction = .true.
          else if(ieos == 21)then ! /EOS/COMPACTION_TAB
            rho_tmd = matparam%eos%uparam(1)
            is_compaction = .true.
          else if(ieos == 18)then ! /EOS/LINEAR
            is_compaction = .false.
          else
              call ancmsg(msgid=67, msgtype=msgerror, anmode=aninfo, &
                i1=mat_uid, &
                c1='/MAT/LAW105 (POWDER-BURN)', &
                c2='INCOMPATIBLE EOS: MUST BE LINEAR, COMPACTION, COMPACTION2 OR COMPACTION_TAB')
          end if

          !< Compute the maximum tshear modulus
          !  if famility Eos pf type 'compaction' is used then evaluate G(rho_tmd), otherwise use Gmax
          shear_max = zero
          if(is_compaction)then
            ! G(rho_tmd)
             shear_max = FINTER( IFUNC(3), rho_tmd, NPC, PLD, slope)
          else
            !max(G)
            CALL FUNC_MAXY(IFUNC(3),FAC_SHEAR,NPC,PLD,SHEAR_MAX)
          end if

          ! value at rho0

          nu = matparam%nu
          young_max =  two*shear_max*(one+nu)
          bulk_max = young_max / (three*(one-two*nu))

          !< Update material parameters (if max are needed instead of initial values)
          !----------------------------
          matparam%shear = shear_max
          matparam%young = young_max
          matparam%bulk = bulk_max
          pm(20) = young_max
          pm(24) = young_max
          if(pm(32) == zero) pm(32) = bulk_max ! if no EoS defined

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine law105_upd
! ----------------------------------------------------------------------------------------------------------------------
      end module law105_upd_mod

