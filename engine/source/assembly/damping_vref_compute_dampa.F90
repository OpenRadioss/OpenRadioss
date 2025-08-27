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
!||    damping_vref_compute_dampa_mod   ../engine/source/assembly/damping_vref_compute_dampa.F90
!||--- called by ------------------------------------------------------
!||    damping51                        ../engine/source/assembly/damping.F
!||    damping_vref_rby                 ../engine/source/assembly/damping_vref_rby.F90
!||    dtnodarayl                       ../engine/source/time_step/dtnodarayl.F
!||    resol                            ../engine/source/engine/resol.F
!||====================================================================
      module damping_vref_compute_dampa_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes damping paramters for /DAMP/VREL
!=======================================================================================================================
!
!||====================================================================
!||    damping_vref_compute_dampa   ../engine/source/assembly/damping_vref_compute_dampa.F90
!||--- called by ------------------------------------------------------
!||    damping51                    ../engine/source/assembly/damping.F
!||    damping_vref_rby             ../engine/source/assembly/damping_vref_rby.F90
!||    dtnodarayl                   ../engine/source/time_step/dtnodarayl.F
!||    resol                        ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    get_u_func                   ../engine/source/user_interface/ufunc.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                 ../common_source/modules/constant_mod.F
!||    precision_mod                ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine damping_vref_compute_dampa(id,ndamp,nrdamp,dampr,dt1,tt,damp_a)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: pi,one,zero,two,half,em20
          use precision_mod , only: WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in)    :: id                          !< damping id
          integer,                                   intent(in)    :: ndamp                       !< number of /DAMP
          integer,                                   intent(in)    :: nrdamp                      !< first dimension of array DAMP
          real(kind=WP),                             intent(inout) :: dampr(nrdamp,ndamp)         !< main structure for option /DAMP
          real(kind=WP),                             intent(in)    :: dt1                         !< time step
          real(kind=WP),                             intent(in)    :: tt                          !< current time
          real(kind=WP),                             intent(inout) :: damp_a(3)                   !< damping coefficients
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: id_func
          real(kind=WP) :: freq,fact,get_u_func,dxdy,dtini
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          external get_u_func
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
!
          id_func = nint(dampr(26,id))
          freq = dampr(28,id)
          dtini = dampr(29,id)
!
          if (id_func > 0) THEN
            fact = get_u_func(id_func,tt,dxdy)
          else
            fact = one
          end if
!
          if (dt1 > zero) then
            if (freq > zero) then
              damp_a(1)  = fact*dampr(3,id)*4*pi*freq
              damp_a(2)  = fact*dampr(5,id)*4*pi*freq
              damp_a(3)  = fact*dampr(7,id)*4*pi*freq
            else
              if (dtini == zero) then
!               Initial time step is saved
                dtini = dt1
                dampr(29,id) = dt1
              end if
              damp_a(1)  = fact*dampr(3,id)*(one/dtini)
              damp_a(2)  = fact*dampr(5,id)*(one/dtini)
              damp_a(3)  = fact*dampr(7,id)*(one/dtini)
            end if
          else
            damp_a(1)  = zero
            damp_a(2)  = zero
            damp_a(3)  = zero
          end if
!
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_vref_compute_dampa
      end module damping_vref_compute_dampa_mod
