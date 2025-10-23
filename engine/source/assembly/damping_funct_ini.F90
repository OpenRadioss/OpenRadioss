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
!||    damping_funct_ini_mod   ../engine/source/assembly/damping_funct_ini.F90
!||--- called by ------------------------------------------------------
!||    resol                   ../engine/source/engine/resol.F
!||====================================================================
      module damping_funct_ini_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine initializes damping alpha values for /DAMP/FUNCT
!=======================================================================================================================
!||====================================================================
!||    damping_funct_ini   ../engine/source/assembly/damping_funct_ini.F90
!||--- called by ------------------------------------------------------
!||    resol               ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    get_u_func          ../engine/source/user_interface/ufunc.F
!||--- uses       -----------------------------------------------------
!||    constant_mod        ../common_source/modules/constant_mod.F
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine damping_funct_ini(dampr, nrdamp, ndamp,   tt,  iroddl)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod , only: WP
          use constant_mod , only: one
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: ndamp                       !< number of /DAMP
          integer,                                   intent(in) :: nrdamp                      !< first dimension of array DAMP
          integer,                                   intent(in) :: iroddl                      !< rotational degrees of freedom flag
          real(kind=WP),                             intent(in) :: tt                          !< time
          real(kind=WP),                          intent(inout) :: dampr(nrdamp,ndamp)         !< data array of /DAMP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nd,itype,id_func
          real(kind=WP) :: alpha,fact,get_u_func,dxdy
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          external get_u_func
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

!
          do nd=1,ndamp
!
            itype = nint(dampr(21,nd))
            if (itype==4) then
              id_func = nint(dampr(26,nd))
              if (id_func > 0) then
                fact = get_u_func(id_func,tt,dxdy)
              else
                fact = one
              end if
              alpha = fact*dampr(16,nd)
              dampr(3,nd) = alpha*dampr(32,nd)
              dampr(5,nd) = alpha*dampr(33,nd)
              dampr(7,nd) = alpha*dampr(34,nd)
              if(iroddl/=0)then
                dampr(9,nd)  = alpha*dampr(35,nd)
                dampr(11,nd) = alpha*dampr(36,nd)
                dampr(13,nd) = alpha*dampr(37,nd)
              end if
            end if !(itype==4)
!
          end do
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_funct_ini
      end module damping_funct_ini_mod
