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
      !||====================================================================
      !||    sdistor_ini_mod   ../engine/source/elements/solid/solide/sdistror_ini.F90
      !||--- called by ------------------------------------------------------
      !||    s10forc3          ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s4forc3           ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s8eforc3          ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    sforc3            ../engine/source/elements/solid/solide/sforc3.F
      !||    szforc3           ../engine/source/elements/solid/solidez/szforc3.F
      !||====================================================================
      module sdistor_ini_mod
      contains
! ======================================================================================================================
! \brief some parameter initialization for solid distortion control
! ======================================================================================================================
      !||====================================================================
      !||    sdistor_ini    ../engine/source/elements/solid/solide/sdistror_ini.F90
      !||--- called by ------------------------------------------------------
      !||    s10forc3       ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s4forc3        ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s8eforc3       ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    sforc3         ../engine/source/elements/solid/solide/sforc3.F
      !||    szforc3        ../engine/source/elements/solid/solidez/szforc3.F
      !||--- calls      -----------------------------------------------------
      !||    scre_sig3      ../engine/source/elements/solid/solide/scre_sig3.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod   ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine sdistor_ini(                                 &
                nel      ,sti_c    ,npropm     ,nummat  ,       &
                ismstr   ,imat     ,istab      ,pm      ,       &
                sig      ,rho      ,cxx        ,off     ,       &
                offg     ,ll       ,vol        ,fld     ,       &
                mu       ,fqmax      )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,          only : one,zero,zep05,onep333,ep03,ep02,em02,third,fourth
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "mvsiz_p.inc"
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in)                              :: nel             !< number of elements
          integer, intent(in)                              :: npropm          !< number of properties
          integer, intent(in)                              :: nummat          !< number of laws
          integer, intent(in)                              :: ismstr          !< small strain falg
          integer, dimension(mvsiz), intent(in   )         :: imat            !< material id
          integer, dimension(mvsiz), intent(inout)         :: istab           !< buckling criterion flag
          my_real, dimension(npropm,nummat) ,intent(in)    :: pm              !< material data
          my_real, dimension(nel,6), intent(in   )         :: sig             !< stress tensor for buckling check
          my_real, dimension(nel),   intent(in   )         :: rho             !< density
          my_real, dimension(mvsiz), intent(in   )         :: cxx             !< speed sound 
          my_real, dimension(mvsiz), intent(in   )         :: off             !< off value 
          my_real, dimension(mvsiz), intent(in   )         :: vol             !< volume 
          my_real, dimension(nel),   intent(in   )         :: offg            !< offg value 
          my_real, dimension(mvsiz), intent(inout)         :: ll              !< charactistic length
          my_real, dimension(mvsiz), intent(inout)         :: fld             !< damping charactistic 
          my_real, dimension(mvsiz), intent(inout)         :: sti_c           !< nodal stiffness
          my_real, intent(inout)                           :: mu              !< damping coefficient
          my_real, intent(inout)                           :: fqmax           !< quadratic stiffness limit for contact
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,j,mx
          my_real :: nu,f_nu,c1,caq,c2
!=======================================================================
!
         mu=zep05
         mx = imat(1)
         nu =pm(21,mx)
         if (nu > 0.48999) then 
             fqmax = ep02
             f_nu = em02
         else 
             fqmax = ep03
             f_nu = one
         end if
         c1 = pm(107,mx)+onep333*pm(22,mx)
         c2 = f_nu*c1
         do i=1,nel
          ll(i) = vol(i)**third
          caq=mu*rho(i)*ll(i)
          fld(i)=fourth*caq*cxx(i)*off(i)
          sti_c(i) = c2 * ll(i) *off(i)
         enddo
         call scre_sig3(sig, c1, istab,offg,ismstr ,nel)
!         
        end subroutine sdistor_ini
!-------------------
      end module sdistor_ini_mod
