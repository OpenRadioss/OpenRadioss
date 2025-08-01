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
!||    sdistor_ini_mod   ../engine/source/elements/solid/solide/sdistror_ini.F90
!||--- called by ------------------------------------------------------
!||    s10forc3          ../engine/source/elements/solid/solide10/s10forc3.F
!||    s4forc3           ../engine/source/elements/solid/solide4/s4forc3.F
!||    s6cforc3          ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||    s8cforc3          ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    s8eforc3          ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    scforc3           ../engine/source/elements/thickshell/solidec/scforc3.F
!||    sforc3            ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3           ../engine/source/elements/solid/solidez/szforc3.F
!||====================================================================
      module sdistor_ini_mod
      contains
! ======================================================================================================================
! \brief some parameter initialization for solid distortion control
! ======================================================================================================================
!||====================================================================
!||    sdistor_ini     ../engine/source/elements/solid/solide/sdistror_ini.F90
!||--- called by ------------------------------------------------------
!||    s10forc3        ../engine/source/elements/solid/solide10/s10forc3.F
!||    s4forc3         ../engine/source/elements/solid/solide4/s4forc3.F
!||    s6cforc3        ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||    s8cforc3        ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    s8eforc3        ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    scforc3         ../engine/source/elements/thickshell/solidec/scforc3.F
!||    sforc3          ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3         ../engine/source/elements/solid/solidez/szforc3.F
!||--- calls      -----------------------------------------------------
!||    scre_sig3       ../engine/source/elements/solid/solide/scre_sig3.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    mvsiz_mod       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
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
          use constant_mod, only : one,zero,two,zep05,onep333,em03,ep02,em02,third,fourth,hundred80,ten,three,half,em20
          use precision_mod, only : WP
          use mvsiz_mod , only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in)                              :: nel             !< number of elements
          integer, intent(in)                              :: npropm          !< number of properties
          integer, intent(in)                              :: nummat          !< number of laws
          integer, intent(in)                              :: ismstr          !< small strain flag
          integer, dimension(mvsiz), intent(in   )         :: imat            !< material id
          integer, dimension(mvsiz), intent(inout)         :: istab           !< buckling criterion flag
          real(kind=WP), dimension(npropm,nummat) ,intent(in)    :: pm              !< material data
          real(kind=WP), dimension(nel,6), intent(in   )         :: sig             !< stress tensor for buckling check
          real(kind=WP), dimension(nel),   intent(in   )         :: rho             !< density
          real(kind=WP), dimension(mvsiz), intent(in   )         :: cxx             !< speed sound 
          real(kind=WP), dimension(mvsiz), intent(in   )         :: off             !< off value 
          real(kind=WP), dimension(mvsiz), intent(in   )         :: vol             !< volume 
          real(kind=WP), dimension(nel),   intent(in   )         :: offg            !< offg value 
          real(kind=WP), dimension(mvsiz), intent(inout)         :: ll              !< charactistic length
          real(kind=WP), dimension(mvsiz), intent(inout)         :: fld             !< damping charactistic 
          real(kind=WP), dimension(mvsiz), intent(inout)         :: sti_c           !< nodal stiffness
          real(kind=WP), intent(inout)                           :: mu              !< damping coefficient
          real(kind=WP), intent(inout)                           :: fqmax           !< quadratic stiffness limit for contact
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer :: i,mx
          real(kind=WP) :: nu,f_nu,c1,caq,c2,es,f_es,aj2,f_min
!=======================================================================
!
         mu=zep05
         mx = imat(1)
         nu =pm(21,mx)
         fqmax = ep02
         if (nu > 0.48999) then 
             f_nu = em02
         elseif (nu>0.4) then
             f_nu = one-two*nu
             mu=f_nu*mu
         else
             f_nu = one
             if (pm(107,mx) >= hundred80*pm(32,mx)) f_nu = ten 
         end if
         c1 = max(pm(32,mx),pm(100,mx))+onep333*pm(22,mx)
         c2 = f_nu*c1
         f_min = em03
         do i=1,nel
          aj2=half*(sig(i,1)**2+sig(i,2)**2+sig(i,3)**2)          &
                  + sig(i,4)**2+sig(i,5)**2+sig(i,6)**2
          es = sqrt(three*aj2)/c1
          f_es = max(f_min,ep02*es)
          f_es = min(one,f_es)
          ll(i) = vol(i)**third
          caq=f_es*mu*rho(i)*ll(i)
          fld(i)=fourth*caq*cxx(i)*off(i)
          sti_c(i) = c2 * ll(i) *off(i)
         enddo
         call scre_sig3(sig, c1, istab,offg,ismstr ,nel)
!         
        end subroutine sdistor_ini
!-------------------
      end module sdistor_ini_mod
