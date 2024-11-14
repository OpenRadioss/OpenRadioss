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
      !||    dttherm_mod   ../engine/source/time_step/dttherm.F90
      !||--- called by ------------------------------------------------------
      !||    c3forc3       ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    cbaforc3      ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cdk6forc3     ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3      ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3        ../engine/source/elements/shell/coque/cforc3.F
      !||    czforc3       ../engine/source/elements/shell/coquez/czforc3.F
      !||====================================================================
      module dttherm_mod
      contains
! ======================================================================================================================
!                           dttherm
!! \brief calculates thermal time step
!! \details 
! ======================================================================================================================
      !||====================================================================
      !||    dttherm          ../engine/source/time_step/dttherm.F90
      !||--- called by ------------------------------------------------------
      !||    c3forc3          ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    cbaforc3         ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cdk6forc3        ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3         ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3           ../engine/source/elements/shell/coque/cforc3.F
      !||    czforc3          ../engine/source/elements/shell/coquez/czforc3.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod     ../common_source/modules/constant_mod.F
      !||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||====================================================================
      subroutine dttherm(nel     ,pm      ,npropm  ,glob_therm,            &
                         jtur    ,tempel  ,vol0    ,rho       ,            &
                         lc      ,off     ,conde   ,re        ,rk      )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use glob_therm_mod
          use constant_mod  , only : one, half, four,em15,em20
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none

#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer ,intent(in)        :: nel
      integer ,intent(in)        :: npropm
      integer ,intent(in)        :: jtur
      my_real ,dimension(nel)    :: tempel
      my_real ,dimension(nel)    :: lc
      my_real ,dimension(nel)    :: off
      my_real ,dimension(nel)    :: vol0
      my_real ,dimension(nel)    :: rho
      my_real ,dimension(nel)    :: rk
      my_real ,dimension(nel)    :: re
      my_real ,dimension(nel)    :: conde
      my_real ,dimension(npropm) :: pm
      type (glob_therm_) ,intent(inout) :: glob_therm
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i
      my_real :: rhocp,as,bs,al,bl,tmelt,akk,xmu,tmu,atu,dt,lc2,rpr
!=======================================================================
      rhocp = pm(69)
      as    = pm(75)
      bs    = pm(76)
      al    = pm(77)
      bl    = pm(78)
      tmelt = pm(80)
!
      do i=1,nel
        if (tempel(i) < tmelt) then
          akk = as + bs*tempel(i)
        else
          akk = al + bl*tempel(i)
        endif
        if (jtur /= 0) then
          xmu = pm(24)*rho(i)
          tmu = pm(81)
          rpr = pm(95)
          atu = rpr*tmu*rk(i)*rk(i) / (max(em15,re(I)*vol0(I))*xmu)
          akk = akk*(one+atu)
        endif
        akk = akk*glob_therm%theaccfact
        lc2 = lc(i)*lc(i)
        dt  = glob_therm%dtfactherm * half*lc2*rhocp/max(akk,em20)
        if (dt < glob_therm%dt_therm)  glob_therm%dt_therm = dt
        conde(i) = four*vol0(i)*akk/lc2  
        conde(i) = conde(i)*off(i)
      enddo
!-----------
      return
      end
!-----------      
      end module dttherm_mod
