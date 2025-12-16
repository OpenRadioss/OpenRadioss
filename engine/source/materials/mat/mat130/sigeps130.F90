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
!||    sigeps130_mod   ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- called by ------------------------------------------------------
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||====================================================================
      module sigeps130_mod
      implicit none
      contains
        ! ======================================================================================================================
        ! \brief Modified honeycomb material law /MAT/LAW130
        ! \details Modified honeycomb material law 
        ! ======================================================================================================================
!||====================================================================
!||    sigeps130               ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- called by ------------------------------------------------------
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    fix_principal_dirs      ../engine/source/materials/mat/mat130/sigeps130.F90
!||    order3                  ../engine/source/materials/mat/mat130/sigeps130.F90
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||    valpvec_v               ../engine/source/materials/mat/mat033/sigeps33.F
!||    valpvecdp_v             ../engine/source/materials/mat/mat033/sigeps33.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod               ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
        subroutine sigeps130(                                                  &
          nel      ,nuvar    ,uvar     ,matparam ,et       ,time     ,         &    
          epsd     ,sigy     ,ssp      ,nvartmp  ,vartmp   ,ngl      ,         &
          depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   ,         &
          epsxx    ,epsyy    ,epszz    ,epsxy    ,epsyz    ,epszx    ,         &
          epspxx   ,epspyy   ,epspzz   ,epspxy   ,epspyz   ,epspzx   ,         &
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   ,         &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          sigvxx   ,sigvyy   ,sigvzz   ,sigvxy   ,sigvyz   ,sigvzx   ,         &
          rho0     ,rho      ,iresp    ,off      ,timestep ,deltax   ,         &
          asrate   ,l_dmg    ,dmg      )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
          use table_mat_vinterp_mod, only : table_mat_vinterp
          use precision_mod, only : WP
          use mvsiz_mod, only : mvsiz
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
#include  "units_c.inc"
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                             intent(in)     :: nel       !< Number of elements in the group
          integer,                             intent(in)     :: nuvar     !< Number of user variables
          real(kind=WP), dimension(nel,nuvar), intent(inout)  :: uvar      !< User variables array
          type(matparam_struct_),              intent(in)     :: matparam  !< Material parameters data
          real(kind=WP), dimension(nel),       intent(inout)  :: et        !< Coefficient for hourglass
          real(kind=WP),                       intent(in)     :: time      !< Current time
          real(kind=WP), dimension(nel),       intent(in)     :: epsd      !< Equivalent strain rate
          real(kind=WP), dimension(nel),       intent(inout)  :: sigy      !< Yield stress
          real(kind=WP), dimension(nel),       intent(inout)  :: ssp       !< Sound speed
          integer,                             intent(in)     :: nvartmp   !< Temporary number of user variables
          integer,       dimension(nel,nvartmp),intent(inout) :: vartmp    !< Temporary user variables
          integer,       dimension(nel),       intent(in)     :: ngl       !< Element global ID
          real(kind=WP), dimension(nel),       intent(in)     :: depsxx    !< Strain increment xx
          real(kind=WP), dimension(nel),       intent(in)     :: depsyy    !< Strain increment yy
          real(kind=WP), dimension(nel),       intent(in)     :: depszz    !< Strain increment zz
          real(kind=WP), dimension(nel),       intent(in)     :: depsxy    !< Strain increment xy
          real(kind=WP), dimension(nel),       intent(in)     :: depsyz    !< Strain increment yz
          real(kind=WP), dimension(nel),       intent(in)     :: depszx    !< Strain increment zx
          real(kind=WP), dimension(nel),       intent(in)     :: epsxx     !< Total strain xx
          real(kind=WP), dimension(nel),       intent(in)     :: epsyy     !< Total strain yy
          real(kind=WP), dimension(nel),       intent(in)     :: epszz     !< Total strain zz
          real(kind=WP), dimension(nel),       intent(in)     :: epsxy     !< Total strain xy
          real(kind=WP), dimension(nel),       intent(in)     :: epsyz     !< Total strain yz
          real(kind=WP), dimension(nel),       intent(in)     :: epszx     !< Total strain zx
          real(kind=WP), dimension(nel),       intent(in)     :: epspxx    !< Total strain rate xx
          real(kind=WP), dimension(nel),       intent(in)     :: epspyy    !< Total strain rate yy
          real(kind=WP), dimension(nel),       intent(in)     :: epspzz    !< Total strain rate zz
          real(kind=WP), dimension(nel),       intent(in)     :: epspxy    !< Total strain rate xy
          real(kind=WP), dimension(nel),       intent(in)     :: epspyz    !< Total strain rate yz
          real(kind=WP), dimension(nel),       intent(in)     :: epspzx    !< Total strain rate zx
          real(kind=WP), dimension(nel),       intent(in)     :: sigoxx    !< Initial stress xx
          real(kind=WP), dimension(nel),       intent(in)     :: sigoyy    !< Initial stress yy
          real(kind=WP), dimension(nel),       intent(in)     :: sigozz    !< Initial stress zz
          real(kind=WP), dimension(nel),       intent(in)     :: sigoxy    !< Initial stress xy
          real(kind=WP), dimension(nel),       intent(in)     :: sigoyz    !< Initial stress yz
          real(kind=WP), dimension(nel),       intent(in)     :: sigozx    !< Initial stress zx
          real(kind=WP), dimension(nel),       intent(out)    :: signxx    !< New stress xx
          real(kind=WP), dimension(nel),       intent(out)    :: signyy    !< New stress yy
          real(kind=WP), dimension(nel),       intent(out)    :: signzz    !< New stress zz
          real(kind=WP), dimension(nel),       intent(out)    :: signxy    !< New stress xy
          real(kind=WP), dimension(nel),       intent(out)    :: signyz    !< New stress yz
          real(kind=WP), dimension(nel),       intent(out)    :: signzx    !< New stress zx
          real(kind=WP), dimension(nel),       intent(inout)  :: sigvxx    !< Viscous stress xx
          real(kind=WP), dimension(nel),       intent(inout)  :: sigvyy    !< Viscous stress yy
          real(kind=WP), dimension(nel),       intent(inout)  :: sigvzz    !< Viscous stress zz
          real(kind=WP), dimension(nel),       intent(inout)  :: sigvxy    !< Viscous stress xy
          real(kind=WP), dimension(nel),       intent(inout)  :: sigvyz    !< Viscous stress yz
          real(kind=WP), dimension(nel),       intent(inout)  :: sigvzx    !< Viscous stress zx
          real(kind=WP), dimension(nel),       intent(in)     :: rho0      !< Material initial density
          real(kind=WP), dimension(nel),       intent(in)     :: rho       !< Material current density
          integer,                             intent(in)     :: iresp     !< Indicator for simple precision
          real(kind=WP), dimension(nel),       intent(inout)  :: off       !< Element deletion variable
          real(kind=WP),                       intent(in)     :: timestep  !< Current time step
          real(kind=WP), dimension(nel),       intent(in)     :: deltax    !< Element characteristic length
          real(kind=WP),                       intent(in)     :: asrate    !< Strain rate filtering factor
          integer,                             intent(in)     :: l_dmg     !< Damage array size
          real(kind=WP), dimension(nel,l_dmg), intent(inout)  :: dmg       !< Damage array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,nindxc,nindxuc,indxc(nel),indxuc(nel),itype,k,i1,i2,  &
            i3,ipos(nel,20),lcsrtmp,shdflg,ipru,nindxoff(nel,6)
          real(kind=WP) :: young,nu,shear,bulk,sigy0,vf,mu,eaau,ebbu,eccu,gabu,&
            gbcu,gcau,sigyd0,sigyp0,pruab,pruac,prubc,pruba,pruca,prucb,tsef,  &
            ssef,a,aaa,abb,acc,aab,abc,aca
          real(kind=WP) :: num,denom,cs,phiu,m,sp1,sp2,sp3,ldav
          real(kind=WP), dimension(nel) :: vr,beta,eaa,ebb,ecc,gab,gbc,gca,    &
            sigaa,dsigaa,sigbb,dsigbb,sigcc,dsigcc,sigab,dsigab,sigbc,dsigbc,  &
            sigca,dsigca,devexx,deveyy,devezz,devexy,deveyz,devezx,pold,pnew,  &
            sxx,syy,szz,sxy,syz,szx,seq,scale,sigs,dsigs,sigw,dsigw,           &
            su,sp,sd1,sd2,sd3,sigu,dsigu,sigyp,sigyd,lambda,dlambda,lambdaa,   &
            dlambdaa,lambdbb,dlambdbb,lambdcc,dlambdcc,lambdab,dlambdab,       &
            lambdbc,dlambdbc,lambdca,dlambdca,epspm,nuab,nubc,nuac,nuba,nuca,  &
            nucb,delta,caa,cab,cac,cba,cbb,cbc,cca,ccb,ccc,damab,dambc,damca,  &
            ddamab,ddambc,ddamca,le,epspxx_f,epspyy_f,epspzz_f,epspxy_f,       &
            epspyz_f,epspzx_f,epsv
          real(kind=WP), dimension(nel,20)    :: xvec
          real(kind=WP), dimension(nel,3)     :: phi,c2,s2,sigi,dsigi,sigyi
          real(kind=WP), dimension(mvsiz,3)   :: sigp
          real(kind=WP), dimension(mvsiz,6)   :: sig
          real(kind=WP), dimension(mvsiz,3,3) :: dirprv    
          real(kind=WP) :: Astrong(3)
          Astrong = (/ one, zero, zero /)        
!
          !=====================================================================
          !< Initialisation of computation on time step
          !=====================================================================
          !< Recovering integer model parameter
          itype   = matparam%iparam(1)
          lcsrtmp = matparam%iparam(2)
          shdflg  = matparam%iparam(3)
          ipru    = matparam%iparam(4)
          !< Recovering real model parameters
          young   = matparam%young
          nu      = matparam%nu
          shear   = matparam%shear
          bulk    = matparam%bulk
          sigy0   = matparam%uparam(1)
          vf      = matparam%uparam(2)
          mu      = matparam%uparam(3)
          eaau    = matparam%uparam(4)
          ebbu    = matparam%uparam(5)
          eccu    = matparam%uparam(6)
          gabu    = matparam%uparam(7) 
          gbcu    = matparam%uparam(8)
          gcau    = matparam%uparam(9)
          sigyd0  = matparam%uparam(10)
          sigyp0  = matparam%uparam(11)
          pruab   = matparam%uparam(12)
          pruac   = matparam%uparam(13)
          prubc   = matparam%uparam(14)
          pruba   = matparam%uparam(15)
          pruca   = matparam%uparam(16)
          prucb   = matparam%uparam(17)
          tsef    = matparam%uparam(18)
          ssef    = matparam%uparam(19)
!
          !=====================================================================
          !< Initial element characteristic length and strain rate filtering
          !===================================================================== 
          do i = 1,nel
            !< Element characteristic length
            if (uvar(i,5) == zero) uvar(i,5) = deltax(i)
            le(i) = uvar(i,5)
            !< Strain rates filtering
            epspxx_f(i) = asrate*epspxx(i) + (one - asrate)*uvar(i,6)
            epspyy_f(i) = asrate*epspyy(i) + (one - asrate)*uvar(i,7)
            epspzz_f(i) = asrate*epspzz(i) + (one - asrate)*uvar(i,8)
            epspxy_f(i) = asrate*epspxy(i) + (one - asrate)*uvar(i,9)
            epspyz_f(i) = asrate*epspyz(i) + (one - asrate)*uvar(i,10)
            epspzx_f(i) = asrate*epspzx(i) + (one - asrate)*uvar(i,11)
            !< Store filtered strain rates in user variables array
            uvar(i,6)   = epspxx_f(i)
            uvar(i,7)   = epspyy_f(i)
            uvar(i,8)   = epspzz_f(i)
            uvar(i,9)   = epspxy_f(i)
            uvar(i,10)  = epspyz_f(i)
            uvar(i,11)  = epspzx_f(i)
          enddo
!
          !=====================================================================
          !< Check for element deletion
          !=====================================================================    
          do i = 1,nel
            nindxoff(i,1:6) = 0
            if (off(i) <  one) off(i) = off(i)*four_over_5
            if (off(i) < em02) off(i) = zero
            !< Check failure criteria
            if (off(i) == one) then 
              if (epsxx(i) >= tsef) then 
                off(i) = four_over_5
                nindxoff(i,1) = 1
              endif
              if (epsyy(i) >= tsef) then 
                off(i) = four_over_5
                nindxoff(i,2) = 1
              endif
              if (epszz(i) >= tsef) then 
                off(i) = four_over_5
                nindxoff(i,3) = 1
              endif
              if (abs(epsxy(i)) >= ssef) then 
                off(i) = four_over_5
                nindxoff(i,4) = 1
              endif
              if (abs(epsyz(i)) >= ssef) then 
                off(i) = four_over_5
                nindxoff(i,5) = 1
              endif
              if (abs(epszx(i)) >= ssef) then 
                off(i) = four_over_5
                nindxoff(i,6) = 1
              endif
            endif
          enddo
!
          !=====================================================================
          !< Computation of the volume variation coefficient
          !=====================================================================
          nindxc  = 0
          nindxuc = 0
          do i = 1,nel
            !< Relative volume
            vr(i) = rho0(i)/rho(i)
            !< Volumetric strain
            epsv(i) = one - vr(i)
            !< Volume variation coefficient
            beta(i) = max(min((one-vr(i))/(one-vf),one),zero)
            !< List compacted and uncompacted elements
            if ((beta(i) >= one).or.(uvar(i,1) == one)) then
              nindxc = nindxc + 1
              indxc(nindxc) = i
              uvar(i,1) = one
              beta(i) = one
            else
              nindxuc = nindxuc + 1
              indxuc(nindxuc) = i
            endif
            !< Strain rate dependency factor
            lambda(i)  = one
            lambdaa(i) = one
            lambdbb(i) = one
            lambdcc(i) = one
            lambdab(i) = one
            lambdbc(i) = one
            lambdca(i) = one
            damab(i)   = one
            dambc(i)   = one
            damca(i)   = one
            !< Coefficient for hourglass control
            et(i) = one
          enddo
!
          !=====================================================================
          !< Treatment of uncompacted elements
          !=====================================================================
          if (nindxuc > 0) then
!
            !-------------------------------------------------------------------
            !< Computation of trial stress tensor
            !-------------------------------------------------------------------
            do j = 1, nindxuc
              i = indxuc(j)
              !< Update the elastic moduli
              eaa(i) = eaau + beta(i)*(young - eaau)
              ebb(i) = ebbu + beta(i)*(young - ebbu)
              ecc(i) = eccu + beta(i)*(young - eccu)
              gab(i) = gabu + beta(i)*(shear - gabu)
              gbc(i) = gbcu + beta(i)*(shear - gbcu)
              gca(i) = gcau + beta(i)*(shear - gcau)
              !< Update the Poisson's ratios
              ! -> No Poisson's effect
              if (ipru == 0) then 
                nuab(i) = zero
                nuac(i) = zero 
                nubc(i) = zero 
                nuba(i) = zero 
                nuca(i) = zero
                nucb(i) = zero
              ! -> Ramp on Poisson's effect
              elseif (ipru == 1) then 
                nuab(i) = beta(i)*nu
                nuac(i) = beta(i)*nu
                nubc(i) = beta(i)*nu
                nuba(i) = beta(i)*nu
                nuca(i) = beta(i)*nu
                nucb(i) = beta(i)*nu
              ! -> Anisotropic Poisson's effect 
              elseif (ipru == 2) then 
                nuab(i) = pruab
                nuac(i) = pruac
                nubc(i) = prubc
                nuba(i) = pruba
                nuca(i) = pruca
                nucb(i) = prucb
              endif
              !< Elastic matrix coefficients
              delta(i) = (one - nuab(i)*nuba(i) - nubc(i)*nucb(i) -            &
                             nuac(i)*nuca(i) - two*nuab(i)*nubc(i)*nuca(i))/   &
                                                 (eaa(i) * ebb(i) * ecc(i))
              caa(i) =     (one - nubc(i)*nucb(i))/(ebb(i)*ecc(i)*delta(i))
              cab(i) = (nuba(i) + nuca(i)*nubc(i))/(ebb(i)*ecc(i)*delta(i))
              cac(i) = (nuca(i) + nuba(i)*nucb(i))/(ebb(i)*ecc(i)*delta(i))
              cba(i) = (nuab(i) + nuac(i)*nucb(i))/(eaa(i)*ecc(i)*delta(i))
              cbb(i) =     (one - nuac(i)*nuca(i))/(eaa(i)*ecc(i)*delta(i))
              cbc(i) = (nucb(i) + nuca(i)*nuab(i))/(eaa(i)*ecc(i)*delta(i))
              cca(i) = (nuac(i) + nuab(i)*nubc(i))/(eaa(i)*ebb(i)*delta(i))
              ccb(i) = (nubc(i) + nuca(i)*nuba(i))/(eaa(i)*ebb(i)*delta(i))
              ccc(i) =     (one - nuab(i)*nuba(i))/(eaa(i)*ebb(i)*delta(i))
              !< Update stresses
              signxx(i) = sigoxx(i) + caa(i)*depsxx(i) +                       &
                                      cab(i)*depsyy(i) +                       &
                                      cac(i)*depszz(i)
              signyy(i) = sigoyy(i) + cba(i)*depsxx(i) +                       &
                                      cbb(i)*depsyy(i) +                       &
                                      cbc(i)*depszz(i)
              signzz(i) = sigozz(i) + cca(i)*depsxx(i) +                       &
                                      ccb(i)*depsyy(i) +                       &
                                      ccc(i)*depszz(i)
              signxy(i) = sigoxy(i)/max(one-uvar(i,2),em20) + gab(i)*depsxy(i)
              signyz(i) = sigoyz(i)/max(one-uvar(i,3),em20) + gbc(i)*depsyz(i)
              signzx(i) = sigozx(i)/max(one-uvar(i,4),em20) + gca(i)*depszx(i)
              !< Update soundspeed
              ssp(i) = sqrt(max(caa(i),cbb(i),ccc(i),gab(i),gbc(i),gca(i))/    &
                                                      min(rho(i),rho0(i)))
            enddo          
!
            !===================================================================
            !< Check for yielding and update stresses
            !===================================================================
            select case (itype)
!
              !-----------------------------------------------------------------
              !< Classic directionnal clipping of stresses
              !-----------------------------------------------------------------
              case (1) 
!
                !< Preparation of interpolation variables
                do j = 1, nindxuc
                  i = indxuc(j)
                  ! -> Strain in direction A
                  xvec(j,1)  = epsxx(i)
                  xvec(j,2)  = epsd(i)
                  ipos(j,1)  = vartmp(i,1)
                  ipos(j,2)  = vartmp(i,2)
                  ! -> Strain in direction B
                  xvec(j,3)  = epsyy(i)
                  xvec(j,4)  = epsd(i)
                  ipos(j,3)  = vartmp(i,3)
                  ipos(j,4)  = vartmp(i,4)
                  ! -> Strain in direction C
                  xvec(j,5)  = epszz(i)
                  xvec(j,6)  = epsd(i)
                  ipos(j,5)  = vartmp(i,5)
                  ipos(j,6)  = vartmp(i,6)
                  ! -> Shear strain in plane AB
                  xvec(j,7)  = half*epsxy(i)
                  xvec(j,8)  = epsd(i)
                  ipos(j,7)  = vartmp(i,7)
                  ipos(j,8)  = vartmp(i,8)
                  ! -> Shear strain in plane BC
                  xvec(j,9)  = half*epsyz(i)
                  xvec(j,10) = epsd(i)
                  ipos(j,9)  = vartmp(i,9)
                  ipos(j,10) = vartmp(i,10)
                  ! -> Shear strain in plane CA
                  xvec(j,11) = half*epszx(i)
                  xvec(j,12) = epsd(i)
                  ipos(j,11) = vartmp(i,11)
                  ipos(j,12) = vartmp(i,12)
                  !< Interpolation for strain rate dependency
                  if (lcsrtmp > 0) then 
                    xvec(j,13) = epsd(i)
                    ipos(j,13) = vartmp(i,13)
                  elseif (lcsrtmp < 0) then
                    epspm(i)   = third*(epspxx_f(i)+epspyy_f(i)+epspzz_f(i))
                    if (abs(epspxx_f(i) - epspm(i)) > zero) then 
                      xvec(j,13) = log(abs(epspxx_f(i) - epspm(i)))
                    else
                      xvec(j,13) = zero
                    endif
                    ipos(j,13) = vartmp(i,13)
                    if (abs(epspyy_f(i) - epspm(i)) > zero) then 
                      xvec(j,14) = log(abs(epspyy_f(i) - epspm(i)))
                    else
                      xvec(j,14) = zero
                    endif
                    ipos(j,14) = vartmp(i,14)
                    if (abs(epspzz_f(i) - epspm(i)) > zero) then 
                      xvec(j,15) = log(abs(epspzz_f(i) - epspm(i)))
                    else
                      xvec(j,15) = zero
                    endif
                    ipos(j,15) = vartmp(i,15)
                    if (abs(epspxy_f(i)) > zero) then 
                      xvec(j,16) = log(abs(epspxy_f(i)))
                    else
                      xvec(j,16) = zero
                    endif
                    ipos(j,16) = vartmp(i,16)
                    if (abs(epspyz_f(i)) > zero) then 
                      xvec(j,17) = log(abs(epspyz_f(i)))
                    else
                      xvec(j,17) = zero
                    endif
                    ipos(j,17) = vartmp(i,17)
                    if (abs(epspzx_f(i)) > zero) then 
                      xvec(j,18) = log(abs(epspzx_f(i)))
                    else
                      xvec(j,18) = zero
                    endif
                    ipos(j,18) = vartmp(i,18)
                  endif
                enddo       
!
                !< Interpolation on direction A
                call table_mat_vinterp(matparam%table(1),nindxuc,nindxuc,      &
                    ipos(1:nindxuc,1),xvec(1:nindxuc,1),sigaa(1:nindxuc),      &
                    dsigaa(1:nindxuc))
                !< Interpolation on direction B
                call table_mat_vinterp(matparam%table(2),nindxuc,nindxuc,      &
                    ipos(1:nindxuc,3),xvec(1:nindxuc,3),sigbb(1:nindxuc),      &
                    dsigbb(1:nindxuc))
                !< Interpolation on direction C
                call table_mat_vinterp(matparam%table(3),nindxuc,nindxuc,      &
                    ipos(1:nindxuc,5),xvec(1:nindxuc,5),sigcc(1:nindxuc),      &
                    dsigcc(1:nindxuc))
                !< Interpolation of shear plane AB
                call table_mat_vinterp(matparam%table(4),nindxuc,nindxuc,      &
                    ipos(1:nindxuc,7),xvec(1:nindxuc,7),sigab(1:nindxuc),      &
                    dsigab(1:nindxuc))
                !< Interpolation of shear plane BC
                call table_mat_vinterp(matparam%table(5),nindxuc,nindxuc,      &
                    ipos(1:nindxuc,9),xvec(1:nindxuc,9),sigbc(1:nindxuc),      &
                    dsigbc(1:nindxuc))
                !< Interpolation of shear plane CA
                call table_mat_vinterp(matparam%table(6),nindxuc,nindxuc,      &
                    ipos(1:nindxuc,11),xvec(1:nindxuc,11),sigca(1:nindxuc),    &
                    dsigca(1:nindxuc))
                !< Strain rate dependency interpolation
                ! -> General strain rate scale factor
                if (lcsrtmp > 0) then 
                  call table_mat_vinterp(matparam%table(7),nindxuc,nindxuc,    &
                     ipos(1:nindxuc,13),xvec(1:nindxuc,13),lambda(1:nindxuc),  &
                     dlambda(1:nindxuc))
                ! -> Directional strain rate scale factors
                elseif (lcsrtmp < 0) then
                  call table_mat_vinterp(matparam%table(7),nindxuc,nindxuc,    &
                     ipos(1:nindxuc,13),xvec(1:nindxuc,13),lambdaa(1:nindxuc), &
                     dlambdaa(1:nindxuc))
                  call table_mat_vinterp(matparam%table(8),nindxuc,nindxuc,    &
                     ipos(1:nindxuc,14),xvec(1:nindxuc,14),lambdbb(1:nindxuc), &
                     dlambdbb(1:nindxuc))
                  call table_mat_vinterp(matparam%table(9),nindxuc,nindxuc,   &
                     ipos(1:nindxuc,15),xvec(1:nindxuc,15),lambdcc(1:nindxuc), &
                     dlambdcc(1:nindxuc))
                  call table_mat_vinterp(matparam%table(10),nindxuc,nindxuc,   &
                     ipos(1:nindxuc,16),xvec(1:nindxuc,16),lambdab(1:nindxuc), &
                     dlambdab(1:nindxuc))
                  call table_mat_vinterp(matparam%table(11),nindxuc,nindxuc,   &
                     ipos(1:nindxuc,17),xvec(1:nindxuc,17),lambdbc(1:nindxuc), &
                     dlambdbc(1:nindxuc))
                  call table_mat_vinterp(matparam%table(12),nindxuc,nindxuc,   &
                     ipos(1:nindxuc,18),xvec(1:nindxuc,18),lambdca(1:nindxuc), &
                     dlambdca(1:nindxuc))
                endif
!                 
                !< Check for yielding and update stresses
                do j = 1, nindxuc
                  i = indxuc(j)
                  ! -> Clip stress in direction A
                  sigaa(j) = lambda(j)*lambdaa(j)*max(sigaa(j),zero)
                  if (abs(signxx(i)) > sigaa(j)) then 
                    signxx(i) = sigaa(j)*signxx(i)/abs(signxx(i))
                  endif
                  vartmp(i,1) = ipos(j,1)
                  vartmp(i,2) = ipos(j,2)
                  ! -> Clip stress in direction B
                  sigbb(j) = lambda(j)*lambdbb(j)*max(sigbb(j),zero)
                  if (abs(signyy(i)) > sigbb(j)) then 
                    signyy(i) = sigbb(j)*signyy(i)/abs(signyy(i))
                  endif
                  vartmp(i,3) = ipos(j,3)
                  vartmp(i,4) = ipos(j,4)
                  ! -> Clip stress in direction C
                  sigcc(j) = lambda(j)*lambdcc(j)*max(sigcc(j),zero)
                  if (abs(signzz(i)) > sigcc(j)) then 
                    signzz(i) = sigcc(j)*signzz(i)/abs(signzz(i))
                  endif
                  vartmp(i,5) = ipos(j,5)
                  vartmp(i,6) = ipos(j,6)
                  ! -> Clip shear stress in plane AB
                  sigab(j) = lambda(j)*lambdab(j)*max(sigab(j),zero)
                  if (abs(signxy(i)) > sigab(j)) then 
                    signxy(i) = sigab(j)*signxy(i)/abs(signxy(i))
                  endif
                  vartmp(i,7) = ipos(j,7)
                  vartmp(i,8) = ipos(j,8)
                  ! -> Clip shear stress in plane BC
                  sigbc(j) = lambda(j)*lambdbc(j)*max(sigbc(j),zero)
                  if (abs(signyz(i)) > sigbc(j)) then 
                    signyz(i) = sigbc(j)*signyz(i)/abs(signyz(i))
                  endif
                  vartmp(i,9) = ipos(j,9)
                  vartmp(i,10)= ipos(j,10)
                  ! -> Clip shear stress in plane CA
                  sigca(j) = lambda(j)*lambdca(j)*max(sigca(j),zero)
                  if (abs(signzx(i)) > sigca(j)) then 
                    signzx(i) = sigca(j)*signzx(i)/abs(signzx(i))
                  endif
                  vartmp(i,11) = ipos(j,11)
                  vartmp(i,12) = ipos(j,12)
                  ! -> Save interpolation position for strain rate dependency
                  if (lcsrtmp > 0) then 
                    vartmp(i,13) = ipos(j,13)
                  elseif (lcsrtmp < 0) then
                    vartmp(i,13) = ipos(j,13)
                    vartmp(i,14) = ipos(j,14)
                    vartmp(i,15) = ipos(j,15)
                    vartmp(i,16) = ipos(j,16)
                    vartmp(i,17) = ipos(j,17)
                    vartmp(i,18) = ipos(j,18)
                  endif
                  ! -> Save the global yield stress
                  sigy(i) = max(sigaa(j),sigbb(j),sigcc(j),                    &
                                sigab(j),sigbc(j),sigca(j))
                enddo
!
              !-----------------------------------------------------------------
              !< Transverse isotropic yield surface
              !-----------------------------------------------------------------
              case(2)
!
                !< Assemble stress tensor
                do j = 1, nindxuc
                  i = indxuc(j)
                  sig(j,1) = signxx(i)
                  sig(j,2) = signyy(i)
                  sig(j,3) = signzz(i)
                  sig(j,4) = signxy(i)
                  sig(j,5) = signyz(i)
                  sig(j,6) = signzx(i)
                enddo
!
                !< Compute principal strains and directions
                if (iresp == 1) then
                  call valpvecdp_v(sig ,sigp ,dirprv ,nindxuc)
                else
                  call valpvec_v(sig ,sigp ,dirprv ,nindxuc)
                endif
!
                !< Strong axis = direction A = e_x in the material frame
                do j = 1, nindxuc
                  call fix_principal_dirs(sigp(j,1:3), dirprv(j,1:3,1:3), Astrong)
                enddo
!
                !< Compute angle phi between direction A and first principal 
                !  direction
                do i = 1,3
                  do j = 1, nindxuc
                    phi(j,i) = acosd(abs(dirprv(j,1,i)))
                    c2(j,i)  = min(one, max(zero, dirprv(j,1,i)**2))
                    s2(j,i)  = one - c2(j,i)
                  enddo
                enddo
!
                !< Preparation of interpolation variables
                do j = 1, nindxuc
                  i = indxuc(j)
                  !< Table 1 : principal directions 1,2,3
                  ! -> Limit stress sigi = f(phi_i)
                  xvec(j,1) = phi(j,1)
                  xvec(j,2) = phi(j,2)
                  xvec(j,3) = phi(j,3)
                  ipos(j,1) = vartmp(i,1)
                  ipos(j,2) = vartmp(i,2)
                  ipos(j,3) = vartmp(i,3)
                  !< Table 2 : Strong axis limit stress 
                  ! -> sigs = f(epsv)
                  xvec(j,4) = epsv(i)
                  ipos(j,4) = vartmp(i,4)
                  !< Table 3 : Weak axis limit stress
                  ! -> sigw = f(epsv)
                  xvec(j,5) = epsv(i)
                  ipos(j,5) = vartmp(i,5)
                  !< Interpolation for strain rate dependency
                  if (lcsrtmp > 0) then 
                    xvec(j,9) = epsd(i)
                    ipos(j,9) = vartmp(i,9)
                  endif
                enddo
!
                !< Limit stress in direction 1 as function of angle phi
                call table_mat_vinterp(matparam%table(1),nindxuc,nindxuc,      &
                   ipos(1:nindxuc,1),xvec(1:nindxuc,1),sigi(1:nindxuc,1),      &
                   dsigi(1:nindxuc,1))
                !< Limit stress in direction 2 as function of angle phi
                call table_mat_vinterp(matparam%table(1),nindxuc,nindxuc,      &
                   ipos(1:nindxuc,2),xvec(1:nindxuc,2),sigi(1:nindxuc,2),      &
                   dsigi(1:nindxuc,2))
                !< Limit stress in direction 3 as function of angle phi
                call table_mat_vinterp(matparam%table(1),nindxuc,nindxuc,      &
                   ipos(1:nindxuc,3),xvec(1:nindxuc,3),sigi(1:nindxuc,3),      &
                   dsigi(1:nindxuc,3))
                !< Strong axis limit stress as function of volumic strain
                call table_mat_vinterp(matparam%table(2),nindxuc,nindxuc,      &
                   ipos(1:nindxuc,4),xvec(1:nindxuc,4),sigs(1:nindxuc),        &
                   dsigs(1:nindxuc))
                !< Weak axis limit stress as function of volumic strain
                call table_mat_vinterp(matparam%table(3),nindxuc,nindxuc,      &
                   ipos(1:nindxuc,5),xvec(1:nindxuc,5),sigw(1:nindxuc),        &
                   dsigw(1:nindxuc))
                !< Strain rate dependency interpolation
                if (lcsrtmp > 0) then 
                  call table_mat_vinterp(matparam%table(7),nindxuc,nindxuc,    &
                     ipos(1:nindxuc,9),xvec(1:nindxuc,9),lambda(1:nindxuc),    &
                     dlambda(1:nindxuc))
                endif
!                
                !< Clip principal stresses
                do j = 1, nindxuc
                  i = indxuc(j)
                  !< Save interpolation position
                  vartmp(i,1) = ipos(j,1)
                  vartmp(i,2) = ipos(j,2)
                  vartmp(i,3) = ipos(j,3)
                  vartmp(i,4) = ipos(j,4)
                  vartmp(i,5) = ipos(j,5)
                  ! -> Save interpolation position for strain rate dependency
                  if (lcsrtmp > 0) then 
                    vartmp(i,9) = ipos(j,9)
                  endif
                  !< Compute yield stress in principal directions
                  sigyi(j,1) = sigi(j,1) + c2(j,1)*sigs(j) + s2(j,1)*sigw(j)                     
                  sigyi(j,2) = sigi(j,2) + c2(j,2)*sigs(j) + s2(j,2)*sigw(j)  
                  sigyi(j,3) = sigi(j,3) + c2(j,3)*sigs(j) + s2(j,3)*sigw(j)  
                  !< Assemble general yield stress
                  num = zero
                  denom = zero
                  do k = 1, 3
                    num = num + sigyi(j,k)*sigp(j,k)*sigp(j,k)
                    denom = denom + sigp(j,k)*sigp(j,k)
                  enddo
                  denom = max(denom,em20)
                  sigy(i) = num/denom
                  !< Clip principal stresses
                  sigp(j,1) = sigp(j,1)*min(one,sigy(i)*lambda(j)/(sqrt(denom)))
                  sigp(j,2) = sigp(j,2)*min(one,sigy(i)*lambda(j)/(sqrt(denom)))
                  sigp(j,3) = sigp(j,3)*min(one,sigy(i)*lambda(j)/(sqrt(denom))) 
                  !< Reassemble stress tensor
                  signxx(i) = dirprv(j,1,1) * dirprv(j,1,1) * sigp(j,1) +      &
                              dirprv(j,1,2) * dirprv(j,1,2) * sigp(j,2) +      &
                              dirprv(j,1,3) * dirprv(j,1,3) * sigp(j,3)
                  signyy(i) = dirprv(j,2,1) * dirprv(j,2,1) * sigp(j,1) +      &
                              dirprv(j,2,2) * dirprv(j,2,2) * sigp(j,2) +      &
                              dirprv(j,2,3) * dirprv(j,2,3) * sigp(j,3)
                  signzz(i) = dirprv(j,3,1) * dirprv(j,3,1) * sigp(j,1) +      &
                              dirprv(j,3,2) * dirprv(j,3,2) * sigp(j,2) +      &
                              dirprv(j,3,3) * dirprv(j,3,3) * sigp(j,3)
                  signxy(i) = dirprv(j,1,1) * dirprv(j,2,1) * sigp(j,1) +      &
                              dirprv(j,1,2) * dirprv(j,2,2) * sigp(j,2) +      &
                              dirprv(j,1,3) * dirprv(j,2,3) * sigp(j,3)
                  signyz(i) = dirprv(j,2,1) * dirprv(j,3,1) * sigp(j,1) +      &
                              dirprv(j,2,2) * dirprv(j,3,2) * sigp(j,2) +      &
                              dirprv(j,2,3) * dirprv(j,3,3) * sigp(j,3)
                  signzx(i) = dirprv(j,3,1) * dirprv(j,1,1) * sigp(j,1) +      &
                              dirprv(j,3,2) * dirprv(j,1,2) * sigp(j,2) +      &
                              dirprv(j,3,3) * dirprv(j,1,3) * sigp(j,3)   
!
                enddo
!
              !-----------------------------------------------------------------
              !< Hydrostatic/shear limit yield surface
              !-----------------------------------------------------------------
              case(3)
!
                !< Assemble stress tensor
                do j = 1, nindxuc
                  i = indxuc(j)
                  sig(j,1) = signxx(i)
                  sig(j,2) = signyy(i)
                  sig(j,3) = signzz(i)
                  sig(j,4) = signxy(i)
                  sig(j,5) = signyz(i)
                  sig(j,6) = signzx(i)
                enddo
!
                !< Compute principal strains and directions
                if (iresp == 1) then
                  call valpvecdp_v(sig ,sigp ,dirprv ,nindxuc)
                else
                  call valpvec_v(sig ,sigp ,dirprv ,nindxuc)
                endif
!
                !< Loop over uncompacted elements
                do j = 1, nindxuc
                  i = indxuc(j)
!
                  !< Sort principal directions
                  call order3(sigp(j,1:3),i1,i2,i3,sp1,sp2,sp3)
!
                  !< Compression case
                  if (sp1 <= -sp3) then 
                    m = max(abs(sp2),abs(sp3))
                    !< Compute the uniaxial stress
                    su(j) = sp1 + m
                    !< Compute the hydrostatic and deviatoric stresses
                    sp(j)  = third*(-m + sp2 + sp3)
                    sd1(j) = -m  - sp(j)
                    sd2(j) = sp2 - sp(j)
                    sd3(j) = sp3 - sp(j)
                    !< Compute cosine of angle between uniaxial stress direction
                    !  and direction A
                    cs = max(zero, min(one, abs(dirprv(j,1,i1))))
                  !< Tension case
                  else
                    m = max(abs(sp1),abs(sp2))
                    !< Compute the uniaxial stress
                    su(j) = sp3 - m
                    !< Compute the hydrostatic and deviatoric stresses
                    sp(j) = third*(m + sp1 + sp2)
                    sd1(j) = sp1 - sp(j)
                    sd2(j) = sp2 - sp(j)
                    sd3(j) = m   - sp(j)
                    !< Compute cosine of angle between uniaxial stress direction 
                    !  and direction A
                    cs = max(zero, min(one, abs(dirprv(j,1,i3))))
                  endif
!
                  !< Compute angle phiu between direction of uniaxial stress and 
                  !  direction A
                  phiu = acosd(cs)
!
                  !< Preparation of interpolation variables
                  ! -> Table 1 : uniaxial stress limit = f(phi, epsv) 
                  xvec(j,1) = phiu
                  xvec(j,2) = epsv(i)
                  ipos(j,1) = vartmp(i,1)
                  ipos(j,2) = vartmp(i,2)
                  ! -> Table 2 : shear/hydrostatic stress limit = f(epsv)
                  xvec(j,3) = epsv(i)
                  ipos(j,3) = vartmp(i,3)
                  !< Interpolation for strain rate dependency
                  if (lcsrtmp > 0) then 
                    xvec(j,9) = epsd(i)
                    ipos(j,9) = vartmp(i,9)
                  endif
                enddo  
!
                !< Uniaxial stress interpolation
                call table_mat_vinterp(matparam%table(1),nindxuc,nindxuc,      &
                     ipos(1:nindxuc,1),xvec(1:nindxuc,1),sigu(1:nindxuc),      &
                     dsigu(1:nindxuc))
                !< Shear/hydrostatic stress interpolation
                call table_mat_vinterp(matparam%table(2),nindxuc,nindxuc,      &
                     ipos(1:nindxuc,3),xvec(1:nindxuc,3),sigs(1:nindxuc),      &
                     dsigs(1:nindxuc))       
                !< Strain rate dependency interpolation
                if (lcsrtmp > 0) then 
                  call table_mat_vinterp(matparam%table(7),nindxuc,nindxuc,    &
                     ipos(1:nindxuc,9),xvec(1:nindxuc,9),lambda(1:nindxuc),    &
                     dlambda(1:nindxuc))
                endif    
!
                !< Loop over uncompacted elements
                do j = 1, nindxuc
                  i = indxuc(j)
                  !< Save interpolation position
                  vartmp(i,1) = ipos(j,1)
                  vartmp(i,2) = ipos(j,2)
                  vartmp(i,3) = ipos(j,3)
                  ! -> Save interpolation position for strain rate dependency
                  if (lcsrtmp > 0) then 
                    vartmp(i,9) = ipos(j,9)
                  endif
                  !< Hydrostatic stress limit
                  sigyp(j) = sigyp0 + sigs(j)
                  !< Deviatoric stress limit
                  sigyd(j) = sigyd0 + sigs(j)
                  !< Compute general yield stress
                  sigy(i) = sigu(j)*(su(j)**2) +                               &
                                      three*sqrt(three)*sigyp(j)*(sp(j)**2) +  & 
                        sqrt(two)*sigyd(j)*(sd1(j)**2 + sd2(j)**2 + sd3(j)**2)
                  sigy(i) = sigy(i)/max((su(j)**2 + three*(sp(j)**2) +         &
                                  sd1(j)**2 + sd2(j)**2 + sd3(j)**2),em20)
                  !< Compute denominator for stress clipping
                  denom = zero
                  do k = 1, 3
                    denom = denom + sigp(j,k)*sigp(j,k)
                  enddo
                  denom = max(denom,em20)
                  !< Clip principal stresses
                  sigp(j,1) = sigp(j,1)*min(one,sigy(i)*lambda(j)/(sqrt(denom)))
                  sigp(j,2) = sigp(j,2)*min(one,sigy(i)*lambda(j)/(sqrt(denom)))
                  sigp(j,3) = sigp(j,3)*min(one,sigy(i)*lambda(j)/(sqrt(denom))) 
                  !< Reassemble stress tensor
                  signxx(i) = dirprv(j,1,1) * dirprv(j,1,1) * sigp(j,1) +      &
                              dirprv(j,1,2) * dirprv(j,1,2) * sigp(j,2) +      &
                              dirprv(j,1,3) * dirprv(j,1,3) * sigp(j,3)
                  signyy(i) = dirprv(j,2,1) * dirprv(j,2,1) * sigp(j,1) +      &
                              dirprv(j,2,2) * dirprv(j,2,2) * sigp(j,2) +      &
                              dirprv(j,2,3) * dirprv(j,2,3) * sigp(j,3)
                  signzz(i) = dirprv(j,3,1) * dirprv(j,3,1) * sigp(j,1) +      &
                              dirprv(j,3,2) * dirprv(j,3,2) * sigp(j,2) +      &
                              dirprv(j,3,3) * dirprv(j,3,3) * sigp(j,3)
                  signxy(i) = dirprv(j,1,1) * dirprv(j,2,1) * sigp(j,1) +      &
                              dirprv(j,1,2) * dirprv(j,2,2) * sigp(j,2) +      &
                              dirprv(j,1,3) * dirprv(j,2,3) * sigp(j,3)
                  signyz(i) = dirprv(j,2,1) * dirprv(j,3,1) * sigp(j,1) +      &
                              dirprv(j,2,2) * dirprv(j,3,2) * sigp(j,2) +      &
                              dirprv(j,2,3) * dirprv(j,3,3) * sigp(j,3)
                  signzx(i) = dirprv(j,3,1) * dirprv(j,1,1) * sigp(j,1) +      &
                              dirprv(j,3,2) * dirprv(j,1,2) * sigp(j,2) +      &
                              dirprv(j,3,3) * dirprv(j,1,3) * sigp(j,3)    
                enddo
!
            end select
!
            !< Viscous stresses computation
            if (mu > zero) then
              do j = 1, nindxuc
                i = indxuc(j)
                !< Viscous damping coefficient
                a = ssp(i)*rho(i)*mu*le(i)/(one + epsv(i))
                aaa = a*(eaa(i)/young)
                abb = a*(ebb(i)/young)
                acc = a*(ecc(i)/young)
                aab = a*(gab(i)/shear)
                abc = a*(gbc(i)/shear)
                aca = a*(gca(i)/shear)
                !< Viscous stresses
                sigvxx(i) = aaa*epspxx_f(i)
                sigvyy(i) = abb*epspyy_f(i)
                sigvzz(i) = acc*epspzz_f(i)
                sigvxy(i) = aab*epspxy_f(i)/two
                sigvyz(i) = abc*epspyz_f(i)/two
                sigvzx(i) = aca*epspzx_f(i)/two
                !< Update the soundspeed to include the viscous damping stiffness
                if (timestep > zero) then
                  ssp(i) = sqrt(ssp(i)**2 +                                    &
                  (max(aaa,abb,acc,aab,abc,aca)/timestep)/min(rho(i),rho0(i)))
                endif
              enddo
            endif
!
          endif
!
          !=====================================================================
          !< Treatment of fully compacted elements
          !=====================================================================
          if (nindxc > 0) then
            do j = 1, nindxc
              i = indxc(j)
              !< Deviatoric strain increments
              devexx(i) = depsxx(i) - third*(depsxx(i)+depsyy(i)+depszz(i)) 
              deveyy(i) = depsyy(i) - third*(depsxx(i)+depsyy(i)+depszz(i)) 
              devezz(i) = depszz(i) - third*(depsxx(i)+depsyy(i)+depszz(i)) 
              devexy(i) = depsxy(i)
              deveyz(i) = depsyz(i)
              devezx(i) = depszx(i)
              !< Previous hydrostatic pressure
              pold(i) = -third*(sigoxx(i) + sigoyy(i) + sigozz(i))
              !< Trial deviatoric stresses
              sxx(i) = sigoxx(i) + two*shear*devexx(i)   + pold(i)
              syy(i) = sigoyy(i) + two*shear*deveyy(i)   + pold(i)
              szz(i) = sigozz(i) + two*shear*devezz(i)   + pold(i)
              sxy(i) = sigoxy(i)/max(one-uvar(i,2),em20) + shear*devexy(i)
              syz(i) = sigoyz(i)/max(one-uvar(i,3),em20) + shear*deveyz(i)
              szx(i) = sigozx(i)/max(one-uvar(i,4),em20) + shear*devezx(i)
              !< Equivalent stress
              seq(i) = sxx(i)**2 + syy(i)**2 + szz(i)**2 +                     &
                              two*(sxy(i)**2 + syz(i)**2 + szx(i)**2)
              seq(i) = sqrt(three*half*seq(i))
              !< Scaling the deviatoric stresses
              sigy(i) = sigy0
              scale(i) = min(one,sigy0/max(seq(i),em20))
              sxx(i) = scale(i)*sxx(i)
              syy(i) = scale(i)*syy(i)
              szz(i) = scale(i)*szz(i)
              sxy(i) = scale(i)*sxy(i)
              syz(i) = scale(i)*syz(i)
              szx(i) = scale(i)*szx(i)
              !< Update hydrostatic pressure
              pnew(i) = pold(i) - bulk*(depsxx(i) + depsyy(i) + depszz(i))
              !< Assembling the new Cauchy stress tensor
              signxx(i) = sxx(i) - pnew(i)
              signyy(i) = syy(i) - pnew(i)
              signzz(i) = szz(i) - pnew(i)
              signxy(i) = sxy(i)
              signyz(i) = syz(i)
              signzx(i) = szx(i)
              !< Sound speed
              ssp(i) = sqrt((bulk + four_over_3*shear)/min(rho(i),rho0(i)))
              !< Viscous stresses computation
              if (mu > zero) then
                !< Viscous damping coefficient
                a = ssp(i)*rho(i)*mu*le(i)/(one + epsv(i))
                ldav = third*(epspxx_f(i) + epspyy_f(i) + epspzz_f(i))
                !< Viscous stresses
                sigvxx(i) = a*(epspxx_f(i)-ldav)
                sigvyy(i) = a*(epspyy_f(i)-ldav)
                sigvzz(i) = a*(epspzz_f(i)-ldav)
                sigvxy(i) = a*epspxy_f(i)/two
                sigvyz(i) = a*epspyz_f(i)/two
                sigvzx(i) = a*epspzx_f(i)/two
                !< Update the soundspeed to include the viscous damping stiffness
                if (timestep > zero) then
                  ssp(i) = sqrt(ssp(i)**2 + (a/timestep)/min(rho(i),rho0(i)))
                end if
              endif
            enddo
          endif
!
          !=====================================================================
          !< Shear damage treatment
          !=====================================================================    
          if (itype > 1) then 
            do i = 1,nel       
              !< Table shear damage in ab plane
              xvec(i,6) = abs(half*epsxy(i))
              ipos(i,6) = vartmp(i,6)
              !< Table shear damage in bc plane
              xvec(i,7) = abs(half*epsyz(i))
              ipos(i,7) = vartmp(i,7)
              !< Table shear damage in ca plane
              xvec(i,8) = abs(half*epszx(i))
              ipos(i,8) = vartmp(i,8)
            enddo
            !< Shear damage in ab plane
            if (matparam%table(4)%notable > 0) then 
              call table_mat_vinterp(matparam%table(4),nel,nel,ipos(1:nel,6),  &
                 xvec(1:nel,6),damab(1:nel),ddamab(1:nel))    
            endif  
            !< Shear damage in bc plane
            if (matparam%table(5)%notable > 0) then 
              call table_mat_vinterp(matparam%table(5),nel,nel,ipos(1:nel,7),  &
                 xvec(1:nel,7),dambc(1:nel),ddambc(1:nel))
            endif  
            !< Shear damage in ca plane
            if (matparam%table(6)%notable > 0) then 
              call table_mat_vinterp(matparam%table(6),nel,nel,ipos(1:nel,8),  &
                 xvec(1:nel,8),damca(1:nel),ddamca(1:nel))
            endif            
            do i = 1,nel
              !< Save interpolation positions
              vartmp(i,6) = ipos(i,6)
              vartmp(i,7) = ipos(i,7)
              vartmp(i,8) = ipos(i,8)
              !< Check damage values
              damab(i) = max(zero, min(one, damab(i)))
              dambc(i) = max(zero, min(one, dambc(i)))
              damca(i) = max(zero, min(one, damca(i)))
              damab(i) = min(damab(i), one - dmg(i,2))
              dambc(i) = min(dambc(i), one - dmg(i,3))
              damca(i) = min(damca(i), one - dmg(i,4))
              !< Shear damaged stresses
              signxy(i) = signxy(i)*damab(i)
              signyz(i) = signyz(i)*dambc(i)
              signzx(i) = signzx(i)*damca(i)
              if (shdflg == 1) then 
                uvar(i,2) = one - damab(i)
                uvar(i,3) = one - dambc(i)
                uvar(i,4) = one - damca(i)
              endif
              !< Save damage for output
              dmg(i,2) = one - damab(i)
              dmg(i,3) = one - dambc(i)
              dmg(i,4) = one - damca(i)
              dmg(i,1) = max(dmg(i,2),dmg(i,3),dmg(i,4))
            enddo
          endif
!
          !=====================================================================
          !< Printout element deletion info
          !=====================================================================
          do i = 1,nel
            if (off(i) == four_over_5) then 
              write(iout, 1000) ngl(i),time
              write(istdo,1000) ngl(i),time
              if (nindxoff(i,1) == 1) write(iout ,1001) epsxx(i),tsef
              if (nindxoff(i,1) == 1) write(istdo,1001) epsxx(i),tsef
              if (nindxoff(i,2) == 1) write(iout ,1002) epsyy(i),tsef
              if (nindxoff(i,2) == 1) write(istdo,1002) epsyy(i),tsef
              if (nindxoff(i,3) == 1) write(iout ,1003) epszz(i),tsef
              if (nindxoff(i,3) == 1) write(istdo,1003) epszz(i),tsef
              if (nindxoff(i,4) == 1) write(iout ,1004) abs(epsxy(i)),ssef
              if (nindxoff(i,4) == 1) write(istdo,1004) abs(epsxy(i)),ssef
              if (nindxoff(i,5) == 1) write(iout ,1005) abs(epsyz(i)),ssef
              if (nindxoff(i,5) == 1) write(istdo,1005) abs(epsyz(i)),ssef
              if (nindxoff(i,6) == 1) write(iout ,1006) abs(epszx(i)),ssef
              if (nindxoff(i,6) == 1) write(istdo,1006) abs(epszx(i)),ssef
            endif
          enddo
 1000 format(1x,'-- RUPTURE (MODIFIED_HONEYCOMB) OF SOLID ELEMENT :',i10,' AT TIME :',1pe12.4)  
 1001 format(1x,'  STRAIN EPSXX = ',1pe12.4,' EXCEEDS LIMIT VALUE TSEF =',1pe12.4)
 1002 format(1x,'  STRAIN EPSYY = ',1pe12.4,' EXCEEDS LIMIT VALUE TSEF =',1pe12.4)
 1003 format(1x,'  STRAIN EPSZZ = ',1pe12.4,' EXCEEDS LIMIT VALUE TSEF =',1pe12.4)
 1004 format(1x,'  STRAIN EPSXY = ',1pe12.4,' EXCEEDS LIMIT VALUE SSEF =',1pe12.4)
 1005 format(1x,'  STRAIN EPSYZ = ',1pe12.4,' EXCEEDS LIMIT VALUE SSEF =',1pe12.4)
 1006 format(1x,'  STRAIN EPSZX = ',1pe12.4,' EXCEEDS LIMIT VALUE SSEF =',1pe12.4)
!
        end subroutine sigeps130
!
        !<----------------------------------------------------------------------
        !< Function to compare two reals with a tolerance
        !<----------------------------------------------------------------------
        pure logical function gt(a,b)
          use precision_mod, only: WP
          real(WP), intent(in) :: a,b
          real(WP) :: tol
          tol = 1.0e-12_WP * max(1._WP,abs(a),abs(b))
          gt = (a > b + tol)
        end function gt
!
        !<----------------------------------------------------------------------
        !< Subroutine to order 3 values and return their indexes
        !<----------------------------------------------------------------------
!||====================================================================
!||    order3          ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- called by ------------------------------------------------------
!||    sigeps130       ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- calls      -----------------------------------------------------
!||    iswap           ../engine/source/materials/mat/mat130/sigeps130.F90
!||    rswap           ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine order3(sig, i1,i2,i3, s1,s2,s3)
          use precision_mod, only: WP
          real(WP), intent(in)  :: sig(3)
          integer,  intent(out) :: i1,i2,i3
          real(WP), intent(out) :: s1,s2,s3
          integer :: a,b,c
          real(WP) :: x,y,z
          !< Initial indexes and values
          a = 1
          b = 2
          c = 3
          x = sig(a)
          y = sig(b)
          z = sig(c)
          !< Sort 3 values and indexes
          if (gt(x,y)) then
            call iswap(a,b)
            call rswap(x,y)
          endif
          if (gt(y,z)) then
            call iswap(b,c)
            call rswap(y,z)
          endif
          if (gt(x,y)) then
            call iswap(a,b)
            call rswap(x,y)
          endif
          !< Outputs indexes and values
          i1=a
          i2=b
          i3=c
          s1=x
          s2=y
          s3=z
        contains
!||====================================================================
!||    iswap    ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- called by ------------------------------------------------------
!||    order3   ../engine/source/materials/mat/mat130/sigeps130.F90
!||====================================================================
          subroutine iswap(i,j)
            integer, intent(inout) :: i,j
            integer :: t
            t = i
            i = j
            j = t
          end subroutine iswap
!||====================================================================
!||    rswap    ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- called by ------------------------------------------------------
!||    order3   ../engine/source/materials/mat/mat130/sigeps130.F90
!||====================================================================
          subroutine rswap(r,t)
            real(WP), intent(inout) :: r,t
            real(WP) :: q
            q = r
            r = t
            t = q
          end subroutine rswap
        end subroutine order3
!
!||====================================================================
!||    fix_principal_dirs          ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- called by ------------------------------------------------------
!||    sigeps130                   ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- calls      -----------------------------------------------------
!||    enforce_plane_orientation   ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod               ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine fix_principal_dirs(sigp, dir, strong_axis)
          use precision_mod, only : WP
          implicit none
          real(WP), intent(in)    :: sigp(3)        
          real(WP), intent(inout) :: dir(3,3)       
          real(WP), intent(in)    :: strong_axis(3)
        
          real(WP) :: s1, s2, s3, smax, tol
          logical  :: eq12, eq23, eq13
          real(WP) :: A(3), nplane(3), vperp(3), vother(3), tmp(3)
          real(WP) :: nrm
        
          s1 = sigp(1)
          s2 = sigp(2)
          s3 = sigp(3)
        
          smax = max( abs(s1), abs(s2), abs(s3), 1.0_WP )
          tol  = 1.0e-6_WP * smax
        
          eq12 = (abs(s1 - s2) <= tol)
          eq23 = (abs(s2 - s3) <= tol)
          eq13 = (abs(s1 - s3) <= tol)
        
          A = strong_axis
          nrm = sqrt( A(1)**2 + A(2)**2 + A(3)**2 )
          if (nrm > 0.0_WP) then
            A = A / nrm
          else
            return
          endif
        
          ! ==== CASE 1 : all eigenvalues are equal (hydrostatic) ====
          if (eq12 .and. eq23) then
            dir(:,1) = A
        
            if (abs(A(1)) < 0.9_WP) then
              tmp = (/ 1.0_WP, 0.0_WP, 0.0_WP /)
            else
              tmp = (/ 0.0_WP, 1.0_WP, 0.0_WP /)
            endif
        
            vperp = tmp - (dot_product(tmp, A))*A
            nrm = sqrt( vperp(1)**2 + vperp(2)**2 + vperp(3)**2 )
            if (nrm > 0.0_WP) then
              vperp = vperp / nrm
            else
              return
            endif
        
            vother(1) = A(2)*vperp(3) - A(3)*vperp(2)
            vother(2) = A(3)*vperp(1) - A(1)*vperp(3)
            vother(3) = A(1)*vperp(2) - A(2)*vperp(1)
        
            nrm = sqrt( vother(1)**2 + vother(2)**2 + vother(3)**2 )
            if (nrm > 0.0_WP) vother = vother / nrm
        
            dir(:,2) = vperp
            dir(:,3) = vother
        
            return
          endif
        
          ! ==== CASE 2 : two eigenvalues are equal ====
          if (eq12 .and. .not.eq23) then
            nplane = dir(:,3)
            call enforce_plane_orientation(nplane, A, vperp, vother)
            if (vperp(1)**2 + vperp(2)**2 + vperp(3)**2 > 0.0_WP) then
              dir(:,1) = vperp
              dir(:,2) = vother
            endif
            return
          endif
        
          if (eq23 .and. .not.eq12) then
            nplane = dir(:,1)
            call enforce_plane_orientation(nplane, A, vperp, vother)
            if (vperp(1)**2 + vperp(2)**2 + vperp(3)**2 > 0.0_WP) then
              dir(:,2) = vperp
              dir(:,3) = vother
            endif
            return
          endif
        
          if (eq13 .and. .not.eq12) then
            nplane = dir(:,2)
            call enforce_plane_orientation(nplane, A, vperp, vother)
            if (vperp(1)**2 + vperp(2)**2 + vperp(3)**2 > 0.0_WP) then
              dir(:,1) = vperp
              dir(:,3) = vother
            endif
            return
          endif
        
        contains
        
!||====================================================================
!||    enforce_plane_orientation   ../engine/source/materials/mat/mat130/sigeps130.F90
!||--- called by ------------------------------------------------------
!||    fix_principal_dirs          ../engine/source/materials/mat/mat130/sigeps130.F90
!||====================================================================
          subroutine enforce_plane_orientation(nplane, A, vperp, vother)
            real(WP), intent(in)  :: nplane(3), A(3)
            real(WP), intent(out) :: vperp(3), vother(3)
            real(WP) :: n(3), cp(3), nrm, cosNA
          
            n   = nplane
            nrm = sqrt( n(1)**2 + n(2)**2 + n(3)**2 )
            if (nrm > 0.0_WP) n = n / nrm
          
            cosNA = abs( n(1)*A(1) + n(2)*A(2) + n(3)*A(3) )
            if (cosNA > 1.0_WP - 1.0e-8_WP) then
              vperp  = (/ 0.0_WP, 0.0_WP, 0.0_WP /)
              vother = (/ 0.0_WP, 0.0_WP, 0.0_WP /)
              return
            endif
          
            cp(1) = n(2)*A(3) - n(3)*A(2)
            cp(2) = n(3)*A(1) - n(1)*A(3)
            cp(3) = n(1)*A(2) - n(2)*A(1)
          
            nrm = sqrt( cp(1)**2 + cp(2)**2 + cp(3)**2 )
          
            if (nrm <= 1.0e-12_WP) then
              vperp  = (/ 0.0_WP, 0.0_WP, 0.0_WP /)
              vother = (/ 0.0_WP, 0.0_WP, 0.0_WP /)
              return
            endif
          
            vperp = cp / nrm
          
            vother(1) = n(2)*vperp(3) - n(3)*vperp(2)
            vother(2) = n(3)*vperp(1) - n(1)*vperp(3)
            vother(3) = n(1)*vperp(2) - n(2)*vperp(1)
          
            nrm = sqrt( vother(1)**2 + vother(2)**2 + vother(3)**2 )
            if (nrm > 0.0_WP) vother = vother / nrm
          end subroutine enforce_plane_orientation
        
        end subroutine fix_principal_dirs
!
      end module sigeps130_mod
