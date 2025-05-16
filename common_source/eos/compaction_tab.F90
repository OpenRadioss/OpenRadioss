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
      !||    compaction_tab_mod   ../common_source/eos/compaction_tab.F90
      !||--- called by ------------------------------------------------------
      !||    eosmain              ../common_source/eos/eosmain.F
      !||====================================================================
      module compaction_tab_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
! This subroutine contains numerical solving of eos_compaction_tab
!----------------------------------------------------------------------------
! STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
! COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!
!  STAGGERED SCHEME
!     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!    This Eos does not depends on Energy but there is also the two pass in EOSMAIN (for the staggered scheme)
!      The second pass (iflg=1) is then only an update with work of pressure force.
!
!  COLLOCATED SCHEME
!     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------
      !||====================================================================
      !||    compaction_tab          ../common_source/eos/compaction_tab.F90
      !||--- called by ------------------------------------------------------
      !||    eosmain                 ../common_source/eos/eosmain.F
      !||--- calls      -----------------------------------------------------
      !||    compaction_tab_init     ../common_source/eos/compaction_tab.F90
      !||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod            ../common_source/modules/constant_mod.F
      !||    eos_param_mod           ../common_source/modules/mat_elem/eos_param_mod.F90
      !||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
      subroutine compaction_tab(&
                            iflag , nel   , pm     , off    , eint   , &
                            dvol  , mat   , psh    , dt1    , rho    , rho0  , &
                            pnew  , dpdm  , dpde   , rho_bak, &
                            npropm, nummat, nvareos, vareos , nvartmp, vartmp, &
                            eos_param)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   C o m m e n t s
! ----------------------------------------------------------------------------------------------------------------------
!! \brief  Tabulated Compaction EoS
!! \brief  ------------------------
!! \details  - unloading path is defined as following : for a given density rho, find density lambda such as c(lambda)**2.(rho-lambda) = Pc(rho)
!! \details  this is intersection point between compaction curve and straight line with slope c(lambda)**2
!! \details
!! \details   - gamma factor allows to shape the unloading path (non linearity)
!! \details
!! \details   - Buffer is used as following
!! \details                 ! vareos(I,1) is min(lambda, rho_tmd)
!! \details                 ! vareos(I,2) is c**2
!! \details                 ! vareos(I,3) is Pnew
!! \details                 ! vareos(I,4) is gamma
!! \details                 ! vareos(I,5) is Pc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
       use constant_mod , only : zero, em10, em06, half, one, two, ep03, ep20
       use eos_param_mod , only : eos_param_
       use table_mat_vinterp_mod , only : table_mat_vinterp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
       implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nvareos
      my_real,intent(inout) :: vareos(nel,nvareos)
      integer,intent(in) :: nel !< number of element in the currenbt group
      integer,intent(in) :: npropm, nummat !< array sizes
      integer,intent(in) :: mat(nel), iflag
      my_real,intent(inout) :: pm(npropm,nummat) !< material data (real parameters)
      my_real,intent(inout) :: off(nel),eint(nel),dvol(nel)
      my_real,intent(inout) :: pnew(nel),dpdm(nel),dpde(nel)
      type(eos_param_),intent(in) :: eos_param !< data structure for EoS parameters
      my_real,intent(inout) :: rho_bak(nel) !< backup of mu for unloading
      my_real,intent(in) :: dt1 !< time step
      my_real,intent(in) :: rho(nel)  !< current density
      my_real,intent(in) :: rho0(nel) !< initial density
      integer ,intent(in) :: nvartmp                       !< size for vartmp
      integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< vartmp is the history of index position on the user curve (optimization in order not to loop from first point at each cycle)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      integer i,mx,iter,niter
      integer :: iform
      integer :: plasexp
      my_real :: residu, ff, df
      my_real :: p0,psh(nel)
      my_real :: p(nel)
      my_real :: rho_tmd
      my_real :: gamma_tmd
      my_real :: c_solid
      my_real :: lambda
      my_real :: tol
      my_real :: pmin
      my_real :: rhomax_plastic
      my_real :: cunl(nel), c_prime(nel)
      my_real :: Pc(nel)
      my_real :: dpdr(nel)
      my_real :: gamma(nel), gl, rhol, rhol_, c2
      my_real :: b(nel)
      my_real :: tmp1,tmp2, xx
      my_real, dimension(nel,1) :: xvec1 !<temporary array for table interpolation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
       mx             = mat(1)
       psh(1:nel)     = pm(88,mx)
       p0             = pm(31,mx)
       pmin           = pm(37,mx)
       rho_tmd        = eos_param%uparam(1)
       c_solid        = eos_param%uparam(2)
       psh            = eos_param%uparam(3)
       rhomax_plastic = eos_param%uparam(4)
       gamma_tmd      = eos_param%uparam(5)
       iform          = eos_param%iparam(1)
       plasexp        = eos_param%iparam(2)

       niter = 20
       tol = em06
       IF(DT1 == zero) call compaction_tab_init(eos_param,nel,nvartmp,vartmp,nvareos,vareos,rho,rho_tmd) ! initial condition : set vereos(1:nel,:)

       if(iflag /= 1)then

         do i=1,nel

           if(rho(i) > rho_bak(i) .and. rho(i) < rhomax_plastic)then
           ! --- path along compaction curve ---------------------------------------------------------------------------

               ! Read pressure on compaction Curve
               xvec1(1:1,1) = rho(i)
               call table_mat_vinterp(eos_param%table(1),1,1,vartmp(1,1),xvec1,Pc,dPdr)  ! get Pc(xvec1) for current elem
               P(i) = Pc(1)
               P(i) = max(P(i), Pmin)
               vareos(I,5) = Pc(1)  ! Pc

               ! Update lambda  (find lambda such as unloading path interesct compaction curve)
               !   and c_unl(lambda)
               iter = 0
               residu = ep20
               lambda=vareos(i,1)
               do while(iter <= niter .and. residu > tol)
                 xvec1(1:1,1) = lambda
                 call table_mat_vinterp(eos_param%table(2),1,1,vartmp(1,2),xvec1,cunl,c_prime) ! get C_unload(labmda)
                 FF = P(i) - cunl(1)*cunl(1) * (rho(i)-lambda)
                 DF = two * cunl(1) * c_prime(1) * (lambda-rho(i)) + cunl(1)*cunl(1)
                 LAMBDA = LAMBDA - FF / DF
                 residu = abs(FF)/RHO_TMD
                 iter = iter + 1
               enddo
               vareos(I,1) = min(lambda, rho_tmd)       ! lambda
               vareos(I,2) = cunl(1)*cunl(1)            ! c(lambda)**2

               !non linear formulation only : upodate gamma value
               if(iform == 2)then
                  xvec1(1:1,1) = lambda
                  call table_mat_vinterp(eos_param%table(3),1,1,vartmp(1,3),xvec1,gamma,dPdr)  !slope not used
                  gamma(1) = min(max(em10,gamma(1)), ep03)
                  vareos(I,4) =  gamma(1)! gamma
               end if

               !for sound speed
               dpdm(i) = rho0(i)*dPdr(i)   ! total derivative dP/d(mu) = rho0*dP/d(rho)

               ! History variable: density at current plastic state
               rho_bak(i) = min(rhomax_plastic,rho(i))  ! history rho_plastic

           else
           ! --- unloading path (or P=Pmin path)------------------------------------------------------------------------

               if(rho(i) > rhomax_plastic)then
                 vareos(i,1) = rho_tmd
                 vareos(i,2) = c_solid*c_solid
                 vareos(I,4) = gamma_tmd
               end if
               lambda = vareos(1,i)

               gl = vareos(i,4)
               if(iform == 1 .or. gl < em10)then
                 !linear unloading gamma=0
                 b(i) = vareos(i,2)
                 P(i) = b(i)*(rho(i)-lambda)
                 P(i) = max(P(i), Pmin)
                 dpdm(i) = rho0(i)*b(i)
               else
                 !non-linear unloading
                 c2 = vareos(i,2)
                 Pc = vareos(i,5)
                 rhol = Pc(1)/c2
                 rhol_ = abs(Pmin/c2) ! left size in case Pmin /= 0.0
                 tmp1 = (Pc(1)-Pmin)/(exp(gl)-one)
                 tmp2 = exp(gl/(rhol+rhol_)*(max(zero,rho(i)-lambda+rhol_)))
                 P(i) = Pmin+tmp1*(tmp2-one)
                 P(i) = max(P(i), Pmin)
                 b(i) = tmp1*tmp2 * gl/rhol
                 b(i) = max(vareos(i,2), b(i)) ! upper bound for non linear unloading
                 dpdm(i) = rho0(i)*b(i)
               endif

               ! --- OPTIONNAL : PLASEXP == 1 update re-loading path for next cycle (if needed) ---!
               ! Sand will be considered as virgin sand
               if (plasexp == 1 .and. p(i) == pmin)then
                 vareos(i,1)=min(rho(i),rho_tmd)
                 lambda = vareos(i,1)
                 xvec1(1:1,1) = lambda
                 call table_mat_vinterp(eos_param%table(2),1,1,vartmp(1,2),xvec1,cunl,c_prime) ! C_unload(lambda)
                 vareos(i,2)=cunl(1)*cunl(1)
                 xvec1(1:1,1) = lambda
                 call table_mat_vinterp(eos_param%table(3),1,1,vartmp(1,3),xvec1,gamma,c_prime) ! gamma(lambda)
                 vareos(i,4) = gamma(1)
                 if(iform==2)then
                   ! update pc value
                   !intersection line to find new Pc value
                   !  Pc is used to determine variable rhol which is required to build the unloading path
                   !  (This may be optimized by building a function Pc(lambda) during starter with the union on sets x->Pc(x) and x->c_unl(x)
                   iter = 0
                   residu = ep20
                   xx = lambda
                   do while(iter <= niter .and. residu > tol)
                     xvec1(1:1,1) = xx
                     call table_mat_vinterp(eos_param%table(1),1,1,vartmp(1,1),xvec1,Pc,dPdr) ! provides cunl=c(x) and c_prime=c'(x)
                     FF = Pc(1) - cunl(1)*cunl(1)*(xx-lambda)
                     DF = dPdr(1) - cunl(1)*cunl(1)
                     xx = xx - FF / DF
                     residu = abs(FF)/RHO_TMD
                     iter = iter + 1
                   enddo
                   vareos(i,5) = Pc(1)
                 endif
                 rho_bak(i) = xx
               end if

           endif
         enddo !next i (1:nel)

         dpde(1:nel) = zero ! not energy depedent
         pnew(1:nel) = p(1:nel)*off(1:nel)-psh(1:nel) ! can then be used later for constitutive laws
         VAREOS(1:nel,3) = Pnew(1:nel) ! do not recompute everything for second call of EOSMAIN with iflag=1


      elseif(iflag == 1) THEN

        ! internal energy updated with work of pressure forces
        Pnew(1:nel) = VAREOS(1:nel,3) ! pressure already calculated (independent from energy since dpde=0, nothing more to do)
        do i=1,nel
          eint(i) = eint(i) - half*dvol(i)*(pnew(i)+psh(i) ) ! Second part of the parallelogram integration using Pnew only (Pold contribution already integrated)
        enddo !next i

      endif

!------------------------
      return
      end subroutine compaction_tab
!------------------------




      !||====================================================================
      !||    compaction_tab_init     ../common_source/eos/compaction_tab.F90
      !||--- called by ------------------------------------------------------
      !||    compaction_tab          ../common_source/eos/compaction_tab.F90
      !||--- calls      -----------------------------------------------------
      !||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod            ../common_source/modules/constant_mod.F
      !||    eos_param_mod           ../common_source/modules/mat_elem/eos_param_mod.F90
      !||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
      subroutine compaction_tab_init(eos_param,nel,nvartmp,vartmp,nvareos,vareos,rho,rho_tmd)
!! \brief  This subroutine initialize VAREOS aray for initial time 0.0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
       use constant_mod , only : zero, em10, half, one, two, ep20
       use eos_param_mod , only : eos_param_
       use table_mat_vinterp_mod , only : table_mat_vinterp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
       implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nvareos
      my_real,intent(inout) :: vareos(nel,nvareos)
      integer,intent(in) :: nel !< number of element in the currenbt group
      integer ,intent(in) :: nvartmp                       !< size for vartmp
      integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< vartmp is the history of index position on the user curve (optimization in order not to loop from first point at each cycle)
      type(eos_param_),intent(in) :: eos_param !< data structure for EoS parameters
      my_real,intent(in) :: rho(nel),rho_tmd
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variable
! ----------------------------------------------------------------------------------------------------------------------
      my_real :: Pc(nel),dPdr(nel),Cunl(nel),slope(nel)
      my_real :: residu,lambda,ff,df,tol
      my_real, dimension(nel,1) :: xvec1 !<temporary array for table interpolation
      integer :: i, iter,niter
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
         niter = 10
         tol = em10

         !The current unloading speed-of-sound velocity
         xvec1(1:nel,1) = rho(1:nel)
         call table_mat_vinterp(eos_param%table(2),nel,nel,vartmp(1,2),xvec1,cunl,slope)
         VAREOS(1:nel,2) = Cunl(1:nel)*Cunl(1:nel)

         !solve the zero-crossing of the current unloading
         call table_mat_vinterp(eos_param%table(1),nel,nel,vartmp(1,1),xvec1,Pc,dPdr) !The current compaction pressure
          DO I=1,NEL
            IF(Pc(i) == zero)THEN
              VAREOS(I,1) = min(RHO(I),rho_tmd) ! LAMBDA
            ELSE
              iter = 0
              residu = ep20
              lambda = rho(i)
              do while(iter <= niter .and. residu > tol)
                xvec1(1:1,1) = lambda
                call table_mat_vinterp(eos_param%table(1),1,1,vartmp(1,1),xvec1,Pc,dPdr) ! provides cunl=c(x) and c_prime=c'(x)
                FF = Pc(1)
                DF = dPdr(1)
                LAMBDA = LAMBDA - FF / DF
                residu = abs(FF)/RHO_TMD
                iter = iter + 1
              enddo
              VAREOS(I,1) = min(lambda,rho_tmd)
            END IF
            VAREOS(I,5) = zero
          ENDDO

          VAREOS(1:nel,3) = Pc(1:nel)

      end subroutine compaction_tab_init


      end module compaction_tab_mod
