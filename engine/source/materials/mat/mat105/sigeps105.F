Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    sigeps105            ../engine/source/materials/mat/mat105/sigeps105.F
!||--- called by ------------------------------------------------------
!||    mulaw                ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    eosmain105           ../engine/source/materials/mat/mat105/sigeps105.F
!||    finter               ../engine/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||    compaction2_mod      ../common_source/eos/compaction2.F90
!||    compaction_mod       ../common_source/eos/compaction.F90
!||    compaction_tab_mod   ../common_source/eos/compaction_tab.F90
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    eos_param_mod        ../common_source/modules/mat_elem/eos_param_mod.F90
!||    eoslinear_mod        ../common_source/eos/eoslinear.F
!||====================================================================
      SUBROUTINE SIGEPS105 (
     1     NEL     ,NUPARAM ,NUVAR   ,NFUNC     ,IFUNC     ,TBURN     ,
     2     NPF     ,TF      ,TIME    ,TIMESTEP  ,UPARAM    ,BFRAC     ,
     3     RHO0    ,RHO     ,VOLUME  ,EINT      ,SIGY      ,DELTAX    ,
     5     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY    ,DEPSYZ    ,DEPSZX    ,
     7     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY    ,SIGOYZ    ,SIGOZX    ,
     8     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     9     SIGVXX  ,SIGVYY  ,SIGVZZ  ,SIGVXY    ,SIGVYZ    ,SIGVZX    ,
     A     SOUNDSP ,UVAR    ,OFF     ,SNPC      ,STF       ,MU_BAK    ,
     B     DVOL    ,PSH     ,IEOS    ,EOS_STRUCT,NVAREOS   ,VAREOS    ,
     C     NVARTMP ,VARTMP  ,DPLA    ,DEFP)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine computes Powder Burn EOS and is based on (Atwood,Friis,Moxnes) publications.
C ELEMENT BUFFER
C   %BFRAC  : is ignition fraction (depends on flamme position regarding the cell)
C   UVAR(6) : is burn fraction (depends on F(t) function and also on gas pressure and grains shape)
C EOS
C   GAS    : Exponantial EOS
C   POWDER : User EOS (Linear or Compaction or Compaction2 or Compaction_tab)
C MATERIAL BUFFER
C   UPARAM(1:NUPARAM) : material parameters
C   UVAR  (1:NUVAR)   : cell buffer specific for this material law : Pgas, etc...
C
C Shear modulus and yield strength are density dependent (user functions)
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE EOS_PARAM_MOD      , ONLY : EOS_PARAM_
        USE CONSTANT_MOD       , ONLY : ONE,TWO,THREE
        USE COMPACTION_MOD     , ONLY : COMPACTION     !  13    !  COMPACTION     ! 2019.1    !
        USE EOSLINEAR_MOD      , ONLY : EOSLINEAR      !  18    !  LINEAR         ! 2019.0    !
        USE COMPACTION2_MOD    , ONLY : COMPACTION2    !  20    !  COMPACTION2    ! 2025.1    !
        USE COMPACTION_TAB_MOD , ONLY : COMPACTION_TAB !  21    !  COMPACTION_TAB ! 2026.0    !
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR
      my_real
     .   TIME         ,TIMESTEP   ,UPARAM(NUPARAM),
     . SIGY(NEL) ,
     .   RHO(NEL)     ,RHO0(NEL)  ,VOLUME(NEL),
     .   EINT(NEL)    ,
     .   DVOL(NEL)    ,
     .   DEPSXX(NEL)  ,DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL)  ,DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL)  ,SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL)  ,SIGOYZ(NEL),SIGOZX(NEL)
      my_real,intent(inout) :: dpla(nel), defp(nel)
      my_real,intent(inout) :: MU_BAK(NEL)
      my_real,intent(inout) :: PSH(NEL)
      integer,intent(in) :: ieos
      type(eos_param_),intent(in) :: eos_struct !< data structure for EoS parameters
      INTEGER, INTENT(IN) :: NVAREOS !< number of additional variables for EOS
      my_real, INTENT(INOUT) :: VAREOS(NEL,NVAREOS) !< additional variables for EOS
      INTEGER ,INTENT(IN) :: NVARTMP                              !< NUMBER OF TEMPORARY INTERNAL VARIABLES
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP    !< TEMPORARY INTERNAL VARIABLES
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL), TBURN(NEL), BFRAC(NEL), DELTAX(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SNPC,STF
      INTEGER,INTENT(IN) :: NPF(SNPC), NFUNC, IFUNC(NFUNC)
      my_real TF(STF)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real,EXTERNAL :: FINTER
      ! EXTERNAL FINTER
      !      Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
      !      Y       : y = f(x)
      !      X       : x
      !      DYDX    : f'(x) = dy/dx
      !      IFUNC(J): FUNCTION INDEX
      !            J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
      !      NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: POLD(NEL), PNEW(NEL)
      my_real :: SSP
      my_real :: D, EG, Gr, C, ALPHA, C1
      my_real :: XL, DELTA_BF, BRATE
      my_real :: PG, PS(1), RHO_G, RHO_S
      my_real :: V_g, V_s
      my_real :: V_S_(NEL), RHO_S_(NEL), RHO_G_(NEL) ! backup arrays for sound speed calculation (P must be updated firt to estimate G(P))
      INTEGER :: ITER, MAX_ITER
      INTEGER :: func_shear, func_yield, func_burnrate ! shear modulus and yield strength functions for user defined functions
      INTEGER :: I
      my_real :: nu
      my_real :: TB, DIFF, M_G, M_S
      my_real :: compac
      my_real :: rhos0,rho_ref ! same meaning
      my_real :: mu_s(1), mu2_s(1), rho_si(1), rho_si0(1), dpdm_s(nel)
      my_real :: fP, dfP
      my_real :: M0, F
      my_real :: ALPHA_S, ALPHA_g, SSP_S2, SSP_G2
      my_real :: pmin
      my_real :: fscale_shear,fscale_yield,fscale_burnrate
      my_real :: xscale_shear,xscale_yield,xscale_burnrate
      my_real :: dav
      my_real :: J2, VM(NEL) ! von mises criterion
      my_real :: shear(nel), g2(nel) ! shear modulus (density dependent) and 2*shear modulus for CPU cost reduction
      my_real :: mu(nel) !< volumetric strain
      my_real :: mu2(nel) !< MU**2 IF MU>0 ; 0. otherwise
      my_real :: DPDM(nel),DPDE(nel) ! fake array
      my_real :: g0, ratio
      my_real :: PREF_REL_ERROR, TOL_ABS,TOL_REL, absfp
      LOGICAL :: debug
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      debug = .false.
      nu              = UPARAM(01)
      pmin            = UPARAM(02)
      fscale_shear    = UPARAM(03)
      fscale_yield    = UPARAM(04)
      D               = UPARAM(05)  ! exponential coefficient for gas EOS
      EG              = UPARAM(06)  ! exponential coefficient for gas EOS
      Gr              = UPARAM(07)  ! grain shape factor for burn rate
      C               = UPARAM(08)  ! exponent for burn rate regarding grain burn fraction F(t)
      ALPHA           = UPARAM(09)  ! exponent for burn rate regarding grain burn fraction F(t)
      C1              = UPARAM(10)  ! coefficient for ignition fraction regarding flamme position and time
      Xscale_shear    = UPARAM(11)  ! function scale factor
      fscale_burnrate = UPARAM(12)  ! function scale factor
      Xscale_yield    = UPARAM(13)  ! function scale factor
      Xscale_burnrate = UPARAM(14)  ! function scale factor
      func_burnrate   = IFUNC(1)    ! user function identifier
      func_shear      = IFUNC(2)    ! user function identifier
      func_yield      = IFUNC(3)    ! user function identifier
      compac = ZEP93 ! assumed initial compaction 0.93
      rhos0 = rho0(1)
      rho_ref = rho0(1)
      PSH(:) = EOS_STRUCT%PSH   ! optionnal pressure shift (to get in output relative pressure dP instead of total pressure P)
      PREF_REL_ERROR = rhos0*Eg*exp(rhos0/D)
      TOL_ABS = EM16
      TOL_REL = EM10*PREF_REL_ERROR

      ! --- PREVIOUS PRESSURE ---!
      do i=1,nel
        pold(i)  = -(sigoxx(i) + sigoyy(i) + sigozz(i))*third
      enddo

      ! --- SHEAR MODULUS ---!
      IF(func_shear > 0)then
        do i=1,nel
          shear(i) = FSCALE_SHEAR*FINTER(func_shear,RHO(i)/Xscale_shear,NPF,TF,DIFF)
        enddo
      else
        shear(1:NEL) = FSCALE_SHEAR
      endif
      G2(1:NEL)=TWO*shear(1:NEL)

      ! --- VOLUMETRIC STRAIN ---!
      DO I=1,NEL
        MU(I) = RHO(I)/RHO0(I) - ONE
      ENDDO
      DO I=1,NEL
        MU2(I) = MU(I)*MAX(ZERO,MU(I))
      ENDDO

      ! --- INITIALIZATION ---!
      ! initialization at time = 0 : set initial internal energy, pressure, density, burn fraction, etc...
      IF(DT1 == ZERO)THEN

        CALL EOSMAIN105(NEL, PMIN, OFF, EINT, MU, MU2, DVOL, PSH, PNEW,
     .                  DPDM, DPDE, EOS_STRUCT, MU_BAK, DT1, IEOS,
     .                  RHO, RHO0, NVAREOS, VAREOS, NPF, TF,
     .                  SNPC, STF, NVARTMP, VARTMP)

        DO I=1,NEL
          EINT(I)   = ZERO                      !no chemical release
          RHO(I)    = RHO0(I)                   !rho_ref
          UVAR(I,1) = RHO0(I)*VOLUME(I)         !M0
          UVAR(I,2) = ZERO                      !F
          UVAR(I,3) = RHO0(I)                   !RHO_S
          UVAR(I,4) = zero                      !RHO_G
          UVAR(I,5) = VOLUME(I)                 !V_s
          UVAR(I,6) = 0                         !V_g
        ENDDO

        SIGNXX(1:NEL) = -PNEW(1:NEL)  !PSH already included
        SIGNYY(1:NEL) = -PNEW(1:NEL)
        SIGNZZ(1:NEL) = -PNEW(1:NEL)
        SIGNXY(1:NEL) = ZERO
        SIGNYZ(1:NEL) = ZERO
        SIGNZX(1:NEL) = ZERO

        SIGVXX(1:NEL) = ZERO
        SIGVYY(1:NEL) = ZERO
        SIGVZZ(1:NEL) = ZERO
        SIGVXY(1:NEL) = ZERO
        SIGVYZ(1:NEL) = ZERO
        SIGVZX(1:NEL) = ZERO

        DO I=1,NEL
          SOUNDSP(I)= SQRT((DPDM(I)+TWO_THIRD*shear(i))/RHO0(I))
        ENDDO

      ENDIF

      !========================================================================
      !< POWDER/GAS EQUILIBRIUM SOLVING
      !========================================================================
      DO I=1,NEL
        !--------------------------------!
        ! INIT.                          !
        !--------------------------------!
        XL     = DELTAX(I)
        M0     = UVAR(I,1)
        F      = UVAR(I,2)
        RHO_S  = UVAR(I,3)
        RHO_G  = UVAR(I,4)
        V_s    = UVAR(I,5)
        V_g    = UVAR(I,6)

        !--------------------------------!
        ! IGNITION FRACTION              !
        !--------------------------------!
        !--> valid for const flamme velocity

        IF(BFRAC(I) < ONE) THEN
         TB = - TBURN(I)
         BFRAC(I) = ZERO
         IF(TIME > TB) BFRAC(I) = C1*(TIME-TB)/XL/THREE_HALF
         IF(BFRAC(I) > ONE) THEN
           BFRAC(I) = ONE
         ENDIF
        ENDIF
        ! BFRAC(I) = ONE  ! instantaneous ignition (infinite flamme velocity)

        !------------------------------------------------!
        ! BURN FRACTION                                  !
        !  BFRAC  : ignition fraction (flamme evolution) !
        !  UVAR(6): burn fraction (grain burn evolution) !
        !------------------------------------------------!
        IF(BFRAC(I) <= ZERO)THEN
          ! no ignition, no burn (so far)
          F = ZERO
          DELTA_BF = ZERO
        ELSEIF(F /= ONE)THEN
            ! F < 1  flamme has already reached the current element, but grain are still burning
            BRATE       = Fscale_burnrate*FINTER(func_burnrate,POLD(I)/Xscale_burnrate,NPF,TF,DIFF)
            DELTA_BF    = Gr*EXP(C*LOG((ONE-ALPHA*F)))*BRATE*TIMESTEP  !F_dot * DT
            DELTA_BF = MIN(DELTA_BF, ONE-F)
            F           = F + DELTA_BF
            F           = MAX(MIN(ONE,F),ZERO)
        ELSE
            ! complete burn, flamme has already reached the current element and grain are fully burned
            BFRAC(I) = ONE
            DELTA_BF = ZERO
        ENDIF

        !--------------------------------!
        ! EOS SOLVING                    !
        !--------------------------------!
        M_S = (ONE-F)*M0
        M_G = M0-M_S! mass of solid burned during the time step

        IF( F == ONE)THEN

           !trivial solving
            V_g   = VOLUME(I)
            V_s   = ZERO
            RHO_G = M_g / V_g      ! gas density in gas volume
            RHO_S = zero
            PNEW(I) = RHO_G*Eg*exp(RHO_G/D)
            dpdm_s(i) = ZERO


        ELSEIF (F == ZERO)THEN
          CALL EOSMAIN105(1, PMIN, OFF(I), EINT(I), MU(I), MU2(I), DVOL(I), PSH(I), PNEW(I),
     .                    DPDM(I), DPDE(I), EOS_STRUCT, MU_BAK(I), DT1, IEOS,
     .                    RHO(I), RHO0(I), NVAREOS, VAREOS(I,1:NVAREOS), NPF, TF,
     .                    SNPC, STF, NVARTMP, VARTMP(I,1:NVARTMP))
          V_g = (one-compac)*VOLUME(I)
          V_s = compac*VOLUME(I)
          RHO_S = rhos0/compac                ! intrinsic grain density (consistent with init & ELSE branch)
          RHO_G = ZERO                        ! no gas yet
          dpdm_s(i) = DPDM(I)
        ELSE

          ! numerical solving
          MAX_ITER = 20
          ITER = 0
          !--- solving : find rho_s such as Ps=Pg with Newton-Raphson method ---
          ! fixed point method with function F(rho_s) := Ps(rho_s) - Pg(rho_g(rho_s)) = 0
          !   Pg(rho_g) := rho_g*eg*exp(rho_g/D)
          !   Ps(rho_s) := from user EoS, solved by EOSMAIN105 subroutine
          !   rho_g(rho_s) = M_g/(VOLUME(I)-M_S/rho_s)
          !   derivative : Ps'(rho_s) : provided by user EoS (EOSMAIN105) dpdm / rho0
          !   derivative : Pg'(rho_s) = dPg/drho_g * drho_g/drho_s = (eg*exp(rho_g/D) + rho_g*eg/D*exp(rho_g/D)) * M_S/(VOLUME(I)-M_S/rho_s)**2
          ! Consequently , iterations are :
          !  rho_s[k+1] = rho_s[k] - F(rho_s[k])/F'(rho_s[k])

          !  rho_s is initialized from previous cycle.

          rho_si0(1) = rhos0/compac                  ! intrinsic grain density at P=0 (EoS reference)

          DO WHILE (ITER < MAX_ITER)

           ! --- solid state : compression of the GRAIN material ---
           MU_S(1)   = RHO_S/rho_si0(1) - ONE
           MU2_S(1)  = MU_S(1)*MAX(ZERO,MU_S(1))
           rho_si(1) = RHO_S
           CALL EOSMAIN105(1, PMIN, OFF(I), EINT(I), MU_S(1), MU2_S(1), DVOL(I), PSH(I), Ps(1),
     .                     DPDM_s(I), DPDE(I), EOS_STRUCT, MU_BAK(I), DT1, IEOS,
     .                     RHO_Si(1), rho_si0(1), NVAREOS, VAREOS(I,1:NVAREOS), NPF, TF,
     .                     SNPC, STF, NVARTMP, VARTMP(I,1:NVARTMP))

            ! Volume occupied by the solid
            V_s = M_S / RHO_S
            V_s = MIN(V_s, VOLUME(I)*(ONE-EM10)) ! safety limit

            ! Volume occupied by the gas : the gas fills ONLY V_g
            V_g = VOLUME(I) - V_s
            V_g = MAX(EM20, V_g) ! safety limit

            ! Gas pressure : gas density in the GAS volume (not the whole cell)
            RHO_G = M_g / V_g
            Pg    = RHO_G*Eg*exp(RHO_G/D)

            ! Newton on  fP(rho_s) = Ps(rho_s) - Pg(rho_g(rho_s)) = 0
            fP    = Ps(1) - Pg
            absfp = ABS(fP)
            IF (absfp <= TOL_ABS .OR. absfp <= TOL_REL) EXIT

            ! drho_g/drho_s < 0 (compressing the solid frees volume for the gas)
            dfP = dpdm_s(I)/rho_si0(1) + eg*exp(RHO_G/D)*(one+RHO_G/D)*( M_g*M_S/(RHO_S*RHO_S*V_g*V_g) )
            RHO_S = RHO_S - fP/dfP
            RHO_S = MAX(RHO_S, EM20)                   ! keep the density positive
            ITER  = ITER + 1

          END DO

          PNEW(I) = Pg

        END IF



        !--------------------------------!
        ! Cell Internal Energy           !
        !--------------------------------!
        ! total energy for output (mixture, linear eos does not depend on energy)
        EINT(I)= EINT(I) - HALF*(PNEW(I)+POLD(I))*DVOL(I)  ! mechanical work due to volume change of the full cell
        EINT(I) = EINT(I)/MAX(EM20,VOLUME(I))  ! J/m3 for output

        PNEW(I) = PNEW(I) - PSH(I)

        V_S_(I) = V_s
        RHO_S_(I) = RHO_S
        RHO_G_(I) = RHO_G

        !--------------------------------!
        ! Returning values               !
        !--------------------------------!     
        SIGNXX(I)  = -PNEW(I)
        SIGNYY(I)  = -PNEW(I)
        SIGNZZ(I)  = -PNEW(I)

        SIGVXX(I)  = ZERO
        SIGVYY(I)  = ZERO
        SIGVZZ(I)  = ZERO
        SIGVXY(I)  = ZERO
        SIGVYZ(I)  = ZERO
        SIGVZX(I)  = ZERO
        
        ! M0     = UVAR(I,1)
        UVAR(I,2) = F
        UVAR(I,3) = RHO_S
        UVAR(I,4) = RHO_G
        UVAR(I,5) = V_s
        UVAR(I,6) = V_g
       
        ! --- verification output
        if(debug)then
          if(time == ZERO)then
            open(UNIT=111,FILE='F.txt')
            open(UNIT=112,FILE='Pg.txt')
            open(UNIT=113,FILE='rhoS.txt')
            open(UNIT=114,FILE='Ifrac.txt')
            open(UNIT=115,FILE='rhog_mean.txt')
            open(UNIT=116,FILE='rhog.txt')
            open(UNIT=117,FILE='dpdm.txt')
            open(UNIT=118,FILE='vg.txt')
            open(UNIT=119,FILE='vs.txt')
            open(UNIT=120,FILE='mg.txt')
            open(UNIT=121,FILE='ms.txt')
          else
            open(UNIT=111,FILE='F.txt'     ,position="append")
            open(UNIT=112,FILE='Pg.txt'    ,position="append")
            open(UNIT=113,FILE='rhoS.txt'  ,position="append")
            open(UNIT=114,FILE='Ifrac.txt' ,position="append")
            open(UNIT=115,FILE='rhog_m.txt',position="append")
            open(UNIT=116,FILE='rhog.txt'  ,position="append")
            open(UNIT=117,FILE='dpdm.txt'  ,position="append")
            open(UNIT=118,FILE='vg.txt'    ,position="append")
            open(UNIT=119,FILE='vs.txt'    ,position="append")
            open(UNIT=120,FILE='mg.txt'    ,position="append")
            open(UNIT=121,FILE='ms.txt'    ,position="append")
          endif
          WRITE(111,*)TIME,F
          WRITE(112,*)TIME,PNEW(I)
          WRITE(113,*)TIME,M_s/VOLUME(I) !RHO_S/compac    !test 4.4 shows it is M_s/VOLUME(I)
          WRITE(114,*)BFRAC(I)
          WRITE(115,*)TIME,M_g/VOLUME(I)
          WRITE(116,*)TIME,RHO_G
          WRITE(117,*)TIME,DPDM(I)
          WRITE(118,*)TIME,V_G
          WRITE(119,*)TIME,V_S
          WRITE(120,*)TIME,M_G
          WRITE(121,*)TIME,M_S
          CLOSE(111)
          CLOSE(112)
          CLOSE(113)
          CLOSE(114)
          close(115)
          CLOSE(116)
          close(117)
          CLOSE(118)
          CLOSE(119)
          CLOSE(120)
          CLOSE(121)
        end if !debug

      ENDDO! next I


      ! --- ELASTIC INCREMENT ---!
      do i=1,nel
        dav       =  (depsxx(i) + depsyy(i) + depszz(i))*third
        signxx(i) = sigoxx(i) + half*(pold(i)+pnew(i)) + g2(i)*(depsxx(i)-dav)
        signyy(i) = sigoyy(i) + half*(pold(i)+pnew(i)) + g2(i)*(depsyy(i)-dav)
        signzz(i) = sigozz(i) + half*(pold(i)+pnew(i)) + g2(i)*(depszz(i)-dav)
        signxy(i) = sigoxy(i) + shear(i)*depsxy(i)
        signyz(i) = sigoyz(i) + shear(i)*depsyz(i)
        signzx(i) = sigozx(i) + shear(i)*depszx(i)
        j2        = half*(signxx(i)**2+signyy(i)**2+signzz(i)**2) + signxy(i)**2+signyz(i)**2+signzx(i)**2
        vm(i)     = sqrt(three*j2)
      end do


      ! --- YIELD FUNCTION Y=Y(P) ---!
      do i=1,nel
        sigy(i) = FSCALE_Yield*FINTER(func_yield,PNEW(I)/Xscale_yield,NPF,TF,DIFF)
        g0 = sigy(i)
        if(g0 < ep20)then
          g0 = max(zero,g0)
          if( pnew(i) <= pmin ) g0 = zero
          ratio = one
          if(g0 == zero)then
            ratio = zero
          else if( vm(i) > g0 )then     ! at the yield surface or within the yield surface
            ratio = g0/vm(i)
            dpla(i) = (one-ratio)*vm(i) / max(em20,three*shear(i))
            defp(i) = defp(i) + dpla(i)
          end if
          ! projection
          signxx(i) = ratio*signxx(i)
          signyy(i) = ratio*signyy(i)
          signzz(i) = ratio*signzz(i)
          signxy(i) = ratio*signxy(i)
          signyz(i) = ratio*signyz(i)
          signzx(i) = ratio*signzx(i)
          ! hydrostatic part
          signxx(i) = signxx(i) - pnew(i)
          signyy(i) = signyy(i) - pnew(i)
          signzz(i) = signzz(i) - pnew(i)
        endif
      end do

      ! --- GLOBAL PRESSURE & SOUND SPEED ---!
      DO I=1,NEL
        SSP = ZERO
        M0  = UVAR(I,1)
        F   = UVAR(I,2)
        M_S = (ONE-F)*M0
        M_G = M0 - M_S
        ALPHA_S = one-F
        IF(RHO_S_(I) > ZERO)THEN
          SSP_S2 = (DPDM_s(i)+TWO_THIRD*G2(I))/RHO_REF
          SSP = ALPHA_S/(M_s/VOLUME(I))/SSP_S2
        ENDIF
        IF(RHO_G_(I) > ZERO)THEN
          SSP_G2 = PNEW(I)/((M_g/VOLUME(I)))*(one+RHO_G_(I)/D)
          ALPHA_g = ONE - ALPHA_s
          SSP = SSP + ALPHA_g/(M_g/VOLUME(I))/SSP_G2
        ENDIF
        SSP = MAX(SSP, EM20)
        SSP = ONE/SSP/RHO(I)  ! Wood's formula for sound speed in a mixture
        SSP = SQRT(SSP)
        SOUNDSP(I) = SSP
      ENDDO

C-----------------------------------------------
      RETURN
C-----------------------------------------------
      END SUBROUTINE SIGEPS105
C-----------------------------------------------

!||====================================================================
!||    eosmain105           ../engine/source/materials/mat/mat105/sigeps105.F
!||--- called by ------------------------------------------------------
!||    sigeps105            ../engine/source/materials/mat/mat105/sigeps105.F
!||--- calls      -----------------------------------------------------
!||    compaction           ../common_source/eos/compaction.F90
!||    compaction2          ../common_source/eos/compaction2.F90
!||    compaction_tab       ../common_source/eos/compaction_tab.F90
!||    eoslinear            ../common_source/eos/eoslinear.F
!||--- uses       -----------------------------------------------------
!||    compaction2_mod      ../common_source/eos/compaction2.F90
!||    compaction_mod       ../common_source/eos/compaction.F90
!||    compaction_tab_mod   ../common_source/eos/compaction_tab.F90
!||    eos_param_mod        ../common_source/modules/mat_elem/eos_param_mod.F90
!||    eoslinear_mod        ../common_source/eos/eoslinear.F
!||====================================================================
      SUBROUTINE EOSMAIN105(
     1     NEL, PMIN, OFF, EINT, MU, MU2, DVOL, PSH, PNEW, DPDM, DPDE,
     2     EOS_STRUCT, MU_BAK, DT1, IEOS, RHO, RHO0, NVAREOS, VAREOS,
     3     NPF, TF, SNPC, STF, NVARTMP, VARTMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE EOS_PARAM_MOD      , ONLY : eos_param_
        USE COMPACTION_MOD     , ONLY : COMPACTION
        USE COMPACTION2_MOD    , ONLY : COMPACTION2
        USE COMPACTION_TAB_MOD , ONLY : COMPACTION_TAB
        USE EOSLINEAR_MOD      , ONLY : EOSLINEAR
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real, INTENT(IN) :: PMIN, OFF(NEL), MU(NEL), MU2(NEL), DVOL(NEL)
      my_real, INTENT(INOUT) :: EINT(NEL), PSH(NEL), PNEW(NEL)
      my_real, INTENT(INOUT) :: DPDM(NEL), DPDE(NEL)
      my_real, INTENT(INOUT) :: MU_BAK(NEL)
      my_real,INTENT(IN) :: DT1
      TYPE(eos_param_), INTENT(IN) :: EOS_STRUCT
      INTEGER, INTENT(IN) :: IEOS
      my_real, INTENT(IN) :: RHO(NEL), RHO0(NEL)
      INTEGER, INTENT(IN) :: NVAREOS, SNPC, STF, NVARTMP
      my_real, INTENT(INOUT) :: VAREOS(NEL,NVAREOS)
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF)
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: BULK, P0, C0, VNEW(NEL)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      VNEW(1:NEL) = ONE !fake array foe EOSLINEAR

        IF(IEOS == 18)THEN
          !LINEAR EOS
          BULK = EOS_STRUCT%UPARAM(2)
          P0   = EOS_STRUCT%UPARAM(1) !P0-PSH
          C0   = P0 + PSH(1)
          CALL EOSLINEAR(2    ,NEL       ,PMIN ,OFF  ,EINT ,MU  ,DVOL   ,VNEW   ,PSH    ,PNEW  ,DPDM,
     .                   DPDE ,EOS_STRUCT, C0  ,BULK)

        ELSEIF(IEOS == 13)THEN
          !COMPACTION
          CALL COMPACTION(2, NEL, PMIN, OFF, EINT, MU, MU2, DVOL, PSH, PNEW, DPDM, DPDE, MU_BAK, EOS_STRUCT)

        ELSEIF(IEOS == 20)THEN
          !COMPACTION2
          CALL COMPACTION2( 2    ,NEL  ,PMIN ,OFF     ,EINT    ,MU  ,DVOL ,PSH  ,
     .                      PNEW ,DPDM ,DPDE ,NVAREOS ,VAREOS ,NPF  ,TF   ,SNPC ,STF ,EOS_STRUCT)

        ELSEIF(IEOS == 21)THEN
          !COMPACTION_TAB
          CALL COMPACTION_TAB( 2    ,NEL  ,PMIN ,OFF     ,EINT   ,DVOL    ,PSH    ,DT1        ,RHO   ,RHO0 ,
     .                        PNEW  ,DPDM ,DPDE ,NVAREOS ,VAREOS ,NVARTMP ,VARTMP ,EOS_STRUCT)
        ENDIF

C-----------------------------------------------
      RETURN
C-----------------------------------------------
      END SUBROUTINE EOSMAIN105
C-----------------------------------------------

