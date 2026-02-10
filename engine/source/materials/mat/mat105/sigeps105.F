Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    sigeps105        ../engine/source/materials/mat/mat105/sigeps105.F
!||--- called by ------------------------------------------------------
!||    mulaw            ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    finter           ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE SIGEPS105 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC    ,TBURN  ,
     2     NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM   ,BFRAC  ,
     3     RHO0   ,RHO    ,VOLUME  ,EINT    ,SIGY     ,DELTAX ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NFT      ,V      ,
     B     W      ,X      ,IX      ,NIX      ,JTHE   ,
     C     GEO    ,PID    ,ILAY    ,NG      ,PM     ,
     D     IPARG  ,BUFVOIS ,IPM    ,BUFMAT  ,STIFN    ,
     E     VD2    ,VDX    ,VDY     ,VDZ     ,MAT      ,VOLN   ,
     F     QNEW   ,DVOL  ,QOLD    ,PSH)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine computes Powder Burn EOS and is based on (Atwood,Friis,Moxnes) publication.
C ELEMENT BUFFER
C   %BFRAC  : is ignition fraction (depends on flamme position regarding the cell)
C   UVAR(6) : is burn fraction (depends on F(t) function and also on gas pressure and grains shape)
C EOS
C   GAS    : Exponantial EOS
C   POWDER : Linear EOS
C MATERIAL BUFFER
C   UPARAM(1:NUPARAM) : material parameters
C   UVAR  (1:NUVAR)   : cell buffer specific for this material law : Pgas, etc...
C
C An Alternative implementation is to use /EOS option : see ./common_source/eos/powder_burn.F
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
!
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NFT,NIX,JTHE,
     .        IX(NIX,*), PID(*), ILAY, NG,PM(NPROPM,*),IPARG(*),MAT(*),
     .        IPM(NPROPMI,*)
      my_real 
     .   TIME         ,TIMESTEP   ,UPARAM(NUPARAM),
     . SIGY(NEL) ,
     .   RHO(NEL)     ,RHO0(NEL)  ,VOLUME(NEL),
     .   EINT(NEL)    ,BUFVOIS(*) ,QNEW(NEL)    ,
     .   DVOL(NEL)    ,QOLD(NEL)  ,
     .   EPSPXX(NEL)  ,EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL)  ,EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL)  ,DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL)  ,DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL)   ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL)   ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL)  ,SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL)  ,SIGOYZ(NEL),SIGOZX(NEL),
     .   V(3,*)       ,W(3,*)     ,X(3,*)     ,
     .   GEO(NPROPG,*),BUFMAT(*)  ,
     .   STIFN(MVSIZ) ,VD2(*)     ,VDX(*)     ,
     .   VDY(*)       ,VDZ(*)     ,SSP        ,
     .   VOLN(*)
      my_real,intent(inout) :: PSH(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL), TBURN(NEL), BFRAC(NEL), DELTAX(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real TF(*)
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
      my_real :: POLD, EINC, PNEW
      my_real :: BULK, D, EG, EG_PROD, Gr, C, ALPHA, C1, C2, FSCALE_G, FSCALE_B, FSCALE_P, FSCALE_RHO
      my_real :: P0, XL, ESPE, TOTAL_BFRAC, DELTA_BF, BRATE
      my_real :: PG, PG_, PS, RHO_G, RHO_S, RHO_Si
      my_real :: E ! rho0.e or EINT/V0 where V0 such as rho0.v0 = rho.v ;   EINT = rho.e.V = rho/rho0 * E.V
      my_real :: V_g, V_s, V_g_prev
      INTEGER :: funcb, funcg
      INTEGER :: I
      my_real :: MASS, TB, DIFF, SSP_G, SSP_S, M_G, M_S
      my_real :: compac
      my_real :: rhos0,rho_ref ! same meaning
      my_real :: mu_s
      my_real :: EINT_g ! Eintg = eg.Mg
      my_real :: fP, dfP, Pk, RESIDUE
      my_real :: M0, P, F, dEINT_chem, dEint_W, dM_s, dV_g
      my_real :: ALPHa_s, ALPHA_g, SSP_S2, SSP_G2
      INTEGER :: ITER, MAX_ITER


C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      BULK       = UPARAM(01)  ! solid bulk modulus for linear EoS
      P0         = UPARAM(02)  ! initial pressure
      PSH(:)     = UPARAM(03)  ! optionnal pressure shift (to get in output relative pressure dP instead of total pressure P)
      !unused      UPARAM(04)
      D          = UPARAM(05)  ! exponential coefficient for gas EOS
      EG_PROD    = UPARAM(06)  ! exponential coefficient for gas EOS
      Gr         = UPARAM(07)  ! grain shape factor for burn rate
      C          = UPARAM(08)  ! exponent for burn rate regarding grain burn fraction F(t)
      ALPHA      = UPARAM(09)  ! exponent for burn rate regarding grain burn fraction F(t)
      C1         = UPARAM(10)  ! coefficient for ignition fraction regarding flamme position and time
      C2         = UPARAM(11)  ! coefficient for ignition fraction regarding flamme position and time
      FSCALE_G   = UPARAM(12)  ! function scale factor
      FSCALE_B   = UPARAM(13)  ! function scale factor
      FSCALE_P   = UPARAM(14)  ! function scale factor
      FSCALE_RHO = UPARAM(15)  ! function scale factorl
      funcb      = IFUNC(1)    ! user function identifier
      funcg      = IFUNC(2)    ! user function identifier
      
      compac = 0.93 ! assumed initial compaction
      rhos0 = rho0(1)
      rho_ref = rho0(1)
                 
      ! initialization at time = 0 : set initial internal energy, pressure, density, burn fraction, etc...
      IF(DT1 == ZERO)THEN
        DO I=1,NEL
          EINT(I)   = ZERO
          RHO(I)    = compac*RHO0(I)
          UVAR(I,1) = RHO0(I)*compac*VOLUME(I)  !M0   !rho0 is rhos(0)=rho_ref  not rho(0)=compac*rho_ref
          UVAR(I,2) = ZERO                      !F
          UVAR(I,3) = RHO0(I)                   !RHO_S
          UVAR(I,4) = zero                      !RHO_G
          UVAR(I,5) = compac*VOLUME(I)          !V_s
          UVAR(I,6) = ZERO                      !V_g
          UVAR(I,7) = P0                        !P (total pressure, not relative pressure)
          UVAR(I,8) = ZERO                      !EINTg = eg.M_g
        ENDDO
        SIGNXX(1:NEL) = -(P0-PSH(1:NEL))
        SIGNYY(1:NEL) = -(P0-PSH(1:NEL))
        SIGNZZ(1:NEL) = -(P0-PSH(1:NEL))
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
          SOUNDSP(I)= SQRT(BULK/RHO0(I))
        ENDDO
      ENDIF

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
        POLD   = UVAR(I,7)
        EINT_g = UVAR(I,8)
        V_g_prev = V_g
                        
        !--------------------------------!
        ! IGNITION FRACTION              !
        !--------------------------------!
          !--> valid for const flamme velocity
        !IF(BFRAC(I) < ONE) THEN
        ! TB = - TBURN(I)
        ! BFRAC(I) = ZERO
        ! IF(TIME > TB) BFRAC(I) = C1*(TIME-TB)*TWO_THIRD/XL
        ! IF(BFRAC(I) < EM04) THEN
        !   BFRAC(I) = ZERO
        ! ELSEIF(BFRAC(I) > ONE) THEN
        !   BFRAC(I) = ONE
        ! ENDIF
        !ENDIF
        BFRAC(I) = ONE  ! instantaneous ignition (infinite flamme velocity)

        !------------------------------------------------!
        ! BURN FRACTION                                  !
        !  BFRAC  : ignition fraction (flamme evolution) !
        !  UVAR(6): burn fraction (grain burn evolution) !
        !------------------------------------------------!
        IF(BFRAC(I) <= ZERO)THEN
          ! no ignition, no burn (so far)
          TOTAL_BFRAC = ZERO
          F = ZERO
          DELTA_BF = ZERO
        ELSEIF(F /= ONE)THEN
            ! F < 1  flamme has already reached the current element, but grain are still burning
            BRATE       = FSCALE_B*FINTER(funcb,POLD/FSCALE_P,NPF,TF,DIFF)
            DELTA_BF    = Gr*EXP(C*LOG((ONE-ALPHA*F)))*BRATE*TIMESTEP  !F_dot * DT
            F           = F + DELTA_BF
            F           = MAX(MIN(ONE,F),ZERO)
            TOTAL_BFRAC = BFRAC(I)*F  !BFRAC(I) = ONE
        ELSE
            ! complete burn, flamme has already reached the current element and grain are fully burned
            TOTAL_BFRAC = ONE
            BFRAC(I) = ONE
            DELTA_BF = ZERO
        ENDIF       

        !--------------------------------!
        ! EOS SOLVING                    !
        !--------------------------------!
        M_S = (ONE-F)*M0
        M_G = M0-M_S
        dM_s = DELTA_BF*M0   ! mass of solid burned during the time step
        dEint_chem = EG_PROD * dM_s  ! chemical released
        EINT_g = EINT_g + dEint_chem  ! gaz internal energy increase due to chemical energy release
        IF(M_g > 0) THEN
          Eg = EINT_G / M_g
        ELSE
          Eg = zero
        ENDIF

        IF( F == ONE)THEN

           !trivial solving
            V_g   = VOLUME(I)
            V_s   = ZERO
            RHO_G = M_g / VOLUME(I)
            RHO_S = zero
            P = RHO_G*Eg*exp(RHO_G/D)
            Pk = P

        ELSE

          ! numerical solving
          MAX_ITER = 20
          ITER = 0
          RESIDUE = EP20
          Pk = POLD
          !solving P=Ps=Pg with Newton-Raphson method, where Ps is given by linear EOS and Pg by exponential EOS + chemical energy release
          !  f(P) = P - Pg(RHO_G(P), eg)
          !  P[k+1] = P[k] - f(P[k])/f'(P[k])

          DO WHILE (RESIDUE > EM08 .AND. ITER < MAX_ITER)

            RHO_S = rho_ref * (one + (Pk-P0)/BULK)  ! solid density from Linear EOS
            V_s   = M_S / RHO_S
            V_s = MIN(V_s, VOLUME(I)*(ONE-EM10))
            V_g = MAX(EM20, VOLUME(I) - V_s)
            RHO_G = M_g / V_g
            Pg   = RHO_G*Eg*exp(RHO_G/D)
            fP = Pk - RHO_G*Eg*exp(RHO_G/D)
            dfP = 1 - (Eg*exp(RHO_G/D)*(one+RHO_G/D)) * (-M_g*M_s/V_g/V_g*rho_ref/BULK/RHO_S/RHO_S)  ! derivative of f(P) regarding P, with RHO_G and RHO_S depending on P
            Pk = Pk - fP/dfP
            Pk = MAX(P0, Pk)  ! avoid negative pressure

            RESIDUE = ABS(fP/(EM20+Pk))
            ITER = ITER + 1
          END DO

        END IF

        PNEW = Pk

        ! SOLID DOES NOT DEPEND ON ENERGY, SO NO NEED TO UPDATE SOLID INTERNAL ENERGY DUE TO PHASE CHANGE, ONLY GAS INTERNAL ENERGY INCREASE DUE TO CHEMICAL ENERGY RELEASE
        !dV_g = V_g - V_g_prev
        !dEint_W = -HALF*(POLD+PNEW)*dV_g  ! chemical released + work (phase change)
        !EINT_g = EINT_g + dEint_W  ! gaz internal energy increase due to chemical energy release
        !IF(M_G > ZERO) EG = EINT_G / M_G

        !--------------------------------!
        ! Cell Internal Energy           !
        !--------------------------------!
        ! total energy for output (mixture, linear eos does not depend on energy)
        EINT(I) = EINT(I) + dEint_chem
        EINT(I)= EINT(I) - HALF*(PNEW+POLD)*DVOL(I)  ! mechanical work due to volume change of the full cell
        EINT(I) = EINT(I)/MAX(EM20,VOLUME(I))  ! J/m3 for output

        POLD = PNEW
        PNEW = PNEW - PSH(I)

        print *, "V,Vs,Vg=", VOLUME(I), V_s, V_g
        print *, "M,Ms,Mg=", M0, M_S, M_G
        print *, "rho,RHO_S,RHO_G=", RHO(I), RHO_S, RHO_G
        print *, "F, P=", F, PNEW
        print *, "EINT(I), Eg", EINT(I), Eg
        print *, "______________________________________"

        !--------------------------------!
        ! Global Pressure & SoundSpeed   !
        !--------------------------------!
        SSP = ZERO
        ALPHA_S = V_s / VOLUME(I)
        IF(RHO_S > ZERO)THEN
          SSP_S2 = BULK/RHO_REF
          SSP = ALPHA_S/RHO_S/SSP_S2
        ENDIF
        IF(RHO_G > ZERO)THEN
          SSP_G2 = Pg/RHO_G*(one+RHO_G/D)
          ALPHA_g = ONE - ALPHA_s
          SSP = SSP + ALPHA_g/RHO_G/SSP_G2
        ENDIF
        SSP = MAX(SSP, EM20)
        SSP = ONE/SSP/RHO(I)  ! Wood's formula for sound speed in a mixture
        SSP = SQRT(SSP)

        !--------------------------------!
        ! Returning values               !
        !--------------------------------!     
        SIGNXX(I)  = -PNEW
        SIGNYY(I)  = -PNEW
        SIGNZZ(I)  = -PNEW
        SIGNXY(I)  = ZERO
        SIGNYZ(I)  = ZERO
        SIGNZX(I)  = ZERO
        SIGVXX(I)  = ZERO
        SIGVYY(I)  = ZERO
        SIGVZZ(I)  = ZERO
        SIGVXY(I)  = ZERO
        SIGVYZ(I)  = ZERO
        SIGVZX(I)  = ZERO   
        SOUNDSP(I) = SSP
        
        ! M0     = UVAR(I,1)
        UVAR(I,2) = F
        UVAR(I,3) = RHO_S
        UVAR(I,4) = RHO_G
        UVAR(I,5) = V_s
        UVAR(I,6) = V_g
        UVAR(I,7) = POLD
        UVAR(I,8) = EINT_g
       
        ! --- verification output
        if(time == ZERO)then
          open(UNIT=1,FILE='F.txt'    )
          open(UNIT=2,FILE='Pg.txt'   )
          open(UNIT=3,FILE='rhoS.txt' )
          open(UNIT=4,FILE='Ifrac.txt' )
        else
          open(UNIT=1,FILE='F.txt'   , position="append")
          open(UNIT=2,FILE='Pg.txt'  , position="append")
          open(UNIT=3,FILE='rhoS.txt', position="append")
          open(UNIT=4,FILE='Ifrac.txt',position="append")
        endif

        WRITE(1,*)TIME,F
        WRITE(2,*)TIME,Pg
        WRITE(3,*)TIME,RHO_S
        WRITE(4,*)BFRAC(I)

        CLOSE(1)
        CLOSE(2)
        CLOSE(3)
        CLOSE(4)

      ENDDO! next I   

C-----------------------------------------------
      RETURN
C-----------------------------------------------
      END SUBROUTINE SIGEPS105
C-----------------------------------------------
