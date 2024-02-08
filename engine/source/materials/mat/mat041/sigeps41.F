Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  SIGEPS41                      source/materials/mat/mat041/sigeps41.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        BURN_FRACTION_IREAC1          source/materials/mat/mat041/sigeps41.F
Chd|        BURN_FRACTION_IREAC2          source/materials/mat/mat041/sigeps41.F
Chd|        MIXTURE_EQUILIBRIUM           source/materials/mat/mat041/sigeps41.F
Chd|====================================================================
      SUBROUTINE SIGEPS41 (
     1     NEL    ,NUPARAM,NUVAR   ,
     2     TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,DELTVOL ,BFRAC  ,
     B     TEMP)
      USE ROOT_FINDING_ALGO_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   MULAW
C    |__SIGEPS41 (LEE TARVER)
C         |__ MIX (JWL EOS solve mixture state for both reagent & product, Pr=Pp=P).
C         |    iterative solver
C         |    |__ JWL_EOS_STATE (JWL EoS single phase / reagent)
C         |    |__ JWL_EOS_STATE (JWL EoS single phase / product)
C         |    |__ FTNCH
C         |
C         |__ BURN_FRACTION_IREAC1 (update F function, case Ireac=1)
C         |__ BURN_FRACTION_IREAC2 (update F function, case Ireac=2)
C
C   ________________________________
C   IREAC FLAG
C
C   *** IREAC = 1 ***
C   ORIGINAL 2-TERM-MODEL   --> dF/dT = IGNITION TERM + GROWTH TERM
C     IGNITION TERM : ratei = I * (1.-F)**b * (etar-1.)**x     , if P>0
C     GROWTH TERM   : rateg = G1 * (1.-F)**b * F**d * pres**y  , if P>0
C
C   *** IREAC = 2 ***
C   EXTENDED 3-TERM-MODEL   --> dF/dT = IGNITION TERM + GROWTH TERM 1 + GROWTH TERM 2
C        IGNITION TERM :  ratei = I * (1.-F+tol)**b * abs(etar-1.-ccrit)**x     , if 0<f<Figmax & P>0 & compression thresold (etar-1>=ccrit)
C        GROWTH TERM 1 : rateg1 = G1 * (1.-F+tol)**c * (F+tol)**d * pres**y     , if 0<f<FG1max & P>0
C        GROWTH TERM 2 : rateg2 = G2 * (1.-F+tol)**e * (F+tol)**g * pres**z     , if FG2min<f<1 & P>0
C
C   ________________________________
C   NOTATION
C     index 'p' : for 'p'roducts of detonation
C     index 'r' : for 'r'eagent (unreacted explosive)
C     fc : Function F described by Lee-Tarver model dF/dt = ...
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C DELTVOL | NEL     | F |R  | VOLUME VARIATION
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR

      my_real, INTENT(IN) ::
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   DELTVOL(NEL)

      my_real, INTENT(INOUT) ::
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),EINT(NEL),BFRAC(NEL),TEMP(NEL)

      my_real,INTENT(INOUT) :: UVAR(NEL,NUVAR)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real ar,br,r1r,r2r,r3r,cvr,etar,
     .        ap,bp,r1p,r2p,r3p,cvp,etap,
     .        epsil,ftol, tmp,
     .        I_,B_,X_,G1,D_,Y_,G2,
     .        Z_,G_,C_,E_,Figmax,FG1max,FG2min,
     .        cappa,chi,ccrit,tol,cv,dedv,
     .        artvisc,dt,pres,fc,cburn,enq,SHEAR,vol_rel,dvol_rel
      my_real oldfc,tem1,tem2,chydro,chydro2,dpdmu,dpde,heat
      my_real mt,pold,er,tfextt,wr,wp
      integer  itrmax,i_reac,i
      my_real :: beta !volumic fraction of reagent
      my_real, dimension(25) :: funct_parameter

C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      TFEXTT=ZERO        
      if (TIME == ZERO) then
        do i=1,nel
          uvar(i,1) = ZERO       ! Burn Fraction (F)
          uvar(i,2) = uparam(39) ! temperature
          uvar(i,3) = uparam(14) ! cv mixture, volumetric heat capacity (SI: J/m3/K)
          uvar(i,4) = ONE        ! relative volume
          uvar(i,5) = ZERO       ! dedv
          uvar(i,6) = ZERO       ! eta_p (eta:=rho/rho0)
          uvar(i,7) = ONE        ! eta_r (eta:=rho/rho0)
        enddo
      endif

      i_reac = int(uparam(1))
      itrmax = int(uparam(18))
      dt = timestep
      ar = uparam(2)
      br = uparam(3)
      r1r = uparam(4)
      r2r = uparam(5)
      r3r = uparam(6)
      wr = uparam(7)
      cvr = uparam(14)
      ap = uparam(8)
      bp = uparam(9)
      r1p = uparam(10)
      r2p = uparam(11)
      r3p = uparam(12)
      wp = uparam(13)
      cvp = uparam(15)
      enq = uparam(16)
      epsil = uparam(17)
      ftol = uparam(19)
      I_ = uparam(20)
      B_ = uparam(21)
      X_ = uparam(22)
      G1 = uparam(23)
      D_ = uparam(24)
      Y_ = uparam(25)
      cappa = uparam(26)
      chi = uparam(27)
      tol = uparam(28)
      ccrit = uparam(29)
      G2 = uparam(30)
      C_ = uparam(31)
      E_ = uparam(32)
      G_ = uparam(33)
      Z_ = uparam(34)
      Figmax = uparam(35)
      FG1max = uparam(36)
      FG2min = uparam(37)
      SHEAR = uparam(38)
c

      funct_parameter(1:25) = zero
      funct_parameter(1) = ar
      funct_parameter(2) = br 
      funct_parameter(3) = r1r 
      funct_parameter(4) = r2r 
      funct_parameter(5) = r3r
      funct_parameter(6) = cvr
      ! funct_parameter(7) = tmp output
      ! funct_parameter(8) = dedvr output
      ! funct_parameter(9) = preac output
      ! funct_parameter(10) = bthr ! output
      ! funct_parameter(11) = dpdtr! output
      ! funct_parameter(12) = enr! output

      funct_parameter(13) = ap
      funct_parameter(14) = bp
      funct_parameter(15) = r1p
      funct_parameter(16) = r2p
      funct_parameter(17) = r3p
      funct_parameter(18) = cvp
      ! funct_parameter(19) = dedvp
      ! funct_parameter(20) = pprod
      ! funct_parameter(21) = bthp
      ! funct_parameter(22) = dpdtp
      ! funct_parameter(23) = enp
      ! funct_parameter(24) = etac
      ! funct_parameter(25) = fc


      do i=1,nel
        vol_rel = RHO0(i) / RHO(i) !no unit
        pres = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I)) * THIRD ! pressure
        pold = pres
        artvisc = ZERO !pseudo viscosity
        mt=RHO(I)*VOLUME(I) !mass
        IF (TIME > ZERO .AND. IALE + IEULER > 0) THEN
          UVAR(I, 1) = UVAR(I, 1) / MT
          UVAR(I, 1) = MAX(ZERO, MIN(UVAR(I, 1), ONE))
        ENDIF
        fc   = uvar(i,1)  ! F function (no unit)
        cv   = uvar(i,3)  ! volumetric heat capacity (SI: J/m3/K)
        tmp  = uparam(39) ! temperature K
        if(TIME > ZERO .AND. mt > EM20) tmp  = EINT(I)/(mt*cv)
        dvol_rel = vol_rel - uvar(i,4)
        dedv = uvar(i,5)
        etap = uvar(i,6)
        etar = uvar(i,7)
C=======================================================================
c        Pressure and Sound Speed
c
c  initi
        oldfc = fc
c
c  hydrodynamic state (pass 1)
        tem1 = (pres+dedv+artvisc) / cv  ! (Cv  SI:J/m3/K = Pa/K)
        tmp = tmp - tem1*dvol_rel
        heat=ZERO
        dpdmu=ZERO
        dpde=ZERO
        CALL MIXTURE_EQUILIBRIUM(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dpdmu,dpde,ftol,enq,
     .           tmp,heat,vol_rel ,fc,cv,dedv,pres, beta,funct_parameter)
c
c  hydrodynamic state (pass 2)
        tem2 = (pres+dedv+artvisc) / cv          ! (Cv  SI:J/m3/K = Pa/K)
        tmp = tmp - HALF*(tem2-tem1)*dvol_rel
        CALL MIXTURE_EQUILIBRIUM(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dpdmu,dpde,ftol,enq,
     .           tmp,heat,vol_rel ,fc,cv,dedv,pres, beta,funct_parameter)
c
c  dF/dt (F function)
        !ORIGINAL 2-TERM-MODEL
        if (i_reac == 1) CALL BURN_FRACTION_IREAC1(I_,B_,X_,G1,D_,Y_,cappa,artvisc,dt,pres,etar,fc,cburn,oldfc)
        !EXTENDED 3-TERM-MODEL
        if (i_reac == 2) CALL BURN_FRACTION_IREAC2(I_,B_,X_,G1,D_,Y_,G2,Z_,G_,C_,E_,Figmax,FG1max,FG2min,
     .                             cappa,chi,ccrit,tol,artvisc,dt,pres,etar,fc,cburn,oldfc)
        tmp = tmp + (fc - oldfc) * heat/cv
c  sound speed (time step criteria)
        chydro2 = dpdmu + abs(pres)*(vol_rel*vol_rel)*dpde + ONEP33*SHEAR
        chydro2=max(chydro2,max(wr+ONE,wp+ONE)*abs(pres)/max(em30,RHO(I)))
        chydro = sqrt(chydro2)
c
C       BUFFER STORAGE
C       --------------
        soundsp(i) = max(chydro,cburn)
        viscmax(i) = ZERO
c       stress tensor
        signxx(i) = -pres
        signyy(i) = -pres
        signzz(i) = -pres
        signxy(i) = ZERO
        signyz(i) = ZERO
        signzx(i) = ZERO
c       viscous stress tensor
        sigvxx(i) = ZERO
        sigvyy(i) = ZERO
        sigvzz(i) = ZERO
        sigvxy(i) = ZERO
        sigvyz(i) = ZERO
        sigvzx(i) = ZERO
c       material buffer
        uvar(i,1) = fc
        uvar(i,2) = tmp
        uvar(i,3) = cv
        uvar(i,4) = vol_rel
        uvar(i,5) = dedv
        uvar(i,6) = etap !products:rho_p/rho0_p
        uvar(i,7) = etar !reagents:rho_r/rho0_r
C       Burn fraction
        BFRAC(I) = fc ! burn fraction (function F)
        uvar(i,8) = ONE - beta !volume fraction of products
C       energie released by the reaction = D(energie totale) - (-P.DV)
        er=mt*cv*tmp + HALF*(pres+pold)*DELTVOL(I)  !misunderstanding between volumetric heat capacity (J/m3/K = Pa/K) and heat capacity(J/K) ? law41 to be verified.
        TFEXTT=TFEXTT+er-EINT(I)
        EINT(I)=mt*cv*tmp + HALF*(pres+pold)*DELTVOL(I)
C       EINT(I)=EINT(I)-0.5*(pres+pold)*DELTVOL(I) done in mulaw.
        TEMP(I) = tmp !temperature
      enddo
c
#include "atomic.inc"
      TFEXT=TFEXT+TFEXTT
#include "atomend.inc"
C-----------------------------------------------
      RETURN
      END
C-----------------------------------------------

C-----------------------------------------------
Chd|====================================================================
Chd|  BURN_FRACTION_IREAC1          source/materials/mat/mat041/sigeps41.F
Chd|-- called by -----------
Chd|        SIGEPS41                      source/materials/mat/mat041/sigeps41.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BURN_FRACTION_IREAC1(I_,B_,X_,G1,D_,Y_,cappa,artvisc,dt,pres,etar,fc,cburn,oldfc)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------

C   *** IREAC = 1 ***
C   ORIGINAL 2-TERM-MODEL   --> dF/dT = IGNITION TERM + GROWTH TERM
C     IGNITION TERM : ratei = I * (1.-F)**b * (etar-1.)**x     , if P>0
C     GROWTH TERM   : rateg = G1 * (1.-F)**b * F**d * pres**y  , if P>0
c
c    bounds :
c    -dfc limited during 1 cycle (chi)
c    -dfc limited within a shock front (cappa)
c   cburn not calculated here
c
c   input : etar, pres, oldfc, dt
c   output : fc, cburn
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   I_,B_,X_,G1,D_,Y_,cappa,
     .   artvisc,dt,pres,etar,fc,cburn,oldfc
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real ratei,rateg,dfci,dfcg
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------
      cburn = ZERO
      if (pres <= ZERO) return              !no rate to calculate for material expansion
      if (artvisc >= (cappa*pres)) return   !
      if (fc == ONE) return                 !everything alread reacted, 100% products, Fc remains 1.00
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
c     ignition term
      ratei = I_ * (ONE -fc)**B_ * (etar-ONE)**X_
      if ((etar - ONE) <= ZERO) ratei=ZERO
      dfci = ratei * dt
c     growth term
      rateg =  G1 * (ONE - fc)**B_ * fc**D_ * pres**Y_
      dfcg = rateg * dt
c     F function  (0. < F < 1.)
      fc = oldfc + max(dfci,zero) + max(dfcg,zero)
      if (fc >= ONE) fc = ONE
C-----------------------------------------------
      RETURN
      END SUBROUTINE BURN_FRACTION_IREAC1
C-----------------------------------------------


Chd|====================================================================
Chd|  BURN_FRACTION_IREAC2          source/materials/mat/mat041/sigeps41.F
Chd|-- called by -----------
Chd|        SIGEPS41                      source/materials/mat/mat041/sigeps41.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BURN_FRACTION_IREAC2(I_,B_,X_,G1,D_,Y_,G2,Z_,G_,C_,E_,Figmax,FG1max,FG2min,cappa,chi,ccrit,tol,
     .                 artvisc,dt,pres,etar,fc,cburn,oldfc)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   *** IREAC = 2 ***
C   EXTENDED 3-TERM-MODEL   --> dF/dT = IGNITION TERM + GROWTH TERM 1 + GROWTH TERM 2
C        IGNITION TERM :  ratei = I * (1.-F+tol)**b * abs(etar-1.-ccrit)**x     , if 0<f<Figmax & P>0 & compression thresold (etar-1>=ccrit)
C        GROWTH TERM 1 : rateg1 = G1 * (1.-F+tol)**c * (F+tol)**d * pres**y , if 0<f<FG1max & P>0
C        GROWTH TERM 2 : rateg2 = G2 * (1.-F+tol)**e * (F+tol)**g * pres**z     , if FG2min<f<1 & P>0
c
c    bounds :
c    - dfc limited during 1 cycle (chi)
c    - dfc limited within a shock front (cappa)
c    buirn not calculated here
c
c   input : etar, pres, oldfc, dt
c   output : fc, cburn
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real I_,B_,X_,G1,D_,Y_,G2,Z_,G_,C_,E_,
     .        Figmax,FG1max,FG2min,cappa,chi,ccrit,tol,
     .        artvisc,dt,pres,etar,fc,cburn,oldfc
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real ratei,rateg,rateg1,rateg2,dfci,dfcg
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------
      cburn = ZERO
      if (pres <= ZERO) return
      if (fc == ONE) return
      if (artvisc >= (cappa*pres)) return
      if (fc <= Figmax) then
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       ! ignition term
        ratei = I_ * (ONE-fc+tol)**B_ * abs(etar-ONE-ccrit)**X_
        if ((etar- ONE -ccrit)<=ZERO) ratei = ZERO
        dfci = ratei * dt
        fc = oldfc + max(dfci,zero)
        if (fc > Figmax) fc = Figmax
      endif

      ! growth term 1
      rateg1 = G1 * (ONE -fc+tol)**C_ * (fc+tol)**D_ * pres**Y_
      if (fc > FG1max) rateg1 = ZERO
      ! growth term 2
      rateg2 = G2 * (ONE -fc+tol)**E_ * (fc+tol)**G_ * pres**Z_
      if (fc < FG2min) rateg2 = ZERO
      ! F function  (0. < F < 1.)
      rateg = rateg1 + rateg2
      dfcg = rateg * dt
      fc = fc + max(dfcg,zero)
      if ((fc-oldfc)>chi) fc = oldfc + chi
      if (fc >= ONE) fc = ONE
C-----------------------------------------------
      RETURN
      END SUBROUTINE BURN_FRACTION_IREAC2
C-----------------------------------------------


Chd|====================================================================
Chd|  MIXTURE_EQUILIBRIUM           source/materials/mat/mat041/sigeps41.F
Chd|-- called by -----------
Chd|        SIGEPS41                      source/materials/mat/mat041/sigeps41.F
Chd|-- calls ---------------
Chd|        JWL_EOS_STATE                 source/materials/mat/mat041/sigeps41.F
Chd|====================================================================
      SUBROUTINE MIXTURE_EQUILIBRIUM(
     .               ar   ,br   ,r1r    ,r2r  ,r3r   ,cvr ,etar,
     .               ap   ,bp   ,r1p    ,r2p  ,r3p   ,cvp ,etap,
     .               dpdmu,dpde ,ftol,enq ,
     .               tmp  ,heat ,vol_rel,fc,cv,dedv  ,pres, beta,funct_parameter)
      USE ROOT_FINDING_ALGO_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
c  fc massic fraction (F function) ;
c  beta : volume fraction of reagent (unreacted explosive)
c  T,v,fc knwon : find beta such as Pr=Pp  (hypothesis : equilibrium between two phase for system closure)
c  Iterative method : iterate on b value until delta = Pp-Pr < eps
c
c  input : vol_rel, etar, etap, fc, tmp
c  output : etar, etap, pres, dedv,cv,heat,dpde,dpdmu
c  local : beta
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .        ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .        ftol,heat,dpdmu,dpde,enq, tmp,
     .        vol_rel ,fc,cv,dedv,pres, beta
      my_real, dimension(25), intent(inout) :: funct_parameter
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer iter,i1
      my_real delta,fc1,etac,guess,pmax,
     .        trans,transr,transp,trans1,trans2,dbdeta,detar,detap,
     .        dedr,dedp,bth,dbdt,detrdt,detpdt,dpdt,dbdf,detrdf,
     .        detpdf,cvr0,cvp0,slope,tl,tu,a,b,c,
     .        bthp,dedvp,preac,pprod,dpdtp,dpdtr,bthr,dedvr,
     .        enp,enr
      my_real :: lower_bound,upper_bound
      my_real :: jwl_eos_delta
      external :: jwl_eos_delta
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
c   initi.
      etac = ONE/vol_rel
      fc1 = ONE -fc
      if (fc < ftol) then
c       reaction has not started : F=0., beta=Vfrac(reagent)=1.
        etar = fc1 * etac
        CALL JWL_EOS_STATE(ar,br,r1r,r2r,r3r,cvr,etar,tmp,dedvr,pres,bthr,dpdtr,enr)
        beta = ONE
        cv = cvr
        dedv = dedvr
        dpde = dpdtr / cv
        dpdmu = bthr + dpdtr*dedv / (cv*etac**2)
        etap = EM6
        CALL JWL_EOS_STATE(ap,bp,r1p,r2p,r3p,cvp,etap,tmp,dedvp,pprod,bthp,dpdtp,enp)
        heat = enq-enp+enr
        return
c
      elseif (fc > (ONE -ftol)) then
c       reaction has finished : F=1., beta=Vfrac(reagent)=0.
        etap = fc * etac
        CALL JWL_EOS_STATE(ap,bp,r1p,r2p,r3p,cvp,etap,tmp,dedvp,pres,bthp,dpdtp,enp )
        cv = cvp
        beta = ZERO
        heat = enq
        dedv = dedvp
        dpde = dpdtp / cv
        dpdmu = bthp + dpdtp*dedv / (cv*etac**2)
        return
c
      endif


      funct_parameter(7) = tmp
      funct_parameter(24) = etac
      funct_parameter(25) = fc

      funct_parameter(8) = zero !dedvr output
      funct_parameter(9) = zero !preac output
      funct_parameter(10) = zero !bthr ! output
      funct_parameter(11) = zero !dpdtr! output
      funct_parameter(12) = zero !enr! output

      funct_parameter(19) = zero !dedvp
      funct_parameter(20) = zero !pprod
      funct_parameter(21) = zero !bthp
      funct_parameter(22) = zero !dpdtp
      funct_parameter(23) = zero !enp



      guess = fc1 * etap/(fc1*etap+fc*etar)
      beta = guess
      if (beta == ONE) beta = fc1
      lower_bound = EPSILON(BETA)
      upper_bound = ONE - EPSILON(BETA)
      beta = BRENT_ALGO(lower_bound,upper_bound,100*epsilon(ONE),jwl_eos_delta,25,FUNCT_PARAMETER)       

      etar = fc1 * etac/beta
      CALL JWL_EOS_STATE(ar,br,r1r,r2r,r3r,cvr,etar,tmp,dedvr,preac,bthr,dpdtr,enr)
      etap = fc*etac/(one-beta)
      CALL JWL_EOS_STATE(ap,bp,r1p,r2p,r3p,cvp,etap,tmp,dedvp,pprod,bthp,dpdtp,enp)
c   pressure
      pres = (preac+pprod)*HALF
c
c   local derrivatives
      transr = bthr / beta
      transp = bthp / (ONE - beta)
      trans = transr*etar + transp*etap
c    dbdeta = d(beta)/d(eta) at equilibrium
      dbdeta = (transr*fc1 - transp*fc) / trans
c    detar = d(etar)/d(eta) at equilibrium
      detar = (fc1 - etar*dbdeta) / beta
      detap = (fc + etap*dbdeta) / (ONE - beta)
c    dedr = d(er)/d(etar)
      dedr = -dedvr / etar**2
      dedp = -dedvp / etap**2
c    bth = d(p)/d(eta) at constant temperature
      bth = (bthr*detar + bthp*detap)*HALF
c    dbdt = d(beta)/d(tmp) at equilibrium
      dbdt = (dpdtr-dpdtp) / trans
      detrdt = -etar*dbdt/beta
      detpdt = etap*dbdt/(ONE-beta)
      dpdt = (dpdtr+dpdtp+dbdt*(transp*etap-transr*etar))*HALF
c    dbdf = d(beta)/d(fc) at equilibrium
      dbdf = -(transr+transp)*etac/trans
c    detrdf = d(etar)/d(fc) at equilibrium
      detrdf = -(etac+etar*dbdf) / beta
      detpdf = (etac+etap*dbdf) / (ONE-beta)
c
c   output derivatives
c    dedv = de/dv (for constant T & fc values)
      dedv = -etac**2 * (fc1*dedr*detar+fc*dedp*detap)
c    cv = de/dT(for constant V & fc values)
      cvr0 = cvr + dedr*detrdt
      cvp0 = cvp + dedp*detpdt
      cv = fc1*cvr0 + fc*cvp0 !cv mixture
c    heat = -de/d(fc) (for constant T & V values)
      heat = enq-enp+enr - fc*dedp*detpdf-fc1*dedr*detrdf
c    dpde = dp/de (for constant V & fc values)
      dpde = dpdt / cv
c    dpdmu = dp/d(eta) (for constant E & fc values)
      dpdmu = bth + dpdt*dedv/cv/etac**2
C-----------------------------------------------
      RETURN
      END SUBROUTINE MIXTURE_EQUILIBRIUM
C-----------------------------------------------


C-----------------------------------------------

C-----------------------------------------------
Chd|====================================================================
Chd|  JWL_EOS_STATE                 source/materials/mat/mat041/sigeps41.F
Chd|-- called by -----------
Chd|        MIXTURE_EQUILIBRIUM           source/materials/mat/mat041/sigeps41.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JWL_EOS_STATE(a,b,r1,r2 ,r3,cv ,eta,tmp,dedv,p,bth,dpdt,en)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
c  JWL EoS
c  p = a*exp(-r1/eta) + b*exp(-r2/eta) + r3*eta*tmp
c  cv = volumetric heat capacity (cte)
c  eta : rho/rho0 = V0/V = 1/v  (v relative volume)
c
c  input : eta, tmp
c  output : p, en, dedv, bth, dpdT (JWL derivative)
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real a ,b ,r1  ,r2 ,r3 ,cv ,eta, tmp,dedv,p,bth,dpdT,en
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real trans1,trans2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      trans1 = a * exp(-r1/eta)
      trans2 = b * exp(-r2/eta)
      dedv = -trans1 -trans2
      p = -dedv + r3*eta*tmp
      bth = r3*tmp + (r1*trans1+r2*trans2)/eta**2
      dpdT = r3*eta
      en = trans1/r1 + trans2/r2 + cv*tmp
C-----------------------------------------------
      return
      end
C-----------------------------------------------

      function jwl_eos_delta(beta,funct_parameter)     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, intent(in) :: beta
      my_real, dimension(25), intent(inout) :: funct_parameter
      my_real :: jwl_eos_delta
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: tmp
      my_real :: ar,br,r1r,r2r,r3r,cvr,dedvr,preac,bthr,dpdtr,enr
      my_real :: ap,bp,r1p,r2p,r3p,cvp,dedvp,pprod,bthp,dpdtp,enp
      my_real :: etac,fc,fc1,etar,etap
C-----------------------------------------------
      ar = funct_parameter(1)
      br = funct_parameter(2)
      r1r = funct_parameter(3)
      r2r = funct_parameter(4)
      r3r = funct_parameter(5)
      cvr = funct_parameter(6)
      tmp = funct_parameter(7)
      dedvr = funct_parameter(8)
      preac = funct_parameter(9)
      bthr = funct_parameter(10)
      dpdtr = funct_parameter(11)
      enr = funct_parameter(12)

      ap = funct_parameter(13)
      bp = funct_parameter(14)
      r1p = funct_parameter(15)
      r2p = funct_parameter(16)
      r3p = funct_parameter(17)
      cvp = funct_parameter(18)
      dedvp = funct_parameter(19)
      pprod = funct_parameter(20)
      bthp = funct_parameter(21)
      dpdtp = funct_parameter(22)
      enp = funct_parameter(23)

      etac = funct_parameter(24)
      fc = funct_parameter(25)
      fc1 = ONE - fc

      etar = fc1 * etac/beta
      call JWL_EOS_STATE(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           tmp,dedvr,preac,bthr,dpdtr,enr )
      etap = fc * etac/(ONE -beta)
      call JWL_EOS_STATE(ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           tmp,dedvp,pprod,bthp,dpdtp,enp )
      jwl_eos_delta = preac-pprod
      return
      end function jwl_eos_delta
