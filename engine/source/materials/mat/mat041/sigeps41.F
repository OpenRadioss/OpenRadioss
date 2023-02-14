Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
      SUBROUTINE SIGEPS41 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,DELTVOL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
! IALE
#include      "com01_c.inc"
#include      "com06_c.inc"
C   S p e c i f i c a t i o n s
c       Subroutine Ig & Gr pour explosifs,
c       appelle la subroutine lee_tarver
c       Le paragraphe sur les fonctions est mis en commentaires


C   C o m m e n t a i r e s   M e c a l o g
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX
C NPF     |  *      | I | R | FUNCTION ARRAY
C TF      |  *      | F | R | FUNCTION ARRAY
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
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
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
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C DELTVOL | NEL     | F |R  | VOLUME VARIATION
C---------+---------+---+---+--------------------------------------------
C   D e c l a r a t i o n s    i n p u t
      INTEGER NEL, NUPARAM, NUVAR
      my_real TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   DELTVOL(NEL)
C   D e c l a r a t i o n s    o u t p u t
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)

C   D e c l a r a t i o n s    i n p u t / o u t p u t
      my_real UVAR(NEL,NUVAR), OFF(NEL)

C   V a r i a b l e s   "FOR FUNCTION INTERPOLATION" (???)
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real TF(*)
C
      my_real
     .   ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .   tmp,
     .   ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .   epsil,check,
     .   rki,ex,ri,rkg,yg,zg,grow2,
     .   zg2,yg2,ex1,ex2,fmxig,fmxgr,fmngr,
     .   cappa,chi,ccrit,tol,cv,dedv,
     .   artvisc,dt,pres,fc,cburn,enq,shr,vol,dvol
      my_real
     .   oldfc,tem1,tem2,chydro,chydro2,dpdmu,dpde,heat
      my_real
     .   mt,pold,er,tfextt,wr,wp
      integer  itrmax,i_reac,i
      my_real :: beta !volumic fraction of unreacted explosif
c
c
c     parametres et variables utilises par lee_tarver :
c
      TFEXTT=0.
C   I n i t i a l i s a t i o n s
      if (TIME==ZERO) then
        do i=1,nel
          uvar(i,1) = ZERO
          uvar(i,3) = uparam(14)
          uvar(i,4) = ONE
          uvar(i,5) = ZERO
          uvar(i,6) = ZERO
          uvar(i,7) = ONE
        enddo
ccc        return
      endif

C   C a l c u l s
cc     provisoire...
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
      check = uparam(19)
      rki = uparam(20)
      ex = uparam(21)
      ri = uparam(22)
      rkg = uparam(23)
      yg = uparam(24)
      zg = uparam(25)
      cappa = uparam(26)
      chi = uparam(27)
      tol = uparam(28)
      ccrit = uparam(29)
      grow2 = uparam(30)
      ex1 = uparam(31)
      ex2 = uparam(32)
      yg2 = uparam(33)
      zg2 = uparam(34)
      fmxig = uparam(35)
      fmxgr = uparam(36)
      fmngr = uparam(37)
      shr = uparam(38)
c
      do i=1,nel
        vol = RHO0(i) / RHO(i)
        pres = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I)) * THIRD
        pold = pres
        artvisc = ZERO
        mt=RHO(I)*VOLUME(I)
        IF (TIME > ZERO .AND. IALE + IEULER > 0) THEN
          UVAR(I, 1) = UVAR(I, 1) / MT
          UVAR(I, 1) = MAX(ZERO, MIN(UVAR(I, 1), ONE))
        ENDIF
        fc   = uvar(i,1)
        cv   = uvar(i,3)
        tmp  = uparam(39)
        if(TIME>0. .and. mt>em30)
     .     tmp  = EINT(I)/(mt*cv)
        dvol = vol - uvar(i,4)
        dedv = uvar(i,5)
        etap = uvar(i,6)
        etar = uvar(i,7)
C=======================================================================
c        calcul de pres et cson
c
c  initialisations
        oldfc = fc
c
c  premier calcul hydro
        tem1 = (pres+dedv+artvisc) / cv
        tmp = tmp - tem1*dvol
        heat=ZERO
        dpdmu=ZERO
        dpde=ZERO
        call mix(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dpdmu,dpde,epsil,check,itrmax,enq,
     .           tmp,heat,vol ,fc,cv,dedv,pres, beta)
c
c  deuxieme calcul hydro
        tem2 = (pres+dedv+artvisc) / cv
        tmp = tmp - HALF*(tem2-tem1)*dvol
        call mix(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dpdmu,dpde,epsil,check,itrmax,enq,
     .           tmp,heat,vol ,fc,cv,dedv,pres, beta)
c
c  reaction chimique
        if (i_reac==1) call burn1(rki,ex,ri,rkg,yg,zg,cappa,
     .                   artvisc,dt,pres,etar,fc,cburn,oldfc)
        if (i_reac==2) call burn2(rki,ex,ri,rkg,yg,zg,grow2,
     .                  zg2,yg2,ex1,ex2,fmxig,fmxgr,fmngr,
     .                  cappa,chi,ccrit,tol,
     .                  artvisc,dt,pres,etar,fc,cburn,oldfc)
        tmp = tmp + (fc - oldfc) * heat/cv
c  vitesse du son et controle du pas de temps
        chydro2 = dpdmu + abs(pres)*(vol**2)*dpde
     +                + ONEP33*shr
        chydro2=max(chydro2,
     .              max(wr+ONE,wp+ONE)*abs(pres)/max(em30,RHO(I)))
        chydro = sqrt(chydro2)
c
C=======================================================================
        soundsp(i) = max(chydro,cburn)
        viscmax(i) = ZERO
c        contraintes
        signxx(i) = - pres
        signyy(i) = - pres
        signzz(i) = - pres
        signxy(i) = ZERO
        signyz(i) = ZERO
        signzx(i) = ZERO
        sigvxx(i) = ZERO
        sigvyy(i) = ZERO
        sigvzz(i) = ZERO
        sigvxy(i) = ZERO
        sigvyz(i) = ZERO
        sigvzx(i) = ZERO
c        mise   jour
        uvar(i,1) = fc
        uvar(i,2) = tmp
        uvar(i,3) = cv
        uvar(i,4) = vol
        uvar(i,5) = dedv
        uvar(i,6) = etap
        uvar(i,7) = etar
C     Burnt fraction
        uvar(i,8) = ONE - beta
C       energie degagee par la reaction = D(energie totale) - (-P.DV)
        er=mt*cv*tmp + HALF*(pres+pold)*DELTVOL(I)
        TFEXTT=TFEXTT+er-EINT(I)
        EINT(I)=mt*cv*tmp + HALF*(pres+pold)*DELTVOL(I)
C       EINT(I)=EINT(I)-0.5*(pres+pold)*DELTVOL(I) dans mulaw.
      enddo
c
#include "atomic.inc"
      TFEXT=TFEXT+TFEXTT
#include "atomend.inc"
c
      RETURN
      END
c
c -------------------------------------------------------------------
c
      subroutine burn1(rki,ex,ri,rkg,yg,zg,cappa,
     .                 artvisc,dt,pres,etar,fc,cburn,oldfc)
c
c  reaction chimique :
c   ignition & growth selon Lee/Tarver
c     ratei = rki * (1.-fc)**ex * (etar-1.)**ri
c     rateg = rkg * (1.-fc)**ex * fc**yg * pres**zg
c   limites :
c    -pas de reaction si P<0
c    -dfc limite pdt 1 cycle (avec chi)
c    -dfc limite ds un front de choc (avec cappa)
c   pas de calcul de cburn pour le moment...
c
c   entrees : etar, pres, oldfc, dt
c   sorties : fc, cburn
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   rki,ex,ri,rkg,yg,zg,cappa,
     .   artvisc,dt,pres,etar,fc,cburn,oldfc
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  ratei,rateg,dfci,dfcg
c
c   controle du pas de temps (  faire)
      cburn = ZERO
c   short cuts
      if (pres<=ZERO) return
      if (artvisc>=(cappa*pres)) return
      if (fc==ONE) return
c
c   ignition & growth
      ratei = rki * (ONE -fc)**ex * (etar-ONE)**ri
      if ((etar - ONE)<=ZERO) ratei=ZERO
      dfci = ratei * dt
      rateg =  rkg * (ONE - fc)**ex * fc**yg * pres**zg
      dfcg = rateg * dt
      fc = oldfc + max(dfci,zero) + max(dfcg,zero)
      if (fc>=ONE) fc = ONE
c
      return
      end
c
c ------------------------------------------------------------
c
      subroutine burn2(rki,ex,ri,rkg,yg,zg,grow2,zg2,yg2,ex1,ex2,
     +                fmxig,fmxgr,fmngr,cappa,chi,ccrit,tol,
     .   artvisc,dt,pres,etar,fc,cburn,oldfc)
c
c  reaction chimique :
c   ignition & growth comme dans l'eqos7 de dyna2d
c        ratei = rki * (1.-fc+tol)**ex * abs(etar-1.-ccrit)**ri
c        rateg1 = grow1 * (1.-fc+tol)**ex1 * (fc+tol)**yg1 * pres**zg1
c        rateg2 = grow2 * (1.-fc+tol)**ex2 * (fc+tol)**yg2 * pres**zg2
c   limites :
c    - pas d'ignition si fc>fmxig ; fc<fmxig apres ignition
c    - pas d'ignition si compression insuffisante (etar-1<ccrit)
c    - pas de reaction si P<0
c    - 2 growth rates suivant fc par rapport a fmxgr et fmngr
c    - dfc limite pdt 1 cycle (avec chi)
c    - dfc limite ds un front de choc (avec cappa)
c   pas de calcul de cburn pour le moment...
c
c   entrees : etar, pres, oldfc, dt
c   sorties : new fc, cburn
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   rki,ex,ri,rkg,yg,zg,grow2,zg2,yg2,ex1,ex2,
     +   fmxig,fmxgr,fmngr,cappa,chi,ccrit,tol,
     .   artvisc,dt,pres,etar,fc,cburn,oldfc
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   ratei,rateg,rateg1,rateg2,dfci,dfcg,grow1,yg1,zg1
c
c
c  controle du pas de temps (a faire)
      cburn = ZERO
c   initialisations et short cuts
      grow1 = rkg
      yg1 = yg
      zg1 = zg
      if (pres<=ZERO) return
      if (fc==1.) return
      if (artvisc>=(cappa*pres)) return
      if (fc<=fmxig) then
c
c       ignition & growth
c
        ratei = rki * (ONE-fc+tol)**ex * abs(etar-ONE-ccrit)**ri
        if ((etar- ONE -ccrit)<=ZERO) ratei = ZERO
        dfci = ratei * dt
        fc = oldfc + max(dfci,zero)
        if (fc>fmxig) fc = fmxig
      endif
      rateg1 = grow1 * (ONE -fc+tol)**ex1 * (fc+tol)**yg1 * pres**zg1
      if (fc>fmxgr) rateg1 = ZERO
      rateg2 = grow2 * (ONE -fc+tol)**ex2 * (fc+tol)**yg2 * pres**zg2
      if (fc<fmngr) rateg2 = ZERO
      rateg = rateg1 + rateg2
      dfcg = rateg * dt
      fc = fc + max(dfcg,zero)
      if ((fc-oldfc)>chi) fc = oldfc + chi
      if (fc>=ONE) fc = ONE
c
      return
      end
c
c ------------------------------------------------------------
c
      subroutine mix(ar   ,br   ,r1r  ,r2r  ,r3r   ,cvr ,etar,
     .               ap   ,bp   ,r1p  ,r2p  ,r3p   ,cvp ,etap,
     .               dpdmu,dpde ,epsil,check,itrmax,enq ,
     .               tmp  ,heat ,vol  ,fc,cv,dedv  ,pres, beta)
c
c  fc fraction massique de produits ;
c  beta fraction volumique de reactifs.
c  T,v,fc donnes : on trouve le beta unique tel que Pr=Pp
c  Newton : iterations sur beta tant que delta = Pp-Pr > eps
c
c  entrees : vol, etar, etap, fc, tmp
c  sorties : etar, etap, pres
c           dedv,cv,heat,dpde,dpdmu
c  interne : beta
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .   tmp ,
     .   ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .   epsil,check,heat,dpdmu,dpde,enq,
     .   vol ,fc,cv,dedv,pres, beta
      integer  itrmax
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer iter,i1
      my_real
     .   delta,fc1,etac,p999,guess,pmax,
     .   trans,transr,transp,trans1,trans2,dbdeta,detar,detap,
     .   dedr,dedp,bth,dbdt,detrdt,detpdt,dpdt,dbdf,detrdf,
     .   detpdf,cvr0,cvp0,slope,tl,tu,a,b,c,
     .   bthp,dedvp,preac,pprod,dpdtp,dpdtr,bthr,dedvr,
     .   enp,enr
c
c   initialisations
      etac = ONE/vol
      fc1 = ONE -fc
      if (fc<check) then
c   que des reactifs, avant reaction
        etar = fc1 * etac
        call react(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           tmp,dedvr,pres,bthr,dpdtr,enr )
        beta = ONE
        cv = cvr
        dedv = dedvr
        dpde = dpdtr / cv
        dpdmu = bthr + dpdtr*dedv / (cv*etac**2)
        etap = EM6
        call react(ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           tmp,dedvp,pprod,bthp,dpdtp,enp )
        heat = enq-enp+enr
        return
c
      elseif (fc>(ONE -check)) then
c   que des produits, apres reaction
        etap = fc * etac
        call react(ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           tmp,dedvp,pres,bthp,dpdtp,enp )
        cv = cvp
        beta = ZERO
        heat = enq
        dedv = dedvp
        dpde = dpdtp / cv
        dpdmu = bthp + dpdtp*dedv / (cv*etac**2)
        return
c
      endif
      guess = fc1 * etap/(fc1*etap+fc*etar)
      beta = guess
      if (beta==ONE) beta = fc1
c
c   debut iterations
C=======================================================================
      do iter=1,15
        etar = fc1 * etac/beta
        call react(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           tmp,dedvr,preac,bthr,dpdtr,enr )
        etap = fc * etac/(ONE -beta)
        call react(ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           tmp,dedvp,pprod,bthp,dpdtp,enp )
        pmax = max(preac,pprod)
        delta = preac-pprod
        if (abs(delta/pmax)<=epsil) goto 800
c   Newton
        trans1 = bthr * etar/beta
        trans2 = bthp * etap/(ONE - beta)
        slope = -(trans1+trans2)
        beta = beta - delta/slope
c       dans dyna2d :
c       if ((beta<=0.).or.(beta>=1.)) beta = fc1
c       mais pb si beta=fc1 redonne un beta > 1 ...
        if (beta<=ZERO) beta = - beta
        if (beta>=ONE) beta = ONE/beta
      enddo
C=======================================================================
      iter = 2
      beta=guess - EM01
      if(beta<EM5) beta=EM5
      tl=beta
      call ftnch(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           tmp,dedvr,preac,bthr,dpdtr,enr,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dedvp,pprod,bthp,dpdtp,enp,
     .           beta,delta,fc1,etac,fc,epsil)
      b=sign(ONE,delta)
      beta=beta+ ONE_FIFTH
      beta=min(beta,ZEP9999)
      tu=beta
      call ftnch(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           tmp,dedvr,preac,bthr,dpdtr,enr,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dedvp,pprod,bthp,dpdtp,enp,
     .           beta,delta,fc1,etac,fc,epsil)
      a=sign(ONE,delta)
      if(a/=b) go to 300
      beta=ZEP9999
      tu=beta
      call ftnch(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           tmp,dedvr,preac,bthr,dpdtr,enr,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dedvp,pprod,bthp,dpdtp,enp,
     .           beta,delta,fc1,etac,fc,epsil)
      a=sign(ONE,delta)
      if (delta>ZERO) then
cc        write(*,*)'  delta (beta=0.9999999) > 0.'
cc        write(*,*)'  il faut augmenter check...'
        goto 900
      endif
C=======================================================================
      do iter = 4,itrmax+1
        beta=beta - FIVEEM2
        beta=max(beta,em6)
        tl=beta
        call ftnch(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .            tmp,dedvr,preac,bthr,dpdtr,enr,
     .            ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .            dedvp,pprod,bthp,dpdtp,enp,
     .            beta,delta,fc1,etac,fc,epsil)
        b=sign(ONE,delta)
        if(a/=b) goto 300
        tu=tl
      enddo
      goto 900
C=======================================================================
  300 i1 = iter+1
      do iter=i1,itrmax+1
        beta=HALF*(tl+tu)
        call ftnch(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .            tmp,dedvr,preac ,bthr,dpdtr,enr,
     .            ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .            dedvp,pprod,bthp,dpdtp,enp ,
     .            beta,delta,fc1,etac,fc,epsil)
        if(delta==ZERO) go to 800
        c=sign(ONE,delta)
        if(c==b) tl=beta
        if(c==a) tu=beta
      enddo
      go to 900
C=======================================================================
c
c   fin iterations
c
C=======================================================================
c   pression
  800 pres = (preac+pprod)*HALF
c
c   derivees internes
      transr = bthr / beta
      transp = bthp / (ONE - beta)
      trans = transr*etar + transp*etap
c    dbdeta = d(beta)/d(eta) a l'equil.
      dbdeta = (transr*fc1 - transp*fc) / trans
c    detar = d(etar)/d(eta) a l'equil.
      detar = (fc1 - etar*dbdeta) / beta
      detap = (fc + etap*dbdeta) / (ONE - beta)
c    dedr = d(er)/d(etar)
      dedr = -dedvr / etar**2
      dedp = -dedvp / etap**2
c    bth = d(p)/d(eta) a tmp cte
      bth = (bthr*detar + bthp*detap)*HALF
c    dbdt = d(beta)/d(tmp) a l'equil.
      dbdt = (dpdtr-dpdtp) / trans
      detrdt = -etar*dbdt/beta
      detpdt = etap*dbdt/(1.-beta)
      dpdt = (dpdtr+dpdtp+dbdt*(transp*etap-transr*etar))*HALF
c    dbdf = d(beta)/d(fc) a l'equil.
      dbdf = -(transr+transp)*etac/trans
c    detrdf = d(etar)/d(fc) a l'equil.
      detrdf = -(etac+etar*dbdf) / beta
      detpdf = (etac+etap*dbdf) / (1.-beta)
c
c   derivees rendues en sortie
c    dedv = de/dv a T et fc ctes
      dedv = -etac**2 * (fc1*dedr*detar+fc*dedp*detap)
c    cv = de/dT a v et fc ctes
      cvr0 = cvr + dedr*detrdt
      cvp0 = cvp + dedp*detpdt
      cv = fc1*cvr0 + fc*cvp0
c    heat = -de/d(fc) a T et v ctes
      heat = enq-enp+enr - fc*dedp*detpdf-fc1*dedr*detrdf
c    dpde = dp/de a v et fc ctes
      dpde = dpdt / cv
c    dpdmu = dp/d(eta) a E et fc ctes
      dpdmu = bth + dpdt*dedv/cv/etac**2
      return
c
C=======================================================================
  900 write(*,*) '** WARNING** : MIX NOT CONVERGING'
cc      write(*,*) '    etar, etap, delta = ',etar,etap,delta
      return
      end
c
c --------------------------------------------------------
c
      subroutine ftnch(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .           tmp,dedvr,preac,bthr,dpdtr,enr,
     .           ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .           dedvp,pprod,bthp,dpdtp,enp ,
     .           beta,delta,fc1,etac,fc,epsil)
c
c  entrees :  fc1, etac, beta
c  sorties :  delta, preac, pprod, etar, etap, derivees eos
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .   tmp,dedvr,preac,bthr,dpdtr,enr ,
     .   ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .   dedvp,pprod,bthp,dpdtp,enp,
     .   beta,delta,fc1,etac,fc,epsil
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   pmax
c
      etar = fc1 * etac/beta
      call react(ar ,br   ,r1r  ,r2r   ,r3r ,cvr  ,etar,
     .         tmp,dedvr,preac,bthr,dpdtr,enr )
      etap = fc * etac/(1.-beta)
      call react(ap ,bp   ,r1p  ,r2p   ,r3p ,cvp  ,etap,
     .         tmp,dedvp,pprod,bthp,dpdtp,enp )
      pmax = max(preac,pprod)
      delta = preac - pprod
      if (abs(delta/pmax)<=epsil) delta = ZERO
      return
      end
c
c ------------------------------------------------------------
c
      subroutine react(a ,b   ,r1 ,r2   ,r3 ,cv  ,eta,
     .                 tmp,dedv,p,bth,dpdt,en )
c
c  eos reactifs, jwl modifiee
c  p=a*exp(-r1/eta)+b*exp(-r2/eta)+r3*eta*tmp
c  cv = heat capacity (cte)
c
c  entrees : eta, tmp
c  sorties : p, en
c           dedv, bth, dpdt (derivees JWL)
c
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   a ,b   ,r1  ,r2 ,r3 ,cv ,eta,
     .   tmp,dedv,p,bth,dpdt,en
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real trans1,trans2
c
      trans1 = a * exp(-r1/eta)
      trans2 = b * exp(-r2/eta)
      dedv = -trans1 -trans2
      p = -dedv + r3*eta*tmp
      bth = r3*tmp + (r1*trans1+r2*trans2)/eta**2
      dpdt = r3*eta
      en = trans1/r1 + trans2/r2 + cv*tmp
      return
      end

