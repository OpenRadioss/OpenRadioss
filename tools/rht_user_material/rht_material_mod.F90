! SPDX-License-Identifier: AGPL-3.0-or-later
! RHT equations: Borrvall and Riedel, 8th European LS-DYNA Conference,
! 2011, section 5; LS-DYNA R16 Material Manual, MAT_272.
! See README.md for the integration choices and verification scope.
module rht_material_mod
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
#ifdef MYREAL4
  integer, parameter, public :: wp = kind(1.0)
#else
  integer, parameter, public :: wp = kind(1.0d0)
#endif
  integer, parameter, public :: nparam = 38, nstate = 16
  real(wp), parameter :: zero = 0.0_wp, one = 1.0_wp
  real(wp), parameter :: tol = max(2.0e-10_wp, 32*epsilon(one))
  public :: rht_defaults, rht_validate, rht_eos, rht_rates
  public :: rht_lode, rht_failure, rht_surfaces, rht_update
contains
  subroutine rht_defaults(c)
    real(wp), intent(out) :: c(nparam)
    ! SI: m, kg, s, Pa. No hidden conversion or zero-as-default semantics.
    c = [2314.0_wp, 1.67e10_wp, 1.0e6_wp, 2.0_wp, &
         1.22_wp, 1.22_wp, 3.527e10_wp, 1.6_wp, 0.61_wp, &
         3.5e7_wp, 0.18_wp, 0.10_wp, 0.6805_wp, 0.0105_wp, &
         zero, 3.0e-5_wp, 3.0e-6_wp, 3.0e25_wp, 3.0e25_wp, &
         0.032_wp, 0.036_wp, 0.001_wp, 0.53_wp, 0.70_wp, &
         0.50_wp, 0.04_wp, one, 0.01_wp, 1.6_wp, 0.61_wp, &
         zero, 3.527e10_wp, 3.958e10_wp, 9.04e9_wp, &
         2.33e7_wp, 6.0e9_wp, 3.0_wp, 1.1884_wp]
  end subroutine rht_defaults

  subroutine rht_validate(c, status)
    real(wp), intent(in) :: c(nparam)
    integer, intent(out) :: status
    real(wp) :: q1
    status = 0
    if (.not. all(ieee_is_finite(c))) status = 1
    if (min(c(1), c(2), c(3), c(7), c(8), c(9), c(10), c(11), c(12)) <= zero) status = 1
    if (c(9) > one .or. c(13) <= 0.5_wp .or. c(13) > one .or. c(14) < zero) status = 1
    if (min(c(16),c(17)) <= zero .or. c(18) < c(16) .or. c(19) < c(17)) status = 1
    if (min(c(20),c(21)) < zero .or. max(c(20),c(21)) > one/3) status = 1
    if (c(22) <= zero .or. c(22) > one) status = 1
    if (min(c(23),c(24),c(25),c(26),c(27),c(28),c(30)) <= zero) status = 1
    if (max(c(23),c(24),c(25)) > one .or. min(c(4),c(5),c(6),c(29),c(31)) < zero) status = 1
    if (c(32) <= zero .or. min(c(33),c(34),c(35)) < zero) status = 1
    if (c(36) <= c(35) .or. c(37) < one .or. c(38) < one) status = 1
    if (status == 0) then
      q1 = lode_factor(sqrt(3.0_wp)/2, c(13))
      if (c(13)*c(11) <= q1*c(12)) status = 1
    end if
  end subroutine rht_validate

  pure real(wp) function lode_factor(costheta, q)
    real(wp), intent(in) :: costheta, q
    real(wp) :: a, b, rad
    a = one-q*q
    b = 2*q-one
    rad = max(zero, 4*a*costheta**2+5*q*q-4*q)
    lode_factor = (2*a*costheta+b*sqrt(rad))/(4*a*costheta**2+b*b)
  end function lode_factor

  pure real(wp) function rht_lode(c, pstar, s)
    real(wp), intent(in) :: c(nparam), pstar, s(6)
    real(wp) :: sn(6), scale, vm, det, cos3, theta, q
    scale = maxval(abs(s))
    q = min(one, max(0.5_wp+epsilon(one), c(13)+c(14)*pstar))
    rht_lode = one
    if (scale > tiny(one)) then
      sn = s/scale
      vm = sqrt(1.5_wp*(sum(sn(1:3)**2)+2*sum(sn(4:6)**2)))
      det = sn(1)*sn(2)*sn(3)+2*sn(4)*sn(5)*sn(6) &
            -sn(1)*sn(5)**2-sn(2)*sn(6)**2-sn(3)*sn(4)**2
      cos3 = max(-one, min(one, 13.5_wp*det/vm**3))
      theta = acos(cos3)/3
      rht_lode = lode_factor(cos(theta),q)
    end if
  end function rht_lode

  pure real(wp) function rate_branch(rate, ref, brk, beta)
    real(wp), intent(in) :: rate, ref, brk, beta
    if (rate <= brk) then
      rate_branch = (max(rate,ref)/ref)**beta
    else
      rate_branch = (brk/ref)**beta*(rate/brk)**(one/3)
    end if
  end function rate_branch

  pure subroutine rht_rates(c, rate, pstar, frc, frt, fr, fe)
    real(wp), intent(in) :: c(nparam), rate, pstar
    real(wp), intent(out) :: frc, frt, fr, fe
    real(wp) :: mix
    frc = rate_branch(rate,c(16),c(18),c(20))
    frt = rate_branch(rate,c(17),c(19),c(21))
    mix = max(zero,min(one,(frc-3*pstar)/(frc+frt*c(12))))
    fr = frc+mix*(frt-frc)
    mix = max(zero,min(one,(frc*c(23)-3*pstar)/(frc*c(23)+frt*c(24)*c(12))))
    fe = c(23)+mix*(c(24)-c(23))
  end subroutine rht_rates

  pure subroutine rht_failure(c, pstar, fr, strength, pt)
    real(wp), intent(in) :: c(nparam), pstar, fr
    real(wp), intent(out) :: strength, pt
    real(wp) :: q1, q2, slope
    q1 = lode_factor(sqrt(3.0_wp)/2,c(13))
    q2 = min(one,max(0.5_wp+epsilon(one),c(13)+c(14)*pstar))
    slope = 3*(c(11)/(q1*c(12))-one/q2)
    pt = -fr*c(11)/(q1*slope)
    if (3*pstar >= fr) then
      strength = c(8)*(pstar-fr/3+(fr/c(8))**(one/c(9)))**c(9)
    else if (pstar >= zero) then
      strength = fr*c(11)/q1+3*pstar*(one-c(11)/q1)
    else
      strength = max(zero,fr*c(11)/q1+slope*pstar)
    end if
  end subroutine rht_failure

  pure real(wp) function crush_pressure(c, alpha)
    real(wp), intent(in) :: c(nparam), alpha
    crush_pressure = c(36)
    if (c(38) > one) then
      crush_pressure = c(36)-(c(36)-c(35))* &
        (max(zero,alpha-one)/(c(38)-one))**(one/c(37))
    end if
  end function crush_pressure

  pure subroutine solid_eos(c, rho, energy, alpha, pressure, bulk, thermal_rho)
    real(wp), intent(in) :: c(nparam), rho, energy, alpha
    real(wp), intent(out) :: pressure, bulk
    real(wp), intent(in), optional :: thermal_rho
    real(wp) :: eta, poly, dp, term, rhot, dpde_rho
    rhot=rho
    if (present(thermal_rho)) rhot=thermal_rho
    eta = alpha*rho/(c(38)*c(1))-one
    if (c(5) > zero) then
      if (eta >= zero) then
        poly = eta*(c(32)+eta*(c(33)+eta*c(34)))
        dp = c(32)+eta*(2*c(33)+3*c(34)*eta)
        term = c(5)+c(6)*eta
        pressure = poly/alpha+term*rhot*energy
        bulk = (one+eta)*dp/alpha + rhot*energy*(term+c(6)*(one+eta))
        dpde_rho=term
      else
        poly = eta*(c(7)+c(15)*eta)
        dp = c(7)+2*c(15)*eta
        pressure = poly/alpha+c(5)*rhot*energy
        bulk = (one+eta)*dp/alpha+c(5)*rhot*energy
        dpde_rho=c(5)
      end if
    else
      poly = eta*(c(32)+eta*(c(33)+eta*c(34)))
      dp = c(32)+eta*(2*c(33)+3*c(34)*eta)
      term = one-c(31)*eta/2
      pressure = poly*term/alpha+c(31)*rhot*energy
      bulk = (one+eta)*(dp*term-c(31)*poly/2)/alpha+c(31)*rhot*energy
      dpde_rho=c(31)
    end if
    ! Isentropic acoustic modulus includes (p/rho)*dp/de. Retaining
    ! the fixed-energy bound when p<0 makes the time-step estimate conservative.
    bulk=bulk+dpde_rho*max(pressure,zero)
  end subroutine solid_eos

  subroutine rht_eos(c, rho, energy, oldalpha, pressure, alpha, bulk, status, thermal_rho)
    real(wp), intent(in) :: c(nparam), rho, energy, oldalpha
    real(wp), intent(out) :: pressure, alpha, bulk
    integer, intent(out) :: status
    real(wp), intent(in), optional :: thermal_rho
    real(wp) :: lo, hi, mid, residual
    integer :: i
    alpha = max(one,min(c(38),oldalpha))
    call solid_eos(c,rho,energy,alpha,pressure,bulk,thermal_rho)
    if (pressure > crush_pressure(c,alpha) .and. alpha > one) then
      hi = alpha
      lo = one
      call solid_eos(c,rho,energy,lo,pressure,bulk,thermal_rho)
      if (pressure < c(36)) then
        do i=1,80
          mid = (hi+lo)/2
          call solid_eos(c,rho,energy,mid,pressure,bulk,thermal_rho)
          residual = pressure-crush_pressure(c,mid)
          if (residual > zero) then
            hi = mid
          else
            lo = mid
          end if
          if (hi-lo <= tol*mid) exit
        end do
      end if
      alpha = (lo+hi)/2
      if (pressure >= c(36)) alpha = one
      call solid_eos(c,rho,energy,alpha,pressure,bulk,thermal_rho)
    end if
    ! The fixed-porosity modulus bounds the softer active crush tangent.
    ! This is the modulus used for an explicit stability estimate.
    status = 0
    if (.not. ieee_is_finite(pressure) .or. .not. ieee_is_finite(bulk)) status = 2
    if (rho <= zero .or. bulk <= zero) status = 2
  end subroutine rht_eos

  subroutine rht_surfaces(c, p, s, alpha, ep, rate, oldep, olddamage, oldfailed, &
                          y, hard, damage, failed, pcut, fr, stage)
    real(wp), intent(in) :: c(nparam), p, s(6), alpha, ep, rate, oldep, olddamage, oldfailed
    real(wp), intent(out) :: y, hard, damage, failed, pcut, fr
    integer, intent(in), optional :: stage
    real(wp) :: ps, frc, frt, fe, cap, pc, pu, r3, yf, pt, epsh, gamma, base
    real(wp) :: lo, hi, fm, residual, epdamage, epfail, yr
    integer :: i, branch
    branch=0
    if (present(stage)) branch=stage
    ps = p/c(10)
    call rht_rates(c,rate,ps,frc,frt,fr,fe)
    r3 = rht_lode(c,ps,s)
    call rht_failure(c,ps,fr,yf,pt)
    yf = c(10)*yf*r3
    pc = crush_pressure(c,alpha)/c(10)
    pu = frc*c(23)/3+c(25)*c(2)*ep/c(10)
    cap = one
    if (ps >= pc) then
      cap = zero
    else if (ps > pu .and. pc > pu) then
      cap = sqrt(max(zero,one-((ps-pu)/(pc-pu))**2))
    end if
    base = fe*cap
    epsh = yf*(one-base)/(3*c(25)*c(2))
    failed = oldfailed
    hard = one
    damage = olddamage
    pcut = pt*c(10)
    if (ep >= epsh .or. oldfailed > 0.5_wp .or. branch == 2) then
      epdamage = max(zero,ep-max(oldep,epsh))
      if (oldfailed > 0.5_wp) epdamage = max(zero,ep-oldep)
      if (epdamage > zero .or. oldfailed > 0.5_wp .or. branch == 2) failed = one
      epfail = max(c(28),c(26)*max(zero,ps-(one-olddamage)*pt)**c(27))
      damage = min(one,olddamage+epdamage/epfail)
      y = yf
      if (failed > 0.5_wp .and. branch /= 1) then
        if (p >= zero) then
          yr = c(10)*c(29)*ps**c(30)
          y = (one-damage)*yf+damage*yr
        else
          y = yf*max(zero,one-damage-ps/pt)
        end if
        pcut = (one-damage)*pt*c(10)
      end if
    else
      ! Solve the implicit normalized hardening strain from the published
      ! homothetic yield surface. No pressure-independent hardening shortcut.
      lo = base
      hi = one
      do i=1,120
        gamma = (lo+hi)/2
        call rht_failure(c,ps/gamma,fr,fm,pt)
        residual = c(10)*fm*r3*(gamma-base)-3*c(25)*c(2)*ep
        if (residual > zero) then
          hi = gamma
        else
          lo = gamma
        end if
        ! Near a closed cap gamma can be tiny and y scales as gamma**(1-N).
        ! A fixed absolute gamma tolerance would introduce a spurious stress
        ! floor. Control the constitutive residual in stress units instead.
        if (abs(residual) <= tol*max(c(10)*1.0e-6_wp,3*c(25)*c(2)*ep)) exit
      end do
      call rht_failure(c,ps/gamma,fr,fm,pt)
      y = c(10)*fm*r3*gamma
      hard = (gamma-base)/max(one-base,epsilon(one))
      pcut = gamma*pt*c(10)
    end if
  end subroutine rht_surfaces

  subroutine trial_at_multiplier(c, rho, energy, dt, trial, old, z, sig, state, f, bulk, status, stage)
    real(wp), intent(in) :: c(nparam), rho, energy, dt, trial(6), old(nstate), z
    real(wp), intent(out) :: sig(6), state(nstate), f, bulk
    integer, intent(out) :: status
    integer, intent(in), optional :: stage
    real(wp) :: p, alpha, rhomat, p0, plo, phi, pmid, peos, bulk0, evp, dpv
    real(wp) :: q, dep, rate, y, h, d, failed, pcut, fr, s(6), lambda
    integer :: i
    lambda = z/(2*c(2))
    rhomat = rho*exp(old(4))
    call rht_eos(c,rhomat,energy,old(1),p,alpha,bulk,status,rho)
    bulk0 = bulk
    p0 = p
    dpv = zero
    if (p0 < zero .and. z > zero) then
      ! Backward Euler flow d(ep) = lambda*(s - PTF*p*I).
      ! The elastic density rho*exp(ep_vol) separates tensile plastic
      ! volume from the EOS, while alpha stores irreversible pore collapse.
      plo = p0
      phi = zero
      do i=1,80
        pmid = (plo+phi)/2
        dpv = -3*c(22)*lambda*pmid
        rhomat = rho*exp(old(4)+dpv)
        call solid_eos(c,rhomat,energy,old(1),peos,bulk,rho)
        if (pmid > peos) then
          phi = pmid
        else
          plo = pmid
        end if
        if (phi-plo <= tol*c(10)) exit
      end do
      p = (plo+phi)/2
      dpv = -3*c(22)*lambda*p
      alpha = old(1)
      rhomat = rho*exp(old(4)+dpv)
      call solid_eos(c,rhomat,energy,alpha,peos,bulk,rho)
    end if
    evp = old(4)+dpv
    s = trial/(one+z)
    q = sqrt(1.5_wp*(sum(s(1:3)**2)+2*sum(s(4:6)**2)))
    dep = sqrt((2*lambda*q/3)**2+2*dpv**2/9)
    rate = dep/dt
    call rht_surfaces(c,p,trial,alpha,old(2)+dep,rate,old(2),old(3),old(6), &
                      y,h,d,failed,pcut,fr,stage)
    ! The tensile apex needs a pressure condition as well as q <= y.
    f = max(q-y,pcut-p)
    sig = s
    sig(1:3) = sig(1:3)-p
    state = old
    state(1:14) = [alpha,old(2)+dep,d,evp,h,failed,rho,p,rate,q,fr,y, &
                    log(c(38)/alpha),old(14)+2*lambda*q/3]
    state(15) = energy
    bulk = max(bulk,bulk0)
    if (.not. all(ieee_is_finite(state)) .or. .not. all(ieee_is_finite(sig))) status = 3
  end subroutine trial_at_multiplier

  subroutine tensile_endpoint(c,rho,energy,dt,trial,old,sig,state,bulk,accepted,status)
    ! At full tensile damage, both p and q vanish. The stress-directed
    ! plastic multiplier tends to infinity, but the plastic strain remains
    ! finite. Evaluate that limiting return without an arbitrary stress floor.
    real(wp), intent(in) :: c(nparam),rho,energy,dt,trial(6),old(nstate)
    real(wp), intent(out) :: sig(6),state(nstate),bulk
    logical, intent(out) :: accepted
    integer, intent(out) :: status
    real(wp) :: p,alpha,lo,hi,mid,bulk0,dpv,depdev,dep,y,h,d,failed,pcut,fr
    integer :: i
    accepted=.false.
    call rht_eos(c,rho*exp(old(4)),energy,old(1),p,alpha,bulk,status,rho)
    bulk0=bulk
    state=old
    sig=trial
    if (status == 0 .and. p <= zero) then
      lo=log(rho)+old(4)
      hi=max(lo,log(c(38)*c(1)/old(1)))
      do i=1,12
        call solid_eos(c,exp(hi),energy,old(1),p,bulk,rho)
        if (p >= zero) exit
        hi=hi+one
      end do
      do i=1,80
        mid=(lo+hi)/2
        call solid_eos(c,exp(mid),energy,old(1),p,bulk,rho)
        if (p > zero) then
          hi=mid
        else
          lo=mid
        end if
        if (hi-lo < tol*0.01_wp) exit
      end do
      dpv=max(zero,(lo+hi)/2-log(rho)-old(4))
      depdev=sqrt(1.5_wp*(sum(trial(1:3)**2)+2*sum(trial(4:6)**2)))/(3*c(2))
      dep=sqrt(depdev**2+2*dpv**2/9)
      call rht_surfaces(c,zero,trial,old(1),old(2)+dep,dep/dt,old(2),old(3),old(6), &
                        y,h,d,failed,pcut,fr,2)
      if (d >= one) then
        sig=zero
        state(1:14)=[old(1),old(2)+dep,one,old(4)+dpv,one,one,rho,zero, &
                     dep/dt,zero,fr,zero,log(c(38)/old(1)),old(14)+depdev]
        state(15)=energy
        accepted=.true.
      end if
    end if
    bulk=max(bulk,bulk0)
  end subroutine tensile_endpoint

  subroutine single_step(c, dt, rho, energy, deps, oldsig, old, sig, state, sound, status)
    real(wp), intent(in) :: c(nparam), dt, rho, energy, deps(6), oldsig(6), old(nstate)
    real(wp), intent(out) :: sig(6), state(nstate), sound
    integer, intent(out) :: status
    real(wp) :: trial(6), trace, mean, bulk, f, lo, hi, mid, scale
    integer :: i,stage
    logical :: endpoint
    trace = sum(deps(1:3))
    mean = sum(oldsig(1:3))/3
    trial(1:3) = oldsig(1:3)-mean+2*c(2)*(deps(1:3)-trace/3)
    trial(4:6) = oldsig(4:6)+c(2)*deps(4:6)
    scale = max(c(10),maxval(abs(trial)))
    call tensile_endpoint(c,rho,energy,dt,trial,old,sig,state,bulk,endpoint,status)
    ! Solve the pre-peak branch first, then the post-peak branch if its
    ! plastic strain reaches the failure surface. In tension the published
    ! post-peak equation has a finite strength drop even at D=0. Switching
    ! branches inside one scalar bisection can converge to that jump rather
    ! than to a stress-consistent root (especially with plastic-rate coupling).
    do stage=1,2
      if (endpoint .or. status /= 0) exit
      if (stage == 1 .and. old(6) > 0.5_wp) cycle
      call trial_at_multiplier(c,rho,energy,dt,trial,old,zero,sig,state,f,bulk,status,stage)
      if (f > tol*scale .and. status == 0) then
        lo = zero
        hi = 0.01_wp
        do i=1,70
          call trial_at_multiplier(c,rho,energy,dt,trial,old,hi,sig,state,f,bulk,status,stage)
          if (f <= zero .or. status /= 0) exit
          hi = 2*hi
        end do
        if (f > tol*scale) status = 4
        if (status == 0) then
          do i=1,100
            mid = (lo+hi)/2
            call trial_at_multiplier(c,rho,energy,dt,trial,old,mid,sig,state,f,bulk,status,stage)
            if (abs(f) <= tol*scale .or. status /= 0) exit
            if (f > zero) then
              lo = mid
            else
              hi = mid
            end if
          end do
          if (abs(f) > 10*tol*scale) status = 4
        end if
      end if
      if (status /= 0 .or. state(6) < 0.5_wp) exit
    end do
    sound = sqrt(max(zero,bulk+4*c(2)/3)/rho)
    if (state(3)-old(3) > 0.025_wp) status = 5
    if (c(4) > zero .and. state(2) >= c(4)) then
      state(16) = one
      sig = zero
    end if
  end subroutine single_step

  subroutine rht_update(c, dt, rho, energy, deps, sig, state, sound, status)
    real(wp), intent(in) :: c(nparam), dt, rho, energy, deps(6)
    real(wp), intent(inout) :: sig(6), state(nstate)
    real(wp), intent(out) :: sound
    integer, intent(out) :: status
    real(wp) :: initial(nstate), work(nstate), next(nstate), ss(6), sn(6), de(6)
    real(wp) :: fraction, rsub, esub, cs, rhobegin, p, alpha, bulk
    integer :: ns, i, attempt
    call rht_validate(c,status)
    if (dt < zero .or. rho <= zero .or. .not. ieee_is_finite(dt) .or. &
        .not. ieee_is_finite(rho) .or. .not. ieee_is_finite(energy)) status = 1
    if (.not. all(ieee_is_finite(deps)) .or. .not. all(ieee_is_finite(sig)) .or. &
        .not. all(ieee_is_finite(state))) status = 1
    sound = zero
    if (status == 0) then
      initial = state
      if (initial(1) <= zero .and. all(abs(initial) <= tiny(one))) then
        initial = zero
        initial(1) = c(38)
        initial(7) = c(1)
        initial(11) = one
      end if
      if (initial(1) < one .or. initial(1) > c(38) .or. initial(7) <= zero) status = 1
      if (initial(2) < zero .or. initial(3) < zero .or. initial(3) > one .or. initial(4) < zero) status = 1
      if (dt <= zero .and. status == 0) then
        ! Engine makes a zero-time call to obtain the initial time step.
        ! Do not evolve stress, plasticity, damage or history on this call.
        call rht_eos(c,rho*exp(initial(4)),energy,initial(1),p,alpha,bulk,status,rho)
        sound = sqrt(max(zero,bulk+4*c(2)/3)/rho)
      else if (initial(16) > 0.5_wp) then
        sig = zero
      else if (status == 0) then
        rhobegin = initial(7)
        ns = max(1,ceiling(max(maxval(abs(deps)),abs(log(rho/rhobegin)))/0.001_wp))
        do attempt=1,12
          if (ns > 65536) then
            status = 6
            exit
          end if
          work = initial
          ss = sig
          de = deps/ns
          sound = zero
          do i=1,ns
            fraction = real(i,wp)/ns
            rsub = rhobegin*exp(fraction*log(rho/rhobegin))
            esub = initial(15)+fraction*(energy-initial(15))
            call single_step(c,dt/ns,rsub,esub,de,ss,work,sn,next,cs,status)
            if (status /= 0) exit
            sound = max(sound,cs)
            ss = sn
            work = next
            if (work(16) > 0.5_wp) exit
          end do
          if (status == 0) exit
          ns = ns*2
        end do
        if (status == 0) then
          sig = ss
          state = work
        end if
      end if
    end if
  end subroutine rht_update
end module rht_material_mod
