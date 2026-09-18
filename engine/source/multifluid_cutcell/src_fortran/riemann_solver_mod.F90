module riemann_solver_mod
  use precision_mod, only : wp                            !provides kind for eigther single or double precision (wp means working precision)
  implicit none
contains
  function fl(pL, aL, gamma, p_star)
    implicit none
    real(kind=wp) :: pL, aL, gamma, p_star
    real(kind=wp) :: fl
    if (p_star > pL) then
      ! Left shock
      fl = 2 * (aL / sqrt(2 * gamma * (gamma - 1))) * ((1 - p_star / pL) / sqrt(1 + ((gamma + 1) / (gamma - 1)) * (p_star / pL))) !Leveque (14.52)
    else
      ! Left rarefaction
      fl = 2 * (aL / (gamma - 1)) * (1 - (p_star / pL)**((gamma - 1) / (2 * gamma))) !Leveque (14.51)
    end if
  end function

  ! Gradient of function fl
  function Dfl(pL, aL, gamma, p_star)
    implicit none
    real(kind=wp) :: pL, aL, gamma, p_star
    real(kind=wp) :: Dfl
    if (p_star > pL) then
      ! Left shock
      Dfl = -2 * aL / sqrt(2 * gamma * (gamma - 1)) / sqrt(1 + (gamma + 1) / (gamma - 1) * p_star / pL) &
            * (1.0 + 0.5 * (gamma + 1) / (gamma - 1) / (1 + (gamma + 1) / (gamma - 1) * p_star / pL)) / pL
    else
      ! Left rarefaction
      Dfl = -aL / (gamma * pL) * (p_star / pL)**(-(gamma + 1) / (2 * gamma))
    end if
  end function Dfl

  function fr(pR, aR, gamma, p_star)
    implicit none
    real(kind=wp) :: pR, aR, gamma, p_star
    real(kind=wp) ::  fr
    if (p_star > pR) then
      ! Right shock
      fr = 2 * (aR / sqrt(2 * gamma * (gamma - 1))) * ((1 - p_star / pR) / sqrt(1 + ((gamma + 1) / (gamma - 1)) * (p_star / pR))) !Leveque (14.55)
    else
      ! Right rarefaction
      fr = 2 * (aR / (gamma - 1)) * ((1 - (p_star / pR)**((gamma - 1) / (2 * gamma)))) !Leveque (14.54)
    end if
  end function fr

  ! Gradient of function fr
  function Dfr(pR, aR, gamma, p_star)
    implicit none
    real(kind=wp) :: pR, aR, gamma, p_star
    real(kind=wp) ::  Dfr
    if (p_star > pR) then
      ! Left shock
      Dfr = -2 * aR / sqrt(2 * gamma * (gamma - 1)) / sqrt(1 + (gamma + 1) / (gamma - 1) * p_star / pR) &
              * (1.0 + 0.5 * (gamma + 1) / (gamma - 1) / (1 + (gamma + 1) / (gamma - 1) * p_star / pR)) / pR
    else
      ! Left rarefaction
      Dfr = -aR / (gamma * pR) * (p_star / pR)**(-(gamma + 1) / (2 * gamma))
    end if 
  end function Dfr

  ! Function to solve for pressure in the star region
  function phi(pL, aL, uL, gammaL, pR, aR, uR, gammaR, p_star)
    implicit none 
    real(kind=wp) :: pL, aL, uL 
    real(kind=wp) :: pR, aR, uR 
    real(kind=wp) :: gammaL, gammaR, p_star
    real(kind=wp) :: phi

    phi = fl(pL, aL, gammaL, p_star) + fr(pR, aR, gammaR, p_star) + uL - uR
  end function phi

  ! Gradient of function phi
  !function Dphi(pL, aL, uL, pR, aR, uR, gamma, p_star)
  function Dphi(pL, aL, gammaL, pR, aR, gammaR, p_star)
    implicit none 
    real(kind=wp) :: pL, aL 
    real(kind=wp) :: pR, aR 
    !real(kind=wp) :: uL, uR 
    real(kind=wp) :: gammaL, gammaR, p_star
    real(kind=wp) :: Dphi
   
    Dphi = Dfl(pL, aL, gammaL, p_star) + Dfr(pR, aR, gammaR, p_star)
  end function Dphi


  ! Use a Newton method to find the root
  function find_root(pL, aL, uL, gammaL, pR, aR, uR, gammaR, eps) result(p)
    implicit none 
    real(kind=wp) :: pL, aL, uL 
    real(kind=wp) :: pR, aR, uR 
    real(kind=wp) :: gammaL, gammaR, eps
    real(kind=wp) :: p 

    real(kind=wp) :: p_prev, res

    p = pL
    p_prev = pR
    do while (abs(p - p_prev) > eps)
      res = phi(pL, aL, uL, gammaL, pR, aR, uR, gammaR, p)
      p_prev = p
      p = max(1e-10, p - res / Dphi(pL, aL, gammaL, pR, aR, gammaR, p))
    end do
  end function find_root

  ! Function to solve the Riemann problem for the Euler equations and return wave velocity and flux at the interface
  ! wave-types: 1=left shock, 2=contact discontinuity, 3=right shock
  subroutine solve_riemann_problem(gammaL, gammaR, rhoL, rhoR, velyL, velyR, velzL, velzR, pL, pR, wave_type, &
                                  normalVecy, normalVecz, us, vsL, vsR, ps)
    implicit none

    real(kind=wp) :: gammaL, gammaR, rhoL, rhoR, velyL, velyR, velzL, velzR, pL, pR
    integer :: wave_type
    real(kind=wp) :: normalVecy, normalVecz
    real(kind=wp) :: us, vsL, vsR, ps

    real(kind=wp), parameter :: prec_root_find = 1e-10

    real(kind=wp) :: uL, uR, vL, vR, aL, aR
    real(kind=wp) :: SL, SR
    real(kind=wp) :: p_star, u_star
    real(kind=wp) :: rho_starL, rho_starR, v_starL, v_starR

    uL = velyL*normalVecy + velzL*normalVecz
    uR = velyR*normalVecy + velzR*normalVecz

    vL = -velyL*normalVecz + velzL*normalVecy
    vR = -velyR*normalVecz + velzR*normalVecy


    ! Compute primitive variables
    aL = sqrt(gammaL * pL / rhoL)
    aR = sqrt(gammaR * pR / rhoR)

    ! Find the pressure in the star region
    p_star = find_root(pL, aL, uL, gammaL, pR, aR, uR, gammaR, prec_root_find)

    ! Compute the velocity in the star region
    u_star = uL + fl(pL, aL, gammaL, p_star)


    !Determine left and right shock velocities
    SL = uL - aL * sqrt((gammaL + 1) / (2 * gammaL) * p_star / pL + (gammaL - 1) / (2 * gammaL))
    SR = uR + aR * sqrt((gammaR + 1) / (2 * gammaR) * p_star / pR + (gammaR - 1) / (2 * gammaR))

    ! Compute densities in the star region
    if (p_star > pL) then
      ! Left shock
      rho_starL = rhoL * ((p_star / pL) + (gammaL - 1) / (gammaL + 1)) / (((gammaL - 1) / (gammaL + 1)) * (p_star / pL) + 1)
      v_starL = SL + rhoL / rho_starL * (vL - SL)
    else
      ! Left rarefaction
      rho_starL = rhoL * (p_star / pL)**(1 / gammaL)
      v_starL = vL
    end if

    if (p_star > pR) then
      !Right shock
      rho_starR = rhoR * ((p_star / pR) + (gammaR - 1) / (gammaR + 1)) / (((gammaR - 1) / (gammaR + 1)) * (p_star / pR) + 1)
      v_starR = SR + rhoR / rho_starR * (vR - SR)
    else
      ! Right rarefaction
      rho_starR = rhoR * (p_star / pR)**(1 / gammaR)
      v_starR = vR
    end if

    if (wave_type == 2) then
      us = u_star
      vsL = v_starL
      vsR = v_starR
      ps = p_star
    elseif (wave_type == 1) then
      us = SL
      vsL = vL
      vsR = v_starL
      ps = pL
    elseif (wave_type == 3) then
      us = SR
      vsL = v_starR
      vsR = vR
      ps = pR
    end if
  end subroutine solve_riemann_problem
end module riemann_solver_mod
