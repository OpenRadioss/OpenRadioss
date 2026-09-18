module FV_fluxes
  implicit none
  
  contains

  subroutine FV_flux_hllc_Euler(gamma, rhoL, rhoR, velyL, velyR, velzL, velzR, pL, pR, normalVec, rho, rhovy, rhovz, rhoE)
    use grid2D_struct_multicutcell_mod
    use polygon_cutcell_mod
    use precision_mod, only : wp                            !provides kind for either single or double precision (wp means working precision)

    implicit none

    real(kind=wp), intent(in) :: gamma, rhoL, rhoR, velyL, velyR, velzL, velzR, pL, pR
    type(Point2D), intent(in) :: normalVec
    real(kind=wp), intent(out) :: rho, rhovy, rhovz, rhoE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! DUMMY VARIABLES
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(kind=wp) :: normal_vel_L, tang_vel_L, normal_vel_R, tang_vel_R !Projected velocities, normal and tangential
    real(kind=wp) :: rhoEL, rhoER, SL, SR, S_star
    real(kind=wp) :: rho_star, rhou_star, rhov_star, rhoE_star
    real(kind=wp) :: aL, aR

    normal_vel_L = velyL*normalVec%y + velzL*normalVec%z
    normal_vel_R = velyR*normalVec%y + velzR*normalVec%z
    tang_vel_L = -velyL*normalVec%z + velzL*normalVec%y
    tang_vel_R = -velyR*normalVec%z + velzR*normalVec%y

    !Compute left and right speeds of sound
    aL = sqrt(gamma * pL / rhoL)
    aR = sqrt(gamma * pR / rhoR)
  
    !Compute conservative variables
    rhoEL = 0.5*rhoL*(normal_vel_L*normal_vel_L + tang_vel_L*tang_vel_L) + pL/(gamma-1) !energy
    rhoER = 0.5*rhoR*(normal_vel_R*normal_vel_R + tang_vel_R*tang_vel_R) + pR/(gamma-1)
  
    !Compute wave speeds
    SL = min(normal_vel_L - aL, normal_vel_R - aR)
    SR = max(normal_vel_L + aL, normal_vel_R + aR)
  
    !Compute the contact wave speed
    S_star = (pR - pL + rhoL * normal_vel_L * (SL - normal_vel_L) - rhoR * normal_vel_R * (SR - normal_vel_R)) / &
              (rhoL * (SL - normal_vel_L) - rhoR * (SR - normal_vel_R)) !Equation (37) in Toro slides
  
    !Compute the HLLC flux
    if (0 <= SL) then
      !return flux(UL, normalVec)
      rho   = rhoL * normal_vel_L
      rhovy = rhoL * velyL * normal_vel_L + pL * normalVec%y
      rhovz = rhoL * velzL * normal_vel_L + pL * normalVec%z
      rhoE  = normal_vel_L * (0.5 * rhoL * (velyL*velyL + velzL*velzL) + gamma / (gamma - 1) * pL)
    elseif (SL <= 0 .and. 0 <= S_star) then
      ! Compute the left and right star region states: Equation (39) in Toro slides
      rho_star  = rhoL * (SL - normal_vel_L) / (SL - S_star)
      rhou_star = rho_star * S_star
      rhov_star = rho_star * tang_vel_L
      rhoE_star = (SL - normal_vel_L) / (SL - S_star) * (rhoEL + rhoL * (S_star - normal_vel_L) * &
                  (S_star + pL / (rhoL * (SL - normal_vel_L))))
    
      ! Compute the fluxes in the star region
      rho   = rhoL * normal_vel_L + SL * (rho_star - rhoL)
      rhovy = rhoL * velyL * normal_vel_L + pL * normalVec%y + &
              SL*((rhou_star - rhoL * normal_vel_L) * normalVec%y - (rhov_star - rhoL * tang_vel_L) * normalVec%z)
      rhovz = rhoL * velzL * normal_vel_L + pL * normalVec%z + &
              SL*((rhou_star - rhoL * normal_vel_L) * normalVec%z + (rhov_star - rhoL * tang_vel_L) * normalVec%y)
      rhoE  = normal_vel_L * (0.5 * rhoL * (velyL*velyL + velzL*velzL) + gamma / (gamma - 1) * pL) + SL*(rhoE_star - rhoEL)

    elseif (S_star <= 0 .and. 0 <= SR) then
      ! Compute the left and right star region states: Equation (39) in Toro slides
      rho_star  = rhoR * (SR - normal_vel_R) / (SR - S_star)
      rhou_star = rho_star * S_star
      rhov_star = rho_star * tang_vel_R
      rhoE_star = (SR - normal_vel_R) / (SR - S_star) * (rhoER + rhoR * (S_star - normal_vel_R) * &
                  (S_star + pR / (rhoR * (SR - normal_vel_R))))
    
      ! Compute the fluxes in the star region
      rho   = rhoR * normal_vel_R + SR * (rho_star - rhoR)
      rhovy = rhoR * velyR * normal_vel_R + pR * normalVec%y + &
              SR*((rhou_star - rhoR * normal_vel_R) * normalVec%y - (rhov_star - rhoR * tang_vel_R) * normalVec%z)
      rhovz = rhoR * velzR * normal_vel_R + pR * normalVec%z + &
              SR*((rhou_star - rhoR * normal_vel_R) * normalVec%z + (rhov_star - rhoR * tang_vel_R) * normalVec%y)
      rhoE  = normal_vel_R * (0.5 * rhoR * (velyR*velyR + velzR*velzR) + gamma / (gamma - 1) * pR) + SR*(rhoE_star - rhoER)
    else
      rho   = rhoR * normal_vel_R
      rhovy = rhoR * velyR * normal_vel_R + pR * normalVec%y
      rhovz = rhoR * velzR * normal_vel_R + pR * normalVec%z
      rhoE  = normal_vel_R * (0.5 * rhoR * (velyR*velyR + velzR*velzR) + gamma / (gamma - 1) * pR)
    end if
  end subroutine FV_flux_hllc_Euler

end module FV_fluxes