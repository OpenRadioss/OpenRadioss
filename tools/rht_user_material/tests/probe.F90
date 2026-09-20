! SPDX-License-Identifier: AGPL-3.0-or-later
! C ABI for independent Python verification; not part of the user library.
subroutine defaults(c) bind(C,name='defaults')
  use rht_material_mod
  use iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(out) :: c(nparam)
  call rht_defaults(c)
end subroutine

subroutine eos(c,rho,e,oldalpha,p,alpha,bulk,status) bind(C,name='eos')
  use rht_material_mod
  use iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in) :: c(nparam),rho,e,oldalpha
  real(c_double), intent(out) :: p,alpha,bulk
  integer(c_int), intent(out) :: status
  call rht_eos(c,rho,e,oldalpha,p,alpha,bulk,status)
end subroutine

subroutine surfaces(c,p,s,alpha,ep,rate,oldep,d0,f0,out) bind(C,name='surfaces')
  use rht_material_mod
  use iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in) :: c(nparam),p,s(6),alpha,ep,rate,oldep,d0,f0
  real(c_double), intent(out) :: out(6)
  call rht_surfaces(c,p,s,alpha,ep,rate,oldep,d0,f0,out(1),out(2),out(3),out(4),out(5),out(6))
end subroutine

subroutine failure(c,p,fr,s,y,pt,r3) bind(C,name='failure')
  use rht_material_mod
  use iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in) :: c(nparam),p,fr,s(6)
  real(c_double), intent(out) :: y,pt,r3
  call rht_failure(c,p,fr,y,pt)
  r3=rht_lode(c,p,s)
end subroutine

subroutine rates(c,rate,p,out) bind(C,name='rates')
  use rht_material_mod
  use iso_c_binding, only: c_double
  implicit none
  real(c_double), intent(in) :: c(nparam),rate,p
  real(c_double), intent(out) :: out(4)
  call rht_rates(c,rate,p,out(1),out(2),out(3),out(4))
end subroutine

subroutine update(c,dt,rho,e,de,sig,state,sound,status) bind(C,name='update')
  use rht_material_mod
  use iso_c_binding, only: c_double, c_int
  implicit none
  real(c_double), intent(in) :: c(nparam),dt,rho,e,de(6)
  real(c_double), intent(inout) :: sig(6),state(nstate)
  real(c_double), intent(out) :: sound
  integer(c_int), intent(out) :: status
  call rht_update(c,dt,rho,e,de,sig,state,sound,status)
end subroutine
