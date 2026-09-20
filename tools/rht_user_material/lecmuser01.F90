! SPDX-License-Identifier: AGPL-3.0-or-later
! OpenRadioss /MAT/USER01 reader. Native density precedes the 38 constants.
subroutine lecmuser01(iin,iout,uparam,maxu,nu,ns,ifunc,maxf,nf,parmat,userbuf)
  use law_user, only: ulawbuf
  use rht_material_mod, only: wp, nparam, nstate, rht_validate
  implicit none
  integer, intent(in) :: iin,iout,maxu,maxf
  integer, intent(out) :: nu,ns,nf,ifunc(maxf)
  real(wp), intent(out) :: uparam(maxu),parmat(100)
  type(ulawbuf), intent(inout) :: userbuf
  real(wp) :: bulk,g
  integer :: ios, status
  if (maxu < nparam) then
    write(iout,*) 'RHT USER01: insufficient material parameter storage'
    call arret(2)
  else
    read(iin,*,iostat=ios) uparam(1:nparam)
    status = 1
    if (ios == 0) call rht_validate(uparam(1:nparam),status)
    if (ios /= 0 .or. status /= 0) then
      write(iout,*) 'RHT USER01: invalid 38-parameter card, material ',userbuf%id
      call arret(2)
    else
      nu = nparam
      ns = nstate
      nf = 0
      ifunc(1:maxf) = 0
      bulk = uparam(32)/uparam(38)
      g = uparam(2)
      parmat(1:100) = 0.0_wp
      parmat(1) = bulk
      parmat(2) = 9*bulk*g/(3*bulk+g)
      parmat(3) = (3*bulk-2*g)/(2*(3*bulk+g))
      parmat(16) = 2.0_wp
      parmat(17) = 2*g/(bulk+4*g/3)
      write(iout,*) 'RHT USER01: Borrvall-Riedel three-surface concrete, solid elements'
      write(iout,*) 'RHT USER01: rho, shear, fc = ',uparam(1),uparam(2),uparam(10)
    end if
  end if
end subroutine lecmuser01
