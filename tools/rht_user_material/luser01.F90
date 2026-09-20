! SPDX-License-Identifier: AGPL-3.0-or-later
! OpenRadioss extended solid user material ABI (USER01).
subroutine luser01(nel,nu,ns,nf,ifunc,npf,tf,time,dt,uparam,rho,volume,eint, &
                   ngl,sound,visc,uvar,off,sigy,pla,userbuf)
  use law_userso, only: ulawintbuf
  use rht_material_mod, only: wp,nparam,nstate,rht_update
  implicit none
  integer, intent(in) :: nel,nu,ns,nf,ifunc(nf),npf(*),ngl(nel)
  real(wp), intent(in) :: tf(*),time,dt,uparam(nu),rho(nel),volume(nel),eint(nel)
  real(wp), intent(out) :: sound(nel),visc(nel),sigy(nel),pla(nel)
  real(wp), intent(inout) :: uvar(nel,ns),off(nel)
  type(ulawintbuf), intent(inout) :: userbuf
  real(wp) :: deps(6),sig(6),state(nstate),old_ep,energy
  integer :: i,status,nchar
  character(len=160) :: message
  if (nu /= nparam .or. ns /= nstate) then
    message='RHT USER01: incompatible material or restart state size'
    nchar=len_trim(message)
    call write_iout(message,nchar)
    call arret(2)
  else
    do i=1,nel
      sound(i)=0.0_wp
      visc(i)=0.0_wp
      sigy(i)=0.0_wp
      pla(i)=uvar(i,2)
      userbuf%dpla(i)=0.0_wp
      sig(1:6)=0.0_wp
      if (off(i) > 0.0_wp) then
        if (rho(i) <= 0.0_wp .or. volume(i) <= 0.0_wp .or. &
            abs(userbuf%rho0(i)/uparam(1)-1.0_wp) > 1.0e-6_wp) then
          write(message,'(A,I12)') 'RHT USER01: invalid/inconsistent density or volume, element ',ngl(i)
          nchar=len_trim(message)
          call write_iout(message,nchar)
          call arret(2)
        else
          deps = [userbuf%depsxx(i),userbuf%depsyy(i),userbuf%depszz(i), &
                  userbuf%depsxy(i),userbuf%depsyz(i),userbuf%depszx(i)]
          sig = [userbuf%sigoxx(i),userbuf%sigoyy(i),userbuf%sigozz(i), &
                 userbuf%sigoxy(i),userbuf%sigoyz(i),userbuf%sigozx(i)]
          state=uvar(i,1:nstate)
          old_ep=state(2)
          ! EINT is element internal energy, NOT energy per unit mass.
          energy=eint(i)/(rho(i)*volume(i))
          call rht_update(uparam(1:nparam),dt,rho(i),energy,deps,sig,state,sound(i),status)
          if (status /= 0) then
            write(message,'(A,I12,A,I4)') 'RHT USER01: integration failed, element ',ngl(i),' status ',status
            nchar=len_trim(message)
            call write_iout(message,nchar)
            call arret(2)
          else
            uvar(i,1:nstate)=state
            pla(i)=state(2)
            sigy(i)=state(12)
            userbuf%dpla(i)=state(2)-old_ep
            if (state(16) > 0.5_wp) off(i)=0.0_wp
          end if
        end if
      end if
      userbuf%signxx(i)=sig(1)
      userbuf%signyy(i)=sig(2)
      userbuf%signzz(i)=sig(3)
      userbuf%signxy(i)=sig(4)
      userbuf%signyz(i)=sig(5)
      userbuf%signzx(i)=sig(6)
      userbuf%sigvxx(i)=0.0_wp
      userbuf%sigvyy(i)=0.0_wp
      userbuf%sigvzz(i)=0.0_wp
      userbuf%sigvxy(i)=0.0_wp
      userbuf%sigvyz(i)=0.0_wp
      userbuf%sigvzx(i)=0.0_wp
    end do
  end if
end subroutine luser01
