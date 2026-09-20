! SPDX-License-Identifier: AGPL-3.0-or-later
program test_adapter
  use rht_material_mod
  use law_userso, only: ulawintbuf
  use law_user, only: ulawbuf
  implicit none
  integer, parameter :: nel=3
  type(ulawintbuf) :: buf
  type(ulawbuf) :: stbuf
  real(wp) :: c(nparam),readback(nparam),parmat(100),rho(nel),vol(nel),energy(nel)
  real(wp) :: sound(nel),visc(nel),history(nel,nstate),off(nel),sigy(nel),pla(nel)
  real(wp) :: expected(6,nel),hs(nstate,nel),cs(nel),de(6),e,tf(1)
  integer :: i,status,ngl(nel),ifunc(1),npf(1),nu,ns,nf,iu
  call rht_defaults(c)
  stbuf%id=123
  open(newunit=iu,status='scratch',form='formatted')
  write(iu,*) c
  rewind(iu)
  call lecmuser01(iu,6,readback,nparam,nu,ns,ifunc,1,nf,parmat,stbuf)
  close(iu)
  if (nu /= nparam .or. ns /= nstate .or. nf /= 0) stop 1
  if (maxval(abs(readback-c)) > 1.0e-10_wp) stop 2
  if (abs(parmat(1)/(c(32)/c(38))-1.0_wp) > 1.0e-12_wp) stop 3
  if (abs(parmat(2)/(2*(1+parmat(3)))/c(2)-1.0_wp) > 1.0e-12_wp) stop 4
  rho=[c(1),c(1),c(1)*1.01_wp]
  vol=[0.001_wp,0.002_wp,0.003_wp]
  energy=[1.0_wp,0.0_wp,100.0_wp]
  ngl=[11,22,33]
  off=[1.0_wp,0.0_wp,1.0_wp]
  history=0.0_wp
  tf=0.0_wp
  npf=0
  ifunc=0
  buf%rho0(1:nel)=c(1)
  buf%depsxx(1:nel)=0.0_wp
  buf%depsyy(1:nel)=0.0_wp
  buf%depszz(1:nel)=0.0_wp
  buf%depsxy(1:nel)=[1.0e-5_wp,2.0e-5_wp,0.0_wp]
  buf%depsyz(1:nel)=[0.0_wp,0.0_wp,3.0e-5_wp]
  buf%depszx(1:nel)=0.0_wp
  buf%sigoxx(1:nel)=0.0_wp
  buf%sigoyy(1:nel)=0.0_wp
  buf%sigozz(1:nel)=0.0_wp
  buf%sigoxy(1:nel)=0.0_wp
  buf%sigoyz(1:nel)=0.0_wp
  buf%sigozx(1:nel)=0.0_wp
  expected=0.0_wp
  hs=0.0_wp
  cs=0.0_wp
  do i=1,nel
    if (off(i) > 0) then
      de=[0.0_wp,0.0_wp,0.0_wp,buf%depsxy(i),buf%depsyz(i),0.0_wp]
      e=energy(i)/(rho(i)*vol(i))
      call rht_update(c,1.0e-6_wp,rho(i),e,de,expected(:,i),hs(:,i),cs(i),status)
      if (status /= 0) then
        print *, 'Reference point failed: ',i,status
        stop 5
      end if
    end if
  end do
  call luser01(nel,nparam,nstate,0,ifunc,npf,tf,1.0e-6_wp,1.0e-6_wp,c,rho,vol,energy, &
               ngl,sound,visc,history,off,sigy,pla,buf)
  do i=1,nel
    de=[buf%signxx(i),buf%signyy(i),buf%signzz(i),buf%signxy(i),buf%signyz(i),buf%signzx(i)]
    if (maxval(abs(de-expected(:,i))) > 1.0e-7_wp) stop 6
    if (maxval(abs(history(i,:)-hs(:,i))) > 1.0e-7_wp) stop 7
    if (abs(sound(i)-cs(i)) > 1.0e-7_wp) stop 8
    if (pla(i) /= hs(2,i) .or. buf%dpla(i) /= hs(2,i)) stop 9
    if (visc(i) /= 0.0_wp) stop 10
  end do
  if (off(2) /= 0.0_wp .or. off(1) /= 1.0_wp .or. off(3) /= 1.0_wp) stop 11
  print *, 'PASS: Starter parameters, PARMAT, batched state, shear order, energy units, inactive element'
end program test_adapter

subroutine arret(code)
  implicit none
  integer, intent(in) :: code
  print *, 'Unexpected ARRET in adapter test: ',code
  stop 99
end subroutine arret

subroutine write_iout(message,nchar)
  implicit none
  integer, intent(in) :: nchar
  character(len=*), intent(in) :: message
  print *, message(1:nchar)
end subroutine write_iout
