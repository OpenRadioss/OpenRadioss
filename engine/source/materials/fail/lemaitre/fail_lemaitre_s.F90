!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module fail_lemaitre_s_mod
      contains
      subroutine fail_lemaitre_s(                                              &
        nel      ,nuparam  ,uparam   ,time     ,ngl      ,matparam ,           &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        dpla     ,pla      ,loff     ,off      ,dfmax    ,tdele    ,           &
        niparam  ,iparam   ,dmgscl   ,noff     ,npg      )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use constant_mod
      use matparam_def_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
#include "my_real.inc"
#include "units_c.inc"
!-----------------------------------------------
!   I N P U T   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)                     :: nel      !< Number of elements
      integer, intent(in)                     :: nuparam  !< Number of real parameters
      my_real, dimension(nuparam), intent(in) :: uparam   !< Real parameters
      my_real, intent(in)                     :: time     !< Current time
      integer, dimension(nel), intent(in)     :: ngl      !< Global element numbers
      type(matparam_struct_) , intent(in)     :: matparam !< Material parameters data structure
      my_real, dimension(nel), intent(inout)  :: signxx   !< Stress xx
      my_real, dimension(nel), intent(inout)  :: signyy   !< Stress yy
      my_real, dimension(nel), intent(inout)  :: signzz   !< Stress zz
      my_real, dimension(nel), intent(inout)  :: signxy   !< Stress xy
      my_real, dimension(nel), intent(inout)  :: signyz   !< Stress yz
      my_real, dimension(nel), intent(inout)  :: signzx   !< Stress zx
      my_real, dimension(nel), intent(in)     :: dpla     !< Plastic strain increment
      my_real, dimension(nel), intent(in)     :: pla      !< Cumulated plastic strain
      my_real, dimension(nel), intent(inout)  :: loff     !< Integration point failure flag
      my_real, dimension(nel), intent(inout)  :: off      !< Element failure flag
      my_real, dimension(nel), intent(inout)  :: dfmax    !< Damage variable
      my_real, dimension(nel), intent(inout)  :: tdele    !< Deletion time
      integer, intent(in)                     :: niparam  !< Number of integer parameters
      integer, dimension(niparam), intent(in) :: iparam   !< Integer parameters
      my_real, dimension(nel), intent(inout)  :: dmgscl   !< Damage softening scaling factor
      integer, dimension(nel), intent(inout)  :: noff     !< Number of failed integration points
      integer, intent(in)                     :: npg      !< Number of integration points
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,indx(nel),nindx,failip
      integer :: indx2(nel),nindx2
      my_real :: epsd,s,dc,nu,young
      my_real :: p,svm(nel),triax(nel),r_inter
      my_real :: i1,i2,i3,q,r,phi,s11,s22,s33
      my_real :: rv,ye,sig1(nel)
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
!
      !=========================================================================
      !< Recover failure criterion parameters
      !=========================================================================
      !< Integer parameter
      failip = iparam(1)
      failip = min(failip,npg)
      !< Real parameters
      epsd   = uparam(1)
      s      = uparam(2)
      dc     = uparam(3)
      !< Material parameters
      nu     = matparam%nu
      young  = matparam%young
!
      !< Compute the loading characteristic values
      nindx  = 0 
      indx(1:nel) = 0
      do i = 1,nel
        !< Check the element status and the plastic strain trigger
        if ((pla(i) >= epsd).and.(loff(i) == one).and.(off(i)  == one)) then 
          !< Hydrostatic pressure
          p   = third*(signxx(i) + signyy(i) + signzz(i))
          !< Von Mises stress
          svm(i) = half*((signxx(i) - signyy(i))**2  +                           &
                         (signyy(i) - signzz(i))**2  +                           &
                         (signzz(i) - signxx(i))**2) +                           &
                   three*(signxy(i)**2 + signyz(i)**2 + signzx(i)**2)
          svm(i) = sqrt(svm(i))
          !< Stress triaxiality
          triax(i) = p/max(svm(i),em20)
          triax(i) = min(triax(i), one)
          triax(i) = max(triax(i),-one)
          !< Principal stresses
          i1 = signxx(i) + signyy(i) + signzz(i)
          i2 = signxx(i)*signyy(i) + signyy(i)*signzz(i) + signzz(i)*signxx(i) - &
               signxy(i)*signxy(i) - signzx(i)*signzx(i) - signyz(i)*signyz(i)
          i3 = signxx(i)*signyy(i)*signzz(i)-signxx(i)*signyz(i)*signyz(i) -     &
               signyy(i)*signzx(i)*signzx(i)-signzz(i)*signxy(i)*signxy(i) +     &
               two*signxy(i)*signzx(i)*signyz(i)
          q  = (three*i2 - i1*i1)/nine
          r  = (two*i1*i1*i1 - nine*i1*i2 + twenty7*i3)/fifty4
          r_inter = min(r/sqrt(max(em20,(-q**3))),one)
          phi = acos(max(r_inter,-one))
          s11 = two*sqrt(-q)*cos(phi/three)             + third*i1
          s22 = two*sqrt(-q)*cos((phi + two*pi)/three)  + third*i1
          s33 = two*sqrt(-q)*cos((phi + four*pi)/three) + third*i1
          sig1(i) = max(s11,s22,s33)
          if (sig1(i) > zero) then 
            nindx = nindx + 1
            indx(nindx) = i
          endif
        endif
      enddo
!
      !< Update Lemaitre damage variable
      nindx2 = 0
      indx2(1:nel) = 0
      do j = 1,nindx
        i  = indx(j)
        !< Strain energy release rate
        rv = two_third*(one + nu) + three*(one - two*nu)*(triax(i)**2)
        ye = (svm(i)**2)*rv/(two*young)
        !< Damage variable evolution
        dfmax(i) = dfmax(i) + max((ye/max(s,em20))*dpla(i),zero)
        dfmax(i) = min(dfmax(i),dc)
        !< Check for element failure
        if (dfmax(i) >= dc) then
          dfmax(i) = dc
          loff(i)  = zero
          noff(i)  = noff(i)  + 1
          if (noff(i) >= failip) then 
            nindx2 = nindx2 + 1
            indx2(nindx2) = i
            off(i) = zero
            tdele(i) = time
          endif 
        endif
      enddo
!
      !< Update the damage scaling factor
      do i = 1,nel
        dmgscl(i) = one - dfmax(i)
        dmgscl(i) = min(dmgscl(i),one)
        dmgscl(i) = max(dmgscl(i),zero)
      enddo
!
      !< Printing out element failure information
      if (nindx2 > 0) then
        do j = 1,nindx2
          i = indx2(j)
          write(iout ,1000) ngl(i),time
          write(istdo,1000) ngl(i),time
        enddo
      endif           
!
      !< Output message format
 1000 format (1X,'-- RUPTURE (LEMAITRE) OF SOLID ELEMENT:',I10,' AT TIME :',1PE12.4)
!
      end subroutine fail_lemaitre_s
      end module fail_lemaitre_s_mod
