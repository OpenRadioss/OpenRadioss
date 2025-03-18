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
      !||====================================================================
      !||    fail_spalling_s_mod   ../engine/source/materials/fail/spalling/fail_spalling_s.F90
      !||--- called by ------------------------------------------------------
      !||    mmain                 ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8                ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8                ../engine/source/materials/mat_share/mulaw8.F90
      !||    usermat_solid         ../engine/source/materials/mat_share/usermat_solid.F
      !||====================================================================
      module fail_spalling_s_mod
      contains
      !||====================================================================
      !||    fail_spalling_s   ../engine/source/materials/fail/spalling/fail_spalling_s.F90
      !||--- called by ------------------------------------------------------
      !||    mmain             ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8            ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw             ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8            ../engine/source/materials/mat_share/mulaw8.F90
      !||    usermat_solid     ../engine/source/materials/mat_share/usermat_solid.F
      !||--- calls      -----------------------------------------------------
      !||    valpvecdp_v       ../engine/source/materials/mat/mat033/sigeps33.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod      ../common_source/modules/constant_mod.F
      !||====================================================================
      subroutine fail_spalling_s(                                              &
        nel      ,nuparam  ,time     ,uparam   ,ngl      ,                     &
        signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,           &
        dpla     ,epsp     ,tstar    ,off      ,lf_dammx ,dfmax    ,           &
        tdele    ,offg     ,niparam  ,iparam   ,mvsiz    )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
#include "my_real.inc"
#include "units_c.inc"
!-----------------------------------------------
!   I N P U T   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in) :: nel
      integer, intent(in) :: nuparam
      my_real, intent(in) :: time
      my_real, dimension(nuparam), intent(in) :: uparam
      integer, dimension(nel), intent(in) :: ngl
      my_real, dimension(nel), intent(inout) :: signxx
      my_real, dimension(nel), intent(inout) :: signyy
      my_real, dimension(nel), intent(inout) :: signzz
      my_real, dimension(nel), intent(inout) :: signxy
      my_real, dimension(nel), intent(inout) :: signyz
      my_real, dimension(nel), intent(inout) :: signzx
      my_real, dimension(nel), intent(in) :: dpla
      my_real, dimension(nel), intent(in) :: epsp
      my_real, dimension(nel), intent(in) :: tstar
      my_real, dimension(nel), intent(inout) :: off
      integer, intent(in) :: lf_dammx
      my_real, dimension(nel,lf_dammx), intent(inout) :: dfmax
      my_real, dimension(nel), intent(inout) :: tdele
      my_real, dimension(nel), intent(in) :: offg
      integer, intent(in) :: niparam
      integer, dimension(niparam), intent(in) :: iparam
      integer, intent(in) :: mvsiz
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,idel,idev,iflag,indx(nel),iadbuf,nindx,nindex,index(nel), &
                 ifail,jj,ispall
      my_real :: d1,d2,d3,d4,d5,epsp0,p,pmin,epsf,svm,scale,sxx,syy,szz,       &
                 sig(mvsiz,6),valp(mvsiz,3),vec(mvsiz,9),sigp_max(nel)
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
!
      !=========================================================================
      !< Recover failure criterion parameters
      !=========================================================================
      !< Integer parameter
      iflag = iparam(1)
      !< Real parameters
      d1    = uparam(1)
      d2    = uparam(2)
      d3    = uparam(3)
      d4    = uparam(4)
      d5    = uparam(5)
      epsp0 = uparam(6)
      pmin  = uparam(7)
!
      !< Element deletion flag
      idel = 0
      !< Deviatoric stress vanishing flag
      idev = 0
!      
      !< Initialization flag
      ispall = 1
      if (iflag == 2) then
        idel   = 1
      elseif (iflag == 3) then
        idev   = 1  
      elseif (iflag == 4) then
        idel   = 1
        ispall = 2
      elseif (iflag == 5) then
        ispall = 3
      elseif (iflag == 6) then
        ispall = 4
        sig(1:nel,1) = signxx(1:nel)
        sig(1:nel,2) = signyy(1:nel)
        sig(1:nel,3) = signzz(1:nel)
        sig(1:nel,4) = signxy(1:nel)
        sig(1:nel,5) = signyz(1:nel)
        sig(1:nel,6) = signzx(1:nel)
        call valpvecdp_v(sig,valp,vec,nel)
        do i = 1,nel
          sigp_max(i) = maxval(valp(i,1:3))
        enddo 
      endif
!
      !< Update the element status 
      do i=1,nel
        if (off(i) < em01) off(i) = zero
        if (off(i) <  one) off(i) = off(i)*four_over_5
      end do
!
      !< In case of element deletion
      if (idel == 1) then
        nindx = 0  
        do i = 1,nel
          !< Update Johnson-Cook failure criterion
          if ((iflag == 2 .or. iflag == 4) .and. off(i) == one                 &
               .and. dpla(i) /= zero .and. offg(i) > zero) then        
            p = third*(signxx(i) + signyy(i) + signzz(i))
            sxx = signxx(i) - p
            syy = signyy(i) - p
            szz = signzz(i) - p
            svm = half*(sxx**2 + syy**2 + szz**2)                              &
                + signxy(i)**2 + signzx(i)**2 + signyz(i)**2
            svm = sqrt(three*svm)
            epsf = d3*p/max(em20,svm)
            epsf = (d1 + d2*exp(epsf))*(one + d4*log(max(one,epsp(i)/epsp0)))  &
                                      *(one + d5*tstar(i))
            if (epsf > zero) dfmax(i,2) = dfmax(i,2) + dpla(i)/epsf      
            !< Trigger element deletion 
            if (dfmax(i,2) >= one .and. off(i) == one) then
              dfmax(i,2) = one
              off(i) = four_over_5
              nindx  = nindx+1
              indx(nindx) = i
              tdele(i) = time                  
            endif
          endif 
        enddo
        !< Printing out element deletion message
        if (nindx > 0)then
          do j=1,nindx
            write(iout ,1000) ngl(indx(j)),time
            write(istdo,1000) ngl(indx(j)),time
          end do
        end if         
      endif
!
      !< In case of deviatoric stress vanishing
      if (idev==1) then
        nindx  = 0 
        nindex = 0 
        do i = 1,nel
          !< Update Johnson-Cook failure criterion
          if (iflag == 3 .and. off(i) == one .and. dpla(i) /= zero .and.       &
              offg(i) > zero) then
            if (dfmax(i,2) < one)then
              p   = third*(signxx(i) + signyy(i) + signzz(i))
              sxx = signxx(i) - p
              syy = signyy(i) - p
              szz = signzz(i) - p
              svm = half*(sxx**2+ syy**2 + szz**2)                             &
                    + signxy(i)**2 + signzx(i)**2 + signyz(i)**2
              svm = sqrt(three*svm)
              epsf = d3*p/max(em20,svm)
              epsf = (d1 + d2*exp(epsf))*(one + d4*log(max(one,epsp(i)/epsp0)))&
                                        *(one + d5*tstar(i))
              if (epsf > zero) dfmax(i,2) = dfmax(i,2) + dpla(i)/epsf
              if (dfmax(i,2) >= one .and. off(i) == one) then
                dfmax(i,2) = one
                nindx = nindx+1
                indx(nindx) = i
                signxx(i) =   p
                signyy(i) =   p
                signzz(i) =   p
                signxy(i) = zero
                signyz(i) = zero
                signzx(i) = zero                 
              endif
            else 
              p = third*(signxx(i) + signyy(i) + signzz(i))
              signxx(i) =   p
              signyy(i) =   p
              signzz(i) =   p
              signxy(i) = zero
              signyz(i) = zero
              signzx(i) = zero
            endif
          endif  
        enddo
        !< Printing out deviatoric stress vanishing message
        if (nindx > 0) then
          do j = 1,nindx
            i = indx(j)
            write(iout ,2000) ngl(i),time
            write(iout ,2100)
            write(istdo,2000) ngl(i),time
            write(istdo,2100)
          enddo
        endif           
      endif
!
      !< Update spalling criterion
      nindex = 0
      nindx  = 0
      do i = 1,nel
        p = -third*(signxx(i) + signyy(i) + signzz(i))
        if (ispall == 1 .and. offg(i) > zero) then
          if (dfmax(i,3) < one) then 
            dfmax(i,3) = max(dfmax(i,3),min(p,zero)/pmin)
            dfmax(i,3) = min(dfmax(i,3),one)
            if (dfmax(i,3) >= one)then
              dfmax(i,3) = one
              signxx(i) = zero
              signyy(i) = zero
              signzz(i) = zero
              signxy(i) = zero
              signzx(i) = zero
              signyz(i) = zero
              nindx = nindx+1
              indx(nindx) = i 
            endif
          else
            signxx(i) = -max(p , zero)
            signyy(i) = -max(p , zero)
            signzz(i) = -max(p , zero)
            signxy(i) = zero
            signzx(i) = zero
            signyz(i) = zero
          endif
        elseif (ispall == 2  .and. offg(i) > zero) then
          dfmax(i,3) = max(dfmax(i,3),min(p,zero)/pmin)
          dfmax(i,3) = min(dfmax(i,3),one)
          if (p <= pmin .and. off(i) == one) then 
            off(i) = four_over_5
            nindex = nindex+1
            index(nindex) = i
            tdele(i) = time  
          endif
        elseif (ispall == 3 .and. offg(i) > zero) then
          dfmax(i,3) = max(dfmax(i,3),min(p,zero)/pmin)
          dfmax(i,3) = min(dfmax(i,3),one)
          if (p <= pmin .and. off(i) == one) then 
            dfmax(i,3) = one 
            signxx(i)  = signxx(i) + p - pmin
            signyy(i)  = signyy(i) + p - pmin
            signzz(i)  = signzz(i) + p - pmin
          endif
        elseif (ispall == 4 .and. offg(i) > zero) then
          if (dfmax(i,3) < one) then 
            dfmax(i,3) = max(dfmax(i,3),max(sigp_max(i),zero)/(abs(pmin)))
            dfmax(i,3) = min(dfmax(i,3),one)
            if (dfmax(i,3) >= one)then
              dfmax(i,3) = one
              signxx(i) = zero
              signyy(i) = zero
              signzz(i) = zero
              signxy(i) = zero
              signzx(i) = zero
              signyz(i) = zero
              nindx = nindx+1
              indx(nindx) = i 
            endif
          else
            signxx(i) = -max(p , zero)
            signyy(i) = -max(p , zero)
            signzz(i) = -max(p , zero)
            signxy(i) = zero
            signzx(i) = zero
            signyz(i) = zero
          endif
        endif
      enddo
!
      !< Printing out spalling message
      if (nindx > 0) then
        do j = 1,nindx
          write(iout ,3000) ngl(indx(j)),time
          write(istdo,3000) ngl(indx(j)),time
        enddo
      endif
!       
      if (nindex > 0) then
        do j=1,nindex
          write(iout ,3100) ngl(index(j)),time
          write(istdo,3100) ngl(index(j)),time
        enddo
      endif  
!
      !< Storing the maximum damage for output
      do i=1,nel
        dfmax(i,1) = max(dfmax(i,2),dfmax(i,3))
      enddo                   
!
      !< Output message format
 1000 format (1X,'-- RUPTURE (JOHNSON-COOK) OF SOLID ELEMENT:',I10,            &
                 ' AT TIME :',1PE12.4)   
 2000 format (1X,'FOR SOLID ELEMENT NUMBER:',I10,                              &
                 ' JOHNSON-COOK CRITERION REACHED AT TIME:',1PE12.4)  
 2100 format (1X,'-- DEVIATORIC STRESS TENSOR WILL BE VANISHED')  
 3000 format (1X,'-- SPALLING OF SOLID ELEMENT:',I10,' AT TIME :',1PE12.4)
 3100 format (1X,'-- RUPTURE (SPALLING) OF SOLID ELEMENT:',I10,                &
                 ' AT TIME :',1PE12.4)   

      end subroutine fail_spalling_s
      end module fail_spalling_s_mod
