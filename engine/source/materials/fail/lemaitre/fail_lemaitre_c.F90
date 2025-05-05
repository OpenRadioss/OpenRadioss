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
      module fail_lemaitre_c_mod
      contains
      subroutine fail_lemaitre_c(                                              &
        nel      ,nuparam  ,uparam   ,matparam ,                               &
        signxx   ,signyy   ,signxy   ,dpla     ,pla      ,foff     ,           &
        off      ,dfmax    ,tdele    ,dmg_flag ,dmgscl   ,ipg      ,           &
        ply_id   ,ilay     ,ipt      ,ngl      ,time     ,igtyp    )
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
      type(matparam_struct_) , intent(in)     :: matparam !< Material parameters data structure
      my_real, dimension(nel), intent(inout)  :: signxx   !< Stress xx
      my_real, dimension(nel), intent(inout)  :: signyy   !< Stress yy
      my_real, dimension(nel), intent(inout)  :: signxy   !< Stress xy
      my_real, dimension(nel), intent(in)     :: dpla     !< Plastic strain increment
      my_real, dimension(nel), intent(in)     :: pla      !< Cumulated plastic strain
      integer, dimension(nel), intent(inout)  :: foff     !< Integration point failure flag
      my_real, dimension(nel), intent(inout)  :: off      !< Element failure flag
      my_real, dimension(nel), intent(inout)  :: dfmax    !< Damage variable
      my_real, dimension(nel), intent(inout)  :: tdele    !< Deletion time
      integer, intent(inout)                  :: dmg_flag !< Damage softening flag
      my_real, dimension(nel), intent(inout)  :: dmgscl   !< Damage softening scaling factor
      integer, intent(in)                     :: ipg      !< Gauss point number
      integer, intent(in)                     :: ply_id   !< Ply ID
      integer, intent(in)                     :: ilay     !< Layer number
      integer, intent(in)                     :: ipt      !< Integration point number
      integer, dimension(nel), intent(in)     :: ngl      !< Global element numbers
      my_real, intent(in)                     :: time     !< Current time
      integer, intent(in)                     :: igtyp    !< Property type
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,indx(nel),nindx
      integer :: indx2(nel),nindx2
      my_real :: epsd,s,dc,nu,young
      my_real :: p,svm(nel),triax(nel)
      my_real :: rv,ye,center,radius,sig1(nel)
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
!
      !=========================================================================
      !< Recover failure criterion parameters
      !=========================================================================
      !< Real parameters
      epsd   = uparam(1)
      s      = uparam(2)
      dc     = uparam(3)
      !< Material parameters
      nu     = matparam%nu
      young  = matparam%young
      !< Damage softening flag
      dmg_flag = 1
!
      !< Compute the loading characteristic values
      nindx  = 0 
      indx(1:nel) = 0
      do i = 1,nel
        !< Check the element status and the plastic strain trigger
        if ((pla(i) >= epsd).and.(foff(i) == 1).and.(off(i) == one)) then 
          !< Hydrostatic pressure
          p = third*(signxx(i) + signyy(i))
          !< Von Mises stress
          svm(i) = sqrt(                                                       &
             signxx(i)*signxx(i) + signyy(i)*signyy(i) - signxx(i)*signyy(i)   &
                                                 + three*signxy(i)*signxy(i))
          !< Stress triaxiality
          triax(i) = p/max(svm(i),em20)
          triax(i) = min(triax(i), two_third)
          triax(i) = max(triax(i),-two_third)
          !< Principal stresses
          center = (signxx(i) + signyy(i))/two
          radius = sqrt((half*(signxx(i) - signyy(i)))**2 + signxy(i)**2)
          sig1(i) = center + radius 
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
        !< Check for intg. point failure
        if (dfmax(i) >= dc) then
          dfmax(i) = dc
          foff(i)  = 0
          tdele(i) = time 
          nindx2 = nindx2 + 1
          indx2(nindx2) = i
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
      !< Printing out the integration point failure information
      if (nindx2 > 0) then
        do j = 1,nindx2
          i = indx2(j)
          if (igtyp == 17 .or. igtyp == 51 .or. igtyp == 52) then 
            write(iout ,1200) ngl(i),ipg,ply_id,ipt
            write(istdo,1200) ngl(i),ipg,ply_id,ipt
          elseif (igtyp == 1 .or. igtyp == 9) then 
            write(iout ,1000) ngl(i),ipg,ipt
            write(istdo,1000) ngl(i),ipg,ipt
          else 
            write(iout ,1100) ngl(i),ipg,ilay,ipt
            write(istdo,1100) ngl(i),ipg,ilay,ipt
          endif
        enddo
      endif           
!
      !< Output message format
1000 format(1X,'FAILURE (LEMAITRE) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',     &
            I2,1X,',INTEGRATION PT',I3)
1100 format(1X,'FAILURE (LEMAITRE) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',     &
            I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
1200 format(1X,'FAILURE (LEMAITRE) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',     &
            I2,1X,',PLY ID',I10,1X,',INTEGRATION PT',I3)
!
      end subroutine fail_lemaitre_c
      end module fail_lemaitre_c_mod
