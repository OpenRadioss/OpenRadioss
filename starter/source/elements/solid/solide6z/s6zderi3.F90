!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    s6zderi3_mod   ../starter/source/elements/solid/solide6z/s6zderi3.F90
!||--- called by ------------------------------------------------------
!||    s6zinit3       ../starter/source/elements/solid/solide6z/s6zinit3.F90
!||====================================================================
      module s6zderi3_mod
      contains

!||====================================================================
!||    s6zderi3        ../starter/source/elements/solid/solide6z/s6zderi3.F90
!||--- called by ------------------------------------------------------
!||    s6zinit3        ../starter/source/elements/solid/solide6z/s6zinit3.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg          ../starter/source/output/message/message.F
!||    slen            ../starter/source/elements/solid/solide/slen.F
!||--- uses       -----------------------------------------------------
!||    message_mod     ../starter/share/message_module/message_mod.F
!||====================================================================
      subroutine s6zderi3(nel ,vol ,vzl ,ngl ,deltax ,det ,   &
          x1, x2, x3, x4, x5, x6,        &
          y1, y2, y3, y4, y5, y6,        &
          z1, z2, z3, z4, z5, z6)
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      use message_mod
      use precision_mod, only : wp
      use constant_mod , only :third, one_over_8, zero, fourth, em4, ep03, one, four


      implicit none
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
     integer , intent(in) ::  nel

     integer, dimension(nel),             intent(inout)   :: ngl
     real(kind=wp), dimension(nel),       intent(inout)   :: vol       
     real(kind=wp), dimension(nel),       intent(inout)   :: vzl      
     real(kind=wp), dimension(nel),       intent(inout)   :: deltax       
     real(kind=wp), dimension(nel),       intent(inout)   :: det
     real(kind=8), dimension(nel),       intent(in)   :: x1       
     real(kind=8), dimension(nel),       intent(in)   :: x2       
     real(kind=8), dimension(nel),       intent(in)   :: x3       
     real(kind=8), dimension(nel),       intent(in)   :: x4       
     real(kind=8), dimension(nel),       intent(in)   :: x5       
     real(kind=8), dimension(nel),       intent(in)   :: x6       
     real(kind=8), dimension(nel),       intent(in)   :: y1       
     real(kind=8), dimension(nel),       intent(in)   :: y2       
     real(kind=8), dimension(nel),       intent(in)   :: y3       
     real(kind=8), dimension(nel),       intent(in)   :: y4       
     real(kind=8), dimension(nel),       intent(in)   :: y5       
     real(kind=8), dimension(nel),       intent(in)   :: y6       
     real(kind=8), dimension(nel),       intent(in)   :: z1       
     real(kind=8), dimension(nel),       intent(in)   :: z2       
     real(kind=8), dimension(nel),       intent(in)   :: z3       
     real(kind=8), dimension(nel),       intent(in)   :: z4       
     real(kind=8), dimension(nel),       intent(in)   :: z5       
     real(kind=8), dimension(nel),       intent(in)   :: z6       

!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
      integer ::i, nfac
      
      real(kind=8), dimension(nel) :: jac1, jac2, jac3
      real(kind=8), dimension(nel) :: jac4, jac5, jac6
      real(kind=8), dimension(nel) :: jac7, jac8, jac9
      real(kind=8), dimension(nel) :: jac_59_68, jac_67_49, jac_48_57


     real(kind=wp), dimension(nel) :: xioff
     real(kind=wp), dimension(nel) :: aream
     real(kind=wp), dimension(nel) :: atest
     real(kind=wp), dimension(6,nel) :: area

     real(kind=8), dimension(nel) :: x21, x31, x41, x54, x64
     real(kind=8), dimension(nel) :: y21, y31, y41, y54, y64
     real(kind=8), dimension(nel) :: z21, z31, z41, z54, z64
 
!c=======================================================================
      do i=1,nel
        x21(i)=x2(i)-x1(i)
        x31(i)=x3(i)-x1(i)
        x41(i)=x4(i)-x1(i)
        x54(i)=x5(i)-x4(i)
        x64(i)=x6(i)-x4(i)
!c
        y21(i)=y2(i)-y1(i)
        y31(i)=y3(i)-y1(i)
        y41(i)=y4(i)-y1(i)
        y54(i)=y5(i)-y4(i)
        y64(i)=y6(i)-y4(i)
!c
        z21(i)=z2(i)-z1(i)
        z31(i)=z3(i)-z1(i)
        z41(i)=z4(i)-z1(i)
        z54(i)=z5(i)-z4(i)
        z64(i)=z6(i)-z4(i)
        
      enddo

      do i=1,nel
!c-------ri.xi---->ksi--------
        jac1(i)=x21(i)+x54(i)
        jac2(i)=y21(i)+y54(i)
        jac3(i)=z21(i)+z54(i)
      enddo

      do i=1,nel
!c-------si.xi--->eta--------
        jac4(i)=x31(i)+x64(i)
        jac5(i)=y31(i)+y64(i)
        jac6(i)=z31(i)+z64(i)
!c-------ti.xi----zeta-------
        jac7(i)=third*(x41(i)+x5(i)-x2(i)+x6(i)-x3(i))
        jac8(i)=third*(y41(i)+y5(i)-y2(i)+y6(i)-y3(i))
        jac9(i)=third*(z41(i)+z5(i)-z2(i)+z6(i)-z3(i))
      enddo

      do i=1,nel
        jac_59_68(i)=jac5(i)*jac9(i)-jac6(i)*jac8(i)
        jac_67_49(i)=jac6(i)*jac7(i)-jac4(i)*jac9(i)
        jac_48_57(i)=jac4(i)*jac8(i)-jac5(i)*jac7(i)
      enddo
!c
      do i=1,nel
        det(i)=one_over_8*(jac1(i)*jac_59_68(i)+jac2(i)*jac_67_49(i)+jac3(i)*jac_48_57(i))
        vol(i)=det(i)
       
      enddo
!
      do i=1,nel
        if(det(i)>zero) cycle
        call ancmsg(msgid=245,&
                   msgtype=msgerror,&
                   anmode=aninfo,&
                   i1=ngl(i))
      enddo

      do i=1,nel
         vzl(i) = fourth*(jac9(i)*(x54(i)*y64(i)-x21(i)*y31(i)-x64(i)*y54(i)+x31(i)*y21(i)) &
                        -jac8(i)*(x54(i)*z64(i)+x31(i)*z21(i)-x21(i)*z31(i)-x64(i)*z54(i)) &
                        +jac7(i)*(y54(i)*z64(i)+y31(i)*z21(i)-y21(i)*z31(i)-y64(i)*z54(i)))
!c
      enddo
      do i=1,nel
        xioff(i)=one
        aream(i)=zero
      enddo
!c
      call slen(x1,x2,x5,x4,y1,y2,y4,y5,z1,z2,z5,z4,1, area, aream)
      call slen(x2,x5,x6,x3,y2,y5,y6,y3,z2,z5,z6,z3,2, area, aream)
      call slen(x1,x4,x6,x3,y1,y4,y6,y3,z1,z4,z6,z3,3, area, aream)
      call slen(x1,x2,x3,x3,y1,y2,y3,y3,z1,z2,z3,z3,4, area, aream)
      call slen(x4,x5,x6,x6,y4,y5,y6,y6,z4,z5,z6,z6,5, area, aream)
!c
      do i=1,nel
        atest(i)=em4*aream(i)
        nfac=0
        if(area(1,i)<atest(i)) nfac=nfac+1
        if(area(2,i)<atest(i)) nfac=nfac+1
        if(area(3,i)<atest(i)) nfac=nfac+1
        if(area(4,i)<atest(i)) nfac=nfac+1
        if(area(5,i)<atest(i)) nfac=nfac+1
        if(nfac>=2) xioff(i)=ep03
      enddo
      do i=1,nel
         deltax(i)=four*det(i)*xioff(i)/sqrt(aream(i))
      enddo


      end subroutine s6zderi3
      end module s6zderi3_mod
