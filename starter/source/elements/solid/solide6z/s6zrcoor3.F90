!copyright>        openradioss
!copyright>        copyright (c) 1986-2025 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    s6ccoor3         ../starter/source/elements/thickshell/solide6c/s6ccoor3.f
!||--- called by ------------------------------------------------------
!||    s6cinit3         ../starter/source/elements/thickshell/solide6c/s6cinit3.f
!||    s6zinit3         ../starter/source/elements/solid/solide6z/s6zinit3.f90
!||--- calls      -----------------------------------------------------
!||    checkvolume_6n   ../starter/source/elements/solid/solide/checksvolume.f
!||    s6cortho3        ../starter/source/elements/thickshell/solide6c/s6cortho3.f
!||--- uses       -----------------------------------------------------
!||    message_mod      ../starter/share/message_module/message_mod.f
!||====================================================================
      module s6zrcoor3_mod
      contains

      subroutine s6zrcoor3(x      ,ixs ,ngl  ,mxt  ,ngeo ,           &
           rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   ,    &
           r11  ,r21  ,r31  ,r12  ,r22  ,r32  ,r13  ,r23  ,r33  ,    &
           f1x  ,f1y  ,f1z  ,f2x  ,f2y  ,f2z  ,temp0, temp,nintemp,  &
           ix1, ix2, ix3, ix4, ix5, ix6,     &
           x1, x2, x3, x4, x5, x6,           &
           y1, y2, y3, y4, y5, y6,           &
           z1, z2, z3, z4, z5, z6,           &
           xd1, xd2, xd3, xd4, xd5, xd6,                                   &
           yd1, yd2, yd3, yd4, yd5, yd6,                                   &
           zd1, zd2, zd3, zd4, zd5, zd6, nel,numnod, jthe)

      use message_mod
      use element_mod , only : nixs
      use precision_mod, only : wp
      use constant_mod 
      use s6zortho3_mod

!c-----------------------------------------------
!c   i m p l i c i t   t y p e s
!c-----------------------------------------------
      implicit none

!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------

      integer, dimension(nixs,nel), intent(inout) :: ixs
      integer, dimension(nel),    intent(out)   :: mxt
      integer, dimension(nel),    intent(out)   :: ngl
      integer, dimension(nel),    intent(out)   :: ngeo
      integer ,intent(in) :: nintemp
      integer ,intent(in) :: nel
      integer ,intent(in) :: numnod
      integer ,intent(in) :: jthe

      real(kind=wp), dimension(3,numnod), intent(in)     :: x
      real(kind=wp), dimension(nel),     intent(inout)   :: rx 
      real(kind=wp), dimension(nel),     intent(inout)   :: ry 
      real(kind=wp), dimension(nel),     intent(inout)   :: rz
      real(kind=wp), dimension(nel),     intent(inout)   :: sx
      real(kind=wp), dimension(nel),     intent(inout)   :: sy
      real(kind=wp), dimension(nel),     intent(inout)   :: sz
      real(kind=wp), dimension(nel),     intent(inout)   :: tx
      real(kind=wp), dimension(nel),     intent(inout)   :: ty 
      real(kind=wp), dimension(nel),     intent(inout)   :: tz
      real(kind=wp), dimension(nel),     intent(inout)   :: r11 
      real(kind=wp), dimension(nel),     intent(inout)   :: r12 
      real(kind=wp), dimension(nel),     intent(inout)   :: r13
      real(kind=wp), dimension(nel),     intent(inout)   :: r21
      real(kind=wp), dimension(nel),     intent(inout)   :: r22 
      real(kind=wp), dimension(nel),     intent(inout)   :: r23
      real(kind=wp), dimension(nel),     intent(inout)   :: r31
      real(kind=wp), dimension(nel),     intent(inout)   :: r32
      real(kind=wp), dimension(nel),     intent(inout)   :: r33
      real(kind=wp), dimension(nel),     intent(inout)   :: f1x
      real(kind=wp), dimension(nel),     intent(inout)   :: f1y
      real(kind=wp), dimension(nel),     intent(inout)   :: f1z
      real(kind=wp), dimension(nel),     intent(inout)   :: f2x
      real(kind=wp), dimension(nel),     intent(inout)   :: f2y
      real(kind=wp), dimension(nel),     intent(inout)   :: f2z
      real(kind=wp), dimension(nel),     intent(in)    :: temp0
      real(kind=wp), dimension(nel),     intent(inout) :: temp

      integer, dimension(nel),       intent(inout) :: ix1
      integer, dimension(nel),       intent(inout) :: ix2
      integer, dimension(nel),       intent(inout) :: ix3
      integer, dimension(nel),       intent(inout) :: ix4
      integer, dimension(nel),       intent(inout) :: ix5
      integer, dimension(nel),       intent(inout) :: ix6

      real(kind=wp), dimension(nel),       intent(inout)   :: x1        
      real(kind=wp), dimension(nel),       intent(inout)   :: x2       
      real(kind=wp), dimension(nel),       intent(inout)   :: x3       
      real(kind=wp), dimension(nel),       intent(inout)   :: x4       
      real(kind=wp), dimension(nel),       intent(inout)   :: x5       
      real(kind=wp), dimension(nel),       intent(inout)   :: x6       
      real(kind=wp), dimension(nel),       intent(inout)   :: y1       
      real(kind=wp), dimension(nel),       intent(inout)   :: y2       
      real(kind=wp), dimension(nel),       intent(inout)   :: y3       
      real(kind=wp), dimension(nel),       intent(inout)   :: y4       
      real(kind=wp), dimension(nel),       intent(inout)   :: y5       
      real(kind=wp), dimension(nel),       intent(inout)   :: y6       
      real(kind=wp), dimension(nel),       intent(inout)   :: z1       
      real(kind=wp), dimension(nel),       intent(inout)   :: z2       
      real(kind=wp), dimension(nel),       intent(inout)   :: z3       
      real(kind=wp), dimension(nel),       intent(inout)   :: z4       
      real(kind=wp), dimension(nel),       intent(inout)   :: z5       
      real(kind=wp), dimension(nel),       intent(inout)   :: z6   
     
      real(kind=8), dimension(nel),       intent(inout)   :: xd1        
      real(kind=8), dimension(nel),       intent(inout)   :: xd2       
      real(kind=8), dimension(nel),       intent(inout)   :: xd3       
      real(kind=8), dimension(nel),       intent(inout)   :: xd4       
      real(kind=8), dimension(nel),       intent(inout)   :: xd5       
      real(kind=8), dimension(nel),       intent(inout)   :: xd6       
      real(kind=8), dimension(nel),       intent(inout)   :: yd1       
      real(kind=8), dimension(nel),       intent(inout)   :: yd2       
      real(kind=8), dimension(nel),       intent(inout)   :: yd3       
      real(kind=8), dimension(nel),       intent(inout)   :: yd4       
      real(kind=8), dimension(nel),       intent(inout)   :: yd5       
      real(kind=8), dimension(nel),       intent(inout)   :: yd6       
      real(kind=8), dimension(nel),       intent(inout)   :: zd1       
      real(kind=8), dimension(nel),       intent(inout)   :: zd2       
      real(kind=8), dimension(nel),       intent(inout)   :: zd3       
      real(kind=8), dimension(nel),       intent(inout)   :: zd4       
      real(kind=8), dimension(nel),       intent(inout)   :: zd5       
      real(kind=8), dimension(nel),       intent(inout)   :: zd6   
!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
      integer :: i
      real(kind=8) :: xl,yl,zl
      real(kind=wp) :: xl_t,yl_t,zl_t
!c-----------------------------------------------
!c   e x t e r n a l  f u n c t i o n s
!c-----------------------------------------------
      real(kind=wp), external :: checkvolume_6n
!c=================================================
!c=======================================================================
!c     connectivities and material number and pid
!c--------------------------------------------------
      do i=1,nel
        mxt(i)=ixs(1,i)
        ix1(i)=ixs(2,i)
        ix2(i)=ixs(3,i)
        ix3(i)=ixs(4,i)  
        ix4(i)=ixs(6,i)
        ix5(i)=ixs(7,i)
        ix6(i)=ixs(8,i)
        ngeo(i)=ixs(nixs-1,i)
        ngl(i)=ixs(nixs,i)
        if (checkvolume_6n(x ,ixs(1,i)) < zero) then
!c         renumber connectivity
          ix1(i)=ixs(6,i)
          ix2(i)=ixs(7,i)
          ix3(i)=ixs(8,i)
          ix4(i)=ixs(2,i)
          ix5(i)=ixs(3,i)
          ix6(i)=ixs(4,i)
          ixs(2,i)=ix1(i)
          ixs(3,i)=ix2(i)
          ixs(4,i)=ix3(i)
          ixs(6,i)=ix4(i)
          ixs(7,i)=ix5(i)
          ixs(8,i)=ix6(i)
        endif    
      enddo
!c
!c----------------------------
!c     coordonnees
!c----------------------------
      do 20 i=1,nel
      x1(i)=x(1,ix1(i))
      y1(i)=x(2,ix1(i))
      z1(i)=x(3,ix1(i))
      x2(i)=x(1,ix2(i))
      y2(i)=x(2,ix2(i))
      z2(i)=x(3,ix2(i))
      x3(i)=x(1,ix3(i))
      y3(i)=x(2,ix3(i))
      z3(i)=x(3,ix3(i))
      x4(i)=x(1,ix4(i))
      y4(i)=x(2,ix4(i))
      z4(i)=x(3,ix4(i))
      x5(i)=x(1,ix5(i))
      y5(i)=x(2,ix5(i))
      z5(i)=x(3,ix5(i))
      x6(i)=x(1,ix6(i))
      y6(i)=x(2,ix6(i))
      z6(i)=x(3,ix6(i))

      
   20 continue
!c

        do i=1,nel
          xd1(i) = x1(i)
          yd1(i) = y1(i)
          zd1(i) = z1(i)
          xd2(i) = x2(i)
          yd2(i) = y2(i)
          zd2(i) = z2(i)
          xd3(i) = x3(i)
          yd3(i) = y3(i)
          zd3(i) = z3(i)
          xd4(i) = x4(i)
          yd4(i) = y4(i)
          zd4(i) = z4(i)
          xd5(i) = x5(i)
          yd5(i) = y5(i)
          zd5(i) = z5(i)
          xd6(i) = x6(i)
          yd6(i) = y6(i)
          zd6(i) = z6(i)

        enddo

   do i=1,nel
        f1x(i) = x2(i) - x1(i)
        f1y(i) = y2(i) - y1(i)
        f1z(i) = z2(i) - z1(i)
        f2x(i) = x3(i) - x1(i)
        f2y(i) = y3(i) - y1(i)
        f2z(i) = z3(i) - z1(i)
      enddo
!c
      do i=1,nel
        xl=one_over_6*(xd1(i)+xd2(i)+xd3(i)+xd4(i)+xd5(i)+xd6(i))
        yl=one_over_6*(yd1(i)+yd2(i)+yd3(i)+yd4(i)+yd5(i)+yd6(i))
        zl=one_over_6*(zd1(i)+zd2(i)+zd3(i)+zd4(i)+zd5(i)+zd6(i))
        xd1(i)=xd1(i)-xl
        yd1(i)=yd1(i)-yl
        zd1(i)=zd1(i)-zl
        xd2(i)=xd2(i)-xl
        yd2(i)=yd2(i)-yl
        zd2(i)=zd2(i)-zl
        xd3(i)=xd3(i)-xl
        yd3(i)=yd3(i)-yl
        zd3(i)=zd3(i)-zl
        xd4(i)=xd4(i)-xl
        yd4(i)=yd4(i)-yl
        zd4(i)=zd4(i)-zl
        xd5(i)=xd5(i)-xl
        yd5(i)=yd5(i)-yl
        zd5(i)=zd5(i)-zl
        xd6(i)=xd6(i)-xl
        yd6(i)=yd6(i)-yl
        zd6(i)=zd6(i)-zl
      enddo
!c-----------
!c     convected frame .
!c-----------
      call s6zortho3(                                   &
       xd1, xd2, xd3, xd4, xd5, xd6,                          &
       yd1, yd2, yd3, yd4, yd5, yd6,                          &
       zd1, zd2, zd3, zd4, zd5, zd6,                          &
       rx  ,ry  ,rz  ,sx  ,sy  ,sz  ,tx  ,ty  ,tz  ,    &
       r11 ,r21 ,r31 ,r12 ,r22 ,r32 ,r13, r23,  r33, nel)    
!c
       do i=1,nel


        xl=r11(i)*xd1(i)+r21(i)*yd1(i)+r31(i)*zd1(i)
        yl=r12(i)*xd1(i)+r22(i)*yd1(i)+r32(i)*zd1(i)
        zl=r13(i)*xd1(i)+r23(i)*yd1(i)+r33(i)*zd1(i)
        xd1(i)=xl
        yd1(i)=yl
        zd1(i)=zl
        xl=r11(i)*xd2(i)+r21(i)*yd2(i)+r31(i)*zd2(i)
        yl=r12(i)*xd2(i)+r22(i)*yd2(i)+r32(i)*zd2(i)
        zl=r13(i)*xd2(i)+r23(i)*yd2(i)+r33(i)*zd2(i)
        xd2(i)=xl
        yd2(i)=yl
        zd2(i)=zl
        xl=r11(i)*xd3(i)+r21(i)*yd3(i)+r31(i)*zd3(i)
        yl=r12(i)*xd3(i)+r22(i)*yd3(i)+r32(i)*zd3(i)
        zl=r13(i)*xd3(i)+r23(i)*yd3(i)+r33(i)*zd3(i)
        xd3(i)=xl
        yd3(i)=yl
        zd3(i)=zl
        xl=r11(i)*xd4(i)+r21(i)*yd4(i)+r31(i)*zd4(i)
        yl=r12(i)*xd4(i)+r22(i)*yd4(i)+r32(i)*zd4(i)
        xd4(i)=xl
        yd4(i)=yl
        zd4(i)=-zd1(i)
        xl=r11(i)*xd5(i)+r21(i)*yd5(i)+r31(i)*zd5(i)
        yl=r12(i)*xd5(i)+r22(i)*yd5(i)+r32(i)*zd5(i)
        xd5(i)=xl
        yd5(i)=yl
        zd5(i)=-zd2(i)
        xl=r11(i)*xd6(i)+r21(i)*yd6(i)+r31(i)*zd6(i)
        yl=r12(i)*xd6(i)+r22(i)*yd6(i)+r32(i)*zd6(i)
        xd6(i)=xl
        yd6(i)=yl
        zd6(i)=-zd3(i)


       enddo


       do i=1,nel
          x1(i) = xd1(i)
          y1(i) = yd1(i)
          z1(i) = zd1(i)
          x2(i) = xd2(i)
          y2(i) = yd2(i)
          z2(i) = zd2(i)
          x3(i) = xd3(i)
          y3(i) = yd3(i)
          z3(i) = zd3(i)
          x4(i) = xd4(i)
          y4(i) = yd4(i)
          z4(i) = zd4(i)
          x5(i) = xd5(i)
          y5(i) = yd5(i)
          z5(i) = zd5(i)
          x6(i) = xd6(i)
          y6(i) = yd6(i)
          z6(i) = zd6(i)


        enddo
!c
      if(jthe < 0 ) then                                      
        if(nintemp > 0 ) then                                 
         do i=1,nel                                      
           if(temp(ix1(i))== zero) temp(ix1(i)) = temp0(i)    
           if(temp(ix2(i))== zero) temp(ix2(i)) = temp0(i)    
           if(temp(ix3(i))== zero) temp(ix3(i)) = temp0(i)    
           if(temp(ix4(i))== zero) temp(ix4(i)) = temp0(i)    
           if(temp(ix5(i))== zero) temp(ix5(i)) = temp0(i)    
           if(temp(ix6(i))== zero) temp(ix6(i)) = temp0(i)    
         enddo                                                
        else                                                  
         do i=1,nel                                       
           temp(ix1(i))=temp0(i)                              
           temp(ix2(i))=temp0(i)                              
           temp(ix3(i))=temp0(i)                              
           temp(ix4(i))=temp0(i)                              
           temp(ix5(i))=temp0(i)                              
           temp(ix6(i))=temp0(i)                              
         enddo                                                
        endif                                                 
      endif  
                                             
!c-----------
      return
      
      end subroutine s6zrcoor3
      end module s6zrcoor3_mod