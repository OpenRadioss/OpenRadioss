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
!||    s6zrcoor3_mod   ../starter/source/elements/solid/solide6z/s6zrcoor3.f90
!||--- called by ------------------------------------------------------
!||    s6zinit3        ../starter/source/elements/solid/solide6z/s6zinit3.f90
!||====================================================================
      module s6zrcoor3_mod
      contains

!||====================================================================
!||    s6zrcoor3        ../starter/source/elements/solid/solide6z/s6zrcoor3.f90
!||--- called by ------------------------------------------------------
!||    s6zinit3         ../starter/source/elements/solid/solide6z/s6zinit3.f90
!||--- calls      -----------------------------------------------------
!||    checkvolume_6n   ../starter/source/elements/solid/solide/checksvolume.f
!||    s6zortho3        ../starter/source/elements/solid/solide6z/s6zortho3.f90
!||--- uses       -----------------------------------------------------
!||    message_mod      ../starter/share/message_module/message_mod.f
!||    s6zortho3_mod    ../starter/source/elements/solid/solide6z/s6zortho3.f90
!||====================================================================
      subroutine s6zrcoor3(x      ,ixs ,ngl  ,mxt  ,ngeo ,           &
           rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   ,    &
           e1x  ,e1y  ,e1z,  e2x  ,e2y  ,e2z   ,e3x ,e3y  ,e3z  ,    &
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
      real(kind=wp), dimension(nel),     intent(inout)   :: f1x
      real(kind=wp), dimension(nel),     intent(inout)   :: f1y
      real(kind=wp), dimension(nel),     intent(inout)   :: f1z
      real(kind=wp), dimension(nel),     intent(inout)   :: f2x
      real(kind=wp), dimension(nel),     intent(inout)   :: f2y
      real(kind=wp), dimension(nel),     intent(inout)   :: f2z

      real(kind=wp), dimension(nel),     intent(inout)   :: e1x
      real(kind=wp), dimension(nel),     intent(inout)   :: e1y
      real(kind=wp), dimension(nel),     intent(inout)   :: e1z
      real(kind=wp), dimension(nel),     intent(inout)   :: e2x
      real(kind=wp), dimension(nel),     intent(inout)   :: e2y
      real(kind=wp), dimension(nel),     intent(inout)   :: e2z
      real(kind=wp), dimension(nel),     intent(inout)   :: e3x
      real(kind=wp), dimension(nel),     intent(inout)   :: e3y
      real(kind=wp), dimension(nel),     intent(inout)   :: e3z

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
      real(kind=wp), dimension(nel) :: xdl1, xdl2, xdl3, xdl4, xdl5, xdl6, xdl7, xdl8
      real(kind=wp), dimension(nel) :: ydl1, ydl2, ydl3, ydl4, ydl5, ydl6, ydl7, ydl8
      real(kind=wp), dimension(nel) :: zdl1, zdl2, zdl3, zdl4, zdl5, zdl6, zdl7, zdl8
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



          xdl1(i)=xd1(i)
          ydl1(i)=yd1(i)
          zdl1(i)=zd1(i)
!
          xdl2(i)=xd2(i)
          ydl2(i)=yd2(i)
          zdl2(i)=zd2(i)
!
          xdl3(i)=xd3(i)
          ydl3(i)=yd3(i)
          zdl3(i)=zd3(i)
!
          xdl4(i)=xd3(i)
          ydl4(i)=yd3(i)
          zdl4(i)=zd3(i)
!
            xdl5(i)=xd4(i)
            ydl5(i)=yd4(i)
            zdl5(i)=zd4(i)
!
            xdl6(i)=xd5(i)
            ydl6(i)=yd5(i)
            zdl6(i)=zd5(i)
!
            xdl7(i)=xd6(i)
            ydl7(i)=yd6(i)
            zdl7(i)=zd6(i)
!
            xdl8(i)=xd6(i)  
            ydl8(i)=yd6(i)  
            zdl8(i)=zd6(i) 

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

!c-----------
!c     convected frame .
!c-----------

!c     isoparametric reference frame according to the manual for bricks
      call srepiso3(                                            &
            xdl1, xdl2, xdl3, xdl4, xdl5, xdl6, xdl7, xdl8,     &
            ydl1, ydl2, ydl3, ydl4, ydl5, ydl6, ydl7, ydl8,     &
            zdl1, zdl2, zdl3, zdl4, zdl5, zdl6, zdl7, zdl8,     &
            rx, ry, rz, sx, sy, sz, tx, ty,                     &
            tz ,f1x  ,f1y  ,f1z  ,f2x  ,f2y  ,f2z   )
      !c

      call sortho3(&
           rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   ,   &
           e2x  ,e2y  ,e2z  ,e3x  ,e3y  ,e3z  ,e1x  ,e1y  ,e1z  )
      
       do i=1,nel

          xl=e1x(i)*xdl1(i)+e1y(i)*ydl1(i)+e1z(i)*zdl1(i)
          yl=e2x(i)*xdl1(i)+e2y(i)*ydl1(i)+e2z(i)*zdl1(i)
          zl=e3x(i)*xdl1(i)+e3y(i)*ydl1(i)+e3z(i)*zdl1(i)
          xdl1(i)=xl
          ydl1(i)=yl
          zdl1(i)=zl
          xl=e1x(i)*xdl2(i)+e1y(i)*ydl2(i)+e1z(i)*zdl2(i)
          yl=e2x(i)*xdl2(i)+e2y(i)*ydl2(i)+e2z(i)*zdl2(i)
          zl=e3x(i)*xdl2(i)+e3y(i)*ydl2(i)+e3z(i)*zdl2(i)
          xdl2(i)=xl
          ydl2(i)=yl
          zdl2(i)=zl
          xl=e1x(i)*xdl3(i)+e1y(i)*ydl3(i)+e1z(i)*zdl3(i)
          yl=e2x(i)*xdl3(i)+e2y(i)*ydl3(i)+e2z(i)*zdl3(i)
          zl=e3x(i)*xdl3(i)+e3y(i)*ydl3(i)+e3z(i)*zdl3(i)
          xdl3(i)=xl
          ydl3(i)=yl
          zdl3(i)=zl
          xl=e1x(i)*xdl4(i)+e1y(i)*ydl4(i)+e1z(i)*zdl4(i)
          yl=e2x(i)*xdl4(i)+e2y(i)*ydl4(i)+e2z(i)*zdl4(i)
          zl=e3x(i)*xdl4(i)+e3y(i)*ydl4(i)+e3z(i)*zdl4(i)
          xdl4(i)=xl
          ydl4(i)=yl
          zdl4(i)=zl
          xl=e1x(i)*xdl5(i)+e1y(i)*ydl5(i)+e1z(i)*zdl5(i)
          yl=e2x(i)*xdl5(i)+e2y(i)*ydl5(i)+e2z(i)*zdl5(i)
          zl=e3x(i)*xdl5(i)+e3y(i)*ydl5(i)+e3z(i)*zdl5(i)
          xdl5(i)=xl
          ydl5(i)=yl
          zdl5(i)=zl
          xl=e1x(i)*xdl6(i)+e1y(i)*ydl6(i)+e1z(i)*zdl6(i)
          yl=e2x(i)*xdl6(i)+e2y(i)*ydl6(i)+e2z(i)*zdl6(i)
          zl=e3x(i)*xdl6(i)+e3y(i)*ydl6(i)+e3z(i)*zdl6(i)
          xdl6(i)=xl
          ydl6(i)=yl
          zdl6(i)=zl
          xl=e1x(i)*xdl7(i)+e1y(i)*ydl7(i)+e1z(i)*zdl7(i)
          yl=e2x(i)*xdl7(i)+e2y(i)*ydl7(i)+e2z(i)*zdl7(i)
          zl=e3x(i)*xdl7(i)+e3y(i)*ydl7(i)+e3z(i)*zdl7(i)
          xdl7(i)=xl
          ydl7(i)=yl
          zdl7(i)=zl
          xl=e1x(i)*xdl8(i)+e1y(i)*ydl8(i)+e1z(i)*zdl8(i)
          yl=e2x(i)*xdl8(i)+e2y(i)*ydl8(i)+e2z(i)*zdl8(i)
          zl=e3x(i)*xdl8(i)+e3y(i)*ydl8(i)+e3z(i)*zdl8(i)
          xdl8(i)=xl
          ydl8(i)=yl
          zdl8(i)=zl


       enddo


       do i=1,nel

            xd1(i) = xdl1(i)
            yd1(i) = ydl1(i)
            zd1(i) = zdl1(i)
!
            xd2(i) = xdl2(i)
            yd2(i) = ydl2(i)
            zd2(i) = zdl2(i)
!
            xd3(i) = xdl3(i)
            yd3(i) = ydl3(i)
            zd3(i) = zdl3(i)
!
            xd4(i) = xdl5(i)
            yd4(i) = ydl5(i)
            zd4(i) = zdl5(i)
!
            xd5(i) = xdl6(i)
            yd5(i) = ydl6(i)
            zd5(i) = zdl6(i)
!
            xd6(i) = xdl7(i) 
            yd6(i) = ydl7(i) 
            zd6(i) = zdl7(i) 



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
