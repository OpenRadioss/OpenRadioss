Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      
Chd|====================================================================
Chd|  GETVLOC                       source/elements/solid/solide8s/getvloc.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETVLOC(
     1   TRM,     VX1,     VX2,     VX3,
     2   VX4,     VX5,     VX6,     VX7,
     3   VX8,     VY1,     VY2,     VY3,
     4   VY4,     VY5,     VY6,     VY7,
     5   VY8,     VZ1,     VZ2,     VZ3,
     6   VZ4,     VZ5,     VZ6,     VZ7,
     7   VZ8,     ULX1,    ULX2,    ULX3,
     8   ULX4,    ULX5,    ULX6,    ULX7,
     9   ULX8,    ULY1,    ULY2,    ULY3,
     A   ULY4,    ULY5,    ULY6,    ULY7,
     B   ULY8,    ULZ1,    ULZ2,    ULZ3,
     C   ULZ4,    ULZ5,    ULZ6,    ULZ7,
     D   ULZ8,    R,       NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
!      my_real
!     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   TRM(NEL,24,24), R(3,3,MVSIZ),    
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ)
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J,K    
C=======================================================================
      DOUBLE PRECISION U,V,W!,UL,VL,WL 
      DOUBLE PRECISION UG(24),UL(24) 
c
      DO I=1,NEL
        UG(1) = ZERO 
        UG(2) = ZERO 
        UG(3) = ZERO 
        UG(4) = VX2(I)-VX1(I)
        UG(5) = VY2(I)-VY1(I)
        UG(6) = VZ2(I)-VZ1(I)
        UG(7) = VX3(I)-VX1(I)
        UG(8) = VY3(I)-VY1(I)
        UG(9) = VZ3(I)-VZ1(I)
        UG(10) = VX4(I)-VX1(I)
        UG(11) = VY4(I)-VY1(I)
        UG(12) = VZ4(I)-VZ1(I)
        UG(13) = VX5(I)-VX1(I)
        UG(14) = VY5(I)-VY1(I)
        UG(15) = VZ5(I)-VZ1(I)
        UG(16) = VX6(I)-VX1(I)
        UG(17) = VY6(I)-VY1(I)
        UG(18) = VZ6(I)-VZ1(I)
        UG(19) = VX7(I)-VX1(I)
        UG(20) = VY7(I)-VY1(I)
        UG(21) = VZ7(I)-VZ1(I)
        UG(22) = VX8(I)-VX1(I)
        UG(23) = VY8(I)-VY1(I)
        UG(24) = VZ8(I)-VZ1(I)
!        if (i==10) write(*,'("Vitesse G ",/,3(1x,1pe10.3))')   ug(1:24)
          
        DO J=1,24
          UL(J) = ZERO
          DO K=1,24
            UL(J) = UL(J)+TRM(I,J,K)*UG(K)
          ENDDO
        ENDDO
        
!        if (i==10) write(*,'("vLOC ",/,3(1x,1pe10.3))')   ul(1:24)
        !
        !U = R11(I)*VX1(I)+R21(I)*VY1(I)+R31(I)*VZ1(I)
        !V = R12(I)*VX1(I)+R22(I)*VY1(I)+R32(I)*VZ1(I)
        !W = R13(I)*VX1(I)+R23(I)*VY1(I)+R33(I)*VZ1(I)
        
        ULX1(I) = UL(1)!-u
        ULY1(I) = UL(2)!-v
        ULZ1(I) = UL(3)!-w
        ULX2(I) = UL(4)!-u
        ULY2(I) = UL(5)!-v
        ULZ2(I) = UL(6)!-w
        ULX3(I) = UL(7)!-u
        ULY3(I) = UL(8)!-v
        ULZ3(I) = UL(9)!-w
        ULX4(I) = UL(10)!-u
        ULY4(I) = UL(11)!-v
        ULZ4(I) = UL(12)!-w
        ULX5(I) = UL(13)!-u
        ULY5(I) = UL(14)!-v
        ULZ5(I) = UL(15)!-w
        ULX6(I) = UL(16)!-u
        ULY6(I) = UL(17)!-v
        ULZ6(I) = UL(18)!-w
        ULX7(I) = UL(19)!-u
        ULY7(I) = UL(20)!-v
        ULZ7(I) = UL(21)!-w
        ULX8(I) = UL(22)!-u
        ULY8(I) = UL(23)!-v
        ULZ8(I) = UL(24)!-w
        !if (i==10) then
        !  write(*,'("vLx vly vlz ",/,3(1x,1pe10.3))')   ULX1(I),ULY1(I),ULZ1(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX2(I),ULY2(I),ULZ2(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX3(I),ULY3(I),ULZ3(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX4(I),ULY4(I),ULZ4(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX5(I),ULY5(I),ULZ5(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX6(I),ULY6(I),ULZ6(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX7(I),ULY7(I),ULZ7(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX8(I),ULY8(I),ULZ8(I)
        !endif
      ENDDO
!      print*,"vitesses :"
!      DO I=1,NEL
!        ULX1(I) = ZERO    ! sb : On pourrait eviter de stocker ces 3 tables (toujours zero)
!        ULY1(I) = ZERO
!        ULZ1(I) = ZERO
!
!        U = VX2(I)-VX1(I)
!        V = VY2(I)-VY1(I)
!        W = VZ2(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX2(I) = UL
!        ULY2(I) = VL
!        ULZ2(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!
!        U = VX3(I)-VX1(I)
!        V = VY3(I)-VY1(I)
!        W = VZ3(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX3(I) = UL
!        ULY3(I) = VL
!        ULZ3(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX4(I)-VX1(I)
!        V = VY4(I)-VY1(I)
!        W = VZ4(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX4(I) = UL
!        ULY4(I) = VL
!        ULZ4(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX5(I)-VX1(I)
!        V = VY5(I)-VY1(I)
!        W = VZ5(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX5(I) = UL
!        ULY5(I) = VL
!        ULZ5(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX6(I)-VX1(I)
!        V = VY6(I)-VY1(I)
!        W = VZ6(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX6(I) = UL
!        ULY6(I) = VL
!        ULZ6(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX7(I)-VX1(I)
!        V = VY7(I)-VY1(I)
!        W = VZ7(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX7(I) = UL
!        ULY7(I) = VL
!        ULZ7(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX8(I)-VX1(I)
!        V = VY8(I)-VY1(I)
!        W = VZ8(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX8(I) = UL
!        ULY8(I) = VL
!        ULZ8(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!        if (i==10) then
!          write(*,'("vLx vly vlz ",/,3(1x,1pe10.3))')   ULX1(I),ULY1(I),ULZ1(I)
!          write(*,'(3(1x,1pe10.3))')   ULX2(I),ULY2(I),ULZ2(I)
!          write(*,'(3(1x,1pe10.3))')   ULX3(I),ULY3(I),ULZ3(I)
!          write(*,'(3(1x,1pe10.3))')   ULX4(I),ULY4(I),ULZ4(I)
!          write(*,'(3(1x,1pe10.3))')   ULX5(I),ULY5(I),ULZ5(I)
!          write(*,'(3(1x,1pe10.3))')   ULX6(I),ULY6(I),ULZ6(I)
!          write(*,'(3(1x,1pe10.3))')   ULX7(I),ULY7(I),ULZ7(I)
!          write(*,'(3(1x,1pe10.3))')   ULX8(I),ULY8(I),ULZ8(I)
!        endif
!       
!      ENDDO
      RETURN
      END
