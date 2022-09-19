Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SFINT43                       source/elements/solid/sconnect/sfint43.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SFINT43(IPG   ,NPG   ,NEL   ,HH    ,AREAPG,SOFT  ,
     .     FX1   ,FX2   ,FX3   ,FX4   ,FX5   ,FX6   ,FX7   ,FX8   ,
     .     FY1   ,FY2   ,FY3   ,FY4   ,FY5   ,FY6   ,FY7   ,FY8   ,
     .     FZ1   ,FZ2   ,FZ3   ,FZ4   ,FZ5   ,FZ6   ,FZ7   ,FZ8   ,
     .     SIGNZZ,SIGNYZ,SIGNZX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPG,NEL,NPG
      my_real, DIMENSION(NEL) :: AREAPG,SIGNZZ,SIGNYZ,SIGNZX,SOFT,
     .  FX1,FX2,FX3,FX4,FX5,FX6,FX7,FX8,FY1,FY2,FY3,FY4,FY5,FY6,FY7,FY8,
     .  FZ1,FZ2,FZ3,FZ4,FZ5,FZ6,FZ7,FZ8
      my_real, DIMENSION(NPG,NPG) :: HH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real FZZ,FYZ,FZX,RX,RY
      my_real, DIMENSION(NEL) :: DFX1,DFX2,DFX3,DFX4,DFX5,DFX6,DFX7,DFX8,
     .                           DFY1,DFY2,DFY3,DFY4,DFY5,DFY6,DFY7,DFY8,
     .                           DFZ1,DFZ2,DFZ3,DFZ4,DFZ5,DFZ6,DFZ7,DFZ8
C=======================================================================
      DO I=1,NEL
        FZZ = SIGNZZ(I)*AREAPG(I)*SOFT(I)
        FYZ = SIGNYZ(I)*AREAPG(I)*SOFT(I)
        FZX = SIGNZX(I)*AREAPG(I)*SOFT(I)
C       inf
        DFX1(I) = FZX*HH(1,IPG)
        DFX2(I) = FZX*HH(2,IPG)
        DFX3(I) = FZX*HH(3,IPG)
        DFX4(I) = FZX*HH(4,IPG)
        DFY1(I) = FYZ*HH(1,IPG)
        DFY2(I) = FYZ*HH(2,IPG)
        DFY3(I) = FYZ*HH(3,IPG)
        DFY4(I) = FYZ*HH(4,IPG)
        DFZ1(I) = FZZ*HH(1,IPG)
        DFZ2(I) = FZZ*HH(2,IPG)
        DFZ3(I) = FZZ*HH(3,IPG)
        DFZ4(I) = FZZ*HH(4,IPG)
C       sup
        DFX5(I) =-FZX*HH(1,IPG)
        DFX6(I) =-FZX*HH(2,IPG)
        DFX7(I) =-FZX*HH(3,IPG)
        DFX8(I) =-FZX*HH(4,IPG)
        DFY5(I) =-FYZ*HH(1,IPG)
        DFY6(I) =-FYZ*HH(2,IPG)
        DFY7(I) =-FYZ*HH(3,IPG)
        DFY8(I) =-FYZ*HH(4,IPG)
        DFZ5(I) =-FZZ*HH(1,IPG)
        DFZ6(I) =-FZZ*HH(2,IPG)
        DFZ7(I) =-FZZ*HH(3,IPG)
        DFZ8(I) =-FZZ*HH(4,IPG)
      ENDDO
C-----------------------------
      DO I=1,NEL
C       inf
        FX1(I) = FX1(I) + DFX1(I)
        FX2(I) = FX2(I) + DFX2(I)
        FX3(I) = FX3(I) + DFX3(I)
        FX4(I) = FX4(I) + DFX4(I)
        FY1(I) = FY1(I) + DFY1(I)
        FY2(I) = FY2(I) + DFY2(I)
        FY3(I) = FY3(I) + DFY3(I)
        FY4(I) = FY4(I) + DFY4(I)
        FZ1(I) = FZ1(I) + DFZ1(I)
        FZ2(I) = FZ2(I) + DFZ2(I)
        FZ3(I) = FZ3(I) + DFZ3(I)
        FZ4(I) = FZ4(I) + DFZ4(I)
C       sup             
        FX5(I) = FX5(I) + DFX5(I)
        FX6(I) = FX6(I) + DFX6(I)
        FX7(I) = FX7(I) + DFX7(I)
        FX8(I) = FX8(I) + DFX8(I)
        FY5(I) = FY5(I) + DFY5(I)
        FY6(I) = FY6(I) + DFY6(I)
        FY7(I) = FY7(I) + DFY7(I)
        FY8(I) = FY8(I) + DFY8(I)
        FZ5(I) = FZ5(I) + DFZ5(I)
        FZ6(I) = FZ6(I) + DFZ6(I)
        FZ7(I) = FZ7(I) + DFZ7(I)
        FZ8(I) = FZ8(I) + DFZ8(I)
      ENDDO
C-----------
      RETURN
      END
