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
Chd|  I9COR3                        source/interfaces/int09/i9cor3.F
Chd|-- called by -----------
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I9COR3(X     ,V     ,A       ,IRECT ,MSR   ,
     2                  NSV   ,IRTL  ,IX1     ,IX2   ,IX3   ,
     3                  IX4   ,X1    ,X2      ,X3    ,X4    ,
     4                  XI    ,Y1    ,Y2      ,Y3    ,Y4    ,
     5                  YI    ,Z1    ,Z2      ,Z3    ,Z4    ,
     6                  ZI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), MSR(*), NSV(*), IRTL(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(OUT):: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  Z1,Z2,Z3,Z4,ZI
      my_real
     .   X(3,*),V(3,*),A(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, L, NN
C-----------------------------------------------
!      EQUIVALENCE (Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (YI,BV(1,14)),(ZI,BV(1,15))
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        IL=I+NFT
        IG=NSV(IL)
        XI(I)=X(1,IG) + (V(1,IG) + A(1,IG)*DT12)*DT2
        YI(I)=X(2,IG) + (V(2,IG) + A(2,IG)*DT12)*DT2
        ZI(I)=X(3,IG) + (V(3,IG) + A(3,IG)*DT12)*DT2
      ENDDO
      
      DO I=LFT,LLT
      IL=I+NFT
      IG=NSV(IL)
      L=IRTL(IL)
      NN=MSR(IRECT(1,L))
      IX1(I)=NN
      X1(I)=X(1,NN) + (V(1,NN) + A(1,NN)*DT12)*DT2
      Y1(I)=X(2,NN) + (V(2,NN) + A(2,NN)*DT12)*DT2
      Z1(I)=X(3,NN) + (V(3,NN) + A(3,NN)*DT12)*DT2
      NN=MSR(IRECT(2,L))
      IX2(I)=NN
      X2(I)=X(1,NN) + (V(1,NN) + A(1,NN)*DT12)*DT2
      Y2(I)=X(2,NN) + (V(2,NN) + A(2,NN)*DT12)*DT2
      Z2(I)=X(3,NN) + (V(3,NN) + A(3,NN)*DT12)*DT2
      NN=MSR(IRECT(3,L))
      IX3(I)=NN
      X3(I)=X(1,NN) + (V(1,NN) + A(1,NN)*DT12)*DT2
      Y3(I)=X(2,NN) + (V(2,NN) + A(2,NN)*DT12)*DT2
      Z3(I)=X(3,NN) + (V(3,NN) + A(3,NN)*DT12)*DT2
      NN=MSR(IRECT(4,L))
      IX4(I)=NN
      X4(I)=X(1,NN) + (V(1,NN) + A(1,NN)*DT12)*DT2
      Y4(I)=X(2,NN) + (V(2,NN) + A(2,NN)*DT12)*DT2
      Z4(I)=X(3,NN) + (V(3,NN) + A(3,NN)*DT12)*DT2
      ENDDO
C-----------------------------------------------
      RETURN
      END
