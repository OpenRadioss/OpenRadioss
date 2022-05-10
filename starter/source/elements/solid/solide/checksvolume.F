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
#include "my_real.inc"
      my_real 
Chd|====================================================================
Chd|  CHECKVOLUME_8N                source/elements/solid/solide/checksvolume.F
Chd|-- called by -----------
Chd|        S16COOR3                      source/elements/thickshell/solide16/s16coor3.F
Chd|        S20COOR3                      source/elements/solid/solide20/s20coor3.F
Chd|        SCCOOR3                       source/elements/thickshell/solidec/sccoor3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SPCOOR3                       source/elements/solid/sconnect/spcoor3.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|-- calls ---------------
Chd|====================================================================
     .   FUNCTION CHECKVOLUME_8N(X ,IXS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS)
      my_real 
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   X17, X28, X35, X46,Y17, Y28, Y35, Y46, Z17, Z28, Z35, Z46,
     .   AJ1,AJ2,AJ3,AJ4,AJ5,AJ6,AJ7,AJ8,AJ9,A17,A28,B17,B28,C17,C28, 
     .   AJ5968, AJ6749, AJ4857
C=======================================================================
      CHECKVOLUME_8N = ZERO
C
      X1=X(1,IXS(2))
      Y1=X(2,IXS(2))
      Z1=X(3,IXS(2))
      X2=X(1,IXS(3))
      Y2=X(2,IXS(3))
      Z2=X(3,IXS(3))
      X3=X(1,IXS(4))
      Y3=X(2,IXS(4))
      Z3=X(3,IXS(4))
      X4=X(1,IXS(5))
      Y4=X(2,IXS(5))
      Z4=X(3,IXS(5))
      X5=X(1,IXS(6))
      Y5=X(2,IXS(6))
      Z5=X(3,IXS(6))
      X6=X(1,IXS(7))
      Y6=X(2,IXS(7))
      Z6=X(3,IXS(7))
      X7=X(1,IXS(8))
      Y7=X(2,IXS(8))
      Z7=X(3,IXS(8))
      X8=X(1,IXS(9))
      Y8=X(2,IXS(9))
      Z8=X(3,IXS(9))
C
      X17=X7-X1
      X28=X8-X2
      X35=X5-X3
      X46=X6-X4
      Y17=Y7-Y1
      Y28=Y8-Y2
      Y35=Y5-Y3
      Y46=Y6-Y4
      Z17=Z7-Z1
      Z28=Z8-Z2
      Z35=Z5-Z3
      Z46=Z6-Z4
C
      AJ1=X17+X28-X35-X46
      AJ2=Y17+Y28-Y35-Y46
      AJ3=Z17+Z28-Z35-Z46
      A17=X17+X46
      A28=X28+X35
      B17=Y17+Y46
      B28=Y28+Y35
      C17=Z17+Z46
      C28=Z28+Z35
      AJ4=A17+A28
      AJ5=B17+B28
      AJ6=C17+C28
      AJ7=A17-A28
      AJ8=B17-B28
      AJ9=C17-C28
      AJ5968=AJ5*AJ9-AJ6*AJ8
      AJ6749=AJ6*AJ7-AJ4*AJ9
      AJ4857=AJ4*AJ8-AJ5*AJ7
      CHECKVOLUME_8N=O64TH*(AJ1*AJ5968+AJ2*AJ6749+AJ3*AJ4857)
C-----------
      RETURN
      END
      

      my_real 
Chd|====================================================================
Chd|  CHECKVOLUME_6N                source/elements/solid/solide/checksvolume.F
Chd|-- called by -----------
Chd|        S6CCOOR3                      source/elements/thickshell/solide6c/s6ccoor3.F
Chd|-- calls ---------------
Chd|====================================================================
     .   FUNCTION CHECKVOLUME_6N(X ,IXS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS)
      my_real 
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   X1,X2,X3,X4,X5,X6,Y1,Y2,Y3,Y4,Y5,Y6,Z1,Z2,Z3,Z4,Z5,Z6,
     .   X21,X31,X41,X54,X64,Y21,Y31,Y41,Y54,Y64,Z21,Z31,Z41,Z54,Z64,
     .   AJ1,AJ2,AJ3,AJ4,AJ5,AJ6,AJ7,AJ8,AJ9, AJ5968,AJ6749,AJ4857
C=======================================================================
      CHECKVOLUME_6N = ZERO
C
      X1=X(1,IXS(2))
      Y1=X(2,IXS(2))
      Z1=X(3,IXS(2))
      X2=X(1,IXS(3))
      Y2=X(2,IXS(3))
      Z2=X(3,IXS(3))
      X3=X(1,IXS(4))
      Y3=X(2,IXS(4))
      Z3=X(3,IXS(4))
      X4=X(1,IXS(6))
      Y4=X(2,IXS(6))
      Z4=X(3,IXS(6))
      X5=X(1,IXS(7))
      Y5=X(2,IXS(7))
      Z5=X(3,IXS(7))
      X6=X(1,IXS(8))
      Y6=X(2,IXS(8))
      Z6=X(3,IXS(8))
C
      X21=X2-X1
      X31=X3-X1
      X41=X4-X1
      X54=X5-X4
      X64=X6-X4
      Y21=Y2-Y1
      Y31=Y3-Y1
      Y41=Y4-Y1
      Y54=Y5-Y4
      Y64=Y6-Y4
      Z21=Z2-Z1
      Z31=Z3-Z1
      Z41=Z4-Z1
      Z54=Z5-Z4
      Z64=Z6-Z4
C
      AJ1=X21+X54
      AJ2=Y21+Y54
      AJ3=Z21+Z54
      AJ4=X31+X64
      AJ5=Y31+Y64
      AJ6=Z31+Z64
      AJ7=THIRD*(X41+X5-X2+X6-X3)
      AJ8=THIRD*(Y41+Y5-Y2+Y6-Y3)
      AJ9=THIRD*(Z41+Z5-Z2+Z6-Z3)
C
      AJ5968=AJ5*AJ9-AJ6*AJ8
      AJ6749=AJ6*AJ7-AJ4*AJ9
      AJ4857=AJ4*AJ8-AJ5*AJ7
C
      CHECKVOLUME_6N=UNHUIT*(AJ1*AJ5968+AJ2*AJ6749+AJ3*AJ4857)
C-----------
      RETURN
      END
      

      my_real 
Chd|====================================================================
Chd|  CHECKVOLUME_4N                source/elements/solid/solide/checksvolume.F
Chd|-- called by -----------
Chd|        S10COOR3                      source/elements/solid/solide10/s10coor3.F
Chd|        S10COOR3_OLD                  source/elements/solid/solide10/s10coor3.F
Chd|        S4COOR3                       source/elements/solid/solide4/s4coor3.F
Chd|        SOLTOSPHX4                    source/elements/sph/soltosph.F
Chd|-- calls ---------------
Chd|====================================================================
     .   FUNCTION CHECKVOLUME_4N(X ,IXS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS)
      my_real 
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   X1,X2,X3,X4,X5,X6,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43, 
     .   B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4
C=======================================================================
      CHECKVOLUME_4N = ZERO
C
      X1=X(1,IXS(2))
      Y1=X(2,IXS(2))
      Z1=X(3,IXS(2))
      X2=X(1,IXS(4))
      Y2=X(2,IXS(4))
      Z2=X(3,IXS(4))
      X3=X(1,IXS(7))
      Y3=X(2,IXS(7))
      Z3=X(3,IXS(7))
      X4=X(1,IXS(6))
      Y4=X(2,IXS(6))
      Z4=X(3,IXS(6))
C
      X43 = X4 - X3 
      Y43 = Y4 - Y3 
      Z43 = Z4 - Z3 
      X41 = X4 - X1 
      Y41 = Y4 - Y1 
      Z41 = Z4 - Z1 
      X42 = X4 - X2 
      Y42 = Y4 - Y2 
      Z42 = Z4 - Z2 
C
      B1 = Y43*Z42 - Y42*Z43        
      B2 = Y41*Z43 - Y43*Z41        
      B3 = Y42*Z41 - Y41*Z42        
      B4 = -(B1 + B2 + B3) 
C
      C1 = Z43*X42 - Z42*X43        
      C2 = Z41*X43 - Z43*X41        
      C3 = Z42*X41 - Z41*X42        
      C4 = -(C1 + C2 + C3) 
C
      D1 = X43*Y42 - X42*Y43        
      D2 = X41*Y43 - X43*Y41        
      D3 = X42*Y41 - X41*Y42        
      D4 = -(D1 + D2 + D3) 
C
      CHECKVOLUME_4N = (X41*B1 + Y41*C1 + Z41*D1)*UNSIX
C-----------
      RETURN
      END
      
