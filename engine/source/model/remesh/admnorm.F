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
Chd|  ADMNORM4                      source/model/remesh/admnorm.F 
Chd|-- called by -----------
Chd|        ADMDIV                        source/model/remesh/admdiv.F  
Chd|-- calls ---------------
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMNORM4(N,IXC,X)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, IXC(NIXC,*)
      my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SON,LEVEL,IP,NLEV,N1,N2,N3,N4
      my_real
     .        X13,Y13,Z13,X24,Y24,Z24,NX,NY,NZ,AAA
C-----------------------------------------------
      N1=IXC(2,N)
      N2=IXC(3,N)
      N3=IXC(4,N)
      N4=IXC(5,N)

      TAGNOD(N1)=1
      TAGNOD(N2)=1
      TAGNOD(N3)=1
      TAGNOD(N4)=1

      X13 = X(1,N3) - X(1,N1)
      Y13 = X(2,N3) - X(2,N1)
      Z13 = X(3,N3) - X(3,N1)

      X24 = X(1,N4) - X(1,N2)
      Y24 = X(2,N4) - X(2,N2)
      Z24 = X(3,N4) - X(3,N2)

      NX = Y13*Z24 - Z13*Y24
      NY = Z13*X24 - X13*Z24
      NZ = X13*Y24 - Y13*X24

      AAA=UN/MAX(EM30,SQRT(NX*NX+NY*NY+NZ*NZ))
      NX = NX * AAA
      NY = NY * AAA
      NZ = NZ * AAA

      NODNORM(1,N1)=NODNORM(1,N1)+NX
      NODNORM(2,N1)=NODNORM(2,N1)+NY
      NODNORM(3,N1)=NODNORM(3,N1)+NZ
c  
      NODNORM(1,N2)=NODNORM(1,N2)+NX
      NODNORM(2,N2)=NODNORM(2,N2)+NY
      NODNORM(3,N2)=NODNORM(3,N2)+NZ
c  
      NODNORM(1,N3)=NODNORM(1,N3)+NX
      NODNORM(2,N3)=NODNORM(2,N3)+NY
      NODNORM(3,N3)=NODNORM(3,N3)+NZ
c  
      NODNORM(1,N4)=NODNORM(1,N4)+NX
      NODNORM(2,N4)=NODNORM(2,N4)+NY
      NODNORM(3,N4)=NODNORM(3,N4)+NZ

      RETURN
      END     

Chd|====================================================================
Chd|  ADMNORM3                      source/model/remesh/admnorm.F 
Chd|-- called by -----------
Chd|        ADMDIV                        source/model/remesh/admdiv.F  
Chd|-- calls ---------------
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMNORM3(N,IXTG,X)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, IXTG(NIXTG,*)
      my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N3
      my_real
     .       X12,Y12,Z12,X13,Y13,Z13,NX,NY,NZ,AAA
C-----------------------------------------------
      N1=IXTG(2,N)
      N2=IXTG(3,N)
      N3=IXTG(4,N)

      TAGNOD(N1)=1
      TAGNOD(N2)=1
      TAGNOD(N3)=1

      X13 = X(1,N3) - X(1,N1)
      Y13 = X(2,N3) - X(2,N1)
      Z13 = X(3,N3) - X(3,N1)

      X12 = X(1,N2) - X(1,N1)
      Y12 = X(2,N2) - X(2,N1)
      Z12 = X(3,N2) - X(3,N1)

      NX = Y12*Z13 - Z12*Y13
      NY = Z12*X13 - X12*Z13
      NZ = X12*Y13 - Y12*X13

      AAA=UN/MAX(EM30,SQRT(NX*NX+NY*NY+NZ*NZ))
      NX = NX * AAA
      NY = NY * AAA
      NZ = NZ * AAA

      NODNORM(1,N1)=NODNORM(1,N1)+NX
      NODNORM(2,N1)=NODNORM(2,N1)+NY
      NODNORM(3,N1)=NODNORM(3,N1)+NZ
c  
      NODNORM(1,N2)=NODNORM(1,N2)+NX
      NODNORM(2,N2)=NODNORM(2,N2)+NY
      NODNORM(3,N2)=NODNORM(3,N2)+NZ
c  
      NODNORM(1,N3)=NODNORM(1,N3)+NX
      NODNORM(2,N3)=NODNORM(2,N3)+NY
      NODNORM(3,N3)=NODNORM(3,N3)+NZ

      RETURN
      END     





