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
C
Chd|====================================================================
Chd|  NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|-- called by -----------
Chd|        FINDELE                       source/boundary_conditions/ebcs/findele.F
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I12SOL3                       source/interfaces/inter3d1/insol3.F
Chd|        I20NELTS                      source/interfaces/inter3d1/inelt.F
Chd|        I2SURFS                       source/interfaces/inter3d1/i2surfs.F
Chd|        INELTIGEO                     source/interfaces/inter3d1/inelt.F
Chd|        INELTS                        source/interfaces/inter3d1/inelt.F
Chd|        INELTS_NP                     source/interfaces/inter3d1/i24sti3.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NORMA1(N1,N2,N3,AREA)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   N1, N2, N3, AREA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.2.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
      XX13 =XX1(3)-XX1(1)
      YY13 =XX2(3)-XX2(1)
      ZZ13 =XX3(3)-XX3(1)
      XX24 =XX1(4)-XX1(2)
      YY24 =XX2(4)-XX2(2)
      ZZ24 =XX3(4)-XX3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
      AREA=UNDEMI*AREA
      RETURN
      END



Chd|====================================================================
Chd|  NORMA1D                       source/interfaces/inter3d1/norma1.F
Chd|-- called by -----------
Chd|        INSOL3D                       source/interfaces/inter3d1/insol3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NORMA1D(N1,N2,N3,AREA)
C
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
      my_real
     .   N1, N2, N3, AREA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.2.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
      my_real
     .   XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
      XX13 =XX1(3)-XX1(1)
      YY13 =XX2(3)-XX2(1)
      ZZ13 =XX3(3)-XX3(1)
      XX24 =XX1(4)-XX1(2)
      YY24 =XX2(4)-XX2(2)
      ZZ24 =XX3(4)-XX3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
C
      AREA=UNDEMI*AREA
      RETURN
      END



Chd|====================================================================
Chd|  NORMA4N                       source/interfaces/inter3d1/norma1.F
Chd|-- called by -----------
Chd|        I25NEIGH_REMOVEALLBUT1        source/interfaces/inter3d1/i25neigh.F
Chd|        REMOVEIC                      source/interfaces/inter3d1/i24tools.F
Chd|        REMOVEIC1                     source/interfaces/inter3d1/i24tools.F
Chd|        SEG_OPP                       source/interfaces/inter3d1/i24tools.F
Chd|        VOISIN2                       source/interfaces/inter3d1/i24tools.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NORMA4N(N1,N2,N3,AREA,IRECT,X)
C
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
      INTEGER IRECT(4)
C     REAL ou REAL*8
      my_real
     .   N1, N2, N3, AREA, X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,NN
C     REAL ou REAL*8
      my_real
     .   X1(4),X2(4),X3(4),XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
       DO J=1,4
        NN=IRECT(J)
        X1(J)=X(1,NN)
        X2(J)=X(2,NN)
        X3(J)=X(3,NN)
       END DO 
C       
      XX13 =X1(3)-X1(1)
      YY13 =X2(3)-X2(1)
      ZZ13 =X3(3)-X3(1)
      XX24 =X1(4)-X1(2)
      YY24 =X2(4)-X2(2)
      ZZ24 =X3(4)-X3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
C
      AREA=UNDEMI*AREA
C      
      RETURN
      END
Chd|====================================================================
Chd|  NORMA4NX                      source/interfaces/inter3d1/norma1.F
Chd|-- called by -----------
Chd|        VOISIN2                       source/interfaces/inter3d1/i24tools.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NORMA4NX(N1,N2,N3,AREA,IRECT,X ,I1,I2,SHF)
C
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
      INTEGER IRECT(4),I1,I2
C     REAL ou REAL*8
      my_real
     .   N1, N2, N3, AREA, X(3,*),SHF(3)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
c FUNCTION: calculate normale with pertubation
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,NN
C     REAL ou REAL*8
      my_real
     .   X1(4),X2(4),X3(4),XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
       DO J=1,4
        NN=IRECT(J)
        IF (NN == I1 .OR. NN == I2) THEN
         X1(J)=X(1,NN)
         X2(J)=X(2,NN)
         X3(J)=X(3,NN)
        ELSE
         X1(J)=X(1,NN)+SHF(1)
         X2(J)=X(2,NN)+SHF(2)
         X3(J)=X(3,NN)+SHF(3)
        END IF        
       END DO 
C       
      XX13 =X1(3)-X1(1)
      YY13 =X2(3)-X2(1)
      ZZ13 =X3(3)-X3(1)
      XX24 =X1(4)-X1(2)
      YY24 =X2(4)-X2(2)
      ZZ24 =X3(4)-X3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
C
      AREA=UNDEMI*AREA
C      
      RETURN
      END
