Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  THSKEWC                       source/output/th/thskewc.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THSKEWC(
     1       RTHBUF   ,ITHGRP   ,ITHBUF,X     ,IXC     ,IXTG   ,SKEW,NTHGRP)


C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTHGRP,ITHGRP(NITHGR,*),ITHBUF(*),IXC(NIXC,*),IXTG(NIXTG,*)
      my_real
     .   RTHBUF(*), X(3,*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NNE,IAD,IAD2,IADR,ISK,NN,N1,N2,N3,N4,IGS,N,ITYP,K
C     REAL
      my_real
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4,
     .     E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,
     .     X31, Y31, Z31, X32, Y32, Z32, X21, Y21, Z21,
     .     X42, Y42, Z42,  S1, S2, VX, VY, VZ, V, VR, VS,
     .     SUMA,AREA

C   Fill table RTHBUF
        IADR=0
        DO N=1,NTHGRP
           ITYP=ITHGRP(2,N)
           NNE =ITHGRP(4,N)
           IAD =ITHGRP(5,N)
           IAD2=IAD+3*NNE
           IF(ITYP==3)THEN
              DO K=1,NNE
                 NN=ITHBUF(IAD)
                 ISK=1+ITHBUF(IAD2)
c
                 IF(ISK > 1) THEN
C             Corotational Frame E1 E2 E3 
                    N1=IXC(2,NN)
                    N2=IXC(3,NN)
                    N3=IXC(4,NN)
                    N4=IXC(5,NN)
                  
                    X1=X(1,N1)
                    X2=X(1,N2)
                    X3=X(1,N3)
                    X4=X(1,N4)

                    Y1=X(2,N1)
                    Y2=X(2,N2)
                    Y3=X(2,N3)
                    Y4=X(2,N4)

                    Z1=X(3,N1)
                    Z2=X(3,N2)
                    Z3=X(3,N3)
                    Z4=X(3,N4)


                    X21=X2-X1
                    Y21=Y2-Y1
                    Z21=Z2-Z1
                    X31=X3-X1
                    Y31=Y3-Y1
                    Z31=Z3-Z1
                    X42=X4-X2
                    Y42=Y4-Y2
                    Z42=Z4-Z2

                    E3X=Y31*Z42-Z31*Y42
                    E3Y=Z31*X42-X31*Z42
                    E3Z=X31*Y42-Y31*X42
                    SUMA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)

                    E1X = X2+X3-X1-X4
                    E1Y = Y2+Y3-Y1-Y4
                    E1Z = Z2+Z3-Z1-Z4
c
                    E2X = X3+X4-X1-X2
                    E2Y = Y3+Y4-Y1-Y2
                    E2Z = Z3+Z4-Z1-Z2
c
                    E3X = E1Y*E2Z-E1Z*E2Y
                    E3Y = E1Z*E2X-E1X*E2Z
                    E3Z = E1X*E2Y-E1Y*E2X

                    SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z  
                    SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
                    E3X = E3X*SUMA                              
                    E3Y = E3Y*SUMA                              
                    E3Z = E3Z*SUMA                              
c
                    S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z
                    S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z
                    SUMA   = SQRT(S1/S2)                
                    E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA
                    E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA
                    E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA
c
                   SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z 
                   SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
                   E1X = E1X*SUMA                              
                   E1Y = E1Y*SUMA                              
                   E1Z = E1Z*SUMA                              
c
                   E2X = E3Y * E1Z - E3Z * E1Y
                   E2Y = E3Z * E1X - E3X * E1Z
                   E2Z = E3X * E1Y - E3Y * E1X

C            Project First axe of the skew
                   VX = SKEW(1,ISK)
                   VY = SKEW(2,ISK)
                   VZ = SKEW(3,ISK)

                   V =VX*E3X+VY*E3Y+VZ*E3Z
                   VX=VX-V*E3X
                   VY=VY-V*E3Y
                   VZ=VZ-V*E3Z
                   V =SQRT(VX*VX+VY*VY+VZ*VZ)

                   VX=VX/MAX(V,EM20)
                   VY=VY/MAX(V,EM20)
                   VZ=VZ/MAX(V,EM20)

C           Cos and Sin calculation
                   VR=VX*E1X+VY*E1Y+VZ*E1Z
                   VS=VX*E2X+VY*E2Y+VZ*E2Z
c           Save data in RTHBUF
                   ITHBUF(IAD2)=IADR+1
                   RTHBUF(IADR+1)=VR
                   RTHBUF(IADR+2)=VS 
 
                   IADR=IADR+2
                 ENDIF
                 IAD=IAD+1
                 IAD2=IAD2+1
              ENDDO
           ELSEIF(ITYP==7)THEN
              DO K=1,NNE
                 NN=ITHBUF(IAD)
                 ISK=ITHBUF(IAD2)
                 IF(ISK /= 0) THEN
C             Corotational Frame E1 E2 E3 
                    N1=IXC(2,NN)
                    N2=IXC(3,NN)
                    N3=IXC(4,NN)

                    X1=X(1,N1)
                    X2=X(1,N2)
                    X3=X(1,N3)

                    Y1=X(2,N1)
                    Y2=X(2,N2)
                    Y3=X(2,N3)

                    Z1=X(3,N1)
                    Z2=X(3,N2)
                    Z3=X(3,N3)

                    X21=X2-X1
                    Y21=Y2-Y1
                    Z21=Z2-Z1
                    X31=X3-X1
                    Y31=Y3-Y1
                    Z31=Z3-Z1
                    X32=X3-X2
                    Y32=Y3-Y2
                    Z32=Z3-Z2
c
                    E1X= X21
                    E1Y= Y21
                    E1Z= Z21
                    SUMA = SQRT(E1X*E1X+E1Y*E1Y+E1Z*E1Z)
                    E1X=E1X/SUMA
                    E1Y=E1Y/SUMA
                    E1Z=E1Z/SUMA
c
                    E3X=Y31*Z32-Z31*Y32
                    E3Y=Z31*X32-X31*Z32
                    E3Z=X31*Y32-Y31*X32
                    SUMA = SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
                    E3X=E3X/SUMA
                    E3Y=E3Y/SUMA
                    E3Z=E3Z/SUMA
                    AREA = HALF * SUMA
c
                    E2X=E3Y*E1Z-E3Z*E1Y
                    E2Y=E3Z*E1X-E3X*E1Z
                    E2Z=E3X*E1Y-E3Y*E1X
                    SUMA = SQRT(E2X*E2X+E2Y*E2Y+E2Z*E2Z)
                    E2X=E2X/SUMA
                    E2Y=E2Y/SUMA
                    E2Z=E2Z/SUMA

C            Project First axe of the skew
                    VX = SKEW(1,ISK)
                    VY = SKEW(2,ISK)
                    VZ = SKEW(3,ISK)

                    V =VX*E3X+VY*E3Y+VZ*E3Z
                    VX=VX-V*E3X
                    VY=VY-V*E3Y
                    VZ=VZ-V*E3Z
                    V =SQRT(VX*VX+VY*VY+VZ*VZ)

                    VX=VX/MAX(V,EM20)
                    VY=VY/MAX(V,EM20)
                    VZ=VZ/MAX(V,EM20)
C           Cos and Sin calculation
                    VR=VX*E1X+VY*E1Y+VZ*E1Z
                    VS=VX*E2X+VY*E2Y+VZ*E2Z

c           Save data in RTHBUF
                    ITHBUF(IAD2)=IADR+1
                    RTHBUF(IADR+1)=VR
                    RTHBUF(IADR+2)=VS 
                    IADR=IADR+2
                 ENDIF
                 IAD=IAD+1
                 IAD2=IAD2+1
              ENDDO
           ENDIF
        ENDDO
      RETURN
      END

