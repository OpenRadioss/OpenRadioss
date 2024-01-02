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
Chd|  S6MASS3                       source/elements/thickshell/solide6c/s6mass3.F
Chd|-- called by -----------
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|-- calls ---------------
Chd|        S6FRACA                       source/elements/thickshell/solide6c/s6fraca3.F
Chd|====================================================================
      SUBROUTINE S6MASS3(RHO,MS,PARTSAV,X,V,IPART,MSS,
     .     RHOCP,MCP ,MCPS,MSSA,FILL, VOLU, 
     .     NC1, NC2, NC3, NC4, NC5, NC6)
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
      INTEGER IPART(*),
     .     NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*)
      my_real
     .   RHO(*), MS(*),X(3,*),V(3,*),PARTSAV(20,*),
     .   RHOCP(*), MCP(*), MCPS(8,*), MSSA(*), FILL(*), VOLU(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, IP,I1,I2,I3,I4,I5,I6, J
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX,MASS(MVSIZ),MSS(8,*), MASSP,PTG(MVSIZ,3)
C-----------------------------------------------------------------------
       CALL S6FRACA(X,NC1, NC2, NC3, NC4, NC5, NC6 ,PTG   ,LLT )
       DO I=LFT,LLT
        MASS(I)=FILL(I)*RHO(I)*VOLU(I)*ONE_OVER_6
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        MSS(1,I)=MASS(I)*PTG(I,1)
        MSS(2,I)=MASS(I)*PTG(I,2)
        MSS(3,I)=MASS(I)*PTG(I,3)
        MSS(4,I)=ZERO
        MSS(5,I)=MASS(I)*PTG(I,1)
        MSS(6,I)=MASS(I)*PTG(I,2)
        MSS(7,I)=MASS(I)*PTG(I,3)
        MSS(8,I)=ZERO
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + SIX*MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + MASS(I)*
     .       (X(1,I1)+X(1,I2)+X(1,I3)+X(1,I4)
     .       +X(1,I5)+X(1,I6))
        PARTSAV(3,IP)=PARTSAV(3,IP) + MASS(I)*
     .       (X(2,I1)+X(2,I2)+X(2,I3)+X(2,I4)
     .       +X(2,I5)+X(2,I6))
        PARTSAV(4,IP)=PARTSAV(4,IP) + MASS(I)*
     .       (X(3,I1)+X(3,I2)+X(3,I3)+X(3,I4)
     .       +X(3,I5)+X(3,I6))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2)
     .       +X(1,I3)*X(1,I3)+X(1,I4)*X(1,I4)
     .       +X(1,I5)*X(1,I5)+X(1,I6)*X(1,I6)
     .       )
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2)
     .       +X(1,I3)*X(2,I3)+X(1,I4)*X(2,I4)
     .       +X(1,I5)*X(2,I5)+X(1,I6)*X(2,I6)
     .       )
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .       +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4)
     .       +X(2,I5)*X(2,I5)+X(2,I6)*X(2,I6)
     .       )
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .       +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4)
     .       +X(2,I5)*X(3,I5)+X(2,I6)*X(3,I6)
     .       )
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .       +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4)
     .       +X(3,I5)*X(3,I5)+X(3,I6)*X(3,I6)
     .       )
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2)
     .       +X(3,I3)*X(1,I3)+X(3,I4)*X(1,I4)
     .       +X(3,I5)*X(1,I5)+X(3,I6)*X(1,I6)
     .       )
        PARTSAV(5,IP) =PARTSAV(5,IP)  + MASS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + MASS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + MASS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - MASS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - MASS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - MASS(I) * ZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + MASS(I)*
     .       (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4)
     .       +V(1,I5)+V(1,I6))
        PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*
     .       (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4)
     .       +V(2,I5)+V(2,I6))
        PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*
     .       (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4)
     .       +V(3,I5)+V(3,I6))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .     +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .     +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4)
     .     +V(1,I5)*V(1,I5)+V(2,I5)*V(2,I5)+V(3,I5)*V(3,I5)
     .     +V(1,I6)*V(1,I6)+V(2,I6)*V(2,I6)+V(3,I6)*V(3,I6)
     .     )
       ENDDO
C
       IF(IREST_MSELT /= 0)THEN
        DO I=LFT,LLT
         MSSA(I)=MASS(I)
        ENDDO
       ENDIF
C
C  --- for FEM solid heat transfer
C
        IF(JTHE < 0 ) THEN
          DO I=LFT,LLT
           MASSP=FILL(I)*RHOCP(I)*VOLU(I)*ONE_OVER_6
           I1 = NC1(I)
           I2 = NC2(I)
           I3 = NC3(I)
           I4 = NC4(I)
           I5 = NC5(I)
           I6 = NC6(I)
           MCPS(1,I)=MASSP*PTG(I,1)
           MCPS(2,I)=MASSP*PTG(I,2)
           MCPS(3,I)=MASSP*PTG(I,3)
           MCPS(4,I)=ZERO
           MCPS(5,I)=MASSP*PTG(I,1)
           MCPS(6,I)=MASSP*PTG(I,2)
           MCPS(7,I)=MASSP*PTG(I,3)
           MCPS(8,I)=ZERO
          ENDDO
        ENDIF   
C
      RETURN
      END
