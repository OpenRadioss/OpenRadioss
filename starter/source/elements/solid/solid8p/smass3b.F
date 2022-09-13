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
Chd|  SMASS3B                       source/elements/solid/solid8p/smass3b.F
Chd|-- called by -----------
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMASS3B(
     1   RHO    ,MS ,VOLGP,LVLOC,MSS,
     2   PARTSAV,X  ,V    ,IPART,MSNF,
     3   MSSF   ,WMA,RHOCP,MCP   ,MCPS ,
     4   MSSA, VOLU, 
     5   NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8)
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
#include      "com01_c.inc"
#include      "com07_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "ale_param.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LVLOC,IPART(*)
      my_real
     .     RHO(*), MS(*), VOLGP(LVLOC,*), X(3,*), V(3,*), PARTSAV(20,*),    
     .     MSNF(*), MSS(8,*), MSSF(8,*),WMA(*),RHOCP(*),MCP(*),MCPS(8,*),
     .     MSSA(*), VOLU(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*), NC8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,IP, I,J,K
      my_real
     .   MASS(MVSIZ), DMASGP(8,MVSIZ),
     .   XX,XY,YY,YZ,ZZ,ZX,DMCPGP(8,MVSIZ)
C=======================================================================
      DO I=LFT,LLT
        MASS(I)=RHO(I)*VOLU(I)*ONE_OVER_8
      ENDDO
C
      IF(IREST_MSELT /= 0)THEN
       DO I=LFT,LLT
        MSSA(NFT+I)=MASS(I)
       ENDDO
      ENDIF
C
      
      DO I=LFT,LLT
        DMASGP(1,I)=RHO(I)*VOLGP(1,I)
        DMASGP(2,I)=RHO(I)*VOLGP(2,I)
        DMASGP(3,I)=RHO(I)*VOLGP(3,I)
        DMASGP(4,I)=RHO(I)*VOLGP(4,I)
        DMASGP(5,I)=RHO(I)*VOLGP(5,I)
        DMASGP(6,I)=RHO(I)*VOLGP(6,I)
        DMASGP(7,I)=RHO(I)*VOLGP(7,I)
        DMASGP(8,I)=RHO(I)*VOLGP(8,I)
      ENDDO
      IF(JTHE < 0 ) THEN
       DO I=LFT,LLT
        DMCPGP(1,I)=RHOCP(I)*VOLGP(1,I)
        DMCPGP(2,I)=RHOCP(I)*VOLGP(2,I)
        DMCPGP(3,I)=RHOCP(I)*VOLGP(3,I)
        DMCPGP(4,I)=RHOCP(I)*VOLGP(4,I)
        DMCPGP(5,I)=RHOCP(I)*VOLGP(5,I)
        DMCPGP(6,I)=RHOCP(I)*VOLGP(6,I)
        DMCPGP(7,I)=RHOCP(I)*VOLGP(7,I)
        DMCPGP(8,I)=RHOCP(I)*VOLGP(8,I)
       ENDDO  
      ENDIF 
C     mass init en parith/on si spmd
      IF (IMACH/=3) THEN
       DO I=LFT,LLT
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        I7 = NC7(I)
        I8 = NC8(I)
        MS(I1)=MS(I1)+DMASGP(1,I)
        MS(I2)=MS(I2)+DMASGP(2,I)
        MS(I3)=MS(I3)+DMASGP(3,I)
        MS(I4)=MS(I4)+DMASGP(4,I)
        MS(I5)=MS(I5)+DMASGP(5,I)
        MS(I6)=MS(I6)+DMASGP(6,I)
        MS(I7)=MS(I7)+DMASGP(7,I)
        MS(I8)=MS(I8)+DMASGP(8,I)
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + EIGHT*MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP)
     .     +DMASGP(1,I)*X(1,I1)+DMASGP(2,I)*X(1,I2)+DMASGP(3,I)*X(1,I3)
     .     +DMASGP(4,I)*X(1,I4)+DMASGP(5,I)*X(1,I5)+DMASGP(6,I)*X(1,I6)
     .     +DMASGP(7,I)*X(1,I7)+DMASGP(8,I)*X(1,I8)
        PARTSAV(3,IP)=PARTSAV(3,IP)
     .     +DMASGP(1,I)*X(2,I1)+DMASGP(2,I)*X(2,I2)+DMASGP(3,I)*X(2,I3)
     .     +DMASGP(4,I)*X(2,I4)+DMASGP(5,I)*X(2,I5)+DMASGP(6,I)*X(2,I6)
     .     +DMASGP(7,I)*X(2,I7)+DMASGP(8,I)*X(2,I8)
        PARTSAV(4,IP)=PARTSAV(4,IP)
     .     +DMASGP(1,I)*X(3,I1)+DMASGP(2,I)*X(3,I2)+DMASGP(3,I)*X(3,I3)
     .     +DMASGP(4,I)*X(3,I4)+DMASGP(5,I)*X(3,I5)+DMASGP(6,I)*X(3,I6)
     .     +DMASGP(7,I)*X(3,I7)+DMASGP(8,I)*X(3,I8)
        XX = X(1,I1)*X(1,I1)
        XY = X(1,I1)*X(2,I1)
        YY = X(2,I1)*X(2,I1)
        YZ = X(2,I1)*X(3,I1)
        ZZ = X(3,I1)*X(3,I1)
        ZX = X(3,I1)*X(1,I1)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(1,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(1,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(1,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(1,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(1,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(1,I) * ZX
        XX = X(1,I2)*X(1,I2)
        XY = X(1,I2)*X(2,I2)
        YY = X(2,I2)*X(2,I2)
        YZ = X(2,I2)*X(3,I2)
        ZZ = X(3,I2)*X(3,I2)
        ZX = X(3,I2)*X(1,I2)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(2,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(2,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(2,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(2,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(2,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(2,I) * ZX
        XX = X(1,I3)*X(1,I3)
        XY = X(1,I3)*X(2,I3)
        YY = X(2,I3)*X(2,I3)
        YZ = X(2,I3)*X(3,I3)
        ZZ = X(3,I3)*X(3,I3)
        ZX = X(3,I3)*X(1,I3)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(3,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(3,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(3,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(3,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(3,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(3,I) * ZX
        XX = X(1,I4)*X(1,I4)
        XY = X(1,I4)*X(2,I4)
        YY = X(2,I4)*X(2,I4)
        YZ = X(2,I4)*X(3,I4)
        ZZ = X(3,I4)*X(3,I4)
        ZX = X(3,I4)*X(1,I4)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(4,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(4,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(4,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(4,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(4,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(4,I) * ZX
        XX = X(1,I5)*X(1,I5)
        XY = X(1,I5)*X(2,I5)
        YY = X(2,I5)*X(2,I5)
        YZ = X(2,I5)*X(3,I5)
        ZZ = X(3,I5)*X(3,I5)
        ZX = X(3,I5)*X(1,I5)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(5,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(5,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(5,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(5,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(5,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(5,I) * ZX
        XX = X(1,I6)*X(1,I6)
        XY = X(1,I6)*X(2,I6)
        YY = X(2,I6)*X(2,I6)
        YZ = X(2,I6)*X(3,I6)
        ZZ = X(3,I6)*X(3,I6)
        ZX = X(3,I6)*X(1,I6)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(6,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(6,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(6,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(6,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(6,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(6,I) * ZX
        XX = X(1,I7)*X(1,I7)
        XY = X(1,I7)*X(2,I7)
        YY = X(2,I7)*X(2,I7)
        YZ = X(2,I7)*X(3,I7)
        ZZ = X(3,I7)*X(3,I7)
        ZX = X(3,I7)*X(1,I7)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(7,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(7,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(7,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(7,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(7,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(7,I) * ZX
        XX = X(1,I8)*X(1,I8)
        XY = X(1,I8)*X(2,I8)
        YY = X(2,I8)*X(2,I8)
        YZ = X(2,I8)*X(3,I8)
        ZZ = X(3,I8)*X(3,I8)
        ZX = X(3,I8)*X(1,I8)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(8,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(8,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(8,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(8,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(8,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(8,I) * ZX
C   les quantites suivantes sont calculees en tenant compte
C   de M_element/8. et non pas des masses associees aux noeuds.
        PARTSAV(11,IP)=PARTSAV(11,IP) + MASS(I)*
     .       (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4)
     .       +V(1,I5)+V(1,I6)+V(1,I7)+V(1,I8))
        PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*
     .       (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4)
     .       +V(2,I5)+V(2,I6)+V(2,I7)+V(2,I8))
        PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*
     .       (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4)
     .       +V(3,I5)+V(3,I6)+V(3,I7)+V(3,I8))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .     +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .     +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4)
     .     +V(1,I5)*V(1,I5)+V(2,I5)*V(2,I5)+V(3,I5)*V(3,I5)
     .     +V(1,I6)*V(1,I6)+V(2,I6)*V(2,I6)+V(3,I6)*V(3,I6)
     .     +V(1,I7)*V(1,I7)+V(2,I7)*V(2,I7)+V(3,I7)*V(3,I7)
     .     +V(1,I8)*V(1,I8)+V(2,I8)*V(2,I8)+V(3,I8)*V(3,I8))
       ENDDO

C
c   For FEM  heat transfer
c 
      IF(JTHE < 0 ) THEN
       DO I=LFT,LLT
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        I7 = NC7(I)
        I8 = NC8(I)
        MCP(I1)=MCP(I1)+DMCPGP(1,I)
        MCP(I2)=MCP(I2)+DMCPGP(2,I)
        MCP(I3)=MCP(I3)+DMCPGP(3,I)
        MCP(I4)=MCP(I4)+DMCPGP(4,I)
        MCP(I5)=MCP(I5)+DMCPGP(5,I)
        MCP(I6)=MCP(I6)+DMCPGP(6,I)
        MCP(I7)=MCP(I7)+DMCPGP(7,I)
        MCP(I8)=MCP(I8)+DMCPGP(8,I) 
       ENDDO
      ENDIF
C
      IF(JALE+JEUL>0)THEN
       DO I=LFT,LLT
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        I7 = NC7(I)
        I8 = NC8(I)
        MSNF(I1)=MSNF(I1)+DMASGP(1,I)
        MSNF(I2)=MSNF(I2)+DMASGP(2,I)
        MSNF(I3)=MSNF(I3)+DMASGP(3,I)
        MSNF(I4)=MSNF(I4)+DMASGP(4,I)
        MSNF(I5)=MSNF(I5)+DMASGP(5,I)
        MSNF(I6)=MSNF(I6)+DMASGP(6,I)
        MSNF(I7)=MSNF(I7)+DMASGP(7,I)
        MSNF(I8)=MSNF(I8)+DMASGP(8,I)
       ENDDO
      ENDIF
C
      ELSE
       DO I=LFT,LLT
        MSS(1,I)=DMASGP(1,I)
        MSS(2,I)=DMASGP(2,I)
        MSS(3,I)=DMASGP(3,I)
        MSS(4,I)=DMASGP(4,I)
        MSS(5,I)=DMASGP(5,I)
        MSS(6,I)=DMASGP(6,I)
        MSS(7,I)=DMASGP(7,I)
        MSS(8,I)=DMASGP(8,I)
C
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        I7 = NC7(I)
        I8 = NC8(I)
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + EIGHT*MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP)
     .     +DMASGP(1,I)*X(1,I1)+DMASGP(2,I)*X(1,I2)+DMASGP(3,I)*X(1,I3)
     .     +DMASGP(4,I)*X(1,I4)+DMASGP(5,I)*X(1,I5)+DMASGP(6,I)*X(1,I6)
     .     +DMASGP(7,I)*X(1,I7)+DMASGP(8,I)*X(1,I8)
        PARTSAV(3,IP)=PARTSAV(3,IP)
     .     +DMASGP(1,I)*X(2,I1)+DMASGP(2,I)*X(2,I2)+DMASGP(3,I)*X(2,I3)
     .     +DMASGP(4,I)*X(2,I4)+DMASGP(5,I)*X(2,I5)+DMASGP(6,I)*X(2,I6)
     .     +DMASGP(7,I)*X(2,I7)+DMASGP(8,I)*X(2,I8)
        PARTSAV(4,IP)=PARTSAV(4,IP)
     .     +DMASGP(1,I)*X(3,I1)+DMASGP(2,I)*X(3,I2)+DMASGP(3,I)*X(3,I3)
     .     +DMASGP(4,I)*X(3,I4)+DMASGP(5,I)*X(3,I5)+DMASGP(6,I)*X(3,I6)
     .     +DMASGP(7,I)*X(3,I7)+DMASGP(8,I)*X(3,I8)
        XX = X(1,I1)*X(1,I1)
        XY = X(1,I1)*X(2,I1)
        YY = X(2,I1)*X(2,I1)
        YZ = X(2,I1)*X(3,I1)
        ZZ = X(3,I1)*X(3,I1)
        ZX = X(3,I1)*X(1,I1)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(1,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(1,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(1,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(1,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(1,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(1,I) * ZX
        XX = X(1,I2)*X(1,I2)
        XY = X(1,I2)*X(2,I2)
        YY = X(2,I2)*X(2,I2)
        YZ = X(2,I2)*X(3,I2)
        ZZ = X(3,I2)*X(3,I2)
        ZX = X(3,I2)*X(1,I2)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(2,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(2,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(2,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(2,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(2,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(2,I) * ZX
        XX = X(1,I3)*X(1,I3)
        XY = X(1,I3)*X(2,I3)
        YY = X(2,I3)*X(2,I3)
        YZ = X(2,I3)*X(3,I3)
        ZZ = X(3,I3)*X(3,I3)
        ZX = X(3,I3)*X(1,I3)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(3,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(3,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(3,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(3,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(3,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(3,I) * ZX
        XX = X(1,I4)*X(1,I4)
        XY = X(1,I4)*X(2,I4)
        YY = X(2,I4)*X(2,I4)
        YZ = X(2,I4)*X(3,I4)
        ZZ = X(3,I4)*X(3,I4)
        ZX = X(3,I4)*X(1,I4)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(4,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(4,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(4,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(4,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(4,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(4,I) * ZX
        XX = X(1,I5)*X(1,I5)
        XY = X(1,I5)*X(2,I5)
        YY = X(2,I5)*X(2,I5)
        YZ = X(2,I5)*X(3,I5)
        ZZ = X(3,I5)*X(3,I5)
        ZX = X(3,I5)*X(1,I5)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(5,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(5,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(5,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(5,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(5,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(5,I) * ZX
        XX = X(1,I6)*X(1,I6)
        XY = X(1,I6)*X(2,I6)
        YY = X(2,I6)*X(2,I6)
        YZ = X(2,I6)*X(3,I6)
        ZZ = X(3,I6)*X(3,I6)
        ZX = X(3,I6)*X(1,I6)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(6,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(6,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(6,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(6,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(6,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(6,I) * ZX
        XX = X(1,I7)*X(1,I7)
        XY = X(1,I7)*X(2,I7)
        YY = X(2,I7)*X(2,I7)
        YZ = X(2,I7)*X(3,I7)
        ZZ = X(3,I7)*X(3,I7)
        ZX = X(3,I7)*X(1,I7)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(7,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(7,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(7,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(7,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(7,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(7,I) * ZX
        XX = X(1,I8)*X(1,I8)
        XY = X(1,I8)*X(2,I8)
        YY = X(2,I8)*X(2,I8)
        YZ = X(2,I8)*X(3,I8)
        ZZ = X(3,I8)*X(3,I8)
        ZX = X(3,I8)*X(1,I8)
        PARTSAV(5,IP) =PARTSAV(5,IP)  + DMASGP(8,I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + DMASGP(8,I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + DMASGP(8,I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - DMASGP(8,I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - DMASGP(8,I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - DMASGP(8,I) * ZX
C   les quantites suivantes sont calculees en tenant compte
C   de M_element/8. et non pas des masses associees aux noeuds.
        PARTSAV(11,IP)=PARTSAV(11,IP) + MASS(I)*
     .       (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4)
     .       +V(1,I5)+V(1,I6)+V(1,I7)+V(1,I8))
        PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*
     .       (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4)
     .       +V(2,I5)+V(2,I6)+V(2,I7)+V(2,I8))
        PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*
     .       (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4)
     .       +V(3,I5)+V(3,I6)+V(3,I7)+V(3,I8))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * MASS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .     +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .     +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4)
     .     +V(1,I5)*V(1,I5)+V(2,I5)*V(2,I5)+V(3,I5)*V(3,I5)
     .     +V(1,I6)*V(1,I6)+V(2,I6)*V(2,I6)+V(3,I6)*V(3,I6)
     .     +V(1,I7)*V(1,I7)+V(2,I7)*V(2,I7)+V(3,I7)*V(3,I7)
     .     +V(1,I8)*V(1,I8)+V(2,I8)*V(2,I8)+V(3,I8)*V(3,I8))
       ENDDO
C
      IF(JALE+JEUL>0)THEN
       DO I=LFT,LLT
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        I7 = NC7(I)
        I8 = NC8(I)
        MSS(1,I)=DMASGP(1,I)
        MSS(2,I)=DMASGP(2,I)
        MSS(3,I)=DMASGP(3,I)
        MSS(4,I)=DMASGP(4,I)
        MSS(5,I)=DMASGP(5,I)
        MSS(6,I)=DMASGP(6,I)
        MSS(7,I)=DMASGP(7,I)
        MSS(8,I)=DMASGP(8,I)
       ENDDO
      ENDIF
C
C  For FEM heat trasnfert
C
       IF(JTHE < 0 ) THEN  
         DO I=LFT,LLT
          MCPS(1,I)=DMCPGP(1,I)
          MCPS(2,I)=DMCPGP(2,I)
          MCPS(3,I)=DMCPGP(3,I)
          MCPS(4,I)=DMCPGP(4,I)
          MCPS(5,I)=DMCPGP(5,I)
          MCPS(6,I)=DMCPGP(6,I)
          MCPS(7,I)=DMCPGP(7,I)
          MCPS(8,I)=DMCPGP(8,I)
         ENDDO
       ENDIF
      ENDIF
C
      IF(JALE>0 .AND. NWALE==4)THEN
        DO I=LFT,LLT
          I1 = NC1(I)
          I2 = NC2(I)
          I3 = NC3(I)
          I4 = NC4(I)
          I5 = NC5(I)
          I6 = NC6(I)
          I7 = NC7(I)
          I8 = NC8(I)
          WMA(I1)=WMA(I1)+THREE_HALF
          WMA(I2)=WMA(I2)+THREE_HALF
          WMA(I3)=WMA(I3)+THREE_HALF
          WMA(I4)=WMA(I4)+THREE_HALF
          WMA(I5)=WMA(I5)+THREE_HALF
          WMA(I6)=WMA(I6)+THREE_HALF
          WMA(I7)=WMA(I7)+THREE_HALF
          WMA(I8)=WMA(I8)+THREE_HALF
        ENDDO
      ENDIF
C-----------     
      RETURN
      END
