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
Chd|  S16MASS3                      source/elements/thickshell/solide16/s16mass3.F
Chd|-- called by -----------
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S16MASS3(
     1   MASS  ,MS  ,PARTSAV,IPART  ,MSS,VOLG,
     2   XX    ,YY  ,ZZ     ,VX     ,VY ,VZ  ,  
     3   NC    ,STI ,STIFN  ,DELTAX2,RHO,DTX ,
     4   DTELEM,MSSX,RHOCP  ,MCP     ,MCPS, MCPSX,
     5   FILL  )
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
      INTEGER IPART(*), NC(MVSIZ,16)
      my_real
     .   MASS(*), MS(*),PARTSAV(20,*), MSS(8,*),DELTAX2(*),
     .   XX(MVSIZ,16), YY(MVSIZ,16), ZZ(MVSIZ,16),
     .   VX(MVSIZ,16), VY(MVSIZ,16), VZ(MVSIZ,16),STI(*),STIFN(*), 
     .   VOLG(MVSIZ),RHO(MVSIZ),DTX(MVSIZ),DTELEM(MVSIZ),
     .   MSSX(12,*), RHOCP(*), MCP(*), MCPS(8,*),MCPSX(12,*),
     .   FILL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,N1,N2, IPERM1(16),IPERM2(16),N
C     REAL
      my_real
     .   AXX,AYY,AZZ,AXY,AYZ,AZX,AM,BM,FAC,MASSCP
C
      DATA IPERM1/0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8/
      DATA IPERM2/0,0,0,0,0,0,0,0,2,3,4,1,6,7,8,5/
C-----------------------------------------------------------------------
      DO I=LFT,LLT
C
        MASS(I)=FILL(I)*RHO(I)*VOLG(I)
        DTELEM(I)=MIN(DTELEM(I),DTX(I))
        STI(I) = FILL(I) * RHO(I) * VOLG(I) * TWO / SIXTY4 /
     .        MAX(EM20,DTX(I)*DTX(I))
        AM = MASS(I)/THIRTY2
        BM = MASS(I)*THREE/THIRTY2
        MSS(1,I)=AM
        MSS(2,I)=AM
        MSS(3,I)=AM
        MSS(4,I)=AM
        MSS(5,I)=AM
        MSS(6,I)=AM
        MSS(7,I)=AM
        MSS(8,I)=AM
        STIFN(NC(I,1))=STIFN(NC(I,1))+STI(I)*DELTAX2(I)
        STIFN(NC(I,2))=STIFN(NC(I,2))+STI(I)*DELTAX2(I)
        STIFN(NC(I,3))=STIFN(NC(I,3))+STI(I)*DELTAX2(I)
        STIFN(NC(I,4))=STIFN(NC(I,4))+STI(I)*DELTAX2(I)
        STIFN(NC(I,5))=STIFN(NC(I,5))+STI(I)*DELTAX2(I)
        STIFN(NC(I,6))=STIFN(NC(I,6))+STI(I)*DELTAX2(I)
        STIFN(NC(I,7))=STIFN(NC(I,7))+STI(I)*DELTAX2(I)
        STIFN(NC(I,8))=STIFN(NC(I,8))+STI(I)*DELTAX2(I)
        DO N=9,16
          N1=IPERM1(N)
          N2=IPERM2(N)
          IF(NC(I,N)/=0)THEN
            MSSX(N-8,I)=BM
            STIFN(NC(I,N))=STIFN(NC(I,N))+STI(I)
          ELSE
            MSS(N1,I)=MSS(N1,I)+ HALF*BM
            MSS(N2,I)=MSS(N2,I)+ HALF*BM
            STIFN(NC(I,N1))=STIFN(NC(I,N1))+ HALF*STI(I)
            STIFN(NC(I,N2))=STIFN(NC(I,N2))+ HALF*STI(I)
          ENDIF
        ENDDO
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP)
     .    + AM*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4)
     .         +XX(I,5)+XX(I,6)+XX(I,7)+XX(I,8))
     .    + BM*(XX(I,9) +XX(I,10)+XX(I,11)+XX(I,12)+XX(I,13)+XX(I,14)
     .         +XX(I,15)+XX(I,16))
        PARTSAV(3,IP)=PARTSAV(3,IP)
     .    + AM*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4)
     .         +YY(I,5)+YY(I,6)+YY(I,7)+YY(I,8))
     .    + BM*(YY(I,9) +YY(I,10)+YY(I,11)+YY(I,12)+YY(I,13)+YY(I,14)
     .         +YY(I,15)+YY(I,16))
        PARTSAV(4,IP)=PARTSAV(4,IP)
     .    + AM*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4)
     .         +ZZ(I,5)+ZZ(I,6)+ZZ(I,7)+ZZ(I,8))
     .    + BM*(ZZ(I,9) +ZZ(I,10)+ZZ(I,11)+ZZ(I,12)+ZZ(I,13)+ZZ(I,14)
     .         +ZZ(I,15)+ZZ(I,16))
        AXX = AM*(XX(I,1)*XX(I,1)+XX(I,2)*XX(I,2)
     .           +XX(I,3)*XX(I,3)+XX(I,4)*XX(I,4)
     .           +XX(I,5)*XX(I,5)+XX(I,6)*XX(I,6)
     .           +XX(I,7)*XX(I,7)+XX(I,8)*XX(I,8))
     .       +BM*(XX(I,9) *XX(I,9) +XX(I,10)*XX(I,10)
     .           +XX(I,11)*XX(I,11)+XX(I,12)*XX(I,12)
     .           +XX(I,13)*XX(I,13)+XX(I,14)*XX(I,14)
     .           +XX(I,15)*XX(I,15)+XX(I,16)*XX(I,16))
        AYY = AM*(YY(I,1)*YY(I,1)+YY(I,2)*YY(I,2)
     .           +YY(I,3)*YY(I,3)+YY(I,4)*YY(I,4)
     .           +YY(I,5)*YY(I,5)+YY(I,6)*YY(I,6)
     .           +YY(I,7)*YY(I,7)+YY(I,8)*YY(I,8))
     .       +BM*(YY(I,9) *YY(I,9) +YY(I,10)*YY(I,10)
     .           +YY(I,11)*YY(I,11)+YY(I,12)*YY(I,12)
     .           +YY(I,13)*YY(I,13)+YY(I,14)*YY(I,14)
     .           +YY(I,15)*YY(I,15)+YY(I,16)*YY(I,16))
        AZZ = AM*(ZZ(I,1)*ZZ(I,1)+ZZ(I,2)*ZZ(I,2)
     .           +ZZ(I,3)*ZZ(I,3)+ZZ(I,4)*ZZ(I,4)
     .           +ZZ(I,5)*ZZ(I,5)+ZZ(I,6)*ZZ(I,6)
     .           +ZZ(I,7)*ZZ(I,7)+ZZ(I,8)*ZZ(I,8))
     .       +BM*(ZZ(I,9) *ZZ(I,9) +ZZ(I,10)*ZZ(I,10)
     .           +ZZ(I,11)*ZZ(I,11)+ZZ(I,12)*ZZ(I,12)
     .           +ZZ(I,13)*ZZ(I,13)+ZZ(I,14)*ZZ(I,14)
     .           +ZZ(I,15)*ZZ(I,15)+ZZ(I,16)*ZZ(I,16))
        AXY = AM*(XX(I,1)*YY(I,1)+XX(I,2)*YY(I,2)
     .           +XX(I,3)*YY(I,3)+XX(I,4)*YY(I,4)
     .           +XX(I,5)*YY(I,5)+XX(I,6)*YY(I,6)
     .           +XX(I,7)*YY(I,7)+XX(I,8)*YY(I,8))
     .       +BM*(XX(I,9) *YY(I,9) +XX(I,10)*YY(I,10)
     .           +XX(I,11)*YY(I,11)+XX(I,12)*YY(I,12)
     .           +XX(I,13)*YY(I,13)+XX(I,14)*YY(I,14)
     .           +XX(I,15)*YY(I,15)+XX(I,16)*YY(I,16))
        AYZ = AM*(YY(I,1)*ZZ(I,1)+YY(I,2)*ZZ(I,2)
     .           +YY(I,3)*ZZ(I,3)+YY(I,4)*ZZ(I,4)
     .           +YY(I,5)*ZZ(I,5)+YY(I,6)*ZZ(I,6)
     .           +YY(I,7)*ZZ(I,7)+YY(I,8)*ZZ(I,8))
     .       +BM*(YY(I,9) *ZZ(I,9) +YY(I,10)*ZZ(I,10)
     .           +YY(I,11)*ZZ(I,11)+YY(I,12)*ZZ(I,12)
     .           +YY(I,13)*ZZ(I,13)+YY(I,14)*ZZ(I,14)
     .           +YY(I,15)*ZZ(I,15)+YY(I,16)*ZZ(I,16))
        AZX = AM*(ZZ(I,1)*XX(I,1)+ZZ(I,2)*XX(I,2)
     .           +ZZ(I,3)*XX(I,3)+ZZ(I,4)*XX(I,4)
     .           +ZZ(I,5)*XX(I,5)+ZZ(I,6)*XX(I,6)
     .           +ZZ(I,7)*XX(I,7)+ZZ(I,8)*XX(I,8))
     .       +BM*(ZZ(I,9) *XX(I,9) +ZZ(I,10)*XX(I,10)
     .           +ZZ(I,11)*XX(I,11)+ZZ(I,12)*XX(I,12)
     .           +ZZ(I,13)*XX(I,13)+ZZ(I,14)*XX(I,14)
     .           +ZZ(I,15)*XX(I,15)+ZZ(I,16)*XX(I,16))
        PARTSAV(5,IP) =PARTSAV(5,IP)  + (AYY+AZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + (AZZ+AXX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + (AXX+AYY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - AXY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - AYZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - AZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP)
     .    + AM*(VX(I,1)+VX(I,2)+VX(I,3)+VX(I,4)
     .         +VX(I,5)+VX(I,6)+VX(I,7)+VX(I,8))
     .    + BM*(VX(I,9) +VX(I,10)+VX(I,11)+VX(I,12)+VX(I,13)+VX(I,14)
     .         +VX(I,15)+VX(I,16))
        PARTSAV(12,IP)=PARTSAV(12,IP)
     .    + AM*(VY(I,1)+VY(I,2)+VY(I,3)+VY(I,4)
     .         +VY(I,5)+VY(I,6)+VY(I,7)+VY(I,8))
     .    + BM*(VY(I,9) +VY(I,10)+VY(I,11)+VY(I,12)+VY(I,13)+VY(I,14)
     .         +VY(I,15)+VY(I,16))
        PARTSAV(13,IP)=PARTSAV(13,IP)
     .    + AM*(VZ(I,1)+VZ(I,2)+VZ(I,3)+VZ(I,4)
     .         +VZ(I,5)+VZ(I,6)+VZ(I,7)+VZ(I,8))
     .    + BM*(VZ(I,9) +VZ(I,10)+VZ(I,11)+VZ(I,12)+VZ(I,13)+VZ(I,14)
     .         +VZ(I,15)+VZ(I,16))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * 
     .       (AM*(VX(I,1)*VX(I,1)+VX(I,2)*VX(I,2)
     .           +VX(I,3)*VX(I,3)+VX(I,4)*VX(I,4)
     .           +VX(I,5)*VX(I,5)+VX(I,6)*VX(I,6)
     .           +VX(I,7)*VX(I,7)+VX(I,8)*VX(I,8)
     .           +VY(I,1)*VY(I,1)+VY(I,2)*VY(I,2)
     .           +VY(I,3)*VY(I,3)+VY(I,4)*VY(I,4)
     .           +VY(I,5)*VY(I,5)+VY(I,6)*VY(I,6)
     .           +VY(I,7)*VY(I,7)+VY(I,8)*VY(I,8)
     .           +VZ(I,1)*VZ(I,1)+VZ(I,2)*VZ(I,2)
     .           +VZ(I,3)*VZ(I,3)+VZ(I,4)*VZ(I,4)
     .           +VZ(I,5)*VZ(I,5)+VZ(I,6)*VZ(I,6)
     .           +VZ(I,7)*VZ(I,7)+VZ(I,8)*VZ(I,8))
     .       +BM*(VX(I,9) *VX(I,9) +VX(I,10)*VX(I,10)
     .           +VX(I,11)*VX(I,11)+VX(I,12)*VX(I,12)
     .           +VX(I,13)*VX(I,13)+VX(I,14)*VX(I,14)
     .           +VX(I,15)*VX(I,15)+VX(I,16)*VX(I,16)
     .           +VY(I,9) *VY(I,9) +VY(I,10)*VY(I,10)
     .           +VY(I,11)*VY(I,11)+VY(I,12)*VY(I,12)
     .           +VY(I,13)*VY(I,13)+VY(I,14)*VY(I,14)
     .           +VY(I,15)*VY(I,15)+VY(I,16)*VY(I,16)
     .           +VZ(I,9) *VZ(I,9) +VZ(I,10)*VZ(I,10)
     .           +VZ(I,11)*VZ(I,11)+VZ(I,12)*VZ(I,12)
     .           +VZ(I,13)*VZ(I,13)+VZ(I,14)*VZ(I,14)
     .           +VZ(I,15)*VZ(I,15)+VZ(I,16)*VZ(I,16)))
       ENDDO  
CC
C  heat transfer for solid
C      
       IF(JTHE < 0 ) THEN
        DO I=LFT,LLT
            MASSCP=FILL(I)*RHOCP(I)*VOLG(I)
            AM = MASSCP/THIRTY2
            BM = MASSCP*THREE/THIRTY2
            MSS(1,I)=AM
            MSS(2,I)=AM
            MSS(3,I)=AM
            MSS(4,I)=AM
            MSS(5,I)=AM
            MSS(6,I)=AM
            MSS(7,I)=AM
            MSS(8,I)=AM
            DO N=9,16
                N1=IPERM1(N)
                N2=IPERM2(N)
                IF(NC(I,N) /= 0)THEN
                  MCPSX(N-8,I)=BM
                ELSE
                  MCPS(N1,I)=MCPS(N1,I)+ HALF*BM
                  MCPS(N2,I)=MCPS(N2,I)+ HALF*BM
                ENDIF
            ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
