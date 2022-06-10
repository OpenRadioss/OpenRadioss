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
Chd|  S20MASS3                      source/elements/solid/solide20/s20mass3.F
Chd|-- called by -----------
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20MASS3(
     1   MASS  ,MS  ,PARTSAV,IPART  ,MSS    ,VOLG,
     2   XX    ,YY  ,ZZ     ,VX     ,VY     ,VZ,  
     3   NC    ,STI ,STIFN  ,DELTAX2,RHO    ,DTX,
     4   DTELEM,MSSX,RHOCP  ,MCP     ,MCPS  ,MCPSX,
     5   FILL  )
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
      INTEGER IPART(*), NC(MVSIZ,20)
      my_real
     .   MASS(*), MS(*),PARTSAV(20,*), MSS(8,*),DELTAX2(*),
     .   XX(MVSIZ,20), YY(MVSIZ,20), ZZ(MVSIZ,20),
     .   VX(MVSIZ,20), VY(MVSIZ,20), VZ(MVSIZ,20),STI(*),STIFN(*), 
     .   VOLG(MVSIZ),RHO(MVSIZ),DTX(MVSIZ),DTELEM(MVSIZ),MSSX(12,*),
     .   RHOCP(*), MCP(*), MCPS(8,*),MCPSX(12,*), FILL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,N1,N2, IPERM1(20),IPERM2(20),N
C     REAL
      my_real
     .   AXX,AYY,AZZ,AXY,AYZ,AZX,AM,BM,FAC,MASSCP
C
      DATA IPERM1/0,0,0,0,0,0,0,0,1,2,3,4,1,2,3,4,5,6,7,8/
      DATA IPERM2/0,0,0,0,0,0,0,0,2,3,4,1,5,6,7,8,6,7,8,5/
C-----------------------------------------------------------------------
C     mass init en parith pour spmd
       DO I=LFT,LLT
C
        MASS(I)=FILL(I)*RHO(I)*VOLG(I)
        DTELEM(I)=MIN(DTELEM(I),DTX(I))
        STI(I) = FILL(I) * RHO(I) * VOLG(I) * TWO / SIXTY4 /
     .        MAX(EM20,DTX(I)*DTX(I))
        FAC=TRHEE_OVER_14
        AM = MASS(I)*FAC/(EIGHT*FAC + TWELVE)
        BM = MASS(I)*ONE/(EIGHT*FAC + TWELVE)
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
        DO N=9,20
          N1=IPERM1(N)
          N2=IPERM2(N)
          IF(NC(I,N) /= 0)THEN
            MSSX(N-8,I)=BM
            STIFN(NC(I,N))=STIFN(NC(I,N))+STI(I)
          ELSE
            MSS(N1,I)=MSS(N1,I)+ HALF*BM
            MSS(N2,I)=MSS(N2,I)+ HALF*BM
            STIFN(NC(I,N1))=STIFN(NC(I,N1)) + HALF*STI(I)
            STIFN(NC(I,N2))=STIFN(NC(I,N2)) + HALF*STI(I)
          ENDIF
        ENDDO
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP)
     .    + AM*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4)
     .         +XX(I,5)+XX(I,6)+XX(I,7)+XX(I,8))
     .    + BM*(XX(I,9) +XX(I,10)+XX(I,11)+XX(I,12)+XX(I,13)+XX(I,14)
     .         +XX(I,15)+XX(I,16)+XX(I,17)+XX(I,18)+XX(I,19)+XX(I,20))
        PARTSAV(3,IP)=PARTSAV(3,IP)
     .    + AM*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4)
     .         +YY(I,5)+YY(I,6)+YY(I,7)+YY(I,8))
     .    + BM*(YY(I,9) +YY(I,10)+YY(I,11)+YY(I,12)+YY(I,13)+YY(I,14)
     .         +YY(I,15)+YY(I,16)+YY(I,17)+YY(I,18)+YY(I,19)+YY(I,20))
        PARTSAV(4,IP)=PARTSAV(4,IP)
     .    + AM*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4)
     .         +ZZ(I,5)+ZZ(I,6)+ZZ(I,7)+ZZ(I,8))
     .    + BM*(ZZ(I,9) +ZZ(I,10)+ZZ(I,11)+ZZ(I,12)+ZZ(I,13)+ZZ(I,14)
     .         +ZZ(I,15)+ZZ(I,16)+ZZ(I,17)+ZZ(I,18)+ZZ(I,19)+ZZ(I,20))
        AXX = AM*(XX(I,1)*XX(I,1)+XX(I,2)*XX(I,2)
     .           +XX(I,3)*XX(I,3)+XX(I,4)*XX(I,4)
     .           +XX(I,5)*XX(I,5)+XX(I,6)*XX(I,6)
     .           +XX(I,7)*XX(I,7)+XX(I,8)*XX(I,8))
     .       +BM*(XX(I,9) *XX(I,9) +XX(I,10)*XX(I,10)
     .           +XX(I,11)*XX(I,11)+XX(I,12)*XX(I,12)
     .           +XX(I,13)*XX(I,13)+XX(I,14)*XX(I,14)
     .           +XX(I,15)*XX(I,15)+XX(I,16)*XX(I,16)
     .           +XX(I,17)*XX(I,17)+XX(I,18)*XX(I,18)
     .           +XX(I,19)*XX(I,19)+XX(I,20)*XX(I,20))
        AYY = AM*(YY(I,1)*YY(I,1)+YY(I,2)*YY(I,2)
     .           +YY(I,3)*YY(I,3)+YY(I,4)*YY(I,4)
     .           +YY(I,5)*YY(I,5)+YY(I,6)*YY(I,6)
     .           +YY(I,7)*YY(I,7)+YY(I,8)*YY(I,8))
     .       +BM*(YY(I,9) *YY(I,9) +YY(I,10)*YY(I,10)
     .           +YY(I,11)*YY(I,11)+YY(I,12)*YY(I,12)
     .           +YY(I,13)*YY(I,13)+YY(I,14)*YY(I,14)
     .           +YY(I,15)*YY(I,15)+YY(I,16)*YY(I,16)
     .           +YY(I,17)*YY(I,17)+YY(I,18)*YY(I,18)
     .           +YY(I,19)*YY(I,19)+YY(I,20)*YY(I,20))
        AZZ = AM*(ZZ(I,1)*ZZ(I,1)+ZZ(I,2)*ZZ(I,2)
     .           +ZZ(I,3)*ZZ(I,3)+ZZ(I,4)*ZZ(I,4)
     .           +ZZ(I,5)*ZZ(I,5)+ZZ(I,6)*ZZ(I,6)
     .           +ZZ(I,7)*ZZ(I,7)+ZZ(I,8)*ZZ(I,8))
     .       +BM*(ZZ(I,9) *ZZ(I,9) +ZZ(I,10)*ZZ(I,10)
     .           +ZZ(I,11)*ZZ(I,11)+ZZ(I,12)*ZZ(I,12)
     .           +ZZ(I,13)*ZZ(I,13)+ZZ(I,14)*ZZ(I,14)
     .           +ZZ(I,15)*ZZ(I,15)+ZZ(I,16)*ZZ(I,16)
     .           +ZZ(I,17)*ZZ(I,17)+ZZ(I,18)*ZZ(I,18)
     .           +ZZ(I,19)*ZZ(I,19)+ZZ(I,20)*ZZ(I,20))
        AXY = AM*(XX(I,1)*YY(I,1)+XX(I,2)*YY(I,2)
     .           +XX(I,3)*YY(I,3)+XX(I,4)*YY(I,4)
     .           +XX(I,5)*YY(I,5)+XX(I,6)*YY(I,6)
     .           +XX(I,7)*YY(I,7)+XX(I,8)*YY(I,8))
     .       +BM*(XX(I,9) *YY(I,9) +XX(I,10)*YY(I,10)
     .           +XX(I,11)*YY(I,11)+XX(I,12)*YY(I,12)
     .           +XX(I,13)*YY(I,13)+XX(I,14)*YY(I,14)
     .           +XX(I,15)*YY(I,15)+XX(I,16)*YY(I,16)
     .           +XX(I,17)*YY(I,17)+XX(I,18)*YY(I,18)
     .           +XX(I,19)*YY(I,19)+XX(I,20)*YY(I,20))
        AYZ = AM*(YY(I,1)*ZZ(I,1)+YY(I,2)*ZZ(I,2)
     .           +YY(I,3)*ZZ(I,3)+YY(I,4)*ZZ(I,4)
     .           +YY(I,5)*ZZ(I,5)+YY(I,6)*ZZ(I,6)
     .           +YY(I,7)*ZZ(I,7)+YY(I,8)*ZZ(I,8))
     .       +BM*(YY(I,9) *ZZ(I,9) +YY(I,10)*ZZ(I,10)
     .           +YY(I,11)*ZZ(I,11)+YY(I,12)*ZZ(I,12)
     .           +YY(I,13)*ZZ(I,13)+YY(I,14)*ZZ(I,14)
     .           +YY(I,15)*ZZ(I,15)+YY(I,16)*ZZ(I,16)
     .           +YY(I,17)*ZZ(I,17)+YY(I,18)*ZZ(I,18)
     .           +YY(I,19)*ZZ(I,19)+YY(I,20)*ZZ(I,20))
        AZX = AM*(ZZ(I,1)*XX(I,1)+ZZ(I,2)*XX(I,2)
     .           +ZZ(I,3)*XX(I,3)+ZZ(I,4)*XX(I,4)
     .           +ZZ(I,5)*XX(I,5)+ZZ(I,6)*XX(I,6)
     .           +ZZ(I,7)*XX(I,7)+ZZ(I,8)*XX(I,8))
     .       +BM*(ZZ(I,9) *XX(I,9) +ZZ(I,10)*XX(I,10)
     .           +ZZ(I,11)*XX(I,11)+ZZ(I,12)*XX(I,12)
     .           +ZZ(I,13)*XX(I,13)+ZZ(I,14)*XX(I,14)
     .           +ZZ(I,15)*XX(I,15)+ZZ(I,16)*XX(I,16)
     .           +ZZ(I,17)*XX(I,17)+ZZ(I,18)*XX(I,18)
     .           +ZZ(I,19)*XX(I,19)+ZZ(I,20)*XX(I,20))
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
     .         +VX(I,15)+VX(I,16)+VX(I,17)+VX(I,18)+VX(I,19)+VX(I,20))
        PARTSAV(12,IP)=PARTSAV(12,IP)
     .    + AM*(VY(I,1)+VY(I,2)+VY(I,3)+VY(I,4)
     .         +VY(I,5)+VY(I,6)+VY(I,7)+VY(I,8))
     .    + BM*(VY(I,9) +VY(I,10)+VY(I,11)+VY(I,12)+VY(I,13)+VY(I,14)
     .         +VY(I,15)+VY(I,16)+VY(I,17)+VY(I,18)+VY(I,19)+VY(I,20))
        PARTSAV(13,IP)=PARTSAV(13,IP)
     .    + AM*(VZ(I,1)+VZ(I,2)+VZ(I,3)+VZ(I,4)
     .         +VZ(I,5)+VZ(I,6)+VZ(I,7)+VZ(I,8))
     .    + BM*(VZ(I,9) +VZ(I,10)+VZ(I,11)+VZ(I,12)+VZ(I,13)+VZ(I,14)
     .         +VZ(I,15)+VZ(I,16)+VZ(I,17)+VZ(I,18)+VZ(I,19)+VZ(I,20))
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
     .           +VX(I,17)*VX(I,17)+VX(I,18)*VX(I,18)
     .           +VX(I,19)*VX(I,19)+VX(I,20)*VX(I,20)
     .           +VY(I,9) *VY(I,9) +VY(I,10)*VY(I,10)
     .           +VY(I,11)*VY(I,11)+VY(I,12)*VY(I,12)
     .           +VY(I,13)*VY(I,13)+VY(I,14)*VY(I,14)
     .           +VY(I,15)*VY(I,15)+VY(I,16)*VY(I,16)
     .           +VY(I,17)*VY(I,17)+VY(I,18)*VY(I,18)
     .           +VY(I,19)*VY(I,19)+VY(I,20)*VY(I,20)
     .           +VZ(I,9) *VZ(I,9) +VZ(I,10)*VZ(I,10)
     .           +VZ(I,11)*VZ(I,11)+VZ(I,12)*VZ(I,12)
     .           +VZ(I,13)*VZ(I,13)+VZ(I,14)*VZ(I,14)
     .           +VZ(I,15)*VZ(I,15)+VZ(I,16)*VZ(I,16)
     .           +VZ(I,17)*VZ(I,17)+VZ(I,18)*VZ(I,18)
     .           +VZ(I,19)*VZ(I,19)+VZ(I,20)*VZ(I,20)))
       ENDDO
C
c --- for FEM solide heat trasnfert  
c
       IF(JTHE < 0 ) THEN
        DO I=LFT,LLT
          MASSCP=FILL(I)*RHOCP(I)*VOLG(I)        
          FAC=TRHEE_OVER_14
          AM = MASSCP*FAC/(EIGHT*FAC + TWELVE)
          BM = MASSCP*ONE/(EIGHT*FAC + TWELVE)
          MCPS(1,I)=AM
          MCPS(2,I)=AM
          MCPS(3,I)=AM
          MCPS(4,I)=AM
          MCPS(5,I)=AM
          MCPS(6,I)=AM
          MCPS(7,I)=AM
          MCPS(8,I)=AM
          DO N=9,20
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
C-----------      
      RETURN
      END SUBROUTINE S20MASS3
C

Chd|====================================================================
Chd|  S20MSI                        source/elements/solid/solide20/s20mass3.F
Chd|-- called by -----------
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20MSI(RHO  ,MASS ,VOLU  ,DTELEM ,STI,
     .                  OFF  ,SIG  ,EINT  ,DTX    ,NEL,
     .                  OFFG ,SIGG ,EINTG ,RHOG   ,WIP)
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
      INTEGER NEL
C     REAL
      my_real
     .   RHO(*), MASS(*),VOLU(*),DTELEM(*),STI(*),
     .   SIG(NEL,6),EINT(*),OFF(*),
     .   SIGG(NEL,6),EINTG(*),RHOG(*),OFFG(*),WIP
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DTX(MVSIZ)    
C-----------------------------------------------
      DO I=LFT,LLT
c        MASS(I)=RHO(I)*VOLU(I)
c        DTELEM(I)=MIN(DTELEM(I),DTX(I))
c        STI(I) = RHO(I) * VOLU(I) / 
c     .        MAX(EM20,DTX(I)*DTX(I))
        SIGG(I,1) = SIGG(I,1) + WIP * SIG(I,1)
        SIGG(I,2) = SIGG(I,2) + WIP * SIG(I,2)
        SIGG(I,3) = SIGG(I,3) + WIP * SIG(I,3)
        SIGG(I,4) = SIGG(I,4) + WIP * SIG(I,4)
        SIGG(I,5) = SIGG(I,5) + WIP * SIG(I,5)
        SIGG(I,6) = SIGG(I,6) + WIP * SIG(I,6)
        RHOG(I)   = RHOG(I)   + WIP * RHO(I)
        EINTG(I)  = EINTG(I)  + WIP * EINT(I)
        OFFG(I)   = OFF(I)
      ENDDO
C-----------
      RETURN
      END SUBROUTINE S20MSI
C.....
Chd|====================================================================
Chd|  SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|-- called by -----------
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|        SROTA6_M1                     source/output/anim/srota6_M1.F
Chd|====================================================================
      SUBROUTINE SIGIN20B(
     .           SIG     ,PM      ,VOL     ,SIGSP  ,
     .           SIGI    ,EINT    ,RHO     ,UVAR   ,EPS     ,
     .           IX      ,NIX     ,NSIGI   ,IPT    ,NUVAR   ,
     .           NEL     ,IUSER   ,IDEF    ,NSIGS  ,STRSGLOB,
     .           STRAGLOB,JHBE    ,IGTYP   ,X      ,BUFGAMA ,
     .           MAT     ,EPSP    ,L_PLA   ,PT     ,SIGB    ,
     .           L_SIGB  ,IPM     ,BUFMAT  ,VOLDP  )      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "param_c.inc"
#include       "scry_c.inc" 
#include       "com01_c.inc"
#include       "com04_c.inc"
#include       "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*)
      INTEGER NIX,N,JPS,JHBE,IGTYP,NSIGI,NUVAR,NEL,IUSER,NSIGS,
     .   IDEF,L_PLA,L_SIGB
      INTEGER IX(NIX,*),STRSGLOB(*),STRAGLOB(*),MAT(NEL),PT(*),SIGB(NEL*L_SIGB)
C     REAL
      my_real
     .   SIG(NEL,6)   , EINT(*), RHO(*), SIGSP(NSIGI,*),
     .   PM(NPROPM,*), VOL(*), UVAR(*), SIGI(NSIGS,*),
     .   EPS(NEL,6),X(3,*),BUFGAMA(*),EPSP(*),BUFMAT(*)
      DOUBLE PRECISION
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IIP,JPT, II, JJ, IPT, IPP,IUS,IPSU,JPS1,MA,IFLAGINI,
     .   NVAR_TMP,IADB,NRATE
      my_real
     .   GAMA(6),TENS(6)
C=======================================================================
       DO I=LFT,LLT
         MA = MAT(I)        
         EINT(I)=PM(23,MA)
         RHO(I) =PM(89,MA)
       ENDDO
C      
       IF (ISIGI /= 0) THEN
C         
        JPS=1+ (IPT-1)*9
        JPS1 = NVSOLID1 + (IPT-1)*6      
        DO I=LFT,LLT
          IFLAGINI = 0
          IF (STRAGLOB(I) == 1 .OR. STRSGLOB(I) == 1) THEN
            IF(JCVT==2)THEN
              GAMA(1)=BUFGAMA(I        )
              GAMA(2)=BUFGAMA(I +   NEL)
              GAMA(3)=BUFGAMA(I + 2*NEL)
              GAMA(4)=BUFGAMA(I + 3*NEL)
              GAMA(5)=BUFGAMA(I + 4*NEL)
              GAMA(6)=BUFGAMA(I + 5*NEL)
            ELSE
              GAMA(1)=ONE
              GAMA(2)=ZERO
              GAMA(3)=ZERO
              GAMA(4)=ZERO
              GAMA(5)=ONE
              GAMA(6)=ZERO
            END IF
          ENDIF
C         CONTRAINTES INITIALES

          II=NFT+I
          JJ=PT(II)
          IFLAGINI = 1
          IF(JJ.EQ.0)IFLAGINI = 0
c
            IF (IFLAGINI == 1) THEN   
              IPP = I  
              IF(NVSOLID1 /= 0 )THEN                 
                SIG(I,1)=SIGSP(JPS+1,JJ)
                SIG(I,2)=SIGSP(JPS+2,JJ)
                SIG(I,3)=SIGSP(JPS+3,JJ)
                SIG(I,4)=SIGSP(JPS+4,JJ)
                SIG(I,5)=SIGSP(JPS+5,JJ)
                SIG(I,6)=SIGSP(JPS+6,JJ)
                IF (STRSGLOB(I) == 1) THEN
                  TENS(1)=SIG(I,1)
                  TENS(2)=SIG(I,2)
                  TENS(3)=SIG(I,3)
                  TENS(4)=SIG(I,4)
                  TENS(5)=SIG(I,5)
                  TENS(6)=SIG(I,6)
                  CALL SROTA6_M1(X,IX(1,II),JCVT,TENS,GAMA,JHBE,IGTYP)                      
                  SIG(I,1)=TENS(1)
                  SIG(I,2)=TENS(2)
                  SIG(I,3)=TENS(3)
                  SIG(I,4)=TENS(4)
                  SIG(I,5)=TENS(5)
                  SIG(I,6)=TENS(6)
                ENDIF
                IF(L_PLA /= 0 .AND. SIGSP(JPS+7,JJ) /= ZERO) 
     .                   EPSP(I) = SIGSP(JPS+7,JJ)       
                IF (SIGSP(JPS+8,JJ) /= ZERO) EINT(I)=SIGSP(JPS+8,JJ)
                IF (SIGSP(JPS+9,JJ) /= ZERO) THEN
                  VOLDP(I) = SIGSP(JPS+9,JJ)*VOLDP(I) / RHO(I)
                  VOL(I) = SIGSP(JPS+9,JJ)*VOL(I) / RHO(I)
                  RHO(I) = SIGSP(JPS+9,JJ)
                ENDIF
              ENDIF
c
              IF (MTN >= 28 .AND. IUSER == 1) THEN
                IF (MTN == 36 .and. L_SIGB == 6) THEN                       
                  IADB = IPM(7,MAT(1))               
                  NRATE = NINT(BUFMAT(IADB))
                  NVAR_TMP = SIGSP(NVSOLID1 + NVSOLID2 + 3, JJ)  
                  IPSU = NVSOLID1 + NVSOLID2 + 4 + (IPT - 1)*NVAR_TMP
                  DO IUS = 1,6         
                    IPP = I + (IUS -1)*NEL
                    SIGB(IPP) = SIGSP(IPSU + NRATE + IUS, JJ)                        
                  ENDDO 
                ELSE IF (MTN /= 36) THEN
                  NVAR_TMP = SIGSP(NVSOLID1 + NVSOLID2 + 3, JJ)  
                  IPSU = NVSOLID1 + NVSOLID2 + 4 + (IPT - 1)*NVAR_TMP
                  DO IUS = 1, NVAR_TMP             
                    IPP = I + (IUS -1)*NEL
                    UVAR(IPP) = SIGSP(IPSU + IUS, JJ)          
                  ENDDO 
                  DO IUS = NVAR_TMP + 1, NUVAR             
                    IPP = I + (IUS -1)*NEL
                    UVAR(IPP) = ZERO        
                  ENDDO
                ENDIF
              ENDIF
c
              IF (NVSOLID2 /= 0 .AND. IDEF /=0) THEN      
                EPS(I,1) = SIGSP(JPS1 + 1 , JJ)
                EPS(I,2) = SIGSP(JPS1 + 2 , JJ)
                EPS(I,3) = SIGSP(JPS1 + 3 , JJ)
                EPS(I,4) = SIGSP(JPS1 + 4 , JJ)
                EPS(I,5) = SIGSP(JPS1 + 5 , JJ)
                EPS(I,6) = SIGSP(JPS1 + 6 , JJ)
                IF (STRAGLOB(I) == 1) THEN
                  TENS(1)=EPS(I,1)
                  TENS(2)=EPS(I,2)
                  TENS(3)=EPS(I,3)
                  TENS(4)=EPS(I,4)
                  TENS(5)=EPS(I,5)
                  TENS(6)=EPS(I,6)
                  CALL SROTA6_M1(X,IX(1,II),JCVT,TENS,GAMA,JHBE,IGTYP)
                  EPS(I,1) = TENS(1)
                  EPS(I,2) = TENS(2)
                  EPS(I,3) = TENS(3)
                  EPS(I,4) = TENS(4)
                  EPS(I,5) = TENS(5)
                  EPS(I,6) = TENS(6)
                ENDIF
              ENDIF  
            ENDIF !IF (IFLAGINI == 1)
c        
        ENDDO ! I=LFT,LLT
       ENDIF  ! ISIGI /= 0
c-----------        
      RETURN
      END SUBROUTINE SIGIN20B
