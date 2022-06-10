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
Chd|  S10MASS3                      source/elements/solid/solide10/s10mass3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|-- calls ---------------
Chd|        S4FRACA10                     source/elements/solid/solide10/s10mass3.F
Chd|====================================================================
      SUBROUTINE S10MASS3(
     1   MASS,MS ,PARTSAV,IPART  ,MSS ,VOLU,
     2   XX  ,YY ,ZZ     ,VX     ,VY  ,VZ  ,  
     3   NC  ,STI,STIFN  ,DELTAX2,MSSX,MASSCP,
     4   MCP ,MCPS       ,MCPSX  ,IN  ,STIFR,
     5   INS ,MSSA       ,X      ,FILL)
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
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(*), NC(MVSIZ,10)
      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C     REAL
      my_real
     .   MASS(*), MS(*),PARTSAV(20,*), MSS(8,*),DELTAX2(*), 
     .   VOLU(*),
     .   VX(MVSIZ,10), VY(MVSIZ,10), VZ(MVSIZ,10),STI(*),STIFN(*),
     .   MSSX(12,*), MASSCP(*),  MCP(*) ,MCPS(8,*) ,MCPSX(12,*),
     .   IN(*),STIFR(*),INS(8,*), MSSA(*), X(3,*), FILL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,N1,N2,N11,N22,IPERM1(10),IPERM2(10),N,IPERM3(10)
C     REAL
      my_real
     .   AXX,AYY,AZZ,AXY,AYZ,AZX,AM,BM,FAC,BI,AAA,FACIROT,PTG(10,MVSIZ),
     .   BBB
C
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
      DATA IPERM3/1,3,6,5,0,0,0,0,0,0/
C
C-----------------------------------------------------------------------
      FAC=TRHEE_OVER_14
      FACIROT = NINE + THIRD

      IF(IREST_MSELT /= 0)THEN
       DO I=LFT,LLT
        MSSA(I)=MASS(I)
       ENDDO
      ENDIF
      CALL S4FRACA10(X,NC(1,1),NC(1,2),NC(1,3),NC(1,4),PTG   )
C
      IF(IDT1TET10==0.AND.ISROT == 0)THEN
C       same as RD Engine
        DO I=LFT,LLT
          STI(I)=FOURTH*STI(I)
        ENDDO
      END IF
      DO I=LFT,LLT
       IF(ISROT == 0)THEN
        IF(IDT1TET10==0)THEN
          AM = MASS(I)*FAC/(FOUR*FAC + SIX)
          BM = MASS(I)*ONE/(FOUR*FAC + SIX)
          MSS(1,I)=AM*PTG(1,I)
          MSS(3,I)=AM*PTG(2,I)
          MSS(6,I)=AM*PTG(3,I)
          MSS(5,I)=AM*PTG(4,I)
          STIFN(NC(I,1))=STIFN(NC(I,1))+STI(I)*DELTAX2(I)
          STIFN(NC(I,2))=STIFN(NC(I,2))+STI(I)*DELTAX2(I)
          STIFN(NC(I,3))=STIFN(NC(I,3))+STI(I)*DELTAX2(I)
          STIFN(NC(I,4))=STIFN(NC(I,4))+STI(I)*DELTAX2(I)
          DO N=5,10	    
           IF(NC(I,N).NE.0)THEN
             MSSX(N-4,I)=BM*PTG(N,I)
             STIFN(NC(I,N))=STIFN(NC(I,N))+STI(I)
           ELSE
             N11=IPERM1(N)
             N1=IPERM3(N11)
             N22=IPERM2(N)
             N2=IPERM3(N22)
             MSS(N1,I)=MSS(N1,I) + HALF*BM*PTG(N,I)
             MSS(N2,I)=MSS(N2,I) + HALF*BM*PTG(N,I)	     
             STIFN(NC(I,N11))=STIFN(NC(I,N11))  + HALF*STI(I)
             STIFN(NC(I,N22))=STIFN(NC(I,N22))  + HALF*STI(I)      
           ENDIF
          ENDDO
        ELSE
C
C         Not yet compatible with IMAS_DS/=0
          AM = MASS(I)*FAC/(FOUR*FAC + SIX)
          BM = MASS(I)*ONE/(FOUR*FAC + SIX)
          MSS(1,I)=AM
          MSS(3,I)=AM
          MSS(6,I)=AM
          MSS(5,I)=AM
          AAA=STI(I)*TWO/THIRTY2
          BBB=STI(I)*TWO*SEVEN/FOURTY8
          STIFN(NC(I,1))=STIFN(NC(I,1))+AAA
          STIFN(NC(I,2))=STIFN(NC(I,2))+AAA
          STIFN(NC(I,3))=STIFN(NC(I,3))+AAA
          STIFN(NC(I,4))=STIFN(NC(I,4))+AAA
          DO N=5,10	    
           IF(NC(I,N).NE.0)THEN
             MSSX(N-4,I)=BM
             STIFN(NC(I,N))=STIFN(NC(I,N))+BBB
           ELSE
             N11=IPERM1(N)
             N1 =IPERM3(N11)
             N22=IPERM2(N)
             N2 =IPERM3(N22)
             MSS(N1,I)=MSS(N1,I) + HALF*BM
             MSS(N2,I)=MSS(N2,I) + HALF*BM       
             STIFN(NC(I,N11))=STIFN(NC(I,N11))  + HALF*BBB
             STIFN(NC(I,N22))=STIFN(NC(I,N22))  + HALF*BBB      
           ENDIF
          ENDDO
        END IF
       ELSE IF(ISROT == 1)THEN
         AM = MASS(I)/FOUR
         AAA = ((SIX*SQR2*FOUR*VOLU(I))**TWO_THIRD)
         BM = ZERO
c         BI = (BM*AAA*THREE/EIGHT)
c         BI = (BM*AAA*THREE/EIGHT)*EIGHT
c         BI = (AM*AAA/FOUR)*EIGHT
         BI = (SEVEN/FIVE)*DELTAX2(I)*MASS(I)
         MSS(1,I)=AM*PTG(1,I)
         MSS(3,I)=AM*PTG(2,I)
         MSS(6,I)=AM*PTG(3,I)
         MSS(5,I)=AM*PTG(4,I)
         AAA = STI(I)*TWO/FOUR
         STIFN(NC(I,1))=STIFN(NC(I,1))+AAA
         STIFN(NC(I,2))=STIFN(NC(I,2))+AAA
         STIFN(NC(I,3))=STIFN(NC(I,3))+AAA
         STIFN(NC(I,4))=STIFN(NC(I,4))+AAA
         INS(1,I)= BI
         INS(3,I)= BI
         INS(6,I)= BI
         INS(5,I)= BI
         AAA = STI(I)*DELTAX2(I)*THREE/EIGHT/FOUR
         STIFR(NC(I,1))=STIFR(NC(I,1))+AAA
         STIFR(NC(I,2))=STIFR(NC(I,2))+AAA
         STIFR(NC(I,3))=STIFR(NC(I,3))+AAA
         STIFR(NC(I,4))=STIFR(NC(I,4))+AAA

       ELSE IF(ISROT == 2)THEN

         IF(IDT1TET10==0)THEN
           AM = MASS(I)/FOUR
           AAA = ((SIX*SQR2*FOUR*VOLU(I))**TWO_THIRD)
           BM = ZERO
c           BI = (BM*AAA*THREE/EIGHT)
c           BI = (BM*AAA*THREE/EIGHT)*EIGHT
c           BI = (AM*AAA/FOUR)*EIGHT
c           BI = (SEVEN/FIVE)*DELTAX2(I)*MASS(I)
           MSS(1,I)=AM*PTG(1,I)
           MSS(3,I)=AM*PTG(2,I)
           MSS(6,I)=AM*PTG(3,I)
           MSS(5,I)=AM*PTG(4,I)
           AAA = STI(I)*TWO/FOUR
           STIFN(NC(I,1))=STIFN(NC(I,1))+AAA
           STIFN(NC(I,2))=STIFN(NC(I,2))+AAA
           STIFN(NC(I,3))=STIFN(NC(I,3))+AAA
           STIFN(NC(I,4))=STIFN(NC(I,4))+AAA
           DO N=5,10
             IF(NC(I,N).NE.0)THEN
               MSSX(N-4,I) = AM*FACIROT
               STIFN(NC(I,N))=STIFN(NC(I,N))+AAA*FACIROT
             ENDIF
           ENDDO
         ELSE
C
C          Not yet compatible with IMAS_DS/=0
           AM = MASS(I)/FOUR
           BM = ZERO
           MSS(1,I)=AM
           MSS(3,I)=AM
           MSS(6,I)=AM
           MSS(5,I)=AM
           AAA = STI(I)*TWO/FOUR
           STIFN(NC(I,1))=STIFN(NC(I,1))+AAA
           STIFN(NC(I,2))=STIFN(NC(I,2))+AAA
           STIFN(NC(I,3))=STIFN(NC(I,3))+AAA
           STIFN(NC(I,4))=STIFN(NC(I,4))+AAA
           DO N=5,10
             IF(NC(I,N).NE.0)THEN
               MSSX(N-4,I) = AM*FACIROT !  meme masse que dans le cas standard
               STIFN(NC(I,N))=STIFN(NC(I,N))+AAA*FACIROT
             ENDIF
           ENDDO
         END IF

       ENDIF
C
       IP=IPART(I)
       PARTSAV(1,IP)=PARTSAV(1,IP) + MASS(I)
       PARTSAV(2,IP)=PARTSAV(2,IP)
     .     + AM*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4))
     .     + BM*(XX(I,5)+XX(I,6)+XX(I,7)+XX(I,8)+XX(I,9)+XX(I,10))
       PARTSAV(3,IP)=PARTSAV(3,IP)
     .     + AM*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4))
     .     + BM*(YY(I,5)+YY(I,6)+YY(I,7)+YY(I,8)+YY(I,9)+YY(I,10))
       PARTSAV(4,IP)=PARTSAV(4,IP)
     .     + AM*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4))
     .     + BM*(ZZ(I,5)+ZZ(I,6)+ZZ(I,7)+ZZ(I,8)+ZZ(I,9)+ZZ(I,10))
       AXX = AM*(XX(I,1)*XX(I,1)+XX(I,2)*XX(I,2)
     .  	+XX(I,3)*XX(I,3)+XX(I,4)*XX(I,4))
     .      +BM*(XX(I,5)*XX(I,5)+XX(I,6)*XX(I,6)
     .  	+XX(I,7)*XX(I,7)+XX(I,8)*XX(I,8)
     .  	+XX(I,9)*XX(I,9)+XX(I,10)*XX(I,10))
       AYY = AM*(YY(I,1)*YY(I,1)+YY(I,2)*YY(I,2)
     .  	+YY(I,3)*YY(I,3)+YY(I,4)*YY(I,4))
     .      +BM*(YY(I,5)*YY(I,5)+YY(I,6)*YY(I,6)
     .  	+YY(I,7)*YY(I,7)+YY(I,8)*YY(I,8)
     .  	+YY(I,9)*YY(I,9)+YY(I,10)*YY(I,10))
       AZZ = AM*(ZZ(I,1)*ZZ(I,1)+ZZ(I,2)*ZZ(I,2)
     .  	+ZZ(I,3)*ZZ(I,3)+ZZ(I,4)*ZZ(I,4))
     .      +BM*(ZZ(I,5)*ZZ(I,5)+ZZ(I,6)*ZZ(I,6)
     .  	+ZZ(I,7)*ZZ(I,7)+ZZ(I,8)*ZZ(I,8)
     .  	+ZZ(I,9)*ZZ(I,9)+ZZ(I,10)*ZZ(I,10))
       AXY = AM*(XX(I,1)*YY(I,1)+XX(I,2)*YY(I,2)
     .  	+XX(I,3)*YY(I,3)+XX(I,4)*YY(I,4))
     .      +BM*(XX(I,5)*YY(I,5)+XX(I,6)*YY(I,6)
     .  	+XX(I,7)*YY(I,7)+XX(I,8)*YY(I,8)
     .  	+XX(I,9)*YY(I,9)+XX(I,10)*YY(I,10))
       AYZ = AM*(YY(I,1)*ZZ(I,1)+YY(I,2)*ZZ(I,2)
     .  	+YY(I,3)*ZZ(I,3)+YY(I,4)*ZZ(I,4))
     .      +BM*(YY(I,5)*ZZ(I,5)+YY(I,6)*ZZ(I,6)
     .  	+YY(I,7)*ZZ(I,7)+YY(I,8)*ZZ(I,8)
     .  	+YY(I,9)*ZZ(I,9)+YY(I,10)*ZZ(I,10))
       AZX = AM*(ZZ(I,1)*XX(I,1)+ZZ(I,2)*XX(I,2)
     .  	+ZZ(I,3)*XX(I,3)+ZZ(I,4)*XX(I,4))
     .      +BM*(ZZ(I,5)*XX(I,5)+ZZ(I,6)*XX(I,6)
     .  	+ZZ(I,7)*XX(I,7)+ZZ(I,8)*XX(I,8)
     .  	+ZZ(I,9)*XX(I,9)+ZZ(I,10)*XX(I,10))
       PARTSAV(5,IP) =PARTSAV(5,IP)  + (AYY+AZZ)
       PARTSAV(6,IP) =PARTSAV(6,IP)  + (AZZ+AXX)
       PARTSAV(7,IP) =PARTSAV(7,IP)  + (AXX+AYY)
       PARTSAV(8,IP) =PARTSAV(8,IP)  - AXY
       PARTSAV(9,IP) =PARTSAV(9,IP)  - AYZ
       PARTSAV(10,IP)=PARTSAV(10,IP) - AZX
C
       PARTSAV(11,IP)=PARTSAV(11,IP)
     .     + AM*(VX(I,1)+VX(I,2)+VX(I,3)+VX(I,4))
     .     + BM*(VX(I,5)+VX(I,6)+VX(I,7)+VX(I,8)+VX(I,9)+VX(I,10))
       PARTSAV(12,IP)=PARTSAV(12,IP)
     .     + AM*(VY(I,1)+VY(I,2)+VY(I,3)+VY(I,4))
     .     + BM*(VY(I,5)+VY(I,6)+VY(I,7)+VY(I,8)+VY(I,9)+VY(I,10))
       PARTSAV(13,IP)=PARTSAV(13,IP)
     .     + AM*(VZ(I,1)+VZ(I,2)+VZ(I,3)+VZ(I,4))
     .     + BM*(VZ(I,5)+VZ(I,6)+VZ(I,7)+VZ(I,8)+VZ(I,9)+VZ(I,10))
       PARTSAV(14,IP)=PARTSAV(14,IP) + HALF *
     .      (AM*(VX(I,1)*VX(I,1)+VX(I,2)*VX(I,2)
     .  	+VX(I,3)*VX(I,3)+VX(I,4)*VX(I,4)
     .  	+VY(I,1)*VY(I,1)+VY(I,2)*VY(I,2)
     .  	+VY(I,3)*VY(I,3)+VY(I,4)*VY(I,4)
     .  	+VZ(I,1)*VZ(I,1)+VZ(I,2)*VZ(I,2)
     .  	+VZ(I,3)*VZ(I,3)+VZ(I,4)*VZ(I,4))
     .      +BM*(VX(I,5)*VX(I,5)+VX(I,6)*VX(I,6)
     .  	+VX(I,7)*VX(I,7)+VX(I,8)*VX(I,8)
     .  	+VX(I,9)*VX(I,9)+VX(I,10)*VX(I,10)
     .  	+VY(I,5)*VY(I,5)+VY(I,6)*VY(I,6)
     .  	+VY(I,7)*VY(I,7)+VY(I,8)*VY(I,8)
     .  	+VY(I,9)*VY(I,9)+VY(I,10)*VY(I,10)
     .  	+VZ(I,5)*VZ(I,5)+VZ(I,6)*VZ(I,6)
     .  	+VZ(I,7)*VZ(I,7)+VZ(I,8)*VZ(I,8)
     .  	+VZ(I,9)*VZ(I,9)+VZ(I,10)*VZ(I,10)))
      ENDDO

      IF(JTHE < 0) THEN
       DO I=LFT,LLT
        FAC=TRHEE_OVER_14
        AM = MASSCP(I)*FAC/(FOUR*FAC + SIX)
        BM = MASSCP(I)*ONE/(FOUR*FAC + SIX)
        MCPS(1,I)=AM
        MCPS(3,I)=AM
        MCPS(6,I)=AM
        MCPS(5,I)=AM
        DO N=5,10	  
          IF(NC(I,N).NE.0)THEN
            MCPSX(N-4,I) = BM
          ELSE
            N11=IPERM1(N)
            N1=IPERM3(N11)
            N22=IPERM2(N)
            N2=IPERM3(N22)
            MCPS(N1,I)=MCPS(N1,I) + HALF*BM
            MCPS(N2,I)=MCPS(N2,I) + HALF*BM
          ENDIF
        ENDDO
       ENDDO
      ENDIF  
C
      RETURN
      END
C
Chd|====================================================================
Chd|  S10MSI                        source/elements/solid/solide10/s10mass3.F
Chd|-- called by -----------
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10MSI(RHO,MASS,VOLU,DTELEM,STI,
     .                  OFF,SIG,EINT,
     .                  OFFG,SIGG,EINTG,RHOG,WIP,
     .                  MASSCP,RHOCP,FILL,NEL, DTX)
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
     .   SIGG(NEL,6),EINTG(*),RHOG(*),OFFG(*),WIP,
     .   RHOCP(*) ,MASSCP(*), FILL(*), DTX(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      IF(JTHE < 0) THEN
         DO I=LFT,LLT
           MASS(I)=MASS(I)+FILL(I)*RHO(I)*VOLU(I)
           DTELEM(I)=MIN(DTELEM(I),DTX(I))
           STI(I) = STI(I) + FILL(I) * RHO(I) * VOLU(I) / MAX(EM20,DTX(I)*DTX(I))
           SIGG(I,1) = SIGG(I,1) + WIP * SIG(I,1)
           SIGG(I,2) = SIGG(I,2) + WIP * SIG(I,2)
           SIGG(I,3) = SIGG(I,3) + WIP * SIG(I,3)
           SIGG(I,4) = SIGG(I,4) + WIP * SIG(I,4)
           SIGG(I,5) = SIGG(I,5) + WIP * SIG(I,5)
           SIGG(I,6) = SIGG(I,6) + WIP * SIG(I,6)
           RHOG(I)   = RHOG(I)   + WIP * RHO(I)
           EINTG(I)  = EINTG(I)  + WIP * EINT(I)
           OFFG(I)   = OFF(I)
           MASSCP(I) = MASSCP(I) + FILL(I)*RHOCP(I)*VOLU(I)
         ENDDO
       ELSE
         DO I=LFT,LLT
           MASS(I)=MASS(I)+FILL(I)*RHO(I)*VOLU(I)
           DTELEM(I)=MIN(DTELEM(I),DTX(I))
           STI(I) = STI(I) + FILL(I) * RHO(I) * VOLU(I) / MAX(EM20,DTX(I)*DTX(I))
           SIGG(I,1) = SIGG(I,1) + WIP * SIG(I,1)
           SIGG(I,2) = SIGG(I,2) + WIP * SIG(I,2)
           SIGG(I,3) = SIGG(I,3) + WIP * SIG(I,3)
           SIGG(I,4) = SIGG(I,4) + WIP * SIG(I,4)
           SIGG(I,5) = SIGG(I,5) + WIP * SIG(I,5)
           SIGG(I,6) = SIGG(I,6) + WIP * SIG(I,6)
           RHOG(I)   = RHOG(I)   + WIP * RHO(I)
           EINTG(I)  = EINTG(I)  + WIP * EINT(I)
           OFFG(I)   = OFF(I)
           MASSCP(I) = MASSCP(I) + FILL(I)*RHOCP(I)*VOLU(I)
         ENDDO       
         
       
       ENDIF  
C
      RETURN
      END
Chd|====================================================================
Chd|  S4FRACA10                     source/elements/solid/solide10/s10mass3.F
Chd|-- called by -----------
Chd|        S10MASS3                      source/elements/solid/solide10/s10mass3.F
Chd|-- calls ---------------
Chd|        AREA_TRIA                     source/elements/solid/solide4/s4mass3.F
Chd|====================================================================
      SUBROUTINE S4FRACA10(X,IX1 ,IX2,IX3 ,IX4 ,PTG   )
C----------------------------------------------
C     MASS PARTITION IN FUNCTION OF NODAL ANGLES by AREA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr21_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX1(*), IX2(*), IX3(*),IX4(*)
      my_real
     .   X(3,*),PTG(10,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,IP,I1,I2,I3,I4
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX,P1,P2,P3,P4,S
      my_real
     .   A1(MVSIZ), A2(MVSIZ), A3(MVSIZ),A4(MVSIZ)
C=======================================================================
      IF (IMAS_DS==0) THEN
       DO I=LFT,LLT
       DO J=1,10
        PTG(J,I)=ONE
       END DO
       END DO
       RETURN
      END IF
C ------------Ai -> A²(i)-----------
       CALL AREA_TRIA(X,IX2,IX3,IX4, A1  )
       CALL AREA_TRIA(X,IX1,IX3,IX4, A2  )
       CALL AREA_TRIA(X,IX1,IX2,IX4, A3  )
       CALL AREA_TRIA(X,IX1,IX2,IX3, A4  )
      DO I=LFT,LLT
        S = FOUR/(A1(I) +A2(I) +A3(I) +A4(I)) 
        PTG(1,I)=A1(I)*S
        PTG(2,I)=A2(I)*S
        PTG(3,I)=A3(I)*S
        PTG(4,I)=A4(I)*S
        PTG(5,I)=HALF*(PTG(1,I)+PTG(2,I))
        PTG(6,I)=HALF*(PTG(2,I)+PTG(3,I))
        PTG(7,I)=HALF*(PTG(1,I)+PTG(3,I))
        PTG(8,I)=HALF*(PTG(1,I)+PTG(4,I))
        PTG(9,I)=HALF*(PTG(2,I)+PTG(4,I))
        PTG(10,I)=HALF*(PTG(3,I)+PTG(4,I))
      END DO
C
C-----------
      RETURN
      END
