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
Chd|  PMASS                         source/elements/beam/pmass.F  
Chd|-- called by -----------
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        PINIT3                        source/elements/beam/pinit3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PMASS(GEO    ,PM      ,MS     ,TINER   ,
     .                 STIFN ,STIFR  ,PARTSAV ,V      ,IPART   ,
     .                 MSP   ,INP    ,IGEO    ,STIFINT,STP     ,
     .                 X1,X2,Y1,Y2,Z1,Z2,
     .                 NC1,NC2,MXT,MXG,AREA,AL,STRP , MCP     ,
     .                 MCPP ,TEMP )
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
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
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(*),IGEO(NPROPGI,*),
     .   NC1(MVSIZ), NC2(MVSIZ), MXT(MVSIZ), MXG(MVSIZ)
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*), MS(*), TINER(*),
     .   STIFN(*),STIFR(*),V(3,*),PARTSAV(20,*),MSP(*),INP(*),
     .   STIFINT(*), STP(*),
     .   X1(MVSIZ), X2(MVSIZ),  
     .   Y1(MVSIZ), Y2(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), AREA(MVSIZ), AL(MVSIZ), STRP(*),
     .   MCPP(*),MCP(*),TEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,I1,I2,J,K,IPY,IPZ,IPA,IGTYP,NIP
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX,
     .   STI, E, G, AA, BB, DX,DY,DZ, ARI,INI,RYI,RZI,
     .   STIR, PHI, SHF, DSH, DMP, SL2I, FAC,LL
      my_real
     .   RHO(MVSIZ), EMS(MVSIZ),COEFI(MVSIZ),
     .   TIN(MVSIZ), TIXX(MVSIZ), TIYY(MVSIZ), TIZZ(MVSIZ),RHOCP(MVSIZ)
      my_real
     .   FACDT(MVSIZ), PHII(MVSIZ),KPHI(MVSIZ),CST,PHMAX,PHMIN,FSH(MVSIZ)
C=======================================================================
      IPY = 200  
      IPZ = 300  
      IPA = 400
      DO I=LFT,LLT
        IGTYP =  nint(GEO(12,MXG(I)))
        IF (IGTYP == 18) THEN
          RHO(I) = PM(89,MXT(I))
          AREA(I)= ZERO
          TIYY(I)= ZERO
          TIZZ(I)= ZERO
          NIP    = IGEO(3,MXG(I))
          DO J=1,NIP
            ARI = GEO(IPA+J,MXG(I))
            INI = ARI*ARI*ONE_OVER_12
            RYI = GEO(IPY+J,MXG(I))
            RZI = GEO(IPZ+J,MXG(I))
            AREA(I) = AREA(I) + ARI
            TIYY(I) = TIYY(I) + INI + ARI * RYI*RYI
            TIZZ(I) = TIZZ(I) + INI + ARI * RZI*RZI
          ENDDO
          TIXX(I) = TIYY(I) + TIZZ(I)
          GEO( 1,MXG(I)) = AREA(I) 
          GEO( 4,MXG(I)) = TIXX(I) 
          GEO( 2,MXG(I)) = TIYY(I) 
          GEO(18,MXG(I)) = TIZZ(I) 
        ELSE
          AREA(I)=GEO(1,MXG(I)) 
          TIXX(I)=GEO(4,MXG(I))
          TIYY(I)=GEO(2,MXG(I))
          TIZZ(I)=GEO(18,MXG(I))
          RHO(I) =PM (89,MXT(I))
        ENDIF
      ENDDO
C----------------------------------------------
C     for dt
C----------------------------------------------
      DO I=LFT,LLT
        E = PM (20,MXT(I))
        G = PM (22,MXT(I))
        CST = SIX_OVER_5*E/G
        BB  = MAX(TIYY(I),TIZZ(I),EM30)
        SL2I= AREA(I)*AL(I)**2 /BB
        FACDT(I) = ONE_OVER_12*SL2I
        PHMAX = CST/FACDT(I)        
        PHMIN = MIN(TIYY(I),TIZZ(I))*PHMAX/BB
        KPHI(I) = (FOUR+PHMIN)/(ONE+PHMIN)
        PHII(I) = KPHI(I)/(ONE+FACDT(I))
        PHII(I) = MAX(ONE,PHII(I)) 
        FSH(I) = AL(I)/(FACDT(I)+CST)
        FSH(I) = MAX(ONE,FSH(I))
        COEFI(I) =ONE_OVER_12 
      ENDDO
      IF (IGTYP == 18) THEN
         DO I=LFT,LLT
           FSH(I) = ONE
           KPHI(I) = MAX(ONE,TWELVE*FACDT(I))  
           IF (KPHI(I) > TWELVE ) COEFI(I) =ONE 
         ENDDO
      END IF
C----------------------------------------------
C     MASSE ELEMENT /2
C----------------------------------------------
      DO I=LFT,LLT
       LL = ONEP2*AL(I)
       EMS(I)=RHO(I)*AL(I)*AREA(I)* HALF
       TIN(I)=ONEP2*EMS(I)*LL**2*COEFI(I) + RHO(I)*(AL(I)*HALF)
     .       * MAX(TIYY(I),TIZZ(I))
       IF (FACDT(I)<ONE) TIN(I)=PHII(I)*TIN(I)
       TIN(I)= MAX(TIN(I),RHO(I)*AL(I)/TWO*TIXX(I))
      ENDDO
       IF( JTHE > 0 ) THEN
           DO I=LFT,LLT
              RHOCP(I) = PM(69,MXT(I))
              MCPP(I)  = RHOCP(I)*AL(I)*AREA(I)* HALF
           ENDDO
       ENDIF
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES + RHOCP 
C----------------------------------------------
      IF (IMACH/=3) THEN
        DO I=LFT,LLT
          MS(NC1(I))=MS(NC1(I))+EMS(I)
          MS(NC2(I))=MS(NC2(I))+EMS(I)
        ENDDO
        IF( JTHE > 0 ) THEN
           DO I=LFT,LLT
              MCP(NC1(I))  = MCPP(I)
              MCP(NC2(I))  = MCPP(I)
           ENDDO
       ENDIF
      ELSE
        DO I=LFT,LLT
          MSP(I) = EMS(I)
        ENDDO
      ENDIF
C----------------------------------------------
C     INERTIES SPHERIQUES
C----------------------------------------------
      IF (IMACH/=3) THEN
        DO I=LFT,LLT
         TINER(NC1(I))=TINER(NC1(I))+TIN(I)
         TINER(NC2(I))=TINER(NC2(I))+TIN(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          INP(I) = TIN(I)
        ENDDO
      ENDIF
C----------------------------------------------
C     INITIALISATION DES RIGIDITES NODALES POUR INTERFACES
C----------------------------------------------
      IF(I7STIFS/=0)THEN
       IF (IMACH/=3) THEN
        DO I=LFT,LLT
          E = PM (20,MXT(I))
          STI = E * AREA(I) / AL(I)
          STIFINT(NC1(I))=STIFINT(NC1(I))+STI
          STIFINT(NC2(I))=STIFINT(NC2(I))+STI
        ENDDO
       ELSE
        DO I=LFT,LLT
          E = PM (20,MXT(I))
          STI = E * AREA(I) / AL(I)
          STP(I) = STI
        ENDDO
       ENDIF
      ENDIF
C----------------------------------------------
C     INITIALISATION DES RIGIDITES NODALES
C----------------------------------------------
      DO I=LFT,LLT
       E = PM (20,MXT(I))
       G = PM (22,MXT(I))
C
       DMP =MAX(GEO(16,MXG(I)),GEO(17,MXG(I)))
       DMP =DMP*SQRT(TWO)
       AA  =(SQRT(ONE +DMP*DMP)-DMP)
       AA  = AL(I) * AA * AA
       BB  = MAX(TIYY(I),TIZZ(I))
       STIR = MAX(G*TIXX(I),KPHI(I)*E*BB) / AA
       STI  = FSH(I)*AREA(I) * E / AA
C
       STIFN(NC1(I))=STIFN(NC1(I))+STI
       STIFN(NC2(I))=STIFN(NC2(I))+STI
       STIFR(NC1(I))=STIFR(NC1(I))+STIR
       STIFR(NC2(I))=STIFR(NC2(I))+STIR
       STRP(I)=STIR
      ENDDO
C
      DO I=LFT,LLT
        I1 = NC1(I)
        I2 = NC2(I)
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X1(I)+X2(I))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(Y1(I)+Y2(I))
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(Z1(I)+Z2(I))
        XX = (X1(I)*X1(I)+X2(I)*X2(I))
        XY = (X1(I)*Y1(I)+X2(I)*Y2(I))
        YY = (Y1(I)*Y1(I)+Y2(I)*Y2(I))
        YZ = (Y1(I)*Z1(I)+Y2(I)*Z2(I))
        ZZ = (Z1(I)*Z1(I)+Z2(I)*Z2(I))
        ZX = (Z1(I)*X1(I)+Z2(I)*X2(I))
        PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*TIN(I) + EMS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*TIN(I) + EMS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*TIN(I) + EMS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
      ENDDO
      IF(JTHE > 0 ) THEN                                          
        IF(NINTEMP > 0 ) THEN                                   
          DO I= LFT,LLT                                          
            IF(TEMP(NC1(I))== ZERO) TEMP(NC1(I)) = PM(79,MXT(I))   
            IF(TEMP(NC2(I))== ZERO) TEMP(NC2(I)) = PM(79,MXT(I))
          ENDDO                                                  
        ELSE                                          
          DO I=LFT,LLT                                           
            TEMP(NC1(I)) = PM(79,MXT(I))                           
            TEMP(NC2(I)) = PM(79,MXT(I))                         
          ENDDO                                                  
        ENDIF                                                    
      ENDIF          
C-----------
      RETURN
      END
