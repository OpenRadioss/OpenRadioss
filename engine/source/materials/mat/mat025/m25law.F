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
Chd|  M25LAW                        source/materials/mat/mat025/m25law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        M25CPLA                       source/materials/mat/mat025/m25cpla.F
Chd|        MREPLOC                       source/materials/mat_share/mreploc.F
Chd|        MROTENS                       source/materials/mat_share/mrotens.F
Chd|====================================================================
      SUBROUTINE M25LAW(  PM        ,MAT  ,OFF  ,SIG  ,EINT ,
     1                    S01       ,S02  ,S03  ,S04  ,S05  ,S06 ,
     2                    D1        ,D2   ,D3   ,D4   ,D5   ,D6  ,
     3                    RX        ,RY   ,RZ   ,SX   ,SY   ,SZ  ,
     4                    TX        ,TY   ,TZ   ,GAMA ,VNEW ,SSP , 
     5                    VOL       ,EPSP ,WPLA ,DAMT ,CRAK ,EPST, 
     6                    DEFP      ,SIGY ,SIGL ,NGL  ,ILAY ,FLAY,
     7                    SEQ_OUTPUT,NEL  ,IPM  ,IPG  )
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
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), NGL(MVSIZ),IPM(NPROPMI,*),ILAY,NEL,IPG
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*),OFF(*), SIG(NEL,6), WPLA(*), GAMA(MVSIZ,6), 
     .   EINT(*), DAMT(NEL,2), CRAK(*),  EPSP(*),
     .   RX(*) ,RY(*),RZ(*) ,SX(*),SY(*),SZ(*),
     .   TX(*) ,TY(*) ,TZ(*),DEFP(*), SIGY(*),SEQ_OUTPUT(*)
      my_real
     .   D1(*), D2(*), D3(*), D4(*),
     .   D5(*), D6(*),
     .   S01(*),S02(*),S03(*),S04(*),S05(*),s06(*),
     .   VOl(*),VNEW(*),SSP(*),EPST(NEL,6),SIGL(MVSIZ,6),FLAY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFIS1(MVSIZ), NFIS2(MVSIZ), NFIS3(MVSIZ), INDX(MVSIZ)
      INTEGER I, J, JADR, J1, J2, J3, JJ, II, I2, NINDX, I1,
     .   IOFF,JOFF,MX,IDIR,ISRATE
C     REAL ou REAL*8
      my_real
     .   DEGMB(MVSIZ),DEGFX(MVSIZ),
     .   WPLAR(MVSIZ),STRN1(MVSIZ),STRN2(MVSIZ),STRN3(MVSIZ),
     .   DAMCR(MVSIZ,2), DMAXT(MVSIZ),STRP1(MVSIZ) ,STRP2(MVSIZ),
     .   EPSPL(MVSIZ),S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),
     .   S4(MVSIZ),S5(MVSIZ),S6(MVSIZ),
     .   EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),EP4(MVSIZ),EP5(MVSIZ),
     .   EP6(MVSIZ),EPST1(MVSIZ),EPST2(MVSIZ),EPSM1(MVSIZ),EPSM2(MVSIZ),
     .   DMAX(MVSIZ),R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),
     .   R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
       my_real 
     .   ASRATE,EPS_K2, EPS_M2, SIGT1, SIGT2, 
     .   ZT, WMC, VISC, DTINV ,EPD,DAV,DAM1,DAM2,DT5,
     .   EPST1_1,EPST2_1,EPSM1_1,EPSM2_1,DMAX_1,SSP_1
C=======================================================================
C
       DO  I=LFT,LLT
         WPLAR(I)=ZERO
         NFIS1(I)=0
         NFIS2(I)=0
         NFIS3(I)=0
!         DAMCR(I,1)=ZERO
!         DAMCR(I,2)=ZERO
         DMAXT(I)=ZERO
       ENDDO  
C-----------------------------------------------------------
C     STRAIN RATE FILTERING (EQUIVALENT EPSP)
C-----------------------------------------------------------
       MX = MAT(LFT)
       ISRATE = IPM(3,MX)
       ASRATE = PM(9,MX)*DT1     
       ASRATE = ASRATE / (ASRATE + UN)   
       DO I=LFT,LLT                                                                                                    
         IF(ISRATE >= 1)THEN                                     
           DAV = -THIRD*(D1(I)+D2(I)+D3(I))                         
           EPD = UNDEMI*((D1(I)+DAV )**2+(D2(I)+DAV)**2          
     .                                   +(D3(I)+DAV)**2)           
     .            + QUART*(D4(I)**2+D5(I)**2+D6(I)**2)              
           EPD = SQRT(TROIS*EPD)/TDEMI                        
           EPSP(I) = ASRATE*EPD + (UN - ASRATE)*EPSP(I)     
         ELSE                                                       
           EPSP(I)=OFF(I)* MAX( ABS(D1(I)), ABS(D2(I)), ABS(D3(I)), 
     .     UNDEMI*ABS(D4(I)),UNDEMI*ABS(D5(I)),UNDEMI*ABS(D6(I)))   
         ENDIF                                         
       ENDDO                                                        
C--------------------------------------------
C     STRESS TRANSFORMATION (GLOBAL -> FIBER)
C-------------------------------------------- 
       
        DO I=LFT,LLT
          EP1(I) = D1(I)*DT1
          EP2(I) = D2(I)*DT1
          EP3(I) = D3(I)*DT1
          EP4(I) = D4(I)*DT1
          EP5(I) = D5(I)*DT1
          EP6(I) = D6(I)*DT1 
          S1(I) = SIG(I,1)
          S2(I) = SIG(I,2)
          S3(I) = SIG(I,3)
          S4(I) = SIG(I,4)
          S5(I) = SIG(I,5)
          S6(I) = SIG(I,6)
        ENDDO
      IF(ISORTH > 0 .AND. JCVT == 0) THEN
        CALL MREPLOC( GAMA,R11,R12,R13,R21,R22    ,
     2                 R23,R31,R32 ,R33,RX ,RY  ,RZ, SX    ,
     3                 SY ,SZ , TX  ,TY ,TZ )
        DO I=LFT,LLT
         EP4(I) = UNDEMI*EP4(I)
         EP5(I) = UNDEMI*EP5(I)
         EP6(I) = UNDEMI*EP6(I)
        ENDDO
        CALL MROTENS(LFT,LLT,EP1,EP2,EP3,EP4,EP5,EP6,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
        DO I=LFT,LLT
         EP4(I) = DEUX*EP4(I)
         EP5(I) = DEUX*EP5(I)
         EP6(I) = DEUX*EP6(I)
        ENDDO
C stress
        CALL MROTENS(LFT,LLT,S1,S2,S3,S4,S5,S6,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33) 
       ENDIF  
C-----------------------------------------------------------
C-----------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C-----------------------
      CALL M25CPLA(LFT, LLT ,PM   , MAT ,NGL, OFF,
     2             S1   ,S2   ,S3  ,S4   ,S5   ,S6    ,
     3             EP1  ,EP2  ,EP3 ,EP4  , EP5 ,EP6,
     4             EPST ,DAMT ,CRAK ,NFIS1  ,NFIS2    ,NFIS3,
     5             WPLAR    ,EPSP, WPLA, SIGL, ILAY,FLAY,
     6             SEQ_OUTPUT,NEL, IPG )
C      
        IF(ISORTH > 0 .AND. JCVT == 0) THEN
         CALL MROTENS(LFT,LLT,
     .              S1 ,S2 ,S3 ,
     .              S4 ,S5 ,S6 ,
     .              R11,R21,R31,
     .              R12,R22,R32,
     .              R13,R23,R33 )
        ENDIF
     
          DO I=LFT,LLT
              SIG(I,1) = S1(I)*OFF(I)
              SIG(I,2) = S2(I)*OFF(I)
              SIG(I,3) = S3(I)*OFF(I)
              SIG(I,4) = S4(I)*OFF(I)
              SIG(I,5) = S5(I)*OFF(I)
              SIG(I,6) = S6(I)*OFF(I)
          ENDDO
          
         
C-----------------------
C     TENSILE RUPTURE
C-----------------------
      MX        =MAT(LFT)
      EPST1_1  =PM(60,MX)
      EPST2_1  =PM(61,MX)
      EPSM1_1  =PM(62,MX)
      EPSM2_1  =PM(63,MX)
      DMAX_1   =PM(64,MX)
      SSP_1 = PM(27,MX)
      DO  I=LFT,LLT
         EPST1(I)  =EPST1_1
         EPST2(I)  =EPST2_1
         EPSM1(I)  =EPSM1_1
         EPSM2(I)  =EPSM2_1
         DMAX(I)   =DMAX_1
         SSP(I) = SSP_1
      ENDDO
C.....STRAINS IN ORTHOTROPIC DIRECTIONS
C
C.....GATHER DIRECTION 1
      NINDX=0
      DO  I=LFT,LLT
        IF(EPST(I,1) >= EPST1(I)
     +    .AND. DAMT(I,1) == ZERO .AND. OFF(I) == UN)THEN
          NINDX=NINDX+1
          INDX(NINDX)=I
        ENDIF  
      ENDDO
C.....1.FIRST FAILURE DIRECTION 1
      IF(NINDX.GT.0)THEN
        IDIR=1
          DO  J=1,NINDX
            I=INDX(J)
            DAM1=(EPST(I,1)-EPST1(I))/(EPSM1(I)-EPST1(I))
            DAM2= DAM1*EPSM1(I)/EPST(I,1)
            DAMT(I,1)= MIN(DAM2,DMAX(I))
            IF(DAMT(I,1).EQ.DMAX(I).AND.IMCONV.EQ.1)THEN
#include "lockon.inc"
               WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),
     +        ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +        ', TIME=',TT      
#include "lockoff.inc"
            ENDIF      
         ENDDO
       ENDIF      
C.....GATHER DIRECTION 2
       NINDX=0
      DO  I=LFT,LLT
        IF(EPST(I,2) >= EPST2(I)
     +         .AND. DAMT(I,2) == ZERO .AND. OFF(I) == UN) THEN
          NINDX=NINDX+1
          INDX(NINDX)=I
        ENDIF  
      ENDDO
C.....1.FIRST FAILURE DIRECTION 2
      IF(NINDX > 0)THEN
         IDIR=2
         DO  J=1,NINDX
          I=INDX(J)
          DAM1=(EPST(I,2)-EPST2(I))/(EPSM2(I)-EPST2(I))
          DAM2= DAM1*EPSM2(I)/EPST(I,2)
          DAMT(I,2)= MIN(DAM2,DMAX(I))
          IF(DAMT(I,2) == DMAX(I) .AND. IMCONV == 1)THEN
#include "lockon.inc"
              WRITE(IOUT, '(A,I1,A,I10,A,I3,A,I3,A,1PE11.4)')
     +        ' FAILURE-',IDIR,', ELEMENT #',NGL(I),
     +        ', LAYER #',ILAY,', INTEGRATION POINT #',IPG,
     +        ', TIME=',TT      
#include "lockoff.inc"
          ENDIF      
        ENDDO
      ENDIF      
C.....DOMMAGE GLOBAL
c      DO  I=LFT,LLT
c      DAMCR(I,1)=DAMCR(I,1)+THLY(I)*DAMT(I,1)
c      DAMCR(I,2)=DAMCR(I,2)+THLY(I)*DAMT(I,2)
c      DMAXT(I)  =DMAXT(I)  +THLY(I)*DMAX(I)
c      ENDDO
C----------------------------
C     TEST DE RUPTURE 
C----------------------------
      DO  I=LFT,LLT
        IF(OFF(I) < EM01) OFF(I)=ZERO
        IF(OFF(I) < UN  ) OFF(I)=OFF(I)*QCINQ
      ENDDO

      NINDX=0
      MX = MAT(LFT)
       DO  I=LFT,LLT
           IF(OFF(I).EQ.UN) THEN
             IOFF=NINT(PM(42,MX))
             IF(IOFF < 0) IOFF=-(IOFF+1)
             JOFF=0
c             IF(IOFF == 0 .AND. WPLAR(I) >= UN) JOFF=1
c             IF(IOFF == 1 .AND. NINT(WPLAR(I)) >= NPT) JOFF=1
c             IF(IOFF == 2 .AND. NFIS1(I) == NPT) JOFF=1
c             IF(IOFF == 3 .AND. NFIS2(I) == NPT) JOFF=1
c             IF(IOFF == 4 .AND. NFIS1(I) == NPT 
c     .                     .AND. NFIS2(I) == NPT) JOFF=1
c             IF(IOFF == 5 .AND. NFIS1(I) == NPT) JOFF=1
c             IF(IOFF == 5 .AND. NFIS2(I) == NPT) JOFF=1
c             IF(IOFF == 6 .AND. NFIS3(I) == NPT) JOFF=1
             IF(JOFF == UN) THEN
                 OFF(I)=OFF(I)*QCINQ
                 II=I+NFT
                 NINDX=NINDX+1
                 INDX(NINDX)=I
                 IF(IMCONV.EQ.1)THEN
#include "lockon.inc"
                    WRITE(IOUT,1000)  NGL(I)
                    WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
                 ENDIF
                 IDEL7NOK = 1
             ENDIF
          ENDIF
       ENDDO
      DT5=UNDEMI*DT1
      DO I=LFT,LLT
        EINT(I)=EINT(I) + DT5*VNEW(I)*
     .           ( D1(I)*(S01(I)+SIG(I,1))
     .           + D2(I)*(S02(I)+SIG(I,2))
     .           + D3(I)*(S03(I)+SIG(I,3))
     .           + D4(I)*(S04(I)+SIG(I,4))
     .           + D5(I)*(S05(I)+SIG(I,5))
     .           + D6(I)*(S06(I)+SIG(I,6)))
        EINT(I)=EINT(I)/VOL(I)
      ENDDO
       
 1000 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT :',I10,' AT TIME :',G11.4)
      RETURN
      END
