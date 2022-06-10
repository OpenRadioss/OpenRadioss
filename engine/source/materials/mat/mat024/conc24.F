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
Chd|  CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- called by -----------
Chd|        M24LAW                        source/materials/mat/mat024/m24law.F
Chd|-- calls ---------------
Chd|        AGLO24                        source/materials/mat/mat024/aglo24.F
Chd|        CARM24                        source/materials/mat/mat024/carm24.F
Chd|        CRIT24                        source/materials/mat/mat024/crit24.F
Chd|        DAMA24                        source/materials/mat/mat024/dama24.F
Chd|        ELAS24                        source/materials/mat/mat024/elas24.F
Chd|        GLOA24                        source/materials/mat/mat024/gloa24.F
Chd|        PLAS24                        source/materials/mat/mat024/plas24.F
Chd|        PLAS24B                       source/materials/mat/mat024/plas24b.F
Chd|        ROTLOC                        source/materials/mat/mat024/rotloc.F
Chd|        UDAM24N                       source/materials/mat/mat024/udam24.F
Chd|====================================================================
      SUBROUTINE CONC24(PM      ,OFF    ,SIG    ,EINT   ,SIGA   ,
     2                  EPXA    ,GAMA   ,DAM    ,ANG    ,EPS_F  ,
     3                  VK0     ,STRAIN ,CRAK   ,DAMSUM ,ROB    ,
     4                  SIGC    ,VK     ,PLA    ,NGL    ,D1     ,
     5                  D2      ,D3     ,D4     ,D5     ,D6     ,
     6                  RX      ,RY     ,RZ     ,SX     ,SY     ,
     7                  SZ      ,SEQ    ,RHO    ,EPSVP  ,NEL    ,
     8                  R11     ,R12    ,R13    ,R21    ,R22    ,
     9                  R23     ,R31    ,R32    ,R33    )
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
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(NEL),NEL
      my_real GAMA(MVSIZ,6)
      my_real PM(NPROPM), OFF(NEL), SIG(NEL,6), EINT(NEL), SIGA(NEL,3),
     .   EPXA(NEL,3), DAM(NEL,3), ANG(NEL,6), EPS_F(NEL,3), VK0(NEL),VK(NEL),
     .   STRAIN(NEL,6), CRAK(NEL,3), DAMSUM(NEL), ROB(NEL), SIGC(NEL,6),
     .   RX(NEL),RY(NEL),RZ(NEL),SX(NEL),SY(NEL),SZ(NEL),PLA(NEL,7),
     .   D1(NEL), D2(NEL), D3(NEL), D4(NEL), D5(NEL), D6(NEL),SEQ(NEL),RHO(NEL),
     .   EPSVP(NEL)
      my_real, DIMENSION(NEL) :: R11,R12,R13,R21,R22,R23,R31,R32,R33
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,I1,I2,ICAP,NBDAMA
      INTEGER INDEX1(NEL), INDEX2(NEL),DAMAI(NEL)
      my_real :: YMS,Y0S,ETS,VMAX,EPSMAX,RT,ARMA,PLUS,DIFF,ETEST
      my_real, DIMENSION(3)   :: DAM0,EPS_F0,CRAK0
      my_real, DIMENSION(6)   :: ANG0,SIGC0
      my_real, DIMENSION(NEL) :: DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6,
     .   ARM1,ARM2,ARM3,SCLE1, SCLE2, SCLE3,SCAL1, SCAL2, SCAL3,
     .   H1, H2, H3, H4, H5, H6, S01, S02, S03, S04, S05, S06, 
     .   SM, DSM, S1,S2,S3,S4,S5,S6,DE1,DE2, DE3, C44, C55, C66
      my_real, DIMENSION(NEL,3,3) :: CDAM
C=======================================================================
      DO I=LFT,LLT
        STRAIN(I,1) = STRAIN(I,1) + D1(I)*DT1
        STRAIN(I,2) = STRAIN(I,2) + D2(I)*DT1
        STRAIN(I,3) = STRAIN(I,3) + D3(I)*DT1
        STRAIN(I,4) = STRAIN(I,4) + D4(I)*DT1
        STRAIN(I,5) = STRAIN(I,5) + D5(I)*DT1
        STRAIN(I,6) = STRAIN(I,6) + D6(I)*DT1
      ENDDO
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C ARMATURES
C . . . . . . . . . . .
      DO I=LFT,LLT
        ARM1(I) = PM(53)
        ARM2(I) = PM(54)
        ARM3(I) = PM(55)
      ENDDO
      VMAX = PM(27)
      RT   = PM(34)
      EPSMAX = PM(47)
      YMS  = PM(50)         
      Y0S  = PM(51)         
      ETS  = PM(52)         
      ICAP = NINT(PM(57))   
c
      ARMA = ZERO
      DO I=LFT,LLT
        ARMA = ARMA + ARM1(I) + ARM2(I) + ARM3(I)
      ENDDO
c     GAMA = repere  d'armature
      CALL ROTLOC(NEL,GAMA,
     .            RX,RY,RZ,SX,SY,SZ,
     .            R11,R12,R13,R21,R22,
     .            R23,R31,R32,R33)
c
      CALL GLOA24(NEL,STRAIN,DT1,ARM1,ARM2,ARM3,
     .            D1,D2,D3,D4,D5,D6,
     .            R11,R12,R13,R21,R22,
     .            R23,R31,R32,R33,
     .            DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6)
c
      IF (ARMA > ZERO ) THEN
        CALL CARM24(NEL,YMS,Y0S,ETS,
     .              EPXA,SIGA,DEPS1,DEPS2,DEPS3,
     .              DEPS4,DEPS5,DEPS6)
      ENDIF
c
c------
C BETON
c------
C     1 - EVOLUTION DU DOMMAGE ET PREVISION ELASTIQUE
c------
      CALL ELAS24(NEL  ,PM   ,
     .            CDAM ,SIGC ,DAM  ,ANG  ,EPS_F,CRAK ,
     .            C44  ,C55  ,C66  ,DE1  ,DE2  ,DE3  ,
     .            DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6,
     .            S01  ,S02  ,S03  ,S04  ,S05  ,S06  ,
     .            SCAL1,SCAL2,SCAL3)
c------
C     2 - EVALUATION DU CRITERE
c------
      CALL CRIT24(NEL,PM,SIGC,VK0,OFF,
     .            ROB,NGL,SEQ,
     .            S01,S02,S03,S04,S05,S06,
     .            S1 ,S2 ,S3 ,S4 ,S5 ,S6 ,
     .            SCAL1,SCAL2,SCAL3,SCLE1,SCLE2,SCLE3,
     .            SM,DSM)
c------
C     3 - ELASTIQUE, ENDOMAGE ET/OU PLASTIQUE
c------
      I1 = 0
      I2 = 0
c
      DO I=LFT,LLT
        IF (OFF(I) /= ZERO) THEN        !calculated in CRIT24>FR() SCLE3=-/+1 -1:elastic
          !---------------------------!
          !---      ELASTIC        ---!
          !---------------------------!          
          IF (SCLE3(I) < ZERO) THEN
            SIGC(I,1)=S01(I)
            SIGC(I,2)=S02(I)
            SIGC(I,3)=S03(I)
            SIGC(I,4)=S04(I)
            SIGC(I,5)=S05(I)
            SIGC(I,6)=S06(I)			 
          !---------------------------!
          !---      DAMAGE         ---!
          !---------------------------!           
          ELSEIF (SM(I) + SCLE2(I)*DSM(I) >= RT) THEN
            SIGC(I,1)=S01(I)
            SIGC(I,2)=S02(I)
            SIGC(I,3)=S03(I)
            SIGC(I,4)=S04(I)
            SIGC(I,5)=S05(I)
            SIGC(I,6)=S06(I)			 
            IF (DAM(I,1) == ZERO .or. DAM(I,2) == ZERO .or. DAM(I,3) == ZERO) THEN 
              I1 = I1 + 1
              INDEX1(I1) = I
            ENDIF             
          !---------------------------!
          !---      PLASTIC        ---!
          !---------------------------!           
          ELSEIF (STRAIN(I,1) + STRAIN(I,2) + STRAIN(I,3) > VMAX) THEN 
            I2 = I2 + 1
            INDEX2(I2) = I
          !---------------------------!
          !---      ELASTIC        ---!
          !---------------------------!                   
          ELSE        
            SIGC(I,1)=S01(I)
            SIGC(I,2)=S02(I)
            SIGC(I,3)=S03(I)
            SIGC(I,4)=S04(I)
            SIGC(I,5)=S05(I)
            SIGC(I,6)=S06(I)
          ENDIF
        ENDIF
      ENDDO
C
c---------------------------------------------------------------
      IF (I1 > 0) THEN
        CALL DAMA24(NEL   ,I1    ,INDEX1,NGL   ,PM    ,SCLE2 ,                         
     .              SIGC  ,DAM   ,ANG   ,EPS_F ,CRAK  ,CDAM  ,
     .              S01   ,S02   ,S03   ,S04   ,S05   ,S06   ,
     .              DEPS1 ,DEPS2 ,DEPS3 ,DEPS4 ,DEPS5 ,DEPS6 ,   
     .              DE1   ,DE2   ,DE3   ,SCAL1 ,SCAL2 ,SCAL3 )               
      ENDIF                  
c---------------------------------------------------------------
      IF (I2 > 0) THEN
        IF (ICAP < 2) THEN
          CALL PLAS24(NEL   ,I2    ,INDEX2,NGL   ,PM    ,         
     .                SIGC  ,DAM   ,CRAK  ,                       
     .                RHO   ,EINT  ,VK0   ,VK    ,ROB   ,CDAM  ,  
     .                DEPS1 ,DEPS2 ,DEPS3 ,DEPS4 ,DEPS5 ,DEPS6 ,  
     .                S1    ,S2    ,S3    ,S4    ,S5    ,S6    ,  
     .                SCAL1 ,SCAL2 ,SCAL3 ,SCLE2 )                
        ELSE
          CALL PLAS24B(NEL   ,I2    ,INDEX2,NGL   ,PM    ,                
     .                 SIGC  ,DAM   ,CRAK  ,EPSVP ,CDAM  ,         
     .                 RHO   ,EINT  ,VK0   ,VK    ,ROB   ,PLA   ,        
     .                 DEPS1 ,DEPS2 ,DEPS3 ,DEPS4 ,DEPS5 ,DEPS6 ,  
     .                 S1    ,S2    ,S3    ,S4    ,S5    ,S6    ,  
     .                 SCAL1 ,SCAL2 ,SCAL3 ,SCLE2 )                
        ENDIF                  
      ENDIF                  
c-----
      DO I=LFT,LLT
        SIG(I,1)=SIGC(I,1)
        SIG(I,2)=SIGC(I,2)
        SIG(I,3)=SIGC(I,3)
        SIG(I,4)=SIGC(I,4)
        SIG(I,5)=SIGC(I,5)
        SIG(I,6)=SIGC(I,6)
      ENDDO
c-----
      DAMSUM(1:NEL) = DAM(1:NEL,1) + DAM(1:NEL,2) + DAM(1:NEL,3)
c-----
C . . . . . . . . . . . . . .
C     CALCUL DU OFF
C . . . . . . . . . . . . .
      DO I=LFT,LLT
        IF(OFF(I) < EM01) OFF(I)=ZERO
        IF(OFF(I) < ONE)   OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO
C
      I1 = 0
      IF(N2D.NE.1) THEN
        DO I=LFT,LLT
          IF (OFF(I).GE.ONE) THEN
            ETEST= MAX(CRAK(I,1),CRAK(I,2),CRAK(I,3))
            IF(ETEST.GE.EPSMAX)THEN
              OFF(I)=OFF(I)*FOUR_OVER_5
              I1 = I1 + 1
              INDEX1(I1) = I
            ENDIF
          ENDIF
        ENDDO
      ELSE
        DO I=LFT,LLT
          IF(OFF(I).GE.ONE) THEN
            PLUS=STRAIN(I,1)+STRAIN(I,2)
            DIFF=STRAIN(I,1)-STRAIN(I,2)
            ETEST=HALF*(PLUS+SQRT(DIFF**2+STRAIN(I,4)**2))
            IF (ETEST.GE.EPSMAX)THEN
              OFF(I) = OFF(I)*FOUR_OVER_5
              I1 = I1 + 1
              INDEX1(I1) = I
            ENDIF
          ENDIF        
        ENDDO
      ENDIF
c-----------------------
      IF (I1.NE.0) THEN
        IDEL7NOK = 1
        DO J = 1, I1
          I = INDEX1(J)
#include "lockon.inc"
            WRITE(IOUT,1000) NGL(I)
            WRITE(ISTDO,1000)NGL(I)
#include "lockoff.inc"
        ENDDO
      ENDIF         
c-----------------------
c     RETOUR REPERE ARMATURE
c-----------------------
      NBDAMA = 0
      DO I=LFT,LLT
        IF (DAMSUM(I) > ZERO) THEN
          NBDAMA = NBDAMA + 1
          DAMAI(NBDAMA)=I
        ENDIF
      ENDDO
c
      CALL UDAM24N(SIG,ANG,NBDAMA,DAMAI,NEL)
c
c     ADDITION DES FORCES ARMATURES ET RETOUR REPERE GLOBAL
      IF (ARMA > ZERO) THEN
        DO I=LFT,LLT
          SIG(I,1) = SIG(I,1) * (ONE - ARM1(I)) + ARM1(I)*SIGA(I,1)
          SIG(I,2) = SIG(I,2) * (ONE - ARM2(I)) + ARM2(I)*SIGA(I,2)
          SIG(I,3) = SIG(I,3) * (ONE - ARM3(I)) + ARM3(I)*SIGA(I,3)
        ENDDO
      ENDIF
c-------------------------
      CALL AGLO24(NEL,SIG,R11,R12,R13,
     .            R21,R22,R23,R31,R32,R33)
c-------------------------
      DO I=LFT,LLT
        SIG(I,1)=SIG(I,1)*OFF(I)
        SIG(I,2)=SIG(I,2)*OFF(I)
        SIG(I,3)=SIG(I,3)*OFF(I)
        SIG(I,4)=SIG(I,4)*OFF(I)
        SIG(I,5)=SIG(I,5)*OFF(I)
        SIG(I,6)=SIG(I,6)*OFF(I)
      ENDDO
c-----------
 1000 FORMAT(1X,'*** TOTAL FAILURE ELEMENT #',I10)
c-----------
      RETURN
      END
