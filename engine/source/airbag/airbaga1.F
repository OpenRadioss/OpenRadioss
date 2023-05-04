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
Chd|  AIRBAGA1                      source/airbag/airbaga1.F      
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE AIRBAGA1(IVOLU  ,NJET    ,IBAGJET ,NVENT   ,IBAGHOL ,
     2                    ICBAG  ,RVOLU   ,RBAGJET ,RBAGHOL ,RCBAG   ,
     3                    RVOLUV ,NPC     ,TF      ,NSENSOR ,SENSOR_TAB ,
     4                    VOL    ,PMAIN ,GEO     ,IGEO    ,PM      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*),IVOLU(*),NJET,IBAGJET(NIBJET,*),
     .        NVENT,IBAGHOL(NIBHOL,*),ICBAG(NICBAG,*),PMAIN,
     .        IGEO(NPROPGI,*)
C     REAL
      my_real
     .   TF(*),RVOLU(*),RBAGJET(NRBJET,*),
     .   RBAGHOL(NRBHOL,*),RCBAG(NRCBAG,*),RVOLUV(NRVOLU,*),VOL,
     .   GEO(NPROPG,*),PM(NPROPM,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IMASS, ITEMP, ISENS, K, IFLU, IEQUI,
     .        IDEF, NAV, II, IS, IDTPDEF,IDPDEF,
     .        IV, IJ,ITTF
      INTEGER IK, KK, I_GAS, I_INJ, IMOL, NGASES, I_TYPINJ
      my_real
     .   PDEF, PEXT, PVOIS, DTPDEFC,
     .   GAMA, AMU, TSTART, TSG,TSG2,
     .   AMTOT, P, RO, DTX, POLD, DV,
     .   ROT, DYDX, FTEMP, FMASS, DT, AREA,
     .   CV, CP, CVG, CPG, CPA, CPB, CPC, CVI, CPI, CPAI, CPBI, CPCI,
     .   RMWI, RMWG,
     .   RNM_OLD, RNM, RNMG,
     .   GMTOT, GMTOT_OLD, GMASS, GMASS_OLD, DGMASS, DGMOUT,
     .   GMI, GMI_OLD,
     .   EFAC, DGEOUT, RIGHT, LEFT, 
     .   VOLD, VEPS, VMIN, AMTOT_OLD, TBAG_OLD, TBAG,
     .   QOLD,Q,CX,QX,TEMP
      my_real
     .   MW, CPD, CPE, CPF, CPDI, CPEI, CPFI, R_IGC1, DGMIN, ASTIME,
     .   DGEIN, HCONV, TEXT, MOLFR, MW_MIX
      my_real FINTER 
      EXTERNAL FINTER
C-----------------------------------------------
      PEXT     =RVOLU(3)
      POLD     =RVOLU(12)
      R_IGC1   =PM(27,IVOLU(66))
C------------------------
      DO IV=1,NVENT
        IDEF   = IBAGHOL(1,IV)
        PDEF   = RBAGHOL(1,IV)
        DTPDEFC= RBAGHOL(5,IV)
        IDTPDEF= IBAGHOL(11,IV)
        IDPDEF = IBAGHOL(12,IV)
C
        IF (IDTPDEF==0) THEN
          IF(IDEF==0.AND.POLD>PDEF+PEXT) THEN
            DTPDEFC = DTPDEFC+DT1
          END IF
        ELSE IF (IDTPDEF==1) THEN
          IF (POLD>PDEF+PEXT) THEN
            IDPDEF = 1
          END IF
          IF (IDPDEF==1) THEN
            DTPDEFC = DTPDEFC+DT1
          END IF
        END IF
        IBAGHOL(12,IV) = IDPDEF 
        RBAGHOL(5,IV)  = DTPDEFC
      ENDDO
      IF(IMACH/=3.OR.ISPMD+1==PMAIN) THEN
C Mass loss & Enthalpy Loss
        DO IV=1,NVENT
          RBAGHOL(19,IV)= RBAGHOL(19,IV) + RBAGHOL(21,IV)*DT1
          RBAGHOL(20,IV)= RBAGHOL(20,IV) + RBAGHOL(22,IV)*DT1
        ENDDO
C Heat Loss
        RVOLU(54)=RVOLU(54)+RVOLU(55)*DT1
      END IF
C-------------------------
C     AIRBAG COMMUNIQUANTS
C-------------------------
      NAV   = IVOLU(3)
      DO I=1,NAV
         II     = ICBAG(1,I)
         IDEF   = ICBAG(3,I)
         PDEF   = RCBAG(1,I)
         DTPDEFC= RCBAG(5,I)
c         PVOIS  =RVOLUV(12,II)
c           afin d'eviter probleme de meme adressage memoire
c           de RVOLU(12) et RVOLUV(12) on utilise RVOLU(31)
c           qui etait dispo
         PVOIS  =RVOLUV(31,II)
         IF(IDEF==0.AND.POLD>PDEF+PVOIS)
     .      RCBAG(5,I)=DTPDEFC+DT1
      ENDDO
C---------------------
      AMU = RVOLU(2)
C
      IEQUI=IVOLU(15)
C changement d'etat ?
      IS = 0 
      ITTF=IVOLU(17)
      DO IJ=1,NJET
        ISENS=IBAGJET(4,IJ)
        IF(ISENS==0)THEN
           TSTART=ZERO
        ELSE
           TSTART=SENSOR_TAB(ISENS)%TSTART
        ENDIF
        IF(TT>=TSTART)IS=1
        IF(IEQUI==0)THEN
          DGMOUT=RBAGJET(9,IJ)
          IF(DGMOUT/=0.)IS=1
        END IF
        IF(IS==1.AND.(ITTF==1.OR.ITTF==2.OR.ITTF==3)) THEN
          ITTF=ITTF+10
C store in rvolu(60) airbag time to fire
          RVOLU(60)=TSTART
          IVOLU(17)=ITTF
        END IF
      ENDDO
      IF(IEQUI==0)THEN
        TBAG_OLD =RVOLU(13)
        VOLD     =RVOLU(16)
        VEPS     =RVOLU(17)
        AREA     =RVOLU(18)
        ROT      =RVOLU(21)
        QOLD     =RVOLU(23)
        VOL      = VOL + VEPS
C
        DGMOUT=RVOLU(24)
        IF(DGMOUT/=ZERO)IS=1
C
        DV = VOL-VOLD
        IF(DV/=ZERO)IS=1
      ELSE
        IF(IS==1)THEN
          TBAG_OLD =RVOLU(13)
          AREA     =RVOLU(18)
          IF(IEQUI==1)THEN
            VMIN     =EM4*AREA**THREE_HALF
            VEPS     =MAX(ZERO,VMIN-VOL)
            RVOLU(17)=VEPS
            RMWI     = RVOLU(10)
            GMI      = PEXT*(VOL+VEPS)/(RMWI*TBAG_OLD)
            RVOLU(11)= GMI
            RVOLU(14)= RMWI*GMI
            RVOLU(20)= GMI
            IF(IMACH/=3.OR.ISPMD+1==PMAIN) THEN
              WRITE(IOUT,*)
     .        ' *** MONITORED VOLUME : INITIAL EQUILIBRIUM IS SET ***'
              WRITE(IOUT,'(A,I10,A,G20.13,A)') 
     .        ' *** MONITORED VOLUME ',IVOLU(1),' VOLUME ',VOL,' ***'
              WRITE(ISTDO,*)
     .        ' *** MONITORED VOLUME : INITIAL EQUILIBRIUM IS SET ***'
            ENDIF
            IVOLU(15)= -1
C
            VOL  =VOL + VEPS
            VOLD =VOL
            QOLD =ZERO
          ELSE
            VOLD =RVOLU(16)
            VEPS =RVOLU(17)
            QOLD =RVOLU(23)
            VOL  = VOL + VEPS
          ENDIF
          ROT  =RVOLU(21)
          DV   =VOL-VOLD
        ENDIF
      ENDIF
C--------------------------
      DGMIN =RVOLU(64)*DT1
      IF(DGMIN > ZERO) IS=1
C
      IF (IS/=0)THEN
C-----------------------------
C     MASSE MOLAIRE DU MELANGE
C-----------------------------
       DO IJ=1,NJET
         I_INJ    = IBAGJET(13,IJ)
         IF(I_INJ <= 0) CYCLE
         I_TYPINJ = IGEO(22,I_INJ)
         IF(I_TYPINJ /= 2) CYCLE
         ISENS=IBAGJET(4,IJ)
         IF(ISENS==0)THEN
          TSTART=ZERO
         ELSE
          TSTART=SENSOR_TAB(ISENS)%TSTART
         ENDIF
         IF(TT < TSTART) CYCLE
C
         ASTIME = GEO(201,I_INJ)
         TSG = (TT - TSTART) / ASTIME
         NGASES = IGEO(23,I_INJ)
         MW_MIX = ZERO
C
         DO IK=1,NGASES
            MOLFR = GEO(211+(IK-1)+1,I_INJ)
            I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
            IMOL  = IGEO(100+(IK-1)*2+2,I_INJ)
            MW    = PM(20,I_GAS)
            IF(IMOL == 0) THEN
               MW_MIX = MW_MIX + MW*MOLFR
            ELSE
               MW_MIX = MW_MIX + MW*FINTER(IMOL,TSG,NPC,TF,DYDX)
            ENDIF
         ENDDO
         RBAGJET(19,IJ) = MW_MIX
       ENDDO
C---------------------------------------------------------
C      FRACTION MOLAIRE
C              inclus caracteristiques du gaz a l'initial.
C---------------------------------------------------------
C      RNM=R*NM=R*M/MW
       RNM_OLD  =RVOLU(14)
       AMTOT_OLD=RVOLU(20)
C
       LEFT =ZERO
       RIGHT=ZERO
       AMTOT=AMTOT_OLD
C---------------------
C     INJECTEURS
C---------------------
       DO IJ=1,NJET
        I_INJ    = IBAGJET(13,IJ)
        IF(I_INJ <= 0) CYCLE
C
        I_TYPINJ = IGEO(22,I_INJ)
        NGASES   = IGEO(23,I_INJ)
        IFLU     = IGEO(24,I_INJ)
        ASTIME   = GEO(201,I_INJ)
C
        ISENS=IBAGJET(4,IJ)
        IF(ISENS==0)THEN
         TSTART=ZERO
        ELSE
         TSTART=SENSOR_TAB(ISENS)%TSTART
        ENDIF
C
        DO IK=1,NGASES
         IF (I_TYPINJ==1) THEN
           FMASS = GEO(208+(IK-1)*2+1,I_INJ)
           FTEMP = GEO(208+(IK-1)*2+2,I_INJ)
           I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
           IMASS = IGEO(100+(IK-1)*3+2,I_INJ)
           ITEMP = IGEO(100+(IK-1)*3+3,I_INJ)
         ELSE IF (I_TYPINJ==2) THEN
           FMASS = GEO(209,I_INJ)
           FTEMP = GEO(210,I_INJ)
           MOLFR = GEO(211+(IK-1)+1,I_INJ)
           IMASS = IGEO(25,I_INJ)
           ITEMP = IGEO(26,I_INJ)
           I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
           IMOL  = IGEO(100+(IK-1)*2+2,I_INJ)
         END IF
C
         MW   =PM(20,I_GAS)
         CPA  =PM(21,I_GAS)
         CPB  =PM(22,I_GAS)
         CPC  =PM(23,I_GAS)
         CPD  =PM(24,I_GAS)
         CPE  =PM(25,I_GAS)
         CPF  =PM(26,I_GAS)
C
         IF(TT>=TSTART)THEN
          TSG = (TT - TSTART) / ASTIME
          TSG2 = (TT - DT1 - TSTART) / ASTIME
          IF (TSG2 <= ZERO) TSG2 = TSG
C
          IF(ITEMP>0) THEN
            TEMP=FTEMP*FINTER(ITEMP,TSG,NPC,TF,DYDX)
          ELSE
            TEMP=FTEMP
          ENDIF
          EFAC= TEMP*(CPA+HALF*CPB*TEMP+THIRD*CPC*TEMP*TEMP
     .                +FOURTH*CPD*TEMP*TEMP*TEMP
     .                -CPE/(TEMP*TEMP)
     .                +ONE_FIFTH*CPF*TEMP*TEMP*TEMP*TEMP)
         ELSE
          EFAC=ZERO
         ENDIF
C
         KK=20+(IK-1)*4
         GMTOT_OLD=RBAGJET(KK+1,IJ)
         GMASS_OLD=RBAGJET(KK+2,IJ)
         IF(TT>=TSTART)THEN
          IF(IMASS>0) THEN
            GMASS=HALF * FMASS * (FINTER(IMASS,TSG,NPC,TF,DYDX) + 
     .            FINTER(IMASS,TSG2,NPC,TF,DYDX))
            IF(I_TYPINJ == 2) THEN
               MW_MIX = RBAGJET(19,IJ)
               IF(IMOL == 0) THEN
                 GMASS=GMASS*MW*MOLFR/MW_MIX
               ELSE
                 GMASS=GMASS*MW*FINTER(IMOL,TSG,NPC,TF,DYDX)/MW_MIX
               ENDIF
            ENDIF
            IF(IFLU==1)GMASS = GMASS*DT1 + GMASS_OLD
            DGMASS=MAX(ZERO,GMASS-GMASS_OLD)
            RIGHT =RIGHT+DGMASS*EFAC
            RBAGJET(11,IJ) = RBAGJET(11,IJ)+DGMASS*EFAC
          ELSE
            GMASS=ZERO
            DGMASS=ZERO
          ENDIF
         ELSE
          DGMASS=ZERO
          GMASS =ZERO
         ENDIF
C
         RMWG = R_IGC1/MW
         GMTOT = GMTOT_OLD + DGMASS
         RIGHT = RIGHT -DGMASS*TBAG_OLD*
     .                 (CPA+HALF*CPB*TBAG_OLD
     .                  +THIRD*CPC*TBAG_OLD*TBAG_OLD
     .                  +FOURTH*CPD*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .                  -CPE/(TBAG_OLD*TBAG_OLD)
     .                  +ONE_FIFTH*CPF*TBAG_OLD*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .                  -RMWG)
         KK=20+(IK-1)*4
         RBAGJET(KK+1,IJ) = GMTOT
         RBAGJET(KK+2,IJ) = GMASS
         AMTOT = AMTOT + DGMASS
        ENDDO      !fin de boucle gaz
       ENDDO       !fin de boucle injecteurs
C-------------------------
C FUITES ET COMMUNICATIONS
C-------------------------
       DO IJ=1,NJET
         I_INJ    = IABS(IBAGJET(13,IJ))
         I_TYPINJ = IGEO(22,I_INJ)
         NGASES   = IGEO(23,I_INJ)
         DO IK=1,NGASES
           IF (I_TYPINJ==1) THEN
             I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
           ELSE IF (I_TYPINJ==2) THEN
             I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
           END IF

           MW   =PM(20,I_GAS)
           CPA  =PM(21,I_GAS)
           CPB  =PM(22,I_GAS)
           CPC  =PM(23,I_GAS)
           CPD  =PM(24,I_GAS)
           CPE  =PM(25,I_GAS)
           CPF  =PM(26,I_GAS)
           RMWG = R_IGC1/MW

           KK=20+(IK-1)*4
           DGMOUT = RBAGJET(KK+3,IJ)*DT1
           DGEOUT = RBAGJET(KK+4,IJ)*DT1
C
           RIGHT = RIGHT -DGEOUT
           RIGHT = RIGHT +DGMOUT*TBAG_OLD*
     .                   (CPA+HALF*CPB*TBAG_OLD
     .                   +THIRD*CPC*TBAG_OLD*TBAG_OLD
     .                   +FOURTH*CPD*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .                   -CPE/(TBAG_OLD*TBAG_OLD)
     .                   +ONE_FIFTH*CPF*TBAG_OLD*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .                   -RMWG)
           CVG  =  CPA+CPB*TBAG_OLD+CPC*TBAG_OLD*TBAG_OLD
     .            +CPD*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .            +CPE/(TBAG_OLD*TBAG_OLD)
     .            +CPF*TBAG_OLD*TBAG_OLD*TBAG_OLD*TBAG_OLD-RMWG

           GMTOT = RBAGJET(KK+1,IJ)-DGMOUT
           LEFT  = LEFT +GMTOT*CVG
           RIGHT = RIGHT+GMTOT*CVG*TBAG_OLD

           RBAGJET(KK+1,IJ) = GMTOT
           AMTOT = AMTOT - DGMOUT
         ENDDO     !fin de boucle gaz
       ENDDO       !fin de boucle injecteurs
C------------------------------------
C      CONTRIBUTION DU GAZ INITIAL...
C------------------------------------
       DGMOUT=RVOLU(24)*DT1
       DGMIN =RVOLU(64)*DT1
C
       CPAI   =RVOLU(7)
       CPBI   =RVOLU(8)
       CPCI   =RVOLU(9)
       CPDI   =RVOLU(56)
       CPEI   =RVOLU(57)
       CPFI   =RVOLU(58)
       RMWI   =RVOLU(10)
       GMI_OLD=RVOLU(11)
       CVI    = CPAI+CPBI*TBAG_OLD+CPCI*TBAG_OLD*TBAG_OLD
     .          +CPDI*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .          +CPEI/(TBAG_OLD*TBAG_OLD)
     .          +CPFI*TBAG_OLD*TBAG_OLD*TBAG_OLD*TBAG_OLD-RMWI
       GMI=GMI_OLD-DGMOUT+DGMIN

       DGEOUT=RVOLU(22)*DT1
       DGEIN =RVOLU(65)*DT1
C
       RIGHT = RIGHT+(DGMOUT-DGMIN)*TBAG_OLD
     .              *(CPAI+HALF*CPBI*TBAG_OLD
     .                +THIRD*CPCI*TBAG_OLD*TBAG_OLD
     .                +FOURTH*CPDI*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .                -CPEI/(TBAG_OLD*TBAG_OLD)
     .                +ONE_FIFTH*CPFI*TBAG_OLD*TBAG_OLD*TBAG_OLD*TBAG_OLD
     .                -RMWI)
       RIGHT = RIGHT+DGEIN-DGEOUT
       LEFT  = LEFT+GMI*CVI
       RIGHT = RIGHT+GMI*CVI*TBAG_OLD
C
       AMTOT = AMTOT-DGMOUT+DGMIN
       RVOLU(11)=GMI
C------------------------------------------
C      VARIATION DE TEMPERATURE et PRESSION
C------------------------------------------
       GMI =RVOLU(11)
       RNM =GMI*RMWI
       DO IJ=1,NJET
         I_INJ = IABS(IBAGJET(13,IJ))
         I_TYPINJ = IGEO(22,I_INJ)
         NGASES   = IGEO(23,I_INJ)
         DO IK=1,NGASES
          IF (I_TYPINJ==1) THEN
            I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
          ELSE IF (I_TYPINJ==2) THEN
            I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
          END IF
          MW   = PM(20,I_GAS)
          RMWG = R_IGC1/MW
          KK=20+(IK-1)*4
          GMTOT= RBAGJET(KK+1,IJ)
          RNMG = GMTOT*RMWG
          RNM  = RNM+RNMG
         ENDDO
       ENDDO
C---------------------
       HCONV = RVOLU(19)
       TEXT  = RVOLU(25)
       LEFT  = LEFT + HALF*(RNM*DV/VOL+HCONV*AREA*DT1)
       RIGHT = RIGHT- HALF*TBAG_OLD*(RNM_OLD*DV/VOLD+HCONV*AREA*DT1)
     .              + HCONV*AREA*TEXT*DT1
C---------------------------------------------------------------------
       TBAG  = RIGHT/LEFT
       TBAG  = MAX(TBAG,ZERO)
       P=RNM*TBAG/VOL
C--------------------
C RATE OF HEAT LOSS
C--------------------
       RVOLU(55) = HCONV*AREA*MAX((TBAG-TEXT),ZERO)
C-------------------------------------------------------------------
C      GAMA DU MELANGE (inclus caracteristiques du gaz a l'initial).
C-------------------------------------------------------------------
       CPI=CPAI+CPBI*TBAG+CPCI*TBAG*TBAG
     .     +CPDI*TBAG*TBAG*TBAG
     .     +CPEI/(TBAG*TBAG)
     .     +CPFI*TBAG*TBAG*TBAG*TBAG
       CVI=CPI-RMWI
       CP =GMI*CPI
       CV =GMI*CVI
       DO IJ=1,NJET
         I_INJ = IABS(IBAGJET(13,IJ))
         I_TYPINJ = IGEO(22,I_INJ)
         NGASES   = IGEO(23,I_INJ)
         DO IK=1,NGASES
          IF (I_TYPINJ==1) THEN
            I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
          ELSE IF (I_TYPINJ==2) THEN
            I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
          END IF
          MW   = PM(20,I_GAS)
          RMWG = R_IGC1/MW
          CPA  =PM(21,I_GAS)
          CPB  =PM(22,I_GAS)
          CPC  =PM(23,I_GAS)
          CPD  =PM(24,I_GAS)
          CPE  =PM(25,I_GAS)
          CPF  =PM(26,I_GAS)
          CPG  =CPA+CPB*TBAG+CPC*TBAG*TBAG
     .          +CPD*TBAG*TBAG*TBAG
     .          +CPE/(TBAG*TBAG)
     .          +CPF*TBAG*TBAG*TBAG*TBAG
          CVG  =CPG-RMWG
          KK=20+(IK-1)*4
          GMTOT= RBAGJET(KK+1,IJ)
          CP   =CP+GMTOT*CPG
          CV   =CV+GMTOT*CVG
         ENDDO
       ENDDO
C       CP =CP/AMTOT
C       CV =CV/AMTOT
       GAMA=CP/CV
C------
       RVOLU(1) =GAMA
       RVOLU(14)=RNM
C------
       IF(DT1==ZERO.OR.DV>ZERO)THEN
        Q=ZERO
       ELSE
        Q=-AMU*SQRT(P*AREA*ROT/VOL)*DV/AREA/DT1
       ENDIF
       IF (IMACH/=3.OR.ISPMD+1==PMAIN) THEN
        TFEXT=TFEXT+(HALF*(P+POLD+Q+QOLD)-PEXT)*DV
        RVOLU(32)=RVOLU(32) + (HALF*(P+POLD)-PEXT)*DV
       ENDIF
C------
       RVOLU(12)=P
       RVOLU(13)=TBAG
       RVOLU(16)=VOL
       RVOLU(20)=AMTOT
       RVOLU(23)=Q

       RVOLU(22)=ZERO
       RVOLU(24)=ZERO
       DO IJ=1,NJET
         I_INJ = IABS(IBAGJET(13,IJ))
         NGASES   = IGEO(23,I_INJ)
         DO IK=1,NGASES
            KK=20+(IK-1)*4
            RBAGJET(KK+3,IJ)=ZERO
            RBAGJET(KK+4,IJ)=ZERO
         ENDDO
         RBAGJET( 9,IJ)=ZERO
         RBAGJET(10,IJ)=ZERO
       ENDDO
C------
C      DP/DRO GAZ PARFAIT+ADIABATIQUE=GAMA*R/MW*TBAG
       CX = SQRT(2.*GAMA*P*VOL/(GAMA-1.)/(AMTOT+AREA*ROT))
       QX = AMU*CX
       DT = DTFAC1(9)*VOL/AREA/MAX(EM20,QX+SQRT(QX*QX+CX*CX))
       IF(DT<DT2)THEN
          DT2=DT
          NELTS =IVOLU(1)
          ITYPTS=9
       ENDIF
       IF(IDTMIN(9)==1.AND.DT<DTMIN1(9)) THEN
         TSTOP = TT
         IF (IMACH/=3.OR.ISPMD+1==PMAIN) THEN
           WRITE(IOUT,*)  '-- MINIMUM MONITORED VOLUME TIME STEP '
           WRITE(ISTDO,*) '-- MINIMUM MONITORED VOLUME TIME STEP '
         ENDIF
       ELSEIF(IDTMIN(9)==5.AND.DT<DTMIN1(9)) THEN
         MSTOP = 2
         IF (IMACH/=3.OR.ISPMD+1==PMAIN) THEN
           WRITE(IOUT,*)  '-- MINIMUM MONITORED VOLUME TIME STEP '
           WRITE(ISTDO,*) '-- MINIMUM MONITORED VOLUME TIME STEP '
         ENDIF
       ENDIF
      ENDIF
C
      RETURN
      END
