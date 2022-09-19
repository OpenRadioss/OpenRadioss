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
Chd|  AIRBAGA                       source/airbag/airbag1.F       
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE AIRBAGA(IVOLU ,NJET   ,IBAGJET ,NVENT   ,IBAGHOL,
     2                 ICBAG   ,RVOLU   ,RBAGJET ,RBAGHOL,RCBAG   ,
     3                 RVOLUV  ,NPC     ,TF      ,SENSOR  ,LSENSOR ,
     4                 VOL     ,PMAIN )
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
#include      "couple_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),LSENSOR(NSENSI,*),IVOLU(*),NJET,IBAGJET(NIBJET,*),
     .        NVENT,IBAGHOL(NIBHOL,*),ICBAG(NICBAG,*),PMAIN
C     REAL
      my_real
     .   TF(*),SENSOR(NSENSR,*),RVOLU(*),RBAGJET(NRBJET,*),
     .   RBAGHOL(NRBHOL,*),RCBAG(NRCBAG,*),RVOLUV(NRVOLU,*),VOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IMASS, ITEMP, ISENS, K, IFLU, IEQUI,
     .        IDEF, NAV, II, IS, IDTPDEF,IDPDEF,
     .        IV, IJ,ITTF
C     REAL
      my_real
     .   PDEF, PEXT, PVOIS, DTPDEFC,
     .   GAMA, AMU, TSTART, TSG,
     .   AMTOT, P, RO, DTX, POLD, DV,
     .   ROT, DYDX, FTEMP, FMASS, DT, AREA,
     .   CV, CP, CVG, CPG, CPA, CPB, CPC, CVI, CPI, CPAI, CPBI, CPCI,
     .   RMWI, RMWG,
     .   RNM_OLD, RNM, RNMG,
     .   GMTOT, GMTOT_OLD, GMASS, GMASS_OLD, DGMASS, DGMOUT,
     .   GMI, GMI_OLD,
     .   EFAC, DGEOUT, RIGHT, LEFT,
     .   VOLD, VEPS, VMIN, AMTOT_OLD, TBAG_OLD, TBAG,SCALT,
     .   QOLD,Q,CX,QX,TEMP
      my_real FINTER
      EXTERNAL FINTER
C-----------------------------------------------
      PEXT     =RVOLU(3)
      POLD     =RVOLU(12)
      SCALT    =RVOLU(26)
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
        DO IV=1,NVENT
          RBAGHOL(19,IV)= RBAGHOL(19,IV)
     .                           +RBAGHOL(21,IV)*DT1
          RBAGHOL(20,IV)= RBAGHOL(20,IV)
     .                           +RBAGHOL(22,IV)*DT1
        ENDDO
      END IF
C     AIRBAG COMMUNIQUANTS
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
      AMU      =RVOLU(2)
C
      IEQUI=IVOLU(15)
C       changement d'etat ?
        IS = 0
      ITTF=IVOLU(17)
        DO IJ=1,NJET
          ISENS=IBAGJET(4,IJ)
          IF(ISENS==0)THEN
           TSTART=ZERO
          ELSE
           TSTART=SENSOR(2,ISENS)
          ENDIF
          IF(TT>=TSTART)IS=1
        IF(IEQUI==0)THEN
          DGMOUT=RBAGJET(9,IJ)
          IF(DGMOUT/=0.)IS=1
        END IF
        IF (IS==1.AND.(ITTF==1.OR.ITTF==2.OR.ITTF==3)) THEN
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
C
            VMIN     =EM4*AREA**THREE_HALF
            VEPS     =MAX(ZERO,VMIN-VOL)
            RVOLU(17)=VEPS
C
C           equilibre initial.
            RMWI     = RVOLU(10)
            GMI      = PEXT*(VOL+VEPS)/(RMWI*TBAG_OLD)
            RVOLU(11)= GMI
            RVOLU(14)= RMWI*GMI
            RVOLU(20)= GMI
            IF(IMACH/=3.OR.ISPMD+1==PMAIN) THEN
         WRITE(IOUT,*)
     .   ' *** MONITORED VOLUME : INITIAL EQUILIBRIUM IS SET ***'
         WRITE(IOUT,'(A,I10,A,G20.13,A)')
     .   ' *** MONITORED VOLUME ',IVOLU(1),' VOLUME ',VOL,' ***'
         WRITE(ISTDO,*)
     .   ' *** MONITORED VOLUME : INITIAL EQUILIBRIUM IS SET ***'
            ENDIF
            IVOLU(15)= -1
C
            VOL      =VOL + VEPS
            VOLD     =VOL
            QOLD     =ZERO
          ELSE
            VOLD     =RVOLU(16)
            VEPS     =RVOLU(17)
            QOLD     =RVOLU(23)
            VOL      = VOL + VEPS
          END IF
          ROT      =RVOLU(21)
          DV       =VOL-VOLD
        END IF
      END IF
C---------------------
C
      IF (IS/=0)THEN
C---------------------
C      FRACTION MOLAIRE
C              inclus caracteristiques du gaz a l'initial.
C---------------------
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
        FMASS=RBAGJET(5,IJ)
        FTEMP=RBAGJET(6,IJ)
        IMASS=IBAGJET(1,IJ)
        IFLU =IBAGJET(2,IJ)
        ITEMP=IBAGJET(3,IJ)
C
        ISENS=IBAGJET(4,IJ)
        IF(ISENS==0)THEN
         TSTART=ZERO
        ELSE
         TSTART=SENSOR(2,ISENS)
        ENDIF
C
        RMWG=RBAGJET(1,IJ)
        CPA =RBAGJET(2,IJ)
        CPB =RBAGJET(3,IJ)
        CPC =RBAGJET(4,IJ)
C
        IF(TT>=TSTART)THEN
         TSG = (TT-TSTART)*RVOLU(26)
C
         IF(ITEMP>0) THEN
           TEMP=FTEMP*FINTER(ITEMP,TSG,NPC,TF,DYDX)
         ELSE
           TEMP=FTEMP
         ENDIF
         EFAC= TEMP*(CPA+HALF*CPB*TEMP+THIRD*CPC*TEMP*TEMP)
        ELSE
        ENDIF
C
        GMTOT_OLD=RBAGJET(8,IJ)
        GMASS_OLD=RBAGJET(7,IJ)
        IF(TT>=TSTART)THEN
         IF(IMASS>0) THEN
           GMASS=FMASS*FINTER(IMASS,TSG,NPC,TF,DYDX)
           IF(IFLU==1)GMASS = GMASS*RVOLU(26)*DT1 + GMASS_OLD
         ELSE
           GMASS=FMASS
         ENDIF
         DGMASS=MAX(ZERO,GMASS-GMASS_OLD)
         RIGHT =RIGHT+DGMASS*EFAC
        ELSE
         DGMASS=ZERO
         GMASS =ZERO
        ENDIF
C
        DGMOUT=RBAGJET( 9,IJ)*DT1
        GMTOT =GMTOT_OLD+DGMASS-DGMOUT
        DGEOUT=RBAGJET(10,IJ)*DT1
C
        RIGHT= RIGHT-DGEOUT
        RIGHT= RIGHT+(DGMOUT-DGMASS)*TBAG_OLD*
     .                 (CPA+HALF*CPB*TBAG_OLD
     .                     +THIRD*CPC*TBAG_OLD*TBAG_OLD-RMWG)
C
        CVG  = CPA+CPB*TBAG_OLD+CPC*TBAG_OLD*TBAG_OLD-RMWG
        LEFT = LEFT +GMTOT*CVG
        RIGHT= RIGHT+GMTOT*CVG*TBAG_OLD
C
        RBAGJET(8,IJ)=GMTOT
        RBAGJET(7,IJ)=GMASS
        AMTOT=AMTOT+GMTOT-GMTOT_OLD
C
       ENDDO
C
C---------------------
C      contribution du GAZ A L'INITIAL...
C---------------------
C
       DGMOUT=RVOLU(24)*DT1
C
       CPAI   =RVOLU(7)
       CPBI   =RVOLU(8)
       CPCI   =RVOLU(9)
       RMWI   =RVOLU(10)
       GMI_OLD=RVOLU(11)
       CVI    = CPAI+CPBI*TBAG_OLD+CPCI*TBAG_OLD*TBAG_OLD-RMWI
       GMI=GMI_OLD-DGMOUT
       DGEOUT=RVOLU(22)*DT1
C
       RIGHT=RIGHT+DGMOUT*TBAG_OLD*
     .             (CPAI+HALF*CPBI*TBAG_OLD
     .                  +THIRD*CPCI*TBAG_OLD*TBAG_OLD-RMWI)
     .            -DGEOUT
       LEFT  = LEFT+GMI*CVI
       RIGHT = RIGHT+GMI*CVI*TBAG_OLD
C
       AMTOT=AMTOT-DGMOUT
       RVOLU(11)=GMI
C---------------------
C      variation de TEMPERATURE et PRESSION
C---------------------
       GMI =RVOLU(11)
       CPAI=RVOLU(7)
       CPBI=RVOLU(8)
       CPCI=RVOLU(9)
       RMWI=RVOLU(10)
       RNM =GMI*RMWI
       DO IJ=1,NJET
        RMWG =RBAGJET(1,IJ)
        GMTOT=RBAGJET(8,IJ)
        RNMG =GMTOT*RMWG
        RNM  =RNM+RNMG
       ENDDO
C---------------------
       LEFT  = LEFT + HALF*RNM*DV/VOL
       RIGHT = RIGHT- HALF*RNM_OLD*TBAG_OLD*DV/VOLD
C---------------------
       TBAG  = RIGHT/LEFT
       TBAG  = MAX(TBAG,ZERO)
       P=RNM*TBAG/VOL
C---------------------
C      GAMA DU MELANGE (inclus caracteristiques du gaz a l'initial).
C---------------------
       CPI=CPAI+CPBI*TBAG+CPCI*TBAG*TBAG
       CVI=CPI-RMWI
       CP =GMI*CPI
       CV =GMI*CVI
       DO IJ=1,NJET
        RMWG =RBAGJET(1,IJ)
        CPA  =RBAGJET(2,IJ)
        CPB  =RBAGJET(3,IJ)
        CPC  =RBAGJET(4,IJ)
        GMTOT=RBAGJET(8,IJ)
        CPG  =CPA+CPB*TBAG+CPC*TBAG*TBAG
        CVG  =CPG-RMWG
        CP   =CP+GMTOT*CPG
        CV   =CV+GMTOT*CVG
       ENDDO
C      CP =CP/AMTOT
C      CV =CV/AMTOT
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
       ENDIF
C------
       RVOLU(12)=P
       RVOLU(13)=TBAG
       RVOLU(16)=VOL
       RVOLU(20)=AMTOT
       RVOLU(23)=Q
C------
       RVOLU(22)=ZERO
       RVOLU(24)=ZERO
       DO IJ=1,NJET
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
