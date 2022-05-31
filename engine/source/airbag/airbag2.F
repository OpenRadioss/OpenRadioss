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
Chd|  AIRBAGB                       source/airbag/airbag2.F       
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE AIRBAGB(
     1                 IVOLU   ,ICBAG   ,NJET    ,IBAGJET ,NVENT   ,
     2                 IBAGHOL ,RVOLU   ,RVOLUV  ,RCBAG   ,RBAGJET ,
     3                 RBAGHOL ,FSAV    ,N       ,NN      ,
     4                 IGRSURF ,PORO    ,IVOLUV  ,RBAGVJET,
     4                 FR_MV   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C      STRUCTURES AIRBAG, INPUT STARTER 4.4
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(*),ICBAG(NICBAG,*),NJET,IBAGJET(NIBJET,*),
     .        NVENT,IBAGHOL(NIBHOL,*),
     .        NN,IVOLUV(NIMV,*),FR_MV(*)
C     REAL ou REAL*8
      my_real
     .   RVOLU(*), RVOLUV(NRVOLU,*),RCBAG(NRCBAG,*),PORO(*),
     .   RBAGJET(NRBJET,*),RBAGHOL(NRBHOL,*),FSAV(*),N(3,*),RBAGVJET(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II, NAV, K, IDEF, KK, IPVENT, NNC, IAD,
     .   IPORT,IPORP,IPORA,IPORT1,IPORP1,IPORA1,IVDP,
     .   IJ ,IV, RADVOIS,PMAIN,
     .   IN1,IN2,IN3,IN4,ITTF,IDTPDEF
C     REAL ou REAL*8
      my_real
     .   GAMA, CV, CP, PEXT, PDEF, DTPDEFI, DTPDEFC, TVENT, TSTOPE,
     .   APVENT, AVENT, BVENT, 
     .   AMTOT, P, RO, VOL, HSPEC, 
     .   GMTOT, CPA, CPB, CPC, GMI, CPAI, CPBI, CPCI, TBAG,
     .   U, DEOUT, DMOUT, AREA, PCRIT, PVOIS, TVOIS, AA, VEPS,
     .   AOUT, AOUT1, AOUTOT, FLOUT, DE, VVOIS, 
     .   DGEOUT, DGMOUT, RNM, RMWI, RNMI, RMWG, RNMG, 
     .   DERI, TEMP(4),AISENT,ACHEMK,FCHEMK,VMAX,
     .   FPORT,FPORP,FPORA,FPORT1,FPORP1,FPORA1,SCALT,SCALP,SCALS,
     .   FVDP, ROEX, UISENT,TT1,
     .   F1(NN), F2(NN),TTF
      my_real GET_U_FUNC
      EXTERNAL GET_U_FUNC
      DOUBLE PRECISION 
     .   FRMV6(2,6), FRMV6B(6)
C-----------------------------------------------
       PMAIN = FR_MV(NSPMD+2)
       NAV   = IVOLU(3)
       ITTF  = IVOLU(17)
       TTF    =RVOLU(60)
       GAMA   =RVOLU(1)
       PEXT   =RVOLU(3)
       VOL    =RVOLU(16)
       VEPS   =RVOLU(17)
       TBAG   =RVOLU(13)
       AMTOT  =RVOLU(20)
       P      =RVOLU(12)
       AREA   =RVOLU(18)
       SCALT  =RVOLU(26)   
       SCALP  =RVOLU(27)   
       SCALS  =RVOLU(28)   
C
       RO    = AMTOT/VOL
       PCRIT = P*(TWO/(GAMA+ONE))**(GAMA/(GAMA-ONE))
C
C      contribution du gaz a l'initial.
       CPAI=RVOLU(7)
       CPBI=RVOLU(8)
       CPCI=RVOLU(9)
       GMI =RVOLU(11)
       HSPEC=GMI*TBAG*(CPAI+HALF*CPBI*TBAG+THIRD*CPCI*TBAG*TBAG)
C
       DO IJ=1,NJET
        CPA  = RBAGJET(2,IJ)
        CPB  = RBAGJET(3,IJ)
        CPC  = RBAGJET(4,IJ)
        GMTOT= RBAGJET(8,IJ)
       HSPEC=HSPEC+GMTOT*TBAG*(CPA+HALF*CPB*TBAG+ THIRD*CPC*TBAG*TBAG)
       ENDDO
       HSPEC=HSPEC/MAX(EM20,AMTOT)
C--------------------------------
C      FLUX SORTANT PAR LES TROUS
C--------------------------------
       AISENT=ZERO
       ACHEMK=ZERO
       FCHEMK=ZERO
       DO IV=1,NVENT
        IDEF   = IBAGHOL(1,IV)
        IPVENT = IBAGHOL(2,IV)
        IDTPDEF =IBAGHOL(11,IV)
C
        PDEF   = RBAGHOL(1,IV)
        DTPDEFI= RBAGHOL(4,IV)
        DTPDEFC= RBAGHOL(5,IV)
        AVENT  = RBAGHOL(2,IV)
        TVENT  = RBAGHOL(3,IV)
        BVENT  = RBAGHOL(6,IV)
        TSTOPE = RBAGHOL(14,IV)
        RBAGHOL(16,IV)=ZERO
        RBAGHOL(17,IV)=ZERO
        RBAGHOL(18,IV)=ZERO
        RBAGHOL(21,IV)=ZERO
        RBAGHOL(22,IV)=ZERO
C
        IF(ITTF.EQ.11.OR.ITTF.EQ.12.OR.ITTF.EQ.13) THEN
        IF(IDEF.EQ.0.AND.P.GT.PDEF+PEXT.
     .               AND.DTPDEFC.GT.DTPDEFI.
     .               AND.VOL.GT.EM3*AREA**THREE_HALF.
     .               AND.TT.LT.TSTOPE+TTF
     .              .AND.IDTPDEF.EQ.0) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
          WRITE(IOUT,*)' *** AIRBAG VENT HOLES MEMBRANE IS DEFLATED ***'
          WRITE(IOUT,*)' *** MONITORED VOLUME ',IVOLU(1),
     .                 ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENT HOLES MEMBRANE IS DEFLATED ***'
         ENDIF
        ENDIF
        IF(IDEF.EQ.0.AND.DTPDEFC.GT.DTPDEFI.
     .               AND.TT.LT.TSTOPE+TTF
     .              .AND.IDTPDEF.EQ.1) THEN
         IDEF=1
          WRITE(IOUT,*)' *** AIRBAG VENT HOLES MEMBRANE IS DEFLATED ***'
          WRITE(IOUT,*)' *** MONITORED VOLUME ',IVOLU(1),
     .                 ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENT HOLES MEMBRANE IS DEFLATED ***'
        ENDIF
        IF(IDEF.EQ.0 .AND. TT.GT.TVENT+TTF
     .               .AND. TT.LT.TSTOPE+TTF) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
          WRITE(IOUT,*) ' *** AIRBAG VENTING STARTS ***'
          WRITE(IOUT,*) ' *** MONITORED VOLUME ',IVOLU(1),
     .                ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENTING STARTS ***'
         ENDIF
        ENDIF
        IF(IDEF.EQ.1 .AND. TT.GE.TSTOPE+TTF) THEN
         IDEF=0
         IF(IMACH.NE.3.OR.ISPMD+1.EQ.PMAIN) THEN
          WRITE(IOUT,*) ' *** AIRBAG VENTING STOPS ***'
          WRITE(IOUT,*) ' *** MONITORED VOLUME ',IVOLU(1),
     .                ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENTING STOPS ***'
         END IF
        END IF
        ELSE IF(ITTF.EQ.0) THEN
        IF(IDEF.EQ.0.AND.P.GT.PDEF+PEXT.
     .               AND.DTPDEFC.GT.DTPDEFI.
     .               AND.VOL.GT.EM3*AREA**THREE_HALF.
     .               AND.TT.LT.TSTOPE
     .              .AND.IDTPDEF.EQ.0) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
          WRITE(IOUT,*)' *** AIRBAG VENT HOLES MEMBRANE IS DEFLATED ***'
          WRITE(IOUT,*)' *** MONITORED VOLUME ',IVOLU(1),
     .                 ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENT HOLES MEMBRANE IS DEFLATED ***'
         ENDIF
        ENDIF
        IF(IDEF.EQ.0.AND.DTPDEFC.GT.DTPDEFI.
     .               AND.TT.LT.TSTOPE
     .              .AND.IDTPDEF.EQ.1) THEN
         IDEF=1
          WRITE(IOUT,*)' *** AIRBAG VENT HOLES MEMBRANE IS DEFLATED ***'
          WRITE(IOUT,*)' *** MONITORED VOLUME ',IVOLU(1),
     .                 ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENT HOLES MEMBRANE IS DEFLATED ***'
        ENDIF
        IF(IDEF.EQ.0 .AND. TT.GT.TVENT
     .               .AND. TT.LT.TSTOPE) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
          WRITE(IOUT,*) ' *** AIRBAG VENTING STARTS ***'
          WRITE(IOUT,*) ' *** MONITORED VOLUME ',IVOLU(1),
     .                ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENTING STARTS ***'
         ENDIF
        ENDIF
        IF(IDEF.EQ.1 .AND. TT.GE.TSTOPE) THEN
         IDEF=0
         IF(IMACH.NE.3.OR.ISPMD+1.EQ.PMAIN) THEN
          WRITE(IOUT,*) ' *** AIRBAG VENTING STOPS ***'
          WRITE(IOUT,*) ' *** MONITORED VOLUME ',IVOLU(1),
     .                ' VENT HOLES MEMBRANE NUMBER ',IV,' ***'
          WRITE(ISTDO,*)' *** VENTING STOPS ***'
         END IF
        END IF
        END IF
C
        IF(IPVENT.NE.0)THEN
           IF(INTBAG.EQ.0)THEN
             NNC=IGRSURF(IPVENT)%NSEG
             DO KK=1,NNC
               IF(IGRSURF(IPVENT)%ELTYP(KK).EQ.3)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK)
               ELSEIF(IGRSURF(IPVENT)%ELTYP(KK).EQ.7)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC
               ELSE
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC + NUMELTG
               ENDIF
               F1(KK) = SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
               F2(KK) = ZERO
             ENDDO
           ELSE
             NNC=IGRSURF(IPVENT)%NSEG
             DO KK=1,NNC
               IF(IGRSURF(IPVENT)%ELTYP(KK).EQ.3)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK)
               ELSEIF(IGRSURF(IPVENT)%ELTYP(KK).EQ.7)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC
               ELSE
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC + NUMELTG
               ENDIF
               AA   = SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
               F1(KK) = (ONE - PORO(K))*AA
               F2(KK) =  PORO(K)*AA
             ENDDO
           ENDIF
C
C somme parith/on
C
           DO K = 1, 6
             FRMV6(1,K) = ZERO
             FRMV6(2,K) = ZERO
           END DO
           CALL SUM_6_FLOAT(1, NNC, F1, FRMV6(1,1),2)
           CALL SUM_6_FLOAT(1, NNC, F2, FRMV6(2,1),2)
C comm si necessaire
           IF(NSPMD > 1) THEN
             CALL SPMD_EXCH_FR6(FR_MV,FRMV6,2*6)
           ENDIF
C
           AOUT  = FRMV6(1,1)+FRMV6(1,2)+FRMV6(1,3)+
     .             FRMV6(1,4)+FRMV6(1,5)+FRMV6(1,6)
           AOUT1 = FRMV6(2,1)+FRMV6(2,2)+FRMV6(2,3)+
     .             FRMV6(2,4)+FRMV6(2,5)+FRMV6(2,6)
        ELSE
           AOUT =AVENT
           AVENT=ONE 
           AOUT1=ZERO
        ENDIF
C
        IF(IDEF.EQ.1 .AND. P.GT.PEXT.
     .               AND.VOL.GT.EM3*AREA**THREE_HALF)THEN
         IPORT =IBAGHOL(3,IV)
         IPORP =IBAGHOL(4,IV)
         IPORA =IBAGHOL(5,IV)
         IPORT1=IBAGHOL(6,IV)
         IPORP1=IBAGHOL(7,IV)
         IPORA1=IBAGHOL(8,IV)
         FPORT = RBAGHOL(7,IV)      
         FPORP = RBAGHOL(8,IV)      
         FPORA = RBAGHOL(9,IV)      
         FPORT1= RBAGHOL(10,IV)     
         FPORP1= RBAGHOL(11,IV)     
         FPORA1= RBAGHOL(12,IV)     
         IF(IPORA.NE.0.AND.IPVENT.NE.0)THEN
           AOUT=FPORA*AVENT*GET_U_FUNC(IPORA,AOUT*SCALS,DERI)
         ELSE
           AOUT=AVENT*AOUT
         ENDIF
         TT1=TT
         IF (ITTF.EQ.12) TT1=TT-TTF
         IF (ITTF.EQ.13) TT1=TT-TTF-TVENT
         IF(IPORT.NE.0)AOUT=FPORT*AOUT*
     .                 GET_U_FUNC(IPORT,TT1*SCALT,DERI)
         IF(IPORP.NE.0)AOUT=FPORP*AOUT*
     .                 GET_U_FUNC(IPORP,(P-PEXT)*SCALP,DERI)
         IF(IPORA1.NE.0.AND.IPVENT.NE.0)THEN
           AOUT1=FPORA*BVENT*GET_U_FUNC(IPORA1,AOUT1*SCALS,DERI)
         ELSE
           AOUT1=BVENT*AOUT1
         ENDIF
         IF(IPORT1.NE.0)AOUT1=FPORT*AOUT1*
     .                  GET_U_FUNC(IPORT1,TT1*SCALT,DERI)
         IF(IPORP1.NE.0)AOUT1=FPORP*AOUT1*
     .                  GET_U_FUNC(IPORP1,(P-PEXT)*SCALP,DERI)
         IF(ISPMD+1.EQ.PMAIN) THEN
           RBAGHOL(16,IV)=AOUT
           RBAGHOL(17,IV)=AOUT1
         END IF
         IVDP=IBAGHOL(9,IV)
         IF(IVDP.EQ.0)THEN
          AISENT=AISENT+AOUT+AOUT1
         ELSE
          ACHEMK=ACHEMK+AOUT+AOUT1
          FVDP=RBAGHOL(13,IV)
          U=FVDP*GET_U_FUNC(IVDP,(P-PEXT)*SCALP,DERI)
          FCHEMK= FCHEMK+(AOUT+AOUT1)*U
          IF(IMACH.NE.3.OR.ISPMD+1.EQ.PMAIN)
     .      RBAGHOL(18,IV)=U
         ENDIF
        ENDIF
        IBAGHOL(1,IV)=IDEF
       ENDDO
C-------
       AOUTOT=AISENT+ACHEMK
       UISENT=ZERO
       FLOUT =ZERO
       DMOUT =ZERO
       IF(AOUTOT.GT.ZERO)THEN
        ROEX =RO*(PEXT/P)**(ONE/GAMA)
        VMAX  =HALF*(P-PEXT)*VOL/(GAMA-ONE)
     .  /MAX(EM20,HSPEC*(ROEX*AISENT+RO*ACHEMK)*DT1)
        VMAX  =MIN(VMAX,HALF*VOL/MAX(EM20,AOUTOT*DT1))
C
        IF(AISENT.GT.ZERO)THEN
         PEXT = MAX(PEXT,PCRIT)
         ROEX =RO*(PEXT/P)**(ONE/GAMA)
         UISENT=TWO*GAMA/(GAMA-ONE)*P/RO*(ONE-(PEXT/P)**((GAMA-ONE)/GAMA))
         UISENT=MAX(UISENT,ZERO)
         UISENT=SQRT(UISENT)
         UISENT=MIN(UISENT,VMAX)
         FLOUT=AISENT*UISENT
         DMOUT=FLOUT*ROEX
        ENDIF
C
        IF(ACHEMK.GT.ZERO)THEN
         FCHEMK=MIN(FCHEMK,VMAX*ACHEMK)
         FLOUT =FLOUT +FCHEMK
         DMOUT =DMOUT +RO*FCHEMK
        ENDIF
        IF(ISPMD+1.EQ.PMAIN)THEN
          DO IV=1,NVENT
            IDEF=IBAGHOL(1,IV)
            IVDP=IBAGHOL(9,IV)
            IF(IDEF.EQ.1)THEN
              IF(IVDP.EQ.0)THEN
                RBAGHOL(18,IV)=UISENT
                RBAGHOL(21,IV)= ROEX*UISENT
     .                         *(RBAGHOL(16,IV)+RBAGHOL(17,IV))
                RBAGHOL(22,IV)=RBAGHOL(21,IV)*HSPEC
              ELSE
                RBAGHOL(18,IV)=MIN(RBAGHOL(18,IV),VMAX)
                RBAGHOL(21,IV)= RO*RBAGHOL(18,IV)
     .                         *(RBAGHOL(16,IV)+RBAGHOL(17,IV))
                RBAGHOL(22,IV)=RBAGHOL(21,IV)*HSPEC
              END IF
            END IF
          END DO
        END IF
       ENDIF
C---------------------------------------------
       RNM  =RVOLU(14)
       CV   =RNM/AMTOT/(GAMA-ONE)
       CP   =GAMA*CV
       IF(ISPMD+1.EQ.PMAIN) THEN
         FSAV(1) =AMTOT
         FSAV(2) =VOL
         FSAV(3) =P
         FSAV(4) =AREA
         FSAV(5) =TBAG
         FSAV(6) =AOUTOT
         FSAV(7) =FLOUT/MAX(EM20,AOUTOT)
         FSAV(8)=ZERO
         FSAV(9)=ZERO
         FSAV(10)=CP
         FSAV(11)=CV
         FSAV(12)=GAMA
         FSAV(15)=ZERO
         DO IJ=1,NJET
            FSAV(15)=FSAV(15)+RBAGJET(7,IJ)
         ENDDO
       ENDIF
C---------------------------------------------
C      MASSE et TRAVAIL par GAZ
C---------------------------------------------
       RMWI=RVOLU(10)
       RNMI=GMI*RMWI
C      VOLG/VOL=fraction molaire=RNMG/RNM
       DGMOUT=RNMI/MAX(EM20,RNM)*DMOUT
       DGEOUT=DGMOUT*TBAG*(CPAI+HALF*CPBI*TBAG+THIRD*CPCI*TBAG*TBAG)
       RVOLU(22)=RVOLU(22) + DGEOUT
       RVOLU(24)=RVOLU(24) + DGMOUT
C------
       DO IJ=1,NJET
         CPA  = RBAGJET(2,IJ)
         CPB  = RBAGJET(3,IJ)
         CPC  = RBAGJET(4,IJ)
         GMTOT =RBAGJET(8,IJ)
         RMWG  =RBAGJET(1,IJ)
         RNMG  =GMTOT*RMWG
         DGMOUT=RNMG/MAX(EM20,RNM)*DMOUT
         DGEOUT=DGMOUT*TBAG*(CPA+HALF*CPB*TBAG+THIRD*CPC*TBAG*TBAG)
         RBAGJET( 9,IJ)=RBAGJET( 9,IJ)+DGMOUT
         RBAGJET(10,IJ)=RBAGJET(10,IJ)+DGEOUT
       ENDDO
C---------------------------------------------
C      AIRBAG COMMUNIQUANTS
C---------------------------------------------
       DO I=1,NAV
         II     = ICBAG(1,I)
         IPVENT = ICBAG(2,I)
         IDEF   = ICBAG(3,I)
         IPORT  = ICBAG(4,I)
         IPORP  = ICBAG(5,I)
         PDEF   = RCBAG(1,I)
         AVENT  = RCBAG(2,I)
         TVENT  = RCBAG(3,I)
         DTPDEFI= RCBAG(4,I)
         DTPDEFC= RCBAG(5,I)
         FPORT  = RCBAG(6,I)
         FPORP  = RCBAG(7,I)
         PVOIS=RVOLUV(12,II)
         VVOIS=RVOLUV(16,II)
         IF(ITTF==0.OR.ITTF==11.OR.ITTF==12.OR.ITTF==13)THEN
           IF(IDEF.EQ.0.AND.P.GT.PDEF+PVOIS
     .               .AND.DTPDEFC.GT.DTPDEFI
     .               .AND.VOL.GT.EM3*AREA**THREE_HALF)THEN
             IDEF=1
             IF(ISPMD+1.EQ.PMAIN) THEN
               WRITE(IOUT,*) 
     .          ' *** CHAMBER VENT HOLES MEMBRANE IS DEFLATED ***'
               WRITE(IOUT,*)
     .          ' *** MONITORED VOLUME ',IVOLU(1),' ***'
               WRITE(ISTDO,*)
     .          ' *** CHAMBER VENT HOLES MEMBRANE IS DEFLATED ***'
             ENDIF
           ENDIF
           IF(IDEF.EQ.0 .AND. TT.GT.TVENT+TTF) THEN
             IDEF=1
             IF(ISPMD+1.EQ.PMAIN) THEN
               WRITE(IOUT,*) ' *** CHAMBER COMMUNICATION STARTS ***'
               WRITE(IOUT,*) ' *** MONITORED VOLUME ',IVOLU(1),' ***'
               WRITE(ISTDO,*)' *** COMMUNICATION STARTS ***'
             ENDIF
           ENDIF
         ENDIF
C 
         IF(IPVENT.NE.0)THEN
             NNC=IGRSURF(IPVENT)%NSEG
             DO KK=1,NNC
               IF(IGRSURF(IPVENT)%ELTYP(KK).EQ.3)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK)
               ELSEIF(IGRSURF(IPVENT)%ELTYP(KK).EQ.7)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC
               ELSE
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC + NUMELTG
               ENDIF
               F1(KK) = SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
             ENDDO
C
C Sommation p/on
C
             DO K = 1, 6
               FRMV6B(K) = ZERO
             END DO
             CALL SUM_6_FLOAT(1, NNC, F1, FRMV6B, 1)
C comm si necessaire
             IF(NSPMD > 1) THEN
               CALL SPMD_EXCH_FR6(FR_MV,FRMV6B,6)
             ENDIF
             APVENT = FRMV6B(1)+FRMV6B(2)+FRMV6B(3)+
     .                FRMV6B(4)+FRMV6B(5)+FRMV6B(6)             
         ELSE
             APVENT = ONE
         ENDIF
C
         AOUT=AVENT*APVENT
C
         IF(IDEF.EQ.1 .AND. P.GT.PVOIS.
     .                 AND.VOL.GT.EM3*AREA**THREE_HALF)THEN
           PVOIS = MAX(PVOIS,PCRIT)
           U=TWO*GAMA/(GAMA-ONE)*P/RO*(ONE-(PVOIS/P)**((GAMA-ONE)/GAMA))
           U=SQRT(U)
           U=MIN(U,HALF*VOL/MAX(EM20,AOUT*DT1))
           DE=RO*(PVOIS/P)**(ONE/GAMA)*HSPEC
           U=MIN(U,(P-PVOIS)*HALF*MIN(VOL,VVOIS)
     .      /(GAMA-ONE)/DE/MAX(EM20,AOUT*DT1))
           FLOUT=AOUT*U
           DMOUT=FLOUT*RO*(PVOIS/P)**(ONE/GAMA)
         ELSE
           DMOUT=ZERO
           FLOUT=ZERO
         ENDIF
         ICBAG(3,I) = IDEF
C---------------------------------------------
C        MASSE et TRAVAIL par GAZ
C---------------------------------------------
C        VOLG/VOL=fraction molaire=RNMG/RNM
         DGMOUT=RNMI/MAX(EM20,RNM)*DMOUT
         DGEOUT=DGMOUT*TBAG*(CPAI+HALF*CPBI*TBAG+THIRD*CPCI*TBAG*TBAG)
C OUT
         RVOLU(22)=RVOLU(22) + DGEOUT
         RVOLU(24)=RVOLU(24) + DGMOUT
C IN
         RVOLUV(22,II)=RVOLUV(22,II) - DGEOUT
         RVOLUV(24,II)=RVOLUV(24,II) - DGMOUT
C------
         RADVOIS= IVOLUV(10,II)
         DO IJ=1,NJET
           GMTOT =RBAGJET(8,IJ)
           RMWG  =RBAGJET(1,IJ)
           RNMG  =GMTOT*RMWG
           DGMOUT=RNMG/MAX(EM20,RNM)*DMOUT
           CPA  = RBAGJET(2,IJ)
           CPB  = RBAGJET(3,IJ)
           CPC  = RBAGJET(4,IJ)
           DGEOUT=DGMOUT*TBAG*(CPA+HALF*CPB*TBAG+THIRD*CPC*TBAG*TBAG)
C OUT
           RBAGJET(9,IJ)=
     .         RBAGJET(9,IJ)+DGMOUT
           RBAGJET(10,IJ)=
     .         RBAGJET(10,IJ)+DGEOUT
C IN
           RBAGVJET(RADVOIS+NRBJET*(IJ-1)+9)=
     .       RBAGVJET(RADVOIS+NRBJET*(IJ-1)+9)-DGMOUT
           RBAGVJET(RADVOIS+NRBJET*(IJ-1)+10)=
     .       RBAGVJET(RADVOIS+NRBJET*(IJ-1)+10)-DGEOUT
C
         ENDDO
         IF(ISPMD+1.EQ.PMAIN) THEN
           FSAV(8)=FSAV(8)+AOUT
           FSAV(9)=FSAV(9)+FLOUT
         ENDIF
      ENDDO
      IF(ISPMD+1.EQ.PMAIN) THEN
        FSAV(9)=FSAV(9)/MAX(EM20,FSAV(8))
      ENDIF
C---------------------------------------------
      RETURN
      END
