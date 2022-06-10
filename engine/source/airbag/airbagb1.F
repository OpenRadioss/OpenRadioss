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
Chd|  AIRBAGB1                      source/airbag/airbagb1.F      
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        PORFOR4                       source/airbag/porfor4.F       
Chd|        PORFOR5                       source/airbag/porfor5.F       
Chd|        PORFOR6                       source/airbag/porfor6.F       
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE AIRBAGB1(
     1                 IVOLU   ,ICBAG   ,NJET     ,IBAGJET ,NVENT   , 
     2                 IBAGHOL ,RVOLU   ,RVOLUV   ,RCBAG   ,RBAGJET , 
     3                 RBAGHOL ,FSAV    ,N        ,NN      , 
     4                 IGRSURF ,PORO    ,IVOLUV   ,RBAGVJET,
     5                 FR_MV   ,IPARG   ,IPART    ,IPARTC  ,IPARTTG , 
     6                 IPM     ,PM      ,ELBUF_TAB,IGROUPC ,IGROUPTG,
     7                 IGEO    ,GEO     )
C-----------------------------------------------
C      STRUCTURES AIRBAG
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(*),ICBAG(NICBAG,*),NJET,IBAGJET(NIBJET,*),
     .        NVENT,IBAGHOL(NIBHOL,*),
     .        NN,IVOLUV(NIMV,*),FR_MV(*),IPARG(NPARG,*),
     .        IPART(LIPART1,*),IPM(NPROPMI,*),
     .        IPARTC(NUMELC) ,IPARTTG(NUMELTG),
     .        IGROUPC(*),IGROUPTG(*)    
      INTEGER IGEO(NPROPGI,*)
C     REAL
      my_real
     .   RVOLU(*), RVOLUV(NRVOLU,*),RCBAG(NRCBAG,*),PORO(*),
     .   RBAGJET(NRBJET,*),RBAGHOL(NRBHOL,*),FSAV(*),N(3,*),RBAGVJET(*),
     .   GEO(NPROPG,*), PM(NPROPM,*)
C
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II, NAV, K,NFT,IEL, IDEF, KK, IPVENT, NNC,
     .   IPORT,IPORP,IPORA,IPORT1,IPORP1,IPORA1,IVDP,
     .   IJ ,IV, RADVOIS,PMAIN,
     .   IN1,IN2,IN3,IN4,ITTF,IDTPDEF,
     .   IOFF,NG,IM,IFVENT,NFUNC,ITY,IAD,MTN,
     .   IVENTYP,ILEAKAGE,IBLOCKAGE
      INTEGER NEL,TITREVENT(20)
      INTEGER IK, I_INJ, I_TYPINJ, I_GAS, NGASES
C     REAL
      my_real
     .   GAMA, CV, CP, PEXT, PDEF, DTPDEFI, DTPDEFC, TVENT, TSTOPE,
     .   APVENT, AVENT, BVENT, 
     .   AMTOT, P, RO, VOL, HSPEC, 
     .   GMTOT, CPA, CPB, CPC, GMI, CPAI, CPBI, CPCI, TBAG,
     .   CPD, CPE, CPF, CPDI, CPEI, CPFI,
     .   U, DEOUT, DMOUT, AREA, PCRIT, PVOIS, TVOIS, AA, VEPS,
     .   AOUT, AOUT1, AOUTOT, FLOUT, DE, VVOIS, 
     .   DGEOUT, DGMOUT, RNM, RMWI, RNMI, RMWG, RNMG, 
     .   DERI, TEMP, AISENT, ACHEMK, FCHEMK, VMAX,
     .   FPORT,FPORP,FPORA,FPORT1,FPORP1,FPORA1,SCALT,SCALP,SCALS,
     .   FVDP, ROEX, UISENT, TT1,
     .   F1(NN), F2(NN), TTF, SVTFAC, FLC, FAC, FACP,
     .   AISENT1, DGMIN, DGEIN, GAMAI, RHOI, RHO2, P2, ETA, 
     .   PCRIT1, HSPEC1
      my_real
     .   MW, R_IGC1, TOUT
      my_real GET_U_FUNC
      EXTERNAL GET_U_FUNC
      CHARACTER*20 VENTTITLE
      DOUBLE PRECISION 
     .   FRMV6(2,6), FRMV6B(6)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C 
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
C
       SCALT  =RVOLU(26)   
       SCALP  =RVOLU(27)   
       SCALS  =RVOLU(28)   
C
       R_IGC1= PM(27,IVOLU(66))
       RO    = AMTOT/VOL
       PCRIT = P*(TWO/(GAMA+ONE))**(GAMA/(GAMA-ONE))
C---------------------------------
C contribution du gaz a l'initial.
C---------------------------------
       CPAI=RVOLU(7)
       CPBI=RVOLU(8)
       CPCI=RVOLU(9)
       CPDI=RVOLU(56)
       CPEI=RVOLU(57)
       CPFI=RVOLU(58)
       GMI =RVOLU(11)
       HSPEC=GMI*TBAG*(
     .       CPAI+HALF*CPBI*TBAG+THIRD*CPCI*TBAG*TBAG
     .       +FOURTH*CPDI*TBAG*TBAG*TBAG
     .       -CPEI/(TBAG*TBAG)
     .       +ONE_FIFTH*CPFI*TBAG*TBAG*TBAG*TBAG)
C
       DO IJ=1,NJET
         I_INJ = IABS(IBAGJET(13,IJ))
         I_TYPINJ = IGEO(22,I_INJ)
         NGASES   = IGEO(23,I_INJ)
         DO IK=1,NGASES
          IF (I_TYPINJ.EQ.1) THEN
            I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
          ELSE IF (I_TYPINJ.EQ.2) THEN
            I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
          END IF
          CPA  =PM(21,I_GAS)
          CPB  =PM(22,I_GAS)
          CPC  =PM(23,I_GAS)
          CPD  =PM(24,I_GAS)
          CPE  =PM(25,I_GAS)
          CPF  =PM(26,I_GAS)
          GMTOT= RBAGJET(20+(IK-1)*4+1,IJ)
          HSPEC= HSPEC+GMTOT*TBAG*
     .          (CPA+HALF*CPB*TBAG+THIRD*CPC*TBAG*TBAG
     .           +FOURTH*CPD*TBAG*TBAG*TBAG
     .           -CPE/(TBAG*TBAG)
     .           +ONE_FIFTH*CPF*TBAG*TBAG*TBAG*TBAG)
         ENDDO
       ENDDO
       HSPEC=HSPEC/MAX(EM20,AMTOT)
C--------------------------------
C      FLUX SORTANT PAR LES TROUS
C--------------------------------
       AISENT =ZERO
       AISENT1=ZERO
       ACHEMK=ZERO
       FCHEMK=ZERO
       DO IV=1,NVENT
        IDEF   = IBAGHOL(1,IV)
        IPVENT = IBAGHOL(2,IV)
        IFVENT = IBAGHOL(10,IV)
        IDTPDEF= IBAGHOL(11,IV)
        IVENTYP= IBAGHOL(13,IV)
        IBLOCKAGE= IBAGHOL(14,IV)
C
        PDEF   = RBAGHOL(1,IV)
        DTPDEFI= RBAGHOL(4,IV)
        DTPDEFC= RBAGHOL(5,IV)
        AVENT  = RBAGHOL(2,IV)
        TVENT  = RBAGHOL(3,IV)
        BVENT  = RBAGHOL(6,IV)
        TSTOPE = RBAGHOL(14,IV)
C
        RBAGHOL(16,IV)=ZERO
        RBAGHOL(17,IV)=ZERO
        RBAGHOL(18,IV)=ZERO
        RBAGHOL(21,IV)=ZERO
        RBAGHOL(22,IV)=ZERO
C
        DO K=1,20
          TITREVENT(K)=IBAGHOL(14+K,IV)
          VENTTITLE(K:K) = ACHAR(TITREVENT(K))
        ENDDO
C
        IF(ITTF.EQ.11.OR.ITTF.EQ.12.OR.ITTF.EQ.13) THEN
        IF(IDEF.EQ.0.AND.P.GT.PDEF+PEXT
     .              .AND.DTPDEFC.GT.DTPDEFI
     .              .AND.VOL.GT.EM3*AREA**THREE_HALF
     .              .AND.TT.LT.TSTOPE+TTF
     .              .AND.IDTPDEF.EQ.0) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
            WRITE(IOUT,'(A)') 
     .                  ' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') 
     .                  ' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
         ENDIF
        ENDIF
        IF(IDEF.EQ.0.AND.DTPDEFC.GT.DTPDEFI
     .              .AND.TT.LT.TSTOPE+TTF
     .              .AND.IDTPDEF.EQ.1) THEN
         IDEF=1
            WRITE(IOUT,'(A)') 
     .                  ' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') 
     .                  ' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
        ENDIF
        IF(IDEF.EQ.0 .AND. TT.GT.TVENT+TTF
     .               .AND. TT.LT.TSTOPE+TTF) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STARTS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STARTS ',VENTTITLE
         ENDIF
        ENDIF
        IF(IDEF.EQ.1 .AND. TT.GE.TSTOPE+TTF) THEN
         IDEF=0
         IF(IMACH.NE.3.OR.ISPMD+1.EQ.PMAIN) THEN
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STOPS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STOPS ',VENTTITLE
         END IF
        END IF
C
       ELSE IF(ITTF.EQ.0) THEN
        IF(IDEF.EQ.0.AND.P.GT.PDEF+PEXT.
     .               AND.DTPDEFC.GT.DTPDEFI.
     .               AND.VOL.GT.EM3*AREA**THREE_HALF.
     .               AND.TT.LT.TSTOPE
     .              .AND.IDTPDEF.EQ.0) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
            WRITE(IOUT,'(A)') 
     .                  ' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') 
     .                  ' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
         ENDIF
        ENDIF
        IF(IDEF.EQ.0.AND.DTPDEFC.GT.DTPDEFI.
     .               AND.TT.LT.TSTOPE
     .              .AND.IDTPDEF.EQ.1) THEN
          IDEF=1
            WRITE(IOUT,'(A)') 
     .                  ' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') 
     .                  ' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
        ENDIF
        IF(IDEF.EQ.0 .AND. TT.GT.TVENT
     .               .AND. TT.LT.TSTOPE) THEN
         IDEF=1
         IF(ISPMD+1.EQ.PMAIN) THEN
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STARTS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STARTS ',VENTTITLE
         ENDIF
        ENDIF
        IF(IDEF.EQ.1 .AND. TT.GE.TSTOPE) THEN
         IDEF=0
         IF(IMACH.NE.3.OR.ISPMD+1.EQ.PMAIN) THEN
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STOPS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IV   ,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STOPS ',VENTTITLE
         ENDIF
        ENDIF
       ENDIF
        IBAGHOL(1,IV)=IDEF
        IF(IDEF.EQ.0) CYCLE
C-----------------------------------
C  COMPUTE EFFECTIVE VENTING SURFACE
C-----------------------------------
        TT1=TT-TTF
        IF (ITTF.EQ.13) TT1=TT-TTF-TVENT
C--------------------------------
C VENTING AREA GIVEN BY A SURFACE
C--------------------------------
        IF(IPVENT.NE.0)THEN
           IF(IVENTYP.EQ.0)THEN
C-------------
C  VENT HOLES
C-------------
             NNC=IGRSURF(IPVENT)%NSEG
             DO KK=1,NNC
               IF(IGRSURF(IPVENT)%ELTYP(KK).EQ.3)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK)
               ELSEIF(IGRSURF(IPVENT)%ELTYP(KK).EQ.7)THEN
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC
               ELSE
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC + NUMELTG
               ENDIF
               AA = SQRT(N(1,K)**2+N(2,K)**2+N(3,K)**2)
               IF(INTBAG.EQ.0) THEN
                 F1(KK) = AA
                 F2(KK) = ZERO
               ELSE
                 F1(KK) = (ONE - PORO(K))*AA
                 F2(KK) =  PORO(K)*AA
               ENDIF
             ENDDO
           ELSE
C------------
C  POROSITY
C------------
             NNC=IGRSURF(IPVENT)%NSEG
C
             DO KK=1,NNC                                                  
               ITY=IGRSURF(IPVENT)%ELTYP(KK)
               K  =IGRSURF(IPVENT)%ELEM(KK)
               FACP=ZERO
               SVTFAC=ZERO
               IF(ITY.EQ.3)THEN
                 IM=IPART(1,IPARTC(K))
               ELSEIF(ITY.EQ.7)THEN
                 IM=IPART(1,IPARTTG(K))
               ELSE
                 GO TO 200
               ENDIF
               MTN   = IPM(2,IM)
               IF (MTN.NE.19.AND.MTN.NE.58) GOTO 200
C
               ILEAKAGE = IPM(4,IM)
               NFUNC    = IPM(10,IM)+IPM(6,IM)
               IF(ILEAKAGE.EQ.0) THEN
                  SVTFAC=ZERO
               ELSEIF(ILEAKAGE.EQ.1) THEN
                  FLC=PM(164,IM)
                  FAC=PM(165,IM)
                  SVTFAC=FLC*FAC
               ELSEIF(ILEAKAGE.EQ.2.OR.ILEAKAGE.EQ.3) THEN
                  FLC=ZERO
                  IPORT=IPM(10+NFUNC-1,IM)
                  IF(IPORT > 0) THEN
                    SCALT=PM(160,IM)
                    FPORT=PM(164,IM)
                    FLC=FPORT*GET_U_FUNC(IPORT,TT1*SCALT,DERI)
                  ENDIF
                  FAC=ZERO
                  IPORP=IPM(10+NFUNC-2,IM)
                  IF(IPORP > 0) THEN
                    SCALP=PM(161,IM)
                    FPORP=PM(165,IM)
                    IF(ILEAKAGE.EQ.2) THEN
                       FAC=FPORP*GET_U_FUNC(IPORP,P*SCALP,DERI)
                    ELSE
                       FAC=FPORP*GET_U_FUNC(IPORP,(P-PEXT)*SCALP,DERI)
                    ENDIF
                  ENDIF
                  SVTFAC=FLC*FAC
               ELSE          ! ILEAKAGE >= 4
                  IF(ITY.EQ.3)NG=IGROUPC(K)
                  IF(ITY.EQ.7)NG=IGROUPTG(K)
                  NEL = IPARG(2,NG)
                  NFT = IPARG(3,NG)
                  IEL = K-NFT
                  IF(ILEAKAGE.EQ.4) THEN
                    CALL PORFOR4(SVTFAC,IM,IPM,PM,
     .                           ELBUF_TAB(NG)%GBUF%STRA,P,PEXT,IEL,NEL)
                  ELSEIF(ILEAKAGE.EQ.5) THEN
                    CALL PORFOR5(SVTFAC,IM,IPM,PM,
     .                           ELBUF_TAB(NG),P,PEXT,IEL,NEL)
                  ELSEIF(ILEAKAGE.EQ.6) THEN
                    CALL PORFOR6(SVTFAC,IM,IPM,PM,
     .                           ELBUF_TAB(NG)%GBUF%STRA,P,PEXT,IEL,NEL)
                  ENDIF
               ENDIF
C
               FACP=PM(162,IM)
               IF(FACP == ZERO) THEN
                 IPORT=IPM(10+NFUNC,IM)
                 IF(IPORT > 0) THEN
                   SCALT=PM(160,IM)
                   FPORT=PM(163,IM)
                   FACP=FPORT*GET_U_FUNC(IPORT,TT1*SCALT,DERI)
                 ENDIF
               ENDIF
C

 200           CONTINUE
               IF (ITY.EQ.7) K=K+NUMELC
               AA   = SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
               IF(INTBAG==0) THEN
                 F1(KK) = AA*SVTFAC
                 F2(KK) = ZERO
               ELSE
                 IF(IBLOCKAGE==1) THEN
                   F1(KK) = (ONE - PORO(K))*AA*SVTFAC
                   F2(KK) = ZERO
                 ELSE
                   F1(KK) = (ONE - PORO(K))*AA*SVTFAC
                   F2(KK) =  FACP*PORO(K) *AA*SVTFAC
                 ENDIF
               ENDIF
             ENDDO
           ENDIF
C----------------
C somme parith/on
C----------------
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
C---------------------------------
C VENTING THROUGH A CONSTANT AREA
C---------------------------------
           AOUT1=ZERO
           IF(IVENTYP.EQ.0) THEN
             AOUT =AVENT
             AVENT=ONE
           ELSE
             IF(AVENT.EQ.ZERO) THEN
               IPORA = IBAGHOL(5,IV)
               FPORA = RBAGHOL(9,IV)      
               AVENT=FPORA*GET_U_FUNC(IPORA,(P-PEXT)*SCALP,DERI)
             ENDIF
             IF(BVENT.EQ.ZERO) THEN
               IPORT = IBAGHOL(3,IV)
               FPORT = RBAGHOL(7,IV)      
               BVENT=FPORT*GET_U_FUNC(IPORT,TT1*SCALT,DERI)
             ENDIF
             AOUT=AVENT*BVENT
           ENDIF
        ENDIF
C
         IF(IVENTYP.EQ.0) THEN
C-------------
C VENT HOLES
C-------------
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
          IF(IPORT.NE.0)
     .      AOUT=FPORT*AOUT*GET_U_FUNC(IPORT,TT1*SCALT,DERI)
          IF(IPORP.NE.0)
     .      AOUT=FPORP*AOUT*GET_U_FUNC(IPORP,(P-PEXT)*SCALP,DERI)
          IF(IPORA1.NE.0.AND.IPVENT.NE.0)THEN
            AOUT1=FPORA1*BVENT*GET_U_FUNC(IPORA1,AOUT1*SCALS,DERI)
          ELSE
            AOUT1=BVENT*AOUT1
          ENDIF
          IF(IPORT1.NE.0)
     .      AOUT1=FPORT1*AOUT1*GET_U_FUNC(IPORT1,TT1*SCALT,DERI)
          IF(IPORP1.NE.0)
     .      AOUT1=FPORP1*AOUT1*GET_U_FUNC(IPORP1,(P-PEXT)*SCALP,DERI)
C
          IF(IFVENT==1)THEN
            AISENT=AISENT+AOUT+AOUT1
          ELSEIF(IFVENT==2) THEN
            ACHEMK=ACHEMK+AOUT+AOUT1
            IVDP=IBAGHOL(9,IV)
            FVDP=RBAGHOL(13,IV)
            U=FVDP*GET_U_FUNC(IVDP,(P-PEXT)*SCALP,DERI)
            FCHEMK= FCHEMK+(AOUT+AOUT1)*U
            IF(ISPMD+1.EQ.PMAIN) RBAGHOL(18,IV)=U
          ELSEIF(IFVENT==4) THEN
            AISENT1=AISENT1+AOUT+AOUT1
          ENDIF
         ELSE
C------------
C  POROSITY
C------------
          IF(IFVENT <= 1) THEN
            AISENT=AISENT+AOUT+AOUT1
          ELSEIF(IFVENT==2) THEN
            ACHEMK=ACHEMK+AOUT+AOUT1
            IVDP=IBAGHOL(9,IV)
            FVDP=RBAGHOL(13,IV)
            U=FVDP*GET_U_FUNC(IVDP,(P-PEXT)*SCALP,DERI)
            FCHEMK= FCHEMK+(AOUT+AOUT1)*U
            IF(ISPMD+1.EQ.PMAIN) RBAGHOL(18,IV)=U
          ELSEIF(IFVENT==3) THEN
            ACHEMK=ACHEMK+AOUT+AOUT1
            U=MAX(TWO*(P-PEXT)/RO,ZERO)
            U=SQRT(U)
            FCHEMK= FCHEMK+(AOUT+AOUT1)*U
            IF(ISPMD+1.EQ.PMAIN) RBAGHOL(18,IV)=U
          ENDIF
         ENDIF
C
        IF(ISPMD+1.EQ.PMAIN) THEN
            RBAGHOL(16,IV)=AOUT
            RBAGHOL(17,IV)=AOUT1
        ENDIF
       ENDDO
C-------------------
C  END LOOP ON NVENT
C------------------------------------------------
C  COMPUTE MASS FLOW RATE OUT : ISENTROPIC MODEL
C------------------------------------------------
       AOUTOT=AISENT+AISENT1+ACHEMK
       UISENT=ZERO
       FLOUT =ZERO
       DMOUT =ZERO
       DGMIN =ZERO
       TOUT  =TBAG
       IF(AOUTOT.GT.ZERO)THEN
        ROEX =RO*(PEXT/P)**(ONE/GAMA)
        TEMP =ROEX*AISENT+RO*ACHEMK+ROEX*AISENT1
        VMAX =HALF*(P-PEXT)*VOL/(GAMA-ONE)
     .        /MAX(EM20,HSPEC*TEMP*DT1)
        VMAX =MIN(VMAX,HALF*VOL/MAX(EM20,AOUTOT*DT1))
        VMAX =MAX(VMAX,ZERO)
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
C
        IF(AISENT1.GT.ZERO)THEN
         IF(P < PEXT) THEN
           GAMAI=RVOLU(1)
           RHOI=RVOLU(62)
           HSPEC1=RVOLU(63)
           ETA=(GAMAI-ONE)/GAMAI
           PCRIT1=PEXT*(TWO/(GAMAI+ONE))**(ONE/ETA)
           P2 = MAX(P,PCRIT1)
           RHO2 =RHOI*(P2/PEXT)**(ONE/GAMAI)
           UISENT=TWO*PEXT*(ONE-(P2/PEXT)**ETA)/(RHOI*ETA)
           UISENT=MAX(UISENT,ZERO)
           UISENT=SQRT(UISENT)
           VMAX =HALF*(PEXT-P)*VOL/(GAMA-ONE)
     .          /MAX(EM20,HSPEC1*RHOI*AISENT1*DT1)
           UISENT=MIN(UISENT,VMAX)
           FLOUT=FLOUT -AISENT1*UISENT
           DGMIN=DGMIN +AISENT1*UISENT*RHO2
           UISENT=-UISENT
           ROEX=RHO2
         ELSE
           PEXT = MAX(PEXT,PCRIT)
           ROEX =RO*(PEXT/P)**(ONE/GAMA)
           ETA=(GAMA-ONE)/GAMA
           UISENT=TWO*P*(ONE-(PEXT/P)**ETA)/(RO*ETA)
           UISENT=MAX(UISENT,ZERO)
           UISENT=SQRT(UISENT)
           UISENT=MIN(UISENT,VMAX)
           FLOUT=FLOUT +AISENT1*UISENT
           DMOUT=DMOUT +AISENT1*UISENT*ROEX
           HSPEC1=HSPEC
         ENDIF
        ENDIF
C
        IF(ISPMD+1.EQ.PMAIN)THEN
          DO IV=1,NVENT
            IDEF=IBAGHOL(1,IV)
            IVDP=IBAGHOL(9,IV)
            IF(IDEF.EQ.1)THEN
              IFVENT = IBAGHOL(10,IV)
              IF(IFVENT <= 1)THEN
                RBAGHOL(18,IV)=UISENT
                RBAGHOL(21,IV)= ROEX*UISENT
     .                         *(RBAGHOL(16,IV)+RBAGHOL(17,IV))
                RBAGHOL(22,IV)=RBAGHOL(21,IV)*HSPEC
              ELSEIF(IFVENT==2.OR.IFVENT==3) THEN
                RBAGHOL(18,IV)=MIN(RBAGHOL(18,IV),VMAX)
                RBAGHOL(21,IV)= RO*RBAGHOL(18,IV)
     .                         *(RBAGHOL(16,IV)+RBAGHOL(17,IV))
                RBAGHOL(22,IV)=RBAGHOL(21,IV)*HSPEC
              ELSEIF(IFVENT==4)THEN
                RBAGHOL(18,IV)=UISENT
                RBAGHOL(21,IV)= ROEX*UISENT
     .                         *(RBAGHOL(16,IV)+RBAGHOL(17,IV))
                RBAGHOL(22,IV)=RBAGHOL(21,IV)*HSPEC1
              END IF
            END IF
          END DO
        END IF
       ENDIF
C--------------
C SAVE FOR T.H.
C--------------
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
         FSAV(16)=ZERO
         DO IJ=1,NJET
           I_INJ = IBAGJET(13,IJ)
           IF(I_INJ <= 0) CYCLE
           NGASES   = IGEO(23,I_INJ)
           DO IK=1,NGASES
            FSAV(15)=FSAV(15)+RBAGJET(20+(IK-1)*4+2,IJ)
           ENDDO
           FSAV(16)=FSAV(16)+RBAGJET(11,IJ)
         ENDDO
         FSAV(17)=AMTOT*CV*TBAG
         FSAV(18)=RVOLU(32)
       ENDIF
C---------------------------------------------
C      MASSE et TRAVAIL par GAZ
C---------------------------------------------
       RMWI=RVOLU(10)
       RNMI=GMI*RMWI
C
C      VOLG/VOL=fraction molaire=RNMG/RNM
       DGMOUT=RNMI/MAX(EM20,RNM)*DMOUT
       DGEOUT=DGMOUT*TOUT*(
     .        CPAI+HALF*CPBI*TOUT+THIRD*CPCI*TOUT*TOUT
     .        +FOURTH*CPDI*TOUT*TOUT*TOUT
     .        -CPEI/(TOUT*TOUT)
     .        +ONE_FIFTH*CPFI*TOUT*TOUT*TOUT*TOUT)
       DGEIN =DGMIN*RVOLU(63)
       RVOLU(22)=RVOLU(22)+DGEOUT
       RVOLU(24)=RVOLU(24)+DGMOUT
       RVOLU(64)=DGMIN
       RVOLU(65)=DGEIN
C
       DO IJ=1,NJET
         I_INJ = IABS(IBAGJET(13,IJ))
         I_TYPINJ = IGEO(22,I_INJ)
         NGASES   = IGEO(23,I_INJ)
         DO IK=1,NGASES
          IF (I_TYPINJ.EQ.1) THEN
            I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
          ELSE IF (I_TYPINJ.EQ.2) THEN
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
          KK=20+(IK-1)*4
          GMTOT= RBAGJET(KK+1,IJ)
          RNMG  =GMTOT*RMWG
          DGMOUT=RNMG/MAX(EM20,RNM)*DMOUT
          DGEOUT=DGMOUT*TOUT*(
     .           CPA+HALF*CPB*TOUT+THIRD*CPC*TOUT*TOUT
     .           +FOURTH*CPD*TOUT*TOUT*TOUT
     .           -CPE/(TOUT*TOUT)
     .           +ONE_FIFTH*CPF*TOUT*TOUT*TOUT*TOUT)
          RBAGJET(KK+3,IJ)=RBAGJET(KK+3,IJ)+DGMOUT
          RBAGJET(KK+4,IJ)=RBAGJET(KK+4,IJ)+DGEOUT
          RBAGJET( 9,IJ)=RBAGJET(9 ,IJ)+DGMOUT
          RBAGJET(10,IJ)=RBAGJET(10,IJ)+DGEOUT
         ENDDO
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
     .          ' ** CHAMBER COMMUNICATION MEMBRANE IS DEFLATED **'
               WRITE(IOUT,*)
     .          ' ** MONITORED VOLUME ',IVOLU(1),' **'
               WRITE(ISTDO,*)
     .          ' ** CHAMBER COMMUNICATION MEMBRANE IS DEFLATED **'
             ENDIF
           ENDIF
           IF(IDEF.EQ.0 .AND. TT.GT.TVENT+TTF) THEN
             IDEF=1
             IF(ISPMD+1.EQ.PMAIN) THEN
               WRITE(IOUT,*) ' ** CHAMBER COMMUNICATION STARTS **'
               WRITE(IOUT,*) ' ** MONITORED VOLUME ',IVOLU(1),' **'
               WRITE(ISTDO,*)' ** COMMUNICATION STARTS **'
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
             ENDDO
             CALL SUM_6_FLOAT(1, NNC, F1, FRMV6B,1)
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
         IF(IPORT > 0) THEN
           TT1=TT-TTF
           IF(ITTF==13) TT1=TT-TTF-TVENT
           SCALT=RVOLU(26)
           AOUT =AOUT*FPORT*GET_U_FUNC(IPORT,TT1*SCALT,DERI)
         ENDIF
         IF(IPORP > 0) THEN
           SCALP=RVOLU(27)   
           AOUT =AOUT*FPORP*GET_U_FUNC(IPORP,(P-PVOIS)*SCALP,DERI)
         ENDIF
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
           U=ZERO
         ENDIF
         ICBAG(3,I) = IDEF
         RCBAG(8,I) = RCBAG(8,I) + DMOUT*DT1
         RCBAG(9,I) = U
C---------------------------------------------
C        MASSE et TRAVAIL par GAZ
C---------------------------------------------
C        VOLG/VOL=fraction molaire=RNMG/RNM
         DGMOUT=RNMI/MAX(EM20,RNM)*DMOUT
         DGEOUT=DGMOUT*TBAG*(
     .          CPAI+HALF*CPBI*TBAG+THIRD*CPCI*TBAG*TBAG
     .          +FOURTH*CPDI*TBAG*TBAG*TBAG
     .          -CPEI/(TBAG*TBAG)
     .          +ONE_FIFTH*CPFI*TBAG*TBAG*TBAG*TBAG)
C OUT
         RVOLU(22)=RVOLU(22) + DGEOUT
         RVOLU(24)=RVOLU(24) + DGMOUT
C IN
         RVOLUV(22,II)=RVOLUV(22,II) - DGEOUT
         RVOLUV(24,II)=RVOLUV(24,II) - DGMOUT
C
         RADVOIS= IVOLUV(10,II)
         DO IJ=1,NJET
          I_INJ = IABS(IBAGJET(13,IJ))
          I_TYPINJ = IGEO(22,I_INJ)
          NGASES   = IGEO(23,I_INJ)
          NFT=RADVOIS+NRBJET*(IJ-1)
C
          DO IK=1,NGASES
           IF (I_TYPINJ.EQ.1) THEN
             I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
           ELSE IF (I_TYPINJ.EQ.2) THEN
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
           KK=20+(IK-1)*4
           GMTOT= RBAGJET(KK+1,IJ)
           RNMG  =GMTOT*RMWG
           DGMOUT=RNMG/MAX(EM20,RNM)*DMOUT
           DGEOUT=DGMOUT*TBAG*(
     .            CPA+HALF*CPB*TBAG+THIRD*CPC*TBAG*TBAG
     .            +FOURTH*CPD*TBAG*TBAG*TBAG
     .            -CPE/(TBAG*TBAG)
     .            +ONE_FIFTH*CPF*TBAG*TBAG*TBAG*TBAG)
C OUT
           RBAGJET(KK+3,IJ) = RBAGJET(KK+3,IJ)+DGMOUT
           RBAGJET(KK+4,IJ) = RBAGJET(KK+4,IJ)+DGEOUT
           RBAGJET( 9,IJ) = RBAGJET( 9,IJ)+DGMOUT
           RBAGJET(10,IJ) = RBAGJET(10,IJ)+DGEOUT
C IN
           RBAGVJET(NFT+KK+3) = RBAGVJET(NFT+KK+3)-DGMOUT
           RBAGVJET(NFT+KK+4) = RBAGVJET(NFT+KK+4)-DGEOUT
           RBAGVJET(NFT+ 9) = RBAGVJET(NFT+ 9)-DGMOUT
           RBAGVJET(NFT+10) = RBAGVJET(NFT+10)-DGEOUT
           ENDDO
         ENDDO
         IF(ISPMD+1.EQ.PMAIN) THEN
           FSAV(8)=FSAV(8)+AOUT
           FSAV(9)=FSAV(9)+FLOUT
         ENDIF
       ENDDO  ! I=1,NAV

      IF(ISPMD+1.EQ.PMAIN) THEN
        FSAV(9)=FSAV(9)/MAX(EM20,FSAV(8))
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      RETURN
      END
