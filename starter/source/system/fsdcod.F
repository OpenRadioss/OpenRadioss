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
Chd|  FSDCOD                        source/system/fsdcod.F        
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INIGUSER                      source/system/iniguser.F      
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE FSDCOD(BUFMAT    ,PM        ,GEO       ,IBCL      ,IPRES   ,
     .                  IBFV      ,ISKEW     ,ISKN      ,SENSORS   ,
     .                  ITABM1    ,SKEW      ,LACCELM   ,INSEL     ,BUFGEO  ,
     .                  IBCSLAG   ,IGEO      ,IPM       ,
     .                  DAMPR     ,IBFT      ,IBCV      ,IBFVEL    ,
     .                  IBCR      ,TABLE     ,NPC1      ,NPC       ,PLD     ,
     .                  NOM_OPT   ,IBFFLUX   )  
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------C     
C CONVERTING USER IDENTIFIER INTO INTERNAL IDENTIFIERS (/SKEW, /FUNCT, /TABLE, /SENSOR, ...)
C    user_funct_id -> [1, NFUNCT]
C    user_skew_id  -> [1, NSKEW]
C    ...
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE TABLE_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),NPC(*), NPC1(*), IBCL(NIBCLD,*), IPRES(NIBCLD,*),
     .   ISKEW(*), ISKN(LISKN,*), ITABM1(*),
     .   LACCELM(3,*),INSEL(*),IBCSLAG(5,*), 
     .   IPM(NPROPMI,NUMMAT), IGEO(NPROPGI,NUMGEO),IBCV(NICONV,*),
     .   IBCR(NIRADIA,*),IBFFLUX(NITFLUX,*)
      INTEGER ,DIMENSION(NIFV,NFXVEL)  ,INTENT(INOUT) :: IBFVEL
      INTEGER ,DIMENSION(NIFT,NFXTEMP) ,INTENT(INOUT) :: IBFT
      my_real
     .   PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO),SKEW(LSKEW,*),
     .   DAMPR(NRDAMP,*), PLD(*),BUFMAT(*)
      TYPE(TTABLE) , DIMENSION(NTABLE) :: TABLE
      DOUBLE PRECISION BUFGEO(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISERV(18), IFLAG1, IFLAG2, IFLAG3, I,II,ILAW,J,JJ,K,I1,
     .  IS,IGTYP,NF,NOSKEW,ND,IUN,IFAIL,IADD,NFUNC,NFUND,IEXPAN,IFUNC,
     .  IERR1,IERR2,INUM_FAIL,IP,IR, KDIR, ICOND, IFUNCT, OK, ITABLE,
     .  ISK,IFC,IFD,IC1,IC2,ID1,ID2,NMUAL,NOGD,NC,IFLAG,ITENS,
     .  ICHK, IFLAG0, NI,EFUNC,IE,IE2,IFE,NRATE,ERRF,H,NP1,NP2,J1,K1,
     .  LOAD,UNLOAD, NTY, IRUP,IDN,IDT,PN1,PN2,PT1,PT2,KK,
     .  IFRIC1,IFRIC2,IDAMP1,IDAMP2,LOAD0,UNLOAD0,NF2,FUNC,FUND,IOK,ISENS,IMAT,IEOS,
     .  A_FUNC, B_FUNC
     
      LOGICAL IS_FOUND

      INTEGER NINTRI

      my_real
     .  PUN,X0,DX,DY,DERI,E,G,MUAL(10),MU,GS,RBULK,EMAX,GMAX,E0,EPSMAX,
     .  YFAC,DERI0,X1,EPS0,EPST1,EPST2,Y0,Y1,DYDX,DTDS,FAC(6),FAC1,FAC2,
     .  S1,S2,T1,T2,XX1,X2,YY1,Y2,SX,TY,XSCALE,ALPHA1,ALPHA2,
     .  STIFF,STIFF0,KC,KT,NU,YOUNG,DERIK(20),X_SCALE
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      CHARACTER*40 MESS
      CHARACTER*80 MESS1
      DATA IUN/1/

!     ICHECK - checking level in LAW69 curve fitting
!            <=0      no validity checking of mu_i and alpha_i in curve
!                     fitting
!            1        SUM( mu(i) * alpha(i) ) > 0.0
!            2        mu(i) * alpha(i) > 0.0
!            3        Try ICHECK=2 at first, if fails, switch to ICHECK=1 and try again.
      INTEGER ICHECK
      INTEGER NSTART
!     ERRTOL  - Tolerance for convergence checking in LAW69 curve fitting
!               If ERRAVE < ERRTOL, data fitting converges.
!               ERRAVE = ( SUM [ ABS ( ( Y_inp-Y_fit) / Y_inp )  ) / NPT
      my_real ERRTOL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C
      DATA MESS/'11TH MATERIAL LAW DEFINITION            '/
      DATA PUN/0.1/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IFLAG1=0
      IFLAG2=0
C----------------------------
C     (I) FUNCTIONS (/FUNCT)
C----------------------------
C
C
 15   CONTINUE
C
C         2) MATERIAL LAWS 11 18 20 21 28-31
C
      DO 300 I=1,NUMMAT
C
        ID=IPM(1,I)
        CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
        ILAW=NINT(PM(19,I))
C
        IF(ILAW == 11) THEN
C
C         UPDATING NODE IDENTIFIER
          IF(NINT(PM(51,I))/=0)THEN
            PM(51,I) = USR2SYS(NINT(PM(51,I)),ITABM1,MESS,ID)+PUN
          ENDIF
C
          DO J=1,10
            ISERV(J)=IPM(10+J,I)
          ENDDO!next J
          DO 230 K=1,10
            IF(ISERV(K)/=0) THEN
              DO J=1,NFUNCT
                IF(ISERV(K) == NPC1(J)) THEN
                  IPM(10+K,I)=J
                  
                  !check density function : IPM(11)
                  IF(K == 1)THEN
                    IC1 = NPC(J)
                    IC2 = NPC(J+1)
                    JJ=0
                    DO II = IC1,IC2-2,2
                      JJ = JJ+1
                      Y0 = PLD(II+1)
                      IF(Y0 <= ZERO)THEN
                        CALL ANCMSG(MSGID=132,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                              I1=ID, I2=ISERV(K), I3=JJ,
     .                              C1=TITR,
     .                              R1=Y0)
                        EXIT
                      ENDIF
                    ENDDO
                  ENDIF ! !end check 
                                 
                  GOTO 230
                ENDIF
              ENDDO!next J
              IPM(10+K,I) = 0 !function does not exist. Avoid check bounds issues
              CALL ANCMSG(MSGID=126,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISERV(K))
            ENDIF
 230      CONTINUE
C
        ELSE IF(ILAW == 18) THEN
         NF=IPM(10,I)
         DO 250 K=1,NF
          IS=IPM(10+K,I)
          IF(IS/=0)THEN
           DO J=1,NFUNCT
            IF(IS == NPC1(J))  THEN
              IPM(10+K,I)=J
              GOTO 250
            ENDIF
           ENDDO
           CALL ANCMSG(MSGID=126,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IS)
          ENDIF
 250      CONTINUE
c
        ELSE IF(ILAW == 19) THEN
         NF=IPM(10,I)+IPM(6,I)
         DO 240 K=1,NF
          IS=IPM(10+K,I)
          IF(IS/=0)THEN
           DO J=1,NFUNCT
            IF(IS == NPC1(J))  THEN
              IPM(10+K,I)=J
              GOTO 240
            ENDIF
           ENDDO
           CALL ANCMSG(MSGID=126,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IS)
          ENDIF
 240      CONTINUE
C
        ELSE IF(ILAW == 21) THEN
C
          IS=IPM(11,I)
          IF(IS/=0) THEN
            DO 260 J=1,NFUNCT
              IF(IS == NPC1(J))  THEN
                IPM(11,I)=J
               GOTO 183
              ENDIF
 260        CONTINUE
          ENDIF
              CALL ANCMSG(MSGID=126,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IS)
C
C-------
        ELSE IF(ILAW == 43) THEN
         EFUNC = 0
         NF=IPM(10,I)
         IF(IPM(10+NF,I) /= 0)EFUNC=1
         DO 243 K=1,NF
          IS=IPM(10+K,I)
          IF(IS/=0)THEN
           DO J=1,NFUNCT
            IF(IS == NPC1(J))  THEN
              IPM(10+K,I)=J
              GOTO 243
            ENDIF
           ENDDO
           CALL ANCMSG(MSGID=126,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IS)
          ENDIF
 243      CONTINUE
          IF (EFUNC > 0) THEN
            IFE=IPM(10+NF,I)
            IF(NF > EFUNC)THEN
              IE =NPC(IFE)
              IE2=NPC(IFE+1) 
              DO II = IE+1,IE2-3,2
                 IF(PLD(II) < PLD(II+2))THEN
                    CALL ANCMSG(MSGID=975,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                    EXIT 
                 ENDIF       
              ENDDO
            ENDIF
           ENDIF
C law 52
        ELSE IF (ILAW == 52) THEN
           DO 52 K = 1,IPM(226,I)!NTABLE
            ITABLE = IPM(226+k,I)
            IF(ITABLE/=0)THEN
             DO J=1,NTABLE
              IF(ITABLE == TABLE(J)%NOTABLE)  THEN
                IPM(226+k,I)=J
                ITABLE=IPM(226+K,I)
                GOTO 52
              ENDIF
             END DO
             CALL ANCMSG(MSGID=779,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=ITABLE)
            ENDIF
 52     CONTINUE
C
        ELSE IF(ILAW == 57) THEN
         EFUNC = 0
         NF=IPM(10,I)
         IF(IPM(10+NF,I) /= 0)EFUNC=1
         DO 257 K=1,NF
          IS=IPM(10+K,I)
          IF(IS/=0)THEN
           DO J=1,NFUNCT
            IF(IS == NPC1(J))  THEN
              IPM(10+K,I)=J
              GOTO 257
            ENDIF
           ENDDO
           CALL ANCMSG(MSGID=126,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IS)
          ENDIF
 257      CONTINUE
          IF (EFUNC > 0) THEN
            IFE=IPM(10+NF,I)
            IF(NF > EFUNC)THEN
              IE =NPC(IFE)
              IE2=NPC(IFE+1) 
              DO II = IE+1,IE2-3,2
                 IF(PLD(II) < PLD(II+2))THEN
                    CALL ANCMSG(MSGID=975,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                    EXIT 
                 ENDIF       
              ENDDO
            ENDIF
           ENDIF
c------------------------
C
        ELSE IF(ILAW == 59) THEN
          NF = IPM(10,I)
          DO 280 K=1,NF
            IS = IPM(10+K,I)
            IF (IS /= 0) THEN
              DO J=1,NFUNCT
                IF(IS == NPC1(J))  THEN
                  IPM(10+K,I)=J
                  GOTO 280
                ENDIF
              ENDDO
              CALL ANCMSG(MSGID=126,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IS)
            ENDIF
 280       CONTINUE
c
          IF (NF > 0)THEN
            IADD  = IPM(7,I) - 1
            E     = BUFMAT(IADD+1)
            G     = BUFMAT(IADD+2)
            NRATE = BUFMAT(IADD+3)
            EMAX  = ZERO           
            GMAX  = ZERO           
            DO K=1,2*NRATE-1,2
              IDN = IPM(10+K,I)
              IDT = IPM(10+K+1,I)
              PN1 = NPC(IDN)
              PN2 = NPC(IDN+1)
              PT1 = NPC(IDT)
              PT2 = NPC(IDT+1)
              KK = (K+1)/2
              YFAC= BUFMAT(IADD+7+KK)
              DO JJ = PN1,PN2-4,2
                DX = PLD(JJ+2) - PLD(JJ)
                DY = PLD(JJ+3) - PLD(JJ+1)
                DERI = ABS(DY*YFAC / DX)
                EMAX = MAX(EMAX, DERI)
              ENDDO
              DO JJ = PT1,PT2-4,2
                DX = PLD(JJ+2) - PLD(JJ)
                DY = PLD(JJ+3) - PLD(JJ+1)
                DERI = ABS(DY*YFAC / DX)
                GMAX = MAX(GMAX, DERI)
              ENDDO
            ENDDO
            IF (EMAX > E) THEN
              BUFMAT(IADD+1) = EMAX                  
              CALL ANCMSG(MSGID= 1041,           
     .                    MSGTYPE=MSGWARNING,      
     .                    ANMODE=ANINFO,         
     .                    I1=ID,                 
     .                    C1=TITR,C2='YOUNG MODULUS',R1=EMAX)               
            ENDIF                                
            IF (GMAX > G) THEN
              BUFMAT(IADD+2) = GMAX                  
              CALL ANCMSG(MSGID= 1041,           
     .                    MSGTYPE=MSGWARNING,      
     .                    ANMODE=ANINFO,         
     .                    I1=ID,                 
     .                    C1=TITR,C2='SHEAR MODULUS',R1=GMAX)               
            ENDIF                                
          ENDIF

C
        ELSE IF(ILAW == 60) THEN
         EFUNC = 0
         NF=IPM(10,I)
         IF(IPM(10+NF,I) /= 0)THEN 
           EFUNC=1
           IF(IPM(10+NF-1,I) /= 0 ) EFUNC =2
         ENDIF
         DO 287 K=1,NF
          IS=IPM(10+K,I)
          IF(IS/=0)THEN
           DO J=1,NFUNCT
            IF(IS == NPC1(J))  THEN
              IPM(10+K,I)=J
              GOTO 287
            ENDIF
           ENDDO
           CALL ANCMSG(MSGID=126,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IS)
          ENDIF
 287      CONTINUE
          IF (EFUNC > 0) THEN
            IFE=IPM(10+NF,I)
            IF(NF > EFUNC)THEN
              IE =NPC(IFE)
              IE2=NPC(IFE+1) 
              DO II = IE+1,IE2-3,2
                 IF(PLD(II) < PLD(II+2))THEN
                    CALL ANCMSG(MSGID=975,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                    EXIT 
                 ENDIF       
              ENDDO
            ENDIF
          ENDIF
C-------------------------------
        ELSE IF (ILAW == 65) THEN
         NF = IPM(10,I)
         DO 296 K=1,NF
           IS = IPM(10+K,I)
           IF (IS /=0)THEN
             DO J=1,NFUNCT
               IF(IS == NPC1(J))  THEN
                 IPM(10+K,I)=J
                 GOTO 296
               ENDIF
             ENDDO
             CALL ANCMSG(MSGID=126,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IS)
           ENDIF
 296     CONTINUE
C
         IF (NF > 0) THEN
           IADD = IPM(7,I) - 1
           NRATE= BUFMAT(IADD+1)
           E    = BUFMAT(IADD+2)
           G    = BUFMAT(IADD+8)
           
c           DO K=1,NF-1,2
           DO K=1,NRATE
             IFC = IPM(10+K,I)
             IFD = IPM(10+K+NRATE,I)           
             YFAC=BUFMAT(IADD+14+NRATE+K)
             IF (IFC > 0 .AND. IFD > 0) THEN
               IC1 = NPC(IFC)
               IC2 = NPC(IFC+1)
               ID1 = NPC(IFD)
               ID2 = NPC(IFD+1)
               IERR1 = 0
               IERR2 = 0
C              loading function
               X0 = PLD(IC1)
               DO II = IC1,IC2-4,2
                 JJ = II+2
                 DX = PLD(JJ)   - X0
                 DY = PLD(JJ+1) - PLD(II+1)
                 DERI = DY*YFAC / DX
                 DX = DX*(E - DERI)/E
                 X0 =  PLD(JJ)
                 IF (DX < ZERO) IERR1 = 1
c                 PLD(JJ) = PLD(II) + DX
               ENDDO
c              unloading function
               X0 = PLD(ID1)
               DO II = ID1,ID2-4,2
                 JJ = II+2
                 DX = PLD(JJ)   - X0
                 DY = PLD(JJ+1) - PLD(II+1)
                 DERI = DY *YFAC/ DX
                 DX = DX*(E - DERI)/E
                 IF (DX < ZERO) IERR2 = 1
                  X0 =  PLD(JJ)
c                 PLD(JJ) = PLD(II) + DX
               ENDDO
               IF (IERR1 == 1) THEN
                 CALL ANCMSG(MSGID=808,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=NPC1(IFC))
               ENDIF
               IF (IERR2 == 1) THEN
                 CALL ANCMSG(MSGID=808,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=NPC1(IFD))
                 ENDIF
             ENDIF
           ENDDO
         ENDIF
C
        ELSE IF (ILAW == 75) THEN
C CHANGE USER MATERIAL NUMBER TO INTERNAL 
          IADD = IPM(7,I)-1
          II   = NINT(BUFMAT(IADD+6))
          JJ   = NINTRI(II,IPM,NPROPMI,NUMMAT,1)
          BUFMAT(IADD+6) = JJ
          IF(JJ == 0) THEN
            CALL ANCMSG(MSGID=1008,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,I2=II,
     .                  C1=TITR)
          ENDIF
C
        ELSE IF (ILAW == 78) THEN
         NF = IPM(10,I)
         DO 378 K=1,NF
           IS = IPM(10+K,I)
           IF (IS /=0)THEN
             DO J=1,NFUNCT
               IF(IS == NPC1(J))  THEN
                 IPM(10+K,I)=J
                 GOTO 378
               ENDIF
             ENDDO
             CALL ANCMSG(MSGID=126,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IS)
           ENDIF
 378     CONTINUE
         IF (NF > 0) THEN
            IFE=IPM(10+NF,I)
            IE =NPC(IFE)
            IE2=NPC(IFE+1) 
            DO II = IE+1,IE2-3,2
               IF(PLD(II) < PLD(II+2))THEN
                  CALL ANCMSG(MSGID=975,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1=TITR)
                  EXIT 
               ENDIF       
            ENDDO
         ENDIF
C law 88 - tabulated ogden law  removed to updmat.F
        ELSEIF (ILAW < 29) THEN
C
          NF = IPM(10,I)
          IF (NF > 0) THEN
            DO K=1,NF
              IS = IPM(10+K,I)
              OK = 0
              IF (IS > 0) THEN
                DO J=1,NFUNCT
                  IF(IS == NPC1(J))  THEN
                    IPM(10+K,I)=J
                    OK = 1
                    EXIT
                  ENDIF
                ENDDO
                IF (OK == 0) THEN
                  CALL ANCMSG(MSGID=126,
     .                 MSGTYPE=MSGERROR,          
     .                 ANMODE=ANINFO_BLIND_1,     
     .                 I1=ID,                     
     .                 C1=TITR,                   
     .                 I2=IS)                     
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
C------------
         INUM_FAIL = IPM(220,I)
         IF(INUM_FAIL > 0) THEN
           DO IR = 1,INUM_FAIL
            IRUP = IPM(111 + 15*(IR-1),I)
c
            IF (IRUP == 23 ) THEN
               IP = (IR -1)*15
               !numbering for ifunc(1) in lecr23.F
               ITABLE=IPM(115 + IP + 1,I)
               IF(ITABLE /= 0)THEN
                DO J=1,NTABLE
                  IF(ITABLE == TABLE(J)%NOTABLE)  THEN
                   IPM(115 + IP + 1,I)=J
                   GOTO 923
                  ENDIF
                ENDDO
                CALL ANCMSG(MSGID=779,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
               ENDIF
 923           CONTINUE

               !numbering for ifunc(5) in lecr23.F
               ITABLE=IPM(115 + IP + 5,I)
               IF(ITABLE /= 0)THEN
                DO J=1,NTABLE
                  IF(ITABLE == TABLE(J)%NOTABLE)  THEN
                   IPM(115 + IP + 5,I)=J
                   GOTO 823
                  ENDIF
                ENDDO
                CALL ANCMSG(MSGID=779,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
               ENDIF
 823           CONTINUE

               !numbering for ifunc(2-3-6)
               NF=IPM(115 +IP ,I)
               DO 223 K=2,NF
                 IS=IPM(115 + IP + K,I)
                 IF(IS /= 0.AND. K /= 5)THEN
                   DO 323 J=1,NFUNCT
                    IF(IS == NPC1(J))  THEN
                     IPM(115+ IP + K ,I)=J
                     GOTO 223
                    ENDIF
 323                CONTINUE
                    CALL ANCMSG(MSGID=126,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .                        I2=IS)
                 ENDIF
 223           CONTINUE
c
            ELSE ! IRUP /= 23
            IP = (IR -1)*15
              NF=IPM(115 +IP ,I)
              DO 291 K=1,NF
                IS=IPM(115 + IP + K,I)
                IF(IS/=0)THEN
                  DO 292 J=1,NFUNCT
                    IF(IS == NPC1(J))  THEN
                     IPM(115+ IP + K ,I)=J
                      GOTO 291
                    ENDIF
 292              CONTINUE
                  CALL ANCMSG(MSGID=126,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID,
     .                        C1=TITR,
     .                        I2=IS)
                ENDIF
 291          CONTINUE
            ENDIF
           ENDDO
         ENDIF
C------------
183         IEXPAN =  IPM(218,I)
         IF(IEXPAN > 0)THEN
           IS=IPM(219,I)
           IF(IS > 0)THEN
             DO J=1,NFUNCT
               IF(IS == NPC1(J))  THEN
                 IPM(219,I)=J
                 GOTO 299
               ENDIF
             ENDDO
             CALL ANCMSG(MSGID=126,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IS)
           ENDIF
         ENDIF
 299   CONTINUE
C------------
C     fin boucle sur mats :
 300  CONTINUE
 
 
      !---EOS INPUT BASED ON FUNCTION (TABULATED EoS : IEOS=17 ---!
      DO IMAT=1,NUMMAT
        IEOS = IPM(4,IMAT)

        IF(IEOS == 17)THEN

          ID=IPM(1,IMAT)
          CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,IMAT),LTITR)
          ILAW=NINT(PM(19,I))

          A_FUNC = PM(35,IMAT) 
          IF(A_FUNC /= 0)THEN                                                                                                
            IS_FOUND = .FALSE.                                                                                       
            DO J=1,NFUNCT                                                                                            
              IF(A_FUNC == NPC1(J))  THEN                                                                              
                PM(35,IMAT)=J                                                                                        
                IS_FOUND = .TRUE.                                                                                    
                EXIT                                                                                                 
              ENDIF                                                                                                  
            ENDDO                                                                                                    
            IF(.NOT.IS_FOUND)CALL ANCMSG(MSGID=125,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=A_FUNC)
          ENDIF  

          B_FUNC = PM(36,IMAT)
          IF(B_FUNC /= 0)THEN                                                                                     
            IS_FOUND = .FALSE.                                                                                       
            DO J=1,NFUNCT                                                                                            
              IF(B_FUNC == NPC1(J))  THEN                                                                                
                PM(36,IMAT)=J                                                                                        
                IS_FOUND = .TRUE.                                                                                    
                EXIT                                                                                                 
              ENDIF                                                                                                  
            ENDDO                                                                                                    
            IF(.NOT.IS_FOUND)CALL ANCMSG(MSGID=125,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=B_FUNC)    
           ENDIF
             
         ENDIF

      ENDDO
 
 
 
C
C     3) PID SPRING/AIRBAG/GENERAL SPRING
C
      DO 420 I=1,NUMGEO
C
        IGTYP=IGEO(11,I)
C
        ID=IGEO(1,I)
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I),LTITR)
C
        IF (IGTYP == 4) THEN
C
          ISERV(1)=IGEO(101,I)
          ISERV(2)=IGEO(102,I)
          ISERV(3)=IGEO(103,I)
          LOAD0 =IGEO(101,I)
          UNLOAD0=IGEO(103,I)
          ISERV(4)=4
          ISERV(5)=14
          ISERV(6)=18
          H = GEO(7,I)
          DO 330 K=1,3
            IF(ISERV(K)/=0) THEN
              DO 320 J=1,NFUNCT
                IF(ISERV(K) == NPC1(J)) THEN
                  GEO(ISERV(K+3),I)=J+PUN
                  IGEO(100+K,I)=J
                  GO TO 330
                ENDIF
 320          CONTINUE
              CALL ANCMSG(MSGID=127,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISERV(K+3))
            ENDIF
 330      CONTINUE
          IF (IGEO(119,I) /=0)THEN                    
             ERRF = 1                   
             DO  J=1,NFUNCT  
               IF(IGEO(119,I) == NPC1(J)) THEN    
                 IGEO(119,I)=J
                 ERRF = 0                   
                 EXIT                       
               ENDIF                              
             ENDDO 
             IF (ERRF == 1) THEN                          
                CALL ANCMSG(MSGID=127,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IGEO(119,I))
             ENDIF
          ENDIF   
          !compute max slope for ifunc3
          YFAC  = GEO(132,I) !GF3 in lecgeo4       
          IFUNC = IGEO(119,I) !IFUNC3 in lecgeo4
          X_SCALE = GEO(18,I)
          IF (IFUNC /= 0)THEN                       
            IC1 = NPC(IFUNC)                        
            IC2 = NPC(IFUNC+1)                      
            X0 = PLD(IC1)                           
            EMAX = ZERO                             
            DO II = IC1,IC2-4,2                     
               JJ = II+2                            
               DX = PLD(JJ)   - X0                  
               DY = PLD(JJ+1) - PLD(II+1)           
               Y0 =  PLD(II+1)                      
               Y1 =  PLD(JJ+1)                      
               DERI = YFAC * X_SCALE * DY / DX                
               X1 =  PLD(JJ)                        
               EMAX = MAX(EMAX, DERI)               
               X0 =  PLD(JJ)                        
            ENDDO                                   
            GEO(141,I) =  EMAX                      
          ENDIF                                     

          IF (H == 7)THEN
             XSCALE=GEO(39,I)
             LOAD=IGEO(101,I)
             UNLOAD=IGEO(103,I)
             NP1  = (NPC(LOAD+1)-NPC(LOAD))*HALF
             NP2  = (NPC(UNLOAD+1)-NPC(UNLOAD))*HALF
             ALPHA1=ZERO  
             ALPHA2=ZERO             
c---
             DO 777 J=2,NP1
               J1=2*(J-2)
               S1=PLD(NPC(LOAD)+J1)*XSCALE
               S2=PLD(NPC(LOAD)+J1+2)*XSCALE
               T1=PLD(NPC(LOAD)+J1+1)
               T2=PLD(NPC(LOAD)+J1+3)
               TY=ZERO
               SX=ZERO
               IF ( S1<=ZERO .AND.S2> ZERO)ALPHA1=(T2-T1)/(S2-S1)
               DO K=2,NP2
                 K1=2*(K-2)
                 XX1=PLD(NPC(UNLOAD)+K1)*XSCALE
                 X2 =PLD(NPC(UNLOAD)+K1+2)*XSCALE
                 YY1=PLD(NPC(UNLOAD)+K1+1)
                 Y2 =PLD(NPC(UNLOAD)+K1+3)
                 IF ( XX1<=ZERO .AND.X2> ZERO)ALPHA2=(Y2-YY1)/(X2-XX1)!  passage par zero
                 IF (Y2>=T1 .AND.YY1<=T2.AND.X2>=S1.AND.XX1<=S2)THEN
                   DYDX = (Y2-YY1) / (X2-XX1)
                   DTDS = (T2-T1) / (S2-S1)
                   IF (DYDX > DTDS) THEN ! intersection des courbes
                     SX = (T1-YY1-DTDS*S1+DYDX*XX1) / (DYDX-DTDS)
                     TY =  T1 + DTDS*(SX - S1)
                   ENDIF 
                   IF (TY/=ZERO .AND. SX/=ZERO )THEN  
                    IF (TY>=YY1.AND.TY<=Y2.AND.SX>=XX1.AND.SX<=X2
     .                      .AND.SX>=S2.AND.TY<=T2)THEN 
           
                      CALL ANCMSG(MSGID=982,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            C1=TITR,
     .                            I1=UNLOAD0,
     .                            I2=LOAD0)
                     GOTO 777         
                    ENDIF     
                   ENDIF                                
                 ENDIF
               ENDDO
 777         CONTINUE      
             IF(ALPHA2>=ALPHA1)THEN
               CALL ANCMSG(MSGID=982,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     C1=TITR,
     .                     I1=UNLOAD,
     .                     I2=LOAD)
             ENDIF
          ENDIF                             
c-------
        ELSEIF(IGTYP == 12) THEN
C
          ISERV(1)=IGEO(101,I)
          ISERV(2)=IGEO(102,I)
          ISERV(3)=IGEO(103,I)
          H = GEO(7,I)
          DO 331 K=1,3
            IF(ISERV(K)/=0) THEN
              DO J=1,NFUNCT
                IF(ISERV(K) == NPC1(J)) THEN
                  IGEO(100+K,I)=J
                  GO TO 331
                ENDIF
              ENDDO
              CALL ANCMSG(MSGID=127,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISERV(K+3))
            ENDIF
 331      CONTINUE
          IF (IGEO(201,I) > 0) THEN
           DO J=1,NTABLE
            IF (IGEO(201,I) == TABLE(J)%NOTABLE)  THEN
              IGEO(201,I) = J
              GOTO 332
            ENDIF
           END DO
           CALL ANCMSG(MSGID=779,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
          ENDIF
 332     CONTINUE
c
          IF (IGEO(119,I) /=0)THEN                    
             ERRF = 1                   
             DO  J=1,NFUNCT  
               IF(IGEO(119,I) == NPC1(J)) THEN    
                 IGEO(119,I)=J
                 ERRF = 0                   
                 EXIT                       
               ENDIF                              
             ENDDO 
             IF (ERRF == 1) THEN                          
                CALL ANCMSG(MSGID=127,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IGEO(119,I))
             ENDIF
          ENDIF   
          
          YFAC  = GEO(132,I) !GF3 in lecgeo12       
          IFUNC = IGEO(119,I) !IFUNC3 in lecgeo12 
          X_SCALE = GEO(18,I)  
          IF (IFUNC /= 0)THEN                       
            IC1 = NPC(IFUNC)                        
            IC2 = NPC(IFUNC+1)                      
            X0 = PLD(IC1)                           
            EMAX = ZERO                             
            DO II = IC1,IC2-4,2                     
               JJ = II+2                            
               DX = PLD(JJ)   - X0                  
               DY = PLD(JJ+1) - PLD(II+1)           
               Y0 =  PLD(II+1)                      
               Y1 =  PLD(JJ+1)                      
               DERI = YFAC * X_SCALE * DY / DX                
               X1 =  PLD(JJ)                        
               EMAX = MAX(EMAX, DERI)               
               X0 =  PLD(JJ)                        
            ENDDO                                   
            GEO(141,I) =  EMAX                      
          ENDIF                                     

          IF (H == 7)THEN
             XSCALE=GEO(39,I)
             LOAD=IGEO(101,I)
             UNLOAD=IGEO(103,I)
             NP1  = (NPC(LOAD+1)-NPC(LOAD))*HALF
             NP2  = (NPC(UNLOAD+1)-NPC(UNLOAD))*HALF
             ALPHA1=ZERO  
             ALPHA2=ZERO             
c---
             DO 778 J=2,NP1
               J1=2*(J-2)
               S1=PLD(NPC(LOAD)+J1)*XSCALE
               S2=PLD(NPC(LOAD)+J1+2)*XSCALE
               T1=PLD(NPC(LOAD)+J1+1)
               T2=PLD(NPC(LOAD)+J1+3)
               TY=ZERO
               SX=ZERO
               IF ( S1<=ZERO .AND.S2> ZERO)ALPHA1=(T2-T1)/(S2-S1)
               DO K=2,NP2
                 K1=2*(K-2)
                 XX1=PLD(NPC(UNLOAD)+K1)*XSCALE
                 X2=PLD(NPC(UNLOAD)+K1+2)*XSCALE
                 YY1=PLD(NPC(UNLOAD)+K1+1)
                 Y2=PLD(NPC(UNLOAD)+K1+3)
                 IF ( XX1<=ZERO .AND.X2> ZERO)ALPHA2=(Y2-YY1)/(X2-XX1)  
                 IF (Y2>=T1 .AND.YY1<=T2.AND.X2>=S1.AND.XX1<=S2)THEN
                   DYDX = (Y2-YY1) / (X2-XX1)
                   DTDS = (T2-T1) / (S2-S1)
                   IF (DYDX > DTDS) THEN
                     SX = (T1-YY1-DTDS*S1+DYDX*XX1) / (DYDX-DTDS)
                     TY =  T1 + DTDS*(SX - S1)
                   ENDIF
                   IF (TY/=ZERO .AND. SX/=ZERO )THEN  
                    IF (TY>=YY1.AND.TY<=Y2.AND.SX>=XX1.AND.SX<=X2
     .                      .AND.SX>=S2.AND.TY<=T2)THEN 
                      CALL ANCMSG(MSGID=982,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            C1=TITR,
     .                            I1=UNLOAD,
     .                            I2=LOAD)
                     GOTO 778         
                    ENDIF     
                   ENDIF                                
                 ENDIF
               ENDDO
 778         CONTINUE                          
             IF(ALPHA2>=ALPHA1)THEN
               CALL ANCMSG(MSGID=982,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     C1=TITR,
     .                     I1=UNLOAD,
     .                     I2=LOAD)
             ENDIF
          ENDIF                             
C
        ELSE IF(IGTYP == 7) THEN
C
          ISERV(1)=NINT(GEO(19,I))
          ISERV(2)=NINT(GEO(44,I))
          ISERV(3)=19
          ISERV(4)=44
          DO 360 K=1,2
            DO 340 J=1,NFUNCT
              IF(ISERV(K) == NPC1(J)) THEN
                GEO(ISERV(K+2),I)=J+PUN
                GO TO 360
              ENDIF
 340        CONTINUE
              CALL ANCMSG(MSGID=127,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=ISERV(K))
 360        CONTINUE
c------------------------
        ELSEIF(IGTYP==8.OR.IGTYP==13) THEN
C
          DO 400 J=1,6
            ISERV(1)=IGEO(101+3*(J-1),I)
            ISERV(2)=IGEO(102+3*(J-1),I)
            ISERV(3)=IGEO(103+3*(J-1),I)
            IFLAG1 = 0
            IFLAG2 = 0
            IFLAG3 = 0
            IF(ISERV(1) == 0)IFLAG1=1
            IF(ISERV(2) == 0)IFLAG2=1
            IF(ISERV(3) == 0)IFLAG3=1
            IF(IFLAG1+IFLAG2+IFLAG3 == 3)GOTO 400
            DO 380 K=1,NFUNCT
              IF(ISERV(1) == NPC1(K)) THEN
                IGEO(101+3*(J-1),I) = K
                IFLAG1=1
              ENDIF
              IF(ISERV(2) == NPC1(K)) THEN
                IGEO(102+3*(J-1),I) = K
                IFLAG2=1
              ENDIF
              IF(ISERV(3) == NPC1(K)) THEN
                IGEO(103+3*(J-1),I) = K
                IFLAG3=1
              ENDIF
              IF(IFLAG1+IFLAG2+IFLAG3 == 3)GOTO 400
 380        CONTINUE
            
            IF(IFLAG1 == 0) ID1=ISERV(1)
            IF(IFLAG2 == 0) ID1=ISERV(2)
            IF(IFLAG3 == 0) ID1=ISERV(3)
            CALL ANCMSG(MSGID=127,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ID1)
 400      CONTINUE 
c -------- 
          DO  J=1, 6
            ERRF = 1  
            IF (IGEO(119+J-1,I) /=0)THEN       
              DO  K=1,NFUNCT
                IF(IGEO(119+J-1,I) == NPC1(K)) THEN !ifunc3
                  IGEO(119+J-1,I) = K
                  ERRF = 0
                  EXIT
                ENDIF                            
              ENDDO
              IF (ERRF == 1)THEN 
                IF (IGTYP == 8)THEN
                ELSE
                ENDIF
                CALL ANCMSG(MSGID=127,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IGEO(119+J-1,I))
              ENDIF
            ENDIF
          ENDDO
!compute max slope for ifunc3
          DO  J=1, 6
            YFAC  = GEO(131+J,I) !GF3 in lecgeo13 -8
            IFUNC = IGEO(118+J,I) !IFUNC3 in lecgeo13 -8
            X_SCALE=GEO(44+4*(J-1),I)
            IF (IFUNC /= 0)THEN
              IC1 = NPC(IFUNC)           
              IC2 = NPC(IFUNC+1)         
              X0 = PLD(IC1)
              EMAX = ZERO
              DO II = IC1,IC2-4,2                   
                 JJ = II+2                          
                 DX = PLD(JJ)   - X0                
                 DY = PLD(JJ+1) - PLD(II+1)         
                 Y0 =  PLD(II+1)               
                 Y1 =  PLD(JJ+1)               
                 DERI = YFAC * X_SCALE * DY / DX              
                 X1 =  PLD(JJ)                      
                 EMAX = MAX(EMAX, DERI)             
                 X0 =  PLD(JJ)                      
              ENDDO  
              GEO(140+J,I) =  EMAX  
            ENDIF                        
          ENDDO
C
          DO 877 J=1, 6 
            IF(J<= 2)THEN
              H=GEO(7*J,I)
            ELSE
              H=GEO(14+(J-2)*4,I)
            ENDIF         
            IF (H == 7)THEN
              IF (J==1)THEN
               XSCALE=GEO(39,I)
              ELSE
               XSCALE=GEO(172+J,I) 
              ENDIF
              LOAD=IGEO(101+3*(J-1),I)
              UNLOAD=IGEO(103+3*(J-1),I)
              NP1  = (NPC(LOAD+1)-NPC(LOAD))*HALF
              NP2  = (NPC(UNLOAD+1)-NPC(UNLOAD))*HALF
              ALPHA1=ZERO  
              ALPHA2=ZERO             
c---
              DO JJ=2,NP1
                J1=2*(JJ-2)
                S1=PLD(NPC(LOAD)+J1)*XSCALE
                S2=PLD(NPC(LOAD)+J1+2)*XSCALE
                T1=PLD(NPC(LOAD)+J1+1)
                T2=PLD(NPC(LOAD)+J1+3)
                TY=ZERO
                SX=ZERO
                IF ( S1<=ZERO .AND.S2> ZERO)ALPHA1=(T2-T1)/(S2-S1)
                DO K=2,NP2
                 K1=2*(K-2)
                 XX1=PLD(NPC(UNLOAD)+K1)*XSCALE
                 X2=PLD(NPC(UNLOAD)+K1+2)*XSCALE
                 YY1=PLD(NPC(UNLOAD)+K1+1)
                 Y2=PLD(NPC(UNLOAD)+K1+3)
                 IF ( XX1<=ZERO .AND.X2> ZERO)ALPHA2=(Y2-YY1)/(X2-XX1)  
                 IF (Y2>=T1 .AND.YY1<=T2.AND.X2>=S1.AND.XX1<=S2)THEN
                   DYDX = (Y2-YY1) / (X2-XX1)
                   DTDS = (T2-T1) / (S2-S1)
                   IF (DYDX > DTDS) THEN
                     SX = (T1-YY1-DTDS*S1+DYDX*XX1) / (DYDX-DTDS)
                     TY =  T1 + DTDS*(SX - S1)
                   ENDIF
                   IF (TY/=ZERO .AND. SX/=ZERO )THEN  
                    IF (TY>=YY1.AND.TY<=Y2.AND.SX>=XX1.AND.SX<=X2
     .                      .AND.SX>=S2.AND.TY<=T2)THEN 
                      IF (IGTYP == 8)THEN
                      ELSE
                      ENDIF        
                      CALL ANCMSG(MSGID=982,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            C1=TITR,
     .                            I1=UNLOAD,
     .                            I2=LOAD)
                     GOTO 877         
                    ENDIF     
                   ENDIF                                
                 ENDIF
                ENDDO
              ENDDO
              IF(ALPHA2>=ALPHA1)THEN
                IF (IGTYP == 8)THEN
                ELSE
                ENDIF        
                CALL ANCMSG(MSGID=982,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      C1=TITR,
     .                      I1=UNLOAD,
     .                      I2=LOAD)
              ENDIF
           ENDIF      
 877      CONTINUE     

C         ENDDO
c -------- 
C
        ELSEIF (IGTYP==25) THEN
C
          DO 401 J=1,4 ! instead of 6 pmo
            ISERV(1)=IGEO(102+4*(J-1),I)
            ISERV(2)=IGEO(103+4*(J-1),I)
            ISERV(3)=IGEO(104+4*(J-1),I)
            IFLAG1 = 0
            IFLAG2 = 0
            IFLAG3 = 0
            IF(ISERV(1) == 0)IFLAG1=1
            IF(ISERV(2) == 0)IFLAG2=1
            IF(ISERV(3) == 0)IFLAG3=1
            IF(IFLAG1+IFLAG2+IFLAG3 == 3)GOTO 401
            DO 381 K=1,NFUNCT
              IF(ISERV(1) == NPC1(K)) THEN
                IGEO(102+4*(J-1),I) = K
                IFLAG1=1
              ENDIF
              IF(ISERV(2) == NPC1(K)) THEN
                IGEO(103+4*(J-1),I) = K
                IFLAG2=1
              ENDIF
              IF(ISERV(3) == NPC1(K)) THEN
                IGEO(104+4*(J-1),I) = K
                IFLAG3=1
              ENDIF
              IF(IFLAG1+IFLAG2+IFLAG3 == 3)GOTO 401
 381        CONTINUE
            IF(IFLAG1 == 0) ID1=ISERV(1)
            IF(IFLAG2 == 0) ID1=ISERV(2)
            IF(IFLAG3 == 0) ID1=ISERV(3)
            CALL ANCMSG(MSGID=127,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ID1)
 401      CONTINUE
C
c -------- 
         DO  J=1,4                                          
           ERRF = 1                                         
           IF (IGEO(119+J-1,I) /=0)THEN                     
             DO  K=1,NFUNCT                                 
               IF(IGEO(119+J-1,I) == NPC1(K)) THEN          
                 IGEO(119+J-1,I) = K                        
                 ERRF = 0                                   
                 EXIT                                       
               ENDIF                                        
             ENDDO                                          
             IF (ERRF == 1)THEN                             
               CALL ANCMSG(MSGID=127,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IGEO(119+J-1,I))
             ENDIF                                          
           ENDIF                                            
         ENDDO
!compute max slope for ifunc3
          DO  J=1, 4
            YFAC  = GEO(131+J,I) !GF3 in lecgeo25
            IFUNC = IGEO(118+J,I) !IFUNC3 in lecgeo25
            IF (J==1) X_SCALE = GEO(44,I)
            IF (J==2) X_SCALE = GEO(48,I) 
            IF (J==3) X_SCALE = GEO(56,I) 
            IF (J==4) X_SCALE = GEO(60,I)  
            IF (IFUNC /= 0)THEN
              IC1 = NPC(IFUNC)           
              IC2 = NPC(IFUNC+1)         
              X0 = PLD(IC1)
              EMAX = ZERO
              DO II = IC1,IC2-4,2                   
                 JJ = II+2                          
                 DX = PLD(JJ)   - X0                
                 DY = PLD(JJ+1) - PLD(II+1)         
                 Y0 =  PLD(II+1)               
                 Y1 =  PLD(JJ+1)               
                 DERI = YFAC * X_SCALE * DY / DX              
                 X1 =  PLD(JJ)                      
                 EMAX = MAX(EMAX, DERI)             
                 X0 =  PLD(JJ)                      
              ENDDO  
              GEO(140+J,I) =  EMAX      
            ENDIF                        
          ENDDO
         
         DO 888 J=1,4                                                     
           H=IGEO(101+(J-1)*4,I)                                      
           IF (H == 7)THEN                                              
             IF (J==1)THEN                                              
                  XSCALE=GEO(39,I)                                       
                ELSEIF (J==2)THEN                                                   
                  XSCALE=GEO(174,I)                                    
                ELSEIF (J==3)THEN                                                   
                  XSCALE=GEO(176,I)                                    
                ELSEIF (J==4)THEN                                                   
                  XSCALE=GEO(177,I)                                    
             ENDIF                                                      
             LOAD=IGEO(102+4*(J-1),I)                                  
             UNLOAD=IGEO(103+4*(J-1),I)                              
             NP1  = (NPC(LOAD+1)-NPC(LOAD))*HALF                      
             NP2  = (NPC(UNLOAD+1)-NPC(UNLOAD))*HALF   
             ALPHA1=ZERO  
             ALPHA2=ZERO             
c---
             DO JJ=2,NP1                                                
              J1=2*(JJ-2)                                                   
              S1=PLD(NPC(LOAD)+J1)*XSCALE                                   
              S2=PLD(NPC(LOAD)+J1+2)*XSCALE                                 
              T1=PLD(NPC(LOAD)+J1+1)                                        
              T2=PLD(NPC(LOAD)+J1+3)      
              TY=ZERO
              SX=ZERO                                  
              IF ( S1<=ZERO .AND.S2> ZERO)ALPHA1=(T2-T1)/(S2-S1)            
              DO K=2,NP2                                                    
               K1=2*(K-2)                                                   
               XX1=PLD(NPC(UNLOAD)+K1)*XSCALE                               
               X2=PLD(NPC(UNLOAD)+K1+2)*XSCALE                              
               YY1=PLD(NPC(UNLOAD)+K1+1)                                    
               Y2=PLD(NPC(UNLOAD)+K1+3)                                     
               IF ( XX1<=ZERO .AND.X2> ZERO)ALPHA2=(Y2-YY1)/(X2-XX1)        
               IF (Y2>=T1 .AND.YY1<=T2.AND.X2>=S1.AND.XX1<=S2)THEN          
                 DYDX = (Y2-YY1) / (X2-XX1)                                 
                 DTDS = (T2-T1) / (S2-S1)                                   
                 IF (DYDX > DTDS) THEN                                      
                   SX = (T1-YY1-DTDS*S1+DYDX*XX1) / (DYDX-DTDS)             
                   TY =  T1 + DTDS*(SX - S1)                                
                 ENDIF                                                      
                 IF (TY/=ZERO .AND. SX/=ZERO )THEN                          
                    IF (TY>=YY1.AND.TY<=Y2.AND.SX>=XX1.AND.SX<=X2
     .                      .AND.SX>=S2.AND.TY<=T2)THEN 
                     CALL ANCMSG(MSGID=982,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO_BLIND_1,
     .                           C1=TITR,
     .                           I1=UNLOAD,
     .                           I2=LOAD)
                    GOTO 888                                                
                   ENDIF                                                    
                 ENDIF                                                      
               ENDIF                                                        
              ENDDO                                                         
             ENDDO                                                      
             IF(ALPHA2>=ALPHA1)THEN   
               CALL ANCMSG(MSGID=982,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     C1=TITR,
     .                     I1=UNLOAD,
     .                     I2=LOAD)
             ENDIF                                                      
           ENDIF   ! H == 7                                                     
 888     CONTINUE                                                               
c --------         
         ELSEIF (IGTYP == 26) THEN
          NFUNC = IGEO(20,I)
          NFUND = IGEO(21,I)
          IADD = 100
          DO K=1,NFUNC
            IFLAG1 = 0
            DO J=1,NFUNCT
              IF (IGEO(IADD+K,I) == NPC1(J)) THEN
                IGEO(IADD+K,I) = J
                IFLAG1 = 1
                EXIT
              ENDIF
            ENDDO
            IF (IFLAG1 == 0) THEN
            ENDIF
          ENDDO
          IADD = NFUNC+100
          DO K=1,NFUND
            IFLAG1 = 0
            DO J=1,NFUNCT
              IF (IGEO(IADD+K,I) == NPC1(J)) THEN
                IGEO(IADD+K,I) = J
                IFLAG1 = 1
                EXIT
              ENDIF
            ENDDO
            IF (IFLAG1 == 0) THEN
            ENDIF
          ENDDO

        ELSEIF (IGTYP == 27) THEN
C
          ! Convert User ID function in internal ID
          ISERV(1) = IGEO(101,I)
          ISERV(2) = IGEO(102,I)
          ISERV(3) = 4
          ISERV(4) = 14
          DO K=1,2
            IFLAG1 = 0
            IF (ISERV(K) /= 0) THEN
              DO J=1,NFUNCT
                IF (ISERV(K) == NPC1(J)) THEN
                  GEO(ISERV(K+2),I) = J+PUN
                  IGEO(100+K,I)     = J
                  IFLAG1 = 1
                  EXIT
                ENDIF
              ENDDO
              IF (IFLAG1 == 0) THEN 
                CALL ANCMSG(MSGID=127,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ISERV(K+2))
              ENDIF
            ENDIF
          ENDDO    
          ! Compute max slop for damping
          IFUNC = IGEO(102,I) 
          IF (IFUNC /= 0)THEN   
            YFAC    = GEO(132,I)
            X_SCALE = GEO(18,I)                    
            IC1 = NPC(IFUNC)                        
            IC2 = NPC(IFUNC+1)                      
            X0  = PLD(IC1)                           
            EMAX = ZERO                             
            DO II = IC1,IC2-4,2                     
               JJ = II+2                            
               DX = PLD(JJ)   - X0                  
               DY = PLD(JJ+1) - PLD(II+1)           
               Y0 = PLD(II+1)                      
               Y1 = PLD(JJ+1)                      
               DERI = YFAC * X_SCALE * DY / DX                
               X1 = PLD(JJ)
               EMAX = MAX(EMAX,DERI)               
               X0 = PLD(JJ)                        
            ENDDO                                   
            GEO(141,I) = EMAX                      
          ENDIF      
        ENDIF
C
 420  CONTINUE

C
C
C     4) CONCENTRATED LOADS
C
      DO 460 I=1,NCONLD-NPRELD
        DO 440 J=1,NFUNCT
          IF(IBCL(3,I) == NPC1(J)) THEN
            IBCL(3,I)=J
            GOTO 460
          ENDIF
 440    CONTINUE
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='CONCENTRED LOADS',
     .              I1=IBCL(3,I))
 460  CONTINUE
C
C     5) PRESSURE LOADS
C
      DO 500 I=1,NPRELD
        DO 480 J=1,NFUNCT
          IF(IPRES(5,I) == NPC1(J)) THEN
            IPRES(5,I)=J
            GO TO 500
          ENDIF
 480    CONTINUE
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='PRESSURE LOADS',
     .              I1=IPRES(5,I))
 500  CONTINUE
C
C     6) FIXED VELOCITIES
C
      DO 540 I=1,NFXVEL
        DO 520 J=1,NFUNCT
          IF(IBFV(3,I) == NPC1(J)) THEN
            IBFV(3,I)=J
            GOTO 540
          ENDIF
 520    CONTINUE
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='IMPOSED VELOCITIES',
     .              I1=IBFV(3,I))
 540  CONTINUE
c
      DO 560 I=1,NFXVEL
        DO J=1,NFUNCT
          IF (IBFV(15,I)== 0) THEN 
            GOTO 560
          ELSE    
            IF(IBFV(15,I) == NPC1(J)) THEN
              IBFV(15,I)=J
              GOTO 560
            ENDIF
          ENDIF
        END DO
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='IMPOSED VELOCITIES',
     .              I1=IBFV(3,I))
 560  CONTINUE
C

C=======================================================================
C
C     (II) SKEW
C
C=======================================================================
C
C     1) BOUNDARY CONDITIONS
C
c      DO 660 I=1,NUMNOD
c        DO 640 J=0,NUMSKW
c          IF(ISKEW(I) == ISKN(4,J+1)) THEN
c            ISKEW(I)=J+1
c            GO TO 660
c          ENDIF
c 640    CONTINUE
c        CALL ANSTCKC(19,'BOUNDARY CONDITIONS')
c        CALL ANSTCKI(ISKEW(I))
c        CALL ANCERR(137,ANINFO_BLIND_1)
c 660  CONTINUE
C
C     2) CONCENTRATED LOADS
C
c      DO 700 I=1,NCONLD-NPRELD
c        IS=IBCL(2,I)/10
c          DO 680 J=0,NUMSKW
c            IF(IS == ISKN(4,J+1)) THEN
c              IBCL(2,I)=(J+1)*10+MOD(IBCL(2,I),10)
c              GO TO 700
c            ENDIF
c 680      CONTINUE
c          CALL ANSTCKC(18,'CONCENTRATED LOADS')
c          CALL ANSTCKI(IS)
c          CALL ANCERR(137,ANINFO_BLIND_1)
c 700  CONTINUE


C
C     3) FIXED VELOCITIES
C
c      DO 745 I=1,NFXVEL
c        IF (IBFV(9,I)>0) THEN
c          IS=IBFV(9,I)
c          JJ=(NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+1
c          DO J=1,NUMFRAM
c            JJ = JJ+1
c            IF(IS == ISKN(4,JJ)) THEN
c              IBFV(9,I)=J+1
c              GO TO 745
c            ENDIF
c          ENDDO
c        ELSE
c          IS=IBFV(2,I)/10
c          DO J=0,NUMSKW
c            IF(IS == ISKN(4,J+1)) THEN
c              IBFV(2,I)=(J+1)*10+MOD(IBFV(2,I),10)
c              GO TO 745
c            ENDIF
c          ENDDO
c        ENDIF
c        CALL ANSTCKC(18,'IMPOSED VELOCITIES')
c        CALL ANSTCKI(IS)
c        CALL ANCERR(137,ANINFO_BLIND_1)
c 745  CONTINUE


C---------------------------
C     6) FIXED temperatures
C---------------------------
      DO 751 I=1,NFXTEMP
        DO 750 J=1,NFUNCT
          IF(IBFT(2,I) == NPC1(J)) THEN
            IBFT(2,I)=J
            GOTO 751
          ENDIF
 750    CONTINUE
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='IMPOSED TEMPERATURE',
     .              I1=IBFT(2,I))
 751  CONTINUE
C------------------------------
C     6) FIXED convective flux
C------------------------------
      DO 753 I=1,NUMCONV
        DO 752 J=1,NFUNCT
          IF(IBCV(5,I) == NPC1(J)) THEN
            IBCV(5,I)=J
            GOTO 753
          ENDIF
 752    CONTINUE
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='FIXED FLUX',
     .              I1=IBCV(5,I))
 753  CONTINUE
C-----------------------------
C     6) FIXED radiative flux
C-----------------------------
      DO 755 I=1,NUMRADIA
        DO 754 J=1,NFUNCT
          IF(IBCR(5,I) == NPC1(J)) THEN
            IBCR(5,I)=J
            GOTO 755
          ENDIF
 754    CONTINUE
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='FIXED RADIATIVE FLUX',
     .              I1=IBCR(5,I))
 755  CONTINUE
C---------------------------
C     7) FIXED heat flux
C---------------------------
      DO 757 I=1,NFXFLUX
        DO 756 J=1,NFUNCT
          IF(IBFFLUX(5,I) == NPC1(J)) THEN
            IBFFLUX(5,I)=J
            GOTO 757
          ENDIF
 756    CONTINUE
        CALL ANCMSG(MSGID=120,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              C1='FIXED HEAT FLUX',
     .              I1=IBFFLUX(5,I))
 757  CONTINUE

 
C
c     4) PID SOLID, GENERAL SPRING, POROUS MEDIUM
C
c      DO 780 I=1,NUMGEO
c        IGTYP=IGEO(11,I)
c        IF (IGTYP == 6 .OR. IGTYP == 21 .OR. IGTYP == 22) THEN
c          IS=-IGEO(2,I)
c          IF(IS>=0) THEN
c            DO K=0,NUMSKW
c              IF(IS == ISKN(4,K+1)) THEN
c                IGEO(2,I)=-(K+1)
c                GO TO 780
c              ENDIF
c            ENDDO
c          CALL ANSTCKC(17,'ORTHOTROPIC SOLID')
c          CALL ANSTCKI(IS)
c          CALL ANCERR(137,ANINFO_BLIND_1)
c          ENDIF
c        ELSEIF(IGTYP == 34)THEN
c          IS=NINT(GEO(2,I))
c          IF (IS/=0)THEN
c           DO 758 K=0,NUMSKW
c            IF(IS == ISKN(4,K+1)) THEN
c              GEO(2,I)=(K+1)+PUN
c              IGEO(2,I)=K+1
c              GO TO 780
c            ENDIF
c 758       CONTINUE
c
c           CALL ANSTCKC(18,'GENERAL SPH PID')
c           CALL ANSTCKI(IS)
c           CALL ANCERR(137,ANINFO_BLIND_1)
c          ENDIF

c        ELSEIF(IGTYP == 8.OR.IGTYP == 13.OR.IGTYP == 25.OR.
c     .                      (IGTYP>=29.AND.IGTYP<50)) THEN
c          IS=IGEO(2,I)
c          DO 760 K=0,NUMSKW
c            IF(IS == ISKN(4,K+1)) THEN
c              GEO(2,I)=(K+1)+PUN
c              IGEO(2,I)=K+1
c              GO TO 780
c            ENDIF
c 760      CONTINUE
c          CALL ANSTCKC(18,'GENERAL SPRING PID')
c          CALL ANSTCKI(IS)
c          CALL ANCERR(137,ANINFO_BLIND_1)
c        ELSEIF(IGTYP == 15)THEN
c          IS=NINT(GEO(27,I))
c          DO 765 K=0,NUMSKW
c            IF(IS == ISKN(4,K+1)) THEN
c              GEO(27,I)=(K+1)+PUN
c              GO TO 780
c            ENDIF
c 765      CONTINUE
c          CALL ANSTCKC(17,'POROUS MEDIUM PID')
c          CALL ANSTCKI(IS)
c          CALL ANCERR(137,ANINFO_BLIND_1)
c        ENDIF
c 780  CONTINUE
C
C     5) RIGID BODIES
C
c      DO 810 I=1,NRBYKIN
c          IS = NPBY(9,I)
c          DO 800 J=0,NUMSKW
c            IF(IS == ISKN(4,J+1)) THEN
c              NPBY(9,I)=J+1
c              GO TO 810
c            ENDIF
c 800      CONTINUE
c          CALL ANSTCKC(12,'RIGID BODIES')
c          CALL ANSTCKI(IS)
c          CALL ANCERR(137,ANINFO_BLIND_1)
c 810  CONTINUE
C
C     5) ACCELEROMETER
C
c      DO 850 I=1,NACCELM
c          IS=LACCELM(3,I)
c          IF(LACCELM(1,I) > 0) THEN
c             DO J=0,NUMSKW
c               IF(IS == ISKN(4,J+1)) THEN
c                 LACCELM(3,I)=J+1
c                 GO TO 850
c               ENDIF
c             ENDDO
c             CALL ANSTCKC(13,'ACCELEROMETER')
c             CALL ANSTCKI(IS)
c             CALL ANCERR(137,ANINFO_BLIND_1)
c         ENDIF
c 850  CONTINUE
c      DO 880 I=1,NBCSLAG
c          IS=IBCSLAG(4,I)
c          DO J=0,NUMSKW
c            IF(IS == ISKN(4,J+1)) THEN
c              IBCSLAG(4,I)=J+1
c              GO TO 880
c            ENDIF
c          ENDDO
c          CALL ANSTCKC(36,'BOUNDARY CONDITIONS WITH LAGR. MULT.')
c          CALL ANSTCKI(IS)
c          CALL ANCERR(137,ANINFO_BLIND_1)
c 880  CONTINUE
C
C     GRAVITY
c      DO 890 I=1,NGRAV
c        NOSKEW=IGRV(2,I)/10
c        ND    =IGRV(2,I)-10*NOSKEW
c        DO 895 J=0,NUMSKW
c          IF(NOSKEW == ISKN(4,J+1)) THEN
c            IGRV(2,I)=ND+10*(J+1)
c            GO TO 890
c          ENDIF
c 895    CONTINUE
c        CALL ANSTCKC(7,'GRAVITY')
c        CALL ANSTCKI(NOSKEW)
c        CALL ANCERR(137,ANINFO_BLIND_1)
c 890  CONTINUE
C
C     Raileigh DAMPING
C
c      IF(NRDAMP>=15)THEN
c       DO 670 I=1,NDAMP
c        NOSKEW=NINT(DAMPR(15,I))
c        DO 675 J=0,NUMSKW
c          IF(NOSKEW == ISKN(4,J+1)) THEN
c            DAMPR(15,I)=J+1
c            GO TO 670
c          ENDIF
c 675    CONTINUE
c        CALL ANSTCKC(16,'RAILEIGH DAMPING')
c        CALL ANSTCKI(NOSKEW)
c        CALL ANCERR(137,ANINFO_BLIND_1)
c 670   CONTINUE
c      END IF

C=======================================================================
C
C    (III) SENSOR NUMBERING
C
C=======================================================================
C----------------------------
C     MATERIAL LAWS
C----------------------------
C
      DO I=1,NUMMAT
        ILAW = NINT(PM(19,I))
        IF (ILAW == 19) THEN
          ID = IPM(1,I)
          CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
          IADD  = IPM(7,I) - 1
          ISENS = BUFMAT(IADD+13)
          IOK = 0
          IF (ISENS > 0) THEN
            DO J=1,SENSORS%NSENSOR
              IF (ISENS == SENSORS%SENSOR_TAB(J)%SENS_ID) THEN
                BUFMAT(IADD+13) = J
                IOK = 1
                EXIT
              END IF
            ENDDO
            IF (IOK == 0) THEN  
              CALL ANCMSG(MSGID=1240,ANMODE=ANINFO,MSGTYPE=MSGWARNING,
     .                    I1=ID,C1=TITR,I2=ISENS) 
              BUFMAT(IADD+13) = 0   ! If not found set SENSOR ID to 0 as if there is no sensor
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-------------------------
C  CONVECTIVE HEAT FLUX
C-------------------------
       DO I=1,NUMCONV
          ISENS = IBCV(6,I)
          IF(ISENS/=0) THEN
            DO J=1,SENSORS%NSENSOR
              IF(ISENS == SENSORS%SENSOR_TAB(J)%SENS_ID) THEN
                 IBCV(6,I) = J
                 GO TO 801
              ENDIF
            ENDDO  
            CALL ANCMSG(MSGID=1605,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                  C1='CONVECTIVE HEAT FLUX',I1=ISENS)
          ENDIF
 801   CONTINUE
       ENDDO
C------------------------
C  RADIATIVE HEAT FLUX
C------------------------
       DO I=1,NUMRADIA
          ISENS = IBCR(6,I)
          IF(ISENS/=0) THEN
            DO J=1,SENSORS%NSENSOR
              IF(ISENS == SENSORS%SENSOR_TAB(J)%SENS_ID) THEN
                 IBCR(6,I) = J
                 GO TO 802
              ENDIF
            ENDDO  
           CALL ANCMSG(MSGID=1605,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                 C1='RADIATIVE HEAT FLUX',I1=ISENS)
          ENDIF
 802   CONTINUE
       ENDDO
C---------------------
C  IMPOSED HEAT FLUX
C---------------------
       DO I=1,NFXFLUX
          ISENS = IBFFLUX(6,I)
          IF(ISENS/=0) THEN
            DO J=1,SENSORS%NSENSOR
              IF(ISENS == SENSORS%SENSOR_TAB(J)%SENS_ID) THEN
                 IBFFLUX(6,I) = J
                 GO TO 803
              ENDIF
            ENDDO  
           CALL ANCMSG(MSGID=1605,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                 C1='IMPOSED HEAT FLUX',I1=ISENS)
          ENDIF
 803   CONTINUE
       ENDDO

C---------------------
C  IMPOSED TEMPERATURE
C---------------------
      DO I=1,NFXTEMP
        ISENS = IBFT(3,I)
        IF (ISENS > 0) THEN
          DO J=1,SENSORS%NSENSOR
            IF (ISENS == SENSORS%SENSOR_TAB(J)%SENS_ID) THEN
               IBFT(3,I) = J
               EXIT
            ENDIF
          ENDDO  
        END IF
      ENDDO

C---------------------
C  IMPOSED DISPLACEMENTS/VELOCITIES/ACCELERATIONS
C---------------------
      DO I=1,NFXVEL
        ISENS = IBFVEL(4,I)
        IF (ISENS > 0) THEN
          DO J=1,SENSORS%NSENSOR
            IF (ISENS == SENSORS%SENSOR_TAB(J)%SENS_ID) THEN
               IBFVEL(4,I) = J
               EXIT
            ENDIF
          ENDDO  
        END IF
      ENDDO
c
C----------------------------
C     (IV) TRAITEMENT DES TABLES
C----------------------------
C
C         1) LOIS 73
C
      DO I=1,NUMMAT
C
        ILAW=NINT(PM(19,I))
C
        ID=IPM(1,I)
        CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
        IF(ILAW == 73) THEN
          ITABLE=IPM(227,I)
          IF(ITABLE/=0)THEN
           DO J=1,NTABLE
            IF(ITABLE == TABLE(J)%NOTABLE)  THEN
              IPM(227,I)=J
              GOTO 900
            ENDIF
           END DO
           CALL ANCMSG(MSGID=779,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
          ENDIF
 900     CONTINUE
         ITABLE=IPM(227,I)
         IF(TABLE(ITABLE)%NDIM/=3)THEN
           CALL ANCMSG(MSGID=780,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
         END IF
c -- young evolution
         IF (NF > 0) THEN
            IFE=IPM(10+NF,I)
            IF (IFE /= 0)THEN
              IE =NPC(IFE)                    
              IE2=NPC(IFE+1)                  
              DO II = IE+1,IE2-3,2            
                 IF(PLD(II) < PLD(II+2))THEN  
                    CALL ANCMSG(MSGID=975,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                    EXIT                      
                 ENDIF                        
              ENDDO                           
            ENDIF
         ENDIF

        ELSEIF(ILAW == 74)THEN
          ITABLE=IPM(227,I)
          IF(ITABLE/=0)THEN
           DO J=1,NTABLE
            IF(ITABLE == TABLE(J)%NOTABLE)  THEN
              IPM(227,I)=J
              GOTO 901
            ENDIF
           END DO
           CALL ANCMSG(MSGID=779,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
          ENDIF
 901     CONTINUE
         ITABLE=IPM(227,I)
         IF(TABLE(ITABLE)%NDIM/=2.AND.TABLE(ITABLE)%NDIM/=3)THEN
           CALL ANCMSG(MSGID=823,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
         END IF
c -- young evolution
         IF (NF > 0) THEN
            IFE=IPM(10+NF,I)
            IF(IFE /= 0)THEN
              IE =NPC(IFE)
              IE2=NPC(IFE+1) 
              DO II = IE+1,IE2-3,2
                 IF(PLD(II) < PLD(II+2))THEN
                    CALL ANCMSG(MSGID=975,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                    EXIT 
                 ENDIF       
              ENDDO
            ENDIF
         ENDIF


        ELSEIF(ILAW == 80)THEN
          DO 980 K = 1,IPM(226,I)!NTABLE
          ITABLE= IPM(226+k,I)
          IADD  = IPM(7,I) - 1
          IF(ITABLE/=0)THEN
           DO J=1,NTABLE
            IF(ITABLE == TABLE(J)%NOTABLE)  THEN
              IPM(226+k,I)=J
              ITABLE=IPM(226+K,I)
              IF(TABLE(ITABLE)%NDIM >= 2 )THEN
                BUFMAT(IADD+15) = ZERO                  
              ENDIF
              IF(TABLE(ITABLE)%NDIM > 3 )THEN
               CALL ANCMSG(MSGID=1030,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
                EXIT
              END IF
              GOTO 980
            ENDIF
           END DO
           CALL ANCMSG(MSGID=779,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ITABLE)
          ENDIF
 980     CONTINUE
c -- young evolution
         IF (NF > 0) THEN
            IFE=IPM(10+NF,I)
            IF(IFE /= 0)THEN
              IE =NPC(IFE)
              IE2=NPC(IFE+1) 
              DO II = IE+1,IE2-3,2
                 IF(PLD(II) < PLD(II+2))THEN
                    CALL ANCMSG(MSGID=975,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                    EXIT 
                 ENDIF       
              ENDDO
            ENDIF
         ENDIF

        ENDIF
       END DO
C----------------------
C      USER
C----------------------
      CALL INIGUSER(BUFGEO,IGEO,IPM,NPC1)
C------------------------------------------------------------------
C      POINTS DE DETONATION,
C      SEGMENTS DE DETONATION,
C      DETONATION AVEC ECRAN,
C      PLANAR DETONATION WAVE.
C------------------------------------------------------------------
C Traitement des id matriau (Mdet) dans lecdet.F avec le check des user flags

      RETURN
C-----
      END
C
Chd|====================================================================
Chd|  M20DCOD                       source/system/fsdcod.F        
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE M20DCOD(MLAW_TAG, IPM  ,PM)
      USE MESSAGE_MOD
C     SOUS PROGRAMME DE DECODAGE DES SOUS-MATERIAUX (LOI 20 et LOI 151)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE ELBUFTAG_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MLAW_TAG_), TARGET, DIMENSION(NUMMAT)  :: MLAW_TAG
      INTEGER IPM(NPROPMI,NUMMAT)
      my_real  PM(NPROPM,NUMMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ILAW, J, K, IS, NF,ILAWk
      my_real PUN,RHO_MAX
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      LOGICAL PASSED
      TYPE(MLAW_TAG_), POINTER :: MTAG, MTAGk      
C-----------------------------------------------
      DATA PUN/0.1/
C-----------------------------------------------
C     TRAITEMENT DES SOUS-MATERIAUX (LOI 20)
C-----------------------------------------------
C
      DO I=1,NUMMAT
C
        ILAW=NINT(PM(19,I))
C
C
        IF(ILAW == 20) THEN
C
          ID=IPM(1,I)
          CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
          NF=NINT(PM(40,I))
          RHO_MAX=ZERO
          DO K=1,2
            PASSED=.TRUE.          
            IS=NINT(PM(20+K,I))
            IF(IS/=0) THEN
              DO J=1,NUMMAT
                IF(IS == IPM(1,J))  THEN
                  PM(20+K,I)=J+PUN
                  RHO_MAX=MAX(RHO_MAX,PM(1,J))
                  GOTO 200
                ENDIF
              ENDDO
              PASSED=.FALSE.
            ENDIF
            CALL ANCMSG(MSGID=128,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,C1=TITR,
     .                   I2=IS)
	    CALL ARRET(2)
 200        CONTINUE !found
            !LAW20 material buffer is dimensioned from submaterial buffer dimensions.
            IF(PASSED)THEN
              ILAWk = IPM(2,J)            
              MTAG  => MLAW_TAG(I)   
              MTAGk => MLAW_TAG(J) 
              MTAG%L_BFRAC= MAX(MTAG%L_BFRAC, MTAGk%L_BFRAC)  	        
              MTAG%L_TEMP = MAX(MTAG%L_TEMP , MTAGk%L_TEMP )
              MTAG%L_PLA  = MAX(MTAG%L_PLA  , MTAGk%L_PLA  )       
              MTAG%G_BFRAC= MAX(MTAG%G_BFRAC, MTAGk%G_BFRAC)        
              MTAG%G_TEMP = MAX(MTAG%G_TEMP , MTAGk%G_TEMP )          
              MTAG%G_PLA  = MAX(MTAG%G_PLA  , MTAGk%G_PLA  )             
            ENDIF            
          ENDDO !next K
          PM(91,I)=RHO_MAX

        ELSE IF (ILAW == 151) THEN
           ID=IPM(1,I)
           CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
           NF = IPM(20, I) ! Number of submaterials
           RHO_MAX=ZERO
           DO K = 1, NF
              IS = IPM(20 + K, I)
              DO J = 1, NUMMAT
                 IF (IS == IPM(1, J)) THEN
                    IPM(20 + K, I) = J
                    ILAWk = IPM(2,J)            
                    MTAG  => MLAW_TAG(I)   
                    MTAGk => MLAW_TAG(J) 
                    MTAG%L_BFRAC= MAX(MTAG%L_BFRAC, MTAGk%L_BFRAC)  	        
                    MTAG%L_TB= MAX(MTAG%L_BFRAC, MTAGk%L_TB)  	        
                    MTAG%L_TEMP = MAX(MTAG%L_TEMP , MTAGk%L_TEMP )
                    MTAG%L_PLA  = MAX(MTAG%L_PLA  , MTAGk%L_PLA  )       
                    MTAG%G_BFRAC= MAX(MTAG%G_BFRAC, MTAGk%G_BFRAC)        
                    MTAG%G_TB= MAX(MTAG%G_TB, MTAGk%G_TB)        
                    MTAG%G_TEMP = MAX(MTAG%G_TEMP , MTAGk%G_TEMP )          
                    MTAG%G_PLA  = MAX(MTAG%G_PLA  , MTAGk%G_PLA  ) 
                    RHO_MAX=MAX(RHO_MAX,PM(1,J))
                 ENDIF
              ENDDO!next J
              PM(91,I)=RHO_MAX              
           ENDDO
        ENDIF
      ENDDO !next I=1,NUMMAT
C
      RETURN
      END
