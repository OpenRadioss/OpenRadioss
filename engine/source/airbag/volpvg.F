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
C new routine (starter input format >= 4.4)
Chd|====================================================================
Chd|  VOLPVGA                       source/airbag/volpvg.F        
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VOLPVGA(IVOLU ,RVOLU   ,VOL     ,FSAV    ,NVENT   ,
     .                 IBAGHOL ,RBAGHOL ,PMAIN )
C-----------------------------------------------
C      MONITORED VOLUMES TYPE PERFECT GAS, INPUT STARTER >= 4.4 (PRESSURE).
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(*),NVENT,IBAGHOL(NIBHOL,*),PMAIN
      my_real RVOLU(*),VOL,FSAV(*),RBAGHOL(NRBHOL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDEF,IV, II
      my_real
     .   VINC,GAMA,PRES,PMAX,VEPS,AREA,POLD,VOLD,
     .   Q,QOLD,DV,AMU,ROT,
     .   AMTOT,AMTOT_OLD,ENERGY,ENERG_OLD,DMOUT,DEOUT,FAC,
     .   PDEF,DTPDEFC,M0, VOL0
      my_real
     .   TRELAX,DEIN, PEXT, PINI, CV, TEMPERATURE
      INTEGER :: IEQUI
      my_real :: MIN_TVENT
C-----------------------------------------------
      MIN_TVENT = EP30
      DO II = 1, NVENT
         MIN_TVENT = MIN(MIN_TVENT, RBAGHOL(3, II))
      ENDDO

      PEXT = RVOLU(3)
      PINI = RVOLU(56)
      POLD   =RVOLU(12)
      M0 = RVOLU(57)
      VOL0 = RVOLU(4)
      DO IV=1,NVENT
        IDEF   = IBAGHOL(1,IV)
        PDEF   = RBAGHOL(1,IV)
        DTPDEFC= RBAGHOL(5,IV)
        IF(IDEF <= 0 .AND. POLD > PDEF + PEXT)
     .     RBAGHOL(5,IV)=DTPDEFC+DT1
      ENDDO
C------------------------
      IDEF   =IVOLU(14)
      IEQUI = IVOLU(15)
      GAMA   =RVOLU(1)
      VINC   =RVOLU(5)
      PMAX   =RVOLU(6)
      ENERG_OLD=RVOLU(13)
      VOLD   =RVOLU(16)
      VEPS   =RVOLU(17)
      AREA   =RVOLU(18)
      QOLD   =RVOLU(23)
      VOL    =VOL + VEPS
      DMOUT  =RVOLU(24)
      DEOUT  =RVOLU(22)
      AMTOT  =RVOLU(20)
      AMU    =RVOLU(2)
      ROT    =RVOLU(21)
      TRELAX =RVOLU(48)
      CV = RVOLU(19)
      DV     =VOL-VOLD
      TEMPERATURE = ZERO
C
      IF(IDEF==1)THEN
        PRES  = PEXT
        Q     = ZERO
      ELSE
         IF (IEQUI == 0 .OR. TRELAX == ZERO) THEN
            AMTOT=AMTOT-DMOUT*DT1
C     
C     CALCUL DE L ENERGIE PUIS DE LA PRESSION
            FAC  = HALF*(GAMA-ONE)*DV
            IF(TRELAX == ZERO .OR. TT > TRELAX)THEN
               DEIN = ZERO
            ELSE
               DEIN = RVOLU(52)
            ENDIF
            ENERGY= ((ONE-FAC/(VOLD-VINC))*ENERG_OLD +(DEIN-DEOUT)*DT1) /
     .           (ONE+FAC/(VOL-VINC))
            ENERGY = MAX(ENERGY,ZERO)
C     
            PRES=(GAMA-ONE)*ENERGY/(VOL-VINC)
         ELSE IF (IEQUI == 1) THEN
            IF (TT <= TRELAX) THEN
               PRES = PEXT + TT * (PINI - PEXT) / TRELAX 
               ENERGY = PRES * (VOL - VINC) / (GAMA - ONE) 
               AMTOT = M0 * PRES * VOL / (PEXT * VOL0)
            ELSE
               PRES = PEXT * AMTOT / M0 * VOL0 / VOL
               ENERGY = PRES * (VOL - VINC) / (GAMA - ONE)
            ENDIF
         ELSE IF (IEQUI == 2) THEN
            IF (TT <= TRELAX) THEN
               PRES = PEXT + TT * (PINI - PEXT) / TRELAX 
               ENERGY = PRES * (VOL - VINC) / (GAMA - ONE) 
               AMTOT = M0 * (PRES / PEXT)**(ONE / GAMA) * VOL / VOL0
            ELSE
               PRES = PEXT * (AMTOT / M0 * VOL0 / VOL)**GAMA
               ENERGY = PRES * (VOL - VINC) / (GAMA - ONE)
            ENDIF
         ENDIF
         IF (IEQUI /= 0) THEN
            IF (TT > MIN_TVENT .AND. TT > TRELAX) THEN
!     IEQUI = 0
               IVOLU(15) = 0
            ENDIF
         ENDIF
         
         IF(DT1==ZERO.OR.DV>ZERO)THEN
            Q=ZERO
         ELSE
            Q=-AMU*SQRT(PRES*AREA*ROT/VOL)*DV/AREA/DT1
         ENDIF
C     
         IF(PRES>PMAX)THEN
            IDEF=1
            PRES = PEXT
            Q    = ZERO
         ENDIF
         IF (IEQUI > 0) THEN
            TEMPERATURE = ENERGY / (CV * AMTOT)
         ENDIF
         RVOLU(20) = AMTOT
         RVOLU(13) = ENERGY
      ENDIF
C
      IVOLU(14) = IDEF
      RVOLU(16) = VOL
      RVOLU(12) = PRES
C
      IF (ISPMD+1==PMAIN) THEN
        TFEXT=TFEXT+(HALF*(Q+QOLD+PRES+POLD)-PEXT)*DV
        FSAV(1)=AMTOT
        FSAV(2)=VOL
        FSAV(3)=PRES
        FSAV(4)=AREA
        FSAV(5)=TEMPERATURE
        FSAV(6)=ZERO
        FSAV(7)=ZERO
        FSAV(8)=ZERO
        FSAV(9)=ZERO	
        FSAV(10)=ZERO
        FSAV(11)=ZERO	
        FSAV(12)=GAMA
      ENDIF
C
      RVOLU(22) = ZERO
      RVOLU(23) = Q
      RVOLU(24) = ZERO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  VOLPVGB                       source/airbag/volpvg.F        
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE VOLPVGB(IVOLU ,RVOLU   ,VOL     ,FSAV    ,NVENT   ,
     .                 IBAGHOL ,RBAGHOL ,N       ,NN      ,IGRSURF ,
     .                 IPARG   ,ELBUF_TAB,FR_MV, IGROUPC,IGROUPTG)
C-----------------------------------------------
C      MONITORED VOLUMES TYPE PERFECT GAS, INPUT STARTER >= 4.4 (FUITES).
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
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(*),NVENT,IBAGHOL(NIBHOL,*), FR_MV(*),
     .        NN,IPARG(NPARG,*)
      INTEGER, INTENT(IN) :: IGROUPC(*), IGROUPTG(*)
      my_real
     .   RVOLU(*),FSAV(*),RBAGHOL(NRBHOL,*),N(3,*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K,IDEF,KK,PMAIN,
     .   II,IPVENT,NNC,KAD,IPORT,IPORP,IPORA,
     .   IOFF,NG,NEL,ISTRA,IEXPAN,IOK

      my_real
     .   GAMA, PEXT, PDEF, AVENT, VOL, VINC,
     .   AMTOT, ENERGY, P, RO, TEMP(4), M0, VOL0,
     .   U, DEOUT, DMOUT, TVENT,AREA, PCRIT, AOUTOT,
     .   APVENT,AOUT,FLOUT,DE,DERI,DTPDEFI,DTPDEFC,
     .   F1(NN),SCALT,SCALP,SCALS
      my_real 
     .   GET_U_FUNC
      EXTERNAL GET_U_FUNC
      DOUBLE PRECISION 
     .   FRMV6(6)
      my_real,
     .  DIMENSION(:), POINTER :: OFFG
C-----------------------------------------------
      PMAIN = FR_MV(NSPMD+2)
      IDEF  = IVOLU(14)
      IF(IDEF>0)GOTO 999
C--------------------------------
       GAMA   =RVOLU(1)
       PEXT   =RVOLU(3)
       VINC   =RVOLU(5)
       VOL    =RVOLU(16)
       ENERGY =RVOLU(13)
       AMTOT  =RVOLU(20)
       SCALT  =RVOLU(26)    
       SCALP  =RVOLU(27)    
       SCALS  =RVOLU(28)
C
       P      =RVOLU(12)
       AREA   =RVOLU(18)
C
       RO    = AMTOT/(VOL-VINC)
       PCRIT = P*(TWO/(GAMA+ONE))**(GAMA/(GAMA-ONE))
C
C      FLUX SORTANT PAR LES TROUS
C
       AOUTOT=ZERO
       DO II = 1, NVENT
        IDEF   = IBAGHOL(1, II)
        IPVENT = IBAGHOL(2, II)
C
        PDEF   = RBAGHOL(1, II)
        DTPDEFI= RBAGHOL(4, II)
        DTPDEFC= RBAGHOL(5, II)
        AVENT  = RBAGHOL(2, II)
        TVENT  = RBAGHOL(3, II)
C
        IF(IDEF<=0.AND.P>PDEF+PEXT.
     .               AND.DTPDEFC>DTPDEFI.
     .               AND.VOL>0.001*AREA**1.5) THEN
         IDEF=ABS(IDEF)
         IF(ISPMD+1==PMAIN) THEN
         WRITE(IOUT,*)
     .         ' *** MONITORED VOLUME MEMBRANE IS DEFLATED ***'
         WRITE(IOUT,*)' *** MONITORED VOLUME ',IVOLU(1),
     .                ' VENT HOLES MEMBRANE NUMBER ',II,' ***'
         WRITE(ISTDO,*)' *** VENT HOLES MEMBRANE IS DEFLATED ***'
         ENDIF
        ENDIF
        IF(IDEF<=0 .AND. TT>TVENT) THEN
         IDEF=ABS(IDEF)
         IF(ISPMD+1==PMAIN) THEN
         WRITE(IOUT,*) 
     .         ' *** MONITORED VOLUME VENTING STARTS ***'
         WRITE(IOUT,*) ' *** MONITORED VOLUME ',IVOLU(1),
     .                ' VENT HOLES MEMBRANE NUMBER ',II,' ***'
         WRITE(ISTDO,*)' *** VENTING STARTS ***'
         ENDIF
        ENDIF
C
        IF(IPVENT/=0)THEN
C
          IF(IDEF==1)THEN
c            APVENT = ZERO
            NNC=IGRSURF(IPVENT)%NSEG
            DO KK=1,NNC
               IF(IGRSURF(IPVENT)%ELTYP(KK)==3)THEN
C                segment from shell
                 K=IGRSURF(IPVENT)%ELEM(KK)
               ELSEIF(IGRSURF(IPVENT)%ELTYP(KK)==7)THEN
C                segment from sh3n
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC
               ELSE
C                segment only
                 K=IGRSURF(IPVENT)%ELEM(KK) + NUMELC + NUMELTG
               ENDIF
               F1(KK) = SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
c               APVENT= APVENT+
c     .            SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
               KAD=KAD+NISX
            ENDDO
c            AOUT=APVENT
C
          ELSEIF(IDEF>=2)THEN
c            APVENT = ZERO
            NNC=IGRSURF(IPVENT)%NSEG
            DO KK=1,NNC
               IF(IGRSURF(IPVENT)%ELTYP(KK)==3)THEN
C                segment from shell
                 K=IGRSURF(IPVENT)%ELEM(KK)
                 IOK = 0
                 NG=IGROUPC(K)
                  ITY=IPARG(5,NG)
                  IF(ITY==3)THEN
                     IOK = 1
                   ENDIF
                 IF (IOK == 1) THEN
                   NEL   =IPARG(2,NG)
                   NFT   =IPARG(3,NG)
                   IAD   =IPARG(4,NG)
                   NPT   =IPARG(6,NG)
                   ISTRA =IPARG(44,NG)
                   JHBE  =IPARG(23,NG)
                   IEXPAN=IPARG(49,NG)
                   OFFG => ELBUF_TAB(NG)%GBUF%OFF
                   IOFF = INT(OFFG(K-NFT))
                 ELSE
                   IOFF = 1
                 ENDIF
               ELSEIF(IGRSURF(IPVENT)%ELTYP(KK)==7)THEN
C                segment from sh3n
                 K=IGRSURF(IPVENT)%ELEM(KK)
                 IOK = 0
                 NG=IGROUPTG(K)
                  ITY=IPARG(5,NG)
                  IF(ITY==7)THEN
                     IOK = 1
                   ENDIF
                 IF (IOK == 1) THEN
                   NEL   =IPARG(2,NG)
                   NFT   =IPARG(3,NG)
                   IAD   =IPARG(4,NG)
                   NPT   =IPARG(6,NG)
                   ISTRA =IPARG(44,NG)
                   JHBE  =IPARG(23,NG)
                   IEXPAN=IPARG(49,NG)
                   OFFG => ELBUF_TAB(NG)%GBUF%OFF
                   IOFF=INT(OFFG(K-NFT))
                 ELSE
                   IOFF = 1
                 ENDIF
                 K=K+NUMELC
               ELSE
C                segment only
                 IOFF=1
               ENDIF
               IF(IOFF==0) THEN
                 F1(KK) = SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
               ELSE
                 F1(KK) = ZERO
               END IF
c               IF(IOFF==0) APVENT= APVENT+
c     .                 SQRT( N(1,K)**2+N(2,K)**2+N(3,K)**2 )
               KAD=KAD+NISX
            ENDDO
c            AOUT=APVENT
          ENDIF
C
          
          IF (IDEF==1.OR.IDEF>=2) THEN
            DO KK = 1, 6
                FRMV6(KK) = ZERO
            END DO
            CALL SUM_6_FLOAT(1, NNC, F1, FRMV6, 1)
            IF (NSPMD>1) THEN          
                IF(FR_MV(ISPMD+1)/=0) THEN
                CALL SPMD_EXCH_FR6(FR_MV,FRMV6,6)
                END IF
            ENDIF
            APVENT = FRMV6(1)+FRMV6(2)+FRMV6(3)+
     .               FRMV6(4)+FRMV6(5)+FRMV6(6)
C
            AOUT = APVENT
          ELSE
            AOUT =AVENT
            AVENT=1.0
          END IF
        ELSE
          AOUT =AVENT
          AVENT=1.0
        ENDIF
C
        IF(IDEF>0 .AND. P>PEXT.
     .               AND.VOL>EM3*AREA**1.5)THEN
         IPORT =IBAGHOL(3,II)
         IPORP =IBAGHOL(4,II)
         IPORA =IBAGHOL(5,II)
         IF(IPORA/=0.AND.IPVENT/=0)THEN
           AOUT=AVENT*GET_U_FUNC(IPORA,AOUT*SCALS,DERI)
         ELSE
           AOUT=AVENT*AOUT
         ENDIF
         IF(IPORT/=0)AOUT=AOUT*GET_U_FUNC(IPORT,TT*SCALT,DERI)
         IF(IPORP/=0)AOUT=AOUT*GET_U_FUNC(IPORP,(P-PEXT)*SCALP,DERI)
         AOUTOT=AOUTOT+AOUT
        ENDIF
        IBAGHOL(1,II)=IDEF
       ENDDO
C-------
       IF(AOUTOT>0.)THEN
           PEXT = MAX(PEXT,PCRIT)
           U=TWO*GAMA/(GAMA-ONE)*P/RO*(ONE-(PEXT/P)**((GAMA-ONE)/GAMA))
           U=SQRT(U)
           DE=(ENERGY/(VOL-VINC)+P)*(PEXT/P)**(ONE/GAMA)
           U=MIN(U,(P-PEXT)*HALF*(VOL-VINC)
     .      /(GAMA-ONE)/DE/MAX(EM20,AOUTOT*DT1))
           U=MIN(U,HALF*(VOL-VINC)/MAX(EM20,AOUTOT*DT1))
           FLOUT=AOUTOT*U
           DEOUT=FLOUT*DE
           DMOUT=FLOUT*RO*(PEXT/P)**(ONE/GAMA)
       ELSE
           U=ZERO
           DEOUT=ZERO
           DMOUT=ZERO
           FLOUT= ZERO   
       ENDIF
C-------
       IF(ISPMD+1==PMAIN) THEN
         FSAV(6)=AOUTOT
         FSAV(7)=FLOUT/MAX(EM20,AOUTOT)
       ENDIF
C
       RVOLU(22)=RVOLU(22) + DEOUT
       RVOLU(24)=RVOLU(24) + DMOUT
C--------------------------------
 999  CONTINUE
      RETURN
      END
