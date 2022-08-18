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
Chd|  SENSOR1                       source/tools/sensor/sensor1.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        BUTTERWORTH                   source/tools/univ/butterworth.F
Chd|        SENSOR_DIST_SURF              source/tools/sensor/sensor_dist_surf.F
Chd|        SENSOR_ENERGY                 source/tools/sensor/sensor_energy.F
Chd|        SENSOR_HIC                    source/tools/sensor/sensor_hic.F
Chd|        SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|        SENSOR_TEMP                   source/tools/sensor/sensor_temp.F
Chd|        USENS_SHIFT_AB                source/user_interface/usensor.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR1(ACC       ,LSENS1    ,LSENS2   ,SENS1  ,SENS2   ,
     .                   X         ,V         ,IPARI    ,NPRW   ,XSENS   ,
     .                   FSAV      ,GAUGE     ,DIMFB    ,TABS   ,FBSAV6  ,
     .                   STABS     ,IPARSENS  ,FSAVSUB  ,ISENS  ,PARTSAV2,
     .                   SUBSET    ,SENSOR_TAB,A        ,IGRSURF,IGRNOD  ,
     .                   TEMP      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DIMFB,STABS,IPARSENS,ISENS
      INTEGER LSENS1(*),LSENS2(11,*),IPARI(NPARI,NINTER),NPRW(*),
     .        TABS(STABS)
      my_real ACC(LLACCELM,*),SENS1(*),SENS2(220,*),
     . X(3,NUMNOD),V(3,NUMNOD),A(3,NUMNOD),TEMP(NUMNOD),
     . FSAV(NTHVKI,*), GAUGE(LLGAUGE,*),FSAVSUB(NTHVKI,*),PARTSAV2(2,*),
     . XSENS(12)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE (SUBSET_) ,DIMENSION(NSUBS)  ,INTENT(IN) :: SUBSET
      TYPE(SURF_)    ,DIMENSION(NSURF)  ,INTENT(IN) :: IGRSURF
      TYPE(GROUP_)   ,DIMENSION(NGRNOD) ,INTENT(IN) :: IGRNOD
      TYPE (SENSOR_STR_)  :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TYP, I,J,K,K0,IAD,ACTIVE,ACT1,ACT2,IDIR,NSENS,NAC,N1,N2,IACC,
     . IN,IS,N5,IGAU,ISECT,INTSUB,ID_INTER,IRWAL,IRBOD,IPART,ISUBS,NP,M
      my_real
     . AMIN, DURE, TACT,TACT1,TACT2,TDELAY, TSTART, DD, DM1, DM2,
     . V1,V2,V3,FV1,FV2,FV,VV,VV1,VV2,FREQ,VEL,TSTOPS,
     . FX,FY,FZ,FF,FMIN,FMAX,WMAX,DX,DY,DZ,WORK,IMP,EINT,EKIN,IEMIN,
     . IEMAX,KEMIN,KEMAX,IETOL,IETIME,KETOL,KETIME,EI_SAV,EK_SAV
       INTEGER :: NGR,NBR_GROUP,L,ITSK
C=======================================================================
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      NSENS  = LSENS1(1)
      TYP    = LSENS1(2)
      NAC    = LSENS1(3)
      TDELAY = SENS1(1)
      TSTART = SENS1(2)
      INTSUB = 0
C
      IF (TYP == 0) THEN
C--------------------------------
C         CAPTEUR - START TIME
C--------------------------------
        IF (TT >= TDELAY .AND. LSENS1(3) == 0) THEN
            LSENS1(3) = 1
            SENS1(2)  = TT
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TT
#include "lockoff.inc"
        ENDIF
C--------------------------------
C         CAPTEUR - ACCELEROMETRE
C--------------------------------
      ELSEIF(TYP == 1)THEN
        IF(TSTART-TDELAY.LE.TT)RETURN
        DO I = 1,NAC
          AMIN = SENS2(1+(I-1)*3,1)
          DURE = SENS2(2+(I-1)*3,1)
          TACT = SENS2(3+(I-1)*3,1)
          IACC = LSENS2(1+(I-1)*2,1) 
          IDIR = LSENS2(2+(I-1)*2,1)
          ACTIVE = 0
          IF(IDIR == 1)THEN
C           X
            IF(ABS(ACC(20,IACC)) > AMIN)ACTIVE = 1
          ELSEIF(IDIR == 2)THEN
C           Y
            IF(ABS(ACC(21,IACC)) > AMIN)ACTIVE = 1
          ELSEIF(IDIR == 4)THEN
C           Z
            IF(ABS(ACC(22,IACC)) > AMIN)ACTIVE = 1
          ELSEIF(IDIR == 3)THEN
C           XY
            IF(SQRT(ACC(20,IACC)*ACC(20,IACC)
     .             +ACC(21,IACC)*ACC(21,IACC))  > AMIN )ACTIVE = 1
          ELSEIF(IDIR == 5)THEN
C           XZ
            IF(SQRT(ACC(20,IACC)*ACC(20,IACC)
     .             +ACC(22,IACC)*ACC(22,IACC))  > AMIN )ACTIVE = 1
          ELSEIF(IDIR == 6)THEN
C           YZ
            IF(SQRT(ACC(21,IACC)*ACC(21,IACC)
     .             +ACC(22,IACC)*ACC(22,IACC))  > AMIN )ACTIVE = 1
          ELSEIF(IDIR == 7)THEN
C           XYZ
            IF(SQRT(ACC(20,IACC)*ACC(20,IACC)
     .             +ACC(21,IACC)*ACC(21,IACC)
     .             +ACC(22,IACC)*ACC(22,IACC))  > AMIN )ACTIVE = 1
          ENDIF
          IF(ACTIVE == 1)THEN
            TACT = MIN(TACT,TT)
          ELSE
            TACT = EP20
          ENDIF
          SENS2(3+(I-1)*3,1)=TACT
          IF(TACT+DURE.LE.TT)THEN
C           ACTIVATION DU SENSOR
            TSTART  =TT+TDELAY
            SENS1(2)=TSTART
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
            RETURN
          ENDIF
        ENDDO
C--------------------------------
C         CAPTEUR - DISTANCE
C--------------------------------
      ELSEIF(TYP == 2)THEN
        IF(TSTART-TDELAY.LE.TT)RETURN
        N1    = LSENS1(4)
        N2    = LSENS1(5)
        DM1   = SENS1(3)
        DM2   = SENS1(4)
        IF(IMACH /= 3.OR.NSPMD == 1)THEN
          DX = X(1,N1)-X(1,N2)
          DY = X(2,N1)-X(2,N2)
          DZ = X(3,N1)-X(3,N2)
        ELSE
          DX = XSENS(1)-XSENS(4)
          DY = XSENS(2)-XSENS(5)
          DZ = XSENS(3)-XSENS(6)
        END IF
        DD = SQRT(DX*DX+DY*DY+DZ*DZ)
        IF (DD < DM1 .OR. DD > DM2)THEN
            TSTART  =TT+TDELAY
            SENS1(2)=TSTART
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
            RETURN
        ENDIF
C--------------------------------
C         CAPTEUR - CAPTEURS -> SENSOR2
C--------------------------------
      ELSEIF (TYP == 3 .OR. TYP == 4 .OR. TYP == 8) THEN
C       FLAG D'activation POUR SENSORS HIERARCHIQUES EN //
        IF (TT > = TSTART) THEN
          LSENS1(3)=1
        ELSE
          LSENS1(3)=0
        ENDIF
C--------------------------------
C       SENSOR "OR"
C--------------------------------
      ELSEIF (TYP == 5) THEN
        IF (TT >= TSTART) THEN
          LSENS1(3)=1
        ELSE
          LSENS1(3)=0
        ENDIF
C--------------------------------
C         CAPTEUR - INTERFACE
C--------------------------------
      ELSEIF (TYP == 6)THEN
        IN     = LSENS1(4)
        INTSUB = LSENS1(5)
        IDIR   = LSENS1(6)
        TSTART = SENS1(2)
        TSTOPS = SENS1(3)
        FMIN   = SENS1(4)
        FMAX   = SENS1(5)
        DURE   = SENS1(6)
        TACT   = SENS1(7)
        FREQ   = SENS1(8)
        ACTIVE = 0
        FF = ZERO
c
c   BUTTERWORTH FILTERING 
c
        IF(FREQ  /=  ZERO)THEN
         ISECT=TABS(NSECT+IN+1)-TABS(NSECT+IN)
         IF (IDIR == 1) THEN
            FF = SQRT( (FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     +      	  FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     +      	  FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)) **2   +
     +              (FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     +      	    FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     +      	    FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)) **2  +
     +                (FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     +      	      FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     +      	      FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)) **2)  
c  
    	     IF (TT == ZERO) THEN                             
    	       VV1 = FF                                       
    	       VV2 = FF                                       
    	       FV1 = FF                                       
    	       FV2 = FF                                       
    	       SENS2(9,1) = VV1                               
    	       SENS2(10,1) = VV2                              
    	       SENS2(11,1) = FV1                              
    	       SENS2(12,1) = FV2                              
    	     ENDIF                                            
    	     VV1 = SENS2(9,1)                                 
    	     VV2 = SENS2(10,1)                                
    	     FV1 = SENS2(11,1)                                
    	     FV2 = SENS2(12,1)                                
    	     CALL BUTTERWORTH(DT2,FREQ,VV2,VV1,FF,FV2,FV1,FV) 
    	     SENS2(9,1) = FF                                  
    	     SENS2(10,1) = VV1                                
    	     SENS2(11,1) = FV                                 
    	     SENS2(12,1) = FV1                                
    	     FF = FV                                          
c
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
         ELSEIF (IDIR == 2) THEN
              FF = SQRT( (FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)+
     +      	  FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)+
     +      	  FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)) **2   +
     +              (FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)+
     +      	    FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)+
     +      	    FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)) **2  +
     +                (FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)+
     +      	      FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)+
     +      	      FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)) **2) 
c          
    	  IF (TT == ZERO) THEN
    	    VV1 = FF
    	    VV2 = FF
    	    FV1 = FF
    	    FV2 = FF
    	    SENS2(15,1) = VV1
    	    SENS2(16,1) = VV2
    	    SENS2(17,1) = FV1
    	    SENS2(18,1) = FV2
    	  ENDIF
    	  VV1 = SENS2(15,1)
    	  VV2 = SENS2(16,1)
    	  FV1 = SENS2(17,1)
    	  FV2 = SENS2(18,1)
    	  CALL BUTTERWORTH(DT2,FREQ,VV2,VV1,FF,FV2,FV1,FV)
    	  SENS2(15,1) = FF  
    	  SENS2(16,1) = VV1  
    	  SENS2(17,1) = FV  
    	  SENS2(18,1) = FV1  
    	  FF = FV
c
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
         ELSE
          ACTIVE = 1
         ENDIF
        ENDIF
c       
        ID_INTER = IN
        IF(IN > NINTER)ID_INTER = INTSUB
        IF (IPARI(29,ID_INTER) == 1) THEN
          IF(FREQ == ZERO)THEN
            ISECT=TABS(NSECT+IN+1)-TABS(NSECT+IN)
            IF (IDIR == 1) THEN 
              FF = SQRT( (FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     +      	  FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     +      	  FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)) **2   +
     +              (FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     +      	    FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     +      	    FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)) **2  +
     +                (FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     +      	      FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     +      	      FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)) **2) 
c
              IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
            ELSEIF (IDIR == 2) THEN
              FF = SQRT( (FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)+
     +      	  FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)+
     +      	  FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)) **2   +
     +              (FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)+
     +      	    FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)+
     +      	    FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)) **2  +
     +                (FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)+
     +      	      FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)+
     +      	      FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)) **2)  
c
              IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
            ELSE
              ACTIVE = 1
            ENDIF
          ENDIF
          IF (ACTIVE == 1) THEN
            TACT = MIN(TACT,TT)
          ELSE
            TACT = EP20
          ENDIF
c
          SENS1(7) = TACT
          IF (TACT+DURE <= TT) THEN
C           activation du sensor
            TSTOPS = TT+TDELAY
            SENS1(3)  = TSTOPS
            IF (LSENS1(3) == 0) THEN
              LSENS1(3) = 1
              TSTART    = TT
              SENS1(2)  = TSTART
#include "lockon.inc"
              IF(IMACH /= 3.OR.ISPMD == 0)
     .          WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .          ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
              RETURN
            ENDIF
          ENDIF
        ELSEIF(TT > TSTOPS .AND. LSENS1(3) == 1) THEN
C           desactivation
            LSENS1(3)=0
            TSTART = EP20
            SENS1(2) = TSTART
            TSTOPS = EP20
            SENS1(3) = TSTOPS
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' DEACTIVATED AT TIME ',TT
#include "lockoff.inc"
            RETURN
        ENDIF
C--------------------------------
C         CAPTEUR - RWALL
C--------------------------------
      ELSEIF(TYP == 7)THEN
        IN = LSENS1(4)
        IDIR   = LSENS1(6)
        TSTART = SENS1(2)
        TSTOPS = SENS1(3)
        FMIN   = SENS1(4)
        FMAX   = SENS1(5)
        DURE   = SENS1(6)
        TACT   = SENS1(7)
        ACTIVE = 0
        N5 = IN + 4*NRWALL
        IF (NPRW(N5) == 1) THEN
          IF (IDIR == 1) THEN
            ISECT=TABS(IN+NSECT+NINTER+NINTSUB+1)-TABS(IN+NSECT+NINTER+NINTSUB)
            FX = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     +           FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     +           FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
            FF = FX
            IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
          ELSEIF (IDIR == 2) THEN
            ISECT=TABS(IN+NSECT+NINTER+NINTSUB+1)-TABS(IN+NSECT+NINTER+NINTSUB)
            FY = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     +           FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     +           FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
            FF = FY
            IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
          ELSEIF (IDIR == 4) THEN
            ISECT=TABS(IN+NSECT+NINTER+NINTSUB+1)-TABS(IN+NSECT+NINTER+NINTSUB)
            FZ = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     +           FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     +           FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
            FF = FZ
            IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
          ELSEIF (IDIR == 5) THEN
            ISECT=TABS(IN+NSECT+NINTER+NINTSUB+1)-TABS(IN+NSECT+NINTER+NINTSUB)
            FX = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     +           FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     +           FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
            FY = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     +           FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     +           FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
            FZ = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     +           FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     +           FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
            FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
            IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
          ELSEIF (IDIR == 6) THEN
            ISECT=TABS(IN+NSECT+NINTER+NINTSUB+1)-TABS(IN+NSECT+NINTER+NINTSUB)
            FX = FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)+
     +           FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)+
     +           FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)
            FY = FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)+
     +           FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)+
     +           FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)
            FZ = FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)+
     +           FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)+
     +           FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)
            FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
            IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
          ELSE
            ACTIVE = 1
          ENDIF
          IF (ACTIVE == 1) THEN
            TACT = MIN(TACT,TT)
          ELSE
            TACT = EP20
          ENDIF
          SENS1(7) = TACT
          IF (TACT+DURE <= TT) THEN
            TSTOPS = TT+TDELAY
            SENS1(3)  = TSTOPS
            IF (LSENS1(3) == 0) THEN
C           activation du sensor
              LSENS1(3) = 1
              TSTART    = TT
              SENS1(2)  = TSTART
#include "lockon.inc"
              IF(IMACH /= 3.OR.ISPMD == 0)
     .          WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .         ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
              RETURN
            ENDIF
          ENDIF
        ELSEIF(TT > TSTOPS.AND.LSENS1(3) == 1)THEN
C -desactivation
            LSENS1(3)=0
            TSTART = EP20
            SENS1(2) = TSTART
            TSTOPS = EP20
            SENS1(3) = TSTOPS
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' DEACTIVATED AT TIME ',TT
#include "lockoff.inc"
            RETURN
        ENDIF
C--------------------------------
C         CAPTEUR - NODAL VELOCITY
C--------------------------------
      ELSEIF(TYP == 9)THEN
        IF (TSTART-TDELAY <= TT) RETURN
        N1    = LSENS2(1,1)
        VEL   = SENS2(1,1)
        FREQ  = SENS2(2,1)
        
        V1 = V(1,N1)
        V2 = V(2,N1)
        V3 = V(3,N1)
        VV = SQRT(V1*V1+V2*V2+V3*V3)
        IF (FREQ > 0) THEN
          IF (TT == ZERO) THEN
            VV1 = VV
            VV2 = VV
            FV1 = VV
            FV2 = VV
            SENS2(3,1) = VV1
            SENS2(4,1) = VV2
            SENS2(5,1) = FV1
            SENS2(6,1) = FV2
          ENDIF
          VV1 = SENS2(3,1)
          VV2 = SENS2(4,1)
          FV1 = SENS2(5,1)
          FV2 = SENS2(6,1)
          CALL BUTTERWORTH(DT2,FREQ,VV2,VV1,VV,FV2,FV1,FV)
          SENS2(3,1) = VV  
          SENS2(4,1) = VV1  
          SENS2(5,1) = FV  
          SENS2(6,1) = FV1  
          VV = FV
        ENDIF
        IF (VV > VEL) THEN
            TSTART  =TT+TDELAY
            SENS1(2)=TSTART
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
            RETURN
        ENDIF
C--------------------------------
C         CAPTEUR - GAUGE
C--------------------------------
      ELSEIF(TYP == 10)THEN
        IF(TSTART-TDELAY.LE.TT)RETURN
        DO I = 1,NAC
          AMIN = SENS2(1,I)
          DURE = SENS2(2,I)
          TACT = SENS2(3,I)
          IGAU = LSENS2(1,I)
          ACTIVE = 0
          IF(ABS(GAUGE(30,IGAU)) > AMIN)ACTIVE = 1
          IF(ACTIVE == 1)THEN
            TACT = MIN(TACT,TT)
          ELSE
            TACT = EP20
          ENDIF
          SENS2(3,I)=TACT
          IF(TACT+DURE.LE.TT)THEN
C           ACTIVATION DU SENSOR
            TSTART  =TT+TDELAY
            SENS1(2)=TSTART
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
            RETURN
          ENDIF
        ENDDO
C--------------------------------
C         SENSOR - RBODY
C--------------------------------
      ELSEIF (TYP == 11) THEN
        IF (TSTART-TDELAY <= TT) RETURN
        IN = LSENS1(4)
        IDIR   = LSENS1(6)
        TSTART = SENS1(2)
        TSTOPS = SENS1(3)
        FMIN   = SENS1(4)
        FMAX   = SENS1(5)
        DURE   = SENS1(6)
        TACT   = SENS1(7)
        ACTIVE = 0
        IAD = NINTER+NRWALL+IN 

        IF (IDIR == 1) THEN
          ISECT=TABS(IN+NSECT+NINTER+NINTSUB+NRWALL+1)-
     .                TABS(IN+NSECT+NINTER+NINTSUB+NRWALL)
          FX = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     +         FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     +         FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
          FY = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     +         FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     +         FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
          FZ = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     +         FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     +         FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
          FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
        ELSEIF (IDIR == 2) THEN
          ISECT=TABS(IN+NSECT+NINTER+NINTSUB+NRWALL+1)-
     .                TABS(IN+NSECT+NINTER+NINTSUB+NRWALL)
          FX = FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)+
     +         FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)+
     +         FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)
          FY = FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)+
     +         FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)+
     +         FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)
          FZ = FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)+
     +         FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)+
     +         FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)
          FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1	
        ELSE                                        
          ACTIVE = 1                                
        ENDIF           

        IF (ACTIVE == 1) THEN                       
          TACT = MIN(TACT,TT)                       
        ELSE                                        
          TACT = EP20                               
        ENDIF                                       
        SENS1(7) = TACT                             
        IF (TACT+DURE <= TT) THEN                   
          TSTOPS = TT+TDELAY                        
          SENS1(3)  = TSTOPS                        
          IF (LSENS1(3) == 0) THEN                  
C           activation du sensor                      
            LSENS1(3) = 1                           
            TSTART    = TT                          
            SENS1(2)  = TSTART                      
#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .          WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .         ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
            RETURN
          ENDIF
        ENDIF
C--------------------------------
C       SENSOR - SECTION
C--------------------------------
      ELSEIF (TYP == 12) THEN
        IF (TSTART-TDELAY <= TT) RETURN
        IN = LSENS1(4)
        IDIR   = LSENS1(6)
        TSTART = SENS1(2)
        TSTOPS = SENS1(3)
        FMIN   = SENS1(4)
        FMAX   = SENS1(5)
        DURE   = SENS1(6)
        TACT   = SENS1(7)
        ACTIVE = 0
        IAD = NINTER+NRWALL+NRBODY+IN        
        IF (IDIR == 1) THEN  ! FN                       
          IF(IPARSENS==0) THEN !         Parith/Off
           FX = FSAV(1,IAD)              
           FY = FSAV(2,IAD)                          
           FZ = FSAV(3,IAD)                          
          ELSE               !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FX = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     +           FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     +           FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
            FY = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     +           FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     +           FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
            FZ = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     +           FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     +           FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
          ENDIF
          FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
        ELSEIF (IDIR == 2) THEN  ! FT
          IF(IPARSENS==0) THEN !         Parith/Of
           FX = FSAV(4,IAD)                          
           FY = FSAV(5,IAD)                          
           FZ = FSAV(6,IAD)                          
          ELSE               !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FX = FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)+
     +           FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)+
     +           FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)
            FY = FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)+
     +           FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)+
     +           FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)
            FZ = FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)+
     +           FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)+
     +           FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)
          ENDIF
          FF = SQRT(FX*FX + FY*FY + FZ*FZ)
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
        ELSEIF (IDIR == 3) THEN  ! TF
          IF(IPARSENS==0) THEN !         Parith/Off  
           FX = FSAV(34,IAD)                         
           FY = FSAV(35,IAD)                         
           FZ = FSAV(36,IAD)
          ELSE               !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FX = FBSAV6(10,1,ISECT)+FBSAV6(10,2,ISECT)+
     +           FBSAV6(10,3,ISECT)+FBSAV6(10,4,ISECT)+
     +           FBSAV6(10,5,ISECT)+FBSAV6(10,6,ISECT)
            FY = FBSAV6(11,1,ISECT)+FBSAV6(11,2,ISECT)+
     +           FBSAV6(11,3,ISECT)+FBSAV6(11,4,ISECT)+
     +           FBSAV6(11,5,ISECT)+FBSAV6(11,6,ISECT)
            FZ = FBSAV6(12,1,ISECT)+FBSAV6(12,2,ISECT)+
     +           FBSAV6(12,3,ISECT)+FBSAV6(12,4,ISECT)+
     +           FBSAV6(12,5,ISECT)+FBSAV6(12,6,ISECT)
          ENDIF  
          FF = SQRT(FX*FX + FY*FY + FZ*FZ)          
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1  
        ELSEIF (IDIR == 4) THEN  !  TM
          IF(IPARSENS==0) THEN !         Parith/Off  
           FX = FSAV(31,IAD)                         
           FY = FSAV(32,IAD)                         
           FZ = FSAV(33,IAD)
          ELSE               !         Parith/on
           ISECT=TABS(IN+1)-TABS(IN)
           FX = FBSAV6(7,1,ISECT)+FBSAV6(7,2,ISECT)+
     +          FBSAV6(7,3,ISECT)+FBSAV6(7,4,ISECT)+
     +          FBSAV6(7,5,ISECT)+FBSAV6(7,6,ISECT)
           FY = FBSAV6(8,1,ISECT)+FBSAV6(8,2,ISECT)+
     +          FBSAV6(8,3,ISECT)+FBSAV6(8,4,ISECT)+
     +          FBSAV6(8,5,ISECT)+FBSAV6(8,6,ISECT)
           FZ = FBSAV6(9,1,ISECT)+FBSAV6(9,2,ISECT)+
     +          FBSAV6(9,3,ISECT)+FBSAV6(9,4,ISECT)+
     +          FBSAV6(9,5,ISECT)+FBSAV6(9,6,ISECT)
          ENDIF  
          FF = SQRT(FX*FX + FY*FY + FZ*FZ)          
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1  
        ELSEIF (IDIR == 5) THEN  !  FX
          IF(IPARSENS==0) THEN !         Parith/Off
              FF = FSAV(34,IAD)
          ELSE                  !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FF=FBSAV6(10,1,ISECT)+FBSAV6(10,2,ISECT)+
     +         FBSAV6(10,3,ISECT)+FBSAV6(10,4,ISECT)+
     +         FBSAV6(10,5,ISECT)+FBSAV6(10,6,ISECT)
          ENDIF
         IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
        ELSEIF (IDIR == 6) THEN  !  FY
          IF(IPARSENS==0) THEN!         Parith/Off
              FF = FSAV(35,IAD)
          ELSE                  !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FF=FBSAV6(11,1,ISECT)+FBSAV6(11,2,ISECT)+
     +         FBSAV6(11,3,ISECT)+FBSAV6(11,4,ISECT)+
     +         FBSAV6(11,5,ISECT)+FBSAV6(11,6,ISECT)
          ENDIF                         
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1  
        ELSEIF (IDIR == 7) THEN  !  FZ
          IF(IPARSENS==0) THEN  !         Parith/Off
              FF = FSAV(36,IAD)
          ELSE                  !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FF=FBSAV6(12,1,ISECT)+FBSAV6(12,2,ISECT)+
     +         FBSAV6(12,3,ISECT)+FBSAV6(12,4,ISECT)+
     +         FBSAV6(12,5,ISECT)+FBSAV6(12,6,ISECT)
          ENDIF                          
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1  
        ELSEIF (IDIR == 8) THEN  !  MX
          IF(IPARSENS==0) THEN!         Parith/Off
              FF = FSAV(31,IAD)
          ELSE                  !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FF=FBSAV6(7,1,ISECT)+FBSAV6(7,2,ISECT)+
     +         FBSAV6(7,3,ISECT)+FBSAV6(7,4,ISECT)+
     +         FBSAV6(7,5,ISECT)+FBSAV6(7,6,ISECT)
          ENDIF                          
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1
        ELSEIF (IDIR == 9) THEN  !  MY
          IF(IPARSENS==0) THEN!         Parith/Off
              FF = FSAV(32,IAD)
          ELSE                  !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FF=FBSAV6(8,1,ISECT)+FBSAV6(8,2,ISECT)+
     +         FBSAV6(8,3,ISECT)+FBSAV6(8,4,ISECT)+
     +         FBSAV6(8,5,ISECT)+FBSAV6(8,6,ISECT)
          ENDIF                           
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1  
        ELSEIF (IDIR == 10) THEN  !  MZ
          IF(IPARSENS==0) THEN!         Parith/Off
              FF = FSAV(33,IAD)
          ELSE                  !         Parith/on
            ISECT=TABS(IN+1)-TABS(IN)
            FF=FBSAV6(9,1,ISECT)+FBSAV6(9,2,ISECT)+
     +         FBSAV6(9,3,ISECT)+FBSAV6(9,4,ISECT)+
     +         FBSAV6(9,5,ISECT)+FBSAV6(9,6,ISECT)
           ENDIF                          
          IF (FF < FMIN .OR. FF > FMAX) ACTIVE = 1  
        ELSE                                        
          ACTIVE = 1                                
        ENDIF 
        IF (ACTIVE == 1) THEN                       
          TACT = MIN(TACT,TT)                       
        ELSE                                        
          TACT = EP20                               
        ENDIF                                       
        SENS1(7) = TACT                     
        IF (TACT+DURE <= TT) THEN                   
          TSTOPS = TT+TDELAY                        
          SENS1(3)  = TSTOPS                        
          IF (LSENS1(3) == 0) THEN                  
C         activation du sensor                      
            LSENS1(3) = 1                           
            TSTART    = TT                          
            SENS1(2)  = TSTART                      
#include "lockon.inc"
            IF (ISPMD == 0) WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .         ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
            RETURN
          ENDIF
        ENDIF
C--------------------------------
      ELSEIF (TYP == 13)THEN
C--------------------------------
C       SENSOR WORK
C--------------------------------
        N1 = LSENS1(4) 
        N2 = LSENS1(5)
        TACT   = SENS1(3)
        DURE   = SENS1(5)
        WMAX   = SENS1(6)
        FX = ZERO
        FY = ZERO
        FZ = ZERO
c--------
        IF (NSPMD == 1) THEN
          XSENS(1) = X(1,N1)
          XSENS(2) = X(2,N1)
          XSENS(3) = X(3,N1)
          IF (N2 > 0) THEN
            XSENS(4) = X(1,N2)
            XSENS(5) = X(2,N2)
            XSENS(6) = X(3,N2)
          ELSE
            XSENS(4) = ZERO
            XSENS(5) = ZERO
            XSENS(6) = ZERO
          ENDIF
          IF (TT == ZERO) THEN
            XSENS(7)  = XSENS(1)
            XSENS(8)  = XSENS(2)
            XSENS(9)  = XSENS(3)
            XSENS(10) = XSENS(4)
            XSENS(11) = XSENS(5)
            XSENS(12) = XSENS(6)
          ENDIF
        ENDIF
c
        DX = (XSENS(1) - XSENS(7)) - (XSENS(4) - XSENS(10))
        DY = (XSENS(2) - XSENS(8)) - (XSENS(5) - XSENS(11))
        DZ = (XSENS(3) - XSENS(9)) - (XSENS(6) - XSENS(12))
c
        XSENS(7)  = XSENS(1)
        XSENS(8)  = XSENS(2)
        XSENS(9)  = XSENS(3)
        XSENS(10) = XSENS(4)
        XSENS(11) = XSENS(5)
        XSENS(12) = XSENS(6)
c-------
c       Force in Section
        IN = LSENS1(6)
        IF (IN > 0) THEN
          ISECT = TABS(IN+1)-TABS(IN)
          FX = FX + FBSAV6(10,1,ISECT)+FBSAV6(10,2,ISECT)+
     +              FBSAV6(10,3,ISECT)+FBSAV6(10,4,ISECT)+
     +              FBSAV6(10,5,ISECT)+FBSAV6(10,6,ISECT)
          FY = FY + FBSAV6(11,1,ISECT)+FBSAV6(11,2,ISECT)+
     +              FBSAV6(11,3,ISECT)+FBSAV6(11,4,ISECT)+
     +              FBSAV6(11,5,ISECT)+FBSAV6(11,6,ISECT)
          FZ = FZ + FBSAV6(12,1,ISECT)+FBSAV6(12,2,ISECT)+
     +              FBSAV6(12,3,ISECT)+FBSAV6(12,4,ISECT)+
     +              FBSAV6(12,5,ISECT)+FBSAV6(12,6,ISECT)
          SENS1(20) = SENS1(20) + FX*DX + FY*DY + FZ*DZ
        ENDIF
c       Force in Interface
        IN = LSENS1(7)
        ID_INTER = IN
        IF (IN > NINTER) ID_INTER = LSENS1(8)  !Intsub
        IF (ID_INTER > 0) THEN
          ID_INTER = TABS(NSECT+IN+1)-TABS(NSECT+IN)
          FX = FX + FBSAV6(1,1,ID_INTER)+FBSAV6(1,2,ID_INTER)+
     +              FBSAV6(1,3,ID_INTER)+FBSAV6(1,4,ID_INTER)+
     +              FBSAV6(1,5,ID_INTER)+FBSAV6(1,6,ID_INTER)
          FY = FY + FBSAV6(2,1,ID_INTER)+FBSAV6(2,2,ID_INTER)+
     +              FBSAV6(2,3,ID_INTER)+FBSAV6(2,4,ID_INTER)+
     +              FBSAV6(2,5,ID_INTER)+FBSAV6(2,6,ID_INTER)
          FZ = FZ + FBSAV6(3,1,ID_INTER)+FBSAV6(3,2,ID_INTER)+
     +              FBSAV6(3,3,ID_INTER)+FBSAV6(3,4,ID_INTER)+
     +              FBSAV6(3,5,ID_INTER)+FBSAV6(3,6,ID_INTER)
          SENS1(21) = SENS1(21) + FX*DX + FY*DY + FZ*DZ
        ENDIF


c       Force in Rwall
        IN = LSENS1(9)
        IF (IN > 0) THEN
          N5 = IN + 4*NRWALL
c         IF (NPRW(N5) == 1) THEN
          IRWAL = TABS(IN+NSECT+NINTER+NINTSUB+1)
     .          - TABS(IN+NSECT+NINTER+NINTSUB)
          FX = FX + FBSAV6(1,1,IRWAL)+FBSAV6(1,2,IRWAL)+
     +              FBSAV6(1,3,IRWAL)+FBSAV6(1,4,IRWAL)+
     +              FBSAV6(1,5,IRWAL)+FBSAV6(1,6,IRWAL)
          FY = FY + FBSAV6(2,1,IRWAL)+FBSAV6(2,2,IRWAL)+
     +              FBSAV6(2,3,IRWAL)+FBSAV6(2,4,IRWAL)+
     +              FBSAV6(2,5,IRWAL)+FBSAV6(2,6,IRWAL)
          FZ = FZ + FBSAV6(3,1,IRWAL)+FBSAV6(3,2,IRWAL)+
     +              FBSAV6(3,3,IRWAL)+FBSAV6(3,4,IRWAL)+
     +              FBSAV6(3,5,IRWAL)+FBSAV6(3,6,IRWAL)
          SENS1(22) = SENS1(22) + FX*DX + FY*DY + FZ*DZ
        ENDIF
c       Force in Rbody
        IN  = LSENS1(10)
        IAD = NINTER+NRWALL+IN
        IF (IN > 0) THEN
          IRBOD = TABS(IN+NSECT+NINTER+NINTSUB+NRWALL+1)
     .          - TABS(IN+NSECT+NINTER+NINTSUB+NRWALL)
          FX = FX + FBSAV6(1,1,IRBOD)+FBSAV6(1,2,IRBOD)+
     +              FBSAV6(1,3,IRBOD)+FBSAV6(1,4,IRBOD)+
     +              FBSAV6(1,5,IRBOD)+FBSAV6(1,6,IRBOD)
          FY = FY + FBSAV6(2,1,IRBOD)+FBSAV6(2,2,IRBOD)+
     +              FBSAV6(2,3,IRBOD)+FBSAV6(2,4,IRBOD)+
     +              FBSAV6(2,5,IRBOD)+FBSAV6(2,6,IRBOD)
          FZ = FZ + FBSAV6(3,1,IRBOD)+FBSAV6(3,2,IRBOD)+
     +              FBSAV6(3,3,IRBOD)+FBSAV6(3,4,IRBOD)+
     +              FBSAV6(3,5,IRBOD)+FBSAV6(3,6,IRBOD)
          SENS1(23) = SENS1(23) + FX*DX + FY*DY + FZ*DZ
        ENDIF
c-------
        WORK = ABS(SENS1(20)) + ABS(SENS1(21))
     .       + ABS(SENS1(22)) + ABS(SENS1(23))
c
        IF (WORK > WMAX) THEN
          TACT = MIN(TACT,TT)
        ELSE  
          TACT = EP20                               
        ENDIF                                       
        SENS1(3) = TACT          ! start counting duration              
        IF (TACT + DURE <= TT) THEN                   
          TSTOPS   = TT                
          SENS1(4) = TSTOPS                        
          IF (LSENS1(3) == 0) THEN                  
C         activation du sensor                      
            LSENS1(3) = 1                           
            TSTART    = TT                          
            SENS1(2)  = TSTART
#include "lockon.inc"
            IF (ISPMD == 0) WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .         ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
            RETURN
          ENDIF
        ENDIF
c
c--------------------------------
      ELSEIF (TYP == 14) THEN
c--------------------------------
c       SENSOR --- ENERGY ---
c--------------------------------        
      CALL SENSOR_ENERGY(SENSOR_TAB   ,SENS1    ,LSENS1    ,ISENS    ,
     .                   SUBSET   ,PARTSAV2 )
c
C--------------------------------
      ELSEIF (TYP == 15) THEN
c--------------------------------
c       SENSOR Distance to surface
c
        CALL SENSOR_DIST_SURF(SENSOR_TAB ,X   ,IGRSURF  )
c
c--------------------------------
      ELSEIF (TYP == 16) THEN
c--------------------------------
c       SENSOR HIC
c
        CALL SENSOR_HIC(SENSOR_TAB ,A   ,ACC  )
c
c--------------------------------
      ELSEIF (TYP == 17) THEN
c--------------------------------
c       SENSOR TEMPERATURE
c
        CALL SENSOR_TEMP(SENSOR_TAB ,ISENS ,IGRNOD   ,TEMP     )
c
c--------------------------------
      ELSEIF (TYP == 19) THEN
c--------------------------------
c       SENSOR NIC-Nij
c
        CALL SENSOR_NIC(SENSOR_TAB )
c
c--------------------------------
      ELSEIF(TYP > 28 .AND. TYP < 38)THEN
c--------------------------------
c       USER SENSORS
c
        CALL USENS_SHIFT_AB(SENS1)
c
c----
      ENDIF  ! TYP 
c-----------
      RETURN
      END
