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
Chd|  HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SECTAREA                      source/loads/bolt/sectarea.F  
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PRELOAD(IXS     ,IXS10,IPRELOAD ,PRELOAD  ,IFLAG_BPRELOAD,
     .                           NSTRF   ,SENSORS    ,UNITAB ,X       ,
     .                           ISOLNOD ,ITAB       ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SENSOR_MOD
      USE HM_OPTION_READ_MOD    
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "sysunit.inc"
#include      "boltpr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IXS(NIXS,*), IXS10(6,*), IPRELOAD(3,*), IFLAG_BPRELOAD(*)  
      INTEGER NSTRF(*),ITAB(*),ISOLNOD(*)
      my_real PRELOAD(6,*),X(3,*)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISEC, IG, K, M, I1, I2, I3, I4, IFU, I, ISENS,NPR0,NN,
     .        IAD,ID,J,UID,IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,
     .        JS,SENSNUM
      INTEGER IUGRP(NPRELOAD),IDSENSOR(NPRELOAD)
      INTEGER K0,K1,K2,K3,IS,IE,II,NS,NI,ITYPE,IN1,IN2,IN3
      INTEGER NDEB, NFIN
      my_real
     .   LVAL(NPRELOAD),TS(NPRELOAD),TE(NPRELOAD),
     .   NX(NPRELOAD),NY(NPRELOAD),NZ(NPRELOAD)
      my_real
     .   TDEB,TFIN, LOADVAL,
     .   X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,N3,AREA(NPRELOAD)
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharline
C-----------------------------------------------
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'BOLT PRELOADING DEFINITION              '/
C=======================================================================
      IS_AVAILABLE = .FALSE.
C-----------------------------------------------
      CALL HM_OPTION_START('/PRELOAD')       
C-----------------------------------------------
      NPR0=NPRELOAD                                                
      K=0
      AREA(1:NPRELOAD)=ZERO                                  

      DO I=1,NPR0                                             
        ! Read title, ID and Unit ID
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = ID, 
     .                          UNIT_ID        = UID,
     .                          OPTION_TITR    = TITR,           
     .                          KEYWORD2       = KEY)
C
         IF (KEY(1:LEN_TRIM(KEY))=='AXIAL') CYCLE
        ! Checking unit ID
        IFLAGUNIT = 0 
C
        DO J=1,NUNITS                           
          IF (UNITAB%UNIT_ID(J) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO                                  
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='BOLT PRELOAD',
     .                 C2='BOLT PRELOAD',
     .                 C3=TITR)                              
        ENDIF                                    
 

        !read card line                                           
        CALL HM_GET_INTV('sect_ID'  ,ISEC  ,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('sens_ID'  ,ISENS ,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('Itype'    ,ITYPE ,IS_AVAILABLE,LSUBMODEL) 
 
        CALL HM_GET_FLOATV('Preload',LOADVAL,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Tstart' ,TDEB   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Tstop'  ,TFIN   ,IS_AVAILABLE, LSUBMODEL, UNITAB)

     
        IF (ITYPE == 0) ITYPE = 1 ! Default to effort preloading

      
        IF(LOADVAL <= ZERO) THEN
          CALL ANCMSG(MSGID=1255,MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='BOLT PRELOAD',
     .                I1=ID,
     .                C2=TITR)
          LOADVAL = ZERO 
        ENDIF

        IF (TFIN == ZERO) TFIN=EP30
c        
        SENSNUM = 0
        IF (ISENS > 0) THEN
          DO JS=1,SENSORS%NSENSOR
            IF (ISENS == SENSORS%SENSOR_TAB(JS)%SENS_ID) THEN
              SENSNUM = JS ! not used - not stored
              EXIT
            ENDIF
          ENDDO
          IF (SENSNUM == 0) THEN
            ISENS = 0
            CALL ANCMSG(MSGID=1252,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .               C1='BOLT PRELOAD',
     .               C2='BOLT PRELOAD',
     .               I2=ISENS,I1=ID,C3=TITR)
           END IF
        ENDIF !(ISENS > 0) 
c
             NN = 0
        IF (ISEC > 0) THEN !ISEC=sect_ID input is mandatory
          K0=31
          DO IS=1,NSECT 
            IF (ISEC==NSTRF(K0+23)) THEN
              IUGRP(I) = ISEC
              LVAL(I) = LOADVAL
              IDSENSOR(I) = ISENS
              TS(I) = TDEB
              TE(I) = TFIN
              NN = NSTRF(K0+6)
              NS = NSTRF(K0+7)
              NI = NSTRF(K0+14)
              K1 = K0+30
              K2 = K1+NI
              K3 = K2+NN
              IN1 = NSTRF(K0+3)
              IN2 = NSTRF(K0+4)
              IN3 = NSTRF(K0+5)
              X1=X(1,IN2)-X(1,IN1)
              Y1=X(2,IN2)-X(2,IN1)
              Z1=X(3,IN2)-X(3,IN1)
              X2=X(1,IN3)-X(1,IN1)
              Y2=X(2,IN3)-X(2,IN1)
              Z2=X(3,IN3)-X(3,IN1)
              X3=Y1*Z2-Z1*Y2
              Y3=Z1*X2-Z2*X1
              Z3=X1*Y2-X2*Y1
              N3=X3*X3+Y3*Y3+Z3*Z3
c
              IF(N3 < EM20) THEN
                CALL ANCMSG(MSGID=1244,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      C1='BOLT PRELOAD',
     .                      C2='BOLT PRELOAD',
     .                      I2=ISEC,I1=ID,C3=TITR)
              ELSE
                N3 = SQRT(N3)
                X3 = X3/N3
                Y3 = Y3/N3
                Z3 = Z3/N3
                NX(I) = X3
                NY(I) = Y3
                NZ(I) = Z3
              ENDIF
              IF(NS == 0) THEN
                CALL ANCMSG(MSGID=1251,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                      C1='BOLT PRELOAD',
     .                      C2='BOLT PRELOAD',
     .                      I2=ISEC,I1=ID,C3=TITR)
              ELSE
                ! Calcul de l'aire de la section
                CALL SECTAREA(IXS,IXS10,X,NSTRF(K3),NS,ISOLNOD,AREA(I),ITAB)    !
                IF (ITYPE == 1) THEN  
                  IF (AREA(I) > 1.E-30) LOADVAL = LOADVAL/AREA(I)
                ENDIF
                DO IE=1,NS
                  K=K+1
                  II=(K3-1)+2*IE-1
                  IPRELOAD(1,K)  = NSTRF(II)             ! Num. interne avant tri 
                  IPRELOAD(2,K)  = ISENS                 ! Sensor
                  IPRELOAD(3,K)  = IXS(11,NSTRF(II))     ! User Id.
                  !IFLAG_BPRELOAD(NSTRF(II)) = 1          ! Flag sur element precontraint
                  IFLAG_BPRELOAD(NSTRF(II)) = I          ! Flag sur element precontraint

 
                  PRELOAD(1,K) = TDEB                    ! Temps debut de pretension                       
                  PRELOAD(2,K) = TFIN                    ! Temps fin de pretension
                  PRELOAD(3,K) = LOADVAL                 ! Contrainte de pretension
                  PRELOAD(4,K) = X3                      ! Direction de pretension
                  PRELOAD(5,K) = Y3
                  PRELOAD(6,K) = Z3
                ENDDO
              ENDIF !IF(NS == 0)
              GOTO 100
            ENDIF !IF (ISEC==NSTRF(K0+23))
            K0=NSTRF(K0+24)
          ENDDO
        ENDIF
        CALL ANCMSG(MSGID=1243,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='BOLT PRELOAD',
     .              C2='BOLT PRELOAD',
     .              I2=ISEC,I1=ID,C3=TITR)
100     CONTINUE

      ENDDO
      I1=1
      I2=MIN0(50,NPRELOAD)
c-----------------------------output------------------------------------------
   90  WRITE (IOUT,2000)
       DO I=I1,I2
         WRITE (IOUT,'(I10,1X,I10,1X,1PE10.3,1X,I10,6(1X,1PE10.3))') I,
     .        IUGRP(I), AREA(I), IDSENSOR(I), NX(I), NY(I), NZ(I), LVAL(I), TS(I), TE(I)
       ENDDO
       IF(I2==NPRELOAD)GOTO 200
       I1=I1+50
       I2=MIN0(I2+50,NPRELOAD)
       GOTO 90
       
c-------------------------
 200  RETURN
c-------------------------
 2000 FORMAT(//
     .'     BOLT PRELOADINGS  '/
     .'     ----------------  '/
     .'        ID    SECTION       AREA     SENSOR         NX         NY
     .         NZ    PRELOAD    START-T      END-T')
C-----------
      RETURN
      END
