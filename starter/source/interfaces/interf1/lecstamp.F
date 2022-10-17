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
CCgw|         LECTUR                         /lectur1/lectur.F
Chd|====================================================================
Chd|  LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECSTAMP(IPARI  ,INTSTAMP, UNITAB, NPBY,
     .                    ICODE  ,NOM_OPT ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "sysunit.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER IPARI(NPARI,*), NPBY(NNPBY,*), 
     .        ICODE(NUMNOD)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, L, NI, NIN, N,
     .   NTYP,NOINT,NSTAMP,IROT,
     .   IFLAGUNIT,UID,FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,
     .   ID_INTDAMP,INTDAMP, IRB, MSR, P, IC, SUB_ID
      my_real
     .   FAC_L,FAC_T,FAC_M,FAC_I,
     .   DAMP, DAMPR
      CHARACTER MESS*40, KEY*ncharkey, TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'INTERFACE INPUT                         '/
C=======================================================================

      NI= 0
      NSTAMP = 0
C
C--------------------------------------------------
C START BROWSING MODEL INTERFACES
C--------------------------------------------------
      CALL HM_OPTION_START('/INTER')
C--------------------------------------------------
C BROWSING MODEL INTERFACES 1->HM_NINTER
C--------------------------------------------------
      DO NIN=1,HM_NINTER
C
C--------------------------------------------------
C EXTRACT DATAS OF /INTER/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NOINT,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)
C--------------------------------------------------
C CHECK IF READ OPTION IS /INTER/SUB
C--------------------------------------------------
        IF(KEY(1:LEN_TRIM(KEY))=='SUB') CYCLE 
C--------------------------------------------------
         NI=NI+1
C
        NTYP   = IPARI(7,NI)
        NOINT  = IPARI(15,NI)
C
        IF (NTYP == 21) THEN

          NSTAMP=NSTAMP+1
          INTSTAMP(NSTAMP)%NOINTER=NI

          WRITE(IOUT,2100) NOINT
          IS_AVAILABLE = .FALSE.
c associated rbody
          CALL HM_GET_INTV('ID_RBY',INTSTAMP(NSTAMP)%IRB,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('InterfaceId',ID_INTDAMP,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOATV('DAMP1',DAMP,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('DAMP2',DAMPR,IS_AVAILABLE,LSUBMODEL,UNITAB)

C associated rbody & main node
          IRB=0
          DO N=1,NRBODY
            IF(INTSTAMP(NSTAMP)%IRB==NPBY(6,N))THEN
              IF(NPBY(12,N) == 0) THEN
                IRB=N
                EXIT
              ELSE
                CALL ANCMSG(MSGID=1635,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANSTOP,
     .                      I1=NOINT,
     .                      C1=TITR,
     .                      I2=INTSTAMP(NSTAMP)%IRB)
              ENDIF
            END IF
          END DO
          IF(IRB==0)THEN
            CALL ANCMSG(MSGID=684,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                   I1=NOINT,
     .                   C1=TITR,
     .                   I2=INTSTAMP(NSTAMP)%IRB)
          END IF
          INTSTAMP(NSTAMP)%IRB=IRB
          INTSTAMP(NSTAMP)%MSR=NPBY(1,IRB)
          DO P = 1, NSPMD
	      CALL IFRONTPLUS(NPBY(1,IRB),P)
          ENDDO
C
          WRITE(IOUT,2111) ID_INTDAMP,DAMP
          INTSTAMP(NSTAMP)%INTDAMP=ID_INTDAMP
          INTSTAMP(NSTAMP)%DAMP=DAMP
          IROT=1
          IC=MOD(ICODE(NPBY(1,IRB)),512)
          IF(IC==448)IROT=0
          IF(IROT/=0)THEN
            WRITE(IOUT,2112) DAMPR
            INTSTAMP(NSTAMP)%DAMPR=DAMPR
          END IF
          INTSTAMP(NSTAMP)%IROT=IROT
        END IF
      END DO
C-----
      DO NIN=1,NSTAMP
        ID_INTDAMP=INTSTAMP(NIN)%INTDAMP
        IF(ID_INTDAMP==0) GOTO 110
        DO J=1,NSTAMP
          IF(IPARI(15,INTSTAMP(J)%NOINTER)==ID_INTDAMP)THEN
            INTSTAMP(NIN)%INTDAMP=J
            GOTO 110
          END IF
        END DO
        CALL FRETITL2(TITR,
     .       NOM_OPT(LNOPT1-LTITR+1,NIN),LTITR)
        CALL ANCMSG(MSGID=868,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=NOM_OPT(1,NIN),
     .              C1=TITR,
     .              I2=ID_INTDAMP)
  110   CONTINUE
      END DO
C-----
 2100 FORMAT(//
     .       '  ADDITIONAL INFO FOR INTERFACE ID. . . . . .',I1/,
     .       '                      INTERFACE TYPE. . . . .21',/)
 2111 FORMAT('  DAMPING WRT REFERENCE INTERFACE . . . . . . .',I10/,
     .       '           (0: DAMPING WRT GLOBAL FRAME). . . .',/,
     .       '  TRANSLATIONAL CRITICAL DAMPING FACTOR . . . .',
     .        1PG20.13/)
 2112 FORMAT('  ROTATIONAL CRITICAL DAMPING FACTOR. . . . . .',
     .        1PG20.13/)
C-----
      RETURN
      END
