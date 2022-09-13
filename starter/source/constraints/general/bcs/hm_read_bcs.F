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
Chd|  HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_BCS(ICODE     ,ISKEW   ,ITAB    ,ITABM1 ,IKINE  ,
     .                  IGRNOD    ,IBCSLAG ,LAG_NCF ,LAG_NKF,LAG_NHF,
     .                  IKINE1LAG ,ISKN    ,NOM_OPT ,UNITAB ,LSUBMODEL,
     .                  IBCSCYC   ,LBCSCYC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD    
      USE SUBMODEL_MOD        
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "submod_c.inc"
#include      "titr_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE(*), ISKEW(*), ITAB(*), ITABM1(*), IKINE(*),
     .        IBCSLAG(5,*),
     .        LAG_NCF,LAG_NKF,LAG_NHF,IKINE1LAG(*),ISKN(LISKN,*)
      INTEGER NOM_OPT(LNOPT1,*),IBCSCYC(4,*) ,LBCSCYC(2,*) 
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JJ(12), IC, NC, N, NUSR, IS, IC1, IC2, IC3, IC4,
     .        NOSYS, J,J10(10),IGR,IGRS,ISU,IBCALE,J6(6),K,
     .        IC0, IC01, IC02, IC03, IC04, ID ,ILAGM, NBCSLAG,
     .        IKINE1(3*NUMNOD),SUB_ID,
     .        CHKCOD,ISERR,NOD,S_STRING,SUB_INDEX
      INTEGER IUN,IGR1,IGRS1,IGR2,IGRS2,IAD_L,NBY_NI,NBCSCYCI,ICYC,IS0
      CHARACTER MESS*40,KEY*ncharkey,STRING*ncharfield,CODE*7,
     .        KEY2*ncharkey
      CHARACTER TITR*nchartitle,OPT*8
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,MY_OR,CHECK_NEW,NGR2USR
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA IUN/1/
      DATA MESS/'BOUNDARY CONDITIONS                     '/
C======================================================================|
C
      IS_AVAILABLE = .FALSE.
      NBCSLAG = 0
      NBCSCYCI = 0
      IAD_L = 0
      DO I=1,NUMNOD
        ISKEW(I)=-1
      ENDDO
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
C--------------------------------------------------
C START BROWSING MODEL /BCS
C--------------------------------------------------
      CALL HM_OPTION_START('/BCS')
C--------------------------------------------------
C BROWSING MODEL PARTS 1->NBCS
C--------------------------------------------------
      DO I=1,NUMBCS
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /BCS/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       KEYWORD2 = KEY)
        ILAGM = 0
        IF (KEY(1:6) == 'LAGMUL' ) ILAGM = 1
        ICYC = 0
        IF (KEY(1:6) == 'CYCLIC' ) ICYC = 1
        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        IF (ICYC == 1 )THEN
          CALL HM_GET_INTV('grnd_ID1',IGR1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('grnd_ID2',IGR2,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('skew_ID',IS,IS_AVAILABLE,LSUBMODEL)
        ELSE
          CALL HM_GET_INTV('dof1',J6(1),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dof2',J6(2),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dof3',J6(3),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dof4',J6(4),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dof5',J6(5),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dof6',J6(6),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('inputsystem',IS,IS_AVAILABLE,LSUBMODEL)
        END IF !(ICYC == 1 )THEN
c
        IF(IS == 0 .AND. SUB_INDEX /= 0 ) IS = LSUBMODEL(SUB_INDEX)%SKEW
        IS0 = IS
        CALL HM_GET_INTV('entityid',IGR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C
        DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
          IF(IS == ISKN(4,J+1)) THEN
            IS=J+1
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='BOUNDARY CONDITION',
     .              C2='BOUNDARY CONDITION',
     .              I2=IS,I1=ID,C3=TITR)
 100    CONTINUE
C
        IF (ICYC == 0 )THEN
          CHKCOD = 0
          DO J=1,6
            IF (J6(J) >= 2) THEN
              CHKCOD = 1
            ENDIF
          ENDDO
          IF(CHKCOD == 1) 
     .         CALL ANCMSG(MSGID=1051,ANMODE=ANINFO_BLIND,
     .                MSGTYPE=MSGERROR,I1=ID,C1=TITR,C2=CODE)
          IC1=J6(1)*4 +J6(2)*2 +J6(3)
          IC2=J6(4)*4 +J6(5)*2 +J6(6)
          IC =IC1*512+IC2*64
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGRS=NGR2USR(IGR,INGR2USR,NGRNOD)
          IF (IGRS==0) THEN
            CALL ANCMSG(MSGID=678,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,I2=IGR,C1=TITR)
          END IF
          IF (ILAGM == 0) THEN
            DO J=1,IGRNOD(IGRS)%NENTITY
              NOSYS=IGRNOD(IGRS)%ENTITY(J)
              ICODE(NOSYS)=MY_OR(IC,ICODE(NOSYS))
              IF(ISKEW(NOSYS)==-1.OR.ISKEW(NOSYS)==IS)THEN
                CHECK_NEW=IS
              ELSE
                CALL ANCMSG(MSGID=148,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=ITAB(NOSYS),PRMOD=MSG_CUMU)
              ENDIF
              ISKEW(NOSYS)=CHECK_NEW
            
              DO K=1,6
                IF(J6(K)/=0)
     .          CALL KINSET(1,ITAB(NOSYS),IKINE(NOSYS),K,ISKEW(NOSYS)
     .                    ,IKINE1(NOSYS))
              ENDDO
             ENDDO
             CALL ANCMSG(MSGID=148,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=ID,C1=TITR,PRMOD=MSG_PRINT)
          ELSE
            NBCSLAG = NBCSLAG+1
            IBCSLAG(1,NBCSLAG) = IGRS
            IBCSLAG(2,NBCSLAG) = ID
            IBCSLAG(3,NBCSLAG) = IC
            IBCSLAG(4,NBCSLAG) = IS
            IBCSLAG(5,NBCSLAG) = ID
            DO J=1,IGRNOD(IGRS)%NENTITY
              NOSYS=IGRNOD(IGRS)%ENTITY(J)
              CALL KINSET(512,ITAB(NOSYS),IKINE(NOSYS),7,0
     .                    ,IKINE1LAG(NOSYS))
              DO K=1,6
                IF(J6(K)/=0) THEN
                  LAG_NHF = LAG_NHF + 1
                  LAG_NCF = LAG_NCF + 1
                  IF(IS==0) THEN
                    LAG_NKF = LAG_NKF + 1
                  ELSE
                    LAG_NKF = LAG_NKF + 3
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDIF
C--- /BCS/CYCLIC          
        ELSE
C------Imov=0 only fixing skew is allowed         
         IF (ISKN(5,IS)/=0) THEN
            CALL ANCMSG(MSGID=1760,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=ID,I2=IS0,C1=TITR,PRMOD=MSG_PRINT)
         END IF
C----- in SKEW 1:9 (x',y',z'); 10:12 (X0,Y0,Z0)        
         INGR2USR => IGRNOD(1:NGRNOD)%ID
         IGRS1=NGR2USR(IGR1,INGR2USR,NGRNOD)
         IGRS2=NGR2USR(IGR2,INGR2USR,NGRNOD)
         NBY_NI = IGRNOD(IGRS1)%NENTITY
         NBCSCYCI = NBCSCYCI + 1
         IBCSCYC(1,NBCSCYCI)=IAD_L
         IBCSCYC(2,NBCSCYCI)=IS
         IBCSCYC(3,NBCSCYCI)=NBY_NI
         IBCSCYC(4,NBCSCYCI)=ID
         DO J=1,NBY_NI
           LBCSCYC(1,J+IAD_L)=IGRNOD(IGRS1)%ENTITY(J)
           LBCSCYC(2,J+IAD_L)=IGRNOD(IGRS2)%ENTITY(J)
         END DO
         IAD_L =IAD_L+NBY_NI
        END IF !(IBCSCYC == 0 )THEN
      ENDDO
C
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
