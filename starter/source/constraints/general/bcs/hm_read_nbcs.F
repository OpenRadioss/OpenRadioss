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
Chd|  HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_NBCS(ICODE     ,ISKEW   ,ITAB    ,ITABM1 ,IKINE  ,
     .                        IGRNOD    ,IBCSLAG ,LAG_NCF ,LAG_NKF,LAG_NHF,
     .                        IKINE1LAG ,ISKN    ,NOM_OPT ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE(*), ISKEW(*), ITAB(*), ITABM1(*), IKINE(*),
     .        IBCSLAG(5,*),
     .        LAG_NCF,LAG_NKF,LAG_NHF,IKINE1LAG(*),ISKN(LISKN,*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JJ(12), IC, NC, N, NUSR, IS, IC1, IC2, IC3, IC4,
     .        NOSYS, J,J10(10),IGR,IGRS,ISU,IBCALE,J6(6),K,
     .        IC0, IC01, IC02, IC03, IC04, ID ,ILAGM, NBCSLAG,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IKINE1(3*NUMNOD),SUB_ID,
     .        CHKCOD,ISERR,NOD,SUB_INDEX,NNOD
      INTEGER IUN
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
      FLAG_FMT = 0
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
C--------------------------------------------------
C START BROWSING MODEL /BCS
C--------------------------------------------------
      CALL HM_OPTION_START('/NBCS')
C--------------------------------------------------
C BROWSING MODEL PARTS 1->NBCS
C--------------------------------------------------
      DO I=1,NUMBCSN
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /BCS/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       KEYWORD2 = KEY)
C
        NOM_OPT(1,NUMBCS+I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NUMBCS+I),LTITR)
C
        CALL HM_GET_INTV('number_of_nodes',NNOD,IS_AVAILABLE,LSUBMODEL)
C
        DO K=1,NNOD
C
          CALL HM_GET_INT_ARRAY_INDEX('Tx',J6(1),K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('Ty',J6(2),K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('Tz',J6(3),K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('OmegaX',J6(4),K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('OmegaY',J6(5),K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('OmegaZ',J6(6),K,IS_AVAILABLE,LSUBMODEL)
C
          CALL HM_GET_INT_ARRAY_INDEX('Skew_ID',IS,K,IS_AVAILABLE,LSUBMODEL)
          IF(IS == 0 .AND. SUB_INDEX .NE. 0 ) IS = LSUBMODEL(SUB_INDEX)%SKEW
C
          CALL HM_GET_INT_ARRAY_INDEX('node_ID',NOD,K,IS_AVAILABLE,LSUBMODEL)
C
          NOSYS=USR2SYS(NOD,ITABM1,MESS,ID)
          IF (NOD == 0) THEN
            CALL ANCMSG(MSGID=78,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1='/NBCS/1',
     .                 I1=ID,  
     .                 I2=NOD)
          ENDIF
C
          ISERR = 0
          DO J=0,NUMSKW+NSUBMOD
            IF(IS == ISKN(4,J+1)) THEN
              IS=J+1
              ISERR = 1
            ENDIF
          ENDDO
          IF(ISERR == 0 ) THEN
            CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='BOUNDARY CONDITION',
     .              C2='BOUNDARY CONDITION',
     .              I2=IS,I1=ID,C3=TITR)
          ENDIF
C
          CHKCOD = 0
          DO J=1,6
            IF (J6(J) .GE. 2) THEN
              CHKCOD = 1
            ENDIF
          ENDDO
          IF(CHKCOD == 1) 
     .         CALL ANCMSG(MSGID=1051,ANMODE=ANINFO_BLIND,
     .                MSGTYPE=MSGERROR,I1=ID,C1=TITR,C2=CODE)
c
          IC1=J6(1)*4 +J6(2)*2 +J6(3)
          IC2=J6(4)*4 +J6(5)*2 +J6(6)
          IC =IC1*512+IC2*64
c
          IF(NOSYS .NE. 0) THEN
            ICODE(NOSYS)=MY_OR(IC,ICODE(NOSYS))
             IF(ISKEW(NOSYS).EQ.-1.OR.ISKEW(NOSYS).EQ.IS)THEN
               CHECK_NEW=IS
             ELSE
               CALL ANCMSG(MSGID=148,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=ITAB(NOSYS),PRMOD=MSG_CUMU)
             ENDIF
             ISKEW(NOSYS)=CHECK_NEW
            
             DO J=1,6
               IF(J6(J).NE.0)
     .         CALL KINSET(1,ITAB(NOSYS),IKINE(NOSYS),J,ISKEW(NOSYS)
     .                    ,IKINE1(NOSYS))
             ENDDO
          ENDIF
C
        ENDDO
C
      ENDDO
   

      RETURN
 999  CALL FREERR(1)
      RETURN
      END
