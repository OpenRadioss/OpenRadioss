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
Chd|  HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RBE3(IRBE3  ,LRBE3  ,FRBE3  ,ITAB   ,ITABM1  ,
     .                         IGRNOD ,ISKN   ,LXINTD ,IKINE  ,IDDLEVEL,
     .                         NOM_OPT,ITAGND ,GRNOD_UID,UNITAB,LSUBMODEL )
C-------------------------------------
C     LECTURE STRUCTURE RIGIDES 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
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
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*), LRBE3(*), ITAB(*),ITABM1(*),
     .        ISKN(LISKN,*),LXINTD,
     .        IDDLEVEL,IKINE(*),ITAGND(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real
     .     FRBE3(6,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      INTEGER :: GRNOD_UID
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NSL, NSLT, ITYP, NUSER,  NM, NI, NI_OK,
     .        ISK, ISENS, INGU, IGM, J, P,IAD,NS,NN,J6(6),JJ,II,
     .        IC,IC1,IC2,IROT,ISKS,IADS,IERR1,ISKEW0(SLRBE3/2),IMODIF,
     .        IDIR,IKINE1(3*NUMNOD),NRB,ID,UID,SUB_INDEX
      CHARACTER TITR*nchartitle,KEY*ncharkey,MESS*40
      CHARACTER STRING*ncharfield,CODE*7
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NODGRNR5
C
      DATA MESS/'INTERPOLATION CONSTRAINT BODY  '/
      my_real
     .   WI(6,NUMNOD),W
C-----------------------------------------------
C     IRBE3(1,I) : IAD0 for LRBE3 and FRBE3
C     IRBE3(2,I) : TYPE         usr' id temporaire (print)
C     IRBE3(3,I) : SECONDARY NODES
C     IRBE3(4,I) : REF_DOF
C     IRBE3(5,I) : NUMBER MAIN NODE
C     IRBE3(6,I) : IROT        =0 no rotational dependent
C     IRBE3(7,I) : SENSOR NUMBER - not used yet
C     IRBE3(8,I) :  not used yet,used temporaire for Imodif
C     IRBE3(9,I) :  not used yet
C     IRBE3(10,I) :  Numero interne du IRBE3 (necessaire pour Modif/SPMD)
C=======================================================================

      WRITE(IOUT,1000)
CC
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RBE3')
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
      IADS = SLRBE3/2
      IAD = 0
      NRB = 0

C---------otherwise quadratic cycling---------
      DO J=1,6
       DO N=1,NUMNOD
        WI(J,N)=ZERO
       ENDDO
      ENDDO

      DO I=1,NRBE3
        NRB=NRB+1
C----------Multidomaines --> on ignore les rbe3 non tages---------
        IF(NSUBDOM>0)THEN
          IF(TAGRB3(NRB)==0)CALL HM_SZ_R2R(TAGRB3,NRB,LSUBMODEL)
        END IF
C--------------------------------------------------
C EXTRACT DATAS OF /RBE3/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR,
     .                       SUBMODEL_INDEX = SUB_INDEX)
        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C
        CALL HM_GET_INTV('dependentnode',NSL,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LTX',J6(1),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LTY',J6(2),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LTZ',J6(3),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LRX',J6(4),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LRY',J6(5),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('LRZ',J6(6),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('nset',NN,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('I_Modif',IMODIF,IS_AVAILABLE,LSUBMODEL)
        IRBE3(2,I) = ID
        IRBE3(10,I)=I
        NUSER = ID
C
        IF (IMODIF==0) IMODIF =1
        IRBE3(8,I) = IMODIF
        NS = USR2SYS(NSL,ITABM1,MESS,NUSER)
        IC1=J6(1)*4 +J6(2)*2 +J6(3)
        IC2=J6(4)*4 +J6(5)*2 +J6(6)
        IC =IC1*512+IC2*64
        IF (IC==0) IC =7*512+7*64
        IF (NS10E > 0) THEN
          IF(ITAGND(NS)/=0) THEN
C------- error out      
          CALL ANCMSG(MSGID=1208,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(NS),
     .                C1='RBE3 ',
     .                I2=NUSER,
     .                C2='RBE3 ')
          ENDIF
        END IF
        IRBE3(3,I) = NS
        IRBE3(4,I) = IC
        IRBE3(1,I) = IAD
        IROT = 0
        DO J=1,NN
         CALL HM_GET_FLOAT_ARRAY_INDEX('independentnodesetcoeffs',W,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_INT_ARRAY_INDEX('tx',J6(1),J,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('ty',J6(2),J,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('tz',J6(3),J,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('rx',J6(4),J,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('ry',J6(5),J,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('rz',J6(6),J,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('SKEW_ARRAY',ISK,J,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INT_ARRAY_INDEX('independentnodesets',INGU,J,IS_AVAILABLE,LSUBMODEL)
         IF (W==ZERO.OR.IMODIF==3) W=ONE
         NM = 0
         IF(INGU > 0) THEN
          CALL C_HASH_FIND(GRNOD_UID,INGU,IGM) 
          IF (IGM == 0)THEN
            CALL ANCMSG(MSGID=53,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1= MESS,
     .                  I1=INGU)
          ELSE
            NM = IGRNOD(IGM)%NENTITY
            LRBE3(IAD+1:IAD+NM) = IGRNOD(IGM)%ENTITY(1:NM)
          ENDIF
         END IF !(INGU > 0)

         ISKS = 0
         IF ((J6(1)+J6(2)+J6(3)+J6(4)+J6(5)+J6(6))==0) THEN
          J6(1)=1
          J6(2)=1
          J6(3)=1
         ENDIF
         IF (ISK/=0) THEN
          DO JJ=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
           IF(ISK==ISKN(4,JJ+1)) THEN
            ISKS=JJ+1
            GO TO 10
           ENDIF
          ENDDO
          CALL ANCMSG(MSGID=184,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               C1='RBE3',
     .               I1=NUSER,
     .               C2='RBE3',
     .               C3=TITR,
     .               I2=ISK)
 10      CONTINUE
         ENDIF
         DO K=1,NM
          NI=LRBE3(IAD+K)
          LRBE3(IADS+IAD+K)=ISKS
          ISKEW0(IAD+K)=ISK
          DO JJ=1,6
           II = J6(JJ)
           IF (II>ZERO) THEN
            IF (JJ>3) IROT=1
            IF (WI(JJ,NI)==ZERO) THEN
                   WI(JJ,NI) = W
            ELSE
             CALL ANCMSG(MSGID=705,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=NUSER,
     .                   C1=TITR,
     .                   I2=ITAB(NI))
            ENDIF
           ENDIF
           IF (NS10E > 0) THEN
             IF(ITAGND(NI)/=0) THEN
C------- error out      
             CALL ANCMSG(MSGID=1211,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ITAB(NI),
     .               C1='RBE3',
     .               I2=NUSER,
     .               C2='RBE3')
             ENDIF
           END IF
           FRBE3(JJ,IAD+K) = WI(JJ,NI)
          ENDDO
         END DO ! K=1,NM
         IAD = IAD+NM
        ENDDO ! DO J=1,NN
        IRBE3(5,I) = IAD-IRBE3(1,I)
        IRBE3(6,I) = IROT

! optimisation: only concerned nodes are set to zero
! avoid to set to 0 all nodes (quadratic loop on NRBE3 and NUMNOD)
        DO NI_OK = IRBE3(1,I)+1,IRBE3(1,I)+IRBE3(5,I)
          DO JJ = 1,6
            WI(JJ,LRBE3(NI_OK)) = ZERO
          ENDDO
        ENDDO

      END DO !I=1,NRBE3
C
      IF (IPRI<5) WRITE(IOUT,1103)
      DO I=1,NRBE3
       IAD = IRBE3(1,I)
       NS = IRBE3(3,I)
       NM = IRBE3(5,I)
       NUSER = IRBE3(2,I)
       IMODIF = IRBE3(8,I)
       IF (IMODIF/=2) IRBE3(8,I)=4
       CALL PRERBE3FR(IRBE3 ,I    ,J6  ,J6(4)   )
       IF (IPRI>=5) THEN
        WRITE(IOUT,1100) NUSER,ITAB(NS),J6,NM,IMODIF
        WRITE(IOUT,1101)
        DO J = 1, NM
         WRITE(IOUT,1102) ITAB(LRBE3(IAD+J)),ISKEW0(IAD+J),
     .                    (FRBE3(JJ,IAD+J),JJ=1,6)
        ENDDO
        WRITE(IOUT,*) 
       ELSE
c        WRITE(IOUT,1103) 
        WRITE(IOUT,1104) NUSER,ITAB(NS),J6,NM,IMODIF
       END IF
        LXINTD = LXINTD + NM/4 + 1
        IF (IDDLEVEL == 0) THEN
          DO IDIR=1,6
            CALL KINSET(4096,ITAB(NS),IKINE(NS),IDIR,0,
     .                IKINE1(NS))
          ENDDO
        ENDIF
      ENDDO
      IF (NSPMD==1) LXINTD = 0
C
      RETURN
C
 1000 FORMAT(//
     .'     INTERPOLATION CONSTRAINT BODY (RBE3)   '/
     . '      ---------------------- '/)
 1100 FORMAT( 5X,'NUMBER. . . . . . . . . . . . . ',I10
     .       /5X,'DEPENDENT NODE . . . . . . . . .',I10
     .       /5X,'REFERENCE DOF(Trarot). . . . . . . ',3I1,1X,3I1
     .       /5X,'NUMBER OF INDEPENDENT NODES. . .',I10
     .       /5X,'FLAG OF WEIGHTING MODIFICATION .',I10)
 1101 FORMAT(//
     .'     WEIGHTING FACTORS OF INDEPENDENT NODES   '/
     .'     -------------------    '/
     .'         NODE       SKEW         DIR_TRA_1         DIR_TRA_2',
     .'            DIR_TRA_3         DIR_ROT_1         DIR_ROT_2',
     .'            DIR_ROT_3'/)
 1102 FORMAT(3X,2I10,3X,6G20.13)
 1103 FORMAT('      RBE3_ID DEPENDENT_NODE REF_DOF    #IND.   IMODIF'/)
 1104 FORMAT(3X,2I10,2X,3I1,1X,3I1,2I10)
C 
      END SUBROUTINE HM_READ_RBE3 
Chd|====================================================================
Chd|  INIRBE3                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HIREORBE3                     source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        RBE3CHK                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIRBE3(IRBE3  ,LRBE3  ,FRBE3  ,SKEW   ,X     ,
     .                    MS     ,IN     ,NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------   
      USE MESSAGE_MOD       
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
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*), LRBE3(*)
      my_real
     .     SKEW(LSKEW,*), FRBE3(*),X(3,*),MS(*),IN(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL       
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NSL,NM, NI, NMT,M,IROT,IMO,ICR,
     .        J, P,IAD,NS,NN,J6(6),JJ,II,IADS,IERR1,NP,IC,ICT
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C
      my_real
     .   W,WMIN,IN_T
C-----------------------------------------------
C     FRBE3(1-3,I) : tw(i)
C     FRBE3(4-6,I) : rw(i)
C     FRBE3(3*SLRBE3+I) : MS
C     FRBE3(3*SLRBE3+SLRBE3/2+I) : IN
C========================================================================|
C----- re-ordering by hierrrchy (only 1 level)
      CALL HIREORBE3(IRBE3  ,LRBE3  ,FRBE3 ,NOM_OPT)
      NMT = SLRBE3/2
      IADS = 3*SLRBE3
      DO I=1,NRBE3
       IAD = IRBE3(1,I)
       NS = IRBE3(3,I)
       NM = IRBE3(5,I)
       IROT =IRBE3(6,I)
       IMO = IRBE3(8,I)
        
        ID=NOM_OPT(1,I)
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C------ check if solid node for all independent        
        IF (IROT>0) THEN
          IN_T = ZERO
          DO J=1,NM
            M= LRBE3(IAD+J)
            IN_T = IN_T + IN(M)
          ENDDO
          IF (IN_T<EM20) THEN
             CALL ANCMSG(MSGID=3009,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
            IROT = 0
            IRBE3(6,I) = 0
          END IF
         END IF !(IROT>0) THEN
        CALL RBE3CHK(LRBE3(IAD+1),LRBE3(NMT+IAD+1) ,NS     ,X      ,
     .               FRBE3(6*IAD+1),SKEW    ,NM   ,IROT  ,IMO, WMIN,
     .               IERR1 )
        IF (IERR1>0) THEN
             ID= IRBE3(2,I)
             CALL ANCMSG(MSGID=706,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR)
        ELSEIF (IERR1<0) THEN
           IF (IMO==2) THEN
             ID= IRBE3(2,I)
             CALL ANCMSG(MSGID=749,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
           ELSE
             ID= IRBE3(2,I)
             CALL ANCMSG(MSGID=757,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   R1=WMIN)
           END IF
        END IF
        DO J=1,NM
          M= LRBE3(IAD+J)
          FRBE3(IADS+J) =MS(M)
          IF (IRODDL/=0) FRBE3(IADS+NMT+J) =IN(M)
        ENDDO
C------IF NS not in any proc : done already in domdec2
        DO P = 1, NSPMD
         IF (NLOCAL(NS,P)==0) THEN
           GO TO 200
         ENDIF
         DO J=1,NM
          M= LRBE3(IAD+J)
          IF (NSPMD>1) CALL IFRONTPLUS(M,P)
         ENDDO
C optimisation possible
 200     CONTINUE
        END DO
       IADS = IADS + NM
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3CHK                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        INIRBE3                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- calls ---------------
Chd|        INVERT                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3UF                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3UM                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        WRRINF                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        ZERO1                         source/constraints/general/rbe3/hm_read_rbe3.F
Chd|====================================================================
      SUBROUTINE RBE3CHK(INRBE3  ,ILRBE3  ,NS     ,XYZ    ,FRBE3   ,
     .                   SKEW    ,NG      ,IROT   ,IMODIF ,WMIN    ,
     .                   IERR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INRBE3(*),ILRBE3(*),NG, NS,IROT,IMODIF,IERR
C     REAL
      my_real
     .   XYZ(3,*), FRBE3(6,*), SKEW(LSKEW,*),WMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,N, M ,NML, IAD,JJ,KG,NSNGLR,IELSUB,KDIAG
C     REAL
      my_real
     *        TW(3,NG), RW(3,NG),
     *        FUFXLC(3,NG), FUFYLC(3,NG), FUFZLC(3,NG),
     *        FUMXLC(3,NG), FUMYLC(3,NG), FUMZLC(3,NG),
     *        MXLC(3,NG), MYLC(3,NG), MZLC(3,NG),
     *        FUFX(3,NG), FUFY(3,NG), FUFZ(3,NG),
     *        MUFX(3,NG), MUFY(3,NG), MUFZ(3,NG),
     *        FUMX(3,NG), FUMY(3,NG), FUMZ(3,NG),
     *        MX(3,NG), MY(3,NG), MZ(3,NG),
     *        MUMX(3,NG), MUMY(3,NG), MUMZ(3,NG),
     *        FLOCAL(3,NG,6), MLOCAL(3,NG,6),
     *        FBASIC(3,NG,6), MBASIC(3,NG,6),
     *        FDSTNL(3,NG,6), MDSTNL(3,NG,6),
     *        FDSTNB(3,NG,6), MDSTNB(3,NG,6),EL(3,3,NG)
      my_real
     *                 DENFX, DENFY, DENFZ, DENMX, DENMY, DENMZ,
     *                 REFPT(3), CGMX(3), CGMY(3), CGMZ(3), AVEREF,
     *                 TFUFX(3), TFUFY(3), TFUFZ(3),
     *                 TMUFX(3), TMUFY(3), TMUFZ(3),
     *                 TFUMX(3), TFUMY(3), TFUMZ(3),
     *                 TMUMX(3), TMUMY(3), TMUMZ(3),
     *                 A(6,6), C(6,6), T(3,3),SMIN,SMAX,MMAX,TMAX
C
C     INITIALIZATION
C
      CALL ZERO1(FLOCAL,3*NG*6)
      CALL ZERO1(MLOCAL,3*NG*6)
      CALL ZERO1(FBASIC,3*NG*6)
      CALL ZERO1(MBASIC,3*NG*6)
      CALL ZERO1(FDSTNL,3*NG*6)
      CALL ZERO1(MDSTNL,3*NG*6)
      CALL ZERO1(FDSTNB,3*NG*6)
      CALL ZERO1(MDSTNB,3*NG*6)
      CALL ZERO1(A,36)
      CALL ZERO1(C,36)
      CALL ZERO1(CGMX,3)
      CALL ZERO1(CGMY,3)
      CALL ZERO1(CGMZ,3)
      IERR = 0
      WMIN =ZERO
C----------debug use------------
      KDIAG = 0
      REFPT(1) = XYZ(1,NS)
      REFPT(2) = XYZ(2,NS)
      REFPT(3) = XYZ(3,NS)
      DO K = 1, NG
            DO I = 1, 3
               TW(I,K) = FRBE3(I,K)
               RW(I,K) = FRBE3(I+3,K)
            ENDDO
      ENDDO
C
C     ERROR OUT IF RBE3 ELEMENT HAS TWO INDEPENDENT NODES WITH
C     NO ROTATIONAL WEIGHTS SET (THIS MEANS THE ELEMENT CANNOT
C     SUPPORT A MOMENT ALONG ITS AXIS)
C
      IF (NG == 2.AND.IROT==0) THEN
            IERR = 322
            GOTO 999
      ENDIF
C
C     CALCULATE DIRECTION COSINES OF LOCAL COORDINATE SYSTEMS, IF ANY
C
        DO K = 1, NG
            IELSUB = ILRBE3(K)
            IF (IELSUB > 0) THEN
               DO I = 1, 3
                     EL(I,1,K) = SKEW(I,IELSUB)
                     EL(I,2,K) = SKEW(I+3,IELSUB)
                     EL(I,3,K) = SKEW(I+6,IELSUB)
               ENDDO
            ENDIF
        ENDDO
C
C     DENOMINATORS FOR DISTRIBUTING FORCES (DENFX, DENFY AND DENFZ)
C
      DENFX = ZERO
      DENFY = ZERO
      DENFZ = ZERO
      AVEREF = ZERO
C
      DO 70 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           IF GRID POINT HAS A LOCAL COORDINATE SYSTEM
C
            DO 60 I = 1, 3
               DENFX = DENFX + TW(I,K)*EL(I,1,K)**2
               DENFY = DENFY + TW(I,K)*EL(I,2,K)**2
               DENFZ = DENFZ + TW(I,K)*EL(I,3,K)**2
 60         CONTINUE
         ELSE
            DENFX = DENFX + TW(1,K)
            DENFY = DENFY + TW(2,K)
            DENFZ = DENFZ + TW(3,K)
         END IF
C
            AVEREF = AVEREF + SQRT( (XYZ(1,KG) - REFPT(1))**2 +
     *                              (XYZ(2,KG) - REFPT(2))**2 +
     *                              (XYZ(3,KG) - REFPT(3))**2 )
 70   CONTINUE
C
      IF (ABS(DENFX) <= EM20) THEN
         IERR = 326
      ENDIF
C
      IF (ABS(DENFY) <= EM20) THEN
         IERR = 327
      ENDIF
C
      IF (ABS(DENFZ) <= EM20) THEN
         IERR = 328
      ENDIF
      IF (IERR > 0) GOTO 999
         AVEREF = AVEREF/NG
         IF (AVEREF == ZERO) AVEREF = 1.0D0
C--- IMODIF=4: normalized TW;       
       IF (IMODIF==4) THEN
         DO K = 1, NG
           FRBE3(1,K) =  FRBE3(1,K)/DENFX
           FRBE3(2,K) =  FRBE3(2,K)/DENFY
           FRBE3(3,K) =  FRBE3(3,K)/DENFZ
           FRBE3(4,K) =  FRBE3(4,K)/DENFX
           FRBE3(5,K) =  FRBE3(5,K)/DENFY
           FRBE3(6,K) =  FRBE3(6,K)/DENFZ
         ENDDO
       END IF
C
C     CALCULATE 3 CENTERS OF GRAVITY (CGMX, CGMY AND CGMZ) AND
C     DENOMINATORS FOR DISTRIBUTING MOMENTS (DENMX, DENMY AND DENMZ)
C
      DO 40 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           IF THERE IS A LOCAL COORDINATE SYSTEM AT THE GRID POINT
C
            DO 10 I = 1, 3
               CGMX(2) = CGMX(2) + TW(I,K)*EL(I,3,K)**2*XYZ(2,KG)
               CGMX(3) = CGMX(3) + TW(I,K)*EL(I,2,K)**2*XYZ(3,KG)
 10         CONTINUE
C
            DO 20 I = 1, 3
               CGMY(3) = CGMY(3) + TW(I,K)*EL(I,1,K)**2*XYZ(3,KG)
               CGMY(1) = CGMY(1) + TW(I,K)*EL(I,3,K)**2*XYZ(1,KG)
 20         CONTINUE
C
            DO 30 I = 1, 3
               CGMZ(1) = CGMZ(1) + TW(I,K)*EL(I,2,K)**2*XYZ(1,KG)
               CGMZ(2) = CGMZ(2) + TW(I,K)*EL(I,1,K)**2*XYZ(2,KG)
 30         CONTINUE
C
         ELSE
            CGMX(2) = CGMX(2) + TW(3,K)*XYZ(2,KG)
            CGMX(3) = CGMX(3) + TW(2,K)*XYZ(3,KG)
C
            CGMY(3) = CGMY(3) + TW(1,K)*XYZ(3,KG)
            CGMY(1) = CGMY(1) + TW(3,K)*XYZ(1,KG)
C
            CGMZ(1) = CGMZ(1) + TW(2,K)*XYZ(1,KG)
            CGMZ(2) = CGMZ(2) + TW(1,K)*XYZ(2,KG)
         END IF
 40   CONTINUE
         CGMX(2) = CGMX(2)/DENFZ
         CGMX(3) = CGMX(3)/DENFY
C
         CGMY(3) = CGMY(3)/DENFX
         CGMY(1) = CGMY(1)/DENFZ
C
         CGMZ(1) = CGMZ(1)/DENFY
         CGMZ(2) = CGMZ(2)/DENFX
C
      DENMX = ZERO
      DENMY = ZERO
      DENMZ = ZERO
C
      DO 90 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
C
C        NOTE: AS IMPLEMENTED IN NASTRAN 70.7, WE SCALE THE ROTATIONAL
C              WEIGHTS WITH THE SQUARE OF THE AVERAGE DISTANCE OF THE
C              INDEPENDENT GRID POINTS FROM THE REFERENCE POINT TO
C              RENDER THE RBE3 CALCULATIONS UNIT INDEPENDENT
C
         IF (IELSUB > 0) THEN
C
C           IF GRID POINT HAS A LOCAL COORDINATE SYSTEM
C
            DO 80 I = 1, 3
               DENMX = DENMX + RW(I,K)*EL(I,1,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,3,K)*(XYZ(2,KG) - CGMX(2)) -
     *                           EL(I,2,K)*(XYZ(3,KG) - CGMX(3))
     *                         ) **2
               DENMY = DENMY + RW(I,K)*EL(I,2,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,1,K)*(XYZ(3,KG) - CGMY(3)) -
     *                           EL(I,3,K)*(XYZ(1,KG) - CGMY(1))
     *                         ) **2
               DENMZ = DENMZ + RW(I,K)*EL(I,3,K)**2*AVEREF**2 +
     *                 TW(I,K)*( EL(I,2,K)*(XYZ(1,KG) - CGMZ(1)) -
     *                           EL(I,1,K)*(XYZ(2,KG) - CGMZ(2))
     *                         ) **2
 80         CONTINUE
         ELSE
            DENMX = DENMX + RW(1,K)*AVEREF**2 +
     *                      TW(2,K)*(XYZ(3,KG) - CGMX(3))**2 +
     *                      TW(3,K)*(XYZ(2,KG) - CGMX(2))**2
            DENMY = DENMY + RW(2,K)*AVEREF**2 +
     *                      TW(1,K)*(XYZ(3,KG) - CGMY(3))**2 +
     *                      TW(3,K)*(XYZ(1,KG) - CGMY(1))**2
            DENMZ = DENMZ + RW(3,K)*AVEREF**2 +
     *                      TW(2,K)*(XYZ(1,KG) - CGMZ(1))**2 +
     *                      TW(1,K)*(XYZ(2,KG) - CGMZ(2))**2
         END IF
 90   CONTINUE
C
C     PERFORM SOME CHECKS ON WEIGHTS, TO MAKE SURE THAT THE RBE3
C     ELEMENT HAS NO UNCONSTRAINED DEGREES OF FREEDOM
C
C
      IF (ABS(DENMX) <= EM20) THEN
         IERR = 329
      ENDIF
C
      IF (ABS(DENMY) <= EM20) THEN
         IERR = 330
      ENDIF
C
      IF (ABS(DENMZ) <= EM20) THEN
         IERR = 331
      ENDIF
C
      SMIN = MIN(ABS(DENMX),ABS(DENMY),ABS(DENMZ))
      SMAX = MAX(ABS(DENMX),ABS(DENMY),ABS(DENMZ))
C
      IF (IERR > 0) GOTO 999
C
      IF (IROT==0 .AND.(SMAX/SMIN)>THIRTY) IERR = -100
C     CALCULATE 3 FORCE DISTRIBUTIONS THAT CREATE NET X, Y AND Z FORCES
C     OF 1 (BESIDES OTHER NONZERO FORCES/MOMENTS IN ALL THE DIRECTIONS)
C
      CALL RBE3UF(INRBE3,ILRBE3,EL,TW,XYZ,REFPT,
     *            FUFXLC,FUFYLC,FUFZLC,FUFX,FUFY,FUFZ,MUFX,MUFY,MUFZ,
     *            TFUFX,TFUFY,TFUFZ,TMUFX,TMUFY,TMUFZ,
     *            DENFX,DENFY,DENFZ,NG)
C
C     CALCULATE 3 MOMENT/FORCE DISTRIBUTIONS THAT CREATE NET X, Y AND Z
C     MOMENTS OF 1 (BESIDES OTHER NONZERO FORCES/MOMENTS IN ALL THE
C     DIRECTIONS) AT CGMX, CGMY AND CGMZ RESPECTIVELY
C
      CALL RBE3UM(INRBE3,ILRBE3,EL,TW,RW,XYZ,REFPT,CGMX,CGMY,CGMZ,
     *            FUMXLC,FUMYLC,FUMZLC,MXLC,MYLC,MZLC,
     *            FUMX,FUMY,FUMZ,MX,MY,MZ,MUMX,MUMY,MUMZ,
     *            TFUMX,TFUMY,TFUMZ,TMUMX,TMUMY,TMUMZ,
     *            AVEREF,DENMX,DENMY,DENMZ,NG,IROT )
C
C     DETERMINE COMBINATORY COEFFICIENTS FOR THESE 6 DISTRIBUTIONS
C     (6 COEFFICIENTS FOR EACH OF 6 CASES)
C
C     CASE 1 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT X-FORCE AT REFERENCE POINT
C     CASE 2 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Y-FORCE AT REFERENCE POINT
C     CASE 3 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Z-FORCE AT REFERENCE POINT
C     CASE 4 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT X-MOMENT AT REFERENCE POINT
C     CASE 5 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Y-MOMENT AT REFERENCE POINT
C     CASE 6 - RESULTANT OF THE LINEAR COMBINATION OF THESE FORCE/MOMENT
C              DISTRIBUTIONS IS A UNIT Z-MOMENT AT REFERENCE POINT
C
C     IN ORDER TO DETERMINE THESE COEFFICIENTS, FIRST SET UP A 6X6
C     MATRIX.  THE 6 COLUMNS OF THE INVERSE OF THIS MATRIX ARE THE
C     DESIRED 6 SETS OF COEFFICIENTS.
C
      DO 120 I = 1, 3
         K = I + 3
         A(I,1) = TFUFX(I)
         A(K,1) = TMUFX(I)
         A(I,2) = TFUFY(I)
         A(K,2) = TMUFY(I)
         A(I,3) = TFUFZ(I)
         A(K,3) = TMUFZ(I)
         A(I,4) = TFUMX(I)
         A(K,4) = TMUMX(I)
         A(I,5) = TFUMY(I)
         A(K,5) = TMUMY(I)
         A(I,6) = TFUMZ(I)
         A(K,6) = TMUMZ(I)
 120  CONTINUE
C
C     INVERT THE 6X6 MATRIX
C
      NSNGLR  = 0
      CALL INVERT(A,C,6,NSNGLR)
      IF (NSNGLR /= 0) THEN
         IERR = 332
         GOTO 999
      ENDIF
      IF (KDIAG >= 1) THEN
         CALL WRRINF('C(i,1)=',c(1,1),3)
         CALL WRRINF('C(i,2)=',c(1,2),3)
         CALL WRRINF('C(i,3)=',c(1,3),3)
      ENDIF
      IF (KDIAG==0.AND.IERR==0) RETURN
C
C     LINEARLY COMBINE FORCE/MOMENT DISTRIBUTIONS FOR THE 6 CASES
C
      DO 170 J = 1, 6
         DO 160 K = 1, NG
            DO 150 I = 1, 3
               FLOCAL(I,K,J) = C(1,J)*FUFXLC(I,K) + C(2,J)*FUFYLC(I,K) +
     *                         C(3,J)*FUFZLC(I,K) + C(4,J)*FUMXLC(I,K) +
     *                         C(5,J)*FUMYLC(I,K) + C(6,J)*FUMZLC(I,K)
               MLOCAL(I,K,J) = C(4,J)*MXLC(I,K) + C(5,J)*MYLC(I,K) +
     *                         C(6,J)*MZLC(I,K)
               FBASIC(I,K,J) = C(1,J)*FUFX(I,K) + C(2,J)*FUFY(I,K) +
     *                         C(3,J)*FUFZ(I,K) + C(4,J)*FUMX(I,K) +
     *                         C(5,J)*FUMY(I,K) + C(6,J)*FUMZ(I,K)
               MBASIC(I,K,J) = C(4,J)*MX(I,K) + C(5,J)*MY(I,K) +
     *                         C(6,J)*MZ(I,K)
 150        CONTINUE
 160     CONTINUE
 170  CONTINUE
C
C
C        NO LOCAL COORDINATE SYSTEM AT REFERENCE POINT
C
         DO 270 J = 1, 6
            DO 260 K = 1, NG
               DO 250 I = 1, 3
                  FDSTNL(I,K,J) = FLOCAL(I,K,J)
                  MDSTNL(I,K,J) = MLOCAL(I,K,J)
                  FDSTNB(I,K,J) = FBASIC(I,K,J)
                  MDSTNB(I,K,J) = MBASIC(I,K,J)
 250           CONTINUE
 260        CONTINUE
 270     CONTINUE
C--------------special case with Imodif
       IF (IERR==-100) THEN
        MMAX=ZERO
         DO J = 4, 6
            DO K = 1, NG
              DO I = 1, 3
               IF (MMAX<ABS(FDSTNB(I,K,J))) MMAX = ABS(FDSTNB(I,K,J))
              END DO
            END DO
         END DO
        IF (MMAX<=ONE) THEN
         IERR=0
        ELSE
         TMAX=ZERO
         IF (IMODIF/=2) THEN
           DO K = 1, NG
            DO I = 1, 3
              IF (TMAX<TW(I,K)) TMAX=TW(I,K)
            ENDDO
           ENDDO
         ENDIF
         WMIN=TMAX*EM04
         DO K = 1, NG
           FRBE3(1,K) = MAX(WMIN,FRBE3(1,K))
           FRBE3(2,K) = MAX(WMIN,FRBE3(2,K))
           FRBE3(3,K) = MAX(WMIN,FRBE3(3,K))
         ENDDO
        ENDIF
       END IF
C
 999  CONTINUE
C
C     DIAGNOSTIC INFORMATION
C
      IF (KDIAG >= 2) THEN
c         CALL WRRINF('REF_POINT',REFPT,3)
c         CALL WRRINF('INDPT_GRDS',INRBE3,NG)
c      IF (SMAX/SMIN > THIRTY) print *,'SMAX/SMIN=',SMAX/SMIN
         CALL WRRINF('TRAN_WGHTS',TW,3*NG)
         CALL WRRINF('ROT_WGHTS',RW,3*NG)
         CALL WRRINF('CGMX',CGMX,3)
         CALL WRRINF('CGMY',CGMY,3)
         CALL WRRINF('CGMZ',CGMZ,3)
         CALL WRRINF('DENFX',DENFX,1)
         CALL WRRINF('DENFY',DENFY,1)
         CALL WRRINF('DENFZ',DENFZ,1)
         CALL WRRINF('DENMX',DENMX,1)
         CALL WRRINF('DENMY',DENMY,1)
         CALL WRRINF('DENMZ',DENMZ,1)
         CALL WRRINF('AVEREF',AVEREF,1)
C
            IF (KDIAG == 9.or.IERR/=0) THEN
               CALL WRRINF('FDSTNB_ULFX@REF',FDSTNB(1,1,1),3*NG)
               CALL WRRINF('FDSTNB_ULFY@REF',FDSTNB(1,1,2),3*NG)
               CALL WRRINF('FDSTNB_ULFZ@REF',FDSTNB(1,1,3),3*NG)
               CALL WRRINF('FDSTNB_ULMX@REF',FDSTNB(1,1,4),3*NG)
               CALL WRRINF('FDSTNB_ULMY@REF',FDSTNB(1,1,5),3*NG)
               CALL WRRINF('FDSTNB_ULMZ@REF',FDSTNB(1,1,6),3*NG)
               CALL WRRINF('MDSTNB_ULFX@REF',MDSTNB(1,1,1),3*NG)
               CALL WRRINF('MDSTNB_ULFY@REF',MDSTNB(1,1,2),3*NG)
               CALL WRRINF('MDSTNB_ULFZ@REF',MDSTNB(1,1,3),3*NG)
               CALL WRRINF('MDSTNB_ULMX@REF',MDSTNB(1,1,4),3*NG)
               CALL WRRINF('MDSTNB_ULMY@REF',MDSTNB(1,1,5),3*NG)
               CALL WRRINF('MDSTNB_ULMZ@REF',MDSTNB(1,1,6),3*NG)
            ENDIF
         IF (KDIAG >= 30) THEN
            CALL WRRINF('FDSTNL_ULFX@REF',FDSTNL(1,1,1),3*NG)
            CALL WRRINF('FDSTNL_ULFY@REF',FDSTNL(1,1,2),3*NG)
            CALL WRRINF('FDSTNL_ULFZ@REF',FDSTNL(1,1,3),3*NG)
            CALL WRRINF('FDSTNL_ULMX@REF',FDSTNL(1,1,4),3*NG)
            CALL WRRINF('FDSTNL_ULMY@REF',FDSTNL(1,1,5),3*NG)
            CALL WRRINF('FDSTNL_ULMZ@REF',FDSTNL(1,1,6),3*NG)
            CALL WRRINF('MDSTNL_ULFX@REF',MDSTNL(1,1,1),3*NG)
            CALL WRRINF('MDSTNL_ULFY@REF',MDSTNL(1,1,2),3*NG)
            CALL WRRINF('MDSTNL_ULFZ@REF',MDSTNL(1,1,3),3*NG)
            CALL WRRINF('MDSTNL_ULMX@REF',MDSTNL(1,1,4),3*NG)
            CALL WRRINF('MDSTNL_ULMY@REF',MDSTNL(1,1,5),3*NG)
            CALL WRRINF('MDSTNL_ULMZ@REF',MDSTNL(1,1,6),3*NG)
C
C
         ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3UF                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        RBE3CHK                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3CL                        source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|        ZERO1                         source/constraints/general/rbe3/hm_read_rbe3.F
Chd|====================================================================
      SUBROUTINE RBE3UF(INRBE3,ILRBE3,EL,TW,XYZ,REFPT,
     *                  FUFXLC,FUFYLC,FUFZLC,
     *                  FUFX,FUFY,FUFZ,MUFX,MUFY,MUFZ,
     *                  TFUFX,TFUFY,TFUFZ,TMUFX,TMUFY,TMUFZ,
     *                  DENFX,DENFY,DENFZ,NG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER NG
      INTEGER INRBE3(NG), ILRBE3(NG)
      my_real
     *                 EL(3,3,*),TW(3,NG), XYZ(3,*), REFPT(3),
     *                 FUFXLC(3,NG), FUFYLC(3,NG), FUFZLC(3,NG),
     *                 FUFX(3,NG), FUFY(3,NG), FUFZ(3,NG),
     *                 MUFX(3,NG), MUFY(3,NG), MUFZ(3,NG),
     *                 TFUFX(3), TFUFY(3), TFUFZ(3),
     *                 TMUFX(3), TMUFY(3), TMUFZ(3)
      my_real
     *    DENFX, DENFY, DENFZ,XARM, YARM, ZARM
      INTEGER I, J, K, KG, IELSUB
C
C     INITIALIZE FORCE AND MOMENT DISTRIBUTIONS TO ZERO
C
      CALL ZERO1(FUFX,3*NG)
      CALL ZERO1(FUFY,3*NG)
      CALL ZERO1(FUFZ,3*NG)
      CALL ZERO1(TFUFX,3)
      CALL ZERO1(TFUFY,3)
      CALL ZERO1(TFUFZ,3)
      CALL ZERO1(TMUFX,3)
      CALL ZERO1(TMUFY,3)
      CALL ZERO1(TMUFZ,3)
C
C     FORCE DISTRIBUTIONS AT RBE3 GRID POINTS CORRESPONDING TO UNIT
C     APPLIED FORCES AT RBE3 REFERENCE POINT ALONG (BASIC COORDINATE)
C     X, Y AND Z DIRECTIONS
C
      DO 50 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           FORCES AT GRID POINT ALONG GRID POINT'S LOCAL (OUTPUT)
C           COORDINATE AXES
C
            DO 10 I = 1, 3
               FUFXLC(I,K) = TW(I,K)*EL(I,1,K)/DENFX
               FUFYLC(I,K) = TW(I,K)*EL(I,2,K)/DENFY
               FUFZLC(I,K) = TW(I,K)*EL(I,3,K)/DENFZ
 10         CONTINUE
C
C           FORCES AT GRID POINT ALONG BASIC COORDINATE AXES
C
            DO 30 I = 1, 3
               DO 20 J = 1, 3
                  FUFX(J,K) = FUFX(J,K) + FUFXLC(I,K)*EL(I,J,K)
                  FUFY(J,K) = FUFY(J,K) + FUFYLC(I,K)*EL(I,J,K)
                  FUFZ(J,K) = FUFZ(J,K) + FUFZLC(I,K)*EL(I,J,K)
 20            CONTINUE
 30         CONTINUE
C
         ELSE
            FUFXLC(1,K) = TW(1,K)/DENFX
            FUFYLC(2,K) = TW(2,K)/DENFY
            FUFZLC(3,K) = TW(3,K)/DENFZ
            FUFX(1,K) = FUFXLC(1,K)
            FUFY(2,K) = FUFYLC(2,K)
            FUFZ(3,K) = FUFZLC(3,K)
         ENDIF
C
C        MOMENTS AT REFERENCE POINT DUE TO THESE FORCE DISTRIBUTIONS
C
         XARM = XYZ(1,KG) - REFPT(1)
         YARM = XYZ(2,KG) - REFPT(2)
         ZARM = XYZ(3,KG) - REFPT(3)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUFX
C
         MUFX(1,K) = YARM*FUFX(3,K) - ZARM*FUFX(2,K)
         MUFX(2,K) = ZARM*FUFX(1,K) - XARM*FUFX(3,K)
         MUFX(3,K) = XARM*FUFX(2,K) - YARM*FUFX(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUFY
C
         MUFY(1,K) = YARM*FUFY(3,K) - ZARM*FUFY(2,K)
         MUFY(2,K) = ZARM*FUFY(1,K) - XARM*FUFY(3,K)
         MUFY(3,K) = XARM*FUFY(2,K) - YARM*FUFY(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUFZ
C
         MUFZ(1,K) = YARM*FUFZ(3,K) - ZARM*FUFZ(2,K)
         MUFZ(2,K) = ZARM*FUFZ(1,K) - XARM*FUFZ(3,K)
         MUFZ(3,K) = XARM*FUFZ(2,K) - YARM*FUFZ(1,K)
C
C        TOTAL FORCES AND MOMENTS
C
         DO 40 J = 1, 3
            TFUFX(J) = TFUFX(J) + FUFX(J,K)
            TFUFY(J) = TFUFY(J) + FUFY(J,K)
            TFUFZ(J) = TFUFZ(J) + FUFZ(J,K)
            TMUFX(J) = TMUFX(J) + MUFX(J,K)
            TMUFY(J) = TMUFY(J) + MUFY(J,K)
            TMUFZ(J) = TMUFZ(J) + MUFZ(J,K)
 40      CONTINUE
C
 50   CONTINUE
C
      RETURN
      END
C
Chd|====================================================================
Chd|  RBE3UM                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        RBE3CHK                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3CL                        source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|        ZERO1                         source/constraints/general/rbe3/hm_read_rbe3.F
Chd|====================================================================
      SUBROUTINE RBE3UM(INRBE3,ILRBE3,EL,TW,RW,XYZ,REFPT,CGMX,CGMY,CGMZ,
     *                  FUMXLC,FUMYLC,FUMZLC,MXLC,MYLC,MZLC,
     *                  FUMX,FUMY,FUMZ,MX,MY,MZ,MUMX,MUMY,MUMZ,
     *                  TFUMX,TFUMY,TFUMZ,TMUMX,TMUMY,TMUMZ,
     *                  AVEREF,DENMX,DENMY,DENMZ,NG ,IROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER NG,IROT
      INTEGER INRBE3(NG), ILRBE3(NG)
      my_real
     *                 EL(3,3,*),TW(3,NG), RW(3,NG), XYZ(3,*),
     *                 REFPT(3), CGMX(3), CGMY(3), CGMZ(3),
     *                 FUMXLC(3,NG), FUMYLC(3,NG), FUMZLC(3,NG),
     *                 MXLC(3,NG), MYLC(3,NG), MZLC(3,NG),
     *                 FUMX(3,NG), FUMY(3,NG), FUMZ(3,NG),
     *                 MX(3,NG), MY(3,NG), MZ(3,NG),
     *                 MUMX(3,NG), MUMY(3,NG), MUMZ(3,NG),
     *                 TFUMX(3), TFUMY(3), TFUMZ(3),
     *                 TMUMX(3), TMUMY(3), TMUMZ(3)
      my_real
     *         AVEREF, DENMX, DENMY, DENMZ,XARM, YARM, ZARM
      INTEGER I, J, K, KG, IELSUB
C
C     INITIALIZE FORCE AND MOMENT DISTRIBUTIONS TO ZERO
C
      CALL ZERO1(FUMX,3*NG)
      CALL ZERO1(FUMY,3*NG)
      CALL ZERO1(FUMZ,3*NG)
      CALL ZERO1(MX,3*NG)
      CALL ZERO1(MY,3*NG)
      CALL ZERO1(MZ,3*NG)
      CALL ZERO1(TFUMX,3)
      CALL ZERO1(TFUMY,3)
      CALL ZERO1(TFUMZ,3)
      CALL ZERO1(TMUMX,3)
      CALL ZERO1(TMUMY,3)
      CALL ZERO1(TMUMZ,3)
C
C     FORCE AND MOMENT DISTRIBUTIONS AT RBE3 GRID POINTS CORRESPONDING
C     TO UNIT APPLIED MOMENTS AT RBE3 REFERENCE POINT ALONG (BASIC
C     COORDINATE) X, Y AND Z DIRECTIONS
C
      DO 50 K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           FORCES  AT GRID POINT ALONG GRID POINT'S LOCAL
C           (OUTPUT) COORDINATE AXES
C
            DO 10 I = 1, 3
               FUMXLC(I,K) = TW(I,K)*
     *                       ( EL(I,3,K)*(XYZ(2,KG) - CGMX(2)) -
     *                         EL(I,2,K)*(XYZ(3,KG) - CGMX(3))
     *                       )/DENMX
               FUMYLC(I,K) = TW(I,K)*
     *                       ( EL(I,1,K)*(XYZ(3,KG) - CGMY(3)) -
     *                         EL(I,3,K)*(XYZ(1,KG) - CGMY(1))
     *                       )/DENMY
               FUMZLC(I,K) = TW(I,K)*
     *                       ( EL(I,2,K)*(XYZ(1,KG) - CGMZ(1)) -
     *                         EL(I,1,K)*(XYZ(2,KG) - CGMZ(2))
     *                       )/DENMZ
 10         CONTINUE
C
C           FORCES AND MOMENTS AT GRID POINT ALONG BASIC COORDINATE AXES
C
            DO 30 I = 1, 3
               DO 20 J = 1, 3
                  FUMX(J,K) = FUMX(J,K) + FUMXLC(I,K)*EL(I,J,K)
                  FUMY(J,K) = FUMY(J,K) + FUMYLC(I,K)*EL(I,J,K)
                  FUMZ(J,K) = FUMZ(J,K) + FUMZLC(I,K)*EL(I,J,K)
 20            CONTINUE
 30         CONTINUE
C
         ELSE
            FUMXLC(2,K) = -TW(2,K)*(XYZ(3,KG) - CGMX(3))/DENMX
            FUMXLC(3,K) = TW(3,K)*(XYZ(2,KG) - CGMX(2))/DENMX
            FUMYLC(1,K) = TW(1,K)*(XYZ(3,KG) - CGMY(3))/DENMY
            FUMYLC(3,K) = -TW(3,K)*(XYZ(1,KG) - CGMY(1))/DENMY
            FUMZLC(1,K) = -TW(1,K)*(XYZ(2,KG) - CGMZ(2))/DENMZ
            FUMZLC(2,K) = TW(2,K)*(XYZ(1,KG) - CGMZ(1))/DENMZ
C
            FUMX(2,K) = FUMXLC(2,K)
            FUMX(3,K) = FUMXLC(3,K)
            FUMY(1,K) = FUMYLC(1,K)
            FUMY(3,K) = FUMYLC(3,K)
            FUMZ(1,K) = FUMZLC(1,K)
            FUMZ(2,K) = FUMZLC(2,K)
         ENDIF
C
C        MOMENTS AT REFERENCE POINT DUE TO FUMX
C
         XARM = XYZ(1,KG) - REFPT(1)
         YARM = XYZ(2,KG) - REFPT(2)
         ZARM = XYZ(3,KG) - REFPT(3)
C
         MUMX(1,K) = YARM*FUMX(3,K) - ZARM*FUMX(2,K)
         MUMX(2,K) = ZARM*FUMX(1,K) - XARM*FUMX(3,K)
         MUMX(3,K) = XARM*FUMX(2,K) - YARM*FUMX(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUMY
C
         MUMY(1,K) = YARM*FUMY(3,K) - ZARM*FUMY(2,K)
         MUMY(2,K) = ZARM*FUMY(1,K) - XARM*FUMY(3,K)
         MUMY(3,K) = XARM*FUMY(2,K) - YARM*FUMY(1,K)
C
C        MOMENTS AT REFERENCE POINT DUE TO FUMZ
C
         MUMZ(1,K) = YARM*FUMZ(3,K) - ZARM*FUMZ(2,K)
         MUMZ(2,K) = ZARM*FUMZ(1,K) - XARM*FUMZ(3,K)
         MUMZ(3,K) = XARM*FUMZ(2,K) - YARM*FUMZ(1,K)
C
 50   CONTINUE
C
      IF (IROT>0) THEN
       DO K = 1, NG
         KG = INRBE3(K)
         IELSUB = ILRBE3(K)
         IF (IELSUB > 0) THEN
C
C           MOMENTS AT GRID POINT ALONG GRID POINT'S LOCAL
C           (OUTPUT) COORDINATE AXES
C
            DO I = 1, 3
               MXLC(I,K) = AVEREF**2*RW(I,K)*EL(I,1,K)/DENMX
               MYLC(I,K) = AVEREF**2*RW(I,K)*EL(I,2,K)/DENMY
               MZLC(I,K) = AVEREF**2*RW(I,K)*EL(I,3,K)/DENMZ
            END DO
C
C           MOMENTS AT GRID POINT ALONG BASIC COORDINATE AXES
C
            DO I = 1, 3
               DO J = 1, 3
                  MX(J,K) = MX(J,K) + MXLC(I,K)*EL(I,J,K)
                  MY(J,K) = MY(J,K) + MYLC(I,K)*EL(I,J,K)
                  MZ(J,K) = MZ(J,K) + MZLC(I,K)*EL(I,J,K)
               END DO
            END DO
C
         ELSE
            MXLC(1,K) = AVEREF**2*RW(1,K)/DENMX
            MYLC(2,K) = AVEREF**2*RW(2,K)/DENMY
            MZLC(3,K) = AVEREF**2*RW(3,K)/DENMZ
C
            MX(1,K) = MXLC(1,K)
            MY(2,K) = MYLC(2,K)
            MZ(3,K) = MZLC(3,K)
         ENDIF
C
         DO J = 1, 3
          MUMX(J,K) = MUMX(J,K) + MX(J,K)
          MUMY(J,K) = MUMY(J,K) + MY(J,K)
          MUMZ(J,K) = MUMZ(J,K) + MZ(J,K)
         END DO
       END DO
      END IF
C
C
C        TOTAL FORCES AND MOMENTS
C
C
      DO K = 1, NG
         DO J = 1, 3
            TFUMX(J) = TFUMX(J) + FUMX(J,K)
            TFUMY(J) = TFUMY(J) + FUMY(J,K)
            TFUMZ(J) = TFUMZ(J) + FUMZ(J,K)
            TMUMX(J) = TMUMX(J) + MUMX(J,K)
            TMUMY(J) = TMUMY(J) + MUMY(J,K)
            TMUMZ(J) = TMUMZ(J) + MUMZ(J,K)
         END DO
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  INVERT                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|        MASS_FLUID_TG                 source/fluid/mass-fluid_tg.F  
Chd|        RBE3CHK                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3CL                        source/constraints/general/kinchk.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE INVERT(MATRIX, INVERSE, N, ERRORFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
c       !DECLARATIONS
        INTEGER, INTENT(IN) :: N
        INTEGER, INTENT(OUT) :: ERRORFLAG  !RETURN ERROR STATUS. -1 FOR ERROR, 0 FOR NORMAL
      my_real
     *     , INTENT(IN), DIMENSION(N,N) :: MATRIX  !INPUT MATRIX
      my_real
     *     , INTENT(OUT), DIMENSION(N,N) :: INVERSE !INVERTED MATRIX

        LOGICAL :: FLAG = .TRUE.
        INTEGER :: I, J, K, L
      my_real
     *     :: M
      my_real
     *       , DIMENSION(N,2*N) :: AUGMATRIX !AUGMENTED MATRIX

c       !AUGMENT INPUT MATRIX WITH AN IDENTITY MATRIX
        DO I = 1, N
                DO J = 1, 2*N
                        IF (J <= N ) THEN
                                AUGMATRIX(I,J) = MATRIX(I,J)
                        ELSE IF ((I+N) == J) THEN
                                AUGMATRIX(I,J) = ONE
                        ELSE
                                AUGMATRIX(I,J) = ZERO
                        ENDIF
                END DO
        END DO

c       !REDUCE AUGMENTED MATRIX TO UPPER TRIANGULAR FORM
        DO K =1, N-1
                IF (AUGMATRIX(K,K) == 0) THEN
                        FLAG = .FALSE.
                        DO I = K+1, N
                                IF (AUGMATRIX(I,K) /= 0) THEN
                                        DO J = 1,2*N
                                                AUGMATRIX(K,J) = AUGMATRIX(K,J)+AUGMATRIX(I,J)
                                        END DO
                                        FLAG = .TRUE.
                                        EXIT
                                ENDIF
                                IF (FLAG .EQV. .FALSE.) THEN
                                        PRINT*, "MATRIX IS NON - INVERTIBLE"
                                        INVERSE = 0
                                        ERRORFLAG = -1
                                        RETURN
                                ENDIF
                        END DO
                ENDIF
                DO J = K+1, N
                        M = AUGMATRIX(J,K)/AUGMATRIX(K,K)
                        DO I = K, 2*N
                                AUGMATRIX(J,I) = AUGMATRIX(J,I) - M*AUGMATRIX(K,I)
                        END DO
                END DO
        END DO

c       !TEST FOR INVERTIBILITY
        DO I = 1, N
                IF (AUGMATRIX(I,I) == 0) THEN
                        PRINT*, "MATRIX IS NON - INVERTIBLE"
                        INVERSE = 0
                        ERRORFLAG = -1
                        RETURN
                ENDIF
        END DO

c       !MAKE DIAGONAL ELEMENTS AS 1
        DO I = 1 , N
                M = AUGMATRIX(I,I)
                DO J = I , (2 * N)
                           AUGMATRIX(I,J) = (AUGMATRIX(I,J) / M)
                END DO
        END DO

c       !REDUCED RIGHT SIDE HALF OF AUGMENTED MATRIX TO IDENTITY MATRIX
        DO K = N-1, 1, -1
                DO I =1, K
                M = AUGMATRIX(I,K+1)
                        DO J = K, (2*N)
                                AUGMATRIX(I,J) = AUGMATRIX(I,J) -AUGMATRIX(K+1,J) * M
                        END DO
                END DO
        END DO

c       !STORE ANSWER
        DO I =1, N
                DO J = 1, N
                        INVERSE(I,J) = AUGMATRIX(I,J+N)
                END DO
        END DO
        ERRORFLAG = 0
        RETURN
        END SUBROUTINE INVERT
C----------------------------
Chd|====================================================================
Chd|  WRRINF                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        RBE3CHK                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRRINF(TITLE,R,N)
#include      "implicit_f.inc"
#include      "units_c.inc"
c       !DECLARATIONS
      INTEGER N
      my_real
     .        R(N),RMAX
      CHARACTER TITLE*(*)
C----------------------------
      INTEGER I
      write(iout, *)TITLE,(R(I),I=1,N)
c      print *,TITLE,(R(I),I=1,N)
      RETURN
      END
C----------------------------
Chd|====================================================================
Chd|  ZERO1                         source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        RBE3CHK                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3CL                        source/constraints/general/kinchk.F
Chd|        RBE3UF                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        RBE3UM                        source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ZERO1(R,N)
#include      "implicit_f.inc"
c       !DECLARATIONS
      INTEGER N
      my_real
     .        R(N)
C----------------------------
      INTEGER I
      DO I = 1,N
       R(I) = ZERO
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE3FR                     source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),JT(3)  ,JR(3),N
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IC,ICT,ICR
C======================================================================|
        IC=IRBE3(4,N)
        ICT=IC/512
        ICR=(IC-512*(ICT))/64
            DO J =1,3
           JT(J)=0
           JR(J)=0
            ENDDO
        SELECT CASE (ICT)
          CASE(1)
           JT(3)=1
          CASE(2)
           JT(2)=1
          CASE(3)
           JT(2)=1
           JT(3)=1
          CASE(4)
           JT(1)=1
          CASE(5)
           JT(1)=1
           JT(3)=1
          CASE(6)
           JT(1)=1
           JT(2)=1
          CASE(7)
           JT(1)=1
           JT(2)=1
           JT(3)=1
       END SELECT
       SELECT CASE (ICR)
          CASE(1)
           JR(3)=1
          CASE(2)
           JR(2)=1
          CASE(3)
           JR(2)=1
           JR(3)=1
          CASE(4)
           JR(1)=1
          CASE(5)
           JR(1)=1
           JR(3)=1
          CASE(6)
           JR(1)=1
           JR(2)=1
          CASE(7)
           JR(1)=1
           JR(2)=1
           JR(3)=1
       END SELECT
C---
      RETURN
      END
Chd|====================================================================
Chd|  HIREORBE3                     source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- called by -----------
Chd|        INIRBE3                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE HIREORBE3(IRBE3  ,LRBE3  ,FRBE3 ,NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*), LRBE3(*),NOM_OPT(LNOPT1,*)
      my_real
     .        FRBE3(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, J,K, NS,NM, NI, NMT,M,NFT,JLT,IAD,II,M2,III
C
      INTEGER ID, INDEX(NRBE3),ITAG(NUMNOD),NZ,IAD1,IADS,
     .        LCOPY(SLRBE3),ICOPY(NRBE3L,NRBE3)
      CHARACTER*nchartitle,
     .   TITR
      my_real
     .        FCOPY(SLRBE3*3)
C========================================================================|
C-----only one level of hierarchy is allowed
      ITAG(1:NUMNOD)=0
      DO I=1,NRBE3
       NS = IRBE3(3,I)
       IF (ITAG(NS)==0) ITAG(NS)=I
       INDEX(I) = I
      ENDDO
      NZ = 0
      DO I=1,NRBE3
       IAD = IRBE3(1,I)
       NM = IRBE3(5,I)
       DO J =1,NM
        M = LRBE3(IAD+J)
        IF (ITAG(M)>0) THEN
         NZ = NZ +1
C----- exchange INDEX(I) , NZ        
         IF (INDEX(I) > NZ) THEN
          NI = INDEX(I)
          INDEX(I) = NZ
          INDEX(NZ) = NI
         END IF
         CYCLE
        ENDIF
       ENDDO
      ENDDO
      IF (NZ >0 ) THEN
C-----error out if >1 level      
        DO I=1,NRBE3
         IAD = IRBE3(1,I)
         NM = IRBE3(5,I)
         DO J =1,NM
          M = LRBE3(IAD+J)
          IF (ITAG(M)>0) THEN
            II = ITAG(M)
            NMT = IRBE3(5,II)
            DO K =1,NMT
              NFT = IRBE3(1,II)
              M2 = LRBE3(NFT+K)
              IF (ITAG(M2)>0) THEN
               III = ITAG(M2)
               ID=NOM_OPT(1,III)
               CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,III),LTITR)
               CALL ANCMSG(MSGID=1887,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=NOM_OPT(1,I),
     .                     I3=NOM_OPT(1,II))
              END IF
            ENDDO
          ENDIF
         ENDDO
        ENDDO
C----- re-ordering
        IADS = SLRBE3/2
        LCOPY(1:SLRBE3) = LRBE3(1:SLRBE3)       
        ICOPY(1:NRBE3L,1:NRBE3) = IRBE3(1:NRBE3L,1:NRBE3)
        FCOPY(1:SLRBE3*3) = FRBE3(1:SLRBE3*3)
        IAD1 = 0
        DO N=1,NRBE3
          I = INDEX(N)
          IAD = ICOPY(1,I)
          NS = ICOPY(3,I)
          NM = ICOPY(5,I)
          IRBE3(1,N) = IAD1
          DO J =2,NRBE3L
           IRBE3(J,N) = ICOPY(J,I)
          ENDDO
          NOM_OPT(1,N)=IRBE3(2,N)
          DO J =1,NM
           LRBE3(IAD1+J)=LCOPY(IAD+J)
           LRBE3(IADS+IAD1+J)=LCOPY(IADS+IAD+J)
          ENDDO
          DO J =1,6*NM
           FRBE3(6*IAD1+J)=FCOPY(6*IAD+J)
          ENDDO
          IAD1 =IAD1+NM
        ENDDO
      END IF
C
      RETURN
      END SUBROUTINE HIREORBE3
C========================================================================|

