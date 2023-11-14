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
Chd|  R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- called by -----------
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MODIF_TAG                     source/coupling/rad2rad/routines_r2r.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        R2R_COUNT                     source/coupling/rad2rad/r2r_count.F
Chd|        TAGINT_R2R                    source/coupling/rad2rad/tagint_r2r.F
Chd|        TAG_ELEM_VOID_R2R             source/coupling/rad2rad/tagelem_r2r.F
Chd|        TAG_ELEM_VOID_R2R_LIN         source/coupling/rad2rad/tagelem_r2r.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F   
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE R2R_PRELEC(IPARTS,
     2           IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,IPARTSP,COMPT_T2,
     3           MODIF,PASSE,INOM_OPT,NSPCONDN,NSPHION,IPART_L,MEMTR,
     4           PM_STACK ,IWORKSH   ,IGRNOD  ,IGRSURF ,IGRSLIN ,
     5           IGRBRIC  ,IGRQUAD   ,IGRSH4N ,IGRSH3N ,IGRTRUSS,
     6           IGRBEAM  ,IGRSPRING ,NEW_NSLASH_INT,LSUBMODEL,NEW_HM_NINTER,
     7           NEW_NINTSUB,NEW_NINIVOL,IXS10,IXS20,IXS16,
     8           DETONATORS,NSENSOR,SEATBELT_SHELL_TO_SPRING,NB_SEATBELT_SHELLS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE R2R_MOD
        USE NOD2EL_MOD
        USE MESSAGE_MOD
        USE GROUPDEF_MOD
        USE SUBMODEL_MOD
        USE HM_OPTION_READ_MOD
        USE INIVOL_ARRAY_MOD
        USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "lagmult.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ,INTENT(IN) :: NSENSOR
        TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
        INTEGER IPARTS(*),IPARTC(*),IPARTG(*),IPARTT(*),IPARTP(*),
     .     IPARTR(*),COMPT_T2,MODIF,PASSE,INOM_OPT(*),IPARTSP(*),NSPCONDN,
     .     NSPHION,IPART_L(LIPART1,*),MEMTR(*),IWORKSH(*),NEW_NSLASH_INT,NEW_HM_NINTER,NEW_NINTSUB,
     .     NEW_NINIVOL,IXS10(*), IXS16(*), IXS20(*)
        INTEGER ,INTENT(IN) :: NB_SEATBELT_SHELLS
        INTEGER ,INTENT(IN) :: SEATBELT_SHELL_TO_SPRING(NUMELC,2)
        my_real
     .    PM_STACK(*)
C-----------------------------------------------
        TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
        TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
        TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
        TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
        TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
        TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
        TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
        TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
        TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
        TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
        TYPE(DETONATOR_STRUCT_),TARGET,INTENT(IN) :: DETONATORS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,L,NI,GRM,GRS,IGU,MAIN,NUL,NOD,ISK,NRB
        INTEGER GR_ID,COMPT,TAG,IAD,CUR_ID,NB_RBY
        INTEGER NB_KIN,NB_LAG,N1,N2,ID_RBY,DOMA,ISTER
        INTEGER ID_CYL,CCPL,DIFF,D1,D2,G1,G2,GX,IP,JOE,JIE
        INTEGER COMPT_M,COMPT_S,CUR_TYP,SUM,CONT,K,TYPE2
        INTEGER NB_INT,ID_INTER,LNM,LNS,L1,L2,ISENS,VAL,WARN
        INTEGER ID_RLINK,NUL50(50),ID_RBE3,ID_RBE2,ID_JOIN
        INTEGER NU(4),NS(4),JREC,ID_MPC,ISUR,ISURS,ID_MON,FLG
        INTEGER COMPT2,IGR9_TEMP,IGR8_TEMP,IGR2_TEMP,ID,ID_PART,IDS
        INTEGER SPTFL,BID(LNOPT1),IUD,IGRPP_R2R(2,NGRNOD),FLAG_T24
        INTEGER SUB_ID,IDTITL,IDINT,GR_BRIC,NUMC
        INTEGER NTRANS,NNODE_TRANSFORM,NODE_TRANSFORM(6)
        my_real F,NUL_M
        CHARACTER MESS*40,KEY2*ncharkey,TSENS*40
        CHARACTER TITR*nchartitle,KEY*ncharkey,STRING*ncharfield
        INTEGER, DIMENSION(:), ALLOCATABLE :: BUF_TEMP
        DATA MESS/'MULTIDOMAIN PREREADING OF OPTIONS'/
        LOGICAL :: IS_AVAILABLE
C=======================================================================

C--------------------------------------------------------------------C
C---Couting of nodes / elts for groups and surfaces------------------C
C--------------------------------------------------------------------C

        CALL R2R_COUNT(PASSE,IPARTS,
     .                 IPARTC,IPARTG,IGRPP_R2R ,PM_STACK , IWORKSH,
     .                 IGRNOD,IGRSURF,IGRSLIN,IGRBRIC,IXS10,
     .                 IXS20,IXS16)

C--------------------------------------------------------------------C
C------Detection of incompatible options-----------------------------C
C--------------------------------------------------------------------C

        IF (PASSE==0) THEN

C-----DAMPING--------------------------------------------------------

          IF (NDAMP>0) THEN
            CALL ANCMSG(MSGID=837,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1="/DAMP")
            NDAMP = 0
          ENDIF

C-----Flexible BODY---------------------------------------------------

          IF (NFXBODY>0) THEN
            CALL ANCMSG(MSGID=837,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  C1="/FXBODY")
            NFXBODY = 0
          ENDIF

C-----AMS - temporarily incompatible----------------------------------
          IF (ISMS>0) THEN
            CALL ANCMSG(MSGID=835,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1="/AMS")
          ENDIF

C--------------------------------------------------------------------C
C------Prereading and tag of SECTIONS--------------------------------C
C--------------------------------------------------------------------C

          CALL PRELECSEC(
     1    NUL     ,NUL       ,ITABM1  ,1        ,NOM_OPT(LNOPT1*INOM_OPT(8)+1),
     2    IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N  ,IGRTRUSS,
     3    IGRBEAM ,IGRSPRING ,IGRNOD, LSUBMODEL , SEATBELT_SHELL_TO_SPRING,
     4    NB_SEATBELT_SHELLS)

C--------------------------------------------------------------------C
C------Prereading and tag of Accelerometers--------------------------C
C--------------------------------------------------------------------C
          CALL HM_OPTION_START('/ACCEL')
          DO I = 1, NACCELM
            CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID)
            CALL HM_GET_INTV('nodeid', NOD, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('skewid', ISK, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOATV('cutoff', F, IS_AVAILABLE, LSUBMODEL, UNITAB)
            NOD = USR2SYS(NOD, ITABM1, MESS, ID)
            CALL MODIF_TAG(TAGNO(NPART + NOD), 2, MODIF)
          ENDDO
C--------------------------------------------------------------------C
C------Prereading and tag of Transformations-------------------------C
C--------------------------------------------------------------------C
          NTRANS = 0
          CALL HM_OPTION_COUNT('TRANSFORM',NTRANS)
          CALL HM_OPTION_START('TRANSFORM')
          DO I=1,NTRANS
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                           OPTION_ID = ID,
     .                           OPTION_TITR = TITR,
     .                           KEYWORD2 = KEY)
C----
            IF (KEY(1:3)/='MAT') THEN
              CALL HM_GET_INTV('node1',N1,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INTV('node2',N2,IS_AVAILABLE,LSUBMODEL)
              IF (N1/=0) THEN
                N1=USR2SYS(N1,ITABM1,MESS,NI)
                IF (TAGNO(NPART+N1)==-1) CALL MODIF_TAG(TAGNO(NPART+N1),0,MODIF)
              ENDIF
              IF (N2/=0) THEN
                N2=USR2SYS(N2,ITABM1,MESS,NI)
                IF (TAGNO(NPART+N2)==-1) CALL MODIF_TAG(TAGNO(NPART+N2),0,MODIF)
              ENDIF
            ENDIF
C----
          ENDDO
C--------------------------------------------------------------------C
C------Prereading and tag of Sensors---------------------------------C
C--------------------------------------------------------------------C

          CALL HM_OPTION_START('/SENSOR')
          DO I=1,NSENSOR
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .           OPTION_ID   = ISENS, KEYWORD2    = KEY   )
            IF (KEY(1:4) == 'DIST'.OR. KEY(1:5) == 'TYPE2') THEN
              CALL HM_GET_INTV  ('Sensor1' ,N1 ,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INTV  ('Sensor2' ,N2 ,IS_AVAILABLE,LSUBMODEL)
              N1 = USR2SYS(N1,ITABM1,MESS,NI)
              CALL MODIF_TAG(TAGNO(NPART+N1),2,MODIF)
              N2 = USR2SYS(N2,ITABM1,MESS,NI)
              CALL MODIF_TAG(TAGNO(NPART+N2),2,MODIF)
            ELSEIF (KEY(1:5) == 'INTER'.OR.KEY(1:5) == 'TYPE6') THEN
              CALL ANCMSG(MSGID=835, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                  C1="/SENSOR/INTER")
            ELSEIF (KEY(1:4) == 'RWAL'.OR.KEY(1:5) == 'TYPE7') THEN
              CALL ANCMSG(MSGID=835, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                  C1="/SENSOR/RWALL")
            ELSEIF (KEY(1:3) == 'VEL' .OR. KEY(1:5) == 'TYPE9') THEN
c          READ(IIN,REC=IREC,ERR=999,FMT=FMT_I_2F)N1,NUL,NUL
c          N1 = USR2SYS(N1,ITABM1,MESS,NI)
c           CALL MODIF_TAG(TAGNO(NPART+N1),2,MODIF)
            ELSEIF (KEY(1:4) /='SENS'.AND.KEY(1:5)/='TYPE3'
     .          .AND.KEY(1:3)/='AND'.AND.KEY(1:5)/='TYPE4'
     .          .AND.KEY(1:2)/='OR'.AND.KEY(1:5)/='TYPE5'
     .          .AND.KEY(1:3)/='NOT'.AND.KEY(1:5)/='TYPE8'
     .          .AND.KEY(1:4)/='TIME'.AND.KEY(1:5)/='TYPE0'
     .          .AND.KEY(1:4)/='ACCE'.AND.KEY(1:5)/='TYPE1') THEN
              TSENS = '/SENSOR/'//KEY(1:5)
              CALL ANCMSG(MSGID=835, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                 C1=TSENS)
            ENDIF
          END DO
C
C----------------------------------------------------------------------------------C
C------Prereading and tag of elements/nodes for GAUGES-----------------------------C
C----------------------------------------------------------------------------------C

          CALL HM_OPTION_START('/GAUGE')

          ALLOCATE(TAGGAU(NBGAUGE))
          TAGGAU(:)=0

          DO I=1,NBGAUGE
            KEY=''
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                             OPTION_ID   = ID,
     .                             KEYWORD2    = KEY   )

            FLG = 0
            IF (KEY(1:3) == 'SPH') CYCLE

            TAGGAU(I) = ID

            CALL HM_GET_INTV  ('NODE1'    ,NOD ,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV  ('shell_ID' ,IDS ,IS_AVAILABLE,LSUBMODEL)
            IF ((NOD== 0).AND.(IDS /= 0)) THEN
              FLG = 1
              !gauges defined by element-------------------------------------
              DO J=1,NUMELC
                IF(IXC(NIXC*J)==IDS)THEN
                  IDS = J
                  EXIT
                ENDIF
              ENDDO
              IF (TAG_ELC(IDS+NPART) < 1) THEN
                CALL MODIF_TAG(TAG_ELC(IDS+NPART),1,MODIF)
              ENDIF
            ELSEIF ((NOD/=0).AND.(IDS==0)) THEN
              !gauges defined by nodes---------------------------------------
              NOD=USR2SYS(NOD,ITABM1,MESS,ID)
              CALL MODIF_TAG(TAGNO(NPART+NOD),2,MODIF)
            ENDIF
          END DO

C-----Prereading and tag of elements/nodes for GAUGES and /GAUGE/SPH-------
          CALL HM_OPTION_START('/GAUGE')
          DO I=1,NBGAUGE
            KEY=''
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                               OPTION_ID   = ID,
     .                               KEYWORD2    = KEY   )

            FLG = 0
            IF (KEY(1:3) == 'SPH') THEN

              TAGGAU(I) = -ID

              CALL HM_GET_INTV  ('NODE1'    ,NOD ,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INTV  ('shell_ID' ,IDS ,IS_AVAILABLE,LSUBMODEL)
              IF ((NOD== 0).AND.(IDS /= 0)) THEN
                FLG = 1
                !gauges defined by element-------------------------------------
                DO J=1,NUMELC
                  IF(IXC(NIXC*J)==IDS)THEN
                    IDS = J
                    EXIT
                  ENDIF
                ENDDO
                IF (TAG_ELC(IDS+NPART) < 1) THEN
                  CALL MODIF_TAG(TAG_ELC(IDS+NPART),1,MODIF)
                ENDIF
              ELSEIF ((NOD/=0).AND.(IDS==0)) THEN
                !gauges defined by nodes---------------------------------------
                NOD=USR2SYS(NOD,ITABM1,MESS,ID)
                CALL MODIF_TAG(TAGNO(NPART+NOD),2,MODIF)
              ENDIF
            ENDIF
          END DO
C----------------------------------------------------------------------------------C
C------Prereading and tag of MONVOL------------------------------------------------C
C----------------------------------------------------------------------------------C

          NI=0
          ALLOCATE(TAGMON(NMONVOL + NVOLU))
          TAGMON(:)=0

C-------------------------------------------------------------------C
C---------------------------- New Reader ---------------------------C
C-------------------------------------------------------------------C

          CALL HM_OPTION_START('/MONVOL')
          COMPT = 0
C
          DO I=1,NMONVOL
C
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                              OPTION_ID = ID_MON,
     .                              KEYWORD2 = KEY)
C
            IF (KEY(1:7) == 'AIRBAG1') THEN
              CALL HM_GET_INTV('surf', ISUR, IS_AVAILABLE, LSUBMODEL)
            ELSEIF ((KEY(1:4) == 'PRES').OR.(KEY(1:6) == 'AIRBAG')) THEN
              CALL HM_GET_INTV('entityiddisplayed', ISUR, IS_AVAILABLE, LSUBMODEL)
            ENDIF
C
            ISURS = 0
            DO J=1,NSURF
              IF(ISUR==IGRSURF(J)%ID) ISURS=J
            ENDDO
C             ---> error - tag for error in in reading of monvol  <---
            IF (ISURS==0) GOTO 139
C
            IF (IGRSURF(ISURS)%NSEG>0) THEN
              IF (ISURF_R2R(3,ISURS)==IGRSURF(ISURS)%NSEG) THEN
C             ---> case normal <---
                GOTO 139
              ELSEIF (ISURF_R2R(3,ISURS)/=0) THEN
C             ---> case monvol between domains - not allowed <---
                CALL ANCMSG(MSGID=836,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID_MON)
              ENDIF
            ENDIF
C
            GOTO 140
139         TAGMON(I)=ID_MON
            COMPT = COMPT+1
140         CONTINUE
C
          END DO
C
          NEW_HM_NVOLU = COMPT
          NEW_NVOLU = 0

        ENDIF

C--------------------------------------------------------------------C
C------Prereading and tag of Cyl joints -> icompatible with interface-C
C--------------------------------------------------------------------C

        CALL HM_OPTION_START('/CYL_JOINT')
        IF (PASSE==0) ALLOCATE(TAGCYL(NJOINT))
        TAGCYL(:)=0
        COMPT = 0

        DO I=1,NJOINT
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID_CYL)
          CALL HM_GET_INTV('independentnode',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dependentnodes',N2,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dependentnodeset',IGU,IS_AVAILABLE,LSUBMODEL)
C---------------------------------------------------------------------
          GR_ID = 0
          DO J=1,NGRNOD
            IF (IGRNOD(J)%ID==IGU) GR_ID = J
          END DO
          N1=USR2SYS(N1,ITABM1,MESS,ID_CYL)
          N2=USR2SYS(N2,ITABM1,MESS,ID_CYL)
C---------Tag of error situation--------------------------------------
          IF (GR_ID==0) THEN
            IGR2_TEMP = 0
            IGR8_TEMP = 0
            IGR9_TEMP = 0
          ELSE
            IGR2_TEMP = IGRNOD(GR_ID)%NENTITY
            IGR8_TEMP = IGRNOD(GR_ID)%R2R_ALL
            IGR9_TEMP = IGRNOD(GR_ID)%R2R_SHARE
          ENDIF
C--------- n1 et n2 must be taken into account in the interface-------
          IF (TAGNO(N1+NPART)>1) IGR9_TEMP=IGR9_TEMP+1
          IF (TAGNO(N2+NPART)>1) IGR9_TEMP=IGR9_TEMP+1
          IF (TAGNO(N1+NPART)>0) IGR8_TEMP=IGR8_TEMP+1
          IF (TAGNO(N2+NPART)>0) IGR8_TEMP=IGR8_TEMP+1
          IF (TAGNO(N1+NPART)/=0) IGR2_TEMP=IGR2_TEMP+1
          IF (TAGNO(N2+NPART)/=0) IGR2_TEMP=IGR2_TEMP+1
C---------------------------------------------------------------------
          IF (IGR8_TEMP>0) THEN
            DIFF = IGR2_TEMP-IGR8_TEMP
            IF ((IGR9_TEMP>0).OR.(DIFF/=0)) THEN
C--------CYL_JOINTS on the interface----------------------------------
              IF (GR_ID>0) THEN
                DO J=1,IGRNOD(GR_ID)%NENTITY
                  CUR_ID = IGRNOD(GR_ID)%ENTITY(J)
                  IF (TAGNO(CUR_ID+NPART)<3) THEN
                    CALL MODIF_TAG(TAGNO(NPART+CUR_ID),5,MODIF)
                  ENDIF
                END DO
              ENDIF
              IF (TAGNO(N1+NPART)<3) THEN
                CALL MODIF_TAG(TAGNO(NPART+N1),5,MODIF)
              ENDIF
              IF (TAGNO(N2+NPART)<3) THEN
                CALL MODIF_TAG(TAGNO(NPART+N2),5,MODIF)
              ENDIF
            ENDIF
            COMPT = COMPT + 1
C--------Tag of CYL_JOINT to keep it----------------------------------
            TAGCYL(I)=ID_CYL
          ENDIF

350       CONTINUE
        END DO

        NEW_NJOINT = COMPT

C--------------------------------------------------------------------C
C------Prereading and tag of MPC -> icompatible with interface-------C
C--------------------------------------------------------------------C

        CALL HM_OPTION_START('/MPC')
        IF (PASSE==0) ALLOCATE(TAGMPC(NUMMPC))
        TAGMPC(:)=0
        COMPT = 0

        DO I=1,NUMMPC
          COMPT_M = 0
          COMPT_S = 0
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID_MPC,
     .                         OPTION_TITR = TITR)
          CALL HM_GET_INTV('number_of_nodes',NUMC,IS_AVAILABLE,LSUBMODEL)
          DO J=1,NUMC
            CALL HM_GET_INT_ARRAY_INDEX('node_ID',N1,J,IS_AVAILABLE,LSUBMODEL)
            N2 = USR2SYS(N1,ITABM1,MESS,ID_MPC)
            IF (TAGNO(N2+NPART)>=0) COMPT_M=COMPT_M+1
            IF (TAGNO(N2+NPART)>1) COMPT_S=COMPT_S+1
            IF (TAGNO(N2+NPART)<=0) COMPT_S=COMPT_S+1
          END DO
C-----check if MPC must be kept----------------------------------
          IF (COMPT_M>0) THEN
            IF (COMPT_S==0) THEN
C-----MPC intern ->
              TAGMPC(I) = ID_MPC
              COMPT = COMPT + 1
            ELSE
C--------Temporarily MPC is not allowed on interface
              CALL ANCMSG(MSGID=896,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1="/MPC",
     .                    I1=ID_MPC)
            ENDIF
          ENDIF
        END DO

        NEW_NUMMPC = COMPT

C--------------------------------------------------------------------C
C------Prereading and tag of GJOINTS -> icompatible with interface---C
C--------------------------------------------------------------------C
C
        CALL HM_OPTION_START('/GJOINT')
        IF (PASSE==0) ALLOCATE(TAGJOIN(NGJOINT))
        TAGJOIN(:)=0
        COMPT = 0

        DO I=1,NGJOINT
          COMPT_M = 0
          COMPT_S = 0
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID_JOIN,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY2)
C
          CALL HM_GET_INTV('node_ID0',NU(1),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node_ID1',NU(2),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node_ID2',NU(3),IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('node_ID3',NU(4),IS_AVAILABLE,LSUBMODEL)
C
          VAL = 3
          IF(KEY2(1:4)=='DIFF') VAL = 4

          DO J=1,VAL
            NS(J) = USR2SYS(NU(J),ITABM1,MESS,ID_JOIN)
            IF (TAGNO(NS(J)+NPART)>=0) COMPT_M=COMPT_M+1
            IF (TAGNO(NS(J)+NPART)>1) COMPT_S=COMPT_S+1
            IF (TAGNO(NS(J)+NPART)<=0) COMPT_S=COMPT_S+1
          END DO

C-----check if GJOINT must be kept----------------------------------
          IF (COMPT_M>0) THEN
            IF (COMPT_S==0) THEN
C-----GJOINT intern ->
              TAGJOIN(I) = ID_JOIN
              COMPT = COMPT + 1
            ELSE
C--------Temporarily GJOINT is not allowed on interface
              CALL ANCMSG(MSGID=896,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1="/GJOINT",
     .                    I1=ID_JOIN)
            ENDIF
          ENDIF
        END DO

        NEW_NGJOINT = COMPT

C--------------------------------------------------------------------C
C------Prereading and tag of RBE2 -> icompatible with interface------C
C--------------------------------------------------------------------C

        IF (PASSE==0) ALLOCATE(TAGRB2(NRBE2))
        TAGRB2(:)=0
        COMPT = 0

        CALL HM_OPTION_START('/RBE2')
        DO I=1,NRBE2

          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID_RBE2,
     .                         OPTION_TITR = TITR)
C
          CALL HM_GET_INTV('independentnode',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('dependentnodeset',IGU,IS_AVAILABLE,LSUBMODEL)
C
          N2 = USR2SYS(N1,ITABM1,MESS,ID_RBE2)
          GR_ID=0
          DO J=1,NGRNOD
            IF (IGRNOD(J)%ID==IGU) GR_ID = J
          END DO
C---------tag of error situation--------------------------------------
          IF (GR_ID==0) THEN
            COMPT = COMPT + 1
            TAGRB2(I)=ID_RBE2
            GOTO 360
          ENDIF
C---------------------------------------------------------------------
          COMPT_M = IGRNOD(GR_ID)%R2R_ALL
          COMPT_S = IGRNOD(GR_ID)%R2R_SHARE
          IF (TAGNO(N2+NPART)>=0) COMPT_M=COMPT_M+1
          IF (TAGNO(N2+NPART)>1) COMPT_S=COMPT_S+1
          IF (TAGNO(N2+NPART)<=0) COMPT_S=COMPT_S+1
C-----check if RBE2 must be kept--------------------------------------
          IF (COMPT_M>0) THEN
            IF (COMPT_S==0) THEN
C-----RBE2 intern ->
              TAGRB2(I) = ID_RBE2
              COMPT = COMPT + 1
            ELSE
C--------Temporarily RBE2 is not allowed on interface
              CALL ANCMSG(MSGID=896,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1="/RBE2",
     .                    I1=ID_RBE2)
            ENDIF
          ENDIF
360       CONTINUE
        END DO

        NEW_NRBE2 = COMPT

C--------------------------------------------------------------------C
C------Prereading and tag of RBE. -> icompatible with interface------C
C--------------------------------------------------------------------C

        IF (PASSE==0) ALLOCATE(TAGRB3(NRBE3))
        TAGRB3(:)=0
        COMPT = 0
        CALL HM_OPTION_START('/RBE3')

        DO I=1,NRBE3
          COMPT_S = 0
          COMPT_M = 0
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID_RBE3,
     .                         OPTION_TITR = TITR)
          CALL HM_GET_INTV('dependentnode',N1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('nset',VAL,IS_AVAILABLE,LSUBMODEL)

          ALLOCATE(BUF_TEMP(VAL))
          N2 = USR2SYS(N1,ITABM1,MESS,ID_RBE3)
C-----Reading of groups------------------------------------------
          DO L=1,VAL
            CALL HM_GET_FLOAT_ARRAY_INDEX('independentnodesetcoeffs',F,L,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('independentnodesets',IGU,L,IS_AVAILABLE,LSUBMODEL)
            DO J=1,NGRNOD
              IF (IGRNOD(J)%ID==IGU) GR_ID = J
            END DO
            BUF_TEMP(L)=GR_ID
            COMPT_M = COMPT_M + IGRNOD(GR_ID)%R2R_ALL
            COMPT_S = COMPT_S + IGRNOD(GR_ID)%R2R_SHARE
          END DO

          IF (TAGNO(N2+NPART)>=0) COMPT_M=COMPT_M+1
          IF (TAGNO(N2+NPART)>1) COMPT_S=COMPT_S+1
          IF (TAGNO(N2+NPART)<=0) COMPT_S=COMPT_S+1

C-----check if RBE3 must be kept----------------------------------
          IF (COMPT_M>0) THEN
            IF (COMPT_S==0) THEN
C-----RBE3 intern ->
              TAGRB3(I) = ID_RBE3
              COMPT = COMPT + 1
            ELSE
C-----RBE3 between domains -> all nodes on the interface
              IF (TAGNO(NPART+N2)/=-1) THEN
C-----tag of rbe3 that can see secondary nodes
                TAGRB3(I) = ID_RBE3
                COMPT = COMPT +1
              ENDIF
              DO L=1,VAL
                GR_ID = BUF_TEMP(L)
                DO J=1,IGRNOD(GR_ID)%NENTITY
                  CUR_ID = IGRNOD(GR_ID)%ENTITY(J)
                  IF (TAGNO(CUR_ID+NPART)<3) THEN
                    CALL MODIF_TAG(TAGNO(NPART+CUR_ID),4,MODIF)
                  ENDIF
                END DO
              END DO
            ENDIF
          ENDIF
          DEALLOCATE(BUF_TEMP)
        END DO

        NEW_NRBE3 = COMPT

C--------------------------------------------------------------------C
C------Prereading and tag of Rigid links -> icompatible with interface-C
C--------------------------------------------------------------------C

        IF (PASSE==0) ALLOCATE(TAGLNK(NLINK))
        TAGLNK(:)=0
        COMPT = 0
        CALL HM_OPTION_START('/RLINK')

        DO I=1,NLINK
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID      = ID_RLINK,
     .                            OPTION_TITR    = TITR)
          CALL HM_GET_INTV('dependentnodeset'  ,IGU ,IS_AVAILABLE,LSUBMODEL)
C---------------------------------------------------------------------
          GR_ID = 0
          DO J=1,NGRNOD
            IF (IGRNOD(J)%ID==IGU) GR_ID = J
          END DO
C-----check if RBE3 must be kept-----------------------------------
          TAG = 0
          NOD=IGRNOD(GR_ID)%R2R_ALL
          IF (IGRNOD(GR_ID)%R2R_SHARE==0) TAG = 1
C-----tag of rlink and of all nodes--------------------------------
          IF (NOD>0) THEN
            COMPT = COMPT+1
            TAGLNK(I)=ID_RLINK
            IF(TAG/=1) THEN
C-----RLINK between domains -> all nodes on the interface
              DO J=1,IGRNOD(GR_ID)%NENTITY
                CUR_ID = IGRNOD(GR_ID)%ENTITY(J)
                IF (TAGNO(CUR_ID+NPART)<3) THEN
                  CALL MODIF_TAG(TAGNO(NPART+CUR_ID),5,MODIF)
                ENDIF
              END DO
            ENDIF
          ENDIF
        END DO

        NEW_NLINK = COMPT
C
C-------------------------------------------------------------------C
C------Prereading and tag of contact interfaces --------------------C
C----  if contact between domains > pretag of needed elements-------C
C-------------------------------------------------------------------C
C
        COMPT_T2 = 0
        NB_INT = 0
        KCUR = KINTER
C
C-->  for TYPE19 interfaces NSLASH(KCUR) > NINTER because of specific /card for TYPE19
        IF (PASSE==0) ALLOCATE(TAGINT(HM_NINTER+NSLASH(KCUR)),TAGINT_WARN(NINTER+1))
C
C--     -> FOR TAGINT subinterfaces are stored with interfaces in order of the reader
C--     -> TAGINT( --- INTER new reader + SUB interfaces --- INTER old reader -- )
C
        TAGINT(:)=0
        TAGINT_WARN(:)=0
        FLG_TIED(:) = 0
C
C-------------------------------------------------------------------C
C---------------------------- New Reader ---------------------------C
C-------------------------------------------------------------------C
C
        CALL HM_OPTION_START('/INTER')
        NI = 0
        COMPT = 0
C
        DO I=1,HM_NINTER
C
          TAG = 0
          VAL = IDDOM
          TYPE2 = 0
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID_INTER,
     .                            UNIT_ID = NUL,
     .                            SUBMODEL_ID = SUB_ID,
     .                            OPTION_TITR = TITR,
     .                            KEYWORD2 = KEY,
     .                            KEYWORD3 = KEY2)
C
          TAG = 0
          VAL = IDDOM
          TYPE2 = 0
C
          FLG = 0
          IF (KEY(6:6)=='/') FLG = 1
          IF ((LEN_TRIM(KEY))==5) FLG = 1
C
C ---------> case nodes/surfaces---------------------------------------
          IF (((KEY(1:5)=='TYPE2').AND.(FLG==1)).OR.
     .         (KEY(1:5)=='TYPE7').OR.(KEY(1:5)=='TYPE5').OR.(KEY(1:5)=='TYPE8').OR.
     .         (KEY(1:6)=='TYPE10').OR.(KEY(1:6)=='TYPE14') ) THEN
            CONT = 1
            CALL HM_GET_INTV('secondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)
C
C--> type2 interfaces with penality are treated like type7  <------C
            IF ((KEY(1:5)=='TYPE2').AND.(FLG==1)) THEN
              CALL HM_GET_INTV('WFLAG',SPTFL,IS_AVAILABLE,LSUBMODEL)
              IF ((SPTFL/=25).AND.(SPTFL/=26)) THEN
                CONT = 0
                TYPE2 = 1
              ENDIF
            ENDIF

C--> check if the contact is between domains and if it's asymmetric  <------C
            CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .               TYPE2,VAL,TAG,I,COMPT,PASSE,0,IGRPP_R2R,
     .                      IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)

C--> for type 2 contact type5 r2r coupling is used to prevent null mass on nodes
            IF (TYPE2==1) THEN
              DO J=1,IGRNOD(G1)%NENTITY
                CUR_ID = IGRNOD(G1)%ENTITY(J)
                IF (TAGNO(CUR_ID+NPART)==2) FLG_TIED(4) = 1
                IF (TAGNO(CUR_ID+NPART)==4) FLG_TIED(5) = 1
              END DO
            ENDIF
C-->
            IF (TAG>0) THEN
              COMPT_T2 = COMPT_T2 + 1
C--> pretag of elts on main side of contact to keep them with void material <---C
              IF ((TAG==3).OR.(TAG==1).OR.(TAG==4)) THEN
                CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .          IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .          IGRSURF(G2),IGRNOD,G2)
              ENDIF
C--> pretag of elts on second. side of contact to keep them with void material <---C
              IF ((TAG==2).OR.(TAG==1)) THEN
                CALL TAG_ELEM_VOID_R2R(IGRNOD(G1)%NENTITY,IPARTS,
     .          IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,1,EANI,
     .          IGRSURF,IGRNOD,G1)
              ENDIF
            ENDIF
C
C ---------> case of TYPE18 contact interface---------------------------------------
          ELSEIF (KEY(1:6)=='TYPE18') THEN
            CONT = 1
            CALL HM_GET_INTV('secondaryentityids',GR_BRIC,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('tempsecondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)

C--> check if the contact is between domains and if it's asymmetric  <------C
            IF (GRS > 0) THEN
              CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .               TYPE2,VAL,TAG,I,COMPT,PASSE,0,IGRPP_R2R,
     .                      IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
            ELSEIF (GR_BRIC > 0) THEN
              CALL TAGINT_R2R(G1,G2,GR_BRIC,GRM,ID_INTER,
     .               TYPE2,VAL,TAG,I,COMPT,PASSE,3,IGRPP_R2R,
     .                      IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
            ENDIF
C-->
            IF (TAG>0) THEN
              COMPT_T2 = COMPT_T2 + 1
C--> pretag of elts on main side of contact to keep them with void material <---C
              IF ((TAG==3).OR.(TAG==1).OR.(TAG==4)) THEN
                CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .          IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .          IGRSURF(G2),IGRNOD,G2)
              ENDIF
C--> pretag of elts on second. side of contact to keep them with void material <---C
              IF ((TAG==2).OR.(TAG==1)) THEN
                IF (GRS > 0) THEN
                  CALL TAG_ELEM_VOID_R2R(IGRNOD(G1)%NENTITY,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,1,EANI,
     .            IGRSURF,IGRNOD,G1)
                ELSEIF (GR_BRIC > 0) THEN
                  DO J=1,IGRBRIC(G1)%NENTITY
                    CUR_ID = IGRBRIC(G1)%ENTITY(J)
C------------------------>   Tag of solid elements of grbric  <-------------C
                    IF ((TAG_ELS(CUR_ID+NPART)<(1+CONT)).AND.(TAGNO(IPARTS(CUR_ID))/=VAL)) THEN
                      CALL MODIF_TAG(TAG_ELS(CUR_ID+NPART),1+CONT,MODIF)
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
C
          ELSEIF (KEY(1:6)=='TYPE11') THEN
            CONT = 1
            WARN = 0
            CALL HM_GET_INTV('secondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)
C--> check if the contact is between domains and if it's asymmetric  <------C
            CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .               TYPE2,VAL,TAG,I,COMPT,PASSE,2,IGRPP_R2R,
     .                      IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
            IF (TAG>0) THEN
              COMPT_T2 = COMPT_T2 + 1
C--> pretag of elts of line1 to keep them with void material <---C
              IF ((TAG==2).OR.(TAG==1)) THEN
                CALL TAG_ELEM_VOID_R2R_LIN(IGRSLIN(G1)%NSEG,
     .          IPARTS,IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,VAL,CONT,
     .          MODIF,WARN,IGRSLIN(G1))
              ENDIF
C--> pretag of elts of line2 to keep them with void material <---C
              IF ((TAG==3).OR.(TAG==1)) THEN
                CALL TAG_ELEM_VOID_R2R_LIN(IGRSLIN(G2)%NSEG,
     .          IPARTS,IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,VAL,CONT,
     .          MODIF,WARN,IGRSLIN(G2))
              ENDIF
            ENDIF
            IF (WARN==1) THEN
              CALL ANCMSG(MSGID=892,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=ID_INTER)
            ENDIF
          ELSEIF (KEY(1:6)=='TYPE24'.OR.KEY(1:6)=='TYPE21'.OR.KEY(1:5)=='TYPE6'.OR.
     .              KEY(1:6)=='TYPE23'.OR.KEY(1:6)=='TYPE20'.OR.KEY(1:6)=='TYPE15'.OR.
     .              KEY(1:6)=='TYPE25'.OR.((KEY(1:5)=='TYPE3').AND.(FLG==1))) THEN
            CONT = 1
            FLAG_T24 = 0
            IF ((KEY(1:6)=='TYPE24').OR.(KEY(1:6)=='TYPE25')) THEN
              FLAG_T24 = 1
            ELSE
              FLAG_T24 = 0
            ENDIF
            CALL HM_GET_INTV('secondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)
            IF ((FLAG_T24 == 1).AND.(GRM==0)) GRM = GRS
C--> check if the contact is between domains and if it's asymmetric  <------C
            CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .               TYPE2,VAL,TAG,I,COMPT,PASSE,1,IGRPP_R2R,
     .                      IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
            IF (TAG>0) THEN
              COMPT_T2 = COMPT_T2 + 1
C--> pretag of elts of surface 1 to keep them with void material <---C
              IF ((TAG==2).OR.(TAG==1)) THEN
                CALL TAG_ELEM_VOID_R2R(IGRSURF(G1)%NSEG,IPARTS,
     .          IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .          IGRSURF(G1),IGRNOD,G1)
              ENDIF
C--> pretag of elts of surface 2 to keep them with void material <---C
              IF ((TAG==3).OR.(TAG==1)) THEN
                CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .          IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .          IGRSURF(G2),IGRNOD,G2)
              ENDIF
            ENDIF
C
C ---------> other contact interfaces not compatible with multidomains
          ELSEIF (KEY(1:3)/='SUB') THEN
            CALL ANCMSG(MSGID=835,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID_INTER,
     .                C1=LINE(1:13))
          ENDIF

        END DO
C
        NEW_HM_NINTER = COMPT
        NEW_NINTER     = 0
        NEW_NSLASH_INT = 0
C
C-------------------------------------------------------------------C
C------Prereading and tag of ∕INTER/SUB ----------------------------C
C-------------------------------------------------------------------C
C
        CALL HM_OPTION_START('/INTER')
C
        COMPT = 0
        DO I=1,HM_NINTER
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID_INTER,
     .                            UNIT_ID = NUL,
     .                            SUBMODEL_ID = SUB_ID,
     .                            OPTION_TITR = TITR,
     .                            KEYWORD2 = KEY,
     .                            KEYWORD3 = KEY2)
C
          IF (KEY(1:3)=='SUB') THEN
            CALL HM_GET_INTV('InterfaceId',IDINT,IS_AVAILABLE,LSUBMODEL)
C
            DO J=1,HM_NINTER+NSLASH(KCUR)
              IF (TAGINT(J)==IDINT) THEN
                TAGINT(I) = ID_INTER
                COMPT = COMPT + 1
              ENDIF
            END DO

          ENDIF
C
        END DO
C
        NEW_NINTSUB = COMPT
        NEW_HM_NINTER = NEW_HM_NINTER + COMPT
C
C--------------------------------------------------------------------C
C------Prereading and tag of rigid bodies ---------------------------C
C--------------------------------------------------------------------C

        NI=0
        NB_RBY = 0
        NB_KIN = 0
        NB_LAG = 0
        IF (PASSE==0) ALLOCATE(TAGRBY(NRBODY))
        TAGRBY(:)=0
        DOMA = 1

C--------------------------------------------------
C START BROWSING MODEL RBODY
C--------------------------------------------------
        CALL HM_OPTION_START('/RBODY')
        NRB = 0
        DO I=1,NRBODY
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
          KEY=''
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID_RBY,
     .                       KEYWORD2 = KEY,
     .                       OPTION_TITR = TITR)
C---------
          IF(KEY(1:6)=='LAGMUL') CYCLE
C---------
          NRB=NRB+1
C---------
          CALL HM_GET_INTV('node_ID',MAIN,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('sens_ID',ISENS,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('grnd_ID',IGU,IS_AVAILABLE,LSUBMODEL)
C---------
          MAIN=USR2SYS(MAIN,ITABM1,MESS,ID_RBY)
          DO J=1,NGRNOD
            IF (IGRNOD(J)%ID==IGU) GR_ID = J
          END DO
C-----check if RBODY must be kept----------------------------------
          TAG = 0
          COMPT=IGRNOD(GR_ID)%R2R_ALL
          COMPT2=IGRNOD(GR_ID)%R2R_SHARE
C--> check of tag of the main node
          IF (TAGNO(MAIN+NPART)>1) COMPT = COMPT + 1
          IF (TAGNO(MAIN+NPART)>1) COMPT2 = COMPT2 + 1
          IF (COMPT2==0) TAG = 1
C-----tag of the main node-----------------------------------------
          IF (COMPT>0) THEN
            IF(TAG==1) THEN
C-----RGBODY intern ->
              CALL MODIF_TAG(TAGNO(NPART+MAIN),1,MODIF)
            ELSE
C-----RGBODY between domains -> all nodes one the r2r interface
              IF (TAGNO(MAIN+NPART)<3) THEN
                CALL MODIF_TAG(TAGNO(NPART+MAIN),3,MODIF)
                IF (ISENS/=0) THEN
                  CALL ANCMSG(MSGID=976,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        C1="FOR RBODY ID=",
     .                        I1=ID_RBY,
     .                        C2="- RBODY WITH SENSOR")
                ENDIF
              ENDIF
            ENDIF
C-----tag of the RBODY to keep it
            TAGRBY(I)=ID_RBY
            NB_RBY = NB_RBY + 1
            NB_KIN = NB_KIN + 1
          ENDIF
        END DO
        NEW_NRBYKIN=NB_KIN

C--------------------------------------------------
C START BROWSING MODEL /RBODY/LAGMUL
C--------------------------------------------------
        CALL HM_OPTION_START('/RBODY')
        NRB = 0
        DO I=1,NRBODY
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
          KEY=''
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                        OPTION_ID = ID_RBY,
     .                        KEYWORD2 = KEY,
     .                        OPTION_TITR = TITR)
C---------
          IF(KEY(1:6)=='LAGMUL') THEN
            NRB=NRB+1
C--------Temporarily RBY/LAGMUL are not allowed
            CALL ANCMSG(MSGID=835,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1=LINE(1:L+9))
C-----------------------------------------------------------------
            CALL HM_GET_INTV('node_ID',MAIN,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('grnd_ID',IGU,IS_AVAILABLE,LSUBMODEL)
C--------Temporarily RBY/LAGMUL are not allowed
            MAIN=USR2SYS(MAIN,ITABM1,MESS,ID_RBY)
            DO J=1,NGRNOD
              IF (IGRNOD(J)%ID==IGU) GR_ID = J
            END DO

C-----check if RBODY kas to be kept--------------------------------

            TAG = 0
            COMPT=IGRNOD(GR_ID)%R2R_ALL
            COMPT2=IGRNOD(GR_ID)%R2R_SHARE
C--> check of tag of the main node
            IF (TAGNO(MAIN+NPART)>1) COMPT = COMPT + 1
            IF (TAGNO(MAIN+NPART)>1) COMPT2 = COMPT2 + 1
            IF (COMPT2==0) TAG = 1

C-----Tag of the main node-----------------------------------------

            IF (COMPT>0) THEN
              IF(TAG==1) THEN
C-----RGBODY intern ->
                CALL MODIF_TAG(TAGNO(NPART+MAIN),1,MODIF)
              ELSE
C-----RGBODY between domains -> all nodes one the r2r interface
                IF (TAGNO(MAIN+NPART)<3) THEN
                  CALL MODIF_TAG(TAGNO(NPART+MAIN),3,MODIF)
                ENDIF
              ENDIF
C-----tag of the RBODY to keep it-----------------------------------
              TAGRBY(I)=ID_RBY
              NB_RBY = NB_RBY + 1
              NB_LAG = NB_LAG + 1
            ENDIF  ! IF (COMPT>0) THEN
          END IF ! IF(KEY(1:6)=='LAGMUL') THEN
        END DO

        NEW_NRBY     = NB_RBY
        NRBYLAG      = NB_LAG

C--------------------------------------------------------------------C
C------Prereading and tag of /SPHBCS --------------------------------C
C--------------------------------------------------------------------C

        NSPCONDN = 0
        CALL HM_OPTION_START('/SPHBCS')
        IF (PASSE==0) ALLOCATE(TAGSPHBCS(NSPCOND))
        TAGSPHBCS(:) = 0
        DO I=1,NSPCOND
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID,
     .                         OPTION_TITR = TITR,
     .                         KEYWORD2 = KEY)
          CALL HM_GET_INTV('entityid',IGU,IS_AVAILABLE,LSUBMODEL)
C---------------------------------------------------------------------
          GR_ID = 0
          DO J=1,NGRNOD
            IF (IGRNOD(J)%ID==IGU) GR_ID = J
          END DO
C---------------------------------------------------------------------
          COMPT = 0
          DO L=1,IGRNOD(GR_ID)%NENTITY
            CUR_ID = NOD2SP(IGRNOD(GR_ID)%ENTITY(L))
            IF (TAGNO(IPARTSP(CUR_ID))/=0) THEN
              COMPT = COMPT + 1
            ENDIF
          ENDDO
C---------------------------------------------------------------------
          IF (COMPT>0) THEN
            NSPCONDN = NSPCONDN + 1
            TAGSPHBCS(I) = ID
          ENDIF
        END DO

C--------------------------------------------------------------------C
C------Prereading and tag of inlets/outlets for SPH -----------------C
C--------------------------------------------------------------------C


        NSPHION = 0
        IF (PASSE == 0) ALLOCATE(TAGSPHIO(NSPHIO))
        TAGSPHIO(:) = 0
        CALL HM_OPTION_START('/SPH/INOUT')
        DO I = 1,NSPHIO
          ! Title and ID
          TITR = ''
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = ID,
     .                            OPTION_TITR = TITR)
          CALL HM_GET_INTV('pid'     ,ID_PART  ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('SURF_ID' ,ISUR     ,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------------------------
          DO J=1,NPART
            IF (IPART_L(4,J) == ID_PART) IDS = J
          ENDDO
          DO J=1,NSURF
            IF (IGRSURF(J)%ID == ISUR)   G2  = J
          END DO
C--------------------------------------------------------------------
          IF (TAGNO(IDS) /= 0) THEN
            CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .          IPARTC,IPARTG,IPARTSP,1,0,MODIF,MEMTR,-2,0,EANI,
     .          IGRSURF(G2),IGRNOD,G2)
            NSPHION     = NSPHION + 1
            TAGSPHIO(I) = ID
          ENDIF
        END DO

C--------------------------------------------------------------------C
C------Prereading of ALE/LINK----------------------------------------C
C--------------------------------------------------------------------C

        CALL HM_OPTION_COUNT('/ALE/LINK/VEL', NALELK)
        IF (NALELK > 0) THEN
          CALL HM_OPTION_START('/ALE/LINK/VEL')
          DO I = 1, NALELK
            CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)
            CALL HM_GET_INTV('node_ID1', N1, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('node_ID2', N2, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('grnod_ID', GR_ID, IS_AVAILABLE, LSUBMODEL)
            DO J = 1, NGRNOD
              IF (IGRNOD(J)%ID == GR_ID) THEN
                GR_ID = J
                EXIT
              ENDIF
            ENDDO
            N1 = USR2SYS(N1, ITABM1, MESS, ID)
            N2 = USR2SYS(N2, ITABM1, MESS, ID)
            IF (N1 > 0) THEN
              IF ((IGRNOD(GR_ID)%R2R_ALL > 0) .AND. (TAGNO(NPART+N1) < 1)) THEN
                CALL MODIF_TAG(TAGNO(NPART + N1), 2, MODIF)
              ELSEIF ((IGRNOD(GR_ID)%R2R_SHARE > 0) .AND. (TAGNO(NPART+N1) == 1)) THEN
                CALL MODIF_TAG(TAGNO(NPART+N1), 2, MODIF)
              ELSEIF (TAGNO(NPART+N1) == 0) THEN
                CALL MODIF_TAG(TAGNO(NPART+N1), 2, MODIF)
              ELSEIF (TAGNO(NPART+N1) == -1) THEN
                CALL MODIF_TAG(TAGNO(NPART+N1), 0, MODIF)
              ENDIF
            ENDIF
            IF (N2 > 0) THEN
              IF ((IGRNOD(GR_ID)%R2R_ALL > 0) .AND. (TAGNO(NPART+N2) < 1)) THEN
                CALL MODIF_TAG(TAGNO(NPART+N2), 2, MODIF)
              ELSEIF ((IGRNOD(GR_ID)%R2R_SHARE > 0) .AND. (TAGNO(NPART+N2) == 1)) THEN
                CALL MODIF_TAG(TAGNO(NPART+N2), 2, MODIF)
              ELSEIF (TAGNO(NPART+N2) == 0) THEN
                CALL MODIF_TAG(TAGNO(NPART+N2), 2, MODIF)
              ELSEIF (TAGNO(NPART+N2) == -1) THEN
                CALL MODIF_TAG(TAGNO(NPART+N2), 0, MODIF)
              ENDIF
            ENDIF
          ENDDO
        ENDIF

C--------------------------------------------------------------------C
C------Prereading and tag of RWALL-----------------------------------C
C--------------------------------------------------------------------C

        CALL HM_OPTION_START('/RWALL')
C
        DO I=1,NRWALL
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID_MON,
     .                            KEYWORD2 = KEY)
C
          IF (KEY(1:7) == 'LAGMUL') THEN
C--------RWALL /LAGMUL not allowed with multidomains
            CALL ANCMSG(MSGID=835,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1=LINE(1:L+6))
          ELSE
            CALL HM_GET_INTV('Node1',NOD,IS_AVAILABLE,LSUBMODEL)
            IF (NOD>0) THEN
              NOD=USR2SYS(NOD,ITABM1,MESS,ID)
              CALL MODIF_TAG(TAGNO(NPART+NOD),4,MODIF)
            ENDIF
          ENDIF
        ENDDO

C--------------------------------------------------------------------C
C------Prereading and tag of INIVOL----------------------------------C
C--------------------------------------------------------------------C

        NEW_NINIVOL = 0
        IF (PASSE==0) ALLOCATE(TAG_INIVOL(NINIVOL))
        TAG_INIVOL(:) = 0
        CALL HM_OPTION_START('/INIVOL')

        DO I=1,NINIVOL
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('secondarycomponentlist', ID_PART, IS_AVAILABLE, LSUBMODEL)
C
          DO J=1,NPART
            IF(IPART_L(4,J)==ID_PART) IDS=J
          ENDDO
C
          IF (TAGNO(IDS) > 0) THEN
            TAG_INIVOL(I) = ID
            NEW_NINIVOL = NEW_NINIVOL + 1
          ENDIF
        ENDDO

C--------------------------------------------------------------------C
C------Prereading of /TRANSFORM--------------------------------------C
C--------------------------------------------------------------------C
C
        CALL HM_OPTION_COUNT('TRANSFORM',NTRANS)
        CALL HM_OPTION_START('TRANSFORM')
C
        DO I=1,NTRANS
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID,
     .                            KEYWORD2 = KEY)
C
          NNODE_TRANSFORM = 0
          NODE_TRANSFORM(1:6) = 0
C
          IF ((KEY(1:3)=='TRA').OR.(KEY(1:3)=='ROT').OR.(KEY(1:3)=='SYM')) THEN
            NNODE_TRANSFORM = 2
            CALL HM_GET_INTV('node1',NODE_TRANSFORM(1),IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('node2',NODE_TRANSFORM(2),IS_AVAILABLE,LSUBMODEL)
          ELSEIF (KEY(1:3)=='SCA') THEN
            NNODE_TRANSFORM = 1
            CALL HM_GET_INTV('node1',NODE_TRANSFORM(1),IS_AVAILABLE,LSUBMODEL)
          ELSEIF (KEY(1:3)=='POS') THEN
            NNODE_TRANSFORM = 6
            CALL HM_GET_INTV('node1',NODE_TRANSFORM(1),IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('node2',NODE_TRANSFORM(2),IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('node3',NODE_TRANSFORM(3),IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('node4',NODE_TRANSFORM(4),IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('node5',NODE_TRANSFORM(5),IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INTV('node6',NODE_TRANSFORM(6),IS_AVAILABLE,LSUBMODEL)
          ENDIF
C
          DO J=1,NNODE_TRANSFORM
            IF (NODE_TRANSFORM(J) > 0) THEN
              NOD=USR2SYS(NODE_TRANSFORM(J),ITABM1,MESS,ID)
              IF (TAGNO(NPART+NOD) == -1) CALL MODIF_TAG(TAGNO(NPART+NOD),0,MODIF)
            ENDIF
          ENDDO
C
        ENDDO
C
C--------------------------------------------------------------------C
C------Prereading of detonation points-------------------------------C
C--------------------------------------------------------------------C
C
        CALL HM_OPTION_START('/DFS')
C
        DO I=1,DETONATORS%NDET+NLASER
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = ID,
     .                            KEYWORD2 = KEY)
C
          IF (KEY(1:7)=='DETPOIN') THEN
            CALL HM_GET_INTV('rad_det_node1', NOD, IS_AVAILABLE, LSUBMODEL)
            IF (NOD>0) THEN
              NOD=USR2SYS(NOD,ITABM1,MESS,ID)
              IF (TAGNO(NPART+NOD) == -1) CALL MODIF_TAG(TAGNO(NPART+NOD),0,MODIF)
            ENDIF
          ENDIF
C
        ENDDO

C------------------------------------------------------------------

        RETURN

C------------------------------------------------------------------
 999    CALL FREERR(3)
        RETURN

      END SUBROUTINE R2R_PRELEC

Chd|====================================================================
Chd|  R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- called by -----------
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE R2R_MONVOL(TAGPART,TAGPRO,IGRSURF,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE R2R_MOD
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
#include      "com04_c.inc"
#include      "r2r_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER TAGPART(*),TAGPRO(*)
        TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
        TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,NUL,COMPT,TYPE,J,ISUR,ISURS,FLAG,K,CCPL
        INTEGER CPT_NOD,CPT_SEG,CPT_CPL,CUR_ID,IAD,CPT_SEG2
        INTEGER MAT_ID,NJET,IJET,N1,N2,N3,WARN
        CHARACTER MESS*40
        CHARACTER TITR*nchartitle,KEY*ncharkey
        DATA MESS/'INJECTORS '/
        LOGICAL :: IS_AVAILABLE
C=======================================================================

C--------------------------------------------------------------------C
C------Prereading and tag of injectors (MONVOL)----------------------C
C--------------------------------------------------------------------C

        CALL HM_OPTION_START('/MONVOL')
C
        DO I=1,NMONVOL
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            KEYWORD2 = KEY)
C
          IF(KEY(1:7)=='AIRBAG1')THEN
            TYPE = 1
            CALL HM_GET_INTV('surf', ISUR, IS_AVAILABLE, LSUBMODEL)
          ELSEIF(KEY(1:6)=='AIRBAG')THEN
            TYPE = 2
            CALL HM_GET_INTV('entityiddisplayed', ISUR, IS_AVAILABLE, LSUBMODEL)
          ENDIF

C----------------------------------------------------------
          DO J=1,NSURF
            IF (ISUR==IGRSURF(J)%ID) ISURS=J
          ENDDO
          CPT_SEG=0
          CCPL=0
          FLAG=0
          DO J=1,IGRSURF(ISURS)%NSEG
            CPT_NOD=0
            CPT_CPL=0
            DO K=1,4
              CUR_ID = IGRSURF(ISURS)%NODES(J,K)
C             --> count of taged nodes <--
              IF (TAGNO(CUR_ID+NPART)>=0) THEN
                CPT_NOD=CPT_NOD+1
                IF (TAGNO(CUR_ID+NPART)==2) CPT_CPL=CPT_CPL+1
              ENDIF
            END DO
C             --> if all the nodes of the segment are in the subdomain -it's validated <--
            IF (CPT_CPL/=0) WARN = 1
            IF (CPT_NOD==4) CPT_SEG=CPT_SEG+1
          END DO
C--------MONVOL not allowed on multidomains interface--------------------------
C    IF (WARN>0) THEN
C          CALL ANSTCKC(LEN_TRIM(LINE),LINE)
C                CALL ANCWARN(843,ANINFO_BLIND_1)
C    ENDIF
C-------Case AIRBAG1----------------------------------------
          IF ((TYPE==1).AND.(TAGMON(I)>0)) THEN
            CALL HM_GET_INTV('nb_jet', NJET, IS_AVAILABLE, LSUBMODEL)
C             --> loop on injectors <--
            DO J=1,NJET
              CALL HM_GET_INT_ARRAY_INDEX('ijet', IJET, J, IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('node1', N1, J, IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('node2', N2, J, IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('node3', N3, J, IS_AVAILABLE, LSUBMODEL)
              IF (IJET==1) THEN
                IF (N1/=0) THEN
                  N1=USR2SYS(N1,ITABM1,MESS,NUL)
                  IF(TAGNO(N1+NPART)<2) TAGNO(N1+NPART) = 1
                ENDIF
                IF (N2/=0) THEN
                  N2=USR2SYS(N2,ITABM1,MESS,NUL)
                  IF(TAGNO(N2+NPART)<2) TAGNO(N2+NPART) = 1
                ENDIF
                IF (N3/=0) THEN
                  N3=USR2SYS(N3,ITABM1,MESS,NUL)
                  IF(TAGNO(N3+NPART)<2) TAGNO(N3+NPART) = 1
                ENDIF
              ENDIF
            END DO
          ENDIF
C-------Case AIRBAG----------------------------------------
          IF ((TYPE==2).AND.(TAGMON(I)>0)) THEN
            CALL HM_GET_INTV('ABG_Njet', NJET, IS_AVAILABLE, LSUBMODEL)
C             --> loop on injectors <--
            DO J=1,NJET
              CALL HM_GET_INT_ARRAY_INDEX('ABG_Ijet', IJET, J,IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('ABG_N1', N1, J,IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('ABG_N2', N2, J,IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('ABG_N3', N3, J,IS_AVAILABLE, LSUBMODEL)
C
              IF (IJET==1) THEN
                IF (N1/=0) THEN
                  N1=USR2SYS(N1,ITABM1,MESS,NUL)
                  IF(TAGNO(N1+NPART)<2) TAGNO(N1+NPART) = 1
                ENDIF
                IF (N2/=0) THEN
                  N2=USR2SYS(N2,ITABM1,MESS,NUL)
                  IF(TAGNO(N2+NPART)<2) TAGNO(N2+NPART) = 1
                ENDIF
                IF (N3/=0) THEN
                  N3=USR2SYS(N3,ITABM1,MESS,NUL)
                  IF(TAGNO(N3+NPART)<2) TAGNO(N3+NPART) = 1
                ENDIF
              ENDIF
            END DO
          ENDIF
C---------------------------------------------------------
        END DO
C
        NMONVOL = NEW_HM_NVOLU
        NVOLU = NEW_NVOLU

C-----------
        RETURN

C------------------------------------------------------------------
 999    CALL FREERR(3)
        RETURN

      END SUBROUTINE R2R_MONVOL
