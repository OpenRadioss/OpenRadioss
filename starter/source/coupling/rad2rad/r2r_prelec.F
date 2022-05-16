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
Chd|  R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- called by -----------
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREDEC_2KEY_2ID               source/starter/freform.F      
Chd|        FREDEC_2KEY_3ID_T             source/starter/freform.F      
Chd|        FREDEC_2KEY_4ID               source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MODIF_TAG                     source/coupling/rad2rad/routines_r2r.F
Chd|        NEXTSLA                       source/starter/freform.F      
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        R2R_COUNT                     source/coupling/rad2rad/r2r_count.F
Chd|        TAGINT_R2R                    source/coupling/rad2rad/tagint_r2r.F
Chd|        TAG_ELEM_VOID_R2R             source/coupling/rad2rad/tagelem_r2r.F
Chd|        TAG_ELEM_VOID_R2R_LIN         source/coupling/rad2rad/tagelem_r2r.F
Chd|        USR2SYS                       source/system/sysfus.F        
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
     7           NEW_NINTSUB,NEW_NINIVOL,IXS10,IXS20,IXS16)
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
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
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      INTEGER IPARTS(*),IPARTC(*),IPARTG(*),IPARTT(*),IPARTP(*),
     .   IPARTR(*),COMPT_T2,MODIF,PASSE,INOM_OPT(*),IPARTSP(*),NSPCONDN,
     .   NSPHION,IPART_L(LIPART1,*),MEMTR(*),IWORKSH(*),NEW_NSLASH_INT,NEW_HM_NINTER,NEW_NINTSUB,
     .   NEW_NINIVOL,IXS10(*), IXS16(*), IXS20(*)
      my_real
     .  PM_STACK(*)
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
      my_real F,NUL_M
      CHARACTER MESS*40,KEY2*ncharkey,TSENS*40
      CHARACTER TITR*nchartitle,KEY*ncharkey,STRING*ncharfield      
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUF_TEMP                 
      DATA MESS/'MULTIDOMAIN PREREADING OF OPTIONS'/
      LOGICAL :: IS_AVAILABLE
C=======================================================================	  	 
	
C--------------------------------------------------------------------C
C---Precomptage des noeuds/elts  garder dans les groupes/surfaces---C
C--------------------------------------------------------------------C
	
        CALL R2R_COUNT(PASSE,IPARTS,
     .                 IPARTC,IPARTG,IGRPP_R2R ,PM_STACK , IWORKSH,
     .                 IGRNOD,IGRSURF,IGRSLIN,IGRBRIC,IXS10,
     .                 IXS20,IXS16)
		      
C--------------------------------------------------------------------C
C------Detection d'options incompatibles-----------------------------C
C--------------------------------------------------------------------C	

        IF (PASSE.EQ.0) THEN
	
C-----DAMPING--------------------------------------------------------

        IF (NDAMP.GT.0) THEN
           CALL ANCMSG(MSGID=837,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 C1="/DAMP")
	   NDAMP = 0
	ENDIF
	
C-----Flexible BODY---------------------------------------------------

        IF (NFXBODY.GT.0) THEN
           CALL ANCMSG(MSGID=837,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO,
     .                 C1="/FXBODY")
	   NFXBODY = 0
	ENDIF
	
C-----AMS - provisoirement incompatible-------------------------------

        IF (ISMS.GT.0) THEN
           CALL ANCMSG(MSGID=835,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1="/AMS")
	ENDIF
	
C--------------------------------------------------------------------C
C------Prlecture et tag des SECTIONS---------------------------------C
C--------------------------------------------------------------------C

        CALL PRELECSEC(
     1  NUL     ,NUL       ,ITABM1  ,1        ,NOM_OPT(LNOPT1*INOM_OPT(8)+1),
     2  IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N  ,IGRTRUSS,
     3  IGRBEAM ,IGRSPRING ,IGRNOD, LSUBMODEL  )
				
C--------------------------------------------------------------------C
C------Prlecture et tag des Accelerometres--------------------------C
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
C------Prlecture et tag des Transformations--------------------------C
C--------------------------------------------------------------------C     	          
        KCUR = KTRANSF   
        IREC=KOPTAD(KCUR) 
        DO I=1,NSLASH(KCUR)	  	  			
          CALL NEXTSLA()
          IREC=IREC+1
          CALL FREDEC_2KEY_4ID(KEY,ID,NUL,NUL,NUL)
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')TITR                               
          IREC=IREC+1
C----
          IF (KEY(1:3).NE.'MAT') THEN
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            READ(LINE,ERR=999,FMT=FMT_I_3F_2I) NUL,NUL_M,NUL_M,NUL_M,N1,N2
            IF (N1.NE.0) THEN
              N1=USR2SYS(N1,ITABM1,MESS,NI)
	      IF (TAGNO(NPART+N1).EQ.-1) CALL MODIF_TAG(TAGNO(NPART+N1),0,MODIF)
            ENDIF
            IF (N2.NE.0) THEN
              N2=USR2SYS(N2,ITABM1,MESS,NI)
	      IF (TAGNO(NPART+N2).EQ.-1) CALL MODIF_TAG(TAGNO(NPART+N2),0,MODIF)
            ENDIF
          ENDIF
C----
        ENDDO
	
C--------------------------------------------------------------------C
C------Prlecture et tag des Sensors---------------------------------C
C--------------------------------------------------------------------C
      	          
      CALL HM_OPTION_START('/SENSOR')
      DO I=1,NSENSOR	  	  			
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .       OPTION_ID   = ISENS, KEYWORD2    = KEY   )
        IF (KEY(1:4) == 'DIST'.OR. KEY(1:5) == 'TYPE2') THEN
          CALL HM_GET_INTV  ('Sensor1' ,N1 ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV  ('Sensor2' ,N2 ,IS_AVAILABLE,LSUBMODEL)
          N1 = USR2SYS(N1,ITABM1,MESS,NI)
	         CALL MODIF_TAG(TAGNO(NPART+N1),2,MODIF)
          N2 = USR2SYS(N2,ITABM1,MESS,NI)
	         CALL MODIF_TAG(TAGNO(NPART+N2),2,MODIF)
        ELSEIF (KEY(1:5) == 'INTER'.OR.KEY(1:5) == 'TYPE6') THEN
          CALL ANCMSG(MSGID=835, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .              C1="/SENSOR/INTER")
        ELSEIF (KEY(1:4) == 'RWAL'.OR.KEY(1:5) == 'TYPE7') THEN
          CALL ANCMSG(MSGID=835, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .              C1="/SENSOR/RWALL")
        ELSEIF (KEY(1:3) == 'VEL' .OR. KEY(1:5) == 'TYPE9') THEN
c          READ(IIN,REC=IREC,ERR=999,FMT=FMT_I_2F)N1,NUL,NUL    
c          N1 = USR2SYS(N1,ITABM1,MESS,NI)
c	         CALL MODIF_TAG(TAGNO(NPART+N1),2,MODIF)	    
        ELSEIF (KEY(1:4) .NE.'SENS'.AND.KEY(1:5).NE.'TYPE3'
     .      .AND.KEY(1:3).NE.'AND'.AND.KEY(1:5).NE.'TYPE4'
     .      .AND.KEY(1:2).NE.'OR'.AND.KEY(1:5).NE.'TYPE5'
     .      .AND.KEY(1:3).NE.'NOT'.AND.KEY(1:5).NE.'TYPE8'
     .      .AND.KEY(1:4).NE.'TIME'.AND.KEY(1:5).NE.'TYPE0'
     .      .AND.KEY(1:4).NE.'ACCE'.AND.KEY(1:5).NE.'TYPE1') THEN
           TSENS = '/SENSOR/'//KEY(1:5)
           CALL ANCMSG(MSGID=835, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .              C1=TSENS)
	       ENDIF	  
      END DO
C----------------------------------------------------------------------------------C
C------Prlecture et tag des elements/noeuds pour GAUGES----------------------------C
C----------------------------------------------------------------------------------C

      CALL HM_OPTION_START('/GAUGE')       

	 ALLOCATE(TAGGAU(NBGAUGE))
	 TAGGAU(:)=0

      DO I=1,NBGAUGE
          KEY=''
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                           OPTION_ID   = ID, 
     .                           KEYWORD2    = KEY   )

          FLG = 0
          IF (KEY(1:3) == 'SPH') CYCLE 

          TAGGAU(I) = ID

          CALL HM_GET_INTV  ('NODE1'    ,NOD ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV  ('shell_ID' ,IDS ,IS_AVAILABLE,LSUBMODEL)
          IF ((NOD== 0).AND.(IDS /= 0)) THEN
            FLG = 1
            !cas des gauges par element------------------------------------   
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
            !cas des gauges par noeuds-------------------------------------
            NOD=USR2SYS(NOD,ITABM1,MESS,ID)
	       CALL MODIF_TAG(TAGNO(NPART+NOD),2,MODIF)
          ENDIF
      END DO
        
C-----Prlecture et tag des elements/noeuds pour GAUGES /GAUGE/SPH----------
      CALL HM_OPTION_START('/GAUGE')       
      DO I=1,NBGAUGE
        KEY=''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                           OPTION_ID   = ID, 
     .                           KEYWORD2    = KEY   )

        FLG = 0
        IF (KEY(1:3) == 'SPH') THEN 

          TAGGAU(I) = -ID

          CALL HM_GET_INTV  ('NODE1'    ,NOD ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV  ('shell_ID' ,IDS ,IS_AVAILABLE,LSUBMODEL)
          IF ((NOD== 0).AND.(IDS /= 0)) THEN
            FLG = 1
            !cas des gauges par element------------------------------------   
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
            !cas des gauges par noeuds-------------------------------------
            NOD=USR2SYS(NOD,ITABM1,MESS,ID)
	       CALL MODIF_TAG(TAGNO(NPART+NOD),2,MODIF)
          ENDIF
         ENDIF
        END DO
C----------------------------------------------------------------------------------C
C------Prlecture et tag des MONVOL-------------------------------------------------C
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
     .                            OPTION_ID = ID_MON,
     .                            KEYWORD2 = KEY)
C
          IF (KEY(1:7) == 'AIRBAG1') THEN
            CALL HM_GET_INTV('surf', ISUR, IS_AVAILABLE, LSUBMODEL)
          ELSEIF ((KEY(1:4) == 'PRES').OR.(KEY(1:6) == 'AIRBAG')) THEN
            CALL HM_GET_INTV('entityiddisplayed', ISUR, IS_AVAILABLE, LSUBMODEL)
          ENDIF
C
	  ISURS = 0
          DO J=1,NSURF
               IF(ISUR.EQ.IGRSURF(J)%ID) ISURS=J
          ENDDO
C             ---> cas d'erreur tag pour traiter dans lecture des monvol <---
	  IF (ISURS.EQ.0) GOTO 139
C	    
	  IF (IGRSURF(ISURS)%NSEG.GT.0) THEN
	      IF (ISURF_R2R(3,ISURS).EQ.IGRSURF(ISURS)%NSEG) THEN
C             ---> cas normal  tager <---
	         GOTO 139	  
	      ELSEIF (ISURF_R2R(3,ISURS).NE.0) THEN
C             ---> cas monvol coupe interdit <---	      
              CALL ANCMSG(MSGID=836,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID_MON)
	      ENDIF	      
	  ENDIF
C	  
	  GOTO 140
139	  TAGMON(I)=ID_MON
	  COMPT = COMPT+1
140	  CONTINUE
C
        END DO
C
	NEW_HM_NVOLU = COMPT
C       
C-------------------------------------------------------------------C
C---------------------------- Old Reader ---------------------------C
C-------------------------------------------------------------------C

	COMPT = 0
        KCUR = KMONVO
        IREC=KOPTAD(KCUR)-1

        DO I=1,NVOLU
          CALL NEXTSLA
          READ(IIN,REC=IREC,ERR=999,FMT='(A)') KLINE
          CALL FREDEC_2KEY_2ID(KEY,ID_MON,NUL)
          IREC=IREC+2
          READ (IIN,REC=IREC,ERR=999,FMT=FMT_I)ISUR
          IREC=IREC+1
	  ISURS = 0
          DO J=1,NSURF
               IF(ISUR.EQ.IGRSURF(J)%ID) ISURS=J
          ENDDO

C             ---> cas d'erreur tag pour traiter dans lecture des monvol <---
	  IF (ISURS.EQ.0) GOTO 159
C	    
	  IF (IGRSURF(ISURS)%NSEG.GT.0) THEN
	      IF (ISURF_R2R(3,ISURS).EQ.IGRSURF(ISURS)%NSEG) THEN
C             ---> cas normal  tager <---
	         GOTO 159	  
	      ELSEIF (ISURF_R2R(3,ISURS).NE.0) THEN
C             ---> cas monvol coupe interdit <---	      
              CALL ANCMSG(MSGID=836,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID_MON)
	      ENDIF	      
	  ENDIF
C	  
	  GOTO 160
159	  TAGMON(I)=ID_MON
	  COMPT = COMPT+1
160	  CONTINUE
	END DO

	NEW_NVOLU = COMPT
		
        ENDIF
		  	  	  		  		
C--------------------------------------------------------------------C
C------Prlecture et tag des Cyl joints -> icompatible sur interface-C
C--------------------------------------------------------------------C

        CALL HM_OPTION_START('/CYL_JOINT')
	IF (PASSE.EQ.0) ALLOCATE(TAGCYL(NJOINT))
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
	    IF (IGRNOD(J)%ID.EQ.IGU) GR_ID = J
	  END DO
	  N1=USR2SYS(N1,ITABM1,MESS,ID_CYL)
	  N2=USR2SYS(N2,ITABM1,MESS,ID_CYL)
C---------on tag les cas d'erreur-------------------------------------		  
	  IF (GR_ID.EQ.0) THEN
            IGR2_TEMP = 0          
            IGR8_TEMP = 0
            IGR9_TEMP = 0           	     	  
          ELSE
            IGR2_TEMP = IGRNOD(GR_ID)%NENTITY
            IGR8_TEMP = IGRNOD(GR_ID)%R2R_ALL
            IGR9_TEMP = IGRNOD(GR_ID)%R2R_SHARE
	  ENDIF 	  
C--------- n1 et n2 doivent    tre pris en compte dans l'interface -----	  
	  IF (TAGNO(N1+NPART).GT.1) IGR9_TEMP=IGR9_TEMP+1
          IF (TAGNO(N2+NPART).GT.1) IGR9_TEMP=IGR9_TEMP+1
	  IF (TAGNO(N1+NPART).GT.0) IGR8_TEMP=IGR8_TEMP+1
          IF (TAGNO(N2+NPART).GT.0) IGR8_TEMP=IGR8_TEMP+1
	  IF (TAGNO(N1+NPART).NE.0) IGR2_TEMP=IGR2_TEMP+1
          IF (TAGNO(N2+NPART).NE.0) IGR2_TEMP=IGR2_TEMP+1                           
C---------------------------------------------------------------------	    	  
	  IF (IGR8_TEMP.GT.0) THEN
	    DIFF = IGR2_TEMP-IGR8_TEMP
	    IF ((IGR9_TEMP.GT.0).OR.(DIFF.NE.0)) THEN
C--------CYL_JOINTS sur l'interface--------------------------
              IF (GR_ID.GT.0) THEN
                DO J=1,IGRNOD(GR_ID)%NENTITY
	      	  CUR_ID = IGRNOD(GR_ID)%ENTITY(J)
	          IF (TAGNO(CUR_ID+NPART).LT.3) THEN
                    CALL MODIF_TAG(TAGNO(NPART+CUR_ID),5,MODIF)   	       		   
	          ENDIF
                END DO
              ENDIF  
	      IF (TAGNO(N1+NPART).LT.3) THEN
	        CALL MODIF_TAG(TAGNO(NPART+N1),5,MODIF)
              ENDIF		
              IF (TAGNO(N2+NPART).LT.3) THEN
	        CALL MODIF_TAG(TAGNO(NPART+N2),5,MODIF)
              ENDIF
	    ENDIF
	    COMPT = COMPT + 1
C--------on tag le CYL_JOINT     garder------------------------            
	    TAGCYL(I)=ID_CYL	    		    			    	    	    
	  ENDIF 
                   
350	  CONTINUE	  	  
        END DO 
	   
	NEW_NJOINT = COMPT
	
C--------------------------------------------------------------------C
C------Prlecture et tag des MPC -> icompatible sur interface--------C
C--------------------------------------------------------------------C

        CALL HM_OPTION_START('/MPC')		
        IF (PASSE.EQ.0) ALLOCATE(TAGMPC(NUMMPC))
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
	    IF (TAGNO(N2+NPART).GE.0) COMPT_M=COMPT_M+1  		   
            IF (TAGNO(N2+NPART).GT.1) COMPT_S=COMPT_S+1 
            IF (TAGNO(N2+NPART).LE.0) COMPT_S=COMPT_S+1	    	    	    
	  END DO   	 
C-----On check si le MPC doit etre garde-------------------------
	  IF (COMPT_M.GT.0) THEN
	    IF (COMPT_S.EQ.0) THEN
C-----MPC interne ->	  
	      TAGMPC(I) = ID_MPC
              COMPT = COMPT + 1 	     	  
	    ELSE	    
C--------Provisoirement on interdit les MPC sur l'interface
           CALL ANCMSG(MSGID=896,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1="/MPC",
     .                 I1=ID_MPC)
	    ENDIF
	  ENDIF	  	      		    	  
	END DO
	        
	NEW_NUMMPC = COMPT
		  
C--------------------------------------------------------------------C
C------Prlecture et tag des GJOINTS -> icompatible sur interface----C
C--------------------------------------------------------------------C
C      
        CALL HM_OPTION_START('/GJOINT')
        IF (PASSE.EQ.0) ALLOCATE(TAGJOIN(NGJOINT))
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
	  IF(KEY2(1:4).EQ.'DIFF') VAL = 4
	  	  	  
          DO J=1,VAL
	    NS(J) = USR2SYS(NU(J),ITABM1,MESS,ID_JOIN)
	    IF (TAGNO(NS(J)+NPART).GE.0) COMPT_M=COMPT_M+1  		   
            IF (TAGNO(NS(J)+NPART).GT.1) COMPT_S=COMPT_S+1 
            IF (TAGNO(NS(J)+NPART).LE.0) COMPT_S=COMPT_S+1	    
	  END DO
	  
C-----On check si le GJOINT doit etre garde-------------------------
	  IF (COMPT_M.GT.0) THEN
	    IF (COMPT_S.EQ.0) THEN
C-----GJOINT interne ->	  
	      TAGJOIN(I) = ID_JOIN
              COMPT = COMPT + 1 	     	  
	    ELSE	    
C--------Provisoirement on interdit les GJOINT sur l'interface
           CALL ANCMSG(MSGID=896,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1="/GJOINT",
     .                 I1=ID_JOIN)
	    ENDIF
	  ENDIF	  	      		    	  
	END DO
	        
	NEW_NGJOINT = COMPT
		 
C--------------------------------------------------------------------C
C------Prlecture et tag des RBE2------------------------------------C
C--------------------------------------------------------------------C

        IF (PASSE.EQ.0) ALLOCATE(TAGRB2(NRBE2))
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
	    IF (IGRNOD(J)%ID.EQ.IGU) GR_ID = J
	  END DO
C---------on tag les cas d'erreur-------------------------------------		  
	  IF (GR_ID.EQ.0) THEN	     	  
	    COMPT = COMPT + 1		    
	    TAGRB2(I)=ID_RBE2
	    GOTO 360
	  ENDIF
C---------------------------------------------------------------------	  
	  COMPT_M = IGRNOD(GR_ID)%R2R_ALL
	  COMPT_S = IGRNOD(GR_ID)%R2R_SHARE
	  IF (TAGNO(N2+NPART).GE.0) COMPT_M=COMPT_M+1  		   
          IF (TAGNO(N2+NPART).GT.1) COMPT_S=COMPT_S+1 
          IF (TAGNO(N2+NPART).LE.0) COMPT_S=COMPT_S+1 	  
C-----On check si le RBE2 doit etre garde-------------------------
	  IF (COMPT_M.GT.0) THEN
	    IF (COMPT_S.EQ.0) THEN
C-----RBE2 interne ->	  
	      TAGRB2(I) = ID_RBE2
              COMPT = COMPT + 1 	     	  
	    ELSE	    
C--------Provisoirement on interdit les RBE2 sur l'interface
           CALL ANCMSG(MSGID=896,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1="/RBE2",
     .                 I1=ID_RBE2)
	    ENDIF
	  ENDIF
360	  CONTINUE	  	      		    	  
	END DO
	        
	NEW_NRBE2 = COMPT
		 	
C--------------------------------------------------------------------C
C------Prlecture et tag des RBE3------------------------------------C
C--------------------------------------------------------------------C

        IF (PASSE.EQ.0) ALLOCATE(TAGRB3(NRBE3))
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
C-----Lecture des groupes----------------------------------------	  	  
          DO L=1,VAL
            CALL HM_GET_FLOAT_ARRAY_INDEX('independentnodesetcoeffs',F,L,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_INT_ARRAY_INDEX('independentnodesets',IGU,L,IS_AVAILABLE,LSUBMODEL)
            DO J=1,NGRNOD	  
	      IF (IGRNOD(J)%ID.EQ.IGU) GR_ID = J
	    END DO
	    BUF_TEMP(L)=GR_ID	    
	    COMPT_M = COMPT_M + IGRNOD(GR_ID)%R2R_ALL
	    COMPT_S = COMPT_S + IGRNOD(GR_ID)%R2R_SHARE
	  END DO
	  
	  IF (TAGNO(N2+NPART).GE.0) COMPT_M=COMPT_M+1  		   
          IF (TAGNO(N2+NPART).GT.1) COMPT_S=COMPT_S+1 
          IF (TAGNO(N2+NPART).LE.0) COMPT_S=COMPT_S+1 
	  
C-----On check si le RBE3 doit etre garde-------------------------
	  IF (COMPT_M.GT.0) THEN
	    IF (COMPT_S.EQ.0) THEN
C-----RBE3 interne ->	  
	      TAGRB3(I) = ID_RBE3
              COMPT = COMPT + 1 	     	  
	    ELSE				    	  	  
C-----RBE3   cheval -> on met tous le noeuds mains sur l'interface
	      IF (TAGNO(NPART+N2).NE.-1) THEN
C-----on tag le rbe3 dans les domaines qui voeint l'second    
	        TAGRB3(I) = ID_RBE3
	        COMPT = COMPT +1	    	       	          
	      ENDIF 	      	         
              DO L=1,VAL
	        GR_ID = BUF_TEMP(L)
                DO J=1,IGRNOD(GR_ID)%NENTITY
	          CUR_ID = IGRNOD(GR_ID)%ENTITY(J)
	          IF (TAGNO(CUR_ID+NPART).LT.3) THEN
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
C------Prlecture et tag des Rigid links-----------------------------C
C--------------------------------------------------------------------C

	IF (PASSE.EQ.0) ALLOCATE(TAGLNK(NLINK))
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
	    IF (IGRNOD(J)%ID.EQ.IGU) GR_ID = J
	  END DO	  	   	  	  
C-----On check si le RLINK doit etre garde------------------------- 	  
	  TAG = 0
          NOD=IGRNOD(GR_ID)%R2R_ALL
	  IF (IGRNOD(GR_ID)%R2R_SHARE.EQ.0) TAG = 1
C-----On tag le rlink et tout ses noeuds --------------------------
	  IF (NOD.GT.0) THEN
	    COMPT = COMPT+1
	    TAGLNK(I)=ID_RLINK	  
	    IF(TAG.NE.1) THEN  	    
C-----RLINK "a cheval" -> on met tous les noeuds dans le couplage
              DO J=1,IGRNOD(GR_ID)%NENTITY
	      	CUR_ID = IGRNOD(GR_ID)%ENTITY(J)
	        IF (TAGNO(CUR_ID+NPART).LT.3) THEN
                  CALL MODIF_TAG(TAGNO(NPART+CUR_ID),5,MODIF)   	       		   
	        ENDIF
              END DO		      		
	    ENDIF	         
          ENDIF	    	  	  
        END DO

	NEW_NLINK = COMPT
C
C-------------------------------------------------------------------C
C------Prlecture et tag des interfaces de contact ------------------C
C----  Si contact interdomaine -> pretag elements necessaires-------C
C-------------------------------------------------------------------C
C
	COMPT_T2 = 0
	NB_INT = 0
        KCUR = KINTER
C
C-->  for TYPE19 interfaces NSLASH(KCUR) > NINTER because of specif /card for TYPE19	
	IF (PASSE.EQ.0) ALLOCATE(TAGINT(HM_NINTER+NSLASH(KCUR)),TAGINT_WARN(NINTER+1))
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
          IF (KEY(6:6).EQ.'/') FLG = 1
          IF ((LEN_TRIM(KEY))==5) FLG = 1
C	    	    
C ---------> cas nodes/surfaces---------------------------------------	  	  
	  IF (((KEY(1:5).EQ.'TYPE2').AND.(FLG.EQ.1)).OR.     
     .       (KEY(1:5).EQ.'TYPE7').OR.(KEY(1:5).EQ.'TYPE5').OR.(KEY(1:5).EQ.'TYPE8').OR.
     .       (KEY(1:6).EQ.'TYPE10').OR.(KEY(1:6).EQ.'TYPE14') ) THEN
             CONT = 1
             CALL HM_GET_INTV('secondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)
C	  
C--> les interfaces type2 penalite sont traitees comme des type7  <------C
             IF ((KEY(1:5).EQ.'TYPE2').AND.(FLG.EQ.1)) THEN
               CALL HM_GET_INTV('WFLAG',SPTFL,IS_AVAILABLE,LSUBMODEL)
               IF ((SPTFL.NE.25).AND.(SPTFL.NE.26)) THEN
	         CONT = 0
		 TYPE2 = 1
               ENDIF
             ENDIF   	      
C--> on determine si l'interface est  cheval et asymetrique  <------C	   		      	          
             CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,0,IGRPP_R2R,
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)

C--> pour les inter type 2 on couple en type5 si il ya un risque de masse nulle     
             IF (TYPE2.EQ.1) THEN
                 DO J=1,IGRNOD(G1)%NENTITY
	           CUR_ID = IGRNOD(G1)%ENTITY(J)
		   IF (TAGNO(CUR_ID+NPART).EQ.2) FLG_TIED(4) = 1
		   IF (TAGNO(CUR_ID+NPART).EQ.4) FLG_TIED(5) = 1		       
	         END DO  		 
	     ENDIF
C-->           	          	          	     	   
             IF (TAG.GT.0) THEN
	      	COMPT_T2 = COMPT_T2 + 1	     
C--> pretag elts cote main du contact pour les garder en void  <---C		   		      	          	     
                IF ((TAG==3).OR.(TAG==1).OR.(TAG==4)) THEN	   
                  CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .            IGRSURF(G2),IGRNOD,G2)
                ENDIF		  
C--> pretag elts cote second. du contact pour les garder en void <---C					      
                IF ((TAG==2).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R(IGRNOD(G1)%NENTITY,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,1,EANI,
     .            IGRSURF,IGRNOD,G1)
		ENDIF	   		      	          			   
	     ENDIF
C
C ---------> cas interface TYPE18 ---------------------------------------	  	  
	  ELSEIF (KEY(1:6).EQ.'TYPE18') THEN     
             CONT = 1
             CALL HM_GET_INTV('secondaryentityids',GR_BRIC,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INTV('tempsecondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)
  	      
C--> on determine si l'interface est  cheval et asymetrique  <------C
             IF (GRS > 0) THEN	   		      	          
               CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,0,IGRPP_R2R,
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
             ELSEIF (GR_BRIC > 0) THEN
               CALL TAGINT_R2R(G1,G2,GR_BRIC,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,3,IGRPP_R2R,
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
             ENDIF
C-->           	          	          	     	   
             IF (TAG.GT.0) THEN
	      	COMPT_T2 = COMPT_T2 + 1	     
C--> pretag elts cote main du contact pour les garder en void  <---C		   		      	          	     
                IF ((TAG==3).OR.(TAG==1).OR.(TAG==4)) THEN	   
                  CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .            IGRSURF(G2),IGRNOD,G2)
                ENDIF		  
C--> pretag elts cote second. du contact pour les garder en void <---C					      
                IF ((TAG==2).OR.(TAG==1)) THEN
                  IF (GRS > 0) THEN
                    CALL TAG_ELEM_VOID_R2R(IGRNOD(G1)%NENTITY,IPARTS,
     .              IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,1,EANI,
     .              IGRSURF,IGRNOD,G1)
                  ELSEIF (GR_BRIC > 0) THEN
                    DO J=1,IGRBRIC(G1)%NENTITY
                      CUR_ID = IGRBRIC(G1)%ENTITY(J)
C------------------------>   Tag des solides du grbric  <-------------C		    
                      IF ((TAG_ELS(CUR_ID+NPART).LT.(1+CONT)).AND.(TAGNO(IPARTS(CUR_ID)).NE.VAL)) THEN    	    
		        CALL MODIF_TAG(TAG_ELS(CUR_ID+NPART),1+CONT,MODIF)
	              ENDIF
                    ENDDO
                  ENDIF
		ENDIF	   		      	          			   
	     ENDIF
C
	  ELSEIF (KEY(1:6).EQ.'TYPE11') THEN
             CONT = 1
	     WARN = 0   	  	        
             CALL HM_GET_INTV('secondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)	     	      
C--> on determine si l'interface est  cheval et asymetrique  <------C	   		      	          
             CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,2,IGRPP_R2R,     
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
             IF (TAG.GT.0) THEN
	      	COMPT_T2 = COMPT_T2 + 1	     
C--> pretag elts ligne 1 du contact pour les garder en void  <-----C		   		      	          	     
                IF ((TAG==2).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R_LIN(IGRSLIN(G1)%NSEG,
     .            IPARTS,IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,VAL,CONT,
     .            MODIF,WARN,IGRSLIN(G1))        
                ENDIF     
C--> pretag elts ligne 2 du contact pour les garder en void  <-----C     				   
                IF ((TAG==3).OR.(TAG==1)) THEN		
                  CALL TAG_ELEM_VOID_R2R_LIN(IGRSLIN(G2)%NSEG,
     .            IPARTS,IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,VAL,CONT,
     .            MODIF,WARN,IGRSLIN(G2))          		
		ENDIF	     		      	          			   
	     ENDIF
             IF (WARN.EQ.1) THEN
             CALL ANCMSG(MSGID=892,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANSTOP,
     .                   I1=ID_INTER)
             ENDIF
	  ELSEIF (KEY(1:6).EQ.'TYPE24'.OR.KEY(1:6).EQ.'TYPE21'.OR.KEY(1:5).EQ.'TYPE6'.OR.
     .            KEY(1:6).EQ.'TYPE23'.OR.KEY(1:6).EQ.'TYPE20'.OR.KEY(1:6).EQ.'TYPE15'.OR.
     .          ((KEY(1:5).EQ.'TYPE3').AND.(FLG==1))) THEN
             CONT = 1
             FLAG_T24 = 0
             IF (KEY(1:6).EQ.'TYPE24') THEN
               FLAG_T24 = 1
             ELSE
               FLAG_T24 = 0
             ENDIF   	  	        
             CALL HM_GET_INTV('secondaryentityids',GRS,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INTV('mainentityids',GRM,IS_AVAILABLE,LSUBMODEL)	
             IF ((FLAG_T24 == 1).AND.(GRM.EQ.0)) GRM = GRS      
C--> on determine si l'interface est  cheval et asymetrique  <------C	   		      	          
             CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,1,IGRPP_R2R,
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
             IF (TAG.GT.0) THEN
	      	COMPT_T2 = COMPT_T2 + 1	     
C--> pretag elts surface 1 du contact pour les garder en void  <-----C		   		      	          	     
                IF ((TAG==2).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R(IGRSURF(G1)%NSEG,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .            IGRSURF(G1),IGRNOD,G1)
                ENDIF
C--> pretag elts surface 2 du contact pour les garder en void  <-----C     				   
                IF ((TAG==3).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .            IGRSURF(G2),IGRNOD,G2)	
		ENDIF	
             ENDIF    		    
C
C ---------> cas d'interfaces incompatibles
	  ELSEIF (KEY(1:3).NE.'SUB') THEN
            CALL ANCMSG(MSGID=835,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID_INTER,
     .                C1=LINE(1:13))
	  ENDIF
	  	     	  	  	     
        END DO
C
	NEW_HM_NINTER = COMPT
C		           	
C-------------------------------------------------------------------C
C---------------------------- Old Reader ---------------------------C
C-------------------------------------------------------------------C
	COMPT = 0
        COMPT2 = 0
        IREC=KOPTAD(KCUR)
C		
        DO NI=1,NSLASH(KCUR)
          I = NI + HM_NINTER
          CALL NEXTSLA
	  TAG = 0
	  VAL = IDDOM 	         	
          READ(IIN,REC=IREC,ERR=999,FMT='(A)') KLINE
	  TYPE2 = 0
	  FLG = 0
 
          IF (LINE(13:13).EQ.'/') FLG = 1
          IF (LINE(13:13).EQ.' ') FLG = 1
	    	    
C ---------> cas nodes/surfaces---------------------------------------	  	  
	  IF ((LINE(8:12).EQ.'TYPE5').OR.
     .    (LINE(8:12).EQ.'TYPE8').OR.(LINE(8:13).EQ.'TYPE10').OR.
     .    (LINE(8:13).EQ.'TYPE14').OR.
     .    ((LINE(8:12).EQ.'TYPE2').AND.(FLG.EQ.1)).OR.     
     .    (LINE(8:12).EQ.'TYPE7')) THEN
             CONT = 1
             CALL FREDEC_2KEY_3ID_T(KEY,ID_INTER,NUL,NUL,TITR)	     
             IREC=IREC+1
C--> les interfaces type2 penalite sont traitees comme des type7  <------C
             IF ((LINE(8:12).EQ.'TYPE2').AND.(FLG.EQ.1)) THEN
               READ(IIN,REC=IREC,ERR=999,FMT='(A)') LINE
               READ(LINE,ERR=999,FMT=FMT_4I) GRS,GRM,NUL,SPTFL
               IF ((SPTFL.NE.25).AND.(SPTFL.NE.26)) THEN
	         CONT = 0
		 TYPE2 = 1
               ENDIF
             ELSE
               READ(IIN,REC=IREC,ERR=999,FMT='(A)') LINE
               READ(LINE,ERR=999,FMT=FMT_2I) GRS,GRM
             ENDIF	     	      
C--> on determine si l'interface est  cheval et asymetrique  <------C	   		      	          
             CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,0,IGRPP_R2R,
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
C--> pour les inter type 2 on couple en type5 si il ya un risque de masse nulle     
             IF (TYPE2.EQ.1) THEN
                 DO J=1,IGRNOD(G1)%NENTITY
	           CUR_ID = IGRNOD(G1)%ENTITY(J)
		   IF (TAGNO(CUR_ID+NPART).EQ.2) FLG_TIED(4) = 1
		   IF (TAGNO(CUR_ID+NPART).EQ.4) FLG_TIED(5) = 1		       
	         END DO  		 
	     ENDIF
C-->	               	          	          	     	   
             IF (TAG.GT.0) THEN
	      	COMPT_T2 = COMPT_T2 + 1	     
C--> pretag elts cote main du contact pour les garder en void  <---C		   		      	          	     
                IF ((TAG==3).OR.(TAG==1).OR.(TAG==4)) THEN	   
                  CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .            IGRSURF(G2),IGRNOD,G2)
                ENDIF		  
C--> pretag elts cote second. du contact pour les garder en void <---C					      
                IF ((TAG==2).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R(IGRNOD(G1)%NENTITY,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,1,EANI,
     .            IGRSURF,IGRNOD,G1)
		ENDIF	   		      	          			   
             ELSE
	        IREC=IREC+1
	     ENDIF
	     
C ---------> cas surface/surface--------------------------------------	     
	  ELSEIF ((LINE(8:12).EQ.'TYPE6').OR.
     .    (LINE(8:13).EQ.'TYPE23').OR.(LINE(8:13).EQ.'TYPE20').OR.
     .    (LINE(8:13).EQ.'TYPE15').OR.((LINE(8:12).EQ.'TYPE3').AND.(FLG.EQ.1))) THEN
             CONT = 1	  	        
             CALL FREDEC_2KEY_3ID_T(KEY,ID_INTER,NUL,NUL,TITR)	  	        
             IREC=IREC+1
             READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE	    
             READ(LINE,ERR=999,FMT=FMT_2I) GRS,GRM
C--> on determine si l'interface est  cheval et asymetrique  <------C	   		      	          
             CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,1,IGRPP_R2R,
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
             IF (TAG.GT.0) THEN
	      	COMPT_T2 = COMPT_T2 + 1	     
C--> pretag elts surface 1 du contact pour les garder en void  <-----C		   		      	          	     
                IF ((TAG==2).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R(IGRSURF(G1)%NSEG,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .            IGRSURF(G1),IGRNOD,G1)
                ENDIF
C--> pretag elts surface 2 du contact pour les garder en void  <-----C     				   
                IF ((TAG==3).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R(IGRSURF(G2)%NSEG,IPARTS,
     .            IPARTC,IPARTG,IPARTSP,VAL,CONT,MODIF,MEMTR,0,0,EANI,
     .            IGRSURF(G2),IGRNOD,G2)	
		ENDIF	     		      	          			   
             ELSE
	        IREC=IREC+1
	     ENDIF
	     
C ---------> cas lignes/lignes---------------------------------------	     
	  ELSEIF (LINE(8:13).EQ.'TYPE11') THEN
             CONT = 1
	     WARN = 0   	  	        
             CALL FREDEC_2KEY_3ID_T(KEY,ID_INTER,NUL,NUL,TITR)	  	        
             IREC=IREC+1
             READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE	    
             READ(LINE,ERR=999,FMT=FMT_2I) GRS,GRM	     	      
C--> on determine si l'interface est  cheval et asymetrique  <------C	   		      	          
             CALL TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     .                	     TYPE2,VAL,TAG,I,COMPT,PASSE,2,IGRPP_R2R,     
     .                       IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
             IF (TAG.GT.0) THEN
	      	COMPT_T2 = COMPT_T2 + 1	     
C--> pretag elts ligne 1 du contact pour les garder en void  <-----C		   		      	          	     
                IF ((TAG==2).OR.(TAG==1)) THEN
                  CALL TAG_ELEM_VOID_R2R_LIN(IGRSLIN(G1)%NSEG,
     .            IPARTS,IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,VAL,CONT,
     .            MODIF,WARN,IGRSLIN(G1))        
                ENDIF     
C--> pretag elts ligne 2 du contact pour les garder en void  <-----C     				   
                IF ((TAG==3).OR.(TAG==1)) THEN		
                  CALL TAG_ELEM_VOID_R2R_LIN(IGRSLIN(G2)%NSEG,
     .            IPARTS,IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,VAL,CONT,
     .            MODIF,WARN,IGRSLIN(G2))          		
		ENDIF	     		      	          			   
             ELSE
	        IREC=IREC+1
	     ENDIF
             IF (WARN.EQ.1) THEN
             CALL ANCMSG(MSGID=892,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANSTOP,
     .                   I1=ID_INTER)
             ENDIF
	     
C ---------> cas interface type 19---------------------------------------	     
	  ELSEIF (LINE(8:13).EQ.'TYPE19') THEN
C-- /TYPE19 card is kept on both domains --
            TAGINT(I) = -1
            COMPT2 = COMPT2 + 1
	    IREC=IREC+1
	               	          	     
C ---------> cas d'interfaces incompatibles
	  ELSE
            CALL ANCMSG(MSGID=835,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID_INTER,
     .                C1=LINE(1:13))
	    IREC=IREC+1
	  ENDIF
	  	     	  	  	     
        END DO
C
	NEW_NINTER     = COMPT
	NEW_NSLASH_INT = COMPT + COMPT2
C
C-------------------------------------------------------------------C
C------Prlecture et tag des /INTER/SUB - (New reader)  -------------C
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
C------Prlecture et tag des Rigid bodies-----------------------------C
C--------------------------------------------------------------------C
      	     
        NI=0
	NB_RBY = 0
	NB_KIN = 0
	NB_LAG = 0
	IF (PASSE.EQ.0) ALLOCATE(TAGRBY(NRBODY))
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
	    IF (IGRNOD(J)%ID.EQ.IGU) GR_ID = J
	  END DO
C-----On check si le RBODY doit etre garde-------------------------
	  TAG = 0
          COMPT=IGRNOD(GR_ID)%R2R_ALL
	  COMPT2=IGRNOD(GR_ID)%R2R_SHARE
C--> on check aussi le tag du noeud main	    	  	  
	  IF (TAGNO(MAIN+NPART).GT.1) COMPT = COMPT + 1
	  IF (TAGNO(MAIN+NPART).GT.1) COMPT2 = COMPT2 + 1
	  IF (COMPT2.EQ.0) TAG = 1
C-----On tag le noeud main---------------------------------------
	  IF (COMPT.GT.0) THEN
	    IF(TAG.EQ.1) THEN
C-----RGBODY interne ->   
              CALL MODIF_TAG(TAGNO(NPART+MAIN),1,MODIF)	      
	    ELSE  	    
C-----RGBODY "a cheval" -> on met le main node dans le couplage 	    
	      IF (TAGNO(MAIN+NPART).LT.3) THEN
                CALL MODIF_TAG(TAGNO(NPART+MAIN),3,MODIF)
                IF (ISENS.NE.0) THEN
               CALL ANCMSG(MSGID=976,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1="FOR RBODY ID=",
     .                     I1=ID_RBY,
     .                     C2="- RBODY WITH SENSOR")
	        ENDIF                                              
	      ENDIF
	    ENDIF
	    TAGRBY(I)=ID_RBY         
	    NB_RBY = NB_RBY + 1
	    NB_KIN = NB_KIN + 1     
C-----On tag les rbys a garder-------------------------------------	      
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
     .                       OPTION_ID = ID_RBY,
     .                       KEYWORD2 = KEY,
     .                       OPTION_TITR = TITR)
C---------
         IF(KEY(1:6)=='LAGMUL') THEN
          NRB=NRB+1
C--------Provisoirement on interdit totalement les RBY/LAGMUL	    
          CALL ANCMSG(MSGID=835,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=LINE(1:L+9))
C-----------------------------------------------------------------
          CALL HM_GET_INTV('node_ID',MAIN,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('grnd_ID',IGU,IS_AVAILABLE,LSUBMODEL)
C--------Provisoirement on interdit totalement les RBY/LAGMUL	     	  
          MAIN=USR2SYS(MAIN,ITABM1,MESS,ID_RBY)
          DO J=1,NGRNOD         
	    IF (IGRNOD(J)%ID.EQ.IGU) GR_ID = J
	  END DO
	   	  	  
C-----On check si le RBODY doit etre garde-------------------------
 	  
	  TAG = 0
          COMPT=IGRNOD(GR_ID)%R2R_ALL
	  COMPT2=IGRNOD(GR_ID)%R2R_SHARE
C--> on check aussi le tag du noeud main	    	  	  
	  IF (TAGNO(MAIN+NPART).GT.1) COMPT = COMPT + 1
	  IF (TAGNO(MAIN+NPART).GT.1) COMPT2 = COMPT2 + 1
	  IF (COMPT2.EQ.0) TAG = 1
	  
C-----On tag le noeud main---------------------------------------

	  IF (COMPT.GT.0) THEN
	    IF(TAG.EQ.1) THEN
C-----RGBODY interne ->   
              CALL MODIF_TAG(TAGNO(NPART+MAIN),1,MODIF)           
	    ELSE          
C-----RGBODY "a cheval" -> on met le main node dans le couplage 	    
	      IF (TAGNO(MAIN+NPART).LT.3) THEN
                CALL MODIF_TAG(TAGNO(NPART+MAIN),3,MODIF)
	      ENDIF
	    ENDIF
C-----On tag les rbys a garder-------------------------------------	      
	    TAGRBY(I)=ID_RBY         
	    NB_RBY = NB_RBY + 1
	    NB_LAG = NB_LAG + 1 
          ENDIF  ! IF (COMPT.GT.0) THEN
         END IF ! IF(KEY(1:6)=='LAGMUL') THEN
        END DO
	
	NEW_NRBY     = NB_RBY
	NRBYLAG      = NB_LAG

C--------------------------------------------------------------------C
C------Prlecture et tag des BCS pour SPH-----------------------------C
C--------------------------------------------------------------------C
      	     
        NSPCONDN = 0
        CALL HM_OPTION_START('/SPHBCS')
	IF (PASSE.EQ.0) ALLOCATE(TAGSPHBCS(NSPCOND))
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
	    IF (IGRNOD(J)%ID.EQ.IGU) GR_ID = J
	  END DO
C---------------------------------------------------------------------
          COMPT = 0
          DO L=1,IGRNOD(GR_ID)%NENTITY
	    CUR_ID = NOD2SP(IGRNOD(GR_ID)%ENTITY(L))
            IF (TAGNO(IPARTSP(CUR_ID)).NE.0) THEN
              COMPT = COMPT + 1
            ENDIF
          ENDDO
C---------------------------------------------------------------------
          IF (COMPT.GT.0) THEN
            NSPCONDN = NSPCONDN + 1
            TAGSPHBCS(I) = ID
          ENDIF
        END DO
			
C--------------------------------------------------------------------C
C------Prlecture et tag des inlets/outlets pour SPH------------------C
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
C------Prlecture des ALE/LINK----------------------------------------C
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
                 IF ((IGRNOD(GR_ID)%R2R_ALL > 0) .AND. (TAGNO(NPART+N1) == -1)) THEN
                    CALL MODIF_TAG(TAGNO(NPART + N1), 2, MODIF)
                 ELSEIF ((IGRNOD(GR_ID)%R2R_SHARE > 0) .AND. (TAGNO(NPART+N1) == 1)) THEN
                    CALL MODIF_TAG(TAGNO(NPART+N1), 2, MODIF)
                 ELSEIF (TAGNO(NPART+N1) == -1) THEN
                    CALL MODIF_TAG(TAGNO(NPART+N1), 0, MODIF)
                 ENDIF
              ENDIF
              IF (N2 > 0) THEN
                 IF ((IGRNOD(GR_ID)%R2R_ALL > 0) .AND. (TAGNO(NPART+N2) == -1)) THEN
                    CALL MODIF_TAG(TAGNO(NPART+N2), 2, MODIF)
                 ELSEIF ((IGRNOD(GR_ID)%R2R_SHARE > 0) .AND. (TAGNO(NPART+N2) == 1)) THEN
                    CALL MODIF_TAG(TAGNO(NPART+N2), 2, MODIF)
                 ELSEIF (TAGNO(NPART+N2) == -1) THEN
                    CALL MODIF_TAG(TAGNO(NPART+N2), 0, MODIF)
                 ENDIF
              ENDIF
           ENDDO
        ENDIF
        
C--------------------------------------------------------------------C
C------Prlecture et tag des RWALL------------------------------------C
C--------------------------------------------------------------------C

C--------------------------------------------------------------------C
C----------New Reader -----------------------------------------------C
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
C--------On interdit les RWALL /LAGMUL	    
             CALL ANCMSG(MSGID=835,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=LINE(1:L+6))
	  ELSE
            CALL HM_GET_INTV('Node1',NOD,IS_AVAILABLE,LSUBMODEL)  
            IF (NOD.GT.0) THEN
              NOD=USR2SYS(NOD,ITABM1,MESS,ID)
              CALL MODIF_TAG(TAGNO(NPART+NOD),4,MODIF)
            ENDIF	    
	  ENDIF
        ENDDO
C--------------------------------------------------------------------C
C------Prlecture et tag des INIVOL-----------------------------------C
C--------------------------------------------------------------------C

        NEW_NINIVOL = 0
	IF (PASSE.EQ.0) ALLOCATE(TAG_INIVOL(NINIVOL))
        TAG_INIVOL(:) = 0
        CALL HM_OPTION_START('/INIVOL')

        DO I=1,NINIVOL
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('secondarycomponentlist', ID_PART, IS_AVAILABLE, LSUBMODEL)
C
          DO J=1,NPART
            IF(IPART_L(4,J).EQ.ID_PART) IDS=J
          ENDDO
C
          IF (TAGNO(IDS) > 0) THEN
            TAG_INIVOL(I) = ID
            NEW_NINIVOL = NEW_NINIVOL + 1
          ENDIF
        ENDDO

C------------------------------------------------------------------	

      RETURN
                  
C------------------------------------------------------------------
 999  CALL FREERR(3)
      RETURN          
      
      END

Chd|====================================================================
Chd|  R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FREDEC3                       source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NEXTSLA                       source/starter/freform.F      
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "lagmult.inc"
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
C------Prlecture et tag des Injecteurs (MONVOL)- New reader ---------C
C--------------------------------------------------------------------C

        CALL HM_OPTION_START('/MONVOL')
C
        DO I=1,NMONVOL
C
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            KEYWORD2 = KEY)
C
          IF(KEY(1:7).EQ.'AIRBAG1')THEN
            TYPE = 1
            CALL HM_GET_INTV('surf', ISUR, IS_AVAILABLE, LSUBMODEL)
          ELSEIF(KEY(1:6).EQ.'AIRBAG')THEN
            TYPE = 2
            CALL HM_GET_INTV('entityiddisplayed', ISUR, IS_AVAILABLE, LSUBMODEL)
          ENDIF

C----------------------------------------------------------
          DO J=1,NSURF
            IF (ISUR.EQ.IGRSURF(J)%ID) ISURS=J
          ENDDO
	  CPT_SEG=0
	  CCPL=0
	  FLAG=0
          DO J=1,IGRSURF(ISURS)%NSEG
	      CPT_NOD=0
	      CPT_CPL=0  	    
              DO K=1,4		
	        CUR_ID = IGRSURF(ISURS)%NODES(J,K)
C             --> on compte les noeuds tags <--		
                IF (TAGNO(CUR_ID+NPART).GE.0) THEN
	          CPT_NOD=CPT_NOD+1
		  IF (TAGNO(CUR_ID+NPART).EQ.2) CPT_CPL=CPT_CPL+1     		   
                ENDIF
              END DO	  
C             --> si tous les noeuds du seg sont dans le subdomaine on valide <--
              IF (CPT_CPL.NE.0) WARN = 1
              IF (CPT_NOD.EQ.4) CPT_SEG=CPT_SEG+1	      
	  END DO	      
C--------On interdit les surfaces de monvol sur l'interface--------------------
C	  IF (WARN.GT.0) THEN
C	        CALL ANSTCKC(LEN_TRIM(LINE),LINE)			  
C                CALL ANCWARN(843,ANINFO_BLIND_1)			      
C	  ENDIF      	  
C-------Cas AIRBAG1----------------------------------------
          IF ((TYPE==1).AND.(TAGMON(I).GT.0)) THEN
             CALL HM_GET_INTV('nb_jet', NJET, IS_AVAILABLE, LSUBMODEL)
C             --> on boucle sur les injecteurs <--
             DO J=1,NJET
               CALL HM_GET_INT_ARRAY_INDEX('ijet', IJET, J, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('node1', N1, J, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('node2', N2, J, IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('node3', N3, J, IS_AVAILABLE, LSUBMODEL)
	       IF (IJET.EQ.1) THEN
	          IF (N1.NE.0) THEN	       
                    N1=USR2SYS(N1,ITABM1,MESS,NUL)
	            IF(TAGNO(N1+NPART).LT.2) TAGNO(N1+NPART) = 1
		  ENDIF
	          IF (N2.NE.0) THEN		    
	            N2=USR2SYS(N2,ITABM1,MESS,NUL)
	            IF(TAGNO(N2+NPART).LT.2) TAGNO(N2+NPART) = 1
		  ENDIF    
		  IF (N3.NE.0) THEN	     
	            N3=USR2SYS(N3,ITABM1,MESS,NUL)
	            IF(TAGNO(N3+NPART).LT.2) TAGNO(N3+NPART) = 1
		  ENDIF  
	       ENDIF
	     END DO	     	           
	  ENDIF
C-------Cas AIRBAG----------------------------------------
          IF ((TYPE==2).AND.(TAGMON(I).GT.0)) THEN
             CALL HM_GET_INTV('ABG_Njet', NJET, IS_AVAILABLE, LSUBMODEL)
C             --> on boucle sur les injecteurs <--
             DO J=1,NJET
               CALL HM_GET_INT_ARRAY_INDEX('ABG_Ijet', IJET, J,IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('ABG_N1', N1, J,IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('ABG_N2', N2, J,IS_AVAILABLE, LSUBMODEL)
               CALL HM_GET_INT_ARRAY_INDEX('ABG_N3', N3, J,IS_AVAILABLE, LSUBMODEL)
C
	       IF (IJET.EQ.1) THEN
	          IF (N1.NE.0) THEN	       
                    N1=USR2SYS(N1,ITABM1,MESS,NUL)
	            IF(TAGNO(N1+NPART).LT.2) TAGNO(N1+NPART) = 1
		  ENDIF
	          IF (N2.NE.0) THEN		    
	            N2=USR2SYS(N2,ITABM1,MESS,NUL)
	            IF(TAGNO(N2+NPART).LT.2) TAGNO(N2+NPART) = 1
		  ENDIF
	          IF (N3.NE.0) THEN	     
	            N3=USR2SYS(N3,ITABM1,MESS,NUL)
	            IF(TAGNO(N3+NPART).LT.2) TAGNO(N3+NPART) = 1
	          ENDIF 
	       ENDIF	   
	     END DO	     	           
	  ENDIF
C---------------------------------------------------------	   	  	           	   	    	    
	END DO
C
	NMONVOL = NEW_HM_NVOLU
C
C--------------------------------------------------------------------C
C------Prlecture et tag des Injecteurs (MONVOL)- Old reader ---------C
C--------------------------------------------------------------------C
      	                	          
        KCUR = KMONVO
        IREC=KOPTAD(KCUR)-1
	  
        DO I=1,NVOLU
	  WARN = 0
	  TYPE = 0
	  CALL NEXTSLA
C--------on determine le type-----------------------------	
C          CALL RDSLAKE_UID(NUL,NUL,TITR,NUL,40,KEY)
          CALL FREDEC3(KEY,NUL,TITR)
          IREC=IREC+1	  
          IF(KEY(1:7).EQ.'AIRBAG1')THEN
              TYPE = 1
          ELSEIF(KEY(1:6).EQ.'AIRBAG')THEN
              TYPE = 2
          ELSEIF(KEY(1:5).EQ.'COMMU')THEN
              TYPE = 3
          ENDIF
C----------------------------------------------------------
          READ (IIN,REC=IREC,ERR=999,FMT=FMT_I)ISUR  
          DO J=1,NSURF
            IF (ISUR.EQ.IGRSURF(J)%ID) ISURS=J
          ENDDO
	  CPT_SEG=0
	  CCPL=0
	  FLAG=0
          DO J=1,IGRSURF(ISURS)%NSEG
	      CPT_NOD=0
	      CPT_CPL=0  	    
              DO K=1,4		
	        CUR_ID = IGRSURF(ISURS)%NODES(J,K)
C             --> on compte les noeuds tags <--		
                IF (TAGNO(CUR_ID+NPART).GE.0) THEN
	          CPT_NOD=CPT_NOD+1
		  IF (TAGNO(CUR_ID+NPART).EQ.2) CPT_CPL=CPT_CPL+1     		   
                ENDIF
              END DO	  
C             --> si tous les noeuds du seg sont dans le subdomaine on valide <--
              IF (CPT_CPL.NE.0) WARN = 1
              IF (CPT_NOD.EQ.4) CPT_SEG=CPT_SEG+1	      
	  END DO	      
C--------On interdit les surfaces de monvol sur l'interface--------------------
C	  IF (WARN.GT.0) THEN
C	        CALL ANSTCKC(LEN_TRIM(LINE),LINE)			  
C                CALL ANCWARN(843,ANINFO_BLIND_1)			      
C	  ENDIF      	  
C-------Cas AIRBAG1----------------------------------------
          IF ((TYPE==1).AND.(TAGMON(I).GT.0)) THEN
	     IREC=IREC+3
             READ (IIN,REC=IREC,ERR=999,FMT=FMT_I) NJET
	     IREC=IREC+1
C             --> on boucle sur les injecteurs <--
             DO J=1,NJET
               READ (IIN,REC=IREC,ERR=999,FMT='(A)')LINE
               READ (LINE,ERR=999,FMT=FMT_6I) 
     .             NUL,NUL,IJET,N1,N2,N3
               IF (J.LT.NJET) IREC=IREC+1+IJET
	       IF (IJET.EQ.1) THEN
	          IF (N1.NE.0) THEN	       
                    N1=USR2SYS(N1,ITABM1,MESS,NUL)
	            IF(TAGNO(N1+NPART).LT.2) TAGNO(N1+NPART) = 1
		  ENDIF
	          IF (N2.NE.0) THEN		    
	            N2=USR2SYS(N2,ITABM1,MESS,NUL)
	            IF(TAGNO(N2+NPART).LT.2) TAGNO(N2+NPART) = 1
		  ENDIF    
		  IF (N3.NE.0) THEN	     
	            N3=USR2SYS(N3,ITABM1,MESS,NUL)
	            IF(TAGNO(N3+NPART).LT.2) TAGNO(N3+NPART) = 1
		  ENDIF  
	       ENDIF
	     END DO	     	           
	  ENDIF
C-------Cas AIRBAG----------------------------------------
          IF ((TYPE==2).AND.(TAGMON(I).GT.0)) THEN
	     IREC=IREC+4
             READ (IIN,REC=IREC,ERR=999,FMT=FMT_I) NJET
	     IREC=IREC+3
C             --> on boucle sur les injecteurs <--
             DO J=1,NJET
               READ (IIN,REC=IREC,ERR=999,FMT='(A)')LINE
               READ (LINE,ERR=999,FMT=FMT_4I) IJET,N1,N2,N3
	       IF (J.LT.NJET) IREC=IREC+3+IJET
	       IF (IJET.EQ.1) THEN
	          IF (N1.NE.0) THEN	       
                    N1=USR2SYS(N1,ITABM1,MESS,NUL)
	            IF(TAGNO(N1+NPART).LT.2) TAGNO(N1+NPART) = 1
		  ENDIF
	          IF (N2.NE.0) THEN		    
	            N2=USR2SYS(N2,ITABM1,MESS,NUL)
	            IF(TAGNO(N2+NPART).LT.2) TAGNO(N2+NPART) = 1
		  ENDIF
	          IF (N3.NE.0) THEN	     
	            N3=USR2SYS(N3,ITABM1,MESS,NUL)
	            IF(TAGNO(N3+NPART).LT.2) TAGNO(N3+NPART) = 1
	          ENDIF 
	       ENDIF	   
	     END DO	     	           
	  ENDIF
C-------Cas COMMU----------------------------------------
          IF ((TYPE==3).AND.(TAGMON(I).GT.0)) THEN
	     IREC=IREC+4
             READ (IIN,REC=IREC,ERR=999,FMT=FMT_I) NJET
	     IREC=IREC+3
C             --> on boucle sur les injecteurs <--
             DO J=1,NJET
               READ (IIN,REC=IREC,ERR=999,FMT='(A)')LINE
               READ (LINE,ERR=999,FMT=FMT_4I) NUL,N1,N2,N3
	       IF (J.LT.NJET) IREC=IREC+4
	       IF (N1.NE.0) THEN	       
                   N1=USR2SYS(N1,ITABM1,MESS,NUL)
	           IF(TAGNO(N1+NPART).LT.2) TAGNO(N1+NPART) = 1
	       ENDIF
	       IF (N2.NE.0) THEN	          
	           N2=USR2SYS(N2,ITABM1,MESS,NUL)
	           IF(TAGNO(N2+NPART).LT.2) TAGNO(N2+NPART) = 1
	       ENDIF	   
	       IF (N3.NE.0) THEN	     
	          N3=USR2SYS(N3,ITABM1,MESS,NUL)
	          IF(TAGNO(N3+NPART).LT.2) TAGNO(N3+NPART) = 1
	       ENDIF  
	     END DO     	     	           
	  ENDIF
C---------------------------------------------------------	   	  	           	   	    	    
	END DO
        	
	NVOLU = NEW_NVOLU

C-----------
      RETURN
           
C------------------------------------------------------------------
 999  CALL FREERR(3)
      RETURN          
      
      END            
