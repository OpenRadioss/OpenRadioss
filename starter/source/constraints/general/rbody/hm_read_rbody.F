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
Chd|  HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        NEWDBL                        source/system/sysfus.F        
Chd|        RIGMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|        SPMDSET                       source/constraints/general/rbody/spmdset.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        NODGRNR6                      source/starter/freform.F      
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RBODY(RBY       ,NPBY    ,LPBY    ,ITAB     ,ITABM1   ,
     2                         IGRNOD    ,IGRSURF ,IBFV    ,IGRV     ,IBGR     ,
     3                         SENSORS   ,IMERGE  ,UNITAB  ,ISKN     ,NOM_OPT  ,
     4                         NUMSL     ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,KNOD2EL1D,
     5                         KNOD2ELQ  ,ITAGND  ,ICDNS10 ,LSUBMODEL,ICFIELD  ,
     6                         LCFIELD   )
C-------------------------------------
C     LECTURE STRUCTURE RIGIDES 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NPBY(NNPBY,*), LPBY(*), ITAB(*), ITABM1(*)
      INTEGER IBFV(NIFV,*)
      INTEGER IGRV(NIGRV,*),IBGR(*),IMERGE(*),
     .        ISKN(LISKN,*),NUMSL,
     .        KNOD2ELS(*),KNOD2ELC(*),KNOD2ELTG(*),KNOD2EL1D(*),KNOD2ELQ(*),
     .        ITAGND(*),ICDNS10(*), ICFIELD(SIZFIELD,*), LCFIELD(*)
C     REAL
      my_real
     .   RBY(NRBY,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF        
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NSL, NSL0, NSKEW, IC,
     .   ISPHER, IGU,IGS,ISENS,ID,ICDG,
     .   JC,UID,IFLAGUNIT,SUB_INDEX,NRB,
     .   IFAIL,NRB_R2R
      INTEGER IDSURF, ISU, NN, IAD, M, IOPT, IEXPAMS, NEL
      CHARACTER MESS*40,TITR*nchartitle,TITR1*nchartitle,KEY*ncharkey
      my_real
     .     BID, MASS, I1, I2, I3, I12, I23, I13, FN, FT, EXPN, EXPT
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TABSL
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NGR2USR,NODGRNR6
C-----------------------------------------------
C   NPBY(NNPBY,NRBYKIN), NNPBY=17
C    1 : main NODE
C    2 : NUMBER OF SECONDARY NODES
C    3 : ICOG
C    4 : ISENS 
C    5 : FLAG SPHERICAL INERTIA
C    6 : IDENTIFIER
C    7 : 1 ON(1) OFF(0)
C    8 : ISU
C    9 : NSKEW
C   10 : IEXPAMS (AMS - Hidden)
C         = 1 (default) : AMS expansion ; = 2 (Hidden) : No expansion
C   11 : IAD => SECONDARY nodes LPBY(IAD+1:IAD+NSN)
C   12 : RIGID BODY LEVEL (IN MERGE RELATIONS)
C   13 : MERGING FLAG FOR THE main RIGID BODY
C   14 : NUMBER OF SECONDARY NODES WITH MERGING FLAG = 1
C   15 : NUMBER OF SECONDARY NODES WITH MERGING FLAG = 2 
C   16 : NUMBER OF SECONDARY NODES WITH MERGING FLAG = 3
C   17 : IKREM
C   18 : IFAIL
C-----------------------------------------------
C   RBY(NRBY,NRBYKIN), NRBY=25
C   LOADED DURING READING       AFTER INITIALIZATION (including in RD ENGINE)
C   1   : Added Mass            1..9 :   Principal directions
C   2..4: IXX, IYY, IZZ         10..12:  Principal inertia I1, I2, I3
C   5..7: IXY, IYZ, IXZ         13:      Initial inertia of Main Node (cf deactivation of rbody)
C                               14:      Rigid body mass
C                               15:      Initial mass of main node (cf deactivation of rbody)
C                               17..25:  Inertia matrix in global system
C   26: FN : Normal force at failure (Ifail=1)
C   27: FT : Shear force at failure (Ifail=1)
C   28: EXPN (Ifail=1)
C   29: EXPT (Ifail=1)
C   30: CRIT (computed at each cycle in RD Engine) 
C=======================================================================
      DATA MESS/'RIGID BODY DEFINITION                   '/
C=======================================================================
      IF (NUMSL > 0) THEN
        CALL MY_ALLOC(TABSL,2,NUMSL)
        TABSL=0
      END IF

      WRITE(IOUT,1000)
C--------------------------------------------------
C START BROWSING MODEL RBODY
C--------------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RBODY')
C
      CALL MY_ALLOC(ITAG,NUMNOD) 
      ITAG(1:NUMNOD) = 0
C
      K=0
      NRB=0
      NRB_R2R=0
C
      DO N=1,NRBODY
C
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
C
       NRB_R2R = NRB_R2R + 1     
       IF (NSUBDOM > 0) THEN
         IF(TAGRBY(NRB_R2R) == 0) CALL HM_SZ_R2R(TAGRBY,NRB_R2R,LSUBMODEL)
       ENDIF
C
       KEY=''
       CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       SUBMODEL_INDEX = SUB_INDEX)
       IF(KEY=='')THEN ! not a /RBODY/LAGMUL     
        NRB = NRB + 1
C-------
        IFLAGUNIT = 0
        DO J=1,NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID/=0.AND.IFLAGUNIT == 0) THEN
        CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I2=UID,I1=ID,C1='RIGID BODY',
     .               C2='RIGID BODY',
     .               C3=TITR) 
        ENDIF
C
        NOM_OPT(1,NRB)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NRB),LTITR)
C
        CALL HM_GET_INTV('node_ID',NPBY(1,NRB),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('sens_ID',ISENS,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Skew_ID',NSKEW,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Ispher',ISPHER,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('grnd_ID',IGU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Ikrem',IKREM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('ICoG',ICDG,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('surf_ID',IDSURF,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV('Mass',MASS,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
        IF(ISPHER == 0) ISPHER=2
        IF(ICDG == 0)ICDG=1
c
        IF(NSKEW == 0 .AND. SUB_INDEX /= 0 ) NSKEW = LSUBMODEL(SUB_INDEX)%SKEW
        DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(NSKEW == ISKN(4,J+1)) THEN
            NSKEW=J+1
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .          C1='RIGID BODY',
     .          C2='RIGID BODY',
     .          I2=NSKEW,I1=ID,C3=TITR)
 100    CONTINUE
C
        RBY(1,NRB) = MASS
C
        ISU=0
        IF (IDSURF/=0) THEN
          INGR2USR => IGRSURF(1:NSURF)%ID
          ISU=NGR2USR(IDSURF,INGR2USR,NSURF)
          IF (ISU == 0) THEN
             CALL ANCMSG(MSGID=158,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I2=IDSURF,I1=ID,C1=TITR)
          ELSEIF (IGRSURF(ISU)%TYPE/=101) THEN
             TITR1 = IGRSURF(IGS)%TITLE
             CALL ANCMSG(MSGID=159,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I2=IDSURF,C2=TITR1,I1=ID,C1=TITR)
          ENDIF
        ENDIF
        NPBY(8,NRB)=ISU
C
        CALL HM_GET_FLOATV('Jxx',I1,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Jyy',I2,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Jzz',I3,IS_AVAILABLE,LSUBMODEL,UNITAB)
        RBY(2,NRB) = I1
        RBY(3,NRB) = I2
        RBY(4,NRB) = I3
        CALL HM_GET_FLOATV('Jxy',I12,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Jyz',I23,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Jxz',I13,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
        CALL HM_GET_INTV('Ioptoff',IOPT,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Iexpams',IEXPAMS,IS_AVAILABLE,LSUBMODEL)
C
        CALL HM_GET_INTV('Ifail',IFAIL,IS_AVAILABLE,LSUBMODEL)
        NPBY(18,NRB)=IFAIL
        IF(IFAIL==1)THEN
          CALL HM_GET_FLOATV('FN',FN,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('FT',FT,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('expN',EXPN,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('expT',EXPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
          IF(FN==ZERO)FN=EP20
          IF(FT==ZERO)FT=EP20
          IF(EXPN==ZERO) EXPN=TWO
          IF(EXPT==ZERO) EXPT=TWO
          RBY(26,NRB)=FN
          RBY(27,NRB)=FT
          RBY(28,NRB)=EXPN
          RBY(29,NRB)=EXPT
        END IF
C
        RBY(5,NRB) = I12
        RBY(6,NRB) = I23
        RBY(7,NRB) = I13
        NPBY(1,NRB)= USR2SYS(NPBY(1,NRB),ITABM1,MESS,ID)
!
        DO JC=1,NMERGED
          IF (NPBY(1,NRB) == IMERGE(JC)) NPBY(1,NRB)=IMERGE(NUMCNOD+JC)
	ENDDO
        CALL ANODSET(NPBY(1,NRB), CHECK_RB_M)
C
        NPBY(11,NRB)=K
        M = NPBY(1,NRB)
        NSL = NODGRNR6(M,IGU,IGS,LPBY(K+1),IGRNOD,ITABM1,MESS,ID)
c
        DO I=1,NSL
          ITAG(LPBY(K+I)) = 1
        ENDDO
c
        IF (NS10E > 0 ) THEN
         CALL RIGMODIF_ND(NSL,LPBY(K+1),ITAGND,ICDNS10,ID,TITR,ITAB)
         M = NPBY(1,NRB)
         IF (ITAGND(M)/=0) THEN
          CALL ANCMSG(MSGID=1211,
     .           MSGTYPE=MSGERROR,
     .           ANMODE=ANINFO,
     .           I1=ITAB(M),
     .           C1='RBODY',
     .           I2=ID,
     .           C2='RBODY')
         END IF
        END IF
        NPBY(2,NRB)=NSL
        DO J=1, NSL
           CALL ANODSET(LPBY(J+K), CHECK_RB_S)
           TABSL(1,J+K)=ITAB(LPBY(J+K))
           TABSL(2,J+K)=N
        ENDDO
C
        IF(ISENS > 0)THEN
          DO I=1,SENSORS%NSENSOR
            IF (ISENS == SENSORS%SENSOR_TAB(I)%SENS_ID) NPBY(4,NRB)=I
          ENDDO
          IF(NPBY(4,NRB) == 0)THEN
             TITR1 = IGRSURF(IGS)%TITLE
             CALL ANCMSG(MSGID=159,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I2=ISENS,C2=TITR1,I1=ID,C1=TITR)
          ENDIF
          RBY(1,NRB)=ZERO
          RBY(2,NRB)=ZERO
          RBY(3,NRB)=ZERO
          RBY(4,NRB)=ZERO
          RBY(5,NRB)=ZERO
          RBY(6,NRB)=ZERO
          RBY(7,NRB)=ZERO
          NSKEW=0
          ICDG =0
          IKREM=1
        ENDIF
        NPBY(5,NRB)=ISPHER
        NPBY(6,NRB)=ID
        NPBY(17,NRB)=IKREM
        IF(ISENS == 0)THEN
          NPBY(7,NRB)=1
        ELSE
          NPBY(7,NRB)=0
        ENDIF
        NPBY(3,NRB) =ICDG
        NPBY(9,NRB) =NSKEW
        IF(IEXPAMS==0)THEN
          IEXPAMS=1
        ELSEIF(IEXPAMS==2)THEN
          IEXPAMS=0
        END IF
        NPBY(10,NRB)=IEXPAMS
        NSL0 = NSL
        IF (NSUBDOM > 0) NSL0 = IGRNOD(IGS)%R2R_ALL
        IF (NSL0 == 0) THEN
           CALL ANCMSG(MSGID=352,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
C
        CALL SPMDSET(NRB,NPBY,NNPBY,LPBY,NSL,K)
C
        IF(ISMS==0)THEN
          IF (ISENS/=0) THEN
           WRITE(IOUT,1100) ID,TRIM(TITR),ISENS,ITAB(NPBY(1,NRB)),NSL,
     .            IDSURF,ISPHER
          ELSE
           WRITE(IOUT,1111) ID,TRIM(TITR),ITAB(NPBY(1,NRB)),NSL,
     .            IDSURF,ISKN(4,NSKEW),ISPHER,IKREM,ICDG,
     .            (RBY(J,NRB),J=1,7)
          ENDIF
        ELSE
          IF (ISENS/=0) THEN
           WRITE(IOUT,1102) ID,TRIM(TITR),ISENS,ITAB(NPBY(1,NRB)),NSL,
     .            IDSURF,ISPHER
          ELSE
           WRITE(IOUT,1112) ID,TRIM(TITR),ITAB(NPBY(1,NRB)),NSL,
     .            IDSURF,ISKN(4,NSKEW),ISPHER,IKREM,ICDG,
     .            (RBY(J,NRB),J=1,7)
          ENDIF
          WRITE(IOUT,1103)
        END IF
        IF(IFAIL==1)THEN
          WRITE(IOUT,1151)
          WRITE(IOUT,1152) FN, EXPN, FT, EXPT
        END IF
        WRITE(IOUT,1201)
        WRITE(IOUT,1202) (ITAB(LPBY(I+K)),I=1,NSL)
        K=K+NSL
C-------------------------------
C VITESSE FIXE SUR main EN ROT
C-------------------------------
        DO J=1,NFXVEL
          IF(IABS(IBFV(1,J)) == NPBY(1,NRB).AND.
     .       IBFV(2,J)-10*(IBFV(2,J)/10)>=4)THEN
            IBFV(6,J)=N
          ENDIF
        ENDDO
C-------------------------------
C main BELONGS TO MESH
C-------------------------------
        NEL=KNOD2ELS(NPBY(1,NRB)+1) -KNOD2ELS(NPBY(1,NRB))
     .     +KNOD2ELC(NPBY(1,NRB)+1) -KNOD2ELC(NPBY(1,NRB))
     .     +KNOD2ELTG(NPBY(1,NRB)+1)-KNOD2ELTG(NPBY(1,NRB))
     .     +KNOD2EL1D(NPBY(1,NRB)+1)-KNOD2EL1D(NPBY(1,NRB))
     .     +KNOD2ELQ(NPBY(1,NRB)+1)-KNOD2ELQ(NPBY(1,NRB))
        IF(NEL/=0)THEN
         IF(ISMS==0)THEN
          ID=NOM_OPT(1,NRB)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NRB),LTITR)
          CALL ANCMSG(MSGID=448,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ITAB(NPBY(1,NRB)),
     .                I2=ID,
     .                C1=TITR)
         ELSE
          ID=NOM_OPT(1,NRB)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NRB),LTITR)
          CALL ANCMSG(MSGID=1066,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(NPBY(1,NRB)),
     .                I2=ID,
     .                C1=TITR)
         END IF
        END IF
       END IF ! IF(KEY=='')THEN
      ENDDO
C-------------------------------------
C Recherche des Rigid Body ID doubles
C-------------------------------------
      CALL UDOUBLE(NPBY(6,1),NNPBY,NRBYKIN,MESS,0,BID)
C-------------------------------------
C Recherche des Main Node ID doubles
C-------------------------------------
      IC = 442
      I = 0
      CALL NEWDBL(NPBY(1,1),NNPBY,NRBYKIN,ITAB,442,ANINFO_BLIND_1,
     .            NOM_OPT)
C------------------------------------
C     tag des noeuds SECONDARYs rby avec gravite ou load/centri
C     pour calcul du travail des forces externes
C-------------------------------------
      DO I=1,NUMNOD
        ITAG(I)=0
      ENDDO
      K=0
      DO N=1,NRBYKIN
        NSL=NPBY(2,N)
        IF(NPBY(7,N)/=0)THEN
         DO I=1,NSL
          ITAG(LPBY(I+K))=1
         ENDDO
        ENDIF
        K=K+NSL
      ENDDO
C
      DO K=1,NGRAV
        NN =IGRV(1,K)
        IAD=IGRV(4,K)
        DO I=1,NN
          N=IBGR(I+IAD-1)
          IF(ITAG(N) == 1)IBGR(I+IAD-1) = -N
        ENDDO
      ENDDO
C
      DO K=1,NLOADC
	NN   = ICFIELD(1,K) 
        IAD  = ICFIELD(4,K)
        DO I=1,NN
         N=LCFIELD(IAD+I-1)
         IF(ITAG(N) == 1)LCFIELD(IAD+I-1) = -N
        END DO
      ENDDO 
C------------------------------------
      IF(ALLOCATED(ITAG)) DEALLOCATE(ITAG)
      IF(ALLOCATED(TABSL))DEALLOCATE(TABSL)
C------------------------------------
      RETURN
C
1000  FORMAT(/
     . '      RIGID BODY DEFINITIONS '/
     . '      ---------------------- '/)
1100  FORMAT( /5X,'RIGID BODY ID ',I10,1X,A
     .       /10X,'SENSOR                                  ',I10
     .       /10X,'PRIMARY NODE                            ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'SURFACE LINKED TO BODY                  ',I10
     .       /10X,'SPHERICAL INERTIA FLAG                  ',I10)
1102  FORMAT( /5X,'RIGID BODY ID ',I10,1X,A
     .       /10X,'SENSOR                                  ',I10
     .       /10X,'PRIMARY NODE                            ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'SURFACE LINKED TO BODY                  ',I10
     .       /10X,'SPHERICAL INERTIA FLAG                  ',I10)
1103  FORMAT( /10X,'NO AMS EXPANSION OVERALL THE RBODY      ')
1111  FORMAT( /5X,'RIGID BODY ID ',I10,1X,A
     .       /10X,'PRIMARY NODE                            ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'SURFACE LINKED TO BODY                  ',I10
     .       /10X,'SKEW NUMBER                             ',I10
     .       /10X,'SPHERICAL INERTIA FLAG                  ',I10
     .       /10X,'REMOVE SECONDARY NODES FROM RIGID WALL(IF=0)',I10
     .       /10X,'CENTER OF MASS FLAG                     ',I10
     .       /10X,'ADDED MASS                              ',1PG20.4
     .       /10X,'ADDED INERTIA                           ',1P6G20.4)
1112  FORMAT( /5X,'RIGID BODY ID ',I10,1X,A
     .       /10X,'PRIMARY NODE                            ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'SURFACE LINKED TO BODY                  ',I10
     .       /10X,'SKEW NUMBER                             ',I10
     .       /10X,'SPHERICAL INERTIA FLAG                  ',I10
     .       /10X,'REMOVE SECONDARY NODES FROM RIGID WALL(IF=0)',I10
     .       /10X,'CENTER OF MASS FLAG                     ',I10
     .       /10X,'ADDED MASS                              ',1PG20.4
     .       /10X,'ADDED INERTIA                           ',1P6G20.4)
1101  FORMAT(/10X,'RIGID BODIES SAVED FOR TIME HISTORY',/)
1151  FORMAT(/10X,'FAILURE CRITERIA : ')
1152  FORMAT(/10X,'NORMAL FORCE AT FAILURE. . . . . . . . . . . . .',1PG20.4
     .       /10X,'FAILURE EXPONENT PARAMETER IN NORMAL DIRECTION  ',1PG20.4
     .       /10X,'SHEAR FORCE AT FAILURE . . . . . . . . . . . . .',1PG20.4
     .       /10X,'FAILURE EXPONENT PARAMETER IN SHEAR DIRECTION   ',1PG20.4)
1201  FORMAT(/10X,'SECONDARY NODES ')
1202  FORMAT( 10X,10I10)
      END SUBROUTINE HM_READ_RBODY
C
Chd|====================================================================
Chd|  SETRBYON                      source/constraints/general/rbody/hm_read_rbody.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SETRBYON(IXS     ,IXC      ,IXTG   ,IGRNOD  ,IGRNRBY ,
     2                    ISOLOFF  ,ISHEOFF ,ITRIOFF,KNOD2ELS,KNOD2ELC,
     3                    KNOD2ELTG,NOD2ELS ,NOD2ELC,NOD2ELTG,IXQ     ,
     4                    IQUAOFF  ,KNOD2ELQ,NOD2ELQ,LSUBMODEL)
C-------------------------------------
C    PRE LECTURE STRUCTURE RIGIDES POUR OPTIMIZATION
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGRNRBY(*),ISOLOFF(*),ISHEOFF(*),ITRIOFF(*),
     .        IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .        KNOD2ELC(*), KNOD2ELTG(*), NOD2ELC(*), NOD2ELTG(*),
     .        KNOD2ELS(*), NOD2ELS(*),KNOD2ELQ(*),IQUAOFF(*),
     .        NOD2ELQ(*) ,IXQ(NIXQ,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, ISENS, IG, NSN, II, NALL,
     .        IGU, N, ID, IRBYON, IOPT, NN, JJ, NRB
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG
      CHARACTER MESS*40,TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------
C
      DO I = 1, NUMELS
        ISOLOFF(I) = 0
      END DO
      DO I = 1, NUMELC
        ISHEOFF(I) = 0
      END DO
      DO I = 1, NUMELTG
        ITRIOFF(I) = 0
      END DO
      DO I = 1, NUMELQ
        IQUAOFF(I) = 0
      END DO
C
C init a 0 de itag
      CALL MY_ALLOC(ITAG,NUMNOD) 
      DO I=1,NUMNOD
        ITAG(I)=0
      ENDDO
C--------------------------------------------------
C START BROWSING MODEL RBODY
C--------------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RBODY')
C
      NRB=0
C
      DO N=1,NRBYKIN
        NRB = NRB + 1
        IF (NSUBDOM > 0)THEN        !   TAGRBY is allocated only if NSUBDOM>0
            IF(TAGRBY(NRB) == 0) CALL HM_SZ_R2R(TAGRBY,NRB,LSUBMODEL)
        ENDIF
C-----------------------------------------------------------------
        IGRNRBY(N)=0
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR)
C
        CALL HM_GET_INTV('sens_ID',ISENS,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('grnd_ID',IGU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Ioptoff',IOPT,IS_AVAILABLE,LSUBMODEL)
C
        IF (IOPT == 1) THEN
          IRBYON=2
        ELSE
C par defaut rbody desactive
          IRBYON=1
        END IF
C si sensor rbody active
        IF(ISENS/=0) IRBYON=0
C si Imls utilisee rbody active temporairement
        IF(NDSOLV == 1) IRBYON=0
        IF(IRBYON>=1)THEN
C
          IF(IGU/=0)THEN
            IG = 0
            DO I=1,NGRNOD
              IF(IGRNOD(I)%ID == IGU)THEN
                IG=I
                GOTO 100
              END IF
            END DO
 100        CONTINUE
C
            IF(IG/=0) THEN
              IGRNRBY(N)=IG
              NSN = IGRNOD(IG)%NENTITY
              DO I=1,NSN
                ITAG(IGRNOD(IG)%ENTITY(I)) = 1
              END DO
C
cc              DO II = 1, NUMELS
              DO I=1,NSN
                NN = IGRNOD(IG)%ENTITY(I)
                DO JJ = KNOD2ELS(NN)+1,KNOD2ELS(NN+1)
                  II = NOD2ELS(JJ)
                  NALL = ITAG(IXS(2,II)) * ITAG(IXS(3,II)) *
     +                   ITAG(IXS(4,II)) * ITAG(IXS(5,II)) *
     +                   ITAG(IXS(6,II)) * ITAG(IXS(7,II)) *
     +                   ITAG(IXS(8,II)) * ITAG(IXS(9,II))
                  IF(NALL/=0)THEN
                    ISOLOFF(II) = IRBYON
                  END IF
                END DO
C
cc              DO II = 1, NUMELC
                DO JJ = KNOD2ELC(NN)+1,KNOD2ELC(NN+1)
                  II = NOD2ELC(JJ)
                  NALL = ITAG(IXC(2,II)) * ITAG(IXC(3,II)) *
     +                   ITAG(IXC(4,II)) * ITAG(IXC(5,II))
                  IF(NALL/=0)THEN
                    ISHEOFF(II) = IRBYON
                  END IF
                END DO
C
cc              DO II = 1, NUMELTG
                DO JJ = KNOD2ELTG(NN)+1,KNOD2ELTG(NN+1)
                  II = NOD2ELTG(JJ)
                  NALL = ITAG(IXTG(2,II)) * ITAG(IXTG(3,II)) *
     +                   ITAG(IXTG(4,II))
                  IF(NALL/=0)THEN
                    ITRIOFF(II) = IRBYON
                  END IF
                END DO
C
                DO JJ = KNOD2ELQ(NN)+1,KNOD2ELQ(NN+1)
                  II = NOD2ELQ(JJ)
                  NALL = ITAG(IXQ(2,II)) * ITAG(IXQ(3,II)) *
     +                   ITAG(IXQ(4,II)) * ITAG(IXQ(5,II))
                  IF(NALL/=0)THEN
                    IQUAOFF(II) = IRBYON
                  END IF
                END DO
C
              END DO
C reinit a 0 sur la partie concernee
              DO I=1,NSN
                ITAG(IGRNOD(IG)%ENTITY(I))=0
              END DO
            END IF
          END IF
        END IF
C
      END DO
C
      IF(ALLOCATED(ITAG)) DEALLOCATE(ITAG)
C
      RETURN
      END SUBROUTINE SETRBYON
C
Chd|====================================================================
Chd|  SETELOFF                      source/constraints/general/rbody/hm_read_rbody.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SETELOFF(IXS    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                    IXTG   ,IPARG  ,        ISOLOFF,ISHEOFF,
     3                    ITRUOFF,IPOUOFF,IRESOFF,ITRIOFF,IGRNRBY,
     4                    IGRNOD ,ELBUF_STR,IQUAOFF,IXQ  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
C-------------------------------------
C    PRE LECTURE STRUCTURE RIGIDES POUR OPTIMIZATION
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISOLOFF(*), ISHEOFF(*), ITRIOFF(*),ITRUOFF(*),
     .        IPOUOFF(*), IRESOFF(*),
     .        IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*),
     .        IPARG(NPARG,*),IGRNRBY(*),
     .        IQUAOFF(*),IXQ(NIXQ,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_STR
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, MLW, ITY, NEL, NFT, IAD, I, II, IGOF, NR, IG,
     .        NSN, NALL, ISHFT, IOK, IRBYON
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG
C-----------------------
C     MISE DE OFF A -OFF
C======================================================================|
      IF(IPRI>=5) THEN
        WRITE(IOUT,*)' '

        WRITE(IOUT,*)' LIST OF DEACTIVATED ELEMENTS FROM RIGID BODIES'
        WRITE(IOUT,*)' ----------------------------------------------'
      END IF
C
      IRBYON = 1
C
      CALL MY_ALLOC(ITAG,NUMNOD) 
C init initiale sur numnod
      DO I=1,NUMNOD
        ITAG(I)=0
      ENDDO
C
      DO NR = 1, NRBODY
        IG = IGRNRBY(NR)
        IF(IG > 0)THEN
          NSN = IGRNOD(IG)%NENTITY
          DO I=1,NSN
           ITAG(IGRNOD(IG)%ENTITY(I))=1
          END DO
C
          DO II = 1, NUMELT
            NALL = ITAG(IXT(2,II)) * ITAG(IXT(3,II))
            IF(NALL/=0)THEN
              ITRUOFF(II) = IRBYON
            END IF
          END DO
C
          DO II = 1, NUMELP
            NALL = ITAG(IXP(2,II)) * ITAG(IXP(3,II))
            IF(NALL/=0)THEN
              IPOUOFF(II) = IRBYON
            END IF
          END DO
C
          DO II = 1, NUMELR
            NALL = ITAG(IXR(2,II)) * ITAG(IXR(3,II))
            IF(NALL/=0)THEN
              IRESOFF(II) = IRBYON
            END IF
          END DO
C
C reinit a 0 sur la partie concernee uniquement
          DO I=1,NSN
            ITAG(IGRNOD(IG)%ENTITY(I))=0
          END DO
        END IF
      END DO
C
C     IF COND
        DO NG=1,NGROUP
          GBUF => ELBUF_STR(NG)%GBUF
          MLW=IPARG(1,NG)
          ITY=IPARG(5,NG)
          NEL=IPARG(2,NG)
          NFT=IPARG(3,NG)
          IAD=IPARG(4,NG) - 1
C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
         IF(ITY == 1.AND.MLW/=0)THEN        ! loi0, pas de off
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ISOLOFF(II)/=0)THEN
               GBUF%OFF(I)= -ABS(GBUF%OFF(I))
               IF(IPRI>=5) WRITE(IOUT,*)' BRICK DEACTIVATION:',
     .         IXS(11,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK == 1)THEN
             IGOF = 1
             DO I = 1,NEL
               II=I+NFT
               IF (GBUF%OFF(I) > ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     2. ELEMENTS QUADS
C-----------------------
         ELSEIF(ITY == 2.AND.MLW/=0)THEN        ! loi0, pas de off
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(IQUAOFF(II)/=0)THEN
               GBUF%OFF(I)= -ABS(GBUF%OFF(I))
               IF(IPRI>=5) WRITE(IOUT,*)' QUAD DEACTIVATION:',
     .         IXQ(NIXQ,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK == 1)THEN
             IGOF = 1
             DO I = 1,NEL
               II=I+NFT
               IF (GBUF%OFF(I) > ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
         ELSEIF(ITY == 3.AND.MLW/=0)THEN       ! loi0, pas de off
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ISHEOFF(II)/=0)THEN
               IF (GBUF%OFF(I) > ZERO)THEN
                 GBUF%OFF(I) = -GBUF%OFF(I)
                 IF(IPRI>=5) WRITE(IOUT,*)' SHELL DEACTIVATION:',
     .           IXC(7,II)
                 IOK = 1
               ENDIF
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK == 1)THEN
             IGOF = 1
             DO I = 1,NEL
               II=I+NFT
               IF (GBUF%OFF(I) > ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     4. ELEMENTS TRUSS
C-----------------------
         ELSEIF(ITY == 4)THEN
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ITRUOFF(II)/=0)THEN
               GBUF%OFF(I)= -ABS(GBUF%OFF(I))
               IF(IPRI>=5) WRITE(IOUT,*)' TRUSS DEACTIVATION:',
     .         IXT(5,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
C Incompatible avec option de gap dans propriete de truss
C        IGOF = 1
C        DO I = 1,NEL
C         IF(ELBUF(IAD + I)/=ZERO) IGOF=0
C        ENDDO
C        IPARG(8,NG) = IGOF
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
         ELSEIF(ITY == 5)THEN
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(IPOUOFF(II)/=0)THEN
               GBUF%OFF(I)= -ABS(GBUF%OFF(I))
               IF(IPRI>=5) WRITE(IOUT,*)' BEAM DEACTIVATION:',
     .         IXP(6,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK == 1)THEN
             IGOF = 1
             DO I = 1,NEL
               IF(GBUF%OFF(I) > ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
         ELSEIF(ITY == 6.AND.MLW/=3)THEN
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(IRESOFF(II)/=0)THEN
               IF (GBUF%OFF(I) /= -TEN) GBUF%OFF(I) = -ABS(GBUF%OFF(I))
C             spring is active
               IF(IPRI>=5) WRITE(IOUT,*)' SPRING DEACTIVATION:',
     .         IXR(6,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK == 1)THEN
             IGOF = 1
             DO I = 1,NEL
               IF(GBUF%OFF(I)/=ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
         ELSEIF(ITY == 7.AND.MLW/=0)THEN       ! loi0, pas de off
           ISHFT=16
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ITRIOFF(II)/=0)THEN
               GBUF%OFF(I)= -ABS(GBUF%OFF(I))
               IF(IPRI>=5) WRITE(IOUT,*)' SH_3N DEACTIVATION:',
     .         IXTG(6,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK == 1)THEN
             IGOF = 1
             DO I = 1,NEL
               II=I+NFT
               IF (GBUF%OFF(I) > ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C----------------------------------------
         ENDIF
        ENDDO
C-----------
      IF(ALLOCATED(ITAG)) DEALLOCATE(ITAG)
C
      RETURN
      END SUBROUTINE SETELOFF

