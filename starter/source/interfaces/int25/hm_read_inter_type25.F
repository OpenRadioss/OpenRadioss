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
Chd|  HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        INTER_DCOD_FRICTION           source/interfaces/reader/inter_dcod_friction.F
Chd|        INTER_DCOD_FUNCTION           source/interfaces/reader/inter_dcod_function.F
Chd|        INTER_DCOD_SENSOR             source/interfaces/reader/inter_dcod_sensor.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE25(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,
     2        IGRNOD     ,IGRSURF    ,XFILTR     ,FRIC_P    ,DEF_INTER  ,
     3        LSENSOR    ,NOM_OPT    ,UNITAB    ,LSUBMODEL  ,TITR       ,
     4        NOM_OPTFRIC,INTBUF_FRIC_TAB,NPC   ,NPC1       ,TF         )
C============================================================================
C     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*), NOM_OPTFRIC(LNOPT1,*)
      INTEGER ISU1,ISU2,NI
      INTEGER IPARI(*), DEF_INTER(*),LSENSOR(NSENSI,*),NPC(*),NPC1(*)
      my_real
     .   STFAC,XFILTR
      my_real
     .   FRIGAP(*),FRIC_P(*),TF(*)
      CHARACTER TITR*nchartitle
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "intstamp_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "fluid_scr_c.inc"
#include      "scr12_c.inc"
#include      "remesh_c.inc"
#include      "sysunit.inc"
#include      "thknod_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IBC1, IBC2, IBC3, IBUC, NOINT, NTYP,INACTI,
     .    IBC1M, IBC2M, IBC3M, IGSTI, IVIS2,IS1, IS2,ILEV,
     .    IGAP,MULTIMP,MFROT,IFQ,IBAG,MODFR,ISYM,IEDGE,NOD1,
     .    IDEL25,KK,IGAP0,FLAGREMNOD,IDSENS,IDELKEEP,INTKG,
     .    IREM25I2,IPRINT,INTFRIC,MULTIMPE,IASSIGN,ISHARP,
     .    ITHK,NOTUSED,INTTH,IFORM,IKTHE,FCOND,IFRICTH,NPREF
      my_real
     .    FRIC,GAP,STARTT,BUMULT,STOPT,C1,C2,C3,C4,C5,C6,ALPHA,
     .    GAPSCALE,STMIN,STMAX,EDG_ANGL,VISC,VISCF,PERCENT_SIZE,
     .    GAPMAX_S,GAPMAX_M,PENMAX,PENMIN,SIGMAXADH,VISCADHFACT,
     .    VISCFLUID,PMAX,KTHE,TINT,XTHE,FRAD,DRAD,FHEATS,
     .    FHEATM,DCOND,XTHE_UNIT,TPFIT
      CHARACTER KEY1*ncharkey,BCFLAG*ncharfield
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C-----------------------------------------------
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE25
C=======================================================================

C Initializations
      IS1=0
      IS2=0
      IBC1=0
      IBC2=0
      IBC3=0
      IBC1M=0
      IBC2M=0
      IBC3M=0
      IBUC=0
      MFROT=0
      IFQ=0
      IBAG=0
      IGSTI = 0
      ILEV=0
      IDSENS = 0
      IDELKEEP=0
      INTFRIC = 0
      NOD1 = 0
      INTKG = 0
      FLAGREMNOD = 0
      MULTIMP  = 0
      IREM25I2 = 0
      IGAP0 = 0
      INTTH = 0
      IKTHE = 0
      IFORM = 0
      FCOND = 0
      IFRICTH = 0

C-------for the moment
C     IGSTI = 0
      MULTIMPE = 0
C
      STOPT=EP30
      INACTI = 0
      VISCF = ZERO
      FRIC = ZERO
      GAP = ZERO
      GAPSCALE = ZERO
      STARTT = ZERO
      VISC = ZERO
      XFILTR = ZERO
      DO I = 1, 10
          FRIC_P(I) = ZERO
      ENDDO
      C1=ZERO
      C2=ZERO
      C3=ZERO
      C4=ZERO
      C5=ZERO
      C6=ZERO

      GAPSCALE = ZERO
      PERCENT_SIZE = ZERO
      EDG_ANGL = ZERO
      GAPMAX_S = ZERO
      GAPMAX_M = ZERO
      PENMAX = ZERO
      PENMIN = ZERO
      VISCFLUID = ZERO
      SIGMAXADH = ZERO
      VISCADHFACT = ZERO
      KTHE = ZERO
      XTHE = ZERO
      TINT = ZERO
      FRAD = ZERO
      DRAD = ZERO
      FHEATS= ZERO
      FHEATM= ZERO
      DCOND = ZERO

C
      IASSIGN = 1
C
      NTYP = 25
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C
      ITHK = 0
      NOTUSED = 0
      PMAX = EP30
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
C
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ithe',INTTH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IGAP',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE24_Idel',IDEL25,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Irem_i2',IREM25I2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE24_Iedge',IEDGE,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('GRNOD_ID',NOD1,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Igap0',IGAP0,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ishape',ISHARP,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Deactivate_X_BC',IBC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IBC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IBC3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IVIS2',IVIS2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ithick',ITHK,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',IDSENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Fric_ID',INTFRIC,IS_AVAILABLE,LSUBMODEL)
C
      IF(INTTH > 0 ) THEN
         CALL HM_GET_INTV('IFORM1',IFORM,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV('fct_ID_k',IKTHE,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV('F_COND',FCOND,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV('FRIC_FUN',IFRICTH,IS_AVAILABLE,LSUBMODEL)
      ENDIF
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('PrMesh_Size',PERCENT_SIZE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Gap_scale',GAPSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP1',GAPMAX_S,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP2',GAPMAX_M,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STMIN',STMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMAX',STMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TYPE24_Edge_Angle',EDG_ANGL,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STFAC',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STIFF_DC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Pmax',PMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tpressfit',TPFIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('Xfreq',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (MFROT>0) THEN
         CALL HM_GET_FLOATV('C1',C1,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C2',C2,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C3',C3,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C4',C4,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C5',C5,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
      IF (MFROT>1) THEN
        CALL HM_GET_FLOATV('C6',C6,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
      IF (IVIS2>0) THEN
         CALL HM_GET_FLOATV('C1',C1,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C2',C2,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C3',C3,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C4',C4,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('C5',C5,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
      IF(IVIS2==-1) THEN
         INTERADHESION = 1
         CALL HM_GET_FLOATV('ViscFluid',VISCFLUID,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('SigMaxAdh',SIGMAXADH,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('ViscAdhFact',VISCADHFACT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
C
      IF(INTTH > 0 ) THEN
         CALL HM_GET_FLOATV('Kthe',KTHE,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('T_Initial',TINT,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('Crx',XTHE,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('F_RAD',FRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('D_RAD',DRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('Fmax',FHEATS,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('HEAT_AL',FHEATM,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('D_COND',DCOND,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
C
C--------------------------------------------------
C DEFAULT VALUES DEFINITION 
C--------------------------------------------------

        IPRINT = 0

        KEY1='IGSTI'
        CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IGAP'
        CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IREM25I2'
        CALL DEFINTER(KEY1     ,IREM25I2,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
        KEY1='IEDGE'
        CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IBAG'
        CALL DEFINTER(KEY1     ,IBAG     ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IDEL25'
        CALL DEFINTER(KEY1     ,IDEL25  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)

        KEY1='IGAP0'
        CALL DEFINTER(KEY1     ,IGAP0    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='ISHARP'
        CALL DEFINTER(KEY1     ,ISHARP  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
        KEY1='INACTI'
        CALL DEFINTER(KEY1     ,INACTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='ITHK'
        CALL DEFINTER(KEY1     ,ITHK    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
C--------------------------------------------------
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------
C
C
C....* Card1 :flags  *.............
C
        IF(IGAP==2) THEN
           FLAGREMNOD = 2
        ENDIF
        IF (FLAGREMNOD == 0) FLAGREMNOD = 1

C.....* Storage IPARI FRIGAP *........

        IPARI(63) = FLAGREMNOD
        IPARI(83) = IREM25I2
C         IPARI(83) is used temporarily before determined only by FLAGREMNOD   
        IF (IREM25I2 /= 1) IPARI(83)=0

        IF(IGAP==2) THEN
           IPARI(21)=1
        ELSE
           IPARI(21)=IGAP
        ENDIF

C
C....* Card2 :Gaps  *.............
C
C
C------ILEV =1 : ISU1>0,ISU2=0; IS3>=0
C            2 : ISU1>0,ISU2>0; IS3>=0
C            3 : ISU1=0,ISU2>0;IS3>0

        IF(ISU1 /= 0 .AND.ISU2 == 0)THEN
           ILEV = 1
        ELSEIF(ISU1 /= 0 .AND.ISU2 /= 0)THEN
           ILEV = 2
           IF (ISU1 == ISU2) THEN
             ILEV = 1
             ISU2 = 0
             CALL ANCMSG(MSGID=1022,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=NOINT,
     .                  C1=TITR)
           END IF
          ELSEIF(ISU1 == 0 .AND.ISU2 /= 0 .AND. NOD1 /= 0) THEN
           ILEV = 3
          ELSE
           IF(ISU2 == 0) THEN
             CALL ANCMSG(MSGID=119,
     .                   ANMODE=ANINFO,
     .                   MSGTYPE=MSGERROR,
     .                   I1=NOINT,
     .                   C1=TITR)
           ELSEIF (NOD1 == 0) THEN
             CALL ANCMSG(MSGID=1001,
     .                   ANMODE=ANINFO,
     .                   MSGTYPE=MSGERROR,
     .                   I1=NOINT,
     .                   C1=TITR)
           ENDIF
           IS1=0
           IS2=0
        END IF

        ISYM = 1

        SELECT CASE (ILEV)
          CASE(1)
            INGR2USR => IGRSURF(1:NSURF)%ID
            ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
            ISU2=ISU1
            IS1=-1
            IS2=-3
          CASE(2)
C----------------if we copy int7--
            IS1 = 0
            INGR2USR => IGRSURF(1:NSURF)%ID
            IS1=NGR2USR(ISU1,INGR2USR,NSURF)
            IF (IS1==0) THEN
              CALL ANCMSG(MSGID=1000,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    MSGTYPE=MSGERROR,
     .                    I1=NOINT,
     .                    C1=TITR)
            ELSE
             ISU1=IS1
            END IF
            INGR2USR => IGRSURF(1:NSURF)%ID
            ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
            IS1=-1
            IS2=-1
          CASE(3)
            INGR2USR => IGRSURF(1:NSURF)%ID
            ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
           IS1=-2
           IS2=-1
        END SELECT
        INGR2USR => IGRNOD(1:NGRNOD)%ID
        IF(NOD1 /= 0) NOD1=NGR2USR(NOD1,INGR2USR,NGRNOD)

        IPARI(13)=IS1*10+IS2
        IPARI(45)=ISU1
        IPARI(46)=ISU2

        IPARI(26)=NOD1
        IPARI(43)=ISYM
        IPARI(58)=IEDGE
        IF(IEDGE == 1000) IPARI(58)=0

C-------tempo: Isurf <- ILEV=IPARI(20) -----
        IPARI(20)=ILEV
        IF (INACTI==-1.AND.(IGSTI==0.OR.IGSTI==1000)) IGSTI = 4
        IPARI(34)=IGSTI

C
C....* Card4  *.............
C
C
        IF(IGSTI>1)THEN
            I7STIFS=1
            IF(STMAX==ZERO)STMAX=EP30
        ELSE
            IF(STMAX==ZERO)STMAX=EP30
        END IF


C-----------  GAPMAX->GAPMAX_S: EP30
C-----------IGAP0: used only for starter;sharing places w/ int20      
        IPARI(53)=IGAP0
c       FRIGAP(16) = EP30
        FRIGAP(17) = STMIN
        FRIGAP(18) = STMAX
C
C-------- default EDG_ANGL=135         
        IF(IEDGE /= 0 .AND. EDG_ANGL==ZERO)
     +           EDG_ANGL=HUNDRED+THIRTY+FIVE
        FRIGAP(26)  = COS((HUNDRED80-EDG_ANGL)*PI/HUNDRED80)

        IF(IGAP == 3 .AND. PERCENT_SIZE == ZERO) PERCENT_SIZE = FOUR*EM01
        FRIGAP(28) = PERCENT_SIZE

	IF(GAPSCALE==ZERO)GAPSCALE=ONE
	FRIGAP(13) = GAPSCALE ! historically stored, get modified in I25STI3 (GAPMIN)
        FRIGAP(45) = GAPSCALE


        IF(GAPMAX_S==ZERO)GAPMAX_S=EP30
        IF(GAPMAX_M==ZERO)GAPMAX_M=EP30
C       IF(PENMAX==ZERO)PENMAX=EP30
        IF(PENMIN==ZERO)PENMIN=EM8
	FRIGAP(35) = GAPMAX_S
        FRIGAP(36) = GAPMAX_M
        FRIGAP(37) = PENMAX
        FRIGAP(38) = PENMIN
C
C....* Card4   *.............
C
C
C
        GAP=ZERO
        IF (STOPT == ZERO) STOPT = EP30
C
        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT

C
C....* Card5   *.............
C
C ----- presfit    
          IF (INACTI==-1.AND.IIMPLICIT==0) THEN
           NPREF=10000
           IPARI(40) = NPREF
          ELSE
           TPFIT=ZERO
          END IF

C Hidden flag no more read using HM reader
c        IF (BCFLAGM(LFIELD-2:LFIELD-2)== '1') IBC1M = 1
c        IF (BCFLAGM(LFIELD-1:LFIELD-1)== '1') IBC2M = 1
c        IF (BCFLAGM(LFIELD  :LFIELD  )== '1') IBC3M = 1

        IF (IIMPLICIT>0.AND.INACTI==0) INACTI=-1
        IF (INACTI/=0.AND.INACTI/=1.AND.INACTI/=-1.AND.INACTI/=3
     .      .AND.INACTI/=5) THEN
             IF(INACTI==6)THEN
               CALL ANCMSG(MSGID=1559,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=NOINT,I2=INACTI,
     .                  C1=TITR)
               INACTI=5
             ELSE
               CALL ANCMSG(MSGID=1558,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=NOINT,I2=INACTI,
     .                  C1=TITR)
               INACTI = 0
             END IF
        END IF
        IF(STFAC==0.) THEN
           STFAC=ONE
        ENDIF
        IF(IGSTI==1)STFAC=-STFAC
C------no use of  VISCF
        VISCF=ZERO
c        FRIGAP(15)=VISCF**2
C-------used for TPFIT
        FRIGAP(15)=TPFIT

        IF(VISC==ZERO)THEN
            IF(IVIS2==5)THEN
              VISC=ONE
            ELSE
              VISC=FIVEEM2
            ENDIF
        ENDIF
        FRIGAP(14)=VISC
C-------MAIN side gap changes with thickness change
        IF(ITHK==1) ITHKNOD=1
C
C-------* Storage IPARI FRIGAP *----------------------------
C
C         default w=> harmonic mass, IVIS2=6 <=> SECONDARY node mass
C         IVIS2=-1 interface adhesion
        IF(IVIS2==0)IVIS2=1
        IF (IDEL25 < 0) THEN
            IDELKEEP=1
            IDEL25=ABS(IDEL25)
        END IF
        IPARI(61)=IDELKEEP
        IF(IDEL25>2) IDEL25 = 0
        IPARI(17)=IDEL25
C
        IF(IGAP==3.AND.ISHARP==1) THEN
            CALL ANCMSG(MSGID=1619,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=NOINT,I2=ISHARP,I3=IGAP,
     .                  C1=TITR)
            ISHARP=2
        END IF
        IPARI(84)=ISHARP
C
        IPARI(14)=IVIS2
        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)

        IPARI(22)=INACTI
        IPARI(91)=ITHK

C------------remove BUMULT   
        BUMULT=ZERO

        IF(BUMULT==ZERO) THEN
          BUMULT = BMUL0
C bmult augmente pour les tres gros modeles et interface 7 ou 20

            IF(NUMNOD > 2500000) THEN
              BUMULT = BMUL0*TWO
            ELSEIF(NUMNOD > 1500000) THEN
              BUMULT = BMUL0*THREE/TWO
            END IF
        END IF
        FRIGAP(4)=BUMULT


C
C....* Card6 : Friction  *.............
C
C           IF (MFROT/=0.AND.VISCF==0.0) VISCF=ONE
          MODFR = 2
          IF (ALPHA==0.) IFQ = 0
          IF (MODFR==0) MODFR = 1
          IF (MODFR==2.AND.IFQ<10) IFQ = IFQ + 10
C           IF(MODFR==2)VISCF=ZERO
          IF (IFQ>0) THEN
             IF (IFQ==10) XFILTR = ONE
             IF (MOD(IFQ,10)==1) XFILTR = ALPHA
             IF (MOD(IFQ,10)==2) XFILTR=FOUR*ATAN2(ONE,ZERO) / ALPHA
             IF (MOD(IFQ,10)==3) XFILTR=FOUR*ATAN2(ONE,ZERO) * ALPHA
             IF (XFILTR<ZERO) THEN
               CALL ANCMSG(MSGID=554,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=NOINT,
     .                     C1=TITR,
     .                     R1=ALPHA)
             ELSEIF (XFILTR>1.AND.MOD(IFQ,10)<=2) THEN
                CALL ANCMSG(MSGID=554,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=NOINT,
     .                      C1=TITR,
     .                      R1=ALPHA)
             ENDIF
          ELSE
            XFILTR = ZERO
          ENDIF

          IF (IBAG/=0.AND.NVOLU==0 .AND. IALELAG == 0) THEN
             CALL ANCMSG(MSGID=614,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IBAG=0
          ENDIF
          INTBAG = MAX(INTBAG,IBAG)
          KCONTACT =MAX(KCONTACT,IBAG)

C
          IPARI(30) = MFROT
          IPARI(31) = IFQ
          IPARI(32) = IBAG
          IPARI(72) =  INTFRIC
          IPARI(64) = IDSENS
C
C....* OPTIONAL Card7 :C1...C6 friction data    *.............
C

        FRIC_P(1) = C1
        FRIC_P(2) = C2
        FRIC_P(3) = C3
        FRIC_P(4) = C4
        FRIC_P(5) = C5
        FRIC_P(6) = C6

C
C....* Option Card8 :Adhesion data    *.............
C
        FRIGAP(42) = VISCFLUID
        FRIGAP(43) = SIGMAXADH
        FRIGAP(44) = VISCADHFACT
C-----
        IPARI(12)=IBUC
        IPARI(65) = INTKG

C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)

C       POUR STOCKER LE FLAG D'INACTIVATION DES PENE INITIALES
C       (0 = RIEN , 1 = NOEUDS , 2 = FACETTES )
        MULTIMP = 4
        IPARI(23)=MULTIMP

        IF(IEDGE /= 0) MULTIMPE = 4
        IPARI(87)=MULTIMPE ! MAIN shell edges
        IPARI(89)=MULTIMPE ! MAIN solid edges
C
C
C.....* Card7 : Thermal input *.........
C

        IF(INTTH > 0.AND.KTHE == ZERO)THEN
           IF(IKTHE /= 0)THEN
              KTHE = ONE
            ENDIF
        ENDIF
C
        IF (INTTH > 0.AND.XTHE == ZERO) THEN
        !units
          CALL HM_GET_FLOATV_DIM('Crx' ,XTHE_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XTHE = ONE * XTHE_UNIT
        ENDIF
C
        IF(INTTH > 0 ) INTHEAT = 1
        IPARI(47) = INTTH

        IF(ITHERM_FE == 0 .AND. INTTH > 0 ) THEN
           INTHEAT = 0
           IPARI(47) = 0
           CALL ANCMSG(MSGID=702,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO,
     .                 I1=NOINT,
     .                 C1=TITR)
        ENDIF
C
        IF(INTTH > 0 ) THEN
            IF(FHEATS+FHEATM > 1) THEN
               CALL ANCMSG(MSGID=1064,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR,
     .                   R1=FHEATS,
     .                   R2=FHEATM)
            ENDIF
        ENDIF
C
        IF(FCOND ==0) DCOND = ZERO

        IF(FRAD==ZERO ) DRAD = ZERO

        IF(FCOND /= 0.AND.DCOND/=ZERO.AND.DRAD==ZERO) THEN
          DRAD = DCOND
          CALL ANCMSG(MSGID=1810,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=NOINT,
     .                C1=TITR,
     .                R1=DCOND,
     .                R2=DRAD)
        ENDIF

        IF(DCOND > DRAD) THEN
          DCOND = DRAD
          CALL ANCMSG(MSGID=1809,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=NOINT,
     .                C1=TITR,
     .                R1=DCOND,
     .                R2=DRAD)
        ENDIF

        IF(INTTH > 0.AND.FRIC == ZERO)THEN
             IF(IFRICTH /= 0)THEN
              FRIC = ONE
             ENDIF
        ENDIF

        FRIGAP(20)  = KTHE
        FRIGAP(21 ) = FHEATS
        FRIGAP(22 ) = TINT
        FRIGAP(41 ) = FHEATM
        FRIGAP(31 ) = FRAD
        FRIGAP(32 ) = DRAD
        IPARI(48)   = IFORM
        IPARI(92)   = IKTHE
        FRIGAP(33)  = XTHE
        IPARI(93)   = FCOND
        FRIGAP(34)  = DCOND
        IPARI(50)   = IFRICTH

C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS AND SENSOR - USER TO INTERNAL ID
C------------------------------------------------------------
C
        CALL INTER_DCOD_FUNCTION(NTYP,NI,IPARI,NPC1,NOM_OPT,NPC,TF)
        CALL INTER_DCOD_SENSOR (NTYP,NI,IPARI,NOM_OPT,LSENSOR)
        CALL INTER_DCOD_FRICTION(NTYP,NI,IPARI,NOM_OPT,NOM_OPTFRIC,
     .                INTBUF_FRIC_TAB)

C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------

C........* PRINT DEFAULT VALUES *.....................
C
      IPRINT = 1
C
        KEY1='IGSTI'
        CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IGAP'
        CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IREM25I2'
        CALL DEFINTER(KEY1     ,IREM25I2,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IEDGE'
        CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IBAG'
        CALL DEFINTER(KEY1     ,IBAG     ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IDEL25'
        CALL DEFINTER(KEY1     ,IDEL25  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IGAP0'
        CALL DEFINTER(KEY1     ,IGAP0    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='ISHARP'
        CALL DEFINTER(KEY1     ,ISHARP  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='INACTI'
        CALL DEFINTER(KEY1     ,INACTI  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='ITHK'
        CALL DEFINTER(KEY1     ,ITHK    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C

C........* PRINT INTERFACES INPUT *.....................

        IF(IDSENS/=0) THEN
         WRITE(IOUT,3526)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 IGSTI,STFAC,ILEV,IEDGE,EDG_ANGL,
     .                 STMIN,STMAX,
     .                 FRIC,IGAP,GAPSCALE,GAPMAX_S,GAPMAX_M,PERCENT_SIZE,IGAP0,ISHARP,
     .                 IREM25I2,IDSENS,
     .                 INACTI,VISC,IPARI(14),FLAGREMNOD
        ELSE
         WRITE(IOUT,3527)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 IGSTI,STFAC,ILEV,IEDGE,EDG_ANGL,
     .                 STMIN,STMAX,
     .                 FRIC,IGAP,GAPSCALE,GAPMAX_S,GAPMAX_M,PERCENT_SIZE,IGAP0,ISHARP,
     .                 IREM25I2,STARTT,STOPT,
     .                 INACTI,VISC,IPARI(14),FLAGREMNOD
        ENDIF

        IF(TPFIT > ZERO) WRITE(IOUT,5040) TPFIT
        IF(IDEL25/=3) THEN
          WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',
     . ' (1:YES-ALL/2:YES-ANY/1000:NO) : ',IDEL25
          IF(IDELKEEP == 1)THEN
            WRITE(IOUT,'(A)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
          ENDIF
        ENDIF
C
        IF(INTFRIC > 0 ) THEN
          WRITE(IOUT,5030) INTFRIC
        ELSE
          WRITE(IOUT,1520)MOD(IFQ,10), XFILTR
          IF(MFROT==0)THEN
             IF(IFRICTH ==0 ) THEN
                WRITE(IOUT,1524) FRIC
              ELSE
                WRITE(IOUT,1525) IFRICTH,FRIC
              ENDIF
          ELSEIF(MFROT==1)THEN
            WRITE(IOUT,1515)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .      FRIC_P(4),FRIC_P(5)
          ELSEIF(MFROT==2)THEN
            WRITE(IOUT,1522)FRIC,FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .      FRIC_P(4),FRIC_P(5),FRIC_P(6)
          ELSEIF(MFROT==3)THEN
            WRITE(IOUT,1523)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .      FRIC_P(4),FRIC_P(5),FRIC_P(6)
          ENDIF
        ENDIF
C Interface Adhesion 
        IF(IVIS2==-1) THEN
          WRITE(IOUT,5050)VISCFLUID,SIGMAXADH,VISCADHFACT
        ENDIF
       IF(INTTH > 0 )THEN
         IF(IKTHE==0)THEN
           WRITE(IOUT,2102) KTHE,TINT,FRAD,DRAD,FHEATS,FHEATM,IFORM,FCOND,DCOND
         ELSE
           WRITE(IOUT,2103) IKTHE,XTHE,KTHE,TINT,FRAD,DRAD,FHEATS,FHEATM,IFORM,FCOND,DCOND
         END IF
       END IF

C--------------------------------------------------------------
      IF(IS1==0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT'
      ELSEIF(IS1==1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1==2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
      ELSEIF(IS1==3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1==4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS'
      ELSEIF(IS1==5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS'
      ENDIF
      IF(IS2==0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT'
      ELSEIF(IS2==1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2==2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES'
      ELSEIF(IS2==3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2==4)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE REFERS ',
     .                      'TO HYPER-ELLIPSOIDAL SURFACE'
      ENDIF
C
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN
 999  CALL FREERR(3)
      RETURN


 1515 FORMAT(//
     .    '    FRICTION MODEL 1 (Viscous Polynomial)'/,
     .    '     MU = MUo + C1 p + C2 v + C3 pv + C4 p^2 + C5 v^2'/,
     .    '    C1 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C2 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C3 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C4 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C5 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    TANGENTIAL PRESSURE LIMIT. . .. . . . . .',1PG20.13/)
 1522 FORMAT(/
     .    '    FRICTION MODEL 2 (Darmstad Law) :'/,
     .    '     MU = MUo+c1*exp(c2*v)*p^2+c3*exp(c4*v)*p+c5*exp(c6*v)'/,
     .    '    Muo. . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C1 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C2 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C3 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C4 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C5 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C6 . . . . . . . . . . . . . . . . . . . ',1PG20.13/)
 1523 FORMAT(/
     .    '    FRICTION MODEL 3 (Renard law) :'/,
     .    '    C1 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C2 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C3 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C4 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C5 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C6 . . . . . . . . . . . . . . . . . . . ',1PG20.13/)
 1524 FORMAT(/
     .    '    FRICTION MODEL 0 (Coulomb Law) :'/,
     .    '      FRICTION COEFFICIENT . . . . . . . . . ',1PG20.13/)
 1525   FORMAT(//
     .    '    FRICTION MODEL 0 (Coulomb Law) :'/,
     .  '   Function for FRICTION COEFFICIENT wrt TEMPERATURE',I10/,
     .      '    Ordinate scale factor on IFUNTCF .  . .  . ',1PG20.13/)
 1518 FORMAT( '    FRICTION FORMULATION: INCREMENTAL (STIFFNESS) ',
     .             'FORMULATION')
 1519 FORMAT( '    FRICTION FORMULATION: TOTAL (VISCOUS) ',
     .             'FORMULATION')
 1520 FORMAT(
     .    '    FRICTION FILTERING FLAG. . . . . . . . . ',I10/,
     .    '    FILTERING FACTOR . . . . . . . . . . . . ',1PG20.13)
C----------     
 3526 FORMAT(//
     .       '    TYPE==25  MUTI-TYPE IMPACTING             ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . . ',I5/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    CONTACT TYPES (1:S1/S1;2:S1/S2;3:N/S . . .' ,I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . ' ,I5/,
     .       '    ANGLE FOR EDGE COMPUTATION (Iedge/=0). . .',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .       '    GAP FLAG. . . . . . . . . . . . . . . . . ',I5/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM SECONDARY GAP VALUE . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM MAIN GAP VALUE. . . . . . . . . ',1PG20.13/,
     .       '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . '/,
     .       '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . ',1PG20.13/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE . . ',I5/,
     .       '    GAP SHAPE ALONG MAIN FREE EDGES . . . . ',/,
     .       '            (1:SQUARE,2:CYLINDRICAL) . . . . .',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLICTING W/ TYPE2',I10/,
     .       '    START/STOP TIMES activated by SENSOR ID '  ,I10/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE   ',I5/)
C
 3527 FORMAT(//
     .       '    TYPE==25  MUTI-TYPE IMPACTING             ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . . ',I5/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    CONTACT TYPES (1:S1/S1;2:S1/S2;3:N/S . . .' ,I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . ' ,I5/,
     .       '    ANGLE FOR EDGE COMPUTATION (Iedge/=0). . .',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .       '    GAP FLAG. . . . . . . . . . . . . . . . . ',I5/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM SECONDARY GAP VALUE . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM MAIN GAP VALUE. . . . . . . . . ',1PG20.13/,
     .       '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . '/,
     .       '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . ',1PG20.13/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE . . ',I5/,
     .       '    GAP SHAPE ALONG MAIN FREE EDGES . . . . ',/,
     .       '            (1:SQUARE,2:CYLINDRICAL). . . . . ',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLICTING W/ TYPE2',I10/,
     .       '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE   ',I5/)
 5025 FORMAT(/
     .       '    INTERFACE TYPE 25                         ' //,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I5/,
     .       '    GAP FLAG. . . . . . . . . . . . . . . . . ',I5/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE . . ',I5/,
     .       '    GAP SHAPE ALONG MAIN FREE EDGES . . . . ',/,
     .       '            (1:SQUARE,2:CYLINDRICAL) . . . . .',I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . ',I5/,
     .       '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLICTING W/ TYPE2',I10/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I5/,
     .       '    FRICTION FORMULATION. . . . . . . . . . . ',I5/)
C
 5030 FORMAT(/
     .       '    INTERFACE FRICTION MODEL.  . . . . . . . . ',I10)
 5040 FORMAT('    PRESS-FIT DURATION TIME .  . . . . . . . . ',1PG20.13/)
C
 5050 FORMAT(/
     .       '    INTERFACE ADHESION MODEL                ' /,
     .       '    INTERFACE FLUID VISCOSITY . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM ADHESIVE FORCE PER UNIT AREA. . ',1PG20.13/,
     .       '    TANGENTIAL VISCOUS FORCE SCALE FACTOR . ',1PG20.13/)
C
 2102   FORMAT(//
     .      '    Thermal interface                          ' //,
     .      '    Thermal heat exchange coefficient . . . . .',1PG20.13/,
     .      '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .      '    Radiation factor . . . . . . . . . . . . . ',1PG20.13/,
     .      '    Maximum distance for radiation computation.',1PG20.13/,
     .      '    Frictional heat transfer on SECONDARY surface . ',1PG20.13/,
     .      '    Frictional heat transfer on MAIN surface. ',1PG20.13/,
     .      '    FORMULATION CHOICE : . . . . . . . . . . . ',I10,/,
     .      '          0 : heat transfer between SECONDARY side',/,
     .      '                 and constant temperature in interface',/,
     .      '          1 : heat Exchange between pieces in contact'/
     .  '   Function for thermal heat exchange coefficient wrt distance',I10/,
     .      '    Maximum distance for conductive heat exchange',1PG20.13)
 2103   FORMAT(//
     .      '    Thermal interface                          ' //,
     .  '   Function for thermal heat exchange coefficient wrt contact pressure',I10/,
     .      '    Abscissa scale factor on IFUNTCK. . . .  . ',1PG20.13/,
     .      '    Ordinate scale factor on IFUNTCK .  . .  . ',1PG20.13/,
     .      '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .      '    Radiation factor . . . . . . . . . . . . . ',1PG20.13/,
     .      '    Maximum distance for radiation computation.',1PG20.13/,
     .      '    Frictional heat transfer on SECONDARY surface . ',1PG20.13/,
     .      '    Frictional heat transfer on MAIN surface. ',1PG20.13/,
     .      '    FORMULATION CHOICE : . . . . . . . . . . . ',I10,/,
     .      '          0 : heat transfer between SECONDARY side',/,
     .      '                 and constant temperature in interface',/,
     .      '          1 : heat Exchange between pieces in contact'/
     .  '   Function for thermal heat exchange coefficient wrt distance',I10/,
     .      '    Maximum distance for conductive heat exchange',1PG20.13)
      END
