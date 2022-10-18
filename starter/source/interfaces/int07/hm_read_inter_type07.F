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
Chd|  HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
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
      SUBROUTINE HM_READ_INTER_TYPE07(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,    
     2        IGRNOD     ,IGRSURF    ,XFILTR     ,FRIC_P    ,NOM_OPT    ,
     3        DEF_INTER  ,NPC1       ,LSENSOR    ,UNITAB    ,LSUBMODEL  ,
     4        TITR       ,NOM_OPTFRIC,INTBUF_FRIC_TAB,NPC   ,TF         ,
     4        ID_TYPE19)
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
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*), NOM_OPTFRIC(LNOPT1,*)
      INTEGER NI,NOINT,ID_TYPE19
      INTEGER IPARI(*),DEF_INTER(*),NPC1(*),LSENSOR(NSENSI,*),NPC(*)
      my_real
     .   STFAC,XFILTR
      my_real
     .   FRIGAP(*),FRIC_P(*),TF(*)
      CHARACTER TITR*nchartitle
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr12_c.inc"
#include      "remesh_c.inc"
#include      "sysunit.inc"
#include      "thknod_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IBC1, IBC2, IBC3, IBUC,  NTYP,INTTH,
     .    INACTI, IBC1M, IBC2M, IBC3M, IGSTI, IVIS2,IS1, IS2,
     .    ILEV, IGAP,MULTIMP,MFROT,IFQ,IBAG,MODFR,
     .    IEDGE,NOD1,
     .    IGAP0,FLAGREMNOD,IDSENS,IDELKEEP,INTKG,IREM7I2,IPRINT,    
     .    INTFRIC,IASSIGN,IGAP_TYPE19,IRSTH,IDEL7N,
     .    ICURV,IFRICTH,NA1,NA2,IFORM,IADM,NRADM,FIELD6,ITIED,
     .    IB1,IB2,IB3,ISU1,ISU2,ISU3,NTYP0
      my_real
     .   FRIC,GAP,STARTT,BUMULT,STOPT,C1,C2,C3,C4,C5,C6,ALPHA,
     .   GAPSCALE,GAPMAX,STMIN,STMAX,PADM, ANGLADM, CADM,
     .   PERCENT_SIZE,PENMAX,PENMIN,VISC,VISCF,FPENMAX,DTMIN,
     .   RSTH,TINT, XTHE, FRAD, DRAD,XFRICTH,FHEATS,FHEATM,
     .   EDG_ANGL,VISCFLUID,SIGMAXADH,VISCADHFACT,XTHE_UNIT
      CHARACTER KEY1*ncharkey
      LOGICAL IS_AVAILABLE
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C-----------------------------------------------
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE7
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
      IFRICTH = 0
      NOD1 = 0    
      INTKG = 0
      FLAGREMNOD = 0
      MULTIMP  = 0
      IREM7I2 = 0
      NA1 = 0
      NA2 = 0
      IRSTH = 0
      IFORM = 0
      IADM =0
      NRADM=1
      INTTH = 0
      IDEL7N = 0
      IGAP_TYPE19 = 0

C Hidden flags : no more read using HM reader 
      IVIS2 = 0
      ISU3 = 0
C
      STOPT=EP30
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
      PENMAX = ZERO
      PENMIN = ZERO
      VISCFLUID = ZERO
      SIGMAXADH = ZERO
      VISCADHFACT = ZERO
      PADM =ONE
      ANGLADM=ZERO
      XFRICTH = ONE
      RSTH = EM30
      TINT = ZERO
      XTHE = ZERO
      FRAD = ZERO
      DRAD = ZERO
      FHEATS= ZERO
      FHEATM= ZERO
      DTMIN=ZERO
C
      IASSIGN = 1
C
      NTYP = 7
      IPARI(7)= NTYP
      IPARI(15)=NOINT 
C
      IF (ID_TYPE19 == -1) THEN
C-- first type7 of type19
        NTYP = 19
        IPARI(71) = -1
      ELSEIF (ID_TYPE19 > 0) THEN
C-- symmetrical type7 of type19 - internal id of main type7 must be identified
        NTYP = 19
        DO I=1,NI-1
          IF (NOM_OPT(1,I)==ID_TYPE19) THEN
            IPARI(71) = I
            EXIT
          ENDIF
        ENDDO           
      ENDIF     
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('type7_Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('I_TH',INTTH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Multimp',FIELD6,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ibag',IBAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idel7',IDEL7N,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icurv',ICURV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iadm',IADM,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Itied',ITIED,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('IKREM',FLAGREMNOD,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ICOG',IREM7I2,IS_AVAILABLE,LSUBMODEL)
C
      IF(ICURV == 1 .OR. ICURV ==2 )THEN
         CALL HM_GET_INTV('TYPE7_N1',NA1,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV('TYPE7_N2',NA2,IS_AVAILABLE,LSUBMODEL)
      ENDIF
C
      CALL HM_GET_INTV('Deactivate_X_BC',IB1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IB2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IB3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IFORM',MODFR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',IDSENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Crx_Fun',IFRICTH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Fric_ID',INTFRIC,IS_AVAILABLE,LSUBMODEL)
C
      IF(IADM==2) CALL HM_GET_INTV('NRadm',NRADM,IS_AVAILABLE,LSUBMODEL)
C
      IF(INTTH > 0 ) THEN
         CALL HM_GET_INTV('IFORM1',IFORM,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV('fct_ID_k',IRSTH,IS_AVAILABLE,LSUBMODEL)
      ENDIF
C
      IF(NTYP ==  19) THEN
         CALL HM_GET_INTV('IEDGE_TYPE19',FIELD6,IS_AVAILABLE,LSUBMODEL)
      ENDIF
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('GAPSCALE',GAPSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAPMAX',GAPMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FpenMax',FPENMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STMIN',STMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMAX',STMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('PrMesh_Size',PERCENT_SIZE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin',DTMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('TYPE7_SCALE',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STIFF_DC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV('FRIC_DC',VISCF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SORT_FACT',BUMULT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('Xfreq',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale1',XFRICTH,IS_AVAILABLE,LSUBMODEL,UNITAB)
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
C
      IF(IADM==2)THEN
        CALL HM_GET_FLOATV('Padm',PADM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Angladm',ANGLADM,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
C
C
      IF(INTTH > 0 ) THEN
         CALL HM_GET_FLOATV('R_TH',RSTH,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('T_Initial',TINT,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('Crx',XTHE,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('F_RAD',FRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('D_RAD',DRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('Fmax',FHEATS,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('HEAT_AL',FHEATM,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
C
C--------------------------------------------------
C DEFAULT VALUES DEFINITION 
C--------------------------------------------------
C
        IPRINT = 0
C
        KEY1='IGSTI'
        CALL DEFINTER(KEY1    ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IGAP'
        CALL DEFINTER(KEY1    ,IGAP    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IBAG'
        CALL DEFINTER(KEY1    ,IBAG    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IDEL'
        CALL DEFINTER(KEY1    ,IDEL7N  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='FLAGREMNOD'
        CALL DEFINTER(KEY1     ,FLAGREMNOD,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IREM7I2'
        CALL DEFINTER(KEY1     ,IREM7I2    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='INACTI'
        CALL DEFINTER(KEY1     ,INACTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='MODFR'
        CALL DEFINTER(KEY1     ,MODFR,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        IF (NTYP==19) THEN
C-- for TYPE19 field6 is iedge
          IF (FIELD6<=2) THEN
            IEDGE = FIELD6
          ELSE
C-- Compatibility with old inputs where multimp can be read in field6
            IEDGE = 2
          ENDIF
C
          KEY1='IEDGE'
          CALL DEFINTER(KEY1    ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
          IF (IGAP == 4) THEN
C-- IGAP->4 variable gap Igap==2  for node/surface in TYPE19 contact
            IGAP_TYPE19 = IGAP 
            IGAP = 2          
          ENDIF
        ENDIF
C
C--------------------------------------------------
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------
C
C
C....* Card1 :flags  *.............
C
        IS2=1
        INGR2USR => IGRSURF(1:NSURF)%ID
        ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
        IF(ISU1/=0)THEN
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)
          IS1 =2
        ENDIF

        IF(INTTH > 0 ) INTHEAT = 1
        IPARI(47) = INTTH
C Ibag
        IF (IBAG/=0.AND.NVOLU==0.AND.IALELAG==0.AND.NSPHSOL==0) THEN
             CALL ANCMSG(MSGID=614,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IBAG=0
        ENDIF

C Iadm
        IF (IADM/=0.AND.NADMESH==0) THEN
             CALL ANCMSG(MSGID=647,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IADM=0
        ENDIF


        IF (IADM/=0.AND.ICURV/=0) THEN
             CALL ANCMSG(MSGID=648,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR)
        ENDIF

        KCONTACT =MAX(KCONTACT,IBAG,IADM)
        IPARI(44)=IADM

        INTBAG = MAX(INTBAG,IBAG)
        IPARI(32) = IBAG

        IPARI(34)=IGSTI

        IPARI(21)=IGAP

        IPARI(39) = ICURV

        MULTIMP = 4
        IPARI(23)=MULTIMP
C
C....* Card2  *.............
C
        IF(ISU1==0)THEN
          IF(ISU3/=0)THEN
            IS1 =1
            INGR2USR => IGRSURF(1:NSURF)%ID
            ISU1=NGR2USR(ISU3,INGR2USR,NSURF)
          ELSE
              ISU1=ISU2
              IS1 =1
          ENDIF
        ENDIF
        IPARI(45)=ISU1
        IPARI(46)=ISU2
        IPARI(13)=IS1*10+IS2


        IF(IGAP>=2)THEN
          IF(GAPSCALE==ZERO)GAPSCALE=ONE
          FRIGAP(13) = GAPSCALE
          FRIGAP(16) = GAPMAX
        END IF
        IF(FRIGAP(16)==ZERO)THEN
          GAPMAX=EP30
          FRIGAP(16)=GAPMAX
        END IF

        IPARI(85)=ITIED
        FRIGAP(27) = FPENMAX
C
C....* Card3  *.............
C

        IF(IGSTI>1)THEN
          I7STIFS=1
          IF(STMAX==ZERO)STMAX=EP30
          FRIGAP(17) = STMIN
          FRIGAP(18) = STMAX
        ELSE
          STMIN = ZERO
          STMAX = EP30
        END IF
C
        IF (FLAGREMNOD == 0) FLAGREMNOD = 1
        IF (IGAP_TYPE19 == 4) FLAGREMNOD = 2
        IPARI(63) = FLAGREMNOD
C

C-----IPARI(54) is used temporately  before determined only by FLAGREMNOD   
        IF (IREM7I2==0) THEN
           IF (IIMPLICIT>0) IREM7I2=1
        END IF
        IF (IREM7I2 == 3) IREM7I2=0 
        IPARI(54) = IREM7I2
C
        FRIGAP(41)=DTMIN
C
        IF(IGAP==3)THEN
          IF(PERCENT_SIZE==ZERO) PERCENT_SIZE = FOUR*EM01
        END IF
        FRIGAP(28) = PERCENT_SIZE
C
C....* Card4 : curvature  *.............
C
        IF (ICURV ==1) THEN
            IF(NA1==0) THEN
               CALL ANCMSG(MSGID=1727,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=NOINT,
     .                     C1=TITR)
            ENDIF
            WRITE(IOUT,*)' SPHERICAL CURVATURE:'
            WRITE(IOUT,*)'    NA1 =',NA1,' NA2 =',NA2

        ELSEIF(ICURV==2)THEN

            IF(NA1==0.OR.NA2==0) THEN
               CALL ANCMSG(MSGID=1728,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=NOINT,
     .                     C1=TITR)
            ENDIF
            WRITE(IOUT,*)' CYLINDRICAL CURVATURE:'
            WRITE(IOUT,*)'    NA1 =',NA1,' NA2 =',NA2
        ELSEIF(ICURV == 3)THEN
            WRITE(IOUT,*)' SURFACE WITH CUBIC CURVATURE'
        ENDIF

        IPARI(40) = NA1
        IPARI(41) = NA2
C
C.....* Card5 *.....
C
        IF(STFAC== ZERO) STFAC=ONE
        IF(IGSTI==1)STFAC=-STFAC

        IF (STOPT == ZERO) STOPT = EP30

        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT

C
C.....* Card6 *.........
C
        IF (IB1==1) IBC1  = 1
        IF (IB2==1) IBC2  = 1
        IF (IB3==1) IBC3  = 1

C Hidden flag no more read using HM reader
c        IF (BCFLAGM(LFIELD-2:LFIELD-2)== '1') IBC1M = 1
c        IF (BCFLAGM(LFIELD-1:LFIELD-1)== '1') IBC2M = 1
c        IF (BCFLAGM(LFIELD  :LFIELD  )== '1') IBC3M = 1

        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)

        IF(FRIC/=ZERO.AND.VISCF==ZERO)VISCF=ONE
        IF(VISC==ZERO)THEN
           IF(IVIS2==5)THEN
             VISC=ONE
           ELSE
             VISC=FIVEEM2
           ENDIF
        ENDIF
        FRIGAP(14)=VISC
C
        IF (IDEL7N < 0) THEN
            IDELKEEP=1
            IDEL7N=ABS(IDEL7N)
        END IF
        IPARI(61)=IDELKEEP

        IF (IDEL7N>2.OR.N2D==1) IDEL7N = 0
        IPARI(17)=IDEL7N

        IPARI(14)=IVIS2 ! IVIS2 = 0  Hidden flag : no more read using HM reader 

        IPARI(22)=INACTI

C BUMULT is increased for big models
        IF(BUMULT==ZERO) THEN
          BUMULT = BMUL0
          IF(NUMNOD > 2500000) THEN
             BUMULT = BMUL0*TWO
           ELSEIF(NUMNOD > 1500000) THEN
             BUMULT = BMUL0*THREE/TWO
           END IF
        END IF
        FRIGAP(4)=BUMULT

C
C.....* Card7 : FRICTION data *.........
C

        IF (ALPHA==0.) IFQ = 0
C
C
        IF(FRIC == ZERO)THEN
             IF(IFRICTH /= 0)THEN
              FRIC = ONE
             ENDIF
        ENDIF
        IPARI(50) =  IFRICTH

        IF(XFRICTH == ZERO) XFRICTH=ONE
        FRIGAP(34) =  XFRICTH

        IF (MODFR==0) MODFR = 1
        IF (MODFR==2.AND.IFQ<10) IFQ = IFQ + 10

        IF (MFROT/=0.AND.VISCF==0.0) VISCF=ONE
        IF(MODFR==2)VISCF=ZERO

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
        IPARI(31) = IFQ

        IPARI(64) = IDSENS

        IPARI(30) = MFROT

        IPARI(72) =  INTFRIC

C
C.....* OPTIONAL Card8 : C1...C6 friction data *.........
C
        FRIGAP(15)=VISCF**2

        FRIC_P(1) = C1
        FRIC_P(2) = C2
        FRIC_P(3) = C3
        FRIC_P(4) = C4
        FRIC_P(5) = C5
        FRIC_P(6) = C6

C
C.....* Card 9 *.........
C     
        IF(IADM==2)THEN
          IF(NRADM==0)   NRADM  =3
          IF(PADM==ZERO) PADM   =ONE
        ELSE
          NRADM  =1
          PADM   =ONE
          ANGLADM=ZERO
        END IF


        CADM      =COS(ANGLADM*PI/HUNDRED80)

        IPARI(49) =NRADM
        FRIGAP(24)=PADM
        FRIGAP(25)=CADM

C
C.....* Card7 : Thermal input *.........
C

        IF(RSTH == ZERO)THEN
           IF(IRSTH /= 0)THEN
              RSTH = ONE
            ENDIF
        ENDIF
C
        IF (XTHE == ZERO) THEN
        !units
          CALL HM_GET_FLOATV_DIM('Crx' ,XTHE_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XTHE = ONE * XTHE_UNIT
        ENDIF
C
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
        IF(RSTH == ZERO)RSTH = EM30


        FRIGAP(20)  = ONE/RSTH
        FRIGAP(21 ) = FHEATS
        FRIGAP(22 ) = TINT
        FRIGAP(23 ) = FHEATM
        FRIGAP(31 ) = FRAD
        FRIGAP(32 ) = DRAD
        IPARI(48)   = IFORM
        IPARI(42)   = IRSTH
        FRIGAP(33)  = XTHE
C
        IPARI(65) = INTKG
C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)

C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS AND SENSOR - USER TO INTERNAL ID
C------------------------------------------------------------
C
        NTYP0 = 7
        CALL INTER_DCOD_FUNCTION(NTYP0,NI,IPARI,NPC1,NOM_OPT,NPC,TF)
        CALL INTER_DCOD_SENSOR (NTYP0,NI,IPARI,NOM_OPT,LSENSOR)
        CALL INTER_DCOD_FRICTION(NTYP0,NI,IPARI,NOM_OPT,NOM_OPTFRIC,
     .                           INTBUF_FRIC_TAB)
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
C--   No printout for subinterfaces of interface type19
      IF (ID_TYPE19 > 0) RETURN

      IPRINT = 1
C
C........* PRINT DEFAULT VALUES *.....................
C
       KEY1='IGSTI'
       CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='IGAP'
       CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       IF (NTYP==19) THEN
            IF (IGAP_TYPE19 == 4) IGAP = IGAP_TYPE19
            KEY1='IEDGE'
            CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
       ENDIF
C
       KEY1='IBAG'
       CALL DEFINTER(KEY1     ,IBAG     ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='IDEL'
       CALL DEFINTER(KEY1     ,IDEL7N   ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='FLAGREMNOD'
       CALL DEFINTER(KEY1     ,FLAGREMNOD,IASSIGN  ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='IREM7I2'
       CALL DEFINTER(KEY1     ,IREM7I2  ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='INACTI'
       CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='MODFR'
       CALL DEFINTER(KEY1     ,MODFR    ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
C........* PRINT INTERFACES INPUT IN 0.OUT FILE *.....................
C
       IF (NTYP == 19) THEN
         WRITE(IOUT,1550)
       ELSE
         WRITE(IOUT,1549)
       ENDIF
C  
       IF( (IGAP>=2 .AND. IGAP/=1000).
     .        OR.IGSTI>1 .AND. IGSTI/=1000)THEN
           IF(IGSTI==1)THEN
               IF(IDSENS/=0) THEN
                  WRITE(IOUT,1537)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,
     .                 FRIC,IGAP,GAP,GAPMAX,PERCENT_SIZE,FLAGREMNOD,
     .                 IREM7I2,GAPSCALE,IDSENS,                 
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
                ELSE
                  WRITE(IOUT,1538)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,
     .                 FRIC,IGAP,GAP,GAPMAX,PERCENT_SIZE,FLAGREMNOD, 
     .                 GAPSCALE,STARTT,STOPT,       
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14), 
     .                 IPARI(20),MULTIMP
                ENDIF
           ELSE
                IF(IDSENS/=0) THEN
                  WRITE(IOUT,1547)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,IGSTI,STMIN,STMAX,
     .                 FRIC,IGAP,GAP,GAPMAX,PERCENT_SIZE,FLAGREMNOD,
     .                 IREM7I2,GAPSCALE,IDSENS,                 
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
                ELSE
                  WRITE(IOUT,1548)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,IGSTI,STMIN,STMAX,
     .                 FRIC,IGAP,GAP,GAPMAX,PERCENT_SIZE,FLAGREMNOD,
     .                 IREM7I2,GAPSCALE,STARTT,STOPT,                 
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
                ENDIF
           ENDIF
       ELSEIF(STFAC>=ZERO)THEN
           IF(IDSENS/=0) THEN
               WRITE(IOUT,1507)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,IGAP,GAP,PERCENT_SIZE,FLAGREMNOD,
     .                 IREM7I2,IDSENS,
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
           ELSE
                WRITE(IOUT,1594)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,IGAP,GAP,PERCENT_SIZE,FLAGREMNOD,
     .                 IREM7I2,STARTT,STOPT,
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
           ENDIF
       ELSE
C
           IF(IDSENS/=0) THEN
                 WRITE(IOUT,1517)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 -STFAC,IGAP,GAP,PERCENT_SIZE,FLAGREMNOD,
     .                 IREM7I2,IDSENS,
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
           ELSE
                 WRITE(IOUT,1595)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 -STFAC,IGAP,GAP,PERCENT_SIZE,FLAGREMNOD,
     .                 IREM7I2,STARTT,STOPT,
     .                 BUMULT,INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
           ENDIF
       ENDIF
C
       IF(ITIED /=0)THEN
            WRITE(IOUT,'(A/,A,I10/,A/,A)')
     .    '    NODE TYING IF LARGE PENETRATION (LET DT < DTMIN) ',
     .    '    ITIED . . . . . . . . . . . . . . . . . . . . . .',ITIED,
     .    '       1: TIED (during contact)',
     .    '       2: TIED (no rebound)'
       END IF

       IF (DTMIN/=ZERO) THEN
            WRITE(IOUT,1577)DTMIN
       ENDIF

       IF(IDEL7N/=0) THEN
           WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',
     . ' (1:YES-ALL/2:YES-ANY) : ',IDEL7N
           IF(IDELKEEP == 1)THEN
              WRITE(IOUT,'(A)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
           ENDIF         
        ENDIF

        IF(INTFRIC == 0 ) THEN
           IF(IFQ>=10) THEN
               WRITE(IOUT,1518)
           ELSE
               WRITE(IOUT,1519)
           ENDIF

           WRITE(IOUT,1520)MOD(IFQ,10), XFILTR

           IF(MFROT==0)THEN

             IF(IFRICTH ==0 ) THEN
                WRITE(IOUT,1524) FRIC
              ELSE
                WRITE(IOUT,1525) IFRICTH,XFRICTH,FRIC
              ENDIF 

           ELSEIF(MFROT==1)THEN
                WRITE(IOUT,1515)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .          FRIC_P(4),FRIC_P(5)

           ELSEIF(MFROT==2)THEN
                WRITE(IOUT,1522)FRIC,FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .          FRIC_P(4),FRIC_P(5),FRIC_P(6)

           ELSEIF(MFROT==3)THEN
                WRITE(IOUT,1523)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .           FRIC_P(4),FRIC_P(5),FRIC_P(6)

           ENDIF
        ELSE

           WRITE(IOUT,5030) INTFRIC
           IF(IFRICTH > 0 ) THEN
              WRITE(IOUT,1525) IFRICTH,XFRICTH,FRIC
           ENDIF

        ENDIF

        IF(IBAG/=1000) THEN
              WRITE(IOUT,*)'    AIRBAG POROSITY COUPLING ON   '
        ENDIF

        IF(IADM/=0) THEN
           WRITE(IOUT,*)'    MESH REFINEMENT CASE OF CONTACT',
     .' (0:NO/1:DUE TO CURVATURE/2:DUE TO CURVATURE OR PENETRATION)',
     .' SET TO ',IADM
           IF(IADM==2)THEN
              WRITE(IOUT,1557) NRADM,PADM,ANGLADM
           END IF
        ENDIF

        IF(INTTH > 0 )THEN
          IF(IRSTH==0)THEN
            WRITE(IOUT,2500) RSTH,TINT,FRAD,DRAD,IFORM,
     .         FHEATS,FHEATM
          ELSE
             WRITE(IOUT,2503) IRSTH,XTHE,RSTH,TINT,FRAD,DRAD,IFORM,
     .         FHEATS,FHEATM
          END IF
        ENDIF
C---- printout for TYPE19
        IF (NTYP == 19) THEN 
          IF (IGAP == 4) THEN
            WRITE(IOUT,1552) IEDGE,GAP
          ELSE
            WRITE(IOUT,1551) IEDGE
          ENDIF
        ENDIF

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


 1515 FORMAT(//
     .    '    FRICTION MODEL 1 (Viscous Polynomial)'/,
     .    '     MU = MUo + C1 p + C2 v + C3 pv + C4 p^2 + C5 v^2'/,
     .    '    C1 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C2 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C3 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C4 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    C5 . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    TANGENTIAL PRESSURE LIMIT. . .. . . . . .',1PG20.13/)
 1518 FORMAT( '    FRICTION FORMULATION: INCREMENTAL (STIFFNESS) ',
     .             'FORMULATION')
 1519 FORMAT( '    FRICTION FORMULATION: TOTAL (VISCOUS) ',
     .             'FORMULATION')
 1520 FORMAT(
     .    '    FRICTION FILTERING FLAG. . . . . . . . . ',I10/,
     .    '    FILTERING FACTOR . . . . . . . . . . . . ',1PG20.13)
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
     .      '    Abscissa scale factor on IFUNTCF. . . .  . ',1PG20.13/,
     .      '    Ordinate scale factor on IFUNTCF .  . .  . ',1PG20.13/)
C----------     
 1537 FORMAT(
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    CONSTANT STIFFNESS. . . . . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . . ',I5/,
     .       '    MINIMUM GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM GAP (= 0. <=> NO MAXIMUM GAP) . . . ',1PG20.13/,
     .       '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . . '/,
     .       '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . . ',1PG20.13/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE   ',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2   ',I10/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME/STOP TIME ACTIVATED BY SENSOR ID ',I10/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . . ',I10/,
     .       '    MAXIMAL INITIAL PENETRATION FACTOR. . . . . ',1PG20.13/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION CRITICAL DAMPING FACTOR. . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . . ',I5/,
     .       '    FORMULATION LEVEL . . . . . . . . . . . . . ',I5/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . . ',I5/)
 1538 FORMAT(
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    CONSTANT STIFFNESS. . . . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM GAP (= 0. <=> NO MAXIMUM GAP) . . ',1PG20.13/,
     .       '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . '/,
     .       '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . ',1PG20.13/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE ',I5/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .       '    MAXIMAL INITIAL PENETRATION FACTOR. . . . ',1PG20.13/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .       '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)
 1547 FORMAT(
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I4/,
     .' 2,3,4,5 : STIFFNESS IS COMPUTED FROM STIFFNESS ON MAIN SIDE'/,
     .'                                 AND STIFFNESS AT SECONDARY NODES'/,
     .'    1000 : STIFFNESS IS COMPUTED FROM STIFFNESS ON MAIN SIDE'/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . . ',I5/,
     .       '    MINIMUM GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM GAP (= 0. <=> NO MAXIMUM GAP) . . . ',1PG20.13/,
     .       '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . . '/,
     .       '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . . ',1PG20.13/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE   ',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2  ',I10/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME/STOP TIME ACTIVATED BY SENSOR ID ',I10/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . . ',I10/,
     .       '    MAXIMAL INITIAL PENETRATION FACTOR. . . . . ',1PG20.13/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION CRITICAL DAMPING FACTOR. . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . . ',I5/,
     .       '    FORMULATION LEVEL . . . . . . . . . . . . . ',I5/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . . ',I5/)
 1548 FORMAT(
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I4/,
     .' 2,3,4,5 : STIFFNESS IS COMPUTED FROM STIFFNESS ON MAIN SIDE'/,
     .'                                 AND STIFFNESS AT SECONDARY NODES'/,
     .'    1000 : STIFFNESS IS COMPUTED FROM STIFFNESS ON MAIN SIDE'/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM GAP (= 0. <=> NO MAXIMUM GAP) . . ',1PG20.13/,
     .       '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . '/,
     .       '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . ',1PG20.13/,
     .       '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . '/,
     .       '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE   ',I5/,
     .       '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2  ',I10/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .       '    MAXIMAL INITIAL PENETRATION FACTOR. . . . ',1PG20.13/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .       '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)
 1507 FORMAT(
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . . '/,
     .    '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . . ',1PG20.13/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . '/,
     .    '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE   ',I5/,
     .    '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2   ',I10/,
     .    '    START TIME/STOP TIME ACTIVATED BY SENSOR ID ',I10/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . . ',I10/,
     .    '    MAXIMAL INITIAL PENETRATION FACTOR. . . . . ',1PG20.13/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . . ',I5/)
 1594 FORMAT(
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . '/,
     .    '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . ',1PG20.13/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . '/,
     .    '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE ',I5/,
     .    '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2   ',I10/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .    '    MAXIMAL INITIAL PENETRATION FACTOR. . . . ',1PG20.13/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)
 1517 FORMAT(
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    CONSTANT STIFFNESS. . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . . '/,
     .    '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . . ',1PG20.13/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . '/,
     .    '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE   ',I5/,
     .    '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2   ',I10/,
     .    '    START TIME/STOP TIME ACTIVATED BY SENSOR ID ',I10/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . . ',I10/,
     .    '    MAXIMAL INITIAL PENETRATION FACTOR. . . . . ',1PG20.13/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . . ',I5/)
 1595 FORMAT(
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    CONSTANT STIFFNESS. . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    %MESHSIZE TAKEN INTO ACCOUNT FOR  . . . . '/,
     .    '    CALCULATION OF MAXIMUM GAP(IGAP = 3). . . ',1PG20.13/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . '/,
     .    '    NODES IN CASE OF ELEMENT SIZE < GAP VALUE ',I5/,
     .    '    DE-ACTIVATION FLAG IF CONFLITING W/ TYPE2 ',I10/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .    '    MAXIMAL INITIAL PENETRATION FACTOR. . . . ',1PG20.13/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)

 1549 FORMAT(//
     .    '    TYPE==7   PARALLEL/AUTO IMPACTING           ' //)
 1550 FORMAT(//
     .    '    TYPE==19  PARALLEL/AUTO IMPACTING           ' //)
 1577 FORMAT(
     .       '    MINIMUM TIME STEP . . . . . . . . . . . . ',1PG20.13/)
C
 5030 FORMAT(/
     .       '    INTERFACE FRICTION MODEL.  . . . . . . . . ',I10)
C
 1557 FORMAT(
     .'  NUMBER OF ELEMENTS WITHIN A 90 DEGREES FILLET         ',I5/,
     .'  ---------------------------------------------         '/,
     .'  CRITERIA FOR REFINEMENT DUE TO PENETRATION :          '/,
     .'  ------------------------------------------            '/,
     .'  MINIMUM PERCENTAGE OF PENETRATION                     ',
     .   1PG20.13/,
     .'  MAXIMUM ANGLE ON MAIN SIDE AT CONTACT LOCATION      ',
     .   1PG20.13//) 
 2500   FORMAT(//
     .       '    TYPE==7   thermal interface          ' //,
     .       '    Heat conduction per surface unit. . . .  . ',1PG20.13/,
     .       '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .       '    Radiation factor . . . . . . . . . . . . . ',1PG20.13/,
     .       '    Maximum distance for radiation computation.',1PG20.13/,
     .       '    FORMULATION CHOICE : . . . . . . . . . . . . ',I10,/,
     .       '          0 : heat transfer between shells(SECONDARY side)',/,
     .       '                 and constant temperature in interface',/,
     .       '          1 : heat Exchange between pieces in contact',/,
     .       '  Frictional heat transfer on SECONDARY surface . ',1PG20.13/,
     .       '  Frictional heat transfer on MAIN surface. ',1PG20.13/)
 2501   FORMAT(//
     .       '    TYPE==7   thermal interface          ' //,
     .       '    Heat conduction per surface unit. . . .  . ',1PG20.13/,
     .       '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .       '    FORMULATION CHOICE : . . . . . . . . . . . . ',I10,/,
     .       '          0 : heat transfer between shells(SECONDARY side)',/,
     .       '                 and constant temperature in interface',/,
     .       '          1 : heat Exchange between pieces in contact')
2503   FORMAT(//
     .       '    TYPE==7   thermal interface          ' //,
     .  '   Function for thermal heat exchange coefficient wrt contact pressure',I10/,
     .      '    Abscissa scale factor on IFUNTCK. . . .  . ',1PG20.13/,
     .      '    Ordinate scale factor on IFUNTCK .  . .  . ',1PG20.13/,
     .       '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .       '    Radiation factor . . . . . . . . . . . . . ',1PG20.13/,
     .       '    Maximum distance for radiation computation.',1PG20.13/,
     .       '    FORMULATION CHOICE : . . . . . . . . . . . . ',I10,/,
     .       '          0 : heat transfer between shells(SECONDARY side)',/,
     .       '                 and constant temperature in interface',/,
     .       '          1 : heat Exchange between pieces in contact',/,
     .       '  Frictional heat transfer on SECONDARY surface . ',1PG20.13/,
     .       '  Frictional heat transfer on MAIN surface. ',1PG20.13/)
 1551 FORMAT(
     .       '    EDGES SELECTION FLAG . . . . . . . . . . . ',I5/)
 1552 FORMAT(
     .       '    EDGES SELECTION FLAG . . . . . . . . . . . ',I5/,
     .       '    CONSTANT GAP FOR EDGE TO EDGE CONTACT . . .',1PG20.13/,
     .       '    VARIABLE GAP FOR NODE TO SURFACE CONTACT (IGAP=2)',/)
      END
