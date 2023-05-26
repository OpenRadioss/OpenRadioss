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
Copyright>        GNU Affero General Public License for more details
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
Chd|  HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        INTER_DCOD_FRICTION           source/interfaces/reader/inter_dcod_friction.F
Chd|        INTER_DCOD_SENSOR             source/interfaces/reader/inter_dcod_sensor.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PARAMETERS_MOD                ../common_source/modules/interfaces/parameters_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE24(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,
     2        IGRNOD     ,IGRSURF    ,XFILTR     ,FRIC_P     ,DEF_INTER ,
     3        SENSORS    ,NOM_OPT    ,UNITAB     ,LSUBMODEL  ,TITR      ,
     4        NOM_OPTFRIC,INTBUF_FRIC_TAB,PARAMETERS )
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
      USE SENSOR_MOD
      USE PARAMETERS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*), NOM_OPTFRIC(LNOPT1,*)
      INTEGER ISU1,ISU2,NI
      INTEGER IPARI(*),DEF_INTER(*)
      my_real
     .   STFAC,XFILTR
      my_real
     .   FRIGAP(*),FRIC_P(*)
      CHARACTER TITR*nchartitle,TITR1*nchartitle
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
      TYPE (SENSORS_)  ,INTENT(IN) :: SENSORS
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (PARAMETERS_) ,INTENT(INOUT):: PARAMETERS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr12_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,IBC1, IBC2, IBC3, IBUC, NOINT, NTYP, IBID,
     .    INACTI, IBC1M, IBC2M, IBC3M, IGSTI, IVIS2,IS1, IS2,
     .    ILEV, IGAP,MULTIMP,HIERA,MFROT,IFQ,IBAG,MODFR,IERR1, 
     .    IFLAGUNIT,ISYM,IEDGE,NOD1,IDUM,IGNORE,IDEL24,KK,II, 
     .    IGAP0,FLAGREMNOD,IDSENS,IDELKEEP,INTKG,IREM24I2,IPRINT,    
     .    INTFRIC,MULTIMPE,IASSIGN,ICURV,INTPLY,INTTH,NPREF,
     .    INTNITSCHE,IPEN0,NA1,NA2,IADM,NRADM,IFORM,IPSTIF
      my_real
     .    FRIC,GAP,STARTT,BUMULT,STOPT,C1,C2,C3,C4,C5,C6,ALPHA,
     .    GAPSCALE,GAPMAX,STMIN,STMAX,STIFF,EDG_ANGL,
     .    PERCENT_SIZE,GAPMAX_S,GAPMAX_M,PENMAX,PENMIN,VISC,VISCF,
     .    ETEN_M,PADM,ANGLADM,RSTH,FHEAT,TINT,CADM,TPFIT,STFACM,
     .    DTSTIF
      CHARACTER MESS*40, MSGTITL*nchartitle
      CHARACTER OPT*ncharkey,KEY*ncharkey,KEY1*ncharkey,
     .          BCFLAG*ncharfield, BCFLAGM*ncharfield
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE24
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
      ICURV = 0
      INTPLY = 0
      IREM24I2 = 0
      INTTH = 0
      IDEL24= 0
      INTNITSCHE = 0
      IADM = 0
      IFORM = 0
      IVIS2=0
      IPSTIF = 0

C-------for the moment
C     IGSTI = 0
      IBAG = 0
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
      ETEN_M = ZERO
      RSTH = EP30
      FHEAT= ONE
      TINT = ZERO
      STFACM= ZERO
C
      IASSIGN = 1
C
      NTYP = 24
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C
      IS_AVAILABLE = .FALSE.
C
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Irem_i2',IREM24I2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE24_Idel',IDEL24,IS_AVAILABLE,LSUBMODEL)

      CALL HM_GET_INTV('GRNOD_ID',NOD1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE24_Iedge',IEDGE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IPSTIF',IPSTIF,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Igap',IGAP0,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ipen',IPEN0,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Deactivate_X_BC',IBC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IBC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IBC3,IS_AVAILABLE,LSUBMODEL)

      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',IDSENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Fric_ID',INTFRIC,IS_AVAILABLE,LSUBMODEL)
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('GAP1',GAPMAX_S,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP2',GAPMAX_M,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STMIN',STMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMAX',STMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TYPE24_Edge_Angle',EDG_ANGL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Ipen_max',PENMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Ipen_min',PENMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STFAC_MDT',STFACM,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STFAC',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STIFF_DC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV('Tpressfit',TPFIT,IS_AVAILABLE,LSUBMODEL,UNITAB) 
C
      CALL HM_GET_FLOATV('Xfreq',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DTSTIF',DTSTIF,IS_AVAILABLE,LSUBMODEL,UNITAB)
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
C----- was hiding flag
        NPREF=0
C
C--------------------------------------------------
C DEFAULT VALUES DEFINITION 
C--------------------------------------------------
C
        IPRINT = 0

        KEY1='IGSTI'
        CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IDEL24'
        CALL DEFINTER(KEY1     ,IDEL24  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IEDGE'
        CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
        KEY1='IGAP0'
        CALL DEFINTER(KEY1     ,IGAP0    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IPEN0'
        CALL DEFINTER(KEY1     ,IPEN0    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='INACTI'
        CALL DEFINTER(KEY1     ,INACTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
C--------------------------------------------------
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------
C
C
C....* Card1 :flags  *.............
C

C
C....* CHECKS *.............

        IF (IREM24I2==0) THEN
           IREM24I2=1
c           IF (IIMPLICIT>0) IREM24I2=1
        END IF
        IF (IREM24I2==3) IREM24I2 = 0

C
C....* Card2 :Gaps  *.............
C

        IGAP = 1
        IPARI(21)=IGAP
        IPARI(63)=IREM24I2
C------ consisting w/ manuel         
        IF (IREM24I2==0) IREM24I2 = 3


C.....* CHECKS  AND Storage IPARI FRIGAP  *..........
C
C
C------ILEV =1 : ISU1>0,ISU2=0; IS3>=0
C            2 : ISU1>0,ISU2>0; IS3>=0
C            3 : ISU1=0,ISU2>0;IS3>0
        ILEV=0

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
            IF (IEDGE > 0 ) THEN
             IEDGE = 0
             EDG_ANGL=ZERO
c             CALL ANCWAR(119,ANINFO)
            END IF
        END SELECT
        INGR2USR => IGRNOD(1:NGRNOD)%ID
        IF(NOD1 /= 0) NOD1=NGR2USR(NOD1,INGR2USR,NGRNOD)
C------only IEDGE=5 or 	IEDGE=0 
        IF (IEDGE>0) IEDGE=5
        IF (IEDGE==4.OR.IEDGE==5) THEN
          IPARI(59)=IEDGE-3
c          IEDGE=MIN(4,IEDGE)
        END IF

C--------change default EDG_ANGL=135         
        IF((IEDGE>=3).AND. EDG_ANGL==ZERO) 
C     +            EDG_ANGL=NINTY+ONE
     +            EDG_ANGL=HUNDRED+THIRTY+FIVE
        FRIGAP(26)  = COS((HUNDRED80-EDG_ANGL)*PI/HUNDRED80)


C---------Default Istif for implicit
        IF (IIMPLICIT>0.AND.IGSTI==0) IGSTI=4

        IF(IGSTI >= 10) THEN ! Nitsche Method Activation instead of penalty
           INTNITSCHE = 1
           IPARI(86) = 1
           IGSTI = IGSTI - 10 
           IF(INTNITSCHE > 0 ) NITSCHE = 1
           IF (ILEV /= 1.AND.ILEV /= 2) THEN
              CALL ANCMSG(MSGID=1622,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=NOINT,
     .                  C1=TITR)
           ENDIF
           IF (IEDGE > 0) THEN
              CALL ANCMSG(MSGID=1746,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=NOINT,
     .                  C1=TITR)
           ENDIF
        ENDIF

C-------tempo: Isurf <- ILEV=IPARI(20) -----

        IPARI(26)=NOD1

        IPARI(13)=IS1*10+IS2
        IPARI(45)=ISU1
        IPARI(46)=ISU2

        IPARI(43)=ISYM
        IPARI(58)=MIN(4,IEDGE)
        IPARI(63)=IREM24I2

C------ consisting w/ manuel         
        IF (IREM24I2==0) IREM24I2 = 3

        IF (INACTI==-1.AND.(IGSTI==0.OR.IGSTI==1000)) IGSTI = 4
        IPARI(20)=ILEV
        IPARI(34)=IGSTI
        IPARI(47) = INTTH
        IF(INTTH > 0 ) INTHEAT = 1
        IF (IIMPLICIT>0) INTKG=1
C flag to activate update of gap with plyxfem
        IPARI(66) = INTPLY    

C
C....* Card3  *.............
C

        IF(IGSTI>1)THEN
            I7STIFS=1
            IF(STMAX==ZERO)STMAX=EP30
          ELSE
            STMIN = ZERO
            STMAX = EP30
        END IF

c          IF(IGAP==3)THEN
c            GAPMAX = GAPMAX * FAC_L
c            IF(GAPSCALE==ZERO)GAPSCALE=ONE
c            FRIGAP(13) = GAPSCALE
c            FRIGAP(16) = GAPMAX
c            IF(PERCENT_SIZE==ZERO) PERCENT_SIZE = 0.4
c          END IF

        PERCENT_SIZE=ZERO

C-----------  GAPMAX->GAPMAX_S: EP30
C-----------IGAP0,IPEN0: used only for starter;sharing places w/ int20      
        IPARI(53)=IGAP0
        IPARI(54)=IPEN0
c       FRIGAP(16) = EP30
        FRIGAP(17) = STMIN
        FRIGAP(18) = STMAX
c       FRIGAP(28) = PERCENT_SIZE

        IF(GAPMAX_S==ZERO)GAPMAX_S=EP30
        IF(GAPMAX_M==ZERO)GAPMAX_M=EP30

C       IF(PENMAX==ZERO)PENMAX=EP30
        IF(PENMIN==ZERO)PENMIN=EM8
        IF(ETEN_M == ZERO)ETEN_M =TWENTY5*EM03 
        FRIGAP(35) = GAPMAX_S
        FRIGAP(36) = GAPMAX_M
        FRIGAP(37) = PENMAX
        FRIGAP(38) = PENMIN
        FRIGAP(39) = ETEN_M

        NA1 = 0
        NA2 = 0
c          IF(ICURV==1)THEN
c            WRITE(IOUT,*)' SPHERICAL CURVATURE:'
c            WRITE(IOUT,*)'    NA1 =',NA1,' NA2 =',NA2
c          ELSEIF(ICURV==2)THEN
c            WRITE(IOUT,*)' CYLINDRICAL CURVATURE:'
c            WRITE(IOUT,*)'    NA1 =',NA1,' NA2 =',NA2
c          ELSEIF(ICURV == 3)THEN
c            WRITE(IOUT,*)' SURFACE WITH CUBIC CURVATURE'
c          ENDIF
        IPARI(39) = 0
        IPARI(40) = NA1
        IPARI(41) = NA2

C
C....* Card4  *.............
C

C
C.....* CHECKS *.....
C
        GAP=ZERO
C       GAP = GAP * FAC_L
        IF (STOPT == ZERO) STOPT = EP30

C.....* Storage IPARI FRIGAP *.......

        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT

C
C....* Card5  *.............
C


C ----- presfit    
          IF (INACTI==-1.AND.IIMPLICIT==0) THEN
           NPREF=10000
           IPARI(40) = NPREF
          ELSE
           TPFIT=ZERO
          END IF
C
C......* CHECKS  AND Storage IPARI FRIGAP *.....
C  
C Hidden flag no more read using HM reader
c        IF (BCFLAGM(LFIELD-2:LFIELD-2)== '1') IBC1M = 1
c        IF (BCFLAGM(LFIELD-1:LFIELD-1)== '1') IBC2M = 1
c        IF (BCFLAGM(LFIELD  :LFIELD  )== '1') IBC3M = 1

        IF (IIMPLICIT>0.AND.INACTI==0) INACTI=-1
        IF (INACTI/=0.AND.INACTI/=1.AND.INACTI/=-1.AND.INACTI/=3
     .      .AND.INACTI/=5) THEN
            IF (INACTI==6) THEN
               INACTI = 5
            ELSE
               INACTI = 0
            END IF
            CALL ANCMSG(MSGID=1031,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=NOINT,
     .                  C1=TITR ,
     .                  I2=INACTI)
        END IF
        IPARI(22)=INACTI

        IF(STFAC==ZERO) THEN
            STFAC=ONE
        ENDIF
        IF(IGSTI==1)STFAC=-STFAC

C------no use of  VISCF
        VISCF=ZERO
        FRIGAP(15)=VISCF**2
C-------used for TPFIT
        FRIGAP(15)=TPFIT
C         default 1: harmonic mass, IVIS2=-1 old: SECONDARY node mass
        IF (IVIS2==0) THEN
           IVIS2=1
        ELSEIF (IVIS2==-1) THEN
           IVIS2=0
        END IF
        IF(VISC==ZERO)THEN
            IF(IVIS2==5)THEN
              VISC=ONE
            ELSE
              VISC=FIVEEM2
            ENDIF
        ENDIF
        FRIGAP(14)=VISC

        IF (IDEL24 < 0) THEN
            IDELKEEP=1
            IDEL24=ABS(IDEL24)
        END IF
        IPARI(61)=IDELKEEP

        IF (IDEL24>2.OR.N2D==1) IDEL24 = 0
        IPARI(17)=IDEL24

        IPARI(14)=IVIS2

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

C---------
        IF(IPSTIF >0 .OR.IGSTI==7) IPARI(97)   = 1
        IF(IGSTI==7.AND.IPSTIF ==0) IPSTIF = 1
        IPARI(98) = IPSTIF
        IF(STFACM == ZERO.AND.IPARI(97) > 0) STFACM = EM01
        IF(IPARI(97) > 0) THEN
           FRIGAP(48) = DTSTIF 
           PARAMETERS%ISTIF_DT = 1
           FRIGAP(47) = STFACM
        ELSE
           DTSTIF = ZERO 
           STFACM = ZERO
        ENDIF

C
C....* Card6 *.............
C

C......* CHECKS *.....
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

C-------* Storage IPARI FRIGAP *----------------------------
          IPARI(30) = MFROT
          IPARI(31) = IFQ
          IPARI(32) = IBAG
C          INTBAG = MAX(INTBAG,IBAG)
          IPARI(72) =  INTFRIC
          IPARI(64) = IDSENS

C
C....* Card7 : C1...C6 friction data  *.............
C

        FRIC_P(1) = C1
        FRIC_P(2) = C2
        FRIC_P(3) = C3
        FRIC_P(4) = C4
        FRIC_P(5) = C5
        FRIC_P(6) = C6

C
C....* Card9 : Thermal input not available yet  *.............
C

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
        IPARI(44)=IADM
        KCONTACT =MAX(KCONTACT,IBAG,IADM)
C
        IF(IADM==2)THEN
            IREC=IREC+1
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            READ(LINE,ERR=999,FMT=FMT_I_2F) NRADM,PADM,ANGLADM
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
C....* Card10: Thermal input not available yet  *.............
C
C
          IF(ITHERM_FE == 0 .AND. INTTH > 0 ) THEN
             INTHEAT = 0
             IPARI(47) = 0
             CALL ANCMSG(MSGID=702,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR)
           ENDIF
C
           IF(RSTH == ZERO)RSTH = EP30
           FRIGAP(20)  = ONE/RSTH
           FRIGAP(21 ) = FHEAT
           FRIGAP(22 ) = TINT
           IPARI(48)   = IFORM


C
         IPARI(12)=IBUC
         IPARI(65) = INTKG


C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)

        MULTIMP = 4
        IPARI(23)=MULTIMP

C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS AND SENSOR - USER TO INTERNAL ID
C------------------------------------------------------------
C
        CALL INTER_DCOD_SENSOR (NTYP,NI,IPARI,NOM_OPT,SENSORS)
        CALL INTER_DCOD_FRICTION(NTYP,NI,IPARI,NOM_OPT,NOM_OPTFRIC,
     .                INTBUF_FRIC_TAB)
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
C........* PRINT DEFAULT VALUES *.....................
C
      IPRINT = 1
C
       KEY1='IGSTI'
       CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='IEDGE'
       CALL DEFINTER(KEY1     ,IEDGE    ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='INACTI'
       CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)

C........* PRINT INTERFACES INPUT *.....................

       IDUM =IEDGE
       IF (IEDGE/=1000.AND.IEDGE>0)IDUM =MIN(1,IEDGE)

       IF(INTNITSCHE > 0) THEN
         IF(IDSENS/=0) THEN 
          WRITE(IOUT,3528)IBC1,IBC2,IBC3,
     .                 IGSTI,STFAC,ILEV,IDUM,EDG_ANGL,
     .                 STMIN,STMAX,
     .                 FRIC,GAPMAX_S,GAPMAX_M,IGAP0,IPEN0,
     .                 PENMAX,IDSENS,
     .                 INACTI,IREM24I2,VISC,IPARI(14)
         ELSE
          WRITE(IOUT,3529)IBC1,IBC2,IBC3,
     .                 IGSTI,STFAC,ILEV,IDUM,EDG_ANGL,
     .                 STMIN,STMAX,
     .                 FRIC,GAPMAX_S,GAPMAX_M,IGAP0,IPEN0,
     .                  PENMAX,STARTT,STOPT,
     .                 INACTI,IREM24I2,VISC,IPARI(14)
         ENDIF
       ELSE
         IF(IDSENS/=0) THEN 
          WRITE(IOUT,3524)IBC1,IBC2,IBC3,
     .                 IGSTI,STFAC,ILEV,IDUM,EDG_ANGL,
     .                 STMIN,STMAX,
     .                 FRIC,GAPMAX_S,GAPMAX_M,IGAP0,IPEN0,
     .                 PENMAX,IDSENS,
     .                 INACTI,IREM24I2,VISC,IPARI(14)
         ELSE
          WRITE(IOUT,3525)IBC1,IBC2,IBC3,
     .                 IGSTI,STFAC,ILEV,IDUM,EDG_ANGL,
     .                 STMIN,STMAX,
     .                 FRIC,GAPMAX_S,GAPMAX_M,IGAP0,IPEN0,
     .                  PENMAX,STARTT,STOPT,
     .                 INACTI,IREM24I2,VISC,IPARI(14)
         ENDIF
       ENDIF
       IF(INTPLY > 0) WRITE(IOUT,*)'    CONTACT WITH VARIABLE GAP'
       IF(TPFIT > ZERO) WRITE(IOUT,5040) TPFIT

       IF(IDEL24/=3) THEN
          WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',
     . ' (1:YES-ALL/2:YES-ANY) : ',IDEL24
          IF(IDELKEEP == 1)THEN
            WRITE(IOUT,'(A)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
          ENDIF         
       ENDIF
C
        IF(STFACM > 0) WRITE(IOUT,3500) STFACM,DTSTIF, IPSTIF
C
       IF(INTFRIC > 0 ) THEN
          WRITE(IOUT,5030) INTFRIC
        ELSE
          WRITE(IOUT,1520)MOD(IFQ,10), XFILTR
          IF(MFROT==0)THEN
            WRITE(IOUT,1524) FRIC
          ELSEIF(MFROT==1)THEN
            WRITE(IOUT,1515)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .      FRIC_P(4),FRIC_P(5)
          ELSEIF(MFROT==2)THEN
            WRITE(IOUT,1522)FRIC,FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .      FRIC_P(4),FRIC_P(5),FRIC_P(6)
          ELSEIF(MFROT==3)THEN
            WRITE(IOUT,1523)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .      FRIC_P(4),FRIC_P(5),FRIC_P(6)
          ELSEIF(MFROT==4)THEN
            WRITE(IOUT,1526)  FRIC,FRIC_P(1),FRIC_P(2)
          ENDIF
       ENDIF
C        
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
C          
       IF(INTTH > 0 )  WRITE(IOUT,2501) RSTH,TINT,IFORM

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
     .      '    Abscissa scale factor on IFUNTCF. . . .  . ',1PG20.13/,
     .      '    Ordinate scale factor on IFUNTCF .  . .  . ',1PG20.13/)
 1526 FORMAT(/
     .    '     EXPONENTIAL DECAY FRICTION LAW   '/
     .    '      MU = c1+(MUo-c1)*exp(-c2*v)'/
     .    '      STATIC COEFFICIENT MUo . . . . . . . . . ',1PG20.13/,
     .    '      DYNAMIC COEFFICIENT C1 . . . . . . . . . ',1PG20.13/,
     .    '      EXPONENTIAL DECAY COEFFICIENT C2 . . . . ',1PG20.13/)

 1518 FORMAT( '    FRICTION FORMULATION: INCREMENTAL (STIFFNESS) ',
     .             'FORMULATION')
 1519 FORMAT( '    FRICTION FORMULATION: TOTAL (VISCOUS) ',
     .             'FORMULATION')
 1520 FORMAT(
     .    '    FRICTION FILTERING FLAG. . . . . . . . . ',I10/,
     .    '    FILTERING FACTOR . . . . . . . . . . . . ',1PG20.13)
 1557 FORMAT(
     .'  NUMBER OF ELEMENTS WITHIN A 90 DEGREES FILLET         ',I5/,
     .'  ---------------------------------------------         '/,
     .'  CRITERIA FOR REFINEMENT DUE TO PENETRATION :          '/,
     .'  ------------------------------------------            '/,
     .'  MINIMUM PERCENTAGE OF PENETRATION                     ',
     .   1PG20.13/,
     .'  MAXIMUM ANGLE ON MAIN SIDE AT CONTACT LOCATION      ',
     .   1PG20.13//)
C----------     
 2501   FORMAT(//
     .       '    TYPE==7   thermal interface          ' //,
     .       '    Heat conduction per surface unit. . . .  . ',1PG20.13/,
     .       '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .       '    FORMULATION CHOICE : . . . . . . . . . . . . ',I10,/,
     .       '          0 : heat transfer between shells(SECONDARY side)',/,
     .       '                 and constant temperature in interface',/,
     .       '          1 : heat Exchange between pieces in contact')
C----------change message here     
 3524 FORMAT(//
     .       '    TYPE==24  MUTI-TYPE IMPACTING             ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT . . . . . IN X DIR       ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)                Y DIR       ',I1/,
     .       '                                                   Z DIR       ',I1/,
     .       '    STIFFNESS FORMULATION . . . . . . . . . . .            ',I5/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .       '    CONTACT TYPES (1:S1/S1;2:S1/S2;3:N/S  . . .            ',I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . .            ',I5/,
     .       '    ANGLE FOR EDGE COMPUTATION (Iedge>0). . . . ',1PG20.13/,
C     .       '    MAIN SEG EXTENSION . . . . . . . . . .  ',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
C     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    MAXIMUM SECONDARY GAP VALUE . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM MAIN GAP VALUE. . . . . . . . . . . ',1PG20.13/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE .            ',I5/,
     .       '    INITIAL PENETRATION DETECTION METHOD  . . .            ',I5/,
     .       '    MAXIMUM INITIAL PENETRATION VALUE . . . . . ',1PG20.13/,
C     .       '    MINIMUM INITIAL PENETRATION VALUE . . . . ',1PG20.13/,
     .       '    START/STOP TIMES activated by SENSOR ID . .       ',I10/,
c     .       '    BUCKET FACTOR . . . . . . . . . . . . .  ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . .         ',I10/,
     .       '    DE-ACTIVATION FLAG IF CONFLICT W/ TYPE2 . .       ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . .            ',I5/)
C
 3525 FORMAT(//
     .       '    TYPE==24  MUTI-TYPE IMPACTING             ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT . . . . . IN X DIR       ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)                Y DIR       ',I1/,
     .       '                                                   Z DIR       ',I1/,
     .       '    STIFFNESS FORMULATION . . . . . . . . . . .            ',I5/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .       '    CONTACT TYPES (1:S1/S1;2:S1/S2;3:N/S  . . .            ',I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . .            ',I5/,
     .       '    ANGLE FOR EDGE COMPUTATION (Iedge>0). . . . ',1PG20.13/,
C     .       '    MAIN SEG EXTENSION . . . . . . . . . .  ',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
C     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    MAXIMUM SECONDARY GAP VALUE . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM MAIN GAP VALUE. . . . . . . . . . . ',1PG20.13/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE .            ',I5/,
     .       '    INITIAL PENETRATION DETECTION METHOD  . . .            ',I5/,
     .       '    MAXIMUM INITIAL PENETRATION VALUE . . . . . ',1PG20.13/,
C     .       '    MINIMUM INITIAL PENETRATION VALUE . . . . ',1PG20.13/,
     .       '    START TIME  . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME   . . . . . . . . . . . . . . . . ',1PG20.13/,
C     .       '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . .         ',I10/,
     .       '    DE-ACTIVATION FLAG IF CONFLICT W/ TYPE2 . .       ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . .            ',I5/)
C
C----------   
 3528 FORMAT(//
     .       '    TYPE==24  MUTI-TYPE IMPACTING             ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT . . . . . IN X DIR       ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)                Y DIR       ',I1/,
     .       '                                                   Z DIR       ',I1/,
     .       '    NITSCHE METHOD FOR CONTACT IS USED INSTEAD OF PENALTY'/,
     .       '    STIFFNESS FORMULATION . . . . . . . . . . .            ',I5/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .       '    CONTACT TYPES (1:S1/S1;2:S1/S2;3:N/S  . . .            ',I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . .            ',I5/,
     .       '    ANGLE FOR EDGE COMPUTATION (Iedge>0). . . . ',1PG20.13/,
C     .       '    MAIN SEG EXTENSION . . . . . . . . . .  ',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
C     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    MAXIMUM SECONDARY GAP VALUE . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM MAIN GAP VALUE. . . . . . . . . . . ',1PG20.13/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE .            ',I5/,
     .       '    INITIAL PENETRATION DETECTION METHOD  . . .            ',I5/,
     .       '    MAXIMUM INITIAL PENETRATION VALUE . . . . . ',1PG20.13/,
C     .       '    MINIMUM INITIAL PENETRATION VALUE . . . . ',1PG20.13/,
     .       '    START/STOP TIMES activated by SENSOR ID . .       ',I10/,
c     .       '    BUCKET FACTOR . . . . . . . . . . . . .  ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . .         ',I10/,
     .       '    DE-ACTIVATION FLAG IF CONFLICT W/ TYPE2 . .       ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . .            ',I5/)
C
 3529 FORMAT(//
     .       '    TYPE==24  MUTI-TYPE IMPACTING             ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT . . . . . IN X DIR       ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)                Y DIR       ',I1/,
     .       '                                                   Z DIR       ',I1/,
     .       '    NITSCHE METHOD FOR CONTACT IS USED INSTEAD OF PENALTY'/,
     .       '    STIFFNESS FORMULATION . . . . . . . . . . .            ',I5/,
     .       '    STIFFNESS FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .       '    CONTACT TYPES (1:S1/S1;2:S1/S2;3:N/S  . . .            ',I5/,
     .       '    EDGE FLAG . . . . . . . . . . . . . . . . .            ',I5/,
     .       '    ANGLE FOR EDGE COMPUTATION (Iedge>0). . . . ',1PG20.13/,
C     .       '    MAIN SEG EXTENSION . . . . . . . . . .  ',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
C     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    MAXIMUM SECONDARY GAP VALUE . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM MAIN GAP VALUE. . . . . . . . . . . ',1PG20.13/,
     .       '    ZERO GAP FOR SECONDARY SHELL ON FREE EDGE .            ',I5/,
     .       '    INITIAL PENETRATION DETECTION METHOD  . . .            ',I5/,
     .       '    MAXIMUM INITIAL PENETRATION VALUE . . . . . ',1PG20.13/,
C     .       '    MINIMUM INITIAL PENETRATION VALUE . . . . ',1PG20.13/,
     .       '    START TIME  . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME   . . . . . . . . . . . . . . . . ',1PG20.13/,
C     .       '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . .         ',I10/,
     .       '    DE-ACTIVATION FLAG IF CONFLICT W/ TYPE2 . .       ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . .            ',I5/)
C----------   
C
 3500 FORMAT(/
     .    '    STIFFNESS BASED OF TIME STEP AND MASSES  :'/,
     .    '    SCALE FACTOR . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    TIME STEP  . . . . . . . . . . . . . . . . . . ',1PG20.13/,   
     .    '    FLAG FOR PENALTY STIFFNESS . . . . . . . . . . ',1PG20.13/)   
C
 5030 FORMAT(/
     .       '    INTERFACE FRICTION MODEL.  . . . . . . . . ',I10)
 5040 FORMAT('    PRESS-FIT DURATION TIME .  . . . . . . . . ',1PG20.13/)
C

      END
