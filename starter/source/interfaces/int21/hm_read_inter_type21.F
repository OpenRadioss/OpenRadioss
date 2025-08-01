Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_inter_type21   ../starter/source/interfaces/int21/hm_read_inter_type21.F
!||--- called by ------------------------------------------------------
!||    hm_read_inter_struct   ../starter/source/interfaces/reader/hm_read_inter_struct.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim      ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    inter_dcod_friction    ../starter/source/interfaces/reader/inter_dcod_friction.F
!||    inter_dcod_function    ../starter/source/interfaces/reader/inter_dcod_function.F
!||    inter_dcod_sensor      ../starter/source/interfaces/reader/inter_dcod_sensor.F
!||    ngr2usr                ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_INTER_TYPE21(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT    ,NI        ,
     2        IGRSURF    ,XFILTR     ,FRIC_P     ,NPC1     ,SENSORS   ,
     3        NOM_OPT    ,UNITAB     ,LSUBMODEL  ,TITR     ,NPC       ,
     4        TF         ,NPARI      ,NPARIR     ,SNPC     ,SNPC1     ,
     5        LNOPT1     ,ITHERM_FE  ,INTHEAT    ,NOM_OPTFRIC ,INTBUF_FRIC_TAB )
C============================================================================
C    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE UNITAB_MOD
      USE SENSOR_MOD
      USE INTBUFDEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY, NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NPARI,NPARIR,SNPC,SNPC1,LNOPT1
      INTEGER,INTENT(IN)    :: ITHERM_FE
      INTEGER,INTENT(INOUT) :: INTHEAT
      INTEGER,INTENT(IN)    :: NOM_OPTFRIC(LNOPT1,NINTERFRIC)
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER ISU1,ISU2,ILAGM,NI
      INTEGER IPARI(NPARI),NPC1(SNPC1),NPC(SNPC)
      my_real STFAC,XFILTR
      my_real FRIGAP(NPARIR),FRIC_P(10),TF(*)
      CHARACTER(LEN=NCHARTITLE),INTENT(IN) :: TITR
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
      TYPE (SENSORS_)  ,INTENT(IN) :: SENSORS
      TYPE(INTBUF_FRIC_STRUCT_),INTENT(INOUT) ::  INTBUF_FRIC_TAB(NINTERFRIC)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "intstamp_c.inc"
#include      "units_c.inc"
#include      "scr12_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=NCHARTITLE) :: TITR1
      INTEGER I,J,L,IBC1, IBC2, IBC3, NOINT, NTYP, IBID,INACTI, IBC1M,
     .    IBC2M, IBC3M, IGSTI,IEULT, IVIS2,IS1, IS2,ILEV, IGAP,MULTIMP,
     .    HIERA,MFROT,IFQ,MODFR,IADM,UID,NRADM,INTTH,IFORM,IERR1,IKTHE,
     .    IDEL21,IMOD,IFILTR,IFUNS,IFUNN,IFUNT,IFUN1,IFUN2,HFLAG,NUVAR, 
     .    IFSTF,KK,II,IGAP0,FLAGREMNOD,IDSENS,IDELKEEP,INTKG,IRSTH,INVN,
     .    IFRICTH,IFTLIM,FCOND,INTFRIC
      my_real
     .   FRIC,GAP,STARTT,BUMULT,STOPT,FHEAT,FHEATS,FHEATM,RSTH,TINT,PADM, 
     .   ANGLADM, CADM, DEPTH,C1,C2,C3,C4,C5,C6, ALPHA,GAPSCALE,GAPMAX, 
     .   STMIN,STMAX,STIFF, PMAX,AREA, KTHE, XTHE, FRAD, DRAD,
     .   VISC,XFRICTH,PTMAX,PSKID,DCOND,XTHE_UNIT,KTHE_UNIT
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
CC=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE21
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
      MFROT=0
      IFQ=0
      IGSTI = 0        
      INVN = 0
      IFTLIM=1 
      IADM =0
      NRADM=1    
      INACTI = 0
      ILEV = 0
      IDSENS = 0
      IDEL21= 0
      IDELKEEP=0
      INTTH = 0
      IFRICTH = 0
      IKTHE= 0
      IFORM = 0
      INTKG = 0
      FCOND = 0
      IGAP = 0
      MULTIMP = 0
      INTFRIC = 0
C
      PADM =ONE
      ANGLADM=ZERO
      STOPT=EP30
      FRIC = ZERO
      GAP = ZERO
      GAPSCALE = ZERO
      STARTT = ZERO
      VISC = ZERO
      XFILTR = ZERO

      DO I = 1, 10
         FRIC_P(I) = ZERO
      ENDDO

      KTHE = ZERO
      XTHE = ZERO
      TINT = ZERO
      FRAD = ZERO
      DRAD = ZERO
      FHEAT= ZERO
      C1=ZERO
      C2=ZERO
      C3=ZERO
      C4=ZERO
      C5=ZERO
      C6=ZERO
      GAPMAX=EP30
      GAPSCALE = ZERO
      DCOND = ZERO
      DEPTH = ZERO
C
      NINTSTAMP=NINTSTAMP+1
      IPARI(43)=NINTSTAMP
C
      NTYP=21
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ithe',INTTH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Multimp',MULTIMP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE21_Idel',IDEL21,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('TYPE21_Invn',INVN,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iadm',IADM,IS_AVAILABLE,LSUBMODEL)
C
       IF(IGAP==1.OR.IGAP==2) CALL HM_GET_INTV('TYPE21_ITim',IFTLIM,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Deactivate_X_BC',IBC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IBC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IBC3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',IDSENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Crx_Fun',IFRICTH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Fric_ID',INTFRIC,IS_AVAILABLE,LSUBMODEL)
C
      IF(IADM==2) CALL HM_GET_INTV('NRadm',NRADM,IS_AVAILABLE,LSUBMODEL)
C
      IF(INTTH > 0) THEN
        CALL HM_GET_INTV('Ithe_form',IFORM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('fct_ID_k',IKTHE,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('F_COND',FCOND,IS_AVAILABLE,LSUBMODEL)
      ENDIF
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      IF(IGAP==1.OR.IGAP==2)THEN
         CALL HM_GET_FLOATV('GAPSCALE',GAPSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('GAPMAX',GAPMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('DIST',DEPTH,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('PMAX',PMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
C
      CALL HM_GET_FLOATV('STMIN',STMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMAX',STMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Pskid',PSKID,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STFAC',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MINI',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('VISC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB) 
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
      IF(INTTH > 0) THEN
        CALL HM_GET_FLOATV('Kthe',KTHE,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Tint',TINT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('A_scale_k',XTHE,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('F_RAD',FRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('D_RAD',DRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('HEAT_AL',FHEAT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('D_COND',DCOND,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF

C------------------------------------------------------------
C  Card1 :flags
C------------------------------------------------------------

C
C.....* CHECKS *........
C
       IF (IDEL21 < 0) THEN
          IDELKEEP=1
          IDEL21=ABS(IDEL21)
       END IF
C
       IF (IDEL21>2.OR.N2D==1) IDEL21 = 0
C
       IF(ILEV==0)THEN
          ILEV=1
       ELSEIF(ILEV==-1)THEN
C emul past
          ILEV=0
       END IF
C
       IS1=1
       INGR2USR => IGRSURF(1:NSURF)%ID
       ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
       IS2=1
       ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
C
        IF (IADM/=0.AND.NADMESH==0) THEN
             CALL ANCMSG(MSGID=647,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IADM=0
        ENDIF

C.......* Storage IPARI FRIGAP *........
       IPARI(21) = IGAP
       IPARI(61) = IDELKEEP
       IPARI(17) = IDEL21
       IPARI(20) = ILEV
       IPARI(34) = IGSTI
       IPARI(51) = INVN  ! flag to detect if normals are inverted (default =0)
       IPARI(45)=ISU1
       IPARI(46)=ISU2
       IPARI(13)=IS1*10+IS2
       IPARI(44)=IADM

        IF(MULTIMP==0)THEN
          MULTIMP=12
        END IF
        IPARI(23)=MULTIMP

C------------------------------------------------------------
C  Card2 
C------------------------------------------------------------
        IF(IGAP==1.OR.IGAP==2)THEN
C
C.....* CHECKS *........
C
          IF(GAPSCALE==ZERO)GAPSCALE=ONE

ce flag dit sil faut calculer les epaisseurs nodales dans l'engine
          IF(IGAP==2) INTER_ITHKNOD=1 !defined in interface module (common_source directory)
C
          IF(IGAP==2)THEN
              IF(PMAX==ZERO) PMAX=EP30
          ELSE
C Igap=1, Pmax is not used
              PMAX=EP30
          END IF
C

        ELSE
C Igap=0, Pmax is not used
            PMAX=EP30
        ENDIF

C.......* Storage IPARI FRIGAP *........
        FRIGAP(19) = GAPSCALE
        FRIGAP(16) = GAPMAX
        FRIGAP(23) = DEPTH
        FRIGAP(15) = PMAX
        IPARI(52) = IFTLIM  ! flag to deactivate tangential force limitation (default =1)

C------------------------------------------------------------
C  Card3 
C------------------------------------------------------------

C
C.....* CHECKS *........
C
        IF(IGSTI==0)THEN
          I7STIFS=1
          IF(STMAX==ZERO)STMAX=EP30
          FRIGAP(17) = STMIN
          FRIGAP(18) = STMAX
        ELSE
          STMIN = ZERO
          STMAX = EP30
        END IF
C
        IF(PSKID==ZERO) PSKID=EP30
        FRIGAP(35) = PSKID
C------------------------------------------------------------
C  Card3 
C------------------------------------------------------------

C
C.....* CHECKS *........
C
        IF(STFAC==ZERO) THEN
           STFAC=ONE
        ENDIF
        IF(IGSTI==1)STFAC=-STFAC
C
        IF (STOPT == ZERO) STOPT = EP30

C.....* Storage IPARI FRIGAP *.......
        IPARI(34)=IGSTI
        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT

C------------------------------------------------------------
C  Card4 
C------------------------------------------------------------
        IF(VISC==ZERO) VISC=ONE

C.....* Storage IPARI FRIGAP *.........

        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
        IPARI(22)=INACTI
        FRIGAP(14)=VISC

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
C------------------------------------------------------------
C  Card5 : FRICTION data
C------------------------------------------------------------

C
C.....* CHECKS *........
C
        IF(FRIC == ZERO)THEN
          IF(IFRICTH /= 0)THEN
             FRIC = ONE
          ENDIF
        ENDIF

        IF(XFRICTH == ZERO) XFRICTH=ONE
C
        IF (ALPHA==ZERO) IFQ = 0
c
        MODFR=2
        IFQ = IFQ + 10
c
        IF (IFQ>0) THEN
            IF (IFQ==10) XFILTR = ONE
            IF (MOD(IFQ,10)==1) XFILTR = ALPHA
            IF (MOD(IFQ,10)==2) XFILTR=FOUR*ATAN2(ONE,ZERO) / ALPHA
            IF (MOD(IFQ,10)==3) XFILTR=FOUR*ATAN2(ONE,ZERO) * ALPHA
            IF (XFILTR<ZERO) THEN
              CALL ANCMSG(MSGID=554,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=NOINT,
     .                    C1=TITR,
     .                    R1=ALPHA)
            ELSEIF (XFILTR>1.AND.MOD(IFQ,10)<=2) THEN
               CALL ANCMSG(MSGID=554,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=NOINT,
     .                     C1=TITR,
     .                     R1=ALPHA)
            ENDIF
        ELSE
            XFILTR = ZERO
        ENDIF
C------------------------------------------------------------
C  Optional Card5 Card6 : C1...C5.C6 friction data 
C------------------------------------------------------------

C.....* Storage IPARI FRIGAP *.........
        FRIC_P(1) = C1
        FRIC_P(2) = C2
        FRIC_P(3) = C3
        FRIC_P(4) = C4
        FRIC_P(5) = C5
        FRIC_P(6) = C6
        IPARI(50) =  IFRICTH
        FRIGAP(34) =  XFRICTH
        IPARI(30) = MFROT
        IPARI(31) = IFQ
        IPARI(72) =  INTFRIC

C------------------------------------------------------------
C  Option Card8 :Adm = 2 adaptative mesh
C------------------------------------------------------------
C
        IF(IADM==2)THEN
          IF(NRADM==0)   NRADM  =3
          IF(PADM==ZERO) PADM   =ONE     
        ELSE
          NRADM  =1
          PADM   =ONE
          ANGLADM=ZERO
        END IF

C.....* Storage IPARI FRIGAP *.........
        CADM      =COS(ANGLADM*PI/HUNDRED80)
        KCONTACT =MAX(KCONTACT,IADM)
        IPARI(49) =NRADM
        FRIGAP(24)=PADM
        FRIGAP(25)=CADM
C------------------------------------------------------------
C  Option Card9 :Thermal input
C------------------------------------------------------------
        IF(INTTH > 0 ) THEN
           INTHEAT = 1
        ENDIF
C
C....* CHECKS *.............
C
        IF(KTHE == ZERO)THEN
          IF(IKTHE /= 0)THEN
            KTHE = ONE
            CALL HM_GET_FLOATV_DIM('Kthe' ,KTHE_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
             KTHE = ONE * KTHE_UNIT
          ENDIF
        ENDIF

        IF (XTHE == ZERO) THEN
        !units
          CALL HM_GET_FLOATV_DIM('A_scale_k' ,XTHE_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XTHE = ONE * XTHE_UNIT
        ENDIF

        IF(IFORM /= 0)THEN
           INTTH = 2
           IPARI(47) = 2 
        ENDIF

C.....* Storage IPARI FRIGAP *.........
        IPARI(47) = INTTH
        IPARI(48)   = IFORM
        IPARI(42)   = IKTHE
        FRIGAP(21 ) = TINT
        FRIGAP(30)  = XTHE
C------------------------------------------------------------
C  Card5 : THERMAL MODELLING card2
C------------------------------------------------------------

C
C....* CHECKS *.............
C
        IF(ITHERM_FE == 0 .AND. INTTH > 0 ) THEN
          INTHEAT   = 0
          IPARI(47) = 0
          CALL ANCMSG(MSGID=702,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=NOINT,
     .                C1=TITR,
     .                R1=DCOND,
     .                R2=DRAD)
        ENDIF

        IF(INTTH == 2  ) FTEMPVAR21 = 1

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

C
C.....* Storage IPARI FRIGAP *.........
        FRIGAP(20)  = KTHE
        FRIGAP(31)  = FRAD
        FRIGAP(32)  = DRAD
        FRIGAP(33)  = FHEAT
        FRIGAP(36)  = DCOND  ! max conduction distance
        IPARI(53)   = FCOND  ! function of variation of heat exchange as funct of distance
C
        IPARI(65) = INTKG
C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)
C
C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS AND SENSOR - USER TO INTERNAL ID
C------------------------------------------------------------
C
        CALL INTER_DCOD_FUNCTION(NTYP,NI,IPARI,NPC1,NOM_OPT,NPC,TF)
        CALL INTER_DCOD_SENSOR (NTYP,NI,IPARI,NOM_OPT,SENSORS)
        CALL INTER_DCOD_FRICTION(NTYP,NI,IPARI,NOM_OPT,NOM_OPTFRIC,
     .                INTBUF_FRIC_TAB)

C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
        IF(IDSENS/=0) THEN 
          WRITE(IOUT,2101)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 ILEV,IGSTI,STFAC,STMIN,STMAX,
     .                 FRIC,IGAP,GAP,GAPMAX,GAPSCALE,IDSENS,
     .                 BUMULT,INACTI,VISC,PMAX,MULTIMP,INVN,IFTLIM,
     .                 PSKID
        ELSE
          WRITE(IOUT,2104)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 ILEV,IGSTI,STFAC,STMIN,STMAX,
     .                 FRIC,IGAP,GAP,GAPMAX,GAPSCALE,STARTT,STOPT,
     .                 BUMULT,INACTI,VISC,PMAX,MULTIMP,INVN,IFTLIM,
     .                 PSKID
        ENDIF

        IF(INTFRIC > 0 ) THEN
          WRITE(IOUT,1527) INTFRIC
        ELSE
C
          WRITE(IOUT,1520)MOD(IFQ,10), XFILTR
C
          IF(MFROT==0)THEN
             IF(IFRICTH ==0 ) THEN
               WRITE(IOUT,1524) FRIC
             ELSE
               WRITE(IOUT,1525) IFRICTH,XFRICTH,FRIC
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
          ELSEIF(MFROT==4)THEN
            WRITE(IOUT,1526)  FRIC,FRIC_P(1),FRIC_P(2)
          ENDIF
        ENDIF
C
        IF(IDEL21/=0) THEN
          WRITE(IOUT,'(A,I5/)')
     . '    DELETION FLAG ON FAILURE (1:YES) : ',IDEL21
          IF(IDELKEEP == 1)THEN
            WRITE(IOUT,'(A)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
          ENDIF         
        ENDIF
        IF(IADM/=0) THEN
         WRITE(IOUT,*)'    MESH REFINEMENT CASE OF CONTACT',
     . ' (0:NO/1:DUE TO CURVATURE/2:DUE TO CURVATURE OR PENETRATION)',
     . ' SET TO ',IADM
          IF(IADM==2)THEN
             WRITE(IOUT,1557) NRADM,PADM,ANGLADM
          END IF
        ENDIF
C
       IF(INTTH > 0 )THEN
         IF(IKTHE==0)THEN
           WRITE(IOUT,2102) KTHE,TINT,FRAD,DRAD,FHEAT,IFORM,FCOND,DCOND
         ELSE
           WRITE(IOUT,2103) IKTHE,XTHE,KTHE,TINT,FRAD,DRAD,FHEAT,IFORM,FCOND,DCOND
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
 1534 FORMAT(
     .    '    THERMAL INTERFACE . . . . . . . . . . . . . ',//
     .    '    THERMAL heat exchange coefficient .. . . . .',1PG20.13/)
 2502 FORMAT('    RUPTURE PARAMETERS '
     .         /10X,'SCAL_F  . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'SCAL_DISP . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'SCAL_SR . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'FILTERING COEFF . . . . . . . . . . ',1PG20.13
     .         /10X,'DEFAULT SECONDARY AREA. . . . . . . . . ',1PG20.13
     .         /10X,'DN_MAX  . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'DT_MAX  . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'IFUNN   . . . . . . . . . . . . . . ',I10
     .         /10X,'IFUNT   . . . . . . . . . . . . . . ',I10
     .         /10X,'IFUNS   . . . . . . . . . . . . . . ',I10
     .         /10X,'IMOD    . . . . . . . . . . . . . . ',I10
     .         /10X,'ISYM    . . . . . . . . . . . . . . ',I10
     .         /10X,'IFILTR  . . . . . . . . . . . . . . ',I10//)
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
C
 1527 FORMAT(/
     .       '    INTERFACE FRICTION MODEL.  . . . . . . . . ',I10)

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
 2101 FORMAT(//
     .       '    TYPE==21   PARALLEL/STAMPING              ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    FORMULATION LEVEL. . . . . . . . . . . . . .',I1/,
     .       '             (0:OLD, 1:OPTIMIZED). . . . . . . .',/,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I1/,
     .'       0 : STIFFNESS IS COMPUTED FROM STIFFNESS ON SECONDARY SIDE'/,
     .'       1 : STFAC IS A STIFFNESS VALUE                      '/,
     .       '    STIFFNESS FACTOR OR STIFFNESS VALUE . . . . ',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . . ',I5/,
     .       '    MINIMUM GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM GAP (= 0. <=> NO MAXIMUM GAP) . . . ',1PG20.13/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME/STOP TIME activated by SENSOR ID ',I10/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . . ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM PRESSURE DUE TO THICKNESS VARIATION '/,
     .       '                    CASE OF IGAP=2. . . . . . . ',1PG20.13/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . . ',I5/,
     .       '    INVERTED NORMALS DETECTION FLAG . . . . . . ',I5/,
     .       '    TANGENTIAL FORCE LIMITATION FLAG . . . . . . ',I5/,
     .       '    MAXIMUM PRESSURE FOR SKID LINE OUTPUT        ',1PG20.13)
 2104 FORMAT(//
     .       '    TYPE==21   PARALLEL/STAMPING              ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    FORMULATION LEVEL. . . . . . . . . . . . . .',I1/,
     .       '             (0:OLD, 1:OPTIMIZED). . . . . . . .',/,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I1/,
     .'       0 : STIFFNESS IS COMPUTED FROM STIFFNESS ON SECONDARY SIDE'/,
     .'       1 : STFAC IS A STIFFNESS VALUE                      '/,
     .       '    STIFFNESS FACTOR OR STIFFNESS VALUE . . . ',1PG20.13/,
     .       '    MINIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .       '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .       '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .       '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM GAP (= 0. <=> NO MAXIMUM GAP) . . ',1PG20.13/,
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .       '    MAXIMUM PRESSURE DUE TO THICKNESS VARIATION'/,
     .       '                    CASE OF IGAP=2. . . . . . ',1PG20.13/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/,
     .       '    INVERTED NORMALS DETECTION FLAG . . . . . . ',I5/,
     .       '    TANGENTIAL FORCE LIMITATION FLAG . . . . . . ',I5/,
     .       '    MAXIMUM PRESSURE FOR SKID LINE OUTPUT         ',1PG20.13)        
 2102   FORMAT(//
     .      '    Thermal interface                          ' //,
     .      '    Thermal heat exchange coefficient . . . . .',1PG20.13/,
     .      '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .      '    Radiation factor . . . . . . . . . . . . . ',1PG20.13/,
     .      '    Maximum distance for radiation computation.',1PG20.13/,
     .      '    Frictional heat transfer. . . . . . . . . .',1PG20.13/,
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
     .      '    Frictional heat generation . . . . . . . . ',1PG20.13/,
     .      '    FORMULATION CHOICE : . . . . . . . . . . . ',I10,/,
     .      '          0 : heat transfer between SECONDARY side',/,
     .      '                 and constant temperature in interface',/,
     .      '          1 : heat Exchange between pieces in contact'/
     .  '   Function for thermal heat exchange coefficient wrt distance',I10/,
     .      '    Maximum distance for conductive heat exchange',1PG20.13)
C  
      END
