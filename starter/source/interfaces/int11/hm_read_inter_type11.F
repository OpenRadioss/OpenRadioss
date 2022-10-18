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
Chd|  HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        INTER_DCOD_FRICTION           source/interfaces/reader/inter_dcod_friction.F
Chd|        INTER_DCOD_SENSOR             source/interfaces/reader/inter_dcod_sensor.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE11(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRSURF    ,IGRSLIN    ,LSENSOR    ,DEF_INTER ,TITR      ,
     3        NOM_OPT    ,UNITAB     ,LSUBMODEL  ,NOM_OPTFRIC,INTBUF_FRIC_TAB,
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*), NOM_OPTFRIC(LNOPT1,*)
      INTEGER NI,NOINT
      INTEGER IPARI(*),DEF_INTER(*),
     .        LSENSOR(NSENSI,*),ID_TYPE19
      my_real
     .   STFAC
      my_real
     .   FRIGAP(*)
      CHARACTER TITR*nchartitle
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   ,TARGET , DIMENSION(NSLIN)   :: IGRSLIN
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
#include      "sysunit.inc"
#include      "thknod_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU1,ISU2,IBC1, IBC2, IBC3, NTYP,INTTH,
     .    INACTI, IBC1M, IBC2M, IBC3M, IGSTI, IS1, IS2,
     .    IGAP,MULTIMP, IDEL11,KK,II, IDSENS,
     .    IPRINT,INTFRIC,IASSIGN,IFORM,IFORM_TH,IDELKEEP,
     .    IKTHE,FIELD6,FLAGREMNOD,IEDGE,IB1,IB2,IB3,NTYP0,
     .    NLIN1,NLIN2,I,MODFR
      my_real
     .   FRIC,GAP,STARTT,BUMULT,STOPT,STMIN,STMAX,
     .   PERCENT_SIZE,VISC,VISCF,TINT,KTHE, XTHE,
     .    FRAD, DRAD,DTMIN,XTHE_UNIT,STFAC_UNIT 
      CHARACTER KEY1*ncharkey
      LOGICAL IS_AVAILABLE
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE11
C=======================================================================
      IS1=0
      IS2=0
      IBC1=0
      IBC2=0
      IBC3=0
      IBC1M=0
      IBC2M=0
      IBC3M=0
      IGSTI = 0        
      INACTI = 0 
      IDSENS = 0  
      IDEL11= 0
      INTTH = 0
      IFORM_TH= 0
      IFORM = 0
      INTFRIC = 0
      IDELKEEP=0
C

      STOPT=EP30
      VISCF = ZERO

C var a initialiser !
      DTMIN=ZERO
      FRIC = ZERO
      GAP = ZERO
      STARTT = ZERO
      VISC = ZERO
      TINT = ZERO
      IKTHE= 0
      KTHE = ZERO
      XTHE = ZERO
      FRAD = ZERO
      DRAD = ZERO
      PERCENT_SIZE = ZERO
C
      IASSIGN = 1
C
      NTYP = 11
      IPARI(7)= NTYP
      IPARI(15)=NOINT
C
      IF (ID_TYPE19 > 0) THEN
C-- type11 generated from type19 - internal id of main type7 must be identified 
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
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ithe',INTTH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Multimp',FIELD6,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NodDel11',IDEL11,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IKREM',FLAGREMNOD,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Deactivate_X_BC',IB1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IB2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IB3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('IFORM',IFORM,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',IDSENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Fric_ID',INTFRIC,IS_AVAILABLE,LSUBMODEL)
C
      IF(INTTH > 0 ) THEN
         CALL HM_GET_INTV('IFORM1',IFORM_TH,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_INTV('fct_ID_k',IKTHE,IS_AVAILABLE,LSUBMODEL)
      ENDIF
C
      IF(NTYP ==  19) THEN
         CALL HM_GET_INTV('IEDGE_TYPE19',FIELD6,IS_AVAILABLE,LSUBMODEL)
      ENDIF
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
C
      CALL HM_GET_FLOATV('STMIN',STMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMAX',STMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('PrMesh_Size',PERCENT_SIZE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin',DTMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('TYPE11_SCALE',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STIFF_DC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV('FRIC_DC',VISCF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SORT_FACT',BUMULT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF(INTTH > 0 ) THEN
         CALL HM_GET_FLOATV('Kthe',KTHE,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('Tint',TINT,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('A_scale_k',XTHE,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('F_RAD',FRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
         CALL HM_GET_FLOATV('D_RAD',DRAD,IS_AVAILABLE,LSUBMODEL,UNITAB)
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
        KEY1='IDEL'
        CALL DEFINTER(KEY1    ,IDEL11  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='INACTI'
        CALL DEFINTER(KEY1     ,INACTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        IF (NTYP == 19) THEN
            MODFR = IFORM
            KEY1='MODFR'
            CALL DEFINTER(KEY1    ,MODFR  ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
            IFORM = MODFR
        ELSE
            KEY1='IFORM'
            CALL DEFINTER(KEY1    ,IFORM  ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
        ENDIF
C
        KEY1='FLAGREMNOD'
        CALL DEFINTER(KEY1     ,FLAGREMNOD,IASSIGN   ,IPRINT   ,
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
          IF (IEDGE==1) THEN
C-- /LINE/EDGE are used instead of /LINE/SURFACE
            ISU1 = ISU1 + 1
            ISU2 = ISU2 + 1 
          ENDIF
C
          IF (IGAP == 4) THEN
C-- IGAP->4 constant gap = gapmin for edge/edge in TYPE19 contact
            IGAP = 0          
          ENDIF
C
        ENDIF
C
C--------------------------------------------------
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------

C
C....* Card1 :flags  *.............
C

C-- for TYPE11 field6 is multimp (obsolete)
        MULTIMP = 4
        IPARI(23)=MULTIMP

        IF(IGAP == 2) THEN
           IF (IPARI(71)==0) THEN
              CALL ANCMSG(MSGID=1056,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR)
           ELSE
              CALL ANCMSG(MSGID=1181,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR)
           ENDIF
        ELSEIF (IGAP==3) THEN
           IF (FLAGREMNOD == 2 )THEN
              FLAGREMNOD = 1
           ENDIF
        ENDIF
C
        IF(ISU1==0) THEN
           CALL ANCMSG(MSGID=118,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=NOINT,
     .                 C1=TITR)
        ENDIF
        IF(ISU2==0)ISU2=ISU1
        IF(ISU1==ISU2)THEN
           IPARI(42)=1
        ELSE
           IPARI(42)=0
        ENDIF
        IS1=3
        IS2=3
        INGR2USR => IGRSLIN(1:NSLIN)%ID
        ISU1=NGR2USR(ISU1,INGR2USR,NSLIN)
        ISU2=NGR2USR(ISU2,INGR2USR,NSLIN)
C
        IF (IDEL11 < 0) THEN
           IDELKEEP=1
           IDEL11=ABS(IDEL11)
        END IF
        IF (IDEL11>2.OR.N2D==1) IDEL11 = 0
        IPARI(17)=IDEL11
        IPARI(61)=IDELKEEP
C
        IF (FLAGREMNOD == 0) FLAGREMNOD = 1
        IPARI(63) = FLAGREMNOD
C
        IPARI(45)=ISU1
        IPARI(46)=ISU2
        IPARI(13)=IS1*10+IS2
        IPARI(21 )= IGAP
        IPARI(22)=INACTI
C
C....* Card2  *.............
C
        IF(IGSTI/=1)THEN
           IF(STMAX==ZERO)STMAX=EP30
        ELSE
           STMIN = ZERO
           STMAX = EP30
        END IF

        IF (IFORM == 0) IFORM = 1
        IPARI(30) = IFORM
C
        FRIGAP(41) = DTMIN
C
        IF(IGAP == 3 .AND. PERCENT_SIZE == ZERO)
     .      PERCENT_SIZE = 0.4

        FRIGAP(28) = PERCENT_SIZE
        IPARI(64) = IDSENS
        FRIGAP(17) = STMIN
        FRIGAP(18) = STMAX

C
C....* Card3  *.............
C
        IF (STFAC == ZERO) THEN
        !units
          CALL HM_GET_FLOATV_DIM('TYPE11_SCALE' ,STFAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          STFAC = ONE * STFAC_UNIT
        ENDIF
        IF (IGSTI == 1)STFAC=-STFAC
        IF (STFAC < ZERO) IGSTI = 1
        IF(IGSTI==0)IGSTI=5
C
        IF (STOPT == ZERO) STOPT = EP30

        IPARI(34)=IGSTI
        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT
C
C....* Card4  *.............
C

        IF (IB1==1) IBC1  = 1
        IF (IB2==1) IBC2  = 1
        IF (IB3==1) IBC3  = 1

C Hidden flag no more read using HM reader
c        IF (BCFLAGM(LFIELD-2:LFIELD-2)== '1') IBC1M = 1
c        IF (BCFLAGM(LFIELD-1:LFIELD-1)== '1') IBC2M = 1
c        IF (BCFLAGM(LFIELD  :LFIELD  )== '1') IBC3M = 1


        IF(VISC==ZERO)VISC=FIVEEM2

        IF(FRIC/=ZERO.AND.VISCF==ZERO.AND.IFORM/=2)VISCF=ONE

        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
        FRIGAP(14)=VISC
        FRIGAP(15)=VISCF**2
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
C....* Card5 : Friction model *.............
C

        IPARI(72) =  INTFRIC
C
C....* Card6 : THERMAL MODELLING card1 *.............
C
        IF(INTTH > 0 ) INTHEAT = 1
        IF(KTHE == ZERO)THEN
           IF(IKTHE /= 0)THEN
              KTHE = ONE
           ENDIF
        ENDIF
        IF (XTHE == ZERO) THEN
        !units
          CALL HM_GET_FLOATV_DIM('A_scale_k' ,XTHE_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XTHE = ONE * XTHE_UNIT
        ENDIF

C
C....* Card7 : THERMAL MODELLING card2 *.............
C
C
        IF(ITHERM_FE == 0 .AND. INTTH > 0 ) THEN
            INTHEAT   = 0
            IPARI(47) = 0
            CALL ANCMSG(MSGID=702,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=NOINT,
     .                  C1=TITR)
        ENDIF
C
        IPARI(47) = INTTH
        IPARI(44)   = IFORM_TH
        FRIGAP(20)  = KTHE
        FRIGAP(21 ) = TINT
        IPARI(43)   = IKTHE
        FRIGAP(22)  = XTHE
        FRIGAP(23)  = FRAD
        FRIGAP(24)  = DRAD

C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)
C
        NLIN1 = IGRSLIN(ISU1)%NSEG
        NLIN2 = IGRSLIN(ISU2)%NSEG
        IF ((NTYP == 19).AND.((NLIN1==0).OR.(NLIN2==0))) THEN
C-- For type19 if line1 or lin2 is empty type11 is deactivated           
            NTYP = 0
            IPARI(7) = NTYP
            CALL ANCMSG(MSGID=1577,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=IPARI(15),
     .                  C1=TITR)
        ENDIF        
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS AND SENSOR - USER TO INTERNAL ID
C------------------------------------------------------------
C
        NTYP0 = 11
        CALL INTER_DCOD_SENSOR (NTYP0,NI,IPARI,NOM_OPT,LSENSOR)
        CALL INTER_DCOD_FRICTION(NTYP0,NI,IPARI,NOM_OPT,NOM_OPTFRIC,
     .           INTBUF_FRIC_TAB)
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
C--    No printout for type11 of interface type19
       IF (ID_TYPE19 > 0) RETURN

       IPRINT = 1

C........* PRINT DEFAULT VALUES *.....................
C
C
        KEY1='IGSTI'
        CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IGAP'
        CALL DEFINTER(KEY1     ,IGAP     ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IDEL'
        CALL DEFINTER(KEY1     ,IDEL11   ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IFORM'
        CALL DEFINTER(KEY1    ,IFORM  ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='INACTI'
        CALL DEFINTER(KEY1     ,INACTI   ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='FLAGREMNOD'
        CALL DEFINTER(KEY1     ,FLAGREMNOD,IASSIGN  ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
C........* PRINT INTERFACES INPUT *.....................
C
        IF(IGSTI/=1)THEN
         IF(IDSENS/=0) THEN
           WRITE(IOUT,1511)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 IGSTI,STMIN,STMAX,STFAC,
     .                 FRIC,IGAP,GAP,IDSENS,          
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP,FLAGREMNOD
         ELSE
           WRITE(IOUT,1592)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 IGSTI,STMIN,STMAX,STFAC,
     .                 FRIC,IGAP,GAP,STARTT,STOPT,        
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP,FLAGREMNOD
         ENDIF
        ELSE
         IF(IDSENS/=0) THEN
           WRITE(IOUT,1591)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 -STFAC,FRIC,IGAP,GAP,IDSENS,       
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP,FLAGREMNOD
         ELSE
           WRITE(IOUT,1593)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 -STFAC,FRIC,IGAP,GAP,STARTT,STOPT,  
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP,FLAGREMNOD
         ENDIF
        ENDIF
        IF(IFORM == 2) THEN
          WRITE(IOUT,1518)
        ELSE
          WRITE(IOUT,1519)
        ENDIF
        IF (DTMIN/=ZERO) THEN
          WRITE(IOUT,1577)DTMIN
        ENDIF
        IF(IDEL11/=0) THEN
          WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',
     . ' (1:YES-ALL/2:YES-ANY) SET TO ',IDEL11
          IF(IDELKEEP == 1)THEN
            WRITE(IOUT,'(A)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
          ENDIF         
        ENDIF
        IF(INTTH > 0 )THEN
         IF(IKTHE==0)THEN
           WRITE(IOUT,2504) KTHE,TINT,FRAD,DRAD
         ELSE
           WRITE(IOUT,2505) IKTHE,XTHE,KTHE,TINT,FRAD,DRAD
         END IF
        ENDIF
         IF(INTFRIC > 0 ) WRITE(IOUT,5030) INTFRIC
C--------------------------------------------------------------

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
 1300 FORMAT(    /1X,'   INTERFACES               ' /
     .            1X,' --------------             '// )
C------------
      RETURN
C------------
 1518 FORMAT( '    FRICTION FORMULATION: INCREMENTAL (STIFFNESS) ',
     .             'FORMULATION')
 1519 FORMAT( '    FRICTION FORMULATION: TOTAL (VISCOUS) ',
     .             'FORMULATION')
 1577 FORMAT(
     .       '    MINIMUM TIME STEP . . . . . . . . . . . . ',1PG20.13/)
 1511 FORMAT(//
     .    '    TYPE==11   EDGE to EDGE AUTO IMPACTING    ' //,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .       '    STIFFNESS FORMULATION .  . . . . . . . . ',I1/,
     .' 2,3,4,5 : STIFFNESS IS COMPUTED FROM STIFFNESS ON MAIN SIDE'/,
     .'                                 AND STIFFNESS AT SECONDARY NODES'/,
     .    '    MINIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .    '    MAXIMUM STIFFNESS. . . .  . . . . . . . . . ',1PG20.13/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME/STOP TIME ACTIVATED BY SENSOR ID ',I10/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . . ',I10/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/LINE . . . . ',I5/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . ',I5/)
 1592 FORMAT(//
     .    '    TYPE==11   EDGE to EDGE AUTO IMPACTING    ' //,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .       '    STIFFNESS FORMULATION .  . . . . . . . . ',I1/,
     .' 2,3,4,5 : STIFFNESS IS COMPUTED FROM STIFFNESS ON MAIN SIDE'/,
     .'                                 AND STIFFNESS AT SECONDARY NODES'/,
     .    '    MINIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .    '    MAXIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/LINE . . . ',I5/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . ',I5/)
 2504   FORMAT(//
     .      '    Thermal interface                          ' //,
     .      '    Thermal heat exchange coefficient. . . . .',1PG20.13/,
     .      '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .      '    Radiation factor . . . . . . . . . . . . . ',1PG20.13/,
     .      '    Maximum distance for radiation computation.',1PG20.13)
 2505   FORMAT(//
     .      '    Thermal interface                          ' //,
     .  '   Function for thermal heat exchange coefficient wrt contact pressure',I10/,
     .      '    Abscissa scale factor on IFUNTCK. . . .  . ',1PG20.13/,
     .      '    Ordinate scale factor on IFUNTCK .  . .  . ',1PG20.13/,
     .      '    Interface temperature  . . . . . . . . . . ',1PG20.13/,
     .      '    Radiation factor . . . . . . . . . . . . . ',1PG20.13/,
     .      '    Maximum distance for radiation computation.',1PG20.13)

 1591 FORMAT(//
     .    '    TYPE==11  EDGE to EDGE AUTO IMPACTING     ' //,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    CONSTANT STIFFNESS. . . . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME/STOP TIME ACTIVATED BY SENSOR ID ',I10/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . . ',I10/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/LINE . . . . ',I5/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . ',I5/)
 1593 FORMAT(//
     .    '    TYPE==11  EDGE to EDGE AUTO IMPACTING     ' //,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    CONSTANT STIFFNESS. . . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I5/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/LINE . . . ',I5/,
     .    '    FLAGREMNODE : NOT CONSIDER NEIGHBORING  . . ',I5/)
C
 5030 FORMAT(/
     .       '    INTERFACE FRICTION MODEL.  . . . . . . . . ',I10)
C  
      END
