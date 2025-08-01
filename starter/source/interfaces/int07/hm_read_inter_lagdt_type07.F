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
!||    hm_read_inter_lagdt_type07   ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
!||--- called by ------------------------------------------------------
!||    hm_read_interfaces           ../starter/source/interfaces/reader/hm_read_interfaces.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                       ../starter/source/output/message/message.F
!||    definter                     ../starter/source/interfaces/interf1/definter.F
!||    fretitl                      ../starter/source/starter/freform.F
!||    hm_get_floatv                ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv                  ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    ngr2usr                      ../starter/source/system/nintrr.F
!||    usr2sys                      ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    message_mod                  ../starter/share/message_module/message_mod.F
!||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_INTER_LAGDT_TYPE07(
     1        IPARI       ,STFAC      ,FRIGAP         ,XFILTR   ,FRIC_P   ,
     2        IGRNOD      ,IGRSURF    ,ILAGM         ,UNITAB    ,NI       ,
     4        NOM_OPT     ,TITR       ,DEF_INTER     ,NOINT     ,LSUBMODEL,
     5        NPARI       ,NPARIR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA, NSUBMOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPARI, NPARIR !< array sizes IPARI and FRIGAP
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER ISU1,ISU2,ILAGM,NI, NOINT
      INTEGER IPARI(NPARI),DEF_INTER(100)
      my_real STFAC,XFILTR
      my_real FRIGAP(NPARIR),FRIC_P(10)
      CHARACTER(LEN=NCHARTITLE) :: TITR,TITR1
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr12_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,IBC1, IBC2, IBC3, NTYP,IDELKEEP,
     .    INACTI, IBC1M, IBC2M, IBC3M, IGSTI, IVIS2,
     .    IS1, IS2, ISU10,ISU20,ILEV, IGAP,MULTIMP,MFROT,IFQ,
     .    IBAG,IDEL7N,MODFR,IADM,IFORM,IASSIGN,FIELD6,IPRINT
      my_real
     .   FRIC,GAP,STARTT,BUMULT,STOPT,C1,C2,C3,C4,C5,C6,
     .   ALPHA,GAPSCALE,GAPMAX,STMIN,STMAX,
     .   VISC,VISCF
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      CHARACTER(LEN=NCHARKEY) ::  KEY1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,SUR2USR,NGR2USR
      LOGICAL IS_AVAILABLE

CC=======================================================================
C     READING Automatic Penalty Lagrange INTERFACES /INTER/LAGDT/TYPE7
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
      IBAG=0
      IGSTI = 0  
      IVIS2 = 0
C
      IADM =0
C
      STOPT=EP30
      INACTI = 0
      VISCF = ZERO
C
      FRIC = ZERO
      GAP = ZERO
      GAPSCALE = ZERO
      STARTT = ZERO
      VISC = ZERO
      XFILTR = ZERO
      DO I = 1, 10
         FRIC_P(I) = ZERO
      ENDDO
      IDEL7N = 0
      IDELKEEP=0
C
      C1=ZERO
      C2=ZERO
      C3=ZERO
      C4=ZERO
      C5=ZERO
      C6=ZERO
C
      IFORM = 0
C
      GAPSCALE = ZERO
      IASSIGN = 1
C
C
C---------------------------------
C     Automatic Penalty Lagrange
C---------------------------------
      ILAGM = 2
      IPARI(33) = MAX(0,ILAGM)
      NOM_OPT(1,NI)=NOINT
      NTYP = 7
      IPARI(7)=NTYP
      IPARI(15)=NOINT

C----PRINT OUT  Title + NOINT --------------------------------
      WRITE(IOUT,1000) NOINT,TRIM(TITR)
C---------------------------------
      CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C---------------------------------
C     Reading Input INTER Card
C---------------------------------
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------

      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Multimp',FIELD6,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ibag',IBAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idel7',IDEL7N,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Deactivate_X_BC',IBC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IBC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IBC3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IFORM',MODFR,IS_AVAILABLE,LSUBMODEL)
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('GAPSCALE',GAPSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAPMAX',GAPMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STMIN',STMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMAX',STMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
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
C--------------------------------------------------
C DEFAULT VALUES DEFINITION 
C--------------------------------------------------
C     
      IPRINT = 0
C
       KEY1='IGSTI'
       CALL DEFINTER(KEY1    ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='IGAP'
       CALL DEFINTER(KEY1    ,IGAP    ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
       KEY1='IBAG'
       CALL DEFINTER(KEY1    ,IBAG    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
       KEY1='IDEL'
       CALL DEFINTER(KEY1    ,IDEL7N  ,IASSIGN   ,IPRINT   ,
     .               NTYP     ,DEF_INTER)
C
        KEY1='INACTI'
        CALL DEFINTER(KEY1     ,INACTI,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C
        KEY1='MODFR'
        CALL DEFINTER(KEY1     ,MODFR,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)
C------------------------------------------------------------
C  Card1
C------------------------------------------------------------
C
C-------*CHECKS*-------------------
       IF(ISU2==0) THEN
          CALL ANCMSG(MSGID=119,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NOINT,
     .                C1=TITR)
       ENDIF
!
       IF(ISU1==0)THEN
         ISU1=ISU2
         IS1 =1
       ENDIF
       IS2=1
       INGR2USR => IGRSURF(1:NSURF)%ID
       ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
       IF(ISU1/=0)THEN
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)
          IS1 =2
       ENDIF

C-------*STORAGE IN IPARI AND FRIGAP *-------------------
       IPARI(45)=ISU1
       IPARI(46)=ISU2
       IPARI(13)=IS1*10+IS2   
       IPARI(34)=IGSTI
       IPARI(21)=IGAP
       
C------------------------------------------------------------
C  Card2
C------------------------------------------------------------

C-------*CHECKS*-------------------
       IF(IGAP>=2)THEN
         IF(GAPSCALE==ZERO)GAPSCALE=ONE
         FRIGAP(13) = GAPSCALE
         FRIGAP(16) = GAPMAX
       END IF
       IF(FRIGAP(16)==ZERO)THEN
          GAPMAX=EP30
          FRIGAP(16)=GAPMAX
       END IF
C------------------------------------------------------------
C  Card3
C------------------------------------------------------------
       IF(IGSTI>1)THEN
          I7STIFS=1
          IF(STMAX==ZERO)STMAX=EP30
          FRIGAP(17) = STMIN
          FRIGAP(18) = STMAX
       ELSE
          STMIN = ZERO
          STMAX = EP30
       END IF
C------------------------------------------------------------
C  Card4
C------------------------------------------------------------

       FRIGAP(2)=GAP
       IF (STOPT == ZERO) STOPT = EP30
       FRIGAP(3)=STARTT
       FRIGAP(11)=STOPT
C------------------------------------------------------------
C  Card5
C------------------------------------------------------------

        IF (STFAC == ZERO ) STFAC = ONE
        IF(IGSTI==1)STFAC=-STFAC
 
        IF(FRIC/=ZERO.AND.VISCF==ZERO)VISCF=ONE
        IF(VISC==ZERO)THEN
          IF(IVIS2==5)THEN
            VISC=ONE
          ELSE
            VISC=FIVEEM2
          ENDIF
        ENDIF
        IF (IDEL7N < 0) THEN
          IDELKEEP=1
          IDEL7N=ABS(IDEL7N)
        END IF
        IPARI(61)=IDELKEEP
        IF (IDEL7N>2.OR.N2D==1) IDEL7N = 0
C
        IF(BUMULT==ZERO) THEN
          BUMULT = BMUL0
C BMULT have to be increased for case of big models
          IF(NUMNOD > 2500000) THEN
             BUMULT = BMUL0*TWO
          ELSEIF(NUMNOD > 1500000) THEN
             BUMULT = BMUL0*THREE/TWO
          END IF
        END IF
C

        IPARI(22)=INACTI
        IPARI(17)=IDEL7N
        IPARI(14)=IVIS2
        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
        FRIGAP(14)=VISC
        FRIGAP(4)=BUMULT
C------------------------------------------------------------
C  Card6
C------------------------------------------------------------
C
       IF (MFROT/=0.AND.VISCF==0.0) VISCF=ONE
           IF (ALPHA==0.) IFQ = 0
C
           IF (MODFR==0) MODFR = 1
           IF (MODFR==2.AND.IFQ<10) IFQ = IFQ + 10
           IF (MODFR==2)VISCF=ZERO
C
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
C------------------------------------------------------------
C  Card7 +8 : friction coefficients
C------------------------------------------------------------
C
        IPARI(30)=MFROT
        IPARI(31) = IFQ
        FRIGAP(1)=FRIC
        FRIGAP(15)=VISCF**2
        FRIC_P(1) = C1
        FRIC_P(2) = C2
        FRIC_P(3) = C3
        FRIC_P(4) = C4
        FRIC_P(5) = C5
        FRIC_P(6) = C6

        IF (IBAG/=0.AND.NVOLU==0.AND.IALELAG==0.AND.NSPHSOL==0) THEN
             CALL ANCMSG(MSGID=614,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IBAG=0
        ENDIF
        IPARI(32) = IBAG
        INTBAG = MAX(INTBAG,IBAG)
C
        KCONTACT =MAX(KCONTACT,IBAG,IADM)
C
        MULTIMP = 4
        IPARI(23)=MULTIMP
C
C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)

C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
      IPRINT = 1

C
      WRITE(IOUT,1549)
C  
      IF( (IGAP>=2 .AND. IGAP/=1000).
     .           OR.IGSTI>1 .AND. IGSTI/=1000)THEN
         IF(IGSTI==1)THEN
            WRITE(IOUT,1538)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,FRIC,IGAP,GAP,GAPMAX,
     .                 GAPSCALE,STARTT,STOPT,                 
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
         ELSE
             WRITE(IOUT,1548)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,IGSTI,STMIN,STMAX,
     .                 FRIC,IGAP,GAP,GAPMAX,
     .                 GAPSCALE,STARTT,STOPT,                 
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
         ENDIF
      ELSEIF(STFAC>=ZERO)THEN
          WRITE(IOUT,1594)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 STFAC,IGAP,GAP,STARTT,STOPT,
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
      ELSE
           WRITE(IOUT,1595)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                 -STFAC,IGAP,GAP,
     .                 STARTT,STOPT,
     .                 BUMULT,INACTI,VISC,VISCF,IPARI(14),
     .                 IPARI(20),MULTIMP
          WRITE(IOUT,'(A)')
     . '    LAGRANGE MULTIPLIER WITH /DT/INTER/LAG. . '
       ENDIF
C
       IF(IDEL7N/=0) THEN
          WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',
     . ' (1:YES-ALL/2:YES-ANY) : ',IDEL7N
          IF(IDELKEEP == 1)THEN
            WRITE(IOUT,'(A)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
          ENDIF         
       ENDIF

       IF(IFQ>=10) THEN
          WRITE(IOUT,1518)
       ELSE
          WRITE(IOUT,1519)
       ENDIF
       WRITE(IOUT,1520)MOD(IFQ,10), XFILTR
       IF(MFROT==0)THEN
          WRITE(IOUT,1524) FRIC
       ELSEIF(MFROT==1)THEN
          WRITE(IOUT,1515)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .    FRIC_P(4),FRIC_P(5)
       ELSEIF(MFROT==2)THEN
          WRITE(IOUT,1522)FRIC,FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .    FRIC_P(4),FRIC_P(5),FRIC_P(6)
       ELSEIF(MFROT==3)THEN
          WRITE(IOUT,1523)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .    FRIC_P(4),FRIC_P(5),FRIC_P(6)
       ENDIF


       IF(IBAG/=1000) THEN
          WRITE(IOUT,*)'    AIRBAG POROSITY COUPLING ON   '
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

 1549 FORMAT(//
     .    '    TYPE==7   PARALLEL/AUTO IMPACTING           ' //)

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
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .       '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)

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
     .       '    GAP SCALE FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .       '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .       '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .       '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .       '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .       '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .       '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)
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
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I5/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)
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

 1518 FORMAT( '    FRICTION FORMULATION: INCREMENTAL (STIFFNESS) ',
     .             'FORMULATION')
 1519 FORMAT( '    FRICTION FORMULATION: TOTAL (VISCOUS) ',
     .             'FORMULATION')

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

      END
