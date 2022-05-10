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
Chd|  HM_READ_INTER_TYPE23          source/interfaces/int23/hm_read_inter_type23.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE23(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRSURF    ,XFILTR     ,FRIC_P     ,NPC1      ,TITR     ,
     3        LSUBMODEL  ,UNITAB     )
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER ISU1,ISU2,NI
      INTEGER IPARI(*),NPC1(*)
      my_real
     .   STFAC,XFILTR
      my_real
     .   FRIGAP(*),FRIC_P(*)
      CHARACTER TITR*nchartitle
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
C-----------------------------------------------
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
#include      "scr12_c.inc"
#include      "sysunit.inc"
#include      "thknod_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IBC1, IBC2, IBC3, IBUC, NOINT, NTYP,
     .    INACTI, IBC1M, IBC2M, IBC3M, IGSTI,IS1, IS2,
     .    ILEV, IGAP,MULTIMP,MFROT,IFQ,IBAG,MODFR, INTKG, 
     .    IDEL23,OK,IDELKEEP,IADM,IFSTF
      my_real
     .    FRIC,GAP,STARTT,BUMULT,STOPT,C1,C2,C3,C4,C5,C6,ALPHA,
     .    GAPSCALE,GAPMAX,STMIN,STMAX,VISC,FPENMAX,SCAL_T
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C-----------------------------------------------
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE23
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
      IDELKEEP=0
      INTKG = 0
      MULTIMP  = 0
      IBAG = 0
      IADM=0
      IFSTF=0
C
      STOPT=EP30
      INACTI = 0
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
C
      NTYP = 23
      IPARI(15)=NOINT
      IPARI(7)=NTYP

      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('type7_Istf',IGSTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ibag',IBAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idel7',IDEL23,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_X_BC',IBC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IBC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IBC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('GAPSCALE',GAPSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAPMAX',GAPMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FpenMax',FPENMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMIN',STMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STMAX',STMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TYPE7_SCALE',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF_DC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SORT_FACT',BUMULT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Xfreq',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C1',C1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C2',C2,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C3',C3,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C4',C4,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C5',C5,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C6',C6,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C....* CHECKS And Storage IPARI FRIGAP  *.............

      IF (IDEL23 < 0) THEN
        IDELKEEP=1
        IDEL23=ABS(IDEL23)
      END IF
      IPARI(61)=IDELKEEP

      IF (IDEL23.GT.2.OR.N2D.EQ.1) IDEL23 = 0
      IPARI(17)=IDEL23

      IS1=1
      INGR2USR => IGRSURF(1:NSURF)%ID
      ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
      IS2=1
      ISU2=NGR2USR(ISU2,INGR2USR,NSURF)

      IPARI(34)=IGSTI

      IF (IBAG.NE.0.AND.NVOLU.EQ.0 .AND. IALELAG == 0) THEN
        CALL ANCMSG(MSGID=614,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=NOINT,
     .              C1=TITR)
        IBAG=0
      ENDIF
C.......* *........
      IPARI(45)=ISU1
      IPARI(46)=ISU2
      IPARI(13)=IS1*10+IS2
      IPARI(21)=IGAP
      INTBAG = MAX(INTBAG,IBAG)
      KCONTACT =MAX(KCONTACT,IBAG,IADM)
      IPARI(32) = IBAG

      IF(MULTIMP.EQ.0)THEN
        IF(IMACH.NE.3)THEN
C set MULTIMP default value = 4 for SMP
          MULTIMP=4
        ELSE
C set MULTIMP default value = 12 for SPMD
          MULTIMP=12
        END IF
      END IF
      IPARI(23)=MULTIMP
C
      FRIGAP(16) = GAPMAX
      IF(GAPSCALE.EQ.ZERO.OR.IGAP.EQ.0)GAPSCALE=UN
      FRIGAP(19) = GAPSCALE
      IF (FPENMAX == ZERO) FPENMAX = UN
      FRIGAP(27) = FPENMAX
C

      IF(STMAX.EQ.ZERO)STMAX=EP30
      FRIGAP(17) = STMIN
      FRIGAP(18) = STMAX
      IF(IGSTI==0)I7STIFS=1
      IPARI(48) =IFSTF    ! IFSTF is not read anymore 
      SCAL_T=UN           ! SCAL_T is not read anymore 
      FRIGAP(33)=SCAL_T
C
      IF(STFAC.EQ.ZERO) THEN
        STFAC=UN
      ENDIF
      IF(IGSTI==1)STFAC=-STFAC
      IF (STOPT == ZERO) STOPT = EP30
C
      FRIGAP(1)=FRIC
      FRIGAP(2)=GAP
      FRIGAP(3)=STARTT
      FRIGAP(11)=STOPT
C
      IF(VISC==ZERO) VISC=UN
C
      IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
      IPARI(22)=INACTI
      FRIGAP(14)=VISC

C BUMULT is increased for big models
      IF(BUMULT.EQ.ZERO) BUMULT = BMUL0

      FRIGAP(4)=BUMULT
C
      IF (ALPHA.EQ.0.) IFQ = 0
C
      MODFR=2
      IFQ = IFQ + 10

      IF (IFQ.GT.0) THEN
        IF (IFQ.EQ.10) XFILTR = UN
        IF (MOD(IFQ,10).EQ.1) XFILTR = ALPHA
        IF (MOD(IFQ,10).EQ.2) XFILTR=QUATRE*ATAN2(UN,ZERO) / ALPHA
        IF (MOD(IFQ,10).EQ.3) XFILTR=QUATRE*ATAN2(UN,ZERO) * ALPHA
        IF (XFILTR.LT.ZERO) THEN
          CALL ANCMSG(MSGID=554,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=NOINT,
     .                C1=TITR,
     .                R1=ALPHA)
        ELSEIF (XFILTR.GT.1.AND.MOD(IFQ,10).LE.2) THEN
          CALL ANCMSG(MSGID=554,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=NOINT,
     .                C1=TITR,
     .                R1=ALPHA)
        ENDIF
      ELSE
        XFILTR = ZERO
      ENDIF
C
      IPARI(30) = MFROT
      IPARI(31) = IFQ
C
      FRIC_P(1) = C1
      FRIC_P(2) = C2
      FRIC_P(3) = C3
      FRIC_P(4) = C4
      FRIC_P(5) = C5
      FRIC_P(6) = C6
C
      IPARI(65)=INTKG
      IPARI(20)=ILEV
      IPARI(12)=IBUC

C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
      FRIGAP(10)=FLOAT(0)
C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS  USER TO INTERNAL ID
C------------------------------------------------------------
C
      IF (IPARI(48) /= 0) THEN        
        OK = 0
        DO J=1,NFUNCT
          IF (IPARI(48) == NPC1(J)) THEN
            IPARI(48)=J
            OK = 1
            EXIT
          ENDIF
        ENDDO
        IF(OK == 1) THEN    
          CALL ANCMSG(MSGID=121,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=NOINT,
     .                C1=TITR,
     .                I2=IPARI(48))
        ENDIF
      ENDIF
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
      WRITE(IOUT,2301)IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                IGSTI,STFAC,IFSTF,SCAL_T,STMIN,STMAX,
     .                FRIC,IGAP,GAP,GAPMAX,GAPSCALE,STARTT,STOPT,
     .                BUMULT,INACTI,VISC,MULTIMP,IBAG
      WRITE(IOUT,1520)MOD(IFQ,10), XFILTR
      IF(MFROT.EQ.0)THEN
        WRITE(IOUT,1524) FRIC
      ELSEIF(MFROT.EQ.1)THEN
        WRITE(IOUT,1515)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .  FRIC_P(4),FRIC_P(5)
      ELSEIF(MFROT.EQ.2)THEN
        WRITE(IOUT,1522)FRIC,FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .  FRIC_P(4),FRIC_P(5),FRIC_P(6)
      ELSEIF(MFROT.EQ.3)THEN
        WRITE(IOUT,1523)FRIC_P(1),FRIC_P(2),FRIC_P(3),
     .  FRIC_P(4),FRIC_P(5),FRIC_P(6)
      ENDIF
      IF(IDEL23.NE.0) THEN
        WRITE(IOUT,'(A,I5/)')
     .  '    DELETION FLAG ON FAILURE (1:YES) : ',IDEL23
        IF(IDELKEEP == 1)THEN
          WRITE(IOUT,'(A)')
     .    '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
        ENDIF         
      ENDIF

C--------------------------------------------------------------
      IF(IS1.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT'
      ELSEIF(IS1.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
      ELSEIF(IS1.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS'
      ELSEIF(IS1.EQ.5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS'        
      ENDIF
      IF(IS2.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT'
      ELSEIF(IS2.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES'
      ELSEIF(IS2.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.4)THEN
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
 1518 FORMAT( '    FRICTION FORMULATION: INCREMENTAL (STIFFNESS) ',
     .             'FORMULATION')
 1519 FORMAT( '    FRICTION FORMULATION: TOTAL (VISCOUS) ',
     .             'FORMULATION')
 1520 FORMAT(
     .    '    FRICTION FILTERING FLAG. . . . . . . . . ',I10/,
     .    '    FILTERING FACTOR . . . . . . . . . . . . ',1PG20.13)
C----------     
 2301 FORMAT(//
     .       '    TYPE.EQ.23   PARALLEL/AUTO IMPACTING        ' //,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .       '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .       '                                         Z DIR  ',I1/,
     .       '    STIFFNESS FORMULATION. .  . . . . . . . . ',I1/,
     .'       0 : STIFFNESS IS COMPUTED FROM STIFFNESS ON SECONDARY SIDE'/,
     .'       1 : STFAC IS A STIFFNESS VALUE                      '/,
     .       '    STIFFNESS FACTOR OR STIFFNESS VALUE . . . ',1PG20.13/,
     . '    IFSTF:FUNCTION ID FOR STIFFNESS FACTOR VS TIME. ',I10/,
     . '    SCALE FACTOR ON ABSCISSA FOR FUNCTION IFSTF . . ',1PG20.13/,
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
     .       '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/,
     .       '    IBAG . . . . . . . . . . . . . . .  . . . ',I5/)
      END
