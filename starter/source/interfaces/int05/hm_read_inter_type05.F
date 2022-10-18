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
Chd|  HM_READ_INTER_TYPE05          source/interfaces/int05/hm_read_inter_type05.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        INTER_DCOD_SENSOR             source/interfaces/reader/inter_dcod_sensor.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE05(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,    
     2        IGRNOD     ,IGRSURF    ,NOM_OPT    ,XFILTR    ,FRIC_P   ,
     3        LSENSOR    ,UNITAB     ,LSUBMODEL  ,TITR       )
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
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER ISU1,ISU2,ILAGM,NI,NOINT
      INTEGER IPARI(*),LSENSOR(NSENSI,*)
      my_real
     .   STFAC,XFILTR
      my_real
     .   FRIGAP(*),FRIC_P(*)
      CHARACTER TITR*nchartitle,TITR1*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
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
#include      "remesh_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,IBC1, IBC2, IBC3, IBUC,  NTYP, IVOID,
     .    INACTI, IBC1M, IBC2M, IBC3M, IGSTI, IS1, IS2,
     .    ILEV, MFROT,IFQ,IBAG,MODFR, IDEL5,IDSENS,IDELKEEP,
     .    INTKG,IADM,MULTIMP,IBID,IRM,NRADM
      my_real
     .   FRIC,GAP,STARTT,STOPT,C1,C2,C3,C4,C5,C6,ALPHA,
     .   VISC,VISCF,PTMAX,PADM,ANGLADM
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
C     READING PENALTY INTERFACE /INTER/TYPE5
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
      INTKG = 0
      INACTI = 0
      IDEL5 = 0
      MULTIMP = 0
      IADM =0
      NRADM=1
C
      STOPT=EP30

      FRIC = ZERO
      GAP = ZERO
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

      PTMAX=EP30

      PADM =ONE
      ANGLADM=ZERO
C

      NTYP = 5
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
C
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ibag',IBAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NodDel5',IDEL5,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Deactivate_X_BC',IBC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IBC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IBC3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Vflag',IRM,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Ifric',MFROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifiltr',IFQ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',IDSENS,IS_AVAILABLE,LSUBMODEL)
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------

      CALL HM_GET_FLOATV('TYPE5_SCALE',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('Xfreq',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Ptlim',PTMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
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
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------
C

C
C....* Card1 :flags  *.............
C
       IS1=2
       IS2=1
       INGR2USR => IGRNOD(1:NGRNOD)%ID
       ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)
       INGR2USR => IGRSURF(1:NSURF)%ID
       ISU2=NGR2USR(ISU2,INGR2USR,NSURF)

       IPARI(45)=ISU1
       IPARI(46)=ISU2
       IPARI(13)=IS1*10+IS2


        IF (IDEL5 < 0) THEN
           IDELKEEP=1
           IDEL5=ABS(IDEL5)
        END IF
        IPARI(61)=IDELKEEP
        IF (IDEL5>2.OR.N2D==1) IDEL5 = 0
        IPARI(17)=IDEL5

        IF (IBAG/=0.AND.NVOLU==0.AND.IALELAG==0.AND.NSPHSOL==0) THEN
             CALL ANCMSG(MSGID=614,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IBAG=0
        ENDIF
        IPARI(32)=IBAG

C         Iadm has not been implemented for TYPE5 yet.
        IF (IADM/=0.AND.NADMESH==0) THEN
             CALL ANCMSG(MSGID=647,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IADM=0
        ENDIF
        IPARI(44)=IADM

        KCONTACT =MAX(KCONTACT,IBAG,IADM)
        INTBAG=MAX(INTBAG,IBAG)

        IPARI(20)=ILEV

C
C....* Card2   *.............
C
        IF (STFAC == ZERO) STFAC = ONE_FIFTH

        IF (STOPT == ZERO) STOPT = EP30

        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT
C
C....* Card3   *.............
C
        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
        IPARI(22)=INACTI
        FRIGAP(14)=VISC
        IPARI(24) = IRM
C
C....* Card4 : FRICTION data    *.............
C

        IF(PTMAX==ZERO) PTMAX=EP30
        FRIGAP(16)=PTMAX

       IF (ALPHA==0) IFQ = 0
       IF (IFQ>0) THEN
          IF (IFQ==1) XFILTR = ALPHA
          IF (IFQ==2) XFILTR = FOUR*ATAN2(ONE,ZERO) / ALPHA
          IF (IFQ==3) XFILTR = FOUR*ATAN2(ONE,ZERO) * ALPHA
          IF (XFILTR<0.OR.(IFQ<=2.AND.XFILTR>1.)) THEN
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
       IPARI(31)=IFQ

       IPARI(64) = IDSENS
       IPARI(30) = MFROT

C
C....* Card4 : OPTIONAL Card8 Card9 : C1...C6 friction data   *
C
       FRIC_P(1)=C1
       FRIC_P(2)=C2
       FRIC_P(3)=C3
       FRIC_P(4)=C4
       FRIC_P(5)=C5
       FRIC_P(6)=C6
C
      IPARI(65) = INTKG
C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS AND SENSOR - USER TO INTERNAL ID
C------------------------------------------------------------
C
        CALL INTER_DCOD_SENSOR (NTYP,NI,IPARI,NOM_OPT,LSENSOR)
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
        IF(IDSENS/=0) THEN
           WRITE(IOUT,1505)IBC1,IBC2,IBC3,STFAC,GAP,IDSENS,IRM,PTMAX
        ELSE
           WRITE(IOUT,1516)IBC1,IBC2,IBC3,STFAC,GAP,STARTT,STOPT,IRM,PTMAX 
        ENDIF
        WRITE(IOUT,4000)INACTI
        IF(IDEL5/=0) THEN
          WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG ON FAILURE OF MAIN ELEMENT',
     . ' (1:YES-ALL/2:YES-ANY) SET TO ',IDEL5
          IF(IDELKEEP == 1)THEN
            WRITE(IOUT,'(A)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
          ENDIF         
        ENDIF
        WRITE(IOUT,1520)IFQ, XFILTR
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
        IF(IBAG/=0) THEN
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

C------------
 1505 FORMAT(//
     .    '    TYPE==5  SLIDING AND VOIDS(NON SYMMETRIC)' //,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       (1:YES 0:NO)                      Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . . ',1PG20.13/,
     .    '    INITIAL GAP . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME/STOP TIME activated by SENSOR ID ',I10/,
     .    '    MAIN SURFACE REORDERING FLAG. . . . . . . ',I1/,
     .    '    TANGENTIAL PRESSURE LIMIT. . .. . . . . . . ',1PG20.13/)
 1516 FORMAT(//
     .    '    TYPE==5  SLIDING AND VOIDS(NON SYMMETRIC)' //,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       (1:YES 0:NO)                      Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .    '    INITIAL GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    MAIN SURFACE REORDERING FLAG. . . . . . ',I1/,
     .    '    TANGENTIAL PRESSURE LIMIT. . .. . . . . . ',1PG20.13/)
C------------

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

 1557 FORMAT(
     .'  NUMBER OF ELEMENTS WITHIN A 90 DEGREES FILLET         ',I5/,
     .'  ---------------------------------------------         '/,
     .'  CRITERIA FOR REFINEMENT DUE TO PENETRATION :          '/,
     .'  ------------------------------------------            '/,
     .'  MINIMUM PERCENTAGE OF PENETRATION                     ',
     .   1PG20.13/,
     .'  MAXIMUM ANGLE ON MAIN SIDE AT CONTACT LOCATION      ',
     .   1PG20.13//)

 4000 FORMAT(
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/)
      END
