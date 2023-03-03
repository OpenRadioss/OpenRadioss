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
Chd|  HM_READ_INTER_TYPE20          source/interfaces/int20/hm_read_inter_type20.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE20(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRNOD     ,IGRSURF    ,IGRSLIN    ,XFILTR    ,FRIC_P   ,  
     3        UNITAB     ,LSUBMODEL  ,TITR     )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU1,ISU2,NI
      INTEGER IPARI(*)
      my_real
     .   STFAC,XFILTR
      my_real
     .   FRIGAP(*),FRIC_P(*)
      CHARACTER TITR*nchartitle,TITR1*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   ,TARGET , DIMENSION(NSLIN)   :: IGRSLIN
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
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
      INTEGER I,J,L,IBC1, IBC2, IBC3, NOINT, NTYP, IBID,
     .    INACTI, IBC1M, IBC2M, IBC3M, IGSTI,IS1,IS2,
     .    IGAP,MULTIMP,MFROT,IFQ,IBAG,MODFR,IERR1,IVIS2, 
     .    ISYM,NOD1,IDUM,IGNORE,KK,II,IGAP0,IDELKEEP,
     .    IRSTH,ICURV,NA1,NA2,IFORM,IADM,IEDGE,NRADM,ISU10,ISU20,
     .    NOD10,NOD20,LINE10,LINE20,IBID1,IBID2,IDEL7N,LINE1,
     .    LINE2
      my_real
     .   FRIC,GAP,STARTT,BUMULT,STOPT,C1,C2,C3,C4,C5,C6,ALPHA,
     .   VISC,VISCF,EGDE_ANGL,FPENMAX,EDG_ANGL,GAPSOL,STMIN,STMAX,
     .   PADM,ANGLADM,CADM,GAPMAX,GAPSCALE
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
C     READING PENALTY INTERFACE /INTER/TYPE20
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
      IDELKEEP=0
      NOD1 = 0    
      MULTIMP  = 0
      IFORM = 0
      IBAG = 0
      IDEL7N=0
      IVIS2 = 0
      NRADM  =1
      IADM = 0
C
      STOPT=EP30
      INACTI = 0
      VISCF = ZERO
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

      GAPSOL = ZERO
      STMIN = ZERO
      STMAX = ZERO
      FPENMAX = ZERO
      GAPMAX = ZERO
      GAPSCALE = ZERO
      BUMULT = ZERO

C ADMESH not read but initialized
      PADM   =ONE
      ANGLADM=ZERO
C
      NTYP = 20
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
      CALL HM_GET_INTV('I_sym',ISYM,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('I_edge',IEDGE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('GRNOD_ID',NOD1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Line1_set',LINE1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Line2_set',LINE2,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Igap',IGAP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ibag',IBAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NodDel3',IDEL7N,IS_AVAILABLE,LSUBMODEL)
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
      CALL HM_GET_FLOATV('ANGLE2',EDG_ANGL,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('FpenMax',FPENMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STFAC',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MINI',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('STIFF_DC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV('FRIC_DC',VISCF,IS_AVAILABLE,LSUBMODEL,UNITAB) 
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
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------
C

C
C....* Card1 :flags  *.............
C

      IF(ISYM == 0)ISYM = 1
      IPARI(43)=ISYM

      IS1=-1
      IS2=-1
      ISU10 = ISU1
      ISU20 = ISU2
      NOD10 = NOD1
      LINE10 = LINE1
      LINE20 = LINE2
      INGR2USR => IGRSURF(1:NSURF)%ID
      IF(ISU1 /= 0)ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
      IF(ISU2 == 0)THEN
         IF(ISYM == 1)THEN
            ISU2 = ISU1
            ISU20 = ISU10
            IPARI(43)=0
         ENDIF
      ELSE
         ISU2 = NGR2USR(ISU2,INGR2USR,NSURF)
         IF(ISYM == 1)THEN
            IPARI(43)=1
          ENDIF
      ENDIF

      IF (ISU1 == 0 .AND. ISU2 == 0) IEDGE = -1
      IPARI(58)=IEDGE

      IF(IEDGE==3 .and. EDG_ANGL==ZERO) EDG_ANGL=NINTY+ONE
      FRIGAP(26)  = COS((HUNDRED80-EDG_ANGL)*PI/HUNDRED80)

      INGR2USR => IGRNOD(1:NGRNOD)%ID
      IF(NOD1 /= 0) NOD1=NGR2USR(NOD1,INGR2USR,NGRNOD)
      IPARI(26)=NOD1

      IF(LINE2 == 0 .and. ISU1 == 0)LINE2=LINE1
      IF(IEDGE == 0)THEN
         IF(LINE1 == LINE2)THEN
            IPARI(42)=1
         ELSE
            IPARI(42)=0
         ENDIF
      ELSE
         IF(LINE1 == LINE2 .and. ISU1 == ISU2)THEN
            IPARI(42)=1
         ELSE
            IPARI(42)=0
         ENDIF
      ENDIF

      INGR2USR => IGRSLIN(1:NSLIN)%ID
      IF(LINE1 /= 0)LINE1=NGR2USR(LINE1,INGR2USR,NSLIN)
      IF(LINE2 /= 0)LINE2=NGR2USR(LINE2,INGR2USR,NSLIN)
      IPARI(59)=LINE1
      IPARI(60)=LINE2

C.......* *........
      IPARI(45)=ISU1
      IPARI(46)=ISU2
      IPARI(13)=IS1*10+IS2

C
C....* Card2 :flags  *.............
C

       IF(IGSTI==0)IGSTI = 3
       IF(ISMS==1) IGSTI = 4
       IPARI(34)=IGSTI
       IF (IDEL7N < 0) THEN
          IDELKEEP=1
          IDEL7N=ABS(IDEL7N)
        END IF
        IPARI(61)=IDELKEEP
        IF (IDEL7N>2.OR.N2D==1) IDEL7N = 0
        IPARI(17)=IDEL7N

        IF (IBAG/=0.AND.NVOLU==0 .AND. IALELAG == 0 ) THEN
             CALL ANCMSG(MSGID=614,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=NOINT,
     .                   C1=TITR)
             IBAG=0
        ENDIF
        IPARI(32) = IBAG
        INTBAG = MAX(INTBAG,IBAG)

        KCONTACT =MAX(KCONTACT,IBAG,IADM)

        IPARI(21)=IGAP

C
C....* Card4  *.............
C

       IF(IGAP==2)THEN
          IF(GAPSCALE==ZERO)GAPSCALE=ONE
          FRIGAP(13) = GAPSCALE
          FRIGAP(16) = GAPMAX
        END IF

        IF(FRIGAP(16)==ZERO)THEN
           GAPMAX=EP30
           FRIGAP(16)=GAPMAX
        END IF

        IF (FPENMAX == ZERO) FPENMAX = ONE
        FRIGAP(27) = FPENMAX
        FRIGAP(29) = GAPSOL/FOUR
C
C....* Card6  *.............
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

        IF(STFAC==ZERO.AND.IGSTI/=1) THEN
          STFAC=ONE
        ENDIF
        IF (STFAC == ZERO )STFAC = ONE_FIFTH

        IF (STOPT == ZERO) STOPT = EP30

C.....* Storage IPARI FRIGAP *.......
        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT

C
C....* Card7  *.............
C

C Hidden flag no more read using HM reader
c        IF (BCFLAGM(LFIELD-2:LFIELD-2)== '1') IBC1M = 1
c        IF (BCFLAGM(LFIELD-1:LFIELD-1)== '1') IBC2M = 1
c        IF (BCFLAGM(LFIELD  :LFIELD  )== '1') IBC3M = 1

        IF(FRIC/=ZERO.AND.VISCF==ZERO)VISCF=ONE
          IF(VISC==ZERO)THEN
            IF(IVIS2==5)THEN
              VISC=ONE
            ELSE
              VISC=FIVEEM2
            ENDIF
        ENDIF

        IPARI(22)=INACTI
        IPARI(14)=IVIS2
        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
C
C....* Card8 : FRICTION data *.............
C

         IF (MFROT/=0.AND.VISCF==0.0) VISCF=ONE

         IF (ALPHA==0.) IFQ = 0

         IF (MODFR==0) MODFR = 2
         IF (MODFR==2.AND.IFQ<10) IFQ = IFQ + 10
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

C
C....* Card9 : FRICTION data *.............
C

        FRIC_P(1) = C1
        FRIC_P(2) = C2
        FRIC_P(3) = C3
        FRIC_P(4) = C4
        FRIC_P(5) = C5
        FRIC_P(6) = C6

        IPARI(30) = MFROT
        IPARI(31) = IFQ
        FRIGAP(14)=VISC
        FRIGAP(15)=VISCF**2

C------------------------------------------------------------
C  General Storage IPARI FRIGAP 
C------------------------------------------------------------

        CADM      =COS(ANGLADM*PI/HUNDRED80)
        IPARI(49) =NRADM
        FRIGAP(24)=PADM
        FRIGAP(25)=CADM

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

C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
      FRIGAP(10)=FLOAT(0)

      MULTIMP = 4
      IPARI(23)=MULTIMP
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
       WRITE(IOUT,3507)
     .                ISU10,ISU20,ISYM,MAX(IEDGE,0),NOD10,LINE10,LINE20,
     .                EDG_ANGL,
     .                IBC1,IBC2,IBC3,IBC1M,IBC2M,IBC3M,
     .                IGSTI,STFAC,STMIN,STMAX,
     .                FRIC,IGAP,GAP,GAPSOL,STARTT,STOPT,
     .                INACTI,FPENMAX,VISC,VISCF,IPARI(14),
     .                IPARI(20),MULTIMP

C        
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
 3507 FORMAT(//
     .    '    TYPE==20  PARALLEL/AUTO IMPACTING           ' //,
     .    '    FIRST SURFACE ID. . . . . . . . . . . . . ',I10/,
     .    '    SECOND SURFACE ID . . . . . . . . . . . . ',I10/,
     .    '    SYMMETRY FLAG . . . . . . . . . . . . . . ',I10/,
     .    '    EDGE FLAG . . . . . . . . . . . . . . . . ',I10/,
     .    '     =0 No edges'/,
     .    '     =1 Edges from surface border'/,
     .    '     =2 Edges from each segment(element) edge'/,
     .    '     =3 same as 1 + sharp edges between segment'/,
     .    '    NOD GROUP ID (ADDITIONAL) . . . . . . . . ',I10/,
     .    '    FIRST LINE ID (ADDITIONAL). . . . . . . . ',I10/,
     .    '    SECOND LINE ID (ADDITIONAL) . . . . . . . ',I10/,
     .    '    ANGLE FOR EDGE COMPUTATION (Iedge=3). . . ',1PG20.13/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       SECONDARY NODE  (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       MAIN NODE (1:YES 0:NO)          Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    STIFFNESS FORMULATION. .  . . . . . . . . ',I1/,
     .    '    STIFFNESS FACTOR OR STIFFNESS VALUE . . . ',1PG20.13/,
     .    '    MINIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .    '    MAXIMUM STIFFNESS. . . .  . . . . . . . . ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .    '    VARIABLE GAP FLAG . . . . . . . . . . . . ',I10/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    MINIMUM SOLID THICKNESS . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .    '    MAXIMAL INITIAL PENETRATION FACTOR. . . . ',1PG20.13/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION CRITICAL DAMPING FACTOR. . . . . ',1PG20.13/,
     .    '    QUADRATIC DAMPING FLAG. . . . . . . . . . ',I10/,
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I10/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I10/)
C----------   

   
      END
