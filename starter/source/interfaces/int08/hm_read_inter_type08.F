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
!||    hm_read_inter_type08   ../starter/source/interfaces/int08/hm_read_inter_type08.F
!||--- called by ------------------------------------------------------
!||    hm_read_inter_struct   ../starter/source/interfaces/reader/hm_read_inter_struct.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    ngr2usr                ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_INTER_TYPE08(
     1       IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2       IGRNOD     ,IGRSURF    ,UNITAB     ,LSUBMODEL ,TITR     )
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU1,ISU2,NOINT
      INTEGER IPARI(*)
      my_real
     .   STFAC
      my_real
     .   FRIGAP(*)
      CHARACTER(LEN=NCHARTITLE) :: TITR,TITR1
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L, NTYP,IS1, IS2,IGSTI,IFT0,IFORM,IRM
      my_real
     .   FRIC,STARTT,STOPT,FNOR,DBDEPTH,VISC,VISCF,
     .   FRIC_LAST,FNOR_LAST
      CHARACTER(LEN=40)::MESS
      CHARACTER(LEN=NCHARTITLE)::MSGTITL
      CHARACTER(LEN=NCHARKEY) ::  OPT
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C=======================================================================
C     READING Drawbead INTERFACE /INTER/TYPE8
C=======================================================================

C Initializations
      IS1=0
      IS2=0
      IFORM = 0  
      IFT0 = 0
      IRM = 0
C
      FRIC = ZERO
      STARTT = ZERO
      STOPT=EP30
      FNOR = ZERO
      DBDEPTH =ZERO
      VISC = ZERO
      FRIC_LAST = ZERO
      FNOR_LAST = ZERO
C
      NTYP = 8
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
      CALL HM_GET_INTV('IFORM1',IFORM,IS_AVAILABLE,LSUBMODEL)
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------

      CALL HM_GET_FLOATV('MU',FNOR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DBEAD_FORCE',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('PEXT',DBDEPTH,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MU_LAST',FNOR_LAST,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DBEAD_FORCE_LAST',FRIC_LAST,IS_AVAILABLE,LSUBMODEL,UNITAB)
        
C
C--------------------------------------------------
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------
C

C
C....* Card1 :flags  *.............

      IS1=2
      IS2=1
      IF(IFORM==0)IFORM=2
      IF(IFORM==1.AND.ISTATCND/=0)THEN
            CALL ANCMSG(MSGID=703,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR)
       END IF

       IPARI(48) = IFORM

       INGR2USR => IGRNOD(1:NGRNOD)%ID
       ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)
       IF(IGRNOD(ISU1)%SORTED/=1)THEN
           CALL ANCMSG(MSGID=112,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR)
       ENDIF
       INGR2USR => IGRSURF(1:NSURF)%ID
       ISU2=NGR2USR(ISU2,INGR2USR,NSURF)

C.......* Storage IPARI FRIGAP *........
      IPARI(45)=ISU1
      IPARI(46)=ISU2
      IPARI(13)=IS1*10+IS2

C
C....* Card1 :flags  *.............

      IF (STOPT == ZERO) STOPT = EP30

C.....* Storage IPARI FRIGAP *.......
      FRIGAP(1)=FRIC
      FRIGAP(3)=STARTT
      FRIGAP(11)=STOPT
      FRIGAP(4)=FNOR
      FRIGAP(5)=DBDEPTH
      FRIGAP(6)=FRIC_LAST
      FRIGAP(7)=FNOR_LAST
      IF(FRIC_LAST/= ZERO.OR.FNOR_LAST/= ZERO) IPARI(49) = 1 ! flag to activate linear force computation
C------------------------------------------------------------

       IF(IFORM==2) THEN
C VISC is a non documented parameter
            IF(VISC==ZERO) VISC=EM01
       END IF

       IF(NADMESH/=0) KCONTACT=1

       FRIGAP(14)=VISC

       IF (STFAC == ZERO ) STFAC = ONE_FIFTH
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
       IF(FNOR_LAST==ZERO.AND.FRIC_LAST==ZERO) THEN
        WRITE(IOUT,1508)FRIC,FNOR,DBDEPTH,STARTT,STOPT,IRM,IFORM,IFT0
       ELSEIF(FNOR_LAST==ZERO) THEN
        WRITE(IOUT,1509)FRIC,FRIC_LAST,FNOR,DBDEPTH,STARTT,STOPT,IRM,IFORM,IFT0
       ELSEIF(FRIC_LAST==ZERO) THEN
        WRITE(IOUT,1510)FRIC,FNOR,FNOR_LAST,DBDEPTH,STARTT,STOPT,IRM,IFORM,IFT0
       ELSE
        WRITE(IOUT,1511)FRIC,FRIC_LAST,FNOR,FNOR_LAST,DBDEPTH,STARTT,STOPT,IRM,IFORM,IFT0
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

 1508 FORMAT(//
     .    '    TYPE==8  DRAW-BEAD                       ' //,
     .    '    RETRAINING DRAW-BEAD FORCE / UNIT LENGTH . ',1PG20.13/,
     .    '    NORMAL DRAW-BEAD FORCE / UNIT LENGTH . . . ',1PG20.13/,
     .    '    DRAW-BEAD DEPTH . . . . . . . . . .  . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    MAIN SURFACE REORDERING FLAG. . . . . . ',I1/,
     .    '    FORMULATION FOR TANGENTIAL FORCE COMPUTATION',/,
     .    '            (1: VISCOUS, 2: INCREMENTAL) . . .',I1/,
     .    '    DEACTIVATION FLAG FOR RETRAINING FORCE REDUCING',I1/)
 1509 FORMAT(//
     .    '    TYPE==8  DRAW-BEAD                       ' //,
     .    '    RETRAINING DRAW-BEAD FORCE / UNIT LENGTH . ',/,
     .    '                AT THE FIRST NODE. . . . . . . ',1PG20.13/,
     .    '    RETRAINING DRAW-BEAD FORCE / UNIT LENGTH . ',/,
     .    '                AT THE LAST NODE . . . . . . . ',1PG20.13/,
     .    '    NORMAL DRAW-BEAD FORCE / UNIT LENGTH . . . ',1PG20.13/,
     .    '    DRAW-BEAD DEPTH . . . . . . . . . .  . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    MAIN SURFACE REORDERING FLAG. . . . . . ',I1/,
     .    '    FORMULATION FOR TANGENTIAL FORCE COMPUTATION',/,
     .    '            (1: VISCOUS, 2: INCREMENTAL) . . .',I1/,
     .    '    DEACTIVATION FLAG FOR RETRAINING FORCE REDUCING',I1/)
 1510 FORMAT(//
     .    '    TYPE==8  DRAW-BEAD                       ' //,
     .    '    RETRAINING DRAW-BEAD FORCE / UNIT LENGTH . ',1PG20.13/,
     .    '    NORMAL DRAW-BEAD FORCE / UNIT LENGTH . . . ',/,
     .    '                AT THE FIRST NODE. . . . . . . ',1PG20.13/,
     .    '    NORMAL DRAW-BEAD FORCE / UNIT LENGTH . . . ',/,
     .    '                AT THE LAST NODE . . . . . . . ',1PG20.13/,
     .    '    DRAW-BEAD DEPTH . . . . . . . . . .  . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    MAIN SURFACE REORDERING FLAG. . . . . . ',I1/,
     .    '    FORMULATION FOR TANGENTIAL FORCE COMPUTATION',/,
     .    '            (1: VISCOUS, 2: INCREMENTAL) . . .',I1/,
     .    '    DEACTIVATION FLAG FOR RETRAINING FORCE REDUCING',I1/)
 1511 FORMAT(//
     .    '    TYPE==8  DRAW-BEAD                       ' //,
     .    '    RETRAINING DRAW-BEAD FORCE / UNIT LENGTH . ',/,
     .    '                AT THE FIRST NODE. . . . . . . ',1PG20.13/,
     .    '    RETRAINING DRAW-BEAD FORCE / UNIT LENGTH . ',/,
     .    '                AT THE LAST NODE . . . . . . . ',1PG20.13/,
     .    '    NORMAL DRAW-BEAD FORCE / UNIT LENGTH . . . ',/,
     .    '                AT THE FIRST NODE. . . . . . . ',1PG20.13/,
     .    '    NORMAL DRAW-BEAD FORCE / UNIT LENGTH . . . ',/,
     .    '                AT THE LAST NODE . . . . . . . ',1PG20.13/,
     .    '    DRAW-BEAD DEPTH . . . . . . . . . .  . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    MAIN SURFACE REORDERING FLAG. . . . . . ',I1/,
     .    '    FORMULATION FOR TANGENTIAL FORCE COMPUTATION',/,
     .    '            (1: VISCOUS, 2: INCREMENTAL) . . .',I1/,
     .    '    DEACTIVATION FLAG FOR RETRAINING FORCE REDUCING',I1/)

      END
