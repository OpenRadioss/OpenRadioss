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
Chd|  HM_READ_INTER_LAGMUL_TYPE02   source/interfaces/int02/hm_read_inter_lagmul_type02.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_LAGMUL          source/interfaces/reader/hm_read_inter_lagmul.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_LAGMUL_TYPE02(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRNOD     ,IGRSURF    ,DEF_INTER  ,TITR      ,UNITAB   ,
     3        LSUBMODEL  )
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
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
      INTEGER ISU1,ISU2,NOINT,NI
      INTEGER IPARI(*),DEF_INTER(*)
      my_real
     .   FRIGAP(*) , STFAC
      CHARACTER TITR*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C----------------------s-------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTYP, ILEV, IPRINT,IASSIGN,IGNORE,
     .    IDEL2,IBUC,INTKG,IS1,IS2
      my_real
     .   STARTT,STOPT,DSEARCH
      CHARACTER KEY1*ncharkey
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C     READING LAGRANGE MULTIPLIER INTERFACES /INTER/LAGMUL/TYPE2
C=======================================================================

C Initializations
      IS1=0
      IS2=0
      IBUC=0
C
      NTYP = 2
      IPARI(15)=NOINT
      IPARI(7) =NTYP
C
      IPRINT = 0
C------------------------------------------------------------
C  Card1
C------------------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Isearch',IBUC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('dsearch',DSEARCH,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      KEY1='IBUC'
      IASSIGN = 1
      CALL DEFINTER(KEY1     ,IBUC    ,IASSIGN   ,IPRINT   ,
     .              NTYP     ,DEF_INTER)

C
      IF(IBUC==0)IBUC=2
C------------------------------------------------------------
      IF (ISU1==0) THEN
         CALL ANCMSG(MSGID=1001,
     .               ANMODE=ANINFO_BLIND_1,
     .               MSGTYPE=MSGERROR,
     .               I1=NOINT,
     .               C1=TITR)
       END IF
       IF(ISU2 == 0) THEN
         CALL ANCMSG(MSGID=119,
     .               ANMODE=ANINFO_BLIND_1,
     .               MSGTYPE=MSGERROR,
     .               I1=NOINT,
     .               C1=TITR)
       END IF 
C
       IS1=2
       IS2=1
       INGR2USR => IGRNOD(1:NGRNOD)%ID
       ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)
       INGR2USR => IGRSURF(1:NSURF)%ID
       ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
       IF (IGRNOD(ISU1)%NENTITY == 0) THEN
           CALL ANCMSG(MSGID=1131,
     .   		ANMODE=ANINFO_BLIND_1,
     .  		MSGTYPE=MSGERROR,
     .    		I1=NOINT,
     .    		C1=TITR)
       END IF

C-------*STORAGE IN IPARI AND FRIGAP *-------------------
C
       IPARI(12) = IBUC

       IPARI(45) = ISU1
       IPARI(46) = ISU2
       IPARI(13) = IS1*10+IS2

       FRIGAP(4) = DSEARCH

       IDEL2    = 0
       IPARI(17)= IDEL2
C
C      IGNORE is working with /INTER/LAGMUL/TYPE2 but not documented
       IGNORE = 0
       IPARI(34) = IGNORE

       INTKG = 0
       IPARI(65) = INTKG

       ILEV  = 0
       IPARI(20) = ILEV

       STARTT = ZERO
       STOPT  = EP30
       FRIGAP(3) = STARTT
       FRIGAP(11)= STOPT
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
      WRITE(IOUT,1602) IBUC,FRIGAP(4)          
C
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
      RETURN

 1602 FORMAT(//
     .    '    TYPE==2   TIED SLIDING                  ' //
     .    '    LAGRANGE MULTIPLIER FORMULATION           '  /
     .    '    SEARCH FORMULATION. . . . . . . . . . . . ',I5/,
     .    '    SEARCH DISTANCE . . . . . . . . . . . . . ',1PG20.13/)

      END
