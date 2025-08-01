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
!||    hm_read_inter_type15   ../starter/source/interfaces/int15/hm_read_inter_type15.F
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
      SUBROUTINE HM_READ_INTER_TYPE15(
     1        IPARI     ,STFAC      ,FRIGAP     ,NOINT     ,
     2        IGRSURF   ,TITR       ,LSUBMODEL  ,UNITAB    )
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      my_real STFAC
      my_real FRIGAP(*)
      CHARACTER(LEN=NCHARTITLE),INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=NCHARTITLE) :: TITR1
      INTEGER I,J,L, NTYP,IS1, IS2,ISU20,INTKG
      my_real FRIC,GAP,STARTT,STOPT,VISC
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C-----------------------------------------------
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE15
C=======================================================================

C Initializations
      IS1=0
      IS2=0
      INTKG = 0
C
      FRIC = ZERO
      GAP = ZERO
      STARTT = ZERO
      STOPT=EP30
      VISC = ZERO
C
      NTYP = 15
      IPARI(15)=NOINT
      IPARI(7)=NTYP

      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('STIFF1',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)

C....* CHECKS *.............

      IS1=1
      IS2=4
      INGR2USR => IGRSURF(1:NSURF)%ID
      IF(ISU1/=0)ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
        ISU20=ISU2
        ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
        IF ( IGRSURF(ISU2)%TYPE/=100
     .    .AND.IGRSURF(ISU2)%TYPE/=101) THEN
           TITR1 = IGRSURF(ISU20)%TITLE
             CALL ANCMSG(MSGID=111,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR,
     .                   I2=ISU20,
     .                   C2=TITR1)
      END IF

C.......* Storage IPARI FRIGAP *........
      IPARI(45)=ISU1
      IPARI(46)=ISU2
      IPARI(13)=IS1*10+IS2
C
C.....* CHECKS *.....
C
      VISC  =ZERO
      STARTT=ZERO
      STOPT =EP30

C.....* Storage IPARI FRIGAP *.......
      FRIGAP(1)=FRIC
      FRIGAP(3)=STARTT
      FRIGAP(11)=STOPT
      FRIGAP(14)=VISC
C
      IPARI(65) = INTKG
      FRIGAP(2)=GAP

C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
      WRITE(IOUT,1615)STFAC,FRIC,STARTT,STOPT

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


 1615 FORMAT(//
     .    '    TYPE==15   ELEMENTS to HYPER-ELLIPSOID    ' //,
     .    '    INTERFACE STIFFNESS FACTOR. . . . . . . . . ',1PG20.13/,
     .    '    FRICTION COEFFICIENT  . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13/)

      END
