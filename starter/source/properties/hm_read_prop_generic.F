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
!||    hm_read_prop_generic   ../starter/source/properties/hm_read_prop_generic.F
!||--- called by ------------------------------------------------------
!||    hm_read_properties     ../starter/source/properties/hm_read_properties.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fretitl2               ../starter/source/starter/freform.F
!||    hm_read_prop28         ../starter/source/properties/xelem/hm_read_prop28.F
!||    hm_read_prop32         ../starter/source/properties/spring/hm_read_prop32.F
!||    hm_read_prop33         ../starter/source/properties/spring/hm_read_prop33.F
!||    hm_read_prop34         ../starter/source/properties/sph/hm_read_prop34.F
!||    hm_read_prop35         ../starter/source/properties/spring/hm_read_prop35.F
!||    hm_read_prop36         ../starter/source/properties/spring/hm_read_prop36.F
!||    hm_read_prop43         ../starter/source/properties/solid/hm_read_prop43.F
!||    hm_read_prop44         ../starter/source/properties/spring/hm_read_prop44.F
!||    hm_read_prop45         ../starter/source/properties/spring/hm_read_prop45.F
!||    hm_read_prop46         ../starter/source/properties/spring/hm_read_prop46.F
!||    hm_read_prop_user      ../starter/source/properties/user_spring_solid/hm_read_prop_user.F
!||    hm_read_prop_user4     ../starter/source/properties/user_spring_solid/hm_read_prop_user4.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod           ../starter/source/modules/defaults_mod.F90
!||    elbuftag_mod           ../starter/share/modules1/elbuftag_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP_GENERIC(
     1                    IGTYP  ,IG      ,IDTITL  ,KEYTYPE  ,GEO      ,
     2                    IGEO   ,LBUFGEO ,BUFGEO  ,IADBUF   ,UNITAB   ,
     3                    ISKN   ,KNOT    ,IAD_KNOT,PROP_TAG ,LSUBMODEL,
     4                    RTRANS ,SUB_ID  ,IUNIT   ,SUB_INDEX,DEFAULTS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE DEFAULTS_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARLINE
      USE READER_OLD_MOD , ONLY : KLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     INPUT ARGUMENTS
      CHARACTER(LEN=NCHARTITLE),INTENT(IN):: IDTITL
      CHARACTER(LEN=NCHARLINE) :: KEYTYPE
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN):: IG,ISKN(LISKN,*),SUB_ID,IUNIT,SUB_INDEX
      my_real  ,INTENT(IN):: RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
C     OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT):: IGTYP
C     MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT) :: IGEO(NPROPGI), LBUFGEO, IADBUF, IAD_KNOT
      my_real  ,INTENT(INOUT):: 
     .        GEO(NPROPG), KNOT(*)
      DOUBLE PRECISION,INTENT(INOUT)::  BUFGEO(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP),INTENT(INOUT) :: PROP_TAG
      TYPE(DEFAULTS_), INTENT(IN)  :: DEFAULTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISKW,IADFUN,IADMAT,IADPID,IADTAB
      INTEGER I, ILAW,NPG,JCVT,NUVAR(2)
      my_real PARGEO(100),STIF,TTHICK
      my_real QA,QB
      CHARACTER FILNAM*512,CLAW*4
      INTEGER LEN_FILNAM
      CHARACTER(LEN=NCHARTITLE) :: TITR
C=======================================================================
c------------------------------
      DO I = 1,100
       PARGEO(I) = ZERO
      ENDDO
C
      DO I = 1, BGEOSIZE
        BUFGEO0(I) = ZERO
      ENDDO
C
      NUVAR(1)= 0
      NUVAR(2)= 0
      NUPARAM = 0
      NJFUNC  = 0
      NJMAT   = 0
      NJPID   = 0
      NJTAB  = 0
C
      IGEO(1) =IG
      IGEO(5) = 4 ! ISMSTR
C
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
C
      SELECT CASE(KEYTYPE(1:LEN_TRIM(KEYTYPE)))

        CASE ('TYPE28','NSTRAND')              
          IGTYP     =28
          CALL HM_READ_PROP28(IOUT , NUVAR ,PARGEO,UNITAB,IG,
     .                        TITR ,IGTYP ,PROP_TAG,LSUBMODEL,IUNIT)

        CASE ('TYPE32','SPR_PRE')              
          IGTYP     =32
          CALL HM_READ_PROP32(IOUT , NUVAR ,PARGEO,UNITAB,IG,
     .                        IGTYP,PROP_TAG,TITR,LSUBMODEL)

        CASE ('TYPE33','KJOINT')
          IGTYP     =33
          CALL HM_READ_PROP33(IOUT ,NUVAR ,PARGEO,UNITAB,
     .                        IUNIT ,IG    ,TITR  ,PROP_TAG,
     .                        IGTYP ,LSUBMODEL)

        CASE ('TYPE34','SPH')
          IGTYP     =34
          CALL HM_READ_PROP34(GEO,IGEO,IOUT,NUVAR,PARGEO,
     .                        QA,QB,ISKN,IG,TITR,UNITAB,
     .                        PROP_TAG,IGTYP,LSUBMODEL)

        CASE ('TYPE35','STITCH')
          IGTYP     =35
          CALL HM_READ_PROP35(IOUT , NUVAR ,PARGEO,UNITAB,
     .                        IG,IGTYP ,PROP_TAG,LSUBMODEL)

        CASE ('TYPE36','PREDIT')
          IGTYP     =36
          CALL HM_READ_PROP36(IOUT , NUVAR ,PARGEO,UNITAB,
     .                        ISKN,IG,TITR,IGTYP,PROP_TAG,GEO,
     .                        LSUBMODEL,SUB_INDEX)
        CASE ('USER4')
C--------------------------------------------------
C     rupture property for interface type 2
C          ( not a true user property )
C--------------------------------------------------
          IGTYP=37
          CALL HM_READ_PROP_USER4(IOUT  ,NUVAR ,PARGEO ,UNITAB ,LSUBMODEL)

             
        CASE ('TYPE43','CONNECT')
          IGTYP=43
          CALL HM_READ_PROP43(GEO     ,IGEO     ,IOUT    ,IG      ,
     .                        NUVAR   ,PARGEO   ,IGTYP ,PROP_TAG,
     .                        UNITAB  ,LSUBMODEL,DEFAULTS%SOLID)

        CASE ('TYPE44','SPR_CRUS')
          IGTYP=44
          CALL HM_READ_PROP44(IOUT  ,IG   ,NUVAR ,PARGEO,
     .                        UNITAB,ISKN  ,IGEO ,TITR  ,IGTYP ,
     .                        PROP_TAG,LSUBMODEL ,SUB_INDEX,IUNIT)

        CASE ('TYPE45','KJOINT2')              
          IGTYP     =45
          CALL HM_READ_PROP45(IOUT , NUVAR ,PARGEO,UNITAB,IGTYP,
     .                        IG,PROP_TAG,TITR,LSUBMODEL,IUNIT)

        CASE ('TYPE46','SPR_MUSCLE')              
          IGTYP     =46
          CALL HM_READ_PROP46(IOUT , NUVAR ,PARGEO,UNITAB,IGTYP,
     .                        IG,PROP_TAG,TITR,LSUBMODEL,IUNIT)

        CASE ('TYPE29','TYPE30','TYPE31',
     .        'USER1' ,'USER2' ,'USER3')
C--------------------------------------------------
C           HM READING PROPERTY USERnn 
C--------------------------------------------------
          CALL HM_READ_PROP_USER(
     .                 IOUT    ,NUVAR ,PARGEO   ,UNITAB,IGTYP,
     .                 IG      ,TITR  ,LSUBMODEL,IUNIT ,ISKN ,
     .                 KEYTYPE ,PROP_TAG)
!
        CASE DEFAULT
          CALL ANCMSG(MSGID=1647,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .    I1=IG,C1='TITR',I2=IGTYP) ! This message can not occur !

      END SELECT
C
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+EM01
C-----
      LBUFGEO = LBUFGEO + NUPARAM + NJFUNC + NJMAT + NJPID + NJTAB
C
      DO I=1,NUPARAM
        BUFGEO(IADBUF+I-1)=BUFGEO0(I)
      ENDDO
      IADFUN  = IADBUF + NUPARAM
      DO I=1,NJFUNC
        BUFGEO(IADFUN+I-1)=JFUNC(I)
      ENDDO
      IADMAT  = IADFUN + NJFUNC
      DO I=1,NJMAT
        BUFGEO(IADMAT+I-1)=JMAT(I)
      ENDDO
      IADPID  = IADMAT + NJMAT
      DO I=1,NJPID
        BUFGEO(IADPID+I-1)=JPID(I)
      ENDDO
      IADTAB  = IADPID + NJPID
      DO I=1,NJTAB
        BUFGEO(IADTAB+I-1)=JTAB(I)
      ENDDO
C
      ISKW  = NINT(PARGEO(1))
      STIF  = PARGEO(2)
      IFRWV = MAX(IFRWV,NINT(PARGEO(3)))
      NPG   = NINT(PARGEO(4))
      JCVT  = NINT(PARGEO(5))
C
      IF (ISKW == 0 .AND. IGTYP  /=  34) ISKW = 1
      GEO(2)=ISKW
      IGEO(2)=ISKW
      IGEO(4)=NPG
      GEO(3)=STIF
      GEO(8)=5
      GEO(25) = NUVAR(1)
      GEO(26) = NUPARAM
      GEO(27) = IADBUF
      GEO(28) = NJFUNC
      GEO(29) = IADFUN
      GEO(30) = NJMAT
      GEO(31) = IADMAT
      GEO(32) = NJPID
      GEO(33) = IADPID
      GEO(35) = NUVAR(2)
      GEO(36) = NJTAB
      GEO(37) = IADTAB
C
      IGEO(16) = JCVT
      IGEO(27) = NUVAR(1)
      IGEO(28) = NUVAR(2) 
      IGEO(52) = NUPARAM
      IGEO(53) = NJFUNC
      IGEO(54) = NJMAT
      IGEO(55) = NJPID
      IGEO(56) = NJTAB
      IGEO(57) = IADBUF
      IGEO(58) = IADFUN
      IGEO(59) = IADMAT
      IGEO(60) = IADPID
      IGEO(61) = IADTAB
C-----------
      IADBUF  = IADTAB + NJTAB
C-----------
      IF (IGTYP > 28 .and. IGTYP < 32 .or.  IGTYP > 36 .and. IGTYP < 43) THEN   ! prop user
          PROP_TAG(IGTYP)%G_SIG  = 6
          PROP_TAG(IGTYP)%G_VOL  = 1
          PROP_TAG(IGTYP)%G_EINT = 1
          PROP_TAG(IGTYP)%G_OFF  = 1
          PROP_TAG(IGTYP)%G_FILL = 1
          PROP_TAG(IGTYP)%L_SIG  = 6
          PROP_TAG(IGTYP)%L_VOL  = 1
          PROP_TAG(IGTYP)%L_EINT = 1
          PROP_TAG(IGTYP)%L_OFF  = 1
          PROP_TAG(IGTYP)%NUVAR  = IGEO(27)  !  User solid property
          IGEO(4) = 1    ! NPT
          PROP_TAG(IGTYP)%L_SIG  = 6
          PROP_TAG(IGTYP)%L_VOL  = 1
          PROP_TAG(IGTYP)%L_EINT = 1
      ENDIF ! IF (IGTYP)
C-----------
      RETURN

      RETURN
      END

