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
Chd|  HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PROP_READ21(GEO   ,IGEO      ,IG    ,ISKN   ,UNITAB ,
     .                       RTRANS   ,LSUBMODEL ,SUB_ID,IDTITL ,IGTYP  ,PROP_TAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
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
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI),IG,ISKN(LISKN,*),SUB_ID,IGTYP
      my_real GEO(NPROPG)
      my_real RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IHBE,ISMSTR,IPLAST,ICPRE,ICSTR,NPT,NPTR,NPTS,NPTT,
     .    IINT,JCVT,IP,ISK,IREP,IDSK,ISTRAIN
      my_real
     .    CVIS,QA,QB,VX,VY,VZ,ANGLE,DTMIN,PUN
      INTEGER J
      LOGICAL IS_AVAILABLE, IS_CRYPTED
      CHARACTER IDTITL*nchartitle
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C======================================================================|
C----------------------
C      21  ORTHOTROPIC THICK SHELL
C----------------------
C======================================================================|

       IS_CRYPTED = .FALSE.
       IS_AVAILABLE = .FALSE.

C HIDDEN FLAGS
C----------------------
       ISTRAIN = 1
       IPLAST = 2
       JCVT = 2
       IINT = 1
       ICPRE = 0

C      Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
       IGEO( 1)=IG
       IGEO(11)=IGTYP
       GEO(12) =IGTYP+PUN
       
       NPT = 0

C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISOLID',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icstr',ICSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NBP',NPT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SKEW_CSID',IDSK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iorth',IREP,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('dn',CVIS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('VECTOR_X',VX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('VECTOR_Y',VY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('VECTOR_Z',VZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA',ANGLE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('deltaT_min',DTMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
Capply submodel transform to V (VX,VY,VZ) 
C
      IF (SUB_ID .NE. 0)
     .         CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)

C-----------------------
C---   Default values
C
C  ihbe
      IF (IHBE == 0) IHBE = IHBE_DS
      IF (IHBE /= 14 .AND. IHBE /= 15) THEN
c        CALL ANSTCKI(IG)
c        CALL ANSTCKI(IHBE)
c        CALL ANCERR(175,ANINFO_BLIND_1)
        CALL ANCMSG(MSGID=549,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL,
     .              I2=IHBE,
     .              I3=21)
      ENDIF     
C
C  smstr
      IF(ISMSTR == 0) ISMSTR=ISST_DS
      IF(ISMSTR == 0) ISMSTR=4
      IF (ISST_DS == -2.OR.ISMSTR<0) ISMSTR=4          
C
C  icpre/icstr
C
      IF (IHBE == 14 .AND.
     .    (ICSTR /= 1.AND.ICSTR /= 10.AND.ICSTR /= 100)) THEN
         CALL ANCMSG(MSGID=677,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .                   C1=IDTITL,
     .                   I2=ICSTR)
      ELSE
        SELECT CASE (ICSTR)
          CASE(100)
           IP = 2
          CASE(10)
           IP = 3
          CASE(1)
           IP = 1
        END SELECT
      END IF
C
C  npt
C
      SELECT CASE (IHBE)
      CASE(15)
        IF (NPT == 0) NPT = 3
        IF (NPT < 1 .OR. NPT > 9)  THEN
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL,
     .                I2=NPT,
     .                I3=IHBE)
        ENDIF
      CASE(14)
        IF (NPT == 0) NPT = 222
        NPTR= NPT/100
        NPTS= MOD(NPT/10,10)
        NPTT= MOD(NPT,10)
        IF (IHBE == 14 .AND.
     .     (NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR.
     .      NPTR > 9 .OR. NPTS > 9 .OR. NPTT > 9)) THEN
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL,
     .                I2=NPT,
     .                I3=IHBE)
        ENDIF
      END SELECT
C
C  viscosity
C

      IF (CVIS == ZERO) THEN
        CVIS = EM01
      ENDIF
C igeo(31)  flag for default qa qb for law 70 can be used for other law
      IF(QA == ZERO .AND. QB == ZERO) IGEO(31) = 1
      IF (QA == ZERO) QA = ONEP1
      IF (QB == ZERO) QB = FIVEEM2
C
C  orthotropy  (plane r,t)
C
      IF (IHBE /= 14) IP = 3
C
      ISK = 0
      IF (IDSK.NE.0) THEN
        DO J=0,NUMSKW
          IF(IDSK.EQ.ISKN(4,J+1)) THEN
            ISK=J+1
            GO TO 10
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=184,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=IG,
     .              C2='PROPERTY',
     .              C3=IDTITL,
     .              I2=IDSK)
 10     CONTINUE
      ENDIF
      IF (IP .LE. 0) THEN
        DO J=0,NUMSKW
          IF(ISK == ISKN(4,J+1)) THEN
            IP=-(J+1)
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='PROPERTY',
     .              C2='PROPERTY',
     .              I2=ISK,
     .              I1=IG,
     .              C3=IDTITL)
100     CONTINUE
      ENDIF
      IGEO(2)  = IP
      IGEO(4)  = NPT
      IGEO(5)  = ISMSTR
      IGEO(6)  = IREP
      IGEO(7)  = ISK
      IGEO(9)  = IPLAST-1
      IGEO(10) = IHBE
      IGEO(12) = ISTRAIN
      IGEO(13) = ICPRE
      IGEO(14) = ICSTR
      IGEO(15) = IINT
      IGEO(16) = JCVT-1
C
      GEO(1)   = ANGLE
      GEO(7)   = VX
      GEO(8)   = VY
      GEO(9)   = VZ
      GEO(13)  = CVIS
      GEO(14)  = QA
      GEO(15)  = QB
      GEO(172) = DTMIN
C----
      IF(.NOT.IS_CRYPTED)THEN
        IF(IGEO(31) == 1)THEN
          WRITE(IOUT,1100)IG,IHBE,ISMSTR,NPT,ICSTR,
     .                  CVIS,QA,QB,DTMIN
        ELSE
          WRITE(IOUT,1000)IG,IHBE,ISMSTR,NPT,ICSTR,
     .                  CVIS,QA,QB,DTMIN
        ENDIF
        IF(ISK == 0)THEN
            WRITE(IOUT,1002) GEO(7),GEO(8),GEO(9),IREP,ANGLE
        ELSE
            WRITE(IOUT,1001) IDSK,IREP,ANGLE
        ENDIF
      ELSE
        WRITE(IOUT,1099) IG
      ENDIF
C
      IF(GEO( 3)/=ZERO.AND.IGEO( 5)== 0) IGEO( 5)=NINT(GEO( 3))
      IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0) IGEO( 9)=NINT(GEO(39))
      IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))

      IF (GEO(16) /= ZERO .OR. GEO(17) /= ZERO) THEN
         IGEO(33) = 1   ! ISVIS flag
      ENDIF         

      IGEO(17)=1


C--------   Variables stored in element buffer 
c----   Thick Shells
      PROP_TAG(IGTYP)%G_SIG  = 6
      PROP_TAG(IGTYP)%G_VOL  = 1
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_QVIS = 1
      PROP_TAG(IGTYP)%L_SIG  = 6
      PROP_TAG(IGTYP)%L_EINT = 1
      PROP_TAG(IGTYP)%L_VOL  = 1
      PROP_TAG(IGTYP)%L_QVIS = 1
      PROP_TAG(IGTYP)%G_FILL = 1

      PROP_TAG(IGTYP)%G_GAMA = 6
      PROP_TAG(IGTYP)%L_SIGL = 6
C
C         Probably useless for PID21 (but addressed in thsol.F ...)
      PROP_TAG(IGTYP)%L_GAMA = 6


C----
      RETURN
 999  CALL FREERR(3)
      RETURN
C---
 1000 FORMAT(
     & 5X,'ORTHOTROPIC THICK SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13)
 1001 FORMAT(
     & 5X,'ORTHOTROPIC SKEW FRAME. . . . . . . . .=',I10/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'ORTHOTROPIC ANGLE . . . . . . . . . . .=',1PG20.13/)
 1002 FORMAT(
     & 5X,'REFERENCE VECTOR VX . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VY . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VZ . . . . . . . . . .=',1PG20.13/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'ORTHOTROPIC ANGLE . . . . . . . . . . .=',1PG20.13/)
 1099 FORMAT(
     & 5X,'ORTHOTROPIC THICK SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I8/,
     & 5X,'CONFIDENTIAL DATA'//)
 1100 FORMAT(
     & 5X,'ORTHOTROPIC THICK SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'DEFAULT VALUE FOR QUADRATIC BULK. . . . ',/,
     & 5X,'     VISCOSITY (QA) WILL BE USED. . . .=',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70 WHERE QA = 0.     ',/,
     & 5X,'DEFAULT VALUE FOR LINEAR BULK . . . . . ',/,
     & 5X,'     VISCOSITY (QB) WILL BE USED . . . =',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70 WHERE QB = 0.     ',/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/)
C---
      END SUBROUTINE HM_PROP_READ21
