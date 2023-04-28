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
Chd|  HM_READ_PROP06                source/properties/solid/hm_read_prop06.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP06(GEO      ,IGEO    ,PROP_TAG,MULTI_FVM ,IGTYP   ,
     .                          IG       ,IDTITL  ,UNITAB  ,LSUBMODEL ,RTRANS  ,
     .                          SUB_ID   ,ISKN    ,IPART   ,SUB_INDEX)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ PROPERTY TYPE06 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     GEO             PROPERTY ARRAY(REAL)
C     IGEO            PROPERTY ARRAY(INTEGER)
C     PROP_TAG        PROPERTY TAGS STRUCTURE
C     MULTI_FVM       MULTI_FVM STRUCTURE
C     IGTYP           PROPERTY TYPE
C     IG              PROPERTY ID
C     IDTITL          TITLES ARRAY
C     UNITAB          UNITS ARRAY
C     LSUBMODEL       SUBMODEL STRUCTURE   
C     RTRANS          TRANSFORMATION ARRAY(REAL)
C     SUB_ID          SUBMODEL ID
C     ISKN            SKEW ARRAY(INTEGER)
C     IPART           PART ARRAY(INTEGER)
C     SUB_INDEX       SUBMODEL INDEX
C-----------------------------------------------
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD  
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: IGEO(NPROPGI)
      INTEGER,INTENT(IN) :: IG
      INTEGER,INTENT(IN) :: ISKN(LISKN,SISKWN/LISKN)
      INTEGER,INTENT(IN) :: IPART(LIPART1,NPART+NTHPART) 
      INTEGER,INTENT(IN) :: IGTYP
      INTEGER,INTENT(IN) :: SUB_ID 
      INTEGER,INTENT(IN) :: SUB_INDEX 
C
      my_real,INTENT(INOUT) :: GEO(NPROPG)
      my_real,INTENT(IN) :: RTRANS(NTRANSF,NRTRANS)
C
      CHARACTER,INTENT(IN) :: IDTITL*nchartitle
C
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP),INTENT(INOUT) :: PROP_TAG
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IHBE,ISMSTR,IPLAS,ICPRE,ICSTR,IINT,IP,ISK,IREP,JCVT,
     .    NPT,NPTR,NPTS,NPTT,ITET4,IET,ISTR,IHBE_OLD,ITET10,
     .    ISHEAR,ISORTH
C     REAL
      my_real
     .    CVIS,QA,QB,QH,VX,VY,VZ,ANGLE,DTMIN,PX,PY,PZ,VN,
     .    VDEFMIN,VDEFMAX,ASPMAX,ASPTET
      INTEGER K,NSPHDIR,ID_PARTSPH,IPARTSPH,J
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      ISORTH=1
      ISMSTR=0
      IHBE=0
C======================================================================|
C
      GEO(3)=ISMSTR

      IGEO( 1)=IG
      IGEO(10)=IHBE
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+EM01
      GEO(171)=IHBE
C
      ICSTR = 0 
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISOLID',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
C
C     IPLAS=0 ! New HM reader <=> possible numerical change for decks translated from old formats (IPLAS is no more read since format 2018)
c      CALL HM_GET_INTV('Iplas',IPLAS,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Itetra10',ITET10,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Itetra4',ITET4,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icpre',ICPRE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_R',NPTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_S',NPTS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_T',NPTT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('REFPLANE',IP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iframe',JCVT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ORTHTROP',IREP,IS_AVAILABLE,LSUBMODEL)
c      CALL HM_GET_INTV('Istrain',ISTR,IS_AVAILABLE,LSUBMODEL)
c      CALL HM_GET_INTV('IHKT',IET,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ndir',NSPHDIR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SPHPART_ID',ID_PARTSPH,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('dn',CVIS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vx',VX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vy',VY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vz',VZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA',ANGLE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h',QH,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('deltaT_min',DTMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('vdef_min',VDEFMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('vdef_max',VDEFMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ASP_max',ASPMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('COL_min',ASPTET,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Px',PX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Py',PY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Pz',PZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
      IF(ISK == 0 .AND. SUB_INDEX /= 0 ) ISK = LSUBMODEL(SUB_INDEX)%SKEW
C-----removed flags:
      ISTR=1 
      IPLAS = 2
      IET = 0
C----------------------
      IF(ITET4 == 0)  ITET4 = ITET4_D
      IF(ITET10 == 0) ITET10 = ITET10_D
C
C
      IF (SUB_ID /= 0)
     .         CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)
c
      IPARTSPH=0
      IF(ID_PARTSPH > 0) THEN
        DO J=1,NPART
          IF(IPART(4,J)==ID_PARTSPH) THEN
           IPARTSPH=J
           GOTO 175
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=1037,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=IG,
     .              C1=IDTITL,
     .              I2=ID_PARTSPH)
        CALL ARRET(2)
175     CONTINUE
      END IF
C
C---------------------------------------------------
C---   Default values
C
      IINT = 1
C------  new solid17, IHBE will be changed to 17 in sgrtails.F    
      IF (IHBE == 18 ) IINT = 2 
      CVIS = ZERO
C
C  ihbe
C
      IF (IHBE == 0) IHBE = IHBE_DS
C
C     old formats < 5.1 (BTW these formats are no more supported)
      IF(IHBE == 101) THEN
        IHBE=1
        JCVT=2
      END IF
      IF(IHBE == 102) THEN
        IHBE=2
        JCVT=2
      END IF
      IF(IHBE == 104) THEN
        IHBE=24
        JCVT=2
      END IF
      IF(IHBE == 112) THEN
        IHBE=12
        JCVT=2
      END IF
C------- used for elasto-platic critia parameter no necessary with IET---
      IF(N2D>0 .AND. IHBE/=0 .AND. IHBE/=2.AND. IHBE/=17)THEN
        IHBE_OLD=IHBE
        IHBE=0
        CALL ANCMSG(MSGID=321,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=IG,
     .              C1=IDTITL,
     .              I2=IHBE_OLD,
     .              I3=IHBE)
      ELSEIF (IHBE/= 1.AND.IHBE/= 2.AND.IHBE/=14.AND.IHBE/= 24
     .        .AND.IHBE/= 17.AND.IHBE/= 18) THEN
        CALL ANCMSG(MSGID=533,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL,
     .              I2=IHBE)
        IHBE=1
      ENDIF
C
C   jcvt
C
      IF (JCVT == 0)  JCVT = IFRAME_DS
      IF (IHBE == 14.OR.IHBE == 18) JCVT = 2
      IF (IHBE == 24) JCVT = 2
      IF (IFRAME_DS == -2.OR.JCVT<0) JCVT = -1      
C
C  smstr
C
      IF(ISMSTR == 0) ISMSTR=ISST_DS
      IF(ISMSTR == 0.AND.IHBE /= 18) ISMSTR=4
      IF (ISST_DS == -2) ISMSTR = -1      
C
C  IPLAS
C
c      IF (IPLAS == 0)IPLAS=IPLA_DS
c      IF (IPLAS /=1 .AND. IPLAS /=2 .AND. IPLAS /=3 ) THEN
c         CALL ANCMSG(MSGID=416,
c     .               MSGTYPE=MSGERROR,
c     .               ANMODE=ANINFO_BLIND_1,
c     .               I1=IG,
c     .               C1=IDTITL,
c     .               I2=IPLAS)
c      ENDIF
c      IF (IPLAS == 1)THEN
c         CALL ANCMSG(MSGID=417,
c     .               MSGTYPE=MSGWARNING,
c     .               ANMODE=ANINFO_BLIND_1,
c     .               I1=IG,
c     .               C1=IDTITL,
c     .               I2=IPLAS)
c      ENDIF
C
C  icpre/icstr
C
      IF (ICPRE == 0) ICPRE = ICPRE_D      
      IF((N2D > 0 .AND. IHBE == 17) ) THEN
        IF(ICPRE/=1 .AND. ICPRE/=2) ICPRE=0
C------ no effet for Axi Isolid17 for the moment	
        IF(N2D == 1 .AND. IHBE == 17) ICPRE=0
      ELSE
        IF (IHBE /= 14 .AND. IHBE /= 24 .AND. IHBE /= 17 .AND. IHBE /= 18) ICPRE = 0
        IF (IHBE == 17 ) THEN
           IF (ICPRE == 0 ) THEN
             ICPRE = 1
           ELSEIF(ICPRE == 3 ) THEN
             ICPRE = 0
           ENDIF
        ENDIF
        IF (ICPRE == 3 .AND. IHBE /= 18) ICPRE =0
      END IF!((N2D > 0 .AND. IHBE == 17) .OR. 
      ICSTR = 0
      IF (ICPRE_D == -2) ICPRE = -1      
C
C  NPT
C
      SELECT CASE (IHBE)
      CASE(14,16)
        IF(NPTR == 0) NPTR = 2
        IF(NPTS == 0) NPTS = 2
        IF(NPTT == 0) NPTT = 2
        NPT=NPTR*100+NPTS*10+NPTT
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
        ELSEIF (IHBE == 16 .AND.
     .     (NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR.
     .      NPTR > 3 .OR. NPTS > 9 .OR. NPTT > 3)) THEN
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL,
     .                I2=NPT,
     .                I3=IHBE)
        ENDIF
      CASE(1,2,24)
        NPT = 1
      CASE(12,13,17,18)
        NPT = 8
      END SELECT
C      
      IF((N2D > 0 .AND. IHBE == 17) .OR. 
     .   (N2D == 1 .AND. IHBE == 22)) THEN
          NPT = 4                                                 
      ENDIF                                                      
C
C  viscosity / hourglass
C
C igeo(31)  flag for default qa qb for law 70 can be used for other law
      IF(QA == ZERO .AND. QB == ZERO) IGEO(31) = 1
      IF (QA == ZERO) QA = ONEP1
      IF (QB == ZERO) QB = FIVEEM2
      IF (QH == ZERO) QH = EM01
C
C  hourglass
C
      IF (IHBE == 24) THEN
        IF (CVIS == ZERO) CVIS = EM01
        GEO(13) = CVIS
        QH   = ZERO
        IINT = IET
      ELSEIF (IHBE==1.OR.IHBE==2) THEN
        GEO(13) = QH
      ELSE
        QH   = ZERO
        GEO(13) = ZERO
      ENDIF
C
C  orthotropy
C
C  IP =20, 21:Pj;23:Vj+MAT_BETA;24:Vj+Pj
      IF (IP == 23 .OR. IP == 24) THEN
C---- error-out if Vj=0+IP=23,24
        VN = VX*VX+VY*VY+VZ*VZ
        IF (VN<EM20) THEN
          CALL ANCMSG(MSGID=1918,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL,
     .                I2=IP)
        ENDIF
      END IF
      IF (IP == 0) THEN
        DO K=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(ISK == ISKN(4,K+1)) THEN
            IP=-(K+1)
            ISK=K+1
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='PROPERTY',
     .              C2='PROPERTY',
     .              I1=IG,I2=ISK,C3=IDTITL)
100     CONTINUE
      ENDIF
C   Istrain
c      IF (ISTR == 0)ISTR =ISTRA_D
C----
      IGEO(2)  = IP
      IGEO(4)  = NPT
      IGEO(5)  = ISMSTR
      IGEO(6)  = IREP
      IGEO(9)  = IPLAS-1
      IGEO(10) = IHBE
      IGEO(12) = ISTR
      IGEO(13) = ICPRE
      IGEO(14) = ICSTR
      IGEO(15) = IINT
      IGEO(16) = JCVT-1
      IGEO(37) = NSPHDIR
      IGEO(38) = IPARTSPH
C
      GEO(1)  = ANGLE
      GEO(7)  = VX
      GEO(8)  = VY
      GEO(9)  = VZ
      GEO(14) = QA
      GEO(15) = QB
      GEO(172)= DTMIN
      GEO(33)  = PX
      GEO(34)  = PY
      GEO(35)  = PZ
      GEO(190)= VDEFMIN
      GEO(191)= VDEFMAX
      GEO(192)= ASPMAX
      GEO(193)= ASPTET
C----
      IF(.NOT. IS_ENCRYPTED)THEN
        IF(IGEO(31) == 1)THEN
          WRITE(IOUT,1100)IG,IHBE,ISMSTR,IPLAS,NPT,JCVT,ITET4,ITET10,
     .                ICPRE,ICSTR,CVIS,QA,QB,QH,DTMIN,IREP,ISTR
        ELSE
          WRITE(IOUT,1000)IG,IHBE,ISMSTR,IPLAS,NPT,JCVT,ITET4,ITET10,
     .                ICPRE,ICSTR,CVIS,QA,QB,QH,DTMIN,IREP,ISTR
        ENDIF
        IF((VDEFMIN+VDEFMAX+ASPMAX+ASPTET)>ZERO) THEN
          IF (VDEFMAX==ZERO) VDEFMAX=EP10
          IF (ASPMAX==ZERO) ASPMAX=EP10
          WRITE(IOUT,3000) VDEFMIN,VDEFMAX,ASPMAX,ASPTET 
        END IF          
       IF(IP < 0) THEN
         WRITE(IOUT,1001) ISKN(4,ISK)
       ELSEIF(IP == 1 .OR. IP == 2 .OR. IP == 3) THEN
         WRITE(IOUT,1002) IP,ANGLE
       ELSEIF(IP == 11 .OR. IP == 12 .OR. IP == 13) THEN
         WRITE(IOUT,1003) IP,VX,VY,VZ
       ELSEIF(IP == 20) THEN
         WRITE(IOUT,2001) IP
       ELSEIF(IP == 21) THEN
         WRITE(IOUT,2002) IP,PX,PY,PZ
       ELSEIF(IP == 23) THEN
         WRITE(IOUT,2003) IP,ANGLE,VX,VY,VZ
       ELSEIF(IP == 24) THEN
         WRITE(IOUT,2004) IP,PX,PY,PZ,VX,VY,VZ
       ENDIF
       IF (IET > 0) WRITE(IOUT,2010) IET
       IF(NSPHDIR/=0)WRITE(IOUT,2020)NSPHDIR, ID_PARTSPH
      ELSE
       WRITE(IOUT,1099) IG
      ENDIF

      IF (ITET4 == 1000) ITET4 = 0
      IGEO(20) = ITET4
      IF (ITET10 == 1000) ITET10 = 0
      IGEO(50) = ITET10
C
      PROP_TAG(IGTYP)%G_SIG  = 6
      PROP_TAG(IGTYP)%L_SIG  = 6
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_QVIS = 1
      PROP_TAG(IGTYP)%L_EINT = 1
      PROP_TAG(IGTYP)%G_VOL  = 1
      PROP_TAG(IGTYP)%L_VOL  = 1
      PROP_TAG(IGTYP)%L_QVIS = 1
      IF (MULTI_FVM%IS_USED) THEN
         PROP_TAG(IGTYP)%G_MOM = 3
      ENDIF
C
      PROP_TAG(IGTYP)%G_FILL = 1
      PROP_TAG(IGTYP)%L_STRA = 6 
      IF (N2D /= 0 .AND. MULTI_FVM%IS_USED) THEN
!     2D analysis
        PROP_TAG(IGTYP)%G_AREA = 1
      ENDIF
      PROP_TAG(IGTYP)%G_GAMA = 6
      PROP_TAG(IGTYP)%L_SIGL = 6
      IF (GEO(16) /= ZERO .OR. GEO(17) /= ZERO) THEN
        IGEO(33) = 1   ! ISVIS flag
      ENDIF 

C-------------------------------
      IGEO(1) =IG
      IGEO(11)=IGTYP
      IGEO(17)=ISORTH
      IF(GEO( 3)/=ZERO.AND.IGEO( 5)== 0) IGEO( 5)=NINT(GEO( 3))
      IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0) IGEO( 9)=NINT(GEO(39))
      IF(GEO(171)/=ZERO.AND.IGEO(10)== 0) IGEO(10)=NINT(GEO(171))
C
C----
      RETURN
C---
 1000 FORMAT(
     & 5X,'ORTHOTROPIC SOLID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SOLID FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SOLID STRESS PLASTICITY FLAG. . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/,
     & 5X,'COROTATIONAL SYSTEM FLAG. . . . . . . .=',I10/,
     & 5X,'TETRA4 FORMULATION FLAG. . . . .  . . .=',I10/,
     & 5X,'TETRA10 FORMULATION FLAG . . . .  . . .=',I10/,
     & 5X,'CONSTANT PRESSURE FLAG. . . . . . . . .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'HOURGLASS VISCOSITY . . . . . . . . . .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/)
 1099 FORMAT(
     & 5X,'ORTHOTROPIC SOLID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I8/,
     & 5X,'CONFIDENTIAL DATA'//)
 1100 FORMAT(
     & 5X,'ORTHOTROPIC SOLID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SOLID FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SOLID STRESS PLASTICITY FLAG. . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/,
     & 5X,'COROTATIONAL SYSTEM FLAG. . . . . . . .=',I10/,
     & 5X,'TETRA4 FORMULATION FLAG. . . . .  . . .=',I10/,
     & 5X,'TETRA10 FORMULATION FLAG . . . .  . . .=',I10/,
     & 5X,'CONSTANT PRESSURE FLAG. . . . . . . . .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'DEFAULT VALUE FOR QUADRATIC BULK. . . . ',/,
     & 5X,'     VISCOSITY (QA) WILL BE USED. . . .=',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70 WHERE QA = 0.     ',/,
     & 5X,'DEFAULT VALUE FOR LINEAR BULK . . . . . ',/,
     & 5X,'     VISCOSITY (QB) WILL BE USED . . . =',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70 WHERE QB = 0.     ',/,
     & 5X,'HOURGLASS VISCOSITY . . . . . . . . . .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/)
 1001 FORMAT(
     & 5X,'ORTHOTROPIC SKEW FRAME. . . . . . . . .=',I10)
 1002 FORMAT(
     & 5X,'ORTHOTROPIC PLANE NUMBER. . . . . . . .=',I10/,
     & 5X,'     1=(R,S)  2=(S,T)  3=(T,R)            ',/,
     & 5X,'ORTHOTROPIC ANGLE . . . . . . . . . . .=',1PG20.13)
 1003 FORMAT(
     & 5X,'ORTHOTROPIC PLANE NUMBER. . . . . . . .=',I10/,
     & 5X,'     1=(R,S)  2=(S,T)  3=(T,R)            ',/,
     & 5X,'REFERENCE VECTOR VX . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VY . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VZ . . . . . . . . . .=',1PG20.13)
 2010 FORMAT(
     & 5X,'HOURGLASS MODULUS FLAG. . . . . . . . .=',I10/)
 2020 FORMAT(
     & 5X,'NUMBER OF SPH PARTICLES PER DIRECTION .=',I10/,
     & 5X,'CORRESPONDING PART FOR SPH PARTICLES. .=',I10/)
 2001 FORMAT(
     & 5X,'ORTHOTROPIC DIRECTIONS BY ELEMENT CONNECTIVITY,IP='I10)
 2002 FORMAT(
     & 5X,'ORTHOTROPIC DIRECTION FLAG IP. . . . . =',I10/,
     & 5X,'REFERENCE POINT PX . . . . . . . . . . =',1PG20.13/,
     & 5X,'REFERENCE POINT PY . . . . . . . . . . =',1PG20.13/,
     & 5X,'REFERENCE POINT PZ . . . . . . . . . . =',1PG20.13)
 2003 FORMAT(
     & 5X,'ORTHOTROPIC DIRECTION FLAG IP . . . . .=',I10/,
     & 5X,'ORTHOTROPIC ANGLE . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VX . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VY . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VZ . . . . . . . . . .=',1PG20.13)
 2004 FORMAT(
     & 5X,'ORTHOTROPIC DIRECTION FLAG IP. . . . . =',I10/,
     & 5X,'REFERENCE POINT PX . . . . . . . . . . =',1PG20.13/,
     & 5X,'REFERENCE POINT PY . . . . . . . . . . =',1PG20.13/,
     & 5X,'REFERENCE POINT PZ . . . . . . . . . . =',1PG20.13/,
     & 5X,'REFERENCE VECTOR VX . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VY . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE VECTOR VZ . . . . . . . . . .=',1PG20.13)
 3000 FORMAT(
     & 5X,'SOLID MINIMUM VOLUMETRIC STRAIN........=',1PG20.13/,
     & 5X,'SOLID MAXIMUM VOLUMETRIC STRAIN........=',1PG20.13/,
     & 5X,'SOLID MAXIMUM ASPECT RATIO.............=',1PG20.13/,
     & 5X,'SOLID MINIMUM COLLAPSE RATIO...........=',1PG20.13/)
C---
      END SUBROUTINE 
