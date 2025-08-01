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
!||    hm_read_prop20           ../starter/source/properties/thickshell/hm_read_prop20.F
!||--- called by ------------------------------------------------------
!||    hm_read_properties       ../starter/source/properties/hm_read_properties.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod             ../starter/source/modules/defaults_mod.F90
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP20(GEO,IGEO,PROP_TAG ,MULTI_FVM,IGTYP,IG,TITR,UNITAB,LSUBMODEL,
     .                          DEFAULTS_SOLID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ PROPERTY TYPE01 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IGEO            PROPERTY ARRAY(INTEGER)
C     GEO             PROPERTY ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     IG              PROPERTY ID(INTEGER)
C     TITR          MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD         
      USE SUBMODEL_MOD
      USE ELBUFTAG_MOD            
      USE MULTI_FVM_MOD
      USE DEFAULTS_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IG,IGTYP
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)::TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(*)
      my_real,
     .  INTENT(INOUT)::GEO(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(SOLID_DEFAULTS_), INTENT(IN) :: DEFAULTS_SOLID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER IHBE,ISMSTR,IPLAST,ICPRE,ICSTR,IINT,JCVT,
     .   NPG,NPT,NPTR,NPTS,NPTT, ISTRAIN,IET,IHBE_OLD,ID,
     .   NLY
      INTEGER     IHBE_DS,ISST_DS,ICONTROL_D,ICONTROL
      my_real
     .    CVIS,QA,QB,QH,VNS1,VNS2,DTMIN,VDEFMIN,VDEFMAX,ASPMAX,ASPTET 
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C-----hide and removed flags:
      IPLAST = 2                              
      ID = IG
C---- will branch later     
      ICPRE = 0  
      ISTRAIN =1 
      JCVT = 0
!--- defaults values      
      IHBE_DS=  DEFAULTS_SOLID%ISOLID
      ISST_DS=  DEFAULTS_SOLID%ISMSTR
      ICONTROL_D=DEFAULTS_SOLID%ICONTROL
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISOLID',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
c      CALL HM_GET_INTV('Icpre',ICPRE,IS_AVAILABLE,LSUBMODEL)
c      CALL HM_GET_INTV('Istrain',ISTRAIN,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icstr',ICSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_R',NPTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_S',NPTS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_T',NPTT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NBP',NPT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iint',IINT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icontrol',ICONTROL,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('dn',CVIS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('deltaT_min',DTMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('vdef_min',VDEFMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('vdef_max',VDEFMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ASP_max',ASPMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('COL_min',ASPTET,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-------for lecture check 
c         WRITE(IOUT,2100)IG,IHBE,ISMSTR,IPLAST,IINT,ICPRE,ICSTR,CVIS,QA,QB,
c     .                   DTMIN,ISTRAIN,NPTR,NPTS,NPTT
C-----------------------
C---   Default values
C-----------------------
C
C  ihbe
      IF (IHBE == 0) IHBE = IHBE_DS      
      IF (IHBE /= 14 .AND. IHBE /= 15 .AND. IHBE /= 16) THEN    
        CALL ANCMSG(MSGID=549,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR,
     .              I2=IHBE,
     .              I3=20)
      ENDIF                                                    
C
C   jcvt
C 
      IF (IHBE == 14 .OR. IHBE == 15) JCVT = 2 
      IF (IHBE == 16) JCVT = 1        
C
C  smstr
C
      IF (ISMSTR == 0) ISMSTR=ISST_DS    
      IF (ISMSTR == 0) ISMSTR=4          
      IF (ISST_DS == -2.OR.ISMSTR<0) ISMSTR=4          
c       IF(GEO( 3)/=ZERO.AND.IGEO( 5)== 0)IGEO( 5)=NINT(GEO( 3))
C
C  icpre/icstr
C
C      ICPRE = 0  
      IF (IHBE == 14 .AND. ICSTR == 0) ICSTR = 10
      IF (ICSTR > 111) ICSTR=0
      IF (IHBE == 14 .AND.
     .    (ICSTR /= 1.AND.ICSTR /= 10.AND.ICSTR /= 100)) THEN
         CALL ANCMSG(MSGID=677,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=ICSTR)
      END IF
C
C  iint
C     
      IF (IHBE == 16) THEN 
        IF (IINT == 0) IINT = 1    ! gauss integration
      ELSEIF (IHBE == 14 .OR. IHBE == 15) THEN 
        IINT = 1
      ENDIF
C
C  npt
C
      NLY = 0  
      SELECT CASE (IHBE)
      CASE(15)                                             
        IF (NPT == 0) NPT = 3                              
        IF (NPT < 1 .OR. NPT > 9)  THEN                    
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR,
     .                I2=NPT,
     .                I3=IHBE)
        ENDIF                                              
        NPG = NPT
        NLY = NPT
      CASE(14,16)                                          
c        NPT = NPTR*100+NPTS*10+ NPTT   
        IF (NPT == 0) THEN
         NPT = 222
         NPTR = 2 
         NPTS = 2
         NPTT = 2     
        END IF         
        NPG = NPTR*NPTS*NPTT 
        NLY = 0  
        IF (IHBE == 14) THEN
          SELECT CASE(ICSTR)
            CASE(1)
              NLY = NPTT
            CASE(10)
              NLY = NPTS
            CASE(100)
              NLY = NPTR
          END SELECT 
        ELSE
          NLY = NPTS
        ENDIF
                                    
        IF (IHBE == 14 .AND.                               
     .     (NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR.     
     .      NPTR > 9 .OR. NPTS > 9 .OR. NPTT > 9)) THEN   
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR,
     .                I2=NPT,
     .                I3=IHBE)
        ELSEIF (IHBE == 16 .AND.                           
     .     (NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR.     
     .      NPTR > 3 .OR. NPTS > 9 .OR. NPTT > 3)) THEN   
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR,
     .                I2=NPT,
     .                I3=IHBE)
        ENDIF                                              
      END SELECT 
!     
      IF (ICONTROL==0) ICONTROL=ICONTROL_D
      IF (ICONTROL>1) ICONTROL=0
C                                 
C  viscosity
C
      IF (IHBE /= 15) THEN
        CVIS = ZERO
      ELSEIF (CVIS == ZERO) THEN
        CVIS = EM01
      ENDIF
C igeo(31)  flag for default qa qb for law 70 can be used for other law
      IF(QA == ZERO .AND. QB == ZERO) IGEO(31) = 1
      IF (QA == ZERO) QA = ONEP1         
      IF (QB == ZERO) QB = FIVEEM2      
      IGEO(4)  = NPT                                  
      IGEO(5)  = ISMSTR               
      IGEO(9)  = IPLAST-1                                       
      IGEO(10) = IHBE 
      IGEO(12) = ISTRAIN                                       
      IGEO(13) = ICPRE                                 
      IGEO(14) = ICSTR                                 
      IGEO(15) = IINT                                 
      IGEO(16) = JCVT-1  
      IGEO(30) = NLY
      IGEO(97) = ICONTROL
C
      GEO(13)  = CVIS        
      GEO(14)  = QA
      GEO(15)  = QB
      GEO(172) = DTMIN
      GEO(190)= VDEFMIN
      GEO(191)= VDEFMAX
      GEO(192)= ASPMAX
      GEO(193)= ASPTET

C----------------------
      IF(.NOT.IS_ENCRYPTED)THEN
        IF(IGEO(31) == 1) THEN   
         WRITE(IOUT,1100)IG,IHBE,ISMSTR,IINT,ICSTR,CVIS,QA,QB,
     .                   DTMIN,ISTRAIN,ICONTROL
        ELSE
         WRITE(IOUT,1000)IG,IHBE,ISMSTR,IINT,ICSTR,CVIS,QA,QB,
     .                   DTMIN,ISTRAIN,ICONTROL
        ENDIF
        IF((VDEFMIN+VDEFMAX+ASPMAX+ASPTET)>ZERO) THEN
          IF (VDEFMAX==ZERO) VDEFMAX=EP10
          IF (ASPMAX==ZERO) ASPMAX=EP10
          WRITE(IOUT,3000) VDEFMIN,VDEFMAX,ASPMAX,ASPTET 
        END IF          
       IF (NPT > 200) THEN
        WRITE(IOUT,1001) NPG,NPT
       ELSE
        WRITE(IOUT,1002) NPG
       ENDIF  
      ELSE
       WRITE(IOUT,1099) IG
      ENDIF

C
C----Initialization in lecgeo:
       PROP_TAG(IGTYP)%G_SIG  = 6
       PROP_TAG(IGTYP)%L_SIG  = 6
       PROP_TAG(IGTYP)%G_EINT = 1
       PROP_TAG(IGTYP)%G_QVIS = 1
       PROP_TAG(IGTYP)%L_EINT = 1
       PROP_TAG(IGTYP)%G_VOL  = 1
       PROP_TAG(IGTYP)%L_VOL  = 1
       PROP_TAG(IGTYP)%L_QVIS = 1
       IF (MULTI_FVM%IS_USED)  PROP_TAG(IGTYP)%G_MOM = 3
       PROP_TAG(IGTYP)%G_FILL = 1
       PROP_TAG(IGTYP)%L_STRA = 6 
        IGEO(1) =IG
        IGEO(11)=IGTYP
        IGEO(17)=0
        GEO(12)= IGTYP + 0.1
C        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))
C---
      RETURN
C---
 1000 FORMAT(
     & 5X,'STANDARD THICK SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'INTEGRATION FORMULATION FLAG. . . .  . =',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SOLID DISTORTION CONTROL FLAG . . . . .=',I10/)
 1099 FORMAT(
     & 5X,'STANDARD THICK SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I8/,
     & 5X,'CONFIDENTIAL DATA'//)

 1100 FORMAT(
     & 5X,'STANDARD THICK SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'INTEGRATION FORMULATION FLAG. . . .  . =',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'DEFAULT VALUE FOR QUADRATIC BULK. . . . ',/,
     & 5X,'     VISCOSITY (QA) WILL BE USED. . . .=',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70 WHERE QA = 0.     ',/,
     & 5X,'DEFAULT VALUE FOR LINEAR BULK . . . . . ',/,
     & 5X,'     VISCOSITY (QB) WILL BE USED . . . =',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70 WHERE QB = 0.     ',/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SOLID DISTORTION CONTROL FLAG . . . . .=',I10/)
 1001 FORMAT(
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I3,
     &    ' (',I3,')'/)
 1002 FORMAT(
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/) 
 3000 FORMAT(
     & 5X,'SOLID MINIMUM VOLUMETRIC STRAIN........=',1PG20.13/,
     & 5X,'SOLID MAXIMUM VOLUMETRIC STRAIN........=',1PG20.13/,
     & 5X,'SOLID MAXIMUM ASPECT RATIO.............=',1PG20.13/,
     & 5X,'SOLID MINIMUM COLLAPSE RATIO...........=',1PG20.13/)
C

      END SUBROUTINE HM_READ_PROP20
