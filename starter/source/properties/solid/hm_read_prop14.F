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
!||    hm_read_prop14           ../starter/source/properties/solid/hm_read_prop14.F
!||--- called by ------------------------------------------------------
!||    hm_read_properties       ../starter/source/properties/hm_read_properties.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    arret                    ../starter/source/system/arret.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    hm_read_ale_close        ../starter/source/ale/hm_read_ale_close.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod             ../starter/source/modules/defaults_mod.F90
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP14(GEO,IGEO,PROP_TAG,MULTI_FVM,IGTYP,IG,TITR,UNITAB,
     .                          LSUBMODEL,IPART,DEFAULTS_SOLID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ PROPERTY TYPE14 (/PROP/SOLID)
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
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD         
      USE SUBMODEL_MOD
      USE ELBUFTAG_MOD            
      USE MULTI_FVM_MOD
      USE ALE_MOD
      USE DEFAULTS_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IG,IGTYP
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)::TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(NSUBMOD)
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM      
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(NPROPGI)
      my_real,INTENT(INOUT)::GEO(NPROPG)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SOLID_DEFAULTS_), INTENT(IN) :: DEFAULTS_SOLID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IHBE,ISMSTR,IPLAS,ICPRE,ICSTR,IINT,JCVT,NPG,NPT,NPTR,NPTS,NPTT, ISTRAIN,IET,IHBE_OLD,ID
      INTEGER I8PT,ITET4,ITET4_PREV,NSPHDIR,ID_SENS,ID_PARTSPH,IPARTSPH,J,ITET10,I_ALE_FLAG,IHBE_PR
      my_real CVIS,QA,QB,QH,VNS1,VNS2,DTMIN,VDEFMIN,VDEFMAX,ASPMAX,ASPTET 
      INTEGER     IHBE_DS,ISST_DS,IPLA_DS,IFRAME_DS,ITET4_D,ITET10_D,ICPRE_D,ICONTROL_D,ICONTROL
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED, lFOUND
      CHARACTER(LEN=NCHARLINE) ::  MSGLINE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      NSPHDIR = 0
      ID_PARTSPH = 0
!--- defaults values      
      IHBE_DS=  DEFAULTS_SOLID%ISOLID
      ISST_DS=  DEFAULTS_SOLID%ISMSTR
      ICPRE_D=  DEFAULTS_SOLID%ICPRE
      ITET4_D=  DEFAULTS_SOLID%ITETRA4
      ITET10_D= DEFAULTS_SOLID%ITETRA10
      IFRAME_DS= DEFAULTS_SOLID%IFRAME
      ICONTROL_D=DEFAULTS_SOLID%ICONTROL
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISOLID',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iale',I_ALE_FLAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icpre',ICPRE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('I_rot',ITET4,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iframe',JCVT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ndir',NSPHDIR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SPHPART_ID',ID_PARTSPH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Itetra10',ITET10,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_R',NPTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_S',NPTS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_T',NPTT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icontrol',ICONTROL,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Lambda',VNS1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MU',VNS2,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h',QH,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('dn',CVIS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('deltaT_min',DTMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('vdef_min',VDEFMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('vdef_max',VDEFMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ASP_max',ASPMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('COL_min',ASPTET,IS_AVAILABLE,LSUBMODEL,UNITAB)

      ICSTR = 0
      ID_SENS = 0
      ISTRAIN = 1
      IPLAS = 2
      IET = 0
      
C--------------------------------------------------
C DEFAULT VALUES & CHECKS
C--------------------------------------------------
      ! --default ITET10 ---
      IF(ITET10 == 0)THEN
        ITET10 = ITET10_D
      ENDIF
      IF(ITET10/=0 .AND. ITET10/=2 .AND. ITET10/=3 .AND. ITET10/=1000)THEN
        ITET10=1000
      ENDIF

      ! --default ITET4 ---  
      ITET4_PREV=ITET4    
      IF(ITET4 == 0 .OR. (ITET4 >= 4 .AND. ITET4/=1000) )THEN
        ! use value from /DEF_SOLID (by default or in case of unexpected value) : and check it below
        ITET4 = ITET4_D
      ENDIF 
      IF(ITET4 == 2) THEN
        ! old single tetra4 & tetra10 formulation flag : set ITET4 to 1000
        ITET10 = 2
        ITET4 = 1000  
        MSGLINE='   ITETRA4 IS SET TO 1000'
        CALL ANCMSG(MSGID=2027,MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1=IG,C1=TITR,I2=2,C2=TRIM(MSGLINE))      
      ELSEIF(ITET4 >=4 .AND. ITET4 /= 1000)THEN
        ! ITET4 may have been updated (ITET4=ITET4_D) : do not allow unexpected values from /DEF_SOLID
        MSGLINE='   ITETRA4 IS SET TO 1000'
        CALL ANCMSG(MSGID=2027,MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1=IG,C1=TITR,I2=ITET4,C2=TRIM(MSGLINE))
        ITET4 = 1000
      ELSEIF(ITET4_PREV >= 4 .AND. ITET4_PREV/=1000)THEN  
        !warn that unexpected user value was overwritten by value from /def_solid which is an expected one
         MSGLINE='   ITETRA4 IS SET TO           '
         WRITE(MSGLINE(22:31),FMT='(I0)')ITET4
        CALL ANCMSG(MSGID=2027,MSGTYPE=MSGWARNING,ANMODE=ANINFO,I1=IG,C1=TITR,I2=ITET4_PREV,C2=TRIM(MSGLINE))            
      ENDIF
      !backward compatibility
      IINT = 0
      IF(ITET4 < 0) THEN
        IINT = -ITET4
        ITET4 = 0   
      ENDIF
        
      ! --check SPHPART_ID  ---
      ID = IG      
      IPARTSPH=0
      lFOUND=.FALSE.
      IF(ID_PARTSPH>0)THEN
        DO J=1,NPART
          IF(IPART(4,J) == ID_PARTSPH) THEN
           IPARTSPH=J
           lFOUND=.TRUE.
          ENDIF
        ENDDO
        IF(.NOT.lFOUND)THEN
          CALL ANCMSG(MSGID=1037,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID, C1=TITR,I2=ID_PARTSPH)
          CALL ARRET(2)
        ENDIF
      END IF
      
      ! --default IHBE  ---      
      IF (IHBE == 0) IHBE = IHBE_DS      
      !  iint : Lobato/GAuss (cache)
      ! used for elasto-platic critia parameter: IET---
      IF (IHBE == 16 ) THEN  ! not allowed
        IF (IINT == 0) IINT = 1    ! gauss integration
      ELSEIF (N2D ==1.AND.IHBE == 17 ) THEN 
        IF (IINT == 0) IINT = 1    
      ELSEIF (IHBE == 5 ) THEN 
        IHBE = 1  
        IINT = 3
      ELSE
        IF (IHBE /= 24 ) IINT = 1
      ENDIF
      ! solid17, IHBE will be changed to 17 in sgrtails.F    
      IF (IHBE == 18 ) IINT = 2 
      ! solid17 IINT=3  
      IF (IHBE == 19 ) THEN
       IHBE = 17                                     
       IINT = 3 
      END IF
      
      ! --check IHBE  ---     
      IF (N2D > 0 .AND. IHBE/=0 .AND. IHBE/=2 .AND. IHBE/=17) THEN  
        IHBE_OLD=IHBE
        IHBE=IHBE_DS                                     
        CALL ANCMSG(MSGID=321,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,I1=ID,C1=TITR,I2=IHBE_OLD,I3=IHBE)
      ELSEIF (IHBE  / =1 .AND. IHBE/=2 .AND. IHBE/=12 .AND. IHBE / =13 .AND. IHBE /= 14 .AND. IHBE /= 16 .AND. 
     .        IHBE /= 24 .AND. IHBE /= 222.AND. IHBE /= 17.AND. IHBE /= 18) THEN    
        CALL ANCMSG(MSGID=549, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,I1=ID,C1=TITR,I2=IHBE,I3=14)
        IHBE=1
      ENDIF  
      
      ! --default I_ALE_FLAG  ---
      ! I_ALEF_LAG : 0 (Lagrange)
      !              1 (ale)
      !              2 (euler)
      IF(I_ALE_FLAG <= 0 .OR. I_ALE_FLAG >= 3)THEN
        I_ALE_FLAG = 0
      ENDIF
      IF(I_ALE_FLAG /= 0)THEN
        IF(IHBE /= 0 .AND. IHBE /= 1 .AND. IHBE /= 2 )THEN
           CALL ANCMSG(MSGID=131,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR, I2=IHBE)          
        ENDIF
      ENDIF          
      
      ! --check QH  ---           
      IF(QH < ZERO .OR. QH >= FIFTEEN/HUNDRED)THEN                                                                       
         CALL ANCMSG(MSGID=311,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1, I1=ID,C1=TITR,R1=QH)
      ENDIF 
            
      ! --default JCVT  --- 
      IF (JCVT == 0)  JCVT = IFRAME_DS
      IF (IHBE == 14.OR.IHBE == 18) JCVT = 2        
      IF (IHBE == 15) JCVT = 2        
      IF (IHBE == 16) JCVT = 1        
      IF (IHBE == 24) JCVT = 2   
      IF (IFRAME_DS == -2.OR.JCVT<0) JCVT = -1      
      ! JCVT = 2 for lagrangian IHBE=1,2

      ! --default ISMSTR --- 
      IF (ISMSTR == 0) ISMSTR=ISST_DS                            
      IF (ISMSTR == 0.AND.IHBE /= 18) ISMSTR=4
      IF (ISST_DS == -2) ISMSTR = -1      

      ! --default icpre/icstr  --- 
      IF (ICPRE == 0) ICPRE = ICPRE_D      
      IF((N2D > 0 .AND. IHBE == 17) ) THEN
        IF(ICPRE/=1 .AND. ICPRE/=2) ICPRE=0
        ! no effet for Axi Isolid17 for the moment
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
        ICSTR = 0 
      ENDIF
      IF (ICPRE_D == -2) ICPRE = -1
            
      ! --default NPT  --- 
      NPT = NPTR*100 + NPTS*10 + NPTT     
      SELECT CASE (IHBE)
      CASE(14,16,222) 
        IF (NPT== 0) THEN                              
         NPTR= 2                                        
         NPTS= 2                                 
         NPTT= 2
         NPT = 222     
        END IF
        NPG = NPTR*NPTS*NPTT                                    
        IF (IHBE == 14 .AND.(NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR.NPTR > 9 .OR. NPTS > 9 .OR. NPTT > 9)) THEN     
          CALL ANCMSG(MSGID=563,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,I1=ID, C1=TITR,I2=NPT, I3=IHBE)
        ELSEIF (IHBE == 16 .AND.(NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR. NPTR > 3 .OR. NPTS > 9 .OR. NPTT > 3)) THEN     
          CALL ANCMSG(MSGID=563,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1, I1=ID, C1=TITR,I2=NPT, I3=IHBE)
        ENDIF                                                
      CASE(1,2,101,102,24)                                   
        NPT = 1
        NPG = NPT
      CASE(12,112,13,18)                                        
        NPT = 8                                              
        NPG = NPT
      END SELECT      

      IF (IHBE == 12 .OR. IHBE == 13 .OR. IHBE == 17 .OR.(N2D == 1 .AND. IHBE == 22)) THEN
        IF (N2D == 0) THEN 
          NPT = 8
          NPG = NPT
        ELSE                        
          NPT = 4                                                 
          NPG = NPT
        ENDIF                                                          
      ENDIF                                                      

      IF(N2D > 0 .AND. IHBE/=0 .AND. IHBE/=2 .AND. IHBE/=17 .AND. (.NOT.(N2D==1.AND.IHBE==22))) THEN
         IHBE_OLD=IHBE
         IHBE=0                                                        
         CALL ANCMSG(MSGID=324, MSGTYPE=MSGWARNING,  ANMODE=ANINFO_BLIND_2, I1=ID,  C1=TITR, I2=IHBE_OLD, I3=IHBE)
      ENDIF                                                            
      IF (ICONTROL==0) ICONTROL=ICONTROL_D
      IF (ICONTROL>1) ICONTROL=0

      ! --default QA,QB (PSEUDO VISCOSITY)  ---  
      ! igeo(31)  flag for default qa qb for law 70  can be used for other law
      IF (QA == ZERO .AND. QB == ZERO) IGEO(31) = 1
      IF (QA == ZERO) QA = ONEP1         
      IF (QB == ZERO) QB = FIVEEM2      

      ! --default CVIS (hourglass)  --- 
      IF (IHBE == 24) THEN
        IF (CVIS == ZERO) CVIS = EM01
        GEO(13) = CVIS        
        QH   = ZERO
        ! default : auto        
        IINT = IET
      ELSEIF (IHBE==1.OR.IHBE==2) THEN
        IF (QH == ZERO.AND.ICONTROL==0) QH = EM01
        IF (QH == ZERO.AND.ICONTROL==1) QH = ONE
        CVIS = ZERO
        GEO(13) = QH
      ELSE        
        QH   = ZERO
        CVIS = ZERO
        GEO(13) = ZERO
      ENDIF

C--------------------------------------------------
C BUFFER STORAGE
C--------------------------------------------------     
      IGEO(4)  = NPT                                  
      IGEO(5)  = ISMSTR               
      IGEO(9)  = IPLAS-1                                       
      IGEO(10) = IHBE                                        
      IGEO(12) = ISTRAIN
      IGEO(13) = ICPRE                                 
      IGEO(14) = ICSTR                                 
      IGEO(15) = IINT                                 
      IGEO(16) = JCVT-1
      IGEO(37) = NSPHDIR
      IGEO(38) = IPARTSPH
      IGEO(39) = ID_SENS
      IGEO(62) = I_ALE_FLAG
      IGEO(97) = ICONTROL
      
      GEO(14)  = QA
      GEO(15)  = QB
      GEO(16)  = VNS1
      GEO(17)  = VNS2
      GEO(172)= DTMIN
      GEO(190)= VDEFMIN
      GEO(191)= VDEFMAX
      GEO(192)= ASPMAX
      GEO(193)= ASPTET
      
C--------------------------------------------------
C LISTING OUTPUTS
C--------------------------------------------------
      IHBE_PR = IHBE
      IF (IHBE==1.AND.IINT==3) IHBE_PR=5
      IF(.NOT.IS_ENCRYPTED)THEN
        IF(IGEO(31) == 1) THEN   
          WRITE(IOUT,1100)IG,IHBE_PR,ISMSTR,I_ALE_FLAG,IPLAS,JCVT,ITET4,
     .                    ITET10,ICPRE,ICSTR,CVIS,QA,QB,QH,VNS1,VNS2,DTMIN, ISTRAIN,ICONTROL
        ELSE
          WRITE(IOUT,1000)IG,IHBE_PR,ISMSTR,I_ALE_FLAG,IPLAS,JCVT,ITET4,
     .                    ITET10,ICPRE,ICSTR,CVIS,QA,QB,QH,VNS1,VNS2,DTMIN, ISTRAIN,ICONTROL
        ENDIF
        IF((VDEFMIN+VDEFMAX+ASPMAX+ASPTET)>ZERO) THEN
          IF (VDEFMAX==ZERO) VDEFMAX=EP10
          IF (ASPMAX==ZERO) ASPMAX=EP10
          WRITE(IOUT,3000) VDEFMIN,VDEFMAX,ASPMAX,ASPTET 
        END IF          
          IF (IET > 0) WRITE(IOUT,2010) IET
          IF (NPT > 200) THEN
           WRITE(IOUT,1001) NPG,NPT
          ELSE
           WRITE(IOUT,1002) NPG
          ENDIF  
          IF(NSPHDIR/=0)WRITE(IOUT,2020)NSPHDIR, ID_PARTSPH, ID_SENS
      ELSE
       WRITE(IOUT,1099) IG
      ENDIF

      IF (ITET4 == 1000) ITET4 = 0
      IGEO(20) = ITET4
      IF (ITET10 == 1000) ITET10 = 0
      IGEO(50) = ITET10

C--------------------------------------------------
C /ALE/CLOSE/<prop_id>
C--------------------------------------------------
      CALL HM_READ_ALE_CLOSE(UNITAB, LSUBMODEL, GEO)    !GEO(129) & GEO(130)
      
C--------------------------------------------------
C SIZES FOR INITIALIZATION (lecgeo)
C--------------------------------------------------

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
       IF (N2D  /=  0 .AND. MULTI_FVM%IS_USED) PROP_TAG(IGTYP)%G_AREA = 1
       IF (GEO(16) /= ZERO .OR. GEO(17) /= ZERO) THEN
         IGEO(33) = 1   ! ISVIS flag
       ENDIF         
        IGEO(1) =IG
        IGEO(11)=IGTYP
        IGEO(17)=0
        IF(GEO(39)/=ZERO.AND.IGEO(9)== 0)IGEO(9)=NINT(GEO(39))
        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))
        GEO(12)= IGTYP + 0.1
        IF(ALE%GLOBAL%ICAA==1) THEN
         GEO(1) = IGEO(4)
         IGEO(36) = 1
        ENDIF
C---
      RETURN
C---
 1000 FORMAT(
     & 5X,'STANDARD SOLID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SOLID FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'IALE FLAG (0:LAGRANGE,1:ALE,2:EULER). .=',I10/,       
     & 5X,'SOLID STRESS PLASTICITY FLAG. . . . . .=',I10/,
     & 5X,'COROTATIONAL SYSTEM FLAG. . . . . . . .=',I10/,
     & 5X,'TETRA4 FORMULATION FLAG. . . . .  . . .=',I10/,
     & 5X,'TETRA10 FORMULATION FLAG . . . .  . . .=',I10/,
     & 5X,'CONSTANT PRESSURE FLAG. . . . . . . . .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'HOURGLASS VISCOSITY . . . . . . . . . .=',1PG20.13/,
     & 5X,'NUMERICAL NAVIER STOKES VISCO. LAMBDA .=',1PG20.13/,
     & 5X,'NUMERICAL NAVIER STOKES VISCOSITY MU. .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SOLID DISTORTION CONTROL FLAG . . . . .=',I10/)
 1001 FORMAT(
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I3,
     &    ' (',I3,')'/)
 1002 FORMAT(
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/)
 1099 FORMAT(
     & 5X,'STANDARD SOLID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I8/,
     & 5X,'CONFIDENTIAL DATA'//)
 1100 FORMAT(
     & 5X,'STANDARD SOLID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SOLID FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'IALE FLAG (0:LAGRANGE,1:ALE,2:EULER). .=',I10/,       
     & 5X,'SOLID STRESS PLASTICITY FLAG. . . . . .=',I10/,
     & 5X,'COROTATIONAL SYSTEM FLAG. . . . . . . .=',I10/,
     & 5X,'TETRA4 FORMULATION FLAG. . . . .  . . .=',I10/,
     & 5X,'TETRA10 FORMULATION FLAG . . . .  . . .=',I10/,
     & 5X,'CONSTANT PRESSURE FLAG. . . . . . . . .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'DEFAULT VALUE FOR QUADRATIC BULK. . . . ',/,
     & 5X,'     VISCOSITY (QA) WILL BE USED. . . .=',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70 QA = 0.     ',/,
     & 5X,'DEFAULT VALUE FOR LINEAR BULK . . . . . ',/,
     & 5X,'     VISCOSITY (QB) WILL BE USED . . . =',1PG20.13/,
     & 5X,'EXCEPT IN CASE LAW 70  QB = 0.     ',/,
     & 5X,'HOURGLASS VISCOSITY . . . . . . . . . .=',1PG20.13/,
     & 5X,'NUMERICAL NAVIER STOKES VISCO. LAMBDA .=',1PG20.13/,
     & 5X,'NUMERICAL NAVIER STOKES VISCOSITY MU. .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SOLID DISTORTION CONTROL FLAG . . . . .=',I10/)
 2010 FORMAT(
     & 5X,'HOURGLASS MODULUS FLAG. . . . . . . . .=',I10/)   
 2020 FORMAT(
     & 5X,'NUMBER OF SPH PARTICLES PER DIRECTION .=',I10/,
     & 5X,'CORRESPONDING PART FOR SPH PARTICLES. .=',I10/,
     & 5X,'SENSOR TO ACTIVATE SPH PARTICLES ......=',I10/)
 3000 FORMAT(
     & 5X,'SOLID MINIMUM VOLUMETRIC STRAIN........=',1PG20.13/,
     & 5X,'SOLID MAXIMUM VOLUMETRIC STRAIN........=',1PG20.13/,
     & 5X,'SOLID MAXIMUM ASPECT RATIO.............=',1PG20.13/,
     & 5X,'SOLID MINIMUM COLLAPSE RATIO...........=',1PG20.13/)
C
      END SUBROUTINE HM_READ_PROP14


!||====================================================================
!||    hm_read_prop14f          ../starter/source/properties/solid/hm_read_prop14.F
!||--- called by ------------------------------------------------------
!||    hm_read_properties       ../starter/source/properties/hm_read_properties.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    hm_read_ale_close        ../starter/source/ale/hm_read_ale_close.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod             ../starter/source/modules/defaults_mod.F90
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP14F(GEO,IGEO,PROP_TAG,MULTI_FVM,IGTYP,IG,TITR,UNITAB,LSUBMODEL,
     .                          DEFAULTS_SOLID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ PROPERTY /PROP/FLUID
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
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD         
      USE SUBMODEL_MOD
      USE ELBUFTAG_MOD            
      USE MULTI_FVM_MOD
      USE ALE_MOD
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
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IG,IGTYP
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)::TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(NSUBMOD)
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM      
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(NPROPGI)
      my_real,INTENT(INOUT)::GEO(NPROPG)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SOLID_DEFAULTS_), INTENT(IN) :: DEFAULTS_SOLID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IHBE,ISMSTR,IPLAS,ICPRE,ICSTR,IINT,JCVT,NPG,NPT,NPTR,NPTS,NPTT, ISTRAIN,IET,IHBE_OLD,ID,I8PT,J
      INTEGER     IHBE_DS,ISST_DS,IPLA_DS,IFRAME_DS
      my_real CVIS,QA,QB,QH,VNS1,VNS2,DTMIN 
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h',QH,IS_AVAILABLE,LSUBMODEL,UNITAB)
!--- defaults values      
      IHBE_DS=  DEFAULTS_SOLID%ISOLID
      ISST_DS=  DEFAULTS_SOLID%ISMSTR
      IFRAME_DS= DEFAULTS_SOLID%IFRAME
C-----hide and removed flags:
      ICSTR = 0                              
      IHBE=1
      ISMSTR=ISST_DS                            
      IF (ISMSTR == 0) ISMSTR=4
      IPLAS=0
      ICPRE = 0
      NPT = 1
      NPG = NPT
      IINT=1 !Lobato/GAuss (cache)
      JCVT = IFRAME_DS
      CVIS=ZERO  
      ID = IG
      VNS1=ZERO
      VNS2=ZERO
C-----------------------
C---   Default values
C-----------------------
C
C  ihbe
C  hide flag IHGFLU lost
      IF(N2D > 0) IHBE = 0
c      IF(N2D > 0)IHBE = IHGFLU
C      
      IF (N2D > 0 .AND. IHBE/=0 .AND. IHBE/=2) THEN  
        IHBE_OLD=IHBE
        IHBE=IHBE_DS                                     
        CALL ANCMSG(MSGID=321,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=ID,
     .              C1=TITR,
     .              I2=IHBE_OLD,
     .              I3=IHBE)
      ENDIF                                                

      IF(N2D > 0 .AND. IHBE /=0 .AND. IHBE /= 2)THEN                   
         IHBE_OLD=IHBE
         IHBE=0                                                        
         CALL ANCMSG(MSGID=324,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=IHBE_OLD,
     .               I3=IHBE)
      ENDIF                                                            

      IF(QH  < ZERO .OR. QH >= FIFTEEN/HUNDRED)THEN                                                                       
         CALL ANCMSG(MSGID=311,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               R1=QH)
      ENDIF  
C      
        IF (QH == ZERO) QH = EM01
        GEO(13) = QH
C    
      !PSEUDO-VISCOSITY IS REQUIRED FOR STABILITY AND FOR SHOCK MODELING (STAGGERED SCHEME) 
      IF(ALE%GLOBAL%ICAA == 0)THEN
        !CAA is an obsolete option. Unless it is defined, program will set default values
        IF (QA == ZERO) QA = ONEP1         
        IF (QB == ZERO) QB = FIVEEM2     
      ENDIF  
 
!     /ALE/CLOSE
!     ----------
      CALL HM_READ_ALE_CLOSE(UNITAB, LSUBMODEL, GEO)

      IGEO(4)  = NPT                                  
      IGEO(5)  = ISMSTR               
      IGEO(10) = IHBE                                        
      IGEO(13) = ICPRE                                 
      IGEO(14) = ICSTR                                 
      IGEO(15) = IINT                                 
      IGEO(16) = JCVT-1
      IGEO(36) = 1 !IGFLU
C
      GEO(14)  = QA
      GEO(15)  = QB
      GEO(16)  = ZERO
      GEO(17)  = ZERO
C----Initialization in lecgeo:
c       IF(GEO( 3)/=ZERO.AND.IGEO( 5)== 0)IGEO( 5)=NINT(GEO( 3))
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
       IF (N2D  /=  0 .AND. MULTI_FVM%IS_USED) PROP_TAG(IGTYP)%G_AREA = 1     
        IGEO(1) =IG
        IGEO(11)=IGTYP
        IGEO(17)=0
        IF(GEO(39)/=ZERO.AND.IGEO(9)== 0)IGEO(9)=NINT(GEO(39))
        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))
        GEO(12)= IGTYP + 0.1
        GEO(1) = IGEO(4)
C----------------------
      IF(.NOT.IS_ENCRYPTED)THEN
       WRITE(IOUT,1000)IG,IHBE,ISMSTR,JCVT,IINT,CVIS,QA,QB,QH,VNS1,VNS2
        WRITE(IOUT,1002) NPG
      ELSE
       WRITE(IOUT,1099) IG
      ENDIF
C----
      RETURN
C---
 1000 FORMAT(
     & 5X,'STANDARD FLUID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'COROTATIONAL SYSTEM FLAG. . . . . . . .=',I10/,
     & 5X,'INTEGRATION FORMULATION FLAG. . . .  . =',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'HOURGLASS VISCOSITY . . . . . . . . . .=',1PG20.13/,
     & 5X,'NUMERICAL NAVIER STOKES VISCO. LAMBDA .=',1PG20.13/,
     & 5X,'NUMERICAL NAVIER STOKES VISCOSITY MU. .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13)
 1002 FORMAT(
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/)
C---
 1099 FORMAT(
     & 5X,'STANDARD FLUID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I8/,
     & 5X,'CONFIDENTIAL DATA'//)
C
      END SUBROUTINE HM_READ_PROP14F
