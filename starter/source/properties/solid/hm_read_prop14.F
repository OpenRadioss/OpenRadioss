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
Chd|  HM_READ_PROP14                source/properties/solid/hm_read_prop14.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP14(GEO,IGEO,PROP_TAG ,MULTI_FVM,IGTYP,IG,TITR,UNITAB,LSUBMODEL,
     .                          IPART)
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
C---s----1---------2---------3---------4---------5---------6---------7->
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "scr21_c.inc"
#include      "submod_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tablen_c.inc"
#include      "fluid_scr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IG,IGTYP
      INTEGER,INTENT(IN)::IPART(LIPART1,*)
      CHARACTER*nchartitle,INTENT(IN)::TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(*)
      my_real,
     .  INTENT(INOUT)::GEO(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER IHBE,ISMSTR,IPLAS,ICPRE,ICSTR,IINT,JCVT,
     .   NPG,NPT,NPTR,NPTS,NPTT, ISTRAIN,IET,IHBE_OLD,ID
      INTEGER I8PT,ITET4,NSPHDIR,ID_SENS,ID_PARTSPH,IPARTSPH,J,ITET10
      my_real
     .    CVIS,QA,QB,QH,VNS1,VNS2,DTMIN 
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C=======================================================================
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
        NSPHDIR = 0
        ID_PARTSPH = 0
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISOLID',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icpre',ICPRE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('I_rot',ITET4,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iframe',JCVT,IS_AVAILABLE,LSUBMODEL)
c      CALL HM_GET_INTV('Istrain',ISTRAIN,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ndir',NSPHDIR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SPHPART_ID',ID_PARTSPH,IS_AVAILABLE,LSUBMODEL)
c      CALL HM_GET_INTV('Imod',IET,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Itetra10',ITET10,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_R',NPTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_S',NPTS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_T',NPTT,IS_AVAILABLE,LSUBMODEL)
c      CALL HM_GET_INTV('Iplas',IPLAS,IS_AVAILABLE,LSUBMODEL)
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
C-------for lecture check 
c          WRITE(IOUT,2000)IG,IHBE,ISMSTR,IPLAS,JCVT,ITET4,ITET10,
c     .   		ICPRE,ICSTR,CVIS,QA,QB,QH,VNS1,VNS2,DTMIN, ISTRAIN,
c     .                  NPTR,NPTS,NPTT,IET,NSPHDIR,ID_PARTSPH
C-----hide and removed flags:
      ICSTR = 0                              
      ID_SENS = 0
      ISTRAIN=1 
      IPLAS = 2
      IET = 0
C
      IF(ITET4 == 0)  ITET4 = ITET4_D
      IF(ITET10 == 0)  ITET10 = ITET10_D

      IF(ITET4 == 2) THEN
c old single tetra4 & tetra10 formulation flag
        ITET10 = 2
        ITET4 = 1000
      ENDIF
C
      IINT = 0
      IF(ITET4 < 0) THEN
        IINT = -ITET4
        ITET4 = 0
      ENDIF
      ID = IG
C-------------------
      IPARTSPH=0
      IF(ID_PARTSPH>0)THEN
        DO J=1,NPART
          IF(IPART(4,J).EQ.ID_PARTSPH) THEN
           IPARTSPH=J
           GOTO 175
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=1037,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR,
     .              I2=ID_PARTSPH)
        CALL ARRET(2)
175     CONTINUE
      END IF
C-----------------------
C---   Default values
C-----------------------
C
C  ihbe
C
      IF (IHBE == 0) IHBE = IHBE_DS      
C     
C  iint : Lobato/GAuss (cache)
C------- used for elasto-platic critia parameter: IET---
      IF (IHBE == 16 ) THEN  ! not allowed
        IF (IINT == 0) IINT = 1    ! gauss integration
      ELSEIF (N2D ==1.AND.IHBE == 17 ) THEN 
        IF (IINT == 0) IINT = 1    
      ELSE
        IF (IHBE /= 24 ) IINT = 1
      ENDIF
C------  new solid17, IHBE will be changed to 17 in sgrtails.F    
      IF (IHBE == 18 ) IINT = 2 
C------  for Salim new solid17 IINT=3  
      IF (IHBE == 19 ) THEN
       IHBE = 17                                     
       IINT = 3 
      END IF
      IF (N2D > 0 .AND. IHBE/=0 .AND. IHBE/=2 .AND. IHBE/=17) THEN  
        IHBE_OLD=IHBE
        IHBE=IHBE_DS                                     
        CALL ANCMSG(MSGID=321,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=ID,
     .              C1=TITR,
     .              I2=IHBE_OLD,
     .              I3=IHBE)
      ELSEIF (IHBE  / =1 .AND. IHBE/=2 .AND. IHBE/=12 .AND.    
     .        IHBE / =13 .AND. IHBE /= 14 .AND. IHBE /= 16 .AND. 
     .        IHBE /= 24 .AND. IHBE /= 222.AND. IHBE /= 17.AND. IHBE /= 18) THEN    
        CALL ANCMSG(MSGID=549,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=ID,
     .              C1=TITR,
     .              I2=IHBE,
     .              I3=14)
      ENDIF    
      
      IF(QH.LT.ZERO .OR. QH.GE.QUINZE/CENT)THEN                                                                       
         CALL ANCMSG(MSGID=311,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               R1=QH)
      ENDIF 
            
C
C   jcvt
C 
      IF (JCVT == 0)  JCVT = IFRAME_DS
      IF (IHBE == 14.OR.IHBE == 18) JCVT = 2        
      IF (IHBE == 15) JCVT = 2        
      IF (IHBE == 16) JCVT = 1        
      IF (IHBE == 24) JCVT = 2   
      IF (IFRAME_DS == -2.OR.JCVT<0) JCVT = -1      
C-----      JCVT = 2 for lagrangian IHBE=1,2
C
C  smstr
C
      IF (ISMSTR == 0) ISMSTR=ISST_DS                            
      IF (ISMSTR == 0.AND.IHBE /= 18) ISMSTR=4
      IF (ISST_DS == -2) ISMSTR = -1      
C
C  Iplas
C
c      IF (IPLAS == 0) IPLAS=IPLA_DS                              
c      IF (IPLAS /=1 .AND. IPLAS /=2 .AND. IPLAS /=3 ) THEN 
c        CALL ANCMSG(MSGID=416,
c     .              MSGTYPE=MSGERROR,
c     .              ANMODE=ANINFO_BLIND_1,
c     .              I1=ID,
c     .              C1=TITR,
c     .              I2=IPLAS)
c      ENDIF                                                     
c      IF (IPLAS == 1) THEN                                    
c       IPLAS = 2                                    
c        CALL ANCMSG(MSGID=417,
c     .              MSGTYPE=MSGWARNING,
c     .              ANMODE=ANINFO_BLIND_1,
c     .              I1=ID,
c     .              C1=TITR,
c     .              I2=IPLAS)
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
        ICSTR = 0 
      ENDIF
      IF (ICPRE_D == -2) ICPRE = -1      
C
C  npt
C
      NPT = NPTR*100+NPTS*10+ NPTT     
      SELECT CASE (IHBE)
      CASE(14,16,222) 
        IF (NPT== 0) THEN                              
         NPTR= 2                                        
         NPTS= 2                                 
         NPTT= 2
         NPT = 222     
        END IF
        NPG = NPTR*NPTS*NPTT                                    
        IF (IHBE == 14 .AND.                                 
     .     (NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR.       
     .      NPTR > 9 .OR. NPTS > 9 .OR. NPTT > 9)) THEN     
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=NPT,
     .                I3=IHBE)
        ELSEIF (IHBE == 16 .AND.                             
     .     (NPTR < 1 .OR. NPTS < 1 .OR. NPTT < 1 .OR.       
     .      NPTR > 3 .OR. NPTS > 9 .OR. NPTT > 3)) THEN     
          CALL ANCMSG(MSGID=563,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=NPT,
     .                I3=IHBE)
        ENDIF                                                
      CASE(1,2,101,102,24)                                   
        NPT = 1
        NPG = NPT
      CASE(12,112,13,18)                                        
        NPT = 8                                              
        NPG = NPT
      END SELECT      
C
      IF (IHBE == 12 .OR. IHBE == 13 .OR. IHBE == 17 .OR. 
     .    (N2D == 1 .AND. IHBE == 22)) THEN
        IF (N2D == 0) THEN 
          NPT = 8
          NPG = NPT
        ELSE                        
          NPT = 4                                                 
          NPG = NPT
        ENDIF                                                          
      ENDIF                                                      

      IF(N2D > 0 .AND. IHBE/=0 .AND. IHBE/=2 .AND. IHBE/=17 .AND.
     .   (.NOT.(N2D==1.AND.IHBE==22))) THEN
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
C                                 
C  viscosity
C
C igeo(31)  flag for default qa qb for law 70 can be used for other law
      IF(QA == ZERO .AND. QB == ZERO) IGEO(31) = 1
      IF (QA == ZERO) QA = UNP1         
      IF (QB == ZERO) QB = CINQEM2      
C                                 
C  hourglass
C
      IF (IHBE == 24) THEN
        IF (CVIS == ZERO) CVIS = EM01
        GEO(13) = CVIS        
        QH   = ZERO
C---- default : auto        
        IINT = IET
      ELSEIF (IHBE==1.OR.IHBE==2) THEN
        IF (QH == ZERO) QH = EM01
        CVIS = ZERO
        GEO(13) = QH
      ELSE        
        QH   = ZERO
        CVIS = ZERO
        GEO(13) = ZERO
      ENDIF
C   Istrain : removed
C
c      IF (ISTRAIN == 0)ISTRAIN=ISTRA_D 
C
C   NSPHdir
C
C---------------------
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
C
      GEO(14)  = QA
      GEO(15)  = QB
      GEO(16)  = VNS1
      GEO(17)  = VNS2
      GEO(172)= DTMIN
C----------------------
      IF(.NOT.IS_CRYPTED)THEN
        IF(IGEO(31) == 1) THEN   
          WRITE(IOUT,1100)IG,IHBE,ISMSTR,IPLAS,JCVT,ITET4,ITET10,
     .   		ICPRE,ICSTR,CVIS,QA,QB,QH,VNS1,VNS2,DTMIN, ISTRAIN
        ELSE
          WRITE(IOUT,1000)IG,IHBE,ISMSTR,IPLAS,JCVT,ITET4,ITET10,
     .   		ICPRE,ICSTR,CVIS,QA,QB,QH,VNS1,VNS2,DTMIN, ISTRAIN

        ENDIF

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

C
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
       IF (N2D .NE. 0 .AND. MULTI_FVM%IS_USED) PROP_TAG(IGTYP)%G_AREA = 1
       IF (GEO(16) /= ZERO .OR. GEO(17) /= ZERO) THEN
         IGEO(33) = 1   ! ISVIS flag
       ENDIF         
        IGEO(1) =IG
        IGEO(11)=IGTYP
        IGEO(17)=0
        IF(GEO(39)/=ZERO.AND.IGEO(9)== 0)IGEO(9)=NINT(GEO(39))
        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))
        GEO(12)= IGTYP + 0.1
c        GEO(171)= IGEO(10)
        IF(ICAA==1) THEN
         GEO(1) = IGEO(4)
         IGEO(36) = 1
        ENDIF
C---
      RETURN
 999  CALL FREERR(3)
      RETURN
C---
 1000 FORMAT(
     & 5X,'STANDARD SOLID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SOLID FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
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
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/)
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
     & 5X,'SOLID STRESS PLASTICITY FLAG. . . . . .=',I10/,
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
     & 5X,'NUMERICAL NAVIER STOKES VISCO. LAMBDA .=',1PG20.13/,
     & 5X,'NUMERICAL NAVIER STOKES VISCOSITY MU. .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/)
 2010 FORMAT(
     & 5X,'HOURGLASS MODULUS FLAG. . . . . . . . .=',I10/)   
 2020 FORMAT(
     & 5X,'NUMBER OF SPH PARTICLES PER DIRECTION .=',I10/,
     & 5X,'CORRESPONDING PART FOR SPH PARTICLES. .=',I10/,
     & 5X,'SENSOR TO ACTIVATE SPH PARTICLES ......=',I10/)
 2000 FORMAT(
     & 5X,'BY HM LECTURE:'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SOLID FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
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
     & 5X,'Inpts_R . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'Inpts_S . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'Inpts_T . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'IHKT    . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'NSPHDIR . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'ID_PARTSPH. . . . . . . . . . . . . . .=',I10/)
C

      END SUBROUTINE HM_READ_PROP14
Chd|====================================================================
Chd|  HM_READ_PROP14F               source/properties/solid/hm_read_prop14.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_READ_ALE_CLOSE             source/ale/hm_read_ale_close.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP14F(GEO,IGEO,PROP_TAG ,MULTI_FVM,IGTYP,IG,TITR,UNITAB,LSUBMODEL)
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
C---s----1---------2---------3---------4---------5---------6---------7->
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "scr21_c.inc"
#include      "submod_c.inc"
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
      CHARACTER*nchartitle,INTENT(IN)::TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(*)
      my_real,
     .  INTENT(INOUT)::GEO(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL ou REAL*8
      INTEGER IHBE,ISMSTR,IPLAS,ICPRE,ICSTR,IINT,JCVT,
     .   NPG,NPT,NPTR,NPTS,NPTT, ISTRAIN,IET,IHBE_OLD,ID
      INTEGER I8PT,J
      my_real
     .    CVIS,QA,QB,QH,VNS1,VNS2,DTMIN 
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h',QH,IS_AVAILABLE,LSUBMODEL,UNITAB)
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

      IF(QH.LT.ZERO .OR. QH.GE.QUINZE/CENT)THEN                                                                       
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
       IF (N2D .NE. 0 .AND. MULTI_FVM%IS_USED) PROP_TAG(IGTYP)%G_AREA = 1
c       IF (GEO(16) /= ZERO .OR. GEO(17) /= ZERO) THEN
c         IGEO(33) = 1   ! ISVIS flag
c       ENDIF         
        IGEO(1) =IG
        IGEO(11)=IGTYP
        IGEO(17)=0
        IF(GEO(39)/=ZERO.AND.IGEO(9)== 0)IGEO(9)=NINT(GEO(39))
        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))
        GEO(12)= IGTYP + 0.1
        GEO(1) = IGEO(4)
C----------------------
      IF(.NOT.IS_CRYPTED)THEN
       WRITE(IOUT,1000)IG,IHBE,ISMSTR,JCVT,IINT,CVIS,QA,QB,QH,VNS1,VNS2
        WRITE(IOUT,1002) NPG
      ELSE
       WRITE(IOUT,1099) IG
      ENDIF
C----
      RETURN
 999  CALL FREERR(3)
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
