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
Chd|  HM_READ_PROP22                source/properties/thickshell/hm_read_prop22.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP22(GEO,IGEO ,IGTYP,IG,TITR,UNITAB,LSUBMODEL,
     .                          PM ,IPM ,RTRANS   ,SUB_ID ,ISKN,PROP_TAG)
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
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IG,IGTYP,SUB_ID,ISKN(LISKN,*),IPM(NPROPMI,*)
      CHARACTER*nchartitle,INTENT(IN)::TITR
      my_real,
     .  INTENT(IN)::RTRANS(NTRANSF,*),PM(NPROPM,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(*)
      my_real,
     .  INTENT(INOUT)::GEO(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER IHBE,ISMSTR,IPLAST,ICPRE,ICSTR,IINT,JCVT,
     .   NPG,NPT,NPTR,NPTS,NPTT, ISTRAIN,IET,IHBE_OLD,ID,
     .   IREP,IPOS
      my_real
     .    CVIS,QA,QB,QH,VNS1,VNS2,DTMIN,ASHEAR
      INTEGER NUML(200), NLY, NLYMAX, N, J, M1, M2, M3,ICOMPA,
     .   K,  N1,IDSK, IPANG, IPTHK, IPPOS, IPMAT,IERRS,IP,ISK
      my_real
     .   AN,  DT, THK, TMIN, TMAX,ANG,VX,VY,VZ
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      NLYMAX= 200
      IPANG = 200
      IPTHK = IPANG+NLYMAX
      IPPOS = IPTHK+NLYMAX
      IPMAT = 100
C-----hide and removed flags:
      IPLAST = 2                              
      ID = IG
C---- will branche later     
      ICPRE = 0  
      ISTRAIN = 1  
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
      CALL HM_GET_INTV('NBP',NPT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Icstr',ICSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_R',NPTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_S',NPTS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Inpts_T',NPTT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iint',IINT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SKEW_CSID',IDSK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ORTHTROP',IREP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ipos',IPOS,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('dn',CVIS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vx',VX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vy',VY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vz',VZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('AREA_SHEAR',ASHEAR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('deltaT_min',DTMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-------for lecture check 
c         WRITE(IOUT,2100)IG,IHBE,ISMSTR,IPLAST,IINT,ICPRE,ICSTR,CVIS,QA,QB,
c     .                   DTMIN,ISTRAIN,NPTR,NPTS,NPTT
Capply submodel transform to V (VX,VY,VZ) 
C
      IF (SUB_ID /= 0)
     .         CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)
C-----------------------
C---   Default values
C-----------------------
C  ihbe
      IF (IHBE == 0) IHBE = IHBE_DS      
      IF (IHBE /= 14 .AND. IHBE /= 15 ) THEN    
        CALL ANCMSG(MSGID=549,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR,
     .              I2=IHBE,
     .              I3=22)
      ENDIF                                                    
C
C   jcvt
C 
      IF (IHBE == 14 .OR. IHBE == 15) JCVT = 2 
c      IF (IHBE == 16) JCVT = 1        
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
C  iint Isolid=16 incompatible to ortho
C     
      IF (IHBE == 15) IINT = 1
C
C  npt
C
      SELECT CASE (IHBE)
      CASE(15)                                             
c        NPT = NPTT
        NLY = NPT
        IP = 3
      CASE(14)
        SELECT CASE (ICSTR)
          CASE(100)
           NLY = NPTR
           IP = 2
           IF (NPTS < 2 .OR. NPTT < 2 ) THEN
            CALL ANCMSG(MSGID=563,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IG,
     .                  C1=TITR,
     .                  I2=NPT,
     .                  I3=IHBE)
           ENDIF
          CASE(10)
           NLY = NPTS
           IP = 3
           IF (NPTR < 2 .OR. NPTT < 2 ) THEN
            CALL ANCMSG(MSGID=563,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IG,
     .                  C1=TITR,
     .                  I2=NPT,
     .                  I3=IHBE)
           ENDIF
          CASE(1)
           NLY = NPTT
           IP = 1
           IF (NPTS < 2 .OR. NPTR < 2 ) THEN
            CALL ANCMSG(MSGID=563,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IG,
     .                  C1=TITR,
     .                  I2=NPT,
     .                  I3=IHBE)
           ENDIF
        END SELECT
        IF (NLY ==0) THEN
	 NLY =IINT
	ELSE
	 IINT = 1
	ENDIF
      END SELECT      
      IF (NLY<=0) THEN
         CALL ANCMSG(MSGID=27,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (NLY>NLYMAX) THEN
         CALL ANCMSG(MSGID=28,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I2=NLYMAX,
     .               I1=IG,
     .               C1=TITR)
	     NLY =NLYMAX
      ENDIF
C                                 
C  viscosity
C
      IF (IHBE == 14) THEN
        CVIS = ZERO
      ELSEIF (CVIS == ZERO) THEN
        CVIS = EM01
      ENDIF
C igeo(31)  flag for default qa qb for law 70 can be used for other law
      IF(QA == ZERO .AND. QB == ZERO) IGEO(31) = 1
      IF (QA == ZERO) QA = ONEP1         
      IF (QB == ZERO) QB = FIVEEM2  
C      
      IF(ASHEAR==ZERO)ASHEAR=ONE
      IF(NLY==1) ASHEAR= EM10
      AN=SQRT(VX*VX+VY*VY+VZ*VZ)
      IF(AN < EM10)THEN
        VX=ONE
        VY=ZERO
        VZ=ZERO
      ELSE
        AN=ONE/AN
        VX=VX*AN
        VY=VY*AN
        VZ=VZ*AN
      ENDIF
      ISK = 0
      IF (IDSK/=0) THEN
        DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(IDSK==ISKN(4,J+1)) THEN
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
     .              C3=TITR,
     .              I2=IDSK)
 10     CONTINUE
      ENDIF
      IF (IP <= 0) THEN
        DO J=0,NUMSKW
          IF(ISK == ISKN(4,J+1)) THEN
            IP=-(J+1)
            GO TO 20
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,
     .              ANMODE=ANINFO,
     .              MSGTYPE=MSGERROR,
     .              C1='PROPERTY',
     .              C2='PROPERTY',
     .              I1=IG,
     .              I2=ISK,
     .              C3=TITR)
20      CONTINUE
      ENDIF
C---------------------
      IGEO(2)  = IP
      IGEO(4)  = NPT
      IGEO(5)  = ISMSTR
      IGEO(6)  = IREP
      IGEO(7)  = ISK
      IGEO(9)  = IPLAST-1
      IGEO(10) = IHBE
      IGEO(13) = ICPRE
      IGEO(14) = ICSTR
      IGEO(15) = IINT
      IGEO(16) = JCVT-1
      IGEO(30) = NLY
      IGEO(12) = ISTRAIN
C
      GEO(1)   = ZERO
      GEO(7)   = VX
      GEO(8)   = VY
      GEO(9)   = VZ
      GEO(13)  = CVIS
      GEO(14)  = QA
      GEO(15)  = QB
      GEO(38) = ASHEAR
C
      GEO(172) = DTMIN
C---------------------
       THK =ZERO
      DO K=1,NLY
        CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_phi',GEO(IPANG+K),K,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Thick',GEO(IPTHK+K),K,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Zi',GEO(IPPOS+K),K,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_INT_ARRAY_INDEX('Prop_mi',NUML(K),K,IS_AVAILABLE,LSUBMODEL)
        THK=THK+GEO(IPTHK+K)
      ENDDO
C--------------
       IERRS = THK*EP02
       IF (IABS(IERRS-100)>1) THEN
        CALL ANCMSG(MSGID=675,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
       ENDIF
C---  Position des couches-----------
       IF (IPOS>0)THEN
        TMIN = EM20
        TMAX =-EM20
        DO K=1,NLY
          DT = HALF*GEO(IPTHK+K)
          TMIN = MIN(TMIN,GEO(IPPOS+K)-DT)
          TMAX = MAX(TMAX,GEO(IPPOS+K)+DT)
        ENDDO
C------------[-0.5,0.5]----------
        THK = ABS(TMAX-TMIN-ONE)
        IF ( TMIN<-HALF.OR.TMAX>HALF.OR.THK>EM10) THEN
         CALL ANCMSG(MSGID=674,
         
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
        ENDIF
       ELSE
C---  calcul automatique de position des couches -->(-1,1)
        GEO(IPPOS+1) = -HALF +HALF*GEO(IPTHK+1)
        DO K=2,NLY
         GEO(IPPOS+K)=GEO(IPPOS+K-1)+
     .                HALF*(GEO(IPTHK+K)+GEO(IPTHK+K-1))
        ENDDO
       ENDIF
C      TEST MATERIAUX: NON incompability---------
       ICOMPA = 0
       DO 100 K=1,NLY
         DO J=1,NUMMAT
          IF(IPM(1,J)==NUML(K)) THEN
            IGEO(IPMAT+K)=J
            IF(NINT(PM(19,J))==14.OR.NINT(PM(19,J))==24
     .        .OR.NINT(PM(19,J))==25) ICOMPA = J
            GO TO 100
          ENDIF
        ENDDO
C          ERREUR NO DE LOI DE COMP. ERRONEE
         CALL ANCMSG(MSGID=676,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=NUML(K))
         IGEO(IPMAT+K)=1
  100    CONTINUE

C----------------------
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,1000)IG
      ELSE
       IF(ISK==0)THEN
         IF(IGEO(31) == 1)THEN
           
           WRITE(IOUT,2100)IG,IHBE,ISMSTR,NPT,ICSTR,
     .                  CVIS,QA,QB,DTMIN,NLY,IPOS,ASHEAR,IREP,
     .                  VX,VY,VZ
         ELSE
           WRITE(IOUT,2000)IG,IHBE,ISMSTR,NPT,ICSTR,
     .                  CVIS,QA,QB,DTMIN,NLY,IPOS,ASHEAR,IREP,
     .                  VX,VY,VZ
         ENDIF
       ELSE
         IF(IGEO(31) == 1)THEN
           WRITE(IOUT,2101)IG,IHBE,ISMSTR,NPT,ICSTR,
     .                CVIS,QA,QB,DTMIN,NLY,IPOS,ASHEAR,IREP,ISKN(4,ISK)
         ELSE
           WRITE(IOUT,2001)IG,IHBE,ISMSTR,NPT,ICSTR,
     .                CVIS,QA,QB,DTMIN,NLY,IPOS,ASHEAR,IREP,ISKN(4,ISK)
         ENDIF
       ENDIF
      ENDIF
C
      DO K=1,NLY
          M1=IPANG+K
          M2=IPTHK+K
          M3=IPPOS+K
          IF(.NOT.IS_ENCRYPTED) WRITE(IOUT,3000) K,GEO(M1),GEO(M2),GEO(M3),NUML(K)
          GEO(M2)=TWO*GEO(M2)
          GEO(M3)=TWO*GEO(M3)
      ENDDO
C----Initialization in lecgeo:
       PROP_TAG(IGTYP)%G_SIG  = 6
       PROP_TAG(IGTYP)%L_SIG  = 6
       PROP_TAG(IGTYP)%G_EINT = 1
       PROP_TAG(IGTYP)%G_QVIS = 1
       PROP_TAG(IGTYP)%L_EINT = 1
       PROP_TAG(IGTYP)%G_VOL  = 1
       PROP_TAG(IGTYP)%L_VOL  = 1
       PROP_TAG(IGTYP)%L_QVIS = 1
       PROP_TAG(IGTYP)%G_FILL = 1
       PROP_TAG(IGTYP)%L_STRA = 6 
       PROP_TAG(IGTYP)%G_GAMA = 6
       PROP_TAG(IGTYP)%L_GAMA = 6
C
        IGEO(1) =IG
        IGEO(11)=IGTYP
        IGEO(17)=1
        GEO(12)= IGTYP + 0.1
C        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))

C---
      RETURN
 999  CALL FREERR(3)
      RETURN
C---
 1000    FORMAT(//,
     &    5X,'COMPOSITE LAYERED THICK SHELL PROPERTY SET'/,
     &    5X,'------------------------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
 2000 FORMAT(//,
     & 5X,'COMPOSITE LAYERED THICK SHELL PROPERTY SET'/,
     & 5X,'WITH HETEROGENIOUS PROPERTY IN THICKNESS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13//,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POSITION INPUT FLAG . . . . . . . . . .=',I10/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/)
 2001 FORMAT(//,
     & 5X,'COMPOSITE LAYERED THICK SHELL PROPERTY SET'/,
     & 5X,'WITH HETEROGENIOUS PROPERTY IN THICKNESS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. .  . .  .=',I10/,
     & 5X,'CONSTANT STRESS FLAG. . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS NUMERICAL DAMPING . . . . . .=',1PG20.13/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POSITION INPUT FLAG . . . . . . . . . .=',I10/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/)
 2100 FORMAT(//,
     & 5X,'COMPOSITE LAYERED THICK SHELL PROPERTY SET'/,
     & 5X,'WITH HETEROGENIOUS PROPERTY IN THICKNESS'/,
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
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13//,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POSITION INPUT FLAG . . . . . . . . . .=',I10/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/)
 2101 FORMAT(//,
     & 5X,'COMPOSITE LAYERED THICK SHELL PROPERTY SET'/,
     & 5X,'WITH HETEROGENIOUS PROPERTY IN THICKNESS'/,
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
     & 5X,'BRICK MINIMUM TIME STEP................=',1PG20.13/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POSITION INPUT FLAG . . . . . . . . . .=',I10/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'LOCAL ORTHOTROPY SYSTEM FORMULATION . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/)
 3000 FORMAT(
     & 5X,'LAYER :',I3/,
     & 5X,'      ANGLE (DIR 1,PROJ(DIR 1 /TSHELL) .=',1PG20.13/,
     & 5X,'      THICKNESS (PER TOTAL THICKNESS). .=',1PG20.13/,
     & 5X,'      POSITION  ([-0.5,+0.5]). . . . . .=',1PG20.13/,
     & 5X,'      MATERIAL NUMBER . . . . . . . . . =',I10/)
C

      END SUBROUTINE HM_READ_PROP22
