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
Chd|  HM_READ_PROP16                source/properties/shell/hm_read_prop16.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP16(GEO      ,IGEO     ,PM       ,IPM      ,ISKN     ,
     .                          UNITAB   ,RTRANS   ,LSUBMODEL,SUB_ID   ,PROP_TAG ,
     .                          IGTYP    ,IG       ,IDTITL   )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI),IPM(NPROPMI,*),SUB_ID,IGTYP,IG
C     REAL ou REAL*8
      my_real
     .   GEO(NPROPG), PM(NPROPM,*)
      my_real
     .   RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      CHARACTER IDTITL*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER LAW_ID*4
      CHARACTER ISS*3
      INTEGER NUML(100), NLY, NLYMAX, N, J, IERREL, M1, M2, M3,
     .   M4, K, N2, N3, ISTRAIN, MLAWLY1,ISHEAR,IHBE,ITHK,IREP,
     .   IDSK, ISK, IPANG, IPPHI, IPTHK, IPPOS, IPOS, IPMAT, PID1,
     .   ISKN(LISKN,*),ISMSTR,ISROT,IHBEOUTP,ISORTH,IHBE_OLD
C     REAL ou REAL*8
      my_real
     .    AN, VX, VY, VZ, THICKT, DT, TMIN, TMAX, ASHEAR,PTHK,CVIS,PUN
      INTEGER ISH3N,IRP
      INTEGER ID,IUN
      CHARACTER*nchartitle,
     .   TITR,TITR1
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA IUN/1/
      DATA PUN/0.1/
C======================================================================|
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      NLYMAX= 100
      IPMAT = 100
      IPANG = 200
      IPTHK = 300
      IPPOS = 400
      IPPHI = 500
      IDSK = 0
      IPOS = 0
      IREP = 2
      IRP = 0
C----------------------
C HIDDEN FLAGS
C----------------------
C ISHEAR NEVER USED
      ISHEAR = 0
C CVIS USED in coquez routines
C
      ISROT = 0  
C
C----------------------
C     Double stockage (to be cleaned one day ...)
      IGEO(1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISHELL',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISH3N',ISH3N,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_INTV('NIP',NLY,IS_AVAILABLE,LSUBMODEL)
C      CALL HM_GET_INTV('ISTRAIN',ISTRAIN,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ITHICK',ITHK,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_INTV('SKEW_CSID',IDSK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ipos',IPOS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ip',IRP,IS_AVAILABLE,LSUBMODEL)
c
      DO K=1,NLY 
        CALL HM_GET_INT_ARRAY_INDEX('Prop_mi',NUML(K),K,IS_AVAILABLE,LSUBMODEL) 
      ENDDO
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('P_Thick_Fail',PTHK,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('Hm',GEO(13),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hf',GEO(14),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hr',GEO(15),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dm',GEO(16),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dn',GEO(17),IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_FLOATV('THICK',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('AREA_SHEAR',ASHEAR,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_FLOATV('VECTOR_X',VX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('VECTOR_Y',VY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('VECTOR_Z',VZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      DO K=1,NLY    
        CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_phi',GEO(IPANG+K),K,IS_AVAILABLE,LSUBMODEL,UNITAB) 
        CALL HM_GET_FLOAT_ARRAY_INDEX('Alpha_arr',GEO(IPPHI+K),K,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Thick',GEO(IPTHK+K),K,IS_AVAILABLE,LSUBMODEL,UNITAB) 
        CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Zi',GEO(IPPOS+K),K,IS_AVAILABLE,LSUBMODEL,UNITAB) 
        IF (GEO(IPPHI+K) == ZERO) GEO(IPPHI+K) = NINTY         
      ENDDO  
C--------------------------------------------------
C SUBMODEL TRANSFORMATION
C--------------------------------------------------
      IF (SUB_ID .NE. 0)
     .       CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)   
C----------------------
      PTHK = MIN(PTHK, ONE-EM06)
      GEO(42) = PTHK

C fixing flag and removed from input
        ISTRAIN=1
C----------------------
      IF(IHBE==0)IHBE=IHBE_D
      IHBEOUTP=IHBE
      IF (IHBE == 4 .AND. ISH3N==0 .AND. ISH3N_D == 1) THEN
        CALL ANCMSG(MSGID=680,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IHBE==22.OR.IHBE==23) THEN
        CALL ANCMSG(MSGID=539,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
        IHBE=24
      ENDIF         
      IF(ISH3N==0) ISH3N = ISH3N_D
      IGEO(18) = ISH3N
      IF (GEO(16) == ZERO) IGEO(31) = 1
      
      IF (IHBE==24) THEN
        IF (GEO(17)==ZERO) GEO(17)=ZEP015
      ENDIF
C---
      IF(ISMSTR==0)ISMSTR=ISST_D
      IF (ISST_D == -2) ISMSTR = -1      
      IF(IHBE==3)THEN
        IF(GEO(13)==ZERO)GEO(13)=EM01
        IF(GEO(14)==ZERO)GEO(14)=EM01
        IF(GEO(15)==ZERO)GEO(15)=EM02
      ELSE
        IF(GEO(13)==ZERO)GEO(13)=EM02
        IF(GEO(14)==ZERO)GEO(14)=EM02
        IF(GEO(15)==ZERO)GEO(15)=EM02
      ENDIF

      IF (IHBE==24) THEN
        GEO(13)=GEO(17)
        GEO(17)=ONE
      ENDIF
C     Double stockage (to be cleaned one day ...)
c
      IGEO(10)=IHBE
      GEO(171)=IHBE
C----------Idrill is not in manual /PROP/TYPE16
c      IF(ISROT==0)ISROT=IDRIL_D
c      IF(ISROT==2) ISROT = 0
      IGEO(20)=ISROT

      IF (ISMSTR == 0) ISMSTR=4
      IF (ISMSTR==3 .AND. IHBE/=0 .AND. IHBE/=2) THEN
        ISMSTR = 4
        CALL ANCMSG(MSGID=319,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
      IGEO(5)= ISMSTR
      GEO(3) = ISMSTR
C-----
      IF (ASHEAR == ZERO) ASHEAR=FIVE_OVER_6
      IF (NLY == -1) NLY = NPTS_D
      IF (NLY == 0)  NLY = 1
      IF (NLY == 1)  ASHEAR = ZERO
      AN = SQRT(VX*VX+VY*VY+VZ*VZ)
      IF(AN < EM10)THEN
        VX=ONE
        VY=ZERO
        VZ=ZERO
        IF (IRP==23) THEN
          CALL ANCMSG(MSGID=1922,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=IG,
     .              C2='PROPERTY',
     .              C3=TITR,
     .              I2=IRP)
        END IF
      ELSE
        VX=VX/AN
        VY=VY/AN
        VZ=VZ/AN
      ENDIF
      ISK = 0
      IF (IDSK /= 0) THEN
        DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
          IF(IDSK == ISKN(4,J+1)) THEN
            ISK=J+1
            GO TO 10
          ENDIF
        END DO
        CALL ANCMSG(MSGID=184,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=ID,
     .              C2='PROPERTY',
     .              C3=TITR,
     .              I2=IDSK)
 10     CONTINUE
      ENDIF
      IF (IRP==22.AND.ISK==0) THEN
        CALL ANCMSG(MSGID=1923,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=IG,
     .              C2='PROPERTY',
     .              C3=TITR,
     .              I2=IRP)
      END IF
      IGEO(2)=ISK
      IGEO(6)=IREP
      GEO(7) =VX
      GEO(8) =VY
      GEO(9) =VZ
      IF(ITHK   == 0)ITHK=ITHK_D
      IF(ITHK_D==-2)ITHK=-1
      IF(ISHEAR == 0)ISHEAR=ISHEA_D
C      IF(ISTRAIN== 0)ISTRAIN=ISTR_D
      GEO(11)=ISTRAIN
      IHBE = NINT(GEO(171))
      GEO(35)=ITHK
      GEO(37)=ISHEAR
      GEO(38)=ASHEAR
      IGEO(14)  = IRP
C-----------------------------------------------------------------
       IF(IS_CRYPTED)THEN
         WRITE(IOUT,1000)IG
 1000    FORMAT(
     &    5X,'COMPOSITE FABRIC LAYERED SHELL PROPERTY SET '/,
     &    5X,'------------------------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
       ELSE
         IF(ISK == 0)THEN
           IF (IHBE > 11 .AND. IHBE < 29) THEN
             WRITE(IOUT,2010)IG,NLY,ISTRAIN,GEO(1),IGEO(5),IHBE,
     .                 ISH3N,
     .                 GEO(16),GEO(13),GEO(38),GEO(42),ISHEAR,ITHK,
     .                 GEO(7),GEO(8),GEO(9),IGEO(14)
           ELSE
             WRITE(IOUT,1010)IG,NLY,ISTRAIN,GEO(1),IGEO(5),IHBE,
     .                 ISH3N,
     .                 GEO(13),GEO(14),GEO(15),GEO(16),
     .                 ASHEAR,GEO(42),ISHEAR,ITHK,
     .                 GEO(7),GEO(8),GEO(9),IGEO(14)
           ENDIF
         ELSE
           IF (IHBE > 11 .AND. IHBE < 29) THEN
             WRITE(IOUT,2011)IG,NLY,ISTRAIN,GEO(1),IGEO(5),IHBE,
     .                 ISH3N,GEO(16),
     .                 GEO(13),ASHEAR,GEO(42),ISHEAR,ITHK,IDSK,IGEO(14)
           ELSE
              WRITE(IOUT,1011)IG,NLY,ISTRAIN,GEO(1),IGEO(5),IHBE,
     .                 ISH3N,
     .                 GEO(13),GEO(14),GEO(15),GEO(16),
     .                 ASHEAR,GEO(42),ISHEAR,ITHK,IDSK,IGEO(14)
           ENDIF
         ENDIF
      ENDIF
C---
      IF (NLY > NLYMAX) THEN
         CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
         CALL ANCMSG(MSGID=28,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=NLYMAX)
      ENDIF
      NLY = MIN(NLY,NLYMAX)
      IGEO(4) = NLY                                                          
C---  calcul epaIGEO(5)eur & position des couches
      IF (IPOS > 0)THEN
        TMIN = EP20
        TMAX =-EP20

        DO N=1,NLY
          DT = HALF*GEO(IPTHK+N)
          TMIN = MIN(TMIN,GEO(IPPOS+N)-DT)
          TMAX = MAX(TMAX,GEO(IPPOS+N)+DT)
        ENDDO
        THICKT = TMAX - TMIN
        GEO(200)=THICKT
        DO N=1,NLY
          GEO(IPTHK+N)=GEO(IPTHK+N) / MAX(THICKT,EM20)
          GEO(IPPOS+N)=GEO(IPPOS+N) / MAX(THICKT,EM20)
        ENDDO
      ELSE
        THICKT = ZERO
        DO N=1,NLY
          THICKT=THICKT+GEO(IPTHK+N)
        ENDDO
        DO N=1,NLY
          GEO(IPTHK+N)=GEO(IPTHK+N) / MAX(THICKT,EM20)
        ENDDO
C---      calcul automatique de position des couches
        GEO(IPPOS+1) = -HALF + HALF*GEO(IPTHK+1)
        DO N=2,NLY
          GEO(IPPOS+N) = GEO(IPPOS+N-1)
     .                 + HALF*(GEO(IPTHK+N)+GEO(IPTHK+N-1))
        ENDDO
        GEO(200)=THICKT
      ENDIF
C---  Test validite epaIGEO(5)eur
      IERREL=NINT(ABS((THICKT-GEO(1))/GEO(1))*EP02)
      IF(IERREL > TEN) THEN
        PID1=IGEO(1)
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
        CALL ANCMSG(MSGID=29,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I2=IERREL,
     .              I1=PID1,
     .              C1=TITR)
      ENDIF
C--------------------------------------------------------------
C      MATERIAUX: TRANSFORMATION EN NOS INTERNES + TESTS DE VALIDITE
C-------LAYER 1
      DO J=1,NUMMAT
        IF (IPM(1,J) == NUML(1)) THEN
          IGEO(IPMAT+1) = J
          MLAWLY1= NINT(PM(19,J))
          IF(MLAWLY1 == 58.OR.MLAWLY1 == 98) GO TO 250
          IF(MLAWLY1.GE.29.AND.MLAWLY1.LE.31) GO TO 250
          IF(MLAWLY1.EQ.99) GO TO 250
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
          CALL FRETITL2(TITR1,IPM(NPROPMI-LTITR+1,J),LTITR)
          CALL ANCMSG(MSGID=1201,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=IG,
     .                C1=TITR,
     .                I2=NUML(1),
     .                C2=TITR1,
     .                I3=MLAWLY1)
          GO TO 250
        ENDIF
      ENDDO
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
      CALL ANCMSG(MSGID=31,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            I1=IG,
     .            C1=TITR,
     .            I2=NUML(1))         
      IGEO(IPMAT+1) = 1
  250 CONTINUE
C-----OTHER LAYERS
      DO 350 N=2,NLY
        DO J=1,NUMMAT
           IF(IPM(1,J) == NUML(N)) THEN
             IGEO(IPMAT+N) = J
             IF(NINT(PM(19,J)) == MLAWLY1) GO TO 350
             WRITE(LAW_ID,'(I2)')MLAWLY1
             IF (MLAWLY1.EQ.99) LAW_ID='USER'
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
             CALL FRETITL2(TITR1,IPM(NPROPMI-LTITR+1,J),LTITR)
             CALL ANCMSG(MSGID=334,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IG,
     .                   C1=TITR,
     .                   I2=NUML(N),
     .                   C2=TITR1,
     .                   C3=LAW_ID)
             GO TO 350
           ENDIF
       ENDDO
       CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
       CALL ANCMSG(MSGID=31,
     .             ANMODE=ANINFO,
     .             MSGTYPE=MSGERROR,
     .             I1=IG,
     .             C1=TITR,
     .             I2=NUML(1))         
       IGEO(IPMAT+N) = 1
  350 CONTINUE
C
      DO N=1,NLY
        M1=IPANG+N
        M2=IPTHK+N
        M3=IPPOS+N
        M4=IPPHI+N
        IF(.NOT. IS_CRYPTED)WRITE(IOUT,3000) N,GEO(M1),GEO(M4),
     .                   GEO(M2)*GEO(1),GEO(M3)*GEO(1),NUML(N)
        GEO(M1)=GEO(M1)*PI/HUNDRED80
        GEO(M4)=GEO(M4)*PI/HUNDRED80
      ENDDO
C-------------------------------
C FROM LECGEO
C-------------------------------
      IF (ISMSTR==3 .AND. IHBE/=0 .AND. IHBE/=2) THEN
        ISMSTR = 4
        CALL ANCMSG(MSGID=319,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
      IF(IHBE == 3)THEN
        IF(GEO(13)==ZERO) GEO(13)=EM01
        IF(GEO(14)==ZERO) GEO(14)=EM01
        IF(GEO(15)==ZERO) GEO(15)=EM02
      ELSEIF(IHBE < 10)THEN
        IF(GEO(13)==ZERO) GEO(13)=EM02
        IF(GEO(14)==ZERO) GEO(14)=EM02
        IF(GEO(15)==ZERO) GEO(15)=EM02
      ENDIF
      GEO(3)=ISMSTR
      IGEO(5)=ISMSTR
C-----------------------------
C       IHBE IPLA ISTRAIN
C-----------------------------
      IHBE=NINT(GEO(171))
      IF(IHBE==0)THEN
        GEO(171)=0
      ELSEIF(IHBE==1)THEN
        GEO(171)=1
      ELSEIF(IHBE==2)THEN
        GEO(171)=0
      ELSEIF(IHBE.GE.3.AND.IHBE.LT.100.AND.IHBE.NE.4)THEN
        GEO(171)=IHBE-1
      ENDIF
      ISHEAR = GEO(37)
      IF(ISHEAR==0)THEN
        GEO(37)=0
      ELSEIF(ISHEAR==1)THEN
        GEO(37)=1
      ELSEIF(ISHEAR==2)THEN
        GEO(37)=0
      ENDIF
C-------------------------------
C  Double stockage temporaire : GEO() / IGEO() : a supprimer a terme
C-------------------------------
      IGEO(1) =IG
      IGEO(11)=IGTYP
      ISORTH = 0
      IGEO(17)=ISORTH
      IF(GEO( 3)/=ZERO.AND.IGEO( 5)== 0)IGEO( 5)=NINT(GEO( 3))
      IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0)IGEO( 9)=NINT(GEO(39))
      IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)
     .   IGEO(10)=NINT(GEO(171))
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
      PROP_TAG(IGTYP)%G_SIG = 0
      PROP_TAG(IGTYP)%G_FOR = 5
      PROP_TAG(IGTYP)%G_MOM = 3
      PROP_TAG(IGTYP)%G_THK = 1
      PROP_TAG(IGTYP)%G_EINT= 2
      PROP_TAG(IGTYP)%G_EINS= 0
      PROP_TAG(IGTYP)%L_SIG = 5
      PROP_TAG(IGTYP)%L_THK = 0
      PROP_TAG(IGTYP)%L_EINT= 2
      PROP_TAG(IGTYP)%L_EINS= 0
      PROP_TAG(IGTYP)%G_VOL = 0
      PROP_TAG(IGTYP)%L_VOL = 0
      PROP_TAG(IGTYP)%LY_DMG = 2
C
      PROP_TAG(IGTYP)%LY_GAMA = 6
      PROP_TAG(IGTYP)%LY_DIRA = 2
      PROP_TAG(IGTYP)%LY_DIRB = 2
      PROP_TAG(IGTYP)%LY_PLAPT = 1      
      PROP_TAG(IGTYP)%LY_SIGPT = 5
      PROP_TAG(IGTYP)%G_FORPG  = 5
      PROP_TAG(IGTYP)%G_MOMPG  = 3
      PROP_TAG(IGTYP)%G_STRPG  = 8
C-----------------------------
C----
      RETURN
 999  CALL FREERR(3)
      RETURN
C---
 1010 FORMAT(
     & 5X,'COMPOSITE LAYERED SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'% OF LAYER THICKNESS TO FAIL. . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 1011 FORMAT(
     & 5X,'COMPOSITE LAYERED SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'% OF LAYER THICKNESS TO FAIL. . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2010 FORMAT(
     & 5X,'COMPOSITE LAYERED SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'% OF LAYER THICKNESS TO FAIL. . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2011 FORMAT(
     & 5X,'COMPOSITE LAYERED SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'% OF LAYER THICKNESS TO FAIL. . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 3000 FORMAT(
     & 5X,'LAYER ',I3/,
     & 5X,'      ANGLE (DIR 1,PROJ(VECT / SHELL) .=',1PG20.13/,
     & 5X,'      ANGLE (DIR 1,DIR 2) . . . . . . .=',1PG20.13/,
     & 5X,'      THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'      POSITION. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'      MATERIAL NUMBER . . . . . . . . .=',I10/)
      END SUBROUTINE HM_READ_PROP16
