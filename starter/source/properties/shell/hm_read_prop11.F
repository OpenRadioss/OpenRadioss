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
!||    hm_read_prop11             ../starter/source/properties/shell/hm_read_prop11.F
!||--- called by ------------------------------------------------------
!||    hm_read_properties         ../starter/source/properties/hm_read_properties.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    fretitl2                   ../starter/source/starter/freform.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    subrotvect                 ../starter/source/model/submodel/subrot.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod               ../starter/source/modules/defaults_mod.F90
!||    elbuftag_mod               ../starter/share/modules1/elbuftag_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP11(GEO       ,IGEO     ,PM       ,IPM      ,ISKN     ,
     .                   UNITAB    ,RTRANS   ,LSUBMODEL,SUB_ID   ,IDTITL   ,
     .                   PROP_TAG  ,IG       ,IGTYP  ,DEFAULTS_SHELL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD  
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI),IPM(NPROPMI,*),SUB_ID,IG,IGTYP
      my_real
     .   GEO(NPROPG), PM(NPROPM,*),RTRANS(NTRANSF,*)
      my_real
     .   FAC_M, FAC_L, FAC_T
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      CHARACTER(LEN=NCHARTITLE)::IDTITL
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SHELL_DEFAULTS_), INTENT(IN)  :: DEFAULTS_SHELL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER LAW_ID*4
      INTEGER NUML(100),NLY,NLYMAX,N1,N,J,IERREL,M1,M2,M3,M4,
     .   K, N2, N3, ISTRAIN, MLAWLY1,ISHEAR,IPLAST,IHBE,ITHK,IREP,
     .   IDSK, ISK, IPANG, IPTHK, IPPOS, IPOS, IPMAT, IPWEIGHT,ISKN(LISKN,*),
     .   ISS, PID1,ISMSTR,ISROT,IBID,IGMAT,IHBEOUTP,IHBE_OLD,IRP
      my_real AN, VX, VY, VZ, THICKT, DT, TMIN, TMAX,WEIGHT,PTHK,CVIS,PUN,
     .        PHI_I,THK_I,Z_I,WF_I,ZSHIFT
      INTEGER ISH3N
      INTEGER ID,ID1,IUN,IPPLY,NLY_TMP,L,M
      CHARACTER(LEN=NCHARTITLE) :: TITR,TITR1
      INTEGER     IHBE_D,IPLA_D,ISTR_D,ITHK_D,ISHEA_D,ISST_D,
     .           ISH3N_D, ISTRA_D,NPTS_D,IDRIL_D
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------

C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA IUN/1/
      DATA PUN/0.1/
C----------------------------------------------------------------
C      COMPOSITE LAYERED SHELL
C      LAYERS WITH : -VARIABLE THICKNESS
C                    -VARIABLE MATERIAL  (BUT LAW 25 OR 27 ONLY)
C----------------------------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      IHBE_D =  DEFAULTS_SHELL%ishell
      ISH3N_D=  DEFAULTS_SHELL%ish3n
      ISST_D =  DEFAULTS_SHELL%ismstr
      IPLA_D =  DEFAULTS_SHELL%iplas
      ITHK_D =  DEFAULTS_SHELL%ithick
      IDRIL_D=  DEFAULTS_SHELL%idrill
      ISHEA_D = 0
      NPTS_D = 0
      ISTRA_D = 1
!
      IHBE=0
      ISMSTR=0
      ISROT=0
      PTHK = ZERO
      IRP = 0
C fixing flag and removed from input
        ISTRAIN=1
c
      NLYMAX= 100
      IPANG = 200
      IPTHK = 300
      IPPOS = 400
      IPMAT = 100
      IPWEIGHT = 900
      IDSK = 0
      IREP = 0
      IPOS = 0
C----------------------
C HIDDEN FLAGS
C----------------------
C ISHEAR NEVER USED
        ISHEAR = 0
C Cvis used in shell routines
        CVIS = ZERO
C IGMAT IS USED GENERALLY > 0 in other routines  => put always to 1
        IGMAT = 1
        IGEO(98)= IGMAT

C     Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN

C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C PARSE ALL THE PLIES TO COMPUTE NEW PLY NUMBER ACCORDINGLY TO IPPLY
C--------------------------------------------------
      CALL HM_GET_INTV('NIP',NLY,IS_AVAILABLE,LSUBMODEL)
      NLY_TMP = 0
      DO K=1,NLY
          CALL HM_GET_INT_ARRAY_INDEX('Ipply',IPPLY,K,IS_AVAILABLE,LSUBMODEL)
          IF(IPPLY == 0) IPPLY = 1
          NLY_TMP = NLY_TMP + IPPLY
      ENDDO

C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Ishell',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISH3N',ISH3N,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idrill',ISROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NIP',NLY,IS_AVAILABLE,LSUBMODEL)
C      CALL HM_GET_INTV('ISTRAIN',ISTRAIN,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ITHICK',ITHK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IPLAS',IPLAST,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SKEW_CSID',IDSK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iorth',IREP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ipos',IPOS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ip',IRP,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('P_Thick_Fail',PTHK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hm',GEO(13),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hf',GEO(14),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hr',GEO(15),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dm',GEO(16),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dn',GEO(17),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('THICK',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('AREA_SHEAR',GEO(38),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('V_X',VX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('V_Y',VY,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('V_Z',VZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------

      IF (PTHK == ZERO) PTHK = ONE-EM06
      PTHK = MIN(PTHK, ONE)
      PTHK = MAX(PTHK,-ONE) 
      GEO(42) = PTHK
C
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
        IF (CVIS==ZERO) CVIS=ONE
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
      IF(ISROT==0)ISROT=IDRIL_D
      IF(ISROT==2) ISROT = 0
      IGEO(20)=ISROT
C-------to have DR----            
      IF (ISMSTR==10.AND.ISROT>0.AND.IDROT==0) IDROT = 1
c
      IF(ISMSTR==0) ISMSTR=2
      IF(ISMSTR==3.AND.IHBE/=0.AND.IHBE/=2) THEN
        ISMSTR = 2
        CALL ANCMSG(MSGID=319,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
      GEO(3)   = ISMSTR
      IGEO(5)  = ISMSTR
C
C     Temporary double storage
      IGEO(10)=IHBE
      GEO(171)=IHBE
C
      IF(IHBE==3)THEN
        IF(GEO(13)==ZERO)GEO(13)=EM01
        IF(GEO(14)==ZERO)GEO(14)=EM01
        IF(GEO(15)==ZERO)GEO(15)=EM02
      ELSEIF(IHBE<10)THEN
        IF(GEO(13)==ZERO)GEO(13)=EM02
        IF(GEO(14)==ZERO)GEO(14)=EM02
        IF(GEO(15)==ZERO)GEO(15)=EM02
      ENDIF
      IF (IHBE>11.AND.IHBE<29) THEN
        GEO(13)=GEO(17)
        GEO(17)=CVIS
      ENDIF

C
      IF(GEO(38) == ZERO) GEO(38)=FIVE_OVER_6
      IF(NLY == -1)NLY=NPTS_D
      IF(NLY == 0) NLY = 1
      IF(NLY == 1) GEO(38)= ZERO
      AN=SQRT(VX*VX+VY*VY+VZ*VZ)
C---- error out w/ IRP=22,23      
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

      IF (SUB_ID /= 0)
     .         CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)


      ISK = 0
      IF (IDSK/=0) THEN
        DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
          IF(IDSK == ISKN(4,J+1)) THEN
            ISK=J+1
            GO TO 10
          ENDIF
        END DO
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
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
      IF ((IRP==22.OR.IRP==25).AND.ISK==0) THEN
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
      GEO(7)=VX
      GEO(8)=VY
      GEO(9)=VZ
      IF(ITHK == 0)ITHK=ITHK_D
      IF(ITHK_D==-2)ITHK=-1
      IF(ISHEAR == 0)ISHEAR=ISHEA_D
      IF(IPLAST == 0)IPLAST=IPLA_D
      IF(IPLA_D==-2) IPLAST=-1
C      IF(ISTRAIN == 0)ISTRAIN=ISTR_D
      GEO(11)=ISTRAIN
      IHBE=IGEO(10)
      IGEO(6)=IREP
      GEO(35)=ITHK
      GEO(37)=ISHEAR
      GEO(39)=IPLAST
      ISS=IGEO(5)
      IG=IGEO(1)
C-----using IGEO(14)=ICSTR      
      IGEO(14)  = IRP
C-----------------------------------------------------------------
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,1000)IG
 1000    FORMAT(
     &    5X,'COMPOSITE LAYERED SHELL PROPERTY SET'/,
     &    5X,'------------------------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
      ELSE
       IF(ISK == 0)THEN
        IF (IHBE>11.AND.IHBE<29) THEN
          WRITE(IOUT,2112)IG,NLY,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,IGEO(20),
     .               GEO(16),GEO(13),GEO(38),GEO(42),ISHEAR,ITHK,
     .               IPLAST,IREP,GEO(7),GEO(8),GEO(9),IGEO(14)
         ELSE
          WRITE(IOUT,2110)IG,NLY,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,
     .               GEO(13),GEO(14),GEO(15),GEO(16),
     .               GEO(38),GEO(42),ISHEAR,ITHK,IPLAST,IREP,
     .               GEO(7),GEO(8),GEO(9),IGEO(14)
         ENDIF
       ELSE
        IF (IHBE>11.AND.IHBE<29) THEN
          WRITE(IOUT,2113)IG,NLY,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,IGEO(20),
     .               GEO(16),GEO(13),GEO(38),GEO(42),ISHEAR,ITHK,
     .               IPLAST,IREP,IDSK,IGEO(14)
         ELSE
           WRITE(IOUT,2111)IG,NLY,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,
     .               GEO(13),GEO(14),GEO(15),GEO(16),GEO(38),
     .               GEO(42),ISHEAR,ITHK,IPLAST,IREP,IDSK,IGEO(14)
        ENDIF
       ENDIF
      ENDIF
C---
      IF (NLY == 0) THEN         
         CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
         CALL ANCMSG(MSGID=27,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (NLY>NLYMAX) THEN
         CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
         CALL ANCMSG(MSGID=28,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I2=NLYMAX,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (NLY_TMP>NLYMAX) THEN
         CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
         CALL ANCMSG(MSGID=28,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I2=NLYMAX,
     .               I1=IG,
     .               C1=TITR)
      ENDIF

      N1    = MIN0(NLY,NLYMAX)
      GEO(6)= MAX(N1,NLY_TMP)+EM01  
      IGEO(4) = MAX(N1,NLY_TMP)
C--------------------------------------------------------------------
C    READING OF ANGLES, THICKNESSES, LAYER POSITIONS AND MATERIALS
C--------------------------------------------------------------------
C----------------------
C READ LINE 5 -> 5+N1
C----------------------
        M = 0
        DO K=1,N1
            CALL HM_GET_INT_ARRAY_INDEX('Ipply',IPPLY,K,IS_AVAILABLE,LSUBMODEL)

            CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_phi',PHI_I,K,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Thick',THK_I,K,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Zi',Z_I,K,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_WFi',WEIGHT,K,IS_AVAILABLE,LSUBMODEL,UNITAB)
            IF (IPPLY == 0) IPPLY = 1
C----------------------
           DO L=1,IPPLY
             M = M + 1
             CALL HM_GET_INT_ARRAY_INDEX('Prop_mi',NUML(M),K,IS_AVAILABLE,LSUBMODEL)
             GEO(IPANG+M) = PHI_I
             GEO(IPTHK+M) = THK_I/IPPLY 
             GEO(IPPOS+M) = Z_I 
             IF (WEIGHT == ZERO) WEIGHT = ONE
             GEO(IPWEIGHT+M)= WEIGHT
           ENDDO
        ENDDO
C--- Thickness & position of the layers
      IGEO(99) = IPOS
      ZSHIFT = ZERO
      IF (IPOS > 0)THEN
        TMIN = EP20
        TMAX = -EP20

        DO N=1,NLY_TMP
          DT = HALF*GEO(IPTHK+N)
          TMIN = MIN(TMIN,GEO(IPPOS+N)-DT)
          TMAX = MAX(TMAX,GEO(IPPOS+N)+DT)
        ENDDO
        THICKT = TMAX - TMIN
        ZSHIFT = HALF*(TMAX + TMIN)
        DT = ZERO
        IF (DEFAULTS_SHELL%IOFFSET==3) DT = ZSHIFT
        GEO(200)=THICKT
        DO N=1,NLY_TMP
          GEO(IPTHK+N)=GEO(IPTHK+N) / MAX(THICKT,EM20)
          GEO(IPPOS+N)=(GEO(IPPOS+N)-DT) / MAX(THICKT,EM20)
        ENDDO
      ELSE
        THICKT = ZERO
        DO N=1,NLY_TMP
          THICKT=THICKT+GEO(IPTHK+N)
        ENDDO
        DO N=1,NLY_TMP
          GEO(IPTHK+N)=GEO(IPTHK+N) / MAX(THICKT,EM20)
        ENDDO
C--- automatic calculation of layer positions
        GEO(IPPOS+1) = -HALF + HALF*GEO(IPTHK+1)
        DO N=2,NLY_TMP
          GEO(IPPOS+N) = GEO(IPPOS+N-1)
     .                 + HALF*(GEO(IPTHK+N)+GEO(IPTHK+N-1))
        ENDDO
        GEO(200)=THICKT
      ENDIF
      GEO(199) = ZSHIFT
C---  Test validite epaisseur
      IERREL=NINT(ABS((THICKT-GEO(1))/GEO(1))*EP02)
      IF(IERREL>10) THEN
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
            IF(MLAWLY1 == 25) GO TO 250
            IF(MLAWLY1 == 27) GO TO 250
            IF(MLAWLY1 == 15) GO TO 250
            IF(MLAWLY1>=29) GO TO 250
            CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
            CALL FRETITL2(TITR1,IPM(NPROPMI-LTITR+1,J),LTITR)
            CALL ANCMSG(MSGID=30,
     .                  ANMODE=ANINFO,
     .                  MSGTYPE=MSGERROR,
     .                  I1=IG,
     .                  C1=TITR,
     .                  I2=NUML(1),
     .                  C2=TITR1,
     .                  I3=MLAWLY1)
            GO TO 250
          ENDIF
        ENDDO
        CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1),LTITR)
        CALL ANCMSG(MSGID=31,
     .              ANMODE=ANINFO,
     .              MSGTYPE=MSGERROR,
     .              I1=IG,
     .              C1=TITR1,
     .              I2=NUML(1)) 
        IGEO(IPMAT+1) = 1
  250 CONTINUE
C-----OTHER LAYERS
      DO 350 N=2,NLY_TMP
        DO J=1,NUMMAT
           IF(IPM(1,J) == NUML(N)) THEN
             IGEO(IPMAT+N) = J
             IF(NINT(PM(19,J)) == MLAWLY1) GO TO 350
             WRITE(LAW_ID,'(I2)')MLAWLY1
             IF (MLAWLY1==99) LAW_ID='USER'
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
     .             I2=NUML(N)) 
       IGEO(IPMAT+N) = 1
  350 CONTINUE
C
      DO N=1,NLY_TMP
        M1=IPANG+N
        M2=IPTHK+N
        M3=IPPOS+N
        M4=IPWEIGHT+N
        IF(.NOT. IS_ENCRYPTED)THEN
          WRITE(IOUT,2121)N,GEO(M1),GEO(M2)*GEO(1),GEO(M3)*GEO(1),
     .     GEO(M4),NUML(N)
        ENDIF
        GEO(M1)=GEO(M1)*PI/HUNDRED80
      ENDDO
C-----------------------------
C     IHBE IPLA ISTRAIN - shells
C-----------------------------
      IHBE=NINT(GEO(171))
      IF(IHBE==0)THEN
        GEO(171)=0
      ELSEIF(IHBE==1)THEN
        GEO(171)=1
      ELSEIF(IHBE==2)THEN
        GEO(171)=0
      ELSEIF(IHBE>=3.AND.IHBE<100.AND.IHBE/=4)THEN
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

c----   Shells
          PROP_TAG(IGTYP)%G_SIG = 0
          PROP_TAG(IGTYP)%G_FOR = 5
          PROP_TAG(IGTYP)%G_MOM = 3
          PROP_TAG(IGTYP)%G_THK = 1
          PROP_TAG(IGTYP)%G_EINT= 2
          PROP_TAG(IGTYP)%G_EINS= 0
          PROP_TAG(IGTYP)%L_SIG = 5
          IF (IGTYP == 17) PROP_TAG(IGTYP)%L_SIGPLY = 3
          PROP_TAG(IGTYP)%L_THK = 0
          PROP_TAG(IGTYP)%L_EINT= 2
          PROP_TAG(IGTYP)%L_EINS= 0
          PROP_TAG(IGTYP)%G_VOL = 0
          PROP_TAG(IGTYP)%L_VOL = 0
          PROP_TAG(IGTYP)%LY_DMG = 2

          PROP_TAG(IGTYP)%LY_GAMA = 6
          PROP_TAG(IGTYP)%LY_DIRA = 2

          PROP_TAG(IGTYP)%LY_PLAPT = 1      
          PROP_TAG(IGTYP)%LY_SIGPT = 5
          PROP_TAG(IGTYP)%G_FORPG  = 5
          PROP_TAG(IGTYP)%G_MOMPG  = 3
          PROP_TAG(IGTYP)%G_STRPG  = 8
          IF(IGMAT > 1) 
     .                                      PROP_TAG(IGTYP)%LY_DMG = 2


        IGEO(1) =IG
        IGEO(11)=IGTYP
        IF(GEO( 3)/=ZERO.AND.IGEO( 5)== 0)IGEO( 5)=NINT(GEO( 3))
        IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0)IGEO( 9)=NINT(GEO(39))
        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)
     .     IGEO(10)=NINT(GEO(171))

C----
      RETURN
 2110 FORMAT(
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
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS      ',/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2111 FORMAT(
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
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS      ',/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2112 FORMAT(
     & 5X,'COMPOSITE LAYERED SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS      ',/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2113 FORMAT(
     & 5X,'COMPOSITE LAYERED SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS      ',/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2121 FORMAT(
     & 5X,'LAYER ',I3/,
     & 5X,'      ANGLE (DIR 1,PROJ(DIR 1 / SHELL).=',1PG20.13/,
     & 5X,'      THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'      POSITION. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'      LAYER FAILURE WEIGHT FACTOR . . .=',1PG20.13/,
     & 5X,'      MATERIAL NUMBER . . . . . . . . .=',I10/)
      END
