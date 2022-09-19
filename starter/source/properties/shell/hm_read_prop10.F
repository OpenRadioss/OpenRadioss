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
Chd|  HM_READ_PROP10                source/properties/shell/hm_read_prop10.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP10(IGTYP , GEO   ,  IGEO   ,PROP_TAG  ,UNITAB ,
     .                   RTRANS,LSUBMODEL,IDTITL ,IG     ,SUB_ID   ,ISKN  )
C============================================================================
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
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tablen_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER
     .        IGTYP , IGEO(*),ISKN(LISKN,*), IG
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real GEO(*),RTRANS(NTRANSF,*)

      CHARACTER IDTITL*nchartitle

      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISMSTR, NIP, J,
     .   ISHEAR, IP, ISTRAIN,IHBE,IPLAST,ITHK,IBID,IDF,IHBEOUTP,K,N,
     .   IUNIT,ISEN,ISS,PID1,IPID1, IHBE_OLD,NSTACK,IGMAT,NN,NUMS
      INTEGER ISH3N,ISROT,SUB_ID,NLY,IRP,IDSK,ISK,IUN
      INTEGER NLYMAX,N1,M1,N2, N3, IPANG,IPTHK,IPPOS,IPWEIGHT,IPOS

      my_real
     .    ANGL,PUN,CVIS,RBID,VX,VY,VZ,
     .    PTHK, AN, PHI_I,THK10
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA IUN/1/
      DATA PUN/0.1/
C=======================================================================
C------------------------
C      ORTHOTROPIC SHELL
C------------------------
C=======================================================================
C fixing flag and removed from input
        ISTRAIN=1
        IHBE=0
        ISMSTR=0
        ISROT=0
        IGMAT =0
        PTHK = ZERO
        IRP = 0
        IDSK = 0

        IS_ENCRYPTED = .FALSE.
        IS_AVAILABLE = .FALSE.

C HIDDEN FLAGS
C----------------------
        ISHEAR = 0
C CVIS USED in coquez routines
        CVIS = ONE


C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
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
Capply submodel transform to V (VX,VY,VZ) 
C
      IF (SUB_ID /= 0)
     .         CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)

C------lecgeo :
c       WRITE(IOUT,'(A40)') IDTITL
C        Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
         IGEO(1)=IG
         IGEO(11)=IGTYP
         GEO(12) =IGTYP+PUN

         PTHK = MIN(PTHK, ONE-EM06)
         GEO(42) = PTHK
C fixing flag and removed from input
        ISTRAIN=1
C---------IHBE,ISH3N-------------
         IF(IHBE==0)IHBE=IHBE_D
         IHBEOUTP=IHBE
         IF (IHBE == 4 .AND. ISH3N==0 .AND. ISH3N_D == 1) THEN
           CALL ANCMSG(MSGID=680,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                  I1=IG,
     .                  C1=IDTITL)
         ENDIF
         IF (IHBE==22.OR.IHBE==23) THEN
           CALL ANCMSG(MSGID=539,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=IG,
     .                 C1=IDTITL)
           IHBE=24
         ENDIF
C    Double stockage temporaire - 
         IGEO(10)=IHBE
         GEO(171)=IHBE
C------don't know if still useful         
         IF(IHBE==2)THEN
           GEO(171)=0
         ELSEIF(IHBE>=3.AND.IHBE<100.AND.IHBE/=4)THEN
           GEO(171)=IHBE-1
         ENDIF
         IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))
         IF(ISH3N==0) ISH3N = ISH3N_D
         IGEO(18) = ISH3N
C---------ISMSTR-------------
         IF(ISMSTR==0)ISMSTR=ISST_D
         IF (ISST_D == -2) ISMSTR = -1
         IF(ISMSTR==0)ISMSTR=2
         IF(ISMSTR==3.AND.IHBE/=0.AND.IHBE/=2) THEN
            ISMSTR = 2
            CALL ANCMSG(MSGID=319,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=IG,
     .                  C1=IDTITL)
         ENDIF
         GEO(3)=ISMSTR
         IGEO(5)  = ISMSTR
C---------ISROT-------------
         IF(ISROT==0)ISROT=IDRIL_D
         IF(ISROT>=2) ISROT = 0
         IGEO(20)=ISROT
C---------ITHK,ISHEAR,IPLAST,ISTRAIN-------------
         IF(ITHK == 0)ITHK=ITHK_D
         IF(ITHK_D==-2)ITHK=-1
         IF(ISHEAR == 0)ISHEAR=ISHEA_D
         IF(IPLAST == 0)IPLAST=IPLA_D
         IF(IPLA_D==-2) IPLAST=-1
C         IF(ISTRAIN == 0)ISTRAIN=ISTR_D
         GEO(11)=ISTRAIN
         GEO(35)=ITHK
         GEO(37)=ISHEAR
         IF(ISHEAR==2) GEO(37)=0
         GEO(39)=IPLAST
         IF(GEO(39)/=ZERO.AND.IGEO(9)== 0)IGEO(9)=IPLAST
C-------other initializations----            
         IF (ISMSTR==10.AND.ISROT>0.AND.IDROT==0) IDROT = 1
C
         IF(IHBE==3)THEN
           IF(GEO(13)==ZERO)GEO(13)=EM01
           IF(GEO(14)==ZERO)GEO(14)=EM01
           IF(GEO(15)==ZERO)GEO(15)=EM02
         ELSE
           IF(GEO(13)==ZERO)GEO(13)=EM02
           IF(GEO(14)==ZERO)GEO(14)=EM02
           IF(GEO(15)==ZERO)GEO(15)=EM02
         ENDIF
         IF(ISMSTR==3) GEO(5)=EP06
C------Dm         
         IF (GEO(16) == ZERO) IGEO(31) = 1
C------Dn  : GEO(13) in Engine       
         IF (IHBE==12) GEO(13)=GEO(17)
         IF (IHBE==24) THEN
           IF (GEO(17)==ZERO) GEO(17)=ZEP015
           GEO(13)=GEO(17)
           GEO(17)=CVIS
         ENDIF
C------lecgeo10 :
      NLYMAX= 100
      IPANG = 200
      IPTHK = 300
      IPPOS = 400
      IPWEIGHT = 900
      IF(GEO(38) == ZERO) GEO(38)=FIVE_OVER_6
      IF(NLY == -1)NLY=NPTS_D
      IF(NLY == 0) NLY = 1
      IF(NLY == 1) GEO(38)= ZERO
      AN=SQRT(VX*VX+VY*VY+VZ*VZ)
c
      DO I=1,NLY
        GEO(IPWEIGHT+I) = ONE
      ENDDO
c
      IF(AN <EM10)THEN
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
      GEO(7)=VX
      GEO(8)=VY
      GEO(9)=VZ
      ISS=ISMSTR
C----      
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
      IGEO(14) = IRP

C---+#---1----+----2----+----3----+----4----+----5----+----6----+----7-# 
       IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,1000)IG
 1000    FORMAT(
     &    5X,'ORTHOTROPIC LAYERED SHELL PROPERTY SET'/,
     &    5X,'--------------------------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
       ELSE
       IF (IHBE>11.AND.IHBE<29) THEN
        WRITE(IOUT,2011)IG,NLY,ISTRAIN,GEO(1),ISS,IHBE,
     .              ISH3N,IGEO(20),
     .              GEO(16),GEO(13),GEO(38),GEO(42),ISHEAR,ITHK,IPLAST,
     .              GEO(7),GEO(8),GEO(9),IDSK,IGEO(14)
       ELSE
        WRITE(IOUT,2010)IG,NLY,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,
     .              GEO(13),GEO(14),GEO(15),GEO(16),
     .              GEO(38),GEO(42),ISHEAR,ITHK,IPLAST,
     .              GEO(7),GEO(8),GEO(9),IDSK,IGEO(14)
       ENDIF
      ENDIF
      IF (NLY == 0) THEN
         CALL ANCMSG(MSGID=27,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (NLY>NLYMAX) THEN
         CALL ANCMSG(MSGID=28,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I2=NLYMAX,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      N1=MIN0(NLY,NLYMAX)
      GEO(6)  = N1+EM01
      IGEO(4) = N1
      THK10 = ONE/N1
      DO K=1,N1
       CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_phi',PHI_I,K,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
       M1 = IPANG+K
       GEO(M1) = PHI_I
       GEO(IPTHK+K) = THK10
       GEO(IPPOS+K) = -HALF*(ONE+THK10)+K*THK10
       IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,2020)  K,GEO(M1)
       GEO(M1)=GEO(M1)*PI/HUNDRED80
      ENDDO
      GEO(200)=GEO(1)


C--------   Variables stored in element buffer 

c----   Shells
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
C
          PROP_TAG(IGTYP)%LY_PLAPT = 1
          PROP_TAG(IGTYP)%LY_SIGPT = 5
          PROP_TAG(IGTYP)%G_FORPG  = 5
          PROP_TAG(IGTYP)%G_MOMPG  = 3
          PROP_TAG(IGTYP)%G_STRPG  = 8
C


C-----------
      RETURN
C-----------
 2010 FORMAT(
     & 5X,'ORTHOTROPIC LAYERED SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN. . . . . . . . . . . . . .=',I10/,
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
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2011 FORMAT(
     & 5X,'ORTHOTROPIC LAYERED SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN. . . . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'% OF LAYER THICKNESS TO FAIL. . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2020 FORMAT(
     & 5X,'LAYER,ANGLE (DIR 1,PROJ(DIR 1 / SHELL).=',I10,E12.5)
C-----------

      END




