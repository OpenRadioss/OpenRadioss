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
Chd|  HM_READ_PROP18                source/properties/beam/hm_read_prop18.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEFBEAM_SECT                  source/properties/beam/hm_read_prop18.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP18(GEO       ,IGEO     ,PROP_TAG ,IGTYP    ,IG        ,
     .                          IDTITL    ,UNITAB   ,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "tablen_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER IGEO(*)
      INTEGER IGTYP,IG,NBADI
      my_real
     .   GEO(*)
      CHARACTER IDTITL*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER STRING*ncharfield,CHROT*7
      INTEGER I,J,IP,INTR,INTS,NIP,NIPMAX,NC,NS,IPY,IPZ,IPA,IREF,ISECT,
     .        IRX,IR1X,IR1Y,IR1Z,IR2X,IR2Y,IR2Z,ISS
      INTEGER IHBE,ISMSTR,ISHEAR
C     REAL
      my_real
     .    DM,DR,PY,PZ,AI,YI,ZI,WI,AREA,Y0,Z0,L1,L2,L3,L4,L5,L6,PUN
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
      DATA PUN/0.1/
C=======================================================================
C
        IS_ENCRYPTED = .FALSE.
        IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C OLD HIDDEN FLAGS - SET TO ZERO
C READ(LINE,ERR=999,FMT=FMT_4I_4X_F) IHBE,ISMSTR,ISH3N,ISROT,CVIS
C IHBE -> ISECT
C ISH3N,ISROT,CVIS not used
C--------------------------------------------------
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('ISFLAG',ISECT,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('NIP'   ,NIP,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Iref'  ,IREF,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('NITRS' ,INTR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wx1',IR1X,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wy1',IR1Y,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wz1',IR1Z,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wx2',IR2X,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wy2',IR2Y,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wz2',IR2Z,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('Dm',DM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('df',DR,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Y0',Y0,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Z0',Z0,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('L1',L1,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('L2',L2,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
C
        GEO(3)=ISMSTR
        IF (ISMSTR==3) GEO(5)=EP06
C  double stockage temporaire - supprimer GEO(12)=IGTYP apres tests
        IGEO( 1)=IG
        IGEO(10)=ISECT
        IGEO(11)=IGTYP
        GEO(12) =IGTYP+PUN
        GEO(171)=ISECT

C----------------------
        IF (ISMSTR==2 .OR. ISMSTR==4) THEN
          ISMSTR=0
        ELSEIF (ISMSTR==1 .OR. ISMSTR==3) THEN
          ISMSTR=1
        ENDIF
        GEO(3)  = ISMSTR

C-------
        NIPMAX = 100
        IPY    = 200
        IPZ    = 300
        IPA    = 400
        AREA   = ZERO
        ISECT  = IGEO(10)
        ISS    = INT(GEO(3))
        IF(ISS == 0) ISS = 4
        IF (DR == ZERO) DR = EM04
C---                                                  
        NIP = MIN(NIP,NIPMAX)
c------------------------
        IF (ISECT == 0) THEN
C---      user-defined integration points
          DO IP = 1,NIP
            CALL HM_GET_FLOAT_ARRAY_INDEX('Y_IP',PY,IP,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('Z_IP',PZ,IP,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('AREA_IP',AI,IP,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
            IF (AI<=ZERO) THEN
             CALL ANCMSG(MSGID=314,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IG,
     .                   C1=IDTITL,
     .                   R1=AI)
            ENDIF
            GEO(IPY+IP) = PY
            GEO(IPZ+IP) = PZ
            GEO(IPA+IP) = AI
            AREA = AREA + AI
          ENDDO

        ELSE
C---    predefined sections
C--------------------------------------------------
C OLD HIDDEN FLAGS - SET TO ZERO     
C          READ(IIN,REC=IREC,ERR=999,FMT=FMT_2I_4F) INTR,INTS,L1,L2,L3,L4     
C          READ(IIN,REC=IREC,ERR=999,FMT=FMT_2F) L5,L6
C
          INTS = 0
          L3 = ZERO
          L4 = ZERO
          L5 = ZERO
          L6 = ZERO
C-------------------------------------------------- 
C
          IF (INTR == 0)  INTR = 2
          IF (INTS == 0)  INTS = 2
          IF (L1 == ZERO) L1 = ONE
          IF (L2 == ZERO) L2 = ONE
          IF (L3 == ZERO) L3 = ONE
          IF (L4 == ZERO) L4 = ONE
          IF (L5 == ZERO) L5 = ONE
          IF (L6 == ZERO) L6 = ONE
c
          CALL DEFBEAM_SECT(GEO,ISECT,INTR,INTS,NIP,
     .                    AREA,L1,L2,L3,L4,L5,L6,IG,IDTITL)
        ENDIF
c------------------------
        IF (NIP > 100) THEN
          CALL ANCMSG(MSGID=977,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL,
     .              I2=NIP)
        ENDIF
C---  coord isoparametriques et surfaces relatives des pts d'integration
        IF (ISECT == 0.AND.IREF == 0) THEN
          Y0 = ZERO
          Z0 = ZERO
          DO IP = 1,NIP
            Y0 = Y0 + GEO(IPY+IP)*GEO(IPA+IP)
            Z0 = Z0 + GEO(IPZ+IP)*GEO(IPA+IP)
          ENDDO
          Y0 = Y0 / AREA
          Z0 = Z0 / AREA
        ENDIF
        DO IP = 1,NIP
          GEO(IPY+IP) = GEO(IPY+IP) - Y0
          GEO(IPZ+IP) = GEO(IPZ+IP) - Z0
        ENDDO
C---  rotation dof 
        IRX = MIN(1,IR1X+IR2X)
        GEO(7) = 1.1-IRX
        GEO(8) = 1.1-IR1Y
        GEO(9) = 1.1-IR1Z
        GEO(10)= 1.1-IR2Y
        GEO(11)= 1.1-IR2Z
C---
        IF(.NOT. IS_ENCRYPTED)THEN
          WRITE(IOUT,1000)IG,ISS,IR1X,IR1Y,IR1Z,IR2X,IR2Y,IR2Z,DM,DR,ISECT,NIP
          DO IP=1,NIP
            WRITE(IOUT,1010) IP,Y0+GEO(IPY+IP),Z0+GEO(IPZ+IP),
     .                   GEO(IPA+IP)
          ENDDO
          WRITE(IOUT,*)
        ELSE
          WRITE(IOUT,1020) IG
        ENDIF
C---                 
        GEO(1)  = AREA
        GEO(16) = DM
        GEO(17) = DR
        GEO(21) = Y0
        GEO(22) = Z0
        IGEO(3) = NIP
C
C----------------------
C
        ISHEAR = GEO(37)
        IF(ISHEAR==0)THEN
          GEO(37)=0
        ELSEIF(ISHEAR==1)THEN
          GEO(37)=1
        ELSEIF(ISHEAR==2)THEN
          GEO(37)=0
        ENDIF
C
        IF(GEO( 3)/=ZERO.AND.IGEO(5)== 0)IGEO(5)=NINT(GEO(3))
        IF(GEO(171)/=ZERO.AND.IGEO(10)== 0) IGEO(10)=NINT(GEO(171))
C
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
C
        PROP_TAG(IGTYP)%G_FOR = 3
        PROP_TAG(IGTYP)%G_MOM = 3
        PROP_TAG(IGTYP)%G_EINT = 2
        PROP_TAG(IGTYP)%G_LENGTH = 1 ! total length
        PROP_TAG(IGTYP)%G_SKEW = 3  ! local skew (RLOC)
        PROP_TAG(IGTYP)%L_SIG = 3
        PROP_TAG(IGTYP)%L_STRA = 3
C-----------------------------
C
        RETURN
 999    CALL FREERR(3)
        RETURN
C---
 1000 FORMAT(
     & 5X,'INTEGRATED BEAM PROPERTY SET (TYPE 18)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'NODE 1 LOCAL ROTATION RELEASE X DIR.. .=',I10/,
     & 5X,'NODE 1 LOCAL ROTATION RELEASE Y DIR.. .=',I10/,
     & 5X,'NODE 1 LOCAL ROTATION RELEASE Z DIR.. .=',I10/,
     & 5X,'NODE 2 LOCAL ROTATION RELEASE X DIR.. .=',I10/,
     & 5X,'NODE 2 LOCAL ROTATION RELEASE Y DIR.. .=',I10/,
     & 5X,'NODE 2 LOCAL ROTATION RELEASE Z DIR.. .=',I10/,
     & 5X,'BEAM STRUCTURAL MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'BEAM STRUCTURAL FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SECTION TYPE. . . . . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. . . . . .=',I10//,
     & 5X,'INTEGRATION POINTS:'/)
 1010 FORMAT(
     & 5X,'POINT NO: . . . . . . . . . . . . . =',I10/,
     & 8X,'LOCAL Y POSITION. . . . . . . . . . =',1PG20.13/,
     & 8X,'LOCAL Z POSITION. . . . . . . . . . =',1PG20.13/,
     & 8X,'POINT AREA. . . . . . . . . . . . . =',1PG20.13)
 1020 FORMAT(
     & 5X,'INTEGRATED BEAM PROPERTY SET (TYPE 18)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,
     & 5X,'CONFIDENTIAL DATA'//)

      END SUBROUTINE

Chd|====================================================================
Chd|  DEFBEAM_SECT                  source/properties/beam/hm_read_prop18.F
Chd|-- called by -----------
Chd|        HM_READ_PROP18                source/properties/beam/hm_read_prop18.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DEFBEAM_SECT(GEO,ISECT,INTR,INTS,NIP,
     .                        AREA,L1,L2,L3,L4,L5,L6,IG,IDTITL)
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISECT,INTR,INTS,NIP,IG
C     REAL
      my_real
     .   AREA,L1,L2,L3,L4,L5,L6
      my_real
     .   GEO(NPROPG)
      CHARACTER IDTITL*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IP,NC,NS,IPY,IPZ,IPA
C     REAL
      my_real
     .    AI,YI,ZI,WI,PHI,DPHI,R1,R2,R3,R4,D2, D3, D4
      my_real
     +  W_GAUSS(9,9),A_GAUSS(9,9),W_LOBATTO(9,9),A_LOBATTO(9,9),LEN(10)
C-----------------------------------------------
      DATA W_GAUSS /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
C-----------------------------------------------
      DATA W_LOBATTO /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.333333333333333,1.333333333333333,0.333333333333333,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.166666666666667,0.833333333333333,0.833333333333333,
     4 0.166666666666667,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.100000000000000,0.544444444444444,0.711111111111111,
     5 0.544444444444444,0.100000000000000,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.066666666666667,0.378474956297847,0.554858377035486,
     6 0.554858377035486,0.378474956297847,0.066666666666667,
     6 0.               ,0.               ,0.               ,
     7 0.047619047619048,0.276826047361566,0.431745381209863,
     7 0.487619047619048,0.431745381209863,0.276826047361566,
     7 0.047619047619048,0.               ,0.               ,
     8 0.035714285714286,0.210704227143506,0.341122692483504,
     8 0.412458794658704,0.412458794658704,0.341122692483504,
     8 0.210704227143506,0.035714285714286,0.               ,
     9 0.027777777777778,0.165495361560806,0.274538712500162,
     9 0.346428510973046,0.371519274376417,0.346428510973046,
     9 0.274538712500162,0.165495361560806,0.027777777777778/
      DATA A_LOBATTO /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -1.00000000000000,1.000000000000000,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -1.00000000000000,0.               ,1.000000000000000,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -1.00000000000000,-.447213595499958,0.447213595499958,
     4 1.000000000000000,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -1.00000000000000,-.654653670707977,0.               ,
     5 0.654653670707977,1.000000000000000,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -1.00000000000000,-.765055323929465,-.285231516480645,
     6 0.285231516480645,0.765055323929465,1.000000000000000,
     6 0.               ,0.               ,0.               ,
     7 -1.00000000000000,-.830223896278567,-.468848793470714,
     7 0.               ,0.468848793470714,0.830223896278567,
     7 1.000000000000000,0.               ,0.               ,
     8 -1.00000000000000,-.871740148509607,-.591700181433142,
     8 -.209299217902479,0.209299217902479,0.591700181433142,
     8 0.871740148509607,1.000000000000000,0.               ,
     9 -1.00000000000000,-.899757995411460,-.677186279510737,
     9 -.363117463826178,0.               ,0.363117463826178,
     9 0.677186279510737,0.899757995411460,1.000000000000000/
C======================================================================|
C======================================================================|
      IPY    = 200
      IPZ    = 300
      IPA    = 400
c-----------------------
      SELECT CASE (ISECT)
c---------------
        CASE (1) ! basic rectangular section with Gauss integration rule
c---------------
          AREA = L1*L2
!         INER = AREA*AREA*ONE_OVER_12
          IF (INTR < 1 .OR. INTR > 9) THEN
c           write error message
          ELSEIF (INTR == 1) THEN
            NIP = INTR
            GEO(IPY+1) = ZERO
            GEO(IPZ+1) = ZERO
            GEO(IPA+1) = AREA
          ELSE
            NIP = INTR*INTR
            R1 = L1*HALF
            R2 = L2*HALF
            AI = AREA*FOURTH
            IP = 0
            DO I = 1,INTR
              DO J = 1,INTR
                IP = IP+1
                GEO(IPY+IP)=A_GAUSS(I,INTR)*R1
                GEO(IPZ+IP)=A_GAUSS(J,INTR)*R2
                GEO(IPA+IP)=W_GAUSS(I,INTR)*W_GAUSS(J,INTR)*AI
              ENDDO
            ENDDO
          ENDIF
c---------------
        CASE (2) ! basic circular section with Gauss integration rule
c---------------
          AREA = PI*L1*L1
!         INER = AREA*L1*L1*FOURTH
          IF (INTR < 1 .OR. INTR > 9) THEN
c           write error message
          ELSEIF (INTR == 1) THEN
            NIP = 1
            GEO(IPY+1) = ZERO
            GEO(IPZ+1) = ZERO
            GEO(IPA+1) = AREA
          ELSEIF (INTR == 2) THEN ! cercle divise en 4
            NIP = INTR*INTR
            AI  = AREA/NIP
            R1 = L1/SQR3
            R1 = L1*SQR2*HALF
            DPHI = TWO*PI/NIP
            PHI  = DPHI*HALF
            DO IP = 1,NIP
              GEO(IPY+IP) = R1*SIN(PHI)
              GEO(IPZ+IP) = R1*COS(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
            ENDDO
          ELSEIF (INTR == 3) THEN
            NIP = INTR*INTR
            AI  = AREA/TWELVE   !
            IP = 1
            GEO(IPY+IP) = ZERO
            GEO(IPZ+IP) = ZERO
            GEO(IPA+IP) = AI*FOUR
            R1  = L1*SQR3*HALF
            DPHI = PI*FOURTH
            PHI  = ZERO
            DO IP = 2,NIP
              GEO(IPY+IP) = R1*SIN(PHI)
              GEO(IPZ+IP) = R1*COS(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
            ENDDO
          ELSEIF (INTR == 4) THEN
            NIP = 7
            R1 = L1*SQR2/SQR3
            DPHI = PI*THIRD
            IP = 1
            GEO(IPY+IP) = ZERO
            GEO(IPZ+IP) = ZERO
            GEO(IPA+IP) = AREA*FOURTH
            AI  = AREA/EIGHT
            PHI = ZERO
            DO IP = 2,NIP
              GEO(IPY+IP) = R1*SIN(PHI)
              GEO(IPZ+IP) = R1*COS(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
            ENDDO
          ELSEIF (INTR == 5) THEN
            NIP = 21
            IP = 1
            GEO(IPY+IP) = ZERO
            GEO(IPZ+IP) = ZERO
            GEO(IPA+IP) = AREA/NINE
            AI  = AREA*(SIXTEEN + SQR6)/360.
            R1  = SQRT((SIX-SQR6)/TEN)*L1
            PHI = PI/FIVE
            DO IP = 2,11
              GEO(IPY+IP) = R1*COS(PHI*IP)
              GEO(IPZ+IP) = R1*SIN(PHI*IP)
              GEO(IPA+IP) = AI
            ENDDO
            AI  = AREA*(SIXTEEN - SQR6)/360.
            R1 = SQRT((SIX+SQR6)/TEN)*L1
            DO IP = 12,21
              GEO(IPY+IP) = R1*COS(PHI*IP)
              GEO(IPZ+IP) = R1*SIN(PHI*IP)
              GEO(IPA+IP) = AI
            ENDDO
          ENDIF
c---------------
        CASE (3) ! basic rectangular section with Lobatto integration rule
c---------------
          AREA = L1*L2
!         INER = AREA*AREA*ONE_OVER_12
          IF (INTR < 1 .OR. INTR > 9) THEN
            CALL ANCMSG(MSGID=1878,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
          ELSEIF (INTR == 1) THEN
            NIP = INTR
            GEO(IPY+1) = ZERO
            GEO(IPZ+1) = ZERO
            GEO(IPA+1) = AREA
          ELSE
            NIP = INTR*INTR
            R1 = L1*HALF
            R2 = L2*HALF
            AI = AREA*FOURTH
            IP = 0
            DO I = 1,INTR
              DO J = 1,INTR
                IP = IP+1
                GEO(IPY+IP)=A_LOBATTO(I,INTR)*R1
                GEO(IPZ+IP)=A_LOBATTO(J,INTR)*R2
                GEO(IPA+IP)=W_LOBATTO(I,INTR)*W_LOBATTO(J,INTR)*AI
              ENDDO
            ENDDO
          ENDIF
c---------------
        CASE (4)    ! circular section    Lobatto 5 OR 7 in radius 8 in circonference
          !POINTS RADIALS ALIGNES
          IF (INTR == 17) THEN
             NIP = 17
             R1 = 0.5477225575*L1
             R2 = 0.8062257748*L1
             R3 = L1
             D2 = A_LOBATTO(4,5)*L1
             PHI = ZERO
             DPHI= PI * FOURTH
             IP = 1
             GEO(IPY+IP) = ZERO
             GEO(IPZ+IP) = ZERO
             GEO(IPA+IP) = PI*R1*R1
             AI = PI * (R2*R2 - R1*R1)/EIGHT
             DO IP = 2,NIP-1,2
              GEO(IPY+IP) = D2*COS(PHI)
              GEO(IPZ+IP) = D2*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
             ENDDO
             PHI = ZERO
             AI = PI * (R3*R3 - R2*R2)/EIGHT
             DO IP = 3,NIP,2
              GEO(IPY+IP) = L1*COS(PHI)
              GEO(IPZ+IP) = L1*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
             ENDDO
          ELSEIF (INTR == 25) THEN
             NIP = 25
             R1 = 0.46291005*L1
             R2 = 0.69006559*L1
             R3 = 0.859124693*L1
             R4 = L1
             D2 = A_LOBATTO(5,7)*L1
             D3 = A_LOBATTO(6,7)*L1
             D4 = A_LOBATTO(7,7)*L1
             IP = 1
             GEO(IPY+IP) = ZERO
             GEO(IPZ+IP) = ZERO
             GEO(IPA+IP) = PI*R1*R1
             PHI = ZERO
             DPHI= PI * FOURTH
             AI = PI * (R2*R2 - R1*R1)/EIGHT
             DO IP = 2,NIP-2,3
              GEO(IPY+IP) = D2*COS(PHI)
              GEO(IPZ+IP) = D2*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
             ENDDO
             PHI = ZERO
             AI = PI * (R3*R3 - R2*R2)/EIGHT
             DO IP = 3,NIP-1,3
              GEO(IPY+IP) = D3*COS(PHI)
              GEO(IPZ+IP) = D3*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
             ENDDO
             PHI = ZERO
             AI = PI * (R4*R4 - R3*R3)/EIGHT
             DO IP = 4,NIP,3
              GEO(IPY+IP) = D4*COS(PHI)
              GEO(IPZ+IP) = D4*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
             ENDDO


          ENDIF
c---------------
c---------------
        CASE (5) ! circular section with points on edge
c---------------
          AREA = PI*L1*L1
          IF (INTR /= 1.AND. INTR /= 9 .AND. INTR /= 17 ) THEN
            CALL ANCMSG(MSGID=1877,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)

          ELSEIF (INTR == 1) THEN
            NIP = 1
            GEO(IPY+1) = ZERO
            GEO(IPZ+1) = ZERO
            GEO(IPA+1) = AREA
          ELSEIF (INTR == 9) THEN
            NIP = INTR
            R2 = 0.57346235*L1
            IP = 1
            GEO(IPY+IP) = ZERO
            GEO(IPZ+IP) = ZERO
            GEO(IPA+IP) = PI*R2*R2
            R1  =  0.7*L1
            DPHI = PI*HALF
            PHI  = ZERO
            AI   = PI*(L1*L1 - R2*R2)/EIGHT
            DO IP = 2,NIP-1,2
              GEO(IPY+IP) = L1*SIN(PHI)
              GEO(IPZ+IP) = L1*COS(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
            ENDDO
            PHI  = PI*FOURTH
            DO IP = 3,NIP,2
              GEO(IPY+IP) = R1*COS(PHI)
              GEO(IPZ+IP) = R1*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
            ENDDO
          ELSEIF (INTR == 17) THEN   ! POINTS RADIALS NON ALIGNES
             NIP = INTR
             R1 = 0.4472136*L1 ! rayon petit cercle
             R2 = 0.774597 *L1 !rayon cercle moyen
             R3 = L1   ! rayon de la section
             D2 = HALF*L1 !distance au point d'integ
             AI = PI * (R3*R3 - R2*R2)/EIGHT
             PHI = ZERO
             DPHI= PI * FOURTH
             IP = 1
             GEO(IPY+IP) = ZERO
             GEO(IPZ+IP) = ZERO
             GEO(IPA+IP) = PI*R1*R1
             DO IP = 2,NIP-1,2
              GEO(IPY+IP) = L1*COS(PHI)
              GEO(IPZ+IP) = L1*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
             ENDDO
             PHI = PI / EIGHT
             AI = PI * (R2*R2 - R1*R1)/EIGHT
             DO IP = 3,NIP,2
              GEO(IPY+IP) = D2*COS(PHI)
              GEO(IPZ+IP) = D2*SIN(PHI)
              GEO(IPA+IP) = AI
              PHI = PHI + DPHI
             ENDDO
          ENDIF
c---------------
        CASE (6)    ! circular section           not in documentation
c---------------
            NC = 3+INTR   ! number of layers
            NS = 4*NC     ! number of sections
            NIP = NC*NS
            AREA = PI*L1*L1
            AI   = AREA/NIP
            DPHI = PI*TWO/NS
            R1 = ZERO
            R2 = L1 / SQRT(EM20+NC)
            IP = 0
            DO I = 1,NC
              LEN(I) = (R2 + R1*(SQR3-ONE)) / SQR3
              R1 = R2
              R2 = L1*SQRT((I+ONE)/NC)
              PHI  = ZERO
              DO J = 1,NS
                IP = IP+1
                GEO(IPY+IP) = LEN(I)*SIN(PHI)
                GEO(IPZ+IP) = LEN(I)*COS(PHI)
                GEO(IPA+IP) = AI
                PHI  = PHI+DPHI
                AREA = AREA + AI
              ENDDO
            ENDDO
        CASE DEFAULT
      END SELECT
c---------------
      RETURN
      END SUBROUTINE DEFBEAM_SECT
