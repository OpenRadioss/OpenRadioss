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
Chd|  HM_READ_PROP03                source/properties/beam/hm_read_prop03.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP03(GEO       ,IGEO     ,PROP_TAG ,IGTYP    ,IG        ,
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER IGTYP,IG
      my_real
     .   GEO(*)
      CHARACTER IDTITL*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISMSTR,ISHEAR,IHBE,
     .   IHBE_OLD, 
     .   IR1X, IR1Y, IR1Z, IR2X, IR2Y, IR2Z,IRX 
      my_real
     .    PUN,CVIS
      CHARACTER STRING*ncharfield,CHROT*7
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C=======================================================================
C
        IS_ENCRYPTED = .FALSE.
        IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C OLD HIDDEN FLAGS - SET TO ZERO
C--------------------------------------------------
        IHBE=0
        ISMSTR=0
        CVIS=0
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wx1',IR1X,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wy1',IR1Y,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wz1',IR1Z,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wx2',IR2X,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wy2',IR2Y,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Translation_Wz2',IR2Z,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('ISHEAR',ISHEAR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('MAT_Dm',GEO(16),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('MAT_Df',GEO(17),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('AREA',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('IYY',GEO(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('IZZ',GEO(18),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('IXX',GEO(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
C
C----------------------
        IF(N2D>0.AND.IHBE/=0.AND.IHBE/=2)THEN
          IHBE_OLD=IHBE
          IHBE=0
          CALL ANCMSG(MSGID=321,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=IG,
     .                C1=IDTITL,
     .                I2=IHBE_OLD,
     .                I3=IHBE)
        ENDIF
C
        GEO(3)=ISMSTR
        IF ((ISMSTR==3).OR.(ISMSTR==1)) GEO(5)=EP06
C  double stockage temporaire - supprimer GEO(12)=IGTYP apres tests
        IGEO( 1)=IG
        IGEO(10)=IHBE
        IGEO(11)=IGTYP
        GEO(12) =IGTYP+PUN
        GEO(171)=IHBE
C
        IF (IHBE>11.AND.IHBE<29) THEN
C---------GEO(13,I) est utilise pour porte dn;GEO(17),CVIS se change ----
          GEO(13)=GEO(17)
          GEO(17)=CVIS
C          IGEO(20)=ISROT
        ENDIF
C
C----------------------
C
        IF(ISMSTR==2.OR.ISMSTR==4)THEN
          ISMSTR=0
        ELSEIF(ISMSTR==1.OR.ISMSTR==3)THEN
          ISMSTR=1
        ENDIF
        IF(GEO(17)==ZERO) GEO(17)=EM02
C
        IRX=MIN(1,IR1X+IR2X)
        GEO(7)= 1.1-IRX
        GEO(8)= 1.1-IR1Y
        GEO(9)= 1.1-IR1Z
        GEO(10)=1.1-IR2Y
        GEO(11)=1.1-IR2Z
        IF(ISHEAR/=0) THEN
          GEO(37)=ONE
        ELSE
          GEO(37)=ZERO
        ENDIF
C
        IF (GEO(1)<=ZERO) THEN
          CALL ANCMSG(MSGID=314,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IG,
     .                   C1=IDTITL,
     .                   R1=GEO(1))
        ENDIF
        IF (GEO(2)<=ZERO) THEN
          CALL ANCMSG(MSGID=315,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IG,
     .                   C1=IDTITL,
     .                   R1=GEO(2))
        ENDIF
        IF (GEO(18)<=ZERO) THEN
           CALL ANCMSG(MSGID=316,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IG,
     .                   C1=IDTITL,
     .                   R1=GEO(18))
        ENDIF
        IF (GEO(4)<=ZERO) THEN
           CALL ANCMSG(MSGID=317,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IG,
     .                   C1=IDTITL,
     .                   R1=GEO(4))
        ENDIF
        IF(.NOT. IS_ENCRYPTED)THEN
          WRITE(IOUT,1300)IG,GEO(1),GEO(2),GEO(18),GEO(4),
     .                    IR1X,IR1Y,IR1Z,IR2X,IR2Y,IR2Z,ISHEAR,
     .                    GEO(16),GEO(17)
        ELSE
          WRITE(IOUT,1399)IG
        ENDIF
        GEO(3)=ISMSTR
C
        ISHEAR = GEO(37)
        IF(ISHEAR==0)THEN
          GEO(37)=0
        ELSEIF(ISHEAR==1)THEN
          GEO(37)=1
        ELSEIF(ISHEAR==2)THEN
          GEO(37)=0
        ENDIF
        IF(GEO(3)/=ZERO.AND.IGEO(5)== 0) IGEO(5)=NINT(GEO(3))
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
        PROP_TAG(IGTYP)%L_STRA = 3
C
C------------------------------
C----
      RETURN
C-----------
 1300 FORMAT(
     & 5X,'BEAM PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'BEAM AREA . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MOMENT OF INERTIA IYY . . . . . . . . .=',1PG20.13/,
     & 5X,'MOMENT OF INERTIA IZZ . . . . . . . . .=',1PG20.13/,
     & 5X,'MOMENT OF INERTIA IXX . . . . . . . . .=',1PG20.13/,
     & 5X,'NODE 1 LOCAL ROTATION RELEASE X DIR.. .=',I10/,
     & 5X,'NODE 1 LOCAL ROTATION RELEASE Y DIR.. .=',I10/,
     & 5X,'NODE 1 LOCAL ROTATION RELEASE Z DIR.. .=',I10/,
     & 5X,'NODE 2 LOCAL ROTATION RELEASE X DIR.. .=',I10/,
     & 5X,'NODE 2 LOCAL ROTATION RELEASE Y DIR.. .=',I10/,
     & 5X,'NODE 2 LOCAL ROTATION RELEASE Z DIR.. .=',I10/,
     & 5X,'SHEAR FORMULATION (0=YES/1=NO). . . . .=',I10/,
     & 5X,'BEAM STRUCTURAL MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'BEAM STRUCTURAL FLEXURAL DAMPING. . . .=',1PG20.13/)
 1399 FORMAT(
     & 5X,'BEAM PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'CONFIDENTIAL DATA'//)
c
      END

