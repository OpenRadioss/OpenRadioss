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
Chd|  HM_READ_PROP0                 source/properties/void/hm_read_prop0.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP0(GEO    ,IGEO   ,IG    ,IGTYP, UNITAB,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ PROPERTY TYPE0 WITH HM READER
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
C     SUB_INDEX       SUBMODEL INDEX(INTEGER)
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD  
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IG
      INTEGER,INTENT(IN)::IGTYP 
c      INTEGER,INTENT(IN)::SUB_INDEX
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(*)
      my_real,
     .  INTENT(INOUT)::GEO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IP, K, N, IHBE, ISMSTR, ISORTH, ISHEAR
      my_real
     .   PUN
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IHBE=0
      ISMSTR=0
      ISORTH= 0
C
      GEO(3)=ISMSTR
      IF(ISMSTR==3.AND.IGTYP/=6.AND.IGTYP/=14)THEN
        GEO(5)=EP06
      ENDIF
C  double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
      IGEO( 1)=IG
      IGEO(10)=IHBE
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
      GEO(171)=IHBE
c
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('THICK',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      WRITE(IOUT,1010)IG
      IF(.NOT. IS_ENCRYPTED)THEN
        WRITE(IOUT,1011)IG,GEO(1)
      ELSE
        WRITE(IOUT,1099)IG
      ENDIF
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
      IGEO(1) =IG
      IGEO(11)=IGTYP
      IGEO(17)=ISORTH
      IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0) IGEO( 9)=NINT(GEO(39))
      IF(GEO(171)/=ZERO.AND.IGEO(10)== 0) IGEO(10)=NINT(GEO(171))
C
 1010 FORMAT(
     & 5X,'DUMMY PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10//)
 1011 FORMAT(
     & 5X,'DUMMY PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'THICKNESS . . . . . . . . . . . . . . .=',1PG20.13//)
 1099 FORMAT(
     & 5X,'DUMMY PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'CONFIDENTIAL DATA'//)
C      
      RETURN
      END


