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
Chd|  HM_READ_PROP02                source/properties/truss/hm_read_prop02.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP02(IGTYP ,IG  , IGEO   ,  GEO   ,PROP_TAG  ,
     .                   UNITAB ,IDTITL,LSUBMODEL )
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
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tablen_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER 
     .        IGTYP , IGEO(*)
      my_real GEO(*)

      CHARACTER IDTITL*nchartitle

      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J
     
      my_real
     .    PUN,GAP
      CHARACTER*nchartitle,
     .   TITR
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C=======================================================================
C------------------------
C      TRUSS PROPERTY
C------------------------
C=======================================================================

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

C        Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN

      CALL HM_GET_FLOATV('AREA',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      GEO(2) = MAX(ZERO,GAP)

      IF(.NOT. IS_ENCRYPTED)THEN
         WRITE(IOUT,1200)IG,GEO(1),GEO(2)
      ELSE
         WRITE(IOUT,1299)IG
      ENDIF

      IF (GEO(1)<=ZERO) THEN
          CALL ANCMSG(MSGID=497,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL,
     .                R1=GEO(1))
      ENDIF


      PROP_TAG(IGTYP)%G_FOR  = 1
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_LENGTH = 1 ! total length
      PROP_TAG(IGTYP)%G_AREA = 1
      PROP_TAG(IGTYP)%G_STRA = 1

C-----------
      RETURN
 999  CALL FREERR(3)
      RETURN
      CALL FREERR(3)
      RETURN
C-----------
 1200 FORMAT(
     & 5X,'TRUSS PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'TRUSS AREA. . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'TRUSS INITIAL GAP . . . . . . . . . . .=',1PG20.13//)
 1299 FORMAT(
     & 5X,'TRUSS PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'CONFIDENTIAL DATA'//)
C-----------

      END




