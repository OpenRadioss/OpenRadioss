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
Chd|  HM_READ_PROP05                source/properties/rivet/hm_read_prop05.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP05(GEO,IGEO,IG,IGTYP,IDTITL,UNITAB,LSUBMODEL )
C============================================================================
C   M o d u l e s
C-----------------------------------------------    
      USE UNITAB_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI),IG,IGTYP
C     REAL
      my_real GEO(NPROPG)
      CHARACTER*nchartitle,
     .   IDTITL
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IROT ,IMODE
C     REAL
      my_real
     .    PUN ,FN ,FT ,DX
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C======================================================================|
c------------------------------
C      RIVETS (Obsolete)
C----------------------

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE. 

      IMODE = 0
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('WFLAG',IROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Imod',IMODE,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('NFORCE',FN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TFORCE',FT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LENGTH',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)

      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
c

      IF (IMODE==0) IMODE = 1
      IF(FN==ZERO)FN=EP15
      IF(FT==ZERO)FT=EP15
      IF(DX==ZERO)DX=EP15
      IF(.NOT.IS_ENCRYPTED)THEN
          WRITE(IOUT,1500)IG,FN,FT,DX,IROT,IMODE
      ELSE
          WRITE(IOUT,1599)IG
      ENDIF

      GEO(1)=FN**2
      GEO(2)=FT**2
      GEO(3)=DX**2
      GEO(4)=IROT+PUN
      GEO(5)=IMODE+PUN

C----
      RETURN
C---
 1500 FORMAT(
     & 5X,'RIVET PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'MAX. TENSILE FORCE. . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAX. SHEAR FORCE. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAX. RIVET LENGTH . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ROTATION FLAG . . . . . . . . . . . . .=',I10/,
     & 5X,' (TRANS = 0 / TRANS+ROT = 1)'/
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10/,
     & 5X,' RIGID BODY = 1 / RIGID LINK = 2'/)
 1599 FORMAT(
     & 5X,'RIVET PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'CONFIDENTIAL DATA'//)
C---
      END SUBROUTINE HM_READ_PROP05
