Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    hm_read_prop43           ../starter/source/properties/solid/hm_read_prop43.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_prop_generic     ../starter/source/properties/hm_read_prop_generic.F
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- uses       -----------------------------------------------------
      !||    defaults_mod             ../starter/source/modules/defaults_mod.F90
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_PROP43(GEO   ,IGEO  ,IOUT      ,IG     ,
     .                          NUVAR ,PARGEO,IGTYP,PROP_TAG,
     .                          UNITAB,LSUBMODEL,DEFAULTS_SOLID)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE ELBUFTAG_MOD           
      USE DEFAULTS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C----------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "tablen_c.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C----------+---------+---+---+--------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI)
      INTEGER IG,IGTYP
      my_real   GEO(*) 

      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      my_real   
     .        PARGEO(*)
      TYPE(SOLID_DEFAULTS_), INTENT(IN) :: DEFAULTS_SOLID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IOUT,IERROR,NUVAR,NPG,JCVT,ISMSTR
      INTEGER     IHBE_DS,ISST_DS,IPLA_DS,IFRAME_DS
      my_real
     .   TTHICK
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C
      NUVAR  = 0
      NPG    = 4
      JCVT   = 1
      ISMSTR = 0
!--- defaults values      
      IHBE_DS=  DEFAULTS_SOLID%ISOLID
      ISST_DS=  DEFAULTS_SOLID%ISMSTR
      IFRAME_DS= DEFAULTS_SOLID%IFRAME
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('THICK',TTHICK,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      GEO(41) = TTHICK
C
      PARGEO(1) = 0                                                  
      PARGEO(2) = 0                                                  
      PARGEO(3) = 0                                                  
      PARGEO(4) = NPG                                                
      PARGEO(5) = JCVT                                               
      IF (ISMSTR == 0) ISMSTR = ISST_DS                              
      IF (ISMSTR  < 0 .OR. ISST_DS==-2) ISMSTR = 1                            
      IF (ISMSTR == 0 .OR. ISMSTR == 2 .OR. ISMSTR == 3) ISMSTR = 1  
      IF (ISMSTR == 10) ISMSTR = 4 
                                   
      IGEO(5) = ISMSTR   
c
C-----------     
c
      IF(.NOT. IS_ENCRYPTED)THEN
        IF(TTHICK > 0) THEN
          WRITE(IOUT,1100) IG,ISMSTR,TTHICK
        ELSE
          WRITE(IOUT,1000) IG,ISMSTR
        ENDIF
      ELSE
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ENDIF

      PROP_TAG(IGTYP)%G_SIG  = 6
      PROP_TAG(IGTYP)%L_SIG  = 6
      PROP_TAG(IGTYP)%G_VOL  = 1
      PROP_TAG(IGTYP)%L_VOL  = 1
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%L_EINT = 1
      PROP_TAG(IGTYP)%G_GAMA = 9
      PROP_TAG(IGTYP)%G_FILL = 1

C-----------
      RETURN
C-----------
 1000 FORMAT(
     & 5X,'SOLID SPOTWELD PROPERTY SET'/,
     & 5X,'PROPERTY SET ID . . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,'SOLID SPOTWELD PROPERTY SET'/,
     & 5X,'PROPERTY SET ID . . . . . . . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'TRUE THICKNESS  . . . . . . . . . . . .=',1PG20.13/)
C-----------
      RETURN
      END SUBROUTINE HM_READ_PROP43
