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
Chd|  HM_READ_PROP35                source/properties/spring/hm_read_prop35.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP35(IOUT  ,NUVAR ,PARGEO,UNITAB,
     .                          ID, IGTYP ,PROP_TAG,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD
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
#include      "scr17_c.inc"
#include      "tablen_c.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
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
      INTEGER IOUT,NUVAR,ID,IGTYP
      my_real   
     .        PARGEO(*)
      INTEGER SET_U_PNU,SET_U_GEO,KFUNC
      EXTERNAL SET_U_PNU,SET_U_GEO
      PARAMETER (KFUNC=29)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC1,IFUNC2,IFUNC3,IFUNC4,IERROR,ILOAD
      my_real 
     .  AMAS,ELASTIF,XLIM1,XLIM2,XK,D1,D2,RLOAD,FSCAL
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C=======================================================================
C
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A1',IFUNC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C1',IFUNC3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D1',IFUNC4,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('Amas',AMAS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Elastif',ELASTIF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Xlim1',XLIM1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Xk',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Damg',D1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Fdelay',D2,IS_AVAILABLE,LSUBMODEL,UNITAB)
      !units
      CALL HM_GET_FLOATV_DIM('Elastif',FSCAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C     Hidden flag XLIM2,ILOAD - not documented
      ILOAD = 0
      XLIM2 = ZERO
C----------------------
C
      IF(.NOT. IS_CRYPTED)THEN
        WRITE(IOUT,1400) ID
      ELSE
        WRITE(IOUT,1500) ID
      ENDIF
C
      NUVAR  = 3    
C
      PARGEO(1) = 0
      PARGEO(2) = XK
C     front wave = 1
      PARGEO(3) = 1
C
      IF(.NOT. IS_CRYPTED)THEN
        WRITE(IOUT,1000)
     .    AMAS,ELASTIF,XLIM1,XLIM2,XK,D1,D2,ILOAD,
     .    IFUNC1,IFUNC2,IFUNC3,IFUNC4
      ENDIF
C
      IERROR = SET_U_GEO(1,AMAS)
      IERROR = SET_U_GEO(2,ELASTIF)
      IERROR = SET_U_GEO(3,XLIM1)
      IERROR = SET_U_GEO(4,XLIM2)
      IERROR = SET_U_GEO(5,D1)
      IERROR = SET_U_GEO(6,D2)
      RLOAD  = ILOAD
      IERROR = SET_U_GEO(7,RLOAD)
      IERROR = SET_U_GEO(8,FSCAL)
      IERROR = SET_U_PNU(1,IFUNC1,KFUNC)
      IERROR = SET_U_PNU(2,IFUNC2,KFUNC)
      IERROR = SET_U_PNU(3,IFUNC3,KFUNC)
      IERROR = SET_U_PNU(4,IFUNC4,KFUNC)
C
C-----------------------------
C     PROPERTY BUFFER 
C-----------------------------
      PROP_TAG(IGTYP)%G_FOR  = 3
      PROP_TAG(IGTYP)%G_MOM = 5
      PROP_TAG(IGTYP)%G_SKEW = 6
      PROP_TAG(IGTYP)%G_MASS = 1
      PROP_TAG(IGTYP)%G_V_REPCVT  = 3 ! -- VITESSES REPERE CONVECTEE (V_REPCVT)
      PROP_TAG(IGTYP)%G_VR_REPCVT = 3  ! -- VITESSES REPERE CONVECTEE (VR_REPCVT)
      PROP_TAG(IGTYP)%G_NUVAR = NUVAR
C
      RETURN
 999  CONTINUE
        WRITE(IOUT,*)' **ERROR IN PROPERTY 35 INPUT'
      RETURN
 1000 FORMAT(
     & 5X,'MASS PER UNIT LENGTH. . . . . . . . . .=',1PG20.13/,
     & 5X,'STIFFNESS PER UNIT LENGTH . . . . . . .=',1PG20.13/,
     & 5X,'TRACTION TRANSITION DEFORMATION . . . .=',1PG20.13/,
     & 5X,'COMPRESSION TRANSITION DEFORMATION. . .=',1PG20.13/,
     & 5X,'STIFFNESS FOR INTERFACE . . . . . . . .=',1PG20.13/,
     & 5X,'DAMAGE FACTOR . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'DAMAGE DELAY PARAMETER. . . . . . . . .=',1PG20.13/,
     & 5X,'DAMAGE IN LOAD FLAG . . . . . . . . . .=',I10/,
     & 5X,'INITIAL TRACTION USER FUNCTION ID . . .=',I10/,
     & 5X,'INITIAL COMPRESSION USER FUNCTION ID. .=',I10/,
     & 5X,'FINAL TRACTION USER FUNCTION ID . . . .=',I10/,
     & 5X,'FINAL COMPRESSION USER FUNCTION ID. . .=',I10//)
 1400 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)
C
 1500 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,
     & 5X,'CONFIDENTIAL DATA'//)
      END
