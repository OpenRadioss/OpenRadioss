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
Chd|  HM_READ_PROP46                source/properties/spring/hm_read_prop46.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP46(IOUT,NUVAR ,PARGEO, UNITAB,IGTYP,
     .                          ID,PROP_TAG,TITR,LSUBMODEL,IUNIT)
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "tablen_c.inc"
C----------+---------+---+---+--------------------------------------------
C----------+---------+---+---+--------------------------------------------
C
C     This subroutine read the user geometry parameters.
C
C     The geometry datas has to bee stored in radioss storage 
C     with the function SET_U_GEO(value_index,value). 
C
C     If some standard radioss functions (time function or 
C     x,y function) are used, this function IDs has to 
C     bee stored with the function SET_U_PNU(func_index,func_id,KFUNC).
C
C     If this property refers to a user material, this 
C     material IDs has to bee stored with the function 
C     SET_U_PNU(mat_index,mat_id,KMAT).
C
C     If this property refers to a user property, this 
C     sub-property IDs has to bee stored with the function 
C     SET_U_PNU(sub_prop_index,sub_prop_id,KMAT).
C
C     SET_U_GEO and SET_U_PNU return 0 if no error 
C     SET_U_GEO and SET_U_PNU return the maximum allowed index 
C     if index is larger than this maximum
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,NUVAR,IGTYP,ID,IUNIT
      my_real PARGEO(*)
      CHARACTER*nchartitle,
     .   TITR
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC1,IFUNC2,IFUNC3,IFUNC4,IFUNC5,IERROR,KFUNC,
     .   EPSI,IDENS
      my_real
     .  AMAS,ELASTIF,XLIM1,XLIM2,XK,DAMP,FAC_M, FAC_L, FAC_T,FSCALE,
     .  SCALET,SCALEX,SCALEV,SCALEF,R_EPSI,SCALEX_UNIT,SCALEV_UNIT
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C-----------------------------------------------

      INTEGER SET_U_PNU,SET_U_GEO
      EXTERNAL SET_U_PNU,SET_U_GEO
      PARAMETER (KFUNC=29)
      !PARAMETER (KMAT=31)
      !PARAMETER (KPROP=30)
C             
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
      CALL HM_GET_INTV('MAT_IMASS',IDENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('EPSI',EPSI,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('PROP_MASS',AMAS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF0',ELASTIF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('VEL_X',XLIM1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('NFORCE',XLIM2,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF1',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP1',DAMP,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-- Optional ligne
      CALL HM_GET_FLOATV('FScale11',SCALET,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FScale22',SCALEX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FScale21',SCALEV,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FScale12',SCALEF,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('FScale22',SCALEX_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('FScale21',SCALEV_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
C
      IF(.NOT. IS_CRYPTED)THEN
        WRITE(IOUT,1400) ID
      ELSE
        WRITE(IOUT,1500) ID
      ENDIF
C
      FAC_M  = UNITAB%FAC_M(IUNIT)                        
      FAC_L  = UNITAB%FAC_L(IUNIT)
      FAC_T  = UNITAB%FAC_T(IUNIT)
      FSCALE = FAC_M * FAC_L / (FAC_T **TWO)
c        WRITE(IOUT,*) ' My User routine : Debug Start USER'
c        WRITE(IOUT,*) '        *** MAT MUSCLE ***'
      NUVAR     = 4
C
      !default contact stiff
      IF (XK == ZERO)THEN
         XK = ELASTIF
      ENDIF
      PARGEO(1) = ZERO
      PARGEO(2) = XK
C     front wave = 1
      PARGEO(3) = ONE
C
      IF (XLIM1 == ZERO) XLIM1 = EP30*SCALEV_UNIT
      IF (SCALET == ZERO) SCALET = ONE*FAC_T
      IF (SCALEX == ZERO) SCALEX = ONE*SCALEX_UNIT
      IF (SCALEV == ZERO) SCALEV = ONE*SCALEV_UNIT
      IF (SCALEF == ZERO) SCALEF = ONE*FSCALE
C
      IERROR = SET_U_GEO(1,AMAS)
      IERROR = SET_U_GEO(2,ELASTIF)
      IERROR = SET_U_GEO(3,XLIM1)
      IERROR = SET_U_GEO(4,XLIM2)
      IERROR = SET_U_GEO(5,XK)
      IERROR = SET_U_GEO(6,DAMP)
      R_EPSI = EPSI
      IERROR = SET_U_GEO(7,R_EPSI)
      IERROR = SET_U_GEO(8,SCALET)
      IERROR = SET_U_GEO(9,SCALEX)
      IERROR = SET_U_GEO(10,SCALEV)
      IERROR = SET_U_GEO(11,SCALEF)
      IERROR = SET_U_GEO(12,(REAL(IDENS)+EM05))
C
      IFUNC5 = 0
      IERROR = SET_U_PNU(1,IFUNC1,KFUNC)
      IERROR = SET_U_PNU(2,IFUNC2,KFUNC)
      IERROR = SET_U_PNU(3,IFUNC3,KFUNC)
      IERROR = SET_U_PNU(4,IFUNC4,KFUNC)
      IERROR = SET_U_PNU(5,IFUNC5,KFUNC)
C
      IF(.NOT. IS_CRYPTED)THEN
        WRITE(IOUT,1000)
     .     AMAS,ELASTIF,XLIM1,XLIM2,XK,DAMP,EPSI,
     .     IFUNC1,IFUNC2,IFUNC3,
     .     IFUNC4,IDENS,SCALEF,SCALET,SCALEX,SCALEV
      ENDIF
C
C-----------------------------
C       PROPERTY BUFFER 
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
 999  CALL FREERR(1)
      RETURN
C
 1000 FORMAT(
     & 5X,'MASS PER ELEMENT . . . . . . . . . . . . .=',E12.4/,
     & 5X,'STIFFNESS PER UNIT LENGTH. . . . . . . . .=',E12.4/,
     & 5X,'MAXIMUM STRAIN RATE. . . . . . . . . . . .=',E12.4/,
     & 5X,'MAXIMUM FORCE. . . . . . . . . . . . . . .=',E12.4/,
     & 5X,'STIFFNESS FOR INTERFACE. . . . . . . . . .=',E12.4/,
     & 5X,'DAMPING VALUE. . . . . . . . . . . . . . .=',E12.4/,
     & 5X,'ELONGATION CALCULATION FLAG:1=L;0=L/L_o-1 =',I10/,
C     & 5X,' (1: elongation=L , 0(def): =(L / L_0) -1)',E12.4/,
     & 5X,'FORCE VS. TIME ACTIVE FUNCTION ID. . . . .=',I10/,
     & 5X,'FORCE VS. DEFLECTION ACTIVE FUNCTION ID. .=',I10/,
     & 5X,'FORCE VS. VELOCITY ACTIVE FUNCTION ID. . .=',I10/,
     & 5X,'FORCE VS. DEFLECTION PASSIVE FUNCTION ID .=',I10/,
C     & 5X,'DEBUGGING FUNCTION ID. . . . . . . . . . .=',I10/,
     & 5X,'FLAG FOR MASS INPUT. . . . . . . . . . . .=',I10/,
     & 5X,'FORCE SCALE FACTOR . . . . . . . . . . . .=',E12.4/,
     & 5X,'TIME SCALE FACTOR . . . . . . . . . . . .=',E12.4/,
     & 5X,'ELONGATION SCALE FACTOR . . . . . . . . .=',E12.4/,
     & 5X,'VELOCITY SCALE FACTOR . . . . . . . . . .=',E12.4//)
C
 1400 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)
C
 1500 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,
     & 5X,'CONFIDENTIAL DATA'//)
C
      END
