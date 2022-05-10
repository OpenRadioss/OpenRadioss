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
Chd|  HM_READ_PROP_USER4            source/properties/user_spring_solid/hm_read_prop_user4.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP_USER4(IOUT  ,NUVAR ,PARGEO ,UNITAB ,LSUBMODEL)
C-----------------------------------------------
C     This routine may be used for user defined rupture property 
C     for interface type 2
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr15_c.inc"
!#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IOUT
      INTEGER NUVAR
      my_real   
     .        PARGEO(*)

      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SET_U_PNU,SET_U_GEO
C-----------------------------------------------
C   P a r a m e t e r s
C-----------------------------------------------
      INTEGER KFUNC,KMAT,KPROP
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR,IDEBUG,IFUNN,IFUNT,IFUNS,IRUPT,IFILTR
      my_real
     .  SCAL_F,SCAL_D,SCAL_SR,DNMAX,DTMAX,ALPHA,RUPT,DEBUG,FILTR
!
      LOGICAL IS_AVAILABLE
C=======================================================================
C---  CARD1 :  
!!      READ(IIN,ERR=999,FMT='(4F20.0)')SCAL_F,SCAL_D,SCAL_SR,ALPHA
C---  CARD2 :  
!!      READ(IIN,ERR=999,FMT='(6I10,2F20.0)')IRUPT,IDEBUG,IFILTR,IFUNS,
!!     .                                     IFUNN,IFUNT,DNMAX,DTMAX
C---
      IS_AVAILABLE = .FALSE.
!---
C---  CARD1 :
      CALL HM_GET_FLOATV('F_scale_stress',SCAL_F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('F_scale_strrate',SCAL_SR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('F_scale_dist',SCAL_D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Alpha',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
C---  CARD2 :
      CALL HM_GET_INTV('RUPT',IRUPT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('DEBUG',IDEBUG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('LFILTR',IFILTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUNCT_ID_sr',IFUNS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUNCT_ID_sn',IFUNN,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUNCT_ID_st',IFUNT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAX_N_DIST',DNMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_T_DIST',DTMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
!---
      IF (SCAL_F  == 0.0) SCAL_F  = UN
      IF (SCAL_D  == 0.0) SCAL_D  = UN
      IF (SCAL_SR == 0.0) SCAL_SR = UN
      IF (DNMAX == 0.0) DNMAX = EP20
      IF (DTMAX == 0.0) DTMAX = EP20
      IF (IFILTR == 1) THEN
        IF (ALPHA  == ZERO) ALPHA  = UN
        ALPHA = MIN(ALPHA,UN)
        ALPHA = MAX(ALPHA,ZERO)
      ENDIF
      IF (IFUNN == 0 .OR. IFUNT == 0) GOTO 999
C
      RUPT  = IRUPT
      DEBUG = IDEBUG
      FILTR = IFILTR
C
      IERROR = SET_U_GEO( 2,SCAL_F)
      IERROR = SET_U_GEO( 3,SCAL_D)
      IERROR = SET_U_GEO( 4,SCAL_SR)
      IERROR = SET_U_GEO( 5,DNMAX)
      IERROR = SET_U_GEO( 6,DTMAX)
      IERROR = SET_U_GEO( 7,ALPHA)
      IERROR = SET_U_GEO( 8,RUPT)
      IERROR = SET_U_GEO( 9,DEBUG)
      IERROR = SET_U_GEO(10,FILTR)
C
      IERROR = SET_U_PNU(1,IFUNN,KFUNC)
      IERROR = SET_U_PNU(2,IFUNT,KFUNC)
      IERROR = SET_U_PNU(3,IFUNS,KFUNC)
C
C     this is the number of user variables per slave node used in engine:
C
      NUVAR = 2
C------------------------------
      WRITE(IOUT,1000)
      WRITE(IOUT,1100) SCAL_F,SCAL_D,SCAL_SR,ALPHA,
     .     DNMAX,DTMAX,IFUNN,IFUNT,IFUNS,IRUPT,IFILTR,IDEBUG
C------------------------------
      RETURN
999   WRITE(IOUT,*)' **ERROR IN USER INTERFACE PROPERTY INPUT'
      CALL MY_EXIT(2)
C-----
 1000 FORMAT(
     . '      USER INTERFACE RUPTURE PARAMETERS '/
     . '      ----------------------  '/)
 1100 FORMAT(/10X,'SCAL_F . . . . . . . . . .',1PG20.13
     .       /10X,'SCAL_DISP. . . . . . . . .',1PG20.13
     .       /10X,'SCAL_SR  . . . . . . . . .',1PG20.13
     .       /10X,'FILTERING COEFF. . . . . .',1PG20.13
     .       /10X,'DN_MAX . . . . . . . . . .',1PG20.13
     .       /10X,'DT_MAX . . . . . . . . . .',1PG20.13
     .       /10X,'IFUNN    . . . . . . . .',I10
     .       /10X,'IFUNT    . . . . . . . .',I10
     .       /10X,'IFUNS    . . . . . . . .',I10
     .       /10X,'IRUPT    . . . . . . . .',I10
     .       /10X,'IFILTR   . . . . . . . .',I10
     .       /10X,'IDEBUG . . . . . . . . .',I10//)
C=================================================================
      RETURN
      END
