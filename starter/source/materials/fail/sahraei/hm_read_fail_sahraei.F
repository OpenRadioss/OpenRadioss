Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    hm_read_fail_sahraei     ../starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_SAHRAEI(    
     .           FAIL    ,MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Sahraei failure criterion (irup = 29)
C-----------------------------------------------
C    M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER            ,INTENT(IN) :: FAIL_ID         ! failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID          ! material law ID
      INTEGER            ,INTENT(IN) :: IRUPT           ! failure model type number
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB          ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)    ! submodel table
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL         ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NUM,DEN,ORDI,COMP_DIR,IDEL,NUPARAM,NUVAR,NFUNC
      INTEGER ,PARAMETER :: NSIZE = 2
      INTEGER ,DIMENSION(NSIZE) :: IFUNC
      my_real :: VOL_STRAIN,MAX_COMP_STRAIN,RATIO,EL_REF,EL_REF_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
c!#
c!# NUMERATOR    =  1  ==> Eps_xx 
c!# NUMERATOR    =  2  ==> Eps_yy 
c!# NUMERATOR    =  3  ==> Eps_zz 
c!# NUMERATOR    =  4  ==> Eps_1   1st. 3d-principal
c!# NUMERATOR    =  5  ==> Eps_2   2nd. 3d-principal
c!# NUMERATOR    =  6  ==> Eps_3   3rd. 3d-principal
c!#
c!# Denominator  =  1  ==> 2D e1 = x-z - plane 
c!# Denominator  =  2  ==> 2D e1 = x-y - plane 
c!# Denominator  =  3  ==> 2D e1 = y-z - plane 
c!# Denominator  =  4  ==> Eps_1   1st. 3d-principal
c!# Denominator  =  5  ==> Eps_2   2nd. 3d-principal 
c!# Denominator  =  6  ==> Eps_3   3rd. 3d-principal 
c!#
c!# Ordinate     =  1  ==> MAX(Eps_xx,Eps_yy,Eps_zz)
c!# Ordinate     =  2  ==> Eps_xx
c!# Ordinate     =  3  ==> Eps_yy
c!# Ordinate     =  4  ==> Eps_zz
c!# Ordinate     =  5  ==> Eps_1   1st. 3d-principal
c!# Ordinate     =  6  ==> 2D - e1 = x-z - plane
c!# Ordinate     =  7  ==> 2D - e1 = x-y - plane
c!# Ordinate     =  8  ==> 2D - e1 = y-z - plane
c!#
c!# COMP_DIR     = 0   ==> Feature is not taken into account
c!# COMP_DIR     = 1   ==> Normal to layers is in xx
c!# COMP_DIR     = 2   ==> Normal to layers is in yy
c!# COMP_DIR     = 3   ==> Normal to layers is in zz
c!#
c!# MAX_COMP_STRAIN = In-plane failure compression strain (not deleting, only damage=1)
c!# if value is negative, the element will NOT be deleted, only DAMAGE set to 1
c!# RATIO        = RATIO of the two other failure strains (default = 1.0)
c!# 
C===================================================================================
C READING CARD 1
C===================================================================================
      CALL HM_GET_INTV   ('Fct_Ratio'      ,IFUNC(1)        ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV   ('NUM'            ,NUM             ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV   ('DEN'            ,DEN             ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('ORDI'           ,ORDI            ,IS_AVAILABLE,LSUBMODEL)  
      IF (ORDI == 0) ORDI = 1
      CALL HM_GET_FLOATV ('Vol_strain'     ,VOL_STRAIN      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('Fct_Elsize'     ,IFUNC(2)        ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('EL_REF'         ,EL_REF          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((EL_REF == ZERO).AND.(IFUNC(2) > 0)) THEN 
        CALL HM_GET_FLOATV_DIM('EL_REF' ,EL_REF_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        EL_REF = ONE*EL_REF_UNIT
      ENDIF 
C===================================================================================
C READING CARD 2 
C===================================================================================    
      CALL HM_GET_INTV   ('Comp_dir'       ,COMP_DIR        ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV   ('IDEL'           ,IDEL            ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_FLOATV ('Max_Comp_Strain',MAX_COMP_STRAIN ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MAX_COMP_STRAIN == ZERO) MAX_COMP_STRAIN = INFINITY
      CALL HM_GET_FLOATV ('Ratio'          ,RATIO           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (RATIO == ZERO) RATIO = ONE
C===================================================================================
C NUMBER OF PARAMETERS,FUNCTIONS, AND USER VARIABLE
C===================================================================================
      NUPARAM = 9
      IF (IFUNC(2) > 0) THEN
        NFUNC = 2
        NUVAR = 1
      ELSE
        NFUNC = 1
        NUVAR = 0
      ENDIF
C===================================================================================
C STORING PARAMETERS
C===================================================================================
      FAIL%KEYWORD = 'SAHRAEI' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = NUPARAM
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = NUVAR
      FAIL%NFUNC   = NFUNC
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
      
      FAIL%IFUNC(1:NFUNC) = IFUNC(1:NFUNC)
c
      FAIL%UPARAM(1) = VOL_STRAIN
      FAIL%UPARAM(2) = NUM
      FAIL%UPARAM(3) = DEN
      FAIL%UPARAM(4) = ORDI
      IF (COMP_DIR == ZERO) THEN
        FAIL%UPARAM(5) = ZERO
      ELSE
        FAIL%UPARAM(5) = COMP_DIR
      ENDIF
      FAIL%UPARAM(6) = MAX_COMP_STRAIN
      FAIL%UPARAM(7) = RATIO
      FAIL%UPARAM(8) = IDEL
      FAIL%UPARAM(9) = EL_REF
C===================================================================================
C PRINTING OUT DATA
C===================================================================================
      IF (IS_ENCRYPTED) THEN
        WRITE (IOUT,'(5X,A,//)') 'CONFIDENTIAL DATA'
      ELSE 
        WRITE (IOUT, 1000) IFUNC(1),VOL_STRAIN
        WRITE (IOUT, 2000) NUM,DEN,ORDI
        IF (IFUNC(2) > 0) THEN 
          WRITE (IOUT, 3000) IFUNC(2),EL_REF
        ENDIF
        IF (COMP_DIR /= ZERO) THEN
          WRITE (IOUT, 4000) COMP_DIR, IDEL, MAX_COMP_STRAIN, RATIO
        ENDIF
      ENDIF
C===================================================================================
C PRINTOUT FORMAT
C===================================================================================
 1000 FORMAT(
     & 5X,'    SAHRAEI LOCAL ELECTRIC BATTERY FAILURE      ',/,
     & 5X,'    --------------------------------------      ',/,
     & 5X,'STRAIN RATIO FUNCTION ID . . . . . . . . . . . . .=',I10/,
     & 5X,'VOLUMETRIC STRAIN LIMIT. . . . . . . . . . . . . .=',1PG20.13/)
 2000 FORMAT(
     & 5X,'NUMERATOR FLAG FOR STRAIN RATIO. . . . . . . . . .=',I10/,
     & 5X,'DENOMINATOR FLAG FOR STRAIN RATIO. . . . . . . . .=',I10/,
     & 5X,'FAILURE ORDINATE FOR FAILURE LIMIT . . . . . . . .=',I10/)
 3000 FORMAT(
     & 5X,'ELEMENT SIZE REGULARIZATION FUNCTION ID. . . . . .=',I10/,
     & 5X,'ELEMENT REFERENCE SIZE . . . . . . . . . . . . . .=',1PG20.13/)
 4000 FORMAT(
     & 5X,'NORMAL IN-PLANE COMPRESSION DIRECTION. . . . . . .=',I10/,
     & 5X,'FLAG FOR ELEMENT DELETION IN COMPRESSION . . . . .=',I10/,
     & 5X,'IN-PLANE COMPRESSION FAILURE STRAIN  . . . . . . .=',1PG20.13/,
     & 5X,'RATIO FOR IN-PLANE 2ND DIRECTION FAILURE STRAIN . =',1PG20.13/)
C-----------
      RETURN
      END
