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
Chd|  HM_READ_FAIL_SAHRAEI          source/materials/fail/sahraei/hm_read_fail_sahraei.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_SAHRAEI(UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,    
     .                                NFUNC    ,IFUNC    ,MAXFUNC  ,FAIL_ID  ,
     .                                MAT_ID   ,LSUBMODEL,UNITAB   ,TITR     )
C-----------------------------------------------
c   Sahraei failure criterion (irup = 29)
C-----------------------------------------------
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
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)   :: UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
      INTEGER MAXUPARAM,NUPARAM,NUVAR,NFUNC,FAIL_ID,
     .        MAT_ID,MAXFUNC,IFUNC(MAXFUNC)
      my_real UPARAM(MAXUPARAM) 
      CHARACTER TITR*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NUM,DEN,ORDI,COMP_DIR,IDEL
      my_real 
     .    VOL_STRAIN,MAX_COMP_STRAIN,RATIO,EL_REF,EL_REF_UNIT
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
c=======================================================================
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C-----
c!#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|
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
      UPARAM(1) = VOL_STRAIN
      UPARAM(2) = NUM
      UPARAM(3) = DEN
      UPARAM(4) = ORDI
      IF (COMP_DIR == ZERO) THEN
        UPARAM(5) = ZERO
      ELSE
        UPARAM(5) = COMP_DIR
      ENDIF
      UPARAM(6) = MAX_COMP_STRAIN
      UPARAM(7) = RATIO
      UPARAM(8) = IDEL
      UPARAM(9) = EL_REF
C===================================================================================
C PRINTING OUT DATA
C===================================================================================
      IF (IS_CRYPTED) THEN
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
C-----------       
      RETURN
      END
