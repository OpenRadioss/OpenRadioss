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
      !||    read_material_models   ../starter/source/materials/read_material_models.F
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    fill_buffer_51_0       ../starter/source/materials/mat/mat051/fill_buffer_51_0.F
      !||    hm_read_eos            ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_fail           ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_leak           ../starter/source/airbag/hm_read_leak.F
      !||    hm_read_mat            ../starter/source/materials/mat/hm_read_mat.F
      !||    hm_read_nonlocal       ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_therm          ../starter/source/materials/therm/hm_read_therm.F
      !||    hm_read_therm_stress   ../starter/source/materials/therm/hm_read_therm_stress.F
      !||    hm_read_visc           ../starter/source/materials/visc/hm_read_visc.F
      !||    ini_eos_vars           ../starter/source/elements/elbuf_init/ini_eos_vars.F
      !||    ini_mat_elem           ../starter/source/materials/mat/ini_mat_elem.F
      !||    ini_mlaw_vars          ../starter/source/elements/elbuf_init/ini_mlaw_vars.F
      !||    multi_check_eos        ../starter/source/multifluid/multi_check_eos.F
      !||    multi_check_psh        ../starter/source/multifluid/multi_check_psh.F
      !||    read_ale_mat           ../starter/source/materials/ale/read_ale_mat.F
      !||    read_euler_mat         ../starter/source/materials/ale/read_euler_mat.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod           ../starter/share/modules1/elbuftag_mod.F
      !||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
      !||    hm_read_mat_mod        ../starter/source/materials/mat/hm_read_mat.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||    table_mod              ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE READ_MATERIAL_MODELS(
     .             MAT_ELEM    ,MLAW_TAG ,FAIL_TAG ,EOS_TAG  ,
     .             BUFMAT      ,BUFLEN   ,IPM      ,PM       ,UNITAB   ,
     .             MULTI_FVM   ,FAILWAVE ,NLOC_DMG ,LSUBMODEL,
     .             TABLE       ,NPC      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is managing all readers related to material model :
C    - Constitutive Laws  : /MAT
C    - Equations of State : /EOS
C    - Failure models     : /FAIL
C    - viscosity          : /VISC
C    - thermal parameters : /HEAT
C    - etc ...
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD
      USE HM_READ_MAT_MOD
c-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "tablen_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr19_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: BUFLEN
      INTEGER,INTENT(IN) :: NPC(SNPC)
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(INOUT) :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(INOUT) :: PM
      my_real ,DIMENSION(SBUFMAT)       , INTENT(INOUT) :: BUFMAT
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
      TYPE(MLAW_TAG_) , DIMENSION(NUMMAT)  , INTENT(INOUT) :: MLAW_TAG
      TYPE(EOS_TAG_)  ,DIMENSION(0:MAXEOS) , INTENT(INOUT) :: EOS_TAG
      TYPE(FAIL_TAG_) ,DIMENSION(0:MAXFAIL), INTENT(INOUT) :: FAIL_TAG
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(FAILWAVE_STR_) :: FAILWAVE 
      TYPE(NLOCAL_STR_) :: NLOC_DMG 
      TYPE(SUBMODEL_DATA) ,INTENT(IN)   :: LSUBMODEL(NSUBMOD)
      TYPE(TTABLE)  ,DIMENSION(NTABLE)  :: TABLE 
      TYPE(MAT_ELEM_) ,INTENT(INOUT)    :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ::  I,J,K,N,IADBUF,ILAW
      LOGICAL IS_ENCRYPTED, IS_AVAILABLE
      my_real ::  T0,SPH,AS,BS,T1,AL,BL
      INTEGER IFUNC(MAXFUNC)
      CHARACTER MESS*40
C-----------------------------------------------
      DATA IADBUF /1/
      DATA MESS/'MATERIAL DEFINITION                     '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C=======================================================================
      CALL INI_MAT_ELEM (MAT_ELEM)
    
      !Set default parameters used in material laws
      CALL INI_MLAW_VARS(MLAW_TAG ,NUMMAT)  ! attention ca ne marchera pas
      CALL INI_EOS_VARS (EOS_TAG)
c----------      
      ILAW    = 0
      BUFLEN  = 0
      ILAG    = 0
      IALE    = 0
      IEULER  = 0
      ITHERM  = 0
      ITURB   = 0
      PM(1:100,NUMMAT)=ZERO
      MULTI_FVM%NBMAT   = 0 !Initialize number of material in law151  
      FAILWAVE%WAVE_MOD = 0 !Initialization of failwave option
      FAILWAVE%NDDL = 0
c----------       
      !PRECONDITION - RETURN
      IF (NUMMAT == 1) THEN
        ILAG=1
        RETURN
      ENDIF
c---------- 
    
      WRITE(IOUT,1000)NUMMAT-1

c-------------------------------------------------------------------
c     /MAT : Material laws (Constitutive law : sigma,epsilon)
c-------------------------------------------------------------------      
      CALL HM_READ_MAT(
     .     MAT_ELEM%MAT_PARAM       ,MLAW_TAG    ,EOS_TAG     ,BUFMAT      ,
     .     BUFLEN      ,IADBUF      ,IPM         ,PM          ,
     .     MULTI_FVM   ,UNITAB      ,LSUBMODEL   ,TABLE)

c-------------------------------------------------------------------
c     /EOS : Equations of State P=P(µ,E)
c-------------------------------------------------------------------
      CALL HM_READ_EOS(MAT_ELEM%MAT_PARAM ,IPM      ,PM       ,BUFMAT   ,
     .                 BUFLEN   ,IADBUF   ,EOS_TAG  ,UNITAB   ,LSUBMODEL)

c-------------------------------------------------------------------
c     /FAIL : Failure Models
c-------------------------------------------------------------------     
      CALL HM_READ_FAIL(MAT_ELEM%MAT_PARAM,NUMMAT,MAXFAIL  ,FAIL_TAG,
     .                  NTABLE  ,TABLE    ,FAILWAVE ,NLOC_DMG,
     .                  UNITAB  ,LSUBMODEL)
     
c-------------------------------------------------------------------
c     /VISC : Visco elastic  Models
c-------------------------------------------------------------------    
      CALL HM_READ_VISC(MAT_ELEM%MAT_PARAM ,IPM     ,BUFMAT, 
     .                  UNITAB  ,LSUBMODEL ,TABLE   )

c-------------------------------------------------------------------
c     /LEAK : Fabric Leakage Models
c-------------------------------------------------------------------
      CALL HM_READ_LEAK(IPM, PM, UNITAB, LSUBMODEL)

c-------------------------------------------------------------------
C     /ALE/MAT
c-------------------------------------------------------------------
        CALL READ_ALE_MAT(LSUBMODEL, UNITAB, IPM, PM)

c-------------------------------------------------------------------
C     /EULER/MAT
c-------------------------------------------------------------------
        CALL READ_EULER_MAT(LSUBMODEL, UNITAB, IPM, PM)
        
c-------------------------------------------------------------------
c     /MAT/LAW51 - FILL BUFFER (modern input)
c-------------------------------------------------------------------
      CALL FILL_BUFFER_51_0(MAT_ELEM%MAT_PARAM, IPM, PM, BUFMAT)

c-------------------------------------------------------------------
c     /HEAT/MAT : thermal parameters
c-------------------------------------------------------------------
      CALL HM_READ_THERM(BUFMAT, BUFLEN, IPM, PM, UNITAB, LSUBMODEL)

c-------------------------------------------------------------------
c     /THERM/STRESS : thermal expansion
c-------------------------------------------------------------------
      CALL HM_READ_THERM_STRESS(MLAW_TAG, IPM, PM, UNITAB, LSUBMODEL)

c-------------------------------------------------------------------
c     /NONLOCAL : Non-local regularization
c------------------------------------------------------------------
      CALL HM_READ_NONLOCAL(MAT_ELEM%MAT_PARAM,NLOC_DMG ,MLAW_TAG ,IPM, UNITAB, LSUBMODEL)

c-------------------------------------------------------------------
c      ALE EULER SPECIFIC TREATMENTS 
c-------------------------------------------------------------------
      
      !SPECIFIC TREATMENTS FOR LAW151 MATERIAL LAWS RELATED TO PSH PARAMETERS & EOS
      IF (MULTI_FVM%IS_USED) THEN
         !Check pressure shift consistency between submaterials
         !They must be equal one to another, otherwise, throw an error
         CALL MULTI_CHECK_PSH(MULTI_FVM, NUMMAT, NPROPMI, NPROPM, IPM, PM)
         CALL MULTI_CHECK_EOS(MULTI_FVM, NUMMAT, NPROPMI, NPROPM, IPM, PM)
      ENDIF   

C------------------------------
      RETURN
c------------------------------
 1000 FORMAT(//
     .    '    MATERIAL LAWS'/
     .    '    -------------'/,
     .    '    NUMBER OF MATERIALS. . . . . . . . . =',I10//)
c------------------------------
      END
