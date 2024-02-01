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
Chd|====================================================================
Chd|  HM_READ_MAT18                 source/materials/mat/mat018/hm_read_mat18.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT18(
     .           NUPARAM  ,NUVAR    ,NFUNC    ,MAT_ID   ,TITR      ,
     .           UNITAB   ,LSUBMODEL,MTAG     ,PM       ,IPM       ,
     .           JTHE     ,MATPARAM ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW18 WITH HM READER
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle ,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL
      my_real, DIMENSION(*), INTENT(IN) :: UNITAB
      INTEGER, INTENT(IN) :: MAT_ID
      INTEGER, INTENT(INOUT) :: IPM(NPROPMI)
      my_real, INTENT(INOUT) :: PM(NPROPM)     
      INTEGER, INTENT(INOUT) :: NUPARAM,NUVAR,NFUNC,JTHE
      TYPE(MLAW_TAG_) ,INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: ILAW,ITF,ISPH,IAS
      my_real :: RHOR,RHO0,T0,SPH,AS,BS,E0,TIMESCAL,TSCAL,ESCAL,KSCAL
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 18
      JTHE = 1
c--------------------------------------------------
c     Check crypting option
c--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
c--------------------------------------------------
c     Read input Parameters
c--------------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_SPHEAT',SPH      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A'     ,AS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'     ,BS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('Xt_fun'    ,ITF      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('MAT_T0'    ,T0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SCALE'     ,TIMESCAL ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('FUN_A1'    ,ISPH     ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('FUN_A2'    ,IAS      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('FScale11'  ,TSCAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22'  ,ESCAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33'  ,KSCAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c--------------------------------------------------
c     DEFAULT VALUES
c--------------------------------------------------
c
      IF (TIMESCAL == ZERO) THEN
        TIMESCAL = ONE
      ENDIF
      IF (TSCAL == ZERO) THEN
        TSCAL = ONE
      ENDIF
      IF (ESCAL == ZERO) THEN
        ESCAL = ONE
      ENDIF
      IF (KSCAL == ZERO) THEN
        KSCAL = ONE
      ENDIF
      TIMESCAL = ONE / TIMESCAL
      ESCAL    = ONE / ESCAL
      IF (T0 == ZERO) T0 = THREE100
      E0 = T0*SPH
c
c------------------
      NFUNC   = 0
      NUPARAM = 0
      NUVAR   = 0
c------------------
c
      IPM(10)= 3    ! Nfunc
      IPM(11)= ITF
      IPM(12)= ISPH
      IPM(13)= IAS
c
      PM(23) = E0
      PM(41) = TIMESCAL
      PM(42) = TSCAL
      PM(43) = ESCAL
      PM(44) = KSCAL
      PM(71) = JTHE+EM01
      PM(72) = ZERO
      PM(69) = SPH
      PM(75) = AS
      PM(76) = BS
      PM(79) = T0
      PM(80) = INFINITY
c---
      PM(1)  = RHOR
      PM(89) = RHO0
c--------------------------
c
c---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_TEMP   = 1
      MTAG%G_DELTAX = 1
      MTAG%L_TEMP   = 1
      MTAG%L_DELTAX = 1

      ! MATPARM keywords

      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ORTHOTROPIC")
c--------------------------------------------------
c     Starter output
c--------------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,RHOR
        IF (ITF > 0) WRITE(IOUT,1300) ITF
        WRITE(IOUT,1400) T0,SPH,AS,BS,ISPH,IAS
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,'    THERMAL MATERIAL  ',/,
     & 5X,'--------------------  ',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY  . . . . . . . . . . . . . .=',1PG20.13/,  
     & 5X,'REFERENCE DENSITY. . . . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'TEMPERATURE LOAD CURVE . . . . . . . . . . .=',I10/)
 1400 FORMAT(
     & 5X,'T0 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPECIFIC HEAT. . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'AS . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BS . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'CONDUCTIVITY CURVE NUMBER. . . . . . . . . .=',I10/,
     & 5X,'TEMPERATURE VERSUS ENERGY CURVE NUMBER . . .=',I10/)
c--------------------------------------------------
      RETURN
      END
