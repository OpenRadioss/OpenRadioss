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
Chd|  HM_READ_MAT28                 source/materials/mat/mat028/hm_read_mat28.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT28(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW28 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
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
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: ISRATE,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       my_real 
     . E11,E22,E33,G12,G23,G31,EMX11,EMX22,EMX33,EMX12,EMX23,EMX31,
     . FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,DMIN,DMAX
      INTEGER I11,I22,I33,I12,I23,I31,IF1,IF2
      my_real :: RHOR,RHO0,FAC_UNIT
      LOGICAL :: IS_ENCRYPTED,IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE=0
      IMATVIS=0


      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_EA'       ,E11      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EB'       ,E22      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EC'       ,E33      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_GAB'      ,G12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC'      ,G23      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA'      ,G31      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('FUN_A1'       ,I11      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('FUN_B1'       ,I22      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('FUN_A2'       ,I33      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('Gflag'        ,IF1      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('FScale11'     ,FAC1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22'     ,FAC2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33'     ,FAC3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_EPSR1'    ,EMX11    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR2'    ,EMX22    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR3'    ,EMX33    ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('FUN_A3'       ,I12      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('FUN_B3'       ,I23      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('FUN_A4'       ,I31      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('Vflag'        ,IF2      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('FScale12'     ,FAC4     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale23'     ,FAC5     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale13'     ,FAC6     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_EPSR4'    ,EMX12    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR5'    ,EMX23    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR6'    ,EMX31    ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV_DIM('FScale11' ,FAC_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)      


C     -------------------------
      IF (FAC1 == ZERO) FAC1 = ONE * FAC_UNIT  
      IF (FAC2 == ZERO) FAC2 = ONE * FAC_UNIT  
      IF (FAC3 == ZERO) FAC3 = ONE * FAC_UNIT  
      IF (FAC4 == ZERO) FAC4 = ONE * FAC_UNIT  
      IF (FAC5 == ZERO) FAC5 = ONE * FAC_UNIT  
      IF (FAC6 == ZERO) FAC6 = ONE * FAC_UNIT
      UPARAM(1)=E11
      UPARAM(2)=E22
      UPARAM(3)=E33
      UPARAM(4)=G12
      UPARAM(5)=G23
      UPARAM(6)=G31
      UPARAM(7)=IF1
      UPARAM(8)=IF2
      UPARAM(9) =EMX11
      UPARAM(10)=EMX22
      UPARAM(11)=EMX33
      UPARAM(12)=EMX12
      UPARAM(13)=EMX23
      UPARAM(14)=EMX31
      IF(UPARAM(9) ==ZERO)UPARAM(9) =INFINITY
      IF(UPARAM(10)==ZERO)UPARAM(10)=INFINITY
      IF(UPARAM(11)==ZERO)UPARAM(11)=INFINITY
      IF(UPARAM(12)==ZERO)UPARAM(12)=INFINITY
      IF(UPARAM(13)==ZERO)UPARAM(13)=INFINITY
      IF(UPARAM(14)==ZERO)UPARAM(14)=INFINITY
      UPARAM(15)=FAC1
      UPARAM(16)=FAC2
      UPARAM(17)=FAC3
      UPARAM(18)=FAC4
      UPARAM(19)=FAC5
      UPARAM(20)=FAC6
      NUPARAM = 20
C
      PARMAT(1) = MAX(E11,E22,E33,G12,G23,G31)
C
      IFUNC(1)=I11
      IFUNC(2)=I22
      IFUNC(3)=I33
      IFUNC(4)=I12
      IFUNC(5)=I23
      IFUNC(6)=I31
      NFUNC = 6
C
      NUVAR = 6
CCC     Formulation for solid elements time step computation.
      DMIN  = MIN(E11*E22, E22*E33,E11*E33)
      DMAX = MAX(E11,E22,E33)
      PARMAT(16) = 1
      PARMAT(17) = DMIN/DMAX/DMAX
      
      MTAG%G_PLA = 1
      MTAG%L_PLA = 1
c
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      CALL INIT_MAT_KEYWORD(MATPARAM,"ORTHOTROPIC")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ORTHOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")
c          
      IF(RHOR==ZERO)RHOR=RHO0
      PM(01)=RHOR
      PM(89)=RHO0      
c--------------------------------------------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SMALL_STRAIN")
c--------------------------------------------------
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,33
      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0   
        WRITE(IOUT,1100)E11,E22,E33,G12,G23,G31,I11,I22,I33,I12,I23,I31,FAC1,FAC2,FAC3,FAC4,FAC5,FAC6
       IF(IF1+IF2/=0)WRITE(IOUT,1200)IF1,IF2
       IF(EMX11+EMX22+EMX33+EMX12+EMX23+EMX31/=0)WRITE(IOUT,1300)EMX11,EMX22,EMX33,EMX12,EMX23,EMX31
      ENDIF
C
      RETURN
 1000 FORMAT(
     & 5X,40H  HONEYCOMB LAW                         ,/,
     & 5X,40H  -------------                         ,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT(
     & 5X,'E11 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'E22 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'E33 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G12 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G23 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G31 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS 11 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 22 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 33 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 12 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 23 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 31 FUNCTION NUMBER . . . .=',I10/
     & 5X,'STRESS 11 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 22 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 33 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 12 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 23 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 31 FUNCTION SCALE FACTOR . . . .=',1PG20.13/)
 1200 FORMAT(
     & 5X,'YIELD FUNCTION 11,22,33 FLAG .  . . . .=',I10/
     & 5X,'YIELD FUNCTION 12,23,31 FLAG .  . . . .=',I10/)
 1300 FORMAT(
     & 5X,'TENSION FAILURE STRAIN 11 . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 22 . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 33 . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 12 . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 23 . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 31 . . . . . . . .=',1PG20.13//)


      END


