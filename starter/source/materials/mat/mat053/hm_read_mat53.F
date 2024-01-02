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
Chd|  HM_READ_MAT53                 source/materials/mat/mat053/hm_read_mat53.F
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
      SUBROUTINE HM_READ_MAT53(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW53 WITH HM READER
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
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
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .      E11, E22, N12, G12, G23, G13,FAC1,FAC2,FAC3,FAC4,FAC5,
     .      FAC_L,FAC_T,FAC_M,FAC_C,DMIN,DMAX,SSP
      INTEGER NRATE,J,I,IFLAG
      my_real :: RHO0, RHOR, FAC_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 0
      IMATVIS = 0
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      
      NUPARAM=10
      NFUNC = 5
      NUVAR = 6
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'      ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E1'         ,E11      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E2'         ,E22      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT_GAB'        ,G12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC'        ,G23      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_INTV  ('FUN_A1'         ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B1'         ,IFUNC(2) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A3'         ,IFUNC(3) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A5'         ,IFUNC(4) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A6'         ,IFUNC(5) ,IS_AVAILABLE, LSUBMODEL)
      IFLAG=0 !no longer read
      !line-5
      CALL HM_GET_FLOATV('MAT_SFAC11'     ,FAC1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC22'     ,FAC2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC12'     ,FAC3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC23'     ,FAC4     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC45'     ,FAC5     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !units
      CALL HM_GET_FLOATV_DIM('MAT_SFAC11' ,FAC_UNIT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        

      !---DEFAULT VALUES
      IF(RHOR == ZERO)RHOR=RHO0
      PM(1)  = RHOR
      PM(89) = RHO0  
      SSP    = SQRT(MAX(E11,E22,G12,G23)/RHO0)
      PM(27) = SSP
      IF (FAC1 == ZERO) FAC1 = ONE*FAC_UNIT
      IF (FAC2 == ZERO) FAC2 = ONE*FAC_UNIT
      IF (FAC3 == ZERO) FAC3 = ONE*FAC_UNIT
      IF (FAC4 == ZERO) FAC4 = ONE*FAC_UNIT
      IF (FAC5 == ZERO) FAC5 = ONE*FAC_UNIT

      !---STORAGE
      UPARAM(1)=E11
      UPARAM(2)=E22      
      UPARAM(3)=G12
      UPARAM(4)=G23
      UPARAM(5)=IFLAG
      UPARAM(6)=FAC1
      UPARAM(7)=FAC2
      UPARAM(8)=FAC3
      UPARAM(9)=FAC4
      UPARAM(10)=FAC5      
      !Formulation for solid elements time step computation.
      PARMAT(1)  = MAX(E11,E22,G12,G23)
      PARMAT(2)  = MAX(E11,E22)
      PARMAT(16) = 1
      DMIN = E11*E22   
      DMAX = MAX(E11,E22)
      PARMAT(17) = DMIN/DMAX/DMAX
c
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      CALL INIT_MAT_KEYWORD(MATPARAM,"ORTHOTROPIC")
c
      ! Properties compatibility       
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ORTHOTROPIC")  
c
      !---LISTING OUTPUT
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,53
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100)E11,E22,G12,G23,(IFUNC(I),I=1,5)
        WRITE(IOUT,1200)FAC1,FAC2,FAC3,FAC4,FAC5
      ENDIF
      
      !---OUTPUT FORMAT

 1000 FORMAT(
     & 5X,30H   /MAT/LAW53 (TSAI_TAB)                ,/,
     & 5X,30H  -------------                         ,//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'E11 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'E22 . . . . . . . . . . . . . . . . . .=',1PG20.13/   
     & 5X,'G12 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G23 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS 11 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 22 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 12 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 23 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 45 FUNCTION NUMBER . . . .=',I10)
 1200 FORMAT(
     & 5X,'STRESS 11 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 22 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 33 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 12 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 23 FUNCTION SCALE FACTOR . . . .=',1PG20.13/
     & 5X,'STRESS 45 FUNCTION SCALE FACTOR . . . .=',1PG20.13//)


      RETURN
      END SUBROUTINE

