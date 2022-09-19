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
Chd|  HM_READ_MAT64                 source/materials/mat/mat064/hm_read_mat64.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT64(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   ,
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW64 WITH HM READER 
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
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    E,NU,CP,VMC,D,N,MD,
     .    C1, G,TEMP,V0,C2,YFAC(2),FAC_L,FAC_T,FAC_M,FAC_C
      INTEGER I,J
      my_real :: RHO0, RHOR, YFAC_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 0
      IMATVIS = 0
      NUPARAM = 15
      NUVAR = 7
      NFUNC=2
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'      ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'          ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'         ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CP'             ,CP       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT_D'          ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N'          ,N        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('M64_Md'         ,MD       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('M64_Vo'         ,V0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('M64_Vm'         ,VMC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_INTV  ('M64_FUNCT_ID_0' ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('M64_FUNCT_ID_1' ,IFUNC(2) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('M64_SCALE_0'    ,YFAC(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('M64_SCALE_1'    ,YFAC(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('M64_INI_TEMP'   ,TEMP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !CALL HM_GET_FLOATV(''       ,HL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)   no longer read
      !units
      CALL HM_GET_FLOATV_DIM('M64_SCALE_0'    ,YFAC_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      !---DEFAULT VALUES---!
      IF(RHOR == ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
      IF(V0 == ZERO.OR.V0 >= ONE)V0=EM20
      IF(CP == ZERO)THEN
       CP = ZERO
      ELSE
       CP = ONE/CP
      ENDIF
      IF(YFAC(1) == ZERO) YFAC(1)= ONE*YFAC_UNIT
      IF(YFAC(2) == ZERO) YFAC(2)= ONE*YFAC_UNIT
      IF(VMC == ZERO.OR.VMC > ONE) VMC = ONE

      !---STORAGE---!
      G = HALF*E/(ONE + NU)
      C1=E/THREE/(ONE - TWO*NU)
      UPARAM(1)  = E
      UPARAM(2)  = E/(ONE - NU*NU)
      UPARAM(3)  = NU*UPARAM(2)
      UPARAM(4)  = G
      UPARAM(5)  = NU
      UPARAM(6)  = D
      UPARAM(7)  = N
      UPARAM(8)  = MD
      UPARAM(9)  = V0
      UPARAM(10) = VMC
      UPARAM(11) = CP
      UPARAM(12) = TEMP
      UPARAM(13) = YFAC(1)
      UPARAM(14) = YFAC(2)
      UPARAM(15) = ZERO !HL

      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      !Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)

       !---LISTING OUTPUT---!
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,64
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100)E,NU,CP,N,D,V0,MD,VMC,TEMP
        WRITE(IOUT,1200)(IFUNC(J),YFAC(J),J=1,2)
      ENDIF

      RETURN
 1000 FORMAT(/
     . 5X,40H  TRIP-STEELS  PLASTIC LAW              ,/,
     & 5X,40H  ------------------------              ,/)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS. . . . . . . . . . . . . . .  =',E12.4/
     & 5X,'POISSON''S RATIO. . . . . . . . . . . . . . .  =',E12.4/
     & 5X,'HEAT CAPACITIVITY  . . .. . . . . . . . . . . =',E12.4/
C     & 5X,'HEAT LATENT OF PHASE CHANGE . . . . . . . . . =',E12.4/
     & 5X,'MATERIAL PARAMETER  (n)      . . . . . . . . .=',E12.4/
     & 5X,'MATERIAL PARAMETER  (D)     . . . . .. . . . .=',E12.4/
     & 5X,'MATERIAL PARAMETER  (V0)     . . . . .. . . . =',E12.4/
     & 5X,'LIMIT MARTENISTE TEMPERATURE (Md) . . . . . . =',E12.4/
     & 5X,'CONTENT MARTENSITE VOLUME FRACTION(Vmc) . . . =',E12.4/
     & 5X,'INITIAL TEMPERATURE . .  . . . . . . . . . .  =',E12.4)
 1200 FORMAT(
     & 5X,'YIELD STRESS FUNCTION NUMBER. . . . . . . . . =',I8/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . . . =',E12.4)


      END SUBROUTINE



