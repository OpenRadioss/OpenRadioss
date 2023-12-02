Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_MAT103                source/materials/mat/mat103/hm_read_mat103.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT103(UPARAM ,MAXUPARAM,NUPARAM  ,FSMOOTH   , IMATVIS  ,
     .                          NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                          UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                          PM     ,IPM      ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW102 WITH HM READER ( TO BE COMPLETED )
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
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: IPM(NPROPMI),FSMOOTH,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER ILAW
       my_real 
     .  E, NU, G, A0, FCUT, EPS0, PMIN, RCP, TINI, ETA, BULK,
     .  M1, M2, M3, M4, M5, M7,RHO0,RHOR
       LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IPM(3) = 1 ! 
      ILAW = 103

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      !line-2
      CALL HM_GET_FLOATV('MAT_E'     , E      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    , NU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT103_A0'    , A0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT103_M1'    , M1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT103_M2'    , M2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT103_M3'    , M3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT103_M4'    , M4     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT103_M5'    , M5     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT103_M7'    , M7     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4 
      CALL HM_GET_INTV('MAT_FSMOOTH' , FSMOOTH  ,IS_AVAILABLE, LSUBMODEL)      
      CALL HM_GET_FLOATV('Fcut'      , FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'   , EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'    , PMIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5 
      CALL HM_GET_FLOATV('MAT_SPHEAT'  ,RCP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOATV('MAT_T0'    ,TINI  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT103_ETA',ETA  ,IS_AVAILABLE, LSUBMODEL, UNITAB)

C----------------------------------------------- 
C     DEFAULTS   
C----------------------------------------------- 
      IF(PMIN == ZERO) PMIN=-INFINITY
      IF(RCP  == ZERO) RCP = INFINITY 
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
C----------------------------------------------- 
C     CHECKS   
C-----------------------------------------------
      IF(E <= ZERO)THEN
        CALL ANCMSG(MSGID=276,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=103,I2=MAT_ID,C1=TITR)   
      ENDIF 

      IF(NU <= -ONE)  THEN       
        CALL ANCMSG(MSGID=300,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=103,I2=MAT_ID,C1=TITR)
      ENDIF 
C
      ETA=MIN(ONE,ETA)
C----------------------------------------------- 
C     USER MATERIAL PARAMETERS DEFINITION   
C----------------------------------------------- 
      G = E/TWO/(ONE+NU) 
      NUPARAM   = 14
      UPARAM(1) = A0
      UPARAM(2) = ONE/FAC_T_WORK
      UPARAM(3) = G
      UPARAM(4) = M1
      UPARAM(5) = M2
      UPARAM(6) = M3
      UPARAM(7) = M4
      UPARAM(8) = M5
      UPARAM(9) = M7
      UPARAM(10) = RCP
      UPARAM(11) = TINI
      UPARAM(12) = ETA
      UPARAM(13) = 273.15
      UPARAM(14) = EPS0
C    
      NUVAR     = 1  ! temperature
      NFUNC     = 0 
      BULK   = E/THREE/(ONE-TWO*NU)
      PARMAT(1) = BULK 
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = FSMOOTH
      PARMAT(5) = FCUT
      PM(32) = BULK
      PM(37) = PMIN
      PM(69) = RCP
      PM(100)= BULK
C   flag activation   
      MTAG%G_TEMP = 1  ! 
      MTAG%L_TEMP = 1
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
C------------------------------------------------- 
      !MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")
C------------------------------------------------- 
C     LISTING OUTPUT 
C------------------------------------------------- 
      WRITE(IOUT,1010) TRIM(TITR),MAT_ID,103
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1020)RHO0,RHOR
        WRITE(IOUT,1100) E, NU, A0, M1, M2, M3, M4, M5, M7
        WRITE(IOUT,1200) FSMOOTH, FCUT, EPS0, PMIN, RCP, TINI, ETA
      ENDIF
C
 1000 FORMAT(
     & 5X,'  HENSEL SPITTEL MATERIAL                   ',/,
     & 5X,'  ----------------------- ',/,
     & 5X,'                                            ') 
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/) 
 1020 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13)
 1100 FORMAT(
     & 5X,'YOUNG MODULUS. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD PARAMETER A0 . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD PARAMETER M1 . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD PARAMETER M2 . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD PARAMETER M3 . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD PARAMETER M4 . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD PARAMETER M5 . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD PARAMETER M7 . . . . . . . . . . . . .=',1PG20.13/)
 1200 FORMAT(
     & 5X,'STRAIN RATE SMOOTHING FLAG . . . . . . . . .=',I10/
     & 5X,'CUT OFF FREQUENCY STRAIN RATE SMOOTHING . . =',1PG20.13/
     & 5X,'REFERENCE STRAIN . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'PRESSURE CUT-OFF . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SPECIFIC HEAT Rho*Cp . . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL TEMPERATURE K. . . . . . . . . . . .=',1PG20.13/
     & 5X,'HEAT CONVERSION PARAMETER ETA. . . . . . . .=',1PG20.13/)
C
      RETURN      
      
      
      RETURN
      END
