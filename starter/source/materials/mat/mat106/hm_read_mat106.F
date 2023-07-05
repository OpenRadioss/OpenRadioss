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
Chd|  HM_READ_MAT106                source/materials/mat/mat106/hm_read_mat106.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT106(UPARAM ,MAXUPARAM,NUPARAM  ,FSMOOTH   , IMATVIS  ,
     .                          NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                          UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                          PM     ,IPM   )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW106 WITH HM READER ( TO BE COMPLETED )
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
      INTEGER, INTENT(INOUT)          :: IPM(NPROPMI),FSMOOTH,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       my_real E, NU, CA, CB, CM, CN, EPSM,
     .        SIGM, TREF, TMELT, TMAX, TOL, FCUT,RHO0,RHOR,CM_UNIT
       my_real 
     .    PMIN, CS, STIFINT, DUMMY
       INTEGER NMAX,ILAW
       LOGICAL IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW= 106

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      !line-2
      CALL HM_GET_FLOATV('MAT_E'      , E      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     , NU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('MLAW106_FCT_ID1' , IFUNC(1)  ,IS_AVAILABLE, LSUBMODEL)   
      CALL HM_GET_INTV('MLAW106_FCT_ID2' , IFUNC(2)  ,IS_AVAILABLE, LSUBMODEL)  
      CALL HM_GET_INTV('MLAW106_FCT_ID3' , IFUNC(3)  ,IS_AVAILABLE, LSUBMODEL)
    
      !line-3
      CALL HM_GET_FLOATV('MAT_SIGY'           , CA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'           , CB     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'           , CN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW106_EP_MAX '    , EPSM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW106_SIGMA_MAX'  , SIGM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_FLOATV('MAT_PC'        ,PMIN      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('MLAW106_NMAX'    ,NMAX      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('MLAW106_TOL'   ,TOL      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5
      CALL HM_GET_FLOATV('MAT_M'       , CM        ,IS_AVAILABLE, LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_TMELT'   , TMELT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX'    , TMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-6 
      CALL HM_GET_FLOATV('MAT_SPHEAT'   , CS     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW106_TR'   , TREF   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------- 
C     DEFAULTS   
C-----------------------------------------------    
      IF(CS == ZERO.AND.TMELT == ZERO)THEN
         CALL ANCMSG(MSGID=593,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MAT_ID,C1=TITR)
         CS=INFINITY
      ELSEIF(CS == ZERO)THEN
         CALL ANCMSG(MSGID=594,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MAT_ID,C1=TITR)
      END IF
      IF(PMIN == ZERO) PMIN  =-INFINITY
C      IF(CN == ZERO.OR.CN == ONE)   CN = ONEP0001
      IF(EPSM == ZERO) EPSM  = INFINITY
      IF(SIGM == ZERO) SIGM  = INFINITY
!!      IF(CC == ZERO)   EPS0  = ONE
      IF(CM == ZERO)   CM    = ONE
      IF(TMELT <= ZERO)TMELT = INFINITY
      IF(TMAX <= ZERO) TMAX  = INFINITY
      IF(TREF <= ZERO) TREF  = THREE100
      IF(NMAX == 0)    NMAX  = 1
      IF(TOL <= ZERO)  TOL   = EM7
      IF(RHOR == ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
C----------------------------------------------- 
C     CHECKS   
C-----------------------------------------------
      IF(E <= ZERO)THEN
        CALL ANCMSG(MSGID=276,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=106,I2=MAT_ID,C1=TITR)   
      ENDIF 
      IF(NU <= -ONE)  THEN       
        CALL ANCMSG(MSGID=300,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=106,I2=MAT_ID,C1=TITR)
      ENDIF 

C----------------------------------------------- 
C     USER MATERIAL PARAMETERS DEFINITION   
C----------------------------------------------- 
      NUPARAM   = 15
      UPARAM(1) = E  
      UPARAM(2) = NU 
      UPARAM(3) = CA 
      UPARAM(4) = CB 
      UPARAM(5) = CM 
      UPARAM(6) = CN 
      UPARAM(7) = TMELT 
      UPARAM(8) = TREF 
      UPARAM(9) = ZERO
      UPARAM(10) = ZERO 
      UPARAM(11) = EPSM 
      UPARAM(12) = SIGM  
      UPARAM(13) = NMAX  
      UPARAM(14) = TOL  
      UPARAM(15) = CS

      NUVAR = 14
      NFUNC = 3  
C     STIFINT   = E/THREE/(ONE-TWO*NU)
      STIFINT   = E*(ONE-NU)/(ONE+NU)/(ONE-TWO*NU)
      PARMAT(1) = STIFINT 
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = 0
!!      PARMAT(5) = FCUT
C ----
      MTAG%G_TEMP = 1
      MTAG%L_TEMP = 1
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      PM(100) = PARMAT(1)
C------------------------------------------------- 
C     LISTING OUTPUT 
C------------------------------------------------- 
      WRITE(IOUT,1010) TRIM(TITR),MAT_ID,106
      WRITE(IOUT,1000)   
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1020)RHO0
        WRITE(IOUT,1100) E,NU,IFUNC(1),IFUNC(2),IFUNC(3),CA,CB,CM,CN,EPSM,SIGM,TMELT,TREF, CS
        WRITE(IOUT,1300) NMAX,TOL
      ENDIF
C
 1000 FORMAT(
     & 5X,'  JOHNSON COOK LAW - TEMPERATURE DEPENDENT YOUNG MODULUS ',/,
     & 5X,'  ------------------------------------------------------ '//)
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1020 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'YOUNG MODULUS E. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO NU . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'FUNCTION E(T) (HEATING). . . . . . . . . . .=',I10/ 
     & 5X,'FUNCTION E(T) (COOLING). . . . . . . . . . .=',I10/ 
     & 5X,'FUNCTION NU(T) . . . . . . . . . . . . . . .=',I10/ 
     & 5X,'YIELD COEFFICIENT A. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD COEFFICIENT B. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD COEFFICIENT M (TEMPERATURE). . . . . .=',1PG20.13/
     & 5X,'YIELD COEFFICIENT N (HARDENING). . . . . . .=',1PG20.13/
     & 5X,'EPS-MAX. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SIG-MAX. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'MELTING TEMPERATURE. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'REFERENCE TEMPERATURE. . . . . . . . . . . .=',1PG20.13/
     & 5X,'SPECIFIC HEAT RHO*CP . . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'MAXIMUM NUMBER OF ITERATIONS . . . . . . . .=',I10/
     & 5X,'PLASTICITY TOLERANCE . . . . . . . . . . . .=',1PG20.13/)     
      RETURN
      END
