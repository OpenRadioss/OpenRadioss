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
Chd|  HM_READ_MAT102                source/materials/mat/mat102/hm_read_mat102.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT102(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                          NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                          UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                          PM     ,MATPARAM )
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
      my_real   :: E,NU,C,PSTAR,AMAX,G, DELTA,STIFINT,PMIN,BID,PHI_DEG
      my_real   :: A0,A1,A2,RHO0,RHOR
  
      CHARACTER :: STRING*ncharfield
      CHARACTER ::  KEY*nchartitle,KEYNET*ncharfield      
      INTEGER   :: IFORM
      
      DOUBLE PRECISION :: PHI,K,ALPHA
      
      LOGICAL :: IS_ENCRYPTED,IS_AVAILABLE
      
      CHARACTER*64 :: CHAIN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_INTV  ('IFORM',IFORM  ,IS_AVAILABLE, LSUBMODEL) 
      
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_E'        ,E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,NU    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT102_C'     ,C     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT102_PHI'   ,PHI   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT102_AMAX'  ,AMAX  ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT102_PMIN'  ,PMIN    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
 
      !   C   : Mohr-Coulomb cohesion (intercept) (in Pa)
      ! PHI   : Mohr-Coulomb angle of internal friction (angle, degree)
      ! Iform : Formulation Flag (circumscribed,inscribed,middle)

C----------------------------------------------- 
C     UNITS   
C----------------------------------------------- 
      PHI_DEG = PHI
      PHI     = PHI*3.141592653589793238D00/180.D00
C----------------------------------------------- 
C     DEFAULTS   
C----------------------------------------------- 
      IF(IFORM<=0 .OR. IFORM>=4)IFORM=2
C----------------------------------------------- 
C     YIELD PARAMETERS   
C-----------------------------------------------
      G=E/TWO/(ONE+NU) 
      SELECT CASE(IFORM)
        CASE(1)
          K     = SIX*C*COS(PHI)/SQRT(THREE)/(THREE-SIN(PHI))
          ALPHA = TWO*SIN(PHI)/SQRT(THREE)/(THREE-SIN(PHI))
        CASE(2)
          K     = SIX*C*COS(PHI)/SQRT(THREE)/(THREE+SIN(PHI))
          ALPHA = TWO*SIN(PHI)/SQRT(THREE)/(THREE+SIN(PHI))
        CASE(3)
          K     = THREE*C*COS(PHI)/SQRT(NINE+THREE*SIN(PHI)*SIN(PHI))
          ALPHA = SIN(PHI)/SQRT(NINE+THREE*SIN(PHI)*SIN(PHI))
      END SELECT
      A0 = K*K
      A1 = SIX*K*ALPHA
      A2 = NINE*ALPHA*ALPHA

      IF(E<=ZERO)THEN
        chain='YOUNG MODULUS MUST BE DEFINED                           '
        CALL ANCMSG(MSGID=829,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=10,
     .                I2=MAT_ID,
     .                C1='ERROR',
     .                C2=TITR,
     .                C3=chain)   
      ENDIF 

      IF(NU<=ZERO)THEN
        chain='POISSON RATIO MUST BE DEFINED                           '
        CALL ANCMSG(MSGID=829,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=10,
     .                I2=MAT_ID,
     .                C1='ERROR',
     .                C2=TITR,
     .                C3=chain)   
      ENDIF 

      PSTAR = -INFINITY
      IF(A2==ZERO .AND. A1/=ZERO)THEN 
        PSTAR=-A0/A1 
      ELSEIF(A2/=ZERO)THEN
        DELTA = A1*A1-FOUR*A0*A2
        !Si intersection avec l'axe
        IF(DELTA >= ZERO)THEN
          DELTA=SQRT(DELTA)
          PSTAR = (-A1+DELTA)/TWO/A2
        
        ELSE
          PSTAR = -A1/TWO/A2 ! extremum          
    !      chain='FITTED YIELD FUNCTION HAS NO ROOT. CHECK INPUT PARAMETER        '      
    !      CALL ANCMSG(  MSGID=829,
    ! .                  MSGTYPE=MSGWARNING,
    ! .                  ANMODE=ANINFO,
    ! .                  I1=10,
    ! .                  I2=MAT_ID,
    ! .                  C1='WARNING',
    ! .                  C2=TITR,
    ! .                  C3=chain)               
        ENDIF
      ELSE
        !do nothing let user do what he wants
        PSTAR = -INFINITY
      ENDIF
C
      IF(AMAX==ZERO) AMAX = INFINITY
      IF(PMIN==ZERO) PMIN =-INFINITY

      RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
      PM(37)= PMIN
      
      ISRATE=0

C----------------------------------------------- 
C     USER MATERIAL PARAMETERS DEFINITION   
C----------------------------------------------- 
      NUPARAM   = 11
      UPARAM(1) = C
      UPARAM(2) = PHI
      UPARAM(3) = PSTAR
      UPARAM(4) = A0
      UPARAM(5) = A1
      UPARAM(6) = A2
      UPARAM(7) = AMAX
      UPARAM(8) = G
      UPARAM(9) = IFORM
      UPARAM(10)= E
      UPARAM(11)= NU
      NUVAR     = 0 
      NFUNC     = 0 
      STIFINT   = E
      PARMAT(1) = STIFINT/THREE !
      PARMAT(2) = STIFINT
      PARMAT(3) = NU
c---------------------------   
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
c
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
C------------------------------------------------- 
C     LISTING OUTPUT 
C------------------------------------------------- 
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,102
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002)RHO0,RHOR    
        WRITE(IOUT,1100)E,NU,C,PHI_DEG,PMIN
        WRITE(IOUT,1200)IFORM
        SELECT CASE(IFORM)
          CASE(1)
            WRITE(IOUT,1201)
          CASE(2)
            WRITE(IOUT,1202)
          CASE(3)
            WRITE(IOUT,1203)
          CASE(4)
            WRITE(IOUT,1204)
        END SELECT
        WRITE(IOUT,1300)A0,A1,A2,AMAX,PSTAR
      ENDIF
C
 1000 FORMAT(
     & 5X,'  EXTENDED DRUCKER-PRAGER MATERIAL (DPRAG2) ',/,
     & 5X,'  ----------------------------------------- ')
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . .=',1PG20.13/)  
 1100 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'COHESION. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'ANGLE OF INTERNAL FRICTION. . . . . . .=',1PG20.13/
     & 5X,'MINIMUM PRESSURE. . . . . . . . . . . .=',1PG20.13)   
 1200 FORMAT(
     & 5X,'DRUCKER-PRAGER MATERIAL CRITERION DEFINED FROM MOHR-COULOMB PARAMETERS',/,
     & 5X,'FORMULATION FLAG. . . . . . . . . . . .=',I10)     
 1201 FORMAT(
     & 5X,'-> CIRCUMBSCRIBED CRITERIA')   
 1202 FORMAT(
     & 5X,'-> MIDDLE CRITERIA') 
 1203 FORMAT(
     & 5X,'-> INSCRIBED CRITERIA') 
 1204 FORMAT(
     & 5X,'-> ORIGINAL MOHR-COULOMB CRITERIA')   
 1300 FORMAT(
     & 5X,'PARAMETERS USED TO DEFINE CRITERIA',/,
     & 5X,'A0. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'A1. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'A2. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'AMAX. . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD FUNCTION PRESSURE ROOT. . . . . .=',1PG20.13//)        

      RETURN
      END
