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
Chd|  HM_READ_MAT05                 source/materials/mat/mat005/hm_read_mat05.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED          source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT05(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,UID)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW05 WITH HM READER
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
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      INTEGER, INTENT(IN)                   :: UID
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      my_real,INTENT(IN)                    :: UNITAB(LUNIT,NUNITS)
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     A, B, R1, R2, W, D, PCJ, E0, C0, C1, VCJ,
     .     EADD, TBEGIN, TEND, 
     .     PSH,REACTION_RATE,REACTION_RATE2,A_MIL,M_MIL,N_MIL,ALPHA_UNIT,
     .     BID, VAL(5), BULK
      INTEGER :: IBID, IBFRAC, QOPT, I_ERROR, IFLAGUNIT, J
      my_real :: RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------
      IPM(4) = 15
      TBEGIN = ZERO
      TEND = INFINITY
      IBFRAC = 0
      PSH = ZERO
      C0 = ZERO
      EADD = ZERO
      REACTION_RATE = ZERO
      REACTION_RATE2 = ZERO
      A_MIL = ZERO
      M_MIL = ZERO
      N_MIL = ZERO
      ALPHA_UNIT = ZERO
      BID = ZERO
      I_ERROR = 0
      VAL(1:5) = ZERO
      BULK = ZERO

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 0
      IMATVIS = 0
      
      !unit needed for millers extension
      IFLAGUNIT = 0                            
      DO J=1,NUNITS                            
        IF (NINT(UNITAB(1,J)) == UID) THEN     
          IFLAGUNIT = 1                        
          EXIT                                 
        ENDIF                                  
      ENDDO                                    

      !======== READING INPUT FILE ===========!
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO', RHO0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHOR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_A', A, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B', B, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PDIR1', R1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PDIR2', R2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Omega', W, IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT_D', D, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC', PCJ, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E0', E0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E', EADD, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('MAT_IBFRAC', IBFRAC, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('QOPT', QOPT, IS_AVAILABLE, LSUBMODEL)

      IF(QOPT < 0 .OR. QOPT > 3)THEN
        QOPT = 0
      ENDIF

      !line-4
      CALL HM_GET_FLOATV('LAW5_P0', C0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW5_PSH', PSH, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('BUNREACTED', BULK, IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      !-----AFTERBURNING
      IF((QOPT == 0 .OR. QOPT == 1 .OR. QOPT == 2) .AND. EADD > 0)THEN
      !line-4
        CALL HM_GET_FLOATV('TSTART', TBEGIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('TSTOP', TEND, IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF(TEND==ZERO) TEND=INFINITY 
        IF(EADD>ZERO)THEN                                   
          IF(TBEGIN==TEND)THEN                                 
            !Dirac function release : this means instantaneous 
            QOPT = 0                                           
          ENDIF                                                
        ENDIF                                                  
      ELSEIF(QOPT == 3 .AND. EADD > 0)THEN
        CALL HM_GET_FLOATV('LAW5_A', A_MIL, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('LAW5_M', M_MIL, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('LAW5_N', N_MIL, IS_AVAILABLE, LSUBMODEL, UNITAB)      
      ENDIF
      
      !========AFTERBURNING REACTION RATE===========!
      REACTION_RATE  = ZERO
      REACTION_RATE2 = ZERO
      SELECT CASE(QOPT)      
        CASE(0)
          !---INSTANTANEOUS
          REACTION_RATE  = ZERO
          REACTION_RATE2 = ZERO 
        CASE(1)
          !---CONSTANT AFTERBURNING RATE
          REACTION_RATE  = ONE/(TEND-TBEGIN) 
          REACTION_RATE2 = ZERO       
        CASE(2)
          !---LINEAR AFTERBURNING RATE
          REACTION_RATE  = TWO/(TEND-TBEGIN)**2 
          REACTION_RATE2 = TBEGIN**2/(TEND-TBEGIN)**2
        CASE(3)
          !---MILLER S EXTENSION
          REACTION_RATE  = ZERO
          REACTION_RATE2 = ZERO 
          IF(ALPHA_UNIT == ZERO)ALPHA_UNIT=ONE        
        CASE DEFAULT
          !CLASSICAL MODEL
          EADD          = ZERO
          REACTION_RATE = ZERO   
      END SELECT     
      
      !========C1: MODULE EQUIVALENT POUR LES RIGIDITES D'INTERFACES
      IF(BULK>ZERO)THEN
        C1 = BULK      
      ELSE
        C1 = W*(PCJ+E0)
      ENDIF

      !---DEFAULT VALUES
      IF(RHOR == ZERO)RHOR=RHO0
      PM(01) = RHOR
      PM(89) = RHO0           
      !========STORAGE 
      PM(23)  = E0
      PM(31)  = C0-PSH
      PM(32)  = C1
      PM(33)  = A
      PM(34)  = B
      PM(35)  = R1
      PM(36)  = R2
      PM(37)  = -PSH
      PM(45)  = W
      PM(38)  = D
      PM(39)  = PCJ
      PM(40)  = PM(1)*D**2/PCJ
      PM(41)  = IBFRAC
      PM(42)  = QOPT
      PM(43)  = C0
      PM(44)  = BULK
      VCJ     = ONE-ONE/PM(40)
      PM(104) = C0-PSH
      PM(160) = EADD
      PM(161) = TBEGIN
      PM(162) = TEND
      PM(163) = REACTION_RATE
      PM(164) = A_MIL
      PM(165) = M_MIL
      PM(166) = N_MIL
      PM(167) = REACTION_RATE2
      PM(168) = ALPHA_UNIT
      PM(88)  = PSH
      !ssp0
      PM(27)  = D
          
      !======== BUFFER ALLOCATION SIZES
      MTAG%G_TB     = 1  
      MTAG%G_TEMP   = 1  
      MTAG%G_BFRAC  = 1
      MTAG%G_ABURN  = 1
      MTAG%L_TB     = 1
      MTAG%L_TEMP   = 1
      MTAG%L_BFRAC  = 1 
      MTAG%L_ABURN  = 1       
      
      !======== LISTING OUTPUT
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,5
      WRITE(IOUT,1000)     
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT,1002) RHO0,RHOR
         WRITE(IOUT,1300)A,B,R1,R2,W
         WRITE(IOUT,1400)D,PCJ,VCJ,E0,C0,PSH,BULK,IBFRAC
         !AFTERBURNING OPTIONAL MODEL
         IF(EADD == ZERO) THEN
           WRITE(IOUT,1500)
         ELSE
           IF(QOPT == 0) THEN
             WRITE(IOUT,1504)EADD,TBEGIN
           ELSEIF(QOPT == 1) THEN
             WRITE(IOUT,1501)EADD,TBEGIN,TEND
           ELSEIF(QOPT == 2) THEN
             WRITE(IOUT,1502)EADD,TBEGIN,TEND
           ELSEIF(QOPT == 3) THEN
             WRITE(IOUT,1503)EADD,A_MIL,M_MIL,N_MIL, ALPHA_UNIT
           ENDIF
         ENDIF
      ENDIF
      
      RETURN
 1000 FORMAT(
     &     5X,'  J.W.L. EXPLOSIVE                      ',/,
     &     5X,'  ----------------                      ',//)
 1001 FORMAT(
     &     5X,A,/,
     &     5X,'MATERIAL NUMBER . . . . . . . . . . . . =',I10/,
     &     5X,'MATERIAL LAW. . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . . =',1PG20.13/)
 1300 FORMAT(
     &     5X,'A . . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'B . . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'R1. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'R2. . . . . . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'W . . . . . . . . . . . . . . . . . . .=',E12.4//)
 1400 FORMAT(
     &     5X,'DETONATION VELOCITY . . . . . . . . . .=',E12.4/,
     &     5X,'CHAPMAN JOUGUET PRESSURE. . . . . . . .=',E12.4/,
     &     5X,'CHAPMAN JOUGUET VOLUME. . . . . . . . .=',E12.4/,
     &     5X,'INITIAL ENERGY PER UNIT VOLUME. . . . .=',E12.4/,  
     &     5X,'INITIAL PRESSURE OF UNREACTED EXPLO.. .=',E12.4/,
     &     5X,'PRESSURE SHIFT. . . . . . . . . . . . .=',E12.4/,  
     &     5X,'UNREACTED EXPLOSIVE BULK MODULUS. . . .=',E12.4/,
     &     5X,'BURN FRACTION METHOD. . . . . . . . . .=',I12/)
 1500 FORMAT(
     &     5X,'NO AFTERBURNING MODELING        '//)
 1501 FORMAT(
     &     5X,'AFTERBURNING MODEL : CONSTANT REACTION RATE        ',/,
     &     5X,'ADDITIONAL ENERGY PER UNIT VOLUME . . .=',E12.4/,
     &     5X,'BEGIN TIME. . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'END TIME. . . . . . . . . . . . . . . .=',E12.4//)
 1502 FORMAT(
     &     5X,'AFTERBURNING : LINEAR REACTION RATE          ',/,
     &     5X,'ADDITIONAL ENERGY PER UNIT VOLUME . . .=',E12.4/,
     &     5X,'BEGIN TIME. . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'END TIME. . . . . . . . . . . . . . . .=',E12.4//)
 1503 FORMAT(
     &     5X,'AFTERBURNING : MILLER S EXTENSION       ',/,
     &     5X,'ADDITIONAL ENERGY PER UNIT VOLUME . . .=',E12.4/,
     &     5X,'a PARAMETER . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'m PARAMETER . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'n PARAMETER . . . . . . . . . . . . . .=',E12.4/,
     &     5X,'PRESSURE TRANSLATION FACTOR . . . . . .=',E12.4//)
 1504 FORMAT(
     &     5X,'AFTERBURNING : INSTANTANEOUS                 ',/,
     &     5X,'ADDITIONAL ENERGY PER UNIT VOLUME . . .=',E12.4/,
     &     5X,'BEGIN TIME. . . . . . . . . . . . . . .=',E12.4//)
 999  CALL FREERR(3)
      RETURN
      END
