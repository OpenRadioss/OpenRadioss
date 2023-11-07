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
Chd|  HM_READ_FAIL_ORTHENERG        source/materials/fail/orthenerg/hm_read_fail_orthenerg.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
       SUBROUTINE HM_READ_FAIL_ORTHENERG(
     .           FAIL     ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read Orthenerg failure model parameters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include  "units_c.inc"
#include  "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER             ,INTENT(IN) :: FAIL_ID       ! failure model ID
      INTEGER             ,INTENT(IN) :: IRUPT         ! failure model number
      TYPE (UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        ! table of input units
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)  ! submodel table 
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER FAILIP,ISHAP11T,ISHAP11C,ISHAP22T,ISHAP22C,ISHAP33T,ISHAP33C,
     .        ISHAP12T,ISHAP12C,ISHAP23T,ISHAP23C,ISHAP31T,ISHAP31C,NMOD
      my_real 
     .    PTHICKFAIL,SIGMA_11T,G_11T,SIGMA_11C,G_11C,SIGMA_22T,G_22T,
     .    SIGMA_22C,G_22C,SIGMA_33T,G_33T,SIGMA_33C,G_33C,SIGMA_12T,G_12T,
     .    SIGMA_12C,G_12C,SIGMA_23T,G_23T,SIGMA_23C,G_23C,SIGMA_31T,G_31T,
     .    SIGMA_31C,G_31C
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      ! 1st card: Element deletion parameters, strain formulation
      CALL HM_GET_FLOATV ('PTHICKFAIL',PTHICKFAIL ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('NMOD'      ,NMOD       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('FAILIP'    ,FAILIP     ,IS_AVAILABLE,LSUBMODEL)
c
      ! 2nd card: direction 11 failure parameters
      CALL HM_GET_FLOATV ('SIGMA_11T' ,SIGMA_11T  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_11T'     ,G_11T      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP11T'  ,ISHAP11T   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('SIGMA_11C' ,SIGMA_11C  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_11C'     ,G_11C      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP11C'  ,ISHAP11C   ,IS_AVAILABLE,LSUBMODEL)    
c
      ! 3rd card: direction 22 failure parameters
      CALL HM_GET_FLOATV ('SIGMA_22T' ,SIGMA_22T  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_22T'     ,G_22T      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP22T'  ,ISHAP22T   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('SIGMA_22C' ,SIGMA_22C  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_22C'     ,G_22C      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP22C'  ,ISHAP22C   ,IS_AVAILABLE,LSUBMODEL)       
c
      ! 4th card: direction 33 failure parameters
      CALL HM_GET_FLOATV ('SIGMA_33T' ,SIGMA_33T  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_33T'     ,G_33T      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP33T'  ,ISHAP33T   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('SIGMA_33C' ,SIGMA_33C  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_33C'     ,G_33C      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP33C'  ,ISHAP33C   ,IS_AVAILABLE,LSUBMODEL)  
c
      ! 5th card: shear in plane 12 failure parameters
      CALL HM_GET_FLOATV ('SIGMA_12T' ,SIGMA_12T  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_12T'     ,G_12T      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP12T'  ,ISHAP12T   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('SIGMA_12C' ,SIGMA_12C  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_12C'     ,G_12C      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP12C'  ,ISHAP12C   ,IS_AVAILABLE,LSUBMODEL)  
c
      ! 6th card: shear in plane 23 failure parameters
      CALL HM_GET_FLOATV ('SIGMA_23T' ,SIGMA_23T  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_23T'     ,G_23T      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP23T'  ,ISHAP23T   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('SIGMA_23C' ,SIGMA_23C  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_23C'     ,G_23C      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP23C'  ,ISHAP23C   ,IS_AVAILABLE,LSUBMODEL)  
c
      ! 7th card: shear in plane 31 failure parameters
      CALL HM_GET_FLOATV ('SIGMA_31T' ,SIGMA_31T  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_31T'     ,G_31T      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP31T'  ,ISHAP31T   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('SIGMA_31C' ,SIGMA_31C  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('G_31C'     ,G_31C      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('ISHAP31C'  ,ISHAP31C   ,IS_AVAILABLE,LSUBMODEL)  
c
      !==========================================================================
      ! Check parameters and set defaults value
      !==========================================================================
      ! Percentage of failed thickness/plys/layers/ratio
      IF (PTHICKFAIL  == ZERO) PTHICKFAIL = ONE-EM06
      PTHICKFAIL = MIN(PTHICKFAIL, ONE)
      PTHICKFAIL = MAX(PTHICKFAIL,-ONE)
      ! Number of failed modes prior to integration point failure
      IF (NMOD == 0) NMOD = 1
      NMOD = MIN(NMOD,12)
      ! Number of failed integration points prior to solid deletion
      IF (FAILIP == 0) FAILIP = 1
      ! Check if value are positive
      SIGMA_11T = ABS(SIGMA_11T)
      SIGMA_22T = ABS(SIGMA_22T)
      SIGMA_12T = ABS(SIGMA_12T)
      SIGMA_11C = ABS(SIGMA_11C)
      SIGMA_22C = ABS(SIGMA_22C)
      SIGMA_12C = ABS(SIGMA_12C)
      ! Critical stresses in tension
      IF (SIGMA_11T == ZERO) SIGMA_11T = INFINITY
      IF (SIGMA_22T == ZERO) SIGMA_22T = INFINITY
      IF (SIGMA_33T == ZERO) SIGMA_33T = INFINITY
      IF (SIGMA_12T == ZERO) SIGMA_12T = INFINITY
      IF (SIGMA_23T == ZERO) SIGMA_23T = INFINITY 
      IF (SIGMA_31T == ZERO) SIGMA_31T = INFINITY
      ! Critical stresses in compression
      IF (SIGMA_11C == ZERO) SIGMA_11C = SIGMA_11T
      IF (SIGMA_22C == ZERO) SIGMA_22C = SIGMA_22T
      IF (SIGMA_33C == ZERO) SIGMA_33C = SIGMA_33T
      IF (SIGMA_12C == ZERO) SIGMA_12C = SIGMA_12T 
      IF (SIGMA_23C == ZERO) SIGMA_23C = SIGMA_23T
      IF (SIGMA_31C == ZERO) SIGMA_31C = SIGMA_31T
      ! Fracture energy in tension
      IF (G_11T == ZERO) G_11T = INFINITY
      IF (G_22T == ZERO) G_22T = INFINITY
      IF (G_33T == ZERO) G_33T = INFINITY
      IF (G_12T == ZERO) G_12T = INFINITY
      IF (G_23T == ZERO) G_23T = INFINITY 
      IF (G_31T == ZERO) G_31T = INFINITY   
      ! Fracture energy in compression 
      IF (G_11C == ZERO) G_11C = G_11T
      IF (G_22C == ZERO) G_22C = G_22T
      IF (G_33C == ZERO) G_33C = G_33T
      IF (G_12C == ZERO) G_12C = G_12T
      IF (G_23C == ZERO) G_23C = G_23T 
      IF (G_31C == ZERO) G_31C = G_31T       
      ! Softening shape flags
      IF (ISHAP11T == ZERO) ISHAP11T = 1
      ISHAP11T = MIN(ISHAP11T,2)
      ISHAP11T = MAX(ISHAP11T,1)
      IF (ISHAP11C == ZERO) ISHAP11C = 1
      ISHAP11C = MIN(ISHAP11C,2)
      ISHAP11C = MAX(ISHAP11C,1)
      IF (ISHAP22T == ZERO) ISHAP22T = 1
      ISHAP22T = MIN(ISHAP22T,2)
      ISHAP22T = MAX(ISHAP22T,1)
      IF (ISHAP22C == ZERO) ISHAP22C = 1
      ISHAP22C = MIN(ISHAP22C,2)
      ISHAP22C = MAX(ISHAP22C,1)
      IF (ISHAP33T == ZERO) ISHAP33T = 1
      ISHAP33T = MIN(ISHAP33T,2)
      ISHAP33T = MAX(ISHAP33T,1)
      IF (ISHAP33C == ZERO) ISHAP33C = 1
      ISHAP33C = MIN(ISHAP33C,2)
      ISHAP33C = MAX(ISHAP33C,1)
      IF (ISHAP12T == ZERO) ISHAP12T = 1
      ISHAP12T = MIN(ISHAP12T,2)
      ISHAP12T = MAX(ISHAP12T,1)
      IF (ISHAP12C == ZERO) ISHAP12C = 1
      ISHAP12C = MIN(ISHAP12C,2)
      ISHAP12C = MAX(ISHAP12C,1)
      IF (ISHAP23T == ZERO) ISHAP23T = 1
      ISHAP23T = MIN(ISHAP23T,2)
      ISHAP23T = MAX(ISHAP23T,1)
      IF (ISHAP23C == ZERO) ISHAP23C = 1
      ISHAP23C = MIN(ISHAP23C,2)
      ISHAP23C = MAX(ISHAP23C,1)
      IF (ISHAP31T == ZERO) ISHAP31T = 1
      ISHAP31T = MIN(ISHAP31T,2)
      ISHAP31T = MAX(ISHAP31T,1)
      IF (ISHAP31C == ZERO) ISHAP31C = 1
      ISHAP31C = MIN(ISHAP31C,2)
      ISHAP31C = MAX(ISHAP31C,1)
c
      !==========================================================================
      ! Save parameters and set buffer tables size
      !==========================================================================
      FAIL%KEYWORD = 'ORTHENERG' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 39
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 25
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
      FAIL%PTHK    = PTHICKFAIL
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      ! Failure criterion parameters
      FAIL%UPARAM(1)  = PTHICKFAIL
      FAIL%UPARAM(2)  = FAILIP
      FAIL%UPARAM(3)  = SIGMA_11T
      FAIL%UPARAM(4)  = SIGMA_11C
      FAIL%UPARAM(5)  = SIGMA_22T
      FAIL%UPARAM(6)  = SIGMA_22C
      FAIL%UPARAM(7)  = SIGMA_33T
      FAIL%UPARAM(8)  = SIGMA_33C
      FAIL%UPARAM(9)  = SIGMA_12T
      FAIL%UPARAM(10) = SIGMA_12C
      FAIL%UPARAM(11) = SIGMA_23T
      FAIL%UPARAM(12) = SIGMA_23C
      FAIL%UPARAM(13) = SIGMA_31T
      FAIL%UPARAM(14) = SIGMA_31C
      FAIL%UPARAM(15) = G_11T
      FAIL%UPARAM(16) = G_11C
      FAIL%UPARAM(17) = G_22T
      FAIL%UPARAM(18) = G_22C
      FAIL%UPARAM(19) = G_33T
      FAIL%UPARAM(20) = G_33C
      FAIL%UPARAM(21) = G_12T
      FAIL%UPARAM(22) = G_12C
      FAIL%UPARAM(23) = G_23T
      FAIL%UPARAM(24) = G_23C
      FAIL%UPARAM(25) = G_31T
      FAIL%UPARAM(26) = G_31C
      FAIL%UPARAM(27) = ISHAP11T
      FAIL%UPARAM(28) = ISHAP11C
      FAIL%UPARAM(29) = ISHAP22T
      FAIL%UPARAM(30) = ISHAP22C
      FAIL%UPARAM(31) = ISHAP33T
      FAIL%UPARAM(32) = ISHAP33C
      FAIL%UPARAM(33) = ISHAP12T
      FAIL%UPARAM(34) = ISHAP12C
      FAIL%UPARAM(35) = ISHAP23T
      FAIL%UPARAM(36) = ISHAP23C
      FAIL%UPARAM(37) = ISHAP31T
      FAIL%UPARAM(38) = ISHAP31C
      FAIL%UPARAM(39) = NMOD
c
      !==========================================================================
      ! Print out parameters
      !==========================================================================
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000) 
        WRITE(IOUT,1100) PTHICKFAIL,FAILIP,NMOD
        WRITE(IOUT,1200) SIGMA_11T,G_11T,ISHAP11T,SIGMA_11C,G_11C,ISHAP11C
        WRITE(IOUT,1300) SIGMA_22T,G_22T,ISHAP22T,SIGMA_22C,G_22C,ISHAP22C
        WRITE(IOUT,1400) SIGMA_33T,G_33T,ISHAP33T,SIGMA_33C,G_33C,ISHAP33C
        WRITE(IOUT,1500) SIGMA_12T,G_12T,ISHAP12T,SIGMA_12C,G_12C,ISHAP12C
        WRITE(IOUT,1600) SIGMA_23T,G_23T,ISHAP23T,SIGMA_23C,G_23C,ISHAP23C
        WRITE(IOUT,1700) SIGMA_31T,G_31T,ISHAP31T,SIGMA_31C,G_31C,ISHAP31C
      ENDIF
c-----------------------------------------------------
1000  FORMAT(
     & 5X,'--------------------------------------------------'/,
     & 5X,'      ENERGY BASED ORTHOTROPIC FAILURE MODEL      '/,
     & 5X,'--------------------------------------------------'/)
1100  FORMAT(
     & 5X,'ELEMENT DELETION PARAMETERS:                      '/,
     & 5X,'                                                  '/,
     & 5X,'SHELL ELEMENT DELETION PARAMETER PTHICKFAIL. . . .=',1PG20.13,/,
     & 5X,'  > 0.0 : FRACTION OF FAILED THICKNESS             ',/,
     & 5X,'  < 0.0 : FRACTION OF FAILED INTG. POINTS OR LAYERS',/,
     & 5X,'NUMBER OF FAILED INTG. PTS PRIOR TO SOLID DELETION=',I5/,
     & 5X,'NUMBER OF FAILED MODES PRIOR TO ITG. POINT FAILURE=',I5/)
1200  FORMAT(
     & 5X,'DIRECTION 11 PARAMETERS:                           '/,
     & 5X,'                                                   '/,     
     & 5X,'CRITICAL STRESS IN TENSION SIGMA_11T . . . . . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN TENSION G_11T . . . . . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN TENSION ISHAP11T. . . . .=',I3/, 
     & 5X,'  ISHAP11T = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP11T = 2: EXPONENTIAL                        ',/,
     & 5X,'CRITICAL STRESS IN COMPRESSION SIGMA_11C . . . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN COMPRESSION G_11C . . . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN COMPRESSION ISHAP11C. . .=',I3/, 
     & 5X,'  ISHAP11C = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP11C = 2: EXPONENTIAL                        ',/)
1300  FORMAT(
     & 5X,'DIRECTION 22 PARAMETERS:                           '/,
     & 5X,'                                                   '/,     
     & 5X,'CRITICAL STRESS IN TENSION SIGMA_22T . . . . . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN TENSION G_22T . . . . . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN TENSION ISHAP22T. . . . .=',I3/, 
     & 5X,'  ISHAP22T = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP22T = 2: EXPONENTIAL                        ',/,
     & 5X,'CRITICAL STRESS IN COMPRESSION SIGMA_22C . . . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN COMPRESSION G_22C . . . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN COMPRESSION ISHAP22C. . .=',I3/, 
     & 5X,'  ISHAP22C = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP22C = 2: EXPONENTIAL                        ',/)
1400  FORMAT(
     & 5X,'DIRECTION 33 PARAMETERS:                           '/,
     & 5X,'                                                   '/,     
     & 5X,'CRITICAL STRESS IN TENSION SIGMA_33T . . . . . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN TENSION G_33T . . . . . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN TENSION ISHAP33T. . . . .=',I3/, 
     & 5X,'  ISHAP33T = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP33T = 2: EXPONENTIAL                        ',/,
     & 5X,'CRITICAL STRESS IN COMPRESSION SIGMA_33C . . . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN COMPRESSION G_33C . . . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN COMPRESSION ISHAP33C. . .=',I3/, 
     & 5X,'  ISHAP33C = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP33C = 2: EXPONENTIAL                        ',/)
1500  FORMAT(
     & 5X,'PLANE 12 PARAMETERS:                               '/,
     & 5X,'                                                   '/,     
     & 5X,'CRITICAL STRESS IN POSITIVE SHEAR SIGMA_12T. . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN POSITIVE SHEAR G_12T. . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN POSITIVE SHEAR ISHAP12T .=',I3/, 
     & 5X,'  ISHAP12T = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP12T = 2: EXPONENTIAL                        ',/,
     & 5X,'CRITICAL STRESS IN NEGATIVE SHEAR SIGMA_12C. . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN NEGATIVE SHEAR G_12C. . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN NEGATIVE SHEAR ISHAP12C .=',I3/, 
     & 5X,'  ISHAP12C = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP12C = 2: EXPONENTIAL                        ',/)
1600  FORMAT(
     & 5X,'PLANE 23 PARAMETERS:                               '/,
     & 5X,'                                                   '/,     
     & 5X,'CRITICAL STRESS IN POSITIVE SHEAR SIGMA_23T. . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN POSITIVE SHEAR G_23T. . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN POSITIVE SHEAR ISHAP23T .=',I3/, 
     & 5X,'  ISHAP23T = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP23T = 2: EXPONENTIAL                        ',/,
     & 5X,'CRITICAL STRESS IN NEGATIVE SHEAR SIGMA_23C. . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN NEGATIVE SHEAR G_23C. . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN NEGATIVE SHEAR ISHAP23C .=',I3/, 
     & 5X,'  ISHAP23C = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP23C = 2: EXPONENTIAL                        ',/)
1700  FORMAT(
     & 5X,'PLANE 31 PARAMETERS:                               '/,
     & 5X,'                                                   '/,     
     & 5X,'CRITICAL STRESS IN POSITIVE SHEAR SIGMA_31T. . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN POSITIVE SHEAR G_31T. . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN POSITIVE SHEAR ISHAP31T .=',I3/, 
     & 5X,'  ISHAP31T = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP31T = 2: EXPONENTIAL                        ',/,
     & 5X,'CRITICAL STRESS IN NEGATIVE SHEAR SIGMA_31C. . . .=',1PG20.13,/,
     & 5X,'FRACTURE ENERGY IN NEGATIVE SHEAR G_31C. . . . . .=',1PG20.13,/,
     & 5X,'STRESS SOFTENING FLAG IN NEGATIVE SHEAR ISHAP31C .=',I3/, 
     & 5X,'  ISHAP31C = 1: LINEAR (DEFAULT)                   ',/,
     & 5X,'  ISHAP31C = 2: EXPONENTIAL                        ',/)
      END
