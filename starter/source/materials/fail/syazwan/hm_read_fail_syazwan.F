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
Chd|  HM_READ_FAIL_SYAZWAN          source/materials/fail/syazwan/hm_read_fail_syazwan.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_SYAZWAN(
     .           UPARAM    ,MAXUPARAM ,NUPARAM ,NUVAR  ,IFUNC  ,
     .           MAXFUNC   ,NFUNC     ,UNITAB  ,MAT_ID ,FAIL_ID,
     .           TITR      ,LSUBMODEL )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE SYAZWAN
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     UNITAB          UNITS ARRAY
C     FAIL_ID         FAILURE ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C UPARAM   | NUPARAM | F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER  VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C FAIL_ID  |  1      | I | W | ID OF FAILURE CRITERIA
C----------+---------+---+---+--------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
      INTEGER,INTENT(IN) :: MAXUPARAM,MAXFUNC,FAIL_ID,MAT_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
      CHARACTER TITR*nchartitle
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT) :: IFUNC(MAXFUNC),NFUNC
      my_real,INTENT(INOUT) :: UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)   :: NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real C1,C2,C3,C4,C5,C6
      my_real DAM_SF,MAX_DAM,N_VAL,SOFTEXP
      my_real REF_LEN,REF_SIZ_UNIT,REG_SCALE
      my_real EPS_BICOMP,EPS_COMP,EPS_CISAIL,EPS_TENS,
     .        EPS_PLANE,EPS_BITRAC,EPFMIN,EPF_COMP,
     .        EPF_SHEAR,EPF_TENS,EPF_PLSTRN,EPF_BIAX
      my_real TRIAX,LODEP,COS3THETA,EPSFAIL
      DOUBLE PRECISION :: EPF(6),MAT(6,6)
      INTEGER ICARD,IFORM,DINIT,INST,REG_FUNC,INFO,IPIV(6),I,FAILIP
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED,NEGATIVE
C-----------------------------------------------    
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS
C--------------------------------------------------
      ! 1st Line : failure criterion parameters
      CALL HM_GET_INTV   ('ICARD'    ,ICARD   ,IS_AVAILABLE,LSUBMODEL)
      IF (ICARD <= 0) ICARD = 1
      ICARD = MIN(ICARD,2)
      CALL HM_GET_FLOATV ('EPFMIN'   ,EPFMIN  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('FAILIP'   ,FAILIP  ,IS_AVAILABLE,LSUBMODEL)
      ! 2nd Line : failure criterion parameters
      IF (ICARD == 1) THEN 
        CALL HM_GET_FLOATV ('C1'        ,C1        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('C2'        ,C2        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('C3'        ,C3        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('C4'        ,C4        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('C5'        ,C5        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('C6'        ,C6        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ELSE
        CALL HM_GET_FLOATV ('EPF_COMP'  ,EPF_COMP  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('EPF_SHEAR' ,EPF_SHEAR ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('EPF_TENS'  ,EPF_TENS  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('EPF_PLSTRN',EPF_PLSTRN,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV ('EPF_BIAX'  ,EPF_BIAX  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
      ! 3rd Line : Damage initialization control
      CALL HM_GET_INTV   ('DINIT'    ,DINIT   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('DAM_SF'   ,DAM_SF  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAX_DAM'  ,MAX_DAM ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! 4th Line : Instability + stress softening
      CALL HM_GET_INTV   ('INST'     ,INST    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('IFORM'    ,IFORM   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('N_VAL'    ,N_VAL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('SOFTEXP'  ,SOFTEXP ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! 5th Line : Element size regularization control
      CALL HM_GET_INTV   ('REG_FUNC' ,REG_FUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('REF_LEN'  ,REF_LEN ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (REG_FUNC > 0 .AND. REF_LEN == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('REF_LEN',REF_SIZ_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        REF_LEN = ONE*REF_SIZ_UNIT
      ENDIF
      CALL HM_GET_FLOATV ('REG_SCALE',REG_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (REG_FUNC > 0 .AND. REG_SCALE == ZERO) REG_SCALE = ONE
C--------------------------------------------------
C COMPUTE C PARAMETERS IF ICARD = 2
C--------------------------------------------------
      IF (ICARD == 2) THEN 
        ! Filling failure strain vector
        EPF(1) = ZERO        ! IMPOSE MINIMUM VALUE IN PLANE STRAIN
        EPF(2) = EPF_COMP    ! COMP
        EPF(3) = EPF_SHEAR   ! CISAIL
        EPF(4) = EPF_TENS    ! TENS
        EPF(5) = EPF_PLSTRN  ! PLANE STRAIN
        EPF(6) = EPF_BIAX    ! BITRAC

        ! Filling the linear system Matrix
        MAT(1:6,1:6) = ZERO
        ! -> Impose minimum value in plane strain
        MAT(1,1) = ZERO
        MAT(1,2) = ONE
        MAT(1,3) = -18.0D0/PI
        MAT(1,4) = TWO/SQRT(THREE)
        MAT(1,5) = ZERO
        MAT(1,6) = -18.0D0/(PI*SQRT(THREE))
        ! -> Compression
        MAT(2,1) = ONE
        MAT(2,2) = -THIRD
        MAT(2,3) = -ONE
        MAT(2,4) = ONE/NINE
        MAT(2,5) = ONE
        MAT(2,6) = THIRD
        ! -> Shear
        MAT(3,1) = ONE
        MAT(5,2) = ZERO
        MAT(5,3) = ZERO
        MAT(5,4) = ZERO
        MAT(5,5) = ZERO
        MAT(5,6) = ZERO
        ! -> Traction
        MAT(4,1) = ONE
        MAT(4,2) = THIRD
        MAT(4,3) = ONE
        MAT(4,4) = ONE/NINE
        MAT(4,5) = ONE
        MAT(4,6) = THIRD
        ! -> Plane strain
        MAT(5,1) = ONE
        MAT(5,2) = ONE/SQRT(THREE)
        MAT(5,3) = ZERO
        MAT(5,4) = THIRD
        MAT(5,5) = ZERO
        MAT(5,6) = ZERO
        ! -> Bitraction
        MAT(6,1) = ONE
        MAT(6,2) = TWO_THIRD
        MAT(6,3) = -ONE
        MAT(6,4) = FOUR/NINE
        MAT(6,5) = ONE
        MAT(6,6) = -TWO_THIRD
c
        ! Solve linear system 
#ifndef WITHOUT_LINALG
        CALL DGESV(6, 1, MAT, 6, IPIV, EPF, 6, INFO)
#else
        WRITE(6,*) "Error: Blas/Lapack required"
#endif
c
        ! Copy C parameters
        C1 = EPF(1)
        C2 = EPF(2)
        C3 = EPF(3)
        C4 = EPF(4)
        C5 = EPF(5)
        C6 = EPF(6)
c
      ENDIF
C--------------------------------------------------
C CHECK VALUES
C--------------------------------------------------
      ! Formulation for instability strain
      IF (IFORM <= 0) IFORM = 1
      IFORM = MIN(IFORM,2)
      ! Damage initialization scale factor
      DAM_SF = MAX(DAM_SF,ZERO)
      IF (DAM_SF  == ZERO) DAM_SF  = ONE
      ! Maximal damage for initialization
      IF (MAX_DAM == ZERO) MAX_DAM = ONE
      MAX_DAM = MIN(MAX_DAM,ONE)
      MAX_DAM = MAX(MAX_DAM,ZERO)
      ! Plastic strain at instability valye (N_VAL)
      IF(INST==1 .AND. N_VAL==ZERO) THEN
        N_VAL = FOURTH
      ENDIF
      ! Softening exponent
      IF (SOFTEXP == ZERO) SOFTEXP = ONE
      SOFTEXP = MAX(EM06,SOFTEXP)
      ! Minimum plastic strain at failure
      EPFMIN = MAX(ZERO,EPFMIN)
      ! Check if criterion takes negative values
      TRIAX = -TWO_THIRD
      NEGATIVE = .FALSE.
      DO I = 1,1001
        COS3THETA = -HALF*TWENTY7*TRIAX*(TRIAX**2 - THIRD)
        IF (COS3THETA < -ONE ) COS3THETA = -ONE
        IF (COS3THETA >  ONE ) COS3THETA =  ONE
        LODEP = ONE - TWO*ACOS(COS3THETA)/PI 
        EPSFAIL = C1 + C2*TRIAX + C3*LODEP + C4*(TRIAX**2) + 
     .            C5*(LODEP**2) + C6*TRIAX*LODEP
        IF (EPSFAIL <= EPFMIN) THEN 
          NEGATIVE = .TRUE.
          EXIT
        ENDIF 
        TRIAX = TRIAX + (TWO*TWO_THIRD)/EP03
      ENDDO
      IF (NEGATIVE) THEN 
        CALL ANCMSG(MSGID=2091,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=MAT_ID,
     .              C1=TITR,
     .              R1=EPFMIN)
      ENDIF
      ! Default value for NUMFIP
      IF (FAILIP == 0) FAILIP = 1
C--------------------------------------------------
C SAVE PARAMETERS
C--------------------------------------------------
      ! Number of functions
      NFUNC      = 1
      IFUNC(1)   = REG_FUNC
      ! Number of parameters
      NUPARAM    = 17
      UPARAM(1)  = C1
      UPARAM(2)  = C2
      UPARAM(3)  = C3
      UPARAM(4)  = C4
      UPARAM(5)  = C5
      UPARAM(6)  = C6
      UPARAM(7)  = IFORM
      UPARAM(8)  = DINIT
      UPARAM(9)  = DAM_SF
      UPARAM(10) = MAX_DAM
      UPARAM(11) = INST
      UPARAM(12) = N_VAL
      UPARAM(13) = SOFTEXP
      UPARAM(14) = REF_LEN
      UPARAM(15) = REG_SCALE
      UPARAM(16) = EPFMIN
      UPARAM(17) = FAILIP
      ! Number of internal variables
      NUVAR      = 3
C--------------------------------------------------
C PRINT OUT PARAMETERS
C--------------------------------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000) 
        IF (ICARD == 2) THEN 
          WRITE(IOUT,1100) EPF_COMP,EPF_SHEAR,EPF_TENS,
     .                     EPF_PLSTRN,EPF_BIAX,
     .                     C1,C2,C3,C4,C5,C6
        ELSE 
          WRITE(IOUT,1200) C1,C2,C3,C4,C5,C6
        ENDIF
        WRITE(IOUT,1300) EPFMIN,FAILIP
        WRITE(IOUT,1400) DINIT,DAM_SF,MAX_DAM
        WRITE(IOUT,1500) INST,IFORM,N_VAL,SOFTEXP
        IF (REG_FUNC > 0) THEN 
          WRITE(IOUT,1600) REG_FUNC,REF_LEN,REG_SCALE
        ENDIF
      ENDIF
C---
C
 1000 FORMAT(
     & 5X,' --------------------------------------------------------',/,
     & 5X,'               SYAZWAN FAILURE CRITERION                 ',/,
     & 5X,' --------------------------------------------------------',/)
 1100 FORMAT(
     & 5X,'PLAST. STRAIN AT FAILURE IN UNIAXIAL COMP . . . . . . . =',1PG20.13/
     & 5X,'PLAST. STRAIN AT FAILURE IN SHEAR . . . . . . . . . . . =',1PG20.13/
     & 5X,'PLAST. STRAIN AT FAILURE IN UNIAXIAL TENS . . . . . . . =',1PG20.13/
     & 5X,'PLAST. STRAIN AT FAILURE IN PLANE STRAIN. . . . . . . . =',1PG20.13/
     & 5X,'PLAST. STRAIN AT FAILURE IN BIAXIAL TENS. . . . . . . . =',1PG20.13/
     & 5X,'                                                         ',/,
     & 5X,'CORRESPONDING C PARAMETERS:                              ',/,
     & 5X,'1ST FAILURE PARAMETER (C1). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'2ND FAILURE PARAMETER (C2). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'3RD FAILURE PARAMETER (C3). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'4TH FAILURE PARAMETER (C4). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'5TH FAILURE PARAMETER (C5). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'6TH FAILURE PARAMETER (C6). . . . . . . . . . . . . . . =',1PG20.13/)
 1200 FORMAT(
     & 5X,'1ST FAILURE PARAMETER (C1). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'2ND FAILURE PARAMETER (C2). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'3RD FAILURE PARAMETER (C3). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'4TH FAILURE PARAMETER (C4). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'5TH FAILURE PARAMETER (C5). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'6TH FAILURE PARAMETER (C6). . . . . . . . . . . . . . . =',1PG20.13/)
 1300 FORMAT(
     & 5X,'MINIMUM PLASTIC STRAIN AT FAILURE (EPFMIN)  . . . . . . =',1PG20.13/
     & 5X,'NUM. OF FAILED INTG. PTS. PRIOR TO SOLID ELM. DELETION. =',I10/)
 1400 FORMAT(
     & 5X,'DAMAGE VARIABLE INITIALIZATION FLAG (DINIT) . . . . . . =',I10/,
     & 5X,'   DINIT = 0: NO DAMAGE INITIALIZATION                   ',/,
     & 5X,'   DINIT = 1: DAMAGE INITIALIZATION FROM STRAIN TENSOR   ',/,
     & 5X,'DAMAGE INITIALIZATION SCALE FACTOR. . . . . . . . . . . =',1PG20.13/
     & 5X,'DAMAGE INITIALIZATION MAXIMAL VALUE . . . . . . . . . . =',1PG20.13/)
 1500 FORMAT(
     & 5X,'INSTABILITY FLAG (INST) . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'   INST  = 0: NO NECKING INSTABILITY                     ',/,
     & 5X,'   INST  = 1: NECKING INSTABILITY ACTIVATED              ',/,
     & 5X,'INSTABILITY FORMULATION FLAG (IFORM)  . . . . . . . . . =',I10/,
     & 5X,'   IFORM = 1: INCREMENTAL FORMULATION (DEFAULT)          ',/,
     & 5X,'   IFORM = 2: DIRECT FORMULATION                         ',/,
     & 5X,"HOLLOMON'S LAW N VALUE  . . . . . . . . . . . . . . . . =",1PG20.13/
     & 5X,'STRESS SOFTENING EXPONENT (SOFTEXP) . . . . . . . . . . =',1PG20.13/)
 1600 FORMAT(
     & 5X,'ELEMENT SIZE SCALING FUNCTION ID . . . . . . . . . . . .=',I10/
     & 5X,'   REFERENCE ELEMENT SIZE  . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'   SCALE FACTOR  . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)

      END