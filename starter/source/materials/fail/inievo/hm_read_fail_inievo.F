Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_fail_inievo            ../starter/source/materials/fail/inievo/hm_read_fail_inievo.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail                   ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                         ../starter/source/output/message/message.F
!||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_float_array_index_dim   ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
!||    hm_get_floatv                  ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                    ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted         ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod             ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                    ../starter/share/message_module/message_mod.F
!||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
!||====================================================================
       SUBROUTINE HM_READ_FAIL_INIEVO(
     .           FAIL     ,MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           TITR     ,LSUBMODEL,UNITAB   )
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
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER            ,INTENT(IN) :: FAIL_ID         ! failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID          ! material law ID
      INTEGER            ,INTENT(IN) :: IRUPT           ! failure model type number
      CHARACTER          ,INTENT(IN) :: TITR*500        ! material model title
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB          ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)    ! submodel table
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL         ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINIEVO,FAILIP,ISHEAR,ILEN,NUPARAM,NUVAR,NTABLE
      INTEGER,DIMENSION(:), ALLOCATABLE :: 
     .    INITYPE,EVOTYPE,EVOSHAP,COMPTYP,TAB_ID,TAB_EL
      my_real :: PTHK,LENGTH_UNIT,RATE_UNIT
      my_real, DIMENSION(:), ALLOCATABLE :: 
     .    SR_REF,FSCALE,INI_P1,EL_REF,ELSCAL,DISP,ENER,ALPHA
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C======================================================================================
C EXTRACT DATAS (REAL VALUES)
C======================================================================================
!---------------
! -> Card1
!---------------
      CALL HM_GET_INTV   ('NINIEVO'  ,NINIEVO  ,IS_AVAILABLE,LSUBMODEL)
      IF (NINIEVO == 0) NINIEVO = 1
      CALL HM_GET_INTV   ('ISHEAR'   ,ISHEAR   ,IS_AVAILABLE,LSUBMODEL)
      ISHEAR = MIN(ISHEAR,1)
      CALL HM_GET_INTV   ('ILEN'     ,ILEN     ,IS_AVAILABLE,LSUBMODEL)
      ILEN   = MIN(ILEN,2)
      IF (ILEN == 2) THEN 
        CALL ANCMSG(MSGID=2094,
     .              MSGTYPE=MSGINFO,
     .              ANMODE=ANINFO_BLIND,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      CALL HM_GET_INTV   ('FAILIP'   ,FAILIP   ,IS_AVAILABLE,LSUBMODEL)
      IF (FAILIP == 0)  FAILIP  = 1
      CALL HM_GET_FLOATV ('PTHK'     ,PTHK     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (PTHK == ZERO) PTHK = EM06
      PTHK = MIN(PTHK, ONE)
      PTHK = MAX(PTHK,-ONE)
!---------------
! -> Card2
!---------------
      ALLOCATE(INITYPE(NINIEVO),EVOTYPE(NINIEVO),EVOSHAP(NINIEVO),COMPTYP(NINIEVO))
      ALLOCATE(TAB_ID(NINIEVO) ,SR_REF(NINIEVO) ,FSCALE(NINIEVO) ,INI_P1(NINIEVO))
      ALLOCATE(TAB_EL(NINIEVO) ,EL_REF(NINIEVO) ,ELSCAL(NINIEVO) )
      ALLOCATE(DISP(NINIEVO)   ,ENER(NINIEVO)   ,ALPHA(NINIEVO)  )
      INITYPE(1:NINIEVO) = 0
      EVOTYPE(1:NINIEVO) = 0
      EVOSHAP(1:NINIEVO) = 0
      COMPTYP(1:NINIEVO) = 0
      TAB_ID(1:NINIEVO)  = 0
      TAB_EL(1:NINIEVO)  = 0
      SR_REF(1:NINIEVO)  = ZERO
      FSCALE(1:NINIEVO)  = ZERO
      INI_P1(1:NINIEVO)  = ZERO
      EL_REF(1:NINIEVO)  = ZERO
      ELSCAL(1:NINIEVO)  = ZERO
      DISP(1:NINIEVO)    = ZERO
      ENER(1:NINIEVO)    = ZERO
      ALPHA(1:NINIEVO)   = ZERO
      ! Loop over inievo cards
      DO J = 1, NINIEVO
        ! First line 
        CALL HM_GET_INT_ARRAY_INDEX('INITYPE',INITYPE(J),J,IS_AVAILABLE,LSUBMODEL)
        INITYPE(J) = MIN(INITYPE(J),5)
        IF (INITYPE(J) == 0) INITYPE(J) = 1
        CALL HM_GET_INT_ARRAY_INDEX('EVOTYPE',EVOTYPE(J),J,IS_AVAILABLE,LSUBMODEL)
        EVOTYPE(J) = MAX(EVOTYPE(J),0)
        EVOTYPE(J) = MIN(EVOTYPE(J),2)
        CALL HM_GET_INT_ARRAY_INDEX('EVOSHAP',EVOSHAP(J),J,IS_AVAILABLE,LSUBMODEL)
        EVOSHAP(J) = MIN(EVOSHAP(J),2)
        IF (EVOSHAP(J) == 0) EVOSHAP(J) = 1
        CALL HM_GET_INT_ARRAY_INDEX('COMPTYP',COMPTYP(J),J,IS_AVAILABLE,LSUBMODEL)
        COMPTYP(J) = MIN(COMPTYP(J),2)
        IF (COMPTYP(J) == 0) COMPTYP(J) = 1
        ! Second line
        CALL HM_GET_INT_ARRAY_INDEX  ('TAB_ID',TAB_ID(J),J,IS_AVAILABLE,LSUBMODEL)
        IF (TAB_ID(J) == 0) THEN
          CALL ANCMSG(MSGID=2088, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                I2=J)
        ENDIF
        CALL HM_GET_FLOAT_ARRAY_INDEX('SR_REF',SR_REF(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (SR_REF(J) == ZERO) THEN 
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SR_REF' ,RATE_UNIT,J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          SR_REF(J) = ONE*RATE_UNIT
        ENDIF
        CALL HM_GET_FLOAT_ARRAY_INDEX('FSCALE',FSCALE(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (FSCALE(J) == ZERO) FSCALE(J) = ONE
        CALL HM_GET_FLOAT_ARRAY_INDEX('INI_P1',INI_P1(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ! Third line
        CALL HM_GET_INT_ARRAY_INDEX  ('TAB_EL',TAB_EL(J),J,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('EL_REF',EL_REF(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (EL_REF(J) == ZERO) THEN 
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('EL_REF' ,LENGTH_UNIT,J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          EL_REF(J) = ONE*LENGTH_UNIT
        ENDIF
        CALL HM_GET_FLOAT_ARRAY_INDEX('ELSCAL',ELSCAL(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (ELSCAL(J) == ZERO) ELSCAL(J) = ONE
        ! Fourth line
        CALL HM_GET_FLOAT_ARRAY_INDEX('DISP'  ,DISP(J) ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (EVOTYPE(J) == 1 .AND. DISP(J) == ZERO) THEN 
          CALL ANCMSG(MSGID=2089, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                I2=J)
        ENDIF
        CALL HM_GET_FLOAT_ARRAY_INDEX('ALPHA' ,ALPHA(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (ALPHA(J) == ZERO) ALPHA(J) = ONE
        CALL HM_GET_FLOAT_ARRAY_INDEX('ENER'  ,ENER(J) ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (EVOTYPE(J) == 2 .AND. ENER(J) == ZERO) THEN 
          CALL ANCMSG(MSGID=2090, MSGTYPE=MSGERROR,  ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                I2=J)
        ENDIF
      ENDDO
c
C======================================================================================
c     Filling buffer tables
C======================================================================================
      NTABLE = NINIEVO*2
      ! -> Number of user variables
      NUVAR = 2 + 3*NINIEVO   
      ! -> Number of parameters
      NUPARAM = 5 + NINIEVO*14
c-----------------------------------------------------    
      FAIL%KEYWORD = 'INIEVO' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = NUPARAM
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = NUVAR
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = NTABLE
      FAIL%NMOD    = 0
      FAIL%PTHK    = PTHK
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))

      FAIL%UPARAM(1) = NINIEVO
      FAIL%UPARAM(2) = ISHEAR
      FAIL%UPARAM(3) = ILEN
      FAIL%UPARAM(4) = FAILIP
      FAIL%UPARAM(5) = PTHK
c
      FAIL%TABLE(1:NINIEVO)           = TAB_ID(1:NINIEVO)
      FAIL%TABLE(NINIEVO+1:NINIEVO*2) = TAB_EL(1:NINIEVO)
c      
      DO J = 1,NINIEVO
        FAIL%UPARAM(6  + 14*(J-1)) = INITYPE(J)
        FAIL%UPARAM(7  + 14*(J-1)) = EVOTYPE(J)
        FAIL%UPARAM(8  + 14*(J-1)) = EVOSHAP(J)
        FAIL%UPARAM(9  + 14*(J-1)) = COMPTYP(J)
        FAIL%UPARAM(10 + 14*(J-1)) = 0              !  INT_TAB_ID(J)
        FAIL%UPARAM(11 + 14*(J-1)) = SR_REF(J)
        FAIL%UPARAM(12 + 14*(J-1)) = FSCALE(J)
        FAIL%UPARAM(13 + 14*(J-1)) = INI_P1(J)
        FAIL%UPARAM(14 + 14*(J-1)) = 0              ! INT_TAB_EL(J)
        FAIL%UPARAM(15 + 14*(J-1)) = EL_REF(J) 
        FAIL%UPARAM(16 + 14*(J-1)) = ELSCAL(J)  
        FAIL%UPARAM(17 + 14*(J-1)) = DISP(J) 
        FAIL%UPARAM(18 + 14*(J-1)) = ALPHA(J)
        FAIL%UPARAM(19 + 14*(J-1)) = ENER(J)
      ENDDO    
c-----------------------------------------------------
c     Printout data
c-------------------------- 
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000)
        WRITE(IOUT,1100) NINIEVO,ISHEAR,ILEN
        WRITE(IOUT,1200) FAILIP,PTHK
        DO J = 1,NINIEVO
          WRITE (IOUT,1300) J,INITYPE(J),TAB_ID(J),SR_REF(J),FSCALE(J),
     .                      INI_P1(J),TAB_EL(J),EL_REF(J),ELSCAL(J)
          IF (EVOTYPE(J) == 1) THEN
            IF (EVOSHAP(J) == 1) THEN 
              WRITE (IOUT,1400) EVOTYPE(J),EVOSHAP(J),COMPTYP(J),
     .                          DISP(J)
            ELSEIF (EVOSHAP(J) == 2) THEN  
              WRITE (IOUT,1500) EVOTYPE(J),EVOSHAP(J),COMPTYP(J),
     .                          DISP(J),ALPHA(J)
            ENDIF
          ELSEIF (EVOTYPE(J) == 2) THEN 
              WRITE (IOUT,1600) EVOTYPE(J),EVOSHAP(J),COMPTYP(J),
     .                          ENER(J)
          ENDIF
        ENDDO
        WRITE(IOUT,2000)
      ENDIF
c--------------------------
c     Deallocation
c--------------------------  
      IF (ALLOCATED(INITYPE))  DEALLOCATE(INITYPE)
      IF (ALLOCATED(EVOTYPE))  DEALLOCATE(EVOTYPE)
      IF (ALLOCATED(EVOSHAP))  DEALLOCATE(EVOSHAP)
      IF (ALLOCATED(COMPTYP))  DEALLOCATE(COMPTYP)
      IF (ALLOCATED(TAB_ID))   DEALLOCATE(TAB_ID)
      IF (ALLOCATED(TAB_EL))   DEALLOCATE(TAB_EL)
      IF (ALLOCATED(SR_REF))   DEALLOCATE(SR_REF)
      IF (ALLOCATED(FSCALE))   DEALLOCATE(FSCALE)
      IF (ALLOCATED(INI_P1))   DEALLOCATE(INI_P1)
      IF (ALLOCATED(EL_REF))   DEALLOCATE(EL_REF)
      IF (ALLOCATED(ELSCAL))   DEALLOCATE(ELSCAL)
      IF (ALLOCATED(DISP))     DEALLOCATE(DISP)
      IF (ALLOCATED(ENER))     DEALLOCATE(ENER)
      IF (ALLOCATED(ALPHA))    DEALLOCATE(ALPHA)
c-----------------------------------------------------
 1000 FORMAT(
     & 5X,'  ----------------------------------------------------   ',/
     & 5X,'              FAILURE CRITERION : INIEVO                 ',/,
     & 5X,'  ----------------------------------------------------   ',/)
 1100 FORMAT(
     & 5X,'NUMBER OF INIEVO CRITERIA. . . . . . . . . . . . . . . .=',I10,/,
     & 5X,'TRANSVERSE SHEAR FLAG (SHELLS ONLY)  . . . . . . . . . .=',I10,/,
     & 5X,'  ISHEAR = 0: TRANSVERSE SHEAR STRESS COMPONENTS NOT CONSIDERED ',/,
     & 5X,'  ISHEAR = 1: TRANSVERSE SHEAR STRESS COMPONENTS CONSIDERED     ',/,
     & 5X,'ELEMENT CHARACTERISTIC LENGTH FLAG . . . . . . . . . . .=',I10,/,
     & 5X,'  ILEN   = 0: INITIAL GEOMETRIC FORMULATION              ',/,
     & 5X,'  ILEN   = 1: INITIAL CRITICAL TIMESTEP FORMULATION      ',/,
     & 5X,'  ILEN   = 2: CURRENT GEOMETRIC FORMULATION (SHELLS ONLY)',/)
 1200 FORMAT(
     & 5X,'ELEMENT DELETION PARAMETERS:                             ',/,
     & 5X,'NUMBER OF FAILED INTG. POINTS PRIOR TO SOLID DELETION . =',I10,/,
     & 5X,'SHELL ELEMENT DELETION PARAMETER PTHICKFAIL . . . . . . =',1PG20.13,/,
     & 5X,'  > 0.0 : FRACTION OF FAILED THICKNESS                   ',/,
     & 5X,'  < 0.0 : FRACTION OF FAILED INTG. POINTS OR LAYERS      ',/)
 1300 FORMAT(     
     & 5X,'|| INIEVO CRITERION NUMBER #',I3,/,
     & 5X,'  -------------------------------------------------     ',/,
     & 5X,'> INITIATION PARAMETERS:                                ',/,
     & 5X,'DAMAGE INITIATION TYPE . . . . . . . . . . . . . . . . =',I10,/,
     & 5X,'  TABLE DEFINING PLASTIC STRAIN AT FAILURE WITH:        ',/,
     & 5X,'     = 1: TRIAXIALITY VS STRAIN RATE                    ',/,
     & 5X,'     = 2: SHEAR INFLUENCE VS STRAIN RATE                ',/,
     & 5X,'     = 3: PRINC. STRAIN RATES RATIO VS STRAIN RATE (TRIAX > 0.0)',/,
     & 5X,'     = 4: PRINC. STRAIN RATES RATIO VS STRAIN RATE      ',/,                                     
     & 5X,'     = 5: STRESS STATE PARAMETER VS STRAIN RATE         ',/,
     & 5X,'INITIATION TABLE IDENTIFIER. . . . . . . . . . . . . . =',I10,/,
     & 5X,'   REFERENCE STRAIN RATE  . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'   SCALE FACTOR . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'DAMAGE INITIATION PARAMETER . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'   INI. TYPE = 1: NOT USED                              ',/,
     & 5X,'   INI. TYPE = 2: PRESSURE INFLUENCE PARAMETER          ',/,
     & 5X,'   INI. TYPE = 3: DIRECT / INCREMENTAL FORMULATION      ',/,
     & 5X,'   INI. TYPE = 4: DIRECT / INCREMENTAL FORMULATION      ',/,
     & 5X,'   INI. TYPE = 5: TRIAXIALITY INFLUENCE PARAMETER       ',/,
     & 5X,'ELEMENT SIZE SCALING TABLE IDENTIFIER  . . . . . . . . =',I10,/,
     & 5X,'   REFERENCE SIZE . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'   SCALE FACTOR . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1400 FORMAT(                                                         
     & 5X,'> EVOLUTION PARAMETERS:                                 ',/,
     & 5X,'DAMAGE EVOLUTION TYPE  . . . . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: PLASTIC DISPLACEMENT                          ',/,
     & 5X,'     = 2: FRACTURE ENERGY                               ',/,
     & 5X,'DAMAGE EVOLUTION SHAPE . . . . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: LINEAR                                        ',/,
     & 5X,'     = 2: EXPONENTIAL                                   ',/,
     & 5X,'DAMAGE EVOLUTION COMBINATION . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: MAXIMUM DAMAGE                                ',/,
     & 5X,'     = 2: MULTIPLICATIVE                                ',/,
     & 5X,'PLASTIC DISPLACEMENT AT FAILURE . . . . . . . . . . . .=',1PG20.13/)
 1500 FORMAT(                                                         
     & 5X,'> EVOLUTION PARAMETERS:                                 ',/,
     & 5X,'DAMAGE EVOLUTION TYPE  . . . . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: PLASTIC DISPLACEMENT                          ',/,
     & 5X,'     = 2: FRACTURE ENERGY                               ',/,
     & 5X,'DAMAGE EVOLUTION SHAPE . . . . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: LINEAR                                        ',/,
     & 5X,'     = 2: EXPONENTIAL                                   ',/,
     & 5X,'DAMAGE EVOLUTION COMBINATION . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: MAXIMUM DAMAGE                                ',/,
     & 5X,'     = 2: MULTIPLICATIVE                                ',/,
     & 5X,'PLASTIC DISPLACEMENT AT FAILURE . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'EXPONENTIAL EVOLUTION PARAMETER . . . . . . . . . . . .=',1PG20.13/)
 1600 FORMAT(                                                         
     & 5X,'> EVOLUTION PARAMETERS:                                 ',/,
     & 5X,'DAMAGE EVOLUTION TYPE  . . . . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: PLASTIC DISPLACEMENT                          ',/,
     & 5X,'     = 2: FRACTURE ENERGY                               ',/,
     & 5X,'DAMAGE EVOLUTION SHAPE . . . . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: LINEAR                                        ',/,
     & 5X,'     = 2: EXPONENTIAL                                   ',/,
     & 5X,'DAMAGE EVOLUTION COMBINATION . . . . . . . . . . . . . =',I10,/,
     & 5X,'     = 1: MAXIMUM DAMAGE                                ',/,
     & 5X,'     = 2: MULTIPLICATIVE                                ',/,
     & 5X,'FRACTURE ENERGY . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 2000 FORMAT(
     & 5X,' -------------------------------------------------------',//)
c----------- 
      END
