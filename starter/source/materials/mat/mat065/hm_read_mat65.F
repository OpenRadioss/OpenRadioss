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
Chd|  HM_READ_MAT65                 source/materials/mat/mat065/hm_read_mat65.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT65(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM       ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     ,
     .           MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW50 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
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
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, INTENT(INOUT)                         :: NUPARAM,NUVAR,NFUNC
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)   :: IFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,ILAW,NRATE,ISRAT
      my_real :: RHO0,RHOR,E,NU,G,C1,EPSMAX,FCUT,PRESS_SCALE
      INTEGER ,DIMENSION(MAXFUNC) ::  FUN_LOAD,FUN_UNLOAD
      my_real ,DIMENSION(MAXFUNC) ::  YFAC,RATE
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW   = 65
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c      
      CALL HM_GET_FLOATV('MAT_E'        ,E         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,NU        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'      ,EPSMAX    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('NRATEP'       ,NRATE     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Fsmooth'      ,ISRAT     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'         ,Fcut      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV_DIM('MAT_E',PRESS_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      DO I=1,NRATE    
        CALL HM_GET_INT_ARRAY_INDEX('FUN_LOAD'  ,FUN_LOAD(I)  ,I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INT_ARRAY_INDEX('FUN_UNLOAD',FUN_UNLOAD(I),I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_LOAD',YFAC(I)     ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_LOAD',RATE(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDDO
c
c--------------------------------------------------
      NUPARAM = NRATE*3 + 14
      NUVAR   = NRATE*4 + 8 
      NFUNC   = NRATE*2
c--------------------------------------------------
      IFUNC(1:NRATE)       = FUN_LOAD(1:NRATE)
      IFUNC(NRATE+1:NFUNC) = FUN_UNLOAD(1:NRATE)
c--------------------------------------------------
c     Warning / Error messages
c--------------------------------------------------
      DO I=1,NFUNC
        IF (IFUNC(I) ==  0) THEN
          CALL ANCMSG(MSGID=126,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=IFUNC(I))
        ENDIF
      ENDDO
c
      IF (NRATE > 1 .AND. ISRAT == 0) THEN 
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF       
c
      IF (NRATE > 1) THEN 
        DO I=2,NRATE
          IF (RATE(I) < RATE(I-1)) THEN
            CALL ANCMSG(MSGID=478,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR)
            EXIT
          ENDIF
        ENDDO
      ENDIF
c
c--------------------------------------------------
c     Default values
c--------------------------------------------------
      DO I=1,NRATE
        IF (YFAC(I) == ZERO) YFAC(I) = ONE*PRESS_SCALE
      ENDDO
c
      IF (EPSMAX == ZERO) EPSMAX = EP20
c--------------------------------------------------
      C1 = THIRD*E/(ONE - TWO*NU)
      G  = HALF*E/(ONE+NU)
      ISRATE = MAX(ISRATE,ISRAT)
c---------------------------
      UPARAM(1) = NRATE
      UPARAM(2) = E
      UPARAM(3) = G
      UPARAM(4) = NU
      UPARAM(5) = ONE/(ONE - NU*NU)
      UPARAM(6) = NU*UPARAM(5)
      UPARAM(7) = G*TWO
      UPARAM(8) = G*THREE
      UPARAM(9) = C1
      UPARAM(10)= C1 + G*FOUR*THIRD
      UPARAM(11)= EPSMAX 
      DO I=1,NRATE
        UPARAM(14+I)=RATE(I)
        UPARAM(14+I+NRATE)=YFAC(I)
        UPARAM(14+I+NRATE*2)=EP20
      ENDDO
c-----------------
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRAT
      PARMAT(5) = FCUT
c
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
c-----------------
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(100)= C1 
c-----------------
c     Element buffer variable allocation
c-----------------
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
c-----------------
      ! Properties compatibility  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC") 
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC") 
c--------------------------------------------------
c     Starter output
c--------------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,65
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) E,NU,EPSMAX,ISRATE,FCUT
        DO I=1,NRATE
          WRITE(IOUT,1400) FUN_LOAD(I),FUN_UNLOAD(I),YFAC(I),RATE(I)
        ENDDO
        WRITE(IOUT,*)' '
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . .=',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : TABULATED ELASTIC PLASTIC (LAW65) ',/,
     & 5X,'---------------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON''S RATIO . . . . . . . . . . . .=',1PG20.13/
     & 5X,'MAXIMUM PLASTIC STRAIN  . . . . . . . . =',1PG20.13/
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . . =',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . . =',1PG20.13/)
 1400 FORMAT(
     & 5X,'LOAD STRESS FUNCTION NUMBER   . . . . . =',I10/
     & 5X,'UNLOAD STRESS FUNCTION NUMBER . . . . . =',I10/
     & 7X,'YIELD SCALE FACTOR. . . . . . . . . . . =',1PG20.13/
     & 7X,'STRAIN RATE . . . . . . . . . . . . . . =',1PG20.13)
c-----------------------------------------------------------------------
      RETURN
      END
