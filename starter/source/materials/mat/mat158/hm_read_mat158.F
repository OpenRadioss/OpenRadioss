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
Chd|  HM_READ_MAT158                source/materials/mat/mat158/hm_read_mat158.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
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
      SUBROUTINE HM_READ_MAT158(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM       ,LSUBMODEL,MAT_ID   ,TITR     ,MATPARAM )
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
C   READ MAT LAW158 WITH HM READER
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
      INTEGER :: I,ILAW,ISENS
      my_real :: RHO0,KFLEX,KFLEX1,KFLEX2,EMBC,EMBT,ZEROSTRESS,
     .           LC0,LT0,DC0,DT0,HC0,HT0,STRESS_UNIT
      my_real ,DIMENSION(5) ::  YFAC
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW  = 158
      NFUNC = 5
c-----------------------------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
card1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2
      CALL HM_GET_FLOATV('S1'         ,EMBC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('S2'         ,EMBT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FLEX'   ,KFLEX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FLX1'   ,KFLEX1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FLX2'   ,KFLEX2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card3
      CALL HM_GET_FLOATV('Zerostress' ,ZEROSTRESS,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISENSOR'    ,ISENS     ,IS_AVAILABLE, LSUBMODEL)
card4
      CALL HM_GET_INTV  ('FUN_A1'     ,IFUNC(1)  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_C1'     ,YFAC(1)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card5
      CALL HM_GET_INTV  ('FUN_A2'     ,IFUNC(2)  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_C2'     ,YFAC(2)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card6
      CALL HM_GET_INTV  ('FUN_A3'     ,IFUNC(3)  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_C3'     ,YFAC(3)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card7
      CALL HM_GET_INTV  ('FUN_A4'     ,IFUNC(4)  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A5'     ,IFUNC(5)  ,IS_AVAILABLE, LSUBMODEL)
c-----------------------------------------------------------------------
      IF (IFUNC(1) == 0) THEN
       CALL ANCMSG(MSGID=1578 , MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             C1=TITR)
      ENDIF 
      IF (IFUNC(2) == 0)  THEN
       CALL ANCMSG(MSGID=1579 , MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             C1=TITR)
      ENDIF 
      IF (IFUNC(3) == 0) THEN
       CALL ANCMSG(MSGID=1580 , MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             C1=TITR)
      ENDIF
c-----------------------------------------------------------------------
c     Default values
c-----------------------------------------------------------------------
      CALL HM_GET_FLOATV_DIM('MAT_FLEX',STRESS_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      IF (YFAC(1) == ZERO) YFAC(1) = ONE * STRESS_UNIT
      IF (YFAC(2) == ZERO) YFAC(2) = ONE * STRESS_UNIT
      IF (YFAC(3) == ZERO) YFAC(3) = ONE * STRESS_UNIT
      IF (KFLEX1 == ZERO) KFLEX1 = ONE * STRESS_UNIT
      IF (KFLEX2 == ZERO) KFLEX2 = ONE * STRESS_UNIT
      IF (KFLEX  == ZERO) KFLEX  = ONE * STRESS_UNIT
      IF (EMBC  == ZERO) EMBC = EM01
      IF (EMBT  == ZERO) EMBT = EM01
c-----------------------------------------------------------------------
      LC0 = ONE
      LT0 = ONE
      DC0 = LC0 * (ONE + EMBC) 
      DT0 = LT0 * (ONE + EMBT) 
      HC0 = SQRT(DC0*DC0 - LC0*LC0)
      HT0 = SQRT(DT0*DT0 - LT0*LT0)
c-----------------------------------------------------------------------
      NUVAR   = 40   
      NUPARAM = 15
c-----------------------------------------------------------------------
      UPARAM(1:NUPARAM) = ZERO
c
      UPARAM( 1) = DC0
      UPARAM( 2) = DT0
      UPARAM( 3) = HC0
      UPARAM( 4) = HT0
      UPARAM( 5) = KFLEX 
      UPARAM( 6) = KFLEX1
      UPARAM( 7) = KFLEX2
      UPARAM( 8) = ZEROSTRESS
      UPARAM( 9) = ISENS
      UPARAM(10) = ZERO       ! KTMAX  = max fiber stiffness  
      UPARAM(11) = ZERO       ! GMAX   = max shear stiffness  
      UPARAM(12) = YFAC(1)
      UPARAM(13) = YFAC(2)
      UPARAM(14) = YFAC(3)
c--------------------------
!      PARMAT(1) = YOUNG/THREE
!      PARMAT(2) = YOUNG
!      PARMAT(3) = ZERO                                      
!      PARMAT(4) = ZERO                                      
!      PARMAT(5) = ZERO                                      
c--------------------------
      PM(1)  = RHO0
      PM(89) = RHO0
!      PM(23) = YOUNG
c--------------------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"ANISOTROPIC")
c--------------------------
      MTAG%L_ANG  = 1
c--------------------------------------------------
c     Starter output
c--------------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,EMBC,EMBT,KFLEX,KFLEX1,KFLEX2,
     .                   IFUNC(1),IFUNC(2),IFUNC(3),IFUNC(4),IFUNC(5),
     .                   YFAC(1),YFAC(2),YFAC(3),ISENS,ZEROSTRESS
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : ANISOTROPIC FABRIC (LAW158)     ',/,
     & 5X,'--------------------------------------------     ',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . .  .  . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'NOMINAL WARP STRETCH. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'NOMINAL WEFT STRETCH. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'COUPLING STIFFNESS MODULUS. . . . . . . . . . . .=',1PG20.13/
     & 5X,'FLEX MODULUS (WARP) . . . . . . . . .  . . . .  .=',1PG20.13/
     & 5X,'FLEX MODULUS (WEFT) . . . . . . . . .  . . . .  .=',1PG20.13/
     & 5X,'LOADING STRESS FUNCTION ID IN WARP DIRECTION. . .=',I10/
     & 5X,'LOADING STRESS FUNCTION ID IN WEFT DIRECTION. . .=',I10/ 
     & 5X,'LOADING STRESS FUNCTION ID IN SHEAR . . . . . . .=',I10/
     & 5X,'FLEX STRESS FUNCTION ID IN WARP DIRECTION . . . .=',I10/
     & 5X,'FLEX STRESS FUNCTION ID IN WEFT DIRECTION . . . .=',I10/ 
     & 5X,'LOADING FUNCTION SCALE FACTOR (WARP). . . . . . .=',1PG20.13/
     & 5X,'LOADING FUNCTION SCALE FACTOR (WEFT). . . . . . .=',1PG20.13/
     & 5X,'LOADING FUNCTION SCALE FACTOR (SHEAR) . . . . . .=',1PG20.13/
     & 5X,'SENSOR ID . . . . . . . . . . . . . . . . . . . .=',I10/
     & 5X,'REF-STATE STRESS RELAXATION FACTOR. . . . . . . .=',1PG20.13/)
 1250 FORMAT(
     & 5X,'YOUNG MODULUS E1 (WARP DIRECTION) . . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS E2 (WEFT DIRECTION) . . . . . . . .=',1PG20.13/)
c-----------------------------------------------------------------------
      RETURN
      END
