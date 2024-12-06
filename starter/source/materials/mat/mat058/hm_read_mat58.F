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
      !||====================================================================
      !||    hm_read_mat58            ../starter/source/materials/mat/mat058/hm_read_mat58.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_MAT58(MATPARAM ,NUVAR    ,NFUNC    ,
     .                         MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,
     .                         UNITAB   ,LSUBMODEL,MAT_ID   ,TITR     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW58 WITH HM READER
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
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)    :: MAT_ID,MAXFUNC
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: NUVAR,NFUNC
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT) :: IFUNC
      my_real, DIMENSION(100)       ,INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_)      ,INTENT(IN)    :: UNITAB 
      TYPE(MLAW_TAG_)        ,INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,ILAW,NC,NT,ISENS,ILOAD,ULOAD
      my_real :: RHO0,RHOR,YOUNG,EC,ET,BC,BT,G,G0,GT,GB,GSH,GFROT,
     .   KC,KT,KKC,KKT,KXC,KXT,KFC,KFT,FLEX,FLEX1,FLEX2,EMBC,EMBT,
     .   LC0,LT0,DC0,DT0,HC0,HT0,COSIN,TAN_LOCK,PHI_LOCK,
     .   VISCE,VISCG,AREAMIN1,AREAMIN2,ZEROSTRESS,STRESS_UNIT
      my_real ,DIMENSION(6) ::  YFAC
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW  = 58
      ILOAD = 0
      NFUNC = 3
      AREAMIN1 = ZERO
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
      CALL HM_GET_FLOATV('MAT_RHO'       ,RHO0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'     ,RHOR      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c      
      CALL HM_GET_FLOATV('MAT_E1'        ,EC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B1'        ,BC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E2'        ,ET        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B2'        ,BT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_F'         ,FLEX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_G0'        ,G0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GI'        ,GT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA'     ,PHI_LOCK  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G5'        ,GSH       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISENSOR'       ,ISENS     ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_Df'        ,VISCE     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_dS'        ,VISCG     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Friction_phi'  ,GFROT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('M58_Zerostress',ZEROSTRESS,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('N1_warp'       ,NC        ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('N2_weft'       ,NT        ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('S1'            ,EMBC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('S2'            ,EMBT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C4'        ,FLEX1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C5'        ,FLEX2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     Optional tabulated function data
c
      CALL HM_GET_INTV  ('FUN_A1'        ,IFUNC(1)  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_C1'        ,YFAC(1)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('FUN_A2'        ,IFUNC(2)  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_C2'        ,YFAC(2)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('FUN_A3'        ,IFUNC(3)  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_C3'        ,YFAC(3)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('FUN_A4'        ,IFUNC(4)  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A5'        ,IFUNC(5)  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('scale4'        ,YFAC(4)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale5'        ,YFAC(5)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A6'        ,IFUNC(6)  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('scale6'        ,YFAC(6)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c-----------------------------------------------------------------------
c     Check consistency of tabulated input data (loading and unloading)
c     a) there's no unloading functions => loading curves are optional
c        analytic and tabulated loading may be mixed
c     b) at least one unloading curve is defined => all loading corves must be defined
c        missing unloading curves may be created by Radioss by copying the loading ones
c-----------------------------------------------------------------------
       IF (IFUNC(1) /= 0 .or. IFUNC(2) /= 0 .or. IFUNC(3) /= 0) THEN
         ILOAD = 1
c
         IF (IFUNC(4) /= 0 .or. IFUNC(5) /= 0 .or. IFUNC(6) /= 0) THEN
           NT = 1
           NC = 1
           NFUNC = 6
           ILOAD = 2
c          if unloading is active, all unloading functions must be properly defined
           IF (IFUNC(4) == 0) THEN
             IFUNC(4) = IFUNC(1)
             YFAC(4)  = YFAC(1)
           ENDIF
           IF (IFUNC(5) == 0) THEN
             IFUNC(5) = IFUNC(2)
             YFAC(5)  = YFAC(2)
           ENDIF
           IF (IFUNC(6) == 0) THEN
             IFUNC(6) = IFUNC(3)
             YFAC(6)  = YFAC(3)
           ENDIF

           IF (IFUNC(1) == 0) THEN
            CALL ANCMSG(MSGID=1578 ,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=MAT_ID,
     .                  C1=TITR)
           ENDIF 
           IF (IFUNC(2) == 0)  THEN
            CALL ANCMSG(MSGID=1579 ,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=MAT_ID,
     .                  C1=TITR)
           ENDIF 
           IF (IFUNC(3) == 0) THEN
            CALL ANCMSG(MSGID=1580 ,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=MAT_ID,
     .                  C1=TITR)
           ENDIF  
         ENDIF  
       ENDIF
c-----------------------------------------------------------------------
c     Default values
c-----------------------------------------------------------------------
      CALL HM_GET_FLOATV_DIM('MAT_E1',STRESS_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      DO I=1,6       
        IF (YFAC(I) == ZERO) YFAC(I) = ONE * STRESS_UNIT
      ENDDO
c
      IF (NC == 0) NC = 1
      IF (NT == 0) NT = 1
      IF (EMBC == ZERO) EMBC = EM01
      IF (EMBT == ZERO) EMBT = EM01
      IF (FLEX == ZERO) FLEX = EM03
      IF (FLEX1  == ZERO .AND. FLEX2 == ZERO)THEN
          FLEX1  = FLEX
          FLEX2  = FLEX
      ELSEIF (FLEX1  == ZERO .AND. FLEX2 /= ZERO)THEN
          FLEX1 = FLEX2
      ELSEIF (FLEX2  == ZERO .AND. FLEX1 /= ZERO)THEN
          FLEX2 = FLEX1
      ENDIF
c
      IF (ILOAD == 2) THEN
        ULOAD = 1
      ELSE
        ULOAD = 0
      ENDIF
      IF (GT == ZERO) GT = FOURTH*(EC + ET)
c-----------------------------------------------------------------------
      LC0 = ONE / NT
      LT0 = ONE / NC
      DC0 = LC0*(ONE+EMBC) 
      DT0 = LT0*(ONE+EMBT) 
      HC0 = SQRT(DC0*DC0 - LC0*LC0)
      HT0 = SQRT(DT0*DT0 - LT0*LT0)
c---  rigidite fil
      KC  = EC/NC
      KT  = ET/NT
      KKC = BC/NC
      KKT = BT/NT
c---  rigidite flexion
      KFC = FLEX1*KC*HC0/DC0
      KFT = FLEX2*KT*HT0/DT0
c
c---  angle blocage cisaillement
      IF (PHI_LOCK == ZERO) THEN
        COSIN = HALF*(HC0/LC0 + HT0/LT0)
        TAN_LOCK = SQRT(ONE - COSIN*COSIN) / COSIN
        PHI_LOCK = ATAN(TAN_LOCK)
      ELSE
        PHI_LOCK = PHI_LOCK*PI/HUNDRED80
        TAN_LOCK = TAN(PHI_LOCK)
      ENDIF
c
      G = GT / (ONE + TAN_LOCK*TAN_LOCK)
      IF (G0 == ZERO) G0 = G 
      GB = TAN_LOCK*(G0 - G)
c
      IF (GFROT == ZERO .and. ILOAD == 0) GFROT = G0
      IF (GSH   == ZERO .and. ILOAD == 0) GSH   = G0
c-----------------------------------------------------------------------
      NUVAR   = 40   
c-----------------------------------------------------------------------
      MATPARAM%NUPARAM = 46 ! 4pts pour l intersection +flag+PR SHEAR
      MATPARAM%NIPARAM = 4
      MATPARAM%NFUNC   = NFUNC
!
      ALLOCATE (MATPARAM%UPARAM(MATPARAM%NUPARAM))
      ALLOCATE (MATPARAM%IPARAM(MATPARAM%NIPARAM))
      MATPARAM%UPARAM(:) = ZERO 
      MATPARAM%IPARAM(:) = 0 
c-----------------------------------------------------------------------
      MATPARAM%IPARAM(1) = ULOAD
      MATPARAM%IPARAM(2) = ISENS
      MATPARAM%IPARAM(3) = NC
      MATPARAM%IPARAM(4) = NT
!
      MATPARAM%UPARAM( 1) = LC0
      MATPARAM%UPARAM( 2) = LT0
      MATPARAM%UPARAM( 3) = DC0
      MATPARAM%UPARAM( 4) = DT0
      MATPARAM%UPARAM( 5) = HC0
      MATPARAM%UPARAM( 6) = HT0
      MATPARAM%UPARAM( 7) = 0  ! moved to IPARAM (NC)
      MATPARAM%UPARAM( 8) = 0  ! moved to IPARAM (NT) 
      MATPARAM%UPARAM( 9) = KC
      MATPARAM%UPARAM(10) = KT
      MATPARAM%UPARAM(11) = KFC
      MATPARAM%UPARAM(12) = KFT
      MATPARAM%UPARAM(13) = G0 
      MATPARAM%UPARAM(14) = G 
      MATPARAM%UPARAM(15) = GB
      MATPARAM%UPARAM(16) = TAN_LOCK
      MATPARAM%UPARAM(17) = VISCE
      MATPARAM%UPARAM(18) = VISCG
      MATPARAM%UPARAM(19) = KKC
      MATPARAM%UPARAM(20) = KKT
      MATPARAM%UPARAM(21) = GFROT
      MATPARAM%UPARAM(22) = AREAMIN1
      AREAMIN2 = ONE + HALF*(AREAMIN1-ONE)
      IF (AREAMIN2 > AREAMIN1) THEN
        MATPARAM%UPARAM(23)= ONE / (AREAMIN2-AREAMIN1)
      ELSE
        MATPARAM%UPARAM(23)= ZERO
      ENDIF
      MATPARAM%UPARAM(24) = ZEROSTRESS
      MATPARAM%UPARAM(25) = 0  ! not used
      MATPARAM%UPARAM(26) = FLEX1
      MATPARAM%UPARAM(27) = FLEX2
      MATPARAM%UPARAM(28) = YFAC(1)
      MATPARAM%UPARAM(29) = YFAC(2)
      MATPARAM%UPARAM(30) = YFAC(3)
      MATPARAM%UPARAM(31) = 0 ! not used
      MATPARAM%UPARAM(32) = GSH
      MATPARAM%UPARAM(33) = YFAC(4)
      MATPARAM%UPARAM(34) = YFAC(5)
      MATPARAM%UPARAM(35) = 0  ! not used
      MATPARAM%UPARAM(42) = YFAC(6)
c-----------------------------------------------------------------------
      YOUNG = MAX(KC,KT)
c--------------------------
      PARMAT(1) = YOUNG/THREE
      PARMAT(2) = YOUNG
      PARMAT(3) = ZERO
      PARMAT(4) = ZERO
      PARMAT(5) = ZERO
c--------------------------
      MATPARAM%RHO   = RHOR
      MATPARAM%RHO0  = RHO0
      MATPARAM%YOUNG = YOUNG
c--------------------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"ANISOTROPIC")
c
      ! Properties compatibility       
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ANISOTROPIC") 
c--------------------------
      MTAG%L_ANG  = 1
c--------------------------------------------------
c     Starter output
c--------------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,58
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1250) EC,ET
        IF (ILOAD == 0) THEN
          WRITE(IOUT,1300) BC,BT,G0,GT,PHI_LOCK*HUNDRED80/PI
        ELSE
          WRITE(IOUT,1400) IFUNC(1),IFUNC(2),IFUNC(3),YFAC(1),YFAC(2),YFAC(3)
          IF (ILOAD == 2)
     .    WRITE(IOUT,1500) IFUNC(4),IFUNC(5),IFUNC(6),YFAC(4),YFAC(5),YFAC(6)
        ENDIF
        WRITE(IOUT,1600) VISCE,VISCG,GFROT,GSH,ZEROSTRESS,
     .        EMBC,EMBT,NC,NT,ISENS,FLEX1,FLEX2
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : ANISOTROPIC FABRIC (LAW58) ',/,
     & 5X,'--------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . .  .  . . . . . . . . . . .=',1PG20.13/)  
 1250 FORMAT(
     & 5X,'YOUNG MODULUS E1 (WARP DIRECTION) . . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS E2 (WEFT DIRECTION) . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'SOFTENING COEFFICIENT B1. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SOFTENING COEFFICIENT B2. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL SHEAR MODULUS . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LOCK SHEAR MODULUS. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR LOCK ANGLE. . . . . . . . . . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'LOADING STRESS FUNCTION ID IN WARP DIRECTION. . .=',I10/
     & 5X,'LOADING STRESS FUNCTION ID IN WEFT DIRECTION. . .=',I10/ 
     & 5X,'LOADING STRESS FUNCTION ID IN SHEAR . . . . . . .=',I10/
     & 5X,'LOADING FUNCTION SCALE FACTOR (WARP). . . . . . .=',1PG20.13/
     & 5X,'LOADING FUNCTION SCALE FACTOR (WEFT). . . . . . .=',1PG20.13/
     & 5X,'LOADING FUNCTION SCALE FACTOR (SHEAR) . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'UNLOADING STRESS FUNCTION ID IN WARP DIRECTION. .=',I10/
     & 5X,'UNLOADING STRESS FUNCTION ID IN WEFT DIRECTION. .=',I10/ 
     & 5X,'UNLOADING STRESS FUNCTION ID IN SHEAR DIRECTION .=',I10/
     & 5X,'UNLOADING FUNCTION SCALE FACTOR (WARP). . . . . .=',1PG20.13/
     & 5X,'UNLOADING FUNCTION SCALE FACTOR (WEFT). . . . . .=',1PG20.13/
     & 5X,'UNLOADING FUNCTION SCALE FACTOR (SHEAR) . . . . .=',1PG20.13/)
 1600 FORMAT(
     & 5X,'FIBER VISCOSITY COEF. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FRICTION COEF . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FRICTION MODULUS. . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'TRANSVERSE SHEAR MODULUS. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'REF-STATE STRESS RELAXATION FACTOR. . . . . . . .=',1PG20.13/
     & 5X,'NOMINAL WARP STRETCH. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'NOMINAL WEFT STRETCH. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'FIBER DENSITY IN WARP DIRECTION . . . . . . . . .=',I10/
     & 5X,'FIBER DENSITY IN WEFT DIRECTION . . . . . . . . .=',I10/ 
     & 5X,'SENSOR ID . . . . . . . . . . . . . . . . . . . .=',I10/
     & 5X,'FLEX MODULUS REDUCTION FACTOR (WARP). . . . . . .=',1PG20.13/
     & 5X,'FLEX MODULUS REDUCTION FACTOR (WEFT). . . . . . .=',1PG20.13)
c-----------------------------------------------------------------------
      RETURN
      END
