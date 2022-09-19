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
Chd|  HM_READ_MAT109                source/materials/mat/mat109/hm_read_mat109.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT109(UPARAM   ,MAXUPARAM,NUPARAM   ,NUVAR  ,NVARTMP  ,
     .                          ITABLE   ,MAXTABL  ,NTABL     ,PARMAT ,UNITAB   ,
     .                          MAT_ID   ,TITR     ,RHO       ,MTAG   ,MATPARAM ,
     .                          LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE HM_OPTION_READ_MOD
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
      INTEGER ,INTENT(IN)  :: MAT_ID,MAXTABL,MAXUPARAM
      CHARACTER*nchartitle ,INTENT(IN) :: TITR
      my_real ,INTENT(OUT) :: RHO
      my_real ,DIMENSION(100)       ,INTENT(OUT)  :: PARMAT
      my_real ,DIMENSION(MAXUPARAM) ,INTENT(OUT)  :: UPARAM
      INTEGER ,DIMENSION(MAXTABL)   ,INTENT(OUT)  :: ITABLE
      INTEGER ,INTENT(INOUT)          :: NTABL,NUPARAM,NUVAR,NVARTMP
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE (UNIT_TYPE_)  ,INTENT(IN)  :: UNITAB
      TYPE(MATPARAM_STRUCT_) ,INTENT(OUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ISMOOTH,ILAW,TAB_YLD,TAB_TEMP,TAB_ETA
      my_real :: YOUNG,NU,G,LAME,BULK,A1,A2,CP,TREF,TINI,ETA,BETA,XRATE,
     .           XSCALE,YSCALE,YSCALE_UNIT,XSCALE_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 109
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------

Card1
      CALL HM_GET_FLOATV('MAT_RHO'       ,RHO      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2
      CALL HM_GET_FLOATV('MAT_E'         ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'        ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c      CALL HM_GET_INTV  ('TAB_E'        ,TAB_E    ,IS_AVAILABLE,LSUBMODEL)
Card3  
      CALL HM_GET_FLOATV('MAT_SPHEAT'    ,CP       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA'       ,ETA      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('WPREF'         ,TREF     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('T_Initial'     ,TINI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card4
      CALL HM_GET_INTV  ('MAT_TAB_YLD'   ,TAB_YLD  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_TAB_TEMP'  ,TAB_TEMP ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_Xscale'    ,XSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Yscale'    ,YSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Ismooth'   ,ISMOOTH  ,IS_AVAILABLE,LSUBMODEL)
Card6   
      CALL HM_GET_INTV  ('TAB_ETA'       ,TAB_ETA  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_Xrate'     ,XRATE    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c------------------------
c     Elastic parameters
c------------------------
      G    = HALF *YOUNG / (ONE + NU)
      LAME = TWO * G * NU /(ONE - TWO*NU)
      BULK = THIRD * YOUNG / (ONE - NU*TWO)
c      SOUNDSP = SQRT((BULK + FOUR_OVER_3*G)/RHO)
c------------------------
c     Default input values
c------------------------
      IF (ISMOOTH == 0)   ISMOOTH = 1
      IF (TREF   == ZERO) TREF    = 293.0
      IF (TINI   == ZERO) TINI    = TREF
      IF (CP > ZERO) THEN
        BETA = ETA / (RHO*CP)
      ELSE
        BETA = ZERO
      END IF
      IF (YSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_Yscale' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        YSCALE = ONE * YSCALE_UNIT
      ENDIF
      IF (XSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_Xscale' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        XSCALE = ONE * XSCALE_UNIT
      ENDIF
      IF (XRATE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_XRATE' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        XRATE = ONE * XSCALE_UNIT
      ENDIF
c-----------------------------------------------
      A1   = YOUNG / (ONE - NU*NU)
      A2   = A1 * NU
c-----------------------------------------------
      UPARAM(1)  = YOUNG     ! Young modulus
      UPARAM(2)  = NU        ! Poisson ratio
      UPARAM(3)  = BETA      ! Thermal work coefficient
      UPARAM(4)  = TREF      ! Reference temperature
      UPARAM(5)  = TINI      ! Initial temperature
      UPARAM(6)  = ISMOOTH   ! table interpolation flag:
                             ! ISMOOTH = 1 => linear interpolation
                             ! ISMOOTH = 2 => logarythmic interpolation base 10
                             ! ISMOOTH = 3 => logarythmic interpolation base n
      UPARAM(7)  = ONE/XRATE  ! strain rate abscissa factor for eta function
      UPARAM(8)  = ONE/XSCALE ! strain rate abscissa factor for yld function
      UPARAM(9)  = YSCALE    ! Yld function scale factor
      UPARAM(10) = 0
      UPARAM(11) = G         ! Shear modulus
      UPARAM(12) = G * TWO
      UPARAM(13) = G * THREE
      UPARAM(14) = BULK      ! Bulk modulus
      UPARAM(15) = LAME      ! Lame parameter
      UPARAM(16) = A1
      UPARAM(17) = A2
      UPARAM(18) = NU / (ONE - NU)              ! NNU
      UPARAM(19) = (ONE - TWO*NU) / (ONE - NU)  ! NNU1
c----------------
      PARMAT(1) = BULK
      PARMAT(2) = YOUNG
      PARMAT(3) = NU
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G / (BULK + FOUR_OVER_3*G)
c----------------
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%G_TEMP = 1
      MTAG%L_TEMP = 1
c----------------
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN"  )
c----------------
      NUPARAM = 19
      NUVAR   = 0
      NTABL   = 3
c
      ITABLE(1) = TAB_YLD     ! Yield function table              = f(epsp,epsdot)
      ITABLE(2) = TAB_TEMP    ! Temperature scale factor table    = f(epsp,T)
      ITABLE(3) = TAB_ETA     ! Taylor-Quinney scale factor table = f(epsp,epsdot,T)
      IF (TAB_TEMP > 0 .or. TAB_ETA > 0) THEN
        NVARTMP  = 5
      ELSE
        NVARTMP  = 1
      ENDIF
c-----------------------
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1100) RHO,YOUNG,NU,CP,TREF,TINI,ETA,TAB_YLD,TAB_TEMP,
     .                   TAB_ETA,ISMOOTH,XRATE,XSCALE,YSCALE
      ENDIF
c-----------
      RETURN
c-----------
 1000 FORMAT(
     & 5X,'    TABULATED ELASTIC PLASTIC LAW 109     ',/
     & 5X,'    ---------------------------------     ')
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SPECIFIC HEAT COEFFICIENT . . . . . . . . .=',1PG20.13/
     & 5X,'REFERENCE TEMPERATURE . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL TEMPERATURE . . . . . . . . . . . .=',1PG20.13/
     & 5X,'TAYLOR-QUINNEY COEFFICIENT. . . . . . . . .=',1PG20.13/
     & 5X,'YIELD-STRAIN RATE TABLE ID. . . . . . . . .=',I10/
     & 5X,'YIELD-TEMPERATURE TABLE ID. . . . . . . . .=',I10/
     & 5X,'HEAT FRACTION TABLE ID. . . . . . . . . . .=',I10/
     & 5X,'INTERPOLATION FLAG. . . . . . . . . . . . .=',I10/
     & 5X,'STRAIN RATE SCALE FACTOR FOR HEAT TABLE . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR FOR YLD TABLES . .=',1PG20.13/
     & 5X,'SCALE FACTOR FOR YLD-STRAIN RATE TABLE. . .=',1PG20.13//)
c-----------
      RETURN
      END
