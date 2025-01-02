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
      !||    hm_read_mat190           ../starter/source/materials/mat/mat190/hm_read_mat190.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||    table_mod                ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_MAT190(
     .           NUVAR    ,NUMTABL  ,MAXTABL  ,ITABLE   ,PARMAT   ,UNITAB   ,    
     .           PM       ,LSUBMODEL,MAT_ID   ,TITR     ,MATPARAM ,NVARTMP  ,
     .           IMATVIS  ) 
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   FOAM_DUBOIS path dependent foam material 
C   isotropic material law for foam
C-------------------------------------------------------------------------
C-------------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C-----------------------------------------------
C     IPM             MATERIAL ARRAY(INTEGER)
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
C----------------------------------------------- 
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)                  :: UNITAB 
      INTEGER, INTENT(IN)                           :: MAT_ID,MAXTABL
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM     
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)         :: TITR
      INTEGER, INTENT(INOUT)                        :: NUVAR,NUMTABL,NVARTMP,IMATVIS
      INTEGER, DIMENSION(MAXTABL) ,INTENT(INOUT)    :: ITABLE
      my_real, DIMENSION(100),INTENT(INOUT)         :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN)  :: LSUBMODEL
      TYPE (MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,ILAW
      my_real :: RHO0,NU,BULK,E,EMOD,G,A11,SCAL,HU, SHAPE,
     .    SCALE_1_UNIT,XSCALE_1_UNIT,XFAC(2)
C=======================================================================
      ILAW = 190
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE   = .FALSE.
      IMATVIS = 2
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
card1
      ! Initial density
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 
      ! Initial young modulus
      CALL HM_GET_FLOATV('MAT_E'     ,EMOD    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      NU = ZERO
card3
      ! Unloading parameters
      CALL HM_GET_FLOATV('MAT_HU'    ,HU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SHAPE' ,SHAPE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card4
      ! Loading table
      CALL HM_GET_INTV  ('FUN_1'     ,ITABLE(1),IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('XSCALE_1'  ,XFAC(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SCALE_1'   ,XFAC(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      ! Check scale factor units
      CALL HM_GET_FLOATV_DIM('XSCALE_1' ,XSCALE_1_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (XFAC(1) == ZERO) XFAC(1) = ONE * XSCALE_1_UNIT
      CALL HM_GET_FLOATV_DIM('SCALE_1'  ,SCALE_1_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (XFAC(2) == ZERO) XFAC(2) = ONE * SCALE_1_UNIT      
c
      ! Check default value and compute elastic parameters
      E    = EMOD
      G    = EMOD / TWO / (ONE+NU)
      BULK = EMOD / THREE / (ONE-TWO * NU)      
      A11  = EMOD * (ONE - NU) / (ONE + NU) / (ONE-TWO * NU)
      IF (HU == ZERO) HU = ONE                    
c
!--------------------------
!     Filling buffer tables
!-------------------------- 
      NUVAR   = 13         ! Number of User Element Variables and Curves
      NUMTABL = 2          ! Number of tables
      NVARTMP = 18         ! Number of temporary variables
      MATPARAM%NUPARAM = 7 ! Number of real parameters
c
      ! Allocate and fill real parameters table
      ALLOCATE(MATPARAM%UPARAM(MATPARAM%NUPARAM))
      MATPARAM%UPARAM(1) = EMOD
      MATPARAM%UPARAM(2) = BULK
      MATPARAM%UPARAM(3) = G
      MATPARAM%UPARAM(4) = XFAC(1)
      MATPARAM%UPARAM(5) = XFAC(2)
      MATPARAM%UPARAM(6) = HU
      MATPARAM%UPARAM(7) = SHAPE
c
      ! PM table
      PM(1)  = RHO0
      PM(89) = RHO0
c
      ! PARMAT table
      PARMAT(1)  = BULK
      PARMAT(2)  = E
      PARMAT(3)  = NU
      PARMAT(16) = 2
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU)
c
      ! Material settings
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
c
!--------------------------
!     Parameters printout
!--------------------------
      WRITE(IOUT,900) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1050) RHO0
        WRITE(IOUT,1110) E,BULK,G
        WRITE(IOUT,1150) HU,SHAPE
        WRITE(IOUT,1200) ITABLE(1),XFAC(1),XFAC(2)
      ENDIF
 900  FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . . . =',I10/)
 1000 FORMAT(/                                                        
     & 5X,'-----------------------------------------------',/          
     & 5X,' MATERIAL MODEL: ISOTROPIC PATH DEPENDENT FOAM ',/,      
     & 5X,'-----------------------------------------------',/)
 1050 FORMAT(/
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . . . . .=',1PG20.13/)  
 1110 FORMAT(/
     & 5X,'INITIAL YOUNG MODULUS E. . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'INITIAL BULK MODULUS K . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'INITIAL SHEAR MODULUS G. . . . . . . . . . . .=',1PG20.13/)
 1150 FORMAT(/
     & 5X,'HYSTERETIC UNLOADING FACTOR HU . . . . . . . .=',1PG20.13/ 
     & 5X,'SHAPE FACTOR FOR UNLOADING . . . . . . . . . .=',1PG20.13/)
 1200 FORMAT(/
     & 5X,'LOADING TABLE IDENTIFIER . . . . . . . . . . .=',I10/ 
     & 5X,'REFERENCE STRAIN RATE (SCALE FACTOR) . . . . .=',1PG20.13/
     & 5X,'LOADING STRESS SCALE FACTOR. . . . . . . . . .=',1PG20.13/)
c
      END
