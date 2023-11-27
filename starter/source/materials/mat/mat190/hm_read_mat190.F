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
Chd|  HM_READ_MAT190                source/materials/mat/mat190/hm_read_mat190.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_MAT190(
     .           NUVAR    ,NUMTABL    ,
     .           MAXTABL  ,ITABLE    ,PARMAT  ,UNITAB   ,
     .           PM       ,LSUBMODEL,MAT_ID   ,TITR     ,MATPARAM  ,
     .           NVARTMP  ,IMATVIS) 
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "com04_c.inc"
C----------------------------------------------- 
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXTABL
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                         :: NUVAR,NUMTABL,NVARTMP,IMATVIS
      INTEGER, DIMENSION(MAXTABL)   ,INTENT(INOUT)   :: ITABLE

      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE (MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
      TYPE (TTABLE) TABLE(NTABLE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,ILAW
      my_real :: RHO0,RHOR,NU,BULK,E,EMOD,G,A11, SCAL, SCALINT, TOL1,TOL2,
     .  HU, SHAPE,SCALE_1_UNIT,SCALE_INT_UNIT,XFAC(2)
C=======================================================================
      ILAW = 190
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE   = .FALSE.
      IMATVIS = 2
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
card1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 
      CALL HM_GET_FLOATV('MAT_E'     ,EMOD    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      NU = ZERO  ! Poisson ratio internally set to zero
card3
      CALL HM_GET_FLOATV('MAT_HU'    ,HU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SHAPE' ,SHAPE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TOL1'  ,TOL1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TOL2'  ,TOL2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)


card4
      CALL HM_GET_INTV  ('FUN_1'     ,ITABLE(1),IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('SCALE_1'   ,XFAC(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card5
C-----------------------------------------------
      CALL HM_GET_FLOATV_DIM('SCALE_1'  ,SCALE_1_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
      IF ( XFAC(1) == ZERO) XFAC(1) = ONE * SCALE_1_UNIT
C-----------------------------------------------
      IF(TOL1 == ZERO) TOL1 =  1.e-9
      E    = EMOD
      G    = EMOD / TWO / (ONE+NU)
      BULK = EMOD / THREE / (ONE-TWO * NU)      
      A11 = EMOD * (ONE - NU) / (ONE + NU) / (ONE-TWO * NU)

      IF(HU ==  ZERO) HU = ONE

      NUMTABL   = 2                      
C-------------------------------------------
C User Material Parameters Definition
C-------------------------------------------
      MATPARAM%NUPARAM = 11

      ALLOCATE (MATPARAM%UPARAM(MATPARAM%NUPARAM))

      MATPARAM%UPARAM(1) = EMOD
      MATPARAM%UPARAM(2) = E
      MATPARAM%UPARAM(3) = BULK
      MATPARAM%UPARAM(4) = NU
      MATPARAM%UPARAM(5) = G
      MATPARAM%UPARAM(6) = XFAC(1)

      MATPARAM%UPARAM(8) = HU
      MATPARAM%UPARAM(9) = SHAPE

      MATPARAM%UPARAM(10) = TOL1
      MATPARAM%UPARAM(11) = TOL2
      NUVAR   = 30  ! Number of User Element Variables and Curves
      NVARTMP = 18


      MATPARAM%NTABLE = 2

C---- ---------------------------------------
      PM(1)  = RHO0
      PM(89) = RHO0
C---- ---------------------------------------
      PARMAT(1) = BULK
      PARMAT(2) = E
      PARMAT(3) = NU

      !Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
C-------------------------------------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      IF (NU >= 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ORTHOTROPIC")
C-------------------------------------------
C Output File Print
C-------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1100)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
       WRITE(IOUT,1110)NU,BULK,G,E
       WRITE(IOUT,1150)HU,SHAPE
       WRITE(IOUT,1200)ITABLE(1),XFAC(1)
      ENDIF
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . =',I10/)

 1100  FORMAT(
     & 5X,'MATERIAL MODEL : ISOTROPIC PATH DEPENDENT FOAM   ',/,
     & 5X,'---------------  MODEL - LAW FOAM_DUBOIS        :',/,    
     & 5X,'               ---------------------------------- ',/)
C
 1110 FORMAT(
     & 5X,'POISSONS RATIO. . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'K . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'G . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'YOUNG  MODULUS. . . . . . . . . . . . . .=',E12.4//)
 1150 FORMAT(
     & 5X,'HU. . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'SHAPE . . . . . . . . . . . . . . . . . .=',E12.4//)
 1200 FORMAT(
     & 5X,'TABLE IDENTIFIER. . . . . . . . . . . . .=',I10/ 
     & 5X,'SCALE FOR STRAIN RATE . . . . . . . . . .=',E12.4//)
 1300 FORMAT(
     & 5X,'FIDINT . . . . . .  . . . . . . . . . . .=',I10/ 
     & 5X,'SCALINT  . . . . .  . . . . . . . . . . .=',E12.4//)
C 
C-------------------------------------------------------------------------
      RETURN
      END
