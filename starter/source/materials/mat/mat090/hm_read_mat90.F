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
Chd|  HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_MAT90(UPARAM  ,MAXUPARAM,NUPARAM ,NUVAR    ,IFUNC    ,
     .                         MAXFUNC ,NFUNC    ,PARMAT  ,UNITAB   ,ID       ,
     .                         TITR    ,ISRATE   ,PM      ,IMATVIS  ,LSUBMODEL,
     .                         MTAG    ,MATPARAM )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW90 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
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
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE ELBUFTAG_MOD  
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
      my_real, INTENT(INOUT)          :: PARMAT(100),UPARAM(MAXUPARAM),PM(NPROPM)
      INTEGER, INTENT(INOUT)          :: IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,ISRATE,IMATVIS
      INTEGER, INTENT(IN)             :: ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFLAG,IDAM ,ISMOOTH     
      my_real E0,NU,FCUT,G,C1,SHAPE,HYS,ALPHA
      my_real RATE(1000),YFAC(1000),YFAC_DIM(1000)
      my_real :: RHO0, RHOR,FCUT_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IDAM = 0
      ISRATE = 0
      IMATVIS = 2

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E0'    ,E0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('NL'        ,NFUNC   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Ismooth'   ,ISMOOTH ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'      ,FCUT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SHAPE' ,SHAPE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Hys'       ,HYS     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA' ,ALPHA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!   IDAM  ---> not documented, not included within LAW90.cfg
!!      CALL HM_GET_INTV  ('IDAM'      ,IDAM    ,IS_AVAILABLE, LSUBMODEL)
!
      IF (NFUNC == 0) THEN
          CALL ANCMSG(MSGID=866,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND,
     .                I1=ID,
     .                C1=TITR)
      ENDIF 
!
!... fonction de charge      
!
      DO I=1,NFUNC
        CALL HM_GET_INT_ARRAY_INDEX      ('fct_IDL'     ,IFUNC(I)    ,I ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX    ('EpsilondotL' ,RATE(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX    ('FscaleL'     ,YFAC(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('FscaleL'     ,YFAC_DIM(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ENDDO
!
      DO I=1,NFUNC
        IF (IFUNC(I) == 0) THEN
          CALL ANCMSG(MSGID=126,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IFUNC(I))
        ENDIF
        IF(YFAC(I) == ZERO) YFAC(I) = YFAC_DIM(I) * ONE
      ENDDO
      HYS = ABS(HYS)
      IFLAG = 0
      IF (NFUNC > 0  .AND. HYS == ZERO) IFLAG = 1     
      IF (NFUNC > 0  .AND. HYS > ZERO)  IFLAG  = 2    ! unloading is quasi-static
      IF (SHAPE == ZERO) SHAPE = ONE
      IF (ALPHA == ZERO) ALPHA = ONE
      IF (HYS  == ZERO) HYS = ONE
      IF (RHOR==ZERO) RHOR=RHO0
      IF (FCUT == ZERO .AND. NFUNC > 1 ) THEN
        FCUT = EP03*UNITAB%FAC_T_WORK
        ISMOOTH = 1  
      ENDIF
      IF(ISMOOTH == 0 .AND. FCUT > ZERO .AND. NFUNC > 1) ISMOOTH = 1
      IF(NFUNC == 1) ISMOOTH = 0
      PM(1) = RHOR
      PM(89)= RHO0
        
      G = HALF*E0/(ONE + NU)
      UPARAM(1) = E0
      UPARAM(4) = G
      UPARAM(5) = NU 
      UPARAM(6) = SHAPE
      UPARAM(7) = HYS
      UPARAM(9) = IFLAG
      UPARAM(10) = IDAM 
      DO I=1,NFUNC
         UPARAM(I + 10)        =  RATE(I)
         UPARAM(I + 10 + NFUNC) = YFAC(I)
      END DO
      NUPARAM =13 + 2*NFUNC  
      UPARAM(NUPARAM) = ALPHA
!
      C1=E0/THREE/(ONE - TWO*NU)
      PARMAT(1) = C1
      PARMAT(2) = E0
      PARMAT(3) = NU
      PARMAT(4) = ISMOOTH
      PARMAT(5) = FCUT

      ISRATE = ISMOOTH 
      NUVAR = 10
        ! MTAG variable activation   
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
C
      WRITE(IOUT,1000)
      WRITE(IOUT,1001) TRIM(TITR),ID,90
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1100)RHO0                                                     
        WRITE(IOUT,1150)E0,NU
        WRITE(IOUT,1200)FCUT,ISMOOTH,SHAPE,HYS,ALPHA
        WRITE(IOUT,1300)(IFUNC(J),RATE(J),YFAC(J),J=1,NFUNC)
      ENDIF
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      IF (NU >= 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
c-----------------
      RETURN
!---
 1000 FORMAT(
     & 5X,40H  TABULATED NON LINEAR VISCO ELASTIC LAW,/,
     & 5X,40H  --------------------------------------,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1150 FORMAT(
     & 5X,'INITIAL YOUNG''S MODULUS. . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO. . . . . . . . . . . .=',1PG20.13/)
 1200 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR FILTERING STRAIN RATE  . . . .=',I10/,
     & 5X,'SHAPE FACTOR FOR UNLOADING  . . . . . .=',1PG20.13/,
     & 5X,'HYSTERETIC UNLOADING FACTOR . . . . . .=',1PG20.13/,
     & 5X,'EXPONANT FACTOR FOR UNLOADING . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'LOADING STRESS FUNCTION NUMBER  . . . .=',I10/,
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'PRESSURE DEPENDENT YIELD FUNCTION . . .=',I10/
     & 5X,'PRESSURE SCALE FACTOR . . . . . . . . .=',1PG20.13)
      END
