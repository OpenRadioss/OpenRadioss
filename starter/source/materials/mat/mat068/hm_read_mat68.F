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
Chd|  HM_READ_MAT68                 source/materials/mat/mat068/hm_read_mat68.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT68(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW68 WITH HM READER
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
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
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     . E11,E22,E33,G12,G23,G31,EMX11,EMX22,EMX33,EMX12,EMX23,EMX31,
     . EMF11,EMF22,EMF33,EMF12,EMF23,EMF31,
     . FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC7,FAC8,FAC9,
     . FAC10,FAC11,FAC12,FAC13,FAC14,FAC15,FAC16,FAC17,FAC18
      my_real FAC_C,FAC_L,FAC_T,FAC_M,DMIN,DMAX
      INTEGER I11,I22,I33,I12,I23,I31,I21,I32,I13,IF1,IF2
      INTEGER J11,J22,J33,J12,J23,J31,J21,J32,J13
      my_real :: RHO0, RHOR, FAC_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IFRWV = MAX(1,IFRWV)

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 0
      IMATVIS = 0
      MTAG%G_GAMA = 6
      MTAG%L_SIGL = 6
      MTAG%L_SIGD = 6
      MTAG%L_EPSA = 1 ! hyperelastic energy needed for mullins              
      NUPARAM=38
      NFUNC=18
      NUVAR=18
      
      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_EA'       ,E11      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EB'       ,E22      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EC'       ,E33      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT_GAB'      ,G12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC'      ,G23      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA'      ,G31      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_INTV  ('FUN_A1'       ,I11      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B1'       ,I22      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A2'       ,I33      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Gflag'        ,IF1      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('FScale11'     ,FAC1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22'     ,FAC2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33'     ,FAC3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5
      CALL HM_GET_FLOATV('MAT_EPSR1'    ,EMX11      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR2'    ,EMX22      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR3'    ,EMX33      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-6
      CALL HM_GET_INTV  ('FUN_A3'       ,I12      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B3'       ,I23      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A4'       ,I31      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Vflag'        ,IF2      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('FScale12'     ,FAC4     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale23'     ,FAC5     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale31'     ,FAC6     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-7
      CALL HM_GET_FLOATV('MAT_EPSR4'    ,EMX12    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR5'    ,EMX23    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSR6'    ,EMX31    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-8
      CALL HM_GET_INTV  ('FUN_B4'       ,I21      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B5'       ,I32      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B6'       ,I13      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('FScale21'     ,FAC7     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale32'     ,FAC8     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale13'     ,FAC9     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-9
      CALL HM_GET_INTV  ('MAT_YFUN11_2' ,J11      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN22_2' ,J22      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN33_2' ,J33      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('FScale11_2'   ,FAC10    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22_2'   ,FAC11    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33_2'   ,FAC12    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-10
      CALL HM_GET_FLOATV('MAT_EPS11_2'  ,EMF11    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS22_2'  ,EMF22    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS33_2'  ,EMF33    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-11
      CALL HM_GET_INTV  ('MAT_YFUN12_2' ,J12      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN23_2' ,J23      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN31_2' ,J31      ,IS_AVAILABLE, LSUBMODEL)            
      CALL HM_GET_FLOATV('FScale12_2'   ,FAC13    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('FScale23_2'   ,FAC14    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('FScale31_2'   ,FAC15    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      !line-12
      CALL HM_GET_FLOATV('MAT_EPS12_2'  ,EMF12    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('MAT_EPS23_2'  ,EMF23    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('MAT_EPS31_2'  ,EMF31    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      !line-13
      CALL HM_GET_INTV  ('MAT_YFUN21_2' ,J21      ,IS_AVAILABLE, LSUBMODEL)            
      CALL HM_GET_INTV  ('MAT_YFUN32_2' ,J32      ,IS_AVAILABLE, LSUBMODEL)            
      CALL HM_GET_INTV  ('MAT_YFUN13_2' ,J13      ,IS_AVAILABLE, LSUBMODEL)            
      CALL HM_GET_FLOATV('FScale21_2'   ,FAC16    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('FScale32_2'   ,FAC17    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('FScale13_2'   ,FAC18    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      !units
      CALL HM_GET_FLOATV_DIM('FScale11' ,FAC_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)

      !---DEFAULT VALUES
      IF(RHOR == ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0  
      IF (FAC1 == ZERO) FAC1  = ONE*FAC_UNIT
      IF (FAC2 == ZERO) FAC2  = ONE*FAC_UNIT
      IF (FAC3 == ZERO) FAC3  = ONE*FAC_UNIT
      IF (FAC4 == ZERO) FAC4  = ONE*FAC_UNIT
      IF (FAC5 == ZERO) FAC5  = ONE*FAC_UNIT
      IF (FAC6 == ZERO) FAC6  = ONE*FAC_UNIT
      IF (FAC7 == ZERO) FAC7  = ONE*FAC_UNIT
      IF (FAC8 == ZERO) FAC8  = ONE*FAC_UNIT
      IF (FAC9 == ZERO) FAC9  = ONE*FAC_UNIT
      IF (FAC10== ZERO) FAC10 = ONE*FAC_UNIT
      IF (FAC11== ZERO) FAC11 = ONE*FAC_UNIT
      IF (FAC12== ZERO) FAC12 = ONE*FAC_UNIT
      IF (FAC13== ZERO) FAC13 = ONE*FAC_UNIT
      IF (FAC14== ZERO) FAC14 = ONE*FAC_UNIT
      IF (FAC15== ZERO) FAC15 = ONE*FAC_UNIT
      IF (FAC16== ZERO) FAC16 = ONE*FAC_UNIT
      IF (FAC17== ZERO) FAC17 = ONE*FAC_UNIT
      IF (FAC18== ZERO) FAC18 = ONE*FAC_UNIT

      !---STORAGE
      UPARAM(1)=E11
      UPARAM(2)=E22
      UPARAM(3)=E33
      UPARAM(4)=G12
      UPARAM(5)=G23
      UPARAM(6)=G31
      UPARAM(7)=IF1
      UPARAM(8)=IF2
      UPARAM(9) =EMX11
      UPARAM(10)=EMX22
      UPARAM(11)=EMX33
      UPARAM(12)=EMX12
      UPARAM(13)=EMX23
      UPARAM(14)=EMX31
      IF(UPARAM(9) == ZERO)UPARAM(9) =INFINITY
      IF(UPARAM(10) == ZERO)UPARAM(10)=INFINITY
      IF(UPARAM(11) == ZERO)UPARAM(11)=INFINITY
      IF(UPARAM(12) == ZERO)UPARAM(12)=INFINITY
      IF(UPARAM(13) == ZERO)UPARAM(13)=INFINITY
      IF(UPARAM(14) == ZERO)UPARAM(14)=INFINITY
      UPARAM(15)=EMF11
      UPARAM(16)=EMF22
      UPARAM(17)=EMF33
      UPARAM(18)=EMF12
      UPARAM(19)=EMF23
      UPARAM(20)=EMF31
      IF(UPARAM(15) == ZERO)UPARAM(15)=INFINITY
      IF(UPARAM(16) == ZERO)UPARAM(16)=INFINITY
      IF(UPARAM(17) == ZERO)UPARAM(17)=INFINITY
      IF(UPARAM(18) == ZERO)UPARAM(18)=INFINITY
      IF(UPARAM(19) == ZERO)UPARAM(19)=INFINITY
      IF(UPARAM(20) == ZERO)UPARAM(20)=INFINITY
      UPARAM(21)=FAC1 
      UPARAM(22)=FAC2 
      UPARAM(23)=FAC3 
      UPARAM(24)=FAC4 
      UPARAM(25)=FAC5 
      UPARAM(26)=FAC6 
      UPARAM(27)=FAC7 
      UPARAM(28)=FAC8 
      UPARAM(29)=FAC9  
      UPARAM(30)=FAC10
      UPARAM(31)=FAC11
      UPARAM(32)=FAC12
      UPARAM(33)=FAC13
      UPARAM(34)=FAC14
      UPARAM(35)=FAC15
      UPARAM(36)=FAC16
      UPARAM(37)=FAC17
      UPARAM(38)=FAC18
      
      PARMAT(1)= MAX(E11,E22,E33,G12,G23,G31)

      IFUNC(1)=I11
      IFUNC(2)=I22
      IFUNC(3)=I33
      IFUNC(4)=I12
      IFUNC(5)=I23
      IFUNC(6)=I31
      IFUNC(7)=I21
      IFUNC(8)=I32
      IFUNC(9)=I13
      IF(J11 == 0)J11=I11
      IF(J22 == 0)J22=I22
      IF(J33 == 0)J33=I33
      IF(J12 == 0)J12=I12
      IF(J23 == 0)J23=I23
      IF(J31 == 0)J31=I31
      IF(J21 == 0)J21=I21
      IF(J32 == 0)J32=I32
      IF(J13 == 0)J13=I13
      IFUNC(10)=J11
      IFUNC(11)=J22
      IFUNC(12)=J33
      IFUNC(13)=J12
      IFUNC(14)=J23
      IFUNC(15)=J31
      IFUNC(16)=J21
      IFUNC(17)=J32
      IFUNC(18)=J13
 
      !Formulation for solid elements time step computation.
      DMIN  = MIN(E11*E22, E22*E33,E11*E33)
      DMAX = MAX(E11,E22,E33)
      PARMAT(16) = 1
      PARMAT(17) = DMIN/DMAX/DMAX
c
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
c
      !---LISTING OUTPUT
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,68
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100)E11,E22,E33,G12,G23,G31,
     .      I11,I22,I33,I12,I23,I31,I21,I32,I13,
     .      FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC7,FAC8,FAC9,
     .      J11,J22,J33,J12,J23,J31,J21,J32,J13,
     .      FAC10,FAC11,FAC12,FAC13,FAC14,FAC15,FAC16,FAC17,FAC18
        IF(IF1+IF2/=0)WRITE(IOUT,1200)IF1,IF2
        IF(EMX11+EMX22+EMX33+EMX12+EMX23+EMX31/=0)WRITE(IOUT,1300)
     .   EMX11,EMX22,EMX33,EMX12,EMX23,EMX31
      ENDIF

      RETURN
      
      !---OUTPUT FORMAT    
 1000 FORMAT(
     & 5X,40H  HONEYCOMS LAW - COSSERA FORMULATION   ,/,
     & 5X,40H  -----------------------------------   ,//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'E11 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'E22 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'E33 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G12 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G23 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G31 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL STRESS CURVES . . . . . . . . .=',/
     & 5X,'YIELD STRESS 11 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 22 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 33 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 12 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 23 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 31 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 21 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 32 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 13 FUNCTION NUMBER . . . .=',I10/
     & 5X,'FUNCTION 11 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 22 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 33 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 12 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 23 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 31 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 21 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 32 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 13 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'RESIDUAL STRESS CURVES . . . . . . . . .=',/
     & 5X,'YIELD STRESS 11 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 22 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 33 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 12 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 23 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 31 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 21 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 32 FUNCTION NUMBER . . . .=',I10/
     & 5X,'YIELD STRESS 13 FUNCTION NUMBER . . . .=',I10/
     & 5X,'FUNCTION 11 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 22 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 33 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 12 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 23 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 31 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 21 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 32 SCALE FACTOR . . . . . . . =',1PG20.13/
     & 5X,'FUNCTION 13 SCALE FACTOR . . . . . . . =',1PG20.13//)
 1200 FORMAT(
     & 5X,'YIELD FUNCTION 11,22,33 FLAG .  . . . .=',I10/
     & 5X,'YIELD FUNCTION 12,23,31 FLAG .  . . . .=',I10//)
 1300 FORMAT(
     & 5X,'TENSION FAILURE STRAIN 11 . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 22 . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 33 . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 12 . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 23 . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 31 . . . . . . . .=',1PG20.13//)
      END
