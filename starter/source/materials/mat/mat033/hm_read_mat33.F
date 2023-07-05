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
Chd|  HM_READ_MAT33                 source/materials/mat/mat033/hm_read_mat33.F
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
      SUBROUTINE HM_READ_MAT33(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW33 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
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
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: ISRATE,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KEN,IFN1,IFN2,IFN3,IFN4,IFN5,IFN6,ICASE

      my_real 
     .  E,A,B,C,P0,PHI,GAMA0,FAC,SIGT_COFF

      my_real C1,C2,ET,VMU,VMU0

      my_real AXX,BXX,CXX,AYY,BYY,CYY,AZZ,BZZ,CZZ
      my_real AXY,BXY,CXY,AYZ,BYZ,CYZ,AZX,BZX,CZX
      my_real C1XX,C2XX,ETXX,VMUXX
      my_real C1YY,C2YY,ETYY,VMUYY
      my_real C1ZZ,C2ZZ,ETZZ,VMUZZ
      my_real C1XY,C2XY,GTXY,VMUXY
      my_real C1YZ,C2YZ,GTYZ,VMUYZ
      my_real C1ZX,C2ZX,GTZX,VMUZX
      my_real FAC_UNIT,RHO0,RHOR,FAC1
      
      LOGICAL :: IS_ENCRYPTED,IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NFUNC=0
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE=0
      IMATVIS=1

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_E'        ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Itype'        ,KEN      ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_INTV  ('FUN_A1'       ,IFN1     ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('IFscale'      ,FAC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('IFscale'  ,FAC_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      !hidden param/flag
      !CALL HM_GET_INTV  ('IFORM'       ,IFN2     ,IS_AVAILABLE, LSUBMODEL) 
      !CALL HM_GET_FLOATV('MAT_RHO'     ,FAC1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IFN2=0
      FAC1=ZERO

      CALL HM_GET_FLOATV('MAT_P0'       ,P0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PHI'      ,PHI      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMA0'    ,GAMA0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
  
      IF (FAC == ZERO) FAC = ONE * FAC_UNIT
      !hiddent no longer supported 
      !FAC1 = ONE / FAC_UNIT
      PARMAT(1)=E
      
      IF(RHOR==ZERO)RHOR=RHO0
      PM(01)=RHOR
      PM(89)=RHO0
      ICASE = ABS(KEN)+1
      SELECT CASE (ICASE) 
C-------------
        CASE(1,3)      
C-------------
c       KEN = 0 or KEN = 2
C-------------
C-------------
          NUPARAM=11
          CALL HM_GET_FLOATV('MAT_A0'             ,A            ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_A1'             ,B            ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_A2'             ,C            ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGT_CUTOFF'    ,SIGT_COFF    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!!
          IF(SIGT_COFF == ZERO) SIGT_COFF = EP20
          UPARAM(1)=KEN    
          UPARAM(2)=E
          UPARAM(3)=A
          UPARAM(4)=B
          UPARAM(5)=C
          UPARAM(6)=P0
          UPARAM(7)=PHI
          UPARAM(8)=GAMA0
          UPARAM(9)=FAC
          UPARAM(10)=FAC1
          UPARAM(11)=SIGT_COFF

          IFUNC(1)=IFN1
          IFUNC(2)=IFN2
          NFUNC=2
!      
           WRITE(IOUT,1001) TRIM(TITR),MAT_ID,33
           WRITE(IOUT,1000)

           IF(IS_ENCRYPTED)THEN
             WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
           ELSE
             WRITE(IOUT,1002) RHO0           
             WRITE(IOUT,1200) E,KEN,IFN1,FAC,IFN2,FAC1,
     &             A,B,C,SIGT_COFF,P0,PHI,GAMA0
          ENDIF 
C-------------
       CASE(2)
C-------------
c       KEN=1
C-------------
          NUPARAM=15
          CALL HM_GET_FLOATV('MAT_A0'    ,A    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_A1'    ,B    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_A2'    ,C    ,IS_AVAILABLE, LSUBMODEL, UNITAB)

          CALL HM_GET_FLOATV('MAT_E1'    ,C1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_E2'    ,C2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_ETAN'  ,ET    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_ETA1'  ,VMU    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_ETA2'  ,VMU0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)

          IF (VMU<=0..OR.VMU0<=0.) THEN
             CALL ANCMSG(MSGID=310,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=MAT_ID,
     .                   C1=TITR)
          ENDIF
          UPARAM(1)=KEN    
          UPARAM(2)=E
          UPARAM(3)=A
          UPARAM(4)=B
          UPARAM(5)=C
          UPARAM(6)=P0
          UPARAM(7)=PHI
          UPARAM(8)=GAMA0
          UPARAM(9)=C1
          UPARAM(10)=C2
          UPARAM(11)=ET
          UPARAM(12)=VMU
          UPARAM(13)=VMU0
          UPARAM(14)=FAC
          UPARAM(15)=FAC1

          IFUNC(1)=IFN1
          IFUNC(2)=IFN2
          NFUNC=2
C-------------
C
C     Formulation for solid elements time step computation.
         PARMAT(16) = 2
         PARMAT(17) =  ONE 
c-----------------
         CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
c-----------------
          WRITE(IOUT,1001) TRIM(TITR),MAT_ID,33        
          WRITE(IOUT,1000)                             
          IF(IS_ENCRYPTED)THEN                           
            WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA' 
          ELSE                                         
            WRITE(IOUT,1100) E,KEN,IFN1,FAC,IFN2,FAC1, 
     &                 C1,C2,ET,VMU,VMU0,          
     &                 A,B,C,P0,PHI,GAMA0          
          ENDIF                                       
      END SELECT 
      RETURN
C              12345678901234567890123456789012345678901
 1000 FORMAT
     & (5X,43H  LOW DENSITY CLOSED CELL POLYURETHANE FOAM,/,
     &  5X,43H  -----------------------------------------,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,   'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,   'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,   'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT
     & (5X,  'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'FLAG. . . . . . . . . . . . . . . . . .=',I10/
     & ,5X,  'FUNCTION NUMBER FOR THE YIELD CURVE . .=',I10//
     & ,5X,  'YIELD CURVE SCALE FACTOR. . . . . . . .=',1PG20.13/
     & ,5X,  'STRAIN RATE EFFECT CURVE. . . . . . .  =',I10/
     & ,5X,  'STRAIN RATE EFFECT SCALE FACTOR . . . .=',1PG20.13/
     & ,5X,  'USER CONSTANT FOR YOUNG MODULUS C1. . .=',1PG20.13/
     & ,5X,  'USER CONSTANT FOR YOUNG MODULUS C2. . .=',1PG20.13/
     & ,5X,  'TANGENT MODULUS . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'VISCOUS COEFFICIENT (PURE COMPRESSION).=',1PG20.13/
     & ,5X,  'VISCOUS COEFFICIENT (PURE SHEAR). . . .=',1PG20.13//
     & ,5X,  'USER CONSTANT FOR YIELD STRESS A. . . .=',1PG20.13/
     & ,5X,  'USER CONSTANT FOR YIELD STRESS B. . . .=',1PG20.13/
     & ,5X,  'USER CONSTANT FOR YIELD STRESS C. . . .=',1PG20.13//
     & ,5X,  'INITIAL FOAM PRESSURE . . . . . . . . .=',1PG20.13/
     & ,5X,  'RATIO OF FOAM TO POLYMER DENSITY. . . .=',1PG20.13/
     & ,5X,  'INITIAL VOLUMETRIC STRAIN. . . .. . . .=',1PG20.13/)
 1200 FORMAT
     & (5X,  'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'FLAG. . . . . . . . . . . . . . . . . .=',I10/
     & ,5X,  'FUNCTION NUMBER FOR THE YIELD CURVE . .=',I10//
     & ,5X,  'YIELD CURVE SCALE FACTOR. . . . . . . .=',1PG20.13/
     & ,5X,  'STRAIN RATE EFFECT CURVE. . . . . . .  =',I10/
     & ,5X,  'STRAIN RATE EFFECT SCALE FACTOR . . . .=',1PG20.13/
     & ,5X,  'USER CONSTANT FOR YIELD STRESS A. . . .=',1PG20.13/
     & ,5X,  'USER CONSTANT FOR YIELD STRESS B. . . .=',1PG20.13/
     & ,5X,  'USER CONSTANT FOR YIELD STRESS C. . . .=',1PG20.13/
     & ,5X,  'TENSION CUT OFF STRESS . . . . .. . . .=',1PG20.13//
     & ,5X,  'INITIAL FOAM PRESSURE . . . . . . . . .=',1PG20.13/
     & ,5X,  'RATIO OF FOAM TO POLYMER DENSITY. . . .=',1PG20.13/
     & ,5X,  'INITIAL VOLUMETRIC STRAIN. . . .. . . .=',1PG20.13/)
      END
