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
Chd|  HM_READ_MAT92                 source/materials/mat/mat092/hm_read_mat92.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT92(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW92 WITH HM READER ( TO BE COMPLETED )
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
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ID,ITEST,IDAM
      my_real
     .  E,NU,G,MU,LAM,D,RBULK,SCALEFAC,C(5),
     .  A1,A2,A3,A4,NU0 ,FAC_UNIT  
      CHARACTER  STRING*ncharfield
      my_real :: RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IDAM = 0
      ISRATE = 0
      IMATVIS = 1
      NUVAR = 0

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_MUE1'     ,MU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D'        ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Lamda'    ,LAM      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_INTV  ('Itype'        ,ITEST    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_FCT_IDI'  ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_NU'       ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FScale'   ,SCALEFAC ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF(SCALEFAC == ZERO)THEN 
        CALL HM_GET_FLOATV_DIM('MAT_FScale' ,FAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SCALEFAC = ONE * FAC_UNIT
      ENDIF       

      !========== DEFAULT VALUES=============!              
      RHOR=ZERO
      !IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0

      !C By default we consider the uniaxial test 
      IF(ITEST == 0) ITEST = 1
      !C Uniaxial - itest = 1       
      !C equibiaxial - Itest = 2
      !c Planar - Itest = 3
      !C       
      !the proposed one      READ(LINE,FMT=FMT_A_I_F)STRING,IFUNC(1),NU     
      !CSTRING = UNIAXIAL
      !CSTRING = EQUIBIAXIAL 
      !CSTRING = PLANAR 
      !C By default we consider the unniaxial test  
      !!the proposed one       ITEST = 1  
      !!the proposed one       IF(STRING(1:8) == ' ' ) THEN
      !!the proposed one          ITEST = 1
      !!the proposed one       ELSEIF(STRING(1:10) == 'EQUIBIAXIA') THEN
      !!the proposed one          ITEST = 2
      !!the proposed one       ELSEIF(STRING(1:6) == 'PLANAR') THEN
      !!the proposed one         ITEST = 3
      !!the proposed one       ENDIF
      C(1) = HALF
      C(2) = ONE/TWENTY
      C(3) = ELEVEN/1050.D00
      C(4) = 19.D00/7000.D00
      C(5) = 519.D00/673750.D00
C
      IF(LAM == ZERO) LAM = SEVEN  
      A1 = ONE/LAM**2
      A2 = A1**2
      A3 = A1*A2
      A4 = A2**2
      G= MU*(ONE +  THREE*A1/FIVE + EIGHTY19*A2/175.D00 
     .         + 513.D00*A3/875.D00 + 42039.D00*A4/67375.D00) 
        
      NFUNC= 0
      RBULK = ZERO
      IF(IFUNC(1) /= 0) THEN
       NFUNC = 1
       IF (NU == ZERO) NU= 0.495
      ELSEIF(D == ZERO) THEN
        NU = 0.495
        RBULK = TWO_THIRD*(ONE + NU)*G/(ONE - TWO*NU)
        D  = TWO/RBULK 
      ELSE
        RBULK =  TWO/D 
      ENDIF
!
! computing E
!
      E = NINE*RBULK*G/MAX(EM20,(THREE*RBULK + G))  
      IF(IFUNC(1) == 0 ) NU = (THREE*RBULK -TWO*G)/(THREE*RBULK + G)/TWO
C     
      UPARAM(1) = MU
      UPARAM(2) = D
      UPARAM(3) = LAM
      UPARAM(4) = G
      UPARAM(5) = RBULK
      UPARAM(6) = C(1)
      UPARAM(7) = C(2)
      UPARAM(8) = C(3)
      UPARAM(9) = C(4)
      UPARAM(10) = C(5) 
      UPARAM(11) = NU  
      UPARAM(12) = ITEST
      UPARAM(14) = SCALEFAC
     
      NUPARAM = 14
      IF(NUPARAM > MAXUPARAM)THEN
        WRITE(IOUT,*)' ** ERROR : NUPARAM GT MAXUPARAM'
        WRITE(IOUT,*)'      NUPARAM =',NUPARAM,
     .                ' MAXUPARAM =',MAXUPARAM
      ENDIF
!
      PARMAT(1) = RBULK
      PARMAT(2) = E
      PARMAT(3) = NU
      
       MTAG%L_EPSA  = 1 ! hyperelastic energy needed for mullins
       IMATVIS = 1                 
       PM(100) = RBULK
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
c-----------------
      !========== OUTPUT =============!
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,92
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT,1002) RHO0
        IF(IFUNC(1) > 0) THEN
           IF(ITEST == 1) THEN 
              WRITE(IOUT,1200)IFUNC(1),SCALEFAC, NU
           ELSEIF(ITEST == 2) THEN 
              WRITE(IOUT,1300)IFUNC(1),SCALEFAC, NU
           ELSEIF(ITEST == 3) THEN 
              WRITE(IOUT,1400)IFUNC(1),SCALEFAC, NU
           ENDIF   
        ELSE
          WRITE(IOUT,1100)MU,D,LAM, NU
        ENDIF  
      ENDIF

C-----------------------------------------------
 1000 FORMAT(
     & 5X,'  ARRUDA-BOYCE  LAW ',/,
     & 5X,'  ---------------- ',/)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'MU . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'D. . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LAM. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . .=',E12.4//) 
 1200 FORMAT(
     & 5X,'UNIAXIAL DATA TEST CURVE  .  . . . . . .=',I10/
     & 5X,'SCALE FACTOR FOR STRESS IN FUNCTION. . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . .=',E12.4///) 
 1300 FORMAT(
     & 5X,'EQUIBIAXIAL DATA TEST CURVE. . . . . . .=',I10/
     & 5X,'SCALE FACTOR FOR STRESS IN FUNCTION. . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . .=',E12.4///)
 1400 FORMAT(
     & 5X,'PLANAR DATA TEST CURVE  . .  . . . . . .=',I10/
     & 5X,'SCALE FACTOR FOR STRESS IN FUNCTION. . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . .=',E12.4///)
C-----------------------------------------------


      RETURN
      END
