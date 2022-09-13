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
Chd|  HM_READ_MAT94                 source/materials/mat/mat094/hm_read_mat94.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT94(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
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
      INTEGER ID,IDAM,ILAW
      my_real
     .  E,NU,G,MU,RBULK,C10,C20,C30,D1,D2,D3  
      CHARACTER  STRING*ncharfield
      my_real :: RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IDAM = 0
      ISRATE = 0
      IMATVIS = 1
      NUVAR = 0
      ILAW = 94

      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !! CALL HM_GET_FLOATV('MAT_RHO'      ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
       !line-1 Is not active (Blank) 
      !CALL HM_GET_INTV  ('Itype'        ,ITEST    ,IS_AVAILABLE, LSUBMODEL)
      !CALL HM_GET_INTV  ('MAT_FCT_IDI'  ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL)
      !CALL HM_GET_FLOATV('MAT_NU'       ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('LAW94_C01'     ,C10      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW94_C02'     ,C20      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW94_C03'     ,C30      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('LAW94_D1'     ,D1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW94_D2'     ,D2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW94_D3'     ,D3      ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      !========== DEFAULT VALUES=============!              
      RHOR=ZERO
      !!IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
      
C 
C Uniaxial - itest = 1       
C equibiaxial - Itest = 2
c Planar - Itest = 3
C       
!the proposed one      READ(LINE,FMT=FMT_A_I_F)STRING,IFUNC(1),NU     
CSTRING = UNIAXIAL
CSTRING = EQUIBIAXIAL 
CSTRING = PLANAR 

C     
      G = ZERO
      RBULK = ZERO
      E  = ZERO
C I                                    
      IF(D2 /=  ZERO ) D2 = ONE/D2                          
      IF(D3 /= ZERO )  D3 = ONE/D3                          
C                                 
      G = TWO*C10                                         
      IF(D1 == ZERO) THEN                                  
        D2 = ZERO                                          
        D3 = ZERO                                          
        NU = 0.495                                          
        RBULK = TWO_THIRD*G*(ONE + NU)/(ONE-TWO*NU)            
        D1 = RBULK / TWO ! 1/D1                           
        E = TWO*G*(ONE + NU)                               
      ELSE                                                  
        D1 = ONE/D1                                  
        RBULK= TWO*D1                                     
        NU = (THREE*RBULK -TWO*G)/(THREE*RBULK + G)/TWO  
        E = NINE*RBULK*G/(THREE*RBULK + G)                 
      ENDIF                                               
C               
      UPARAM(1) = G
      UPARAM(2) = RBULK
      UPARAM(3) = NU
      UPARAM(4) = C10
      UPARAM(5) = C20
      UPARAM(6) = C30
      UPARAM(7) = D1
      UPARAM(8) = D2
      UPARAM(9) = D3
!! not used     UPARAM(10) = ITEST
C      
      NUPARAM = 9
      IF(NUPARAM>MAXUPARAM)THEN
        WRITE(IOUT,*)' ** ERROR : NUPARAM GT MAXUPARAM'
        WRITE(IOUT,*)'      NUPARAM =',NUPARAM,
     .                ' MAXUPARAM =',MAXUPARAM
      ENDIF    
C     
      PARMAT(1) = RBULK
      PARMAT(2) = E
      PARMAT(3) = NU
C
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(RBULK + FOUR_OVER_3*G) 
C  
       IMATVIS = 1                 
       PM(100) = RBULK
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
c-----------------
      !========== OUTPUT =============!
      
      WRITE(IOUT,1010) TRIM(TITR),MAT_ID,94
      WRITE(IOUT,1000)
      IF(IS_CRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT,1020)RHO0
! not used
!!        IF (IFUNC(1) > 0) THEN
!!           IF(ITEST == 1) THEN 
!!              WRITE(IOUT,1300)IFUNC(1), NU
!!           ELSEIF(ITEST == 2) THEN 
!!              WRITE(IOUT,1310)IFUNC(1), NU
!!           ELSEIF(ITEST == 3) THEN 
!!              WRITE(IOUT,1320)IFUNC(1), NU
!!           ENDIF   
!!        ELSE        
          WRITE(IOUT,1100)C10,C20,C30,D1,D2,D3
          WRITE(IOUT,1200)G,RBULK, NU
!!        ENDIF  
      ENDIF
C
 1000 FORMAT(
     & 5X,'  YEOH LAW ',/,
     & 5X,'  ---------- ',//) 
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . =',I10/)
 1020 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT(
     & 5X,'C10 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C20 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C30 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D1  . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D2  . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D3  . . . . . . . . . . . . . . . . . .=',1PG20.13)
 1200 FORMAT(
     & 5X,'INITIAL SHEAR MODULUS . . . . . . . . . .=',1PG20.13/
     & 5X,'BULK MODULUS. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . . .=',1PG20.13/)      
 1300 FORMAT(
     & 5X,'UNIAXIAL DATA TEST CURVE. . . . . . . . .=',I10/
     & 5X,'POISSON RATIO . . . . . . . . . . . . . .=',1PG20.13///) 
 1310 FORMAT(
     & 5X,'EQUIBIAXIAL DATA TEST CURVE . . . . . . .=',I10/
     & 5X,'POISSON RATIO . . . . . . . . . . . . . .=',1PG20.13///)
 1320 FORMAT(
     & 5X,'PLANAR DATA TEST CURVE. . . . . . . . . .=',I10/
     & 5X,'POISSON RATIO . . . . . . . . . . . . . .=',1PG20.13///)
C
      RETURN
      END
C-----------------------------------------------
