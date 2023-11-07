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
Chd|  HM_READ_FAIL_LADEVEZE         source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_LADEVEZE(FAIL ,
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ HASHIN FAILURE MODEL
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER            ,INTENT(IN) :: FAIL_ID       ! failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID        ! material law ID
      INTEGER            ,INTENT(IN) :: IRUPT         ! failure model type number
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)  ! submodel table
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ISHELL,ISOLID
      my_real :: K1,K2,K3,GAMA1,GAMA2,K,A,TMAX,FAC_L,FAC_T,FAC_M,FAC_C,Y0,YC,R
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
c
C--------------------------------------------------
C     check crypting
C--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
C--------------------------------------------------
C     Read DATA
C--------------------------------------------------
      CALL HM_GET_FLOATV ('K1'         ,K1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('K2'         ,K2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('K3'         ,K3     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Gamma_1'    ,GAMA1  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Gamma_2'    ,GAMA2  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_FLOATV ('Y0'          ,Y0     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Yc'          ,YC     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('k_LAD_DAMA'  ,K         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('a_DAMA'      ,A         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Tau_max'    ,TMAX       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_INTV   ('Ifail_sh'    ,ISHELL    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Ifail_so'    ,ISOLID    ,IS_AVAILABLE,LSUBMODEL)
c--------------------------------------------------
c     Default values
c--------------------------------------------------
      R = ZERO !! used for delamination (plyxfem formulation) 
      Y0  = SQRT(Y0)
      YC  = SQRT(YC)
      IF (K1 == ZERO) K1 = EP30
      IF (K2 == ZERO) K2 = EP30
      IF (K3 == ZERO) K3 = EP30
      IF (A == ZERO)A = EP30
      IF (Y0 == ZERO) Y0 = EP30
      IF (YC == ZERO .OR. YC==Y0)YC=TWO*Y0 
      IF (A == ZERO)A = EP30  
      IF(TMAX <= ZERO) TMAX = EP20    
      IF(ISHELL == 0) ISHELL = 1 
      IF(ISOLID == 0) ISOLID = 1              
c---------------------------
      FAIL%KEYWORD = 'LADEVEZE' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 13
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 12
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c  
      FAIL%UPARAM(1) = K1
      FAIL%UPARAM(2) = K2
      FAIL%UPARAM(3) = K3
      FAIL%UPARAM(4) = GAMA1
      FAIL%UPARAM(5) = GAMA2 
      FAIL%UPARAM(6) = Y0
      FAIL%UPARAM(7) = YC
      FAIL%UPARAM(8) = K
      FAIL%UPARAM(9) = A
      FAIL%UPARAM(10) = TMAX
      FAIL%UPARAM(11) = ISHELL
      FAIL%UPARAM(12) = ISOLID
      FAIL%UPARAM(13) = R   ! used inside delamination failure (plyxfem formulation) 
c---------------------------
      IF(IS_ENCRYPTED)THEN
      
      ELSE
       WRITE(IOUT, 1000)K1,K2,K3,GAMA1,GAMA2,Y0**2,YC**2,K,A,TMAX
C for solid      
       IF(ISOLID  == 1)THEN
        WRITE(IOUT, 1100)
       ELSEIF(ISOLID == 2)THEN
        WRITE(IOUT, 1200)
       ELSEIF(ISOLID == 3)THEN
        WRITE(IOUT, 1300)
       ENDIF       
      ENDIF
C-----------       
 1000 FORMAT(
     & 5X,40H    LADEVEZE DELAMINATION DAMAGE            /,
     & 5X,40H    ------------------------                  /,
     & 5X,'Stiffness in direction 13. .  . . . . . . . =',E12.4/
     & 5X,'Stiffness in direction 23. .  . . . . . . . =',E12.4/
     & 5X,'Stiffness in direction 33. .  . . . . . . . =',E12.4/
     & 5X,'Coupling parameter GAMA1 . . . . . .  . . . =',E12.4/
     & 5X,'Coupling parameter GAMA2. . . . . . . .  .  =',E12.4/
     & 5X,'Yield energy damage  . . . . . . . .  . . . =',E12.4/
     & 5X,'Critical energy damage  . . . . . . . .  .  =',E12.4/
     & 5X,'Parameter k . . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'Parameter a . . . . . . . . . . . . . . . . =',E12.4/
     & 5X,' Relaxation time . . . . . . . . . . . . .  =',E12.4/)
 1100 FORMAT(
     & 5X,' SOLID IS DELETED IF CRITERIA IS REACHED FOR'/
     & 5X,'          ONE INTEGRATION POINT ') 
 1200 FORMAT(
     & 5X,' SOLID IS DELETED IF CRITERIA IS REACHED FOR'/
     & 5X,'         ALL INTEGRATIONS POINTS ')      
 1300 FORMAT(
     & 5X,' OUT OF PLANE STRESS ARE SET TO ZERO IF CRITERIA' /
     & 5X,' IS REACHED FOR  EACH INTEGRATION POINT ') 
c-----------
      RETURN 
      END
