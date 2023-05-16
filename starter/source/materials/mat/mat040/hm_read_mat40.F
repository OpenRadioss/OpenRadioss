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
Chd|  HM_READ_MAT40                 source/materials/mat/mat040/hm_read_mat40.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT40(UPARAM   ,MAXUPARAM, NUPARAM   ,NUVAR  ,
     .                         MAXFUNC  ,NFUNC    , STIFINT   ,UNITAB ,MAT_ID       , 
     .                         MTAG     ,TITR     , LSUBMODEL ,PM     ,IMATVIS,
     .                         MATPARAM )
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
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
#include      "submod_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM), INTENT(INOUT) :: PM     
      my_real, DIMENSION(100), INTENT(INOUT) :: STIFINT
      my_real, DIMENSION(MAXUPARAM), INTENT(INOUT) :: UPARAM
      INTEGER, INTENT(INOUT) :: NFUNC, NUPARAM, NUVAR, IMATVIS
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      INTEGER, INTENT(IN) :: MAT_ID, MAXFUNC, MAXUPARAM
      CHARACTER*nchartitle, INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: AK, G0, G1, G2, G3, G4, G5, GT, BETA1,
     .     BETA2, BETA3, BETA4, BETA5, NU1, NU2,
     .     ASTAS, BSTAS, VMISK, FAC_L, FAC_T, FAC_M, FAC_C, 
     .     RHO0, RHOR
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      
!     Parameter initialization
      NUVAR = 40
      NFUNC = 0
      IMATVIS = 1

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

!     Check input encryption
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!     Initial and reference density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (RHOR == ZERO) THEN
         RHOR = RHO0
      ENDIF
      PM(1) = RHOR
      PM(89) = RHO0 
!     Line 1
      CALL HM_GET_FLOATV('MAT_BULK', AK, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GI', G0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Astass', ASTAS, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bstass', BSTAS, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Kvm', VMISK, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 2
      CALL HM_GET_FLOATV('MAT_G0', G1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G2', G2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G3', G3, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G4', G4, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G5', G5, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 3
      CALL HM_GET_FLOATV('MAT_DECAY', BETA1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DECAY2', BETA2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DECAY3', BETA3, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DECAY4', BETA4, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DECAY5', BETA5, IS_AVAILABLE, LSUBMODEL, UNITAB)

      IF (ASTAS <= EM20) ASTAS = INFINITY
      IF (BSTAS <= EM20) BSTAS = INFINITY
      IF (VMISK <= EM20) VMISK = INFINITY
      NU1 = (THREE * AK - TWO * G0) / (TWO * G0 + SIX * AK) 
      GT = G0 + G1 + G2 + G3 + G4 + G5 
      NU2 = (THREE * AK - TWO * GT) / (TWO * GT + SIX * AK) 
      IF (NU1 < ZERO .OR. NU1 >= HALF) THEN
      CALL ANCMSG(MSGID = 49,
     .     MSGTYPE = MSGERROR,
     .     ANMODE = ANINFO,
     .     R1 = NU1,
     .     I1 = MAT_ID,
     .     C1 = TITR)
      ENDIF
      IF (NU2 < ZERO .OR. NU2 >= HALF) THEN
         CALL ANCMSG(MSGID = 49,
     .        MSGTYPE = MSGERROR,
     .        ANMODE = ANINFO,
     .        R1 = NU2,
     .        I1 = MAT_ID,
     .        C1 = TITR)
      ENDIF
      NUPARAM = 15
      IF(NUPARAM > MAXUPARAM)THEN
         CALL ANCMSG(MSGID = 309,
     .        MSGTYPE = MSGERROR,
     .        ANMODE = ANINFO,
     .        I1 = MAT_ID,
     .        C1 = TITR,
     .        I2 = NUPARAM,
     .        I3 = MAXUPARAM)
      ELSE
         UPARAM(1) = AK 
         UPARAM(2) = G0
         UPARAM(3) = G1
         UPARAM(4) = G2
         UPARAM(5) = G3
         UPARAM(6) = G4
         UPARAM(7) = G5 
         UPARAM(8) =  MAX(BETA1, EM20)
         UPARAM(9) =  MAX(BETA2, EM20)
         UPARAM(10) = MAX(BETA3, EM20)
         UPARAM(11) = MAX(BETA4, EM20)
         UPARAM(12) = MAX(BETA5, EM20)
         UPARAM(13) = ASTAS
         UPARAM(14) = BSTAS
         UPARAM(15) = VMISK
      ENDIF

      STIFINT(1) = AK 

!     Formulation for solid elements time step computation.
      STIFINT(16) = 2
      STIFINT(17) = TWO * G0 / (AK + FOUR_OVER_3 * G0)
c-----------------
      IF (NU1 >= 0.49 .or. NU2 >= 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
c-----------------
      WRITE(IOUT, 800) TRIM(TITR), MAT_ID, 40
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT, 850) RHO0
         WRITE(IOUT,1100)AK,G0,G1,G2,G3,G4,G5,
     .        BETA1,BETA2,BETA3,BETA4,BETA5,
     .        ASTAS,BSTAS,VMISK
      ENDIF
C
  800 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
  850 FORMAT(
     & 5X,'INITIAL DENSITY  . . . . . . . . . . . . . .=',1PG20.13/)
 1000 FORMAT(
     & 5X,'  MAXWELL VISCO-ELASTIC LAW ',/,
     & 5X,'  ------------------------- ',//)
 1100 FORMAT(
     & 5X,'BULK MODULUS  . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LONG TIME SHEAR MODULUS . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS 1 . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS 2 . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS 3 . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS 4 . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS 5 . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DECAY CONSTANT 1  . . . . . . . . . . .=',1PG20.13/
     & 5X,'DECAY CONSTANT 2  . . . . . . . . . . .=',1PG20.13/
     & 5X,'DECAY CONSTANT 3  . . . . . . . . . . .=',1PG20.13/
     & 5X,'DECAY CONSTANT 4  . . . . . . . . . . .=',1PG20.13/
     & 5X,'DECAY CONSTANT 5  . . . . . . . . . . .=',1PG20.13/
     & 5X,'STASSI A COEFFICIENT  . . . . . . . . .=',1PG20.13/
     & 5X,'STASSI B COEFFICIENT  . . . . . . . . .=',1PG20.13/
     & 5X,'K VON MISES COEFFICIENT  . . . . . . . =',1PG20.13//)
C
      RETURN
      END
