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
!||    hm_read_eos_ideal_gas    ../starter/source/materials/eos/hm_read_eos_ideal_gas.F
!||--- called by ------------------------------------------------------
!||    hm_read_eos              ../starter/source/materials/eos/hm_read_eos.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_EOS_IDEAL_GAS(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C reading parameters for
C   IDEAL-GAS EQUATION OF STATE
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C  RHOI = PM(89)   -> provided by /MAT
C  RHOR = PM(01)   -> provided by /MAT (can be erased by EOS if present : obsolete)
C  => MU0 = RHO/RHOR-1.
C  PM(31) = P(MU0,E0) -> will be used to initialize diagonal of stress tensor SIG(1:3,*)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IIN,IOUT,IUNIT
      my_real PM(NPROPM)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      INTEGER,INTENT(IN) :: IMIDEOS
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real  GAMMA, P0,T0, E0, PSH, RHO0,FAC_L,FAC_T,FAC_M,FAC_C,Cv,MU0,PP,RHOI,RHOR,G0,SSP0,DPDMU,DF
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE, IS_AVAILABLE_RHO0
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_AVAILABLE_RHO0 = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('Gamma', GAMMA, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_P0', P0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_PSH', PSH, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('T_Initial', T0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0, IS_AVAILABLE_RHO0,LSUBMODEL,UNITAB)

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF
      
      IF(RHOI == ZERO)THEN
        MU0 = ZERO ! error 683 already displayed
      ELSE
        IF(RHOR /= ZERO)THEN
          MU0 = RHOI/RHOR-ONE
        ELSE
          MU0 = ZERO ! error 683 already displayed
        ENDIF
      ENDIF
      
      IF(RHOI /= ZERO)THEN
        DF = RHOR/RHOI
      ELSE
        DF = ZERO
      ENDIF
      
      IF(GAMMA > ONE) THEN
        E0 = P0/(GAMMA-ONE)/(ONE+MU0)
      ELSE
        E0 = ZERO !manage this error below
      ENDIF

      !ERREURS :
      ! P0 <=0
      ! GAMMA <=ONE
      IF(P0 <= ZERO)THEN
         CALL ANCMSG(MSGID=67,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/IDEAL-GAS',
     .               C2='INITIAL PRESSURE MUST BE POSITIVE')
      ENDIF
      
      IF(GAMMA <= ONE)THEN
         CALL ANCMSG(MSGID=67,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/IDEAL-GAS',
     .               C2='SPECIFIC HEAT RATIO (GAMMA) MUST BE GREATER THAN 1.0')
      ENDIF

      IF(T0 == ZERO)T0=THREE100
      Cv = ZERO
      IF(RHO0*T0 /= ZERO) THEN
        ! this situation is not expected unless in case of input inconsistency (eos_uid does not match mat_uid)
        Cv = E0/RHO0/T0
      ENDIF
      
      PM(32) = GAMMA
      PM(88) = PSH
      PM(23) = E0
      PM(35) = T0
      PM(36) = Cv
      PM(31) = P0-PSH
      PM(79) = T0
      PM(104)=P0-PSH

      MAT_PARAM%EOS%Cv = Cv
      MAT_PARAM%EOS%Cp = GAMMA * Cv
      
      !SSP0
      SSP0 = ZERO 
      G0 = PM(22)
      RHOI = PM(89)
      DPDMU =  (GAMMA-ONE)*E0+(ONE+MU0)*(GAMMA-ONE)*DF*DF*(PM(31)+PSH)

      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0      

      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)GAMMA,P0,PSH,T0,E0,Cv
      ENDIF
      IF(IS_AVAILABLE_RHO0)WRITE(IOUT,1501)PM(1)

      RETURN
 1000 FORMAT(
     & 5X,'  IDEAL GAS EOS     ',/,
     & 5X,'  -------------     ',/)
 1500 FORMAT(
     & 5X,'GAMMA . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'P0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PSH . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'T0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'E0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'COMPUTED HEAT CAPACITY (Cv) . . . . . . .=',1PG20.13)
 1501 FORMAT(     
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13) 
      
      RETURN

      END SUBROUTINE HM_READ_EOS_IDEAL_GAS
