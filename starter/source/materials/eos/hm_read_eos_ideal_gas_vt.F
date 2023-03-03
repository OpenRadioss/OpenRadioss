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
Chd|  HM_READ_EOS_IDEAL_GAS_VT      source/materials/eos/hm_read_eos_ideal_gas_vt.F
Chd|-- called by -----------
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EOS_IDEAL_GAS_VT(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C reading parameters for
C   IDEAL-GAS EQUATION OF STATE WITH CP(T) FUNCTION
C   Cp(T) = A0 + A1.T + A2.T^2 + A3.T^3 + A4.T^4 + A5.T^5
C
C   clarification :
C   Cp : J/K/kg
C   Cv : J/K/kg
C   R  : J.K/mol
C   r  : J/kg/K
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C  RHOI = PM(89)   -> provided by /MAT
C  RHOR = PM(01)   -> provided by /MAT (can be erased by EOS if present : obsolete)
C  => MU0 = RHO/RHOR-1.
C  PM(31) = P(MU0,T0) -> will be used to initialize diagonal of stress tensor SIG(1:3,*)
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: GAMMA, P0,T0, E0, PSH, RHO0,FAC_L,FAC_T,FAC_M,FAC_C,Cv,MU0,PP,RHOI,RHOR,G0,SSP0,DPDMU,DF
      my_real :: R_GAS,r_gas_, M_GAS, A0, A1, A2, A3, A4, A5, Cp
      INTEGER :: USER_CURVEID, CURVEID, JJ
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      GAMMA=ZERO
            
C      CALL HM_GET_FLOATV('MAT_R', R_GAS, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_R', r_GAS_, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_P0', P0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_PSH', PSH, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('T_Initial', T0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C0', A0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C1', A1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C2', A2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C3', A3, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C4', A4, IS_AVAILABLE,LSUBMODEL,UNITAB)
                        
      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF
      
      IF(P0*T0 /= ZERO)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/IDEAL-GAS-VT',
     .               C2='P0 AND T0 CANNOT BE BOTH DEFINED')
      ENDIF
      
      IF(r_gas_ <= ZERO)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/IDEAL-GAS-VT',
     .               C2='GAS CONSTANT r MUST BE STRICTLY POSITIVE')
       P0 = ZERO
      ELSE
        ! P0= rho0.r.T0 
        ! R = r/M
        IF(T0 > ZERO) THEN
          P0 = r_gas_ * RHO0 * T0
        ELSEIF(P0 /= ZERO .AND. RHO0 /= ZERO)THEN
          T0 = P0/r_gas_/RHO0
        ELSE
          T0=0
          P0=ZERO
        ENDIF
        
      ENDIF
      
      IF(P0 <= ZERO)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/IDEAL-GAS-VT',
     .               C2='INITIAL PRESSURE MUST BE POSITIVE')
      ENDIF   
      
      IF(T0 < ZERO)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/IDEAL-GAS-VT',
     .               C2='TEMPERATURE MUST BE POSITIVE (UNIT:KELVIN)')
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
            
      CP = A0 + A1*T0 + A2*T0*T0 + A3*T0**3 + A4*T0**4
      
      IF(CP < r_GAS_)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/IDEAL-GAS-VT',
     .               C2='Cp(0) < r IS NOT EXPECTED. CHECK INPUT FUNCTION')
      ENDIF               
      
      E0 = RHO0 * (A0 * T0 + HALF * A1 * T0**2 + THIRD * A2 * T0**3 + FOURTH * A3 * T0**4 + 
     .     ONE_FIFTH * A4 * T0**5 - r_GAS_ * T0)
     
      IF(T0 == ZERO)T0=THREE100
     
      PM(106) = r_gas_ ! r=R_GAS/M
      PM(88) = PSH
      PM(23) = E0
      PM(31) = P0-PSH   !used to init stress tensor
      PM(32) = A0
      PM(33) = A1
      PM(34) = A2
      PM(35) = A3
      PM(36) = A4
      PM(79) = T0
      IF(PM(79)==ZERO)PM(79)=THREE100      
      
      PM(104)=P0-PSH
            
      !SSP0
      CV = CP - r_gas_ 
      IF(CV /= ZERO)THEN
        GAMMA = CP/CV
      ELSE
        GAMMA = ZERO !  this error is managed above (CP < r_GAS_)
      ENDIF
      SSP0 = SQRT(GAMMA*r_gas_*T0)
      G0 = PM(22) ! 0.0 with physical input.However mathematical model allows any positive value.
      RHOI = PM(89)            
      DPDMU =  RHO0*GAMMA*r_gas_*T0

      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0      

      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)r_GAS_,T0,P0,PSH,RHO0,Cp,A0,A1,A2,A3,A4
      ENDIF

      RETURN
 1000 FORMAT(
     & 5X,'  IDEAL GAS EOS (VOLUME-TEMPERATURE) : P=rho.r.T    ',/,
     & 5X,'  ----------------------------------------------    ',/)
 1500 FORMAT(
     & 5X,'GAS CONSTANT (r). . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PRESSURE SHIFT. . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL MASSIC HEAT CAPACTITY . . . . . .=',1PG20.13/,
     & 5X,'A0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,     
     & 5X,'A1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A3. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A4. . . . . . . . . . . . . . . . . . . .=',1PG20.13/)

      RETURN

      END SUBROUTINE HM_READ_EOS_IDEAL_GAS_VT
