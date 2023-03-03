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
Chd|  HM_READ_EOS_LSZK              source/materials/eos/hm_read_eos_lszk.F
Chd|-- called by -----------
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EOS_LSZK(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
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
C   MURNAGHAN EQUATION OF STATE
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real  GAMMA, P0,T0, E0, PSH, RHO0,FAC_L,FAC_T,FAC_M,FAC_C,PSTAR, FAC_R,A,B,RHOI,RHOR,MU0,DF,SSP0,G0,DPDMU,PP
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('Gamma', GAMMA, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_P0', P0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_PSH', PSH, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('a', A, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('b', B, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('Refer_Rho', RHO0, IS_AVAILABLE,LSUBMODEL,UNITAB)

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF
      
      !ERREURS :
      ! GAMMA <=0
      
      IF(GAMMA <= ZERO)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/LSZK',
     .               C2='GAMMA MUST BE GREATER THAN 1.0')
      ENDIF
      
      PM(34) = GAMMA
      PM(32) = P0
      PM(88) = PSH
      PM(35) = A
      PM(36) = B
      IF(PM(79)==ZERO)PM(79)=THREE100

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
      
      IF(MU0 <= -ONE)MU0=-ONE+EP12
    
      IF(GAMMA > ONE)THEN
        E0 = (P0-A*EXP(B*LOG(ONE+MU0)))/(GAMMA-ONE)/(ONE+MU0)
      ELSE
        E0 = ZERO ! this error is managed above
      ENDIF
      
      IF(E0 < ZERO)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/LSZK',
     .               C2='PARAMETERS RESULTS INTO A NEGATIVE INITIAL ENERGY')      
      ENDIF
                
      PM(23) = E0
      PM(31) = P0-PSH
      PM(104)= P0-PSH
      
      !SSP0
      SSP0 = ZERO 
      G0 = PM(22)
      PP = (GAMMA-ONE)*(ONE+MU0)*E0 + A*EXP(B*LOG(ONE+MU0))
      RHOI = PM(89)      
      DPDMU = (GAMMA-ONE)*E0+A*B/(ONE+MU0)*EXP((B-ONE)*LOG(ONE+MU0))+(GAMMA-ONE)*DF*PP   !total derivative
      DPDMU = MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0
      
      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)GAMMA,P0,PSH,A,B,RHO0
      ENDIF

      RETURN
 1000 FORMAT(
     & 5X,'  LANDAU-STANYUKOVICH-ZELDOVICH-KOMPANEETS EOS (LSZK)    ',/,
     & 5X,'  ---------------------------------------------------     ',/)
 1500 FORMAT(
     & 5X,'GAMMA . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'P0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PSH . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'b . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'RHO0. . . . . . . . . . . . . . . . . . .=',1PG20.13/)

      RETURN
      END SUBROUTINE HM_READ_EOS_LSZK
