Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    hm_read_eos_osborne      ../starter/source/materials/eos/hm_read_eos_osborne.F
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
      SUBROUTINE HM_READ_EOS_OSBORNE(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE CONSTANT_MOD, ONLY : ZERO, ONE, TWO, THREE100, FOUR, HALF, FOUR, TWO_THIRD
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C reading parameters for
C   OSBORNE EQUATION OF STATE
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
      IMPLICIT NONE
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT, IUNIT
      my_real PM(NPROPM)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT) :: MAT_PARAM
      INTEGER,INTENT(IN) :: IMIDEOS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real  P0, E0, PSH, RHO0
      my_real A1,A2,B0,B1,B2,C0,C1,D0,RHOI,RHOR,dPdmu_partial,dPdE,DF,MU0,DPDMU,A2_,G0,SSP0, DENOM
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE, IS_AVAILABLE_RHO0
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_AVAILABLE_RHO0 = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('EOS_A1', A1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_A2', A2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_B0', B0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_B1', B1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_B2', B2, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('EOS_C0', C0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_C_1', C1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_D0', D0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_P0', P0, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('Refer_Rho', RHO0, IS_AVAILABLE_RHO0,LSUBMODEL,UNITAB)

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
        MAT_PARAM%RHO = RHO0
      ELSE
        RHO0=RHOR                   
      ENDIF
            
      IF(
     .   A1 <= ZERO .OR.
     .   A2 <= ZERO .OR.
     .   B0 <= ZERO .OR.
     .   B1 <= ZERO .OR.
     .   B2 <= ZERO .OR.
     .   C0 <= ZERO .OR.
     .   C1 <= ZERO .OR.
     .   D0 <= ZERO .OR.
     .   RHO0 <= ZERO 
     .   )THEN
         CALL ANCMSG(MSGID=67, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/OSBORNE',
     .               C2='PARAMETERS MUST BE STRICTLY POSITIVE')
      ENDIF

      !solve P(0,E0)=P0  :  2 roots
      !E0 =-HALF*(B0-P0+SQRT(FOUR*C0*D0*P0+B0**TWO-TWO*B0*P0+P0**TWO))/C0  !root-1
      E0 =-HALF*(B0-P0-SQRT(FOUR*C0*D0*P0+B0**TWO-TWO*B0*P0+P0**TWO))/C0   !root-2
      
      
      PM(23) = E0
      PM(32) = (A1+B1+C1*E0*E0) / (E0+D0) !BULK = (1+µ)*dP/dmu (partial derivative at constant E)
      PSH    = ZERO
      PM(88) = PSH
      PM(31) = P0

      MAT_PARAM%EOS%NUPARAM = 9
      MAT_PARAM%EOS%NIPARAM = 0
      MAT_PARAM%EOS%NFUNC = 0
      MAT_PARAM%EOS%NTABLE = 0
      CALL MAT_PARAM%EOS%CONSTRUCT() !allocations

      MAT_PARAM%EOS%UPARAM(1) = A1
      MAT_PARAM%EOS%UPARAM(2) = A2
      MAT_PARAM%EOS%UPARAM(3) = B0
      MAT_PARAM%EOS%UPARAM(4) = B1
      MAT_PARAM%EOS%UPARAM(5) = B2
      MAT_PARAM%EOS%UPARAM(6) = C0
      MAT_PARAM%EOS%UPARAM(7) = C1
      MAT_PARAM%EOS%UPARAM(8) = D0
      MAT_PARAM%EOS%UPARAM(9) = P0

      MAT_PARAM%EOS%PSH = PSH
      MAT_PARAM%EOS%E0 = E0
      IF (MAT_PARAM%THERM%TINI == ZERO) THEN
        MAT_PARAM%THERM%TINI =THREE100
        PM(79) = THREE100
      END IF

      !P0
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
      
      !SSP0
      SSP0 = ZERO 
      G0 = PM(22)
      RHOI = PM(89)
      
      A2_=A2                                                                              
      IF(MU0 < ZERO)A2_=-A2                                                               
      DENOM   = (E0+D0)                                                                   
      dPdmu_partial   = (A1+2*A2_*MU0+(TWO*B2*MU0+B1)*E0+C1*E0*E0)/DENOM                 
      dPdE = (((B2*MU0+B1)*MU0+B0)+(TWO*(C1*MU0+C0))*E0 - P0/DENOM)/DENOM                                         
      DPDMU = dPdmu_partial + dPdE*DF*DF*(P0)   !total derivative                         
      
      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0            

      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)A1,A2,B0,B1,B2,C0,C1,D0,P0
        IF(IS_AVAILABLE_RHO0)WRITE(IOUT,1501)PM(1)
      ENDIF

      RETURN
 1000 FORMAT(
     & 5X,'  OSBORNE EOS     ',/,
     & 5X,'  -----------     ',/)
 1500 FORMAT(
     & 5X,'A1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'D0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'P0. . . . . . . . . . . . . . . . . . . .=',1PG20.13)
 1501 FORMAT(     
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13)      

      RETURN
      END
