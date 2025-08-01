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
!||    hm_read_eos_polynomial   ../starter/source/materials/eos/hm_read_eos_polynomial.F
!||--- called by ------------------------------------------------------
!||    hm_read_eos              ../starter/source/materials/eos/hm_read_eos.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_EOS_POLYNOMIAL(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS,ISFLUID)
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
C   POLYNOMIAL EQUATION OF STATE
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
      INTEGER,INTENT(INOUT) :: ISFLUID
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   C0, C1, C2, C3, C4, C5, E0, PSH, RHO0,RHOR,MU0,
     .   FAC_L,FAC_T,FAC_M,FAC_C,RHOI, G0, SSP0, DF, DPDMU
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE, IS_AVAILABLE_RHO0     
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_AVAILABLE_RHO0 = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_C0', C0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C1', C1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C2', C2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C3', C3, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C4', C4, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_C5', C5, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_EA', E0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_PSH', PSH, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0, IS_AVAILABLE_RHO0,LSUBMODEL,UNITAB)

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF

      PM( 23)=E0
      PM(104)=C0-PSH
      PM( 32)=C1
      PM( 33)=C2
      PM( 34)=C3
      PM( 35)=C4
      PM( 36)=C5
      PM( 88)=PSH
      IF(PM(79)==ZERO)PM(79)=THREE100
  
      !initial pressure for stress tensor
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
      
      IF(MU0 >= ZERO)THEN
        PM(31)=C0-PSH+C1*MU0+C2*MU0**2+C3*MU0**3+(C4+C5*MU0)*E0
      ELSE
        PM(31)=C0-PSH+C1*MU0          +C3*MU0**3+(C4+C5*MU0)*E0
      ENDIF
      
      !SSP0
      SSP0 = ZERO 
      G0 = PM(22)
      RHOI = PM(89)
      DPDMU = C1 + TWO*C2 *MAX(ZERO,MU0)+THREE*C3 *MU0*MU0 + C5 *E0+(C4 +C5 *MU0)*DF*DF*(PM(31)+PSH)
      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0

      IF(C1 == ZERO .AND. C2 == ZERO .AND. C3 == ZERO .AND. C4 == C5 .AND. C4 > ONE)THEN
        ISFLUID = 1
      ENDIF

      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)C0,C1,C2,C3,C4,C5,E0,PM(104),PSH
        IF(IS_AVAILABLE_RHO0)WRITE(IOUT,1501)PM(1)
      ENDIF

      RETURN
 1000 FORMAT(
     & 5X,'  POLYNOMIAL EOS     ',/,
     & 5X,'  --------------     ',/)
 1500 FORMAT(
     & 5X,'C0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C3. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C4. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C5. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL INTERNAL ENERGY PER UNIT VOLUME .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PRESSURE SHIFT. . . . . . . . . . . . . .=',1PG20.13)
 1501 FORMAT(     
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13)      

      RETURN
      END

