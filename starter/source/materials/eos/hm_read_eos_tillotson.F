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
Chd|  HM_READ_EOS_TILLOTSON         source/materials/eos/hm_read_eos_tillotson.F
Chd|-- called by -----------
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EOS_TILLOTSON(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
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
C   TILLOTSON EQUATION OF STATE
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
#include      "tabsiz_c.inc"
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
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   C1, C2, A, B, ER, ES, VS, E0, RHO0,RHOI,RHOR, ALPHA, BETA,
     .   FAC_L,FAC_T,FAC_M,FAC_C,FACC1,FACC2,FACPB,MU0,MU2,DF,ETA,
     .   OMEGA,AA,BB,PP,XX,EXPA,EXPB
      LOGICAL :: IS_ENCRYPTED,IS_AVAILABLE
      my_real :: DPDMU, SSP0, G0
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('EOS_C1', C1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_C2', C2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_A', A, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_B', B, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('E_R',ER, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('E_S', ES, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vs', VS, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_EA', E0 ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0 ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('Alpha', ALPHA ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Beta', BETA ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0
      ELSE
        RHO0=RHOR
      ENDIF

      PM(23)  = E0
      PM(32)  = C1
      PM(33)  = C2
      PM(34)  = A
      PM(35)  = B
      PM(36)  = ER
      IF(PM(79)==ZERO)PM(79)=THREE100
      PM(160) = ES
      PM(161) = VS
      PM(162) = ALPHA
      PM(163) = BETA

      !COMPUTING INITIAL PRESSURE FOR PM(31) -> SIG(1:3,*)

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

      MU2   = MU0*MU0
      FACC1 = ONE
      FACC2 = ONE
      FACPB = ONE
      IF(MU0<ZERO) THEN
        FACC2=ZERO
        IF(DF>VS .OR. (DF<=VS .AND. E0>=ES) ) THEN
         XX    = MU0/(ONE+MU0)
         EXPA  = EXP(-ALPHA*XX*XX)
         EXPB  = EXP(BETA*XX)
         FACC1 = EXPA*EXPB
         FACPB = EXPA
        ENDIF
      ENDIF
      ETA   = ONE+MU0
      OMEGA = ONE+E0/(ER*ETA**2)
      AA    = FACC1*C1*MU0+FACC2*C2*MU2
      BB    = A+FACPB*B/OMEGA
      PP    = MAX(AA+BB*ETA*E0,PM(37))
      PM(31)= PP

      !SSP0
      SSP0 = ZERO
      G0 = PM(22)
      RHOI = PM(89)

      DPDMU=FACC1*C1+TWO*FACC2*C2*MU0+BB*ETA*PP*DF*DF
     .         +E0*( BB+(TWO*E0/ETA-PP*DF*DF)
     .         *B*FACPB/(ER*ETA*OMEGA**2) )
      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR)
      PM(27)=SSP0

      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)C1,C2,A,B,ER,ES,VS,E0,PM(31),PM(1),ALPHA,BETA
      ENDIF

      RETURN

 1000 FORMAT(
     & 5X,'  TILLOTSON EOS     ',/,
     & 5X,'  --------------     ',/)
 1500 FORMAT(
     & 5X,'C1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REF INTERNAL ENERGY (PER UNIT VOLUME) . .=',1PG20.13/,
     & 5X,'SUBLIMATION ENERGY  (PER UNIT VOLUME) . .=',1PG20.13/,
     & 5X,'SUBLIMATION RELATIVE VOLUME . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL INTERNAL ENERGY(PER UNIT VOLUME).=',1PG20.13/,
     & 5X,'INITIAL PRESSURE. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY. . .  . . . . . . . . .=',1PG20.13/,
     & 5X,'ALPHA . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BETA. . . . . . . . . . . . . . . . . . .=',1PG20.13//)

      RETURN
      END

