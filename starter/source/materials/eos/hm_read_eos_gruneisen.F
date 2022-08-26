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
Chd|  HM_READ_EOS_GRUNEISEN         source/materials/eos/hm_read_eos_gruneisen.F
Chd|-- called by -----------
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EOS_GRUNEISEN(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
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
C   MIE-GRUNEISEN EQUATION OF STATE
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
      my_real :: C, S1, S2, S3, GAMA0, A, E0, RHO0,RHOI,RHOR,FAC_L,FAC_T
      my_real :: FAC_M,FAC_C,MU,MU2,MU3,MUP1,NUM,DENOM,FAC1,DPDMU,PP,BB,AA
      my_real :: MU0, DF, SSP0, G0, FAC, FF, FG, XX, DFF, DFG
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_C', C, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('S1', S1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('S2', S2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('S3', S3, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('GAMMA',GAMA0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_A', A, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('R0E0', E0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0 ,IS_AVAILABLE,LSUBMODEL,UNITAB)


      IF(A.EQ.ZERO) A=GAMA0

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF

      PM(23)  = E0
      PM(32)  = PM(1)*C*C
      PM(33)  = C
      PM(34)  = S1
      PM(35)  = GAMA0      
      PM(36)  = A
      PM(160) = S2
      PM(161) = S3
      IF(PM(79)==ZERO)PM(79)=THREE100

      !COMPUTE INITIAL PRESSURE FOR PM(31)->SIG(1:3,*)
      !SSP0
      MU0 = RHOI/RHOR-ONE
      MU2=MU0*MU0
      DF = RHOR/RHOI 
      PP=PM(31)    
      SSP0 = ZERO 
      G0 = PM(22)
      RHOI = PM(89) 
        FAC=ONE
        FAC1=ONE
        IF(MU0>0)THEN
         XX= MU0/(ONE+MU0)
         FF=ONE+(ONE-HALF*GAMA0)*MU0-HALF*A*MU2
         FG=ONE-(S1-ONE+S2*XX+S3*XX*XX)*MU0
         FAC=FF/(FG*FG)
         DFF=ONE-HALF*GAMA0-A*MU0
         DFG=ONE-S1+XX*(-TWO*S2+XX*(S2-THREE*S3)+TWO*S3*XX*XX)
         FAC1=FAC*(ONE+MU0*(DFF/FF-TWO*DFG/FG))        
        ENDIF       
        AA=FAC*RHOR*C*C*MU0
        BB=GAMA0+A*MU0
        PP=MAX(AA+BB*E0,PM(37))
        DPDMU=FAC1*RHOI*C*C+PP*DF*DF*BB+A*E0
      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0
      PM(31) = PP     
      PM(104) = PP       
      
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)C,S1,S2,S3,GAMA0,A,E0,PM(31),PM(1)
      ENDIF

      RETURN
      
 1000 FORMAT(
     & 5X,'  GRUNEISEN EOS     ',/,
     & 5X,'  --------------     ',/)
 1500 FORMAT(
     & 5X,'C . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'S1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'S2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'S3. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'GAMA0 . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL INTERNAL ENERGY PER UNIT VOLUME .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE . .  . . . . . . . . . .=',1PG20.13/,
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13//)

 999  CALL FREERR(3)
 
      RETURN
      END SUBROUTINE HM_READ_EOS_GRUNEISEN
