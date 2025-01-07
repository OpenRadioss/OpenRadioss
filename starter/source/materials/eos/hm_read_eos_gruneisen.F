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
      !||    hm_read_eos_gruneisen    ../starter/source/materials/eos/hm_read_eos_gruneisen.F
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
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: C, S1, S2, S3, GAMA0, A, E0, RHO0,RHOI,RHOR
      my_real :: MU,MU2,FAC1,DPDMU,PP,BB,AA
      my_real :: MU0, DF, SSP0, G0, FAC, FF, FG, XX, DFF, DFG
      my_real :: PSH !< pressure shift
      my_real :: P0  !< initial pressure
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE, IS_AVAILABLE_RHO0
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_AVAILABLE_RHO0 = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_C', C, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('S1', S1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('S2', S2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('S3', S3, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('GAMMA',GAMA0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_A', A, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('R0E0', E0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('P0', P0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('PSH', PSH, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0 ,IS_AVAILABLE_RHO0,LSUBMODEL,UNITAB)

      IF(A == ZERO) A=GAMA0


      IF(P0 < ZERO)THEN
         CALL ANCMSG(MSGID=67,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IMIDEOS,
     .               C1='/EOS/GRUNEISEN',
     .               C2='INITIAL PRESSURE MUST BE STRICTLY POSITIVE (TOTAL PRESSURE). USE PSH PARAMETER TO SHIFT THE PRESSURE')
      ENDIF

      IF(P0 > ZERO .AND. E0 /= ZERO)THEN
         CALL ANCMSG(MSGID=67,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IMIDEOS,
     .               C1='/EOS/GRUNEISEN',
     .               C2='INITIAL PRESSURE PROVIDED. E0 IS CONSEQUENTLY REDEFINED SUCH AS P(RHO0,E0)=P0')
      ENDIF


      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF

      !COMPUTE INITIAL PRESSURE FOR PM(31)->SIG(1:3,*)
      !SSP0
      IF(RHOI == ZERO)THEN
        MU0 = ZERO ! error 683 already displayed
      ELSE
        IF(RHOR /= ZERO)THEN
          MU0 = RHOI/RHOR-ONE
        ELSE
          MU0 = ZERO ! error 683 already displayed
        ENDIF
      ENDIF
      ! Internal Energy (if P0 is provided, calculate E0 consequently)
      IF(P0 > ZERO)THEN
        E0 = (P0-RHO0*C*C*MU0)/(GAMA0+a*MU0)
      ENDIF
      !Relative volume
      IF(RHOI /= ZERO)THEN
        DF = RHOR/RHOI
      ELSE
        DF = ZERO
      ENDIF
      !pressure
      MU2=MU0*MU0
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
      !derivatives and sound speed
      DPDMU=FAC1*RHOI*C*C+PP*DF*DF*BB+A*E0
      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR)

      ! Storage in Material Buffer PM
      PM(23)  = E0
      PM(32)  = PM(1)*C*C
      PM(33)  = C
      PM(34)  = S1
      PM(35)  = GAMA0
      PM(36)  = A
      PM(88)  = PSH
      PM(160) = S2
      PM(161) = S3
      IF(PM(79)==ZERO)PM(79)=THREE100
      PM(27) = SSP0
      PM(31) = PP - PSH
      PM(104) = PP - PSH
      
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)C,S1,S2,S3,GAMA0,A,E0,PP,PSH,PP-PSH
        IF(IS_AVAILABLE_RHO0)WRITE(IOUT,1501)PM(1)
      ENDIF

      RETURN
      
 1000 FORMAT(
     & 5X,'  MIE-GRUNEISEN EOS     ',/,
     & 5X,'  -----------------     ',/)
 1500 FORMAT(
     & 5X,'C . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'S1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'S2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'S3. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'GAMA0 . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL INTERNAL ENERGY PER UNIT VOLUME .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE . .  . . . . . . . . . .=',1PG20.13/,
     & 5X,'PRESSURE SHIFT . . .  . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE (SHIFTED)  . . . . . . .=',1PG20.13)
 1501 FORMAT(     
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13)
 
      RETURN
      END SUBROUTINE HM_READ_EOS_GRUNEISEN
