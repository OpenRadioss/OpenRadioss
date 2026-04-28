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
!||    hm_read_eos_tillotson    ../starter/source/materials/eos/hm_read_eos_tillotson.F
!||--- called by ------------------------------------------------------
!||    hm_read_eos              ../starter/source/materials/eos/hm_read_eos.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_EOS_TILLOTSON(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS,EOS_TAG,IEOS,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD      
      USE ELBUFTAG_MOD
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,IUNIT
      INTEGER,INTENT(IN) :: IEOS
      my_real PM(NPROPM)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      INTEGER,INTENT(IN) :: IMIDEOS
      TYPE(EOS_TAG_),DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: C1, C2, A, B, EREF, VSUBL, E0, RHO0,RHOI,RHOR, ALPHA, BETA,
     .           MU0, MU2, DF, ETA,
     .           DPDMU, SSP0, G0, PSH, EIV, ECV
      my_real :: ABW, ABWX, DE_W, DN_W, I12, I4, P12, P4, DE_P12, DE_P4,
     .           DM_P12, DM_P4, DENOM, KK, P3, TMP3
      my_real :: ETA2, ETAE, W, W2, PMIN, REGION, C1EB, EAX, C1EBEAX
      my_real :: DPDM12, DPDM4, PP, XX, P0
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE, IS_AVAILABLE_RHO0
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_AVAILABLE_RHO0 = .FALSE.
      PSH = ZERO

      EOS_TAG(IEOS)%NVAR = 1  !saving Region id for H2D and ANIM output
      PMIN = PM(37)

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('EOS_C1', C1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_C2', C2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_A', A, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_B', B, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('E_R',EREF, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('E_CV', ECV, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Vs', VSUBL, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_EA', E0 ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0 ,IS_AVAILABLE_RHO0,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('Alpha', ALPHA ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Beta', BETA ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('MAT_PSH', PSH ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('LAW5_P0', P0 ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('E_IV', EIV, IS_AVAILABLE,LSUBMODEL,UNITAB)

      RHOR = MAT_PARAM%RHO
      RHOI = MAT_PARAM%RHO0
      IF (RHO0 > ZERO) THEN
        RHOR = RHO0
        MAT_PARAM%RHO = RHO0
        PM(1)= RHO0
      ELSE
        RHO0=RHOR                   
      ENDIF

      IF(EIV == ZERO)THEN
        EIV = ECV
      ENDIF

      PM(23) = E0
      PM(32) = C1+B*E0   !BULK = (1+µ)*dP/dmu (partial derivative at constant E)
      PM(88) = PSH

      MAT_PARAM%EOS%NUPARAM = 10
      MAT_PARAM%EOS%NIPARAM = 0
      MAT_PARAM%EOS%NFUNC   = 0
      MAT_PARAM%EOS%NTABLE  = 0
      CALL MAT_PARAM%EOS%CONSTRUCT() !allocations

      MAT_PARAM%EOS%UPARAM(1) = C1
      MAT_PARAM%EOS%UPARAM(2) = C2
      MAT_PARAM%EOS%UPARAM(3) = A
      MAT_PARAM%EOS%UPARAM(4) = B
      MAT_PARAM%EOS%UPARAM(5) = EREF
      MAT_PARAM%EOS%UPARAM(6) = ECV
      MAT_PARAM%EOS%UPARAM(7) = VSUBL
      MAT_PARAM%EOS%UPARAM(8) = ALPHA
      MAT_PARAM%EOS%UPARAM(9) = BETA
      MAT_PARAM%EOS%UPARAM(10) = EIV
      MAT_PARAM%EOS%PSH = PSH
      MAT_PARAM%EOS%E0 = E0
!
      IF (MAT_PARAM%THERM%TINI == ZERO) THEN
        MAT_PARAM%THERM%TINI =THREE100
        PM(79) = THREE100
      END IF

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

      REGION = 1
      IF(MU0 < ZERO)THEN
        IF(DF > VSUBL)THEN
          REGION = 4
        ELSEIF(E0 >= ECV)THEN
          REGION = 4
        ELSEIF(E0 < EIV)THEN
          REGION = 2
          C2 = ZERO
        ELSE
          REGION = 3
          C2 = ZERO
        ENDIF
      ENDIF

      !---INITIAL PRESSURE
      MU2= MU0*MAX(ZERO,MU0)
      ETA = ONE+MU0
      ETA2 = ETA*ETA
      ETAE = ETA*E0
      W = ONE+E0/(EREF*ETA2)
      W2 = W*W
      ABW = (A+B/W)
      DE_W = ONE/(EREF*ETA2)
      DN_W = -TWO*E0/(EREF*ETA2*ETA)
      I12 = ZERO
      I4 = ZERO
      TMP3 = ZERO
      P12 =  ZERO ; P4 = ZERO
      DPDM12 = ZERO ; DPDM4 = ZERO
      DE_P12 = ZERO ; DE_P4 = ZERO
      IF(REGION <= 3)THEN
        P12 = ABW*ETAE+C1*MU0+C2*MU2
        P12 = MAX(P12,PMIN)
        DE_P12 = (-b*DE_W*ETAE)/W2 + ABW*ETA
        DM_P12 = (-B*DN_W*ETAE)/W2 + ABW*E0 + C1 + TWO*C2*MU0
        DPDM12 = DM_P12 + P12/ETA2*DE_P12
        I12 = ONE
      ENDIF
      IF(REGION >= 3)THEN
        XX = MU0/(ONE+MU0)
        C1EB = C1*exp(beta*xx)
        EAX = exp(-alpha*XX*XX)
        C1EBEAX = C1EB*EAX
        ABWX = (A+B*EAX/W)
        P4 = C1EB*EAX*MU0+ ABWX*ETAE
        P4 = MAX(P4,PMIN)
        DE_P4 = -B*EAX*DE_W*ETAE/W2 + ABWX*ETA
        DM_P4 = BETA/ETA2*C1EBEAX*MU0 - TWO*C1EBEAX*ALPHA*XX*MU0/ETA2 + C1EBEAX
     .         +ETAE*(-TWO*B*ALPHA*XX*EAX/W/ETA2 - B*EAX*DN_W/W2) + ABWX*E0
        DPDM4 = DM_P4 + P4/ETA2*DE_P4
        I4 = ONE
      END IF
       IF(REGION == 3)THEN
         DENOM = ECV-EIV
         KK = (E0-EIV)/DENOM
         P3 = (ONE-KK)*P12 + KK*P4
         TMP3 = P4/DENOM - P12/DENOM
         I12 = (ONE-KK)
         I4 = KK
       ENDIF
       PP = I12*P12 + I4*P4
       DPDMU = I12*DPDM12 + I4*DPDM4 + TMP3
       PM(31)= PP-PSH
        
      !SSP0
      SSP0 = ZERO 
      G0   = PM(22)
      RHOI = PM(89)

      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0      

      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)C1,C2,A,B,ALPHA,BETA,EREF,EIV,ECV,VSUBL,E0,PSH,PM(31)
        IF(IS_AVAILABLE_RHO0)WRITE(IOUT,1501)PM(1)
      ENDIF

      RETURN
      
 1000 FORMAT(
     & 5X,'  TILLOTSON EOS',/,
     & 5X,'  -------------',/)
 1500 FORMAT(
     & 5X,'C1. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ALPHA . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BETA. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'EREF:REFERENCE INTERNAL ENERGY. . . . . .=',1PG20.13/,
     & 5X,'EIV:INCIPIENT VAPORIZATION ENERGY . . . .=',1PG20.13/,
     & 5X,'ECV:COMPLETE VAPORIZATION ENERGY. . . . .=',1PG20.13/,
     & 5X,'VSUBL:SUBLIMATION RELATIVE VOLUME . . . .=',1PG20.13/,
     & 5X,'E0:INITIAL INTERNAL ENERGY. . . . . . . .=',1PG20.13/,
     & 5X,'PSH:PRESSURE SHIFT. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE. . . . . . . . . . . . .=',1PG20.13)

 1501 FORMAT(     
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13)     
 
      RETURN
      END

