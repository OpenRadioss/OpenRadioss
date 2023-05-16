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
Chd|  HM_READ_MAT54                 source/materials/mat/mat054/hm_read_mat54.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT54(UPARAM, MAXUPARAM, NUPARAM, NUVAR, IFUNC, 
     .     MAXFUNC, NFUNC, PARMAT, UNITAB ,MAT_ID, 
     .     MTAG, TITR, LSUBMODEL, PM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM), INTENT(INOUT) :: PM     
      my_real, DIMENSION(100), INTENT(INOUT) :: PARMAT
      my_real, DIMENSION(MAXUPARAM), INTENT(INOUT) :: UPARAM
      INTEGER, DIMENSION(MAXFUNC), INTENT(INOUT) :: IFUNC
      INTEGER, INTENT(INOUT) :: NFUNC, NUPARAM, NUVAR
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      INTEGER, INTENT(IN) :: MAT_ID, MAXFUNC, MAXUPARAM
      CHARACTER*nchartitle, INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NF
      my_real E, NU, AY, AZ, BY, BZ, CX, DC, PR, PS, SIG0, H, M, SFAC, 
     .     RHO0, RHOR, FAC_UNIT
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C     S o u r c e   l i n e s 
C-----------------------------------------------
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
      CALL HM_GET_FLOATV('MAT_E', E, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU', NU, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 2
      CALL HM_GET_INTV('FUNC', NF, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_A', SIG0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B', H, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N', M, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Sfac_Yield', SFAC, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 3
      CALL HM_GET_FLOATV('MAT_Ay', AY, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Az', AZ, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_By', BY, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Bz', BZ, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Cx', CX, IS_AVAILABLE, LSUBMODEL, UNITAB)
!     Line 4
      CALL HM_GET_FLOATV('MAT_Dc', DC, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Rc', PR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS', PS, IS_AVAILABLE, LSUBMODEL, UNITAB)

!     Default values
      IF (SFAC == ZERO) THEN
         CALL HM_GET_FLOATV_DIM('MAT_Sfac_Yield', FAC_UNIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         SFAC = ONE * FAC_UNIT
      ENDIF
      IF (AY == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('MAT_Ay', FAC_UNIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         AY = ONE * FAC_UNIT
      ENDIF
      IF (AZ == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('MAT_Az', FAC_UNIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         AZ = ONE * FAC_UNIT
      ENDIF
      IF (BY == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('MAT_By', FAC_UNIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         BY = ONE * FAC_UNIT
      ENDIF
      IF (BZ == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('MAT_Bz', FAC_UNIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         BZ = ONE * FAC_UNIT
      ENDIF
      IF (CX == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('MAT_Cx', FAC_UNIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CX = ONE * FAC_UNIT
      ENDIF
      IF (DC == ZERO .OR. DC >= ONE) THEN 
         DC = ZEP99999
      ENDIF
      IF (PR == ZERO) THEN 
         PR = TWO * INFINITY
      ENDIF
      IF (PS == ZERO) THEN 
         PS = INFINITY
      ENDIF

!     Input checks
      IF (NF == 0) THEN
         IF (SIG0 == ZERO .AND. H == ZERO .AND. M == ZERO) THEN
            CALL ANCMSG(MSGID = 501, MSGTYPE = MSGERROR, ANMODE = ANINFO_BLIND_1,
     .           I1 = MAT_ID, C1 = TITR)
         ENDIF
      ENDIF 
      IF (PR < PS) THEN
         CALL ANCMSG(MSGID = 502, MSGTYPE = MSGERROR, ANMODE = ANINFO_BLIND_1,
     .        I1 = MAT_ID, C1 = TITR)
      ENDIF

!     Material law parameters
      NUPARAM = 19
      NUVAR = 15
      NFUNC = 1       
 
!     Storage
      IFUNC(1) = NF

      UPARAM(6) = HALF * E / (ONE + NU)
      UPARAM(7) = E
      UPARAM(8) = AY
      UPARAM(9) = AZ
      UPARAM(10) = BY
      UPARAM(11) = BZ
      UPARAM(12) = CX
      UPARAM(13) = DC
      UPARAM(14) = PR
      UPARAM(15) = PS
      UPARAM(16) = SIG0
      UPARAM(17) = H
      UPARAM(18) = M
      UPARAM(19) = SFAC

      PARMAT(1) = E
      PARMAT(2) = E
      PARMAT(3) = NU

      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1

      WRITE(IOUT, 800) TRIM(TITR), MAT_ID, 54
      WRITE(IOUT, 1000)
      IF (IS_ENCRYPTED) THEN
         WRITE(IOUT, '(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT, 850) RHO0
         WRITE(IOUT, 1100) E, NU, AY, AZ, BY, BZ, CX
         WRITE(IOUT, 2000)
         WRITE(IOUT, 2100) NF, SFAC, SIG0, H, M
         WRITE(IOUT, 3000)
         WRITE(IOUT, 3100) DC, PR, PS
      ENDIF
      
  800 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
  850 FORMAT(
     & 5X,'INITIAL DENSITY    . . . . . . . . . . . . .=',1PG20.13/)
 1000 FORMAT(
     & 5X,'  ELASTOPLASTIC USER LAW ',/,
     & 5X,'  ---------------------- ',//)
 1100 FORMAT(
     & 5X,'E . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'NU. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'AY. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'AZ. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'By. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BZ. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'CX. . . . . . . . . . . . . . . . . . .=',1PG20.13//)
C
 2000 FORMAT(
     & 5X,'  HARDENING USER PARAMETERS   ',/,
     & 5X,'  ---------------------- ',//)
 2100 FORMAT(
     & 5X,'YIELD FUNCTION NUMBER . . . . . . . . . . . . =',I10/,
     & 5X,'SCALE FACTOR FOR YIELD FUNCTION . . . . =',1PG20.13/,
     & 5X,'YIELD STRESS. . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'PLASTIC MODULUS.  . . . . . . . . . . . =',1PG20.13/,
     & 5X,'EXPONENT. . . . . . . . . . . . . . . . =',1PG20.13///)
c
 3000 FORMAT(
     & 5X,'  DAMAGE PARAMETER USER LAW ',/,
     & 5X,'  ---------------------- ',//)
 3100 FORMAT(
     & 5X,'D CRITIQUE. . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'P RUPTURE  . . . . . . . . . . . .  . . =',1PG20.13/,
     & 5X,'P SEUIL . . . . . . . . . . . . . . . . =',1PG20.13///)
C
      RETURN
      END
