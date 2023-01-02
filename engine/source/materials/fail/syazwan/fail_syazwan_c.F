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
Chd|  FAIL_SYAZWAN_C                source/materials/fail/syazwan/fail_syazwan_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_SYAZWAN_C(
     1           NEL      ,UPARAM   ,NUPARAM  ,UVAR     ,NUVAR    ,
     2           TIME     ,NGL      ,IPT      ,DPLA     ,PLA      ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     5           DFMAX    ,NFUNC    ,IFUNC    ,ALDT     ,FOFF     ,
     6           IPG      ,DMG_FLAG ,DMG_SCALE,NPF      ,TF       )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "scr18_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: 
     .   NEL    ,NUPARAM,NUVAR  ,IPT    ,NFUNC  ,
     .   IPG    ,NGL(NEL),IFUNC(NFUNC)
      my_real, INTENT(IN) ::
     .   TIME   ,UPARAM(NUPARAM),DPLA(NEL) ,PLA(NEL),
     .   ALDT(NEL),EPSXX(NEL)   ,EPSYY(NEL),
     .   EPSXY(NEL),EPSYZ(NEL)  ,EPSZX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: 
     .   DMG_FLAG,FOFF(NEL)
      my_real, INTENT(INOUT) :: 
     .   UVAR(NEL,NUVAR),DFMAX(NEL),DMG_SCALE(NEL)      ,
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),
     .   SIGNZX(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF)
      my_real
     .         FINTER
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,INST,DINIT,IFORM
      INTEGER, DIMENSION(MVSIZ) :: INDX
      my_real
     .   C1     ,C2     ,C3     ,C4     ,C5     ,C6     ,
     .   N_VAL  ,SOFTEXP,REF_LEN,REG_SCALE,DAM_SF,MAX_DAM
      my_real
     .   LAMBDA ,DYDX   ,FAC(NEL),DC(NEL),P      ,SVM    ,
     .   TRIAX  ,COS3THETA,LODEP ,EPSFAIL,E12    ,S1     ,
     .   S2     ,Q      ,EMAJ    ,EMIN   ,BETA   ,ALPHA  ,
     .   EQUA1  ,EQUA2  ,E1      ,EPS_C  ,DINST(NEL),EPFMIN,
     .   DELTA  ,DENOM
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      C1        = UPARAM(1) 
      C2        = UPARAM(2) 
      C3        = UPARAM(3)
      C4        = UPARAM(4) 
      C5        = UPARAM(5)
      C6        = UPARAM(6)
      IFORM     = NINT(UPARAM(7))
      DINIT     = NINT(UPARAM(8))
      DAM_SF    = UPARAM(9)
      MAX_DAM   = UPARAM(10)
      INST      = NINT(UPARAM(11))
      N_VAL     = UPARAM(12) 
      SOFTEXP   = UPARAM(13) 
      REF_LEN   = UPARAM(14) 
      REG_SCALE = UPARAM(15) 
      EPFMIN    = UPARAM(16)
c
      ! Set flag for stress softening
      IF (INST > 0) DMG_FLAG = 1
c
      ! At first timestep, initialization of the critical damage and 
      ! the element size scaling factor
      IF (UVAR(1,1) == ZERO) UVAR(1:NEL,1) = ONE
      IF (UVAR(1,3) == ZERO) THEN
        IF (IFUNC(1) > 0) THEN 
          DO I=1,NEL
            LAMBDA    = ALDT(I)/REF_LEN
            UVAR(I,3) = FINTER(IFUNC(1),LAMBDA,NPF,TF,DYDX)
            UVAR(I,3) = UVAR(I,3)*REG_SCALE
          ENDDO
        ELSE 
          UVAR(1:NEL,3) = ONE
        ENDIF
      ENDIF
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
        ! Critical damage for necking instability
        DC(I)    = UVAR(I,1)
        ! Instability criterion
        DINST(I) = UVAR(I,2)
        ! Recover element size scaling
        FAC(I)   = UVAR(I,3)
      ENDDO
c
      !====================================================================
      ! - DAMAGE INITIALIZATION
      !==================================================================== 
      IF (TIME == ZERO .AND. ISIGI /= 0 .AND. DINIT > 0) THEN
        DO I = 1,NEL
          IF (PLA(I) > ZERO) THEN
            ! Principal strains computation
            E12   = HALF*EPSXY(I)
            S1    = HALF*(EPSXX(I) + EPSYY(I))
            S2    = HALF*(EPSXX(I) - EPSYY(I))
            Q     = SQRT(S2**2 + E12**2)
            EMAJ  = S1 + Q
            EMIN  = S1 - Q
            ! Computation of the principal strains ratio BETA
            BETA  = EMIN/MAX(EMAJ,EM20)
            BETA  = MAX(-ONE,BETA)
            BETA  = MIN( ONE,BETA)
            ! Estimation of stress triaxiality value
            TRIAX = (ONE/SQRT(THREE))*((ONE + BETA)/SQRT(ONE + BETA + BETA**2))
            IF (TRIAX < -TWO_THIRD) TRIAX = -TWO_THIRD
            IF (TRIAX >  TWO_THIRD) TRIAX =  TWO_THIRD    
            ! Computation of Lode parameter
            COS3THETA = -HALF*TWENTY7*TRIAX*(TRIAX**2 - THIRD)
            IF (COS3THETA < -ONE ) COS3THETA = -ONE
            IF (COS3THETA >  ONE ) COS3THETA =  ONE
            LODEP = ONE - TWO*ACOS(COS3THETA)/PI                    
            ! Computation of the plastic strain at failure
            EPSFAIL = C1 + C2*TRIAX + C3*LODEP + C4*(TRIAX**2) + 
     .                C5*(LODEP**2) + C6*TRIAX*LODEP
            EPSFAIL = EPSFAIL*FAC(I)
            EPSFAIL = MAX(EPFMIN,EPSFAIL)
            ! Estimation of the damage variable value
            DFMAX(I) = PLA(I)/MAX(EPSFAIL,EM6)
            DFMAX(I) = DFMAX(I)*DAM_SF
            DFMAX(I) = MIN(MAX_DAM,DFMAX(I))
            ! Computation of instability curve and stress softening
            IF (INST > 0 .AND. TRIAX > EM10) THEN
              ! Computation of the principal strain ratio
              ALPHA = (TWO*BETA + ONE)/(TWO + BETA)
              ! Computation of the effective plastic strain
              EQUA1 = ONE  - ALPHA + ALPHA**2
              EQUA2 = FOUR - THREE*ALPHA - THREE*ALPHA**2 + FOUR*ALPHA
              E1    = (TWO*(TWO-ALPHA)*EQUA1/MAX(EQUA2,EM20))*N_VAL
              EPS_C = E1*SQRT(FOUR*THIRD*(ONE + BETA + BETA**2))
              EPS_C = EPS_C*FAC(I)
              ! Update instability criterion
              DINST(I)  = PLA(I)/MAX(EPS_C,EM20)
              DINST(I)  = MIN(DINST(I),ONE)
              UVAR(I,2) = DINST(I) 
              ! Test if the instability curve is reached
              IF (DINST(I) >= ONE) THEN
                DC(I)     = DFMAX(I)
                UVAR(I,1) = DC(I)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index
      NINDX = 0  
      INDX(1:NEL) = 0
c
      ! Loop over the elements 
      DO I=1,NEL
c
        IF (FOFF(I) /= 0 .AND. DPLA(I) > ZERO) THEN
c
          ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
          P = THIRD*(SIGNXX(I) + SIGNYY(I))
          ! Von Mises equivalent stress
          SVM = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I) +
     .             THREE*SIGNXY(I)**2
          SVM = SQRT(MAX(SVM,ZERO))
          TRIAX = P/MAX(EM20,SVM)
          IF (TRIAX < -TWO_THIRD) TRIAX = -TWO_THIRD
          IF (TRIAX >  TWO_THIRD) TRIAX =  TWO_THIRD
c
          ! Computation of Lode parameter
          COS3THETA = -HALF*TWENTY7*TRIAX*(TRIAX**2 - THIRD)
          IF (COS3THETA < -ONE ) COS3THETA = -ONE
          IF (COS3THETA >  ONE ) COS3THETA =  ONE
          LODEP = ONE - TWO*ACOS(COS3THETA)/PI
c
          ! Computation of the plastic strain at failure
          EPSFAIL = C1 + C2*TRIAX + C3*LODEP + C4*(TRIAX**2) + 
     .              C5*(LODEP**2) + C6*TRIAX*LODEP
          EPSFAIL = EPSFAIL*FAC(I)
          EPSFAIL = MAX(EPFMIN,EPSFAIL)
c
          ! Computation of the damage variable update 
          DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPSFAIL,EM20)
          DFMAX(I) = MIN(DFMAX(I),ONE) 
c
          ! Computation of instability curve and stress softening
          IF (INST > 0) THEN
            ! If the instability has not been reached
            IF (DINST(I) < ONE .AND. TRIAX > EM10) THEN 
              ! Computation of the principal strains ratio BETA from stress triaxiality
              DELTA = THREE*(TRIAX**2)*(FOUR - NINE*(TRIAX**2))
              DENOM = TWO*(THREE*(TRIAX**2)-ONE)
              DENOM = SIGN(MAX(ABS(DENOM),EM20),DENOM)
              BETA  = ((TWO - THREE*(TRIAX**2))-SQRT(DELTA))/DENOM
              BETA  = MAX(-ONE,BETA)
              BETA  = MIN( ONE,BETA)
              ! Computation of the principal stress ratio
              ALPHA = (TWO*BETA + ONE)/(TWO + BETA)
              ! Computation of the effective plastic strain
              EQUA1 = ONE  - ALPHA + ALPHA**2
              EQUA2 = FOUR - THREE*ALPHA - THREE*ALPHA**2 + FOUR*ALPHA
              E1    = (TWO*(TWO-ALPHA)*EQUA1/MAX(EQUA2,EM20))*N_VAL
              EPS_C = E1*SQRT(FOUR*THIRD*(ONE + BETA + BETA**2))
              EPS_C = EPS_C*FAC(I)
              ! Update instability criterion
              IF (IFORM == 2) THEN 
                DINST(I) = MAX(DINST(I),PLA(I)/MAX(EPS_C,EM20))
              ELSE 
                DINST(I) = DINST(I) + DPLA(I)/MAX(EPS_C,EM20)
              ENDIF
              DINST(I)  = MIN(DINST(I),ONE)
              UVAR(I,2) = DINST(I) 
              ! Test if the instability curve is reached
              IF (DINST(I) >= ONE) THEN
                DC(I)     = DFMAX(I)
                UVAR(I,1) = DC(I)
              ENDIF
            ENDIF  
          ENDIF
c              
          ! Check element failure    
          IF (DFMAX(I) >= ONE) THEN
            FOFF(I)     = 0
            NINDX       = NINDX + 1
            INDX(NINDX) = I
          ENDIF
        ENDIF
      ENDDO
c
      !====================================================================
      ! - UPDATE DAMAGE SCALE FACTOR FOR NECKING INSTABILITY
      !====================================================================
      IF (INST > 0) THEN
        DO I = 1,NEL 
          ! Computation of the damage scale factor 
          IF (DFMAX(I) >= DC(I)) THEN 
            IF (DC(I) < ONE) THEN 
              DMG_SCALE(I) = ONE - ((DFMAX(I)-DC(I))/MAX(ONE-DC(I),EM20))**SOFTEXP
            ELSE
              DMG_SCALE(I) = ZERO
            ENDIF
          ELSE
            DMG_SCALE(I) = ONE
          ENDIF
        ENDDO
      ENDIF
c
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED ELEMENTS
      !====================================================================
      IF (NINDX > 0) THEN
        DO J = 1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),IPG,IPT,TIME
          WRITE(ISTDO,1000) NGL(I),IPG,IPT,TIME
#include "lockoff.inc"
        ENDDO
      ENDIF           
C---------
 1000 FORMAT(1X,'FOR SHELL ELEMENT NUMBER el#',I10,
     .          ' FAILURE (SYAZWAN) AT GAUSS POINT ',I3,' LAYER ',I3,
     .          ' AT TIME :',1PE12.4)
c---------
       END
