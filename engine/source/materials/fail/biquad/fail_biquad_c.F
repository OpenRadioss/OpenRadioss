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
!||    fail_biquad_c           ../engine/source/materials/fail/biquad/fail_biquad_c.F
!||--- called by ------------------------------------------------------
!||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell           ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||    finter                  ../engine/source/tools/curve/finter.F
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE FAIL_BIQUAD_C(FAIL ,
     1           NEL      ,NUVAR    ,
     2           TIME     ,NGL      ,IPT      ,NPTOT    ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           DPLA     ,UVAR     ,UEL1     ,
     5           OFF      ,OFFL     ,DFMAX    ,TDEL     ,NFUNC    ,
     6           IFUNC    ,NPF      ,TF       ,ALDT     ,FOFF     ,
     7           IPG      ,DMG_FLAG ,DMGSCL   )
C-----------------------------------------------
C    Biquadratic failure model for shells
! ---------------------------- ----------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use table_mat_vinterp_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
#include "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NPT     |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C ISHELL  |  *      | I | R | GEOMETRICAL FLAGS   
C NGL     | NEL     | I | R | ELEMENT NUMBER
C SHF     | NEL     | F | R | SHEAR FACTOR
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C---------+---------+---+---+--------------------------------------------
C IPT                         CURRENT INTEGRATION POINT IN ALL LAYERS
C IPTT                        CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C NPTOT                       NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
C NOFF                        NUMBER OF FAILED INTEGRATION POINTS (TOTAL)
C IGTYP                       PROPERTY TYPE
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR,IPT,NPTOT,NFUNC,IPG
      INTEGER NGL(NEL),IFUNC(NFUNC)
      my_real TIME,DPLA(NEL),UEL1(NEL),ALDT(NEL)
      INTEGER, INTENT(INOUT) :: DMG_FLAG
      type (fail_param_) ,target ,intent(inout) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF
      my_real 
     .  UVAR(NEL,NUVAR),OFF(NEL),OFFL(NEL),
     .  SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .  DFMAX(NEL),TDEL(NEL)
      my_real, INTENT(INOUT) :: DMGSCL(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       my_real pressure, svm, triax,REF_EL_LEN
       INTEGER I,J,NINDX1,NINDX2,FLAG_PERTURB,SFLAG
       INTEGER IREG
       INTEGER ICOUP
       INTEGER, DIMENSION(MVSIZ) :: INDX1,INDX2
       INTEGER, DIMENSION(NEL,1) :: IPOS
       my_real  ,DIMENSION(NEL)   :: REG_FAC,regf_eta,DERI
       my_real  ,DIMENSION(NEL,1) :: ETA
       my_real  X_1(3) , X_2(3),c3,DYDX,DC(NEL)
       my_real  EPS_FAIL, EPS_FAIL2, DAMAGE, INST, DCRIT, EXP
       my_real  P1X,P1Y,S1X,S1Y,S2Y, A1, A2, B1, B2, C1, C2,LAMBDA
C-----------------------------------------------
c! UVAR1 = damage due to instability (triax between 1/3 and 2/3)
c! UVAR2 = actual integration point ON or OFF
c! UVAR3-8 = perturbated parameter
c! UVAR3 (if perturbation is not used) or UVAR9 (if used) = element length scale factor
C-----------------------------------------------
       IREG    = FAIL%IPARAM(1)
       SFLAG   = FAIL%IPARAM(3)
       ICOUP   = FAIL%IPARAM(4)
!
       X_1(1)       = FAIL%UPARAM(1)
       X_1(2)       = FAIL%UPARAM(2)
       X_1(3)       = FAIL%UPARAM(3)
       X_2(1)       = FAIL%UPARAM(4)
       X_2(2)       = FAIL%UPARAM(5)
       X_2(3)       = FAIL%UPARAM(6)
       FLAG_PERTURB = NINT(FAIL%UPARAM(8))
       c3           = FAIL%UPARAM(9)
       INST         = FAIL%UPARAM(12)
       REF_EL_LEN   = FAIL%UPARAM(13)
       NINDX1       = 0  
       NINDX2       = 0
       EPS_FAIL     = ZERO
       EPS_FAIL2    = ZERO
       DCRIT        = FAIL%UPARAM(15)
       EXP          = FAIL%UPARAM(16)
!---------------------------------------------------------------------------
!      Element size scale factor on failure plastic strain
!---------------------------------------------------------------------------
       IF (IREG > 0) THEN 
         IF (NUVAR == 3) THEN 
           IF (UVAR(1,3)==ZERO) THEN 
             DO I=1,NEL
               LAMBDA    = ALDT(I) / REF_EL_LEN
               UVAR(I,3) = FINTER(IFUNC(1),LAMBDA,NPF,TF,DYDX) 
             ENDDO
           ENDIF
           REG_FAC(1:NEL) = UVAR(1:NEL,3)
         ELSEIF (NUVAR == 9) THEN 
           IF (UVAR(1,9)==ZERO) THEN 
             DO I=1,NEL
               LAMBDA    = ALDT(I) / REF_EL_LEN
               UVAR(I,9) = FINTER(IFUNC(1),LAMBDA,NPF,TF,DYDX) 
             ENDDO
           ENDIF
           REG_FAC(1:NEL) = UVAR(1:NEL,9)
         ENDIF
       ENDIF
!
       IF (IREG == 2) THEN
         ipos(1:nel,1) = 1
         do i=1,nel
           pressure = (SIGNXX(I)+ SIGNYY(I)) * THIRD
           svm      = sqrt((SIGNXX(I)**2)+(SIGNYY(I)**2)-(SIGNXX(I)*SIGNYY(I))+(THREE*SIGNXY(I)**2))
           triax    = pressure / max(em20,svm)
           eta(i,1) = triax
         end do
!
         ! calculate triaxiality dependent scaling factor on failure limits
         call table_mat_vinterp(fail%table4d(1),nel,nel,ipos,eta,regf_eta,deri)
         reg_fac(1:nel) = (one - regf_eta(1:nel)) + regf_eta(1:nel)*reg_fac(1:nel)
       END IF
!---------------------------------------------------------------------------
       IF(FLAG_PERTURB == 0) THEN
         DO I =1,NEL
           IF(OFF(I) == ONE .AND. DPLA(I) /= ZERO .AND. FOFF(I) == 1 ) THEN
             triax = eta(i,1)
             DAMAGE = UVAR(I,1)
             IF (triax <= THIRD) THEN     ! triax < 1/3
               EPS_FAIL =   X_1(1) +  X_1(2) * triax + X_1(3) * triax**2
               IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
             ELSE                          ! triax > 1/3
              SELECT CASE (SFLAG)
               CASE(1)
                 EPS_FAIL   =   X_2(1) +  X_2(2) * triax + X_2(3) * triax**2
                 IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
               CASE(2)
                 IF (triax <= ONE/SQR3) THEN                     ! triax < 0.57735
                   P1X      = THIRD
                   P1Y      = X_1(1) + X_1(2) * P1X + X_1(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triax + A1 * triax**2
                   IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
                 ELSE                                             ! triax > 0.57735
                   P1X      = TWO * THIRD
                   P1Y      = X_2(1) + X_2(2) * P1X + X_2(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triax + A1 * triax**2
                   IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
                 ENDIF
               CASE(3)
                 IF (triax <= ONE/SQR3) THEN                       ! triax < 0.57735
                   P1X      = THIRD
                   P1Y      = X_1(1) + X_1(2) * P1X + X_1(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL = C1 + B1 * triax + A1 * triax**2
                   IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
                   EPS_FAIL2 = C2 + B2 * triax + A2 * triax**2   ! INSTABILITY
                   IF (IREG > 0) EPS_FAIL2 = EPS_FAIL2 * REG_FAC(I)
                 ELSE                          ! triax > 0.57735
                   P1X      = TWO * THIRD
                   P1Y      = X_2(1) + X_2(2) * P1X + X_2(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL = C1 + B1 * triax + A1 * triax**2
                   IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
                   EPS_FAIL2 = C2 + B2 * triax + A2 * triax**2   ! INSTABILITY
                   IF (IREG > 0) EPS_FAIL2 = EPS_FAIL2 * REG_FAC(I)
                 ENDIF
                 IF (EPS_FAIL2> ZERO) THEN
                   DAMAGE = DAMAGE + DPLA(I)/EPS_FAIL2
                   DAMAGE = MIN(ONE,DAMAGE)
                   UVAR(I,1) = DAMAGE
                   IF ((DAMAGE >= ONE).AND.(UVAR(I,2) == ZERO)) THEN 
                     UVAR(I,2) = DFMAX(I)
                   ENDIF
                 ENDIF
               END SELECT
           ENDIF
           
           DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
           DFMAX(I) = MIN(ONE,DFMAX(I))
           
           IF (DFMAX(I) >= ONE) THEN
             FOFF(I)       = 0
             NINDX1        = NINDX1 + 1
             INDX1(NINDX1) = I  
           ENDIF
           
           IF (DAMAGE >= ONE .AND. OFFL(I) == ONE .AND. ICOUP == 0) THEN  
             UEL1(I) = UEL1(I) + ONE
             OFFL(I) = FOUR_OVER_5
             IF (INT(UEL1(I)) == NPTOT) THEN
               NINDX2        = NINDX2 + 1
               INDX2(NINDX2) = I
               TDEL(I)       = TIME
               OFF(I)        = ZERO
               SIGNXX(I)     = ZERO
               SIGNYY(I)     = ZERO
               SIGNXY(I)     = ZERO
               SIGNYZ(I)     = ZERO
               SIGNZX(I)     = ZERO
             ENDIF
           ENDIF
         ENDIF
         ENDDO
       ELSEIF(FLAG_PERTURB == 1) THEN
         DO I =1,NEL
           IF(OFF(I) == ONE .AND. DPLA(I) /= ZERO .AND. FOFF(I) == 1 ) THEN
             triax  = eta(i,1)
             DAMAGE = UVAR(I,1)
             IF (triax <= THIRD) THEN
             EPS_FAIL =   UVAR(I,3) +  UVAR(I,4) * triax + UVAR(I,5) * triax**2
             IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
             ELSE           ! triax > 1/3
             SELECT CASE (SFLAG)
               CASE(1)
              EPS_FAIL =   UVAR(I,6) +  UVAR(I,7) * triax + UVAR(I,8) * triax**2
                IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
               CASE(2)
                 IF (triax <= ONE/SQR3) THEN
                   P1X      = THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triax + A1 * triax**2
                   IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
                 ELSE 
                   P1X      = TWO * THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triax + A1 * triax**2
                   IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
                 ENDIF
               CASE(3)
                 IF (triax <= ONE/SQR3) THEN
                   P1X      = THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL = C1 + B1 * triax + A1 * triax**2
                   IF (IREG > 0) EPS_FAIL = EPS_FAIL * REG_FAC(I)
                   EPS_FAIL2 = C2 + B2 * triax + A2 * triax**2
                   IF (IREG > 0) EPS_FAIL2 = EPS_FAIL2 * REG_FAC(I)
                 ELSE 
                   P1X      = TWO * THIRD
                   P1Y      = UVAR(I,3) + UVAR(I,4) * P1X + UVAR(I,5) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = UVAR(I,6) + UVAR(I,7) / SQR3  + UVAR(I,8) * (ONE/SQR3)**2
                   S2Y      = INST
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   A2       = (P1Y - S2Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   B2       = -TWO * A2 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   C2       = A2 * S1X**2 + S2Y 
                   EPS_FAIL =  C1 + B1 * triax + A1 * triax**2
                   EPS_FAIL2 = C2 + B2 * triax + A2 * triax**2
                   IF (IREG > 0) EPS_FAIL  = EPS_FAIL  * REG_FAC(I)
                   IF (IREG > 0) EPS_FAIL2 = EPS_FAIL2 * REG_FAC(I)
                 ENDIF
                 IF (EPS_FAIL2 > ZERO) THEN
                   DAMAGE = DAMAGE + DPLA(I)/EPS_FAIL2
                   DAMAGE = MIN(ONE,DAMAGE)
                   UVAR(I,1) = DAMAGE
                   IF ((DAMAGE >= ONE).AND.(UVAR(I,2) == ZERO)) THEN 
                     UVAR(I,2) = DFMAX(I)
                   ENDIF
                 ENDIF
               END SELECT
           ENDIF
           
           DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
           DFMAX(I) = MIN(ONE,DFMAX(I))
             
           IF (DFMAX(I) >= ONE) THEN
             FOFF(I)       = 0
             NINDX1        = NINDX1 + 1
             INDX1(NINDX1) = I  
           ENDIF
           
           IF (DAMAGE >= ONE .AND. OFFL(I) == ONE .AND. ICOUP == 0) THEN  
             UEL1(I) = UEL1(I) + ONE
             OFFL(I) = FOUR_OVER_5           
             IF (INT(UEL1(I)) == NPTOT) THEN
               NINDX2        = NINDX2 + 1
               INDX2(NINDX2) = I
               TDEL(I)       = TIME
               OFF(I)        = ZERO
               SIGNXX(I)     = ZERO
               SIGNYY(I)     = ZERO
               SIGNXY(I)     = ZERO
               SIGNYZ(I)     = ZERO
               SIGNZX(I)     = ZERO
             ENDIF
           ENDIF
         ENDIF
         ENDDO

       ENDIF
c
c------------------------
c      STRESS SOFTENING
c------------------------
       IF (ICOUP > 0) THEN 
         DMG_FLAG = 1
         IF (ICOUP == 1) THEN 
           DO I = 1,NEL 
            IF (DFMAX(I) >= DCRIT) THEN 
              IF (DCRIT < ONE) THEN 
                DMGSCL(I) = ONE - ((DFMAX(I)-DCRIT)/MAX(ONE-DCRIT,EM20))**EXP
              ELSE
                DMGSCL(I) = ZERO
              ENDIF
            ELSE
              DMGSCL(I) = ONE
            ENDIF
           ENDDO
         ELSE
           DO I = 1,NEL 
            DC(I) = UVAR(I,2)
            IF (DC(I) == ZERO) DC(I) = ONE
            IF (DFMAX(I) >= DC(I)) THEN 
              IF (DC(I) < ONE) THEN 
                DMGSCL(I) = ONE - ((DFMAX(I)-DC(I))/MAX(ONE-DC(I),EM20))**EXP
              ELSE
                DMGSCL(I) = ZERO
              ENDIF
            ELSE
              DMGSCL(I) = ONE
            ENDIF
           ENDDO         
         ENDIF
       ENDIF
c------------------------
c------------------------
c------------------------
c------------------------
      IF (NINDX1 > 0) THEN        
        DO J=1,NINDX1             
          I = INDX1(J)         
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,IPT,TIME
          WRITE(ISTDO,2000) NGL(I),IPG,IPT,TIME
#include "lockoff.inc" 
        ENDDO
      ENDIF
      IF (NINDX2 > 0) THEN        
        DO J=1,NINDX2            
          I = INDX2(J)         
#include "lockon.inc"
          WRITE(IOUT, 3000) NGL(I)
          WRITE(IOUT, 2200) NGL(I), TIME
          WRITE(ISTDO,3000) NGL(I)
          WRITE(ISTDO,2200) NGL(I), TIME                 
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDX             
c------------------------
C---------Damage for output  0 < DFMAX < 1 --------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (BIQUAD)',I10,1X,'GAUSS POINT',I3,
     .       1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
 2200 FORMAT(1X,' *** RUPTURE OF SHELL ELEMENT (BIQUAD)',I10,1X,
     . ' AT TIME :',1PE12.4)
 3000 FORMAT(1X,'FOR SHELL ELEMENT (BIQUAD)',I10,
     .       1X,'INSTABILITY REACHED.')
c------------------------
       RETURN
       END
