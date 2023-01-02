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
Chd|  FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_BIQUAD_S(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,
     2     NPF    ,TF     ,TIME   ,TIMESTEP ,UPARAM  ,
     3     NGL    , IPM    ,MAT,
     4     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     5     DPLA    ,EPSP  ,TSTAR   ,UVAR    ,OFF     ,IP     ,
     6     DFMAX   ,TDELE ,ALDT)
C!-----------------------------------------------
C!   I m p l i c i t   T y p e s
C!-----------------------------------------------
#include      "implicit_f.inc"
C!---------+---------+---+---+--------------------------------------------
C! VAR     | SIZE    |TYP| RW| DEFINITION
C!---------+---------+---+---+--------------------------------------------
C! NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C! NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C! NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C!---------+---------+---+---+--------------------------------------------
C! MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C! KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C! NPF     |  *      | I | R | FUNCTION ARRAY   
C! TF      |  *      | F | R | FUNCTION ARRAY 
C!---------+---------+---+---+--------------------------------------------
C! TIME    |  1      | F | R | CURRENT TIME
C! TIMESTEP|  1      | F | R | CURRENT TIME STEP
C! UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C!---------+---------+---+---+--------------------------------------------
C! SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C! SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C! ...     |         |   |   |
C! ...     |         |   |   |
C!---------+---------+---+---+--------------------------------------------
C! UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C! OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C!---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "param_c.inc"
#include "impl1_c.inc"
C!-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IPM(NPROPMI,*),
     .        MAT(NEL),IP
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),UVAR(NEL,NUVAR),
     .   DPLA(NEL),EPSP(NEL),TSTAR(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL),
     .   ALDT(NEL)     
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C!        Y = FINTER(IFUNC(J),X,NPF,TF,DF)
C!        Y       : y = f(x)
C!        X       : x
C!        DF      : f'(x) = dy/dx
C!        IFUNC(J): FUNCTION INDEX
C!              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C!        NPF,TF  : FUNCTION PARAMETER
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,J,IDEL,IDEV,IFLAG(MVSIZ),INDX(MVSIZ),IADBUF,NINDX,
     .        INDEX(MVSIZ),IR,IFAIL,JJ,MATID,SEL
      my_real 
     .        d1(MVSIZ),DF,
     .        EPSP0(MVSIZ),P,triaxs,SVM,SCALE,SXX,SYY,SZZ
      my_real EPS_FAIL
      my_real P1X,P1Y,S1X,S1Y,S2Y, A1, B1, C1, REF_EL_LEN, LAMBDA,FAC

      my_real X_1(3) , X_2(3)

C!--------------------------------------------------------------
       IR    = 0
       MATID = MAT(1)
       X_1(1) = UPARAM(1)
       X_1(2) = UPARAM(2)
       X_1(3) = UPARAM(3)
       X_2(1) = UPARAM(4)
       X_2(2) = UPARAM(5)
       X_2(3) = UPARAM(6)
       d1     = UPARAM(7)
       SEL    = INT(UPARAM(11)+0.0001)
       IF (SEL == 3) SEL = 2
       REF_EL_LEN   = UPARAM(13)
C-----------------------------------------------
C!  Initialization
C-----------------------------------------------
       IF (TIME == ZERO .AND. MFUNC > 0) THEN
        DO I=1,NEL
          IF (NUVAR == 3) THEN
            UVAR(I,3) = ALDT(I) 
            LAMBDA = UVAR(I,3) / REF_EL_LEN
            FAC = FINTER(KFUNC(1),LAMBDA,NPF,TF,DF) 
            UVAR(I,3) = FAC
          ELSEIF (NUVAR ==9) THEN
            UVAR(I,9) = ALDT(I) 
            LAMBDA = UVAR(I,9) / REF_EL_LEN
            FAC = FINTER(KFUNC(1),LAMBDA,NPF,TF,DF) 
            UVAR(I,9) = FAC
          ENDIF
        ENDDO
       ENDIF
C-----------------------------------------------
c! fast degradation
        DO I=1,NEL
         IF(OFF(I)<EM01) OFF(I)=ZERO
         IF(OFF(I)<ONE.AND.OFF(I)>ZERO) OFF(I)=OFF(I)*FOUR_OVER_5
        END DO
C-------------------    
       
       NINDX = 0  
       
       DO I=1,NEL
        IF(OFF(I)== ONE .AND. DPLA(I) /= ZERO)THEN
           P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
           SXX = SIGNXX(I) - P
           SYY = SIGNYY(I) - P
           SZZ = SIGNZZ(I) - P
           SVM =HALF*(SXX**2 + SYY**2 + SZZ**2)
     .            +SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
           SVM=SQRT(THREE*SVM)
           triaxs = P/MAX(EM20,SVM)
           IF (triaxs<-TWO_THIRD) triaxs = -TWO_THIRD
           IF (triaxs>TWO_THIRD)  triaxs =  TWO_THIRD
           IF (triaxs <= THIRD) THEN
              EPS_FAIL =   X_1(1) +  X_1(2) * triaxs + X_1(3) * triaxs**2
              IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
              IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
           ELSE

              SELECT CASE (SEL)
               CASE(1)
                 EPS_FAIL   =   X_2(1) +  X_2(2) * triaxs + X_2(3) * triaxs**2
                 IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                 IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
               CASE(2)
                 IF (triaxs <= ONE/SQR3) THEN                     ! triax < 0.57735
                   P1X      = THIRD
                   P1Y      = X_1(1) + X_1(2) * P1X + X_1(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                   IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
                 ELSE                                             ! triax > 0.57735
                   P1X      = TWO * THIRD
                   P1Y      = X_2(1) + X_2(2) * P1X + X_2(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * triaxs + A1 * triaxs**2
                   IF (NUVAR == 3) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                   IF (NUVAR == 9) EPS_FAIL = EPS_FAIL * UVAR(I,9)
                 ENDIF
               END SELECT
           ENDIF
           DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
           DFMAX(I) = MIN(ONE,DFMAX(I))
         IF(DFMAX(I)>=ONE.AND.OFF(I)==ONE) THEN
          OFF(I)=FOUR_OVER_5
          NINDX=NINDX+1
          INDX(NINDX)=I
          IDEL7NOK = 1   
          TDELE(I) = TIME    
         ENDIF
        ENDIF 
       ENDDO
       
c------------------------
      
       IF(NINDX>0)THEN
         DO J=1,NINDX
          I = INDX(J)     
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"
         END DO
       END IF         
C---------Damage for output  0 < DFMAX < 1 --------------------
c!       DO J=1,IR
c!          I=JST(J)
c!          DFMAX(I)= MIN(ONE,DFMAX(I))
c!       ENDDO
C------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (BIQUAD) el#',I10,
     .          ' AT TIME :',1PE12.4)     
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (BIQUAD) el#',I10,
     .          ' AT TIME :',1PE12.4)     
      RETURN
      END
