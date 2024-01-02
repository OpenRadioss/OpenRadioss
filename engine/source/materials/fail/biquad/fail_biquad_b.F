Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FAIL_BIQUAD_B                 source/materials/fail/biquad/fail_biquad_b.F
Chd|-- called by -----------
Chd|        FAIL_BEAM3                    source/elements/beam/fail_beam3.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_BIQUAD_B(
     .           NEL      ,NGL      ,NUPARAM  ,UPARAM   ,
     .           TIME     ,SVM      ,PRESSURE ,
     .           DPLA     ,OFF      ,DFMAX    ,
     .           TDEL     ,IOUT     ,ISTDO    ,IFUNC    ,NFUNC,
     .           SNPC     ,NPF      , STF     , TF      ,
     .           NUVAR    , UVAR   ,AL)           
C-----------------------------------------------
c    BIQUAD- failure model for standard beams (TYPE 3) IRUP=30
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                     ,INTENT(IN)    :: NEL     ! size of element group
      INTEGER                     ,INTENT(IN)    :: NUPARAM ! size of parameter array
      INTEGER                     ,INTENT(IN)    :: IOUT    ! output file unit
      INTEGER                     ,INTENT(IN)    :: ISTDO   ! output file unit
      INTEGER                     ,INTENT(IN)    :: NFUNC   ! number of functions
      INTEGER                     ,INTENT(IN)    :: SNPC 
      INTEGER                     ,INTENT(IN)    :: STF       
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      INTEGER ,DIMENSION(100)     ,INTENT(IN)    :: IFUNC   ! table of functions identifiers
      INTEGER ,DIMENSION(SNPC)    ,INTENT(IN)    :: NPF
      INTEGER                     ,INTENT(IN)    :: NUVAR
      my_real ,DIMENSION(STF)     ,INTENT(IN)    :: TF
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: DPLA    ! plastic strain increment
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: SVM     ! Von Mises stress
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: PRESSURE! element pressure

      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element desactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
      my_real ,DIMENSION(NEL,NUVAR),INTENT(INOUT) :: UVAR   
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,J,IDEL,IDEV,IFLAG(NEL),INDX(NEL),IADBUF,NINDX,
     .        INDEX(NEL),IR,IFAIL,JJ,MATID,SEL,ICOUP
      my_real 
     .        D1,DF,TRIAX,SCALE,SXX,SYY,SZZ
      my_real EPS_FAIL,FINTER
      EXTERNAL FINTER
      my_real P1X,P1Y,S1X,S1Y,S2Y, A1, B1, C1, REF_EL_LEN, LAMBDA,FAC,DCRIT,EXP
      my_real X_1(3) , X_2(3)

C!--------------------------------------------------------------
       IR    = 0
       X_1(1) = UPARAM(1)
       X_1(2) = UPARAM(2)
       X_1(3) = UPARAM(3)
       X_2(1) = UPARAM(4)
       X_2(2) = UPARAM(5)
       X_2(3) = UPARAM(6)
       D1     = UPARAM(7)
       SEL    = INT(UPARAM(11)+0.0001)
       IF (SEL == 3) SEL = 2
       REF_EL_LEN   = UPARAM(13)
       ICOUP = NINT(UPARAM(14))
       DCRIT = UPARAM(15)
       EXP   = UPARAM(16)
 
C-----------------------------------------------
C!  Initialization
C-----------------------------------------------
       IF (NFUNC > 0) THEN 
        IF (NUVAR == 3) THEN 
          IF (UVAR(1,3)==ZERO) THEN 
            DO I=1,NEL
              UVAR(I,3) = AL(I) 
              LAMBDA = UVAR(I,3) / REF_EL_LEN
              FAC = FINTER(IFUNC(1),LAMBDA,NPF,TF,DF) 
              UVAR(I,3) = FAC
            ENDDO
          ENDIF
        ELSEIF (NUVAR == 9) THEN 
          IF (UVAR(1,9)==ZERO) THEN 
            DO I=1,NEL
              UVAR(I,9) = AL(I) 
              LAMBDA = UVAR(I,9) / REF_EL_LEN
              FAC = FINTER(IFUNC(1),LAMBDA,NPF,TF,DF) 
              UVAR(I,9) = FAC
            ENDDO
          ENDIF
        ENDIF
      ENDIF
c-----------------------------
       
       NINDX = 0  
       
      DO I=1,NEL
        IF (OFF(I) == ONE .AND. DPLA(I) > ZERO) THEN
           TRIAX =  PRESSURE(I) / MAX(EM20, SVM(I))
           IF (TRIAX<-TWO_THIRD) TRIAX = -TWO_THIRD
           IF (TRIAX>TWO_THIRD)  TRIAX =  TWO_THIRD
           IF (TRIAX <= THIRD) THEN
              EPS_FAIL =   X_1(1) +  X_1(2) * TRIAX + X_1(3) * TRIAX**2
              IF ((NUVAR == 3).AND.(NFUNC > 0)) EPS_FAIL = EPS_FAIL* UVAR(I,3)
              IF ((NUVAR == 9).AND.(NFUNC > 0)) EPS_FAIL = EPS_FAIL* UVAR(I,9)
           ELSE
              SELECT CASE (SEL)
               CASE(1)
                 EPS_FAIL   =   X_2(1) +  X_2(2) * TRIAX + X_2(3) * TRIAX**2
                 IF ((NUVAR == 3).AND.(NFUNC > 0))EPS_FAIL = EPS_FAIL * UVAR(I,3)
                 IF ((NUVAR == 9).AND.(NFUNC > 0))EPS_FAIL = EPS_FAIL * UVAR(I,9)
               CASE(2)
                 IF (TRIAX <= ONE/SQR3) THEN                     ! triax < 0.57735
                   P1X      = THIRD
                   P1Y      = X_1(1) + X_1(2) * P1X + X_1(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * TRIAX + A1 * TRIAX**2
                   IF ((NUVAR == 3).AND.(NFUNC > 0))EPS_FAIL = EPS_FAIL * UVAR(I,3)
                   IF ((NUVAR == 9).AND.(NFUNC > 0))EPS_FAIL = EPS_FAIL * UVAR(I,9)
                 ELSE                                             ! triax > 0.57735
                   P1X      = TWO * THIRD
                   P1Y      = X_2(1) + X_2(2) * P1X + X_2(3) * P1X**2
                   S1X      = ONE/SQR3
                   S1Y      = X_2(1) + X_2(2) / SQR3  + X_2(3) * (ONE/SQR3)**2
                   A1       = (P1Y - S1Y) / (P1X - S1X)**2
                   B1       = -TWO * A1 * S1X
                   C1       = A1 * S1X**2 + S1Y 
                   EPS_FAIL = C1 + B1 * TRIAX + A1 * TRIAX**2
                   
                   IF ((NUVAR == 3).AND.(NFUNC> 0)) EPS_FAIL = EPS_FAIL * UVAR(I,3)
                   IF ((NUVAR == 9).AND.(NFUNC> 0)) EPS_FAIL = EPS_FAIL * UVAR(I,9)
                 ENDIF
               END SELECT
           ENDIF

           DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPS_FAIL,EM6)
           DFMAX(I) = MIN(ONE,DFMAX(I))

           IF (DFMAX(I) >= ONE) THEN ! total damage in integration point
             NINDX = NINDX + 1
             INDX(NINDX) = I  
             TDEL(I)     = TIME
             DFMAX(I)    = ONE
             OFF(I)      = FOUR_OVER_5
           ENDIF
        ENDIF 
      ENDDO      
c------------------------
      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)            
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1000) NGL(I),TIME
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDX             
c------------------
 1000 FORMAT(5X,'FAILURE (BIQUAD) OF BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
