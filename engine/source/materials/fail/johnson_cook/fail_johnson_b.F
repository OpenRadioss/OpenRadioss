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
Chd|  FAIL_JOHNSON_B                source/materials/fail/johnson_cook/fail_johnson_b.F
Chd|-- called by -----------
Chd|        FAIL_BEAM3                    source/elements/beam/fail_beam3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_JOHNSON_B(
     .           NEL      ,NGL      ,NUPARAM  ,UPARAM   ,
     .           TIME     ,TSTAR    ,SVM      ,PRESSURE ,
     .           DPLA     ,EPSD     ,OFF      ,DFMAX    ,
     .           TDEL     ,IOUT     ,ISTDO    )
C-----------------------------------------------
c    Johnson-Cook failure model for standard beams (TYPE 3)
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
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: DPLA    ! plastic strain increment
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: SVM     ! Von Mises stress
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: PRESSURE! element pressure
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSD    ! strain rate
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: TSTAR   ! normalized temperature
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element desactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: D1,D2,D3,D4,D5,EPSD0,EPSF
C=======================================================================
      NINDX = 0  
      D1    = UPARAM(1)
      D2    = UPARAM(2)
      D3    = UPARAM(3)
      D4    = UPARAM(4)
      D5    = UPARAM(5)
      EPSD0 = UPARAM(6)
c-----------------------------
      DO I=1,NEL
        IF (OFF(I) == ONE .and. DPLA(I) > ZERO) THEN
          EPSF = D3 * PRESSURE(I) / MAX(EM20, SVM(I))
          EPSF = D1 + D2 * EXP(EPSF)
          IF (D4 /= ZERO)  EPSF = EPSF * (ONE + D4*LOG(MAX(ONE,EPSD(I)/EPSD0)))
          IF (D5 /= ZERO)  EPSF = EPSF * (ONE + D5*TSTAR(I))                  
          IF (EPSF > ZERO) DFMAX(I) = DFMAX(I) + DPLA(I) / EPSF  
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
 1000 FORMAT(5X,'FAILURE (JC) OF BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
