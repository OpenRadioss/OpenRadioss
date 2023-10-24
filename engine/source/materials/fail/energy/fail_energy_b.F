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
Chd|  FAIL_ENERGY_B                 source/materials/fail/energy/fail_energy_b.F
Chd|-- called by -----------
Chd|        FAIL_BEAM3                    source/elements/beam/fail_beam3.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_ENERGY_B(
     .           NEL      ,NGL      ,NUPARAM  ,UPARAM   ,
     .           TIME     ,EPSD     ,OFF      ,DFMAX    ,       
     .           TDEL     ,IOUT     ,ISTDO    ,IFUNC    ,
     .           AREA     ,F1       ,
     .           F2       ,F3       ,M1       ,M2       ,
     .           M3       ,SNPC     ,NPF      ,STF      , 
     .           TF       ,EPSXX    ,EPSXY    ,EPSXZ    ,
     .           KXX      ,KYY      ,KZZ      ,NVAR     ,
     .           UVAR     )
C-----------------------------------------------
c    energy failure model for standard beams (TYPE 3)
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
      INTEGER                     ,INTENT(IN)    :: SNPC 
      INTEGER                     ,INTENT(IN)    :: STF       
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      INTEGER ,DIMENSION(100)     ,INTENT(IN)    :: IFUNC   ! table of functions identifiers
      INTEGER ,DIMENSION(SNPC)    ,INTENT(IN)    :: NPF
      INTEGER                     ,INTENT(IN)    :: NVAR
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real                     ,INTENT(IN)    :: AREA
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSD    ! strain rate
      my_real ,DIMENSION(STF)     ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element desactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: F1,F2,F3
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: M1,M2,M3
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSXX,EPSXY,EPSXZ
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: KXX,KYY,KZZ
      my_real ,DIMENSION(NEL,NVAR),INTENT(INOUT) :: UVAR    ! User variable
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: RFAC,R1,R2,IE_SP,DYDX,RIEF1,RIEF2,XFAC,FINTER,FACT
      EXTERNAL FINTER
      my_real ,DIMENSION(NEL) :: DAMAGE
C=======================================================================
      NINDX = 0  
      RIEF1    = UPARAM(1)             
      RIEF2    = UPARAM(2)           
      XFAC     = UPARAM(4)           
      DO I=1,NEL
        DAMAGE(I) = ZERO
        UVAR(I,1) = UVAR(I,1) + (
     .              F1(I)*EPSXX(I) + F2(I)*EPSXY(I) + F3(I)*EPSXZ(I) + 
     .              M1(I)*KXX(I)   + M2(I)*KYY(I)   + M3(I)*KZZ(I))
      ENDDO
c-----------------------------
      DO I=1,NEL
        IF (OFF(I) == ONE ) THEN
          IF (IFUNC(1) /= 0) THEN
            RFAC = FINTER(IFUNC(1),EPSD(I)*XFAC,NPF,TF,DYDX)
            RFAC = MAX(RFAC,EM20)
          ELSE
            RFAC = ONE
          ENDIF  
          R1 = RIEF1*RFAC
          R2 = RIEF2*RFAC
          IE_SP = UVAR(I,1)/AREA

          IF (IE_SP > R1) THEN
            DAMAGE(I) = (IE_SP-R1)/(R2-R1)
            DAMAGE(I) = MIN(ONE,DAMAGE(I))
            DFMAX(I)  = MAX(DFMAX(I) ,DAMAGE(I)) 
          ENDIF

          IF (IE_SP > R2) THEN
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
 1000 FORMAT(5X,'FAILURE (ENERGY) OF BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
