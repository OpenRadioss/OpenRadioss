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
Chd|  FAIL_ENERGY_IB                source/materials/fail/energy/fail_energy_ib.F
Chd|-- called by -----------
Chd|        FAIL_BEAM18                   source/elements/beam/fail_beam18.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_ENERGY_IB(
     .           NEL      ,NGL      ,NUPARAM  ,UPARAM   ,
     .           TIME     ,EPSD     ,OFF      ,DFMAX    ,       
     .           TDEL     ,IOUT     ,ISTDO    ,IFUNC    ,
     .           EINT     ,AREA     ,AL       , DAMSCL  ,
     .           SNPC     ,NPF      ,STF      , 
     .           TF       ,IPT      ,FOFF)
C-----------------------------------------------
c    energy failure model for standard beams (TYPE 18)
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
      INTEGER                     ,INTENT(IN)    :: IPT     ! current integration point
      INTEGER                     ,INTENT(IN)    :: IOUT    ! output file unit
      INTEGER                     ,INTENT(IN)    :: ISTDO   ! output file unit
      INTEGER                     ,INTENT(IN)    :: SNPC 
      INTEGER                     ,INTENT(IN)    :: STF       
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      INTEGER ,DIMENSION(100)     ,INTENT(IN)    :: IFUNC   ! table of functions identifiers
      INTEGER ,DIMENSION(SNPC)    ,INTENT(IN)    :: NPF
      INTEGER ,DIMENSION(NEL)     ,INTENT(INOUT) :: FOFF    ! integration point desactivation flag
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real                     ,INTENT(IN)    :: AREA    ! AREA  of beam
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSD    ! strain rate
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL,2)   ,INTENT(INOUT) :: EINT    !internal energy
      my_real ,DIMENSION(STF)     ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element desactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DAMSCL  !1-damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,NINDXD
      INTEGER ,DIMENSION(NEL) :: INDX,INDXD
      my_real :: RFAC,R1,R2,DYDX,RIEF1,RIEF2,XFAC,FINTER
      EXTERNAL FINTER
      my_real ,DIMENSION(NEL) :: DAMAGE,VOL,IE_SP
C=======================================================================
      NINDX = 0
      NINDXD  = 0
      RIEF1    = UPARAM(1)             
      RIEF2    = UPARAM(2)           
      XFAC     = UPARAM(4)           
      DO I=1,NEL
        DAMAGE(I) = ZERO
      ENDDO
c-----------------------------
      DO I=1,NEL
        IF (OFF(I) == ONE .AND. FOFF(I) == 1) THEN
          IF (IFUNC(1) /= 0) THEN
            RFAC = FINTER(IFUNC(1),EPSD(I)*XFAC,NPF,TF,DYDX)
            RFAC = MAX(RFAC,EM20)
          ELSE
            RFAC = ONE
          ENDIF  
          R1 = RIEF1*RFAC
          R2 = RIEF2*RFAC
          VOL(I) = AREA*AL(I)
          IE_SP(I) = (EINT(I,1) + EINT(I,2))/VOL(I)
          IF (IE_SP(I) > R1) THEN
            DAMAGE(I) = (IE_SP(I)-R1)/(R2-R1)
            DAMAGE(I) = MIN(ONE,DAMAGE(I))
            IF (DFMAX(I) == ZERO) THEN
              NINDXD = NINDXD + 1  
              INDXD(NINDXD) = I
            ENDIF  
           DFMAX(I)  = MAX(DFMAX(I) ,DAMAGE(I))
           DAMSCL(I) = ONE - DAMAGE(I)
          ENDIF

          IF (IE_SP(I) > R2 .OR. DAMSCL(I) < EM12) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I  
            TDEL(I)     = TIME
            DFMAX(I)    = ONE
            DAMSCL(I)   = ZERO
            FOFF(I)     = 0
          ENDIF
        ENDIF 
      ENDDO         
c------------------------
      IF (NINDXD > 0) THEN        
        DO J=1,NINDXD             
          I = INDXD(J)            
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPT,TIME,IE_SP(I)
          WRITE(ISTDO,2000) NGL(I),IPT,TIME,IE_SP(I)
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDXD             

      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)            
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),IPT,TIME,IE_SP(I)
          WRITE(ISTDO,1000) NGL(I),IPT,TIME,IE_SP(I)
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDX             
c------------------
 1000 FORMAT(5X,'FAILURE (ENERGY) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I5
     .      ,2X,'AT TIME :',1PE12.4,1X,' , ENERGY PER VOLUME : ',1PE12.4 )
 2000 FORMAT(5X,'START DAMAGE (ENERGY) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I5
     .      ,2X,'AT TIME :',1PE12.4,1X,' , ENERGY PER VOLUME : ',1PE12.4 )     
c------------------
      RETURN
      END
