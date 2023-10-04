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
Chd|  FAIL_COCKROFT_B               source/materials/fail/cockroft_latham/fail_cockroft_b.F
Chd|-- called by -----------
Chd|        FAIL_BEAM3                    source/elements/beam/fail_beam3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_COCKROFT_B(
     .           NEL      ,NGL      ,NUPARAM  ,UPARAM   ,
     .           TIME     ,DPLA     ,OFF      ,DFMAX    ,       
     .           TDEL     ,IOUT     ,ISTDO    , EPSXX    ,
     .           F1      , AREA , 
     .           NUVAR   , UVAR )
C-----------------------------------------------
c    cockcroft  failure model for integrated beams (TYPE 18) !irup=34
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
      INTEGER                     ,INTENT(IN)    :: NUVAR
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array    
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSXX
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: DPLA    !plastic strain increment
      my_real                     ,INTENT(IN)    :: AREA    !
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element desactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: F1      ! force direction 1
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
      my_real ,DIMENSION(NEL,NUVAR),INTENT(INOUT) :: UVAR   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDXF
      INTEGER ,DIMENSION(NEL) :: INDXF
      my_real
     .                            C0,EMA,EEQ,SIG_A,SIG_FILTRE
      my_real ,DIMENSION(NEL) ::  EPS_EQ,DAMAGE,EPSRATE,EPS11,EPSI,D_EEQ
C=======================================================================
C     UVAR(I,1) contains previous equivalent strain value increment
C     UVAR(I,2) contains the Cockroft-Latham accumulated value
c     UVAR(I,3) contains the previous first principal stress
c     UVAR(I,4) contains total strain
      C0                  = UPARAM(1)
      EMA                 = UPARAM(2)
      NINDXF              = 0       
      DO I =1,NEL
        UVAR(I,4) =  UVAR(I,4) + EPSXX(I)              
      ENDDO                              
C-----------------------------------------------
      IF(C0 < ZERO)THEN   ! equivalent strain 
        DO I =1,NEL
          IF(OFF(I) >= ONE  ) THEN 
            EEQ   = ABS(EPSXX(I))         
            D_EEQ(I) = EEQ - UVAR(I,1)
            IF (D_EEQ(I) <= ZERO) D_EEQ(I) = ZERO
            UVAR(I,1) = EEQ             
          ENDIF
        ENDDO
      ELSE  ! positive = plastic strain increment
        DO I =1,NEL
          IF(OFF(I) >= ONE  ) THEN 
            D_EEQ(I) = DPLA(I)
            UVAR(I,1) = UVAR(I,1) + DPLA(I)
          ENDIF
        ENDDO
      ENDIF


      DO I =1,NEL
        IF(OFF(I) >= ONE  ) THEN
          ! principal stress Sigma 
          SIG_A        = F1(I) / AREA 
          SIG_FILTRE   = SIG_A * EMA + (ONE-EMA)* UVAR(I,3)

          UVAR(I,3)= SIG_FILTRE
          !integral
          UVAR(I,2) = UVAR(I,2) + MAX(SIG_FILTRE,ZERO) * D_EEQ(I)
          !damage
          DFMAX(I) =  MIN(UVAR(I,2) / MAX(EM20,ABS(C0))  ,ONE)
          IF (DFMAX(I) >= ONE) THEN
            DFMAX(I)    = ONE
            OFF(I)  = FOUR_OVER_5
            TDEL(I) = TIME 
            NINDXF  = NINDXF + 1  
            INDXF(NINDXF) = I
          ENDIF 
        ENDIF
      ENDDO
C!
      DO J=1,NINDXF
         I = INDXF(J)
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),TIME
          WRITE(ISTDO,2000) NGL(I),TIME
#include "lockoff.inc" 

       ENDDO
c------------------
 2000 FORMAT(1X,'FAILURE (COCKROFT-LATHAM) OF BEAM ELEMENT ',I10,1X,',AT TIME : ',1PE12.4)

c------------------

       RETURN
       END
