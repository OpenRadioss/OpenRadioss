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
Chd|  FAIL_TENSSTRAIN_IB                source/materials/fail/energy/fail_tensstrain_ib.F
Chd|-- called by -----------
Chd|        FAIL_BEAM18                   source/elements/beam/fail_beam18.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_TENSSTRAIN_IB(
     .           NEL      ,NGL      ,NUPARAM  ,UPARAM   ,
     .           TIME     ,EPSD     ,OFF      ,DFMAX    ,       
     .           TDEL     ,IOUT     ,ISTDO    ,IFUNC    ,
     .           EPSXX    ,AL       ,TSTAR    ,DAMSCL   ,
     .           SNPC     ,NPF      , STF     , UVAR  ,NVARF,
     .           TF       ,IPT      ,FOFF   ,ISMSTR)
C-----------------------------------------------
c    tens strain failure model for integrated beams (TYPE 18) !irup=10
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
      INTEGER                     ,INTENT(IN)    :: ISMSTR
      INTEGER                     ,INTENT(IN)    :: NVARF   
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      INTEGER ,DIMENSION(100)     ,INTENT(IN)    :: IFUNC   ! table of functions identifiers
      INTEGER ,DIMENSION(SNPC)    ,INTENT(IN)    :: NPF
      INTEGER ,DIMENSION(NEL)     ,INTENT(INOUT) :: FOFF
      my_real                     ,INTENT(IN)    :: TIME    ! current time    
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSD    ! strain rate
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: TSTAR
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSXX 
      my_real ,DIMENSION(STF)     ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element desactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DAMSCL  !1-damage
      my_real ,DIMENSION(NEL,NVARF),INTENT(INOUT) :: UVAR   
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDXD,NINDXF,NINDX1,NINDX2,S_FLAG,SCALE_FLAG,STRDEF,STRFLAG
      INTEGER ,DIMENSION(NEL) :: INDXD,INDXF,INDX1,INDX2
      my_real R1,R2,EPSF1,EPSF2,LAMBDA,FAC,DF,
     .        EL_REF,SC_EL,SCALE_TEMP,RFAC,RFAC2,
     .        UNIT_T,EPSP_UNIT,FINTER,DYDX,EPST1,EPST2
      EXTERNAL FINTER
      my_real ,DIMENSION(NEL) :: EPS_MAX,DAMAGE,EPSRATE,EPS11,EPSI,EPS_EQ
C=======================================================================
      EPST1   = UPARAM(1)
      EPST2   = UPARAM(2)
      EPSF1   = UPARAM(3)            
      EPSF2   = UPARAM(4)
      SC_EL      = UPARAM(5)
      EL_REF     = UPARAM(6)
      SCALE_TEMP = UPARAM(7)
      S_FLAG     = INT(UPARAM(8))
      UNIT_T     = UPARAM(9)
      STRDEF     = INT(UPARAM(10))
c
      DAMAGE(:NEL)  = ZERO
      EPS_MAX(:NEL) = ZERO
      NINDXD  = 0  
      NINDXF  = 0  
      STRFLAG = 0 
C-----------------------------------------------
c     Initialization
C-----------------------------------------------
      IF (UVAR(1,1) == ZERO) THEN 
        SELECT CASE (S_FLAG)
          CASE (2)
            IF (IFUNC(2) > 0)THEN
              DO I=1,NEL
                LAMBDA    = AL(I)  / EL_REF
                FAC       = SC_EL * FINTER(IFUNC(2),LAMBDA,NPF,TF,DF)
                UVAR(I,1) = FAC 
              ENDDO
            ELSE
              DO I=1,NEL
                UVAR(I,1) = ONE
              ENDDO
            ENDIF              
          CASE (3)
            IF (IFUNC(2) > 0)THEN
              DO I=1,NEL
                LAMBDA    = AL(I)  / EL_REF
                FAC       = SC_EL * FINTER(IFUNC(2),LAMBDA,NPF,TF,DF)
                UVAR(I,1) = FAC
              ENDDO
            ELSE
              DO I=1,NEL
                UVAR(I,1) = ONE
              ENDDO
            ENDIF 
          CASE DEFAULT  ! default - old formulation
            DO I=1,NEL
              UVAR(I,1) = EPSF1             
            ENDDO
        END SELECT
      ENDIF
c----------------------------------------------
c     Max strain transformation following input definition
c-------------------     
      IF (STRDEF == 2 .AND. (ISMSTR == 0 .OR. ISMSTR == 4) ) THEN ! failure defined as engineering strain
c       transform true strain to engineering         
        DO I=1,NEL
          EPS11(I)   = EXP(EPSXX(I)) - ONE
          EPSRATE(I) = EXP(EPSD(I))  - ONE
        ENDDO        
c
      ELSE IF (STRDEF == 3 .AND. ISMSTR == 1) THEN   ! failure defined as true strain
c       transform engineering to true strain    
        DO I=1,NEL
          EPS11(I)   = LOG(EPSXX(I) + ONE)
          EPSRATE(I) = LOG(EPSD(I) + ONE)
        ENDDO        
      ELSE
        DO I=1,NEL
          EPS11(I)   = EPSXX(I) 
          EPSRATE(I) = EPSD(I) 
        ENDDO               
      END IF           

c-------------------------
      SELECT CASE (S_FLAG)
c-------------------------
        CASE (1)  ! Equivalent strain criterion  AND PRINCIPAL
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS_EQ(I)  = ABS(EPS11(I))
              EPSI(I)    = EPS11(I)
            END IF
          ENDDO
        CASE (2)  ! Equivalent strain criterion  
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS_EQ(I)  = ABS(EPS11(I))
            END IF
          ENDDO
        CASE (3)  ! Equivalent strain criterion  AND PRINCIPAL
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPSI(I)    = EPS11(I)
            END IF
          ENDDO
      END SELECT
          
          
c-------------------
      NINDXD = 0  
      NINDXF = 0  
      NINDX1 = 0  
      NINDX2 = 0  
c
      SELECT CASE (S_FLAG)
        CASE (2)
C-------------------
c     Equivalent strain. New format: Element length and temperature
C-------------------
          DO I=1,NEL
            IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
              IF (IFUNC(1) > 0) THEN   ! strain rate
                EPSP_UNIT = EPSRATE(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ELSE
                RFAC = ONE
              ENDIF  
              IF (IFUNC(3) > 0) THEN   ! temperatuer
                RFAC2 = FINTER(IFUNC(3),TSTAR(I),NPF,TF,DYDX)
                RFAC2 = MAX(RFAC2,EM20)
              ELSE
                RFAC2 = ONE
              ENDIF  
              R1 = EPST1*RFAC*RFAC2*UVAR(I,1)
              R2 = EPST2*RFAC*RFAC2*UVAR(I,1)  
              IF (EPS_EQ(I) > R1) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1  
                  INDXD(NINDXD) = I
                ENDIF   
                DAMAGE(I) = MIN(ONE, (EPS_EQ(I)-R1)/(R2-R1))
                DFMAX(I)  = MAX(DFMAX(I) ,DAMAGE(I))
              ENDIF
              IF (EPS_EQ(I) >= R2) THEN
                FOFF(I) = 0
                TDEL(I) = TIME 
                NINDXF  = NINDXF + 1  
                INDXF(NINDXF) = I
                DFMAX(I)    = ONE
              ENDIF 
            ENDIF          
          ENDDO
C-------------------
        CASE (3)
C-------------------
c     principal strain. with Element length and temperature
C-------------------
          DO I=1,NEL
            IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
              IF (IFUNC(1) > 0) THEN   ! strain rate
                EPSP_UNIT = EPSRATE(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ELSE
                RFAC = ONE
              ENDIF  
              IF (IFUNC(3) > 0) THEN   ! temperatuer
                RFAC2 = FINTER(IFUNC(3),TSTAR(I),NPF,TF,DYDX)
                RFAC2 = MAX(RFAC2,EM20)
              ELSE
                RFAC2 = ONE
              ENDIF  
              R1 = EPST1*RFAC*RFAC2*UVAR(I,1)
              R2 = EPST2*RFAC*RFAC2*UVAR(I,1)  
              IF (EPSI(I) > R1) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1  
                  INDXD(NINDXD) = I
                ENDIF   
                DAMAGE(I) = MIN(ONE, (EPSI(I)-R1)/(R2-R1))
                DFMAX(I)  = MAX(DFMAX(I) ,DAMAGE(I))
              ENDIF
              IF (EPSI(I) >= R2) THEN
                FOFF(I) = 0
                TDEL(I) = TIME 
                NINDXF  = NINDXF + 1  
                INDXF(NINDXF) = I
                DFMAX(I)    = ONE
              ENDIF 
            ENDIF          
          ENDDO

C-------------------
c     Equivalent strain.  
C-------------------
        CASE DEFAULT  
          DO I=1,NEL
            IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
              IF (IFUNC(1) > 0) THEN   ! strain rate
                EPSP_UNIT = EPSRATE(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ELSE
                RFAC = ONE
              ENDIF  
              R1 = EPST1*RFAC
              R2 = EPST2*RFAC 
              IF (EPS_EQ(I) > R1) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1  
                  INDXD(NINDXD) = I
                ENDIF   
                DAMAGE(I) = MIN(ONE, (EPS_EQ(I)-R1)/(R2-R1))
                DFMAX(I)     = MAX(DFMAX(I), DAMAGE(I))
             ENDIF
              IF (EPS_EQ(I) >= R2) THEN
                FOFF(I) = 0
                TDEL(I) = TIME 
                NINDXF  = NINDXF + 1  
                INDXF(NINDXF) = I
                DFMAX(I)      = ONE
              ENDIF 
              IF (EPSF1 > ZERO .OR. EPSF2 > ZERO) THEN               
                IF ( EPS_EQ(I) > UVAR(I,1) .AND. UVAR(I,1) /= ZERO) THEN
                  FOFF(I) = 0
                  TDEL(I) = TIME 
                  NINDX1  = NINDX1 + 1  
                  INDX1(NINDX1) = I
                ELSEIF  (EPSI(I) > EPSF2 .AND. EPSF2 > ZERO) THEN
                  FOFF(I) = 0
                  TDEL(I) = TIME 
                  NINDX2  = NINDX2 + 1  
                  INDX2(NINDX2) = I
                  EPS_EQ(I) = EPSI(I)
                ENDIF
                DFMAX(I)   = MAX(DFMAX(I),MIN( ONE, (EPS_EQ(I) / UVAR(I,1))))
                SCALE_FLAG = 0
              ENDIF
            ENDIF
c                       
          ENDDO
      END SELECT

C-------------------
      DO I=1,NEL
        DAMSCL(I) = ONE - DFMAX(I)
      END DO
           
         
c------------------------
c     print
c------------------------
      IF (NINDXD > 0) THEN    ! start damage
        DO J=1,NINDXD    
          I = INDXD(J)
          IF (S_FLAG == 3) THEN
#include  "lockon.inc"
            WRITE(IOUT, 2001) NGL(I),IPT,EPSI(I)
            WRITE(ISTDO,2001) NGL(I),IPT,EPSI(I)
#include  "lockoff.inc"
          ELSE
#include  "lockon.inc"
            WRITE(IOUT, 2002) NGL(I),IPT,EPS_EQ(I)
            WRITE(ISTDO,2002) NGL(I),IPT,EPS_EQ(I)
#include  "lockoff.inc"
          ENDIF
        END DO
      END IF              

c
      IF (NINDXF > 0) THEN    ! failure
        DO J=1,NINDXF    
          I = INDXF(J)
          IF (S_FLAG == 3) THEN
#include  "lockon.inc"
            WRITE(IOUT, 3001) NGL(I),IPT,EPSI(I)
            WRITE(ISTDO,3101) NGL(I),IPT,EPS_EQ(I)
#include  "lockoff.inc"
          ELSE
            WRITE(IOUT, 2002) NGL(I),IPT,EPS_EQ(I)
            WRITE(ISTDO,2002) NGL(I),IPT,EPS_EQ(I)
          ENDIF
        END DO
      END IF              
c
      IF (NINDX1 > 0) THEN 
        DO J=1,NINDX1    
          I = INDX1(J)
#include  "lockon.inc"
          WRITE(IOUT, 4000) NGL(I),IPT,EPS_EQ(I),TIME
          WRITE(ISTDO,4100) NGL(I),IPT,EPS_EQ(I),TIME
#include  "lockoff.inc"
        END DO
      END IF              
c
      IF (NINDX2 > 0) THEN 
        DO J=1,NINDX2    
          I = INDX2(J)
#include  "lockon.inc"
          WRITE(IOUT, 5000) NGL(I),IPT,EPS_EQ(I),TIME
          WRITE(ISTDO,5100) NGL(I),IPT,EPS_EQ(I),TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
             
c------------------
 2001 FORMAT(1X,'START DAMAGE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'PRINCIPAL STRAIN=',G11.4)
 2002 FORMAT(1X,'START DAMAGE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'EQUIVALENT STRAIN=',G11.4)
C
 3001 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'PRINCIPAL STRAIN=',G11.4)
 3101 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'EQUIVALENT STRAIN=',G11.4)
C
 4000 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'EQUIVALENT STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
 4100 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'EQUIVALENT STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
C
 5000 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'PRINCIPAL STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
 5100 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'PRINCIPAL STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
