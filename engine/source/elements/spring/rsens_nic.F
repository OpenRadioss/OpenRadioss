Chd|====================================================================
Chd|  RSENS_NIC                        source/elements/spring/rsens_nic.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE RSENS_NIC(NEL   ,IXR   ,FOR   ,MOM    ,SKEW     ,
     .                     SENSOR_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL
      INTEGER ,DIMENSION(NIXR,NUMELR) ,INTENT(IN) :: IXR
      my_real ,DIMENSION(NEL) ,INTENT(IN)  :: FOR,MOM
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(OUT) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,SPRING_ID,ISENS,ISKW_SPRING,ISKW_SENS,IDIR_F,IDIR_M
      my_real :: FX,FY,FZ,MX,MY,MZ, FTENS,FCOMP,MFLEX,MEXT
      my_real ,DIMENSION(3) :: FF,MM
      my_real ,DIMENSION(LSKEW,*) :: SKEW
c-----------------------------------------------------------------------
c     send current normalized forces and moments in the spring to sensor for processing
c=======================================================================
      ISENS = 0
      DO J = 1,NSENSOR
        IF (SENSOR_TAB(J)%TYPE == 19) THEN  ! sensor NIC_Nij
          ISENS = J
          ISKW_SENS   = SENSOR_TAB(ISENS)%IPARAM(3)
          ISKW_SPRING = SENSOR_TAB(ISENS)%IPARAM(4)
          IDIR_F = SENSOR_TAB(ISENS)%IPARAM(5) 
          IDIR_M = SENSOR_TAB(ISENS)%IPARAM(6) 
c
          FTENS  = SENSOR_TAB(ISENS)%RPARAM(2) 
          FCOMP  = SENSOR_TAB(ISENS)%RPARAM(3) 
          MFLEX  = SENSOR_TAB(ISENS)%RPARAM(4) 
          MEXT   = SENSOR_TAB(ISENS)%RPARAM(5) 
          EXIT
        END IF
      END DO
c     we assume theres only one NIC-Nij sensor in the model
      IF (ISENS > 0) THEN   
        DO I = 1,NEL
          SPRING_ID = IXR(5,I)
          IF (SENSOR_TAB(ISENS)%IPARAM(1) == SPRING_ID) THEN
            FF(1) = FOR(I)
            FF(2) = FOR(I+NEL)
            FF(3) = FOR(I+NEL*2)
            MM(1) = MOM(I)
            MM(2) = MOM(I+NEL)
            MM(3) = MOM(I+NEL*2)
c           rotate with sensor and spring skew if necessary



c           save normalized FZ and My values
            FZ = FF(IDIR_F)
            MY = MM(IDIR_M)
            IF (FZ >= ZERO) THEN
              FZ = FZ / FTENS
            ELSE 
              FZ = ABS(FZ) / FCOMP
            END IF
            IF (MY >= ZERO) THEN
              MY = MY / MFLEX
            ELSE 
              MY = ABS(MY) / MEXT
            END IF
            SENSOR_TAB(ISENS)%VAR(1) = FZ
            SENSOR_TAB(ISENS)%VAR(2) = MY
            EXIT
          END IF
        END DO
      END IF
c-----------
      RETURN
      END
