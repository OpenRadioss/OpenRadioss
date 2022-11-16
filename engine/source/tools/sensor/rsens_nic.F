Chd|====================================================================
Chd|  RSENS_NIC                     source/tools/sensor/rsens_nic.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
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
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(INOUT) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,SPRING_ID,ISENS,SPR_SKW,SEN_SKW,IDIR_F,IDIR_M
      my_real :: FZ,MY,FTENS,FCOMP,MFLEX,MEXT,FGX,FGY,FGZ,MGX,MGY,MGZ
      my_real ,DIMENSION(3) :: FF,MM
      my_real ,DIMENSION(LSKEW,NUMSKW) :: SKEW
c-----------------------------------------------------------------------
c     send current normalized forces and moments in the spring to sensor for processing
c=======================================================================
      ISENS = 0
      DO J = 1,NSENSOR
        IF (SENSOR_TAB(J)%TYPE == 19) THEN  ! sensor NIC_Nij
          ISENS = J
          SEN_SKW   = SENSOR_TAB(ISENS)%IPARAM(3)
          SPR_SKW = SENSOR_TAB(ISENS)%IPARAM(4)
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
c     we assume there's only one NIC-Nij sensor in the model
      IF (ISENS > 0) THEN   
        DO I = 1,NEL
          SPRING_ID = IXR(6,I)
          IF (SENSOR_TAB(ISENS)%IPARAM(1) == SPRING_ID) THEN
            FF(1) = FOR(I)
            FF(2) = FOR(I+NEL)
            FF(3) = FOR(I+NEL*2)
            MM(1) = MOM(I)
            MM(2) = MOM(I+NEL)
            MM(3) = MOM(I+NEL*2)
c           rotate to sensor skew if necessary
            IF (SEN_SKW > 1) THEN
              IF (SEN_SKW /= SPR_SKW) THEN
                FGX   = SKEW(1,SPR_SKW)*FF(1)+SKEW(4,SPR_SKW)*FF(2)+SKEW(7,SPR_SKW)*FF(3)
                FGY   = SKEW(2,SPR_SKW)*FF(1)+SKEW(5,SPR_SKW)*FF(2)+SKEW(8,SPR_SKW)*FF(3)
                FGZ   = SKEW(3,SPR_SKW)*FF(1)+SKEW(6,SPR_SKW)*FF(2)+SKEW(9,SPR_SKW)*FF(3)
                FF(1) = SKEW(1,SEN_SKW)*FGX+SKEW(2,SEN_SKW)*FGY+SKEW(3,SEN_SKW)*FGZ
                FF(2) = SKEW(4,SEN_SKW)*FGX+SKEW(5,SEN_SKW)*FGY+SKEW(6,SEN_SKW)*FGZ
                FF(3) = SKEW(7,SEN_SKW)*FGX+SKEW(8,SEN_SKW)*FGY+SKEW(9,SEN_SKW)*FGZ
                MGX   = SKEW(1,SPR_SKW)*MM(1)+SKEW(4,SPR_SKW)*MM(2)+SKEW(7,SPR_SKW)*MM(3)
                MGY   = SKEW(2,SPR_SKW)*MM(1)+SKEW(5,SPR_SKW)*MM(2)+SKEW(8,SPR_SKW)*MM(3)
                MGZ   = SKEW(3,SPR_SKW)*MM(1)+SKEW(6,SPR_SKW)*MM(2)+SKEW(9,SPR_SKW)*MM(3)
                MM(1) = SKEW(1,SEN_SKW)*MGX+SKEW(2,SEN_SKW)*MGY+SKEW(3,SEN_SKW)*MGZ
                MM(2) = SKEW(4,SEN_SKW)*MGX+SKEW(5,SEN_SKW)*MGY+SKEW(6,SEN_SKW)*MGZ
                MM(3) = SKEW(7,SEN_SKW)*MGX+SKEW(8,SEN_SKW)*MGY+SKEW(9,SEN_SKW)*MGZ
              END IF
              FZ = FF(IDIR_F)
              MY = MM(IDIR_M)
            END IF
c           save normalized FZ and My values
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
            SENSOR_TAB(ISENS)%VAR(3) = ONE ! spmd flag for sensor indicating present domain
            EXIT
          END IF
        END DO
      END IF
c-----------
      RETURN
      END
