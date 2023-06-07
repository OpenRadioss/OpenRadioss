Chd|====================================================================
Chd|  PRESS_SEG3                    source/loads/general/load_pcyl/press_seg3.F
Chd|-- called by -----------
Chd|        PRESSURE_CYL                  source/loads/general/load_pcyl/pressure_cyl.F
Chd|-- calls ---------------
Chd|        TABLE_INTERP_DYDX             source/tools/curve/table_tools.F
Chd|        PLOAD_CYL_MOD                 ../common_source/modules/loads/pload_cyl_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE PRESS_SEG3(A     ,B     ,C     ,N1    ,DIR    ,
     .                      IFUNC ,TABLE ,XFACR ,XFACT ,PRESS  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE PLOAD_CYL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IFUNC
      my_real ,INTENT(IN) :: XFACR,XFACT
      my_real ,DIMENSION(3) ,INTENT(IN)  :: A,B,C,N1,DIR
      TYPE (TTABLE), DIMENSION(NTABLE) ,INTENT(IN)       :: TABLE
      my_real ,INTENT(OUT) :: PRESS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NPT
      my_real :: A11,A12,A21,A22,B1,B2,DET,ALPHA,BETA,GAMMA,F1,F2,R,S,
     .   R1,R2,R3,RMAX,P1,P2,P3,DYDX,NX,NY,NZ,NORM,COSP,SINP
      my_real, DIMENSION(3) :: AB,AC,AN,P,AP,BP,CP
      my_real, DIMENSION(2) :: XX
c-----------------------------------------------
c     1) find intersection point P between plane surface A-B-C and line N1-N2
c     2) calculate radial coordinates of each segment node in cylindrical
c        coord system defined by direction axis
c        (distance between point P and each segment node projected
c        on plane perpendicular to axis)
c=======================================================================
      AB(1) = B(1)  - A(1)
      AB(2) = B(2)  - A(2)
      AB(3) = B(3)  - A(3)
      AC(1) = C(1)  - A(1)
      AC(2) = C(2)  - A(2)
      AC(3) = C(3)  - A(3)
      AN(1) = N1(1) - A(1)
      AN(2) = N1(2) - A(2)
      AN(3) = N1(3) - A(3)
      ALPHA = DIR(1)
      BETA  = DIR(2)
      GAMMA = DIR(3)
      NPT   = SIZE(TABLE(IFUNC)%X(1)%VALUES)
      RMAX  = TABLE(IFUNC)%X(1)%VALUES(NPT) / XFACR
      P1    = ZERO
      P2    = ZERO
      P3    = ZERO
c
      IF (ALPHA /= ZERO) THEN
        F1 = BETA  / ALPHA
        F2 = GAMMA / ALPHA
        B1  = AN(2) - F1 * AN(1)
        B2  = AN(3) - F2 * AN(1)
        A11 = AB(2) - F1 * AB(1)
        A12 = AC(2) - F1 * AC(1)
        A21 = AB(3) - F2 * AB(1)
        A22 = AC(3) - F2 * AC(1)
      ELSE IF (BETA /= ZERO) THEN
        F1  = ALPHA / BETA
        F2  = GAMMA / BETA
        B1  = AN(1) - F1 * AN(2)
        B2  = AN(3) - F2 * AN(2)
        A11 = AB(1) - F1 * AB(2)
        A12 = AC(1) - F1 * AC(2)
        A21 = AB(3) - F2 * AB(2)
        A22 = AC(3) - F2 * AC(2)
      ELSE IF (GAMMA /= ZERO) THEN
        F1  = ALPHA / GAMMA
        F2  = BETA  / GAMMA
        B1  = AN(1) - F1 * AN(3)
        B2  = AN(2) - F2 * AN(3)
        A11 = AB(1) - F1 * AB(3)
        A12 = AC(1) - F1 * AC(3)
        A21 = AB(2) - F2 * AB(3)
        A22 = AC(2) - F2 * AC(3)
      END IF
c
      DET = A11*A22 - A12*A21
      R   = (A22 * B1 - A12 * B2) / DET
      S   = (A11 * B2 - A21 * B1) / DET
      ! P = projection of axe origin N1 on ABC plane following direction DIR
      P(1)= A(1) + R*AB(1) + S*AC(1)
      P(2)= A(2) + R*AB(2) + S*AC(2)
      P(3)= A(3) + R*AB(3) + S*AC(3)
      AP(1) = P(1) - A(1)
      AP(2) = P(2) - A(2)
      AP(3) = P(3) - A(3)
      BP(1) = P(1) - B(1)
      BP(2) = P(2) - B(2)
      BP(3) = P(3) - B(3)
      CP(1) = P(1) - C(1)
      CP(2) = P(2) - C(2)
      CP(3) = P(3) - C(3)
      R1 = SQRT(AP(1)**2 + AP(2)**2 + AP(3)**2)
      R2 = SQRT(BP(1)**2 + BP(2)**2 + BP(3)**2)
      R3 = SQRT(CP(1)**2 + CP(2)**2 + CP(3)**2)
      COSP = (DIR(1) * AP(1) + DIR(2) * AP(2) + DIR(3) * AP(3)) / MAX(R1,EM20)
      SINP = MAX(SQRT(ONE - COSP**2), EM20)
c
      XX(2) = TT    ! Time
c     pressure in 1st point of 3N sub-segment
      COSP = (DIR(1) * AP(1) + DIR(2) * AP(2) + DIR(3) * AP(3)) / MAX(R1,EM20)
      SINP = MAX(SQRT(ONE - COSP**2), EM20)
      R1   = R1 * SINP
      IF (R1 <=  RMAX) THEN
        XX(1) = R1
        CALL TABLE_INTERP_DYDX(TABLE(IFUNC),XX,2,P1,DYDX)
      END IF
c     pressure in 2nd point of 3N sub-segment
      COSP = (DIR(1) * BP(1) + DIR(2) * BP(2) + DIR(3) * BP(3)) / MAX(R2,EM20)
      SINP = MAX(SQRT(ONE - COSP**2), EM20)
      R2   = R2 * SINP
      IF (R2 <= RMAX) THEN
        XX(1) = R2
        CALL TABLE_INTERP_DYDX(TABLE(IFUNC),XX,2,P2,DYDX)
      END IF
c     pressure in 3rd point of 3N sub-segment
      COSP = (DIR(1) * CP(1) + DIR(2) * CP(2) + DIR(3) * CP(3)) / MAX(R3,EM20)
      SINP = MAX(SQRT(ONE - COSP**2), EM20)
      R3   = R3 * SINP
      IF (R3 <= RMAX) THEN
        XX(1) = R3
        CALL TABLE_INTERP_DYDX(TABLE(IFUNC),XX,2,P3,DYDX)
      END IF
      PRESS = (P1 + P2 + P3) * THIRD
      IF (PRESS > ZERO) THEN
c       calculate cos(angle) between cylinder axis and segment normal to scale pressure
        NX = AB(2) * AC(3) - AB(3) *  AC(2)
        NY = AB(3) * AC(1) - AB(1) *  AC(3)
        NZ = AB(1) * AC(2) - AB(2) *  AC(1)
        NORM  = SQRT(NX**2 + NY**2 + NZ**2)
        COSP  = ABS(NX * ALPHA + NY * BETA + NZ * GAMMA) / NORM
        PRESS = PRESS * COSP
      END IF
c-----------      
      RETURN
      END
      
