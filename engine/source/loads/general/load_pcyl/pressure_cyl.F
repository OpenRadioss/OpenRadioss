Chd|====================================================================
Chd|  PRESSURE_CYL                  source/loads/general/load_pcyl/pressure_cyl.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PRESS_SEG3                    source/loads/general/load_pcyl/press_seg3.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE PRESSURE_CYL(
     .           LOADS     ,TABLE     ,NSENSOR   ,SENSOR_TAB,IFRAME    ,
     .           DT1       ,X         ,V         ,ACC       ,FEXT      ,
     .           H3D_DATA  ,CPTREAC   ,FTHREAC   ,NODREAC   ,FSKY      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE TABLE_MOD
      USE SENSOR_MOD
      USE LOADS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "impl1_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR,CPTREAC
      my_real ,INTENT(IN) :: DT1
      INTEGER ,DIMENSION(NUMNOD)   ,INTENT(IN)    :: NODREAC
      INTEGER ,DIMENSION(LISKN,*)  ,INTENT(IN)    :: IFRAME
      my_real ,DIMENSION(8,LSKY)   ,INTENT(INOUT) :: FSKY
      my_real ,DIMENSION(3,NUMNOD) ,INTENT(IN)    :: X,V
      my_real ,DIMENSION(3,NUMNOD) ,INTENT(INOUT) :: ACC,FEXT
      my_real ,DIMENSION(6,CPTREAC),INTENT(INOUT) :: FTHREAC
      TYPE (TTABLE)      ,DIMENSION(NTABLE)  ,INTENT(IN) :: TABLE
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE (LOADS_)      ,INTENT(IN) :: LOADS
      TYPE (H3D_DATABASE),INTENT(IN) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IAD,IANIM,ISENS,IFUN,IFRA,N1,N2,N3,N4,M1,M2,
     .   NSEG,NDIM,NPOINT
      my_real :: A11,A12,A21,A22,B1,B2,DET,LEN,DIRX,DIRY,DIRZ,
     .   ALPHA,BETA,GAMMA,F1,F2,R,S,RMAX,XFACR,XFACT,YFAC,
     .   NX,NY,NZ,FX,FY,FZ,SEGP,PRESS,TFEXTT
      my_real, DIMENSION(3) :: P0,DIR,A,B,C,D,M
c=======================================================================
      TFEXTT = ZERO
      IANIM  = ANIM_V(5) + OUTP_V(5) + H3D_DATA%N_VECT_FINT
     .       + ANIM_V(6) + OUTP_V(6) + H3D_DATA%N_VECT_FEXT

c
      DO I = 1,LOADS%NLOAD_CYL
        ISENS = LOADS%LOAD_CYL(I)%ISENS
        IF (ISENS > 0) THEN
          IF (SENSOR_TAB(ISENS)%STATUS == 0) CYCLE
        END IF
c
        NSEG = LOADS%LOAD_CYL(I)%NSEG
        IFRA = LOADS%LOAD_CYL(I)%IFRAME + 1
        XFACR= LOADS%LOAD_CYL(I)%XSCALE_R
        XFACT= LOADS%LOAD_CYL(I)%XSCALE_T
        YFAC = LOADS%LOAD_CYL(I)%YSCALE
        IFUN = LOADS%LOAD_CYL(I)%ITABLE
        NDIM = TABLE(IFUN)%NDIM
        NPOINT = SIZE(TABLE(IFUN)%X(1)%VALUES)
        RMAX = TABLE(IFUN)%X(1)%VALUES(NPOINT)
        M1   = IFRAME(1,IFRA)
        M2   = IFRAME(2,IFRA)
        DIRX = X(1,M2) - X(1,M1)
        DIRY = X(2,M2) - X(2,M1)
        DIRZ = X(3,M2) - X(3,M1)
        LEN  = SQRT(DIRX**2 + DIRY**2 + DIRZ**2)
        ! SEGP beam axis
        DIR(1) = DIRX / LEN
        DIR(2) = DIRY / LEN
        DIR(3) = DIRZ / LEN
        P0(1)  = X(1,M1)
        P0(2)  = X(2,M1)
        P0(3)  = X(3,M1)
        DO J = 1,NSEG
          N1   = LOADS%LOAD_CYL(I)%SEGNOD(J,1)
          N2   = LOADS%LOAD_CYL(I)%SEGNOD(J,2)
          N3   = LOADS%LOAD_CYL(I)%SEGNOD(J,3)
          N4   = LOADS%LOAD_CYL(I)%SEGNOD(J,4)
          A(1) = X(1,N1)
          A(2) = X(2,N1)
          A(3) = X(3,N1)
          B(1) = X(1,N2)
          B(2) = X(2,N2)
          B(3) = X(3,N2)
          C(1) = X(1,N3)
          C(2) = X(2,N3)
          C(3) = X(3,N3)
c
          IF (N4 == 0) THEN    ! 3 node segment
            CALL PRESS_SEG3(A       ,B      ,C      ,P0      ,DIR    , 
     .                      IFUN    ,TABLE  ,XFACR  ,XFACT   ,YFAC   ,
     .                      SEGP)
            NX = (C(2)-A(2))*(C(3)-B(3)) - (C(3)-A(3))*(C(2)-B(2))
            NY = (C(3)-A(3))*(C(1)-B(1)) - (C(1)-A(1))*(C(3)-B(3))
            NZ = (C(1)-A(1))*(C(2)-B(2)) - (C(2)-A(2))*(C(1)-B(1))
            PRESS = PRESS * ONE_OVER_6
            FX = PRESS * NX
            FY = PRESS * NY
            FZ = PRESS * NZ
            TFEXTT = TFEXTT 
     .             + (FX*(V(1,N1) + V(1,N2) + V(1,N3))
     .             +  FY*(V(2,N1) + V(2,N2) + V(2,N3))
     .             +  FZ*(V(3,N1) + V(3,N2) + V(3,N3))) * DT1
c
          ELSE                ! 4 node segment
            D(1) = X(1,N4)
            D(2) = X(2,N4)
            D(3) = X(3,N4)
            M(1) = (X(1,N1) + X(1,N2) + X(1,N3) + X(1,N4)) * FOURTH
            M(2) = (X(2,N1) + X(2,N2) + X(2,N3) + X(2,N4)) * FOURTH
            M(3) = (X(3,N1) + X(3,N2) + X(3,N3) + X(3,N4)) * FOURTH
            PRESS = ZERO
c           1st internal triangle 
            CALL PRESS_SEG3(A       ,B      ,M      ,P0      ,DIR    , 
     .                      IFUN    ,TABLE  ,XFACR  ,XFACT   ,YFAC   ,
     .                      SEGP)
            PRESS = PRESS + SEGP
c           2nd internal triangle 
            CALL PRESS_SEG3(B       ,C      ,M      ,P0      ,DIR    , 
     .                      IFUN    ,TABLE  ,XFACR  ,XFACT   ,YFAC   ,
     .                      SEGP)            
            PRESS = PRESS + SEGP
c           3rd internal triangle 
            CALL PRESS_SEG3(C       ,D      ,M      ,P0      ,DIR    , 
     .                      IFUN    ,TABLE  ,XFACR  ,XFACT   ,YFAC   ,
     .                      SEGP)
            PRESS = PRESS + SEGP
c           4th internal triangle 
            CALL PRESS_SEG3(D       ,A      ,M      ,P0      ,DIR    , 
     .                      IFUN    ,TABLE  ,XFACR  ,XFACT   ,YFAC   ,
     .                      SEGP)
            PRESS = PRESS + SEGP
            PRESS = PRESS * ONE_OVER_8
c            
            NX = (C(2)-A(2))*(D(3)-B(3)) - (C(3)-A(3))*(D(2)-B(2))
            NY = (C(3)-A(3))*(D(1)-B(1)) - (C(1)-A(1))*(D(3)-B(3))
            NZ = (C(1)-A(1))*(D(2)-B(2)) - (C(2)-A(2))*(D(1)-B(1))
            FX = PRESS * NX
            FY = PRESS * NY
            FZ = PRESS * NZ
            TFEXTT = TFEXTT 
     .             + (FX*(V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4))
     .             +  FY*(V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4))
     .             +  FZ*(V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4))) * DT1
          END IF  !  seg 4 node
c-------------------------------------         
c         Accelerations
c-------------------------------------         
          IF (IPARIT == 0) THEN
            ACC(1,N1) = ACC(1,N1) + FX
            ACC(2,N1) = ACC(2,N1) + FY
            ACC(3,N1) = ACC(3,N1) + FZ
            ACC(1,N2) = ACC(1,N2) + FX
            ACC(2,N2) = ACC(2,N2) + FY
            ACC(3,N2) = ACC(3,N2) + FZ
            ACC(1,N3) = ACC(1,N3) + FX
            ACC(2,N3) = ACC(2,N3) + FY
            ACC(3,N3) = ACC(3,N3) + FZ
            IF (N4 > 0) THEN
              ACC(1,N4) = ACC(1,N4) + FX
              ACC(2,N4) = ACC(2,N4) + FY
              ACC(3,N4) = ACC(3,N4) + FZ
            END IF
          ELSE
            IAD = LOADS%LOAD_CYL(I)%SEGMENT_ADRESS(1,J) ! get the adress in the fsky array for N1
            FSKY(1,IAD) = FX
            FSKY(2,IAD) = FY
            FSKY(3,IAD) = FZ
c
            IAD = LOADS%LOAD_CYL(I)%SEGMENT_ADRESS(2,J) ! get the adress in the fsky array for N2
            FSKY(1,IAD) = FX
            FSKY(2,IAD) = FY
            FSKY(3,IAD) = FZ
c
            IAD = LOADS%LOAD_CYL(I)%SEGMENT_ADRESS(3,J) ! get the adress in the fsky array for N3
            FSKY(1,IAD) = FX
            FSKY(2,IAD) = FY
            FSKY(3,IAD) = FZ
c
            IF (N4 > 0) THEN
              IAD = LOADS%LOAD_CYL(I)%SEGMENT_ADRESS(4,J) ! get the adress in the fsky array for N4
              FSKY(1,IAD) = FX
              FSKY(2,IAD) = FY
              FSKY(3,IAD) = FZ
            END IF
          END IF
c
          IF (IANIM > 0) THEN
            FEXT(1,N1) = FEXT(1,N1) + FX
            FEXT(2,N1) = FEXT(2,N1) + FY
            FEXT(3,N1) = FEXT(3,N1) + FZ
            FEXT(1,N2) = FEXT(1,N2) + FX
            FEXT(2,N2) = FEXT(2,N2) + FY
            FEXT(3,N2) = FEXT(3,N2) + FZ
            FEXT(1,N3) = FEXT(1,N3) + FX
            FEXT(2,N3) = FEXT(2,N3) + FY
            FEXT(3,N3) = FEXT(3,N3) + FZ
            IF (N4 > 0) THEN
              FEXT(1,N4) = FEXT(1,N4) + FX
              FEXT(2,N4) = FEXT(2,N4) + FY
              FEXT(3,N4) = FEXT(3,N4) + FZ
            ENDIF
          ENDIF
          IF (CPTREAC > 0) THEN
            IF (NODREAC(N1) > 0) THEN
              FTHREAC(1,NODREAC(N1)) = FTHREAC(1,NODREAC(N1)) + FX*DT1
              FTHREAC(2,NODREAC(N1)) = FTHREAC(2,NODREAC(N1)) + FY*DT1
              FTHREAC(3,NODREAC(N1)) = FTHREAC(3,NODREAC(N1)) + FZ*DT1
            ENDIF
            IF (NODREAC(N2) > 0) THEN
              FTHREAC(1,NODREAC(N2)) = FTHREAC(1,NODREAC(N2)) + FX*DT1
              FTHREAC(2,NODREAC(N2)) = FTHREAC(2,NODREAC(N2)) + FY*DT1
              FTHREAC(3,NODREAC(N2)) = FTHREAC(3,NODREAC(N2)) + FZ*DT1
            ENDIF
            IF (NODREAC(N3) > 0) THEN
              FTHREAC(1,NODREAC(N3)) = FTHREAC(1,NODREAC(N3)) + FX*DT1
              FTHREAC(2,NODREAC(N3)) = FTHREAC(2,NODREAC(N3)) + FY*DT1
              FTHREAC(3,NODREAC(N3)) = FTHREAC(3,NODREAC(N3)) + FZ*DT1
            ENDIF
            IF (N4 > 0) THEN
              IF (NODREAC(N4) > 0) THEN
                FTHREAC(1,NODREAC(N4)) = FTHREAC(1,NODREAC(N4)) + FX*DT1
                FTHREAC(2,NODREAC(N4)) = FTHREAC(2,NODREAC(N4)) + FY*DT1
                FTHREAC(3,NODREAC(N4)) = FTHREAC(3,NODREAC(N4)) + FZ*DT1
              ENDIF
            ENDIF
          ENDIF
c
        END DO     ! NSEG
      END DO       ! 1,NLOAD_CYL
c--------------------
c     external forces
c--------------------
      TFEXT = TFEXT + TFEXTT
c-----------
      RETURN
      END
      
