Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    radiation             ../engine/source/constraints/thermic/radiation.F
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    finter                ../engine/source/tools/curve/finter.F
!||    omp_get_thread_num    ../engine/source/engine/openmp_stub.F90
!||--- uses       -----------------------------------------------------
!||    glob_therm_mod        ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    python_funct_mod      ../common_source/modules/python_mod.F90
!||    sensor_mod            ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE RADIATION(IBCR   ,FRADIA    ,NPC       ,TF     , X  ,
     1                     TEMP   ,NSENSOR   ,SENSOR_TAB,FTHE   ,IAD ,
     2                     FTHESKY, PYTHON   ,GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PYTHON_FUNCT_MOD
      USE SENSOR_MOD
      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type (glob_therm_) ,intent(inout)   :: glob_therm
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*),IAD(4,*)
      INTEGER IBCR(GLOB_THERM%NIRADIA,*)
C     REAL
      my_real
     .   FRADIA(GLOB_THERM%LFACTHER,*), TF(*), X(3,*),
     .   FTHESKY(LSKY), TEMP(*), FTHE(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE(PYTHON_) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, N2, N3, N4, ISENS,
     .        IFUNC_OLD,IFUNC,IAD1,IAD2,IAD3,IAD4
      INTEGER IFLAG
      my_real :: NX, NY, NZ, DYDX, TS, FLUX,
     .   TS_OLD, FCX, FCY, T_INF, TE, AREA,
     .    STARTT, STOPT, FCY_OLD, EMISIG, OFFG
      my_real FINTER 
      my_real :: HEAT_RADIA_L ! thread-local 
      EXTERNAL FINTER
      INTEGER :: OMP_GET_THREAD_NUM,ITSK
      EXTERNAL OMP_GET_THREAD_NUM
      INTEGER S1
      INTEGER :: ISMOOTH

C=======================================================================
      ISMOOTH = 0
      S1 = NUMNOD
      IFUNC_OLD = 0
      TS_OLD = ZERO
      FCY_OLD= ZERO
      HEAT_RADIA_L = ZERO
      T_INF = ZERO
      N1 = 0
      N2 = 0
      N3 = 0
      N4 = 0
      IF(IPARIT==0) THEN
        ITSK = OMP_GET_THREAD_NUM()
C-----------------------------------------------------------
C  CODE SPMD PARITH/OFF OU SMP NE PAS OUBLIER LE CODE P/ON !
C-----------------------------------------------------------
!$OMP DO SCHEDULE(GUIDED)
       DO NL=1,GLOB_THERM%NUMRADIA
         OFFG   = FRADIA(6,NL)
         IF(OFFG <= ZERO) CYCLE 
         STARTT = FRADIA(4,NL)
         STOPT  = FRADIA(5,NL)
         ISENS  = IBCR(6,NL)
         IF(ISENS == 0)THEN
            TS = TT*GLOB_THERM%THEACCFACT - STARTT
         ELSE 
            STARTT = STARTT + SENSOR_TAB(ISENS)%TSTART
            STOPT  = STOPT  + SENSOR_TAB(ISENS)%TSTART      
            TS = TT*GLOB_THERM%THEACCFACT -(STARTT + SENSOR_TAB(ISENS)%TSTART)      
         ENDIF       
C
         IF(TT*GLOB_THERM%THEACCFACT < STARTT) CYCLE
         IF(TT*GLOB_THERM%THEACCFACT > STOPT ) CYCLE 
         N1 = IBCR(1,NL)
         N2 = IBCR(2,NL)
         N3 = IBCR(3,NL)
         N4 = IBCR(4,NL)
         IFUNC = IBCR(5,NL)
         FCY   = FRADIA(1,NL)
         FCX   = FRADIA(2,NL)
         EMISIG= FRADIA(3,NL)
C----------------------
C       RADIATION FLUX
C----------------------
        IF(IFUNC_OLD /= IFUNC .OR. TS_OLD /= TS .OR. FCY_OLD /= FCY ) THEN
          ISMOOTH = 0
          IF (IFUNC > 0) ISMOOTH = NPC(2*NFUNCT+IFUNC+1)
          IF(ISMOOTH < 0) THEN
            CALL PYTHON_CALL_FUNCT1D(PYTHON, -ISMOOTH,TS*FCX, T_INF)
            T_INF = FCY*T_INF
          ELSE
            T_INF = FCY*FINTER(IFUNC, TS*FCX,NPC,TF,DYDX)
          ENDIF
          IFUNC_OLD = IFUNC
          TS_OLD = TS
          FCY_OLD= FCY
        ENDIF
C        ANALYSE 3D
        IF(N4 > 0)THEN
C         
           NX= (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2))
     +        -(X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
           NY= (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2))
     +        -(X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
           NZ= (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2))
     +        -(X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
C
           TE = FOURTH*(TEMP(N1) + TEMP(N2) + TEMP(N3) + TEMP(N4))      
           AREA = HALF*SQRT( NX*NX + NY*NY + NZ*NZ)
           FLUX = AREA*EMISIG*(T_INF**4 - TE**4)*DT1*GLOB_THERM%THEACCFACT 
           HEAT_RADIA_L = HEAT_RADIA_L + FLUX
           FLUX = FOURTH*FLUX
C
           FTHE(S1*ITSK+N1) = FTHE(S1*ITSK+N1) + FLUX
           FTHE(S1*ITSK+N2) = FTHE(S1*ITSK+N2) + FLUX
           FTHE(S1*ITSK+N3)=  FTHE(S1*ITSK+N3) + FLUX
           FTHE(S1*ITSK+N4)=  FTHE(S1*ITSK+N4) + FLUX  

C
         ELSEIF(N3 > 0) THEN  !TRUE TRIANGLES
C
          NX= (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2))
     +       -(X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
          NY= (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2))
     +       -(X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
          NZ= (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2))
     +       -(X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
C
          TE = THIRD*(TEMP(N1) + TEMP(N2) + TEMP(N3) )      
          AREA = HALF*SQRT( NX*NX + NY*NY + NZ*NZ)
          FLUX = AREA*EMISIG*(T_INF**4 - TE**4)*DT1*GLOB_THERM%THEACCFACT
          HEAT_RADIA_L = HEAT_RADIA_L + FLUX
          FLUX = THIRD*FLUX
C 
          FTHE(S1*ITSK+N1) = FTHE(S1*ITSK+N1) + FLUX
          FTHE(S1*ITSK+N2) = FTHE(S1*ITSK+N2) + FLUX
          FTHE(S1*ITSK+N3)=  FTHE(S1*ITSK+N3) + FLUX
        ELSE
C        ANALYSE 2D
         NY=  -X(3,N2)+X(3,N1)
         NZ=   X(2,N2)-X(2,N1)
C     
         TE = HALF*(TEMP(N1) + TEMP(N2) )      
         AREA = SQRT( NY*NY + NZ*NZ)
         FLUX = AREA*EMISIG*(T_INF**4 - TE**4)*DT1*GLOB_THERM%THEACCFACT
         GLOB_THERM%HEAT_RADIA = GLOB_THERM%HEAT_RADIA + FLUX
         FLUX = HALF*FLUX
C
         FTHE(S1*ITSK+N1)=FTHE(S1*ITSK+N1) + FLUX
         FTHE(S1*ITSK+N2)=FTHE(S1*ITSK+N2) + FLUX
C
        ENDIF
       ENDDO
!$OMP END DO

!$OMP CRITICAL
       GLOB_THERM%HEAT_RADIA = GLOB_THERM%HEAT_RADIA + HEAT_RADIA_L
!$OMP END CRITICAL
C
      ELSE
C-------------------------
C   CODE SPMD PARITH/ON
C   CODE NON VECTORIEL
C-------------------------
!$OMP DO SCHEDULE(GUIDED)
        DO NL=1,GLOB_THERM%NUMRADIA
          STARTT = FRADIA(4,NL)
          STOPT  = FRADIA(5,NL)
          OFFG   = FRADIA(6,NL)
          ISENS  = IBCR(6,NL)
          IF(ISENS == 0)THEN
             TS = TT*GLOB_THERM%THEACCFACT - STARTT
          ELSE 
             STARTT = STARTT + SENSOR_TAB(ISENS)%TSTART
             STOPT  = STOPT  + SENSOR_TAB(ISENS)%TSTART   
             TS = TT*GLOB_THERM%THEACCFACT -(STARTT + SENSOR_TAB(ISENS)%TSTART)      
          ENDIF       
          IFLAG = 1
          IF(TT*GLOB_THERM%THEACCFACT < STARTT)  IFLAG = 0
          IF(TT*GLOB_THERM%THEACCFACT > STOPT )  IFLAG = 0
          IF(OFFG <= ZERO) IFLAG = 0
C---------------------
C  RADIATION FLUX
C---------------------
          IF(IFLAG==1) THEN
            N1 =IBCR(1,NL)
            N2 =IBCR(2,NL)
            N3 =IBCR(3,NL)
            N4 =IBCR(4,NL)
            IFUNC = IBCR(5,NL)            
            FCY   = FRADIA(1,NL)
            FCX   = FRADIA(2,NL)
            EMISIG= FRADIA(3,NL)
            IF(IFUNC_OLD /= IFUNC .OR. TS_OLD /= TS) THEN
              IF (IFUNC > 0) ISMOOTH = NPC(2*NFUNCT+IFUNC+1)
              IF(ISMOOTH < 0 .AND. IFUNC > 0) THEN
                CALL PYTHON_CALL_FUNCT1D(PYTHON, -ISMOOTH,TS*FCX, T_INF)
              ELSE
                T_INF = FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
              ENDIF
              IFUNC_OLD = IFUNC
              TS_OLD = TS
            ENDIF
            T_INF = FCY*T_INF
C      ANALYSE 3D
            IF(N4 > 0)THEN
               NX= (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2))
     +            -(X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
               NY= (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2))
     +            -(X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
               NZ= (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2))
     +            -(X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
               TE = FOURTH*(TEMP(N1) + TEMP(N2) + TEMP(N3) + TEMP(N4))      
               AREA = HALF*SQRT( NX*NX + NY*NY + NZ*NZ)
               FLUX = AREA*EMISIG*(T_INF**4 - TE**4)*DT1*GLOB_THERM%THEACCFACT
               HEAT_RADIA_L = HEAT_RADIA_L + FLUX
               FLUX = FOURTH*FLUX
C
               IAD1 = IAD(1,NL)
               FTHESKY(IAD1) = FLUX
               IAD2 = IAD(2,NL)
               FTHESKY(IAD2) = FLUX
               IAD3 = IAD(3,NL)
               FTHESKY(IAD3) = FLUX
               IAD4 = IAD(4,NL)
               FTHESKY(IAD4) = FLUX
C
             ELSEIF( N3 > 0) THEN
C      TRUE TRIANGLES
               NX= (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2))
     +            -(X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
               NY= (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2))
     +            -(X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
               NZ= (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2))
     +            -(X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
C
               TE = THIRD*(TEMP(N1) + TEMP(N2) + TEMP(N3) )      
               AREA = HALF*SQRT( NX*NX + NY*NY + NZ*NZ)
               FLUX = AREA*EMISIG*(T_INF**4 - TE**4)*DT1*GLOB_THERM%THEACCFACT 
               HEAT_RADIA_L = HEAT_RADIA_L + FLUX
               FLUX = THIRD*FLUX
C
               IAD1 = IAD(1,NL)
               FTHESKY(IAD1) = FLUX
C
               IAD2 = IAD(2,NL)
               FTHESKY(IAD2) = FLUX
C
               IAD3 = IAD(3,NL)
               FTHESKY(IAD3) = FLUX
C
             ELSE
C      ANALYSE 2D
               NY=  -X(3,N2)+X(3,N1)
               NZ=   X(2,N2)-X(2,N1)
 
C 
               TE = HALF*(TEMP(N1) + TEMP(N2) )      
               AREA = SQRT( NY*NY + NZ*NZ)
               FLUX = AREA*EMISIG*(T_INF**4 - TE**4)*DT1*GLOB_THERM%THEACCFACT
               HEAT_RADIA_L = HEAT_RADIA_L + FLUX
               FLUX = HALF*FLUX
C
               IAD1 = IAD(1,NL)
               FTHESKY(IAD1) = FLUX
C
               IAD2 = IAD(2,NL)
               FTHESKY(IAD2) = FLUX      
             ENDIF
C
           ELSE   ! IFLAG=0
             IAD1 = IAD(1,NL)
             FTHESKY(IAD1) = ZERO
             IAD2 = IAD(2,NL)
             FTHESKY(IAD2) = ZERO
             IF(N4 > 0)THEN
               IAD3 = IAD(3,NL)
               FTHESKY(IAD3) = ZERO
               IAD4 = IAD(4,NL)
               FTHESKY(IAD4) = ZERO
             ELSEIF(N3 > 0)THEN
               IAD3 = IAD(3,NL)
               FTHESKY(IAD3) = ZERO
             ENDIF
           ENDIF
        ENDDO             
!$OMP END DO

!$OMP CRITICAL
       GLOB_THERM%HEAT_RADIA = GLOB_THERM%HEAT_RADIA + HEAT_RADIA_L
!$OMP END CRITICAL

C
      ENDIF
C   
      RETURN
      END
