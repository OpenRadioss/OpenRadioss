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
!||    fixflux               ../engine/source/constraints/thermic/fixflux.F
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    arret                 ../engine/source/system/arret.F
!||    finter                ../engine/source/tools/curve/finter.F
!||    s4volume              ../engine/source/elements/solid/solide4/s4volume.F
!||    s8evolume             ../engine/source/elements/solid/solide8e/s8evolume.F
!||--- uses       -----------------------------------------------------
!||    glob_therm_mod        ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    python_funct_mod      ../common_source/modules/python_mod.F90
!||    sensor_mod            ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE FIXFLUX (IBFFLUX ,FBFFLUX ,NPC   ,TF  ,X  ,IXS,
     .                    NSENSOR ,SENSOR_TAB,FTHE  ,IAD ,FTHESKY, PYTHON,
     .                    GLOB_THERM)
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
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type (glob_therm_) ,intent(inout)   :: glob_therm
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*),IAD(4,*)
      INTEGER IBFFLUX(GLOB_THERM%NITFLUX,*)
      INTEGER IXS(NIXS,*)
      my_real
     .   FBFFLUX(GLOB_THERM%LFACTHER,*), TF(*), X(3,*),
     .   FTHESKY(LSKY), FTHE(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE(PYTHON_) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, N2, N3, N4, N5, N6, N7, N8, ISENS, 
     .        IFUNC_OLD,IFUNC,IAD1,IAD2,IAD3,IAD4,IFLAG
      INTEGER IEL
      my_real NX, NY, NZ, DYDX, TS, FLUX, TS_OLD, FCX, FCY, FLUX_DENS, AREA
      my_real STARTT, STOPT, FCY_OLD
      my_real TTA, TTB, DT1A, DT1N, VOLG, BID
      my_real FINTER 
      EXTERNAL FINTER
      INTEGER :: ISMOOTH
C=======================================================================
      IFUNC_OLD = 0
      TS_OLD    = ZERO
      FCY_OLD   = ZERO
      FLUX_DENS = ZERO
      N4 = ZERO
      DT1N = ZERO
C
      IF(IPARIT == 0) THEN
C-----------------------------------------------------------
C  CODE PARITH/OFF  NE PAS OUBLIER LE CODE P/ON !
C-----------------------------------------------------------
       DO  NL=1,GLOB_THERM%NFXFLUX
C
         ISENS  = IBFFLUX(6,NL)
C
         STARTT = FBFFLUX(4,NL)
         STOPT  = FBFFLUX(5,NL)
         TTA    = TT *GLOB_THERM%THEACCFACT
         DT1A   = DT1*GLOB_THERM%THEACCFACT
         TTB    = TTA - DT1A
         IF(ISENS == 0)THEN
            TS = TTA - STARTT
         ELSE 
            STARTT = STARTT + SENSOR_TAB(ISENS)%TSTART
            STOPT  = STOPT  + SENSOR_TAB(ISENS)%TSTART
            TS = TTA - STARTT      
         ENDIF       
C
         IF(TTA < STARTT .OR. TTB >= STOPT) CYCLE
         IF(TTA > STOPT ) THEN
            IF(TTB <= STARTT) THEN
               DT1N = STOPT - STARTT
            ELSE
               DT1N = STOPT - TTB
            ENDIF
         ELSEIF(TTA <= STOPT) THEN
            IF(TTB <= STARTT) THEN
               DT1N = TTA - STARTT
            ELSE
               DT1N = DT1A
            ENDIF
         ENDIF     
C
         IFUNC = IBFFLUX(5,NL)
         FCY = FBFFLUX(1,NL)
         FCX = FBFFLUX(2,NL)
         IF(IFUNC_OLD /= IFUNC .OR. TS_OLD /= TS .OR. FCY_OLD /= FCY ) THEN
           ISMOOTH = 0
           IF (IFUNC > 0) ISMOOTH = NPC(2*NFUNCT+IFUNC+1)
           IF(ISMOOTH < 0) THEN
             CALL PYTHON_CALL_FUNCT1D(PYTHON, -ISMOOTH,TS*FCX, FLUX_DENS)
             FLUX_DENS = FCY*FLUX_DENS
           ELSE
             FLUX_DENS = FCY*FINTER(IFUNC, TS*FCX,NPC,TF,DYDX)
           ENDIF
          IFUNC_OLD = IFUNC
          TS_OLD    = TS
          FCY_OLD   = FCY
         ENDIF
C----------------------------
C       IMPOSED SURFACIC FLUX
C----------------------------
         IF(IBFFLUX(10,NL) == 0) THEN
          N1 = IBFFLUX(1,NL)
          N2 = IBFFLUX(2,NL)
          N3 = IBFFLUX(3,NL)
          N4 = IBFFLUX(4,NL)
C        ANALYSE 3D
          IF(N4 > 0)THEN ! QUAD
C         
           NX= (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2))
     +        -(X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
           NY= (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2))
     +        -(X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
           NZ= (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2))
     +        -(X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
C
           AREA = HALF*SQRT(NX*NX + NY*NY + NZ*NZ)
           FLUX = AREA*FLUX_DENS*DT1N 
           GLOB_THERM%HEAT_FFLUX = GLOB_THERM%HEAT_FFLUX + FLUX
           FLUX = FOURTH*FLUX
C
           FTHE(N1) = FTHE(N1) + FLUX
           FTHE(N2) = FTHE(N2) + FLUX
           FTHE(N3) = FTHE(N3) + FLUX
           FTHE(N4) = FTHE(N4) + FLUX  
C
          ELSEIF(N3 > 0) THEN !TRUE TRIANGLES
           NX= (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2))
     +        -(X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
           NY= (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2))
     +        -(X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
           NZ= (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2))
     +        -(X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
C
           AREA = HALF*SQRT( NX*NX + NY*NY + NZ*NZ)
           FLUX = AREA*FLUX_DENS*DT1N
           GLOB_THERM%HEAT_FFLUX = GLOB_THERM%HEAT_FFLUX + FLUX
           FLUX = THIRD*FLUX
C 
           FTHE(N1) = FTHE(N1) + FLUX
           FTHE(N2) = FTHE(N2) + FLUX
           FTHE(N3) = FTHE(N3) + FLUX
C       
          ELSE !ANALYSE 2D
           NY=  -X(3,N2)+X(3,N1)
           NZ=   X(2,N2)-X(2,N1)
C     
           AREA = SQRT(NY*NY + NZ*NZ)
           FLUX = AREA*FLUX_DENS*DT1N
           GLOB_THERM%HEAT_FFLUX = GLOB_THERM%HEAT_FFLUX + FLUX
           FLUX = HALF*FLUX
C
           FTHE(N1) = FTHE(N1) + FLUX
           FTHE(N2) = FTHE(N2) + FLUX
C
          ENDIF
C----------------------------
C       IMPOSED VOLUMIC FLUX
C----------------------------
         ELSE
           IEL = IBFFLUX(1,NL)
           IF(IEL == 0) THEN
              IBFFLUX(1,NL)=IBFFLUX(8,NL)
              IEL = IBFFLUX(1,NL)
           ENDIF
           N1  = IXS(2,IEL)
           N2  = IXS(3,IEL)
           N3  = IXS(4,IEL)
           N4  = IXS(5,IEL)
           N5  = IXS(6,IEL)
           N6  = IXS(7,IEL)
           N7  = IXS(8,IEL)
           N8  = IXS(9,IEL)
             
           IF(N1 == N2 .AND. N3 == N4 .AND. N5 == N8 .AND. N6 == N7) THEN       
             CALL S4VOLUME(X, VOLG, 1, N1, N3, N6, N5)
           ELSE  
             CALL S8EVOLUME(X, VOLG, BID, 1, 0, 0, 0, N1, N2, N3, N4, N5, N6, N7, N8)
           ENDIF

           FLUX = VOLG*FLUX_DENS*DT1N 
           GLOB_THERM%HEAT_FFLUX = GLOB_THERM%HEAT_FFLUX + FLUX
           FLUX = ONE_OVER_8*FLUX
C
           FTHE(N1) = FTHE(N1) + FLUX
           FTHE(N2) = FTHE(N2) + FLUX
           FTHE(N3) = FTHE(N3) + FLUX
           FTHE(N4) = FTHE(N4) + FLUX
           FTHE(N5) = FTHE(N5) + FLUX
           FTHE(N6) = FTHE(N6) + FLUX
           FTHE(N7) = FTHE(N7) + FLUX
           FTHE(N8) = FTHE(N8) + FLUX
         ENDIF
       ENDDO  ! N=1,NFXFLUX
C

      ELSE
C-------------------------
C CODE PARITH/ON
C CODE NON VECTORIEL
C-------------------------
        DO NL=1,GLOB_THERM%NFXFLUX
           ISENS  = IBFFLUX(6,NL)
           STARTT = FBFFLUX(4,NL) 
           STOPT  = FBFFLUX(5,NL)
           TTA    = TT *GLOB_THERM%THEACCFACT
           DT1A   = DT1*GLOB_THERM%THEACCFACT
           IF (ISENS == 0)THEN
              TS = TTA - STARTT
           ELSE 
              STARTT = STARTT + SENSOR_TAB(ISENS)%TSTART
              STOPT  = STOPT  + SENSOR_TAB(ISENS)%TSTART
              TS = TTA - STARTT
           ENDIF       
           IFLAG = 1
           IF(TTA < STARTT)  IFLAG = 0
           IF(TTA > STOPT )  IFLAG = 0

           IF(IBFFLUX(10,NL) == 0) THEN
C----------------------------
C       IMPOSED SURFACIC FLUX
C----------------------------
            IF(IFLAG==1) THEN
             N1 =IBFFLUX(1,NL)
             N2 =IBFFLUX(2,NL)
             N3 =IBFFLUX(3,NL)
             N4 =IBFFLUX(4,NL)
             IFUNC = IBFFLUX(5,NL)            
             FCY   = FBFFLUX(1,NL)
             FCX   = FBFFLUX(2,NL)
             IF(IFUNC_OLD /= IFUNC .OR. TS_OLD /= TS) THEN
               ISMOOTH = 0
               IF (IFUNC > 0) ISMOOTH = NPC(2*NFUNCT+IFUNC+1)
               IF(ISMOOTH < 0) THEN
                 CALL PYTHON_CALL_FUNCT1D(PYTHON, -ISMOOTH,TS*FCX, FLUX_DENS)
               ELSE
                 FLUX_DENS = FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
               ENDIF
               IFUNC_OLD = IFUNC
               TS_OLD = TS
             ENDIF
C
C            ANALYSE 3D
             IF(N4 > 0)THEN
               NX= (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2))
     +            -(X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
               NY= (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2))
     +            -(X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
               NZ= (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2))
     +            -(X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
C
               AREA = HALF*SQRT(NX*NX + NY*NY + NZ*NZ)
               FLUX = AREA*FLUX_DENS*FCY*DT1A
               GLOB_THERM%HEAT_FFLUX = GLOB_THERM%HEAT_FFLUX + FLUX
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
             ELSEIF( N3 > 0) THEN  !TRUE TRIANGLES.
               NX= (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2))
     +            -(X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
               NY= (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2))
     +            -(X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
               NZ= (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2))
     +            -(X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
C
               AREA = HALF*SQRT(NX*NX + NY*NY + NZ*NZ)
               FLUX = AREA*FLUX_DENS*FCY*DT1A 
               GLOB_THERM%HEAT_FFLUX = GLOB_THERM%HEAT_FFLUX + FLUX
               FLUX = THIRD*FLUX
C
               IAD1 = IAD(1,NL)
               FTHESKY(IAD1) = FLUX
               IAD2 = IAD(2,NL)
               FTHESKY(IAD2) = FLUX
               IAD3 = IAD(3,NL)
               FTHESKY(IAD3) = FLUX
C
             ELSE !ANALYSE 2D
               NY=  -X(3,N2)+X(3,N1)
               NZ=   X(2,N2)-X(2,N1) 
C 
               AREA = SQRT(NY*NY + NZ*NZ)
               FLUX = AREA*FLUX_DENS*FCY*DT1A
               GLOB_THERM%HEAT_FFLUX = GLOB_THERM%HEAT_FFLUX + FLUX
               FLUX = HALF*FLUX
C
               IAD1 = IAD(1,NL)
               FTHESKY(IAD1) = FLUX
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
           ELSE
C----------------------------
C       IMPOSED VOLUMIC FLUX
C----------------------------
             WRITE(IOUT,'(//A)') ' VOLUMIC HEAT FLUX IS NOT 
     . COMPATIBLE WITH /PARITH/ON: USE /PARITH/OFF'
             WRITE(6,*) ' VOLUMIC HEAT FLUX IS NOT ',
     .                   'COMPATIBLE WITH /PARITH/ON: USE /PARITH/OFF'
             CALL FLUSH(6)
             CALL ARRET(2)
           ENDIF
        ENDDO       
C
      ENDIF
C   
      RETURN
      END
