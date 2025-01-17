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
      !||    fixtemp          ../engine/source/constraints/thermic/fixtemp.F
      !||--- called by ------------------------------------------------------
      !||    resol            ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    vinter           ../engine/source/tools/curve/vinter.F
      !||    vinter_smooth    ../engine/source/tools/curve/vinter_smooth.F
      !||--- uses       -----------------------------------------------------
      !||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||    sensor_mod       ../common_source/modules/sensor_mod.F90
      !||====================================================================
      SUBROUTINE FIXTEMP(IBFT   ,VAL    , TEMP  ,NPC    ,TF   ,
     .                   NSENSOR,SENSOR_TAB,GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type (glob_therm_) ,intent(inout)   :: glob_therm
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*)
      INTEGER IBFT(GLOB_THERM%NIFT,*)
      my_real :: TEMP(*),TF(*)
      my_real :: VAL(GLOB_THERM%LFACTHER,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,L,II, NN,ISENS,IC, IDEB, ISMOOTH
      INTEGER ILENC(MVSIZ), IPOSC(MVSIZ), IADC(MVSIZ)
      INTEGER INDEX(MVSIZ)
      my_real FAC, FACX, STARTT, STOPT, TS
      my_real YC(MVSIZ), TSC(MVSIZ), DYDXC(MVSIZ)
C-----------------------------------------------
      IDEB = 0
      ISMOOTH = 0
C
      DO NN=1,GLOB_THERM%NFXTEMP,NVSIZ
        IC = 0
        IF (NSENSOR > 0) THEN
          DO II = 1, MIN(GLOB_THERM%NFXTEMP - IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VAL(1,N)
            STOPT  = VAL(2,N)
C
            ISENS = IBFT(3,N)
            IF (ISENS == 0)THEN
               TS = TT*GLOB_THERM%THEACCFACT - STARTT
            ELSE 
               STARTT = STARTT + SENSOR_TAB(ISENS)%TSTART
               STOPT  = STOPT  + SENSOR_TAB(ISENS)%TSTART
               TS = TT*GLOB_THERM%THEACCFACT -(STARTT + SENSOR_TAB(ISENS)%TSTART)      
            ENDIF       
C
            IF(TT*GLOB_THERM%THEACCFACT < STARTT) CYCLE
            IF(TT*GLOB_THERM%THEACCFACT > STOPT)  CYCLE
            FACX   = VAL(3,N)
            I=IABS(IBFT(1,N))
            IC = IC + 1
            INDEX(IC)= N
C            TSC(IC) = (TS+DT2)*FACX     
            TSC(IC) = TS*FACX     
          ENDDO
        ELSE
C       sans aucun sensor
          DO II = 1, MIN(GLOB_THERM%NFXTEMP-IDEB,NVSIZ)
            N = II+IDEB
            STARTT = VAL(1,N)
            STOPT  = VAL(2,N)
            IF (TT*GLOB_THERM%THEACCFACT < STARTT) CYCLE
            IF (TT*GLOB_THERM%THEACCFACT > STOPT)  CYCLE
            FACX   = VAL(3,N)
            I=IABS(IBFT(1,N))
            IC = IC + 1
            INDEX(IC) = N     
            TS = TT*GLOB_THERM%THEACCFACT - STARTT
            TSC(IC) = TS*FACX          
          ENDDO
        ENDIF
C
        IDEB = IDEB + MIN(GLOB_THERM%NFXTEMP-IDEB,NVSIZ)
C

        IF(NCYCLE == 0)THEN
         DO II=1,IC
          N = INDEX(II)
          L = IBFT(2,N)
          IF (L > 0) ISMOOTH = NPC(2*NFUNCT+L+1)
          IPOSC(II) = 0
          IADC(II)  = NPC(L) / 2 + 1
          ILENC(II) = NPC(L+1) / 2 - IADC(II) - IPOSC(II)
         ENDDO
        ELSE
         DO II=1,IC
          N = INDEX(II)
          L = IBFT(2,N)
          IF (L > 0) ISMOOTH = NPC(2*NFUNCT+L+1)
          IPOSC(II) = IBFT(4,N)
          IADC(II)  = NPC(L) / 2 + 1
          ILENC(II) = NPC(L+1) / 2 - IADC(II) - IPOSC(II)
         ENDDO
        ENDIF
!!        CALL VINTER(TF,IADC,IPOSC,ILENC,IC,TSC,DYDXC,YC)
        IF (ISMOOTH == 0) THEN
          CALL VINTER(TF,IADC,IPOSC,ILENC,IC,TSC,DYDXC,YC)
        ELSE
          CALL VINTER_SMOOTH(TF,IADC,IPOSC,ILENC,IC,TSC,DYDXC,YC)
        ENDIF ! IF (ISMOOTH == 0)
C
        IF(IVECTOR == 0) THEN
         DO II=1,IC
          N = INDEX(II) 
          IBFT(4,N) = IPOSC(II)
          FAC  = VAL(4,N)
          YC(II) = YC(II) * FAC
          I=IABS(IBFT(1,N))
          TEMP(I) = YC(II)
         ENDDO 
        ELSE
C       partie vectorielle                  
#include "vectorize.inc"
          DO II=1,IC
           N = INDEX(II)
           IBFT(4,N) = IPOSC(II)
           FAC  = VAL(4,N)
           YC(II) = YC(II) * FAC
           I=IABS(IBFT(1,N))
           TEMP(I) = YC(II) 
          ENDDO
        ENDIF
C
      ENDDO !NN=1,NFXTEMP
C
      RETURN
      END
