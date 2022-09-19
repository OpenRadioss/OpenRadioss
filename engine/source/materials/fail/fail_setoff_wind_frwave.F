Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  FAIL_SETOFF_WIND_FRWAVE       source/materials/fail/fail_setoff_wind_frwave.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_SETOFF_WIND_FRWAVE(
     .           ELBUF_STR,GEO      ,PID      ,NGL      ,
     .           NEL      ,IR       ,IS       ,NLAY     ,
     .           NPTTOT   ,PTHKF    ,THK_LY   ,THKLY    ,
     .           OFF      ,NPG      ,STACK    ,ISUBSTACK,
     .           IGTYP    ,FAILWAVE ,FWAVE_EL ,DMG_FLAG ,
     .           TIME     ,TRELAX   ,TFAIL    ,DMG_SCALE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "scr17_c.inc"
#include "com01_c.inc"
#include "com08_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: NEL,NPTTOT,NLAY,PID,IR,IS,NPG,ISUBSTACK,IGTYP,DMG_FLAG
      my_real  :: TIME,TRELAX
      INTEGER, DIMENSION(NEL) :: NGL,FWAVE_EL
      my_real, DIMENSION(NPTTOT*NEL) :: THKLY
      my_real, DIMENSION(NPROPG,*) :: GEO
      my_real, DIMENSION(NLAY,*)   :: PTHKF
      my_real, DIMENSION(NEL   )   :: OFF,TFAIL,DMG_SCALE
      my_real, DIMENSION(NEL,*)    :: THK_LY
      TYPE(ELBUF_STRUCT_)  ,TARGET :: ELBUF_STR
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IEL,IPOS,IL,IFL,IP,IPT,IG,IPG,JPG,NPTR,NPTS,NPTT,
     .   IDMG,COUNTPG,NINDXPG,NINDXLY,IPT_ALL,NFAIL,IPWEIGHT,IPTHKLY
      INTEGER, DIMENSION(NEL) :: NPTF,INDXPG,INDXLY
      INTEGER, DIMENSION(10) :: ISTRESS
      INTEGER, DIMENSION(:), POINTER :: OFFLY,OFFPG,FOFF
      my_real, DIMENSION(NEL) :: UEL1,DFMAX,TDEL,NPTTF,SIGSCALE
      my_real, DIMENSION(:), POINTER :: DMAX
      my_real, DIMENSION(NLAY) :: WEIGHT,P_THKLY
      my_real :: THK_LAY,P_THICKG,FAIL_EXP,THFACT,NORM,DFAIL
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE (STACK_PLY) :: STACK
c-----------------------------------------------------------------------
c     NPTT       NUMBER OF INTEGRATION POINTS IN CURRENT LAYER
c     NPTTF      NUMBER OF FAILED INTEGRATION POINTS IN THE LAYER
c     NPTTOT     NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
c     OFFPG(NEL,NPG)  failure flag of PG in each layer  1=alive ,0=dead
c     THK_LY     Ratio of layer thickness / element thickness
c     THK        Total element thickness
c     TFAIL(NEL) : global element variable - start of relaxation time before element suppression
C=======================================================================
      IPTHKLY   = 700
      IPWEIGHT  = 900
      P_THICKG  = GEO(42,PID)
      FAIL_EXP  = GEO(43,PID)
c
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPG  = NPTR*NPTS            ! number of in-plane Gauss points
      IPG  = (IS-1)*NPTR + IR     ! current Gauss point
      JPG  = (IPG-1)*NEL
c
c------------------------------------
      IF (NLAY == 1) THEN
c------------------------------------
        IL = 1
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
        NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
        OFFPG => ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)
        OFFLY => ELBUF_STR%BUFLY(IL)%OFF
c
        IF (NFAIL == 1 .and. P_THICKG > ZERO) THEN
          PTHKF(IL,1) = MAX(P_THICKG,EM06)
          PTHKF(IL,1) = MIN(P_THICKG,ONE-EM06)
        ELSE
          DO IFL = 1,NFAIL
            PTHKF(IL,IFL) = MAX(PTHKF(IL,IFL),EM06)
            PTHKF(IL,IFL) = MIN(PTHKF(IL,IFL),ONE-EM06)
          ENDDO
        ENDIF
c------------------
        IF (FAILWAVE%WAVE_MOD > 0) THEN
          DO IFL = 1,NFAIL
            DO IPT=1,NPTT
              DMAX => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%DAMMX
              DO IEL=1,NEL
                IF (OFFLY(IEL) < 0 .and. DMAX(IEL) == ONE) THEN
                  FWAVE_EL(IEL) = OFFLY(IEL)  ! set element frontwave flag
                  OFFLY(IEL) = 0
                ENDIF
              ENDDO     ! IPT=1,NPTT
            ENDDO       ! IFL = 1,NFAIL
          ENDDO         ! IEL=1,NEL
        ENDIF
c------------------
        DO IEL=1,NEL
          IF (OFF(IEL) == ZERO .or. OFFPG(IEL) == 0) CYCLE
          DO IFL = 1,NFAIL
            THFACT = ZERO
            DO IPT=1,NPTT
              FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF
              IF (FOFF(IEL) < ONE)  THEN
                IPOS = (IPT-1)*NEL + IEL
                THFACT = THFACT + THKLY(IPOS)
              ENDIF
              IF (THFACT >= PTHKF(IL,IFL)) THEN  ! delete current PG in the layer
                OFFPG(IEL) = 0
              ENDIF
            ENDDO     ! IPT=1,NPTT
          ENDDO       ! IFL = 1,NFAIL
        ENDDO         ! IEL=1,NEL
c------------------
        NINDXPG = 0
        DO IEL=1,NEL
          IF (OFFPG(IEL) == 0) THEN
            NINDXPG = NINDXPG + 1
            INDXPG(NINDXPG) = IEL
          ENDIF
        ENDDO
c------------------
        IF (IPG == NPG) THEN
          IF (DMG_FLAG == 0) THEN   ! element is deleted immediately after fail crit is reached
            DO IEL=1,NEL
              IF (OFF(IEL) == ONE) THEN
                COUNTPG = 0
                DO IG=1,IPG
                  JPG  = (IG-1)*NEL
                  COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)
                ENDDO
                IF (COUNTPG == 0) THEN  ! all Gauss pts have failed
                  OFF(IEL) = FOUR_OVER_5
                ENDIF
              ENDIF
            ENDDO  ! IEL=1,NEL
          ELSE   ! DMG_FLAG = 1 => Add relaxation time with exp damage before deleting element
            DO IEL=1,NEL
              IF (OFF(IEL) == ONE) THEN
                COUNTPG = 0
                DO IG=1,IPG
                  JPG  = (IG-1)*NEL
                  COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)
                ENDDO
                IF (COUNTPG == 0) THEN  ! all Gauss pts have failed
                  OFF(IEL) = ONE - EM6
                  TFAIL(IEL) = TIME
                ENDIF
              ENDIF
            ENDDO  ! IEL=1,NEL
            DO IEL=1,NEL
              IF (TFAIL(IEL) > ZERO) THEN
                DMG_SCALE(IEL) = EXP(-(TIME - TFAIL(IEL))/TRELAX)
              END IF
            ENDDO  ! IEL=1,NEL
          END IF   ! DMG_FLAG
        ENDIF      ! IPG == NPG
c----------------------------------------
      ENDIF       ! PROPERTY TYPE
c-------------------------------
 2000 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10)
 2100 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10,
     .       1X,'AT TIME :',G11.4)
c-----------
      RETURN
      END
