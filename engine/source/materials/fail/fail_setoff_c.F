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
Chd|  FAIL_SETOFF_C                 source/materials/fail/fail_setoff_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_SETOFF_C(ELBUF_STR,GEO      ,PID      ,NGL      ,
     .                         NEL      ,NLAY     ,NPTTOT   ,PTHKF    ,
     .                         THK_LY   ,THKLY    ,OFF      ,STACK    ,
     .                         ISUBSTACK,IGTYP    ,FAILWAVE ,FWAVE_EL )
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
#include "com08_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: NEL,NPTTOT,NLAY,PID,IGTYP,
     .   ISUBSTACK
      INTEGER, DIMENSION(NEL) :: NGL,FWAVE_EL
      my_real, DIMENSION(NPTTOT*NEL) :: THKLY
      my_real, DIMENSION(NPROPG,*) :: GEO
      my_real, DIMENSION(NLAY,*)   :: PTHKF
      my_real, DIMENSION(NEL   )   :: OFF
      my_real, DIMENSION(NEL,*)    :: THK_LY
      TYPE(ELBUF_STRUCT_), TARGET  :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IEL,IPOS,IL,IFL,IP,IPT,IG,NPTT,
     .   IDMG,NINDXLY,IPT_ALL,NFAIL,IPWEIGHT,IPTHKLY
      INTEGER, DIMENSION(NEL) :: NPTF,INDXLY  
      INTEGER, DIMENSION(NEL,NLAY) :: OFFLY
      INTEGER, DIMENSION(:), POINTER :: FOFF,LAY_OFF
      INTEGER, DIMENSION(10) :: ISTRESS
      my_real, DIMENSION(NEL) :: UEL1,DFMAX,TDEL,NPTTF,SIGSCALE
      my_real, DIMENSION(10) :: P_THICK,PTHKF1
      my_real, DIMENSION(NLAY) :: WEIGHT,P_THKLY
      my_real :: DMG,RESID_DMG,THK_LAY,P_THICKG,FAIL_EXP,DFAIL
      my_real, DIMENSION(NEL) :: THFACT,NORM
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
c-----------------------------------------------------------------------
c     NPTT       NUMBER OF INTEGRATION POINTS IN CURRENT LAYER
c     NPTTF      NUMBER OF FAILED INTEGRATION POINTS IN THE LAYER
c     NPTTOT     NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
c     THK_LY     Ratio of layer thickness / element thickness
c     THK        Total element thickness
C=======================================================================
      RESID_DMG = ZERO
      IPTHKLY   = 700
      IPWEIGHT  = 900
      P_THICKG  = GEO(42,PID)
      FAIL_EXP  = GEO(43,PID)     
c                                                       
c------------------------------------
      IF (NLAY == 1) THEN   ! PID 1,9
c------------------------------------
        IL = 1
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
        NPTT  = ELBUF_STR%BUFLY(IL)%NPTT        
c
        IF (NFAIL == 1 .and. P_THICKG > ZERO) THEN
          PTHKF1(1) = MAX(MIN(P_THICKG,ONE-EM06),EM06)
        ELSE
          DO IFL = 1,NFAIL
            PTHKF1(IFL) = MAX(MIN(PTHKF(IL,IFL),ONE-EM06),EM06)
          ENDDO
        ENDIF
c------------------
        DO IFL = 1,NFAIL
          THFACT(1:NEL) = ZERO                                                  
          DO IPT=1,NPTT                                                         
            FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
            DO IEL=1,NEL                                                        
              IF (OFF(IEL) /= ZERO) THEN                                        
                IF (FOFF(IEL) < ONE)  THEN     ! FOFF(IEL)<1                     
                  IPOS = (IPT-1)*NEL + IEL                                      
                  THFACT(IEL) = THFACT(IEL) + THKLY(IPOS)
                ENDIF                                                           
              ENDIF   ! OFF(IEL)/=0                                             
            ENDDO   ! IEL = 1-->NEL                                             
          ENDDO   ! IPT = 1-->NPTT                                              
          DO IEL=1,NEL                                                          
            IF (OFF(IEL) /= ZERO) THEN                                        
              IF (THFACT(IEL) >= PTHKF1(IFL)) THEN  ! delete element          
                OFF(IEL) = FOUR_OVER_5
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1 ! set frontwave flag                      
              ENDIF   ! THFACT>=PTHKF                                           
            ENDIF                                                           
          ENDDO   ! IEL = 1-->NEL                                               
        ENDDO     ! NFAIL
c---------------------------------------------------
      ELSEIF (NLAY == NPTTOT) THEN  ! PID 10,11,16,17 may be 51 and 52
c---------------------------------------------------
        IPT = 1
c       check old Ishell settings
c
        IF (P_THICKG > ZERO) THEN
          P_THICKG = MAX(MIN(P_THICKG, ONE-EM06), EM06)
        ELSE
          P_THICKG = ONE-EM06
          DO IL=1,NLAY
            DO IFL = 1,ELBUF_STR%BUFLY(IL)%NFAIL
              IF (PTHKF(IL,IFL) > ZERO) THEN
                P_THICKG = MIN(P_THICKG, PTHKF(IL,IFL))
              ENDIF
            ENDDO
          ENDDO
          P_THICKG = MAX(P_THICKG, EM06)
        ENDIF
c
        DO IL=1,NLAY
          NINDXLY  = 0
          NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
          DO IFL = 1,NFAIL
            FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
            DO IEL=1,NEL
              IF (OFF(IEL) == ONE .and. LAY_OFF(IEL) == 1) THEN
                IF (FOFF(IEL) < 1)  THEN
                  NINDXLY = NINDXLY + 1    
                  INDXLY(NINDXLY) = IEL    
                  LAY_OFF(IEL) = 0  ! layer is off
                ENDIF ! FOFF < 1
              ENDIF ! OFF==1 && ELBUF_STR%BUFLY(IL)%OFF(IEL) == 1
            ENDDO ! IEL = 1-->NEL
          ENDDO
c
          IF (NINDXLY > 0) THEN                   
            DO I = 1,NINDXLY                      
#include      "lockon.inc"                           
              WRITE(IOUT, 2000) IL,NGL(INDXLY(I))    
              WRITE(ISTDO,2100) IL,NGL(INDXLY(I)),TT 
#include      "lockoff.inc"                          
            ENDDO
          ENDIF
        ENDDO      ! IL=1-->NLAY
c---------------------
        IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
          IPTHKLY  = 1 + 4*NLAY 
          IPWEIGHT = IPTHKLY + NLAY
          THFACT(1:NEL) = ZERO
          NORM(1:NEL)   = ZERO
c
          DO IL=1,NLAY
            LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
            WEIGHT(IL) = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)
            II = (IL-1)*NEL
            DO IEL =1,NEL
              IF (OFF(IEL) == ONE) THEN
                IPOS  = II + IEL
                DFAIL = THKLY(IPOS)*WEIGHT(IL)
                NORM(IEL)  = NORM(IEL)  + DFAIL
                IF (LAY_OFF(IEL) == 0) THEN
                  THFACT(IEL) = THFACT(IEL) + THKLY(IPOS)*WEIGHT(IL)
                ENDIF
              ENDIF   ! OFF == 1
            ENDDO   ! IEL=1-->NEL
          ENDDO        ! IL=1-->NLAY
c
          DO IEL =1,NEL
            IF (OFF(IEL) == ONE) THEN
              IF (THFACT(IEL) >= P_THICKG*NORM(IEL)) THEN      ! delete element
                OFF(IEL) = FOUR_OVER_5                                            
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1  ! set frontwave flag                      
              ENDIF   ! THFACT>= P_T*NORM
            ENDIF   ! OFF == 1
          ENDDO     ! IEL=1-->NEL
c---------------------
        ELSE ! igtyp =10,11
c
          THFACT(1:NEL) = ZERO
          NORM(1:NEL)   = ZERO
          DO IL=1,NLAY
            WEIGHT(IL) = GEO(IPWEIGHT + IL,PID)
            LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
            II = (IL-1)*NEL
            DO IEL=1,NEL
              IF (OFF(IEL) == ONE) THEN
                IPOS = II + IEL
                DFAIL = THKLY(IPOS)*WEIGHT(IL)
                NORM(IEL)  = NORM(IEL)  + DFAIL
                IF (LAY_OFF(IEL) == 0) THEN
                  THFACT(IEL) = THFACT(IEL) + THKLY(IPOS)*WEIGHT(IL)
                ENDIF
              ENDIF   ! OFF = 1
            ENDDO   ! IEL=1-->NEL
          ENDDO   ! IL=1-->NLAY
c
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              IF (THFACT(IEL) >= P_THICKG*NORM(IEL)) THEN      ! delete element      
                OFF(IEL) = FOUR_OVER_5                                            
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1   ! set frontwave propagation flag                                
              ENDIF
            ENDIF   ! OFF=1
          ENDDO   ! IEL=1-->NEL
c
        ENDIF  ! IGTYP
c------------------------------------------
      ELSE  ! NPTT per layer > 1 <=> PROP51...
c------------------------------------------
        IPT_ALL = 0
        IPTHKLY  = 1 + 4*NLAY 
        IPWEIGHT = IPTHKLY + NLAY
c
        DO IL=1,NLAY
          NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
          NINDXLY  = 0                                                
          NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL               
          P_THKLY(IL) = STACK%GEO(IPTHKLY + IL,ISUBSTACK)          
          WEIGHT(IL)  = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
          II = (IL-1)*NEL
c
          DO IFL = 1,NFAIL
            THFACT(1:NEL) = ZERO
            DO IPT=1,NPTT
              FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
              DO IEL=1,NEL                                                        
                IF (OFF(IEL) == ONE) THEN
                  IF (LAY_OFF(IEL) == 1) THEN
                    IF (FOFF(IEL) < ONE)  THEN
                      IP   = IPT_ALL + IPT
                      IPOS = II + IEL
                      THFACT(IEL) = THFACT(IEL) + THKLY(IPOS)/THK_LY(IEL,IL)
                    ENDIF     ! FOFF<1
                  ENDIF       ! ELBUF_S = 1
                ENDIF ! OFF=1
              ENDDO   ! IEL=1-->NEL
            ENDDO   ! IPT=1-->NPTT
c
            DO IEL=1,NEL
              IF(OFF(IEL) == ONE) THEN
                IF (THFACT(IEL) >= P_THKLY(IL)) THEN
                  NINDXLY = NINDXLY + 1                                     
                  INDXLY(NINDXLY) = IEL                                     
                  LAY_OFF(IEL) = 0  ! layer off     
                  DO IPT=1,NPTT
                    FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
                    FOFF(IEL) = 0
                  ENDDO
                ENDIF
              ENDIF
            ENDDO   ! IEL=1-->NEL
          ENDDO   ! IFL = 1,NFAIL
c
          IF (NINDXLY > 0) THEN                                       
            DO I = 1,NINDXLY                                          
#include     "lockon.inc"                                             
              WRITE(IOUT, 2000) IL,NGL(INDXLY(I))                   
              WRITE(ISTDO,2100) IL,NGL(INDXLY(I)),TT                
#include     "lockoff.inc"                                            
            ENDDO                                                   
          ENDIF
          IPT_ALL = IPT_ALL + NPTT
        ENDDO      ! IL=1,NLAY
c-------------------
        P_THICKG = MAX(P_THICKG, EM06)
        P_THICKG = MIN(P_THICKG, ONE-EM06)
c        
        IPTHKLY  = 1 + 4*NLAY 
        IPWEIGHT = IPTHKLY + NLAY
        THFACT(1:NEL) = ZERO
        NORM(1:NEL)   = ZERO
c
        DO IL=1,NLAY
          WEIGHT(IL) = STACK%GEO(IPWEIGHT + IL,ISUBSTACK)
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              DFAIL = (THK_LY(IEL,IL)*WEIGHT(IL))**FAIL_EXP
              NORM(IEL)  = NORM(IEL) + DFAIL
              IF (LAY_OFF(IEL) == 0) THEN
                THFACT(IEL) = THFACT(IEL)+DFAIL
              ENDIF
            ENDIF   ! OFF==1
          ENDDO   ! IEL=1-->NEL
        ENDDO   ! IL=1-->NLAY
c
        DO IEL=1,NEL
          IF (OFF(IEL) == ONE) THEN
            THFACT(IEL) = THFACT(IEL)**(ONE/FAIL_EXP)
            NORM(IEL)   = NORM(IEL)**(ONE/FAIL_EXP)
            IF (THFACT(IEL) >= P_THICKG*NORM(IEL)) THEN      ! delete element
              OFF(IEL) = FOUR_OVER_5
              IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1  ! set frontwave flag                      
            ENDIF
          ENDIF   ! OFF==1
        ENDDO     ! IEL=1-->NEL
c----------------------------------------
      ENDIF       ! PROPERTY TYPE
c-------------------------------
 2000 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10)
 2100 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10,
     .       1X,'AT TIME :',G11.4)
c-----------
      RETURN
      END
