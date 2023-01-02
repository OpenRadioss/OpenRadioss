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
Chd|  FAIL_SETOFF_NPG_C             source/materials/fail/fail_setoff_npg_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_SETOFF_NPG_C(
     .           ELBUF_STR,GEO      ,PID      ,NGL      ,
     .           NEL      ,IR       ,IS       ,NLAY     ,
     .           NPTTOT   ,PTHKF    ,THK_LY   ,THKLY    ,
     .           OFF      ,NPG      ,STACK    ,ISUBSTACK,
     .           IGTYP    ,FAILWAVE ,FWAVE_EL )
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
      INTEGER  :: NEL,NPTTOT,NLAY,PID,IR,IS,NPG,ISUBSTACK,IGTYP
      INTEGER, DIMENSION(NEL) :: NGL,FWAVE_EL
      INTEGER, DIMENSION(:), POINTER :: FOFF,LAY_OFF
      my_real, DIMENSION(NPTTOT*NEL) :: THKLY
      my_real, DIMENSION(NPROPG,*) :: GEO
      my_real, DIMENSION(NLAY,*)   :: PTHKF
      my_real, DIMENSION(NEL   )   :: OFF
      my_real, DIMENSION(NEL,*)    :: THK_LY
      TYPE(ELBUF_STRUCT_), TARGET  :: ELBUF_STR
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IEL,IPOS,IL,IFL,IP,IPT,IG,IPG,JPG,NPTR,NPTS,NPTT,
     .   IDMG,COUNTPG,NINDXPG,NINDXLY,IPT_ALL,NFAIL,IPWEIGHT,IPTHKLY
      INTEGER, DIMENSION(NEL) :: NPTF,INDXPG,INDXLY  
      INTEGER, DIMENSION(NEL,NLAY) :: OFFLY
      INTEGER, DIMENSION(10) :: ISTRESS
      INTEGER, DIMENSION(:), POINTER :: OFFPG
      my_real, DIMENSION(NEL) :: UEL1,DFMAX,TDEL,NPTTF,SIGSCALE
      my_real, DIMENSION(:), POINTER :: OFFL
      my_real, DIMENSION(NLAY) :: WEIGHT,P_THKLY
      my_real :: DMG,RESID_DMG,THK_LAY,P_THICKG,FAIL_EXP,THFACT,NORM,DFAIL
      TYPE(L_BUFEL_) ,POINTER :: LBUF 
      TYPE (STACK_PLY) :: STACK    
c-----------------------------------------------------------------------
c     NPTT       NUMBER OF INTEGRATION POINTS IN CURRENT LAYER
c     NPTTF      NUMBER OF FAILED INTEGRATION POINTS IN THE LAYER
c     NPTTOT     NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
c     OFFPG(NEL,NPG)  failure flag of PG in each layer  1=alive ,0=dead 
c     THK_LY     Ratio of layer thickness / element thickness
c     THK        Total element thickness
C=======================================================================
      RESID_DMG = ZERO
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
      IF (NLAY == 1) THEN   ! PID 1,9
c------------------------------------
        IL = 1
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
        NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
        OFFPG => ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)
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
        DO IEL=1,NEL
          IF (OFF(IEL) == ZERO .or. OFFPG(IEL) == 0) CYCLE
          DO IFL = 1,NFAIL
            THFACT = ZERO
            DO IPT=1,NPTT
              FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF
c              OFFL => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IPT)%OFF
c              IF (OFFL(IEL) == ONE .and. FOFF(IEL) < 1) OFFL(IEL) = FOFF(IEL)
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
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              COUNTPG = 0
              DO IG=1,IPG
                JPG  = (IG-1)*NEL
                COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)
              ENDDO
              IF (COUNTPG == 0) THEN  ! all Gauss pts are failed
                OFF(IEL) = FOUR_OVER_5          
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1   ! set frontwave propagation flag                                
              ENDIF
            ENDIF
          ENDDO  ! IEL=1,NEL
        ENDIF
c---------------------------------------------------
      ELSEIF (NLAY == NPTTOT) THEN   ! PID 10,11,16,17
c---------------------------------------------------
        IPT = 1
c       check old Ishell settings
        IF (P_THICKG > ZERO) THEN
          P_THICKG = MAX(P_THICKG, EM06)
          P_THICKG = MIN(P_THICKG, ONE-EM06)
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
c-------
        IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN 
          IPTHKLY  = 1 + 4*NLAY                                                   
          IPWEIGHT = IPTHKLY + NLAY                                               
          DO IL=1,NLAY                                                            
            NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL                                     
            OFFPG =>ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)                      
            WEIGHT(IL) = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)                        
            NINDXPG = 0                                                           
            DO IEL=1,NEL                                                          
              IF (OFF(IEL) == ONE .and. OFFPG(IEL) == 1 .and.                      
     .            ELBUF_STR%BUFLY(IL)%OFF(IEL) == 1) THEN                         
                DO IFL = 1,NFAIL                                                  
                  FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF       
                  OFFL => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IPT)%OFF                 
                  IF (FOFF(IEL) < ONE)  THEN                                       
                    OFFPG(IEL) = 0            ! PG per layer                      
                    NINDXPG = NINDXPG + 1                                         
                    INDXPG(NINDXPG) = IEL                                         
                  ENDIF                                                           
                ENDDO    ! IFL = 1,NFAIL                                          
              ENDIF                                                               
            ENDDO     ! IEL=1,NEL                                                 
            IF (IPG == NPG) THEN                                                  
              NINDXLY  = 0                                                       
              DO IEL=1,NEL                                                       
                IF (OFF(IEL) == ONE) THEN                                          
                  IF (ELBUF_STR%BUFLY(IL)%OFF(IEL) == 1) THEN                     
                    COUNTPG = 0                                                   
                    DO IG=1,NPG                                                   
                      JPG  = (IG-1)*NEL                                           
                      COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)      
                    ENDDO                                                         
                    IF (COUNTPG == 0) THEN          ! all Gauss pts failed        
                      NINDXLY = NINDXLY + 1                                       
                      INDXLY(NINDXLY) = IEL                                       
                      ELBUF_STR%BUFLY(IL)%OFF(IEL) = 0  ! layer is off            
                    ENDIF                                                         
                  ENDIF                                                           
                ENDIF                                                             
              ENDDO  ! IEL=1,NEL                                                    
c
              IF (NINDXLY > 0) THEN                        
                DO I = 1,NINDXLY                           
#include       "lockon.inc"                                
                  WRITE(IOUT, 2000) IL,NGL(INDXLY(I))      
                  WRITE(ISTDO,2100) IL,NGL(INDXLY(I)),TT   
#include       "lockoff.inc"                               
                ENDDO                                      
              ENDIF                                        
            ENDIF ! IPG == NPG
          ENDDO      ! IL=1,NLAY  
C
          DO IEL=1,NEL                                                                    
            IF (OFF(IEL) == ONE) THEN                                                      
              THFACT = ZERO                                                               
              NORM   = ZERO                                                               
              DO IL=1,NLAY                                                                
                IPOS = (IL-1)*NEL + IEL                                                   
                WEIGHT(IL) = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)                            
                DFAIL = THKLY(IPOS)*WEIGHT(IL)                                            
                NORM  = NORM  + DFAIL                                                     
                IF (OFF(IEL) == ONE .and. ELBUF_STR%BUFLY(IL)%OFF(IEL) == 0) THEN          
                  THFACT = THFACT + THKLY(IPOS)*WEIGHT(IL)                                
                ENDIF                                                                     
              ENDDO                                                                       
              IF (THFACT >= P_THICKG*NORM) THEN      ! delete element                     
                OFF(IEL) = FOUR_OVER_5                                                           
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1   ! set frontwave propagation flag                                  
              ENDIF                                                                       
            ENDIF                                                                         
          ENDDO     ! IEL=1,NEL                                                           
C                         
        ELSE ! IGTP=10, 11
          DO IL=1,NLAY                                                        
            NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL                                 
            OFFPG =>ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)                  
            WEIGHT(IL) = GEO(IPWEIGHT + IL,PID)                               
            NINDXPG = 0                                                       
            DO IEL=1,NEL                                                      
              IF (OFF(IEL) == ONE .and. OFFPG(IEL) == 1 .and.                  
     .            ELBUF_STR%BUFLY(IL)%OFF(IEL) == 1) THEN                     
                DO IFL = 1,NFAIL                                              
                  FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF   
                  OFFL => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IPT)%OFF             
                  IF (FOFF(IEL) < ONE)  THEN                                   
                    OFFPG(IEL) = 0            ! PG per layer                  
                    NINDXPG = NINDXPG + 1                                     
                    INDXPG(NINDXPG) = IEL                                     
                  ENDIF                                                       
                ENDDO    ! IFL = 1,NFAIL                                      
              ENDIF                                                           
            ENDDO     ! IEL=1,NEL                                             
c
            IF (IPG == NPG) THEN                                               
              NINDXLY  = 0                                                     
              DO IEL=1,NEL                                                     
                IF (OFF(IEL) == ONE) THEN                                       
                  IF (ELBUF_STR%BUFLY(IL)%OFF(IEL) == 1) THEN                  
                    COUNTPG = 0                                                
                    DO IG=1,NPG                                                
                      JPG  = (IG-1)*NEL                                        
                      COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)   
                    ENDDO                                                      
                    IF (COUNTPG == 0) THEN          ! all Gauss pts failed     
                      NINDXLY = NINDXLY + 1                                    
                      INDXLY(NINDXLY) = IEL                                    
                      ELBUF_STR%BUFLY(IL)%OFF(IEL) = 0  ! layer is off         
                    ENDIF                                                      
                  ENDIF                                                        
                ENDIF                                                          
              ENDDO  ! IEL=1,NEL                                               
c
              IF (NINDXLY > 0) THEN                                           
                DO I = 1,NINDXLY                                              
#include      "lockon.inc"                                                    
                 WRITE(IOUT, 2000) IL,NGL(INDXLY(I))                          
                 WRITE(ISTDO,2100) IL,NGL(INDXLY(I)),TT                       
#include      "lockoff.inc"                                                   
                ENDDO                                                         
              ENDIF                                                           
            ENDIF ! IPG == NPG                                               
          ENDDO      ! IL=1,NLAY                                             
C
          DO IEL=1,NEL                                                               
            IF (OFF(IEL) == ONE) THEN                                                 
              THFACT = ZERO                                                          
              NORM   = ZERO                                                          
              DO IL=1,NLAY                                                           
                IPOS = (IL-1)*NEL + IEL                                              
                WEIGHT(IL) = GEO(IPWEIGHT + IL,PID)                                  
                DFAIL = THKLY(IPOS)*WEIGHT(IL)                                       
                NORM  = NORM  + DFAIL                                                
                IF (OFF(IEL) == ONE .and. ELBUF_STR%BUFLY(IL)%OFF(IEL) == 0) THEN     
                  THFACT = THFACT + THKLY(IPOS)*WEIGHT(IL)                           
                ENDIF                                                                
              ENDDO                                                                  
              IF (THFACT >= P_THICKG*NORM) THEN      ! delete element                
                OFF(IEL) = FOUR_OVER_5                                                      
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1   ! set frontwave propagation flag                                
              ENDIF                                                                  
            ENDIF                                                                    
          ENDDO     ! IEL=1,NEL                                                      
C        
        ENDIF ! igtyp  
c------------------------------------------
      ELSE  ! NPTT per layer > 1 <=> PROP51...
c------------------------------------------
      IPT_ALL = 0
      IPTHKLY  = 1 + 4*NLAY 
      IPWEIGHT = IPTHKLY + NLAY
      DO IL=1,NLAY
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL                 
        OFFPG =>ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)  
        NPTT  = ELBUF_STR%BUFLY(IL)%NPTT                 
        P_THKLY(IL) = STACK%GEO(IPTHKLY  + IL,ISUBSTACK)            
        WEIGHT(IL)  = STACK%GEO(IPWEIGHT + IL,ISUBSTACK)            
        NINDXPG = 0 
c
        DO IEL=1,NEL
          IF (OFF(IEL) == ZERO .or. OFFPG(IEL) == 0 .or. 
     .        ELBUF_STR%BUFLY(IL)%OFF(IEL) == 0) CYCLE
          DO IFL = 1,NFAIL
            THFACT = ZERO
            DO IPT=1,NPTT
              FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF
              OFFL => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IPT)%OFF
              IF (FOFF(IEL) < ONE)  THEN
                IP   = IPT_ALL + IPT
                IPOS = (IP-1)*NEL + IEL
                THFACT = THFACT + THKLY(IPOS)/THK_LY(IEL,IL)
              ENDIF
c
              IF (THFACT >= P_THKLY(IL)) THEN  ! delete current PG in the layer
                NINDXPG = NINDXPG + 1
                INDXPG(NINDXPG) = IEL
                OFFPG(IEL) = 0
              ENDIF 
            ENDDO     ! IPT=1,NPTT        
          ENDDO      ! IEL=1,NEL
        ENDDO       ! IFL = 1,NFAIL
        IPT_ALL = IPT_ALL + NPTT
      ENDDO      ! IL=1,NLAY
c
c-----------
C
        IF (IPG == NPG) THEN
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              DO IL=1,NLAY
                NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL                 
                LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
                NINDXLY  = 0
                IF (LAY_OFF(IEL) == 1) THEN                 
                  COUNTPG = 0                                               
                  DO IG=1,NPG                                               
                    JPG  = (IG-1)*NEL                                       
                    COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)  
                  ENDDO                                                     
                  IF (COUNTPG == 0) THEN          ! all Gauss pts failed    
                    NINDXLY = NINDXLY + 1                                   
                    INDXLY(NINDXLY) = IEL                                   
                    LAY_OFF(IEL) = 0  ! layer is off        
                    NPTR  = ELBUF_STR%NPTR             
                    NPTS  = ELBUF_STR%NPTS             
                    NPTT  = ELBUF_STR%BUFLY(IL)%NPTT             
                    DO IFL = 1,NFAIL
                      DO IR=1,NPTR
                      DO IS=1,NPTS
                      DO IPT=1,NPTT
                        FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF
                        FOFF(IEL) = 0
                      ENDDO
                      ENDDO
                      ENDDO
                    ENDDO
                  ENDIF                                                     
                ENDIF                                                       
c-----------
                IF (NINDXLY > 0) THEN                   
                  DO I = 1,NINDXLY                      
#include         "lockon.inc"                           
                    WRITE(IOUT, 2000) IL,NGL(INDXLY(I))    
                    WRITE(ISTDO,2100) IL,NGL(INDXLY(I)),TT 
#include         "lockoff.inc"                          
                  ENDDO
                ENDIF
c-----------
              ENDDO      ! IL=1,NLAY
            ENDIF
          ENDDO    ! IEL=1,NEL
c
c-------------------
         P_THICKG = MAX(P_THICKG, EM06)
         P_THICKG = MIN(P_THICKG, ONE-EM06)
c      
         IPTHKLY  = 1 + 4*NLAY 
         IPWEIGHT = IPTHKLY + NLAY
         DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              THFACT = ZERO
              NORM   = ZERO
              DO IL=1,NLAY
                WEIGHT(IL) = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)
                DFAIL = (THK_LY(IEL,IL)*WEIGHT(IL))**FAIL_EXP
                NORM  = NORM + DFAIL
                IF (ELBUF_STR%BUFLY(IL)%OFF(IEL) == 0) THEN
                  THFACT = THFACT + DFAIL
                ENDIF 
              ENDDO      ! IL=1,NLAY
              THFACT = THFACT**(ONE/FAIL_EXP)
              NORM   = NORM**(ONE/FAIL_EXP)
              IF (THFACT >= P_THICKG*NORM) THEN      ! delete element
                OFF(IEL) = FOUR_OVER_5
              ENDIF
            ENDIF
          ENDDO     ! IEL=1,NEL
c-------------------
        ENDIF     ! IPG == NPG

c----------------------------------------
      ENDIF       ! PROPERTY TYPE
c-------------------------------
 2000 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10)
 2100 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10,
     .       1X,'AT TIME :',G11.4)
c-----------
      RETURN
      END
