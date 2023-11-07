Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_SETOFF_NPG_C(
     .           ELBUF_STR,MAT_ELEM ,GEO      ,PID      ,
     .           NGL      ,NEL      ,IR       ,IS       ,
     .           NLAY     ,NPTTOT   ,THK_LY   ,THKLY    ,
     .           OFF      ,NPG      ,STACK    ,ISUBSTACK,
     .           IGTYP    ,FAILWAVE ,FWAVE_EL ,NLAY_MAX ,
     .           LAYNPT_MAX,NUMGEO  ,IPG      ,NUMSTACK ,
     .           IGEO     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE STACK_MOD
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
      TYPE(ELBUF_STRUCT_), INTENT(INOUT), TARGET    :: ELBUF_STR
      my_real, DIMENSION(NPROPG,NUMGEO), INTENT(IN) :: GEO
      INTEGER, DIMENSION(NPROPGI,NUMGEO),INTENT(IN) :: IGEO
      INTEGER, INTENT(IN) :: PID,NEL,IR,IS,NLAY,NPTTOT,NPG,IGTYP,
     .                       ISUBSTACK,NLAY_MAX,LAYNPT_MAX,NUMGEO,
     .                       IPG,NUMSTACK
      INTEGER, DIMENSION(NEL), INTENT(IN)           :: NGL
      my_real, DIMENSION(NEL,NLAY_MAX*LAYNPT_MAX), INTENT(IN) :: THK_LY
      my_real, DIMENSION(NPTTOT*NEL), INTENT(IN)    :: THKLY
      my_real, DIMENSION(NEL), INTENT(INOUT)        :: OFF
      TYPE (STACK_PLY), INTENT(IN)                  :: STACK    
      TYPE (FAILWAVE_STR_), INTENT(IN), TARGET      :: FAILWAVE 
      INTEGER, DIMENSION(NEL), INTENT(INOUT)        :: FWAVE_EL
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IEL,IPOS,IL,IFL,IP,IPT,IG,JPG,NPTR,NPTS,NPTT,
     .   COUNTPG,NINDXPG,NINDXLY,IPT_ALL,NFAIL,IPWEIGHT,IPTHKLY,
     .   IPS,IPR,ID_PLY,IMAT
      my_real :: P_THICKG,FAIL_EXP,THFACT,NORM,DFAIL,NPFAIL
      my_real, DIMENSION(NLAY,100)   :: PTHKF
      INTEGER, DIMENSION(NEL)        :: INDXPG,INDXLY
      INTEGER, DIMENSION(:), POINTER :: FOFF,LAY_OFF,OFFPG
      my_real, DIMENSION(NLAY)       :: WEIGHT,P_THKLY
      TYPE(L_BUFEL_) ,POINTER        :: LBUF 
c-----------------------------------------------------------------------
c     NPTT       NUMBER OF INTEGRATION POINTS IN CURRENT LAYER
c     NPTTF      NUMBER OF FAILED INTEGRATION POINTS IN THE LAYER
c     NPTTOT     NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
c     OFFPG(NEL,NPG)  failure flag of PG in each layer  1=alive ,0=dead 
c     THK_LY     Ratio of layer thickness / element thickness
c     THK        Total element thickness
C=======================================================================
c 
      !=================================================================
      ! RECOVER PARAMETERS AND INITIALIZATION
      !=================================================================
      P_THICKG  = GEO(42,PID)
      FAIL_EXP  = GEO(43,PID)
      IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN 
        IPTHKLY  = 1 + 4*NLAY     ! Address of PTHKLY in STACK%GEO table
        IPWEIGHT = IPTHKLY + NLAY ! Address of WEIGHT in STACK%GEO table
      ELSE
        IPTHKLY  = 700 ! Address of PTHKLY in GEO table
        IPWEIGHT = 900 ! Address of WEIGHT in GEO table
      ENDIF
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      JPG  = (IPG-1)*NEL
c         
      DO IL=1,NLAY
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
        IMAT  = ELBUF_STR%BUFLY(IL)%IMAT
        DO IFL = 1,NFAIL
          PTHKF(IL,IFL) = MAT_ELEM%MAT_PARAM(IMAT)%FAIL(IFL)%PTHK
        END DO
      END DO
      !=================================================================
      ! 1 LAYER PROPERTIES - TYPE1/TYPE9
      !=================================================================
      IF (NLAY == 1) THEN
c
        ! Only 1 layer with several integration points
        IL = 1
c
        ! Check PTHICKFAIL coming from failure criteria
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
        DO IFL = 1,NFAIL
          ! -> Percentage of broken thickness 
          IF (PTHKF(IL,IFL) > ZERO) THEN 
            PTHKF(IL,IFL) = MIN(PTHKF(IL,IFL),ABS(P_THICKG))
            PTHKF(IL,IFL) = MAX(MIN(PTHKF(IL,IFL),ONE-EM06),EM06)
          ! -> Ratio of broken integration points
          ELSEIF (PTHKF(IL,IFL) < ZERO) THEN
            PTHKF(IL,IFL) = MAX(PTHKF(IL,IFL),-ABS(P_THICKG))
            PTHKF(IL,IFL) = MIN(MAX(PTHKF(IL,IFL),-ONE+EM6),-EM06)
          ! -> If not defined in the failure criterion, the value of the property is used by default
          ELSE 
            PTHKF(IL,IFL) = P_THICKG
          ENDIF
        ENDDO ! |-> IFL
c
        ! Check in-plane Gauss point failure
        NPTT = ELBUF_STR%BUFLY(IL)%NPTT
        OFFPG => ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)
        DO IEL=1,NEL
          IF (OFF(IEL) == ONE .AND. OFFPG(IEL) == 1) THEN 
            DO IFL = 1,NFAIL
              THFACT = ZERO
              NPFAIL = ZERO
              DO IPT=1,NPTT
                FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF
                ! Computation of broken fraction of thickness / 
                ! ratio of intg. points                
                IF (FOFF(IEL) < 1)  THEN
                  IPOS = (IPT-1)*NEL + IEL
                  THFACT = THFACT + THKLY(IPOS)
                  NPFAIL = NPFAIL + ONE/NPTT
                ENDIF
                ! Comparison with critical value PTHICKFAIL
                IF (((THFACT >= PTHKF(IL,IFL)).AND.(PTHKF(IL,IFL) > ZERO)).OR.
     .              ((NPFAIL >= ABS(PTHKF(IL,IFL))).AND.(PTHKF(IL,IFL) < ZERO))) THEN
                  OFFPG(IEL) = 0
                ENDIF 
              ENDDO ! |-> IPT    
            ENDDO ! |-> IFL
          ENDIF
        ENDDO ! |-> IEL
        ! Printing out Gauss point failure message
        NINDXPG = 0
        DO IEL=1,NEL
          IF (OFFPG(IEL) == 0) THEN
            NINDXPG = NINDXPG + 1
            INDXPG(NINDXPG) = IEL
          ENDIF
        ENDDO ! |-> IEL
c 
        ! Check element failure (when IPG = NPG = 4)
        IF (IPG == NPG) THEN
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              COUNTPG = 0
              DO IG=1,NPG
                JPG  = (IG-1)*NEL
                COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)
              ENDDO ! |-> IG
              IF (COUNTPG == 0) THEN
                OFF(IEL) = FOUR_OVER_5          
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1
              ENDIF
            ENDIF
          ENDDO ! |-> IEL
        ENDIF
c
      !=================================================================
      ! MULTI LAYER PROPERTIES / 1 INTG POINT - TYPE10/11/16/17/51/52
      !=================================================================
      ELSEIF (NLAY == NPTTOT) THEN
c
        ! Only one integration points in each layer
        IPT = 1
c
        ! Check in-plane Gauss point failure for each layers                                           
        DO IL=1,NLAY
          NINDXPG = 0                                                          
          NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF                                     
          OFFPG =>ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)                                             
          DO IEL=1,NEL                                                          
            IF (OFF(IEL) == ONE .AND. OFFPG(IEL) == 1 .AND. LAY_OFF(IEL) == 1) THEN                         
              DO IFL = 1,NFAIL                                                  
                FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF       
                IF (FOFF(IEL) < 1)  THEN  
                  NINDXPG = NINDXPG + 1                                         
                  INDXPG(NINDXPG) = IEL                                     
                  OFFPG(IEL) = 0                 
                ENDIF                                                           
              ENDDO ! |-> IFL
            ENDIF
          ENDDO ! |-> IEL
          ! Check layer failure only if IPG = NPG = 4
          IF (IPG == NPG) THEN                                                  
            NINDXLY  = 0   
            LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF                                                    
            DO IEL = 1,NEL                                                       
              IF (OFF(IEL) == ONE) THEN                                          
                IF (LAY_OFF(IEL) == 1) THEN                     
                  COUNTPG = 0                                                   
                  DO IG=1,NPG                                                   
                    JPG  = (IG-1)*NEL                                           
                    COUNTPG = COUNTPG + ELBUF_STR%BUFLY(IL)%OFFPG(JPG+IEL)      
                  ENDDO  ! |-> NPG                                                        
                  IF (COUNTPG == 0) THEN                
                    NINDXLY = NINDXLY + 1                                       
                    INDXLY(NINDXLY) = IEL                                       
                    LAY_OFF(IEL) = 0         
                  ENDIF                                                         
                ENDIF                                                           
              ENDIF                                                             
            ENDDO ! |-> IEL    
            ! Printing out layer/ply failure message  
            IF (NINDXLY > 0) THEN    
              ! -> Print out ply failure
              IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN 
                IF (IGTYP == 17 .OR. IGTYP == 51) THEN 
                  ID_PLY = IGEO(1,STACK%IGEO(2+IL,ISUBSTACK))
                ELSE 
                  ID_PLY = PLY_INFO(1,STACK%IGEO(2+IL,ISUBSTACK)-NUMSTACK)  
                ENDIF 
                DO I = 1,NINDXLY                           
#include       "lockon.inc"                                
                  WRITE(IOUT, 3000) ID_PLY,NGL(INDXLY(I))      
                  WRITE(ISTDO,3100) ID_PLY,NGL(INDXLY(I)),TT   
#include       "lockoff.inc"                               
                ENDDO ! |-> I 
              ! -> Print out layer failure
              ELSE               
                DO I = 1,NINDXLY                           
#include       "lockon.inc"                                
                  WRITE(IOUT, 2000) IL,NGL(INDXLY(I))      
                  WRITE(ISTDO,2100) IL,NGL(INDXLY(I)),TT   
#include       "lockoff.inc"                               
                ENDDO ! |-> I 
              ENDIF                                   
            ENDIF                                        
          ENDIF
        ENDDO 
c 
        ! Check element failure
        DO IEL=1,NEL                                                                    
          IF (OFF(IEL) == ONE) THEN                                                      
            THFACT = ZERO                                                               
            NORM   = ZERO  
            NPFAIL = ZERO                                                             
            DO IL=1,NLAY 
              IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN                                               
                WEIGHT(IL) = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)
              ELSE
                WEIGHT(IL) = GEO(IPWEIGHT + IL,PID)
              ENDIF
              LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF 
              IPOS = (IL-1)*NEL + IEL               
              DFAIL = THKLY(IPOS)*WEIGHT(IL)                                            
              NORM  = NORM  + DFAIL                                                     
              IF (OFF(IEL) == ONE .AND. LAY_OFF(IEL) == 0) THEN          
                THFACT = THFACT + THKLY(IPOS)*WEIGHT(IL)  
                NPFAIL = NPFAIL + ONE/NLAY                              
              ENDIF                                                                     
            ENDDO ! |-> IL                                                                      
            IF (((THFACT >= P_THICKG*NORM).AND.(P_THICKG > ZERO)).OR.
     .          ((NPFAIL >= ABS(P_THICKG)).AND.(P_THICKG < ZERO))) THEN
              OFF(IEL) = FOUR_OVER_5                                                           
              IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1                                 
            ENDIF                                                                       
          ENDIF                                                                         
        ENDDO ! |-> IEL                                                          
c
      !=======================================================================
      ! MULTI LAYER PROPERTIES / SEVERAL INTG. POINTS - TYPE51/TYPE52
      !=======================================================================
      ELSE
c
        ! Several integration points in each layer
        IPT_ALL = 0
c
        ! Check PTHICKFAIL coming from failure criteria
        DO IL = 1,NLAY
          NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
          P_THKLY(IL) = STACK%GEO(IPTHKLY + IL,ISUBSTACK)
          DO IFL = 1,NFAIL
            ! -> Percentage of broken thickness 
            IF (PTHKF(IL,IFL) > ZERO) THEN 
              PTHKF(IL,IFL) = MIN(PTHKF(IL,IFL),ABS(P_THKLY(IL)))
              PTHKF(IL,IFL) = MAX(MIN(PTHKF(IL,IFL),ONE-EM06),EM06)
            ! -> Ratio of broken integration points
            ELSEIF (PTHKF(IL,IFL) < ZERO) THEN 
              PTHKF(IL,IFL) = MAX(PTHKF(IL,IFL),-ABS(P_THKLY(IL)))
              PTHKF(IL,IFL) = MIN(MAX(PTHKF(IL,IFL),-ONE+EM6),-EM06)
            ! -> If not defined in the failure criterion, the value of the property is used by default
            ELSE 
              PTHKF(IL,IFL) = P_THKLY(IL)
            ENDIF
          ENDDO ! |-> IFL
        ENDDO ! |-> IL
c
        ! Check in-plane Gauss point failure for each layers                                           
        DO IL=1,NLAY
          NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
          NINDXPG = 0     
          NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL       
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF          
          OFFPG =>ELBUF_STR%BUFLY(IL)%OFFPG(JPG+1:JPG+NEL)  
          WEIGHT(IL)  = STACK%GEO(IPWEIGHT + IL,ISUBSTACK)            
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE .AND. OFFPG(IEL) == 1 .AND. LAY_OFF(IEL) == 1) THEN 
              DO IFL = 1,NFAIL
                THFACT = ZERO
                NPFAIL = ZERO
                DO IPT = 1,NPTT
                  FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IPT)%FLOC(IFL)%OFF
                  IF (FOFF(IEL) < ONE)  THEN
                    IP   = IPT_ALL + IPT
                    IPOS = (IP-1)*NEL + IEL
                    THFACT = THFACT + THKLY(IPOS)/THK_LY(IEL,IL)
                    NPFAIL = NPFAIL + ONE/NPTT
                  ENDIF
                  IF (((THFACT >= PTHKF(IL,IFL)).AND.(PTHKF(IL,IFL)>ZERO)).OR.
     .                ((THFACT >= ABS(PTHKF(IL,IFL))).AND.(PTHKF(IL,IFL)<ZERO))) THEN
                    NINDXPG = NINDXPG + 1
                    INDXPG(NINDXPG) = IEL
                    OFFPG(IEL) = 0
                  ENDIF 
                ENDDO ! |-> IPT       
              ENDDO ! |-> IFL
            ENDIF
          ENDDO ! |-> IEL
          IPT_ALL = IPT_ALL + NPTT
        ENDDO ! |-> IL
        ! Check layer failure only if IPG = NPG = 4 
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
                  ENDDO ! |-> IG                                                   
                  IF (COUNTPG == 0) THEN          ! all Gauss pts failed    
                    NINDXLY = NINDXLY + 1                                   
                    INDXLY(NINDXLY) = IEL                                   
                    LAY_OFF(IEL) = 0             
                    NPTT  = ELBUF_STR%BUFLY(IL)%NPTT             
                    DO IFL = 1,NFAIL
                      DO IPR=1,NPTR
                        DO IPS=1,NPTS
                          DO IPT=1,NPTT
                            FOFF => ELBUF_STR%BUFLY(IL)%FAIL(IPR,IPS,IPT)%FLOC(IFL)%OFF
                            FOFF(IEL) = 0
                          ENDDO ! |-> IPT
                        ENDDO ! |-> IPS
                      ENDDO ! |-> IPR
                    ENDDO ! |-> IFL
                  ENDIF                                                     
                ENDIF                                                       
                ! Printing out ply failure message
                IF (NINDXLY > 0) THEN    
                  IF (IGTYP == 51) THEN 
                    ID_PLY = IGEO(1,STACK%IGEO(2+IL,ISUBSTACK))
                  ELSE 
                    ID_PLY = PLY_INFO(1,STACK%IGEO(2+IL,ISUBSTACK)-NUMSTACK)  
                  ENDIF                     
                  DO I = 1,NINDXLY                      
#include         "lockon.inc"                           
                    WRITE(IOUT, 3000) ID_PLY,NGL(INDXLY(I))    
                    WRITE(ISTDO,3100) ID_PLY,NGL(INDXLY(I)),TT 
#include         "lockoff.inc"                          
                  ENDDO ! |-> I
                ENDIF
              ENDDO ! |-> IL
            ENDIF
          ENDDO ! |-> IEL
c      
          ! Check element failure
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              THFACT = ZERO
              NORM   = ZERO
              NPFAIL = ZERO
              DO IL=1,NLAY
                WEIGHT(IL) = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)
                LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
                DFAIL = (THK_LY(IEL,IL)*WEIGHT(IL))**FAIL_EXP
                NORM  = NORM + DFAIL
                IF (LAY_OFF(IEL) == 0) THEN
                  THFACT = THFACT + DFAIL
                  NPFAIL = NPFAIL + ONE/NLAY
                ENDIF 
              ENDDO ! |-> IL
              THFACT = THFACT**(ONE/FAIL_EXP)
              NORM   = NORM**(ONE/FAIL_EXP)
              IF (((THFACT >= P_THICKG*NORM).AND.(P_THICKG > ZERO)).OR.
     .            ((THFACT >= ABS(P_THICKG)).AND.(P_THICKG < ZERO))) THEN
                OFF(IEL) = FOUR_OVER_5
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1
              ENDIF
            ENDIF
          ENDDO ! |-> IEL
        ENDIF
c
      ENDIF ! IGTYP PROPERTY TYPE
      !=======================================================================
c
      !=======================================================================
      ! PRINTING OUT FORMATS FOR LAYERS/PLYS FAILURE
      !=======================================================================
 2000 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10)
 2100 FORMAT(1X,'-- FAILURE OF LAYER',I3, ' ,SHELL ELEMENT NUMBER ',I10,
     .       1X,'AT TIME :',G11.4)
 3000 FORMAT(1X,'-- FAILURE OF PLY ID ',I10, ' ,SHELL ELEMENT NUMBER ',I10)
 3100 FORMAT(1X,'-- FAILURE OF PLY ID ',I10, ' ,SHELL ELEMENT NUMBER ',I10,
     .       1X,'AT TIME :',G11.4)
      END
