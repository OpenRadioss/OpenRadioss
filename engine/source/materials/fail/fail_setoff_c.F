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
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_SETOFF_C(ELBUF_STR,GEO      ,PID      ,NGL      ,
     .                         NEL      ,NLAY     ,NPTTOT   ,PTHKF    ,
     .                         THK_LY   ,THKLY    ,OFF      ,STACK    ,
     .                         ISUBSTACK,IGTYP    ,FAILWAVE ,FWAVE_EL ,
     .                         NLAY_MAX ,LAYNPT_MAX,NUMGEO  ,NUMSTACK ,
     .                         IGEO     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
      INTEGER,INTENT(IN) :: PID,NEL,NLAY,NPTTOT,IGTYP,ISUBSTACK,
     .                      NLAY_MAX,LAYNPT_MAX,NUMGEO,NUMSTACK
      INTEGER, DIMENSION(NEL), INTENT(IN)           :: NGL
      my_real, DIMENSION(NLAY,100), INTENT(INOUT)   :: PTHKF
      my_real, DIMENSION(NEL,NLAY_MAX*LAYNPT_MAX), INTENT(IN) :: THK_LY
      my_real, DIMENSION(NPTTOT*NEL), INTENT(IN)    :: THKLY
      my_real, DIMENSION(NEL), INTENT(INOUT)        :: OFF
      TYPE (STACK_PLY), INTENT(IN)                  :: STACK
      TYPE (FAILWAVE_STR_), INTENT(IN), TARGET      :: FAILWAVE 
      INTEGER, DIMENSION(NEL), INTENT(INOUT)        :: FWAVE_EL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IEL,IPOS,IL,IFL,IPT,NPTT,
     .        NINDXLY,NFAIL,IPWEIGHT,IPTHKLY,
     .        ID_PLY
      my_real
     .        P_THICKG,FAIL_EXP,DFAIL
      INTEGER, DIMENSION(NEL)        :: INDXLY
      INTEGER, DIMENSION(:), POINTER :: FOFF,LAY_OFF
      my_real, DIMENSION(NLAY)       :: WEIGHT,P_THKLY
      my_real, DIMENSION(NEL)        :: THFACT,NORM,NPFAIL
      TYPE(L_BUFEL_) ,POINTER        :: LBUF     
c-----------------------------------------------------------------------
c     NPTT       NUMBER OF INTEGRATION POINTS IN CURRENT LAYER
c     NPTTOT     NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
c     THK_LY     Ratio of layer thickness / element thickness
c     THK        Total element thickness
C=======================================================================
c 
      !=================================================================
      ! RECOVER PARAMETERS AND INITIALIZATION
      !=================================================================
      P_THICKG = GEO(42,PID) ! General PTHICKFAIL 
      FAIL_EXP = GEO(43,PID) ! Failure exponent for TYPE 51
      IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN 
        IPTHKLY  = 1 + 4*NLAY     ! Address of PTHKLY in STACK%GEO table
        IPWEIGHT = IPTHKLY + NLAY ! Address of WEIGHT in STACK%GEO table
      ELSE
        IPTHKLY  = 700 ! Address of PTHKLY in GEO table
        IPWEIGHT = 900 ! Address of WEIGHT in GEO table
      ENDIF
c         
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
        ! Check element failure
        DO IFL = 1,NFAIL
          THFACT(1:NEL) = ZERO   
          NPFAIL(1:NEL) = ZERO    
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
          ! Computation of broken fraction of thickness / 
          ! ratio of intg. points                               
          DO IPT=1,NPTT                                                    
            FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
            DO IEL=1,NEL                                                       
              IF (OFF(IEL) == ONE) THEN                                        
                IF (FOFF(IEL) < ONE)  THEN                
                  IPOS = (IPT-1)*NEL + IEL                                      
                  THFACT(IEL) = THFACT(IEL) + THKLY(IPOS) ! -> Percentage of broken thickness (accounting for integration weight)
                  NPFAIL(IEL) = NPFAIL(IEL) + ONE/NPTT    ! -> Ratio of broken integration points
                ENDIF                                                           
              ENDIF                                               
            ENDDO ! |-> IEL                                          
          ENDDO ! |-> IPT
          ! Comparison with critical value PTHICKFAIL                               
          DO IEL=1,NEL                                                          
            IF (OFF(IEL) == ONE) THEN                                        
              IF (((THFACT(IEL) >= PTHKF(IL,IFL)).AND.(PTHKF(IL,IFL) > ZERO)).OR.
     .            ((NPFAIL(IEL) >= ABS(PTHKF(IL,IFL))).AND.(PTHKF(IL,IFL) < ZERO))) THEN
                OFF(IEL) = FOUR_OVER_5
                IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1
              ENDIF                                      
            ENDIF                                                           
          ENDDO ! |-> IEL                                               
        ENDDO ! |-> IFL  
c
      !=================================================================
      ! MULTI LAYER PROPERTIES / 1 INTG POINT - TYPE10/11/16/17/51/52
      !=================================================================
      ELSEIF (NLAY == NPTTOT) THEN  
c
        ! Only one integration points in each layer
        IPT = 1
c
        ! Check layer failure
        DO IL=1,NLAY
          NINDXLY = 0
          NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
          DO IFL = 1,NFAIL
            FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
            DO IEL=1,NEL
              IF (OFF(IEL) == ONE .AND. LAY_OFF(IEL) == 1) THEN
                IF (FOFF(IEL) < 1)  THEN
                  NINDXLY = NINDXLY + 1    
                  INDXLY(NINDXLY) = IEL    
                  LAY_OFF(IEL) = 0
                ENDIF
              ENDIF
            ENDDO ! |-> IEL
          ENDDO ! |-> IFL
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
#include      "lockon.inc"                           
                WRITE(IOUT, 3000) ID_PLY,NGL(INDXLY(I))    
                WRITE(ISTDO,3100) ID_PLY,NGL(INDXLY(I)),TT 
#include      "lockoff.inc"                          
              ENDDO ! |-> I
            ! -> Print out layer failure
            ELSE 
              DO I = 1,NINDXLY
#include      "lockon.inc"                           
                WRITE(IOUT, 2000) IL,NGL(INDXLY(I))    
                WRITE(ISTDO,2100) IL,NGL(INDXLY(I)),TT 
#include      "lockoff.inc"                          
              ENDDO ! |-> I
            ENDIF
          ENDIF
        ENDDO ! |-> IL
c
        ! Check element failure  
        THFACT(1:NEL) = ZERO
        NORM(1:NEL)   = ZERO
        NPFAIL(1:NEL) = ZERO
        ! Computation of broken fraction of thickness / ratio of layers 
        DO IL=1,NLAY
          IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            WEIGHT(IL) = STACK%GEO(IPWEIGHT+ IL,ISUBSTACK)
          ELSE
            WEIGHT(IL) = GEO(IPWEIGHT + IL,PID)
          ENDIF
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
          II = (IL-1)*NEL
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              IPOS = II + IEL
              DFAIL = THKLY(IPOS)*WEIGHT(IL)
              NORM(IEL)  = NORM(IEL)  + DFAIL
              IF (LAY_OFF(IEL) == 0) THEN
                THFACT(IEL) = THFACT(IEL) + THKLY(IPOS)*WEIGHT(IL) ! -> Percentage of broken thickness
                NPFAIL(IEL) = NPFAIL(IEL) + ONE/NLAY               ! -> Ratio of broken layers
              ENDIF
            ENDIF   
          ENDDO ! |-> IEL
        ENDDO ! |-> IL
        ! Comparison with critical value PTHICKFAIL
        DO IEL=1,NEL
          IF (OFF(IEL) == ONE) THEN
            IF (((THFACT(IEL) >= P_THICKG*NORM(IEL)).AND.(P_THICKG > ZERO)).OR.
     .          ((NPFAIL(IEL) >= ABS(P_THICKG)).AND.(P_THICKG < ZERO))) THEN
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
        ! Check layer failure
        DO IL=1,NLAY
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
          NINDXLY = 0
          NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
          II = (IL-1)*NEL
          ! Computation of broken fraction of thickness / ratio of integration points
          DO IFL = 1,NFAIL
            THFACT(1:NEL) = ZERO
            NPFAIL(1:NEL) = ZERO
            DO IPT = 1,NPTT
              FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
              DO IEL=1,NEL                                                        
                IF (OFF(IEL) == ONE) THEN
                  IF (LAY_OFF(IEL) == 1) THEN
                    IF (FOFF(IEL) < ONE) THEN
                      IPOS = II + IEL
                      THFACT(IEL) = THFACT(IEL) + THKLY(IPOS)/THK_LY(IEL,IL) ! -> Percentage of broken thickness
                      NPFAIL(IEL) = NPFAIL(IEL) + ONE/NPTT                   ! -> Ratio of broken integration points
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO ! |-> IEL
            ENDDO ! |-> IPT
            ! Comparison with critical value PTHICKFAIL
            DO IEL=1,NEL
              IF (OFF(IEL) == ONE) THEN
                IF (((THFACT(IEL) >= PTHKF(IL,IFL)).AND.(PTHKF(IL,IFL) > ZERO)).OR.
     .              ((NPFAIL(IEL) >= ABS(PTHKF(IL,IFL))).AND.(PTHKF(IL,IFL) < ZERO))) THEN
                  NINDXLY = NINDXLY + 1
                  INDXLY(NINDXLY) = IEL
                  LAY_OFF(IEL) = 0   
                  DO IPT=1,NPTT
                    FOFF => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IPT)%FLOC(IFL)%OFF
                    FOFF(IEL) = 0
                  ENDDO
                ENDIF
              ENDIF
            ENDDO ! |-> IEL
          ENDDO ! |-> IFL
          ! Printing out ply failure message
          IF (NINDXLY > 0) THEN  
            IF (IGTYP == 51) THEN 
              ID_PLY = IGEO(1,STACK%IGEO(2+IL,ISUBSTACK))
            ELSE 
              ID_PLY = PLY_INFO(1,STACK%IGEO(2+IL,ISUBSTACK)-NUMSTACK)  
            ENDIF                                   
            DO I = 1,NINDXLY                                          
#include     "lockon.inc"                                             
              WRITE(IOUT, 3000) ID_PLY,NGL(INDXLY(I))                   
              WRITE(ISTDO,3100) ID_PLY,NGL(INDXLY(I)),TT                
#include     "lockoff.inc"                                            
            ENDDO ! |-> I                                                  
          ENDIF
        ENDDO ! |-> IL
c        
        ! Check element failure
        THFACT(1:NEL) = ZERO
        NPFAIL(1:NEL) = ZERO
        NORM(1:NEL)   = ZERO
        ! Computation of broken fraction of thickness / ratio of layers 
        DO IL=1,NLAY
          WEIGHT(IL) = STACK%GEO(IPWEIGHT + IL,ISUBSTACK)
          LAY_OFF => ELBUF_STR%BUFLY(IL)%OFF
          DO IEL=1,NEL
            IF (OFF(IEL) == ONE) THEN
              DFAIL = (THK_LY(IEL,IL)*WEIGHT(IL))**FAIL_EXP
              NORM(IEL) = NORM(IEL) + DFAIL
              IF (LAY_OFF(IEL) == 0) THEN
                THFACT(IEL) = THFACT(IEL) + DFAIL    ! -> Percentage of broken thickness (among layers)
                NPFAIL(IEL) = NPFAIL(IEL) + ONE/NLAY ! -> Ratio of broken layers
              ENDIF
            ENDIF
          ENDDO ! |-> IEL
        ENDDO ! |-> IL 
        ! Comparison with critical value PTHICKFAIL
        DO IEL=1,NEL
          IF (OFF(IEL) == ONE) THEN
            THFACT(IEL) = THFACT(IEL)**(ONE/FAIL_EXP)
            NORM(IEL)   = NORM(IEL)**(ONE/FAIL_EXP)
            IF (((THFACT(IEL) >= P_THICKG*NORM(IEL)).AND.(P_THICKG > ZERO)).OR.
     .          ((NPFAIL(IEL) >= ABS(P_THICKG)).AND.(P_THICKG < ZERO))) THEN
              OFF(IEL) = FOUR_OVER_5
              IF (FAILWAVE%WAVE_MOD > 0) FWAVE_EL(IEL) = -1            
            ENDIF
          ENDIF 
        ENDDO ! |-> IEL
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
