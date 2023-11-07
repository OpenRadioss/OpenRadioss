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
Chd|  FAIL_TSAIHILL_C               source/materials/fail/tsaihill/fail_tsaihill_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_TSAIHILL_C(
     1     NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      , 
     2     TIME      ,NGL       ,IPG       ,ILAY      ,IPT       ,                          
     3     SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,  
     4     OFF       ,FOFF      ,DMG_FLAG  ,DMG_SCALE ,
     5     DFMAX     ,TDEL      ,TIMESTEP  )                               
C-----------------------------------------------
C    Tsai-Hill failure model model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFF,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) ::DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,IFAIL_SH
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real
     .   X11,X22,S12,TMAX,FCUT
      my_real
     .   ASRATE,FINDEX,RFACTR,
     .   SXX(NEL),SYY(NEL),SXY(NEL)
C!----------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      X11      = UPARAM(1)
      X22      = UPARAM(2)
      S12      = UPARAM(3)
      TMAX     = UPARAM(4)
      FCUT     = UPARAM(5)
      IFAIL_SH = INT(UPARAM(6))
c
      ! Stress softening activation
      DMG_FLAG = 1    
c
      ! Stress tensor filtering
      IF (FCUT > ZERO) THEN
        ASRATE = TWO*PI*FCUT*TIMESTEP
        ASRATE = ASRATE/(ONE+ASRATE)
        DO I = 1,NEL
          SXX(I)    = ASRATE*SIGNXX(I) + (ONE - ASRATE)*UVAR(I,2)
          SYY(I)    = ASRATE*SIGNYY(I) + (ONE - ASRATE)*UVAR(I,3)
          SXY(I)    = ASRATE*SIGNXY(I) + (ONE - ASRATE)*UVAR(I,4)
          UVAR(I,2) = SXX(I)
          UVAR(I,3) = SYY(I)
          UVAR(I,4) = SXY(I)
        ENDDO
      ELSE
        DO I = 1,NEL
          SXX(I) = SIGNXX(I)
          SYY(I) = SIGNYY(I)
          SXY(I) = SIGNXY(I)
        ENDDO
      ENDIF
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index           
      NINDX = 0
      INDX(1:NEL) = 0  
c
      ! Loop over the elements
      DO I=1,NEL
c
        ! If damage has not been reached yet
        IF (DFMAX(I)<ONE) THEN
c
          ! Compute failure index and reserve factor
          FINDEX = (SXX(I)/X11)**2 - ((SXX(I)*SYY(I))/(X11**2)) +
     .             (SYY(I)/X22)**2 + (SXY(I)/S12)**2
          IF (FINDEX > ZERO) THEN 
            RFACTR = ONE/SQRT(FINDEX)
          ELSE
            RFACTR = ZERO
          ENDIF
c
          ! Damage variable update
          FINDEX   = MAX(ZERO,FINDEX)
          DFMAX(I) = MIN(ONE ,MAX(FINDEX,DFMAX(I)))
          IF (DFMAX(I) >= ONE) THEN
            NINDX = NINDX+1                                    
            INDX(NINDX) = I                                    
            IF (IFAIL_SH > 0) THEN 
              UVAR(I,1) = TIME
            ENDIF                                
          ENDIF
        ENDIF
c
        ! Stress relaxation in case of damage reached
        IF ((UVAR(I,1) > ZERO).AND.(FOFF(I) /= 0).AND.(IFAIL_SH > 0)) THEN 
          DMG_SCALE(I) = EXP(-(TIME - UVAR(I,1))/TMAX)   
          IF (DMG_SCALE(I) < EM02) THEN
            FOFF(I) = 0
            TDEL(I) = TIME
            DMG_SCALE(I) = ZERO
          ENDIF
        ENDIF  
      ENDDO       
c              
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED ELEMENTS
      !====================================================================
      IF (NINDX > 0) THEN  
        DO J=1,NINDX       
          I = INDX(J)      
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
 2000 FORMAT(1X,'FAILURE (TSAIHILL) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (TSAIHILL) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      END
