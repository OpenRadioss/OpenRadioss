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
C modified Puck model ------
Chd|====================================================================
Chd|  FAIL_PUCK_S                   source/materials/fail/puck/fail_puck_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_PUCK_S(
     1     NEL     ,NUVAR   ,ILAY    ,NPT0    ,
     2     TIME    ,TIMESTEP,UPARAM  ,
     3     NGL     ,OFF     ,NOFF    ,SIGNXX  ,
     4     SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     5     UVAR    ,NUPARAM ,DFMAX   ,LF_DAMMX,TDELE   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF    | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "com01_c.inc"
#include "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM, NUVAR,ILAY,NPT0
      INTEGER NGL(NEL)
      INTEGER, INTENT(IN) :: LF_DAMMX
      my_real 
     .   TIME,TIMESTEP(NEL),UPARAM(NUPARAM),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER NOFF(NEL)
      my_real UVAR(NEL,NUVAR), OFF(NEL), TDELE(NEL)
      my_real ,DIMENSION(NEL,LF_DAMMX),INTENT(INOUT) :: DFMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I,J,IDEL,IDEL_L,IFLAG,INDX(MVSIZ),IADBUF,NINDX,
     .        NINDEX,INDEX(MVSIZ),IFAIL,JJ,IMATLY,
     .        IUNIDIR,IFABRIC,INDX0(MVSIZ),NINDX0,FSMOOTH
      my_real 
     .   SIGT1,SIGT2, SIGC1,SIGC2,
     .   FSIG12,F1,FA,FB,FC,PN12,PP12,PN22,
     .   FAC,TMAX,FA_2,FB_2,FC_2,FCUT,ASRATE,
     .   SXX(NEL),SYY(NEL),SZZ(NEL),
     .   SXY(NEL),SYZ(NEL),SZX(NEL)
C--------------------------------------------------------------
      SIGT1  = UPARAM(1 )
      SIGT2  = UPARAM(2)
      SIGC1  = UPARAM(3)
      SIGC2  = UPARAM(4)
      FSIG12 = UPARAM(5)
      PP12   = UPARAM(6)
      PN12   = UPARAM(7)         
      PN22   = UPARAM(8)
      TMAX   = UPARAM(9) 
      IFLAG  = INT(UPARAM(11)) 
      FCUT   = UPARAM(12)  
      IF (FCUT > ZERO) THEN
        FSMOOTH = 1 
        ASRATE  = TWO*PI*FCUT*TIMESTEP(1)
        ASRATE  = ASRATE/(ONE+ASRATE)
      ELSE
        FSMOOTH = 0
      ENDIF
      INDX(1:MVSIZ)  = 0
      INDEX(1:MVSIZ) = 0
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(ISIGI == 0)THEN
        IF ((UVAR(1,8) == ZERO).AND.(OFF(1) == ONE)) THEN
          DO I=1,NEL
            UVAR(I,8)  = ONE
          ENDDO   
        ENDIF    
      ENDIF 
C-----------------------------------------------
      IUNIDIR = 0
      IFABRIC = 0
      DO I=1,NEL
        IF(OFF(I)<EM01) OFF(I)=ZERO
        IF(OFF(I)<ONE)  OFF(I)=OFF(I)*FOUR_OVER_5
        IF (FSMOOTH > 0) THEN 
          SXX(I) = ASRATE*SIGNXX(I) + (ONE - ASRATE)*UVAR(I,9)
          SYY(I) = ASRATE*SIGNYY(I) + (ONE - ASRATE)*UVAR(I,10)
          SZZ(I) = ASRATE*SIGNZZ(I) + (ONE - ASRATE)*UVAR(I,11)
          SXY(I) = ASRATE*SIGNXY(I) + (ONE - ASRATE)*UVAR(I,12)
          SYZ(I) = ASRATE*SIGNYZ(I) + (ONE - ASRATE)*UVAR(I,13)
          SZX(I) = ASRATE*SIGNZX(I) + (ONE - ASRATE)*UVAR(I,14)
          UVAR(I,9)  = SXX(I)
          UVAR(I,10) = SYY(I)
          UVAR(I,11) = SZZ(I)
          UVAR(I,12) = SXY(I)
          UVAR(I,13) = SYZ(I)
          UVAR(I,14) = SZX(I)
        ELSE 
          SXX(I) = SIGNXX(I)
          SYY(I) = SIGNYY(I)
          SZZ(I) = SIGNZZ(I)
          SXY(I) = SIGNXY(I)
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)
        ENDIF
      END DO
C-------------------------------
C      
C     OFF = 0. si la matrice ou la fibre a rompu
C-------------------------------    
        NINDX = 0 
        NINDX0= 0 
        INDX  = 0 
        INDX0 = 0
        DO I=1,NEL
         F1   = ZERO
         FA   = ZERO
         FB   = ZERO
         FC   = ZERO
         FA_2 = ZERO
         FB_2 = ZERO
         FC_2 = ZERO
C
         IF (OFF(I) == ONE )THEN
C         
           IF (IFLAG == 1) THEN
C-------------------------------     
C           OFF = 0 when one layer fiber or matrix criteria is reached
C-------------------------------                  
            IF (UVAR(I,8) < ONE) THEN 
              UVAR(I,8) = EXP(-(TIME - UVAR(I,7))/TMAX)
              IF (UVAR(I,8) < EM02) UVAR(I,8) = ZERO
              SIGNXX(I) = UVAR(I,1)*UVAR(I,8)
              SIGNYY(I) = UVAR(I,2)*UVAR(I,8)
              SIGNZZ(I) = UVAR(I,3)*UVAR(I,8)
              SIGNXY(I) = UVAR(I,4)*UVAR(I,8)
              SIGNYZ(I) = UVAR(I,5)*UVAR(I,8)
              SIGNZX(I) = UVAR(I,6)*UVAR(I,8) 
              IF (UVAR(I,8) == ZERO )THEN
                OFF(I)=FOUR_OVER_5
                NINDX=NINDX+1
                INDX(NINDX)=I 
                TDELE(I) = TIME   
              ENDIF  
            ELSE                  
C
C             fiber criteria
C
              IF (SXX(I) >= ZERO) THEN
                F1 = SXX(I)/SIGT1
                DFMAX(I,2) = MAX(DFMAX(I,2),F1)
                DFMAX(I,2) = MIN(DFMAX(I,2),ONE)
              ELSE
                F1 = -SXX(I)/SIGC1
                DFMAX(I,3) = MAX(DFMAX(I,3),F1)
                DFMAX(I,3) = MIN(DFMAX(I,3),ONE)
              ENDIF
C
C             matrice criteria direction 2
C              
              IF (SYY(I) >= ZERO) THEN
                FAC = ONE - PP12*SIGT2/FSIG12
                FAC = FAC*SYY(I)/SIGT2
                FA = SQRT((SXY(I)/FSIG12)**2 + FAC*FAC) 
     .                   + PP12*SYY(I)/FSIG12
                DFMAX(I,4) = MAX(DFMAX(I,4),FA)
                DFMAX(I,4) = MIN(DFMAX(I,4),ONE)
               ELSE
                FAC = HALF/(ONE + PN22)/FSIG12
                FC = (SXY(I)*FAC)**2 + (SYY(I)/SIGC2)**2
                FC =-FC*SIGC2/SYY(I)
                DFMAX(I,6) = MAX(DFMAX(I,6),FC)
                DFMAX(I,6) = MIN(DFMAX(I,6),ONE)
C plane angle
c                FAC = SQRT(ONE + TWO*PN12(I)*SIGC2(I)/FSIG12(I)) - ONE
c                FAC = HALF*FAC*FSIG12(I)/PN12(I)
c                CC =  (FAC/FSIG12(I))**2
c                CC = CC*(SIGNXY(I)/SIGNYY(I))**2 + ONE
c                FAC = HALF/ONE + PN22(I))
c                CC = SQRT(FAC*CC)
c                ANGLE(I) = ACOS(CC)                               
              ENDIF
              
              FB = SQRT(SXY(I)**2 + (PN12*SYY(I))**2 ) 
     .                               + PN12*SYY(I)
              FB = FB/FSIG12
              DFMAX(I,5) = MAX(DFMAX(I,5),FB)
              DFMAX(I,5) = MIN(DFMAX(I,5),ONE)
C
C             matrice criteria direction 3
C              
              IF (SZZ(I) >= ZERO) THEN
                FAC = ONE - PP12*SIGT2/FSIG12
                FAC = FAC*SZZ(I)/SIGT2
                FA_2 = SQRT((SZX(I)/FSIG12)**2 + FAC*FAC) 
     .                   + PP12*SZZ(I)/FSIG12
                DFMAX(I,4) = MAX(DFMAX(I,4),FA_2)
                DFMAX(I,4) = MIN(DFMAX(I,4),ONE)
              ELSE
                FAC = HALF/(ONE + PN22)/FSIG12
                FC_2 = (SZX(I)*FAC)**2 + (SZZ(I)/SIGC2)**2
                FC_2 =-FC_2*SIGC2/SZZ(I)
                DFMAX(I,6) = MAX(DFMAX(I,6),FC_2)
                DFMAX(I,6) = MIN(DFMAX(I,6),ONE)
C plane angle
c                FAC = SQRT(ONE + TWO*PN12(I)*SIGC2(I)/FSIG12(I)) - ONE
c                FAC = HALF*FAC*FSIG12(I)/PN12(I)
c                CC =  (FAC/FSIG12(I))**2
c                CC = CC*(SIGNZX(I)/SIGNZZ(I))**2 + ONE
c                FAC = HALF/ONE + PN22(I))
c                CC = SQRT(FAC*CC)
c                ANGLE(I) = ACOS(CC)                               
              ENDIF
              
              FB_2 = SQRT(SZX(I)**2 + (PN12*SZZ(I))**2 ) 
     .                               + PN12*SZZ(I)
              FB_2 = FB_2/FSIG12 
              DFMAX(I,5) = MAX(DFMAX(I,5),FB_2)
              DFMAX(I,5) = MIN(DFMAX(I,5),ONE)  
C
              DFMAX(I,1) = MIN(ONE,MAX(DFMAX(I,1),F1,FA,FB,FC,FA_2,FB_2,FC_2))
C         
              IF (F1 >= ONE .OR. FA   >= ONE .OR. FB   >= ONE .OR.
     .            FC >= ONE .OR. FA_2 >= ONE .OR. FB_2 >= ONE .OR.
     .            FC_2 >= ONE   ) THEN
               UVAR(I,1) = SIGNXX(I)
               UVAR(I,2) = SIGNYY(I)
               UVAR(I,3) = SIGNZZ(I)
               UVAR(I,4) = SIGNXY(I)
               UVAR(I,5) = SIGNYZ(I)
               UVAR(I,6) = SIGNZX(I)
               UVAR(I,7) = TIME
               UVAR(I,8) = FOUR_OVER_5            
              ENDIF
             ENDIF
C             
            ELSE    ! iflag/= 1 
C-------------------------------     
C     OFF = 0. all layer fiber or matrix criteria is reateched
C-------------------------------              
              IF(UVAR(I,8) == ZERO )THEN
                SIGNXX(I) = ZERO
                SIGNYY(I) = ZERO
                SIGNZZ(I) = ZERO
                SIGNXY(I) = ZERO
                SIGNZX(I) = ZERO
                SIGNYZ(I) = ZERO
              ELSE IF(UVAR(I,8) < ONE) THEN
                UVAR(I,8)= EXP(-(TIME - UVAR(I,7))/TMAX) 
                IF(UVAR(I,8) < EM02)UVAR(I,8) = ZERO
                SIGNXX(I) = UVAR(I,1)*UVAR(I,8)
                SIGNYY(I) = UVAR(I,2)*UVAR(I,8)
                SIGNZZ(I) = UVAR(I,3)*UVAR(I,8)
                SIGNXY(I) = UVAR(I,4)*UVAR(I,8)
                SIGNYZ(I) = UVAR(I,5)*UVAR(I,8)
                SIGNZX(I) = UVAR(I,6)*UVAR(I,8)
                IF(UVAR(I,8) == ZERO )THEN
                   NOFF(I) = NOFF(I) + 1
                    IF(NPT0 == 1 .OR. NOFF(I) == NPT0 ) THEN
                       OFF(I) = FOUR_OVER_5
                       NINDX=NINDX+1
                       INDX(NINDX)=I
                       TDELE(I) = TIME  
                    ENDIF
                ENDIF            
              ELSE                  
C
C   fiber criteria
C
              IF(SXX(I) > 0 ) THEN
                F1 = SXX(I)/SIGT1
                DFMAX(I,2) = MAX(DFMAX(I,2),F1)
                DFMAX(I,2) = MIN(DFMAX(I,2),ONE)
              ELSE
                F1 = -SXX(I)/SIGC1
                DFMAX(I,3) = MAX(DFMAX(I,3),F1)
                DFMAX(I,3) = MIN(DFMAX(I,3),ONE)
              ENDIF
C
C  matrice criteria --direction 2
C              
              IF(SYY(I) >= ZERO) THEN
                FAC = ONE - PP12*SIGT2/FSIG12
                FAC = FAC*SYY(I)/SIGT2
                FA = SQRT((SXY(I)/FSIG12)**2 + FAC*FAC) 
     .                   + PP12*SYY(I)/FSIG12
                DFMAX(I,4) = MAX(DFMAX(I,4),FA)
                DFMAX(I,4) = MIN(DFMAX(I,4),ONE)
               ELSE
                FAC = HALF/(ONE + PN22)/FSIG12
                FC = (SXY(I)*FAC)**2 + (SYY(I)/SIGC2)**2
                FC =-FC*SIGC2/SYY(I)
                DFMAX(I,6) = MAX(DFMAX(I,6),FC)
                DFMAX(I,6) = MIN(DFMAX(I,6),ONE)
C plane angle
c                FAC = SQRT(ONE + TWO*PN12(I)*SIGC2(I)/FSIG12(I)) - ONE
c                FAC = HALF*FAC*FSIG12(I)/PN12(I)
c                CC =  (FAC/FSIG12(I))**2
c                CC = CC*(SIGNXY(I)/SIGNYY(I))**2 + ONE
c                FAC = HALF/ONE + PN22(I))
c                CC = SQRT(FAC*CC)
c                ANGLE(I) = ACOS(CC)                               
              ENDIF
              FB = SQRT(SXY(I)**2 + (PN12*SYY(I))**2 ) 
     .                               + PN12*SYY(I)
              FB = FB/FSIG12
              DFMAX(I,5) = MAX(DFMAX(I,5),FB)
              DFMAX(I,5) = MIN(DFMAX(I,5),ONE)
C
C  matrice criteria direction 3
C              
              IF(SZZ(I) >= ZERO) THEN
                FAC = ONE - PP12*SIGT2/FSIG12
                FAC = FAC*SZZ(I)/SIGT2
                FA_2 = SQRT((SZX(I)/FSIG12)**2 + FAC*FAC) 
     .                   + PP12*SZZ(I)/FSIG12
                DFMAX(I,4) = MAX(DFMAX(I,4),FA_2)
                DFMAX(I,4) = MIN(DFMAX(I,4),ONE)
               ELSE
                FAC = HALF/(ONE + PN22)/FSIG12
                FC_2 = (SZX(I)*FAC)**2 + (SZZ(I)/SIGC2)**2
                FC_2 =-FC_2*SIGC2/SZZ(I)
                DFMAX(I,6) = MAX(DFMAX(I,6),FC_2)
                DFMAX(I,6) = MIN(DFMAX(I,6),ONE)
C plane angle
c                FAC = SQRT(ONE + TWO*PN12(I)*SIGC2(I)/FSIG12(I)) - ONE
c                FAC = HALF*FAC*FSIG12(I)/PN12(I)
c                CC =  (FAC/FSIG12(I))**2
c                CC = CC*(SIGNZX(I)/SIGNZZ(I))**2 + ONE
c                FAC = HALF/ONE + PN22(I))
c                CC = SQRT(FAC*CC)
c                ANGLE(I) = ACOS(CC)                               
              ENDIF
              
              FB_2 = SQRT(SZX(I)**2 + (PN12*SZZ(I))**2 ) 
     .                               + PN12*SZZ(I)
              FB_2 = FB_2/FSIG12  
              DFMAX(I,5) = MAX(DFMAX(I,5),FB_2)
              DFMAX(I,5) = MIN(DFMAX(I,5),ONE)   
C
              DFMAX(I,1)  = MIN(ONE,MAX(DFMAX(I,1),F1,FA,FB,FC,FA_2,FB_2,FC_2))
C           
              IF(F1 >= ONE .OR. FA >= ONE .OR. FB >= ONE .OR.
     .          FC >= ONE .OR. FA_2 >= ONE .OR. FB_2 >= ONE .OR.
     .          FC_2 >= ONE )THEN            
                   UVAR(I,1) = SIGNXX(I)                                  
                   UVAR(I,2) = SIGNYY(I)                                   
                   UVAR(I,3) = SIGNZZ(I)                                  
                   UVAR(I,4) = SIGNXY(I)                                  
                   UVAR(I,5) = SIGNYZ(I)                                  
                   UVAR(I,6) = SIGNZX(I)                                  
                   UVAR(I,7) = TIME                                       
                   UVAR(I,8) = FOUR_OVER_5
                   NINDX0= NINDX0+1
                   INDX0(NINDX0)=I                    
                 ENDIF
               ENDIF
             ENDIF  !  iflag choice
           ENDIF        
         ENDDO  
C--------------------------------------------      
C       print 
C--------------------------------------------      
        IF(NINDX > 0)THEN
          DO J=1,NINDX
           I = INDX(J)
#include "lockon.inc"
           WRITE(IOUT, 1200) NGL(I),TIME
           WRITE(ISTDO,1200) NGL(I),TIME
#include "lockoff.inc"
          END DO
         ENDIF         
C                
        IF(NINDX0 > 0)THEN
          DO J=1,NINDX0
           I = INDX0(J)
#include "lockon.inc"
           WRITE(IOUT, 1100) NGL(I),ILAY,TIME
           WRITE(ISTDO,1100) NGL(I),ILAY,TIME
#include "lockoff.inc"
          END DO
        ENDIF      
C--------------------------------------------      
 1100 FORMAT(1X,'FAILURE ELEMENT #',I10,1X,
     .'IP #',I10,1X, 'AT TIME #:',1PE20.13)

 1200 FORMAT(1X,'DELETE SOLID ELEMENT (PUCK MODEL) #',I10,1X,
     .'AT TIME # ',1PE20.13)  
C--------------------------------------------      
      RETURN
      END
