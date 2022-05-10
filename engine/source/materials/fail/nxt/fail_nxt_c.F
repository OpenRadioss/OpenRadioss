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
Chd|  FAIL_NXT_C                    source/materials/fail/nxt/fail_nxt_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_NXT_C(
     1     NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      ,
     2     TIME      ,NPF       ,TF        ,NFUNC     ,IFUNC     ,
     3     NGL       ,IPG       ,ILAY      ,IPT       ,HARDM     ,
     4     SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,  
     5     OFF       ,FOFF      ,PTHKF     ,DFMAX     ,TDEL      ,
     6     DAM       )
C-----------------------------------------------
c     NXT Failure
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C PTHKF   |  1      | F | W | PERCENTAGE OF LAYER THICKNESS TO FAIL
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NFUNC,NUPARAM,NUVAR,IPG,ILAY,IPT
      INTEGER, DIMENSION(NFUNC), INTENT(IN) :: IFUNC
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: OFF,HARDM,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,INTENT(OUT) :: PTHKF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX,DAM
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IFAIL_SH,NINDX,CHECK
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: S1,S2,SS,S12,DYDX1,DYDX2,S2DYDX1,SIGSR,SIG3D
      my_real ,DIMENSION(NEL)  :: SIG1,SIG2,LAMBDANXT
c=======================================================================
      DO I=1,NEL
        SIG1(I) = ZERO    ! 1st principal normalized stress
        SIG2(I) = ZERO    ! 2d principal normalized stress
        LAMBDANXT(I) = ZERO  ! Instability factor
      ENDDO 
      NINDX = 0                                                
C-------- ZERO VALUE for deleted element       
      DO I=1,NEL
        IF (OFF(I) == ZERO) THEN 
          UVAR(I,1)= ZERO
          UVAR(I,2)= ZERO
        END IF
      ENDDO    
c-----------------------------
      IFAIL_SH = INT(UPARAM(1))
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = UN
      ENDIF
C-------------------------------------------------------
C     SIGMA1 and SIGMA2 computing: Principal stresses
C-------------------------------------------------------
      DO I=1,NEL
        IF (OFF(I) == UN .and. FOFF(I) == 1) THEN
          S12= SIGNXY(I)
          S1 = UNDEMI*(SIGNXX(I) + SIGNYY(I))
          S2 = UNDEMI*(SIGNXX(I) - SIGNYY(I))
          SIG1(I) = S1  + SQRT(S2**2 + S12**2)
          SS   = S1  - SQRT(S2**2 + S12**2)
          SIG2(I) = SS

          IF(SS >= SIG1(I))THEN
            SIG2(I) = SIG1(I)
            SIG1(I) = SS
          ENDIF
          CHECK = 1
          IF (SIG2(I).NE.ZERO) THEN
            SS = SIG1(I)/SIG2(I)
            IF ((SS < -Un .AND. SS < ZERO) .OR.
     .          (SIG1(I) < ZERO .AND. SIG2(I) < ZERO)) THEN
              CHECK = 0
            ENDIF
          ENDIF
C --- Normalization of stresses
          S1 = SIG1(I)
          S2 = SIG2(I)
          SIG1(I) = SIG1(I)/MAX(HARDM(I),EM20)
          SIG2(I) = SIG2(I)/MAX(HARDM(I),EM20)
          IF (CHECK == 1 ) THEN
C --- compute variables from input curve SR and 3D 
            SIGSR =  FINTER(IFUNC(1),SIG2(I),NPF,TF,DYDX1)
            SIG3D =  FINTER(IFUNC(2),SIG2(I),NPF,TF,DYDX2)      
            IF(SIGSR < ABS(SIG2(I)).AND.SIG3D < ABS(SIG2(I))) THEN
               LAMBDANXT(I) = DEUX
            ELSEIF(SIGSR < ABS(SIG2(I))) THEN
              SIGSR = ABS(SIG2(I))  
              LAMBDANXT(I) = UN + ((SIG1(I) - SIGSR)/(SIG3D - SIGSR))
            ELSE 
C --- The instability factor
             LAMBDANXT(I) = UN + ((SIG1(I) - SIGSR)/(SIG3D - SIGSR))
            ENDIF
            LAMBDANXT(I) = MAX(LAMBDANXT(I),ZERO)
            LAMBDANXT(I) = MIN(LAMBDANXT(I),DEUX)
          ELSE
            LAMBDANXT(I) = ZERO
          ENDIF  
        ENDIF 
      ENDDO
c
c-----------------------
      DO I=1,NEL
        IF (OFF(I) == UN .and. FOFF(I) == 1) THEN
          IF (LAMBDANXT(I) >= DEUX) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I  
            FOFF(I) = 0
            TDEL(I) = TIME
          ENDIF
	       ENDIF
      ENDDO
c-----------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include  "lockon.inc"
           WRITE(IOUT, 2000) NGL(I),IPT,LAMBDANXT(I)
           WRITE(ISTDO,2100) NGL(I),IPT,TIME,LAMBDANXT(I)
#include  "lockoff.inc"
         END DO
      ENDIF   ! NINDX            
c-----------------------
c     SAVE USER VARIABLES
c-----------------------
      DO I=1,NEL                          
        DAM(I)    = MAX(DAM(I),LAMBDANXT(I))                                     
        UVAR(I,1) = SIG1(I)                                                      
        UVAR(I,2) = SIG2(I)                                                      
        DFMAX(I)  = MIN(UN, DAM(I)/DEUX) ! Maximum Dmg for output : 0 < DFMAX < 1  
      ENDDO                                                                      
C-----------------------------------------------
 2000 FORMAT(1X,'(NXT FAILURE) FOR SHELL ELEMENT ',I10,1X,
     .'LAYER',I10,':',/
     . 1X,'WITH FAILURE FACTOR :',1PE20.13)
 2100 FORMAT(1X,'(NXT FAILURE) FOR SHELL ELEMENT',I10,1X,
     .'LAYER',I10,':',/,
     . 1X,'AT TIME :',1PE20.13/,
     . 1X,'WITH FAILURE FACTOR :',1PE20.13)
C-----------------------------------------------
        RETURN
        END
