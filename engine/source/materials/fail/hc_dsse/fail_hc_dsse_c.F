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
Chd|  FAIL_HC_DSSE_C                source/materials/fail/hc_dsse/fail_hc_dsse_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_HC_DSSE_C(
     1           NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,
     2           TIME     ,NGL      ,IPT      ,ILAY     ,IPTT     ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           DPLA     ,OFF      ,FOFF     ,PTHKF    ,
     5           DFMAX    ,TDEL     ,UEL      ,NPTOT    ,IGTYP  )

C-----------------------------------------------
C I m p l i c i t T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C!---------+--------+---+---+-------------------------------------------
C! VAR     | SIZE   |TYP| RW| DEFINITION
C!---------+--------+---+---+-------------------------------------------
C! NEL     | 1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C! NUPARAM | 1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C! NUVAR   | 1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C!---------+--------+---+---+-------------------------------------------
C! NFUNC   | 1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C! IFUNC   | NFUNC  | I | R | FUNCTION INDEX
C! NPF     | *      | I | R | FUNCTION ARRAY
C! NPT0    | 1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS
C! IPT                         CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C! TF      | *      | F | R | FUNCTION ARRAY
C! NGL     | NEL    | I | R | ELEMENT NUMBER
C! IPG     |        |   |   | CURRENT GAUSS POINT (in plane)
C! ILAY    |        |   |   | CURRENT LAYER
C! FOFF    | NEL    | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C! PTHKF   |  1      | F | W | PERCENTAGE OF LAYER THICKNESS TO FAIL
C! DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C! TDEL    | NEL     | F | W | FAILURE TIME
C!---------+--------+---+---+-------------------------------------------
C! TIME    | 1      | F | R | CURRENT TIME
C! TIMESTEP| 1      | F | R | CURRENT TIME STEP
C! UPARAM  | NUPARAM| F | R | USER MATERIAL PARAMETER ARRAY
C! EPSPXX  | NEL    | F | R | STRAIN RATE XX
C! EPSPYY  | NEL    | F | R | STRAIN RATE YY
C! ...     |        |   |   |
C! EPSXX   | NEL    | F | R | STRAIN XX
C! EPSYY   | NEL    | F | R | STRAIN YY
C!---------+--------+---+---+-------------------------------------------
C! SIGNXX  | NEL    | F |R/W| NEW ELASTO PLASTIC STRESS XX
C! SIGNYY  | NEL    | F |R/W| NEW ELASTO PLASTIC STRESS YY
C! ...     |        |   |   |
C!---------+--------+---+---+-------------------------------------------
C! PLA     | NEL    | F | R | PLASTIC STRAIN
C! DPLA    | NEL    | F | R | INCREMENTAL PLASTIC STRAIN
C! EPSP    | NEL    | F | R | EQUIVALENT STRAIN RATE
C! UVAR    |NEL*NUVAR| F|R/W| USER ELEMENT VARIABLE ARRAY
C! OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C!---------+--------+--+--+-------------------------------------------
C! I N P U T A r g u m e n t s
C!-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,ILAY,IPT,NPTOT,IPTT,IGTYP
      INTEGER ,DIMENSION(NEL), INTENT(IN) :: NGL
       my_real ,DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
       my_real ,DIMENSION(NEL), INTENT(IN)  :: DPLA
c!       my_real OFF(NEL), UEL(NEL)
C
C-----------------------------------------------
C I N P U T  O U T P U T  A r g u m e n t s
C-----------------------------------------------
       INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF
       my_real, INTENT(IN) :: TIME
       my_real ,DIMENSION(NEL), INTENT(IN)  :: SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
       my_real :: PTHKF
       my_real ,DIMENSION(NEL), INTENT(INOUT) :: DFMAX, UEL,OFF
       my_real ,DIMENSION(NEL), INTENT(OUT)   :: TDEL
       my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
C L o c a l V a r i a b l e s
C-----------------------------------------------
       INTEGER :: I,J,K,N,NINDX,IFAIL_SH
       INTEGER, DIMENSION(NEL) :: INDX , CONDITION1, CONDITION2,CONDITION3
C-----------------------------------------------
       my_real AFRAC,BFRAC,CFRAC,NFRAC,INST,P_THICKFAIL
       my_real SIGM,SIGVM,ETA,XI
       my_real LODE,DAM1,DAM2,F1,F2,F3,EPSF
       my_real GHC, G1, G2, G3, DSSE
C-----------------------------------------------
       G1      = ZERO
       G2      = ZERO
       EPSF    = ZERO
       DSSE    = ZERO

       AFRAC        = UPARAM(1)
       BFRAC        = UPARAM(2)
       CFRAC        = UPARAM(3)
       NFRAC        = UPARAM(4)
       INST         = UPARAM(5)
       P_THICKFAIL  = UPARAM(6)
       IFAIL_SH     = UPARAM(7)
       CONDITION1(1:NEL)   = 0
       CONDITION2(1:NEL)   = 0
       CONDITION3(1:NEL)   = 0
       NINDX        = 0  
c---------------------------
       IF (P_THICKFAIL /= ZERO .and. IFAIL_SH > 1 .and. IGTYP <= 10) THEN
         PTHKF = P_THICKFAIL
       ELSEIF (IFAIL_SH == 1) THEN
         PTHKF = EM06
       ELSEIF (IFAIL_SH == 2) THEN
         PTHKF = ONE - EM06
       ENDIF
c-----------------------------
C USER VARIABLES INITIALIZATION
C-----------------------------------------------
       DO I =1,NEL
       
         DAM1 = DFMAX(I)
         DAM2 = UVAR(I,2)
         
         IF (DPLA(I) > ZERO .and. OFF(I) == ONE .and. DAM1 < ONE) THEN
           SIGM   = (SIGNXX(I)+ SIGNYY(I))*THIRD
           SIGVM  = sqrt((SIGNXX(I)**2)+(SIGNYY(I)**2)-(SIGNXX(I)*SIGNYY(I))+(3*SIGNXY(I)**2))
           ETA    = SIGM / MAX(EM10,SIGVM)
           
           XI = -27.0*0.5*ETA*(ETA**2 - THIRD)
           IF (XI < -ONE) XI =-ONE
           IF (XI >  ONE) XI = ONE
           
           LODE = ONE - TWO*ACOS(XI)/PI
           
           F1 = TWO/THREE*COS((ONE   -LODE)*PI/SIX)
           F2 = TWO/THREE*COS((THREE+LODE)*PI/SIX)
           F3 =-TWO/THREE*COS((ONE   +LODE)*PI/SIX)
           
           GHC= (HALF*((F1-F2)**AFRAC
     .                  +(F2-F3)**AFRAC
     .                  +(F1-F3)**AFRAC))**(ONE/AFRAC)
     .                  +CFRAC*(TWO*ETA+F1+F3)
     
           IF (ETA < -THIRD) THEN
             EPSF = 100.00
             GHC  = -ONE
           ELSEIF(ETA < TWO_THIRD)THEN
             GHC= (HALF*(MAX(F1-F2,ZERO)**AFRAC
     .                  +MAX(F2-F3,ZERO)**AFRAC
     .                  +MAX(F1-F3,ZERO)**AFRAC))**(ONE/AFRAC)
     .                  +CFRAC*(TWO*ETA+F1+F3)
             EPSF = BFRAC*((ONE+CFRAC)/GHC)**(1.0/NFRAC)             
             IF (ETA > THIRD .and. DAM2 < ONE) THEN
               G1   = THREE/TWO*ETA+SQRT(THIRD+EM20-0.75*(ETA**2)) 
               G2   = THREE/TWO*ETA-SQRT(THIRD+EM20-0.75*(ETA**2))
               G3   = (HALF*((MAX(ZERO,(G1-G2)))**INST+G1**INST+
     .                (MAX(ZERO,G2))**INST))**(ONE/INST)
               DSSE = BFRAC * G3**(-100)
               DAM2 = DAM2 + DPLA(I)/MAX(EM6,DSSE)
               UVAR(I,2) = DAM2
             ENDIF
           ELSE
             ETA  = TWO_THIRD
             F1   = ONE/THREE
             F2   = ONE/THREE
             F3   = -TWO/THREE
             
             GHC= (HALF*(MAX(F1-F2,ZERO)**AFRAC
     .                  +MAX(F2-F3,ZERO)**AFRAC
     .                  +MAX(F1-F3,ZERO)**AFRAC))**(ONE/AFRAC)
     .                  +CFRAC*(TWO*ETA+F1+F3)
             
             EPSF = BFRAC*(ONE+CFRAC)**(ONE/NFRAC)
     .            * ((HALF*(MAX(F1-F2,ZERO)**AFRAC  
     .                       +MAX(F2-F3,ZERO)**AFRAC
     .                       +MAX(F1-F3,ZERO)**AFRAC))**(ONE/AFRAC)
     .            +   CFRAC*(TWO*ETA+F1+F3))**(-ONE/NFRAC)
             
               
             ENDIF
             
           DAM1     = DAM1 + DPLA(I)/MAX(EM6,EPSF)
           DFMAX(I) = MIN(ONE,DAM1)
           IF (DAM1 >= ONE) THEN                    ! failure of integrateion points
             NINDX = NINDX + 1
             INDX(NINDX) = I  
             FOFF(I) = 0
             UEL(I)  = ZERO
             TDEL(I) = TIME
             CONDITION1(NINDX) = IPT
           ENDIF
           
           IF (DAM2 >= ONE .AND. UVAR(I,1) == ZERO .AND. FOFF(I) == 1)THEN  
             UVAR(I,1) = ONE           
             UEL(I) = UEL(I) + ONE
             IF (INT(UEL(I)+1e-6) == NPTOT) THEN   ! rupture due localized necking !!!
               NINDX = NINDX + 1
               INDX(NINDX) = I  
               TDEL(I)= TIME
               OFF(I) = ZERO
               CONDITION2(NINDX) = 1
               CONDITION3(NINDX) = 1
             ENDIF
           ENDIF
           
         ENDIF
       ENDDO
C!     
c------------------------
      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)     
#include "lockon.inc"
         IF(CONDITION1(J) >= 1)  THEN         
           WRITE(IOUT, 2000) NGL(I),CONDITION1(J),TIME
           WRITE(ISTDO,2000) NGL(I),CONDITION1(J),TIME
         ENDIF
         IF(CONDITION3(J)==1) THEN
           WRITE(ISTDO,3000) NGL(I)
           WRITE(IOUT, 3000) NGL(I)
         ENDIF          
         IF(CONDITION2(J)==1) THEN
           WRITE(ISTDO,2200) NGL(I), TIME
           WRITE(IOUT, 2200) NGL(I), TIME
         ENDIF          
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDX             
c------------------------
C---------Damage for output  0 < DFMAX < 1 --------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (HC-DSSE)',I10,1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)

 2200 FORMAT(1X,' *** RUPTURE OF SHELL ELEMENT (HC-DSSE)',I10,1X,
     . ' AT TIME :',1PE12.4)
 3000 FORMAT(1X,'FOR SHELL ELEMENT (HC-DSSE)',I10,
     .       1X,'INSTABILITY REACHED.')
c------------------------
c! 2000 FORMAT(1X,'FAILURE (BIQUAD) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
c!     .       1X,',INTEGRATION PT',I3)
c! 2100 FORMAT(1X,'FAILURE (BIQUAD) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
c!     .       1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
C!
       RETURN
       END
