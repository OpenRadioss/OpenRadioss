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
      !||====================================================================
      !||    fail_fld_c      ../engine/source/materials/fail/fld/fail_fld_c.F
      !||--- called by ------------------------------------------------------
      !||    mulawc          ../engine/source/materials/mat_share/mulawc.F
      !||    usermat_shell   ../engine/source/materials/mat_share/usermat_shell.F
      !||--- calls      -----------------------------------------------------
      !||    finter          ../engine/source/tools/curve/finter.F
      !||    finterfld       ../engine/source/materials/fail/fld/fail_fld_c.F
      !||    vinter          ../engine/source/tools/curve/vinter.F
      !||====================================================================
      SUBROUTINE FAIL_FLD_C(
     1     NEL      ,NUPARAM  ,NFUNC    ,IFUNC    ,
     2     NPF      ,TF       ,TIME     ,UPARAM   ,
     3     NGL      ,IPG      ,ILAY     ,IPTT     ,
     4     EPSXX    ,EPSYY    ,EPSXY    ,
     5     DEPSXX   ,DEPSYY   ,DEPSXY   ,PLA      ,
     6     ZT       ,OFF      ,FOFF     ,TDEL     ,
     7     FLD_IDX  ,DAM      ,DFMAX    ,DT1      ,
     8     NIPARAM  ,IPARAM   ,NUVAR    ,UVAR     )
C-----------------------------------------------
c    FLD failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NFUNC,IPG,ILAY,IPTT,NIPARAM,NUVAR
      INTEGER, DIMENSION(NEL)   :: NGL
      INTEGER, DIMENSION(NFUNC) :: IFUNC
      INTEGER, DIMENSION(NIPARAM) :: IPARAM
      my_real, INTENT(IN) :: TIME,ZT,DT1
      my_real, DIMENSION(NEL), INTENT(IN) :: OFF,
     .   EPSXX,EPSYY,EPSXY,DEPSXX,DEPSYY,DEPSXY,PLA
      my_real, DIMENSION(NUPARAM) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF,FLD_IDX
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: DAM,TDEL
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER , FINTERFLD ,TF(*)
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
      INTEGER :: I,II,J,IENG,LENF,NINDX,NINDXF,IFAIL_SH,ISTRESS,IMARGIN
      INTEGER ,DIMENSION(NEL) :: INDX,INDXF
      my_real :: RANI,R1,R2,S1,S2,SS,Q,E12,FACT_MARGIN,FACT_LOOSEMETAL,
     .    FCUT,ALPHA
      my_real ,ALLOCATABLE, DIMENSION(:) :: XF  
      my_real ,DIMENSION(NEL) :: EMAJ,EMIN,EM,DEMAJ,DEMIN,BETA
      INTEGER, DIMENSION(NEL) :: IPOS,ILENP,IADP
      my_real, DIMENSION(NEL) :: TAB_LOC,Y_LOC,DYDX_LOC
C=======================================================================
c
      !=================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=================================================================
      ! Recovering failure criterion parameters
      ! -> Real parameters
      FACT_MARGIN     = UPARAM(1)
      RANI            = UPARAM(3)
      FACT_LOOSEMETAL = UPARAM(4)
      FCUT            = UPARAM(5)
      IF (UPARAM(6) > ZERO) THEN 
        ALPHA = UPARAM(6) 
      ELSE 
        ALPHA = TWO*PI*FCUT*DT1/(ONE + TWO*PI*FCUT*DT1)
      ENDIF
      ! -> Integer parameters
      IFAIL_SH = IPARAM(1)
      IMARGIN  = IPARAM(2)
      IENG     = IPARAM(3)
c
      ! Shell element deletion
      ISTRESS = 0
      IF (IFAIL_SH == 1) THEN
        ISTRESS = 0
      ELSEIF (IFAIL_SH == 2) THEN
        ISTRESS = 0
      ELSEIF (IFAIL_SH == 3) THEN  ! membrane criterion only
        ISTRESS = 0
      ELSEIF (IFAIL_SH == 4) THEN  ! no element suppression
        ISTRESS = -1
      ENDIF
c
      ! Check if the element is already broken
      NINDX  = 0
      NINDXF = 0  
      DO I = 1,NEL
        IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
          NINDX = NINDX + 1
          INDX(NINDX) = I
        ENDIF
      ENDDO 
c
      !=================================================================
      ! - MINOR AND MAJOR (TRUE) STRAIN DEFORMATION
      !=================================================================
#include "vectorize.inc"
      DO J = 1,NINDX
        I = INDX(J)
        ! For classic linear formulation
        E12 = HALF*EPSXY(I)
        S1  = HALF*(EPSXX(I) + EPSYY(I))
        S2  = HALF*(EPSXX(I) - EPSYY(I))
        Q   = SQRT(S2**2 + E12**2)
        EMAJ(I) = S1 + Q
        EMIN(I) = S1 - Q
        IF (EMIN(I) >= EMAJ(I)) THEN
          SS      = EMIN(I)
          EMIN(I) = EMAJ(I)
          EMAJ(I) = SS
        ENDIF
        ! For non-linear path formulation
        E12 = HALF*DEPSXY(I)
        S1  = HALF*(DEPSXX(I) + DEPSYY(I))
        S2  = HALF*(DEPSXX(I) - DEPSYY(I))
        Q   = SQRT(S2**2 + E12**2)
        DEMAJ(I)  = S1 + Q
        DEMIN(I)  = S1 - Q   
        ! Strain ratio filtering value for non-linear path formulation
        DEMAJ(I)  = ALPHA*DEMAJ(I) + (ONE - ALPHA)*UVAR(I,2) 
        DEMIN(I)  = ALPHA*DEMIN(I) + (ONE - ALPHA)*UVAR(I,3) 
        BETA(I)   = DEMIN(I)/SIGN(MAX(ABS(DEMAJ(I)),EM20),DEMAJ(I))
        UVAR(I,2) = DEMAJ(I)
        UVAR(I,3) = DEMIN(I)
      ENDDO
c
      !=================================================================
      ! FAILURE MAJOR STRAIN FROM INPUT CURVE AND DAMAGE RATIO
      !=================================================================
      ! -> Engineering strains input
      IF (IENG == 1) THEN   ! transform input fld curve to true strain
        II   = NPF(IFUNC(1))
        LENF = NPF(IFUNC(1)+ 1) - NPF(IFUNC(1))
        ALLOCATE(XF(LENF))
        DO I = 1,LENF
          XF(I) = LOG(TF(II + I-1) + ONE)
        ENDDO
c
#include "vectorize.inc"
        DO J = 1,NINDX
          I = INDX(J)
          EM(I)    = FINTERFLD(EMIN(I),LENF,XF)
          DAM(I)   = EMAJ(I) / EM(I)
          DFMAX(I) = MIN(ONE, DAM(I))
        ENDDO
        DEALLOCATE(XF)
      ! -> True strains input
      ELSE
        ! -> Classical formulation
        IF (IENG == 0) THEN
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IPOS(J) = 1
            IADP(J) = NPF(IFUNC(1)) / 2 + 1
            ILENP(J) = NPF(IFUNC(1)+1) / 2 -IADP(J) - IPOS(J)
            TAB_LOC(J) = EMIN(I)
          ENDDO
          CALL VINTER(TF,IADP,IPOS,ILENP,NINDX,TAB_LOC,DYDX_LOC,Y_LOC)
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            EM(I) = Y_LOC(J)
            DAM(I) = EMAJ(I) / EM(I)
            DFMAX(I) = MIN(ONE,DAM(I))
          ENDDO    
        ! -> Non-linear path formulation
        ELSEIF (IENG == 2) THEN
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IPOS(J) = 1
            IADP(J) = NPF(IFUNC(1)) / 2 + 1
            ILENP(J) = NPF(IFUNC(1)+1) / 2 -IADP(J) - IPOS(J)
            TAB_LOC(J) = BETA(I)
          ENDDO
          CALL VINTER(TF,IADP,IPOS,ILENP,NINDX,TAB_LOC,DYDX_LOC,Y_LOC)
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            EM(I) = Y_LOC(J)
            DAM(I) = MAX(PLA(I) / EM(I),DAM(I))
            DFMAX(I) = MIN(ONE,DAM(I))
          ENDDO             
        ENDIF
      ENDIF 
c
      !=================================================================
      ! FLD ZONE INDEX CALCULATION FOR ANIM OUTPUT
      !=================================================================
      R1 = FACT_LOOSEMETAL
      R2 = RANI/(RANI+ONE)
      
      IF (IENG < 2) THEN 
        IF (IMARGIN == 3) THEN
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IF (EMAJ(I) >= EM(I)) THEN
              FLD_IDX(I) = 6      ! zone 6 = failure
            ELSEIF (EMAJ(I) >= EM(I)*(ONE - FACT_MARGIN)) THEN
              FLD_IDX(I) = 5      ! zone 5 = margin to fail
            ELSEIF (EMAJ(I)**2 + EMIN(I)**2 < R1**2) THEN
              FLD_IDX(I) = 1      ! zone 1 = radius 0.02
            ELSEIF (EMAJ(I) >= ABS(EMIN(I))) THEN
              FLD_IDX(I) = 4      ! zone 4 = safe (45 deg line)
            ELSEIF (EMAJ(I) >= R2*ABS(EMIN(I))) THEN
              FLD_IDX(I) = 3      ! zone 3  = angle atan(r/(1+r))  - compression
            ELSE
              FLD_IDX(I) = 2      ! zone 2  - high wrinkle tendency
            ENDIF
          ENDDO
        ELSE
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IF (EMAJ(I) >= EM(I)) THEN
              FLD_IDX(I) = 6      ! zone 6 = failure
            ELSEIF (EMAJ(I) >= EM(I) - FACT_MARGIN) THEN
              FLD_IDX(I) = 5      ! zone 5 = margin to fail
            ELSEIF (EMAJ(I)**2 + EMIN(I)**2 < R1**2) THEN
              FLD_IDX(I) = 1      ! zone 1 = radius 0.02
            ELSEIF (EMAJ(I) >= ABS(EMIN(I))) THEN
              FLD_IDX(I) = 4      ! zone 4 = safe (45 deg line)
            ELSEIF (EMAJ(I) >= R2*ABS(EMIN(I))) THEN
              FLD_IDX(I) = 3      ! zone 3  = angle atan(r/(1+r))  - compression
            ELSE
              FLD_IDX(I) = 2      ! zone 2  - high wrinkle tendency
            ENDIF
          ENDDO
        ENDIF
      ELSE 
        IF (IMARGIN == 3) THEN
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IF (PLA(I) >= EM(I)) THEN
              FLD_IDX(I) = MAX(6,FLD_IDX(I))      ! zone 6 = failure
            ELSEIF (PLA(I) >= EM(I)*(ONE - FACT_MARGIN)) THEN
              FLD_IDX(I) = MAX(5,FLD_IDX(I))      ! zone 5 = margin to fail
            ELSEIF (PLA(I)**2 + BETA(I)**2 < R1**2) THEN
              FLD_IDX(I) = MAX(1,FLD_IDX(I))      ! zone 1 = radius 0.02
            ELSEIF (PLA(I) >= ABS(BETA(I))) THEN
              FLD_IDX(I) = MAX(4,FLD_IDX(I))      ! zone 4 = safe (45 deg line)
            ELSEIF (PLA(I) >= R2*ABS(BETA(I))) THEN
              FLD_IDX(I) = MAX(3,FLD_IDX(I))      ! zone 3  = angle atan(r/(1+r))  - compression
            ELSE
              FLD_IDX(I) = MAX(2,FLD_IDX(I))      ! zone 2  - high wrinkle tendency
            ENDIF
          ENDDO
        ELSE
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IF (PLA(I) >= EM(I)) THEN
              FLD_IDX(I) = MAX(6,FLD_IDX(I))      ! zone 6 = failure
            ELSEIF (PLA(I) >= EM(I) - FACT_MARGIN) THEN
              FLD_IDX(I) = MAX(5,FLD_IDX(I))      ! zone 5 = margin to fail
            ELSEIF (PLA(I)**2 + BETA(I)**2 < R1**2) THEN
              FLD_IDX(I) = MAX(1,FLD_IDX(I))      ! zone 1 = radius 0.02
            ELSEIF (PLA(I) >= ABS(BETA(I))) THEN
              FLD_IDX(I) = MAX(4,FLD_IDX(I))      ! zone 4 = safe (45 deg line)
            ELSEIF (PLA(I) >= R2*ABS(BETA(I))) THEN
              FLD_IDX(I) = MAX(3,FLD_IDX(I))      ! zone 3  = angle atan(r/(1+r))  - compression
            ELSE
              FLD_IDX(I) = MAX(2,FLD_IDX(I))      ! zone 2  - high wrinkle tendency
            ENDIF
          ENDDO
        ENDIF
      ENDIF
c
      !=================================================================
      ! PRINTING OUT ELEMENT DELETION MESSAGES
      !=================================================================
      IF ((IFAIL_SH == 3 .and. ZT == ZERO) .or. IFAIL_SH < 3) THEN
        IF (IENG < 2) THEN 
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IF (EMAJ(I) >= EM(I)) THEN
              NINDXF = NINDXF + 1
              INDXF(NINDXF) = I
              FOFF(I) = ISTRESS
              TDEL(I) = TIME
            ENDIF
          ENDDO
        ELSE
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            IF (PLA(I) >= EM(I)) THEN
              NINDXF = NINDXF + 1
              INDXF(NINDXF) = I
              FOFF(I) = ISTRESS
              TDEL(I) = TIME
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      IF (NINDXF > 0) THEN        
        DO J=1,NINDXF             
          I = INDXF(J)            
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPTT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPTT,TIME
#include "lockoff.inc" 
        END DO                   
      END IF       
c------------------------
 2000 FORMAT(1X,'FAILURE (FLD) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
     .       1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (FLD) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
     .       1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      END
c      
      !||====================================================================
      !||    finterfld       ../engine/source/materials/fail/fld/fail_fld_c.F
      !||--- called by ------------------------------------------------------
      !||    dam_fld_sol     ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
      !||    fail_fld_c      ../engine/source/materials/fail/fld/fail_fld_c.F
      !||    fail_fld_tsh    ../engine/source/materials/fail/fld/fail_fld_tsh.F
      !||    fail_fld_xfem   ../engine/source/materials/fail/fld/fail_fld_xfem.F
      !||    idx_fld_sol     ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
      !||====================================================================
      my_real FUNCTION FINTERFLD(EPST,LENF,XF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENF
      my_real XF(LENF),EPST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real DX1,DX2,DERI
C=======================================================================
      FINTERFLD = ZERO
      DX2 = XF(1) - EPST
      DO I = 2,LENF-2,2
        DX1 = -DX2
        DX2 = XF(I+1) - EPST
        IF (DX2 >= ZERO .OR. I == LENF-2) THEN
          DERI = (XF(I+2) - XF(I)) / (XF(I+1) - XF(I-1))
          IF (DX1 <= DX2) THEN
            FINTERFLD = XF(I) + DX1 * DERI
          ELSE
            FINTERFLD = XF(I+2) - DX2 * DERI
          ENDIF
          EXIT
        ENDIF
      ENDDO
c-----------
      RETURN
      END
