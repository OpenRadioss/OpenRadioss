Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_fld_tsh     ../engine/source/materials/fail/fld/fail_fld_tsh.F
!||--- called by ------------------------------------------------------
!||    h3d_fld_tsh      ../engine/source/output/h3d/h3d_results/h3d_fld_tsh.F
!||--- calls      -----------------------------------------------------
!||    finter           ../engine/source/tools/curve/finter.F
!||    finterfld        ../engine/source/materials/fail/fld/fail_fld_c.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod     ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE FAIL_FLD_TSH(FAIL,
     1     NEL      ,NUVAR    ,UVAR     ,TIME     ,TIMESTEP ,
     3     NGL      ,IPG      ,ILAY     ,IPTT     ,
     4     EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     6     OFF      ,FOFF     ,FLD_IDX  ,DAM      ,DFMAX    ,
     7     PLA      ,LF_DAMMX )
C-----------------------------------------------
c    FLD failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
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
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUVAR,IPG,ILAY,IPTT,LF_DAMMX
      INTEGER ,DIMENSION(NEL)   :: NGL
      my_real TIME
      my_real ,DIMENSION(NEL), INTENT(IN) ::  TIMESTEP,OFF,
     .              EPSXX,EPSYY,EPSXY,EPSYZ,EPSZX,PLA
      type (fail_param_) ,intent(inout) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF,FLD_IDX
      my_real ,DIMENSION(NEL,LF_DAMMX), INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: DAM
      my_real UVAR(NEL,NUVAR)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IENG,LENF,NINDX,IMARGIN
      INTEGER ,DIMENSION(NEL) :: INDX,IPOS
      my_real :: RANI,R1,R2,S1,S2,SS,Q,E12,FACT_MARGIN,FACT_LOOSEMETAL
      my_real ,ALLOCATABLE, DIMENSION(:) :: XF,YF
      my_real ,DIMENSION(NEL) :: EMAJ,EMIN,EM,BETA
      my_real, DIMENSION(NEL) :: TAB_LOC,Y_LOC,DYDX
C=======================================================================
c
      !=================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=================================================================
      ! Recovering failure criterion parameters
      ! -> Real parameters
      FACT_MARGIN     = FAIL%UPARAM(1)
      RANI            = FAIL%UPARAM(3)
      FACT_LOOSEMETAL = FAIL%UPARAM(4)
      ! -> Integer parameters
      IMARGIN  = FAIL%IPARAM(2)
      IENG     = FAIL%IPARAM(3)
      IPOS(:) = 1
c
      ! Check if the element is already broken
      NINDX  = 0
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
      DO J = 1,NINDX
        I = INDX(J)
        E12= HALF*EPSXY(I)
        S1 = HALF*(EPSXX(I) + EPSYY(I))
        S2 = HALF*(EPSXX(I) - EPSYY(I))
        Q  = SQRT(S2**2 + E12**2)
        EMAJ(I) = S1 + Q
        EMIN(I) = S1 - Q
        IF (EMIN(I) >= EMAJ(I)) THEN
          SS      = EMIN(I)
          EMIN(I) = EMAJ(I)
          EMAJ(I) = SS
        ENDIF
        BETA(I) = EMIN(I)/MAX(EMAJ(I),EM20)
        IF (IENG == 2) THEN
          DFMAX(I,4) = BETA(I)
        ENDIF 
      ENDDO
c
      !=================================================================
      ! FAILURE MAJOR STRAIN FROM INPUT CURVE AND DAMAGE RATIO
      !=================================================================
      ! -> Engineering strains input
      IF (IENG == 1) THEN   ! transform input fld curve to true strain
        LENF = size(fail%table4d(1)%x(1)%values)
        call my_alloc(XF, LENF, "XF")
        call my_alloc(YF, LENF, "YF")
        DO I = 1,LENF
          XF(I) = LOG(fail%table4d(1)%x(1)%values(i) + ONE)
          YF(I) = fail%table4d(1)%y1d(i)
        ENDDO
        call vinter_1d(nindx,nel,lenf,ipos,xf,yf,emin,y_loc)
c
#include "vectorize.inc"
        DO J = 1,NINDX
          I = INDX(J)
          EM(I) = Y_LOC(J)
          DAM(I) = EMAJ(I) / EM(I)
          DFMAX(I,2) = DAM(I)
          DFMAX(I,1) = MIN(ONE, DAM(I))
        ENDDO
        call my_dealloc(XF)
        call my_dealloc(YF)
      ! -> True strains input
      ELSE
        ! -> Classical formulation
        IF (IENG == 0) THEN 
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            TAB_LOC(J) = EMIN(I)
          ENDDO
          call table_mat_vinterp(fail%table4d(1),nindx,nel,ipos,tab_loc,y_loc,dydx)
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            EM(I)  = Y_LOC(J)
            DAM(I) = EMAJ(I) / EM(I)
            DFMAX(I,2) = DAM(I)
            DFMAX(I,1) = MIN(ONE, DAM(I))
          ENDDO
        ! -> Non-linear path formulation
        ELSEIF (IENG == 2) THEN 
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            TAB_LOC(J) = BETA(I)
          ENDDO
          call table_mat_vinterp(fail%table4d(1),nindx,nel,ipos,tab_loc,y_loc,dydx)
#include "vectorize.inc"
          DO J = 1,NINDX
            I = INDX(J)
            EM(I)  = Y_LOC(J)
            DAM(I) = EMAJ(I) / EM(I)
            DFMAX(I,2) = DAM(I)
            DFMAX(I,1) = MIN(ONE, DAM(I))
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
            DFMAX(I,3) = FLD_IDX(I)
          ENDDO
        ELSE
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
            DFMAX(I,3) = FLD_IDX(I)
          ENDDO
        ENDIF
      ELSE
        IF (IMARGIN == 3) THEN
          DO J = 1,NINDX
            I = INDX(J)
            IF (PLA(I) >= EM(I)) THEN
              FLD_IDX(I) = 6      ! zone 6 = failure
            ELSEIF (PLA(I) >= EM(I)*(ONE - FACT_MARGIN)) THEN
              FLD_IDX(I) = 5      ! zone 5 = margin to fail
            ELSEIF (PLA(I)**2 + BETA(I)**2 < R1**2) THEN
              FLD_IDX(I) = 1      ! zone 1 = radius 0.02
            ELSEIF (PLA(I) >= ABS(BETA(I))) THEN
              FLD_IDX(I) = 4      ! zone 4 = safe (45 deg line)
            ELSEIF (PLA(I) >= R2*ABS(BETA(I))) THEN
              FLD_IDX(I) = 3      ! zone 3  = angle atan(r/(1+r))  - compression
            ELSE
              FLD_IDX(I) = 2      ! zone 2  - high wrinkle tendency
            ENDIF
            DFMAX(I,3) = FLD_IDX(I)
          ENDDO
        ELSE
          DO J = 1,NINDX
            I = INDX(J)
            IF (PLA(I) >= EM(I)) THEN
              FLD_IDX(I) = 6      ! zone 6 = failure
            ELSEIF (PLA(I) >= EM(I) - FACT_MARGIN) THEN
              FLD_IDX(I) = 5      ! zone 5 = margin to fail
            ELSEIF (PLA(I)**2 + BETA(I)**2 < R1**2) THEN
              FLD_IDX(I) = 1      ! zone 1 = radius 0.02
            ELSEIF (PLA(I) >= ABS(BETA(I))) THEN
              FLD_IDX(I) = 4      ! zone 4 = safe (45 deg line)
            ELSEIF (PLA(I) >= R2*ABS(BETA(I))) THEN
              FLD_IDX(I) = 3      ! zone 3  = angle atan(r/(1+r))  - compression
            ELSE
              FLD_IDX(I) = 2      ! zone 2  - high wrinkle tendency
            ENDIF
            DFMAX(I,3) = FLD_IDX(I)
          ENDDO
        ENDIF
      ENDIF
C------------------------
      RETURN
      END
c      
