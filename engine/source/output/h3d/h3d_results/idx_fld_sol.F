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
Chd|  IDX_FLD_SOL                   source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|-- called by -----------
Chd|        H3D_SOL_SKIN_SCALAR1          source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTERFLD                     source/materials/fail/fld/fail_fld_c.F
Chd|====================================================================
      SUBROUTINE IDX_FLD_SOL(
     1     NEL      ,NUPARAM  ,NFUNC    ,IFUNC    ,
     2     NPF      ,TF       ,UPARAM   ,EPS3     ,FLD_IDX  )
C-----------------------------------------------
c    FLD failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C---------+---------+---+---+--------------------------------------------
C EPS3    | NEL*3   | F | R | IN PLANE STRAIN TENSOR
C---------+---------+---+---+--------------------------------------------
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NFUNC
      INTEGER ,DIMENSION(NFUNC) :: IFUNC
      my_real ,DIMENSION(3,NEL), INTENT(IN) ::  EPS3
      my_real,DIMENSION(NUPARAM) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: FLD_IDX
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
      INTEGER :: I,II,J,IENG,LENF,NINDX,IMARGIN
      my_real :: RANI,R1,R2,S1,S2,SS,Q,DYDX,E12,FACT_MARGIN,FACT_LOOSEMETAL
      my_real ,ALLOCATABLE, DIMENSION(:) :: XF  
      my_real ,DIMENSION(NEL) :: EMAJ,EMIN,EM
C=======================================================================
      IENG     = INT(UPARAM(6))
      RANI     = UPARAM(7)
      IMARGIN  = NINT(UPARAM(2))
      FACT_MARGIN     = UPARAM(3) 
      FACT_LOOSEMETAL = UPARAM(8)
c-----------------------------
C
c --- Minor and major (true) strain deformation.
C
      DO I = 1,NEL
        E12= EPS3(3,I)
        S1 = HALF*(EPS3(1,I) + EPS3(2,I))
        S2 = HALF*(EPS3(1,I) - EPS3(2,I))
        Q  = SQRT(S2**2 + E12**2)
        EMAJ(I) = S1 + Q
        EMIN(I) = S1 - Q
        IF (EMIN(I) >= EMAJ(I)) THEN
          SS      = EMIN(I)
          EMIN(I) = EMAJ(I)
          EMAJ(I) = SS
        ENDIF
      ENDDO
c-----
c     failure major strain from input curve and damage ratio  : 0 < DFMAX < 1 
c-----
      IF (IENG == 1) THEN   ! transform input fld curve to true strain
        II   = NPF(IFUNC(1))
        LENF = NPF(IFUNC(1)+ 1) - NPF(IFUNC(1))
        ALLOCATE(XF(LENF))
        DO I = 1,LENF
          XF(I) = LOG(TF(II + I-1) + ONE)
        ENDDO
c
        DO I = 1,NEL
          EM(I)    = FINTERFLD(EMIN(I),IFUNC(1),LENF,XF)
        ENDDO
        DEALLOCATE(XF)
      ELSE
        DO I = 1,NEL
          EM(I)    = FINTER(IFUNC(1),EMIN(I),NPF,TF,DYDX)
        ENDDO
      ENDIF 
c--------------------------------------------------------------------
c     FLD zone index calculation for ANIM output
c
      R1 = FACT_LOOSEMETAL
      R2 = RANI/(RANI+ONE)
      
      IF (IMARGIN == 3) THEN
        DO I = 1,NEL
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
        DO I = 1,NEL
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
C------------------------
      RETURN
      END
