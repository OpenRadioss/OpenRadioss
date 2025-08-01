Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    h3d_sol_skin_scalar    ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||--- called by ------------------------------------------------------
!||    h3d_skin_scalar        ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
!||--- calls      -----------------------------------------------------
!||    gpsstrain_skin         ../engine/source/output/anim/generate/tensgpstrain.F
!||    h3d_sol_skin_scalar1   ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
!||    spmd_exch_nodarea2     ../engine/source/mpi/anim/spmd_exch_nodarea2.F
!||    spmd_exch_nodareai     ../engine/source/mpi/anim/spmd_exch_nodareai.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod            ../engine/share/resol/initbuf.F
!||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE H3D_SOL_SKIN_SCALAR(
     .                   ELBUF_TAB,SKIN_SCALAR, IPARG   ,IXS     ,X     ,PM  ,
     4                   IPARTS  ,IGEO    ,IXS10 ,IXS16 , IXS20  ,
     5                   IS_WRITTEN_SKIN  ,H3D_PART,INFO1   ,KEYWORD ,NSKIN ,
     6                   IAD_ELEM        ,FR_ELEM     , WEIGHT  , TAG_SKINS6,
     7                   NPF  ,TF   ,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE MAT_ELEM_MOD
      USE ELBUFDEF_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   SKIN_SCALAR(*),PM(NPROPM,*), X(3,*),TF(*)
      INTEGER IPARG(NPARG,*), 
     .   IXS(NIXS,*),IPARTS(*),
     .   IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,
     .   IGEO(NPROPGI,*),IS_WRITTEN_SKIN(*),NPF(*),
     .   H3D_PART(*),INFO1,NSKIN,TAG_SKINS6(*),IAD_ELEM(2,*),FR_ELEM(*),WEIGHT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER(LEN=NCHARLINE100)::KEYWORD
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,I1,II,J,LENR,NEL,NFT,N

      INTEGER JJ,N1,N2
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGPS,TAG_SKIN_ND    
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: AFLU, VFLU,T6GPS
      INTEGER FACES(4,6),NS,K1,PWR(7),LL
      DATA PWR/1,2,4,8,16,32,64/
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
C-----------------------------------------------
C
      ALLOCATE(AFLU(3,NUMNOD),VFLU(3,NUMNOD),T6GPS(6,NUMNOD))
      ALLOCATE(ITAGPS(NUMNOD),TAG_SKIN_ND(NUMNOD))
      AFLU  = ZERO
      VFLU  = ZERO
      T6GPS = ZERO
      ITAGPS = 0
C------TAG_SKIN_ND only the big seg(mid-node of S10 not include)
      TAG_SKIN_ND = 0 
        DO I=1,NUMELS
            LL=TAG_SKINS6(I)
            DO JJ=1,6
              IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
              DO K1=1,4
                NS=IXS(FACES(K1,JJ)+1,I)
                TAG_SKIN_ND(NS) = 1 
              END DO
            END DO
        END DO
       IF (KEYWORD == 'FLDZ/OUTER' .OR. KEYWORD == 'FLDF/OUTER') THEN
         CALL GPSSTRAIN_SKIN(ELBUF_TAB,VFLU ,AFLU    ,IPARG   ,
     .                    IXS      ,IXS10   ,IXS16   ,IXS20   ,X        ,
     .                    ITAGPS  ,PM    ,TAG_SKIN_ND )
         IF(NSPMD > 1)THEN
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
           DO J=1,3
            CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
           ENDDO
         ENDIF
         DO N=1,NUMNOD
          IF (ITAGPS(N)>0) T6GPS(1:3,N)=VFLU(1:3,N)/ITAGPS(N)
         ENDDO
C------------change shear to eij         
         DO N=1,NUMNOD
           IF (ITAGPS(N)>0) T6GPS(4:6,N)=HALF*AFLU(1:3,N)/ITAGPS(N)
         ENDDO
       END IF
C       
      CALL H3D_SOL_SKIN_SCALAR1(ELBUF_TAB,IPARG,IPARTS,IXS,IXS10,
     .                          SKIN_SCALAR,TAG_SKINS6,T6GPS,X  ,
     .                          NPF,TF,H3D_PART,IS_WRITTEN_SKIN,
     .                          KEYWORD,NSKIN,MAT_PARAM)
C
       DEALLOCATE(AFLU,VFLU,T6GPS,ITAGPS,TAG_SKIN_ND)
C-----------
      RETURN
      END 
!||====================================================================
!||    idx_fld_sol            ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||--- called by ------------------------------------------------------
!||    h3d_sol_skin_scalar1   ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
!||--- calls      -----------------------------------------------------
!||    finter                 ../engine/source/tools/curve/finter.F
!||    finterfld              ../engine/source/materials/fail/fld/fail_fld_c.F
!||====================================================================
      SUBROUTINE IDX_FLD_SOL(
     1     NEL      ,NUPARAM  ,NFUNC    ,IFUNC    ,
     2     NPF      ,TF       ,UPARAM   ,EPS3     ,FLD_IDX  ,
     3     NIPARAM  ,IPARAM   ,PLA      )
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
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NFUNC,NIPARAM
      INTEGER ,DIMENSION(NFUNC) :: IFUNC
      INTEGER, DIMENSION(NIPARAM), INTENT(IN) :: IPARAM
      my_real ,DIMENSION(3,NEL), INTENT(IN) ::  EPS3
      my_real,DIMENSION(NUPARAM) :: UPARAM
      my_real ,DIMENSION(NEL), INTENT(IN) :: PLA
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
      my_real ,DIMENSION(NEL) :: EMAJ,EMIN,EM,BETA
C=======================================================================
c
      ! -> Real parameters
      FACT_MARGIN     = UPARAM(1)
      RANI            = UPARAM(3)
      FACT_LOOSEMETAL = UPARAM(4)
      ! -> Integer parameters
      IMARGIN  = IPARAM(2)
      IENG     = IPARAM(3)
c
      !=================================================================
      ! - MINOR AND MAJOR (TRUE) STRAIN DEFORMATION
      !=================================================================
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
        BETA(I) = EMIN(I)/MAX(EMAJ(I),EM20)
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
        DO I = 1,NEL
          EM(I) = FINTERFLD(EMIN(I),LENF,XF)
        ENDDO
        DEALLOCATE(XF)
      ! -> True strains input
      ELSE
        ! -> Classical formulation
        IF (IENG == 0) THEN 
          DO I = 1,NEL
            EM(I) = FINTER(IFUNC(1),EMIN(I),NPF,TF,DYDX)
          ENDDO
        ! -> Non-linear path formulation
        ELSEIF (IENG == 2) THEN           
          DO I = 1,NEL
            EM(I) = FINTER(IFUNC(1),BETA(I),NPF,TF,DYDX)
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
      ELSE
        IF (IMARGIN == 3) THEN
          DO I = 1,NEL
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
          ENDDO
        ELSE
          DO I = 1,NEL
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
          ENDDO
        ENDIF
      ENDIF
C------------------------
      RETURN
      END
!||====================================================================
!||    dam_fld_sol            ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||--- called by ------------------------------------------------------
!||    h3d_sol_skin_scalar1   ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
!||--- calls      -----------------------------------------------------
!||    finter                 ../engine/source/tools/curve/finter.F
!||    finterfld              ../engine/source/materials/fail/fld/fail_fld_c.F
!||====================================================================
      SUBROUTINE DAM_FLD_SOL(
     1     NEL      ,NUPARAM  ,NFUNC    ,IFUNC    ,
     2     NPF      ,TF       ,UPARAM   ,EPS3     ,DAM,
     3     NIPARAM  ,IPARAM   ,PLA      )
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
C EPS3    | NEL*3   | F | R | IN PLANE STRAIN TENSOR
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DAM     | NEL     | F |R/W| DAMAGE FACTOR 
C---------+---------+---+---+--------------------------------------------
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NFUNC,NIPARAM
      INTEGER ,DIMENSION(NFUNC) :: IFUNC
      INTEGER, DIMENSION(NIPARAM), INTENT(IN) :: IPARAM
      my_real ,DIMENSION(3,NEL), INTENT(IN) ::  EPS3
      my_real,DIMENSION(NUPARAM) :: UPARAM
      my_real ,DIMENSION(NEL), INTENT(IN) :: PLA
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: DAM
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
      my_real ,DIMENSION(NEL) :: EMAJ,EMIN,EM,BETA
C=======================================================================
c
      ! -> Real parameters
      FACT_MARGIN     = UPARAM(1)
      RANI            = UPARAM(3)
      FACT_LOOSEMETAL = UPARAM(4)
      ! -> Integer parameters
      IMARGIN  = IPARAM(2)
      IENG     = IPARAM(3)
c
      !=================================================================
      ! - MINOR AND MAJOR (TRUE) STRAIN DEFORMATION
      !=================================================================
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
        BETA(I) = EMIN(I)/MAX(EMAJ(I),EM20)
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
        DO I = 1,NEL
          EM(I)    = FINTERFLD(EMIN(I),LENF,XF)
          DAM(I)   = EMAJ(I) / EM(I)
c          DFMAX(I) = MIN(ONE, DAM(I))
        ENDDO
        DEALLOCATE(XF)
      ! -> True strains input
      ELSE
        ! -> Classical formulation
        IF (IENG == 0) THEN 
          DO I = 1,NEL
            EM(I)  = FINTER(IFUNC(1),EMIN(I),NPF,TF,DYDX)
            DAM(I) = EMAJ(I) / EM(I)
          ENDDO
        ! -> Non-linear path formulation
        ELSEIF (IENG == 2) THEN           
          DO I = 1,NEL
            EM(I)  = FINTER(IFUNC(1),BETA(I),NPF,TF,DYDX)
            DAM(I) = PLA(I) / EM(I)
          ENDDO
        ENDIF
      ENDIF 
C------------------------
      RETURN
      END
      
