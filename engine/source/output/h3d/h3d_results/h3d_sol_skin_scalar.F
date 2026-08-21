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
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    initbuf_mod            ../engine/share/resol/initbuf.F
!||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    my_alloc_mod           ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod         ../common_source/tools/memory/my_dealloc.F90
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
      use element_mod , only : nixs
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
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
      INTEGER I,J,LENR,N

      INTEGER JJ
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
      call my_alloc(AFLU, 3, NUMNOD, "AFLU")
      call my_alloc(VFLU, 3, NUMNOD, "VFLU")
      call my_alloc(T6GPS, 6, NUMNOD, "T6GPS")
      call my_alloc(ITAGPS, NUMNOD, "ITAGPS")
      call my_alloc(TAG_SKIN_ND, NUMNOD, "TAG_SKIN_ND")
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
     .                          H3D_PART,IS_WRITTEN_SKIN,
     .                          KEYWORD,NSKIN,MAT_PARAM)
C
       call my_dealloc(AFLU)
       call my_dealloc(VFLU)
       call my_dealloc(T6GPS)
       call my_dealloc(ITAGPS)
       call my_dealloc(TAG_SKIN_ND)
C-----------
      RETURN
      END 
!||====================================================================
!||    idx_fld_sol             ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||--- called by ------------------------------------------------------
!||    h3d_sol_skin_scalar1    ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
!||--- calls      -----------------------------------------------------
!||    vinter_1d               ../engine/source/materials/fail/fld/fail_fld_c.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    my_alloc_mod            ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod          ../common_source/tools/memory/my_dealloc.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE IDX_FLD_SOL(FAIL       ,
     .     NEL      ,EPS3     ,FLD_IDX  ,PLA      )
C-----------------------------------------------
c    FLD failure model
C-----------------------------------------------
C     Modules
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C---------+---------+---+---+--------------------------------------------
C EPS3    | NEL*3   | F | R | IN PLANE STRAIN TENSOR
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real ,DIMENSION(3,NEL), INTENT(IN) :: EPS3
      my_real ,DIMENSION(NEL)  , INTENT(IN) :: PLA
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: FLD_IDX
      type (fail_param_) ,intent(inout) :: fail    !< failure model data structureC-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IENG,LENF,IMARGIN
      INTEGER ,DIMENSION(NEL) :: IPOS
      my_real :: RANI,R1,R2,S1,S2,SS,Q,DYDX,E12,FACT_MARGIN,FACT_LOOSEMETAL
      my_real ,ALLOCATABLE, DIMENSION(:) :: XF,YF  
      my_real ,DIMENSION(NEL) :: EMAJ,EMIN,EM,BETA
C=======================================================================
      ! -> Real parameters
      FACT_MARGIN     = FAIL%UPARAM(1)
      RANI            = FAIL%UPARAM(3)
      FACT_LOOSEMETAL = FAIL%UPARAM(4)
      ! -> Integer parameters
      IMARGIN  = FAIL%IPARAM(2)
      IENG     = FAIL%IPARAM(3)
      IPOS(:)  = 1
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
      LENF = size(fail%table4d(1)%x(1)%values)
      call my_alloc(XF, LENF, "XF")
      call my_alloc(YF, LENF, "YF")
      !=================================================================
      ! FAILURE MAJOR STRAIN FROM INPUT CURVE AND DAMAGE RATIO
      !=================================================================
      ! -> Engineering strains input
      IF (IENG == 1) THEN   ! transform input fld curve to true strain
        DO I = 1,LENF
          XF(I) = LOG(fail%table4d(1)%x(1)%values(i) + ONE)
          YF(I) = fail%table4d(1)%y1d(i)
        ENDDO
        call vinter_1d(nel,nel,lenf,ipos,xf,yf,emin,em)
      ! -> True strains input
      ELSE
        DO I = 1,LENF
          XF(I) = fail%table4d(1)%x(1)%values(i)
          YF(I) = fail%table4d(1)%y1d(i)
        ENDDO
        ! -> Classical formulation
        IF (IENG == 0) THEN 
          call vinter_1d(nel,nel,lenf,ipos,xf,yf,emin,em)
        ! -> Non-linear path formulation
        ELSEIF (IENG == 2) THEN           
          call vinter_1d(nel,nel,lenf,ipos,xf,yf,beta,em)
        ENDIF
      ENDIF
      !
      call my_dealloc(XF)
      call my_dealloc(YF)
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
!||    dam_fld_sol             ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||--- called by ------------------------------------------------------
!||    h3d_sol_skin_scalar1    ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
!||--- calls      -----------------------------------------------------
!||    vinter_1d               ../engine/source/materials/fail/fld/fail_fld_c.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    my_alloc_mod            ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod          ../common_source/tools/memory/my_dealloc.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE DAM_FLD_SOL(FAIL      ,
     .     NEL      ,EPS3     ,DAM     ,PLA      )
C-----------------------------------------------
c    FLD failure model
C-----------------------------------------------
C     Modules
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C---------+---------+---+---+--------------------------------------------
C EPS3    | NEL*3   | F | R | IN PLANE STRAIN TENSOR
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real ,DIMENSION(3,NEL), INTENT(IN) ::  EPS3
      my_real ,DIMENSION(NEL), INTENT(IN) :: PLA
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: DAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IENG,LENF,IMARGIN
      INTEGER ,DIMENSION(NEL) :: IPOS
      my_real :: RANI,S1,S2,SS,Q,DYDX,E12,FACT_MARGIN,FACT_LOOSEMETAL
      my_real ,ALLOCATABLE, DIMENSION(:) :: XF,YF 
      my_real ,DIMENSION(NEL) :: EMAJ,EMIN,EM,BETA
C=======================================================================
   ! -> Real parameters
      FACT_MARGIN     = FAIL%UPARAM(1)
      RANI            = FAIL%UPARAM(3)
      FACT_LOOSEMETAL = FAIL%UPARAM(4)
      ! -> Integer parameters
      IMARGIN  = FAIL%IPARAM(2)
      IENG     = FAIL%IPARAM(3)
      IPOS(:)  = 1
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
c
      LENF = size(fail%table4d(1)%x(1)%values)
      call my_alloc(XF, LENF, "XF")
      call my_alloc(YF, LENF, "YF")
      !=================================================================
      ! FAILURE MAJOR STRAIN FROM INPUT CURVE AND DAMAGE RATIO
      !=================================================================
      ! -> Engineering strains input
      IF (IENG == 1) THEN   ! transform input fld curve to true strain
        DO I = 1,LENF
          XF(I) = LOG(fail%table4d(1)%x(1)%values(i) + ONE)
          YF(I) = fail%table4d(1)%y1d(i)
        END DO
        call vinter_1d(nel,nel,lenf,ipos,xf,yf,emin,em)
        DO I = 1,NEL
          DAM(I) = EMAJ(I) / EM(I)
        END DO
             ! -> True strains input
      ELSE
        DO I = 1,LENF
          XF(I) = fail%table4d(1)%x(1)%values(i)
          YF(I) = fail%table4d(1)%y1d(i)
        END DO
        ! -> Classical formulation
        IF (IENG == 0) THEN 
          call vinter_1d(nel,nel,lenf,ipos,xf,yf,emin,em)
          DO I = 1,NEL
            DAM(I) = EMAJ(I) / EM(I)
          END DO
       ! -> Non-linear path formulation
        ELSEIF (IENG == 2) THEN           
          call vinter_1d(nel,nel,lenf,ipos,xf,yf,beta,em)
          DO I = 1,NEL
            DAM(I) = PLA(I) / EM(I)
          END DO
        ENDIF
      ENDIF
      !
      call my_dealloc(XF)
      call my_dealloc(YF)
C------------------------
      RETURN
      END
      