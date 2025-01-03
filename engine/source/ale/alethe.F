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
      !||    alethe                           ../engine/source/ale/alethe.F
      !||--- called by ------------------------------------------------------
      !||    alemain                          ../engine/source/ale/alemain.F
      !||--- calls      -----------------------------------------------------
      !||    aconve                           ../engine/source/ale/aconve.F
      !||    aeturb                           ../engine/source/ale/turbulence/aeturb.F
      !||    aflux0                           ../engine/source/ale/aflux0.F
      !||    afluxt                           ../engine/source/ale/ale51/afluxt.F
      !||    agrad0                           ../engine/source/ale/agrad0.F
      !||    akturb                           ../engine/source/ale/turbulence/akturb.F
      !||    ale51_finish                     ../engine/source/ale/ale51/ale51_finish.F
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    ale51_init                       ../engine/source/ale/ale51/ale51_init.F
      !||    arezon                           ../engine/source/ale/arezon.F
      !||    atherm                           ../engine/source/ale/atherm.F
      !||    bhol2                            ../engine/source/ale/ale2d/bhol2.F
      !||    my_barrier                       ../engine/source/system/machine.F
      !||    nvar                             ../engine/source/input/nvar.F
      !||    spmd_xvois                       ../engine/source/mpi/fluid/spmd_cfd.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod             ../common_source/modules/ale/ale_connectivity_mod.F
      !||    ale_mod                          ../common_source/modules/ale/ale_mod.F
      !||    alemuscl_mod                     ../common_source/modules/ale/alemuscl_mod.F
      !||    elbufdef_mod                     ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    glob_therm_mod                   ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||    i22tri_mod                       ../common_source/modules/interfaces/cut-cell-search_mod.F
      !||    segvar_mod                       ../engine/share/modules/segvar_mod.F
      !||====================================================================
      SUBROUTINE ALETHE(
     1   IPARG,      ELBUF_TAB,  FLUX,       VAL2,
     2   PHI,        ALE_CONNECT,IXS,        IXQ,
     3   V,          W,          X,          PM,
     4   MS,         VEUL,       FILL,       DFILL,
     5   ALPH,       FV,         BUFMAT,     TF,
     6   NPF,        ITASK,      NBRCVOIS,   NBSDVOIS,
     7   LNRCVOIS,   LNSDVOIS,   NERCVOIS,   NESDVOIS,
     8   LERCVOIS,   LESDVOIS,   IADS,       SEGVAR,
     9   MSNF,       NODFT,      NODLT,      BHOLE,
     A   IPM,        QMV,        ITAB,       ITABM1,
     B   LENQMV,     NV46,
     C   VOLN,       IAD_ELEM    ,GLOB_THERM   ,
     D   FR_ELEM,    NEL,        IXTG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD   
      USE I22TRI_MOD 
      USE SEGVAR_MOD        
      USE ALE_CONNECTIVITY_MOD
      USE ALEMUSCL_MOD , only:ALEMUSCL_Param
      USE ALE_MOD
      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB

      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(NIXQ,NUMELQ),NPF(*),
     .        IADS(8,*), ITAB(NUMNOD), ITABM1(*), BHOLE(*),
     .        NBRCVOIS(*),NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),IPM(NPROPMI,NUMMAT),
     .        NODFT, NODLT, LENQMV, ITASK, NV46,
     .        IAD_ELEM(2, *), FR_ELEM(*),IXTG(NIXTG,NUMELTG)
 
      my_real VEUL, FLUX(*), VAL2(*), PHI(*), V(*), W(*), X(*), PM(*),
     .        MS(*), FILL(*), DFILL(*), ALPH(*), FV(*), BUFMAT(*), TF(*),
     .        MSNF(*),QMV(*),VOLN(MVSIZ)

      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITRIMAT,NVAR,I, LENCOMN,LENCOM
      my_real,DIMENSION(:,:),ALLOCATABLE:: FLUX_SAV
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALLOCATE(FLUX_SAV(NV46,LENQMV))
      LENCOMN=NBRCVOIS(NSPMD+1)+NBSDVOIS(NSPMD+1)
      LENCOM =NERCVOIS(NSPMD+1)+NESDVOIS(NSPMD+1)

C=======================================================================
C     DIFFUSIVE TERMS
C=======================================================================
      IF (GLOB_THERM%ITHERM+ITURB /= 0) THEN
        !-------------------------------------!
        !  SPMD EXCHANGES                     !
        !-------------------------------------!
        IF (NSPMD  > 1) THEN
!$OMP SINGLE
          CALL SPMD_XVOIS(X,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,LENCOMN)
!$OMP   END SINGLE
        END IF
       !-----------------------------!
       !   DIFFUSIVE FLUXES          !
       !-----------------------------!
        CALL AGRAD0(IPARG,IXS,IXQ,X,ALE_CONNECT,
     +              FLUX,VEUL,ELBUF_TAB,ITASK)
C----------------------
        CALL MY_BARRIER
C----------------------
        IF (GLOB_THERM%ITHERM /= 0) THEN
          IF(ITASK == 0) THEN
            !exchange : SPMD PHI+VAL2 in ATHERM
            CALL ATHERM(
     1   IPARG,      PM,         ELBUF_TAB,  FLUX,
     2   VAL2,       PHI,        ALE_CONNECT,IXS,
     3   IXQ,        FV,         X,          BUFMAT,
     4   TF,         NPF,        NERCVOIS,   NESDVOIS,
     5   LERCVOIS,   LESDVOIS,   LENCOM,     IPM,
     6   NEL)
          ENDIF
C------------------------
          CALL MY_BARRIER
C------------------------
        ENDIF
       !-----------------------------!
       !   TURBULENCY                !
       !-----------------------------!
        IF (ITURB /= 0) THEN
C         SPMD exchange : PHI+VAL2
          CALL AKTURB(IPARG       ,ELBUF_TAB ,FLUX     ,VAL2     ,PHI         ,
     2                ALE_CONNECT ,IXS       ,IXQ      ,PM       ,ITASK       ,
     3                NERCVOIS    ,NESDVOIS  ,LERCVOIS ,LESDVOIS ,LENCOM      )
          CALL AETURB(IPARG       ,ELBUF_TAB ,FLUX     ,VAL2     ,PHI         ,
     2                ALE_CONNECT ,IXS       ,IXQ      ,PM       ,ITASK       ,
     3                NERCVOIS    ,NESDVOIS  ,LERCVOIS ,LESDVOIS ,LENCOM      )
          ENDIF
      ENDIF !(ITHERM+ITURB /= 0)

C=======================================================================
C     CONVECTIVE TERM (REYNOLDS TRANSPORTATION THEOREM)
C=======================================================================
      IF(IALE+IEULER /= 0)THEN
      !-----------------------------!
      !  CONVECTIVE FLUXES          !
      !   ZEROING MASSES            !
      !-----------------------------!
      !zeroing masses for convection
      ! warning : mass treatment in parith/on is different
      !EL: SPMD : MS=0 on boundary domains if /= 1  (in resol) 
        IF(N2D == 0)THEN
          DO I=NODFT,NODLT       
            MS(I)=MS(I)-MSNF(I)    !MS was total mass, it becomes again structural mass. it becomes total mass in RESOL>ASSPAR4 (before calling ACCELE)
            MSNF(I)=ZERO           !MSNF are mass of fluid nodes, 
          ENDDO                  
        ENDIF
        CALL AFLUX0(IPARG      ,ELBUF_TAB,PM      ,IXS       ,IXQ    ,
     .              V          ,W        ,X        ,FLUX     ,VAL2   ,
     .              MS         ,VEUL     ,FILL     ,DFILL    ,PHI    ,
     .              ALE_CONNECT,ALPH     ,ITASK    ,NV46     ,IPM    )
C----------------------
        CALL MY_BARRIER
C----------------------
        !-----------------------------!
        !     BIMAT (LAW20)           !
        !-----------------------------!
        IF(N2D /= 0.AND.NMULT  > 0)THEN
         CALL BHOL2(ITASK,IPARG , PM ,BHOLE ,IXQ  )
C-----------------------
         CALL MY_BARRIER
C-----------------------
        ENDIF
        !-----------------------------!
        !     MULTI-MAT (LAW51)       !
        !-----------------------------!
        ITRIMAT  = 0
        IF (TRIMAT  > 0) THEN
          CALL ALE51_INIT(IPARG  ,PM      ,IXS  ,IXQ         ,
     2                    V      ,W       ,X    ,FLUX        ,VAL2 ,
     3                    MS     ,VEUL    ,PHI  ,ALE_CONNECT ,ITASK,
     4                    ITRIMAT,FLUX_SAV,QMV  ,NV46        ,ELBUF_TAB)
        ENDIF
        !-----------------------------!
        !     REZONE DEVIATEUR        !
        !-----------------------------!
        CALL AREZON(IPARG   ,ELBUF_TAB, FLUX   ,PHI   ,ALE_CONNECT ,  
     2              6       ,2        ,1       ,ITASK ,NERCVOIS    ,  
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE       ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              6       ,2        ,2       ,ITASK  ,NERCVOIS   ,  
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              6       ,2        ,3       ,ITASK  ,NERCVOIS   ,  
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              6       ,2        ,4       ,ITASK  ,NERCVOIS   ,  
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              6       ,2        ,5       ,ITASK  ,NERCVOIS   ,  
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              6       ,2        ,6       ,ITASK  ,NERCVOIS   ,  
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT ,IXS)                                    
        !------------------------------!
        !     REZONING : EPS_PLAS      !
        !------------------------------!
          CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                1       ,10       ,1       ,ITASK ,NERCVOIS   ,
     3                NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                ITRIMAT ,IXS)
c---------specific to law81
          CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                1       ,11       ,0       ,ITASK ,NERCVOIS   ,
     3                NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                ITRIMAT ,IXS)
C-----------------------------
C       MASS & ENERGY CONVECTION
C-----------------------------
         DO NVAR=ALE%GLOBAL%LCONV,1,-1
          !1     : Mass
          !2     : Energy
          !3,4   : Turbulency
          !5     : law37 massic fraction
          !6,7,8 : MomX, MomY, MomZ (ALE with Full FVM)
           IF((NVAR /= 9.AND.ALE%GLOBAL%CODV(NVAR) /= 0))THEN
              CALL ACONVE(
     1                    IPARG       ,ELBUF_TAB ,FLUX    , VAL2     ,PHI     ,
     2                    ALE_CONNECT ,NVAR      ,ITASK   , NERCVOIS ,NESDVOIS,
     3                    LERCVOIS    ,LESDVOIS  ,LENCOM  , SEGVAR   ,BHOLE   ,
     4                    ITRIMAT     ,QMV       ,0       , IXS      ,IXQ     ,
     5                    PM          ,IPM       ,BUFMAT  , X)
            ENDIF

         ENDDO  
C----------------------       
        !-----------------------------!
        !     LAW51 :                 !
        !       SUBMATERIAL TREATMENT !
        !-----------------------------!
         IF (TRIMAT   >  0 .AND. ALEMUSCL_Param%IALEMUSCL   >  0) THEN
            IF (N2D  ==  0) THEN
               CALL ALE51_GRADIENT_RECONSTRUCTION(IPARG, ELBUF_TAB, IXS, X, ALE_CONNECT,
     .              NV46,NERCVOIS,NESDVOIS,  LERCVOIS,LESDVOIS,LENCOM, ITASK, 
     .              IAD_ELEM, FR_ELEM, SEGVAR)
            ELSE
               CALL ALE51_GRADIENT_RECONSTRUCTION2(IPARG, ELBUF_TAB, IXQ, X, ALE_CONNECT,
     .              NV46,NERCVOIS,NESDVOIS,  LERCVOIS,LESDVOIS,LENCOM, ITASK, 
     .              IAD_ELEM, FR_ELEM, SEGVAR)
            ENDIF
         ENDIF

         DO ITRIMAT = 1,TRIMAT
            CALL AFLUXT(IPARG,ELBUF_TAB,PM,IXS,IXQ,
     2                  V       ,W       ,X       ,FLUX       ,VAL2    ,
     3                  MS      ,VEUL    ,PHI     ,ALE_CONNECT,ITASK   ,
     4                  ITRIMAT ,FLUX_SAV,NERCVOIS,NESDVOIS   ,
     5                  LERCVOIS,LESDVOIS,LENCOM  ,QMV        ,ITAB    ,
     6                  ITABM1  ,NV46    ,SEGVAR)
        !-----------------------------!
        !     LAW51 REZONING :        !
        !        DEVIATORIC STRESS    ! 
        !-----------------------------!
            IF(ITRIMAT /= 4 ) THEN
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                     1       ,2        ,1       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                     1       ,2        ,2       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                     1       ,2        ,3       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                     1       ,2        ,4       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                     1       ,2        ,5       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                     1       ,2        ,6       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,IXS)
            ENDIF
        !-----------------------------!
        !     LAW51 REZONING :        !
        !      EPS_PLAS               !
        !      or/and DETONATION TIMES!        
        !-----------------------------!
            CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT   ,
     2                  1       ,10      ,1        ,ITASK ,NERCVOIS,
     3                  NESDVOIS,LERCVOIS,LESDVOIS,LENCOM ,BHOLE   ,
     4                  ITRIMAT ,IXS)
        !-----------------------------!
        !     LAW51 :                 !
        !        EINT CONVECTION      !
        !-----------------------------!
            CALL ACONVE(IPARG       ,ELBUF_TAB,FLUX  ,VAL2    ,PHI     ,
     2                  ALE_CONNECT ,2        ,ITASK ,NERCVOIS,NESDVOIS,
     3                  LERCVOIS    ,LESDVOIS ,LENCOM,SEGVAR  ,BHOLE   ,
     4                  ITRIMAT     ,QMV      ,0     ,IXS     ,IXQ     ,
     5                  PM          ,IPM      ,BUFMAT,X )
        !-----------------------------!
        !     LAW51 :                 !
        !       MASS CONVECTION       !
        !-----------------------------!
            CALL ACONVE(IPARG       ,ELBUF_TAB,FLUX  ,VAL2    ,PHI     ,
     2                  ALE_CONNECT ,1        ,ITASK ,NERCVOIS,NESDVOIS,
     3                  LERCVOIS    ,LESDVOIS ,LENCOM,SEGVAR  ,BHOLE   ,
     4                  ITRIMAT     ,QMV      ,1     ,IXS     ,IXQ     ,
     5                  PM          ,IPM      ,BUFMAT,X )
         ENDDO!next ITRIMAT
        !-----------------------------!
         IF (TRIMAT > 0) THEN
           CALL ALE51_FINISH(IPARG,ELBUF_TAB   ,PM   ,IXS     ,IXQ,
     .                       V    ,W           ,X    ,FLUX    ,VAL2 ,MS  ,VEUL,
     .                       PHI  ,ALE_CONNECT ,ITASK,FLUX_SAV,QMV  ,NV46,ELBUF_TAB      )
         ENDIF
C----------------------
      ENDIF

      IF(ITASK==0) DEALLOCATE(FLUX_SAV)
      RETURN
      END
C
