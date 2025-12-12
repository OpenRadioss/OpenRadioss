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
!||    spmd_xvois                       ../engine/source/mpi/fluid/spmd_cfd.F
!||    startime                         ../engine/source/system/timer_mod.F90
!||    stoptime                         ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod             ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                          ../common_source/modules/ale/ale_mod.F
!||    alemuscl_mod                     ../common_source/modules/ale/alemuscl_mod.F
!||    arezon_mod                       ../engine/source/ale/arezon.F
!||    elbufdef_mod                     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                      ../common_source/modules/elements/element_mod.F90
!||    glob_therm_mod                   ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    i22tri_mod                       ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    matparam_def_mod                 ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    segvar_mod                       ../engine/share/modules/segvar_mod.F
!||    timer_mod                        ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE ALETHE(
     1   IPARG,      ELBUF_TAB,  FLUX,       VAL2,
     2   PHI,        ALE_CONNECT,IXS,        IXQ,
     3   V,          W,          X,          PM,
     4   MS,         VEUL,       FILL,       DFILL,
     5   ALPH,       FV,         BUFMAT,     TF,
     6   NPF,        ITASK,      NBRCVOIS,   NBSDVOIS,
     7   LNRCVOIS,   LNSDVOIS,   NERCVOIS,   NESDVOIS,
     8   LERCVOIS,   LESDVOIS,   SEGVAR,
     9   MSNF,       NODFT,      NODLT,      BHOLE,
     A   IPM,        QMV,
     B   s_flux,     s_qmv,      NV46,
     C   IAD_ELEM,   GLOB_THERM,
     D   FR_ELEM,    MATPARAM  ,timers,nthread,int22)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD   
      USE I22TRI_MOD 
      USE SEGVAR_MOD        
      USE ALE_CONNECTIVITY_MOD
      USE ALEMUSCL_MOD , only:ALEMUSCL_Param
      USE ALE_MOD , ONLY : ALE
      USE AREZON_MOD , ONLY : AREZON
      use glob_therm_mod
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
      use element_mod , only : nixs,nixq
      use timer_mod
      use precision_mod , only : WP
      use aflut_int22_mod , only : aflut_int22
      use ale51_finish_int22_mod , only : ale51_finish_int22
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
#include "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of flux array
      integer, intent(in) :: s_qmv  !< first dimension of QMV array
      integer, intent(in) :: nthread !< number of omp threads
      integer, intent(in) :: int22 !< interface 22 flag 
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB

      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(NIXQ,NUMELQ),NPF(*),
     .        BHOLE(*),
     .        NBRCVOIS(*),NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),IPM(NPROPMI,NUMMAT),
     .        NODFT, NODLT,ITASK, NV46,
     .        IAD_ELEM(2, *), FR_ELEM(*)
 
      my_real VEUL, FLUX(*), VAL2(*), PHI(*), V(*), W(*), X(*), PM(*),
     .        MS(*), FILL(*), DFILL(*), ALPH(*), FV(*), BUFMAT(*), TF(*),
     .        MSNF(*),QMV(s_qmv,2*nv46)

      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      type (glob_therm_) ,intent(inout)   :: glob_therm
      TYPE(MATPARAM_STRUCT_),DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM !< material buffer
      type(timer_), intent(inout) :: timers
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITRIMAT,NVAR,I, LENCOMN,LENCOM
      integer :: n_entity,n_neighbor,s_alpha_mat
      real(kind=WP), dimension(:,:),allocatable ,save:: flux_sav
      real(kind=WP), dimension(:,:),allocatable ,save:: alpha_mat,val2_mat            
      real(kind=WP), dimension(:,:,:), allocatable, save :: flux_mat,qmv_mat
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      if(itask==0) then
        ALLOCATE(FLUX_SAV(s_flux,NV46))
      endif    
      if(n2d==0) then
        n_entity = numels
        n_neighbor = nsvois
      else
        n_entity = numelq
        n_neighbor = nqvois
      end if
      s_alpha_mat = n_entity + n_neighbor   

      call my_barrier
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
          if(itask==0) call startime(timers,timer_spmdcfd)
!$OMP SINGLE
          CALL SPMD_XVOIS(X,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,LENCOMN)
!$OMP   END SINGLE
          if(itask==0) call stoptime(timers,timer_spmdcfd)
        END IF
       !-----------------------------!
       !   DIFFUSIVE FLUXES          !
       !-----------------------------!
        CALL AGRAD0(IPARG,IXS,IXQ,X,ALE_CONNECT,
     +              FLUX,VEUL,ELBUF_TAB,ITASK,s_flux)
C----------------------
        CALL MY_BARRIER
C----------------------
        IF (GLOB_THERM%ITHERM /= 0) THEN
          IF(ITASK == 0) THEN
            !exchange : SPMD PHI+VAL2 in ATHERM

            CALL ATHERM(
     1                  IPARG,      PM,         ELBUF_TAB,  FLUX,
     2                  VAL2,       PHI,        ALE_CONNECT,IXS,
     3                  IXQ,        FV,         X,          BUFMAT,
     4                  TF,         NPF,        NERCVOIS,   NESDVOIS,
     5                  LERCVOIS,   LESDVOIS,   LENCOM,     IPM,
     6                  MATPARAM,   s_flux,     nv46)
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
     3                NERCVOIS    ,NESDVOIS  ,LERCVOIS ,LESDVOIS ,LENCOM      ,
     4                MATPARAM,s_flux,nv46)
          CALL AETURB(IPARG       ,ELBUF_TAB ,FLUX     ,VAL2     ,PHI         ,
     2                ALE_CONNECT ,IXS       ,IXQ      ,PM       ,ITASK       ,
     3                NERCVOIS    ,NESDVOIS  ,LERCVOIS ,LESDVOIS ,LENCOM      ,
     4                MATPARAM,s_flux,nv46)
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
        CALL AFLUX0(IPARG      ,ELBUF_TAB,PM       ,IXS       ,IXQ    ,
     .              V          ,W        ,X        ,FLUX      ,VAL2   ,
     .              VEUL       ,FILL     ,DFILL    ,PHI       ,
     .              ALE_CONNECT,ALPH     ,ITASK    ,NV46      ,IPM    ,
     .              s_flux     )
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
          CALL ALE51_INIT(IPARG       ,PM    ,IXS  ,IXQ      ,
     2                    X           ,FLUX  ,VAL2 ,
     3                    ALE_CONNECT ,ITASK ,
     4                    FLUX_SAV    ,QMV   ,NV46 ,ELBUF_TAB,
     5                    s_flux,s_qmv)
        ENDIF
        !-----------------------------!
        !     REZONE  DEVIATORIC      !
        !-----------------------------!
        CALL AREZON(IPARG   ,ELBUF_TAB, FLUX   ,PHI     ,ALE_CONNECT ,
     2              2       ,1         ,ITASK  ,NERCVOIS,
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM  ,BHOLE       ,
     4              ITRIMAT,timers,s_flux  ,nv46)
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              2        ,2       ,ITASK  ,NERCVOIS   ,
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT,timers,s_flux  ,nv46)
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              2        ,3       ,ITASK  ,NERCVOIS   ,
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT,timers,s_flux  ,nv46)
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              2        ,4       ,ITASK  ,NERCVOIS   ,
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT,timers,s_flux  ,nv46)
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              2        ,5       ,ITASK  ,NERCVOIS   ,
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT,timers,s_flux  ,nv46)
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI    ,ALE_CONNECT,  
     2              2        ,6       ,ITASK  ,NERCVOIS   ,
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM ,BHOLE      ,  
     4              ITRIMAT,timers,s_flux  ,nv46)
        !------------------------------!
        !     REZONING : EPS_PLAS      !
        !------------------------------!
        CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2              10       ,1       ,ITASK ,NERCVOIS   ,
     3              NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4              ITRIMAT,timers,s_flux  ,nv46)
        !-------------------------------!
        !     REZONING : %VAR (MAT/EOS) !
        !-------------------------------!
        DO I = 1,ALE%REZON%NUM_NUVAR_MAT
          CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI      ,ALE_CONNECT,
     2                11      ,I        ,ITASK   ,NERCVOIS ,
     3                NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM   ,BHOLE      ,
     4                ITRIMAT,timers,s_flux  ,nv46,
     5                1  ) ! OPT_FALG_MAT_EOS = 1 : rezoning for material laws
        ENDDO
        DO I = 1,ALE%REZON%NUM_NUVAR_EOS
          CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI      ,ALE_CONNECT,
     2                11      ,I        ,ITASK   ,NERCVOIS ,
     3                NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM   ,BHOLE      ,
     4                ITRIMAT,timers,s_flux  ,nv46,
     5                2  ) ! OPT_FALG_MAT_EOS = 2 : rezoning for EoS
        ENDDO

        !------------------------------!
        !     REZONING : TEMPERATURE   !
        !------------------------------!
          CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT,
     2                12       ,1       ,ITASK ,NERCVOIS   ,
     3                NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                ITRIMAT,timers,s_flux  ,nv46)
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
     5                    PM          ,X         ,timers,s_flux   ,s_qmv   ,
     6                    nv46)
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
     .              NERCVOIS,NESDVOIS,  LERCVOIS,LESDVOIS,LENCOM, ITASK,
     .              IAD_ELEM, FR_ELEM, SEGVAR,timers)
            ELSE
               CALL ALE51_GRADIENT_RECONSTRUCTION2(IPARG, ELBUF_TAB, IXQ, X, ALE_CONNECT,
     .              NV46,NERCVOIS,NESDVOIS,  LERCVOIS,LESDVOIS,LENCOM, ITASK, 
     .              IAD_ELEM, FR_ELEM, SEGVAR,timers)
            ENDIF
         ENDIF

         if(trimat>0) then
          if(itask==0) then
            allocate(flux_mat(s_flux,NV46,trimat))
            allocate(qmv_mat(s_qmv,2*nv46,trimat))
            allocate(alpha_mat(s_alpha_mat,trimat))
            allocate(val2_mat(s_flux,trimat))
          endif         

          call my_barrier

          call AFLUXT(IPARG   ,ELBUF_TAB  ,PM      ,IXS      ,IXQ,
     2                X       ,flux_mat   ,val2_mat    ,s_alpha_mat,
     3                alpha_mat,ALE_CONNECT,ITASK   ,
     4                TRIMAT ,FLUX_SAV   ,NERCVOIS,NESDVOIS ,
     5                LERCVOIS,LESDVOIS   ,qmv_mat  ,
     6                NV46       ,SEGVAR  ,timers,s_flux,s_qmv,
     7                ngroup,n2d,nspmd)
          call my_barrier     
         endif

         DO ITRIMAT = 1,TRIMAT
            if(int22/=0) then
              call aflut_int22(itask,nthread,numels,nparg,ngroup,s_flux,s_alpha_mat,nv46, 
     .                         itrimat,trimat,npropm,nummat,iparg,ixs,pm,alpha_mat,flux_mat,elbuf_tab)
            endif
        !-----------------------------!
        !     LAW51 REZONING :        !
        !        DEVIATORIC STRESS    ! 
        !-----------------------------!
            IF(ITRIMAT /= 4 ) THEN
               CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT,
     2                     2        ,1       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,timers,s_flux  ,nv46)
               CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT,
     2                     2        ,2       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,timers,s_flux  ,nv46)
               CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT,
     2                     2        ,3       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,timers,s_flux  ,nv46)
               CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT,
     2                     2        ,4       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,timers,s_flux  ,nv46)
               CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT,
     2                     2        ,5       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,timers,s_flux  ,nv46)
               CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT,
     2                     2        ,6       ,ITASK ,NERCVOIS   ,
     3                     NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM,BHOLE      ,
     4                     ITRIMAT,timers,s_flux  ,nv46)
            ENDIF
        !-----------------------------!
        !     LAW51 REZONING :        !
        !      EPS_PLAS               !
        !      or/and DETONATION TIMES!        
        !-----------------------------!
            CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT   ,
     2                  10      ,1        ,ITASK ,NERCVOIS,
     3                  NESDVOIS,LERCVOIS,LESDVOIS,LENCOM ,BHOLE   ,
     4                  ITRIMAT,timers,s_flux  ,nv46)
        !-----------------------------!
        !     LAW51 REZONING :        !
        !      TEMPERATURE            !
        !-----------------------------!
            CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI   ,ALE_CONNECT,
     2                  12      ,1        ,ITASK ,NERCVOIS,
     3                  NESDVOIS,LERCVOIS,LESDVOIS,LENCOM ,BHOLE   ,
     4                  ITRIMAT,timers,s_flux  ,nv46)
        !-----------------------------!
        !     LAW51 REZONING :        !
        !      VAREOS                 !
        !-----------------------------!
            DO I = 1,ALE%REZON%NUM_NUVAR_EOS
              CALL AREZON(IPARG   ,ELBUF_TAB,flux_mat(:,:,itrimat)    ,PHI      ,ALE_CONNECT,
     2                    11      ,I        ,ITASK   ,NERCVOIS ,
     3                    NESDVOIS,LERCVOIS ,LESDVOIS,LENCOM   ,BHOLE      ,
     4                    ITRIMAT,timers,s_flux  ,nv46, 
     5                    2   ) ! OPT_FALG_MAT_EOS = 2 : rezoning for EoS
            ENDDO
        !-----------------------------!
        !     LAW51 :                 !
        !        EINT CONVECTION      !
        !-----------------------------!
            CALL ACONVE(IPARG       ,ELBUF_TAB,flux_mat(:,:,itrimat)  ,val2_mat(:,itrimat),PHI     ,
     2                  ALE_CONNECT ,2        ,ITASK ,NERCVOIS,NESDVOIS,
     3                  LERCVOIS    ,LESDVOIS ,LENCOM,SEGVAR  ,BHOLE   ,
     4                  ITRIMAT     ,qmv_mat(:,:,itrimat)      ,0     ,IXS     ,IXQ     ,
     5                  PM          ,X        ,timers,s_flux  ,s_qmv   ,
     6                  nv46)
        !-----------------------------!
        !     LAW51 :                 !
        !       MASS CONVECTION       !
        !-----------------------------!
            CALL ACONVE(IPARG       ,ELBUF_TAB,flux_mat(:,:,itrimat)  ,val2_mat(:,itrimat)    ,PHI     ,
     2                  ALE_CONNECT ,1        ,ITASK ,NERCVOIS,NESDVOIS,
     3                  LERCVOIS    ,LESDVOIS ,LENCOM,SEGVAR  ,BHOLE   ,
     4                  ITRIMAT     ,qmv_mat(:,:,itrimat)      ,1     ,IXS     ,IXQ     ,
     5                  PM          ,X        ,timers,s_flux  ,s_qmv   ,
     6                  nv46)
         ENDDO!next ITRIMAT
        !-----------------------------!
         if (trimat>0.and.int22>0) THEN
          call ale51_finish_int22(itask,nthread,numels,nparg,ngroup, 
     .                            npropm,nummat,iparg,ixs,pm,elbuf_tab)

!           CALL ALE51_FINISH(IPARG,PM   ,IXS  ,IXQ,
!     .                       X    ,FLUX ,val2_mat(:,itrimat) ,PHI  ,ALE_CONNECT ,ITASK,FLUX_SAV,QMV  ,NV46,ELBUF_TAB,
!     .                       s_flux,s_qmv)
         endif
C----------------------
      ENDIF

      call my_barrier
      if(itask==0) then      
        if(trimat>0) then
            deallocate(flux_mat)
            deallocate(qmv_mat)
            deallocate(alpha_mat)
            deallocate(val2_mat)
        endif
        deallocate(flux_sav)
      endif

      RETURN
      END
C
