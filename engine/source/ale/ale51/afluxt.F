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
!||    afluxt                  ../engine/source/ale/ale51/afluxt.F
!||--- called by ------------------------------------------------------
!||    alethe                  ../engine/source/ale/alethe.F
!||--- calls      -----------------------------------------------------
!||    ale51_antidiff2         ../engine/source/ale/ale51/ale51_antidiff2.F
!||    ale51_antidiff3         ../engine/source/ale/ale51/ale51_antidiff3.F
!||    ale51_antidiff3_int22   ../engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
!||    ale51_spmd2             ../engine/source/ale/ale51/ale51_spmd2.F
!||    ale51_spmd3             ../engine/source/ale/ale51/ale51_spmd3.F
!||    ale51_upwind2           ../engine/source/ale/ale51/ale51_upwind2.F
!||    ale51_upwind3           ../engine/source/ale/ale51/ale51_upwind3.F
!||    ale51_upwind3_int22     ../engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
!||    alemuscl_upwind         ../engine/source/ale/alemuscl/alemuscl_upwind.F
!||    alemuscl_upwind2        ../engine/source/ale/alemuscl/alemuscl_upwind2.F
!||    initbuf                 ../engine/share/resol/initbuf.F
!||    my_barrier              ../engine/source/system/machine.F
!||    spmd_e1vois             ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_e4vois             ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_e6vois             ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_i4vois             ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_i8vois             ../engine/source/mpi/fluid/spmd_cfd.F
!||    startime                ../engine/source/system/timer_mod.F90
!||    stoptime                ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod    ../common_source/modules/ale/ale_connectivity_mod.F
!||    alemuscl_mod            ../common_source/modules/ale/alemuscl_mod.F
!||    elbufdef_mod            ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod             ../common_source/modules/elements/element_mod.F90
!||    i22bufbric_mod          ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22tri_mod              ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    initbuf_mod             ../engine/share/resol/initbuf.F
!||    multimat_param_mod      ../common_source/modules/multimat_param_mod.F90
!||    segvar_mod              ../engine/share/modules/segvar_mod.F
!||    timer_mod               ../engine/source/system/timer_mod.F90
!||    trimat_mod              ../engine/share/modules/trimat.F
!||====================================================================
      SUBROUTINE AFLUXT(IPARG   ,ELBUF_TAB   ,PM       ,IXS     ,IXQ     ,
     2                  X       ,flux_mat    ,val2_mat     ,s_alpha_mat,
     3                  alpha_mat,ALE_CONNECT ,ITASK    ,
     4                  trimat  ,FLUX_SAV    ,NERCVOIS ,NESDVOIS,
     5                  LERCVOIS,LESDVOIS    ,qmv_mat ,
     6                  NV46        ,SEGVAR   ,timers  ,s_flux,s_qmv,
     7                  ngroup,n2d,nspmd)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE TRIMAT_MOD
      USE ELBUFDEF_MOD            
      USE SEGVAR_MOD   
      USE ALE_CONNECTIVITY_MOD
      USE ALEMUSCL_MOD , only:ALEMUSCL_Param
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
      use element_mod , only : nixs
      use timer_mod
      use ale51_apply_remote_flux_mod , only : ale51_apply_remote_flux
      use precision_mod , only : WP
      use array_mod
      use spmd_exch_n_neighbor_mod , only : spmd_exch_n_neighbor
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "spmd_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: ngroup !< number of element group
      integer, intent(in) :: n2d !< 2d or 3d case
      integer, intent(in) :: nspmd !< number of mpi processors
      integer, intent(in) :: s_flux !< first dimension of flux array
      integer, intent(in) :: s_qmv !< first dimension of flux array
      integer, intent(in) :: s_alpha_mat !< first dimension of alpha array
      integer, intent(in) :: nv46 !< second dimension of flux & qmv array
      integer, intent(in) :: trimat !< number of materials
      real(kind=WP), dimension(s_alpha_mat,trimat), intent(inout) :: alpha_mat !< alpha array      
      real(kind=WP), dimension(s_flux,nv46,trimat), intent(inout) :: flux_mat !< flux array  
      real(kind=WP), dimension(s_qmv,2*nv46,trimat), intent(inout) :: qmv_mat !< qmv array
      real(kind=WP), dimension(s_flux,trimat), intent(inout) :: val2_mat !< global flux???
      my_real PM(NPROPM,NUMMAT), X(3,NUMNOD),FLUX_SAV(s_flux,nv46)

      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(7,NUMELQ)
      INTEGER ITASK,ADD,NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*)

      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_segvar), INTENT(IN) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      type(timer_), intent(inout) :: timers
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer :: itrimat,ijk
      my_real, DIMENSION(:), POINTER :: UVAR,VOLG,VOLP,pDDVOL
      INTEGER NG, I, K, II, NF1, NFIRST, NLAST, LLT_
      INTEGER :: IB, IE, NIN, NBF, NBL, K0, K1, IBM, J, IE_M, IDLOC, IPOS, ICELL, NCELL, MCELL, tNB, J1, J2, IBV
      integer :: s_flux_vois,n_entity0,n_entity,n_neighbor,n_node_per_elm
      my_real :: VFRAC
      TYPE(BUF_MAT_)  ,POINTER :: MBUF
      real(kind=WP), dimension(:,:,:), allocatable, save :: flux_vois_mat
      type(array_type_my_real_2d), dimension(:), allocatable :: s_alpha,r_alpha
      type(array_type_my_real_3d), dimension(:), allocatable :: s_flux_neigh,r_flux_neigh
      integer :: flag
      integer :: s_proc_nb,r_proc_nb
      integer :: s_lesdvois,s_lercvois
      integer, dimension(nspmd) :: s_index,r_index
      integer, dimension(nspmd) :: s_req,r_req
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      if(n2d==0) then
        n_entity0 = numels
        n_neighbor = nsvois
        n_node_per_elm = 8
      else
        n_entity0 = numelq
        n_neighbor = nqvois
        n_node_per_elm = 4
      end if
      s_flux_vois = n_entity0 + n_neighbor
      if(itask==0) then
        allocate(flux_vois_mat(s_flux_vois,nv46,trimat))
      end if
C--------------------
      CALL MY_BARRIER
C--------------------

C-----------------------------------------------
C     VOLUME FLUXES BACKUP
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF(IPARG    ,NG      ,                  
     2                MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3                NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4                JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5                NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6                IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7                ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE
        VOLG => ELBUF_TAB(NG)%GBUF%VOL
        UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
        LFT=1
        do itrimat=1,trimat
          ADD = M51_N0PHAS + (ITRIMAT-1)*M51_NVPHAS                ! ADD => SIG(1)
          ADD = ADD + 11                                   ! ADD + 11 => VOLUME_Phase
          K = LLT*(ADD-1)                                ! VAR(I,ADD) = VAR(K+I) 
          VOLP => UVAR(K+1:K+LLT)
          DO I=LFT,LLT 
            II  = I+NFT    
            alpha_mat(II,itrimat) = VOLP(I)/VOLG(I)
            alpha_mat(II,itrimat) = MAX(ZERO,MIN(ONE,alpha_mat(II,itrimat))) 
          ENDDO
        enddo
        do itrimat=1,trimat
          DO K=1,NV46
            DO II=NFT+LFT,NFT+LLT
              flux_mat(II,k,itrimat)=FLUX_SAV(II,K)
            ENDDO
          ENDDO
        enddo
        do itrimat=1,trimat
          DO K=1,NV46
            DO II=NFT+LFT,NFT+LLT
              qmv_mat(II,K,itrimat) = ZERO
            ENDDO
          ENDDO
        enddo
      ENDDO
C--------------------
      CALL MY_BARRIER
C--------------------

      s_lesdvois = nesdvois(nspmd+1)
      s_lercvois = nercvois(nspmd+1)
      n_entity = s_alpha_mat
      IF(NSPMD > 1)THEN     
        if(itask==0) then
          call startime(timers,timer_spmdcfd)  
          flag = 0
          s_proc_nb = 0
          r_proc_nb = 0      

          allocate(s_alpha(nspmd))
          allocate(r_alpha(nspmd))

          call spmd_exch_n_neighbor(flag,nspmd,n_entity,trimat,s_lesdvois,s_lercvois, 
     .                               s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, 
     .                               nesdvois,nercvois,lesdvois,lercvois, 
     .                               alpha_mat,s_alpha,r_alpha)
          call stoptime(timers,timer_spmdcfd)       
        endif
      ENDIF
C
C-----------------------------------------------
C zeroing N4_VOIS & test if remote set it
C-----------------------------------------------
      IF(NSPMD > 1)THEN
         IF (N2D  ==  0) THEN
            NFIRST = 1+ITASK*(NUMELS+NUMELQ)/NTHREAD
            NLAST = (ITASK+1)*(NUMELS+NUMELQ)/NTHREAD
         ELSE
            NFIRST = 1+ITASK*(NUMELQ)/NTHREAD
            NLAST = (ITASK+1)*(NUMELQ)/NTHREAD
         ENDIF
         do itrimat=1,trimat
          DO I=1,NV46
            flux_vois_mat(NFIRST:NLAST,I,itrimat) = -EP20
          ENDDO
        enddo
C----------------------
        if(itask==0) then
          call startime(timers,timer_spmdcfd)
          flag = 1
          n_entity = s_alpha_mat          
          call spmd_exch_n_neighbor(flag,nspmd,n_entity,trimat,s_lesdvois,s_lercvois, 
     .                               s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, 
     .                               nesdvois,nercvois,lesdvois,lercvois, 
     .                               alpha_mat,s_alpha,r_alpha)
          deallocate(s_alpha)
          deallocate(r_alpha)
          call stoptime(timers,timer_spmdcfd)
        endif

        CALL MY_BARRIER
C----------------------
      ENDIF
C-----------------------------------------------
C     LAW51
C     submatrial volumefluxes update
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF(IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        !------------------------------!
        ! UNPLUG CONDITIONS            !
        !------------------------------!
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE
!        IF(IPARG(64,NG) == 1) CYCLE        
        !------------------------------------!
        ! VOLUME FLUXES UPDATE (SUBMATERIAL) !
        !------------------------------------!
        LFT=1
        VOLG => ELBUF_TAB(NG)%GBUF%VOL
        IF(N2D == 0)THEN
           IF (ALEMUSCL_Param%IALEMUSCL  >  0) THEN
              CALL ALEMUSCL_UPWIND(flux_mat,flux_sav, ALE_CONNECT, X, IXS, flux_vois_mat,
     .                             nv46,trimat,SEGVAR,s_flux,s_flux_vois)
           ELSE
              CALL ALE51_ANTIDIFF3(flux_mat,flux_sav, ALE_CONNECT, alpha_mat,s_alpha_mat,VOLG, IXS,   
     .                             flux_vois_mat,nv46,trimat, SEGVAR,s_flux,s_flux_vois)
           ENDIF
        ELSE
           IF (ALEMUSCL_Param%IALEMUSCL  >  0) THEN
              CALL ALEMUSCL_UPWIND2(flux_mat,flux_sav, ALE_CONNECT, X    , IXQ, flux_vois_mat, 
     .                              NV46,trimat,SEGVAR,s_flux,s_flux_vois)
           ELSE
              CALL ALE51_ANTIDIFF2(flux_mat,flux_sav,ALE_CONNECT,alpha_mat,s_alpha_mat,VOLG  ,flux_vois_mat,
     .                             trimat,SEGVAR,s_flux,nv46,s_flux_vois)
           ENDIF
        ENDIF
      ENDDO !next NG
C--------------------
      CALL MY_BARRIER
C--------------------

      if(nspmd > 1)THEN

        if(itask==0) then
          call startime(timers,timer_spmdcfd)
          flag = 0
          s_proc_nb = 0
          r_proc_nb = 0      
          n_entity = s_flux_vois
          allocate(s_flux_neigh(nspmd))
          allocate(r_flux_neigh(nspmd))

          call spmd_exch_n_neighbor(flag,nspmd,n_entity,nv46,trimat,s_lesdvois,s_lercvois, 
     .                               s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, 
     .                               nesdvois,nercvois,lesdvois,lercvois, 
     .                               flux_vois_mat,s_flux_neigh,r_flux_neigh)
          flag = 1
          call spmd_exch_n_neighbor(flag,nspmd,n_entity,nv46,trimat,s_lesdvois,s_lercvois, 
     .                               s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, 
     .                               nesdvois,nercvois,lesdvois,lercvois, 
     .                               flux_vois_mat,s_flux_neigh,r_flux_neigh)
          deallocate(s_flux_neigh)
          deallocate(r_flux_neigh)
          call stoptime(timers,timer_spmdcfd)
        endif
        call my_barrier        
      endif

      if(nspmd>1) then
        call ale51_apply_remote_flux(n_entity0,nv46,trimat,s_flux,s_flux_vois,flux_mat,flux_vois_mat,ale_connect)
        call my_barrier ! wait for all tasks to finish the operations flux_voix --> flux
      end if


C-----------------------------------------------
C     UPDATING VOLUME FLUXES & UPWIND
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF(IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        !------------------------------!
        ! UNPLUG CONDITIONS            !
        !------------------------------!
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE    
        LFT    =  1
        NF1    =  NFT+1
        UVAR   => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
        !DDVOL*DT IS SUM OF INCOMING AND OUTGOING VOLUMES
        !------------------------------!
        ! UPWIND, QMV, DDVOL           !
        !------------------------------!
        IF(N2D == 0)THEN
          do itrimat=1,trimat
            ADD    = (M51_N0PHAS + (ITRIMAT-1)*M51_NVPHAS+12)*LLT
            pDDVOL => UVAR(ADD+1:ADD+LLT)
            CALL ALE51_UPWIND3(PM,IXS,flux_mat(:,:,itrimat),val2_mat(:,itrimat),ALE_CONNECT,
     .                         ITRIMAT,pDDVOL,qmv_mat(:,:,itrimat),1,
     .                         NV46,s_flux,s_qmv,
     .                         ALE_CONNECT%group(ng)%nb_element_with_bc,ALE_CONNECT%group(ng)%list_element_with_bc)
          enddo
        ELSE
          do itrimat=1,trimat
            ADD    = (M51_N0PHAS + (ITRIMAT-1)*M51_NVPHAS+12)*LLT
            pDDVOL => UVAR(ADD+1:ADD+LLT)            
            CALL ALE51_UPWIND2(PM,X,IXQ,flux_mat(:,:,itrimat),val2_mat(:,itrimat),ALE_CONNECT,
     .                         ITRIMAT,pDDVOL,qmv_mat(:,:,itrimat),1,nv46,s_flux,s_qmv,
     .                         ALE_CONNECT%group(ng)%nb_element_with_bc,ALE_CONNECT%group(ng)%list_element_with_bc)     
          enddo
        ENDIF
      ENDDO

C--------------------
      CALL MY_BARRIER
C--------------------

      IF(ITASK == 0)THEN
        DEALLOCATE(flux_vois_mat)
      END IF
C-----------------------------------------------
      RETURN
      END
C
