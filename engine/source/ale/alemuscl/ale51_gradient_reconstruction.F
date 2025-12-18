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
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||--- called by ------------------------------------------------------
!||    alethe                          ../engine/source/ale/alethe.F
!||--- calls      -----------------------------------------------------
!||    geom_vec                        ../engine/source/ale/alemuscl/geom_vec.F90
!||    gradient_limitation             ../engine/source/ale/alemuscl/gradient_limitation.F
!||    gradient_reconstruction_new     ../engine/source/ale/alemuscl/gradient_reconstruction.F90
!||    initbuf                         ../engine/share/resol/initbuf.F
!||    my_barrier                      ../engine/source/system/machine.F
!||    spmd_exch_min_max               ../engine/source/mpi/ale/spmd_exch_min_max.F90
!||    spmd_exch_n_neighbor            ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    spmd_exchange_grad              ../engine/source/mpi/fluid/spmd_exchange_grad.F
!||    startime                        ../engine/source/system/timer_mod.F90
!||    stoptime                        ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod            ../common_source/modules/ale/ale_connectivity_mod.F
!||    alemuscl_mod                    ../common_source/modules/ale/alemuscl_mod.F
!||    array_mod                       ../common_source/modules/array_mod.F
!||    elbufdef_mod                    ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                     ../common_source/modules/elements/element_mod.F90
!||    geom_vec_mod                    ../engine/source/ale/alemuscl/geom_vec.F90
!||    gradient_reconstruction_mod     ../engine/source/ale/alemuscl/gradient_reconstruction.F90
!||    initbuf_mod                     ../engine/share/resol/initbuf.F
!||    multimat_param_mod              ../common_source/modules/multimat_param_mod.F90
!||    mvsiz_mod                       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod                   ../common_source/modules/precision_mod.F90
!||    segvar_mod                      ../engine/share/modules/segvar_mod.F
!||    spmd_exch_min_max_mod           ../engine/source/mpi/ale/spmd_exch_min_max.F90
!||    spmd_exch_n_neighbor_mod        ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    timer_mod                       ../engine/source/system/timer_mod.F90
!||    trimat_mod                      ../engine/share/modules/trimat.F
!||====================================================================
      SUBROUTINE ALE51_GRADIENT_RECONSTRUCTION(IPARG   , ELBUF_TAB, IXS    ,  X       , ALE_CONNECT,
     .                                         NERCVOIS ,NESDVOIS,  LERCVOIS, LESDVOIS   ,LENCOM, ITASK,
     .                                         IAD_ELEM, FR_ELEM  ,SEGVAR,timers)
C-----------------------------------------------
C   D e s c r i p t i o n
C   Computes limited gradients for volumic fractions
C   of LAW51 species
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD 
      USE ALEMUSCL_MOD
      USE TRIMAT_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
      use element_mod , only :nixs
      use timer_mod
      use gradient_reconstruction_mod , only : gradient_reconstruction_new
      use precision_mod , only : WP
      use mvsiz_mod, only : MVSIZ
      use spmd_exch_min_max_mod , only : spmd_exch_min_max
      use geom_vec_mod , only : geom_vec
      use spmd_exch_n_neighbor_mod , only : spmd_exch_n_neighbor
      use array_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr07_c.inc"
#include "spmd_c.inc"
#include "com01_c.inc"
#include "com04_c.inc"
#include "vect01_c.inc"
#include "param_c.inc"
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER :: ITASK
      INTEGER IPARG(NPARG,*), IXS(NIXS,*)
      my_real :: X(3,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER :: LENCOM, NERCVOIS(nspmd+1),NESDVOIS(nspmd+1),LERCVOIS(*),LESDVOIS(*)
      INTEGER :: IAD_ELEM(2,nspmd+1), FR_ELEM(*)
      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      type(timer_), intent(inout) :: timers
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG
      INTEGER :: ITRIMAT
      integer :: nel,facet_nb
      my_real, DIMENSION(:), POINTER :: VOLG, VOLP, UVAR
      INTEGER :: ADD
      INTEGER :: K, I, II, JJ, NODE_ID, JMIN, JMAX
      INTEGER :: ELEM_ID
      INTEGER :: FIRST,LAST      
      my_real :: vol
      integer :: isolnod,n_entity
      real(kind=WP), dimension(3,12) :: node_pos
      real(kind=WP), dimension(mvsiz,3,12) :: a,b,c
      real(kind=WP), dimension(3) :: a0,b0,c0

      integer :: flag
      integer :: s_proc_nb_1,r_proc_nb_1,s_proc_nb_2,r_proc_nb_2,s_proc_nb_3,r_proc_nb_3
      integer :: s_lesdvois,s_lercvois,s_fr_elem,buff_size
      integer, dimension(nspmd) :: s_index_1,r_index_1,s_index_2,r_index_2,s_index_3,r_index_3
      integer, dimension(nspmd) :: s_req_1,r_req_1,s_req_2,r_req_2,s_req_3,r_req_3
      real(kind=WP), dimension(:), allocatable :: s_buffer,r_buffer
      type(array_type_my_real_2d), dimension(:), allocatable :: s_elcenter,r_elcenter
      type(array_type_my_real_2d), dimension(:), allocatable :: s_volfrac,r_volfrac
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      s_lesdvois = nesdvois(nspmd+1)
      s_lercvois = nercvois(nspmd+1)
      s_fr_elem = iad_elem(1,nspmd+1) - iad_elem(1,1)
      n_entity = numels+nsvois+numelq+nqvois           
     
      DO NG=ITASK+1,NGROUP,NTHREAD      
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF(IPARG    ,NG      ,                  
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
         IF(JALE+JEUL == 0)    CYCLE
         IF(IPARG(8,NG) == 1)  CYCLE
         IF(IPARG(1,NG)  /= 51) CYCLE
         IF ((JALE   /=  0) .OR. ((JEUL   /=  0) .AND. (NCYCLE  ==  0 .OR. MCHECK   /=  0))) THEN
            !!!   Volume fraction
            DO I=LFT,LLT 
              ii = i + nft    
               
              !!!centroid element
              ALEMUSCL_Buffer%ELCENTER(II,1) = ZERO
              ALEMUSCL_Buffer%ELCENTER(II,2) = ZERO
              ALEMUSCL_Buffer%ELCENTER(II,3) = ZERO
              vol = ZERO

              node_pos(1:3,1) = x(1:3,ixs(1+1,ii)) ! node 1
              node_pos(1:3,2) = x(1:3,ixs(1+2,ii)) ! node 2
              node_pos(1:3,3) = x(1:3,ixs(1+3,ii)) ! node 3
              node_pos(1:3,4) = x(1:3,ixs(1+4,ii)) ! node 4
              node_pos(1:3,5) = x(1:3,ixs(1+5,ii)) ! node 5
              node_pos(1:3,6) = x(1:3,ixs(1+6,ii)) ! node 6
              node_pos(1:3,7) = x(1:3,ixs(1+7,ii)) ! node 7
              node_pos(1:3,8) = x(1:3,ixs(1+8,ii)) ! node 8
              ! face 1 : 321 & 314
              a(i,1:3,1) = node_pos(1:3,3)           
              b(i,1:3,1) = node_pos(1:3,2)
              c(i,1:3,1) = node_pos(1:3,1)
              a(i,1:3,2) = node_pos(1:3,3)
              b(i,1:3,2) = node_pos(1:3,1)
              c(i,1:3,2) = node_pos(1:3,4)

              ! face 2 : 473 & 487
              a(i,1:3,3) = node_pos(1:3,4)
              b(i,1:3,3) = node_pos(1:3,7)
              c(i,1:3,3) = node_pos(1:3,3)
              a(i,1:3,4) = node_pos(1:3,4)
              b(i,1:3,4) = node_pos(1:3,8)
              c(i,1:3,4) = node_pos(1:3,7)

              ! face 3 : 678 & 685
              a(i,1:3,5) = node_pos(1:3,6)
              b(i,1:3,5) = node_pos(1:3,7)
              c(i,1:3,5) = node_pos(1:3,8)
              a(i,1:3,6) = node_pos(1:3,6)
              b(i,1:3,6) = node_pos(1:3,8)
              c(i,1:3,6) = node_pos(1:3,5)

              ! face 4 : 126 & 165
              a(i,1:3,7) = node_pos(1:3,1)
              b(i,1:3,7) = node_pos(1:3,2)
              c(i,1:3,7) = node_pos(1:3,6)
              a(i,1:3,8) = node_pos(1:3,1)
              b(i,1:3,8) = node_pos(1:3,6)
              c(i,1:3,8) = node_pos(1:3,5)

              ! face 5 : 236 & 376
              a(i,1:3,9)  = node_pos(1:3,2)
              b(i,1:3,9)  = node_pos(1:3,3)
              c(i,1:3,9)  = node_pos(1:3,6)
              a(i,1:3,10) = node_pos(1:3,3)
              b(i,1:3,10) = node_pos(1:3,7)
              c(i,1:3,10) = node_pos(1:3,6)

              ! face 6 : 154 & 458
              a(i,1:3,11) = node_pos(1:3,1)
              b(i,1:3,11) = node_pos(1:3,5)
              c(i,1:3,11) = node_pos(1:3,4)
              a(i,1:3,12) = node_pos(1:3,4)
              b(i,1:3,12) = node_pos(1:3,5)
              c(i,1:3,12) = node_pos(1:3,8)
            ENDDO
            nel = llt - lft + 1 
            call geom_vec(nel,nft,n_entity,a,b,c,alemuscl_buffer%elcenter)
         ENDIF
      ENDDO  ! NG=ITASK+1,NGROUP,NTHREAD

      CALL MY_BARRIER   
      ! ------------
      ! MPI comms : iSend & iRecv Centroid coordinates
      if(nspmd>1) then
        if(itask==0) call startime(timers,timer_spmdcfd)
        if(itask==0) then
          flag = 0
          s_proc_nb_1 = 0
          r_proc_nb_1 = 0
         allocate(s_elcenter(nspmd))
         allocate(r_elcenter(nspmd))
          call spmd_exch_n_neighbor(flag,nspmd,n_entity,3,s_lesdvois,s_lercvois, 
     .                              s_proc_nb_1,r_proc_nb_1,s_index_1,r_index_1,s_req_1,r_req_1, 
     .                              nesdvois,nercvois,lesdvois,lercvois, 
     .                              alemuscl_buffer%elcenter,s_elcenter,r_elcenter)
        endif
         if(itask==0) call stoptime(timers,timer_spmdcfd)
      endif
      ! ------------

      DO NG=ITASK+1,NGROUP,NTHREAD      
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF(IPARG    ,NG      ,                  
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
         IF(JALE+JEUL == 0)    CYCLE
         IF(IPARG(8,NG) == 1)  CYCLE
         IF(IPARG(1,NG)  /= 51) CYCLE
         VOLG => ELBUF_TAB(NG)%GBUF%VOL
         UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
         LFT=1
         DO ITRIMAT = 1, TRIMAT   
            ADD    = M51_N0PHAS + (ITRIMAT-1)*M51_NVPHAS ! ADD => SIG(1)
            ADD    = ADD + 11   ! ADD + 11 => VOLUME_Phase
            K      = LLT*(ADD-1) ! VAR(I,ADD) = VAR(K+I) 
            VOLP   =>UVAR(K+1:K+LLT)
            !!!   Volume fraction
            DO I=LFT,LLT 
               II     = I+NFT    
               ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT) = VOLP(I)/VOLG(I)
               ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT) = 
     .              MAX(ZERO,MIN(ONE,ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT)))                
            ENDDO  
         ENDDO  
      ENDDO  ! NG=ITASK+1,NGROUP,NTHREAD

      CALL MY_BARRIER
      ! ------------
      ! MPI comms : Wait & Finalize Centroid coordinates + blocking mpi comms for fraction of volume
      if(nspmd>1) then
        if(itask==0) call startime(timers,timer_spmdcfd)
        if(itask==0) then
          ! wait & Finalize Centroid coordinates 
          flag = 1
          call spmd_exch_n_neighbor(flag,nspmd,n_entity,3,s_lesdvois,s_lercvois, 
     .                              s_proc_nb_1,r_proc_nb_1,s_index_1,r_index_1,s_req_1,r_req_1, 
     .                              nesdvois,nercvois,lesdvois,lercvois, 
     .                              alemuscl_buffer%elcenter,s_elcenter,r_elcenter)
         deallocate(s_elcenter)
         deallocate(r_elcenter)

          ! iSend & iRecv fraction of volume
          flag = 0
          s_proc_nb_2 = 0
          r_proc_nb_2 = 0
          allocate(s_volfrac(nspmd))
          allocate(r_volfrac(nspmd))
          call spmd_exch_n_neighbor(flag,nspmd,n_entity,TRIMAT,s_lesdvois,s_lercvois, 
     .                              s_proc_nb_2,r_proc_nb_2,s_index_2,r_index_2,s_req_2,r_req_2, 
     .                              nesdvois,nercvois,lesdvois,lercvois, 
     .                              alemuscl_buffer%volume_fraction,s_volfrac,r_volfrac)
          ! wait & Finalize fraction of volume
          flag = 1
          call spmd_exch_n_neighbor(flag,nspmd,n_entity,TRIMAT,s_lesdvois,s_lercvois,
     .                              s_proc_nb_2,r_proc_nb_2,s_index_2,r_index_2,s_req_2,r_req_2,
     .                              nesdvois,nercvois,lesdvois,lercvois,
     .                              alemuscl_buffer%volume_fraction,s_volfrac,r_volfrac)
          deallocate(s_volfrac)
          deallocate(r_volfrac) 
        endif
         if(itask==0) call stoptime(timers,timer_spmdcfd)
      endif
      ! ------------      

      CALL MY_BARRIER 
      
      FIRST = 1 + ITASK * NUMNOD / NTHREAD
      LAST = (1 + ITASK) * NUMNOD / NTHREAD
      do itrimat=1,trimat
        ALEMUSCL_Buffer%NODE_MAX_VALUE(FIRST:LAST,itrimat) = -EP30
        ALEMUSCL_Buffer%NODE_MIN_VALUE(FIRST:LAST,itrimat) = EP30
      enddo
      DO ITRIMAT = 1, TRIMAT
        DO NODE_ID = FIRST,LAST
            JMIN = ALEMUSCL_Buffer%pADDCNEL(NODE_ID)
            JMAX = ALEMUSCL_Buffer%pADDTMPL(NODE_ID) - 1
            DO JJ = JMIN, JMAX
               ELEM_ID = ALEMUSCL_Buffer%pCNEL(JJ)
               IF (ELEM_ID   /=  0 .AND. ELEM_ID <= NUMELS) THEN
                  ALEMUSCL_Buffer%NODE_MAX_VALUE(NODE_ID,ITRIMAT) = MAX(ALEMUSCL_Buffer%NODE_MAX_VALUE(NODE_ID,ITRIMAT), 
     .                 ALEMUSCL_Buffer%VOLUME_FRACTION(ELEM_ID,ITRIMAT))
                  ALEMUSCL_Buffer%NODE_MIN_VALUE(NODE_ID,ITRIMAT) = MIN(ALEMUSCL_Buffer%NODE_MIN_VALUE(NODE_ID,ITRIMAT), 
     .                 ALEMUSCL_Buffer%VOLUME_FRACTION(ELEM_ID,ITRIMAT)) 
               ENDIF
            ENDDO
         ENDDO
      ENDDO   
   
      CALL MY_BARRIER 
      ! ------------
      ! MPI comms : iSend & iRecv min/max
      if(nspmd>1) then
        if(itask==0) call startime(timers,timer_spmdcfd)
        if(itask==0) then
          flag = 0
          s_proc_nb_3 = 0
          r_proc_nb_3 = 0
          buff_size = iad_elem(1,nspmd+1)
          allocate(s_buffer(2*buff_size*trimat))
          allocate(r_buffer(2*buff_size*trimat))
          call spmd_exch_min_max(flag,nspmd,numnod,buff_size,s_proc_nb_3,r_proc_nb_3,             
     .                           s_fr_elem,trimat,s_index_3,r_index_3,s_req_3,r_req_3,fr_elem,iad_elem,            
     .                           alemuscl_buffer%node_min_value,alemuscl_buffer%node_max_value,s_buffer,r_buffer)
        endif
         if(itask==0) call stoptime(timers,timer_spmdcfd)
      endif
      ! ------------     


!$omp do schedule(guided)
      DO NG=1,ngroup
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF(IPARG    ,NG      ,                  
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
         IF(JALE+JEUL == 0)    CYCLE
         IF(IPARG(8,NG) == 1)  CYCLE
         IF(IPARG(1,NG)  /= 51) CYCLE
         isolnod = iparg(28,ng)
         facet_nb = 6
         if(ity==1.and.isolnod==4) facet_nb = 4
         if(ity==2) facet_nb = 4
         LFT = 1
         !!!   Reconstruct gradient
         nel = llt - lft + 1 
         call gradient_reconstruction_new(numels,numnod,nel,nft,trimat,facet_nb,ixs,x,ale_connect,segvar)  
      END DO
!$omp end do      

      CALL MY_BARRIER

      ! ------------
      ! MPI comms : Wait & Finalize min/max
      if(nspmd>1) then
        if(itask==0) call startime(timers,timer_spmdcfd)
        if(itask==0) then
          flag = 1
          buff_size = iad_elem(1,nspmd+1)          
          call spmd_exch_min_max(flag,nspmd,numnod,buff_size,s_proc_nb_3,r_proc_nb_3,             
     .                           s_fr_elem,trimat,s_index_3,r_index_3,s_req_3,r_req_3,fr_elem,iad_elem,            
     .                           alemuscl_buffer%node_min_value,alemuscl_buffer%node_max_value,s_buffer,r_buffer)
          deallocate(s_buffer)
          deallocate(r_buffer)
        endif
         if(itask==0) call stoptime(timers,timer_spmdcfd)
      endif
      ! ------------
      CALL MY_BARRIER

      IF (NSPMD   >  1) THEN
C     MPI Communication for gradients
        if(itask==0) call startime(timers,timer_spmdcfd)
        if(itask==0) then
          call spmd_exchange_grad(n_entity,3,trimat,alemuscl_buffer%grad,
     .                            nercvois,nesdvois,lercvois,lesdvois,lencom)
        endif
        if(itask==0) call stoptime(timers,timer_spmdcfd)
      ENDIF
      CALL MY_BARRIER

      DO NG=ITASK+1,NGROUP,NTHREAD  
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF(IPARG    ,NG      ,                  
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
         IF(JALE+JEUL == 0)    CYCLE
         IF(IPARG(8,NG) == 1)  CYCLE
         IF(IPARG(1,NG)  /= 51) CYCLE
         LFT = 1      
         CALL GRADIENT_LIMITATION(IXS, X, TRIMAT)
     
      ENDDO

      CALL MY_BARRIER
 
C-----------------------------------------------      
      END SUBROUTINE ALE51_GRADIENT_RECONSTRUCTION