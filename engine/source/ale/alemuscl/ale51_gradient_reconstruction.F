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
!||    geom                            ../engine/source/ale/alemuscl/geom.F
!||    gradient_limitation             ../engine/source/ale/alemuscl/gradient_limitation.F
!||    gradient_reconstruction         ../engine/source/ale/alemuscl/gradient_reconstruction.F
!||    initbuf                         ../engine/share/resol/initbuf.F
!||    my_barrier                      ../engine/source/system/machine.F
!||    spmd_e1vois                     ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_exch_min_max               ../engine/source/mpi/ale/spmd_exch_min_max.F
!||    spmd_exchange_grad              ../engine/source/mpi/fluid/spmd_exchange_grad.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod            ../common_source/modules/ale/ale_connectivity_mod.F
!||    alemuscl_mod                    ../common_source/modules/ale/alemuscl_mod.F
!||    elbufdef_mod                    ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                     ../common_source/modules/elements/element_mod.F90
!||    initbuf_mod                     ../engine/share/resol/initbuf.F
!||    multimat_param_mod              ../common_source/modules/multimat_param_mod.F90
!||    segvar_mod                      ../engine/share/modules/segvar_mod.F
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
      INTEGER :: LENCOM, NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*)
      INTEGER :: IAD_ELEM(2, *), FR_ELEM(*)
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
      my_real :: VOL, A(3), B(3), C(3)
      integer :: isolnod
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
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
               II     = I+NFT    
               
               !!!centroid element
               ALEMUSCL_Buffer%ELCENTER(II,1) = ZERO ;
               ALEMUSCL_Buffer%ELCENTER(II,2) = ZERO ;
               ALEMUSCL_Buffer%ELCENTER(II,3) = ZERO
               VOL = ZERO
               !!! Face 1
               !!! Points 321
               A = X(1:3, IXS(1+3, II)) ; B = X(1:3, IXS(2+1, II)) ; C = X(1:3, IXS(1+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)
               !!! Points 314
               A = X(1:3, IXS(3+1, II)) ; B = X(1:3, IXS(1+1, II)) ; C = X(1:3, IXS(4+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Face 2
               !!! Points 473
               A = X(1:3, IXS(4+1, II)) ; B = X(1:3, IXS(7+1, II)) ; C = X(1:3, IXS(3+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Points 487
               A = X(1:3, IXS(4+1, II)) ; B = X(1:3, IXS(8+1, II)) ; C = X(1:3, IXS(7+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Face 3
               !!! Points 678
               A = X(1:3, IXS(6+1, II)) ; B = X(1:3, IXS(7+1, II)) ; C = X(1:3, IXS(8+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Points 685
               A = X(1:3, IXS(6+1, II)) ; B = X(1:3, IXS(8+1, II)) ; C = X(1:3, IXS(5+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Face 4
               !!! Points 126
               A = X(1:3, IXS(1+1, II)) ; B = X(1:3, IXS(2+1, II)) ; C = X(1:3, IXS(6+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Points 165
               A = X(1:3, IXS(1+1, II)) ; B = X(1:3, IXS(6+1, II)) ; C = X(1:3, IXS(5+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Face 5
               !!! Points 236
               A = X(1:3, IXS(2+1, II)) ; B = X(1:3, IXS(3+1, II)) ; C = X(1:3, IXS(6+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Points 376
               A = X(1:3, IXS(3+1, II)) ; B = X(1:3, IXS(7+1, II)) ; C = X(1:3, IXS(6+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Face 6
               !!! Points 154
               A = X(1:3, IXS(1+1, II)) ; B = X(1:3, IXS(5+1, II)) ; C = X(1:3, IXS(4+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)

               !!! Points 458
               A = X(1:3, IXS(4+1, II)) ; B = X(1:3, IXS(5+1, II)) ; C = X(1:3, IXS(8+1, II))
               CALL GEOM(A, B, C,ALEMUSCL_Buffer%ELCENTER(II,1),ALEMUSCL_Buffer%ELCENTER(II,2),ALEMUSCL_Buffer%ELCENTER(II,3), VOL)


               VOL = VOL / 6.D0
               ALEMUSCL_Buffer%ELCENTER(II,1:3) = ALEMUSCL_Buffer%ELCENTER(II,1:3) / 12.D0 / VOL
               
               
            ENDDO  
         ENDIF
      ENDDO  ! NG=ITASK+1,NGROUP,NTHREAD
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
      
      !!! MPI Comm
      IF(NSPMD  > 1)THEN
        if(itask==0) call startime(timers,timer_spmdcfd)
!$OMP SINGLE
         !!! Volumic fractions comm
         DO ITRIMAT = 1, TRIMAT
            CALL SPMD_E1VOIS(ALEMUSCL_Buffer%VOLUME_FRACTION(1,ITRIMAT), NERCVOIS, NESDVOIS,
     .           LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
         !!! Centroid coordinates comm
         DO JJ = 1, 3
            CALL SPMD_E1VOIS(ALEMUSCL_Buffer%ELCENTER(1,JJ), NERCVOIS, NESDVOIS,
     .           LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
!$OMP END SINGLE
         if(itask==0) call stoptime(timers,timer_spmdcfd)
      ENDIF
      CALL MY_BARRIER 
      
      FIRST = 1 + ITASK * NUMNOD / NTHREAD
      LAST = (1 + ITASK) * NUMNOD / NTHREAD
      ALEMUSCL_Buffer%NODE_MAX_VALUE(FIRST:LAST,1:TRIMAT) = -EP30
      ALEMUSCL_Buffer%NODE_MIN_VALUE(FIRST:LAST,1:TRIMAT) = EP30
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
       !!! MPI Comm
      IF(NSPMD  > 1)THEN
        if(itask==0) call startime(timers,timer_spmdcfd)
!$OMP SINGLE
         DO ITRIMAT = 1, TRIMAT
            CALL  SPMD_EXCH_MIN_MAX(IAD_ELEM ,FR_ELEM ,
     .                              ALEMUSCL_Buffer%NODE_MIN_VALUE(1,ITRIMAT), ALEMUSCL_Buffer%NODE_MAX_VALUE(1,ITRIMAT) )
         ENDDO
!$OMP END SINGLE
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
         isolnod = iparg(28,ng)
         facet_nb = 6
         if(ity==1.and.isolnod==4) facet_nb = 4
         if(ity==2) facet_nb = 4
         LFT = 1
         !!!   Reconstruct gradient
         nel = llt - lft + 1 
         call gradient_reconstruction_new(numels,numnod,nel,nft,trimat,facet_nb,ixs,x,ale_connect,segvar)

!         LFT = 1
!         !!!   Reconstruct gradient
!         DO ITRIMAT = 1, TRIMAT
!            CALL GRADIENT_RECONSTRUCTION( IXS, X, ALE_CONNECT, ITRIMAT, SEGVAR)
!         ENDDO  !  ITRIMAT = 1, TRIMAT     
      END DO  !  NG=ITASK+1,NGROUP,NTHREAD 

      CALL MY_BARRIER

      IF (NSPMD   >  1) THEN
C     MPI Communication for gradients
        if(itask==0) call startime(timers,timer_spmdcfd)
!$OMP SINGLE
        DO ITRIMAT = 1, TRIMAT
          CALL SPMD_EXCHANGE_GRAD(3, NUMELS + NSVOIS + NUMELQ + NQVOIS,3,ALEMUSCL_Buffer%GRAD(1,1,ITRIMAT), 
     .         NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
        ENDDO
!$OMP END SINGLE         
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


      SUBROUTINE GRADIENT_RECONSTRUCTION(IXS, X, ALE_CONNECT, ITRIMAT,SEGVAR)
C-----------------------------------------------
C  D e s c r i p t i o n
C  This subroutine computes a gradient of the scalar field value in each 
C  element:
C        mean square approximation of the gradient in the face related 
C        neighborhood of the element
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE ALEMUSCL_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
      use element_mod , only :nixs
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "vect01_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXS(NIXS,NUMELS)
      my_real, INTENT(IN) :: X(3,NUMNOD)
      INTEGER, INTENT(IN) :: ITRIMAT
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, KK, IAD2, LGTH
      my_real :: XK, YK, ZK, XL, YL, ZL, XF, YF, ZF
      my_real :: VALK, VALL
      my_real :: mat(3, 3), rhs(3), sol(3)
      INTEGER :: VOIS_ID
      INTEGER :: FACE_TO_NODE_LOCAL_ID(6, 4)
      my_real :: NORM(3), A(3), B(3), C(3), SURF, SURF1, SURF2
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
!!! Once for all, associate node local id to a face number
!!! Face 1
      FACE_TO_NODE_LOCAL_ID(1, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(1, 2) = 4
      FACE_TO_NODE_LOCAL_ID(1, 3) = 3 ; FACE_TO_NODE_LOCAL_ID(1, 4) = 2
!!! Face 2
      FACE_TO_NODE_LOCAL_ID(2, 1) = 3 ; FACE_TO_NODE_LOCAL_ID(2, 2) = 4
      FACE_TO_NODE_LOCAL_ID(2, 3) = 8 ; FACE_TO_NODE_LOCAL_ID(2, 4) = 7
!!! Face 3
      FACE_TO_NODE_LOCAL_ID(3, 1) = 5 ; FACE_TO_NODE_LOCAL_ID(3, 2) = 6
      FACE_TO_NODE_LOCAL_ID(3, 3) = 7 ; FACE_TO_NODE_LOCAL_ID(3, 4) = 8
!!! Face 4
      FACE_TO_NODE_LOCAL_ID(4, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(4, 2) = 2
      FACE_TO_NODE_LOCAL_ID(4, 3) = 6 ; FACE_TO_NODE_LOCAL_ID(4, 4) = 5
!!! Face 5
      FACE_TO_NODE_LOCAL_ID(5, 1) = 2 ; FACE_TO_NODE_LOCAL_ID(5, 2) = 3
      FACE_TO_NODE_LOCAL_ID(5, 3) = 7 ; FACE_TO_NODE_LOCAL_ID(5, 4) = 6
!!! Face 6
      FACE_TO_NODE_LOCAL_ID(6, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(6, 2) = 5
      FACE_TO_NODE_LOCAL_ID(6, 3) = 8 ; FACE_TO_NODE_LOCAL_ID(6, 4) = 4  

      DO I = LFT, LLT
         II = I + NFT
         !!! Reset mat, rhs
         mat(1:3, 1:3) = ZERO ; rhs(1:3) = ZERO
         !!! Value of the target function in the element
         VALK = ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT)
         XK = ALEMUSCL_Buffer%ELCENTER(II,1) ;
         YK = ALEMUSCL_Buffer%ELCENTER(II,2) ;
         ZK = ALEMUSCL_Buffer%ELCENTER(II,3)
         !!! IXS(2:9, II) : Node global ID
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
         LGTH = ALE_CONNECT%ee_connect%iad_connect(II+1)-IAD2
         DO KK = 1, LGTH
            VOIS_ID = ALE_CONNECT%ee_connect%connected(IAD2 + KK - 1)
            !IF (VOIS_ID /= 0 .AND. VOIS_ID <= NUMELS) THEN
            IF (VOIS_ID > 0) THEN
               !!! Value of the target function in the current neighbor
               VALL = ALEMUSCL_Buffer%VOLUME_FRACTION(VOIS_ID,ITRIMAT)
               XL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,1) ;
               YL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,2) ; 
               ZL = ALEMUSCL_Buffer%ELCENTER(VOIS_ID,3) ;
            ELSE
               IF(VOIS_ID == 0) THEN
                 VALL = VALK
               ELSE
                 !vois_id<0 : means EBCS), -vois_id is seg_id
                 VALL = SEGVAR%PHASE_ALPHA(ITRIMAT,-VOIS_ID)
               ENDIF
               XF = ZERO
               YF = ZERO
               ZF = ZERO

               A(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 1) + 1, II))
               B(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 2) + 1, II))
               C(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 3) + 1, II))

               NORM(1) = (B(2) - A(2)) * (C(3) - A(3)) - (B(3) - A(3)) * (C(2) - A(2))
               NORM(2) = (B(3) - A(3)) * (C(1) - A(1)) - (B(1) - A(1)) * (C(3) - A(3))
               NORM(3) = (B(1) - A(1)) * (C(2) - A(2)) - (B(2) - A(2)) * (C(1) - A(1))

               SURF1 = HALF * ABS(SQRT(NORM(1) * NORM(1) + NORM(2) * NORM(2) + NORM(3) * NORM(3)))
               XF = SURF1 * THIRD * (A(1) + B(1) + C(1))
               YF = SURF1 * THIRD * (A(2) + B(2) + C(2))
               ZF = SURF1 * THIRD * (A(3) + B(3) + C(3))

               A(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 1) + 1, II))
               B(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 3) + 1, II))
               C(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 4) + 1, II))

               NORM(1) = (B(2) - A(2)) * (C(3) - A(3)) - (B(3) - A(3)) * (C(2) - A(2))
               NORM(2) = (B(3) - A(3)) * (C(1) - A(1)) - (B(1) - A(1)) * (C(3) - A(3))
               NORM(3) = (B(1) - A(1)) * (C(2) - A(2)) - (B(2) - A(2)) * (C(1) - A(1))

               SURF2 = HALF * ABS(SQRT(NORM(1) * NORM(1) + NORM(2) * NORM(2) + NORM(3) * NORM(3)))
               XF = XF + SURF2 * THIRD * (A(1) + B(1) + C(1))
               YF = YF + SURF2 * THIRD * (A(2) + B(2) + C(2))
               ZF = ZF + SURF2 * THIRD * (A(3) + B(3) + C(3)) 
               
               SURF = SURF1 + SURF2
               XF = XF / SURF
               YF = YF / SURF
               ZF = ZF / SURF
               !!! Build face centroid
c$$$               DO JJ = 1, 4
c$$$                  NODE_ID = IXS(FACE_TO_NODE_LOCAL_ID(KK, JJ) + 1, II)
c$$$                  XF = XF + FOURTH * X(1, NODE_ID)
c$$$                  YF = YF + FOURTH * X(2, NODE_ID)
c$$$                  ZF = ZF + FOURTH * X(3, NODE_ID)
c$$$               ENDDO
               XL = TWO * XF - ALEMUSCL_Buffer%ELCENTER(II,1)
               YL = TWO * YF - ALEMUSCL_Buffer%ELCENTER(II,2)
               ZL = TWO * ZF - ALEMUSCL_Buffer%ELCENTER(II,3)
            ENDIF

            !!! Incrementing mat and rhs
            rhs(1) = rhs(1) + (VALK - VALL) * (XL - XK)
            rhs(2) = rhs(2) + (VALK - VALL) * (YL - YK)
            rhs(3) = rhs(3) + (VALK - VALL) * (ZL - ZK)
            mat(1, 1) = mat(1, 1) + (XL - XK) * (XL - XK)
            mat(1, 2) = mat(1, 2) + (XL - XK) * (YL - YK)
            mat(1, 3) = mat(1, 3) + (XL - XK) * (ZL - ZK)
            mat(2, 1) = mat(2, 1) + (YL - YK) * (XL - XK)
            mat(2, 2) = mat(2, 2) + (YL - YK) * (YL - YK)
            mat(2, 3) = mat(2, 3) + (YL - YK) * (ZL - ZK)
            mat(3, 1) = mat(3, 1) + (ZL - ZK) * (XL - XK)
            mat(3, 2) = mat(3, 2) + (ZL - ZK) * (YL - YK)
            mat(3, 3) = mat(3, 3) + (ZL - ZK) * (ZL - ZK)
         ENDDO

         CALL CG(3, mat, rhs, sol, 3, EM10)
         !CALL DIRECT_SOLVE(mat, rhs ,sol)
         !!! Solution goes to the gradient
         ALEMUSCL_Buffer%GRAD(II,1,ITRIMAT) = -sol(1)
         ALEMUSCL_Buffer%GRAD(II,2,ITRIMAT) = -sol(2)
         ALEMUSCL_Buffer%GRAD(II,3,ITRIMAT) = -sol(3)
      ENDDO  ! I = LFT, LLT
C-----------------------------------------------      
      END SUBROUTINE GRADIENT_RECONSTRUCTION      
      
