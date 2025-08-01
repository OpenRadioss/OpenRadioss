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
!||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
!||--- called by ------------------------------------------------------
!||    alethe                           ../engine/source/ale/alethe.F
!||--- calls      -----------------------------------------------------
!||    gradient_limitation2             ../engine/source/ale/alemuscl/gradient_limitation2.F
!||    gradient_reconstruction2         ../engine/source/ale/alemuscl/gradient_reconstruction2.F
!||    initbuf                          ../engine/share/resol/initbuf.F
!||    my_barrier                       ../engine/source/system/machine.F
!||    spmd_e1vois                      ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_exch_min_max                ../engine/source/mpi/ale/spmd_exch_min_max.F
!||    spmd_exchange_grad               ../engine/source/mpi/fluid/spmd_exchange_grad.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod             ../common_source/modules/ale/ale_connectivity_mod.F
!||    alemuscl_mod                     ../common_source/modules/ale/alemuscl_mod.F
!||    elbufdef_mod                     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod                      ../engine/share/resol/initbuf.F
!||    multimat_param_mod               ../common_source/modules/multimat_param_mod.F90
!||    segvar_mod                       ../engine/share/modules/segvar_mod.F
!||    trimat_mod                       ../engine/share/modules/trimat.F
!||====================================================================
      SUBROUTINE ALE51_GRADIENT_RECONSTRUCTION2(IPARG   ,ELBUF_TAB,IXQ     ,X       ,ALE_CONNECT,
     .                                          NV46    ,NERCVOIS ,NESDVOIS,LERCVOIS,LESDVOIS   ,LENCOM, ITASK,
     .                                          IAD_ELEM,FR_ELEM  ,SEGVAR)
C-----------------------------------------------
C   D e s c r i p t i o n
C   Computes limited gradients for volumic fractions
C   of LAW51 species
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD 
      USE ALEMUSCL_MOD , only : ALEMUSCL_Buffer
      USE TRIMAT_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
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
      INTEGER :: NV46, ITASK
      INTEGER IPARG(NPARG,NGROUP), IXQ(NIXQ,NUMELQ)
      my_real :: X(3,NUMNOD)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER :: LENCOM, NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*)
      INTEGER :: IAD_ELEM(2, *), FR_ELEM(*)
      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG
      INTEGER :: ITRIMAT
      my_real, DIMENSION(:), POINTER :: VOLG, VOLP, UVAR
      INTEGER :: ADD
      INTEGER :: K, I, II, JJ, NODE_ID, JMIN, JMAX
      INTEGER :: ELEM_ID
      INTEGER :: FIRST,LAST
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
               !!! Element centroid
               ALEMUSCL_Buffer%ELCENTER(II,2) = FOURTH * (X(2, IXQ(2, II)) + X(2, IXQ(3, II)) + X(2, IXQ(4, II)) + X(2, IXQ(5, II)))
               ALEMUSCL_Buffer%ELCENTER(II,3) = FOURTH * (X(3, IXQ(2, II)) + X(3, IXQ(3, II)) + X(3, IXQ(4, II)) + X(3, IXQ(5, II)))
            ENDDO  
         ENDIF
      ENDDO  ! NG=ITASK+1,NGROUP,NTHREAD
      DO NG=ITASK+1,NGROUP,NTHREAD      
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF(IPARG    ,NG      ,                  
     2                 MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3                 NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4                 JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5                 NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6                 IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7                 ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
         IF(JALE+JEUL == 0) CYCLE
         IF(IPARG(8,NG) == 1) CYCLE
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
               ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT) = MAX(ZERO,MIN(ONE,ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT))) 
            ENDDO  
         ENDDO  
      ENDDO  ! NG=ITASK+1,NGROUP,NTHREAD

      CALL MY_BARRIER
      
      !!! MPI Comm
      IF(NSPMD > 1)THEN
!$OMP SINGLE
         !!! Volumic fractions comm
         DO ITRIMAT = 1, TRIMAT
            CALL SPMD_E1VOIS(ALEMUSCL_Buffer%VOLUME_FRACTION(1,ITRIMAT), NERCVOIS, NESDVOIS,LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
         !!! Centroid coordinates comm
         DO JJ = 1, 3
            CALL SPMD_E1VOIS(ALEMUSCL_Buffer%ELCENTER(1,JJ), NERCVOIS, NESDVOIS,LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
!$OMP END SINGLE
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
               IF (ELEM_ID   /=  0 .AND. ELEM_ID  <=  NUMELQ) THEN
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
      IF(NSPMD > 1)THEN
!$OMP SINGLE
         DO ITRIMAT = 1, TRIMAT
            CALL  SPMD_EXCH_MIN_MAX(IAD_ELEM ,FR_ELEM ,
     .                              ALEMUSCL_Buffer%NODE_MIN_VALUE(1,ITRIMAT), ALEMUSCL_Buffer%NODE_MAX_VALUE(1,ITRIMAT) )
         ENDDO
!$OMP END SINGLE
      ENDIF
      CALL MY_BARRIER

      DO NG=ITASK+1,NGROUP,NTHREAD  
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF(IPARG   ,NG      ,                  
     2                 MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3                 NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4                 JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5                 NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6                 IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7                 ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
         IF(JALE+JEUL == 0) CYCLE
         IF(IPARG(8,NG) == 1) CYCLE
         IF(IPARG(1,NG)  /= 51) CYCLE
         LFT = 1
         !!!   Reconstruct gradient
         DO ITRIMAT = 1, TRIMAT
            CALL GRADIENT_RECONSTRUCTION2(IXQ, X, ALE_CONNECT, NV46, ITRIMAT, SEGVAR)
         ENDDO  !  ITRIMAT = 1, TRIMAT     
      END DO  !  NG=ITASK+1,NGROUP,NTHREAD 

      CALL MY_BARRIER

      IF (NSPMD  >            1) THEN
C     MPI exchange for gradients
!$OMP SINGLE
         DO ITRIMAT = 1, TRIMAT
            CALL SPMD_EXCHANGE_GRAD(3, NUMELS + NSVOIS + NUMELQ + NQVOIS,3,ALEMUSCL_Buffer%GRAD(1,1,ITRIMAT), 
     .                              NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
!$OMP END SINGLE         
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

         CALL GRADIENT_LIMITATION2(IXQ, X, TRIMAT)
      ENDDO
      CALL MY_BARRIER
 
C-----------------------------------------------  
      END SUBROUTINE ALE51_GRADIENT_RECONSTRUCTION2
      
