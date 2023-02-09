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
Chd|  ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        GRADIENT_LIMITATION2          source/ale/alemuscl/gradient_limitation2.F
Chd|        GRADIENT_RECONSTRUCTION2      source/ale/alemuscl/gradient_reconstruction2.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_ENVOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXCH_MIN_MAX             source/mpi/ale/spmd_exch_min_max.F
Chd|        ALEMUSCL_MOD                  ../common_source/modules/alemuscl_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|        TRIMAT_MOD                    share/modules/trimat.F        
Chd|====================================================================
      SUBROUTINE ALE51_GRADIENT_RECONSTRUCTION2(IPARG   ,ELBUF_TAB,IXQ     ,X       ,ALE_CONNECT,
     .                                          NV46    ,NERCVOIS ,NESDVOIS,LERCVOIS,LESDVOIS   ,LENCOM, ITASK,
     .                                          IAD_ELEM,FR_ELEM  ,SEGVAR)
C-----------------------------------------------
C   D e s c r i p t i o n
C   Computes limited gradients for volumic fractions
C   of LAW51 species
C-----------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD 
      USE ALEMUSCL_MOD
      USE TRIMAT_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l  P a r a m e t e r s
C-----------------------------------------------
#include "mvsiz_p.inc"
#include "mmale51_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr01_c.inc"
#include "scr05_c.inc"
#include "scr07_c.inc"
#include "spmd_c.inc"
#include "com01_c.inc"
#include "com04_c.inc"
#include "com08_c.inc"
#include "vect01_c.inc"
#include "param_c.inc"
#include "task_c.inc"
#include "parit_c.inc"
#include "alemuscl.inc"
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
C-----------------------------------------------
C   S o u r c e   L i n e s 
C----------------------------------------------- 
      DO NG=ITASK+1,NGROUP,NTHREAD      
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF (IPARG    ,NG      ,                  
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
               ELCENTER(II,2) = FOURTH * (X(2, IXQ(2, II)) + X(2, IXQ(3, II)) + X(2, IXQ(4, II)) + X(2, IXQ(5, II)))
               ELCENTER(II,3) = FOURTH * (X(3, IXQ(2, II)) + X(3, IXQ(3, II)) + X(3, IXQ(4, II)) + X(3, IXQ(5, II)))
            ENDDO  
         ENDIF
      ENDDO  ! NG=ITASK+1,NGROUP,NTHREAD
      DO NG=ITASK+1,NGROUP,NTHREAD      
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF (IPARG    ,NG      ,                  
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
            ADD    = N0PHAS + (ITRIMAT-1)*NVPHAS ! ADD => SIG(1)
            ADD    = ADD + 11   ! ADD + 11 => VOLUME_Phase
            K      = LLT*(ADD-1) ! VAR(I,ADD) = VAR(K+I) 
            VOLP   =>UVAR(K+1:K+LLT)
            !!!   Volume fraction
            DO I=LFT,LLT 
               II     = I+NFT    
               VOLUME_FRACTION(II,ITRIMAT) = VOLP(I)/VOLG(I)
               VOLUME_FRACTION(II,ITRIMAT) = MAX(ZERO,MIN(ONE,VOLUME_FRACTION(II,ITRIMAT))) 
            ENDDO  
         ENDDO  
      ENDDO  ! NG=ITASK+1,NGROUP,NTHREAD

      CALL MY_BARRIER
      
      !!! MPI Comm
      IF(NSPMD > 1)THEN
!$OMP SINGLE
         !!! Volumic fractions comm
         DO ITRIMAT = 1, TRIMAT
            CALL SPMD_E1VOIS(VOLUME_FRACTION(1,ITRIMAT), NERCVOIS, NESDVOIS,
     .           LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
         !!! Centroid coordinates comm
         DO JJ = 1, 3
            CALL SPMD_E1VOIS(ELCENTER(1,JJ), NERCVOIS, NESDVOIS,
     .           LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
!$OMP END SINGLE
      ENDIF
      CALL MY_BARRIER 
      
      DO NODE_ID = 1 + ITASK, NUMNOD, NTHREAD
         DO ITRIMAT = 1, TRIMAT
            NODE_MAX_VALUE(NODE_ID,ITRIMAT) = -EP30
            NODE_MIN_VALUE(NODE_ID,ITRIMAT) = EP30
            JMIN = pADDCNEL(NODE_ID)
            JMAX = pADDTMPL(NODE_ID) - 1
            DO JJ = JMIN, JMAX
               ELEM_ID = pCNEL(JJ)
               IF (ELEM_ID   /=  0 .AND. ELEM_ID  <=  NUMELQ) THEN
                  NODE_MAX_VALUE(NODE_ID,ITRIMAT) = MAX(NODE_MAX_VALUE(NODE_ID,ITRIMAT), 
     .                 VOLUME_FRACTION(ELEM_ID,ITRIMAT))
                  NODE_MIN_VALUE(NODE_ID,ITRIMAT) = MIN(NODE_MIN_VALUE(NODE_ID,ITRIMAT), 
     .                 VOLUME_FRACTION(ELEM_ID,ITRIMAT))
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      CALL MY_BARRIER 
       !!! MPI Comm
      IF(NSPMD > 1)THEN
!$OMP SINGLE
         DO ITRIMAT = 1, TRIMAT
            CALL  SPMD_EXCH_MIN_MAX(IAD_ELEM ,FR_ELEM ,NODE_MIN_VALUE(1,ITRIMAT), NODE_MAX_VALUE(1,ITRIMAT) )
         ENDDO
!$OMP END SINGLE
      ENDIF
      CALL MY_BARRIER

      DO NG=ITASK+1,NGROUP,NTHREAD  
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF (IPARG   ,NG      ,                  
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
            CALL GRADIENT_RECONSTRUCTION2(ITASK, IXQ, X, ALE_CONNECT, NV46, ITRIMAT, SEGVAR) 
         ENDDO  !  ITRIMAT = 1, TRIMAT     
      END DO  !  NG=ITASK+1,NGROUP,NTHREAD 

      CALL MY_BARRIER

      IF (NSPMD  >            1) THEN
C     MPI exchange for gradients
!$OMP SINGLE
         DO ITRIMAT = 1, TRIMAT
            CALL SPMD_ENVOIS(3, GRAD(1,1,ITRIMAT), 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         ENDDO
!$OMP END SINGLE         
      ENDIF
      CALL MY_BARRIER
      
      DO NG=ITASK+1,NGROUP,NTHREAD  
C     ALE ON / OFF
         IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
         CALL INITBUF (IPARG    ,NG      ,                  
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
      
