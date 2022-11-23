Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MULTI_MUSCL_FLUXES_COMPUTATIONsource/multifluid/multi_muscl_fluxes_computation.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        COMPUTE_PRESSURE              source/multifluid/multi_muscl_fluxes_computation.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_MUSCL_FLUXES_COMPUTATION(NG, ELBUF_TAB, IPARG, ITASK, 
     .     IXS, IXQ, IXTG,
     .     PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, WGRID, XGRID, ITAB, NBMAT, CURRENT_TIME, BUFMAT,
     .     ID_GLOBAL_VOIS,FACE_VOIS,NPF,TF)
!$COMMENT
!       MULTI_MUSCL_FLUXES_COMPUTATION description :
!           computation of fluxes with 2nd order algorithm
!       MULTI_MUSCL_FLUXES_COMPUTATION organization :
!           The parith/on is ensured by the same 
!           order computation :
!           * check the user ID
!           * if user ID( element1) < user ID( element2)
!             --> element1 drives the computation
!           * if user ID( element1) > user ID( element2)
!             --> element2 drives the computation
!           
!$ENDCOMMENT
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD      
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
! NUMELS      
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "mvsiz_p.inc"
!     DTFAC1
#include      "scr18_c.inc" 
!     SNPC, STF
!#include      "tabsiz_c.inc" 
      COMMON /TABLESIZF/ STF,SNPC
      INTEGER STF,SNPC
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NG
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      ! for parith/on : ID_GLOBAL_VOIS --> user id ; FACE_VOIS --> face of the remote element
      INTEGER, INTENT(IN) :: ID_GLOBAL_VOIS(*),FACE_VOIS(*)
      my_real, INTENT(IN) :: WGRID(3, *), XGRID(3, *), CURRENT_TIME
      INTEGER, INTENT(IN) :: ITAB(*), NBMAT
      my_real, INTENT(INOUT) :: BUFMAT(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF, GBUFJJ
      TYPE(L_BUFEL_), POINTER :: LBUF

      INTEGER :: II, JJ, KFACE, I, J, KFACE2, NGJJ, NFTJJ, NELJJ, IFACE
      INTEGER :: ITY, NFT, NEL
      INTEGER :: IMAT
      my_real :: X1(3), X2(3), X3(3), X4(3)
      my_real :: LAMBDAII, LAMBDAF, NORMUII, NORMUJJ
      my_real :: FII(5), SUB_FII(3), FJJ(5), NORMAL_VELII, NORMAL_VELJJ, VII(5), SUB_VII(3), VJJ(5), 
     .     VELII2, VELJJ2, SURF
      my_real :: SL, SR, SSTAR, PSTAR, FIISTAR(5), SUB_FIISTAR(3), FJJSTAR(5), VIISTAR(5), VJJSTAR(5), PP(5), 
     .     SUB_VIISTAR(3)
      my_real :: SSPII, SSPJJ, RHOII, RHOJJ, PII, PJJ, NX, NY, NZ, NORMALW
      INTEGER :: NODE1, NODE2, NODE3, NODE4
      my_real :: VXII, VYII, VZII
      my_real :: VXJJ, VYJJ, VZJJ, VX, VY, VZ, NORMVEL, SSP, RHO, P
      INTEGER :: NB_FACE, ELEMID, NUMEL_SPMD
      my_real :: MACHII, MACHJJ, PSTAR2, THETA, ALPHAII, SUB_RHOII, SUB_RHOEINTII, 
     .     ALPHASTAR, SUB_RHOSTAR, SUB_ESTAR, SUB_PII, WFAC(3)
      TYPE(LBUF_PTR) :: LBUFS(NBMAT)
      INTEGER :: LOCAL_MATID(NBMAT), MATLAW(NBMAT),IDX(6)
      my_real :: XF(3), XK(3), XL(3)
      my_real :: EINTII, EINTJJ, GRUN, TEMP
      my_real :: PHASE_RHOII(NBMAT), PHASE_PRESII(NBMAT), PHASE_EINTII(NBMAT), 
     .     PHASE_SSPII(NBMAT), PHASE_ALPHAII(NBMAT)
      my_real :: PHASE_RHOJJ(NBMAT), PHASE_PRESJJ(NBMAT), PHASE_EINTJJ(NBMAT), 
     .     PHASE_SSPJJ(NBMAT), PHASE_ALPHAJJ(NBMAT), CUMUL_ALPHA, PSHIFT

      INTEGER :: KFACE3,I_OLD,J_OLD,KFACE_OLD,KFACE2_OLD
      INTEGER :: ID_GLOBAL_VOIS_1,ID_GLOBAL_VOIS_2,IJK
      INTEGER, DIMENSION(MVSIZ) :: GLOBAL_ID_CURRENT_ELM
      INTEGER :: IAD
      my_real :: DIRECTION

      NB_FACE = -1

      GBUF => ELBUF_TAB(NG)%GBUF
      NEL = IPARG(2, NG)
      NFT = IPARG(3, NG)
      ITY = IPARG(5, NG)

      PSHIFT = MULTI_FVM%PRES_SHIFT

      DO I=1,6
        IDX(I) = NEL*(I-1)
      ENDDO

      IF (NBMAT  >  1) THEN
!DIR$ NOVECTOR
         DO IMAT = 1, NBMAT
            LBUFS(IMAT)%LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
         ENDDO
      ELSE
         DO IMAT = 1, NBMAT
            NULLIFY( LBUFS(IMAT)%LBUF )
         ENDDO
      ENDIF
C     Normal and surface computation
      SELECT CASE (MULTI_FVM%SYM)
      CASE (0)
         NB_FACE = 6
C     3D
         NUMEL_SPMD = NUMELS
         DO IMAT = 1, NBMAT
            LOCAL_MATID(IMAT) = IPM(20 + IMAT, IXS(1, 1 + NFT))
         ENDDO
         DO II = 1, NEL
            I = II + NFT
            GLOBAL_ID_CURRENT_ELM(II) = IXS(NIXS,I)
         ENDDO
      CASE (1, 2)
         IF (ITY  ==  2) THEN
C     QUADS
            NB_FACE = 4
            NUMEL_SPMD = NUMELQ
            DO IMAT = 1, NBMAT
               LOCAL_MATID(IMAT) = IPM(20 + IMAT, IXQ(1, 1 + NFT))
            ENDDO
            DO II = 1, NEL
                I = II + NFT
                GLOBAL_ID_CURRENT_ELM(II) = IXQ(NIXQ,I)
            ENDDO
         ELSEIF (ITY  ==  7) THEN
C     TRIANGLES
            NB_FACE = 3
            NUMEL_SPMD = NUMELTG
            DO IMAT = 1, NBMAT
               LOCAL_MATID(IMAT) = IPM(20 + IMAT, IXTG(1, 1 + NFT))
            ENDDO
            DO II = 1, NEL
                I = II + NFT
                GLOBAL_ID_CURRENT_ELM(II) = IXTG(NIXTG,I)
            ENDDO
         ENDIF
      CASE DEFAULT
         CALL ARRET(2)
      END SELECT

C     Flux computation
      DO II = 1, NEL
         I = II + NFT
         IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
         NB_FACE = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1) - 
     .        ALE_CONNECTIVITY%ee_connect%iad_connect(I)
         I_OLD = I
C     Face KFACE
         DO KFACE3 = 1, NB_FACE
            JJ = ALE_CONNECTIVITY%ee_connect%connected(IAD + KFACE3 - 1)
            J_OLD = JJ
C     Face normal
            NX = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE3, I_OLD)
            NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE3, I_OLD)
            NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE3, I_OLD)
            SURF = MULTI_FVM%FACE_DATA%SURF(KFACE3, I_OLD)
            WFAC(1:3) = MULTI_FVM%FACE_DATA%WFAC(1:3, KFACE3, I_OLD)

            ID_GLOBAL_VOIS_1 = GLOBAL_ID_CURRENT_ELM(II)            
            ID_GLOBAL_VOIS_2 = ID_GLOBAL_VOIS( NB_FACE * (I_OLD - 1) + KFACE3 )
            KFACE_OLD = KFACE3
            IF( J_OLD>0 ) THEN
                IF (J_OLD  <=  NUMEL_SPMD) THEN
                    KFACE2_OLD = MULTI_FVM%FVM_CONNECTIVITY%KVOIS(NB_FACE * (I_OLD - 1) + KFACE3)
                ELSE
                    KFACE2_OLD = FACE_VOIS( NB_FACE * (I_OLD - 1) + KFACE3 )
                ENDIF
            ENDIF
            
!   -----------------------------------------------
            IF (J_OLD  >  0 .AND. I_OLD  <  J_OLD) THEN
               ! ----------------------

               IF(ID_GLOBAL_VOIS_1<ID_GLOBAL_VOIS_2) THEN
                    DIRECTION = ONE
                    KFACE = KFACE_OLD
                    KFACE2 = KFACE2_OLD
                    I = I_OLD
                    JJ = J_OLD
               ELSE
                    DIRECTION = -ONE
                    KFACE2 = KFACE_OLD
                    KFACE = KFACE2_OLD
                    I = J_OLD
                    JJ = I_OLD
               ENDIF
C     Face normal
                NX = NX *DIRECTION
                NY = NY *DIRECTION
                NZ = NZ *DIRECTION

                NORMALW = WFAC(1) * NX + WFAC(2) * NY + WFAC(3) * NZ
               ! ----------------------

               J = JJ
!     Conserved variable current element
C     ============
C     MUSCL
C     ============
               XK(1:3) = MULTI_FVM%ELEM_DATA%CENTROID(1:3, I)
               XL(1:3) = MULTI_FVM%ELEM_DATA%CENTROID(1:3, J)
               XF(1:3) = MULTI_FVM%FACE_DATA%CENTROID(1:3, KFACE, I)    ! <-- need to commnicate this value
    
C     Reconstructed velocity current element
               VXII = MULTI_FVM%VEL(1, I)
               VYII = MULTI_FVM%VEL(2, I)
               VZII = MULTI_FVM%VEL(3, I)

               IF (MULTI_FVM%MUSCL  ==  1) THEN
                  VXII = VXII + MULTI_FVM%GRAD_U(1, I) * (XF(1) - XK(1))
     .                 + MULTI_FVM%GRAD_U(2, I) * (XF(2) - XK(2))
     .                 + MULTI_FVM%GRAD_U(3, I) * (XF(3) - XK(3))
                  VYII =  VYII + MULTI_FVM%GRAD_V(1, I) * (XF(1) - XK(1))
     .                 + MULTI_FVM%GRAD_V(2, I) * (XF(2) - XK(2))
     .                 + MULTI_FVM%GRAD_V(3, I) * (XF(3) - XK(3))
                  VZII = VZII + MULTI_FVM%GRAD_W(1, I) * (XF(1) - XK(1))
     .                 + MULTI_FVM%GRAD_W(2, I) * (XF(2) - XK(2))
     .                 + MULTI_FVM%GRAD_W(3, I) * (XF(3) - XK(3))
               ENDIF

C     Squared velocity
               VELII2 = VXII * VXII + VYII * VYII + VZII * VZII
C     Normal velocity current element
               NORMAL_VELII = VXII * NX + VYII * NY + VZII * NZ               
C     Reconstructed internal energy current element
               IF (MULTI_FVM%NBMAT  ==  1) THEN
C     Reconstructed density current element
                  RHOII = MULTI_FVM%RHO(I)
                  EINTII = MULTI_FVM%EINT(I) 

                  IF (MULTI_FVM%MUSCL  ==  1) THEN
                     RHOII = RHOII + MULTI_FVM%GRAD_RHO(1, I) * (XF(1) - XK(1))
     .                    + MULTI_FVM%GRAD_RHO(2, I) * (XF(2) - XK(2))
     .                    + MULTI_FVM%GRAD_RHO(3, I) * (XF(3) - XK(3))       
                     EINTII = EINTII + MULTI_FVM%GRAD_PRES(1, I) * (XF(1) - XK(1))
     .                    + MULTI_FVM%GRAD_PRES(2, I) * (XF(2) - XK(2))
     .                    + MULTI_FVM%GRAD_PRES(3, I) * (XF(3) - XK(3))
                  ENDIF
                  MATLAW(1) = IPM(2, LOCAL_MATID(1))
                  CALL COMPUTE_PRESSURE(MATLAW(1), LOCAL_MATID(1), PM, IPM, NPROPM, NPROPMI, 
     .                 EINTII, RHOII, PII, SSPII,
     .                 MULTI_FVM%BFRAC(1, I), GBUF%TB(II), GBUF%DELTAX(II), CURRENT_TIME,
     .                 BUFMAT, GBUF%OFF(II), GBUF%SIG(II+IDX(1):II+IDX(6)), NPF, TF)
                  SSPII = SQRT(SSPII)
                 
               ELSE
                  RHOII = ZERO
                  PII = ZERO
                  !EINTII = MULTI_FVM%EINT(I)
                  SSPII = ZERO
                  EINTII = ZERO
                  CUMUL_ALPHA = ZERO
                  DO IMAT = 1, NBMAT
                     PHASE_ALPHAII(IMAT) = MULTI_FVM%PHASE_ALPHA(IMAT, I)
     .                    + MULTI_FVM%PHASE_GRAD_ALPHA(1, IMAT, I) * (XF(1) - XK(1))
     .                    + MULTI_FVM%PHASE_GRAD_ALPHA(2, IMAT, I) * (XF(2) - XK(2))
     .                    + MULTI_FVM%PHASE_GRAD_ALPHA(3, IMAT, I) * (XF(3) - XK(3))
                     PHASE_ALPHAII(IMAT) = MAX (PHASE_ALPHAII(IMAT), ZERO)
                     CUMUL_ALPHA = CUMUL_ALPHA + PHASE_ALPHAII(IMAT)
                  ENDDO
                  
                  DO IMAT = 1, NBMAT
                     PHASE_ALPHAII(IMAT) = PHASE_ALPHAII(IMAT) / CUMUL_ALPHA
                     PHASE_RHOII(IMAT) = MULTI_FVM%PHASE_RHO(IMAT, I)
                     PHASE_EINTII(IMAT) = MULTI_FVM%PHASE_EINT(IMAT, I)
                     IF (MULTI_FVM%MUSCL  ==  1) THEN
                        PHASE_RHOII(IMAT) = PHASE_RHOII(IMAT) 
     .                       + MULTI_FVM%PHASE_GRAD_RHO(1, IMAT, I) * (XF(1) - XK(1))
     .                       + MULTI_FVM%PHASE_GRAD_RHO(2, IMAT, I) * (XF(2) - XK(2))
     .                       + MULTI_FVM%PHASE_GRAD_RHO(3, IMAT, I) * (XF(3) - XK(3))
                        PHASE_EINTII(IMAT) = PHASE_EINTII(IMAT)
     .                       + MULTI_FVM%PHASE_GRAD_PRES(1, IMAT, I) * (XF(1) - XK(1))
     .                       + MULTI_FVM%PHASE_GRAD_PRES(2, IMAT, I) * (XF(2) - XK(2))
     .                       + MULTI_FVM%PHASE_GRAD_PRES(3, IMAT, I) * (XF(3) - XK(3))
                     ENDIF
C     Global reconstructed density
                     RHOII = RHOII + PHASE_ALPHAII(IMAT) * PHASE_RHOII(IMAT)
                     MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                     IF (PHASE_ALPHAII(IMAT)  >  ZERO) THEN
                        
                        CALL COMPUTE_PRESSURE(MATLAW(IMAT), LOCAL_MATID(IMAT), PM, IPM, NPROPM, NPROPMI, 
     .                       PHASE_EINTII(IMAT), PHASE_RHOII(IMAT), PHASE_PRESII(IMAT), PHASE_SSPII(IMAT),
     .                       MULTI_FVM%BFRAC(IMAT, I), GBUF%TB(II), GBUF%DELTAX(II), CURRENT_TIME,
     .                       BUFMAT, LBUFS(IMAT)%LBUF%OFF(II),LBUFS(IMAT)%LBUF%SIG(II+IDX(1):II+IDX(6)), NPF,TF)
C     Global reconstructed pressure
                        PII = PII + PHASE_ALPHAII(IMAT) * PHASE_PRESII(IMAT)
                        EINTII = EINTII + PHASE_ALPHAII(IMAT) * PHASE_EINTII(IMAT)
                        SSPII = SSPII + PHASE_ALPHAII(IMAT) * PHASE_RHOII(IMAT) * 
     .                       MAX(EM20, PHASE_SSPII(IMAT))
                     ENDIF
                  ENDDO
                  IF (SSPII / RHOII  >  ZERO) THEN
                     SSPII = SQRT(SSPII / RHOII)
                  ELSE
                     SSPII = MULTI_FVM%SOUND_SPEED(I)
                  ENDIF
               ENDIF

C     Reconstructed velocity adjacent element
               VXJJ = MULTI_FVM%VEL(1, J)  
               VYJJ = MULTI_FVM%VEL(2, J)
               VZJJ = MULTI_FVM%VEL(3, J) 
               IF (MULTI_FVM%MUSCL  ==  1) THEN
                  VXJJ = VXJJ
     .                 + MULTI_FVM%GRAD_U(1, J) * (XF(1) - XL(1))
     .                 + MULTI_FVM%GRAD_U(2, J) * (XF(2) - XL(2))
     .                 + MULTI_FVM%GRAD_U(3, J) * (XF(3) - XL(3))
                  VYJJ = VYJJ
     .                 + MULTI_FVM%GRAD_V(1, J) * (XF(1) - XL(1))
     .                 + MULTI_FVM%GRAD_V(2, J) * (XF(2) - XL(2))
     .                 + MULTI_FVM%GRAD_V(3, J) * (XF(3) - XL(3))
                  VZJJ = VZJJ
     .                 + MULTI_FVM%GRAD_W(1, J) * (XF(1) - XL(1))
     .                 + MULTI_FVM%GRAD_W(2, J) * (XF(2) - XL(2))
     .                 + MULTI_FVM%GRAD_W(3, J) * (XF(3) - XL(3))
               ENDIF
C     Normal velocity adjacent element
               NORMAL_VELJJ = VXJJ * NX + VYJJ * NY + VZJJ * NZ
               VELJJ2 = VXJJ * VXJJ + VYJJ * VYJJ + VZJJ * VZJJ                
                    
C     Reconstructed internal energy adjacent element
               IF (MULTI_FVM%NBMAT  ==  1) THEN
C     Reconstructed density adjacent element
                  RHOJJ = MULTI_FVM%RHO(J)
                  EINTJJ = MULTI_FVM%EINT(J) 
                  IF (MULTI_FVM%MUSCL  ==  1) THEN
                     RHOJJ = RHOJJ
     .                    + MULTI_FVM%GRAD_RHO(1, J) * (XF(1) - XL(1))
     .                    + MULTI_FVM%GRAD_RHO(2, J) * (XF(2) - XL(2))
     .                    + MULTI_FVM%GRAD_RHO(3, J) * (XF(3) - XL(3))
                     EINTJJ = EINTJJ
     .                    + MULTI_FVM%GRAD_PRES(1, J) * (XF(1) - XL(1))
     .                    + MULTI_FVM%GRAD_PRES(2, J) * (XF(2) - XL(2))
     .                    + MULTI_FVM%GRAD_PRES(3, J) * (XF(3) - XL(3))
                  ENDIF
                  MATLAW(1) = IPM(2, LOCAL_MATID(1))
                  CALL COMPUTE_PRESSURE(MATLAW(1), LOCAL_MATID(1), PM, IPM, NPROPM, NPROPMI, 
     .                 EINTJJ, RHOJJ, PJJ, SSPJJ,
     .                 MULTI_FVM%BFRAC(1, J), GBUF%TB(II), GBUF%DELTAX(II), CURRENT_TIME,
     .                 BUFMAT, GBUF%OFF(II),GBUF%SIG(II+IDX(1):II+IDX(6)), NPF,TF)
                  SSPJJ = SQRT(SSPJJ)
               ELSE
                  RHOJJ = ZERO
                  PJJ = ZERO
                  EINTJJ = ZERO
                  SSPJJ = ZERO
                  CUMUL_ALPHA = ZERO
                  DO IMAT = 1, NBMAT
                     PHASE_ALPHAJJ(IMAT) = MULTI_FVM%PHASE_ALPHA(IMAT, J)
     .                    + MULTI_FVM%PHASE_GRAD_ALPHA(1, IMAT, J) * (XF(1) - XL(1))
     .                    + MULTI_FVM%PHASE_GRAD_ALPHA(2, IMAT, J) * (XF(2) - XL(2))
     .                    + MULTI_FVM%PHASE_GRAD_ALPHA(3, IMAT, J) * (XF(3) - XL(3))
                     PHASE_ALPHAJJ(IMAT) = MAX (PHASE_ALPHAJJ(IMAT), ZERO)
                     CUMUL_ALPHA = CUMUL_ALPHA + PHASE_ALPHAJJ(IMAT)
                  ENDDO
                  
                  DO IMAT = 1, NBMAT
                     PHASE_ALPHAJJ(IMAT) = PHASE_ALPHAJJ(IMAT) / CUMUL_ALPHA
                     PHASE_RHOJJ(IMAT) = MULTI_FVM%PHASE_RHO(IMAT, J)
                     PHASE_EINTJJ(IMAT) = MULTI_FVM%PHASE_EINT(IMAT, J)
                     IF (MULTI_FVM%MUSCL  ==  1) THEN
                        PHASE_RHOJJ(IMAT) = PHASE_RHOJJ(IMAT)
     .                       + MULTI_FVM%PHASE_GRAD_RHO(1, IMAT, J) * (XF(1) - XL(1))
     .                       + MULTI_FVM%PHASE_GRAD_RHO(2, IMAT, J) * (XF(2) - XL(2))
     .                       + MULTI_FVM%PHASE_GRAD_RHO(3, IMAT, J) * (XF(3) - XL(3))
                        PHASE_EINTJJ(IMAT) = PHASE_EINTJJ(IMAT)
     .                       + MULTI_FVM%PHASE_GRAD_PRES(1, IMAT, J) * (XF(1) - XL(1))
     .                       + MULTI_FVM%PHASE_GRAD_PRES(2, IMAT, J) * (XF(2) - XL(2))
     .                       + MULTI_FVM%PHASE_GRAD_PRES(3, IMAT, J) * (XF(3) - XL(3))
                     ENDIF
C     Global reconstructed density
                     RHOJJ = RHOJJ + PHASE_ALPHAJJ(IMAT) * PHASE_RHOJJ(IMAT)
                     MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                     IF (PHASE_ALPHAJJ(IMAT)  >  ZERO) THEN
                        
                        CALL COMPUTE_PRESSURE(MATLAW(IMAT), LOCAL_MATID(IMAT), PM, IPM, NPROPM, NPROPMI, 
     .                       PHASE_EINTJJ(IMAT), PHASE_RHOJJ(IMAT), PHASE_PRESJJ(IMAT), PHASE_SSPJJ(IMAT),
     .                       MULTI_FVM%BFRAC(IMAT, J), GBUF%TB(II), GBUF%DELTAX(II), CURRENT_TIME,
     .                       BUFMAT, LBUFS(IMAT)%LBUF%OFF(II),LBUFS(IMAT)%LBUF%SIG(II+IDX(1):II+IDX(6)),NPF,TF)
C     Global reconstructed pressure
                        PJJ = PJJ + PHASE_ALPHAJJ(IMAT) * PHASE_PRESJJ(IMAT)
                        EINTJJ = EINTJJ + PHASE_ALPHAJJ(IMAT) * PHASE_EINTJJ(IMAT)
                        SSPJJ = SSPJJ + PHASE_ALPHAJJ(IMAT) * PHASE_RHOJJ(IMAT) * 
     .                       MAX(EM20, PHASE_SSPJJ(IMAT))
                     ENDIF
                  ENDDO
                  IF (SSPJJ / RHOJJ  >  ZERO) THEN
                     SSPJJ = SQRT(SSPJJ / RHOJJ)
                  ELSE
                     SSPJJ = MULTI_FVM%SOUND_SPEED(J)
                  ENDIF
               ENDIF           
C     Local Mach numbers
               MACHII = ABS(NORMAL_VELII) / SSPII
               MACHJJ = ABS(NORMAL_VELJJ) / SSPJJ               

C     HLL wave speed estimates
               SL = MIN(NORMAL_VELII - SSPII, NORMAL_VELJJ - SSPJJ)
               SR = MAX(NORMAL_VELII + SSPII, NORMAL_VELJJ + SSPJJ)

C     Intermediate wave speed
               SSTAR = PJJ - PII + RHOII * NORMAL_VELII * (SL - NORMAL_VELII) - 
     .              RHOJJ * NORMAL_VELJJ * (SR - NORMAL_VELJJ)
               SSTAR = SSTAR / (RHOII * (SL - NORMAL_VELII) - 
     .              RHOJJ * (SR - NORMAL_VELJJ))
C     Specific for Low Mach number corrections
C     Intermediate pressure
               PSTAR2 = PII + RHOII * (SSTAR - NORMAL_VELII) * (SL - NORMAL_VELII)
               IF (MIN(MACHII, MACHJJ)  <  EM01) THEN
                  THETA = MIN(MACHII, MACHJJ)
               ELSE
                  THETA = ONE
               ENDIF
               PSTAR = (ONE - THETA) * HALF * (PII + PJJ) + THETA * PSTAR2
               IF (MULTI_FVM%LOWMACH_OPT) THEN
                  PP(1) = ZERO
                  PP(2) = PSTAR * NX
                  PP(3) = PSTAR * NY
                  PP(4) = PSTAR * NZ   
                  PP(5) = SSTAR * (PSTAR + PSHIFT)
               ELSE
                  PP(1) = ZERO
                  PP(2) = PSTAR2 * NX
                  PP(3) = PSTAR2 * NY
                  PP(4) = PSTAR2 * NZ
                  PP(5) = SSTAR * (PSTAR2 + PSHIFT)
               ENDIF

               IF (SL  >  NORMALW) THEN
                  VII(1) = RHOII
                  VII(2) = RHOII * VXII
                  VII(3) = RHOII * VYII
                  VII(4) = RHOII * VZII
                  VII(5) = EINTII + HALF * RHOII * VELII2
!     Normal physical flux current element
                  FII(1) = VII(1) * NORMAL_VELII
                  FII(2) = VII(2) * NORMAL_VELII + PII * NX
                  FII(3) = VII(3) * NORMAL_VELII + PII * NY
                  FII(4) = VII(4) * NORMAL_VELII + PII * NZ
                  FII(5) = (VII(5) + PII + PSHIFT) * NORMAL_VELII
C     Take the fluxes of cell II
C     ===
C     Global fluxes
                  MULTI_FVM%FLUXES(1:5, KFACE_OLD, I_OLD) = DIRECTION * (FII(1:5) - NORMALW * VII(1:5)) * SURF
                  MULTI_FVM%FLUXES(6, KFACE_OLD, I_OLD) = DIRECTION *  NORMAL_VELII * SURF
C     ===
C     Submaterial fluxes
                  IF (NBMAT  >  1) THEN
                     DO IMAT = 1, NBMAT
                        ALPHAII = PHASE_ALPHAII(IMAT)
                        SUB_RHOII = PHASE_RHOII(IMAT) ! ALPHA_RHO
                        SUB_RHOEINTII = PHASE_EINTII(IMAT)
                        SUB_VIISTAR(1) = ALPHAII
                        SUB_VIISTAR(2) = ALPHAII * SUB_RHOII ! ALPHA_RHO
                        SUB_VIISTAR(3) = ALPHAII * SUB_RHOEINTII
                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * NORMAL_VELII
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                     ENDDO
                  ENDIF
C     
               ELSEIF (SL  <=  NORMALW .AND. NORMALW  <=  SSTAR) THEN
                  VII(1) = RHOII
                  VII(2) = RHOII * VXII
                  VII(3) = RHOII * VYII
                  VII(4) = RHOII * VZII
                  VII(5) = EINTII + HALF * RHOII * VELII2
!     Normal physical flux current element
                  FII(1) = VII(1) * NORMAL_VELII
                  FII(2) = VII(2) * NORMAL_VELII + PII * NX
                  FII(3) = VII(3) * NORMAL_VELII + PII * NY
                  FII(4) = VII(4) * NORMAL_VELII + PII * NZ
                  FII(5) = (VII(5) + PII + PSHIFT) * NORMAL_VELII
C     Take intermediate state flux (HLLC scheme)
C     ===
C     Global fluxes
                  VIISTAR(1:5) = FII(1:5) - (SL) * VII(1:5) - PP(1:5)
                  VIISTAR(1:5) = VIISTAR(1:5) / (SSTAR - SL)
                  FIISTAR(1:5) = VIISTAR(1:5) * SSTAR + PP(1:5) 
                  MULTI_FVM%FLUXES(1:5, KFACE_OLD, I_OLD) = DIRECTION * 
     .                 (FIISTAR(1:5) - NORMALW * VIISTAR(1:5)) * SURF
                  MULTI_FVM%FLUXES(6, KFACE_OLD, I_OLD) = DIRECTION * SSTAR * SURF
C     ===
C     Submaterial fluxes
                  IF (NBMAT  >  1) THEN
                     DO IMAT = 1, NBMAT
                        MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                        ALPHASTAR = PHASE_ALPHAII(IMAT)
                        SUB_RHOSTAR = PHASE_RHOII(IMAT) * (NORMAL_VELII - SL) / (SSTAR - SL)
                        IF (ALPHASTAR  >  ZERO) THEN
                           SUB_RHOII = PHASE_RHOII(IMAT)
                           SUB_RHOEINTII = PHASE_EINTII(IMAT)
                           SUB_PII = PHASE_PRESII(IMAT)
                           SUB_ESTAR = PHASE_EINTII(IMAT) / PHASE_RHOII(IMAT) - 
     .                          PHASE_PRESII(IMAT) * (ONE / SUB_RHOSTAR - ONE / PHASE_RHOII(IMAT)) 

                           IF (SUB_ESTAR  <  ZERO) THEN
                              SUB_ESTAR = ZERO
                           ENDIF
                        ELSE
                           SUB_ESTAR = ZERO
                        ENDIF
                        SUB_VIISTAR(1) = ALPHASTAR
                        SUB_VIISTAR(2) = ALPHASTAR * SUB_RHOSTAR
                        SUB_VIISTAR(3) = ALPHASTAR * SUB_RHOSTAR * SUB_ESTAR

                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * SSTAR
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                     ENDDO
                  ENDIF
                
               ELSE IF (SSTAR  <  NORMALW .AND. NORMALW  <=  SR) THEN
                  VII(1) = RHOJJ
                  VII(2) = RHOJJ * VXJJ
                  VII(3) = RHOJJ * VYJJ
                  VII(4) = RHOJJ * VZJJ
                  VII(5) = EINTJJ + HALF * RHOJJ * VELJJ2
!     Normal physical flux current element
                  FII(1) = VII(1) * NORMAL_VELJJ
                  FII(2) = VII(2) * NORMAL_VELJJ + PJJ * NX
                  FII(3) = VII(3) * NORMAL_VELJJ + PJJ * NY
                  FII(4) = VII(4) * NORMAL_VELJJ + PJJ * NZ
                  FII(5) = (VII(5) + PJJ + PSHIFT) * NORMAL_VELJJ
C     Take intermediate state flux (HLLC scheme)
C     ===
C     Global fluxes
                  VIISTAR(1:5) = FII(1:5) - (SR) * VII(1:5) - PP(1:5)
                  VIISTAR(1:5) = VIISTAR(1:5) / (SSTAR - SR)
                  FIISTAR(1:5) = VIISTAR(1:5) * SSTAR + PP(1:5) 
                  MULTI_FVM%FLUXES(1:5, KFACE_OLD, I_OLD) = DIRECTION * 
     .                 (FIISTAR(1:5) - NORMALW * VIISTAR(1:5)) * SURF
                  MULTI_FVM%FLUXES(6, KFACE_OLD, I_OLD) = DIRECTION * SSTAR * SURF
C     ===
C     Submaterial fluxes
                  IF (NBMAT  >  1) THEN
                     DO IMAT = 1, NBMAT
                        MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                        ALPHASTAR = PHASE_ALPHAJJ(IMAT)
                        SUB_RHOSTAR = PHASE_RHOJJ(IMAT) * (NORMAL_VELJJ - SR) / (SSTAR - SR)
                        IF (ALPHASTAR  >  ZERO) THEN
                           SUB_RHOII = PHASE_RHOJJ(IMAT)
                           SUB_RHOEINTII = PHASE_EINTJJ(IMAT)
                           SUB_PII = PHASE_PRESJJ(IMAT)
                           SUB_ESTAR = PHASE_EINTJJ(IMAT) / PHASE_RHOJJ(IMAT) - 
     .                          PHASE_PRESJJ(IMAT) * (ONE / SUB_RHOSTAR - ONE / PHASE_RHOJJ(IMAT)) 
                           IF (SUB_ESTAR  <  ZERO) THEN
                              SUB_ESTAR = ZERO
                           ENDIF
                        ELSE
                           SUB_ESTAR = ZERO
                        ENDIF

                        SUB_VIISTAR(1) = PHASE_ALPHAJJ(IMAT)
                        SUB_VIISTAR(2) = PHASE_ALPHAJJ(IMAT) * SUB_RHOSTAR
                        SUB_VIISTAR(3) = PHASE_ALPHAJJ(IMAT) * SUB_RHOSTAR * SUB_ESTAR
                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * SSTAR
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                     ENDDO
                  ENDIF
               ELSE IF (SR  <  NORMALW) THEN
                  VII(1) = RHOJJ
                  VII(2) = RHOJJ * VXJJ
                  VII(3) = RHOJJ * VYJJ
                  VII(4) = RHOJJ * VZJJ
                  VII(5) = EINTJJ + HALF * RHOJJ * VELJJ2
!     Normal physical flux current element
                  FII(1) = VII(1) * NORMAL_VELJJ
                  FII(2) = VII(2) * NORMAL_VELJJ + PJJ * NX
                  FII(3) = VII(3) * NORMAL_VELJJ + PJJ * NY
                  FII(4) = VII(4) * NORMAL_VELJJ + PJJ * NZ
                  FII(5) = (VII(5) + PJJ + PSHIFT) * NORMAL_VELJJ
C     Take the fluxes of cell II
C     ===
C     Global fluxes
                  MULTI_FVM%FLUXES(1:5, KFACE_OLD, I_OLD) = DIRECTION * (FII(1:5) - NORMALW * VII(1:5)) * SURF
                  MULTI_FVM%FLUXES(6, KFACE_OLD, I_OLD) = DIRECTION * NORMAL_VELJJ * SURF
C     ===
C     Submaterial fluxes
                  IF (NBMAT  >  1) THEN
                     DO IMAT = 1, NBMAT
                        ALPHAII = PHASE_ALPHAJJ(IMAT)
                        SUB_RHOII = PHASE_RHOJJ(IMAT) ! ALPHA_RHO
                        SUB_RHOEINTII = PHASE_EINTJJ(IMAT)
                        SUB_VIISTAR(1) = ALPHAII
                        SUB_VIISTAR(2) = ALPHAII * SUB_RHOII ! ALPHA_RHO
                        SUB_VIISTAR(3) = ALPHAII * SUB_RHOEINTII
                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * NORMAL_VELJJ
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION * 
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                     ENDDO
                  ENDIF     
               ELSE
                  PRINT*, "SSPII", SSPII, "SSPJJ", SSPJJ, "NORMALVEL_II", NORMAL_VELII, 
     .                 "NORMALVEL_JJ", NORMAL_VELJJ
                  PRINT*, "SOUNDSPEEDII", MULTI_FVM%SOUND_SPEED(I), "SOUNDSPEEDJJ", MULTI_FVM%SOUND_SPEED(J)
                  CALL ARRET(2)
C
               ENDIF

               IF (J_OLD  <=  NUMEL_SPMD) THEN
!                  KFACE2_OLD = MULTI_FVM%FVM_CONNECTIVITY%KVOIS(NB_FACE * (I_OLD - 1) + KFACE3)
                  MULTI_FVM%FLUXES(1:6, KFACE2_OLD, J_OLD) = -MULTI_FVM%FLUXES(1:6, KFACE_OLD, I_OLD)
                  IF (NBMAT  >  1) THEN
                     DO IMAT = 1, NBMAT
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE2_OLD, J_OLD) = -MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD)
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE2_OLD, J_OLD) = -MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD)
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE2_OLD, J_OLD) = -MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD)
                     ENDDO
                  ENDIF
               ENDIF
!   -----------------------------------------------
            ELSE IF (J_OLD  <=  0) THEN
               ELEMID = I_OLD
               VX = MULTI_FVM%VEL(1, ELEMID)
               VY = MULTI_FVM%VEL(2, ELEMID)
               VZ = MULTI_FVM%VEL(3, ELEMID)
               NORMVEL = VX * NX + VY * NY + VZ * NZ
               NORMALW = WFAC(1) * NX + WFAC(2) * NY + WFAC(3) * NZ
               SSP = MULTI_FVM%SOUND_SPEED(ELEMID)
               RHO = MULTI_FVM%RHO(ELEMID)
C     HLL wave speed estimates
               SL = MIN(NORMVEL - SSP, TWO * NORMALW - NORMVEL - SSP)
               SR = MAX(NORMVEL + SSP, TWO * NORMALW - NORMVEL + SSP)

C     Intermediate wave speed
               SSTAR = NORMALW

               P = MULTI_FVM%PRES(ELEMID)
               PSTAR = P + RHO * (SSTAR - NORMVEL) * (SL - NORMVEL)

               PP(1) = ZERO
               PP(2) = PSTAR * NX
               PP(3) = PSTAR * NY
               PP(4) = PSTAR * NZ
               PP(5) = SSTAR * (PSTAR + PSHIFT)
               MULTI_FVM%FLUXES(1:5, KFACE3, ELEMID) = SURF * PP(1:5)
               MULTI_FVM%FLUXES(6, KFACE3, ELEMID) = SURF * NORMALW
C     ===
C     Submaterial fluxes
               IF (NBMAT  >  1) THEN
                  DO IMAT = 1, NBMAT
                     MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE3, I_OLD) = ZERO
                     MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE3, I_OLD) = ZERO
                     MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE3, I_OLD) = ZERO
                  ENDDO
               ENDIF
            ENDIF
!   -----------------------------------------------
         ENDDO                  !KFACE
      ENDDO                     ! II = 1, NEL


C----------------------
C     Boundary fluxes
C----------------------
      END SUBROUTINE MULTI_MUSCL_FLUXES_COMPUTATION

Chd|====================================================================
Chd|  COMPUTE_PRESSURE              source/multifluid/multi_muscl_fluxes_computation.F
Chd|-- called by -----------
Chd|        MULTI_MUSCL_FLUXES_COMPUTATIONsource/multifluid/multi_muscl_fluxes_computation.F
Chd|-- calls ---------------
Chd|        MULTI_SUBMATLAW               source/multifluid/multi_submatlaw.F
Chd|====================================================================
      SUBROUTINE COMPUTE_PRESSURE(MATLAW, LOCAL_MATID, PM, IPM, NPROPM, NPROPMI, 
     .     EINT, RHO, PRES, SSP, 
     .     BURNFRAC, BURNTIME, DELTAX, CURRENT_TIME,
     .     BUFMAT, OFF, SIG, NPF, TF)
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "tabsiz_c.inc"
      INTEGER, INTENT(IN) :: MATLAW, LOCAL_MATID, NPROPM, NPROPMI
      my_real, INTENT(IN) :: PM(NPROPM, *), CURRENT_TIME
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: SIG(6)
      my_real, INTENT(INOUT) :: SSP, PRES
      my_real, INTENT(INOUT) :: BURNFRAC, BURNTIME, DELTAX, BUFMAT(*)
      my_real, INTENT(OUT) :: OFF
      my_real, INTENT(INOUT) :: EINT, RHO
      INTEGER,INTENT(IN)::NPF(SNPC)
      my_real,INTENT(IN)::TF(STF)

      INTEGER :: ITER, MAX_ITER
      my_real :: TOL, ERROR
      my_real :: FUNC, DFUNC, PSTAR, GRUN, VOL, INCR, TEMP
      LOGICAL :: CONT
      
      MAX_ITER = 50
      TOL = EM06
      ERROR = ONE
      
C     Dummy
      VOL = ONE

C     Initialization
      TEMP = ZERO
      ITER = 0
      CONT = .TRUE.
      CALL MULTI_SUBMATLAW(0, MATLAW, LOCAL_MATID, 1, EINT, PRES, RHO, SSP, VOL, GRUN, 
     .     PM, IPM, NPROPM, NPROPMI, BUFMAT, OFF, TEMP, 
     .     BURNFRAC, BURNTIME, DELTAX, CURRENT_TIME,SIG(1:6),NPF,TF)
      END SUBROUTINE COMPUTE_PRESSURE
