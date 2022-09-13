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
Chd|  MULTI_FLUXES_COMPUTATION      source/multifluid/multi_fluxes_computation.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_FLUXES_COMPUTATION(NG, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .     PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, WGRID, XGRID, ITAB, NBMAT, CURRENT_TIME, BUFMAT,
     .     ID_GLOBAL_VOIS,NPF,TF)
!$COMMENT
!       MULTI_FLUXES_COMPUTATION description :
!           computation of fluxes with 1rst order algorithm
!       MULTI_FLUXES_COMPUTATION organization :
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
      ! for parith/on : ID_GLOBAL_VOIS --> user id
      INTEGER, INTENT(IN) :: ID_GLOBAL_VOIS(*)
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
      INTEGER :: NEL, ISOLNOD, ITY, NFT
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
      INTEGER :: LOCAL_MATID(NBMAT), MATLAW(NBMAT)
      my_real :: XF(3), XK(3), XL(3)
      my_real :: massflux1, massflux2, PSHIFT

      INTEGER :: KFACE3,I_OLD,J_OLD,KFACE_OLD,KFACE2_OLD
      INTEGER :: ID_GLOBAL_VOIS_1,ID_GLOBAL_VOIS_2,IJK
      INTEGER, DIMENSION(MVSIZ) :: GLOBAL_ID_CURRENT_ELM
      my_real :: DIRECTION
      INTEGER :: IAD

      NB_FACE = -1

      GBUF => ELBUF_TAB(NG)%GBUF
      NEL = IPARG(2, NG)
      NFT = IPARG(3, NG)
      ITY = IPARG(5, NG)
      ISOLNOD = IPARG(28, NG)

      PSHIFT = MULTI_FVM%PRES_SHIFT

      IF (NBMAT > 1) THEN
!DIR$ NOVECTOR
         DO IMAT = 1, NBMAT
            LBUFS(IMAT)%LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
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
         IF (ITY == 2) THEN
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
         ELSEIF (ITY == 7) THEN
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
         NB_FACE =ALE_CONNECTIVITY%ee_connect%iad_connect(I+1) -  
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
!   -----------------------------------------------
            IF (J_OLD > 0 .AND. I_OLD < J_OLD) THEN
               ! ----------------------
               KFACE_OLD = KFACE3
               KFACE2_OLD = MULTI_FVM%FVM_CONNECTIVITY%KVOIS(NB_FACE * (I_OLD - 1) + KFACE3)

               IF(ID_GLOBAL_VOIS_1<ID_GLOBAL_VOIS_2) THEN
                    DIRECTION = ONE
                    KFACE = KFACE3
                    KFACE2 = MULTI_FVM%FVM_CONNECTIVITY%KVOIS(NB_FACE * (I_OLD - 1) + KFACE)
                    I = I_OLD
                    JJ = J_OLD
               ELSE
                    KFACE2 = KFACE3
                    KFACE = MULTI_FVM%FVM_CONNECTIVITY%KVOIS(NB_FACE * (I_OLD - 1) + KFACE3)

                    IJK = I
                    I = J_old
                    JJ = i_old
                    DIRECTION = -ONE
               ENDIF
C     Face normal
                NX = NX *DIRECTION
                NY = NY *DIRECTION
                NZ = NZ *DIRECTION

               ! ----------------------
               J = JJ
!     Conserved variable current element
               RHOII = MULTI_FVM%RHO(I)
               VXII = MULTI_FVM%VEL(1, I)
               VYII = MULTI_FVM%VEL(2, I)
               VZII = MULTI_FVM%VEL(3, I)
               VELII2 = VXII * VXII + VYII * VYII + VZII * VZII

!     Pressure current element
               PII = MULTI_FVM%PRES(I)
!     Sound speed current element
               SSPII = MULTI_FVM%SOUND_SPEED(I)


C     Densities
               RHOJJ = MULTI_FVM%RHO(J)
C     Pressures
               PJJ = MULTI_FVM%PRES(J)  
C     Normal velocity current element
               NORMAL_VELII = VXII * NX + VYII * NY + VZII * NZ

!     Normal velocity adjacent element
               VXJJ = MULTI_FVM%VEL(1, J)
               VYJJ = MULTI_FVM%VEL(2, J)
               VZJJ = MULTI_FVM%VEL(3, J)

               NORMAL_VELJJ = VXJJ * NX + VYJJ * NY + VZJJ * NZ
               VELJJ2 = VXJJ * VXJJ + VYJJ * VYJJ + VZJJ * VZJJ
C     Sound speeds adjacent element
               SSPJJ = MULTI_FVM%SOUND_SPEED(J)
C     Normal grid velocity
               NORMALW = WFAC(1) * NX + WFAC(2) * NY + WFAC(3) * NZ
               
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
C               PSTAR2 = HALF * (PSTAR + PJJ + RHOJJ * (SSTAR - NORMAL_VELJJ) * (SR - NORMAL_VELJJ))
               IF (MIN(MACHII, MACHJJ) < EM01) THEN
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

               IF (SL > NORMALW) THEN
C     Take the fluxes of cell II
C     ===
C     Global fluxes
                  VII(1) = RHOII
                  VII(2) = RHOII * VXII
                  VII(3) = RHOII * VYII
                  VII(4) = RHOII * VZII
                  VII(5) = MULTI_FVM%EINT(I) + HALF * RHOII * VELII2
C     Normal physical flux curent element
                  FII(1) = VII(1) * NORMAL_VELII
                  FII(2) = VII(2) * NORMAL_VELII + PII * NX
                  FII(3) = VII(3) * NORMAL_VELII + PII * NY
                  FII(4) = VII(4) * NORMAL_VELII + PII * NZ
                  FII(5) = (VII(5) + PII + PSHIFT) * NORMAL_VELII
                  MULTI_FVM%FLUXES(1:5, KFACE_OLD, I_OLD) = DIRECTION * (FII(1:5) - NORMALW * VII(1:5)) * SURF
                  MULTI_FVM%FLUXES(6, KFACE_OLD, I_OLD) =  DIRECTION * NORMAL_VELII * SURF
                  massflux1 = MULTI_FVM%FLUXES(1, KFACE_OLD, I_OLD)
C     ===
C     Submaterial fluxes
                  IF (NBMAT > 1) THEN
                     massflux2 = ZERO
                     DO IMAT = 1, NBMAT
                        ALPHAII = MULTI_FVM%PHASE_ALPHA(IMAT, I)
                        SUB_RHOII = MULTI_FVM%PHASE_RHO(IMAT, I)
                        SUB_RHOEINTII = MULTI_FVM%PHASE_EINT(IMAT, I)
                        SUB_VIISTAR(1) = ALPHAII
                        SUB_VIISTAR(2) = ALPHAII * SUB_RHOII
                        SUB_VIISTAR(3) = ALPHAII * SUB_RHOEINTII
                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * NORMAL_VELII
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                        massflux2 = massflux2 + MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD)
                     ENDDO
                  ENDIF                  
C     
               ELSEIF (SL <= NORMALW .AND. NORMALW <= SSTAR) THEN
C     Global fluxes
                  VII(1) = RHOII
                  VII(2) = RHOII * VXII
                  VII(3) = RHOII * VYII
                  VII(4) = RHOII * VZII
                  VII(5) = MULTI_FVM%EINT(I) + HALF * RHOII * VELII2
C     Normal physical flux curent element
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
                  massflux1 = MULTI_FVM%FLUXES(1, KFACE_OLD, I_OLD)
C     ===
C     Submaterial fluxes
                  IF (NBMAT > 1) THEN
                     massflux2 = ZERO
                     DO IMAT = 1, NBMAT
                        MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                        ALPHAII = MULTI_FVM%PHASE_ALPHA(IMAT, I)
                        SUB_RHOII = MULTI_FVM%PHASE_RHO(IMAT, I)
                        SUB_RHOEINTII = MULTI_FVM%PHASE_EINT(IMAT, I)
                        ALPHASTAR = ALPHAII
                        SUB_RHOSTAR = SUB_RHOII * (NORMAL_VELII - SL) / (SSTAR - SL)
                        SUB_PII = MULTI_FVM%PHASE_PRES(IMAT, I)
                        IF (ALPHASTAR > ZERO) THEN
c$$$                           CALL ENERGY_JUMP(MATLAW(IMAT), LOCAL_MATID(IMAT), PM, IPM, NPROPM, NPROPMI, 
c$$$     .                          SUB_RHOEINTII / SUB_RHOII, SUB_RHOSTAR, SUB_PII, (SSTAR - NORMAL_VELII) / (SSTAR - SL), 
c$$$     .                          LBUFS(IMAT)%LBUF%BFRAC(II), GBUF%TB(II), GBUF%DELTAX(II), CURRENT_TIME,
c$$$     .                          SUB_ESTAR, BUFMAT)
                              SUB_ESTAR = SUB_RHOEINTII / SUB_RHOII - 
     .                             SUB_PII * (ONE / SUB_RHOSTAR - ONE / SUB_RHOII)
                        ELSE
                           SUB_ESTAR = ZERO
                        ENDIF
                        SUB_RHOEINTII = MULTI_FVM%PHASE_EINT(IMAT, I)
                        SUB_VIISTAR(1) = ALPHASTAR
                        SUB_VIISTAR(2) = ALPHASTAR * SUB_RHOSTAR
                        SUB_VIISTAR(3) = ALPHAII * SUB_RHOSTAR * SUB_ESTAR
                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * SSTAR
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                        massflux2 = massflux2 + MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD)
                     ENDDO
                  ENDIF
               ELSEIF (SSTAR < NORMALW .AND. NORMALW <= SR) THEN
C     Global fluxes
                  VII(1) = RHOJJ
                  VII(2) = RHOJJ * VXJJ
                  VII(3) = RHOJJ * VYJJ
                  VII(4) = RHOJJ * VZJJ
                  VII(5) = MULTI_FVM%EINT(J) + HALF * RHOJJ * VELJJ2
C     Normal physical flux curent element
                  FII(1) = VII(1) * NORMAL_VELJJ
                  FII(2) = VII(2) * NORMAL_VELJJ + PJJ * NX
                  FII(3) = VII(3) * NORMAL_VELJJ + PJJ * NY
                  FII(4) = VII(4) * NORMAL_VELJJ + PJJ* NZ
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
                  massflux1 = MULTI_FVM%FLUXES(1, KFACE_OLD, I_OLD)
C     ===
C     Submaterial fluxes
                  IF (NBMAT > 1) THEN
                     massflux2 = ZERO
                     DO IMAT = 1, NBMAT
                        MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                        ALPHAII = MULTI_FVM%PHASE_ALPHA(IMAT, J)
                        SUB_RHOII = MULTI_FVM%PHASE_RHO(IMAT, J)
                        SUB_RHOEINTII = MULTI_FVM%PHASE_EINT(IMAT, J)
                        ALPHASTAR = ALPHAII
                        SUB_RHOSTAR = SUB_RHOII * (NORMAL_VELJJ - SR) / (SSTAR - SR)
                        SUB_PII = MULTI_FVM%PHASE_PRES(IMAT, J)
                        IF (ALPHASTAR > ZERO) THEN
c$$$                           CALL ENERGY_JUMP(MATLAW(IMAT), LOCAL_MATID(IMAT), PM, IPM, NPROPM, NPROPMI, 
c$$$     .                          SUB_RHOEINTII / SUB_RHOII, SUB_RHOSTAR, SUB_PII, (SSTAR - NORMAL_VELJJ) / (SSTAR - SR), 
c$$$     .                          LBUFS(IMAT)%LBUF%BFRAC(II), GBUF%TB(II), GBUF%DELTAX(II), CURRENT_TIME,
c$$$     .                          SUB_ESTAR, BUFMAT)
                           SUB_ESTAR = SUB_RHOEINTII / SUB_RHOII - 
     .                          SUB_PII * (ONE / SUB_RHOSTAR - ONE / SUB_RHOII)
                           
                        ELSE
                           SUB_ESTAR = ZERO
                        ENDIF
                        SUB_RHOEINTII = MULTI_FVM%PHASE_EINT(IMAT, J)
                        SUB_VIISTAR(1) = ALPHASTAR
                        SUB_VIISTAR(2) = ALPHASTAR * SUB_RHOSTAR
                        SUB_VIISTAR(3) = ALPHAII * SUB_RHOSTAR * SUB_ESTAR
                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * SSTAR
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                        massflux2 = massflux2 + MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD)
                     ENDDO
                  ENDIF
               ELSE IF (SR < NORMALW) THEN
C     Take the fluxes of cell JJ
C     ===
C     Global fluxes
                  VII(1) = RHOJJ
                  VII(2) = RHOJJ * VXJJ
                  VII(3) = RHOJJ * VYJJ
                  VII(4) = RHOJJ * VZJJ
                  VII(5) = MULTI_FVM%EINT(J) + HALF * RHOJJ * VELJJ2
C     Normal physical flux curent element
                  FII(1) = VII(1) * NORMAL_VELJJ
                  FII(2) = VII(2) * NORMAL_VELJJ + PJJ * NX
                  FII(3) = VII(3) * NORMAL_VELJJ + PJJ * NY
                  FII(4) = VII(4) * NORMAL_VELJJ + PJJ * NZ
                  FII(5) = (VII(5) + PJJ + PSHIFT) * NORMAL_VELJJ
                  MULTI_FVM%FLUXES(1:5, KFACE_OLD, I_OLD) = DIRECTION * (FII(1:5) - NORMALW * VII(1:5)) * SURF
                  MULTI_FVM%FLUXES(6, KFACE_OLD, I_OLD) = DIRECTION * NORMAL_VELJJ * SURF
                  massflux1 = MULTI_FVM%FLUXES(1, KFACE_OLD, I_OLD)
C     ===
C     Submaterial fluxes
                  IF (NBMAT > 1) THEN
                     massflux2 = ZERO
                     DO IMAT = 1, NBMAT
                        ALPHAII = MULTI_FVM%PHASE_ALPHA(IMAT, J)
                        SUB_RHOII = MULTI_FVM%PHASE_RHO(IMAT, J)
                        SUB_RHOEINTII = MULTI_FVM%PHASE_EINT(IMAT, J)
                        SUB_VIISTAR(1) = ALPHAII
                        SUB_VIISTAR(2) = ALPHAII * SUB_RHOII
                        SUB_VIISTAR(3) = ALPHAII * SUB_RHOEINTII
                        SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * NORMAL_VELJJ
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD) = DIRECTION *
     .                       (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
                        massflux2 = massflux2 + MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD)
                     ENDDO
                  ENDIF
               ELSE
                  CALL ARRET(2)
C
               ENDIF

               IF (J_OLD <= NUMEL_SPMD) THEN
                  MULTI_FVM%FLUXES(1:6, KFACE2_OLD, J_OLD) = -MULTI_FVM%FLUXES(1:6, KFACE_OLD, I_OLD)
                  IF (NBMAT > 1) THEN
                     DO IMAT = 1, NBMAT
                        MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE2_OLD, J_OLD) = -MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE_OLD, I_OLD)
                        MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE2_OLD, J_OLD) = -MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE_OLD, I_OLD)
                        MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE2_OLD, J_OLD) = -MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE_OLD, I_OLD)
                     ENDDO
                  ENDIF
               ELSE
                  !PRINT*, "VOISIN DOMAINE"
               ENDIF
!   -----------------------------------------------
            ELSE IF(J_OLD <= 0) THEN
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
               IF (NBMAT > 1) THEN
                  DO IMAT = 1, NBMAT
                     MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE3, I_OLD) = ZERO
                     MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE3, I_OLD) = ZERO
                     MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE3, I_OLD) = ZERO
                  ENDDO
               ENDIF
            ENDIF
!   -----------------------------------------------
         ENDDO                  !KFACE3
      ENDDO                     ! II = 1, NEL


C----------------------
C     Boundary fluxes
C----------------------
      END SUBROUTINE MULTI_FLUXES_COMPUTATION

Chd|====================================================================
Chd|  ENERGY_JUMP                   source/multifluid/multi_fluxes_computation.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MULTI_SUBMATLAW               source/multifluid/multi_submatlaw.F
Chd|====================================================================
      SUBROUTINE ENERGY_JUMP(MATLAW, LOCAL_MATID, PM, IPM, NPROPM, NPROPMI, 
     .     EINT, RHOSTAR, PRES, ALPHA, 
     .     BURNFRAC, BURNTIME, DELTAX, CURRENT_TIME,
     .     ESTAR, BUFMAT,SIG,NPF,TF)
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "tabsiz_c.inc"
      INTEGER, INTENT(IN) :: MATLAW, LOCAL_MATID, NPROPM, NPROPMI
      my_real, INTENT(IN) :: PM(NPROPM, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: EINT, PRES, ALPHA,SIG(6)
      my_real, INTENT(INOUT) :: BURNFRAC, BURNTIME, DELTAX, CURRENT_TIME, BUFMAT(*), RHOSTAR
      my_real, INTENT(OUT) :: ESTAR
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF)

      INTEGER :: ITER, MAX_ITER
      my_real :: TOL, ERROR
      my_real :: FUNC, DFUNC, PSTAR, GRUN, SSP, VOL, INCR, TEMP
      LOGICAL :: CONT
      my_real :: OFF
      
      MAX_ITER = 50
      TOL = EM06
      ERROR = ONE
      OFF = ONE
C     Dummy
      VOL = ONE

C     Initialization
      ESTAR = EINT
      TEMP = ZERO
      ITER = 0
      CONT = .TRUE.
      DO WHILE(CONT .AND. ITER < MAX_ITER) 
         ITER = ITER + 1
         CALL MULTI_SUBMATLAW(0, MATLAW, LOCAL_MATID, 1, RHOSTAR * ESTAR, PSTAR, RHOSTAR, SSP, VOL, GRUN, 
     .        PM, IPM, NPROPM, NPROPMI, BUFMAT, OFF, TEMP, 
     .        BURNFRAC, BURNTIME, DELTAX, CURRENT_TIME,SIG(1:6),NPF,TF)

         IF (GRUN == ZERO) THEN
            ERROR = ZERO
            CONT = .FALSE.
            ESTAR = EINT
         ELSE

            FUNC = ESTAR - EINT + ALPHA / RHOSTAR * HALF * (PSTAR + PRES)
            DFUNC = ONE + ALPHA * HALF * GRUN
            INCR = - FUNC / DFUNC
c$$$  IF (INCR < ZERO) THEN
c$$$  INCR = MAX(INCR, - HALF * ESTAR)
c$$$  ENDIF
            ESTAR = ESTAR + INCR
            ERROR = ABS(INCR)
            IF (ERROR < TOL * ABS(ESTAR)) THEN
               CONT = .FALSE.
            ENDIF
         ENDIF
      ENDDO
      
      IF (ERROR > TOL * ABS(ESTAR)) THEN
!         PRINT*, "ESTAR NOT FOUND"
         ESTAR = EINT
         PSTAR = PRES
      ENDIF

      END SUBROUTINE ENERGY_JUMP
