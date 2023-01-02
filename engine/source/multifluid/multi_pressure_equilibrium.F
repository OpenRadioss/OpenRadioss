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
Chd|  MULTI_PRESSURE_EQUILIBRIUM    source/multifluid/multi_pressure_equilibrium.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        MULTI_SUBMATLAW               source/multifluid/multi_submatlaw.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_PRESSURE_EQUILIBRIUM(TIMESTEP, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .     PM, IPM, MULTI_FVM, CURRENT_TIME, BUFMAT, NPF, TF)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD      
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "mvsiz_p.inc"
#include      "vect01_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      INTEGER, INTENT(IN), TARGET :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT), TARGET :: MULTI_FVM
      my_real, INTENT(IN) :: CURRENT_TIME, TIMESTEP
      my_real, INTENT(INOUT) :: BUFMAT(*)
      INTEGER,INTENT(IN)::NPF(SNPC)
      my_real,INTENT(IN)::TF(STF)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_), POINTER :: LBUF
      INTEGER, DIMENSION(:, :), POINTER :: IX
      INTEGER :: NG, II, I
      INTEGER :: NEL
      INTEGER :: NBMAT, IMAT, LOCAL_MATID, MATLAW
      my_real :: VOL_FRAC
      my_real, DIMENSION(:), POINTER :: RHO, SSP, VOL, PRES, EINT, TAU, SPEEINT, 
     .     FEINT, FP
      my_real :: ONE_OVER_RHOC2, MASS
      INTEGER :: ITER, MAX_ITER, REMAINING_ELTS
      my_real :: TOL, TOLE
      LOGICAL :: CONT
      my_real :: GRUN(MVSIZ, MULTI_FVM%NBMAT), COEF(MVSIZ), OLD_PRES(MVSIZ), COEF2(MVSIZ)
      my_real :: FE(MVSIZ)
      INTEGER :: NBMAT_INCELL(MVSIZ)
      my_real :: ONE_OVER_GRUN
      my_real :: ERROR(MVSIZ), RELAXED_PRESSURE(MVSIZ)
      my_real :: PRES_INCR(MVSIZ), TAU_INCR(MVSIZ, MULTI_FVM%NBMAT), 
     .     SPEEINT_INCR(MVSIZ, MULTI_FVM%NBMAT)
      INTEGER :: CV_INDICATOR(MVSIZ)
      my_real, TARGET :: VOLWT(MVSIZ, MULTI_FVM%NBMAT), EINTWT(MVSIZ, MULTI_FVM%NBMAT), 
     .     RHOWT(MVSIZ, MULTI_FVM%NBMAT), PRESWT(MVSIZ, MULTI_FVM%NBMAT),
     .     SSPWT(MVSIZ, MULTI_FVM%NBMAT),
     .     MASSWT(MVSIZ, MULTI_FVM%NBMAT), TAUWT(MVSIZ, MULTI_FVM%NBMAT), 
     .     SPEEINTWT(MVSIZ, MULTI_FVM%NBMAT), ALPHAWT(MVSIZ, MULTI_FVM%NBMAT), 
     .     MFRACWT(MVSIZ, MULTI_FVM%NBMAT),
     .     FPWT(MVSIZ, MULTI_FVM%NBMAT), FEWT(MVSIZ, MULTI_FVM%NBMAT), 
     .     FE0WT(MVSIZ, MULTI_FVM%NBMAT), TAU0WT(MVSIZ, MULTI_FVM%NBMAT)
      my_real :: DPDTAU, DPDE, P0(MVSIZ), COEF1, LIM_TAU(MVSIZ, MULTI_FVM%NBMAT), LIM
      INTEGER :: NIX
      LOGICAL :: IDBG
      my_real :: DUMMY, PSHIFT
      my_real :: SIGOLD(MVSIZ*6)
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      IDBG = .FALSE.

      PSHIFT = MULTI_FVM%PRES_SHIFT

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MTN = IPARG(1, NG)
         IF (MTN == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            LFT = 1
            LLT = NEL
            IF (MULTI_FVM%SYM == 0) THEN
               IX => IXS(1:NIXS, 1 + NFT:NEL + NFT)
               NIX = NIXS
            ELSEIF (ITY == 2) THEN
C     QUADS
               IX => IXQ(1:NIXQ, 1 + NFT:NEL + NFT)
               NIX = NIXQ
            ELSEIF (ITY == 7) THEN
C     TRIANGLES
               IX => IXTG(1:NIXTG, 1 + NFT:NEL + NFT)
               NIX = NIXTG
            ENDIF
C     Multifluid law number, get the number of materials
            NBMAT = MULTI_FVM%NBMAT
C     Global buffer of the current group
            GBUF => ELBUF_TAB(NG)%GBUF
            SIGOLD(1 + 0 * NEL : 1 * NEL)=GBUF%SIG(1 + 0 * NEL : 1 * NEL)
            SIGOLD(1 + 1 * NEL : 2 * NEL)=GBUF%SIG(1 + 1 * NEL : 2 * NEL)
            SIGOLD(1 + 2 * NEL : 3 * NEL)=GBUF%SIG(1 + 2 * NEL : 3 * NEL)
            SIGOLD(1 + 3 * NEL : 4 * NEL)=GBUF%SIG(1 + 3 * NEL : 4 * NEL)
            SIGOLD(1 + 4 * NEL : 5 * NEL)=GBUF%SIG(1 + 4 * NEL : 5 * NEL)
            SIGOLD(1 + 5 * NEL : 6 * NEL)=GBUF%SIG(1 + 5 * NEL : 6 * NEL)                        
            IF (NBMAT == 1) THEN
               LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1, 1, 1)
               IMAT = 1
               LOCAL_MATID = IPM(20 + IMAT, IX(1, 1))
               MATLAW = IPM(2, LOCAL_MATID)
               PRES => MULTI_FVM%PRES(1 + NFT : NEL + NFT)
               SSP  => MULTI_FVM%SOUND_SPEED(1 + NFT : NEL + NFT)
               VOL  => GBUF%VOL
               EINT => MULTI_FVM%EINT(1 + NFT : NEL + NFT)
               RHO => MULTI_FVM%RHO(1 + NFT : NEL + NFT)
               
               CALL MULTI_SUBMATLAW(1, MATLAW, LOCAL_MATID, NEL, EINT, 
     .              PRES, RHO, SSP, VOL, 
     .              GRUN(1:NEL, 1), PM, IPM, NPROPM, NPROPMI, BUFMAT, GBUF%OFF, GBUF%TEMP, 
     .              LBUF%BFRAC, GBUF%TB, GBUF%DELTAX, CURRENT_TIME, SIGOLD,NPF,TF)
               DO II = 1, NEL
                  IF (MULTI_FVM%SOUND_SPEED(II + NFT) > ZERO) THEN
                     MULTI_FVM%SOUND_SPEED(II + NFT) = SQRT(MULTI_FVM%SOUND_SPEED(II + NFT))
                  ELSE
                     MULTI_FVM%SOUND_SPEED(II + NFT) = EM10
                  ENDIF
               ENDDO
               IF (MATLAW == 5) THEN 
                  DO II = 1, NEL
                     I = II + NFT
                     MULTI_FVM%BFRAC(1, I) = LBUF%BFRAC(II)
                  ENDDO
               ENDIF
               GBUF%SIG(1 + 0 * NEL : 1 * NEL) = -PRES(1:NEL)
               GBUF%SIG(1 + 1 * NEL : 2 * NEL) = GBUF%SIG(1 + 0 * NEL : 1 * NEL)
               GBUF%SIG(1 + 2 * NEL : 3 * NEL) = GBUF%SIG(1 + 0 * NEL : 1 * NEL)
            ELSE
C=======================================================================
C     Count number of mat present in cell
C========================               
               NBMAT_INCELL(1:NEL) = 0
               DO IMAT = 1, NBMAT
                  DO II = 1, NEL
                     I = II + NFT
                     IF (MULTI_FVM%PHASE_ALPHA(IMAT, I) > ZERO .AND.
     .                    MULTI_FVM%PHASE_ALPHA(IMAT, I) * MULTI_FVM%PHASE_RHO(IMAT, I) 
     .                    / MULTI_FVM%RHO(I) > EM14) THEN
                        ! At this stage, only significant materials have a
                        ! positive volume. Filtering has been made in MULTI_EVOLVE_PARTIAL
                        NBMAT_INCELL(II) = NBMAT_INCELL(II) + 1
                     ENDIF
                  ENDDO
               ENDDO
C     For cells containing only one material, set eint to global eint
               DO IMAT = 1, NBMAT
                  DO II = 1, NEL
                     I = II + NFT
                     IF (NBMAT_INCELL(II) == 1 .AND. 
     .                    MULTI_FVM%PHASE_ALPHA(IMAT, I) > ZERO) THEN
                        MULTI_FVM%PHASE_EINT(IMAT, I) = MULTI_FVM%EINT(I)
                     ENDIF
                  ENDDO
               ENDDO
C=======================================================================
C     PRESSURE RELAXATION
C=======================================================================
C     As temporary variables, in order not to lose values in case of convergence failure
               DO IMAT = 1, NBMAT
                  DO II = 1, NEL
                     I = II + NFT
C     Volume
                     VOLWT(II, IMAT) = GBUF%VOL(II) * MULTI_FVM%PHASE_ALPHA(IMAT, I)
C     Volume fraction
                     ALPHAWT(II, IMAT) = MULTI_FVM%PHASE_ALPHA(IMAT, I)
C     Internal energy
                     EINTWT(II, IMAT) = MULTI_FVM%PHASE_EINT(IMAT, I)

                     
C     Density
                     RHOWT(II, IMAT) = MULTI_FVM%PHASE_RHO(IMAT, I)
C     Mass fraction
                     MFRACWT(II, IMAT) = ALPHAWT(II, IMAT) * RHOWT(II, IMAT) / GBUF%RHO(II)
C     Specific volume 
                     TAUWT(II, IMAT) = ZERO
C     Specific internal energy
                     SPEEINTWT(II, IMAT) = ZERO
                     IF (RHOWT(II, IMAT) > ZERO) THEN
                        TAUWT(II, IMAT) = ONE / RHOWT(II, IMAT)
                        SPEEINTWT(II, IMAT) = EINTWT(II, IMAT) / RHOWT(II, IMAT)
                     ENDIF
                     TAU0WT(II, IMAT) = TAUWT(II, IMAT)
C     Normalized mass (alpha    rho)
                     MASSWT(II, IMAT) = RHOWT(II, IMAT) * ALPHAWT(II, IMAT)
C     Pressure 
                     PRESWT(II, IMAT) = ZERO
C     Soudspeed
                     SSPWT(II, IMAT) = ZERO
C     Gruneisen coefficient
                     GRUN(II, IMAT) = ZERO
                  ENDDO
               ENDDO
C     ----------------------------------------------
C     OUTER RELAXATION LOOP
C     ----------------------------------------------
               ERROR(1:NEL) = EP10
C     Number of elements that have not converged
               REMAINING_ELTS = 0
               DO II = 1, NEL
                  CV_INDICATOR(II) = 1
                  IF (NBMAT_INCELL(II) > 1) THEN
                     CV_INDICATOR(II) = 0
                     REMAINING_ELTS = REMAINING_ELTS + 1
                  ENDIF
                  RELAXED_PRESSURE(II) = MULTI_FVM%PRES(II + NFT)
                  P0(II) = ZERO
               ENDDO
C     Loop over the fluid layers
               DO IMAT = 1, NBMAT
                  LOCAL_MATID = IPM(20 + IMAT, IX(1, 1))
                  MATLAW = IPM(2, LOCAL_MATID)
                  PRES => PRESWT(1:NEL, IMAT)
                  SSP  => SSPWT(1:NEL, IMAT)
                  VOL  => VOLWT(1:NEL, IMAT)
                  RHO  => RHOWT(1:NEL, IMAT)
                  EINT => EINTWT(1:NEL, IMAT)
                  LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
C     Compute EOS
                  CALL MULTI_SUBMATLAW(1, MATLAW, LOCAL_MATID, NEL, EINT, PRES, RHO, SSP, VOL, 
     .                 GRUN(1:NEL, IMAT), PM, IPM, NPROPM, NPROPMI, BUFMAT, LBUF%OFF, LBUF%TEMP,
     .                 LBUF%BFRAC, GBUF%TB, GBUF%DELTAX, CURRENT_TIME, SIGOLD,NPF,TF)
C     Relaxed pressure initialization
                  DO II = 1, NEL
                     P0(II) = P0(II) + ALPHAWT(II, IMAT) * PRES(II)
                  ENDDO
               ENDDO            ! IMAT = 1, NBMAT    
C     RHS for energy equation
               DO IMAT = 1, NBMAT
                  DO II = 1, NEL
                     IF (ALPHAWT(II, IMAT) /= ZERO) THEN
                        FE0WT(II, IMAT) = SPEEINTWT(II, IMAT)
                     ENDIF
                  ENDDO
               ENDDO
C     It seems that submaterial pressure is not initialized
               IF (TIMESTEP == ZERO) THEN
                  DO II = 1, NEL
                     RELAXED_PRESSURE(II) = P0(II)
                  ENDDO
               ENDIF


C     =================
C     Newton iterations
C     =================
C     Parameters
               MAX_ITER = 50
               TOL = EM4 
               CONT = .TRUE.
               IF (REMAINING_ELTS == 0) CONT = .FALSE.
               ITER = 0
               DO WHILE(ITER < MAX_ITER .AND. CONT)
                  ITER = ITER + 1
                  COEF(1:NEL) = ZERO
                  PRES_INCR(1:NEL) = ZERO
                  ERROR(1:NEL) = ZERO
                  DO IMAT = 1, NBMAT
                     FEINT => FEWT(1:NEL, IMAT)
                     FP => FPWT(1:NEL, IMAT)
                     SSP => SSPWT(1:NEL, IMAT)
                     PRES => PRESWT(1:NEL, IMAT)
                     TAU => TAUWT(1:NEL, IMAT)
                     RHO => RHOWT(1:NEL, IMAT)
                     SPEEINT => SPEEINTWT(1:NEL, IMAT)
                     DO II = 1, NEL
                        IF (CV_INDICATOR(II) /= 1) THEN
                           IF (ALPHAWT(II, IMAT) /= ZERO .AND.
     .                          MFRACWT(II, IMAT) > EM14) THEN
                              FP(II) = PRES(II) - RELAXED_PRESSURE(II)
                              DPDE = GRUN(II, IMAT) / TAU(II)
                              DPDTAU = - (SSP(II) - GRUN(II, IMAT) * (PRES(II) + PSHIFT) * TAU(II)) / TAU(II) / TAU(II)
                              COEF1 =  MASSWT(II, IMAT) / (DPDTAU - (RELAXED_PRESSURE(II) + PSHIFT) * DPDE)
                              COEF(II) = COEF(II) + COEF1 * (ONE + GRUN(II, IMAT))
                              PRES_INCR(II) = PRES_INCR(II) - FP(II) * COEF1
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
                
                 
                  !ERROR(1:NEL) = ZERO
                  DO II = 1, NEL
                     IF (CV_INDICATOR(II) /= 1) THEN
                        PRES_INCR(II) = PRES_INCR(II) / COEF(II)
                        ERROR(II) = ABS(PRES_INCR(II))
                     ENDIF
                  ENDDO
                  LIM_TAU(1:NEL, 1:NBMAT) = ONE
                  DO IMAT = 1, NBMAT
                     FP => FPWT(1:NEL, IMAT)
                     SSP => SSPWT(1:NEL, IMAT)
                     PRES => PRESWT(1:NEL, IMAT)
                     TAU => TAUWT(1:NEL, IMAT)
                     RHO => RHOWT(1:NEL, IMAT)
                     SPEEINT => SPEEINTWT(1:NEL, IMAT)
                     DO II = 1, NEL
                        IF (CV_INDICATOR(II) /= 1) THEN
                           IF (ALPHAWT(II, IMAT) /= ZERO .AND.
     .                          MFRACWT(II, IMAT) > EM14) THEN
                              DPDTAU = - (SSP(II) - GRUN(II, IMAT) * (PRES(II) + PSHIFT) * TAU(II)) / TAU(II) / TAU(II)
                              DPDE = GRUN(II, IMAT) / TAU(II)
                              COEF1 = ONE / (DPDTAU - (RELAXED_PRESSURE(II) + PSHIFT) * DPDE)
                              TAU_INCR(II, IMAT) = COEF1 * (PRES_INCR(II) * (ONE + GRUN(II, IMAT)) + FP(II))
                              LIM = ONE
                              IF (TAU(II) - TAU_INCR(II, IMAT) < ZERO 
     .                             .AND. TAU_INCR(II, IMAT) > ZERO) THEN
                                 LIM = MIN(ONE, HALF * TAU(II) / TAU_INCR(II, IMAT))
                              ENDIF
                              LIM_TAU(II, IMAT) = LIM
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
                  

                  DO IMAT = 1, NBMAT
                     PRES => PRESWT(1:NEL, IMAT)
                     SSP  => SSPWT(1:NEL, IMAT)
                     VOL  => VOLWT(1:NEL, IMAT)
                     RHO  => RHOWT(1:NEL, IMAT)
                     EINT => EINTWT(1:NEL, IMAT) 
                     TAU => TAUWT(1:NEL, IMAT)
                     DO II = 1, NEL
                        IF (CV_INDICATOR(II) /= 1) THEN
                           IF (ALPHAWT(II, IMAT) /= ZERO .AND.
     .                          MFRACWT(II, IMAT) > EM14) THEN
                              LIM = MINVAL(LIM_TAU(II, 1:NBMAT))
                              TAU(II) = TAU(II) - LIM * TAU_INCR(II, IMAT)
                              
                              RHO(II) = ONE / TAU(II)
                              ALPHAWT(II, IMAT) = MASSWT(II, IMAT) * TAU(II)
                              VOL(II) = ALPHAWT(II, IMAT) * GBUF%VOL(II)
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO 
                  DO II = 1, NEL
                     IF (CV_INDICATOR(II) /= 1) THEN
                        RELAXED_PRESSURE(II) = RELAXED_PRESSURE(II) - PRES_INCR(II)
                     ENDIF
                  ENDDO
                  DO IMAT = 1, NBMAT
                     EINT => EINTWT(1:NEL, IMAT) 
                     RHO  => RHOWT(1:NEL, IMAT)
                     SPEEINT => SPEEINTWT(1:NEL, IMAT)
                     TAU => TAUWT(1:NEL, IMAT)
                     DO II = 1, NEL
                        IF (CV_INDICATOR(II) /= 1) THEN
                           IF (ALPHAWT(II, IMAT) /= ZERO .AND.
     .                          MFRACWT(II, IMAT) > EM14) THEN
C                           IF (ALPHAWT(II, IMAT) /= ZERO .AND. GRUN(II, IMAT) /= ZERO) THEN
                              SPEEINT(II) = FE0WT(II, IMAT) - 
     .                             (RELAXED_PRESSURE(II) + PSHIFT) * (TAU(II) - TAU0WT(II, IMAT))
                              EINT(II) = RHO(II) * SPEEINT(II)
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
                  DO II = 1, NEL
                     IF (CV_INDICATOR(II) /= 1) THEN
                        IF (ERROR(II) < TOL * (ONE + ABS(RELAXED_PRESSURE(II)))) THEN
                           CV_INDICATOR(II) = 1
                           REMAINING_ELTS = REMAINING_ELTS - 1
                        ENDIF
                     ENDIF
                  ENDDO
                  IF (REMAINING_ELTS == 0) THEN
                     CONT = .FALSE.
                  ENDIF
C     Equation of state call
                  DO IMAT = 1, NBMAT
                     LOCAL_MATID = IPM(20 + IMAT, IX(1, 1))
                     MATLAW = IPM(2, LOCAL_MATID)
                     PRES => PRESWT(1:NEL, IMAT)
                     SSP  => SSPWT(1:NEL, IMAT)
                     VOL  => VOLWT(1:NEL, IMAT)
                     RHO  => RHOWT(1:NEL, IMAT)
                     EINT => EINTWT(1:NEL, IMAT)
                     LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
C     Compute EOS
                     CALL MULTI_SUBMATLAW(0, MATLAW, LOCAL_MATID, NEL, EINT, PRES, RHO, SSP, VOL, 
     .                    GRUN(1:NEL, IMAT), PM, IPM, NPROPM, NPROPMI, BUFMAT, LBUF%OFF,LBUF%TEMP,
     .                    LBUF%BFRAC, GBUF%TB, GBUF%DELTAX, CURRENT_TIME, SIGOLD, NPF, TF)
                  ENDDO         ! IMAT = 1, NBMAT 
                  
               ENDDO            !ITER = 1, MAX_ITER
               DO II = 1, NEL
                  DO IMAT = 1, NBMAT
                     IF (EINTWT(II, IMAT) < ZERO) THEN
                        !!!PRINT*, "OUPS1"
                        EINTWT(II, IMAT) = -EINTWT(II, IMAT)
                     ENDIF
                  ENDDO
               ENDDO
               IF (REMAINING_ELTS /= 0 .AND. IDBG) THEN
                  PRINT*, "*** CV PB IN PRESSURE EQUILIBRIUM", " CYCLE", NCYCLE, "GROUPE", NG
                  DO II = 1, NEL
                     IF (CV_INDICATOR(II) == 0) THEN
                        WRITE(*, '(A,I10,A,I10)') "LOCAL_ID ", II + NFT, " GLOBAL_ID ", IX(NIX, II)
                        DO IMAT = 1, NBMAT
                           WRITE(*,*) "MAT ", IMAT, ", alph : ", ALPHAWT(II, IMAT), ", PRES : ", PRESWT(II, IMAT)
     .                          , ", EINT : ", EINTWT(II, IMAT)
                        ENDDO
                        CONTINUE
                     ENDIF
                  ENDDO
               ENDIF
C     ----------------------------------------------
C     END OF OUTER RELAXATION LOOP
C     ----------------------------------------------

C     Loop over the fluid layers
               DO IMAT = 1, NBMAT
                  LOCAL_MATID = IPM(20 + IMAT, IX(1, 1))
                  MATLAW = IPM(2, LOCAL_MATID)
                  PRES => PRESWT(1:NEL, IMAT)
                  SSP  => SSPWT(1:NEL, IMAT)
                  VOL  => VOLWT(1:NEL, IMAT)
                  RHO  => RHOWT(1:NEL, IMAT)
                  EINT => EINTWT(1:NEL, IMAT)

                  LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                  MULTI_FVM%PHASE_EINT(IMAT, 1 + NFT : NEL + NFT) = EINTWT(1:NEL, IMAT)
                  MULTI_FVM%PHASE_RHO(IMAT, 1 + NFT : NEL + NFT) = RHOWT(1:NEL, IMAT)
                  DO II = 1, NEL
                     I = II + NFT
                     MULTI_FVM%PHASE_ALPHA(IMAT, I) = VOLWT(II, IMAT) / GBUF%VOL(II)
                  ENDDO
C     Compute EOS
                  CALL MULTI_SUBMATLAW(0, MATLAW, LOCAL_MATID, NEL, EINT, PRES, RHO, SSP, VOL, 
     .                 GRUN(1:NEL, IMAT), PM, IPM, NPROPM, NPROPMI, BUFMAT, LBUF%OFF,LBUF%TEMP,
     .                 LBUF%BFRAC, GBUF%TB, GBUF%DELTAX, CURRENT_TIME, SIGOLD, NPF, TF)
               ENDDO            ! IMAT = 1, NBMAT                             
               
C     Equilibrium densities have been found,
C     ----------------------------------------------
C     IT IS NECESSARY NOW TO RECOVER THE GLOBAL ENERGY
C     THAT IS ENSURING THAT E = E1 + E2 + ...
C     ----------------------------------------------
C     We're solving : 
C     SUM ALPHA_I EINT_I - EINT = 0
C     P1 - P = 0
C     ...
C     PN - P = 0
C     With unknowns P, EINT1, EINT2, ..., EINT_N
C     Initial guess for P is the relaxed pressure coming from the
C     Previous iterative system
C     Initial guesses for submaterial internal energy also come 
C     from the previous system
               ERROR(1:NEL) = EP10
C     Number of elements that have not converged
               REMAINING_ELTS = 0
               DO II = 1, NEL
                  I = II + NFT
                  CV_INDICATOR(II) = 1
                  ONE_OVER_GRUN = ONE
                  DO IMAT = 1, NBMAT
                     IF (MULTI_FVM%PHASE_ALPHA(IMAT, I) > ZERO .AND.
     .                    MFRACWT(II, IMAT) > EM14) THEN
                        ONE_OVER_GRUN = ONE_OVER_GRUN * GRUN(II, IMAT)
                     ENDIF
                  ENDDO
                  IF (NBMAT_INCELL(II) > 1 .AND. ONE_OVER_GRUN > ZERO) THEN
C                  IF (NBMAT_INCELL(II) > 1) THEN
                     CV_INDICATOR(II) = 0
                     REMAINING_ELTS = REMAINING_ELTS + 1
                  ENDIF
               ENDDO

               CONT = .TRUE.
               IF (REMAINING_ELTS == 0) CONT = .FALSE.
               ITER = 0
               TOLE = EM06
               MAX_ITER = 10
               DO WHILE(ITER < MAX_ITER .AND. CONT)
                  ITER = ITER + 1
C     Compute new pressure
                  ! Store coefficients
                  COEF(1:NEL) = ZERO
                  ! Function to cancel
                  FE(1:NEL) = - MULTI_FVM%EINT(1 + NFT : NEL + NFT)
                  PRES_INCR(1:NEL) = ZERO
                  DO IMAT = 1, NBMAT
                     PRES => PRESWT(1:NEL, IMAT)
                     VOL  => VOLWT(1:NEL, IMAT)
                     EINT => EINTWT(1:NEL, IMAT) 
                     FP => FPWT(1:NEL, IMAT)
                     DO II = 1, NEL
                        IF (VOL(II) > ZERO .AND.
     .                       MFRACWT(II, IMAT) > EM14) THEN
                           IF (CV_INDICATOR(II) /= 1) THEN
                              ONE_OVER_GRUN = ZERO
                              IF (GRUN(II, IMAT) > ZERO) THEN
                                 ONE_OVER_GRUN = ONE / GRUN(II, IMAT)
                              ENDIF
                              VOL_FRAC = ALPHAWT(II, IMAT) 

                              FP(II) = PRES(II) - RELAXED_PRESSURE(II)
                              IF (ONE_OVER_GRUN > ZERO) THEN
                                 FE(II) = FE(II) + VOL_FRAC * EINT(II)
                              ENDIF
                              
                              PRES_INCR(II) =  PRES_INCR(II) + VOL_FRAC * ONE_OVER_GRUN * FP(II)
                              COEF(II) = COEF(II) + VOL_FRAC * ONE_OVER_GRUN

                           ENDIF
                        ENDIF
                     ENDDO      ! II = 1, NEL
                  ENDDO  ! IMAT = 1, NBMAT
                  DO II = 1, NEL
                     IF (CV_INDICATOR(II) /= 1) THEN
                        IF (COEF(II) /= ZERO) THEN
! Pressure increment
                           PRES_INCR(II) = (FE(II) - PRES_INCR(II)) / COEF(II)
                           RELAXED_PRESSURE(II) = RELAXED_PRESSURE(II) - PRES_INCR(II)
! Compute ERROR
                           ERROR(II) = ABS(PRES_INCR(II))
                        ENDIF
                     ENDIF
                  ENDDO
C     Compute New energies
                  DO IMAT = 1, NBMAT
                     PRES => PRESWT(1:NEL, IMAT)
                     VOL  => VOLWT(1:NEL, IMAT)
                     RHO  => RHOWT(1:NEL, IMAT)
                     EINT => EINTWT(1:NEL, IMAT) 
                     FP => FPWT(1:NEL, IMAT)
                     DO II = 1, NEL
                        IF (CV_INDICATOR(II) /= 1) THEN
                           IF (VOL(II) > ZERO .AND. GRUN(II, IMAT) > ZERO .AND.
     .                          MFRACWT(II, IMAT) > EM14) THEN
                              LIM = ONE
                              IF ((FP(II) + PRES_INCR(II)) > ZERO) THEN
                                 LIM = MAX(ZERO, MIN (ONE, HALF * EINT(II) * GRUN(II, IMAT) 
     .                                / (FP(II) + PRES_INCR(II))))
                              ENDIF
                              EINT(II) = EINT(II) - LIM * (FP(II) + PRES_INCR(II)) / GRUN(II, IMAT)
                           ENDIF
                        ENDIF
                     ENDDO  ! II = 1, NEL
C     Call equation of state for the submaterial
                     LOCAL_MATID = IPM(20 + IMAT, IX(1, 1))
                     MATLAW = IPM(2, LOCAL_MATID)
                     SSP  => SSPWT(1:NEL, IMAT)
                     VOL  => VOLWT(1:NEL, IMAT)
                     LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                     CALL MULTI_SUBMATLAW(0, MATLAW, LOCAL_MATID, NEL, EINT, PRES, RHO, SSP, VOL, 
     .                    GRUN(1:NEL, IMAT), PM, IPM, NPROPM, NPROPMI, BUFMAT, LBUF%OFF,LBUF%TEMP,
     .                    LBUF%BFRAC, GBUF%TB, GBUF%DELTAX, CURRENT_TIME, SIGOLD, NPF, TF)
                  ENDDO  ! IMAT = 1, NBMAT
                  DO II = 1, NEL
                     IF (CV_INDICATOR(II) /= 1) THEN
                        IF (ERROR(II) < TOL * (ONE + ABS(RELAXED_PRESSURE(II)))) THEN
                           CV_INDICATOR(II) = 1
                           REMAINING_ELTS = REMAINING_ELTS - 1
                        ENDIF
                     ENDIF
                  ENDDO
                  IF (REMAINING_ELTS == 0) THEN
                     CONT = .FALSE.
                  ENDIF
               ENDDO  !  WHILE(ITER < MAX_ITER .AND. CONT)
               DO II = 1, NEL
                  DO IMAT = 1, NBMAT
                     IF (EINTWT(II, IMAT) < ZERO) THEN
C                        PRINT*, "OUPS2"
                     ENDIF
                  ENDDO
               ENDDO
               IF (REMAINING_ELTS /= 0 .AND. CONT .AND. IDBG) THEN
                  PRINT*, "*** CV PB IN ENERGY RESET", "CYCLE", NCYCLE, "GROUPE", NG
               ENDIF 
C     ----------------------------------------------
C     END OF ENERGY RESET
C     THAT IS ENSURING THAT E = E1 + E2 + ...
C     ----------------------------------------------
               GBUF%SIG(1:NEL) = ZERO
C     Store final values
               MULTI_FVM%SOUND_SPEED(1 + NFT:NEL + NFT) = ZERO
               DO IMAT = 1, NBMAT
                  LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                  MULTI_FVM%PHASE_EINT(IMAT, 1 + NFT : NEL + NFT) = EINTWT(1:NEL, IMAT)
                  MULTI_FVM%PHASE_RHO(IMAT, 1 + NFT : NEL + NFT) = RHOWT(1:NEL, IMAT)
C     Pressure
                  MULTI_FVM%PHASE_PRES(IMAT, 1 + NFT : NEL + NFT) = PRESWT(1:NEL, IMAT)
                  LOCAL_MATID = IPM(20 + IMAT, IX(1, 1))
                  MATLAW = IPM(2, LOCAL_MATID)
C     Global Pressure: volumic fraction ponderation
                  DO II = 1, NEL
                     I = II + NFT
                     VOL_FRAC = MULTI_FVM%PHASE_ALPHA(IMAT, I)
                     GBUF%SIG(II) = GBUF%SIG(II) - PRESWT(II, IMAT) * VOL_FRAC
                     MULTI_FVM%SOUND_SPEED(II + NFT) = MULTI_FVM%SOUND_SPEED(II + NFT) + 
     .                    VOL_FRAC * RHOWT(II, IMAT) * MAX(SSPWT(II, IMAT), EM20)
                     IF (MATLAW == 5) THEN
                        MULTI_FVM%BFRAC(IMAT, I) = LBUF%BFRAC(II)
                     ELSE
                        MULTI_FVM%BFRAC(IMAT, I) = ZERO
                     ENDIF
                  ENDDO
               ENDDO
c$$$C     GLobal density (should not have changed except for numerical errors
c$$$               DO II = 1, NEL
c$$$                  I = II + NFT
c$$$                  MULTI_FVM%RHO(I) =ZERO
c$$$                  DO IMAT = 1, NBMAT
c$$$                     MULTI_FVM%RHO(I) = MULTI_FVM%RHO(I) + 
c$$$     .                    MULTI_FVM%PHASE_ALPHA(IMAT, I) * MULTI_FVM%PHASE_RHO(IMAT, I)
c$$$                  ENDDO
c$$$               ENDDO
               GBUF%SIG(1 + 1 * NEL : 2 * NEL) = GBUF%SIG(1 + 0 * NEL : 1 * NEL)
               GBUF%SIG(1 + 2 * NEL : 3 * NEL) = GBUF%SIG(1 + 0 * NEL : 1 * NEL)
C     Global Sound speed
               DO II = 1, NEL
                  I = II + NFT
                  MULTI_FVM%SOUND_SPEED(II + NFT) = SQRT(MULTI_FVM%SOUND_SPEED(II + NFT) 
     .                 / MULTI_FVM%RHO(I))
                  MULTI_FVM%PRES(II + NFT) =  -GBUF%SIG(II)
               ENDDO
C     Global Temperature 
               !we must introduce an iterative solver : solve "find T such as e_global+P/rho = int(Cp_global(T) dT)"
               !this requires also temperature solving from all EoS (improvement to plan)
               GBUF%TEMP(1:NEL) = ZERO
               DO IMAT=1,NBMAT
                 LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)               
                 DO II = 1, NEL
                    GBUF%TEMP(II) = GBUF%TEMP(II) + MFRACWT(II, IMAT)*LBUF%TEMP(II)
                 ENDDO
               ENDDO
            ENDIF               ! NBMAT == 1
         ENDIF                  ! MTN == 151
      ENDDO                     ! NG = ITASK + 1, NGROUP, NTHREAD
C-----------------------------------------------
C     E n d   o f   s u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE MULTI_PRESSURE_EQUILIBRIUM
