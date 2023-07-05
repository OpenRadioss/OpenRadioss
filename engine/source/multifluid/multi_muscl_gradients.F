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
Chd|  MULTI_MUSCL_GRADIENTS         source/multifluid/multi_muscl_gradients.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        LIMITER                       source/multifluid/multi_muscl_gradients.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_MUSCL_GRADIENTS(ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .     PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, WGRID, XGRID, ITAB, NBMAT, CURRENT_TIME, BUFMAT)
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
!     NUMELS      
#include      "param_c.inc"
#include      "task_c.inc"
!     DTFAC1
#include      "macro.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      my_real, INTENT(IN) :: WGRID(3, *), XGRID(3, *), CURRENT_TIME
      INTEGER, INTENT(IN) :: ITAB(*), NBMAT
      my_real, INTENT(INOUT) :: BUFMAT(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, I, J, NB_FACE, NB_FACE2, KFACE, IFACE, NG, IMAT
      my_real :: VALK, VALL, NX(6), NY(6), NZ(6),
     .     XK(3), XF(3, 6), DELTA, LIM_RHO, XL(3, 6)
      my_real :: MAXVAL_RHO, MINVAL_RHO, 
     .     MAXVAL_U, MINVAL_U, 
     .     MAXVAL_V, MINVAL_V, 
     .     MAXVAL_W, MINVAL_W, LIM_VELX, LIM_VELY, LIM_VELZ, LIM_VEL
      my_real :: MAXVAL_PRES, MINVAL_PRES, LIM_PRES
      my_real :: PHASE_MAXVAL_RHO(NBMAT), PHASE_MINVAL_RHO(NBMAT), 
     .     PHASE_MAXVAL_ALPHA(NBMAT), PHASE_MINVAL_ALPHA(NBMAT), 
     .     PHASE_MAXVAL_PRES(NBMAT), PHASE_MINVAL_PRES(NBMAT), 
     .     PHASE_LIM_ALPHA(NBMAT), PHASE_LIM_RHO(NBMAT), PHASE_LIM_PRES(NBMAT)
      my_real :: PEPS, MEPS, LIMFACE(6)
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_), POINTER :: LBUF 
      INTEGER :: VOIS(6), FACE_LIST(6), NEL, ITY, ISOLNOD, NFT, MATLAW, 
     .     NODE1, NODE2, NODE3, NODE4
      my_real :: SUMX, SUMY, SUMZ, VOL, ONE_OVER_VOL, LIM_ALPHA
      my_real :: EPS_INT
      my_real, DIMENSION(:), POINTER :: VOLUME
      my_real :: X1(3), X2(3), X3(3), X4(3)
      my_real :: MAT(3, 3), RHS(3), SOL(3), MATM1(3, 3), DET, MATTEST(3, 3)
      INTEGER :: IAD, LGTH



      IF(ITASK == 0) CALL STARTIME(MACRO_TIMER_MUSCL,1)
      EPS_INT = FIVE * EM02

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MATLAW = IPARG(1, NG)
         IF (MATLAW == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            ISOLNOD = IPARG(28, NG)

            GBUF => ELBUF_TAB(NG)%GBUF
            IF (MULTI_FVM%SYM == 0) THEN
               VOLUME => GBUF%VOL(1:NEL)
            ELSE
               VOLUME => GBUF%AREA(1:NEL)
            ENDIF

            PEPS = EM10
            MEPS = -EM10

            DO IFACE = 1, 6
               FACE_LIST(IFACE) = IFACE
            ENDDO

            NB_FACE = -1
            NB_FACE2 = -1
            SELECT CASE (MULTI_FVM%SYM)
            CASE (0)
C     Solids
               NB_FACE = 6
               NB_FACE2 = 6
               IF (ISOLNOD == 4) THEN
C     Tetra
                  NB_FACE = 4
                  FACE_LIST(1) = 5
                  FACE_LIST(2) = 6
                  FACE_LIST(3) = 2
                  FACE_LIST(4) = 4
                  FACE_LIST(5) = -1
                  FACE_LIST(6) = -1
               ENDIF
            CASE (1, 2)
               IF (ITY == 2) THEN
C     QUADS
                  NB_FACE = 4
                  NB_FACE2 = 4
               ELSEIF (ITY == 7) THEN
C     TRIANGLES
                  NB_FACE = 3
                  NB_FACE2 = 3
               ENDIF
            CASE DEFAULT
               CALL ARRET(2)
            END SELECT

            DO II = 1, NEL
               I = II + NFT
               IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
!               NB_FACE = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1) - 
!     .              ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               VOIS(1:6) = -1
               NX(1:6) = -EP30
               NY(1:6) = -EP30
               NZ(1:6) = -EP30
               VOL = VOLUME(II)
               ONE_OVER_VOL = ONE / VOL
C     Element centroid
               XK(1:3) = MULTI_FVM%ELEM_DATA%CENTROID(1:3, I)
C     Matrice
               MAT(1:3, 1:3) = ZERO
               DO IFACE = 1, NB_FACE
                  KFACE = FACE_LIST(IFACE)
                  J = ALE_CONNECTIVITY%ee_connect%connected(IAD + KFACE - 1)
                  XF(1:3, KFACE) = MULTI_FVM%FACE_DATA%CENTROID(1:3, KFACE, I)
                  IF (J > 0) THEN
                     VOIS(KFACE) = J
                     XL(1:3, KFACE) = MULTI_FVM%ELEM_DATA%CENTROID(1:3, J)
                  ELSE
                     XL(1:3, KFACE) = TWO * XF(1:3, KFACE) - XK(1:3)
                  ENDIF
                  NX(KFACE) = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, I)
                  NY(KFACE) = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, I)
                  NZ(KFACE) = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, I)
                  MAT(1, 1) = MAT(1, 1) + (XL(1, KFACE) - XK(1)) * (XL(1, KFACE) - XK(1))
                  MAT(1, 2) = MAT(1, 2) + (XL(1, KFACE) - XK(1)) * (XL(2, KFACE) - XK(2))
                  MAT(1, 3) = MAT(1, 3) + (XL(1, KFACE) - XK(1)) * (XL(3, KFACE) - XK(3))
                  MAT(2, 1) = MAT(2, 1) + (XL(2, KFACE) - XK(2)) * (XL(1, KFACE) - XK(1))
                  MAT(2, 2) = MAT(2, 2) + (XL(2, KFACE) - XK(2)) * (XL(2, KFACE) - XK(2))
                  MAT(2, 3) = MAT(2, 3) + (XL(2, KFACE) - XK(2)) * (XL(3, KFACE) - XK(3))
                  MAT(3, 1) = MAT(3, 1) + (XL(3, KFACE) - XK(3)) * (XL(1, KFACE) - XK(1))
                  MAT(3, 2) = MAT(3, 2) + (XL(3, KFACE) - XK(3)) * (XL(2, KFACE) - XK(2))
                  MAT(3, 3) = MAT(3, 3) + (XL(3, KFACE) - XK(3)) * (XL(3, KFACE) - XK(3))
               ENDDO

               IF (MULTI_FVM%SYM /= 0) THEN
                  MAT(1, 1) = ONE
                  MAT(1, 2) = ZERO
                  MAT(1, 3) = ZERO
                  MAT(2, 1) = ZERO
                  MAT(3, 1) = ZERO
               ENDIF
               
               DET = MAT(1, 1) * MAT(2, 2) * MAT(3, 3) - MAT(1, 1) * MAT(2, 3)**2 
     .              - MAT(2, 2) * MAT(1, 3)**2 - MAT(3, 3) * MAT(1, 2)**2
     .              + TWO * MAT(1, 2) * MAT(1, 3) * MAT(2, 3)
               IF (DET /= ZERO) THEN
                  DET = ONE / DET
               ELSE
                  CALL ARRET(2)
               ENDIF
               
               MATM1(1, 1) = MAT(2, 2) * MAT(3, 3) - MAT(2, 3)**2
               MATM1(1, 2) = -MAT(1, 2) * MAT(3, 3) + MAT(1, 3) * MAT(2, 3)
               MATM1(1, 3) = MAT(1, 2) * MAT(2, 3) - MAT(1, 3) * MAT(2, 2)
               MATM1(2, 2) = MAT(1, 1) * MAT(3, 3) - MAT(1, 3)**2
               MATM1(2, 3) = -MAT(1, 1) * MAT(2, 3) + MAT(1, 2) * MAT(1, 3)
               MATM1(3, 3) = MAT(1, 1) * MAT(2, 2) - MAT(1, 2)**2
               MATM1(2, 1) = MATM1(1, 2)
               MATM1(3, 1) = MATM1(1, 3)
               MATM1(3, 2) = MATM1(2, 3)
               
               MATM1(1:3, 1:3) = MATM1(1:3, 1:3) * DET

C               MATTEST(1:3, 1:3) = MATMUL(MAT(1:3, 1:3), MATM1(1:3, 1:3))
               
               IF (MULTI_FVM%SYM == 1) THEN
C     Use planar surface here
                  IF (ITY == 2) THEN
C     QUADS
                     NODE1 = IXQ(2, I)
                     NODE2 = IXQ(3, I)
                     NODE3 = IXQ(4, I)
                     NODE4 = IXQ(5, I)
                     X1(1:3) = XGRID(1:3, NODE1)
                     X2(1:3) = XGRID(1:3, NODE2)
                     X3(1:3) = XGRID(1:3, NODE3)
                     X4(1:3) = XGRID(1:3, NODE4)
                 ELSE IF (ITY == 7) THEN
C     TRIANGLES
                     NODE1 = IXTG(2, I)
                     NODE2 = IXTG(3, I)
                     NODE3 = IXTG(4, I)
                     X1(1:3) = XGRID(1:3, NODE1)
                     X2(1:3) = XGRID(1:3, NODE2)
                     X3(1:3) = XGRID(1:3, NODE3)
                  ENDIF
               ENDIF
               
C     Velocity gradient
               MAXVAL_U = MULTI_FVM%VEL(1, I)
               MINVAL_U = MAXVAL_U
               MAXVAL_V = MULTI_FVM%VEL(2, I)
               MINVAL_V = MAXVAL_V
               MAXVAL_W = MULTI_FVM%VEL(3, I)
               MINVAL_W = MAXVAL_W

               IF (NBMAT == 1) THEN
C     =========
C     MONOFLUID
C     =========
C     Density gradient
                  MAXVAL_RHO = MULTI_FVM%RHO(I)
                  MINVAL_RHO = MAXVAL_RHO
C     Pressure gradient
                  MAXVAL_PRES = MULTI_FVM%EINT(I)
                  MINVAL_PRES = MAXVAL_PRES
               ELSE
C     ==========
C     MULTIFLUID
C     ==========
                  DO IMAT = 1, NBMAT
C     Density gradient
                     PHASE_MAXVAL_RHO(IMAT) = MULTI_FVM%PHASE_RHO(IMAT, I)
                     PHASE_MINVAL_RHO(IMAT) = PHASE_MAXVAL_RHO(IMAT)
C     Pressure gradient
                     PHASE_MAXVAL_PRES(IMAT) = MULTI_FVM%PHASE_EINT(IMAT, I)
                     PHASE_MINVAL_PRES(IMAT) = PHASE_MAXVAL_PRES(IMAT)
C     Volume fraction gradient
                     PHASE_MAXVAL_ALPHA(IMAT) = MULTI_FVM%PHASE_ALPHA(IMAT, I)
                     PHASE_MINVAL_ALPHA(IMAT) = PHASE_MAXVAL_ALPHA(IMAT)
                  ENDDO
               ENDIF

               IF (MULTI_FVM%MUSCL == 1) THEN
C     Velocity gradient -> X component
                  VALK = MULTI_FVM%VEL(1, I)
                  RHS(1:3) = ZERO
                  DO IFACE = 1, NB_FACE
                     KFACE = FACE_LIST(IFACE)
                     J = VOIS(KFACE)
                     VALL = VALK
                     IF (J > 0) THEN
                        VALL = MULTI_FVM%VEL(1, J)
                        MAXVAL_U = MAX(MAXVAL_U, VALL)
                        MINVAL_U = MIN(MINVAL_U, VALL)
                        RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                        RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                        RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))
                     ENDIF
                  ENDDO
                  
                  SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                  SUMX = -SOL(1)
                  SUMY = -SOL(2)
                  SUMZ = -SOL(3)
C     Component X limitation
                  LIMFACE(1:6) = ONE
                  DO IFACE = 1, NB_FACE
                     KFACE = FACE_LIST(IFACE)
                     DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                    SUMY * (XF(2, KFACE) - XK(2)) +
     .                    SUMZ * (XF(3, KFACE) - XK(3))  
                     IF (DELTA > ZERO) THEN
                        LIMFACE(KFACE) = HALF * (MAXVAL_U - VALK) / DELTA
                     ELSE IF (DELTA < ZERO) THEN
                        LIMFACE(KFACE) = HALF * (MINVAL_U - VALK) / DELTA
                     ENDIF
                     CALL LIMITER(LIMFACE(KFACE), ONE)
                  ENDDO
                  LIM_VELX = MINVAL(LIMFACE(1:6))
                  MULTI_FVM%GRAD_U(1, I) = SUMX * LIM_VELX
                  MULTI_FVM%GRAD_U(2, I) = SUMY * LIM_VELX
                  MULTI_FVM%GRAD_U(3, I) = SUMZ * LIM_VELX
C     Velocity gradient -> Y component
                  VALK = MULTI_FVM%VEL(2, I)
                  RHS(1:3) = ZERO
                  DO IFACE = 1, NB_FACE
                     KFACE = FACE_LIST(IFACE)
                     J = VOIS(KFACE)
                     VALL = VALK
                     IF (J > 0) THEN
                        VALL = MULTI_FVM%VEL(2, J)
                        MAXVAL_V = MAX(MAXVAL_V, VALL)
                        MINVAL_V = MIN(MINVAL_V, VALL)
                        RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                        RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                        RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))
                     ENDIF
                  ENDDO
                  SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                  SUMX = -SOL(1)
                  SUMY = -SOL(2)
                  SUMZ = -SOL(3)
C     Component Y limitation
                  LIMFACE(1:6) = ONE
                  DO IFACE = 1, NB_FACE
                     KFACE = FACE_LIST(IFACE)
                     DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                    SUMY * (XF(2, KFACE) - XK(2)) +
     .                    SUMZ * (XF(3, KFACE) - XK(3))  
                     IF (DELTA > ZERO) THEN
                        LIMFACE(KFACE) = HALF * (MAXVAL_V - VALK) / DELTA
                     ELSE IF (DELTA < ZERO) THEN
                        LIMFACE(KFACE) = HALF * (MINVAL_V - VALK) / DELTA
                     ENDIF
                     CALL LIMITER(LIMFACE(KFACE), ONE)
                  ENDDO
                  LIM_VELY = MINVAL(LIMFACE(1:6))
                  MULTI_FVM%GRAD_V(1, I) = SUMX * LIM_VELY 
                  MULTI_FVM%GRAD_V(2, I) = SUMY * LIM_VELY 
                  MULTI_FVM%GRAD_V(3, I) = SUMZ * LIM_VELY 
C     Velocity gradient -> Z component
                  VALK = MULTI_FVM%VEL(3, I)
                  RHS(1:3) = ZERO
                  DO IFACE = 1, NB_FACE
                     KFACE = FACE_LIST(IFACE)
                     J = VOIS(KFACE)
                     VALL = VALK
                     IF (J > 0) THEN
                        VALL = MULTI_FVM%VEL(3, J)
                        MAXVAL_W = MAX(MAXVAL_W, VALL)
                        MINVAL_W = MIN(MINVAL_W, VALL)
                        RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                        RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                        RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))
                     ENDIF
                  ENDDO
                  SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                  SUMX = -SOL(1)
                  SUMY = -SOL(2)
                  SUMZ = -SOL(3)
C     Component Z limitation
                  LIMFACE(1:6) = ONE
                  DO IFACE = 1, NB_FACE
                     KFACE = FACE_LIST(IFACE)
                     DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                    SUMY * (XF(2, KFACE) - XK(2)) +
     .                    SUMZ * (XF(3, KFACE) - XK(3))  
                     IF (DELTA > ZERO) THEN
                        LIMFACE(KFACE) = HALF * (MAXVAL_W - VALK) / DELTA
                     ELSE IF (DELTA < ZERO) THEN
                        LIMFACE(KFACE) = HALF * (MINVAL_W - VALK) / DELTA
                     ENDIF
                     CALL LIMITER(LIMFACE(KFACE), ONE)
                  ENDDO  
                  LIM_VELZ = MINVAL(LIMFACE(1:6))
                  MULTI_FVM%GRAD_W(1, I) = SUMX * LIM_VELZ 
                  MULTI_FVM%GRAD_W(2, I) = SUMY * LIM_VELZ 
                  MULTI_FVM%GRAD_W(3, I) = SUMZ * LIM_VELZ 
               ENDIF !MUSCL == 1

               IF (NBMAT == 1) THEN
C     ==========
C     MONOFLUID
C     ==========
C     Density gradient
                  IF (MULTI_FVM%MUSCL == 1) THEN
                     VALK = MULTI_FVM%RHO(I)
                     RHS(1:3) = ZERO
                     DO IFACE = 1, NB_FACE
                        KFACE = FACE_LIST(IFACE)
                        J = VOIS(KFACE)
                        VALL = VALK
                        IF (J > 0) THEN
                           VALL = MULTI_FVM%RHO(J)
                           MAXVAL_RHO = MAX(MAXVAL_RHO, VALL)
                           MINVAL_RHO = MIN(MINVAL_RHO, VALL)
                           RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                           RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                           RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))  
                        ENDIF         
                     ENDDO
                     SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                     SUMX = -SOL(1)
                     SUMY = -SOL(2)
                     SUMZ = -SOL(3)
C     Density gradient limitation
                     LIMFACE(1:6) = ONE
                     DO IFACE = 1, NB_FACE
                        KFACE = FACE_LIST(IFACE)
                        DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                       SUMY * (XF(2, KFACE) - XK(2)) +
     .                       SUMZ * (XF(3, KFACE) - XK(3)) 
                        IF (DELTA > ZERO) THEN
                           LIMFACE(KFACE) = HALF * (MAXVAL_RHO - VALK) / DELTA
                        ELSE IF (DELTA < ZERO) THEN
                           LIMFACE(KFACE) = HALF * (MINVAL_RHO - VALK) / DELTA
                        ENDIF
                        CALL LIMITER(LIMFACE(KFACE), ONE)
                     ENDDO
                     LIM_RHO = MINVAL(LIMFACE(1:6))
                     MULTI_FVM%GRAD_RHO(1, I) = SUMX * LIM_RHO
                     MULTI_FVM%GRAD_RHO(2, I) = SUMY * LIM_RHO
                     MULTI_FVM%GRAD_RHO(3, I) = SUMZ * LIM_RHO
C     Pressure gradient
                     VALK = MULTI_FVM%EINT(I)
                     RHS(1:3) = ZERO
                     DO IFACE = 1, NB_FACE
                        KFACE = FACE_LIST(IFACE)
                        J = VOIS(KFACE)
                        VALL = VALK
                        IF (J > 0) THEN
                           VALL = MULTI_FVM%EINT(J)
                           MAXVAL_PRES = MAX(MAXVAL_PRES, VALL)
                           MINVAL_PRES = MIN(MINVAL_PRES, VALL)
                           RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                           RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                           RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))
                        ENDIF
                     ENDDO                  
                     SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                     SUMX = -SOL(1)
                     SUMY = -SOL(2)
                     SUMZ = -SOL(3)

C     pressure limitation
                     LIMFACE(1:6) = ONE
                     DO IFACE = 1, NB_FACE
                        KFACE = FACE_LIST(IFACE)
                        DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                       SUMY * (XF(2, KFACE) - XK(2)) +
     .                       SUMZ * (XF(3, KFACE) - XK(3)) 
                        IF (DELTA > ZERO) THEN
                           LIMFACE(KFACE) = HALF * (MAXVAL_PRES - VALK) / DELTA
                        ELSE IF (DELTA < ZERO) THEN
                           LIMFACE(KFACE) = HALF * (MINVAL_PRES - VALK) / DELTA
                        ENDIF
                        CALL LIMITER(LIMFACE(KFACE), ONE)
                     ENDDO
                     LIM_PRES = MINVAL(LIMFACE(1:6))
                     MULTI_FVM%GRAD_PRES(1, I) = SUMX * LIM_PRES
                     MULTI_FVM%GRAD_PRES(2, I) = SUMY * LIM_PRES
                     MULTI_FVM%GRAD_PRES(3, I) = SUMZ * LIM_PRES
                  ENDIF         ! MUSCL == 1
               ELSE
C     ==========
C     MULTIFLUID
C     ==========
                  DO IMAT = 1, NBMAT
C     Volume fraction gradient
                     VALK = MULTI_FVM%PHASE_ALPHA(IMAT, I)
                     RHS(1:3) = ZERO
                     DO IFACE = 1, NB_FACE
                        KFACE = FACE_LIST(IFACE)
                        J = VOIS(KFACE)
                        VALL = VALK
                        IF (J > 0) THEN
                           VALL = MULTI_FVM%PHASE_ALPHA(IMAT, J)
                           PHASE_MAXVAL_ALPHA(IMAT) = MAX(PHASE_MAXVAL_ALPHA(IMAT), VALL)
                           PHASE_MINVAL_ALPHA(IMAT) = MIN(PHASE_MINVAL_ALPHA(IMAT), VALL)
                           RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                           RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                           RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))
                        ENDIF
                     ENDDO
                     SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                     SUMX = -SOL(1)
                     SUMY = -SOL(2)
                     SUMZ = -SOL(3)                     
C     Volume fraction gradient limitation
                     LIMFACE(1:6) = ONE
                     DO IFACE = 1, NB_FACE
                        KFACE = FACE_LIST(IFACE)
                        DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                       SUMY * (XF(2, KFACE) - XK(2)) +
     .                       SUMZ * (XF(3, KFACE) - XK(3)) 
                        IF (DELTA > ZERO) THEN
                           LIMFACE(KFACE) = HALF * (PHASE_MAXVAL_ALPHA(IMAT) - VALK) / DELTA
                        ELSE IF (DELTA < ZERO) THEN
                           LIMFACE(KFACE) = HALF * (PHASE_MINVAL_ALPHA(IMAT) - VALK) / DELTA
                        ENDIF
                        CALL LIMITER(LIMFACE(KFACE), MULTI_FVM%BETA)
                     ENDDO
                     PHASE_LIM_ALPHA(IMAT) = MINVAL(LIMFACE(1:6))
                     MULTI_FVM%PHASE_GRAD_ALPHA(1, IMAT, I) = SUMX * PHASE_LIM_ALPHA(IMAT)
                     MULTI_FVM%PHASE_GRAD_ALPHA(2, IMAT, I) = SUMY * PHASE_LIM_ALPHA(IMAT)
                     MULTI_FVM%PHASE_GRAD_ALPHA(3, IMAT, I) = SUMZ * PHASE_LIM_ALPHA(IMAT)
C     Density gradient
                     IF (MULTI_FVM%MUSCL == 1) THEN
                        VALK = MULTI_FVM%PHASE_RHO(IMAT, I)
                        RHS(1:3) = ZERO
                        DO IFACE = 1, NB_FACE
                           KFACE = FACE_LIST(IFACE)
                           J = VOIS(KFACE)
                           VALL = VALK
                           IF (J > 0) THEN
                              VALL = MULTI_FVM%PHASE_RHO(IMAT, J)
                              PHASE_MAXVAL_RHO(IMAT) = MAX(PHASE_MAXVAL_RHO(IMAT), VALL)
                              PHASE_MINVAL_RHO(IMAT) = MIN(PHASE_MINVAL_RHO(IMAT), VALL)
                              RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                              RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                              RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))
                           ENDIF
                        ENDDO
                        SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                        SUMX = -SOL(1)
                        SUMY = -SOL(2)
                        SUMZ = -SOL(3) 
C     Density gradient limitation
                        LIMFACE(1:6) = ONE
                        DO IFACE = 1, NB_FACE
                           KFACE = FACE_LIST(IFACE)
                           DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                          SUMY * (XF(2, KFACE) - XK(2)) +
     .                          SUMZ * (XF(3, KFACE) - XK(3)) 
                           IF (DELTA > ZERO) THEN
                              LIMFACE(KFACE) = HALF * (PHASE_MAXVAL_RHO(IMAT) - VALK) / DELTA
                           ELSE IF (DELTA < ZERO) THEN
                              LIMFACE(KFACE) = HALF * (PHASE_MINVAL_RHO(IMAT) - VALK) / DELTA
                           ENDIF
                           IF (VALK + DELTA <= ZERO) THEN
                              LIMFACE(KFACE) = ZERO
                           ELSE
                              CALL LIMITER(LIMFACE(KFACE), ONE)
                           ENDIF
                        ENDDO
                        PHASE_LIM_RHO(IMAT) = MINVAL(LIMFACE(1:6))
                        MULTI_FVM%PHASE_GRAD_RHO(1, IMAT, I) = SUMX * PHASE_LIM_RHO(IMAT)
                        MULTI_FVM%PHASE_GRAD_RHO(2, IMAT, I) = SUMY * PHASE_LIM_RHO(IMAT)
                        MULTI_FVM%PHASE_GRAD_RHO(3, IMAT, I) = SUMZ * PHASE_LIM_RHO(IMAT)
C     Pressure gradient
                        VALK = MULTI_FVM%PHASE_EINT(IMAT, I)
                        RHS(1:3) = ZERO
                        DO IFACE = 1, NB_FACE
                           KFACE = FACE_LIST(IFACE)
                           J = VOIS(KFACE)
                           VALL = VALK
                           IF (J > 0) THEN
                              VALL = MULTI_FVM%PHASE_EINT(IMAT, J)
                              PHASE_MAXVAL_PRES(IMAT) = MAX(PHASE_MAXVAL_PRES(IMAT), VALL)
                              PHASE_MINVAL_PRES(IMAT) = MIN(PHASE_MINVAL_PRES(IMAT), VALL)
                              RHS(1) = RHS(1) + (VALK - VALL) * (XL(1, KFACE) - XK(1))
                              RHS(2) = RHS(2) + (VALK - VALL) * (XL(2, KFACE) - XK(2))
                              RHS(3) = RHS(3) + (VALK - VALL) * (XL(3, KFACE) - XK(3))
                           ENDIF
                        ENDDO
                        SOL(1:3) = MATMUL(MATM1(1:3, 1:3), RHS(1:3))
                        SUMX = -SOL(1)
                        SUMY = -SOL(2)
                        SUMZ = -SOL(3)
C     Pressure gradient limitation
                        LIMFACE(1:6) = ONE
                        DO IFACE = 1, NB_FACE
                           KFACE = FACE_LIST(IFACE)
                           DELTA = SUMX * (XF(1, KFACE) - XK(1)) +
     .                          SUMY * (XF(2, KFACE) - XK(2)) +
     .                          SUMZ * (XF(3, KFACE) - XK(3)) 
                           IF (DELTA > ZERO) THEN
                              LIMFACE(KFACE) = HALF * (PHASE_MAXVAL_PRES(IMAT) - VALK) / DELTA
                           ELSE IF (DELTA < ZERO) THEN
                              LIMFACE(KFACE) = HALF * (PHASE_MINVAL_PRES(IMAT) - VALK) / DELTA
                           ENDIF
                           CALL LIMITER(LIMFACE(KFACE), ONE)
                        ENDDO
                        PHASE_LIM_PRES(IMAT) = MINVAL(LIMFACE(1:6))
                        MULTI_FVM%PHASE_GRAD_PRES(1, IMAT, I) = SUMX * PHASE_LIM_PRES(IMAT)
                        MULTI_FVM%PHASE_GRAD_PRES(2, IMAT, I) = SUMY * PHASE_LIM_PRES(IMAT)
                        MULTI_FVM%PHASE_GRAD_PRES(3, IMAT, I) = SUMZ * PHASE_LIM_PRES(IMAT)
                     ENDIF      ! MUSCL == 1
                  ENDDO         ! IMAT
               ENDIF            ! NBMAT
            ENDDO               !II = 1, NEL
         ENDIF                  !MATLAW == 151
      ENDDO                     !NG
      IF(ITASK == 0) CALL STOPTIME(MACRO_TIMER_MUSCL,1)

      END SUBROUTINE MULTI_MUSCL_GRADIENTS

Chd|====================================================================
Chd|  LIMITER                       source/multifluid/multi_muscl_gradients.F
Chd|-- called by -----------
Chd|        MULTI_MUSCL_GRADIENTS         source/multifluid/multi_muscl_gradients.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LIMITER(PHI, BETA)
#include      "implicit_f.inc"
      my_real, INTENT(INOUT) :: PHI
      my_real, INTENT(IN) :: BETA
      my_real :: LIM
      
      LIM = MAX(ZERO, MAX(MIN(ONE, BETA * PHI), MIN(PHI, BETA)))
      PHI = LIM
      END SUBROUTINE LIMITER
