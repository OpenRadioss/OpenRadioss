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
Chd|  INIGRAV_EOS                   source/initial_conditions/inigrav/inigrav_eos.F
Chd|-- called by -----------
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|-- calls ---------------
Chd|        EOSMAIN                       ../common_source/eos/eosmain.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INIGRAV_EOS(NELG, NEL, NG, MATID, IPM, GRAV0, RHO0, DEPTH, PM, BUFMAT,
     .                       ELBUF_TAB,PSURF,LIST,PRES,IMAT,MLW,NPF,TF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)    :: NEL, NG, MATID, IPM(NPROPMI, *), LIST(NEL),NELG, IMAT, MLW
      my_real, INTENT(IN)    :: GRAV0, DEPTH(*), PM(NPROPM, *), BUFMAT(*),PSURF
      my_real, INTENT(INOUT) :: PRES(NEL)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET, INTENT(IN) :: ELBUF_TAB
      my_real, INTENT(IN) :: RHO0
      INTEGER,INTENT(IN)::NPF(SNPC)
      my_real,INTENT(IN)::TF(STF)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,K
      INTEGER :: EOSTYPE, ITER, MAX_ITER, MAT(NEL), REMAINING_ELTS, NBMAT
      my_real :: TOL, INCR
      my_real :: FUNC(NEL), DFUNC(NEL), ERROR(NEL), RHOZERO(NEL), RHO(NEL),
     .           MU(NEL), MU2(NEL), ESPE(NEL), DVOL(NEL), DF(NEL), VOL(NEL), PSH(NEL),
     .           DPDRHO(NEL), DPDM(NEL), DPDE(NEL), THETA(NEL), ECOLD(NEL), P0(NEL),
     .           GRUN(NEL),PREF(NEL), OFF(NEL), EINT(NEL), SIG(NEL,6), POLD(NEL),RHO_OLD,V0(NEL)

      LOGICAL :: CONT, CONVERGED(NEL)
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_), POINTER :: LBUF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C LIST IS SUBGROUP TO TREAT : ONLY ELEM WITH RELEVANT PARTS ARE KEPT
C NEL IS ISEZ OF LIST
C NELG IS SIZE OF ORIGINAL GROUP : needed to shift indexes in GBUF%SIG & MBUF%VAR
C IMAT : relevant for multifluid eos. imat=0 => monomaterial law

      GBUF => ELBUF_TAB(NG)%GBUF
      NULLIFY(LBUF)
      IF(IMAT>0)THEN
        LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
      ENDIF

      EOSTYPE          = IPM(4, MATID)
      ITER             = 0
      MAX_ITER         = 30
      CONT             =.TRUE.
      ERROR(1:NEL)     = EP30
      TOL              = EM4
      CONVERGED(1:NEL) =.FALSE.

      !===================================================================!
      !                     BUFFER INITIALIZATION                         !
      !===================================================================!
      ! local array instead of pointers since they may be reshaped with a
      ! subgroup (1:NEL) c (1:NELG) depending on element PART (possibly several PARTS in a given group)
      DO K = 1, NEL
           RHOZERO(K) = PM(1,MATID)
           PSH(K)     = PM(88, MATID)
           MAT(K)     = MATID
           PREF(K)    = PSURF - RHO0 * GRAV0 * DEPTH(K)   !RHO in : RHO0_mixture (gravity model applied to the mixture not to the single submaterial)
           VOL(K)     = ONE   !  Real volume value not useful here, just an indicator : ONE = compute, ZERO = do not compute
           MU(K)      = ZERO
           SIG(K,1)   = -PM(31,MATID)
           SIG(K,2)   = -PM(31,MATID)
           SIG(K,3)   = -PM(31,MATID)
           SIG(K,4)   = ZERO
           SIG(K,5)   = ZERO
           SIG(K,6)   = ZERO
      ENDDO

      IF(IMAT==0)THEN   ! WORK IN GBUF
        DO K = 1, NEL
            I         = LIST(K)
           RHO(K)     = GBUF%RHO(K)
           OFF(K)     = GBUF%OFF(I)
           EINT(K)    = GBUF%EINT(I)
           V0(K)      = GBUF%VOL(I)
        ENDDO
      ELSE ! THEN IMAT>0    WORK IN LBUF
        DO K = 1, NEL
           I          = LIST(K)
           RHO(K)     = PM(1,MATID) !submaterial
           OFF(K)     = LBUF%OFF(I)
           EINT(K)    = LBUF%EINT(I)
           V0(K)      = LBUF%VOL(I)
        ENDDO
      ENDIF
      !EINT=Eint/V in this context with law 151
      !===================================================================!
      !                     ITERATIONS                                    !
      !===================================================================!
      POLD(1:NEL)=PRES(1:NEL)
      REMAINING_ELTS = NEL                      !Number of elements for which convergence is not achieved
      DO WHILE(CONT .AND. ITER < MAX_ITER)   !Start nonlinear solver loop
         ITER = ITER + 1
         DO K = 1, NEL
            MU(K) = RHO(K) / RHOZERO(K) - ONE
            DF(K) = RHOZERO(I) / RHO(K)
            EINT(K) = EINT(K) / DF(K) !with IFLAG==2 we need ESPE=Df*EINT
         ENDDO
         CALL EOSMAIN(2     , NEL    , EOSTYPE, PM   , OFF  , EINT,
     .                RHO   , RHOZERO, MU     , MU2  , ESPE ,
     .                DVOL  , DF     , VOL    , MAT  , PSH  ,
     .                PRES  , DPDM   , DPDE   , THETA, ECOLD,
     .                BUFMAT, SIG    , MU     , MLW  , POLD ,
     .                NPF   , TF)
          DO K=1,NEL
            EINT(K)=EINT(K)*DF(K) ! set back to Eint/V
          ENDDO
         !===================================================================!
         !                     CHECK CONVERGENCY                             !
         !===================================================================!
         !loop over retained element in the current group I \in LIST(1:NEL)
         DO K = 1, NEL
            IF (.NOT. CONVERGED(K)) THEN   ! No need to take the square root, only the squared sound speed is needed here
               DPDRHO(K) = DPDM(K) / RHOZERO(K)  !DPDM is total derivative (variation along isentropic path)
               !GRUN(K) = DPDE(K) / (ONE + MU(K))
               FUNC(K) = PRES(K)-PREF(K)
               DFUNC(K) = DPDRHO(K) !- GRUN(K) * PRES(K) / RHO(K)
               INCR = - FUNC(K) / DFUNC(K)
               RHO_OLD = RHO(K)
               RHO(K) = RHO(K) + INCR
               ERROR(K)= MAX( ABS(INCR)/RHOZERO(K) , ABS(FUNC(K)) )

               INCR =   - (PRES(K)+PSH(K)+HALF*DPDRHO(K)*INCR)*(RHOZERO(K)/RHO(K)-RHOZERO(K)/RHO_OLD)
               EINT(K) = EINT(K) + INCR   !EINT is in fact Eint/V with law 151 here
               IF(DPDE(K)==ZERO)EINT(K)=ZERO

               IF (ERROR(K) < TOL .OR. ITER == MAX_ITER) THEN
                  CONVERGED(K) = .TRUE.
                  REMAINING_ELTS = REMAINING_ELTS - 1
                  VOL(K) = ZERO
                  I = LIST(K)
                  DF(K) = RHOZERO(I) / RHO(K)
                  IF(IMAT==0)THEN
                    GBUF%RHO(I) = RHO(K)
                    GBUF%SIG(I           ) = - PRES(K)
                    GBUF%SIG(I +     NELG) = - PRES(K)
                    GBUF%SIG(I + 2 * NELG) = - PRES(K)
                    GBUF%EINT(I) = EINT(K)/DF(K)     !Eint/V with law 151 here     => EINT*DF == Eint/V0
                  ELSE
                    LBUF%RHO(I) = RHO(K)
                    LBUF%SIG(I           ) = - PRES(K)
                    LBUF%SIG(I +     NELG) = - PRES(K)
                    LBUF%SIG(I + 2 * NELG) = - PRES(K)
                    LBUF%EINT(I) = EINT(K)/DF(K)     !Eint/V with law 151 here
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         CONT = (REMAINING_ELTS /= 0)
      ENDDO                     ! WHILE


      END SUBROUTINE INIGRAV_EOS
