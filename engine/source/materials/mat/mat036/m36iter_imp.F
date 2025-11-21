Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    m36iter_imp   ../engine/source/materials/mat/mat036/m36iter_imp.F
!||--- called by ------------------------------------------------------
!||    sigeps36      ../engine/source/materials/mat/mat036/sigeps36.F
!||--- calls      -----------------------------------------------------
!||    finter2       ../engine/source/tools/curve/vinter.F
!||    imp_stop      ../engine/source/implicit/imp_solv.F
!||    vinter2       ../engine/source/tools/curve/vinter.F
!||    vinter2dp     ../engine/source/tools/curve/vinter.F
!||====================================================================
      SUBROUTINE M36ITER_IMP(NEL   ,G3    ,H     ,YLD   ,YFAC ,PLA  ,
     .                       FUNC_Y ,NVARTMP,VARTMP,DPLA1 ,FISOKIN,
     .                       SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX,
     .                       SIGBXX,SIGBYY,SIGBZZ,SIGBXY,SIGBYZ,SIGBZX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
          USE TABLE4D_MOD
          use table_mat_vinterp_mod
c--------------------------------------------------------------
c
c      This subroutine computes plastic strain rate multiplier
c      "delta_lambda" for the von Mises material characterized by
c      piecewise-linear hardening.
c      In addition, it computes the deviatoric stresses
c      via radial return.
c
c    -------------------
c    -- parameters in --
c    -------------------
c
c    SIGNXX  = Elastic Predictor  (on input)
c              (XX-Component Deviatoric Stress Tensor)
c              (...)
c    SIGNZX  = Elastic Predictor  (on input)
c              (ZX-Component Deviatoric Stress Tensor)
c
c    PLA     = previous time-step equivalent plastic strain  (on input)
c    YLD     = previous time-step yield stress               (on input)
c    G3      = 3 x Kirchhoff's modulus (elastic shear modulus))
c    YFAC    = scaling factor of sigma-eps_plast relation
c
c    NEL     = size of the element group being processed
c    FISOKIN = indicator for isotropic/kinematic/moixed hardening
c
c    --------------------
c    -- parameters out --      
c    --------------------
c
c    DPLA1   = equivalent plastic strain increment
c    PLA     = updated equivalent plastic strain (on output)
c    H       = updated hardening parameter
c    SIGNXX  = updated XX-component of deviatoric stress tensor 
c              (...)
c    SIGNZX  = updated ZX-component of deviatoric stress tensor, 
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
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER :: NEL,NVARTMP
      INTEGER :: VARTMP(NEL,NVARTMP)
      my_real :: FISOKIN
      my_real :: PLA(NEL),DPLA1(NEL),G3(NEL),YLD(NEL),H(NEL)
      my_real :: YFAC(NEL)
      my_real :: SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL)
      my_real :: SIGBXX(NEL),SIGBYY(NEL),SIGBZZ(NEL),SIGBXY(NEL),SIGBYZ(NEL),SIGBZX(NEL)
      TYPE(TABLE_4D_) ,INTENT(IN) :: FUNC_Y
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NI,ITER,NITER
      my_real :: XSI,DXSI,LHS,RHS,ALPHA_RADIAL,VM,HEP,DPLA,Y_SCAL
      my_real :: sxx, syy, szz, sxy, syz, szx
      my_real :: YLD_M_PREV(MVSIZ),H_M_PREV(MVSIZ),YLD_PREV(MVSIZ),YLD_CURR(MVSIZ)
      my_real :: PLA0(MVSIZ),PLAM(MVSIZ),DYDX1(MVSIZ),DFDP(MVSIZ)
      my_real :: YLD_M(MVSIZ),H_M(MVSIZ)
      my_real ,dimension(1) :: fval,fderi
      my_real ,dimension(nel,1) :: xvec1
C=======================================================================
c
c ---- max number of iterations ----
       NITER = 20   
c
c    -- store equivalent plastic strain at the beginning of time-step
c    -- store "fractional" equivalent plastic strain
c
       DO I=1,NEL
         PLA0(I) = PLA(I)
         PLAM(I) = (ONE - FISOKIN) * PLA(I) 
       ENDDO
c
c      -- computes the yield stress --
c
       xvec1(1,1) = plam(i)
       call table_mat_vinterp(func_y,nel,nel,vartmp,xvec1,yld,dydx1)
       YLD(1:nel)  = MAX(YLD(1:nel),EM20)
c
       DO 100 I = 1, NEL ! .. loop on a group of elements ..
c
c     ... temporarily stores elastic predictor
          sxx = SIGNXX(I)
          syy = SIGNYY(I)
          szz = SIGNZZ(I)
          sxy = SIGNXY(I)
          syz = SIGNYZ(I)
          szx = SIGNZX(I)
c
            XSI = ZERO ! .. plastic strain rate multiplier ..
c
c          -- PREVIOUS TIME-STEP EQUIVALENT PLASTIC STRAIN
            PLA(I) = MAX(ZERO, PLA(I))
c
c          -- von Mises stress at elastic predictor --
            VM =THREE*(HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     $                        +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2)
            VM =SQRT(VM)
c
c
c          -- COMPARE VM with PREVIOUS TIME-STEP YIELD STRESS --
c          -- YLD is already multiplied by PFAC (at pressure)
c
            IF (VM > YLD(I) )then ! .. plastically active process
c
c          -- scaling for sigma-eps_plast function --
              Y_SCAL = YFAC(I)
c
c            ------------------------------------------------------
c            yield stress and hardening parameter at m*(eps)
c            m =: (1 - FISOKIN)
c            ------------------------------------------------------
c          -- compute yield stress and hardening parameter DYDX1
c             at equivalent plastic strain PLAM, via interpolation
c
c          -- yield stress at m*(eps)  --+

              ni = 1
              xvec1(1,1) = plam(i)
              call table_mat_vinterp(func_y,ni,ni,vartmp,xvec1,fval,fderi)
              YLD_M_PREV(i) = fval(1)
              H_M_PREV(I)   = fderi(1)
c
c          -- apply scaling --
              YLD_M_PREV(I) = YLD_M_PREV(I) * Y_SCAL 
              YLD_M_PREV(I) = MAX(YLD_M_PREV(I),EM20)
              H_M_PREV(I)   = H_M_PREV(I) * Y_SCAL
c          -- check if H is nonnegative and y0 positive
              if (.not.( H_M_PREV(I) >= ZERO ).or. .not.( YLD_M_PREV(I)  > ZERO ) )then 
               write(ISTDO,1000)PLAM(I),H_M_PREV(I),YLD_M_PREV(I)
               write(IOUT,1000)PLAM(I),H_M_PREV(I),YLD_M_PREV(I)
               CALL IMP_STOP(-1)
              endif
c
c            -------------------
c            yield stress at eps
c            -------------------
c
              xvec1(1,1) = pla0(i)
              call table_mat_vinterp(func_y,ni,ni,vartmp,xvec1,fval,fderi)
              YLD_PREV(I) = fval(1) * Y_SCAL
              YLD_PREV(I) = MAX(YLD_PREV(I),EM20)
c
c          -- check if H is nonnegative and y0 positive
              if( .not.( fderi(1) >= ZERO ).or. .not.( YLD_PREV(I)  > ZERO ) )then
                write(ISTDO,1000)PLA0(I),fderi(1),YLD_PREV(I)
                write(IOUT,1000)PLA0(I),fderi(1),YLD_PREV(I)
                CALL IMP_STOP(-1)
              endif
c
c          =================
c            NR iterations
c          =================
c
            DO 80 ITER = 1,NITER 
c
c            ---------------------------------------------------
c            yield stress and hardening parameter at eps + deps
c            ---------------------------------------------------
C          -- compute yield stress and hardening parameter DYDX1
c             at equivalent plastic strain PLA, via interpolation
c
c          -- yield stress at eps + deps --
c
              xvec1(1,1) = pla(i)
              call table_mat_vinterp(func_y,ni,ni,vartmp,xvec1,fval,fderi)
c
              YLD_CURR(I) =  fval(1)
              H(I) = fderi(1)
c
c          -- apply scaling --
              YLD_CURR(I) =  YLD_CURR(I) * Y_SCAL
              H(I) = H(I) * Y_SCAL
              YLD_CURR(I) =  MAX(YLD_CURR(I),EM20)
c
c          -- check if H is nonnegative and y0 positive
              if( .not.( H(I) >= ZERO ).or.
     $            .not.( YLD_CURR(I)  > ZERO ) )then
               write(ISTDO,1000)PLA(I),H(I),YLD_CURR(I)
               write(IOUT,1000)PLA(I),H(I),YLD_CURR(I)
               CALL IMP_STOP(-1)
              endif
c
c
c            -----------------------------------------
c            RHS and LHS for Newton at the Gauss point
c            -----------------------------------------
c
              IF(FISOKIN == ZERO)then
                 RHS = VM - G3(I) * XSI - YLD_CURR(I)
                 LHS = G3(I) +  H(I)
              ELSEIF(FISOKIN == ONE)then
                 RHS = VM - G3(I)*XSI + YLD_PREV(I) - YLD_CURR(I) - YLD_M_PREV(I)
                 LHS = G3(I) +  H(I)
              ELSE
                 RHS = VM - G3(I)*XSI + YLD_PREV(I) - YLD_CURR(I) - YLD_M_PREV(I)
                 LHS = G3(I) +  H(I)
              ENDIF    
c
c
c            --------------------------------------
c            Solution for Newton at the Gauss point
c            --------------------------------------
              DXSI = RHS/LHS
C          -- update plastic strain rate multiplier
              XSI = XSI + DXSI
C          -- update equivalent plastic strain --      
              PLA(I) =  PLA(I) + DXSI
              PLA(I) =  MAX(ZERO,PLA(I)) !..to prevent negative value   
c
c
c          -- convergence criterion should be stringent --           
              IF( ABS(DXSI)<EM10 ) GO TO 90
c
 80         CONTINUE ! -- ITER --
c          --  we need a warning here --
ccc            WRITE(*,*)
ccc     $     'M36ITER_IMP--NON-CONVERGED ITERATION', ABS(DXSI),ABS(RHS)
c
 90         CONTINUE
c
c       ... equivalent plastic strain increment
            DPLA = MAX(ZERO,XSI) !..to prevent negative value 
c
c
c
c          --------------------------------------
c           update of stresses and back stresses
c          --------------------------------------
c
            IF (FISOKIN == ZERO) then
              ALPHA_RADIAL =  ONE - ( G3(I)/MAX(VM,EM20) )*DPLA
c
              SIGNXX(I) = SIGNXX(I) * ALPHA_RADIAL
              SIGNYY(I) = SIGNYY(I) * ALPHA_RADIAL
              SIGNZZ(I) = SIGNZZ(I) * ALPHA_RADIAL
              SIGNXY(I) = SIGNXY(I) * ALPHA_RADIAL
              SIGNYZ(I) = SIGNYZ(I) * ALPHA_RADIAL
              SIGNZX(I) = SIGNZX(I) * ALPHA_RADIAL
c
            ELSE
c
c          -- update "fractional" equivalent plastic strain --  
              PLAM(I) = (ONE - FISOKIN)*PLA(I) !. for mixed hardeneing
c
c
c            ------------------------------------------------------
c            yield stress and hardening parameter at m*(eps+deps)
c            m =: (1 - FISOKIN)
c            ------------------------------------------------------
c          -- compute yield stress and hardening parameter DYDX1
c             at equivalent plastic strain PLAM, via interpolation
c
c          -- yield stress at m*(eps+deps)  --
              xvec1(1,1) = plam(i)
              call table_mat_vinterp(func_y,ni,ni,vartmp,xvec1,fval,fderi)

              H_M(I)   = fderi(1) * Y_SCAL
              YLD_M(I) = fval(1)  * Y_SCAL 
              YLD_M(I) = MAX(YLD_M(I),EM20)
c
c          -- check if H is nonnegative and y0 positive
              if( .not.( H_M(I) >= ZERO ).or.
     $            .not.( YLD_M(I)  > ZERO ) )then 
               write(ISTDO,1000)PLAM(I),H_M(I),YLD_M(I)
               write(IOUT,1000)PLAM(I),H_M(I),YLD_M(I)
               CALL IMP_STOP(-1)
              endif
c
c           ..updates (shifted) stresses
c
c          -- ALPHA_RADIAL := PARAMETER ALPHA OF THE RADIAL RETURN
c
              ALPHA_RADIAL =  ONE - ( G3(I)/MAX(VM,EM20) )*DPLA
     $        - (YLD_CURR(I) - YLD_PREV(I) -YLD_M(I)  + YLD_M_PREV(I) )/MAX(VM,EM20)
c
              SIGNXX(I) = SIGNXX(I) * ALPHA_RADIAL
              SIGNYY(I) = SIGNYY(I) * ALPHA_RADIAL
              SIGNZZ(I) = SIGNZZ(I) * ALPHA_RADIAL
              SIGNXY(I) = SIGNXY(I) * ALPHA_RADIAL
              SIGNYZ(I) = SIGNYZ(I) * ALPHA_RADIAL
              SIGNZX(I) = SIGNZX(I) * ALPHA_RADIAL
c
c           ..updates back stresses
              HEP =  ( YLD_CURR(I) - YLD_PREV(I)
     $                            -YLD_M(I)  + YLD_M_PREV(I)  )/MAX(VM,EM20)
c
              SIGBXX(I)  = SIGBXX(I)  + sxx *HEP
              SIGBYY(I)  = SIGBYY(I)  + syy *HEP
              SIGBZZ(I)  = SIGBZZ(I)  + szz *HEP
              SIGBXY(I)  = SIGBXY(I) + sxy *HEP 
              SIGBYZ(I)  = SIGBYZ(I) + syz *HEP
              SIGBZX(I)  = SIGBZX(I) + szx *HEP  
C
c           --adds the back stresses  to shifted stresses
             SIGNXX(I) = SIGNXX(I) + SIGBXX(I) 
             SIGNYY(I) = SIGNYY(I) + SIGBYY(I) 
             SIGNZZ(I) = SIGNZZ(I) + SIGBZZ(I) 
             SIGNXY(I) = SIGNXY(I) + SIGBXY(I)
             SIGNYZ(I) = SIGNYZ(I) + SIGBYZ(I) 
             SIGNZX(I) = SIGNZX(I) + SIGBZX(I) 
c
            ENDIF ! ..FISOKIN > ZERO.and.FISOKIN<=ONE .. 
c
c
c        .. updates yield stress.
            IF(FISOKIN == ZERO)then
              YLD(I) = YLD_CURR(I)   
            ELSEIF(FISOKIN == ONE)then
cccc              YLD(I) =  YLD(I)
            ELSE
              YLD(I) =  YLD_M(I)
            ENDIF
c
            DPLA1(I)  = DPLA !--PLASTIC STRAIN MULTIPLIER "delta lambda"

c  
c
c           --- end of plastically active process ---
c
            ELSE !.. elasticity

c
c        .. gets stresses from shifted stresses and back stresses
            IF(FISOKIN > ZERO)then 
             SIGNXX(I) = SIGNXX(I) + SIGBXX(I) 
             SIGNYY(I) = SIGNYY(I) + SIGBYY(I) 
             SIGNZZ(I) = SIGNZZ(I) + SIGBZZ(I) 
             SIGNXY(I) = SIGNXY(I) + SIGBXY(I)
             SIGNYZ(I) = SIGNYZ(I) + SIGBYZ(I) 
             SIGNZX(I) = SIGNZX(I) + SIGBZX(I) 
            ENDIF

            ENDIF ! .. VM > YLD(I)
c
c
 100  CONTINUE  !..LOOP ON A GROUP OF ELEMENTS
c
 1000 FORMAT(3X,'Hardening parameters in law36 is not positive'/,
     $                   2X,'Eps_plast =',E11.4,2X,'H =',E11.4,
     $                   2X,'Sy0 =',E11.4)
c-----------
      RETURN
      END
