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
Chd|  M36ITER_IMP                   source/materials/mat/mat036/m36iter_imp.F
Chd|-- called by -----------
Chd|        SIGEPS36                      source/materials/mat/mat036/sigeps36.F
Chd|-- calls ---------------
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        VINTER2DP                     source/tools/curve/vinter.F   
Chd|        FINTER2                       source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE M36ITER_IMP( SIGNXX,  SIGNYY,  SIGNZZ,
     $                        SIGNXY,  SIGNYZ,  SIGNZX,
     $                        PLA,        YLD,      G3,
     $                        YFAC,     DPLA1,       H,   
     $                        TF,        IAD1,  
     $                        ILEN1,     NEL,
     $                        FISOKIN,  VARTMP, PLA0, 
     $                        PLAM    ,Y1,  DYDX1,
     $                        IPFLAG, PFUN, IPFUN, IPOSP, 
     $                        NRATE,  NPF,  IADP,  ILENP,
     $                        PFAC, PSCALE1, DFDP, FAIL , 
     A                        NVARTMP ,
     B                        SIGBXX,SIGBYY,SIGBZZ,SIGBXY,SIGBYZ,SIGBZX)
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
c    TF      = array containing discrete sigma-eps_plast relation
c    IAD1,  ILEN1 = pre-computed data required by interpolation
c                         routine "VINTER"
c    NEL     = size of the element group being processed
c    FISOKIN = indicator for isotropic/kinematic/moixed hardening
c    UVAR    = array containing various state variables 
c    IPFLAG  = flag for existence of pressure scaling (scalar)
c    PFUN    = flag for existence of pressure scaling (array)
c    PFAC    = scaling factor for pressure at time t (not needed)
c    PSCALE1 = pressure at time t + delta_t
c    IPOSP, ILENP, NRATE,  NPF,  IADP = arrays and parameters required
c              by pressure interpolating routine (for pressure scaling)
c
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
c    UVAR    = array containing various state variables
c
c    -----------------
c    -- work arrays --      
c    -----------------
c
c    Y1     =  yield stress  
c    DYDX1  =  derivative in sig0-eps_plast curve
c
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
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER  NEL, NVARTMP
      INTEGER  IAD1(MVSIZ),  ILEN1(MVSIZ),
     .         IPFLAG, PFUN,  IPFUN, IPOSP(MVSIZ),
     .          IADP(MVSIZ), ILENP(MVSIZ), NRATE, NPF(*)
      INTEGER :: VARTMP(NEL,NVARTMP)
C
C     REAL
      my_real
     .   SIGNXX(*),  SIGNYY(*),  SIGNZZ(*),
     .   SIGNXY(*),  SIGNYZ(*),  SIGNZX(*), 
     .   PLA(*),         G3(*),     YLD(*),
     .   DPLA1(*),        H(*),
     .   TF(*),          Y1(*),   DYDX1(*),
     .   YFAC(MVSIZ,*),
     .   PLA0(MVSIZ), PLAM(MVSIZ),
     .   FISOKIN,
     .   PFAC(MVSIZ),  PSCALE1(MVSIZ), DFDP(MVSIZ),
     .   FAIL(MVSIZ)
      my_real
     .   SIGBXX(NEL),SIGBYY(NEL),SIGBZZ(NEL),SIGBXY(NEL),SIGBYZ(NEL),SIGBZX(NEL)
CC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITER, NITER, I_BILIN, IPOS
C
C     REAL
      MY_REAL
     .   XSI, DXSI, LHS, RHS, ALPHA_RADIAL, VM, HEP, R, DPLA,
     .   YLD_CURR, YLD_PREV, YLD_M, H_M, P_SCAL, Y_SCAL,
     .   TOTAL_SCAL, YLD_M_PREV, H_M_PREV,
     .   sxx, syy, szz, sxy, syz, szx
     
C
C-----------------------------------------------
C   External Functions
C-----------------------------------------------
C
      my_real
     .   FINTER2
      EXTERNAL FINTER2
C
C=======================================================================
C
C
c ---- hardening law flag: i_bilin = 1 -- bilinear hardening law
c ----                     i_bilin = 0 -- general nonlinear hardening
c
       i_bilin = 0
c
c ---- max number of iterations ----
       NITER = 20   
c
c    -- scaling if pressure dependent yield function --
c    -- compute the scaling factor for pressure + delta-pressure
c
        DO I=1,NEL
          PFAC(I)  =  ONE
        ENDDO
c
        IF (IPFLAG == NEL.AND.(IMACH/=3.OR.IPARIT == 0)) THEN
          DO I=1,NEL
            IPOSP(I) = VARTMP(I,2)
            IADP(I)  = NPF(IPFUN) / 2 + 1
            ILENP(I) = NPF(IPFUN + 1) / 2 - IADP(I) - IPOSP(I)
          ENDDO
          IF (IRESP == 1) THEN
            CALL VINTER2DP(TF,IADP,IPOSP,ILENP,NEL,PSCALE1,DFDP,PFAC)
          ELSE
            CALL VINTER2(TF,IADP,IPOSP,ILENP,NEL,PSCALE1,DFDP,PFAC)
          ENDIF
          PFAC(I) = MAX(ZERO, PFAC(I))
        ELSEIF (IPFLAG  >  0) THEN
          IF (PFUN  >  0) THEN
            DO I=1,NEL
              IPOSP(I) = VARTMP(I,2)
              IADP(I)  = NPF(IPFUN) / 2 + 1
              ILENP(I) = NPF(IPFUN  + 1) / 2 - IADP(I) - IPOSP(I)
              PFAC(I)  = FINTER2(TF   ,IADP(I),IPOSP(I),ILENP(I),
     .                         PSCALE1(I),DFDP(I))
              PFAC(I) = MAX(ZERO, PFAC(I))
            ENDDO
          ENDIF
        ENDIF ! .. IPFLAG ...
c    -- end of computation of pressure-scaling factor --
c
c
c    -- store eqivalent plastic strain at the beginning of time-step
c    -- store "fractional" eqivalent plastic strain
c
       DO I=1,NEL
         PLA0(I) = PLA(I)
         PLAM(I) = (ONE - FISOKIN) * PLA(I) 
       ENDDO
c
c      -- computes the yield stress --
c
       DO I=1,NEL
C  2Andrzej--------!!!IPOS is not initialized-----       
          IPOS = 0
          YLD(I) = FINTER2( TF, IAD1(I), IPOS, ILEN1(I),
     $                         PLAM(I),DYDX1(I) )
C------------------same than explicit
          YLD(I)  = MAX(YLD(I),EM20)
       ENDDO
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
          IF(i_bilin/=1 )THEN !..  general, nonlinear hardening
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
c          -- YLD is already muliplied by PFAC (at pressure)
c
            IF( VM > YLD(I) )then ! .. plastically active process
c
c
c          -- scaling for sigma-eps_plast function --
              Y_SCAL = YFAC(I,1)
c
c          -- scaling if pressure dependent yield function --
              P_SCAL = PFAC(I) * FAIL(I) 
c
c          -- total scaling
              TOTAL_SCAL = Y_SCAL * P_SCAL
c
c            ------------------------------------------------------
c            yield stress and hardening parameter at m*(eps)
c            m =: (1 - FISOKIN)
c            ------------------------------------------------------
c          -- compute yield stress and hardening parameter DYDX1
c             at equivalent plastic strain PLAM, via interpolation
c
              IPOS = 0 !.. FINTER2 will search the whole table
c
c          -- yield stress at m*(eps)  --
              YLD_M_PREV = FINTER2( TF, IAD1(I), IPOS, ILEN1(I),
     $                         PLAM(I),DYDX1(I) )
c
c          -- hardening parameter --
              H_M_PREV  = DYDX1(I)
c
c          -- apply scaling --
              YLD_M_PREV =  YLD_M_PREV * TOTAL_SCAL 
              H_M_PREV  = H_M_PREV * TOTAL_SCAL
              YLD_M_PREV  = MAX(YLD_M_PREV,EM20)
c          -- check if H is nonnegative and y0 positive
              if( .not.( H_M_PREV >= ZERO ).or.
     $            .not.( YLD_M_PREV  > ZERO ) )then 
               write(ISTDO,1000)PLAM(I),H_M_PREV,YLD_M_PREV
               write(IOUT,1000)PLAM(I),H_M_PREV,YLD_M_PREV
               CALL IMP_STOP(-1)
              endif
c
c            -------------------
c            yield stress at eps
c            -------------------
c
              IPOS = 0 !.. FINTER2 will search the whole table
c
c          -- yield stress at  eps --
c
              YLD_PREV  = FINTER2( TF, IAD1(i), IPOS, ILEN1(i),
     $                             PLA0(I),DYDX1(I) )
c
c          -- apply scaling --
              YLD_PREV =  YLD_PREV * TOTAL_SCAL
              YLD_PREV =  MAX(YLD_PREV,EM20)
c
c          -- check if H is nonnegative and y0 positive
              if( .not.( DYDX1(I) >= ZERO ).or.
     $            .not.( YLD_PREV  > ZERO ) )then
               write(ISTDO,1000)PLA0(I),DYDX1(I),YLD_PREV
               write(IOUT,1000)PLA0(I),DYDX1(I),YLD_PREV
               CALL IMP_STOP(-1)
              endif
c
c
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
              IPOS = 0 !.. FINTER2 will search the whole table
c
c          -- yield stress at eps + deps --
c
              YLD_CURR = FINTER2( TF,IAD1(i), IPOS, ILEN1(i),
     $                            PLA(I),DYDX1(I) )
c
c          -- hardening parameter --
              H(I)  = DYDX1(I)
c
c          -- apply scaling --
              YLD_CURR =  YLD_CURR * TOTAL_SCAL
              H(I) = H(I) * TOTAL_SCAL
              YLD_CURR =  MAX(YLD_CURR,EM20)
c
c          -- check if H is nonnegative and y0 positive
              if( .not.( H(I) >= ZERO ).or.
     $            .not.( YLD_CURR  > ZERO ) )then
               write(ISTDO,1000)PLA(I),H(I),YLD_CURR
               write(IOUT,1000)PLA(I),H(I),YLD_CURR
               CALL IMP_STOP(-1)
              endif
c
c
c            -----------------------------------------
c            RHS and LHS for Newton at the Gauss point
c            -----------------------------------------
c
              IF(FISOKIN == ZERO)then
                 RHS = VM - G3(I) * XSI - YLD_CURR
                 LHS = G3(I) +  H(I)
              ELSEIF(FISOKIN == ONE)then
                 RHS = VM - G3(I)*XSI + YLD_PREV - YLD_CURR - YLD_M_PREV
                 LHS = G3(I) +  H(I)
              ELSE
                 RHS = VM - G3(I)*XSI + YLD_PREV - YLD_CURR - YLD_M_PREV
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
            IF(FISOKIN == ZERO)then
              ALPHA_RADIAL =  ONE - ( G3(I)/MAX(VM,EM20) )*DPLA
c
              SIGNXX(I) = SIGNXX(I) * ALPHA_RADIAL
              SIGNYY(I) = SIGNYY(I) * ALPHA_RADIAL
              SIGNZZ(I) = SIGNZZ(I) * ALPHA_RADIAL
              SIGNXY(I) = SIGNXY(I) * ALPHA_RADIAL
              SIGNYZ(I) = SIGNYZ(I) * ALPHA_RADIAL
              SIGNZX(I) = SIGNZX(I) * ALPHA_RADIAL
c
            ELSEIF(FISOKIN > ZERO.and.FISOKIN<=ONE)then
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
              IPOS = 0 !.. FINTER2 will search the whole table
c
c          -- yield stress at m*(eps+deps)  --
              YLD_M = FINTER2( TF, IAD1(i), IPOS, ILEN1(i),
     $                         PLAM(I),DYDX1(I) )
c
c          -- hardening parameter --
              H_M  = DYDX1(I)
c          -- apply scaling --
              YLD_M =  YLD_M * TOTAL_SCAL 
              H_M  = H_M * TOTAL_SCAL
              YLD_M =  MAX(YLD_M,EM20)
c
c          -- check if H is nonnegative and y0 positive
              if( .not.( H_M >= ZERO ).or.
     $            .not.( YLD_M  > ZERO ) )then 
               write(ISTDO,1000)PLAM(I),H_M,YLD_M
               write(IOUT,1000)PLAM(I),H_M,YLD_M
               CALL IMP_STOP(-1)
              endif
c
c
c
c           ..updates (shifted) stresses
c
c          -- ALPHA_RADIAL := PARAMETER ALPHA OF THE RADIAL RETURN
c
              ALPHA_RADIAL =  ONE - ( G3(I)/MAX(VM,EM20) )*DPLA
     $        - (YLD_CURR - YLD_PREV -YLD_M  + YLD_M_PREV )/MAX(VM,EM20)
c
              SIGNXX(I) = SIGNXX(I) * ALPHA_RADIAL
              SIGNYY(I) = SIGNYY(I) * ALPHA_RADIAL
              SIGNZZ(I) = SIGNZZ(I) * ALPHA_RADIAL
              SIGNXY(I) = SIGNXY(I) * ALPHA_RADIAL
              SIGNYZ(I) = SIGNYZ(I) * ALPHA_RADIAL
              SIGNZX(I) = SIGNZX(I) * ALPHA_RADIAL
c
c           ..updates back stresses
              HEP =  ( YLD_CURR - YLD_PREV
     $                            -YLD_M  + YLD_M_PREV  )/MAX(VM,EM20)
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
              YLD(I) = YLD_CURR   
            ELSEIF(FISOKIN == ONE)then
cccc              YLD(I) =  YLD(I)
            ELSE
              YLD(I) =  YLD_M
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
c-------------------------------------------------------------------
        ELSE ! -- i_bilin == 1
c   --- bilinear plastic hardening (isotropic, kinematic and mixed)
c-------------------------------------------------------------------
c
c         .. elastic predictor is temporarily stored in sxx, ... , szx
c
c         .. von Mises stress**2  at the elastic predictor ..
            VM =THREE*(HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     $               +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2)
c
            IF(VM > YLD(I)*YLD(I))THEN !..  plastically active process
              VM =SQRT(VM)
              R = YLD(I)/ MAX(VM,EM20)
c          .. plastic strain increment.
              DPLA=(ONE - R)*VM/MAX(G3(I)+H(I),EM20)
c
c         ... updates stresses ..
              ALPHA_RADIAL = ONE - G3(I)*( (ONE - R)/( G3(I) + H(I) ) )
c
              SIGNXX(I) = SIGNXX(I) * ALPHA_RADIAL
              SIGNYY(I) = SIGNYY(I) * ALPHA_RADIAL
              SIGNZZ(I) = SIGNZZ(I) * ALPHA_RADIAL
              SIGNXY(I) = SIGNXY(I) * ALPHA_RADIAL
              SIGNYZ(I) = SIGNYZ(I) * ALPHA_RADIAL
              SIGNZX(I) = SIGNZX(I) * ALPHA_RADIAL
c
              IF(FISOKIN > ZERO)then
c
c             --adds the back stresses at the beginning of the increment
                SIGNXX(I) = SIGNXX(I) + SIGBXX(I)
                SIGNYY(I) = SIGNYY(I) + SIGBYY(I)
                SIGNZZ(I) = SIGNZZ(I) + SIGBZZ(I)
                SIGNXY(I) = SIGNXY(I) + SIGBXY(I)
                SIGNYZ(I) = SIGNYZ(I) + SIGBYZ(I)
                SIGNZX(I) = SIGNZX(I) + SIGBZX(I)
c
c             ..updates back stresses
                HEP = FISOKIN*H(I)* DPLA/VM 
c
                SIGBXX(I) = SIGBXX(I) + sxx *HEP
                SIGBYY(I) = SIGBYY(I) + syy *HEP
                SIGBZZ(I) = SIGBZZ(I) + szz *HEP
                SIGBXY(I) = SIGBXY(I) + sxy *HEP 
                SIGBYZ(I) = SIGBYZ(I) + syz *HEP
                SIGBZX(I) = SIGBZX(I) + szx *HEP  
c
              ENDIF ! .. FISOKIN > ZERO ..       
c
c          .. updated yield stress.
              YLD(I) = MAX(YLD(I)+(ONE - FISOKIN)*DPLA*H(I),ZERO)
c
c          .. updates equivalent plastic strain
              PLA(I)    = PLA(I) + DPLA   
c  
              DPLA1(I)  = DPLA      
c
            ELSE !.. elasticity
c
            IF(FISOKIN > ZERO)then
c          .. gets stresses from shifted stresses and back stresses
              SIGNXX(I) = SIGNXX(I) + SIGBXX(I)
              SIGNYY(I) = SIGNYY(I) + SIGBYY(I)
              SIGNZZ(I) = SIGNZZ(I) + SIGBZZ(I)
              SIGNXY(I) = SIGNXY(I) + SIGBXY(I)
              SIGNYZ(I) = SIGNYZ(I) + SIGBYZ(I)
              SIGNZX(I) = SIGNZX(I) + SIGBZX(I)
            ENDIF
c
            ENDIF ! .. VM > YLD(I)*YLD(I)
c
c
        ENDIF ! .. i_bilin ..    
c
 100  CONTINUE  !..LOOP ON A GROUP OF ELEMENTS
c
 1000 FORMAT(3X,'Hardening parameters in law36 is not positive'/,
     $                   2X,'Eps_plast =',E11.4,2X,'H =',E11.4,
     $                   2X,'Sy0 =',E11.4)
c-----------
      RETURN
      END
