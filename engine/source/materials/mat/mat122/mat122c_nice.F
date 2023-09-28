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
Chd|  MAT122C_NICE                  source/materials/mat/mat122/mat122c_nice.F
Chd|-- called by -----------
Chd|        SIGEPS122C                    source/materials/mat/mat122/sigeps122c.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE MAT122C_NICE(
     1     NEL     ,NUPARAM ,NUVAR   ,UPARAM  ,UVAR    ,
     2     EPSXX   ,EPSYY   ,RHO     ,PLA     ,DPLA    ,
     3     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6     THK     ,THKLY   ,OFF     ,SIGY    ,ETSE    ,
     7     DMG     ,SEQ     ,EPSPL   ,SHF     ,SOUNDSP ,
     8     NFUNC   ,IFUNC   ,NPF     ,TF      ,NVARTMP ,
     9     VARTMP  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,
     .   NFUNC,IFUNC(NFUNC),NPF(SNPC),NVARTMP
      my_real, INTENT(IN) ::
     .   UPARAM(NUPARAM),TF(STF)
      my_real,DIMENSION(NEL), INTENT(IN) :: 
     .   RHO,EPSXX,EPSYY,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   THKLY,SHF,EPSPL
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,
     .   SIGY,ETSE
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: 
     .   PLA,THK,OFF,DMG,SEQ,DPLA
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   I,II,J,NITER,ITER,NINDX,INDEX(NEL),
     .   ISH,ITR,IRES,IBUCK,ICOMP,LTYPE11,LTYPE12,
     .   LTYPER0,IPOS(NEL),ILEN(NEL),IAD(NEL)
      my_real
     .   E10,E20,E30,NU12,NU21,NU23,NU32,NU13,NU31,
     .   G120,G23,G31,E1C,GAMMA,SIGY0,BETA,M,A,EFTI0,
     .   EFTU0,DFTU,EFCI0,EFCU0,DFCU,DSAT1,Y00,YC0,B,
     .   DMAX,YR,YSP,DSAT2,Y0P0,YCP0,DSAT2C,Y0PC0,YCPC0,
     .   EPSD11,D11,N11,D11U,N11U,EPSD12,D22,N22,D12,
     .   N12,EPSDR0,DR0,NR0
      my_real 
     .   DDEP,DFDSIG2,DLAM,NORMYY,NORMXY,SIG_DFDSIG,
     .   H(NEL),DPYY(NEL),DPZZ(NEL),DPXY(NEL),
     .   PHI(NEL),DEZZ(NEL),DPLA_DLAM(NEL),DPHI_DLAM(NEL),
     .   DEELZZ(NEL),DYDX(NEL),A11(NEL),A22(NEL),A12(NEL),
     .   DFT(NEL),DFC(NEL),E1(NEL),E2(NEL),EPSF_EQ,ZD(NEL),
     .   ZDP(NEL),Y(NEL),YP(NEL),D(NEL),DP(NEL),DF(NEL),
     .   G12(NEL),EFTI(NEL),EFTU(NEL),EFCI(NEL),EFCU(NEL),
     .   F11(NEL),F22(NEL),F12(NEL),F11R(NEL),FR0(NEL),
     .   Y0(NEL),YC(NEL),Y0P(NEL),YCP(NEL),Y0PC(NEL),YCPC(NEL),
     .   SEQ0(NEL),DSIGYY(NEL),DSIGXY(NEL),PHI0(NEL),DPHI,
     .   VAR(NEL),DPT(NEL),DPTC(NEL)
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------      
C      
      ! Elastic parameters
      E10     = UPARAM(1)
      E20     = UPARAM(2)
      E30     = UPARAM(3)
      NU12    = UPARAM(4)
      NU21    = UPARAM(5)
      NU13    = UPARAM(6)
      NU31    = UPARAM(7)   
      NU23    = UPARAM(8)
      NU32    = UPARAM(9)
      G120    = UPARAM(10)
      G23     = UPARAM(11)
      G31     = UPARAM(12)
      E1C     = UPARAM(13)
      GAMMA   = UPARAM(14) 
      ISH     = NINT(UPARAM(15))
      ITR     = NINT(UPARAM(16))
      IRES    = NINT(UPARAM(17))
      SIGY0   = UPARAM(18)
      BETA    = UPARAM(19)
      M       = UPARAM(20)
      A       = UPARAM(21)
      EFTI0   = UPARAM(22)
      EFTU0   = UPARAM(23)
      DFTU    = UPARAM(24)
      EFCI0   = UPARAM(25)
      EFCU0   = UPARAM(26)
      DFCU    = UPARAM(27)
      IBUCK   = NINT(UPARAM(28))
      DSAT1   = UPARAM(29)
      Y00     = UPARAM(30)
      YC0     = UPARAM(31)
      B       = UPARAM(32) 
      DMAX    = UPARAM(33)
      YR      = UPARAM(34)
      YSP     = UPARAM(35)
      DSAT2   = UPARAM(36)
      Y0P0    = UPARAM(37)
      YCP0    = UPARAM(38)
      DSAT2C  = UPARAM(39)
      Y0PC0   = UPARAM(40)
      YCPC0   = UPARAM(41)
      EPSD11  = UPARAM(42)
      D11     = UPARAM(43)
      N11     = UPARAM(44)
      D11U    = UPARAM(45)
      N11U    = UPARAM(46)
      EPSD12  = UPARAM(47)
      D22     = UPARAM(48)
      N22     = UPARAM(49)   
      D12     = UPARAM(50)
      N12     = UPARAM(51)
      EPSDR0  = UPARAM(52)
      DR0     = UPARAM(53)
      NR0     = UPARAM(54)
      LTYPE11 = NINT(UPARAM(55))
      LTYPE12 = NINT(UPARAM(56))
      LTYPER0 = NINT(UPARAM(57))
C
      ! Compression matrix damage flag
      ICOMP = 0
      IF (Y0PC0 > ZERO) ICOMP = 1
C
      ! Recovering internal variables
      DO I=1,NEL
        ! Checking deletion flag value
        IF (OFF(I) < ONE)  OFF(I) = FOUR_OVER_5*OFF(I)
        IF (OFF(I) < EM01) OFF(I) = ZERO
        ! Hourglass coefficient
        ETSE(I) = ONE
        H(I)    = ZERO
        ! Old yield stress
        SEQ0(I) = SEQ(I)
        ! Previous yield stress
        PHI0(I) = UVAR(I,18)
        ! Plastic strain increment
        DPLA(I) = ZERO
        DPYY(I) = ZERO
        DPXY(I) = ZERO
        DPZZ(I) = ZERO 
        ! Damage variables
        DF(I)   = UVAR(I,1)
        D(I)    = UVAR(I,2)
        DP(I)   = UVAR(I,3) 
        Y(I)    = UVAR(I,4)
        YP(I)   = UVAR(I,5)  
        DFT(I)  = UVAR(I,6) 
        DFC(I)  = UVAR(I,7)  
      ENDDO 
C
      ! Compute strain rate dependency factor 
      DO I = 1,NEL
        ! Rate dependency in fiber direction 1 for Young modulus 
        IF (LTYPE11 == 1) THEN
          F11(I) = D11*(ABS(EPSPL(I))/EPSD11)**N11
        ELSEIF (LTYPE11 == 2) THEN 
          F11(I) = D11*(ABS(EPSPL(I))/EPSD11) + N11
        ELSEIF (LTYPE11 == 3) THEN 
          F11(I) = D11*LOG(MAX(ABS(EPSPL(I))/EPSD11,ONE)) + LOG(N11)
        ELSEIF (LTYPE11 == 4) THEN 
          F11(I) = D11*TANH(N11*(MAX(ABS(EPSPL(I))-EPSD11,ZERO)))
        ENDIF
        ! Rate dependency in matrix transverse direction 2 for Young modulus
        IF (LTYPE12 == 1) THEN 
          F22(I) = D22*(ABS(EPSPL(I))/EPSD12)**N22
        ELSEIF (LTYPE12 == 2) THEN
          F22(I) = D22*(ABS(EPSPL(I))/EPSD12) + N22
        ELSEIF (LTYPE12 == 3) THEN
          F22(I) = D22*LOG(MAX(ABS(EPSPL(I))/EPSD12,ONE)) + LOG(N22)
        ELSEIF (LTYPE12 == 4) THEN
          F22(I) = D22*TANH(N22*(MAX(ABS(EPSPL(I))-EPSD12,ZERO)))
        ENDIF
        ! Rate dependency for shear plane 12 modulus
        IF (LTYPE12 == 1) THEN 
          F12(I) = D12*(ABS(EPSPL(I))/EPSD12)**N12
        ELSEIF (LTYPE12 == 2) THEN
          F12(I) = D12*(ABS(EPSPL(I))/EPSD12) + N12
        ELSEIF (LTYPE12 == 3) THEN
          F12(I) = D12*LOG(MAX(ABS(EPSPL(I))/EPSD12,ONE)) + LOG(N12)
        ELSEIF (LTYPE12 == 4) THEN
          F12(I) = D12*TANH(N12*(MAX(ABS(EPSPL(I))-EPSD12,ZERO)))
        ENDIF  
        ! Rate dependency in fiber direction 1 for Young modulus 
        IF (LTYPE11 == 1) THEN 
          F11R(I) = D11U*(ABS(EPSPL(I))/EPSD11)**N11U
        ELSEIF (LTYPE11 == 2) THEN 
          F11R(I) = D11U*(ABS(EPSPL(I))/EPSD11) + N11U
        ELSEIF (LTYPE11 == 3) THEN 
          F11R(I) = D11U*LOG(MAX(ABS(EPSPL(I))/EPSD11,ONE)) + LOG(N11U)
        ELSEIF (LTYPE11 == 4) THEN 
          F11R(I) = D11U*TANH(N11U*(MAX(ABS(EPSPL(I))-EPSD11,ZERO)))
        ENDIF         
        ! Rate dependency in fiber direction 1 for Young modulus 
        IF (LTYPER0 == 1) THEN 
          FR0(I) = DR0*(ABS(EPSPL(I))/EPSDR0)**NR0
        ELSEIF (LTYPER0 == 2) THEN
          FR0(I) = DR0*(ABS(EPSPL(I))/EPSDR0) + NR0
        ELSEIF (LTYPER0 == 3) THEN
          FR0(I) = DR0*LOG(MAX(ABS(EPSPL(I))/EPSDR0,ONE)) + LOG(NR0)
        ELSEIF (LTYPER0 == 4) THEN
          FR0(I) = DR0*TANH(NR0*(MAX(ABS(EPSPL(I))-EPSDR0,ZERO)))
        ENDIF
      ENDDO    
c
      ! Elastic parameters, yield stress and strain rate dependency
      DO I = 1,NEL
        ! Fiber (direction 1)
        ! -> Tension 
        IF (EPSXX(I) >= ZERO) THEN 
          E1(I) = E10
        ! -> Compression
        ELSE 
          E1(I) = E1C/(ONE + GAMMA*ABS(EPSXX(I)))
        ENDIF
        E1(I)  = E1(I)*(ONE + F11(I))
        ! Matrix (direction 2)
        E2(I)  = E20*(ONE + F22(I))
        ! In-plane shear 12
        G12(I) = G120*(ONE + F12(I))
        ! Plane stress stiffness matrix terms
        A11(I) = E1(I)/(ONE - NU12*NU21)
        A12(I) = NU21*A11(I)
        A22(I) = E2(I)/(ONE - NU12*NU21)
        ! Yield stress
        SIGY(I) = SIGY0*(ONE + FR0(I)) + BETA*EXP(M*LOG(PLA(I)+EM20))
        ! Rate dependency on fiber failure
        EFTI(I) = UVAR(I,8)
        IF (UVAR(I,8) == ZERO)  EFTI(I) = EFTI0*(ONE + F11R(I))
        EFTU(I) = UVAR(I,9)
        IF (UVAR(I,9) == ZERO)  EFTU(I) = EFTU0*(ONE + F11R(I))
        EFCI(I) = UVAR(I,10)
        IF (UVAR(I,10) == ZERO) EFCI(I) = EFCI0*(ONE + F11R(I))
        EFCU(I) = UVAR(I,11)
        IF (UVAR(I,11) == ZERO) EFCU(I) = EFCU0*(ONE + F11R(I))
        ! Rate dependency on matrix failure
        Y0(I) = UVAR(I,12)
        IF (UVAR(I,12) == ZERO) Y0(I)  = Y00*SQRT(ONE + F12(I))
        YC(I) = UVAR(I,13)
        IF (UVAR(I,13) == ZERO) YC(I)  = YC0*SQRT(ONE + F12(I))
        Y0P(I) = UVAR(I,14)
        IF (UVAR(I,14) == ZERO) Y0P(I) = Y0P0*SQRT(ONE + F22(I))
        YCP(I) = UVAR(I,15)
        IF (UVAR(I,15) == ZERO) YCP(I) = YCP0*SQRT(ONE + F22(I))
        Y0PC(I) = UVAR(I,16)
        IF (UVAR(I,16) == ZERO) Y0PC(I) = Y0PC0*SQRT(ONE + F22(I))
        YCPC(I) = UVAR(I,17)
        IF (UVAR(I,17) == ZERO) YCPC(I) = YCPC0*SQRT(ONE + F22(I))
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================       
      DO I=1,NEL 
c    
        ! Computation of the trial stress tensor  
        SIGNXX(I) = SIGOXX(I)/MAX((ONE-DF(I)),EM20) + A11(I)*DEPSXX(I) + A12(I)*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I)/MAX((ONE-DP(I)),EM20) + A12(I)*DEPSXX(I) + A22(I)*DEPSYY(I)
        SIGNXY(I) = SIGOXY(I)/MAX((ONE- D(I)),EM20) + G12(I)*DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I)/MAX(MIN(ONE-D(I),ONE-DP(I)),EM20) + G23*DEPSYZ(I)*SHF(I)
        SIGNZX(I) = SIGOZX(I)/MAX(MIN(ONE-D(I),ONE-DP(I)),EM20) + G31*DEPSZX(I)*SHF(I)
C
        ! Equivalent stress
        SEQ(I) = SQRT((SIGNXY(I))**2 + A*(SIGNYY(I))**2)
C
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !========================================================================
      PHI(1:NEL) = SEQ(1:NEL) - SIGY(1:NEL)
      ! Checking plastic behavior for all elements
      NINDX = 0
      INDEX(1:NEL) = 0
      DO I=1,NEL         
        IF ((PHI(I)>ZERO).AND.(OFF(I) == ONE)) THEN
          NINDX = NINDX+1
          INDEX(NINDX) = I
        ENDIF
      ENDDO
c             
      !====================================================================================
      ! - PLASTIC CORRECTION WITH N.I.C.E EXPLICIT ALGORITHM (NEXT INCREMENT CORRECT ERROR)
      !====================================================================================
c
#include "vectorize.inc" 
      ! Loop over yielding elements
      DO II=1,NINDX 
        I = INDEX(II)
c
        ! Computation of the trial stress increment
        DSIGYY(I) = A12(I)*DEPSXX(I) + A22(I)*DEPSYY(I)
        DSIGXY(I) = G12(I)*DEPSXY(I)
c          
        ! Note     : in this part, the purpose is to compute for each iteration
        ! a plastic multiplier allowing to update internal variables to satisfy
        ! the consistency condition. 
        ! Its expression is : DLAMBDA = - (PHI + DPHI) / DPHI_DLAMBDA
        ! -> PHI  : old value of yield function (known)
        ! -> DPHI : yield function prediction (to compute)
        ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
        !                   into account of internal variables kinetic : 
        !                   plasticity, temperature and damage (to compute)
c        
        ! 1 - Computation of DPHI_DSIG the normal to the yield surface
        !------------------------------------------------------------- 
        NORMYY = A*(SIGOYY(I)/MAX((ONE-DP(I)),EM20))/MAX(SEQ0(I),EM20)
        NORMXY =   (SIGOXY(I)/MAX((ONE- D(I)),EM20))/MAX(SEQ0(I),EM20)
c        
        ! Restoring previous value of the yield function
        PHI(I) = PHI0(I)
c
        ! Computation of yield surface trial increment DPHI       
        DPHI = NORMYY*DSIGYY(I) + NORMXY*DSIGXY(I)   
c          
        ! 2 - Computation of DPHI_DLAMBDA
        !---------------------------------------------------------
c        
        !   a) Derivative with respect stress increments tensor DSIG			
        !   --------------------------------------------------------
        DFDSIG2 = NORMYY*NORMYY*A22(I) + NORMXY*NORMXY*G12(I)                  
c
        !   b) Derivatives with respect to plastic strain P 			
        !   ------------------------------------------------  
c          
        !     i) Derivative of the yield stress with respect to plastic strain dSIGY / dPLA
        !     ----------------------------------------------------------------------------
        H(I) = BETA*M*EXP((M-1)*LOG(PLA(I)+EM20))
        H(I) = MIN(H(I),MAX(TWO*G120,E20))
c
        !     ii) Derivative of dPLA with respect to DLAM
        !     -------------------------------------------   
        SIG_DFDSIG = (SIGOYY(I)/MAX((ONE-DP(I)),EM20))*NORMYY + 
     .               (SIGOXY(I)/MAX((ONE- D(I)),EM20))*NORMXY                      
        DPLA_DLAM(I) = SIG_DFDSIG/MAX(SIGY(I),EM20)
c
        ! 3 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------
c          
        ! Derivative of PHI with respect to DLAM
        DPHI_DLAM(I) = - DFDSIG2 - H(I)*DPLA_DLAM(I)
        DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20),DPHI_DLAM(I))
c          
        ! Computation of the plastic multiplier
        DLAM = -(PHI(I)+DPHI)/DPHI_DLAM(I)  
c          
        ! Plastic strains tensor increment
        DPYY(I) = DLAM*NORMYY
        DPXY(I) = DLAM*NORMXY
c         
        ! Elasto-plastic stresses update   
        SIGNXX(I) = SIGNXX(I) - A12(I)*DPYY(I)
        SIGNYY(I) = SIGNYY(I) - A22(I)*DPYY(I)
        SIGNXY(I) = SIGNXY(I) - DPXY(I)*G12(I)
c          
        ! Cumulated plastic strain and strain rate update           
        DDEP    = DLAM*DPLA_DLAM(I)
        DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
        PLA(I)  = MAX(ZERO, PLA(I)  + DDEP)   
c
        ! Update equivalent stress          
        SEQ(I) = SQRT((SIGNXY(I))**2 + A*(SIGNYY(I))**2)
c      
        ! Transverse strain update
        DPZZ(I) = DPZZ(I) - DPYY(I)
c
        ! Update the hardening yield stress
        SIGY(I) = SIGY(I) + H(I)*DLAM*DPLA_DLAM(I)
c
        ! Update yield function value
        PHI(I)  = SEQ(I) - SIGY(I)
c
      ENDDO
      ! End of the loop over the yielding elements
c
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH N.I.C.E EXPLICIT ALGORITHM
      !===================================================================                  
c    
      !===================================================================
      ! - DAMAGE VARIABLES COMPUTATION AND UPDATE STRESS TENSOR
      !=================================================================== 
      DO I = 1,NEL
c
        ! Fiber damage (direction 1)
        ! ------------------------------------------
        ! -> Fiber equivalent strain 
        EPSF_EQ = EPSXX(I) + NU21*EPSYY(I)
        ! -> Tension 
        IF (EPSF_EQ >= ZERO) THEN 
          IF (EPSF_EQ < EFTI(I)) THEN 
            DFT(I) = MAX(ZERO,DFT(I))
          ELSEIF ((EPSF_EQ >= EFTI(I)).AND.(EPSF_EQ < EFTU(I))) THEN 
            DFT(I) = MAX(DFTU*((EPSF_EQ-EFTI(I))/(EFTU(I)-EFTI(I))),DFT(I))
            ! Save damage threshold in case of strain rate dependency
            IF (UVAR(I,8) == ZERO) UVAR(I,8) = EFTI(I)
            IF (UVAR(I,9) == ZERO) UVAR(I,9) = EFTU(I)
          ELSEIF (EPSF_EQ >= EFTU(I)) THEN 
            DFT(I) = MAX(ONE - (ONE - DFTU)*(EFTU(I)/EPSF_EQ),DFT(I))
          ENDIF
          DFT(I) = MAX(DFT(I),ZERO)
          DFT(I) = MIN(DFT(I),ONE)
          DF(I)  = DFT(I)
        ! -> Compression
        ELSEIF ((EPSF_EQ < ZERO).AND.(IBUCK > 1)) THEN 
          IF (ABS(EPSF_EQ) < EFCI(I)) THEN 
            DFC(I) = MAX(ZERO,DFC(I))
          ELSEIF ((ABS(EPSF_EQ) >= EFCI(I)).AND.(ABS(EPSF_EQ) < EFCU(I))) THEN 
            DFC(I) = MAX(DFCU*((ABS(EPSF_EQ)-EFCI(I))/(EFCU(I)-EFCI(I))),DFC(I))
            ! Save damage threshold in case of strain rate dependency
            IF (UVAR(I,10) == ZERO) UVAR(I,10) = EFCI(I)
            IF (UVAR(I,11) == ZERO) UVAR(I,11) = EFCU(I)
          ELSEIF (ABS(EPSF_EQ) >= EFCU(I)) THEN 
            DFC(I) = MAX(ONE - (ONE - DFCU)*(EFCU(I)/ABS(EPSF_EQ)),DFC(I))
          ENDIF  
          DFC(I) = MAX(DFC(I),ZERO)
          DFC(I) = MIN(DFC(I),ONE)
          DF(I)  = DFC(I)
        ENDIF
c
        ! Matrix damage (direction 2 and 3)  
        ! ------------------------------------------ 
        ! -> Damage functions (derivatives of elastic energy)
        ZD(I) = HALF*((SIGNXY(I)**2/G120) + (SIGNZX(I)**2/G31))
        ! -> If compression damage is activated
        IF (ICOMP > 0) THEN 
          ZDP(I) = HALF*((SIGNYY(I)**2)/E20)
        ! -> Tension only
        ELSE 
          ZDP(I) = HALF*(((MAX(SIGNYY(I),ZERO))**2)/E20)
        ENDIF
        ! -> Damage evolution functions
        Y(I)  = MAX(Y(I) ,SQRT(ZD(I)+B*ZDP(I)))
        YP(I) = MAX(YP(I),SQRT(ZDP(I)))
      ENDDO
c
      ! -> Shear damage
      !    Linear
      IF (ISH == 1) THEN
        DO I = 1,NEL 
          IF (Y(I)<Y0(I)) THEN 
            D(I) = ZERO
          ELSEIF ((D(I)<DMAX).AND.(Y(I)<YSP).AND.(Y(I)<YR)) THEN 
            D(I) = MAX(Y(I)-Y0(I),ZERO)/MAX(YC(I),EM20)
            D(I) = MIN(D(I),DMAX)
            ! Save damage threshold in case of strain rate dependency
            IF (UVAR(I,12) == ZERO) UVAR(I,12) = Y0(I)
            IF (UVAR(I,13) == ZERO) UVAR(I,13) = YC(I)
          ELSE
            D(I) = ONE - (ONE - DMAX)*UVAR(I,4)/MAX(Y(I),EM20)
          ENDIF
          D(I) = MAX(D(I),ZERO)
          D(I) = MIN(D(I), ONE) 
        ENDDO  
      !    Exponential 
      ELSEIF (ISH == 2) THEN 
        DO I = 1,NEL 
          IF (Y(I)>Y0(I)) THEN 
            D(I) = DSAT1*(ONE - EXP((Y0(I)-Y(I))/MAX(YC(I),EM20)))
            ! Save damage threshold in case of strain rate dependency
            IF (UVAR(I,12) == ZERO) UVAR(I,12) = Y0(I)
            IF (UVAR(I,13) == ZERO) UVAR(I,13) = YC(I)
          ELSE 
            D(I) = ZERO
          ENDIF  
          D(I) = MAX(D(I),ZERO)
          D(I) = MIN(D(I), ONE) 
        ENDDO
      !    Tabulated function
      ELSEIF (ISH == 3) THEN
        IPOS(1:NEL) = VARTMP(1:NEL,1)
        IAD (1:NEL) = NPF(IFUNC(1)) / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        DO I = 1,NEL 
          VAR(I) = Y(I)/Y0(I)
        ENDDO 
        CALL VINTER(TF,IAD,IPOS,ILEN,NEL,VAR,DYDX,D) 
        VARTMP(1:NEL,1) = IPOS(1:NEL)
        DO I = 1,NEL 
          ! Save damage threshold in case of strain rate dependency
          IF (UVAR(I,12) == ZERO .AND. D(I) /= ZERO) UVAR(I,12) = Y0(I)
          D(I) = MAX(D(I),ZERO)
          D(I) = MIN(D(I), ONE)
        ENDDO
      ENDIF
c
      ! -> Transverse damage
      !    Linear
      IF (ITR == 1) THEN 
        DO I = 1,NEL
          ! -> Compression damage if activated
          IF ((ICOMP > 0).AND.(EPSYY(I)<ZERO)) THEN
            IF (YP(I)<Y0PC(I)) THEN 
              DP(I) = ZERO
            ELSEIF ((DP(I)<DMAX).AND.(YP(I)<YSP).AND.(YP(I)<YR)) THEN 
              DP(I) = MAX(YP(I)-Y0PC(I),ZERO)/MAX(YCPC(I),EM20)
              DP(I) = MIN(DP(I),DMAX)
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,16) == ZERO) UVAR(I,16) = Y0PC(I)
              IF (UVAR(I,17) == ZERO) UVAR(I,17) = YCPC(I)
            ELSE
              DP(I) = ONE - (ONE - DMAX)*UVAR(I,5)/MAX(YP(I),EM20)
            ENDIF
          ! -> Tension damage
          ELSE 
            IF (YP(I)<Y0P(I)) THEN 
              DP(I) = ZERO
            ELSEIF ((DP(I)<DMAX).AND.(YP(I)<YSP).AND.(YP(I)<YR)) THEN 
              DP(I) = MAX(YP(I)-Y0P(I),ZERO)/MAX(YCP(I),EM20)
              DP(I) = MIN(DP(I),DMAX)
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,14) == ZERO) UVAR(I,14) = Y0P(I)
              IF (UVAR(I,15) == ZERO) UVAR(I,15) = YCP(I)
            ELSE
              DP(I) = ONE - (ONE - DMAX)*UVAR(I,5)/MAX(YP(I),EM20)
            ENDIF
          ENDIF
          DP(I) = MAX(DP(I),ZERO)
          DP(I) = MIN(DP(I), ONE)           
        ENDDO
      !    Exponential 
      ELSEIF (ITR == 2) THEN 
        DO I = 1,NEL
          ! -> Compression damage if activated
          IF ((ICOMP > 0).AND.(EPSYY(I)<ZERO)) THEN
            IF (YP(I)>Y0PC(I)) THEN 
              DP(I) = DSAT2C*(ONE - EXP((Y0PC(I)-YP(I))/MAX(YCPC(I),EM20)))
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,16) == ZERO) UVAR(I,16) = Y0PC(I)
              IF (UVAR(I,17) == ZERO) UVAR(I,17) = YCPC(I)
            ELSE 
              DP(I) = ZERO
            ENDIF  
          ! -> Tension damage
          ELSE
            IF (YP(I)>Y0P(I)) THEN 
              DP(I) = DSAT2*(ONE - EXP((Y0P(I)-YP(I))/MAX(YCP(I),EM20)))
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,14) == ZERO) UVAR(I,14) = Y0P(I)
              IF (UVAR(I,15) == ZERO) UVAR(I,15) = YCP(I)
            ELSE 
              DP(I) = ZERO
            ENDIF  
          ENDIF
          DP(I) = MAX(DP(I),ZERO)
          DP(I) = MIN(DP(I), ONE)  
        ENDDO
      !    Tabulated function
      ELSEIF (ITR == 3) THEN 
        ! -> Compression tabulated damage
        IF (ICOMP > 0) THEN 
          IPOS(1:NEL) = VARTMP(1:NEL,3)
          IAD (1:NEL) = NPF(IFUNC(3)) / 2 + 1
          ILEN(1:NEL) = NPF(IFUNC(3)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
          DO I = 1,NEL 
            VAR(I) = YP(I)/Y0PC(I)
          ENDDO 
          CALL VINTER(TF,IAD,IPOS,ILEN,NEL,VAR,DYDX,DPTC) 
          VARTMP(1:NEL,3) = IPOS(1:NEL)    
        ENDIF  
        ! -> Tension tabulated damage  
        IPOS(1:NEL) = VARTMP(1:NEL,2)
        IAD (1:NEL) = NPF(IFUNC(2)) / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(2)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        DO I = 1,NEL 
          VAR(I) = YP(I)/Y0P(I)
        ENDDO 
        CALL VINTER(TF,IAD,IPOS,ILEN,NEL,VAR,DYDX,DPT) 
        VARTMP(1:NEL,2) = IPOS(1:NEL) 
        DO I = 1,NEL 
          ! -> Compression damage if activated
          IF ((ICOMP > 0).AND.(EPSYY(I)<ZERO)) THEN 
            DP(I) = DPTC(I) 
            ! Save damage threshold in case of strain rate dependency
            IF ((UVAR(I,16) == ZERO) .AND. (DP(I) /= ZERO)) UVAR(I,16) = Y0PC(I)
          ! -> Tension damage
          ELSE
            DP(I) = DPT(I) 
            ! Save damage threshold in case of strain rate dependency
            IF ((UVAR(I,14) == ZERO) .AND. (DP(I) /= ZERO)) UVAR(I,14) = Y0P(I)
          ENDIF
          DP(I) = MAX(DP(I),ZERO)
          DP(I) = MIN(DP(I), ONE)
        ENDDO
      ENDIF
c
      DO I = 1,NEL
c
        ! Update stresses with damage softening effect
        ! -------------------------------------------- 
        SIGNXX(I) = SIGNXX(I)*(ONE - DF(I))
        SIGNYY(I) = SIGNYY(I)*(ONE - DP(I))
        SIGNXY(I) = SIGNXY(I)*(ONE -  D(I))
        SIGNYZ(I) = SIGNYZ(I)*MIN(ONE - D(I),ONE - DP(I))
        SIGNZX(I) = SIGNZX(I)*MIN(ONE - D(I),ONE - DP(I))
c
        ! -> Store internal variables
        ! ------------------------------------------ 
        UVAR(I,1) = DF(I)
        UVAR(I,2) = D(I) 
        UVAR(I,3) = DP(I)
        UVAR(I,4) = Y(I) 
        UVAR(I,5) = YP(I)
        UVAR(I,6) = DFT(I)
        UVAR(I,7) = DFC(I)
        DMG(I) = MAX(DF(I),D(I),DP(I))
c
      ENDDO
      !=================================================================== 
c
      ! Sound-speed and thickness update
      DO I=1,NEL
        ! Save old yield stress
        IF (DPLA(I) > ZERO) THEN 
          UVAR(I,18) = PHI(I)
        ELSE
          UVAR(I,18) = ZERO
        ENDIF
        ! Sound-speed
        SOUNDSP(I) = SQRT(MAX(A11(I),A22(I))/RHO(I))
        ! Coefficient for hourglass
        IF (DPLA(I)>ZERO) THEN 
          ETSE(I)  = H(I) / (H(I) + MAX(E1(I),E2(I)))
        ELSE
          ETSE(I)  = ONE
        ENDIF
        ! Computation of the thickness variation 
        DEELZZ(I)  = -(NU13/E1(I))*(SIGNXX(I)-SIGOXX(I))-(NU23/E2(I))*(SIGNYY(I)-SIGOYY(I)) 
        DEZZ(I)    = DEELZZ(I) + DPZZ(I)
        THK(I)     = THK(I) + DEZZ(I)*THKLY(I)*OFF(I)  
      ENDDO 
C
      END
C