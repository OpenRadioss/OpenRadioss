Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ASSO_PLAS76                   source/materials/mat/mat076/asso_plas76.F
Chd|-- called by -----------
Chd|        SIGEPS76                      source/materials/mat/mat076/sigeps76.F
Chd|-- calls ---------------
Chd|        TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|        TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|====================================================================
      SUBROUTINE ASSO_PLAS76(
     1     NEL     ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2     NPF     ,TF     ,TIME    ,TIMESTEP,UPARAM  ,PLA    ,
     3     RHO0    ,RHO    ,DPLA    ,ET      ,NUMTABL ,TABLE  ,
     3     DEPSXX  ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     4     SIGOXX  ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     5     SIGNXX  ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     6     SOUNDSP ,UVAR   ,OFF     ,NGL     ,EPSD    ,YLD    ,
     7     INLOC   ,DPLANL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE4D_MOD
      USE TABLE_MAT_VINTERP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
C=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,NGL(NEL),INLOC,NFUNC,NUMTABL
      my_real :: UPARAM(NUPARAM)
      my_real
     .   TIME,TIMESTEP,
     .   RHO(NEL),RHO0(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   EPSD(NEL),DPLANL(NEL)
      TYPE (TABLE_4D_), DIMENSION(NUMTABL) ,INTENT(IN) :: TABLE
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   SOUNDSP(NEL),DPLA(NEL),ET(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   UVAR(NEL,NUVAR),OFF(NEL),YLD(NEL),PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), IFUNC(NFUNC)
      my_real :: FINTER,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,II,ICONV,NINDX,ICAS,ITER
      INTEGER ,PARAMETER :: FUNC_TRAC  = 1
      INTEGER ,PARAMETER :: FUNC_COMP  = 2
      INTEGER ,PARAMETER :: FUNC_SHEAR = 3
      INTEGER ,DIMENSION(NEL)   :: INDX
      INTEGER ,DIMENSION(NEL,2) :: IPOS
      my_real :: G,AA1,AA2,C1,NUPC,EPSPF,EPSPR,XFAC
      my_real ,DIMENSION(NEL) :: PLAT,PLAC,PLAS,EPSPT,EPSPC,EPSPS,SIGT,
     .    SIGS,DSIGS_DP,SIGC,DSIGC_DP,SDXX,SDYY,SDZZ,SDXY,SDYZ,SDZX,DSIGT_DP,
     .    NUP,P,SVM,A0,A1,A2,PHI,PLA_DAM,DAM
      my_real
     .    NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,NORM,AA,CB,
     .    NORMXX_N,NORMYY_N,NORMZZ_N,NORMXY_N,NORMYZ_N,NORMZX_N,
     .    DSIGT_DLAM,DSIGC_DLAM,DSIGS_DLAM,DA1_DSIGS,DA1_DSIGT,
     .    DA1_DSIGC,DA2_DSIGS,DA2_DSIGT,DA2_DSIGC,DA0_DLAM,DA1_DLAM,
     .    DA2_DLAM,DFDSIG2,DPHI_DLAM,DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX,
     .    DPLAC(NEL),DPLAT(NEL),DPLAS(NEL),DPDT_C,DPDT_S,DPDT_T,
     .    DPDT,DF,DFT,DLAM,CC,EPDT_MIN,EPDT_MAX,EPDC_MAX,EPDC_MIN,
     .    EPDS_MIN,EPDS_MAX,ASRATE
      my_real ,DIMENSION(NEL,2)   :: XVEC
      my_real ,DIMENSION(NUMTABL) :: TFAC
      my_real ,DIMENSION(NFUNC)   :: YFAC
      LOGICAL :: CONV(NEL)
      my_real, PARAMETER :: SFAC = 1.05D0 ! Security factor of ICONV
C
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! -> Elastic parameters
      G     = UPARAM(4) ! Shear modulus                
      AA1   = UPARAM(6) ! First component of elastic matrix
      AA2   = UPARAM(7) ! Second component of elastic matrix
      C1    = UPARAM(8) ! Bulk modulus
      ! -> Plastic parameters
      NUPC  = UPARAM(9)  ! Plastic Poisson ratio                      
      EPSPF = UPARAM(10) ! Failure plastic strain (start of damage)                    
      EPSPR = UPARAM(11) ! Maximum plastic strain (element deletion)                     
      ! -> Flags 
      ICONV  = NINT(UPARAM(15)) ! Flag to ensure convexity
      ASRATE = MIN(ONE,UPARAM(16)*TIMESTEP)
      ICAS   = NINT(UPARAM(17)) ! Flag for tabulated case
      !icas      ifunt   | ifunc   | ifuncs
      !  -1         1    |    1    |    1
      !   0         1    |    0    |    0
      !   1         1    |    1    |    0
      !   2         1    |    0    |    1
      XFAC  = UPARAM(18) ! Strain-rate scale factor
      EPDT_MIN = UPARAM(19)
      EPDT_MAX = UPARAM(20)
      EPDC_MIN = UPARAM(21)
      EPDC_MAX = UPARAM(22)
      EPDS_MIN = UPARAM(23)
      EPDS_MAX = UPARAM(24)
      TFAC(1)  = UPARAM(25)
      TFAC(2)  = UPARAM(26)
      TFAC(3)  = UPARAM(27)
      YFAC(1)  = UPARAM(28)
      YFAC(2)  = UPARAM(29)
c
      ! Recovering internal variables
      DO I=1,NEL    
        IF(OFF(I) < EM01) OFF(I)=ZERO
        IF(OFF(I) < ONE)   OFF(I)=OFF(I)*FOUR_OVER_5    
        PLAT(I)  = UVAR(I,1) ! Uniaxial tension plastic strain    
        PLAC(I)  = UVAR(I,2) ! Uniaxial compression plastic strain    
        PLAS(I)  = UVAR(I,3) ! Shear plastic strain
        EPSPT(I) = UVAR(I,4) ! Uniaxial tension plastic strain-rate
        EPSPC(I) = UVAR(I,5) ! Uniaxial compression plastic strain-rate
        EPSPS(I) = UVAR(I,6) ! Shear plastic strain-rate
        EPSPT(I) = MIN(EPDT_MAX, MAX(EPSPT(I),EPDT_MIN))
        EPSPC(I) = MIN(EPDC_MAX, MAX(EPSPC(I),EPDC_MIN))
        EPSPS(I) = MIN(EPDS_MAX, MAX(EPSPS(I),EPDS_MIN))
        DAM(I)   = UVAR(I,7) ! Damage variable
        DPLA(I)  = ZERO
        DPLAC(I) = ZERO
        DPLAT(I) = ZERO
        DPLAS(I) = ZERO
      ENDDO
c     Initialize plastic Poisson ratio
      IF (TIME == ZERO) THEN
        NUP(1:NEL)    = NUPC
        UVAR(1:NEL,9) = NUPC
      ELSE
        NUP(1:NEL) = UVAR(1:NEL,9)
      END IF   
C
      ! Computation of yield stresses
      XVEC(1:NEL,1)   = PLAT(1:NEL)
      XVEC(1:NEL,2)   = EPSPT(1:NEL)*XFAC
      IPOS(1:NEL,1:2) = 1
      CALL TABLE_MAT_VINTERP(TABLE(FUNC_TRAC),NEL,NEL,IPOS,XVEC,SIGT,DSIGT_DP)
      SIGT     = SIGT*TFAC(1)*(ONE-DAM(1:NEL))
      DSIGT_DP = DSIGT_DP*TFAC(1)*(ONE-DAM(1:NEL))
      IF (TABLE(FUNC_COMP)%NOTABLE > 0) THEN 
        XVEC(1:NEL,1)   = PLAC(1:NEL)
        XVEC(1:NEL,2)   = EPSPC(1:NEL)*XFAC
        IPOS(1:NEL,1:2) = 1
        CALL TABLE_MAT_VINTERP(TABLE(FUNC_COMP),NEL,NEL,IPOS,XVEC,SIGC,DSIGC_DP)
        SIGC     = SIGC*TFAC(2)*(ONE-DAM(1:NEL))
        DSIGC_DP = DSIGC_DP*TFAC(2)*(ONE-DAM(1:NEL))
      ENDIF
      IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) THEN 
        XVEC(1:NEL,1)   = PLAS(1:NEL)
        XVEC(1:NEL,2)   = EPSPS(1:NEL)*XFAC
        IPOS(1:NEL,1:2) = 1
        CALL TABLE_MAT_VINTERP(TABLE(FUNC_SHEAR),NEL,NEL,IPOS,XVEC,SIGS,DSIGS_DP)
        SIGS     = SIGS*TFAC(3)*(ONE-DAM(1:NEL))
        DSIGS_DP = DSIGS_DP*TFAC(3)*(ONE-DAM(1:NEL)) 
      ENDIF 
      ! Select case for tabulated yield stresses
      SELECT CASE (ICAS)
        CASE (0)
          SIGC(1:NEL) = SIGT(1:NEL)
          SIGS(1:NEL) = SIGT(1:NEL)/SQR3
        CASE(1)
          DO I = 1,NEL
            SIGS(I) = SQRT(SIGC(I)*SIGT(I)/THREE)
          ENDDO
      END SELECT  
C
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================      
      DO I=1,NEL
C        
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + (AA1*DEPSXX(I)
     .                        + AA2*(DEPSYY(I) + DEPSZZ(I)))
        SIGNYY(I) = SIGOYY(I) + (AA1*DEPSYY(I)
     .                        + AA2*(DEPSXX(I) + DEPSZZ(I)))
        SIGNZZ(I) = SIGOZZ(I) + (AA1*DEPSZZ(I)
     .                        + AA2*(DEPSXX(I) + DEPSYY(I)))
        SIGNXY(I) = SIGOXY(I) + G*DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + G*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + G*DEPSZX(I)
C         
        ! Computation of the pressure of the trial stress tensor
        P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
        ! Computation of the Von Mises equivalent stress of the trial stress tensor
        SDXX(I) = SIGNXX(I) + P(I)
        SDYY(I) = SIGNYY(I) + P(I)
        SDZZ(I) = SIGNZZ(I) + P(I)
        SDXY(I) = SIGNXY(I)
        SDYZ(I) = SIGNYZ(I)
        SDZX(I) = SIGNZX(I)
        SVM(I)  = HALF*(SDXX(I)**2 + SDYY(I)**2 + SDZZ(I)**2)
     .       +           (SDXY(I)**2 + SDZX(I)**2 + SDYZ(I)**2)
        SVM(I)  = SQRT(THREE*SVM(I))
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !======================================================================== 
      ! Compute yield criterion parameters
      IF (ICONV == 1) THEN
        ! Ensured convexity 
        DO I = 1,NEL
          CONV(I) = .FALSE.
          IF (SIGS(I) < SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)) THEN 
            SIGS(I) = SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)
            CONV(I) = .TRUE.
          ENDIF
        ENDDO   
      ENDIF
      ! Compute yield criterion parameters A0,A1,A2
      DO I = 1,NEL
        AA    = ONE/SIGC(I)/SIGT(I)
        A0(I) = THREE*(SIGS(I)**2)
        A1(I) = NINE*(SIGS(I)**2)*(SIGC(I) - SIGT(I))*AA
        A2(I) = NINE*(SIGC(I)*SIGT(I) - THREE*(SIGS(I)**2))*AA
      ENDDO   
c
      ! -> Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL
        PHI(I) = (SVM(I)**2) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
        IF (PHI(I) >= ZERO .AND. OFF(I) == ONE) THEN
          NINDX = NINDX + 1
          INDX(NINDX) = I
        ENDIF
      ENDDO
c      
      !====================================================================
      ! - PLASTIC RETURN MAPPING WITH CUTTING PLANE METHOD
      !====================================================================
      IF (NINDX > 0) THEN
c  
        ! Loop over the iterations   
        DO ITER = 1,3
c
          ! Loop over yielding elements
          DO II = 1,NINDX         
            I = INDX(II)
c        
            ! Note     : in this part, the purpose is to compute for each iteration
            ! a plastic multiplier allowing to update internal variables to satisfy
            ! the consistency condition using the backward Euler implicit method
            ! with a Newton-Raphson iterative procedure
            ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
            ! -> PHI          : current value of yield function (known)
            ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
            !                   into account of internal variables kinetic : 
            !                   plasticity, temperature and damage (to compute)
c        
            ! 1 - Computation of DPHI_DSIG the normal to the yield surface
            !-------------------------------------------------------------
C                  
            ! Computation of normal to yield surface 
            NORMXX   = THREE*SDXX(I) + THIRD*(A1(I) + TWO*A2(I)*P(I)) 
            NORMYY   = THREE*SDYY(I) + THIRD*(A1(I) + TWO*A2(I)*P(I))
            NORMZZ   = THREE*SDZZ(I) + THIRD*(A1(I) + TWO*A2(I)*P(I))
            NORMXY   = THREE*SDXY(I)
            NORMYZ   = THREE*SDYZ(I)
            NORMZX   = THREE*SDZX(I)
C
            ! Computation of the normalized normal to the yield surface
            CB       = A1(I) + TWO*A2(I)*P(I)                                 
            NORM     = MAX(EM20,SQRT(SIX*(SVM(I)**2) + THIRD*CB*CB))     
            NORMXX_N = NORMXX/NORM
            NORMYY_N = NORMYY/NORM
            NORMZZ_N = NORMZZ/NORM
            NORMXY_N = NORMXY/NORM
            NORMYZ_N = NORMYZ/NORM
            NORMZX_N = NORMZX/NORM
C                     
            ! 2 - Computation of DPHI_DLAMBDA
            !---------------------------------------------------------
c        
            !   a) Derivative with respect stress increments tensor DSIG
            !   --------------------------------------------------------
            DFDSIG2 = NORMXX*(AA1*NORMXX_N + AA2*(NORMYY_N + NORMZZ_N)) +
     .                NORMYY*(AA1*NORMYY_N + AA2*(NORMXX_N + NORMZZ_N)) +
     .                NORMZZ*(AA1*NORMZZ_N + AA2*(NORMXX_N + NORMYY_N)) + 
     .                TWO*NORMXY*TWO*G*NORMXY_N + 
     .                TWO*NORMYZ*TWO*G*NORMYZ_N + 
     .                TWO*NORMZX*TWO*G*NORMZX_N
c
            !   b) Derivative of yield criterion parameters
            !   --------------------------------------------
C
            ! Derivative of yield surfaces with respect to yield criterion parameter A0,A1,A2                                                                  
            DSIGT_DLAM = DSIGT_DP(I)*(SVM(I)/NORM)*(THREE/(ONE + NUP(I)))
            IF (TABLE(FUNC_COMP)%NOTABLE  > 0) DSIGC_DLAM = DSIGC_DP(I)*(SVM(I)/NORM)*(THREE/(ONE + NUP(I)))
            IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) DSIGS_DLAM = DSIGS_DP(I)*SQR3*(SVM(I)/NORM)
            SELECT CASE(ICAS)
              CASE(0)
                DSIGC_DLAM = DSIGT_DLAM 
                DSIGS_DLAM = (ONE/SQR3)*DSIGT_DLAM
              CASE(1)
                DSIGS_DLAM = (ONE/SQR3)*(ONE/(TWO*SQRT(SIGT(I)*SIGC(I))))*
     .                       (DSIGC_DLAM*SIGT(I) + SIGC(I)*DSIGT_DLAM)
            END SELECT    
            IF (ICONV == 1) THEN                                      
              IF (CONV(I)) THEN 
                DSIGS_DLAM = SFAC*(ONE/SQR3)*(ONE/(TWO*SQRT(SIGT(I)*SIGC(I))))*
     .                       (DSIGC_DLAM*SIGT(I) + SIGC(I)*DSIGT_DLAM)             
              ENDIF 
            ENDIF    
C
            ! -> A1 derivatives 
            CC = SIGS(I)/SIGC(I)/SIGT(I)
            !   -> With respect to SIGS
            DA1_DSIGS = EIGHTEEN*(SIGC(I) - SIGT(I))*CC
            !   -> With respect to SIGC
            DA1_DSIGC = NINE*(SIGS(I)/SIGC(I))**2
            !   -> With respect to SIGT
            DA1_DSIGT = -NINE*(SIGS(I)/SIGT(I))**2
C
            ! -> A2 derivatives
            !   -> With respect to SIGS
            DA2_DSIGS = -CINQUANTE4*CC
            !   -> With respect to SIGC                                         
            DA2_DSIGC = TWENTY7*CC*SIGS(I)/SIGC(I)  
            !   -> With respect to SIGT                       
            DA2_DSIGT = TWENTY7*CC*SIGS(I)/SIGT(I)                     
c            
            ! -> A parameters derivatives with respect to lambda        
            !   -> A0 with respect to lambda                                               
            DA0_DLAM = SIX*SIGS(I)*DSIGS_DLAM      
            !   -> A1 with respect to lambda                            
            DA1_DLAM = DA1_DSIGS*DSIGS_DLAM + DA1_DSIGT*DSIGT_DLAM + DA1_DSIGC*DSIGC_DLAM
            !   -> A2 with respect to lambda                     
            DA2_DLAM = DA2_DSIGS*DSIGS_DLAM + DA2_DSIGT*DSIGT_DLAM + DA2_DSIGC*DSIGC_DLAM                                                
c          
c            
            ! 3 - Computation of plastic multiplier and variables update
            !----------------------------------------------------------
c          
            ! Derivative of PHI with respect to DLAM
            DPHI_DLAM = - DFDSIG2 - DA0_DLAM - P(I)*DA1_DLAM - (P(I)**2)*DA2_DLAM   
            DPHI_DLAM = SIGN(MAX(ABS(DPHI_DLAM),EM20),DPHI_DLAM)                 
            DLAM = -PHI(I)/DPHI_DLAM    
c          
            ! Plastic strains tensor update
            DPXX = DLAM*NORMXX_N                          
            DPYY = DLAM*NORMYY_N                          
            DPZZ = DLAM*NORMZZ_N                          
            DPXY = DLAM*NORMXY_N                          
            DPYZ = DLAM*NORMYZ_N                          
            DPZX = DLAM*NORMZX_N                 
c          
            ! Elasto-plastic stresses update   
            SIGNXX(I) = SIGNXX(I) - (AA1*DPXX + AA2*(DPYY + DPZZ)) 
            SIGNYY(I) = SIGNYY(I) - (AA1*DPYY + AA2*(DPXX + DPZZ))
            SIGNZZ(I) = SIGNZZ(I) - (AA1*DPZZ + AA2*(DPXX + DPYY))
            SIGNXY(I) = SIGNXY(I) - TWO*G*DPXY     
            SIGNYZ(I) = SIGNYZ(I) - TWO*G*DPYZ  
            SIGNZX(I) = SIGNZX(I) - TWO*G*DPZX
c          
            ! Cumulated plastic strains update       
            PLAT(I)  = PLAT(I) + DLAM*(SVM(I)/NORM)*THREE/(ONE + NUP(I))
            PLAC(I)  = PLAT(I)    
            PLAS(I)  = PLAS(I) + SQR3*(SVM(I)/NORM)*DLAM   
            PLA(I)   = PLA(I)  + TWO*DLAM*(SVM(I)/NORM)
            ! Plastic strain increments update
            DPLAT(I) = DPLAT(I) + DLAM*(SVM(I)/NORM)*THREE/(ONE + NUP(I))
            DPLAC(I) = DPLAT(I)
            DPLAS(I) = DPLAS(I) + SQR3*(SVM(I)/NORM)*DLAM
            DPLA(I)  = DPLA(I)  + TWO*DLAM*(SVM(I)/NORM)
C
            ! Update pressure
            P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
            ! Update Von Mises stress
            SDXX(I) = SIGNXX(I) + P(I)
            SDYY(I) = SIGNYY(I) + P(I)
            SDZZ(I) = SIGNZZ(I) + P(I)
            SDXY(I) = SIGNXY(I)
            SDYZ(I) = SIGNYZ(I)
            SDZX(I) = SIGNZX(I)
            SVM(I)  = HALF*(SDXX(I)**2 + SDYY(I)**2 + SDZZ(I)**2)
     .         +           (SDXY(I)**2 + SDZX(I)**2 + SDYZ(I)**2)
            SVM(I)  = SQRT(THREE*SVM(I))
          ENDDO
C
          ! Update yield stresses
          XVEC(1:NEL,1)   = PLAT(1:NEL)
          XVEC(1:NEL,2)   = EPSPT(1:NEL)*XFAC
          IPOS(1:NEL,1:2) = 1
          CALL TABLE_MAT_VINTERP(TABLE(FUNC_TRAC),NEL,NEL,IPOS,XVEC,SIGT,DSIGT_DP)
          SIGT     = SIGT*TFAC(1)*(ONE-DAM(1:NEL))
          DSIGT_DP = DSIGT_DP*TFAC(1)*(ONE-DAM(1:NEL))
          IF (TABLE(FUNC_COMP)%NOTABLE > 0) THEN 
            XVEC(1:NEL,1)   = PLAC(1:NEL)
            XVEC(1:NEL,2)   = EPSPC(1:NEL)*XFAC
            IPOS(1:NEL,1:2) = 1
            CALL TABLE_MAT_VINTERP(TABLE(FUNC_COMP),NEL,NEL,IPOS,XVEC,SIGC,DSIGC_DP)
            SIGC(1:NEL)     = SIGC*TFAC(2)*(ONE-DAM(1:NEL))
            DSIGC_DP(1:NEL) = DSIGC_DP*TFAC(2)*(ONE-DAM(1:NEL))
          ENDIF
          IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) THEN 
            XVEC(1:NEL,1)   = PLAS(1:NEL)
            XVEC(1:NEL,2)   = EPSPS(1:NEL)*XFAC
            IPOS(1:NEL,1:2) = 1
            CALL TABLE_MAT_VINTERP(TABLE(FUNC_SHEAR),NEL,NEL,IPOS,XVEC,SIGS,DSIGS_DP)
            SIGS(1:NEL)     = SIGS*TFAC(3)*(ONE-DAM(1:NEL)) 
            DSIGS_DP(1:NEL) = DSIGS_DP*TFAC(3)*(ONE-DAM(1:NEL)) 
          ENDIF 
          ! Select case for tabulated yield stresses
          SELECT CASE (ICAS)
            CASE (0)
              DO II = 1,NINDX
                I = INDX(II)
                SIGC(I) = SIGT(I)
                SIGS(I) = SIGT(I)/SQR3
              ENDDO
            CASE(1)
              DO II = 1,NINDX
                I = INDX(II)
                SIGS(I) = SQRT(SIGC(I)*SIGT(I)/THREE)
              ENDDO
          END SELECT
C
          ! Update yield function value
          IF (ICONV == 1) THEN
            DO II = 1,NINDX         
              I = INDX(II)
              CONV(I) = .FALSE.
              IF (SIGS(I) < SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)) THEN 
                SIGS(I) = SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)
                CONV(I) = .TRUE.
              ENDIF
            ENDDO
          ENDIF
          DO II = 1,NINDX         
            I = INDX(II)
            IF (DAM(I) < ONE) THEN 
              AA    = ONE/SIGC(I)/SIGT(I)
              A0(I) = THREE*(SIGS(I)**2)
              A1(I) = NINE*(SIGS(I)**2)*(SIGC(I) - SIGT(I))*AA
              A2(I) = NINE*(SIGC(I)*SIGT(I) - THREE*(SIGS(I)**2))*AA
            ELSE
              A0(I) = ZERO
              A1(I) = ZERO
              A2(I) = ZERO
            ENDIF
            PHI(I) = SVM(I)**2 - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)  
          ENDDO
        ENDDO 
      ENDIF
c-----------------------------------------------
c     Update plastic Poisson coefficient
c-----------------------------------------------
      IF (IFUNC(1) > 0) THEN
        DO I=1,NEL
          NUP(I) = FINTER(IFUNC(1),PLA(I),NPF,TF,DF) * YFAC(1)
          NUP(I)    = MAX(ZERO, MIN(NUP(I), HALF))
          UVAR(I,9) = NUP(I)
        ENDDO
      END IF
c      
      !====================================================================
      ! - STORING NEW VALUES
      !====================================================================
      NINDX = 0
      DO I=1,NEL
        IF (INLOC > 0) THEN 
          PLA_DAM(I) = UVAR(I,8)
        ELSE
          PLA_DAM(I) = PLA(I)
        ENDIF
        ! Update damage variable
        IF (IFUNC(2) == 0) THEN
          IF (PLA_DAM(I) >= EPSPR) THEN
            IF (OFF(I) == ONE) THEN 
              OFF(I)      = FOUR_OVER_5
              IDEL7NOK    = 1
              NINDX       = NINDX+1
              INDX(NINDX) = I
            ENDIF
            DAM(I)    = ONE
            SIGNXX(I) = ZERO
            SIGNYY(I) = ZERO
            SIGNZZ(I) = ZERO
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO
          ELSEIF (PLA_DAM(I) >= EPSPF) THEN  
            DAM(I) = (PLA_DAM(I) - EPSPF)/(EPSPR - EPSPF)
            DAM(I) = MIN(DAM(I),ONE)
          ENDIF
        ! -> Tabulated damage
        ELSE
          DAM(I) = ABS(YFAC(2))*FINTER(IFUNC(2),PLA_DAM(I),NPF,TF,DF)
          DAM(I) = MIN(DAM(I),ONE)
          IF (DAM(I) >= ONE) THEN 
            IF (OFF(I) == ONE) THEN 
              OFF(I) = FOUR_OVER_5
              IDEL7NOK    = 1
              NINDX       = NINDX+1
              INDX(NINDX) = I
            ENDIF
            DAM(I)    = ONE
            SIGNXX(I) = ZERO
            SIGNYY(I) = ZERO
            SIGNZZ(I) = ZERO
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO
          ENDIF 
        ENDIF
        ! Update user variable
        UVAR(I,1) = PLAT(I)          
        UVAR(I,2) = PLAC(I)          
        UVAR(I,3) = PLAS(I)    
        DPDT_T    = DPLAT(I)/MAX(TIMESTEP,EM20)
        UVAR(I,4) = ASRATE*DPDT_T + (ONE-ASRATE)*EPSPT(I)   
        DPDT_C    = DPLAC(I)/MAX(TIMESTEP,EM20)        
        UVAR(I,5) = ASRATE*DPDT_C + (ONE-ASRATE)*EPSPC(I) 
        DPDT_S    = DPLAS(I)/MAX(TIMESTEP,EM20)          
        UVAR(I,6) = ASRATE*DPDT_S + (ONE-ASRATE)*EPSPS(I)
        UVAR(I,7) = DAM(I)
        DPDT      = DPLA(I)/MAX(TIMESTEP,EM20) 
        EPSD(I)   = ASRATE*DPDT   + (ONE-ASRATE)*EPSD(I)
        IF (INLOC > 0) UVAR(I,8) = UVAR(I,8) + MAX(DPLANL(I),ZERO)
        ! Computation of soundspeed
        SOUNDSP(I) = SQRT((C1+FOUR*G/THREE)/RHO0(I)) 
        ! Computation of the yield stress
        YLD(I)    = A0(I) + A1(I)*P(I) + A2(I)*P(I)*P(I)
        ! Computation of the hourglass coefficient
        ET(I)     = HALF
      ENDDO         
c      
      !====================================================================
      ! - PRINTOUT ELEMENT DELETION
      !====================================================================  
      IF (NINDX > 0) THEN
        DO I=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(I))
          WRITE(ISTDO,1100) NGL(INDX(I)),TIME
#include "lockoff.inc"
        ENDDO
      ENDIF
c------------------------------------------------------      
 1000 FORMAT(1X,'RUPTURE (SAMP) OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE (SAMP) OF SOLID ELEMENT NUMBER ',I10,'AT TIME :',G11.4)  
c------------------------------------------------------      
      END
