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
Chd|  NO_ASSO_PLAS76                source/materials/mat/mat076/no_asso_plas76.F
Chd|-- called by -----------
Chd|        SIGEPS76                      source/materials/mat/mat076/sigeps76.F
Chd|-- calls ---------------
Chd|        TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|        TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|====================================================================
      SUBROUTINE NO_ASSO_PLAS76(
     1     NEL     ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NVARTMP ,
     2     NPF     ,TF     ,TIME    ,TIMESTEP,UPARAM  ,VARTMP  ,
     3     RHO0    ,PLA    ,DPLA    ,ET      ,NUMTABL ,TABLE   ,
     3     DEPSXX  ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4     SIGOXX  ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5     SIGNXX  ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6     SOUNDSP ,UVAR   ,OFF     ,EPSD    ,YLD     )
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,NUMTABL
      INTEGER ,INTENT(IN)    :: NVARTMP
      my_real,INTENT(IN) :: TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM),INTENT(IN) :: UPARAM(NUPARAM)
      my_real,DIMENSION(NEL),INTENT(IN) :: 
     .   RHO0,DEPSXX,DEPSYY,DEPSZZ,
     .   DEPSXY,DEPSYZ,DEPSZX,SIGOXX,SIGOYY,SIGOZZ,
     .   SIGOXY,SIGOYZ,SIGOZX 
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,DIMENSION(NEL),INTENT(OUT) ::
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX,
     .   SOUNDSP,DPLA,ET
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      my_real,DIMENSION(NEL),INTENT(INOUT) :: 
     .   OFF,YLD,PLA,EPSD
      my_real,DIMENSION(NEL,NUVAR),INTENT(INOUT) :: 
     .   UVAR
      TYPE(TABLE_4D_),DIMENSION(NUMTABL),INTENT(IN) :: TABLE
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER :: NPF(SNPC), IFUNC(NFUNC)
      my_real :: TF(STF)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,ICONV,NINDX,ICAS,ITER,IQUAD
      INTEGER,PARAMETER :: FUNC_TRAC  = 1
      INTEGER,PARAMETER :: FUNC_COMP  = 2
      INTEGER,PARAMETER :: FUNC_SHEAR = 3
      INTEGER,DIMENSION(NEL)   :: INDX,IAD,ILEN
      my_real :: G,AA1,AA2,C1,NUPC,XFAC
      my_real ,DIMENSION(NEL) :: PLAT,PLAC,PLAS,EPSPT,EPSPC,EPSPS,SIGT,
     .    SIGS,DSIGS_DP,SIGC,DSIGC_DP,SDXX,SDYY,SDZZ,SDXY,SDYZ,SDZX,DSIGT_DP,
     .    NUP,P,SVM,A0,A1,A2,PHI,DPLAC,DPLAT,DPLAS,PSI,ALPHA
      my_real
     .    NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,NORM,AA,CB,
     .    NORMXX_N,NORMYY_N,NORMZZ_N,NORMXY_N,NORMYZ_N,NORMZX_N,
     .    DSIGT_DLAM,DSIGC_DLAM,DSIGS_DLAM,DA1_DSIGS,DA1_DSIGT,
     .    DA1_DSIGC,DA2_DSIGS,DA2_DSIGT,DA2_DSIGC,DA0_DLAM,DA1_DLAM,
     .    DA2_DLAM,DFDSIG2,DPHI_DLAM,DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX,
     .    DPDT_C,DPDT_S,DPDT_T,DPDT,DYDX,DLAM,CC,EPDT_MIN,DA0_DSIGS,
     .    EPDT_MAX,EPDC_MAX,EPDC_MIN,EPDS_MIN,EPDS_MAX,ASRATE
      my_real, DIMENSION(NEL,2)   :: XVEC
      my_real, DIMENSION(NUMTABL) :: TFAC
      my_real, DIMENSION(NFUNC)   :: YFAC
      LOGICAL :: CONV(NEL)
      my_real, PARAMETER :: SFAC = 1.05D0 ! Security factor of ICONV
C
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! -> Elastic parameters
      G      = UPARAM(4) ! Shear modulus                
      AA1    = UPARAM(6) ! First component of elastic matrix
      AA2    = UPARAM(7) ! Second component of elastic matrix
      C1     = UPARAM(8) ! Bulk modulus
      ! -> Plastic parameters
      NUPC   = UPARAM(9)  ! Plastic Poisson ratio                      
      ! -> Flags 
      IQUAD  = NINT(UPARAM(14)) ! Flag for quadratic or non-quatratic yield surface
      ASRATE = MIN(ONE,UPARAM(16)*TIMESTEP)
      ICONV  = NINT(UPARAM(15)) ! Flag to ensure convexity
      ICAS   = NINT(UPARAM(17)) ! Flag for tabulated case
      !icas      ifunt   | ifunc   | ifuncs
      !  -1         1    |    1    |    1
      !   0         1    |    0    |    0
      !   1         1    |    1    |    0
      !   2         1    |    0    |    1
      XFAC     = UPARAM(18) ! Strain-rate scale factor
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
      ! Initialize plastic Poisson ratio
      IF (TIME == ZERO) THEN
        NUP(1:NEL)    = NUPC
        UVAR(1:NEL,7) = NUPC
      ELSE
        NUP(1:NEL) = UVAR(1:NEL,7)
      END IF   
c
      ! Recovering internal variables
      DO I=1,NEL     
        PLAT(I)  = UVAR(I,1) ! Uniaxial tension plastic strain    
        PLAC(I)  = UVAR(I,2) ! Uniaxial compression plastic strain    
        PLAS(I)  = UVAR(I,3) ! Shear plastic strain
        EPSPT(I) = UVAR(I,4) ! Uniaxial tension plastic strain-rate
        EPSPC(I) = UVAR(I,5) ! Uniaxial compression plastic strain-rate
        EPSPS(I) = UVAR(I,6) ! Shear plastic strain-rate
        EPSPT(I) = MIN(EPDT_MAX, MAX(EPSPT(I),EPDT_MIN))
        EPSPC(I) = MIN(EPDC_MAX, MAX(EPSPC(I),EPDC_MIN))
        EPSPS(I) = MIN(EPDS_MAX, MAX(EPSPS(I),EPDS_MIN))
        ALPHA(I) = (NINE/TWO)*((ONE-TWO*NUP(I))/(ONE+NUP(I)))
        DPLA(I)  = ZERO
        DPLAC(I) = ZERO
        DPLAT(I) = ZERO
        DPLAS(I) = ZERO
      ENDDO
C
      ! Computation of yield stresses
      XVEC(1:NEL,1)   = PLAT(1:NEL)
      XVEC(1:NEL,2)   = EPSPT(1:NEL)*XFAC
      CALL TABLE_MAT_VINTERP(TABLE(FUNC_TRAC),NEL,NEL,VARTMP(1,1),XVEC,SIGT,DSIGT_DP)
      SIGT     = SIGT*TFAC(1)
      DSIGT_DP = DSIGT_DP*TFAC(1)
      IF (TABLE(FUNC_COMP)%NOTABLE > 0) THEN 
        XVEC(1:NEL,1)   = PLAC(1:NEL)
        XVEC(1:NEL,2)   = EPSPC(1:NEL)*XFAC
        CALL TABLE_MAT_VINTERP(TABLE(FUNC_COMP),NEL,NEL,VARTMP(1,3),XVEC,SIGC,DSIGC_DP)
        SIGC     = SIGC*TFAC(2)
        DSIGC_DP = DSIGC_DP*TFAC(2)
      ENDIF
      IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) THEN 
        XVEC(1:NEL,1)   = PLAS(1:NEL)
        XVEC(1:NEL,2)   = EPSPS(1:NEL)*XFAC
        CALL TABLE_MAT_VINTERP(TABLE(FUNC_SHEAR),NEL,NEL,VARTMP(1,5),XVEC,SIGS,DSIGS_DP)
        SIGS     = SIGS*TFAC(3)
        DSIGS_DP = DSIGS_DP*TFAC(3)
      ENDIF 
      ! Select case for tabulated yield stresses
      IF (ICAS == 0) THEN 
        SIGC(1:NEL) = SIGT(1:NEL)
        SIGS(1:NEL) = SIGT(1:NEL)/SQR3
      ELSEIF (ICAS == 1) THEN 
        IF (IQUAD == 1) THEN 
          DO I = 1,NEL
            SIGS(I) = SQRT(SIGC(I)*SIGT(I)/THREE)
          ENDDO
        ELSEIF (IQUAD == 0) THEN 
          DO I = 1,NEL
            SIGS(I) = ONE /(SIGT(I) + SIGC(I))/SQR3
            SIGS(I) = TWO*SIGT(I)*SIGC(I)*SIGS(I)
          ENDDO
        ENDIF
      ENDIF
C
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================      
      DO I=1,NEL
C        
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + (AA1*DEPSXX(I)
     .                        +  AA2*(DEPSYY(I) + DEPSZZ(I)))
        SIGNYY(I) = SIGOYY(I) + (AA1*DEPSYY(I)
     .                        +  AA2*(DEPSXX(I) + DEPSZZ(I)))
        SIGNZZ(I) = SIGOZZ(I) + (AA1*DEPSZZ(I)
     .                        +  AA2*(DEPSXX(I) + DEPSYY(I)))
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
          AA = ONE /(SIGT(I) + SIGC(I))/SQR3
          IF ((IQUAD == 1) .AND. (SIGS(I) < SFAC*SQRT(SIGC(I)*SIGT(I)/THREE))) THEN 
            SIGS(I) = SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)
            CONV(I) = .TRUE.
          ELSEIF ((IQUAD == 0) .AND. (SIGS(I) < SFAC*TWO*SIGT(I)*SIGC(I)*AA)) THEN 
            SIGS(I) = SFAC*TWO*SIGT(I)*SIGC(I)*AA
            CONV(I) = .TRUE.
          ENDIF
        ENDDO   
      ENDIF
      ! Compute yield criterion parameters A0,A1,A2
      IF (IQUAD == 1) THEN 
        DO I = 1,NEL
          AA    = ONE/SIGC(I)/SIGT(I)
          A0(I) = THREE*(SIGS(I)**2)
          A1(I) = NINE*(SIGS(I)**2)*(SIGC(I) - SIGT(I))*AA
          A2(I) = NINE*(SIGC(I)*SIGT(I) - THREE*(SIGS(I)**2))*AA
        ENDDO   
      ELSE
        DO I = 1,NEL
          A0(I) = SIGS(I)*SQR3
          A1(I) = THREE*(((SIGT(I)-SIGC(I))/(SIGT(I)+SIGC(I))) - 
     .             A0(I)*((SIGT(I)-SIGC(I))/(SIGT(I)*SIGC(I))))
          A2(I) = EIGHTEEN*((ONE/(SIGT(I)+SIGC(I)))-A0(I)/(TWO*SIGT(I)*SIGC(I)))       
        ENDDO   
      ENDIF
c
      ! -> Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL
        IF (IQUAD == 1) THEN 
          PHI(I) = (SVM(I)**2) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
        ELSE
          PHI(I) = SVM(I) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
        ENDIF
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
            ! the consistency condition using the cutting plane algorithm
            ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
            ! -> PHI          : current value of yield function (known)
            ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
            !                   into account of internal variables kinetic : 
            !                   plasticity ... 
c        
            ! 1 - Computation of DPHI_DSIG the normal to the yield surface
            !-------------------------------------------------------------
C
            ! Non associated flow surface
            PSI(I)   = SQRT((SVM(I)**2)+ALPHA(I)*P(I)**2)
            PSI(I)   = MAX(PSI(I),EM20)
C                  
            ! Computation of normal to non-associated yield surface
            NORMXX_N = ((THREE*SDXX(I)/(TWO*PSI(I))) - THIRD*(ALPHA(I)*P(I)/PSI(I)))
            NORMYY_N = ((THREE*SDYY(I)/(TWO*PSI(I))) - THIRD*(ALPHA(I)*P(I)/PSI(I)))
            NORMZZ_N = ((THREE*SDZZ(I)/(TWO*PSI(I))) - THIRD*(ALPHA(I)*P(I)/PSI(I)))
            NORMXY_N = (THREE*SDXY(I)/(TWO*PSI(I)))
            NORMYZ_N = (THREE*SDYZ(I)/(TWO*PSI(I)))
            NORMZX_N = (THREE*SDZX(I)/(TWO*PSI(I)))
C
            ! Computation of the normal to the yield surface
            IF (IQUAD == 1) THEN 
              NORMXX = THREE*SDXX(I) + THIRD*(A1(I) + TWO*A2(I)*P(I)) 
              NORMYY = THREE*SDYY(I) + THIRD*(A1(I) + TWO*A2(I)*P(I))
              NORMZZ = THREE*SDZZ(I) + THIRD*(A1(I) + TWO*A2(I)*P(I))
              NORMXY = THREE*SDXY(I)
              NORMYZ = THREE*SDYZ(I)
              NORMZX = THREE*SDZX(I)
            ELSE
              NORMXX = THREE_HALF*(SDXX(I)/SVM(I)) + THIRD*(A1(I) + TWO*A2(I)*P(I)) 
              NORMYY = THREE_HALF*(SDYY(I)/SVM(I)) + THIRD*(A1(I) + TWO*A2(I)*P(I))
              NORMZZ = THREE_HALF*(SDZZ(I)/SVM(I)) + THIRD*(A1(I) + TWO*A2(I)*P(I))
              NORMXY = THREE_HALF*(SDXY(I)/SVM(I))
              NORMYZ = THREE_HALF*(SDYZ(I)/SVM(I))
              NORMZX = THREE_HALF*(SDZX(I)/SVM(I))
            ENDIF
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
            DSIGT_DLAM = DSIGT_DP(I)*((SVM(I)/PSI(I))*THREE_HALF/(ONE + NUP(I)))
            IF (TABLE(FUNC_COMP)%NOTABLE  > 0) DSIGC_DLAM = DSIGC_DP(I)*((SVM(I)/PSI(I))*THREE_HALF/(ONE + NUP(I)))
            IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) DSIGS_DLAM = DSIGS_DP(I)*(SQR3/TWO)*(SVM(I)/PSI(I))
            IF (ICAS == 0) THEN 
              DSIGC_DLAM = DSIGT_DLAM 
              DSIGS_DLAM = (ONE/SQR3)*DSIGT_DLAM
            ELSEIF (ICAS == 1) THEN 
              IF (IQUAD == 1) THEN
                DSIGS_DLAM = (ONE/SQR3)*(ONE/(TWO*SQRT(SIGT(I)*SIGC(I))))*
     .                       (DSIGC_DLAM*SIGT(I) + SIGC(I)*DSIGT_DLAM)
              ELSEIF (IQUAD == 0) THEN 
                AA = ONE /(SIGT(I) + SIGC(I))/SQR3
                DSIGS_DLAM = TWO*(DSIGT_DLAM*SIGC(I) + DSIGC_DLAM*SIGT(I))*AA                 
     .                - TWO*SQR3*SIGC(I)*SIGT(I)*(DSIGT_DLAM + DSIGC_DLAM)*AA*AA
              ENDIF
            ENDIF 
            IF (ICONV == 1) THEN                                         
              IF (CONV(I)) THEN 
                IF (IQUAD == 1) THEN
                  DSIGS_DLAM = SFAC*(ONE/SQR3)*(ONE/(TWO*SQRT(SIGT(I)*SIGC(I))))*
     .                         (DSIGC_DLAM*SIGT(I) + SIGC(I)*DSIGT_DLAM)
                ELSEIF (IQUAD == 0) THEN 
                  AA = ONE /(SIGT(I) + SIGC(I))/SQR3
                  DSIGS_DLAM = SFAC*(TWO*(DSIGT_DLAM*SIGC(I) + DSIGC_DLAM*SIGT(I))*AA                 
     .                  - TWO*SQR3*SIGC(I)*SIGT(I)*(DSIGT_DLAM + DSIGC_DLAM)*AA*AA)
                ENDIF           
              ENDIF 
            ENDIF
C
            IF (IQUAD == 1) THEN 
              ! -> A0 derivatives
              DA0_DSIGS = SIX*SIGS(I)
              ! -> A1 derivatives 
              CC = SIGS(I)/SIGC(I)/SIGT(I)
              !   -> With respect to SIGS
              DA1_DSIGS = EIGHTEEN*(SIGC(I) - SIGT(I))*CC
              !   -> With respect to SIGC
              DA1_DSIGC = NINE*(SIGS(I)/SIGC(I))**2
              !   -> With respect to SIGT
              DA1_DSIGT = -NINE*(SIGS(I)/SIGT(I))**2
              ! -> A2 derivatives
              !   -> With respect to SIGS
              DA2_DSIGS = -CINQUANTE4*CC
              !   -> With respect to SIGC                                         
              DA2_DSIGC = TWENTY7*CC*SIGS(I)/SIGC(I)  
              !   -> With respect to SIGT                       
              DA2_DSIGT = TWENTY7*CC*SIGS(I)/SIGT(I)   
            ELSE
              ! -> A0 derivatives
              DA0_DSIGS = SQR3
              ! -> A1 derivatives 
              !   -> With respect to SIGS
              DA1_DSIGS = -THREE*SQR3*(SIGT(I)-SIGC(I))/(SIGT(I)*SIGC(I))
              !   -> With respect to SIGC
              DA1_DSIGC = THREE*((SIGS(I)*SQR3/(SIGC(I)**2))-
     .                             TWO*SIGT(I)/((SIGT(I) + SIGC(I))**2))
              !   -> With respect to SIGT
              DA1_DSIGT = THREE*(TWO*SIGC(I)/((SIGT(I) + SIGC(I))**2) - 
     .                             (SIGS(I)*SQR3/(SIGT(I)**2)))
              ! -> A2 derivatives
              !   -> With respect to SIGS
              DA2_DSIGS = -NINE*SQR3/(SIGT(I)*SIGC(I))
              !   -> With respect to SIGC                                         
              DA2_DSIGC = EIGHTEEN*((SIGS(I)*SQR3/(TWO*SIGT(I)*(SIGC(I)**2))) -
     .                              ONE/((SIGT(I)+SIGC(I))**2))
              !   -> With respect to SIGT                       
              DA2_DSIGT = EIGHTEEN*((SIGS(I)*SQR3/(TWO*SIGC(I)*(SIGT(I)**2))) -
     .                              ONE/((SIGT(I)+SIGC(I))**2))
            ENDIF                  
c             
            ! -> A parameters derivatives with respect to lambda        
            !   -> A0 with respect to lambda                                               
            DA0_DLAM = DA0_DSIGS*DSIGS_DLAM      
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
            PLAT(I)  = PLAT(I) + DLAM*(SVM(I)/PSI(I))*THREE_HALF/(ONE + NUP(I))
            PLAC(I)  = PLAT(I)    
            PLAS(I)  = PLAS(I) + (SQR3/TWO)*(SVM(I)/PSI(I))*DLAM   
            PLA(I)   = PLA(I)  + (SVM(I)/PSI(I))*DLAM  
            ! Plastic strain increments update
            DPLAT(I) = DPLAT(I) + DLAM*(SVM(I)/PSI(I))*THREE_HALF/(ONE + NUP(I))
            DPLAC(I) = DPLAT(I)
            DPLAS(I) = DPLAS(I) + (SQR3/TWO)*(SVM(I)/PSI(I))*DLAM   
            DPLA(I)  = DPLA(I)  + (SVM(I)/PSI(I))*DLAM
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
          CALL TABLE_MAT_VINTERP(TABLE(FUNC_TRAC),NEL,NEL,VARTMP(1,1),XVEC,SIGT,DSIGT_DP)
          SIGT     = SIGT*TFAC(1)
          DSIGT_DP = DSIGT_DP*TFAC(1)
          IF (TABLE(FUNC_COMP)%NOTABLE > 0) THEN 
            XVEC(1:NEL,1)   = PLAC(1:NEL)
            XVEC(1:NEL,2)   = EPSPC(1:NEL)*XFAC
            CALL TABLE_MAT_VINTERP(TABLE(FUNC_COMP),NEL,NEL,VARTMP(1,3),XVEC,SIGC,DSIGC_DP)
            SIGC(1:NEL)     = SIGC*TFAC(2)
            DSIGC_DP(1:NEL) = DSIGC_DP*TFAC(2)
          ENDIF
          IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) THEN 
            XVEC(1:NEL,1)   = PLAS(1:NEL)
            XVEC(1:NEL,2)   = EPSPS(1:NEL)*XFAC
            CALL TABLE_MAT_VINTERP(TABLE(FUNC_SHEAR),NEL,NEL,VARTMP(1,15),XVEC,SIGS,DSIGS_DP)
            SIGS(1:NEL)     = SIGS*TFAC(3) 
            DSIGS_DP(1:NEL) = DSIGS_DP*TFAC(3)
          ENDIF 
          ! Select case for tabulated yield stresses
          IF (ICAS == 0) THEN 
            DO II = 1,NINDX
              I = INDX(II)
              SIGC(I) = SIGT(I)
              SIGS(I) = SIGT(I)/SQR3
            ENDDO
          ELSEIF (ICAS == 1) THEN 
            IF (IQUAD == 1) THEN 
              DO II = 1,NINDX
                I = INDX(II)
                SIGS(I) = SQRT(SIGC(I)*SIGT(I)/THREE)
              ENDDO
            ELSEIF (IQUAD == 0) THEN 
              DO II = 1,NINDX
                I = INDX(II)
                SIGS(I) = ONE /(SIGT(I) + SIGC(I))/SQR3
                SIGS(I) = TWO*SIGT(I)*SIGC(I)*SIGS(I)
              ENDDO                
            ENDIF
          ENDIF
c
          ! Update yield function value
          IF (ICONV == 1) THEN
            DO II = 1,NINDX         
              I = INDX(II)
              AA = ONE /(SIGT(I) + SIGC(I))/SQR3     
              CONV(I) = .FALSE.
              IF ((IQUAD == 1) .AND. (SIGS(I) < SFAC*SQRT(SIGC(I)*SIGT(I)/THREE))) THEN 
                SIGS(I) = SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)
                CONV(I) = .TRUE.
              ELSEIF ((IQUAD == 0) .AND. (SIGS(I) < SFAC*TWO*SIGT(I)*SIGC(I)*AA)) THEN            
                SIGS(I) = SFAC*TWO*SIGT(I)*SIGC(I)*AA
                CONV(I) = .TRUE.
              ENDIF
            ENDDO
          ENDIF
          DO II = 1,NINDX         
            I = INDX(II)
            IF (IQUAD == 1) THEN 
              AA    = ONE/SIGC(I)/SIGT(I)
              A0(I) = THREE*(SIGS(I)**2)
              A1(I) = NINE*(SIGS(I)**2)*(SIGC(I) - SIGT(I))*AA
              A2(I) = NINE*(SIGC(I)*SIGT(I) - THREE*(SIGS(I)**2))*AA
            ELSE
              A0(I) = SIGS(I)*SQR3
              A1(I) = THREE*(((SIGT(I)-SIGC(I))/(SIGT(I)+SIGC(I))) - 
     .                 A0(I)*((SIGT(I)-SIGC(I))/(SIGT(I)*SIGC(I))))
              A2(I) = EIGHTEEN*((ONE/(SIGT(I)+SIGC(I)))-A0(I)/(TWO*SIGT(I)*SIGC(I)))       
            ENDIF
            IF (IQUAD == 1) THEN 
              PHI(I) = (SVM(I)**2) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
            ELSE
              PHI(I) = SVM(I) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
            ENDIF
          ENDDO
        ENDDO 
      ENDIF
c
      !====================================================================
      ! - UPDATE PLASTIC POISSON RATIO
      !====================================================================
      IF (IFUNC(1) > 0) THEN
        IAD(1:NEL)  = NPF(IFUNC(1))   / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - VARTMP(1:NEL,8)
!
        CALL VINTER(TF,IAD,VARTMP(1:NEL,8),ILEN,NEL,PLA,DYDX,NUP)
!        
        UVAR(1:NEL,7) = YFAC(1) * NUP(1:NEL)
        UVAR(1:NEL,7) = MAX(ZERO, MIN(NUP(1:NEL), HALF))
      END IF
c
      !====================================================================
      ! - STORING NEW VALUES
      !====================================================================
      DO I=1,NEL
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
        DPDT      = DPLA(I)/MAX(TIMESTEP,EM20) 
        EPSD(I)   = ASRATE*DPDT   + (ONE-ASRATE)*EPSD(I)
        ! Computation of soundspeed
        SOUNDSP(I) = SQRT((C1+FOUR*G/THREE)/RHO0(I)) 
        ! Computation of the yield stress
        YLD(I)    = A0(I) + A1(I)*P(I) + A2(I)*P(I)*P(I)
        ! Computation of the hourglass coefficient
        ET(I)     = HALF
      ENDDO     
c
      END
