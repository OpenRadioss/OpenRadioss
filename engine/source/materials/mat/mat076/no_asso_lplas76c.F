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
Chd|  NO_ASSO_LPLAS76C              source/materials/mat/mat076/no_asso_lplas76c.F
Chd|-- called by -----------
Chd|        SIGEPS76C                     source/materials/mat/mat076/sigeps76c.F
Chd|-- calls ---------------
Chd|        TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|        TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|====================================================================
      SUBROUTINE NO_ASSO_LPLAS76C(
     .     NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .     NPF     ,TF      ,NUMTABL ,TABLE   ,
     .     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO     ,
     .     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     .     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .     PLA     ,DPLA    ,EPSD    ,OFF     ,GS      ,
     .     YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  )
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
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,NFUNC,NUMTABL,INLOC
      INTEGER, DIMENSION(NFUNC), INTENT(IN) :: IFUNC
      my_real, INTENT(IN) :: TIME,TIMESTEP
      my_real, DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
      my_real, DIMENSION(NEL), INTENT(IN) :: GS,RHO,DPLANL,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      TYPE(TABLE_4D_), DIMENSION(NUMTABL), INTENT(IN) :: TABLE
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(NEL), INTENT(OUT) :: 
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,DEZZ,DPLA
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
      my_real, DIMENSION(NEL), INTENT(INOUT) :: OFF,YLD,PLA,EPSD,SOUNDSP
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER :: NPF(SNPC)
      my_real :: FINTER, TF(STF)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,ITER,NITER,ICONV,ICAS,NINDX
      INTEGER, PARAMETER :: FUNC_TRAC  = 1
      INTEGER, PARAMETER :: FUNC_COMP  = 2
      INTEGER, PARAMETER :: FUNC_SHEAR = 3
      INTEGER, DIMENSION(NEL)   :: INDX
      INTEGER, DIMENSION(NEL,2) :: IPOS
      my_real :: LAM,DLAM,DF1,EPSDOT,DA0,DA1,DA2,DYDX,
     .  CA,CB,AA,BB,CC,A1S,A1C,A1T,A2S,A2C,A2T,E,NU,NU1,NUPC,XFAC,
     .  YY,DX2,DFDSIGDLAM,YLD_NORM,EPD_MIN,EPD_MAX,
     .  NORMXX,NORMYY,NORMXY,NORMZZ,NORMYZ,NORMZX,ALFA,ALFI,DTINV,
     .  NORMGXX,NORMGYY,NORMGXY,NORMGZZ,SIG_DFDSIG,
     .  EPDT_MIN,EPDT_MAX,EPDC_MIN,EPDC_MAX,EPDS_MIN,EPDS_MAX
      my_real, DIMENSION(NEL) :: FF,P,SVM,SVM2,YLDS,SXX,SYY,SXY,SZZ,
     .  DPXX,DPYY,DPXY,DPZZ,SIGT,SIGC,SIGS,DFT,DFC,DFS,A11_2D,A12_2D,G,
     .  A0,A1,A2,NUP,EPSPT,EPSPC,EPSPS,EPDT,EPDC,EPDS,
     .  EPDT_F,EPDC_F,EPDS_F,DPLAT,DPLAC,DPLAS,GF,ALPHA,DLAM_NL
      my_real, DIMENSION(NEL,2)   :: XVEC
      my_real, DIMENSION(NUMTABL) :: TFAC
      my_real, DIMENSION(NFUNC)   :: YFAC
      LOGICAL :: CONV(NEL)
      my_real, PARAMETER :: SFAC = 1.05D0 ! Security factor of ICONV
c-----------------------------------------------
c     associated plasticity with quadratic yield function
c-----------------------------------------
c     icas      ifunt   | ifunc   | ifuncs
c       -1         1    |    1    |    1
c        0         1    |    0    |    0
c        1         1    |    1    |    0
c        2         1    |    0    |    1   
c-----------------------------------------
c     UVAR(1)  : EPSPT
c     UVAR(2)  : EPSPC
c     UVAR(3)  : EPSPS
c     UVAR(4)  : EPDT
c     UVAR(5)  : EPDC
c     UVAR(6)  : EPDS
c     UVAR(7)  : NUP
C=======================================================================
c
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! -> Elastic parameters
      E     = UPARAM(1)                       
      NU    = UPARAM(5)  
      ! -> Plastic parameters     
      NUPC  = UPARAM(9)    
      ! -> Flags                    
      ICONV = NINT(UPARAM(15))
      ICAS  = UPARAM(17)
      XFAC  = UPARAM(18)
      ALFA  = MIN(ONE, UPARAM(16)*TIMESTEP)! filtering coefficient for plastic strain rate
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
      A11_2D(1:NEL) = UPARAM(2)    ! E / (ONE - NU*NU)
      A12_2D(1:NEL) = UPARAM(3)    ! AA2 * NU
      G(1:NEL)      = UPARAM(4)  
      NU1   = NU/(ONE - NU) ! aa1/aa2     
      ALFI  = ONE - ALFA
      DTINV = ONE / MAX(EM20, TIMESTEP)
c
      ! Initialize plastic Poisson ratio
      IF (TIME == ZERO) THEN
        NUP(1:NEL)    = NUPC
        UVAR(1:NEL,7) = NUPC
      ELSE
        NUP(1:NEL) = UVAR(1:NEL,7)
      ENDIF
c
      ! Recovering internal variables
      EPSPT(1:NEL)  = UVAR(1:NEL,1)    
      EPSPC(1:NEL)  = UVAR(1:NEL,2)    
      EPSPS(1:NEL)  = UVAR(1:NEL,3)
      EPDT_F(1:NEL) = UVAR(1:NEL,4)
      EPDC_F(1:NEL) = UVAR(1:NEL,5)
      EPDS_F(1:NEL) = UVAR(1:NEL,6)
      DPLAT(1:NEL)  = ZERO 
      DPLAC(1:NEL)  = ZERO 
      DPLAS(1:NEL)  = ZERO 
      DO I=1,NEL
        EPDT(I) = MIN(EPDT_MAX, MAX(EPDT_F(I),EPDT_MIN)) * XFAC
        EPDC(I) = MIN(EPDC_MAX, MAX(EPDC_F(I),EPDC_MIN)) * XFAC
        EPDS(I) = MIN(EPDS_MAX, MAX(EPDS_F(I),EPDS_MIN)) * XFAC
      ENDDO
c
      ! Computation of yield stresses
      XVEC(1:NEL,1) = EPSPT(1:NEL)
      XVEC(1:NEL,2) = EPDT(1:NEL)
      IPOS(1:NEL,1) = 1
      IPOS(1:NEL,2) = 1
      CALL TABLE_MAT_VINTERP(TABLE(FUNC_TRAC),NEL,NEL,IPOS,XVEC,SIGT,DFT)
      SIGT(1:NEL) = SIGT(1:NEL) * TFAC(1)
      DFT(1:NEL)  = DFT(1:NEL)  * TFAC(1)
      IPOS(1:NEL,1) = 1
      IPOS(1:NEL,2) = 1
      IF (TABLE(FUNC_COMP)%NOTABLE  > 0) THEN
        XVEC(1:NEL,1) = EPSPC(1:NEL)
        XVEC(1:NEL,2) = EPDC(1:NEL)
        CALL TABLE_MAT_VINTERP(TABLE(FUNC_COMP),NEL,NEL,IPOS,XVEC,SIGC,DFC)
        SIGC(1:NEL) = SIGC(1:NEL) * TFAC(2)
        DFC(1:NEL)  = DFC(1:NEL)  * TFAC(2)
      END IF
      IPOS(1:NEL,1) = 1
      IPOS(1:NEL,2) = 1
      IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) THEN
        XVEC(1:NEL,1) = EPSPS(1:NEL)
        XVEC(1:NEL,2) = EPDS(1:NEL)
        CALL TABLE_MAT_VINTERP(TABLE(FUNC_SHEAR),NEL,NEL,IPOS,XVEC,SIGS,DFS)
        SIGS(1:NEL) = SIGS(1:NEL) * TFAC(3)
        DFS(1:NEL)  = DFS(1:NEL)  * TFAC(3)
      END IF
      IF (ICAS == 0) THEN 
        SIGC(1:NEL) = SIGT(1:NEL)
        SIGS(1:NEL) = SIGT(1:NEL)/SQR3                                       
      ELSEIF (ICAS == 1) THEN 
        DO I=1,NEL 
          SIGS(I) = ONE /(SIGT(I) + SIGC(I))/SQR3
          SIGS(I) = TWO*SIGT(I)*SIGC(I)*SIGS(I)
        ENDDO     
      ENDIF
      ! Ensured convexity 
      IF (ICONV == 1) THEN
        DO I=1,NEL 
          CONV(I) = .FALSE.
          AA = ONE /(SIGT(I) + SIGC(I))/SQR3
          IF (SIGS(I) < SFAC*TWO*SIGT(I)*SIGC(I)*AA) THEN 
            SIGS(I) = SFAC*TWO*SIGT(I)*SIGC(I)*AA
            CONV(I) = .TRUE.
          ENDIF
        END DO
      END IF
      DO I=1,NEL 
        A0(I) = SIGS(I)*SQR3
        A1(I) = THREE*(((SIGT(I)-SIGC(I))/(SIGT(I)+SIGC(I))) - 
     .          A0(I)*((SIGT(I)-SIGC(I))/(SIGT(I)*SIGC(I))))
        A2(I) = EIGHTEEN*((ONE/(SIGT(I)+SIGC(I)))-A0(I)/(TWO*SIGT(I)*SIGC(I)))       
      END DO
c
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================   
      DO I=1,NEL
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + A11_2D(I)*DEPSXX(I) + A12_2D(I)*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A11_2D(I)*DEPSYY(I) + A12_2D(I)*DEPSXX(I)
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G(I)
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*GS(I)
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*GS(I)
        P(I) = -(SIGNXX(I) + SIGNYY(I)) * THIRD
        ! Computation of the deviatoric trial stress tensor
        SXX(I) = SIGNXX(I)  + P(I)
        SYY(I) = SIGNYY(I)  + P(I)
        SZZ(I) = P(I)
        DEZZ(I)= -NU1 * (DEPSXX(I) + DEPSYY(I))
        SOUNDSP(I) = SQRT(A11_2D(I)/RHO(I))
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !======================================================================== 
      ! Alpha coefficient for non associated function
      DO I = 1,NEL
        ALPHA(I) = (NINE/TWO)*((ONE-TWO*NUP(I))/(ONE+NUP(I)))
      ENDDO
      NITER = 4
      NINDX = 0
      DO I=1,NEL
        SVM2(I) = THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SIGNXY(I)**2
        SVM(I)  = SQRT(MAX(EM20,SVM2(I)))
        YLDS(I) = SVM(I) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
        IF (YLDS(I) > 0 .AND. OFF(I) == ONE) THEN
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
        DO ITER = 1,NITER
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
            ! function g for non associated flow rule         
            GF(I)   = SQRT(MAX(EM20,(SVM(I)**2)+ALPHA(I)*P(I)**2))
c
            ! dgf/dsig for non-associated plastic flow
            NORMGXX = (THREE_HALF*SXX(I) - THIRD*ALPHA(I)*P(I) ) /GF(I)
            NORMGYY = (THREE_HALF*SYY(I) - THIRD*ALPHA(I)*P(I) ) /GF(I)
            NORMGZZ = (THREE_HALF*SZZ(I) - THIRD*ALPHA(I)*P(I) ) /GF(I)
            NORMGXY = THREE*SIGNXY(I)/GF(I)
c                                                               
            ! df/dsig
            CB = A1(I) + TWO*A2(I)*P(I) 
            NORMXX  = THREE_HALF * SXX(I)/SVM(I) + CB /THREE ! DF/DSIG
            NORMYY  = THREE_HALF * SYY(I)/SVM(I) + CB /THREE 
            NORMZZ  = THREE_HALF * SZZ(I)/SVM(I) + CB /THREE  
            NORMXY  = THREE * SIGNXY(I)/SVM(I) 
c
            ! DF/DSIG * DSIG/DDLAM
            DFDSIGDLAM = NORMXX * (A11_2D(I)*NORMGXX + A12_2D(I)*NORMGYY) 
     .                 + NORMYY * (A11_2D(I)*NORMGYY + A12_2D(I)*NORMGXX) 
     .                 + NORMXY * NORMGXY * G(I) 
c
            YLD_NORM = SVM(I)/GF(I)                                                              
            BB = THREE_HALF/(ONE + NUP(I))                                                                    
            DFT(I) = DFT(I) * YLD_NORM * BB                                          
            IF (TABLE(FUNC_COMP)%NOTABLE  > 0) DFC(I) = DFC(I) * YLD_NORM * BB
            IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) DFS(I) = DFS(I) * YLD_NORM * SQR3/TWO 
            IF (ICAS == 0) THEN 
              DFC(I) = DFT(I) 
              DFS(I) = (ONE/SQR3)*DFT(I)
            ELSEIF (ICAS == 1) THEN 
              AA = ONE /(SIGT(I) + SIGC(I))/SQR3                                                
              DFS(I) = TWO*(DFT(I)*SIGC(I) + DFC(I)*SIGT(I))*AA                 
     .               - TWO*SQR3*SIGC(I)*SIGT(I)*(DFT(I) + DFC(I))*AA*AA
            ENDIF 
            IF (ICONV == 1) THEN                                         
              IF (CONV(I)) THEN 
                AA = ONE /(SIGT(I) + SIGC(I))/SQR3                                                
                DFS(I) = SFAC*(TWO*(DFT(I)*SIGC(I) + DFC(I)*SIGT(I))*AA                 
     .                 - TWO*SQR3*SIGC(I)*SIGT(I)*(DFT(I) + DFC(I))*AA*AA)
              ENDIF 
            ENDIF
c                                     
            ! derivatives dAi/dlam                
            CC = SIGS(I)/SIGC(I)/SIGT(I)
c
            A1S =  -THREE*SQR3*(SIGT(I)-SIGC(I))/(SIGT(I)*SIGC(I))
            A1C =  THREE*(   (SIGS(I)*SQR3/(             SIGC(I)**2)) -
     .                       (TWO*SIGT(I)/( (SIGT(I) + SIGC(I))**2))   )
            A1T = THREE*(TWO*SIGC(I)/((SIGT(I) + SIGC(I))**2) - 
     .                             (SIGS(I)*SQR3/(SIGT(I)**2)))
c
            A2S = -NINE*SQR3/(SIGT(I)*SIGC(I))                                           
            A2C = EIGHTEEN*((SIGS(I)*SQR3/(TWO*SIGT(I)*(SIGC(I)**2))) -
     .                              ONE/((SIGT(I)+SIGC(I))**2))                      
            A2T = EIGHTEEN*((SIGS(I)*SQR3/(TWO*SIGC(I)*(SIGT(I)**2))) -
     .                              ONE/((SIGT(I)+SIGC(I))**2))                    
c                                                                   
            DA0 = SQR3*DFS(I)                                  
            DA1 = A1S*DFS(I) + A1T*DFT(I)  + A1C*DFC(I)                     
            DA2 = A2S*DFS(I) + A2T*DFT(I)  + A2C*DFC(I)         
c
            FF(I) = DFDSIGDLAM + DA0 + P(I)*DA1 + P(I)**2 * DA2        
            FF(I) = SIGN(MAX(ABS(FF(I)),EM20) ,FF(I))      
c                 
            DLAM    = YLDS(I)/FF(I)                          
            !  Plastic strains tensor update
            DPXX(I) = DLAM * NORMGXX  
            DPYY(I) = DLAM * NORMGYY  
            DPZZ(I) = DLAM * NORMGZZ  
            DPXY(I) = DLAM * NORMGXY  
c           
            ! Elasto-plastic stresses update   
            SIGNXX(I) = SIGNXX(I) - (A11_2D(I)*DPXX(I) + A12_2D(I)*DPYY(I))
            SIGNYY(I) = SIGNYY(I) - (A11_2D(I)*DPYY(I) + A12_2D(I)*DPXX(I))
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G(I)
c          
            ! compute EPSPC(I), EPSPT(I), EPSPS(I)
            EPSPT(I) = EPSPT(I) + DLAM* YLD_NORM * BB 
            EPSPC(I) = EPSPT(I)                 
            EPSPS(I) = EPSPS(I) + DLAM* YLD_NORM*SQR3/TWO
c
            PLA(I)  = PLA(I)  + DLAM*YLD_NORM*OFF(I)
            DPLA(I) = DPLA(I) + DLAM *YLD_NORM
            DPLAT(I) = DPLAT(I) + DLAM* YLD_NORM*BB 
            DPLAC(I) = DPLAT(I)
            DPLAS(I) = DPLAS(I) + DLAM* YLD_NORM*SQR3/TWO
c
          ENDDO
c            
          ! Update Yld values and criterion with new plastic strain and strain rate  
          XVEC(1:NEL,1) = EPSPT(1:NEL)
          XVEC(1:NEL,2) = EPDT(1:NEL)
          IPOS(1:NEL,1) = 1
          IPOS(1:NEL,2) = 1
          CALL TABLE_MAT_VINTERP(TABLE(FUNC_TRAC),NEL,NEL,IPOS,XVEC,SIGT,DFT)
          SIGT(1:NEL) = SIGT(1:NEL) * TFAC(1)
          DFT(1:NEL)  = DFT(1:NEL)  * TFAC(1)
          IPOS(1:NEL,1) = 1
          IPOS(1:NEL,2) = 1
          IF (TABLE(FUNC_COMP)%NOTABLE  > 0) THEN
            XVEC(1:NEL,1) = EPSPC(1:NEL)
            XVEC(1:NEL,2) = EPDC(1:NEL)
            CALL TABLE_MAT_VINTERP(TABLE(FUNC_COMP),NEL,NEL,IPOS,XVEC,SIGC,DFC)
            SIGC(1:NEL) = SIGC(1:NEL) * TFAC(2)
            DFC(1:NEL)  = DFC(1:NEL)  * TFAC(2)
          END IF
          IPOS(1:NEL,1) = 1
          IPOS(1:NEL,2) = 1
          IF (TABLE(FUNC_SHEAR)%NOTABLE > 0) THEN
            XVEC(1:NEL,1) = EPSPS(1:NEL)
            XVEC(1:NEL,2) = EPDS(1:NEL)
            CALL TABLE_MAT_VINTERP(TABLE(FUNC_SHEAR),NEL,NEL,IPOS,XVEC,SIGS,DFS)
            SIGS(1:NEL) = SIGS(1:NEL) * TFAC(3)
            DFS(1:NEL)  = DFS(1:NEL)  * TFAC(3)
          END IF
          IF (ICAS == 0) THEN
            DO II = 1,NINDX         
              I = INDX(II) 
              SIGC(I) = SIGT(I)
              SIGS(I) = SIGT(I)/SQR3 
            ENDDO                                     
          ELSEIF (ICAS == 1) THEN 
            DO II = 1,NINDX         
              I = INDX(II)          
              SIGS(I) = ONE /(SIGT(I) + SIGC(I))/SQR3
              SIGS(I) = TWO*SIGT(I)*SIGC(I)*SIGS(I)
            ENDDO     
          ENDIF 
          IF (ICONV == 1) THEN         
            DO II = 1,NINDX         
              I = INDX(II) 
              CONV(I) = .FALSE.         
              AA = ONE /(SIGT(I) + SIGC(I))/SQR3
              IF (SIGS(I) < SFAC*TWO*SIGT(I)*SIGC(I)*AA) THEN 
                SIGS(I) = SFAC*TWO*SIGT(I)*SIGC(I)*AA
                CONV(I) = .TRUE.
              ENDIF
            END DO
          END IF
          DO II = 1,NINDX         
            I = INDX(II)          
            A0(I) = SIGS(I)*SQR3
            A1(I) = THREE*(((SIGT(I)-SIGC(I))/(SIGT(I)+SIGC(I))) - 
     .             A0(I)*((SIGT(I)-SIGC(I))/(SIGT(I)*SIGC(I))))
            A2(I) = EIGHTEEN*((ONE/(SIGT(I)+SIGC(I)))-A0(I)/(TWO*SIGT(I)*SIGC(I)))       
          END DO
c
          ! Update yield function value    
          DO II = 1,NINDX         
            I  = INDX(II)          
            P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) )    
            SXX(I) = SIGNXX(I) + P(I)                              
            SYY(I) = SIGNYY(I) + P(I)                              
            SZZ(I) = P(I)
            SVM2(I)= THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SIGNXY(I)**2
            SVM(I) = SQRT(SVM2(I))
            YLDS(I) = SVM(I) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
            IF (INLOC == 0) THEN
              DEZZ(I) = DEZZ(I) + NU1*(DPXX(I) + DPYY(I)) + DPZZ(I)  
            ENDIF 
          ENDDO
        ENDDO   ! End Newton iterations
      ENDIF  ! Plasticity
c
      !====================================================================
      ! - UPDATE PLASTIC POISSON RATIO
      !====================================================================
      IF (IFUNC(1) > 0) THEN
        DO I=1,NEL
          NUP(I)    = FINTER(IFUNC(1),PLA(I),NPF,TF,DYDX) * YFAC(1)
          UVAR(I,7) = MAX(ZERO, MIN(NUP(I), HALF))
        ENDDO
      END IF
c 
      !====================================================================
      ! - NON-LOCAL THICKNESS VARIATION
      !====================================================================
      IF (INLOC > 0) THEN 
        DO I = 1,NEL   
          ALPHA(I)= (NINE/TWO)*((ONE-TWO*NUP(I))/(ONE+NUP(I)))
          GF(I)   = SQRT(MAX(EM20,(SVM(I)**2)+ALPHA(I)*P(I)**2))
          NORMGXX = (THREE_HALF*SXX(I) - THIRD*ALPHA(I)*P(I))/GF(I)
          NORMGYY = (THREE_HALF*SYY(I) - THIRD*ALPHA(I)*P(I))/GF(I)
          NORMGZZ = (THREE_HALF*SZZ(I) - THIRD*ALPHA(I)*P(I))/GF(I)
          YLD_NORM = SVM(I)/GF(I)
          IF (YLD_NORM /= ZERO) THEN 
            DLAM_NL(I) = (ONE/YLD_NORM)*MAX(DPLANL(I),ZERO)
            DEZZ(I) = DEZZ(I) + NU1*(DLAM_NL(I)*NORMGXX)
     .                        + NU1*(DLAM_NL(I)*NORMGYY)
     .                        + DLAM_NL(I)*NORMGZZ
          ENDIF          
        ENDDO
      ENDIF
c
      !====================================================================
      ! - STORING NEW VALUES
      !====================================================================
      UVAR(1:NEL,1) = EPSPT(1:NEL)       
      UVAR(1:NEL,2) = EPSPC(1:NEL)       
      UVAR(1:NEL,3) = EPSPS(1:NEL)       
      UVAR(1:NEL,4) = ALFA*DPLAT(1:NEL)*DTINV + ALFI*EPDT_F(1:NEL)
      UVAR(1:NEL,5) = ALFA*DPLAC(1:NEL)*DTINV + ALFI*EPDC_F(1:NEL)
      UVAR(1:NEL,6) = ALFA*DPLAS(1:NEL)*DTINV + ALFI*EPDS_F(1:NEL)
      EPSD(1:NEL)   = ALFA*DPLA(1:NEL)*DTINV  + ALFI*EPSD(1:NEL)
c
      END
