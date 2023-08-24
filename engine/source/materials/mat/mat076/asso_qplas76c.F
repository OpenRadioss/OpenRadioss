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
Chd|  ASSO_QPLAS76C                 source/materials/mat/mat076/asso_qplas76c.F
Chd|-- called by -----------
Chd|        SIGEPS76C                     source/materials/mat/mat076/sigeps76c.F
Chd|-- calls ---------------
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE ASSO_QPLAS76C(
     .     NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     .     NPF     ,TF      ,NUMTABL ,TABLE   ,
     .     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,RHO     ,
     .     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     .     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     .     PLA     ,DPLA    ,EPSD    ,OFF     ,GS      ,
     .     YLD     ,SOUNDSP ,DEZZ    ,INLOC   ,DPLANL  )
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C=======================================================================
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,NUMTABL,INLOC
      INTEGER ,DIMENSION(NFUNC)  ,INTENT(IN)  :: IFUNC
      my_real :: TIME,TIMESTEP
      my_real :: UPARAM(NUPARAM)
      my_real ,DIMENSION(NEL)   :: GS,RHO,SOUNDSP,DPLANL
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(NEL) :: 
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,DEZZ
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL,NUVAR) :: UVAR
      my_real ,DIMENSION(NEL)       :: OFF,YLD,PLA,EPSD
      TYPE(TTABLE), DIMENSION(NUMTABL) , TARGET :: TABLE
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER, TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,KK,ITER,NITER,NXK,IPOSN(NFUNC),ID,NDIM,ICONV,ICAS,
     .   NINDX
      INTEGER, DIMENSION(NEL)   :: INDX
      INTEGER, DIMENSION(NEL,2) :: IPOS
      my_real R(NFUNC)
      my_real :: LAM,DLAM,DF1,DA0,DA1,DA2,DYDX,
     .  CA,CB,AA,BB,CC,A1S,A1C,A1T,A2S,A2C,A2T,E,NU,NU1,NUPC,XFAC,
     .  YY,DX2,NORM,DFDSIGDLAM,YLD_NORM,EPD_MIN,EPD_MAX,DPT,DPS,
     .  NORMXX,NORMYY,NORMXY,NORMZZ,NORMYZ,NORMZX,ALPHA,ALPHI,DTINV,
     .  EPDT_MIN,EPDT_MAX,EPDC_MIN,EPDC_MAX,EPDS_MIN,EPDS_MAX,SIG_DFDSIG
      my_real ,DIMENSION(NEL) :: FF,DPLA,P,SVM,SVM2,YLDS,SXX,SYY,SXY,SZZ,
     .  DPXX,DPYY,DPXY,DPZZ,SIGT,SIGC,SIGS,DFT,DFC,DFS,A11_2D,A12_2D,G,
     .  A0,A1,A2,NUP,DAM,EPSPT,EPSPC,EPSPS,EPDT,EPDC,EPDS,
     .  EPDT_F,EPDC_F,EPDS_F,DPLAT,DPLAC,DPLAS,DLAM_NL
      my_real, DIMENSION(NEL,2)   :: XVEC
      my_real ,DIMENSION(NUMTABL) :: TFAC
      my_real ,DIMENSION(NFUNC)   :: YFAC
      TYPE(TTABLE), POINTER :: FUNC_TRAC,FUNC_COMP,FUNC_SHEAR
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
c     UVAR(7)  : DAM
c     UVAR(8)  : DPLANL
c     UVAR(9)  : NUP
C=======================================================================
      NITER = 4
      NINDX = 0
c
      E     = UPARAM(1)                       
      NU    = UPARAM(5)       
      NUPC  = UPARAM(9)                       
      ICONV = NINT(UPARAM(15))
      ICAS  = UPARAM(17)
      XFAC  = UPARAM(18)
      ALPHA = MIN(ONE, UPARAM(16)*TIMESTEP)
      EPDT_MIN = UPARAM(19)
      EPDT_MAX = UPARAM(20)
      EPDC_MIN = UPARAM(21)
      EPDC_MAX = UPARAM(22)
      EPDS_MIN = UPARAM(23)
      EPDS_MAX = UPARAM(24)
      A11_2D(1:NEL) = UPARAM(2)    ! E / (ONE - NU*NU)
      A12_2D(1:NEL) = UPARAM(3)    ! AA2 * NU
      G(1:NEL)      = UPARAM(4)  
      EPSPT(1:NEL)  = UVAR(1:NEL,1)    
      EPSPC(1:NEL)  = UVAR(1:NEL,2)    
      EPSPS(1:NEL)  = UVAR(1:NEL,3)
      EPDT_F(1:NEL) = UVAR(1:NEL,4)
      EPDC_F(1:NEL) = UVAR(1:NEL,5)
      EPDS_F(1:NEL) = UVAR(1:NEL,6)
      DAM(1:NEL)    = ONE - UVAR(1:NEL,7)
      DPLAT(1:NEL)  = ZERO 
      DPLAC(1:NEL)  = ZERO 
      DPLAS(1:NEL)  = ZERO 
      NU1    = NU/(ONE - NU) ! aa1/aa2
      ALPHI  = ONE-ALPHA
      DTINV  = ONE / MAX(EM20, TIMESTEP)
c
      TFAC(1) = UPARAM(25)
      TFAC(2) = UPARAM(26)
      TFAC(3) = UPARAM(27)
      YFAC(1) = UPARAM(28)
      YFAC(2) = UPARAM(29)
      FUNC_TRAC  => TABLE(1)
      FUNC_COMP  => TABLE(2)
      FUNC_SHEAR => TABLE(3)
c     Initialize plastic Poisson ratio
      IF (TIME == ZERO) THEN
        NUP(1:NEL)    = NUPC
        UVAR(1:NEL,9) = NUPC
      ELSE
        NUP(1:NEL) = UVAR(1:NEL,9)
      END IF   
c
      DO I=1,NEL
        EPDT(I) = MIN(EPDT_MAX, MAX(EPDT_F(I),EPDT_MIN)) * XFAC
        EPDC(I) = MIN(EPDC_MAX, MAX(EPDC_F(I),EPDC_MIN)) * XFAC
        EPDS(I) = MIN(EPDS_MAX, MAX(EPDS_F(I),EPDS_MIN)) * XFAC
      ENDDO
c-----------------------------------------------
      XVEC(1:NEL,1) = EPSPT(1:NEL)
      XVEC(1:NEL,2) = EPDT(1:NEL)
      IPOS(1:NEL,1) = 1
      IPOS(1:NEL,2) = 1
c
      CALL TABLE_VINTERP(FUNC_TRAC,NEL,IPOS,XVEC,SIGT,DFT)
      SIGT(1:NEL) = SIGT(1:NEL) * TFAC(1) * DAM(1:NEL)
      DFT(1:NEL)  = DFT(1:NEL)  * TFAC(1) * DAM(1:NEL)
c
      IPOS(1:NEL,1) = 1
      IPOS(1:NEL,2) = 1
      IF (FUNC_COMP%NOTABLE  > 0) THEN
        XVEC(1:NEL,1) = EPSPC(1:NEL)
        XVEC(1:NEL,2) = EPDC(1:NEL)
        CALL TABLE_VINTERP(FUNC_COMP,NEL,IPOS,XVEC,SIGC,DFC)
        SIGC(1:NEL) = SIGC(1:NEL) * TFAC(2) * DAM(1:NEL)
        DFC(1:NEL)  = DFC(1:NEL)  * TFAC(2) * DAM(1:NEL)
      END IF
      IPOS(1:NEL,1) = 1
      IPOS(1:NEL,2) = 1
      IF (FUNC_SHEAR%NOTABLE > 0) THEN
        XVEC(1:NEL,1) = EPSPS(1:NEL)
        XVEC(1:NEL,2) = EPDS(1:NEL)
        CALL TABLE_VINTERP(FUNC_SHEAR,NEL,IPOS,XVEC,SIGS,DFS)
        SIGS(1:NEL) = SIGS(1:NEL) * TFAC(3) * DAM(1:NEL)
        DFS(1:NEL)  = DFS(1:NEL)  * TFAC(3) * DAM(1:NEL)
      END IF
c
c-----------------------------------------------
      SELECT CASE (ICAS)
        CASE (0)
           SIGC(1:NEL) = SIGT(1:NEL)
           SIGS(1:NEL) = SIGT(1:NEL)/SQR3                                   
        CASE(1)
          DO I=1,NEL 
           SIGS(I) = SQRT(SIGT(I)*SIGC(I)/THREE)
          END DO     
      END SELECT
c-----------------------------------------------
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
      DO I=1,NEL 
        AA = ONE/SIGC(I)/SIGT(I)
        A0(I) = THREE*SIGS(I)**2
        A1(I) = NINE*SIGS(I)**2*(SIGC(I) - SIGT(I))*AA
        A2(I) = NINE*(SIGC(I)*SIGT(I) - THREE*SIGS(I)**2)*AA  
      END DO
c-----------------------------------------------
c     Trial stress
c-----------------------------------------------
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
        DEZZ(I) = -NU1 * (DEPSXX(I) + DEPSYY(I))
        SOUNDSP(I) = SQRT(A11_2D(I)/RHO(I))
      ENDDO
c-----------------------------------------------
c     Check plasticity 
c-----------------------------------------------
      DO I=1,NEL
        SVM2(I) = THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SIGNXY(I)**2
        SVM(I)  = SQRT(SVM2(I))
        YLDS(I) = SVM2(I) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
        IF (YLDS(I) > 0 .AND. OFF(I) == ONE) THEN
          NINDX = NINDX + 1
          INDX(NINDX) = I
        ENDIF
      ENDDO
c-----------------------------------------------
      IF (NINDX > 0) THEN

        DO ITER = 1,NITER
c
          DO II = 1,NINDX         
            I = INDX(II)          
            ! df/dsig
            CB = A1(I) + TWO*A2(I)*P(I)                                                          
            NORM = ONE/MAX(EM20, SQRT(SIX*SVM(I)**2  + THIRD*CB**2)) ! norm df/dsig
            NORMXX  = THREE * SXX(I) + CB /THREE ! DF/DSIG
            NORMYY  = THREE * SYY(I) + CB /THREE 
            NORMZZ  = THREE * SZZ(I) + CB /THREE  
            NORMXY  = TWO *THREE * SIGNXY(I) 

            ! DF/DSIG * DSIG/DDLAM
            DFDSIGDLAM = NORMXX * (A11_2D(I)*NORMXX + A12_2D(I)*NORMYY)*NORM
     .                 + NORMYY * (A11_2D(I)*NORMYY + A12_2D(I)*NORMXX)*NORM
     .                 + NORMXY * NORMXY * G(I)*NORM
c
            YLD_NORM = SVM(I)*NORM                                                             
            BB = THREE/(ONE + NUP(I))                                                                    
            DFT(I) = DFT(I) * YLD_NORM * BB
            IF (FUNC_COMP%NOTABLE  > 0) DFC(I) = DFC(I) * YLD_NORM * BB
            IF (FUNC_SHEAR%NOTABLE > 0) DFS(I) = DFS(I) * YLD_NORM * SQR3/TWO            
            SELECT CASE(ICAS)
              CASE(0)
                DFC(I) = DFT(I) 
                DFS(I) = (ONE/SQR3)*DFT(I)
              CASE(1)
                DFS(I) = (ONE/SQR3)*(ONE/(TWO*SQRT(SIGT(I)*SIGC(I))))*
     .                   (DFC(I)*SIGT(I) + SIGC(I)*DFT(I))
            END SELECT 
            IF (ICONV == 1) THEN                                         
              IF (CONV(I)) THEN 
                DFS(I) = SFAC*(ONE/SQR3)*(ONE/(TWO*SQRT(SIGT(I)*SIGC(I))))*
     .                       (DFC(I)*SIGT(I) + SIGC(I)*DFT(I))
              ENDIF 
            ENDIF
c                                     
c           derivatives dAi/dlam                
            CC = SIGS(I)/SIGC(I)/SIGT(I)
C
            A1S = EIGHTEEN*(SIGC(I) - SIGT(I))*CC
            A1C = +NINE*(SIGS(I)/SIGC(I))**2
            A1T = -NINE*(SIGS(I)/SIGT(I))**2
C
            A2S = -CINQUANTE4*CC                                           
            A2C = TWENTY7*CC*SIGS(I)/SIGC(I)                         
            A2T = TWENTY7*CC*SIGS(I)/SIGT(I)                         
c                                                                   
            DA0 = SIX*SIGS(I)*DFS(I)                                  
            DA1 = A1S*DFS(I) + A1T*DFT(I)  + A1C*DFC(I)                     
            DA2 = A2S*DFS(I) + A2T*DFT(I)  + A2C*DFC(I)         
C 
            FF(I) = DFDSIGDLAM + DA0 + P(I)*DA1 + P(I)**2 * DA2        
            FF(I) = SIGN(MAX(ABS(FF(I)),EM20) ,FF(I))      
c                 
            DLAM    = YLDS(I)/FF(I)                          
            DPLA(I) = MAX(ZERO, DPLA(I) + TWO*DLAM*YLD_NORM )
            PLA(I)  = PLA(I) + TWO*DLAM*YLD_NORM
            DPT = DLAM * YLD_NORM*BB
            DPS = DLAM * YLD_NORM*SQR3
            DPLAT(I) = DPLAT(I) + DPT
            DPLAC(I) = DPLAT(I)
            DPLAS(I) = DPLAS(I) + DPS
c
            !  Plastic strains tensor update
            DPXX(I) = DLAM * NORMXX * NORM
            DPYY(I) = DLAM * NORMYY * NORM
            DPZZ(I) = DLAM * NORMZZ * NORM
            DPXY(I) = DLAM * NORMXY * NORM
c           
            ! Elasto-plastic stresses update   
            SIGNXX(I) = SIGNXX(I) - (A11_2D(I)*DPXX(I) + A12_2D(I)*DPYY(I))
            SIGNYY(I) = SIGNYY(I) - (A11_2D(I)*DPYY(I) + A12_2D(I)*DPXX(I))
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G(I)
c            
c           compute EPSPC(I), EPSPT(I), EPSPS(I)
            EPSPT(I) = EPSPT(I) + DPT
            EPSPC(I) = EPSPC(I) + DPT                   
            EPSPS(I) = EPSPS(I) + DPS 
          ENDDO
c            
          ! Update Yld values and criterion with new plastic strain and strain rate
c            
          XVEC(1:NEL,1) = EPSPT(1:NEL)
          XVEC(1:NEL,2) = EPDT(1:NEL)
          IPOS(1:NEL,1) = 1
          IPOS(1:NEL,2) = 1
          CALL TABLE_VINTERP(FUNC_TRAC,NEL,IPOS,XVEC,SIGT,DFT)
          SIGT(1:NEL) = SIGT(1:NEL) * TFAC(1) * DAM(1:NEL)
          DFT(1:NEL)  = DFT(1:NEL)  * TFAC(1) * DAM(1:NEL)
c
          IPOS(1:NEL,1) = 1
          IPOS(1:NEL,2) = 1
          IF (FUNC_COMP%NOTABLE  > 0) THEN
            XVEC(1:NEL,1) = EPSPC(1:NEL)
            XVEC(1:NEL,2) = EPDC(1:NEL)
            CALL TABLE_VINTERP(FUNC_COMP,NEL,IPOS,XVEC,SIGC,DFC)
            SIGC(1:NEL) = SIGC(1:NEL) * TFAC(2) * DAM(1:NEL)
            DFC(1:NEL)  = DFC(1:NEL)  * TFAC(2) * DAM(1:NEL)
          END IF
          IPOS(1:NEL,1) = 1
          IPOS(1:NEL,2) = 1
          IF (FUNC_SHEAR%NOTABLE > 0) THEN
            XVEC(1:NEL,1) = EPSPS(1:NEL)
            XVEC(1:NEL,2) = EPDS(1:NEL)
            CALL TABLE_VINTERP(FUNC_SHEAR,NEL,IPOS,XVEC,SIGS,DFS)
            SIGS(1:NEL) = SIGS(1:NEL) * TFAC(3) * DAM(1:NEL)
            DFS(1:NEL)  = DFS(1:NEL)  * TFAC(3) * DAM(1:NEL)
          END IF
c--------------------------------------------------
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
                SIGS(I) = SQRT(SIGT(I)*SIGC(I)/THREE)
              END DO      
          END SELECT  
c--------------------------------------------------
          IF (ICONV == 1) THEN
            DO II = 1,NINDX         
              I = INDX(II)         
              CONV(I) = .FALSE.
              IF (SIGS(I) < SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)) THEN 
                SIGS(I) = SFAC*SQRT(SIGC(I)*SIGT(I)/THREE)
                CONV(I) = .TRUE.
              ENDIF
            END DO
          ENDIF
          DO II = 1,NINDX         
            I = INDX(II)         
            AA = ONE/SIGC(I)/SIGT(I)
            A0(I) = THREE*SIGS(I)**2
            A1(I) = NINE*SIGS(I)**2*(SIGC(I) - SIGT(I))*AA
            A2(I) = NINE*(SIGC(I)*SIGT(I) - THREE*SIGS(I)**2)*AA  
          END DO
c
          ! Stress and yield surface update       
c
          DO II = 1,NINDX         
            I  = INDX(II)          
            P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) )    
            SXX(I) = SIGNXX(I) + P(I)                              
            SYY(I) = SIGNYY(I) + P(I)                              
            SZZ(I) = P(I)
            SVM2(I)= THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SIGNXY(I)**2
            SVM(I) = SQRT(SVM2(I))
            ! residual YLDS
            YLDS(I) = SVM2(I) - A0(I) - A1(I)*P(I) - A2(I)*P(I)*P(I)
            IF (INLOC == 0) THEN 
              DEZZ(I) = DEZZ(I) + NU1*(DPXX(I) + DPYY(I)) + DPZZ(I)   
            ENDIF
          ENDDO
c
        ENDDO   ! End Newton iterations
      END IF    ! plasticity
c-----------------------------------------------
c     Update plastic Poisson coefficient
c-----------------------------------------------
      IF (IFUNC(1) > 0) THEN
        DO I=1,NEL
          NUP(I)    = FINTER(IFUNC(1),PLA(I),NPF,TF,DYDX) * YFAC(1)
          UVAR(I,9) = MAX(ZERO, MIN(NUP(I), HALF))
        ENDDO
      END IF
C--------------------------------
C     NON-LOCAL THICKNESS VARIATION
C--------------------------------
      IF (INLOC > 0) THEN 
        DO I = 1,NEL
          CB     = A1(I) + TWO*A2(I)*P(I)                                                          
          NORM   = ONE/MAX(EM20, SQRT(SIX*SVM(I)**2  + THIRD*CB**2)) ! norm df/dsig
          NORMXX = THREE*SXX(I) + CB/THREE ! DF/DSIG
          NORMYY = THREE*SYY(I) + CB/THREE 
          NORMZZ = THREE*SZZ(I) + CB/THREE  
          YLD_NORM = SVM(I)*NORM
          IF (YLD_NORM /= ZERO) THEN 
            DLAM_NL(I) = (ONE/(TWO*YLD_NORM))*MAX(DPLANL(I),ZERO)
            DEZZ(I) = DEZZ(I) + NU1*(DLAM_NL(I)*NORMXX)*NORM
     .                        + NU1*(DLAM_NL(I)*NORMYY)*NORM
     .                        + DLAM_NL(I)*NORMZZ*NORM
          ENDIF          
        ENDDO
      ENDIF
c--------------------
      UVAR(1:NEL,1) = EPSPT(1:NEL)       
      UVAR(1:NEL,2) = EPSPC(1:NEL)       
      UVAR(1:NEL,3) = EPSPS(1:NEL)       
      UVAR(1:NEL,4) = ALPHA*DPLAT(1:NEL)*DTINV + ALPHI*EPDT_F(1:NEL)
      UVAR(1:NEL,5) = ALPHA*DPLAC(1:NEL)*DTINV + ALPHI*EPDC_F(1:NEL)
      UVAR(1:NEL,6) = ALPHA*DPLAS(1:NEL)*DTINV + ALPHI*EPDS_F(1:NEL)
      IF (INLOC > 0) THEN 
        DO I = 1,NEL
          UVAR(I,8) = UVAR(I,8) + MAX(DPLANL(I),ZERO)
        ENDDO
      ENDIF
      EPSD(1:NEL) = ALPHA*DPLA(1:NEL)*DTINV + ALPHI*EPSD(1:NEL)
c-----------   
      RETURN
      END
