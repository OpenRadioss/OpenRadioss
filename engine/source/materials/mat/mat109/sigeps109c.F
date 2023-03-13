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
Chd|  SIGEPS109C                    source/materials/mat/mat109/sigeps109c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS109C(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NVARTMP ,NUMTABL ,
     2     UPARAM  ,UVAR    ,VARTMP  ,ITABLE  ,TABLE   ,JTHE    ,
     3     TIME    ,TIMESTEP,OFF     ,RHO     ,PLA     ,DPLA    ,
     4     SOUNDSP ,SIGY    ,ET      ,TEMP    ,EPSP    ,GS      ,
     5     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8     THK     ,THKLY   ,INLOC   ,DPLANL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NVARTMP,NUMTABL,JTHE,INLOC
      INTEGER ,DIMENSION(NUMTABL),INTENT(IN)  :: ITABLE
      INTEGER ,DIMENSION(NEL)    ,INTENT(IN)  :: NGL
c
      my_real  :: TIME
      my_real,DIMENSION(NUPARAM) ,INTENT(IN)  :: UPARAM
      my_real,DIMENSION(NEL)     ,INTENT(IN)  :: RHO,OFF,GS,THKLY,TIMESTEP,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,DPLANL
      my_real ,DIMENSION(NEL)    ,INTENT(OUT) :: SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: PLA,DPLA,EPSP,TEMP,THK
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
c
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,NINDX,ITER,NITER,ISMOOTH,
     .   FUNC_YLD,FUNC_TEMP,FUNC_ETA,NDIM_YLD,NDIM_TEMP,NDIM_ETA
      INTEGER ,DIMENSION(NEL) :: INDEX
c
      my_real  :: YOUNG,LAME,G,G2,G3,A11,A12,NU,NNU,NNU1,TREF,TINI,BETA,LDAV,
     .   XRATE,XSCALE,YSCALE,DTINV,J2,Q2,DPHI_DLAM,DLAMIN,ALPHA,ALPHI,
     .   R,R1,R2,SAT,SBT,DPXX,DPYY,DPXY
      my_real  :: check_svm
c
      my_real, DIMENSION(NEL) ::SVM,SVMT,YLD,YLD_TREF,YLD_TEMP,
     .   SXX,SYY,SXY,SYZ,SZX,SIGM,STXX,STYY,STXY,STYZ,STZX,PLA_NL,DPDT_NL,
     .   FACT_ETA,DYDX,HARDP,HARDR,YLD_I,HARDP_I,HARDR_I,DXDYV,DLAM,PHI,
     .   FTHERM,TFAC,DEPSZZ,NORMXX,NORMYY,NORMXY,DEPSPXX,DEPSPYY,DEPSPXY
     .   
      my_real, DIMENSION(NEL,3) :: XVEC_ETA
      my_real, DIMENSION(NEL,4) :: XVEC
      INTEGER, DIMENSION(NEL,3) :: IPOS_ETA
      INTEGER, DIMENSION(NEL,2) :: IPOS
C-----------------------------------------------
      ! VARTMP(1)   latest position of PLAS in TAB_YLD function 
      ! VARTMP(2)   latest position of PLAS in TAB_TEMP function 
      ! VARTMP(3)   latest position of TEMP in TAB_TEMP function 
      ! VARTMP(4)   latest position of TEMP in TAB_ETA function 
      ! VARTMP(5)   latest position of PLAS in TAB_ETA function 
C=======================================================================
c     Material model parameters
c-----------------------------------------------
      YOUNG   =  UPARAM(1)       ! Young modulus
      NU      =  UPARAM(2)       ! Poisson ratio
      BETA    =  UPARAM(3)       ! Thermal work coefficient
      TREF    =  UPARAM(4)       ! Reference temperature
      TINI    =  UPARAM(5)       ! Initial tempareture
      ISMOOTH =  NINT(UPARAM(6)) ! function interpolation flag
      XRATE   =  UPARAM(7)       ! strain rate abscissa factor for eta function
      XSCALE  =  UPARAM(8)       ! strain rate abscissa factor for yld function
      YSCALE  =  UPARAM(9)       ! Yld function scale factor
      
      G       =  UPARAM(11)      ! Shear modulus
      G2      =  UPARAM(12)      ! Shear modulus * 2
      G3      =  UPARAM(13)      ! Shear modulus * 3
      LAME    =  UPARAM(15)      ! Lame parameter
      A11     =  UPARAM(16)      ! YOUNG / (ONE - NU**2)
      A12     =  UPARAM(17)      ! YOUNG * NU / (ONE - NU**2)
      NNU     =  UPARAM(18)      ! NU / (ONE - NU)
      NNU1    =  UPARAM(19)      !(ONE - TWO*NU) / (ONE - NU)
      IF (JTHE /= 0) BETA = ZERO ! No temperature calculation inside material
c
      FUNC_YLD  = ITABLE(1)
      FUNC_TEMP = ITABLE(2)
      FUNC_ETA  = ITABLE(3)
      NDIM_YLD  = TABLE(FUNC_YLD)%NDIM
      IF (FUNC_TEMP > 0) THEN
        NDIM_TEMP  = TABLE(FUNC_TEMP)%NDIM
      ENDIF
      IF (FUNC_ETA  > 0) THEN
        NDIM_ETA   = TABLE(FUNC_ETA)%NDIM
      ENDIF
c 
      ! Maximal number of Newton iterations
      NITER = 3
c
c     Initializations
c
      DPLA(1:NEL)  = ZERO          ! Initialization of the plastic strain increment
      ET(1:NEL)    = ONE            ! Initialization of tangent stiffness factor
      DEPSPXX(1:NEL)  = ZERO 
      DEPSPYY(1:NEL)  = ZERO 
      DEPSPXY(1:NEL)  = ZERO 
      DTINV  = ONE / MAX(EM20, TIMESTEP(1))
      DLAMIN = EM15
      ALPHA  = 0.025
      ALPHI  = ONE-ALPHA
      ! Non-local plastic strain
      IF (INLOC > 0) THEN 
        DO I = 1,NEL
          UVAR(I,1)  = UVAR(I,1) + MAX(DPLANL(I),ZERO)
          PLA_NL(I)  = UVAR(I,1)
          DPDT_NL(I) = MAX(DPLANL(I),ZERO)*DTINV
        ENDDO
      ENDIF
c      
c---  self heating factor and temperatiure initialization
      IF (JTHE == 0) THEN    ! internal temperature is calculated
        IF (TIME == ZERO) TEMP(1:NEL) = TINI
        IF (BETA > ZERO) THEN
          IF (FUNC_ETA > 0) THEN    ! scale factor function for Taylor-Quinney coefficient
            IF (INLOC == 0) THEN 
              XVEC_ETA(1:NEL,1) = EPSP(1:NEL) * XRATE 
            ELSE
              XVEC_ETA(1:NEL,1) = DPDT_NL(1:NEL) * XRATE 
            ENDIF
            IPOS_ETA(1:NEL,1) = 1
            IF (NDIM_ETA > 1) THEN
              XVEC_ETA(1:NEL,2) = TEMP(1:NEL)
              IPOS_ETA(1:NEL,2) = VARTMP(1:NEL,4)
            END IF
            IF (NDIM_ETA > 2) THEN
              IF (INLOC == 0) THEN 
                XVEC_ETA(1:NEL,3) = PLA(1:NEL)
              ELSE
                XVEC_ETA(1:NEL,3) = PLA_NL(1:NEL)
              ENDIF
              IPOS_ETA(1:NEL,3) = VARTMP(1:NEL,5)
            END IF

            CALL TABLE_VINTERP(TABLE(FUNC_ETA),NEL,IPOS_ETA,XVEC_ETA,FACT_ETA,DXDYV)          
      
            IF (NDIM_ETA > 1) VARTMP(1:NEL,4) = IPOS_ETA(1:NEL,2)
            IF (NDIM_ETA > 2) VARTMP(1:NEL,5) = IPOS_ETA(1:NEL,3)
            DO I=1,NEL
              FTHERM(I) = MIN(BETA*FACT_ETA(I), ONE)
            END DO
          ELSE
            FTHERM(1:NEL) = MIN(BETA, ONE)
          END IF 
        END IF
      ENDIF
c-----------------------------------------------
c     Trial stress
c-----------------------------------------------
      DO I=1,NEL
        LDAV = (DEPSXX(I) + DEPSYY(I)) * LAME
        SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A11*DEPSYY(I) + A12*DEPSXX(I)
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*GS(I)
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*GS(I)
        SIGM(I)   = (SIGNXX(I) + SIGNYY(I)) * THIRD
        ! deviatoric trial stress tensor
        STXX(I) = SIGNXX(I) - SIGM(I)
        STYY(I) = SIGNYY(I) - SIGM(I)
        STXY(I) = SIGNXY(I)
        IF (INLOC == 0) THEN
          DEPSZZ(I) = -NNU * (DEPSXX(I) + DEPSYY(I))
        ENDIF
      ENDDO
c----------------------------------------------------      
c     Computation of the initial yield stress
c----------------------------------------------------      
      XVEC(1:NEL,1) = PLA(1:NEL)
      XVEC(1:NEL,2) = EPSP(1:NEL) * XSCALE
      IPOS(1:NEL,1) = VARTMP(1:NEL,1)
      IPOS(1:NEL,2) = 1
c
      CALL TABLE2D_VINTERP_LOG(TABLE(FUNC_YLD),ISMOOTH,NEL,IPOS,XVEC  ,YLD  ,HARDP,HARDR)               
c
      YLD(1:NEL)   = YLD(1:NEL)   * YSCALE
      HARDP(1:NEL) = HARDP(1:NEL) * YSCALE
      VARTMP(1:NEL,1) = IPOS(1:NEL,1)
c----------------------------------------------------      
c     Computation of temperature dependent yield stress factor from quasistatic curves
c----------------------------------------------------      
      IF (FUNC_TEMP > 0) THEN
c        XVEC(1:NEL,1) = PLA(1:NEL)
        XVEC(1:NEL,2) = TREF
        IPOS(1:NEL,1) = VARTMP(1:NEL,2)
        IPOS(1:NEL,2) = VARTMP(1:NEL,3)
        CALL TABLE_VINTERP(TABLE(FUNC_TEMP),NEL,IPOS,XVEC,YLD_TREF,DYDX)  
        VARTMP(1:NEL,2) = IPOS(1:NEL,1)     
        VARTMP(1:NEL,3) = IPOS(1:NEL,2)     
c     
        XVEC(1:NEL,2) = TEMP(1:NEL)
        CALL TABLE_VINTERP(TABLE(FUNC_TEMP),NEL,IPOS,XVEC,YLD_TEMP,DYDX)          
c     
        TFAC(1:NEL)  = YLD_TEMP(1:NEL) / YLD_TREF(1:NEL)      
        YLD(1:NEL)   = YLD(1:NEL)   * TFAC(1:NEL)      
        HARDP(1:NEL) = HARDP(1:NEL) * TFAC(1:NEL) 
      ELSE
        TFAC(1:NEL) = ONE
      END IF
c-----------------------------------------------
c     Check plasticity 
c-----------------------------------------------
      NINDX = 0
      DO I=1,NEL
        J2 = STXX(I)**2 + STYY(I)**2 + STXX(I)*STYY(I) + STXY(I)**2
        Q2 = THREE*J2
c
        IF (Q2 > YLD(I)**2  .and. OFF(I) == ONE) THEN
          NINDX = NINDX + 1   ! Number of the elements with plastic behaviour
          INDEX(NINDX)  = I
          SVMT(I) = SQRT(Q2)
          PHI(I)  = SVMT(I) - YLD(I)
        ENDIF
      ENDDO
c      
      !====================================================================
      ! - PLASTIC CORRECTION Newton
      !====================================================================
c
      IF (NINDX > 0) THEN
        DO II = 1, NINDX   ! Number of the element with plastic behaviour     
          I = INDEX(II)
          ! initialize incremental update
          SXX(I) = STXX(I)
          SYY(I) = STYY(I)
          SXY(I) = STXY(I)
          SVM(I) = SVMT(I)
        END DO
c
        DO ITER = 1,NITER 
c
          DO II = 1, NINDX      
            I = INDEX(II)
c
            PHI(I) = SVM(I) - YLD(I)
            R      = THREE_HALF / SVM(I)           
            NORMXX(I) = R*SXX(I)
            NORMYY(I) = R*SYY(I)
            NORMXY(I) = R*SXY(I)
            DPHI_DLAM = G3 + HARDP(I)
            DLAM(I) = PHI(I) / DPHI_DLAM
            DPLA(I) = DPLA(I) + DLAM(I)
          END DO
          
          ! Update Yld and Hardp with new plastic strain and strain rate
          
          DO II = 1, NINDX 
            I = INDEX(II)
            XVEC(II,1) = PLA(I) + DPLA(I)
            XVEC(II,2) = EPSP(I)
            IPOS(II,1) = VARTMP(I,1)
            IPOS(II,2) = 1
          ENDDO
c
          CALL TABLE2D_VINTERP_LOG(TABLE(FUNC_YLD),ISMOOTH,NINDX,IPOS,XVEC,YLD_I,HARDP_I,HARDR_I)
c
          DO II = 1, NINDX 
            I = INDEX(II)
            VARTMP(I,1) = IPOS(II,1)
            HARDP(I) = HARDP_I(II)  *YSCALE*TFAC(I)
            YLD(I)   = MIN(YLD_I(II)*YSCALE*TFAC(I), SVMT(I))
          ENDDO
c                    
          ! deviatoric stress update
c
          DO II = 1, NINDX 
            I = INDEX(II)
c
            DPXX = DLAM(I)*NORMXX(I)
            DPYY = DLAM(I)*NORMYY(I)
            DPXY = DLAM(I)*NORMXY(I)
c
            SXX(I) = SXX(I) - G2 * DPXX
            SYY(I) = SYY(I) - G2 * DPYY
            SXY(I) = SXY(I) - G2 * DPXY

            J2 = SXX(I)*SXX(I) + SYY(I)*SYY(I) + SXX(I)*SYY(I) + SXY(I)*SXY(I)
            SVM(I) = SQRT(THREE*J2)
            PHI(I) = SVM(I) - YLD(I)
c
            DEPSPXX(I) = DEPSPXX(I) + DPXX
            DEPSPYY(I) = DEPSPYY(I) + DPYY
            DEPSPXY(I) = DEPSPXY(I) + DPXY
         ENDDO              
c            
        END DO   !  ITER = 1,NITER
c        
        DO II = 1, NINDX 
          I = INDEX(II)
          SIGNXX(I) = SXX(I) + SIGM(I)
          SIGNYY(I) = SYY(I) + SIGM(I)
          SIGNXY(I) = SXY(I)
c
          PLA(I) = PLA(I) + MAX(DPLA(I), ZERO)
          ET(I)  = HARDP(I) / (HARDP(I) + YOUNG)
          IF (INLOC == 0) THEN 
            DEPSZZ(I) = DEPSZZ(I) - NNU1*(DEPSPXX(I) + DEPSPYY(I))
          ENDIF
        ENDDO                      
c
        ! Update the temperature
        IF (JTHE == 0 .AND. BETA > ZERO .AND. INLOC == 0) THEN 
          DO II = 1, NINDX 
            I = INDEX(II)
            TEMP(I) = TEMP(I) + FTHERM(I)*YLD(I)*DPLA(I)
          ENDDO                      
        ENDIF
c
c            
      END IF     !  NINDX > 0
c
      SOUNDSP(1:NEL)= SQRT(A11 / RHO(1:NEL))
      SIGY(1:NEL)   = YLD(1:NEL)
      EPSP(1:NEL)   = ALPHA*DPLA(1:NEL)*DTINV + ALPHI*EPSP(1:NEL)
      IF (INLOC > 0) THEN 
        DO I = 1,NEL     
          DEPSZZ(I) = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(YLD(I),EM20)
          DEPSZZ(I) = - NU*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG) - DEPSZZ(I)
          IF (JTHE == 0 .AND. BETA > ZERO) THEN 
            TEMP(I) = TEMP(I) + FTHERM(I)*YLD(I)*MAX(DPLANL(I),ZERO)
          ENDIF
        ENDDO
      ENDIF
      THK(1:NEL)    = THK(1:NEL) + DEPSZZ(1:NEL)*THKLY(1:NEL)*OFF(1:NEL)
c-----------
      RETURN
      END SUBROUTINE SIGEPS109C
