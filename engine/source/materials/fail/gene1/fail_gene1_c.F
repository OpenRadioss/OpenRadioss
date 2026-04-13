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
!||    fail_gene1_c          ../engine/source/materials/fail/gene1/fail_gene1_c.F
!||--- called by ------------------------------------------------------
!||    mulawc                ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell         ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||    table2d_vinterp_log   ../engine/source/tools/curve/table2d_vinterp_log.F
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    dt_mod                ../engine/source/modules/dt_mod.F
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE FAIL_GENE1_C (FAIL     ,
     1     NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     2     TIME     ,TIMESTEP ,IPG      ,
     3     NGL      ,GBUF_DT  ,EPSP     ,OFF      ,
     4     EPSXX    ,EPSYY    ,EPSXY    ,AREA     ,THKN     ,
     5     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6     TEMP     ,DFMAX    ,ALDT     ,TABLE    ,TDELE    ,
     7     THK0     ,IPT      ,FOFF     ,THKLYL   ,NTABLF   ,
     8     ITABLF   ,LF_DAMMX ,DT       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE DT_MOD
      use fail_param_mod
      use table_mat_vinterp_mod
C!-----------------------------------------------
C!   I m p l i c i t   T y p e s
C!-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "scr18_c.inc"
#include      "tabsiz_c.inc"
C!-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUVAR,NVARTMP,IPG,IPT,NTABLF,LF_DAMMX
      INTEGER, DIMENSION(NTABLF) ,INTENT(IN) :: ITABLF
      INTEGER, DIMENSION(NEL)    ,INTENT(IN) :: NGL
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real, INTENT(IN) :: TIME,TIMESTEP
      my_real, DIMENSION(NEL), INTENT(IN) :: EPSXX,EPSYY,EPSXY,
     .   GBUF_DT,EPSP,OFF,ALDT,TEMP,AREA,THKN,THK0,THKLYL
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: FOFF
      my_real, DIMENSION(NEL), INTENT(INOUT) :: SIGNXX,SIGNYY,SIGNXY,
     .   SIGNYZ,SIGNZX,TDELE
      my_real, DIMENSION(NEL,LF_DAMMX), INTENT(INOUT) :: DFMAX
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
      TYPE (TTABLE), DIMENSION(NTABLE), INTENT(IN) :: TABLE
      TYPE (DT_), INTENT(IN) :: DT
      type (fail_param_) ,intent(inout) :: fail    !< failure model data structure
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,II,J,INDX1(NEL),NINDX1,NINDX_ON,NSTEP,CRIT,NMOD,
     .        fct_ISM,fct_IPS,fct_IDg12,fct_IDe1c,fct_IDel,
     .        NCRIT(NEL),Ismooth,Istrain,
     .        tab_IDfld,Itab,NCS,IPMAX(NEL),IPMIN(NEL),IS1MAX(NEL),
     .        ITMAX(NEL),IMINDT(NEL),ISIGMAX(NEL),ISIGTH(NEL),IEPSMAX(NEL),
     .        IEFFEPS(NEL),IVOLEPS(NEL),IMINEPS(NEL),ISHEAR(NEL),IMIX12(NEL),
     .        IMXE1C(NEL),IFLD(NEL),ITHIN(NEL),IMAXTEMP(NEL)
      INTEGER ,DIMENSION(NEL) :: INDX_ON
      my_real 
     .        MINPRES,MAXPRES,SIGP1,TMAX,DTMIN,EPSDOT_SM,SIGVM,SIGTH,
     .        KF,EPSDOT_PS,MAXEPS,EFFEPS,VOLEPS,MINEPS,EPSSH,EPSDOT_FLD,
     .        THIN,MAXTEMP,Fscale_el,El_ref,LAMBDA,FAC,DF
      my_real 
     .        E12,P(NEL),SXX,SYY,SZZ,SVM(NEL),SH12(NEL),E1C(NEL),
     .        XVEC(NEL,2),Q,R_INTER,E11(NEL),E22(NEL),VOL_STRAIN(NEL),DAV,
     .        E1D,E2D,E3D,E4D,S11(NEL),S22(NEL),EFF_STRAIN(NEL),S1,S2,
     .        EPSMAX(NEL),SIGMAX(NEL),FACL(NEL),E1FLD(NEL),
     .        DFLD(NEL),HARDR(NEL),DENOM,TRIAX(NEL)
      my_real ,dimension(nel) :: xvec1,dydx
!--------------------------------------------------------------
      ! Recovering failure criterion parameters
      ! -> Integer parameters
      CRIT       = FAIL%IPARAM(1)
      Itab       = FAIL%IPARAM(2)
      NSTEP      = FAIL%IPARAM(3)
      NCS        = FAIL%IPARAM(4)
      Ismooth    = FAIL%IPARAM(5)
      Istrain    = FAIL%IPARAM(6)
      ! -> Real parameters
      MINPRES    = FAIL%UPARAM(1)
      MAXPRES    = FAIL%UPARAM(2)
      SIGP1      = FAIL%UPARAM(3)
      TMAX       = FAIL%UPARAM(4)   
      DTMIN      = FAIL%UPARAM(5)
      EPSDOT_SM  = FAIL%UPARAM(6)
      SIGVM      = FAIL%UPARAM(7)
      SIGTH      = FAIL%UPARAM(8)
      KF         = FAIL%UPARAM(9)
      EPSDOT_PS  = FAIL%UPARAM(10)
      MAXEPS     = FAIL%UPARAM(11)
      EFFEPS     = FAIL%UPARAM(12)
      VOLEPS     = FAIL%UPARAM(13)
      MINEPS     = FAIL%UPARAM(14)
      EPSSH      = FAIL%UPARAM(15)
      EPSDOT_FLD = FAIL%UPARAM(16)
      THIN       = FAIL%UPARAM(17)
      MAXTEMP    = FAIL%UPARAM(20)
      Fscale_el  = FAIL%UPARAM(21)
      El_ref     = FAIL%UPARAM(22)
c
      ! Initialization
      NINDX1 = 0
      INDX1(1:NEL)    = 0
      IPMAX(1:NEL)    = 0
      IPMIN(1:NEL)    = 0
      IS1MAX(1:NEL)   = 0
      ITMAX(1:NEL)    = 0
      IMINDT(1:NEL)   = 0
      ISIGMAX(1:NEL)  = 0
      ISIGTH(1:NEL)   = 0
      IEPSMAX(1:NEL)  = 0
      IEFFEPS(1:NEL)  = 0
      IVOLEPS(1:NEL)  = 0
      IMINEPS(1:NEL)  = 0
      ISHEAR(1:NEL)   = 0
      IMIX12(1:NEL)   = 0
      IMXE1C(1:NEL)   = 0
      IFLD(1:NEL)     = 0
      ITHIN(1:NEL)    = 0
      IMAXTEMP(1:NEL) = 0
      NCRIT(1:NEL)    = 0
c     function & tables
      fct_ISM   = 1 
      fct_IPS   = 2  ! -> maximum principal strain VS strain-rate
      fct_IDg12 = 3  ! -> in-plane shear strain VS element size
      fct_IDe1c = 5  ! -> major in plane-strain VS element size
      fct_IDel  = 6  ! -> element size regularization
      IF (NTABLF > 0) THEN
        tab_IDfld = ITABLF(1)
      ELSE
        tab_IDfld = 0
      END IF
c      
      ! At initial time, compute the element size regularization factor
      IF (UVAR(1,1) == ZERO) THEN 
        IF (fail%table4d(fct_IDel)%notable > 0) THEN
          xvec1(:) = ALDT(:) / El_ref
          call table_mat_vinterp(fail%table4d(fct_IDel),nel,nel,vartmp(1:nel,1:1),xvec1,uvar(1:nel,1),dydx)
        ELSE
          uvar(1:nel,1) = one
        ENDIF
      ENDIF
      FACL(1:NEL) = UVAR(1:NEL,1)
!      
      IF (UVAR(1,5) == ZERO .AND. FOFF(1) /= 0) UVAR(1:NEL,5) = ONE
      IF (UVAR(1,6) == ZERO) UVAR(1:NEL,6) = THK0(1:NEL)
      IF (UVAR(1,7) == ZERO) UVAR(1:NEL,7) = THKLYL(1:NEL)
      IF (UVAR(1,8) == ZERO) UVAR(1:NEL,8) = ALDT(1:NEL)
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
       ! Integration point failure
       IF (UVAR(I,5) < ONE .AND. UVAR(I,5) >= EM08) THEN 
         UVAR(I,5) = UVAR(I,5) - ONE/NSTEP
       ENDIF
       IF (UVAR(I,5) <= EM08) UVAR(I,5) = ZERO
       SIGNXX(I) = SIGNXX(I)*UVAR(I,5)
       SIGNYY(I) = SIGNYY(I)*UVAR(I,5)
       SIGNXY(I) = SIGNXY(I)*UVAR(I,5)
       SIGNYZ(I) = SIGNYZ(I)*UVAR(I,5)
       SIGNZX(I) = SIGNZX(I)*UVAR(I,5)
       ! Regularization factors for length, surface and volume
      ENDDO
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE STRESSES AND STRAINS
      !====================================================================    
         
      DO I=1,NEL
c
        ! For active element or Gauss point
        IF ((UVAR(I,5) == ONE).AND.(OFF(I)==ONE)) THEN
          ! ----------------------------------------------------------------------------------------
          ! Computation of volumetric strain, effective strain, shear strain and principal strains
          ! ----------------------------------------------------------------------------------------
          !  -> computation of principal strains
          E12 = HALF*EPSXY(I)
          S1  = HALF*(EPSXX(I) + EPSYY(I))
          S2  = HALF*(EPSXX(I) - EPSYY(I))
          Q   = SQRT(S2**2 + E12**2)
          E11(I) = S1 + Q
          E22(I) = S1 - Q
          IF (E22(I) >= E11(I)) THEN
            R_INTER = E22(I)
            E22(I)  = E11(I)
            E11(I)  = R_INTER
          ENDIF
          !  -> computation of volumetric strain
          VOL_STRAIN(I) = E11(I) + E22(I)
          !  -> computation of effective strain
          DAV = (EPSXX(I)+EPSYY(I))*THIRD
          E1D = EPSXX(I) - DAV
          E2D = EPSYY(I) - DAV
          E3D = - DAV
          E4D = HALF*EPSXY(I)
          EFF_STRAIN(I) = E1D**2 + E2D**2 + E3D**2 + TWO*(E4D**2)
          EFF_STRAIN(I) = SQRT(TWO_THIRD*EFF_STRAIN(I))
c
          ! --------------------------------------------------------------------------
          ! Computation of hydrostatic stress, Von Mises stress and principal stresses
          ! --------------------------------------------------------------------------
          !  -> pressure stress (positive in compression)
          P(I)   = -THIRD*(SIGNXX(I) + SIGNYY(I))
          !  -> equivalent stress of Von Mises
          SXX    = SIGNXX(I) + P(I)
          SYY    = SIGNYY(I) + P(I)
          SZZ    = P(I)
          SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)
     .             - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))
          TRIAX(I) = -P(I)/MAX(SVM(I),EM20)
          !  -> computing the principal stresses
          S1     = HALF*(SIGNXX(I) + SIGNYY(I))
          S2     = HALF*(SIGNXX(I) - SIGNYY(I))
          Q      = SQRT(S2**2 + SIGNXY(I)**2)
          S11(I) = S1 + Q
          S22(I) = S1 - Q 
          IF (S22(I) >= S11(I)) THEN
            R_INTER = S22(I)
            S22(I)  = S11(I)
            S11(I)  = R_INTER
          ENDIF
c
        ! For broken element or Gauss point
        ELSE
          E11(I) = ZERO
          E22(I) = ZERO
          VOL_STRAIN(I) = ZERO
          EFF_STRAIN(I) = ZERO
          P(I) = ZERO
          SVM(I) = ZERO
          TRIAX(I) = ZERO
          S11(I) = ZERO
          S22(I) = ZERO
        ENDIF
c
      ENDDO
c
      !  -> Forming limit diagram
      IF (NTABLF > 0) THEN 
        IF (Itab == 1) THEN 
          ! Diagram using true strains
          IF (Istrain == 0) THEN 
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = E22(1:NEL)
            XVEC(1:NEL,2) = EPSP(1:NEL)/EPSDOT_FLD
            !   -> Tensile yield stress in direction 1 (MD)
            CALL TABLE2D_VINTERP_LOG(TABLE(tab_IDfld),Ismooth,NEL,NEL,VARTMP(1:NEL,6:7),XVEC,E1FLD,DFLD,HARDR)
          ! Diagram using engineering strain
          ELSE
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = EXP(E22(1:NEL))-ONE
            XVEC(1:NEL,2) = EPSP(1:NEL)/EPSDOT_FLD
            !   -> Tensile yield stress in direction 1 (MD)
            CALL TABLE2D_VINTERP_LOG(TABLE(tab_IDfld),Ismooth,NEL,NEL,VARTMP(1:NEL,6:7),XVEC,E1FLD,DFLD,HARDR)
            E1FLD = LOG(ONE + E1FLD)
          ENDIF
        ELSE
          ! Diagram using true strains
          IF (Istrain == 0) THEN 
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = E22(1:NEL)
            XVEC(1:NEL,2) = ALDT(1:NEL)/El_ref
            !   -> Tensile yield stress in direction 1 (MD)
            CALL TABLE_VINTERP(TABLE(tab_IDfld),NEL,NEL,VARTMP(1:NEL,6:7),XVEC,E1FLD,DFLD)
          ! Diagram using engineering strains
          ELSE
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = EXP(E22(1:NEL))-ONE
            XVEC(1:NEL,2) = ALDT(1:NEL)/El_ref
            !   -> Tensile yield stress in direction 1 (MD)
            CALL TABLE_VINTERP(TABLE(tab_IDfld),NEL,NEL,VARTMP(1:NEL,6:7),XVEC,E1FLD,DFLD)
            E1FLD = LOG(ONE + E1FLD)
          ENDIF
        ENDIF
      ENDIF 
!-------------------------------------------------------------------------------
      ! Tag active elements
      nindx_on = 0  
      do i=1,nel
        if (off(i) == one .and. uvar(i,5) == one) then
          nindx_on = nindx_on + 1
          indx_on(nindx_on) = i  
        end if
      end do
!
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO CHECK THE EROSION CRITERIA
      !==================================================================== 

      NMOD = 0
      IF (BTEST(CRIT,1)) THEN !  -> minimum pressure
        NMOD = NMOD + 1
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(P(I)/(MINPRES*FACL(I)),DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)
          IF (P(I) <= MINPRES*FACL(I)) THEN
            NCRIT(I) = NCRIT(I) + 1
            IPMIN(I) = 1
          ENDIF
        END DO
      ENDIF
!
      IF (BTEST(CRIT,2)) THEN  !  -> maximum pressure
        NMOD = NMOD + 1
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(P(I)/(MAXPRES*FACL(I)),DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)
          IF (P(I) >= MAXPRES*FACL(I)) THEN
            NCRIT(I) = NCRIT(I) + 1
            IPMAX(I) = 1
          ENDIF
        END DO
      ENDIF
!         
      IF (BTEST(CRIT,3)) THEN  !  -> maximal principal stress
        NMOD = NMOD + 1
        DO II = 1,NINDX_ON        
          I = INDX_ON(II)
          IF (SIGP1 > ZERO) THEN    ! (unrestricted)
            DFMAX(I,1+NMOD) = MAX(S11(I)/(SIGP1*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE) 
            IF (S11(I) >= SIGP1*FACL(I)) THEN 
              NCRIT(I)  = NCRIT(I) + 1
              IS1MAX(I) = 1
            ENDIF        
          ELSE       ! (restricted to positive stress triaxialities)
            IF (TRIAX(I)>EM10) THEN 
              DFMAX(I,1+NMOD) = MAX(S11(I)/(ABS(SIGP1)*FACL(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE) 
              IF (S11(I) >= ABS(SIGP1)*FACL(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IS1MAX(I) = 1  
              ENDIF          
            ENDIF
          ENDIF
        END DO
      ENDIF
!
      IF (BTEST(CRIT,4)) THEN   !  -> maximum time
        NMOD = NMOD + 1
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(TIME/TMAX,DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)           
          IF (TIME >= TMAX) THEN
            NCRIT(I) = NCRIT(I) + 1
            ITMAX(I) = 1
          ENDIF
        END DO
      ENDIF
         
      IF (BTEST(CRIT,5)) THEN  !  -> minimum timestep
        NMOD = NMOD + 1
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          IF (TIME > ZERO) THEN 
            DFMAX(I,1+NMOD) = MAX(DTMIN/(GBUF_DT(I)*DTFAC1(1)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)   
            IF (GBUF_DT(I)*DTFAC1(1) <= DTMIN) THEN 
              NCRIT(I)  = NCRIT(I) + 1
              IMINDT(I) = 1
            ENDIF 
          ENDIF
        END DO
      ENDIF
!    
      IF (BTEST(CRIT,6)) THEN   !  -> equivalent stress
        NMOD = NMOD + 1 
        IF (EPSDOT_SM /= ZERO) THEN
          XVEC1(1:NEL) = EPSP(1:NEL) / EPSDOT_SM
          call table_mat_vinterp(fail%table4d(fct_ISM),nel,nel,vartmp(1:nel,2:2),xvec1,sigmax,dydx)
        ELSE
          SIGMAX(1:NEL) = SIGVM
        ENDIF
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(SVM(I)/(SIGMAX(I)*FACL(I)),DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)   
          IF (SVM(I) >= SIGMAX(I)*FACL(I)) THEN 
            NCRIT(I)   = NCRIT(I) + 1  
            ISIGMAX(I) = 1
          ENDIF
        END DO
      ENDIF
!      
      IF (BTEST(CRIT,7)) THEN   !  -> Tuler-Butcher
        NMOD = NMOD + 1 
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(UVAR(I,2)/(KF*FACL(I)),DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)   
          IF (S11(I) > SIGTH) THEN 
            UVAR(I,2) = UVAR(I,2) + TIMESTEP*(S11(I) - SIGTH)**2
            IF (UVAR(I,2) >= KF*FACL(I)) THEN 
              NCRIT(I)  = NCRIT(I) + 1 
              ISIGTH(I) = 1
            ENDIF
          ENDIF
        END DO
      ENDIF
!
      IF (BTEST(CRIT,8)) THEN   !  -> maximal principal strain
        NMOD = NMOD + 1 
        IF (EPSDOT_PS /= ZERO) THEN 
          XVEC1(1:NEL) = EPSP(1:NEL) / EPSDOT_PS
          call table_mat_vinterp(fail%table4d(fct_IPS),nel,nel,vartmp(1:nel,3:3),xvec1,epsmax,dydx)
        ELSE
          EPSMAX(1:NEL) = MAXEPS
        ENDIF
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(E11(I)/(EPSMAX(I)*FACL(I)),DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
          IF (E11(I) >= EPSMAX(I)*FACL(I)) THEN 
            NCRIT(I)   = NCRIT(I) + 1       
            IEPSMAX(I) = 1
          ENDIF  
        END DO
      ENDIF
!      
      IF (BTEST(CRIT,9)) THEN   !  -> maximum effective strain
        NMOD = NMOD + 1 
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(EFF_STRAIN(I)/(EFFEPS*FACL(I)),DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
          IF (EFF_STRAIN(I) >= EFFEPS*FACL(I)) THEN
            NCRIT(I)   = NCRIT(I) + 1
            IEFFEPS(I) = 1
          ENDIF
        END DO
      ENDIF
!      
      IF (BTEST(CRIT,10)) THEN     !  -> maximum volumetric strain
        NMOD = NMOD + 1 
        IF (VOLEPS > ZERO) THEN 
          DO II = 1,NINDX_ON
            I = INDX_ON(II)
            DFMAX(I,1+NMOD) = MAX(VOL_STRAIN(I)/(VOLEPS*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF (VOL_STRAIN(I) >= VOLEPS*FACL(I)) THEN 
              NCRIT(I)   = NCRIT(I) + 1
              IVOLEPS(I) = 1
            ENDIF
          END DO
        ELSE
          DO II = 1,NINDX_ON
            I = INDX_ON(II)
            DFMAX(I,1+NMOD) = MAX(VOL_STRAIN(I)/(VOLEPS*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF (VOL_STRAIN(I) <= VOLEPS*FACL(I)) THEN 
              NCRIT(I)   = NCRIT(I) + 1
              IVOLEPS(I) = 1
            ENDIF
          END DO
        ENDIF
      ENDIF
!
      IF (BTEST(CRIT,11)) THEN    !  -> minimum principal strain
        NMOD = NMOD + 1 
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          IF (E22(I) /= ZERO) THEN 
            DFMAX(I,1+NMOD) = MAX(MINEPS*FACL(I)/(E22(I)),DFMAX(I,1+NMOD))
          ENDIF
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
          IF (E22(I) <= MINEPS*FACL(I)) THEN
            NCRIT(I) = NCRIT(I) + 1
            IMINEPS(I) = 1
          ENDIF
        END DO
      ENDIF
!
      IF (BTEST(CRIT,12)) THEN     !  -> maximum tensorial shear strain
        NMOD = NMOD + 1 
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(((E11(I) - E22(I))/TWO)/(EPSSH*FACL(I)),DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
          IF ((E11(I) - E22(I))/TWO >= EPSSH*FACL(I)) THEN 
            NCRIT(I)  = NCRIT(I) + 1
            ISHEAR(I) = 1
          ENDIF 
        END DO
      ENDIF
!
      IF (BTEST(CRIT,13)) THEN     !  -> mixed mode 
        NMOD = NMOD + 1 
        XVEC1(1:NEL) = UVAR(1:NEL,8) / El_ref
        call table_mat_vinterp(fail%table4d(fct_IDel),nel,nel,vartmp(1:nel,4:4),xvec1,sh12,dydx)
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DENOM = SIGN(MAX(ABS(E11(I)),EM20),E11(I))   
          IF (E22(I)/DENOM <= -HALF .AND. E22(I)/DENOM >= -TWO) THEN
            DFMAX(I,1+NMOD) = MAX(((E11(I) - E22(I))/TWO) / (SH12(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF ((E11(I) - E22(I))/TWO >= SH12(I)) THEN 
              NCRIT(I)  = NCRIT(I) + 1
              IMIX12(I) = 1
            ENDIF
          ENDIF
        END DO
      ENDIF
!
      IF (BTEST(CRIT,15)) THEN                                       
        NMOD = NMOD + 1                                                
        XVEC1(1:NEL) = UVAR(1:NEL,8) / El_ref
        call table_mat_vinterp(fail%table4d(fct_IDe1c),nel,nel,vartmp(1:nel,5:5),xvec1,e1c,dydx)
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DENOM  = SIGN(MAX(ABS(E11(I)),EM20),E11(I))                  
          IF (((E22(I)/DENOM)<=ONE).AND.((E22(I)/DENOM)>=-HALF)) THEN  
            DFMAX(I,1+NMOD) = MAX(E11(I)/E1C(I),DFMAX(I,1+NMOD))       
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)                 
            IF (E11(I) >= E1C(I)) THEN                                 
              NCRIT(I)  = NCRIT(I) + 1                                 
              IMXE1C(I) = 1                                            
            ENDIF                                                      
          ENDIF                                                        
        END DO
      ENDIF                                                            
!                    
      IF (BTEST(CRIT,16)) THEN    !  -> Forming limit diagram                   
        NMOD = NMOD + 1                                                      
        IF (NTABLF > 0) THEN                                                    
          IF (Itab == 1) THEN                                                   
            DO II = 1,NINDX_ON
              I = INDX_ON(II)
              DFMAX(I,1+NMOD) = MAX(E11(I)/(E1FLD(I)*FACL(I)),DFMAX(I,1+NMOD))  
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)                        
              IF (E11(I) >= E1FLD(I)*FACL(I)) THEN                              
                NCRIT(I) = NCRIT(I) + 1                                         
                IFLD(I)  = 1                                                    
              ENDIF                                                             
            END DO
          ELSE                                                                  
            DO II = 1,NINDX_ON
              I = INDX_ON(II)
              DFMAX(I,1+NMOD) = MAX(E11(I)/(E1FLD(I)),DFMAX(I,1+NMOD))          
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)                        
              IF (E11(I) >= E1FLD(I))  THEN                                     
                NCRIT(I) = NCRIT(I) + 1                                         
                IFLD(I)  = 1                                                    
              ENDIF                                                             
            END DO
          ENDIF                                                                 
        ENDIF                                                                   
      ENDIF                                                                     
!                    
      IF (BTEST(CRIT,17)) THEN    !  -> maximum shell thinning                  
        NMOD = NMOD + 1                                                         
        IF (THIN < ZERO) THEN                                                
          DO II = 1,NINDX_ON
            I = INDX_ON(II)
            DFMAX(I,1+NMOD) = MAX(((THKN(I)-UVAR(I,6))/UVAR(I,6))/              
     .                            (-ABS(THIN)*FACL(I)),DFMAX(I,1+NMOD))         
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)                          
            IF (((THKN(I)-UVAR(I,6))/UVAR(I,6)) <= -ABS(THIN)*FACL(I)) THEN     
              NCRIT(I) = NCRIT(I) + 1                                           
              ITHIN(I) = 1                                                      
            ENDIF                                                               
          END DO
        ELSE                                                                    
          DO II = 1,NINDX_ON
            I = INDX_ON(II)
            DFMAX(I,1+NMOD) = MAX(((THKLYL(I)-UVAR(I,7))/UVAR(I,7))/             
     .                            (-ABS(THIN)*FACL(I)),DFMAX(I,1+NMOD))          
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)                           
            IF (((THKLYL(I)-UVAR(I,7))/UVAR(I,7)) <= -ABS(THIN)*FACL(I)) THEN    
              NCRIT(I) = NCRIT(I) + 1                                            
              ITHIN(I) = 1                                                       
            ENDIF                                                                
          END DO
        ENDIF                                                                   
      ENDIF                                                                     
!          
      IF (BTEST(CRIT,18)) THEN    !  -> maximum temperature
        NMOD = NMOD + 1 
        DO II = 1,NINDX_ON
          I = INDX_ON(II)
          DFMAX(I,1+NMOD) = MAX(TEMP(I)/MAXTEMP,DFMAX(I,1+NMOD))
          DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
          IF (TEMP(I) >= MAXTEMP) THEN 
            NCRIT(I)    = NCRIT(I) + 1 
            IMAXTEMP(I) = 1
          ENDIF
        END DO
      ENDIF
!-------------------------------------------------------------------------------
      !  -> Checking failure
      DO II = 1,NINDX_ON
        I = INDX_ON(II)
        DO J = 1,NMOD 
          DFMAX(I,1) = MAX(DFMAX(I,1),DFMAX(I,1+J))
        ENDDO
        DFMAX(I,1) = MIN(DFMAX(I,1),ONE)
        IF (NCRIT(I) >= NCS) THEN 
          UVAR(I,5)     = UVAR(I,5) - ONE/NSTEP
          SIGNXX(I)     = SIGNXX(I)*UVAR(I,5)
          SIGNYY(I)     = SIGNYY(I)*UVAR(I,5)
          SIGNXY(I)     = SIGNXY(I)*UVAR(I,5)
          SIGNYZ(I)     = SIGNYZ(I)*UVAR(I,5)
          SIGNZX(I)     = SIGNZX(I)*UVAR(I,5)
          DFMAX(I,1)    = ONE
          NINDX1        = NINDX1 + 1
          INDX1(NINDX1) = I
        ENDIF
      ENDDO
!-------------------------------------------------------------------------------
c
      !====================================================================
      ! - LOOKING FOR ELEMENT DELETION
      !====================================================================
      ! Checking element failure using global damage       
      DO I = 1,NEL
        IF ((UVAR(I,5) == ZERO).AND.(FOFF(I) /= 0)) THEN
          FOFF(I)    = 0 
          DFMAX(I,1) = ONE
          TDELE(I)   = TIME
        ENDIF
      ENDDO
c
c------------------------
c------------------------
      IF (NINDX1 > 0) THEN
        DO J = 1,NINDX1
          I = INDX1(J)
#include "lockon.inc"
          IF (NCRIT(I) == 1) THEN 
            WRITE(IOUT, 1000) NGL(I),IPG,IPT,TIME,NCRIT(I)
            WRITE(ISTDO,1000) NGL(I),IPG,IPT,TIME,NCRIT(I)
          ELSE
            WRITE(IOUT, 1001) NGL(I),IPG,IPT,TIME,NCRIT(I)
            WRITE(ISTDO,1001) NGL(I),IPG,IPT,TIME,NCRIT(I)
          ENDIF     
          IF (IPMAX(I) == 1) THEN 
            WRITE(IOUT, 1002) P(I),MAXPRES*FACL(I)
            WRITE(ISTDO,1002) P(I),MAXPRES*FACL(I)
          ENDIF
          IF (IPMIN(I) == 1) THEN
            WRITE(IOUT, 1003) P(I),MINPRES*FACL(I)
            WRITE(ISTDO,1003) P(I),MINPRES*FACL(I)
          ENDIF
          IF (IS1MAX(I) == 1) THEN
            WRITE(IOUT, 1004) S11(I),ABS(SIGP1)*FACL(I)
            WRITE(ISTDO,1004) S11(I),ABS(SIGP1)*FACL(I)
          ENDIF
          IF (ITMAX(I) == 1) THEN 
            WRITE(IOUT, 1005) TIME,TMAX
            WRITE(ISTDO,1005) TIME,TMAX    
          ENDIF
          IF (IMINDT(I) == 1) THEN 
            WRITE(IOUT, 1006) GBUF_DT(I)*DTFAC1(1),DTMIN
            WRITE(ISTDO,1006) GBUF_DT(I)*DTFAC1(1),DTMIN        
          ENDIF
          IF (ISIGMAX(I) == 1) THEN 
            WRITE(IOUT, 1007) SVM(I),SIGMAX(I)*FACL(I)
            WRITE(ISTDO,1007) SVM(I),SIGMAX(I)*FACL(I)    
          ENDIF
          IF (ISIGTH(I) == 1) THEN 
            WRITE(IOUT, 1008) UVAR(I,2),KF*FACL(I)
            WRITE(ISTDO,1008) UVAR(I,2),KF*FACL(I)             
          ENDIF
          IF (IEPSMAX(I) == 1) THEN 
            WRITE(IOUT, 1009) E11(I),EPSMAX(I)*FACL(I)
            WRITE(ISTDO,1009) E11(I),EPSMAX(I)*FACL(I)
          ENDIF
          IF (IEFFEPS(I) == 1) THEN
            WRITE(IOUT, 1010) EFF_STRAIN(I),EFFEPS*FACL(I)
            WRITE(ISTDO,1010) EFF_STRAIN(I),EFFEPS*FACL(I)
          ENDIF
          IF (IVOLEPS(I) == 1) THEN
            IF (VOLEPS >= ZERO) THEN 
              WRITE(IOUT, 1011) VOL_STRAIN(I),VOLEPS*FACL(I)
              WRITE(ISTDO,1011) VOL_STRAIN(I),VOLEPS*FACL(I)
            ELSE
              WRITE(IOUT, 1012) VOL_STRAIN(I),VOLEPS*FACL(I)
              WRITE(ISTDO,1012) VOL_STRAIN(I),VOLEPS*FACL(I) 
            ENDIF
          ENDIF
          IF (IMINEPS(I) == 1) THEN 
            WRITE(IOUT, 1013) E22(I),MINEPS*FACL(I)
            WRITE(ISTDO,1013) E22(I),MINEPS*FACL(I)
          ENDIF
          IF (ISHEAR(I) == 1) THEN 
            WRITE(IOUT, 1014) (E11(I) - E22(I))/TWO,EPSSH*FACL(I)
            WRITE(ISTDO,1014) (E11(I) - E22(I))/TWO,EPSSH*FACL(I)
          ENDIF
          IF (IMIX12(I) == 1) THEN
            WRITE(IOUT, 1015) (E11(I) - E22(I))/TWO,SH12(I)
            WRITE(ISTDO,1015) (E11(I) - E22(I))/TWO,SH12(I)
          ENDIF
          IF (IMXE1C(I) == 1) THEN 
            WRITE(IOUT, 1017) E11(I),E1C(I)
            WRITE(ISTDO,1017) E11(I),E1C(I)           
          ENDIF
          IF (IFLD(I) == 1) THEN 
            IF (Itab == 1) THEN 
              WRITE(IOUT, 1018) E11(I),E1FLD(I)*FACL(I)
              WRITE(ISTDO,1018) E11(I),E1FLD(I)*FACL(I)   
            ELSE
              WRITE(IOUT, 1018) E11(I),E1FLD(I)
              WRITE(ISTDO,1018) E11(I),E1FLD(I) 
            ENDIF
          ENDIF
          IF (ITHIN(I) == 1) THEN 
            IF (THIN < ZERO) THEN 
              WRITE(IOUT, 1019) (THKN(I)-UVAR(I,6))/UVAR(I,6),-ABS(THIN)*FACL(I)
              WRITE(ISTDO,1019) (THKN(I)-UVAR(I,6))/UVAR(I,6),-ABS(THIN)*FACL(I)
            ELSE
              WRITE(IOUT, 1021) (THKLYL(I)-UVAR(I,7))/UVAR(I,7),-ABS(THIN)*FACL(I)
              WRITE(ISTDO,1021) (THKLYL(I)-UVAR(I,7))/UVAR(I,7),-ABS(THIN)*FACL(I)
            ENDIF
          ENDIF
          IF (IMAXTEMP(I) == 1) THEN 
            WRITE(IOUT, 1020) TEMP(I),MAXTEMP
            WRITE(ISTDO,1020) TEMP(I),MAXTEMP
          ENDIF
#include "lockoff.inc"
        END DO
      END IF     
c------------------------
 1000 FORMAT(1X,'>> FOR SHELL ELEMENT NUMBER (GENE1) el#',I10,', GAUSS POINT #',I3,
     .          ', LAYER #',I3,', FAILURE START AT TIME: ',1PE12.4,', ',I3,' CRITERION HAS BEEN REACHED:')
 1001 FORMAT(1X,'>> FOR SHELL ELEMENT NUMBER (GENE1) el#',I10,', GAUSS POINT #',I3,
     .          ', LAYER #',I3,', FAILURE START AT TIME: ',1PE12.4,', ',I3,' CRITERIA HAVE BEEN REACHED:')
 1002 FORMAT(1X,'HYDROSTATIC PRESSURE VALUE:  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1003 FORMAT(1X,'HYDROSTATIC PRESSURE VALUE:  ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4) 
 1004 FORMAT(1X,'1ST PRINCIPAL STRESS VALUE:  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1005 FORMAT(1X,'TIME VALUE:                  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1006 FORMAT(1X,'ELEMENT TIMESTEP VALUE:      ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4) 
 1007 FORMAT(1X,'EQUIVALENT STRESS VALUE:     ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1008 FORMAT(1X,'T-BUTCHER INTG. VALUE:       ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1009 FORMAT(1X,'1ST PRINCIPAL STRAIN VALUE:  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1010 FORMAT(1X,'EFFECTIVE STRAIN VALUE:      ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1011 FORMAT(1X,'VOLUMETRIC STRAIN VALUE:     ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1012 FORMAT(1X,'VOLUMETRIC STRAIN VALUE:     ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1013 FORMAT(1X,'3RD PRINCIPAL STRAIN VALUE:  ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1014 FORMAT(1X,'MAX. SHEAR STRAIN VALUE:     ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1015 FORMAT(1X,'IN-PLANE SH.STRAIN 12 VALUE: ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1017 FORMAT(1X,'IN-PLANE PRINC.STRAIN VALUE: ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1018 FORMAT(1X,'1ST PRINCIPAL STRESS VALUE:  ',1PE12.4,' > FORMING LIMIT VALUE : ',1PE12.4)
 1019 FORMAT(1X,'AVERAGE THINNING VALUE:      ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1021 FORMAT(1X,'LAYER THINNING VALUE:        ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1020 FORMAT(1X,'TEMPERATURE VALUE:           ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
      END
