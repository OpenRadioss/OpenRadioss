Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    finter                ../engine/source/tools/curve/finter.F
!||    table2d_vinterp_log   ../engine/source/tools/curve/table2d_vinterp_log.F
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    dt_mod                ../engine/source/modules/dt_mod.F
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE FAIL_GENE1_C (
     1     NEL      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     2     NPF      ,TF       ,TIME     ,TIMESTEP ,UPARAM   ,IPG      ,
     3     NGL      ,GBUF_DT  ,EPSP     ,UVAR     ,OFF      ,
     4     EPSXX    ,EPSYY    ,EPSXY    ,AREA     ,THKN     ,
     5     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6     TEMP     ,DFMAX    ,ALDT     ,TABLE    ,TDELE    ,
     7     THK0     ,IPT      ,FOFF     ,THKLYL   ,NTABLF   ,ITABLF   ,
     8     LF_DAMMX ,NIPARAM  ,IPARAM   ,DT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE DT_MOD
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
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,IPT,NFUNC,NTABLF,
     .   NIPARAM,LF_DAMMX
      INTEGER, DIMENSION(NFUNC)  ,INTENT(IN) :: IFUNC
      INTEGER, DIMENSION(NTABLF) ,INTENT(IN) :: ITABLF
      INTEGER, DIMENSION(NEL)    ,INTENT(IN) :: NGL
      INTEGER, DIMENSION(NIPARAM),INTENT(IN) :: IPARAM
      my_real, INTENT(IN) :: TIME,TIMESTEP
      my_real, DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
      my_real, DIMENSION(NEL), INTENT(IN) :: EPSXX,EPSYY,EPSXY,
     .   GBUF_DT,EPSP,OFF,ALDT,TEMP,AREA,THKN,THK0,THKLYL
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: FOFF
      my_real, DIMENSION(NEL), INTENT(INOUT) :: SIGNXX,SIGNYY,SIGNXY,
     .   SIGNYZ,SIGNZX,TDELE
      my_real, DIMENSION(NEL,LF_DAMMX), INTENT(INOUT) :: DFMAX
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
      TYPE (TTABLE), DIMENSION(NTABLE), INTENT(IN) :: TABLE
      TYPE (DT_), INTENT(IN) :: DT
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF)
      my_real  FINTER 
      EXTERNAL FINTER
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,K,J,INDX1(NEL),NINDX1,NSTEP,CRIT,NMOD,
     .        fct_ISM,fct_IPS,fct_IDg12,fct_IDe1c,fct_IDel,
     .        NCRIT(NEL),IPOS(NEL,2),Ismooth,Istrain,IR,IS,IT,
     .        tab_IDfld,Itab,NCS,IPMAX(NEL),IPMIN(NEL),IS1MAX(NEL),ILAY,
     .        ITMAX(NEL),IMINDT(NEL),ISIGMAX(NEL),ISIGTH(NEL),IEPSMAX(NEL),
     .        IEFFEPS(NEL),IVOLEPS(NEL),IMINEPS(NEL),ISHEAR(NEL),IMIX12(NEL),
     .        IMXE1C(NEL),IFLD(NEL),ITHIN(NEL),IMAXTEMP(NEL)
      my_real 
     .        MINPRES,MAXPRES,SIGP1,TMAX,DTMIN,EPSDOT_SM,SIGVM,SIGTH,
     .        KF,EPSDOT_PS,MAXEPS,EFFEPS,VOLEPS,MINEPS,EPSSH,EPSDOT_FLD,
     .        THIN,MAXTEMP,Fscale_el,El_ref,LAMBDA,FAC,DF
      my_real 
     .        E12,P(NEL),SXX,SYY,SZZ,SVM(NEL),SH12(NEL),SH13(NEL),E1C(NEL),
     .        XVEC(NEL,2),Q,R,R_INTER,E11(NEL),E22(NEL),VOL_STRAIN(NEL),DAV,
     .        E1D,E2D,E3D,E4D,S11(NEL),S22(NEL),EFF_STRAIN(NEL),S1,S2,
     .        EPSMAX(NEL),SIGMAX(NEL),FACL(NEL),E1FLD(NEL),
     .        DFLD(NEL),HARDR(NEL),DENOM,TRIAX(NEL)
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      ! -> Integer parameter, activated criteria
      CRIT       = IPARAM(1)
      Itab       = IPARAM(2)
      NSTEP      = IPARAM(3)
      NCS        = IPARAM(4)
      Ismooth    = IPARAM(5)
      Istrain    = IPARAM(6)
      ! -> Real parameters
      MINPRES    = UPARAM(1)
      MAXPRES    = UPARAM(2)
      SIGP1      = UPARAM(3)
      TMAX       = UPARAM(4)   
      DTMIN      = UPARAM(5)
      EPSDOT_SM  = UPARAM(6)
      SIGVM      = UPARAM(7)
      SIGTH      = UPARAM(8)
      KF         = UPARAM(9)
      EPSDOT_PS  = UPARAM(10)
      MAXEPS     = UPARAM(11)
      EFFEPS     = UPARAM(12)
      VOLEPS     = UPARAM(13)
      MINEPS     = UPARAM(14)
      EPSSH      = UPARAM(15)
      EPSDOT_FLD = UPARAM(16)
      THIN       = UPARAM(17)
      MAXTEMP    = UPARAM(20)
      Fscale_el  = UPARAM(21)
      El_ref     = UPARAM(22)
c
      ! Initialization of variable
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
      fct_ISM    = IFUNC(1) 
      fct_IPS    = IFUNC(2)  ! -> maximum principal strain VS strain-rate
      fct_IDg12  = IFUNC(3)  ! -> in-plane shear strain VS element size
      fct_IDe1c  = IFUNC(5)  ! -> major in plane-strain VS element size
      fct_IDel   = IFUNC(6)  ! -> element size regularization
      IF (NTABLF > 0) THEN
        tab_IDfld  = ITABLF(1)
      ELSE
        tab_IDfld  = 0
      END IF
c      
      ! At initial time, compute the element size regularization factor
      IF (UVAR(1,1)==ZERO) THEN 
        IF (fct_IDel > 0) THEN 
          DO I=1,NEL   
            LAMBDA      = ALDT(I)/El_ref
            FAC         = FINTER(fct_IDel,LAMBDA,NPF,TF,DF) 
            UVAR(I,1)   = FAC*Fscale_el
          ENDDO
        ELSE
          UVAR(1:NEL,1) = ONE
        ENDIF
      ENDIF  
      IF ((UVAR(1,5) == ZERO).AND.(FOFF(1) /= 0)) UVAR(1:NEL,5) = ONE
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
       FACL(I)   = UVAR(I,1)
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
          EFF_STRAIN(I) = E1D**2 + E2D**2 + E3D**3 + TWO*(E4D**2)
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
            IPOS(1:NEL,1:2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(tab_IDfld),Ismooth,NEL,NEL,IPOS,XVEC,E1FLD,DFLD,HARDR)
          ! Diagram using engineering strain
          ELSE
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = EXP(E22(1:NEL))-ONE
            XVEC(1:NEL,2) = EPSP(1:NEL)/EPSDOT_FLD
            !   -> Tensile yield stress in direction 1 (MD)
            IPOS(1:NEL,1:2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(tab_IDfld),Ismooth,NEL,NEL,IPOS,XVEC,E1FLD,DFLD,HARDR)
            E1FLD = LOG(ONE + E1FLD)
          ENDIF
        ELSE
          ! Diagram using true strains
          IF (Istrain == 0) THEN 
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = E22(1:NEL)
            XVEC(1:NEL,2) = ALDT(1:NEL)/El_ref
            !   -> Tensile yield stress in direction 1 (MD)
            IPOS(1:NEL,1:2) = 1
            CALL TABLE_VINTERP(TABLE(tab_IDfld),NEL,NEL,IPOS,XVEC,E1FLD,DFLD)
          ! Diagram using engineering strains
          ELSE
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = EXP(E22(1:NEL))-ONE
            XVEC(1:NEL,2) = ALDT(1:NEL)/El_ref
            !   -> Tensile yield stress in direction 1 (MD)
            IPOS(1:NEL,1:2) = 1
            CALL TABLE_VINTERP(TABLE(tab_IDfld),NEL,NEL,IPOS,XVEC,E1FLD,DFLD)
            E1FLD = LOG(ONE + E1FLD)
          ENDIF
        ENDIF
      ENDIF 
c
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO CHECK THE EROSION CRITERIA
      !====================================================================    
      DO I = 1,NEL
        NMOD = 0
        IF ((UVAR(I,5) == ONE).AND.(OFF(I)==ONE)) THEN
          !  -> minimum pressure
          IF (BTEST(CRIT,1)) THEN 
            NMOD = NMOD + 1
            DFMAX(I,1+NMOD) = MAX(P(I)/(MINPRES*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)
            IF (P(I) <= MINPRES*FACL(I)) THEN
              NCRIT(I) = NCRIT(I) + 1
              IPMIN(I) = 1
            ENDIF
          ENDIF
          !  -> maximum pressure
          IF (BTEST(CRIT,2)) THEN 
            NMOD = NMOD + 1
            DFMAX(I,1+NMOD) = MAX(P(I)/(MAXPRES*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)
            IF (P(I) >= MAXPRES*FACL(I)) THEN
              NCRIT(I) = NCRIT(I) + 1
              IPMAX(I) = 1
            ENDIF
          ENDIF
          !  -> maximal principal stress
          IF (BTEST(CRIT,3)) THEN 
            NMOD = NMOD + 1
            ! (unrestricted)
            IF (SIGP1 > ZERO) THEN
              DFMAX(I,1+NMOD) = MAX(S11(I)/(SIGP1*FACL(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE) 
              IF (S11(I) >= SIGP1*FACL(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IS1MAX(I) = 1
              ENDIF
            ! (restricted to positive stress triaxialities)
            ELSE
              IF (TRIAX(I)>EM10) THEN 
                DFMAX(I,1+NMOD) = MAX(S11(I)/(ABS(SIGP1)*FACL(I)),DFMAX(I,1+NMOD))
                DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE) 
                IF (S11(I) >= ABS(SIGP1)*FACL(I)) THEN 
                  NCRIT(I)  = NCRIT(I) + 1
                  IS1MAX(I) = 1  
                ENDIF          
              ENDIF
            ENDIF
          ENDIF
          !  -> maximum time
          IF (BTEST(CRIT,4)) THEN 
            NMOD = NMOD + 1
            DFMAX(I,1+NMOD) = MAX(TIME/TMAX,DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)             
            IF (TIME >= TMAX) THEN
              NCRIT(I) = NCRIT(I) + 1
              ITMAX(I) = 1
            ENDIF
          ENDIF
          !  -> minimum timestep
          IF (BTEST(CRIT,5)) THEN 
            NMOD = NMOD + 1
            IF (TIME > ZERO) THEN 
              DFMAX(I,1+NMOD) = MAX(DTMIN/(GBUF_DT(I)*DTFAC1(1)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)   
              IF (GBUF_DT(I)*DTFAC1(1) <= DTMIN) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMINDT(I) = 1
              ENDIF 
            ENDIF
          ENDIF
          !  -> equivalent stress
          IF (BTEST(CRIT,6)) THEN
            NMOD = NMOD + 1 
            IF (EPSDOT_SM /= ZERO) THEN 
              LAMBDA    = EPSP(I)/EPSDOT_SM
              SIGMAX(I) = FINTER(fct_ISM,LAMBDA,NPF,TF,DF) 
              SIGMAX(I) = SIGMAX(I)*SIGVM        
            ELSE
              SIGMAX(I) = SIGVM
            ENDIF
            DFMAX(I,1+NMOD) = MAX(SVM(I)/(SIGMAX(I)*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)   
            IF (SVM(I) >= SIGMAX(I)*FACL(I)) THEN 
              NCRIT(I)   = NCRIT(I) + 1  
              ISIGMAX(I) = 1
            ENDIF
          ENDIF
          !  -> Tuler-Butcher
          IF (BTEST(CRIT,7)) THEN
            NMOD = NMOD + 1 
            DFMAX(I,1+NMOD) = MAX(UVAR(I,2)/(KF*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)   
            IF (S11(I) > SIGTH) THEN 
              UVAR(I,2) = UVAR(I,2) + TIMESTEP*(S11(I) - SIGTH)**2
              IF (UVAR(I,2) >= KF*FACL(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1 
                ISIGTH(I) = 1
              ENDIF
            ENDIF
          ENDIF
          !  -> maximal principal strain
          IF (BTEST(CRIT,8)) THEN
            NMOD = NMOD + 1 
            IF (EPSDOT_PS /= ZERO) THEN 
              LAMBDA    = EPSP(I)/EPSDOT_PS    
              EPSMAX(I) = FINTER(fct_IPS,LAMBDA,NPF,TF,DF)
              EPSMAX(I) = EPSMAX(I)*MAXEPS
            ELSE
              EPSMAX(I) = MAXEPS
            ENDIF
            DFMAX(I,1+NMOD) = MAX(E11(I)/(EPSMAX(I)*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF (E11(I) >= EPSMAX(I)*FACL(I)) THEN 
              NCRIT(I)   = NCRIT(I) + 1       
              IEPSMAX(I) = 1
            ENDIF  
          ENDIF
          !  -> maximum effective strain
          IF (BTEST(CRIT,9)) THEN
            NMOD = NMOD + 1 
            DFMAX(I,1+NMOD) = MAX(EFF_STRAIN(I)/(EFFEPS*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF (EFF_STRAIN(I) >= EFFEPS*FACL(I)) THEN
              NCRIT(I)   = NCRIT(I) + 1
              IEFFEPS(I) = 1
            ENDIF
          ENDIF
          !  -> maximum volumetric strain
          IF (BTEST(CRIT,10)) THEN
            NMOD = NMOD + 1 
            IF (VOLEPS > ZERO) THEN 
              DFMAX(I,1+NMOD) = MAX(VOL_STRAIN(I)/(VOLEPS*FACL(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
              IF (VOL_STRAIN(I) >= VOLEPS*FACL(I)) THEN 
                NCRIT(I)   = NCRIT(I) + 1
                IVOLEPS(I) = 1
              ENDIF
            ELSE
              DFMAX(I,1+NMOD) = MAX(VOL_STRAIN(I)/(VOLEPS*FACL(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
              IF (VOL_STRAIN(I) <= VOLEPS*FACL(I)) THEN 
                NCRIT(I)   = NCRIT(I) + 1
                IVOLEPS(I) = 1
              ENDIF
            ENDIF
          ENDIF
          !  -> minimum principal strain
          IF (BTEST(CRIT,11)) THEN
            NMOD = NMOD + 1 
            IF (E22(I) /= ZERO) THEN 
              DFMAX(I,1+NMOD) = MAX(MINEPS*FACL(I)/(E22(I)),DFMAX(I,1+NMOD))
            ENDIF
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF (E22(I) <= MINEPS*FACL(I)) THEN
              NCRIT(I) = NCRIT(I) + 1
              IMINEPS(I) = 1
            ENDIF
          ENDIF
          !  -> maximum tensorial shear strain
          IF (BTEST(CRIT,12)) THEN
            NMOD = NMOD + 1 
            DFMAX(I,1+NMOD) = MAX(((E11(I) - E22(I))/TWO)/(EPSSH*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF ((E11(I) - E22(I))/TWO >= EPSSH*FACL(I)) THEN 
              NCRIT(I)  = NCRIT(I) + 1
              ISHEAR(I) = 1
            ENDIF 
          ENDIF
          !  -> mixed mode 
          IF (BTEST(CRIT,13)) THEN
            LAMBDA  = UVAR(I,8)/El_ref
            SH12(I) = FINTER(fct_IDel,LAMBDA,NPF,TF,DF) 
            DENOM   = SIGN(MAX(ABS(E11(I)),EM20),E11(I))   
            NMOD = NMOD + 1 
            IF (((E22(I)/DENOM)<=-HALF).AND.((E22(I)/DENOM)>=-TWO)) THEN
              DFMAX(I,1+NMOD) = MAX(((E11(I) - E22(I))/TWO)/(SH12(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
              IF ((E11(I) - E22(I))/TWO >= SH12(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMIX12(I) = 1
              ENDIF
            ENDIF
          ENDIF
          IF (BTEST(CRIT,15)) THEN
            LAMBDA = UVAR(I,8)/El_ref
            E1C(I) = FINTER(fct_IDe1c,LAMBDA,NPF,TF,DF) 
            DENOM  = SIGN(MAX(ABS(E11(I)),EM20),E11(I))
            NMOD = NMOD + 1 
            IF (((E22(I)/DENOM)<=ONE).AND.((E22(I)/DENOM)>=-HALF)) THEN
              DFMAX(I,1+NMOD) = MAX(E11(I)/E1C(I),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
              IF (E11(I) >= E1C(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMXE1C(I) = 1
              ENDIF
            ENDIF
          ENDIF
          !  -> Forming limit diagram
          IF (BTEST(CRIT,16)) THEN 
            NMOD = NMOD + 1 
            IF (NTABLF > 0) THEN 
              IF (Itab == 1) THEN 
                DFMAX(I,1+NMOD) = MAX(E11(I)/(E1FLD(I)*FACL(I)),DFMAX(I,1+NMOD))
                DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
                IF (E11(I) >= E1FLD(I)*FACL(I)) THEN 
                  NCRIT(I) = NCRIT(I) + 1
                  IFLD(I)  = 1
                ENDIF
              ELSE
                DFMAX(I,1+NMOD) = MAX(E11(I)/(E1FLD(I)),DFMAX(I,1+NMOD))
                DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
                IF (E11(I) >= E1FLD(I))  THEN
                  NCRIT(I) = NCRIT(I) + 1
                  IFLD(I)  = 1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          !  -> maximum shell thinning
          IF (BTEST(CRIT,17)) THEN 
            NMOD = NMOD + 1 
            IF (THIN < ZERO) THEN
              DFMAX(I,1+NMOD) = MAX(((THKN(I)-UVAR(I,6))/UVAR(I,6))/
     .                              (-ABS(THIN)*FACL(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
              IF (((THKN(I)-UVAR(I,6))/UVAR(I,6)) <= -ABS(THIN)*FACL(I)) THEN 
                NCRIT(I) = NCRIT(I) + 1
                ITHIN(I) = 1
              ENDIF
            ELSE
              DFMAX(I,1+NMOD) = MAX(((THKLYL(I)-UVAR(I,7))/UVAR(I,7))/
     .                              (-ABS(THIN)*FACL(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
              IF (((THKLYL(I)-UVAR(I,7))/UVAR(I,7)) <= -ABS(THIN)*FACL(I)) THEN 
                NCRIT(I) = NCRIT(I) + 1
                ITHIN(I) = 1
              ENDIF
            ENDIF
          ENDIF
          !  -> maximum temperature
          IF (BTEST(CRIT,18)) THEN 
            NMOD = NMOD + 1 
            DFMAX(I,1+NMOD) = MAX(TEMP(I)/MAXTEMP,DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF (TEMP(I) >= MAXTEMP) THEN 
              NCRIT(I)    = NCRIT(I) + 1 
              IMAXTEMP(I) = 1
            ENDIF
          ENDIF
c
          !  -> Checking failure
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
        ENDIF
c
      ENDDO
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
 1016 FORMAT(1X,'TRANSV.  SH.STRAIN 13 VALUE: ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1017 FORMAT(1X,'IN-PLANE PRINC.STRAIN VALUE: ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1018 FORMAT(1X,'1ST PRINCIPAL STRESS VALUE:  ',1PE12.4,' > FORMING LIMIT VALUE : ',1PE12.4)
 1019 FORMAT(1X,'AVERAGE THINNING VALUE:      ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1021 FORMAT(1X,'LAYER THINNING VALUE:        ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1020 FORMAT(1X,'TEMPERATURE VALUE:           ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
      END
