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
!||    fail_gene1_s          ../engine/source/materials/fail/gene1/fail_gene1_s.F
!||--- called by ------------------------------------------------------
!||    mmain                 ../engine/source/materials/mat_share/mmain.F90
!||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
!||    usermat_solid         ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    finter                ../engine/source/tools/curve/finter.F
!||    table2d_vinterp_log   ../engine/source/tools/curve/table2d_vinterp_log.F
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE FAIL_GENE1_S (
     1     NEL      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,LOFF     ,
     2     NPF      ,TF       ,TIME     ,TIMESTEP ,UPARAM   ,IPG      ,
     3     NGL      ,DT       ,EPSP     ,UVAR     ,OFF      ,NPG      ,
     4     EPSXX    ,EPSYY    ,EPSZZ    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     5     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6     TEMP     ,VOLN     ,DFMAX    ,TDELE    ,ALDT     ,TABLE    ,
     7     IRUPT    ,ELBUF_TAB,ILAY1    ,NTABLF   ,ITABLF   ,LF_DAMMX ,
     8     NIPARAM  ,IPARAM   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE ELBUFDEF_MOD 
C!-----------------------------------------------
C!   I m p l i c i t   T y p e s
C!-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "tabsiz_c.inc"
C!-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,NPG,NFUNC,NTABLF,
     .   NIPARAM,LF_DAMMX,IRUPT,ILAY1
      INTEGER, DIMENSION(NFUNC)  ,INTENT(IN) :: IFUNC
      INTEGER, DIMENSION(NTABLF) ,INTENT(IN) :: ITABLF
      INTEGER, DIMENSION(NEL)    ,INTENT(IN) :: NGL
      INTEGER, DIMENSION(NIPARAM),INTENT(IN) :: IPARAM
      my_real, INTENT(IN) :: TIME,TIMESTEP
      my_real, DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
      my_real, DIMENSION(NEL), INTENT(IN) :: EPSXX,EPSYY,EPSZZ,
     .   EPSXY,EPSYZ,EPSZX,DT,EPSP,ALDT,TEMP,VOLN
      my_real, DIMENSION(NEL), INTENT(INOUT) :: SIGNXX,SIGNYY,SIGNZZ,
     .   SIGNXY,SIGNYZ,SIGNZX,OFF,LOFF,TDELE
      my_real, DIMENSION(NEL,LF_DAMMX), INTENT(INOUT) :: DFMAX
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
      TYPE (TTABLE), DIMENSION(NTABLE) :: TABLE
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
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
      INTEGER I,K,J,INDX(NEL),NINDX,INDX2(NEL),NINDX2,NSTEP,CRIT,NMOD,
     .        fct_ISM,fct_IPS,fct_IDg12,fct_IDg13,fct_IDe1c,fct_IDel,
     .        NCRIT(NEL),IPOS(NEL,2),Ismooth,Istrain,IR,IS,IT,
     .        ILAY,tab_IDfld,Itab,NCS,INDX3(NEL),NINDX3,IPMAX(NEL),IPMIN(NEL),
     .        IS1MAX(NEL),ITMAX(NEL),IMINDT(NEL),ISIGMAX(NEL),ISIGTH(NEL),
     .        IEPSMAX(NEL),IEFFEPS(NEL),IVOLEPS(NEL),IMINEPS(NEL),ISHEAR(NEL),
     .        IMIX12(NEL),IMIX13(NEL),IMXE1C(NEL),IFLD(NEL),ITHIN(NEL),
     .        IMAXTEMP(NEL)
      my_real 
     .        MINPRES,MAXPRES,SIGP1,TMAX,DTMIN,EPSDOT_SM,SIGVM,SIGTH,
     .        KF,EPSDOT_PS,MAXEPS,EFFEPS,VOLEPS,MINEPS,EPSSH,EPSDOT_FLD,
     .        THIN,VOLFRAC,MAXTEMP,Fscale_el,El_ref,LAMBDA,FAC,DF
      my_real 
     .        E1,E2,E3,E4,E5,E6,E42,E52,E62,I1,I2,I3,P(NEL),SXX,SYY,SZZ,SVM(NEL),
     .        Q,R,R_INTER,PHI,E11(NEL),E22(NEL),E33(NEL),VOL_STRAIN(NEL),DAV,E1D,
     .        E2D,E3D,E4D,E5D,E6D,S11(NEL),S22(NEL),S33(NEL),EFF_STRAIN(NEL),PSI,
     .        EPSMAX(NEL),SIGMAX(NEL),FACL(NEL),SH12(NEL),SH13(NEL),E1C(NEL),
     .        XVEC(NEL,2),E1FLD(NEL),DFLD(NEL),HARDR(NEL),VTOT,VDAM,DENOM,VFAIL(NEL),
     .        TRIAX(NEL)
      TYPE(BUF_FAIL_) ,POINTER :: FBUF
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      ! -> Integer parameter, activated criteria
      CRIT       = IPARAM(1)
      Itab       = IPARAM(2)    !> Table dependency type (used only if tab_IDfld is a table).
      NSTEP      = IPARAM(3)    !> Number of cycles for the stress reduction, Default = 10 (Integer)
      NCS        = IPARAM(4)    !> Number of conditions to reach before the element is deleted, Default = 1 (Integer)
      Ismooth    = IPARAM(5)    !> Interpolation type (in case of tabulated yield function) 
      Istrain    = IPARAM(6)    !> Engineering / True input strain flag
      ! -> Real parameters
      MINPRES    = UPARAM(1)    !> Minimum pressure (positive in compression)
      MAXPRES    = UPARAM(2)    !> Maximum pressure (positive in compression)
      SIGP1      = UPARAM(3)    !> Maximum principal stress
      TMAX       = UPARAM(4)    !> Failure time, Default = 1E+20 (Real)
      DTMIN      = UPARAM(5)    !> Minimum time step
      EPSDOT_SM  = UPARAM(6)    !> Reference strain rate value for fct_IDsm, Default = 1 (Real)
      SIGVM      = UPARAM(7)    !> Von Mises stress 
      SIGTH      = UPARAM(8)    
      KF         = UPARAM(9)    !  -> Tuler-Butcher
      EPSDOT_PS  = UPARAM(10)   !> Reference strain rate value for fct_IDps, Default = 1 (Real)
      MAXEPS     = UPARAM(11)   !> Ordinate scale factor for fct_IDps or maximum principal strain if fct_IDps is not defined, Default = 1 
      EFFEPS     = UPARAM(12)   !> Maximum effective strain
      VOLEPS     = UPARAM(13)   !> Maximum volumetric strain
      MINEPS     = UPARAM(14)   !> Minimum principal strain
      EPSSH      = UPARAM(15) 
      EPSDOT_FLD = UPARAM(16)
      THIN       = UPARAM(17)   !Thinning failure value (Real)
      VOLFRAC    = UPARAM(18)   !> Damaged volume fraction to reach before the element is deleted (fully-integrated and higher order solid elements only), Default = 0.5 (Real)
      MAXTEMP    = UPARAM(20)   !> Maximum temperature.
      Fscale_el  = UPARAM(21)   !> Element size function scale factor for fct_IDel, tab_IDfld (Itab=2), fct_IDg12, fct_IDg23, fct_IDg13 and fct_IDe1c, Default = 1.0 (Real)
      El_ref     = UPARAM(22)   !> Reference element size for fct_IDel, tab_IDfld (Itab=2), fct_IDg12, fct_IDg23, fct_IDg13 and fct_IDe1c, Default = 1.0 (Real)
c
c     function & tables
      fct_ISM    = IFUNC(1)     !> Function identifier of the maximum equivalent stress versus strain rate 
      fct_IPS    = IFUNC(2)     !> maximum principal strain VS strain-rate
      fct_IDg12  = IFUNC(3)     !> in-plane shear strain VS element size 
      fct_IDg13  = IFUNC(4)     !> in-plane shear strain VS element size
      fct_IDe1c  = IFUNC(5)     !> major in plane-strain VS element size
      fct_IDel   = IFUNC(6)     !> element size regularization
      IF (NTABLF > 0) THEN
        tab_IDfld  = ITABLF(1)
      ELSE
        tab_IDfld  = 0
      END IF
c
      ! Initialization of variable
      NINDX  = 0 
      NINDX2 = 0
      NINDX3 = 0
      INDX(1:NEL)     = 0
      INDX2(1:NEL)    = 0 
      INDX3(1:NEL)    = 0 
      VFAIL(1:NEL)    = ZERO
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
      IMIX13(1:NEL)   = 0
      IMXE1C(1:NEL)   = 0
      IFLD(1:NEL)     = 0
      ITHIN(1:NEL)    = 0
      IMAXTEMP(1:NEL) = 0
      NCRIT(1:NEL)    = 0
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
      IF (UVAR(1,5) == ZERO.AND.(OFF(1) /= ZERO)) UVAR(1:NEL,5) = ONE
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
       SIGNZZ(I) = SIGNZZ(I)*UVAR(I,5)
       SIGNXY(I) = SIGNXY(I)*UVAR(I,5)
       SIGNYZ(I) = SIGNYZ(I)*UVAR(I,5)
       SIGNZX(I) = SIGNZX(I)*UVAR(I,5)
       ! Regularization factors for length, surface and volume
       FACL(I)   = UVAR(I,1)
       ! Storage of integration point volume
       IF (NPG > 1) THEN
         UVAR(I,3) = VOLN(I)
         IF (UVAR(I,5) == ZERO) UVAR(I,4) = VOLN(I)
       ENDIF
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
          !  -> computation of tensiorial strain
          E1  = EPSXX(I)
          E2  = EPSYY(I)
          E3  = EPSZZ(I)
          E4  = HALF*EPSXY(I)
          E5  = HALF*EPSYZ(I)
          E6  = HALF*EPSZX(I)
          !  -> computation of strain tensor invariants
          E42 = E4*E4
          E52 = E5*E5
          E62 = E6*E6
          I1  = E1 + E2 + E3
          I2  = E1*E2 + E2*E3 + E3*E1 - E4*E4 - E5*E5 - E6*E6
          I3  = E1*E2*E3 - E1*E52 - E2*E62 - E3*E42 + TWO*E4*E5*E6
          !  -> computation of principal strains
          Q   = (THREE*I2 - I1*I1)/NINE
          R   = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54  
          R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
          PHI = ACOS(MAX(R_INTER,-ONE))
          E11(I) = TWO*SQRT(MAX(ZERO,-Q))*COS(PHI/THREE)+THIRD*I1
          E22(I) = TWO*SQRT(MAX(ZERO,-Q))*COS((PHI+TWO*PI)/THREE)+THIRD*I1
          E33(I) = TWO*SQRT(MAX(ZERO,-Q))*COS((PHI+FOUR*PI)/THREE)+THIRD*I1
          IF (E11(I) < E22(I)) THEN 
            R_INTER = E11(I)
            E11(I)  = E22(I)
            E22(I)  = R_INTER
          ENDIF 
          IF (E22(I) < E33(I))THEN
            R_INTER = E22(I)
            E22(I)  = E33(I)
            E33(I)  = R_INTER
          ENDIF
          IF (E11(I) < E22(I))THEN
            R_INTER = E11(I)
            E11(I)  = E22(I)
            E22(I)  = R_INTER
          ENDIF
          !  -> computation of volumetric strain
          VOL_STRAIN(I) = E11(I) + E22(I) + E33(I)
c       
          !  -> computation of effective strain
          DAV = (EPSXX(I)+EPSYY(I)+EPSZZ(I))*THIRD
          E1D = EPSXX(I) - DAV
          E2D = EPSYY(I) - DAV
          E3D = EPSZZ(I) - DAV
          E4D = HALF*EPSXY(I)
          E5D = HALF*EPSYZ(I)
          E6D = HALF*EPSZX(I)
          EFF_STRAIN(I) = E1D**2 + E2D**2 + E3D**3 + TWO*(E4D**2 + E5D**2 + E6D**2)
          EFF_STRAIN(I) = SQRT(TWO_THIRD*EFF_STRAIN(I))
c
          ! --------------------------------------------------------------------------
          ! Computation of hydrostatic stress, Von Mises stress and principal stresses
          ! --------------------------------------------------------------------------
          !  -> pressure stress (positive in compression)
          P(I)   = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          !  -> equivalent stress of Von Mises
          SXX    = SIGNXX(I) + P(I)
          SYY    = SIGNYY(I) + P(I)
          SZZ    = SIGNZZ(I) + P(I)
          SVM(I) = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .          + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
          SVM(I) = SQRT(THREE*SVM(I))
          TRIAX(I) = -P(I)/MAX(SVM(I),EM20)
          !  -> computing the principal stresses
          I1 = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
          I2 = SIGNXX(I)*SIGNYY(I)+SIGNYY(I)*SIGNZZ(I)+SIGNZZ(I)*SIGNXX(I)-
     .         SIGNXY(I)*SIGNXY(I)-SIGNZX(I)*SIGNZX(I)-SIGNYZ(I)*SIGNYZ(I)
          I3 = SIGNXX(I)*SIGNYY(I)*SIGNZZ(I)-SIGNXX(I)*SIGNYZ(I)*SIGNYZ(I)-
     .         SIGNYY(I)*SIGNZX(I)*SIGNZX(I)-SIGNZZ(I)*SIGNXY(I)*SIGNXY(I)+
     .         TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
          Q  = (THREE*I2 - I1*I1)/NINE
          R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54
          R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
          PSI = ACOS(MAX(R_INTER,-ONE))
          S11(I) = TWO*SQRT(MAX(ZERO,-Q))*COS(PSI/THREE)+THIRD*I1
          S22(I) = TWO*SQRT(MAX(ZERO,-Q))*COS((PSI+TWO*PI)/THREE)+THIRD*I1
          S33(I) = TWO*SQRT(MAX(ZERO,-Q))*COS((PSI+FOUR*PI)/THREE)+THIRD*I1
          IF (S11(I) < S22(I)) THEN 
            R_INTER = S11(I)
            S11(I)  = S22(I)
            S22(I)  = R_INTER
          ENDIF 
          IF (S22(I) < S33(I)) THEN
            R_INTER = S22(I)
            S22(I)  = S33(I)
            S33(I)  = R_INTER
          ENDIF
          IF (S11(I) < S22(I)) THEN
            R_INTER = S11(I)
            S11(I)  = S22(I)
            S22(I)  = R_INTER
          ENDIF
c
        ! For broken element or Gauss point        
        ELSE
          E11(I) = ZERO
          E22(I) = ZERO
          E33(I) = ZERO
          VOL_STRAIN(I) = ZERO
          EFF_STRAIN(I) = ZERO
          P(I) = ZERO
          SVM(I) = ZERO
          TRIAX(I) = ZERO
          S11(I) = ZERO
          S22(I) = ZERO
          S33(I) = ZERO
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
            !     (restricted to positive stress triaxialities)
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
              DFMAX(I,1+NMOD) = MAX(DTMIN/DT(I),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
              IF ((DT(I) <= DTMIN).AND.(TIME > ZERO)) THEN 
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
            IF (E33(I) /= ZERO) THEN 
              DFMAX(I,1+NMOD) = MAX(MINEPS*FACL(I)/(E33(I)),DFMAX(I,1+NMOD))
            ENDIF
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF (E33(I) <= MINEPS*FACL(I)) THEN
              NCRIT(I) = NCRIT(I) + 1
              IMINEPS(I) = 1
            ENDIF
          ENDIF
          !  -> maximum tensorial shear strain
          IF (BTEST(CRIT,12)) THEN
            NMOD = NMOD + 1 
            DFMAX(I,1+NMOD) = MAX(((E11(I) - E33(I))/TWO)/(EPSSH*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE)  
            IF ((E11(I) - E33(I))/TWO >= EPSSH*FACL(I)) THEN 
              NCRIT(I)  = NCRIT(I) + 1
              ISHEAR(I) = 1
            ENDIF 
          ENDIF
          !  -> mixed mode 
          IF (BTEST(CRIT,13)) THEN
            LAMBDA  = UVAR(I,8)/El_ref
            SH12(I) = FINTER(fct_IDg12,LAMBDA,NPF,TF,DF) 
            DENOM   = SIGN(MAX(ABS(E11(I)),EM20),E11(I))  
            NMOD    = NMOD + 1 
            IF (((E22(I)/DENOM)<=-HALF).AND.((E22(I)/DENOM)>=-TWO)) THEN
              DFMAX(I,1+NMOD) = MAX(((E11(I) - E22(I))/TWO)/(SH12(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE) 
              IF ((E11(I) - E22(I))/TWO >= SH12(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMIX12(I) = 1
              ENDIF
            ENDIF
          ENDIF 
          IF (BTEST(CRIT,14)) THEN 
            LAMBDA  = UVAR(I,8)/El_ref
            SH13(I) = FINTER(fct_IDg13,LAMBDA,NPF,TF,DF)
            DENOM   = SIGN(MAX(ABS(E11(I)),EM20),E11(I))  
            NMOD    = NMOD + 1
            IF (((E22(I)/DENOM)<=ONE).AND.((E22(I)/DENOM)>=-HALF)) THEN
              DFMAX(I,1+NMOD) = MAX(((E11(I) - E33(I))/TWO)/(SH13(I)),DFMAX(I,1+NMOD))
              DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE) 
              IF ((E11(I) - E33(I))/TWO >= SH13(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMIX13(I) = 1
              ENDIF
            ENDIF
          ENDIF
          IF (BTEST(CRIT,15)) THEN
            LAMBDA = UVAR(I,8)/El_ref
            E1C(I) = FINTER(fct_IDe1c,LAMBDA,NPF,TF,DF) 
            DENOM  = SIGN(MAX(ABS(E11(I)),EM20),E11(I))  
            NMOD   = NMOD + 1 
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
              IF (E11(I) >= E1FLD(I)) THEN
                NCRIT(I) = NCRIT(I) + 1
                IFLD(I)  = 1
              ENDIF
            ENDIF
          ENDIF
          !  -> maximum shell thinning
          IF (BTEST(CRIT,17)) THEN 
            NMOD = NMOD + 1 
            DFMAX(I,1+NMOD) = MAX((EPSZZ(I))/(-ABS(THIN)*FACL(I)),DFMAX(I,1+NMOD))
            DFMAX(I,1+NMOD) = MIN(DFMAX(I,1+NMOD),ONE) 
            IF (EPSZZ(I) <= -ABS(THIN)*FACL(I)) THEN 
              NCRIT(I) = NCRIT(I) + 1
              ITHIN(I) = 1
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
            UVAR(I,5)   = UVAR(I,5) - ONE/NSTEP
            SIGNXX(I)   = SIGNXX(I)*UVAR(I,5)
            SIGNYY(I)   = SIGNYY(I)*UVAR(I,5)
            SIGNZZ(I)   = SIGNZZ(I)*UVAR(I,5)
            SIGNXY(I)   = SIGNXY(I)*UVAR(I,5)
            SIGNYZ(I)   = SIGNYZ(I)*UVAR(I,5)
            SIGNZX(I)   = SIGNZX(I)*UVAR(I,5)
            DFMAX(I,1)  = ONE
            NINDX       = NINDX + 1
            INDX(NINDX) = I
          ENDIF
        ENDIF
c
      ENDDO
c
      !====================================================================
      ! - LOOKING FOR ELEMENT DELETION
      !====================================================================      
      ! Case of under-integrated solids
      IF (NPG == 1) THEN 
        DO I = 1,NEL
          IF ((UVAR(I,5) == ZERO).AND.(OFF(I) /= ZERO)) THEN 
            NINDX2        = NINDX2 + 1
            INDX2(NINDX2) = I
            DFMAX(I,1)    = ONE 
            OFF(I)        = ZERO
            TDELE(I)      = TIME
          ENDIF
        ENDDO
      ! Case of fully integrated solids
      ELSE
        IF ((IPG == NPG).AND.(ILAY1 == ELBUF_TAB%NLAY)) THEN 
          DO I = 1,NEL       
            IF (OFF(I) == ONE) THEN 
              ! Initialization of volumes computation
              VTOT = ZERO
              VDAM = ZERO
              ! Loop over integration points
              DO ILAY = 1, ELBUF_TAB%NLAY
                DO IR = 1, ELBUF_TAB%NPTR
                  DO IS = 1, ELBUF_TAB%NPTS
                    DO IT = 1, ELBUF_TAB%NPTT
                      FBUF => ELBUF_TAB%BUFLY(ILAY)%FAIL(IR,IS,IT)
                      ! Compute total volume of the element
                      VTOT = VTOT + FBUF%FLOC(IRUPT)%VAR(2*NEL+I)
                      ! Compute damaged volume of the element
                      VDAM = VDAM + FBUF%FLOC(IRUPT)%VAR(3*NEL+I)
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
              ! Checking volume fraction criterion
              IF ((VDAM/VTOT) >= VOLFRAC) THEN
                DFMAX(I,1)    = ONE
                NINDX3        = NINDX3 + 1
                INDX3(NINDX3) = I
                VFAIL(I)      = VDAM/VTOT
                TDELE(I)      = TIME
                OFF(I)        = ZERO
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
c------------------------
c------------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)     
#include "lockon.inc"
          IF (NCRIT(I) == 1) THEN
            WRITE(IOUT, 1000) NGL(I),IPG,TIME,NCRIT(I)
            WRITE(ISTDO,1000) NGL(I),IPG,TIME,NCRIT(I)
          ELSE
            WRITE(IOUT, 1001) NGL(I),IPG,TIME,NCRIT(I)
            WRITE(ISTDO,1001) NGL(I),IPG,TIME,NCRIT(I)
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
            WRITE(IOUT, 1006) DT(I),DTMIN
            WRITE(ISTDO,1006) DT(I),DTMIN        
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
            WRITE(IOUT, 1013) E33(I),MINEPS*FACL(I)
            WRITE(ISTDO,1013) E33(I),MINEPS*FACL(I)
          ENDIF
          IF (ISHEAR(I) == 1) THEN 
            WRITE(IOUT, 1014) (E11(I) - E33(I))/TWO,EPSSH*FACL(I)
            WRITE(ISTDO,1014) (E11(I) - E33(I))/TWO,EPSSH*FACL(I)
          ENDIF
          IF (IMIX12(I) == 1) THEN
            WRITE(IOUT, 1015) (E11(I) - E22(I))/TWO,SH12(I)
            WRITE(ISTDO,1015) (E11(I) - E22(I))/TWO,SH12(I)
          ENDIF
          IF (IMIX13(I) == 1) THEN
            WRITE(IOUT, 1016) (E11(I) - E33(I))/TWO,SH13(I)
            WRITE(ISTDO,1016) (E11(I) - E33(I))/TWO,SH13(I)
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
            WRITE(IOUT, 1019) EPSZZ(I),-ABS(THIN)*FACL(I)
            WRITE(ISTDO,1019) EPSZZ(I),-ABS(THIN)*FACL(I)
          ENDIF
          IF (IMAXTEMP(I) == 1) THEN 
            WRITE(IOUT, 1020) TEMP(I),MAXTEMP
            WRITE(ISTDO,1020) TEMP(I),MAXTEMP
          ENDIF
#include "lockoff.inc"
        END DO
      END IF    
      IF (NINDX2 > 0) THEN
        DO J=1,NINDX2
          I = INDX2(J)     
#include "lockon.inc"
          WRITE(IOUT, 1021) NGL(I),TIME
          WRITE(ISTDO,1021) NGL(I),TIME
#include "lockoff.inc"
        END DO
      END IF      
      IF (NINDX3 > 0) THEN
        DO J=1,NINDX3
          I = INDX3(J)     
#include "lockon.inc"
          WRITE(IOUT, 1021) NGL(I),TIME
          WRITE(ISTDO,1021) NGL(I),TIME
          WRITE(IOUT, 3000) VFAIL(I),VOLFRAC
          WRITE(ISTDO,3000) VFAIL(I),VOLFRAC
#include "lockoff.inc"
        END DO
      END IF     
c------------------------
 1000 FORMAT(1X,'>> FOR SOLID ELEMENT NUMBER (GENE1) el#',I10,', GAUSS POINT # ',I5,
     .          ', FAILURE START AT TIME: ',1PE12.4,', ',I3,' CRITERION HAS BEEN REACHED:')     
 1001 FORMAT(1X,'>> FOR SOLID ELEMENT NUMBER (GENE1) el#',I10,', GAUSS POINT # ',I5,
     .          ', FAILURE START AT TIME: ',1PE12.4,', ',I3,' CRITERIA HAVE BEEN REACHED:')
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
 1019 FORMAT(1X,'THINNING VALUE:              ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1020 FORMAT(1X,'TEMPERATURE VALUE:           ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)  
 1021 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,' AT TIME :',1PE12.4)    
 3000 FORMAT(1X,'DAMAGED VOLUME FRACTION : ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)  
      END
