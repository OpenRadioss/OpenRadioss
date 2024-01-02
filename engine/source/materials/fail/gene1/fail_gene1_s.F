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
Chd|  FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_GENE1_S (
     1     NEL      ,NUPARAM  ,NUVAR    ,MFUNC    ,KFUNC    ,LOFF     ,
     2     NPF      ,TF       ,TIME     ,TIMESTEP ,UPARAM   ,IPG      ,
     3     NGL      ,DT       ,EPSP     ,UVAR     ,OFF      ,NPG      ,
     4     EPSXX    ,EPSYY    ,EPSZZ    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     5     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6     TEMP     ,VOLN     ,DFMAX    ,TDELE    ,ALDT     ,TABLE    ,
     7     IRUPT    ,ELBUF_TAB,ILAY1    )
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
C!-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IPG,NPG,IRUPT,ILAY1
      my_real TIME,TIMESTEP,UPARAM(*),
     .   EPSXX(NEL),EPSYY(NEL),EPSZZ(NEL),
     .   EPSXY(NEL),EPSYZ(NEL),EPSZX(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),UVAR(NEL,NUVAR),
     .   DT(NEL),EPSP(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL),
     .   ALDT(NEL),TEMP(NEL),VOLN(NEL),LOFF(NEL)
      TYPE (TTABLE), DIMENSION(NTABLE) :: TABLE
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C!        Y = FINTER(IFUNC(J),X,NPF,TF,DF)
C!        Y       : y = f(x)
C!        X       : x
C!        DF      : f'(x) = dy/dx
C!        IFUNC(J): FUNCTION INDEX
C!              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C!        NPF,TF  : FUNCTION PARAMETER
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,K,J,INDX(NEL),NINDX,INDX2(NEL),NINDX2,IREG,NSTEP,
     .        ISM,IPS,IG12,IG13,IE1C,fct_IDg12,fct_IDg13,fct_IDe1c,
     .        NCRIT(NEL),fct_IDel,IPOS(NEL,2),Ismooth,Istrain,IR,IS,IT,
     .        ILAY,tab_IDfld,Itab,NCS,INDX3(NEL),NINDX3,IPMAX(NEL),IPMIN(NEL),
     .        IS1MAX(NEL),ITMAX(NEL),IMINDT(NEL),ISIGMAX(NEL),ISIGTH(NEL),
     .        IEPSMAX(NEL),IEFFEPS(NEL),IVOLEPS(NEL),IMINEPS(NEL),ISHEAR(NEL),
     .        IMIX12(NEL),IMIX13(NEL),IMXE1C(NEL),IFLD(NEL),ITHIN(NEL),
     .        IMAXTEMP(NEL)
      my_real 
     .        MINPRES,MAXPRES,SIGP1,TMAX,DTMIN,EPSDOT_SM,SIGVM,SIGTH,
     .        KF,EPSDOT_PS,MAXEPS,EFFEPS,VOLEPS,MINEPS,EPSSH,EPSDOT_FLD,
     .        THIN,VOLFRAC,PTHK,MAXTEMP,Fscale_el,El_ref,LAMBDA,FAC,DF
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
      tab_IDfld  = NINT(UPARAM(16))
      Itab       = NINT(UPARAM(17))
      EPSDOT_FLD = UPARAM(18)
      NSTEP      = NINT(UPARAM(19))
      THIN       = UPARAM(20)
      VOLFRAC    = UPARAM(21)
      PTHK       = UPARAM(22)
      NCS        = NINT(UPARAM(23))
      MAXTEMP    = UPARAM(24)
      Fscale_el  = UPARAM(25)
      El_ref     = UPARAM(26)
      fct_IDg12  = NINT(UPARAM(27))
      fct_IDg13  = NINT(UPARAM(28))
      fct_IDe1c  = NINT(UPARAM(29))
      fct_IDel   = NINT(UPARAM(30))
      Ismooth    = NINT(UPARAM(31))
      Istrain    = NINT(UPARAM(32))
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
      ! Recovering functions
      ! -> equivalent stress VS strain-rate
      ISM  = 0
      IPS  = 0
      IG12 = 0
      IG13 = 0
      IE1C = 0
      IREG = 0 
      IF (EPSDOT_SM /= ZERO) THEN
        ISM = ISM + 1
      ENDIF    
      ! -> maximul principal strain VS strain-rate
      IPS = ISM
      IF (EPSDOT_PS /= ZERO) THEN
        IPS = IPS + 1
      ENDIF   
      ! -> in-plane shear strain VS element size
      IG12 = IPS
      IF (fct_IDg12 > 0) THEN 
        IG12 = IG12 + 1
      ENDIF    
      ! -> transverse shear strain VS element size
      IG13 = IG12
      IF (fct_IDg13 > 0) THEN 
        IG13 = IG13 + 1
      ENDIF
      ! -> major in plane-strain VS element size
      IE1C = IG13
      IF (fct_IDe1c > 0) THEN
        IE1C = IE1C + 1
      ENDIF
      ! -> element size regularization
      IREG  = IE1C
      IF (fct_IDel > 0) THEN
        IREG = IREG + 1
      ENDIF       
c      
      ! At initial time, compute the element size regularization factor
      IF (ISIGI == 0) THEN 
        IF (UVAR(1,1)==ZERO) THEN 
          IF (fct_IDel > 0) THEN 
            DO I=1,NEL   
              LAMBDA      = ALDT(I)/El_ref
              FAC         = FINTER(KFUNC(IREG),LAMBDA,NPF,TF,DF) 
              UVAR(I,1)   = FAC*Fscale_el
            ENDDO
          ELSE
            UVAR(1:NEL,1) = ONE
          ENDIF
        ENDIF  
        IF (UVAR(1,5) == ZERO.AND.(OFF(1) /= ZERO)) UVAR(1:NEL,5) = ONE
        IF (UVAR(1,8) == ZERO) UVAR(1:NEL,8) = ALDT(1:NEL)
      ENDIF
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
        E11(I) = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
        E22(I) = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
        E33(I) = TWO*SQRT(-Q)*COS((PHI+FOUR*PI)/THREE)+THIRD*I1
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
     .        + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
        SVM(I) = SQRT(THREE*SVM(I))
        TRIAX(I) = -P(I)/MAX(SVM(I),EM20)
        !  -> computing the principal stresses
        I1 = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
        I2 = SIGNXX(I)*SIGNYY(I)+SIGNYY(I)*SIGNZZ(I)+SIGNZZ(I)*SIGNXX(I)-
     .       SIGNXY(I)*SIGNXY(I)-SIGNZX(I)*SIGNZX(I)-SIGNYZ(I)*SIGNYZ(I)
        I3 = SIGNXX(I)*SIGNYY(I)*SIGNZZ(I)-SIGNXX(I)*SIGNYZ(I)*SIGNYZ(I)-
     .       SIGNYY(I)*SIGNZX(I)*SIGNZX(I)-SIGNZZ(I)*SIGNXY(I)*SIGNXY(I)+
     .       TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
        Q  = (THREE*I2 - I1*I1)/NINE
        R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54
        R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
        PSI = ACOS(MAX(R_INTER,-ONE))
        S11(I) = TWO*SQRT(-Q)*COS(PSI/THREE)+THIRD*I1
        S22(I) = TWO*SQRT(-Q)*COS((PSI+TWO*PI)/THREE)+THIRD*I1
        S33(I) = TWO*SQRT(-Q)*COS((PSI+FOUR*PI)/THREE)+THIRD*I1
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
      ENDDO
c
      !  -> Forming limit diagram
      IF (tab_IDfld > 0) THEN 
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
        IF ((UVAR(I,5) == ONE).AND.(OFF(I)==ONE)) THEN
          !  -> maximum pressure
          IF (P(I) >= MAXPRES*FACL(I)) THEN
            NCRIT(I) = NCRIT(I) + 1
            IPMAX(I) = 1
          ENDIF
          !  -> minimum pressure
          IF (P(I) <= MINPRES*FACL(I)) THEN
            NCRIT(I) = NCRIT(I) + 1
            IPMIN(I) = 1
          ENDIF
          !  -> maximal principal stress
          !     (unrestricted)
          IF (SIGP1 > ZERO) THEN 
            IF (S11(I) >= SIGP1*FACL(I)) THEN 
              NCRIT(I)  = NCRIT(I) + 1
              IS1MAX(I) = 1
            ENDIF
          !     (restricted to positive stress triaxialities)
          ELSE 
            IF ((S11(I) >= ABS(SIGP1)*FACL(I)).AND.(TRIAX(I)>EM10)) THEN 
              NCRIT(I)  = NCRIT(I) + 1
              IS1MAX(I) = 1
            ENDIF
          ENDIF
          !  -> maximum time
          IF (TIME >= TMAX) THEN
            NCRIT(I) = NCRIT(I) + 1
            ITMAX(I) = 1
          ENDIF
          !  -> minimum timestep
          IF ((DT(I) <= DTMIN).AND.(TIME > ZERO)) THEN 
            NCRIT(I)  = NCRIT(I) + 1
            IMINDT(I) = 1
          ENDIF 
          !  -> equivalent stress
          IF (EPSDOT_SM /= ZERO) THEN 
            LAMBDA    = EPSP(I)/EPSDOT_SM
            SIGMAX(I) = FINTER(KFUNC(ISM),LAMBDA,NPF,TF,DF) 
            SIGMAX(I) = SIGMAX(I)*SIGVM       
          ELSE
            SIGMAX(I) = SIGVM
          ENDIF
          IF (SVM(I) >= SIGMAX(I)*FACL(I)) THEN 
            NCRIT(I)   = NCRIT(I) + 1  
            ISIGMAX(I) = 1
          ENDIF
          !  -> Tuler-Butcher
          IF (S11(I) > SIGTH) THEN 
            UVAR(I,2) = UVAR(I,2) + TIMESTEP*(S11(I) - SIGTH)**2
            IF (UVAR(I,2) >= KF*FACL(I)) THEN 
              NCRIT(I)  = NCRIT(I) + 1 
              ISIGTH(I) = 1
            ENDIF
          ENDIF
          !  -> maximal principal strain
          IF (EPSDOT_PS /= ZERO) THEN 
            LAMBDA    = EPSP(I)/EPSDOT_PS    
            EPSMAX(I) = FINTER(KFUNC(IPS),LAMBDA,NPF,TF,DF)
            EPSMAX(I) = EPSMAX(I)*MAXEPS
          ELSE
            EPSMAX(I) = MAXEPS
          ENDIF
          IF (E11(I) >= EPSMAX(I)*FACL(I)) THEN 
            NCRIT(I)   = NCRIT(I) + 1       
            IEPSMAX(I) = 1
          ENDIF  
          !  -> maximum effective strain
          IF (EFF_STRAIN(I) >= EFFEPS*FACL(I)) THEN
            NCRIT(I)   = NCRIT(I) + 1
            IEFFEPS(I) = 1
          ENDIF
          !  -> maximum volumetric strain
          IF (VOLEPS > ZERO) THEN 
            IF (VOL_STRAIN(I) >= VOLEPS*FACL(I)) THEN 
              NCRIT(I)   = NCRIT(I) + 1
              IVOLEPS(I) = 1
            ENDIF
          ELSE
            IF (VOL_STRAIN(I) <= VOLEPS*FACL(I)) THEN 
              NCRIT(I)   = NCRIT(I) + 1
              IVOLEPS(I) = 1
            ENDIF
          ENDIF
          !  -> minimum principal strain
          IF (E33(I) <= MINEPS*FACL(I)) THEN
            NCRIT(I) = NCRIT(I) + 1
            IMINEPS(I) = 1
          ENDIF
          !  -> maximum tensorial shear strain
          IF ((E11(I) - E33(I))/TWO >= EPSSH*FACL(I)) THEN 
            NCRIT(I)  = NCRIT(I) + 1
            ISHEAR(I) = 1
          ENDIF 
          !  -> mixed mode 
          IF (fct_IDg12 > 0) THEN 
            LAMBDA  = UVAR(I,8)/El_ref
            SH12(I) = FINTER(KFUNC(IG12),LAMBDA,NPF,TF,DF) 
            DENOM   = SIGN(MAX(ABS(E11(I)),EM20),E11(I))  
            IF (((E22(I)/DENOM)<=-HALF).AND.((E22(I)/DENOM)>=-TWO)) THEN
              IF ((E11(I) - E22(I))/TWO >= SH12(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMIX12(I) = 1
              ENDIF
            ENDIF
          ENDIF
          IF (fct_IDg13 > 0) THEN 
            LAMBDA  = UVAR(I,8)/El_ref
            SH13(I) = FINTER(KFUNC(IG13),LAMBDA,NPF,TF,DF)
            DENOM   = SIGN(MAX(ABS(E11(I)),EM20),E11(I))  
            IF (((E22(I)/DENOM)<=ONE).AND.((E22(I)/DENOM)>=-HALF)) THEN
              IF ((E11(I) - E33(I))/TWO >= SH13(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMIX13(I) = 1
              ENDIF
            ENDIF
          ENDIF
          IF (fct_IDe1c > 0) THEN 
            LAMBDA = UVAR(I,8)/El_ref
            E1C(I) = FINTER(KFUNC(IE1C),LAMBDA,NPF,TF,DF) 
            DENOM  = SIGN(MAX(ABS(E11(I)),EM20),E11(I))  
            IF (((E22(I)/DENOM)<=ONE).AND.((E22(I)/DENOM)>=-HALF)) THEN
              IF (E11(I) >= E1C(I)) THEN 
                NCRIT(I)  = NCRIT(I) + 1
                IMXE1C(I) = 1
              ENDIF
            ENDIF
          ENDIF
          !  -> Forming limit diagram
          IF (tab_IDfld > 0) THEN 
            IF (Itab == 1) THEN 
              IF (E11(I) >= E1FLD(I)*FACL(I)) THEN 
                NCRIT(I) = NCRIT(I) + 1
                IFLD(I)  = 1
              ENDIF
            ELSE
              IF (E11(I) >= E1FLD(I)) THEN
                NCRIT(I) = NCRIT(I) + 1
                IFLD(I)  = 1
              ENDIF
            ENDIF
          ENDIF
          !  -> maximum shell thinning
          IF (EPSZZ(I) <= -ABS(THIN)*FACL(I)) THEN 
            NCRIT(I) = NCRIT(I) + 1
            ITHIN(I) = 1
          ENDIF
          !  -> maximum temperature
          IF (TEMP(I) >= MAXTEMP) THEN 
            NCRIT(I)    = NCRIT(I) + 1 
            IMAXTEMP(I) = 1
          ENDIF
c
          !  -> Checking failure
          DFMAX(I) = REAL(NCRIT(I)/NCS,KIND(DFMAX(I)))
          DFMAX(I) = MIN(DFMAX(I),ONE)
          IF (NCRIT(I) >= NCS) THEN 
            UVAR(I,5)   = UVAR(I,5) - ONE/NSTEP
            SIGNXX(I)   = SIGNXX(I)*UVAR(I,5)
            SIGNYY(I)   = SIGNYY(I)*UVAR(I,5)
            SIGNZZ(I)   = SIGNZZ(I)*UVAR(I,5)
            SIGNXY(I)   = SIGNXY(I)*UVAR(I,5)
            SIGNYZ(I)   = SIGNYZ(I)*UVAR(I,5)
            SIGNZX(I)   = SIGNZX(I)*UVAR(I,5)
            DFMAX(I)    = ONE
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
            DFMAX(I)      = ONE 
            OFF(I)        = ZERO
            IDEL7NOK      = 1   
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
                DFMAX(I)      = ONE
                NINDX3        = NINDX3 + 1
                INDX3(NINDX3) = I
                VFAIL(I)      = VDAM/VTOT
                IDEL7NOK      = 1   
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
