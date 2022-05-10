Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  FAIL_GENE1_C                  source/materials/fail/gene1/fail_gene1_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_GENE1_C (
     1     NEL      ,NUPARAM  ,NUVAR    ,MFUNC    ,KFUNC    ,LOFF     ,
     2     NPF      ,TF       ,TIME     ,TIMESTEP ,UPARAM   ,IPG      ,
     3     NGL      ,DT       ,EPSP     ,UVAR     ,OFF      ,NPG      ,
     4     EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,AREA     ,
     5     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,THKN     ,
     6     TEMP     ,DFMAX    ,TDELE    ,ALDT     ,TABLE    ,IRUPT    ,
     7     THK0     ,ELBUF_TAB,IPT      ,MPT      ,NOFF     ,FOFF     ,
     8     IGTYP    ,THKLYL   ,ILAY1    )
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
#include      "mvsiz_p.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C!-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IPG,
     .        NPG,IRUPT,IPT,MPT,NOFF(NEL),IGTYP,ILAY1
      my_real TIME,TIMESTEP,UPARAM(*),
     .   EPSXX(NEL),EPSYY(NEL),EPSXY(NEL),
     .   EPSYZ(NEL),EPSZX(NEL),SIGNXX(NEL),
     .   SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),
     .   SIGNZX(NEL),UVAR(NEL,NUVAR),DT(NEL),
     .   EPSP(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL),
     .   ALDT(NEL),TEMP(NEL),LOFF(NEL),AREA(NEL),
     .   THKN(NEL),THK0(NEL),THKLYL(NEL)
      TYPE (TTABLE), DIMENSION(NTABLE) :: TABLE
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
      INTEGER, DIMENSION(NEL), INTENT(INOUT) :: FOFF
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
      INTEGER I,K,J,INDX(MVSIZ),NINDX,IREG,NSTEP,tab_IDfld,Itab,NCS,
     .        ISM,IPS,IG12,IG13,IE1C,IFLD,fct_IDg12,fct_IDg13,fct_IDe1c,
     .        NCRIT,fct_IDel,IPOS(NEL,2),Ismooth,Istrain,IR,IS,IT,ILAY,
     .        CONDITION1(NEL),CONDITION2(NEL),CONDITION3(NEL),CONDITION4(NEL)
      my_real 
     .        MINPRES,MAXPRES,SIGP1,TMAX,DTMIN,EPSDOT_SM,SIGVM,SIGTH,
     .        KF,EPSDOT_PS,MAXEPS,EFFEPS,VOLEPS,MINEPS,EPSSH,EPSDOT_FLD,
     .        THIN,VOLFRAC,PTHK,MAXTEMP,Fscale_el,El_ref,LAMBDA,FAC,DF
      my_real 
     .        E12,P(NEL),SXX,SYY,SZZ,SVM(NEL),SH12,SH13,E1C,XVEC(NEL,2),
     .        Q,R,R_INTER,E11(NEL),E22(NEL),VOL_STRAIN(NEL),DAV,E1D,
     .        E2D,E3D,E4D,S11(NEL),S22(NEL),EFF_STRAIN(NEL),S1,S2,
     .        EPSMAX,SIGMAX,FACL(NEL),FACS(NEL),FACV(NEL),
     .        E1FLD(NEL),DFLD(NEL),HARDR(NEL),VTOT,VDAM,DENOM
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
      IF (ISIGI == 0 .and. TIME == ZERO) THEN
        IF (fct_IDel > 0) THEN 
          DO I=1,NEL   
            LAMBDA      = ALDT(I)/El_ref
            FAC         = FINTER(KFUNC(IREG),LAMBDA,NPF,TF,DF) 
            UVAR(I,1)   = FAC*Fscale_el
          ENDDO
        ELSE
          UVAR(1:NEL,1) = UN
        ENDIF  
        UVAR(1:NEL,6) = THK0(1:NEL)
        UVAR(1:NEL,7) = THKLYL(1:NEL)
      ENDIF
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
       ! Integration point failure
       IF (TIME == ZERO) UVAR(I,5) = UN
       IF (UVAR(I,5) < UN .AND. UVAR(I,5) >= EM08) THEN 
         UVAR(I,5) = UVAR(I,5) - UN/NSTEP
       ENDIF
       IF (UVAR(I,5) <= EM08) UVAR(I,5) = ZERO
       SIGNXX(I) = SIGNXX(I)*UVAR(I,5)
       SIGNYY(I) = SIGNYY(I)*UVAR(I,5)
       SIGNXY(I) = SIGNXY(I)*UVAR(I,5)
       SIGNYZ(I) = SIGNYZ(I)*UVAR(I,5)
       SIGNZX(I) = SIGNZX(I)*UVAR(I,5)
       ! Regularization factors for length, surface and volume
       FACL(I) = UVAR(I,1)
       FACS(I) = UVAR(I,1)**2
       FACV(I) = UVAR(I,1)**3
       ! Storage of integration point volume
       IF (NPG > 1) THEN
         UVAR(I,3) = THKLYL(I)*AREA(I)
         IF (UVAR(I,5) == ZERO) UVAR(I,4) = THKLYL(I)*AREA(I)
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
        !  -> computation of principal strains
        E12 = UNDEMI*EPSXY(I)
        S1  = UNDEMI*(EPSXX(I) + EPSYY(I))
        S2  = UNDEMI*(EPSXX(I) - EPSYY(I))
        Q   = SQRT(S2**2 + E12**2)
        E11(I) = S1 + Q
        E22(I) = S1 - Q
        IF (E22(I) >= E11(I)) THEN
          R_INTER = E22(I)
          E22(I)  = E11(I)
          E11(I)  = R_INTER
        ENDIF
        !  -> computation of volumetric strain
        VOL_STRAIN(I) = E11(I) + E22(I) + E11(I)*E22(I) 
        !  -> computation of effective strain
        DAV = (EPSXX(I)+EPSYY(I))*THIRD
        E1D = EPSXX(I) - DAV
        E2D = EPSYY(I) - DAV
        E3D = - DAV
        E4D = UNDEMI*EPSXY(I)
        EFF_STRAIN(I) = E1D**2 + E2D**2 + E3D**3 + DEUX*(E4D**2)
        EFF_STRAIN(I) = SQRT(DTIERS*EFF_STRAIN(I))
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
     .           - SIGNXX(I)*SIGNYY(I) + TROIS*SIGNXY(I)*SIGNXY(I))
        !  -> computing the principal stresses
        S1     = UNDEMI*(SIGNXX(I) + SIGNYY(I))
        S2     = UNDEMI*(SIGNXX(I) - SIGNYY(I))
        Q      = SQRT(S2**2 + SIGNXY(I)**2)
        S11(I) = S1 + Q
        S22(I) = S1 - Q 
        IF (S22(I) >= S11(I)) THEN
          R_INTER = S22(I)
          S22(I)  = S11(I)
          S11(I)  = R_INTER
        ENDIF
c       
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
            CALL TABLE2D_VINTERP_LOG(TABLE(tab_IDfld),Ismooth,NEL,IPOS,XVEC,E1FLD,DFLD,HARDR)
          ! Diagram using engineering strain
          ELSE
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = EXP(E22(1:NEL))-UN
            XVEC(1:NEL,2) = EPSP(1:NEL)/EPSDOT_FLD
            !   -> Tensile yield stress in direction 1 (MD)
            IPOS(1:NEL,1:2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(tab_IDfld),Ismooth,NEL,IPOS,XVEC,E1FLD,DFLD,HARDR)
            E1FLD = LOG(UN + E1FLD)
          ENDIF
        ELSE
          ! Diagram using true strains
          IF (Istrain == 0) THEN 
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = E22(1:NEL)
            XVEC(1:NEL,2) = ALDT(1:NEL)/El_ref
            !   -> Tensile yield stress in direction 1 (MD)
            IPOS(1:NEL,1:2) = 1
            CALL TABLE_VINTERP(TABLE(tab_IDfld),NEL,IPOS,XVEC,E1FLD,DFLD)
          ! Diagram using engineering strains
          ELSE
            ! In-plane tabulation with strain-rate
            XVEC(1:NEL,1) = EXP(E22(1:NEL))-UN
            XVEC(1:NEL,2) = ALDT(1:NEL)/El_ref
            !   -> Tensile yield stress in direction 1 (MD)
            IPOS(1:NEL,1:2) = 1
            CALL TABLE_VINTERP(TABLE(tab_IDfld),NEL,IPOS,XVEC,E1FLD,DFLD)
            E1FLD = LOG(UN + E1FLD)
          ENDIF
        ENDIF
      ENDIF 
c
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO CHECK THE EROSION CRITERIA
      !====================================================================    
      DO I = 1,NEL
        IF ((UVAR(I,5) == UN).AND.(OFF(I)==UN)) THEN
          NCRIT = 0
          !  -> maximum pressure
          IF (P(I) >= MAXPRES*FACS(I)) NCRIT = NCRIT + 1
          !  -> minimum pressure
          IF (P(I) <= MINPRES*FACS(I)) NCRIT = NCRIT + 1
          !  -> maximal principal stress
          IF (S11(I) >= SIGP1*FACS(I)) NCRIT = NCRIT + 1
          !  -> maximum time
          IF (TIME >= TMAX)  NCRIT = NCRIT + 1
          !  -> minimum timestep
          IF ((DT(I) <= DTMIN).AND.(TIME > ZERO)) NCRIT = NCRIT + 1
          !  -> equivalent stress
          IF (EPSDOT_SM /= ZERO) THEN 
            LAMBDA = EPSP(I)/EPSDOT_SM
            SIGMAX = FINTER(KFUNC(ISM),LAMBDA,NPF,TF,DF) 
            SIGMAX = SIGMAX*SIGVM
            IF (SVM(I) >= SIGMAX*FACS(I)) NCRIT = NCRIT + 1         
          ELSE
            IF (SVM(I) >= SIGVM*FACS(I))  NCRIT = NCRIT + 1
          ENDIF
          !  -> Tuler-Butcher
          IF (S11(I) > SIGTH*FACS(I)) THEN 
            UVAR(I,2) = UVAR(I,2) + TIMESTEP*(S11(I) - SIGTH*FACS(I))**2
            IF (UVAR(I,2) > KF) NCRIT = NCRIT + 1 
          ENDIF
          !  -> maximal principal strain
          IF (EPSDOT_PS /= ZERO) THEN 
            LAMBDA = EPSP(I)/EPSDOT_PS    
            EPSMAX = FINTER(KFUNC(IPS),LAMBDA,NPF,TF,DF)
            EPSMAX = EPSMAX*MAXEPS
            IF (E11(I) > EPSMAX*FACL(I)) NCRIT = NCRIT + 1         
          ELSE
            IF (E11(I) > MAXEPS*FACL(I)) NCRIT = NCRIT + 1
          ENDIF
          !  -> maximum effective strain
          IF (EFF_STRAIN(I) > EFFEPS*FACL(I)) NCRIT = NCRIT + 1
          !  -> maximum volumetric strain
          IF (VOL_STRAIN(I) > VOLEPS*FACV(I)) NCRIT = NCRIT + 1
          !  -> minimum principal strain
          IF (E22(I) < MINEPS*FACL(I)) NCRIT = NCRIT + 1
          !  -> maximum tensorial shear strain
          IF ((E11(I) - E22(I))/DEUX >= EPSSH*FACL(I)) NCRIT = NCRIT + 1
          !  -> mixed mode 
          IF (fct_IDg12 > 0) THEN 
            LAMBDA = ALDT(I)/El_ref
            SH12   = FINTER(KFUNC(IG12),LAMBDA,NPF,TF,DF) 
            DENOM  = SIGN(MAX(ABS(E22(I)),EM20),E22(I))   
            IF (((E11(I)/DENOM)<=-UNDEMI).AND.((E11(I)/DENOM)>=-DEUX)) THEN
              IF ((E11(I) - E22(I))/DEUX >= SH12) NCRIT = NCRIT + 1
            ENDIF
          ENDIF
          IF (fct_IDe1c > 0) THEN 
            LAMBDA = ALDT(I)/El_ref
            E1C    = FINTER(KFUNC(IE1C),LAMBDA,NPF,TF,DF) 
            DENOM  = SIGN(MAX(ABS(E22(I)),EM20),E22(I))
            IF (((E11(I)/DENOM)<=UN).AND.((E11(I)/DENOM)>=-UNDEMI)) THEN
              IF (E11(I) >= E1C) NCRIT = NCRIT + 1
            ENDIF
          ENDIF
          !  -> Forming limit diagram
          IF (tab_IDfld > 0) THEN 
            IF (Itab == 1) THEN 
              IF (E11(I) >= E1FLD(I)*FACL(I)) NCRIT = NCRIT + 1
            ELSE
              IF (E11(I) >= E1FLD(I)) NCRIT = NCRIT + 1
            ENDIF
          ENDIF
          !  -> maximum shell thinning
          IF (THIN < ZERO) THEN
            IF (((THKN(I)-UVAR(I,6))/UVAR(I,6)) < -ABS(THIN)) NCRIT = NCRIT + 1
          ELSE
            IF (((THKLYL(I)-UVAR(I,7))/UVAR(I,7)) < -ABS(THIN)) NCRIT = NCRIT + 1
          ENDIF
          !  -> maximum temperature
          IF (TEMP(I) > MAXTEMP) NCRIT = NCRIT + 1 
c
          !  -> Checking failure
          DFMAX(I) = REAL(NCRIT)/REAL(NCS)
          IF (NCRIT >= NCS) THEN 
            UVAR(I,5) = UVAR(I,5) - UN/NSTEP
          ENDIF
        ENDIF
c
      ENDDO
c
      !====================================================================
      ! - LOOKING FOR ELEMENT DELETION
      !====================================================================  
      ! Initialization of variable
      NINDX = 0
      INDX(1:NEL) = 0
      CONDITION1(1:NEL) = 0
      CONDITION2(1:NEL) = 0 
      CONDITION3(1:NEL) = 0
      CONDITION4(1:NEL) = 0
      ! Case of under-integrated shells
      IF ((ELBUF_TAB%NPTR == 1).AND.(ELBUF_TAB%NPTS == 1)) THEN       
        DO I = 1,NEL
          ! Checking element failure using global damage
          IF ((UVAR(I,5) == ZERO).AND.(OFF(I) /= ZERO)) THEN
            NOFF(I) = NOFF(I) + 1
            IF (PTHK < ZERO) FOFF(I) = 0
            IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 51 .OR. IGTYP == 52) FOFF(I) = 0
            NINDX = NINDX + 1
            INDX(NINDX) = I  
            IF (LOFF(I) == UN) THEN
              CONDITION1(NINDX) = IPT
              LOFF(I) = ZERO
            ENDIF
            DFMAX(I) = UN
            IF (IGTYP /= 51 .AND. IGTYP /= 52) THEN
              IF (NOFF(I) >= (ABS(PTHK)*REAL(MPT)) .AND. PTHK > ZERO) THEN
                TDELE(I)  = TIME
                OFF(I)    = ZERO
                IDEL7NOK  = 1
                CONDITION2(NINDX) = 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSE
        DO I = 1,NEL
          ! Checking element failure using global damage
          IF ((UVAR(I,5) == ZERO).AND.(OFF(I) /= ZERO)) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I  
            IF (LOFF(I) == UN) THEN
              CONDITION3(NINDX) = IPG
              CONDITION4(NINDX) = IPT
              LOFF(I) = ZERO
            ENDIF
            DFMAX(I) = UN
          ENDIF
          IF ((IPG == NPG).AND.(IPT == ELBUF_TAB%NPTT)
     .         .AND.(OFF(I) /= ZERO).AND.(ILAY1 == ELBUF_TAB%NLAY)) THEN      
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
                    VTOT = VTOT + FBUF%FLOC(IRUPT)%VAR(3+NEL*(I-1))
                    ! Compute damaged volume of the element
                    VDAM = VDAM + FBUF%FLOC(IRUPT)%VAR(4+NEL*(I-1))
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
            ! Checking volume fraction criterion
            IF (((VDAM/VTOT) >= VOLFRAC)) THEN
              TDELE(I) = TIME
              OFF(I)   = ZERO
              IDEL7NOK = 1
              CONDITION2(NINDX) = 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
c
c------------------------
c------------------------
      IF (NINDX > 0) THEN
        DO J = 1,NINDX
         I = INDX(J)
#include "lockon.inc"
         IF (CONDITION1(J) >= 1)  THEN         
           WRITE(IOUT, 2000) NGL(I),CONDITION1(J),TIME
           WRITE(ISTDO,2000) NGL(I),CONDITION1(J),TIME
         ENDIF  
         IF (CONDITION3(J) >= 1) THEN
           WRITE(ISTDO,2100) NGL(I),CONDITION3(J),CONDITION4(J),TIME
           WRITE(IOUT, 2100) NGL(I),CONDITION3(J),CONDITION4(J),TIME
         ENDIF      
         IF (CONDITION2(J) == 1) THEN
           WRITE(ISTDO,2200) NGL(I), TIME
           WRITE(IOUT, 2200) NGL(I), TIME
         ENDIF
#include "lockoff.inc"
        END DO
      END IF         
c------------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (GENE1)',I10,1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
 2100 FORMAT(1X,'FOR SHELL ELEMENT (GENE1)',I10,1X,'GAUSS POINT',I10,1X,
     .       'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
 2200 FORMAT(1X,' *** RUPTURE OF SHELL ELEMENT (GENE1)',I10,1X,
     . ' AT TIME :',1PE12.4)
      END
