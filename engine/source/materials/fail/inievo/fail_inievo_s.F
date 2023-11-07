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
Chd|  FAIL_INIEVO_S                 source/materials/fail/inievo/fail_inievo_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        JACOBIEW_V                    source/materials/mat_share/jacobview_v.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FCRIT                         source/materials/mat/mat081/sigeps81.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_INIEVO_S (
     1     NEL      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     2     NPF      ,TABLE    ,TF       ,TIME     ,UPARAM   , 
     3     NGL      ,ALDT     ,DPLA     ,EPSP     ,UVAR     ,     
     4     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,            
     5     PLA      ,TEMP     ,SIGY     ,OFF      ,DFMAX    ,
     6     TDELE    ,DMG_SCALE,UELR     ,IPG      ,NPG      ,
     7     LOFF     ,DAMINI   ,VOL      ,INLOC    )
C---------+---------+---+---+--------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: 
     .        NEL,NUPARAM,NUVAR,NGL(NEL),NPG,IPG,
     .        INLOC
      my_real, INTENT(IN) ::
     .        TIME,UPARAM(NUPARAM),ALDT(NEL),
     .        DPLA(NEL),EPSP(NEL),TEMP(NEL),
     .        SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .        SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .        PLA(NEL),SIGY(NEL),VOL(NEL)
      my_real, INTENT(INOUT) :: 
     .        UVAR(NEL,NUVAR),DFMAX(NEL),TDELE(NEL),
     .        DMG_SCALE(NEL),OFF(NEL),UELR(NEL),
     .        LOFF(NEL),DAMINI(NEL)
      TYPE (TTABLE), INTENT(IN), DIMENSION(NTABLE) :: TABLE 
C!-----------------------------------------------
C!   VARIABLES FOR FUNCTION INTERPOLATION 
C!-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(SNPC),NFUNC,IFUNC(NFUNC)
      my_real, INTENT(IN) :: TF(STF)
      my_real
     .         FINTER
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,INDX(NEL),NINDX,FAILIP,IPOS(NEL,2),
     .        NROT(NEL),NINIEVO,ILEN
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .        INITYPE,EVOTYPE,EVOSHAP,COMPTYP,TAB_ID,
     .        TAB_EL,FCRIT 
      my_real, DIMENSION(:), ALLOCATABLE :: 
     .        SR_REF,FSCALE,INI_P1,EL_REF,ELSCAL,
     .        DISP,ENER,ALPHA2
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     .        DMGINI,DMGEVO
      my_real
     .        LAMBDA,FAC,DF,L0(NEL)     ,TRIAX(NEL)  ,EPSF(NEL)   ,
     .        DEPSF(NEL)  ,XVEC(NEL,2)  ,SXX,SYY,SZZ ,SIZEFAC(NEL),
     .        EPSMOD(NEL) ,P(NEL)       ,SVM(NEL)    ,DMGMAX(NEL) ,
     .        DMGMUL(NEL) ,SIGTENS(NEL,3,3)          ,SIG_PR(NEL,3),
     .        VECT_PR(NEL,3,3),PLAS_DISP,R_INTER     ,YLD0        ,
     .        MAXSHEAR(NEL),SIGPMAJ(NEL),ALPHA(NEL)  ,DSIZE(NEL)  ,
     .        CENTER      ,DEVSP1       ,DEVSP2      ,RADIUS      ,
     .        SIGP1       ,SIGP2
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      NINIEVO = UPARAM(1)
      ILEN    = INT(UPARAM(3))
      IF (INLOC > 0) ILEN = 1
      FAILIP  = MIN(NINT(UPARAM(4)),NPG)
      ALLOCATE(INITYPE(NINIEVO))
      ALLOCATE(EVOTYPE(NINIEVO))
      ALLOCATE(EVOSHAP(NINIEVO))
      ALLOCATE(COMPTYP(NINIEVO))
      ALLOCATE(TAB_ID (NINIEVO))
      ALLOCATE(SR_REF (NINIEVO))
      ALLOCATE(FSCALE (NINIEVO))
      ALLOCATE(INI_P1 (NINIEVO))
      ALLOCATE(TAB_EL (NINIEVO))
      ALLOCATE(EL_REF (NINIEVO))
      ALLOCATE(ELSCAL (NINIEVO))
      ALLOCATE(DISP   (NINIEVO))
      ALLOCATE(ENER   (NINIEVO))
      ALLOCATE(ALPHA2 (NINIEVO))     
      DO J = 1,NINIEVO
        INITYPE(J) = UPARAM(6  + 14*(J-1))
        EVOTYPE(J) = UPARAM(7  + 14*(J-1))
        EVOSHAP(J) = UPARAM(8  + 14*(J-1))
        COMPTYP(J) = UPARAM(9  + 14*(J-1))
        TAB_ID(J)  = NINT(UPARAM(10 + 14*(J-1)))
        SR_REF(J)  = UPARAM(11 + 14*(J-1))
        FSCALE(J)  = UPARAM(12 + 14*(J-1))
        INI_P1(J)  = UPARAM(13 + 14*(J-1))
        TAB_EL(J)  = NINT(UPARAM(14 + 14*(J-1)))
        EL_REF(J)  = UPARAM(15 + 14*(J-1))
        ELSCAL(J)  = UPARAM(16 + 14*(J-1)) 
        DISP(J)    = UPARAM(17 + 14*(J-1))
        ALPHA2(J)  = UPARAM(18 + 14*(J-1))
        ENER(J)    = UPARAM(19 + 14*(J-1))
      ENDDO 
c
      ! Element characteristic length computation 
      !  -> Initial values 
      IF (UVAR(1,1) == ZERO) THEN 
        ! -> Critical timestep formulation
        IF (ILEN == 1) THEN 
          UVAR(1:NEL,1) = ALDT(1:NEL)
        ! -> Geometric formulation
        ELSE
          DO I = 1,NEL
            UVAR(I,1) = EXP(THIRD*LOG(VOL(I)))
          ENDDO
        ENDIF
      ENDIF
      L0(1:NEL) = UVAR(1:NEL,1)
      ! Positive stress triaxiality bounded plastic strain
      EPSMOD(1:NEL) = UVAR(1:NEL,2)
      ! Damage initiation and evolution variable
      ALLOCATE(DMGINI(NEL,NINIEVO))
      ALLOCATE(DMGEVO(NEL,NINIEVO))
      ALLOCATE(FCRIT(NEL))
      DO J = 1,NINIEVO
        DO I=1,NEL
          ! Initiation damage
          DMGINI(I,J) = UVAR(I,3+(J-1)*3)
          ! Evolution damage
          DMGEVO(I,J) = UVAR(I,4+(J-1)*3)
        ENDDO
      ENDDO
      ! Criterion number leading to element deletion
      FCRIT(1:NEL) = 0
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE STRESS STATE QUANTITIES
      !====================================================================       
      DO I=1,NEL
c
        ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
        P(I)   = -THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
        SXX    = SIGNXX(I) + P(I) 
        SYY    = SIGNYY(I) + P(I) 
        SZZ    = SIGNZZ(I) + P(I) 
        SVM(I) = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .        + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
        SVM(I) = SQRT(THREE*SVM(I))
        TRIAX(I) = -P(I)/MAX(EM20,SVM(I))
        IF (TRIAX(I) < -ONE) TRIAX(I) = -ONE
        IF (TRIAX(I) >  ONE) TRIAX(I) = ONE
c
        ! Increase the modified plastic strain
        IF (TRIAX(I) > ZERO) EPSMOD(I) = EPSMOD(I) + DPLA(I)
c
        ! Stress tensor
        SIGTENS(I,1,1) = SIGNXX(I)
        SIGTENS(I,2,2) = SIGNYY(I)
        SIGTENS(I,3,3) = SIGNZZ(I)
        SIGTENS(I,1,2) = SIGNXY(I)
        SIGTENS(I,2,3) = SIGNYZ(I)
        SIGTENS(I,3,1) = SIGNZX(I)
        SIGTENS(I,2,1) = SIGNXY(I)
        SIGTENS(I,3,2) = SIGNYZ(I)
        SIGTENS(I,1,3) = SIGNZX(I)  
c
      ENDDO
c
      ! Compute principal stresses
      CALL JACOBIEW_V(NEL,3,SIGTENS,SIG_PR,VECT_PR,NROT)
c
      ! Sort principal stresses and compute variables
      DO I = 1,NEL
c
        IF (SIG_PR(I,1) < SIG_PR(I,2)) THEN 
          R_INTER     = SIG_PR(I,1)
          SIG_PR(I,1) = SIG_PR(I,2)
          SIG_PR(I,2) = R_INTER
        ENDIF 
        IF (SIG_PR(I,2) < SIG_PR(I,3)) THEN
          R_INTER     = SIG_PR(I,2)
          SIG_PR(I,2) = SIG_PR(I,3)
          SIG_PR(I,3) = R_INTER
        ENDIF
        IF (SIG_PR(I,1) < SIG_PR(I,2)) THEN
          R_INTER     = SIG_PR(I,1)
          SIG_PR(I,1) = SIG_PR(I,2)
          SIG_PR(I,2) = R_INTER
        ENDIF
        SIGPMAJ(I)  = SIG_PR(I,1)
        MAXSHEAR(I) = (SIG_PR(I,1)-SIG_PR(I,3))*HALF
c
        ! Compute the alpha parameter for FLD/MSFLD
        CENTER = HALF*(SIGNXX(I)+SIGNYY(I))
        RADIUS = SQRT((HALF*(SIGNXX(I)-SIGNYY(I)))**2 + SIGNXY(I)**2)
        SIGP1  = CENTER + RADIUS
        SIGP2  = CENTER - RADIUS
        DEVSP1 = SIGP1  - THIRD*(SIGP1+SIGP2)
        DEVSP2 = SIGP2  - THIRD*(SIGP1+SIGP2)    
        ALPHA(I) = DEVSP2/SIGN(MAX(ABS(DEVSP1),EM20),DEVSP1)
c
      ENDDO
c    
      !====================================================================
      ! - COMPUTE DAMAGE INITIATION AND EVOLUTION
      !====================================================================
      DO J = 1,NINIEVO
        ! Damage initiation type selection
        SELECT CASE(INITYPE(J))
          ! Plastic strain vs triaxiality
          CASE(1)
            XVEC(1:NEL,1) = TRIAX(1:NEL)
          ! Plastic strain vs shear influence (theta)
          CASE(2)
            DO I = 1,NEL
              XVEC(I,1) = (SVM(I) + INI_P1(J)*P(I))/MAX(MAXSHEAR(I),EM08)
            ENDDO
          ! MSFLD / FLD
          CASE(3,4)
            XVEC(1:NEL,1) = ALPHA(1:NEL)
          ! Normalized principal stress
          CASE(5)
            DO I = 1,NEL
              XVEC(I,1) = (SVM(I) + INI_P1(J)*P(I))/MAX(SIGPMAJ(I),EM08)
            ENDDO
        END SELECT
        XVEC(1:NEL,2)   = EPSP(1:NEL)/SR_REF(J)
        IPOS(1:NEL,1:2) = 1
        CALL TABLE_VINTERP(TABLE(TAB_ID(J)),NEL,IPOS,XVEC,EPSF,DEPSF)
        EPSF(1:NEL) = EPSF(1:NEL)*FSCALE(J)
c
        ! Compute the element size regularization factor 
        IF (TAB_EL(J) > 0) THEN 
          XVEC(1:NEL,1) = L0(1:NEL)/EL_REF(J)
          SELECT CASE (INITYPE(J))
            CASE(1)
              XVEC(1:NEL,2) = TRIAX(1:NEL)
            CASE(2)
              DO I = 1,NEL
                XVEC(I,2) = (SVM(I) + INI_P1(J)*P(I))/MAX(MAXSHEAR(I),EM08)
              ENDDO
            CASE(3,4)
              XVEC(1:NEL,2) = ALPHA(1:NEL)
            CASE(5)
              DO I = 1,NEL
                XVEC(I,2) = (SVM(I) + INI_P1(J)*P(I))/MAX(SIGPMAJ(I),EM08)
              ENDDO            
          END SELECT
          IPOS(1:NEL,1:2) = 1
          CALL TABLE_VINTERP(TABLE(TAB_EL(J)),NEL,IPOS,XVEC,SIZEFAC,DSIZE)
          SIZEFAC(1:NEL) = SIZEFAC(1:NEL)*ELSCAL(J)
          EPSF(1:NEL) = EPSF(1:NEL)*SIZEFAC(1:NEL)
        ENDIF
c      
        ! Update damage initiation
        SELECT CASE (INITYPE(J))
          CASE(1,2,5) 
            DO I = 1,NEL
              IF ((DPLA(I) > ZERO).AND.(DMGINI(I,J)<ONE).AND.(LOFF(I) == ONE)) THEN 
                DMGINI(I,J) = DMGINI(I,J) + DPLA(I)/MAX(EPSF(I),EM20)
                DMGINI(I,J) = MIN(DMGINI(I,J),ONE)
              ENDIF
            ENDDO
          CASE(3)
            IF (NINT(INI_P1(J))>0) THEN  
              DO I = 1,NEL
                IF (((EPSMOD(I)-UVAR(I,2)) > ZERO).AND.(DMGINI(I,J)<ONE).AND.(LOFF(I) == ONE)) THEN 
                  DMGINI(I,J) = DMGINI(I,J) + (EPSMOD(I)-UVAR(I,2))/MAX(EPSF(I),EM20)
                  DMGINI(I,J) = MIN(DMGINI(I,J),ONE)
                ENDIF
              ENDDO
            ELSE
              DO I = 1,NEL
                IF (((EPSMOD(I)-UVAR(I,2)) > ZERO).AND.(DMGINI(I,J)<ONE).AND.(LOFF(I) == ONE)) THEN 
                  DMGINI(I,J) = MAX(DMGINI(I,J),EPSMOD(I)/MAX(EPSF(I),EM20))
                  DMGINI(I,J) = MIN(DMGINI(I,J),ONE)
                ENDIF
              ENDDO
            ENDIF
          CASE(4)
            IF (NINT(INI_P1(J))>0) THEN  
              DO I = 1,NEL
                IF ((DPLA(I) > ZERO).AND.(DMGINI(I,J)<ONE).AND.(LOFF(I) == ONE)) THEN 
                  DMGINI(I,J) = DMGINI(I,J) + DPLA(I)/MAX(EPSF(I),EM20)
                  DMGINI(I,J) = MIN(DMGINI(I,J),ONE)
                ENDIF
              ENDDO
            ELSE
              DO I = 1,NEL
                IF ((DPLA(I) > ZERO).AND.(DMGINI(I,J)<ONE).AND.(LOFF(I) == ONE)) THEN 
                  DMGINI(I,J) = MAX(DMGINI(I,J),PLA(I)/MAX(EPSF(I),EM20))
                  DMGINI(I,J) = MIN(DMGINI(I,J),ONE)
                ENDIF
              ENDDO
            ENDIF
        END SELECT
c
        ! Update damage evolution
        SELECT CASE (EVOTYPE(J))
          ! Plastic displacement at failure
          CASE(1) 
            SELECT CASE (EVOSHAP(J))
              ! Linear shape
              CASE(1)
                DO I = 1,NEL
                  IF ((DMGINI(I,J) >= ONE).AND.(DPLA(I)>ZERO).AND.
     .               (LOFF(I) == ONE).AND.(DMGEVO(I,J)<ONE)) THEN   
                    DMGEVO(I,J) = DMGEVO(I,J) + L0(I)*DPLA(I)/DISP(J)
                    DMGEVO(I,J) = MIN(ONE,DMGEVO(I,J))
                    IF (DMGEVO(I,J) >= ONE) FCRIT(I) = J
                  ENDIF
                ENDDO
              ! Exponential shape
              CASE(2)
                DO I = 1,NEL
                  IF ((DMGINI(I,J) >= ONE).AND.(DPLA(I)>ZERO).AND.
     .               (LOFF(I) == ONE).AND.(DMGEVO(I,J)<ONE)) THEN  
                    IF (DMGEVO(I,J) == ZERO) UVAR(I,5+(J-1)*3) = PLA(I)
                    PLAS_DISP = (PLA(I) - UVAR(I,5+(J-1)*3))*L0(I)/DISP(J) 
                    DMGEVO(I,J) = DMGEVO(I,J) + (ALPHA2(J)/(ONE - EXP(-ALPHA2(J))))*
     .                                           EXP(-ALPHA2(J)*PLAS_DISP)*
     .                                           DPLA(I)*L0(I)/DISP(J)
                    IF (DMGEVO(I,J) > 0.999D0) DMGEVO(I,J) = ONE
                    DMGEVO(I,J) = MIN(ONE,DMGEVO(I,J))
                    IF (DMGEVO(I,J) >= ONE) FCRIT(I) = J
                  ENDIF
                ENDDO
            END SELECT
          ! Fracture energy failure
          CASE(2)
            SELECT CASE (EVOSHAP(J))
              ! Linear shape
              CASE(1)
                DO I = 1,NEL
                  IF ((DMGINI(I,J) >= ONE).AND.(DPLA(I)>ZERO).AND.
     .               (LOFF(I) == ONE).AND.(DMGEVO(I,J)<ONE)) THEN  
                    IF (DMGEVO(I,J) == ZERO) UVAR(I,5+(J-1)*3) = SIGY(I)
                    YLD0 = UVAR(I,5+(J-1)*3)
                    DMGEVO(I,J) = DMGEVO(I,J) + DPLA(I)*L0(I)*YLD0/(TWO*ENER(J))
                    DMGEVO(I,J) = MIN(ONE,DMGEVO(I,J))
                    IF (DMGEVO(I,J) >= ONE) FCRIT(I) = J
                  ENDIF
                ENDDO
              ! Exponential shape
              CASE(2)
                DO I = 1,NEL
                  IF ((DMGINI(I,J) >= ONE).AND.(DPLA(I)>ZERO).AND.
     .               (LOFF(I) == ONE).AND.(DMGEVO(I,J)<ONE)) THEN  
                    UVAR(I,5+(J-1)*3) = UVAR(I,5+(J-1)*3) + SIGY(I)*L0(I)*DPLA(I)
                    DMGEVO(I,J) = ONE - EXP(-(UVAR(I,5+(J-1)*3))/ENER(J))
                    IF (DMGEVO(I,J) > 0.999D0) DMGEVO(I,J) = ONE
                    DMGEVO(I,J) = MIN(ONE,DMGEVO(I,J))
                    IF (DMGEVO(I,J) >= ONE) FCRIT(I) = J
                  ENDIF
                ENDDO
              END SELECT
          ! Failure criterion approach    
          CASE DEFAULT 
            DO I = 1,NEL
              IF ((DMGINI(I,J) >= ONE).AND.(DPLA(I)>ZERO).AND.
     .           (LOFF(I) == ONE).AND.(DMGEVO(I,J)<ONE)) THEN
                DMGEVO(I,J) = DMGINI(I,J)
                DMGEVO(I,J) = MIN(ONE,DMGEVO(I,J))
                IF (DMGEVO(I,J) >= ONE) FCRIT(I) = J
              ENDIF
            ENDDO
        END SELECT
      ENDDO
c
      !====================================================================
      ! - COMPUTE GLOBAL DAMAGE VARIABLE AND DAMAGE SCALING
      !====================================================================
      DFMAX(1:NEL)  = ZERO
      DMGMAX(1:NEL) = ZERO
      DMGMUL(1:NEL) = ONE
      DO J = 1,NINIEVO
        SELECT CASE (COMPTYP(J))
          ! Maximum damage
          CASE(1)
            DO I = 1,NEL 
              DMGMAX(I) = MAX(DMGMAX(I),DMGEVO(I,J)) 
            ENDDO
          ! Multiplicative damage
          CASE(2)
            DO I = 1,NEL 
              DMGMUL(I) = DMGMUL(I)*(ONE-DMGEVO(I,J))
            ENDDO
        END SELECT
      ENDDO
      DMGMUL(1:NEL) = ONE - DMGMUL(1:NEL)
      NINDX = 0
      INDX(1:NEL) = 0
      DO I = 1,NEL
        IF ((LOFF(I) == ONE).AND.(OFF(I) > ZERO)) THEN 
          DFMAX(I) = MAX(DMGMAX(I),DMGMUL(I))
          IF (DFMAX(I) >= ONE) THEN 
            NINDX       = NINDX + 1
            INDX(NINDX) = I
            UELR(I)     = UELR(I) + ONE
            LOFF(I)     = ZERO
            IF (NINT(UELR(I)) >= FAILIP) THEN 
              OFF(I)    = ZERO
              IDEL7NOK  = 1   
              TDELE(I)  = TIME
            ENDIF
          ENDIF
        ENDIF
      ENDDO
c
      !====================================================================
      ! - UPDATE THE DAMAGE SCALING FACTOR
      !====================================================================
      DMG_SCALE(1:NEL) = ONE - DFMAX(1:NEL)
c
      !====================================================================
      ! - UPDATE THE USER VARIABLE
      !====================================================================
      ! Positive stress triaxiality bounded plastic strain
      UVAR(1:NEL,2) = EPSMOD(1:NEL)
      DAMINI(1:NEL) = ZERO
      DO J = 1,NINIEVO
        ! Checking element failure and recovering user variable
        DO I=1,NEL
          ! Damage initiation output
          DAMINI(I) = MAX(DMGINI(I,J),DAMINI(I))
          ! Initiation damage
          UVAR(I,3+(J-1)*3) = DMGINI(I,J)
          ! Evolution damage
          UVAR(I,4+(J-1)*3) = DMGEVO(I,J)
        ENDDO
      ENDDO
c
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED ELEMENTS
      !====================================================================
      IF (NINDX > 0) THEN 
        DO J=1,NINDX
          I = INDX(J)     
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),FCRIT(I),IPG,TIME
          WRITE(ISTDO,1000) NGL(I),FCRIT(I),IPG,TIME
          IF (OFF(I) == ZERO) THEN 
            WRITE(IOUT, 2000) NGL(I),TIME
            WRITE(ISTDO,2000) NGL(I),TIME
          ENDIF
#include "lockoff.inc"
        END DO
      END IF    
c
      !====================================================================
      ! - TABLES DEALLOCATION
      !====================================================================
      IF (ALLOCATED(INITYPE)) DEALLOCATE(INITYPE)
      IF (ALLOCATED(EVOTYPE)) DEALLOCATE(EVOTYPE)
      IF (ALLOCATED(EVOSHAP)) DEALLOCATE(EVOSHAP)
      IF (ALLOCATED(COMPTYP)) DEALLOCATE(COMPTYP)
      IF (ALLOCATED(TAB_ID))  DEALLOCATE(TAB_ID)
      IF (ALLOCATED(SR_REF))  DEALLOCATE(SR_REF)
      IF (ALLOCATED(FSCALE))  DEALLOCATE(FSCALE)
      IF (ALLOCATED(INI_P1))  DEALLOCATE(INI_P1)
      IF (ALLOCATED(TAB_EL))  DEALLOCATE(TAB_EL)
      IF (ALLOCATED(EL_REF))  DEALLOCATE(EL_REF)
      IF (ALLOCATED(ELSCAL))  DEALLOCATE(ELSCAL)
      IF (ALLOCATED(DISP))    DEALLOCATE(DISP)
      IF (ALLOCATED(ENER))    DEALLOCATE(ENER)
      IF (ALLOCATED(ALPHA2))  DEALLOCATE(ALPHA2)
      IF (ALLOCATED(DMGINI))  DEALLOCATE(DMGINI)
      IF (ALLOCATED(DMGEVO))  DEALLOCATE(DMGEVO)
      IF (ALLOCATED(FCRIT))   DEALLOCATE(FCRIT)                      
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FOR SOLID ELEMENT NUMBER ',I10,
     .          ' FAILURE (INIEVO) WITH CRITERION NUMBER ',I3,
     .          ' AT GAUSS POINT ',I5,' AT TIME :',1PE12.4)     
 2000 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT :',I10,
     .          ' AT TIME :',1PE12.4)     
c
      END
