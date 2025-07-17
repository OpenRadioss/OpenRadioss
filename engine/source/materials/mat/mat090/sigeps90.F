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
      !||    sigeps90              ../engine/source/materials/mat/mat090/sigeps90.F
      !||--- called by ------------------------------------------------------
      !||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    valpvec_v             ../engine/source/materials/mat/mat033/sigeps33.F
      !||    valpvecdp_v           ../engine/source/materials/mat/mat033/sigeps33.F
      !||    vinter2               ../engine/source/tools/curve/vinter.F
      !||--- uses       -----------------------------------------------------
      !||    file_descriptor_mod   ../engine/source/modules/file_descriptor_mod.F90
      !||====================================================================
      SUBROUTINE SIGEPS90(
     1     NEL    ,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,UPARAM  ,RHO0    ,
     3     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     4     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     5     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     5     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     6     SOUNDSP,VISCMAX,UVAR    ,NVARTMP ,VARTMP  ,ISMSTR ,
     7     ISRATE ,ASRATE ,OFFG    ,IHET    ,ET      ,EPSD   ,
     8     OFF   , NGL     )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use file_descriptor_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
#include "mvsiz_p.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C
      INTEGER, INTENT(IN) :: NEL,NUVAR,ISMSTR,ISRATE,IHET
      INTEGER, INTENT(IN) :: NVARTMP,NGL(NEL)
       
      my_real, INTENT(IN) ,DIMENSION(NEL) ::  RHO0,EPSPXX,EPSPYY,EPSPZZ,
     .                                        EPSPXY,EPSPYZ,EPSPZX,
     .                                        EPSXX,EPSYY,EPSZZ,
     .                                        EPSXY ,EPSYZ,EPSZX ,
     .                                        OFFG,SIGOXX, SIGOYY,
     .                                        SIGOZZ, SIGOXY, SIGOYZ,SIGOZX
      my_real, INTENT(IN) ::  TIME,UPARAM(*),ASRATE
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(OUT)::
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ET(NEL),EPSD(NEL),OFF(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real, INTENT(INOUT) :: UVAR(NEL,NUVAR)
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP ! last interpolation positions in function tables
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(*), NFUNC, IFUNC(NFUNC)
      my_real, INTENT(IN) :: TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,II,KK,I1,J1,J2,IFLAG,IDAM,NE_L,NE_UNL,TFLAG,NINDX,
     .           NINDX_PRINT,N,FAIL
      INTEGER :: ILOAD(MVSIZ,3),INDX_L(MVSIZ),INDX_UNL(MVSIZ),INDX(MVSIZ),
     .           INDX_PRINT(NEL)
      INTEGER ,DIMENSION(NEL) :: IAD,ILEN,IPOS,JJ
      my_real
     . E0,AA,G,NU,SHAPE,HYS,
     . YFAC1,YFACJ1,YFACJ2,RATEJ1,RATEJ2, EPSE,EP1,
     . EP2,EP3,EP4,EP5,EP6,ERT11,ERT12,ERT13,ERT21,
     . ERT22,ERT23,ERT31,ERT32,ERT33,SJ1,SJ2,FAC,T1,T2,T3,
     . DAM,EPE,E_MIN(MVSIZ),DELTA,ALPHA,TCUT,DE,RATEEPS, 
     . DEPS,E_MAX,E_NEW,E_OLD,EPSS
      my_real :: DF(3),EPSP(3)
   
      my_real ,DIMENSION(NEL)       :: DYDX,STMP1,STMP2
      my_real, DIMENSION(NEL,3)     :: STRAIN,STRAINRATE,S,SQSTAT
      my_real, DIMENSION(NEL,2)     :: RATE,YFAC
      my_real, DIMENSION(MVSIZ)     :: QUASI_EINT,EMAX,EMIN,  
     .                                 ECURENT,E,DEINT,
     .  YLD(MVSIZ),EPST(MVSIZ)
      my_real, DIMENSION(MVSIZ,6)   :: AV
      my_real, DIMENSION(MVSIZ,6)   :: EVV,EV
      my_real, DIMENSION(MVSIZ,3,3) :: DIRPRV
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------    
        E0      =  UPARAM(1)
        G       =  UPARAM(4)
        NU      =  UPARAM(5)
        SHAPE   =  UPARAM(6)
        HYS     =  UPARAM(7)
        IFLAG   =  UPARAM(9)  
        IDAM    =  UPARAM(10)
        E_MAX   =  UPARAM(2*NFUNC + 11)
        ALPHA   =  UPARAM(2*NFUNC + 13)
        TFLAG   =  UPARAM(2*NFUNC + 14)
        FAIL    =  NINT(UPARAM(2*NFUNC + 15))
        TCUT    =  UPARAM(2*NFUNC + 16)
C       
        IF(TIME == ZERO )UVAR(1:NEL,8) = E0
C           
C-----------------------------------------------
C     
      ! Deletion of element
        NINDX = 0
        INDX(1:NEL) = 0
        NINDX_PRINT = 0
        INDX_PRINT(1:NEL) = 0
        DO I=1,NEL
           IF(OFF(I) == ZERO ) THEN
               SIGNXX(I) = ZERO
               SIGNYY(I) = ZERO
               SIGNZZ(I) = ZERO
               SIGNXY(I) = ZERO
               SIGNYZ(I) = ZERO  
               SIGNZX(I) = ZERO
               E_OLD = UVAR(I,8)
               AA = E_OLD*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU) 
               SOUNDSP(I) = SQRT(AA/RHO0(I))
           ELSEIF(OFF(I) < ONE ) THEN
             OFF(I) = OFF(I)*FOUR_OVER_5
             SIGNXX(I) = SIGOXX(I)*OFF(I)
             SIGNYY(I) = SIGOYY(I)*OFF(I)
             SIGNZZ(I) = SIGOZZ(I)*OFF(I)
             SIGNXY(I) = SIGOXY(I)*OFF(I)
             SIGNYZ(I) = SIGOYZ(I)*OFF(I)  
             SIGNZX(I) = SIGOZX(I)*OFF(I)
             IF(OFF(I) < EM01) THEN
                  OFF(I) = ZERO
                  SIGNXX(I) = ZERO
                  SIGNYY(I) = ZERO
                  SIGNZZ(I) = ZERO
                  SIGNXY(I) = ZERO
                  SIGNYZ(I) = ZERO  
                  SIGNZX(I) = ZERO
                  E_OLD = UVAR(I,8)
                  AA = E_OLD*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU) 
                  SOUNDSP(I) = SQRT(AA/RHO0(I))
                  ! for print_out
                  NINDX_PRINT = NINDX_PRINT + 1
                  INDX_PRINT(NINDX_PRINT) = I
              ENDIF
            ELSE    
                NINDX = NINDX + 1
                INDX(NINDX) = I
            ENDIF  
          ENDDO 
          !
          IF (NINDX_PRINT > 0) THEN     
             DO J=1,NINDX_PRINT
#include "lockon.inc"
                WRITE(IOUT, 1000) NGL(INDX_PRINT(J))
               WRITE(ISTDO,1100) NGL(INDX_PRINT(J)),TIME
#include "lockoff.inc"
             ENDDO
          ENDIF
          IF(NINDX == 0 ) RETURN  ! all groupe elemenet is deleted
          !
          ! Treatment of not deleted element.
#include "vectorize.inc"   !
        DO N=1,NINDX
            I = INDX(N)                      
            AV(N,1) = EPSXX(I)       
            AV(N,2) = EPSYY(I)       
            AV(N,3) = EPSZZ(I)       
            AV(N,4) = HALF*EPSXY(I)
            AV(N,5) = HALF*EPSYZ(I)
            AV(N,6) = HALF*EPSZX(I)
        ENDDO                     
C Eigenvalues needed to be calculated in double precision
C        for a simple precision executing*
        IF (IRESP==1) THEN
               CALL VALPVECDP_V(AV,EVV,DIRPRV,NINDX)
        ELSE
               CALL VALPVEC_V(AV,EVV,DIRPRV,NINDX)
        ENDIF
C-ISMSTR=0-NO SMALL STRAIN OPTION:STRAINS ARE LOGARITHMIC, STRESS IS CAUCHY
C-ISMSTR=1-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS CAUCHY
C-ISMSTR=2-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS BIOT
C-ISMSTR=3-NO SMALL STRAIN OPTION:STRESS IS BIOT
         IF(ISMSTR==0.OR.ISMSTR==2.OR.ISMSTR==4) THEN
#include "vectorize.inc" 
            DO N=1,NINDX
                I = INDX(N)
C ---- (STRAIN IS LOGARITHMIC)
                EV(N,1)=EXP(EVV(N,1))
                EV(N,2)=EXP(EVV(N,2))
                EV(N,3)=EXP(EVV(N,3))
            ENDDO 
         ELSEIF(ISMSTR==10.OR.ISMSTR==12) THEN
            DO N=1,NINDX
                I = INDX(N)
                IF(OFFG(I)<=ONE) THEN
                    EV(N,1)=SQRT(EVV(N,1) + ONE )
                    EV(N,2)=SQRT(EVV(N,2) + ONE )
                    EV(N,3)=SQRT(EVV(N,3) + ONE )
                ELSE
                    EV(N,1)=EVV(N,1)+ ONE
                    EV(N,2)=EVV(N,2)+ ONE
                    EV(N,3)=EVV(N,3)+ ONE
                END IF
            ENDDO 
         ELSE
C ----  STRAIN IS ENGINEERING)
#include "vectorize.inc" 
            DO N=1,NINDX
                I = INDX(N)
                EV(N,1)=EVV(N,1) + ONE
                EV(N,2)=EVV(N,2) + ONE
                EV(N,3)=EVV(N,3) + ONE 
            ENDDO 
         ENDIF
C engineering strain   and strain rate    
#include "vectorize.inc" 
        DO I=1,NINDX
           II  = INDX(I)
C engineering strain   e  = lambda-1 ,  according the input curve
C e=1-lambda (e > 0 compression and e < 0 traction)            
            STRAIN(I,1) = ONE - EV(I,1)         
            STRAIN(I,2) = ONE - EV(I,2)         
            STRAIN(I,3) = ONE - EV(I,3) 
        
            EPST(I) = SQRT(STRAIN(I,1)**2 + STRAIN(I,2)**2 + STRAIN(I,3)**2)       
C
            EP1 = EPSPXX(II)
            EP2 = EPSPYY(II)      
            EP3 = EPSPZZ(II) 
            EP4 = HALF*EPSPXY(II)        
            EP5 = HALF*EPSPYZ(II)
            EP6 = HALF*EPSPZX(II)
C phi_trans*L*phi_t    
            ERT11 =DIRPRV(I,1,1)*EP1 + DIRPRV(I,2,1)*EP4 + DIRPRV(I,3,1)*EP6
            ERT12 =DIRPRV(I,1,2)*EP1 + DIRPRV(I,2,2)*EP4 + DIRPRV(I,3,2)*EP6
            ERT13 =DIRPRV(I,1,3)*EP1 + DIRPRV(I,2,3)*EP4 + DIRPRV(I,3,3)*EP6
        
            ERT21 =DIRPRV(I,1,1)*EP4 + DIRPRV(I,2,1)*EP2 + DIRPRV(I,3,1)*EP5
            ERT22 =DIRPRV(I,1,2)*EP4 + DIRPRV(I,2,2)*EP2 + DIRPRV(I,3,2)*EP5
            ERT23 =DIRPRV(I,1,3)*EP4 + DIRPRV(I,2,3)*EP2 + DIRPRV(I,3,3)*EP5  
        
            ERT31 =DIRPRV(I,1,1)*EP6 + DIRPRV(I,2,1)*EP5 + DIRPRV(I,3,1)*EP3
            ERT32 =DIRPRV(I,1,2)*EP6 + DIRPRV(I,2,2)*EP5 + DIRPRV(I,3,2)*EP3
            ERT33 =DIRPRV(I,1,3)*EP6 + DIRPRV(I,2,3)*EP5 + DIRPRV(I,3,3)*EP3       
C
            EPSP(1) = DIRPRV(I,1,1)*ERT11 + DIRPRV(I,2,1)*ERT21 
     .                                    + DIRPRV(I,3,1)*ERT31 
            EPSP(2) = DIRPRV(I,1,2)*ERT12 + DIRPRV(I,2,2)*ERT22 
     .                                    + DIRPRV(I,3,2)*ERT32 
            EPSP(3) = DIRPRV(I,1,3)*ERT13 + DIRPRV(I,2,3)*ERT23 
     .                                    + DIRPRV(I,3,3)*ERT33
C    abs(eps) not necessary  
            STRAINRATE(I,1) = EPSP(1)*(ONE - STRAIN(I,1)) ! eng
            STRAINRATE(I,2) = EPSP(2)*(ONE - STRAIN(I,2))
            STRAINRATE(I,3) = EPSP(3)*(ONE - STRAIN(I,3))
        ENDDO
!        
        ! computing energy increase
        YFAC1 = UPARAM(NFUNC + 11)
        QUASI_EINT(1:NINDX)= ZERO
!        
        DO K=1,3
          KK = (K-1)*NFUNC + 1
          DO I=1,NINDX
             II = INDX(I)
             IPOS(I) = VARTMP(II,KK)
             IAD(I)  = NPF(IFUNC(1)) / 2 + 1
             ILEN(I) = NPF(IFUNC(1)+1) / 2 - IAD(I) - IPOS(I)
          ENDDO            
          CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,SQSTAT(1,K))
          !
          DO I=1,NINDX
             II = INDX(I)
             VARTMP(II,KK) = IPOS(I)
             SQSTAT(I,K) = YFAC1 * SQSTAT(I,K) 
          ENDDO   
          !  compute current energy
          DO I=1,NINDX
              IF (TFLAG == 2 .AND. STRAIN(I,K) < ZERO ) SQSTAT(I,K) = E0*STRAIN(I,K)
              QUASI_EINT(I) = QUASI_EINT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)
          ENDDO   
        ENDDO   
        ! 
        DO I=1,NINDX
          II = INDX(I) 
          DEINT(I)  = QUASI_EINT(I) - UVAR(II,9) ! 
          UVAR(II,9) = QUASI_EINT(I)
        ENDDO  
C -----------                   
C check loading and unloading.
        IF(NINDX > 0 ) THEN
          DO K=1,3
            DO I=1,NINDX
                EPE = EPSP(K)*STRAIN(I,K)
                ILOAD(I,K) = 1 
                IF(EPE > EM10 )ILOAD(I,K) = -1
            ENDDO
          ENDDO 
        ENDIF  
C       filtering strain rate
        DO I=1,NINDX
          II = INDX(I)
          RATEEPS = SQRT(STRAINRATE(I,1)**2 + STRAINRATE(I,2)**2 + STRAINRATE(I,3)**2)
          IF (ISRATE > 0) THEN
            RATEEPS =  ASRATE*RATEEPS + (ONE - ASRATE)*UVAR(II,3)
          ENDIF    
          EPSD(II) = RATEEPS
        ENDDO
C sous groupe  
        INDX_L(1:NINDX) = 0
        INDX_UNL(1:NINDX) = 0
        NE_L    = 0
        NE_UNL  = 0
C                
         DO I=1,NINDX
          II = INDX(I)
          DEPS = EPST(I) - UVAR(II,6)
          IF(DEINT(I) >= ZERO .OR. DEPS >= ZERO) THEN
              NE_L = NE_L + 1 
              INDX_L(NE_L) = I
              UVAR(II,3) = EPSD(II)
          ELSE
              EPSD(II) = MIN(EPSD(II), UVAR(II,3))
              NE_UNL = NE_UNL + 1 
              INDX_UNL(NE_UNL) = I
              UVAR(II,3) = EPSD(II)
          ENDIF
          STRAINRATE(I,1) = EPSD(II)
          STRAINRATE(I,2) = EPSD(II)
          STRAINRATE(I,3) = EPSD(II)
        ENDDO  
c
C case with unloading stress-strain curve
c
        IF (IFLAG == 1) THEN
          IF (NFUNC == 1) THEN
            YFAC1 = UPARAM(NFUNC + 11)
            DO I=1,NEL
              EMAX(I) = ZERO  
              ET(I)   = ZERO  
              EMIN(I) = EP20  
            ENDDO
!
            DO K=1,3
              KK = (K-1)*NFUNC + 1
              DO I=1,NINDX
                II = INDX(I)
                IPOS(I) = VARTMP(II,KK)
                IAD(I)  = NPF(IFUNC(1)) / 2 + 1
                ILEN(I) = NPF(IFUNC(1)+1) / 2 - IAD(I) - IPOS(I)
              ENDDO  
              CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,S(1,K))
              DO I=1,NINDX
                II = INDX(I)
                S(I,K) = YFAC1 * S(I,K)
                IF (TFLAG == 2 .AND. STRAIN(I,K) < ZERO ) S(I,K) = E0*STRAIN(I,K)
                EMAX(I) = MAX(EMAX(I), YFAC1*DYDX(I))
                EMIN(I) = MAX(EMIN(I), YFAC1*DYDX(I))
                VARTMP(II,KK) = IPOS(I)
              ENDDO
            ENDDO 
            DO I=1,NINDX
                II = INDX(I)
                E(I) = EMAX(I)
                ET(II) = EMIN(I)/E0
                YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)   
            ENDDO
          ELSE ! multiple functions (strain rate interpolation
            DO I=1,NINDX
              EMAX(I) = ZERO
              EMIN(I) = EP20
            ENDDO
!
            DO K=1,3  ! by direction
              JJ(1:NINDX) = 1 
              DO I=1,NINDX
                DO J = 2,NFUNC - 1       
                  IF (STRAINRATE(I,K) >= UPARAM(10 + J)) JJ(I) = J  
                ENDDO
              END DO
              DO I=1,NINDX
                RATE(I,1) = UPARAM(10 + JJ(I))
                RATE(I,2) = UPARAM(10 + JJ(I)+1)
                YFAC(I,1) = UPARAM(10 + NFUNC + JJ(I))
                YFAC(I,2) = UPARAM(10 + NFUNC + JJ(I)+1)
              END DO
              !
              DO I=1,NINDX
                II = INDX(I)
                J1 = JJ(I)
                KK = (K-1)*NFUNC + J1
                IPOS(I) = VARTMP(II,KK)
                IAD(I)  = NPF(IFUNC(J1)) / 2 + 1
                ILEN(I) = NPF(IFUNC(J1)+1) / 2 - IAD(I) - IPOS(I)
              END DO
              CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,STMP1)

              DO I=1,NINDX
                II = INDX(I)
                KK = (K-1)*NFUNC + JJ(I)        
                STMP1(I) = YFAC(I,1) * STMP1(I)
                EMAX(I) = MAX(EMAX(I), YFAC(I,1)*DYDX(I))
                EMIN(I) = MAX(EMIN(I), YFAC(I,1)*DYDX(I))  
                VARTMP(II,KK) = IPOS(I)       
              END DO
              
              DO I=1,NINDX
                II = INDX(I)
                J2 = JJ(I)+1
                KK = (K-1)*NFUNC + J2
                IPOS(I) = VARTMP(II,KK)
                IAD(I)  = NPF(IFUNC(J2)) / 2 + 1
                ILEN(I) = NPF(IFUNC(J2)+1) / 2 - IAD(I) - IPOS(I)
              END DO
              CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,STMP2)

              DO I=1,NINDX
                II = INDX(I)
                KK = (K-1)*NFUNC + JJ(I)+1    
                STMP2(I) = YFAC(I,2) * STMP2(I)
                EMAX(I) = MAX(EMAX(I), YFAC(I,2)*DYDX(I))
                EMIN(I) = MAX(EMIN(I), YFAC(I,2)*DYDX(I))
                VARTMP(II,KK) = IPOS(I)
              END DO
!
              DO I=1,NINDX
                FAC    = (STRAINRATE(I,K) - RATE(I,1)) / (RATE(I,2) - RATE(I,1))
                S(I,K) = STMP1(I) + FAC*(STMP2(I) - STMP1(I))
                IF (TFLAG == 2 .AND. STRAIN(I,K) < ZERO ) S(I,K) = E0*STRAIN(I,K) 
              END DO

            END DO  ! K=1,3
            DO I=1,NINDX
              II = INDX(I)
              E(I) = MAX(E0,EMAX(I))
              E(I) = EMAX(I)
              ET(II) = EMIN(I)/E0  ! Not used
              YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)
            ENDDO  
          ENDIF
        ENDIF 
c
C unloading with damage based on the energy.
c
      IF (IFLAG == 2) THEN
        IF (NFUNC == 1) THEN  ! quasi-static only => 1D interpolation
          YFAC1 = UPARAM(NFUNC + 11)
          DO I=1,NEL
            ECURENT (I)= ZERO  
            EMAX(I) = ZERO     
            EMIN(I) = EP20     
            ET(I) = ZERO       
          ENDDO
!
          DO K=1,3
            KK = (K-1)*NFUNC + 1
            DO I= 1,NINDX 
              II = INDX(I)
              IAD(I)  = NPF(IFUNC(1)) / 2 + 1
              IPOS(I) = VARTMP(II,KK)
              ILEN(I) = NPF(IFUNC(1)+1) / 2 - IAD(I) - IPOS(I)
            ENDDO  
        
            CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,SQSTAT(1,K))
                   
            DO I=1,NINDX
              II = INDX(I)
              SQSTAT(I,K) = YFAC1 * SQSTAT(I,K)
              IF (TFLAG == 2 .AND. STRAIN(I,K) < ZERO) SQSTAT(I,K) = E0*STRAIN(I,K)  
              S(I,K)  = SQSTAT(I,K)
              EMAX(I) = MAX(EMAX(I), YFAC1*DYDX(I))
              EMIN(I) = MIN(EMIN(I), YFAC1*DYDX(I))
C             compute current energy
              ECURENT(I) = ECURENT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)
              VARTMP(II,KK) = IPOS(I)
            END DO
          END DO
          DO I=1,NINDX
            II = INDX(I)
            ET(II) = EMIN(I)/E0 ! not used
            E(I) = MAX(E0,EMAX(I))
            E_MIN(I) =EMIN(I)
            YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)
          ENDDO  
c
        ELSEIF (NFUNC > 1) THEN ! strain rate dependent => 2D interpolation
C   
          YFAC1 = UPARAM(NFUNC + 11)
          DO I=1,NINDX
            ECURENT(I) = ZERO
            EMAX(I)    = ZERO
            EMIN(I)    = EP20
          END DO 
!
          DO K=1,3
            KK = (K-1)*NFUNC + 1    ! static
            DO I= 1,NINDX 
              II = INDX(I)
              IAD(I)  = NPF(IFUNC(1)) / 2 + 1
              IPOS(I) = VARTMP(II,KK)
              ILEN(I) = NPF(IFUNC(1)+1) / 2 - IAD(I) - IPOS(I)
            ENDDO  

            CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,STMP1)

            ! unloading + loading : 1:NE_UNL + NE_L
            DO I=1,NINDX
              II = INDX(I)
              SQSTAT(I,K) = YFAC1*STMP1(I)
              IF (TFLAG == 2 .AND. STRAIN(I,K) < ZERO ) SQSTAT(I,K) = E0*STRAIN(I,K)  
              S(I,K) = SQSTAT(I,K)
              EMAX(I) = MAX(EMAX(I), YFAC1*DYDX(I))
              EMIN(I) = MIN(EMIN(I), YFAC1*DYDX(I))
              !  compute current energy
              ECURENT(I) = ECURENT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)       
              VARTMP(II,KK) = IPOS(I)             
            END DO

            JJ(1:NINDX) = 1 
            DO I=1,NINDX
              DO J = 2,NFUNC - 1       
                IF (STRAINRATE(I,K) >= UPARAM(10 + J)) JJ(I) = J  
              ENDDO
            END DO
            DO I=1,NINDX
              RATE(I,1) = UPARAM(10 + JJ(I))
              RATE(I,2) = UPARAM(10 + JJ(I)+1)
              YFAC(I,1) = UPARAM(10 + NFUNC + JJ(I))
              YFAC(I,2) = UPARAM(10 + NFUNC + JJ(I)+1)
            END DO
        
            DO I=1,NINDX
              II = INDX(I)
              J1 = JJ(I)
              KK = (K-1)*NFUNC + J1
              IPOS(I) = VARTMP(II,KK)
              IAD(I)  = NPF(IFUNC(J1)) / 2 + 1
              ILEN(I) = NPF(IFUNC(J1)+1) / 2 - IAD(I) - IPOS(I)
            END DO
            CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,STMP1)
            DO N=1,NE_L
              I = INDX_L(N) 
              II = INDX(I)                                                    
              J1 = JJ(I)
              KK = (K-1)*NFUNC + J1
              VARTMP(II,KK) = IPOS(I)
              STMP1(I) = YFAC(I,1) * STMP1(I)
              EMAX(I) = MAX(EMAX(I), YFAC(I,1)*DYDX(I))
              EMIN(I) = MAX(EMIN(I), YFAC(I,1)*DYDX(I))            
            END DO
              
            DO I=1,NINDX
              II = INDX(I)
              J2 = JJ(I)+1
              KK = (K-1)*NFUNC + J2
              IPOS(I) = VARTMP(II,KK)
              IAD(I)  = NPF(IFUNC(J2)) / 2 + 1
              ILEN(I) = NPF(IFUNC(J2)+1) / 2 - IAD(I) - IPOS(I)
            END DO
            CALL VINTER2(TF,IAD,IPOS,ILEN,NINDX,STRAIN(1,K),DYDX,STMP2)
            DO I=1,NINDX
              II = INDX(I)
              J2 = JJ(I) + 1
              KK = (K-1)*NFUNC + J2
              VARTMP(I,KK) = IPOS(I)
              STMP2(I) = YFAC(I,2) * STMP2(I)
              EMAX(I) = MAX(EMAX(I), YFAC(I,2)*DYDX(I))
              EMIN(I) = MAX(EMIN(I), YFAC(I,2)*DYDX(I))
              VARTMP(II,KK) = IPOS(I)
            END DO

            DO II=1,NE_L
              I = INDX_L(II)                                                     
              FAC    = (STRAINRATE(I,K) - RATE(I,1)) / (RATE(I,2) - RATE(I,1))   
              S(I,K) = STMP1(I) + FAC*(STMP2(I) - STMP1(I))                      
              IF (TFLAG == 2 .AND. STRAIN(I,K) < ZERO ) S(I,K) = E0*STRAIN(I,K)   
            END DO                                                               

          ENDDO  ! K                              
!
          DO I=1,NINDX
            II = INDX(I)
            E(I) = MAX(E0,EMAX(I))                                         
            E(I) = EMAX(I)                                           
            ET(II)    = EMIN(I)/E0 ! not used                               
            YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2) 
          ENDDO   

        ENDIF ! NFUNC 
!-----------------------------------------------------------------------    
#include "vectorize.inc"           
        DO N=1,NINDX
          I = INDX(N)
          DELTA = EPST(N) - UVAR(I,6)
          UVAR(I,4)  = UVAR(I,4) + HALF*(YLD(N) + UVAR(I,1))*DELTA
          UVAR(I,4)  = MAX(ZERO, UVAR(I,4))
          UVAR(I,2)  = MAX(UVAR(I,2) , UVAR(I,4))
          UVAR(I,1)  = YLD(N) 
          UVAR(I,6)  = EPST(N)
          ECURENT(N) = UVAR(I,4)
        ENDDO
C 
C       idam is a hidden flag, only idam=0 is activated, Idam > 0 not tested.
        IF (IDAM == 0) THEN
#include "vectorize.inc" 
          DO N=1,NE_UNL                                       
            II = INDX_UNL(N) 
            I = INDX(II)                                
            IF (UVAR(I,2) > ZERO) THEN                        
              DAM = ONE - (ECURENT(II)/UVAR(I,2))**SHAPE    
              DAM = DAM**ALPHA                             
              DAM = ONE - (ONE - HYS)*DAM                  
              UVAR(I,7) = DAM                              
C             global                                       
              DO K=1,3                                     
                  S(II,K)= DAM*S(II,K)                       
              ENDDO                                        
            ENDIF                                            
          ENDDO ! NE_UNL                                       
c
        ELSE                                                     

C damage by direction to be tested for
#include "vectorize.inc"
          DO N=1,NE_UNL                                     
            II = INDX_UNL(N)  
            I = INDX(II)                               
            IF (UVAR(I,2) > ZERO) THEN                        
              DAM = ONE - (ECURENT(II)/UVAR(I,2))**SHAPE    
              DAM = ONE - (ONE - HYS)*DAM                  
              DO K=1,3                                     
                  IF(ILOAD(II,K) < 0)S(II,K) = DAM*S(II,K)    
              ENDDO                                        
            ENDIF                                            
          ENDDO ! nel                                        
        ENDIF ! IDAM   
      ENDIF ! iflag=2 
C                
C =====================================================
      IF( FAIL > 0) THEN
#include "vectorize.inc"       
        DO I = 1,NINDX
           II = INDX(I)
C S > 0 for compression - curve definition
C S < 0 for traction        
            T1 = -S(I,1)
            T2 = -S(I,2)
            T3 = -S(I,3)
            IF(T1 >= TCUT .OR. T2 >= TCUT .OR. T3 >= TCUT ) OFF(II) = FOUR_OVER_5
            T1 = T1/EV(I,2)/EV(I,3) 
            T2 = T2/EV(I,1)/EV(I,3) 
            T3 = T3/EV(I,1)/EV(I,2) 
C 
C cauchy to glabale
C
            SIGNXX(II) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*T1
     .                + DIRPRV(I,1,2)*DIRPRV(I,1,2)*T2
     .                + DIRPRV(I,1,3)*DIRPRV(I,1,3)*T3
     
            SIGNYY(II) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*T2
     .                + DIRPRV(I,2,3)*DIRPRV(I,2,3)*T3
     .                + DIRPRV(I,2,1)*DIRPRV(I,2,1)*T1
     
            SIGNZZ(II) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*T3        
     .                + DIRPRV(I,3,1)*DIRPRV(I,3,1)*T1
     .                + DIRPRV(I,3,2)*DIRPRV(I,3,2)*T2
            SIGNXY(II) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*T1
     .                + DIRPRV(I,1,2)*DIRPRV(I,2,2)*T2     
     .                + DIRPRV(I,1,3)*DIRPRV(I,2,3)*T3
     
            SIGNYZ(II) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*T2
     .                + DIRPRV(I,2,3)*DIRPRV(I,3,3)*T3
     .                + DIRPRV(I,2,1)*DIRPRV(I,3,1)*T1
     
            SIGNZX(II) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*T3
     .                + DIRPRV(I,3,1)*DIRPRV(I,1,1)*T1
     .                + DIRPRV(I,3,2)*DIRPRV(I,1,2)*T2  
            ! 
             ! computing E
            E_OLD = UVAR(II,8)
            EPSS = EPST(I) - YLD(I)/ E_OLD
            EPSS = MAX(ZERO, EPSS)
            EPSS = MIN(ONE, EPSS)
            AA = E_MAX - E0  
            DE = AA*(EPSS - UVAR(II,10))
            E_NEW = AA*EPSS + E0
            E_NEW= MIN(E_MAX, E_NEW)
            UVAR(II,10) = EPSS
            UVAR(II,8) = E_NEW
            AA = E_NEW*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU) 
            SOUNDSP(II) = SQRT(AA/RHO0(II))
            VISCMAX(II) = ZERO 
       ENDDO ! NINDX
      ELSEIF(FAIL == 0) THEN
#include "vectorize.inc"       
       DO I = 1,NINDX
           II = INDX(I)
C S > 0 for compression - curve definition
C S < 0 for traction
           T1 = -S(I,1)
           T2 = -S(I,2)
           T3 = -S(I,3)     
           T1 = MIN(TCUT, T1)
           T2 = MIN(TCUT, T2)
           T3 = MIN(TCUT, T3)
           T1 = T1/EV(I,2)/EV(I,3) 
           T2 = T2/EV(I,1)/EV(I,3) 
           T3 = T3/EV(I,1)/EV(I,2) 
C 
C cauchy to glabale
C
            SIGNXX(II) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*T1
     .                + DIRPRV(I,1,2)*DIRPRV(I,1,2)*T2
     .                + DIRPRV(I,1,3)*DIRPRV(I,1,3)*T3
     
            SIGNYY(II) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*T2
     .                + DIRPRV(I,2,3)*DIRPRV(I,2,3)*T3
     .                + DIRPRV(I,2,1)*DIRPRV(I,2,1)*T1
     
            SIGNZZ(II) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*T3        
     .                + DIRPRV(I,3,1)*DIRPRV(I,3,1)*T1
     .                + DIRPRV(I,3,2)*DIRPRV(I,3,2)*T2
            SIGNXY(II) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*T1
     .                + DIRPRV(I,1,2)*DIRPRV(I,2,2)*T2     
     .                + DIRPRV(I,1,3)*DIRPRV(I,2,3)*T3
     
            SIGNYZ(II) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*T2
     .                + DIRPRV(I,2,3)*DIRPRV(I,3,3)*T3
     .                + DIRPRV(I,2,1)*DIRPRV(I,3,1)*T1
     
            SIGNZX(II) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*T3
     .                + DIRPRV(I,3,1)*DIRPRV(I,1,1)*T1
     .                + DIRPRV(I,3,2)*DIRPRV(I,1,2)*T2  
             ! computing E
            E_OLD = UVAR(II,8)
            EPSS = EPST(I) - YLD(I)/ E_OLD
            EPSS = MAX(ZERO, EPSS)
            EPSS = MIN(ONE, EPSS)
            AA = E_MAX - E0  
            DE = AA*(EPSS - UVAR(II,10))
            E_NEW = AA*EPSS + E0
            E_NEW= MIN(E_MAX, E_NEW)
            UVAR(II,10) = EPSS
            UVAR(II,8) = E_NEW
            AA = E_NEW*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU) 
            SOUNDSP(II) = SQRT(AA/RHO0(II))
            VISCMAX(II) = ZERO 
       ENDDO ! NINDX
      ENDIF ! FAIL      
C================================================== 
        IF (IMPL_S > 0 .OR. IHET > 1) THEN
#include "vectorize.inc" 
          DO N=1,NINDX
            I = INDX(N)
            ET(I) = YLD(N)/MAX(EM20,EPST(N))
            ET(I) = MIN(ONE , ET(I)/E_MAX)
            IF(ET(I) == ZERO) ET(I) = ONE
         ENDDO
        ENDIF
c
 1000 FORMAT(1X,'TCUT REACHED, DELETED SOLID ELEMENT ',I10)
 1100 FORMAT(1X,'TCUT REACHED, DELETED SOLID ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
c  
c-----------        
C------------------------------------ 
        RETURN
        END SUBROUTINE SIGEPS90
C
