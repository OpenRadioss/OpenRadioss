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
Chd|  SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        TABLE_MAT_VINTERP             source/materials/tools/table_mat_vinterp.F
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        TABLE_MAT_VINTERP_MOD         source/materials/tools/table_mat_vinterp.F
Chd|====================================================================
      SUBROUTINE SIGEPS70(
     1   NEL,     NUPARAM, NUVAR,   MFUNC,
     2   KFUNC,   NPF,     TF,      TIME,
     3   TIMESTEP,UPARAM,  RHO0,    RHO,
     4   OFFG,    RHOREF,  NVARTMP, VARTMP,
     5   RHOSP,   DEPSXX,
     7   DEPSYY,  DEPSZZ,  DEPSXY,  DEPSYZ,
     8   DEPSZX,  EPSXX,   EPSYY,   EPSZZ,
     9   EPSXY,   EPSYZ,   EPSZX,   SIG0XX,
     A   SIG0YY,  SIG0ZZ,  SIG0XY,  SIG0YZ,
     B   SIG0ZX,  SIGNXX,  SIGNYY,  SIGNZZ,
     C   SIGNXY,  SIGNYZ,  SIGNZX,  
     E   SOUNDSP, VISCMAX, UVAR,
     F   OFF,     NGL,     IPM,
     G   MAT,     EPSP,    ET,      ISMSTR,
     H   IHET,    JSMS,    MATPARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE TABLE_MAT_VINTERP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
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
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIG0XX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIG0YY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NVARTMP
      INTEGER ,INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
      INTEGER, INTENT(IN) :: ISMSTR, IHET, JSMS
      INTEGER NEL, NUPARAM, NUVAR,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIG0XX(NEL),SIG0YY(NEL),SIG0ZZ(NEL),
     .   SIG0XY(NEL),SIG0YZ(NEL),SIG0ZX(NEL),
     .   EPSP(NEL)
      TYPE(MATPARAM_STRUCT_) ,INTENT(IN) :: MATPARAM
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ET(NEL),
     .    RHOSP(MVSIZ)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .    UVAR(NEL,NUVAR), OFF(NEL),  PLA(NEL), OFFG(NEL), RHOREF(MVSIZ)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   TF(*),FINTER
      EXTERNAL FINTER
C   EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II,J,J1,J2,IADBUF,NFUNC,
     .   NINDEX_EPSSM,NINDEX_EPST,NINDEX_MU,NLOAD,NUNLOAD,MX,
     .         ILOAD0,IDAMAGE,ITENS,NUMTABL
      INTEGER, DIMENSION(100)   :: IFUNC
      INTEGER, DIMENSION(NEL)   :: JJ,IE_CST,ILOAD
      INTEGER, DIMENSION(NEL)   :: INDEX_EPSSM,INDEX_EPST,INDEX_MU
      INTEGER, DIMENSION(NEL)   :: IPOS1,IADP1,ILENP1
      INTEGER, DIMENSION(NEL,2) :: IPOS
      my_real :: YLD_EMAX,R,FAC,YP1,YP2,EPSP0,EPSPMAX,E,DELTA,SVM2,SVM,
     .        AA,E0,NU,EMAX,EPSSMAX,DSIG,P,EXPO,HYS,DE,YFACC,MU,ALPHA
      my_real, DIMENSION(NEL) :: ALPHA_1,AA1,AA2,EPST,YLDMIN,YLDMAX,YLDELAS,
     .   G,SIG0,EPS0,EPSS,TAB_LOC1,TAB_LOC2,YLD,DFC
      my_real, DIMENSION(NEL,2) :: XVEC
C=======================================================================
      MX = MAT(1)
      NFUNC  = IPM(10,MX)
      DO J=1,NFUNC
        IFUNC(J)=IPM(10+J,MX)
      ENDDO
C     
      IADBUF  = IPM(7,MX)-1
      E0      = UPARAM(IADBUF + 2)
      AA      = UPARAM(IADBUF + 3)
      EPSSMAX = UPARAM(IADBUF + 4)
      G(:)    = UPARAM(IADBUF + 5)
      NU      = UPARAM(IADBUF + 6)
      EPSP0   = UPARAM(IADBUF + 9) ! static strain rate
      NLOAD   = NINT(UPARAM(IADBUF + 7))
      NUNLOAD = NINT(UPARAM(IADBUF + 8))
      IDAMAGE = NINT(UPARAM(IADBUF + 2*NFUNC + 9))
      ITENS   = NINT(UPARAM(IADBUF + 2*NFUNC + 13))
      YFACC   = UPARAM(IADBUF + 2*NFUNC + 8)
      EXPO    = UPARAM(IADBUF + 2*NFUNC + 10)
      HYS     = UPARAM(IADBUF + 2*NFUNC + 11)
      EMAX    = UPARAM(IADBUF + 2*NFUNC + 12)
      YLD_EMAX= UPARAM(IADBUF + 2*NFUNC + 15)
      NUMTABL = MATPARAM%NTABLE
C-----------------------------------------------
C     epst_spherique
C-------------------
      DO I=1,NEL
        EPST(I) = EPSXX(I)**2 + EPSYY(I)**2 + EPSZZ(I)**2
     .          + HALF*(EPSXY(I)**2+EPSYZ(I)**2+EPSZX(I)**2)
        EPST(I) = SQRT(EPST(I)) 
        EPS0(I) = UVAR(I,1)
        SIG0(I) = UVAR(I,2)
      ENDDO            
C-------------------
c     Criterion
C-------------------
      NINDEX_EPST = 0
      NINDEX_EPSSM = 0
c
c     static yield at epssmax
      DO I=1,NEL
        IPOS(I,1) = VARTMP(I,1)
        IPOS(I,2) = 1  
        XVEC(I,1) = EPST(I)
        XVEC(I,2) = EPSP0
      ENDDO
c
      CALL TABLE_MAT_VINTERP(MATPARAM%TABLE(1),NEL,NEL,IPOS,XVEC,YLD,DFC)
c
      DO I=1,NEL
        IF (EPST(I) >= EPSSMAX) THEN
          NINDEX_EPSSM = NINDEX_EPSSM + 1
          INDEX_EPSSM(NINDEX_EPSSM) = I  
          YLDELAS(I) = YLD_EMAX + EMAX*(EPST(I) - EPSSMAX)
        ELSE
          NINDEX_EPST = NINDEX_EPST + 1
          INDEX_EPST(NINDEX_EPST) = I
          YLDELAS(I)  = YLD(I)
        ENDIF
      ENDDO
c=======================================================================
c     Loading case
c=======================================================================
      IF (NLOAD > 1) THEN  ! strain rate dependent
        EPSPMAX = MATPARAM%TABLE(1)%X(2)%VALUES(NLOAD)  ! max strain rate
#include "vectorize.inc"
        DO I=1,NEL
          IPOS(I,1) = VARTMP(I,2)
          IPOS(I,2) = VARTMP(I,5)
          XVEC(I,1) = MIN(EPST(I), EPSSMAX)
          XVEC(I,2) = EPSP(I)
c          XVEC(I,2) = MIN(EPSP(I), EPSPMAX)
c          XVEC(I,2) = MAX(EPSP(I), EPSP0)
        ENDDO
c
        CALL TABLE_MAT_VINTERP(MATPARAM%TABLE(1),NEL,NEL,IPOS,XVEC,YLD,DFC)
c
      ELSE IF (NLOAD == 1) THEN  ! static only
c
        IPOS(:,1) = VARTMP(:,2)
        IPOS(:,2) = VARTMP(:,5)
        DO I=1,NEL
          XVEC(I,1)  = MIN(EPST(I), EPSSMAX)
        ENDDO
c
        CALL TABLE_MAT_VINTERP(MATPARAM%TABLE(1),NEL,NEL,IPOS,XVEC,YLD,DFC)
c
      END IF
c
      VARTMP(:,2) = IPOS(:,1)
      YLDMAX(:)   = YLD(:)
c-----------------------------------------------------------------------
c     case EPST(I) >= EPSSMAX
c-----------------------------------------------------------------------
      IF (NINDEX_EPSSM > 0) THEN 
#include "vectorize.inc"
        DO II=1,NINDEX_EPSSM  
          I = INDEX_EPSSM(II)       
          YLDMAX(I) = YLDMAX(I) + EMAX*(EPST(I) - EPSSMAX) 
        ENDDO
      END IF
c=======================================================================
c     Unloading case
c=======================================================================
      IF (NUNLOAD > 0) THEN
        IF (NUNLOAD > 1) THEN
          EPSPMAX = MATPARAM%TABLE(1)%X(2)%VALUES(NUNLOAD)  ! max strain rate
#include "vectorize.inc"
          DO I=1,NEL
            IPOS(I,1) = VARTMP(I,3)
            IPOS(I,2) = VARTMP(I,6)
            XVEC(I,1) = MIN(EPST(I), EPSSMAX)
            XVEC(I,2) = EPSP(I) ! MAX(MIN(EPSP(I), EPSPMAX), EPSP0)
          ENDDO
c
          CALL TABLE_MAT_VINTERP(MATPARAM%TABLE(2),NEL,NEL,IPOS,XVEC,YLD,DFC)
c
        ELSE IF (NUNLOAD == 1) THEN
          IPOS(:,1) = VARTMP(:,3)
          IPOS(:,2) = VARTMP(:,6)
          DO I=1,NEL
            XVEC(I,1) = EPST(I) !  MAX(MIN(EPST(I), EPSSMAX),EPSP0) 
          ENDDO
c
          CALL TABLE_MAT_VINTERP(MATPARAM%TABLE(2),NEL,NEL,IPOS,XVEC,YLD,DFC)
c
        END IF
c
        YLDMIN(:)  = YLD(:)
c-----------------------------------------------------------------------
c       case EPST(I) >= EPSSMAX
c-----------------------------------------------------------------------
        IF (NINDEX_EPSSM > 0) THEN 
#include "vectorize.inc"
          DO II=1,NINDEX_EPSSM  
            I = INDEX_EPSSM(II)       
            YLDMIN(I) = YLDMIN(I) + EMAX*(EPST(I) - EPSSMAX) 
          ENDDO
        END IF
c
      END IF ! UNLOAD
c=======================================================================
c     end loading / unloading
c=======================================================================
      IF (ITENS > 0) THEN
        DO I=1,NEL
          ALPHA = ONE/MAX(EM20,UVAR(I,10))
          SIG0XX(I) = ALPHA*SIG0XX(I)
          SIG0YY(I) = ALPHA*SIG0YY(I)
          SIG0ZZ(I) = ALPHA*SIG0ZZ(I)
          SIG0XY(I) = ALPHA*SIG0XY(I)
          SIG0ZX(I) = ALPHA*SIG0ZX(I)
          SIG0YZ(I) = ALPHA*SIG0YZ(I)
        ENDDO 
      ENDIF
C =====================================================
      DO I = 1,NEL      
        ILOAD0 = INT(UVAR(I,5))    
        IE_CST(I)= 0
        DELTA = EPST(I) - UVAR(I,4)
        E = UVAR(I,3)
C       
        IF (DELTA >= ZERO ) THEN
          YLD(I) = YLDMAX(I)
          ILOAD(I) = 1
        ELSEIF (DELTA < ZERO) THEN
          YLD(I) = YLDMIN(I)
          ILOAD(I) = -1 
          IF (IDAMAGE /= 0 ) THEN
            YLD(I) = YLDMAX(I)
          ENDIF
        ENDIF  
C       
        EPSS(I) = EPST(I) - YLD(I)/ E
        EPSS(I) = MAX(ZERO, EPSS(I))  
        DE = AA*(EPSS(I) - UVAR(I,1))
        IF (ILOAD(I) == 1) THEN
          E = E + MAX(DE, ZERO)
          IF(ILOAD0 == -1) E= UVAR(I,3)
          UVAR(I,1) = MAX(UVAR(I,1), EPSS(I))
        ELSE
          E = E + MIN(DE ,ZERO)
          IF(ILOAD0 == 1) E= UVAR(I,3)
          UVAR(I,1) = MIN(EPSS(I),UVAR(I,1))
        ENDIF
        E = MIN(E, EMAX)
        E = MAX(E, E0)
        UVAR(I,3) = E
C ---- 
        AA1(I) = E*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU)
        AA2(I) = AA1(I)*NU/(ONE - NU)  
        G(I)   = HALF*E/(ONE + NU)
C ---- 
        SIGNXX(I)= AA1(I)*DEPSXX(I) +  AA2(I)*(DEPSYY(I) + DEPSZZ(I))
        SIGNYY(I)= AA1(I)*DEPSYY(I) +  AA2(I)*(DEPSXX(I) + DEPSZZ(I))
        SIGNZZ(I)= AA1(I)*DEPSZZ(I) +  AA2(I)*(DEPSXX(I) + DEPSYY(I))
        SIGNXY(I)= G(I) *DEPSXY(I)
        SIGNYZ(I)= G(I) *DEPSYZ(I)
        SIGNZX(I)= G(I) *DEPSZX(I)
        DSIG = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .          + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
        DSIG =SQRT(DSIG)                         
C       estimate stress   
        SIGNXX(I)=SIG0XX(I) +  AA1(I)*DEPSXX(I)
     .                       +  AA2(I)*(DEPSYY(I) + DEPSZZ(I))
        SIGNYY(I)=SIG0YY(I) +  AA1(I)*DEPSYY(I)
     .                       +  AA2(I)*(DEPSXX(I) + DEPSZZ(I))
        SIGNZZ(I)=SIG0ZZ(I) +  AA1(I)*DEPSZZ(I)
     .                       +  AA2(I)*(DEPSXX(I) + DEPSYY(I))
        SIGNXY(I)=SIG0XY(I) + G(I) *DEPSXY(I)
        SIGNYZ(I)=SIG0YZ(I) + G(I) *DEPSYZ(I)
        SIGNZX(I)=SIG0ZX(I) + G(I) *DEPSZX(I)

        SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .  + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
        SVM =SQRT(SVM2)
C       
        IF (IDAMAGE == 0 ) THEN
          IF(DELTA >= ZERO ) THEN
             YLD(I) = MIN(YLDMAX(I),SVM)
          ELSEIF(DELTA < ZERO) THEN
             YLD(I) = MAX(YLDMIN(I),SVM)
             YLD(I) = MIN(YLD(I),UVAR(I,6))
             IE_CST(I) = 1
             IF (DSIG > YLDMIN(I) .AND. DSIG > SVM)THEN
               YLD(I) = YLDMIN(I)
               IE_CST(I) = 0
             ENDIF
          ENDIF
        ELSE
          YLD(I) = YLDMAX(I)
          UVAR(I,8) = UVAR(I,8) +  HALF*(YLDELAS(I) + UVAR(I,9))*DELTA
          UVAR(I,8) = MAX(ZERO, UVAR(I,8))
          UVAR(I,2) = MAX(UVAR(I,2) , UVAR(I,8))  
        ENDIF
        UVAR(I,9) = YLDELAS(I)
      ENDDO 
C                 
C =====================================================
C sound speed        
C =====================================================
      IF (IDTMINS/=2.OR.JSMS==0)THEN ! compatibilite ascendante
        DO I = 1,NEL
         !-----------------------
         !     P = C1 mu, mu = rho/rho0-1
         !     d(rho)/d(mu)  = rho0
         !-----------------------
          SOUNDSP(I) = SQRT(AA1(I)/RHO0(I))
          RHOSP(I)   = RHO0(I)
          VISCMAX(I) = ZERO 
        ENDDO 
      ELSEIF (ISMSTR==1 .OR. ISMSTR==11)THEN ! Small Strain
        DO I = 1,NEL
         !-----------------------
         !     P = C1 mu, mu = rho/rho0-1
         !     d(rho)/d(mu)  = rho0
         !-----------------------
          SOUNDSP(I) = SQRT(AA1(I)/RHOREF(I))
          RHOSP(I)   = RHOREF(I)
          VISCMAX(I) = ZERO 
        ENDDO 
      ELSE
        DO I = 1,NEL
          IF (ABS(OFFG(I)) <= ONE)THEN ! Large Strain
            !-----------------------
            !     P = C1 mu, mu = ln(rho/rho0), rho= rho0 exp(mu)
            !     d(rho)/d(mu)  = rho
            !-----------------------
            SOUNDSP(I) = SQRT(AA1(I)/RHO(I))
            RHOSP(I)   = RHO(I)
          ELSE ! Small Strain
           !-----------------------
           !     P = C1 mu, mu = rho/rho0-1
           !     d(rho)/d(mu)  = rho0
           !-----------------------
            SOUNDSP(I) = SQRT(AA1(I)/RHOREF(I))
            RHOSP(I)   = RHOREF(I)
          END IF
          VISCMAX(I) = ZERO  
        ENDDO 
      END IF
C 
C-------------------
C projection spherique
C-------------------
      IF (IDAMAGE == 0 ) THEN
        DO I=1,NEL        
          SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
          SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
          SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
          SIGNXY(I)= G(I) *EPSXY(I)
          SIGNYZ(I)= G(I) *EPSYZ(I)
          SIGNZX(I)= G(I) *EPSZX(I)
C
          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .         + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          SVM =SQRT(SVM2)
          R  = YLD(I)/MAX(EM20,SVM)
          ET(I) = R
          SIGNXX(I)=SIGNXX(I)*R                                
          SIGNYY(I)=SIGNYY(I)*R                                
          SIGNZZ(I)=SIGNZZ(I)*R                                
          SIGNXY(I)=SIGNXY(I)*R                                
          SIGNYZ(I)=SIGNYZ(I)*R                                
          SIGNZX(I)=SIGNZX(I)*R            
c         E = E0(I) + AA(I)*EPS0(I)
c         EPSS(I) = EPST(I) - YLD(I)/E
c         EPSS(I) = MAX(EPSS(I), ZERO)      
c         UVAR(I,1) = EPSS(I)
c         E = E0(I) + AA(I)*EPSS(I)
c         UVAR(I,3) = E 
C         
          IF (IE_CST(I) == 1) THEN
            ILOAD0 = INT(UVAR(I,5))
            IF(ILOAD0 /= ILOAD(I))THEN
              ILOAD(I)  = ILOAD0
              UVAR(I,1) = EPS0(I)
            ENDIF
          ENDIF   
          UVAR(I,4) = EPST(I)
          UVAR(I,2) = SVM*R
          UVAR(I,5) = ILOAD(I)
          UVAR(I,6) = YLD(I)
          UVAR(I,7) = EPSP(I)
        ENDDO
c
      ELSEIF (IDAMAGE == 1) THEN
c       
        DO I=1,NEL
          R = ONE
          SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
          SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
          SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
          SIGNXY(I)= G(I) *EPSXY(I)
          SIGNYZ(I)= G(I) *EPSYZ(I)
          SIGNZX(I)= G(I) *EPSZX(I)
C
          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .        + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          SVM =SQRT(SVM2)
          R  = (YLD(I)/MAX(EM20,SVM))        
          ET(I) = R
          SIGNXX(I)=SIGNXX(I)*R                                
          SIGNYY(I)=SIGNYY(I)*R                                
          SIGNZZ(I)=SIGNZZ(I)*R                                
          SIGNXY(I)=SIGNXY(I)*R                                
          SIGNYZ(I)=SIGNYZ(I)*R                                
          SIGNZX(I)=SIGNZX(I)*R   
C         
          IF (ILOAD(I) == -1) THEN
            R = (YLDMIN(I)/MAX(EM20,YLDELAS(I)))
            P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
            SIGNXX(I)=(SIGNXX(I) - P)*R + P 
            SIGNYY(I)=(SIGNYY(I) - P)*R + P                             
            SIGNZZ(I)=(SIGNZZ(I) - P)*R + P                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
          ENDIF        
C       
          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)        
     .         + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)   
          SVM =SQRT(SVM2)                                          
          UVAR(I,4) = EPST(I)
          UVAR(I,5) = ILOAD(I)
          UVAR(I,6) = YLD(I)
          UVAR(I,7) = EPSP(I)       
        ENDDO
c
      ELSEIF(IDAMAGE == 2 ) THEN
c       
        DO I=1,NEL
          SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
          SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
          SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
          SIGNXY(I)= G(I) *EPSXY(I)
          SIGNYZ(I)= G(I) *EPSYZ(I)
          SIGNZX(I)= G(I) *EPSZX(I)
C
          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .           + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          SVM =SQRT(SVM2)
          R  = YLD(I)/MAX(EM20,SVM)         
          ET(I) = R
          SIGNXX(I)=SIGNXX(I)*R                                
          SIGNYY(I)=SIGNYY(I)*R                                
          SIGNZZ(I)=SIGNZZ(I)*R                                
          SIGNXY(I)=SIGNXY(I)*R                                
          SIGNYZ(I)=SIGNYZ(I)*R                                
          SIGNZX(I)=SIGNZX(I)*R   
C       
          IF (ILOAD(I) == -1) THEN
            R = YLDMIN(I)/MAX(EM20,YLDELAS(I))
            SIGNXX(I)=SIGNXX(I)*R 
            SIGNYY(I)=SIGNYY(I)*R                             
            SIGNZZ(I)=SIGNZZ(I)*R                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
            ET(I) = R*ET(I)
          ENDIF        
C       
          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)        
     .    + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)   
          SVM =SQRT(SVM2)                                          
          UVAR(I,4) = EPST(I)
          UVAR(I,5) = ILOAD(I)
          UVAR(I,6) = YLD(I)
          UVAR(I,7) = EPSP(I)
        ENDDO       
c
      ELSEIF (IDAMAGE == 3) THEN
c       
        DO I=1,NEL
          SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
          SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
          SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
          SIGNXY(I)= G(I) *EPSXY(I)
          SIGNYZ(I)= G(I) *EPSYZ(I)
          SIGNZX(I)= G(I) *EPSZX(I)
C
          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .         + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          SVM =SQRT(SVM2)
          R  = (YLD(I)/MAX(EM20,SVM))        
          SIGNXX(I)=SIGNXX(I)*R                                
          SIGNYY(I)=SIGNYY(I)*R                                
          SIGNZZ(I)=SIGNZZ(I)*R                                
          SIGNXY(I)=SIGNXY(I)*R                                
          SIGNYZ(I)=SIGNYZ(I)*R                                
          SIGNZX(I)=SIGNZX(I)*R   
          ET(I) = R
C       
          IF (ILOAD(I)==-1 .AND.UVAR(I,2)/=ZERO)THEN
            R = ONE - (UVAR(I,8)/UVAR(I,2))**EXPO
            R = ONE - (ONE - HYS)*R
            P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
            SIGNXX(I)=(SIGNXX(I) - P)*R + P 
            SIGNYY(I)=(SIGNYY(I) - P)*R + P                             
            SIGNZZ(I)=(SIGNZZ(I) - P)*R + P                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
            ET(I) = R*ET(I)
          ENDIF        
C                                                    
          UVAR(I,4) = EPST(I)
          UVAR(I,5) = ILOAD(I)
          UVAR(I,6) = YLD(I)
          UVAR(I,7) = EPSP(I)
        ENDDO
c
      ELSEIF (IDAMAGE == 4) THEN
c
        DO I=1,NEL
          SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
          SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
          SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
          SIGNXY(I)= G(I) *EPSXY(I)
          SIGNYZ(I)= G(I) *EPSYZ(I)
          SIGNZX(I)= G(I) *EPSZX(I)
C
          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .    + TWO*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          SVM =SQRT(SVM2)
          R  = (YLD(I)/MAX(EM20,SVM))        
          ET(I) = R
          SIGNXX(I)=SIGNXX(I)*R                                
          SIGNYY(I)=SIGNYY(I)*R                                
          SIGNZZ(I)=SIGNZZ(I)*R                                
          SIGNXY(I)=SIGNXY(I)*R                                
          SIGNYZ(I)=SIGNYZ(I)*R                                
          SIGNZX(I)=SIGNZX(I)*R 
C               
          IF (ILOAD(I)==-1 .AND.UVAR(I,2)/= ZERO)THEN
            R = ONE - (UVAR(I,8)/UVAR(I,2))**EXPO
            R = ONE - (ONE - HYS)*R
            SIGNXX(I)=SIGNXX(I)*R 
            SIGNYY(I)=SIGNYY(I)*R                             
            SIGNZZ(I)=SIGNZZ(I)*R                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
          ENDIF   
          UVAR(I,4) = EPST(I)
          UVAR(I,5) = ILOAD(I)
          UVAR(I,6) = YLD(I)
          UVAR(I,7) = EPSP(I)
        ENDDO                           
c
      ENDIF  ! case of IDAMAGE 
C-------------------------------------------------------
      IF (ITENS > 0 ) THEN
        ALPHA_1(1:NEL) = ONE
        NINDEX_MU = 0
C
        DO I=1,NEL
          MU = 1 - RHO(I)/RHO0(I) 
          IF (MU > 0) THEN
            NINDEX_MU = NINDEX_MU + 1
            INDEX_MU(NINDEX_MU) = I
            IPOS1(NINDEX_MU)  = VARTMP(I,4)
            IADP1(NINDEX_MU)  = NPF(IFUNC(NFUNC)) / 2 + 1
            ILENP1(NINDEX_MU) = NPF(IFUNC(NFUNC)+1) / 2 - IADP1(NINDEX_MU) - IPOS1(NINDEX_MU)
            TAB_LOC1(NINDEX_MU) = MU
          ENDIF
        ENDDO
C
        IF ( NINDEX_MU > 0 ) THEN

          CALL VINTER2(TF,IADP1,IPOS1,ILENP1,NINDEX_MU,TAB_LOC1,DFC,YLD)

          DO II=1,NINDEX_MU
            I = INDEX_MU(II)        
            ALPHA_1(I) = YFACC*YLD(II)
            ALPHA_1(I) = MAX(ZERO, ALPHA_1(I))
            VARTMP(I,4) = IPOS1(II)
         ENDDO
        ENDIF
C
        DO I=1,NEL
          SIGNXX(I) = ALPHA_1(I)*SIGNXX(I)
          SIGNYY(I) = ALPHA_1(I)*SIGNYY(I)
          SIGNZZ(I) = ALPHA_1(I)*SIGNZZ(I)
          SIGNXY(I) = ALPHA_1(I)*SIGNXY(I)
          SIGNZX(I) = ALPHA_1(I)*SIGNZX(I)
          SIGNYZ(I) = ALPHA_1(I)*SIGNYZ(I)
          UVAR(I,10) = ALPHA_1(I)
          ET(I) = ALPHA_1(I)*ET(I)
        ENDDO     
      ENDIF  ! ITENS > 0
C-----------------------------------------------------
      IF (IMPL_S > 0 .OR. IHET > 1) THEN       
         DO I=1,NEL
            ET(I) = MIN(ONE , ET(I))
         ENDDO
      ENDIF 
C------------------------------------ 
      RETURN
      END SUBROUTINE
