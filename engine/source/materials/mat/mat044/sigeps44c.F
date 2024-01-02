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
Chd|  SIGEPS44C                     source/materials/mat/mat044/sigeps44c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS44C( 
     1     NEL     ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC  ,
     2     NPF     ,TF     ,TIME    ,TIMESTEP,UPARAM ,
     2     RHO0    ,THKLY  ,OFF     ,ETSE    ,
     3     EPSPXX  ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     4     DEPSXX  ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     EPSXX   ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     6     SIGOXX  ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     SIGNXX  ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     SOUNDSP ,VISCMAX,THK     ,PLA     ,UVAR   ,
     9     GS      ,YLD    ,EPSP    ,DPLA_I  ,ASRATE ,
     A     NVARTMP ,VARTMP ,SIGP    ,INLOC   ,DPLANL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL    | F | R | INITIAL DENSITY
C THKLY   | NEL    | F | R | LAYER THICKNESS
C EPSPXX  | NEL    | F | R | STRAIN RATE XX
C EPSPYY  | NEL    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL    | F | R | STRAIN XX
C EPSYY   | NEL    | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL    | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL    | F |R/W| THICKNESS
C PLA     | NEL    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "mvsiz_p.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM, NUVAR,NVARTMP,INLOC
      my_real ,INTENT(IN) :: TIME,TIMESTEP,ASRATE
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL)     ,INTENT(IN) :: RHO0,THKLY,GS,DPLANL,
     .   EPSPXX,EPSPYY,EPSPXY,EPSPYZ,EPSPZX,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSXX ,EPSYY ,EPSXY ,EPSYZ ,EPSZX ,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      INTEGER ,INTENT(IN) :: NPF(*),MFUNC,KFUNC(MFUNC)
      my_real ,INTENT(IN) :: TF(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: SOUNDSP,VISCMAX,ETSE,
     .    SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,EPSP,DPLA_I
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      my_real ,DIMENSION(NEL)         ,INTENT(INOUT) :: PLA,OFF,THK,YLD
      my_real ,DIMENSION(NEL,3)       ,INTENT(INOUT) :: SIGP
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,N,NINDX,NMAX,IADBUF,ICC1,ISRATE,VFLAG
      my_real :: R,UMR,NUX,A,B,C,S11,S22,S12,P,P2,DEZZ, 
     .   SIGZ,S1,S2,S3,VM2,EPST,F,DF,Q2,YLD_I,E1,A11,A21,
     .   G1,G31,NNU11,NU11,NU21,NU31,DEVE1,DEVE2,DEVE3,DEVE4,DPDT,
     .   EPSM1,EPSR11,EPSR21,FISOKIN1,CA1,CB1,CN1,CC1,CP1,
     .   DSXX,DSYY,DSXY,DEXX,DEYY,DEXY,ALPHA,YSCALE,DAV
      INTEGER ,DIMENSION(MVSIZ) :: INDEX,IPOS,ILEN,IAD,IPOS0
      my_real ,DIMENSION(MVSIZ) :: SVM,DR,AA,BB,DPLA_J,PP,QQ,FAIL,H,HS,
     .   YLO,ZEROR,SIGEXX,SIGEYY,SIGEXY,SIGM,EPSGM,DFDPLA
      my_real, DIMENSION(MVSIZ) :: RQ
C
      DATA NMAX/3/
C-----------------------------------------------
C     MATERIAL PARAMETERS
C-----------------------------------------------
       E1       = UPARAM(1)
       NUX      = UPARAM(2)
       EPSM1    = UPARAM(5)
       EPSR11   = UPARAM(6)
       EPSR21   = UPARAM(7)
       CA1      = UPARAM(3)
       CB1      = UPARAM(8)
       CN1      = UPARAM(9)
       ICC1     = NINT(UPARAM(10))
       CC1      = UPARAM(11)
       CP1      = UPARAM(12)
       FISOKIN1 = UPARAM(15)
       G1       = UPARAM(16)
       G31      = UPARAM(18)
       A11      = UPARAM(20)
       A21      = UPARAM(21)
       ISRATE   = NINT(UPARAM(13)) 
       VFLAG    = NINT(UPARAM(23))
       YSCALE   = UPARAM(24)
C       
       NNU11 = NUX / (ONE - NUX)
       NU11  = ONE/(ONE-NUX)
       NU21  = ONE/(ONE+NUX)
       NU31  = ONE - NNU11
C       
       DO I=1,NEL
         EPSGM(I) = UPARAM(22)
         SIGM(I)  = UPARAM(4)
       ENDDO
C       
       IF (MFUNC > 0) THEN
         ZEROR(1:NEL) = ZERO
         IPOS0(1:NEL) = 1
         IAD (1:NEL)  = NPF(KFUNC(1)) / 2 + 1
         ILEN(1:NEL)  = NPF(KFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS0(1:NEL)
         CALL VINTER(TF,IAD,IPOS0,ILEN,NEL,ZEROR,DFDPLA,YLO)
         YLO(1:NEL)   = YSCALE * YLO(1:NEL)
       ENDIF
C-----------------------------------------------
C     ELASTIC STRESS ESTIMATE 
C-----------------------------------------------
       IF (FISOKIN1 > ZERO) THEN 
           SIGNXX(1:NEL) = SIGOXX(1:NEL) - SIGP(1:NEL,1) + A11*DEPSXX(1:NEL) + A21*DEPSYY(1:NEL)
           SIGNYY(1:NEL) = SIGOYY(1:NEL) - SIGP(1:NEL,2) + A21*DEPSXX(1:NEL) + A11*DEPSYY(1:NEL)
           SIGNXY(1:NEL) = SIGOXY(1:NEL) - SIGP(1:NEL,3) + G1 *DEPSXY(1:NEL)
       ELSE
           SIGNXX(1:NEL) = SIGOXX(1:NEL) + A11*DEPSXX(1:NEL) + A21*DEPSYY(1:NEL)
           SIGNYY(1:NEL) = SIGOYY(1:NEL) + A21*DEPSXX(1:NEL) + A11*DEPSYY(1:NEL)
           SIGNXY(1:NEL) = SIGOXY(1:NEL) + G1 *DEPSXY(1:NEL)
       ENDIF
       SIGNYZ(1:NEL) = SIGOYZ(1:NEL) + GS(1:NEL) * DEPSYZ(1:NEL)
       SIGNZX(1:NEL) = SIGOZX(1:NEL) + GS(1:NEL) * DEPSZX(1:NEL)
       SIGEXX(1:NEL) = SIGNXX(1:NEL)
       SIGEYY(1:NEL) = SIGNYY(1:NEL)
       SIGEXY(1:NEL) = SIGNXY(1:NEL)
C
       SOUNDSP(1:NEL) = SQRT(A11/RHO0(1:NEL))
       VISCMAX(1:NEL) = ZERO
       ETSE(1:NEL) = ONE
C-----------------------------------------------
C     TOTAL OR DEVIATORIC STRAIN-RATE COMPUTATION
C-----------------------------------------------
       IF (VFLAG == 1) THEN 
           EPSP(1:NEL) = UVAR(1:NEL,1)
       ENDIF
       IF (ISRATE > 0) THEN 
           IF (VFLAG == 3) THEN
             DO I=1,NEL
                DAV = (EPSPXX(I)+EPSPYY(I))*THIRD
                DEVE1  = EPSPXX(I) - DAV
                DEVE2  = EPSPYY(I) - DAV
                DEVE3  = - DAV
                DEVE4  = HALF*EPSPXY(I)
                EPSP(I)   = HALF*(DEVE1**2 + DEVE2**2 + DEVE3**2) + DEVE4**2
                EPSP(I)   = SQRT(THREE*EPSP(I))/THREE_HALF             
                EPSP(I)   = ASRATE*EPSP(I) + (ONE - ASRATE)*UVAR(I,1)
                UVAR(I,1) = EPSP(I)
              ENDDO
           ENDIF
       ELSEIF (ISRATE == 0) THEN
           IF (VFLAG == 3) THEN
             DO I=1,NEL
                 DAV = (EPSPXX(I)+EPSPYY(I))*THIRD
                 DEVE1  = EPSPXX(I) - DAV
                 DEVE2  = EPSPYY(I) - DAV
                 DEVE3  = - DAV
                 DEVE4  = HALF*EPSPXY(I)
                 EPSP(I)   = HALF*(DEVE1**2 + DEVE2**2 + DEVE3**2) + DEVE4**2
                 EPSP(I)   = SQRT(THREE*EPSP(I))/THREE_HALF             
                 UVAR(I,1) = EPSP(I)
             ENDDO
       ELSEIF (VFLAG == 2) THEN
             DO I=1,NEL
                  EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .               + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .               + EPSPXY(I)*EPSPXY(I) ) )
             ENDDO
           ENDIF
       ENDIF
C-------------------
C     STRAIN & TENSION FAILURE
C-------------------
C
       DO I=1,NEL
         EPST = HALF*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
         FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR21-EPST)/(EPSR21-EPSR11)))
         DPLA_I(I) = ZERO
C
       ENDDO
C-------------------
C      CURRENT YIELD AND HARDENING
C-------------------
C
       IF (MFUNC > 0) THEN
         IPOS(1:NEL)     = VARTMP(1:NEL,1)
         IAD (1:NEL)     = NPF(KFUNC(1)) / 2 + 1
         ILEN(1:NEL)     = NPF(KFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
         CALL VINTER(TF,IAD,IPOS,ILEN,NEL,PLA,DFDPLA,YLD) 
         VARTMP(1:NEL,1) = IPOS(1:NEL)
       ENDIF
C
       RQ(1:NEL) = ONE
       IF(CC1 /= ZERO) RQ(1:NEL) = ONE + (CC1*EPSP(1:NEL))**CP1
C--------------------
       IF((MFUNC > 0) .AND. (CA1 == ZERO)) THEN
         DO I=1,NEL
           YLO(I) = YLO(I) * RQ(I)
           IF (PLA(I)>ZERO) THEN
             YLD(I) = YSCALE * YLD(I) * RQ(I)
             HS(I)  = YSCALE * DFDPLA(I) * RQ(I)
           ELSE
             YLD(I) = YSCALE * YLD(I) * RQ(I)
             HS(I)  = E1
           ENDIF
         ENDDO
C---------------------------
       ELSEIF ((MFUNC > 0) .AND. (CA1 /= ZERO)) THEN 
C--------------------
         DO I=1,NEL
           YLO(I) = YLO(I) + CA1 * (RQ(I)-ONE)
           IF (PLA(I)>ZERO) THEN
             IF (CN1 == ONE) THEN
               YLD(I) = YSCALE * YLD(I) + (CA1 + CB1*PLA(I)) * (RQ(I)-ONE)
               HS(I)  = YSCALE * DFDPLA(I) + CB1 * (RQ(I)-ONE)
             ELSE
               YLD(I) = YSCALE * YLD(I) + (CA1 + CB1*PLA(I)**CN1) * (RQ(I)-ONE)
               IF (CN1>ONE) THEN
                 HS(I) = YSCALE * DFDPLA(I) + CN1*CB1*(RQ(I)-ONE) * (PLA(I)**(CN1-ONE))
               ELSE 
                 HS(I) = YSCALE * DFDPLA(I) + CN1*CB1*(RQ(I)-ONE) / ((PLA(I)**(ONE-CN1)))
               ENDIF
             ENDIF
           ELSE ! PLA <= 0
             YLD(I) = YSCALE * YLD(I) + CA1 * (RQ(I)-ONE)
             HS(I)  = E1
           ENDIF
         ENDDO
       ELSE
C--------------------
         DO I=1,NEL
           YLO(I) = CA1 * RQ(I)
           IF (PLA(I)>ZERO) THEN
             IF (CN1 == ONE) THEN
               YLD(I) = (CA1 + CB1*PLA(I)) * RQ(I)
               HS(I)  = CB1 * RQ(I)
             ELSE
               YLD(I) = (CA1 + CB1*PLA(I)**CN1) * RQ(I)
               IF (CN1>ONE) THEN
                 HS(I) = CN1*CB1*RQ(I) * (PLA(I)**(CN1-ONE))
               ELSE
                 HS(I) = CN1*CB1*RQ(I) / ((PLA(I)**(ONE-CN1)))
               ENDIF
             ENDIF
           ELSE
             YLD(I) = CA1 * RQ(I)
             HS(I)  = E1
           ENDIF
         ENDDO
       ENDIF

       DO I=1,NEL
           IF(ICC1 == 1) SIGM(I) = SIGM(I) * RQ(I)
           IF (ICC1 /= 1 .and. CN1 /= ZERO .and. CB1 /= ZERO)
     &        EPSGM(I)=((SIGM(I)/RQ(I)-CA1)/CB1)**(ONE/CN1)
           IF (PLA(I)>=EPSGM(I)) THEN
              YLD(I) = SIGM(I)
              HS(I)  = ZERO
           ENDIF
           HS(I) = FAIL(I)*HS(I)
C------           kinematic hardening
           YLD(I)= FAIL(I)*((ONE-FISOKIN1)*YLD(I)+FISOKIN1*YLO(I))
           YLD(I) = MAX(YLD(I),EM20)
       ENDDO

C----------------------------------------
C---  VON MISES 
C
      DO  I=1,NEL
        S1=SIGNXX(I)+SIGNYY(I)
        S2=SIGNXX(I)-SIGNYY(I)
        S3=SIGNXY(I)
        AA(I)=FOURTH*S1*S1
        BB(I)=THREE_OVER_4*S2*S2+3.*S3*S3
        SVM(I)=SQRT(AA(I)+BB(I))  
        IF (INLOC == 0) THEN 
          DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
        ENDIF
      ENDDO
C
C---  GATHER PLASTIC FLOW
C
      NINDX = 0
      DO I = 1,NEL
        IF ((SVM(I) > YLD(I)).AND.(OFF(I) == ONE)) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO
C
C---  DEP EN CONTRAINTE PLANE
C
      IF (NINDX > 0) THEN
#include "vectorize.inc"
        DO J=1,NINDX
          I=INDEX(J)
          HS(I) = MAX(ZERO,HS(I))
          DPLA_J(I)=(SVM(I)-YLD(I))/(G31+HS(I))
          ETSE(I)= HS(I)/(HS(I)+E1)
          H(I) = HS(I)*(ONE-FISOKIN1)
        ENDDO
C
        DO N=1,NMAX
#include "vectorize.inc"
          DO J=1,NINDX
            I=INDEX(J)
            DPLA_I(I)=DPLA_J(I)
            YLD_I =YLD(I)+H(I)*DPLA_I(I)
            DR(I) =HALF*E1*DPLA_I(I)/YLD_I
            PP(I) =ONE/(ONE + DR(I)*NU11)
            QQ(I) =ONE/(ONE + THREE*DR(I)*NU21)
            P2    =PP(I)*PP(I)
            Q2    =QQ(I)*QQ(I)
            F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
            DF=-(AA(I)*NU11*P2*PP(I)+ THREE*BB(I)*NU21*Q2*QQ(I))
     .        *(E1-TWO*DR(I)*H(I))/YLD_I
     .        -TWO*H(I)*YLD_I
            DF = SIGN(MAX(ABS(DF),EM20),DF)
            IF(DPLA_I(I)>ZERO) THEN
              DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
            ELSE
              DPLA_J(I)=ZERO
            ENDIF        
          ENDDO
        ENDDO
C
C---    CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C
#include "vectorize.inc"
        DO J=1,NINDX
          I=INDEX(J)
          PLA(I) = PLA(I) + DPLA_I(I)
          S1=(SIGNXX(I)+SIGNYY(I))*PP(I)
          S2=(SIGNXX(I)-SIGNYY(I))*QQ(I)
          SIGNXX(I)=HALF*(S1+S2)
          SIGNYY(I)=HALF*(S1-S2)
          SIGNXY(I)=SIGNXY(I)*QQ(I)
          IF (INLOC == 0) THEN 
            DEZZ = - NU31*DR(I)*S1/E1
            THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
          ENDIF
        ENDDO

      ENDIF ! nindx /= 0
C
      DO I=1,NEL
        IF ((PLA(I) > EPSM1).AND.(OFF(I) == ONE)) OFF(I) = FOUR_OVER_5
        IF (VFLAG == 1) THEN 
          DPDT      = DPLA_I(I)/MAX(EM20,TIMESTEP)
          UVAR(I,1) = ASRATE * DPDT + (ONE - ASRATE) * UVAR(I,1)
          EPSP(I)   = UVAR(I,1)
        ENDIF
      ENDDO
C
C---     KINEMATIC HARDENING
C
      IF (FISOKIN1 > ZERO) THEN                              
        DO I=1,NEL                                          
          DSXX  = SIGEXX(I) - SIGNXX(I)                      
          DSYY  = SIGEYY(I) - SIGNYY(I)                      
          DSXY  = SIGEXY(I) - SIGNXY(I)                      
          DEXX  = (DSXX - NUX*DSYY)                          
          DEYY  = (DSYY - NUX*DSXX)                          
          DEXY  = TWO*(ONE+NUX)*DSXY                         
          ALPHA = FISOKIN1*HS(I)/(E1+HS(I)) * THIRD          
          SIGNXX(I) = SIGNXX(I) + SIGP(I,1)                  
          SIGNYY(I) = SIGNYY(I) + SIGP(I,2)                  
          SIGNXY(I) = SIGNXY(I) + SIGP(I,3)                  
          SIGP(I,1) = SIGP(I,1) + ALPHA*(FOUR*DEXX+TWO*DEYY) 
          SIGP(I,2) = SIGP(I,2) + ALPHA*(FOUR*DEYY+TWO*DEXX) 
          SIGP(I,3) = SIGP(I,3) + ALPHA*DEXY                 
        ENDDO                                                
      ENDIF                                                  
C
C---    NON-LOCAL THICKNESS VARIATION
C
      IF (INLOC > 0) THEN 
        DO I = 1,NEL 
          SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I)
     .          + SIGNYY(I)*SIGNYY(I)
     .          - SIGNXX(I)*SIGNYY(I)
     .          + THREE*SIGNXY(I)*SIGNXY(I))
          DEZZ   = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(SVM(I),EM20)
          DEZZ   = -NUX*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E1) - DEZZ
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)     
        ENDDO  
      ENDIF 
C-----------
      RETURN
      END
