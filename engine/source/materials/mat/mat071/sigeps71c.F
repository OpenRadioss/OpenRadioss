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
C  New law -- material shape memory
Chd|====================================================================
Chd|  SIGEPS71C                     source/materials/mat/mat071/sigeps71c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS71C(
     1   NEL,     NUPARAM, NUVAR,   MFUNC,
     2   KFUNC,   NPF,     NPT0,    IPT,
     3   IFLAG,   TF,      TIME,    TIMESTEP,
     4   UPARAM,  RHO0,    AREA,    EINT,
     5   THKLY,   EPSPXX,  EPSPYY,  EPSPXY,
     6   EPSPYZ,  EPSPZX,  DEPSXX,  DEPSYY,
     7   DEPSXY,  DEPSYZ,  DEPSZX,  EPSXX,
     8   EPSYY,   EPSXY,   EPSYZ,   EPSZX,
     9   SIGOXX,  SIGOYY,  SIGOXY,  SIGOYZ,
     A   SIGOZX,  SIGNXX,  SIGNYY,  SIGNXY,
     B   SIGNYZ,  SIGNZX,  SIGVXX,  SIGVYY,
     C   SIGVXY,  SIGVYZ,  SIGVZX,  SOUNDSP,
     D   VISCMAX, THK,     PLA,     UVAR,
     E   OFF,     NGL,     IPM,     MAT,
     F   ETSE,    GS,      YLD,     VOL,
     G   TEMPEL,  ISMSTR,  JTHE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYD| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL0    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL0 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C NPT0    |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL0    | F | R | INITIAL DENSITY
C AREA    | NEL0    | F | R | AREA
C EINT    | 2*NEL0  | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C THKLY   | NEL0    | F | R | LAYER THICKNESS
C EPSPXX  | NEL0    | F | R | STRAIN RATE XX
C EPSPYY  | NEL0    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL0    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL0    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL0    | F | R | STRAIN XX
C EPSYY   | NEL0    | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL0    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL0    | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL0    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL0    | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL0    | F |R/W| THICKNESS
C PLA     | NEL0    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL0*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL0    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER, INTENT(IN) :: ISMSTR, JTHE
      INTEGER NEL, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL),MAT(NEL),ISRATE(NEL),NSG,IPM(NPROPMI,*)
      my_real TIME,TIMESTEP,UPARAM(*),
     .   AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THKLY(NEL),PLA(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   GS(*) ,VOL(NEL)  , TEMPEL(NEL) 
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
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
      INTEGER I,I1,I2,J,JJ,J1,J2,KK,ITERK,EFLAG 
      INTEGER CRIT_LOOP(NEL)
      INTEGER INDX_LOOP(NEL),NINDX_LOOP,NINDX_LOOP_LOC,II
      INTEGER, DIMENSION(NEL) :: INDX_LOOP_LOC
     .        
      my_real
     .    E,EMART,NU,G,G2,WAVE,SQDT,A,B,C,FCT,FCTP, DFTR,UNMXN,DB,
     .    ALPHA,EPSL,AA1,FM,DFMSA,DFMAS,UXX,NN,BETA,GM,KM,H,
     .    CB,CC,CAAS,CBAS,POLD,DGT,DKT,CP,TINI,SSPSH ,SSPSOL,
     .    K,P11(NEL),SXX(NEL),CAS,CSA,TSAS,TFAS, TSSA,TFSA,INVE,
     .    SYY(NEL),SZZ(NEL),SXY,SYZ,SZX,FASS,FSAS,FASF,FSAF,RSAS,RFAS,
     .    SV(NEL),FS(NEL),FS0,YLD_ASS,YLD_ASF,YLD_SAS,YLD_SAF,RSSA,RFSA,
     .    DFM(NEL), FSS,DSXX,DSYY,DSXY,DSYZ,DSZX,DSZZ,VAR,RV_PUI,
     .    PM,DELTA,X1,X2,test,test2,ftest,gnew,knew,BETAn,
     .    NX2,NY2 ,NZ2,NXY2,NYZ2,NZX2,NE,DNX,DNY,DNZ,DNXY,DNYZ,DNZX,
     .    NXX(NEL),NYY(NEL),NZZ(NEL),NXY(NEL),NYZ(NEL),NZX(NEL), 
     .    E1(NEL),E2(NEL),E3(NEL),E4(NEL),E5(NEL),E6(NEL),TRDE(NEL),
     .    DE1(NEL),DE2(NEL),DE3(NEL),GT(NEL),KT(NEL),TEMP(NEL),
     .    EE1(NEL),EE2(NEL),EE3(NEL),PP(NEL),NNE(NEL),DET(NEL),
     .    SIGXX(NEL), SIGYY(NEL), SIGZZ(NEL),DFS(NEL)
      my_real
     .    DEPSZZTR(NEL),DEPSZZ(NEL),EPSZZ(NEL),SIGNZZ(NEL),EPSZZTR(NEL),
     .    DEPSIM1(NEL),SIGZIM1(NEL),DEPSI(NEL),EPSIM1(NEL),EPSI(NEL)
      my_real
     .            EV(MVSIZ,3) 
      my_real
     .    EIGV(NEL,3,2),TRAV(NEL),ROOTV(NEL),EVV(NEL,3)

      DATA ITERK/10/
C======================================================================| 
      E         = UPARAM(1)                     
      NU        = UPARAM(2)                     
      G         = UPARAM(3)                     
      K         = UPARAM(4)                     
      AA1       = UPARAM(5)                     
      YLD_ASS   = UPARAM(6)                     
      YLD_ASF   = UPARAM(7)                     
      YLD_SAS   = UPARAM(8)                     
      YLD_SAF   = UPARAM(9)                     
      ALPHA     = UPARAM(10)                    
      EPSL      = UPARAM(11)/(SQRT(TWO_THIRD)+ALPHA)                    
      EMART     = UPARAM(14)                    
      EFLAG     = INT(UPARAM(15))               
      GM        = UPARAM(16)                    
      KM        = UPARAM(17)                    
      G2        = TWO*G                        
      BETA      = EPSL*(G2+NINE*K*ALPHA*ALPHA)  
      SQDT      = SQRT(TWO/THREE)              
      CAS       = UPARAM(18)                    
      CSA       = UPARAM(19)                    
      TSAS      = UPARAM(20)                    
      TFAS      = UPARAM(21)                    
      TSSA      = UPARAM(22)                    
      TFSA      = UPARAM(23)                    
      CP        = UPARAM(24)                    
      TINI      = UPARAM(25)

C     principal stretch (def gradient eigenvalues)                                 
      TRAV(1:NEL)  = EPSXX(1:NEL)+EPSYY(1:NEL)                                     
      ROOTV(1:NEL) = SQRT((EPSXX(1:NEL)-EPSYY(1:NEL))*(EPSXX(1:NEL)-EPSYY(1:NEL))  
     .           + EPSXY(1:NEL)*EPSXY(1:NEL))
	     EVV(1:NEL,1) = HALF*(TRAV(1:NEL)+ROOTV(1:NEL))
      EVV(1:NEL,2) = HALF*(TRAV(1:NEL)-ROOTV(1:NEL))  
      EVV(1:NEL,3) = ZERO                               
C     rot matrix (eigenvectors)
      DO I=1,NEL
        IF(ABS(EVV(I,2)-EVV(I,1)) < EM10) THEN
          EIGV(I,1,1) = ONE
          EIGV(I,2,1) = ONE
          EIGV(I,3,1) = ZERO
          EIGV(I,1,2) = ZERO
          EIGV(I,2,2) = ZERO
          EIGV(I,3,2) = ZERO
        ELSE
          EIGV(I,1,1) = (EPSXX(I)-EVV(I,2)) /ROOTV(I)
          EIGV(I,2,1) = (EPSYY(I)-EVV(I,2)) /ROOTV(I)
          EIGV(I,1,2) = (EVV(I,1)-EPSXX(I)) /ROOTV(I)
          EIGV(I,2,2) = (EVV(I,1)-EPSYY(I)) /ROOTV(I)
          EIGV(I,3,1) = (HALF*EPSXY(I))   /ROOTV(I)
          EIGV(I,3,2) =-(HALF*EPSXY(I))   /ROOTV(I)
        ENDIF
      ENDDO
      IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN  ! engineering strain
        DO I=1,NEL
          EV(I,1)=EVV(I,1)+ ONE
          EV(I,2)=EVV(I,2)+ ONE
          EV(I,3)=ONE/EV(I,1)/EV(I,2)
        ENDDO
      ELSEIF(ISMSTR == 10) THEN
        DO I=1,NEL
          EV(I,1)=SQRT(EVV(I,1)+ ONE)
          EV(I,2)=SQRT(EVV(I,2)+ ONE)
          EV(I,3)=ONE/EV(I,1)/EV(I,2)!initialization before plane stress iterations
        ENDDO
      ELSE  ! true strain
        DO I=1,NEL
          EV(I,1)=EXP(EVV(I,1))
          EV(I,2)=EXP(EVV(I,2))
          EV(I,3)=ONE/EV(I,1)/EV(I,2)
        ENDDO
      ENDIF
C-----------------------------------------------
C  Calcul de la tempurature. (conduction ou adiabatique)
C--------------------    
      IF(JTHE < 0 ) THEN
       DO I=1,NEL     
           TEMP(I) = TEMPEL(I)
       ENDDO
      ELSE         
       DO I=1,NEL
           TEMP(I) = TINI
     .             +(EINT(I,1)+ EINT(I,2))/VOL(I)/ RHO0(I)/CP
       ENDDO
      ENDIF
      !compute limits for start and end of transformation
      RSAS = YLD_ASS *(SQDT+ALPHA)-CAS*TSAS !start from aust to martensite
      RFAS = YLD_ASF *(SQDT+ALPHA)-CAS*TFAS !end from aust to martensite
      RSSA = YLD_SAS *(SQDT+ALPHA)-CSA*TSSA !start from martensite to austenite
      RFSA = YLD_SAF *(SQDT+ALPHA)-CSA*TFSA !end from martensite to austenite
      !---------------------------------------------- 
      CRIT_LOOP(1:NEL) = 0
      DO I=1,NEL
           INDX_LOOP(I) = I
           INDX_LOOP_LOC(I) = 0
      ENDDO
      NINDX_LOOP = NEL
c---------------------------------------------- 
c---------------------------------------------- 
c        DEBUT BOUCLE 1 SUR K plane stress
c---------------------------------------------- 
c---------------------------------------------- 
      DO KK=1,ITERK ! plane stress iteration
#include "vectorize.inc"
        DO II =1,NINDX_LOOP 
           I = INDX_LOOP(II)
           !DET A (A = GRADIENT OF DEFORMATION)
           DET(I) =EV(I,1)*EV(I,2)*EV(I,3)
           IF(DET(I)/=ZERO) THEN
                TRDE(I) = LOG(DET(I)) !TRACE OF DEFORMATION
                RV_PUI = EXP((-THIRD)*TRDE(I))
           ELSE
                RV_PUI = ZERO
                TRDE(I)= ZERO
           ENDIF
           EE1(I)  = LOG(EV(I,1)*RV_PUI) !DEVIATOR OF DEFORMATION
           EE2(I)  = LOG(EV(I,2)*RV_PUI)
           EE3(I)  = LOG(EV(I,3)*RV_PUI)
        ENDDO
        !==============================================================
        IF (EFLAG > ZERO)THEN !dependency on martensite fraction
        !==============================================================
#include "vectorize.inc"
         DO II =1,NINDX_LOOP
           I = INDX_LOOP(II) 
	      FM = UVAR(I,1)   ! fraction of Martensite    
           GT(I) = G + FM * (GM - G) !G_n
           KT(I) = K + FM * (KM - K) !K_n
          !pressure
           P11(I) = KT(I) * (TRDE(I) - THREE*ALPHA*EPSL*FM)
           ! n= e/ norm(e)
           NE = SQRT( EE1(I)**2 + EE2(I)**2 + EE3(I)**2 )
           NXX(I) =EE1(I)/MAX(NE,EM20) 
           NYY(I) =EE2(I)/MAX(NE,EM20)  
           NZZ(I) =EE3(I)/MAX(NE,EM20)  
 
!          Estimation dev(sigma_n+1)
           SXX(I)= TWO*GT(I)*(EE1(I) -EPSL*FM*NXX(I))
           SYY(I)= TWO*GT(I)*(EE2(I) -EPSL*FM*NYY(I))
           SZZ(I)= TWO*GT(I)*(EE3(I) -EPSL*FM*NZZ(I))

	      SV(I) =  SQRT( SXX(I)*SXX(I) + SYY(I)*SYY(I) + SZZ(I)*SZZ(I)  )
	      !-------------------
	      ! transformation  
	      !-------------------
           DFMSA = ZERO
           DFMAS = ZERO

           ! loading function
           FS(I) = SV(I) + THREE*ALPHA*P11(I) - CAS*TEMP(I) ! loading function A--> M
           !----------------------
           !   Check Austenite -----> martensite  (dependance tempurature a voir ...)
           FASS = FS(I) -RSAS 
           FASF = FS(I) -RFAS 
           FS0  = UVAR(I,2)
           BETA      = EPSL*(TWO*GT(I)+NINE*KT(I)*ALPHA*ALPHA)
           IF((FS(I) - FS0) > ZERO .AND. FASS > ZERO.AND. FASF < ZERO .AND. FM < ONE )THEN
           ! DFMAS = MIN(ONE, -(FS-FS0)*(ONE-FM)/(FASF-BETA*(ONE-FM) )  ) ! (should be positive)
            DB    = (TWO * (GM-G) +NINE*ALPHA*ALPHA*(KM-K)) *EPSL
            UNMXN = ONE - FM
            DFTR  = TWO*NE*(GM-G) + THREE*ALPHA*TRDE(I)*(KM-K)
            DFMAS = MIN(ONE, -(FS(I)-FS0)*(ONE-FM)/(FASF-BETA*(ONE-FM) )  )
            A = UNMXN *DB 
            B = (RFAS-FS(I)+UNMXN*(BETA-DFTR))
            C =  UNMXN*(FS0 - FS(I))
            DO JJ = 1,3
             FCT  = DFMAS*DFMAS *A+ DFMAS*   B  +C
             FCTP = TWO*DFMAS *A+ B
             DFMAS = DFMAS - FCT / FCTP     
            ENDDO
            DFMAS = MIN(ONE,DFMAS  ) ! (should be positive)
           ENDIF


           !IF((FS(I) - FS0) > ZERO .AND. FASS > ZERO.AND. FASF < ZERO.AND. FM < ONE ) THEN!          
           !       DFMAS = MIN(ONE, -(FS(I)-FS0)*(ONE-FM)/(FASF-BETA*(ONE-FM) )  ) ! (should be positive)
           !ENDIF
           !----------------------
           !   Check marteniste -----> austenite  (dependance au tempurature a voir ...)
           !----------------------
           FS(I) = SV(I) + THREE*ALPHA*P11(I) - CSA*TEMP(I) ! Unloading function M--> A
           FSAS = FS(I) - RSSA
           FSAF = FS(I) - RFSA
           FS0 = UVAR(I,3)
           IF((FS(I) - FS0) < ZERO .AND. FSAS < ZERO.AND. FSAF > ZERO )THEN 
             !DFMSA =  MAX(-ONE , FM * (FS - FS0)/ (FSAF+BETA*FM) )
            DB    = (TWO * (GM-G) +NINE*ALPHA*ALPHA*(KM-K))*EPSL
            DFTR  = TWO * (GM-G)*NE+ THREE*ALPHA*(KM-K)*TRDE(I)
            DFMSA = ZERO
            A = FM *DB 
            B = -(RFSA-FS(I)+FM*(DFTR-BETA))
            C =  -FM*(FS(I) - FS0)
            DO JJ = 1,3
             FCT  = DFMSA*DFMSA *A+ DFMSA*   B  +C
             FCTP = TWO*DFMSA *A+ B
             DFMSA = DFMSA - FCT / FCTP        
            ENDDO
            DFMSA =  MAX(-ONE , DFMSA )
           ENDIF
           !IF((FS(I) - FS0) < ZERO .AND. FSAS < ZERO .AND. FSAF > ZERO ) THEN                       
           !       DFMSA =  MAX(-ONE , FM * (FS(I) - FS0)/ (FSAF+BETA*FM) )  !compute marteniste fraction
           !ENDIF

                  
           DFM(I) =  DFMAS + DFMSA ! new martensite fraction 
           IF(DFM(I) < ZERO .AND. FM == ZERO) DFM(I) = ZERO
c

           !UPDATE
           DGT = DFM(I) * (GM - G)
           DKT = DFM(I) * (KM - K)
           SXX(I) =  SXX(I) -TWO*GT(I)* EPSL*DFM(I)*NXX(I) 
     .               +       TWO*DGT* (EE1(I)-EPSL*NXX(I)*DFM(I))
           SYY(I) =  SYY(I) -TWO*GT(I)* EPSL*DFM(I)*NYY(I) 
     .               +       TWO*DGT* (EE2(I)-EPSL*NYY(I)*DFM(I))
           SZZ(I) =  SZZ(I) -TWO*GT(I)* EPSL*DFM(I)*NZZ(I) 
     .               +       TWO*DGT* (EE3(I)-EPSL*NZZ(I)*DFM(I))

           P11(I) = P11(I) - KT(I)*EPSL*THREE*ALPHA*DFM(I)
     .          +   DKT *(TRDE(I) -EPSL*THREE*ALPHA*DFM(I))

           !KIRCHHOFF STRESS
           SIGXX(I)= SXX(I) + P11(I)
           SIGYY(I)= SYY(I) + P11(I)
           SIGZZ(I)= SZZ(I) + P11(I)

           SV(I) =  SQRT( SXX(I)*SXX(I) + SYY(I)*SYY(I) + SZZ(I)*SZZ(I) )         
           FS(I) = SV(I) + THREE*ALPHA*P11(I)
           DFS(I) = ZERO
           IF (DFMAS /= ZERO) DFS(I)     = ABS(FS(I)-CAS*TEMP(I) - FS0)
           IF (DFMSA /= ZERO) DFS(I)     = ABS(FS(I)-CSA*TEMP(I) - FS0)
           IF (DFS(I)   /= ZERO .AND. EPSL /= ZERO .AND. DFM(I)/= ZERO) THEN
             H       = DFS(I)/EPSL/DFM(I)
             !ETSE(I) = H / (H+YOUNG) for shells
             ETSE(I) = H /( (E + UVAR(I,1)*(EMART-E))  + H)
           ELSE
             ETSE(I) = ONE
           ENDIF
         ENDDO
         ! ----------------
#include "vectorize.inc"
         DO II =1,NINDX_LOOP
           I = INDX_LOOP(II)
           IF(DET(I)/=zero)THEN 
                   INVE = ONE/DET(I)
           ELSE
                   INVE = zero
           ENDIF
           !CAUCHY STRESS
           SIGXX(I)= SIGXX(I) *INVE
           SIGYY(I)= SIGYY(I) *INVE
           SIGZZ(I)= SIGZZ(I) *INVE
         ENDDO
        !==============================================================
        ELSE ! no  dependency martensite fraction
        !==============================================================
#include "vectorize.inc"
         DO II =1,NINDX_LOOP
           I = INDX_LOOP(II) 
	      FM = UVAR(I,1)   ! fraction of Martensite    
           !pressure
           P11(I) = K * (TRDE(I) - THREE*ALPHA*EPSL*FM)
           ! n= e/ norm(e)
           NE = SQRT( EE1(I)**2 + EE2(I)**2 + EE3(I)**2 )
           NXX(I) =EE1(I)/(NE+EM20) 
           NYY(I) =EE2(I)/(NE+EM20)  
           NZZ(I) =EE3(I)/(NE+EM20)  
 
           !Estimation dev(sigma_n+1)
           SXX(I)= G2*(EE1(I) -EPSL*FM*NXX(I))
           SYY(I)= G2*(EE2(I) -EPSL*FM*NYY(I))
           SZZ(I)= G2*(EE3(I) -EPSL*FM*NZZ(I))
	      SV(I) =  SQRT( SXX(I)*SXX(I) + SYY(I)*SYY(I) + SZZ(I)*SZZ(I)  )

           !-------------------
           ! transformation  
           !-------------------
           DFMSA = ZERO
           DFMAS = ZERO

           ! loading function
           FS(I) = SV(I) + THREE*ALPHA*P11(I) - CAS*TEMP(I) ! loading function A--> M
           !----------------------
           !Check Austenite -----> martensite  (dependance tempurature a voir ...)
           FASS = FS(I) -RSAS 
           FASF = FS(I) -RFAS 
           FS0 = UVAR(I,2)
           IF((FS(I) - FS0) > ZERO .AND. FASS > ZERO.AND. FASF < ZERO.AND. FM < ONE ) THEN!          
                  DFMAS = MIN(ONE, -(FS(I)-FS0)*(ONE-FM)/(FASF-BETA*(ONE-FM) )  ) ! (should be positive)
           ENDIF
           !----------------------
           !Check marteniste -----> austenite  (dependance au tempurature a voir ...)
           FS(I) = SV(I) + THREE*ALPHA*P11(I) - CSA*TEMP(I) ! Unloading function M--> A
           FSAS = FS(I) - RSSA
           FSAF = FS(I) - RFSA
           FS0 = UVAR(I,3)
           IF((FS(I) - FS0) < ZERO .AND. FSAS < ZERO .AND. FSAF > ZERO ) THEN                       
                  DFMSA =  MAX(-ONE , FM * (FS(I) - FS0)/ (FSAF+BETA*FM) )  !compute marteniste fraction
           ENDIF
                 
           DFM(I) =  DFMAS + DFMSA !new martensite fraction 
           IF(DFM(I) < ZERO .AND. FM == ZERO) DFM(I) = ZERO

           !UPDATE
	      SXX(I) =  SXX(I) -G2* EPSL*DFM(I)*NXX(I) 
	      SYY(I) =  SYY(I) -G2* EPSL*DFM(I)*NYY(I)
	      SZZ(I) =  SZZ(I) -G2* EPSL*DFM(I)*NZZ(I)
           P11(I) =  P11(I) - K*EPSL*THREE*ALPHA*DFM(I)
           SIGXX(I)= SXX(I) + P11(I)
           SIGYY(I)= SYY(I) + P11(I)
           SIGZZ(I)= SZZ(I) + P11(I)
           SV(I) =  SQRT( SXX(I)*SXX(I) + SYY(I)*SYY(I) + SZZ(I)*SZZ(I) )         
           FS(I) = SV(I) + THREE*ALPHA*P11(I)
           DFS(I) = ZERO
           IF (DFMAS /= ZERO) DFS(I)     = ABS(FS(I)-CAS*TEMP(I) - FS0)
           IF (DFMSA /= ZERO) DFS(I)     = ABS(FS(I)-CSA*TEMP(I) - FS0)
           IF (DFS(I) /= ZERO .AND. EPSL /= ZERO.AND.DFM(I)/= ZERO) THEN
               H       = DFS(I)/EPSL/DFM(I)
               ETSE(I) = H /(E + H)
           ELSE
               ETSE(I) = ONE
           ENDIF
         ENDDO
         ! ----------------
#include "vectorize.inc"
         DO II =1,NINDX_LOOP
           I = INDX_LOOP(II)
           IF(DET(I)/=zero)THEN 
                   INVE = ONE/DET(I)
           ELSE
                   INVE = zero
           ENDIF
           !CAUCHY STRESS
           SIGXX(I)= SIGXX(I) *INVE
           SIGYY(I)= SIGYY(I) *INVE
           SIGZZ(I)= SIGZZ(I) *INVE
         ENDDO

       !==============================================================
        ENDIF ! dependency on martensite fraction
       !==============================================================

        !-----------------------------------------------------
        !check plane stress condition
        !-----------------------------------------------------
        NINDX_LOOP_LOC = 0         
        DO II =1,NINDX_LOOP
           I = INDX_LOOP(II)
           IF(ABS(SIGZZ(I))>EM20.OR.KK< 3)THEN !  CRITERE           
                IF (KK == 1) THEN                                       
                     EPSIM1(I) = EV(I,3)                                      
                     EV(I,3)   = EV(I,3) /TWO                     
                     SIGZIM1(I) = SIGZZ(I) 
                ELSE       
                     TEST =  SIGZZ(I)-SIGZIM1(I)                                           
                     EPSI(I) = EV(I,3)                               
                     IF (TEST/=ZERO)THEN
                         EV(I,3) = EV(I,3)-SIGZZ(I) *(EV(I,3)-EPSIM1(I))/  
     .                               (SIGZZ(I)-SIGZIM1(I))
                     ELSE
                         EV(I,3) = EV(I,3)-SIGZZ(I) *(EV(I,3)-EPSIM1(I))/  
     .                                   EM10       
                     ENDIF
                     EPSIM1(I) = EPSI(I)                                
                     SIGZIM1(I) = SIGZZ(I)       
                    !COMPUTE SECANT EXPRESSION                            
                ENDIF
                NINDX_LOOP_LOC = NINDX_LOOP_LOC + 1
                INDX_LOOP_LOC(NINDX_LOOP_LOC) = I             
           ENDIF
        ENDDO   ! 1:NEL
        ! ----------------
        NINDX_LOOP = NINDX_LOOP_LOC
        INDX_LOOP(1:NINDX_LOOP) = INDX_LOOP_LOC(1:NINDX_LOOP_LOC)

      ! -------------- -------------------------------
      ENDDO  ! plane stress iteration  
      ! ----------------------------------------------


C ---------------update UVAR -------------------------------
      UVAR(1:NEL,1) = UVAR(1:NEL,1) + DFM(1:NEL)
      UVAR(1:NEL,1) = MAX(ZERO, UVAR(1:NEL,1))
      UVAR(1:NEL,2) = FS(1:NEL)- CAS*TEMP(1:NEL)
      UVAR(1:NEL,3) = FS(1:NEL)- CSA*TEMP(1:NEL)
C ----------------------------------------------------------
      DO I=1,NEL
        EPSZZ(I) =EV(I,3) - ONE ! left gauchy-green strain
      ENDDO
C     TRANSFORM PRINCIPAL  CAUCHY STRESSES TO GLOBAL DIRECTIONS
      SIGNXX(1:NEL) = EIGV(1:NEL,1,1)*SIGXX(1:NEL)+EIGV(1:NEL,1,2)*SIGYY(1:NEL)  
      SIGNYY(1:NEL) = EIGV(1:NEL,2,1)*SIGXX(1:NEL)+EIGV(1:NEL,2,2)*SIGYY(1:NEL) 
      SIGNXY(1:NEL) = EIGV(1:NEL,3,1)*SIGXX(1:NEL)+EIGV(1:NEL,3,2)*SIGYY(1:NEL) 
      SIGNYZ(1:NEL) = SIGOYZ(1:NEL)  + GS(1:NEL)*DEPSYZ(1:NEL)
      SIGNZX(1:NEL) = SIGOZX(1:NEL)  + GS(1:NEL)*DEPSZX(1:NEL)
      THK(1:NEL)    = THK(1:NEL)     + (EPSZZ(1:NEL)-UVAR(1:NEL,4))*THKLY(1:NEL)*OFF(1:NEL)

C ---------------update UVAR ---------------------
      UVAR(1:NEL,1)  = MIN(ONE, UVAR(1:NEL,1))
      UVAR(1:NEL,10) = EPSXX(1:NEL)
      UVAR(1:NEL,11) = TEMP(1:NEL)
      UVAR(1:NEL,4)  = EPSZZ(1:NEL)
      UVAR(1:NEL,8)  = SIGZZ(1:NEL)
c       sound velocity     
      VISCMAX(1:NEL) = ZERO
      DO I=1,NEL
        SSPSH  = SQRT( E /(ONE - NU*NU)/RHO0(I) )
        SSPSOL = SQRT( AA1/RHO0(I) )
        SOUNDSP(I) =MAX(SSPSH ,SSPSOL)
      ENDDO  
C------------------------------------------
C------------------------------------------

      RETURN
      END SUBROUTINE SIGEPS71C
