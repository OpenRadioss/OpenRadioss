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
Chd|  SIGEPS57C                     source/materials/mat/mat057/sigeps57c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS57C(
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,
     2     NPF    ,NPT    ,IPT     ,IFLAG   ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,SHF     ,ETSE    ,SIGY   ,
     C     HARDM  ,SEQ_OUTPUT,ISRATE,EPSP   ,INLOC  ,
     D     DPLANL )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "com01_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C NPT     |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C NGL     | NEL     | I | R | ELEMENT NUMBER
C SHF     | NEL     | F | R | SHEAR FACTOR
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C AREA    | NEL     | F | R | AREA
C EINT    | 2*NEL   | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C THKLY   | NEL     | F | R | LAYER THICKNESS
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
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
C THK     | NEL     | F |R/W| THICKNESS
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUVAR, NPT, IPT,IFLAG(1),
     .   NGL(NEL), NUPARAM,ISRATE,INLOC
      my_real TIME,TIMESTEP
      my_real UPARAM(NUPARAM),
     .   AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THKLY(NEL),PLA(NEL),SHF(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   SEQ_OUTPUT(NEL),EPSP(NEL),DPLANL(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL),SIGY(NEL),HARDM(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .    UVAR(NEL,NUVAR), OFF(NEL),THK(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
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
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NRATE,J1,J2,N,NINDX,NMAX,ITER,NITER,
     .        IAD1(NEL),IPOS1(NEL),ILEN1(NEL),NS,
     .        IAD2(NEL),IPOS2(NEL),ILEN2(NEL),OPTE,
     .        JJ(NEL),INDEX(NEL),NBMAX,NPARAM,IDFUNCE,NINC
      PARAMETER (NPARAM=5)
      my_real A, DEZZ, EPST,NU31,DR0,NU
      my_real DG(NEL),SIGY_O(NEL),SIGY_K(NEL),
     .        E(NEL), A1(NEL),A2(NEL),G(NEL), G3(NEL), 
     .        MC(NEL,4),MD(NEL),A_XY(NEL),PARAM(NEL,NPARAM),
     .        A01, A02, A03, A12,
     .        DYDX1(NEL),DYDX2(NEL),RATE(NEL,2),SVM(NEL),
     .        YLD(NEL), Y1(NEL), Y2(NEL),FRATE(NEL),
     .        YFAC(NEL,2), NNU1(NEL), 
     .        SXX(NEL),SYY(NEL),SXY(NEL),
     .        XXX(NEL),XYY(NEL),XXY(NEL),ALPHA(NEL),
     .        FAIL(NEL), EPSMAX,
     .        EPSR1 ,EPSR2 ,S11(NEL), S22(NEL),
     .        B_1(NEL) ,B_2(NEL) ,B_3(NEL) ,Q12(NEL),
     .        H(NEL),GS(NEL),FISOKIN ,HK(NEL) ,FHK ,M,
     .        CE,EINF, ESCALE(NEL),C1(NEL),DYDXE(NEL),YLDN(NEL),NORM,NORM1

      my_real SEFF,DK1_DSIGXX,DK1_DSIGYY,DK2_DSIGXX,DK2_DSIGYY,DF_DK1,
     .        DF_DK2,NORMXX,NORMYY,DK2_DSIGXY,NORMXY,SIG_DFDSIG,
     .        DDLAM,DYLD_DPLA,DF_DLAM,DFDSIG_N, DPLA_DLAM,DYLD_DLAM
      my_real, DIMENSION(NEL) :: K1,K2,DEPLZZ,DEELZZ,DPLA,DEPLXX,DEPLYY,DEPLXY , YLD0,
     .                            PHI,DPHI,DDEP,DYDX,DEPSZZ
C
      DATA NMAX/4/,NS/10/,NBMAX/3/
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
        A01 = UPARAM(7)
C------- A02 = C*2^M  !!!!
        A02 = UPARAM(8)
        A03 = UPARAM(9)
        A12 = UPARAM(10)
        NU  = UPARAM(6)   
        NRATE = NINT(UPARAM(1))
        M   = UPARAM(NS+2*NRATE+4)
        EPSMAX=UPARAM(NS+2*NRATE+1)

       DO I=1,NEL
        E(I)   = UPARAM(2)
        A1(I)  = UPARAM(3)
        A2(I)  = UPARAM(4)
        G(I)   = UPARAM(5)
        GS(I)  = G(I)*SHF(I)
        G3(I)  = UPARAM(NS+2*NRATE+5)
        NNU1(I)   = NU  / (ONE - NU)
       ENDDO
C       
       IF(EPSMAX==ZERO)THEN
         IF(TF(NPF(IFUNC(1)+1)-1)==ZERO)THEN
          EPSMAX=TF(NPF(IFUNC(1)+1)-2)
         ELSE
          EPSMAX= EP30
         ENDIF
       ENDIF
c------------------------------------------------------
       EPSR1  = UPARAM(NS+2*NRATE+2)
       EPSR2  = UPARAM(NS+2*NRATE+3)
       FISOKIN= UPARAM(NS+2*NRATE+8)
       OPTE = UPARAM(NS+2*NRATE+10)
       EINF = UPARAM(NS+2*NRATE+11)
       CE   = UPARAM(NS+2*NRATE+12)

       IF (OPTE == 1)THEN   
         IDFUNCE = UPARAM(NS+2*NRATE+9)
         DO I=1,NEL    
          IF(PLA(I) > ZERO)THEN                                                        
             ESCALE(I) = FINTER(IFUNC(IDFUNCE),PLA(I),NPF,TF,DYDXE(I))   
          ENDIF
         ENDDO  

         DO I=1,NEL    
          IF(PLA(I) > ZERO)THEN 
             E(I) =  ESCALE(I)* E(I)   
             A1(I) = E(I)/(ONE - NU *NU )
             A2(I) = NU *A1(I) 
             G(I) =  HALF*E(I)/(ONE+NU ) 
             GS(I)   =  G(I)*SHF(I)                                 
             G3(I) = THREE*G(I)                   
           ENDIF  
         ENDDO                                                                                      
        ELSEIF ( CE /= ZERO) THEN      

         DO I=1,NEL                                   
           IF(PLA(I) > ZERO)THEN                                                        
             E(I) = E(I)-(E(I)-EINF)*(ONE-EXP(-CE*PLA(I))) 
             A1(I) = E(I)/(ONE - NU *NU )
             A2(I) = NU *A1(I)                                             
             G(I) =  HALF*E(I)/(ONE+NU )   
             GS(I)   =  G(I)*SHF(I)                                 
             G3(I) = THREE*G(I)                                               
           ENDIF     
         ENDDO 
      ENDIF
      NORM =one !EP03/UPARAM(2) !for SP

      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
         ! Trial stress tensor
         SIGNXX(I) = SIGOXX(I) + A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I) 
         SIGNYY(I) = SIGOYY(I) + A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
         SIGNXY(I) = SIGOXY(I) + G(I) * DEPSXY(I)
         SIGNYZ(I) = SIGOYZ(I) + GS(I) * DEPSYZ(I)
         SIGNZX(I) = SIGOZX(I) + GS(I) * DEPSZX(I)
      ENDDO
      DO I=1,NEL
         ! Sound speed and tangent modulus
         SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE(I)    = ONE
         DPLA(I)    = ZERO
         DEPSZZ(I)  = ZERO
         DEPLZZ(I)  = ZERO 
      ENDDO
               
      DO I=1,NEL
        K1(I)   = (SIGNXX(I) + A03*SIGNYY(I))*NORM/TWO
        K2(I)   = SQRT(((SIGNXX(I) - A03*SIGNYY(I))/TWO)**TWO + (A12*SIGNXY(I))**TWO)*NORM
        SVM(I)  = A01*ABS(K1(I)+K2(I))**M + A01*ABS(K1(I)-K2(I))**M + A02*ABS(K2(I))**M
        SVM(I)  = (HALF*MAX(SVM(I) ,EM20))**(ONE/M)
      ENDDO                  
                                        
c--------------------------------------------------------------
C-------------------
C     STRAIN RATE
C-------------------
      IF (ISRATE == 0) THEN 
        DO I=1,NEL
         EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .               + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                      + EPSPXY(I)*EPSPXY(I) ) )
        ENDDO                  
      ENDIF
C-------------------
C     STRAIN 
C-------------------
      DO I=1,NEL
        EPST   = HALF*( EPSXX(I)+EPSYY(I)
     .          + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
        FAIL(I) = ONE!MAX(ZERO,MIN(ONE,(EPSR2-EPST)/(EPSR2-EPSR1)))
        
      ENDDO                  
C-------------------
C     HARDENING LAW
C-------------------
      DO I=1,NEL
         JJ(I) = 1
      ENDDO
      DO J=2,NRATE-1
        DO I=1,NEL
          IF(EPSP(I)>=UPARAM(NS+J)) JJ(I) = J
        ENDDO
      ENDDO
#include "vectorize.inc"
      DO I=1,NEL
        RATE(I,1)=UPARAM(NS+JJ(I))
        RATE(I,2)=UPARAM(NS+JJ(I)+1)
        YFAC(I,1)=UPARAM(NS+NRATE+JJ(I))
        YFAC(I,2)=UPARAM(NS+NRATE+JJ(I)+1)
      ENDDO
#include "vectorize.inc"
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        IPOS1(I) = NINT(UVAR(I,J1+4))
        IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1
        ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I) - IPOS1(I)
        IPOS2(I) = NINT(UVAR(I,J2+4))
        IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1
        ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I) - IPOS2(I)
      ENDDO
C
      CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
      CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
C
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        UVAR(I,J1+4) = IPOS1(I)
        UVAR(I,J2+4) = IPOS2(I)
      ENDDO
#include "vectorize.inc"
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        Y1(I)=Y1(I)*YFAC(I,1)
        Y2(I)=Y2(I)*YFAC(I,2)
        FRATE(I) = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
        YLD(I) = FAIL(I)*(Y1(I)    + FRATE(I)*(Y2(I)-Y1(I)))
        YLD(I) = MAX(YLD(I),EM20)
        SIGY(I) = YLD(I)
        DYDX(I) = FAIL(I)*(DYDX1(I) + FRATE(I)*(DYDX2(I)-DYDX1(I)))
C       ECROUISSAGE CINEMATIQUE
        Y1(I)=TF(NPF(IFUNC(J1))+1)
        Y2(I)=TF(NPF(IFUNC(J2))+1)
        YLD0(I) = FAIL(I)*(Y1(I)  + FRATE(I) * ( Y2(I)-Y1(I) ))
        YLD(I) = (ONE -FISOKIN) * YLD(I) + FISOKIN * YLD0(I)      
      ENDDO
      !------------------------------------------
      ! Yield function value
      !------------------------------------------
      DO I=1,NEL
            PHI(I)    = SVM(I) /NORM - YLD(I) 
       ENDDO
c
      !========================================================================
      ! - CHECKING PLASTIC BEHAVIOR
      !========================================================================
      NINDX = 0
      DO I=1,NEL        
        IF (PHI(I) >= ZERO .AND. OFF(I) == ONE) THEN
          NINDX = NINDX+1
          INDEX(NINDX) = I
        ENDIF
      ENDDO
      ! Number of Newton iterations
      NITER = 3
c
      DO ITER = 1,NITER
#include "vectorize.inc"
        ! Loop over yielding elements 
        DO J=1,NINDX
          ! Number of the element with plastic behaviour   
          I = INDEX(J)         
          J1 = JJ(I)
          J2 = J1+1
c        
            !  Computation of the derivatives
          DF_DK1 = ((SVM(I)/NORM)**(1-M))*(A01/TWO)*(
     .                           +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(M-1)) 
     .                           +  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(M-1)))
  
          DF_DK2 = ((SVM(I)/NORM)**(1-M))*((A01/TWO)*(
     .                           +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(M-1)) 
     .                           -  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(M-1))) 
     .                           +  A02/TWO*(ABS(K2(I))**(M-1)))
          DK1_DSIGXX = HALF 
          DK1_DSIGYY = A03/TWO
          DK2_DSIGXX = (SIGNXX(I)-A03*SIGNYY(I))*NORM/(MAX(FOUR*K2(I),EM20))
          DK2_DSIGYY = -A03*(SIGNXX(I)-A03*SIGNYY(I))*NORM/(MAX(FOUR*K2(I),EM20))
          DK2_DSIGXY = (A12**TWO)*SIGNXY(I)*NORM/MAX(K2(I),EM20)
          !  Assembling the normal 
          NORMXX     = DF_DK1*DK1_DSIGXX+ DF_DK2*DK2_DSIGXX
          NORMYY     = DF_DK1*DK1_DSIGYY+ DF_DK2*DK2_DSIGYY
          NORMXY     = DF_DK2*DK2_DSIGXY


          ! df/ddlam = DF/DSIG * DSIG/Ddlam ( = df_dsig = N *Cel * N)
          DF_DLAM  = NORMXX * (A1(I)*NORMXX  +  A2(I)*NORMYY)
     .          +    NORMYY * (A1(I)*NORMYY + A2(I)*NORMXX)
     .          +    NORMXY * NORMXY * G(I)

          !compute derivative of effective plastic strain vs dlam
          DFDSIG_N   =  SIGNXX(I) * NORMXX
     .                + SIGNYY(I) * NORMYY
     .                + SIGNXY(I) * NORMXY 
          DPLA_DLAM  = DFDSIG_N / YLD(I)
          !derivative of YLD to dlam
          DYLD_DPLA  = DYDX(I)
          DYLD_DLAM  = DYLD_DPLA * DPLA_DLAM


          !d phi/ddlam
          DPHI(I) = - DF_DLAM - DYLD_DLAM
          DPHI(I) = SIGN(MAX(ABS(DPHI(I)),EM20) ,DPHI(I)) 

           ! Plastic multiplier increment
          DDLAM   = -PHI(I)/DPHI(I)     
          
          ! Plastic strains tensor update              
          DEPLXX(I) = DDLAM*NORMXX
          DEPLYY(I) = DDLAM*NORMYY
          DEPLXY(I) = DDLAM*NORMXY

c
          ! Cumulated plastic strain update
          DDEP(I)   = DDLAM * DPLA_DLAM
          PLA(I)    = PLA(I) + DDEP(I)
c
          ! Total plastic strain increment on the time step
          DPLA(I) = DPLA(I) + DDEP(I) 
          DPLA(I) = MAX(DPLA(I),ZERO)
c
          ! Cauchy stress tensor update              
          SIGNXX(I) = SIGNXX(I) - A1(I)*DEPLXX(I) - A2(I)*DEPLYY(I)                
          SIGNYY(I) = SIGNYY(I) - A2(I)*DEPLXX(I) - A1(I)*DEPLYY(I)               
          SIGNXY(I) = SIGNXY(I) - G(I)*DEPLXY(I)

          K1(I)      = (SIGNXX(I) + A03*SIGNYY(I))*NORM/TWO
          K2(I)      = SQRT(((SIGNXX(I) - A03*SIGNYY(I))/TWO)**TWO + (A12*SIGNXY(I))**TWO)*NORM
          SVM(I)  = A01*ABS(K1(I)+K2(I))**M + A01*ABS(K1(I)-K2(I))**M + A02*ABS(K2(I))**M
          SVM(I)  = (HALF*MAX(SVM(I) ,EM20))**(ONE/M)
          YLD(I) = YLD(I) + DYDX(I)*DDEP(I) 
          PHI(I) = SVM(I)/NORM - YLD(I) 
          ! Thickness plastic strain update
          DEPLZZ(I) = DEPLZZ(I) - (DEPLXX(I)+DEPLYY(I))

        ENDDO
      ENDDO
      DO I=1,NEL
         IF(DPLA(I) > ZERO) THEN
              H(I) = (YLD(I)-UVAR(I,2))/DPLA(I)
              ETSE(I) = H(I)/(H(I)+E(I))
         ELSE
              ETSE(I) = ONE 
         ENDIF        
         DEELZZ(I ) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E(I)
         UVAR(I,2)  = YLD(I)
         SEQ_OUTPUT(I) = SVM(I)/NORM
      ENDDO


!-------------------------------------------------------------
!     NON-LOCAL THICKNESS VARIATION
!-------------------------------------------------------------
      IF (INLOC > 0) THEN
        DO I = 1,NEL
          ! Computation of the normal to the yield surface
          K1(I)      = (SIGNXX(I) + A03*SIGNYY(I))*NORM/TWO
          K2(I)      = SQRT(((SIGNXX(I) - A03*SIGNYY(I))/TWO)**TWO + (A12*SIGNXY(I))**TWO)*NORM
          SEFF    = A01*ABS(K1(I)+K2(I))**M + A01*ABS(K1(I)-K2(I))**M + A02*ABS(TWO*K2(I))**M
          SEFF    = (HALF*MAX(SEFF,EM20))**(ONE/M)
          DF_DK1  = (SEFF**(1-M))*(A01/TWO)*(
     .              +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(M-1)) 
     .              +  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(M-1)))
          DF_DK2  = (SEFF**(1-M))*((A01/TWO)*(
     .              +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(M-1)) 
     .              -  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(M-1))) 
     .              +  A02/TWO*(ABS(K2(I))**(M-1)))
          DK1_DSIGXX = HALF
          DK1_DSIGYY = A03/TWO
          DK2_DSIGXX = (SIGNXX(I)-A03*SIGNYY(I))*NORM/(MAX(FOUR*K2(I),EM20))
          DK2_DSIGYY = -A03*(SIGNXX(I)-A03*SIGNYY(I))*NORM/(MAX(FOUR*K2(I),EM20))
          DK2_DSIGXY = (A12**TWO)*SIGNXY(I)*NORM/MAX(K2(I),EM20)
          NORMXX     = DF_DK1*DK1_DSIGXX + DF_DK2*DK2_DSIGXX
          NORMYY     = DF_DK1*DK1_DSIGYY + DF_DK2*DK2_DSIGYY
          NORMXY     = DF_DK2*DK2_DSIGXY
          SIG_DFDSIG = NORMXX*SIGNXX(I) + NORMYY*SIGNYY(I) + NORMXY*SIGNXY(I)
          ! Updating the thickness
          DEZZ       = -NU *((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E(I))
          IF (SIG_DFDSIG /= ZERO) THEN
            DEZZ     = DEZZ - MAX(DPLANL(I),ZERO)*(SEFF/SIG_DFDSIG)*(NORMXX + NORMYY)
          ENDIF
          THK(I)     = THK(I) + DEZZ*THKLY(I)*OFF(I)   
        ENDDO  
      ELSE 
        DO I = 1,NEL
            DEPSZZ(I)  = DEELZZ(I) + DEPLZZ(I)
            THK(I)     = THK(I) + DEPSZZ(I)*THKLY(I)*OFF(I)
            ENDDO
      ENDIF
!------------------------- 
      RETURN
      END
c============================================================================
c============================================================================