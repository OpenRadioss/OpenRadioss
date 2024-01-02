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
Chd|  SIGEPS60C                     source/materials/mat/mat060/sigeps60c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        INTER_RAT_V                   source/materials/mat/mat060/sigeps60c.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS60C(
     1     NEL0    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC  ,
     2     NPF    ,NPT0    ,IPT     ,IFLAG   ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,YLD    ,EPSP    ,ISRATE  ,DPLA_I ,
     D     SHF    ,INLOC  ,DPLANL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
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
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL0),MAT(NEL0),ISRATE,IPM(NPROPMI,*),INLOC
      my_real TIME,TIMESTEP,UPARAM(*),
     .   AREA(NEL0),RHO0(NEL0),EINT(NEL0,2),
     .   THKLY(NEL0),PLA(NEL0),
     .   EPSPXX(NEL0),EPSPYY(NEL0),
     .   EPSPXY(NEL0),EPSPYZ(NEL0),EPSPZX(NEL0),
     .   DEPSXX(NEL0),DEPSYY(NEL0),
     .   DEPSXY(NEL0),DEPSYZ(NEL0),DEPSZX(NEL0),
     .   EPSXX(NEL0) ,EPSYY(NEL0) ,
     .   EPSXY(NEL0) ,EPSYZ(NEL0) ,EPSZX(NEL0) ,
     .   SIGOXX(NEL0),SIGOYY(NEL0),
     .   SIGOXY(NEL0),SIGOYZ(NEL0),SIGOZX(NEL0),
     .   GS(*)       ,EPSP(NEL0),SHF(NEL0),
     .   DPLANL(NEL0)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL0),SIGNYY(NEL0),
     .    SIGNXY(NEL0),SIGNYZ(NEL0),SIGNZX(NEL0),
     .    SIGVXX(NEL0),SIGVYY(NEL0),
     .    SIGVXY(NEL0),SIGVYZ(NEL0),SIGVZX(NEL0),
     .    SOUNDSP(NEL0),VISCMAX(NEL0),ETSE(NEL0),
     .     DPLA_I(NEL0)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL0,NUVAR), OFF(NEL0),THK(NEL0),YLD(NEL0)
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
      INTEGER I,J,NRATE(MVSIZ),J1(MVSIZ),J2(MVSIZ),N,NINDX,NMAX,IADBUF,NFUNC,
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        IAD3(MVSIZ),IPOS3(MVSIZ),ILEN3(MVSIZ),
     .        IAD4(MVSIZ),IPOS4(MVSIZ),ILEN4(MVSIZ),
     .        JJ(MVSIZ),INDEX(MVSIZ),IFUNC(MVSIZ,100),NRATEM,
     .        NRATE1,IFUNC1(100),IADBUFV(MVSIZ),NFUNCV(MVSIZ),
     .        NFUNCM, J3(MVSIZ), J4(MVSIZ), NN, INDEX1,MOPTE,MEINF,MCE,IFUNCE
      my_real 
     .        E(MVSIZ),A1(MVSIZ),A2(MVSIZ),G(MVSIZ),G3(MVSIZ),
     .        DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ,4),SVM(MVSIZ),
     .        Y1(MVSIZ),Y2(MVSIZ),DR(MVSIZ),
     .        YFAC(MVSIZ,4),NNU1(MVSIZ),NU1(MVSIZ),
     .        NU2(MVSIZ),NU3(MVSIZ),NU4(MVSIZ),NU5(MVSIZ),NU6(MVSIZ),
     .        AA(MVSIZ),BB(MVSIZ),DPLA_J(MVSIZ),     
     .        PP(MVSIZ),QQ(MVSIZ),FAIL(MVSIZ),SVMO(MVSIZ),H(MVSIZ),
     .        EPSMAX(MVSIZ),EPSR1(MVSIZ),EPSR2(MVSIZ),FISOKIN(MVSIZ),
     .        SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEXY(MVSIZ),
     .        HK(MVSIZ),NU(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .        DYDX3(MVSIZ),DYDX4(MVSIZ),EINF(MVSIZ),
     .        CE(MVSIZ),ESCALE(MVSIZ),E0(MVSIZ),DYDXE(MVSIZ)
      my_real
     .        R,UMR,A,B,C,AMU,S11,S22,S12,P,P2,FAC,DEZZ,
     .        SIGZ,S1,S2,S3,DPLA,VM2,EPST,NNU2,CE1, EINF1,
     .        ERR,F,DF,PLA_I,Q2,YLD_I,SIGPXX,SIGPYY,SIGPXY,
     .        ALPHA,
     .        E1,A11,A21,G1,G31,NNU11,NU11,NU21,NU31,NU41,NU51,NU61,
     .        EPSMAX1,EPSR11,EPSR21,FISOKIN1,
     .        DSXX,DSYY,DSXY,DEXX,DEYY,DEXY,NUX, X(MVSIZ)
      my_real
     .        ME, MA1, MA2, MG, MNU,
     .        MEPSMAX,MEPSR1,MEPSR2,MFISOKIN, 
     .        Y11(MVSIZ), Y22(MVSIZ), Y33(MVSIZ), Y44(MVSIZ), Y(MVSIZ),YP(MVSIZ),
     .        X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ)
      INTEGER JST(MVSIZ+1), IC, MNRATE, OPTE1,OPTE(MVSIZ)
C
      DATA NMAX/3/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
       NFUNC  = IPM(10,MAT(1))
       DO J=1,NFUNC
         IFUNC1(J)=IPM(10+J,MAT(1))
       ENDDO
C
       IADBUF = IPM(7,MAT(1)) - 1
       E1   = UPARAM(IADBUF+2)
       A11  = UPARAM(IADBUF+3)
       A21  = UPARAM(IADBUF+4)
       G1   = UPARAM(IADBUF+5)
       G31  = 3.*G1
       NUX  = UPARAM(IADBUF+6)
       NRATE1 = NINT(UPARAM(IADBUF+1))
       EPSMAX1=UPARAM(IADBUF+6+2*NRATE1+1)
c-----------------------------
       OPTE1 = UPARAM(IADBUF+2*NRATE1 + 18)
       EINF1 = UPARAM(IADBUF+2*NRATE1 + 19) 
       CE1 = UPARAM(IADBUF+2*NRATE1 + 20) 
c-----------------------------
       DO I=1,NEL0
        E(I) =  E1   
        A1(I) = A11                                                      
        A2(I) = A21                                                      
        G(I) =  G1                                                       
        G3(I) = G31  
       ENDDO
       IF (OPTE1 == 1)THEN     
        IFUNCE = UPARAM(IADBUF+2*NRATE1 + 17)                                                    
        DO I=1,NEL0                                                            
          IF(PLA(I) > ZERO)THEN   
             ESCALE(I) = FINTER(KFUNC(IFUNCE),PLA(I),NPF,TF,DYDXE(I))
          ENDIF
        ENDDO
        DO I=1,NEL0                                                            
          IF(PLA(I) > ZERO)THEN  
             E(I) =  ESCALE(I)* E1                                        
             A1(I) = E(I)/(ONE - NUX*NUX)                                      
             A2(I) = NUX*A1(I)                                                
             G(I) =  HALF*E(I)/(ONE+NUX)                                     
             G3(I) = THREE*G(I) 
             GS(I) = G(I) * SHF(I)                                                
           ENDIF                                                                        
          ENDDO                                                                          
       ELSEIF ( CE1 /= ZERO) THEN                                                        
          DO I=1,NEL0                                                                       
           IF(PLA(I) > ZERO)THEN                                                        
             E(I) = E1-(E1-EINF1)*(ONE-EXP(-CE1*PLA(I)))                      
             A1(I) = E(I)/(ONE - NUX*NUX)                                      
             A2(I) = NUX*A1(I)                                                
             G(I) =  HALF*E(I)/(ONE+NUX)                                     
             G3(I) = THREE*G(I) 
             GS(I) = G(I) * SHF(I)   
           ENDIF
          ENDDO
       ENDIF                                                                  
       IF(EPSMAX1==0.)THEN
         IF(TF(NPF(IFUNC1(1)+1)-1)==ZERO)THEN
           EPSMAX1=TF(NPF(IFUNC1(1)+1)-2)
         ELSE
           EPSMAX1= EP30
         ENDIF
       ENDIF
C
       NNU11 = NUX / (ONE - NUX)
       NNU2    = NNU11*NNU11
       NU11 = ONE/(ONE - NUX)
       NU21 = ONE/(ONE + NUX)
       NU31 = ONE - NNU11
       NU41 = ONE + NNU2 + NNU11
       NU51 = ONE + NNU2 - TWO*NNU11
       NU61 = HALF - NNU2 + HALF*NNU11
C
       EPSR11 =UPARAM(IADBUF+6+2*NRATE1+2)
       EPSR21 =UPARAM(IADBUF+6+2*NRATE1+3)
       FISOKIN1=UPARAM(IADBUF+6+2*NRATE1+8)
C
       IF (ISIGI==0) THEN
       IF(TIME==ZERO)THEN
         DO I=1,NEL0
           UVAR(I,1)=ZERO
           UVAR(I,2)=ZERO
           UVAR(I,3)=ZERO
           UVAR(I,4)=ZERO
         ENDDO
         DO J=1,NRATE1
          DO I=1,NEL0
             UVAR(I,J+4)=0
          ENDDO
         ENDDO
       ENDIF
       ENDIF
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
       DO I=1,NEL0
!           SIGOXX(I) = SIGOXX(I) - UVAR(I,2)
!           SIGOYY(I) = SIGOYY(I) - UVAR(I,3)
!           SIGOXY(I) = SIGOXY(I) - UVAR(I,4)
       ENDDO
C-----------------------------------------------
C
       DO I=1,NEL0
C
         SIGNXX(I)=SIGOXX(I) - UVAR(I,2) +A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) - UVAR(I,3) +A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I) - UVAR(I,4) +G(I) *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)+GS(I) *DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I)+GS(I) *DEPSZX(I)
         SIGEXX(I) = SIGNXX(I)
         SIGEYY(I) = SIGNYY(I)
         SIGEXY(I) = SIGNXY(I)
C
         SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
C
         IF (ISRATE==0) EPSP(I) = 
     .     HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
C-------------------
C     STRAIN 
C-------------------
C
         EPST = HALF*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
         FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR21-EPST)/(EPSR21-EPSR11)))
C
       ENDDO
C-------------------
C     CRITERE
C-------------------
       DO I=1,NEL0
         JJ(I) = 1
       ENDDO
       IADBUF = IPM(7,MAT(1)) - 1
C   inversion boucles
       DO J=2,NRATE1-1
         DO I=1,NEL0
           IF(EPSP(I)>=UPARAM(IADBUF+6+J)) JJ(I) = J
         ENDDO
       ENDDO
#include "vectorize.inc"
       DO I=1,NEL0
         IF(JJ(I)==1)THEN 
           J1(I) = JJ(I)
           J2(I) = J1(I)+1
           J3(I) = J2(I)+1
           J4(I) = J3(I)+1 
          ELSEIF(JJ(I)==(NRATE1-1))THEN
           J1(I) = JJ(I) - 2
           J2(I) = J1(I)+1 
           J3(I) = J2(I)+1 
           J4(I) = J3(I)+1 
          ELSE
           J1(I) = JJ(I) - 1
           J2(I) = J1(I)+1
           J3(I) = J2(I)+1
           J4(I) = J3(I)+1 
          ENDIF
       ENDDO
#include "vectorize.inc"
       DO I=1,NEL0
          RATE(I,1)=UPARAM(IADBUF + 6 +  J1(I) )
          RATE(I,2)=UPARAM(IADBUF + 6 +  J2(I) )
          RATE(I,3)=UPARAM(IADBUF + 6 +  J3(I) )
          RATE(I,4)=UPARAM(IADBUF + 6 +  J4(I) )
          YFAC(I,1)=UPARAM(IADBUF + 6 + NRATE1 + J1(I) )     
          YFAC(I,2)=UPARAM(IADBUF + 6 + NRATE1 + J2(I) )
          YFAC(I,3)=UPARAM(IADBUF + 6 + NRATE1 + J3(I) )
          YFAC(I,4)=UPARAM(IADBUF + 6 + NRATE1 + J4(I) )
        ENDDO        
C
#include "vectorize.inc"
       DO I=1,NEL0
        IF(JJ(I)==1)THEN 
          J1(I) = JJ(I)
          J2(I) = J1(I)+1
          J3(I) = J2(I)+1
          J4(I) = J3(I)+1 
         ELSEIF(JJ(I)==(NRATE1-1))THEN
          J1(I) = JJ(I) - 2
          J2(I) = J1(I)+1 
          J3(I) = J2(I)+1 
          J4(I) = J3(I)+1 
         ELSE
          J1(I) = JJ(I) - 1
          J2(I) = J1(I)+1
          J3(I) = J2(I)+1
          J4(I) = J3(I)+1 
         ENDIF
        ENDDO
#include "vectorize.inc"
       DO I=1,NEL0
         IPOS1(I) = NINT(UVAR(I,J1(I)+4))
         IAD1(I)  = NPF(IFUNC1(J1(I))) / 2 + 1
         ILEN1(I) = NPF(IFUNC1(J1(I))+1) / 2 - IAD1(I) - IPOS1(I)
         IPOS2(I) = NINT(UVAR(I,J2(I)+4))
         IAD2(I)  = NPF(IFUNC1(J2(I))) / 2 + 1
         ILEN2(I) = NPF(IFUNC1(J2(I))+1) / 2 - IAD2(I) - IPOS2(I)
         IPOS3(I) = NINT(UVAR(I,J3(I)+4))
         IAD3(I)  = NPF(IFUNC1(J3(I))) / 2 + 1
         ILEN3(I) = NPF(IFUNC1(J3(I))+1) / 2 - IAD3(I) - IPOS3(I)
         IPOS4(I) = NINT(UVAR(I,J4(I)+4))
         IAD4(I)  = NPF(IFUNC1(J4(I))) / 2 + 1
         ILEN4(I) = NPF(IFUNC1(J4(I))+1) / 2 - IAD4(I) - IPOS4(I)
       ENDDO
C
       CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL0,PLA,DYDX1,Y1)
       CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL0,PLA,DYDX2,Y2)
       CALL VINTER(TF,IAD3,IPOS3,ILEN3,NEL0,PLA,DYDX3,Y3)
       CALL VINTER(TF,IAD4,IPOS4,ILEN4,NEL0,PLA,DYDX4,Y4)
C
C
       IF (FISOKIN1==0.) THEN
!       ------------------------
#include "vectorize.inc"
        DO I=1,NEL0
          IF(JJ(I)==1)THEN 
            J1(I) = JJ(I)
            J2(I) = J1(I)+1
            J3(I) = J2(I)+1
            J4(I) = J3(I)+1
            DYDX1(I)= DYDX1(I)*YFAC(I,1)
            DYDX2 (I) = DYDX2(I)*YFAC(I,2)          
            FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1)) 
            H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
           ELSEIF(JJ(I)==(NRATE1-1))THEN
            J1(I) = JJ(I) - 2
            J2(I) = J1(I)+1 
            J3(I) = J2(I)+1 
            J4(I) = J3(I)+1
            DYDX3(I) = DYDX3(I)*YFAC(I,3)
            DYDX4(I) = DYDX4(I)*YFAC(I,4)
            FAC   = (EPSP(I) - RATE(I,3))/(RATE(I,4) - RATE(I,3)) 
             H(I)   = FAIL(I)*(DYDX3(I) + FAC*(DYDX4(I)-DYDX3(I)))  
           ELSE
            J1(I) = JJ(I) - 1
            J2(I) = J1(I)+1
            J3(I) = J2(I)+1
            J4(I) = J3(I)+1 
            DYDX2(I) = DYDX2(I)*YFAC(I,2)
            DYDX3(I) = DYDX3(I)*YFAC(I,3)
            FAC   = (EPSP(I) - RATE(I,2))/(RATE(I,3) - RATE(I,2))  
            H(I)   = FAIL(I)*(DYDX2(I) + FAC*(DYDX3(I)-DYDX2(I)))  
           ENDIF
        ENDDO
        DO I=1,NEL0
           UVAR(I,J1(I)+4) = IPOS1(I)
           UVAR(I,J2(I)+4) = IPOS2(I)
           UVAR(I,J3(I)+4) = IPOS3(I)
           UVAR(I,J4(I)+4) = IPOS4(I)
        ENDDO
        X1(1:NEL0) = RATE(1:NEL0,1)
        X2(1:NEL0) = RATE(1:NEL0,2)
        X3(1:NEL0) = RATE(1:NEL0,3)
        X4(1:NEL0) = RATE(1:NEL0,4)
        X(1:NEL0)  = EPSP(1:NEL0)
        Y11(1:NEL0) = Y1(1:NEL0)*YFAC(1:NEL0,1)
        Y22(1:NEL0) = Y2(1:NEL0)*YFAC(1:NEL0,2)
        Y33(1:NEL0) = Y3(1:NEL0)*YFAC(1:NEL0,3)
        Y44(1:NEL0) = Y4(1:NEL0)*YFAC(1:NEL0,4)
        CALL INTER_RAT_V(NEL0,X1,X2,X3,X4,Y11,Y22,Y33,Y44,
     .                       X,Y,YP,JJ, NRATE1)
        YLD(1:NEL0) = FAIL(1:NEL0)*Y(1:NEL0)
        YLD(1:NEL0) = MAX(YLD(1:NEL0),EM20)
!       ------------------------
       ELSEIF (FISOKIN1==ONE) THEN
#include "vectorize.inc"
        DO I=1,NEL0
           IF(JJ(I)==1)THEN 
            J1(I) = JJ(I)
            J2(I) = J1(I)+1
            J3(I) = J2(I)+1
            J4(I) = J3(I)+1
            DYDX1(I)= DYDX1(I)*YFAC(I,1)
            DYDX2 (I) = DYDX2(I)*YFAC(I,2)          
            FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1)) 
            H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
           ELSEIF(JJ(I)==(NRATE1-1))THEN
            J1(I) = JJ(I) - 2
            J2(I) = J1(I)+1 
            J3(I) = J2(I)+1 
            J4(I) = J3(I)+1
            DYDX3(I) = DYDX3(I)*YFAC(I,3)
            DYDX4(I) = DYDX4(I)*YFAC(I,4)
            FAC   = (EPSP(I) - RATE(I,3))/(RATE(I,4) - RATE(I,3)) 
             H(I)   = FAIL(I)*(DYDX3(I) + FAC*(DYDX4(I)-DYDX3(I)))  
           ELSE
            J1(I) = JJ(I) - 1
            J2(I) = J1(I)+1
            J3(I) = J2(I)+1
            J4(I) = J3(I)+1 
            DYDX2(I) = DYDX2(I)*YFAC(I,2)
            DYDX3(I) = DYDX3(I)*YFAC(I,3)
            FAC   = (EPSP(I) - RATE(I,2))/(RATE(I,3) - RATE(I,2))  
            H(I)   = FAIL(I)*(DYDX2(I) + FAC*(DYDX3(I)-DYDX2(I)))  
           ENDIF
        ENDDO
        DO I=1,NEL0  
           UVAR(I,J1(I)+4) = IPOS1(I)
           UVAR(I,J2(I)+4) = IPOS2(I)
           UVAR(I,J3(I)+4) = IPOS3(I)
           UVAR(I,J4(I)+4) = IPOS4(I)
        ENDDO
C       ECROUISSAGE CINEMATIQUE
        Y11(1:NEL0)=TF(NPF(IFUNC1(J1(1:NEL0)))+1)*YFAC(1:NEL0,1)
        Y22(1:NEL0)=TF(NPF(IFUNC1(J2(1:NEL0)))+1)*YFAC(1:NEL0,2)
        Y33(1:NEL0)=TF(NPF(IFUNC1(J3(1:NEL0)))+1)*YFAC(1:NEL0,3)
        Y44(1:NEL0)=TF(NPF(IFUNC1(J4(1:NEL0)))+1)*YFAC(1:NEL0,4)
        X1(1:NEL0) = RATE(1:NEL0,1)
        X2(1:NEL0) = RATE(1:NEL0,2)
        X3(1:NEL0) = RATE(1:NEL0,3)
        X4(1:NEL0) = RATE(1:NEL0,4)
        X(1:NEL0)  = EPSP(1:NEL0)
        CALL  INTER_RAT_V(NEL0,X1,X2,X3,X4,Y11,Y22,Y33,Y44,
     .                                          X,Y,YP,JJ,NRATE1)  
         YLD(1:NEL0) = FAIL(1:NEL0)*Y(1:NEL0)
       ELSE
#include "vectorize.inc"
        DO I=1,NEL0
           IF(JJ(I)==1)THEN 
            J1(I) = JJ(I)
            J2(I) = J1(I)+1
            J3(I) = J2(I)+1
            J4(I) = J3(I)+1
            DYDX1(I)= DYDX1(I)*YFAC(I,1)
            DYDX2 (I) = DYDX2(I)*YFAC(I,2)          
            FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1)) 
            H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
           ELSEIF(JJ(I)==(NRATE1-1))THEN
            J1(I) = JJ(I) - 2
            J2(I) = J1(I)+1 
            J3(I) = J2(I)+1 
            J4(I) = J3(I)+1
            DYDX3(I) = DYDX3(I)*YFAC(I,3)
            DYDX4(I) = DYDX4(I)*YFAC(I,4)
            FAC   = (EPSP(I) - RATE(I,3))/(RATE(I,4) - RATE(I,3)) 
             H(I)   = FAIL(I)*(DYDX3(I) + FAC*(DYDX4(I)-DYDX3(I)))  
           ELSE
            J1(I) = JJ(I) - 1
            J2(I) = J1(I)+1
            J3(I) = J2(I)+1
            J4(I) = J3(I)+1 
            DYDX2(I) = DYDX2(I)*YFAC(I,2)
            DYDX3(I) = DYDX3(I)*YFAC(I,3)
            FAC   = (EPSP(I) - RATE(I,2))/(RATE(I,3) - RATE(I,2))  
            H(I)   = FAIL(I)*(DYDX2(I) + FAC*(DYDX3(I)-DYDX2(I)))  
           ENDIF
        ENDDO
        X1(1:NEL0) = RATE(1:NEL0,1)
        X2(1:NEL0) = RATE(1:NEL0,2)
        X3(1:NEL0) = RATE(1:NEL0,3)
        X4(1:NEL0) = RATE(1:NEL0,4)
        X(1:NEL0)  = EPSP(1:NEL0)
        Y11(1:NEL0) = Y1(1:NEL0)*YFAC(1:NEL0,1)
        Y22(1:NEL0) = Y2(1:NEL0)*YFAC(1:NEL0,2)
        Y33(1:NEL0) = Y3(1:NEL0)*YFAC(1:NEL0,3)
        Y44(1:NEL0) = Y4(1:NEL0)*YFAC(1:NEL0,4)
        CALL  INTER_RAT_V(NEL0,X1,X2,X3,X4,Y11,Y22,Y33,Y44,
     .                                          X,Y,YP,JJ,NRATE1)  
        YLD(1:NEL0) = FAIL(1:NEL0)*Y(1:NEL0)
        YLD(1:NEL0) = MAX(YLD(1:NEL0),EM20)

        DO I=1,NEL0  
                UVAR(I,J1(I)+4) = IPOS1(I)
                UVAR(I,J2(I)+4) = IPOS2(I)
                UVAR(I,J3(I)+4) = IPOS3(I)
                UVAR(I,J4(I)+4) = IPOS4(I)
        ENDDO
C       ECROUISSAGE CINEMATIQUE
        Y11(1:NEL0)=TF(NPF(IFUNC1(J1(1:NEL0)))+1)*YFAC(1:NEL0,1) 
        Y22(1:NEL0)=TF(NPF(IFUNC1(J2(1:NEL0)))+1)*YFAC(1:NEL0,2)
        Y33(1:NEL0)=TF(NPF(IFUNC1(J3(1:NEL0)))+1)*YFAC(1:NEL0,3)
        Y44(1:NEL0)=TF(NPF(IFUNC1(J4(1:NEL0)))+1)*YFAC(1:NEL0,4)
         CALL  INTER_RAT_V(NEL0,X1,X2,X3,X4,Y11,Y22,Y33,Y44,
     .                                          X,Y,YP,JJ,NRATE1)  
         YLD(1:NEL0) = (ONE -FISOKIN1) * YLD(1:NEL0) +   FISOKIN1 * (FAIL(1:NEL0)*Y(1:NEL0))
       ENDIF

C-------------------
C     PROJECTION
C-------------------
       IF(IFLAG(1)==0)THEN
C projection radiale 
         DO I=1,NEL0
           SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .          + THREE*SIGNXY(I)*SIGNXY(I))
           R  = MIN(ONE,YLD(I)/MAX(EM20,SVM(I)))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           UMR = ONE - R
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G3(I)+H(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           S1=HALF*(SIGNXX(I)+SIGNYY(I))
           IF (INLOC == 0) THEN 
             DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
             DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU11-NU31*DEZZ
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
           IF(R<ONE) ETSE(I)= H(I)/(H(I)+E(I))
         ENDDO
C
       ELSEIF(IFLAG(1)==1)THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
         DO I=1,NEL0
           H(I) = MAX(ZERO,H(I))
           S1=SIGNXX(I)+SIGNYY(I)
           S2=SIGNXX(I)-SIGNYY(I)
           S3=SIGNXY(I)
           AA(I)=FOURTH*S1*S1
           BB(I)=THREE_OVER_4*S2*S2+THREE*S3*S3
           SVM(I)=SQRT(AA(I)+BB(I))
           IF (INLOC == 0) THEN   
             DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
         ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL0
           IF(SVM(I)>YLD(I).AND.OFF(I)==ONE) THEN
             NINDX=NINDX+1
             INDEX(NINDX)=I
           ENDIF
         ENDDO
C
         IF(NINDX/=0) THEN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
#include "vectorize.inc"
          DO J=1,NINDX
           I=INDEX(J)
           DPLA_J(I)=(SVM(I)-YLD(I))/(G3(I)+H(I))
           ETSE(I)= H(I)/(H(I)+E(I))
           HK(I) = H(I)*(ONE-FISOKIN1)
          ENDDO
C
          DO N=1,NMAX
#include "vectorize.inc"
           DO J=1,NINDX
             I=INDEX(J)
             DPLA_I(I)=DPLA_J(I)
             YLD_I =YLD(I)+HK(I)*DPLA_I(I)
             DR(I) =HALF*E(I)*DPLA_I(I)/YLD_I
             PP(I)  =ONE/(ONE + DR(I)*NU11)
             QQ(I)  =ONE/(ONE + THREE*DR(I)*NU21)
             P2    =PP(I)*PP(I)
             Q2    =QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU11*P2*PP(I)+ THREE*BB(I)*NU21*Q2*QQ(I))
     .         *(E(I)- TWO*DR(I)*HK(I))/YLD_I
     .         - TWO*HK(I)*YLD_I
             DF = SIGN(MAX(ABS(DF),EM20),DF)
             IF(DPLA_I(I)>ZERO) THEN
               DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
             ELSE
               DPLA_J(I)=ZERO
             ENDIF        
           ENDDO
          ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
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
             DEZZ = - NU31*DR(I)*S1/E(I)
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
          ENDDO
         ENDIF
C-------------------------------------------
       ELSEIF(IFLAG(1)==2)THEN
C projection radial sur le deviateur sur un critere reduit
C projection elastique en z => sig33 = 0
C le coef. de reduction du critere est tel que 
C l'on se trouve sur le critere apres les 2 projections
         DO I=1,NEL0
           P   = -(SIGNXX(I)+SIGNYY(I))*THIRD
           S11 = SIGNXX(I)+P
           S22 = SIGNYY(I)+P
C          s33 = p = -(S11 + S22)
           S12 = SIGNXY(I)
C
           P2 = P*P
           VM2= THREE*(S12*S12 - S11*S22)
           A  = P2*NU41 + VM2
           VM2= THREE*P2  + VM2
           B  = P2*NU61
           C  = P2*NU51 - YLD(I)*YLD(I)
           R  = MIN(ONE,(-B + SQRT(MAX(ZERO,B*B-A*C)))/MAX(A ,EM20))
           SIGNXX(I) = S11*R - P
           SIGNYY(I) = S22*R - P
           SIGNXY(I) = S12*R
           UMR = ONE - R
           SIGZ      = NNU11*P*UMR
           SIGNXX(I) = SIGNXX(I) + SIGZ
           SIGNYY(I) = SIGNYY(I) + SIGZ
           SVM(I)=SQRT(VM2)
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G3(I)+H(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           IF (INLOC == 0) THEN 
             DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
             DEZZ=-(DEPSXX(I)+DEPSYY(I))*NNU11-NU31*DEZZ
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
           IF(R<ONE) ETSE(I)= H(I)/(H(I)+E(I))
         ENDDO
       ENDIF
C
       DO I=1,NEL0
         IF(PLA(I)>EPSMAX1.AND.OFF(I)==ONE)OFF(I)=FOUR_OVER_5
       ENDDO
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
C   test bypass
       IF (FISOKIN1/=ZERO) THEN
        DO I=1,NEL0
          DSXX = SIGEXX(I) - SIGNXX(I)
          DSYY = SIGEYY(I) - SIGNYY(I)
          DSXY = SIGEXY(I) - SIGNXY(I)
          DEXX = (DSXX - NUX*DSYY) 
          DEYY = (DSYY - NUX*DSXX)
          DEXY = TWO*(ONE + NUX)*DSXY
          ALPHA = FISOKIN1*H(I)/(E(I)+H(I))/THREE
          SIGPXX = ALPHA*(FOUR*DEXX + TWO*DEYY)
          SIGPYY = ALPHA*(FOUR*DEYY + TWO*DEXX)
          SIGPXY = ALPHA*DEXY
         SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
         SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
         SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
         UVAR(I,2) = UVAR(I,2) + SIGPXX
         UVAR(I,3) = UVAR(I,3) + SIGPYY
         UVAR(I,4) = UVAR(I,4) + SIGPXY
        ENDDO
       ENDIF
C--------------------------------
C     NON-LOCAL THICKNESS VARIATION
C--------------------------------
      IF (INLOC > 0) THEN
        DO I = 1,NEL0 
          SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I)
     .           + SIGNYY(I)*SIGNYY(I)
     .           - SIGNXX(I)*SIGNYY(I)
     .           + THREE*SIGNXY(I)*SIGNXY(I))
          DEZZ   = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(SVM(I),EM20)
          DEZZ   = -NUX*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E1) - DEZZ
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)     
        ENDDO  
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  INTER_RAT                     source/materials/mat/mat060/sigeps60c.F
Chd|-- called by -----------
Chd|        SIGEPS60                      source/materials/mat/mat060/sigeps60.F
Chd|        SIGEPS60G                     source/materials/mat/mat060/sigeps60g.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTER_RAT(X0,X1,X2,X3,Y0,Y1,Y2,Y3,X,Y,YP,I,N)
C
C     INTERPOLATION (RATIONAL FUNCTION)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I, N
C     REAL
      my_real
     .   X0, X1, X2, X3, Y0, Y1, Y2, Y3, X, Y, YP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   Q, D, R, S, SP, C2, DM, SM, C1, C6, C4, C3, C3D, C5,
     .   A1, A0, A2, T, Y11
C
      Q = X-X1
      D = X2-X1
      R = D-Q
      S = (Y2-Y1) / D
      SP = (Y3-Y2) / (X3-X2)
      C2 = (SP-S) / (X3-X1)
      DM = X1-X0
      DM = SIGN(MAX(EM30,ABS(DM)),DM)
      SM = (Y1-Y0) / DM
      C1 = (S-SM) / (D+DM)
      C6 = 0.
C
      IF(I==1)THEN
          IF(X<=X0) THEN
           C1 = ZERO
           C2 = ZERO
           SM = ZERO
          ELSE
           C2 = C1
           C1 = SM/(X1 - X0)
          ENDIF
          R  = (X1 - X)
          Q = X - X0
          D = X1 - X0
          C3 = ABS(C2*R)
          C3D = C3+ABS(C1*Q)
          C5 = ZERO
        IF(C3D>ZERO) THEN
          C3 = C3/C3D
          C5 = C3*(C1-C2)
        ENDIF
        C4 = C2+C5
        C6 = D*C5*(ONE - C3)
        Y = Y0 + Q*(SM-R*C4)
        YP = SM+(Q-R)*C4+C6
      ELSE IF(I==N-1)THEN
        IF(SP==ZERO.OR.(X>X3)) THEN
          C1 = ZERO
          C2 = ZERO        
         ELSE
            C1 =  (SP - S)/(X3 - X1)
            C2 =  ZERO      
         ENDIF
           R  = (X3 - X)
           Q = X - X2
           D = X3 - X2
           C3 = ABS(C2*R)
           C3D = C3+ABS(C1*Q)
           C5 = ZERO
           IF(C3D>ZERO) THEN
           C3 = C3/C3D
           C5 = C3*(C1-C2)
           ENDIF
           C4 = C2+C5
           C6 = D*C5*(ONE - C3)
           Y = Y2 + (X - X2)*(SP-(X3 - X)*C4)
           YP = SP+(Q-R)*C4+C6
      ELSE
        IF(I==2.AND.SM*(SM-DM*C1)<=ZERO)C1=(S-SM-SM) / D
        C3 = ABS(C2*R)
        C3D = C3+ABS(C1*Q)
        C5 = ZERO
        IF(C3D>ZERO) THEN
          C3 = C3/C3D
          C5 = C3*(C1-C2)
        ENDIF
        C4 = C2+C5
        C6 = D*C5*(ONE - C3)
        Y = Y1 + Q*(S-R*C4)
        YP = S+(Q-R)*C4+C6
C 
      ENDIF
      
      END


Chd|====================================================================
Chd|  INTER_RAT_V                   source/materials/mat/mat060/sigeps60c.F
Chd|-- called by -----------
Chd|        SIGEPS60C                     source/materials/mat/mat060/sigeps60c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTER_RAT_V(NEL0,X0,X1,X2,X3,Y0,Y1,Y2,Y3,X,Y,YP,JJ,N)
!
!     INTERPOLATION (RATIONAL FUNCTION)
!
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL0,JJ(NEL0),N
C     REAL
      my_real
     .   X0(NEL0), X1(NEL0), X2(NEL0), X3(NEL0), 
     .   Y0(NEL0), Y1(NEL0), Y2(NEL0), Y3(NEL0), 
     .   X(NEL0), Y(NEL0), YP(NEL0)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C     REAL
      my_real
     .   Q(NEL0), D(NEL0), R(NEL0), S(NEL0), SP(NEL0), 
     .   C2(NEL0), DM(NEL0), SM(NEL0), C1(NEL0), C6(NEL0), 
     .   C4(NEL0), C3(NEL0), C3D(NEL0), C5(NEL0)
C-----------------------------------------------

        Q(1:NEL0) = X(1:NEL0)-X1(1:NEL0)
        D(1:NEL0) = X2(1:NEL0)-X1(1:NEL0)
        R(1:NEL0) = D(1:NEL0)-Q(1:NEL0)
        S(1:NEL0) = (Y2(1:NEL0)-Y1(1:NEL0)) / D(1:NEL0)
        SP(1:NEL0) = (Y3(1:NEL0)-Y2(1:NEL0)) / (X3(1:NEL0)-X2(1:NEL0))
        C2(1:NEL0) = (SP(1:NEL0)-S(1:NEL0)) / (X3(1:NEL0)-X1(1:NEL0))
        DM(1:NEL0) = X1(1:NEL0)-X0(1:NEL0)
        DM(1:NEL0) = SIGN(MAX(EM30,ABS(DM(1:NEL0))),DM(1:NEL0))
        SM(1:NEL0) = (Y1(1:NEL0)-Y0(1:NEL0)) / DM(1:NEL0)
        C1(1:NEL0) = (S(1:NEL0)-SM(1:NEL0)) / (D(1:NEL0)+DM(1:NEL0))
        C6(1:NEL0) = ZERO
C
#include "vectorize.inc"
        DO I=1,NEL0       
                ! ------------------- 
                IF(JJ(I)==1)THEN
                        IF(X(I)<=X0(I)) THEN
                                C1(I) = ZERO
                                C2(I) = ZERO
                                SM(I) = ZERO
                        ELSE
                                C2(I) = C1(I)
                                C1(I) = SM(I)/(X1(I) - X0(I))
                        ENDIF
                        R(I)  = (X1(I) - X(I))
                        Q(I) = X(I) - X0(I)
                        D(I) = X1(I) - X0(I)
                        C3(I) = ABS(C2(I)*R(I))
                        C3D(I) = C3(I)+ABS(C1(I)*Q(I))
                        C5(I) = ZERO
                        IF(C3D(I)>ZERO) THEN
                                C3(I) = C3(I)/C3D(I)
                                C5(I) = C3(I)*(C1(I)-C2(I))
                        ENDIF
                        C4(I) = C2(I)+C5(I)
                        C6(I) = D(I)*C5(I)*(ONE - C3(I))
                        Y(I) = Y0(I) + Q(I)*(SM(I)-R(I)*C4(I))
                        YP(I) = SM(I)+(Q(I)-R(I))*C4(I)+C6(I)
                ! ------------------- 
                ELSE IF(JJ(I)==N-1)THEN
                        IF(SP(I)==ZERO.OR.(X(I)>X3(I))) THEN
                                C1(I) = ZERO
                                C2(I) = ZERO        
                        ELSE
                                C1(I) =  (SP(I) - S(I))/(X3(I) - X1(I))
                                C2(I) =  ZERO      
                        ENDIF
                        R(I)  = (X3(I) - X(I))
                        Q(I) = X(I) - X2(I)
                        D(I) = X3(I) - X2(I)
                        C3(I) = ABS(C2(I)*R(I))
                        C3D(I) = C3(I)+ABS(C1(I)*Q(I))
                        C5(I) = ZERO
                        IF(C3D(I)>ZERO) THEN
                                C3(I) = C3(I)/C3D(I)
                                C5(I) = C3(I)*(C1(I)-C2(I))
                        ENDIF
                        C4(I) = C2(I)+C5(I)
                        C6(I) = D(I)*C5(I)*(ONE - C3(I))
                        Y(I) = Y2(I) + (X(I) - X2(I))*(SP(I)-(X3(I) - X(I))*C4(I))
                        YP(I) = SP(I)+(Q(I)-R(I))*C4(I)+C6(I)
                ! ------------------- 
                ELSE
                        IF(JJ(I)==2.AND.SM(I)*(SM(I)-DM(I)*C1(I))<=ZERO)C1(I)=(S(I)-SM(I)-SM(I)) / D(I)
                        C3(I) = ABS(C2(I)*R(I))
                        C3D(I) = C3(I)+ABS(C1(I)*Q(I))
                        C5(I) = ZERO
                        IF(C3D(I)>ZERO) THEN
                                C3(I) = C3(I)/C3D(I)
                                C5(I) = C3(I)*(C1(I)-C2(I))
                        ENDIF
                        C4(I) = C2(I)+C5(I)
                        C6(I) = D(I)*C5(I)*(ONE - C3(I))
                        Y(I) = Y1(I) + Q(I)*(S(I)-R(I)*C4(I))
                        YP(I) = S(I)+(Q(I)-R(I))*C4(I)+C6(I)
                ENDIF
                ! ------------------- 
        ENDDO
        RETURN
      END SUBROUTINE INTER_RAT_V
