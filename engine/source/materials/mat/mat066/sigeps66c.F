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
Chd|  SIGEPS66C                     source/materials/mat/mat066/sigeps66c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        PRONY_MODELC                  source/materials/mat/mat066/prony_modelc.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS66C(
     1     NEL    ,NUPARAM  ,NUVAR   ,MFUNC   ,KFUNC  ,
     2     NPF    ,NPT0     ,IPT     ,IFLAG   ,
     2     TF     ,TIME     ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT     ,THKLY   ,
     4     EPSPXX ,EPSPYY   ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY   ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY    ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY   ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY   ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY   ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX  ,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL      ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,YLD      ,EPSP    ,ISRATE  ,INLOC  ,
     D     DPLANL ,MAT_PARAM,NUVARV  ,UVARV   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
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
C NPT0    |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL    | F | R | INITIAL DENSITY
C AREA    | NEL    | F | R | AREA
C EINT    | 2*NEL  | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
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
C SIGVXX  | NEL    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL    | F | W | VISCOUS STRESS YY
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
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUVARV
      INTEGER NEL, NUPARAM, NUVAR, NPT0, IPT,IFLAG(*),
     .   NGL(NEL),MAT(NEL),ISRATE,IPM(NPROPMI,*),INLOC
      my_real
     .   TIME,TIMESTEP(NEL),UPARAM(*),
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
     .   GS(*),EPSP(NEL),DPLANL(NEL)
      TYPE(MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL),
     .    DPLA_I(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL,NUVARV) ,INTENT(INOUT) :: UVARV
      my_real ,INTENT(INOUT) :: UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .        FINTER ,TF(*),FINTER2
      EXTERNAL FINTER,FINTER2
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
      INTEGER I,J,N,NINDX,NMAX,IRATE,NCC,NCT,NFUNC,
     .        J1,J2,NPRONY,IADBUF,
     .        IAD1(NEL),IPOS1(NEL),ILEN1(NEL),
     .        IAD2(NEL),IPOS2(NEL),ILEN2(NEL),
     .        INDEX(NEL),JJC(NEL),JJT(NEL),IFUNC(MFUNC)
      my_real EC, E1T,A11T,A21T,G1T,
     .        YFACC,YFACT,CP,EDP,FISOKIN,
     .        NU,PC,PT,RPCT, EPSPO,NNU11,EPSP0,G31,DSXX,DSYY,DSXY,
     .        DEXX,DEYY,DEXY,SIGPXX,SIGPYY,SIGPXY,KV,VP,SIGY,
     .        FAC,EPD,NU31,R,UMR,DEZZ,S2,S3,NU11,S1,NU21,
     .        NU110,NU210,P2,Q2,F,DF,A,YLD_I,YRATE,HKIN,ALPHA,
     .        DTINV
      my_real 
     .        RATE0(MFUNC),GV(100),BETA(100),YFAC(MFUNC)
      my_real ,DIMENSION(NEL)  :: 
     .        E, A11 , A21, G,G3,C1, SIGEXX ,SIGEYY ,SIGEXY ,  
     .        DYDX1 ,DYDX2 ,SVM ,
     .        YC ,YT ,P ,H ,
     .        HK ,NNU1 ,HI ,DPLA_J ,
     .        AA ,BB ,DR ,PP ,QQ ,
     .        SVM2 ,YLD2 ,Y1 ,
     .        Y2 ,HT ,RATE ,HC ,EPSPZZ 
 
      DATA NMAX/3/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
       IADBUF = IPM(7,MAT(1)) 
C       
       IRATE = NINT(UPARAM(IADBUF ))
       E1T   = UPARAM(IADBUF +1)
       A11T  = UPARAM(IADBUF +2)
       A21T  = UPARAM(IADBUF +3)
       G1T   = UPARAM(IADBUF +4)
       NU    = UPARAM(IADBUF +5)
       PC    = UPARAM(IADBUF +6)
       PT    = UPARAM(IADBUF +7)
       EPSP0 = UPARAM(IADBUF +8)
       CP    = UPARAM(IADBUF +9)
       NCC   = NINT(UPARAM(IADBUF +10))
       NCT   = NINT(UPARAM(IADBUF +11))
       FISOKIN = UPARAM(IADBUF + 12)

       NFUNC = IPM(10,MAT(1))
C               
       DO I=1,NFUNC
        IFUNC(I) = IPM(10+I,MAT(1))
        YFAC(I)  = UPARAM(IADBUF +12+I)
        RATE0(I) = UPARAM(IADBUF +12+NFUNC + I)
       ENDDO
C    
       SIGY = UPARAM(IADBUF + 13 + 2*MFUNC) 
       VP   = UPARAM(IADBUF + 14 + 2*MFUNC)
c       
       EC   = UPARAM(IADBUF + 15 + 2*MFUNC)
       RPCT = UPARAM(IADBUF + 16 + 2*MFUNC)
C
       IF(VP /=0 .AND .IFLAG(1) /= 1) VP = 0
C       
       NNU11 = NU / (ONE - NU)
C       
       IF (ISIGI==0) THEN
        IF(TIME==ZERO)THEN
         DO I=1,NEL
           UVAR(I,1)=ZERO
           UVAR(I,2)=ZERO
           UVAR(I,3)=ZERO
           UVAR(I,4)=ZERO
           DO J=1,NFUNC
             UVAR(I,J+4)=ZERO
           ENDDO
         ENDDO
        ENDIF
       ENDIF
C------------------------------------------
c      estimation if compression mode
C------------------------------------------
      DO I=1,NEL        
         SIGNXX(I)=SIGOXX(I) - UVAR(I,2)  + A11T*DEPSXX(I)+A21T*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) - UVAR(I,3)  + A21T*DEPSXX(I)+A11T*DEPSYY(I)
         P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) )
      ENDDO
      E(1:NEL) = E1T   
      G(1:NEL) = G1T   
      IF(EC > ZERO)THEN
        DO I=1,NEL  
          IF(P(I) <=  - RPCT * PT) THEN
            E(I)   = E1T
          ELSEIF(P(I) >= RPCT *PC) THEN
            E(I)   = EC
          ELSE
            FAC =  RPCT *(PC + PT)
            FAC = (RPCT * PC - P(I))/FAC
            E(I) = FAC*E1T + (ONE -FAC)*EC
          ENDIF  
        ENDDO
      ENDIF
      DO I=1,NEL  
        G(I)   = HALF*E(I)/( ONE + NU)
        A11(I) = E(I)/(ONE - NU*NU)
        A21(I) = NU * A11(I)                          
        G3(I)  = THREE*G(I)                         
      ENDDO


C------------------------------------------
C   estiamte stress
C------------------------------------------
       DO I=1,NEL 
C
C  en cas d'   crouissage cinematique
C       
       
         SIGNXX(I)=SIGOXX(I) - UVAR(I,2)  + A11(I)*DEPSXX(I)+A21(I)*DEPSYY(I)
         SIGNYY(I)=SIGOYY(I) - UVAR(I,3)  + A21(I)*DEPSXX(I)+A11(I)*DEPSYY(I)
         SIGNXY(I)=SIGOXY(I) - UVAR(I,4)  + G(I) *DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)  + GS(I) *DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I)  + GS(I) *DEPSZX(I)
C 
         SIGEXX(I) = SIGNXX(I)
         SIGEYY(I) = SIGNYY(I)
         SIGEXY(I) = SIGNXY(I)
         
         P(I) = -THIRD*(SIGNXX(I) + SIGNYY(I) )
C
         SOUNDSP(I) = SQRT(A11T/RHO0(I))
         VISCMAX(I) = ZERO
         ETSE(I)    = ONE
C-------------------
C     STRAIN RATE
C-------------------
        IF (ISRATE == 0) EPSP(I) = 
     .     HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
          JJC(I) = 1
          JJT(I) = NCC + 1
       ENDDO
C
C id function
C              
      IF(IRATE <=3) THEN 
            DO I=1,NEL
              IPOS1(I) = NINT(UVAR(I,5))
              IAD1(I)  = NPF(IFUNC(1)) / 2 + 1
              ILEN1(I) = NPF(IFUNC(1)+1) / 2 - IAD1(I)-IPOS1(I)
              IPOS2(I) = NINT(UVAR(I,6))
              IAD2(I)  = NPF(IFUNC(2)) / 2 + 1
              ILEN2(I) = NPF(IFUNC(2)  + 1) / 2 - IAD2(I)-IPOS2(I)
C              
               UVAR(I,5) = IPOS1(I)
               UVAR(I,6) = IPOS2(I)
            END DO
C
             CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,YC)
             CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,YT)
             
          IF(FISOKIN == ZERO) THEN
           DO I=1,NEL 
             YC(I)=YC(I)*YFAC(1)
             YT(I)=YT(I)*YFAC(2)
             HC(I)=DYDX1(I)*YFAC(1)
             HT(I)=DYDX2(I)*YFAC(2)
           ENDDO  
          ELSEIF(FISOKIN == ONE ) THEN
            DO I=1,NEL 
              HC(I)=DYDX1(I)*YFAC(1)
              HT(I)=DYDX2(I)*YFAC(2)
              YC(I)=TF(NPF(IFUNC(1)) + 1)
              YT(I)=TF(NPF(IFUNC(2)) + 1)
              YC(I)=YC(I)*YFAC(1)
              YT(I)=YT(I)*YFAC(2)
              YC(I) = MAX(EM20, YC(I))
              YT(I) = MAX(EM20, YT(I))
            ENDDO 
          ELSE
            DO I=1,NEL
                YC(I)=YC(I)*YFAC(1)
                YT(I)=YT(I)*YFAC(2)
                YC(I) = MAX(YC(I),EM20)
                YT(I) = MAX(YT(I),EM20)
                HC(I) = DYDX1(I)*YFAC(1)
                HT(I) = DYDX2(I)*YFAC(2)
C    ECROUISSAGE CINEMATIQUE
                Y1(I)=YFAC(1)*TF(NPF(IFUNC(1))+1)
                Y2(I)=YFAC(2)*TF(NPF(IFUNC(2))+1)
                YC(I) = (ONE - FISOKIN) * YC(I) + FISOKIN * Y1(I)
                YT(I) = (ONE - FISOKIN) * YT(I) + FISOKIN * Y2(I)
             ENDDO
          ENDIF
      ELSE
C multiples curves
C
C  compression
C
            DO J = 2,NCC - 1
              DO I=1,NEL
                IF(EPSP(I) >= RATE0(J) ) JJC(I) = J
              ENDDO
            ENDDO        
            DO I=1,NEL
              FAC=RATE0(JJC(I))
              RATE(I)=(EPSP(I) - FAC)/(RATE0(JJC(I) +1) - FAC)
            ENDDO        
            DO I=1,NEL
              J1 = JJC(I)
              J2 = J1+1
              IPOS1(I) = NINT(UVAR(I,4+J1 ))
              IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1
              ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I)-IPOS1(I)
              IPOS2(I) = NINT(UVAR(I,4+J2))
              IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1
              ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I)-IPOS2(I)
C              
              UVAR(I,4+J1) = IPOS1(I)
              UVAR(I,4+J2) = IPOS2(I) 
            END DO 
             CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
             CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2) 
C                  
             IF (FISOKIN == ZERO) THEN
               DO I=1,NEL
                 J1 = JJC(I)
                 J2 = J1+1
                 Y1(I)=Y1(I)*YFAC(J1)
                 Y2(I)=Y2(I)*YFAC(J2)
                 FAC   = RATE(I)
                 YC(I) =(Y1(I)    + FAC*(Y2(I)-Y1(I)))
                 YC(I) = MAX(YC(I),EM20)
                 DYDX1(I)=DYDX1(I)*YFAC(J1)
                 DYDX2(I)=DYDX2(I)*YFAC(J2)
                 HC(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
               ENDDO
             ELSEIF (FISOKIN == ONE ) THEN
                DO I=1,NEL
                 J1 = JJC(I)
                 J2 = J1+1
                 FAC   = RATE(I)
                 DYDX1(I)=DYDX1(I)*YFAC(J1)
                 DYDX2(I)=DYDX2(I)*YFAC(J2)
                 HC(I)   =(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
C               ECROUISSAGE CINEMATIQUE
                 Y1(I)=TF(NPF(IFUNC(J1)) + 1)
                 Y2(I)=TF(NPF(IFUNC(J2)) + 1)
                 Y1(I)=Y1(I)*YFAC(J1)
                 Y2(I)=Y2(I)*YFAC(J2)
                 YC(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))
                 YC(I) = MAX(EM20,YC(I))
                ENDDO
              ELSE
                DO I=1,NEL
                 J1 = JJC(I)
                 J2 = J1 + 1
                 Y1(I)=Y1(I)*YFAC(J1)
                 Y2(I)=Y2(I)*YFAC(J2)
                 FAC   = RATE(I)
                 YC(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))
                 YC(I) = MAX(YC(I),EM20)
                 DYDX1(I)=DYDX1(I)*YFAC(J1)
                 DYDX2(I)=DYDX2(I)*YFAC(J2)
                 HC(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
C                ECROUISSAGE CINEMATIQUE
                 Y1(I)=TF(NPF(IFUNC(J1))+1)
                 Y2(I)=TF(NPF(IFUNC(J2))+1)
                 Y1(I)=Y1(I)*YFAC(J1)
                 Y2(I)=Y2(I)*YFAC(J2)
                 YC(I) = (ONE - FISOKIN) * YC(I) + 
     .             FISOKIN * ((Y1(I)    + FAC*(Y2(I)-Y1(I))))
                ENDDO
               ENDIF
C
C  traction
C            
            DO J = 2,NCT - 1
              DO I=1,NEL
                IF(EPSP(I) >= RATE0(NCC+J)) JJT(I) = NCC + J
              ENDDO
            ENDDO        
            DO I=1,NEL
              FAC=RATE0(JJT(I))
              RATE(I)=(EPSP(I) - FAC)/(RATE0(JJT(I)+1) - FAC)
            ENDDO        
            DO I=1,NEL
              J1 = JJT(I)
              J2 = J1+1
              IPOS1(I) = NINT(UVAR(I,4+J1))
              IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1
              ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I)-IPOS1(I)
              IPOS2(I) = NINT(UVAR(I,4+J2))
              IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1
              ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I)-IPOS2(I)
C              
              UVAR(I,4+J1) = IPOS1(I)
              UVAR(I,4+J2) = IPOS2(I)
            END DO 
             CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
             CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2) 
C                  
             IF (FISOKIN == ZERO) THEN
               DO I=1,NEL
                 J1 = JJT(I)
                 J2 = J1+1
                 Y1(I)=Y1(I)*YFAC(J1)
                 Y2(I)=Y2(I)*YFAC(J2)
                 FAC   = RATE(I)
                 YT(I) =(Y1(I)    + FAC*(Y2(I)-Y1(I)))
                 YT(I) = MAX(YT(I),EM20)
                 DYDX1(I)=DYDX1(I)*YFAC(J1)
                 DYDX2(I)=DYDX2(I)*YFAC(J2)
                 HT(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
               ENDDO
             ELSEIF (FISOKIN == ONE ) THEN
                DO I=1,NEL
                 J1 = JJT(I)
                 J2 = J1+1
                 FAC   = RATE(I)
                 DYDX1(I)=DYDX1(I)*YFAC(J1)
                 DYDX2(I)=DYDX2(I)*YFAC(J2)
                 HT(I)   =(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
C               ECROUISSAGE CINEMATIQUE
                 Y1(I)=YFAC(J1)*TF(NPF(IFUNC(J1)) + 1)
                 Y2(I)=YFAC(J2)*TF(NPF(IFUNC(J2)) + 1)
                 YT(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))
                 YT(I) = MAX(EM20,YT(I))
                ENDDO
              ELSE
                DO I=1,NEL
                 J1 = JJT(I)
                 J2 = J1 + 1
                 Y1(I)=Y1(I)*YFAC(J1)
                 Y2(I)=Y2(I)*YFAC(J2)
                 FAC   = RATE(I)
                 YT(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))
                 YT(I) = MAX(YT(I),EM20)
                 DYDX1(I)=DYDX1(I)*YFAC(J1)
                 DYDX2(I)=DYDX2(I)*YFAC(J2)
                 HT(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
C                ECROUISSAGE CINEMATIQUE
                 Y1(I)=YFAC(J1)*TF(NPF(IFUNC(J1))+1)
                 Y2(I)=YFAC(J2)*TF(NPF(IFUNC(J2))+1)
                 YT(I) = (ONE - FISOKIN) * YT(I) + 
     .               FISOKIN * ((Y1(I)    + FAC*(Y2(I)-Y1(I))))
                ENDDO
              ENDIF               
      ENDIF    
c
C strain effect on compression and traction
         IF(IRATE == 3) THEN
           DO I=1,NEL
C compression           
            YRATE = YFAC(3)*FINTER(IFUNC(3),EPSP(I),NPF,TF,DF)
            YC(I) = YRATE*YC(I)
            HC(I) = HC(I)*YRATE
c traction            
            YRATE = YFAC(4)*FINTER(IFUNC(4),EPSP(I),NPF,TF,DF)
            YT(I) = YRATE*YT(I)
            HT(I) = HT(I)*YRATE
          ENDDO
         ENDIF
C interpolation entre (tracation & compression         
        DO I=1,NEL   
          IF(P(I) <=  -PT) THEN
            YLD(I) = MAX(YT(I),EM20)
            H(I)   = HT(I)
          ELSEIF(P(I) >= PC) THEN
            YLD(I) = MAX(YC(I), EM20)
            H(I)   = HC(I)
          ELSE
            FAC = PC + PT
            FAC = (PC - P(I))/FAC
            YLD(I) = FAC*YT(I) + (ONE -FAC)*YC(I)
            YLD(I) = MAX(EM20,YLD(I))
            H(I) = FAC*HT(I) + (ONE -FAC)*HC(I)
          ENDIF    
        ENDDO
C
C strain rate effect
c (1 + (epsp/epsp0)**(1/cp)
      IF(VP == 0) THEN
       IF(IRATE == 1) THEN      
         DO I=1,NEL
          EPD = MAX(EM20,EPSP(I)/EPSP0)
          YRATE  = ONE + EXP(CP*LOG(EPD))
          YLD(I) = YLD(I)*YRATE
          H(I) = H(I)*YRATE
         ENDDO     
C 1 + cp*ln(epep/epsp0)     
       ELSEIF(IRATE == 2) THEN
         DO I=1,NEL
            EPD = MAX(EM20,EPSP(I)/EPSP0)
            YRATE = ONE + CP*LOG(EPD)
            YLD(I) = YLD(I)*YRATE
            H(I) = H(I)*YRATE
         ENDDO  
       ENDIF   
      ENDIF               
C-------------------
C     PROJECTION
C-------------------
       IF(IFLAG(1) == 0)THEN
         NU31 = ONE-NNU11
C projection radiale 
         DO I=1,NEL
           SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .          +THREE*SIGNXY(I)*SIGNXY(I))
           R  = MIN(ONE,YLD(I)/MAX(EM20,SVM(I)))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           UMR = ONE - R
           DPLA_I(I) = OFF(I)*SVM(I)*UMR/(G3(I)+H(I))
           PLA(I) = PLA(I) + DPLA_I(I)
           S1=HALF*(SIGNXX(I)+SIGNYY(I))
           IF (INLOC ==0) THEN
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
         DO I=1,NEL
           H(I) = MAX(ZERO,H(I))
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
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL
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
          DO J=1,NINDX
           I=INDEX(J)
           DPLA_J(I)=(SVM(I)-YLD(I))/(G3(I)+H(I))
           ETSE(I)= H(I)/(H(I)+E(I))
           HI(I) = H(I)*(ONE-FISOKIN)
           HK(I) = TWO_THIRD*H(I)*FISOKIN
          ENDDO
C
          IF(VP  == 1) THEN
            DO J=1,NINDX 
                I=INDEX(J)
                DTINV = TIMESTEP(I)/MAX(EM20, TIMESTEP(I)**2)
                EPD = DPLA_J(I)*DTINV
                EPD = MAX(EM20,EPD/EPSP0)
                YRATE  = ONE + EXP(CP*LOG(EPD))
                IF(SIGY == ZERO) THEN
                  YLD(I) = YLD(I)*YRATE
                  HI(I) = HI(I)*YRATE
                  HK(I) = HK(I)*YRATE
                ELSE
                   YLD(I) = YLD(I) + SIGY*(YRATE - ONE)
                ENDIF
            ENDDO 
          ENDIF
C          
          NU11 = ONE/(ONE-NU)
          NU21 = ONE/(ONE+NU)
          NU31 = ONE-NNU11
          DO N=1,NMAX
#include "vectorize.inc"
           DO J=1,NINDX
             I=INDEX(J)
             DPLA_I(I)=DPLA_J(I)
             YLD_I =YLD(I)+HI(I)*DPLA_I(I)
             DR(I) =HALF*E(I)*DPLA_I(I)/YLD_I
             NU110 = NU11+THREE*HK(I)/E(I)
             NU210 = NU21+HK(I)/E(I)
             PP(I)  =ONE/(ONE+DR(I)*NU110)
             QQ(I)  =ONE/(ONE+THREE*DR(I)*NU210)     
             P2    =PP(I)*PP(I)
             Q2    =QQ(I)*QQ(I)
             F     =AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
             DF    =-(AA(I)*NU110*P2*PP(I)+THREE*BB(I)*NU210*Q2*QQ(I))
     .         *(E(I)-TWO*DR(I)*HI(I))/YLD_I
     .         -TWO*HI(I)*YLD_I
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
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
         DO I=1,NEL
           H(I) = MAX(ZERO,H(I))
           SVM2(I)=SIGNXX(I)*SIGNXX(I)
     .             +SIGNYY(I)*SIGNYY(I)
     .             -SIGNXX(I)*SIGNYY(I)
     .             +THREE*SIGNXY(I)*SIGNXY(I)
           SVM(I)=SQRT(SVM2(I))
           IF (INLOC == 0) THEN   
             DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
             THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
           ENDIF
         ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
         NINDX=0
         DO I=1,NEL
           YLD2(I)=YLD(I)*YLD(I)
           IF(SVM2(I)>YLD2(I).AND.OFF(I)==ONE) THEN
             NINDX=NINDX+1
             INDEX(NINDX)=I
           ENDIF
         ENDDO
C
         IF(NINDX/=0) THEN
C-------------
C   PROJ NORMALE AU CRITERE AVEC CALCUL APPROCHE DE LA NORMALE + RETOUR RADIAL
C-------------
          NU31 = ONE-NNU11
          DO J=1,NINDX
           I=INDEX(J)
           A=(SVM2(I)-YLD2(I))
     .      /(FIVE*SVM2(I)+THREE*(-SIGNXX(I)*SIGNYY(I)+SIGNXY(I)*SIGNXY(I)))
           S1=(ONE-TWO*A)*SIGNXX(I)+          A*SIGNYY(I)
           S2=          A*SIGNXX(I)+(ONE-TWO*A)*SIGNYY(I)
           S3=(ONE-THREE*A)*SIGNXY(I)
           SIGNXX(I)=S1
           SIGNYY(I)=S2
           SIGNXY(I)=S3
           DPLA_I(I) = OFF(I)*(SVM(I)-YLD(I))/(G3(I)+H(I))
C
           HK(I) = H(I)*(ONE-FISOKIN)
           YLD(I)= YLD(I)+HK(I)*DPLA_I(I)
          END DO
C
          DO J=1,NINDX
            I=INDEX(J)
            SVM(I)= SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)
     .            - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I)
            IF (SVM(I) > YLD(I)*YLD(I)) THEN
              SVM(I) = SQRT(SVM(I))
              R  = YLD(I) / SVM(I)
              SIGNXX(I) = SIGNXX(I)*R
              SIGNYY(I) = SIGNYY(I)*R
              SIGNXY(I) = SIGNXY(I)*R
              PLA(I) = PLA(I) + DPLA_I(I)
              IF (INLOC == 0) THEN
                DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
                DEZZ = -NU31*DEZZ
                THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
              ENDIF
              ETSE(I)= H(I)/(H(I)+E(I))
            ENDIF
          END DO
         END IF
C
       ENDIF
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------

       IF (FISOKIN > ZERO) THEN
        DO I=1,NEL
          DSXX = SIGEXX(I) - SIGNXX(I)
          DSYY = SIGEYY(I) - SIGNYY(I)
          DSXY = SIGEXY(I) - SIGNXY(I)
          DEXX = (DSXX - NU*DSYY) 
          DEYY = (DSYY - NU*DSXX)
C
          DEXY = TWO*(ONE+NU)*DSXY
                   HKIN = TWO_THIRD*FISOKIN*H(I)
          ALPHA = HKIN/(E(I)+HKIN)
          SIGPXX = ALPHA*(TWO*DEXX+DEYY)
          SIGPYY = ALPHA*(TWO*DEYY+DEXX)
          SIGPXY = ALPHA*DEXY*HALF
          UVAR(I,2) = UVAR(I,2) + SIGPXX
          UVAR(I,3) = UVAR(I,3) + SIGPYY
          UVAR(I,4) = UVAR(I,4) + SIGPXY
C
          SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
          SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
          SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
        ENDDO
       ENDIF       
C
C  visco elastic model (prony)
C             
       IF (MAT_PARAM%IVISC == 1) THEN   
         NPRONY = MAT_PARAM%VISC%IPARAM(1)
         KV     = MAT_PARAM%VISC%UPARAM(1)
c              
        DO I=1,NPRONY
          GV(I)   = MAT_PARAM%VISC%UPARAM(1 + I)         
          BETA(I) = MAT_PARAM%VISC%UPARAM(1 + NPRONY + I)
        ENDDO 
C
        CALL PRONY_MODELC( 
     1     NEL    ,NUPARAM,NUVARV  ,TIME    ,TIMESTEP , 
     2     NPRONY , KV   , GV   ,BETA    ,RHO0   ,   
     3     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     4     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     5     VISCMAX,SOUNDSP,UVARV   ,OFF)        
        ENDIF
C
C--------------------------------
C     NON-LOCAL THICKNESS VARIATION
C--------------------------------
      IF (INLOC > 0) THEN
        DO I = 1,NEL
          SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)
     .           - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))
          DEZZ   = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(SVM(I),EM20)
          DEZZ   = -NU*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E(I)) - DEZZ
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)     
        ENDDO  
      ENDIF
C -----------------------------------------        
      RETURN
      END
C
