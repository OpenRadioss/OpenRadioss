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
Chd|  SIGEPS87C                     source/materials/mat/mat087/sigeps87c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        MAT87C_HANSEL                 source/materials/mat/mat087/mat87c_hansel.F
Chd|        MAT87C_SWIFT_VOCE             source/materials/mat/mat087/mat87c_swift_voce.F
Chd|        MAT87C_TABULATED_PLAS_SR      source/materials/mat/mat087/mat87c_tabulated_plas_sr.F
Chd|        MAT87C_TABULATED_TOTALSR      source/materials/mat/mat087/mat87c_tabulated_totalsr.F
Chd|====================================================================
      SUBROUTINE SIGEPS87C(
     1     NEL    ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC    ,
     2     NPF    ,TF      ,TIME    ,TIMESTEP,UPARAM   ,
     3     UVAR   ,RHO0    ,THKLY   ,THK     ,OFF      ,
     4     EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     5     DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     6     SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     8     SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     9     SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     B     ETSE   ,GS      ,ISRATE  ,ASRATE  ,FACYLDI  ,
     C     TEMPEL ,SIGB    ,INLOC   ,DPLANL  ,SEQ      )
C=======================================================================
C   BARLAT YLD2000 material model
C=======================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL F
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
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
C SIGOXX  | NEL    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL    | F |R/W| THICKNESS
C PLA     | NEL    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,ISRATE,INLOC
      my_real
     .   TIME,TIMESTEP,ASRATE,UPARAM(NUPARAM),
     .   RHO0(NEL),GS(NEL),
     .   THKLY(NEL),PLA(NEL),EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DPLANL(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),FACYLDI(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),ETSE(NEL),SEQ(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,DIMENSION(NEL,12) ,INTENT(INOUT) :: SIGB
      my_real
     .   UVAR(NEL,NUVAR),OFF(NEL),THK(NEL),
     .   YLD(NEL),DPLA(NEL),EPSP(NEL),TEMPEL(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .     FINTER ,TF(*)
      EXTERNAL FINTER
c     Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
c     Y       : y = f(x)
c     X       : x
c     DYDX    : f'(x) = dy/dx
c     IFUNC(J): FUNCTION INDEX
c     J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
c     NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,NRATE,NITER,JJ(NEL),J1,J2,N,NINDX,
     .        NFALG3,IFLAG,IFLAGSR,EXPA,EXPAM2,IYIELD,
     .        INDX(NEL), IPOS1(NEL),ILEN1(NEL),IAD1(NEL),
     .       IPOS2(NEL),ILEN2(NEL),IAD2(NEL)
      my_real
     .        Y1(NEL),Y2(NEL),DYDX(NEL),DYDX1(NEL),DYDX2(NEL),
     .        XPXX(NEL),XPYY(NEL),XPXY(NEL),
     .        XPPXX(NEL),XPPYY(NEL),XPPXY(NEL),PHIP(NEL),PHIPP(NEL),
     .        DEPLXX(NEL),DEPLYY(NEL),DEPLXY(NEL),DEPLZZ(NEL),
     .        DEELZZ(NEL),DEPSZZ(NEL),XP1(NEL),XP2(NEL),XPP1(NEL),
     .        XPP2(NEL),SIG(NEL),H(NEL),SIGTRXX(NEL),SIGTRYY(NEL),
     .        SIGTRXY(NEL),DAEXP(NEL)
      my_real
     .      E,NU,BULK,A1,A2,G,UNSA,DU,R,
     .      AL1, AL2 , AL3 , AL4 ,DSIGPL1,DSPL1,
     .      AL5, AL6 , AL7 , AL8 ,DSIGPL2,DSPL2,
     .      AL9, AL10, AL11, AL12,DSIGPL3,DSPL3,
     .      YSCALE,CEPS,EPS0,EXPN,TERM1,
     .      LPP11,LPP12,LPP21,LPP22,LPP66,
     .      FCTP,BPP,CPP,DPP,FP1,FP2,FP3,
     .      KINT,FPP1,FPP2,FPP3,DFP1,DFP2,DFP3,
     .      DFPP1,DFPP2,DFPP3,DP,AP,DF1,DF2,DF3,
     .      DSDEPL1,DSDEPL2,DSDEPL3,NORMEF,EPSPI,
     .      ASWIFT ,EPSO,QVOCE,BETA,KO,ALPHA,NEXP,
     .      EXPV,KSWIFT,KVOCE,UNSP,UNSC,PLA_I,YLD_I,NORMSIG,
     .      DSWIFT,DVOCE,DFEPSP,YLDP, p2,UNSPT,UNSCT,DFSR,TREF,DVM,
     .      K1,K2,AHS,BHS,MHS,EPS0HS,NHS,HMART,TEMP0,ETA,CP, AM    ,
     .          BM    , CM    , DM    , PPM    , QM    , E0MART, VM0
      my_real
     .        HK(NEL),EPSF(NEL),PLAP(NEL),FRATE(NEL),
     .        SVM2(NEL),YLD2(NEL),HI(NEL),G3(NEL),
     .        HKIN,dezz,svm,DPLA_I(NEL),dr(NEL),
     .        AA(NEL),BB(NEL),DPLA_J(NEL),
     .        PP(NEL),QQ(NEL),FAIL(NEL),SVMO(NEL),
     .        HI2(NEL), ES2(NEL), ATEMP(NEL),AEXP(NEL),
     .        VM(NEL)
      my_real
     .        YFAC(NEL,2),PUI_1,PUI_2
      my_real NORM_SP,FCT,DF,DLAM,YLD_NORM(NEL)
C=======================================================================
c      Swift/Voce
c            k=alpha*k1(eps,p)+(1-alpha)*k2(eps,p)
c            k1=A*(eps,p+e0)**n
c            k2=Q*(1-exp(-b*eps,p))+sig0
c-----------------------------------------------
c     USER VARIABLES
c-----------------------------------------------
!       UVAR1  = PLAP
!       UVAR2  = YLD
!       UVAR3  = H
!       UVAR4  = DPLA
C=======================================================================
      IFLAG   = NINT(UPARAM(24))
      NRATE   = NINT(UPARAM(25))
      IFLAGSR = NINT(UPARAM(25+2*NRATE+8))
      IYIELD  = NINT(UPARAM(34+2*NRATE+21))
      ! IFLAG=1 => Swift-Voce Yld, IFLAG=0 => Tabulated Yld
      ! IFLAGSR=0 => total strain rate,  IFLAGSR=1 => plastic strain rate
c
      SELECT CASE (IYIELD)
          CASE(1)    ! swift voce
            CALL MAT87C_SWIFT_VOCE(
     .         NEL    ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP ,
     .         UPARAM ,UVAR    ,RHO0    ,THKLY   ,THK      ,
     .         EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     .         DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     .         SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     .         SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     .         SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     .         ETSE   ,GS      ,ISRATE  ,ASRATE  ,OFF      ,
     .         SIGB   ,INLOC   ,DPLANL  ,SEQ     )
         CASE(2) !hansel
            CALL MAT87C_HANSEL(
     .         NEL    ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP ,
     .         UVAR   ,RHO0    ,THKLY   ,THK     ,OFF      ,
     .         EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     .         DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     .         SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     .         SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     .         SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     .         ETSE   ,GS      ,ISRATE  ,ASRATE  ,UPARAM   ,
     .         TEMPEL ,SIGB    ,INLOC   ,DPLANL  ,SEQ      )
          CASE(3)
            CALL  MAT87C_TABULATED_PLAS_SR(
     .         NEL    ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC    ,
     .         NPF    ,TF      ,TIME    ,TIMESTEP,UPARAM   ,
     .         UVAR   ,RHO0    ,THKLY   ,THK     ,OFF      ,
     .         EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     .         DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     .         SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     .         SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     .         SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     .         ETSE   ,GS      ,ISRATE  ,ASRATE  ,FACYLDI  ,
     .         SIGB   ,INLOC   ,DPLANL  ,SEQ     )
          CASE(4)
            CALL   MAT87C_TABULATED_TOTALSR(
     .         NEL    ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC    ,
     .         NPF    ,TF      ,TIME    ,TIMESTEP,UPARAM   ,
     .         UVAR   ,RHO0    ,THKLY   ,THK     ,OFF      ,
     .         EPSPXX ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     .         DEPSXX ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     .         SIGOXX ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     .         SIGNXX ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     .         SOUNDSP,PLA     ,DPLA    ,EPSP    ,YLD      ,
     .         ETSE   ,GS      ,ISRATE  ,ASRATE  ,FACYLDI  ,
     .         SIGB   ,INLOC   ,DPLANL  ,SEQ     )
        END SELECT
c-----------------------------------------------
c     COMPUTATION OF STRAIN-RATE
c-----------------------------------------------
c-----------
      RETURN
      END
