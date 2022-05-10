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
Chd|  SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        DREH                          source/materials/mat/mat033/sigeps33.F
Chd|        JACOBIEW                      source/materials/mat/mat033/sigeps33.F
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS33(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF                    )

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR
      my_real 
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real 
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,J,K,L,NROT,KEN
      INTEGER VPVEC
      my_real 
     . EY,C1,C2,ET,VMU,VMU0
     . , A,B,C,D,VAR
     . , AXX,BXX,CXX,AYY,BYY,CYY,AZZ,BZZ,CZZ
     . , AXY,BXY,CXY,AYZ,BYZ,CYZ,AZX,BZX,CZX
     . , C1XX,C2XX,ETXX,VMUXX, C1YY,C2YY,ETYY,VMUYY
     . , C1XY,C2XY,GTXY,VMUXY, C1YZ,C2YZ,GTYZ,VMUYZ
     . , C1ZX,C2ZX,GTZX,VMUZX, P0,PHI,GAMA0,FAC,FAC1
     . , S(3,3),SIGPR(3),DIRPR(3,3)
     . , C1ZZ,C2ZZ,ETZZ,VMUZZ
      my_real  
     .   E(MVSIZ),EDOT(MVSIZ),DAV,E1,E2,E3,E4,E5,E6,EPSP
     . , EPET(MVSIZ),EMET(MVSIZ),EPETS(MVSIZ),EMETS(MVSIZ)
     . , EMXX(MVSIZ),EMYY(MVSIZ),EMZZ(MVSIZ)
     . , GMXY(MVSIZ),GMYZ(MVSIZ),GMZX(MVSIZ) 
     . , EPETXX(MVSIZ),EPETYY(MVSIZ),EPETZZ(MVSIZ)
     . , EMETXX(MVSIZ),EMETYY(MVSIZ),EMETZZ(MVSIZ)
     . , GPGTXY(MVSIZ),GPGTYZ(MVSIZ),GPGTZX(MVSIZ)
     . , GMGTXY(MVSIZ),GMGTYZ(MVSIZ),GMGTZX(MVSIZ)
     . , SYXX(MVSIZ),SYYY(MVSIZ),SYZZ(MVSIZ)
     . , SYXY(MVSIZ),SYYZ(MVSIZ),SYZX(MVSIZ)
     . , SIGAIR(MVSIZ),GAMA(MVSIZ),SYIELD(MVSIZ)
     . , SIGSXX(MVSIZ),SIGSYY(MVSIZ),SIGSZZ(MVSIZ)
     . , SIGSXY(MVSIZ),SIGSYZ(MVSIZ),SIGSZX(MVSIZ)
     . , DEXX(MVSIZ),DEYY(MVSIZ),DEZZ(MVSIZ)
     . , DEXY(MVSIZ),DEYZ(MVSIZ),DEZX(MVSIZ)
     . , DEDTXX(MVSIZ),DEDTYY(MVSIZ),DEDTZZ(MVSIZ)
     . , DEDTXY(MVSIZ),DEDTYZ(MVSIZ),DEDTZX(MVSIZ)
     . , DSDTXX(MVSIZ),DSDTYY(MVSIZ),DSDTZZ(MVSIZ)
     . , DSDTXY(MVSIZ),DSDTYZ(MVSIZ),DSDTZX(MVSIZ)
C
      my_real 
     .   SIGV(MVSIZ,6), SIGPRV(MVSIZ,3), DIRPRV(MVSIZ,3,3)
C

C----------------------------------------------------------------

C INITIALIZATION
      IF(ISIGI == 0) THEN
      IF(TIME.EQ.ZERO) THEN
        DO 1000 J=1,NUVAR
        DO 1000 I=1,NEL
          UVAR(I,J)=ZERO
 1000   CONTINUE
      ENDIF
      ENDIF      

      KEN   = UPARAM(1)
      EY    = UPARAM(2)
      A     = UPARAM(3)
      B     = UPARAM(4)
      C     = UPARAM(5)
      P0    = UPARAM(6)
      PHI   = UPARAM(7)
      GAMA0 = UPARAM(8)

      VPVEC = 0
C COMPUTE VOLUMETRIC STRAIN-AIR PRESSURE
      DO 900 I=1,NEL
        GAMA(I) = RHO0(I)/RHO(I)-UN+GAMA0
        VAR = -(P0*GAMA(I))/(UN+GAMA(I)-PHI+EM15)
        SIGAIR(I) = MAX(ZERO,VAR)
  900 CONTINUE

      GOTO (1,2,3) ,ABS(KEN)+1
C----------------------------------
    1 CONTINUE
C----------------------------------
      FAC = UPARAM(9)
      FAC1 = UPARAM(10)

      DO 100 I=1,NEL
        IF(IFUNC(1).NE.0) THEN
          SYIELD(I)=FAC*FINTER(IFUNC(1),GAMA(I),NPF,TF,B*C)
        ELSE
          SYIELD(I) = ABS(A+B*(UN+C*GAMA(I)))
        ENDIF
        IF(IFUNC(2).NE.0)THEN
           DAV = (EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))*THIRD
           E1 = EPSPXX(I) - DAV
           E2 = EPSPYY(I) - DAV
           E3 = EPSPZZ(I) - DAV
           E4 = UNDEMI*EPSPXY(I)
           E5 = UNDEMI*EPSPYZ(I)
           E6 = UNDEMI*EPSPZX(I)
           EPSP =UNDEMI*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
           EPSP = SQRT(TROIS*EPSP)/TDEMI
           SYIELD(I) = FAC1*SYIELD(I)*(FINTER(IFUNC(2),EPSP,NPF,TF,B*C))
        ENDIF
  100 CONTINUE

      DO 110 I=1,NEL
        SIGSXX(I)=SIGOXX(I)+SIGAIR(I)
        SIGSYY(I)=SIGOYY(I)+SIGAIR(I)
        SIGSZZ(I)=SIGOZZ(I)+SIGAIR(I)
        SIGSXY(I)=SIGOXY(I)
        SIGSYZ(I)=SIGOYZ(I)
        SIGSZX(I)=SIGOZX(I)
  110 CONTINUE

C COMPUTE TRIAL STRESSES
      DO 120 I=1,NEL
        SIGSXX(I)=SIGSXX(I)+EY*EPSPXX(I)*TIMESTEP
        SIGSYY(I)=SIGSYY(I)+EY*EPSPYY(I)*TIMESTEP
        SIGSZZ(I)=SIGSZZ(I)+EY*EPSPZZ(I)*TIMESTEP
        SIGSXY(I)=SIGSXY(I)+EY*EPSPXY(I)*TIMESTEP*UNDEMI
        SIGSYZ(I)=SIGSYZ(I)+EY*EPSPYZ(I)*TIMESTEP*UNDEMI
        SIGSZX(I)=SIGSZX(I)+EY*EPSPZX(I)*TIMESTEP*UNDEMI
  120 CONTINUE
C si ancienne methode alors ....
      IF (VPVEC.EQ.1) THEN
       DO 130 I=1,NEL
        S(1,1)=SIGSXX(I)
        S(2,1)=SIGSXY(I)
        S(2,2)=SIGSYY(I)
        S(3,1)=SIGSZX(I)
        S(3,2)=SIGSYZ(I)
        S(3,3)=SIGSZZ(I)

C TRANSFORM GLOBAL STRESSES (S_IJ) TO PRINCIPAL STRESSES (SIGPR_K)
        CALL JACOBIEW(S,3,SIGPR,DIRPR,NROT)

C YIELD CRITERIA - SCALING
        SIGPR(1)=MIN(SYIELD(I),ABS(SIGPR(1)))*SIGN(UN,SIGPR(1))
        SIGPR(2)=MIN(SYIELD(I),ABS(SIGPR(2)))*SIGN(UN,SIGPR(2))
        SIGPR(3)=MIN(SYIELD(I),ABS(SIGPR(3)))*SIGN(UN,SIGPR(3))

C TRANSFORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
        DO 131 K=1,3
          DO 131 L=1,3
            S(K,L)=ZERO
            IF(K.EQ.L) S(K,L)= SIGPR(K)
  131   CONTINUE
        CALL DREH(S,DIRPR,1,1,1)
   
        SIGSXX(I)=S(1,1)
        SIGSXY(I)=S(2,1)
        SIGSYY(I)=S(2,2)
        SIGSZX(I)=S(3,1)
        SIGSYZ(I)=S(3,2)
        SIGSZZ(I)=S(3,3)
  130  CONTINUE
C fin ancienne methode
C sinon
      ELSE
        DO I = 1, NEL
          SIGV(I,1) = SIGSXX(I)
          SIGV(I,2) = SIGSYY(I)
          SIGV(I,3) = SIGSZZ(I)
          SIGV(I,4) = SIGSXY(I)
          SIGV(I,5) = SIGSYZ(I)
          SIGV(I,6) = SIGSZX(I)
        ENDDO
Cow         for a simple precision executing
        IF (IRESP.EQ.1) THEN
          CALL VALPVECDP_V(SIGV,SIGPRV,DIRPRV,NEL)
        ELSE
          CALL VALPVEC_V(SIGV,SIGPRV,DIRPRV,NEL)
        ENDIF
        DO I = 1, NEL
C YIELD CRITERIA - SCALING
          SIGPRV(I,1)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,1))),SIGPRV(I,1))
          SIGPRV(I,2)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,2))),SIGPRV(I,2))
          SIGPRV(I,3)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,3))),SIGPRV(I,3))
C TRANSFORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
          SIGSXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .              + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
     .              + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
          SIGSYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .              + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
     .              + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
          SIGSZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .              + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
     .              + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
          SIGSXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
     .              + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .              + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
          SIGSYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
     .              + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .              + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
          SIGSZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
     .              + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .              + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
        ENDDO
      ENDIF
C fin chgt

C COMPUTE UPDATED STRESSES AND SOUND SPEED
      DO 140 I=1,NEL
        SIGNXX(I)=OFF(I)*SIGSXX(I)-SIGAIR(I)
        SIGNYY(I)=OFF(I)*SIGSYY(I)-SIGAIR(I)
        SIGNZZ(I)=OFF(I)*SIGSZZ(I)-SIGAIR(I)
        SIGNXY(I)=OFF(I)*SIGSXY(I)
        SIGNYZ(I)=OFF(I)*SIGSYZ(I)
        SIGNZX(I)=OFF(I)*SIGSZX(I)

        SOUNDSP(I)=SQRT(EY/RHO0(I))
  140 CONTINUE
      GOTO 9000

C--------------------------
    2 CONTINUE
C--------------------------
      C1    = UPARAM(9)
      C2    = UPARAM(10)
      ET    = UPARAM(11)
      VMU   = UPARAM(12)
      VMU0  = UPARAM(13)
      FAC   = UPARAM(14)
      FAC1  = UPARAM(15)

C COMPUTE YOUNG MODULUS, ETC.
      DO 200 I=1,NEL
        EDOT(I)=MAX(
     &          ABS(EPSPXX(I)),ABS(EPSPYY(I)),ABS(EPSPZZ(I)),
     &          ABS(EPSPXY(I)),ABS(EPSPYZ(I)),ABS(EPSPZX(I)))
        E(I)=C1*EDOT(I)+C2
        E(I)=MAX(E(I),EY)
        EPET(I)=(E(I)+ET)/VMU
        EMET(I)=(E(I)*ET)/VMU 
        EPETS(I)=(E(I)+ET)/VMU0
        EMETS(I)=DEUX*(E(I)*ET)/VMU0
  200 CONTINUE

C COMPUTE YIELD STRESS
      DO 210 I=1,NEL
        IF(IFUNC(1).NE.0) THEN
          SYIELD(I)=FAC*FINTER(IFUNC(1),GAMA(I),NPF,TF,B*C)
        ELSE
          SYIELD(I) = ABS(A+B*(UN+C*GAMA(I)))
        ENDIF
        IF(IFUNC(2).NE.0)THEN
           DAV = (EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))*THIRD
           E1 = EPSPXX(I) - DAV
           E2 = EPSPYY(I) - DAV
           E3 = EPSPZZ(I) - DAV
           E4 = UNDEMI*EPSPXY(I)
           E5 = UNDEMI*EPSPYZ(I)
           E6 = UNDEMI*EPSPZX(I)
           EPSP =UNDEMI*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
           EPSP = SQRT(TROIS*EPSP)/TDEMI
           SYIELD(I) = FAC1*SYIELD(I)*(FINTER(IFUNC(2),EPSP,NPF,TF,B*C))
        ENDIF
  210 CONTINUE

      DO 220 I=1,NEL
        SIGSXX(I)=SIGOXX(I)+SIGAIR(I)
        SIGSYY(I)=SIGOYY(I)+SIGAIR(I)
        SIGSZZ(I)=SIGOZZ(I)+SIGAIR(I)
        SIGSXY(I)=SIGOXY(I)
        SIGSYZ(I)=SIGOYZ(I)
        SIGSZX(I)=SIGOZX(I)

        DEXX(I)=EPSXX(I)
        DEYY(I)=EPSYY(I)
        DEZZ(I)=EPSZZ(I)
        DEXY(I)=EPSXY(I)* UNDEMI
        DEYZ(I)=EPSYZ(I)* UNDEMI
        DEZX(I)=EPSZX(I)* UNDEMI

        DEDTXX(I)=EPSPXX(I)
        DEDTYY(I)=EPSPYY(I)
        DEDTZZ(I)=EPSPZZ(I)
        DEDTXY(I)=EPSPXY(I) * UNDEMI
        DEDTYZ(I)=EPSPYZ(I) * UNDEMI
        DEDTZX(I)=EPSPZX(I) * UNDEMI	
  220 CONTINUE

C COMPUTE STRESS RATES
      DO 230 I=1,NEL
        DSDTXX(I)=E(I)*DEDTXX(I)-EPET(I)*SIGSXX(I)+EMET(I)*DEXX(I)
        DSDTYY(I)=E(I)*DEDTYY(I)-EPET(I)*SIGSYY(I)+EMET(I)*DEYY(I)
        DSDTZZ(I)=E(I)*DEDTZZ(I)-EPET(I)*SIGSZZ(I)+EMET(I)*DEZZ(I)
        DSDTXY(I)=E(I)*DEDTXY(I)-EPETS(I)*SIGSXY(I)+EMETS(I)*DEXY(I)
        DSDTYZ(I)=E(I)*DEDTYZ(I)-EPETS(I)*SIGSYZ(I)+EMETS(I)*DEYZ(I)
        DSDTZX(I)=E(I)*DEDTZX(I)-EPETS(I)*SIGSZX(I)+EMETS(I)*DEZX(I)
  230 CONTINUE

C COMPUTE TRIAL STRESSES
      DO 240 I=1,NEL
        SIGSXX(I)=SIGSXX(I)+DSDTXX(I)*TIMESTEP
        SIGSYY(I)=SIGSYY(I)+DSDTYY(I)*TIMESTEP
        SIGSZZ(I)=SIGSZZ(I)+DSDTZZ(I)*TIMESTEP
        SIGSXY(I)=SIGSXY(I)+DSDTXY(I)*TIMESTEP
        SIGSYZ(I)=SIGSYZ(I)+DSDTYZ(I)*TIMESTEP
        SIGSZX(I)=SIGSZX(I)+DSDTZX(I)*TIMESTEP
  240 CONTINUE
      IF(KEN.LT.0)GOTO 255
C
C si ancienne methode alors ....
      IF (VPVEC.EQ.1) THEN
       DO 250 I=1,NEL
        S(1,1)=SIGSXX(I)
        S(2,1)=SIGSXY(I)
        S(2,2)=SIGSYY(I)
        S(3,1)=SIGSZX(I)
        S(3,2)=SIGSYZ(I)
        S(3,3)=SIGSZZ(I)

C TRANSFORM GLOBAL STRESSES (S_IJ) TO PRINCIPAL STRESSES (SIGPR_K)
        CALL JACOBIEW(S,3,SIGPR,DIRPR,NROT)

C YIELD CRITERIA - SCALING
        SIGPR(1)=MIN(SYIELD(I),ABS(SIGPR(1)))*SIGN(UN,SIGPR(1))
        SIGPR(2)=MIN(SYIELD(I),ABS(SIGPR(2)))*SIGN(UN,SIGPR(2))
        SIGPR(3)=MIN(SYIELD(I),ABS(SIGPR(3)))*SIGN(UN,SIGPR(3))

C TRANSFORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
        DO 251 K=1,3
          DO 251 L=1,3
            S(K,L)=ZERO
            IF(K.EQ.L) S(K,L)= SIGPR(K)
  251   CONTINUE
        CALL DREH(S,DIRPR,1,1,1)
   
        SIGSXX(I)=S(1,1)
        SIGSXY(I)=S(2,1)
        SIGSYY(I)=S(2,2)
        SIGSZX(I)=S(3,1)
        SIGSYZ(I)=S(3,2)
        SIGSZZ(I)=S(3,3)
  250  CONTINUE
      ELSE
C fin ancienne methode
        DO I = 1, NEL
          SIGV(I,1) = SIGSXX(I)
          SIGV(I,2) = SIGSYY(I)
          SIGV(I,3) = SIGSZZ(I)
          SIGV(I,4) = SIGSXY(I)
          SIGV(I,5) = SIGSYZ(I)
          SIGV(I,6) = SIGSZX(I)
        ENDDO
        CALL VALPVEC_V(SIGV,SIGPRV,DIRPRV,NEL)
        DO I = 1, NEL
C YIELD CRITERIA - SCALING
          SIGPRV(I,1)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,1))),SIGPRV(I,1))
          SIGPRV(I,2)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,2))),SIGPRV(I,2))
          SIGPRV(I,3)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,3))),SIGPRV(I,3))
C TRANSFORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
          SIGSXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .              + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
     .              + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
          SIGSYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .              + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
     .              + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
          SIGSZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .              + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
     .              + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
          SIGSXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
     .              + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .              + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
          SIGSYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
     .              + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .              + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
          SIGSZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
     .              + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .              + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
        ENDDO
      ENDIF
C fin chgt
  255 CONTINUE
C COMPUTE UPDATED STRESSES AND SOUND SPEED
      DO 260 I=1,NEL
        SIGNXX(I)=OFF(I)*SIGSXX(I)-SIGAIR(I)
        SIGNYY(I)=OFF(I)*SIGSYY(I)-SIGAIR(I)
        SIGNZZ(I)=OFF(I)*SIGSZZ(I)-SIGAIR(I)
        SIGNXY(I)=OFF(I)*SIGSXY(I)
        SIGNYZ(I)=OFF(I)*SIGSYZ(I)
        SIGNZX(I)=OFF(I)*SIGSZX(I)

        SOUNDSP(I)=SQRT(E(I)/RHO0(I))
  260 CONTINUE

      GOTO 9000

C--------------------------
    3 CONTINUE
C--------------------------

      AXX   = UPARAM(9)
      BXX   = UPARAM(10)
      CXX   = UPARAM(11)

      AYY   = UPARAM(12)
      BYY   = UPARAM(13)
      CYY   = UPARAM(14)

      AZZ   = UPARAM(15)
      BZZ   = UPARAM(16)
      CZZ   = UPARAM(17)

      AXY   = UPARAM(18)
      BXY   = UPARAM(19)
      CXY   = UPARAM(20)

      AYZ   = UPARAM(21)
      BYZ   = UPARAM(22)
      CYZ   = UPARAM(23)

      AZX   = UPARAM(24)
      BZX   = UPARAM(25)
      CZX   = UPARAM(26)

      C1XX  = UPARAM(27)
      C2XX  = UPARAM(28)
      ETXX  = UPARAM(29)
      VMUXX = UPARAM(30)

      C1YY  = UPARAM(31)
      C2YY  = UPARAM(32)
      ETYY  = UPARAM(33)
      VMUYY = UPARAM(34)

      C1ZZ  = UPARAM(35)
      C2ZZ  = UPARAM(36)
      ETZZ  = UPARAM(37)
      VMUZZ = UPARAM(38)

      C1XY  = UPARAM(39)
      C2XY  = UPARAM(40)
      GTXY  = UPARAM(41)*DEUX
      VMUXY = UPARAM(42)*DEUX

      C1YZ  = UPARAM(43)
      C2YZ  = UPARAM(44)
      GTYZ  = UPARAM(45)*DEUX
      VMUYZ = UPARAM(46)*DEUX

      C1ZX  = UPARAM(47)
      C2ZX  = UPARAM(48)
      GTZX  = UPARAM(49)*DEUX
      VMUZX = UPARAM(50)*DEUX
      FAC   = UPARAM(51)

      DO 310 I=1,NEL
        EMXX(I)=C1XX*EPSPXX(I)+C2XX
        EMYY(I)=C1YY*EPSPYY(I)+C2YY
        EMZZ(I)=C1ZZ*EPSPZZ(I)+C2ZZ

        GMXY(I)=DEUX*(C1XY*EPSPXY(I)+C2XY)
        GMYZ(I)=DEUX*(C1YZ*EPSPYZ(I)+C2YZ)
        GMZX(I)=DEUX*(C1ZX*EPSPZX(I)+C2ZX)	

        EPETXX(I)=(EMXX(I)+ETXX)/VMUXX
        EMETXX(I)=(EMXX(I)*ETXX)/VMUXX 

        EPETYY(I)=(EMYY(I)+ETYY)/VMUYY
        EMETYY(I)=(EMYY(I)*ETYY)/VMUYY 

        EPETZZ(I)=(EMZZ(I)+ETZZ)/VMUZZ
        EMETZZ(I)=(EMZZ(I)*ETZZ)/VMUZZ
 
        GPGTXY(I)=(GMXY(I)+GTXY)/VMUXY
        GMGTXY(I)=(GMXY(I)*GTXY)/VMUXY 

        GPGTYZ(I)=(GMYZ(I)+GTYZ)/VMUYZ
        GMGTYZ(I)=(GMYZ(I)*GTYZ)/VMUYZ
 
        GPGTZX(I)=(GMZX(I)+GTZX)/VMUZX
        GMGTZX(I)=(GMZX(I)*GTZX)/VMUZX 

  310 CONTINUE

C COMPUTE SKELETAL STRESSES
      DO 320 I=1,NEL
        SIGSXX(I)=SIGOXX(I)+SIGAIR(I)
        SIGSYY(I)=SIGOYY(I)+SIGAIR(I)
        SIGSZZ(I)=SIGOZZ(I)+SIGAIR(I)
        SIGSXY(I)=SIGOXY(I)
        SIGSYZ(I)=SIGOYZ(I)
        SIGSZX(I)=SIGOZX(I)

        DEXX(I)=EPSXX(I)
        DEYY(I)=EPSYY(I)
        DEZZ(I)=EPSZZ(I)
        DEXY(I)=EPSXY(I) * UNDEMI
        DEYZ(I)=EPSYZ(I) * UNDEMI
        DEZX(I)=EPSZX(I) * UNDEMI	

        DEDTXX(I)=EPSPXX(I)
        DEDTYY(I)=EPSPYY(I)
        DEDTZZ(I)=EPSPZZ(I)
        DEDTXY(I)=EPSPXY(I) * UNDEMI
        DEDTYZ(I)=EPSPYZ(I) * UNDEMI
        DEDTZX(I)=EPSPZX(I) * UNDEMI	
  320 CONTINUE

      DO 330 I=1,NEL
      DSDTXX(I)=EMXX(I)*DEDTXX(I)-EPETXX(I)*SIGSXX(I)+EMETXX(I)*DEXX(I)
      DSDTYY(I)=EMYY(I)*DEDTYY(I)-EPETYY(I)*SIGSYY(I)+EMETYY(I)*DEYY(I)
      DSDTZZ(I)=EMZZ(I)*DEDTZZ(I)-EPETZZ(I)*SIGSZZ(I)+EMETZZ(I)*DEZZ(I)
      DSDTXY(I)=GMXY(I)*DEDTXY(I)-GPGTXY(I)*SIGSXY(I)+GMGTXY(I)*DEXY(I)
      DSDTYZ(I)=GMYZ(I)*DEDTYZ(I)-GPGTYZ(I)*SIGSYZ(I)+GMGTYZ(I)*DEYZ(I)
      DSDTZX(I)=GMZX(I)*DEDTZX(I)-GPGTZX(I)*SIGSZX(I)+GMGTZX(I)*DEZX(I)
  330 CONTINUE

      DO 340 I=1,NEL
        SIGSXX(I)=SIGSXX(I)+DSDTXX(I)*TIMESTEP
        SIGSYY(I)=SIGSYY(I)+DSDTYY(I)*TIMESTEP
        SIGSZZ(I)=SIGSZZ(I)+DSDTZZ(I)*TIMESTEP
        SIGSXY(I)=SIGSXY(I)+DSDTXY(I)*TIMESTEP
        SIGSYZ(I)=SIGSYZ(I)+DSDTXY(I)*TIMESTEP
        SIGSZX(I)=SIGSZX(I)+DSDTYZ(I)*TIMESTEP
  340 CONTINUE

        IF(IFUNC(1).NE.0) THEN
          SYXX(I)=FAC*FINTER(IFUNC(1),GAMA(I),NPF,TF,BXX*CXX)
        ELSE
          SYXX(I)=AXX+BXX*(1+CXX*GAMA(I))
        ENDIF

        IF(IFUNC(2).NE.0) THEN
          SYYY(I)=FAC*FINTER(IFUNC(2),GAMA(I),NPF,TF,BYY*CYY)
        ELSE
          SYYY(I)=AYY+BYY*(1+CYY*GAMA(I))
        ENDIF

        IF(IFUNC(3).NE.0) THEN
          SYZZ(I)=FAC*FINTER(IFUNC(3),GAMA(I),NPF,TF,BZZ*CZZ)
        ELSE
          SYZZ(I)=AZZ+BZZ*(1+CZZ*GAMA(I))
        ENDIF

        IF(IFUNC(4).NE.0) THEN
          SYXY(I)=FAC*FINTER(IFUNC(4),GAMA(I),NPF,TF,BXY*CXY)
        ELSE
          SYXY(I)=AXY+BXY*(1+CXY*GAMA(I))
        ENDIF

        IF(IFUNC(5).NE.0) THEN
          SYYZ(I)=FAC*FINTER(IFUNC(5),GAMA(I),NPF,TF,BYZ*CYZ)
        ELSE
          SYYZ(I)=AYZ+BYZ*(1+CYZ*GAMA(I))
        ENDIF

        IF(IFUNC(6).NE.0) THEN
          SYZX(I)=FAC*FINTER(IFUNC(6),GAMA(I),NPF,TF,BZX*CZX)
        ELSE
          SYZX(I)=AZX+BZX*(1+CZX*GAMA(I))
        ENDIF

C YIELD CRITERIA - SCALING
        SIGSXX(I)=MIN(ABS(SYXX(I)),ABS(SIGSXX(I)))*SIGN(UN,SIGSXX(I))
        SIGSYY(I)=MIN(ABS(SYYY(I)),ABS(SIGSYY(I)))*SIGN(UN,SIGSYY(I))
        SIGSZZ(I)=MIN(ABS(SYZZ(I)),ABS(SIGSZZ(I)))*SIGN(UN,SIGSZZ(I))
        SIGSXY(I)=MIN(ABS(SYXY(I)),ABS(SIGSXY(I)))*SIGN(UN,SIGSXY(I))
        SIGSYZ(I)=MIN(ABS(SYYZ(I)),ABS(SIGSYZ(I)))*SIGN(UN,SIGSYZ(I))
        SIGSZX(I)=MIN(ABS(SYZX(I)),ABS(SIGSZX(I)))*SIGN(UN,SIGSZX(I))

C COMPUTE UPDATED STRESSES AND SOUND SPEED
      DO 350 I=1,NEL
        SIGNXX(I)=OFF(I)*SIGSXX(I)-SIGAIR(I)
        SIGNYY(I)=OFF(I)*SIGSYY(I)-SIGAIR(I)
        SIGNZZ(I)=OFF(I)*SIGSZZ(I)-SIGAIR(I)
        SIGNXY(I)=OFF(I)*SIGSXY(I)
        SIGNYZ(I)=OFF(I)*SIGSYZ(I)
        SIGNZX(I)=OFF(I)*SIGSZX(I)

        SOUNDSP(I)=SQRT(MAX(EMXX(I),EMYY(I),EMZZ(I))/RHO0(I))

  350 CONTINUE
        
 9000 CONTINUE

      RETURN

      END

Chd|====================================================================
Chd|  JACOBIEW                      source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  JACOBIEW(A,N,EW,EV,NROT)
C-------------------------------------------------------KK141189-
C COMPUTATION OF ALL EIGENVALUES AND EIGENVECTORS OF A SYMMETRIC
C MATRIX A BY THE JACOBI ALGORITHM
C
C A(N,N)      EIGENWERTPROBLEM
C N           DIMENSION OF A
C EW(N)       EIGENVALUES
C EV(N,N)     EIGENVEKTORS
C NROT        NUMBER OF ROTAIONS
C MAXA        MAXIMUM ELEMENT OF A
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER NN
      PARAMETER   (NN=9)

      INTEGER     N,NROT
      my_real
     . A(N,N), EW(N), EV(N,N)
     . , B(NN), Z(NN)
      INTEGER IZ,IS,ITER,J
      my_real
     . SUMRS,EPS,G,H,T,C,S,TAU,THETA      
C----------------------------------------------------------------
      DO 130 IZ=1,N
        DO 120 IS=1,N
C PRACTICAL FOR RADIOSS (PASS ONLY LOWER DIAGONAL MATRIX)
          IF(IZ.GT.IS) A(IS,IZ) = A(IZ,IS) 
          EV(IZ,IS)=ZERO
  120   CONTINUE
        B(IZ)=A(IZ,IZ)
        EW(IZ)=B(IZ)
        Z(IZ)=0.
        EV(IZ,IZ)=UN
  130 CONTINUE

      NROT=0

C START ITERATION
 
      DO 240 ITER = 1,50
        SUMRS = ZERO

C SUM OF THE OFF DIAGONALS
        DO 150 IZ=1,N-1
          DO 140 IS=IZ+1,N
            SUMRS=SUMRS+ABS(A(IZ,IS))
  140     CONTINUE
  150   CONTINUE
 
        IF (SUMRS .EQ.ZERO) GOTO 9000
        IF (ITER .GT. 4)   THEN
          EPS = ZERO
        ELSE
          EPS = UNCINQ*SUMRS/N**2
        ENDIF

        DO 220 IZ=1,N-1
          DO 210 IS=IZ+1,N
            G = 100. * ABS(A(IZ,IS))
            IF (ITER.GT.4 .AND. ABS(EW(IZ))+G.EQ.ABS(EW(IZ))
     &      .AND. ABS(EW(IS))+G.EQ.ABS(EW(IS))) THEN
              A(IZ,IS)=ZERO
            ELSE IF (ABS(A(IZ,IS)) .GT. EPS) THEN
              H = EW(IS)-EW(IZ)
              IF (ABS(H)+G.EQ.ABS(H)) THEN
                T = A(IZ,IS)/H
              ELSE
                THETA = UNDEMI*H/A(IZ,IS)
                T=UN/(ABS(THETA)+SQRT(UN+THETA**2))
                IF (THETA .LT. ZERO) T=-T
              ENDIF
              C=UN/SQRT(UN+T**2)
              S=T*C
              TAU=S/(UN+C)
              H=T*A(IZ,IS)
              Z(IZ)=Z(IZ)-H
              Z(IS)=Z(IS)+H
              EW(IZ)=EW(IZ)-H
              EW(IS)=EW(IS)+H
              A(IZ,IS)=ZERO
              DO 160 J=1,IZ-1
                G=A(J,IZ)
                H=A(J,IS)
                A(J,IZ)=G-S*(H+G*TAU)
                A(J,IS)=H+S*(G-H*TAU)
  160         CONTINUE
              DO 170 J=IZ+1,IS-1
                G=A(IZ,J)
                H=A(J,IS)
                A(IZ,J)=G-S*(H+G*TAU)
                A(J,IS)=H+S*(G-H*TAU)
  170         CONTINUE
              DO 180 J=IS+1,N
                G=A(IZ,J)
                H=A(IS,J)
                A(IZ,J)=G-S*(H+G*TAU)
                A(IS,J)=H+S*(G-H*TAU)
  180         CONTINUE
              DO 190 J=1,N
                G=EV(J,IZ)
                H=EV(J,IS)
                EV(J,IZ)=G-S*(H+G*TAU)
                EV(J,IS)=H+S*(G-H*TAU)
  190         CONTINUE
              NROT=NROT+1
            ENDIF
  210     CONTINUE
  220   CONTINUE

        DO 230 IZ=1,N
          B(IZ)=B(IZ)+Z(IZ)
          EW(IZ)=B(IZ)
          Z(IZ)=ZERO
  230   CONTINUE
  240 CONTINUE

 9000 CONTINUE

      RETURN

      END
 
      
Chd|====================================================================
Chd|  DREH                          source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DREH(B,TR,II,JJ,KEN) 
C-------------------------------------------------------KK010587-
C KEN=0 -> ROTATION OF MATRIX B BY TR = TR(T)*B*TR
C KEN=1 -> ROTATION OF MATRIX B BY TR(T) = TR*B*TR(T)
C II AND JJ POINT TO THE SUBMATRIX TO BE ROTATED; DEFAULT II=JJ=1
C----------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      my_real
     . B(3,3),TR(3,3),LK(3,3),X
      INTEGER KEN,II,JJ
      INTEGER I,J,K,I1,J1
C----------------------------------------------------------------
      IF(II.LE.0) II=1
      IF(JJ.LE.0) JJ=1
                       
      DO 20 I=1,3                                                               
        DO 20 J=1,3                                                               
          J1=JJ+J-1                                                                 
          X=0.0                                                                     
          DO 10 K=1,3                                                               
C          IF(TR(K,I).EQ.ZERO) GOTO 10                                               
           I1=K+II-1                                                                 
C          IF(B(I1,J1).EQ.ZERO) GOTO 10                                             
           IF(KEN.NE.1) X=X+TR(K,I)*B(I1,J1)                                                   
           IF(KEN.EQ.1) X=X+TR(I,K)*B(I1,J1) 
 10       CONTINUE                                                                  
 20   LK(I,J)=X
                                                                 
      DO 40 I=1,3                                                               
        DO 40 J=1,3                                                               
          X=ZERO                                                       
          DO 30 K=1,3                                                               
C          IF(TR(K,J).EQ.ZERO) GOTO 30                                               
C          IF(LK(I,K).EQ.ZERO) GOTO 30                                                
           IF(KEN.NE.1) X=X+LK(I,K)*TR(K,J)                                                      
           IF(KEN.EQ.1) X=X+LK(I,K)*TR(J,K)                                                      
 30       CONTINUE                                                                  
 40   B(II+I-1,JJ+J-1)=X
                                                      
      RETURN
                                                                    
      END 
C
Chd|====================================================================
Chd|  VALPVEC                       source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        EPSF2U                        source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS88                      source/materials/mat/mat088/sigeps88.F
Chd|        SIGEPS90                      source/materials/mat/mat090/sigeps90.F
Chd|-- calls ---------------
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|====================================================================
      SUBROUTINE VALPVEC(SIG,VAL,VEC,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(6,*), VAL(3,*), VEC(9,*)
      INTEGER NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, N, NN, LMAX, LMAXV(MVSIZ),NINDEX3, INDEX3(MVSIZ),
     .        NINDEX1, NINDEX2, INDEX1(MVSIZ), INDEX2(MVSIZ)
      my_real
     .   CS(6), STR(3,MVSIZ), A(3,3,MVSIZ), V(3,3,MVSIZ), B(3,3,MVSIZ),
     .   XMAG(3,MVSIZ), PR, AA, BB, AAA(MVSIZ),
     .   CC, ANGP, DD, FTPI, TTPI, STRMAX,
     .   TOL1(MVSIZ), TOL2(MVSIZ), XMAXV(MVSIZ), NORMINF(MVSIZ), 
     .   VMAG, S11,
     .   S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .   A22, A23, A31, A32, A33,
     .   MDEMI, XMAXINV, FLM
      REAL FLMIN
C-----------------------------------------------
C     DATA FTPI,TTPI / 4.188790205, 2.094395102 /
C     FTPI=(4/3)*PI, TTPI=(2/3)*PI
C
C     DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
      MDEMI = -UNDEMI
      TTPI = ACOS(MDEMI)
      FTPI = DEUX*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = DEUX*SQRT(FLMIN)
      NINDEX3=0  
      DO NN = 1, NEL
        CS(1) = SIG(1,NN)
        CS(2) = SIG(2,NN)
        CS(3) = SIG(3,NN)
        CS(4) = SIG(4,NN)
        CS(5) = SIG(5,NN)
        CS(6) = SIG(6,NN)
        PR = -(CS(1)+CS(2)+CS(3))* THIRD
        CS(1) = CS(1) + PR
        CS(2) = CS(2) + PR
        CS(3) = CS(3) + PR
        AAA(NN)=CS(4)**2 + CS(5)**2 + CS(6)**2 - CS(1)*CS(2)
     &      - CS(2)*CS(3) - CS(1)*CS(3)
        NORMINF(NN) = MAX(ABS(CS(1)),ABS(CS(2)),ABS(CS(3)),
     &      ABS(CS(4)),ABS(CS(5)),ABS(CS(6)))
        NORMINF(NN) = EM10*NORMINF(NN)
C cas racine triple
c        AA = MAX(AAA(NN),NORMINF(NN),EM20)
        AA = MAX(AAA(NN),NORMINF(NN))
C
        BB=CS(1)*CS(5)**2 + CS(2)*CS(6)**2
     &     + CS(3)*CS(4)**2 - CS(1)*CS(2)*CS(3)
     &     - DEUX*CS(4)*CS(5)*CS(6)     
C
        CC=-SQRT(VINGT7/MAX(EM20,AA))*BB*UNDEMI/MAX(EM20,AA)
        CC= MIN(CC,UN)
        CC= MAX(CC,-UN)
        ANGP=ACOS(CC) * THIRD
        DD=DEUX*SQRT(AA * THIRD)
        STR(1,NN)=DD*COS(ANGP)
        STR(2,NN)=DD*COS(ANGP+FTPI)
        STR(3,NN)=DD*COS(ANGP+TTPI)
C
        VAL(1,NN) = STR(1,NN)-PR
        VAL(2,NN) = STR(2,NN)-PR
        VAL(3,NN) = STR(3,NN)-PR
C renforcement de precision en compression----
        IF(ABS(STR(3,NN)).GT.ABS(STR(1,NN))
     &     .AND.AAA(NN).GT.NORMINF(NN)) THEN
         AA=STR(1,NN)
         STR(1,NN)=STR(3,NN)
         STR(3,NN)=AA
         NINDEX3 = NINDEX3+1
         INDEX3(NINDEX3) = NN
        ENDIF
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR(1,NN)),ABS(STR(3,NN)))
        TOL1(NN)= MAX(EM20,FLM*STRMAX**2)
        TOL2(NN)=FLM*STRMAX/3
        A(1,1,NN)=CS(1)-STR(1,NN)
        A(2,2,NN)=CS(2)-STR(1,NN)
        A(3,3,NN)=CS(3)-STR(1,NN)
        A(1,2,NN)=CS(4)
        A(2,1,NN)=CS(4)
        A(2,3,NN)=CS(5)
        A(3,2,NN)=CS(5)
        A(1,3,NN)=CS(6)
        A(3,1,NN)=CS(6)
C
        B(1,1,NN)=A(2,1,NN)*A(3,2,NN)-A(3,1,NN)
     .           *A(2,2,NN)
        B(1,2,NN)=A(2,2,NN)*A(3,3,NN)-A(3,2,NN)
     .           *A(2,3,NN)
        B(1,3,NN)=A(2,3,NN)*A(3,1,NN)-A(3,3,NN)
     .           *A(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*A(1,2,NN)-A(1,1,NN)
     .           *A(3,2,NN)
        B(2,2,NN)=A(3,2,NN)*A(1,3,NN)-A(1,2,NN)
     .           *A(3,3,NN)
        B(2,3,NN)=A(3,3,NN)*A(1,1,NN)-A(1,3,NN)
     .           *A(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*A(2,2,NN)-A(2,1,NN)
     .           *A(1,2,NN)
        B(3,2,NN)=A(1,2,NN)*A(2,3,NN)-A(2,2,NN)
     .           *A(1,3,NN)
        B(3,3,NN)=A(1,3,NN)*A(2,1,NN)-A(2,3,NN)
     .           *A(1,1,NN)
        XMAG(1,NN)=SQRT(B(1,1,NN)**2+B(2,1,NN)**2+B(3,1,NN)**2)
        XMAG(2,NN)=SQRT(B(1,2,NN)**2+B(2,2,NN)**2+B(3,2,NN)**2)
        XMAG(3,NN)=SQRT(B(1,3,NN)**2+B(2,3,NN)**2+B(3,3,NN)**2)

      ENDDO
C 
      NINDEX1 = 0
      NINDEX2 = 0
      DO NN = 1, NEL
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN).LT.NORMINF(NN)) THEN
          VAL(1,NN) = SIG(1,NN)
          VAL(2,NN) = SIG(2,NN)
          VAL(3,NN) = SIG(3,NN)
          V(1,1,NN) = UN
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = UN
          V(3,2,NN) = ZERO

        ELSEIF(XMAXV(NN).GT.TOL1(NN)) THEN
          NINDEX1 = NINDEX1 + 1
          INDEX1(NINDEX1) = NN
        ELSE
          NINDEX2 = NINDEX2 + 1
          INDEX2(NINDEX2) = NN
        ENDIF
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        LMAX = LMAXV(NN)
        XMAXINV = UN/XMAXV(NN)
        V(1,1,NN)=B(1,LMAX,NN)*XMAXINV
        V(2,1,NN)=B(2,LMAX,NN)*XMAXINV
        V(3,1,NN)=B(3,LMAX,NN)*XMAXINV
        A(1,1,NN)=A(1,1,NN)+STR(1,NN)-STR(3,NN)
        A(2,2,NN)=A(2,2,NN)+STR(1,NN)-STR(3,NN)
        A(3,3,NN)=A(3,3,NN)+STR(1,NN)-STR(3,NN)
C
        B(1,1,NN)=A(2,1,NN)*V(3,1,NN)-A(3,1,NN)*V(2,1,NN)
        B(1,2,NN)=A(2,2,NN)*V(3,1,NN)-A(3,2,NN)*V(2,1,NN)
        B(1,3,NN)=A(2,3,NN)*V(3,1,NN)-A(3,3,NN)*V(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*V(1,1,NN)-A(1,1,NN)*V(3,1,NN)
        B(2,2,NN)=A(3,2,NN)*V(1,1,NN)-A(1,2,NN)*V(3,1,NN)
        B(2,3,NN)=A(3,3,NN)*V(1,1,NN)-A(1,3,NN)*V(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*V(2,1,NN)-A(2,1,NN)*V(1,1,NN)
        B(3,2,NN)=A(1,2,NN)*V(2,1,NN)-A(2,2,NN)*V(1,1,NN)
        B(3,3,NN)=A(1,3,NN)*V(2,1,NN)-A(2,3,NN)*V(1,1,NN)
        XMAG(1,NN)=SQRT(B(1,1,NN)**2+B(2,1,NN)**2+B(3,1,NN)**2)
        XMAG(2,NN)=SQRT(B(1,2,NN)**2+B(2,2,NN)**2+B(3,2,NN)**2)
        XMAG(3,NN)=SQRT(B(1,3,NN)**2+B(2,3,NN)**2+B(3,3,NN)**2)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(1,1,NN)**2+V(2,1,NN)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,3,NN)=B(1,LMAX,NN)*XMAXINV
          V(2,3,NN)=B(2,LMAX,NN)*XMAXINV
          V(3,3,NN)=B(3,LMAX,NN)*XMAXINV
          V(1,2,NN)=V(2,3,NN)*V(3,1,NN)-V(2,1,NN)*V(3,3,NN)
          V(2,2,NN)=V(3,3,NN)*V(1,1,NN)-V(3,1,NN)*V(1,3,NN)
          V(3,2,NN)=V(1,3,NN)*V(2,1,NN)-V(1,1,NN)*V(2,3,NN)
          VMAG=UN/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSEIF(VMAG.GT.TOL2(NN))THEN
          V(1,2,NN)=-V(2,1,NN)/VMAG
          V(2,2,NN)=V(1,1,NN)/VMAG
          V(3,2,NN)=ZERO
        ELSE
          V(1,2,NN)=UN
          V(2,2,NN)=ZERO
          V(3,2,NN)=ZERO  
        ENDIF
      ENDDO
C . . . . . . . . . . . . .
C    SOLUTION DOUBLE
C . . . . . . . . . . . . .
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        XMAG(1,NN)=SQRT(A(1,1,NN)**2+A(2,1,NN)**2)
        XMAG(2,NN)=SQRT(A(1,2,NN)**2+A(2,2,NN)**2)
        XMAG(3,NN)=SQRT(A(1,3,NN)**2+A(2,3,NN)**2)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(3,1,NN)),ABS(A(3,2,NN)),ABS(A(3,3,NN)))
     &       .LT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,1,NN)= ZERO
          V(2,1,NN)= ZERO
          V(3,1,NN)= UN
          V(1,2,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,2,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,2,NN)= ZERO
C
        ELSEIF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,1,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,1,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,1,NN)= ZERO
          V(1,2,NN)=-A(3,LMAX,NN)*V(2,1,NN)
          V(2,2,NN)= A(3,LMAX,NN)*V(1,1,NN)
          V(3,2,NN)= A(1,LMAX,NN)*V(2,1,NN)-A(2,LMAX,NN)*V(1,1,NN)
          VMAG=UN/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSE
          V(1,1,NN) = UN
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = UN
          V(3,2,NN) = ZERO	  
        ENDIF
      ENDDO
C
      DO NN = 1, NEL
        VEC(1,NN)=V(1,1,NN)
        VEC(2,NN)=V(2,1,NN)
        VEC(3,NN)=V(3,1,NN)
        VEC(4,NN)=V(1,2,NN)
        VEC(5,NN)=V(2,2,NN)
        VEC(6,NN)=V(3,2,NN)
        VEC(7,NN)=VEC(2,NN)*VEC(6,NN)-VEC(3,NN)*VEC(5,NN)
        VEC(8,NN)=VEC(3,NN)*VEC(4,NN)-VEC(1,NN)*VEC(6,NN)
        VEC(9,NN)=VEC(1,NN)*VEC(5,NN)-VEC(2,NN)*VEC(4,NN)
      ENDDO
C reecriture pour contourner probleme sur itanium2 comp 9. + latency=16
      DO N = 1, NINDEX3
        NN = INDEX3(N)
C str utilise com tableau temporaire au lieu de scalaires temporaires
        STR(1,NN)=VEC(7,NN)
        STR(2,NN)=VEC(8,NN)
        STR(3,NN)=VEC(9,NN)
      ENDDO
      DO N = 1, NINDEX3
        NN = INDEX3(N)
        VEC(7,NN)=VEC(1,NN)
        VEC(8,NN)=VEC(2,NN)
        VEC(9,NN)=VEC(3,NN)
        VEC(1,NN)=-STR(1,NN)
        VEC(2,NN)=-STR(2,NN)
        VEC(3,NN)=-STR(3,NN)
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  VALPVECDP                     source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        EPSF2U                        source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS88                      source/materials/mat/mat088/sigeps88.F
Chd|        SIGEPS90                      source/materials/mat/mat090/sigeps90.F
Chd|-- calls ---------------
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|====================================================================
      SUBROUTINE VALPVECDP(SIG,VAL,VEC,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(6,*), VAL(3,*), VEC(9,*)
      INTEGER NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, N, NN, LMAX, LMAXV(MVSIZ),NINDEX3, INDEX3(MVSIZ),
     .        NINDEX1, NINDEX2, INDEX1(MVSIZ), INDEX2(MVSIZ)
      DOUBLE PRECISION
     .   CS(6), STR(3,MVSIZ), A(3,3,MVSIZ), V(3,3,MVSIZ), B(3,3,MVSIZ),
     .   XMAG(3,MVSIZ), PR, AA, BB, AAA(MVSIZ),
     .   CC, ANGP, DD, FTPI, TTPI, STRMAX,
     .   TOL1(MVSIZ), TOL2(MVSIZ), XMAXV(MVSIZ), NORMINF(MVSIZ), 
     .   VMAG, S11,
     .   S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .   A22, A23, A31, A32, A33,
     .   MDEMI, XMAXINV, FLM,
     .   VALDP(3,MVSIZ),VECDP(9,MVSIZ)
      REAL FLMIN
C-----------------------------------------------
C     DATA FTPI,TTPI / 4.188790205, 2.094395102 /
C     FTPI=(4/3)*PI, TTPI=(2/3)*PI
C
C     DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
      MDEMI = -UNDEMI
      TTPI = ACOS(MDEMI)
      FTPI = DEUX*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = DEUX*SQRT(FLMIN)
      NINDEX3=0  
      DO NN = 1, NEL
        CS(1) = SIG(1,NN)
        CS(2) = SIG(2,NN)
        CS(3) = SIG(3,NN)
        CS(4) = SIG(4,NN)
        CS(5) = SIG(5,NN)
        CS(6) = SIG(6,NN)
        PR = -(CS(1)+CS(2)+CS(3)) * THIRD
        CS(1) = CS(1) + PR
        CS(2) = CS(2) + PR
        CS(3) = CS(3) + PR
        AAA(NN)=CS(4)**2 + CS(5)**2 + CS(6)**2 - CS(1)*CS(2)
     &      - CS(2)*CS(3) - CS(1)*CS(3)
        NORMINF(NN) = MAX(ABS(CS(1)),ABS(CS(2)),ABS(CS(3)),
     &      ABS(CS(4)),ABS(CS(5)),ABS(CS(6)))
        NORMINF(NN) = EM10*NORMINF(NN)
C cas racine triple
c        AA = MAX(AAA(NN),NORMINF(NN),EM20)
        AA = MAX(AAA(NN),NORMINF(NN))
C
        BB=CS(1)*CS(5)**2 + CS(2)*CS(6)**2
     &     + CS(3)*CS(4)**2 - CS(1)*CS(2)*CS(3)
     &     - DEUX*CS(4)*CS(5)*CS(6)
C
        CC=-SQRT(VINGT7/MAX(EM20,AA))*BB*UNDEMI/MAX(EM20,AA)
        CC= MIN(CC,UN)
        CC= MAX(CC,-UN)
        ANGP=ACOS(CC) * THIRD
        DD=DEUX*SQRT(AA * THIRD)
        STR(1,NN)=DD*COS(ANGP)
        STR(2,NN)=DD*COS(ANGP+FTPI)
        STR(3,NN)=DD*COS(ANGP+TTPI)
C
        VALDP(1,NN) = STR(1,NN)-PR
        VALDP(2,NN) = STR(2,NN)-PR
        VALDP(3,NN) = STR(3,NN)-PR
C renforcement de precision en compression simple----
        IF(ABS(STR(3,NN)).GT.ABS(STR(1,NN))
     &     .AND.AAA(NN).GT.NORMINF(NN)) THEN
         AA=STR(1,NN)
         STR(1,NN)=STR(3,NN)
         STR(3,NN)=AA
         NINDEX3 = NINDEX3+1
         INDEX3(NINDEX3) = NN
        ENDIF
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR(1,NN)),ABS(STR(3,NN)))
        TOL1(NN)= MAX(EM20,FLM*STRMAX**2)
        TOL2(NN)=FLM*STRMAX * THIRD
        A(1,1,NN)=CS(1)-STR(1,NN)
        A(2,2,NN)=CS(2)-STR(1,NN)
        A(3,3,NN)=CS(3)-STR(1,NN)
        A(1,2,NN)=CS(4)
        A(2,1,NN)=CS(4)
        A(2,3,NN)=CS(5)
        A(3,2,NN)=CS(5)
        A(1,3,NN)=CS(6)
        A(3,1,NN)=CS(6)
C
        B(1,1,NN)=A(2,1,NN)*A(3,2,NN)-A(3,1,NN)
     .           *A(2,2,NN)
        B(1,2,NN)=A(2,2,NN)*A(3,3,NN)-A(3,2,NN)
     .           *A(2,3,NN)
        B(1,3,NN)=A(2,3,NN)*A(3,1,NN)-A(3,3,NN)
     .           *A(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*A(1,2,NN)-A(1,1,NN)
     .           *A(3,2,NN)
        B(2,2,NN)=A(3,2,NN)*A(1,3,NN)-A(1,2,NN)
     .           *A(3,3,NN)
        B(2,3,NN)=A(3,3,NN)*A(1,1,NN)-A(1,3,NN)
     .           *A(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*A(2,2,NN)-A(2,1,NN)
     .           *A(1,2,NN)
        B(3,2,NN)=A(1,2,NN)*A(2,3,NN)-A(2,2,NN)
     .           *A(1,3,NN)
        B(3,3,NN)=A(1,3,NN)*A(2,1,NN)-A(2,3,NN)
     .           *A(1,1,NN)
        XMAG(1,NN)=SQRT(B(1,1,NN)**2+B(2,1,NN)**2+B(3,1,NN)**2)
        XMAG(2,NN)=SQRT(B(1,2,NN)**2+B(2,2,NN)**2+B(3,2,NN)**2)
        XMAG(3,NN)=SQRT(B(1,3,NN)**2+B(2,3,NN)**2+B(3,3,NN)**2)

      ENDDO
C 
      NINDEX1 = 0
      NINDEX2 = 0
      DO NN = 1, NEL
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN).LT.NORMINF(NN)) THEN
          VALDP(1,NN) = SIG(1,NN)
          VALDP(2,NN) = SIG(2,NN)
          VALDP(3,NN) = SIG(3,NN)
          V(1,1,NN) = UN
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = UN
          V(3,2,NN) = ZERO	  
        ELSEIF(XMAXV(NN).GT.TOL1(NN)) THEN
          NINDEX1 = NINDEX1 + 1
          INDEX1(NINDEX1) = NN
        ELSE
          NINDEX2 = NINDEX2 + 1
          INDEX2(NINDEX2) = NN
        ENDIF
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        LMAX = LMAXV(NN)
        XMAXINV = UN/XMAXV(NN)
        V(1,1,NN)=B(1,LMAX,NN)*XMAXINV
        V(2,1,NN)=B(2,LMAX,NN)*XMAXINV
        V(3,1,NN)=B(3,LMAX,NN)*XMAXINV
        A(1,1,NN)=A(1,1,NN)+STR(1,NN)-STR(3,NN)
        A(2,2,NN)=A(2,2,NN)+STR(1,NN)-STR(3,NN)
        A(3,3,NN)=A(3,3,NN)+STR(1,NN)-STR(3,NN)
C
        B(1,1,NN)=A(2,1,NN)*V(3,1,NN)-A(3,1,NN)*V(2,1,NN)
        B(1,2,NN)=A(2,2,NN)*V(3,1,NN)-A(3,2,NN)*V(2,1,NN)
        B(1,3,NN)=A(2,3,NN)*V(3,1,NN)-A(3,3,NN)*V(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*V(1,1,NN)-A(1,1,NN)*V(3,1,NN)
        B(2,2,NN)=A(3,2,NN)*V(1,1,NN)-A(1,2,NN)*V(3,1,NN)
        B(2,3,NN)=A(3,3,NN)*V(1,1,NN)-A(1,3,NN)*V(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*V(2,1,NN)-A(2,1,NN)*V(1,1,NN)
        B(3,2,NN)=A(1,2,NN)*V(2,1,NN)-A(2,2,NN)*V(1,1,NN)
        B(3,3,NN)=A(1,3,NN)*V(2,1,NN)-A(2,3,NN)*V(1,1,NN)
        XMAG(1,NN)=SQRT(B(1,1,NN)**2+B(2,1,NN)**2+B(3,1,NN)**2)
        XMAG(2,NN)=SQRT(B(1,2,NN)**2+B(2,2,NN)**2+B(3,2,NN)**2)
        XMAG(3,NN)=SQRT(B(1,3,NN)**2+B(2,3,NN)**2+B(3,3,NN)**2)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(1,1,NN)**2+V(2,1,NN)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,3,NN)=B(1,LMAX,NN)*XMAXINV
          V(2,3,NN)=B(2,LMAX,NN)*XMAXINV
          V(3,3,NN)=B(3,LMAX,NN)*XMAXINV
          V(1,2,NN)=V(2,3,NN)*V(3,1,NN)-V(2,1,NN)*V(3,3,NN)
          V(2,2,NN)=V(3,3,NN)*V(1,1,NN)-V(3,1,NN)*V(1,3,NN)
          V(3,2,NN)=V(1,3,NN)*V(2,1,NN)-V(1,1,NN)*V(2,3,NN)
          VMAG=UN/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSEIF(VMAG.GT.TOL2(NN))THEN
          V(1,2,NN)=-V(2,1,NN)/VMAG
          V(2,2,NN)=V(1,1,NN)/VMAG
          V(3,2,NN)=ZERO
        ELSE
          V(1,2,NN)=UN
          V(2,2,NN)=ZERO
          V(3,2,NN)=ZERO  
        ENDIF
      ENDDO
C . . . . . . . . . . . . .
C    SOLUTION DOUBLE
C . . . . . . . . . . . . .
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        XMAG(1,NN)=SQRT(A(1,1,NN)**2+A(2,1,NN)**2)
        XMAG(2,NN)=SQRT(A(1,2,NN)**2+A(2,2,NN)**2)
        XMAG(3,NN)=SQRT(A(1,3,NN)**2+A(2,3,NN)**2)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(3,1,NN)),ABS(A(3,2,NN)),ABS(A(3,3,NN)))
     &       .LT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,1,NN)= ZERO
          V(2,1,NN)= ZERO
          V(3,1,NN)= UN  
          V(1,2,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,2,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,2,NN)= ZERO
C
        ELSEIF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,1,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,1,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,1,NN)= ZERO
          V(1,2,NN)=-A(3,LMAX,NN)*V(2,1,NN)
          V(2,2,NN)= A(3,LMAX,NN)*V(1,1,NN)
          V(3,2,NN)= A(1,LMAX,NN)*V(2,1,NN)-A(2,LMAX,NN)*V(1,1,NN)
          VMAG=UN/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSE
          V(1,1,NN) = UN
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = UN
          V(3,2,NN) = ZERO	  
        ENDIF
      ENDDO
C
      DO NN = 1, NEL
        VECDP(1,NN)=V(1,1,NN)
        VECDP(2,NN)=V(2,1,NN)
        VECDP(3,NN)=V(3,1,NN)
        VECDP(4,NN)=V(1,2,NN)
        VECDP(5,NN)=V(2,2,NN)
        VECDP(6,NN)=V(3,2,NN)
        VECDP(7,NN)=VECDP(2,NN)*VECDP(6,NN)-VECDP(3,NN)*VECDP(5,NN)
        VECDP(8,NN)=VECDP(3,NN)*VECDP(4,NN)-VECDP(1,NN)*VECDP(6,NN)
        VECDP(9,NN)=VECDP(1,NN)*VECDP(5,NN)-VECDP(2,NN)*VECDP(4,NN)
      ENDDO
C
      DO NN = 1, NEL
        VAL(1,NN)=VALDP(1,NN)
        VAL(2,NN)=VALDP(2,NN)
        VAL(3,NN)=VALDP(3,NN)
        VEC(1,NN)=VECDP(1,NN)
        VEC(2,NN)=VECDP(2,NN)
        VEC(3,NN)=VECDP(3,NN)
        VEC(4,NN)=VECDP(4,NN)
        VEC(5,NN)=VECDP(5,NN)
        VEC(6,NN)=VECDP(6,NN)
        VEC(7,NN)=VECDP(7,NN)
        VEC(8,NN)=VECDP(8,NN)
        VEC(9,NN)=VECDP(9,NN)
      ENDDO
C reecriture pour contourner probleme sur itanium2 comp 9. + latency=16
      DO N = 1, NINDEX3
        NN = INDEX3(N)
C str utilise com tableau temporaire au lieu de scalaires temporaires
        STR(1,NN)=VEC(7,NN)
        STR(2,NN)=VEC(8,NN)
        STR(3,NN)=VEC(9,NN)
      ENDDO
      DO N = 1, NINDEX3
        NN = INDEX3(N)
        VEC(7,NN)=VEC(1,NN)
        VEC(8,NN)=VEC(2,NN)
        VEC(9,NN)=VEC(3,NN)
        VEC(1,NN)=-STR(1,NN)
        VEC(2,NN)=-STR(2,NN)
        VEC(3,NN)=-STR(3,NN)
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  EPSF2U                        source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        VALPVEC                       source/materials/mat/mat033/sigeps33.F
Chd|        VALPVECDP                     source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
              SUBROUTINE EPSF2U(
     1      NEL    ,FXX    , FXY  , FXZ  , FYX  ,    
     2      FYY    ,FYZ    , FZX  , FZY  , FZZ  , 
     3      UXX    ,UYY    , UZZ  , UXY  , UYZ  ,
     4      UXZ    )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL
      my_real
     .      FXX(NEL)  ,   FXY(NEL),   FXZ(NEL),
     .      FYX(NEL)  ,   FYY(NEL),   FYZ(NEL),
     .      FZX(NEL)  ,   FZY(NEL),   FZZ(NEL)    
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UXX(NEL)  ,   UXY(NEL),   UXZ(NEL),
     .      UYY(NEL)  ,   UYZ(NEL),   UZZ(NEL)    
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K
      my_real
     .        EV(3,MVSIZ),AV(6,MVSIZ),EVV(3,MVSIZ),DIRPRV(3,3,MVSIZ)
C----------------------------------------------------------------
C--------- [C]=[F]^t[F] strain-----
      DO I=1,NEL
       AV(1,I)=FXX(I)*FXX(I)+FYX(I)*FYX(I)+FZX(I)*FZX(I)
       AV(2,I)=FYY(I)*FYY(I)+FXY(I)*FXY(I)+FZY(I)*FZY(I)
       AV(3,I)=FZZ(I)*FZZ(I)+FZX(I)*FZX(I)+FZY(I)*FZY(I)
       AV(4,I)=FXX(I)*FXY(I)+FYX(I)*FYY(I)+FZX(I)*FZY(I)
       AV(6,I)=FXX(I)*FXZ(I)+FYX(I)*FYZ(I)+FZX(I)*FZZ(I)
       AV(5,I)=FXZ(I)*FXY(I)+FYZ(I)*FYY(I)+FZZ(I)*FZY(I)
      ENDDO
C         for a simple precision executing
      IF (IRESP.EQ.1) THEN
        CALL VALPVECDP(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC(AV,EVV,DIRPRV,NEL)
      ENDIF
      DO I=1,NEL
          EV(1,I)=SQRT(EVV(1,I))
          EV(2,I)=SQRT(EVV(2,I))
          EV(3,I)=SQRT(EVV(3,I))
      ENDDO
* TRANSFORM PRINCIPAL U TO GLOBAL DIRECTIONS
      DO I=1,NEL
C        
        UXX(I) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*EV(1,I)
     .         + DIRPRV(1,2,I)*DIRPRV(1,2,I)*EV(2,I)
     .         + DIRPRV(1,3,I)*DIRPRV(1,3,I)*EV(3,I)
c     
        UYY(I) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*EV(2,I)
     .         + DIRPRV(2,3,I)*DIRPRV(2,3,I)*EV(3,I)
     .         + DIRPRV(2,1,I)*DIRPRV(2,1,I)*EV(1,I)
c     
        UZZ(I) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*EV(3,I)
     .         + DIRPRV(3,1,I)*DIRPRV(3,1,I)*EV(1,I)
     .         + DIRPRV(3,2,I)*DIRPRV(3,2,I)*EV(2,I)
c     
        UXY(I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*EV(1,I)
     .         + DIRPRV(1,2,I)*DIRPRV(2,2,I)*EV(2,I)
     .         + DIRPRV(1,3,I)*DIRPRV(2,3,I)*EV(3,I)
c     
        UYZ(I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*EV(2,I)
     .         + DIRPRV(2,3,I)*DIRPRV(3,3,I)*EV(3,I)
     .         + DIRPRV(2,1,I)*DIRPRV(3,1,I)*EV(1,I)
c     
        UXZ(I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*EV(3,I)
     .         + DIRPRV(3,1,I)*DIRPRV(1,1,I)*EV(1,I)
     .         + DIRPRV(3,2,I)*DIRPRV(1,2,I)*EV(2,I)
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  VALPVECOP                     source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|====================================================================
      SUBROUTINE VALPVECOP(SIG,VAL,VEC,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(6,*), VAL(3,*), VEC(9,*)
      INTEGER NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, N, NN, LMAX, LMAXV(MVSIZ),NINDEX3, INDEX3(MVSIZ),
     .        NINDEX1, NINDEX2, INDEX1(MVSIZ), INDEX2(MVSIZ)
      DOUBLE PRECISION
     .   CS(6), STR(3,MVSIZ), A(3,3,MVSIZ), V(3,3,MVSIZ), B(3,3,MVSIZ),
     .   XMAG(3,MVSIZ), PR, AA, BB, AAA(MVSIZ),
     .   CC, ANGP, DD, FTPI, TTPI, STRMAX,
     .   TOL1(MVSIZ), TOL2(MVSIZ), XMAXV(MVSIZ), NORMINF(MVSIZ), 
     .   VMAG, S11,
     .   S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .   A22, A23, A31, A32, A33,
     .   MDEMI, XMAXINV, FLM,
     .   VALDP(3,MVSIZ),VECDP(9,MVSIZ),S4_2,S5_2,S6_2,C1,C2,SQ_AA
      REAL FLMIN
C-----------------------------------------------
C     DATA FTPI,TTPI / 4.188790205, 2.094395102 /
C     FTPI=(4/3)*PI, TTPI=(2/3)*PI
C
C     DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
      MDEMI = -UNDEMI
      TTPI = ACOS(MDEMI)
      FTPI = DEUX*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = DEUX*SQRT(FLMIN)
      NINDEX3=0 
      C1 = UNDEMI*SQRT(VINGT7)      
      C2 = DEUX*SQRT(THIRD)      
      DO NN = 1, NEL
        CS(1) = SIG(1,NN)
        CS(2) = SIG(2,NN)
        CS(3) = SIG(3,NN)
        CS(4) = SIG(4,NN)
        CS(5) = SIG(5,NN)
        CS(6) = SIG(6,NN)
        PR = -(CS(1)+CS(2)+CS(3)) * THIRD
        CS(1) = CS(1) + PR
        CS(2) = CS(2) + PR
        CS(3) = CS(3) + PR
	S4_2 = CS(4)*CS(4)
	S5_2 = CS(5)*CS(5)
	S6_2 = CS(6)*CS(6)
        AAA(NN)=S4_2 + S5_2 + S6_2 - CS(1)*CS(2)
     &         - CS(2)*CS(3) - CS(1)*CS(3)
        NORMINF(NN) = MAX(ABS(CS(1)),ABS(CS(2)),ABS(CS(3)),
     &      ABS(CS(4)),ABS(CS(5)),ABS(CS(6)))
        NORMINF(NN) = EM10*NORMINF(NN)
C cas racine triple
        AA = MAX(AAA(NN),NORMINF(NN))
C
        BB=CS(1)*S5_2 + CS(2)*S6_2
     &     + CS(3)*S4_2 - CS(1)*CS(2)*CS(3)
     &     - DEUX*CS(4)*CS(5)*CS(6)
C
        SQ_AA = SQRT(AA)
        CC=-C1/MAX(EM10,SQ_AA)*BB/MAX(EM20,AA)
c        CC=-SQRT(VINGT7/MAX(EM20,AA))*BB*UNDEMI/MAX(EM20,AA)
        CC= MIN(CC,UN)
        CC= MAX(CC,-UN)
        ANGP=ACOS(CC) * THIRD
        DD=C2*SQ_AA
        STR(1,NN)=DD*COS(ANGP)
        STR(2,NN)=DD*COS(ANGP+FTPI)
        STR(3,NN)=DD*COS(ANGP+TTPI)
C
        VALDP(1,NN) = STR(1,NN)-PR
        VALDP(2,NN) = STR(2,NN)-PR
        VALDP(3,NN) = STR(3,NN)-PR
C renforcement de precision en compression simple----
        IF(ABS(STR(3,NN)).GT.ABS(STR(1,NN))
     &     .AND.AAA(NN).GT.NORMINF(NN)) THEN
         AA=STR(1,NN)
         STR(1,NN)=STR(3,NN)
         STR(3,NN)=AA
         NINDEX3 = NINDEX3+1
         INDEX3(NINDEX3) = NN
        ENDIF
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR(1,NN)),ABS(STR(3,NN)))
        TOL1(NN)= MAX(EM20,FLM*STRMAX*STRMAX)
        TOL2(NN)=FLM*STRMAX * THIRD
        A(1,1,NN)=CS(1)-STR(1,NN)
        A(2,2,NN)=CS(2)-STR(1,NN)
        A(3,3,NN)=CS(3)-STR(1,NN)
        A(1,2,NN)=CS(4)
        A(2,1,NN)=CS(4)
        A(2,3,NN)=CS(5)
        A(3,2,NN)=CS(5)
        A(1,3,NN)=CS(6)
        A(3,1,NN)=CS(6)
C
        B(1,1,NN)=A(2,1,NN)*A(3,2,NN)-A(3,1,NN)
     .           *A(2,2,NN)
        B(1,2,NN)=A(2,2,NN)*A(3,3,NN)-A(3,2,NN)
     .           *A(2,3,NN)
        B(1,3,NN)=A(2,3,NN)*A(3,1,NN)-A(3,3,NN)
     .           *A(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*A(1,2,NN)-A(1,1,NN)
     .           *A(3,2,NN)
        B(2,2,NN)=A(3,2,NN)*A(1,3,NN)-A(1,2,NN)
     .           *A(3,3,NN)
        B(2,3,NN)=A(3,3,NN)*A(1,1,NN)-A(1,3,NN)
     .           *A(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*A(2,2,NN)-A(2,1,NN)
     .           *A(1,2,NN)
        B(3,2,NN)=A(1,2,NN)*A(2,3,NN)-A(2,2,NN)
     .           *A(1,3,NN)
        B(3,3,NN)=A(1,3,NN)*A(2,1,NN)-A(2,3,NN)
     .           *A(1,1,NN)
        XMAG(1,NN)=B(1,1,NN)*B(1,1,NN)+B(2,1,NN)*B(2,1,NN)+
     .             B(3,1,NN)*B(3,1,NN)
        XMAG(2,NN)=B(1,2,NN)*B(1,2,NN)+B(2,2,NN)*B(2,2,NN)+
     .             	B(3,2,NN)*B(3,2,NN)
        XMAG(3,NN)=B(1,3,NN)*B(1,3,NN)+B(2,3,NN)*B(2,3,NN)+
     .             	B(3,3,NN)*B(3,3,NN)

      ENDDO
C 
      NINDEX1 = 0
      NINDEX2 = 0
      DO NN = 1, NEL
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN).LT.NORMINF(NN)) THEN
          VALDP(1,NN) = SIG(1,NN)
          VALDP(2,NN) = SIG(2,NN)
          VALDP(3,NN) = SIG(3,NN)
          V(1,1,NN) = UN
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = UN
          V(3,2,NN) = ZERO	  
        ELSEIF(XMAXV(NN).GT.TOL1(NN)*TOL1(NN)) THEN
          NINDEX1 = NINDEX1 + 1
          INDEX1(NINDEX1) = NN
        ELSE
          NINDEX2 = NINDEX2 + 1
          INDEX2(NINDEX2) = NN
        ENDIF
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        LMAX = LMAXV(NN)
        XMAXINV = UN/SQRT(XMAXV(NN))
        V(1,1,NN)=B(1,LMAX,NN)*XMAXINV
        V(2,1,NN)=B(2,LMAX,NN)*XMAXINV
        V(3,1,NN)=B(3,LMAX,NN)*XMAXINV
        A(1,1,NN)=A(1,1,NN)+STR(1,NN)-STR(3,NN)
        A(2,2,NN)=A(2,2,NN)+STR(1,NN)-STR(3,NN)
        A(3,3,NN)=A(3,3,NN)+STR(1,NN)-STR(3,NN)
C
        B(1,1,NN)=A(2,1,NN)*V(3,1,NN)-A(3,1,NN)*V(2,1,NN)
        B(1,2,NN)=A(2,2,NN)*V(3,1,NN)-A(3,2,NN)*V(2,1,NN)
        B(1,3,NN)=A(2,3,NN)*V(3,1,NN)-A(3,3,NN)*V(2,1,NN)
        B(2,1,NN)=A(3,1,NN)*V(1,1,NN)-A(1,1,NN)*V(3,1,NN)
        B(2,2,NN)=A(3,2,NN)*V(1,1,NN)-A(1,2,NN)*V(3,1,NN)
        B(2,3,NN)=A(3,3,NN)*V(1,1,NN)-A(1,3,NN)*V(3,1,NN)
        B(3,1,NN)=A(1,1,NN)*V(2,1,NN)-A(2,1,NN)*V(1,1,NN)
        B(3,2,NN)=A(1,2,NN)*V(2,1,NN)-A(2,2,NN)*V(1,1,NN)
        B(3,3,NN)=A(1,3,NN)*V(2,1,NN)-A(2,3,NN)*V(1,1,NN)
        XMAG(1,NN)=B(1,1,NN)*B(1,1,NN)+B(2,1,NN)*B(2,1,NN)+
     .             B(3,1,NN)*B(3,1,NN)
        XMAG(2,NN)=B(1,2,NN)*B(1,2,NN)+B(2,2,NN)*B(2,2,NN)+
     .             	B(3,2,NN)*B(3,2,NN)
        XMAG(3,NN)=B(1,3,NN)*B(1,3,NN)+B(2,3,NN)*B(2,3,NN)+
     .             	B(3,3,NN)*B(3,3,NN)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(1,1,NN)**2+V(2,1,NN)**2)
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,3,NN)=B(1,LMAX,NN)*XMAXINV
          V(2,3,NN)=B(2,LMAX,NN)*XMAXINV
          V(3,3,NN)=B(3,LMAX,NN)*XMAXINV
          V(1,2,NN)=V(2,3,NN)*V(3,1,NN)-V(2,1,NN)*V(3,3,NN)
          V(2,2,NN)=V(3,3,NN)*V(1,1,NN)-V(3,1,NN)*V(1,3,NN)
          V(3,2,NN)=V(1,3,NN)*V(2,1,NN)-V(1,1,NN)*V(2,3,NN)
	  C1 = V(1,2,NN)*V(1,2,NN)+V(2,2,NN)*V(2,2,NN)+
     &         V(3,2,NN)*V(3,2,NN)
          VMAG=UN/SQRT(C1)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSEIF(VMAG.GT.TOL2(NN))THEN
          V(1,2,NN)=-V(2,1,NN)/VMAG
          V(2,2,NN)=V(1,1,NN)/VMAG
          V(3,2,NN)=ZERO
        ELSE
          V(1,2,NN)=UN
          V(2,2,NN)=ZERO
          V(3,2,NN)=ZERO  
        ENDIF
      ENDDO
C . . . . . . . . . . . . .
C    SOLUTION DOUBLE
C . . . . . . . . . . . . .
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        XMAG(1,NN)=A(1,1,NN)*A(1,1,NN)+A(2,1,NN)*A(2,1,NN)
        XMAG(2,NN)=A(1,2,NN)*A(1,2,NN)+A(2,2,NN)*A(2,2,NN)
        XMAG(3,NN)=A(1,3,NN)*A(1,3,NN)+A(2,3,NN)*A(2,3,NN)
C
        XMAXV(NN)=MAX(XMAG(1,NN),XMAG(2,NN),XMAG(3,NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        IF(XMAG(1,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(MAX(ABS(A(3,1,NN)),ABS(A(3,2,NN)),ABS(A(3,3,NN)))
     &       .LT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,1,NN)= ZERO
          V(2,1,NN)= ZERO
          V(3,1,NN)= UN  
          V(1,2,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,2,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,2,NN)= ZERO
C
        ELSEIF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(1,1,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,1,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,1,NN)= ZERO
          V(1,2,NN)=-A(3,LMAX,NN)*V(2,1,NN)
          V(2,2,NN)= A(3,LMAX,NN)*V(1,1,NN)
          V(3,2,NN)= A(1,LMAX,NN)*V(2,1,NN)-A(2,LMAX,NN)*V(1,1,NN)
	  C1 = V(1,2,NN)*V(1,2,NN)+V(2,2,NN)*V(2,2,NN)+
     &         V(3,2,NN)*V(3,2,NN)
          VMAG=UN/SQRT(C1)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSE
          V(1,1,NN) = UN
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = UN
          V(3,2,NN) = ZERO	  
        ENDIF
      ENDDO
C
      DO NN = 1, NEL
        VECDP(1,NN)=V(1,1,NN)
        VECDP(2,NN)=V(2,1,NN)
        VECDP(3,NN)=V(3,1,NN)
        VECDP(4,NN)=V(1,2,NN)
        VECDP(5,NN)=V(2,2,NN)
        VECDP(6,NN)=V(3,2,NN)
        VECDP(7,NN)=VECDP(2,NN)*VECDP(6,NN)-VECDP(3,NN)*VECDP(5,NN)
        VECDP(8,NN)=VECDP(3,NN)*VECDP(4,NN)-VECDP(1,NN)*VECDP(6,NN)
        VECDP(9,NN)=VECDP(1,NN)*VECDP(5,NN)-VECDP(2,NN)*VECDP(4,NN)
      ENDDO
C
      DO NN = 1, NEL
        VAL(1,NN)=VALDP(1,NN)
        VAL(2,NN)=VALDP(2,NN)
        VAL(3,NN)=VALDP(3,NN)
        VEC(1,NN)=VECDP(1,NN)
        VEC(2,NN)=VECDP(2,NN)
        VEC(3,NN)=VECDP(3,NN)
        VEC(4,NN)=VECDP(4,NN)
        VEC(5,NN)=VECDP(5,NN)
        VEC(6,NN)=VECDP(6,NN)
        VEC(7,NN)=VECDP(7,NN)
        VEC(8,NN)=VECDP(8,NN)
        VEC(9,NN)=VECDP(9,NN)
      ENDDO
C reecriture pour contourner probleme sur itanium2 comp 9. + latency=16
      DO N = 1, NINDEX3
        NN = INDEX3(N)
C str utilise com tableau temporaire au lieu de scalaires temporaires
        STR(1,NN)=VEC(7,NN)
        STR(2,NN)=VEC(8,NN)
        STR(3,NN)=VEC(9,NN)
      ENDDO
      DO N = 1, NINDEX3
        NN = INDEX3(N)
        VEC(7,NN)=VEC(1,NN)
        VEC(8,NN)=VEC(2,NN)
        VEC(9,NN)=VEC(3,NN)
        VEC(1,NN)=-STR(1,NN)
        VEC(2,NN)=-STR(2,NN)
        VEC(3,NN)=-STR(3,NN)
      ENDDO
      RETURN
      END


Chd|====================================================================
Chd|  VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|        SIGEPS62                      source/materials/mat/mat062/sigeps62.F
Chd|        SIGEPS69                      source/materials/mat/mat069/sigeps69.F
Chd|        SIGEPS71                      source/materials/mat/mat071/sigeps71.F
Chd|        SIGEPS82                      source/materials/mat/mat082/sigeps82.F
Chd|        SIGEPS92                      source/materials/mat/mat092/sigeps92.F
Chd|        SIGEPS94                      source/materials/mat/mat094/sigeps94.F
Chd|-- calls ---------------
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|====================================================================
      SUBROUTINE VALPVEC_V(SIG,VAL,VEC,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(MVSIZ,6), VAL(MVSIZ,3), VEC(MVSIZ,9)
      INTEGER NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, N, NN, LMAX, LMAXV(MVSIZ),NINDEX3, INDEX3(MVSIZ),
     .        NINDEX1, NINDEX2, INDEX1(MVSIZ), INDEX2(MVSIZ)
      my_real
     .   CS(6), STR(MVSIZ,3), A(MVSIZ,3,3), V(MVSIZ,3,3), B(MVSIZ,3,3),
     .   XMAG(MVSIZ,3), PR, AA, BB, AAA(MVSIZ),
     .   CC, ANGP, DD, FTPI, TTPI, STRMAX,
     .   TOL1(MVSIZ), TOL2(MVSIZ), XMAXV(MVSIZ), NORMINF(MVSIZ), 
     .   VMAG, S11,
     .   S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .   A22, A23, A31, A32, A33,
     .   MDEMI, XMAXINV, FLM
      REAL FLMIN
C-----------------------------------------------
C     DATA FTPI,TTPI / 4.188790205, 2.094395102 /
C     FTPI=(4/3)*PI, TTPI=(2/3)*PI
C
C     DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
      MDEMI = -UNDEMI
      TTPI = ACOS(MDEMI)
      FTPI = DEUX*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = DEUX*SQRT(FLMIN)
      NINDEX3=0  
      DO NN = 1, NEL
        CS(1) = SIG(NN,1)
        CS(2) = SIG(NN,2)
        CS(3) = SIG(NN,3)
        CS(4) = SIG(NN,4)
        CS(5) = SIG(NN,5)
        CS(6) = SIG(NN,6)
        PR = -(CS(1)+CS(2)+CS(3))* THIRD
        CS(1) = CS(1) + PR
        CS(2) = CS(2) + PR
        CS(3) = CS(3) + PR
        AAA(NN)=CS(4)**2 + CS(5)**2 + CS(6)**2 - CS(1)*CS(2)
     &      - CS(2)*CS(3) - CS(1)*CS(3)
        NORMINF(NN) = MAX(ABS(CS(1)),ABS(CS(2)),ABS(CS(3)),
     &      ABS(CS(4)),ABS(CS(5)),ABS(CS(6)))
        NORMINF(NN) = EM10*NORMINF(NN)
C cas racine triple
c        AA = MAX(AAA(NN),NORMINF(NN),EM20)
        AA = MAX(AAA(NN),NORMINF(NN))
C
        BB=CS(1)*CS(5)**2 + CS(2)*CS(6)**2
     &     + CS(3)*CS(4)**2 - CS(1)*CS(2)*CS(3)
     &     - DEUX*CS(4)*CS(5)*CS(6)     
C
        CC=-SQRT(VINGT7/MAX(EM20,AA))*BB*UNDEMI/MAX(EM20,AA)
        CC= MIN(CC,UN)
        CC= MAX(CC,-UN)
        ANGP=ACOS(CC) * THIRD
        DD=DEUX*SQRT(AA * THIRD)
        STR(NN,1)=DD*COS(ANGP)
        STR(NN,2)=DD*COS(ANGP+FTPI)
        STR(NN,3)=DD*COS(ANGP+TTPI)
C
        VAL(NN,1) = STR(NN,1)-PR
        VAL(NN,2) = STR(NN,2)-PR
        VAL(NN,3) = STR(NN,3)-PR
C renforcement de precision en compression----
        IF(ABS(STR(NN,3)).GT.ABS(STR(NN,1))
     &     .AND.AAA(NN).GT.NORMINF(NN)) THEN
         AA=STR(NN,1)
         STR(NN,1)=STR(NN,3)
         STR(NN,3)=AA
         NINDEX3 = NINDEX3+1
         INDEX3(NINDEX3) = NN
        ENDIF
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR(NN,1)),ABS(STR(NN,3)))
        TOL1(NN)= MAX(EM20,FLM*STRMAX**2)
        TOL2(NN)=FLM*STRMAX/3
        A(NN,1,1)=CS(1)-STR(NN,1)
        A(NN,2,2)=CS(2)-STR(NN,1)
        A(NN,3,3)=CS(3)-STR(NN,1)
        A(NN,1,2)=CS(4)
        A(NN,2,1)=CS(4)
        A(NN,2,3)=CS(5)
        A(NN,3,2)=CS(5)
        A(NN,1,3)=CS(6)
        A(NN,3,1)=CS(6)
C
        B(NN,1,1)=A(NN,2,1)*A(NN,3,2)-A(NN,3,1)
     .           *A(NN,2,2)
        B(NN,1,2)=A(NN,2,2)*A(NN,3,3)-A(NN,3,2)
     .           *A(NN,2,3)
        B(NN,1,3)=A(NN,2,3)*A(NN,3,1)-A(NN,3,3)
     .           *A(NN,2,1)
        B(NN,2,1)=A(NN,3,1)*A(NN,1,2)-A(NN,1,1)
     .           *A(NN,3,2)
        B(NN,2,2)=A(NN,3,2)*A(NN,1,3)-A(NN,1,2)
     .           *A(NN,3,3)
        B(NN,2,3)=A(NN,3,3)*A(NN,1,1)-A(NN,1,3)
     .           *A(NN,3,1)
        B(NN,3,1)=A(NN,1,1)*A(NN,2,2)-A(NN,2,1)
     .           *A(NN,1,2)
        B(NN,3,2)=A(NN,1,2)*A(NN,2,3)-A(NN,2,2)
     .           *A(NN,1,3)
        B(NN,3,3)=A(NN,1,3)*A(NN,2,1)-A(NN,2,3)
     .           *A(NN,1,1)
        XMAG(NN,1)=SQRT(B(NN,1,1)**2+B(NN,2,1)**2+B(NN,3,1)**2)
        XMAG(NN,2)=SQRT(B(NN,1,2)**2+B(NN,2,2)**2+B(NN,3,2)**2)
        XMAG(NN,3)=SQRT(B(NN,1,3)**2+B(NN,2,3)**2+B(NN,3,3)**2)

      ENDDO
C 
      NINDEX1 = 0
      NINDEX2 = 0
      DO NN = 1, NEL
        XMAXV(NN)=MAX(XMAG(NN,1),XMAG(NN,2),XMAG(NN,3))
        IF(XMAG(NN,1).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN).LT.NORMINF(NN)) THEN
          VAL(NN,1) = SIG(NN,1)
          VAL(NN,2) = SIG(NN,2)
          VAL(NN,3) = SIG(NN,3)
          V(NN,1,1) = UN
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = UN
          V(NN,3,2) = ZERO

        ELSEIF(XMAXV(NN).GT.TOL1(NN)) THEN
          NINDEX1 = NINDEX1 + 1
          INDEX1(NINDEX1) = NN
        ELSE
          NINDEX2 = NINDEX2 + 1
          INDEX2(NINDEX2) = NN
        ENDIF
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        LMAX = LMAXV(NN)
        XMAXINV = UN/XMAXV(NN)
        V(NN,1,1)=B(NN,1,LMAX)*XMAXINV
        V(NN,2,1)=B(NN,2,LMAX)*XMAXINV
        V(NN,3,1)=B(NN,3,LMAX)*XMAXINV
        A(NN,1,1)=A(NN,1,1)+STR(NN,1)-STR(NN,3)
        A(NN,2,2)=A(NN,2,2)+STR(NN,1)-STR(NN,3)
        A(NN,3,3)=A(NN,3,3)+STR(NN,1)-STR(NN,3)
C
        B(NN,1,1)=A(NN,2,1)*V(NN,3,1)-A(NN,3,1)*V(NN,2,1)
        B(NN,1,2)=A(NN,2,2)*V(NN,3,1)-A(NN,3,2)*V(NN,2,1)
        B(NN,1,3)=A(NN,2,3)*V(NN,3,1)-A(NN,3,3)*V(NN,2,1)
        B(NN,2,1)=A(NN,3,1)*V(NN,1,1)-A(NN,1,1)*V(NN,3,1)
        B(NN,2,2)=A(NN,3,2)*V(NN,1,1)-A(NN,1,2)*V(NN,3,1)
        B(NN,2,3)=A(NN,3,3)*V(NN,1,1)-A(NN,1,3)*V(NN,3,1)
        B(NN,3,1)=A(NN,1,1)*V(NN,2,1)-A(NN,2,1)*V(NN,1,1)
        B(NN,3,2)=A(NN,1,2)*V(NN,2,1)-A(NN,2,2)*V(NN,1,1)
        B(NN,3,3)=A(NN,1,3)*V(NN,2,1)-A(NN,2,3)*V(NN,1,1)
        XMAG(NN,1)=SQRT(B(NN,1,1)**2+B(NN,2,1)**2+B(NN,3,1)**2)
        XMAG(NN,2)=SQRT(B(NN,1,2)**2+B(NN,2,2)**2+B(NN,3,2)**2)
        XMAG(NN,3)=SQRT(B(NN,1,3)**2+B(NN,2,3)**2+B(NN,3,3)**2)
C
        XMAXV(NN)=MAX(XMAG(NN,1),XMAG(NN,2),XMAG(NN,3))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        IF(XMAG(NN,1).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(NN,1,1)**2+V(NN,2,1)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,3)=B(NN,1,LMAX)*XMAXINV
          V(NN,2,3)=B(NN,2,LMAX)*XMAXINV
          V(NN,3,3)=B(NN,3,LMAX)*XMAXINV
          V(NN,1,2)=V(NN,2,3)*V(NN,3,1)-V(NN,2,1)*V(NN,3,3)
          V(NN,2,2)=V(NN,3,3)*V(NN,1,1)-V(NN,3,1)*V(NN,1,3)
          V(NN,3,2)=V(NN,1,3)*V(NN,2,1)-V(NN,1,1)*V(NN,2,3)
          VMAG=UN/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSEIF(VMAG.GT.TOL2(NN))THEN
          V(NN,1,2)=-V(NN,2,1)/VMAG
          V(NN,2,2)=V(NN,1,1)/VMAG
          V(NN,3,2)=ZERO
        ELSE
          V(NN,1,2)=UN
          V(NN,2,2)=ZERO
          V(NN,3,2)=ZERO  
        ENDIF
      ENDDO
C . . . . . . . . . . . . .
C    SOLUTION DOUBLE
C . . . . . . . . . . . . .
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        XMAG(NN,1)=SQRT(A(NN,1,1)**2+A(NN,2,1)**2)
        XMAG(NN,2)=SQRT(A(NN,1,2)**2+A(NN,2,2)**2)
        XMAG(NN,3)=SQRT(A(NN,1,3)**2+A(NN,2,3)**2)
C
        XMAXV(NN)=MAX(XMAG(NN,1),XMAG(NN,2),XMAG(NN,3))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        IF(XMAG(NN,1).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(NN,3,1)),ABS(A(NN,3,2)),ABS(A(NN,3,3)))
     &       .LT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,1)= ZERO
          V(NN,2,1)= ZERO
          V(NN,3,1)= UN
          V(NN,1,2)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,2)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,2)= ZERO
C
        ELSEIF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,1)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,1)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,1)= ZERO
          V(NN,1,2)=-A(NN,3,LMAX)*V(NN,2,1)
          V(NN,2,2)= A(NN,3,LMAX)*V(NN,1,1)
          V(NN,3,2)= A(NN,1,LMAX)*V(NN,2,1)-A(NN,2,LMAX)*V(NN,1,1)
          VMAG=UN/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSE
          V(NN,1,1) = UN
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = UN
          V(NN,3,2) = ZERO	  
        ENDIF
      ENDDO
C
      DO NN = 1,NEL
        VEC(NN,1)=V(NN,1,1)
        VEC(NN,2)=V(NN,2,1)
        VEC(NN,3)=V(NN,3,1)
        VEC(NN,4)=V(NN,1,2)
        VEC(NN,5)=V(NN,2,2)
        VEC(NN,6)=V(NN,3,2)
        VEC(NN,7)=VEC(NN,2)*VEC(NN,6)-VEC(NN,3)*VEC(NN,5)
        VEC(NN,8)=VEC(NN,3)*VEC(NN,4)-VEC(NN,1)*VEC(NN,6)
        VEC(NN,9)=VEC(NN,1)*VEC(NN,5)-VEC(NN,2)*VEC(NN,4)
      ENDDO
C reecriture pour contourner probleme sur itanium2 comp 9. + latency=16
      DO N = 1, NINDEX3
        NN = INDEX3(N)
C str utilise com tableau temporaire au lieu de scalaires temporaires
        STR(NN,1)=VEC(NN,7)
        STR(NN,2)=VEC(NN,8)
        STR(NN,3)=VEC(NN,9)
      ENDDO
      DO N = 1, NINDEX3
        NN = INDEX3(N)
        VEC(NN,7)=VEC(NN,1)
        VEC(NN,8)=VEC(NN,2)
        VEC(NN,9)=VEC(NN,3)
        VEC(NN,1)=-STR(NN,1)
        VEC(NN,2)=-STR(NN,2)
        VEC(NN,3)=-STR(NN,3)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|        SIGEPS62                      source/materials/mat/mat062/sigeps62.F
Chd|        SIGEPS69                      source/materials/mat/mat069/sigeps69.F
Chd|        SIGEPS71                      source/materials/mat/mat071/sigeps71.F
Chd|        SIGEPS82                      source/materials/mat/mat082/sigeps82.F
Chd|        SIGEPS92                      source/materials/mat/mat092/sigeps92.F
Chd|        SIGEPS94                      source/materials/mat/mat094/sigeps94.F
Chd|-- calls ---------------
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|====================================================================
      SUBROUTINE VALPVECDP_V(SIG,VAL,VEC,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(MVSIZ,6), VAL(MVSIZ,3), VEC(MVSIZ,9)
      INTEGER NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, N, NN, LMAX, LMAXV(MVSIZ),NINDEX3, INDEX3(MVSIZ),
     .        NINDEX1, NINDEX2, INDEX1(MVSIZ), INDEX2(MVSIZ)
      DOUBLE PRECISION
     .   CS(6), STR(MVSIZ,3), A(MVSIZ,3,3), V(MVSIZ,3,3), B(MVSIZ,3,3),
     .   XMAG(MVSIZ,3), PR, AA, BB, AAA(MVSIZ),
     .   CC, ANGP, DD, FTPI, TTPI, STRMAX,
     .   TOL1(MVSIZ), TOL2(MVSIZ), XMAXV(MVSIZ), NORMINF(MVSIZ), 
     .   VMAG, S11,
     .   S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .   A22, A23, A31, A32, A33,
     .   MDEMI, XMAXINV, FLM,
     .   VALDP(MVSIZ,3),VECDP(MVSIZ,9)
      REAL FLMIN
C-----------------------------------------------
C     DATA FTPI,TTPI / 4.188790205, 2.094395102 /
C     FTPI=(4/3)*PI, TTPI=(2/3)*PI
C
C     DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
      MDEMI = -UNDEMI
      TTPI = ACOS(MDEMI)
      FTPI = DEUX*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = DEUX*SQRT(FLMIN)
      NINDEX3=0  
      DO NN = 1, NEL
        CS(1) = SIG(NN,1)
        CS(2) = SIG(NN,2)
        CS(3) = SIG(NN,3)
        CS(4) = SIG(NN,4)
        CS(5) = SIG(NN,5)
        CS(6) = SIG(NN,6)
        PR = -(CS(1)+CS(2)+CS(3)) * THIRD
        CS(1) = CS(1) + PR
        CS(2) = CS(2) + PR
        CS(3) = CS(3) + PR
        AAA(NN)=CS(4)**2 + CS(5)**2 + CS(6)**2 - CS(1)*CS(2)
     &      - CS(2)*CS(3) - CS(1)*CS(3)
        NORMINF(NN) = MAX(ABS(CS(1)),ABS(CS(2)),ABS(CS(3)),
     &      ABS(CS(4)),ABS(CS(5)),ABS(CS(6)))
        NORMINF(NN) = EM10*NORMINF(NN)
C cas racine triple
c        AA = MAX(AAA(NN),NORMINF(NN),EM20)
        AA = MAX(AAA(NN),NORMINF(NN))
C
        BB=CS(1)*CS(5)**2 + CS(2)*CS(6)**2
     &     + CS(3)*CS(4)**2 - CS(1)*CS(2)*CS(3)
     &     - DEUX*CS(4)*CS(5)*CS(6)
C
        CC=-SQRT(VINGT7/MAX(EM20,AA))*BB*UNDEMI/MAX(EM20,AA)
        CC= MIN(CC,UN)
        CC= MAX(CC,-UN)
        ANGP=ACOS(CC) * THIRD
        DD=DEUX*SQRT(AA * THIRD)
        STR(NN,1)=DD*COS(ANGP)
        STR(NN,2)=DD*COS(ANGP+FTPI)
        STR(NN,3)=DD*COS(ANGP+TTPI)
C
        VALDP(NN,1) = STR(NN,1)-PR
        VALDP(NN,2) = STR(NN,2)-PR
        VALDP(NN,3) = STR(NN,3)-PR
C renforcement de precision en compression simple----
        IF(ABS(STR(NN,3)).GT.ABS(STR(NN,1))
     &     .AND.AAA(NN).GT.NORMINF(NN)) THEN
         AA=STR(NN,1)
         STR(NN,1)=STR(NN,3)
         STR(NN,3)=AA
         NINDEX3 = NINDEX3+1
         INDEX3(NINDEX3) = NN
        ENDIF
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR(NN,1)),ABS(STR(NN,3)))
        TOL1(NN)= MAX(EM20,FLM*STRMAX**2)
        TOL2(NN)=FLM*STRMAX * THIRD
        A(NN,1,1)=CS(1)-STR(NN,1)
        A(NN,2,2)=CS(2)-STR(NN,1)
        A(NN,3,3)=CS(3)-STR(NN,1)
        A(NN,1,2)=CS(4)
        A(NN,2,1)=CS(4)
        A(NN,2,3)=CS(5)
        A(NN,3,2)=CS(5)
        A(NN,1,3)=CS(6)
        A(NN,3,1)=CS(6)
C
        B(NN,1,1)=A(NN,2,1)*A(NN,3,2)-A(NN,3,1)
     .           *A(NN,2,2)
        B(NN,1,2)=A(NN,2,2)*A(NN,3,3)-A(NN,3,2)
     .           *A(NN,2,3)
        B(NN,1,3)=A(NN,2,3)*A(NN,3,1)-A(NN,3,3)
     .           *A(NN,2,1)
        B(NN,2,1)=A(NN,3,1)*A(NN,1,2)-A(NN,1,1)
     .           *A(NN,3,2)
        B(NN,2,2)=A(NN,3,2)*A(NN,1,3)-A(NN,1,2)
     .           *A(NN,3,3)
        B(NN,2,3)=A(NN,3,3)*A(NN,1,1)-A(NN,1,3)
     .           *A(NN,3,1)
        B(NN,3,1)=A(NN,1,1)*A(NN,2,2)-A(NN,2,1)
     .           *A(NN,1,2)
        B(NN,3,2)=A(NN,1,2)*A(NN,2,3)-A(NN,2,2)
     .           *A(NN,1,3)
        B(NN,3,3)=A(NN,1,3)*A(NN,2,1)-A(NN,2,3)
     .           *A(NN,1,1)
        XMAG(NN,1)=SQRT(B(NN,1,1)**2+B(NN,2,1)**2+B(NN,3,1)**2)
        XMAG(NN,2)=SQRT(B(NN,1,2)**2+B(NN,2,2)**2+B(NN,3,2)**2)
        XMAG(NN,3)=SQRT(B(NN,1,3)**2+B(NN,2,3)**2+B(NN,3,3)**2)

      ENDDO
C 
      NINDEX1 = 0
      NINDEX2 = 0
      DO NN = 1, NEL
        XMAXV(NN)=MAX(XMAG(NN,1),XMAG(NN,2),XMAG(NN,3))
        IF(XMAG(NN,1).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN).LT.NORMINF(NN)) THEN
          VALDP(NN,1) = SIG(NN,1)
          VALDP(NN,2) = SIG(NN,2)
          VALDP(NN,3) = SIG(NN,3)
          V(NN,1,1) = UN
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = UN
          V(NN,3,2) = ZERO	  
        ELSEIF(XMAXV(NN).GT.TOL1(NN)) THEN
          NINDEX1 = NINDEX1 + 1
          INDEX1(NINDEX1) = NN
        ELSE
          NINDEX2 = NINDEX2 + 1
          INDEX2(NINDEX2) = NN
        ENDIF
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        LMAX = LMAXV(NN)
        XMAXINV = UN/XMAXV(NN)
        V(NN,1,1)=B(NN,1,LMAX)*XMAXINV
        V(NN,2,1)=B(NN,2,LMAX)*XMAXINV
        V(NN,3,1)=B(NN,3,LMAX)*XMAXINV
        A(NN,1,1)=A(NN,1,1)+STR(NN,1)-STR(NN,3)
        A(NN,2,2)=A(NN,2,2)+STR(NN,1)-STR(NN,3)
        A(NN,3,3)=A(NN,3,3)+STR(NN,1)-STR(NN,3)
C
        B(NN,1,1)=A(NN,2,1)*V(NN,3,1)-A(NN,3,1)*V(NN,2,1)
        B(NN,1,2)=A(NN,2,2)*V(NN,3,1)-A(NN,3,2)*V(NN,2,1)
        B(NN,1,3)=A(NN,2,3)*V(NN,3,1)-A(NN,3,3)*V(NN,2,1)
        B(NN,2,1)=A(NN,3,1)*V(NN,1,1)-A(NN,1,1)*V(NN,3,1)
        B(NN,2,2)=A(NN,3,2)*V(NN,1,1)-A(NN,1,2)*V(NN,3,1)
        B(NN,2,3)=A(NN,3,3)*V(NN,1,1)-A(NN,1,3)*V(NN,3,1)
        B(NN,3,1)=A(NN,1,1)*V(NN,2,1)-A(NN,2,1)*V(NN,1,1)
        B(NN,3,2)=A(NN,1,2)*V(NN,2,1)-A(NN,2,2)*V(NN,1,1)
        B(NN,3,3)=A(NN,1,3)*V(NN,2,1)-A(NN,2,3)*V(NN,1,1)
        XMAG(NN,1)=SQRT(B(NN,1,1)**2+B(NN,2,1)**2+B(NN,3,1)**2)
        XMAG(NN,2)=SQRT(B(NN,1,2)**2+B(NN,2,2)**2+B(NN,3,2)**2)
        XMAG(NN,3)=SQRT(B(NN,1,3)**2+B(NN,2,3)**2+B(NN,3,3)**2)
C
        XMAXV(NN)=MAX(XMAG(NN,1),XMAG(NN,2),XMAG(NN,3))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        IF(XMAG(NN,1).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(NN,1,1)**2+V(NN,2,1)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,3)=B(NN,1,LMAX)*XMAXINV
          V(NN,2,3)=B(NN,2,LMAX)*XMAXINV
          V(NN,3,3)=B(NN,3,LMAX)*XMAXINV
          V(NN,1,2)=V(NN,2,3)*V(NN,3,1)-V(NN,2,1)*V(NN,3,3)
          V(NN,2,2)=V(NN,3,3)*V(NN,1,1)-V(NN,3,1)*V(NN,1,3)
          V(NN,3,2)=V(NN,1,3)*V(NN,2,1)-V(NN,1,1)*V(NN,2,3)
          VMAG=UN/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSEIF(VMAG.GT.TOL2(NN))THEN
          V(NN,1,2)=-V(NN,2,1)/VMAG
          V(NN,2,2)=V(NN,1,1)/VMAG
          V(NN,3,2)=ZERO
        ELSE
          V(NN,1,2)=UN
          V(NN,2,2)=ZERO
          V(NN,3,2)=ZERO  
        ENDIF
      ENDDO
C . . . . . . . . . . . . .
C    SOLUTION DOUBLE
C . . . . . . . . . . . . .
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        XMAG(NN,1)=SQRT(A(NN,1,1)**2+A(NN,2,1)**2)
        XMAG(NN,2)=SQRT(A(NN,1,2)**2+A(NN,2,2)**2)
        XMAG(NN,3)=SQRT(A(NN,1,3)**2+A(NN,2,3)**2)
C
        XMAXV(NN)=MAX(XMAG(NN,1),XMAG(NN,2),XMAG(NN,3))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        IF(XMAG(NN,1).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(NN,3,1)),ABS(A(NN,3,2)),ABS(A(NN,3,3)))
     &       .LT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,1)= ZERO
          V(NN,2,1)= ZERO
          V(NN,3,1)= UN  
          V(NN,1,2)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,2)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,2)= ZERO
C
        ELSEIF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,1)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,1)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,1)= ZERO
          V(NN,1,2)=-A(NN,3,LMAX)*V(NN,2,1)
          V(NN,2,2)= A(NN,3,LMAX)*V(NN,1,1)
          V(NN,3,2)= A(NN,1,LMAX)*V(NN,2,1)-A(NN,2,LMAX)*V(NN,1,1)
          VMAG=UN/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSE
          V(NN,1,1) = UN
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = UN
          V(NN,3,2) = ZERO	  
        ENDIF
      ENDDO
C
      DO NN = 1, NEL
        VECDP(NN,1)=V(NN,1,1)
        VECDP(NN,2)=V(NN,2,1)
        VECDP(NN,3)=V(NN,3,1)
        VECDP(NN,4)=V(NN,1,2)
        VECDP(NN,5)=V(NN,2,2)
        VECDP(NN,6)=V(NN,3,2)
        VECDP(NN,7)=VECDP(NN,2)*VECDP(NN,6)-VECDP(NN,3)*VECDP(NN,5)
        VECDP(NN,8)=VECDP(NN,3)*VECDP(NN,4)-VECDP(NN,1)*VECDP(NN,6)
        VECDP(NN,9)=VECDP(NN,1)*VECDP(NN,5)-VECDP(NN,2)*VECDP(NN,4)
      ENDDO
C
      DO NN = 1, NEL
        VAL(NN,1)=VALDP(NN,1)
        VAL(NN,2)=VALDP(NN,2)
        VAL(NN,3)=VALDP(NN,3)
        VEC(NN,1)=VECDP(NN,1)
        VEC(NN,2)=VECDP(NN,2)
        VEC(NN,3)=VECDP(NN,3)
        VEC(NN,4)=VECDP(NN,4)
        VEC(NN,5)=VECDP(NN,5)
        VEC(NN,6)=VECDP(NN,6)
        VEC(NN,7)=VECDP(NN,7)
        VEC(NN,8)=VECDP(NN,8)
        VEC(NN,9)=VECDP(NN,9)
      ENDDO
C reecriture pour contourner probleme sur itanium2 comp 9. + latency=16
      DO N = 1, NINDEX3
        NN = INDEX3(N)
C str utilise com tableau temporaire au lieu de scalaires temporaires
        STR(NN,1)=VEC(NN,7)
        STR(NN,2)=VEC(NN,8)
        STR(NN,3)=VEC(NN,9)
      ENDDO
      DO N = 1, NINDEX3
        NN = INDEX3(N)
        VEC(NN,7)=VEC(NN,1)
        VEC(NN,8)=VEC(NN,2)
        VEC(NN,9)=VEC(NN,3)
        VEC(NN,1)=-STR(NN,1)
        VEC(NN,2)=-STR(NN,2)
        VEC(NN,3)=-STR(NN,3)
      ENDDO
      RETURN
      END




Chd|====================================================================
Chd|  VALPVECOP_V                   source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        PRINC_U                       source/materials/mat/mat001/m1law.F
Chd|        PRINC_U1                      source/materials/mat/mat001/m1law.F
Chd|-- calls ---------------
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|====================================================================
      SUBROUTINE VALPVECOP_V(SIG,VAL,VEC,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(MVSIZ,6), VAL(MVSIZ,3), VEC(MVSIZ,9)
      INTEGER NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, N, NN, LMAX, LMAXV(MVSIZ),NINDEX3, INDEX3(MVSIZ),
     .        NINDEX1, NINDEX2, INDEX1(MVSIZ), INDEX2(MVSIZ)
      DOUBLE PRECISION
     .   CS1,CS2,CS3,CS4,CS5,CS6,
     .   A(MVSIZ,3,3), V(MVSIZ,3,3), B(MVSIZ,3,3),
     .   PR, AA, BB, AAA(MVSIZ),
     .   CC, ANGP, DD, FTPI, TTPI, STRMAX,
     .   TOL1(MVSIZ), TOL2(MVSIZ), XMAXV(MVSIZ), NORMINF(MVSIZ), 
     .   VMAG, S11,
     .   S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .   S1,S2,S3,
     .   A22, A23, A31, A32, A33,
     .   MDEMI, XMAXINV, FLM,
     .   S4_2,S5_2,S6_2,C1,C2,SQ_AA,
     .   VALDP1(MVSIZ),VALDP2(MVSIZ),VALDP3(MVSIZ),
     .   STR1(MVSIZ),STR2(MVSIZ),STR3(MVSIZ),
     .   XMAG1(MVSIZ),XMAG2(MVSIZ),XMAG3(MVSIZ),
     .   VECDP1(MVSIZ),VECDP2(MVSIZ),VECDP3(MVSIZ),
     .   VECDP4(MVSIZ),VECDP5(MVSIZ),VECDP6(MVSIZ),
     .   VECDP7(MVSIZ),VECDP8(MVSIZ),VECDP9(MVSIZ)

       DOUBLE PRECISION :: SQRT3DMI, SIN_ANGP, COS_ANGP
       PARAMETER (SQRT3DMI=SQRT(3.0D0) / 2.0D0 )

      REAL FLMIN
C-----------------------------------------------
C     DATA FTPI,TTPI / 4.188790205, 2.094395102 /
C     FTPI=(4/3)*PI, TTPI=(2/3)*PI
C
C     DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
      MDEMI = -UNDEMI
      TTPI = ACOS(MDEMI)
      FTPI = DEUX*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS1,CS2,FLMIN)
      FLM = DEUX*SQRT(FLMIN)
      NINDEX3=0 
      C1 = UNDEMI*SQRT(VINGT7)      
      C2 = DEUX*SQRT(THIRD)      
      DO NN = 1, NEL
        CS1 = SIG(NN,1)
        CS2 = SIG(NN,2)
        CS3 = SIG(NN,3)
        CS4 = SIG(NN,4)
        CS5 = SIG(NN,5)
        CS6 = SIG(NN,6)
        PR = -(CS1+CS2+CS3) * THIRD
        CS1 = CS1 + PR
        CS2 = CS2 + PR
        CS3 = CS3 + PR
        S4_2 = CS4*CS4
        S5_2 = CS5*CS5
        S6_2 = CS6*CS6
        AAA(NN)=S4_2 + S5_2 + S6_2 - CS1*CS2
     &         - CS2*CS3 - CS1*CS3
        NORMINF(NN) = MAX(ABS(CS1),ABS(CS2),ABS(CS3),
     &      ABS(CS4),ABS(CS5),ABS(CS6))
        NORMINF(NN) = EM10*NORMINF(NN)
C cas racine triple
        AA = MAX(AAA(NN),NORMINF(NN))
        SQ_AA = SQRT(AA)
C
        BB=CS1*S5_2 + CS2*S6_2
     &     + CS3*S4_2 - CS1*CS2*CS3
     &     - DEUX*CS4*CS5*CS6
C
        CC=-C1/MAX(EM10,SQ_AA)*BB/MAX(EM20,AA)
c        CC=-SQRT(VINGT7/MAX(EM20,AA))*BB*UNDEMI/MAX(EM20,AA)
        CC= MIN(CC,UN)
        CC= MAX(CC,-UN)
        ANGP=ACOS(CC) * THIRD
        DD=C2*SQ_AA

        COS_ANGP = COS(ANGP)
        SIN_ANGP = SIN(ANGP)
        STR1(NN)=DD*COS_ANGP
        STR2(NN)=DD*((-UNDEMI*COS_ANGP) - (SIN_ANGP*SQRT3DMI))
        STR3(NN)=DD*((-UNDEMI*COS_ANGP) + (SIN_ANGP*SQRT3DMI))

c       STR1(NN)=DD*COS(ANGP)
c       STR2(NN)=DD*COS(ANGP+FTPI)
c       STR3(NN)=DD*COS(ANGP+TTPI)
C
        VALDP1(NN) = STR1(NN)-PR
        VALDP2(NN) = STR2(NN)-PR
        VALDP3(NN) = STR3(NN)-PR
C renforcement de precision en compression simple----
        IF(ABS(STR3(NN)).GT.ABS(STR1(NN))
     &     .AND.AAA(NN).GT.NORMINF(NN)) THEN
         AA=STR1(NN)
         STR1(NN)=STR3(NN)
         STR3(NN)=AA
         INDEX3(NN) = 1
        ELSE
         INDEX3(NN) = 0
        ENDIF
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR1(NN)),ABS(STR3(NN)))
        TOL1(NN)= MAX(EM20,FLM*STRMAX*STRMAX)
        TOL2(NN)=FLM*STRMAX * THIRD
        A(NN,1,1)=CS1-STR1(NN)
        A(NN,2,2)=CS2-STR1(NN)
        A(NN,3,3)=CS3-STR1(NN)
        A(NN,1,2)=CS4
        A(NN,2,1)=CS4
        A(NN,2,3)=CS5
        A(NN,3,2)=CS5
        A(NN,1,3)=CS6
        A(NN,3,1)=CS6
C
        B(NN,1,1)=A(NN,2,1)*A(NN,3,2)-A(NN,3,1)
     .           *A(NN,2,2)
        B(NN,1,2)=A(NN,2,2)*A(NN,3,3)-A(NN,3,2)
     .           *A(NN,2,3)
        B(NN,1,3)=A(NN,2,3)*A(NN,3,1)-A(NN,3,3)
     .           *A(NN,2,1)
        B(NN,2,1)=A(NN,3,1)*A(NN,1,2)-A(NN,1,1)
     .           *A(NN,3,2)
        B(NN,2,2)=A(NN,3,2)*A(NN,1,3)-A(NN,1,2)
     .           *A(NN,3,3)
        B(NN,2,3)=A(NN,3,3)*A(NN,1,1)-A(NN,1,3)
     .           *A(NN,3,1)
        B(NN,3,1)=A(NN,1,1)*A(NN,2,2)-A(NN,2,1)
     .           *A(NN,1,2)
        B(NN,3,2)=A(NN,1,2)*A(NN,2,3)-A(NN,2,2)
     .           *A(NN,1,3)
        B(NN,3,3)=A(NN,1,3)*A(NN,2,1)-A(NN,2,3)
     .           *A(NN,1,1)
        XMAG1(NN)=B(NN,1,1)*B(NN,1,1)+B(NN,2,1)*B(NN,2,1)+
     .             B(NN,3,1)*B(NN,3,1)
        XMAG2(NN)=B(NN,1,2)*B(NN,1,2)+B(NN,2,2)*B(NN,2,2)+
     .                  B(NN,3,2)*B(NN,3,2)
        XMAG3(NN)=B(NN,1,3)*B(NN,1,3)+B(NN,2,3)*B(NN,2,3)+
     .                  B(NN,3,3)*B(NN,3,3)

      ENDDO
C 
      NINDEX1 = 0
      NINDEX2 = 0
      DO NN = 1, NEL
        XMAXV(NN)=MAX(XMAG1(NN),XMAG2(NN),XMAG3(NN))
        IF(XMAG1(NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG2(NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN).LT.NORMINF(NN)) THEN
          VALDP1(NN) = SIG(NN,1)
          VALDP2(NN) = SIG(NN,2)
          VALDP3(NN) = SIG(NN,3)
          V(NN,1,1) = UN
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = UN
          V(NN,3,2) = ZERO        
        ELSEIF(XMAXV(NN).GT.TOL1(NN)*TOL1(NN)) THEN
          NINDEX1 = NINDEX1 + 1
          INDEX1(NINDEX1) = NN
        ELSE
          NINDEX2 = NINDEX2 + 1
          INDEX2(NINDEX2) = NN
        ENDIF
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        LMAX = LMAXV(NN)
        XMAXINV = UN/SQRT(XMAXV(NN))
        V(NN,1,1)=B(NN,1,LMAX)*XMAXINV
        V(NN,2,1)=B(NN,2,LMAX)*XMAXINV
        V(NN,3,1)=B(NN,3,LMAX)*XMAXINV
        A(NN,1,1)=A(NN,1,1)+STR1(NN)-STR3(NN)
        A(NN,2,2)=A(NN,2,2)+STR1(NN)-STR3(NN)
        A(NN,3,3)=A(NN,3,3)+STR1(NN)-STR3(NN)
C
        B(NN,1,1)=A(NN,2,1)*V(NN,3,1)-A(NN,3,1)*V(NN,2,1)
        B(NN,1,2)=A(NN,2,2)*V(NN,3,1)-A(NN,3,2)*V(NN,2,1)
        B(NN,1,3)=A(NN,2,3)*V(NN,3,1)-A(NN,3,3)*V(NN,2,1)
        B(NN,2,1)=A(NN,3,1)*V(NN,1,1)-A(NN,1,1)*V(NN,3,1)
        B(NN,2,2)=A(NN,3,2)*V(NN,1,1)-A(NN,1,2)*V(NN,3,1)
        B(NN,2,3)=A(NN,3,3)*V(NN,1,1)-A(NN,1,3)*V(NN,3,1)
        B(NN,3,1)=A(NN,1,1)*V(NN,2,1)-A(NN,2,1)*V(NN,1,1)
        B(NN,3,2)=A(NN,1,2)*V(NN,2,1)-A(NN,2,2)*V(NN,1,1)
        B(NN,3,3)=A(NN,1,3)*V(NN,2,1)-A(NN,2,3)*V(NN,1,1)
        XMAG1(NN)=B(NN,1,1)*B(NN,1,1)+B(NN,2,1)*B(NN,2,1)+
     .             B(NN,3,1)*B(NN,3,1)
        XMAG2(NN)=B(NN,1,2)*B(NN,1,2)+B(NN,2,2)*B(NN,2,2)+
     .                  B(NN,3,2)*B(NN,3,2)
        XMAG3(NN)=B(NN,1,3)*B(NN,1,3)+B(NN,2,3)*B(NN,2,3)+
     .                  B(NN,3,3)*B(NN,3,3)
C
        XMAXV(NN)=MAX(XMAG1(NN),XMAG2(NN),XMAG3(NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX1
        NN = INDEX1(N)
        IF(XMAG1(NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG2(NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(NN,1,1)**2+V(NN,2,1)**2)
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,3)=B(NN,1,LMAX)*XMAXINV
          V(NN,2,3)=B(NN,2,LMAX)*XMAXINV
          V(NN,3,3)=B(NN,3,LMAX)*XMAXINV
          V(NN,1,2)=V(NN,2,3)*V(NN,3,1)-V(NN,2,1)*V(NN,3,3)
          V(NN,2,2)=V(NN,3,3)*V(NN,1,1)-V(NN,3,1)*V(NN,1,3)
          V(NN,3,2)=V(NN,1,3)*V(NN,2,1)-V(NN,1,1)*V(NN,2,3)
          C1 = V(NN,1,2)*V(NN,1,2)+V(NN,2,2)*V(NN,2,2)+
     &         V(NN,3,2)*V(NN,3,2)
          VMAG=UN/SQRT(C1)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSEIF(VMAG.GT.TOL2(NN))THEN
          V(NN,1,2)=-V(NN,2,1)/VMAG
          V(NN,2,2)=V(NN,1,1)/VMAG
          V(NN,3,2)=ZERO
        ELSE
          V(NN,1,2)=UN
          V(NN,2,2)=ZERO
          V(NN,3,2)=ZERO  
        ENDIF
      ENDDO
C . . . . . . . . . . . . .
C    SOLUTION DOUBLE
C . . . . . . . . . . . . .
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        XMAG1(NN)=A(NN,1,1)*A(NN,1,1)+A(NN,2,1)*A(NN,2,1)
        XMAG2(NN)=A(NN,1,2)*A(NN,1,2)+A(NN,2,2)*A(NN,2,2)
        XMAG3(NN)=A(NN,1,3)*A(NN,1,3)+A(NN,2,3)*A(NN,2,3)
C
        XMAXV(NN)=MAX(XMAG1(NN),XMAG2(NN),XMAG3(NN))
      ENDDO
C
#include      "vectorize.inc"
      DO N = 1, NINDEX2
        NN = INDEX2(N)
        IF(XMAG1(NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG2(NN).EQ.XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(MAX(ABS(A(NN,3,1)),ABS(A(NN,3,2)),ABS(A(NN,3,3)))
     &       .LT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,1)= ZERO
          V(NN,2,1)= ZERO
          V(NN,3,1)= UN  
          V(NN,1,2)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,2)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,2)= ZERO
C
        ELSEIF(XMAXV(NN).GT.TOL2(NN))THEN
          XMAXINV = UN/XMAXV(NN)
          V(NN,1,1)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,1)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,1)= ZERO
          V(NN,1,2)=-A(NN,3,LMAX)*V(NN,2,1)
          V(NN,2,2)= A(NN,3,LMAX)*V(NN,1,1)
          V(NN,3,2)= A(NN,1,LMAX)*V(NN,2,1)-A(NN,2,LMAX)*V(NN,1,1)
          C1 = V(NN,1,2)*V(NN,1,2)+V(NN,2,2)*V(NN,2,2)+
     &         V(NN,3,2)*V(NN,3,2)
          VMAG=UN/SQRT(C1)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSE
          VALDP1(NN) = SIG(NN,1)
          VALDP2(NN) = SIG(NN,2)
          VALDP3(NN) = SIG(NN,3)
          V(NN,1,1) = UN
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = UN
          V(NN,3,2) = ZERO        
          INDEX3(NN) = 0
        ENDIF
      ENDDO
C
      DO NN = 1, NEL
        VECDP1(NN)=V(NN,1,1)
        VECDP2(NN)=V(NN,2,1)
        VECDP3(NN)=V(NN,3,1)
        VECDP4(NN)=V(NN,1,2)
        VECDP5(NN)=V(NN,2,2)
        VECDP6(NN)=V(NN,3,2)
        VECDP7(NN)=VECDP2(NN)*VECDP6(NN)-VECDP3(NN)*VECDP5(NN)
        VECDP8(NN)=VECDP3(NN)*VECDP4(NN)-VECDP1(NN)*VECDP6(NN)
        VECDP9(NN)=VECDP1(NN)*VECDP5(NN)-VECDP2(NN)*VECDP4(NN)
      ENDDO
C
      DO NN = 1, NEL
        VAL(NN,1)=VALDP1(NN)
        VAL(NN,2)=VALDP2(NN)
        VAL(NN,3)=VALDP3(NN)
        VEC(NN,1)=VECDP1(NN)
        VEC(NN,2)=VECDP2(NN)
        VEC(NN,3)=VECDP3(NN)
        VEC(NN,4)=VECDP4(NN)
        VEC(NN,5)=VECDP5(NN)
        VEC(NN,6)=VECDP6(NN)
        VEC(NN,7)=VECDP7(NN)
        VEC(NN,8)=VECDP8(NN)
        VEC(NN,9)=VECDP9(NN)
      ENDDO
C reecriture pour contourner probleme sur itanium2 comp 9. + latency=16
      DO NN = 1, NEL
        IF(INDEX3(NN) == 1) THEN
C str utilise com tableau temporaire au lieu de scalaires temporaires
          S1=VEC(NN,7)
          S2=VEC(NN,8)
          S3=VEC(NN,9)
          VEC(NN,7)=VEC(NN,1)
          VEC(NN,8)=VEC(NN,2)
          VEC(NN,9)=VEC(NN,3)
          VEC(NN,1)=-S1
          VEC(NN,2)=-S2
          VEC(NN,3)=-S3
        ENDIF
      ENDDO
      RETURN
      END
 
