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
     A      SOUNDSP, VISCMAX, UVAR    , OFF             )

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
      INTEGER VPVEC,ICASE
      my_real 
     . EY,C1,C2,ET,VMU,VMU0
     . , A,B,C,D,VAR
     . , AXX,BXX,CXX,AYY,BYY,CYY,AZZ,BZZ,CZZ
     . , AXY,BXY,CXY,AYZ,BYZ,CYZ,AZX,BZX,CZX
     . , C1XX,C2XX,ETXX,VMUXX, C1YY,C2YY,ETYY,VMUYY
     . , C1XY,C2XY,GTXY,VMUXY, C1YZ,C2YZ,GTYZ,VMUYZ
     . , C1ZX,C2ZX,GTZX,VMUZX, P0,PHI,GAMA0,FAC,FAC1
     . , S(3,3),SIGPR(3),DIRPR(3,3)
     . , C1ZZ,C2ZZ,ETZZ,VMUZZ,SIGT_CUTOFF,SIGC_CUTOFF
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
      IF(TIME==ZERO) THEN
        DO 1000 J=1,NUVAR
           DO 1000 I=1,NEL
             UVAR(I,J)=ZERO
 1000     CONTINUE
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
      !!
      VPVEC = 0
C COMPUTE VOLUMETRIC STRAIN-AIR PRESSURE
      DO 900 I=1,NEL
        GAMA(I) = RHO0(I)/RHO(I)-ONE+GAMA0
        VAR = -(P0*GAMA(I))/(ONE+GAMA(I)-PHI+EM15)
        SIGAIR(I) = MAX(ZERO,VAR)
  900 CONTINUE
      ICASE = ABS(KEN) + 1       
      SELECT CASE (ICASE)
C----------------------------------
       CASE(1)
C----------------------------------
         FAC = UPARAM(9)
         FAC1 = UPARAM(10)

         DO 100 I=1,NEL
           IF(IFUNC(1)/=0) THEN
             SYIELD(I)=FAC*FINTER(IFUNC(1),GAMA(I),NPF,TF,B*C)
           ELSE
             SYIELD(I) = ABS(A+B*(ONE+C*GAMA(I)))
           ENDIF
           IF(IFUNC(2)/=0)THEN
              DAV = (EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))*THIRD
              E1 = EPSPXX(I) - DAV
              E2 = EPSPYY(I) - DAV
              E3 = EPSPZZ(I) - DAV
              E4 = HALF*EPSPXY(I)
              E5 = HALF*EPSPYZ(I)
              E6 = HALF*EPSPZX(I)
              EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
              EPSP = SQRT(THREE*EPSP)/THREE_HALF
              SYIELD(I) = FAC1*SYIELD(I)*(FINTER(IFUNC(2),EPSP,NPF,TF,B*C))
           ENDIF
  100     CONTINUE

          DO 110 I=1,NEL
            SIGSXX(I)=SIGOXX(I)+SIGAIR(I)
            SIGSYY(I)=SIGOYY(I)+SIGAIR(I)
            SIGSZZ(I)=SIGOZZ(I)+SIGAIR(I)
            SIGSXY(I)=SIGOXY(I)
            SIGSYZ(I)=SIGOYZ(I)
            SIGSZX(I)=SIGOZX(I)
  110     CONTINUE

C COMP    UTE TRIAL STRESSES
          DO 120 I=1,NEL
            SIGSXX(I)=SIGSXX(I)+EY*EPSPXX(I)*TIMESTEP
            SIGSYY(I)=SIGSYY(I)+EY*EPSPYY(I)*TIMESTEP
            SIGSZZ(I)=SIGSZZ(I)+EY*EPSPZZ(I)*TIMESTEP
            SIGSXY(I)=SIGSXY(I)+EY*EPSPXY(I)*TIMESTEP*HALF
            SIGSYZ(I)=SIGSYZ(I)+EY*EPSPYZ(I)*TIMESTEP*HALF
            SIGSZX(I)=SIGSZX(I)+EY*EPSPZX(I)*TIMESTEP*HALF
  120     CONTINUE
C si ancienne methode alors ....
          IF (VPVEC==1) THEN
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
            SIGPR(1)=MIN(SYIELD(I),ABS(SIGPR(1)))*SIGN(ONE,SIGPR(1))
            SIGPR(2)=MIN(SYIELD(I),ABS(SIGPR(2)))*SIGN(ONE,SIGPR(2))
            SIGPR(3)=MIN(SYIELD(I),ABS(SIGPR(3)))*SIGN(ONE,SIGPR(3))

C TRANSFORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
            DO 131 K=1,3
              DO 131 L=1,3
                S(K,L)=ZERO
                IF(K==L) S(K,L)= SIGPR(K)
  131       CONTINUE
            CALL DREH(S,DIRPR,1,1,1)
   
            SIGSXX(I)=S(1,1)
            SIGSXY(I)=S(2,1)
            SIGSYY(I)=S(2,2)
            SIGSZX(I)=S(3,1)
            SIGSYZ(I)=S(3,2)
            SIGSZZ(I)=S(3,3)
  130      CONTINUE
C fin     ancienne methode
C sino    n
          ELSE
            DO I = 1, NEL
              SIGV(I,1) = SIGSXX(I)
              SIGV(I,2) = SIGSYY(I)
              SIGV(I,3) = SIGSZZ(I)
              SIGV(I,4) = SIGSXY(I)
              SIGV(I,5) = SIGSYZ(I)
              SIGV(I,6) = SIGSZX(I)
            ENDDO
C   for a simple precision executing
            IF (IRESP==1) THEN
              CALL VALPVECDP_V(SIGV,SIGPRV,DIRPRV,NEL)
            ELSE
              CALL VALPVEC_V(SIGV,SIGPRV,DIRPRV,NEL)
            ENDIF
            DO I = 1, NEL
C YIEL D CRITERIA - SCALING
              SIGPRV(I,1)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,1))),SIGPRV(I,1))
              SIGPRV(I,2)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,2))),SIGPRV(I,2))
              SIGPRV(I,3)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,3))),SIGPRV(I,3))
C TRANSFORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
              SIGSXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
              SIGSYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
              SIGSZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
              SIGSXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
              SIGSYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
              SIGSZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
            ENDDO
          ENDIF
C 
C COMPUTE UPDATED STRESSES AND SOUND SPEED
          DO 140 I=1,NEL
            SIGNXX(I)=OFF(I)*SIGSXX(I)-SIGAIR(I)
            SIGNYY(I)=OFF(I)*SIGSYY(I)-SIGAIR(I)
            SIGNZZ(I)=OFF(I)*SIGSZZ(I)-SIGAIR(I)
            SIGNXY(I)=OFF(I)*SIGSXY(I)
            SIGNYZ(I)=OFF(I)*SIGSYZ(I)
            SIGNZX(I)=OFF(I)*SIGSZX(I)

            SOUNDSP(I)=SQRT(EY/RHO0(I))
  140     CONTINUE

C--------------------------
       CASE(2)  
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
     &             ABS(EPSPXX(I)),ABS(EPSPYY(I)),ABS(EPSPZZ(I)),
     &             ABS(EPSPXY(I)),ABS(EPSPYZ(I)),ABS(EPSPZX(I)))
           E(I)=C1*EDOT(I)+C2
           E(I)=MAX(E(I),EY)
           EPET(I)=(E(I)+ET)/VMU
           EMET(I)=(E(I)*ET)/VMU 
           EPETS(I)=(E(I)+ET)/VMU0
           EMETS(I)=TWO*(E(I)*ET)/VMU0
  200    CONTINUE

C COMPUTE YIELD STRESS
         DO 210 I=1,NEL
           IF(IFUNC(1)/=0) THEN
             SYIELD(I)=FAC*FINTER(IFUNC(1),GAMA(I),NPF,TF,B*C)
           ELSE
             SYIELD(I) = ABS(A+B*(ONE+C*GAMA(I)))
           ENDIF
           IF(IFUNC(2)/=0)THEN
              DAV = (EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))*THIRD
              E1 = EPSPXX(I) - DAV
              E2 = EPSPYY(I) - DAV
              E3 = EPSPZZ(I) - DAV
              E4 = HALF*EPSPXY(I)
              E5 = HALF*EPSPYZ(I)
              E6 = HALF*EPSPZX(I)
              EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
              EPSP = SQRT(THREE*EPSP)/THREE_HALF
              SYIELD(I) = FAC1*SYIELD(I)*(FINTER(IFUNC(2),EPSP,NPF,TF,B*C))
           ENDIF
  210    CONTINUE

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
           DEXY(I)=EPSXY(I)* HALF
           DEYZ(I)=EPSYZ(I)* HALF
           DEZX(I)=EPSZX(I)* HALF

           DEDTXX(I)=EPSPXX(I)
           DEDTYY(I)=EPSPYY(I)
           DEDTZZ(I)=EPSPZZ(I)
           DEDTXY(I)=EPSPXY(I) * HALF
           DEDTYZ(I)=EPSPYZ(I) * HALF
           DEDTZX(I)=EPSPZX(I) * HALF
  220    CONTINUE
C COMPUTE STRESS RATES
         DO 230 I=1,NEL
           DSDTXX(I)=E(I)*DEDTXX(I)-EPET(I)*SIGSXX(I)+EMET(I)*DEXX(I)
           DSDTYY(I)=E(I)*DEDTYY(I)-EPET(I)*SIGSYY(I)+EMET(I)*DEYY(I)
           DSDTZZ(I)=E(I)*DEDTZZ(I)-EPET(I)*SIGSZZ(I)+EMET(I)*DEZZ(I)
           DSDTXY(I)=E(I)*DEDTXY(I)-EPETS(I)*SIGSXY(I)+EMETS(I)*DEXY(I)
           DSDTYZ(I)=E(I)*DEDTYZ(I)-EPETS(I)*SIGSYZ(I)+EMETS(I)*DEYZ(I)
           DSDTZX(I)=E(I)*DEDTZX(I)-EPETS(I)*SIGSZX(I)+EMETS(I)*DEZX(I)
  230    CONTINUE

C COMPUTE TRIAL STRESSES
         DO 240 I=1,NEL
           SIGSXX(I)=SIGSXX(I)+DSDTXX(I)*TIMESTEP
           SIGSYY(I)=SIGSYY(I)+DSDTYY(I)*TIMESTEP
           SIGSZZ(I)=SIGSZZ(I)+DSDTZZ(I)*TIMESTEP
           SIGSXY(I)=SIGSXY(I)+DSDTXY(I)*TIMESTEP
           SIGSYZ(I)=SIGSYZ(I)+DSDTYZ(I)*TIMESTEP
           SIGSZX(I)=SIGSZX(I)+DSDTZX(I)*TIMESTEP
  240    CONTINUE
         IF(KEN<0)GOTO 255
C
C si ancienne methode alors ....
         IF (VPVEC==1) THEN
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
           SIGPR(1)=MIN(SYIELD(I),ABS(SIGPR(1)))*SIGN(ONE,SIGPR(1))
           SIGPR(2)=MIN(SYIELD(I),ABS(SIGPR(2)))*SIGN(ONE,SIGPR(2))
           SIGPR(3)=MIN(SYIELD(I),ABS(SIGPR(3)))*SIGN(ONE,SIGPR(3))

C TRANSFORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
           DO 251 K=1,3
             DO 251 L=1,3
               S(K,L)=ZERO
               IF(K==L) S(K,L)= SIGPR(K)
  251      CONTINUE
           CALL DREH(S,DIRPR,1,1,1)
   
           SIGSXX(I)=S(1,1)
           SIGSXY(I)=S(2,1)
           SIGSYY(I)=S(2,2)
           SIGSZX(I)=S(3,1)
           SIGSYZ(I)=S(3,2)
           SIGSZZ(I)=S(3,3)
  250     CONTINUE
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
     .                 + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
     .                 + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
             SIGSYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .                 + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
     .                 + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
             SIGSZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .                 + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
     .                 + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
             SIGSXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
     .                 + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .                 + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
             SIGSYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
     .                 + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .                 + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
             SIGSZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
     .                 + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .                 + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
          ENDDO
        ENDIF
C fin chgt
  255   CONTINUE
C COMPUTE UPDATED STRESSES AND SOUND SPEED
        DO 260 I=1,NEL
          SIGNXX(I)=OFF(I)*SIGSXX(I)-SIGAIR(I)
          SIGNYY(I)=OFF(I)*SIGSYY(I)-SIGAIR(I)
          SIGNZZ(I)=OFF(I)*SIGSZZ(I)-SIGAIR(I)
          SIGNXY(I)=OFF(I)*SIGSXY(I)
          SIGNYZ(I)=OFF(I)*SIGSYZ(I)
          SIGNZX(I)=OFF(I)*SIGSZX(I)

          SOUNDSP(I)=SQRT(E(I)/RHO0(I))
  260   CONTINUE
C--------------------------
      CASE(3)
C--------------------------
       FAC = UPARAM(9)
       FAC1 = UPARAM(10)
       SIGT_CUTOFF = UPARAM(11)
       SIGC_CUTOFF = SIGT_CUTOFF
       DO  I=1,NEL
         IF(IFUNC(1)/=0) THEN
           SYIELD(I)=FAC*FINTER(IFUNC(1),GAMA(I),NPF,TF,B*C)
         ELSE
           SYIELD(I) = ABS(A+B*(ONE+C*GAMA(I)))
         ENDIF
         IF(GAMA(I) > ZERO .AND. (GAMA(I)-UVAR(I,1)) < ZERO) SIGC_CUTOFF = ZERO
         IF(IFUNC(2)/=0)THEN
            DAV = (EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))*THIRD
            E1 = EPSPXX(I) - DAV
            E2 = EPSPYY(I) - DAV
            E3 = EPSPZZ(I) - DAV
            E4 = HALF*EPSPXY(I)
            E5 = HALF*EPSPYZ(I)
            E6 = HALF*EPSPZX(I)
            EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
            EPSP = SQRT(THREE*EPSP)/THREE_HALF
            SYIELD(I) = FAC1*SYIELD(I)*(FINTER(IFUNC(2),EPSP,NPF,TF,B*C))
         ENDIF
       ENDDO

       DO  I=1,NEL
         SIGSXX(I)=SIGOXX(I)+SIGAIR(I)
         SIGSYY(I)=SIGOYY(I)+SIGAIR(I)
         SIGSZZ(I)=SIGOZZ(I)+SIGAIR(I)
         SIGSXY(I)=SIGOXY(I)
         SIGSYZ(I)=SIGOYZ(I)
         SIGSZX(I)=SIGOZX(I)
       ENDDO

C COMPUTE TRIAL STRESSES
        DO  I=1,NEL
          SIGSXX(I)=SIGSXX(I)+EY*EPSPXX(I)*TIMESTEP
          SIGSYY(I)=SIGSYY(I)+EY*EPSPYY(I)*TIMESTEP
          SIGSZZ(I)=SIGSZZ(I)+EY*EPSPZZ(I)*TIMESTEP
          SIGSXY(I)=SIGSXY(I)+EY*EPSPXY(I)*TIMESTEP*FOURTH
          SIGSYZ(I)=SIGSYZ(I)+EY*EPSPYZ(I)*TIMESTEP*FOURTH
          SIGSZX(I)=SIGSZX(I)+EY*EPSPZX(I)*TIMESTEP*FOURTH
        ENDDO
        DO I = 1, NEL
            SIGV(I,1) = SIGSXX(I)
            SIGV(I,2) = SIGSYY(I)
            SIGV(I,3) = SIGSZZ(I)
            SIGV(I,4) = SIGSXY(I)
            SIGV(I,5) = SIGSYZ(I)
            SIGV(I,6) = SIGSZX(I)
        ENDDO
C           for a simple precision executing
          IF (IRESP==1) THEN
            CALL VALPVECDP_V(SIGV,SIGPRV,DIRPRV,NEL)
          ELSE
            CALL VALPVEC_V(SIGV,SIGPRV,DIRPRV,NEL)
          ENDIF
          DO I =1, NEL
C YIELD   CRITERIA - SCALING
             IF(ABS(GAMA(I)) < EM10) THEN
               SIGPRV(I,1)=SIGN(MIN(ABS(SIGPRV(I,1)), SIGT_CUTOFF),SIGPRV(I,1))
               SIGPRV(I,2)=SIGN(MIN(ABS(SIGPRV(I,2)), SIGT_CUTOFF),SIGPRV(I,2))
               SIGPRV(I,3)=SIGN(MIN(ABS(SIGPRV(I,3)), SIGT_CUTOFF),SIGPRV(I,3))
             ELSEIF(GAMA(I) < ZERO )then            
               !!IF(DELTA_GAMA(I) < ZERO)then 
                  SIGPRV(I,1)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,1))),SIGPRV(I,1))
                  SIGPRV(I,2)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,2))),SIGPRV(I,2))
                  SIGPRV(I,3)=SIGN(MIN(SYIELD(I),ABS(SIGPRV(I,3))),SIGPRV(I,3))
               !! ELSE
                  SIGPRV(I,1)=MIN(SIGPRV(I,1), SIGT_CUTOFF)
                  SIGPRV(I,2)=MIN(SIGPRV(I,2), SIGT_CUTOFF)
                  SIGPRV(I,3)=MIN(SIGPRV(I,3), SIGT_CUTOFF)
               !!  ENDIF   
              ELSE
                 SIGPRV(I,1)=SIGN(MIN(ABS(SIGPRV(I,1)), SIGT_CUTOFF),SIGPRV(I,1))
                 SIGPRV(I,2)=SIGN(MIN(ABS(SIGPRV(I,2)), SIGT_CUTOFF),SIGPRV(I,2))
                 SIGPRV(I,3)=SIGN(MIN(ABS(SIGPRV(I,3)), SIGT_CUTOFF),SIGPRV(I,3))
                 !!SIGPRV(I,1)=MIN(SIGPRV(I,1), SIGT_CUTOFF)
                 !!SIGPRV(I,2)=MIN(SIGPRV(I,2), SIGT_CUTOFF)
                 !!SIGPRV(I,3)=MIN(SIGPRV(I,3), SIGT_CUTOFF)
              !!  IF(DELTA_GAMA(I) < ZERO) THEN
                  SIGPRV(I,1)=MAX(SIGPRV(I,1), ZERO)
                  SIGPRV(I,2)=MAX(SIGPRV(I,2), ZERO)
                  SIGPRV(I,3)=MAX(SIGPRV(I,3), ZERO)
              !!  ENDIF
              ENDIF  
C TRANSF  ORM PRINCIPAL STRESSES (SIGMA_K) TO GLOBAL STRESSES (SIGMA_IJ)
              SIGSXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
!!     
              SIGSYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
!!     
              SIGSZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
!!     
              SIGSXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
!!     
              SIGSYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
     .                  + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
!!     
              SIGSZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
     .                  + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .                  + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
!!      
          ENDDO
C fin chgt
C COMPUTE UPDATED STRESSES AND SOUND SPEED
C
        DO  I=1,NEL
          SIGNXX(I)=OFF(I)*SIGSXX(I)-SIGAIR(I)
          SIGNYY(I)=OFF(I)*SIGSYY(I)-SIGAIR(I)
          SIGNZZ(I)=OFF(I)*SIGSZZ(I)-SIGAIR(I)
          SIGNXY(I)=OFF(I)*SIGSXY(I)
          SIGNYZ(I)=OFF(I)*SIGSYZ(I)
          SIGNZX(I)=OFF(I)*SIGSZX(I)
          SOUNDSP(I)=SQRT(EY/RHO0(I))
        ENDDO
      END SELECT 

      RETURN

      END

Chd|====================================================================
Chd|  JACOBIEW                      source/materials/mat/mat033/sigeps33.F
Chd|-- called by -----------
Chd|        FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|        HENCKY_STRAIN                 source/tools/seatbelts/shell_reactivation.F
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
C NROT        NUMBER OF ROTATIONS
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
          IF(IZ>IS) A(IS,IZ) = A(IZ,IS) 
          EV(IZ,IS)=ZERO
  120   CONTINUE
        B(IZ)=A(IZ,IZ)
        EW(IZ)=B(IZ)
        Z(IZ)=0.
        EV(IZ,IZ)=ONE
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
 
        IF (SUMRS ==ZERO) GOTO 9000
        IF (ITER > 4)   THEN
          EPS = ZERO
        ELSE
          EPS = ONE_FIFTH*SUMRS/N**2
        ENDIF

        DO 220 IZ=1,N-1
          DO 210 IS=IZ+1,N
            G = 100. * ABS(A(IZ,IS))
            IF (ITER>4 .AND. ABS(EW(IZ))+G==ABS(EW(IZ))
     &      .AND. ABS(EW(IS))+G==ABS(EW(IS))) THEN
              A(IZ,IS)=ZERO
            ELSE IF (ABS(A(IZ,IS)) > EPS) THEN
              H = EW(IS)-EW(IZ)
              IF (ABS(H)+G==ABS(H)) THEN
                T = A(IZ,IS)/H
              ELSE
                THETA = HALF*H/A(IZ,IS)
                T=ONE/(ABS(THETA)+SQRT(ONE+THETA**2))
                IF (THETA < ZERO) T=-T
              ENDIF
              C=ONE/SQRT(ONE+T**2)
              S=T*C
              TAU=S/(ONE+C)
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
      IF(II<=0) II=1
      IF(JJ<=0) JJ=1
                       
      DO 20 I=1,3                                                               
        DO 20 J=1,3                                                               
          J1=JJ+J-1                                                                 
          X=0.0                                                                     
          DO 10 K=1,3                                                               
C          IF(TR(K,I)==ZERO) GOTO 10                                               
           I1=K+II-1                                                                 
C          IF(B(I1,J1)==ZERO) GOTO 10                                             
           IF(KEN/=1) X=X+TR(K,I)*B(I1,J1)                                                   
           IF(KEN==1) X=X+TR(I,K)*B(I1,J1) 
 10       CONTINUE                                                                  
 20   LK(I,J)=X
                                                                 
      DO 40 I=1,3                                                               
        DO 40 J=1,3                                                               
          X=ZERO                                                       
          DO 30 K=1,3                                                               
C          IF(TR(K,J)==ZERO) GOTO 30                                               
C          IF(LK(I,K)==ZERO) GOTO 30                                                
           IF(KEN/=1) X=X+LK(I,K)*TR(K,J)                                                      
           IF(KEN==1) X=X+LK(I,K)*TR(J,K)                                                      
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
      MDEMI = -HALF
      TTPI = ACOS(MDEMI)
      FTPI = TWO*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = TWO*SQRT(FLMIN)
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
     &     - TWO*CS(4)*CS(5)*CS(6)     
C
        CC=-SQRT(TWENTY7/MAX(EM20,AA))*BB*HALF/MAX(EM20,AA)
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
        ANGP=ACOS(CC) * THIRD
        DD=TWO*SQRT(AA * THIRD)
        STR(1,NN)=DD*COS(ANGP)
        STR(2,NN)=DD*COS(ANGP+FTPI)
        STR(3,NN)=DD*COS(ANGP+TTPI)
C
        VAL(1,NN) = STR(1,NN)-PR
        VAL(2,NN) = STR(2,NN)-PR
        VAL(3,NN) = STR(3,NN)-PR
C renforcement de precision en compression----
        IF(ABS(STR(3,NN))>ABS(STR(1,NN))
     &     .AND.AAA(NN)>NORMINF(NN)) THEN
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN)<NORMINF(NN)) THEN
          VAL(1,NN) = SIG(1,NN)
          VAL(2,NN) = SIG(2,NN)
          VAL(3,NN) = SIG(3,NN)
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
          V(3,2,NN) = ZERO

        ELSEIF(XMAXV(NN)>TOL1(NN)) THEN
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
        XMAXINV = ONE/XMAXV(NN)
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(1,1,NN)**2+V(2,1,NN)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,3,NN)=B(1,LMAX,NN)*XMAXINV
          V(2,3,NN)=B(2,LMAX,NN)*XMAXINV
          V(3,3,NN)=B(3,LMAX,NN)*XMAXINV
          V(1,2,NN)=V(2,3,NN)*V(3,1,NN)-V(2,1,NN)*V(3,3,NN)
          V(2,2,NN)=V(3,3,NN)*V(1,1,NN)-V(3,1,NN)*V(1,3,NN)
          V(3,2,NN)=V(1,3,NN)*V(2,1,NN)-V(1,1,NN)*V(2,3,NN)
          VMAG=ONE/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSEIF(VMAG>TOL2(NN))THEN
          V(1,2,NN)=-V(2,1,NN)/VMAG
          V(2,2,NN)=V(1,1,NN)/VMAG
          V(3,2,NN)=ZERO
        ELSE
          V(1,2,NN)=ONE
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(3,1,NN)),ABS(A(3,2,NN)),ABS(A(3,3,NN)))
     &       <TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)= ZERO
          V(2,1,NN)= ZERO
          V(3,1,NN)= ONE
          V(1,2,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,2,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,2,NN)= ZERO
C
        ELSEIF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,1,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,1,NN)= ZERO
          V(1,2,NN)=-A(3,LMAX,NN)*V(2,1,NN)
          V(2,2,NN)= A(3,LMAX,NN)*V(1,1,NN)
          V(3,2,NN)= A(1,LMAX,NN)*V(2,1,NN)-A(2,LMAX,NN)*V(1,1,NN)
          VMAG=ONE/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSE
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
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
      MDEMI = -HALF
      TTPI = ACOS(MDEMI)
      FTPI = TWO*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = TWO*SQRT(FLMIN)
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
     &     - TWO*CS(4)*CS(5)*CS(6)
C
        CC=-SQRT(TWENTY7/MAX(EM20,AA))*BB*HALF/MAX(EM20,AA)
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
        ANGP=ACOS(CC) * THIRD
        DD=TWO*SQRT(AA * THIRD)
        STR(1,NN)=DD*COS(ANGP)
        STR(2,NN)=DD*COS(ANGP+FTPI)
        STR(3,NN)=DD*COS(ANGP+TTPI)
C
        VALDP(1,NN) = STR(1,NN)-PR
        VALDP(2,NN) = STR(2,NN)-PR
        VALDP(3,NN) = STR(3,NN)-PR
C renforcement de precision en compression simple----
        IF(ABS(STR(3,NN))>ABS(STR(1,NN))
     &     .AND.AAA(NN)>NORMINF(NN)) THEN
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN)<NORMINF(NN)) THEN
          VALDP(1,NN) = SIG(1,NN)
          VALDP(2,NN) = SIG(2,NN)
          VALDP(3,NN) = SIG(3,NN)
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
          V(3,2,NN) = ZERO	  
        ELSEIF(XMAXV(NN)>TOL1(NN)) THEN
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
        XMAXINV = ONE/XMAXV(NN)
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(1,1,NN)**2+V(2,1,NN)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,3,NN)=B(1,LMAX,NN)*XMAXINV
          V(2,3,NN)=B(2,LMAX,NN)*XMAXINV
          V(3,3,NN)=B(3,LMAX,NN)*XMAXINV
          V(1,2,NN)=V(2,3,NN)*V(3,1,NN)-V(2,1,NN)*V(3,3,NN)
          V(2,2,NN)=V(3,3,NN)*V(1,1,NN)-V(3,1,NN)*V(1,3,NN)
          V(3,2,NN)=V(1,3,NN)*V(2,1,NN)-V(1,1,NN)*V(2,3,NN)
          VMAG=ONE/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSEIF(VMAG>TOL2(NN))THEN
          V(1,2,NN)=-V(2,1,NN)/VMAG
          V(2,2,NN)=V(1,1,NN)/VMAG
          V(3,2,NN)=ZERO
        ELSE
          V(1,2,NN)=ONE
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(3,1,NN)),ABS(A(3,2,NN)),ABS(A(3,3,NN)))
     &       <TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)= ZERO
          V(2,1,NN)= ZERO
          V(3,1,NN)= ONE  
          V(1,2,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,2,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,2,NN)= ZERO
C
        ELSEIF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,1,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,1,NN)= ZERO
          V(1,2,NN)=-A(3,LMAX,NN)*V(2,1,NN)
          V(2,2,NN)= A(3,LMAX,NN)*V(1,1,NN)
          V(3,2,NN)= A(1,LMAX,NN)*V(2,1,NN)-A(2,LMAX,NN)*V(1,1,NN)
          VMAG=ONE/SQRT(V(1,2,NN)**2+V(2,2,NN)**2+V(3,2,NN)**2)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSE
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
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
      IF (IRESP==1) THEN
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
      MDEMI = -HALF
      TTPI = ACOS(MDEMI)
      FTPI = TWO*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = TWO*SQRT(FLMIN)
      NINDEX3=0 
      C1 = HALF*SQRT(TWENTY7)      
      C2 = TWO*SQRT(THIRD)      
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
     &     - TWO*CS(4)*CS(5)*CS(6)
C
        SQ_AA = SQRT(AA)
        CC=-C1/MAX(EM10,SQ_AA)*BB/MAX(EM20,AA)
c        CC=-SQRT(TWENTY7/MAX(EM20,AA))*BB*HALF/MAX(EM20,AA)
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
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
        IF(ABS(STR(3,NN))>ABS(STR(1,NN))
     &     .AND.AAA(NN)>NORMINF(NN)) THEN
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN)<NORMINF(NN)) THEN
          VALDP(1,NN) = SIG(1,NN)
          VALDP(2,NN) = SIG(2,NN)
          VALDP(3,NN) = SIG(3,NN)
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
          V(3,2,NN) = ZERO	  
        ELSEIF(XMAXV(NN)>TOL1(NN)*TOL1(NN)) THEN
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
        XMAXINV = ONE/SQRT(XMAXV(NN))
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(1,1,NN)**2+V(2,1,NN)**2)
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,3,NN)=B(1,LMAX,NN)*XMAXINV
          V(2,3,NN)=B(2,LMAX,NN)*XMAXINV
          V(3,3,NN)=B(3,LMAX,NN)*XMAXINV
          V(1,2,NN)=V(2,3,NN)*V(3,1,NN)-V(2,1,NN)*V(3,3,NN)
          V(2,2,NN)=V(3,3,NN)*V(1,1,NN)-V(3,1,NN)*V(1,3,NN)
          V(3,2,NN)=V(1,3,NN)*V(2,1,NN)-V(1,1,NN)*V(2,3,NN)
	  C1 = V(1,2,NN)*V(1,2,NN)+V(2,2,NN)*V(2,2,NN)+
     &         V(3,2,NN)*V(3,2,NN)
          VMAG=ONE/SQRT(C1)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSEIF(VMAG>TOL2(NN))THEN
          V(1,2,NN)=-V(2,1,NN)/VMAG
          V(2,2,NN)=V(1,1,NN)/VMAG
          V(3,2,NN)=ZERO
        ELSE
          V(1,2,NN)=ONE
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
        IF(XMAG(1,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(2,NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(MAX(ABS(A(3,1,NN)),ABS(A(3,2,NN)),ABS(A(3,3,NN)))
     &       <TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)= ZERO
          V(2,1,NN)= ZERO
          V(3,1,NN)= ONE  
          V(1,2,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,2,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,2,NN)= ZERO
C
        ELSEIF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(1,1,NN)=-A(2,LMAX,NN)*XMAXINV
          V(2,1,NN)= A(1,LMAX,NN)*XMAXINV
          V(3,1,NN)= ZERO
          V(1,2,NN)=-A(3,LMAX,NN)*V(2,1,NN)
          V(2,2,NN)= A(3,LMAX,NN)*V(1,1,NN)
          V(3,2,NN)= A(1,LMAX,NN)*V(2,1,NN)-A(2,LMAX,NN)*V(1,1,NN)
	  C1 = V(1,2,NN)*V(1,2,NN)+V(2,2,NN)*V(2,2,NN)+
     &         V(3,2,NN)*V(3,2,NN)
          VMAG=ONE/SQRT(C1)
          V(1,2,NN)=V(1,2,NN)*VMAG
          V(2,2,NN)=V(2,2,NN)*VMAG
          V(3,2,NN)=V(3,2,NN)*VMAG
        ELSE
          V(1,1,NN) = ONE
          V(2,1,NN) = ZERO
          V(3,1,NN) = ZERO
          V(1,2,NN) = ZERO
          V(2,2,NN) = ONE
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
Chd|        SIGEPS124                     source/materials/mat/mat124/sigeps124.F
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
      MDEMI = -HALF
      TTPI = ACOS(MDEMI)
      FTPI = TWO*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = TWO*SQRT(FLMIN)
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
     &     - TWO*CS(4)*CS(5)*CS(6)     
C
        CC=-SQRT(TWENTY7/MAX(EM20,AA))*BB*HALF/MAX(EM20,AA)
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
        ANGP=ACOS(CC) * THIRD
        DD=TWO*SQRT(AA * THIRD)
        STR(NN,1)=DD*COS(ANGP)
        STR(NN,2)=DD*COS(ANGP+FTPI)
        STR(NN,3)=DD*COS(ANGP+TTPI)
C
        VAL(NN,1) = STR(NN,1)-PR
        VAL(NN,2) = STR(NN,2)-PR
        VAL(NN,3) = STR(NN,3)-PR
C renforcement de precision en compression----
        IF(ABS(STR(NN,3))>ABS(STR(NN,1))
     &     .AND.AAA(NN)>NORMINF(NN)) THEN
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
        IF(XMAG(NN,1)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN)<NORMINF(NN)) THEN
          VAL(NN,1) = SIG(NN,1)
          VAL(NN,2) = SIG(NN,2)
          VAL(NN,3) = SIG(NN,3)
          V(NN,1,1) = ONE
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = ONE
          V(NN,3,2) = ZERO

        ELSEIF(XMAXV(NN)>TOL1(NN)) THEN
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
        XMAXINV = ONE/XMAXV(NN)
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
        IF(XMAG(NN,1)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(NN,1,1)**2+V(NN,2,1)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,3)=B(NN,1,LMAX)*XMAXINV
          V(NN,2,3)=B(NN,2,LMAX)*XMAXINV
          V(NN,3,3)=B(NN,3,LMAX)*XMAXINV
          V(NN,1,2)=V(NN,2,3)*V(NN,3,1)-V(NN,2,1)*V(NN,3,3)
          V(NN,2,2)=V(NN,3,3)*V(NN,1,1)-V(NN,3,1)*V(NN,1,3)
          V(NN,3,2)=V(NN,1,3)*V(NN,2,1)-V(NN,1,1)*V(NN,2,3)
          VMAG=ONE/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSEIF(VMAG>TOL2(NN))THEN
          V(NN,1,2)=-V(NN,2,1)/VMAG
          V(NN,2,2)=V(NN,1,1)/VMAG
          V(NN,3,2)=ZERO
        ELSE
          V(NN,1,2)=ONE
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
        IF(XMAG(NN,1)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(NN,3,1)),ABS(A(NN,3,2)),ABS(A(NN,3,3)))
     &       <TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,1)= ZERO
          V(NN,2,1)= ZERO
          V(NN,3,1)= ONE
          V(NN,1,2)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,2)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,2)= ZERO
C
        ELSEIF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,1)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,1)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,1)= ZERO
          V(NN,1,2)=-A(NN,3,LMAX)*V(NN,2,1)
          V(NN,2,2)= A(NN,3,LMAX)*V(NN,1,1)
          V(NN,3,2)= A(NN,1,LMAX)*V(NN,2,1)-A(NN,2,LMAX)*V(NN,1,1)
          VMAG=ONE/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSE
          V(NN,1,1) = ONE
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = ONE
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
Chd|        SIGEPS124                     source/materials/mat/mat124/sigeps124.F
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
      MDEMI = -HALF
      TTPI = ACOS(MDEMI)
      FTPI = TWO*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS(1),CS(2),FLMIN)
      FLM = TWO*SQRT(FLMIN)
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
     &     - TWO*CS(4)*CS(5)*CS(6)
C
        CC=-SQRT(TWENTY7/MAX(EM20,AA))*BB*HALF/MAX(EM20,AA)
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
        ANGP=ACOS(CC) * THIRD
        DD=TWO*SQRT(AA * THIRD)
        STR(NN,1)=DD*COS(ANGP)
        STR(NN,2)=DD*COS(ANGP+FTPI)
        STR(NN,3)=DD*COS(ANGP+TTPI)
C
        VALDP(NN,1) = STR(NN,1)-PR
        VALDP(NN,2) = STR(NN,2)-PR
        VALDP(NN,3) = STR(NN,3)-PR
C renforcement de precision en compression simple----
        IF(ABS(STR(NN,3))>ABS(STR(NN,1))
     &     .AND.AAA(NN)>NORMINF(NN)) THEN
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
        IF(XMAG(NN,1)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN)<NORMINF(NN)) THEN
          VALDP(NN,1) = SIG(NN,1)
          VALDP(NN,2) = SIG(NN,2)
          VALDP(NN,3) = SIG(NN,3)
          V(NN,1,1) = ONE
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = ONE
          V(NN,3,2) = ZERO	  
        ELSEIF(XMAXV(NN)>TOL1(NN)) THEN
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
        XMAXINV = ONE/XMAXV(NN)
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
        IF(XMAG(NN,1)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(NN,1,1)**2+V(NN,2,1)**2)
        LMAX = LMAXV(NN)
        IF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,3)=B(NN,1,LMAX)*XMAXINV
          V(NN,2,3)=B(NN,2,LMAX)*XMAXINV
          V(NN,3,3)=B(NN,3,LMAX)*XMAXINV
          V(NN,1,2)=V(NN,2,3)*V(NN,3,1)-V(NN,2,1)*V(NN,3,3)
          V(NN,2,2)=V(NN,3,3)*V(NN,1,1)-V(NN,3,1)*V(NN,1,3)
          V(NN,3,2)=V(NN,1,3)*V(NN,2,1)-V(NN,1,1)*V(NN,2,3)
          VMAG=ONE/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSEIF(VMAG>TOL2(NN))THEN
          V(NN,1,2)=-V(NN,2,1)/VMAG
          V(NN,2,2)=V(NN,1,1)/VMAG
          V(NN,3,2)=ZERO
        ELSE
          V(NN,1,2)=ONE
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
        IF(XMAG(NN,1)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG(NN,2)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        IF(MAX(ABS(A(NN,3,1)),ABS(A(NN,3,2)),ABS(A(NN,3,3)))
     &       <TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,1)= ZERO
          V(NN,2,1)= ZERO
          V(NN,3,1)= ONE  
          V(NN,1,2)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,2)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,2)= ZERO
C
        ELSEIF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,1)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,1)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,1)= ZERO
          V(NN,1,2)=-A(NN,3,LMAX)*V(NN,2,1)
          V(NN,2,2)= A(NN,3,LMAX)*V(NN,1,1)
          V(NN,3,2)= A(NN,1,LMAX)*V(NN,2,1)-A(NN,2,LMAX)*V(NN,1,1)
          VMAG=ONE/SQRT(V(NN,1,2)**2+V(NN,2,2)**2+V(NN,3,2)**2)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSE
          V(NN,1,1) = ONE
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = ONE
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
Chd|        PRINC_U                       source/materials/mat/mat001/princ_u.F
Chd|        PRINC_U1                      source/materials/mat/mat001/princ_u1.F
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
      MDEMI = -HALF
      TTPI = ACOS(MDEMI)
      FTPI = TWO*TTPI
C precision minimum dependant du type REAL ou DOUBLE
      CALL FLOATMIN(CS1,CS2,FLMIN)
      FLM = TWO*SQRT(FLMIN)
      NINDEX3=0 
      C1 = HALF*SQRT(TWENTY7)      
      C2 = TWO*SQRT(THIRD)      
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
     &     - TWO*CS4*CS5*CS6
C
        CC=-C1/MAX(EM10,SQ_AA)*BB/MAX(EM20,AA)
c        CC=-SQRT(TWENTY7/MAX(EM20,AA))*BB*HALF/MAX(EM20,AA)
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
        ANGP=ACOS(CC) * THIRD
        DD=C2*SQ_AA

        COS_ANGP = COS(ANGP)
        SIN_ANGP = SIN(ANGP)
        STR1(NN)=DD*COS_ANGP
        STR2(NN)=DD*((-HALF*COS_ANGP) - (SIN_ANGP*SQRT3DMI))
        STR3(NN)=DD*((-HALF*COS_ANGP) + (SIN_ANGP*SQRT3DMI))

c       STR1(NN)=DD*COS(ANGP)
c       STR2(NN)=DD*COS(ANGP+FTPI)
c       STR3(NN)=DD*COS(ANGP+TTPI)
C
        VALDP1(NN) = STR1(NN)-PR
        VALDP2(NN) = STR2(NN)-PR
        VALDP3(NN) = STR3(NN)-PR
C renforcement de precision en compression simple----
        IF(ABS(STR3(NN))>ABS(STR1(NN))
     &     .AND.AAA(NN)>NORMINF(NN)) THEN
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
        IF(XMAG1(NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG2(NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
        IF(AAA(NN)<NORMINF(NN)) THEN
          VALDP1(NN) = SIG(NN,1)
          VALDP2(NN) = SIG(NN,2)
          VALDP3(NN) = SIG(NN,3)
          V(NN,1,1) = ONE
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = ONE
          V(NN,3,2) = ZERO        
        ELSEIF(XMAXV(NN)>TOL1(NN)*TOL1(NN)) THEN
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
        XMAXINV = ONE/SQRT(XMAXV(NN))
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
        IF(XMAG1(NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG2(NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        VMAG=SQRT(V(NN,1,1)**2+V(NN,2,1)**2)
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,3)=B(NN,1,LMAX)*XMAXINV
          V(NN,2,3)=B(NN,2,LMAX)*XMAXINV
          V(NN,3,3)=B(NN,3,LMAX)*XMAXINV
          V(NN,1,2)=V(NN,2,3)*V(NN,3,1)-V(NN,2,1)*V(NN,3,3)
          V(NN,2,2)=V(NN,3,3)*V(NN,1,1)-V(NN,3,1)*V(NN,1,3)
          V(NN,3,2)=V(NN,1,3)*V(NN,2,1)-V(NN,1,1)*V(NN,2,3)
          C1 = V(NN,1,2)*V(NN,1,2)+V(NN,2,2)*V(NN,2,2)+
     &         V(NN,3,2)*V(NN,3,2)
          VMAG=ONE/SQRT(C1)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSEIF(VMAG>TOL2(NN))THEN
          V(NN,1,2)=-V(NN,2,1)/VMAG
          V(NN,2,2)=V(NN,1,1)/VMAG
          V(NN,3,2)=ZERO
        ELSE
          V(NN,1,2)=ONE
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
        IF(XMAG1(NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 1
        ELSEIF(XMAG2(NN)==XMAXV(NN)) THEN
          LMAXV(NN) = 2
        ELSE
          LMAXV(NN) = 3
        ENDIF
C
        LMAX = LMAXV(NN)
        XMAXV(NN)=SQRT(XMAXV(NN))
        IF(MAX(ABS(A(NN,3,1)),ABS(A(NN,3,2)),ABS(A(NN,3,3)))
     &       <TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,1)= ZERO
          V(NN,2,1)= ZERO
          V(NN,3,1)= ONE  
          V(NN,1,2)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,2)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,2)= ZERO
C
        ELSEIF(XMAXV(NN)>TOL2(NN))THEN
          XMAXINV = ONE/XMAXV(NN)
          V(NN,1,1)=-A(NN,2,LMAX)*XMAXINV
          V(NN,2,1)= A(NN,1,LMAX)*XMAXINV
          V(NN,3,1)= ZERO
          V(NN,1,2)=-A(NN,3,LMAX)*V(NN,2,1)
          V(NN,2,2)= A(NN,3,LMAX)*V(NN,1,1)
          V(NN,3,2)= A(NN,1,LMAX)*V(NN,2,1)-A(NN,2,LMAX)*V(NN,1,1)
          C1 = V(NN,1,2)*V(NN,1,2)+V(NN,2,2)*V(NN,2,2)+
     &         V(NN,3,2)*V(NN,3,2)
          VMAG=ONE/SQRT(C1)
          V(NN,1,2)=V(NN,1,2)*VMAG
          V(NN,2,2)=V(NN,2,2)*VMAG
          V(NN,3,2)=V(NN,3,2)*VMAG
        ELSE
          VALDP1(NN) = SIG(NN,1)
          VALDP2(NN) = SIG(NN,2)
          VALDP3(NN) = SIG(NN,3)
          V(NN,1,1) = ONE
          V(NN,2,1) = ZERO
          V(NN,3,1) = ZERO
          V(NN,1,2) = ZERO
          V(NN,2,2) = ONE
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
 
