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
Chd|  BEMSOLV                       source/fluid/bemsolv.F        
Chd|-- called by -----------
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        GLBEM                         source/fluid/bemsolv.F        
Chd|        GLSINF                        source/fluid/bemsolv.F        
Chd|        INTGTG                        source/fluid/bemsolv.F        
Chd|        INTHTG                        source/fluid/bemsolv.F        
Chd|        PROGBAR_C                     ../common_source/sortie/progbar_c.c
Chd|====================================================================
      SUBROUTINE BEMSOLV(L_ASSEMB, ILVOUT, IFORM, NN    , NEL    ,
     .                   HBEM    , GBEM  , IBUF , TBEMTG, TELNOR ,
     .                   X       , Q     , PHI  , IPIV  , PHI_INF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILVOUT, IFORM, NN, NEL, IBUF(*), TBEMTG(3,*), IPIV(*)
      my_real
     .        HBEM(NN,*), GBEM(NN,*), TELNOR(3,*), X(3,*), Q(*), PHI(*),
     .        PHI_INF(*)
      LOGICAL L_ASSEMB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IN, JN, IQ, N1, N2, N3, NN1, NN2, NN3, NNJ, INFO, IEL
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X0, Y0, Z0, D2,
     .        NRX, NRY, NRZ, AREA2, RVAL(3), RSUM, S(NN)
C---------------------------------------------------
C 1- Assemblage
C---------------------------------------------------
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
      IF (L_ASSEMB) THEN
         IF (ILVOUT>=1) WRITE(ISTDO,'(A)')
     .   ' ** BEMSOLV : ASSEMBLY OF INTEGRAL OPERATORS'
         IF (IFORM==1) THEN
C Collocation BEM - Elements lineaires en potentiel, constants en flux
            DO IN=1,NN
               DO JN=1,NN
                  HBEM(IN,JN)=ZERO
               ENDDO
               DO IQ=1,NEL
                  GBEM(IN,IQ)=ZERO
               ENDDO
               GBEM(IN,NEL+1)=ZERO
            ENDDO
            DO IEL=1,NEL
               IF (ILVOUT==2) CALL PROGBAR_C(IEL,NEL)
               N1=TBEMTG(1,IEL)
               N2=TBEMTG(2,IEL)
               N3=TBEMTG(3,IEL)
               NN1=IBUF(N1)
               NN2=IBUF(N2)
               NN3=IBUF(N3)
               X1=X(1,NN1)
               X2=X(1,NN2)
               X3=X(1,NN3)
               Y1=X(2,NN1)
               Y2=X(2,NN2)
               Y3=X(2,NN3)
               Z1=X(3,NN1)
               Z2=X(3,NN2)
               Z3=X(3,NN3)
               X0=THIRD*(X1+X2+X3)
               Y0=THIRD*(Y1+Y2+Y3)
               Z0=THIRD*(Z1+Z2+Z3)
               D2=MIN((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2,
     .                (X0-X2)**2+(Y0-Y2)**2+(Z0-Z2)**2,
     .                (X0-X3)**2+(Y0-Y3)**2+(Z0-Z3)**2)
               NRX=TELNOR(1,IEL)
               NRY=TELNOR(2,IEL)
               NRZ=TELNOR(3,IEL)
               AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
               DO JN=1,NN
                  NNJ=IBUF(JN)
                  CALL INTHTG(NNJ, X1 , Y1,  Z1,   X2,
     .                        Y2,  Z2,  X3,  Y3,   Z3,
     .                        X0,  Y0,  Z0,  D2,   AREA2,
     .                        NRX, NRY, NRZ, RVAL, NN1,
     .                        NN2, NN3, X  )
                  HBEM(JN,N1)=HBEM(JN,N1)+RVAL(1)
                  HBEM(JN,N2)=HBEM(JN,N2)+RVAL(2)
                  HBEM(JN,N3)=HBEM(JN,N3)+RVAL(3)
                  CALL INTGTG(NNJ, X1 , Y1,  Z1,   X2,
     .                        Y2,  Z2,  X3,  Y3,   Z3,
     .                        X0,  Y0,  Z0,  D2,   AREA2,
     .                        NRX, NRY, NRZ, RVAL, NN1,
     .                        NN2, NN3, X  )
                  GBEM(JN,IEL)=GBEM(JN,IEL)+RVAL(1)
                  GBEM(JN,NEL+1)=GBEM(JN,NEL+1)+RVAL(1)
               ENDDO
            ENDDO
            DO IN=1,NN
               RSUM=ZERO
               DO JN=1,NN
                  IF (IN/=JN) RSUM=RSUM+HBEM(IN,JN)
               ENDDO
               HBEM(IN,IN)=-RSUM
            ENDDO
         ELSEIF (IFORM==2) THEN
C Galerkin BEM - Elements lineaires en potentiel, constants en flux
            DO IN=1,NN
               DO JN=1,NN
                  HBEM(IN,JN)=ZERO
               ENDDO
               DO IQ=1,NEL
                  GBEM(IN,IQ)=ZERO
               ENDDO
               GBEM(IN,NEL+1)=ZERO
            ENDDO
            DO IEL=1,NEL
               IF (ILVOUT==2) CALL PROGBAR_C(IEL,NEL)
               N1=TBEMTG(1,IEL)
               N2=TBEMTG(2,IEL)
               N3=TBEMTG(3,IEL)
               NN1=IBUF(N1)
               NN2=IBUF(N2)
               NN3=IBUF(N3)
               X1=X(1,NN1)
               X2=X(1,NN2)
               X3=X(1,NN3)
               Y1=X(2,NN1)
               Y2=X(2,NN2)
               Y3=X(2,NN3)
               Z1=X(3,NN1)
               Z2=X(3,NN2)
               Z3=X(3,NN3)
               X0=THIRD*(X1+X2+X3)
               Y0=THIRD*(Y1+Y2+Y3)
               Z0=THIRD*(Z1+Z2+Z3)
               NRX=TELNOR(1,IEL)
               NRY=TELNOR(2,IEL)
               NRZ=TELNOR(3,IEL)
               AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
               CALL GLBEM(X1    , Y1  , Z1    , X2   , Y2  ,
     .                    Z2    , X3  , Y3    , Z3   , X0  ,
     .                    Y0    , Z0  , TELNOR, HBEM , GBEM,
     .                    TBEMTG, IBUF, X     , N1   , N2  ,
     .                    N3    , IEL , NN    , AREA2, NEL )
            ENDDO
         ENDIF
C         
         DO IN=1,NN
            HBEM(IN,NN)=-GBEM(IN,NEL+1)
         ENDDO
C
         CALL DGETRF(NN,   NN, HBEM, NN, IPIV,
     .               INFO)
      ENDIF
C---------------------------------------------------
C 2- Resolution
C---------------------------------------------------
      IF (ILVOUT>=1) WRITE(ISTDO,'(A)')
     .' ** BEMSOLV : LINEAR SYSTEM SOLUTION'
      CALL DGEMV('N',   NN, NEL+1, ONE,   GBEM,
     .           NN,    Q,  1,     ZERO, S,
     .           1    )
C
      IF (IFORM==1) THEN
         DO IN=1,NN
            S(IN)=S(IN)-PHI_INF(IN)
         ENDDO
      ELSEIF (IFORM==2) THEN
         DO IEL=1,NEL
            N1=TBEMTG(1,IEL)
            N2=TBEMTG(2,IEL)
            N3=TBEMTG(3,IEL)
            NRX=TELNOR(1,IEL)
            NRY=TELNOR(2,IEL)
            NRZ=TELNOR(3,IEL)
            AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
            CALL GLSINF(N1,      N2, N3, AREA2, S,
     .                  PHI_INF)
         ENDDO
      ENDIF
C
      CALL DGETRS('N', NN, 1,    HBEM, NN, IPIV,
     .            S,   NN, INFO)
C
      DO IN=1,NN
         PHI(IN)=S(IN)
      ENDDO
#else
       CALL ARRET(5)
#endif
      RETURN
      END
Chd|====================================================================
Chd|  GLSINF                        source/fluid/bemsolv.F        
Chd|-- called by -----------
Chd|        BEMSOLV                       source/fluid/bemsolv.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GLSINF(N1     , N2, N3, JAC, S,
     .                  PHI_INF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1, N2, N3
      my_real
     .        JAC, S(*), PHI_INF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, IAD2, IP
      my_real
     .        PG(50), WPG(25), W, KSIP, ETAP, VAL1, VAL2, VAL3,
     .        PHI_INFP
C
      DATA PG  /.33333333,.33333333,
     .          .33333333,.33333333,
     .          .60000000,.20000000,
     .          .20000000,.60000000,
     .          .20000000,.20000000,
     .          .33333333,.33333333,
     .          .79742699,.10128651,
     .          .10128651,.79742699,
     .          .10128651,.10128651,
     .          .05971587,.47014206,
     .          .47014206,.05971587,
     .          .47014206,.47014206,
     .          .06513010,.06513010,
     .          .86973979,.06513010,
     .          .06513010,.86973979,
     .          .31286550,.04869031,
     .          .63844419,.31286550,
     .          .04869031,.63844419,
     .          .63844419,.04869031,
     .          .31286550,.63844419,
     .          .04869031,.31286550,
     .          .26034597,.26034597,
     .          .47930807,.26034597,
     .          .26034597,.47930807,
     .          .33333333,.33333333/
      DATA WPG  /1.00000000,
     .           -.56250000,.52083333,
     .            .52083333,.52083333,
     .            .22500000,.12593918,
     .            .12593918,.12593918,
     .            .13239415,.13239415,
     .            .13239415,
     .            .05334724,.05334724,
     .            .05334724,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.17561526,
     .            .17561526,.17561526,
     .           -.14957004/
C
      NPG=13
      IAD=13
C
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         KSIP=PG(IAD2)
         ETAP=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=ONE-KSIP-ETAP
         VAL2=KSIP
         VAL3=ETAP
         PHI_INFP=PHI_INF(N1)*VAL1+PHI_INF(N2)*VAL2+PHI_INF(N3)*VAL3
         S(N1)=S(N1)-W*VAL1*PHI_INFP*JAC
         S(N2)=S(N2)-W*VAL2*PHI_INFP*JAC
         S(N3)=S(N3)-W*VAL3*PHI_INFP*JAC
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INTHTG                        source/fluid/bemsolv.F        
Chd|-- called by -----------
Chd|        BEMSOLV                       source/fluid/bemsolv.F        
Chd|        BEMSOLVP                      source/fluid/bemsolvp.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTHTG(NNJ, X1 , Y1,  Z1,   X2,
     .                  Y2,  Z2,  X3,  Y3,   Z3,
     .                  X0,  Y0,  Z0,  D2,   JAC,
     .                  NRX, NRY, NRZ, RVAL, NN1,
     .                  NN2, NN3, X  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNJ, NN1, NN2, NN3
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        X0, Y0, Z0, D2, JAC, NRX, NRY, NRZ, RVAL(*), X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, IAD2, IP, IHG
      my_real
     .        PG(50), WPG(25), XS, YS, ZS, R2,
     .        VAL1, VAL2, VAL3, W, XG, YG, ZG, VALPHI,
     .        ETA1, ETA2
C
      DATA PG  /.33333333,.33333333,
     .          .33333333,.33333333,
     .          .60000000,.20000000,
     .          .20000000,.60000000,
     .          .20000000,.20000000,
     .          .33333333,.33333333,
     .          .79742699,.10128651,
     .          .10128651,.79742699,
     .          .10128651,.10128651,
     .          .05971587,.47014206,
     .          .47014206,.05971587,
     .          .47014206,.47014206,
     .          .06513010,.06513010,
     .          .86973979,.06513010,
     .          .06513010,.86973979,
     .          .31286550,.04869031,
     .          .63844419,.31286550,
     .          .04869031,.63844419,
     .          .63844419,.04869031,
     .          .31286550,.63844419,
     .          .04869031,.31286550,
     .          .26034597,.26034597,
     .          .47930807,.26034597,
     .          .26034597,.47930807,
     .          .33333333,.33333333/
      DATA WPG  /1.00000000,
     .           -.56250000,.52083333,
     .            .52083333,.52083333,
     .            .22500000,.12593918,
     .            .12593918,.12593918,
     .            .13239415,.13239415,
     .            .13239415,
     .            .05334724,.05334724,
     .            .05334724,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.17561526,
     .            .17561526,.17561526,
     .           -.14957004/
C
C DISTANCE A LA SOURCE
      XS=X(1,NNJ)
      YS=X(2,NNJ)
      ZS=X(3,NNJ)
      R2=(X0-XS)**2+(Y0-YS)**2+(Z0-ZS)**2
C NOMBRE DE POINTS DE GAUSS
      IF (R2>HUNDRED*D2) THEN
         NPG=1
         IAD=1
      ELSEIF (R2>TWENTY5*D2) THEN
         NPG=4
         IAD=2
      ELSEIF (R2>FOUR*D2) THEN
         NPG=7
         IAD=6
      ELSE
         NPG=13
         IAD=13
      ENDIF
      IF (NN1==NNJ) NPG=-1
      IF (NN2==NNJ) NPG=-1
      IF (NN3==NNJ) NPG=-1
C INTEGRATION
      RVAL(1)=ZERO
      RVAL(2)=ZERO
      RVAL(3)=ZERO
      IF (NPG<0) RETURN
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         ETA1=PG(IAD2)
         ETA2=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=ONE-ETA1-ETA2
         VAL2=ETA1
         VAL3=ETA2
         XG=VAL1*X1+VAL2*X2+VAL3*X3
         YG=VAL1*Y1+VAL2*Y2+VAL3*Y3
         ZG=VAL1*Z1+VAL2*Z2+VAL3*Z3
         R2=(XG-XS)**2+(YG-YS)**2+(ZG-ZS)**2
         IF(R2>EM20)THEN
           VALPHI=-FOURTH/PI/(R2**THREE_HALF)
     .            *(NRX*(XG-XS)+NRY*(YG-YS)+NRZ*(ZG-ZS))
           RVAL(1)=RVAL(1)+HALF*W*VAL1*VALPHI
           RVAL(2)=RVAL(2)+HALF*W*VAL2*VALPHI
           RVAL(3)=RVAL(3)+HALF*W*VAL3*VALPHI
         ENDIF
C Remarque : la normale (NRX,NRY,NRZ) n'est pas normalisee
C         il faudrait la diviser par JAC et comme on doit multiplier
C         par JAC a la ligne d'en dessous...
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INTGTG                        source/fluid/bemsolv.F        
Chd|-- called by -----------
Chd|        BEMSOLV                       source/fluid/bemsolv.F        
Chd|        BEMSOLVP                      source/fluid/bemsolvp.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTGTG(NNJ, X1 , Y1,  Z1,   X2,
     .                  Y2,  Z2,  X3,  Y3,   Z3,
     .                  X0,  Y0,  Z0,  D2,   JAC,
     .                  NRX, NRY, NRZ, RVAL, NN1,
     .                  NN2, NN3, X  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNJ, NN1, NN2, NN3
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        X0, Y0, Z0, D2, JAC, NRX, NRY, NRZ, RVAL(*),
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, IAD2, IP, NNS, IHG
      my_real
     .        PG(50), WPG(25), XS, YS, ZS, R2,
     .        VAL, VAL1, VAL2, VAL3, W, XG, YG, ZG, VALPHI,
     .        ETA1, ETA2, PG4(32), WPG4(16), JJAC, KSI1, KSI2
C
      DATA PG  /.33333333,.33333333,
     .          .33333333,.33333333,
     .          .60000000,.20000000,
     .          .20000000,.60000000,
     .          .20000000,.20000000,
     .          .33333333,.33333333,
     .          .79742699,.10128651,
     .          .10128651,.79742699,
     .          .10128651,.10128651,
     .          .05971587,.47014206,
     .          .47014206,.05971587,
     .          .47014206,.47014206,
     .          .06513010,.06513010,
     .          .86973979,.06513010,
     .          .06513010,.86973979,
     .          .31286550,.04869031,
     .          .63844419,.31286550,
     .          .04869031,.63844419,
     .          .63844419,.04869031,
     .          .31286550,.63844419,
     .          .04869031,.31286550,
     .          .26034597,.26034597,
     .          .47930807,.26034597,
     .          .26034597,.47930807,
     .          .33333333,.33333333/
      DATA WPG  /1.00000000,
     .           -.56250000,.52083333,
     .            .52083333,.52083333,
     .            .22500000,.12593918,
     .            .12593918,.12593918,
     .            .13239415,.13239415,
     .            .13239415,
     .            .05334724,.05334724,
     .            .05334724,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.17561526,
     .            .17561526,.17561526,
     .           -.14957004/
      DATA PG4 /-.861136311594053,-.861136311594053,
     .          -.861136311594053,-.339981034584856,
     .          -.861136311594053, .339981034584856,
     .          -.861136311594053, .861136311594053,
     .          -.339981034584856,-.861136311594053,
     .          -.339981034584856,-.339981034584856,
     .          -.339981034584856, .339981034584856,
     .          -.339981034584856, .861136311594053,
     .           .339981034584856,-.861136311594053,
     .           .339981034584856,-.339981034584856,
     .           .339981034584856, .339981034584856,
     .           .339981034584856, .861136311594053,
     .           .861136311594053,-.861136311594053,
     .           .861136311594053,-.339981034584856,
     .           .861136311594053, .339981034584856,
     .           .861136311594053, .861136311594053/
C
      WPG4(1) =.652145154862546 * .652145154862546
      WPG4(2) =.652145154862546 * .347854845137454
      WPG4(3) =.652145154862546 * .347854845137454
      WPG4(4) =.652145154862546 * .652145154862546
      WPG4(5) =.347854845137454 * .652145154862546
      WPG4(6) =.347854845137454 * .347854845137454
      WPG4(7) =.347854845137454 * .347854845137454
      WPG4(8) =.347854845137454 * .652145154862546
      WPG4(9) =.347854845137454 * .652145154862546
      WPG4(10)=.347854845137454 * .347854845137454
      WPG4(11)=.347854845137454 * .347854845137454
      WPG4(12)=.347854845137454 * .652145154862546
      WPG4(13)=.652145154862546 * .652145154862546
      WPG4(14)=.652145154862546 * .347854845137454
      WPG4(15)=.652145154862546 * .347854845137454
      WPG4(16)=.652145154862546 * .652145154862546
C DISTANCE A LA SOURCE
      XS=X(1,NNJ)
      YS=X(2,NNJ)
      ZS=X(3,NNJ)
      R2=(X0-XS)**2+(Y0-YS)**2+(Z0-ZS)**2
C NOMBRE DE POINTS DE GAUSS
      IF (R2>HUNDRED*D2) THEN
         NPG=1
         IAD=1
      ELSEIF (R2>TWENTY5*D2) THEN
         NPG=4
         IAD=2
      ELSEIF (R2>FOUR*D2) THEN
         NPG=7
         IAD=6
      ELSE
         NPG=13
         IAD=13
      ENDIF
C INTEGRATION
      RVAL(1)=ZERO
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         ETA1=PG(IAD2)
         ETA2=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=ONE-ETA1-ETA2
         VAL2=ETA1
         VAL3=ETA2
         XG=VAL1*X1+VAL2*X2+VAL3*X3
         YG=VAL1*Y1+VAL2*Y2+VAL3*Y3
         ZG=VAL1*Z1+VAL2*Z2+VAL3*Z3
         R2=(XG-XS)**2+(YG-YS)**2+(ZG-ZS)**2
         IF(R2>EM20)THEN
           VALPHI=FOURTH/PI/SQRT(R2)
           RVAL(1)=RVAL(1)+HALF*W*VALPHI*JAC
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  GLBEM                         source/fluid/bemsolv.F        
Chd|-- called by -----------
Chd|        BEMSOLV                       source/fluid/bemsolv.F        
Chd|-- calls ---------------
Chd|        INTANL                        source/fluid/bemsolv.F        
Chd|====================================================================
      SUBROUTINE GLBEM(X1    , Y1  , Z1    , X2  , Y2  ,
     .                 Z2    , X3  , Y3    , Z3  , X0  ,
     .                 Y0    , Z0  , TELNOR, HBEM, GBEM,
     .                 TBEMTG, IBUF, X     , N1  , N2  ,
     .                 N3    , IELS, NN    , JAC , NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TBEMTG(3,*), IBUF(*), N1, N2, N3, IELS, NN, NEL
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X0, Y0, Z0,
     .        TELNOR(3,*), X(3,*), JAC, HBEM(NN,*), GBEM(NN,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, JBID, NBID, IDEG, IAD2, IP, IEL, NN1, NN2, NN3
      my_real
     .        PG(50), WPG(25), W, KSIP, ETAP, VAL1, VAL2, VAL3, XP,
     .        YP, ZP, CP, XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .        XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6, XX0, YY0,
     .        ZZ0, NRX, NRY, NRZ, D2, RVL(6), RVLH(3), RVLG
     .
C
      DATA PG  /.33333333,.33333333,
     .          .33333333,.33333333,
     .          .60000000,.20000000,
     .          .20000000,.60000000,
     .          .20000000,.20000000,
     .          .33333333,.33333333,
     .          .79742699,.10128651,
     .          .10128651,.79742699,
     .          .10128651,.10128651,
     .          .05971587,.47014206,
     .          .47014206,.05971587,
     .          .47014206,.47014206,
     .          .06513010,.06513010,
     .          .86973979,.06513010,
     .          .06513010,.86973979,
     .          .31286550,.04869031,
     .          .63844419,.31286550,
     .          .04869031,.63844419,
     .          .63844419,.04869031,
     .          .31286550,.63844419,
     .          .04869031,.31286550,
     .          .26034597,.26034597,
     .          .47930807,.26034597,
     .          .26034597,.47930807,
     .          .33333333,.33333333/
      DATA WPG  /1.00000000,
     .           -.56250000,.52083333,
     .            .52083333,.52083333,
     .            .22500000,.12593918,
     .            .12593918,.12593918,
     .            .13239415,.13239415,
     .            .13239415,
     .            .05334724,.05334724,
     .            .05334724,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.17561526,
     .            .17561526,.17561526,
     .           -.14957004/
C
      NPG=13
      IAD=13
C
C
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         KSIP=PG(IAD2)
         ETAP=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=ONE-KSIP-ETAP
         VAL2=KSIP
         VAL3=ETAP
         XP=VAL1*X1+VAL2*X2+VAL3*X3
         YP=VAL1*Y1+VAL2*Y2+VAL3*Y3
         ZP=VAL1*Z1+VAL2*Z2+VAL3*Z3
         CP=ZERO
         DO IEL=1,NEL
            NN1=TBEMTG(1,IEL)
            NN2=TBEMTG(2,IEL)
            NN3=TBEMTG(3,IEL)
            XX1=X(1,IBUF(NN1))
            YY1=X(2,IBUF(NN1))
            ZZ1=X(3,IBUF(NN1))
            XX2=X(1,IBUF(NN2))
            YY2=X(2,IBUF(NN2))
            ZZ2=X(3,IBUF(NN2))
            XX3=X(1,IBUF(NN3))
            YY3=X(2,IBUF(NN3))
            ZZ3=X(3,IBUF(NN3))
            NRX=TELNOR(1,IEL)
            NRY=TELNOR(2,IEL)
            NRZ=TELNOR(3,IEL)
C Matrice H
            CALL INTANL(XX1 , YY1 , ZZ1, XX2, YY2,
     .                  ZZ2 , XX3 , YY3, ZZ3, XP ,
     .                  YP  , ZP  , NRX, NRY, NRZ,
     .                  RVLH, RVLG)
            HBEM(N1,NN1)=HBEM(N1,NN1)+W*VAL1*RVLH(1)*JAC
            HBEM(N1,NN2)=HBEM(N1,NN2)+W*VAL1*RVLH(2)*JAC
            HBEM(N1,NN3)=HBEM(N1,NN3)+W*VAL1*RVLH(3)*JAC
            HBEM(N2,NN1)=HBEM(N2,NN1)+W*VAL2*RVLH(1)*JAC
            HBEM(N2,NN2)=HBEM(N2,NN2)+W*VAL2*RVLH(2)*JAC
            HBEM(N2,NN3)=HBEM(N2,NN3)+W*VAL2*RVLH(3)*JAC
            HBEM(N3,NN1)=HBEM(N3,NN1)+W*VAL3*RVLH(1)*JAC
            HBEM(N3,NN2)=HBEM(N3,NN2)+W*VAL3*RVLH(2)*JAC
            HBEM(N3,NN3)=HBEM(N3,NN3)+W*VAL3*RVLH(3)*JAC
            CP=CP-RVLH(1)-RVLH(2)-RVLH(3)
C Matrice G
            GBEM(N1,IEL)=GBEM(N1,IEL)+W*VAL1*RVLG*JAC
            GBEM(N2,IEL)=GBEM(N2,IEL)+W*VAL2*RVLG*JAC
            GBEM(N3,IEL)=GBEM(N3,IEL)+W*VAL3*RVLG*JAC
            GBEM(N1,NEL+1)=GBEM(N1,NEL+1)+W*VAL1*RVLG*JAC
            GBEM(N2,NEL+1)=GBEM(N2,NEL+1)+W*VAL2*RVLG*JAC
            GBEM(N3,NEL+1)=GBEM(N3,NEL+1)+W*VAL3*RVLG*JAC
         ENDDO
         HBEM(N1,N1)=HBEM(N1,N1)+W*CP*VAL1*VAL1*JAC
         HBEM(N1,N2)=HBEM(N1,N2)+W*CP*VAL1*VAL2*JAC
         HBEM(N1,N3)=HBEM(N1,N3)+W*CP*VAL1*VAL3*JAC
         HBEM(N2,N1)=HBEM(N2,N1)+W*CP*VAL2*VAL1*JAC
         HBEM(N2,N2)=HBEM(N2,N2)+W*CP*VAL2*VAL2*JAC
         HBEM(N2,N3)=HBEM(N2,N3)+W*CP*VAL2*VAL3*JAC
         HBEM(N3,N1)=HBEM(N3,N1)+W*CP*VAL3*VAL1*JAC
         HBEM(N3,N2)=HBEM(N3,N2)+W*CP*VAL3*VAL2*JAC
         HBEM(N3,N3)=HBEM(N3,N3)+W*CP*VAL3*VAL3*JAC
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INTANL                        source/fluid/bemsolv.F        
Chd|-- called by -----------
Chd|        GLBEM                         source/fluid/bemsolv.F        
Chd|        GLBEMP                        source/fluid/bemsolvp.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTANL(X1  , Y1  , Z1 , X2 , Y2 ,
     .                  Z2  , X3  , Y3 , Z3 , XS ,
     .                  YS  , ZS  , NRX, NRY, NRZ,
     .                  RVLH, RVLG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, XS, YS, ZS,
     .        NRX, NRY, NRZ, RVLH(*), RVLG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .        VX1, VY1, VZ1, VX2, VY2, VZ2, S1, S12, S2, NR1, NR2,
     .        X0, Y0, Z0, KSI(4), ETA(4), DKSI(3), DETA(3), R(4),
     .        XLS, YLS, ZLS, XC, YC, S(3), V, FLN, ARG, I1, I2, I3,
     .        SS11, SS12, SS22, DELTA, ALPHA1, BETA1, GAMA1, ALPHA2,
     .        BETA2, GAMA2, NX, NY, NZ, AREA2, D2, L12, L22, L32,
     .        RX, RY, RZ, RR, LM2
C
      AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
      NX=NRX/AREA2
      NY=NRY/AREA2
      NZ=NRZ/AREA2
C
      X0=THIRD*(X1+X2+X3)
      Y0=THIRD*(Y1+Y2+Y3)
      Z0=THIRD*(Z1+Z2+Z3)
C
C SIMPLIFICATION SI SOURCE LOIN DE L'ELEMENT
      D2=(X0-XS)**2+(Y0-YS)**2+(Z0-ZS)**2
      L12=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
      L22=(X3-X2)**2+(Y3-Y2)**2+(Z3-Z2)**2
      L32=(X1-X3)**2+(Y1-Y3)**2+(Z1-Z3)**2
      LM2=MAX(L12,L22,L32)
C
C COORDONNEES LOCALES
      VX1=X2-X1
      VY1=Y2-Y1
      VZ1=Z2-Z1
      VX2=X3-X1
      VY2=Y3-Y1
      VZ2=Z3-Z1
C
      S1=VX1*VX1+VY1*VY1+VZ1*VZ1
      S12=VX1*VX2+VY1*VY2+VZ1*VZ2
      NR1=SQRT(S1)
C     
      VX2=VX2-S12/S1*VX1
      VY2=VY2-S12/S1*VY1
      VZ2=VZ2-S12/S1*VZ1
C
      S2=VX2*VX2+VY2*VY2+VZ2*VZ2
      NR2=SQRT(S2)
      VX1=VX1/NR1
      VY1=VY1/NR1
      VZ1=VZ1/NR1
      VX2=VX2/NR2
      VY2=VY2/NR2
      VZ2=VZ2/NR2
C
      XLS=(XS-X0)*VX1+(YS-Y0)*VY1+(ZS-Z0)*VZ1
      YLS=(XS-X0)*VX2+(YS-Y0)*VY2+(ZS-Z0)*VZ2
      ZLS=(XS-X0)*NX+(YS-Y0)*NY+(ZS-Z0)*NZ
      KSI(1)=(X1-X0)*VX1+(Y1-Y0)*VY1+(Z1-Z0)*VZ1
      ETA(1)=(X1-X0)*VX2+(Y1-Y0)*VY2+(Z1-Z0)*VZ2
      KSI(2)=(X2-X0)*VX1+(Y2-Y0)*VY1+(Z2-Z0)*VZ1
      ETA(2)=(X2-X0)*VX2+(Y2-Y0)*VY2+(Z2-Z0)*VZ2
      KSI(3)=(X3-X0)*VX1+(Y3-Y0)*VY1+(Z3-Z0)*VZ1
      ETA(3)=(X3-X0)*VX2+(Y3-Y0)*VY2+(Z3-Z0)*VZ2
      KSI(4)=KSI(1)
      ETA(4)=ETA(1)
      DKSI(1)=KSI(2)-KSI(1)
      DKSI(2)=KSI(3)-KSI(2)
      DKSI(3)=KSI(1)-KSI(3)
      DETA(1)=ETA(2)-ETA(1)
      DETA(2)=ETA(3)-ETA(2)
      DETA(3)=ETA(1)-ETA(3)
      R(1)=SQRT((XS-X1)**2+(YS-Y1)**2+(ZS-Z1)**2)
      R(2)=SQRT((XS-X2)**2+(YS-Y2)**2+(ZS-Z2)**2)
      R(3)=SQRT((XS-X3)**2+(YS-Y3)**2+(ZS-Z3)**2)
      S(1)=SQRT(L12)
      S(2)=SQRT(L22)
      S(3)=SQRT(L32)
      R(4)=R(1)
C
      I1=ZERO
      I2=ZERO
      I3=ZERO
C INTEGRALE DOUBLE COUCHE
      IF (ZLS/=ZERO) THEN
         DO I=1,3
            I1=I1
     .   +ATAN((DETA(I)*((XLS-KSI(I))**2+ZLS**2)
     .         -DKSI(I)*(XLS-KSI(I))*(YLS-ETA(I)))/(R(I)*ZLS*DKSI(I)))
     .   -ATAN((DETA(I)*((XLS-KSI(I+1))**2+ZLS**2)
     .     -DKSI(I)*(XLS-KSI(I+1))*(YLS-ETA(I+1)))/(R(I+1)*ZLS*DKSI(I)))
         ENDDO
      ENDIF
C
C INTEGRALE SIMPLE COUCHE
      RVLG=ZERO
      DO I=1,3
         XC=XLS-KSI(I)
         YC=YLS-ETA(I)
         V=XC*DETA(I)/S(I)-YC*DKSI(I)/S(I)
         ARG=(R(I)+R(I+1)-S(I))/(R(I)+R(I+1)+S(I))
         IF (ARG>ZERO) THEN
            FLN=-LOG(ARG)
            RVLG=RVLG+V*FLN
         ENDIF
      ENDDO
      RVLG=RVLG-ZLS*I1
      RVLG=-FOURTH/PI*RVLG
C
C MOMENTS D'ORDRE 1 POUR DOUBLE COUCHE
      I2=XLS*I1
      I3=YLS*I1
      DO I=1,3
         XC=XLS-KSI(I)
         YC=YLS-ETA(I)
         ARG=(R(I)+R(I+1)-S(I))/(R(I)+R(I+1)+S(I))
         IF (ARG>ZERO) THEN
            FLN=-LOG(ARG)
            I2=I2+ZLS*FLN*DETA(I)/S(I)
            I3=I3-ZLS*FLN*DKSI(I)/S(I)
         ENDIF
      ENDDO
C
      SS11=
     .(KSI(2)-KSI(1))*(KSI(2)-KSI(1))+(ETA(2)-ETA(1))*(ETA(2)-ETA(1))
      SS22=
     .(KSI(3)-KSI(1))*(KSI(3)-KSI(1))+(ETA(3)-ETA(1))*(ETA(3)-ETA(1))
      SS12=
     .(KSI(2)-KSI(1))*(KSI(3)-KSI(1))+(ETA(2)-ETA(1))*(ETA(3)-ETA(1))
      DELTA=SS11*SS22-SS12*SS12
      ALPHA1=(SS22*(KSI(2)-KSI(1))-SS12*(KSI(3)-KSI(1)))/DELTA
      BETA1=(SS22*(ETA(2)-ETA(1))-SS12*(ETA(3)-ETA(1)))/DELTA
      GAMA1=(SS12*(KSI(1)*(KSI(3)-KSI(1))+ETA(1)*(ETA(3)-ETA(1)))
     .      -SS22*(KSI(1)*(KSI(2)-KSI(1))+ETA(1)*(ETA(2)-ETA(1))))
     .      /DELTA
      ALPHA2=(SS11*(KSI(3)-KSI(1))-SS12*(KSI(2)-KSI(1)))/DELTA
      BETA2=(SS11*(ETA(3)-ETA(1))-SS12*(ETA(2)-ETA(1)))/DELTA
      GAMA2=(SS12*(KSI(1)*(KSI(2)-KSI(1))+ETA(1)*(ETA(2)-ETA(1)))
     .      -SS11*(KSI(1)*(KSI(3)-KSI(1))+ETA(1)*(ETA(3)-ETA(1))))
     .      /DELTA
      RVLH(1)=(1.D0-GAMA1-GAMA2)*I1-(ALPHA1+ALPHA2)*I2
     .       -(BETA1+BETA2)*I3
      RVLH(2)=ALPHA1*I2+BETA1*I3+GAMA1*I1
      RVLH(3)=ALPHA2*I2+BETA2*I3+GAMA2*I1
      RVLH(1)=-RVLH(1)*FOURTH/PI
      RVLH(2)=-RVLH(2)*FOURTH/PI
      RVLH(3)=-RVLH(3)*FOURTH/PI
C
      RETURN
      END
