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
Chd|  BEMSOLVP                      source/fluid/bemsolvp.F       
Chd|-- called by -----------
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|-- calls ---------------
Chd|        GLBEMP                        source/fluid/bemsolvp.F       
Chd|        GLSINFP                       source/fluid/bemsolvp.F       
Chd|        INTGTG                        source/fluid/bemsolv.F        
Chd|        INTHTG                        source/fluid/bemsolv.F        
Chd|        PROGCONDP_C                   source/system/progcond_c.c    
Chd|        SPMD_FL_SUM                   source/mpi/generic/spmd_fl_sum.F
Chd|====================================================================
      SUBROUTINE BEMSOLVP(
     .   ELEM         , NORM , X          , NNRP       , CR_LOC_GLOB,
     .   CR_GLOB_LOC  , NNCP , CC_LOC_GLOB, CC_GLOB_LOC, NCEIP      ,
     .   CCEI_LOC_GLOB, NEP  , CE_LOC_GLOB, CE_GLOB_LOC, NREIP      ,
     .   CREI_LOC_GLOB, NNO  , NEL        , HBEM       , GBEM       ,
     .   Q            , PHI  , IFORM      , L_ASSEMB   , NPROW      ,
     .   NPCOL        , NBLOC, IPIV       , NERP       , ILVOUT     ,
     .   PHI_INF      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ELEM(3,*), NNRP, CR_LOC_GLOB(*), CR_GLOB_LOC(*), NNCP,
     .        CC_GLOB_LOC(*), CC_LOC_GLOB(*), NCEIP, CCEI_LOC_GLOB(*),
     .        NEP, CE_LOC_GLOB(*), CE_GLOB_LOC(*), NNO, NEL, IFORM,
     .        NREIP, CREI_LOC_GLOB(*), NPROW, NPCOL, NBLOC, IPIV(*),
     .        NERP, ILVOUT
      my_real
     .        NORM(3,*), X(3,*), HBEM(NNRP,*), GBEM(NNRP,*), Q(*),
     .        PHI(*), PHI_INF(*)
      LOGICAL L_ASSEMB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEL, N1, N2, N3, NL1, NL2, NL3, I, J, JN, IR, IC, JJ,
     .        ICTXT, DESC_H(9), DESC_G(9), DESC_Q(9), DESC_P(9),
     .        INFO, IBID, NC1, NC2, NC3, OFFNR(3), OFFNC(3)
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X0, Y0, Z0, D2,
     .        NRX, NRY, NRZ, AREA2, RVAL(3), LSUM(NNO), LSUMT(NNO)
#if defined(MPI) && defined(MYREAL8) && !defined(WITHOUT_LINALG)
C---------------------------------------------------
C 0- Initialisations pour utilisation SCALAPACK
C---------------------------------------------------
C Initialisation de la process grid
      CALL SL_INIT(ICTXT, NPROW, NPCOL)
C Descripteur pour HBEM
      DESC_H(1)=1
      DESC_H(2)=ICTXT
      DESC_H(3)=NNO
      DESC_H(4)=NNO
      DESC_H(5)=NBLOC
      DESC_H(6)=NBLOC
      DESC_H(7)=0
      DESC_H(8)=0
      DESC_H(9)=MAX(1,NNRP)
C Descripteur pour GBEM
      DESC_G(1)=1
      DESC_G(2)=ICTXT
      DESC_G(3)=NNO
      DESC_G(4)=NEL
      DESC_G(5)=NBLOC
      DESC_G(6)=NBLOC
      DESC_G(7)=0
      DESC_G(8)=0
      DESC_G(9)=MAX(1,NNRP)
C Descripteur pour Q
      DESC_Q(1)=1
      DESC_Q(2)=ICTXT
      DESC_Q(3)=NEL
      DESC_Q(4)=1
      DESC_Q(5)=NBLOC
      DESC_Q(6)=NBLOC
      DESC_Q(7)=0
      DESC_Q(8)=0
      DESC_Q(9)=MAX(1,NERP)
C Descripteur pour PHI
      DESC_P(1)=1
      DESC_P(2)=ICTXT
      DESC_P(3)=NNO
      DESC_P(4)=1
      DESC_P(5)=NBLOC
      DESC_P(6)=NBLOC
      DESC_P(7)=0
      DESC_P(8)=0
      DESC_P(9)=MAX(1,NNRP)
C---------------------------------------------------
C 1- Assemblage
C---------------------------------------------------
      IF (L_ASSEMB) THEN
         IF (ILVOUT>=1.AND.ISPMD==0) WRITE(ISTDO,'(A)') ' ** BEMSOLV : ASSEMBLY OF INTEGRAL OPERATORS'
         IF (IFORM==1) THEN
C Assemblage de H
            DO I=1,NNRP
               DO J=1,NNCP
                  HBEM(I,J)=ZERO
               ENDDO
               DO J=1,NEP
                  GBEM(I,J)=ZERO
               ENDDO
            ENDDO
            DO I=1,NCEIP
               IF (ILVOUT==2) 
     .            CALL PROGCONDP_C(I,NCEIP+NEP, ISPMD+1, IBID)
               IEL=CCEI_LOC_GLOB(I)
               N1=ELEM(1,IEL)
               N2=ELEM(2,IEL)
               N3=ELEM(3,IEL)
               NL1=CC_GLOB_LOC(N1)
               NL2=CC_GLOB_LOC(N2)
               NL3=CC_GLOB_LOC(N3)
               X1=X(1,N1)
               X2=X(1,N2)
               X3=X(1,N3)
               Y1=X(2,N1)
               Y2=X(2,N2)
               Y3=X(2,N3)
               Z1=X(3,N1)
               Z2=X(3,N2)
               Z3=X(3,N3)
               X0=THIRD*(X1+X2+X3)
               Y0=THIRD*(Y1+Y2+Y3)    
               Z0=THIRD*(Z1+Z2+Z3)
               D2=MIN((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2,
     .                (X0-X2)**2+(Y0-Y2)**2+(Z0-Z2)**2,
     .                (X0-X3)**2+(Y0-Y3)**2+(Z0-Z3)**2)
               NRX=NORM(1,IEL)
               NRY=NORM(2,IEL)
               NRZ=NORM(3,IEL)
               AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
               DO J=1,NNRP
                  JN=CR_LOC_GLOB(J)
                  CALL INTHTG(JN,  X1 , Y1,  Z1,   X2,
     .                        Y2,  Z2,  X3,  Y3,   Z3,
     .                        X0,  Y0,  Z0,  D2,   AREA2,
     .                        NRX, NRY, NRZ, RVAL, N1,
     .                        N2,  N3,  X  )
                  IF (NL1/=0) HBEM(J,NL1)=HBEM(J,NL1)+RVAL(1)
                  IF (NL2/=0) HBEM(J,NL2)=HBEM(J,NL2)+RVAL(2)
                  IF (NL3/=0) HBEM(J,NL3)=HBEM(J,NL3)+RVAL(3)
               ENDDO
            ENDDO
C Assemblage de G
            DO I=1,NEP
               IF (ILVOUT==2) 
     .            CALL PROGCONDP_C(NCEIP+I,NCEIP+NEP, ISPMD+1, IBID)
               IEL=CE_LOC_GLOB(I)
               N1=ELEM(1,IEL)
               N2=ELEM(2,IEL)
               N3=ELEM(3,IEL)
               NL1=CC_GLOB_LOC(N1)
               NL2=CC_GLOB_LOC(N2)
               NL3=CC_GLOB_LOC(N3)
               X1=X(1,N1)
               X2=X(1,N2)
               X3=X(1,N3)
               Y1=X(2,N1)
               Y2=X(2,N2)
               Y3=X(2,N3)
               Z1=X(3,N1)
               Z2=X(3,N2)
               Z3=X(3,N3)
               X0=THIRD*(X1+X2+X3)
               Y0=THIRD*(Y1+Y2+Y3)    
               Z0=THIRD*(Z1+Z2+Z3)
               D2=MIN((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2,
     .                (X0-X2)**2+(Y0-Y2)**2+(Z0-Z2)**2,
     .                (X0-X3)**2+(Y0-Y3)**2+(Z0-Z3)**2)
               NRX=NORM(1,IEL)
               NRY=NORM(2,IEL)
               NRZ=NORM(3,IEL)
               AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
               DO J=1,NNRP
                  JN=CR_LOC_GLOB(J)
                  CALL INTGTG(JN,  X1 , Y1,  Z1,   X2,
     .                        Y2,  Z2,  X3,  Y3,   Z3,
     .                        X0,  Y0,  Z0,  D2,   AREA2,
     .                        NRX, NRY, NRZ, RVAL, N1,
     .                        N2,  N3,  X  )
                  GBEM(J,I)=GBEM(J,I)+RVAL(1)
               ENDDO
            ENDDO
C Somme des lignes pour le terme diagonal de H
            DO I=1,NNO
               LSUM(I)=ZERO
            ENDDO
            DO I=1,NNCP
               DO J=1,NNRP
                  JJ=CR_LOC_GLOB(J)
                  LSUM(JJ)=LSUM(JJ)+HBEM(J,I)
               ENDDO
            ENDDO
            CALL SPMD_FL_SUM(LSUM, NNO, LSUMT)
            DO I=1,NNO
               IR=CR_GLOB_LOC(I)
               IC=CC_GLOB_LOC(I)
               IF (IR>0.AND.IC>0) HBEM(IR,IC)=-LSUMT(I)
            ENDDO
C
         ELSEIF (IFORM==2) THEN
            DO I=1,NNRP
               DO J=1,NNCP
                  HBEM(I,J)=ZERO
               ENDDO
               DO J=1,NEP
                  GBEM(I,J)=ZERO
               ENDDO
            ENDDO
            DO I=1,NREIP
               IF (ILVOUT==2) 
     .            CALL PROGCONDP_C(I,NREIP, ISPMD+1, IBID)
               IEL=CREI_LOC_GLOB(I)
               N1=ELEM(1,IEL)
               N2=ELEM(2,IEL)
               N3=ELEM(3,IEL)
C
               NL1=CR_GLOB_LOC(N1)
               NL2=CR_GLOB_LOC(N2)
               NL3=CR_GLOB_LOC(N3)
               OFFNR(1)=MIN(1,NL1)
               OFFNR(2)=MIN(1,NL2)
               OFFNR(3)=MIN(1,NL3)
               NL1=MAX(1,NL1)
               NL2=MAX(1,NL2)
               NL3=MAX(1,NL3)
C
               NC1=CC_GLOB_LOC(N1)
               NC2=CC_GLOB_LOC(N2)
               NC3=CC_GLOB_LOC(N3)
               OFFNC(1)=MIN(1,NC1)
               OFFNC(2)=MIN(1,NC2)
               OFFNC(3)=MIN(1,NC3)
               NC1=MAX(1,NC1)
               NC2=MAX(1,NC2)
               NC3=MAX(1,NC3)
C
               X1=X(1,N1)
               X2=X(1,N2)
               X3=X(1,N3)
               Y1=X(2,N1)
               Y2=X(2,N2)
               Y3=X(2,N3)
               Z1=X(3,N1)
               Z2=X(3,N2)
               Z3=X(3,N3)
               X0=THIRD*(X1+X2+X3)
               Y0=THIRD*(Y1+Y2+Y3)    
               Z0=THIRD*(Z1+Z2+Z3)
               NRX=NORM(1,IEL)
               NRY=NORM(2,IEL)
               NRZ=NORM(3,IEL)
               AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
               CALL GLBEMP(X1,    Y1,    Z1,    X2,   Y2,
     .                     Z2,    X3,    Y3,    Z3,   X0,
     .                     Y0,    Z0,    NORM,  HBEM, GBEM,
     .                     ELEM,  X,     NL1,   NL2,  NL3, 
     .                     NNRP,  NEL,   AREA2, CC_GLOB_LOC,
     .                     CE_GLOB_LOC,  NC1, NC2,  NC3  ,
     .                     OFFNR, OFFNC)
            ENDDO
         ENDIF
C Somme des lignes de G pour remplacer la derniere colonne de H
         DO I=1,NNO
            LSUM(I)=ZERO
         ENDDO
         DO I=1,NEP
            DO J=1,NNRP
               JJ=CR_LOC_GLOB(J)
               LSUM(JJ)=LSUM(JJ)+GBEM(J,I)
            ENDDO
         ENDDO
         CALL SPMD_FL_SUM(LSUM, NNO, LSUMT)
         IC=CC_GLOB_LOC(NNO)
         IF (IC>0) THEN
            DO I=1,NNO
               IR=CR_GLOB_LOC(I)
               IF (IR>0) HBEM(IR,IC)=-LSUMT(I)
            ENDDO
         ENDIF
C Factorisation de HBEM
         CALL PDGETRF(NNO,    NNO,  HBEM, 1, 1, 
     .                DESC_H, IPIV, INFO)
      ENDIF            
C---------------------------------------------------
C 2- Resolution
C---------------------------------------------------
      IF (ILVOUT>=1.AND.ISPMD==0) THEN
         WRITE(ISTDO,*)
         WRITE(ISTDO,'(A)') ' ** BEMSOLV : PARALLEL LINEAR SYSTEM SOLUTION'
      ENDIF
      CALL PDGEMV('N', NNO, NEL,    ONE,   GBEM,
     .            1,   1,   DESC_G, Q,    1,
     .            1,   DESC_Q, 1,   ZERO, PHI,
     .            1,   1,   DESC_P, 1)
C
      IF (IFORM==1) THEN
         IC=CC_GLOB_LOC(1)
         IF (IC>0) THEN
            DO I=1,NNRP
               IR=CR_LOC_GLOB(I)
               PHI(I)=PHI(I)-PHI_INF(IR)
            ENDDO
         ENDIF
      ELSEIF (IFORM==2) THEN
         IC=CC_GLOB_LOC(1)
         IF (IC>0) THEN
            DO I=1,NREIP
               IEL=CREI_LOC_GLOB(I)
               N1=ELEM(1,IEL)
               N2=ELEM(2,IEL)
               N3=ELEM(3,IEL)
C
               NL1=CR_GLOB_LOC(N1)
               NL2=CR_GLOB_LOC(N2)
               NL3=CR_GLOB_LOC(N3)
               OFFNR(1)=MIN(1,NL1)
               OFFNR(2)=MIN(1,NL2)
               OFFNR(3)=MIN(1,NL3)
               NL1=MAX(1,NL1)
               NL2=MAX(1,NL2)
               NL3=MAX(1,NL3)
C
               NRX=NORM(1,IEL)
               NRY=NORM(2,IEL)
               NRZ=NORM(3,IEL)
               AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
               CALL GLSINFP(N1,      N2,  N3,  AREA2, PHI,
     .                      PHI_INF, NL1, NL2, NL3  , OFFNR)
            ENDDO
         ENDIF
      ENDIF
C
      CALL PDGETRS('N', NNO,    1,    HBEM, 1,
     .             1,   DESC_H, IPIV, PHI,  1,
     .             1,   DESC_P, INFO)
C
      CALL BLACS_GRIDEXIT(ICTXT)
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  GLBEMP                        source/fluid/bemsolvp.F       
Chd|-- called by -----------
Chd|        BEMSOLVP                      source/fluid/bemsolvp.F       
Chd|-- calls ---------------
Chd|        INTANL                        source/fluid/bemsolv.F        
Chd|====================================================================
      SUBROUTINE GLBEMP(X1    , Y1   , Z1    , X2  , Y2   ,
     .                  Z2    , X3   , Y3    , Z3  , X0   ,
     .                  Y0    , Z0   , TELNOR, HBEM, GBEM ,
     .                  TBEMTG, X    , N1    , N2  , N3   ,
     .                  NNRP  , NEL  , JAC   , CC_GLOB_LOC,
     .                  CE_GLOB_LOC  , NC1   , NC2 , NC3  ,
     .                  OFFNR , OFFNC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TBEMTG(3,*), N1, N2, N3, NEL, NNRP, CC_GLOB_LOC(*),
     .        CE_GLOB_LOC(*), NC1, NC2, NC3, OFFNR(*), OFFNC(*)
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X0, Y0, Z0,
     .        TELNOR(3,*), X(3,*), JAC, HBEM(NNRP,*), GBEM(NNRP,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, JBID, NBID, IDEG, IAD2, IP, IEL, NN1, NN2, NN3,
     .        NL1, NL2, NL3, IELL, OFFNR2(3), OFFEL
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
            IELL=CE_GLOB_LOC(IEL)
            OFFEL=MIN(1,IELL)
            IELL=MAX(1,IELL)
C
            NN1=TBEMTG(1,IEL)
            NN2=TBEMTG(2,IEL)
            NN3=TBEMTG(3,IEL)
            NL1=CC_GLOB_LOC(NN1)
            NL2=CC_GLOB_LOC(NN2)
            NL3=CC_GLOB_LOC(NN3)
            OFFNR2(1)=MIN(1,NL1)
            OFFNR2(2)=MIN(1,NL2)
            OFFNR2(3)=MIN(1,NL3)
            NL1=MAX(1,NL1)
            NL2=MAX(1,NL2)
            NL3=MAX(1,NL3)
C
            XX1=X(1,NN1)
            YY1=X(2,NN1)
            ZZ1=X(3,NN1)
            XX2=X(1,NN2)
            YY2=X(2,NN2)
            ZZ2=X(3,NN2)
            XX3=X(1,NN3)
            YY3=X(2,NN3)
            ZZ3=X(3,NN3)
            NRX=TELNOR(1,IEL)
            NRY=TELNOR(2,IEL)
            NRZ=TELNOR(3,IEL)
            CALL INTANL(XX1 , YY1 , ZZ1, XX2, YY2,
     .                  ZZ2 , XX3 , YY3, ZZ3, XP ,
     .                  YP  , ZP  , NRX, NRY, NRZ,
     .                  RVLH, RVLG)
C Matrice H
            HBEM(N1,NL1)=HBEM(N1,NL1)
     .                  +OFFNR(1)*OFFNR2(1)*W*VAL1*RVLH(1)*JAC
            HBEM(N1,NL2)=HBEM(N1,NL2)
     .                  +OFFNR(1)*OFFNR2(2)*W*VAL1*RVLH(2)*JAC
            HBEM(N1,NL3)=HBEM(N1,NL3)
     .                  +OFFNR(1)*OFFNR2(3)*W*VAL1*RVLH(3)*JAC
            HBEM(N2,NL1)=HBEM(N2,NL1)
     .                  +OFFNR(2)*OFFNR2(1)*W*VAL2*RVLH(1)*JAC
            HBEM(N2,NL2)=HBEM(N2,NL2)
     .                  +OFFNR(2)*OFFNR2(2)*W*VAL2*RVLH(2)*JAC
            HBEM(N2,NL3)=HBEM(N2,NL3)
     .                  +OFFNR(2)*OFFNR2(3)*W*VAL2*RVLH(3)*JAC
            HBEM(N3,NL1)=HBEM(N3,NL1)
     .                  +OFFNR(3)*OFFNR2(1)*W*VAL3*RVLH(1)*JAC
            HBEM(N3,NL2)=HBEM(N3,NL2)
     .                  +OFFNR(3)*OFFNR2(2)*W*VAL3*RVLH(2)*JAC
            HBEM(N3,NL3)=HBEM(N3,NL3)
     .                  +OFFNR(3)*OFFNR2(3)*W*VAL3*RVLH(3)*JAC
            CP=CP-RVLH(1)-RVLH(2)-RVLH(3)
C Matrice G
            GBEM(N1,IELL)=GBEM(N1,IELL)+OFFNR(1)*OFFEL*W*VAL1*RVLG*JAC
            GBEM(N2,IELL)=GBEM(N2,IELL)+OFFNR(2)*OFFEL*W*VAL2*RVLG*JAC
            GBEM(N3,IELL)=GBEM(N3,IELL)+OFFNR(3)*OFFEL*W*VAL3*RVLG*JAC
         ENDDO
         HBEM(N1,NC1)=HBEM(N1,NC1)+OFFNR(1)*OFFNC(1)*W*CP*VAL1*VAL1*JAC
         HBEM(N1,NC2)=HBEM(N1,NC2)+OFFNR(1)*OFFNC(2)*W*CP*VAL1*VAL2*JAC
         HBEM(N1,NC3)=HBEM(N1,NC3)+OFFNR(1)*OFFNC(3)*W*CP*VAL1*VAL3*JAC
         HBEM(N2,NC1)=HBEM(N2,NC1)+OFFNR(2)*OFFNC(1)*W*CP*VAL2*VAL1*JAC
         HBEM(N2,NC2)=HBEM(N2,NC2)+OFFNR(2)*OFFNC(2)*W*CP*VAL2*VAL2*JAC
         HBEM(N2,NC3)=HBEM(N2,NC3)+OFFNR(2)*OFFNC(3)*W*CP*VAL2*VAL3*JAC
         HBEM(N3,NC1)=HBEM(N3,NC1)+OFFNR(3)*OFFNC(1)*W*CP*VAL3*VAL1*JAC
         HBEM(N3,NC2)=HBEM(N3,NC2)+OFFNR(3)*OFFNC(2)*W*CP*VAL3*VAL2*JAC
         HBEM(N3,NC3)=HBEM(N3,NC3)+OFFNR(3)*OFFNC(3)*W*CP*VAL3*VAL3*JAC
      ENDDO          
C
      RETURN
      END
Chd|====================================================================
Chd|  GLSINFP                       source/fluid/bemsolvp.F       
Chd|-- called by -----------
Chd|        BEMSOLVP                      source/fluid/bemsolvp.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GLSINFP(N1     , N2,  N3,  JAC, S    ,
     .                   PHI_INF, NL1, NL2, NL3, OFFNR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1, N2, N3, NL1, NL2, NL3, OFFNR(*)
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
         S(NL1)=S(NL1)-OFFNR(1)*W*VAL1*PHI_INFP*JAC
         S(NL2)=S(NL2)-OFFNR(2)*W*VAL2*PHI_INFP*JAC
         S(NL3)=S(NL3)-OFFNR(3)*W*VAL3*PHI_INFP*JAC
*         IF (NL1>0) S(NL1)=S(NL1)-W*VAL1*PHI_INFP*JAC
*         IF (NL2>0) S(NL2)=S(NL2)-W*VAL2*PHI_INFP*JAC
*         IF (NL3>0) S(NL3)=S(NL3)-W*VAL3*PHI_INFP*JAC
      ENDDO
C
      RETURN
      END
