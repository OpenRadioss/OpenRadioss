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
Chd|  JACOBIEW                      source/materials/tools/matrix.F
Chd|-- called by -----------
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
C ...        EV(IZ,IZ)=1.0
        EV(IZ,IZ)=ZERO
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
Chd|  DREH                          source/materials/tools/matrix.F
Chd|-- called by -----------
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
Chd|  VALPVEC                       source/materials/tools/matrix.F
Chd|-- called by -----------
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
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
        AA = MAX(AAA(NN),NORMINF(NN),EM20)
C
        BB=CS(1)*CS(5)**2 + CS(2)*CS(6)**2
     &     + CS(3)*CS(4)**2 - CS(1)*CS(2)*CS(3)
     &     - TWO*CS(4)*CS(5)*CS(6)     
C
        CC=-SQRT(TWENTY7/AA)*BB*HALF/AA
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
C renforcement de precision en compression
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
C #include      "vectorize.inc"
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
C #include      "vectorize.inc"
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
C #include      "vectorize.inc"
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
Chd|  VALPVECDP                     source/materials/tools/matrix.F
Chd|-- called by -----------
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
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
        AA = MAX(AAA(NN),NORMINF(NN),EM20)
C
        BB=CS(1)*CS(5)**2 + CS(2)*CS(6)**2
     &     + CS(3)*CS(4)**2 - CS(1)*CS(2)*CS(3)
     &     - TWO*CS(4)*CS(5)*CS(6)
C
        CC=-SQRT(TWENTY7/AA)*BB*HALF/AA
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
C #include      "vectorize.inc"
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
C #include      "vectorize.inc"
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
C #include      "vectorize.inc"
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
