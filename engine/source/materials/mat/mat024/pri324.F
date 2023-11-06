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
Chd|  PRI324                        source/materials/mat/mat024/pri324.F
Chd|-- called by -----------
Chd|        DAMA24                        source/materials/mat/mat024/dama24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRI324(SIG,EPSTOT,EPS,VEC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real
     .     SIG(*), EPSTOT(*), EPS(*), VEC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IPERM(3), I, L, LMAX
        my_real
     .     CS(6), STR(3), A(3,3), V(3,3), B(3,3), XMAG(3), PR, AA, BB,
     .     CC, ANGP, DD, FTPI, TTPI, STRMAX, TOL1, TOL2, XMAX, VMAG, S11,
     .     S21, S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21,
     .     A22, A23, A31, A32, A33
C-----------------------------------------------
        DATA FTPI,TTPI / 4.188790205, 2.094395102 /
        DATA IPERM/2,3,1/
C=======================================================================
C       DEVIATEUR PRINCIPAL DE CONTRAINTE
C . . . . . . . . . . . . . . . . . . .
        DO I=1,6
          VEC(I)=ZERO
          CS(I)=SIG(I)
        ENDDO
        VEC(1)=1
        VEC(5)=1
C
        PR = -(CS(1)+CS(2)+CS(3)) * THIRD
        CS(1)=CS(1) + PR
        CS(2)=CS(2) + PR
        CS(3)=CS(3) + PR
C
        AA = CS(4)**2 + CS(5)**2 + CS(6)**2 - CS(1)*CS(2) - CS(2)*CS(3)
     &     - CS(1)*CS(3)

        IF (AA < EM20) RETURN
C
        BB = CS(1)*CS(5)**2 + CS(2)*CS(6)**2 + CS(3)*CS(4)**2
     &     - CS(1)*CS(2)*CS(3) - TWO*CS(4)*CS(5)*CS(6)
C
        CC=-SQRT(TWENTY7/AA)*BB*HALF/AA
        CC= MIN(CC,ONE)
        CC= MAX(CC,-ONE)
        ANGP=ACOS(CC) * THIRD
        DD=TWO*SQRT(AA*THIRD)
        STR(1)=DD*COS(ANGP)
        STR(2)=DD*COS(ANGP+FTPI)
        STR(3)=DD*COS(ANGP+TTPI)
C . . . . . . . . . . .
C      VECTEURS PROPRES
C . . . . . . . . . . .
        STRMAX= MAX(ABS(STR(1)),ABS(STR(3)))

        TOL1= MAX(EM20,SIXEM4*STRMAX**2)
        TOL2= TWOEM4*STRMAX

        A(1,1)=CS(1)-STR(1)
        A(2,2)=CS(2)-STR(1)
        A(3,3)=CS(3)-STR(1)
        A(1,2)=CS(4)
        A(2,1)=CS(4)
        A(2,3)=CS(5)
        A(3,2)=CS(5)
        A(1,3)=CS(6)
        A(3,1)=CS(6)
C
        DO L=1,3
          B(1,L) = A(2,L)*A(3,IPERM(L))-A(3,L)*A(2,IPERM(L))
          B(2,L) = A(3,L)*A(1,IPERM(L))-A(1,L)*A(3,IPERM(L))
          B(3,L) = A(1,L)*A(2,IPERM(L))-A(2,L)*A(1,IPERM(L))
          XMAG(L)= SQRT(B(1,L)**2+B(2,L)**2+B(3,L)**2)
        ENDDO

        XMAX=ZERO
        DO L=1,3
          IF (XMAG(L) > XMAX) THEN
            XMAX=XMAG(L)
            LMAX=L
          ENDIF
        ENDDO
C
        IF (XMAX > TOL1) THEN
          V(1,1)=B(1,LMAX)/XMAX
          V(2,1)=B(2,LMAX)/XMAX
          V(3,1)=B(3,LMAX)/XMAX
          A(1,1)=CS(1)-STR(3)
          A(2,2)=CS(2)-STR(3)
          A(3,3)=CS(3)-STR(3)
          A(1,2)=CS(4)
          A(2,1)=CS(4)
          A(2,3)=CS(5)
          A(3,2)=CS(5)
          A(1,3)=CS(6)
          A(3,1)=CS(6)
C
          DO L=1,3
            B(1,L)=A(2,L)*V(3,1)-A(3,L)*V(2,1)
            B(2,L)=A(3,L)*V(1,1)-A(1,L)*V(3,1)
            B(3,L)=A(1,L)*V(2,1)-A(2,L)*V(1,1)
            XMAG(L)=SQRT(B(1,L)**2+B(2,L)**2+B(3,L)**2)
          ENDDO
          XMAX=ZERO
          DO L=1,3
            IF(XMAG(L) > XMAX)THEN
              XMAX=XMAG(L)
              LMAX=L
            ENDIF
          ENDDO
C
          IF (XMAX > TOL2) THEN
            V(1,3)= B(1,LMAX)/XMAX
            V(2,3)= B(2,LMAX)/XMAX
            V(3,3)= B(3,LMAX)/XMAX
            V(1,2)= V(2,3)*V(3,1)-V(2,1)*V(3,3)
            V(2,2)= V(3,3)*V(1,1)-V(3,1)*V(1,3)
            V(3,2)= V(1,3)*V(2,1)-V(1,1)*V(2,3)
            VMAG  = SQRT(V(1,2)**2 + V(2,2)**2 + V(3,2)**2)
            V(1,2)= V(1,2)/VMAG
            V(2,2)= V(2,2)/VMAG
            V(3,2)= V(3,2)/VMAG
          ELSE
            VMAG  = SQRT(V(1,1)**2 + V(2,1)**2 + V(3,1)**2)
            IF (VMAG > TOL2/STRMAX) THEN
              V(1,2)=-V(2,1)/VMAG
              V(2,2)= V(1,1)/VMAG
              V(3,2)= ZERO
            ELSE
              V(1,2)=ONE
              V(2,2)=ZERO
              V(3,2)=ZERO
            ENDIF
          ENDIF
        ELSE
C . . . . . . . . . . . . .
C         SOLUTION DOUBLE
C . . . . . . . . . . . . .
          DO L=1,3
            XMAG(L) = SQRT(A(1,L)**2 + A(2,L)**2)
          ENDDO
          XMAX = ZERO
          DO L=1,3
            IF(XMAG(L) > XMAX)THEN
              LMAX=L
              XMAX=XMAG(L)
            ENDIF
          ENDDO
C
          IF(MAX(ABS(A(3,1)),ABS(A(3,2)),ABS(A(3,3))) < TOL2)THEN
            V(1,1) = ZERO
            V(2,1) = ZERO
            V(3,1) = ONE
            V(1,2) = -A(2,LMAX)/XMAX
            V(2,2) =  A(1,LMAX)/XMAX
            V(3,2) =  ZERO
C
          ELSEIF(XMAX > TOL2)THEN
            V(1,1) = -A(2,LMAX)/XMAX
            V(2,1) =  A(1,LMAX)/XMAX
            V(3,1) =  ZERO
            V(1,2) = -A(3,LMAX)*V(2,1)
            V(2,2) =  A(3,LMAX)*V(1,1)
            V(3,2) =  A(1,LMAX)*V(2,1)-A(2,LMAX)*V(1,1)
            VMAG = SQRT(V(1,2)**2 + V(2,2)**2 + V(3,2)**2)
            V(1,2)=V(1,2)/VMAG
            V(2,2)=V(2,2)/VMAG
            V(3,2)=V(3,2)/VMAG
          ELSE
            V(1,1) = ONE
            V(2,1) = ZERO
            V(3,1) = ZERO
            V(1,2) = ZERO
            V(2,2) = ONE
            V(3,2) = ZERO
          ENDIF
        ENDIF
C
        VEC(1)=V(1,1)
        VEC(2)=V(2,1)
        VEC(3)=V(3,1)
        VEC(4)=V(1,2)
        VEC(5)=V(2,2)
        VEC(6)=V(3,2)
C . . . . . . . . . . . .
C     ROTATION EPS EPSTOT
C . . . . . . . . . . . .
        S11=VEC(1)
        S21=VEC(2)
        S31=VEC(3)
        S12=VEC(4)
        S22=VEC(5)
        S32=VEC(6)
        S13=S21*S32-S31*S22
        S23=S31*S12-S11*S32
  5     S33=S11*S22-S21*S12
C
        A11=SIG(1)*S11+SIG(4)*S21+SIG(6)*S31
        A12=SIG(1)*S12+SIG(4)*S22+SIG(6)*S32
        A13=SIG(1)*S13+SIG(4)*S23+SIG(6)*S33
        A21=SIG(4)*S11+SIG(2)*S21+SIG(5)*S31
        A22=SIG(4)*S12+SIG(2)*S22+SIG(5)*S32
        A23=SIG(4)*S13+SIG(2)*S23+SIG(5)*S33
        A31=SIG(6)*S11+SIG(5)*S21+SIG(3)*S31
        A32=SIG(6)*S12+SIG(5)*S22+SIG(3)*S32
        A33=SIG(6)*S13+SIG(5)*S23+SIG(3)*S33
        SIG(4)=S11*A12+S21*A22+S31*A32
        SIG(5)=S12*A13+S22*A23+S32*A33
        SIG(6)=S11*A13+S21*A23+S31*A33
C
        EPS(4)=HALF*EPS(4)
        EPS(5)=HALF*EPS(5)
        EPS(6)=HALF*EPS(6)
        A11=EPS(1)*S11+EPS(4)*S21+EPS(6)*S31
        A12=EPS(1)*S12+EPS(4)*S22+EPS(6)*S32
        A13=EPS(1)*S13+EPS(4)*S23+EPS(6)*S33
        A21=EPS(4)*S11+EPS(2)*S21+EPS(5)*S31
        A22=EPS(4)*S12+EPS(2)*S22+EPS(5)*S32
        A23=EPS(4)*S13+EPS(2)*S23+EPS(5)*S33
        A31=EPS(6)*S11+EPS(5)*S21+EPS(3)*S31
        A32=EPS(6)*S12+EPS(5)*S22+EPS(3)*S32
        A33=EPS(6)*S13+EPS(5)*S23+EPS(3)*S33
        EPS(1)=S11*A11+S21*A21+S31*A31
        EPS(2)=S12*A12+S22*A22+S32*A32
        EPS(3)=S13*A13+S23*A23+S33*A33
        EPSTOT(4)=HALF*EPSTOT(4)
        EPSTOT(5)=HALF*EPSTOT(5)
        EPSTOT(6)=HALF*EPSTOT(6)
        A11=EPSTOT(1)*S11+EPSTOT(4)*S21+EPSTOT(6)*S31
        A12=EPSTOT(1)*S12+EPSTOT(4)*S22+EPSTOT(6)*S32
        A13=EPSTOT(1)*S13+EPSTOT(4)*S23+EPSTOT(6)*S33
        A21=EPSTOT(4)*S11+EPSTOT(2)*S21+EPSTOT(5)*S31
        A22=EPSTOT(4)*S12+EPSTOT(2)*S22+EPSTOT(5)*S32
        A23=EPSTOT(4)*S13+EPSTOT(2)*S23+EPSTOT(5)*S33
        A31=EPSTOT(6)*S11+EPSTOT(5)*S21+EPSTOT(3)*S31
        A32=EPSTOT(6)*S12+EPSTOT(5)*S22+EPSTOT(3)*S32
        A33=EPSTOT(6)*S13+EPSTOT(5)*S23+EPSTOT(3)*S33
        EPSTOT(1)=S11*A11+S21*A21+S31*A31
        EPSTOT(2)=S12*A12+S22*A22+S32*A32
        EPSTOT(3)=S13*A13+S23*A23+S33*A33
c-----------
        RETURN
      END
