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
C--------------------------------------------------------
C     Pseudo-inverse d'une matrice symetrique 3,3
C          * Ker(IA)==Ker(A)
C          * sur l'orthogonal de Ker(A) : IA est l'inverse de A.
C--------------------------------------------------------
Chd|====================================================================
Chd|  PINVERS                       source/elements/sph/pinvers.F 
Chd|-- called by -----------
Chd|        SPCOMPL                       source/elements/sph/spcompl.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PINVERS(A,IA)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .       A(3,3),IA(3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER PERMUTI,PERMUTJ
      my_real
     .   P(3,3),IP(3,3),AL(3,3),B(3,3),C(3,3),
     .   FI,FJ,FK,U1,U2,U3,PIJ,V1,V2,V3,PIK,
     .   AI,AIJ,AIJ2,AIK,AIK2,AJK,AJK2,NU,NV,NW,
     .   DETMIN
C--------------------------------------------------------
      FI=A(1,1)*A(1,1)+A(2,1)*A(2,1)+A(3,1)*A(3,1)
      FJ=A(1,2)*A(1,2)+A(2,2)*A(2,2)+A(3,2)*A(3,2)
      FK=A(1,3)*A(1,3)+A(2,3)*A(2,3)+A(3,3)*A(3,3)
      IF(FI.LT.EM10.AND.FJ.LT.EM10.AND.FK.LT.EM10)THEN
C      A is almost identical to null matrix
       IA(1,1)=ZERO
       IA(1,2)=ZERO
       IA(1,3)=ZERO
       IA(2,1)=ZERO
       IA(2,2)=ZERO
       IA(2,3)=ZERO
       IA(3,1)=ZERO
       IA(3,2)=ZERO
       IA(3,3)=ZERO       
      ELSE
C---------------------------------------------------------
C       Permute i,j,k pour trouver Ai != 0
C---------------------------------------------------------
        IF(FI.GE.MAX(FJ,FK))THEN
         PERMUTI=1
        ELSEIF(FJ.GE.MAX(FI,FK))THEN
         PERMUTI=2
        ELSE
C              FK.GE.MAX(FI,FJ)
         PERMUTI=3
        ENDIF
        IF(PERMUTI.EQ.2)THEN
C         passage dans j,k,i
          FI=FJ
          C(1,1)=A(2,2)
          C(1,2)=A(2,3)
          C(1,3)=A(2,1)
          C(2,1)=A(3,2)
          C(2,2)=A(3,3)
          C(2,3)=A(3,1)
          C(3,1)=A(1,2)
          C(3,2)=A(1,3)
          C(3,3)=A(1,1)
          A(1,1)=C(1,1)
          A(1,2)=C(1,2)
          A(1,3)=C(1,3)
          A(2,1)=C(2,1)
          A(2,2)=C(2,2)
          A(2,3)=C(2,3)
          A(3,1)=C(3,1)
          A(3,2)=C(3,2)
          A(3,3)=C(3,3)
        ELSEIF(PERMUTI.EQ.3)THEN
C         passage dans k,i,j
          FI=FK
          C(1,1)=A(3,3)
          C(1,2)=A(3,1)
          C(1,3)=A(3,2)
          C(2,1)=A(1,3)
          C(2,2)=A(1,1)
          C(2,3)=A(1,2)
          C(3,1)=A(2,3)
          C(3,2)=A(2,1)
          C(3,3)=A(2,2)
          A(1,1)=C(1,1)
          A(1,2)=C(1,2)
          A(1,3)=C(1,3)
          A(2,1)=C(2,1)
          A(2,2)=C(2,2)
          A(2,3)=C(2,3)
          A(3,1)=C(3,1)
          A(3,2)=C(3,2)
          A(3,3)=C(3,3)
        ENDIF
C       Ai^Aj=(U1,-U2,U3)
        U1=A(2,1)*A(3,2)-A(3,1)*A(2,2)
        U2=A(1,1)*A(3,2)-A(1,2)*A(3,1)
        U3=A(1,1)*A(2,2)-A(2,1)*A(1,2)
        PIJ=U1*U1+U2*U2+U3*U3
C       Ai^Ak=(V1,-V2,V3)
        V1=A(2,1)*A(3,3)-A(3,1)*A(2,3)
        V2=A(1,1)*A(3,3)-A(1,3)*A(3,1)
        V3=A(1,1)*A(2,3)-A(2,1)*A(1,3)
        PIK=V1*V1+V2*V2+V3*V3
C
        IF(PIJ.LT.EM10.AND.PIK.LT.EM10)THEN
C---------------------------------------------------------
C         FI,FJ lies & FI,FK lies (matrice de rang 1).
C---------------------------------------------------------
          IF(     ABS(A(1,1)).GE.ABS(A(2,1))
     .       .AND.ABS(A(1,1)).GE.ABS(A(3,1)))THEN
            AI =A(1,1)
            AIJ=A(1,2)/AI
            AIK=A(1,3)/AI
          ELSEIF(     ABS(A(2,1)).GE.ABS(A(1,1))
     .           .AND.ABS(A(2,1)).GE.ABS(A(3,1)))THEN
            AI =A(2,1)
            AIJ=A(2,2)/AI
            AIK=A(2,3)/AI
          ELSE
            AI =A(3,1)
            AIJ=A(3,2)/AI
            AIK=A(3,3)/AI
          ENDIF
C---------
          AIJ2=AIJ*AIJ
          AIK2=AIK*AIK
C         u=j-AIJ*i appartient at Ker(A)
          NU=1./SQRT(1.+AIJ2)
          P(1,1)=-AIJ*NU
          P(2,1)=     NU
          P(3,1)=     ZERO
C         v = k-AIK*i appartient a Ker(A)
C         w = u^v orthogonal a Ker(A)
C         v = w^u
          NV=1./SQRT(AIK2+AIJ2*AIK2+(1.+AIJ2)*(1.+AIJ2))
          P(1,2)=        -AIK*NV
          P(2,2)=    -AIJ*AIK*NV
          P(3,2)=(ONE +AIJ*AIJ)*NV
C         
          NW=1./SQRT(1.+AIJ2+AIK2)
          P(1,3)=    NW
          P(2,3)=AIJ*NW
          P(3,3)=AIK*NW
C---------
C         Rotation inverse.
          IP(1,1)=  P(1,1)
          IP(1,2)=  P(2,1)
          IP(1,3)=  P(3,1)
          IP(2,1)=  P(1,2)
          IP(2,2)=  P(2,2)
          IP(2,3)=  P(3,2)
          IP(3,1)=  P(1,3)
          IP(3,2)=  P(2,3)
          IP(3,3)=  P(3,3)
C---------
C         Pseudo-inverse dans (u,v,w) : 
C         Bu=0, Bv=0, Bw=(A11/FI)*u puisque Aw=(FI/A11)*w
C         B(1,1)=0.
C         B(1,2)=0.
C         B(2,1)=0.
C         B(2,2)=0.
C         B(3,1)=0.
C         B(3,2)=0.
C         B(1,3)=0.
C         B(2,3)=0.
          B(3,3)=A(1,1)/FI
C---------
C         Retour dans (i,j,k)
C         DO I=1,3
C         DO J=1,3
C          C(I,J)=0.
C          DO K=1,3
C           C(I,J)=C(I,J)+B(I,K)*IP(K,J)
C          ENDDO
C         ENDDO
C         ENDDO
C         DO I=1,3
C         DO J=1,3
C          IA(I,J)=0.
C          DO K=1,3
C           IA(I,J)=IA(I,J)+P(I,K)*C(K,J)
C          ENDDO
C         ENDDO
C         ENDDO
          C(3,1)=B(3,3)*IP(3,1)
          C(3,2)=B(3,3)*IP(3,2)
          C(3,3)=B(3,3)*IP(3,3)
          DO I=1,3
          DO J=1,3
           IA(I,J)=P(I,3)*C(3,J)
          ENDDO
          ENDDO
         ELSE
C---------------------------------------------------------
C        matrice de rang 2.
C---------------------------------------------------------
          PERMUTJ=0
          IF(PIK.GT.PIJ)PERMUTJ=1
          IF(PERMUTJ.EQ.1)THEN
C----------------------------
C          Echange j = k,k = -j pour trouver Ai^Aj != 0
C----------------------------
           C(1,1)= A(1,1)
           C(1,2)= A(1,3)
           C(1,3)=-A(1,2)
           C(2,1)= A(3,1)
           C(2,2)= A(3,3)
           C(2,3)=-A(3,2)
           C(3,1)=-A(2,1)
           C(3,2)=-A(2,3)
           C(3,3)= A(2,2)
           A(1,1)=C(1,1)
           A(1,2)=C(1,2)
           A(1,3)=C(1,3)
           A(2,1)=C(2,1)
           A(2,2)=C(2,2)
           A(2,3)=C(2,3)
           A(3,1)=C(3,1)
           A(3,2)=C(3,2)
           A(3,3)=C(3,3)
c
           U1= V1
           U2=-V3
           U3= V2
          ENDIF
C---------
C         Decomposition Ak=AIK*Ai+AJK*Aj
          IF(ABS(U3).GE.EM10)THEN
C          U3 determinant mineur non nul
           U3=ONE/U3
           AIK=(A(1,3)*A(2,2)-A(2,3)*A(1,2))*U3
           AJK=(A(1,1)*A(2,3)-A(2,1)*A(1,3))*U3
          ELSEIF(ABS(U2).GE.EM10)THEN
C          U2 determinant mineur non nul
           U2=ONE/U2
           AIK=(A(1,3)*A(3,2)-A(3,3)*A(1,2))*U2
           AJK=(A(1,1)*A(3,3)-A(3,1)*A(1,3))*U2
          ELSE
C                ABS(U1).GE.EM10
C          U1 determinant mineur non nul
           U1=ONE/U1
           AIK=(A(2,3)*A(3,2)-A(3,3)*A(2,2))*U1
           AJK=(A(2,1)*A(3,3)-A(3,1)*A(2,3))*U1
          ENDIF
C---------
C          u = k-AIK*i-AJK*j (noyau de A)
           NU=ONE/SQRT(AIK*AIK+AJK*AJK+ONE)
           P(1,1)=-AIK*NU
           P(2,1)=-AJK*NU
           P(3,1)=     NU
C          (v,w) orthogonal a u.
C          v = u^i = j+AJK*k
           NV=ONE/SQRT(ONE + AJK*AJK)
           P(1,2)=     ZERO
           P(2,2)=     NV
           P(3,2)= AJK*NV
C          w= u^v = -(1+AJK**2)*i+AIK*AJK*j-AIK*k
           AJK2=AJK*AJK
           NW=ONE/SQRT((ONE +AJK2)*(ONE +AJK2)+AIK*AIK*AJK2+AIK*AIK)
           P(1,3)=-(ONE+AJK2)*NW
           P(2,3)=   AIK*AJK*NW
           P(3,3)=      -AIK*NW
C---------
C          Rotation inverse.
           IP(1,1)=  P(1,1)
           IP(1,2)=  P(2,1)
           IP(1,3)=  P(3,1)
           IP(2,1)=  P(1,2)
           IP(2,2)=  P(2,2)
           IP(2,3)=  P(3,2)
           IP(3,1)=  P(1,3)
           IP(3,2)=  P(2,3)
           IP(3,3)=  P(3,3)
C---------
C          Tourner A dans (u,v,w)
           DO I=1,3
           DO J=1,3
            C(I,J)=ZERO
            DO K=1,3
             C(I,J)=C(I,J)+A(I,K)*P(K,J)
            ENDDO
           ENDDO
           ENDDO
           DO I=1,3
           DO J=1,3
            AL(I,J)=ZERO
            DO K=1,3
             AL(I,J)=AL(I,J)+IP(I,K)*C(K,J)
            ENDDO
           ENDDO
           ENDDO
C---------
C          Pseudo-inverse dans (u,v,w) : 
           DETMIN=AL(2,2)*AL(3,3)-AL(3,2)*AL(2,3)
           DETMIN=1./DETMIN
           B(2,2)= AL(3,3)*DETMIN
           B(2,3)=-AL(2,3)*DETMIN
           B(3,2)=-AL(3,2)*DETMIN
           B(3,3)= AL(2,2)*DETMIN
           B(1,1)=ZERO
           B(1,2)=ZERO
           B(1,3)=ZERO
           B(2,1)=ZERO
           B(3,1)=ZERO   
C---------
C          Retour dans (i,j,k)
          DO I=1,3
          DO J=1,3
           C(I,J)=ZERO
           DO K=1,3
            C(I,J)=C(I,J)+B(I,K)*IP(K,J)
           ENDDO
          ENDDO
          ENDDO
          DO I=1,3
          DO J=1,3
            IA(I,J)=ZERO
           DO K=1,3
            IA(I,J)=IA(I,J)+P(I,K)*C(K,J)
           ENDDO
          ENDDO
          ENDDO
C---------
          IF(PERMUTJ.EQ.1)THEN
C----------------------------
C           Retour j = k,k = -j soit j = -k, k = j
C----------------------------
           C(1,1)= IA(1,1)
           C(1,2)=-IA(1,3)
           C(1,3)= IA(1,2)
           C(2,1)=-IA(3,1)
           C(2,2)= IA(3,3)
           C(2,3)=-IA(3,2)
           C(3,1)= IA(2,1)
           C(3,2)=-IA(2,3)
           C(3,3)= IA(2,2)
           IA(1,1)=C(1,1)
           iA(1,2)=C(1,2)
           IA(1,3)=C(1,3)
           IA(2,1)=C(2,1)
           IA(2,2)=C(2,2)
           IA(2,3)=C(2,3)
           IA(3,1)=C(3,1)
           IA(3,2)=C(3,2)
           IA(3,3)=C(3,3)
           ENDIF
         ENDIF
C---------------------------------------------------------
C       Retour Permutation i,j,k
C---------------------------------------------------------
         IF(PERMUTI.EQ.2)THEN
C         j,k,i -> i,j,k
          C(1,1)=IA(3,3)
          C(1,2)=IA(3,1)
          C(1,3)=IA(3,2)
          C(2,1)=IA(1,3)
          C(2,2)=IA(1,1)
          C(2,3)=IA(1,2)
          C(3,1)=IA(2,3)
          C(3,2)=IA(2,1)
          C(3,3)=IA(2,2)
          IA(1,1)=C(1,1)
          IA(1,2)=C(1,2)
          IA(1,3)=C(1,3)
          IA(2,1)=C(2,1)
          IA(2,2)=C(2,2)
          IA(2,3)=C(2,3)
          IA(3,1)=C(3,1)
          IA(3,2)=C(3,2)
          IA(3,3)=C(3,3)
         ELSEIF(PERMUTI.EQ.3)THEN
C         k,i,j -> i,j,k
          C(1,1)=IA(2,2)
          C(1,2)=IA(2,3)
          C(1,3)=IA(2,1)
          C(2,1)=IA(3,2)
          C(2,2)=IA(3,3)
          C(2,3)=IA(3,1)
          C(3,1)=IA(1,2)
          C(3,2)=IA(1,3)
          C(3,3)=IA(1,1)
          IA(1,1)=C(1,1)
          IA(1,2)=C(1,2)
          IA(1,3)=C(1,3)
          IA(2,1)=C(2,1)
          IA(2,2)=C(2,2)
          IA(2,3)=C(2,3)
          IA(3,1)=C(3,1)
          IA(3,2)=C(3,2)
          IA(3,3)=C(3,3)
         ENDIF
      ENDIF
C--------------------------------------------------------
C     WRITE(*,'(3G12.5)')IA(1,1),IA(1,2),IA(1,3)
C     WRITE(*,'(3G12.5)')IA(2,1),IA(2,2),IA(2,3)
C     WRITE(*,'(3G12.5)')IA(3,1),IA(3,2),IA(3,3)
C--------------------------------------------------------
      RETURN
      END
