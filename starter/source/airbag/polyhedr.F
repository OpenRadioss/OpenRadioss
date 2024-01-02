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
Chd|  POLYHEDR                      source/airbag/polyhedr.F      
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLYHEDR(IPOLY, RPOLY   , POLB  , NPOLB, POLH,
     .                    NPOLH, NRPMAX  , NPHMAX, IBRIC, LMIN,
     .                    INFO , NPOLHMAX, NPPMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPPMAX, IPOLY(6+NPPMAX,*), POLB(*), NPOLB, NPHMAX, 
     .        POLH(NPHMAX+2,*),NPOLH, NRPMAX, IBRIC, INFO, NPOLHMAX
      my_real
     .        RPOLY(NRPMAX,*), LMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NPOLB), ICMAX, ICUR, II, J, JJ, K, KK, ISTOP,
     .        L, LL, ICUR_OLD, ITY, JMIN, PMIN, POLD
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, XX1, YY1, ZZ1, XX2, YY2, ZZ2,
     .        DD11, DD12, DD21, DD22, TOLE
C
      my_real
     .        DLAMCH_RD
      EXTERNAL DLAMCH_RD
C
      TOLE=EPSILON(ZERO)*0.5*LMIN*LMIN
C
      DO I=1,NPOLB
         ITAG(I)=0
      ENDDO
C
      ICMAX=0
      DO I=1,NPOLB
         IF (ITAG(I)==0) THEN
            ICMAX=ICMAX+1
            ITAG(I)=ICMAX
            ICUR=ICMAX
         ELSE
            ICUR=ITAG(I)
         ENDIF
         II=POLB(I)
         DO J=1,IPOLY(2,II)
            IF (J/=IPOLY(2,II)) THEN
               JJ=J+1
            ELSE
               JJ=1
            ENDIF
            X1=RPOLY(4+3*(J-1)+1,II)
            Y1=RPOLY(4+3*(J-1)+2,II)
            Z1=RPOLY(4+3*(J-1)+3,II)
            X2=RPOLY(4+3*(JJ-1)+1,II)
            Y2=RPOLY(4+3*(JJ-1)+2,II)
            Z2=RPOLY(4+3*(JJ-1)+3,II)
            DO K=1,NPOLB
               IF (K==I) CYCLE
               KK=POLB(K)
               ISTOP=0
               L=0
               DO WHILE (ISTOP==0.AND.L<IPOLY(2,KK))
                  L=L+1
                  IF (L/=IPOLY(2,KK)) THEN
                     LL=L+1
                  ELSE
                     LL=1
                  ENDIF
                  XX1=RPOLY(4+3*(L-1)+1,KK)
                  YY1=RPOLY(4+3*(L-1)+2,KK)
                  ZZ1=RPOLY(4+3*(L-1)+3,KK)
                  XX2=RPOLY(4+3*(LL-1)+1,KK)
                  YY2=RPOLY(4+3*(LL-1)+2,KK)
                  ZZ2=RPOLY(4+3*(LL-1)+3,KK)
                  DD11=(XX1-X1)**2+(YY1-Y1)**2+(ZZ1-Z1)**2
                  DD21=(XX2-X1)**2+(YY2-Y1)**2+(ZZ2-Z1)**2
                  DD12=(XX1-X2)**2+(YY1-Y2)**2+(ZZ1-Z2)**2
                  DD22=(XX2-X2)**2+(YY2-Y2)**2+(ZZ2-Z2)**2
                  IF ((DD11<=TOLE.AND.DD22<=TOLE).OR.
     .                (DD21<=TOLE.AND.DD12<=TOLE)) ISTOP=L
               ENDDO
               IF (ISTOP/=0) THEN
                  IF (ITAG(K)==0) THEN
                     ITAG(K)=ICUR
                  ELSE
                     ICUR_OLD=ICUR
                     ICUR=ITAG(K)
                     DO L=1,NPOLB
                        IF (ITAG(L)==ICUR_OLD) ITAG(L)=ICUR
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
      NPOLH=0
      DO I=1,ICMAX
         II=0
         DO J=1,NPOLB
            IF (ITAG(J)==I) II=II+1
         ENDDO
         IF (II/=0) NPOLH=NPOLH+1
      ENDDO
      IF (NPOLH>NPOLHMAX) THEN
         INFO=1
         NPOLHMAX=NPOLH
         RETURN
      ENDIF
C
      NPOLH=0
      DO I=1,ICMAX
         II=0
         DO J=1,NPOLB
            IF (ITAG(J)==I) II=II+1
         ENDDO
         IF (II/=0) THEN
            NPOLH=NPOLH+1
            POLH(1,NPOLH)=II
            POLH(2,NPOLH)=IBRIC
            II=0
            DO J=1,NPOLB
               IF (ITAG(J)==I) THEN
                  II=II+1
                  JJ=POLB(J)
                  POLH(2+II,NPOLH)=JJ
                  ITY=IPOLY(1,JJ)
                  IF (ITY==1) THEN
                     IPOLY(5,JJ)=NPOLH
                     CYCLE
                  ENDIF
                  IF (IPOLY(5,JJ)==0) THEN
                     IPOLY(5,JJ)=NPOLH
                  ELSE
                     IPOLY(6,JJ)=NPOLH
                  ENDIF
               ENDIF
            ENDDO
C Tri par ordre croissant de polh
            DO J=1,POLH(1,NPOLH)
               JMIN=J
               PMIN=POLH(2+J,NPOLH)
               DO K=J+1,POLH(1,NPOLH)
                  IF (POLH(2+K,NPOLH)<PMIN) THEN
                     JMIN=K
                     PMIN=POLH(2+K,NPOLH)
                  ENDIF
               ENDDO
               POLD=POLH(2+J,NPOLH)
               POLH(2+J,NPOLH)=PMIN
               POLH(2+JMIN,NPOLH)=POLD
            ENDDO
         ENDIF
      ENDDO
C
      INFO=0
      RETURN
      END
