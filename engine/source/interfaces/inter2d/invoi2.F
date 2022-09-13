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
Chd|  INVOI2                        source/interfaces/inter2d/invoi2.F
Chd|-- called by -----------
Chd|        I9MAIN2                       source/interfaces/int09/i9main2.F
Chd|        INTVO2                        source/interfaces/inter2d/intvo2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INVOI2(X,IRECT,LMSR,MSR,NSV,ILOC,IRTL,NSEG,NSN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN
      INTEGER IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), IRTL(*),
     .   NSEG(*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, NUM, NPT, JJ, LL, L, LG, M, MG
C     REAL
      my_real
     .   CMS, DMS, RESULT, C2, C3, S2, S3, CS, D2, CC
C-----------------------------------------------
      DO 200 II=1,NSN
      I=NSV(II)
      J=ILOC(II)
      K=MSR(J)
      CMS=(X(2,I)-X(2,K))**2+(X(3,I)-X(3,K))**2
      NUM=NSEG(J+1)-NSEG(J)
      NPT=NSEG(J)-1
C---------------------------------------------
C     RECHERCHE DU NOEUD main LE PLUS PROCHE
C---------------------------------------------
      DO 100 JJ=1,NUM
      LL=LMSR(NPT+JJ)
      L=IRECT(1,LL)
      IF(L==J)L=IRECT(2,LL)
      LG=MSR(L)
      DMS=(X(2,I)-X(2,LG))**2+(X(3,I)-X(3,LG))**2
      IF(DMS<CMS)THEN
       CMS=DMS
       J=L
       K=LG
      ENDIF
  100 CONTINUE
C------------------------------------------------
C     RECHERCHE DU SEGMENT main LE PLUS PROCHE
C------------------------------------------------
      ILOC(II)=J
      RESULT=EP30
      NUM=NSEG(J+1)-NSEG(J)
      NPT=NSEG(J)-1
C
      DO 150 JJ=1,NUM
      LL=LMSR(JJ+NPT)
      M=IRECT(1,LL)
      IF(M==J)M=IRECT(2,LL)
      MG=MSR(M)
C
      C2=X(2,MG)-X(2,K)
      C3=X(3,MG)-X(3,K)
      S2=X(2,I)-X(2,K)
      S3=X(3,I)-X(3,K)
      CS=C2*S2+C3*S3
      D2=S2*S2+S3*S3
      IF(CS>ZERO)THEN
       CC=C2*C2+C3*C3
       D2=D2-CS*CS/CC
      ENDIF
      IF(D2>RESULT)GOTO 150
       RESULT=D2
       IRTL(II)=LL
  150 CONTINUE
  200 CONTINUE
      RETURN
      END
