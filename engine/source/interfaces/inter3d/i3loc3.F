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
Chd|  I3LOC3                        source/interfaces/inter3d/i3loc3.F
Chd|-- called by -----------
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I3LOC3(X,IRECT,LMSR,MSR,NSV,
     1                  ILOC,NSEG,XI,YI,ZI,
     2                  XFACE)
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
      INTEGER IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), NSEG(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr04_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::  XI,YI,ZI,XFACE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, JL, JLNEW, LL2, LL1, LL, LG, J, K, M, N, JG,
     .   KG, MG, NG
C     REAL ou REAL*8
      my_real
     .   GMS, CMS,
     .   DMS, EMS, FMS
C-----------------------------------------------
!      EQUIVALENCE (XJ,BV(1,1)) ,(YJ,BV(1,2)) ,(ZJ,BV(1,3))
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (XFACE,BV(1,29))
      DO 100 I=LFT,LLT
      XFACE(I)=UN
      IL=I+NFT
      IG=NSV(IL)
      XI(I)=X(1,IG)
      YI(I)=X(2,IG)
      ZI(I)=X(3,IG)
 100  CONTINUE
C
      DO 130 I=LFT,LLT
      IL=I+NFT
      IG=NSV(IL)
      JL=ILOC(IL)
      JLNEW=JL
      LL2=NSEG(JL+1)-1
      LL1=NSEG(JL)
      GMS=1.E30
      DO 120 LL=LL1,LL2
      LG=LMSR(LL)
      J=IRECT(1,LG)
      K=IRECT(2,LG)
      M=IRECT(3,LG)
      N=IRECT(4,LG)
      JG=MSR(J)
      KG=MSR(K)
      MG=MSR(M)
      NG=MSR(N)
      CMS=(XI(I)-X(1,JG))**2+(YI(I)-X(2,JG))**2+(ZI(I)-X(3,JG))**2
      DMS=(XI(I)-X(1,KG))**2+(YI(I)-X(2,KG))**2+(ZI(I)-X(3,KG))**2
      EMS=(XI(I)-X(1,MG))**2+(YI(I)-X(2,MG))**2+(ZI(I)-X(3,MG))**2
      FMS=(XI(I)-X(1,NG))**2+(YI(I)-X(2,NG))**2+(ZI(I)-X(3,NG))**2
      IF(CMS.LT.GMS) THEN
       GMS=CMS
       JLNEW=J
      ENDIF
      IF(DMS.LT.GMS) THEN
       GMS=DMS
       JLNEW=K
      ENDIF
      IF(EMS.LT.GMS) THEN
       GMS=EMS
       JLNEW=M
      ENDIF
      IF(FMS.LT.GMS) THEN
       GMS=FMS
       JLNEW=N
      ENDIF
 120  CONTINUE
      ILOC(IL)=JLNEW
 130  CONTINUE
C
      RETURN
      END
