Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i3loc3    ../engine/source/interfaces/inter3d/i3loc3.F
!||--- called by ------------------------------------------------------
!||    i6main    ../engine/source/interfaces/inter3d/i6main.F
!||    i9main3   ../engine/source/interfaces/int09/i9main3.F
!||    intvo3    ../engine/source/interfaces/inter3d/intvo3.F
!||====================================================================
      SUBROUTINE I3LOC3(
     1   X,       IRECT,   LMSR,    MSR,
     2   NSV,     ILOC,    NSEG,    XI,
     3   YI,      ZI,      XFACE,   LFT,
     4   LLT,     NFT)
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
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: NFT
      INTEGER IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), NSEG(*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::  XI,YI,ZI,XFACE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, JL, JLNEW, LL2, LL1, LL, LG, J, K, M, N, JG,
     .   KG, MG, NG
C     REAL
      my_real
     .   GMS, CMS,
     .   DMS, EMS, FMS
C-----------------------------------------------

      DO 100 I=LFT,LLT
      XFACE(I)=ONE
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
      IF(CMS<GMS) THEN
       GMS=CMS
       JLNEW=J
      ENDIF
      IF(DMS<GMS) THEN
       GMS=DMS
       JLNEW=K
      ENDIF
      IF(EMS<GMS) THEN
       GMS=EMS
       JLNEW=M
      ENDIF
      IF(FMS<GMS) THEN
       GMS=FMS
       JLNEW=N
      ENDIF
 120  CONTINUE
      ILOC(IL)=JLNEW
 130  CONTINUE
C
      RETURN
      END
