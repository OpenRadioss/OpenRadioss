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
C
      !||====================================================================
      !||    invoi2   ../starter/source/interfaces/inter2d1/invoi2.F
      !||--- called by ------------------------------------------------------
      !||    inint2   ../starter/source/interfaces/inter2d1/inint2.F
      !||====================================================================
      SUBROUTINE INVOI2(X,IRECT,LMSR,MSR,NSV,ILOC,IRTL,NSEG,NSN,NRT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C FOR A GIVEN SECND NODE FIND THE NEAREST MAIN SEGMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NRT
      INTEGER IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), IRTL(*),NSEG(*)
      my_real X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, NUM, NPT, JJ, LL, L, LG, M, MG
      my_real CMS, DMS, RESULT, C2, C3, S2, S3, CS, D2, CC
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(NRT == 0) RETURN
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      DO II=1,NSN !loop over nodes in list
        I=NSV(II)  ! current secnd node
        J=ILOC(II) ! nearest main node
        K=MSR(J)
        CMS = (X(2,I)-X(2,K))**2 + (X(3,I)-X(3,K))**2
        NUM=NSEG(J+1)-NSEG(J) ! number of connected main segments
        NPT=NSEG(J)-1 !pointer/index
        !---------------------------------------------
        !     NEAREST NODE
        !---------------------------------------------
        DO JJ=1,NUM !loop over connected main segments
          LL=LMSR(NPT+JJ) ! segment
          L=IRECT(1,LL)   ! first node of segment
          IF(L == J) L=IRECT(2,LL)
          LG=MSR(L)
          DMS = (X(2,I)-X(2,LG))**2 + (X(3,I)-X(3,LG))**2
          IF(DMS < CMS)THEN
           CMS=DMS
           J=L
           K=LG
          ENDIF
        ENDDO
        ILOC(II)=J
        NUM=NSEG(J+1)-NSEG(J)
        NPT=NSEG(J)-1
        !------------------------------------------------
        !     NEAREST SEGMENT : check if node projection si inside or outside the tested segment
        !    D2 = (projected length on main segment)² - (segment length)²
        !    a>0, b>0 : a²-b² > 0 => a-b >0 ;     a²-b² < 0 => a-b < 0
        !------------------------------------------------
        !  CASE D2 < 0                CASE D2 > 0
        !   projection of I             projection of I
        !   lies on segment             is out of segment
        !
        !         I
        !         +                                    I     <- secnd node
        !        /                                     +
        !      /                                       |
        !    /                                         |
        !   +--D2-x----+                  +---------+  x     <- main segment
        !  MG          K                  MG        K
        !

        RESULT=ep20
        DO JJ=1,NUM !loop over connected segment
          LL=LMSR(JJ+NPT) !lag segment
          M=IRECT(1,LL) !first node
          IF(M == J) M=IRECT(2,LL)
          MG=MSR(M)
          ! main segment
          C2=X(2,MG)-X(2,K)
          C3=X(3,MG)-X(3,K)
          !  other node on this segment -> secnd node
          S2=X(2,I)-X(2,K)
          S3=X(3,I)-X(3,K)
          !
          CS=C2*S2+C3*S3 ! dot product
          D2=S2*S2+S3*S3 ! distance : lag second node -> fluid node
          IF(CS > ZERO)THEN
           CC=C2*C2+C3*C3 ! lagrange segment : length
           D2=D2-CS*CS/CC  !
          ENDIF
          IF(D2 <= RESULT)THEN
            RESULT = D2
            IRTL(II)=LL !nearest segment
          ENDIF
        ENDDO !next JJ


      ENDDO ! NEXT II

      RETURN
      END
