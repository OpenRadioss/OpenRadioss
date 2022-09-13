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
Chd|  I3MSR3                        source/interfaces/inter3d/i3msr3.F
Chd|-- called by -----------
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|-- calls ---------------
Chd|        NEAREST_SEG                   ../common_source/interf/nearest_seg.F
Chd|====================================================================
      SUBROUTINE I3MSR3(X,IRECT,LMSR,MSR,NSV,ILOC,IRTL,NSEG,XFACE)
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
      INTEGER, INTENT(IN)  :: IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), NSEG(*)
      INTEGER, INTENT(INOUT) :: IRTL(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  XFACE
      my_real, INTENT(IN)  ::  X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, JL, L, JJJ, JJ, J1, J2, LL1, LL2, LL, LG, IG, JG, M, N
      INTEGER KM1(4), KN1(4), LSEG, LSEG_NEW
      my_real BMIN, BMAX
      DATA KM1/2,3,4,1/
      DATA KN1/4,1,2,3/
C-----------------------------------------------
      DO I=LFT,LLT
        IL=I+NFT
        IG=NSV(IL)
        JL=ILOC(IL)
        JG=MSR(JL)
        L=IRTL(IL)
        IF(XFACE(I)==ZERO)THEN
         IRTL(IL)=MAX(L,1)
        ELSE
         BMAX=-EP30
         LSEG_NEW=0
C
         IF(L==0) GOTO 100
         LSEG=L
         DO JJJ=1,4
           JJ=JJJ
           IF(IRECT(JJ,L)==JL) EXIT
         ENDDO
         J1=KM1(JJ)
         J2=KN1(JJ)
         IF(JJ==3.AND.IRECT(3,L)==IRECT(4,L)) J1=1
         M=MSR(IRECT(J1,L))
         N=MSR(IRECT(J2,L))
         CALL NEAREST_SEG(X, IG, JG, M, N, LSEG, LSEG_NEW, BMIN, BMAX)
         IF(BMIN >= ZERO) GO TO 200
C
 100     CONTINUE
         LL1=NSEG(JL)
         LL2=NSEG(JL+1)-1
         DO LL=LL1,LL2
           LG=LMSR(LL)
           LSEG=LG
           IF(L==LG) CYCLE
           DO JJJ=1,4
             JJ=JJJ
             IF(IRECT(JJ,LG)==JL) EXIT
           ENDDO
           J1=KM1(JJ)
           J2=KN1(JJ)
           IF(JJ==3.AND.IRECT(3,LG)==IRECT(4,LG)) J1=1
           M=MSR(IRECT(J1,LG))
           N=MSR(IRECT(J2,LG))
           CALL NEAREST_SEG(X, IG, JG, M, N, LSEG, LSEG_NEW, BMIN, BMAX)
           IF(BMIN < ZERO) CYCLE
           IRTL(IL)=LSEG_NEW
           GO TO 200
         ENDDO  !LL=LL1,LL2
C
         IRTL(IL)=LSEG_NEW
 200     CONTINUE
        ENDIF
      ENDDO !I=LFT,LLT
C
      RETURN
      END
