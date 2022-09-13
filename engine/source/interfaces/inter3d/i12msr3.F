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
Chd|  I12MSR3                       source/interfaces/inter3d/i12msr3.F
Chd|-- called by -----------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|        I12_NEAREST_SEG               source/interfaces/inter3d/i12_nearest_seg.F
Chd|====================================================================
      SUBROUTINE I12MSR3(XS,XM,IRECT,LMSR,MSR,NSV,ILOC,IRTL,NSEG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), NSEG(*)
      INTEGER, INTENT(INOUT) :: IRTL(*)
      my_real, INTENT(IN)  :: XS(3,*),XM(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, JL, L, JJJ, JJ, J1, J2, LL1, LL2, LL, LG, KKK, KK, K1, K2, M, N
      INTEGER KM1(4), KN1(4), LSEG, LSEG_NEW
      my_real BMIN, BMAX
      DATA KM1/2,3,4,1/
      DATA KN1/4,1,2,3/
C-----------------------------------------------
      DO I=LFT,LLT
        IL=I+NFT
        JL=ILOC(IL)
        L=IRTL(IL)
        BMAX=-EP30
        LSEG_NEW=0

        IF(L/=0)THEN
          LSEG=L
          DO JJJ=1,4
            JJ=JJJ
            IF(IRECT(JJ,L)==JL) EXIT
          ENDDO
          J1=KM1(JJ)
          J2=KN1(JJ)
          IF(JJ==3.AND.IRECT(3,L)==IRECT(4,L)) J1=1
          M=IRECT(J1,L)
          N=IRECT(J2,L)
          CALL I12_NEAREST_SEG(XS, XM, IL, JL, M, N, LSEG, LSEG_NEW, BMIN, BMAX)
          IF(BMIN >= ZERO) CYCLE
        END IF
 
        LL1=NSEG(JL)
        LL2=NSEG(JL+1)-1
        DO LL=LL1,LL2
          LG=LMSR(LL)
          LSEG=LG
          IF(L==LG) CYCLE
          DO KKK=1,4
            KK=KKK
            IF(IRECT(KK,LG)==JL) EXIT
          END DO
          K1=KM1(KK)
          K2=KN1(KK)
          IF(KK==3.AND.IRECT(3,LG)==IRECT(4,LG)) K1=1
          M=IRECT(K1,LG)
          N=IRECT(K2,LG)
          CALL I12_NEAREST_SEG(XS, XM, IL, JL, M, N, LSEG, LSEG_NEW, BMIN, BMAX)
          IF(BMIN < ZERO) CYCLE
          IRTL(IL)=LSEG_NEW
          GO TO 200
        END DO
C
        IRTL(IL)=LSEG_NEW
 200    CONTINUE
      END DO

      RETURN
      END
