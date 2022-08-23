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
Chd|  IQEL02                        source/ale/ale2d/iqel02.F     
Chd|-- called by -----------
Chd|        INTAL2                        source/ale/intal2.F           
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IQEL02(X,IRECT,LMSR,MSR,NSV,ILOC,IRTL,
     +                  NSN,NSEG,CRST,NOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NSN
      INTEGER,INTENT(IN)    :: IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), IRTL(*),NSEG(*)
      my_real,INTENT(IN)    :: X(3,NUMNOD), CRST(2,*)
      my_real,INTENT(INOUT) :: NOR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, L, M1, M2, LK, NUM, NPT, JJ
      my_real N2, N3, YM1, YM2, ZM1, ZM2, SS, SCK, XMG
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO II=1,NSN
        I   = NSV(II)
        J   = ILOC(II)
        L   = IRTL(II)
        M1  = MSR(IRECT(1,L))
        M2  = MSR(IRECT(2,L))
        YM1 = X(2,M1)
        YM2 = X(2,M2)
        ZM1 = X(3,M1)
        ZM2 = X(3,M2)
        !C-------------------------
        !C     NORMAL VECTOR
        !C-------------------------
        N2  = ZM1-ZM2
        N3  = YM2-YM1
        SS  = CRST(1,II)
        SCK = ABS(SS)-ONE
        IF(ABS(SCK) <= FIVEEM2) THEN
          !C----------------------------------
          !C     EXTREMITIES
          !C     WEIGHTED MEAN VALUES FROM NORMAL VECTORS
          !C----------------------------------
          LK=L
          NUM=NSEG(J+1)-NSEG(J)
          IF(NUM == 2) THEN
            NPT=NSEG(J)-1
            DO JJ=1,NUM
              L=LMSR(NPT+JJ)
              IF(L /= LK) THEN
                M1  = MSR(IRECT(1,L))
                M2  = MSR(IRECT(2,L))
                YM1 = X(2,M1)
                YM2 = X(2,M2)
                ZM1 = X(3,M1)
                ZM2 = X(3,M2)
                N2  = N2+ZM1-ZM2
                N3  = N3+YM2-YM1
              ENDIF
            ENDDO !next JJ
          ENDIF
        ENDIF
        XMG       = SQRT(N2*N2+N3*N3)
        N2        = N2/XMG
        N3        = N3/XMG
        NOR(1,II) = ZERO
        NOR(2,II) = N2
        NOR(3,II) = N3
        ENDDO !next II
C-----------------------------------------------        
      RETURN
      END
