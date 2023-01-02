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
C
Chd|====================================================================
Chd|  I11SOL                        source/interfaces/inter3d1/i11sol.F
Chd|-- called by -----------
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11SOL(X,IRECT,IXS,NINT,NEL,I,
     .                  AREA,NOINT,KNOD2ELS,NOD2ELS,IXS10)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr07_c.2.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NEL, I, NOINT
C     REAL
      my_real
     .   AREA
      INTEGER IRECT(2,*), IXS(NIXS,*), KNOD2ELS(*), NOD2ELS(*),
     .        IXS10(6,*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JJ, II, K, NN, IAD,
     .        NUSER, NUSERM
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      NEL=0
      IF(NUMELS==0) RETURN
       NUSERM = -1
       DO 230 IAD=KNOD2ELS(IRECT(1,I))+1,KNOD2ELS(IRECT(1,I)+1)
          N = NOD2ELS(IAD)
          DO 220 JJ=1,2
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 220
            ENDDO
            IF(N > NUMELS8 .AND. N <= NUMELS8+NUMELS10 )THEN
              DO K=1,6
                IF(IXS10(K,N-NUMELS8)==II) GOTO 220
              ENDDO
            ENDIF
            GOTO 230
  220     CONTINUE
          NUSER = IXS(11,N)
          IF (NUSER>NUSERM) THEN
            NEL = N
            NUSERM = NUSER
          ENDIF
  230  CONTINUE
       RETURN
C
      END
