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
Chd|====================================================================
Chd|  SGCOOR10                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGCOOR10(
     1   XX,      YY,      ZZ,      X,
     2   XDP,     X0,      Y0,      Z0,
     3   VX0,     VY0,     VZ0,     SAV,
     4   NC,      D,       OFF,     OFF0,
     5   NEL,     MTN,     ISMSTR)
C-----------------------------------------------
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
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER NC(MVSIZ,10),NEL
C     REAL
      my_real
     .  VX0(MVSIZ,10),VY0(MVSIZ,10),VZ0(MVSIZ,10),
     .  D(3,*) ,OFF(*),OFF0(*),X(3,*)
      DOUBLE PRECISION 
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),XDP(3,*),
     .   X0(MVSIZ,10), Y0(MVSIZ,10), Z0(MVSIZ,10),SAV(NEL,30)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE,N,N2,N3
C     REAL
C-----------------------------------------------
C----------------------------
C     NODAL COORDINATES INITIALES    |
C----------------------------
      NPE = 10
      DO N=1,NPE
       DO I=1,NEL
        X0(I,N)=SAV(I,N)
        Y0(I,N)=SAV(I,N+NPE)
        Z0(I,N)=SAV(I,N+2*NPE)
       ENDDO
      ENDDO 
C----------------------------
C     DISPLACEMENT     |
C----------------------------
        N = NPE
        DO I=1,NEL
        VX0(I,N)=XX(I,N)-X0(I,N)
        VY0(I,N)=YY(I,N)-Y0(I,N)
        VZ0(I,N)=ZZ(I,N)-Z0(I,N)
        ENDDO
       DO N=1,NPE
        DO I=1,NEL
        VX0(I,N)=XX(I,N)-X0(I,N)-VX0(I,NPE)
        VY0(I,N)=YY(I,N)-Y0(I,N)-VY0(I,NPE)
        VZ0(I,N)=ZZ(I,N)-Z0(I,N)-VZ0(I,NPE)
        ENDDO
       ENDDO 
C---   w/ /DT/BRICK/CST    
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        DO I=1,NEL
         OFF0(I)=OFF(I)
        ENDDO
       IF (MTN==1) THEN
         IF(IRESP==1)THEN
          DO I=1,NEL
            IF (OFF(I) <= ONE ) CYCLE 
            N3 = NC(I,NPE)
            VX0(I,NPE)=XDP(1,N3)-X0(I,NPE)
            VY0(I,NPE)=XDP(2,N3)-Y0(I,NPE)
            VZ0(I,NPE)=XDP(3,N3)-Z0(I,NPE)
          ENDDO 
          DO N=1,NPE
           DO I=1,NEL
            IF (OFF(I) <= ONE ) CYCLE 
            N2 = NC(I,N)
            VX0(I,N) =XDP(1,N2)-X0(I,N)-VX0(I,NPE)
            VY0(I,N) =XDP(2,N2)-Y0(I,N)-VY0(I,NPE)
            VZ0(I,N) =XDP(3,N2)-Z0(I,N)-VZ0(I,NPE)
           ENDDO
          ENDDO 
         ELSE      
          DO I=1,NEL
            IF (OFF(I) <= ONE ) CYCLE 
            N3 = NC(I,NPE)
            VX0(I,NPE)=X(1,N3)-X0(I,NPE)
            VY0(I,NPE)=X(2,N3)-Y0(I,NPE)
            VZ0(I,NPE)=X(3,N3)-Z0(I,NPE)
          ENDDO 
          DO N=1,NPE
           DO I=1,NEL
            IF (OFF(I) <= ONE ) CYCLE 
            N2 = NC(I,N)
            VX0(I,N) =X(1,N2)-X0(I,N)-VX0(I,NPE)
            VY0(I,N) =X(2,N2)-Y0(I,N)-VY0(I,NPE)
            VZ0(I,N) =X(3,N2)-Z0(I,N)-VZ0(I,NPE)
           ENDDO
          ENDDO 
         END IF !(IRESP==1)THEN
       ELSE
         DO N=1,NPE
          DO I=1,NEL
           IF (OFF(I) <= ONE ) CYCLE 
            VX0(I,N)=D(1,NC(I,N))-D(1,NC(I,NPE))  
            VY0(I,N)=D(2,NC(I,N))-D(2,NC(I,NPE))
            VZ0(I,N)=D(3,NC(I,N))-D(3,NC(I,NPE))
          ENDDO
         ENDDO
       END IF !(MTN==1) THEN
      END IF !(ISMSTR == 10.AND.IDTMIN(1)==3) THEN
C
      RETURN
      END
