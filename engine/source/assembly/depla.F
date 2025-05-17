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
      !||    depla   ../engine/source/assembly/depla.F
      !||--- called by ------------------------------------------------------
      !||    resol   ../engine/source/engine/resol.F
      !||====================================================================
      SUBROUTINE DEPLA(V, D, X, VR, DR, XDP, DDP, NUMNOD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NUMNOD !< number of node
      my_real :: X(3,NUMNOD) ,D(3,NUMNOD) ,V(3,NUMNOD) ,DR(3,NUMNOD)  ,VR(3,NUMNOD)
      DOUBLE PRECISION :: XDP(3,NUMNOD), DDP(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N
      my_real VDT
      DOUBLE PRECISION VDT2
C-----------------------------------------------
C   B o d y
C-----------------------------------------------
      IF(N2D==1)THEN
!$OMP DO SCHEDULE(guided)
          DO I=1,NUMNOD
            IF(X(2,I)+DT2*V(2,I)<ZERO) V(2,I)=ZERO
          ENDDO
!$OMP END DO
      ENDIF
C-------------------------------------------------------------
C         DISPLACEMENT (TRANSLATION & ROTATIONS)
C-------------------------------------------------------------
      IF (IRESP==1) THEN
!$OMP DO SCHEDULE(guided)
        DO N=1,NUMNOD
          VDT2 = DT2*V(1,N)
          DDP(1,N) = DDP(1,N)+VDT2
          D(1,N)=DDP(1,N)
          XDP(1,N)=XDP(1,N)+VDT2
          X(1,N)=XDP(1,N)

          VDT2 = DT2*V(2,N)
          DDP(2,N) = DDP(2,N)+VDT2
          D(2,N)=DDP(2,N)
          XDP(2,N)=XDP(2,N)+VDT2
          X(2,N)=XDP(2,N)

          VDT2 = DT2*V(3,N)
          DDP(3,N) = DDP(3,N)+VDT2
          D(3,N)=DDP(3,N)
          XDP(3,N)=XDP(3,N)+VDT2
          X(3,N)=XDP(3,N)
        ENDDO
!$OMP END DO NOWAIT
      ELSE
!$OMP DO SCHEDULE(guided)
         DO N=1,NUMNOD
          VDT = DT2*V(1,N)
          D(1,N)=D(1,N)+VDT
          X(1,N)=X(1,N)+VDT
          VDT = DT2*V(2,N)
          D(2,N)=D(2,N)+VDT
          X(2,N)=X(2,N)+VDT
          VDT = DT2*V(3,N)
          D(3,N)=D(3,N)+VDT
          X(3,N)=X(3,N)+VDT
        ENDDO
!$OMP END DO NOWAIT
      ENDIF 
      IF((ISECUT>0 .OR. IISROT>0 .OR. IMPOSE_DR/=0 .OR. IDROT == 1) .AND. IRODDL /= 0) THEN
!$OMP DO SCHEDULE(guided)
        DO N=1,NUMNOD
          DR(1,N)=DR(1,N)+DT2*VR(1,N)
          DR(2,N)=DR(2,N)+DT2*VR(2,N)
          DR(3,N)=DR(3,N)+DT2*VR(3,N)
        ENDDO
!$OMP END DO
      ENDIF

      RETURN
      END
