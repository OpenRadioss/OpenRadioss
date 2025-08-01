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
!||    leceig    ../engine/source/input/leceig.F
!||--- called by ------------------------------------------------------
!||    lectur    ../engine/source/input/lectur.F
!||--- uses       -----------------------------------------------------
!||    eig_mod   ../engine/share/modules/eig_mod.F
!||====================================================================
      SUBROUTINE LECEIG(NEIGOFF, NEOFF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EIG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEIGOFF, NEOFF, NACTI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, ID(NEIG), I, ITEMP(10), J
C      
      DO N=1,NEIG
         ID(N)=ABS(EIGIPM(1,N))
      ENDDO
C
      DO I=1,(NEOFF+9)/10
         READ(IIN,'(10I10)') (ITEMP(J),J=1,10)
         DO 100 J=1,10
            IF (ITEMP(J)==0) GOTO 100
            DO N=1,NEIG
               IF (ID(N)==ITEMP(J)) EIGIPM(1,N)=0
            ENDDO
  100    CONTINUE
      ENDDO
C
      NACTI=0
      DO N=1,NEIG
         IF (EIGIPM(1,N)==0) THEN
            IF (ISPMD==0) 
     .         WRITE(IOUT,'(A,I9,A)') ' EIGENPROBLEM:',ID(N),' SET OFF'
         ELSE
            NACTI=NACTI+1
         ENDIF
      ENDDO
C
      IF (NACTI==0) THEN
         WRITE(IOUT,'(A)') ' ALL EIGENPROBLEMS DEACTIVATED'
         WRITE(IOUT,*)
         NEIG=0
      ENDIF
C
      RETURN
      END
      
