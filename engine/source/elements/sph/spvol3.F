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
!||    spvol3    ../engine/source/elements/sph/spvol3.F
!||--- called by ------------------------------------------------------
!||    spstres   ../engine/source/elements/sph/spstres.F
!||====================================================================
      SUBROUTINE SPVOL3(
     1   OFF,     PM,      VOLO,    EINT,
     2   RHOA,    RHON,    VOLN,    DVOL,
     3   NGL,     MAT,     LFT,     LLT,
     4   JLAG)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: JLAG
C     REAL
      my_real
     .   OFF(*),PM(NPROPM,*),VOLO(*),EINT(*),RHOA(*),
     .   RHON(*),VOLN(*),DVOL(*)
      INTEGER NGL(*),MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .      RHO0(MVSIZ),VOLA(MVSIZ)
C-----------------------------------------------
C     LAGRANGIAN ONLY.
C     IF(JLAG/=0)THEN
      DO I=LFT,LLT
       RHO0(I)=PM(1,MAT(I))
      ENDDO
C
      DO I=LFT,LLT
        VOLA(I)=(RHO0(I)/RHOA(I))*VOLO(I)
        VOLN(I)=(RHO0(I)/RHON(I))*VOLO(I)
        DVOL(I)=VOLN(I)-VOLA(I)
        EINT(I)=EINT(I)*VOLO(I)
      ENDDO
C
C      IF(IDTMIN(1)==1)THEN
C        DO 120 I=LFT,LLT
C        IF(OFF(I) ==0.)THEN
C          VOLN(I)=1.
C        ELSEIF(VOLN(I)<=VOLMIN)THEN
C          VOLN(I)=1.
C          OFF(I)=0.
C          CALL MY_LOCK
C          WRITE(ISTDO,2000) NGL(I)
C          WRITE(IOUT ,2000) NGL(I)
C          CALL MY_FREE
C        ELSEIF(VOLN(I)<=0.0)THEN
C          CALL MY_LOCK
C          WRITE(ISTDO,1000) NGL(I)
C          WRITE(IOUT ,1000) NGL(I)
C          CALL MY_FREE
C          CALL ARRET(2)
C        ENDIF
C  120   CONTINUE
C      ELSEIF(IDTMIN(1)==2)THEN
C        DO 130 I=LFT,LLT
C        IF(OFF(I) ==0.)THEN
C           VOLN(I)=1.
C        ELSEIF(VOLN(I)<=VOLMIN.OR.VOLN(I)<=0.0)THEN
C           VOLN(I)=1.
C           OFF(I)=0.
C           CALL MY_LOCK
C           WRITE(ISTDO,2000) NGL(I)
C           WRITE(IOUT ,2000) NGL(I)
C           CALL MY_FREE
C        ENDIF
C  130   CONTINUE
C      ELSE
        DO I=LFT,LLT
          IF(OFF(I) ==ZERO)VOLN(I)=ONE
        ENDDO
C      ENDIF
C
      RETURN
      END
