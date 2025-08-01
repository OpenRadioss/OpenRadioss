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
!||    sms_bcs            ../engine/source/ams/sms_bcs.F
!||--- called by ------------------------------------------------------
!||    sms_encin_2        ../engine/source/ams/sms_encin_2.F
!||    sms_mass_scale_2   ../engine/source/ams/sms_mass_scale_2.F
!||    sms_pcg            ../engine/source/ams/sms_pcg.F
!||--- calls      -----------------------------------------------------
!||    sms_bcs1v          ../engine/source/ams/sms_bcs.F
!||====================================================================
      SUBROUTINE SMS_BCS(NODFT  ,NODLT  ,INDX1   ,ICODT   ,ISKEW   ,
     2                   SKEW    ,A      ,NODLAST )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODLAST
      INTEGER NODFT,NODLT,INDX1(*),ICODT(*),ISKEW(*)
      my_real
     .    A(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NINDX,
     .        INDX(1024),L,J
C-----------------------------------------------
C     DO 420 I = NODFT,NODLT,1024
      DO 420 I = NODFT,NODLAST,1024
        NINDX = 0
C       DO 400 N=I,MIN(NODLT,I+1023)
        DO 400 J=I,MIN(NODLAST,I+1023)
          N=INDX1(J)
          IF (ICODT(N)/=0)THEN
            NINDX = NINDX + 1
            INDX(NINDX) = N
          ENDIF
  400   CONTINUE
        CALL SMS_BCS1V(NINDX,INDX,ISKEW,ICODT,
     .             A    ,SKEW )
  420 CONTINUE
C
      RETURN
      END
!||====================================================================
!||    sms_bcs1v   ../engine/source/ams/sms_bcs.F
!||--- called by ------------------------------------------------------
!||    sms_bcs     ../engine/source/ams/sms_bcs.F
!||====================================================================
      SUBROUTINE SMS_BCS1V(NINDX,INDX,ISKEW,ICODT,
     .                 A    ,SKEW   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, INDX(*), ISKEW(*), ICODT(*)
C     REAL
      my_real
     .   A(3,*), SKEW(LSKEW,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K, L, ISK, LCOD
C     REAL
      my_real
     .   AA
C-----------------------------------------------
#include "vectorize.inc"
      DO K = 1, NINDX
        L = INDX(K)
        ISK =ISKEW(L)
        LCOD=ICODT(L)
        IF(ISK==1) THEN
C------------------
C     REPERE GLOBAL
C------------------
      IF(LCOD==1)THEN
      A(3,L)=ZERO
      ELSEIF(LCOD==2)THEN
      A(2,L)=ZERO
      ELSEIF(LCOD==3)THEN
      A(2,L)=ZERO
      A(3,L)=ZERO
      ELSEIF(LCOD==4)THEN
      A(1,L)=ZERO
      ELSEIF(LCOD==5)THEN
      A(1,L)=ZERO
      A(3,L)=ZERO
      ELSEIF(LCOD==6)THEN
      A(1,L)=ZERO
      A(2,L)=ZERO
      ELSEIF(LCOD==7)THEN
      A(1,L)=ZERO
      A(2,L)=ZERO
      A(3,L)=ZERO
      ENDIF
C
      ELSE
C-------------------
C     REPERE OBLIQUE
C-------------------
      IF(LCOD==1)THEN
      AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(7,ISK)*AA
      A(2,L)=A(2,L)-SKEW(8,ISK)*AA
      A(3,L)=A(3,L)-SKEW(9,ISK)*AA
      ELSEIF(LCOD==2)THEN
      AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(4,ISK)*AA
      A(2,L)=A(2,L)-SKEW(5,ISK)*AA
      A(3,L)=A(3,L)-SKEW(6,ISK)*AA
      ELSEIF(LCOD==3)THEN
      AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(7,ISK)*AA
      A(2,L)=A(2,L)-SKEW(8,ISK)*AA
      A(3,L)=A(3,L)-SKEW(9,ISK)*AA
      AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(4,ISK)*AA
      A(2,L)=A(2,L)-SKEW(5,ISK)*AA
      A(3,L)=A(3,L)-SKEW(6,ISK)*AA
      ELSEIF(LCOD==4)THEN
      AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(1,ISK)*AA
      A(2,L)=A(2,L)-SKEW(2,ISK)*AA
      A(3,L)=A(3,L)-SKEW(3,ISK)*AA
      ELSEIF(LCOD==5)THEN
      AA  =SKEW(7,ISK)*A(1,L)+SKEW(8,ISK)*A(2,L)+SKEW(9,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(7,ISK)*AA
      A(2,L)=A(2,L)-SKEW(8,ISK)*AA
      A(3,L)=A(3,L)-SKEW(9,ISK)*AA
      AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(1,ISK)*AA
      A(2,L)=A(2,L)-SKEW(2,ISK)*AA
      A(3,L)=A(3,L)-SKEW(3,ISK)*AA
      ELSEIF(LCOD==6)THEN
      AA  =SKEW(1,ISK)*A(1,L)+SKEW(2,ISK)*A(2,L)+SKEW(3,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(1,ISK)*AA
      A(2,L)=A(2,L)-SKEW(2,ISK)*AA
      A(3,L)=A(3,L)-SKEW(3,ISK)*AA
      AA  =SKEW(4,ISK)*A(1,L)+SKEW(5,ISK)*A(2,L)+SKEW(6,ISK)*A(3,L)
      A(1,L)=A(1,L)-SKEW(4,ISK)*AA
      A(2,L)=A(2,L)-SKEW(5,ISK)*AA
      A(3,L)=A(3,L)-SKEW(6,ISK)*AA
      ELSEIF(LCOD==7)THEN
      A(1,L)=ZERO
      A(2,L)=ZERO
      A(3,L)=ZERO
      ENDIF
C
      END IF
C
      ENDDO
C
      RETURN
      END
