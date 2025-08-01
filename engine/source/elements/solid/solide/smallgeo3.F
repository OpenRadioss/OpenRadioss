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
!||    smallgeo3     ../engine/source/elements/solid/solide/smallgeo3.F
!||--- called by ------------------------------------------------------
!||    s10forc3      ../engine/source/elements/solid/solide10/s10forc3.F
!||    s4forc3       ../engine/source/elements/solid/solide4/s4forc3.F
!||    s8eforc3      ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8zforc3      ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    sforc3        ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3       ../engine/source/elements/solid/solidez/szforc3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    dt_mod        ../engine/source/modules/dt_mod.F
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE SMALLGEO3(
     1   NGL,     OFFG,    VOLG,    DELTAX,
     2   VOLG0,   ITET,    NEL,     ISMSTR,DT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE DT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER ITET,NGL(*)
      my_real
     .   OFFG(*),VOLG(*),DELTAX(*), VOLG0(*)
      TYPE(DT_), INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ICOUNT,LIST(MVSIZ)
      my_real
     .   TET_COLLA(MVSIZ),COEF,DEF_V,AL,MIN_ASPECT,MIN_DEFV
C---------------------------------------------
      MIN_ASPECT = DT%BRICK_CST_COL_MIN
      MIN_DEFV = DT%BRICK_CST_DEFV_MIN
C-
      IF(MIN_ASPECT==ZERO.AND.MIN_DEFV==ZERO) RETURN
      IF(ISMSTR/=2.AND.ISMSTR/=12) RETURN
      COEF = ONE
      IF (ITET >0) COEF = ONE/1.24/SQR3
C
      IF (MIN_DEFV >ZERO) THEN
       DO I=1,NEL
        IF (OFFG(I)>ONE.OR.OFFG(I)==ZERO) CYCLE
        DEF_V = VOLG(I)/VOLG0(I)
        IF (DEF_V < MIN_DEFV) THEN
         OFFG(I) = TWO
#include "lockon.inc"
         CALL ANCMSG(MSGID=267,ANMODE=ANINFO,
     .                    I1=NGL(I),
     .                    R1=DEF_V )
#include "lockoff.inc"
        END IF
       END DO
      END IF !(MIN_DEFV)>ZERO) THEN
C      
      IF (MIN_ASPECT>ZERO) THEN
       ICOUNT = 0
       DO I=1,NEL
        IF (OFFG(I)>ONE.OR.OFFG(I)==ZERO) CYCLE
c       AL = VOLG(I)**(1.0D0/3.0D0)!  EXP(THIRD*LOG(VOLG(I)))
c       TET_COLLA(I)= COEF* (DELTAX(I)/AL)**(3.0D0/2.0D0)!COEF*EXP(THREE_HALF*LOG(DELTAX(I)/AL))
        TET_COLLA(I) = COEF * SQRT(DELTAX(I)**3 / VOLG(I))
        IF (TET_COLLA(I) < MIN_ASPECT) THEN
         OFFG(I) = TWO
         ICOUNT=ICOUNT+1
         LIST(ICOUNT)=I
        END IF
       END DO
       IF (ICOUNT > 0) THEN
#include "lockon.inc"
          WRITE(IOUT,1000)
          WRITE(ISTDO,1000)
        DO J=1,ICOUNT
          I=LIST(J)
          WRITE(IOUT,'(6X,I10,1PG20.13)')NGL(I),TET_COLLA(I)
          WRITE(ISTDO,'(6X,I10,1PG20.13)')NGL(I),TET_COLLA(I)
        ENDDO
         WRITE(IOUT,*)
         WRITE(ISTDO,*)
#include "lockoff.inc"
       END IF !(ICOUNT > 0) THEN
      END IF !(MIN_ASPECT)>ZERO) THEN
C
      RETURN
 1000 FORMAT(/
     .    'THE FOLLOWING ELEMENTS WILL BE SWITCHED TO SMALL STRAIN DUE TO BAD ASPECT RATIO' /)
      END
