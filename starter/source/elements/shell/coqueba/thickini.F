Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  THICKINI                      source/elements/shell/coqueba/thickini.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE THICKINI(JFT     ,JLT   ,NFT    ,PTSH  ,NUMEL ,
     2                    THK     ,THKE  ,IX     ,NIX   ,NSIGSH,
     3                    SIGSH   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NUMEL,NIX,NFT,NSIGSH
      INTEGER IX(NIX,*),PTSH(*)
      my_real
     . THK(*),THKE(*),SIGSH(NSIGSH,*)
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,JJ,N
C=======================================================================
!
      DO I=JFT,JLT
        IF (ABS(ISIGI) /= 3.AND.ABS(ISIGI) /= 4.AND.ABS(ISIGI) /= 5) THEN
          II = I+NFT
          N = NINT(SIGSH(1,II))
          IF (N /= IX(NIX,II)) THEN
            JJ = II
            DO J = 1,NUMEL
              II= J
              N = NINT(SIGSH(1,II))
              IF (N == 0) GOTO 100
              IF (N == IX(NIX,JJ)) GOTO 60
            ENDDO
 60         CONTINUE
          ENDIF
        ELSE
          JJ=NFT+I
          N =IX(NIX,JJ)
          II=PTSH(JJ)
          IF (II == 0) GOTO 100
        ENDIF
!
        IF (SIGSH(3,II) /= ZERO) THEN
          THK(I)=SIGSH(3,II)
          THKE(I)=THK(I)
        ENDIF
!
 100  CONTINUE
      ENDDO ! DO I=JFT,JLT
C-----------
      RETURN
      END
