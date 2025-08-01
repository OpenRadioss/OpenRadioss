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
!||    get_etfac_s    ../engine/source/elements/solid/solide8z/get_etfac_s.F
!||--- called by ------------------------------------------------------
!||    cmatc3         ../engine/source/elements/shell/coqueba/cmatc3.F
!||    cmatip3        ../engine/source/elements/shell/coqueba/cmatc3.F
!||    mmats          ../engine/source/elements/solid/solide8z/mmats.F
!||--- calls      -----------------------------------------------------
!||    iktmat_ini     ../engine/source/implicit/imp_init.F
!||--- uses       -----------------------------------------------------
!||    imp_ktan       ../engine/share/modules/impbufdef_mod.F
!||    imp_ktan_def   ../engine/share/modules/impbufdef_mod.F
!||====================================================================
      SUBROUTINE GET_ETFAC_S(NEL,SFAC,MTN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MTN
C     REAL
      my_real
     .   SFAC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J,IKTMAT
C     REAL
C-----------------------------------------------
       CALL IKTMAT_INI(MTN,IKTMAT)
        DO I = 1,NEL
          SFAC(I)= ONE
        ENDDO
       IF (IKTMAT==0.OR.INCONV < 0) RETURN
C
        DO I = 1,NEL
         SFAC(I)=MAX(EM5,KTBUF_STR(NG_IMP)%ETFAC(I))
        ENDDO
C
      RETURN
      END
