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
Chd|  S10UPD11T12                   source/elements/solid/solide10/s10upd11t12.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        S10PIJ12                      source/elements/solid/solide10/s10forc3.F
Chd|        S10PIJSAV12                   source/elements/solid/solide10/s10forc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S10UPD11T12(ELBUF_TAB ,OFFG   ,OFFG0  ,NC   ,
     +                       XX   ,YY   ,ZZ   ,NEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC(MVSIZ,10),NEL
      DOUBLE PRECISION
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
     
C     REAL
      my_real
     .  OFFG(*),OFFG0(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NCH,INDEX(MVSIZ),J ,N   ,IP
C     REAL
      my_real
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5)
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      DOUBLE PRECISION
     .  XXD(MVSIZ,10), YYD(MVSIZ,10), ZZD(MVSIZ,10)
C-----------------------------------------------
        NCH=0
        DO I=LFT,LLT
         IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE ) THEN
           NCH = NCH +1
           INDEX(NCH) = I
         END IF
	END DO
	IF (NCH==0) RETURN
        DO J=1,NCH
          I= INDEX(J)
          XXD(J,1:10)=XX(I,1:10)
          YYD(J,1:10)=YY(I,1:10)
          ZZD(J,1:10)=ZZ(I,1:10)
        END DO
        CALL S10PIJ12(NPT,XXD, YYD, ZZD, PX,PY,PZ ,NCH)
C----
       DO IP=1,NPT
        LBUF => ELBUF_TAB%BUFLY(1)%LBUF(IP,1,1)
        CALL S10PIJSAV12(PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),
     .                   LBUF%PIJ,LBUF%SIG,LBUF%SIGL,INDEX,NCH,NEL)
       ENDDO
C-----------
      RETURN
      END
