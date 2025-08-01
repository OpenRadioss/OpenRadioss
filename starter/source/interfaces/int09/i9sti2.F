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
C
!||====================================================================
!||    i9sti2                 ../starter/source/interfaces/int09/i9sti2.F
!||--- called by ------------------------------------------------------
!||    inint2                 ../starter/source/interfaces/inter2d1/inint2.F
!||--- calls      -----------------------------------------------------
!||    inare2                 ../starter/source/interfaces/inter2d1/inare2.F
!||    inori2                 ../starter/source/interfaces/inter2d1/inori2.F
!||    inrch2                 ../starter/source/interfaces/inter2d1/inrch2.F
!||    local_index            ../starter/source/interfaces/interf1/local_index.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE I9STI2(
     1 X     ,IRECT  ,IXQ   ,
     2 NRT   ,NINT   ,
     3 NSN   ,NSV    ,NOINT ,IELE  ,
     4 ID    ,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      INTEGER NRT, NINT, NSN, NOINT
      INTEGER IRECT(4,*), IXQ(7,*), NSV(*), IELE(*)
      my_real X(3,*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE)::TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, NEL, J, IG, IL
C
      my_real AREA, XL2, YM1, YM2, ZM1, ZM2, YE(4), ZE(4)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
      DO I=1,NRT
        II=I
        CALL INRCH2(X,    IRECT, IXQ, II, NEL,
     .              NINT, YM1,   YM2, ZM1,
     .              ZM2  ,YE,    ZE,  ID, TITR)
        IF(NEL/=0) THEN
         CALL INARE2(AREA, YE, ZE)
         XL2=(YM2-YM1)**2+(ZM2-ZM1)**2
         IELE(I) = NEL
        ENDIF
C
        CALL INORI2(IRECT, II, NEL, NINT,
     .              NOINT, YM1, YM2, ZM1, ZM2,
     .              YE, ZE )
      ENDDO
C-----------------------------------------------------
C     MISE DANS IRECT DU NUMERO LOCAL DU NOEUD
C-----------------------------------------------------
      DO  I=1,NRT
        IG=IRECT(1,I)
        CALL LOCAL_INDEX(IL,IG,NSV,NSN)
        IRECT(1,I)=IL
        IG=IRECT(2,I)
        CALL LOCAL_INDEX(IL,IG,NSV,NSN)
        IRECT(2,I)=IL
      ENDDO
C
      RETURN
      END
