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
!||    grelem_sav   ../engine/source/output/th/grelem_sav.F
!||--- called by ------------------------------------------------------
!||    c3bilan      ../engine/source/elements/sh3n/coque3n/c3bilan.F
!||    cbilan       ../engine/source/elements/shell/coque/cbilan.F
!||    ige3dbilan   ../engine/source/elements/ige3d/ige3dbilan.F
!||    pbilan       ../engine/source/elements/beam/pbilan.F
!||    qbilan       ../engine/source/elements/solid_2d/quad/qbilan.F
!||    r23bilan     ../engine/source/elements/spring/r23bilan.F
!||    r3bilan      ../engine/source/elements/spring/r3bilan.F
!||    r5bilan      ../engine/source/elements/spring/r5bilan.F
!||    rbilan       ../engine/source/elements/spring/rbilan.F
!||    rbilan33     ../engine/source/elements/joint/rbilan33.F
!||    s10bilan     ../engine/source/elements/solid/solide10/s10bilan.F
!||    s16bilan     ../engine/source/elements/thickshell/solide16/s16bilan.F
!||    s20bilan     ../engine/source/elements/solid/solide20/s20bilan.F
!||    s4bilan      ../engine/source/elements/solid/solide4/s4bilan.F
!||    s6cbilan     ../engine/source/elements/thickshell/solide6c/s6cbilan.F
!||    s8bilan      ../engine/source/elements/solid/solide8/s8bilan.F
!||    sbilan       ../engine/source/elements/solid/solide/sbilan.F
!||    spbilan      ../engine/source/elements/sph/spbilan.F
!||    sr8bilan     ../engine/source/elements/solid/solide8/sr8bilan.F
!||    srbilan      ../engine/source/elements/solid/solide/srbilan.F
!||    tbilan       ../engine/source/elements/truss/tbilan.F
!||====================================================================
      SUBROUTINE GRELEM_SAV(JFT   ,JLT   ,GRESAV,IGRTH ,GRTH  ,
     2                      OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                      ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                      XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                      IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                      REK   ,FLAG)
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
      INTEGER JFT,JLT,FLAG
      INTEGER IGRTH(*),GRTH(*)
      my_real
     .   GRESAV(NPSAV,*),OFF(*),
     .   XMAS(*), EI(*),REI(*),REK(*),
     .   EK(*), XM(*), YM(*), ZM(*),
     .   XXM(*), YYM(*), ZZM(*), 
     .   XCG(*), YCG(*), ZCG(*),
     .   IXX(*), IYY(*), IZZ(*),
     .   IXY(*), IYZ(*), IZX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------------------------
      DO I=JFT,JLT
        IF (IGRTH(I) /= IGRTH(I+1)) THEN
          IF(OFF(I) /= ZERO)THEN
            DO J = IGRTH(I),IGRTH(I+1)-1
              GRESAV(1,GRTH(J)) = GRESAV(1,GRTH(J)) + EI(I)
              GRESAV(2,GRTH(J)) = GRESAV(2,GRTH(J)) + EK(I)
              GRESAV(3,GRTH(J)) = GRESAV(3,GRTH(J)) + XM(I)
              GRESAV(4,GRTH(J)) = GRESAV(4,GRTH(J)) + YM(I)
              GRESAV(5,GRTH(J)) = GRESAV(5,GRTH(J)) + ZM(I)
              GRESAV(6,GRTH(J)) = GRESAV(6,GRTH(J)) + XMAS(I)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      IF (FLAG /= 0)THEN
        DO I=JFT,JLT
          IF (IGRTH(I) /= IGRTH(I+1)) THEN
            DO J = IGRTH(I),IGRTH(I+1)-1
              GRESAV(9,GRTH(J))  = GRESAV(9,GRTH(J))  + XCG(I)
              GRESAV(10,GRTH(J)) = GRESAV(10,GRTH(J)) + YCG(I)
              GRESAV(11,GRTH(J)) = GRESAV(11,GRTH(J)) + ZCG(I)
              GRESAV(12,GRTH(J)) = GRESAV(12,GRTH(J)) + XXM(I)
              GRESAV(13,GRTH(J)) = GRESAV(13,GRTH(J)) + YYM(I)
              GRESAV(14,GRTH(J)) = GRESAV(14,GRTH(J)) + ZZM(I)
              GRESAV(15,GRTH(J)) = GRESAV(15,GRTH(J)) + IXX(I)
              GRESAV(16,GRTH(J)) = GRESAV(16,GRTH(J)) + IYY(I)
              GRESAV(17,GRTH(J)) = GRESAV(17,GRTH(J)) + IZZ(I)
              GRESAV(18,GRTH(J)) = GRESAV(18,GRTH(J)) + IXY(I)
              GRESAV(19,GRTH(J)) = GRESAV(19,GRTH(J)) + IYZ(I)
              GRESAV(20,GRTH(J)) = GRESAV(20,GRTH(J)) + IZX(I)
              GRESAV(21,GRTH(J)) = GRESAV(21,GRTH(J)) + REI(I)
              GRESAV(22,GRTH(J)) = GRESAV(22,GRTH(J)) + REK(I)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END
