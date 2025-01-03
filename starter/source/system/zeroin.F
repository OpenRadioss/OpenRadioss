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
      !||    zeroin                 ../starter/source/system/zeroin.F
      !||--- called by ------------------------------------------------------
      !||    c3grtails              ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    cgrtails               ../starter/source/elements/shell/coque/cgrtails.F
      !||    hm_read_thgrki         ../starter/source/output/th/hm_read_thgrki.F
      !||    hm_read_thgrki_rbody   ../starter/source/output/th/hm_read_thgrki_rbody.F
      !||    hm_read_thgrne         ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrns         ../starter/source/output/th/hm_read_thgrns.F
      !||    hm_read_thgrsens       ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf       ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_thgrki_vent         ../starter/source/output/th/hm_thgrki_vent.F
      !||    ig3dgrtails            ../starter/source/elements/ige3d/ig3dgrtails.F
      !||    pgrtails               ../starter/source/elements/beam/pgrtails.F
      !||    qgrtails               ../starter/source/elements/solid_2d/quad/qgrtails.F
      !||    rgrtails               ../starter/source/elements/spring/rgrtails.F
      !||    sgrtails               ../starter/source/elements/solid/solide/sgrtails.F
      !||    spgrtails              ../starter/source/elements/sph/spgrtails.F
      !||    t3grtails              ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    tgrtails               ../starter/source/elements/truss/tgrtails.F
      !||    xgrtails               ../starter/source/elements/xelem/xgrtails.F
      !||====================================================================
      SUBROUTINE ZEROIN(N1,N2,MA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1, N2
      INTEGER MA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
      DO J=N1,N2
        MA(J)=0
      ENDDO
      RETURN
      END
