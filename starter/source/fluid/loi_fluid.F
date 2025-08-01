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
!||    loi_fluid        ../starter/source/fluid/loi_fluid.F
!||--- called by ------------------------------------------------------
!||    ale_euler_init   ../starter/source/materials/ale/ale_euler_init.F
!||    i22err3          ../starter/source/interfaces/inter3d1/i22err3.F
!||    qdlen2           ../starter/source/elements/solid_2d/quad/qdlen2.F
!||    set_poin_ump     ../starter/source/system/set_poin_ump.F
!||====================================================================
      LOGICAL FUNCTION LOI_FLUID(MLN)
#include      "implicit_f.inc"
      INTEGER MLN
      IF(MLN==5   .OR.
     .   MLN==6   .OR.
     .   MLN==11  .OR.
     .   MLN==17  .OR.
     .   MLN==37  .OR.
     .   MLN==51  .OR.
     .   MLN==46  .OR.
     .   MLN==47  .OR.
     .   MLN==97  .OR.
     .   MLN==105 .OR.
     .   MLN==151)THEN
       LOI_FLUID=.TRUE.
      ELSE
       LOI_FLUID=.FALSE.
      ENDIF
      RETURN
      END
