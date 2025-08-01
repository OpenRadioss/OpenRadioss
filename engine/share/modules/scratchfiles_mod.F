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
Chr|           scratchfile names are opened in 
!||====================================================================
!||    scratchfile_mod   ../engine/share/modules/scratchfiles_mod.F
!||--- called by ------------------------------------------------------
!||    arret             ../engine/source/system/arret.F
!||    eigrrest          ../engine/source/output/restart/rdresb.F
!||    eigwrest          ../engine/source/output/restart/wrrest.F
!||    fxbresb           ../engine/source/output/restart/rdresb.F
!||    fxbwrest          ../engine/source/output/restart/wrrest.F
!||    radioss2          ../engine/source/engine/radioss2.F
!||====================================================================
      MODULE SCRATCHFILE_MOD
        CHARACTER(LEN=10192) :: IINFNA,IUSC4_FNAM,IUSC2_FNAM,
     *                        IFXS_FN,IFXM_FN,IEIGM_FN
        INTEGER USE_IFXM, USE_IFXS,USE_IEIGM
        INTEGER LEN_IINFNA,LEN_IUSC4_FNAM,LEN_IFXS_FN,LEN_IFXM_FN,
     *                        LEN_IEIGM_FN

      END MODULE SCRATCHFILE_MOD 
