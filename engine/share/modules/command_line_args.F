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
      !||    command_line_args_mod   ../engine/share/modules/command_line_args.F
      !||--- called by ------------------------------------------------------
      !||    execargcheck            ../engine/source/engine/execargcheck.F
      !||    inter_prepare_sort      ../engine/source/interfaces/generic/inter_prepare_sort.F
      !||    inter_sort              ../engine/source/interfaces/generic/inter_sort.F
      !||    inter_sort_07           ../engine/source/interfaces/int07/inter_sort_07.F
      !||====================================================================
      MODULE COMMAND_LINE_ARGS_MOD
        INTEGER :: GOT_PREVIEW = 0
      END MODULE COMMAND_LINE_ARGS_MOD
