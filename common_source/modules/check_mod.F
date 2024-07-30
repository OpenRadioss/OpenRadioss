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
      !||====================================================================
      !||    check_mod      ../common_source/modules/check_mod.F
      !||--- called by ------------------------------------------------------
      !||    contrl         ../starter/source/starter/contrl.F
      !||    execargcheck   ../engine/source/engine/execargcheck.F
      !||    freform        ../engine/source/input/freform.F
      !||    lectur         ../engine/source/input/lectur.F
      !||    radioss2       ../engine/source/engine/radioss2.F
      !||    resol          ../engine/source/engine/resol.F
      !||    starter0       ../starter/source/starter/starter0.F
      !||====================================================================
        MODULE CHECK_MOD

!--------------------------------------------------------------------
!       RESTART_FILE : 
!       engine :
!       ----------
!       if /RFILE/OFF or if -norst cdl are used, then no restart files
!       are written durint the run, even at the end of the engine
!       RESTART_FILE = 0 --> no restart files
!       RESTART_FILE = 1 (default value) --> restart files are written
!       starter :
!       ----------
!       if /CHECK/RFILE/OFF is used or if -check=norst cdl is used, then 
!       restart files are not written at the end of the starter
!       KCHECK_STARTER :
!       pointer in LKEY0 (starter) 
!--------------------------------------------------------------------
        INTEGER :: KCHECK_STARTER
        INTEGER :: RESTART_FILE
        INTEGER, PARAMETER :: CHECK_MESSAGE_SIZE=3
        CHARACTER(len=2048), DIMENSION(CHECK_MESSAGE_SIZE) :: CHECK_MESSAGE

        CONTAINS
!           ---------------------------------------------------------
      !||====================================================================
      !||    check_message_definition   ../common_source/modules/check_mod.F
      !||--- called by ------------------------------------------------------
      !||    radioss2                   ../engine/source/engine/radioss2.F
      !||    starter0                   ../starter/source/starter/starter0.F
      !||====================================================================
            SUBROUTINE CHECK_MESSAGE_DEFINITION()            
!$COMMENT
!           CHECK_MESSAGE_DEFINITION description
!           CHECK_MESSAGE_DEFINITION defines the output 
!           messages of the /CHECK option
!$ENDCOMMENT
!       -----------------------------------------------
!           I m p l i c i t   T y p e s
!       -----------------------------------------------
#include      "implicit_f.inc"
!       -----------------------------------------------
            CHECK_MESSAGE(1:CHECK_MESSAGE_SIZE)(1:2048) = ''
            !   1 --> 3 : /RFILE/OFF option (or -check=norst cdl for the starter
            !            and -norst cdl for the engine)
            !            1 &3,4 --> starter
            !            2     --> engine
            CHECK_MESSAGE(1)(1:60)=' .. /CHECK/RFILE/OFF : NO PARALLEL RESTART FILES GENERATION'
            CHECK_MESSAGE(2)(1:49)=' /RFILE/OFF OPTION : NO RESTART FILES GENERATION'
            CHECK_MESSAGE(3)(1:66)='   /CHECK/RFILE/OFF OPTION : NO PARALLEL RESTART FILES GENERATION'
            RETURN
            END SUBROUTINE CHECK_MESSAGE_DEFINITION

!           ---------------------------------------------------------
        END MODULE CHECK_MOD
