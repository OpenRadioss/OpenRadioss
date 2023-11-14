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
C
Chd|====================================================================
Chd|  SPMD_CHKW                     source/mpi/generic/spmd_chkw.F
Chd|-- called by -----------
Chd|        FVSTATS                       source/airbag/fvstats.F       
Chd|        FXBYPID                       source/constraints/fxbody/fxbypid.F
Chd|        LECTUR                        source/input/lectur.F         
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SZ_PRINT                      source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|        IO_MOD                        share/modules/io_mod.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_CHKW(IWIOUT,IOUT)
      USE MESSAGE_MOD 
            USE IO_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IWIOUT, IOUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IOS
      CHARACTER(LEN=NCHAROUT) :: LINE
C-----------------------------------------------
            IWIOUT_SAVE = 0
      REWIND(UNIT=IOUT)
      READ(UNIT=IOUT,IOSTAT=IOS,FMT='(A)')
      IOS = 0
      DO WHILE(IOS==0)
        READ(UNIT=IOUT,IOSTAT=IOS,FMT='(A)') LINE
        IF(IOS==0) THEN
C calcul du nombre exacte de ligne de 80c
          IWIOUT = IWIOUT + NCHAROUT
        ENDIF
      ENDDO
            IWIOUT_SAVE = IWIOUT
C ajout rewind ds ts les cas (necessaire sur sgi au moins)
      REWIND(UNIT=IOUT)
      READ(UNIT=IOUT,IOSTAT=IOS,FMT='(A)')
C
      RETURN
      END SUBROUTINE 
