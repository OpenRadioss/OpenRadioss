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
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  PRINTSTSZ                     source/spmd/node/st_array_size.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MAP_MEMORY                    ../common_source/comm/memory_use_c.c
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE PRINTSTSZ(DETONATORS)
C-----------------------------------------------
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "commandline.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER (KIND=8) :: MEMTOTALI,MEMTOTAL
      TYPE(DETONATOR_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN_G,LEN_M,LEN_S , LINTSTAMP,MEM
      INTEGER VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK
C-----------------------------------------------
        LEN_G = NPROPGI*NUMGEO
        LEN_M = NPROPMI*NUMMAT      
        LEN_S = NPART     
        CALL MAP_MEMORY(VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK)

        IF( GOT_INSPIRE_ALM == 1)THEN
            IF(GOT_MEM_MAP == 1 )THEN
                WRITE(IOUT,1101) VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK
            ELSE
                IF(VMHWM/=-1) THEN 
                    MEM = VMHWM
                ELSEIF(VMPEAK/=-1) THEN
                    MEM = VMPEAK
                ELSEIF(VMSIZE/=-1) THEN
                    MEM = VMSIZE
                ELSE
                    MEM = -1
                ENDIF
                WRITE(IOUT,1001) MEM
            ENDIF
        ELSE
            IF(GOT_MEM_MAP == 1 )THEN
                WRITE(IOUT,1100) VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK
            ELSE
                IF(VMHWM/=-1) THEN 
                    MEM = VMHWM
                ELSEIF(VMPEAK/=-1) THEN
                    MEM = VMPEAK
                ELSEIF(VMSIZE/=-1) THEN
                    MEM = VMSIZE
                ELSE
                    MEM = -1
                ENDIF
                WRITE(IOUT,1000) MEM
            ENDIF
        ENDIF    


        
 1000 FORMAT(/,
     . ' '/
     . '    STARTER STORAGE '/
     . '    --------------- '/
     . ' '/
     . '    STARTER MEMORY USAGE ',I10,' MB'/)
 1001 FORMAT(/,
     . ' '/
     . '    SOLVER STORAGE '/
     . '    --------------- '/
     . ' '/
     . '    SOLVER MEMORY USAGE ',I10,' MB'/)

 1100 FORMAT(/,
     . ' '/
     . '    STARTER MEMORY USAGE '/
     . '    --------------- '/
     . ' '/
     . '    VmPeak ',I10,' MB'/
     . '    VmSize ',I10,' MB'/
     . '    VmRSS  ',I10,' MB'/
     . '    VmHWM  ',I10,' MB'/
     . '    VmStk  ',I10,' MB')
 1101 FORMAT(/,
     . ' '/
     . '    SOLVER MEMORY USAGE '/
     . '    --------------- '/
     . ' '/
     . '    VmPeak ',I10,' MB'/
     . '    VmSize ',I10,' MB'/
     . '    VmRSS  ',I10,' MB'/
     . '    VmHWM  ',I10,' MB'/
     . '    VmStk  ',I10,' MB')

      END
