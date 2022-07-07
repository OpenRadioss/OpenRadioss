Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
C  Part of interface buffer:
C  Monitoring informations for interface
C----------------------------------------------------------------------- 
Chd|====================================================================
Chd|  METRIC_MOD                    modules/metric_mod.F          
Chd|-- called by -----------
Chd|        INTBUFDEF_MOD                 common_source/modules/intbufdef_mod.F
Chd|        CLOSE_INTERF_TIME             engine/source/system/timer_interf.F
Chd|        FREFORM                       engine/source/input/freform.F 
Chd|        INIT_INTERF_SORTING_STRATEGY  engine/source/interfaces/init_interf_sorting_strategy.F
Chd|        INTER_DEALLOCATE_WAIT         engine/source/interfaces/generic/inter_deallocate_wait.F
Chd|        INTER_SORT                    engine/source/interfaces/generic/inter_sort.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        INT_STARTIME                  engine/source/system/timer_interf.F
Chd|        INT_STOPTIME                  engine/source/system/timer_interf.F
Chd|        OPEN_INTERF_TIME              engine/source/system/timer_interf.F
Chd|        PRINTIME_INTERF               engine/source/system/timer_interf.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE METRIC_MOD 
C----------------------------------------

#include "my_real.inc"      
C  Flag 2 names
      INTEGER, PARAMETER :: ALGO_VOXEL        = 1
      INTEGER, PARAMETER :: ALGO_BUCKET       = 2
      INTEGER, PARAMETER :: TRY_ALGO_VOXEL    =-1
      INTEGER, PARAMETER :: TRY_ALGO_BUCKET   = 0

C Indexes
      INTEGER, PARAMETER ::  I_MAIN_CRIT_TRI  = 1
      INTEGER, PARAMETER ::  I_MAIN_TRI       = 2
      INTEGER, PARAMETER ::  I_MAIN_OPT_TRI   = 3
      INTEGER, PARAMETER ::  I_MAIN_FORCES    = 4
      INTEGER, PARAMETER ::  I_NOINT          = 5
      INTEGER, PARAMETER ::  I_MULTIMP        = 6
      INTEGER, PARAMETER ::  I_NSN            = 7 
      INTEGER, PARAMETER ::  I_NSNR           = 8 
      INTEGER, PARAMETER ::  I_NCONT          = 9
C Sizes
      INTEGER, PARAMETER ::  SIZE_TIMER       = 4
      INTEGER, PARAMETER ::  SIZE_METRIC      = 9

C Debug
      INTEGER, PARAMETER ::  FILE_ID= 4999
 
      INTEGER :: GLOBAL_INTERFACE_SORTING_ALGO 

      TYPE METRIC_STRUCT_ 
        INTEGER :: NOINT
        INTEGER :: MULTIMP
        INTEGER :: NSN
        INTEGER :: NSNR
        INTEGER :: IMPACT 
        INTEGER :: NCONT
        INTEGER :: CYCLE0
        INTEGER :: ALGO ! 1: definitive voxel, 2:definitive bucket , -1:test voxel, 0: test bucket
        DOUBLE PRECISION :: TIC,TOC, TOLD ! timers use choose algo
        DOUBLE PRECISION, DIMENSION(SIZE_TIMER,2) ::  TIME
      END TYPE
      CONTAINS

Chd|====================================================================
Chd|  INT_PRINTIME                  modules/metric_mod.F          
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INT_PRINTIME(THIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(METRIC_STRUCT_) :: THIS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       WRITE(4999,'(6I10,4F12.5)') 
     . THIS%NOINT,
     . THIS%CYCLE0,
     . THIS%NCONT,
     . THIS%IMPACT,
     . THIS%MULTIMP,
     . THIS%NSNR,
     . THIS%TIME(I_MAIN_CRIT_TRI,1),
c    . THIS%TIME(I_COMMCRIT,1),
c    . THIS%TIME(I_GFRONT,1),
     . THIS%TIME(I_MAIN_TRI,1),
     . THIS%TIME(I_MAIN_OPT_TRI,1)
c      CALL INT_FLUSHTIME(THIS)
      RETURN
      END SUBROUTINE


Chd|====================================================================
Chd|  INT_FLUSHTIME                 modules/metric_mod.F          
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INT_FLUSHTIME(THIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(METRIC_STRUCT_) :: THIS

      THIS%TIME(1:SIZE_TIMER,1:2) = 0.0D0
      RETURN
      END SUBROUTINE

      END MODULE
