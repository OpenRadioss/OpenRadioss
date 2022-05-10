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
Chd|====================================================================
Chd|  TH_MOD                        share/modules/th_mod.F        
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|        INIT_TH0                      source/output/th/init_th0.F   
Chd|        WRITE_TH                      source/output/th/write_th.F   
Chd|-- calls ---------------
Chd|====================================================================
        MODULE TH_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------
        TYPE TH_EL_TYPE
            INTEGER :: TH_ELM_SIZE
            INTEGER, DIMENSION(:,:), ALLOCATABLE :: TH_ELM
        END TYPE TH_EL_TYPE
!   -------------------------
        TYPE TH_WA_REAL
            my_real, DIMENSION(:), ALLOCATABLE :: WA_REAL
        END TYPE TH_WA_REAL
!   -------------------------
        TYPE TH_COMM
            INTEGER, DIMENSION(:), ALLOCATABLE :: TH_DIPLS
            INTEGER, DIMENSION(:), ALLOCATABLE :: TH_SIZE
        END TYPE TH_COMM
!   -------------------------
        TYPE TH_PROC_TYPE
            TYPE(TH_EL_TYPE), DIMENSION(:), ALLOCATABLE :: TH_PROC
        END TYPE TH_PROC_TYPE
!   -------------------------
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_WA_ELTYPE

        !   SPRING
        INTEGER, DIMENSION(10), TARGET :: WA_SPRING_SIZE,TOTAL_WA_SPRING_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: SPRING_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_SPRING_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_SPRING_P0,WA_SPRING
        !   NODE
        INTEGER, DIMENSION(10), TARGET :: WA_NOD_SIZE,TOTAL_WA_NOD_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: NOD_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_NOD_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_NOD_P0,WA_NOD
        !   SOL
        INTEGER, DIMENSION(10), TARGET :: WA_SOL_SIZE,TOTAL_WA_SOL_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: SOL_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_SOL_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_SOL_P0,WA_SOL
        !   QUAD
        INTEGER, DIMENSION(10), TARGET :: WA_QUAD_SIZE,TOTAL_WA_QUAD_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: QUAD_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_QUAD_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_QUAD_P0,WA_QUAD
        !   SHELL/SHELL3N
        INTEGER, DIMENSION(10), TARGET :: WA_COQ_SIZE,TOTAL_WA_COQ_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: COQ_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_COQ_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_COQ_P0,WA_COQ
        !   TRUSS
        INTEGER, DIMENSION(10), TARGET :: WA_TRUS_SIZE,TOTAL_WA_TRUS_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: TRUS_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_TRUS_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_TRUS_P0,WA_TRUS
        !   BEAM
        INTEGER, DIMENSION(10), TARGET :: WA_POUT_SIZE,TOTAL_WA_POUT_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: POUT_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_POUT_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_POUT_P0,WA_POUT
        !   SPH
        INTEGER, DIMENSION(10), TARGET :: WA_SPH_SIZE,TOTAL_WA_SPH_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: SPH_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_SPH_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_SPH_P0,WA_SPH

        !   NST
        INTEGER, DIMENSION(10), TARGET :: WA_NST_SIZE,TOTAL_WA_NST_SIZE
        TYPE(TH_PROC_TYPE), DIMENSION(10), TARGET :: NST_STRUCT       
        TYPE(TH_COMM), DIMENSION(10), TARGET :: WA_NST_COMM
        TYPE(TH_WA_REAL), DIMENSION(10), TARGET :: WA_NST_P0,WA_NST
C
        END MODULE TH_MOD
