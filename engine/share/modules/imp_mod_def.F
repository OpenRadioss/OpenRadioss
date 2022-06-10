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
Chd|  IMP_INTBUFDEF                 share/modules/imp_mod_def.F   
Chd|-- called by -----------
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|        IMP_INTBUF_INI                source/implicit/imp_solv.F    
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_INTBUFDEF 
C----------------------------------------------------------------------- 
#include "my_real.inc"      
c
C=======================================================================
      TYPE IMP_INTBUF_STRUCT_
C=================================================  
c define typeINTBUF_STRUCT_ for interface buffer IMP_INTBUF_TAB
C=======================================================================
c DEFINE SIZES (integers arrays)
C=======================================================================
        integer ::   S_I_STOK     !  replace NUM_IMP (after)            
        integer ::   S_CAND_N     !  replace NS_IMP (after)  
        integer ::   S_CAND_E     !  replace NE_IMP (after)   
        integer ::   S_INDSUBT    !  replace IND_IMP (after)  
C=======================================================================
c DEFINE SIZES (float arrays)
C=======================================================================
        integer ::   S_HJ           !  (4,S_I_STOK)
        integer ::   S_NJ           !  (3,S_I_STOK)       Normal
        integer ::   S_STIF         !  (3,S_I_STOK)       N
C=======================================================================
c DEFINE ARRAYS (integers arrays)
C=======================================================================
        integer, DIMENSION(:) , POINTER ::  I_STOK      
        integer, DIMENSION(:) , POINTER ::  CAND_N      
        integer, DIMENSION(:) , POINTER ::  CAND_E    
        integer, DIMENSION(:) , POINTER ::  INDSUBT   
!type24
C=======================================================================
c DEFINE ARRAYS (float arrays)
C=======================================================================
        my_real, DIMENSION(:) , POINTER ::   HJ           
        my_real, DIMENSION(:) , POINTER ::   NJ           
        my_real, DIMENSION(:) , POINTER ::   STIF         
C=======================================================================
      END TYPE IMP_INTBUF_STRUCT_
C=======================================================================
c---------------
      END MODULE IMP_INTBUFDEF
Chd|====================================================================
Chd|  IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|-- called by -----------
Chd|        CP_IMPBUF                     source/implicit/produt_v.F    
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        DIM_INT7                      source/implicit/ind_glob_k.F  
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|        DIM_KINE_I                    source/implicit/ind_glob_k.F  
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|        INI_BMINMA_IMP                source/implicit/imp_solv.F    
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        IMP_INTBUFDEF                 share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      MODULE IMP_INTBUF
        USE INTBUFDEF_MOD
        USE IMP_INTBUFDEF
        TYPE(INTBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: INTBUF_TAB_CP
        TYPE(IMP_INTBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: INTBUF_TAB_IMP
      my_real, DIMENSION(:,:), ALLOCATABLE :: BMINMA_IMP
      END MODULE IMP_INTBUF
Chd|====================================================================
Chd|  IMP_INTTD                     share/modules/imp_mod_def.F   
Chd|-- called by -----------
Chd|        CP_INTTD                      source/implicit/imp_int_k.F   
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|        IMP_RNUMCD                    source/implicit/imp_int_k.F   
Chd|        SAV_INTTD                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_INTTD
C tableau de travail interface traverse detection 
      INTEGER, DIMENSION(:),ALLOCATABLE :: NS_IMP1,IAD1_NIN
      INTEGER, DIMENSION(:),ALLOCATABLE :: NE_IMP1,IND_IMP1
C      
      END MODULE IMP_INTTD
Cqz52f4 ---
