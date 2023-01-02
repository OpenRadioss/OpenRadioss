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
Chd|====================================================================
Chd|  ECND_MOD                      share/modules/ecdn_mod.F      
Chd|-- called by -----------
Chd|        PVP_K                         source/implicit/imp_solv.F    
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ECND_MOD 
C----------------------------------------------------------------------- 
C    CND : CondeNsation Dynamic, ND means aussi Node_Dependent
C----------------------------------------------------------------------- 
#include "my_real.inc"      
C=======================================================================
c DEFINE ARRAYS (integers arrays)
C=======================================================================
        integer, DIMENSION(:) , POINTER ::  ICNDS10   !  3*NS10E 
C-- :ND, 2:N1, 3:N2	; ND <0 : degenerated  
C=======================================================================
c DEFINE ARRAYS (float arrays) : internal arrays :restarter
C=======================================================================
        integer, DIMENSION(:) , POINTER ::  IAD_CNDM,FR_CNDM   !  edge node on the front 
        integer, DIMENSION(:) , POINTER ::  IAD_CNDM1,FR_CNDM1   !  edge node on the front 
        integer, DIMENSION(:) , POINTER ::  ADDCNCND,PROCNCND,IADCND   !  P/ON id 
        integer, DIMENSION(:) , POINTER ::  IAD_CNDS,FR_CNDS   ! middle node on the front 
C=======================================================================
c DEFINE ARRAYS (float arrays) : work arrays
C=======================================================================
        integer, DIMENSION(:) , POINTER ::  ITAGND   !  (1:NUMNOD) : id of ICNDS10 
C-- in Engine ITAGND(ND)= id of ICNDS10 < 0 : degenerated iabs() > NS10E :main of Int2
C------------ ITAGND(N1,N2)= id of Int2 :secnd of Int2
        integer, DIMENSION(:) , POINTER ::  FR_NBCCCND,FR_NBCCCND1   !  P/ON id 
         my_real
     .          , DIMENSION(:), ALLOCATABLE :: VND, STIFND,FSKYCND ! (3,NS10E),(4,LCNCND)
         my_real
     .          , DIMENSION(:), ALLOCATABLE :: VMD        ! (3,NUMNOD) workarray
C-- For calculate rightly the KE of ND which is also main node of int2 
        INTEGER NKEND 
        integer, DIMENSION(:) , ALLOCATABLE ::  IMAP2ND   !  to id of ICNDS10
         my_real
     .          MAS_ND,KEND
         my_real
     .          , DIMENSION(:), ALLOCATABLE ::  MASI2ND0 ! (NKEND)
c---------------
      END MODULE ECND_MOD
