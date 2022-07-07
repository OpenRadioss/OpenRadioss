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
Chd|  OUTMAX_MOD                 modules/outmax_mod.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
        MODULE OUTMAX_MOD

#include "my_real.inc"
!--------------------------------------------------------------------
!       OUTMAX_MOD : elementary MOD is directly in GBUF :elbuf_mod
!--------------------------------------------------------------------
        integer  NMAX_N     ! nodal Tmax number, work var
        integer  NMAX_E     ! elementary Tmax number, work var
        integer  LMAX_DIS
        integer  LMAX_VEL
        integer  LMAX_NSIG  ! noda p1&p3 for 3D
        integer  LMAX_NSTRA ! noda p1&p3 for 3D
C------ used to initilize LMAX_* (for elementary only)        
        integer :: NKEYMAX = 6  
C        /VONM;/SIGEQ; /ENER ; /DAMA, /TENS/STRESS ;/TENS/STRAIN
        integer, DIMENSION(:) ,  ALLOCATABLE :: IKEYMAX 
        integer, DIMENSION(:,:) ,ALLOCATABLE :: IPART_OK 
        my_real, DIMENSION(:) , ALLOCATABLE ::  TM_DIS,TM_VEL  ! 3*NUMNOD
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_NSIG1 ,TM_NSIG3 ! 6*NUMNOD
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_NSTRA1,TM_NSTRA3
C------ work arrays        
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_PDIS,TM_PVEL
        my_real, DIMENSION(:) , ALLOCATABLE ::   TM_PNSIG,TM_PNSTRA
        my_real, DIMENSION(:) , ALLOCATABLE ::   GPSTMP,P2TMP

        END MODULE OUTMAX_MOD
