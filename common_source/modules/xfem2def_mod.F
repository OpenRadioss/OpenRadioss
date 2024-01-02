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
Chd|====================================================================
Chd|  XFEM2DEF_MOD                  modules/xfem2def_mod.F        
Chd|-- called by -----------
Chd|        ACTIV_XFEM                    starter/source/elements/xfem/lslocal.F
Chd|        ALLOCXFEM                     starter/source/elements/xfem/allocxfem.F
Chd|        C_NCRKXFEM                    starter/source/restart/ddsplit/c_ncrkxfem.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        EDGETIP3N                     starter/source/elements/xfem/lslocal.F
Chd|        EDGETIP4N                     starter/source/elements/xfem/lslocal.F
Chd|        INICRKFILL                    starter/source/elements/xfem/inicrkfill.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        LSLOCAL                       starter/source/elements/xfem/lslocal.F
Chd|        PREINICRK3N                   starter/source/elements/xfem/preinicrk3N.F
Chd|        PREINICRK4N                   starter/source/elements/xfem/preinicrk4N.F
Chd|        W_ANIM_CRK                    starter/source/restart/ddsplit/w_anim_crk.F
Chd|        XFEM_CRACK_INIT               starter/source/elements/xfem/xfem_crack_init.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE XFEM2DEF_MOD
C-----------------------------------------------
#include      "my_real.inc"
C=======================================================================
C Structures needed for cracking of layered shell process
C---------------------------------------------
      TYPE XFEM_SHELL_      !   (NLEVMAX)  phantom element data structure  
         INTEGER  CRKNUMSHELL                                 ! Number of shell elements for each level
         INTEGER  CRKNUMSH3                                   ! Number of SH3N  elements for each level
         INTEGER  CRKNUMSH4                                   ! Number of SH4N  elements for each level
         INTEGER, DIMENSION(:)   , ALLOCATABLE :: PHANTOML    ! Local  shell N for each level = old CRKSHID - local starter 
         INTEGER, DIMENSION(:)   , ALLOCATABLE :: PHANTOMG    ! Global shell N for each level = old CRKSHELLID
         INTEGER, DIMENSION(:)   , ALLOCATABLE :: ELTYPE      ! shell element type (xfem elements)
c        nodal connectivities of phantom shells   
         INTEGER, DIMENSION(:,:) , ALLOCATABLE :: XNODEG      ! global phantom node num  (KNOD2ELC - not used)
         INTEGER, DIMENSION(:,:) , ALLOCATABLE :: XNODEL      ! local  phantom node num per ilev = old XFENODES
      END TYPE XFEM_SHELL_
C====================================================================================

      TYPE XFEM_PHANTOM_       !  (NXLAYMAX)  
           INTEGER, DIMENSION(:,:)   ,ALLOCATABLE :: ITRI      
           INTEGER, DIMENSION(:,:,:) ,ALLOCATABLE :: TAGXP ! velocity link flag
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: ELCUT ! flag of cut xfem element     
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: IFI   ! sign within partitioned superposed element     
      END TYPE XFEM_PHANTOM_
c-------------------------------------------------------------------------------------
c
      TYPE XFEM_LVSET_     !    (NLEVMAX)                                                    
         INTEGER, DIMENSION(:),    ALLOCATABLE :: ELCUT       !  +/- ICRK (element N)
         INTEGER, DIMENSION(:,:) , ALLOCATABLE :: HFI0                                               
         INTEGER, DIMENSION(:,:) , ALLOCATABLE :: ENRICH0                                            
         INTEGER, DIMENSION(:,:) , ALLOCATABLE :: EDGE0                                              
         INTEGER, DIMENSION(:,:) , ALLOCATABLE :: EDGE                                               
         INTEGER, DIMENSION(:,:) , ALLOCATABLE :: EDGETG                                             
         INTEGER, DIMENSION(:)   , ALLOCATABLE :: ICUTEDGE                                           
         my_real, DIMENSION(:)   , ALLOCATABLE :: RATIOEDGE                                          
      END TYPE XFEM_LVSET_                                                                           
C---------------------------------------------
c
      TYPE XFEM_EDGE_       !  (NXLAYMAX)         
         INTEGER, DIMENSION(:)    , ALLOCATABLE :: LAYCUT     ! cut xfem element flag = (0,1,2)
         INTEGER, DIMENSION(:)    , ALLOCATABLE :: EDGEICRK   ! Id of cracked element if cut
         INTEGER, DIMENSION(:,:)  , ALLOCATABLE :: EDGEIFI   
         INTEGER, DIMENSION(:,:)  , ALLOCATABLE :: EDGEENR   
         INTEGER, DIMENSION(:,: ) , ALLOCATABLE :: EDGETIP   
      END TYPE XFEM_EDGE_                                    
C-------------------------------
c
      TYPE XFEM_AVX_        !  (NLEVMAX)
         my_real, DIMENSION(:,:), ALLOCATABLE :: A   
         my_real, DIMENSION(:,:), ALLOCATABLE :: AR  
         my_real, DIMENSION(:,:), ALLOCATABLE :: V   
         my_real, DIMENSION(:,:), ALLOCATABLE :: VR
         my_real, DIMENSION(:,:), ALLOCATABLE :: X   
         my_real, DIMENSION(:,:), ALLOCATABLE :: U   
         my_real, DIMENSION(:,:), ALLOCATABLE :: XX  
      END TYPE XFEM_AVX_                             
C-------------------------------
c
      TYPE XFEM_SKY_      !       
         my_real, DIMENSION(:,:), ALLOCATABLE :: FSKY  
      END TYPE XFEM_SKY_                               
c
C----------------------------
      END MODULE XFEM2DEF_MOD

