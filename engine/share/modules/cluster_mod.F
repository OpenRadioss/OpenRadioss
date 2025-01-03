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
      !||    cluster_mod    ../engine/share/modules/cluster_mod.F
      !||--- called by ------------------------------------------------------
      !||    clusterf       ../engine/source/output/cluster/clusterf.F
      !||    eig            ../engine/stub/eig.F
      !||    eig1           ../engine/stub/eig1.F
      !||    eigcond        ../engine/stub/eigcond.F
      !||    eigp           ../engine/stub/eigp.F
      !||    genani         ../engine/source/output/anim/generate/genani.F
      !||    genh3d         ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    hist2          ../engine/source/output/th/hist2.F
      !||    imp_buck       ../engine/source/implicit/imp_buck.F
      !||    init_th        ../engine/source/output/th/init_th.F
      !||    init_th0       ../engine/source/output/th/init_th0.F
      !||    rdresb         ../engine/source/output/restart/rdresb.F
      !||    read_cluster   ../engine/source/output/cluster/read_cluster.F
      !||    resol          ../engine/source/engine/resol.F
      !||    resol_head     ../engine/source/engine/resol_head.F
      !||    restalloc      ../engine/source/output/restart/arralloc.F
      !||    restmod        ../engine/share/modules/restart_mod.F
      !||    sortie_main    ../engine/source/output/sortie_main.F
      !||    thcluster      ../engine/source/output/th/thcluster.F
      !||    w_cluster      ../engine/source/output/cluster/w_cluster.F
      !||    wrrestp        ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE CLUSTER_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c
      TYPE CLUSTER_
        integer     :: ID    
        integer     :: TYPE   
        integer     :: IFAIL   
        integer     :: IGR    
        integer     :: NEL      
        integer     :: NNOD     
        integer     :: SKEW     
        integer     :: OFF     
c       
        integer, DIMENSION(:), ALLOCATABLE :: NG
        integer, DIMENSION(:), ALLOCATABLE :: ELEM
        integer, DIMENSION(:), ALLOCATABLE :: NOD1  
        integer, DIMENSION(:), ALLOCATABLE :: NOD2  
c       
        my_real, DIMENSION(:), ALLOCATABLE :: FMAX  
        my_real, DIMENSION(:), ALLOCATABLE :: MMAX  
        my_real, DIMENSION(:), ALLOCATABLE :: AX  
        my_real, DIMENSION(:), ALLOCATABLE :: NX  
        my_real, DIMENSION(3)    :: FOR     
        my_real, DIMENSION(3)    :: MOM     
        my_real                  :: FAIL  
      END TYPE CLUSTER_
c
c---------------
      END MODULE CLUSTER_MOD
